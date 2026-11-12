/* Copyright (C) 2026 Open64 Project */
/* VHO-owned symbolic MLA cost exploration, independent of native allocation.
 * Reuses the existing detached schedule admission, rounded occupancy and
 * checked readiness arithmetic; it does not change any kernel algorithm.
 * Design: doc/AI-COMPILER-MLA-TWO-PHASE-SEARCH-v0.1.md. */
#include <algorithm>
#include <stdexcept>
#include "tensor_attention_search.h"
#include "dsl_gemm_occupancy.h"

const char *const VHO_mla_cost_names[VHO_MLA_COST_FIELDS]={
    "global_bytes_per_cycle","scalar_lanes_per_cycle","shared_bytes_per_cycle",
    "dependent_scalar_cycles","exp_warp_cycles","div_warp_cycles","shuffle_warp_cycles",
    "cta_barrier_cycles","warp_fence_cycles","launch_cycles","load_latency_cycles","latency_hiding_warps"};
namespace {
/* Checked model arithmetic cannot let an overflowing estimate win. */
UINT64 add(UINT64 a,UINT64 b) {uint64_t r;if(!DSL_readiness_add(a,b,&r)) throw std::runtime_error("estimate_overflow");return r;}
/* Preserve a finite whole-stage cost across shapes, rates and grid rounds. */
UINT64 mul(UINT64 a,UINT64 b) {uint64_t r;if(!DSL_readiness_mul(a,b,&r)) throw std::runtime_error("estimate_overflow");return r;}
/* Apply the shared positive-denominator ceiling operation. */
UINT64 ceilq(UINT64 a,UINT64 b) {return DSL_readiness_ceil(a,b);}
/* Evaluate whole-stage additive bulk rounds; no speculative overlap credit.
 * Work is shared per SM, global traffic uses one device bandwidth, dependency
 * chains bound scalar service, and resident CTAs do not multiply SM throughput. */
void Cost_stage(VHO_MLA_STAGE_ESTIMATE &s,const VHO_TENSOR_GPU_PROFILE &g,const VHO_MLA_COST_PROFILE &p) {
    const UINT64 *v=p.values;UINT64 capacity=mul(g.sms,s.resident);
    s.launch=v[VHO_MLA_LAUNCH];
    for(UINT32 tail=0;tail<2;++tail) {
        UINT64 active=tail?s.ctas%capacity:capacity,rounds=tail?(active!=0):s.ctas/capacity;
        if(!rounds) continue;
        UINT64 q=ceilq(active,g.sms),warps=mul(q,s.threads/32);
        UINT64 compute=std::max(mul(q,ceilq(s.scalar_ops,v[VHO_MLA_SCALAR_RATE])),mul(s.dependency,v[VHO_MLA_DEPENDENCY]));
        UINT64 memory=add(ceilq(mul(active,s.global_bytes),v[VHO_MLA_GLOBAL_RATE]),ceilq(v[VHO_MLA_LOAD_LATENCY],std::min(warps,v[VHO_MLA_HIDING_WARPS])));
        UINT64 special=mul(q,add(add(mul(s.shuffles,v[VHO_MLA_SHUFFLE]),mul(s.exps,v[VHO_MLA_EXP])),mul(s.divs,v[VHO_MLA_DIV])));
        UINT64 local=mul(q,ceilq(s.shared_bytes,v[VHO_MLA_SHARED_RATE]));
        UINT64 sync=mul(q,add(mul(s.barriers,v[VHO_MLA_CTA_BARRIER]),mul(s.fences,v[VHO_MLA_WARP_FENCE])));
        s.compute=add(s.compute,mul(rounds,compute));s.memory=add(s.memory,mul(rounds,memory));
        s.special=add(s.special,mul(rounds,special));s.local=add(s.local,mul(rounds,local));s.sync=add(s.sync,mul(rounds,sync));s.rounds=add(s.rounds,rounds);
    }
    s.total=add(add(add(s.compute,s.memory),add(s.special,s.local)),add(s.sync,s.launch));
}
/* Describe full-chunk/all-mask-true work of the already admitted native
 * realization. Padding, shared publications, warp tuples and the unchanged
 * inter-chunk merge have distinct costs. Counts are estimates, not ISA facts. */
void Attention_counts(VHO_MLA_STAGE_ESTIMATE &s,const VHO_TENSOR_ATTENTION &a,const VHO_TENSOR_KERNEL &k) {
    UINT64 warps=k.threads/32,groups=ceilq(a.rank,k.warp_owned_partials?4:k.threads/8);
    UINT64 span=k.mapping==3?k.split_width:k.chunks,capacity=mul(ceilq(span,k.threads),k.threads);
    s.shuffles=add(mul(10,warps),mul(mul(20,groups),warps));
    s.dependency=k.mapping==3?mul(add(a.rank,a.rope),ceilq(span,k.threads)):ceilq(span,k.threads);
    if(k.mapping==3) {
        s.scalar_ops=add(mul(span,add(mul(2,add(a.rank,a.rope)),2)),mul(mul(2,span),a.rank));
        s.global_bytes=add(add(mul(mul(8,span),add(a.rank,a.rope)),mul(mul(4,span),a.rank)),add(span,mul(4,a.rank+(k.stage==1?2:1))));
    } else {
        s.scalar_ops=mul(span,add(mul(2,a.rank),6));
        s.global_bytes=add(mul(mul(4,span),a.rank+4),mul(4,a.rank+1));
    }
    s.exps=ceilq(capacity,32);s.divs=ceilq(capacity,32);
    s.barriers=k.threads==32?0:k.warp_owned_partials?1:k.two_warp_statistics?3:5;
    s.fences=k.threads==32 && !k.register_statistics?3:0;
    if(k.register_statistics) s.shuffles=add(s.shuffles,mul(2,warps));
    else if(k.warp_owned_partials) {
        s.shuffles=add(s.shuffles,mul(4,warps));s.exps=add(s.exps,4);s.divs=add(s.divs,4);
        s.shared_bytes=add(mul(16,a.rank),64);
    } else if(k.two_warp_statistics) {
        s.shuffles=add(s.shuffles,mul(2,warps));
        s.shared_bytes=mul(4,add(add(span,mul(mul(span,groups),warps)),12));
    } else s.shared_bytes=mul(4,add(add(mul(5,capacity),mul(mul(span,groups),warps)),add(mul(3,k.threads),add(mul(4,warps),4))));
}
/* Cost one candidate at most once. All-stage schedule admission and occupancy
 * precede selection. No table creation, source emission or regression occurs. */
void Evaluate(VHO_MLA_SEARCH_CANDIDATE &c,const VHO_TENSOR_PROGRAM &source,const VHO_TENSOR_GPU_PROFILE &g,const VHO_MLA_COST_PROFILE &model) {
    c.evaluated=true;c.reason="native_capacity";
    VHO_TENSOR_PROGRAM p=source;p.auto_chunks=false;p.search_selected=false;p.search_evidence.clear();p.search_identity.clear();
    p.register_latent=true;p.chunk_policy=c.policy;p.split_width=c.width;
    VHO_TENSOR_ATTENTION a=VHO_tensor_attention(p);
    if((!c.width && a.length>4096) || (c.width && c.width>=a.length)) return;
    VHO_TENSOR_SCHEDULE plan;if(!VHO_tensor_schedule(p,&plan,NULL)) return;
    c.scratch=c.width?mul(mul(mul(4,a.batch*a.heads),ceilq(a.length,c.width)),a.rank+2):0;
    c.reason="scratch_budget";if(c.scratch>g.scratch_budget) return;
    c.reason="resource_fit";
    for(size_t i=0;i<plan.kernels.size();++i) {
        const VHO_TENSOR_KERNEL &k=plan.kernels[i];VHO_MLA_STAGE_ESTIMATE s;
        s.operation=k.id;s.ctas=VHO_tensor_grid(p,k);s.threads=k.threads;s.shared=VHO_tensor_shared_bytes(p,k);
        DSL_GEMM_OCCUPANCY_RESULT occupancy;
        if(!DSL_gemm_occupancy(g.resources,32,g.sms,s.threads,4*g.registers_per_thread,s.shared,s.ctas,&occupancy) || !occupancy.resident) return;
        s.resident=occupancy.resident;
        if(k.mapping>=3) Attention_counts(s,a,k);
        else {
            const VHO_TENSOR_OPERATION &op=p.operations[k.root];UINT64 count=ceilq(k.elements,s.ctas),width=1;
            if(op.name=="common.matmul") width=p.values[op.inputs[0]].shape.back();
            s.scalar_ops=mul(count,add(mul(2,width),1));s.global_bytes=mul(mul(4,count),add(mul(2,width),1));
            s.dependency=width;
            if(k.mapping==2) {s.barriers=mul(2,ceilq(width,32));s.shared_bytes=mul(640,ceilq(width,32));s.shuffles=20;}
        }
        Cost_stage(s,g,model);c.total=add(c.total,s.total);c.stages.push_back(s);
    }
    c.reason="eligible";c.eligible=true;
}
/* Stable finite pseudo-random ordering; no host RNG or nondeterministic seed. */
UINT64 Next_random(UINT64 &state) {state=(state*UINT64(1664525)+UINT64(1013904223))&UINT64(0xffffffff);return state;}
/* Structural neighbors share a width or adjacent widths in one family. */
bool Neighbors(const VHO_MLA_SEARCH_CANDIDATE &a,const VHO_MLA_SEARCH_CANDIDATE &b) {
    return a.width==b.width || (a.policy==b.policy && ((!a.width && b.width==32) || (!b.width && a.width==32) ||
        (a.width && b.width && (a.width==2*b.width || b.width==2*a.width))));
}
/* Apply explicitly scoped stopping predicates. The bound is solely on this
 * additive estimator (every realization launches at least five kernels), never
 * a bound on physical GPU execution. Economic costs use caller-converted units. */
std::string Stop(const VHO_MLA_SEARCH_OPTIONS &o,UINT64 best,UINT64 lower,UINT64 stale) {
    if(o.stop=="plateau" && stale>=o.patience) return "empirical_plateau";
    if(o.stop=="target" && best<=o.target) return "estimated_target_reached";
    if(o.stop=="relative-gap" && best>=lower && best-lower<=mul(lower,o.gap_ppm)/1000000) return "estimator_gap_certificate";
    if(o.stop=="cost-aware" && best>=lower && best-lower<=o.evaluation_cost/o.remaining_invocations) return "estimator_benefit_bound";
    return "";
}
}

/* Explicit complete timings are required; neither unknown zeros nor a generic
 * architecture alias supplies absent exp/div/shuffle/launch costs. */
BOOL VHO_mla_search_inputs_valid(const VHO_MLA_COST_PROFILE &p,const VHO_MLA_SEARCH_OPTIONS &o,FILE *f) {
    bool valid=!p.id.empty() && p.id.size()<=96 && (p.provenance=="synthetic" || p.provenance=="estimated" || p.provenance=="measured-input") &&
        (o.strategy=="exhaustive" || o.strategy=="random" || o.strategy=="local" || o.strategy=="portfolio") &&
        (o.stop=="exhausted" || o.stop=="budget" || o.stop=="plateau" || o.stop=="target" || o.stop=="relative-gap" || o.stop=="cost-aware") &&
        o.budget>0 && o.budget<=12 && o.patience>0 && o.patience<=12 && o.seed<=0xffffffffU && o.minimum_gain<=1000000000 && o.gap_ppm<=1000000 &&
        (o.stop!="target" || o.target>0) && (o.stop!="cost-aware" || (o.remaining_invocations>0 && o.evaluation_cost>0));
    for(UINT32 i=0;i<VHO_MLA_COST_FIELDS;++i) valid=valid && p.values[i]>0 && p.values[i]<=1000000000;
    if(!valid && f) fprintf(f,"MLA search: invalid/unsupported model, strategy or stopping evidence\n");return valid;
}

/* Run independent proposal orders with common ranking and a shared lazy
 * estimate cache. A strategy winner is descriptive for this workload/model;
 * only complete enumeration certifies the minimum of the declared estimator. */
BOOL VHO_mla_explore(const VHO_TENSOR_PROGRAM &source,const VHO_TENSOR_GPU_PROFILE &g,
    const VHO_MLA_COST_PROFILE &model,const VHO_MLA_SEARCH_OPTIONS &options,VHO_MLA_SEARCH_RESULT *out,FILE *f) {
    try {
        if(!out || !VHO_mla_search_inputs_valid(model,options,f) || !VHO_tensor_gpu_profile_valid(g,f) || model.architecture!=g.architecture ||
           source.opt_level!=3 || !source.fuse_attention || !source.cooperative || !VHO_tensor_validate_program(source,f)) throw std::runtime_error("search admission failed");
        VHO_MLA_SEARCH_RESULT result;result.lower_bound=mul(5,model.values[VHO_MLA_LAUNCH]);
        const UINT32 widths[]={0,32,64,128,256,512,1024,2048,4096,32,64,64};
        const UINT32 policies[]={6,6,6,6,6,6,6,6,6,3,3,5};
        for(UINT32 i=0;i<12;++i) {VHO_MLA_SEARCH_CANDIDATE c;c.width=widths[i];c.policy=policies[i];result.candidates.push_back(c);}
        const char *strategies[]={"local","random","exhaustive"};bool global_found=false;
        for(UINT32 r=0;r<(options.strategy=="portfolio"?3U:1U);++r) {
            VHO_MLA_SEARCH_RUN run;run.strategy=options.strategy=="portfolio"?strategies[r]:options.strategy;
            bool visited[12]={false},found=false;UINT64 state=options.seed,stale=0;UINT32 anchor=Next_random(state)%12;
            while(run.visited.size()<options.budget) {
                std::vector<UINT32> available;
                for(UINT32 i=0;i<12;++i) if(!visited[i] && (run.strategy!="local" || Neighbors(result.candidates[anchor],result.candidates[i]))) available.push_back(i);
                bool restart=available.empty();
                if(restart) {for(UINT32 i=0;i<12;++i) if(!visited[i]) available.push_back(i);if(run.strategy=="local") ++run.restarts;}
                if(available.empty()) break;
                UINT32 id=available[run.strategy=="exhaustive"?0:Next_random(state)%available.size()];visited[id]=true;run.visited.push_back(id);
                VHO_MLA_SEARCH_CANDIDATE &c=result.candidates[id];
                if(!c.evaluated) {
                    ++result.evaluations;
                    try {Evaluate(c,source,g,model);} catch(const std::exception &e) {c.eligible=false;c.reason=e.what();c.total=0;c.stages.clear();}
                } else ++result.cache_hits;
                ++stale;
                if(c.eligible && (!found || c.total<run.best || (c.total==run.best && id<run.winner))) {
                    if(!found || run.best-c.total>options.minimum_gain) stale=0;
                    run.winner=id;run.best=c.total;run.first_best_trial=run.visited.size();found=true;
                }
                if(restart || (c.eligible && (!result.candidates[anchor].eligible || c.total<=result.candidates[anchor].total))) anchor=id;
                if(found) run.termination=Stop(options,run.best,result.lower_bound,stale);
                if(!run.termination.empty()) break;
            }
            if(run.termination.empty()) run.termination=run.visited.size()==12?"exhausted_declared_space":"evaluation_budget";
            result.runs.push_back(run);
            if(found && (!global_found || run.best<result.candidates[result.winner].total ||
               (run.best==result.candidates[result.winner].total && run.winner<result.winner))) {result.winner=run.winner;global_found=true;}
        }
        if(!global_found) throw std::runtime_error("no estimated legal candidate within search budget");
        result.complete=result.evaluations==12;
        bool best_run=false;
        for(UINT32 i=0;i<result.runs.size();++i) if(result.runs[i].best==result.candidates[result.winner].total &&
            (!best_run || result.runs[i].first_best_trial<result.runs[result.best_run].first_best_trial)) {result.best_run=i;best_run=true;}
        *out=result;return TRUE;
    } catch(const std::exception &e) {if(f) fprintf(f,"MLA search: %s\n",e.what());return FALSE;}
}
