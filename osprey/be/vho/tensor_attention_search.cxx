/* Copyright (C) 2026 Open64 Project */
/* VHO-owned symbolic MLA cost exploration, independent of native allocation.
 * Reuses the existing detached schedule admission, rounded occupancy and
 * checked readiness arithmetic; it does not change any kernel algorithm.
 * Design: doc/AI-COMPILER-MLA-TWO-PHASE-SEARCH-v0.1.md. */
#include <algorithm>
#include <stdexcept>
#include <cctype>
#include <set>
#include <sstream>
#include <time.h>
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
void Evaluate(VHO_MLA_SEARCH_CANDIDATE &c,const VHO_TENSOR_PROGRAM &source,const VHO_TENSOR_GPU_PROFILE &g,const VHO_MLA_COST_PROFILE &model,bool counts_only=false) {
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
        if(!counts_only) {Cost_stage(s,g,model);c.total=add(c.total,s.total);}
        c.stages.push_back(s);
    }
    c.reason="eligible";c.eligible=true;
}
/* Stable finite pseudo-random ordering; no host RNG or nondeterministic seed. */
UINT64 Next_random(UINT64 &state) {state=(state*UINT64(1664525)+UINT64(1013904223))&UINT64(0xffffffff);return state;}
/* Structural neighbors share a width or adjacent widths in one family. */
bool Neighbors(const VHO_MLA_SEARCH_CANDIDATE &a,const VHO_MLA_SEARCH_CANDIDATE &b) {
    return (a.descriptor.group==b.descriptor.group && a.width==b.width) || (a.descriptor.group==b.descriptor.group && a.policy==b.policy && ((!a.width && b.width==32) || (!b.width && a.width==32) ||
        (a.width && b.width && (a.width==2*b.width || b.width==2*a.width))));
}
/* One declared catalog shared by v1 archival and v2 controlled exploration. */
void VHO_search_catalog(VHO_MLA_SEARCH_RESULT &result) {
    const UINT32 widths[]={0,32,64,128,256,512,1024,2048,4096,32,64,64};
    const UINT32 policies[]={6,6,6,6,6,6,6,6,6,3,3,5};
    for(UINT32 i=0;i<12;++i) {VHO_MLA_SEARCH_CANDIDATE c;c.width=widths[i];c.policy=policies[i];result.candidates.push_back(c);}
}
/* Existing deterministic proposal semantics; callers own visits and budgets. */
UINT32 VHO_search_propose(const std::string &strategy,const bool *visited,UINT32 anchor,
    const std::vector<VHO_MLA_SEARCH_CANDIDATE> &candidates,UINT64 &state,bool &restart) {
    std::vector<UINT32> available;
    for(UINT32 i=0;i<candidates.size();++i) if(!visited[i] && (strategy!="local" || Neighbors(candidates[anchor],candidates[i]))) available.push_back(i);
    restart=available.empty();
    if(restart) for(UINT32 i=0;i<candidates.size();++i) if(!visited[i]) available.push_back(i);
    return available.empty()?candidates.size():available[strategy=="exhaustive"?0:Next_random(state)%available.size()];
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

#include "tensor_attention_movement.inc"

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
        VHO_search_catalog(result);
        const char *strategies[]={"local","random","exhaustive"};bool global_found=false;
        for(UINT32 r=0;r<(options.strategy=="portfolio"?3U:1U);++r) {
            VHO_MLA_SEARCH_RUN run;run.strategy=options.strategy=="portfolio"?strategies[r]:options.strategy;
            bool visited[12]={false},found=false;UINT64 state=options.seed,stale=0;UINT32 anchor=Next_random(state)%12;
            while(run.visited.size()<options.budget) {
                bool restart=false;UINT32 id=VHO_search_propose(run.strategy,visited,anchor,result.candidates,state,restart);
                if(restart && run.strategy=="local") ++run.restarts;
                if(id==12) break;
                visited[id]=true;run.visited.push_back(id);
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

#ifdef VHO_MLA_SEARCH_TEST_CLOCK
/* Test-only deterministic clock. It is absent from the production object and
 * cannot be supplied through options, source metadata or a native callback. */
extern UINT64 VHO_mla_search_test_clock_us();
#endif

namespace {
/* Read a native-free monotonic clock; a missing clock never disables a cap. */
UINT64 VHO_search_clock() {
#ifdef VHO_MLA_SEARCH_TEST_CLOCK
    return VHO_mla_search_test_clock_us();
#else
    struct timespec value;
    if(clock_gettime(CLOCK_MONOTONIC,&value)) throw std::runtime_error("clock_unavailable");
    return UINT64(value.tv_sec)*1000000+UINT64(value.tv_nsec)/1000;
#endif
}
/* Reject clock rollback and account admission, descriptors and all proposals. */
void VHO_search_elapsed(UINT64 start,UINT64 &last,VHO_MLA_PRODUCTION_RESULT &result) {
    UINT64 now=VHO_search_clock();
    if(now<last) throw std::runtime_error("nonmonotonic_clock");
    last=now;result.elapsed_us=now-start;
}
/* Fixed textual run IDs are safe in both framed keys and line-based traces. */
bool VHO_search_id(const std::string &id) {
    if(id.empty() || id.size()>96) return false;
    for(size_t i=0;i<id.size();++i)
        if(!std::isalnum((unsigned char)id[i]) && id[i]!='_' && id[i]!='-' && id[i]!='.') return false;
    return true;
}
/* Predicate composition is an explicit set, independent of proposal order. */
bool VHO_search_stop_enabled(const VHO_MLA_SEARCH_CONTROL &control,const char *name) {
    return std::find(control.stopping.begin(),control.stopping.end(),name)!=control.stopping.end();
}
struct VHO_MLA_RUN_STATE {
    bool visited[36], active, found;
    UINT64 random, stale;
    UINT32 anchor;
    /* Independent deterministic state; shared estimates live only in result. */
    explicit VHO_MLA_RUN_STATE(UINT64 seed,unsigned count=12) : active(true), found(false), random(seed), stale(0) {
        std::fill(visited,visited+36,false);anchor=Next_random(random)%count;
    }
};
/* Record all terminal predicates in a fixed priority order. Completeness is
 * stronger than a partial budget result; no model bound is a GPU guarantee. */
std::vector<std::string> VHO_search_predicates(const VHO_MLA_SEARCH_CONTROL &control,
    const VHO_MLA_PRODUCTION_RESULT &result,bool active) {
    std::vector<std::string> predicates;
    if(result.exploration.evaluations==result.exploration.candidates.size()) predicates.push_back("declared_space_complete");
    if(result.proposals>=control.global_proposals) predicates.push_back("global_proposal_budget");
    if(result.exploration.evaluations>=control.global_unique) predicates.push_back("global_unique_budget");
    if(control.global_time_us && result.elapsed_us>=control.global_time_us) predicates.push_back("global_time_budget");
    if(result.has_winner) {
        UINT64 best=result.exploration.candidates[result.exploration.winner].total,lower=result.exploration.lower_bound;
        if(VHO_search_stop_enabled(control,"target") && best<=control.target) predicates.push_back("estimated_target_reached");
        if(VHO_search_stop_enabled(control,"relative-gap") && best>=lower && best-lower<=mul(lower,control.gap_ppm)/1000000)
            predicates.push_back("estimator_gap_certificate");
        if(VHO_search_stop_enabled(control,"cost-aware") && best>=lower && best-lower<=control.evaluation_cost/control.remaining_invocations)
            predicates.push_back("estimator_benefit_bound");
    }
    if(!active) predicates.push_back("all_runs_stopped");
    return predicates;
}
}

#include "tensor_quantized_movement.inc"
#include "tensor_q5_cost.inc"

/* Default full finite search; v2 JSON still requires every field explicitly. */
VHO_MLA_SEARCH_CONTROL::VHO_MLA_SEARCH_CONTROL() : space("mla-family-width-space-v1"),mode("estimate"),
    global_proposals(36),global_unique(12),global_time_us(0),patience(12),minimum_gain(0),target(0),gap_ppm(10000),
    remaining_invocations(0),evaluation_cost(0),economic_conversion(false),host_us(0),cycles_per_us(0) {
    const char *strategies[]={"local","random","exhaustive"};
    for(unsigned i=0;i<3;++i) {VHO_MLA_SEARCH_RUN_CONTROL run;run.id=run.strategy=strategies[i];runs.push_back(run);}
    stopping.push_back("exhausted");stopping.push_back("budget");
}

/* Validate hard bounds, unique IDs, provider support and dimensional evidence.
 * A caller cannot disable the safety caps by omitting budget/exhaustion stops. */
static BOOL VHO_mla_control_valid(const VHO_MLA_SEARCH_CONTROL &control,FILE *diagnostic,bool movement,bool quantized=false,bool q5=false) {
    UINT64 count=(quantized || q5)?17:movement?36:12;
    bool valid=control.space==(q5?VHO_q5_catalog():quantized?VHO_q4_catalog():movement?VHO_mla_movement_catalog():"mla-family-width-space-v1") && control.mode=="estimate" &&
        !control.runs.empty() && control.runs.size()<=16 && !control.stopping.empty() && control.stopping.size()<=6 &&
        control.global_proposals>0 && control.global_proposals<=16*count && control.global_unique>0 && control.global_unique<=count &&
        control.global_time_us<=UINT64(3600000000ULL) && control.patience>0 && control.patience<=count &&
        control.minimum_gain<=1000000000 && control.gap_ppm<=1000000 && control.target<=UINT64(1000000000000000000ULL) &&
        control.remaining_invocations<=UINT64(1000000000000ULL) && control.evaluation_cost<=UINT64(1000000000000000000ULL);
    std::set<std::string> ids,stops;
    for(size_t i=0;i<control.runs.size();++i) {
        const VHO_MLA_SEARCH_RUN_CONTROL &run=control.runs[i];
        valid=valid && VHO_search_id(run.id) && ids.insert(run.id).second &&
            (run.strategy=="local" || run.strategy=="random" || run.strategy=="exhaustive") &&
            run.proposal_limit>0 && run.proposal_limit<=count && run.seed<=0xffffffffU;
    }
    for(size_t i=0;i<control.stopping.size();++i) {
        const std::string &stop=control.stopping[i];
        valid=valid && stops.insert(stop).second && (stop=="exhausted" || stop=="budget" || stop=="plateau" ||
            stop=="target" || stop=="relative-gap" || stop=="cost-aware");
    }
    if(VHO_search_stop_enabled(control,"target")) valid=valid && control.target>0;
    if(VHO_search_stop_enabled(control,"cost-aware")) valid=valid && control.remaining_invocations>0 && control.evaluation_cost>0 && control.economic_conversion;
    if(control.economic_conversion) {
        uint64_t converted=0;
        valid=valid && control.host_us>0 && control.host_us<=UINT64(1000000000000ULL) && control.cycles_per_us>0 &&
            control.cycles_per_us<=1000000000 && (control.conversion_provenance=="synthetic" ||
            control.conversion_provenance=="estimated" || control.conversion_provenance=="measured-input") &&
            DSL_readiness_mul(control.host_us,control.cycles_per_us,&converted) && converted==control.evaluation_cost;
    } else valid=valid && !control.host_us && !control.cycles_per_us && control.conversion_provenance.empty();
    if((movement || quantized || q5) && (VHO_search_stop_enabled(control,"relative-gap") || VHO_search_stop_enabled(control,"cost-aware"))) valid=false;
    if(!valid && diagnostic) fprintf(diagnostic,"MLA controlled search: invalid controls or unsupported stopping evidence\n");
    return valid;
}

BOOL VHO_mla_search_control_valid(const VHO_MLA_SEARCH_CONTROL &c,FILE *f) {return VHO_mla_control_valid(c,f,false);}
BOOL VHO_mla_movement_control_valid(const VHO_MLA_SEARCH_CONTROL &c,FILE *f) {return VHO_mla_control_valid(c,f,true);}

/* Bind all policy values, including same-ID run changes and unit conversion.
 * This full identity is not a cryptographic signature or a portable time trace. */
std::string VHO_mla_search_control_key(const VHO_MLA_SEARCH_CONTROL &control) {
    std::ostringstream key;
    key<<(control.space==VHO_q5_catalog()?"mla-search-controls-q5-v1/":control.space==VHO_q4_catalog()?"mla-search-controls-q4-v1/":control.space==VHO_mla_movement_catalog()?"mla-search-controls-v3/":"mla-search-controls-v2/")<<control.space<<'/'<<control.mode<<'/'<<control.global_proposals<<'/'<<control.global_unique<<'/'
        <<control.global_time_us<<'/'<<control.patience<<'/'<<control.minimum_gain<<'/'<<control.target<<'/'<<control.gap_ppm<<'/'
        <<control.remaining_invocations<<'/'<<control.evaluation_cost<<'/'<<control.economic_conversion<<'/'<<control.host_us<<'/'
        <<control.cycles_per_us<<'/'<<control.conversion_provenance<<"/runs="<<control.runs.size();
    for(size_t i=0;i<control.runs.size();++i) {
        const VHO_MLA_SEARCH_RUN_CONTROL &run=control.runs[i];
        key<<'/'<<run.id.size()<<':'<<run.id<<'/'<<run.strategy<<'/'<<run.seed<<'/'<<run.proposal_limit;
    }
    key<<"/stops="<<control.stopping.size();
    for(size_t i=0;i<control.stopping.size();++i) key<<'/'<<control.stopping[i];
    return key.str();
}

/* Reuse the v1 catalog/proposers/evaluator under a fair portfolio scheduler.
 * Caps are checked before every proposal and after every complete estimate;
 * a cache hit consumes only a proposal. No native callback or context exists. */
static BOOL VHO_mla_controlled(const VHO_TENSOR_PROGRAM &source,const VHO_TENSOR_GPU_PROFILE &gpu,
    const VHO_MLA_COST_PROFILE &model,const VHO_MLA_SEARCH_CONTROL &control,VHO_MLA_PRODUCTION_RESULT *output,FILE *diagnostic,const VHO_MLA_MOVEMENT_CONTEXT *movement,const VHO_Q4_PROFILE *q4_profile=NULL,const VHO_Q4_SNAPSHOT *q4_snapshot=NULL,UINT64 q5_log=0) {
    try {
        UINT64 start=VHO_search_clock(),last=start;
        VHO_MLA_SEARCH_OPTIONS legacy_validation;
        if(!output || !VHO_mla_control_valid(control,diagnostic,movement!=NULL,q4_profile!=NULL && !q5_log,q5_log!=0) ||
            (movement && (!VHO_mla_movement_profile_valid(movement->profile,diagnostic) || movement->profile.architecture!=gpu.architecture)) || !VHO_mla_search_inputs_valid(model,legacy_validation,diagnostic) ||
            !VHO_tensor_gpu_profile_valid(gpu,diagnostic) || model.architecture!=gpu.architecture ||
            (q4_profile?(!q4_snapshot || !Q4_valid(*q4_profile,*q4_snapshot,gpu) || (q5_log && (q5_log>1000000000 || !VHO_tensor_validate_quantized_high(source,NULL) || q4_snapshot->schema.source_key!=VHO_tensor_quantized_high_key(source)))):
                (source.opt_level!=3 || !source.fuse_attention || !source.cooperative || !VHO_tensor_validate_program(source,diagnostic))))
            throw std::runtime_error("controlled_search_admission_failed");
        VHO_MLA_PRODUCTION_RESULT result;VHO_MLA_SEARCH_RESULT &search=result.exploration;
        if(q4_profile) {search.lower_bound=0;Q4_catalog(search);}
        else if(movement) {search.lower_bound=0;Movement_catalog(search,VHO_tensor_attention(source));}
        else {search.lower_bound=mul(5,model.values[VHO_MLA_LAUNCH]);VHO_search_catalog(search);}
        unsigned count=search.candidates.size();
        std::vector<VHO_MLA_RUN_STATE> states;
        for(size_t i=0;i<control.runs.size();++i) {
            states.push_back(VHO_MLA_RUN_STATE(control.runs[i].seed,count));
            VHO_MLA_SEARCH_RUN run;run.strategy=control.runs[i].strategy;search.runs.push_back(run);
        }
        result.run_predicates.resize(control.runs.size());size_t cursor=0;
        for(;;) {
            VHO_search_elapsed(start,last,result);bool active=false;
            for(size_t i=0;i<states.size();++i) active=active || states[i].active;
            result.predicates=VHO_search_predicates(control,result,active);
            if(!result.predicates.empty()) break;
            while(!states[cursor].active) cursor=(cursor+1)%states.size();
            size_t current=cursor;cursor=(cursor+1)%states.size();
            VHO_MLA_RUN_STATE &state=states[current];VHO_MLA_SEARCH_RUN &run=search.runs[current];
            bool restart=false;UINT32 id=VHO_search_propose(run.strategy,state.visited,state.anchor,search.candidates,state.random,restart);
            /* Charge a compatible G1 seed as the first ordinary proposal. */
            if(q4_profile && !result.proposals) id=0;
            if(movement && !result.proposals) id=VHO_tensor_attention(source).length<=4096?0:3;
            if(id==count) throw std::runtime_error("controlled_proposal_state_inconsistent");
            if(restart && run.strategy=="local") ++run.restarts;
            state.visited[id]=true;run.visited.push_back(id);++result.proposals;
            VHO_MLA_SEARCH_CANDIDATE &candidate=search.candidates[id];bool hit=candidate.evaluated;
            if(!hit) {
                ++search.evaluations;
                try {if(q5_log) Q5_evaluate(candidate,source,gpu,model,*q4_profile,*q4_snapshot,q5_log);else if(q4_profile) Q4_evaluate(candidate,gpu,model,*q4_profile,*q4_snapshot);else if(movement) Movement_evaluate(candidate,source,gpu,model,*movement);else Evaluate(candidate,source,gpu,model);}
                catch(const std::exception &error) {candidate.evaluated=true;candidate.eligible=false;candidate.reason=error.what();candidate.total=0;candidate.stages.clear();candidate.movement=VHO_MLA_MOVEMENT_ESTIMATE();candidate.quantized=VHO_Q4_ESTIMATE();}
            } else ++search.cache_hits;
            ++state.stale;
            if(candidate.eligible && (!state.found || candidate.total<run.best || (candidate.total==run.best && id<run.winner))) {
                if(!state.found || run.best-candidate.total>control.minimum_gain) state.stale=0;
                run.winner=id;run.best=candidate.total;run.first_best_trial=run.visited.size();state.found=true;
            }
            if(candidate.eligible && (!result.has_winner || candidate.total<search.candidates[search.winner].total ||
                (candidate.total==search.candidates[search.winner].total && id<search.winner))) {
                result.has_winner=true;search.winner=id;search.best_run=current;
            }
            if(restart || (candidate.eligible && (!search.candidates[state.anchor].eligible || candidate.total<=search.candidates[state.anchor].total))) state.anchor=id;
            VHO_MLA_SEARCH_EVENT event;
            event.proposal=result.proposals;event.unique=search.evaluations;event.run=current;event.candidate=id;event.cache_hit=hit;
            event.eligible=candidate.eligible;event.run_best=run.best;event.global_best=result.has_winner?search.candidates[search.winner].total:0;
            result.events.push_back(event);
            std::vector<std::string> &predicates=result.run_predicates[current];
            if(run.visited.size()==count) predicates.push_back("run_space_exhausted");
            if(run.visited.size()>=control.runs[current].proposal_limit) predicates.push_back("run_proposal_budget");
            if(VHO_search_stop_enabled(control,"plateau") && state.found && state.stale>=control.patience) predicates.push_back("empirical_plateau");
            if(!predicates.empty()) {run.termination=predicates[0];state.active=false;}
        }
        /* Final accounting records simultaneous bounds even if several limits
         * and a model target become true at this same terminal snapshot. */
        VHO_search_elapsed(start,last,result);bool active=false;
        for(size_t i=0;i<states.size();++i) active=active || states[i].active;
        result.predicates=VHO_search_predicates(control,result,active);result.termination=result.predicates[0];
        search.complete=search.evaluations==count;
        for(size_t i=0;i<search.runs.size();++i)
            if(search.runs[i].termination.empty()) search.runs[i].termination="global:"+result.termination;
        *output=result;return TRUE;
    } catch(const std::exception &error) {
        if(diagnostic) fprintf(diagnostic,"MLA controlled search: %s\n",error.what());return FALSE;
    }
}

/* Legacy callers preserve their exact catalog, timings and controls. */
BOOL VHO_mla_explore_controlled(const VHO_TENSOR_PROGRAM &s,const VHO_TENSOR_GPU_PROFILE &g,
    const VHO_MLA_COST_PROFILE &m,const VHO_MLA_SEARCH_CONTROL &c,VHO_MLA_PRODUCTION_RESULT *o,FILE *f) {
    return VHO_mla_controlled(s,g,m,c,o,f,NULL);
}
BOOL VHO_mla_explore_movement(const VHO_TENSOR_PROGRAM &s,const VHO_TENSOR_GPU_PROFILE &g,
    const VHO_MLA_COST_PROFILE &m,const VHO_MLA_SEARCH_CONTROL &c,const VHO_MLA_MOVEMENT_CONTEXT &x,
    VHO_MLA_PRODUCTION_RESULT *o,FILE *f) {return VHO_mla_controlled(s,g,m,c,o,f,&x);}

/* A separate entry prevents these analysis-only incumbents from entering the
 * archived FP32 materializer/selected-plan route. Shared portfolio only. */
BOOL VHO_mla_explore_quantized(const VHO_TENSOR_GPU_PROFILE &g,const VHO_MLA_COST_PROFILE &m,
    const VHO_MLA_SEARCH_CONTROL &c,const VHO_Q4_PROFILE &p,const VHO_Q4_SNAPSHOT &x,
    VHO_MLA_PRODUCTION_RESULT *o,FILE *f) {
    VHO_TENSOR_PROGRAM unused;return VHO_mla_controlled(unused,g,m,c,o,f,NULL,&p,&x);
}

/* This entry qualifies complete Q5 plans; archived Q4 reports remain studies. */
BOOL VHO_mla_explore_q5(const VHO_TENSOR_PROGRAM &p,const VHO_TENSOR_GPU_PROFILE &g,
    const VHO_MLA_COST_PROFILE &m,const VHO_MLA_SEARCH_CONTROL &c,const VHO_Q4_PROFILE &h,
    const VHO_Q4_SNAPSHOT &x,UINT64 log_cycles,VHO_MLA_PRODUCTION_RESULT *o,FILE *f) {
    if(!log_cycles) return FALSE;return VHO_mla_controlled(p,g,m,c,o,f,NULL,&h,&x,log_cycles);
}
