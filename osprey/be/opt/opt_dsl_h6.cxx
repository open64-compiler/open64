/* Copyright (C) 2026 Open64 Project */
/* WOPT H6 harvest, owned finite search and selected-only preparation. Reuses
 * LN2/LN4/R2, H2 and H3, with no alternate source matcher or phase traversal.
 * Design: doc/AI-COMPILER-H6-NORM-REDUCTION-v1.0.md. */
#include "opt_dsl_h6.h"
#include "opt_dsl_movement_internal.h"
#include "config_dsl.h"
#include <sstream>
#include <set>

namespace {
/* Preserve release refusal and checked positive byte/count arithmetic. */
uint64_t H6_add(uint64_t a,uint64_t b) {return WOPT_DSL_movement_sum(a,b);}
/* Overflow cannot produce a cheap winning recipe. */
uint64_t H6_mul(uint64_t a,uint64_t b) {return WOPT_DSL_movement_product(a,b);}
/* Positive-divisor ceiling does not overflow at large source widths. */
uint64_t H6_ceil(uint64_t a,uint64_t b) {if(!b) throw std::runtime_error("h6_zero_divisor");return a/b+(a%b!=0);}
/* H20 identity and imported finite request ownership are mandatory. */
bool H6_ready() {const DSL_GPU_PLAN_SNAPSHOT g=DSL_GPU_PLAN_SNAPSHOT::Capture(VHO_DSL_GPU_Context);return Opt_Level>=3 && VHO_DSL_H3_Search && g.Bound() && (g.Device()=="h20_96gb" || g.Device()=="h20_141gb");}
/* Bind every admitted plan field through the existing all-u64 guard export,
 * excluding borrowed architecture pointers and native request/profile padding. */
std::string H6_norm_key(WN *scope,WN *node,const WOPT_DSL_NORMALIZATION_COST_REQUEST &r) {
    if(!WOPT_DSL_normalization_requests_comparable(&r)) throw std::runtime_error("h6_incomparable_normalization");
    std::ostringstream s;s<<r.max_jobs<<';'<<r.margin<<';';
    s<<r.profile.version<<';'<<r.profile.arch_id<<';'<<r.profile.description_revision<<';'<<r.profile.model_id<<';';
    for(unsigned i=0;i<DSL_SCALAR_FIELD_COUNT;++i) {
        s<<r.profile.provenance[i]<<';';for(unsigned j=0;j<3;++j) s<<r.profile.values[i][j]<<';';
    }
    for(unsigned i=0;i<3;++i) {
        OPEN64_NORMALIZATION_GUARD guard;
        if(!WOPT_DSL_export_normalization_guard(scope,node,&r.plans[i],&guard,NULL,NULL)) throw std::runtime_error("h6_unbound_normalization_control");
        s<<r.registers_per_thread[i]<<';';s.write((const char *)&guard,sizeof(guard));
    }
    return s.str();
}
/* Bind every direct source/runtime control without retaining architecture's
 * borrowed C string. Actual source/range facts are separately compared by H2. */
std::string H6_reduce_key(const WOPT_DSL_REDUCTION_DIRECT_REQUEST &r) {
    std::ostringstream s;s<<(r.architecture?r.architecture:"")<<';'<<r.instruction<<';'<<r.fp_options<<';'<<r.runtime_contract_id<<';'<<r.context_id<<';'<<r.row_mapping<<';'<<r.launched_threads;
    for(unsigned i=0;i<WOPT_DSL_REDUCE_EVENT_COUNT;++i) s<<';'<<r.before[i];return s.str();
}
/* Worst aligned-F32 residue is explicit: no unproved 32B base alignment or L1
 * hit is assumed. Count warp instruction sectors, not a source-byte rewrite.
 * Enumerate only <=32 k residues and <=32 lanes, independent of tensor width. */
uint64_t H6_strided(uint64_t rows,uint64_t width,uint64_t quantum) {
    uint64_t worst=0,period=quantum/4;
    for(uint64_t offset=0;offset<quantum;offset+=4) {
        uint64_t total=0;
        for(uint64_t warp=0;warp<rows;warp+=32) for(uint64_t k=0;k<std::min(width,period);++k) {
            std::set<uint64_t> sectors;
            for(uint64_t lane=warp;lane<std::min(rows,warp+32);++lane) sectors.insert(H6_add(H6_mul(H6_add(H6_mul(lane,width),k),4),offset)/quantum);
            uint64_t repeats=width/period+(k<width%period);total=H6_add(total,H6_mul(H6_mul(sectors.size(),quantum),repeats));
        }
        worst=std::max(worst,total);
    }return worst;
}
/* Contiguous warp loads, including last active lanes; worst base residue is
 * shared by every candidate. Instruction boundaries remain 32 F32 elements. */
uint64_t H6_contiguous(uint64_t width,uint64_t quantum) {
    uint64_t total=0;
    for(uint64_t count=0;count<2;++count) {
        uint64_t n=count?width%32:32,copies=count?(n!=0):width/32;
        if(copies) total=H6_add(total,H6_mul(copies,H6_mul(H6_ceil(H6_add(H6_mul(n,4),quantum-4),quantum),quantum)));
    }return total;
}
/* Construct a detached LN4 work envelope; C1 has FOUR global input sweeps,
 * C2 three, C0 five. Carrier traffic is an explicit conservative private-to-
 * global service scenario, not a spill-free or physical-placement claim. */
DSL_H3_PLAN H6_norm_recipe(const DSL_MOVEMENT_FACTS &f,const WOPT_DSL_NORMALIZATION_COST_REQUEST &r,UINT32 kind,UINT32 threads) {
    const uint64_t n=f.Capture().work.width,rows=f.Capture().work.instances,q=VHO_DSL_H3_Search->Model().transaction_bytes;
    DSL_H3_PLAN p;std::ostringstream id;id<<"H6/LayerNorm/C"<<kind-1<<"/T"<<threads;p.id=id.str();p.capability="LN4-centered-ordered-pointer-scalar";p.executable=true;
    p.live_bytes=0;std::set<std::string> allocations;
    for(size_t i=0;i<f.Capture().storages.size();++i) {const DSL_MOVEMENT_STORAGE &s=f.Capture().storages[i];if(allocations.insert(s.allocation).second) p.live_bytes=H6_add(p.live_bytes,s.capacity);}
    DSL_H3_STAGE s;s.id=p.id;s.ctas=rows;s.threads=threads;s.registers=r.registers_per_thread[kind-1];s.shared_allocation=kind==1?0:r.plans[kind-1].shared_bytes;
    uint64_t scalar=H6_add(H6_mul(kind==1?8:7,n),5);
    // Binary packaging: three 4B reads and three 4B writes. Unary sqrt:
    // two reads and two writes. Pay one full sector per lane access; no
    // unproved local-memory coalescing, register promotion or spill-free credit.
    uint64_t carrier_accesses=H6_add(H6_mul(scalar-1,3),2);
    uint64_t carrier_bytes=H6_mul(carrier_accesses,4),carrier_tx=H6_mul(carrier_accesses,q);
    s.read_requests=H6_add(H6_mul(H6_mul(kind==1?5:kind==2?4:3,n),4),carrier_bytes);s.write_through_requests=H6_add(H6_mul(n,4),carrier_bytes);
    s.read_transactions=H6_add(carrier_tx,kind==1?H6_mul(H6_mul(5,n),q):H6_add(kind==2?H6_mul(n,q):0,H6_mul(3,H6_contiguous(n,q))));
    s.write_transactions=H6_add(kind==1?H6_mul(n,q):H6_contiguous(n,q),carrier_tx);
    s.transaction_proof="H6-LN4-v1/F32/worst-4B-base-residue/warp32/serial-statistics/cooperative-retained/carriers-conservative-global-service";
    s.scalar_ops=scalar;s.issue_ops=H6_add(scalar,H6_add(s.read_requests,s.write_through_requests)/4);s.add_chain=H6_mul(kind==1?3:2,n);s.mul_chain=n;s.divs=3;s.rsqrts=1;
    s.steps=n;s.barriers=kind==1?0:kind==2?4:5;
    s.shared_requests=kind==1?0:H6_add(H6_mul(kind==2?12:24,n),H6_add(8,H6_mul(8,std::min<uint64_t>(n,threads))));
    p.stages.push_back(s);return p;
}
/* Direct row-thread traffic is strided across lanes; cooperative CTA-per-row
 * stages a contiguous panel and preserves one ascending accumulator. Full and
 * partial CTA classes pay exact active rows and only one kernel launch. */
DSL_H3_PLAN H6_reduce_recipe(const DSL_MOVEMENT_FACTS &f,UINT32 kind,UINT32 threads,UINT32 chunk) {
    uint64_t rows=f.Capture().work.instances,n=f.Capture().work.width,q=VHO_DSL_H3_Search->Model().transaction_bytes;
    DSL_H3_PLAN p;std::ostringstream id;id<<"H6/Reduction/K"<<kind<<"/T"<<threads<<"/C"<<chunk;p.id=id.str();p.capability="ordered-F32-direct-or-cooperative-panel";p.executable=true;p.live_bytes=0;
    std::set<std::string> allocations;for(size_t i=0;i<f.Capture().storages.size();++i) {const DSL_MOVEMENT_STORAGE &s=f.Capture().storages[i];if(allocations.insert(s.allocation).second) p.live_bytes=H6_add(p.live_bytes,s.capacity);}
    for(unsigned tail=0;tail<(kind==1?2U:1U);++tail) {
        uint64_t active=kind==1?(tail?rows%threads:threads):1,ctas=kind==1?(tail?(active!=0):rows/threads):rows;if(!ctas) continue;
        DSL_H3_STAGE s;s.id=p.id+(tail?"/tail":"/full");s.ctas=ctas;s.threads=threads;s.registers=WOPT_DSL_H6_REDUCTION_REGISTERS;s.launch=p.stages.empty();
        s.read_requests=H6_mul(H6_mul(active,n),4);s.write_through_requests=H6_mul(active,4);
        s.read_transactions=kind==1?H6_strided(active,n,q):H6_contiguous(n,q);
        s.write_transactions=kind==1?H6_contiguous(active,q):q;
        s.transaction_proof="H6-R2-v1/F32/warp32/worst-4B-residue/ascending-fold/guarded-active-rows-and-elements";
        s.scalar_ops=H6_mul(active,n);s.issue_ops=H6_add(s.scalar_ops,H6_add(s.read_requests,s.write_through_requests)/4);s.add_chain=n;
        s.steps=kind==1?n:H6_ceil(n,chunk);s.shared_allocation=kind==1?0:H6_mul(chunk,4);s.shared_requests=kind==1?0:H6_mul(n,8);s.barriers=kind==1?0:H6_mul(2,s.steps);
        p.stages.push_back(s);
    }return p;
}
/* Share the driver H3 budgets; incomplete comparisons cannot select a new
 * implementation. No native service, callback, execution or model mutation. */
bool H6_search(WOPT_DSL_H6_SELECTION &s,FILE *trace) {
    uint64_t best=~uint64_t(0);s.complete=true;
    for(size_t i=0;i<s.candidates.size();++i) {std::string why;WOPT_DSL_H6_CANDIDATE &c=s.candidates[i];
        if(trace) for(size_t j=0;j<c.recipe.stages.size();++j) {
            const DSL_H3_STAGE &w=c.recipe.stages[j];
            fprintf(trace,"AI_H6_WORK 1 plan=%s stage=%lu ctas=%llu threads=%llu read=%llu write=%llu read_tx=%llu write_tx=%llu shared=%llu barriers=%llu proof=%s\n",c.recipe.id.c_str(),(unsigned long)j,(unsigned long long)w.ctas,(unsigned long long)w.threads,(unsigned long long)w.read_requests,(unsigned long long)w.write_through_requests,(unsigned long long)w.read_transactions,(unsigned long long)w.write_transactions,(unsigned long long)w.shared_requests,(unsigned long long)w.barriers,w.transaction_proof.c_str());
        }
        bool ok=VHO_DSL_H3_Search->Evaluate(s.source,c.recipe,c.estimate,why);s.complete=s.complete && ok;VHO_h3_print(trace,s.source,c.recipe,c.estimate);
        if(ok && c.estimate.rank_ns<best) {best=c.estimate.rank_ns;s.winner=i;}}
    s.model=VHO_DSL_H3_Search->Model().key;
    if(trace) fprintf(trace,"AI_H6_SELECT 1 candidates=%lu complete=%u winner=%u selected_only=true native_search_mutation=none fallback=existing\n",(unsigned long)s.candidates.size(),unsigned(s.complete),s.winner);
    return s.complete;
}
/* Compare the full fixed recipe and physical binding without spending new
 * proposal/unique budget or restoring the completed source PU. */
bool H6_fixed(const WOPT_DSL_H6_SELECTION &s,const DSL_MOVEMENT_FACTS &fresh,const DSL_H3_PLAN &recipe) {
    if(!H6_ready() || !s.complete || s.winner>=s.candidates.size() || !s.source.Same_content(fresh) || s.model!=VHO_DSL_H3_Search->Model().key) return false;
    const WOPT_DSL_H6_CANDIDATE &c=s.candidates[s.winner];DSL_H3_ESTIMATE estimate;std::string why;
    if(!c.estimate.complete || !c.estimate.executable || !c.recipe.executable) return false;
    for(size_t i=0;i<s.candidates.size();++i) if(!s.candidates[i].estimate.complete ||
        s.candidates[i].estimate.rank_ns<c.estimate.rank_ns ||
        (i<s.winner && s.candidates[i].estimate.rank_ns==c.estimate.rank_ns)) return false;
    return VHO_h3_plan_key(recipe)==VHO_h3_plan_key(c.recipe) && VHO_h3_estimate(fresh,VHO_DSL_H3_Search->Model(),recipe,estimate,why) && estimate.key==c.estimate.key && estimate.binding==c.estimate.binding && estimate.rank_ns==c.estimate.rank_ns;
}
}
/* Native capture/admission precedes the detached loop; source recapture after
 * exploration proves current facts. No lowering or native allocation occurs. */
BOOL WOPT_DSL_select_h6_normalization(WN *scope,WN *node,const WOPT_DSL_NORMALIZATION_COST_REQUEST *r,WOPT_DSL_H6_SELECTION &out,FILE *trace) {
    out=WOPT_DSL_H6_SELECTION();if(!r || !H6_ready() || !VHO_DSL_H3_Search->Working_items(8388608)) return FALSE;
    try {WOPT_DSL_H6_SELECTION s;std::string why;if(!WOPT_DSL_capture_normalization_movement(scope,node,&r->plans[0],s.source,why)) return FALSE;s.controls=H6_norm_key(scope,node,*r);
        const UINT32 threads[]={128,32,64};
        for(unsigned t=0;t<3;++t) for(unsigned kind=1;kind<=3;++kind) {
            WOPT_DSL_NORMALIZATION_PLAN_REQUEST p=r->plans[kind-1];p.threads_per_cta=threads[t];if(kind>1) p.barrier_participants=threads[t];
            WOPT_DSL_NORMALIZATION_PLAN_BINDING b;if(!WOPT_DSL_bind_normalization_plan(scope,node,&p,&b,NULL,NULL) || b.status) return FALSE;
            WOPT_DSL_H6_CANDIDATE c;c.kind=kind;c.threads=threads[t];c.recipe=H6_norm_recipe(s.source,*r,kind,threads[t]);s.candidates.push_back(c);
        }
        bool ok=H6_search(s,trace);DSL_MOVEMENT_FACTS fresh;if(!WOPT_DSL_capture_normalization_movement(scope,node,&r->plans[0],fresh,why) || !s.source.Same_content(fresh)) return FALSE;
        out=s;return ok;
    } catch(const std::exception &) {return FALSE;}
}
/* Only the winner reaches the existing LN4 body. The selected-only legacy
 * graph supplies its honest cycle record; it is never used to rerank H3. */
BOOL WOPT_DSL_prepare_h6_normalization(WN *scope,WN *node,const WOPT_DSL_NORMALIZATION_COST_REQUEST *r,const WOPT_DSL_H6_SELECTION &s,WOPT_DSL_NORMALIZATION_LOWERING &out) {
    if(!r || !H6_ready() || s.winner>=s.candidates.size()) return FALSE;
    try {if(s.controls!=H6_norm_key(scope,node,*r)) return FALSE;DSL_MOVEMENT_FACTS f;std::string why;const WOPT_DSL_H6_CANDIDATE &c=s.candidates[s.winner];
        if(c.kind<1 || c.kind>3 || (c.threads!=32 && c.threads!=64 && c.threads!=128) || c.chunk ||
            !WOPT_DSL_capture_normalization_movement(scope,node,&r->plans[0],f,why) || !H6_fixed(s,f,H6_norm_recipe(f,*r,c.kind,c.threads))) return FALSE;
        WOPT_DSL_NORMALIZATION_COST_REQUEST fixed=*r;fixed.plans[c.kind-1].threads_per_cta=c.threads;if(c.kind>1) fixed.plans[c.kind-1].barrier_participants=c.threads;
        WOPT_DSL_NORMALIZATION_LOWERING p;if(!WOPT_DSL_prepare_normalization_fixed(scope,node,&fixed,c.kind,&p)) return FALSE;
        WOPT_DSL_seal_normalization_h6(p,s.model.c_str(),c.estimate.key.c_str(),c.estimate.rank_ns);out=p;return TRUE;
    } catch(const std::exception &) {return FALSE;}
}
#ifndef WOPT_DSL_H6_NORMALIZATION_ONLY
/* Reduction admission reuses R2's exact source/range/lease/event proof. */
BOOL WOPT_DSL_select_h6_reduction(WN *scope,WN *node,const WOPT_DSL_REDUCTION_DIRECT_REQUEST *r,WOPT_DSL_H6_SELECTION &out,FILE *trace) {
    out=WOPT_DSL_H6_SELECTION();if(!r || !H6_ready() || !VHO_DSL_H3_Search->Working_items(8388608)) return FALSE;
    try {WOPT_DSL_H6_SELECTION s;std::string why;if(!WOPT_DSL_capture_reduction_movement(scope,node,r,s.source,why)) return FALSE;s.controls=H6_reduce_key(*r);
        const UINT32 threads[]={128,32,64};for(unsigned i=0;i<3;++i) {WOPT_DSL_H6_CANDIDATE c;c.kind=1;c.threads=threads[i];c.recipe=H6_reduce_recipe(s.source,1,c.threads,0);s.candidates.push_back(c);}
        for(unsigned i=0;i<2;++i) {WOPT_DSL_H6_CANDIDATE c;c.kind=2;c.threads=i?128:32;c.chunk=c.threads;c.recipe=H6_reduce_recipe(s.source,2,c.threads,c.chunk);s.candidates.push_back(c);}
        bool ok=H6_search(s,trace);DSL_MOVEMENT_FACTS fresh;if(!WOPT_DSL_capture_reduction_movement(scope,node,r,fresh,why) || !s.source.Same_content(fresh)) return FALSE;out=s;return ok;
    } catch(const std::exception &) {return FALSE;}
}
/* Preserve the source guard's old row-thread obligations as provenance only;
 * H6's actual launch facts are separate and must be enforced by its caller. */
BOOL WOPT_DSL_prepare_h6_reduction(WN *scope,WN *node,const WOPT_DSL_REDUCTION_DIRECT_REQUEST *r,const WOPT_DSL_H6_SELECTION &s,WOPT_DSL_H6_REDUCTION &out) {
    if(!r || !H6_ready() || s.winner>=s.candidates.size() || s.controls!=H6_reduce_key(*r)) return FALSE;
    try {DSL_MOVEMENT_FACTS f;std::string why;const WOPT_DSL_H6_CANDIDATE &c=s.candidates[s.winner];
        if((c.kind!=1 && c.kind!=2) || (c.threads!=32 && c.threads!=64 && c.threads!=128) ||
            (c.kind==1?c.chunk!=0:(c.chunk!=c.threads || c.threads==64)) ||
            !WOPT_DSL_capture_reduction_movement(scope,node,r,f,why) || !H6_fixed(s,f,H6_reduce_recipe(f,c.kind,c.threads,c.chunk))) return FALSE;
        WOPT_DSL_REDUCTION_DIRECT_BINDING b;WOPT_DSL_H6_REDUCTION p;
        if(!WOPT_DSL_bind_reduction_direct(scope,node,r,&b,NULL,NULL) || b.status || !WOPT_DSL_export_reduction_direct_guard(scope,node,&b,&p.source_guard,NULL,NULL)) return FALSE;
        p.device=f.Capture().binding.device;p.model=s.model;p.binding=c.estimate.binding;p.rows=f.Capture().work.instances;p.width=f.Capture().work.width;p.kind=c.kind;p.threads=c.threads;p.chunk=c.chunk;p.seal=WOPT_DSL_h6_reduction_seal(p);out=p;return TRUE;
    } catch(const std::exception &) {return FALSE;}
}

#endif
