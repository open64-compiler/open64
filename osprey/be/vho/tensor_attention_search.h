/* Copyright (C) 2026 Open64 Project */
/* PU-local estimation-only MLA exploration and strategy portfolio. Candidate
 * schedules are detached records; only the selected recipe is materialized by
 * the driver. No WN/ST/TY, CUDA or compiler-context allocation occurs here.
 * Design: doc/AI-COMPILER-MLA-TWO-PHASE-SEARCH-v0.1.md. */
#ifndef tensor_attention_search_INCLUDED
#define tensor_attention_search_INCLUDED
#include "tensor_kernel_lower.h"

enum VHO_MLA_COST_FIELD {
    VHO_MLA_GLOBAL_RATE, VHO_MLA_SCALAR_RATE, VHO_MLA_SHARED_RATE,
    VHO_MLA_DEPENDENCY, VHO_MLA_EXP, VHO_MLA_DIV, VHO_MLA_SHUFFLE,
    VHO_MLA_CTA_BARRIER, VHO_MLA_WARP_FENCE, VHO_MLA_LAUNCH,
    VHO_MLA_LOAD_LATENCY, VHO_MLA_HIDING_WARPS, VHO_MLA_COST_FIELDS
};
extern const char *const VHO_mla_cost_names[VHO_MLA_COST_FIELDS];
struct VHO_MLA_COST_PROFILE {
    /* Unknown timings start at zero; admission requires explicit positive values. */
    VHO_MLA_COST_PROFILE() {memset(values,0,sizeof(values));}
    std::string id,architecture,provenance;
    UINT64 values[VHO_MLA_COST_FIELDS];
};
struct VHO_MLA_SEARCH_OPTIONS {
    /* Start with a deterministic bounded portfolio; stopping remains explicit. */
    VHO_MLA_SEARCH_OPTIONS():strategy("portfolio"),stop("exhausted"),budget(12),seed(17),patience(12),
        minimum_gain(0),target(0),gap_ppm(10000),remaining_invocations(0),evaluation_cost(0) {}
    std::string strategy,stop;
    UINT64 budget,seed,patience,minimum_gain,target,gap_ppm,remaining_invocations,evaluation_cost;
};
/* Counts are per CTA, costs cover the entire stage including grid rounds. */
struct VHO_MLA_STAGE_ESTIMATE {
    /* Zero detached counts and costs without allocating compiler state. */
    VHO_MLA_STAGE_ESTIMATE():operation(0),ctas(0),threads(0),shared(0),resident(0),scalar_ops(0),global_bytes(0),shared_bytes(0),
        shuffles(0),exps(0),divs(0),barriers(0),fences(0),dependency(0),rounds(0),compute(0),memory(0),special(0),local(0),sync(0),launch(0),total(0) {}
    UINT64 operation,ctas,threads,shared,resident,scalar_ops,global_bytes,shared_bytes,shuffles,exps,divs,barriers,fences,dependency;
    UINT64 rounds,compute,memory,special,local,sync,launch,total;
};
struct VHO_MLA_SEARCH_CANDIDATE {
    /* A specification is ineligible until its own estimate/preflight completes. */
    VHO_MLA_SEARCH_CANDIDATE():policy(6),width(0),evaluated(false),eligible(false),scratch(0),total(0),reason("not_evaluated") {}
    UINT32 policy,width;
    bool evaluated,eligible;
    UINT64 scratch,total;
    std::string reason;
    std::vector<VHO_MLA_STAGE_ESTIMATE> stages;
};
struct VHO_MLA_SEARCH_RUN {
    /* A zero best cost denotes that this run has no legal incumbent yet. */
    VHO_MLA_SEARCH_RUN():winner(0),best(0),first_best_trial(0),restarts(0) {}
    std::string strategy,termination;
    UINT32 winner;
    UINT64 best,first_best_trial,restarts;
    std::vector<UINT32> visited;
};
struct VHO_MLA_SEARCH_RESULT {
    /* Accumulate a private incomplete census; publish only after successful search. */
    VHO_MLA_SEARCH_RESULT():winner(0),best_run(0),evaluations(0),cache_hits(0),lower_bound(0),complete(false) {}
    UINT32 winner,best_run;
    UINT64 evaluations,cache_hits,lower_bound;
    bool complete;
    std::vector<VHO_MLA_SEARCH_CANDIDATE> candidates;
    std::vector<VHO_MLA_SEARCH_RUN> runs;
};
/* Validate explicit units/provenance/rates and bounded strategy options. No
 * unknown instruction timing is inferred from the architecture spelling. */
extern BOOL VHO_mla_search_inputs_valid(const VHO_MLA_COST_PROFILE &,const VHO_MLA_SEARCH_OPTIONS &,FILE *);
/* Explore the finite implemented family/width space with a shared lazy cost
 * cache. Only detached schedule preflight and existing occupancy/checked
 * arithmetic services are used. Output remains unchanged on failure. */
extern BOOL VHO_mla_explore(const VHO_TENSOR_PROGRAM &,const VHO_TENSOR_GPU_PROFILE &,
    const VHO_MLA_COST_PROFILE &,const VHO_MLA_SEARCH_OPTIONS &,VHO_MLA_SEARCH_RESULT *,FILE *);
#endif
