/* Copyright (C) 2026 Open64 Project */
/* PU-local estimation-only MLA exploration and strategy portfolio. Candidate
 * schedules are detached records; only the selected recipe is materialized by
 * the driver. No WN/ST/TY, CUDA or compiler-context allocation occurs here.
 * Design: doc/AI-COMPILER-MLA-TWO-PHASE-SEARCH-v0.1.md. */
#ifndef tensor_attention_search_INCLUDED
#define tensor_attention_search_INCLUDED
#include "tensor_kernel_lower.h"
#include "tensor_attention_movement.h"
#include "tensor_quantized_movement.h"
#include "tensor_quantized_plan.h"

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
    VHO_MLA_MOVEMENT_STAGE movement;
    /* Zero detached counts and costs without allocating compiler state. */
    VHO_MLA_STAGE_ESTIMATE():operation(0),ctas(0),threads(0),shared(0),resident(0),scalar_ops(0),global_bytes(0),shared_bytes(0),
        shuffles(0),exps(0),divs(0),barriers(0),fences(0),dependency(0),rounds(0),compute(0),memory(0),special(0),local(0),sync(0),launch(0),total(0) {}
    UINT64 operation,ctas,threads,shared,resident,scalar_ops,global_bytes,shared_bytes,shuffles,exps,divs,barriers,fences,dependency;
    UINT64 rounds,compute,memory,special,local,sync,launch,total;
};
struct VHO_MLA_SEARCH_CANDIDATE {
    VHO_MLA_MOVEMENT_DESCRIPTOR descriptor;
    VHO_MLA_MOVEMENT_ESTIMATE movement;
    VHO_Q4_ESTIMATE quantized;
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

/* Version-two controls are separate from the archived per-run v1 contract.
 * Global means one bound PU search, shared across all its strategy runs.
 * Design: doc/AI-COMPILER-SEARCH-M3-CONTROLS-v1.0.md. */
struct VHO_MLA_SEARCH_RUN_CONTROL {
    std::string id, strategy;
    UINT64 seed, proposal_limit;
    /* Deterministic, bounded independent proposal state. */
    VHO_MLA_SEARCH_RUN_CONTROL() : seed(17), proposal_limit(12) {}
};
struct VHO_MLA_SEARCH_CONTROL {
    std::string space, mode;
    std::vector<VHO_MLA_SEARCH_RUN_CONTROL> runs;
    std::vector<std::string> stopping;
    UINT64 global_proposals, global_unique, global_time_us;
    UINT64 patience, minimum_gain, target, gap_ppm, remaining_invocations, evaluation_cost;
    bool economic_conversion;
    UINT64 host_us, cycles_per_us;
    std::string conversion_provenance;
    /* Complete estimation is default; JSON v2 must supply every field. */
    VHO_MLA_SEARCH_CONTROL();
};
struct VHO_MLA_SEARCH_EVENT {
    UINT64 proposal, unique, run_best, global_best;
    UINT32 run, candidate;
    bool cache_hit, eligible;
};
struct VHO_MLA_PRODUCTION_RESULT {
    VHO_MLA_SEARCH_RESULT exploration;
    UINT64 proposals, elapsed_us;
    bool has_winner;
    std::string termination;
    std::vector<std::string> predicates;
    std::vector<std::vector<std::string> > run_predicates;
    std::vector<VHO_MLA_SEARCH_EVENT> events;
    /* No incumbent is a reportable bounded outcome, never a fake winner. */
    VHO_MLA_PRODUCTION_RESULT() : proposals(0), elapsed_us(0), has_winner(false) {}
};
/* Strict finite controls, including explicit compatible economic conversion.
 * Unknown providers/certificates are refused before proposal generation. */
extern BOOL VHO_mla_search_control_valid(const VHO_MLA_SEARCH_CONTROL &,FILE *);
/* Full framed policy identity; no elapsed time or compact hash substitutes for
 * source/resource/model/space/control binding at the owning native boundary. */
extern std::string VHO_mla_search_control_key(const VHO_MLA_SEARCH_CONTROL &);
/* Fair interleaving with hard global caps and a single lazy estimate cache.
 * Deadlines are checked cooperatively before/after each atomic proposal; they
 * do not preempt an in-flight estimate. No native context/callback is accepted.
 * Invalid input/clock leaves output unchanged; no incumbent publishes counts. */
extern BOOL VHO_mla_explore_controlled(const VHO_TENSOR_PROGRAM &,const VHO_TENSOR_GPU_PROFILE &,
    const VHO_MLA_COST_PROFILE &,const VHO_MLA_SEARCH_CONTROL &,VHO_MLA_PRODUCTION_RESULT *,FILE *);
/* V3 reuses the same portfolio/cache/global budgets with a separately bound
 * model/catalog. Only proved executable G1 entries may become incumbents. */
extern std::string VHO_mla_movement_descriptor_key(const VHO_MLA_SEARCH_CANDIDATE &);
extern BOOL VHO_mla_movement_control_valid(const VHO_MLA_SEARCH_CONTROL &,FILE *);
extern BOOL VHO_mla_explore_movement(const VHO_TENSOR_PROGRAM &,const VHO_TENSOR_GPU_PROFILE &,
    const VHO_MLA_COST_PROFILE &,const VHO_MLA_SEARCH_CONTROL &,const VHO_MLA_MOVEMENT_CONTEXT &,
    VHO_MLA_PRODUCTION_RESULT *,FILE *);
/* Q4 comparison only. Eligibility means model/resource admissibility within
 * this separate space; all entries are analysis-only, including its incumbent.
 * Caller freezes/revalidates outside search. Output unchanged on admission error. */
extern BOOL VHO_mla_explore_quantized(const VHO_TENSOR_GPU_PROFILE &,const VHO_MLA_COST_PROFILE &,
    const VHO_MLA_SEARCH_CONTROL &,const VHO_Q4_PROFILE &,const VHO_Q4_SNAPSHOT &,
    VHO_MLA_PRODUCTION_RESULT *,FILE *);
/* Q5 complete-plan costs share the same finite portfolio/global budgets. The
 * owning runtime freezes/revalidates Q3 outside exploration; only one winner
 * may later enter native High construction and reference qualification. */
extern BOOL VHO_mla_explore_q5(const VHO_TENSOR_PROGRAM &,const VHO_TENSOR_GPU_PROFILE &,
    const VHO_MLA_COST_PROFILE &,const VHO_MLA_SEARCH_CONTROL &,const VHO_Q4_PROFILE &,
    const VHO_Q4_SNAPSHOT &,UINT64,VHO_MLA_PRODUCTION_RESULT *,FILE *);
#endif
