/* Copyright (C) 2026 Open64 Project */

/* LN3 owning-phase complete analytical plans, not executable normalization.
 * Reuses target hardware and VHO-owned evolution/candidate/cost/plan APIs; no reduction API.
 * Design: doc/AI-COMPILER-LAYERNORM-PROFITABILITY-v0.1.md. */
#ifndef opt_dsl_normalization_cost_INCLUDED
#define opt_dsl_normalization_cost_INCLUDED
#include "opt_dsl_normalization_plan.h"
#include "dsl_memory_hierarchy.h"
#include "dsl_opt_plan.h"

enum WOPT_DSL_NORMALIZATION_COST_STATUS {
  WOPT_DSL_NORM_COST_COMPLETE, WOPT_DSL_NORM_COST_PLAN_REFUSED,
  WOPT_DSL_NORM_COST_UNKNOWN, WOPT_DSL_NORM_COST_PROFILE_INVALID,
  WOPT_DSL_NORM_COST_BUDGET, WOPT_DSL_NORM_COST_OVERFLOW, WOPT_DSL_NORM_COST_RESOURCE
};
enum WOPT_DSL_NORMALIZATION_SELECTION_REASON {
  WOPT_DSL_NORM_NO_BASELINE, WOPT_DSL_NORM_INCOMPLETE_COST,
  WOPT_DSL_NORM_BASELINE_MINIMUM, WOPT_DSL_NORM_UNCERTAIN_BENEFIT,
  WOPT_DSL_NORM_ROBUST_BENEFIT
};

/* Three comparable C0/C1/C2 requests, one exact hardware timing envelope.
 * RF estimates are per-thread upper reservations for this analytical schedule,
 * NOT generated-register proof. max_jobs bounds expansion across all rows for
 * EACH plan; margin is reference cycles. All selections stay analysis-only.
 * Model: device-resident inputs ready, allocations live; scalar pointer
 * packaging uses private lane-coalesced F32 scratch with all-miss sector service
 * on the same global bandwidth engine. No RF-row retention/cache-hit credit. */
typedef struct {
  WOPT_DSL_NORMALIZATION_PLAN_REQUEST plans[3];
  DSL_SCALAR_EXECUTION_PROFILE profile;
  UINT32 registers_per_thread[3];
  UINT64 max_jobs, margin;
} WOPT_DSL_NORMALIZATION_COST_REQUEST;

/* Counts describe the declared lowering recipe; durations are a conditional
 * analytical envelope, never measured time. Components reconcile exactly to
 * nominal total. Lower bound ignores engine contention; upper bound serializes
 * every slow-corner job, so greedy-scheduler timing anomalies cannot forge a
 * confidence interval. Common IDs are ephemeral verified query-local identities. */
typedef struct {
  UINT32 plan_status, status, resident_ctas, candidate_id, plan_id;
  UINT64 jobs, global_logical_bytes, global_sector_bytes, carrier_sector_bytes,
      shared_bytes, scalar_lanes, scalar_instructions, control_instructions, barriers;
  UINT64 lower, total, upper, terms[DSL_OPT_COST_TERM_COUNT];
} WOPT_DSL_NORMALIZATION_COST;
typedef struct {
  WOPT_DSL_NORMALIZATION_CHARACTERIZATION source;
  WOPT_DSL_NORMALIZATION_COST costs[3];
  UINT32 nominal_kind, selected_kind, fallback_kind, reason;
  DSL_OPT_SELECTION_RESULT common_selection;
  OPEN64_NORMALIZATION_GUARD selected_guard;
  /* Selection evidence survives unchanged numeric choices; never a runtime ABI. */
  DSL_GPU_PLAN_STAMP gpu_stamp;
  UINT64 gpu_live_bytes;
} WOPT_DSL_NORMALIZATION_COST_RESULT;

/* Fresh active-PU rebinding and strict comparability checks before any output.
 * Invalid scope/member/API or cross-plan mismatch returns FALSE with unchanged
 * output/trace. Valid source-v1/unknown/resource/budget queries return TRUE with
 * refusal/incompleteness and no selection. Imported capacity/SM bounds replace
 * synthetic resource inputs; instruction costs remain an explicitly separate
 * reference_cycles scenario. Missing physical requirements cannot win. HBM ns
 * service is reported separately and is never added to that scenario.
 * Records remain read-only/analysis-
 * only; export does not discharge runtime or selected-lowering obligations. */
extern BOOL WOPT_DSL_select_normalization_plan(WN *scope, WN *assignment,
    const WOPT_DSL_NORMALIZATION_COST_REQUEST *request,
    WOPT_DSL_NORMALIZATION_COST_RESULT *output, FILE *diagnostic, FILE *trace);
#endif
