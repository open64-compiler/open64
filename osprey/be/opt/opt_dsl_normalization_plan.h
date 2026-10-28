/* Copyright (C) 2026 Open64 Project */

/* Active-PU LN2 conditional storage/event legality, not cost or lowering.
 * Design: doc/AI-COMPILER-LAYERNORM-STORAGE-EVENTS-v0.1.md.
 * Transient phase-owned contracts; no mapped-image layout or reduction API. */
#ifndef opt_dsl_normalization_plan_INCLUDED
#define opt_dsl_normalization_plan_INCLUDED
#include "opt_dsl_normalization_target.h"

enum WOPT_DSL_NORMALIZATION_PLAN_KIND { WOPT_DSL_NORM_C0 = 1, WOPT_DSL_NORM_C1, WOPT_DSL_NORM_C2 };
enum WOPT_DSL_NORMALIZATION_PLAN_STATUS {
  WOPT_DSL_NORM_PLAN_BOUND, WOPT_DSL_NORM_PLAN_TARGET_REFUSED,
  WOPT_DSL_NORM_PLAN_KIND_UNSUPPORTED, WOPT_DSL_NORM_PLAN_RUNTIME_UNBOUND,
  WOPT_DSL_NORM_PLAN_RANGE_INVALID, WOPT_DSL_NORM_PLAN_ALIAS_INVALID,
  WOPT_DSL_NORM_PLAN_OWNER_INVALID, WOPT_DSL_NORM_PLAN_STORAGE_INVALID,
  WOPT_DSL_NORM_PLAN_EVENTS_INVALID, WOPT_DSL_NORM_PLAN_LAYOUT_UNSUPPORTED
};
enum WOPT_DSL_NORMALIZATION_EVENT {
  WOPT_DSL_NORM_INPUT_READY, WOPT_DSL_NORM_LAUNCH, WOPT_DSL_NORM_ROW_READY,
  WOPT_DSL_NORM_MEAN_DONE, WOPT_DSL_NORM_MU_VISIBLE, WOPT_DSL_NORM_CENTERED_READY,
  WOPT_DSL_NORM_VARIANCE_DONE, WOPT_DSL_NORM_Q_VISIBLE, WOPT_DSL_NORM_EPILOGUE_DONE,
  WOPT_DSL_NORM_READERS_DONE, WOPT_DSL_NORM_REUSE_READY, WOPT_DSL_NORM_KERNEL_DONE,
  WOPT_DSL_NORM_RESULT_VISIBLE, WOPT_DSL_NORM_INPUT_RELEASE, WOPT_DSL_NORM_OUTPUT_RELEASE,
  WOPT_DSL_NORM_EVENT_COUNT
};
enum {
  WOPT_DSL_NORM_IMMUTABLE_INPUT = 1, WOPT_DSL_NORM_EXCLUSIVE_OUTPUT = 2,
  WOPT_DSL_NORM_CTA_PER_ROW_ORDERED_OWNER_ZERO = 1,
  WOPT_DSL_NORM_PREDICATE_VALID_BEFORE_ACCESS_ARITHMETIC = 1,
  WOPT_DSL_NORM_CONTIGUOUS_ROW_MAJOR_F32 = 1,
  WOPT_DSL_NORM_OBLIGATION_CANONICAL_STORAGE = 1U << 0,
  WOPT_DSL_NORM_OBLIGATION_LIVE_LEASES = 1U << 1,
  WOPT_DSL_NORM_OBLIGATION_EVENT_ENFORCEMENT = 1U << 2,
  WOPT_DSL_NORM_OBLIGATION_LAUNCH_CONFORMANCE = 1U << 3,
  WOPT_DSL_NORM_OBLIGATION_REGISTER_ALLOCATION = 1U << 4,
  WOPT_DSL_NORM_OBLIGATION_SELECTED_LOWERING = 1U << 5
};

/* Canonical device-global backing identity within runtime context/invocation.
 * IDs are not WN/ST identities or guessed pointers. Capacities/offsets are
 * bytes; alignment describes the backing base. Providers must enforce leases. */
typedef struct {
  UINT64 allocation_id, allocation_bytes, offset_bytes;
  UINT32 base_alignment, lease;
} WOPT_DSL_NORMALIZATION_RANGE;

/* One row per CTA, lane zero owns both ordered statistics. Retained plans use
 * scalar contiguous cooperative access and uniform barriers including tails.
 * before[i] is an immediate successor bitset; proof uses acyclic reachability.
 * Row stride covers N F4 values; shared mu/q follow at stride and stride+4.
 * C2 has X generation 0 then d generation 1 at the same physical row offset. */
typedef struct {
  WOPT_DSL_NORMALIZATION_TARGET_REQUEST target;
  UINT32 kind, layout;
  UINT64 runtime_contract_id, context_id;
  WOPT_DSL_NORMALIZATION_RANGE ranges[4]; /* X, gamma, beta, Y */
  UINT32 ownership, threads_per_cta, tail_policy;
  UINT64 launched_ctas, row_stride_bytes, shared_bytes;
  UINT32 barrier_participants, cta_barriers;
  UINT32 before[WOPT_DSL_NORM_EVENT_COUNT];
} WOPT_DSL_NORMALIZATION_PLAN_REQUEST;

/* Validated-prefix facts only: bound still needs real runtime enforcement,
 * launch/register checks, complete profitability and executable WHIRL lowering.
 * Scalar/RF resources, occupancy, GMEM transactions and latency are not proven. */
typedef struct {
  WOPT_DSL_NORMALIZATION_TARGET_BINDING target;
  UINT32 status, stages, runtime_obligations;
  UINT64 bytes[4], row_bytes, required_shared, reserved_shared, shared_limit, shared_quantum;
  UINT32 required_barriers, bank_count, bank_word_bytes, warp_lanes, bank_conflict_max;
} WOPT_DSL_NORMALIZATION_PLAN_BINDING;

/* Rebind exact active-PU source and target before checking any plan. Invalid
 * scope/member/null leaves output/trace untouched and returns FALSE. Valid
 * refused requests return TRUE with status and no runtime obligation grant.
 * Read-only; optional trace uses the caller's existing AI trace control. */
extern BOOL WOPT_DSL_bind_normalization_plan(WN *scope, WN *assignment,
    const WOPT_DSL_NORMALIZATION_PLAN_REQUEST *request,
    WOPT_DSL_NORMALIZATION_PLAN_BINDING *output, FILE *diagnostic, FILE *trace);

#endif
