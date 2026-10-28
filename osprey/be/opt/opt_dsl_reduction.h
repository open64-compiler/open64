/*
 * Copyright (C) 2026 Open64 Project
 */

/* PU-local direct ordered reduction target/storage/event binding.
 * Design: doc/AI-COMPILER-REDUCTION-DIRECT-BINDING-v0.1.md.
 * Transient planning contracts only; no mapped state or executable selection. */
#ifndef opt_dsl_reduction_INCLUDED
#define opt_dsl_reduction_INCLUDED

#include "opt_dsl.h"

enum WOPT_DSL_REDUCTION_INSTRUCTION {
  WOPT_DSL_REDUCE_INSTRUCTION_UNKNOWN,
  WOPT_DSL_REDUCE_PTX_ADD_RN_F32
};
enum WOPT_DSL_REDUCTION_LEASE {
  WOPT_DSL_REDUCE_LEASE_UNKNOWN,
  WOPT_DSL_REDUCE_INPUT_SNAPSHOT,
  WOPT_DSL_REDUCE_OUTPUT_EXCLUSIVE
};
enum WOPT_DSL_REDUCTION_EVENT {
  WOPT_DSL_REDUCE_INPUT_READY,
  WOPT_DSL_REDUCE_LAUNCH,
  WOPT_DSL_REDUCE_COMPLETE,
  WOPT_DSL_REDUCE_CONSUME,
  WOPT_DSL_REDUCE_INPUT_RELEASE,
  WOPT_DSL_REDUCE_OUTPUT_RELEASE,
  WOPT_DSL_REDUCE_EVENT_COUNT
};
enum WOPT_DSL_REDUCTION_DIRECT_STATUS {
  WOPT_DSL_REDUCE_DIRECT_BOUND,
  WOPT_DSL_REDUCE_DIRECT_SOURCE_UNBOUND,
  WOPT_DSL_REDUCE_DIRECT_STALE_SOURCE,
  WOPT_DSL_REDUCE_DIRECT_TARGET_UNKNOWN,
  WOPT_DSL_REDUCE_DIRECT_INSTRUCTION_UNSUPPORTED,
  WOPT_DSL_REDUCE_DIRECT_FP_OPTIONS_UNSAFE,
  WOPT_DSL_REDUCE_DIRECT_LAYOUT_UNSUPPORTED,
  WOPT_DSL_REDUCE_DIRECT_RANGE_INVALID,
  WOPT_DSL_REDUCE_DIRECT_LEASE_MISSING,
  WOPT_DSL_REDUCE_DIRECT_ALIAS_OVERLAP,
  WOPT_DSL_REDUCE_DIRECT_OWNER_INVALID,
  WOPT_DSL_REDUCE_DIRECT_EVENTS_INVALID
};
enum {
  WOPT_DSL_REDUCE_FP_FTZ = 1U << 0,
  WOPT_DSL_REDUCE_FP_FAST_MATH = 1U << 1,
  WOPT_DSL_REDUCE_FP_REASSOCIATE = 1U << 2,
  WOPT_DSL_REDUCE_FP_WIDEN = 1U << 3,
  WOPT_DSL_REDUCE_FP_OBSERVABLE_EXCEPTIONS = 1U << 4,
  WOPT_DSL_REDUCE_FP_FINITE_ONLY = 1U << 5,
  WOPT_DSL_REDUCE_ROW_LINEAR_THREAD = 1,
  WOPT_DSL_REDUCE_BUFFER_DEVICE_GLOBAL = 1,
  /* A validated compiler contract is not an observed runtime proof. */
  WOPT_DSL_REDUCE_RUNTIME_CANONICAL_ALLOCATIONS = 1U << 0,
  WOPT_DSL_REDUCE_RUNTIME_LIVE_LEASES = 1U << 1,
  WOPT_DSL_REDUCE_RUNTIME_EVENT_ENFORCEMENT = 1U << 2,
  WOPT_DSL_REDUCE_RUNTIME_LAUNCH_CONFORMANCE = 1U << 3
};

/* Allocation IDs are canonical storage-object identities within one context/
 * invocation contract, not WHIRL symbols or sampled pointer addresses.
 * Offsets and capacities are bytes; alignment is the base-address guarantee. */
typedef struct {
  UINT64 allocation_id;
  UINT64 allocation_bytes;
  UINT64 offset_bytes;
  UINT32 base_alignment;
  UINT32 lease;
  UINT32 memory_space;
} WOPT_DSL_REDUCTION_BUFFER_BINDING;

/* Supplied by target/host-plan construction, never by diagnostic metadata.
 * Runtime provider must enforce the nonzero contract/context and lease/event
 * semantics. before[i] is a mask of events immediately following event i. */
typedef struct {
  WOPT_DSL_REDUCTION_CHARACTERIZATION expected_source;
  const char *architecture;
  UINT32 instruction;
  UINT32 fp_options;
  UINT64 runtime_contract_id;
  UINT64 context_id;
  WOPT_DSL_REDUCTION_BUFFER_BINDING input;
  WOPT_DSL_REDUCTION_BUFFER_BINDING output;
  UINT32 row_mapping;
  UINT64 launched_threads;
  UINT32 before[WOPT_DSL_REDUCE_EVENT_COUNT];
} WOPT_DSL_REDUCTION_DIRECT_REQUEST;

/* Native source is always freshly derived. Rejected requests publish status
 * and validated-prefix stages, but grant no runtime mask. Success still needs runtime
 * obligations, selected lowering and instruction/native certification. */
typedef struct {
  WOPT_DSL_REDUCTION_CHARACTERIZATION source;
  UINT32 status;
  UINT32 architecture_id;
  UINT32 runtime_obligations;
  UINT64 input_bytes;
  UINT64 output_bytes;
  UINT32 stages_bound;
  UINT32 instruction;
  UINT32 fp_options;
  UINT64 runtime_contract_id;
  UINT64 context_id;
  WOPT_DSL_REDUCTION_BUFFER_BINDING input;
  WOPT_DSL_REDUCTION_BUFFER_BINDING output;
  UINT32 row_mapping;
  UINT64 launched_threads;
  UINT32 before[WOPT_DSL_REDUCE_EVENT_COUNT];
} WOPT_DSL_REDUCTION_DIRECT_BINDING;

/* Invalid scope/member/null arguments leave output unchanged. A well-formed
 * native source with a refused request returns TRUE and an explicit status.
 * Whole active PU only; no tree/table mutation, selection or cost model.
 * Caller controls trace through existing AI trace policy when integrated. */
extern BOOL WOPT_DSL_bind_reduction_direct
    (WN *scope, WN *assignment, const WOPT_DSL_REDUCTION_DIRECT_REQUEST *request,
     WOPT_DSL_REDUCTION_DIRECT_BINDING *output, FILE *diagnostic, FILE *trace);

#endif
