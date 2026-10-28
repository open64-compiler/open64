/* Ordered staging binding; no executable selection or mapped state.
 * Design: doc/AI-COMPILER-REDUCTION-STAGED-BINDING-v0.1.md. */
#ifndef opt_dsl_reduction_staged_INCLUDED
#define opt_dsl_reduction_staged_INCLUDED

#include "opt_dsl_reduction.h"

enum WOPT_DSL_REDUCTION_STAGED_STATUS {
  WOPT_DSL_REDUCE_STAGED_BOUND,
  WOPT_DSL_REDUCE_STAGED_BASE_REFUSED,
  WOPT_DSL_REDUCE_STAGED_CHUNK_INVALID,
  WOPT_DSL_REDUCE_STAGED_COPY_UNSUPPORTED,
  WOPT_DSL_REDUCE_STAGED_STORAGE_INVALID,
  WOPT_DSL_REDUCE_STAGED_OWNER_INVALID,
  WOPT_DSL_REDUCE_STAGED_CAPACITY,
  WOPT_DSL_REDUCE_STAGED_EVENTS_INVALID,
  WOPT_DSL_REDUCE_STAGED_REUSE_INVALID,
  WOPT_DSL_REDUCE_STAGED_TAIL_UNSUPPORTED
};
enum {
  WOPT_DSL_REDUCE_STAGED_SHARED = 2,
  WOPT_DSL_REDUCE_COPY_SYNC_BITS32 = 1,
  WOPT_DSL_REDUCE_VISIBLE_SAME_THREAD = 1,
  WOPT_DSL_REDUCE_TAIL_EXACT_COPY_READ = 1,
  WOPT_DSL_REDUCE_SLOT_ACQUIRE = 0,
  WOPT_DSL_REDUCE_SLOT_COPY = 1,
  WOPT_DSL_REDUCE_SLOT_VISIBLE = 2,
  WOPT_DSL_REDUCE_SLOT_READ = 3,
  WOPT_DSL_REDUCE_SLOT_RELEASE = 4,
  WOPT_DSL_REDUCE_SLOT_EVENT_COUNT = 5,
  WOPT_DSL_REDUCE_RUNTIME_SCRATCH_ALLOCATION = 16,
  WOPT_DSL_REDUCE_RUNTIME_COPY_VISIBILITY = 32,
  WOPT_DSL_REDUCE_RUNTIME_SLOT_LIFETIME = 64,
  WOPT_DSL_REDUCE_RUNTIME_EXACT_TAIL_ORDER = 128
};

/* One CTA-shared object, partitioned into one disjoint row-thread slot each.
 * Budgets are bytes. before[] orders events within generation g; next[] orders
 * g -> g+1. Initial support requires release(g) -> acquire(g+1), no other carry.
 * No cooperative producer, cross-thread visibility, async engine or second slot.
 * Source v2 supplies the unchanged single accumulator and ascending read order. */
typedef struct {
  UINT64 chunk_elements, scratch_object, scratch_offset, row_stride;
  UINT64 allocation_bytes, per_row_budget, cta_budget, other_shared_bytes;
  UINT64 rows_per_cta, cta_count;
  UINT32 slots, memory_space, copy_mode, visibility, tail_policy;
  UINT32 before[WOPT_DSL_REDUCE_SLOT_EVENT_COUNT];
  UINT32 next[WOPT_DSL_REDUCE_SLOT_EVENT_COUNT];
} WOPT_DSL_REDUCTION_STAGING;

typedef struct {
  WOPT_DSL_REDUCTION_DIRECT_REQUEST base;
  WOPT_DSL_REDUCTION_STAGING staging;
} WOPT_DSL_REDUCTION_STAGED_REQUEST;

/* Owned transient contract, conditional on target/runtime/codegen obligations.
 * Hardware shared-budget/quantum/headroom fields are existing planning-envelope
 * data, not probed CUDA limits or an occupancy/profitability result. */
typedef struct {
  WOPT_DSL_REDUCTION_DIRECT_BINDING base;
  WOPT_DSL_REDUCTION_STAGING staging;
  UINT32 status, runtime_obligations;
  UINT64 chunk, chunks, last_count, last_active_rows, row_bytes;
  UINT64 scratch_end, reserved_bytes, hardware_budget, headroom, quantum, max_threads;
} WOPT_DSL_REDUCTION_STAGED_BINDING;

/* Native-invalid/null arguments leave output unchanged. Source-valid refusals
 * return TRUE with a reason and no granted runtime mask. Active whole PU only;
 * no mutation, runtime discharge, cost decision or CUDA template. */
extern BOOL WOPT_DSL_bind_reduction_staged
    (WN *scope, WN *assignment, const WOPT_DSL_REDUCTION_STAGED_REQUEST *request,
     WOPT_DSL_REDUCTION_STAGED_BINDING *output, FILE *diagnostic, FILE *trace);

typedef struct {
  UINT64 row, generation, base, count, input_offset, scratch_offset, output_offset;
} WOPT_DSL_REDUCTION_STAGED_SLICE;

/* Revalidate current native binding and derived extents before describing one
 * active row/generation. Refuse inactive owners, past-tail or stale/tampered
 * derived snapshots without touching output. O(1), never unroll a large domain.
 * A slice is a planning witness, NOT an executed copy or lowered instruction. */
extern BOOL WOPT_DSL_reduction_staged_slice
    (WN *scope, WN *assignment, const WOPT_DSL_REDUCTION_STAGED_BINDING *binding,
     UINT64 row, UINT64 generation, WOPT_DSL_REDUCTION_STAGED_SLICE *output,
     FILE *diagnostic, FILE *trace);

#endif
