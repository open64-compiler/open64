/* Guarded ordered-reduction runtime boundary.
 * Design: doc/AI-COMPILER-REDUCTION-RUNTIME-GUARDS-v0.1.md.
 * Additive, process-local ABI; not a mapped WHIRL record or device kernel. */
#ifndef OPEN64_REDUCTION_RUNTIME_H
#define OPEN64_REDUCTION_RUNTIME_H

#include <stdio.h>
#include "open64_target_buffer_service.h"
#include "open64_dsl_runtime_abi.h"

#ifdef __cplusplus
extern "C" {
#endif

enum {
    OPEN64_REDUCTION_GUARD_V1 = 1,
    OPEN64_REDUCTION_READY = 0, OPEN64_REDUCTION_LAUNCH = 1,
    OPEN64_REDUCTION_COMPLETE = 2, OPEN64_REDUCTION_CONSUME = 3,
    OPEN64_REDUCTION_INPUT_RELEASE = 4, OPEN64_REDUCTION_OUTPUT_RELEASE = 5,
    OPEN64_REDUCTION_EVENT_COUNT = 6,
    OPEN64_REDUCTION_OK = 0, OPEN64_REDUCTION_INVALID = 1,
    OPEN64_REDUCTION_CONFLICT = 2, OPEN64_REDUCTION_ORDER = 3,
    OPEN64_REDUCTION_BUSY = 4, OPEN64_REDUCTION_BACKEND = 5,
    OPEN64_REDUCTION_FAILED = 6, OPEN64_REDUCTION_CAPACITY = 7,
    OPEN64_REDUCTION_HOST_TEST = 1
};

typedef struct {
    uint64_t role, capacity, offset;
    uint32_t alignment, lease, memory_space, reserved;
} OPEN64_REDUCTION_RANGE_V1;

/* Exact scalar v2 source and conditional target contract, exported by WOPT.
 * arch uses common HW-description IDs (1..4), not target-profile ABI values.
 * All integers are host-native; no process-local WHIRL pointers are exported. */
typedef struct {
    uint32_t abi_version, size;
    uint32_t source_version, node, input_value, result_value, source_line;
    uint32_t architecture, instruction, fp_options, obligations, row_mapping;
    uint64_t rows, width, threads, contract, context, input_bytes, output_bytes;
    OPEN64_REDUCTION_RANGE_V1 input, output;
    uint32_t before[OPEN64_REDUCTION_EVENT_COUNT];
} OPEN64_REDUCTION_GUARD;

typedef struct OPEN64_REDUCTION_RUNTIME OPEN64_REDUCTION_RUNTIME;

/* Trusted provider verifies WHOLE-root extent and returns a nonzero backing-
 * object key, stable for the root lifetime and shared by all virtual aliases.
 * Uses TargetBufferService.context. Plain pointer inequality is not noalias.
 * Suballocated/aliased imports must resolve to their root, not register views. */
typedef int (*OPEN64_REDUCTION_ROOT_RESOLVER)
    (void *, const OPEN64_TARGET_BUFFER_VIEW *, uint64_t *backing_object);

typedef struct {
    OPEN64_REDUCTION_GUARD guard;
    OPEN64_TARGET_BUFFER_VIEW input, output; /* Exact accessed subranges. */
    uint64_t ticket;
} OPEN64_REDUCTION_INVOCATION;

/* Trusted adapter owns the entry, stream, input readiness, and consumer joins.
 * perform(READY/COMPLETE/CONSUME) waits; perform(LAUNCH) issues once. A failed
 * launch MAY have queued work: COMPLETE must still drain it before cleanup.
 * No callback may throw. Context/entry objects must outlive outstanding calls.
 * Host-test entries cannot enter a production (flags=0) context. A production
 * manifest is an adapter assertion, NOT instruction/native certification. */
typedef struct {
    OPEN64_REDUCTION_GUARD manifest;
    uint32_t flags;
    void *context;
    int (*perform)(void *, uint32_t, const OPEN64_REDUCTION_INVOCATION *);
} OPEN64_REDUCTION_ENTRY;

/* Reuses V1 validate_import; never changes V1 async or ownership semantics.
 * Context destruction must be externally synchronized; it refuses live objects.
 * APIs otherwise serialize registry/state changes and call adapters unlocked.
 * A NULL trace disables diagnostics only. flags is 0 or HOST_TEST. */
OPEN64_REDUCTION_RUNTIME *open64_reduction_runtime_create
    (const OPEN64_TARGET_BUFFER_SERVICE *, OPEN64_REDUCTION_ROOT_RESOLVER,
     uint32_t architecture,
     uint64_t context, uint32_t flags, FILE *trace);
int open64_reduction_runtime_destroy(OPEN64_REDUCTION_RUNTIME *);

/* Register canonical WHOLE allocations, not overlapping aliases/subviews.
 * Handles are runtime-minted, never reused, and context-local. Retirement must
 * precede external free/rebind; raw out-of-band accesses remain forbidden.
 * Failure leaves output handles unchanged. Fixed capacity: 64 live objects. */
int open64_reduction_register(OPEN64_REDUCTION_RUNTIME *,
    const OPEN64_TARGET_BUFFER_VIEW *, uint64_t *allocation);
int open64_reduction_retire(OPEN64_REDUCTION_RUNTIME *, uint64_t allocation);

/* Snapshot descriptors and entry, validate the exact guard, then atomically
 * acquire read-snapshot/write-exclusive range leases. No launch on refusal.
 * Descriptor blobs use the existing runtime tensor ABI, including bounds.
 * No implicit fallback. Caller must keep supplied blob/entry stable in call. */
int open64_reduction_admit(OPEN64_REDUCTION_RUNTIME *,
    const OPEN64_REDUCTION_GUARD *, const OPEN64_REDUCTION_ENTRY *,
    uint64_t input_allocation, uint64_t output_allocation,
    const void *input_descriptor, size_t input_size,
    const void *output_descriptor, size_t output_size, uint64_t *ticket);

/* Honor ALL supplied edges; retries do not reissue completed events. Busy
 * tickets refuse concurrent transitions. Failed waits keep leases pinned.
 * A failed launch must drain, cannot publish output, and is abort-only. */
int open64_reduction_advance(OPEN64_REDUCTION_RUNTIME *, uint64_t ticket,
    uint32_t event);

/* Abort before launch, or after drain before consumer starts/after it joins.
 * Close only released/aborted tickets. Never free memory inside this layer. */
int open64_reduction_abort(OPEN64_REDUCTION_RUNTIME *, uint64_t ticket);
int open64_reduction_close(OPEN64_REDUCTION_RUNTIME *, uint64_t ticket);

#ifdef __cplusplus
}
#endif
#endif
