/* Copyright (C) 2026 Open64 Project */

/* LN-REF-1 guard export and independent runtime enforcement boundary.
 * Design: doc/AI-COMPILER-LAYERNORM-RUNTIME-GUARDS-v0.1.md.
 * Process-local, host-native ABI; neither mapped WHIRL nor kernel evidence. */
#ifndef OPEN64_NORMALIZATION_RUNTIME_H
#define OPEN64_NORMALIZATION_RUNTIME_H
#include <stdio.h>
#include "open64_target_buffer_service.h"
#ifdef __cplusplus
extern "C" {
#endif

enum {
  OPEN64_NORM_OK=0, OPEN64_NORM_INVALID=1, OPEN64_NORM_CONFLICT=2,
  OPEN64_NORM_ORDER=3, OPEN64_NORM_BUSY=4, OPEN64_NORM_BACKEND=5,
  OPEN64_NORM_FAILED=6, OPEN64_NORM_CAPACITY=7,
  OPEN64_NORM_HOST_TEST=1,
  OPEN64_NORM_READY=0, OPEN64_NORM_LAUNCH=1, OPEN64_NORM_DRAIN=11,
  OPEN64_NORM_PUBLISH=12, OPEN64_NORM_INPUT_RELEASE=13,
  OPEN64_NORM_OUTPUT_RELEASE=14, OPEN64_NORM_EVENT_COUNT=15
};

/* Canonical backing role is a compile-time name, NOT a runtime handle.
 * Roles map bijectively to runtime roots at admission; input roles may alias. */
typedef struct {
  uint64_t role, capacity, offset, alignment, lease, bytes;
} OPEN64_NORMALIZATION_RANGE;

/* All fields use uint64_t, including flags and masks: no padding, pointers,
 * string storage, or compiler-owned types. Exact equality includes provenance.
 * This v1 GUARD accepts only explicit SOURCE v2; it does not upgrade source v1.
 * Raw bytes are a same-ABI review fixture, not a portable file/network format. */
typedef struct {
  uint64_t abi_version, size, source_version, source_flags, pu, node;
  uint64_t operands[3], result, operand_types[3], result_type;
  uint64_t rows, width, epsilon_bits, source_line;
  uint64_t architecture, route, toolkit, ptx, environment, fp_options;
  uint64_t kind, layout, contract, context, ownership, threads, ctas, tail;
  uint64_t stride, shared, reserved_shared, shared_limit, shared_quantum;
  uint64_t participants, barriers, banks, bank_word, warp, bank_conflict;
  uint64_t obligations;
  OPEN64_NORMALIZATION_RANGE ranges[4];
  uint64_t before[OPEN64_NORM_EVENT_COUNT];
} OPEN64_NORMALIZATION_GUARD;

typedef struct OPEN64_NORMALIZATION_RUNTIME OPEN64_NORMALIZATION_RUNTIME;

/* Trusted synchronous provider verifies WHOLE allocation extent and resolves
 * aliases to one nonzero backing key. Uses TargetBufferService.context.
 * Registration/free/rebind/destruction require external synchronization.
 * All accesses must cooperate with this registry; out-of-band writes forbidden. */
typedef int (*OPEN64_NORMALIZATION_ROOT_RESOLVER)
  (void *, const OPEN64_TARGET_BUFFER_VIEW *, uint64_t *);

typedef struct {
  OPEN64_NORMALIZATION_GUARD guard;
  OPEN64_TARGET_BUFFER_VIEW views[4];
  uint64_t ticket;
} OPEN64_NORMALIZATION_INVOCATION;

/* Trusted adapter manifest asserts exact selected-kernel semantics, including
 * internal row generations, predicates, uniform barriers and register binding.
 * This assertion is NOT proof of executable WHIRL, instructions or hardware.
 * READY waits inputs; LAUNCH issues once; DRAIN joins ALL CTAs even after an
 * ambiguous failed launch; PUBLISH waits consumer completion. Failures pin
 * leases; callbacks cannot throw and are called unlocked. Objects outlive calls.
 * Internal nodes 2..10 cannot be advanced by this host API or called observed.
 * HOST_TEST adapters cannot enter a production context (flags=0).
 * Limits are actual provider launch/resource admission limits, not new compiler
 * architecture constants. max_shared includes kernel static+dynamic admission;
 * max_threads also includes the compiled entry's register/resource constraints. */
typedef struct {
  OPEN64_NORMALIZATION_GUARD manifest;
  uint64_t max_ctas, max_threads, max_shared;
  uint32_t flags;
  void *context;
  int (*perform)(void *, uint32_t, const OPEN64_NORMALIZATION_INVOCATION *);
} OPEN64_NORMALIZATION_ENTRY;

/* Create a registry for one trusted provider/context; invalid configuration
 * returns NULL without taking provider ownership. NULL trace disables logs. */
OPEN64_NORMALIZATION_RUNTIME *open64_normalization_runtime_create
  (const OPEN64_TARGET_BUFFER_SERVICE *, OPEN64_NORMALIZATION_ROOT_RESOLVER,
   uint32_t architecture, uint64_t context, uint32_t flags, FILE *trace);
/* Externally synchronize destruction; live roots/tickets refuse deletion. */
int open64_normalization_runtime_destroy(OPEN64_NORMALIZATION_RUNTIME *);
/* Register a validated WHOLE root once. Failure preserves the output handle;
 * handles are minted by this registry, never compiler allocation roles. */
int open64_normalization_register(OPEN64_NORMALIZATION_RUNTIME *,
  const OPEN64_TARGET_BUFFER_VIEW *, uint64_t *allocation);
/* Retire only unleased live roots before external free/rebind; no memory free. */
int open64_normalization_retire(OPEN64_NORMALIZATION_RUNTIME *, uint64_t allocation);

/* Snapshot exact guard/entry; atomically acquire three immutable-input and exclusive-Y
 * leases over canonical subranges. Fixed capacity 64 roots/tickets. Failures
 * preserve the caller's ticket and never dispatch. No implicit fallback:
 * caller may use a separately admitted C0 or return a diagnostic.
 * Sizes/layout are guard-bound contiguous F32 [R,N], [N], [N], [R,N]; provider
 * must pass actual tensors, not untrusted metadata describing different data. */
int open64_normalization_admit(OPEN64_NORMALIZATION_RUNTIME *,
  const OPEN64_NORMALIZATION_GUARD *, const OPEN64_NORMALIZATION_ENTRY *,
  const uint64_t allocations[4], uint64_t *ticket);
/* Gate callback begin on predecessor completion; retries are idempotent.
 * Internal nodes 2..10 are invalid here. Failed work retains range leases. */
int open64_normalization_advance(OPEN64_NORMALIZATION_RUNTIME *, uint64_t ticket,
  uint32_t event);
/* Cancel only before launch or after required device/consumer drains; busy
 * or undrained tickets refuse cancellation without dropping leases. */
int open64_normalization_abort(OPEN64_NORMALIZATION_RUNTIME *, uint64_t ticket);
/* Forget only a released/aborted ticket; stale handles never become valid. */
int open64_normalization_close(OPEN64_NORMALIZATION_RUNTIME *, uint64_t ticket);
#ifdef __cplusplus
}
#endif
#endif
