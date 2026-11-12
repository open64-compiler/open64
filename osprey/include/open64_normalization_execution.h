/* Copyright (C) 2026 Open64 Project */

/* LN4 runtime-only host/service consumer over the shared registry, target buffer
 * service and exact CUDA entry. No compiler/WHIRL or reduction implementation.
 * Design: doc/AI-COMPILER-LAYERNORM-EXECUTION-CONSUMER-v0.1.md. */
#ifndef OPEN64_NORMALIZATION_EXECUTION_H
#define OPEN64_NORMALIZATION_EXECUTION_H
#include "open64_normalization_cuda.h"
#ifdef __cplusplus
extern "C" {
#endif

typedef struct OPEN64_NORMALIZATION_EXECUTION OPEN64_NORMALIZATION_EXECUTION;

/* Whole-root request. Host views require allocation=0; device views borrow an
 * existing shared-registry handle. Repeated guard roles must repeat this exact
 * view/handle. Host and device lifetimes extend through successful cleanup. */
typedef struct {
  OPEN64_TARGET_BUFFER_VIEW view;
  uint64_t allocation;
} OPEN64_NORMALIZATION_ROOT_REQUEST;

/* Borrow every object below; keep it alive until successful destroy. The shared
 * registry must describe the same service/device/context. Callbacks cannot throw;
 * ready/publish are idempotent joins. quiesce joins ALL work started by service/
 * hooks after any failure/cancel before release, including ambiguous copy errors.
 * It uses hooks.context; no implicit crypto or target-buffer provider is installed. */
typedef struct {
  OPEN64_NORMALIZATION_RUNTIME *runtime;
  OPEN64_TARGET_BUFFER_SERVICE buffers;
  OPEN64_NORMALIZATION_CUDA_HOOKS hooks;
  int (*quiesce)(void *);
  void *cuda_context, *cuda_stream;
  uint32_t device, flags;
  uint64_t context_id;
  FILE *trace;
} OPEN64_NORMALIZATION_EXECUTION_CONFIG;

/* Open the selected compiled entry without owning the shared registry. Normally
 * refusal preserves output; failed module rollback returns a cleanup-only handle
 * with BACKEND status for destroy retry. No context creation or plan selection. */
int open64_normalization_execution_open(const OPEN64_NORMALIZATION_COMPILED_ENTRY *,
  const void *cubin, size_t bytes, const OPEN64_NORMALIZATION_EXECUTION_CONFIG *,
  OPEN64_NORMALIZATION_EXECUTION **output);
/* Begin one invocation. Preflight refusal preserves idle state. After allocation
 * starts, any failure keeps active state and host loans for explicit cancel.
 * No host input is read until atomic lease admission and READY processing. */
int open64_normalization_execution_begin(OPEN64_NORMALIZATION_EXECUTION *,
  const OPEN64_NORMALIZATION_ROOT_REQUEST roots[4]);
/* Complete remaining ordered events and cleanup. Failed launch is never retried;
 * use cancel. Other failures retain state for retry after quiescence. */
int open64_normalization_execution_resume(OPEN64_NORMALIZATION_EXECUTION *);
/* Synchronous convenience: begin, then resume. Failure retains the same cleanup
 * obligations; it does not silently discard a partially active invocation. */
int open64_normalization_execution_run(OPEN64_NORMALIZATION_EXECUTION *,
  const OPEN64_NORMALIZATION_ROOT_REQUEST roots[4]);
/* Join started work, abort/close and clean owned roots only. Failed drain,
 * consumer join, quiesce, retirement or free preserves retryable state. */
int open64_normalization_execution_cancel(OPEN64_NORMALIZATION_EXECUTION *);
/* Refuse active/busy state; unload adapter only. Caller externally synchronizes
 * object destruction and never destroys the borrowed registry first. */
int open64_normalization_execution_destroy(OPEN64_NORMALIZATION_EXECUTION *);
#ifdef __cplusplus
}
#endif
#endif
