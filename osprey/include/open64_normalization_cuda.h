/* Copyright (C) 2026 Open64 Project */

/* Optional LN4 compiled-entry/CUDA boundary. No compiler-owned types or
 * reduction dependency; existing guard/runtime ABI and source v1 unchanged.
 * Design: doc/AI-COMPILER-LAYERNORM-COMPILED-ENTRY-v0.1.md. */
#ifndef OPEN64_NORMALIZATION_CUDA_H
#define OPEN64_NORMALIZATION_CUDA_H
#include "open64_normalization_runtime.h"
#ifdef __cplusplus
extern "C" {
#endif

typedef struct {
  uint64_t version, size;
  OPEN64_NORMALIZATION_GUARD guard;
  uint64_t plan_seal, tree_seal, registers_upper, compiled_registers;
  uint64_t static_shared, resident_floor, cubin_bytes;
  unsigned char cubin_sha256[32];
  char kernel[80];
} OPEN64_NORMALIZATION_COMPILED_ENTRY;

typedef struct OPEN64_NORMALIZATION_CUDA OPEN64_NORMALIZATION_CUDA;

/* Caller-owned trusted services; no callback may throw. sha256 hashes the
 * owned immutable byte span, returns zero on success, and fills all 32 bytes.
 * ready/publish synchronously join input/consumer work, never issue the kernel.
 * Hooks and their context outlive every adapter/ticket using them. */
typedef struct {
  void *context;
  int (*sha256)(void *, const void *, size_t, unsigned char[32]);
  int (*ready)(void *, const OPEN64_NORMALIZATION_INVOCATION *);
  int (*publish)(void *, const OPEN64_NORMALIZATION_INVOCATION *);
} OPEN64_NORMALIZATION_CUDA_HOOKS;

/* Open in an initialized, current CUDA >=12.8 context with an explicit stream.
 * cuda_context/stream are opaque CUcontext/CUstream pointers; never owned here.
 * Copy/authenticate at most 64 MiB of cubin bytes before loading. Caller owns
 * manifest authenticity and context/stream lifetime. Normally refusal leaves
 * output untouched. If rollback unload fails, output receives a cleanup-only
 * handle with OPEN64_NORM_BACKEND; only destroy is then allowed. */
int open64_normalization_cuda_open(const OPEN64_NORMALIZATION_COMPILED_ENTRY *,
  const void *, size_t, void *cuda_context, void *cuda_stream, uint32_t device,
  uint64_t context_id, uint32_t flags, const OPEN64_NORMALIZATION_CUDA_HOOKS *,
  FILE *trace, OPEN64_NORMALIZATION_CUDA **output);
/* Refuse live tickets; unload in the original current context. Failure keeps
 * the object valid for retry. Externally synchronize object destruction. */
int open64_normalization_cuda_destroy(OPEN64_NORMALIZATION_CUDA *);
/* Keep module alive through the existing registry's full lease lifecycle.
 * Failure preserves ticket; caller must retain runtime until wrapper close. */
int open64_normalization_cuda_admit(OPEN64_NORMALIZATION_CUDA *,
  OPEN64_NORMALIZATION_RUNTIME *, const uint64_t roots[4], uint64_t *ticket);
/* Erase module pin only after base runtime close succeeds. Do not directly
 * close wrapper-owned tickets through the base API. Other events/abort reuse
 * the existing runtime API and its failure-safe drain/order rules. */
int open64_normalization_cuda_close(OPEN64_NORMALIZATION_CUDA *,
  OPEN64_NORMALIZATION_RUNTIME *, uint64_t ticket);
#ifdef __cplusplus
}
#endif
#endif
