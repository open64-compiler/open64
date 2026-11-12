// Server-owned dense BF16 GEMM ABI, preserved from Testing scaffold v1.
// Source/corpus validation is separate from this minimal compiler entry point.
// Controlling specification: doc/GPU-TEST-SHARED-RESULTS-v1.0.md.
#pragma once
#include <stddef.h>
#include <stdint.h>
#include <cuda_runtime_api.h>
#include <cuda_bf16.h>

// Host descriptor; all tensor pointers address device memory.
// Row-major BF16 storage, FP32 accumulation, no overlapping tensors.
struct WorkloadArgs {
    const __nv_bfloat16 *a, *b, *c;
    __nv_bfloat16* d;
    int64_t m, n, k;
    int64_t lda, ldb, ldc, ldd; // Element strides, not byte strides.
    int32_t trans_a, trans_b;   // 0 = identity, 1 = transpose.
    float alpha, beta;
};
// Query scratch size from the immutable host descriptor without device work.
extern "C" size_t workspace_bytes(const WorkloadArgs*);
// Enqueue on the caller's stream using owned, valid buffers and scratch.
// Do not allocate or synchronize; report enqueue failure through cudaError_t.
extern "C" cudaError_t run(const WorkloadArgs*, void*, cudaStream_t);
