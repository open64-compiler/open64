// Server-owned BF16 attention-core ABI, preserved from Testing scaffold v1.
// This host descriptor does not implement the Q7 quantized/Q3 serving contract.
// Controlling specification: doc/GPU-TEST-SHARED-RESULTS-v1.0.md.
#pragma once
#include <stddef.h>
#include <stdint.h>
#include <cuda_runtime_api.h>
#include <cuda_bf16.h>

// Dense paged decode, one query/sequence; absorbed latent attention only.
// Contiguous BF16 storage, FP32 arithmetic, immutable inputs.
struct WorkloadArgs {
    const __nv_bfloat16* q;  // [batch, heads, latent_dim + rope_dim]
    const __nv_bfloat16* kv; // [physical_pages, page_size, latent_dim + rope_dim]
    const int32_t* page_table;      // [batch, max_pages], device memory
    const int32_t* context_lengths; // [batch], device memory; positive
    __nv_bfloat16* out;             // [batch, heads, latent_dim]
    int32_t batch, heads, latent_dim, rope_dim, page_size, max_pages;
    float scale;
};
// Query scratch size from the immutable host descriptor without device work.
extern "C" size_t workspace_bytes(const WorkloadArgs*);
// Enqueue on the caller's stream using owned, valid buffers and scratch.
// Do not allocate or synchronize; report enqueue failure through cudaError_t.
extern "C" cudaError_t run(const WorkloadArgs*, void*, cudaStream_t);
