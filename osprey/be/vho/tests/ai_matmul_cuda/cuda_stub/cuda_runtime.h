// Copyright (c) 2026 Open64 contributors.
// Host-only CUDA stand-in for the VHO GEMM source parity test. It models
// CTA barriers, not device execution. See doc/AI-COMPILER-OPTIMIZATION-IMPLEMENTATION-PLAN.md.
#ifndef OPEN64_AI_MATMUL_CUDA_STUB_H
#define OPEN64_AI_MATMUL_CUDA_STUB_H

#define __global__
#define __device__
#define __shared__ static
#define __constant__
#define __align__(bytes) __attribute__((aligned(bytes)))
#define __forceinline__ inline

struct dim3 {
    unsigned x, y, z;
};
extern thread_local dim3 threadIdx;
extern dim3 blockIdx;
void cuda_stub_barrier();
inline void __syncthreads() { cuda_stub_barrier(); }

#endif
