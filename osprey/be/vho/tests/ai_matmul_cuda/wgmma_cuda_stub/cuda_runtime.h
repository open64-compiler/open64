/* Host-only MMA4-H geometry/protocol shim, never a WGMMA ISA simulator.
 * Design: doc/AI-COMPILER-MMA4-H-HOPPER-LOWERING.md. */
#ifndef OPEN64_WGMMA_CUDA_STUB_H
#define OPEN64_WGMMA_CUDA_STUB_H
#include <cstring>
#define __global__
#define __device__
#define __shared__ static
#define __align__(bytes) __attribute__((aligned(bytes)))
struct dim3 { unsigned x, y, z; };
extern thread_local dim3 threadIdx;
extern dim3 blockIdx, blockDim, gridDim;
/* Join the test's current publication or completed-reader CTA phase. */
void VHO_DSL_wgmma_host_barrier();
/* Record per-thread operand publication before a uniform CTA join. */
void VHO_DSL_wgmma_host_proxy();
/* Apply a completed-atom mathematical model; never emulate ISA rounding. */
void VHO_DSL_wgmma_host_atom(float *, unsigned short *, unsigned short *);
/* Dispatch only the test's CTA barrier; no device operation is executed. */
inline void __syncthreads() { VHO_DSL_wgmma_host_barrier(); }
/* Preserve the source SIMT bitcast without inventing a numeric conversion. */
inline float __uint_as_float(unsigned value) { float result; std::memcpy(&result, &value, sizeof(result)); return result; }
#endif
