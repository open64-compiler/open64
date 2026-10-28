/* Host-only declarations for executing actual WHIRL-derived MMA3 kernels.
 * No CUDA runtime, device timing or native MMA is simulated.
 * Design: doc/AI-COMPILER-MMA3-TYPED-SIMT-BASELINE.md. */
#ifndef OPEN64_TYPED_SIMT_CUDA_STUB_H
#define OPEN64_TYPED_SIMT_CUDA_STUB_H
#define __global__
#define __device__
struct dim3 { unsigned x, y, z; };
extern dim3 threadIdx;
extern dim3 blockIdx;
extern "C" float __uint_as_float(unsigned value);
#endif
