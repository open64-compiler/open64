// Host-only operation model. Copies complete only at wait, never at issue.
#ifndef OPEN64_CUDA_PIPELINE_STUB_CUH
#define OPEN64_CUDA_PIPELINE_STUB_CUH
#include <cuda_runtime.h>
void __open64_cuda_barrier_init(void *);
void __open64_cuda_proxy_publish();
void __open64_cuda_tma_acquire(void *, int);
void __open64_cuda_tma_issue(void *, void *, int, int, void *);
void __open64_cuda_tma_wait(void *, int);
void __open64_cuda_barrier_invalidate(void *);
#endif
