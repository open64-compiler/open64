/* Copyright (C) 2026 Open64 Project.
 * Concrete target operations for WHIRL-produced P8 schedules. No scheduling
 * policy belongs here. Requires sm_90+, exact unswizzled 2-D tensor maps.
 * Design: doc/AI-COMPILER-OPTIMIZATION-AIO10-FETCH-PIPELINE.md.
 */
#ifndef OPEN64_CUDA_PIPELINE_CUH
#define OPEN64_CUDA_PIPELINE_CUH

#include <cuda_runtime.h>

#if defined(__CUDA_ARCH__) && __CUDA_ARCH__ < 900
#error "Open64 TMA pipeline requires sm_90 or newer"
#endif

/* Initialize one aligned shared ready barrier; caller elects a single producer. */
__device__ __forceinline__ void __open64_cuda_barrier_init(void *bar) {
  unsigned address = static_cast<unsigned>(__cvta_generic_to_shared(bar));
  asm volatile("mbarrier.init.shared::cta.b64 [%0], 1;" :: "r"(address) : "memory");
}

/* Publish generic-proxy barrier initialization before the caller's CTA sync. */
__device__ __forceinline__ void __open64_cuda_proxy_publish() {
  asm volatile("fence.proxy.async.shared::cta;" ::: "memory");
}

/* Arm a free/released slot for exact payload bytes before either operand issue. */
__device__ __forceinline__ void __open64_cuda_tma_acquire(void *bar, int bytes) {
  unsigned address = static_cast<unsigned>(__cvta_generic_to_shared(bar));
  asm volatile("mbarrier.arrive.expect_tx.release.cta.shared::cta.b64 _, [%0], %1;"
               :: "r"(address), "r"(bytes) : "memory");
}

/* Issue one exact 2-D transfer into a caller-acquired slot; no completion wait. */
__device__ __forceinline__ void __open64_cuda_tma_issue(
    void *destination, void *tensor_map, int x, int y, void *bar) {
  unsigned dst = static_cast<unsigned>(__cvta_generic_to_shared(destination));
  unsigned ready = static_cast<unsigned>(__cvta_generic_to_shared(bar));
  asm volatile("cp.async.bulk.tensor.2d.shared::cluster.global.tile."
               "mbarrier::complete_tx::bytes [%0], [%1, {%2, %3}], [%4];"
               :: "r"(dst), "l"(tensor_map), "r"(x), "r"(y), "r"(ready) : "memory");
}

/* Observe the selected generation with acquire semantics before shared reads. */
__device__ __forceinline__ void __open64_cuda_tma_wait(void *bar, int phase) {
  unsigned address = static_cast<unsigned>(__cvta_generic_to_shared(bar));
  unsigned complete;
  do {
    asm volatile("{ .reg .pred done; "
                 "mbarrier.try_wait.parity.acquire.cta.shared::cta.b64 "
                 "done, [%1], %2; selp.b32 %0, 1, 0, done; }"
                 : "=r"(complete) : "r"(address), "r"(phase) : "memory");
  } while (!complete);
}

/* Retire a fully drained barrier; the caller proves every consumer released. */
__device__ __forceinline__ void __open64_cuda_barrier_invalidate(void *bar) {
  unsigned address = static_cast<unsigned>(__cvta_generic_to_shared(bar));
  asm volatile("mbarrier.inval.shared::cta.b64 [%0];" :: "r"(address) : "memory");
}

#endif
