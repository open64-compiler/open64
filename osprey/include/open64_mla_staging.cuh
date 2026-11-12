/* Copyright (C) 2026 Open64 Project */
/* Exact Q6c instruction spellings. All partitioning, participants, byte counts,
 * phases and synchronization placement are explicit in native WHIRL. */
#ifndef OPEN64_MLA_STAGING_CUH
#define OPEN64_MLA_STAGING_CUH
#include <cuda_runtime.h>
#if defined(__CUDA_ARCH__) && __CUDA_ARCH__ < 900
#error "Q6c bulk staging requires sm_90"
#endif
__device__ __forceinline__ void __open64_q6c_init(void *bar) {
    unsigned a=(unsigned)__cvta_generic_to_shared(bar);
    asm volatile("mbarrier.init.shared::cta.b64 [%0], 1;"::"r"(a):"memory");
}
__device__ __forceinline__ void __open64_q6c_proxy() {
    asm volatile("fence.proxy.async.shared::cta;":::"memory");
}
__device__ __forceinline__ void __open64_q6c_expect(void *bar,unsigned bytes) {
    unsigned a=(unsigned)__cvta_generic_to_shared(bar);
    asm volatile("mbarrier.expect_tx.relaxed.cta.shared::cta.b64 [%0], %1;"::"r"(a),"r"(bytes):"memory");
}
__device__ __forceinline__ void __open64_q6c_bulk(void *dst,void *src,unsigned bytes,void *bar) {
    unsigned d=(unsigned)__cvta_generic_to_shared(dst),b=(unsigned)__cvta_generic_to_shared(bar);
    asm volatile("cp.async.bulk.shared::cluster.global.mbarrier::complete_tx::bytes [%0], [%1], %2, [%3];"::"r"(d),"l"(src),"r"(bytes),"r"(b):"memory");
}
__device__ __forceinline__ void __open64_q6c_arrive(void *bar) {
    unsigned a=(unsigned)__cvta_generic_to_shared(bar);
    asm volatile("mbarrier.arrive.release.cta.shared::cta.b64 _, [%0];"::"r"(a):"memory");
}
__device__ __forceinline__ void __open64_q6c_wait(void *bar,unsigned phase) {
    unsigned a=(unsigned)__cvta_generic_to_shared(bar),done;
    do {
        asm volatile("{ .reg .pred p; mbarrier.try_wait.parity.acquire.cta.shared::cta.b64 p, [%1], %2; selp.b32 %0, 1, 0, p; }":"=r"(done):"r"(a),"r"(phase):"memory");
    } while(!done);
}
__device__ __forceinline__ void __open64_q6c_cta_sync() {__syncthreads();}
__device__ __forceinline__ void __open64_q6c_invalidate(void *bar) {
    unsigned a=(unsigned)__cvta_generic_to_shared(bar);
    asm volatile("mbarrier.inval.shared::cta.b64 [%0];"::"r"(a):"memory");
}
#endif
