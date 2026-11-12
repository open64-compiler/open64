#include <workload.h>

// Illustrative shared-memory tiling; not a production Tensor Core GEMM.
__global__ void tiled(WorkloadArgs a) {
    __shared__ float x[16][16], y[16][16];
    int tx = threadIdx.x, ty = threadIdx.y;
    int64_t i = int64_t(blockIdx.y)*16+ty, j = int64_t(blockIdx.x)*16+tx;
    float sum = 0;
    for (int64_t p = 0; p < a.k; p += 16) {
        int64_t px = p+tx, py = p+ty;
        x[ty][tx] = i<a.m && px<a.k ? __bfloat162float(
            a.a[a.trans_a ? px*a.lda+i : i*a.lda+px]) : 0;
        y[ty][tx] = py<a.k && j<a.n ? __bfloat162float(
            a.b[a.trans_b ? j*a.ldb+py : py*a.ldb+j]) : 0;
        __syncthreads();
        #pragma unroll
        for (int t=0; t<16; ++t) sum = fmaf(x[ty][t], y[t][tx], sum);
        __syncthreads();
    }
    if (i<a.m && j<a.n) a.d[i*a.ldd+j] = __float2bfloat16_rn(
        a.alpha*sum + a.beta*__bfloat162float(a.c[i*a.ldc+j]));
}
extern "C" size_t workspace_bytes(const WorkloadArgs*) { return 0; }
extern "C" cudaError_t run(const WorkloadArgs* a, void*, cudaStream_t s) {
    tiled<<<dim3((a->n+15)/16, (a->m+15)/16), dim3(16,16), 0, s>>>(*a);
    return cudaGetLastError();
}
