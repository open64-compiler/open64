#include <workload.h>

__global__ void naive(WorkloadArgs a) {
    int64_t j = int64_t(blockIdx.x) * blockDim.x + threadIdx.x;
    int64_t i = int64_t(blockIdx.y) * blockDim.y + threadIdx.y;
    if (i >= a.m || j >= a.n) return;
    float sum = 0;
    for (int64_t p = 0; p < a.k; ++p) {
        float x = __bfloat162float(a.a[a.trans_a ? p*a.lda+i : i*a.lda+p]);
        float y = __bfloat162float(a.b[a.trans_b ? j*a.ldb+p : p*a.ldb+j]);
        sum = fmaf(x, y, sum);
    }
    a.d[i*a.ldd+j] = __float2bfloat16_rn(
        a.alpha*sum + a.beta*__bfloat162float(a.c[i*a.ldc+j]));
}
extern "C" size_t workspace_bytes(const WorkloadArgs*) { return 0; }
extern "C" cudaError_t run(const WorkloadArgs* a, void*, cudaStream_t s) {
    naive<<<dim3((a->n+15)/16, (a->m+15)/16), dim3(16,16), 0, s>>>(*a);
    return cudaGetLastError();
}
