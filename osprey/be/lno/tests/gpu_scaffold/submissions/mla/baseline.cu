#include <workload.h>
#include <math.h>

__device__ float score(WorkloadArgs a, int b, int h, int t) {
    int width = a.latent_dim+a.rope_dim;
    int page = a.page_table[b*a.max_pages+t/a.page_size];
    int64_t base = (int64_t(page)*a.page_size+t%a.page_size)*width;
    int64_t query = (int64_t(b)*a.heads+h)*width;
    float sum = 0;
    for (int d=0; d<width; ++d)
        sum = fmaf(__bfloat162float(a.q[query+d]), __bfloat162float(a.kv[base+d]), sum);
    return sum*a.scale;
}
// One thread per latent output: intentionally simple, redundant reference baseline.
__global__ void naive(WorkloadArgs a) {
    int d = blockIdx.x*blockDim.x+threadIdx.x;
    int h = blockIdx.y, b = blockIdx.z;
    if (d>=a.latent_dim) return;
    int len = a.context_lengths[b], width = a.latent_dim+a.rope_dim;
    float maximum = -INFINITY;
    for (int t=0; t<len; ++t) maximum = fmaxf(maximum, score(a,b,h,t));
    float den=0, num=0;
    for (int t=0; t<len; ++t) {
        float w = expf(score(a,b,h,t)-maximum);
        int page = a.page_table[b*a.max_pages+t/a.page_size];
        int64_t base = (int64_t(page)*a.page_size+t%a.page_size)*width;
        den += w;
        num = fmaf(w, __bfloat162float(a.kv[base+d]), num);
    }
    a.out[(int64_t(b)*a.heads+h)*a.latent_dim+d] = __float2bfloat16_rn(num/den);
}
extern "C" size_t workspace_bytes(const WorkloadArgs*) { return 0; }
extern "C" cudaError_t run(const WorkloadArgs* a, void*, cudaStream_t s) {
    naive<<<dim3((a->latent_dim+63)/64,a->heads,a->batch),64,0,s>>>(*a);
    return cudaGetLastError();
}
