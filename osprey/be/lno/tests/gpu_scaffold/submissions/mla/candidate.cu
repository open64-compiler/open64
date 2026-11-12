#include <workload.h>
#include <math.h>

// Compute each score once, then reuse it across latent output dimensions.
__global__ void scores(WorkloadArgs a, float* scratch) {
    int t=blockIdx.x*blockDim.x+threadIdx.x, h=blockIdx.y, b=blockIdx.z;
    int capacity=a.max_pages*a.page_size;
    if (t>=a.context_lengths[b]) return;
    int width=a.latent_dim+a.rope_dim;
    int page=a.page_table[b*a.max_pages+t/a.page_size];
    int64_t base=(int64_t(page)*a.page_size+t%a.page_size)*width;
    int64_t query=(int64_t(b)*a.heads+h)*width;
    float sum=0;
    for (int d=0; d<width; ++d)
        sum=fmaf(__bfloat162float(a.q[query+d]),__bfloat162float(a.kv[base+d]),sum);
    scratch[(int64_t(b)*a.heads+h)*capacity+t]=sum*a.scale;
}
__global__ void weights(WorkloadArgs a, float* scratch) {
    int h=blockIdx.x, b=blockIdx.y, tid=threadIdx.x;
    int len=a.context_lengths[b], capacity=a.max_pages*a.page_size;
    float* row=scratch+(int64_t(b)*a.heads+h)*capacity;
    __shared__ float reduce[128];
    float mx=-INFINITY;
    for (int t=tid; t<len; t+=128) mx=fmaxf(mx,row[t]);
    reduce[tid]=mx; __syncthreads();
    for (int offset=64; offset; offset/=2) {
        if (tid<offset) reduce[tid]=fmaxf(reduce[tid],reduce[tid+offset]);
        __syncthreads();
    }
    mx=reduce[0];
    // All threads must read the maximum before the shared buffer is reused.
    __syncthreads();
    float sum=0;
    for (int t=tid; t<len; t+=128) { float w=expf(row[t]-mx); row[t]=w; sum+=w; }
    reduce[tid]=sum; __syncthreads();
    for (int offset=64; offset; offset/=2) {
        if (tid<offset) reduce[tid]+=reduce[tid+offset];
        __syncthreads();
    }
    float den=reduce[0];
    for (int t=tid; t<len; t+=128) row[t]/=den;
}
__global__ void output(WorkloadArgs a, const float* scratch) {
    int d=blockIdx.x*blockDim.x+threadIdx.x, h=blockIdx.y, b=blockIdx.z;
    if (d>=a.latent_dim) return;
    int capacity=a.max_pages*a.page_size, width=a.latent_dim+a.rope_dim;
    const float* row=scratch+(int64_t(b)*a.heads+h)*capacity;
    float sum=0;
    for (int t=0; t<a.context_lengths[b]; ++t) {
        int page=a.page_table[b*a.max_pages+t/a.page_size];
        int64_t base=(int64_t(page)*a.page_size+t%a.page_size)*width;
        sum=fmaf(row[t],__bfloat162float(a.kv[base+d]),sum);
    }
    a.out[(int64_t(b)*a.heads+h)*a.latent_dim+d]=__float2bfloat16_rn(sum);
}
extern "C" size_t workspace_bytes(const WorkloadArgs* a) {
    return size_t(a->batch)*a->heads*a->max_pages*a->page_size*sizeof(float);
}
extern "C" cudaError_t run(const WorkloadArgs* a, void* workspace, cudaStream_t s) {
    auto* w=static_cast<float*>(workspace);
    scores<<<dim3((a->max_pages*a->page_size+127)/128,a->heads,a->batch),128,0,s>>>(*a,w);
    cudaError_t e=cudaGetLastError(); if(e!=cudaSuccess) return e;
    weights<<<dim3(a->heads,a->batch),128,0,s>>>(*a,w);
    e=cudaGetLastError(); if(e!=cudaSuccess) return e;
    output<<<dim3((a->latent_dim+127)/128,a->heads,a->batch),128,0,s>>>(*a,w);
    return cudaGetLastError();
}
