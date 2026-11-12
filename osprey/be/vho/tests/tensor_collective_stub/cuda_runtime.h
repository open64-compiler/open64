/* Host-only CUDA collective simulator for WHIRL-derived kernel validation.
 * It models CTA publication and full-warp register exchange, not GPU timing.
 * One CTA runs at a time so static shared arrays have CTA-local lifetime. */
#ifndef OPEN64_TENSOR_COLLECTIVE_STUB_H
#define OPEN64_TENSOR_COLLECTIVE_STUB_H
#include <cmath>
#include <cstring>
#include <thread>
#include <mutex>
#include <condition_variable>
#include <chrono>
#include <vector>
#include <cstdio>
#include <cstdlib>
#define __global__
#define __shared__ static
#define __align__(n) __attribute__((aligned(n)))
struct dim3 { unsigned x=0,y=0,z=0; };
static thread_local dim3 threadIdx,blockIdx;
static float __uint_as_float(unsigned x) { float f; memcpy(&f,&x,4); return f; }
/* Bounded reusable barrier diagnoses divergent collective participation. */
class OPEN64_HOST_BARRIER {
    std::mutex _lock;
    std::condition_variable _changed;
    unsigned _waiting=0,_epoch=0,_participants;
public:
    explicit OPEN64_HOST_BARRIER(unsigned count):_participants(count) {}
    void Reset(unsigned count) {std::lock_guard<std::mutex> lock(_lock);if(_waiting) std::abort();_participants=count;}
    void Wait() {
        std::unique_lock<std::mutex> lock(_lock);unsigned epoch=_epoch;
        if(++_waiting==_participants) {_waiting=0;++_epoch;_changed.notify_all();return;}
        if(!_changed.wait_for(lock,std::chrono::seconds(45),[&]{return _epoch!=epoch;})) {
            fprintf(stderr,"divergent host collective\n");std::abort();
        }
    }
};
static OPEN64_HOST_BARRIER open64_cta(128);
static OPEN64_HOST_BARRIER open64_warp0(32),open64_warp1(32),open64_warp2(32),open64_warp3(32);
static OPEN64_HOST_BARRIER *open64_warps[]={&open64_warp0,&open64_warp1,&open64_warp2,&open64_warp3};
static float open64_exchange[128];
/* The second barrier prevents a fast lane from overwriting a pending read. */
static float __shfl_down_sync(unsigned mask,float value,unsigned delta,int width) {
    if(mask!=0xffffffffU || width!=32 || delta>=32) std::abort();
    unsigned tid=threadIdx.x,lane=tid%32;
    open64_exchange[tid]=value;open64_warps[tid/32]->Wait();
    float result=open64_exchange[tid+(lane+delta<32?delta:0)];
    open64_warps[tid/32]->Wait();return result;
}
static void __syncthreads() {open64_cta.Wait();}
static void __syncwarp(unsigned mask) {if(mask!=0xffffffffU) std::abort();open64_warps[threadIdx.x/32]->Wait();}
/* Persistent workers execute all blocks; the final barrier isolates CTAs. */
template<class FUNCTION> static void open64_host_launch(unsigned grid,unsigned block,FUNCTION function,bool reverse=false) {
    if(block!=32 && block!=64 && block!=128) std::abort();open64_cta.Reset(block);std::vector<std::thread> workers;
    for(unsigned worker=0;worker<block;++worker) workers.emplace_back([&,worker] {
        threadIdx.x=reverse?block-1-worker:worker;
        for(unsigned i=0;i<grid;++i) {
            blockIdx.x=reverse?grid-1-i:i;function();open64_cta.Wait();
        }
    });
    for(auto &worker:workers) worker.join();
}
#endif
