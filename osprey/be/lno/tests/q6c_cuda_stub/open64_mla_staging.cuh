/* Test-only concrete instruction model. Copies remain poisoned/undelivered
 * until arrive/wait; actual CUDA source and native PTX are assembled separately.
 * No timing, hardware cache, stream or device-lease behavior is inferred. */
#ifndef OPEN64_Q6C_HOST_STAGING
#define OPEN64_Q6C_HOST_STAGING
#include <map>
#include <algorithm>
struct Q6C_COPY {void *dst,*src;unsigned bytes;};
struct Q6C_BARRIER {unsigned phase=0,expected=0;bool arrived=false;std::vector<Q6C_COPY> copies;};
static std::map<void *,Q6C_BARRIER> q6c_barriers;
static unsigned long long q6c_bulk_bytes=0,q6c_copies=0,q6c_waits=0,q6c_syncs=0;
static void q6c_need(bool b) {if(!b) {fprintf(stderr,"Q6c instruction protocol failure\n");std::abort();}}
static void __open64_q6c_init(void *p) {q6c_need(threadIdx.x==0 && !q6c_barriers.count(p) && !((uintptr_t)p%8));q6c_barriers[p]=Q6C_BARRIER();}
static void __open64_q6c_proxy() {q6c_need(threadIdx.x==0);}
static void __open64_q6c_expect(void *p,unsigned n) {q6c_need(threadIdx.x==0 && q6c_barriers.count(p) && n && !(n%16));auto &b=q6c_barriers[p];q6c_need(!b.arrived);b.expected+=n;}
static void __open64_q6c_bulk(void *d,void *s,unsigned n,void *p) {
    q6c_need(threadIdx.x==0 && q6c_barriers.count(p) && n && !(n%16) && !((uintptr_t)d%16) && !((uintptr_t)s%16));
    auto &b=q6c_barriers[p];q6c_need(!b.arrived && b.expected>=n);b.copies.push_back({d,s,n});memset(d,0xad,n);++q6c_copies;q6c_bulk_bytes+=n;
}
static void __open64_q6c_arrive(void *p) {q6c_need(threadIdx.x==0 && q6c_barriers.count(p));auto &b=q6c_barriers[p];q6c_need(!b.arrived);b.arrived=true;}
static void __open64_q6c_wait(void *p,unsigned phase) {
    q6c_need(threadIdx.x==0 && q6c_barriers.count(p));auto &b=q6c_barriers[p];q6c_need(b.arrived && phase==b.phase);
    for(auto i=b.copies.rbegin();i!=b.copies.rend();++i) {q6c_need(b.expected>=i->bytes);memcpy(i->dst,i->src,i->bytes);b.expected-=i->bytes;}
    q6c_need(!b.expected);b.copies.clear();b.arrived=false;b.phase^=1;++q6c_waits;
}
static void __open64_q6c_cta_sync() {open64_cta.Wait();if(threadIdx.x==0) ++q6c_syncs;}
static void __open64_q6c_invalidate(void *p) {q6c_need(threadIdx.x==0 && q6c_barriers.count(p));auto &b=q6c_barriers[p];q6c_need(!b.arrived && !b.expected && b.copies.empty());q6c_barriers.erase(p);}
#endif
