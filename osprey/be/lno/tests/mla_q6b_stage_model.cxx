/* Copyright (C) 2026 Open64 Project */
/* Test-only delayed-copy execution model for actual emitted Q6b High C.
 * Per-thread logical scopes validate protocol, not GPU timing or instructions.
 * Never linked into be.so or a production serving adapter. Contract:
 * doc/AI-COMPILER-MLA-Q6B-NATIVE-STAGING-v1.0.md. */
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <cstdint>
#include <map>
#include <vector>
#include <string>
#include <sstream>
namespace {
struct MLA_COPY {void *dst,*src;uint64_t bytes;bool async;};
struct MLA_SLOT {
    unsigned state;void *raw,*decoded;uint64_t raw_bytes,decoded_bytes,expected,issued,tile;unsigned pass;
    std::vector<MLA_COPY> copies;
};
struct MLA_MODEL {
    std::map<void *,MLA_SLOT> slots;
    void *decoded_owner;
    uint64_t acquired,waited,published,consumed,released,ended,copies,delayed_bytes,sync_bytes,overlaps,max_live;
    /* Scope identities are local to one host thread; callbacks may be concurrent. */
    MLA_MODEL():decoded_owner(NULL),acquired(0),waited(0),published(0),consumed(0),released(0),ended(0),copies(0),delayed_bytes(0),sync_bytes(0),overlaps(0),max_live(0) {}
};
thread_local MLA_MODEL model;
/* A protocol violation fails the isolated review process; no partial result
 * is silently accepted by a Python callback or the Q3 publication gate. */
void require(bool ok,const char *why) {if(!ok) {std::fprintf(stderr,"Q6B_MODEL_REFUSAL %s\n",why);std::abort();}}
/* Compare borrowed in-process byte ranges with checked unsigned arithmetic. */
bool within(void *pointer,uint64_t bytes,void *base,uint64_t size) {
    uintptr_t p=reinterpret_cast<uintptr_t>(pointer),b=reinterpret_cast<uintptr_t>(base);
    return p>=b && p-b<=size && bytes<=size-(p-b);
}
/* These scopes must have distinct raw storage and one common decoded tile. */
bool overlap(void *a,uint64_t n,void *b,uint64_t m) {
    uintptr_t x=reinterpret_cast<uintptr_t>(a),y=reinterpret_cast<uintptr_t>(b);
    return x<=y?y-x<n:x-y<m;
}
/* No implicit initialization or foreign scope is accepted. */
MLA_SLOT &slot(void *identity) {require(model.slots.count(identity)==1,"unknown_scope");return model.slots.find(identity)->second;}
}
extern "C" {
/* Initialize one logical raw slot before the work unit's first pass. */
void __open64_mla_stage_init(void *identity,void *raw,uint64_t raw_bytes,void *decoded,uint64_t decoded_bytes) {
    require(identity && raw && decoded && raw_bytes && decoded_bytes && !model.slots.count(identity),"duplicate_or_empty_scope");
    require(!overlap(raw,raw_bytes,decoded,decoded_bytes),"raw_decoded_overlap");
    for(const auto &entry:model.slots) {
        const MLA_SLOT &s=entry.second;
        require(!overlap(raw,raw_bytes,s.raw,s.raw_bytes) && decoded==s.decoded && decoded_bytes==s.decoded_bytes,"scope_storage_join");
    }
    MLA_SLOT s={};s.raw=raw;s.raw_bytes=raw_bytes;s.decoded=decoded;s.decoded_bytes=decoded_bytes;model.slots.emplace(identity,s);
}
/* Acquire preserves increasing tile/pass order within a reused raw slot.
 * Destination poison ensures a missing wait cannot accidentally read old data. */
void __open64_mla_stage_acquire(void *identity,uint64_t tile,unsigned pass,uint64_t expected) {
    MLA_SLOT &s=slot(identity);require((s.state==0 || s.state==6) && expected && expected<=s.raw_bytes && pass<3,"acquire_busy_or_bad_extent");
    if(s.state==6) require((pass==s.pass && tile>s.tile) || (pass==s.pass+1 && tile<2),"slot_generation_order");
    if(model.decoded_owner) ++model.overlaps;
    s.state=1;s.expected=expected;s.issued=0;s.tile=tile;s.pass=pass;s.copies.clear();std::memset(s.raw,0xa7,s.raw_bytes);++model.acquired;
    uint64_t live=0;for(const auto &entry:model.slots) live+=entry.second.state>0 && entry.second.state<6;
    if(live>model.max_live) model.max_live=live;
}
/* Copy has exact byte semantics. Async copies complete only at wait, in reverse
 * issue order; sync fragments complete immediately but still join at wait. */
void __open64_mla_stage_copy(void *identity,void *dst,void *src,uint64_t bytes,unsigned delivery) {
    MLA_SLOT &s=slot(identity);require(s.state==1 && src && bytes && (delivery==1 || delivery==2),"copy_without_acquire");
    require(within(dst,bytes,s.raw,s.raw_bytes) && bytes<=s.expected-s.issued,"copy_out_of_bounds_or_excess_bytes");
    for(const MLA_COPY &c:s.copies) require(!overlap(dst,bytes,c.dst,c.bytes),"overlapping_copy_destinations");
    MLA_COPY c={dst,src,bytes,delivery==2};s.copies.push_back(c);s.issued+=bytes;++model.copies;
    if(c.async) model.delayed_bytes+=bytes;else {std::memcpy(dst,src,bytes);model.sync_bytes+=bytes;}
}
/* Join the whole logical transfer, then grant exclusive decoded preparation.
 * Physical mbarrier bulk-byte accounting belongs to Q6c, not this host model. */
void __open64_mla_stage_wait(void *identity) {
    MLA_SLOT &s=slot(identity);require(s.state==1 && s.issued==s.expected && !model.decoded_owner,"wait_incomplete_or_decoded_busy");
    for(auto i=s.copies.rbegin();i!=s.copies.rend();++i) if(i->async) std::memcpy(i->dst,i->src,i->bytes);
    s.state=3;model.decoded_owner=identity;std::memset(s.decoded,0x7f,s.decoded_bytes);++model.waited;
}
/* Publish is an explicit collective visibility obligation before consumers. */
void __open64_mla_stage_publish(void *identity) {MLA_SLOT &s=slot(identity);require(s.state==3 && model.decoded_owner==identity,"publish_before_wait");s.state=4;++model.published;}
/* Completion joins all logical consumers before decoded storage is reused. */
void __open64_mla_stage_consume(void *identity) {MLA_SLOT &s=slot(identity);require(s.state==4 && model.decoded_owner==identity,"consume_before_publication");s.state=5;model.decoded_owner=NULL;++model.consumed;}
/* Only a completed consumer scope may release raw storage for overwrite. */
void __open64_mla_stage_release(void *identity) {MLA_SLOT &s=slot(identity);require(s.state==5,"release_before_consumers");s.state=6;s.copies.clear();std::memset(s.raw,0xbc,s.raw_bytes);++model.released;}
/* A drained work unit invalidates its scope; pending copies never escape. */
void __open64_mla_stage_end(void *identity) {MLA_SLOT &s=slot(identity);require((s.state==0 || (s.state==6 && s.pass==2)) && model.decoded_owner!=identity,"end_before_final_drain");model.slots.erase(identity);++model.ended;}
/* Review resets counters only between quiescent invocations, outside search. */
void open64_q6b_model_reset() {require(model.slots.empty() && !model.decoded_owner,"reset_before_drain");model=MLA_MODEL();}
/* Export exact executed-event counts; no latency, GPU or performance inference. */
const char *open64_q6b_model_report() {
    static thread_local std::string report;std::ostringstream s;
    s<<"{\"quiescent\":"<<(model.slots.empty() && !model.decoded_owner?"true":"false")
        <<",\"acquired\":"<<model.acquired<<",\"waited\":"<<model.waited<<",\"published\":"<<model.published
        <<",\"consumed\":"<<model.consumed<<",\"released\":"<<model.released<<",\"ended\":"<<model.ended
        <<",\"copies\":"<<model.copies<<",\"delayed_bytes\":"<<model.delayed_bytes<<",\"sync_bytes\":"<<model.sync_bytes
        <<",\"prefetch_while_current_owned\":"<<model.overlaps<<",\"max_live_raw_slots\":"<<model.max_live<<"}";
    report=s.str();return report.c_str();
}
}
