/* Copyright (C) 2026 Open64 Project */
/* WOPT H2 shared projection mechanics only: framing, checked counts, copied
 * source binding and existing contract-event projection. Family mathematics
 * and native admission remain in separate owner adapters; no cross-family link.
 * Design: doc/AI-COMPILER-H2-OWNED-MOVEMENT-FACTS-v1.0.md. */
#ifndef opt_dsl_movement_internal_INCLUDED
#define opt_dsl_movement_internal_INCLUDED
#include "opt_dsl_movement.h"
#include "../com/dsl_movement_native.h"
#include "config.h"
#include "config_dsl.h"
#include <sstream>
#include <stdexcept>
namespace {
/* Frame numeric identities without preserving compiler/request pointers. */
template<class T> std::string WOPT_DSL_movement_number(T value) {std::ostringstream out;out<<value;return out.str();}
/* Checked source formula failure never becomes a small traffic count. */
uint64_t WOPT_DSL_movement_product(uint64_t a,uint64_t b) {
    uint64_t out;if(!DSL_movement_mul(a,b,out)) throw std::runtime_error("movement_source_overflow");return out;
}
/* Check source sums independently of any later model's arithmetic. */
uint64_t WOPT_DSL_movement_sum(uint64_t a,uint64_t b) {
    uint64_t out;if(!DSL_movement_add(a,b,out)) throw std::runtime_error("movement_source_overflow");return out;
}
/* Bind the entire active-PU tree/declarations, not just shape/opcode/node ID.
 * WOPT has no supplied activation epoch: all consumers must freshly recapture. */
void WOPT_DSL_movement_binding(WN *scope,UINT32 node,const char *family,DSL_MOVEMENT_CAPTURE &c) {
    c.binding.family=family;c.binding.owner=ST_name(WN_st(scope));
    c.binding.source_key=DSL_movement_native_key(scope,node);
    c.binding.device=DSL_GPU_PLAN_SNAPSHOT::Capture(VHO_DSL_GPU_Context);
    c.binding.device_key=c.binding.device.Cache_key();
}
/* Project a bound source/contract access, retaining actual canonical byte range.
 * Input multiplicity is an opportunity only under the existing immutable lease. */
void WOPT_DSL_movement_access(DSL_MOVEMENT_CAPTURE &c,const DSL_MOVEMENT_STORAGE &s,
    uint64_t reads,uint64_t writes,uint64_t multiplicity,const char *relation) {
    DSL_MOVEMENT_ACCESS a;a.counts_known=true;a.id=s.id+"/access";a.storage=s.id;a.version=s.version;a.owner=c.work.owner;
    a.offset=s.offset;a.extent=s.bytes;a.read_bytes=reads;a.write_bytes=writes;a.multiplicity=multiplicity;
    a.relation=relation;a.reuse_scope="conditional-invocation-lease";a.reuse_proved=!writes && s.alias_proved && s.strides_known;
    c.storages.push_back(s);c.accesses.push_back(a);
    if(!writes) DSL_movement_source_lifetime(c,s.id,s.version,"invocation","existing-immutable-lease-and-event-contract");
}
/* Copy the existing owner's exact DAG; no source position becomes a fence. */
void WOPT_DSL_movement_events(DSL_MOVEMENT_CAPTURE &c,const UINT32 *before,unsigned count,const char *prefix) {
    unsigned first=c.events.size();
    for(unsigned i=0;i<count;++i) {
        DSL_MOVEMENT_EVENT e;e.id=std::string(prefix)+"/"+WOPT_DSL_movement_number(i);e.kind=e.id;
        e.scope="conditional-invocation";e.evidence="existing-bound-event-contract";c.events.push_back(e);
        for(unsigned j=0;j<count;++j) if(before[i]&(UINT32(1)<<j)) c.edges.push_back(DSL_MOVEMENT_EDGE(first+i,first+j));
    }
}
}

#endif
