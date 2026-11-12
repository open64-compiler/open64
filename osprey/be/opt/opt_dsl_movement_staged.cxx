/* Copyright (C) 2026 Open64 Project */
/* WOPT-owned H2 staged projection, reusing its existing source/range/event
 * admission. No other family's mathematics, native mutation, search, lowering
 * or runtime authority; owned output requires fresh native recapture.
 * Design: doc/AI-COMPILER-H2-OWNED-MOVEMENT-FACTS-v1.0.md. */
#include "opt_dsl_movement_internal.h"

/* The staged owner supplies exact chunk/tail/storage and cross-generation
 * order. Source counts remain unchanged; shared copy/read requests are separate. */
BOOL WOPT_DSL_capture_staged_movement(WN *scope,WN *assignment,
    const WOPT_DSL_REDUCTION_STAGED_REQUEST *r,DSL_MOVEMENT_FACTS &out,std::string &reason)
{
    out=DSL_MOVEMENT_FACTS();if(!r || Opt_Level<3) {reason="movement_staged_scope_or_level";return FALSE;}
    WOPT_DSL_REDUCTION_STAGED_BINDING b;
    if(!WOPT_DSL_bind_reduction_staged(scope,assignment,r,&b,NULL,NULL) || b.status!=WOPT_DSL_REDUCE_STAGED_BOUND) {reason="movement_staging_unbound";return FALSE;}
    DSL_MOVEMENT_FACTS base;if(!WOPT_DSL_capture_reduction_movement(scope,assignment,&r->base,base,reason)) return FALSE;
    try {
        DSL_MOVEMENT_CAPTURE c=base.Capture();const WOPT_DSL_REDUCTION_STAGING &s=r->staging;
        c.work.owner="row-thread/disjoint-CTA-slot";c.work.width=b.chunk;c.work.active_last=b.last_count;
        c.work.axes.clear();c.work.extents.clear();c.work.axes.push_back("row");c.work.extents.push_back(c.work.instances);c.work.axes.push_back("chunk");c.work.extents.push_back(b.chunks);c.work.axes.push_back("within_chunk");c.work.extents.push_back(b.chunk);
        c.work.inactive_last=b.chunk-b.last_count;c.work.tail_policy="exact-active-tail-no-padding-load";
        std::vector<uint64_t> shape;shape.push_back(s.rows_per_cta);shape.push_back(b.chunk);
        DSL_MOVEMENT_STORAGE slot=DSL_movement_dense_storage("stage-slot",WOPT_DSL_movement_number(s.scratch_object),"chunk/g0","CTA-shared-slot",shape,4,true);
        slot.offset=s.scratch_offset;slot.capacity=s.allocation_bytes;slot.bytes=b.scratch_end-s.scratch_offset;
        slot.strides[0]=s.row_stride;slot.first_use="slot-acquire(g)";slot.last_use="slot-release(g)";c.storages.push_back(slot);
        unsigned first=c.events.size();const char *kinds[]={"acquire","copy","raw-ready","consumer","readers-complete"};
        unsigned generations=b.chunks>1?2:1;
        for(unsigned g=0;g<generations;++g) for(unsigned i=0;i<5;++i) {
            DSL_MOVEMENT_EVENT e;e.id="stage/"+WOPT_DSL_movement_number(g)+"/"+WOPT_DSL_movement_number(i);e.kind=kinds[i];
            e.storage=slot.id;e.version=g?"chunk/g1":"chunk/g0";e.scope="same-row-thread";e.evidence="existing-R2-staged-contract";c.events.push_back(e);
            for(unsigned j=0;j<5;++j) if(s.before[i]&(1U<<j)) c.edges.push_back(DSL_MOVEMENT_EDGE(first+g*5+i,first+g*5+j));
        }
        if(generations==2) for(unsigned i=0;i<5;++i) for(unsigned j=0;j<5;++j)
            if(s.next[i]&(1U<<j)) c.edges.push_back(DSL_MOVEMENT_EDGE(first+i,first+5+j));
        DSL_MOVEMENT_LIFETIME life;life.storage=slot.id;life.version="chunk/g0";life.raw_ready=life.decoded_ready=first+2;life.consumer=first+3;life.readers_done=first+4;
        if(generations==2) {life.has_reuse=true;life.next_version="chunk/g1";life.reuse_ready=first+5;}c.lifetimes.push_back(life);
        c.ledger.shared=DSL_MOVEMENT_BYTES(WOPT_DSL_movement_product(b.base.input_bytes,2));
        std::ostringstream proof;proof<<c.binding.proof_key<<";staging;"<<s.chunk_elements<<';'<<s.scratch_object<<';'<<s.scratch_offset<<';'
            <<s.row_stride<<';'<<s.allocation_bytes<<';'<<s.per_row_budget<<';'<<s.cta_budget<<';'<<s.other_shared_bytes<<';'
            <<s.rows_per_cta<<';'<<s.cta_count<<';'<<s.slots<<';'<<s.memory_space<<';'<<s.copy_mode<<';'<<s.visibility<<';'<<s.tail_policy<<';'<<b.chunks<<';'<<b.last_active_rows;
        for(unsigned i=0;i<5;++i) proof<<';'<<s.before[i]<<';'<<s.next[i];c.binding.proof_key=proof.str();
        return DSL_MOVEMENT_FACTS::Create(c,out,reason);
    } catch(const std::exception &e) {reason=e.what();return FALSE;}
}
