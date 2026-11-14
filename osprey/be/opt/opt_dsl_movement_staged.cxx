/* Copyright (C) 2026 Open64 Project */
/* WOPT-owned H2 staged projection, reusing its existing source/range/event
 * admission. No other family's mathematics, native mutation, search, lowering
 * or runtime authority; owned output requires fresh native recapture.
 * H3 adds detached direct/staged physical estimates, never a materializer.
 * Designs: doc/AI-COMPILER-H2-OWNED-MOVEMENT-FACTS-v1.0.md and
 * doc/AI-COMPILER-H3-MOVEMENT-COSTING-v1.0.md. */
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

/* R2 remains its own algorithm owner. Bind/capture before the two detached
 * estimates, then recapture before returning; no candidate WHIRL is generated. */
BOOL WOPT_DSL_cost_reduction_movement(WN *scope,WN *assignment,const WOPT_DSL_REDUCTION_STAGED_REQUEST *request,
    DSL_H3_SEARCH &search,WOPT_DSL_REDUCTION_MOVEMENT_COST &out,std::string &reason,FILE *trace)
{
    out=WOPT_DSL_REDUCTION_MOVEMENT_COST();DSL_MOVEMENT_FACTS facts;
    if(!WOPT_DSL_capture_staged_movement(scope,assignment,request,facts,reason)) return FALSE;
    WOPT_DSL_REDUCTION_STAGED_BINDING binding;
    if(!WOPT_DSL_bind_reduction_staged(scope,assignment,request,&binding,NULL,NULL) || binding.status!=WOPT_DSL_REDUCE_STAGED_BOUND) return FALSE;
    try {
        WOPT_DSL_REDUCTION_MOVEMENT_COST result;
        for(unsigned i=0;i<2;++i) {
            DSL_H3_PLAN plan;plan.id=i?"Reduction/staged":"Reduction/direct";plan.capability="R2-conditional-analysis-only";
            const WOPT_DSL_REDUCTION_BUFFER_BINDING &x=request->base.input,&y=request->base.output;
            plan.live_bytes=WOPT_DSL_movement_sum(x.allocation_bytes,x.allocation_id==y.allocation_id?0:y.allocation_bytes);
            DSL_H3_STAGE s;s.id=plan.id;s.ctas=request->staging.cta_count;s.threads=request->staging.rows_per_cta;
            // Explicit conservative scalar reservation is a recipe assumption,
            // not actual register allocation or a spill-free assertion.
            s.registers=16;s.steps=i?binding.chunks:binding.base.source.width;
            s.shared_allocation=i?binding.reserved_bytes:0;
            s.read_requests=WOPT_DSL_movement_product(WOPT_DSL_movement_product(request->staging.rows_per_cta,binding.base.source.width),4);
            s.write_through_requests=WOPT_DSL_movement_product(request->staging.rows_per_cta,4);
            s.scalar_ops=WOPT_DSL_movement_product(request->staging.rows_per_cta,binding.base.source.width);
            s.issue_ops=WOPT_DSL_movement_sum(s.scalar_ops,s.read_requests/4);s.add_chain=binding.base.source.width;
            s.shared_requests=i?WOPT_DSL_movement_product(s.read_requests,2):0;
            // Each input is read once. Staging is not a new source reuse proof.
            plan.stages.push_back(s);DSL_H3_ESTIMATE &estimate=i?result.staged:result.direct;
            search.Evaluate(facts,plan,estimate,reason);VHO_h3_print(trace,facts,plan,estimate);
        }
        if(result.direct.complete && result.staged.complete) result.nominal=result.staged.rank_ns<result.direct.rank_ns?2:1;
        DSL_MOVEMENT_FACTS fresh;
        if(!WOPT_DSL_capture_staged_movement(scope,assignment,request,fresh,reason) || !facts.Same_content(fresh)) return FALSE;
        out=result;if(trace) fprintf(trace,"AI_H3_SELECT 1 family=Reduction nominal=%u executable=0 fallback=existing_direct_contract\n",result.nominal);
        return TRUE;
    } catch(const std::exception &error) {reason=error.what();return FALSE;}
}
