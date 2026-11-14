/* Copyright (C) 2026 Open64 Project */
/* WOPT-owned H2 normalization projection, reusing its existing source/range/event
 * admission. No other family's mathematics, native mutation, search, lowering
 * or runtime authority; owned output requires fresh native recapture.
 * Design: doc/AI-COMPILER-H2-OWNED-MOVEMENT-FACTS-v1.0.md. */
#include "opt_dsl_movement_internal.h"

/* Existing canonical centered/ordered source remains the mathematical owner;
 * staging variants carry their exact request but do not change source counts. */
BOOL WOPT_DSL_capture_normalization_movement(WN *scope,WN *assignment,
    const WOPT_DSL_NORMALIZATION_PLAN_REQUEST *r,DSL_MOVEMENT_FACTS &out,std::string &reason)
{
    out=DSL_MOVEMENT_FACTS();
    if(!r || Opt_Level<3) {reason="movement_normalization_scope_or_level";return FALSE;}
    WOPT_DSL_NORMALIZATION_PLAN_BINDING b;
    if(!WOPT_DSL_bind_normalization_plan(scope,assignment,r,&b,NULL,NULL) || b.status!=WOPT_DSL_NORM_PLAN_BOUND) {reason="movement_normalization_unbound";return FALSE;}
    try {
        DSL_MOVEMENT_CAPTURE c;WOPT_DSL_movement_binding(scope,b.target.source.node_id,"LayerNorm",c);
        c.work.owner="row-CTA/ordered-lane-zero";c.work.domain="row,feature";
        c.work.instances=b.target.source.rows;c.work.width=b.target.source.width;
        c.work.axes.push_back("row");c.work.extents.push_back(c.work.instances);c.work.axes.push_back("feature");c.work.extents.push_back(c.work.width);
        c.work.active_last=c.work.width;c.work.tail_policy="valid-feature-predicate-before-address";
        c.work.numerical="LN-REF-1 centered ordered F32; no reassociation/reformulation";
        std::ostringstream proof;proof<<r->kind<<';'<<r->layout<<';'<<r->runtime_contract_id<<';'<<r->context_id<<';'
            <<r->ownership<<';'<<r->threads_per_cta<<';'<<r->launched_ctas<<';'<<r->tail_policy<<';'
            <<r->row_stride_bytes<<';'<<r->shared_bytes<<';'<<r->barrier_participants<<';'<<r->cta_barriers<<';'
            <<r->target.route<<';'<<r->target.toolkit<<';'<<r->target.ptx<<';'<<r->target.environment<<';'<<r->target.fp_options;
        uint64_t useful=0,reads=0;
        const char *roles[]={"X","gamma","beta","Y"};
        for(unsigned i=0;i<4;++i) {
            std::vector<uint64_t> shape;if(i==0 || i==3) shape.push_back(c.work.instances);shape.push_back(c.work.width);
            const WOPT_DSL_NORMALIZATION_RANGE &range=r->ranges[i];
            DSL_MOVEMENT_STORAGE s=DSL_movement_dense_storage(roles[i],WOPT_DSL_movement_number(range.allocation_id),"invocation-input",roles[i],shape,4,true);
            s.offset=range.offset_bytes;s.capacity=range.allocation_bytes;s.alignment=range.base_alignment;s.first_use="input-ready";s.last_use="input-release";
            if(i==3) {s.version="invocation-output";s.first_use="epilogue";s.last_use="result-visible";}
            uint64_t count=i==0?WOPT_DSL_movement_product(s.bytes,3):i==3?0:WOPT_DSL_movement_product(s.bytes,c.work.instances);
            WOPT_DSL_movement_access(c,s,count,i==3?s.bytes:0,i==0?3:i==3?1:c.work.instances,"canonical centered source algorithm");
            if(i<3) {useful=WOPT_DSL_movement_sum(useful,s.bytes);reads=WOPT_DSL_movement_sum(reads,count);}
            proof<<';'<<range.allocation_id<<';'<<range.allocation_bytes<<';'<<range.offset_bytes<<';'<<range.base_alignment<<';'<<range.lease;
        }
        // LN2 owns the in-place per-element C2 overwrite proof. Keep exact
        // physical alias/generations, without inventing a global all-X-read
        // barrier between element-local X reads and d writes.
        if(r->kind!=WOPT_DSL_NORM_C0) {
            for(unsigned generation=0;generation<(r->kind==WOPT_DSL_NORM_C2?2U:1U);++generation) {
                std::vector<uint64_t> shape(1,c.work.width);
                DSL_MOVEMENT_STORAGE scratch=DSL_movement_dense_storage(generation?"row-d":"row-X","LN2-CTA-shared-row",
                    generation?"row/g1":"row/g0",generation?"centered-row":"retained-input-row",shape,4,true);
                scratch.capacity=r->shared_bytes;scratch.alignment=4;
                scratch.first_use=generation?"centered-ready":"row-ready";scratch.last_use=generation?"readers-done":"LN2-per-element-last-X-read";
                c.storages.push_back(scratch);
            }
            for(unsigned statistic=0;statistic<2;++statistic) {
                DSL_MOVEMENT_STORAGE slot=DSL_movement_dense_storage(statistic?"q":"mu","LN2-CTA-shared-row","row-statistic",
                    statistic?"inverse-standard-deviation":"ordered-mean",std::vector<uint64_t>(1,1),4,true);
                slot.offset=r->row_stride_bytes+4*statistic;slot.capacity=r->shared_bytes;slot.alignment=4;
                slot.first_use=statistic?"q-visible":"mu-visible";slot.last_use="readers-done";c.storages.push_back(slot);
            }
        }
        c.ledger.useful_reads=DSL_MOVEMENT_BYTES(useful);c.ledger.useful_writes=DSL_MOVEMENT_BYTES(b.bytes[3]);
        c.ledger.source_reads=DSL_MOVEMENT_BYTES(reads);c.ledger.source_writes=DSL_MOVEMENT_BYTES(b.bytes[3]);
        // A source recipe's reads do not predict the selected pipeline's requests.
        WOPT_DSL_movement_events(c,r->before,WOPT_DSL_NORM_EVENT_COUNT,"LN2");
        for(unsigned i=0;i<WOPT_DSL_NORM_EVENT_COUNT;++i) proof<<';'<<r->before[i];
        c.binding.proof_key=proof.str();c.binding.controls=c.work.numerical;
        return DSL_MOVEMENT_FACTS::Create(c,out,reason);
    } catch(const std::exception &e) {reason=e.what();return FALSE;}
}
