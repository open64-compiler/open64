/* Copyright (C) 2026 Open64 Project */
/* WOPT-owned H2 reduction projection, reusing its existing source/range/event
 * admission. No other family's mathematics, native mutation, search, lowering
 * or runtime authority; owned output requires fresh native recapture.
 * Design: doc/AI-COMPILER-H2-OWNED-MOVEMENT-FACTS-v1.0.md. */
#include "opt_dsl_movement_internal.h"

/* Source order, strict arithmetic and range/event authority are rechecked by
 * the existing direct owner before detached data is copied. */
BOOL WOPT_DSL_capture_reduction_movement(WN *scope,WN *assignment,
    const WOPT_DSL_REDUCTION_DIRECT_REQUEST *r,DSL_MOVEMENT_FACTS &out,std::string &reason)
{
    out=DSL_MOVEMENT_FACTS();
    if(!r || Opt_Level<3) {reason="movement_reduction_scope_or_level";return FALSE;}
    WOPT_DSL_REDUCTION_DIRECT_BINDING b;
    if(!WOPT_DSL_bind_reduction_direct(scope,assignment,r,&b,NULL,NULL) || b.status!=WOPT_DSL_REDUCE_DIRECT_BOUND) {reason="movement_reduction_unbound";return FALSE;}
    try {
        DSL_MOVEMENT_CAPTURE c;WOPT_DSL_movement_binding(scope,b.source.node_id,"Reduction",c);
        c.work.owner="row-thread/one-ordered-accumulator";c.work.domain="row,ascending-feature";
        c.work.instances=b.source.rows;c.work.width=b.source.width;c.work.active_last=c.work.width;
        c.work.axes.push_back("row");c.work.extents.push_back(c.work.instances);c.work.axes.push_back("feature");c.work.extents.push_back(c.work.width);
        c.work.tail_policy="inactive-rows-do-not-access";c.work.numerical="ascending add.rn.f32; no tree/reassociation";
        std::ostringstream proof;proof<<r->runtime_contract_id<<';'<<r->context_id<<';'<<r->instruction<<';'<<r->fp_options<<';'<<r->row_mapping<<';'<<r->launched_threads;
        for(unsigned i=0;i<2;++i) {
            const WOPT_DSL_REDUCTION_BUFFER_BINDING &range=i?r->output:r->input;
            std::vector<uint64_t> shape;shape.push_back(c.work.instances);if(!i) shape.push_back(c.work.width);
            DSL_MOVEMENT_STORAGE s=DSL_movement_dense_storage(i?"Y":"X",WOPT_DSL_movement_number(range.allocation_id),i?"invocation-output":"invocation-input",i?"result":"operand",shape,4,true);
            s.offset=range.offset_bytes;s.capacity=range.allocation_bytes;s.alignment=range.base_alignment;s.first_use=i?"row-finalize":"input-ready";s.last_use=i?"result-visible":"input-release";
            WOPT_DSL_movement_access(c,s,i?0:s.bytes,i?s.bytes:0,1,"source ordered row fold");
            proof<<';'<<range.allocation_id<<';'<<range.allocation_bytes<<';'<<range.offset_bytes<<';'<<range.base_alignment<<';'<<range.lease<<';'<<range.memory_space;
        }
        // Logical per-row fold state is not an allocated hardware register or
        // spill-free claim. R2 owns its ordered update and final output store.
        DSL_MOVEMENT_STORAGE accumulator=DSL_movement_dense_storage("accumulator","R2-logical-row-state","ascending-fold",
            "logical-per-row-accumulator",std::vector<uint64_t>(1,1),4,true);
        accumulator.layout="logical-scalar; hardware-placement-unproved";accumulator.first_use="row-initialize";accumulator.last_use="row-finalize";
        c.storages.push_back(accumulator);
        c.ledger.useful_reads=c.ledger.source_reads=DSL_MOVEMENT_BYTES(b.input_bytes);
        c.ledger.useful_writes=c.ledger.source_writes=DSL_MOVEMENT_BYTES(b.output_bytes);
        c.ledger.partial=DSL_MOVEMENT_BYTES(0); // Direct source has no global partial array.
        WOPT_DSL_movement_events(c,r->before,WOPT_DSL_REDUCE_EVENT_COUNT,"R2");
        for(unsigned i=0;i<WOPT_DSL_REDUCE_EVENT_COUNT;++i) proof<<';'<<r->before[i];
        c.binding.proof_key=proof.str();c.binding.controls=c.work.numerical;
        return DSL_MOVEMENT_FACTS::Create(c,out,reason);
    } catch(const std::exception &e) {reason=e.what();return FALSE;}
}
