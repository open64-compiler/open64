/* Copyright (C) 2026 Open64 Project */
/* Full-operation Q5 estimates for the exact strict High materializer. Reuses
 * the existing finite portfolio and checked arithmetic; no native construction.
 * Design: doc/AI-COMPILER-MLA-Q5-SELECTED-PLANS-v1.0.md. */
const char *VHO_q5_catalog() {return "mla-q5-strict-plans-v1-17";}
namespace {
struct Q5_WORK {
    UINT64 reads,writes,scalar,exp,log,div,dependency,launches;
    /* Counts are source-execution envelopes, not instruction measurements. */
    Q5_WORK():reads(0),writes(0),scalar(0),exp(0),log(0),div(0),dependency(0),launches(0) {}
};
/* Match the actual Q1 materialized loops. Broadcast inputs are charged for each
 * output element; matmul/reductions charge ordered contributions. Scatter
 * includes the complete checked update scan. No producer is silently free. */
void Q5_operation(Q5_WORK &w,const VHO_TENSOR_PROGRAM &p,unsigned i) {
    const VHO_TENSOR_OPERATION &o=p.operations[i];if(o.name=="common.reshape") return;
    const VHO_TENSOR_VALUE &out=p.values[o.output];UINT64 n=VHO_tensor_elements(out),terms=1;
    if(o.name=="common.matmul" || o.name=="common.reduce_max" || o.name=="common.reduce_sum") terms=p.values[o.inputs[0]].shape.back();
    UINT64 lanes=mul(n,terms);w.scalar=add(w.scalar,mul(lanes,o.name=="common.matmul"?2:1));
    w.dependency=add(w.dependency,terms);w.launches=add(w.launches,1);
    w.writes=add(w.writes,mul(n,out.dtype=="float32"?4:out.dtype=="int64"?8:1));
    for(unsigned k=0;k<o.inputs.size();++k) {
        const VHO_TENSOR_VALUE &v=p.values[o.inputs[k]];UINT64 reads=lanes;
        if(o.name=="common.fill_like") reads=0;
        if(o.name=="common.scatter" && k==1) reads=mul(n,VHO_tensor_elements(v));
        w.reads=add(w.reads,mul(reads,v.dtype=="float32"?4:v.dtype=="int64"?8:1));
    }
    if(o.name=="common.scatter") w.scalar=add(w.scalar,mul(mul(n,VHO_tensor_elements(p.values[o.inputs[1]])),4));
    if(o.name=="common.round_even") w.scalar=add(w.scalar,mul(n,16));
    if(o.name=="common.pack_u4" || o.name=="common.unpack_u4" || o.name=="common.convert" || o.name=="common.clamp") w.scalar=add(w.scalar,mul(n,4));
    if(o.name=="common.exp") w.exp=add(w.exp,ceilq(n,32));
    if(o.name=="common.log") w.log=add(w.log,ceilq(n,32));
    if(o.name=="common.div") w.div=add(w.div,ceilq(n,32));
}
/* Evaluate the complete source entry, including maintenance, both projections,
 * attention/statistic/copy stages and fixed guards. GPU placement is an explicit
 * future-mapping obligation; host code executes the same scalar algorithm. */
void Q5_evaluate(VHO_MLA_SEARCH_CANDIDATE &c,const VHO_TENSOR_PROGRAM &source,
    const VHO_TENSOR_GPU_PROFILE &gpu,const VHO_MLA_COST_PROFILE &model,const VHO_Q4_PROFILE &profile,
    const VHO_Q4_SNAPSHOT &snapshot,UINT64 log_cycles) {
    c.evaluated=true;VHO_Q5_LAYOUT layout;std::string why;
    if(c.width && !VHO_q5_layout(source,layout,why)) throw std::runtime_error(why);
    Q5_WORK w;
    if(!c.width) for(unsigned i=0;i<source.operations.size();++i) Q5_operation(w,source,i);
    else {
        std::set<unsigned> removed(layout.removed.begin(),layout.removed.end());
        for(unsigned i=0;i<source.operations.size();++i)
            if(!removed.count(i) && (i<=layout.start || i>=layout.start+27)) Q5_operation(w,source,i);
    }
    const VHO_TENSOR_WORKLOAD_GEOMETRY &g=snapshot.schema.geometry;UINT64 rows=mul(mul(g.batch,g.heads),g.queries),tokens=mul(rows,g.keys);
    VHO_Q4_ESTIMATE &e=c.quantized;UINT64 ctas=std::max(UINT64(1),ceilq(rows,128)),registers=gpu.registers_per_thread;
    VHO_MLA_STAGE_ESTIMATE stage;stage.threads=128;
    if(c.width) {
        // Q4's fragment ledger is reusable, but its one-pass score is not.
        VHO_TENSOR_GPU_PROFILE envelope=gpu;envelope.scratch_budget=~UINT64(0);
        VHO_MLA_SEARCH_CANDIDATE cache=c;Q4_evaluate(cache,envelope,model,profile,snapshot);e=cache.quantized;
        e.packed=mul(e.packed,3);e.scales=mul(e.scales,3);e.page_requests=snapshot.schema.paged?mul(mul(mul(3,g.batch*g.keys),g.queries),ceilq(g.heads,c.descriptor.group)):0;
        e.conversions=mul(e.conversions,3);e.unpacks=mul(e.unpacks,3);e.partial=0;e.scenario_cycles.clear();e.traffic.clear();
        UINT64 repeats=mul(g.queries,ceilq(g.heads,c.descriptor.group)),capacity=mul(g.batch,g.keys);
        e.decoded_shared=add(mul(mul(mul(12,capacity),add(g.latent,g.rotary)),repeats),
            mul(mul(4,tokens),add(mul(4,g.latent),mul(3,g.rotary))));
        w.reads=add(w.reads,mul(8,e.page_requests));w.scalar=add(w.scalar,mul(4,e.page_requests));
        w.scalar=add(w.scalar,mul(tokens,add(mul(6,add(g.latent,g.rotary)),add(mul(2,g.latent),12))));
        w.exp=add(w.exp,mul(2,ceilq(tokens,32)));w.div=add(w.div,ceilq(tokens,32));
        w.reads=add(w.reads,add(mul(mul(12,tokens),add(g.latent,g.rotary)),mul(tokens,add(mul(4,g.latent),17))));
        w.writes=add(w.writes,add(mul(tokens,add(mul(4,g.latent),8)),mul(rows,add(mul(4,g.latent),16))));
        w.dependency=add(w.dependency,mul(g.keys,add(mul(3,add(g.latent,g.rotary)),4)));
        w.launches=add(w.launches,1);ctas=mul(mul(g.batch,g.queries),ceilq(g.heads,c.descriptor.group));
        stage.threads=mul(32,c.descriptor.group);stage.shared=cache.stages[0].shared;
        registers=add(registers,ceilq(g.latent,32));
    }
    stage.ctas=ctas;DSL_GEMM_OCCUPANCY_RESULT occupancy;
    if(!DSL_gemm_occupancy(gpu.resources,32,gpu.sms,stage.threads,mul(4,registers),stage.shared,ctas,&occupancy) || !occupancy.resident)
        throw std::runtime_error("q5_resource_fit");
    stage.resident=occupancy.resident;stage.movement.registers=registers;stage.movement.rounded_registers=occupancy.registers;stage.movement.rounded_shared=occupancy.shared;
    c.scratch=c.width?mul(mul(4,std::min((UINT64)c.width,g.keys)),add(g.latent,g.rotary)):0;
    if(c.scratch>gpu.scratch_budget) throw std::runtime_error("q5_tile_budget");
    UINT64 active=std::min(gpu.sms,ctas),rounds=ceilq(ctas,mul(gpu.sms,occupancy.resident)),weight=0,weighted=0,worst=0;
    const VHO_MLA_MOVEMENT_PROFILE &h=profile.hierarchy;
    for(unsigned s=0;s<h.scenarios.size();++s) {
        const VHO_MLA_CACHE_SCENARIO &scenario=h.scenarios[s];UINT64 read=Q4_sectors(w.reads,h),write=Q4_sectors(w.writes,h);
        UINT64 hbm=add(ceilq(mul(read,1000000-scenario.other_hit_ppm),1000000),write),l2=add(read,write);
        if(c.width) {
            // Source fragments are recomputed by the detached cache ledger;
            // three passes are explicit and never borrow Q4's score/merge costs.
            VHO_TENSOR_GPU_PROFILE envelope=gpu;envelope.scratch_budget=~UINT64(0);VHO_MLA_SEARCH_CANDIDATE cache=c;
            cache.quantized=VHO_Q4_ESTIMATE();cache.stages.clear();Q4_evaluate(cache,envelope,model,profile,snapshot);
            for(unsigned k=0;k<4;++k) {
                VHO_MLA_TRAFFIC_ESTIMATE t=cache.quantized.traffic[s*5+k];t.requested=mul(t.requested,3);t.transactions=t.l2=mul(t.transactions,3);
                t.hbm=ceilq(mul(t.transactions,1000000-scenario.cache_hit_ppm),1000000);hbm=add(hbm,t.hbm);l2=add(l2,t.l2);e.traffic.push_back(t);
            }
        }
        VHO_MLA_TRAFFIC_ESTIMATE t;t.operand="complete-source-other-reads";t.direction="read";t.scenario=scenario.id;t.requested=w.reads;t.transactions=t.l2=read;t.hbm=ceilq(mul(read,1000000-scenario.other_hit_ppm),1000000);e.traffic.push_back(t);
        t.operand="complete-source-writes";t.direction="write";t.requested=w.writes;t.transactions=t.l2=t.hbm=write;t.shared=e.decoded_shared;e.traffic.push_back(t);
        UINT64 memory=add(ceilq(l2,h.l2_rate),ceilq(hbm,h.hbm_rate));
        UINT64 compute=add(ceilq(w.scalar,mul(active,model.values[VHO_MLA_SCALAR_RATE])),mul(mul(w.dependency,model.values[VHO_MLA_DEPENDENCY]),rounds));
        UINT64 special=add(add(mul(w.exp,model.values[VHO_MLA_EXP]),mul(w.log,log_cycles)),mul(w.div,model.values[VHO_MLA_DIV]));
        compute=add(compute,ceilq(special,active));
        if(c.width) compute=add(compute,add(ceilq(e.conversions,mul(active,profile.convert_lanes)),ceilq(e.unpacks,mul(active,profile.unpack_lanes))));
        UINT64 local=ceilq(ceilq(mul(e.decoded_shared,h.shared_conflict_ppm),1000000),mul(active,model.values[VHO_MLA_SHARED_RATE]));
        UINT64 sync=c.width?mul(mul(mul(6,ceilq(g.keys,c.width)),rounds),model.values[VHO_MLA_CTA_BARRIER]):0;
        UINT64 total=add(Movement_sequence(memory,add(compute,local),sync),mul(w.launches,model.values[VHO_MLA_LAUNCH]));
        total=add(total,mul(w.launches,model.values[VHO_MLA_LOAD_LATENCY]));
        e.scenario_cycles.push_back(total);worst=std::max(worst,total);weighted=add(weighted,mul(total,scenario.weight));weight=add(weight,scenario.weight);
    }
    e.key="mla-q5-complete-v1/strict-three-pass/log=";std::ostringstream detail;detail<<log_cycles<<'/'<<Q4_key(profile,snapshot,gpu,model,c);e.key+=detail.str();
    e.modeled=true;e.explanation="Q5 complete strict-plan estimate: source quantization/update guards, query/value projections, three ordered attention passes, finite/zero guards, statistics and result copies included. Explicit synchronous placement/resource model; no measured GPU latency, TMA or AOT dispatch permission.";
    c.total=h.objective=="worst"?worst:ceilq(weighted,weight);c.stages.clear();c.stages.push_back(stage);c.eligible=true;c.reason="qualified_synchronous_high_v1";
}
}
