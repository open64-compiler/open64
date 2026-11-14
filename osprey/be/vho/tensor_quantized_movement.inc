/* Copyright (C) 2026 Open64 Project */
/* Included by the existing VHO estimator: Q4 cache-service hypotheses share its
 * checked arithmetic, occupancy, readiness and bounded portfolio. This is not
 * a GPU realization or a replacement source program.
 * Design: doc/AI-COMPILER-MLA-Q4-QUANTIZED-MOVEMENT-v1.0.md. */
const char *VHO_q4_catalog() {return "mla-quantized-cache-space-v1-17";}
/* Frame every source/workload/version/page input used by the detached study.
 * This identity detects accidental stale copies; the live Q3 owner authorizes
 * export and revalidation. A caller-created string is not runtime authority. */
std::string VHO_q4_snapshot_key(const VHO_Q4_SNAPSHOT &x) {
    std::ostringstream k;k<<"mla-q4-snapshot-v1/"<<x.schema.source_key.size()<<':'<<x.schema.source_key
        <<'/'<<x.facts.workload.binding_key.size()<<':'<<x.facts.workload.binding_key
        <<'/'<<x.facts.workload.phase.size()<<':'<<x.facts.workload.phase
        <<'/'<<x.facts.workload.workload.size()<<':'<<x.facts.workload.workload
        <<'/'<<x.schema.encoding<<'/'<<x.schema.paged<<'/'<<x.schema.page_tokens<<'/'<<x.schema.pages
        <<'/'<<x.schema.geometry.batch<<'/'<<x.schema.geometry.heads<<'/'<<x.schema.geometry.queries
        <<'/'<<x.schema.geometry.keys<<'/'<<x.schema.geometry.latent<<'/'<<x.schema.geometry.rotary
        <<'/'<<x.maintenance_bytes<<'/'<<x.maintenance_ops<<'/'<<x.facts.bound<<'/'<<x.facts.ticket<<'/'<<x.facts.parent_bundle;
    k<<"/versions="<<x.facts.allocation_ids.size();
    for(size_t i=0;i<x.facts.allocation_ids.size();++i) {
        k<<'/'<<x.facts.allocation_ids[i];
        k<<'/'<<(i<x.facts.allocation_generations.size()?x.facts.allocation_generations[i]:0);
        k<<'/'<<(i<x.facts.content_versions.size()?x.facts.content_versions[i]:0);
    }
    k<<"/segments="<<x.facts.workload.segments.size();
    for(size_t i=0;i<x.facts.workload.segments.size();++i) {
        const VHO_TENSOR_WORKLOAD_SEGMENT &s=x.facts.workload.segments[i];
        k<<'/'<<s.queries<<'/'<<s.valid<<'/'<<s.history;
    }
    k<<"/pages="<<x.facts.pages.size();
    for(size_t i=0;i<x.facts.pages.size();++i) {
        const VHO_SERVING_PAGE &p=x.facts.pages[i];
        k<<'/'<<p.request<<'/'<<p.logical_page<<'/'<<p.physical_page<<'/'<<p.capacity_tokens<<'/'<<p.valid_tokens;
        for(unsigned j=0;j<4;++j) k<<'/'<<p.byte_offsets[j]<<'/'<<p.row_bytes[j]<<'/'<<p.base_alignment[j]<<'/'<<p.page_alignment[j];
    }
    return k.str();
}
namespace {
/* Refuse missing timing/binding inputs before consuming any proposal budget. */
bool Q4_valid(const VHO_Q4_PROFILE &p,const VHO_Q4_SNAPSHOT &x,const VHO_TENSOR_GPU_PROFILE &g) {
    const VHO_TENSOR_WORKLOAD_GEOMETRY &s=x.schema.geometry;
    if(!VHO_mla_movement_profile_valid(p.hierarchy,NULL) || p.hierarchy.architecture!=g.architecture ||
        !p.convert_lanes || p.convert_lanes>1000000000 || !p.unpack_lanes || p.unpack_lanes>1000000000 ||
        !p.page_cycles || p.page_cycles>1000000000 || !s.known || !s.batch || !s.heads || !s.queries ||
        !s.keys || !s.latent || !s.rotary || !x.facts.bound || !x.facts.ticket || x.schema.source_key.empty() ||
        s.source_key!=x.schema.source_key || x.facts.workload.geometry.source_key!=s.source_key ||
        !x.facts.workload.geometry.known ||
        x.key!=VHO_q4_snapshot_key(x) || x.facts.allocation_ids.empty() ||
        x.facts.allocation_ids.size()!=x.facts.allocation_generations.size() ||
        x.facts.allocation_ids.size()!=x.facts.content_versions.size() || x.facts.workload.segments.size()!=s.batch) return false;
    bool s8=x.schema.encoding=="s8-token-scaled-f32.v1",u4=x.schema.encoding=="u4-offset8-f32.v1",fake=x.schema.encoding=="";
    if(!s8 && !u4 && !fake) return false;
    if(u4 && (s.latent%2 || s.rotary%2)) return false;
    if(x.schema.paged && (!x.schema.page_tokens || !x.schema.pages ||
        x.facts.pages.size()!=mul(s.batch,ceilq(s.keys,x.schema.page_tokens)))) return false;
    if(!x.schema.paged && !x.facts.pages.empty()) return false;
    for(size_t i=0;i<x.facts.workload.segments.size();++i) {
        const VHO_TENSOR_WORKLOAD_SEGMENT &r=x.facts.workload.segments[i];
        if(r.queries<0 || (UINT64)r.queries>s.queries || r.valid<0 || (UINT64)r.valid>s.keys) return false;
    }
    return true;
}
/* Source-materialized cache service plus sixteen fused retained-tile studies.
 * All remain analysis-only; model eligibility never grants application rights. */
void Q4_catalog(VHO_MLA_SEARCH_RESULT &r) {
    VHO_MLA_SEARCH_CANDIDATE base;base.policy=0;r.candidates.push_back(base);
    const UINT32 groups[]={1,2,4,8},widths[]={32,64,128,256};
    for(unsigned i=0;i<4;++i) for(unsigned j=0;j<4;++j) {
        VHO_MLA_SEARCH_CANDIDATE c;c.policy=0;c.width=widths[j];
        c.descriptor.group=groups[i];c.descriptor.retained=true;r.candidates.push_back(c);
    }
}
/* Per-fragment sector envelope with unknown device-base alignment: charge up
 * to one extra sector at either end. Host addresses never prove GPU alignment.
 * Stores are write-through; read hit scenarios are fixed across candidates. */
UINT64 Q4_sectors(UINT64 bytes,const VHO_MLA_MOVEMENT_PROFILE &p) {
    if(!bytes) return 0;
    UINT64 amplified=ceilq(mul(bytes,p.request_amplification_ppm),1000000);
    return mul(ceilq(add(amplified,p.sector_bytes-1),p.sector_bytes),p.sector_bytes);
}
/* Complete model identity includes timings, resources and controls elsewhere;
 * never identify changed facts or synthetic rates by profile name alone. */
std::string Q4_key(const VHO_Q4_PROFILE &p,const VHO_Q4_SNAPSHOT &x,const VHO_TENSOR_GPU_PROFILE &g,
    const VHO_MLA_COST_PROFILE &m,const VHO_MLA_SEARCH_CANDIDATE &c) {
    std::ostringstream k;k<<"mla-q4-cache-service-v1/"<<x.key.size()<<':'<<x.key<<'/'
        <<VHO_mla_movement_profile_key(p.hierarchy)<<'/'<<p.convert_lanes<<'/'<<p.unpack_lanes<<'/'<<p.page_cycles
        <<"/W="<<c.width<<"/G="<<c.descriptor.group<<"/all-analysis-only/synchronous/no-prefix-credit/"
        <<g.id.size()<<':'<<g.id<<'/'<<g.architecture<<'/'<<g.sms<<'/'<<g.registers_per_thread<<'/'
        <<g.target_ctas_per_sm<<'/'<<g.scratch_budget;
    const DSL_HARDWARE_RESOURCE_RECORD &r=g.resources;
    k<<'/'<<r.max_blocks<<'/'<<r.max_warps<<'/'<<r.max_threads<<'/'<<r.max_threads_per_cta<<'/'
        <<r.registers_per_sm<<'/'<<r.max_registers_per_thread<<'/'<<r.register_quantum<<'/'<<r.register_warp_quantum
        <<'/'<<r.shared_capacity<<'/'<<r.shared_budget<<'/'<<r.shared_quantum<<'/'<<r.shared_reserved;
    k<<'/'<<m.id.size()<<':'<<m.id<<'/'<<m.provenance;
    for(unsigned i=0;i<VHO_MLA_COST_FIELDS;++i) k<<'/'<<m.values[i];return k.str();
}
/* Model only cache service (maintenance, delivery/dequantization, cache reads,
 * staging and split tuples). Query/output projections and attention arithmetic
 * are excluded, so this score is never advertised as end-to-end kernel time.
 * Capacity work remains charged even with zero active queries/masked tokens:
 * Q3 metadata alone does not authorize source-loop elimination. */
void Q4_evaluate(VHO_MLA_SEARCH_CANDIDATE &c,const VHO_TENSOR_GPU_PROFILE &g,
    const VHO_MLA_COST_PROFILE &m,const VHO_Q4_PROFILE &profile,const VHO_Q4_SNAPSHOT &x) {
    c.evaluated=true;VHO_Q4_ESTIMATE &e=c.quantized;
    const VHO_TENSOR_WORKLOAD_GEOMETRY &a=x.schema.geometry;
    const VHO_MLA_MOVEMENT_PROFILE &h=profile.hierarchy;
    UINT64 group=c.descriptor.group,groups=ceilq(a.heads,group),tiles=c.width?ceilq(a.keys,c.width):1;
    UINT64 rows[4]={a.latent,4,a.rotary,4};
    bool u4=x.schema.encoding=="u4-offset8-f32.v1",fake=x.schema.encoding=="";
    if(u4) {rows[0]/=2;rows[2]/=2;}if(fake) {rows[0]=mul(rows[0],4);rows[2]=mul(rows[2],4);}
    UINT64 physical=0;std::map<UINT64,UINT64> page_lengths;
    UINT64 sectors=0,stream_sectors[4]={0,0,0,0},logical=0,valid=0;
    if(x.schema.paged) {
        for(size_t i=0;i<x.facts.pages.size();++i) {
            const VHO_SERVING_PAGE &p=x.facts.pages[i];
            UINT64 expected_page=i%ceilq(a.keys,x.schema.page_tokens),expected_request=i/ceilq(a.keys,x.schema.page_tokens);
            if(p.request!=expected_request || p.logical_page!=expected_page || p.physical_page>=x.schema.pages ||
                p.capacity_tokens!=std::min(x.schema.page_tokens,a.keys-p.logical_page*x.schema.page_tokens) ||
                p.valid_tokens>p.capacity_tokens) throw std::runtime_error("q4_page_join");
            for(unsigned k=0;k<4;++k) if(p.row_bytes[k]!=rows[k] ||
                p.byte_offsets[k]!=mul(mul(p.physical_page,x.schema.page_tokens),rows[k])) throw std::runtime_error("q4_scale_page_layout");
            logical=add(logical,p.capacity_tokens);valid=add(valid,p.valid_tokens);
            page_lengths[p.physical_page]=std::max(page_lengths[p.physical_page],p.capacity_tokens);
            // Intersect allocator pages and compute tiles; P is never tile W.
            UINT64 first=mul(p.logical_page,x.schema.page_tokens),left=p.capacity_tokens;
            while(left) {
                UINT64 n=c.width?std::min(left,(UINT64)c.width-first%c.width):left;
                for(unsigned k=0;k<4;++k) stream_sectors[k]=add(stream_sectors[k],Q4_sectors(mul(n,rows[k]),h));
                e.page_requests=add(e.page_requests,1);first=add(first,n);left-=n;
            }
        }
        for(std::map<UINT64,UINT64>::const_iterator i=page_lengths.begin();i!=page_lengths.end();++i) physical=add(physical,i->second);
    } else {
        logical=mul(a.batch,a.keys);physical=logical;
        for(UINT64 b=0;b<a.batch;++b) {
            valid=add(valid,x.facts.workload.segments[b].valid);
            for(UINT64 t=0;t<a.keys;) {
                UINT64 n=c.width?std::min(a.keys-t,(UINT64)c.width):a.keys-t;
                for(unsigned k=0;k<4;++k) stream_sectors[k]=add(stream_sectors[k],Q4_sectors(mul(n,rows[k]),h));t=add(t,n);
            }
        }
    }
    e.logical_capacity=logical;e.logical_valid=valid;e.unique_physical=physical;
    for(size_t i=0;i<x.facts.workload.segments.size();++i) e.active_queries=add(e.active_queries,x.facts.workload.segments[i].queries);
    if(!c.width && x.schema.paged) e.page_requests=logical;
    UINT64 repeats=c.width?mul(a.queries,groups):1;
    e.packed=mul(mul(logical,add(rows[0],rows[2])),repeats);e.scales=mul(mul(logical,8),repeats);
    e.page_requests=mul(e.page_requests,repeats);
    for(unsigned k=0;k<4;++k) {stream_sectors[k]=mul(stream_sectors[k],repeats);sectors=add(sectors,stream_sectors[k]);}
    UINT64 values=mul(logical,add(a.latent,a.rotary));e.conversions=mul(values,repeats);e.unpacks=u4?e.conversions:0;
    // Source baseline writes then rereads gathered payload/scales before decode.
    UINT64 other=x.maintenance_bytes;
    if(!c.width && x.schema.paged) other=add(other,mul(2,add(e.packed,e.scales)));
    if(!c.width) {
        e.decoded_global=add(mul(4,values),mul(mul(mul(4,logical),mul(a.queries,a.heads)),add(mul(2,a.latent),a.rotary)));
        other=add(other,e.decoded_global);
    } else {
        // Retain decoded rows once per head group/query. Shared consumers read
        // latent twice (score + weighted sum), rotary once, with padded storage.
        e.decoded_shared=add(mul(mul(4,values),repeats),
            mul(mul(mul(4,logical),mul(a.queries,a.heads)),add(mul(2,a.latent),a.rotary)));
        e.partial=tiles>1?mul(mul(mul(mul(4,a.batch),a.queries),a.heads),mul(tiles,add(a.latent,2))):0;
        c.scratch=e.partial;if(c.scratch>g.scratch_budget) throw std::runtime_error("q4_scratch_budget");
        other=add(other,mul(2,e.partial));
    }
    e.maintenance=x.maintenance_bytes;
    UINT64 ctas=c.width?mul(mul(mul(a.batch,a.queries),groups),tiles):mul(mul(mul(a.batch,a.queries),a.heads),ceilq(a.keys,128));
    VHO_MLA_STAGE_ESTIMATE stage;stage.threads=c.width?mul(32,group):128;stage.ctas=ctas;
    stage.shared=c.width?mul(4,add(mul(c.width,add(a.latent,a.rotary)),mul(group,add(a.latent,2)))):0;
    UINT64 registers=add(g.registers_per_thread,c.width?ceilq(a.latent,32):0);
    DSL_GEMM_OCCUPANCY_RESULT occ;
    if(!DSL_gemm_occupancy(g.resources,32,g.sms,stage.threads,mul(4,registers),stage.shared,ctas,&occ) || !occ.resident)
        throw std::runtime_error("q4_resource_fit");
    stage.resident=occ.resident;stage.movement.registers=registers;stage.movement.rounded_registers=occ.registers;
    stage.movement.rounded_shared=occ.shared;stage.movement.unused_slots=occ.unused;
    UINT64 rounds=ceilq(ctas,mul(g.sms,occ.resident)),active_sms=std::min(g.sms,ctas),weight=0,weighted=0,worst=0;
    UINT64 lookup=mul(e.page_requests,8);other=add(other,lookup);
    UINT64 other_sectors=Q4_sectors(other,h);
    for(size_t i=0;i<h.scenarios.size();++i) {
        const VHO_MLA_CACHE_SCENARIO &s=h.scenarios[i];
        UINT64 hbm=other_sectors;
        for(unsigned k=0;k<4;++k) hbm=add(hbm,ceilq(mul(stream_sectors[k],1000000-s.cache_hit_ppm),1000000));
        const char *operands[]={"latent-packed","latent-scale","rotary-packed","rotary-scale"};
        for(unsigned k=0;k<4;++k) {
            VHO_MLA_TRAFFIC_ESTIMATE t;t.operand=operands[k];t.direction="read";t.scenario=s.id;
            t.requested=mul(mul(logical,rows[k]),repeats);t.transactions=t.l2=stream_sectors[k];
            t.hbm=ceilq(mul(t.transactions,1000000-s.cache_hit_ppm),1000000);e.traffic.push_back(t);
        }
        VHO_MLA_TRAFFIC_ESTIMATE t;t.operand="maintenance-decoded-tuples-page-controls";t.direction="mixed";t.scenario=s.id;
        t.requested=other;t.transactions=t.l2=t.hbm=other_sectors;t.shared=e.decoded_shared;e.traffic.push_back(t);
        // Maintenance contains stores and arbitrary source reads: conservatively
        // no hit credit. Shared-prefix physical overlap earns no scheduling reuse.
        UINT64 delivery=add(ceilq(add(sectors,other_sectors),h.l2_rate),ceilq(hbm,h.hbm_rate));
        delivery=add(delivery,mul(rounds,m.values[VHO_MLA_LOAD_LATENCY]));
        UINT64 convert=ceilq(e.conversions,mul(active_sms,profile.convert_lanes));
        UINT64 unpack=ceilq(e.unpacks,mul(active_sms,profile.unpack_lanes));
        UINT64 local=ceilq(ceilq(mul(e.decoded_shared,h.shared_conflict_ppm),1000000),mul(active_sms,m.values[VHO_MLA_SHARED_RATE]));
        UINT64 bookkeeping=add(ceilq(x.maintenance_ops,mul(active_sms,m.values[VHO_MLA_SCALAR_RATE])),
            ceilq(mul(e.page_requests,profile.page_cycles),active_sms));
        UINT64 sync=c.width?mul(mul(2,rounds),m.values[VHO_MLA_CTA_BARRIER]):0;
        UINT64 launches=mul(c.width?(tiles>1?2:1):2,m.values[VHO_MLA_LAUNCH]);
        UINT64 total=add(Movement_sequence(delivery,add(add(convert,unpack),add(local,bookkeeping)),sync),launches);
        e.scenario_cycles.push_back(total);worst=std::max(worst,total);weight=add(weight,s.weight);weighted=add(weighted,mul(total,s.weight));
    }
    c.total=h.objective=="worst"?worst:ceilq(weighted,weight);c.stages.push_back(stage);
    e.key=Q4_key(profile,x,g,m,c);e.modeled=true;c.eligible=true;c.reason="analysis_only_cache_service";
    std::ostringstream text;text<<"Q4 analysis-only cache service: "<<(c.width?"retained decoded tile":"materialized decoded baseline")
        <<"; G="<<group<<", W="<<c.width<<", allocator P="<<x.schema.page_tokens
        <<". Packed rows and paired F32 scales follow the Q3 page mapping; source-capacity reads remain even for masked/zero-active work. "
        <<"Decode/convert, shared writes and both latent consumers, page fragments, split tuple writes/reads and unchanged source maintenance are charged. "
        <<"Shared prefixes earn no cross-request reuse; host alignment is not GPU alignment. Synchronous delivery precedes consumption; no TMA/overlap credit. "
        <<"Model score excludes attention arithmetic and projections and cannot authorize lowering, execution, launch elision or AOT selection.";
    e.explanation=text.str();
}
}
