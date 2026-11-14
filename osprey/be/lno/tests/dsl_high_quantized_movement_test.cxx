/* Copyright (C) 2026 Open64 Project */
/* LNO Q4 detached cost-study tests. Synthetic snapshots below are deliberately
 * not runtime authority. Genuine Q3 export/freshness is tested separately.
 * Design: doc/AI-COMPILER-MLA-Q4-QUANTIZED-MOVEMENT-v1.0.md. */
#include "dsl_high_observer.h"
#include <cassert>
#include <stdexcept>
static unsigned checks=0;
/* Deterministic clock isolates budget semantics from host execution speed. */
UINT64 VHO_mla_search_test_clock_us() {return 100;}
/* Abort a focused test at the first violated independent invariant. */
static void check(bool ok,const char *name) {if(!ok) throw std::runtime_error(name);++checks;}
/* Supply all timing inputs; architecture does not imply any measured latency. */
static VHO_Q4_PROFILE profile() {
    VHO_Q4_PROFILE p;VHO_MLA_MOVEMENT_PROFILE &h=p.hierarchy;
    h.id="synthetic";h.architecture="sm_90";h.provenance="synthetic";h.objective="worst";
    h.hbm_rate=64;h.l2_rate=256;h.sector_bytes=32;h.request_amplification_ppm=h.shared_conflict_ppm=1000000;
    VHO_MLA_CACHE_SCENARIO s;s.id="cold";s.weight=1;h.scenarios.push_back(s);
    s.id="warm";s.cache_hit_ppm=900000;h.scenarios.push_back(s);
    p.convert_lanes=64;p.unpack_lanes=64;p.page_cycles=4;return p;
}
/* A bounded full study shares one lazy cache among existing strategy proposers. */
static VHO_MLA_SEARCH_CONTROL control() {
    VHO_MLA_SEARCH_CONTROL c;c.space=VHO_q4_catalog();c.global_proposals=51;c.global_unique=17;c.patience=17;
    for(unsigned i=0;i<c.runs.size();++i) c.runs[i].proposal_limit=17;return c;
}
/* Normalized synthetic pages have shared nonmonotone mappings, ragged valid
 * lengths and capacity tails. They exercise arithmetic, not serving admission. */
static VHO_Q4_SNAPSHOT snapshot(bool packed) {
    VHO_Q4_SNAPSHOT x;VHO_TENSOR_WORKLOAD_GEOMETRY &g=x.schema.geometry;
    g.known=true;g.batch=2;g.heads=3;g.queries=3;g.keys=129;g.latent=16;g.rotary=4;
    x.schema.paged=true;x.schema.page_tokens=64;x.schema.pages=4;x.schema.source_key="synthetic-not-live";
    x.schema.encoding=packed?"u4-offset8-f32.v1":"s8-token-scaled-f32.v1";
    g.source_key=x.schema.source_key;x.facts.workload.geometry=g;
    x.facts.bound=true;x.facts.ticket=1;x.facts.parent_bundle=2;
    x.facts.allocation_ids.push_back(3);x.facts.allocation_generations.push_back(4);x.facts.content_versions.push_back(5);
    x.facts.workload.binding_key="synthetic-workload";x.maintenance_bytes=1024;x.maintenance_ops=256;
    for(unsigned b=0;b<2;++b) {
        VHO_TENSOR_WORKLOAD_SEGMENT s;s.queries=b?0:3;s.valid=b?65:129;s.history=s.valid;x.facts.workload.segments.push_back(s);
        for(unsigned page=0;page<3;++page) {
            VHO_SERVING_PAGE p;p.request=b;p.logical_page=page;p.physical_page=page==0?2:page==1?0:3;
            p.capacity_tokens=page==2?1:64;p.valid_tokens=page==0?64:b?(page==1?1:0):p.capacity_tokens;
            UINT64 rows[]={packed?8U:16U,4,packed?2U:4U,4};
            for(unsigned k=0;k<4;++k) {p.row_bytes[k]=rows[k];p.byte_offsets[k]=p.physical_page*64*rows[k];p.base_alignment[k]=p.page_alignment[k]=16;}
            x.facts.pages.push_back(p);
        }
    }
    x.key=VHO_q4_snapshot_key(x);return x;
}
/* Refusals preserve the caller's existing result, including its prior winner. */
static void refuse(const DSL_HIGH_SEARCH_INPUT &input,const VHO_Q4_SNAPSHOT &x,
    const VHO_Q4_PROFILE &p,VHO_MLA_SEARCH_CONTROL c) {
    VHO_MLA_PRODUCTION_RESULT r;r.proposals=123;
    check(!VHO_mla_explore_quantized(input.gpu,input.model,c,p,x,&r,NULL) && r.proposals==123,"atomic_admission_refusal");
}
/* Exercise the normalized estimator with no runtime/compiler-context callback. */
int main(int argc,char **argv) {
    try {
        assert(argc==3);DSL_HIGH_SEARCH_INPUT input;std::string why;
        check(LNO_DSL_read_search_input(argv[1],input,why),"profile_decode");
        for(unsigned format=0;format<2;++format) {
            VHO_Q4_SNAPSHOT x=snapshot(format);VHO_Q4_PROFILE p=profile();VHO_MLA_SEARCH_CONTROL c=control();
            VHO_MLA_PRODUCTION_RESULT r;
            check(VHO_mla_explore_quantized(input.gpu,input.model,c,p,x,&r,stderr),"complete_study");
            check(r.exploration.complete && r.exploration.evaluations==17 && r.has_winner,"census");
            check(r.events[0].candidate==0 && r.proposals==r.exploration.evaluations+r.exploration.cache_hits,"shared_cache_baseline_first");
            for(unsigned i=0;i<r.exploration.candidates.size();++i) {
                const VHO_MLA_SEARCH_CANDIDATE &a=r.exploration.candidates[i];const VHO_Q4_ESTIMATE &e=a.quantized;
                check(a.eligible && e.modeled,"resource_fit");
                UINT64 repeats=a.width?3*((3+a.descriptor.group-1)/a.descriptor.group):1;
                check(e.packed==258*(format?10:20)*repeats && e.scales==258*8*repeats,"exact_packed_scale_counts");
                check(e.unique_physical==129 && e.logical_capacity==258 && e.logical_valid==194 && e.active_queries==3,"no_free_shared_prefix_or_mask_elision");
                check(e.conversions==258*20*repeats && e.unpacks==(format?e.conversions:0),"unpack_conversion_counts");
                if(a.width) {
                    UINT64 tiles=(129+a.width-1)/a.width;
                    check(e.decoded_global==0 && e.partial==(tiles>1?4*2*3*3*tiles*18:0),"split_tuple_and_no_score_buffer");
                    check(e.page_requests==2*(a.width==32?5:3)*repeats,"page_tile_intersections");
                    check(e.decoded_shared==4*258*20*repeats+4*258*3*3*36,"shared_producer_consumer_bytes");
                } else check(e.page_requests==258 && e.decoded_global==4*258*20+4*258*3*3*36,"materialized_baseline");
                check(e.scenario_cycles[1]<=e.scenario_cycles[0],"explicit_hit_scenario");
            }
            c.global_unique=1;
            check(VHO_mla_explore_quantized(input.gpu,input.model,c,p,x,&r,NULL) && r.exploration.evaluations==1,"global_unique_budget");
            c=control();c.stopping.push_back("relative-gap");refuse(input,x,p,c);
            c=control();c.stopping.push_back("cost-aware");refuse(input,x,p,c);

            c=control();
            for(unsigned strategy=0;strategy<3;++strategy) {
                VHO_MLA_SEARCH_CONTROL one=control();one.runs.assign(1,c.runs[strategy]);
                check(VHO_mla_explore_quantized(input.gpu,input.model,one,p,x,&r,NULL) && r.exploration.complete,"each_strategy_finite_exhaustion");
            }
            VHO_Q4_PROFILE weighted=p;weighted.hierarchy.objective="weighted";
            check(VHO_mla_explore_quantized(input.gpu,input.model,c,weighted,x,&r,NULL),"weighted_objective");
            VHO_TENSOR_GPU_PROFILE tight=input.gpu;tight.resources.shared_budget=1;
            check(VHO_mla_explore_quantized(tight,input.model,c,p,x,&r,NULL) && r.has_winner && r.exploration.winner==0,"resource_refusal_keeps_materialized_study");
            tight=input.gpu;tight.scratch_budget=1;
            check(VHO_mla_explore_quantized(tight,input.model,c,p,x,&r,NULL) && !r.exploration.candidates[1].eligible && r.exploration.candidates[4].eligible,"split_scratch_refusal_unsplit_survives");
            std::string key=x.key;x.facts.allocation_generations[0]++;
            check(key!=VHO_q4_snapshot_key(x),"allocation_generation_identity");x=snapshot(format);

            c=control();p.convert_lanes=0;refuse(input,x,p,c);p=profile();
            x.facts.content_versions[0]++;refuse(input,x,p,c);x=snapshot(format);
            x.facts.bound=false;x.key=VHO_q4_snapshot_key(x);refuse(input,x,p,c);x=snapshot(format);
            x.facts.allocation_generations.clear();x.key=VHO_q4_snapshot_key(x);refuse(input,x,p,c);x=snapshot(format);
            // A copied page with a mismatched scale stride cannot yield a model winner.
            x.facts.pages[0].row_bytes[1]=8;x.key=VHO_q4_snapshot_key(x);
            check(VHO_mla_explore_quantized(input.gpu,input.model,c,p,x,&r,NULL) && !r.has_winner,"per_candidate_page_refusal");
            x=snapshot(format);p.hierarchy.hbm_rate=1;
            check(VHO_mla_explore_quantized(input.gpu,input.model,c,p,x,&r,NULL),"slow_memory_scenario");
            // Saturating extents must fail rather than wrap into a cheap winner.
            x.schema.geometry.heads=~UINT64(0);x.key=VHO_q4_snapshot_key(x);
            check(VHO_mla_explore_quantized(input.gpu,input.model,c,p,x,&r,NULL) && !r.has_winner,"overflow_is_ineligible");
        }
        printf("PASS Q4 detached checks=%u; synthetic model only; no native state or GPU\n",checks);return 0;
    } catch(const std::exception &e) {fprintf(stderr,"FAIL Q4 %s\n",e.what());return 1;}
}
