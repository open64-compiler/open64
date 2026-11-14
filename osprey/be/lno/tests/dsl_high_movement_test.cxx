/* Copyright (C) 2026 Open64 Project */
/* Detached analytical MR2 fixtures. Synthetic assumptions, never GPU timing. */
#include "dsl_high_observer.h"
#include <algorithm>
#include <cassert>
#include <stdexcept>
static unsigned checks=0;
static UINT64 clock_now=100,clock_step=0;
UINT64 VHO_mla_search_test_clock_us() {UINT64 value=clock_now;clock_now+=clock_step;return value;}
static void check(bool ok,const char *name) {
    if(!ok) throw std::runtime_error(name);printf("PASS %s\n",name);++checks;
}
static VHO_MLA_MOVEMENT_PROFILE profile() {
    VHO_MLA_MOVEMENT_PROFILE p;p.id="synthetic";p.architecture="sm_90";p.provenance="synthetic";p.objective="worst";
    p.hbm_rate=64;p.l2_rate=256;p.sector_bytes=32;p.request_amplification_ppm=p.shared_conflict_ppm=1000000;
    VHO_MLA_CACHE_SCENARIO c;c.id="cold";c.weight=1;p.scenarios.push_back(c);c.id="warm";c.cache_hit_ppm=900000;p.scenarios.push_back(c);
    return p;
}
static VHO_MLA_SEARCH_CONTROL control() {
    VHO_MLA_SEARCH_CONTROL c;c.space=VHO_mla_movement_catalog();c.global_proposals=108;c.global_unique=36;c.patience=36;
    for(size_t i=0;i<c.runs.size();++i) c.runs[i].proposal_limit=36;return c;
}
static VHO_MLA_PRODUCTION_RESULT explore(const DSL_HIGH_SEARCH_INPUT &input,VHO_MLA_SEARCH_CONTROL c,
    VHO_MLA_MOVEMENT_PROFILE h=profile()) {
    VHO_TENSOR_PROGRAM p=input.algorithm;p.fuse=p.cooperative=p.fuse_attention=p.register_latent=true;p.split_width=128;
    VHO_TENSOR_ATTENTION a=VHO_tensor_attention(p);
    VHO_MLA_MOVEMENT_CONTEXT x;x.profile=h;x.proof_key="synthetic-unit-proof";
    x.latent_payload=4*a.batch*a.length*a.rank;x.rotary_payload=4*a.batch*a.length*a.rope;
    VHO_MLA_PRODUCTION_RESULT r;check(VHO_mla_explore_movement(p,input.gpu,input.model,c,x,&r,stderr),"movement_call");return r;
}
static UINT64 traffic(const VHO_MLA_SEARCH_CANDIDATE &c,const char *scenario,const char *operand,const char *field) {
    UINT64 total=0;
    for(size_t j=0;j<c.stages.size();++j) for(size_t k=0;k<c.stages[j].movement.traffic.size();++k) {
        const VHO_MLA_TRAFFIC_ESTIMATE &t=c.stages[j].movement.traffic[k];
        if(t.scenario==scenario && t.operand==operand) total+=std::string(field)=="HBM"?t.hbm:std::string(field)=="L2"?t.l2:t.requested;
    }return total;
}
int main(int argc,char **argv) {
    try {
        assert(argc==3);DSL_HIGH_SEARCH_INPUT inputs[2];std::string why;
        for(unsigned i=0;i<2;++i) check(LNO_DSL_read_search_input(argv[i+1],inputs[i],why),"legacy_input_decode");
        VHO_MLA_MOVEMENT_PROFILE h=profile();check(VHO_mla_movement_profile_valid(h,NULL),"explicit_profile");
        std::string key=VHO_mla_movement_profile_key(h);h.scenarios[1].cache_hit_ppm--;
        check(key!=VHO_mla_movement_profile_key(h),"same_id_hit_rebinding");
        h=profile();h.hbm_rate=0;check(!VHO_mla_movement_profile_valid(h,NULL),"missing_rate_refused");
        h=profile();h.scenarios[1].cache_hit_ppm=1000001;check(!VHO_mla_movement_profile_valid(h,NULL),"invalid_hit_refused");
        h=profile();h.scenarios[1].id="cold";check(!VHO_mla_movement_profile_valid(h,NULL),"duplicate_scenario_refused");
        h=profile();h.request_amplification_ppm=999999;check(!VHO_mla_movement_profile_valid(h,NULL),"free_transaction_discount_refused");
        h=profile();h.shared_conflict_ppm=0;check(!VHO_mla_movement_profile_valid(h,NULL),"missing_shared_assumption_refused");
        VHO_MLA_SEARCH_CONTROL c=control();
        check(VHO_mla_movement_control_valid(c,NULL) && !VHO_mla_search_control_valid(c,NULL),"separate_control_domain");
        c.stopping.push_back("relative-gap");check(!VHO_mla_movement_control_valid(c,NULL),"legacy_gap_certificate_disabled");
        c=control();c.stopping.push_back("cost-aware");check(!VHO_mla_movement_control_valid(c,NULL),"legacy_economic_bound_disabled");
        for(unsigned i=0;i<2;++i) {
            c=control();VHO_MLA_PRODUCTION_RESULT r=explore(inputs[i],c);
            check(r.has_winner && r.exploration.complete && r.exploration.evaluations==36,"full_finite_census");
            check(r.exploration.candidates[r.exploration.winner].descriptor.group==1,"unimplemented_cannot_win");
            check(r.proposals==r.exploration.evaluations+r.exploration.cache_hits && r.events[0].candidate==(i?3:0),"baseline_first_charged_once");
            for(size_t j=0;j<r.exploration.candidates.size();++j)
                if(r.exploration.candidates[j].descriptor.group>1) check(!r.exploration.candidates[j].eligible,"capability_gate");
            const VHO_MLA_SEARCH_CANDIDATE &g1=r.exploration.candidates[1],&g2=r.exploration.candidates[12],
                &retained=r.exploration.candidates[13],&g4=r.exploration.candidates[20];
            UINT64 l=i?8193:127,b=1,heads=3,rank=16,rope=4,n=(l+31)/32;
            check(g1.movement.modeled && g2.movement.modeled && retained.movement.modeled && g4.movement.modeled,"analytical_group_counts_available");
            check(g1.movement.cache_requests==4*b*heads*l*(2*rank+rope),"G1_two_sweeps_exact_tail");
            check(g2.movement.cache_requests==4*b*((heads+1)/2)*l*(2*rank+rope),"G2_real_head_reuse");
            check(g4.movement.cache_requests==4*b*l*(2*rank+rope) && g4.movement.head_slots==4,"G4_padded_head_not_charged_as_read");
            check(retained.movement.cache_requests==4*b*((heads+1)/2)*l*(rank+rope),"retained_tile_one_latent_sweep");
            bool rounded=false;
            for(size_t st=0;st<g2.stages.size();++st) if(g2.stages[st].operation==33) {
                const VHO_MLA_STAGE_ESTIMATE &s=g2.stages[st];
                rounded=true;check(s.movement.rounded_registers==7168 && s.movement.rounded_shared==2816,"rounded_reserved_resources_include_head_tail");
            }
            check(rounded,"attention_resources_present");
            check(g2.scratch==g1.scratch && g1.scratch==4*b*heads*n*(rank+2),"grouping_keeps_per_head_tuples");
            check(g1.movement.partial_requests==4*b*heads*n*(2*rank+6),"tuple_write_read_repeated_statistics");
            check(g1.movement.score_storage==0 && g2.movement.score_storage==0,"no_score_buffers");
            check(traffic(g1,"warm","latent-cache","HBM")<traffic(g1,"cold","latent-cache","HBM") &&
                traffic(g1,"warm","latent-cache","L2")==traffic(g1,"cold","latent-cache","L2"),"cache_scenario_level_separation");
            check(traffic(g1,"warm","partial-tuple","HBM")>=g1.scratch,"stores_no_hit_discount");
            check(g2.movement.cache_requests==4*b*((heads+1)/2)*l*(2*rank+rope),"units_device_bytes");
            UINT64 minimum=UINT64_MAX;
            for(size_t j=0;j<r.exploration.candidates.size();++j) if(r.exploration.candidates[j].eligible)
                minimum=std::min(minimum,r.exploration.candidates[j].total);
            check(r.exploration.candidates[r.exploration.winner].total==minimum,"minimum_executable_new_model");
            for(size_t j=0;j<g1.movement.scenario_cycles.size();++j) {
                UINT64 sum=0;
                for(size_t k=0;k<g1.stages.size();++k) if(g1.stages[k].movement.traffic[0].scenario==profile().scenarios[j].id) sum+=g1.stages[k].total;
                check(sum==g1.movement.scenario_cycles[j],"stage_costs_sum_without_double_charge");
            }
            c.global_unique=1;c.global_proposals=1;VHO_MLA_PRODUCTION_RESULT one=explore(inputs[i],c);
            check(one.has_winner && one.exploration.evaluations==1 && one.proposals==1,"one_budget_keeps_compatible_baseline");
            h=profile();h.objective="weighted";h.scenarios[1].weight=3;c=control();one=explore(inputs[i],c,h);
            const VHO_MLA_SEARCH_CANDIDATE &candidate=one.exploration.candidates[1];
            check(candidate.total==(candidate.movement.scenario_cycles[0]+3*candidate.movement.scenario_cycles[1]+3)/4,"fixed_weight_objective");
            DSL_HIGH_SEARCH_INPUT highreg=inputs[i];highreg.gpu.registers_per_thread=250;
            one=explore(highreg,c);
            check(!one.exploration.candidates[12].movement.modeled && one.exploration.candidates[12].reason=="movement_resource_fit","no_implicit_spilling");
        }
        c=control();c.runs.resize(1);c.runs[0].strategy="exhaustive";c.global_proposals=36;
        VHO_MLA_PRODUCTION_RESULT one=explore(inputs[1],c);
        check(one.proposals==36 && one.exploration.complete,"exhaustive_with_forced_seed_no_duplicate");
        c=control();c.global_time_us=50;clock_step=100;
        one=explore(inputs[1],c);clock_step=0;
        check(!one.has_winner && !one.proposals && one.termination=="global_time_budget","new_catalog_deadline_before_seed");
        DSL_HIGH_SEARCH_INPUT huge=inputs[1];
        for(size_t v=0;v<huge.algorithm.values.size();++v) for(size_t d=0;d<huge.algorithm.values[v].shape.size();++d)
            if(huge.algorithm.values[v].shape[d]==8193) huge.algorithm.values[v].shape[d]=524288;
        for(size_t op=0;op<huge.algorithm.operations.size();++op) {
            std::map<std::string,std::string> &attrs=huge.algorithm.operations[op].attributes;
            if(attrs.count("attr.target_shape")) {
                std::string &shape=attrs["attr.target_shape"];size_t pos=shape.find("8193");
                if(pos!=std::string::npos) shape.replace(pos,4,"524288");
            }
        }
        for(unsigned f=0;f<VHO_MLA_COST_FIELDS;++f) huge.model.values[f]=1000000000;
        h=profile();h.scenarios[0].weight=h.scenarios[1].weight=1000000;c=control();one=explore(huge,c,h);
        bool overflow=false;
        for(size_t j=0;j<one.exploration.candidates.size();++j) {
            const VHO_MLA_SEARCH_CANDIDATE &candidate=one.exploration.candidates[j];
            if(candidate.reason=="estimate_overflow") {
                overflow=true;check(!candidate.eligible && !candidate.movement.modeled && !candidate.total && candidate.stages.empty(),"overflow_has_no_partial_estimate");
            }
        }
        check(overflow,"checked_weighted_count_overflow");
        DSL_HIGH_SEARCH_INPUT wide=inputs[1];
        for(size_t v=0;v<wide.algorithm.values.size();++v) for(size_t d=0;d<wide.algorithm.values[v].shape.size();++d)
            if(wide.algorithm.values[v].shape[d]==16) wide.algorithm.values[v].shape[d]=64;
        for(size_t op=0;op<wide.algorithm.operations.size();++op) {
            std::map<std::string,std::string> &attrs=wide.algorithm.operations[op].attributes;
            if(attrs.count("attr.target_shape")) {
                std::string &shape=attrs["attr.target_shape"];size_t pos=shape.find("16");
                if(pos!=std::string::npos) shape.replace(pos,2,"64");
            }
        }
        one=explore(wide,control());
        check(one.exploration.candidates[1].reason=="movement_private_tuple_capacity","private_source_tuple_capacity_admitted_before_win");
        check(one.exploration.candidates[19].reason=="movement_static_smem_48KiB" &&
            one.exploration.candidates[18].movement.modeled,"retention_credit_requires_full_live_tile_fit");
        printf("movement_cost_checks=%u PASS\n",checks);return 0;
    } catch(const std::exception &e) {fprintf(stderr,"FAIL %s\n",e.what());return 1;}
}
