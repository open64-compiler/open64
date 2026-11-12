/* Copyright (C) 2026 Open64 Project */
/* Pure controlled-search regressions with a test-only monotonic clock.
 * Loads real retained MLA recipes; no native compiler contexts or factories.
 * Design: doc/AI-COMPILER-SEARCH-M3-CONTROLS-v1.0.md. */
#include "../dsl_high_observer.h"
#include <algorithm>
#include <cstdio>
#include <stdexcept>

static std::vector<UINT64> VHO_test_ticks;
static size_t VHO_test_tick_index;
static unsigned VHO_test_checks;

/* Only the test search object calls this symbol; production uses libc time. */
UINT64 VHO_mla_search_test_clock_us() {
    if(VHO_test_tick_index<VHO_test_ticks.size()) return VHO_test_ticks[VHO_test_tick_index++];
    return VHO_test_ticks.back();
}
/* Reset deterministic time between tests; it cannot be selected by JSON. */
static void VHO_test_clock(UINT64 first=100,UINT64 second=100,UINT64 third=100) {
    VHO_test_ticks.clear();VHO_test_ticks.push_back(first);VHO_test_ticks.push_back(second);VHO_test_ticks.push_back(third);VHO_test_tick_index=0;
}
/* Fail immediately and retain individual checked contract names. */
static void VHO_test_require(bool value,const char *name) {
    if(!value) {fprintf(stderr,"FAIL %s\n",name);throw std::runtime_error(name);}
    printf("PASS %s\n",name);++VHO_test_checks;
}
/* Prepare only detached hypotheses, exactly as the native observer does. */
static VHO_TENSOR_PROGRAM VHO_test_program(const DSL_HIGH_SEARCH_INPUT &input) {
    VHO_TENSOR_PROGRAM program=input.algorithm;
    program.fuse=program.cooperative=program.fuse_attention=program.register_latent=true;program.split_width=128;return program;
}
/* Run typed controls against the real shape/model. Invalid input never writes
 * an apparently successful result or substitutes a fallback candidate. */
static VHO_MLA_PRODUCTION_RESULT VHO_test_search(const DSL_HIGH_SEARCH_INPUT &input,const VHO_MLA_SEARCH_CONTROL &control) {
    VHO_MLA_PRODUCTION_RESULT result;
    VHO_test_require(VHO_mla_explore_controlled(VHO_test_program(input),input.gpu,input.model,control,&result,NULL),"controlled_call_success");return result;
}
/* Test independent hard caps, shared-cache charging, every supported stopping
 * mechanism, simultaneous predicates, fairness and cooperative time limits. */
int main(int argc,char **argv) {
    try {
        if(argc!=3) throw std::runtime_error("expected short and long binding paths");
        DSL_HIGH_SEARCH_INPUT short_input,long_input;std::string reason;
        if(!LNO_DSL_read_search_input(argv[1],short_input,reason) || !LNO_DSL_read_search_input(argv[2],long_input,reason))
            throw std::runtime_error("fixture preparation: "+reason);
        VHO_MLA_SEARCH_CONTROL control;VHO_test_clock();VHO_MLA_PRODUCTION_RESULT full=VHO_test_search(short_input,control);
        VHO_MLA_SEARCH_OPTIONS legacy_options;VHO_MLA_SEARCH_RESULT legacy;
        if(!VHO_mla_explore(VHO_test_program(short_input),short_input.gpu,short_input.model,legacy_options,&legacy,NULL))
            throw std::runtime_error("legacy reference exploration failed");
        VHO_test_require(full.has_winner && full.exploration.complete && full.exploration.evaluations==12 &&
            full.exploration.winner==legacy.winner,"full_union_matches_legacy_optimum");
        VHO_test_require(full.events.size()==full.proposals && full.proposals==full.exploration.evaluations+full.exploration.cache_hits,"cache_and_proposal_accounting");
        VHO_test_require(full.events[0].run==0 && full.events[1].run==1 && full.events[2].run==2,"round_robin_fairness");
        VHO_test_require(full.termination=="declared_space_complete" && full.predicates.size()>=2,"complete_and_unique_limit_both_recorded");

        control.global_proposals=1;VHO_test_clock();VHO_MLA_PRODUCTION_RESULT one=VHO_test_search(short_input,control);
        VHO_test_require(one.proposals==1 && one.exploration.evaluations==1 && !one.exploration.complete && one.termination=="global_proposal_budget","global_proposal_cap");
        control=VHO_MLA_SEARCH_CONTROL();control.global_unique=1;VHO_test_clock();one=VHO_test_search(short_input,control);
        VHO_test_require(one.exploration.evaluations==1 && one.proposals==1 && one.termination=="global_unique_budget","global_unique_cap");

        control=VHO_MLA_SEARCH_CONTROL();control.runs.resize(2);
        for(unsigned i=0;i<2;++i) {control.runs[i].strategy="exhaustive";control.runs[i].id=i?"second":"first";}
        control.global_unique=2;VHO_test_clock();one=VHO_test_search(short_input,control);
        VHO_test_require(one.proposals==3 && one.exploration.evaluations==2 && one.exploration.cache_hits==1 && one.events[1].cache_hit,"cache_hit_uses_proposal_not_unique");
        control.global_unique=12;control.global_proposals=2;VHO_test_clock();one=VHO_test_search(short_input,control);
        VHO_test_require(one.proposals==2 && one.exploration.evaluations==1 && one.exploration.cache_hits==1,"cached_proposal_respects_global_cap");

        control=VHO_MLA_SEARCH_CONTROL();control.runs.resize(1);control.runs[0].strategy="exhaustive";control.runs[0].proposal_limit=1;
        VHO_test_clock();one=VHO_test_search(long_input,control);
        VHO_test_require(!one.has_winner && !one.exploration.complete && one.exploration.evaluations==1 && one.termination=="all_runs_stopped","no_eligible_incumbent_is_reported");
        control.runs[0].proposal_limit=12;control.stopping.push_back("plateau");control.patience=1;control.minimum_gain=1000000000;
        VHO_test_clock();one=VHO_test_search(short_input,control);
        VHO_test_require(one.exploration.runs[0].termination=="empirical_plateau" && one.proposals==2 && !one.exploration.complete,"per_run_empirical_plateau");
        UINT64 minimum=0;for(size_t i=0;i<one.exploration.candidates.size();++i) {
            const VHO_MLA_SEARCH_CANDIDATE &candidate=one.exploration.candidates[i];
            if(candidate.evaluated && candidate.eligible && (!minimum || candidate.total<minimum)) minimum=candidate.total;
        }
        VHO_test_require(one.exploration.candidates[one.exploration.winner].total==minimum,"minimum_gain_does_not_discard_better_incumbents");

        control=VHO_MLA_SEARCH_CONTROL();control.runs.resize(1);control.runs[0].strategy="exhaustive";
        control.stopping.push_back("target");control.target=UINT64(1000000000000000000ULL);control.global_proposals=1;
        VHO_test_clock();one=VHO_test_search(short_input,control);
        VHO_test_require(one.has_winner && one.proposals==1 && std::find(one.predicates.begin(),one.predicates.end(),"estimated_target_reached")!=one.predicates.end() &&
            std::find(one.predicates.begin(),one.predicates.end(),"global_proposal_budget")!=one.predicates.end(),"simultaneous_target_and_budget");
        DSL_HIGH_SEARCH_INPUT launch_dominated=short_input;launch_dominated.model.values[VHO_MLA_LAUNCH]=1000000000;
        control.stopping.push_back("relative-gap");control.gap_ppm=1000000;
        control.stopping.push_back("cost-aware");control.remaining_invocations=1;control.economic_conversion=true;
        control.host_us=1000000000;control.cycles_per_us=1000000000;control.evaluation_cost=UINT64(1000000000000000000ULL);control.conversion_provenance="synthetic";
        VHO_test_clock();one=VHO_test_search(launch_dominated,control);
        VHO_test_require(std::find(one.predicates.begin(),one.predicates.end(),"estimator_gap_certificate")!=one.predicates.end(),"estimator_relative_gap");
        VHO_test_require(std::find(one.predicates.begin(),one.predicates.end(),"estimator_benefit_bound")!=one.predicates.end(),"converted_cost_benefit_bound");
        VHO_test_require(one.predicates.size()>=4 && !one.exploration.complete,"all_composite_predicates_retained");

        control=VHO_MLA_SEARCH_CONTROL();control.global_time_us=50;
        VHO_test_clock(100,200,200);one=VHO_test_search(short_input,control);
        VHO_test_require(one.proposals==0 && !one.has_winner && one.termination=="global_time_budget","deadline_before_first_proposal");
        VHO_test_clock(100,100,200);one=VHO_test_search(short_input,control);
        VHO_test_require(one.proposals==1 && one.exploration.evaluations==1 && one.termination=="global_time_budget","deadline_after_atomic_estimate");
        VHO_test_clock(100,99,99);one.proposals=777;
        VHO_test_require(!VHO_mla_explore_controlled(VHO_test_program(short_input),short_input.gpu,short_input.model,control,&one,NULL) && one.proposals==777,"clock_rollback_preserves_output");

        control=VHO_MLA_SEARCH_CONTROL();std::string key=VHO_mla_search_control_key(control);control.global_proposals=2;
        VHO_test_require(key!=VHO_mla_search_control_key(control),"global_budget_is_in_control_identity");
        control=VHO_MLA_SEARCH_CONTROL();control.runs[1].id=control.runs[0].id;
        VHO_test_require(!VHO_mla_search_control_valid(control,NULL),"duplicate_run_id_refused");
        control=VHO_MLA_SEARCH_CONTROL();control.runs[0].strategy="bayesian";
        VHO_test_require(!VHO_mla_search_control_valid(control,NULL),"unsupported_provider_refused");
        control=VHO_MLA_SEARCH_CONTROL();control.stopping.push_back("probabilistic-confidence");
        VHO_test_require(!VHO_mla_search_control_valid(control,NULL),"uncalibrated_confidence_refused");
        control=VHO_MLA_SEARCH_CONTROL();control.stopping.push_back("cost-aware");control.remaining_invocations=1;control.evaluation_cost=1;
        VHO_test_require(!VHO_mla_search_control_valid(control,NULL),"missing_economic_conversion_refused");
        control.economic_conversion=true;control.host_us=UINT64(1000000000000ULL);control.cycles_per_us=1000000000;control.conversion_provenance="synthetic";
        VHO_test_require(!VHO_mla_search_control_valid(control,NULL),"economic_conversion_overflow_refused");
        control=VHO_MLA_SEARCH_CONTROL();control.global_unique=0;
        VHO_test_require(!VHO_mla_search_control_valid(control,NULL),"zero_unique_budget_refused");
        control=VHO_MLA_SEARCH_CONTROL();control.global_proposals=0;
        VHO_test_require(!VHO_mla_search_control_valid(control,NULL),"zero_proposal_budget_refused");
        printf("controlled_search_checks=%u PASS\n",VHO_test_checks);return 0;
    } catch(const std::exception &error) {fprintf(stderr,"control test: %s\n",error.what());return 1;}
}
