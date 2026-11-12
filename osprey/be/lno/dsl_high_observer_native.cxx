/* Copyright (C) 2026 Open64 Project */
/* Active-PU LNO observer boundary: read native lineage/ABI and verify physical
 * tree/table isolation around detached search. No native mutation on any path.
 * Design: doc/AI-COMPILER-SEARCH-M2C-OBSERVER-v1.0.md. */
#include "dsl_high_observer.h"
#include "pu_info.h"
#include "wn_util.h"
#include "config.h"
#include "config_opt.h"
#include "dep_graph.h"
#include "flags.h"
#include <stddef.h>
#include <cstdlib>
#include <sstream>
#include <time.h>

namespace {
struct DSL_HIGH_SEARCH_CONFIGURATION {
    bool specified;
    std::string mode, path;
    /* Unspecified preserves the legacy v1 environment adapter. */
    DSL_HIGH_SEARCH_CONFIGURATION() : specified(false) {}
};
/* Reuse the public option descriptor lookup; do not depend on flags.c's
 * private auxiliary layout or change the binary LNO_FLAGS configuration ABI. */
DSL_HIGH_SEARCH_CONFIGURATION DSL_observer_configuration() {
    DSL_HIGH_SEARCH_CONFIGURATION configuration;
    OPTION_GROUP *group=Get_Command_Line_Group(Common_Option_Groups,"LNO");
    if(!group) return configuration;
    for(OPTION_DESC *option=OGROUP_options(group);ODESC_kind(option)!=OVK_COUNT;++option) {
        if(!ODESC_name(option) || ODESC_kind(option)!=OVK_NAME || !ODESC_variable(option)) continue;
        const char *value=*(const char **)ODESC_variable(option);
        if(!strcmp(ODESC_name(option),"high_search") && value) {
            configuration.specified=true;configuration.mode=value;
        } else if(!strcmp(ODESC_name(option),"high_search_bindings") && value) configuration.path=value;
    }
    return configuration;
}
struct DSL_HIGH_NATIVE_STATE {
    std::string tree, tables;
    unsigned nodes, symbols, types;
    /* Counters describe live state, not a fabricated native-allocation counter. */
    DSL_HIGH_NATIVE_STATE() : nodes(0), symbols(0), types(0) {}
};
/* Copy bytes from already live rows/nodes without interpreting native padding.
 * The equality check occurs in the same PU/lifetime, not across mapped reopens. */
void DSL_observer_bytes(std::string &output, const void *data, size_t bytes) {
    output.append((const char *)data,bytes);
}
/* Include statement links, source position, all node fields and kid pointers.
 * This catches edits hidden by semantic normalization or snapshot-key hashing. */
void DSL_observer_tree(WN *node, DSL_HIGH_NATIVE_STATE &state) {
    void *start = NULL; INT32 size = WN_Size_and_StartAddress(node,&start);
    DSL_observer_bytes(state.tree,start,size); ++state.nodes;
    if (WN_operator(node) == OPR_BLOCK) {
        for (WN *statement = WN_first(node); statement; statement = WN_next(statement)) DSL_observer_tree(statement,state);
    } else for (INT i = 0; i < WN_kid_count(node); ++i)
        if (WN_kid(node,i)) DSL_observer_tree(WN_kid(node,i),state);
}
/* Cover existing global/local ST rows and all TY rows, plus row-count changes.
 * Detached-object symbol auditing separately rules out unattached creation. */
DSL_HIGH_NATIVE_STATE DSL_observer_state(WN *scope) {
    DSL_HIGH_NATIVE_STATE state; DSL_observer_tree(scope,state);
    for (unsigned level = GLOBAL_SYMTAB; level <= CURRENT_SYMTAB; ++level) {
        unsigned count = ST_Table_Size(level); state.symbols += count;
        DSL_observer_bytes(state.tables,&count,sizeof(count));
        for (unsigned i = 0; i < count; ++i) DSL_observer_bytes(state.tables,&(*Scope_tab[level].st_tab)[i],sizeof(ST));
    }
    state.types = TY_Table_Size(); DSL_observer_bytes(state.tables,&state.types,sizeof(state.types));
    for (unsigned i = 0; i < state.types; ++i) DSL_observer_bytes(state.tables,&Ty_Table[i],sizeof(TY));
    return state;
}
/* Metadata is an owned producer witness, not a model-name recognition rule. */
std::string DSL_observer_metadata(ST_IDX symbol, const char *name) {
    const char *value = ST_tensor_metadata(symbol,name); return value ? value : "";
}
/* Verify complete producer recipe equality and formal storage ABI. The full
 * snapshot source/fact join is checked separately; names alone grant nothing. */
bool DSL_observer_native_binding(WN *scope, const DSL_HIGH_SEARCH_INPUT &input, std::string &reason) {
    if (input.function != ST_name(WN_st(scope)) ||
        DSL_observer_metadata(WN_st_idx(scope),"tensor.high.schema") != "common-high-v1" ||
        DSL_observer_metadata(WN_st_idx(scope),"tensor.high.buffer_contract") != "disjoint-storage-formals" ||
        DSL_observer_metadata(WN_st_idx(scope),"tensor.high.recipe_key") != VHO_tensor_recipe_key(input.algorithm))
        return reason = "native_producer_manifest_mismatch", false;
    if ((size_t)WN_num_formals(scope) != input.buffers.size()) return reason = "formal_abi_mismatch", false;
    for (unsigned i = 0; i < input.buffers.size(); ++i) {
        const VHO_TENSOR_VALUE &value = input.algorithm.values[input.buffers[i]];
        ST *formal = WN_st(WN_formal(scope,i)); TY_IDX pointer = ST_type(formal);
        if(DSL_observer_metadata(ST_st_idx(formal),"tensor.cache.trait") != value.cache_trait)
            return reason = "formal_cache_trait_mismatch", false;
        if (ST_sclass(formal) != SCLASS_FORMAL || !ST_is_value_parm(formal) || TY_kind(pointer) != KIND_POINTER ||
            !TY_is_restrict(pointer) || TY_mtype(TY_pointed(pointer)) != (value.dtype == "bool" ? MTYPE_U1 : MTYPE_F4))
            return reason = "formal_abi_mismatch", false;
    }
    reason = "native_binding_validated"; return true;
}
/* Retain the scoped estimated census, not an executable selected-plan handoff. */
void DSL_observer_print(FILE *trace, const DSL_HIGH_SEARCH_REPORT &report, const DSL_HIGH_SEARCH_INPUT &input) {
    const VHO_MLA_SEARCH_RESULT &result = report.exploration;
    fprintf(trace,"AI_HIGH_SEARCH 1 BINDINGS manifest=%s\nAI_HIGH_SEARCH 1 RESOURCE_KEY %s\nAI_HIGH_SEARCH 1 MODEL_KEY %s\n",
        report.manifest_key.c_str(),report.resource_key.c_str(),report.model_key.c_str());
    for (unsigned i = 0; i < result.candidates.size(); ++i) {
        const VHO_MLA_SEARCH_CANDIDATE &candidate = result.candidates[i];
        fprintf(trace,"AI_HIGH_SEARCH 1 CANDIDATE ordinal=%u policy=%u width=%u evaluated=%s eligible=%s cycles=%llu reason=%s\n",
            i,candidate.policy,candidate.width,candidate.evaluated?"true":"false",candidate.eligible?"true":"false",
            (unsigned long long)candidate.total,candidate.reason.c_str());
    }
    if(report.movement) {
        fprintf(trace,"AI_HIGH_MOVEMENT 1 PROFILE %s\nAI_HIGH_MOVEMENT 1 CATALOG %s capability=%s\n",
            VHO_mla_movement_profile_key(input.movement_profile).c_str(),VHO_mla_movement_catalog(),VHO_mla_movement_capabilities());
        fprintf(trace,"AI_HIGH_MOVEMENT 1 SCOPE modeled=true measured=false traffic_units=device_bytes latency_units=reference_cycles overlap=none L1=ignored stores=write-through spills=not-admitted actual_spills=unknown certificates=disabled\n");
        for(unsigned i=0;i<result.candidates.size();++i) {
            const VHO_MLA_SEARCH_CANDIDATE &c=result.candidates[i];
            fprintf(trace,"AI_HIGH_MOVEMENT 1 CANDIDATE ordinal=%u G=%u W=%u column=%u sweeps=%u modeled=%s eligible=%s cycles=%llu cache_payload=%llu cache_requests=%llu partial_allocation=%llu partial_requests=%llu score_storage=%llu head_slots=%llu reason=%s\n",
                i,c.descriptor.group,c.width,c.descriptor.column,c.descriptor.retained?1:2,c.movement.modeled?"true":"false",c.eligible?"true":"false",
                (unsigned long long)c.total,(unsigned long long)c.movement.cache_payload,(unsigned long long)c.movement.cache_requests,
                (unsigned long long)c.movement.partial_allocation,(unsigned long long)c.movement.partial_requests,
                (unsigned long long)c.movement.score_storage,(unsigned long long)c.movement.head_slots,c.reason.c_str());
            for(size_t q=0;q<c.movement.scenario_cycles.size();++q)
                fprintf(trace,"AI_HIGH_MOVEMENT 1 SCENARIO candidate=%u id=%s cycles=%llu\n",i,input.movement_profile.scenarios[q].id.c_str(),(unsigned long long)c.movement.scenario_cycles[q]);
            for(size_t j=0;j<c.stages.size();++j) {
                const VHO_MLA_STAGE_ESTIMATE &st=c.stages[j];
                fprintf(trace,"AI_HIGH_MOVEMENT 1 STAGE candidate=%u index=%lu op=%llu tokens=%llu heads=%llu ctas=%llu threads=%llu shared=%llu rounded_shared=%llu shared_transfer_per_cta=%llu registers=%llu rounded_registers=%llu resident=%llu unused=%llu rounds=%llu compute=%llu memory=%llu special=%llu local=%llu sync=%llu launch=%llu total=%llu\n",
                    i,(unsigned long)j,(unsigned long long)st.operation,(unsigned long long)st.movement.valid_tokens,
                    (unsigned long long)st.movement.valid_heads,(unsigned long long)st.ctas,(unsigned long long)st.threads,
                    (unsigned long long)st.shared,(unsigned long long)st.movement.rounded_shared,(unsigned long long)st.shared_bytes,(unsigned long long)st.movement.registers,
                    (unsigned long long)st.movement.rounded_registers,(unsigned long long)st.resident,(unsigned long long)st.movement.unused_slots,
                    (unsigned long long)st.rounds,(unsigned long long)st.compute,(unsigned long long)st.memory,(unsigned long long)st.special,
                    (unsigned long long)st.local,(unsigned long long)st.sync,(unsigned long long)st.launch,(unsigned long long)st.total);
                for(size_t k=0;k<st.movement.traffic.size();++k) {
                    const VHO_MLA_TRAFFIC_ESTIMATE &t=st.movement.traffic[k];
                    fprintf(trace,"AI_HIGH_MOVEMENT 1 TRAFFIC candidate=%u stage=%lu scenario=%s operand=%s direction=%s requested=%llu transactions=%llu L2=%llu HBM=%llu SMEM=%llu\n",
                        i,(unsigned long)j,t.scenario.c_str(),t.operand.c_str(),t.direction.c_str(),(unsigned long long)t.requested,
                        (unsigned long long)t.transactions,(unsigned long long)t.l2,(unsigned long long)t.hbm,(unsigned long long)t.shared);
                }
            }
        }
    }
    if(!report.controlled) fprintf(trace,"AI_HIGH_SEARCH 1 OBSERVED estimates=%llu cache_hits=%llu runs=%lu complete=%s winner=%u conclusion=%s applied=false schedule_legality=not_proved\n",
        (unsigned long long)result.evaluations,(unsigned long long)result.cache_hits,(unsigned long)result.runs.size(),
        result.complete?"true":"false",result.winner,result.complete?"best_estimated_in_declared_space":"best_estimated_among_explored");
    else {
        const VHO_MLA_PRODUCTION_RESULT &production=report.production;
        fprintf(trace,"AI_HIGH_SEARCH 2 CONTROL_KEY %s\n",report.control_key.c_str());
        for(size_t i=0;i<production.events.size();++i) {
            const VHO_MLA_SEARCH_EVENT &event=production.events[i];
            fprintf(trace,"AI_HIGH_SEARCH 2 EVENT proposal=%llu unique=%llu run=%u candidate=%u cache_hit=%s eligible=%s run_best=%llu global_best=%llu\n",
                (unsigned long long)event.proposal,(unsigned long long)event.unique,event.run,event.candidate,event.cache_hit?"true":"false",
                event.eligible?"true":"false",(unsigned long long)event.run_best,(unsigned long long)event.global_best);
        }
        for(size_t i=0;i<result.runs.size();++i) {
            const VHO_MLA_SEARCH_RUN &run=result.runs[i];
            std::ostringstream run_winner;if(run.best) run_winner<<run.winner;else run_winner<<"none";
            fprintf(trace,"AI_HIGH_SEARCH 2 RUN index=%lu id=%s strategy=%s proposals=%lu winner=%s best=%llu first_best_trial=%llu restarts=%llu termination=%s\n",
                (unsigned long)i,input.controls.runs[i].id.c_str(),run.strategy.c_str(),(unsigned long)run.visited.size(),
                run_winner.str().c_str(),(unsigned long long)run.best,(unsigned long long)run.first_best_trial,(unsigned long long)run.restarts,run.termination.c_str());
            for(size_t j=0;j<production.run_predicates[i].size();++j)
                fprintf(trace,"AI_HIGH_SEARCH 2 RUN_PREDICATE run=%lu name=%s\n",(unsigned long)i,production.run_predicates[i][j].c_str());
        }
        for(size_t i=0;i<production.predicates.size();++i) fprintf(trace,"AI_HIGH_SEARCH 2 PREDICATE %s\n",production.predicates[i].c_str());
        std::ostringstream winner;winner<<(production.has_winner?"":"none");if(production.has_winner) winner<<result.winner;
        const char *conclusion=production.has_winner?(result.complete?"best_estimated_in_declared_space":"best_estimated_among_explored"):
            (result.complete?"no_estimated_candidate_in_declared_space":"no_estimated_candidate_among_explored");
        fprintf(trace,"AI_HIGH_SEARCH 2 OBSERVED proposals=%llu estimates=%llu cache_hits=%llu elapsed_us=%llu runs=%lu complete=%s winner=%s termination=%s conclusion=%s applied=false schedule_legality=not_proved\n",
            (unsigned long long)production.proposals,(unsigned long long)result.evaluations,(unsigned long long)result.cache_hits,
            (unsigned long long)production.elapsed_us,(unsigned long)result.runs.size(),result.complete?"true":"false",winner.str().c_str(),
            production.termination.c_str(),conclusion);
        if(!production.has_winner) fprintf(trace,"AI_HIGH_SEARCH 2 FALLBACK reason=search_without_eligible_candidate fallback=ordinary_lno\n");
    }
}
}

#ifdef LNO_DSL_HIGH_REUSE_TEST
#include "tests/dsl_high_reuse_native_test.inc"
#endif

/* A production request uses its own file-scope opt-in, still gated by the
 * owning driver's effective O3/LNO/scope checks before any snapshot is made. */
bool LNO_DSL_production_search_requested() {
    DSL_HIGH_SEARCH_CONFIGURATION configuration=DSL_observer_configuration();
    return configuration.specified && configuration.mode!="off";
}

/* Keep host accounting separate from estimated reference-cycle model units. */
uint64_t LNO_DSL_observer_time_us() {
    struct timespec value;
    if (clock_gettime(CLOCK_MONOTONIC,&value)) return 0;
    return (uint64_t)value.tv_sec*1000000+(uint64_t)value.tv_nsec/1000;
}

/* The owner retains ordinary LNO on missing/refused input. Candidate execution
 * receives only owned data. An isolation invariant violation is a compiler
 * defect and aborts rather than continuing with potentially corrupted state. */
void LNO_DSL_observe_high(WN *scope, DU_MANAGER *du, ALIAS_MANAGER *alias, ARRAY_DIRECTED_GRAPH16 *graph,
    const DSL_HIGH_EPOCH &epoch, const DSL_HIGH_BINDINGS &bindings, const DSL_HIGH_SNAPSHOT &snapshot,
    uint64_t snapshot_us, FILE *trace, DSL_HIGH_SEARCH_INPUT *selected_input, DSL_HIGH_SEARCH_REPORT *selected_report) {
    if(selected_input) *selected_input=DSL_HIGH_SEARCH_INPUT();
    if(selected_report) *selected_report=DSL_HIGH_SEARCH_REPORT();
    DSL_HIGH_SEARCH_CONFIGURATION configuration=DSL_observer_configuration();
    if(configuration.specified && configuration.mode!="on") {
        if(trace) fprintf(trace,"AI_HIGH_SEARCH 2 SKIP reason=%s fallback=ordinary_lno\n",
            configuration.mode=="off"?"search_explicitly_disabled":"invalid_search_mode");
        return;
    }
    const char *path = configuration.specified?configuration.path.c_str():getenv("OPEN64_HIGH_SEARCH_BINDINGS");
    if (!path || !*path) {
        if (trace) fprintf(trace,"AI_HIGH_SEARCH 1 SKIP reason=no_binding_document fallback=ordinary_lno\n");
        return;
    }
    if (!snapshot.Is_current(epoch,bindings) || Roundoff_Level < ROUNDOFF_ASSOC) {
        if (trace) fprintf(trace,"AI_HIGH_SEARCH 1 SKIP reason=epoch_or_numerical_permission fallback=ordinary_lno\n");
        return;
    }
    uint64_t boundary_begin = LNO_DSL_observer_time_us();
    DSL_HIGH_NATIVE_STATE before = DSL_observer_state(scope);
    DSL_HIGH_SEARCH_INPUT input; DSL_HIGH_SEARCH_REPORT report; std::string reason;
    uint64_t begin = LNO_DSL_observer_time_us();
    bool valid = LNO_DSL_read_search_input(path,input,reason);
    if(valid && configuration.specified && !input.production_controls) {valid=false;reason="production_controls_required";}
    if(valid) valid=LNO_DSL_validate_search_input(snapshot,bindings,input,reason) && DSL_observer_native_binding(scope,input,reason);
    DSL_HIGH_REUSE_FACTS reuse;std::string reuse_reason="source_not_validated";
    if(valid) LNO_DSL_derive_cache_reuse(snapshot,epoch,bindings,input.algorithm,reuse,reuse_reason);
#ifdef LNO_DSL_HIGH_REUSE_TEST
    if(valid) {
        DSL_high_reuse_test(snapshot,epoch,bindings,input.algorithm,reuse,reuse_reason,trace);
        const char *fault=getenv("OPEN64_HIGH_REUSE_TEST");
        if(fault && !strcmp(fault,"decline")) {reuse=DSL_HIGH_REUSE_FACTS();reuse_reason="test_unproved_reuse";}
    }
#endif
    uint64_t validation_us = LNO_DSL_observer_time_us()-begin, exploration_us = 0;
    bool observed = false;
    if (valid) {
        begin = LNO_DSL_observer_time_us(); observed = LNO_DSL_observe_search(input,report,reason,&reuse);
        exploration_us = LNO_DSL_observer_time_us()-begin;
    }
    std::string current_reason;
    bool analyses_current = LNO_DSL_revalidate_high(scope,du,alias,graph,epoch,bindings,snapshot,current_reason);
    DSL_HIGH_NATIVE_STATE after = DSL_observer_state(scope);
    bool unchanged = before.tree == after.tree && before.tables == after.tables &&
        before.nodes == after.nodes && before.symbols == after.symbols && before.types == after.types &&
        analyses_current;
    uint64_t observer_us = LNO_DSL_observer_time_us()-boundary_begin;
    FmtAssert(unchanged,("read-only High observer changed native source/tables/analyses"));
    if(observed) {
        report.reuse=reuse;
        if(selected_input) *selected_input=input;
        if(selected_report) *selected_report=report;
    }
    if (trace) {
        if(reuse.Available()) reuse.Print(trace,"COLLECTED",reuse.Is_current(snapshot,epoch,bindings,input.algorithm));
        else if(valid) fprintf(trace,"AI_HIGH_REUSE 1 UNAVAILABLE reason=%s fallback=existing_G1_search\n",reuse_reason.c_str());
        fprintf(trace,"AI_HIGH_SEARCH 1 IMAGE_ABI dep_bytes=%lu must_offset=%lu bool_bytes=%lu vertex_bytes=%lu edge_bytes=%lu word_bytes=%lu pu_field_bytes=%lu\n",
            (unsigned long)sizeof(DEP_STRUCT),(unsigned long)offsetof(DEP_STRUCT,Is_Must),(unsigned long)sizeof(mBOOL),
            (unsigned long)sizeof(VINDEX16),(unsigned long)sizeof(EINDEX16),(unsigned long)sizeof(Elf64_Word),
            (unsigned long)(3*sizeof(mINT32)+sizeof(DST_IDX)));
        fprintf(trace,"AI_HIGH_SEARCH 1 ISOLATION tree_equal=true ST_equal=true TY_equal=true analyses_equal=true nodes=%u symbols=%u types=%u candidate_native_api=unavailable\n",
            after.nodes,after.symbols,after.types);
        fprintf(trace,"AI_HIGH_SEARCH 1 ACCOUNTING snapshot_us=%llu validation_us=%llu exploration_us=%llu estimates=%llu isolation_us=%llu observer_us=%llu\n",
            (unsigned long long)snapshot_us,(unsigned long long)validation_us,(unsigned long long)exploration_us,
            (unsigned long long)(observed?report.exploration.evaluations:0),
            (unsigned long long)(observer_us-validation_us-exploration_us),(unsigned long long)observer_us);
        if (observed) DSL_observer_print(trace,report,input);
        else fprintf(trace,"AI_HIGH_SEARCH 1 SKIP reason=%s fallback=ordinary_lno\n",reason.c_str());
    }
}
