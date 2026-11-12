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
#include <stddef.h>
#include <cstdlib>
#include <sstream>
#include <time.h>

namespace {
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
        if (ST_sclass(formal) != SCLASS_FORMAL || !ST_is_value_parm(formal) || TY_kind(pointer) != KIND_POINTER ||
            !TY_is_restrict(pointer) || TY_mtype(TY_pointed(pointer)) != (value.dtype == "bool" ? MTYPE_U1 : MTYPE_F4))
            return reason = "formal_abi_mismatch", false;
    }
    reason = "native_binding_validated"; return true;
}
/* Retain the scoped estimated census, not an executable selected-plan handoff. */
void DSL_observer_print(FILE *trace, const DSL_HIGH_SEARCH_REPORT &report) {
    const VHO_MLA_SEARCH_RESULT &result = report.exploration;
    fprintf(trace,"AI_HIGH_SEARCH 1 BINDINGS manifest=%s\nAI_HIGH_SEARCH 1 RESOURCE_KEY %s\nAI_HIGH_SEARCH 1 MODEL_KEY %s\n",
        report.manifest_key.c_str(),report.resource_key.c_str(),report.model_key.c_str());
    for (unsigned i = 0; i < result.candidates.size(); ++i) {
        const VHO_MLA_SEARCH_CANDIDATE &candidate = result.candidates[i];
        fprintf(trace,"AI_HIGH_SEARCH 1 CANDIDATE ordinal=%u policy=%u width=%u evaluated=%s eligible=%s cycles=%llu reason=%s\n",
            i,candidate.policy,candidate.width,candidate.evaluated?"true":"false",candidate.eligible?"true":"false",
            (unsigned long long)candidate.total,candidate.reason.c_str());
    }
    fprintf(trace,"AI_HIGH_SEARCH 1 OBSERVED estimates=%llu cache_hits=%llu runs=%lu complete=%s winner=%u conclusion=%s applied=false schedule_legality=not_proved\n",
        (unsigned long long)result.evaluations,(unsigned long long)result.cache_hits,(unsigned long)result.runs.size(),
        result.complete?"true":"false",result.winner,result.complete?"best_estimated_in_declared_space":"best_estimated_among_explored");
}
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
    uint64_t snapshot_us, FILE *trace) {
    const char *path = getenv("OPEN64_HIGH_SEARCH_BINDINGS");
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
    bool valid = LNO_DSL_read_search_input(path,input,reason) &&
        LNO_DSL_validate_search_input(snapshot,bindings,input,reason) && DSL_observer_native_binding(scope,input,reason);
    uint64_t validation_us = LNO_DSL_observer_time_us()-begin, exploration_us = 0;
    bool observed = false;
    if (valid) {
        begin = LNO_DSL_observer_time_us(); observed = LNO_DSL_observe_search(input,report,reason);
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
    if (trace) {
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
        if (observed) DSL_observer_print(trace,report);
        else fprintf(trace,"AI_HIGH_SEARCH 1 SKIP reason=%s fallback=ordinary_lno\n",reason.c_str());
    }
}
