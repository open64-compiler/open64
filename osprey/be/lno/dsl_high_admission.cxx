/* Copyright (C) 2026 Open64 Project */
/* LNO-owned admission at the first successful full dependence-graph boundary.
 * Design: doc/AI-COMPILER-SEARCH-M2A-HIGH-ADMISSION-v1.0.md.
 * Reuses canonical loop/access/DU/alias/graph services without constructing IR,
 * changing analysis maps or selecting a transformation. */
#include "dsl_high_admission.h"
#include "pu_info.h"
#include "wn_util.h"
#include "wn_pragmas.h"
#include "lnopt_main.h"
#include "access_vector.h"
#include "dep_graph.h"
#include "opt_du.h"
#include "opt_alias_interface.h"

namespace {
struct LNO_DSL_HIGH_CHECK {
    ARRAY_DIRECTED_GRAPH16 *graph;
    UINT32 loops, memory;
    const char *reason;
    /* Borrow existing analysis for this synchronous read-only walk only. */
    LNO_DSL_HIGH_CHECK(ARRAY_DIRECTED_GRAPH16 *g) : graph(g), loops(0), memory(0), reason("admitted") {}
    /* Record the first precise refusal without changing the compiler state. */
    BOOL Refuse(const char *why) { reason = why; return FALSE; }
    /* Query canonical bounds; do not call Trip_Count or a mutating normalizer. */
    BOOL Loop(WN *loop) {
        OPCODE comparison; BOOL increasing;
        WN *lower = WN_LOOP_LowerBound(loop), *upper = WN_LOOP_UpperBound(loop, &comparison);
        WN *step = WN_LOOP_Increment(loop, &increasing);
        if (!lower || !upper || !step) return Refuse("noncanonical_loop");
        TYPE_ID type = WN_desc(WN_start(loop));
        if ((type != MTYPE_I4 && type != MTYPE_I8) || OPCODE_desc(comparison) != type)
            return Refuse("unsigned_or_noncanonical_iv");
        if (!lower || !upper || !step || WN_operator(lower) != OPR_INTCONST ||
            WN_operator(upper) != OPR_INTCONST || WN_operator(step) != OPR_INTCONST ||
            WN_const_val(lower) != 0 || !increasing || WN_const_val(step) != 1)
            return Refuse("nonconstant_or_nonunit_loop");
        INT64 extent = WN_const_val(upper);
        if (OPCODE_operator(comparison) == OPR_LE) {
            if (extent < 0 || extent >= 16777216) return Refuse("loop_extent");
            ++extent;
        } else if (OPCODE_operator(comparison) != OPR_LT) return Refuse("loop_comparison");
        if (extent <= 0 || extent > 16777216) return Refuse("loop_extent");
        DO_LOOP_INFO *info = (DO_LOOP_INFO *)WN_MAP_Get(LNO_Info_Map, loop);
        if (!info || !Do_Loop_Is_Good(loop) || info->Has_Bad_Mem || info->Has_Unsummarized_Calls ||
            info->Has_Exits || info->Has_Gotos || info->Has_Barriers)
            return Refuse("loop_memory_or_effect_unproved");
        ++loops; return TRUE;
    }
    /* Every indirect memory operation must retain an affine ARRAY and a full
     * graph vertex. Graph build success alone cannot establish coverage. */
    BOOL Memory(WN *node) {
        WN *address = WN_operator(node) == OPR_ILOAD ? WN_kid0(node) : WN_kid1(node);
        if (WN_offset(node) || WN_field_id(node) || WN_operator(address) != OPR_ARRAY ||
            TY_is_volatile(WN_ty(node))) return Refuse("opaque_or_volatile_memory");
        ACCESS_ARRAY *access = (ACCESS_ARRAY *)WN_MAP_Get(LNO_Info_Map, address);
        if (!access || access->Too_Messy) return Refuse("nonaffine_memory");
        for (INT i = 0; i < access->Num_Vec(); ++i)
            if (access->Dim(i)->Too_Messy) return Refuse("nonaffine_memory");
        if (!graph->Get_Vertex(node)) return Refuse("memory_not_in_full_graph");
        ++memory; return TRUE;
    }
    /* Walk structured High statements and pure expressions. Branch/call or
     * aggregate/atomic memory forms are refused, never treated as independent. */
    BOOL Walk(WN *node) {
        OPERATOR op = WN_operator(node);
        if (op == OPR_BLOCK) {
            for (WN *s = WN_first(node); s; s = WN_next(s)) if (!Walk(s)) return FALSE;
            return TRUE;
        }
        if (op == OPR_COMMENT || op == OPR_RETURN) return TRUE;
        if (op == OPR_PRAGMA && WN_pragma(node) == WN_PRAGMA_PREAMBLE_END) return TRUE;
        if (op == OPR_DO_LOOP && !Loop(node)) return FALSE;
        if ((op == OPR_ILOAD || op == OPR_ISTORE) && !Memory(node)) return FALSE;
        if (op == OPR_INTRINSIC_OP && WN_intrinsic(node) != INTRN_F4EXP && WN_intrinsic(node) != INTRN_F4LOG)
            return Refuse("unsupported_intrinsic");
        if ((op == OPR_LDID || op == OPR_STID) && TY_is_volatile(WN_ty(node)))
            return Refuse("volatile_scalar");
        if ((op == OPR_LDID || op == OPR_STID) && ST_class(WN_st(node)) != CLASS_PREG &&
            ST_level(WN_st(node)) != CURRENT_SYMTAB)
            return Refuse("unrepresented_global_memory");
        if (!OPCODE_is_expression(WN_opcode(node)) && op != OPR_DO_LOOP && op != OPR_IF &&
            op != OPR_STID && op != OPR_ISTORE && op != OPR_IDNAME)
            return Refuse("unsupported_control_or_effect");
        if (op == OPR_MLOAD || op == OPR_ILOADX || op == OPR_COMMA || op == OPR_RCOMMA)
            return Refuse("opaque_memory_or_nested_effect");
        for (INT i = 0; i < WN_kid_count(node); ++i)
            if (WN_kid(node, i) && !Walk(WN_kid(node, i))) return FALSE;
        return TRUE;
    }
};
}

/* Inspect only the active function while all analyses are live. The trace is
 * a form/coverage result, not a durable snapshot or scheduling certificate. */
BOOL LNO_DSL_admit_common_high(WN *scope, DU_MANAGER *du, ALIAS_MANAGER *alias,
    ARRAY_DIRECTED_GRAPH16 *graph, FILE *trace) {
    LNO_DSL_HIGH_CHECK check(graph);
    BOOL admitted = scope && WN_operator(scope) == OPR_FUNC_ENTRY && Current_PU_Info &&
        WN_st_idx(scope) == PU_Info_proc_sym(Current_PU_Info) &&
        du && Du_Built(du) && alias && graph;
    if (!admitted) check.reason = "scope_or_analysis_unavailable";
    else {
        admitted = check.Walk(WN_func_body(scope));
        if (admitted && (!check.loops || !check.memory)) {
            admitted = FALSE; check.reason = "no_loop_memory_algorithm";
        }
    }
    if (trace) fprintf(trace,
        "AI_HIGH 1 ADMISSION %s %s %s loops=%u memory=%u snapshot=false schedule_legality=not_proved\n",
        Current_PU_Info ? ST_name(PU_Info_proc_sym(Current_PU_Info)) : "none",
        admitted ? "admitted" : "refused", check.reason, check.loops, check.memory);
    return admitted;
}
