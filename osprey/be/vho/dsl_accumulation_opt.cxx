/* Copyright (C) 2026 Open64 Project */

/* VHO-owned PU-local bounded partial-sum planning and scalar WHIRL adapter.
 * Hopper FP8 can be planned but not certified/lowered by this scalar adapter;
 * Blackwell requires a separate scaled profile. Binary WHIRL is unchanged.
 * Design: doc/AI-COMPILER-NUMERICAL-ACCUMULATION-DESIGN-v0.1.md. */
#include <limits.h>
#include <set>
#include <vector>
#include "dsl_accumulation_opt.h"
#include "dsl_memory_hierarchy.h"
#include "config.h"
#include "wn.h"
#include "wn_util.h"
#include "wn_tree_util.h"
#include "symtab.h"
#include "symtab_utils.h"
#include "pu_info.h"

struct VHO_DSL_ACCUMULATION {
    DSL_NUMERICAL_CONTRACT_RECORD contract;
    DSL_ACCUMULATION_PLAN_RECORD plan;
    std::vector<DSL_ACCUMULATION_SEGMENT_RECORD> segments;
};

/* Report policy/preflight failure without modifying caller-owned IR. */
static BOOL
VHO_DSL_Accumulation_Error(FILE *file, const char *reason)
{
    if (file != NULL)
        fprintf(file, "VHO accumulation: %s\n", reason);
    return FALSE;
}

/* Validate immutable records at each public boundary rather than trusting an
 * earlier phase's success or leaking stale PU state across lowering. */
static BOOL
VHO_DSL_Accumulation_Valid(const VHO_DSL_ACCUMULATION *analysis, FILE *file)
{
    return analysis != NULL && !analysis->segments.empty() &&
        DSL_accumulation_verify(&analysis->contract, &analysis->plan,
            &analysis->segments[0], analysis->segments.size(), file);
}

/* Build bounded segments without changing input records. Hopper evidence is
 * kept separate from scalar ISA facts; no Blackwell inference is permitted. */
VHO_DSL_ACCUMULATION *
VHO_DSL_accumulation_create(const DSL_NUMERICAL_CONTRACT_RECORD &contract,
    const DSL_ACCUMULATION_PLAN_RECORD &request, FILE *diagnostic)
{
    if (Opt_Level < 3 || Current_PU_Info == NULL ||
        request.owner_pu_st != PU_Info_proc_sym(Current_PU_Info) ||
        request.total_k == 0 || request.total_k >= INT_MAX ||
        request.requested_span == 0 || request.instruction_k == 0 ||
        request.internal_precision_bits || request.internal_evidence_kind ||
        request.internal_evidence_id ||
        request.segment_count || request.effective_max_span ||
        request.requested_span > contract.maximum_span) {
        VHO_DSL_Accumulation_Error(diagnostic, "invalid PU or reduction request");
        return NULL;
    }
    const BOOL scalar = request.target_profile_id == DSL_TARGET_PROFILE_CPU_BASELINE &&
        request.instruction_id == VHO_DSL_ACC_SCALAR_F32 && request.instruction_k == 1 &&
        contract.input_format == DSL_ACC_FORMAT_F32;
    const BOOL hopper = request.target_profile_id == DSL_TARGET_PROFILE_NVIDIA_HOPPER &&
        request.instruction_id == VHO_DSL_ACC_HOPPER_WGMMA_FP8 &&
        request.instruction_k == 32 &&
        (contract.input_format == DSL_ACC_FORMAT_F8_E4M3 ||
         contract.input_format == DSL_ACC_FORMAT_F8_E5M2);
    if ((!scalar && !hopper) || contract.product_format != DSL_ACC_FORMAT_F32 ||
        (request.scale_group_k && request.scale_group_k % request.instruction_k)) {
        VHO_DSL_Accumulation_Error(diagnostic, "unsupported instruction/scale profile; Blackwell is separate");
        return NULL;
    }
    UINT32 span = request.requested_span / request.instruction_k * request.instruction_k;
    if (span == 0 || (request.scale_group_k != 0 && span > request.scale_group_k))
        span = request.scale_group_k;
    if (span == 0 || span > request.requested_span) {
        VHO_DSL_Accumulation_Error(diagnostic, "interval smaller than an instruction");
        return NULL;
    }
    VHO_DSL_ACCUMULATION *analysis = new VHO_DSL_ACCUMULATION;
    analysis->contract = contract;
    analysis->plan = request;
    analysis->plan.internal_precision_bits = scalar ? 24 : 0;
    analysis->plan.internal_evidence_kind = DSL_ACC_EVIDENCE_ISA;
    analysis->plan.internal_evidence_id = scalar ? 1 : 2;
    analysis->plan.effective_max_span = 0;
    UINT32 begin = 0;
    while (begin < request.total_k && analysis->segments.size() < 4096) {
        UINT32 valid = request.total_k - begin;
        if (valid > span)
            valid = span;
        if (request.scale_group_k) {
            UINT32 remaining = request.scale_group_k - begin % request.scale_group_k;
            if (valid > remaining)
                valid = remaining;
        }
        UINT64 issued = ((UINT64)valid + request.instruction_k - 1) /
            request.instruction_k * request.instruction_k;
        if (issued > request.requested_span || (UINT64)begin + issued > INT_MAX)
            break;
        DSL_ACCUMULATION_SEGMENT_RECORD segment = {};
        segment.ordinal = analysis->segments.size();
        segment.begin_k = begin;
        segment.valid_k = valid;
        segment.issued_k = issued;
        analysis->segments.push_back(segment);
        if (analysis->plan.effective_max_span < issued)
            analysis->plan.effective_max_span = issued;
        begin += valid;
    }
    analysis->plan.segment_count = analysis->segments.size();
    if (begin != request.total_k || !VHO_DSL_Accumulation_Valid(analysis, diagnostic)) {
        delete analysis;
        return NULL;
    }
    return analysis;
}

/* Destroy only the runtime plan, never any tree or symbol supplied by VHO. */
void
VHO_DSL_accumulation_destroy(VHO_DSL_ACCUMULATION *analysis)
{
    delete analysis;
}

/* Return copied records only when both caller destinations are available. */
BOOL
VHO_DSL_accumulation_records(const VHO_DSL_ACCUMULATION *analysis,
    DSL_NUMERICAL_CONTRACT_RECORD *contract, DSL_ACCUMULATION_PLAN_RECORD *plan)
{
    if (contract == NULL || plan == NULL || !VHO_DSL_Accumulation_Valid(analysis, NULL))
        return FALSE;
    *contract = analysis->contract;
    *plan = analysis->plan;
    return TRUE;
}

/* Preserve caller output on malformed state or an out-of-range ordinal. */
BOOL
VHO_DSL_accumulation_segment(const VHO_DSL_ACCUMULATION *analysis,
    UINT32 ordinal, DSL_ACCUMULATION_SEGMENT_RECORD *segment)
{
    if (segment == NULL || !VHO_DSL_Accumulation_Valid(analysis, NULL) ||
        ordinal >= analysis->segments.size())
        return FALSE;
    *segment = analysis->segments[ordinal];
    return TRUE;
}

/* The caller controls trace enablement and FILE lifetime, using a distinct
 * schema so existing AI_OPT v7 readers are not given new field meanings. */
void
VHO_DSL_accumulation_print(FILE *file, const VHO_DSL_ACCUMULATION *analysis)
{
    if (VHO_DSL_Accumulation_Valid(analysis, NULL))
        DSL_accumulation_print(file, &analysis->contract, &analysis->plan,
            &analysis->segments[0], analysis->segments.size());
}

/* Admit only private active-scope scalars and F32 formal pointer operands.
 * Native FP8/FP4 must never pass through this synchronous scalar adapter. */
static BOOL
VHO_DSL_Accumulation_Binding(const VHO_DSL_ACCUMULATION *analysis,
    const VHO_DSL_ACCUMULATION_BINDING &binding, FILE *file)
{
    if (!VHO_DSL_Accumulation_Valid(analysis, file) || Opt_Level < 3 || Current_PU_Info == NULL ||
        analysis->plan.owner_pu_st != PU_Info_proc_sym(Current_PU_Info))
        return VHO_DSL_Accumulation_Error(file, "wrong compilation scope");
    if (analysis->plan.instruction_id != VHO_DSL_ACC_SCALAR_F32 ||
        analysis->contract.source_format != DSL_ACC_FORMAT_F32 ||
        analysis->contract.output_format != DSL_ACC_FORMAT_F32 || analysis->plan.scale_group_k)
        return VHO_DSL_Accumulation_Error(file, "native FP8/FP4 lowering unavailable; no scalar substitution");
    ST *symbols[] = {binding.a, binding.b, binding.output, binding.index, binding.partial, binding.outer};
    for (UINT32 i = 0; i != 6; ++i) {
        ST *st = symbols[i];
        if (st == NULL || ST_class(st) != CLASS_VAR || ST_level(st) != CURRENT_SYMTAB ||
            &St_Table[ST_st_idx(st)] != st)
            return VHO_DSL_Accumulation_Error(file, "invalid active-PU symbol");
        for (UINT32 j = 0; j != i; ++j)
            if (i >= 3 && j >= 3 && ST_st_idx(st) == ST_st_idx(symbols[j]))
                return VHO_DSL_Accumulation_Error(file, "aliased accumulator symbols");
        TY_IDX ty = ST_type(st);
        if (i < 3) {
            if (ST_sclass(st) != SCLASS_FORMAL || TY_kind(ty) != KIND_POINTER ||
                TY_mtype(TY_pointed(ty)) != MTYPE_F4)
                return VHO_DSL_Accumulation_Error(file, "expected F32 pointer formal");
        } else if (ST_sclass(st) != SCLASS_AUTO ||
                   TY_mtype(ty) != (i == 3 ? MTYPE_I4 : MTYPE_F4))
            return VHO_DSL_Accumulation_Error(file, "invalid private accumulator type");
    }
    return TRUE;
}

/* Produce fresh scalar loads; standard WHIRL constructors honor wn_simp. */
static WN *
VHO_DSL_Accumulation_Load(ST *st)
{
    return WN_Ldid(TY_mtype(ST_type(st)), 0, ST_st_idx(st), ST_type(st));
}

/* Form a typed F32 load using a fresh index tree for strict WN ownership. */
static WN *
VHO_DSL_Accumulation_Operand(ST *st, ST *index)
{
    WN *address = WN_Add(Pointer_Mtype, VHO_DSL_Accumulation_Load(st),
        WN_Mpy(Pointer_Mtype, WN_Cvt(MTYPE_I4, Pointer_Mtype,
            VHO_DSL_Accumulation_Load(index)), WN_Intconst(Pointer_Mtype, 4)));
    return WN_Iload(MTYPE_F4, 0, MTYPE_To_TY(MTYPE_F4), address);
}

/* Construct the selected explicit segments, reset and merge using standard
 * high-level loops. This first adapter is scalar and synchronously complete. */
static WN *
VHO_DSL_Accumulation_Body(const VHO_DSL_ACCUMULATION *analysis,
    const VHO_DSL_ACCUMULATION_BINDING &binding)
{
    WN *block = WN_CreateBlock();
    WN_INSERT_BlockLast(block, WN_Stid(MTYPE_F4, 0, binding.outer,
        ST_type(binding.outer), WN_Floatconst(MTYPE_F4, 0.0)));
    for (UINT32 i = 0; i != analysis->segments.size(); ++i) {
        const DSL_ACCUMULATION_SEGMENT_RECORD &segment = analysis->segments[i];
        WN_INSERT_BlockLast(block, WN_Stid(MTYPE_F4, 0, binding.partial,
            ST_type(binding.partial), WN_Floatconst(MTYPE_F4, 0.0)));
        WN *body = WN_CreateBlock();
        WN_INSERT_BlockLast(body, WN_Stid(MTYPE_F4, 0, binding.partial,
            ST_type(binding.partial), WN_Add(MTYPE_F4,
                VHO_DSL_Accumulation_Load(binding.partial),
                WN_Mpy(MTYPE_F4, VHO_DSL_Accumulation_Operand(binding.a, binding.index),
                    VHO_DSL_Accumulation_Operand(binding.b, binding.index)))));
        WN *loop = WN_CreateDO(WN_CreateIdname(0, binding.index),
            WN_Stid(MTYPE_I4, 0, binding.index, ST_type(binding.index),
                WN_Intconst(MTYPE_I4, segment.begin_k)),
            WN_LT(MTYPE_I4, VHO_DSL_Accumulation_Load(binding.index),
                WN_Intconst(MTYPE_I4, segment.begin_k + segment.valid_k)),
            WN_Stid(MTYPE_I4, 0, binding.index, ST_type(binding.index),
                WN_Add(MTYPE_I4, VHO_DSL_Accumulation_Load(binding.index),
                    WN_Intconst(MTYPE_I4, 1))), body, NULL);
        WN_INSERT_BlockLast(block, loop);
        WN_INSERT_BlockLast(block, WN_Stid(MTYPE_F4, 0, binding.outer,
            ST_type(binding.outer), WN_Add(MTYPE_F4,
                VHO_DSL_Accumulation_Load(binding.outer),
                VHO_DSL_Accumulation_Load(binding.partial))));
    }
    WN_INSERT_BlockLast(block, WN_Istore(MTYPE_F4, 0, ST_type(binding.output),
        VHO_DSL_Accumulation_Load(binding.output), VHO_DSL_Accumulation_Load(binding.outer)));
    WN_TREE_CONTAINER<PRE_ORDER> tree(block);
    for (WN_TREE_CONTAINER<PRE_ORDER>::iterator it = tree.begin(); it != tree.end(); ++it)
        if (OPCODE_has_next_prev(WN_opcode(it.Wn())))
            WN_Set_Linenum(it.Wn(), binding.source_position);
    return block;
}

/* Use the existing WN structural-equivalence traversal to verify the full
 * selected sequence, including types, addresses, loop bounds and ordering. */
BOOL
VHO_DSL_accumulation_verify_f32(const VHO_DSL_ACCUMULATION *analysis,
    const VHO_DSL_ACCUMULATION_BINDING &binding, WN *block, FILE *diagnostic)
{
    if (block == NULL || !VHO_DSL_Accumulation_Binding(analysis, binding, diagnostic))
        return FALSE;
    WN *expected = VHO_DSL_Accumulation_Body(analysis, binding);
    WN_TREE_CONTAINER<PRE_ORDER> actual_tree(block), expected_tree(expected);
    WN_TREE_CONTAINER<PRE_ORDER>::iterator a = actual_tree.begin();
    WN_TREE_CONTAINER<PRE_ORDER>::iterator b = expected_tree.begin();
    std::set<WN *> seen;
    while (a != actual_tree.end() && b != expected_tree.end() &&
           a.Depth() == b.Depth() && seen.insert(a.Wn()).second && WN_Equiv(a.Wn(), b.Wn())) {
        ++a;
        ++b;
    }
    BOOL valid = a == actual_tree.end() && b == expected_tree.end();
    WN_DELETE_Tree(expected);
    return valid || VHO_DSL_Accumulation_Error(diagnostic, "reset/compute/merge sequence mismatch");
}

/* Build and verify detached WHIRL before changing the caller's output slot.
 * The caller alone commits it to the active PU and emits the action trace. */
BOOL
VHO_DSL_accumulation_materialize_f32(const VHO_DSL_ACCUMULATION *analysis,
    const VHO_DSL_ACCUMULATION_BINDING &binding, WN **block, FILE *diagnostic)
{
    if (block == NULL || !VHO_DSL_Accumulation_Binding(analysis, binding, diagnostic))
        return FALSE;
    WN *candidate = VHO_DSL_Accumulation_Body(analysis, binding);
    if (!VHO_DSL_accumulation_verify_f32(analysis, binding, candidate, diagnostic)) {
        WN_DELETE_Tree(candidate);
        return FALSE;
    }
    *block = candidate;
    return TRUE;
}
