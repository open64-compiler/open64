/* Copyright (C) 2026 Open64 Project */

/* Linked contract and before/after WHIRL producer for explicit accumulation.
 * Scalar host execution proves structure/semantics, not Hopper FP8 accuracy.
 * Design: doc/AI-COMPILER-NUMERICAL-ACCUMULATION-DESIGN-v0.1.md.
 * MMA1 typed inventory boundary: doc/AI-COMPILER-MMA-COMPUTE-REALIZATION-DESIGN-v0.1.md. */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <vector>
#if !defined(BUILD_OS_DARWIN)
#include <elf.h>
#endif
#include "defs.h"
#include "mempool.h"
#include "wn.h"
#include "wn_util.h"
#include "wn_tree_util.h"
#include "stab.h"
#include "symtab_utils.h"
#include "strtab.h"
#include "pu_info.h"
#include "glob.h"
#include "ir_reader.h"
#include "ir_bwrite.h"
#include "erglob.h"
#include "errors.h"
#include "err_host.tab"
#include "config.h"
#include "config_dsl.h"
#include "controls.h"
#include "config_targ_opt.h"
#include "dwarf_DST_mem.h"
#include "dsl_builder.h"
#include "dsl_accumulation_opt.h"
#include "dsl_memory_hierarchy.h"

BOOL Run_vsaopt = FALSE;
INT8 Debug_Level = 0;

/* Standalone producer has no interrupted compiler output to clean up. */
void Signal_Cleanup(INT sig) {}

/* Test diagnostics do not require platform-specific parameter formatting. */
const char *Host_Format_Parm(INT kind, MEM_PTR parm) { return ""; }

/* Use normal Open64 initialization and mapped-image infrastructure. */
static void
Initialize_Context()
{
    MEM_Initialize();
    Set_Error_Tables(Phases, host_errlist);
    Init_Error_Handler(10);
    Set_Error_File(NULL);
    Set_Error_Line(ERROR_LINE_UNKNOWN);
    Preconfigure();
    Init_Controls_Tbl();
    ABI_Name = "n64";
    Configure();
    Opt_Level = 3;
    IR_reader_init();
    Initialize_Symbol_Tables(TRUE);
    DST_Init(NULL, 0);
}

/* Test-owned symbols carry the same source mapping as their definitions. */
static ST *
New_Symbol(const char *name, TY_IDX ty, ST_SCLASS storage, SRCPOS position)
{
    ST *st = New_ST(CURRENT_SYMTAB);
    ST_Init(st, Save_Str(name), CLASS_VAR, storage, EXPORT_LOCAL, ty);
    Set_ST_Srcpos(*st, position);
    if (storage == SCLASS_FORMAL)
        Set_ST_is_value_parm(st);
    return st;
}

/* Publish only a complete independently reopenable binary image. */
static BOOL
Write_Image(PU_Info *pu, const char *path)
{
    char temporary[4096];
    if (snprintf(temporary, sizeof(temporary), "%s.tmp", path) >= sizeof(temporary) ||
        access(path, F_OK) == 0)
        return FALSE;
    Irb_File_Name = temporary;
    if (Open_Output_Info(Irb_File_Name) == NULL)
        return FALSE;
    Write_PU_Info(pu);
    Write_Global_Info(pu);
    Close_Output_Info();
    return rename(temporary, path) == 0;
}

/* Construct a source-precision baseline in the same PU and with the same
 * symbols as the segmented after image; no CUDA templates are used. */
static WN *
Baseline(const VHO_DSL_ACCUMULATION_BINDING &binding, UINT32 total)
{
    WN *block = WN_CreateBlock();
    WN_INSERT_BlockLast(block, WN_Stid(MTYPE_F4, 0, binding.outer,
        ST_type(binding.outer), WN_Floatconst(MTYPE_F4, 0.0)));
    WN *body = WN_CreateBlock();
    ST *inputs[] = {binding.a, binding.b};
    WN *values[2];
    for (UINT32 i = 0; i != 2; ++i)
        values[i] = WN_Iload(MTYPE_F4, 0, MTYPE_To_TY(MTYPE_F4),
            WN_Add(Pointer_Mtype,
                WN_Ldid(Pointer_Mtype, 0, ST_st_idx(inputs[i]), ST_type(inputs[i])),
                WN_Mpy(Pointer_Mtype, WN_Cvt(MTYPE_I4, Pointer_Mtype,
                    WN_Ldid(MTYPE_I4, 0, ST_st_idx(binding.index), ST_type(binding.index))),
                    WN_Intconst(Pointer_Mtype, 4))));
    WN_INSERT_BlockLast(body, WN_Stid(MTYPE_F4, 0, binding.outer,
        ST_type(binding.outer), WN_Add(MTYPE_F4,
            WN_Ldid(MTYPE_F4, 0, ST_st_idx(binding.outer), ST_type(binding.outer)),
            WN_Mpy(MTYPE_F4, values[0], values[1]))));
    WN_INSERT_BlockLast(block, WN_CreateDO(WN_CreateIdname(0, binding.index),
        WN_Stid(MTYPE_I4, 0, binding.index, ST_type(binding.index), WN_Intconst(MTYPE_I4, 0)),
        WN_LT(MTYPE_I4, WN_Ldid(MTYPE_I4, 0, ST_st_idx(binding.index), ST_type(binding.index)),
            WN_Intconst(MTYPE_I4, total)),
        WN_Stid(MTYPE_I4, 0, binding.index, ST_type(binding.index),
            WN_Add(MTYPE_I4, WN_Ldid(MTYPE_I4, 0, ST_st_idx(binding.index), ST_type(binding.index)),
                WN_Intconst(MTYPE_I4, 1))), body, NULL));
    WN_INSERT_BlockLast(block, WN_Istore(MTYPE_F4, 0, ST_type(binding.output),
        WN_Ldid(Pointer_Mtype, 0, ST_st_idx(binding.output), ST_type(binding.output)),
        WN_Ldid(MTYPE_F4, 0, ST_st_idx(binding.outer), ST_type(binding.outer))));
    WN_TREE_CONTAINER<PRE_ORDER> tree(block);
    for (WN_TREE_CONTAINER<PRE_ORDER>::iterator it = tree.begin(); it != tree.end(); ++it)
        if (OPCODE_has_next_prev(WN_opcode(it.Wn())))
            WN_Set_Linenum(it.Wn(), binding.source_position);
    return block;
}

/* Exercise contract corruption independently of the VHO plan generator. */
static BOOL
Malformed_Contracts(const DSL_NUMERICAL_CONTRACT_RECORD &contract,
    const DSL_ACCUMULATION_PLAN_RECORD &plan,
    const std::vector<DSL_ACCUMULATION_SEGMENT_RECORD> &segments)
{
    for (UINT32 i = 0; i != 14; ++i) {
        DSL_NUMERICAL_CONTRACT_RECORD c = contract;
        DSL_ACCUMULATION_PLAN_RECORD p = plan;
        std::vector<DSL_ACCUMULATION_SEGMENT_RECORD> s = segments;
        switch (i) {
        case 0: c.permissions = 0; break;
        case 1: c.evidence_id = 0; break;
        case 2: c.maximum_span = 1; break;
        case 3: p.contract_id++; break;
        case 4: p.effective_max_span++; break;
        case 5: p.segment_count++; break;
        case 6: s[0].begin_k++; break;
        case 7: s[0].valid_k = 0; break;
        case 8: s[0].issued_k = p.requested_span + 1; break;
        case 9: s[0].reserved = 1; break;
        case 10: p.internal_evidence_kind = 0; break;
        case 11: c.input_format = DSL_ACC_FORMAT_F8_E4M3; break;
        case 12: p.scale_group_k = 1; break;
        case 13: c.partial_format = DSL_ACC_FORMAT_F8_E4M3; break;
        }
        if (DSL_accumulation_verify(&c, &p, &s[0], s.size(), NULL))
            return FALSE;
    }
    return TRUE;
}

/* Produce one same-PU transformation and negative evidence. Return failure
 * before publishing the after image if any preflight or postcheck fails. */
int
main(int argc, char **argv)
{
    if (argc != 4)
        return 2;
    Initialize_Context();
    PU_Info *pu = DSL_Builder_Create_Minimal_PU("accumulation_case");
    DSL_BUILDER_PU_SOURCE_IDENTITY identity = {};
    identity.canonical_definition_name = "accumulation_case";
    identity.defining_module = "ai.accumulation.contract";
    identity.defining_file = __FILE__;
    identity.defining_line = 1;
    if (!DSL_Builder_Set_PU_Source_Identity(pu, &identity))
        return 3;
    UINT32 file = DSL_Builder_Register_Source_File(pu, __FILE__);
    USRCPOS position;
    USRCPOS_clear(position);
    USRCPOS_filenum(position) = file;
    USRCPOS_linenum(position) = __LINE__;
    VHO_DSL_ACCUMULATION_BINDING binding = {};
    binding.source_position = USRCPOS_srcpos(position);
    TY_IDX pointer = Make_Pointer_Type(MTYPE_To_TY(MTYPE_F4));
    binding.a = New_Symbol("a", pointer, SCLASS_FORMAL, binding.source_position);
    binding.b = New_Symbol("b", pointer, SCLASS_FORMAL, binding.source_position);
    binding.output = New_Symbol("output", pointer, SCLASS_FORMAL, binding.source_position);
    binding.index = New_Symbol("k", MTYPE_To_TY(MTYPE_I4), SCLASS_AUTO, binding.source_position);
    binding.partial = New_Symbol("partial", MTYPE_To_TY(MTYPE_F4), SCLASS_AUTO, binding.source_position);
    binding.outer = New_Symbol("outer", MTYPE_To_TY(MTYPE_F4), SCLASS_AUTO, binding.source_position);
    ST *function = &St_Table[PU_Info_proc_sym(pu)];
    Set_ST_device_symbol_kind(*function, DEVICE_SYMBOL_KERNEL);
    Set_ST_export(function, EXPORT_PREEMPTIBLE);
    TY_IDX function_ty = ST_type(function);
    Set_TY_has_prototype(function_ty);
    TYLIST_IDX list;
    Set_TYLIST_type(New_TYLIST(list), MTYPE_To_TY(MTYPE_V));
    Set_TY_tylist(Ty_Table[function_ty], list);
    for (UINT32 i = 0; i != 3; ++i)
        Set_TYLIST_type(New_TYLIST(list), pointer);
    Set_TYLIST_type(New_TYLIST(list), TY_IDX_ZERO);
    WN *entry = WN_CreateEntry(3, function, Baseline(binding, 257), NULL, NULL);
    ST *formals[] = {binding.a, binding.b, binding.output};
    for (UINT32 i = 0; i != 3; ++i)
        WN_kid(entry, i) = WN_CreateIdname(0, formals[i]);
    Set_PU_Info_tree_ptr(pu, entry);
    if (getenv("OPEN64_ACC_BEFORE_ONLY") != NULL)
        return Write_Image(pu, argv[1]) ? 0 : 4;
    DSL_NUMERICAL_CONTRACT_RECORD contract = {};
    contract.id = 1;
    contract.source_format = contract.input_format = contract.product_format =
        contract.partial_format = contract.outer_format = contract.output_format = DSL_ACC_FORMAT_F32;
    contract.permissions = DSL_ACC_PERMISSION_REASSOCIATE;
    contract.maximum_span = 128;
    contract.evidence_kind = DSL_ACC_EVIDENCE_ISA;
    contract.evidence_id = 1;
    DSL_ACCUMULATION_PLAN_RECORD request = {};
    request.id = request.contract_id = 1;
    request.owner_pu_st = PU_Info_proc_sym(pu);
    request.target_profile_id = DSL_TARGET_PROFILE_CPU_BASELINE;
    request.instruction_id = VHO_DSL_ACC_SCALAR_F32;
    request.instruction_k = 1;
    request.total_k = 257;
    request.staging_k = 64;
    request.requested_span = 128;
    // Inventory formats must never enable the F32-buffer materializer.
    for (UINT32 format = DSL_ACC_FORMAT_F16; format <= DSL_ACC_FORMAT_BF16; ++format) {
        DSL_NUMERICAL_CONTRACT_RECORD typed = contract;
        typed.source_format = typed.input_format = format;
        if (VHO_DSL_accumulation_create(typed, request, NULL) != NULL)
            return 26;
    }
    VHO_DSL_ACCUMULATION *analysis = VHO_DSL_accumulation_create(contract, request, stderr);
    for (UINT32 level = 0; level != 3; ++level) {
        Opt_Level = level;
        if (VHO_DSL_accumulation_create(contract, request, NULL) != NULL)
            return 18;
    }
    Opt_Level = 3;
    DSL_ACCUMULATION_PLAN_RECORD plan;
    if (!VHO_DSL_accumulation_records(analysis, &contract, &plan))
        return 5;
    std::vector<DSL_ACCUMULATION_SEGMENT_RECORD> segments(plan.segment_count);
    for (UINT32 i = 0; i != plan.segment_count; ++i)
        if (!VHO_DSL_accumulation_segment(analysis, i, &segments[i]))
            return 6;
    if (plan.segment_count != 3 || segments[2].valid_k != 1 ||
        !Malformed_Contracts(contract, plan, segments))
        return 7;
    WN *after = NULL;
    if (!VHO_DSL_accumulation_materialize_f32(analysis, binding, &after, stderr))
        return 8;
    WN *reset = WN_next(WN_first(after));
    WN *merge = WN_next(WN_next(reset));
    ST_IDX saved = WN_st_idx(merge);
    WN_st_idx(merge) = ST_st_idx(binding.partial);
    if (VHO_DSL_accumulation_verify_f32(analysis, binding, after, NULL))
        return 9;
    WN_st_idx(merge) = saved;
    WN *bad = WN_COPY_Tree(after);
    WN_DELETE_FromBlock(bad, WN_next(WN_first(bad)));
    if (VHO_DSL_accumulation_verify_f32(analysis, binding, bad, NULL))
        return 10;
    WN_DELETE_Tree(bad);
    bad = WN_COPY_Tree(after);
    WN *bad_loop = WN_next(WN_next(WN_first(bad)));
    ++WN_const_val(WN_kid1(WN_end(bad_loop)));
    if (VHO_DSL_accumulation_verify_f32(analysis, binding, bad, NULL))
        return 19;
    WN_DELETE_Tree(bad);
    bad = WN_COPY_Tree(after);
    bad_loop = WN_next(WN_next(WN_first(bad)));
    WN *bad_merge = WN_next(bad_loop);
    WN_EXTRACT_FromBlock(bad, bad_merge);
    WN_INSERT_BlockLast(WN_do_body(bad_loop), bad_merge);
    if (VHO_DSL_accumulation_verify_f32(analysis, binding, bad, NULL))
        return 20;
    WN_DELETE_Tree(bad);
    WN *preserved = after;
    VHO_DSL_ACCUMULATION_BINDING invalid_binding = binding;
    invalid_binding.outer = binding.partial;
    if (VHO_DSL_accumulation_materialize_f32(analysis, invalid_binding, &preserved, NULL) ||
        preserved != after)
        return 21;
    Opt_Level = 2;
    if (VHO_DSL_accumulation_materialize_f32(analysis, binding, &preserved, NULL) ||
        preserved != after)
        return 22;
    Opt_Level = 3;
    FILE *trace = fopen(argv[3], "w");
    if (trace == NULL)
        return 11;
    VHO_DSL_accumulation_print(trace, analysis);
    WN_func_body(entry) = after;
    if (!Write_Image(pu, argv[2]))
        return 12;
    fprintf(trace, "# AI_ACC 1 ACTION plan segments resets merges completion proof backend native\n");
    fprintf(trace, "AI_ACC 1 ACTION 1 3 3 3 synchronous verified scalar_f32 no\n");
    VHO_DSL_accumulation_destroy(analysis);
    const UINT32 lengths[] = {1, 31, 32, 33, 127, 128, 129, 257, 1025};
    const UINT32 intervals[] = {1, 32, 64, 127, 128};
    for (UINT32 i = 0; i != 9; ++i)
        for (UINT32 j = 0; j != 5; ++j) {
            request.total_k = lengths[i];
            request.requested_span = intervals[j];
            analysis = VHO_DSL_accumulation_create(contract, request, stderr);
            if (analysis == NULL || !VHO_DSL_accumulation_records(analysis, &contract, &plan) ||
                plan.segment_count != (lengths[i] + intervals[j] - 1) / intervals[j])
                return 16;
            VHO_DSL_accumulation_destroy(analysis);
        }
    request.total_k = 257;
    request.requested_span = 128;
    request.scale_group_k = 64;
    analysis = VHO_DSL_accumulation_create(contract, request, stderr);
    if (analysis == NULL || !VHO_DSL_accumulation_records(analysis, &contract, &plan) ||
        plan.segment_count != 5 || plan.effective_max_span != 64)
        return 17;
    VHO_DSL_accumulation_destroy(analysis);
    request.scale_group_k = 0;
    contract.input_format = DSL_ACC_FORMAT_F8_E4M3;
    contract.id = request.contract_id = 2;
    contract.permissions |= DSL_ACC_PERMISSION_NARROW;
    request.target_profile_id = DSL_TARGET_PROFILE_NVIDIA_HOPPER;
    request.instruction_id = VHO_DSL_ACC_HOPPER_WGMMA_FP8;
    request.instruction_k = 32;
    request.id = 2;
    analysis = VHO_DSL_accumulation_create(contract, request, stderr);
    if (analysis == NULL || !VHO_DSL_accumulation_records(analysis, &contract, &plan) ||
        plan.internal_precision_bits != 0)
        return 13;
    WN *unchanged = after;
    if (VHO_DSL_accumulation_materialize_f32(analysis, binding, &unchanged, NULL) || unchanged != after)
        return 14;
    VHO_DSL_accumulation_print(trace, analysis);
    fprintf(trace, "AI_ACC 1 REJECT 2 native_hopper_fp8_lowering_unavailable\n");
    VHO_DSL_accumulation_destroy(analysis);
    contract.source_format = DSL_ACC_FORMAT_F8_E4M3;
    contract.permissions = DSL_ACC_PERMISSION_REASSOCIATE;
    contract.maximum_span = 160;
    request.requested_span = 130;
    analysis = VHO_DSL_accumulation_create(contract, request, stderr);
    if (analysis == NULL || !VHO_DSL_accumulation_records(analysis, &contract, &plan) ||
        plan.effective_max_span != 128 || plan.segment_count != 3)
        return 23;
    VHO_DSL_accumulation_destroy(analysis);
    request.requested_span = 31;
    if (VHO_DSL_accumulation_create(contract, request, NULL) != NULL)
        return 24;
    request.requested_span = 128;
    request.internal_precision_bits = 14;
    if (VHO_DSL_accumulation_create(contract, request, NULL) != NULL)
        return 25;
    request.internal_precision_bits = 0;
    request.target_profile_id = DSL_TARGET_PROFILE_NVIDIA_BLACKWELL;
    request.instruction_id = VHO_DSL_ACC_BLACKWELL_SCALED_FP4;
    request.id = 3;
    if (VHO_DSL_accumulation_create(contract, request, NULL) != NULL)
        return 15;
    fprintf(trace, "AI_ACC 1 REJECT 3 blackwell_requires_independent_scaled_profile\n");
    fclose(trace);
    puts("accumulation: 14 malformed records and 4 altered WHIRL sequences rejected; Hopper/Blackwell fail closed");
    return 0;
}
