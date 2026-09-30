/*
 * Copyright (C) 2026 Open64 Project
 *
 * Purpose: implement FHE semantic runtime-handle resolution and detached,
 * checked standard-WHIRL call construction for the SYNC-5 ABI boundary. This
 * file selects FHE runtime roles and operations; it deliberately does not
 * mutate persistent DSL tables, rewrite native definitions, or alter mapped
 * WHIRL layout.
 *
 * Compilation scope: VHO FHE semantic runtime lowering, one active PU.
 * The standard blocks produced here are consumed by the reviewed generic
 * common/com native-value lowering transaction.
 *
 * Design references:
 *   doc/FHE-SYNC5-NATIVE-OWNERSHIP-AUDIT.md
 *   doc/FHE-SYNC5-STANDARD-WHIRL-LOWERING-CONTRACT.md
 *   doc/FHE-SYNC5-RUNTIME-ENTRY-BINDING-GAP.md
 *   doc/FHE-RUNTIME-C-ABI-V1-CONTRACT.md
 *   doc/FHE-CONSOLIDATED-IMPLEMENTATION-PLAN.md
 */

#include <string.h>

#include "fhe_semantic_runtime_lower.h"
#include "ir_reader.h"
#include "pu_info.h"
#include "symtab.h"
#include "wn.h"

enum {
    VHO_FHE_RUNTIME_OP_BOOTSTRAP = 3,
    VHO_FHE_RUNTIME_OP_RELU_NORMALIZE = 4,
    VHO_FHE_RUNTIME_OP_RELU_POLY_STAGE = 5,
    VHO_FHE_RUNTIME_OP_RELU_RECONSTRUCT = 6
};

static BOOL
VHO_FHE_Runtime_Semantic_Report (FILE *diagnostic, const char *message)
{
    if (diagnostic != NULL)
        fprintf(diagnostic, "CFHELOWER-SEM-001: %s\n", message);
    return FALSE;
}

static BOOL
VHO_FHE_Runtime_Active_PU
        (struct pu_info *pu_info, ST_IDX *owner_pu_st)
{
    if (pu_info == NULL || Current_PU_Info != pu_info ||
        PU_Info_tree_ptr(pu_info) == NULL)
        return FALSE;
    ST_IDX owner = PU_Info_proc_sym(pu_info);
    if (ST_IDX_level(owner) != GLOBAL_SYMTAB ||
        ST_IDX_index(owner) == 0 ||
        ST_IDX_index(owner) >= ST_Table_Size(GLOBAL_SYMTAB) ||
        ST_class(St_Table[owner]) != CLASS_FUNC)
        return FALSE;
    if (owner_pu_st != NULL)
        *owner_pu_st = owner;
    return TRUE;
}

static BOOL
VHO_FHE_Runtime_Handle_ST_Valid
        (ST_IDX handle_st, TY_IDX handle_ty)
{
    return ST_IDX_level(handle_st) == CURRENT_SYMTAB &&
           ST_IDX_index(handle_st) != 0 &&
           ST_IDX_index(handle_st) < ST_Table_Size(CURRENT_SYMTAB) &&
           handle_ty != TY_IDX_ZERO &&
           TY_IDX_index(handle_ty) < TY_Table_Size() &&
           ST_type(St_Table[handle_st]) == handle_ty &&
           TY_kind(handle_ty) == KIND_POINTER;
}

void
VHO_FHE_Runtime_Call_Sequence_Init
        (VHO_FHE_RUNTIME_CALL_SEQUENCE *sequence)
{
    if (sequence != NULL)
        memset(sequence, 0, sizeof(*sequence));
}

BOOL
VHO_FHE_Runtime_Resolve_Value_Handle
        (struct pu_info *pu_info, DSL_IR_VALUE_ID source_value_id,
         FILE *diagnostic, VHO_FHE_RUNTIME_HANDLE_BINDING *binding)
{
    if (binding != NULL)
        memset(binding, 0, sizeof(*binding));
    ST_IDX owner_pu_st;
    DSL_RUNTIME_VALUE_PROJECTION_RECORD projection;
    if (binding == NULL || source_value_id == DSL_IR_VALUE_INVALID_ID ||
        !VHO_FHE_Runtime_Active_PU(pu_info, &owner_pu_st) ||
        !DSL_Runtime_Interface_Image_Find_Value
            (owner_pu_st, source_value_id, &projection) ||
        projection.owner_pu_st != owner_pu_st ||
        projection.source_value_id != source_value_id ||
        !VHO_FHE_Runtime_Handle_ST_Valid
            (projection.handle_st, projection.handle_ty))
        return VHO_FHE_Runtime_Semantic_Report
                   (diagnostic, "projected value handle is unavailable");

    binding->owner_pu_st = owner_pu_st;
    binding->handle_st = projection.handle_st;
    binding->handle_ty = projection.handle_ty;
    binding->source_value_id = source_value_id;
    binding->binding_kind = projection.binding_kind;
    return TRUE;
}

BOOL
VHO_FHE_Runtime_Resolve_Role_Handle
        (struct pu_info *pu_info, const char *semantic_role,
         FILE *diagnostic, VHO_FHE_RUNTIME_HANDLE_BINDING *binding)
{
    if (binding != NULL)
        memset(binding, 0, sizeof(*binding));
    ST_IDX owner_pu_st;
    if (binding == NULL || semantic_role == NULL ||
        semantic_role[0] == '\0' ||
        !VHO_FHE_Runtime_Active_PU(pu_info, &owner_pu_st))
        return VHO_FHE_Runtime_Semantic_Report
                   (diagnostic, "runtime role request is invalid");

    UINT32 match_count = 0;
    DSL_RUNTIME_INPUT_BINDING_RECORD match;
    for (UINT32 id = 1;
         id <= DSL_Program_Interface_Image_Runtime_Binding_Count(); ++id) {
        DSL_RUNTIME_INPUT_BINDING_RECORD candidate;
        if (!DSL_Program_Interface_Image_Get_Runtime_Binding(id, &candidate))
            return VHO_FHE_Runtime_Semantic_Report
                       (diagnostic, "runtime binding table is malformed");
        if (candidate.owner_pu_st != owner_pu_st ||
            candidate.semantic_role == STR_IDX_ZERO ||
            strcmp(Index_To_Str(candidate.semantic_role), semantic_role) != 0)
            continue;
        match = candidate;
        ++match_count;
    }
    if (match_count != 1 ||
        !VHO_FHE_Runtime_Handle_ST_Valid(match.handle_st, match.handle_ty))
        return VHO_FHE_Runtime_Semantic_Report
                   (diagnostic, "runtime role is missing or ambiguous");

    DSL_RUNTIME_INPUT_RECORD input;
    if (match.runtime_input_id != DSL_RUNTIME_INPUT_INVALID_ID &&
        (!DSL_Program_Interface_Image_Get_Runtime_Input
             (match.runtime_input_id, &input) ||
         input.handle_ty != match.handle_ty))
        return VHO_FHE_Runtime_Semantic_Report
                   (diagnostic, "runtime role input does not match binding");

    binding->owner_pu_st = owner_pu_st;
    binding->handle_st = match.handle_st;
    binding->handle_ty = match.handle_ty;
    binding->runtime_input_id = match.runtime_input_id;
    binding->binding_kind = match.binding_kind;
    return TRUE;
}

static TY_IDX
VHO_FHE_Runtime_Opaque_Handle_TY (const char *name)
{
    for (UINT32 index = 1; index < TY_Table_Size(); ++index) {
        TY_IDX candidate = make_TY_IDX(index);
        if (TY_kind(candidate) != KIND_POINTER)
            continue;
        TY_IDX pointee = TY_pointed(candidate);
        if (pointee == TY_IDX_ZERO ||
            TY_IDX_index(pointee) >= TY_Table_Size() ||
            TY_kind(pointee) != KIND_STRUCT || TY_name(pointee) == NULL ||
            strcmp(TY_name(pointee), name) != 0)
            continue;
        return Make_Pointer_Type(pointee);
    }

    TY_IDX opaque_ty;
    TY &opaque = New_TY(opaque_ty);
    TY_Init(opaque, 0, KIND_STRUCT, MTYPE_M, Save_Str(name));
    Set_TY_align(opaque_ty, 1);
    return Make_Pointer_Type(opaque_ty);
}

static WN *
VHO_FHE_Runtime_Handle_ST_Load (ST_IDX st, TY_IDX ty)
{
    TYPE_ID mtype = TY_mtype(ty);
    return WN_CreateLdid(OPR_LDID, mtype, mtype, 0, st, ty);
}

static WN *
VHO_FHE_Runtime_Handle_Load
        (const VHO_FHE_RUNTIME_HANDLE_BINDING &binding)
{
    return VHO_FHE_Runtime_Handle_ST_Load
               (binding.handle_st, binding.handle_ty);
}

static BOOL
VHO_FHE_Runtime_Append_Call
        (WN *block, const VHO_FHE_STANDARD_CALL_SPEC *spec,
         FILE *diagnostic, VHO_FHE_STANDARD_CALL_RESULT *result)
{
    return VHO_FHE_Build_Standard_Call(spec, diagnostic, result) &&
           VHO_FHE_Commit_Standard_Call(block, NULL, result, diagnostic);
}

static BOOL
VHO_FHE_Runtime_Append_Selector
        (WN *block, const VHO_FHE_RUNTIME_HANDLE_BINDING &model,
         ST_IDX input_st, TY_IDX input_ty, UINT32 static_ordinal,
         UINT32 operation_kind, SRCPOS source_position,
         VHO_FHE_STANDARD_FAILURE_BUILDER build_failure,
         void *failure_context, FILE *diagnostic,
         VHO_FHE_STANDARD_CALL_RESULT *selected)
{
    TY_IDX status_ty = MTYPE_To_TY(MTYPE_I4);
    TY_IDX u4_ty = MTYPE_To_TY(MTYPE_U4);
    TY_IDX descriptor_ty = VHO_FHE_Runtime_Opaque_Handle_TY
                               ("open64_fhe_operation_desc_v1");
    TY_IDX descriptor_output_ty = Make_Pointer_Type(descriptor_ty);
    VHO_FHE_STANDARD_PARM parameters[5];
    memset(parameters, 0, sizeof(parameters));
    parameters[0].formal_ty = model.handle_ty;
    parameters[0].actual_ty = model.handle_ty;
    parameters[0].actual = VHO_FHE_Runtime_Handle_Load(model);
    parameters[0].policy = VHO_FHE_STANDARD_PARM_BORROWED_READ_ONLY;
    parameters[1].formal_ty = input_ty;
    parameters[1].actual_ty = input_ty;
    parameters[1].actual = VHO_FHE_Runtime_Handle_ST_Load(input_st, input_ty);
    parameters[1].policy = VHO_FHE_STANDARD_PARM_BORROWED_READ_ONLY;
    parameters[2].formal_ty = u4_ty;
    parameters[2].actual_ty = u4_ty;
    parameters[2].actual = WN_Intconst(MTYPE_U4, static_ordinal);
    parameters[2].policy = VHO_FHE_STANDARD_PARM_BY_VALUE;
    parameters[3].formal_ty = u4_ty;
    parameters[3].actual_ty = u4_ty;
    parameters[3].actual = WN_Intconst(MTYPE_U4, operation_kind);
    parameters[3].policy = VHO_FHE_STANDARD_PARM_BY_VALUE;
    parameters[4].formal_ty = descriptor_output_ty;
    parameters[4].policy = VHO_FHE_STANDARD_PARM_OUTPUT_SLOT;
    parameters[4].output_name = "fhe_selected_descriptor";
    parameters[4].output_ty = descriptor_ty;

    VHO_FHE_STANDARD_CALL_SPEC spec;
    memset(&spec, 0, sizeof(spec));
    spec.function_name = "open64_fhe_operation_desc_select_v1";
    spec.status_ty = status_ty;
    spec.parameters = parameters;
    spec.parameter_count = 5;
    spec.status_name = "fhe_descriptor_status";
    spec.source_position = source_position;
    spec.build_failure = build_failure;
    spec.failure_context = failure_context;
    BOOL valid = VHO_FHE_Runtime_Append_Call
                     (block, &spec, diagnostic, selected);
    for (UINT32 i = 0; i < 4; ++i)
        WN_DELETE_Tree(parameters[i].actual);
    return valid;
}

static BOOL
VHO_FHE_Runtime_Append_Unary_Evaluation
        (WN *block, const char *function_name,
         const VHO_FHE_RUNTIME_HANDLE_BINDING &model,
         ST_IDX input_st, TY_IDX input_ty, ST_IDX descriptor_st,
         TY_IDX descriptor_ty, SRCPOS source_position,
         VHO_FHE_STANDARD_FAILURE_BUILDER build_failure,
         void *failure_context, FILE *diagnostic,
         VHO_FHE_STANDARD_CALL_RESULT *evaluated)
{
    VHO_FHE_STANDARD_PARM parameters[4];
    memset(parameters, 0, sizeof(parameters));
    parameters[0].formal_ty = model.handle_ty;
    parameters[0].actual_ty = model.handle_ty;
    parameters[0].actual = VHO_FHE_Runtime_Handle_Load(model);
    parameters[0].policy = VHO_FHE_STANDARD_PARM_BORROWED_READ_ONLY;
    parameters[1].formal_ty = input_ty;
    parameters[1].actual_ty = input_ty;
    parameters[1].actual = VHO_FHE_Runtime_Handle_ST_Load(input_st, input_ty);
    parameters[1].policy = VHO_FHE_STANDARD_PARM_BORROWED_READ_ONLY;
    parameters[2].formal_ty = descriptor_ty;
    parameters[2].actual_ty = descriptor_ty;
    parameters[2].actual = VHO_FHE_Runtime_Handle_ST_Load
                               (descriptor_st, descriptor_ty);
    parameters[2].policy = VHO_FHE_STANDARD_PARM_BORROWED_READ_ONLY;
    parameters[3].formal_ty = Make_Pointer_Type(input_ty);
    parameters[3].policy = VHO_FHE_STANDARD_PARM_OUTPUT_SLOT;
    parameters[3].output_name = "fhe_runtime_result";
    parameters[3].output_ty = input_ty;

    VHO_FHE_STANDARD_CALL_SPEC spec;
    memset(&spec, 0, sizeof(spec));
    spec.function_name = function_name;
    spec.status_ty = MTYPE_To_TY(MTYPE_I4);
    spec.parameters = parameters;
    spec.parameter_count = 4;
    spec.status_name = "fhe_runtime_status";
    spec.source_position = source_position;
    spec.build_failure = build_failure;
    spec.failure_context = failure_context;
    BOOL valid = VHO_FHE_Runtime_Append_Call
                     (block, &spec, diagnostic, evaluated);
    for (UINT32 i = 0; i < 3; ++i)
        WN_DELETE_Tree(parameters[i].actual);
    return valid;
}

BOOL
VHO_FHE_Runtime_Build_Bootstrap_Sequence
        (struct pu_info *pu_info, DSL_IR_VALUE_ID anchor_value_id,
         UINT32 static_ordinal, SRCPOS source_position,
         VHO_FHE_STANDARD_FAILURE_BUILDER build_failure,
         void *failure_context, FILE *diagnostic,
         VHO_FHE_RUNTIME_CALL_SEQUENCE *sequence)
{
    VHO_FHE_Runtime_Call_Sequence_Init(sequence);
    if (sequence == NULL || static_ordinal == 0 || source_position == 0 ||
        build_failure == NULL)
        return VHO_FHE_Runtime_Semantic_Report
                   (diagnostic, "bootstrap call request is incomplete");

    VHO_FHE_RUNTIME_HANDLE_BINDING model;
    VHO_FHE_RUNTIME_HANDLE_BINDING anchor;
    if (!VHO_FHE_Runtime_Resolve_Role_Handle
             (pu_info, "fhe.model", diagnostic, &model) ||
        !VHO_FHE_Runtime_Resolve_Value_Handle
             (pu_info, anchor_value_id, diagnostic, &anchor))
        return FALSE;

    TY_IDX status_ty = MTYPE_To_TY(MTYPE_I4);
    TY_IDX u4_ty = MTYPE_To_TY(MTYPE_U4);
    TY_IDX descriptor_ty = VHO_FHE_Runtime_Opaque_Handle_TY
                               ("open64_fhe_operation_desc_v1");
    TY_IDX descriptor_output_ty = Make_Pointer_Type(descriptor_ty);
    TY_IDX ciphertext_output_ty = Make_Pointer_Type(anchor.handle_ty);
    WN *block = WN_CreateBlock();

    VHO_FHE_STANDARD_PARM select_parameters[5];
    memset(select_parameters, 0, sizeof(select_parameters));
    select_parameters[0].formal_ty = model.handle_ty;
    select_parameters[0].actual_ty = model.handle_ty;
    select_parameters[0].actual = VHO_FHE_Runtime_Handle_Load(model);
    select_parameters[0].policy = VHO_FHE_STANDARD_PARM_BORROWED_READ_ONLY;
    select_parameters[1].formal_ty = anchor.handle_ty;
    select_parameters[1].actual_ty = anchor.handle_ty;
    select_parameters[1].actual = VHO_FHE_Runtime_Handle_Load(anchor);
    select_parameters[1].policy = VHO_FHE_STANDARD_PARM_BORROWED_READ_ONLY;
    select_parameters[2].formal_ty = u4_ty;
    select_parameters[2].actual_ty = u4_ty;
    select_parameters[2].actual = WN_Intconst(MTYPE_U4, static_ordinal);
    select_parameters[2].policy = VHO_FHE_STANDARD_PARM_BY_VALUE;
    select_parameters[3].formal_ty = u4_ty;
    select_parameters[3].actual_ty = u4_ty;
    select_parameters[3].actual =
        WN_Intconst(MTYPE_U4, VHO_FHE_RUNTIME_OP_BOOTSTRAP);
    select_parameters[3].policy = VHO_FHE_STANDARD_PARM_BY_VALUE;
    select_parameters[4].formal_ty = descriptor_output_ty;
    select_parameters[4].policy = VHO_FHE_STANDARD_PARM_OUTPUT_SLOT;
    select_parameters[4].output_name = "fhe_selected_descriptor";
    select_parameters[4].output_ty = descriptor_ty;

    VHO_FHE_STANDARD_CALL_SPEC select_spec;
    memset(&select_spec, 0, sizeof(select_spec));
    select_spec.function_name = "open64_fhe_operation_desc_select_v1";
    select_spec.status_ty = status_ty;
    select_spec.parameters = select_parameters;
    select_spec.parameter_count = 5;
    select_spec.status_name = "fhe_descriptor_status";
    select_spec.source_position = source_position;
    select_spec.build_failure = build_failure;
    select_spec.failure_context = failure_context;

    VHO_FHE_STANDARD_CALL_RESULT selected;
    BOOL valid = VHO_FHE_Runtime_Append_Call
                     (block, &select_spec, diagnostic, &selected);
    for (UINT32 i = 0; i < 4; ++i)
        WN_DELETE_Tree(select_parameters[i].actual);
    if (!valid) {
        WN_DELETE_Tree(block);
        return FALSE;
    }

    VHO_FHE_STANDARD_PARM bootstrap_parameters[4];
    memset(bootstrap_parameters, 0, sizeof(bootstrap_parameters));
    bootstrap_parameters[0].formal_ty = model.handle_ty;
    bootstrap_parameters[0].actual_ty = model.handle_ty;
    bootstrap_parameters[0].actual = VHO_FHE_Runtime_Handle_Load(model);
    bootstrap_parameters[0].policy =
        VHO_FHE_STANDARD_PARM_BORROWED_READ_ONLY;
    bootstrap_parameters[1].formal_ty = anchor.handle_ty;
    bootstrap_parameters[1].actual_ty = anchor.handle_ty;
    bootstrap_parameters[1].actual = VHO_FHE_Runtime_Handle_Load(anchor);
    bootstrap_parameters[1].policy =
        VHO_FHE_STANDARD_PARM_BORROWED_READ_ONLY;
    bootstrap_parameters[2].formal_ty = descriptor_ty;
    bootstrap_parameters[2].actual_ty = descriptor_ty;
    bootstrap_parameters[2].actual = WN_CreateLdid
        (OPR_LDID, Pointer_Mtype, Pointer_Mtype, 0, selected.output_st,
         descriptor_ty);
    bootstrap_parameters[2].policy =
        VHO_FHE_STANDARD_PARM_BORROWED_READ_ONLY;
    bootstrap_parameters[3].formal_ty = ciphertext_output_ty;
    bootstrap_parameters[3].policy = VHO_FHE_STANDARD_PARM_OUTPUT_SLOT;
    bootstrap_parameters[3].output_name = "fhe_bootstrap_result";
    bootstrap_parameters[3].output_ty = anchor.handle_ty;

    VHO_FHE_STANDARD_CALL_SPEC bootstrap_spec;
    memset(&bootstrap_spec, 0, sizeof(bootstrap_spec));
    bootstrap_spec.function_name = "open64_fhe_bootstrap_v1";
    bootstrap_spec.status_ty = status_ty;
    bootstrap_spec.parameters = bootstrap_parameters;
    bootstrap_spec.parameter_count = 4;
    bootstrap_spec.status_name = "fhe_bootstrap_status";
    bootstrap_spec.source_position = source_position;
    bootstrap_spec.build_failure = build_failure;
    bootstrap_spec.failure_context = failure_context;

    VHO_FHE_STANDARD_CALL_RESULT bootstrapped;
    valid = VHO_FHE_Runtime_Append_Call
                (block, &bootstrap_spec, diagnostic, &bootstrapped);
    for (UINT32 i = 0; i < 3; ++i)
        WN_DELETE_Tree(bootstrap_parameters[i].actual);
    if (!valid) {
        WN_DELETE_Tree(block);
        return FALSE;
    }

    sequence->block = block;
    sequence->output_st = bootstrapped.output_st;
    sequence->standard_call_count = 2;
    sequence->output_handle_count = 2;
    sequence->status_check_count = 2;
    return TRUE;
}

static BOOL
VHO_FHE_Runtime_Append_Poly_Evaluation
        (WN *block, const VHO_FHE_RUNTIME_HANDLE_BINDING &model,
         ST_IDX input_st, TY_IDX input_ty,
         const VHO_FHE_RUNTIME_HANDLE_BINDING &coefficients,
         ST_IDX descriptor_st, TY_IDX descriptor_ty,
         SRCPOS source_position,
         VHO_FHE_STANDARD_FAILURE_BUILDER build_failure,
         void *failure_context, FILE *diagnostic,
         VHO_FHE_STANDARD_CALL_RESULT *evaluated)
{
    VHO_FHE_STANDARD_PARM parameters[5];
    memset(parameters, 0, sizeof(parameters));
    parameters[0].formal_ty = model.handle_ty;
    parameters[0].actual_ty = model.handle_ty;
    parameters[0].actual = VHO_FHE_Runtime_Handle_Load(model);
    parameters[0].policy = VHO_FHE_STANDARD_PARM_BORROWED_READ_ONLY;
    parameters[1].formal_ty = input_ty;
    parameters[1].actual_ty = input_ty;
    parameters[1].actual = VHO_FHE_Runtime_Handle_ST_Load(input_st, input_ty);
    parameters[1].policy = VHO_FHE_STANDARD_PARM_BORROWED_READ_ONLY;
    parameters[2].formal_ty = coefficients.handle_ty;
    parameters[2].actual_ty = coefficients.handle_ty;
    parameters[2].actual = VHO_FHE_Runtime_Handle_Load(coefficients);
    parameters[2].policy = VHO_FHE_STANDARD_PARM_BORROWED_READ_ONLY;
    parameters[3].formal_ty = descriptor_ty;
    parameters[3].actual_ty = descriptor_ty;
    parameters[3].actual = VHO_FHE_Runtime_Handle_ST_Load
                               (descriptor_st, descriptor_ty);
    parameters[3].policy = VHO_FHE_STANDARD_PARM_BORROWED_READ_ONLY;
    parameters[4].formal_ty = Make_Pointer_Type(input_ty);
    parameters[4].policy = VHO_FHE_STANDARD_PARM_OUTPUT_SLOT;
    parameters[4].output_name = "fhe_relu_stage_result";
    parameters[4].output_ty = input_ty;

    VHO_FHE_STANDARD_CALL_SPEC spec;
    memset(&spec, 0, sizeof(spec));
    spec.function_name = "open64_fhe_relu_poly_stage_v1";
    spec.status_ty = MTYPE_To_TY(MTYPE_I4);
    spec.parameters = parameters;
    spec.parameter_count = 5;
    spec.status_name = "fhe_relu_stage_status";
    spec.source_position = source_position;
    spec.build_failure = build_failure;
    spec.failure_context = failure_context;
    BOOL valid = VHO_FHE_Runtime_Append_Call
                     (block, &spec, diagnostic, evaluated);
    for (UINT32 i = 0; i < 4; ++i)
        WN_DELETE_Tree(parameters[i].actual);
    return valid;
}

static BOOL
VHO_FHE_Runtime_Append_Reconstruct_Evaluation
        (WN *block, const VHO_FHE_RUNTIME_HANDLE_BINDING &model,
         ST_IDX refreshed_st, ST_IDX stage_st, TY_IDX ciphertext_ty,
         ST_IDX descriptor_st, TY_IDX descriptor_ty,
         SRCPOS source_position,
         VHO_FHE_STANDARD_FAILURE_BUILDER build_failure,
         void *failure_context, FILE *diagnostic,
         VHO_FHE_STANDARD_CALL_RESULT *evaluated)
{
    VHO_FHE_STANDARD_PARM parameters[5];
    memset(parameters, 0, sizeof(parameters));
    parameters[0].formal_ty = model.handle_ty;
    parameters[0].actual_ty = model.handle_ty;
    parameters[0].actual = VHO_FHE_Runtime_Handle_Load(model);
    parameters[0].policy = VHO_FHE_STANDARD_PARM_BORROWED_READ_ONLY;
    parameters[1].formal_ty = ciphertext_ty;
    parameters[1].actual_ty = ciphertext_ty;
    parameters[1].actual = VHO_FHE_Runtime_Handle_ST_Load
                               (refreshed_st, ciphertext_ty);
    parameters[1].policy = VHO_FHE_STANDARD_PARM_BORROWED_READ_ONLY;
    parameters[2].formal_ty = ciphertext_ty;
    parameters[2].actual_ty = ciphertext_ty;
    parameters[2].actual = VHO_FHE_Runtime_Handle_ST_Load
                               (stage_st, ciphertext_ty);
    parameters[2].policy = VHO_FHE_STANDARD_PARM_BORROWED_READ_ONLY;
    parameters[3].formal_ty = descriptor_ty;
    parameters[3].actual_ty = descriptor_ty;
    parameters[3].actual = VHO_FHE_Runtime_Handle_ST_Load
                               (descriptor_st, descriptor_ty);
    parameters[3].policy = VHO_FHE_STANDARD_PARM_BORROWED_READ_ONLY;
    parameters[4].formal_ty = Make_Pointer_Type(ciphertext_ty);
    parameters[4].policy = VHO_FHE_STANDARD_PARM_OUTPUT_SLOT;
    parameters[4].output_name = "fhe_relu_result";
    parameters[4].output_ty = ciphertext_ty;

    VHO_FHE_STANDARD_CALL_SPEC spec;
    memset(&spec, 0, sizeof(spec));
    spec.function_name = "open64_fhe_relu_reconstruct_v1";
    spec.status_ty = MTYPE_To_TY(MTYPE_I4);
    spec.parameters = parameters;
    spec.parameter_count = 5;
    spec.status_name = "fhe_relu_reconstruct_status";
    spec.source_position = source_position;
    spec.build_failure = build_failure;
    spec.failure_context = failure_context;
    BOOL valid = VHO_FHE_Runtime_Append_Call
                     (block, &spec, diagnostic, evaluated);
    for (UINT32 i = 0; i < 4; ++i)
        WN_DELETE_Tree(parameters[i].actual);
    return valid;
}

BOOL
VHO_FHE_Runtime_Build_Relu_Sequence
        (struct pu_info *pu_info, DSL_IR_VALUE_ID anchor_value_id,
         UINT32 first_static_ordinal, SRCPOS source_position,
         VHO_FHE_STANDARD_FAILURE_BUILDER build_failure,
         void *failure_context, FILE *diagnostic,
         VHO_FHE_RUNTIME_CALL_SEQUENCE *sequence)
{
    VHO_FHE_Runtime_Call_Sequence_Init(sequence);
    if (sequence == NULL || first_static_ordinal == 0 ||
        source_position == 0 || build_failure == NULL)
        return VHO_FHE_Runtime_Semantic_Report
                   (diagnostic, "ReLU call request is incomplete");

    VHO_FHE_RUNTIME_HANDLE_BINDING model;
    VHO_FHE_RUNTIME_HANDLE_BINDING anchor;
    VHO_FHE_RUNTIME_HANDLE_BINDING coefficients[3];
    const char *roles[3] = {
        "fhe.relu.coefficient.stage0",
        "fhe.relu.coefficient.stage1",
        "fhe.relu.coefficient.stage2"
    };
    if (!VHO_FHE_Runtime_Resolve_Role_Handle
             (pu_info, "fhe.model", diagnostic, &model) ||
        !VHO_FHE_Runtime_Resolve_Value_Handle
             (pu_info, anchor_value_id, diagnostic, &anchor))
        return FALSE;
    for (UINT32 i = 0; i < 3; ++i) {
        if (!VHO_FHE_Runtime_Resolve_Role_Handle
                 (pu_info, roles[i], diagnostic, &coefficients[i]))
            return FALSE;
    }

    VHO_FHE_RUNTIME_CALL_SEQUENCE bootstrap;
    if (!VHO_FHE_Runtime_Build_Bootstrap_Sequence
             (pu_info, anchor_value_id, first_static_ordinal,
              source_position, build_failure, failure_context,
              diagnostic, &bootstrap))
        return FALSE;
    WN *block = bootstrap.block;
    TY_IDX descriptor_ty = VHO_FHE_Runtime_Opaque_Handle_TY
                               ("open64_fhe_operation_desc_v1");
    ST_IDX refreshed_st = bootstrap.output_st;
    ST_IDX current_st = refreshed_st;
    TY_IDX ciphertext_ty = anchor.handle_ty;

    VHO_FHE_STANDARD_CALL_RESULT selected;
    VHO_FHE_STANDARD_CALL_RESULT evaluated;
    BOOL valid = VHO_FHE_Runtime_Append_Selector
        (block, model, current_st, ciphertext_ty,
         first_static_ordinal + 1, VHO_FHE_RUNTIME_OP_RELU_NORMALIZE,
         source_position, build_failure, failure_context, diagnostic,
         &selected) &&
        VHO_FHE_Runtime_Append_Unary_Evaluation
        (block, "open64_fhe_relu_normalize_v1", model,
         current_st, ciphertext_ty, selected.output_st, descriptor_ty,
         source_position, build_failure, failure_context, diagnostic,
         &evaluated);
    if (!valid) {
        WN_DELETE_Tree(block);
        return FALSE;
    }
    current_st = evaluated.output_st;

    for (UINT32 stage = 0; stage < 3; ++stage) {
        valid = VHO_FHE_Runtime_Append_Selector
            (block, model, current_st, ciphertext_ty,
             first_static_ordinal + 2 + stage,
             VHO_FHE_RUNTIME_OP_RELU_POLY_STAGE, source_position,
             build_failure, failure_context, diagnostic, &selected) &&
            VHO_FHE_Runtime_Append_Poly_Evaluation
            (block, model, current_st, ciphertext_ty,
             coefficients[stage], selected.output_st, descriptor_ty,
             source_position, build_failure, failure_context, diagnostic,
             &evaluated);
        if (!valid) {
            WN_DELETE_Tree(block);
            return FALSE;
        }
        current_st = evaluated.output_st;
    }

    valid = VHO_FHE_Runtime_Append_Selector
        (block, model, refreshed_st, ciphertext_ty,
         first_static_ordinal + 5, VHO_FHE_RUNTIME_OP_RELU_RECONSTRUCT,
         source_position, build_failure, failure_context, diagnostic,
         &selected) &&
        VHO_FHE_Runtime_Append_Reconstruct_Evaluation
        (block, model, refreshed_st, current_st, ciphertext_ty,
         selected.output_st, descriptor_ty, source_position,
         build_failure, failure_context, diagnostic, &evaluated);
    if (!valid) {
        WN_DELETE_Tree(block);
        return FALSE;
    }

    sequence->block = block;
    sequence->output_st = evaluated.output_st;
    sequence->standard_call_count = 12;
    sequence->output_handle_count = 12;
    sequence->status_check_count = 12;
    return TRUE;
}

BOOL
VHO_FHE_Runtime_Finalize_Projected_Output
        (const VHO_FHE_RUNTIME_HANDLE_BINDING *output,
         SRCPOS source_position, VHO_FHE_RUNTIME_CALL_SEQUENCE *sequence,
         WN **result_definition)
{
    if (result_definition != NULL)
        *result_definition = NULL;
    if (output == NULL || sequence == NULL || sequence->block == NULL ||
        sequence->output_st == ST_IDX_ZERO || result_definition == NULL ||
        source_position == 0 ||
        !VHO_FHE_Runtime_Handle_ST_Valid
             (output->handle_st, output->handle_ty) ||
        ST_IDX_level(sequence->output_st) != CURRENT_SYMTAB ||
        ST_IDX_index(sequence->output_st) == 0 ||
        ST_IDX_index(sequence->output_st) >= ST_Table_Size(CURRENT_SYMTAB) ||
        ST_type(St_Table[sequence->output_st]) != output->handle_ty)
        return FALSE;
    WN *value = VHO_FHE_Runtime_Handle_ST_Load
                    (sequence->output_st, output->handle_ty);
    WN *definition = WN_CreateStid
        (OPR_STID, MTYPE_V, TY_mtype(output->handle_ty), 0,
         output->handle_st, output->handle_ty, value);
    WN_Set_Linenum(definition, source_position);
    WN_INSERT_BlockLast(sequence->block, definition);
    *result_definition = definition;
    return TRUE;
}
