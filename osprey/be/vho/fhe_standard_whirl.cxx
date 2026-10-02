/*
 * Copyright (C) 2026 Open64 Project
 *
 * Standard WHIRL construction for the SYNC-5 runtime ABI boundary.  This
 * file deliberately knows no FHE operation names or schedule semantics.
 */

#include <string.h>

#include "fhe_standard_whirl.h"
#include "mtypes.h"
#include "symtab_utils.h"
#include "wn.h"
#include "wn_util.h"

/* Reject malformed guard shapes with the same code in producer and reader. */
static BOOL
VHO_FHE_Standard_Guard_Report
        (FILE *diagnostic, const char *code, const char *message)
{
    if (diagnostic != NULL)
        fprintf(diagnostic, "%s: %s\n", code, message);
    return FALSE;
}

/* A mapped PU root needs a complete body before any guard tree is inspected. */
BOOL
VHO_FHE_Standard_Function_Body_Valid
        (const WN *entry, FILE *diagnostic)
{
    if (entry == NULL || WN_operator(entry) != OPR_FUNC_ENTRY ||
        WN_kid_count(entry) < 3 || WN_func_body(entry) == NULL ||
        WN_operator(WN_func_body(entry)) != OPR_BLOCK)
        return VHO_FHE_Standard_Guard_Report
                   (diagnostic, "CFHELOWER-CALL-000",
                    "runtime PU has no valid function body");
    return TRUE;
}

/* Require a fresh null store immediately before a void callee writes out. */
BOOL
VHO_FHE_Standard_Null_Initializer_Valid
        (const WN *initialization, ST_IDX result_st, FILE *diagnostic)
{
    if (initialization == NULL || result_st == ST_IDX_ZERO ||
        WN_operator(initialization) != OPR_STID ||
        WN_st_idx(initialization) != result_st ||
        WN_kid_count(initialization) != 1 ||
        WN_kid0(initialization) == NULL ||
        WN_operator(WN_kid0(initialization)) != OPR_INTCONST ||
        WN_const_val(WN_kid0(initialization)) != 0)
        return VHO_FHE_Standard_Guard_Report
                   (diagnostic, "CFHELOWER-CALL-003",
                    "runtime result slot is not freshly null");
    return TRUE;
}

/* Check the entire immediate null-result branch before dereferencing kids. */
BOOL
VHO_FHE_Standard_Null_Guard_Valid
        (const WN *check, ST_IDX result_st, SRCPOS source_position,
         FILE *diagnostic)
{
    if (check == NULL || result_st == ST_IDX_ZERO ||
        source_position == 0 || WN_operator(check) != OPR_IF ||
        WN_kid_count(check) != 3 ||
        WN_Get_Linenum(check) != source_position)
        return VHO_FHE_Standard_Guard_Report
                   (diagnostic, "CFHELOWER-CALL-005",
                    "runtime call result guard is missing");
    const WN *test = WN_if_test(check);
    const WN *failure = WN_then(check);
    const WN *success = WN_else(check);
    if (test == NULL || WN_operator(test) != OPR_EQ ||
        WN_kid_count(test) != 2 || WN_kid0(test) == NULL ||
        WN_kid1(test) == NULL ||
        WN_operator(WN_kid0(test)) != OPR_LDID ||
        WN_st_idx(WN_kid0(test)) != result_st ||
        WN_operator(WN_kid1(test)) != OPR_INTCONST ||
        WN_const_val(WN_kid1(test)) != 0 ||
        failure == NULL || WN_operator(failure) != OPR_BLOCK ||
        WN_first(failure) == NULL ||
        WN_first(failure) != WN_last(failure) ||
        WN_operator(WN_first(failure)) != OPR_RETURN ||
        WN_Get_Linenum(WN_first(failure)) != source_position ||
        success == NULL || WN_operator(success) != OPR_BLOCK ||
        WN_first(success) != NULL)
        return VHO_FHE_Standard_Guard_Report
                   (diagnostic, "CFHELOWER-CALL-005",
                    "runtime call result guard is malformed");
    return TRUE;
}

static BOOL
VHO_FHE_Standard_Report (FILE *diagnostic, const char *message)
{
    if (diagnostic != NULL)
        fprintf(diagnostic, "CFHELOWER-001: %s\n", message);
    return FALSE;
}

static BOOL
VHO_FHE_Standard_TY_Valid (TY_IDX ty)
{
    return ty != TY_IDX_ZERO && TY_IDX_index(ty) < TY_Table_Size();
}

static BOOL
VHO_FHE_Standard_Status_TY_Valid (TY_IDX ty)
{
    if (!VHO_FHE_Standard_TY_Valid(ty) || TY_kind(ty) != KIND_SCALAR)
        return FALSE;
    TYPE_ID mtype = TY_mtype(ty);
    return mtype == MTYPE_I4 || mtype == MTYPE_U4 ||
           mtype == MTYPE_I8 || mtype == MTYPE_U8;
}

static BOOL
VHO_FHE_Standard_Parm_Valid (const VHO_FHE_STANDARD_PARM *parameter)
{
    if (parameter == NULL ||
        !VHO_FHE_Standard_TY_Valid(parameter->formal_ty))
        return FALSE;

    TYPE_ID formal_mtype = TY_mtype(parameter->formal_ty);
    switch (parameter->policy) {
    case VHO_FHE_STANDARD_PARM_BY_VALUE:
        return parameter->actual_ty == parameter->formal_ty &&
               parameter->actual != NULL &&
               WN_rtype(parameter->actual) == formal_mtype;
    case VHO_FHE_STANDARD_PARM_BORROWED_READ_ONLY:
    case VHO_FHE_STANDARD_PARM_BORROWED_MUTABLE:
        return TY_kind(parameter->formal_ty) == KIND_POINTER &&
               parameter->actual_ty == parameter->formal_ty &&
               parameter->actual != NULL &&
               WN_rtype(parameter->actual) == formal_mtype;
    case VHO_FHE_STANDARD_PARM_OUTPUT_SLOT:
        return TY_kind(parameter->formal_ty) == KIND_POINTER &&
               parameter->actual_ty == TY_IDX_ZERO &&
               parameter->actual == NULL &&
               parameter->output_name != NULL &&
               parameter->output_name[0] != '\0' &&
               VHO_FHE_Standard_TY_Valid(parameter->output_ty) &&
               TY_pointed(parameter->formal_ty) == parameter->output_ty;
    default:
        return FALSE;
    }
}

static TY_IDX
VHO_FHE_Standard_Function_TY
        (TY_IDX status_ty,
         const VHO_FHE_STANDARD_PARM *parameters,
         UINT32 parameter_count)
{
    TY_IDX function_ty;
    TY &function = New_TY(function_ty);
    TY_Init(function, 0, KIND_FUNCTION, MTYPE_UNKNOWN, 0);
    Set_TY_align(function_ty, 1);

    TYLIST_IDX tylist_idx;
    Set_TYLIST_type(New_TYLIST(tylist_idx), status_ty);
    Set_TY_tylist(function_ty, tylist_idx);
    for (UINT32 i = 0; i < parameter_count; ++i)
        Set_TYLIST_type(New_TYLIST(tylist_idx), parameters[i].formal_ty);
    Set_TYLIST_type(New_TYLIST(tylist_idx), TY_IDX_ZERO);

    TY_IDX unique_ty = TY_is_unique(function_ty);
    if (unique_ty != function_ty &&
        TY_IDX_index(function_ty) == Ty_tab.Size() - 1) {
        Tylist_Table.Delete_last(parameter_count + 2);
        Ty_tab.Delete_last();
    }
    return unique_ty;
}

static ST *
VHO_FHE_Standard_Function_ST
        (TY_IDX function_ty, const char *function_name, FILE *diagnostic)
{
    ST *st;
    INT32 index;
    FOREACH_SYMBOL(GLOBAL_SYMTAB, st, index) {
        if (strcmp(ST_name(st), function_name) != 0)
            continue;
        if (ST_class(st) != CLASS_FUNC) {
            VHO_FHE_Standard_Report
                (diagnostic,
                 "runtime function name conflicts with a non-function symbol");
            return NULL;
        }
        TY_IDX existing_ty = ST_pu_type(st);
        if (existing_ty == function_ty ||
            TY_are_equivalent(existing_ty, function_ty,
                              TY_EQUIV_ALIGN | TY_EQUIV_QUALIFIER))
            return st;
        VHO_FHE_Standard_Report
            (diagnostic, "runtime function name has a conflicting prototype");
        return NULL;
    }
    return Gen_Intrinsic_Function(function_ty, function_name);
}

static ST_IDX
VHO_FHE_Standard_Local_ST
        (const char *name, TY_IDX ty, SRCPOS source_position)
{
    ST *st = New_ST(CURRENT_SYMTAB);
    ST_Init(st, Save_Str(name), CLASS_VAR, SCLASS_AUTO, EXPORT_LOCAL, ty);
    Set_ST_is_temp_var(*st);
    Set_ST_Srcpos(*st, source_position);
    return ST_st_idx(st);
}

void
VHO_FHE_Standard_Call_Result_Init (VHO_FHE_STANDARD_CALL_RESULT *result)
{
    if (result != NULL)
        memset(result, 0, sizeof(*result));
}

BOOL
VHO_FHE_Build_Standard_Call
        (const VHO_FHE_STANDARD_CALL_SPEC *spec,
         FILE *diagnostic,
         VHO_FHE_STANDARD_CALL_RESULT *result)
{
    VHO_FHE_Standard_Call_Result_Init(result);
    if (spec == NULL || result == NULL || spec->function_name == NULL ||
        spec->function_name[0] == '\0' || spec->status_name == NULL ||
        spec->status_name[0] == '\0' || spec->source_position == 0 ||
        !VHO_FHE_Standard_Status_TY_Valid(spec->status_ty) ||
        spec->build_failure == NULL ||
        (spec->parameter_count != 0 && spec->parameters == NULL))
        return VHO_FHE_Standard_Report
                   (diagnostic, "standard-call specification is incomplete");

    UINT32 output_count = 0;
    for (UINT32 i = 0; i < spec->parameter_count; ++i) {
        if (!VHO_FHE_Standard_Parm_Valid(&spec->parameters[i]))
            return VHO_FHE_Standard_Report
                       (diagnostic, "standard-call parameter is invalid");
        if (spec->parameters[i].policy ==
            VHO_FHE_STANDARD_PARM_OUTPUT_SLOT)
            ++output_count;
    }
    if (output_count > 1)
        return VHO_FHE_Standard_Report
                   (diagnostic, "standard-call v1 permits one output slot");

    TY_IDX function_ty = VHO_FHE_Standard_Function_TY
                             (spec->status_ty, spec->parameters,
                              spec->parameter_count);
    ST *function_st = VHO_FHE_Standard_Function_ST
                          (function_ty, spec->function_name, diagnostic);
    if (function_st == NULL)
        return VHO_FHE_Standard_Report
                   (diagnostic, "could not create runtime function symbol");
    Set_ST_Srcpos(*function_st, spec->source_position);

    ST_IDX status_st = VHO_FHE_Standard_Local_ST
                           (spec->status_name, spec->status_ty,
                            spec->source_position);
    ST_IDX output_st = ST_IDX_ZERO;
    for (UINT32 i = 0; i < spec->parameter_count; ++i) {
        if (spec->parameters[i].policy ==
            VHO_FHE_STANDARD_PARM_OUTPUT_SLOT) {
            output_st = VHO_FHE_Standard_Local_ST
                            (spec->parameters[i].output_name,
                             spec->parameters[i].output_ty,
                             spec->source_position);
            break;
        }
    }

    WN *failure = spec->build_failure
                      (status_st, output_st, spec->failure_context, diagnostic);
    if (failure == NULL || WN_operator(failure) != OPR_BLOCK) {
        if (failure != NULL)
            WN_DELETE_Tree(failure);
        return VHO_FHE_Standard_Report
                   (diagnostic, "failure-block construction failed");
    }

    TYPE_ID status_mtype = TY_mtype(spec->status_ty);
    WN *block = WN_CreateBlock();
    if (output_st != ST_IDX_ZERO) {
        TYPE_ID output_mtype = TY_mtype(ST_type(output_st));
        WN *initialize = WN_CreateStid
                             (OPR_STID, MTYPE_V, output_mtype, 0, output_st,
                              ST_type(output_st),
                              WN_Intconst(output_mtype, 0));
        WN_Set_Linenum(initialize, spec->source_position);
        WN_INSERT_BlockLast(block, initialize);
    }
    WN *call = WN_Call(status_mtype, MTYPE_V, spec->parameter_count,
                       function_st);
    WN_Set_Call_Default_Flags(call);
    WN_Set_Linenum(call, spec->source_position);
    for (UINT32 i = 0; i < spec->parameter_count; ++i) {
        const VHO_FHE_STANDARD_PARM *parameter = &spec->parameters[i];
        UINT32 flags = WN_PARM_BY_VALUE;
        WN *actual = NULL;
        if (parameter->policy == VHO_FHE_STANDARD_PARM_OUTPUT_SLOT) {
            flags = WN_PARM_BY_REFERENCE | WN_PARM_PASSED_NOT_SAVED |
                    WN_PARM_OUT;
            actual = WN_Lda(Pointer_Mtype, 0, &St_Table[output_st]);
        }
        else {
            actual = WN_COPY_Tree(parameter->actual);
            if (parameter->policy ==
                VHO_FHE_STANDARD_PARM_BORROWED_READ_ONLY)
                flags = WN_PARM_BY_REFERENCE |
                        WN_PARM_PASSED_NOT_SAVED | WN_PARM_READ_ONLY;
            else if (parameter->policy ==
                     VHO_FHE_STANDARD_PARM_BORROWED_MUTABLE)
                flags = WN_PARM_BY_REFERENCE |
                        WN_PARM_PASSED_NOT_SAVED | WN_PARM_OUT;
        }
        WN_kid(call, i) = WN_CreateParm
                              (TY_mtype(parameter->formal_ty), actual,
                               parameter->formal_ty, flags);
    }
    WN_INSERT_BlockLast(block, call);

    WN *return_value = WN_CreateLdid
                           (OPR_LDID, status_mtype, status_mtype, -1,
                            Return_Val_Preg, spec->status_ty);
    WN *capture = WN_CreateStid
                      (OPR_STID, MTYPE_V, status_mtype, 0, status_st,
                       spec->status_ty, return_value);
    WN_Set_Linenum(capture, spec->source_position);
    WN_INSERT_BlockLast(block, capture);

    WN *status_load = WN_CreateLdid
                          (OPR_LDID, status_mtype, status_mtype, 0,
                           status_st, spec->status_ty);
    WN *test = WN_NE(status_mtype, status_load,
                     WN_Intconst(status_mtype, 0));
    WN *check = WN_CreateIf(test, failure, WN_CreateBlock());
    WN_Set_Linenum(check, spec->source_position);
    WN_INSERT_BlockLast(block, check);

    result->block = block;
    result->function_st = ST_st_idx(function_st);
    result->status_st = status_st;
    result->output_st = output_st;
    result->parameter_count = spec->parameter_count;
    return TRUE;
}

BOOL
VHO_FHE_Commit_Standard_Call
        (WN *parent_block, WN *before,
         VHO_FHE_STANDARD_CALL_RESULT *result, FILE *diagnostic)
{
    if (parent_block == NULL || WN_operator(parent_block) != OPR_BLOCK ||
        result == NULL || result->block == NULL ||
        WN_operator(result->block) != OPR_BLOCK)
        return VHO_FHE_Standard_Report
                   (diagnostic, "standard-call commit target is invalid");
    if (before != NULL) {
        BOOL found = FALSE;
        for (WN *stmt = WN_first(parent_block); stmt != NULL;
             stmt = WN_next(stmt)) {
            if (stmt == before) {
                found = TRUE;
                break;
            }
        }
        if (!found)
            return VHO_FHE_Standard_Report
                       (diagnostic, "standard-call insertion point is invalid");
    }

    while (WN_first(result->block) != NULL) {
        WN *statement = WN_EXTRACT_FromBlock
                            (result->block, WN_first(result->block));
        if (before != NULL)
            WN_INSERT_BlockBefore(parent_block, before, statement);
        else
            WN_INSERT_BlockLast(parent_block, statement);
    }
    WN_DELETE_Tree(result->block);
    result->block = NULL;
    return TRUE;
}
