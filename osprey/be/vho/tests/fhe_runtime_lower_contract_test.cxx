/*
 * Contract test for SYNC-5 runtime lowering and standard-WHIRL construction.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "defs.h"
#include "config.h"
#include "config_fhe.h"
#include "config_targ_opt.h"
#include "controls.h"
#include "dsl_builder.h"
#include "dsl_opcode.h"
#include "dwarf_DST_mem.h"
#include "erglob.h"
#include "errors.h"
#include "err_host.tab"
#include "fhe_runtime_lower.h"
#include "fhe_semantic_runtime_lower.h"
#include "fhe_standard_whirl.h"
#include "fhe_unlowered_gate.h"
#include "glob.h"
#include "ir_bwrite.h"
#include "ir_reader.h"
#include "mempool.h"
#include "pu_info.h"
#include "stab.h"
#include "wn.h"

BOOL Run_vsaopt = FALSE;
INT8 Debug_Level = 0;

void
Signal_Cleanup (INT sig)
{
    (void)sig;
}

const char *
Host_Format_Parm (INT kind, MEM_PTR parm)
{
    (void)kind;
    (void)parm;
    return "";
}

static UINT32 observed_gate_count;
static UINT32 observed_pass_count;
static const char *observed_checkpoint_path;
static UINT32 test_source_file_id;

static BOOL
Observe_Runtime_Gate
        (struct pu_info *pu_info, WN *tree,
         const VHO_FHE_RUNTIME_LOWER_OPTIONS *options, FILE *diagnostic)
{
    (void)diagnostic;
    ++observed_gate_count;
    if (pu_info == NULL || tree == NULL || options == NULL ||
        options->checkpoint_output_path == NULL ||
        strcmp(options->checkpoint_output_path, "/tmp/fhe_runtime.mid.B") != 0)
        return FALSE;
    if (observed_checkpoint_path == NULL)
        observed_checkpoint_path = options->checkpoint_output_path;
    return observed_checkpoint_path == options->checkpoint_output_path;
}

static BOOL
Observe_Runtime_Pass
        (struct pu_info *pu_info, WN **tree,
         const VHO_FHE_RUNTIME_LOWER_OPTIONS *options, FILE *diagnostic,
         VHO_FHE_RUNTIME_LOWER_RESULT *result)
{
    (void)options;
    (void)diagnostic;
    ++observed_pass_count;
    result->standard_call_count = 2;
    result->output_handle_count = 2;
    result->status_check_count = 2;
    return pu_info != NULL && tree != NULL && *tree != NULL;
}

static void
Initialize_Test_Context (void)
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
    IR_reader_init();
    Initialize_Symbol_Tables(TRUE);
    DST_Init(NULL, 0);
}

static SRCPOS
Test_Source_Position (void)
{
    USRCPOS position;
    USRCPOS_clear(position);
    USRCPOS_filenum(position) = test_source_file_id;
    USRCPOS_linenum(position) = 37;
    USRCPOS_column(position) = 5;
    USRCPOS_stmt_begin(position) = 1;
    return USRCPOS_srcpos(position);
}

static void
Set_Block_Source_Position (WN *tree, SRCPOS source_position)
{
    if (tree == NULL)
        return;
    OPERATOR opr = WN_operator(tree);
    if ((OPERATOR_is_stmt(opr) || OPERATOR_is_scf(opr)) &&
        opr != OPR_BLOCK)
        WN_Set_Linenum(tree, source_position);
    if (opr == OPR_BLOCK) {
        for (WN *stmt = WN_first(tree); stmt != NULL; stmt = WN_next(stmt))
            Set_Block_Source_Position(stmt, source_position);
        return;
    }
    for (INT32 i = 0; i < WN_kid_count(tree); ++i)
        Set_Block_Source_Position(WN_kid(tree, i), source_position);
}

static WN *
Build_Failure_Block
        (ST_IDX status_st, ST_IDX output_st, void *context, FILE *diagnostic)
{
    (void)context;
    (void)diagnostic;
    WN *block = WN_CreateBlock();
    WN *status = WN_CreateLdid
                     (OPR_LDID, MTYPE_I4, MTYPE_I4, 0, status_st,
                      ST_type(status_st));
    WN_INSERT_BlockLast(block, WN_CreateEval(status));
    if (output_st != ST_IDX_ZERO) {
        TYPE_ID output_mtype = TY_mtype(ST_type(output_st));
        WN *output = WN_CreateLdid
                         (OPR_LDID, output_mtype, output_mtype, 0, output_st,
                          ST_type(output_st));
        WN_INSERT_BlockLast(block, WN_CreateEval(output));
    }
    WN_INSERT_BlockLast(block, WN_CreateReturn());
    Set_Block_Source_Position(block, Test_Source_Position());
    return block;
}

static UINT32
Count_Global_Functions (const char *name)
{
    UINT32 count = 0;
    ST *st;
    INT32 index;
    FOREACH_SYMBOL(GLOBAL_SYMTAB, st, index) {
        if (ST_class(st) == CLASS_FUNC && strcmp(ST_name(st), name) == 0)
            ++count;
    }
    return count;
}

static UINT32
Count_Global_Variables (void)
{
    UINT32 count = 0;
    ST *st;
    INT32 index;
    FOREACH_SYMBOL(GLOBAL_SYMTAB, st, index) {
        if (ST_class(st) == CLASS_VAR)
            ++count;
    }
    return count;
}

static TY_IDX
Create_Opaque_Handle_TY (const char *name)
{
    for (UINT32 index = 1; index < TY_Table_Size(); ++index) {
        TY_IDX candidate = make_TY_IDX(index);
        if (TY_kind(candidate) != KIND_POINTER)
            continue;
        TY_IDX pointee = TY_pointed(candidate);
        if (pointee != TY_IDX_ZERO &&
            TY_IDX_index(pointee) < TY_Table_Size() &&
            TY_kind(pointee) == KIND_STRUCT && TY_name(pointee) != NULL &&
            strcmp(TY_name(pointee), name) == 0)
            return Make_Pointer_Type(pointee);
    }

    TY_IDX opaque_ty;
    TY &opaque = New_TY(opaque_ty);
    TY_Init(opaque, 0, KIND_STRUCT, MTYPE_M, Save_Str(name));
    Set_TY_align(opaque_ty, 1);
    return Make_Pointer_Type(opaque_ty);
}

static ST_IDX
Create_Local_Handle_ST
        (const char *name, TY_IDX ty, SRCPOS source_position)
{
    ST *st = New_ST(CURRENT_SYMTAB);
    ST_Init(st, Save_Str(name), CLASS_VAR, SCLASS_AUTO, EXPORT_LOCAL, ty);
    Set_ST_is_temp_var(*st);
    Set_ST_Srcpos(*st, source_position);
    return ST_st_idx(st);
}

static BOOL
Build_Two_Calls (WN *body)
{
    TY_IDX status_ty = MTYPE_To_TY(MTYPE_I4);
    TY_IDX handle_ty = Make_Pointer_Type(MTYPE_To_TY(MTYPE_V));
    TY_IDX output_ty = Make_Pointer_Type(handle_ty);
    SRCPOS source_position = Test_Source_Position();

    VHO_FHE_STANDARD_PARM first_parameters[2];
    memset(first_parameters, 0, sizeof(first_parameters));
    first_parameters[0].formal_ty = MTYPE_To_TY(MTYPE_U8);
    first_parameters[0].actual_ty = MTYPE_To_TY(MTYPE_U8);
    first_parameters[0].actual = WN_Intconst(MTYPE_U8, 17);
    first_parameters[0].policy = VHO_FHE_STANDARD_PARM_BY_VALUE;
    first_parameters[1].formal_ty = output_ty;
    first_parameters[1].policy = VHO_FHE_STANDARD_PARM_OUTPUT_SLOT;
    first_parameters[1].output_name = "fhe_context";
    first_parameters[1].output_ty = handle_ty;

    VHO_FHE_STANDARD_CALL_SPEC first_spec;
    memset(&first_spec, 0, sizeof(first_spec));
    first_spec.function_name = "open64_fhe_context_import_v1";
    first_spec.status_ty = status_ty;
    first_spec.parameters = first_parameters;
    first_spec.parameter_count = 2;
    first_spec.status_name = "fhe_status_0";
    first_spec.source_position = source_position;
    first_spec.build_failure = Build_Failure_Block;

    VHO_FHE_STANDARD_CALL_RESULT first_result;
    if (!VHO_FHE_Build_Standard_Call
             (&first_spec, stderr, &first_result) ||
        first_result.output_st == ST_IDX_ZERO ||
        ST_Srcpos(St_Table[first_result.output_st]) != source_position ||
        !VHO_FHE_Commit_Standard_Call
             (body, NULL, &first_result, stderr)) {
        return FALSE;
    }

    UINT32 type_count = TY_Table_Size();
    VHO_FHE_STANDARD_CALL_RESULT reuse_result;
    BOOL reused = VHO_FHE_Build_Standard_Call
                      (&first_spec, stderr, &reuse_result) &&
                  reuse_result.function_st == first_result.function_st &&
                  TY_Table_Size() == type_count;
    if (reuse_result.block != NULL)
        WN_DELETE_Tree(reuse_result.block);
    if (!reused) {
        fprintf(stderr, "standard function type/symbol interning changed\n");
        return FALSE;
    }

    VHO_FHE_STANDARD_PARM second_parameters[2];
    memset(second_parameters, 0, sizeof(second_parameters));
    second_parameters[0].formal_ty = handle_ty;
    second_parameters[0].actual_ty = handle_ty;
    second_parameters[0].actual = WN_CreateLdid
                                      (OPR_LDID, Pointer_Mtype,
                                       Pointer_Mtype, 0,
                                       first_result.output_st, handle_ty);
    second_parameters[0].policy =
        VHO_FHE_STANDARD_PARM_BORROWED_READ_ONLY;
    second_parameters[1].formal_ty = output_ty;
    second_parameters[1].policy = VHO_FHE_STANDARD_PARM_OUTPUT_SLOT;
    second_parameters[1].output_name = "fhe_ciphertext";
    second_parameters[1].output_ty = handle_ty;

    VHO_FHE_STANDARD_CALL_SPEC second_spec;
    memset(&second_spec, 0, sizeof(second_spec));
    second_spec.function_name = "open64_fhe_ciphertext_import_v1";
    second_spec.status_ty = status_ty;
    second_spec.parameters = second_parameters;
    second_spec.parameter_count = 2;
    second_spec.status_name = "fhe_status_1";
    second_spec.source_position = source_position;
    second_spec.build_failure = Build_Failure_Block;

    VHO_FHE_STANDARD_CALL_RESULT second_result;
    BOOL valid = VHO_FHE_Build_Standard_Call
                     (&second_spec, stderr, &second_result) &&
                 second_result.output_st != ST_IDX_ZERO &&
                 VHO_FHE_Commit_Standard_Call
                     (body, NULL, &second_result, stderr);
    WN_DELETE_Tree(second_parameters[0].actual);
    if (!valid)
        return FALSE;

    TY_IDX wrong_handle_ty = Make_Pointer_Type(MTYPE_To_TY(MTYPE_I4));
    VHO_FHE_STANDARD_PARM wrong_parameter;
    memset(&wrong_parameter, 0, sizeof(wrong_parameter));
    wrong_parameter.formal_ty = handle_ty;
    wrong_parameter.actual_ty = wrong_handle_ty;
    wrong_parameter.actual = WN_CreateLdid
                                 (OPR_LDID, Pointer_Mtype, Pointer_Mtype, 0,
                                  first_result.output_st, handle_ty);
    wrong_parameter.policy = VHO_FHE_STANDARD_PARM_BORROWED_READ_ONLY;
    VHO_FHE_STANDARD_CALL_SPEC wrong_spec;
    memset(&wrong_spec, 0, sizeof(wrong_spec));
    wrong_spec.function_name = "open64_fhe_wrong_handle_v1";
    wrong_spec.status_ty = status_ty;
    wrong_spec.parameters = &wrong_parameter;
    wrong_spec.parameter_count = 1;
    wrong_spec.status_name = "fhe_status_wrong";
    wrong_spec.source_position = source_position;
    wrong_spec.build_failure = Build_Failure_Block;
    VHO_FHE_STANDARD_CALL_RESULT wrong_result;
    BOOL wrong_rejected = !VHO_FHE_Build_Standard_Call
                               (&wrong_spec, NULL, &wrong_result) &&
                          wrong_result.block == NULL;
    WN_DELETE_Tree(wrong_parameter.actual);
    if (!wrong_rejected)
        return FALSE;

    VHO_FHE_STANDARD_PARM conflicting_parameters[2] = {
        first_parameters[0], first_parameters[1]
    };
    conflicting_parameters[0].formal_ty = MTYPE_To_TY(MTYPE_I4);
    conflicting_parameters[0].actual_ty = MTYPE_To_TY(MTYPE_I4);
    conflicting_parameters[0].actual = WN_Intconst(MTYPE_I4, 17);
    VHO_FHE_STANDARD_CALL_SPEC conflicting_spec = first_spec;
    conflicting_spec.parameters = conflicting_parameters;
    UINT32 function_count = Count_Global_Functions(first_spec.function_name);
    VHO_FHE_STANDARD_CALL_RESULT conflicting_result;
    BOOL conflict_rejected = !VHO_FHE_Build_Standard_Call
                                  (&conflicting_spec, NULL,
                                   &conflicting_result) &&
                             conflicting_result.block == NULL &&
                             Count_Global_Functions(first_spec.function_name) ==
                                 function_count;
    WN_DELETE_Tree(conflicting_parameters[0].actual);
    return conflict_rejected;
}

static BOOL
Verify_Two_Calls (WN *body)
{
    UINT32 call_count = 0;
    UINT32 status_count = 0;
    UINT32 if_count = 0;
    for (WN *statement = WN_first(body); statement != NULL;
         statement = WN_next(statement)) {
        if (WN_operator(statement) == OPR_CALL) {
            ++call_count;
            TY_IDX function_ty = ST_pu_type(WN_st(statement));
            TYLIST_IDX list = TY_tylist(function_ty);
            SRCPOS call_position = WN_Get_Linenum(statement);
            if (TYLIST_type(Tylist_Table[list]) != MTYPE_To_TY(MTYPE_I4) ||
                TYLIST_type(Tylist_Table[list + 1]) == TY_IDX_ZERO ||
                TYLIST_type(Tylist_Table[list + 2]) == TY_IDX_ZERO ||
                TYLIST_type(Tylist_Table[list + 3]) != TY_IDX_ZERO ||
                SRCPOS_linenum(call_position) != 37) {
                fprintf(stderr,
                        "standard call prototype or source position changed: "
                        "return=0x%x parm0=0x%x parm1=0x%x end=0x%x line=%u\n",
                        TYLIST_type(Tylist_Table[list]),
                        TYLIST_type(Tylist_Table[list + 1]),
                        TYLIST_type(Tylist_Table[list + 2]),
                        TYLIST_type(Tylist_Table[list + 3]),
                        SRCPOS_linenum(call_position));
                return FALSE;
            }
            WN *initialize = WN_prev(statement);
            WN *capture = WN_next(statement);
            WN *check = capture == NULL ? NULL : WN_next(capture);
            WN *output_parm = WN_kid
                                  (statement, WN_kid_count(statement) - 1);
            WN *output_address = WN_kid0(output_parm);
            if (initialize == NULL || WN_operator(initialize) != OPR_STID ||
                WN_operator(WN_kid0(initialize)) != OPR_INTCONST ||
                WN_const_val(WN_kid0(initialize)) != 0 ||
                WN_operator(output_parm) != OPR_PARM ||
                WN_operator(output_address) != OPR_LDA ||
                WN_st_idx(initialize) != WN_st_idx(output_address) ||
                capture == NULL || WN_operator(capture) != OPR_STID ||
                WN_operator(WN_kid0(capture)) != OPR_LDID ||
                WN_st(WN_kid0(capture)) != Return_Val_Preg ||
                check == NULL || WN_operator(check) != OPR_IF) {
                fprintf(stderr, "standard output/status sequence changed\n");
                return FALSE;
            }
            WN *failure = WN_then(check);
            WN *status_use = WN_first(failure);
            WN *output_use = status_use == NULL ? NULL : WN_next(status_use);
            if (status_use == NULL || WN_operator(status_use) != OPR_EVAL ||
                WN_operator(WN_kid0(status_use)) != OPR_LDID ||
                WN_st_idx(WN_kid0(status_use)) != WN_st_idx(capture) ||
                output_use == NULL || WN_operator(output_use) != OPR_EVAL ||
                WN_operator(WN_kid0(output_use)) != OPR_LDID ||
                WN_st_idx(WN_kid0(output_use)) != WN_st_idx(initialize)) {
                fprintf(stderr, "standard failure path lost call results\n");
                return FALSE;
            }
        }
        else if (WN_operator(statement) == OPR_STID)
            ++status_count;
        else if (WN_operator(statement) == OPR_IF)
            ++if_count;
    }
    if (call_count != 2 || status_count != 4 || if_count != 2) {
        fprintf(stderr,
                "standard call sequence changed: calls=%u status=%u if=%u\n",
                call_count, status_count, if_count);
        return FALSE;
    }
    return TRUE;
}

static BOOL
Build_Descriptor_Selected_Call (WN *tree)
{
    const UINT32 static_ordinal = 23;
    const UINT32 operation_kind = 3;
    WN *body = WN_func_body(tree);
    SRCPOS source_position = Test_Source_Position();
    TY_IDX status_ty = MTYPE_To_TY(MTYPE_I4);
    TY_IDX u4_ty = MTYPE_To_TY(MTYPE_U4);
    TY_IDX model_ty = Create_Opaque_Handle_TY("open64_fhe_model_v1");
    TY_IDX ciphertext_ty =
        Create_Opaque_Handle_TY("open64_fhe_ciphertext_v1");
    TY_IDX descriptor_ty =
        Create_Opaque_Handle_TY("open64_fhe_operation_desc_v1");
    TY_IDX descriptor_output_ty = Make_Pointer_Type(descriptor_ty);
    TY_IDX ciphertext_output_ty = Make_Pointer_Type(ciphertext_ty);
    ST_IDX model_st = Create_Local_Handle_ST
                          ("fhe_model", model_ty, source_position);
    ST_IDX anchor_st = Create_Local_Handle_ST
                           ("fhe_anchor", ciphertext_ty, source_position);
    UINT32 formal_count = WN_num_formals(tree);
    UINT32 global_variable_count = Count_Global_Variables();

    VHO_FHE_STANDARD_PARM select_parameters[5];
    memset(select_parameters, 0, sizeof(select_parameters));
    select_parameters[0].formal_ty = model_ty;
    select_parameters[0].actual_ty = model_ty;
    select_parameters[0].actual = WN_CreateLdid
                                      (OPR_LDID, Pointer_Mtype,
                                       Pointer_Mtype, 0, model_st, model_ty);
    select_parameters[0].policy =
        VHO_FHE_STANDARD_PARM_BORROWED_READ_ONLY;
    select_parameters[1].formal_ty = ciphertext_ty;
    select_parameters[1].actual_ty = ciphertext_ty;
    select_parameters[1].actual = WN_CreateLdid
                                      (OPR_LDID, Pointer_Mtype,
                                       Pointer_Mtype, 0, anchor_st,
                                       ciphertext_ty);
    select_parameters[1].policy =
        VHO_FHE_STANDARD_PARM_BORROWED_READ_ONLY;
    select_parameters[2].formal_ty = u4_ty;
    select_parameters[2].actual_ty = u4_ty;
    select_parameters[2].actual = WN_Intconst(MTYPE_U4, static_ordinal);
    select_parameters[2].policy = VHO_FHE_STANDARD_PARM_BY_VALUE;
    select_parameters[3].formal_ty = u4_ty;
    select_parameters[3].actual_ty = u4_ty;
    select_parameters[3].actual = WN_Intconst(MTYPE_U4, operation_kind);
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
    select_spec.status_name = "fhe_select_status";
    select_spec.source_position = source_position;
    select_spec.build_failure = Build_Failure_Block;

    VHO_FHE_STANDARD_CALL_RESULT select_result;
    BOOL selected = VHO_FHE_Build_Standard_Call
                        (&select_spec, stderr, &select_result) &&
                    select_result.output_st != ST_IDX_ZERO &&
                    ST_type(select_result.output_st) == descriptor_ty &&
                    ST_Srcpos(St_Table[select_result.output_st]) ==
                        source_position &&
                    VHO_FHE_Commit_Standard_Call
                        (body, NULL, &select_result, stderr);
    for (UINT32 i = 0; i < 4; ++i)
        WN_DELETE_Tree(select_parameters[i].actual);
    if (!selected)
        return FALSE;

    VHO_FHE_STANDARD_PARM evaluate_parameters[4];
    memset(evaluate_parameters, 0, sizeof(evaluate_parameters));
    evaluate_parameters[0].formal_ty = model_ty;
    evaluate_parameters[0].actual_ty = model_ty;
    evaluate_parameters[0].actual = WN_CreateLdid
                                        (OPR_LDID, Pointer_Mtype,
                                         Pointer_Mtype, 0, model_st, model_ty);
    evaluate_parameters[0].policy =
        VHO_FHE_STANDARD_PARM_BORROWED_READ_ONLY;
    evaluate_parameters[1].formal_ty = ciphertext_ty;
    evaluate_parameters[1].actual_ty = ciphertext_ty;
    evaluate_parameters[1].actual = WN_CreateLdid
                                        (OPR_LDID, Pointer_Mtype,
                                         Pointer_Mtype, 0, anchor_st,
                                         ciphertext_ty);
    evaluate_parameters[1].policy =
        VHO_FHE_STANDARD_PARM_BORROWED_READ_ONLY;
    evaluate_parameters[2].formal_ty = descriptor_ty;
    evaluate_parameters[2].actual_ty = descriptor_ty;
    evaluate_parameters[2].actual = WN_CreateLdid
                                        (OPR_LDID, Pointer_Mtype,
                                         Pointer_Mtype, 0,
                                         select_result.output_st,
                                         descriptor_ty);
    evaluate_parameters[2].policy =
        VHO_FHE_STANDARD_PARM_BORROWED_READ_ONLY;
    evaluate_parameters[3].formal_ty = ciphertext_output_ty;
    evaluate_parameters[3].policy = VHO_FHE_STANDARD_PARM_OUTPUT_SLOT;
    evaluate_parameters[3].output_name = "fhe_selected_result";
    evaluate_parameters[3].output_ty = ciphertext_ty;

    VHO_FHE_STANDARD_CALL_SPEC evaluate_spec;
    memset(&evaluate_spec, 0, sizeof(evaluate_spec));
    evaluate_spec.function_name = "open64_fhe_bootstrap_v1";
    evaluate_spec.status_ty = status_ty;
    evaluate_spec.parameters = evaluate_parameters;
    evaluate_spec.parameter_count = 4;
    evaluate_spec.status_name = "fhe_evaluate_status";
    evaluate_spec.source_position = source_position;
    evaluate_spec.build_failure = Build_Failure_Block;

    VHO_FHE_STANDARD_CALL_RESULT evaluate_result;
    BOOL evaluated = VHO_FHE_Build_Standard_Call
                         (&evaluate_spec, stderr, &evaluate_result) &&
                     evaluate_result.output_st != ST_IDX_ZERO &&
                     ST_type(evaluate_result.output_st) == ciphertext_ty &&
                     ST_Srcpos(St_Table[evaluate_result.output_st]) ==
                         source_position &&
                     VHO_FHE_Commit_Standard_Call
                         (body, NULL, &evaluate_result, stderr);
    for (UINT32 i = 0; i < 3; ++i)
        WN_DELETE_Tree(evaluate_parameters[i].actual);
    if (!evaluated || WN_num_formals(tree) != formal_count ||
        Count_Global_Variables() != global_variable_count) {
        fprintf(stderr,
                "descriptor selection changed the PU or global interface\n");
        return FALSE;
    }
    return TRUE;
}

static BOOL
Verify_Descriptor_Selected_Call (WN *tree)
{
    WN *body = WN_func_body(tree);
    WN *select_call = NULL;
    WN *evaluate_call = NULL;
    UINT32 call_count = 0;
    for (WN *stmt = WN_first(body); stmt != NULL; stmt = WN_next(stmt)) {
        if (WN_operator(stmt) != OPR_CALL)
            continue;
        ++call_count;
        const char *name = ST_name(WN_st(stmt));
        if (strcmp(name, "open64_fhe_operation_desc_select_v1") == 0)
            select_call = stmt;
        else if (strcmp(name, "open64_fhe_bootstrap_v1") == 0)
            evaluate_call = stmt;
    }
    if (call_count != 2 || select_call == NULL || evaluate_call == NULL ||
        WN_kid_count(select_call) != 5 ||
        WN_kid_count(evaluate_call) != 4) {
        fprintf(stderr, "descriptor-selected call sequence changed\n");
        return FALSE;
    }

    WN *static_ordinal = WN_kid0(WN_kid(select_call, 2));
    WN *operation_kind = WN_kid0(WN_kid(select_call, 3));
    WN *descriptor_address = WN_kid0(WN_kid(select_call, 4));
    WN *descriptor_actual = WN_kid0(WN_kid(evaluate_call, 2));
    WN *select_capture = WN_next(select_call);
    WN *select_check = select_capture == NULL ? NULL :
                           WN_next(select_capture);
    WN *evaluate_initialize = WN_prev(evaluate_call);
    SRCPOS select_position = WN_Get_Linenum(select_call);
    SRCPOS evaluate_position = WN_Get_Linenum(evaluate_call);
    if (WN_operator(static_ordinal) != OPR_INTCONST ||
        WN_const_val(static_ordinal) != 23 ||
        WN_operator(operation_kind) != OPR_INTCONST ||
        WN_const_val(operation_kind) != 3 ||
        WN_operator(descriptor_address) != OPR_LDA ||
        WN_operator(descriptor_actual) != OPR_LDID ||
        WN_st_idx(descriptor_address) != WN_st_idx(descriptor_actual) ||
        select_capture == NULL || WN_operator(select_capture) != OPR_STID ||
        select_check == NULL || WN_operator(select_check) != OPR_IF ||
        evaluate_initialize == NULL ||
        WN_operator(evaluate_initialize) != OPR_STID ||
        WN_next(select_check) != evaluate_initialize ||
        SRCPOS_linenum(select_position) != 37 ||
        SRCPOS_linenum(evaluate_position) != 37) {
        fprintf(stderr,
                "descriptor selection lost ordering, identity, or source\n");
        return FALSE;
    }
    return TRUE;
}

static BOOL
Verify_Gate_Rejection (struct pu_info *pu_info)
{
    WN *native = DSL_WN_Create_Native
                     (OPR_DSLTENSORCONST, 1,
                      "value_kind=implicit_zero", NULL, 0);
    WN *block = WN_CreateBlock();
    WN_INSERT_BlockLast(block, WN_CreateEval(native));
    VHO_FHE_UNLOWERED_GATE_RESULT result;
    BOOL rejected = !VHO_FHE_Unlowered_Gate_Program_Unit
                         (pu_info, block, stderr, &result) &&
                    result.executable_dsl_carrier_count == 1 &&
                    result.error_count == 1;
    if (!rejected)
        fprintf(stderr,
                "unlowered gate changed: carriers=%u errors=%u\n",
                result.executable_dsl_carrier_count, result.error_count);
    WN_DELETE_Tree(block);
    return rejected;
}

static BOOL
Build_And_Lower_Resolved_Relu_Sequence
        (DSL_BUILDER_PROGRAM_UNIT pu, TY_IDX ciphertext_ty, TY_IDX model_ty,
         TY_IDX plaintext_ty, DSL_BUILDER_VALUE anchor,
         DSL_BUILDER_VALUE weight, DSL_BUILDER_VALUE bias,
         DSL_BUILDER_VALUE relu)
{
    DSL_RUNTIME_VALUE_PROJECTION_REQUEST value_requests[4];
    memset(value_requests, 0, sizeof(value_requests));
    value_requests[0].owner_pu_st = PU_Info_proc_sym(pu);
    value_requests[0].source_value_id = DSL_Builder_Get_Value_Image_Id(anchor);
    value_requests[0].expected_source_st =
        DSL_Builder_Get_Value_Result_Symbol(anchor);
    value_requests[0].expected_source_ty =
        ST_type(value_requests[0].expected_source_st);
    value_requests[0].handle_ty = ciphertext_ty;
    value_requests[0].binding_kind = DSL_RUNTIME_BINDING_INPUT_FORMAL;
    value_requests[0].formal_ordinal = 0;
    DSL_BUILDER_VALUE plain_values[2] = { weight, bias };
    for (UINT32 i = 0; i < 2; ++i) {
        value_requests[1 + i].owner_pu_st = PU_Info_proc_sym(pu);
        value_requests[1 + i].source_value_id =
            DSL_Builder_Get_Value_Image_Id(plain_values[i]);
        value_requests[1 + i].expected_source_st =
            DSL_Builder_Get_Value_Result_Symbol(plain_values[i]);
        value_requests[1 + i].expected_source_ty =
            ST_type(value_requests[1 + i].expected_source_st);
        value_requests[1 + i].handle_ty = plaintext_ty;
        value_requests[1 + i].binding_kind =
            DSL_RUNTIME_BINDING_INPUT_FORMAL;
        value_requests[1 + i].formal_ordinal = 1 + i;
    }
    value_requests[3].owner_pu_st = PU_Info_proc_sym(pu);
    value_requests[3].source_value_id = DSL_Builder_Get_Value_Image_Id(relu);
    value_requests[3].expected_source_st =
        DSL_Builder_Get_Value_Result_Symbol(relu);
    value_requests[3].expected_source_ty =
        ST_type(value_requests[3].expected_source_st);
    value_requests[3].handle_ty = ciphertext_ty;
    value_requests[3].binding_kind = DSL_RUNTIME_BINDING_LOCAL_VALUE;
    value_requests[3].formal_ordinal =
        DSL_RUNTIME_INTERFACE_INVALID_ORDINAL;

    DSL_RUNTIME_INTERFACE_PLAN runtime_plan;
    memset(&runtime_plan, 0, sizeof(runtime_plan));
    runtime_plan.values = value_requests;
    runtime_plan.value_count = 4;

    const char *roles[4] = {
        "fhe.model",
        "fhe.relu.coefficient.stage0",
        "fhe.relu.coefficient.stage1",
        "fhe.relu.coefficient.stage2"
    };
    DSL_RUNTIME_INPUT_REQUEST input_requests[4];
    DSL_RUNTIME_INPUT_BINDING_REQUEST binding_requests[4];
    memset(input_requests, 0, sizeof(input_requests));
    memset(binding_requests, 0, sizeof(binding_requests));
    for (UINT32 i = 0; i < 4; ++i) {
        TY_IDX handle_ty = i == 0 ? model_ty : plaintext_ty;
        input_requests[i].input_kind = DSL_RUNTIME_INPUT_OPAQUE_RESOURCE;
        input_requests[i].stable_role = roles[i];
        input_requests[i].handle_ty = handle_ty;
        binding_requests[i].owner_pu_st = PU_Info_proc_sym(pu);
        binding_requests[i].runtime_input_index = i;
        binding_requests[i].handle_ty = handle_ty;
        binding_requests[i].binding_kind =
            DSL_RUNTIME_INPUT_BINDING_ROOT_RESOURCE;
        binding_requests[i].semantic_role = roles[i];
        binding_requests[i].source_position = Test_Source_Position();
    }

    DSL_PROGRAM_INTERFACE_PLAN program_plan;
    memset(&program_plan, 0, sizeof(program_plan));
    program_plan.runtime_inputs = input_requests;
    program_plan.runtime_input_count = 4;
    program_plan.runtime_input_bindings = binding_requests;
    program_plan.runtime_input_binding_count = 4;

    DSL_PROGRAM_INTERFACE_RESULT interface_result;
    memset(&interface_result, 0, sizeof(interface_result));
    BOOL interface_valid = DSL_Program_Interface_Plan_Validate
        (&program_plan, &runtime_plan, stderr) && DSL_Builder_Select_PU(pu) &&
        DSL_Program_Interface_Apply_PU
        (pu, &program_plan, &runtime_plan, stderr, &interface_result);
    if (!interface_valid || interface_result.runtime_input_count != 4 ||
        interface_result.runtime_binding_count != 4 ||
        interface_result.canonical_projection_count != 4) {
        fprintf(stderr,
                "runtime interface changed: valid=%d inputs=%u bindings=%u "
                "projections=%u\n", interface_valid,
                interface_result.runtime_input_count,
                interface_result.runtime_binding_count,
                interface_result.canonical_projection_count);
        return FALSE;
    }

    VHO_FHE_RUNTIME_HANDLE_BINDING model;
    VHO_FHE_RUNTIME_HANDLE_BINDING projected_anchor;
    VHO_FHE_RUNTIME_HANDLE_BINDING projected_relu;
    VHO_FHE_RUNTIME_HANDLE_BINDING rejected_binding;
    if (!VHO_FHE_Runtime_Resolve_Role_Handle
             (pu, "fhe.model", stderr, &model) ||
        !VHO_FHE_Runtime_Resolve_Value_Handle
             (pu, value_requests[0].source_value_id, stderr,
              &projected_anchor) ||
        !VHO_FHE_Runtime_Resolve_Value_Handle
             (pu, value_requests[3].source_value_id, stderr,
              &projected_relu) ||
        model.handle_ty != model_ty ||
        projected_anchor.handle_ty != ciphertext_ty ||
        projected_relu.handle_ty != ciphertext_ty ||
        VHO_FHE_Runtime_Resolve_Role_Handle
            (pu, "fhe.missing", NULL, &rejected_binding) ||
        VHO_FHE_Runtime_Resolve_Value_Handle
            (pu, DSL_IR_VALUE_INVALID_ID, NULL, &rejected_binding))
    {
        fprintf(stderr, "runtime handle resolution evidence changed\n");
        return FALSE;
    }

    const DSL_IR_VALUE_ID conv_operands[3] = {
        value_requests[0].source_value_id,
        value_requests[1].source_value_id,
        value_requests[2].source_value_id
    };
    const DSL_IR_VALUE_ID residual_operands[2] = {
        value_requests[0].source_value_id,
        value_requests[0].source_value_id
    };
    const DSL_IR_VALUE_ID unary_operands[1] = {
        value_requests[0].source_value_id
    };
    const UINT32 operation_kinds[5] = {
        VHO_FHE_RUNTIME_OP_CONV2D_PLAIN,
        VHO_FHE_RUNTIME_OP_RESIDUAL_ADD,
        VHO_FHE_RUNTIME_OP_AVERAGE_POOL,
        VHO_FHE_RUNTIME_OP_LAYOUT_CONVERT,
        VHO_FHE_RUNTIME_OP_LINEAR_PLAIN
    };
    const DSL_IR_VALUE_ID *operation_operands[5] = {
        conv_operands, residual_operands, unary_operands, unary_operands,
        conv_operands
    };
    const UINT32 operation_operand_counts[5] = { 3, 2, 1, 1, 3 };
    const char *operation_functions[5] = {
        "open64_fhe_conv2d_plain_v1",
        "open64_fhe_residual_add_v1",
        "open64_fhe_average_pool_v1",
        "open64_fhe_layout_convert_v1",
        "open64_fhe_linear_plain_v1"
    };
    for (UINT32 i = 0; i < 5; ++i) {
        VHO_FHE_RUNTIME_OPERATION_REQUEST operation;
        memset(&operation, 0, sizeof(operation));
        operation.operation_kind = operation_kinds[i];
        operation.static_ordinal = 50 + i;
        operation.operand_value_ids = operation_operands[i];
        operation.operand_count = operation_operand_counts[i];
        VHO_FHE_RUNTIME_CALL_SEQUENCE operation_sequence;
        if (!VHO_FHE_Runtime_Build_Operation_Sequence
                 (pu, &operation, Test_Source_Position(),
                  Build_Failure_Block, NULL, stderr, &operation_sequence) ||
            operation_sequence.block == NULL ||
            operation_sequence.standard_call_count != 2 ||
            operation_sequence.output_handle_count != 2 ||
            operation_sequence.status_check_count != 2 ||
            ST_type(operation_sequence.output_st) != ciphertext_ty) {
            fprintf(stderr, "runtime operation sequence %u changed\n", i);
            return FALSE;
        }
        UINT32 call_count = 0;
        BOOL saw_selector = FALSE;
        BOOL saw_evaluation = FALSE;
        for (WN *stmt = WN_first(operation_sequence.block); stmt != NULL;
             stmt = WN_next(stmt)) {
            if (WN_operator(stmt) != OPR_CALL)
                continue;
            ++call_count;
            const char *name = ST_name(WN_st(stmt));
            if (strcmp(name, "open64_fhe_operation_desc_select_v1") == 0) {
                WN *ordinal = WN_kid0(WN_kid(stmt, 2));
                WN *kind = WN_kid0(WN_kid(stmt, 3));
                saw_selector = WN_operator(ordinal) == OPR_INTCONST &&
                    WN_const_val(ordinal) == 50 + i &&
                    WN_operator(kind) == OPR_INTCONST &&
                    WN_const_val(kind) == operation_kinds[i];
            }
            else if (strcmp(name, operation_functions[i]) == 0)
                saw_evaluation = TRUE;
        }
        BOOL valid_operation = call_count == 2 && saw_selector &&
                               saw_evaluation;
        WN_DELETE_Tree(operation_sequence.block);
        if (!valid_operation) {
            fprintf(stderr, "runtime operation ABI %u changed\n", i);
            return FALSE;
        }
    }

    VHO_FHE_RUNTIME_OPERATION_REQUEST unsupported;
    memset(&unsupported, 0, sizeof(unsupported));
    unsupported.operation_kind = VHO_FHE_RUNTIME_OP_RELU_POLY_STAGE;
    unsupported.static_ordinal = 60;
    unsupported.operand_value_ids = unary_operands;
    unsupported.operand_count = 1;
    VHO_FHE_RUNTIME_CALL_SEQUENCE rejected_sequence;
    if (VHO_FHE_Runtime_Build_Operation_Sequence
            (pu, &unsupported, Test_Source_Position(), Build_Failure_Block,
             NULL, NULL, &rejected_sequence)) {
        fprintf(stderr, "specialized ReLU operation was admitted generically\n");
        return FALSE;
    }

    VHO_FHE_RUNTIME_CALL_SEQUENCE identity_sequence;
    WN *identity_result = NULL;
    if (!VHO_FHE_Runtime_Build_Identity_Sequence
             (pu, value_requests[0].source_value_id, stderr,
              &identity_sequence) ||
        identity_sequence.block == NULL ||
        identity_sequence.output_st != projected_anchor.handle_st ||
        identity_sequence.standard_call_count != 0 ||
        identity_sequence.output_handle_count != 0 ||
        identity_sequence.status_check_count != 0 ||
        !VHO_FHE_Runtime_Finalize_Projected_Output
             (&projected_relu, Test_Source_Position(), &identity_sequence,
              &identity_result) ||
        identity_result == NULL || WN_first(identity_sequence.block) !=
            identity_result || WN_next(identity_result) != NULL) {
        fprintf(stderr, "runtime identity sequence changed\n");
        return FALSE;
    }
    WN_DELETE_Tree(identity_sequence.block);

    VHO_FHE_RUNTIME_CALL_SEQUENCE sequence;
    if (!VHO_FHE_Runtime_Build_Relu_Sequence
             (pu, value_requests[0].source_value_id, 31,
              Test_Source_Position(), Build_Failure_Block, NULL,
              stderr, &sequence) ||
        sequence.block == NULL || sequence.output_st == ST_IDX_ZERO ||
        sequence.standard_call_count != 12 ||
        sequence.output_handle_count != 12 ||
        sequence.status_check_count != 12 ||
        ST_type(sequence.output_st) != ciphertext_ty)
    {
        fprintf(stderr,
                "ReLU sequence changed: calls=%u outputs=%u checks=%u\n",
                sequence.standard_call_count, sequence.output_handle_count,
                sequence.status_check_count);
        return FALSE;
    }

    UINT32 call_count = 0;
    UINT32 selector_count = 0;
    BOOL saw_select = FALSE;
    BOOL saw_bootstrap = FALSE;
    BOOL saw_normalize = FALSE;
    UINT32 poly_stage_count = 0;
    BOOL saw_reconstruct = FALSE;
    for (WN *stmt = WN_first(sequence.block); stmt != NULL;
         stmt = WN_next(stmt)) {
        if (WN_operator(stmt) != OPR_CALL)
            continue;
        ++call_count;
        const char *name = ST_name(WN_st(stmt));
        if (strcmp(name, "open64_fhe_operation_desc_select_v1") == 0) {
            saw_select = TRUE;
            WN *ordinal = WN_kid0(WN_kid(stmt, 2));
            WN *kind = WN_kid0(WN_kid(stmt, 3));
            const UINT32 expected_kind[6] = {
                3, 4, 5, 5, 5, 6
            };
            if (WN_operator(ordinal) != OPR_INTCONST ||
                selector_count >= 6 ||
                WN_const_val(ordinal) != 31 + selector_count ||
                WN_operator(kind) != OPR_INTCONST ||
                WN_const_val(kind) != expected_kind[selector_count])
                return FALSE;
            ++selector_count;
        }
        else if (strcmp(name, "open64_fhe_bootstrap_v1") == 0)
            saw_bootstrap = TRUE;
        else if (strcmp(name, "open64_fhe_relu_normalize_v1") == 0)
            saw_normalize = TRUE;
        else if (strcmp(name, "open64_fhe_relu_poly_stage_v1") == 0)
            ++poly_stage_count;
        else if (strcmp(name, "open64_fhe_relu_reconstruct_v1") == 0)
            saw_reconstruct = TRUE;
    }
    BOOL valid = call_count == 12 && selector_count == 6 && saw_select &&
                 saw_bootstrap &&
                 saw_normalize && poly_stage_count == 3 && saw_reconstruct;
    WN *result_definition = NULL;
    if (valid && !VHO_FHE_Runtime_Finalize_Projected_Output
                     (&projected_relu, Test_Source_Position(), &sequence,
                      &result_definition))
        valid = FALSE;

    WN *body = WN_func_body(PU_Info_tree_ptr(pu));
    DSL_IR_NATIVE_VALUE_LOWER_REQUEST request;
    memset(&request, 0, sizeof(request));
    request.pu_root = PU_Info_tree_ptr(pu);
    request.containing_block = body;
    request.native_definition = relu;
    request.source_value_id = value_requests[3].source_value_id;
    request.expected_operator = OPR_DSLRELU;
    request.expected_version = 2;
    request.mode = DSL_IR_NATIVE_LOWER_COMPUTED_STANDARD_BLOCK;
    request.relation.relation_kind =
        DSL_IR_LOWER_RELATION_RUNTIME_VALUE_PROJECTION;
    DSL_RUNTIME_VALUE_PROJECTION_RECORD relu_projection;
    memset(&relu_projection, 0, sizeof(relu_projection));
    if (valid && !DSL_Runtime_Interface_Image_Find_Value
                     (PU_Info_proc_sym(pu), request.source_value_id,
                      &relu_projection))
        valid = FALSE;
    request.relation.value_projection_id = relu_projection.id;
    request.standard_block = sequence.block;
    request.result_handle_definition = result_definition;
    DSL_IR_NATIVE_VALUE_LOWER_RESULT lower_result;
    if (valid &&
        (!DSL_IR_Lower_Native_Values_To_Standard_Blocks
             (PU_Info_proc_sym(pu), &request, 1, stderr, &lower_result) ||
         lower_result.source_value_id != request.source_value_id ||
         lower_result.mode != DSL_IR_NATIVE_LOWER_COMPUTED_STANDARD_BLOCK ||
         lower_result.relation_kind !=
             DSL_IR_LOWER_RELATION_RUNTIME_VALUE_PROJECTION ||
         lower_result.handle_st != projected_relu.handle_st ||
         lower_result.inserted_statement_count == 0))
        valid = FALSE;
    if (valid)
        sequence.block = NULL;
    if (sequence.block != NULL)
        WN_DELETE_Tree(sequence.block);
    if (!valid)
        fprintf(stderr, "atomic ReLU lowering evidence changed\n");
    return valid;
}

static BOOL
Write_Review_Trace (WN *tree)
{
    const char *path = getenv("OPEN64_FHE_RUNTIME_LOWER_TRACE");
    if (path == NULL || path[0] == '\0')
        return TRUE;
    FILE *trace = fopen(path, "w");
    if (trace == NULL)
        return FALSE;
    fprintf(trace,
            "# SYNC-5 standard-WHIRL call construction before semantic "
            "runtime lowering\n");
    fdump_tree(trace, tree);
    return fclose(trace) == 0;
}

static BOOL
Write_Review_Artifact
        (DSL_BUILDER_PROGRAM_UNIT *program_units, UINT32 program_unit_count)
{
    const char *path = getenv("OPEN64_FHE_RUNTIME_LOWER_ARTIFACT");
    if (path == NULL || path[0] == '\0')
        return TRUE;
    if (program_units == NULL || program_unit_count == 0)
        return FALSE;
    Irb_File_Name = (char *)path;
    if (Open_Output_Info(Irb_File_Name) == NULL)
        return FALSE;
    for (UINT32 i = 0; i < program_unit_count; ++i) {
        if (!DSL_Builder_Select_PU(program_units[i])) {
            Close_Output_Info();
            return FALSE;
        }
        Write_PU_Info(program_units[i]);
    }
    Write_Global_Info(program_units[0]);
    Close_Output_Info();
    return TRUE;
}

int
main (void)
{
    Initialize_Test_Context();
    if (!DSL_Builder_Begin_Program() ||
        !DSL_Opcode_Register_Common_Substrate())
        return 1;
    DSL_BUILDER_PROGRAM_UNIT pu =
        DSL_Builder_Create_Minimal_PU("fhe_runtime_lower_contract");
    if (pu == NULL || !DSL_Builder_Select_PU(pu))
        return 1;
    test_source_file_id = DSL_Builder_Register_Source_File(pu, __FILE__);
    if (test_source_file_id == 0)
        return 1;
    WN *tree = PU_Info_tree_ptr(pu);
    WN *body = WN_func_body(tree);
    if (!Build_Two_Calls(body) || !Verify_Two_Calls(body) ||
        !Write_Review_Trace(tree) ||
        !Verify_Gate_Rejection(pu)) {
        fprintf(stderr, "standard FHE WHIRL construction changed\n");
        return 1;
    }

    DSL_BUILDER_PROGRAM_UNIT selector_pu =
        DSL_Builder_Create_Minimal_PU("fhe_descriptor_selection_contract");
    if (selector_pu == NULL || !DSL_Builder_Select_PU(selector_pu))
        return 1;
    WN *selector_tree = PU_Info_tree_ptr(selector_pu);
    if (!Build_Descriptor_Selected_Call(selector_tree) ||
        !Verify_Descriptor_Selected_Call(selector_tree) ||
        !Write_Review_Trace(selector_tree)) {
        fprintf(stderr, "FHE descriptor selection contract changed\n");
        return 1;
    }

    DSL_BUILDER_TENSOR_TYPE_CORE type_core;
    DSL_BUILDER_TENSOR_DESCRIPTOR tensor_descriptor;
    memset(&type_core, 0, sizeof(type_core));
    memset(&tensor_descriptor, 0, sizeof(tensor_descriptor));
    type_core.kind = "tensor";
    type_core.dtype = "float32";
    type_core.rank = 1;
    type_core.logical_shape = "[8]";
    tensor_descriptor.type_core = type_core;
    tensor_descriptor.traits.traits = "activation";
    tensor_descriptor.representation.layout = "packed";
    tensor_descriptor.representation.sharding = "replicated";
    tensor_descriptor.representation.placement = "host";
    tensor_descriptor.representation.memory = "contiguous";
    tensor_descriptor.representation.quantization = "none";
    TY_IDX source_tensor_ty = DSL_Builder_Intern_Tensor_Type
        ("fhe_runtime_source_tensor", MTYPE_To_TY(MTYPE_F4),
         &tensor_descriptor);
    DSL_BUILDER_PROGRAM_UNIT binding_pu =
        DSL_Builder_Create_Minimal_PU("fhe_runtime_binding_contract");
    DSL_BUILDER_PU_SOURCE_IDENTITY binding_identity;
    memset(&binding_identity, 0, sizeof(binding_identity));
    binding_identity.canonical_definition_name =
        "FHERuntimeBindingContract";
    binding_identity.defining_module = "fhe_runtime_lower_contract_test";
    binding_identity.defining_file = __FILE__;
    binding_identity.defining_line = 37;
    DSL_BUILDER_SOURCE_POSITION binding_position;
    memset(&binding_position, 0, sizeof(binding_position));
    binding_position.file_id = DSL_Builder_Register_Source_File
                                   (binding_pu, __FILE__);
    binding_position.line = 37;
    binding_position.column = 5;
    binding_position.statement_begin = 1;
    DSL_BUILDER_VALUE binding_anchor = DSL_Builder_Declare_PU_Formal
        (binding_pu, "ciphertext_anchor", 0, source_tensor_ty,
         &binding_position);
    DSL_BUILDER_VALUE binding_weight = DSL_Builder_Declare_PU_Formal
        (binding_pu, "conv_weight", 1, source_tensor_ty,
         &binding_position);
    DSL_BUILDER_VALUE binding_bias = DSL_Builder_Declare_PU_Formal
        (binding_pu, "conv_bias", 2, source_tensor_ty,
         &binding_position);
    DSL_BUILDER_VALUE relu_kids[1] = { binding_anchor };
    DSL_BUILDER_VALUE binding_relu = DSL_Builder_Create_Operator_With_Result
        (DSL_Opcode_Find(DSL_Domain_Find("common"), "common.relu", 2), 2,
         relu_kids, 1, NULL, 0, "relu_result", source_tensor_ty);
    TY_IDX binding_model_ty = Create_Opaque_Handle_TY
                                  ("open64_fhe_model_v1");
    TY_IDX binding_ciphertext_ty = Create_Opaque_Handle_TY
                                       ("open64_fhe_ciphertext_v1");
    TY_IDX binding_plaintext_ty = Create_Opaque_Handle_TY
                                      ("open64_fhe_plain_tensor_v1");
    BOOL binding_setup = source_tensor_ty != TY_IDX_ZERO &&
        binding_pu != NULL && binding_position.file_id != 0 &&
        binding_anchor != NULL && binding_weight != NULL &&
        binding_bias != NULL && binding_relu != NULL;
    if (binding_setup)
        binding_setup = DSL_Builder_Set_PU_Source_Identity
                            (binding_pu, &binding_identity);
    if (binding_setup)
        binding_setup = DSL_Builder_Set_Value_Source_Position
                            (binding_relu, &binding_position);
    if (binding_setup)
        binding_setup = DSL_Builder_Append_PU_Value(binding_pu, binding_relu);
    if (binding_setup)
        binding_setup = DSL_Builder_Return_PU_Values(binding_pu, NULL, 0);
    if (!binding_setup) {
        fprintf(stderr, "FHE runtime binding fixture setup changed\n");
        return 1;
    }
    if (!Build_And_Lower_Resolved_Relu_Sequence
            (binding_pu, binding_ciphertext_ty, binding_model_ty,
             binding_plaintext_ty, binding_anchor, binding_weight,
             binding_bias, binding_relu)) {
        fprintf(stderr, "FHE runtime ReLU lowering changed\n");
        return 1;
    }

    DSL_BUILDER_PROGRAM_UNIT runtime_pu =
        DSL_Builder_Create_Minimal_PU("fhe_runtime_lower_phase_contract");
    if (runtime_pu == NULL || !DSL_Builder_Select_PU(runtime_pu))
        return 1;
    tree = PU_Info_tree_ptr(runtime_pu);

    VHO_FHE_Enable_Runtime_Lowering = FALSE;
    VHO_FHE_RUNTIME_LOWER_RESULT result;
    if (!VHO_FHE_Runtime_Lower_Program_Unit
             (runtime_pu, &tree, stderr, &result) ||
        result.runtime_lowering_pass_count != 0)
        return 1;

    VHO_FHE_Enable_Runtime_Lowering = TRUE;
    VHO_FHE_Runtime_Lowering_Checkpoint_Output =
        (char *)"/tmp/fhe_runtime.mid.B";
    if (!VHO_FHE_Runtime_Lower_Register_Semantic_Gatekeeper
             (Observe_Runtime_Gate) ||
        !VHO_FHE_Runtime_Lower_Register_Pass(Observe_Runtime_Pass) ||
        VHO_FHE_Runtime_Lower_Register_Pass(Observe_Runtime_Pass))
        return 1;
    observed_gate_count = 0;
    observed_pass_count = 0;
    observed_checkpoint_path = NULL;
    if (!VHO_FHE_Runtime_Lower_Program_Unit
             (runtime_pu, &tree, stderr, &result) ||
        result.semantic_gatekeeper_count != 2 ||
        result.runtime_lowering_pass_count != 1 ||
        result.standard_call_count != 2 ||
        result.output_handle_count != 2 ||
        result.status_check_count != 2 || result.error_count != 0)
        return 1;

    DSL_BUILDER_PROGRAM_UNIT second_runtime_pu =
        DSL_Builder_Create_Minimal_PU("fhe_runtime_lower_phase_contract_2");
    if (second_runtime_pu == NULL ||
        !DSL_Builder_Select_PU(second_runtime_pu))
        return 1;
    tree = PU_Info_tree_ptr(second_runtime_pu);
    if (!VHO_FHE_Runtime_Lower_Program_Unit
             (second_runtime_pu, &tree, stderr, &result) ||
        observed_gate_count != 4 || observed_pass_count != 2 ||
        result.semantic_gatekeeper_count != 2 ||
        result.runtime_lowering_pass_count != 1 ||
        observed_checkpoint_path !=
            VHO_FHE_Runtime_Lowering_Checkpoint_Output ||
        result.error_count != 0)
        return 1;

    DSL_BUILDER_PROGRAM_UNIT program_units[5] = {
        pu, selector_pu, binding_pu, runtime_pu, second_runtime_pu
    };
    if (!Write_Review_Artifact(program_units, 5)) {
        fprintf(stderr, "FHE runtime lowering artifact write failed\n");
        return 1;
    }

    DSL_Builder_Abort_Program();
    VHO_FHE_Runtime_Lower_Reset_Passes();
    VHO_FHE_Unlowered_Gate_Reset();
    printf("FHE runtime lowering contract passed\n");
    return 0;
}
