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
#include "fhe_standard_whirl.h"
#include "fhe_unlowered_gate.h"
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
    USRCPOS_filenum(position) = 1;
    USRCPOS_linenum(position) = 37;
    USRCPOS_column(position) = 5;
    USRCPOS_stmt_begin(position) = 1;
    return USRCPOS_srcpos(position);
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

int
main (void)
{
    Initialize_Test_Context();
    if (!DSL_Builder_Begin_Program())
        return 1;
    DSL_BUILDER_PROGRAM_UNIT pu =
        DSL_Builder_Create_Minimal_PU("fhe_runtime_lower_contract");
    if (pu == NULL || !DSL_Builder_Select_PU(pu))
        return 1;
    WN *tree = PU_Info_tree_ptr(pu);
    WN *body = WN_func_body(tree);
    if (!Build_Two_Calls(body) || !Verify_Two_Calls(body) ||
        !Write_Review_Trace(tree) ||
        !Verify_Gate_Rejection(pu)) {
        fprintf(stderr, "standard FHE WHIRL construction changed\n");
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

    DSL_Builder_Abort_Program();
    VHO_FHE_Runtime_Lower_Reset_Passes();
    VHO_FHE_Unlowered_Gate_Reset();
    printf("FHE runtime lowering contract passed\n");
    return 0;
}
