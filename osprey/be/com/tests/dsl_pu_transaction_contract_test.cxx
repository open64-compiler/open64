/*
 * Copyright (C) 2026 Open64 Project
 *
 * Linked physical/logical PU clone contract.  This exercises the be/com
 * service with a real frontend-produced tree but keeps builder APIs out of
 * be.so.  See doc/FHE-SYNC6-PU-SPECIALIZATION-TRANSACTION.md.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "defs.h"
#include "config.h"
#include "config_targ_opt.h"
#include "controls.h"
#include "dsl_builder.h"
#include "dsl_ir_image.h"
#include "dsl_opcode.h"
#include "dsl_pu_transaction.h"
#include "dsl_region.h"
#include "dwarf_DST_mem.h"
#include "erglob.h"
#include "errors.h"
#include "err_host.tab"
#include "glob.h"
#include "ir_reader.h"
#include "ir_bread.h"
#include "ir_bwrite.h"
#include "mempool.h"
#include "strtab.h"
#include "symtab.h"
#include "targ_const.h"
#include "wn.h"

BOOL Run_vsaopt = FALSE;
INT8 Debug_Level = 0;

void Signal_Cleanup(INT sig) {}

const char *
Host_Format_Parm(INT kind, MEM_PTR parm)
{
    return "";
}

static void
DSL_PU_Transaction_Test_Init (BOOL producer)
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
    if (producer) {
        Initialize_Symbol_Tables(TRUE);
        DST_Init(NULL, 0);
    }
}

static BOOL
DSL_PU_Transaction_Test_Plan
        (PU_Info *, DSL_PU_TRANSACTION_PLAN *, void **, FILE *)
{
    return FALSE;
}

static void
DSL_PU_Transaction_Test_Release (void *)
{
}

static BOOL
DSL_PU_Transaction_Write_Artifact
        (PU_Info *source, PU_Info *clone, PU_Info *caller)
{
    const char *artifact = getenv("OPEN64_DSL_PU_TRANSACTION_ARTIFACT");
    if (artifact == NULL || artifact[0] == '\0')
        return TRUE;
    PU_Info_next(clone) = caller;
    PU_Info_next(caller) = NULL;
    Irb_File_Name = const_cast<char *>(artifact);
    if (Open_Output_Info(Irb_File_Name) == NULL)
        return FALSE;
    PU_Info *program_units[3] = { source, clone, caller };
    for (UINT32 i = 0; i < 3; ++i) {
        PU_Info *pu = program_units[i];
        if (pu == clone) {
            Restore_Local_Symtab(clone);
            Current_pu = &PU_Info_pu(clone);
            Current_Map_Tab = PU_Info_maptab(clone);
        } else if (!DSL_Builder_Select_PU(pu)) {
            Close_Output_Info();
            return FALSE;
        }
        Write_PU_Info(pu);
    }
    Write_Global_Info(source);
    Close_Output_Info();
    return TRUE;
}

static DSL_IR_VALUE_ID
DSL_PU_Transaction_Find_Named_Value
        (const char *owner, const char *name)
{
    for (UINT32 i = 1; i <= DSL_IR_Image_Value_Count(); ++i) {
        DSL_IR_VALUE_RECORD value;
        if (!DSL_IR_Image_Get_Value(i, &value) ||
            value.name == STR_IDX_ZERO ||
            value.metadata == STR_IDX_ZERO)
            continue;
        if (strcmp(Index_To_Str(value.name), name) == 0 &&
            strcmp(Index_To_Str(value.metadata), owner) == 0)
            return i;
    }
    return DSL_IR_VALUE_INVALID_ID;
}

static int
DSL_PU_Transaction_Reopen_Artifact (const char *path)
{
    if (Open_Input_Info(const_cast<char *>(path)) == NULL)
        return 1;
    Initialize_Symbol_Tables(FALSE);
    New_Scope(GLOBAL_SYMTAB, Malloc_Mem_Pool, FALSE);
    PU_Info *program = Read_Global_Info(NULL);
    if (program == NULL)
        return 1;
    IR_reader_init();
    UINT32 verified = 0;
    for (PU_Info *pu = program; pu != NULL; pu = PU_Info_next(pu)) {
        MEM_POOL_Push(MEM_pu_nz_pool_ptr);
        MEM_POOL_Push(MEM_pu_pool_ptr);
        Read_Local_Info(MEM_pu_nz_pool_ptr, pu);
        const char *owner_name = ST_name
            (St_Table[PU_Info_proc_sym(pu)]);
        if (strcmp(owner_name, "pu_transaction_source") == 0) {
            DSL_IR_VALUE_ID root_value =
                DSL_PU_Transaction_Find_Named_Value
                    ("owner_pu=pu_transaction_source",
                     "__dsl_root_bound");
            TCON_IDX tcon = TCON_IDX_ZERO;
            if (root_value == DSL_IR_VALUE_INVALID_ID ||
                !DSL_PU_Transaction_Scalar_TCON_Active
                    (pu, root_value, &tcon, stderr) ||
                Targ_To_Host_Float(Tcon_Table[tcon]) != 1.5)
                return 1;
            ++verified;
        } else if (strcmp(owner_name, "pu_transaction_caller") == 0) {
            const double expected[2] = { 2.0, 3.0 };
            DSL_IR_VALUE_ID root_value =
                DSL_PU_Transaction_Find_Named_Value
                    ("owner_pu=pu_transaction_source",
                     "__dsl_root_bound");
            TCON_IDX wrong_owner_tcon = TCON_IDX_ZERO;
            if (root_value == DSL_IR_VALUE_INVALID_ID ||
                DSL_PU_Transaction_Scalar_TCON_Active
                    (pu, root_value, &wrong_owner_tcon, NULL))
                return 1;
            for (UINT32 i = 0; i < 2; ++i) {
                DSL_CALL_ARGUMENT_RECORD argument;
                TCON_IDX tcon = TCON_IDX_ZERO;
                if (!DSL_Call_ABI_Image_Find_Argument_By_Id
                        (i + 1, 1, &argument) ||
                    !DSL_PU_Transaction_Scalar_TCON_Active
                        (pu, argument.argument_value_id,
                         &tcon, stderr) ||
                    Targ_To_Host_Float(Tcon_Table[tcon]) != expected[i])
                    return 1;
                ++verified;
            }
        }
    }
    if (verified != 3)
        return 1;
    printf("DSL PU transaction mapped scalar TCON proof passed\n");
    return 0;
}

int
main (int argc, char **argv)
{
#define CLONE_CHECK(condition, stage) \
    do { \
        if (!(condition)) { \
            fprintf(stderr, "DSL PU transaction failed: %s\n", stage); \
            return 1; \
        } \
    } while (0)
    DSL_PU_Transaction_Test_Init
        (!(argc == 3 && strcmp(argv[1], "--reopen") == 0));
    if (argc == 3 && strcmp(argv[1], "--reopen") == 0)
        return DSL_PU_Transaction_Reopen_Artifact(argv[2]);
    DSL_PU_TRANSACTION_POLICY policy;
    memset(&policy, 0, sizeof(policy));
    CLONE_CHECK(!DSL_PU_Transaction_Get_Policy(&policy),
                "policy absent by default");
    policy.build_plan = DSL_PU_Transaction_Test_Plan;
    policy.release = DSL_PU_Transaction_Test_Release;
    CLONE_CHECK(DSL_PU_Transaction_Register_Policy(&policy) &&
                !DSL_PU_Transaction_Register_Policy(&policy),
                "single policy registration");
    DSL_PU_TRANSACTION_POLICY registered;
    CLONE_CHECK(DSL_PU_Transaction_Get_Policy(&registered) &&
                registered.build_plan == policy.build_plan &&
                DSL_PU_Transaction_Register_Policy(NULL) &&
                !DSL_PU_Transaction_Get_Policy(&registered),
                "policy retrieval and reset");
    CLONE_CHECK(DSL_Builder_Begin_Program(), "program initialization");
    DSL_Opcode_Register_Common_Substrate();

    DSL_BUILDER_TENSOR_DESCRIPTOR descriptor;
    memset(&descriptor, 0, sizeof(descriptor));
    descriptor.type_core.kind = "tensor";
    descriptor.type_core.dtype = "float32";
    descriptor.type_core.rank = 1;
    descriptor.type_core.logical_shape = "[2]";
    TY_IDX tensor_ty = DSL_Builder_Intern_Tensor_Type
                           ("pu_transaction_tensor", MTYPE_To_TY(MTYPE_F4),
                            &descriptor);
    PU_Info *source = DSL_Builder_Create_Minimal_PU
                          ("pu_transaction_source");
    UINT32 file_id = DSL_Builder_Register_Source_File(source, __FILE__);
    DSL_BUILDER_PU_SOURCE_IDENTITY identity;
    memset(&identity, 0, sizeof(identity));
    identity.canonical_definition_name = "PUTransaction.source";
    identity.defining_module = "dsl_pu_transaction_contract_test";
    identity.defining_file = __FILE__;
    identity.defining_line = __LINE__;
    CLONE_CHECK(tensor_ty != TY_IDX_ZERO && source != NULL && file_id != 0 &&
                DSL_Builder_Set_PU_Source_Identity(source, &identity),
                "source identity");

    DSL_BUILDER_SOURCE_POSITION position;
    memset(&position, 0, sizeof(position));
    position.file_id = file_id;
    position.line = __LINE__;
    position.column = 1;
    position.statement_begin = 1;
    WN *input = DSL_Builder_Declare_PU_Formal
                    (source, "input", 0, tensor_ty, &position);
    WN *result = DSL_Builder_Declare_PU_Result
                     (source, "output", 0, tensor_ty,
                      DSL_PU_RESULT_TENSOR, &position);
    WN *relu = DSL_Builder_Create_Operator_With_Result
                   (DSL_Opcode_Find(DSL_Domain_Find("common"),
                                    "common.relu", 2),
                    2, &input, 1, NULL, 0, "result", tensor_ty);
    CLONE_CHECK(input != NULL && result != NULL && relu != NULL &&
                DSL_Builder_Append_PU_Value(source, relu),
                "source value");
    DSL_REGION region = DSL_Region_Create
                            (source, NULL, "cnn.basic_block", 1);
    CLONE_CHECK(region != NULL && DSL_Region_Append_To_PU(region) &&
                DSL_Builder_Return_PU_Values(source, &relu, 1) &&
                DSL_Region_Verify_PU(source, stderr), "source REGION");

    PU_Info *caller = DSL_Builder_Create_Minimal_PU
                          ("pu_transaction_caller");
    UINT32 caller_file = DSL_Builder_Register_Source_File(caller, __FILE__);
    identity.canonical_definition_name = "PUTransaction.caller";
    identity.defining_line = __LINE__;
    CLONE_CHECK(caller != NULL && caller_file != 0 &&
                DSL_Builder_Set_PU_Source_Identity(caller, &identity),
                "caller identity");
    position.file_id = caller_file;
    position.line = __LINE__;
    WN *caller_input = DSL_Builder_Declare_PU_Formal
                           (caller, "caller_input", 0, tensor_ty, &position);
    DSL_BUILDER_CALLSITE_INFO call_info;
    memset(&call_info, 0, sizeof(call_info));
    call_info.canonical_class_name = "PUTransaction";
    call_info.instance_path = "caller.first";
    call_info.context_identity = "caller.first.context";
    call_info.call_ordinal = 1;
    call_info.source_position = position;
    const char *first_result_name = "first_result";
    WN *first_call = DSL_Builder_Create_PU_Call
                         (caller, source, &caller_input, 1,
                          &first_result_name, 1, &call_info);
    call_info.instance_path = "caller.second";
    call_info.context_identity = "caller.second.context";
    call_info.call_ordinal = 2;
    call_info.source_position.line = __LINE__;
    const char *second_result_name = "second_result";
    WN *second_call = DSL_Builder_Create_PU_Call
                          (caller, source, &caller_input, 1,
                           &second_result_name, 1, &call_info);
    DSL_CALLSITE_METADATA_RECORD first_site;
    DSL_CALLSITE_METADATA_RECORD second_site;
    CLONE_CHECK(caller_input != NULL && first_call != NULL &&
                second_call != NULL &&
                DSL_Builder_Set_PU_Call_Argument_Role
                    (first_call, 0, 0, "common.input") &&
                DSL_Builder_Set_PU_Call_Argument_Role
                    (second_call, 0, 0, "common.input") &&
                DSL_Call_Image_Find_Callsite(first_call, &first_site) &&
                DSL_Call_Image_Find_Callsite(second_call, &second_site) &&
                first_site.id != second_site.id,
                "two managed caller contexts");

    CLONE_CHECK(DSL_Builder_Select_PU(source), "source activation");
    PU_Info_next(source) = caller;
    PU_Info_next(caller) = NULL;
    if (PU_Info_symtab_ptr(source) == NULL)
        Save_Local_Symtab(CURRENT_SYMTAB, source);
    DSL_PU_TRANSACTION_FORMAL_REQUEST plan_formal;
    memset(&plan_formal, 0, sizeof(plan_formal));
    plan_formal.name = "bound_b";
    plan_formal.semantic_role = "fhe.relu.bound";
    plan_formal.ty = MTYPE_To_TY(MTYPE_F8);
    plan_formal.source_position =
        ST_Srcpos(St_Table[WN_st_idx(WN_formal(PU_Info_tree_ptr(source),
                                              0))]);
    DSL_PU_TRANSACTION_VARIANT_REQUEST variants[2];
    memset(variants, 0, sizeof(variants));
    variants[0].source_pu_st = PU_Info_proc_sym(source);
    variants[0].signature_bytes =
        reinterpret_cast<const unsigned char *>("a");
    variants[0].signature_size = 1;
    variants[0].signature_sha256 =
        "ca978112ca1bbdcafac231b39a23dc4da786eff8147c4e72b9807785afee48bb";
    variants[0].use_existing_pu = TRUE;
    variants[1].source_pu_st = PU_Info_proc_sym(source);
    variants[1].clone_name = "pu_transaction_variant";
    variants[1].signature_bytes =
        reinterpret_cast<const unsigned char *>("b");
    variants[1].signature_size = 1;
    variants[1].signature_sha256 =
        "3e23e8160039594a33894f6564e1b1348bbd7a0088d42c4acb73eeaed59c009d";
    variants[1].formals = &plan_formal;
    variants[1].formal_count = 1;
    DSL_PU_TRANSACTION_ACTUAL_REQUEST planned_actuals[2];
    memset(planned_actuals, 0, sizeof(planned_actuals));
    planned_actuals[0].callee_formal_ordinal = 1;
    planned_actuals[0].ty = plan_formal.ty;
    planned_actuals[0].semantic_role = plan_formal.semantic_role;
    planned_actuals[0].source_tcon =
        Enter_tcon(Host_To_Targ_Float(MTYPE_F8, 2.0));
    planned_actuals[1] = planned_actuals[0];
    planned_actuals[1].source_tcon =
        Enter_tcon(Host_To_Targ_Float(MTYPE_F8, 3.0));
    DSL_PU_TRANSACTION_ROUTE_REQUEST routes[2];
    memset(routes, 0, sizeof(routes));
    routes[0].callsite_id = first_site.id;
    routes[0].variant_index = 1;
    routes[0].actuals = &planned_actuals[0];
    routes[0].actual_count = 1;
    routes[1].callsite_id = second_site.id;
    routes[1].variant_index = 1;
    routes[1].actuals = &planned_actuals[1];
    routes[1].actual_count = 1;
    DSL_PU_TRANSACTION_PLAN plan;
    plan.variants = variants;
    plan.variant_count = 2;
    plan.routes = routes;
    plan.route_count = 1;
    UINT32 values_before_preflight = DSL_IR_Image_Value_Count();
    CLONE_CHECK(!DSL_PU_Transaction_Preflight_Resident
                    (source, &plan, NULL) &&
                values_before_preflight == DSL_IR_Image_Value_Count(),
                "incomplete route rejection is read-only");
    plan.route_count = 2;
    CLONE_CHECK(DSL_PU_Transaction_Preflight_Resident
                    (source, &plan, stderr),
                "complete program route preflight");
    UINT32 global_st_count = ST_Table_Size(GLOBAL_SYMTAB);
    DSL_PU_CLONE_VALUE_PAIR rejected_values[8];
    UINT32 rejected_count = 0;
    PU_Info *rejected_clone = NULL;
    CLONE_CHECK(!DSL_PU_Transaction_Clone_Active
                    (source, "pu_transaction_source", rejected_values, 8,
                     &rejected_count, &rejected_clone, NULL) &&
                global_st_count == ST_Table_Size(GLOBAL_SYMTAB) &&
                rejected_clone == NULL && rejected_count == 0,
                "duplicate clone rejection is read-only");

    if (argc == 2 && strcmp(argv[1], "--apply") == 0) {
        DSL_PU_TRANSACTION_RESULT *applied = NULL;
        CLONE_CHECK(DSL_PU_Transaction_Apply_Resident
                        (source, &plan, &applied, stderr),
                    "whole-program apply");
        ST_IDX variant_st = DSL_PU_Transaction_Variant_PU_ST(applied, 1);
        PU_Info *variant = PU_Info_next(source);
        DSL_PU_FORMAL_RECORD source_formal;
        DSL_IR_VALUE_ID copied_value = DSL_IR_VALUE_INVALID_ID;
        CLONE_CHECK(variant != NULL &&
                    variant_st == PU_Info_proc_sym(variant) &&
                    DSL_PU_Interface_Image_Find_Formal
                        (PU_Info_proc_sym(source), 0, &source_formal) &&
                    DSL_PU_Transaction_Cloned_Value
                        (applied, 1, source_formal.formal_value_id,
                         &copied_value) &&
                    copied_value != source_formal.formal_value_id &&
                    WN_num_formals(PU_Info_tree_ptr(variant)) == 3 &&
                    DSL_Builder_Select_PU(caller),
                    "whole-program owner-qualified result");
        DSL_CALL_ARGUMENT_RECORD first_bound;
        DSL_CALL_ARGUMENT_RECORD second_bound;
        TCON_IDX first_tcon = TCON_IDX_ZERO;
        TCON_IDX second_tcon = TCON_IDX_ZERO;
        CLONE_CHECK(DSL_Call_ABI_Image_Find_Argument_By_Id
                        (first_site.id, 1, &first_bound) &&
                    DSL_Call_ABI_Image_Find_Argument_By_Id
                        (second_site.id, 1, &second_bound) &&
                    DSL_PU_Transaction_Scalar_TCON_Active
                        (caller, first_bound.argument_value_id,
                         &first_tcon, stderr) &&
                    DSL_PU_Transaction_Scalar_TCON_Active
                        (caller, second_bound.argument_value_id,
                         &second_tcon, stderr) &&
                    first_tcon == planned_actuals[0].source_tcon &&
                    second_tcon == planned_actuals[1].source_tcon,
                    "caller-bound exact TCON evidence");
        CLONE_CHECK(DSL_Builder_Select_PU(source),
                    "root activation");
        TCON_IDX root_tcon = Enter_tcon
            (Host_To_Targ_Float(MTYPE_F8, 1.5));
        ST_IDX root_st = ST_IDX_ZERO;
        DSL_IR_VALUE_ID root_value = DSL_IR_VALUE_INVALID_ID;
        TCON_IDX rooted_tcon = TCON_IDX_ZERO;
        CLONE_CHECK(DSL_PU_Transaction_Root_Constant_Active
                        (source, DSL_Builder_Get_Value_Image_Id(relu),
                         "__dsl_root_bound", MTYPE_To_TY(MTYPE_F8),
                         root_tcon, &root_st, &root_value, stderr) &&
                    DSL_PU_Transaction_Scalar_TCON_Active
                        (source, root_value, &rooted_tcon, stderr) &&
                    root_st != ST_IDX_ZERO &&
                    rooted_tcon == root_tcon,
                    "entry-owned root bound");
        WN *root_definition = NULL;
        for (WN *stmt = WN_first
                 (WN_func_body(PU_Info_tree_ptr(source)));
             stmt != NULL; stmt = WN_next(stmt)) {
            if (WN_operator(stmt) == OPR_STID &&
                WN_st_idx(stmt) == root_st)
                root_definition = stmt;
        }
        CLONE_CHECK(root_definition != NULL &&
                    WN_operator(WN_kid0(root_definition)) == OPR_CONST,
                    "root constant physical definition");
        WN *root_constant = WN_kid0(root_definition);
        ST_IDX saved_constant_st = WN_st_idx(root_constant);
        WN_st_idx(root_constant) = make_ST_IDX
            (ST_Table_Size(GLOBAL_SYMTAB) + 7, GLOBAL_SYMTAB);
        TCON_IDX rejected_tcon = TCON_IDX_ZERO;
        CLONE_CHECK(!DSL_PU_Transaction_Scalar_TCON_Active
                        (source, root_value, &rejected_tcon, NULL),
                    "out-of-range mapped constant rejects safely");
        WN_st_idx(root_constant) = saved_constant_st;
        CLONE_CHECK(DSL_PU_Transaction_Write_Artifact
                        (source, variant, caller),
                    "whole-program artifact");
        DSL_PU_Transaction_Result_Delete(applied);
        printf("DSL PU transaction whole-program apply passed\n");
        return 0;
    }

    DSL_PU_CLONE_VALUE_PAIR values[8];
    UINT32 value_count = 0;
    PU_Info *clone = NULL;
    CLONE_CHECK(DSL_PU_Transaction_Clone_Active
                    (source, "pu_transaction_variant", values, 8,
                     &value_count, &clone, stderr),
                "physical and logical clone");
    CLONE_CHECK(clone != NULL && value_count == 3 &&
                PU_Info_next(source) == clone &&
                PU_Info_tree_ptr(source) != PU_Info_tree_ptr(clone) &&
                PU_Info_maptab(source) != PU_Info_maptab(clone) &&
                PU_Info_proc_sym(source) != PU_Info_proc_sym(clone) &&
                values[0].source_value_id != values[0].clone_value_id,
                "independent clone identity");
    CLONE_CHECK(DSL_IR_Image_Validate(stderr), "clone managed rows");
    WN *cloned_region = NULL;
    for (WN *stmt = WN_first(WN_func_body(PU_Info_tree_ptr(clone)));
         stmt != NULL; stmt = WN_next(stmt)) {
        if (WN_operator(stmt) == OPR_REGION)
            cloned_region = stmt;
    }
    CLONE_CHECK(cloned_region != NULL &&
                DSL_Region_Is_Managed_WN(clone, cloned_region),
                "clone REGION ownership");
    Restore_Local_Symtab(clone);
    Current_pu = &PU_Info_pu(clone);
    Current_Map_Tab = PU_Info_maptab(clone);
    CLONE_CHECK(DSL_PU_Interface_Image_Validate_PU(clone, stderr) &&
                DSL_Region_Verify_PU(clone, stderr),
                "active clone interface and REGION");

    WN *old_entry = PU_Info_tree_ptr(clone);
    CLONE_CHECK(WN_num_formals(old_entry) == 2,
                "source input and hidden result");
    ST_IDX old_result_st = WN_st_idx(WN_formal(old_entry, 1));
    DSL_PU_TRANSACTION_FORMAL_REQUEST request;
    memset(&request, 0, sizeof(request));
    request.name = "bound_b";
    request.semantic_role = "fhe.relu.bound";
    request.ty = MTYPE_To_TY(MTYPE_F8);
    request.source_position =
        ST_Srcpos(St_Table[WN_st_idx(WN_formal(old_entry, 0))]);
    ST_IDX bound_st;
    DSL_IR_VALUE_ID bound_value;
    UINT32 value_count_before = DSL_IR_Image_Value_Count();
    request.name = "input";
    CLONE_CHECK(!DSL_PU_Transaction_Insert_Formals_Active
                    (clone, &request, 1, &bound_st, &bound_value, NULL) &&
                PU_Info_tree_ptr(clone) == old_entry &&
                DSL_IR_Image_Value_Count() == value_count_before,
                "duplicate formal rejection is read-only");
    request.name = "bound_b";
    CLONE_CHECK(DSL_PU_Transaction_Insert_Formals_Active
                    (clone, &request, 1, &bound_st, &bound_value, stderr),
                "insert typed formal before hidden result");
    WN *new_entry = PU_Info_tree_ptr(clone);
    DSL_PU_FORMAL_RECORD bound_row;
    DSL_PU_FORMAL_RECORD result_row;
    CLONE_CHECK(new_entry != old_entry &&
                WN_num_formals(new_entry) == 3 &&
                WN_st_idx(WN_formal(new_entry, 1)) == bound_st &&
                WN_st_idx(WN_formal(new_entry, 2)) == old_result_st &&
                DSL_PU_Interface_Image_Find_Formal
                    (PU_Info_proc_sym(clone), 1, &bound_row) &&
                bound_row.formal_value_id == bound_value &&
                DSL_PU_Interface_Image_Find_Formal
                    (PU_Info_proc_sym(clone), 2, &result_row) &&
                result_row.formal_st == old_result_st &&
                DSL_PU_Interface_Image_Validate_PU(clone, stderr),
                "typed formal and hidden-result ordinal evidence");

    CLONE_CHECK(DSL_Builder_Select_PU(caller), "caller activation");
    DSL_PU_TRANSACTION_ACTUAL_REQUEST actual;
    memset(&actual, 0, sizeof(actual));
    actual.callee_formal_ordinal = 1;
    actual.ty = MTYPE_To_TY(MTYPE_F8);
    actual.semantic_role = "fhe.relu.bound";
    actual.source_tcon = Enter_tcon(Host_To_Targ_Float(MTYPE_F8, 2.0));
    WN *first_before = const_cast<WN *>
        (DSL_Call_Image_Get_Call_WN(first_site.id));
    UINT32 values_before_route = DSL_IR_Image_Value_Count();
    actual.ty = MTYPE_To_TY(MTYPE_I4);
    CLONE_CHECK(!DSL_PU_Transaction_Route_Call_Active
                    (caller, first_site.id, PU_Info_proc_sym(clone),
                     &actual, 1, NULL) &&
                DSL_Call_Image_Get_Call_WN(first_site.id) == first_before &&
                DSL_IR_Image_Value_Count() == values_before_route,
                "typed route rejection is read-only");
    actual.ty = MTYPE_To_TY(MTYPE_F8);
    CLONE_CHECK(DSL_PU_Transaction_Route_Call_Active
                    (caller, first_site.id, PU_Info_proc_sym(clone),
                     &actual, 1, stderr), "first typed call route");
    actual.source_tcon = Enter_tcon(Host_To_Targ_Float(MTYPE_F8, 3.0));
    CLONE_CHECK(DSL_PU_Transaction_Route_Call_Active
                    (caller, second_site.id, PU_Info_proc_sym(clone),
                     &actual, 1, stderr), "second typed call route");
    const WN *first_routed = DSL_Call_Image_Get_Call_WN(first_site.id);
    const WN *second_routed = DSL_Call_Image_Get_Call_WN(second_site.id);
    DSL_CALL_ARGUMENT_RECORD first_bound;
    DSL_CALL_ARGUMENT_RECORD second_bound;
    CLONE_CHECK(first_routed != NULL && second_routed != NULL &&
                first_routed != first_call &&
                second_routed != second_call &&
                WN_st_idx(first_routed) == PU_Info_proc_sym(clone) &&
                WN_st_idx(second_routed) == PU_Info_proc_sym(clone) &&
                WN_kid_count(first_routed) == 3 &&
                WN_kid_count(second_routed) == 3 &&
                DSL_Call_ABI_Image_Find_Argument_By_Id
                    (first_site.id, 1, &first_bound) &&
                DSL_Call_ABI_Image_Find_Argument_By_Id
                    (second_site.id, 1, &second_bound) &&
                first_bound.argument_value_id !=
                    second_bound.argument_value_id &&
                DSL_Call_ABI_Image_Validate_PU(caller, stderr),
                "two caller-owned bounds and shifted hidden results");
    CLONE_CHECK(DSL_PU_Transaction_Write_Artifact
                    (source, clone, caller), "binary artifact");
    printf("DSL PU transaction physical clone passed\n");
#undef CLONE_CHECK
    return 0;
}
