/*
 * Contract test for mandatory VHO DSL tensor shape refinement.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "defs.h"
#include "mempool.h"
#include "wn.h"
#include "stab.h"
#include "pu_info.h"
#include "ir_reader.h"
#include "erglob.h"
#include "errors.h"
#include "err_host.tab"
#include "config.h"
#include "controls.h"
#include "config_targ_opt.h"
#include "dwarf_DST_mem.h"
#include "dsl_builder.h"
#include "dsl_gatekeeper.h"
#include "dsl_region.h"
#include "dsl_shape_refine.h"

BOOL Run_vsaopt = FALSE;
INT8 Debug_Level = 0;

void
Signal_Cleanup(INT sig) {}

const char *
Host_Format_Parm(INT kind, MEM_PTR parm)
{
    return "";
}

static void
Initialize_Test_Context(void)
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

static void
Initialize_Descriptor
        (DSL_BUILDER_TENSOR_DESCRIPTOR *descriptor,
         const char *shape,
         const char *traits)
{
    memset(descriptor, 0, sizeof(*descriptor));
    descriptor->type_core.kind = "tensor";
    descriptor->type_core.dtype = "float32";
    descriptor->type_core.rank = 2;
    descriptor->type_core.logical_shape = shape;
    descriptor->traits.traits = traits;
    descriptor->representation.layout = "row_major";
    descriptor->representation.sharding = "replicated";
    descriptor->representation.placement = "host";
    descriptor->representation.memory = "contiguous";
    descriptor->representation.quantization = "none";
}

static BOOL
Value_Type_Is (DSL_BUILDER_VALUE value, TY_IDX expected)
{
    DSL_IR_VALUE_RECORD record;
    return value != NULL && WN_operator(value) == OPR_STID &&
           WN_ty(value) == expected &&
           ST_type(St_Table[WN_st_idx(value)]) == expected &&
           DSL_IR_Image_Get_Value
               (DSL_Builder_Get_Value_Image_Id(value), &record) &&
           record.ty == expected;
}

int
main(void)
{
    Initialize_Test_Context();
    DSL_Builder_Begin_Program();
    DSL_Opcode_Register_Common_Substrate();

    DSL_BUILDER_TENSOR_DESCRIPTOR input_descriptor;
    DSL_BUILDER_TENSOR_DESCRIPTOR pending_descriptor;
    Initialize_Descriptor(&input_descriptor, "[2,3]", "activation");
    Initialize_Descriptor
        (&pending_descriptor, "[2,<pending>]", "derived_activation");
    TY_IDX input_ty = DSL_Builder_Intern_Tensor_Type
                          ("sp5_input", MTYPE_To_TY(MTYPE_F4),
                           &input_descriptor);
    TY_IDX pending_ty = DSL_Builder_Intern_Tensor_Type
                            ("sp5_pending", MTYPE_To_TY(MTYPE_F4),
                             &pending_descriptor);
    DSL_BUILDER_PROGRAM_UNIT pu =
        DSL_Builder_Create_Minimal_PU("dsl_shape_refine_sp5");
    UINT32 file_id = DSL_Builder_Register_Source_File(pu, __FILE__);
    DSL_BUILDER_VALUE input = DSL_Builder_Create_Model_Input
                                  ("shape_input", input_ty, 0);
    DSL_BUILDER_OPERATOR_ATTRIBUTE attribute;
    attribute.name = "attr.broadcast_rule";
    attribute.value = "none";
    DSL_BUILDER_VALUE add_kids[2] = { input, input };
    DSL_BUILDER_VALUE add = DSL_Builder_Create_Operator_With_Result
        (DSL_Opcode_Find(DSL_Domain_Find("common"),
                         DSL_OPCODE_COMMON_ADD, 1),
         1, add_kids, 2, &attribute, 1, "shape_add", pending_ty);
    DSL_BUILDER_VALUE relu_kids[1] = { add };
    DSL_BUILDER_VALUE relu = DSL_Builder_Create_Operator_With_Result
        (DSL_Opcode_Find(DSL_Domain_Find("common"), "common.relu", 2),
         2, relu_kids, 1, NULL, 0, "shape_relu", pending_ty);
    DSL_BUILDER_SOURCE_POSITION position;
    memset(&position, 0, sizeof(position));
    position.file_id = file_id;
    position.line = 120;
    position.statement_begin = 1;
    DSL_BUILDER_REGION region = DSL_Builder_Create_Region
                                    (pu, NULL, "shape.refine.v1", 1);
    ST_IDX unrelated = DSL_Builder_Create_Tensor_Result_Symbol
                           ("unrelated_pending", pending_ty,
                            SCLASS_AUTO, EXPORT_LOCAL);
    if (input_ty == TY_IDX_ZERO || pending_ty == TY_IDX_ZERO || pu == NULL ||
        file_id == 0 || input == NULL || add == NULL || relu == NULL ||
        region == NULL || ST_IDX_index(unrelated) == 0 ||
        !DSL_Builder_Set_Tensor_Unique_Ownership(unrelated) ||
        !DSL_Builder_Set_Value_Source_Position(add, &position) ||
        (++position.line,
         !DSL_Builder_Set_Value_Source_Position(relu, &position)) ||
        !DSL_Builder_Append_PU_Value(pu, input) ||
        !DSL_Builder_Append_Region_Value(region, add) ||
        !DSL_Builder_Append_Region_Value(region, relu) ||
        !DSL_Builder_Declare_Region_Value
             (region, relu,
              DSL_REGION_VALUE_OUTPUT | DSL_REGION_VALUE_RESULT,
              0, DSL_REGION_INTERFACE_FLAG_NONE) ||
        !DSL_Builder_Append_PU_Region(pu, region)) {
        fprintf(stderr, "SP5 fixture creation failed\n");
        return 1;
    }

    DSL_GATEKEEPER_RESULT gatekeeper;
    if (!DSL_Gatekeeper_Verify_PU_Mode
             (pu, DSL_GATEKEEPER_ADMISSION, stderr, &gatekeeper) ||
        DSL_Gatekeeper_Verify_PU_Mode
             (pu, DSL_GATEKEEPER_STRICT, NULL, &gatekeeper)) {
        fprintf(stderr, "SP5 admission/strict split changed\n");
        return 1;
    }

    VHO_DSL_SHAPE_REFINE_RESULT disabled;
    if (VHO_DSL_Shape_Refine_Program_Unit
            (pu, PU_Info_tree_ptr(pu), FALSE, NULL, &disabled) ||
        !Value_Type_Is(add, pending_ty) ||
        !Value_Type_Is(relu, pending_ty)) {
        fprintf(stderr, "SP5 disabled check-only mode mutated or accepted\n");
        return 1;
    }

    TY_TENSOR_TYPE_CORE_REFINEMENT refinement;
    refinement.rank = 2;
    refinement.logical_shape = "[2,3]";
    BOOL created = FALSE;
    TY_IDX refined_ty = TY_Intern_Refined_Tensor_Type
                            (pending_ty, &refinement, &created);
    DSL_IR_VALUE_TYPE_REFINEMENT_REQUEST requests[2];
    requests[0].owner_pu_st = PU_Info_proc_sym(pu);
    requests[0].value_id = DSL_Builder_Get_Value_Image_Id(add);
    requests[0].expected_old_ty = pending_ty;
    requests[0].refined_ty = refined_ty;
    requests[1].owner_pu_st = PU_Info_proc_sym(pu);
    requests[1].value_id = DSL_Builder_Get_Value_Image_Id(relu);
    requests[1].expected_old_ty = pending_ty;
    requests[1].refined_ty = refined_ty;
    DSL_IR_VALUE_TYPE_REFINEMENT_RESULT forced_failure;
    setenv("OPEN64_DSL_SHAPE_RETYPE_TEST_POSTFAIL", "1", 1);
    BOOL unexpectedly_committed = DSL_IR_Refine_Native_Value_Types
        (pu, PU_Info_tree_ptr(pu), requests, 2, stderr, &forced_failure);
    unsetenv("OPEN64_DSL_SHAPE_RETYPE_TEST_POSTFAIL");
    if (TY_IDX_index(refined_ty) == 0 || !created || unexpectedly_committed ||
        forced_failure.rollback_count != 2 ||
        !Value_Type_Is(add, pending_ty) ||
        !Value_Type_Is(relu, pending_ty) ||
        ST_type(St_Table[unrelated]) != pending_ty ||
        !DSL_Region_Verify_PU(pu, stderr)) {
        fprintf(stderr, "SP5 late-failure rollback changed\n");
        return 1;
    }

    VHO_DSL_SHAPE_REFINE_RESULT refined;
    if (!VHO_DSL_Shape_Refine_Program_Unit
             (pu, PU_Info_tree_ptr(pu), TRUE, stderr, &refined) ||
        refined.solver.refinable_value_count != 2 ||
        refined.requested_value_count != 2 ||
        refined.retyped_value_count != 2 ||
        refined.reused_type_count != 2 ||
        refined.rollback_count != 0 ||
        !Value_Type_Is(add, refined_ty) ||
        !Value_Type_Is(relu, refined_ty) ||
        WN_ty(WN_kid0(WN_kid0(relu))) != refined_ty ||
        ST_type(St_Table[unrelated]) != pending_ty ||
        !DSL_Region_Verify_PU(pu, stderr) ||
        !DSL_Gatekeeper_Verify_PU_Mode
             (pu, DSL_GATEKEEPER_STRICT, stderr, &gatekeeper)) {
        fprintf(stderr, "SP5 shape refinement result changed\n");
        return 1;
    }

    const char *artifact = getenv("OPEN64_DSL_SHAPE_SP5_ARTIFACT");
    if (artifact != NULL && artifact[0] != '\0') {
        DSL_BUILDER_MAPPED_IMAGE_REQUEST image;
        image.path = artifact;
        image.flags = 0;
        (void)unlink(artifact);
        if (!DSL_Builder_Finalize_Mapped_Image(&image) ||
            access(artifact, F_OK) != 0) {
            fprintf(stderr, "SP5 mapped-image finalization failed\n");
            return 1;
        }
    }

    printf("SP5 shape refinement contract passed: refinable=%u "
           "retyped=%u reused_types=%u rollback=%u\n",
           refined.solver.refinable_value_count,
           refined.retyped_value_count, refined.reused_type_count,
           forced_failure.rollback_count);
    return 0;
}
