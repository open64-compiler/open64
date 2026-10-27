/* Native R1 contract tests and mapped-image producer.
 * Design: doc/AI-COMPILER-NATIVE-REDUCTION-CONTRACT-v0.1.md.
 * This checks representation and preservation, not reduction execution.
 */
#include <stdio.h>
#include <string.h>
#include "defs.h"
#include "mempool.h"
#include "wn.h"
#include "stab.h"
#include "pu_info.h"
#include "ir_reader.h"
#include "glob.h"
#include "erglob.h"
#include "errors.h"
#include "err_host.tab"
#include "config.h"
#include "config_opt.h"
#include "controls.h"
#include "config_targ_opt.h"
#include "dwarf_DST_mem.h"
#include "dsl_builder.h"
#include "dsl_gatekeeper.h"
#include "dsl_ir_image.h"
#include "dsl_shape.h"
#include "opt_dsl.h"

BOOL Run_vsaopt = FALSE;
INT8 Debug_Level = 0;

/* Standalone tools do not install a backend signal cleanup action. */
void Signal_Cleanup(INT sig) {}

/* No host-specific diagnostic parameter formatting is needed by this test. */
const char *Host_Format_Parm(INT kind, MEM_PTR parm) { return ""; }

/* Initialize the ordinary native WHIRL tables, not a parallel test IR. */
static void Initialize_Context(void)
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

/* Intern a sealed static FP32 tensor through the production descriptor API. */
static TY_IDX Tensor(const char *name, UINT32 rank, const char *shape)
{
  DSL_BUILDER_TENSOR_DESCRIPTOR descriptor;
  memset(&descriptor, 0, sizeof(descriptor));
  descriptor.type_core.kind = "tensor";
  descriptor.type_core.dtype = "float32";
  descriptor.type_core.rank = rank;
  descriptor.type_core.logical_shape = shape;
  descriptor.representation.layout = "row_major";
  return DSL_Builder_Intern_Tensor_Type
      (name, MTYPE_To_TY(MTYPE_F4), &descriptor);
}

/* Report each negative admission check without hiding the offending case. */
static BOOL Check(BOOL condition, const char *name)
{
  printf("contract %s %s\n", name, condition ? "PASS" : "FAIL");
  return condition;
}

/* Build, reject malformed variants, bridge through WOPT and write native .B. */
int main(int argc, char **argv)
{
  if (argc != 2 && (argc != 3 || strcmp(argv[2], "cross-pu") != 0)) {
    fprintf(stderr, "usage: %s output.B [cross-pu]\n", argv[0]);
    return 2;
  }
  Initialize_Context();
  if (!DSL_Builder_Begin_Program()) return 1;
  DSL_BUILDER_PROGRAM_UNIT pu = DSL_Builder_Create_Minimal_PU("row_sum");
  if (pu == NULL || !DSL_Builder_Select_PU(pu)) return 1;
  Current_PU_Info = pu;
  TY_IDX input_ty = Tensor("rows_3_17", 2, "[3,17]");
  TY_IDX result_ty = Tensor("rows_3", 1, "[3]");
  TY_IDX wrong_ty = Tensor("rows_4", 1, "[4]");
  DSL_BUILDER_SOURCE_POSITION position;
  memset(&position, 0, sizeof(position));
  position.file_id = DSL_Builder_Register_Source_File(pu, __FILE__);
  position.line = __LINE__ + 1;
  position.statement_begin = 1;
  DSL_BUILDER_VALUE input = DSL_Builder_Declare_PU_Formal
      (pu, "x", 0, input_ty, &position);
  if (input == NULL || DSL_Builder_Declare_PU_Result
      (pu, "sum", 0, result_ty, DSL_PU_RESULT_TENSOR, &position) == NULL)
    return 1;
  DSL_Opcode_Register_Common_Substrate();
  DSL_DOMAIN_ID common = DSL_Domain_Find("common");
  DSL_OPCODE_ID opcode = DSL_Opcode_Find(common, "common.reduce_sum", 1);
  DSL_BUILDER_VALUE kids[1] = { input };
  DSL_BUILDER_OPERATOR_ATTRIBUTE attrs[5] = {
    { "attr.axis", "1" }, { "attr.keepdim", "false" },
    { "attr.accum_dtype", "float32" }, { "attr.reassociate", "false" },
    { "attr.extra", "not_allowed" }
  };
  if (argc == 3) {
    DSL_BUILDER_PROGRAM_UNIT other = DSL_Builder_Create_Minimal_PU("other");
    if (other == NULL || !DSL_Builder_Select_PU(other)) return 1;
    UINT32 nodes = DSL_IR_Image_Node_Count();
    UINT32 symbols = ST_Table_Size(CURRENT_SYMTAB);
    return Check(DSL_Builder_Create_Operator_With_Result
        (opcode, 1, kids, 1, attrs, 4, "foreign", result_ty) == NULL &&
        DSL_IR_Image_Node_Count() == nodes &&
        ST_Table_Size(CURRENT_SYMTAB) == symbols, "cross_pu_rejected") ? 0 : 1;
  }
  DSL_IR_IMAGE_HEADER before, after;
  DSL_IR_Image_Get_Header(&before);
  UINT32 symbol_count = ST_Table_Size(CURRENT_SYMTAB);
  if (!Check(opcode != DSL_OPCODE_INVALID_ID, "registry") ||
      !Check(DSL_Builder_Create_Operator(opcode, 1, kids, 1, attrs, 4)
             == NULL, "explicit_result") ||
      !Check(DSL_Builder_Create_Operator_With_Result
          (opcode, 2, kids, 1, attrs, 4, "bad_version", result_ty) == NULL,
          "unsupported_version") ||
      !Check(DSL_Builder_Create_Operator_With_Result
          (opcode, 1, kids, 1, attrs, 3, "missing", result_ty) == NULL,
          "missing_attribute") ||
      !Check(DSL_Builder_Create_Operator_With_Result
          (opcode, 1, kids, 1, attrs, 5, "extra", result_ty) == NULL,
          "extra_attribute") ||
      !Check(DSL_Builder_Create_Operator_With_Result
          (opcode, 1, kids, 1, attrs, 4, "shape", wrong_ty) == NULL,
          "result_shape") ||
      !Check(DSL_Builder_Create_Operator_With_Result
          (opcode, 1, kids, 1, attrs, 4, "rank", input_ty) == NULL,
          "result_rank")) return 1;
  const char *bad_values[] = { "-1", "true", "float16", "true" };
  for (UINT32 i = 0; i < 4; ++i) {
    const char *saved = attrs[i].value;
    attrs[i].value = bad_values[i];
    BOOL rejected = DSL_Builder_Create_Operator_With_Result
        (opcode, 1, kids, 1, attrs, 4, "bad_attr", result_ty) == NULL;
    attrs[i].value = saved;
    if (!Check(rejected, attrs[i].name)) return 1;
  }
  attrs[3].name = "attr.axis";
  BOOL duplicate_rejected = DSL_Builder_Create_Operator_With_Result
      (opcode, 1, kids, 1, attrs, 4, "duplicate", result_ty) == NULL;
  attrs[3].name = "attr.reassociate";
  DSL_IR_Image_Get_Header(&after);
  if (!Check(duplicate_rejected, "duplicate_attribute") ||
      !Check(memcmp(&before, &after, sizeof(before)) == 0 &&
             ST_Table_Size(CURRENT_SYMTAB) == symbol_count,
             "rejection_preserves_image_and_symbols") ||
      !Check(!DSL_Shape_Reduce_Sum_Types_Valid
          (Tensor("empty_rows", 2, "[3,0]"), result_ty,
           "1", "false", "float32", "false"), "empty_axis") ||
      !Check(!DSL_Shape_Reduce_Sum_Types_Valid
          (Tensor("dynamic_rows", 2, "[3,?]"), result_ty,
           "1", "false", "float32", "false"), "dynamic_axis")) return 1;
  position.line = __LINE__ + 1;
  DSL_BUILDER_VALUE sum = DSL_Builder_Create_Operator_With_Result
      (opcode, 1, kids, 1, attrs, 4, "sum_value", result_ty);
  if (sum == NULL || !DSL_Builder_Set_Value_Source_Position(sum, &position) ||
      !DSL_Builder_Append_PU_Value(pu, sum) ||
      !DSL_Builder_Return_PU_Values(pu, &sum, 1)) return 1;
  DSL_GATEKEEPER_RESULT gate;
  if (!Check(DSL_Gatekeeper_Verify_PU(pu, stderr, &gate), "gatekeeper"))
    return 1;
  WN *input_load = WN_kid0(WN_kid0(sum));
  WN_set_ty(input_load, wrong_ty);
  BOOL corruption_rejected = !DSL_Gatekeeper_Verify_PU(pu, NULL, &gate);
  WN_set_ty(input_load, input_ty);
  if (!Check(corruption_rejected, "gatekeeper_operand_type_corruption") ||
      !Check(!WOPT_DSL_verify_reduction_forms
          (WN_func_body(PU_Info_tree_ptr(pu)), "scope", NULL, NULL),
          "non_pu_scope_rejected")) return 1;
  WOPT_DSL_SEMANTIC_INFO info;
  if (!Check(WOPT_DSL_import_semantic_info
      (WN_kid0(sum), DSL_Builder_Get_Value_Result_Symbol(sum), "row_sum",
       &info, stderr), "wopt_import")) return 1;
  WN *operand[1] = { WN_COPY_Tree(WN_kid0(WN_kid0(sum))) };
  WN *emitted = WOPT_DSL_emit_WN
      (&info, WN_kid0(sum), DSL_Builder_Get_Value_Result_Symbol(sum),
       operand, 1, stderr);
  if (!Check(emitted != NULL, "wopt_emit")) return 1;
  WN_kid0(sum) = emitted;
  if (!Check(DSL_Gatekeeper_Verify_PU(pu, stderr, &gate),
             "emitted_gatekeeper") ||
      !Check(WOPT_DSL_verify_reduction_forms
          (PU_Info_tree_ptr(pu), "bridge", stderr, NULL), "canonical_form"))
    return 1;
  DSL_BUILDER_MAPPED_IMAGE_REQUEST request;
  request.path = argv[1];
  request.flags = 0;
  if (!Check(DSL_Builder_Finalize_Mapped_Image(&request), "mapped_image"))
    return 1;
  printf("native row sum contract passed; execution lowering not selected\n");
  return 0;
}
