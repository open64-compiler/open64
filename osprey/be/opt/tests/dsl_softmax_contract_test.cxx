/* Native R1 contract tests and mapped-image producer.
 * Design: doc/AI-COMPILER-NATIVE-SOFTMAX-CONTRACT-v0.1.md.
 * This checks representation and preservation, not Softmax execution.
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

/* Intern descriptors through the production API, including malformed test TYs. */
static TY_IDX Tensor(const char *name, UINT32 rank, const char *shape,
                     const char *dtype = "float32", TYPE_ID element = MTYPE_F4)
{
  DSL_BUILDER_TENSOR_DESCRIPTOR descriptor;
  memset(&descriptor, 0, sizeof(descriptor));
  descriptor.type_core.kind = "tensor";
  descriptor.type_core.dtype = dtype;
  descriptor.type_core.rank = rank;
  descriptor.type_core.logical_shape = shape;
  descriptor.representation.layout = "row_major";
  return DSL_Builder_Intern_Tensor_Type
      (name, MTYPE_To_TY(element), &descriptor);
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
  DSL_BUILDER_PROGRAM_UNIT pu = DSL_Builder_Create_Minimal_PU("stable_softmax");
  if (pu == NULL || !DSL_Builder_Select_PU(pu)) return 1;
  Current_PU_Info = pu;
  TY_IDX input_ty = Tensor("rows_3_17", 2, "[3,17]");
  TY_IDX result_ty = Tensor("result_3_17", 2, "[3,17]");
  TY_IDX wrong_ty = Tensor("rows_4_17", 2, "[4,17]");
  DSL_BUILDER_SOURCE_POSITION position;
  memset(&position, 0, sizeof(position));
  position.file_id = DSL_Builder_Register_Source_File(pu, __FILE__);
  position.line = __LINE__ + 1;
  position.statement_begin = 1;
  DSL_BUILDER_VALUE input = DSL_Builder_Declare_PU_Formal
      (pu, "x", 0, input_ty, &position);
  if (input == NULL || DSL_Builder_Declare_PU_Result
      (pu, "probabilities", 0, result_ty, DSL_PU_RESULT_TENSOR, &position) == NULL)
    return 1;
  DSL_Opcode_Register_Common_Substrate();
  DSL_DOMAIN_ID common = DSL_Domain_Find("common");
  DSL_OPCODE_ID opcode = DSL_Opcode_Find(common, "common.stable_softmax", 1);
  DSL_BUILDER_VALUE kids[1] = { input };
  DSL_BUILDER_OPERATOR_ATTRIBUTE attrs[6] = {
    { "attr.axis", "1" }, { "attr.accum_dtype", "float32" },
    { "attr.reassociate", "false" }, { "attr.state_reformulation", "false" },
    { "attr.exp_semantics", "source_unresolved" },
    { "attr.extra", "not_allowed" }
  };
  if (argc == 3) {
    DSL_BUILDER_PROGRAM_UNIT other = DSL_Builder_Create_Minimal_PU("other");
    if (other == NULL || !DSL_Builder_Select_PU(other)) return 1;
    UINT32 nodes = DSL_IR_Image_Node_Count();
    UINT32 symbols = ST_Table_Size(CURRENT_SYMTAB);
    return Check(DSL_Builder_Create_Operator_With_Result
        (opcode, 1, kids, 1, attrs, 5, "foreign", result_ty) == NULL &&
        DSL_IR_Image_Node_Count() == nodes &&
        ST_Table_Size(CURRENT_SYMTAB) == symbols, "cross_pu_rejected") ? 0 : 1;
  }
  TY_IDX rank1_ty = Tensor("rank1", 1, "[3]");
  TY_IDX width_ty = Tensor("wrong_width", 2, "[3,18]");
  DSL_IR_IMAGE_HEADER before, after;
  DSL_IR_Image_Get_Header(&before);
  UINT32 symbol_count = ST_Table_Size(CURRENT_SYMTAB);
  if (!Check(opcode != DSL_OPCODE_INVALID_ID, "registry") ||
      !Check(OPR_DSLREDUCESUM == 37 && OPR_DSLSTABLESOFTMAX == 38 &&
          DSL_Operator_Find("common.softmax", strlen("common.softmax"), 1) ==
              OPR_DSLUNKNOWN, "append_only_distinct_from_generic_softmax") ||
      !Check(DSL_Builder_Create_Operator_With_Result
          (opcode, 1, NULL, 0, attrs, 5, "arity", result_ty) == NULL,
          "zero_operands") ||
      !Check(DSL_Builder_Create_Operator(opcode, 1, kids, 1, attrs, 5)
             == NULL, "explicit_result") ||
      !Check(DSL_Builder_Create_Operator_With_Result
          (opcode, 2, kids, 1, attrs, 5, "bad_version", result_ty) == NULL,
          "unsupported_version") ||
      !Check(DSL_Builder_Create_Operator_With_Result
          (opcode, 1, kids, 1, attrs, 4, "missing", result_ty) == NULL,
          "missing_attribute") ||
      !Check(DSL_Builder_Create_Operator_With_Result
          (opcode, 1, kids, 1, attrs, 6, "extra", result_ty) == NULL,
          "extra_attribute") ||
      !Check(DSL_Builder_Create_Operator_With_Result
          (opcode, 1, kids, 1, attrs, 5, "shape", wrong_ty) == NULL,
          "result_shape") ||
      !Check(DSL_Builder_Create_Operator_With_Result
          (opcode, 1, kids, 1, attrs, 5, "width", width_ty) == NULL,
          "result_width") ||
      !Check(DSL_Builder_Create_Operator_With_Result
          (opcode, 1, kids, 1, attrs, 5, "rank",
           rank1_ty) == NULL,
          "result_rank")) return 1;
  const char *bad_values[] = { "-1", "float16", "true", "true", "fast_math" };
  for (UINT32 i = 0; i < 5; ++i) {
    const char *saved = attrs[i].value;
    attrs[i].value = bad_values[i];
    BOOL rejected = DSL_Builder_Create_Operator_With_Result
        (opcode, 1, kids, 1, attrs, 5, "bad_attr", result_ty) == NULL;
    attrs[i].value = saved;
    if (!Check(rejected, attrs[i].name)) return 1;
  }
  attrs[4].name = "attr.axis";
  BOOL duplicate_rejected = DSL_Builder_Create_Operator_With_Result
      (opcode, 1, kids, 1, attrs, 5, "duplicate", result_ty) == NULL;
  attrs[4].name = "attr.exp_semantics";
  DSL_IR_Image_Get_Header(&after);
  if (!Check(duplicate_rejected, "duplicate_attribute") ||
      !Check(memcmp(&before, &after, sizeof(before)) == 0 &&
             ST_Table_Size(CURRENT_SYMTAB) == symbol_count,
             "rejection_preserves_image_and_symbols") ||
      !Check(!DSL_Shape_Stable_Softmax_Types_Valid
          (Tensor("empty_rows", 2, "[3,0]"), result_ty,
           "1", "float32", "false", "false", "source_unresolved"), "empty_axis") ||
      !Check(!DSL_Shape_Stable_Softmax_Types_Valid
          (Tensor("dynamic_rows", 2, "[3,?]"), result_ty,
           "1", "float32", "false", "false", "source_unresolved"), "dynamic_axis") ||
      !Check(!DSL_Shape_Stable_Softmax_Types_Valid
          (input_ty, Tensor("wrong_dtype", 2, "[3,17]", "float64", MTYPE_F8),
           "1", "float32", "false", "false", "source_unresolved"), "result_dtype") ||
      !Check(!DSL_Shape_Stable_Softmax_Types_Valid
          (Tensor("wrong_element", 2, "[3,17]", "float32", MTYPE_F8), result_ty,
           "1", "float32", "false", "false", "source_unresolved"), "element_mtype") ||
      !Check(DSL_Shape_Stable_Softmax_Types_Valid
          (input_ty, input_ty, "1", "float32", "false", "false", "source_unresolved"),
          "same_ty_legal") ||
      !Check(DSL_Shape_Stable_Softmax_Types_Valid
          (Tensor("unit", 2, "[1,1]"), Tensor("unit", 2, "[1,1]"),
           "1", "float32", "false", "false", "source_unresolved"), "unit_shape")) return 1;
  position.line = __LINE__ + 1;
  DSL_BUILDER_VALUE sum = DSL_Builder_Create_Operator_With_Result
      (opcode, 1, kids, 1, attrs, 5, "sum_value", result_ty);
  if (sum == NULL || !DSL_Builder_Set_Value_Source_Position(sum, &position) ||
      !DSL_Builder_Append_PU_Value(pu, sum) ||
      !DSL_Builder_Return_PU_Values(pu, &sum, 1)) return 1;
  DSL_GATEKEEPER_RESULT gate;
  if (!Check(DSL_Gatekeeper_Verify_PU(pu, stderr, &gate), "gatekeeper"))
    return 1;
  WOPT_DSL_SOFTMAX_CHARACTERIZATION fact;
  memset(&fact, 0, sizeof(fact));
  DSL_IR_Image_Get_Header(&before);
  UINT32 types_before = TY_Table_Size();
  if (!Check(WOPT_DSL_characterize_softmax
      (PU_Info_tree_ptr(pu), sum, &fact, stderr), "native_characterization") ||
      !Check(fact.recipe == WOPT_DSL_SOFTMAX_STABLE_F32_ROWS_V1 &&
          fact.rows == 3 && fact.width == 17 && fact.input_ty == input_ty &&
          fact.result_ty == result_ty && fact.input_value_id != fact.result_value_id &&
          fact.flags == 0xfff && fact.source_line == (UINT32)position.line,
          "characterization_state_access_numerical")) return 1;
  DSL_IR_Image_Get_Header(&after);
  if (!Check(memcmp(&before, &after, sizeof(before)) == 0 &&
             TY_Table_Size() == types_before,
             "characterization_read_only")) return 1;
  WOPT_DSL_SOFTMAX_CHARACTERIZATION untouched, saved;
  memset(&untouched, 0x5a, sizeof(untouched));
  memcpy(&saved, &untouched, sizeof(saved));
  if (!Check(!WOPT_DSL_characterize_softmax
          (WN_func_body(PU_Info_tree_ptr(pu)), sum, &untouched, NULL) &&
          memcmp(&untouched, &saved, sizeof(saved)) == 0,
          "characterization_scope_rejected") ||
      !Check(!WOPT_DSL_characterize_softmax
          (PU_Info_tree_ptr(pu), WN_COPY_Tree(sum), &untouched, NULL) &&
          memcmp(&untouched, &saved, sizeof(saved)) == 0,
          "characterization_nonmember_rejected") ||
      !Check(!WOPT_DSL_characterize_softmax
          (PU_Info_tree_ptr(pu), sum, NULL, NULL),
          "characterization_null_output_rejected")) return 1;
  ST_tensor_bind_metadata(DSL_Builder_Get_Value_Result_Symbol(sum),
                          "r1.test.non_authoritative_reassociate", "true");
  ST_tensor_bind_metadata(DSL_Builder_Get_Value_Result_Symbol(sum),
                          "r1.test.non_authoritative_finite", "true");
  WOPT_DSL_SOFTMAX_CHARACTERIZATION again;
  memset(&again, 0, sizeof(again));
  if (!Check(WOPT_DSL_characterize_softmax
          (PU_Info_tree_ptr(pu), sum, &again, stderr) &&
          memcmp(&fact, &again, sizeof(fact)) == 0,
          "characterization_ignores_nonauthoritative_metadata")) return 1;
  WN *input_load = WN_kid0(WN_kid0(sum));
  WN_set_ty(input_load, wrong_ty);
  BOOL corruption_rejected = !DSL_Gatekeeper_Verify_PU(pu, NULL, &gate);
  BOOL characterization_rejected = !WOPT_DSL_characterize_softmax
      (PU_Info_tree_ptr(pu), sum, &untouched, NULL) &&
      memcmp(&untouched, &saved, sizeof(saved)) == 0;
  WN_set_ty(input_load, input_ty);
  if (!Check(corruption_rejected, "gatekeeper_operand_type_corruption") ||
      !Check(characterization_rejected, "characterization_corruption_rejected") ||
      !Check(!WOPT_DSL_verify_softmax_forms
          (WN_func_body(PU_Info_tree_ptr(pu)), "scope", NULL, NULL),
          "non_pu_scope_rejected")) return 1;
  WOPT_DSL_SEMANTIC_INFO info;
  if (!Check(WOPT_DSL_import_semantic_info
      (WN_kid0(sum), DSL_Builder_Get_Value_Result_Symbol(sum), "stable_softmax",
       &info, stderr), "wopt_import")) return 1;
  WN *operand[1] = { WN_COPY_Tree(WN_kid0(WN_kid0(sum))) };
  WN *emitted = WOPT_DSL_emit_WN
      (&info, WN_kid0(sum), DSL_Builder_Get_Value_Result_Symbol(sum),
       operand, 1, stderr);
  if (!Check(emitted != NULL, "wopt_emit")) return 1;
  WN_kid0(sum) = emitted;
  if (!Check(WOPT_DSL_characterize_softmax
          (PU_Info_tree_ptr(pu), sum, &again, stderr) &&
          memcmp(&fact, &again, sizeof(fact)) == 0,
          "characterization_after_wopt_emit")) return 1;
  if (!Check(DSL_Gatekeeper_Verify_PU(pu, stderr, &gate),
             "emitted_gatekeeper") ||
      !Check(WOPT_DSL_verify_softmax_forms
          (PU_Info_tree_ptr(pu), "bridge", stderr, NULL), "canonical_form"))
    return 1;
  DSL_BUILDER_MAPPED_IMAGE_REQUEST request;
  request.path = argv[1];
  request.flags = 0;
  if (!Check(DSL_Builder_Finalize_Mapped_Image(&request), "mapped_image"))
    return 1;
  printf("native stable Softmax contract passed; execution lowering not selected\n");
  return 0;
}
