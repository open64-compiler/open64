/* Native R1 contract tests and mapped-image producer.
 * Design: doc/AI-COMPILER-NATIVE-NORMALIZATION-CONTRACT-v0.1.md.
 * This checks representation and preservation, not normalization execution.
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

/* Exercise rejection/rollback, read-only characterization and ternary emission.
 * Each mapped producer invocation owns a fresh native context and whole PU. */
int main(int argc, char **argv)
{
  if (argc != 2 && argc != 3) return 2;
  if (argc == 3 && strcmp(argv[2], "unit") != 0 && strcmp(argv[2], "wide") != 0 &&
      strcmp(argv[2], "epsilon2") != 0 && strcmp(argv[2], "cross-pu") != 0) return 2;
  UINT64 rows = 3, width = 17;
  if (argc == 3 && strcmp(argv[2], "unit") == 0) rows = width = 1;
  if (argc == 3 && strcmp(argv[2], "wide") == 0) { rows = 7; width = 65; }
  Initialize_Context();
  if (!DSL_Builder_Begin_Program()) return 1;
  DSL_BUILDER_PROGRAM_UNIT pu = DSL_Builder_Create_Minimal_PU("centered_norm");
  if (pu == NULL || !DSL_Builder_Select_PU(pu)) return 1;
  Current_PU_Info = pu;
  char shape[64], affine_shape[64], bad_shape[64];
  snprintf(shape, sizeof(shape), "[%llu,%llu]", (unsigned long long)rows,
           (unsigned long long)width);
  snprintf(affine_shape, sizeof(affine_shape), "[%llu]", (unsigned long long)width);
  snprintf(bad_shape, sizeof(bad_shape), "[%llu]", (unsigned long long)width + 1);
  TY_IDX input_ty = Tensor("rows", 2, shape);
  TY_IDX affine_ty = Tensor("affine", 1, affine_shape);
  TY_IDX bad_affine_ty = Tensor("bad_affine", 1, bad_shape);
  TY_IDX fp64_ty = Tensor("fp64", 2, shape, "float64", MTYPE_F8);
  DSL_BUILDER_SOURCE_POSITION position;
  memset(&position, 0, sizeof(position));
  position.file_id = DSL_Builder_Register_Source_File(pu, __FILE__);
  position.line = __LINE__ + 1;
  position.statement_begin = 1;
  DSL_BUILDER_VALUE kids[3] = {
    DSL_Builder_Declare_PU_Formal(pu, "x", 0, input_ty, &position),
    DSL_Builder_Declare_PU_Formal(pu, "gamma", 1, affine_ty, &position),
    DSL_Builder_Declare_PU_Formal(pu, "beta", 2, affine_ty, &position)
  };
  if (kids[0] == NULL || kids[1] == NULL || kids[2] == NULL ||
      DSL_Builder_Declare_PU_Result(pu, "y", 0, input_ty,
          DSL_PU_RESULT_TENSOR, &position) == NULL) return 1;
  DSL_Opcode_Register_Common_Substrate();
  DSL_OPCODE_ID opcode = DSL_Opcode_Find(DSL_Domain_Find("common"), "common.mean_variance_norm", 1);
  DSL_BUILDER_OPERATOR_ATTRIBUTE attrs[9] = {
    { "attr.axis", "1" }, { "attr.accum_dtype", "float32" },
    { "attr.variance_kind", "population_centered" }, { "attr.epsilon_bits", "3727c5ac" },
    { "attr.reassociate", "false" }, { "attr.state_reformulation", "false" },
    { "attr.contract", "false" }, { "attr.rsqrt_semantics", "source_unresolved" },
    { "attr.extra", "forbidden" }
  };
  if (argc == 3 && strcmp(argv[2], "epsilon2") == 0) attrs[3].value = "3a83126f";
  if (argc == 3 && strcmp(argv[2], "cross-pu") == 0) {
    DSL_BUILDER_PROGRAM_UNIT other = DSL_Builder_Create_Minimal_PU("other");
    if (other == NULL || !DSL_Builder_Select_PU(other)) return 1;
    Current_PU_Info = other;
    DSL_BUILDER_VALUE own[3] = {
      DSL_Builder_Declare_PU_Formal(other, "own_x", 0, input_ty, &position),
      DSL_Builder_Declare_PU_Formal(other, "own_gamma", 1, affine_ty, &position),
      DSL_Builder_Declare_PU_Formal(other, "own_beta", 2, affine_ty, &position)
    };
    if (own[0] == NULL || own[1] == NULL || own[2] == NULL) return 1;
    for (UINT32 i = 0; i < 3; ++i) {
      DSL_BUILDER_VALUE mixed[3] = { own[0], own[1], own[2] };
      mixed[i] = kids[i];
      UINT32 nodes = DSL_IR_Image_Node_Count(), symbols = ST_Table_Size(CURRENT_SYMTAB);
      if (!Check(DSL_Builder_Create_Operator_With_Result
          (opcode, 1, mixed, 3, attrs, 8, "foreign", input_ty) == NULL &&
          nodes == DSL_IR_Image_Node_Count() && symbols == ST_Table_Size(CURRENT_SYMTAB),
          "cross_pu_operand_rejected")) return 1;
    }
    return 0;
  }
  DSL_IR_IMAGE_HEADER before, after;
  DSL_IR_Image_Get_Header(&before);
  UINT32 symbols = ST_Table_Size(CURRENT_SYMTAB);
  if (!Check(opcode != DSL_OPCODE_INVALID_ID && OPR_DSLMEANVARNORM == 39 &&
             OPR_DSLSTABLESOFTMAX == 38, "registry_append_only") ||
      !Check(DSL_Builder_Create_Operator(opcode, 1, kids, 3, attrs, 8) == NULL, "explicit_result") ||
      !Check(DSL_Builder_Create_Operator_With_Result
          (opcode, 1, NULL, 0, attrs, 8, "arity", input_ty) == NULL, "arity") ||
      !Check(DSL_Builder_Create_Operator_With_Result
          (opcode, 2, kids, 3, attrs, 8, "version", input_ty) == NULL, "version") ||
      !Check(DSL_Builder_Create_Operator_With_Result
          (opcode, 1, kids, 3, attrs, 7, "missing", input_ty) == NULL, "missing_attribute") ||
      !Check(DSL_Builder_Create_Operator_With_Result
          (opcode, 1, kids, 3, attrs, 9, "extra", input_ty) == NULL, "extra_attribute") ||
      !Check(DSL_Builder_Create_Operator_With_Result
          (opcode, 1, kids, 3, attrs, 8, "rank", affine_ty) == NULL, "result_rank") ||
      !Check(DSL_Builder_Create_Operator_With_Result
          (opcode, 1, kids, 3, attrs, 8, "dtype", fp64_ty) == NULL, "result_dtype"))
    return 1;
  const char *bad_values[8] = { "-1", "float64", "sample", "00000000",
                              "true", "true", "true", "fast_rsqrt" };
  for (UINT32 i = 0; i < 8; ++i) {
    const char *saved = attrs[i].value;
    attrs[i].value = bad_values[i];
    BOOL rejected = DSL_Builder_Create_Operator_With_Result
        (opcode, 1, kids, 3, attrs, 8, "bad_attr", input_ty) == NULL;
    attrs[i].value = saved;
    if (!Check(rejected, attrs[i].name)) return 1;
  }
  attrs[7].name = "attr.axis";
  BOOL duplicate = DSL_Builder_Create_Operator_With_Result
      (opcode, 1, kids, 3, attrs, 8, "duplicate", input_ty) == NULL;
  attrs[7].name = "attr.rsqrt_semantics";
  DSL_IR_Image_Get_Header(&after);
  if (!Check(duplicate, "duplicate_attribute") ||
      !Check(memcmp(&before, &after, sizeof(before)) == 0 &&
             symbols == ST_Table_Size(CURRENT_SYMTAB), "rejection_rollback")) return 1;
  const char *bad_epsilon[] = { "", "1e-5", "0x3727c5ac", "3727C5AC", "00000000",
                               "80000000", "bf800000", "7f800000", "7fc00000", "3727c5ag" };
  UINT32 bits = 0x12345678;
  for (UINT32 i = 0; i < sizeof(bad_epsilon)/sizeof(bad_epsilon[0]); ++i)
    if (!Check(!DSL_positive_f32_bits_valid(bad_epsilon[i], &bits) &&
               bits == 0x12345678, "epsilon_rejected_output_unchanged")) return 1;
  if (!Check(DSL_positive_f32_bits_valid("00000001", &bits) && bits == 1,
             "subnormal_representable_not_target_proof") ||
      !Check(!DSL_mean_variance_norm_types_valid
          (input_ty, bad_affine_ty, affine_ty, input_ty, "1", "float32",
           "population_centered", attrs[3].value, "false", "false", "false", "source_unresolved"),
          "gamma_broadcast_shape") ||
      !Check(!DSL_mean_variance_norm_types_valid
          (input_ty, affine_ty, input_ty, input_ty, "1", "float32",
           "population_centered", attrs[3].value, "false", "false", "false", "source_unresolved"),
          "beta_rank") ||
      !Check(!DSL_mean_variance_norm_types_valid
          (Tensor("empty", 2, "[3,0]"), affine_ty, affine_ty, input_ty, "1", "float32",
           "population_centered", attrs[3].value, "false", "false", "false", "source_unresolved"),
          "empty_axis") ||
      !Check(!DSL_mean_variance_norm_types_valid
          (Tensor("dynamic", 2, "[3,?]"), affine_ty, affine_ty, input_ty, "1", "float32",
           "population_centered", attrs[3].value, "false", "false", "false", "source_unresolved"),
          "dynamic_axis")) return 1;
  position.line = __LINE__ + 1;
  DSL_BUILDER_VALUE norm = DSL_Builder_Create_Operator_With_Result
      (opcode, 1, kids, 3, attrs, 8, "norm_value", input_ty);
  if (norm == NULL || !DSL_Builder_Set_Value_Source_Position(norm, &position) ||
      !DSL_Builder_Append_PU_Value(pu, norm) || !DSL_Builder_Return_PU_Values(pu, &norm, 1)) return 1;
  DSL_GATEKEEPER_RESULT gate;
  if (!Check(DSL_Gatekeeper_Verify_PU(pu, stderr, &gate), "gatekeeper")) return 1;
  WOPT_DSL_NORMALIZATION_CHARACTERIZATION fact, again, untouched, saved;
  memset(&fact, 0, sizeof(fact));
  DSL_IR_Image_Get_Header(&before);
  UINT32 types = TY_Table_Size();
  if (!Check(WOPT_DSL_characterize_normalization
          (PU_Info_tree_ptr(pu), norm, &fact, stderr), "characterization") ||
      !Check(fact.rows == rows && fact.width == width && fact.flags == 0x1fff &&
          fact.recipe == WOPT_DSL_NORM_CENTERED_POPULATION_F32_ROWS_V1 &&
          fact.operand_types[0] == input_ty && fact.operand_types[1] == affine_ty &&
          fact.operand_types[2] == affine_ty && fact.result_ty == input_ty &&
          fact.source_line == (UINT32)position.line &&
          DSL_positive_f32_bits_valid(attrs[3].value, &bits) && bits == fact.epsilon_bits,
          "state_broadcast_numerical_facts")) return 1;
  DSL_IR_Image_Get_Header(&after);
  if (!Check(memcmp(&before, &after, sizeof(before)) == 0 && TY_Table_Size() == types,
             "characterization_read_only")) return 1;
  memset(&untouched, 0x5a, sizeof(untouched));
  memcpy(&saved, &untouched, sizeof(saved));
  if (!Check(!WOPT_DSL_characterize_normalization
          (WN_func_body(PU_Info_tree_ptr(pu)), norm, &untouched, NULL) &&
          memcmp(&saved, &untouched, sizeof(saved)) == 0, "scope_output_unchanged") ||
      !Check(!WOPT_DSL_characterize_normalization
          (PU_Info_tree_ptr(pu), WN_COPY_Tree(norm), &untouched, NULL) &&
          memcmp(&saved, &untouched, sizeof(saved)) == 0, "nonmember_output_unchanged") ||
      !Check(!WOPT_DSL_characterize_normalization
          (PU_Info_tree_ptr(pu), norm, NULL, NULL), "null_output")) return 1;
  ST_tensor_bind_metadata(DSL_Builder_Get_Value_Result_Symbol(norm), "r1.test.epsilon_bits", "3f800000");
  if (!Check(WOPT_DSL_characterize_normalization
          (PU_Info_tree_ptr(pu), norm, &again, stderr) &&
          memcmp(&fact, &again, sizeof(fact)) == 0, "debug_metadata_not_authority")) return 1;
  for (UINT32 i = 0; i < 3; ++i) {
    WN *load = WN_kid(WN_kid0(norm), i);
    TY_IDX saved_ty = WN_ty(load);
    WN_set_ty(load, i == 0 ? affine_ty : bad_affine_ty);
    BOOL rejected = !WOPT_DSL_characterize_normalization
        (PU_Info_tree_ptr(pu), norm, &untouched, NULL) &&
        memcmp(&saved, &untouched, sizeof(saved)) == 0 &&
        !WOPT_DSL_verify_normalization_forms(PU_Info_tree_ptr(pu), "corrupt", NULL, NULL);
    WN_set_ty(load, saved_ty);
    if (!Check(rejected, "corrupt_operand_rejected_trace_off")) return 1;
  }
  WOPT_DSL_SEMANTIC_INFO info;
  if (!Check(WOPT_DSL_import_semantic_info
          (WN_kid0(norm), DSL_Builder_Get_Value_Result_Symbol(norm), "centered_norm", &info, stderr),
          "wopt_import")) return 1;
  WN *operands[3];
  for (UINT32 i = 0; i < 3; ++i) operands[i] = WN_COPY_Tree(WN_kid(WN_kid0(norm), i));
  if (!Check(WOPT_DSL_emit_WN(&info, WN_kid0(norm), DSL_Builder_Get_Value_Result_Symbol(norm),
          operands, 2, NULL) == NULL, "wopt_wrong_arity")) return 1;
  WN *emitted = WOPT_DSL_emit_WN(&info, WN_kid0(norm),
      DSL_Builder_Get_Value_Result_Symbol(norm), operands, 3, stderr);
  if (!Check(emitted != NULL, "wopt_emit_ternary")) return 1;
  WN_kid0(norm) = emitted;
  if (!Check(WOPT_DSL_characterize_normalization
          (PU_Info_tree_ptr(pu), norm, &again, stderr) &&
          memcmp(&fact, &again, sizeof(fact)) == 0, "ternary_roundtrip_same_facts") ||
      !Check(DSL_Gatekeeper_Verify_PU(pu, stderr, &gate), "emitted_gatekeeper") ||
      !Check(!WOPT_DSL_verify_normalization_forms(WN_func_body(PU_Info_tree_ptr(pu)),
          "region", NULL, NULL), "non_pu_scope_rejected")) return 1;
  DSL_BUILDER_MAPPED_IMAGE_REQUEST request;
  request.path = argv[1];
  request.flags = 0;
  if (!Check(DSL_Builder_Finalize_Mapped_Image(&request), "mapped_image")) return 1;
  return 0;
}
