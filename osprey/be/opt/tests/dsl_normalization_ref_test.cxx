/* Copyright (C) 2026 Open64 Project */

/* Explicit native LN-REF-1 source producer and admission/PREOPT bridge tests.
 * Design: doc/AI-COMPILER-LAYERNORM-NATIVE-REFERENCE-CONTRACT-v0.1.md.
 * This authors v2 meaning directly; it neither upgrades Python/v1 nor executes
 * normalization, selects a plan, or links reduction optimization services. */
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

/* Standalone native tools install no backend signal cleanup action. */
void Signal_Cleanup(INT sig) {}

/* Native test diagnostics do not need host-specific parameter formatting. */
const char *Host_Format_Parm(INT kind, MEM_PTR parm) { return ""; }

/* Initialize normal WHIRL tables once for this fresh producer process. */
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

/* Request real native descriptors, including intentionally malformed cases;
 * no sampled tensor or alternative IR is constructed. */
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
  return DSL_Builder_Intern_Tensor_Type(name, MTYPE_To_TY(element), &descriptor);
}

/* Author the complete explicit reference declaration, not a source proof tag.
 * The sixteenth slot is reserved for extra-field rejection tests. */
static void Attributes(DSL_BUILDER_OPERATOR_ATTRIBUTE *attrs, const char *epsilon)
{
  const char *names[16] = { "attr.axis", "attr.accum_dtype", "attr.variance_kind",
    "attr.epsilon_bits", "attr.reassociate", "attr.state_reformulation",
    "attr.contract", "attr.rsqrt_semantics", "attr.mean_order", "attr.mean_identity_bits",
    "attr.mean_finalize", "attr.count_conversion", "attr.fp_environment",
    "attr.nan_policy", "attr.reference_binding", "attr.producer_authorized" };
  const char *values[16] = { "1", "float32", "population_centered", epsilon,
    "false", "false", "false", "sqrt_rne_then_div_rne_f32", "ascending_left_fold",
    "00000000", "sum_then_div_rne", "exact_f32_from_static_n",
    "rne_gradual_no_traps", "class_only", "ln_ref_1", "true" };
  for (UINT32 i = 0; i < 16; ++i) {
    attrs[i].name = names[i];
    attrs[i].value = values[i];
  }
}

/* Report an individually named assertion so durable logs show actual coverage. */
static BOOL Check(BOOL condition, const char *name)
{
  printf("contract %s %s\n", name, condition ? "PASS" : "FAIL");
  return condition;
}

/* Invalid preflight must not allocate product rows, types or symbols. Existing
 * context/registry initialization is outside this snapshot. */
static BOOL Rejected(DSL_OPCODE_ID opcode, UINT16 version, DSL_BUILDER_VALUE *kids,
                     UINT32 kid_count, DSL_BUILDER_OPERATOR_ATTRIBUTE *attrs,
                     UINT32 attr_count, TY_IDX result_ty, const char *name)
{
  DSL_IR_IMAGE_HEADER before, after;
  DSL_IR_Image_Get_Header(&before);
  UINT32 symbols = ST_Table_Size(CURRENT_SYMTAB), types = TY_Table_Size();
  DSL_BUILDER_VALUE result = DSL_Builder_Create_Operator_With_Result
      (opcode, version, kids, kid_count, attrs, attr_count, "ref_invalid", result_ty);
  DSL_IR_Image_Get_Header(&after);
  return Check(result == NULL && memcmp(&before, &after, sizeof(before)) == 0 &&
      symbols == ST_Table_Size(CURRENT_SYMTAB) && types == TY_Table_Size(), name);
}

/* Query only active-PU member WHIRL and verify immutable, version-specific
 * characterization; target/storage/accuracy proofs must remain outstanding. */
static BOOL Verify_Facts(DSL_BUILDER_PROGRAM_UNIT pu, DSL_BUILDER_VALUE value,
                         UINT64 rows, UINT64 width, UINT32 epsilon, UINT32 line)
{
  WOPT_DSL_NORMALIZATION_CHARACTERIZATION fact, again, untouched, saved;
  DSL_IR_IMAGE_HEADER before, after;
  DSL_IR_Image_Get_Header(&before);
  UINT32 symbols = ST_Table_Size(CURRENT_SYMTAB), types = TY_Table_Size();
  if (!Check(WOPT_DSL_characterize_normalization
      (PU_Info_tree_ptr(pu), value, &fact, stderr), "ref_characterization") ||
      !Check(fact.recipe == WOPT_DSL_NORM_CENTERED_ORDERED_LN_REF_1 &&
        fact.flags == 0x73f7 && fact.rows == rows && fact.width == width &&
        fact.epsilon_bits == epsilon && fact.source_line == line,
        "ref_known_arithmetic_target_unbound")) return FALSE;
  ST_tensor_bind_metadata(DSL_Builder_Get_Value_Result_Symbol(value),
      "ln.reference_binding", "source_unresolved");
  if (!Check(WOPT_DSL_characterize_normalization
      (PU_Info_tree_ptr(pu), value, &again, stderr) &&
      memcmp(&fact, &again, sizeof(fact)) == 0, "metadata_cannot_unbind_or_promote")) return FALSE;
  DSL_IR_Image_Get_Header(&after);
  if (!Check(memcmp(&before, &after, sizeof(before)) == 0 &&
      symbols == ST_Table_Size(CURRENT_SYMTAB) && types == TY_Table_Size(),
      "ref_characterization_read_only")) return FALSE;
  memset(&untouched, 0x5a, sizeof(untouched));
  memcpy(&saved, &untouched, sizeof(saved));
  if (!Check(!WOPT_DSL_characterize_normalization
      (WN_func_body(PU_Info_tree_ptr(pu)), value, &untouched, NULL) &&
      memcmp(&saved, &untouched, sizeof(saved)) == 0, "ref_region_refused") ||
      !Check(!WOPT_DSL_characterize_normalization
      (PU_Info_tree_ptr(pu), WN_COPY_Tree(value), &untouched, NULL) &&
      memcmp(&saved, &untouched, sizeof(saved)) == 0, "ref_nonmember_refused") ||
      !Check(!WOPT_DSL_characterize_normalization
      (PU_Info_tree_ptr(pu), value, NULL, NULL), "ref_null_output_refused")) return FALSE;
  for (UINT32 i = 0; i < 3; ++i) {
    WN *load = WN_kid(WN_kid0(value), i);
    TY_IDX original = WN_ty(load);
    WN_set_ty(load, MTYPE_To_TY(MTYPE_F4));
    BOOL rejected = !WOPT_DSL_characterize_normalization
        (PU_Info_tree_ptr(pu), value, &untouched, NULL) &&
        memcmp(&saved, &untouched, sizeof(saved)) == 0 &&
        !WOPT_DSL_verify_normalization_forms(PU_Info_tree_ptr(pu), "corrupt", NULL, NULL);
    WN_set_ty(load, original);
    if (!Check(rejected, "ref_corrupt_operand_refused_trace_off")) return FALSE;
  }
  WOPT_DSL_SEMANTIC_INFO info;
  if (!Check(WOPT_DSL_import_semantic_info(WN_kid0(value),
      DSL_Builder_Get_Value_Result_Symbol(value), ST_name(PU_Info_proc_sym(pu)),
      &info, stderr) && info.version == 2 && sizeof(info) == 64,
      "ref_import_version_and_unchanged_key_layout")) return FALSE;
  WN *kids[3];
  for (UINT32 i = 0; i < 3; ++i) kids[i] = WN_COPY_Tree(WN_kid(WN_kid0(value), i));
  if (!Check(WOPT_DSL_emit_WN(&info, WN_kid0(value),
      DSL_Builder_Get_Value_Result_Symbol(value), kids, 2, NULL) == NULL,
      "ref_emit_wrong_arity_refused")) return FALSE;
  WN *emitted = WOPT_DSL_emit_WN(&info, WN_kid0(value),
      DSL_Builder_Get_Value_Result_Symbol(value), kids, 3, stderr);
  if (!Check(emitted != NULL, "ref_emit_three_direct_operands")) return FALSE;
  WN_kid0(value) = emitted;
  return Check(WOPT_DSL_characterize_normalization
      (PU_Info_tree_ptr(pu), value, &again, stderr) &&
      memcmp(&fact, &again, sizeof(fact)) == 0, "ref_emit_roundtrip_same_facts");
}

/* Build the source declaration and negative admission witnesses. Each process
 * owns its context; publication uses the normal mapped-image writer only. */
int main(int argc, char **argv)
{
  if (argc < 2 || argc > 3) return 2;
  const char *variant = argc == 3 ? argv[2] : "default";
  const char *variants[] = { "default", "unit", "wide", "epsilon2", "subnormal",
                            "count-limit", "mixed", "shared-affine", "cross-pu" };
  BOOL known = FALSE;
  for (UINT32 i = 0; i < sizeof(variants)/sizeof(variants[0]); ++i)
    known = known || strcmp(variant, variants[i]) == 0;
  if (!known) return 2;
  UINT64 rows = 3, width = 17;
  if (strcmp(variant, "unit") == 0) rows = width = 1;
  if (strcmp(variant, "wide") == 0) { rows = 7; width = 65; }
  if (strcmp(variant, "count-limit") == 0) { rows = 1; width = 16777216ULL; }
  const char *epsilon = strcmp(variant, "epsilon2") == 0 ? "3a83126f" :
                        strcmp(variant, "subnormal") == 0 ? "00000001" : "3727c5ac";
  Initialize_Context();
  if (!DSL_Builder_Begin_Program()) return 1;
  DSL_BUILDER_PROGRAM_UNIT pu = DSL_Builder_Create_Minimal_PU("ln_ref_1_source");
  if (pu == NULL || !DSL_Builder_Select_PU(pu)) return 1;
  Current_PU_Info = pu;
  char shape[64], affine[64];
  snprintf(shape, sizeof(shape), "[%llu,%llu]", (unsigned long long)rows,
           (unsigned long long)width);
  snprintf(affine, sizeof(affine), "[%llu]", (unsigned long long)width);
  TY_IDX x_ty = Tensor("ref_x", 2, shape), affine_ty = Tensor("ref_affine", 1, affine);
  TY_IDX wrong_ty = Tensor("ref_wrong", 1, "[2]"),
         fp64_ty = Tensor("ref_f64", 2, shape, "float64", MTYPE_F8);
  DSL_BUILDER_SOURCE_POSITION position;
  memset(&position, 0, sizeof(position));
  position.file_id = DSL_Builder_Register_Source_File(pu, __FILE__);
  position.line = __LINE__ + 1;
  position.statement_begin = 1;
  DSL_BUILDER_VALUE kids[3] = {
    DSL_Builder_Declare_PU_Formal(pu, "x", 0, x_ty, &position),
    DSL_Builder_Declare_PU_Formal(pu, "gamma", 1, affine_ty, &position),
    DSL_Builder_Declare_PU_Formal(pu, "beta", 2, affine_ty, &position)
  };
  if (kids[0] == NULL || kids[1] == NULL || kids[2] == NULL) return 1;
  if (strcmp(variant, "shared-affine") == 0) kids[2] = kids[1];
  DSL_Opcode_Register_Common_Substrate();
  DSL_DOMAIN_ID domain = DSL_Domain_Find("common");
  DSL_OPCODE_ID opcode = DSL_Opcode_Find(domain, "common.mean_variance_norm", 2),
                legacy = DSL_Opcode_Find(domain, "common.mean_variance_norm", 1);
  DSL_BUILDER_OPERATOR_ATTRIBUTE attrs[16];
  Attributes(attrs, epsilon);
  if (!Check(opcode != DSL_OPCODE_INVALID_ID && legacy != DSL_OPCODE_INVALID_ID &&
      opcode != legacy && OPR_DSLMEANVARNORM == 39 &&
      DSL_Operator_Find("common.mean_variance_norm", 25, 3) == OPR_DSLUNKNOWN,
      "ref_exact_version_registry")) return 1;
  if (strcmp(variant, "cross-pu") == 0) {
    DSL_BUILDER_PROGRAM_UNIT other = DSL_Builder_Create_Minimal_PU("ln_other");
    if (other == NULL || !DSL_Builder_Select_PU(other)) return 1;
    Current_PU_Info = other;
    DSL_BUILDER_VALUE own[3] = {
      DSL_Builder_Declare_PU_Formal(other, "own_x", 0, x_ty, &position),
      DSL_Builder_Declare_PU_Formal(other, "own_gamma", 1, affine_ty, &position),
      DSL_Builder_Declare_PU_Formal(other, "own_beta", 2, affine_ty, &position)
    };
    for (UINT32 i = 0; i < 3; ++i) {
      DSL_BUILDER_VALUE mixed[3] = { own[0], own[1], own[2] };
      mixed[i] = kids[i];
      if (!Rejected(opcode, 2, mixed, 3, attrs, 15, x_ty, "ref_foreign_operand")) return 1;
    }
    return 0;
  }
  if (!Check(DSL_Builder_Create_Operator(opcode, 2, kids, 3, attrs, 15) == NULL,
      "ref_explicit_result_required") ||
      !Rejected(opcode, 2, kids, 2, attrs, 15, x_ty, "ref_wrong_arity") ||
      !Rejected(opcode, 2, NULL, 3, attrs, 15, x_ty, "ref_null_kids") ||
      !Rejected(opcode, 1, kids, 3, attrs, 15, x_ty, "ref_opcode_version_mismatch") ||
      !Rejected(opcode, 2, kids, 3, attrs, 8, x_ty, "ref_v1_schema_not_upgraded") ||
      !Rejected(legacy, 1, kids, 3, attrs, 15, x_ty, "ref_fields_not_accepted_by_v1") ||
      !Rejected(opcode, 2, kids, 3, attrs, 16, x_ty, "ref_authorization_string_not_a_proof") ||
      !Rejected(opcode, 2, kids, 3, attrs, 15, wrong_ty, "ref_result_shape") ||
      !Rejected(opcode, 2, kids, 3, attrs, 15, fp64_ty, "ref_result_dtype")) return 1;
  for (UINT32 i = 0; i < 15; ++i) {
    const char *saved = attrs[i].value;
    attrs[i].value = i == 3 ? "7f800000" : "invalid_reference_field";
    if (!Rejected(opcode, 2, kids, 3, attrs, 15, x_ty, attrs[i].name)) return 1;
    attrs[i].value = NULL;
    if (!Rejected(opcode, 2, kids, 3, attrs, 15, x_ty, "ref_null_attribute_value")) return 1;
    attrs[i].value = saved;
    DSL_BUILDER_OPERATOR_ATTRIBUTE missing[15];
    UINT32 next = 0;
    for (UINT32 j = 0; j < 15; ++j) if (j != i) missing[next++] = attrs[j];
    if (!Rejected(opcode, 2, kids, 3, missing, 14, x_ty, "ref_missing_attribute")) return 1;
    const char *name = attrs[i].name;
    attrs[i].name = attrs[(i + 1) % 15].name;
    if (!Rejected(opcode, 2, kids, 3, attrs, 15, x_ty, "ref_duplicate_attribute")) return 1;
    attrs[i].name = name;
  }
  UINT32 count_bits = 0xdeadbeef;
  if (!Check(!DSL_mean_variance_norm_ref_count_bits(0, &count_bits) &&
      !DSL_mean_variance_norm_ref_count_bits(16777217ULL, &count_bits) &&
      count_bits == 0xdeadbeef, "ref_invalid_count_output_unchanged") ||
      !Check(DSL_mean_variance_norm_ref_count_bits(16777216ULL, &count_bits) &&
      count_bits == 0x4b800000U, "ref_count_limit_exact") ||
      !Check(DSL_mean_variance_norm_ref_count_bits(17, &count_bits) &&
      count_bits == 0x41880000U, "ref_count_17_exact")) return 1;
  TY_IDX too_wide = Tensor("ref_too_wide", 2, "[1,16777217]"),
         too_wide_affine = Tensor("ref_too_wide_affine", 1, "[16777217]");
  if (!Check(!DSL_mean_variance_norm_ref_types_valid(too_wide, too_wide_affine,
      too_wide_affine, too_wide, "1", "float32", "population_centered", epsilon,
      "false", "false", "false", "sqrt_rne_then_div_rne_f32") &&
      DSL_mean_variance_norm_types_valid(too_wide, too_wide_affine, too_wide_affine,
      too_wide, "1", "float32", "population_centered", epsilon,
      "false", "false", "false", "source_unresolved"),
      "ref_width_bound_does_not_narrow_v1")) return 1;
  TY_IDX overflow = Tensor("ref_overflow", 2, "[18446744073709551615,17]"),
         seventeen = Tensor("ref_seventeen", 1, "[17]");
  if (!Check(!DSL_mean_variance_norm_ref_types_valid(overflow, seventeen, seventeen,
      overflow, "1", "float32", "population_centered", epsilon,
      "false", "false", "false", "sqrt_rne_then_div_rne_f32"),
      "ref_footprint_overflow")) return 1;
  const char *invalid_shapes[] = { "[0,17]", "[3,0]", "[-1,17]", "[3,-1]",
      "[R,17]", "[3,N]", "[3,16777217]", "[3,17,1]", "[17]" };
  for (UINT32 i = 0; i < sizeof(invalid_shapes)/sizeof(invalid_shapes[0]); ++i) {
    TY_IDX invalid = Tensor("ref_invalid_shape", 2, invalid_shapes[i]);
    if (!Check(!DSL_mean_variance_norm_ref_types_valid(invalid, seventeen, seventeen,
        invalid, "1", "float32", "population_centered", epsilon,
        "false", "false", "false", "sqrt_rne_then_div_rne_f32"),
        "ref_nonstatic_or_malformed_descriptor")) return 1;
  }
  DSL_BUILDER_VALUE values[4];
  UINT32 value_count = 0;
  if (DSL_Builder_Declare_PU_Result(pu, "y", 0, x_ty,
      DSL_PU_RESULT_TENSOR, &position) == NULL) return 1;
  position.line = __LINE__ + 1;
  DSL_BUILDER_VALUE ref = DSL_Builder_Create_Operator_With_Result
      (opcode, 2, kids, 3, attrs, 15, "ln_ref_value", x_ty);
  if (ref == NULL || !DSL_Builder_Set_Value_Source_Position(ref, &position) ||
      !DSL_Builder_Append_PU_Value(pu, ref)) return 1;
  values[value_count++] = ref;
  UINT32 reference_line = position.line;
  UINT32 epsilon_bits;
  if (!DSL_positive_f32_bits_valid(epsilon, &epsilon_bits)) return 1;
  if (strcmp(variant, "mixed") == 0) {
    DSL_BUILDER_OPERATOR_ATTRIBUTE v1[8];
    for (UINT32 i = 0; i < 8; ++i) v1[i] = attrs[i];
    v1[7].value = "source_unresolved";
    for (UINT32 i = 1; i < 4; ++i) {
      if (DSL_Builder_Declare_PU_Result(pu, i == 1 ? "legacy_y" : i == 2 ? "reordered_y" : "epsilon_y",
          i, x_ty, DSL_PU_RESULT_TENSOR, &position) == NULL) return 1;
      DSL_BUILDER_OPERATOR_ATTRIBUTE reordered[15];
      for (UINT32 j = 0; j < 15; ++j) reordered[j] = attrs[14 - j];
      DSL_BUILDER_OPERATOR_ATTRIBUTE changed[16];
      Attributes(changed, "3f800000");
      position.line = __LINE__ + 1;
      DSL_BUILDER_VALUE value = DSL_Builder_Create_Operator_With_Result
          (i == 1 ? legacy : opcode, i == 1 ? 1 : 2, kids, 3,
           i == 1 ? v1 : i == 2 ? reordered : changed, i == 1 ? 8 : 15,
           i == 1 ? "legacy_value" : i == 2 ? "reordered_value" : "epsilon_value", x_ty);
      if (value == NULL || !DSL_Builder_Set_Value_Source_Position(value, &position) ||
          !DSL_Builder_Append_PU_Value(pu, value)) return 1;
      values[value_count++] = value;
      WOPT_DSL_SEMANTIC_INFO first, next;
      if (!WOPT_DSL_import_semantic_info(WN_kid0(ref), DSL_Builder_Get_Value_Result_Symbol(ref),
          "ln_ref_1_source", &first, stderr) ||
          !WOPT_DSL_import_semantic_info(WN_kid0(value), DSL_Builder_Get_Value_Result_Symbol(value),
          "ln_ref_1_source", &next, stderr)) return 1;
      if (!Check(i == 1 ? first.version != next.version : i == 2 ?
          first.canonical_attribute_hash == next.canonical_attribute_hash :
          first.canonical_attribute_hash != next.canonical_attribute_hash,
          i == 1 ? "ref_v1_v2_identity_distinct" : i == 2 ?
          "ref_attribute_order_not_semantic" : "ref_epsilon_identity_distinct")) return 1;
    }
  }
  if (!DSL_Builder_Return_PU_Values(pu, values, value_count)) return 1;
  if (!Verify_Facts(pu, ref, rows, width, epsilon_bits, reference_line)) return 1;
  if (strcmp(variant, "mixed") == 0) {
    WOPT_DSL_NORMALIZATION_CHARACTERIZATION v1_fact;
    if (!Check(WOPT_DSL_characterize_normalization
        (PU_Info_tree_ptr(pu), values[1], &v1_fact, stderr) && v1_fact.flags == 0x1fff &&
        v1_fact.recipe == WOPT_DSL_NORM_CENTERED_POPULATION_F32_ROWS_V1,
        "ref_v1_unknowns_preserved_in_same_pu")) return 1;
  }
  DSL_GATEKEEPER_RESULT gate;
  if (!Check(DSL_Gatekeeper_Verify_PU(pu, stderr, &gate), "ref_final_gatekeeper")) return 1;
  DSL_BUILDER_MAPPED_IMAGE_REQUEST request;
  request.path = argv[1];
  request.flags = 0;
  return Check(DSL_Builder_Finalize_Mapped_Image(&request), "ref_mapped_image") ? 0 : 1;
}
