/* Native R2 numerical binding tests and mapped-image producer.
 * Design: doc/AI-COMPILER-REDUCTION-NUMERICAL-BINDING-v0.1.md.
 * This checks representation and preservation, not reduction execution.
 */
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
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
static void reduction_test_initialize(void)
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
static TY_IDX reduction_test_tensor(const char *name, UINT32 rank, const char *shape)
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
static BOOL reduction_test_check(BOOL condition, const char *name)
{
  printf("contract %s %s\n", name, condition ? "PASS" : "FAIL");
  return condition;
}


/* Exercise explicit v2 binding, v1 refusal and target-independent legal recipes.
 * The image remains high-level WHIRL; no executable kernel is fabricated. */
int main(int argc, char **argv)
{
  if (argc != 7) {
    fprintf(stderr, "usage: %s output.B query.t version width chunk budget\n", argv[0]);
    return 2;
  }
  UINT16 version = (UINT16)atoi(argv[3]);
  UINT64 width = strtoull(argv[4], NULL, 10);
  UINT64 chunk = strtoull(argv[5], NULL, 10);
  UINT64 budget = strtoull(argv[6], NULL, 10);
  if ((version != 1 && version != 2) || width == 0) return 2;
  reduction_test_initialize();
  if (!DSL_Builder_Begin_Program()) return 1;
  DSL_BUILDER_PROGRAM_UNIT pu = DSL_Builder_Create_Minimal_PU("ordered_row_sum");
  if (pu == NULL || !DSL_Builder_Select_PU(pu)) return 1;
  Current_PU_Info = pu;
  char shape[80];
  snprintf(shape, sizeof(shape), "[3,%llu]", (unsigned long long)width);
  TY_IDX input_ty = reduction_test_tensor("input", 2, shape);
  TY_IDX result_ty = reduction_test_tensor("output", 1, "[3]");
  DSL_BUILDER_SOURCE_POSITION pos;
  memset(&pos, 0, sizeof(pos));
  pos.file_id = DSL_Builder_Register_Source_File(pu, __FILE__);
  pos.line = __LINE__ + 1;
  DSL_BUILDER_VALUE input = DSL_Builder_Declare_PU_Formal(pu, "x", 0, input_ty, &pos);
  if (input == NULL || DSL_Builder_Declare_PU_Result
      (pu, "y", 0, result_ty, DSL_PU_RESULT_TENSOR, &pos) == NULL) return 1;
  DSL_Opcode_Register_Common_Substrate();
  DSL_OPCODE_ID opcode = DSL_Opcode_Find(DSL_Domain_Find("common"), "common.reduce_sum", version);
  DSL_BUILDER_OPERATOR_ATTRIBUTE attrs[9] = {
    { "attr.axis", "1" }, { "attr.keepdim", "false" },
    { "attr.accum_dtype", "float32" }, { "attr.reassociate", "false" },
    { "attr.order", "ascending_left_fold" }, { "attr.identity_bits", "00000000" },
    { "attr.fp_environment", "rne_gradual_no_traps" }, { "attr.nan_policy", "class_only" },
    { "attr.extra", "not_allowed" }
  };
  DSL_IR_IMAGE_HEADER before, after;
  DSL_IR_Image_Get_Header(&before);
  UINT32 symbols = ST_Table_Size(CURRENT_SYMTAB);
  if (version == 2) {
    const char *bad[8] = { "-1", "true", "float64", "true", "tree", "80000000", "ftz", "ignore_nan" };
    for (UINT32 i = 0; i < 8; ++i) {
      const char *saved = attrs[i].value;
      attrs[i].value = bad[i];
      BOOL rejected = DSL_Builder_Create_Operator_With_Result
          (opcode, 2, &input, 1, attrs, 8, "invalid", result_ty) == NULL;
      attrs[i].value = saved;
      if (!reduction_test_check(rejected, attrs[i].name)) return 1;
    }
    for (UINT32 n = 4; n <= 9; ++n) {
      if (n == 8) continue;
      if (!reduction_test_check(DSL_Builder_Create_Operator_With_Result
          (opcode, 2, &input, 1, attrs, n, "incomplete_or_extra", result_ty) == NULL,
          "exact_schema")) return 1;
    }
    DSL_IR_Image_Get_Header(&after);
    if (!reduction_test_check(memcmp(&before, &after, sizeof(before)) == 0 &&
          ST_Table_Size(CURRENT_SYMTAB) == symbols, "rejection_preserves_image")) return 1;
  }
  pos.line = __LINE__ + 1;
  DSL_BUILDER_VALUE sum = DSL_Builder_Create_Operator_With_Result
      (opcode, version, &input, 1, attrs, version == 1 ? 4 : 8, "ordered_sum", result_ty);
  if (sum == NULL || !DSL_Builder_Set_Value_Source_Position(sum, &pos) ||
      !DSL_Builder_Append_PU_Value(pu, sum) || !DSL_Builder_Return_PU_Values(pu, &sum, 1)) return 1;
  WN *scope = PU_Info_tree_ptr(pu);
  WOPT_DSL_REDUCTION_ALTERNATIVES plans, again, sentinel, saved;
  memset(&sentinel, 0x5a, sizeof(sentinel));
  saved = sentinel;
  DSL_IR_Image_Get_Header(&before);
  if (!reduction_test_check(!WOPT_DSL_query_reduction_alternatives
      (scope, sum, 0, budget, &sentinel, NULL, NULL) &&
      memcmp(&sentinel, &saved, sizeof(saved)) == 0, "zero_chunk_untouched") ||
      !reduction_test_check(!WOPT_DSL_query_reduction_alternatives
      (WN_func_body(scope), sum, chunk, budget, &sentinel, NULL, NULL) &&
      memcmp(&sentinel, &saved, sizeof(saved)) == 0, "scope_untouched") ||
      !reduction_test_check(!WOPT_DSL_query_reduction_alternatives
      (scope, WN_COPY_Tree(sum), chunk, budget, &sentinel, NULL, NULL) &&
      memcmp(&sentinel, &saved, sizeof(saved)) == 0, "nonmember_untouched") ||
      !reduction_test_check(!WOPT_DSL_query_reduction_alternatives
      (scope, sum, chunk, budget, NULL, NULL, NULL), "null_output")) return 1;
  FILE *trace = fopen(argv[2], "w");
  if (trace == NULL) return 1;
  BOOL ok = WOPT_DSL_query_reduction_alternatives(scope, sum, chunk, budget, &plans, stderr, trace);
  fclose(trace);
  if (!reduction_test_check(ok, "alternatives_query")) return 1;
  UINT64 effective = chunk < width ? chunk : width;
  if (!reduction_test_check(plans.fact.recipe == version && plans.chunk_elements == effective &&
      plans.scratch_bytes == 4 * effective && plans.scratch_budget_bytes == budget &&
      plans.direct_status == (version == 1 ? WOPT_DSL_REDUCE_SOURCE_UNBOUND : WOPT_DSL_REDUCE_LEGAL_RECIPE) &&
      plans.staged_status == (version == 1 ? WOPT_DSL_REDUCE_SOURCE_UNBOUND :
          (4 * effective <= budget ? WOPT_DSL_REDUCE_LEGAL_RECIPE : WOPT_DSL_REDUCE_STAGING_BUDGET)) &&
      plans.tree_status == WOPT_DSL_REDUCE_REASSOC_FORBIDDEN, "legality_and_scratch")) return 1;
  if (!reduction_test_check(WOPT_DSL_query_reduction_alternatives
      (scope, sum, chunk, budget, &again, stderr, NULL) &&
      memcmp(&plans, &again, sizeof(plans)) == 0, "trace_off_same_facts")) return 1;
  ST_tensor_bind_metadata(DSL_Builder_Get_Value_Result_Symbol(sum), "test.order", "tree");
  if (!reduction_test_check(WOPT_DSL_query_reduction_alternatives
      (scope, sum, chunk, budget, &again, stderr, NULL) &&
      memcmp(&plans, &again, sizeof(plans)) == 0, "metadata_not_binding")) return 1;
  DSL_IR_Image_Get_Header(&after);
  if (!reduction_test_check(memcmp(&before, &after, sizeof(before)) == 0, "query_read_only")) return 1;
  WOPT_DSL_SEMANTIC_INFO info;
  if (!reduction_test_check(WOPT_DSL_import_semantic_info(WN_kid0(sum),
      DSL_Builder_Get_Value_Result_Symbol(sum), "ordered_row_sum", &info, stderr) &&
      info.version == version, "wopt_version_identity")) return 1;
  WN *kid[1] = { WN_COPY_Tree(WN_kid0(WN_kid0(sum))) };
  WN *emitted = WOPT_DSL_emit_WN(&info, WN_kid0(sum),
      DSL_Builder_Get_Value_Result_Symbol(sum), kid, 1, stderr);
  if (!reduction_test_check(emitted != NULL, "wopt_emit")) return 1;
  WN_kid0(sum) = emitted;
  if (!reduction_test_check(WOPT_DSL_query_reduction_alternatives
      (scope, sum, chunk, budget, &again, stderr, NULL) &&
      memcmp(&plans, &again, sizeof(plans)) == 0, "emission_preserves_binding")) return 1;
  DSL_BUILDER_MAPPED_IMAGE_REQUEST request;
  request.path = argv[1];
  request.flags = 0;
  return reduction_test_check(DSL_Builder_Finalize_Mapped_Image(&request), "mapped_image") ? 0 : 1;
}
