/* Native R2 numerical binding tests and mapped-image producer.
 * Design: doc/AI-COMPILER-REDUCTION-DIRECT-BINDING-v0.1.md.
 * This checks representation and preservation, not reduction execution.
 */
#include <stdio.h>
#include "../../../common/ir_image/dsl_image_service.h"
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
#include "opt_dsl_reduction.h"

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



/* Bind actual source WHIRL to documented scalar arithmetic and symbolic host
 * contracts; refusal tests never masquerade as executed runtime proofs. */
int main(int argc, char **argv)
{
  if (argc != 7 && argc != 8) {
    fprintf(stderr, "usage: %s output.B direct.t arch version width layout [guard.bin]\n", argv[0]);
    return 2;
  }
  UINT32 version = atoi(argv[4]);
  UINT64 width = strtoull(argv[5], NULL, 10);
  if ((version != 1 && version != 2) || width == 0 || width > 65) return 2;
  reduction_test_initialize();
  if (!DSL_Builder_Begin_Program()) return 1;
  DSL_BUILDER_PROGRAM_UNIT pu = DSL_Builder_Create_Minimal_PU("direct_ordered_row_sum");
  if (pu == NULL || !DSL_Builder_Select_PU(pu)) return 1;
  Current_PU_Info = pu;
  char shape[80];
  snprintf(shape, sizeof(shape), "[3,%llu]", (unsigned long long)width);
  DSL_BUILDER_TENSOR_DESCRIPTOR desc;
  memset(&desc, 0, sizeof(desc));
  desc.type_core.kind = "tensor";
  desc.type_core.dtype = "float32";
  desc.type_core.rank = 2;
  desc.type_core.logical_shape = shape;
  desc.representation.layout = argv[6];
  TY_IDX input_ty = DSL_Builder_Intern_Tensor_Type("input", MTYPE_To_TY(MTYPE_F4), &desc);
  desc.type_core.rank = 1;
  desc.type_core.logical_shape = "[3]";
  TY_IDX result_ty = DSL_Builder_Intern_Tensor_Type("output", MTYPE_To_TY(MTYPE_F4), &desc);
  DSL_BUILDER_SOURCE_POSITION pos;
  memset(&pos, 0, sizeof(pos));
  pos.file_id = DSL_Builder_Register_Source_File(pu, __FILE__);
  pos.line = __LINE__ + 1;
  DSL_BUILDER_VALUE input = DSL_Builder_Declare_PU_Formal(pu, "x", 0, input_ty, &pos);
  if (input == NULL || DSL_Builder_Declare_PU_Result
      (pu, "y", 0, result_ty, DSL_PU_RESULT_TENSOR, &pos) == NULL) return 1;
  DSL_Opcode_Register_Common_Substrate();
  DSL_OPCODE_ID opcode = DSL_Opcode_Find(DSL_Domain_Find("common"), "common.reduce_sum", version);
  DSL_BUILDER_OPERATOR_ATTRIBUTE attrs[8] = {
    { "attr.axis", "1" }, { "attr.keepdim", "false" }, { "attr.accum_dtype", "float32" },
    { "attr.reassociate", "false" }, { "attr.order", "ascending_left_fold" },
    { "attr.identity_bits", "00000000" }, { "attr.fp_environment", "rne_gradual_no_traps" },
    { "attr.nan_policy", "class_only" }
  };
  pos.line = __LINE__ + 1;
  DSL_BUILDER_VALUE sum = DSL_Builder_Create_Operator_With_Result
      (opcode, version, &input, 1, attrs, version == 1 ? 4 : 8, "sum_value", result_ty);
  if (sum == NULL || !DSL_Builder_Set_Value_Source_Position(sum, &pos) ||
      !DSL_Builder_Append_PU_Value(pu, sum) || !DSL_Builder_Return_PU_Values(pu, &sum, 1)) return 1;
  WN *scope = PU_Info_tree_ptr(pu);
  WOPT_DSL_REDUCTION_DIRECT_REQUEST req;
  memset(&req, 0, sizeof(req));
  if (!WOPT_DSL_characterize_reduction(scope, sum, &req.expected_source, stderr)) return 1;
  req.architecture = argv[3];
  req.instruction = WOPT_DSL_REDUCE_PTX_ADD_RN_F32;
  req.runtime_contract_id = 101;
  req.context_id = 201;
  req.input.allocation_id = 1;
  req.input.allocation_bytes = 1024;
  req.input.base_alignment = 16;
  req.input.lease = WOPT_DSL_REDUCE_INPUT_SNAPSHOT;
  req.input.memory_space = WOPT_DSL_REDUCE_BUFFER_DEVICE_GLOBAL;
  req.output.allocation_id = 2;
  req.output.allocation_bytes = 1024;
  req.output.base_alignment = 16;
  req.output.lease = WOPT_DSL_REDUCE_OUTPUT_EXCLUSIVE;
  req.output.memory_space = WOPT_DSL_REDUCE_BUFFER_DEVICE_GLOBAL;
  req.row_mapping = WOPT_DSL_REDUCE_ROW_LINEAR_THREAD;
  req.launched_threads = 4;
  req.before[0] = 1U << 1;
  req.before[1] = 1U << 2;
  req.before[2] = (1U << 3) | (1U << 4);
  req.before[3] = 1U << 5;
  WOPT_DSL_REDUCTION_DIRECT_BINDING plan, again, untouched, saved;
  memset(&untouched, 0x5a, sizeof(untouched));
  saved = untouched;
  DSL_IR_IMAGE_HEADER before, after;
  DSL_IR_Image_Get_Header(&before);
  UINT32 symbols = ST_Table_Size(CURRENT_SYMTAB), types = TY_Table_Size();
  if (!reduction_test_check(!WOPT_DSL_bind_reduction_direct(scope, sum, NULL, &untouched, NULL, NULL) &&
      memcmp(&saved, &untouched, sizeof(saved)) == 0, "null_request_untouched") ||
      !reduction_test_check(!WOPT_DSL_bind_reduction_direct(scope, sum, &req, NULL, NULL, NULL),
      "null_output") ||
      !reduction_test_check(!WOPT_DSL_bind_reduction_direct(WN_func_body(scope), sum, &req,
      &untouched, NULL, NULL) && memcmp(&saved, &untouched, sizeof(saved)) == 0, "scope_untouched") ||
      !reduction_test_check(!WOPT_DSL_bind_reduction_direct(scope, WN_COPY_Tree(sum), &req,
      &untouched, NULL, NULL) && memcmp(&saved, &untouched, sizeof(saved)) == 0, "nonmember_untouched"))
    return 1;
  FILE *trace = fopen(argv[2], "w");
  if (trace == NULL) return 1;
  BOOL ok = WOPT_DSL_bind_reduction_direct(scope, sum, &req, &plan, stderr, trace);
  UINT32 expected = version == 1 ? WOPT_DSL_REDUCE_DIRECT_SOURCE_UNBOUND :
      (strcmp(argv[6], "row_major") == 0 ? WOPT_DSL_REDUCE_DIRECT_BOUND :
                                           WOPT_DSL_REDUCE_DIRECT_LAYOUT_UNSUPPORTED);
  if (!reduction_test_check(ok && plan.status == expected, "baseline_binding") ||
      !reduction_test_check(WOPT_DSL_bind_reduction_direct(scope, sum, &req, &again, stderr, NULL) &&
      memcmp(&plan, &again, sizeof(plan)) == 0, "trace_off_same_binding")) return 1;
  OPEN64_REDUCTION_GUARD guard, guard_saved;
  memset(&guard, 0x5a, sizeof(guard));
  guard_saved = guard;
  BOOL exported = WOPT_DSL_export_reduction_direct_guard(scope, sum, &plan, &guard, stderr, trace);
  if (!reduction_test_check(exported == (expected == WOPT_DSL_REDUCE_DIRECT_BOUND),
      "export_only_bound") || (!exported && !reduction_test_check(
      memcmp(&guard, &guard_saved, sizeof(guard)) == 0, "refused_export_untouched"))) return 1;
  if (exported) {
    OPEN64_REDUCTION_GUARD marker = guard_saved;
    for (unsigned test = 0; test < 6; ++test) {
      WOPT_DSL_REDUCTION_DIRECT_BINDING bad = plan;
      if (test == 0) ++bad.source.node_id;
      if (test == 1) ++bad.input_bytes;
      if (test == 2) bad.stages_bound = 0;
      if (test == 3) bad.fp_options = 1;
      if (test == 4) bad.before[0] = 0;
      if (test == 5) bad.runtime_obligations = 0;
      if (!reduction_test_check(!WOPT_DSL_export_reduction_direct_guard(scope, sum, &bad,
          &guard_saved, NULL, NULL) && memcmp(&guard_saved, &marker, sizeof(marker)) == 0,
          "stale_export_untouched")) return 1;
    }
    if (argc == 8) {
      FILE *file = fopen(argv[7], "wb");
      if (!file || fwrite(&guard, sizeof(guard), 1, file) != 1 || fclose(file)) return 1;
    }
  }
  if (expected == WOPT_DSL_REDUCE_DIRECT_BOUND) {
    if (!reduction_test_check(plan.stages_bound == 15 && plan.runtime_obligations == 15 &&
        plan.input_bytes == 12 * width && plan.output_bytes == 12 &&
        plan.runtime_contract_id == req.runtime_contract_id && plan.context_id == req.context_id &&
        plan.input.allocation_id == req.input.allocation_id &&
        plan.output.allocation_id == req.output.allocation_id &&
        plan.instruction == req.instruction && plan.fp_options == req.fp_options &&
        plan.row_mapping == req.row_mapping && plan.launched_threads == req.launched_threads &&
        memcmp(plan.before, req.before, sizeof(plan.before)) == 0, "exact_contract_footprints")) return 1;
    for (UINT32 test = 0; test < 31; ++test) {
      WOPT_DSL_REDUCTION_DIRECT_REQUEST bad = req;
      UINT32 status = WOPT_DSL_REDUCE_DIRECT_EVENTS_INVALID;
      switch (test) {
      case 0: ++bad.expected_source.node_id; status = WOPT_DSL_REDUCE_DIRECT_STALE_SOURCE; break;
      case 1: bad.architecture = "sm_999"; status = WOPT_DSL_REDUCE_DIRECT_TARGET_UNKNOWN; break;
      case 2: bad.architecture = NULL; status = WOPT_DSL_REDUCE_DIRECT_TARGET_UNKNOWN; break;
      case 3: bad.instruction = 0; status = WOPT_DSL_REDUCE_DIRECT_INSTRUCTION_UNSUPPORTED; break;
      case 4: case 5: case 6: case 7: case 8: case 9:
        bad.fp_options = 1U << (test - 4); status = WOPT_DSL_REDUCE_DIRECT_FP_OPTIONS_UNSAFE; break;
      case 10: bad.fp_options = 1U << 31; status = WOPT_DSL_REDUCE_DIRECT_FP_OPTIONS_UNSAFE; break;
      case 11: bad.input.allocation_id = 0; status = WOPT_DSL_REDUCE_DIRECT_RANGE_INVALID; break;
      case 12: bad.input.offset_bytes = ~(UINT64)0; status = WOPT_DSL_REDUCE_DIRECT_RANGE_INVALID; break;
      case 13: bad.output.allocation_bytes = 11; status = WOPT_DSL_REDUCE_DIRECT_RANGE_INVALID; break;
      case 14: bad.input.base_alignment = 3; status = WOPT_DSL_REDUCE_DIRECT_RANGE_INVALID; break;
      case 15: bad.output.offset_bytes = 2; status = WOPT_DSL_REDUCE_DIRECT_RANGE_INVALID; break;
      case 16: bad.input.memory_space = 0; status = WOPT_DSL_REDUCE_DIRECT_RANGE_INVALID; break;
      case 17: bad.runtime_contract_id = 0; status = WOPT_DSL_REDUCE_DIRECT_LEASE_MISSING; break;
      case 18: bad.context_id = 0; status = WOPT_DSL_REDUCE_DIRECT_LEASE_MISSING; break;
      case 19: bad.input.lease = 0; status = WOPT_DSL_REDUCE_DIRECT_LEASE_MISSING; break;
      case 20: bad.output.lease = 0; status = WOPT_DSL_REDUCE_DIRECT_LEASE_MISSING; break;
      case 21: bad.output.allocation_id = 1; status = WOPT_DSL_REDUCE_DIRECT_ALIAS_OVERLAP; break;
      case 22: bad.row_mapping = 0; status = WOPT_DSL_REDUCE_DIRECT_OWNER_INVALID; break;
      case 23: bad.launched_threads = 2; status = WOPT_DSL_REDUCE_DIRECT_OWNER_INVALID; break;
      case 24: bad.before[0] = 0; break;
      case 25: bad.before[1] = 0; break;
      case 26: bad.before[2] &= ~(1U << 4); break;
      case 27: bad.before[3] = 0; break;
      case 28: bad.before[2] |= 1U << 0; break;
      case 29: bad.before[5] = 1U << 5; break;
      case 30: bad.before[4] = 1U << 31; break;
      }
      if (!WOPT_DSL_bind_reduction_direct(scope, sum, &bad, &again, stderr, trace) ||
          !reduction_test_check(again.status == status && again.runtime_obligations == 0,
                                "refused_contract")) return 1;
    }
    WOPT_DSL_REDUCTION_DIRECT_REQUEST adjacent = req;
    adjacent.output.allocation_id = 1;
    adjacent.output.offset_bytes = plan.input_bytes;
    if (!reduction_test_check(WOPT_DSL_bind_reduction_direct(scope, sum, &adjacent, &again,
        stderr, trace) && again.status == WOPT_DSL_REDUCE_DIRECT_BOUND, "adjacent_same_allocation")) return 1;
    adjacent.output.offset_bytes -= 4;
    if (!reduction_test_check(WOPT_DSL_bind_reduction_direct(scope, sum, &adjacent, &again,
        stderr, trace) && again.status == WOPT_DSL_REDUCE_DIRECT_ALIAS_OVERLAP, "overlap_last_element"))
      return 1;
    WOPT_DSL_REDUCTION_DIRECT_REQUEST transitive = req;
    transitive.before[2] = 1U << 3;
    transitive.before[3] = (1U << 4) | (1U << 5);
    if (!reduction_test_check(WOPT_DSL_bind_reduction_direct(scope, sum, &transitive,
        &again, stderr, trace) && again.status == WOPT_DSL_REDUCE_DIRECT_BOUND,
        "transitive_release_order")) return 1;
  }
  fclose(trace);
  DSL_IR_Image_Get_Header(&after);
  if (!reduction_test_check(memcmp(&before, &after, sizeof(before)) == 0 &&
      symbols == ST_Table_Size(CURRENT_SYMTAB) && types == TY_Table_Size(), "read_only")) return 1;
  DSL_BUILDER_MAPPED_IMAGE_REQUEST image;
  image.path = argv[1];
  image.flags = 0;
  return reduction_test_check(DSL_Builder_Finalize_Mapped_Image(&image), "mapped_image") ? 0 : 1;
}
