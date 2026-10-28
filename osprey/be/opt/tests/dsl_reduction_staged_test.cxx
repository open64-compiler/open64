/* Native staged-binding fixture, not a reduction kernel implementation.
 * Design: doc/AI-COMPILER-REDUCTION-STAGED-BINDING-v0.1.md. */
#include <stdio.h>
#include <stdlib.h>
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
#include "dsl_ir_image.h"
#include "opt_dsl_reduction_staged.h"

BOOL Run_vsaopt = FALSE;
INT8 Debug_Level = 0;

/* Standalone producer has no backend signal-cleanup action. */
void Signal_Cleanup(INT sig) {}

/* No host diagnostic parameter formatting is needed by this test. */
const char *Host_Format_Parm(INT kind, MEM_PTR parm) { return ""; }

/* Initialize production native tables and ordinary descriptor construction. */
static void staged_test_initialize()
{
  MEM_Initialize(); Set_Error_Tables(Phases, host_errlist); Init_Error_Handler(10);
  Set_Error_File(NULL); Set_Error_Line(ERROR_LINE_UNKNOWN); Preconfigure();
  Init_Controls_Tbl(); ABI_Name = "n64"; Configure(); IR_reader_init();
  Initialize_Symbol_Tables(TRUE); DST_Init(NULL, 0);
}

/* Report named contract assertions while retaining all negative diagnostics. */
static BOOL staged_test_check(BOOL condition, const char *name)
{
  printf("contract %s %s\n", name, condition ? "PASS" : "FAIL");
  return condition;
}

/* Intern source types through the native tensor API, not test-side TY layouts. */
static TY_IDX staged_test_tensor(const char *name, UINT32 rank, const char *shape,
                                 const char *layout)
{
  DSL_BUILDER_TENSOR_DESCRIPTOR d;
  memset(&d, 0, sizeof(d));
  d.type_core.kind = "tensor"; d.type_core.dtype = "float32";
  d.type_core.rank = rank; d.type_core.logical_shape = shape;
  d.representation.layout = layout;
  return DSL_Builder_Intern_Tensor_Type(name, MTYPE_To_TY(MTYPE_F4), &d);
}

/* Bind native source, inspect all supported slices and retain unchanged WHIRL.
 * Invalid choices are planning refusals, never executed target experiments. */
int main(int argc, char **argv)
{
  if (argc != 9) {
    fprintf(stderr, "usage: %s output.B staged.t arch version rows width chunk layout\n", argv[0]);
    return 2;
  }
  UINT32 version = atoi(argv[4]);
  UINT64 rows = strtoull(argv[5], NULL, 10), width = strtoull(argv[6], NULL, 10);
  UINT64 chunk = strtoull(argv[7], NULL, 10);
  if ((version != 1 && version != 2) || !rows || rows > 33 || !width || width > 65 || !chunk)
    return 2;
  staged_test_initialize();
  if (!DSL_Builder_Begin_Program()) return 1;
  DSL_BUILDER_PROGRAM_UNIT pu = DSL_Builder_Create_Minimal_PU("staged_ordered_sum");
  if (!pu || !DSL_Builder_Select_PU(pu)) return 1;
  Current_PU_Info = pu;
  char shape[80];
  snprintf(shape, sizeof(shape), "[%llu,%llu]", (unsigned long long)rows, (unsigned long long)width);
  TY_IDX input_ty = staged_test_tensor("input", 2, shape, argv[8]);
  snprintf(shape, sizeof(shape), "[%llu]", (unsigned long long)rows);
  TY_IDX output_ty = staged_test_tensor("output", 1, shape, argv[8]);
  DSL_BUILDER_SOURCE_POSITION pos;
  memset(&pos, 0, sizeof(pos));
  pos.file_id = DSL_Builder_Register_Source_File(pu, __FILE__); pos.line = __LINE__ + 1;
  DSL_BUILDER_VALUE input = DSL_Builder_Declare_PU_Formal(pu, "x", 0, input_ty, &pos);
  if (!input || !DSL_Builder_Declare_PU_Result(pu, "y", 0, output_ty, DSL_PU_RESULT_TENSOR, &pos)) return 1;
  DSL_Opcode_Register_Common_Substrate();
  DSL_OPCODE_ID op = DSL_Opcode_Find(DSL_Domain_Find("common"), "common.reduce_sum", version);
  DSL_BUILDER_OPERATOR_ATTRIBUTE attrs[8] = {
    {"attr.axis", "1"}, {"attr.keepdim", "false"}, {"attr.accum_dtype", "float32"},
    {"attr.reassociate", "false"}, {"attr.order", "ascending_left_fold"},
    {"attr.identity_bits", "00000000"}, {"attr.fp_environment", "rne_gradual_no_traps"},
    {"attr.nan_policy", "class_only"}
  };
  pos.line = __LINE__ + 1;
  DSL_BUILDER_VALUE sum = DSL_Builder_Create_Operator_With_Result
      (op, version, &input, 1, attrs, version == 1 ? 4 : 8, "sum", output_ty);
  if (!sum || !DSL_Builder_Set_Value_Source_Position(sum, &pos) ||
      !DSL_Builder_Append_PU_Value(pu, sum) || !DSL_Builder_Return_PU_Values(pu, &sum, 1)) return 1;
  WN *scope = PU_Info_tree_ptr(pu);
  WOPT_DSL_REDUCTION_STAGED_REQUEST req;
  memset(&req, 0, sizeof(req));
  if (!WOPT_DSL_characterize_reduction(scope, sum, &req.base.expected_source, stderr)) return 1;
  req.base.architecture = argv[3]; req.base.instruction = WOPT_DSL_REDUCE_PTX_ADD_RN_F32;
  req.base.runtime_contract_id = 301; req.base.context_id = 201;
  req.base.input.allocation_id = 1; req.base.input.allocation_bytes = 4 * rows * width + 256;
  req.base.input.offset_bytes = 16; req.base.input.base_alignment = 16;
  req.base.input.lease = WOPT_DSL_REDUCE_INPUT_SNAPSHOT;
  req.base.input.memory_space = WOPT_DSL_REDUCE_BUFFER_DEVICE_GLOBAL;
  req.base.output.allocation_id = 2; req.base.output.allocation_bytes = 4 * rows + 256;
  req.base.output.offset_bytes = 32; req.base.output.base_alignment = 16;
  req.base.output.lease = WOPT_DSL_REDUCE_OUTPUT_EXCLUSIVE;
  req.base.output.memory_space = WOPT_DSL_REDUCE_BUFFER_DEVICE_GLOBAL;
  req.base.row_mapping = WOPT_DSL_REDUCE_ROW_LINEAR_THREAD;
  req.base.launched_threads = 32 * (1 + (rows - 1) / 32);
  req.base.before[0] = 2; req.base.before[1] = 4; req.base.before[2] = 8 | 16; req.base.before[3] = 32;
  WOPT_DSL_REDUCTION_STAGING &s = req.staging;
  UINT64 effective = chunk < width ? chunk : width;
  s.chunk_elements = chunk; s.scratch_object = 401; s.scratch_offset = 16;
  s.row_stride = 4 * effective + 4; s.allocation_bytes = 16 + 32 * s.row_stride;
  s.per_row_budget = 4 * effective; s.cta_budget = 65536; s.other_shared_bytes = 128;
  s.rows_per_cta = 32; s.cta_count = 1 + (rows - 1) / 32;
  s.slots = 1; s.memory_space = WOPT_DSL_REDUCE_STAGED_SHARED;
  s.copy_mode = WOPT_DSL_REDUCE_COPY_SYNC_BITS32;
  s.visibility = WOPT_DSL_REDUCE_VISIBLE_SAME_THREAD;
  s.tail_policy = WOPT_DSL_REDUCE_TAIL_EXACT_COPY_READ;
  for (UINT32 i = 0; i < 4; ++i) s.before[i] = 1U << (i + 1);
  s.next[4] = 1;
  WOPT_DSL_REDUCTION_STAGED_BINDING b, again, sentinel, marker;
  memset(&sentinel, 0x5a, sizeof(sentinel)); marker = sentinel;
  DSL_IR_IMAGE_HEADER before, after;
  DSL_IR_Image_Get_Header(&before);
  UINT32 symbols = ST_Table_Size(CURRENT_SYMTAB), types = TY_Table_Size();
  if (!staged_test_check(!WOPT_DSL_bind_reduction_staged(scope, sum, NULL, &sentinel, NULL, NULL) &&
      memcmp(&sentinel, &marker, sizeof(marker)) == 0, "null_request_untouched") ||
      !staged_test_check(!WOPT_DSL_bind_reduction_staged(scope, sum, &req, NULL, NULL, NULL), "null_output") ||
      !staged_test_check(!WOPT_DSL_bind_reduction_staged(WN_func_body(scope), sum, &req, &sentinel, NULL, NULL) &&
      memcmp(&sentinel, &marker, sizeof(marker)) == 0, "invalid_scope_untouched") ||
      !staged_test_check(!WOPT_DSL_bind_reduction_staged(scope, WN_COPY_Tree(sum), &req, &sentinel, NULL, NULL) &&
      memcmp(&sentinel, &marker, sizeof(marker)) == 0, "nonmember_untouched")) return 1;
  FILE *trace = fopen(argv[2], "w");
  if (!trace) return 1;
  WOPT_DSL_REDUCTION_DIRECT_BINDING direct;
  if (!WOPT_DSL_bind_reduction_direct(scope, sum, &req.base, &direct, stderr, trace)) return 1;
  BOOL supported = version == 2 && strcmp(argv[8], "row_major") == 0;
  if (!staged_test_check(WOPT_DSL_bind_reduction_staged(scope, sum, &req, &b, stderr, trace) &&
      b.status == (supported ? WOPT_DSL_REDUCE_STAGED_BOUND : WOPT_DSL_REDUCE_STAGED_BASE_REFUSED),
      "baseline_binding") || !staged_test_check(WOPT_DSL_bind_reduction_staged(scope, sum, &req, &again,
      stderr, NULL) && memcmp(&b, &again, sizeof(b)) == 0, "trace_off_same_contract")) return 1;
  if (supported) {
    if (!staged_test_check(b.runtime_obligations == 255 && b.chunk == effective &&
        b.chunks == 1 + (width - 1) / effective && b.last_count == 1 + (width - 1) % effective,
        "ordered_tail_and_obligations")) return 1;
    for (UINT32 test = 0; test < 40; ++test) {
      WOPT_DSL_REDUCTION_STAGED_REQUEST bad = req;
      UINT32 expected = WOPT_DSL_REDUCE_STAGED_CAPACITY;
      switch (test) {
      case 0: ++bad.base.expected_source.node_id; expected = 1; break;
      case 1: bad.base.architecture = "sm_999"; expected = 1; break;
      case 2: bad.base.fp_options = 1; expected = 1; break;
      case 3: bad.base.input.lease = 0; expected = 1; break;
      case 4: bad.staging.chunk_elements = 0; expected = 2; break;
      case 5: bad.staging.copy_mode = 2; expected = 3; break;
      case 6: bad.staging.visibility = 2; expected = 3; break;
      case 7: bad.staging.slots = 2; expected = 4; break;
      case 8: bad.staging.memory_space = 1; expected = 4; break;
      case 9: bad.staging.scratch_object = 0; expected = 4; break;
      case 10: bad.staging.scratch_offset = 2; expected = 4; break;
      case 11: bad.staging.row_stride = 2; expected = 4; break;
      case 12: bad.staging.row_stride = 4 * (effective - 1); expected = 4; break;
      case 13: bad.staging.rows_per_cta = 0; expected = 5; break;
      case 14: bad.staging.rows_per_cta = 1025; expected = 5; break;
      case 15: ++bad.staging.cta_count; expected = 5; break;
      case 16: ++bad.base.launched_threads; expected = 5; break;
      case 17: --bad.staging.per_row_budget; break;
      case 18: bad.staging.allocation_bytes = b.scratch_end - 1; break;
      case 19: bad.staging.cta_budget = b.hardware_budget + 1; break;
      case 20: bad.staging.cta_budget = b.reserved_bytes - 1; break;
      case 21: bad.staging.allocation_bytes = ~(UINT64)0; break;
      case 22: bad.staging.scratch_offset = (~(UINT64)0) - 3; break;
      case 23: bad.staging.row_stride = (~(UINT64)0) - 3; break;
      case 24: bad.staging.other_shared_bytes = ~(UINT64)0; break;
      case 25: case 26: case 27: case 28: bad.staging.before[test - 25] = 0; expected = 7; break;
      case 29: bad.staging.before[2] |= 4; expected = 7; break;
      case 30: bad.staging.before[4] = 1U << 31; expected = 7; break;
      case 31: bad.staging.before[4] = 1; expected = 7; break;
      case 32: bad.staging.next[4] = 0; expected = 8; break;
      case 33: bad.staging.next[3] = 1; bad.staging.next[4] = 0; expected = 8; break;
      case 34: bad.staging.next[0] = 1; expected = 8; break;
      case 35: bad.staging.tail_policy = 0; expected = 9; break;
      case 36: bad.staging.tail_policy = 2; expected = 9; break;
      case 37: bad.staging.slots = 0; expected = 4; break;
      case 38: bad.base.context_id = 0; expected = 1; break;
      case 39: bad.base.before[2] = 0; expected = 1; break;
      }
      if (!staged_test_check(WOPT_DSL_bind_reduction_staged(scope, sum, &bad, &again, stderr, trace) &&
          again.status == expected && again.runtime_obligations == 0, "refused_staging_contract")) return 1;
    }
    WOPT_DSL_REDUCTION_STAGED_REQUEST shortcut = req;
    shortcut.staging.before[0] |= 16;
    if (!staged_test_check(WOPT_DSL_bind_reduction_staged(scope, sum, &shortcut, &again, stderr, trace) &&
        again.status == 0, "legal_forward_shortcut")) return 1;
    for (UINT64 row = 0; row < rows; ++row)
      for (UINT64 g = 0; g < b.chunks; ++g) {
        WOPT_DSL_REDUCTION_STAGED_SLICE slice, no_trace;
        if (!staged_test_check(WOPT_DSL_reduction_staged_slice(scope, sum, &b, row, g, &slice, stderr, trace) &&
            WOPT_DSL_reduction_staged_slice(scope, sum, &b, row, g, &no_trace, stderr, NULL) &&
            memcmp(&slice, &no_trace, sizeof(slice)) == 0 && slice.base == g * effective &&
            slice.count == (g + 1 == b.chunks ? b.last_count : effective) &&
            slice.scratch_offset == 16 + (row % 32) * s.row_stride, "bounded_slice")) return 1;
      }
    WOPT_DSL_REDUCTION_STAGED_SLICE slice_marker, slice_saved;
    memset(&slice_marker, 0x5a, sizeof(slice_marker)); slice_saved = slice_marker;
    for (UINT32 test = 0; test < 7; ++test) {
      WOPT_DSL_REDUCTION_STAGED_BINDING bad = b;
      UINT64 row = 0, generation = 0;
      if (test == 0) row = rows;
      if (test == 1) generation = b.chunks;
      if (test == 2) ++bad.last_count;
      if (test == 3) --bad.scratch_end;
      if (test == 4) ++bad.base.source.node_id;
      if (test == 5) bad.runtime_obligations = 15;
      if (test == 6) bad.base.stages_bound = 0;
      if (!staged_test_check(!WOPT_DSL_reduction_staged_slice(scope, sum, &bad, row, generation,
          &slice_marker, NULL, NULL) && memcmp(&slice_marker, &slice_saved, sizeof(slice_marker)) == 0,
          "invalid_slice_untouched")) return 1;
    }
  }
  fclose(trace);
  DSL_IR_Image_Get_Header(&after);
  if (!staged_test_check(memcmp(&before, &after, sizeof(before)) == 0 &&
      symbols == ST_Table_Size(CURRENT_SYMTAB) && types == TY_Table_Size(), "read_only_native_tables")) return 1;
  DSL_BUILDER_MAPPED_IMAGE_REQUEST image;
  image.path = argv[1]; image.flags = 0;
  return staged_test_check(DSL_Builder_Finalize_Mapped_Image(&image), "mapped_image") ? 0 : 1;
}
