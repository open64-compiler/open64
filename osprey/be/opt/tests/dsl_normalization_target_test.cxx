/* Copyright (C) 2026 Open64 Project */

/* LN1-c native binding/negative tests and separate scalar carrier producer.
 * Reuse LN1-b source construction, not reduction fixtures or a second matcher.
 * Design: doc/AI-COMPILER-LAYERNORM-TARGET-ARITHMETIC-v0.1.md. */
#include "defs.h"
#include "dsl_builder.h"
#include <unistd.h>
#include <sys/wait.h>
#include <stdlib.h>

/* The ordinary writer finalizes in place; fork so later queries see live IR. */
static BOOL LN1c_Source_Checkpoint(const DSL_BUILDER_MAPPED_IMAGE_REQUEST *request);
#define DSL_Builder_Finalize_Mapped_Image LN1c_Source_Checkpoint
#define main LN1b_Source_Producer
#include "dsl_normalization_ref_test.cxx"
#undef main
#undef DSL_Builder_Finalize_Mapped_Image
#include "opt_dsl_normalization_target.h"
#include "dsl_memory_hierarchy.h"
#include "ir_bwrite.h"

/* Publish a fully finalized child image without replacing a prior receipt. */
static BOOL LN1c_Source_Checkpoint(const DSL_BUILDER_MAPPED_IMAGE_REQUEST *request)
{
  if (access(request->path, F_OK) == 0) return FALSE;
  fflush(NULL);
  pid_t child = fork();
  if (child < 0) return FALSE;
  if (!child) _exit(DSL_Builder_Finalize_Mapped_Image(request) ? 0 : 1);
  int status = 0;
  return waitpid(child, &status, 0) == child && WIFEXITED(status) && WEXITSTATUS(status) == 0;
}

/* Snapshot all relevant table counters around each read-only target query. */
static BOOL LN1c_Query(WN *scope, WN *value,
    const WOPT_DSL_NORMALIZATION_TARGET_REQUEST &request, UINT32 status, FILE *trace)
{
  DSL_IR_IMAGE_HEADER before, after;
  DSL_IR_Image_Get_Header(&before);
  UINT32 symbols = ST_Table_Size(CURRENT_SYMTAB), types = TY_Table_Size();
  WOPT_DSL_NORMALIZATION_TARGET_BINDING binding, quiet;
  if (!WOPT_DSL_bind_normalization_target(scope, value, &request, &binding, stderr, trace) ||
      !WOPT_DSL_bind_normalization_target(scope, value, &request, &quiet, NULL, NULL) ||
      memcmp(&binding, &quiet, sizeof(binding)) || binding.status != status) return FALSE;
  DSL_IR_Image_Get_Header(&after);
  return Check(!memcmp(&before, &after, sizeof(before)) &&
      symbols == ST_Table_Size(CURRENT_SYMTAB) && types == TY_Table_Size() &&
      (binding.source.flags & WOPT_DSL_NORM_TARGET_UNBOUND ||
       binding.source.recipe == WOPT_DSL_NORM_CENTERED_POPULATION_F32_ROWS_V1),
      "target_query_read_only_trace_parity_and_expected_status");
}

/* Bind actual mixed v1/v2 source and mutate every request boundary. */
static int LN1c_Binding(int argc, char **argv)
{
  char mixed[] = "mixed";
  char *source_args[] = { argv[0], argv[1], mixed };
  if (LN1b_Source_Producer(3, source_args)) return 1;
  WN *scope = PU_Info_tree_ptr(Current_PU_Info);
  WN *value = WN_first(WN_func_body(scope)), *legacy = NULL;
  while (value && (WN_operator(value) != OPR_STID || WN_operator(WN_kid0(value)) != OPR_DSL))
    value = WN_next(value);
  for (WN *s = value; s; s = WN_next(s)) {
    WOPT_DSL_NORMALIZATION_CHARACTERIZATION fact;
    if (WN_operator(s) == OPR_STID && WOPT_DSL_characterize_normalization(scope, s, &fact, NULL) && fact.recipe == 1)
      legacy = s;
  }
  if (!value || !legacy) return 1;
  OPT_Fast_Math = FALSE; Roundoff_Level = ROUNDOFF_NONE; IEEE_Arithmetic = IEEE_ACCURATE;
  WOPT_DSL_NORMALIZATION_TARGET_REQUEST request = {};
  if (!WOPT_DSL_characterize_normalization(scope, value, &request.expected_source, stderr)) return 1;
  request.architecture = argv[3]; request.route = WOPT_DSL_NORM_ROUTE_PTX_POINTER_SCALAR_V1;
  request.toolkit = 12080; request.ptx = 807; request.environment = WOPT_DSL_NORM_ENV_RNE_GRADUAL_NO_TRAPS_CLASS;
  FILE *trace = fopen(argv[2], "wx");
  if (!trace) return 1;
  if (!LN1c_Query(scope, value, request, WOPT_DSL_NORM_TARGET_BOUND, trace)) return 1;
  for (UINT32 fault = 0; fault < 28; ++fault) {
    WOPT_DSL_NORMALIZATION_TARGET_REQUEST bad = request;
    UINT32 expected = WOPT_DSL_NORM_TARGET_STALE_SOURCE;
    switch (fault) {
    case 0: bad.expected_source.recipe++; break;
    case 1: bad.expected_source.flags ^= WOPT_DSL_NORM_TARGET_UNBOUND; break;
    case 2: bad.expected_source.owner_pu++; break;
    case 3: bad.expected_source.node_id++; break;
    case 4: bad.expected_source.result_value_id++; break;
    case 5: bad.expected_source.result_ty++; break;
    case 6: bad.expected_source.rows++; break;
    case 7: bad.expected_source.width++; break;
    case 8: bad.expected_source.epsilon_bits++; break;
    case 9: bad.expected_source.source_line++; break;
    case 10: case 11: case 12: bad.expected_source.operand_types[fault - 10]++; break;
    case 13: case 14: case 15: bad.expected_source.operand_value_ids[fault - 13]++; break;
    case 16: bad.architecture = "sm_100f"; expected = WOPT_DSL_NORM_TARGET_UNKNOWN; break;
    case 17: bad.architecture = NULL; expected = WOPT_DSL_NORM_TARGET_UNKNOWN; break;
    case 18: bad.route = 2; expected = WOPT_DSL_NORM_TARGET_ROUTE_UNSUPPORTED; break;
    case 19: bad.toolkit = 11999; expected = WOPT_DSL_NORM_TARGET_TOOLCHAIN_UNSUPPORTED; break;
    case 20: bad.ptx = 799; expected = WOPT_DSL_NORM_TARGET_TOOLCHAIN_UNSUPPORTED; break;
    case 21: bad.environment = 0; expected = WOPT_DSL_NORM_TARGET_UNSAFE_ENVIRONMENT; break;
    case 22: bad.fp_options = 0x80000000; expected = WOPT_DSL_NORM_TARGET_UNSAFE_ENVIRONMENT; break;
    case 23: OPT_Fast_Math = TRUE; expected = WOPT_DSL_NORM_TARGET_UNSAFE_ENVIRONMENT; break;
    case 24: Roundoff_Level = ROUNDOFF_ASSOC; expected = WOPT_DSL_NORM_TARGET_UNSAFE_ENVIRONMENT; break;
    case 25: IEEE_Arithmetic = IEEE_INEXACT; expected = WOPT_DSL_NORM_TARGET_UNSAFE_ENVIRONMENT; break;
    case 26: bad.toolkit = 12079; expected = strstr(argv[3], "100") ? WOPT_DSL_NORM_TARGET_TOOLCHAIN_UNSUPPORTED : WOPT_DSL_NORM_TARGET_BOUND; break;
    case 27: bad.ptx = 806; expected = strstr(argv[3], "100") ? WOPT_DSL_NORM_TARGET_TOOLCHAIN_UNSUPPORTED : WOPT_DSL_NORM_TARGET_BOUND; break;
    }
    if (!LN1c_Query(scope, value, bad, expected, trace)) return 1;
    OPT_Fast_Math = FALSE; Roundoff_Level = ROUNDOFF_NONE; IEEE_Arithmetic = IEEE_ACCURATE;
  }
  for (UINT32 bit = 0; bit < 9; ++bit) {
    WOPT_DSL_NORMALIZATION_TARGET_REQUEST bad = request;
    bad.fp_options = 1U << bit;
    if (!LN1c_Query(scope, value, bad, WOPT_DSL_NORM_TARGET_UNSAFE_ENVIRONMENT, trace)) return 1;
  }
  WOPT_DSL_NORMALIZATION_TARGET_REQUEST v1 = request;
  if (!WOPT_DSL_characterize_normalization(scope, legacy, &v1.expected_source, NULL) ||
      !LN1c_Query(scope, legacy, v1, WOPT_DSL_NORM_TARGET_SOURCE_UNBOUND, trace)) return 1;
  WOPT_DSL_NORMALIZATION_TARGET_BINDING untouched, saved;
  memset(&untouched, 0x5a, sizeof(untouched)); saved = untouched;
  long length = ftell(trace);
  if (!Check(!WOPT_DSL_bind_normalization_target(WN_func_body(scope), value, &request, &untouched, NULL, trace) &&
      !WOPT_DSL_bind_normalization_target(scope, WN_COPY_Tree(value), &request, &untouched, NULL, trace) &&
      !WOPT_DSL_bind_normalization_target(scope, value, NULL, &untouched, NULL, trace) &&
      !WOPT_DSL_bind_normalization_target(scope, value, &request, NULL, NULL, trace) &&
      !memcmp(&untouched, &saved, sizeof(saved)) && ftell(trace) == length,
      "target_invalid_queries_preserve_output_and_trace")) return 1;
  fclose(trace);
  return 0;
}

/* Write a standard scalar-only device image, preserving parent tables. */
static BOOL LN1c_Scalar_Image(PU_Info *pu, const char *path)
{
  char temporary[4096];
  if (snprintf(temporary, sizeof(temporary), "%s.tmp", path) >= sizeof(temporary) ||
      access(path, F_OK) == 0 || access(temporary, F_OK) == 0) return FALSE;
  fflush(NULL);
  pid_t child = fork();
  if (child < 0) return FALSE;
  if (!child) {
    Irb_File_Name = temporary;
    if (!Open_Output_Info(Irb_File_Name)) _exit(1);
    Write_PU_Info(pu); Write_Global_Info(pu); Close_Output_Info();
    if (link(temporary, path) != 0) _exit(1);
    _exit(unlink(temporary) == 0 ? 0 : 1);
  }
  int status = 0;
  return waitpid(child, &status, 0) == child && WIFEXITED(status) && WEXITSTATUS(status) == 0;
}

/* Build five standalone primitive operations, not a normalized row kernel.
 * Formal arrays are one F4 each; each result has its own pointer argument. */
static int LN1c_Scalar(int argc, char **argv)
{
  Initialize_Context();
  if (!DSL_Builder_Begin_Program()) return 1;
  PU_Info *pu = DSL_Builder_Create_Minimal_PU("ln_ref_scalar_carriers");
  if (!pu || !DSL_Builder_Select_PU(pu)) return 1;
  Current_PU_Info = pu;
  OPT_Fast_Math = FALSE; Roundoff_Level = ROUNDOFF_NONE; IEEE_Arithmetic = IEEE_ACCURATE;
  DSL_HARDWARE_DESCRIPTION_RECORD hardware;
  if (!DSL_hardware_description_find_arch(argv[3], &hardware)) return 1;
  DSL_BUILDER_SOURCE_POSITION position = {};
  position.file_id = DSL_Builder_Register_Source_File(pu, __FILE__); position.line = __LINE__;
  USRCPOS source; USRCPOS_clear(source); USRCPOS_filenum(source) = position.file_id;
  USRCPOS_linenum(source) = position.line;
  TY_IDX pointer = Make_Pointer_Type(MTYPE_To_TY(MTYPE_F4));
  ST *formals[7];
  const char *names[] = { "left", "right", "sum", "difference", "product", "quotient", "root" };
  for (UINT32 i = 0; i < 7; ++i) {
    formals[i] = New_ST(CURRENT_SYMTAB);
    ST_Init(formals[i], Save_Str(names[i]), CLASS_VAR, SCLASS_FORMAL, EXPORT_LOCAL, pointer);
    Set_ST_Srcpos(*formals[i], USRCPOS_srcpos(source));
  }
  ST *function = &St_Table[PU_Info_proc_sym(pu)];
  Set_ST_device_symbol_kind(*function, DEVICE_SYMBOL_KERNEL); Set_ST_export(function, EXPORT_PREEMPTIBLE);
  TY_IDX type = ST_type(function); Set_TY_has_prototype(type); TYLIST_IDX list;
  Set_TYLIST_type(New_TYLIST(list), MTYPE_To_TY(MTYPE_V)); Set_TY_tylist(Ty_Table[type], list);
  for (UINT32 i = 0; i < 7; ++i) Set_TYLIST_type(New_TYLIST(list), pointer);
  Set_TYLIST_type(New_TYLIST(list), TY_IDX_ZERO);
  WN *body = WN_CreateBlock(), *entry = WN_CreateEntry(7, function, body, NULL, NULL);
  for (UINT32 i = 0; i < 7; ++i) WN_kid(entry, i) = WN_CreateIdname(0, formals[i]);
  Set_PU_Info_tree_ptr(pu, entry);
  WN *ret = WN_CreateReturn(); WN_INSERT_BlockLast(body, ret);
  if (!LN1c_Scalar_Image(pu, argv[1])) return 1;
  WN_EXTRACT_FromBlock(body, ret);
  for (UINT32 operation = 1; operation <= 5; ++operation) {
    WN *addresses[3] = { WN_Ldid(Pointer_Mtype, 0, formals[operation + 1], pointer),
      WN_Ldid(Pointer_Mtype, 0, formals[0], pointer), WN_Ldid(Pointer_Mtype, 0, formals[1], pointer) };
    UINT32 count = operation == 5 ? 2 : 3;
    UINT32 symbols = ST_Table_Size(CURRENT_SYMTAB), types = TY_Table_Size();
    if (!Check(!WOPT_DSL_create_normalization_scalar(0, operation, addresses, count, USRCPOS_srcpos(source)) &&
        !WOPT_DSL_create_normalization_scalar(hardware.arch_id, 99, addresses, count, USRCPOS_srcpos(source)) &&
        !WOPT_DSL_create_normalization_scalar(hardware.arch_id, operation, addresses, count - 1, USRCPOS_srcpos(source)) &&
        !WOPT_DSL_create_normalization_scalar(hardware.arch_id, operation, NULL, count, USRCPOS_srcpos(source)) &&
        !WOPT_DSL_create_normalization_scalar(hardware.arch_id, operation, addresses, count, 0) &&
        symbols == ST_Table_Size(CURRENT_SYMTAB) && types == TY_Table_Size(),
        "scalar_preflight_no_product_allocation")) return 1;
    for (UINT32 i = 0; i < count; ++i) {
      WN *saved = addresses[i]; addresses[i] = NULL;
      BOOL rejected = !WOPT_DSL_create_normalization_scalar(hardware.arch_id, operation, addresses, count, USRCPOS_srcpos(source));
      addresses[i] = saved;
      TY_IDX saved_type = WN_ty(saved); WN_set_ty(saved, MTYPE_To_TY(MTYPE_F4));
      rejected = rejected && !WOPT_DSL_create_normalization_scalar(hardware.arch_id, operation, addresses, count, USRCPOS_srcpos(source));
      WN_set_ty(saved, saved_type);
      if (!Check(rejected && symbols == ST_Table_Size(CURRENT_SYMTAB) && types == TY_Table_Size(),
          "scalar_null_and_untyped_pointer_refused")) return 1;
    }
    OPT_Fast_Math = TRUE;
    BOOL unsafe = !WOPT_DSL_create_normalization_scalar(hardware.arch_id, operation, addresses, count, USRCPOS_srcpos(source));
    OPT_Fast_Math = FALSE;
    if (!Check(unsafe && symbols == ST_Table_Size(CURRENT_SYMTAB), "scalar_unsafe_global_controls_refused")) return 1;
    WN *statement = WOPT_DSL_create_normalization_scalar(hardware.arch_id, operation, addresses, count, USRCPOS_srcpos(source));
    if (!Check(statement && WOPT_DSL_verify_normalization_scalar(statement, operation),
        "scalar_explicit_instruction_effects")) return 1;
    WN_asm_flag(statement) = 0;
    if (!Check(!WOPT_DSL_verify_normalization_scalar(statement, operation), "scalar_corrupt_effects_refused")) return 1;
    WN_asm_flag(statement) = WN_ASM_VOLATILE | WN_ASM_CLOBBERS_MEM;
    WN_asm_opnd_num(WN_kid(statement, 2)) = 99;
    if (!Check(!WOPT_DSL_verify_normalization_scalar(statement, operation), "scalar_corrupt_operand_order_refused")) return 1;
    WN_asm_opnd_num(WN_kid(statement, 2)) = 0;
    WN *load = WN_kid0(WN_kid(statement, 2));
    ST_IDX saved_symbol = WN_st_idx(load); WN_st_idx(load) = WN_st_idx(statement);
    if (!Check(!WOPT_DSL_verify_normalization_scalar(statement, operation), "scalar_class_name_operand_refused")) return 1;
    WN_st_idx(load) = saved_symbol;
    if (!Check(!WOPT_DSL_verify_normalization_scalar(statement, operation == 5 ? 1 : 5),
        "scalar_wrong_instruction_refused")) return 1;
    WN_INSERT_BlockLast(body, statement);
  }
  WN_INSERT_BlockLast(body, ret);
  if (!DSL_Gatekeeper_Verify_PU(pu, stderr, NULL) || !LN1c_Scalar_Image(pu, argv[2])) return 1;
  return 0;
}

/* Select separate fresh-process source binding or scalar-carrier evidence. */
#ifndef DSL_NORMALIZATION_TARGET_NO_MAIN
int main(int argc, char **argv)
{
  if (argc != 4 && argc != 5) return 2;
  return argc == 5 && strcmp(argv[4], "scalar") == 0 ? LN1c_Scalar(argc, argv) : LN1c_Binding(argc, argv);
}
#endif
