/* Copyright (C) 2026 Open64 Project */

/* LN1-c active-PU binding reuses native characterization, exact common target
 * lookup and standard WHIRL assembly. No reduction service, extra IR layout,
 * runtime, plan selection or full LayerNorm lowering is introduced.
 * Design: doc/AI-COMPILER-LAYERNORM-TARGET-ARITHMETIC-v0.1.md. */
#include <stdio.h>
#include <string.h>
#include "opt_dsl_normalization_target.h"
#include "dsl_memory_hierarchy.h"
#include "dsl_shape.h"
#include "config.h"
#include "config_opt.h"
#include "pu_info.h"
#include "wn.h"
#include "stab.h"

/* Compare fields, not padding or diagnostic metadata; stale snapshots never
 * grant a different source node the reference's arithmetic permission. */
static BOOL WOPT_DSL_normalization_source_equal(
    const WOPT_DSL_NORMALIZATION_CHARACTERIZATION &a,
    const WOPT_DSL_NORMALIZATION_CHARACTERIZATION &b)
{
  if (a.recipe != b.recipe || a.flags != b.flags || a.owner_pu != b.owner_pu ||
      a.node_id != b.node_id || a.result_value_id != b.result_value_id ||
      a.result_ty != b.result_ty || a.rows != b.rows || a.width != b.width ||
      a.epsilon_bits != b.epsilon_bits || a.source_line != b.source_line) return FALSE;
  for (UINT32 i = 0; i < 3; ++i)
    if (a.operand_value_ids[i] != b.operand_value_ids[i] ||
        a.operand_types[i] != b.operand_types[i]) return FALSE;
  return TRUE;
}

/* Explicit strict controls supplement instruction rounding. Do not infer
 * strictness from a target name or clear the global options on the caller. */
static BOOL WOPT_DSL_normalization_strict_controls(void)
{
  return !OPT_Fast_Math && Roundoff_Level == ROUNDOFF_NONE &&
      IEEE_Arithmetic >= IEEE_STRICT && IEEE_Arithmetic <= IEEE_ACCURATE;
}

/* Evaluate only arithmetic/carrier eligibility. Toolkit/PTX floors cover the
 * architecture and reviewed route, not a hardware measurement/certification. */
static UINT32 WOPT_DSL_normalization_target_status(
    const WOPT_DSL_NORMALIZATION_TARGET_REQUEST &request,
    WOPT_DSL_NORMALIZATION_TARGET_BINDING *binding)
{
  if (!WOPT_DSL_normalization_source_equal(request.expected_source, binding->source))
    return WOPT_DSL_NORM_TARGET_STALE_SOURCE;
  if (binding->source.recipe != WOPT_DSL_NORM_CENTERED_ORDERED_LN_REF_1)
    return WOPT_DSL_NORM_TARGET_SOURCE_UNBOUND;
  binding->stages_bound = 1;
  DSL_HARDWARE_DESCRIPTION_RECORD hardware;
  if (!DSL_hardware_description_find_arch(request.architecture, &hardware))
    return WOPT_DSL_NORM_TARGET_UNKNOWN;
  binding->architecture_id = hardware.arch_id;
  if (request.route != WOPT_DSL_NORM_ROUTE_PTX_POINTER_SCALAR_V1)
    return WOPT_DSL_NORM_TARGET_ROUTE_UNSUPPORTED;
  UINT32 toolkit_floor = hardware.target_profile_id == DSL_TARGET_PROFILE_NVIDIA_BLACKWELL ? 12080 : 12000;
  UINT32 ptx_floor = hardware.target_profile_id == DSL_TARGET_PROFILE_NVIDIA_BLACKWELL ? 807 : 800;
  if (request.toolkit < toolkit_floor || request.ptx < ptx_floor)
    return WOPT_DSL_NORM_TARGET_TOOLCHAIN_UNSUPPORTED;
  if (request.environment != WOPT_DSL_NORM_ENV_RNE_GRADUAL_NO_TRAPS_CLASS ||
      request.fp_options != 0 || !WOPT_DSL_normalization_strict_controls())
    return WOPT_DSL_NORM_TARGET_UNSAFE_ENVIRONMENT;
  if (!DSL_mean_variance_norm_ref_count_bits(binding->source.width, &binding->count_bits))
    return WOPT_DSL_NORM_TARGET_SOURCE_UNBOUND;
  binding->stages_bound |= 2;
  return WOPT_DSL_NORM_TARGET_BOUND;
}

/* Each positional row exposes the actual controls and status inputs; source
 * line stays last. No producer, measured timing or rewrite claim is emitted. */
static void WOPT_DSL_normalization_target_trace(
    const WOPT_DSL_NORMALIZATION_TARGET_REQUEST &request,
    const WOPT_DSL_NORMALIZATION_TARGET_BINDING &binding, FILE *trace)
{
  const char *reasons[] = { "conditional_isa_carrier_binding", "source_unbound",
      "stale_source", "unknown_architecture", "unsupported_route",
      "unsupported_toolchain", "unsafe_environment" };
  fprintf(trace, "AI_NORM_TARGET 1 HEADER pu node result recipe arch arch_id route toolkit ptx environment fp_options fast_math roundoff ieee source_match status stages count_bits reason source_line\n");
  fprintf(trace, "AI_NORM_TARGET 1 BIND %u %u %u %u %s %u %u %u %u %u %u %u %u %u %u %u %u %08x %s %u\n",
      (UINT32)binding.source.owner_pu, binding.source.node_id, binding.source.result_value_id,
      binding.source.recipe, request.architecture ? request.architecture : "none",
      binding.architecture_id, request.route, request.toolkit, request.ptx,
      request.environment, request.fp_options, (UINT32)OPT_Fast_Math,
      (UINT32)Roundoff_Level, (UINT32)IEEE_Arithmetic,
      (UINT32)WOPT_DSL_normalization_source_equal(request.expected_source, binding.source),
      binding.status, binding.stages_bound, binding.count_bits,
      reasons[binding.status], binding.source.source_line);
}

/* Native characterization enforces membership, active PU, exact schema and
 * read-only validation before any output or optional trace is published. */
BOOL WOPT_DSL_bind_normalization_target(WN *scope, WN *assignment,
    const WOPT_DSL_NORMALIZATION_TARGET_REQUEST *request,
    WOPT_DSL_NORMALIZATION_TARGET_BINDING *output, FILE *diagnostic, FILE *trace)
{
  WOPT_DSL_NORMALIZATION_TARGET_BINDING binding;
  memset(&binding, 0, sizeof(binding));
  if (!request || !output || !WOPT_DSL_characterize_normalization
      (scope, assignment, &binding.source, diagnostic)) return FALSE;
  binding.status = WOPT_DSL_normalization_target_status(*request, &binding);
  *output = binding;
  if (trace) WOPT_DSL_normalization_target_trace(*request, binding, trace);
  return TRUE;
}

/* Closed spellings make rounding/precision visible in the actual mapped WN
 * carrier, rather than a source template or inferred emitter instruction. */
const char *WOPT_DSL_normalization_scalar_instruction(UINT32 operation)
{
  const char *instructions[] = { NULL, "add.rn.f32", "sub.rn.f32", "mul.rn.f32",
      "div.rn.f32", "sqrt.rn.f32" };
  return operation < sizeof(instructions)/sizeof(instructions[0]) ? instructions[operation] : NULL;
}

/* Require the driver's active PU and local symbol table to describe the same
 * scope; this service never selects another PU or restores its tables. */
static BOOL WOPT_DSL_normalization_active_scope(void)
{
  return Current_PU_Info && CURRENT_SYMTAB > GLOBAL_SYMTAB &&
      Scope_tab[CURRENT_SYMTAB].st &&
      ST_st_idx(Scope_tab[CURRENT_SYMTAB].st) == PU_Info_proc_sym(Current_PU_Info) &&
      PU_Info_tree_ptr(Current_PU_Info) && WN_operator(PU_Info_tree_ptr(Current_PU_Info)) == OPR_FUNC_ENTRY;
}

/* Validate a detached address against the current PU's real symbol/type join.
 * No integer-to-pointer guess or foreign/local-table alias is admitted. */
static BOOL WOPT_DSL_normalization_address_valid(const WN *address)
{
  return address && WN_operator(address) == OPR_LDID &&
      WN_rtype(address) == Pointer_Mtype && WN_st_idx(address) &&
      ST_IDX_level(WN_st_idx(address)) == CURRENT_SYMTAB &&
      ST_IDX_index(WN_st_idx(address)) < ST_Table_Size(CURRENT_SYMTAB) &&
      WN_ty(address) && TY_IDX_index(WN_ty(address)) < TY_Table_Size() &&
      TY_kind(WN_ty(address)) == KIND_POINTER &&
      TY_pointed(WN_ty(address)) && TY_IDX_index(TY_pointed(WN_ty(address))) < TY_Table_Size() &&
      TY_mtype(TY_pointed(WN_ty(address))) == MTYPE_F4 &&
      ST_class(WN_st(address)) == CLASS_VAR &&
      WN_ty(address) == ST_type(WN_st(address)) && WN_offset(address) == 0;
}

/* Produce one exact body spelling for construction and postchecking. No target
 * choice, assembler macro or source-emitter repair is hidden in this helper. */
static BOOL WOPT_DSL_normalization_scalar_text(UINT32 operation, char *text, UINT32 size)
{
  const char *instruction = WOPT_DSL_normalization_scalar_instruction(operation);
  if (!instruction) return FALSE;
  snprintf(text, size, operation == WOPT_DSL_NORM_SCALAR_SQRT ?
      "{ .reg .f32 a, c; ld.f32 a, [%%1]; %s c, a; st.f32 [%%0], c; }" :
      "{ .reg .f32 a, b, c; ld.f32 a, [%%1]; ld.f32 b, [%%2]; %s c, a, b; st.f32 [%%0], c; }", instruction);
  return TRUE;
}

/* Reuse the already supported pointer-input ASM protocol. The deliberately
 * conservative memory carrier exposes load-before-store and rounding, while
 * register-output/performance work stays a separate reviewed optimization. */
WN *WOPT_DSL_create_normalization_scalar(UINT32 architecture_id,
    UINT32 operation, WN **addresses, UINT32 count, SRCPOS source)
{
  DSL_HARDWARE_DESCRIPTION_RECORD hardware;
  const char *instruction = WOPT_DSL_normalization_scalar_instruction(operation);
  if (!WOPT_DSL_normalization_active_scope() || !source || !SRCPOS_filenum(source) ||
      SRCPOS_linenum(source) <= 0 || !instruction || !addresses ||
      !WOPT_DSL_normalization_strict_controls() ||
      !DSL_hardware_description_get_arch(architecture_id, &hardware) ||
      count != (operation == WOPT_DSL_NORM_SCALAR_SQRT ? 2U : 3U)) return NULL;
  for (UINT32 i = 0; i < count; ++i) {
    WN *address = addresses[i];
    if (!WOPT_DSL_normalization_address_valid(address)) return NULL;
    for (UINT32 j = 0; j < i; ++j) if (address == addresses[j]) return NULL;
  }
  char text[256];
  WOPT_DSL_normalization_scalar_text(operation, text, sizeof(text));
  WN *statement = WN_CreateAsm_Stmt(count + 2, text);
  Set_ST_Srcpos(*WN_st(statement), source);
  WN_Set_Linenum(statement, source);
  WN_asm_flag(statement) = WN_ASM_VOLATILE | WN_ASM_CLOBBERS_MEM;
  WN_kid0(statement) = WN_CreateBlock();
  WN_kid1(statement) = WN_CreateBlock();
  for (UINT32 i = 0; i < count; ++i) {
    WN_kid(statement, i + 2) = WN_CreateAsm_Input((char *)"l", i, addresses[i]);
    Set_ST_Srcpos(*WN_st(WN_kid(statement, i + 2)), source);
  }
  return statement;
}

/* Verify the real carrier and symbol joins, including effects and empty
 * copyout/clobber blocks required by the supported CUDA emitter protocol. */
BOOL WOPT_DSL_verify_normalization_scalar(const WN *statement, UINT32 operation)
{
  char text[256];
  UINT32 count = operation == WOPT_DSL_NORM_SCALAR_SQRT ? 2 : 3;
  if (!WOPT_DSL_normalization_active_scope() || !statement || !WOPT_DSL_normalization_scalar_text(operation, text, sizeof(text)) ||
      WN_operator(statement) != OPR_ASM_STMT || WN_kid_count(statement) != count + 2 ||
      WN_asm_flag(statement) != (WN_ASM_VOLATILE | WN_ASM_CLOBBERS_MEM) ||
      strcmp(WN_asm_string(statement), text)) return FALSE;
  for (UINT32 i = 0; i < 2; ++i)
    if (!WN_kid(statement, i) || WN_operator(WN_kid(statement, i)) != OPR_BLOCK || WN_first(WN_kid(statement, i))) return FALSE;
  for (UINT32 i = 0; i < count; ++i) {
    const WN *input = WN_kid(statement, i + 2);
    if (!input || WN_operator(input) != OPR_ASM_INPUT || WN_kid_count(input) != 1 ||
        WN_asm_opnd_num(input) != i || strcmp(WN_asm_input_constraint(input), "l") ||
        !WOPT_DSL_normalization_address_valid(WN_kid0(input))) return FALSE;
    for (UINT32 j = 0; j < i; ++j)
      if (input == WN_kid(statement, j + 2) || WN_kid0(input) == WN_kid0(WN_kid(statement, j + 2))) return FALSE;
  }
  return TRUE;
}
