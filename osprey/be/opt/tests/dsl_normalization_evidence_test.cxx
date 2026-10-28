/* Copyright (C) 2026 Open64 Project */

/* LN1-e native evidence joins reuse explicit source admission and target gates.
 * No kernel, reduction service or source promotion is introduced.
 * Design: doc/AI-COMPILER-LAYERNORM-INTEGRATED-BINDING-v0.1.md. */
#define DSL_NORMALIZATION_TARGET_NO_MAIN
#include "dsl_normalization_target_test.cxx"

/* Publish source, query every native assignment and checkpoint it unchanged.
 * The external review joins these actual requests to real PREOPT facts and
 * exact full-tensor numerical inputs; it does not execute this WHIRL image. */
int main(int argc, char **argv)
{
  if (argc != 5) return 2;
  char *source_args[] = { argv[0], argv[1], argv[4] };
  if (LN1b_Source_Producer(3, source_args)) return 1;
  WN *scope = PU_Info_tree_ptr(Current_PU_Info);
  OPT_Fast_Math = FALSE; Roundoff_Level = ROUNDOFF_NONE; IEEE_Arithmetic = IEEE_ACCURATE;
  FILE *trace = fopen(argv[2], "wx");
  if (!trace) return 1;
  UINT32 contracts = 0;
  for (WN *statement = WN_first(WN_func_body(scope)); statement; statement = WN_next(statement)) {
    if (WN_operator(statement) != OPR_STID || WN_operator(WN_kid0(statement)) != OPR_DSL) continue;
    WOPT_DSL_NORMALIZATION_TARGET_REQUEST request = {};
    if (!WOPT_DSL_characterize_normalization(scope, statement, &request.expected_source, stderr)) return 1;
    request.architecture = argv[3]; request.route = WOPT_DSL_NORM_ROUTE_PTX_POINTER_SCALAR_V1;
    request.toolkit = 12080; request.ptx = 807;
    request.environment = WOPT_DSL_NORM_ENV_RNE_GRADUAL_NO_TRAPS_CLASS;
    BOOL reference = request.expected_source.recipe == WOPT_DSL_NORM_CENTERED_ORDERED_LN_REF_1;
    if (!LN1c_Query(scope, statement, request,
        reference ? WOPT_DSL_NORM_TARGET_BOUND : WOPT_DSL_NORM_TARGET_SOURCE_UNBOUND, trace)) return 1;
    if (reference) {
      WOPT_DSL_NORMALIZATION_TARGET_REQUEST stale = request, unsafe = request;
      stale.expected_source.epsilon_bits ^= 1;
      unsafe.fp_options = WOPT_DSL_NORM_FP_FTZ;
      if (!LN1c_Query(scope, statement, stale, WOPT_DSL_NORM_TARGET_STALE_SOURCE, trace) ||
          !LN1c_Query(scope, statement, unsafe, WOPT_DSL_NORM_TARGET_UNSAFE_ENVIRONMENT, trace)) return 1;
    }
    ++contracts;
  }
  if (fclose(trace) != 0 || !contracts) return 1;
  char preserved[4096];
  if (snprintf(preserved, sizeof(preserved), "%s.preserved.B", argv[1]) >= sizeof(preserved)) return 1;
  DSL_BUILDER_MAPPED_IMAGE_REQUEST checkpoint = {};
  checkpoint.path = preserved;
  return LN1c_Source_Checkpoint(&checkpoint) ? 0 : 1;
}
