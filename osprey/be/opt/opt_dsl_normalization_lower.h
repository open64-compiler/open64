/* Copyright (C) 2026 Open64 Project */

/* LN4 source-revalidated handoff and active-device-PU materialization.
 * Design: doc/AI-COMPILER-LAYERNORM-EXECUTABLE-LOWERING-v0.1.md.
 * Transient native contracts; no mapped layout, v1 or reduction dependency. */
#ifndef opt_dsl_normalization_lower_INCLUDED
#define opt_dsl_normalization_lower_INCLUDED
#include "opt_dsl_normalization_cost.h"

/* Driver-owned numeric handoff, not a portable ABI or trusted input format.
 * Seal detects accidental changes, not adversarial authorization. The guard
 * retains all runtime/resource obligations; no measured timing is asserted. */
typedef struct {
  UINT64 version, revision, registers_per_thread, max_jobs, margin;
  DSL_SCALAR_EXECUTION_PROFILE profile;
  OPEN64_NORMALIZATION_GUARD guard;
  WOPT_DSL_NORMALIZATION_COST cost;
  UINT64 seal;
  /* Appended after the legacy seal prefix to preserve no-import fingerprints.
   * Imported selections seal/revalidate both keys and the declared live bytes. */
  DSL_GPU_PLAN_STAMP gpu_stamp;
  UINT64 gpu_live_bytes;
} WOPT_DSL_NORMALIZATION_LOWERING;

/* Identity of the constructed executable tree, not a substitute for semantic
 * verification or runtime admission. Reopen compares the actual mapped tree.
 * No WN/ST pointers or mapped-image extension are needed. */
typedef struct {
  UINT64 plan_seal, tree_seal, nodes, loops, scalar_operations, barriers;
} WOPT_DSL_NORMALIZATION_DEVICE;

/* Driver/JIT must establish n64 and strict compiler/host FP state before
 * preparation or materialization. Read-only: never alter the service's FP state. */
extern BOOL WOPT_DSL_normalization_compiler_environment_ready();

/* Recompute LN3 in the active analysis PU and require exact selected-result
 * agreement, including the current imported device/resource/cost/budget stamp.
 * Refusal leaves output/trace/tables/source unchanged. */
extern BOOL WOPT_DSL_prepare_normalization_lowering(WN *scope, WN *assignment,
    const WOPT_DSL_NORMALIZATION_COST_REQUEST *request,
    const WOPT_DSL_NORMALIZATION_COST_RESULT *expected,
    WOPT_DSL_NORMALIZATION_LOWERING *output, FILE *diagnostic, FILE *trace);

/* Fill only the driver's active empty kernel PU; never create/select a PU.
 * Preflight revalidates imported evidence and declared CTA/storage demands;
 * refusal changes nothing. Success changes the active tree/formals
 * and symbol/type tables. Source position belongs to this device PU's file.
 * Allocation failures are compiler-fatal, not recoverable partial success. */
extern BOOL WOPT_DSL_materialize_normalization_device(
    const WOPT_DSL_NORMALIZATION_LOWERING *plan, SRCPOS source,
    WOPT_DSL_NORMALIZATION_DEVICE *output, FILE *diagnostic, FILE *trace);

/* Read-only strict-tree/ASM/effects/resource-attribute identity verification
 * in the driver's active device scope, including after mapped reopen. A bound
 * compiler handoff also requires its original current imported context. */
extern BOOL WOPT_DSL_verify_normalization_device(
    const WOPT_DSL_NORMALIZATION_LOWERING *plan,
    const WOPT_DSL_NORMALIZATION_DEVICE *expected, FILE *diagnostic, FILE *trace);
#endif
