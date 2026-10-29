/* Copyright (C) 2026 Open64 Project */

/* Active-PU LN1-c arithmetic/environment binding and explicit scalar carriers.
 * Transient contracts, not source promotion, storage planning or selection.
 * Design: doc/AI-COMPILER-LAYERNORM-TARGET-ARITHMETIC-v0.1.md. */
#ifndef opt_dsl_normalization_target_INCLUDED
#define opt_dsl_normalization_target_INCLUDED

#include "opt_dsl.h"
#include "srcpos.h"

enum WOPT_DSL_NORMALIZATION_TARGET_STATUS {
  WOPT_DSL_NORM_TARGET_BOUND,
  WOPT_DSL_NORM_TARGET_SOURCE_UNBOUND,
  WOPT_DSL_NORM_TARGET_STALE_SOURCE,
  WOPT_DSL_NORM_TARGET_UNKNOWN,
  WOPT_DSL_NORM_TARGET_ROUTE_UNSUPPORTED,
  WOPT_DSL_NORM_TARGET_TOOLCHAIN_UNSUPPORTED,
  WOPT_DSL_NORM_TARGET_UNSAFE_ENVIRONMENT
};
enum WOPT_DSL_NORMALIZATION_SCALAR {
  WOPT_DSL_NORM_SCALAR_ADD = 1,
  WOPT_DSL_NORM_SCALAR_SUB,
  WOPT_DSL_NORM_SCALAR_MUL,
  WOPT_DSL_NORM_SCALAR_DIV,
  WOPT_DSL_NORM_SCALAR_SQRT
};
enum {
  WOPT_DSL_NORM_ROUTE_PTX_POINTER_SCALAR_V1 = 1,
  WOPT_DSL_NORM_ENV_RNE_GRADUAL_NO_TRAPS_CLASS = 1,
  WOPT_DSL_NORM_FP_FTZ = 1U << 0,
  WOPT_DSL_NORM_FP_FAST_MATH = 1U << 1,
  WOPT_DSL_NORM_FP_APPROX_DIV = 1U << 2,
  WOPT_DSL_NORM_FP_APPROX_SQRT = 1U << 3,
  WOPT_DSL_NORM_FP_CONTRACT = 1U << 4,
  WOPT_DSL_NORM_FP_REASSOCIATE = 1U << 5,
  WOPT_DSL_NORM_FP_OBSERVABLE_EXCEPTIONS = 1U << 6,
  WOPT_DSL_NORM_FP_DAZ = 1U << 7,
  WOPT_DSL_NORM_FP_NAN_PAYLOAD = 1U << 8
};

/* Driver/lowering contract, not proof of a toolchain invocation. Toolkit uses
 * major*1000+minor*10 (12080), PTX major*100+minor (807). Actual compile flags,
 * versions and generated instructions remain independent review evidence. */
typedef struct {
  WOPT_DSL_NORMALIZATION_CHARACTERIZATION expected_source;
  const char *architecture;
  UINT32 route;
  UINT32 toolkit;
  UINT32 ptx;
  UINT32 environment;
  UINT32 fp_options;
} WOPT_DSL_NORMALIZATION_TARGET_REQUEST;

/* Source flags stay source facts, including TARGET_UNBOUND. This separate
 * result records conditional ISA/carrier eligibility, never GPU certification. */
typedef struct {
  WOPT_DSL_NORMALIZATION_CHARACTERIZATION source;
  UINT32 status;
  UINT32 architecture_id;
  UINT32 stages_bound;
  UINT32 count_bits;
} WOPT_DSL_NORMALIZATION_TARGET_BINDING;

/* Recharacterize the active whole PU and match every snapshot field. Invalid
 * scope/IR/pointers leave output/trace unchanged and return FALSE. A valid query
 * returns TRUE with a bound/refused status; no IR/table mutation or selection.
 * NULL trace is the ordinary trace-off path and performs identical checks. */
extern BOOL WOPT_DSL_bind_normalization_target(WN *scope, WN *assignment,
    const WOPT_DSL_NORMALIZATION_TARGET_REQUEST *request,
    WOPT_DSL_NORMALIZATION_TARGET_BINDING *output, FILE *diagnostic, FILE *trace);

/* Exact instruction for a closed scalar operation; unsupported IDs return
 * NULL. No plain DIV/SQRT, approximate instruction or legacy alias is chosen. */
extern const char *WOPT_DSL_normalization_scalar_instruction(UINT32 operation);

/* Build one detached standard ASM_STMT in the active PU after preflight.
 * Addresses are distinct owned WN trees: result, left, optional right, typed
 * F4 pointers. Loads precede store (alias permitted); caller guarantees live,
 * naturally aligned four-byte locations. No runtime/storage proof is supplied.
 * Uses existing input-only "l" operands, volatile+memory effects and explicit
 * rn/no-ftz PTX. Refusal allocates no WN/ST/TY and never takes kid ownership.
 * Success takes address-tree ownership; caller owns insertion/publication.
 * This primitive service does not lower a v1/v2 LayerNorm node. */
extern WN *WOPT_DSL_create_normalization_scalar(UINT32 architecture_id,
    UINT32 operation, WN **addresses, UINT32 count, SRCPOS source);

/* Recheck actual text, rounding, flags, ordered inputs and typed symbols after
 * edits/mapped reopen. Read-only; no metadata tag substitutes for the body. */
extern BOOL WOPT_DSL_verify_normalization_scalar(const WN *statement, UINT32 operation);

#endif
