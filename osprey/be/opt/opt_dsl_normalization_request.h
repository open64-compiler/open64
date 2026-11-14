/* Copyright (C) 2026 Open64 Project */

/* LN4 driver/JIT request boundary, owned by WOPT in the active whole PU.
 * Design: doc/AI-COMPILER-LAYERNORM-DRIVER-REQUEST-v0.1.md.
 * No mapped-layout change, source-v1 promotion or runtime launch authority. */
#ifndef opt_dsl_normalization_request_INCLUDED
#define opt_dsl_normalization_request_INCLUDED
#include <stddef.h>
#include "opt_dsl_normalization_lower.h"

/* Owned, pointer-free request; source IDs are bound to the supplied WHIRL
 * image, not portable semantic names. Timing provenance is caller input. */
typedef struct WOPT_DSL_NORMALIZATION_REQUEST {
  UINT32 version;
  char architecture[16];
  WOPT_DSL_NORMALIZATION_CHARACTERIZATION source;
  UINT32 route, toolkit, ptx, environment, fp_options;
  UINT64 runtime_contract_id, context_id, launched_ctas;
  UINT32 threads_per_cta;
  WOPT_DSL_NORMALIZATION_RANGE ranges[4];
  DSL_SCALAR_EXECUTION_PROFILE profile;
  UINT32 registers_per_thread[3];
  UINT64 max_jobs, margin;
} WOPT_DSL_NORMALIZATION_REQUEST;

/* Shared-library handoff follows the existing WOPT init/weak pointer protocol.
 * Decode is context-free. Prepare requires a canonical active whole PU, O3,
 * n64 and strict compiler FP controls; outputs stay unchanged on refusal.
 * Materialize/verify require the driver-selected device scope as in LN4. */
typedef struct WOPT_DSL_NORMALIZATION_DRIVER_API {
  BOOL (*decode)(const char *, size_t, WOPT_DSL_NORMALIZATION_REQUEST *, FILE *);
  BOOL (*prepare)(WN *, const WOPT_DSL_NORMALIZATION_REQUEST *, UINT32,
      WOPT_DSL_NORMALIZATION_LOWERING *, SRCPOS *, FILE *, FILE *);
  BOOL (*materialize)(const WOPT_DSL_NORMALIZATION_LOWERING *, SRCPOS,
      WOPT_DSL_NORMALIZATION_DEVICE *, FILE *, FILE *);
  BOOL (*verify)(const WOPT_DSL_NORMALIZATION_LOWERING *,
      const WOPT_DSL_NORMALIZATION_DEVICE *, FILE *, FILE *);
  BOOL (*environment_ready)();
} WOPT_DSL_NORMALIZATION_DRIVER_API;

extern const WOPT_DSL_NORMALIZATION_DRIVER_API WOPT_DSL_normalization_driver_api;
#endif
