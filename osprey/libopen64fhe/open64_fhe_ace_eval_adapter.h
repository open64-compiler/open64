/*
 * Copyright (C) 2026 Open64 Project
 *
 * Open64-owned ownership/status adapter for ACE-shaped CKKS arithmetic.
 * Design: doc/FHE-ACE-RTLIB-RUNTIME-DECISION.md and
 * doc/FHE-SYNC6-CKKS-IR-CONFORMANCE-GATE.md. This private API is not ABI v1.
 */

#ifndef open64_fhe_ace_eval_adapter_INCLUDED
#define open64_fhe_ace_eval_adapter_INCLUDED

#include "open64_fhe_runtime_abi.h"
#include "open64_fhe_ace_api.h"

enum OPEN64_FHE_ACE_EVAL_OPERATION {
  OPEN64_FHE_ACE_EVAL_ADD = 1,
  OPEN64_FHE_ACE_EVAL_SUB = 2,
  OPEN64_FHE_ACE_EVAL_MUL = 3,
  OPEN64_FHE_ACE_EVAL_RESCALE = 4,
  OPEN64_FHE_ACE_EVAL_ROTATE = 5,
  OPEN64_FHE_ACE_EVAL_BOOTSTRAP = 6
};

/* Borrow inputs and publish one distinct owned result only after success. */
open64_fhe_status_v1 Open64_FHE_ACE_Evaluate(
    OPEN64_FHE_ACE_EVAL_OPERATION operation,
    CIPHER left,
    CIPHER right,
    int32_t parameter,
    CIPHER *out_result);

/* Release one adapter-owned result; callers own their imported inputs. */
void Open64_FHE_ACE_Release(CIPHER *value);

#endif /* open64_fhe_ace_eval_adapter_INCLUDED */
