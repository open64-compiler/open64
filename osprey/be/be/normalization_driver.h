/* Copyright (C) 2026 Open64 Project */

/* Backend-owned LN4 checkpoint lifecycle, not optimization policy.
 * Design: doc/AI-COMPILER-LAYERNORM-DRIVER-REQUEST-v0.1.md. */
#ifndef normalization_driver_INCLUDED
#define normalization_driver_INCLUDED
#include "defs.h"
#include "pu_info.h"
struct WOPT_DSL_NORMALIZATION_DRIVER_API;
/* Resident PU lifetime and context switches belong to the driver. PREOPT and
 * WOPT callbacks operate only in the established active scope. No host/GPU run. */
extern BOOL BE_DSL_normalization_checkpoint(PU_Info *program,
    const WOPT_DSL_NORMALIZATION_DRIVER_API *api, WN *(*preopt)(WN *, WN *), FILE *diagnostic);
#endif
