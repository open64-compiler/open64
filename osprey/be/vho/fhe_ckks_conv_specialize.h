/*
 * Copyright (C) 2026 Open64 Project
 *
 * FHE-owned policy for specializing shared ResNet block PUs before CKKS Conv
 * expansion. Generic physical cloning remains in be/com dsl_pu_transaction.
 * Design: doc/FHE-SYNC6-S6-0C-DETAILED-EXECUTION-PLAN.md and
 * doc/FHE-SYNC6-CONTEXT-SPECIALIZATION-CONTRACT.md.
 */

#ifndef fhe_ckks_conv_specialize_INCLUDED
#define fhe_ckks_conv_specialize_INCLUDED

#include "defs.h"

/* Register the production Conv specialization policy exactly once. */
BOOL VHO_FHE_CKKS_Register_Conv_Specialization(void);

#endif /* fhe_ckks_conv_specialize_INCLUDED */
