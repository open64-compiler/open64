/*
 * Copyright (C) 2026 Open64 Project
 *
 * Purpose: register FHE-owned SYNC-5 production lowering and final-verifier
 * callbacks with the shared VHO runtime checkpoint shell.
 * Design: doc/FHE-SYNC5-NATIVE-OWNERSHIP-AUDIT.md, S5-G.
 */

#ifndef fhe_runtime_production_INCLUDED
#define fhe_runtime_production_INCLUDED

#include "defs.h"

/* Register the one approved ResNet-20 runtime-lowering policy per process. */
extern BOOL VHO_FHE_Register_Default_Runtime_Production (void);

#endif /* fhe_runtime_production_INCLUDED */
