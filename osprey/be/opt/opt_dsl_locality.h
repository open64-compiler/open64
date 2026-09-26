/*
 * Copyright (C) 2026 Open64 Project
 */

/*
 * WOPT adapter for the AIO-4 PU-local control snapshot. The common analysis
 * consumes copied IDs and facts; it never retains WOPT object pointers.
 * Design: doc/AI-COMPILER-OPTIMIZATION-AIO4-LIFETIME-LOCALITY.md.
 */

#ifndef opt_dsl_locality_INCLUDED
#define opt_dsl_locality_INCLUDED

#include <stdio.h>

#include "defs.h"

class CFG;
struct pu_info;
struct DSL_TENSOR_CONTROL_SNAPSHOT;

extern BOOL WOPT_DSL_Populate_Tensor_Control_Snapshot
    (CFG *cfg, struct pu_info *pu,
     struct DSL_TENSOR_CONTROL_SNAPSHOT *snapshot,
     FILE *diagnostic);

#endif /* opt_dsl_locality_INCLUDED */
