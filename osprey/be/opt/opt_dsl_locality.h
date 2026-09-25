/*
 * Copyright (C) 2026 Open64 Project
 */

#ifndef opt_dsl_locality_INCLUDED
#define opt_dsl_locality_INCLUDED

#include <stdio.h>

#include "defs.h"

class CFG;
struct pu_info;
struct dsl_tensor_control_snapshot;

extern BOOL WOPT_DSL_Populate_Tensor_Control_Snapshot
    (CFG *cfg, struct pu_info *pu,
     struct dsl_tensor_control_snapshot *snapshot,
     FILE *diagnostic);

#endif /* opt_dsl_locality_INCLUDED */
