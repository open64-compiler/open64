/*
 * Copyright (C) 2026 Open64 Project
 */

/*
 * WOPT owns CFG capture into the be/com AIO-4 PU-local control snapshot.
 * Consumers borrow copied IDs and facts; no WOPT object pointer is retained.
 * Runtime-only capture changes no binary .B representation.
 * Design: doc/AI-COMPILER-OPTIMIZATION-AIO4-LIFETIME-LOCALITY.md.
 */

#ifndef opt_dsl_locality_INCLUDED
#define opt_dsl_locality_INCLUDED

#include <stdio.h>

#include "defs.h"

class CFG;
struct pu_info;
struct DSL_TENSOR_CONTROL_SNAPSHOT;

/*
 * Copy blocks and DSL-definition positions from the active PU CFG into a
 * caller-owned unsealed snapshot, then seal it. Borrow the CFG only during this
 * call; return FALSE on invalid input or rejected append/seal. Partial copied
 * state remains on failure, so the caller owns disposal rather than rollback.
 */
extern BOOL WOPT_DSL_populate_tensor_control_snapshot
    (CFG *cfg, struct pu_info *pu,
     struct DSL_TENSOR_CONTROL_SNAPSHOT *snapshot,
     FILE *diagnostic);

#endif /* opt_dsl_locality_INCLUDED */
