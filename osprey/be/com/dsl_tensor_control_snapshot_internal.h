/*
 * Copyright (C) 2026 Open64 Project
 */

/*
 * Private be/com storage for one copied control snapshot per caller handle.
 * The PU pointer is borrowed driver context, not authority to activate a PU;
 * owned vectors contain copied scalar facts, never WOPT objects. No .B change.
 * Design: doc/DSL-BACKEND-OWNERSHIP-RELOCATION.md.
 */

#ifndef dsl_tensor_control_snapshot_internal_INCLUDED
#define dsl_tensor_control_snapshot_internal_INCLUDED

#include <vector>

#include "dsl_tensor_control_snapshot.h"
#include "pu_info.h"

struct DSL_TENSOR_CONTROL_SNAPSHOT {
    PU_Info *pu;
    ST_IDX owner_pu_st;
    BOOL sealed;
    std::vector<DSL_TENSOR_CONTROL_BLOCK> blocks;
    std::vector<DSL_TENSOR_CONTROL_POSITION> positions;
};

#endif /* dsl_tensor_control_snapshot_internal_INCLUDED */
