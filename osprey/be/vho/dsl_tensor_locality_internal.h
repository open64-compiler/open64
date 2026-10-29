/*
 * Copyright (C) 2026 Open64 Project
 */

/*
 * Private VHO locality storage shared with its fact-derivation service.
 * Own fact/use vectors and borrow tensor facts, the be/com snapshot, and the
 * active driver PU. Retain existing runtime layout; no binary .B change.
 * Design: doc/DSL-BACKEND-OWNERSHIP-RELOCATION.md.
 */

#ifndef dsl_tensor_locality_internal_INCLUDED
#define dsl_tensor_locality_internal_INCLUDED

#include <vector>

#include "dsl_tensor_locality.h"
#include "dsl_tensor_control_snapshot_internal.h"
#include "pu_info.h"

struct DSL_TENSOR_LOCALITY_ANALYSIS {
    PU_Info *pu;
    ST_IDX owner_pu_st;
    const DSL_TENSOR_ANALYSIS *tensor_analysis;
    const DSL_TENSOR_CONTROL_SNAPSHOT *snapshot;
    std::vector<DSL_TENSOR_LOCALITY_FACT_RECORD> facts;
    std::vector<DSL_TENSOR_LOCALITY_USE_RECORD> uses;
};

#endif /* dsl_tensor_locality_internal_INCLUDED */
