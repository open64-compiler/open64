/*
 * Copyright (C) 2026 Open64 Project
 */

/* Private shared storage for common locality IR and its phase producers. */

#ifndef dsl_tensor_locality_internal_INCLUDED
#define dsl_tensor_locality_internal_INCLUDED

#include <vector>

#include "dsl_tensor_locality.h"
#include "pu_info.h"

struct DSL_TENSOR_CONTROL_SNAPSHOT {
    PU_Info *pu;
    ST_IDX owner_pu_st;
    BOOL sealed;
    std::vector<DSL_TENSOR_CONTROL_BLOCK> blocks;
    std::vector<DSL_TENSOR_CONTROL_POSITION> positions;
};

struct DSL_TENSOR_LOCALITY_ANALYSIS {
    PU_Info *pu;
    ST_IDX owner_pu_st;
    const DSL_TENSOR_ANALYSIS *tensor_analysis;
    const DSL_TENSOR_CONTROL_SNAPSHOT *snapshot;
    std::vector<DSL_TENSOR_LOCALITY_FACT_RECORD> facts;
    std::vector<DSL_TENSOR_LOCALITY_USE_RECORD> uses;
};

#endif
