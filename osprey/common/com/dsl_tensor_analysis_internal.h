/*
 * Copyright (C) 2026 Open64 Project
 */

/* Private shared storage for common TensorAnalysisIR and its VHO producer. */

#ifndef dsl_tensor_analysis_internal_INCLUDED
#define dsl_tensor_analysis_internal_INCLUDED

#include <vector>

#include "dsl_tensor_analysis.h"
#include "pu_info.h"

struct DSL_TENSOR_ANALYSIS {
    PU_Info *pu;
    ST_IDX owner_pu_st;
    const DSL_TENSOR_EVOLUTION_GRAPH *graph;
    std::vector<DSL_TENSOR_FACT_RECORD> facts;
    std::vector<DSL_TENSOR_USE_FACT_RECORD> uses;
    UINT32 incomplete_fact_count;
};

#endif
