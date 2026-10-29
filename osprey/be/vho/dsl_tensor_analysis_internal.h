/*
 * Copyright (C) 2026 Open64 Project
 */

/*
 * Private VHO storage shared by the AIO-3 runtime service and fact producer.
 * The analysis owns vectors and cached completeness, but borrows its driver PU,
 * evolution graph and all table identities; those must outlive every use.
 * The producer maintains contiguous IDs/use spans and invalidates or rebuilds
 * facts when their source IR changes. This is not a mapped record and changes
 * no .B rows, sections or layout.
 * Ownership: doc/DSL-BACKEND-OWNERSHIP-RELOCATION.md.
 * Fact semantics: doc/AI-COMPILER-OPTIMIZATION-AIO3-SEMANTIC-TENSOR.md.
 */

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
