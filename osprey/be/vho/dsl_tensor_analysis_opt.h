/*
 * Copyright (C) 2026 Open64 Project
 */

/* VHO ownership for AIO-3 active-PU tensor fact capture. */

#ifndef dsl_tensor_analysis_opt_INCLUDED
#define dsl_tensor_analysis_opt_INCLUDED

#include "dsl_tensor_analysis.h"

extern BOOL VHO_DSL_Tensor_Analysis_Build
                                (DSL_TENSOR_ANALYSIS *analysis,
                                 FILE *diagnostic);
extern BOOL VHO_DSL_Tensor_Analysis_Verify
                                (const DSL_TENSOR_ANALYSIS *analysis,
                                 FILE *diagnostic);

#endif
