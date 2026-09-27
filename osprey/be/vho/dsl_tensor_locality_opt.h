/*
 * Copyright (C) 2026 Open64 Project
 */

/* VHO ownership for AIO-4 PU-local lifetime and locality derivation. */

#ifndef dsl_tensor_locality_opt_INCLUDED
#define dsl_tensor_locality_opt_INCLUDED

#include "dsl_tensor_locality.h"

extern BOOL VHO_DSL_Tensor_Locality_Build
                                (DSL_TENSOR_LOCALITY_ANALYSIS *analysis,
                                 FILE *diagnostic);

#endif
