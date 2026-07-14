/*
 * Copyright (C) 2026 Open64 Project
 */

#ifndef dsl_gatekeeper_INCLUDED
#define dsl_gatekeeper_INCLUDED

#include <stdio.h>

#include "defs.h"
#include "pu_info.h"

typedef struct {
    UINT32 native_node_count;
    UINT32 result_symbol_count;
    UINT32 error_count;
} DSL_GATEKEEPER_RESULT;

extern BOOL DSL_Gatekeeper_Verify_Program
                                (PU_Info *pu_tree,
                                 FILE *diagnostic,
                                 DSL_GATEKEEPER_RESULT *result);
extern BOOL DSL_Gatekeeper_Verify_PU
                                (PU_Info *pu,
                                 FILE *diagnostic,
                                 DSL_GATEKEEPER_RESULT *result);

#endif /* dsl_gatekeeper_INCLUDED */
