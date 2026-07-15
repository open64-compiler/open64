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

typedef enum {
    DSL_KV_CACHE_UPDATE_INVALID = 0,
    DSL_KV_CACHE_UPDATE_FUNCTIONAL_APPEND = 1
} DSL_KV_CACHE_UPDATE;

typedef struct {
    UINT32 version;
    UINT32 batch_size;
    UINT32 decode_sequence_length;
    UINT32 query_head_count;
    UINT32 kv_head_count;
    UINT32 head_dimension;
    UINT32 input_cache_length;
    UINT32 output_cache_length;
    UINT32 cache_position;
    UINT32 rope_capacity;
    UINT32 cache_rank;
    UINT32 cache_sequence_axis;
    DSL_KV_CACHE_UPDATE cache_update;
} DSL_TRANSFORMER_DECODE_PROFILE;

extern BOOL DSL_Gatekeeper_Verify_Program
                                (PU_Info *pu_tree,
                                 FILE *diagnostic,
                                 DSL_GATEKEEPER_RESULT *result);
extern BOOL DSL_Gatekeeper_Verify_PU
                                (PU_Info *pu,
                                 FILE *diagnostic,
                                 DSL_GATEKEEPER_RESULT *result);
extern BOOL DSL_Gatekeeper_Verify_Transformer_Decode_Profile
                                (const DSL_TRANSFORMER_DECODE_PROFILE *profile,
                                 FILE *diagnostic);

#endif /* dsl_gatekeeper_INCLUDED */
