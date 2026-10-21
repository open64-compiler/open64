/*
 * Copyright (C) 2026 Open64 Project
 *
 * Private helpers shared by single-value and grouped native DSL lowering.
 * Public callers use dsl_ir_image.h; this header does not extend the image.
 */

#ifndef dsl_ir_lower_internal_INCLUDED
#define dsl_ir_lower_internal_INCLUDED

#include <vector>

#include "dsl_ir_image.h"
#include "pu_info.h"
#include "wn.h"

typedef struct {
    const DSL_IR_NATIVE_VALUE_LOWER_REQUEST *request;
    UINT32 request_index;
    UINT32 tree_order;
    UINT32 block_ordinal;
    UINT32 statement_count;
    DSL_IR_VALUE_RECORD value;
    DSL_IR_NODE_RECORD node;
    DSL_IR_OPCODE_DESCRIPTOR_RECORD opcode;
    ST_IDX handle_st;
    TY_IDX handle_ty;
    ST_IDX local_projection_st;
    DSL_RUNTIME_VALUE_PROJECTION_ID local_projection_id;
    WN *containing_block;
    WN *result_handle_definition;
} DSL_IR_NATIVE_VALUE_LOWER_JOURNAL;

typedef struct {
    ST_IDX source_st;
    ST_IDX handle_st;
    const WN *result_handle_definition;
    SRCPOS source_position;
    UINT32 handle_definition_count;
    UINT32 statement_count;
    BOOL valid;
} DSL_IR_LOWER_STANDARD_BLOCK_SCAN;

extern BOOL DSL_IR_Lower_Current_PU_Is (PU_Info *pu_info);
extern BOOL DSL_IR_Lower_Report (FILE *diagnostic, UINT32 request_index,
                                 const char *message);
extern void DSL_IR_Lower_Scan_Standard_Block
                    (const WN *tree, DSL_IR_LOWER_STANDARD_BLOCK_SCAN *scan);

#endif /* dsl_ir_lower_internal_INCLUDED */
