/*
 * Copyright (C) 2026 Open64 Project
 *
 * Private helpers shared by single-value and grouped native DSL lowering.
 * Public callers use dsl_ir_lower.h; this header does not extend the image.
 * Design: doc/WHIRL-DSL-INFRASTRUCTURE.md, persistent IR / service separation.
 */

#ifndef dsl_ir_lower_internal_INCLUDED
#define dsl_ir_lower_internal_INCLUDED

#include <vector>
#include "dsl_ir_lower.h"

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

/* Test exact active-PU pointer identity without changing scope or resident
 * locals. */
extern BOOL DSL_IR_Lower_Current_PU_Is (PU_Info *pu_info);
/* Emit a lowering diagnostic when requested and return FALSE; no IR mutation. */
extern BOOL DSL_IR_Lower_Report (FILE *diagnostic, UINT32 request_index,
                                 const char *message);
/* Read-only preflight of a detached standard block and final result, filling
 * caller scratch; rejection does not consume or attach WNs. */
extern void DSL_IR_Lower_Scan_Standard_Block
                    (const WN *tree, DSL_IR_LOWER_STANDARD_BLOCK_SCAN *scan);

/* Private group preflight state, never a persistent image row. */
typedef struct {
    const DSL_IR_NATIVE_GROUP_LOWER_REQUEST *request;
    UINT32 request_index;
    UINT32 first_member;
    UINT32 member_count;
    UINT32 first_order;
    UINT32 last_order;
    UINT32 statement_count;
    WN *containing_block;
    WN *first_definition;
    WN *output_definition;
    DSL_IR_VALUE_RECORD semantic_source;
    DSL_IR_VALUE_RECORD output;
    ST_IDX handle_st;
    TY_IDX handle_ty;
} DSL_IR_GROUP_LOWER_JOURNAL;

struct DSL_IR_GROUP_LOWER_LESS {
    /* Compare preflight tree order only; neither journal nor WN is changed. */
    inline BOOL operator()
        (const DSL_IR_GROUP_LOWER_JOURNAL &left,
         const DSL_IR_GROUP_LOWER_JOURNAL &right) const
    {
        return left.first_order < right.first_order;
    }
};

#endif /* dsl_ir_lower_internal_INCLUDED */
