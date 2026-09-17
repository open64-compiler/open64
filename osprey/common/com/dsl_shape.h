/*
 * Copyright (C) 2026 Open64 Project
 */

#ifndef dsl_shape_INCLUDED
#define dsl_shape_INCLUDED

#include <stddef.h>
#include <stdio.h>

#include "defs.h"
#include "dsl_ir_image.h"
#include "dsl_opcode.h"
#include "symtab_idx.h"

typedef struct pu_info PU_Info;
class WN;

#define DSL_SHAPE_MAX_RANK 16

typedef enum {
    DSL_SHAPE_CHECK_UNREGISTERED = 0,
    DSL_SHAPE_CHECK_VALID = 1,
    DSL_SHAPE_CHECK_INVALID = 2
} DSL_SHAPE_CHECK_RESULT;

typedef struct {
    DSL_OPERATOR dsl_operator;
    UINT16 version;
    const DSL_IR_NODE_RECORD *node;
    const TY_IDX *operand_types;
    UINT32 operand_count;
    TY_IDX result_ty;
} DSL_SHAPE_OPERATOR_INPUT;

typedef enum {
    DSL_SHAPE_FACT_PENDING = 0,
    DSL_SHAPE_FACT_COMPLETE = 1,
    DSL_SHAPE_FACT_CONTRADICTION = 2
} DSL_SHAPE_FACT_STATE;

typedef struct {
    UINT32 state;
    INT32 rank;
    UINT8 dimension_known[DSL_SHAPE_MAX_RANK];
    UINT64 dimension[DSL_SHAPE_MAX_RANK];
} DSL_SHAPE_FACT;

typedef enum {
    DSL_SHAPE_INFERENCE_UNREGISTERED = 0,
    DSL_SHAPE_INFERENCE_PENDING = 1,
    DSL_SHAPE_INFERENCE_COMPLETE = 2,
    DSL_SHAPE_INFERENCE_CONTRADICTION = 3
} DSL_SHAPE_INFERENCE_RESULT;

typedef struct {
    DSL_OPERATOR dsl_operator;
    UINT16 version;
    const DSL_IR_NODE_RECORD *node;
    const TY_IDX *operand_types;
    const DSL_SHAPE_FACT *operand_facts;
    UINT32 operand_count;
    TY_IDX result_ty;
} DSL_SHAPE_INFERENCE_INPUT;

typedef struct {
    UINT32 visited_node_count;
    UINT32 value_count;
    UINT32 unchanged_value_count;
    UINT32 refinable_value_count;
    UINT32 pending_value_count;
    UINT32 unresolved_value_count;
    UINT32 contradiction_count;
    UINT32 iteration_count;
    UINT32 diagnostic_count;
} DSL_SHAPE_SOLVER_RESULT;

extern BOOL DSL_Shape_Tensor_Core_Complete(TY_IDX ty);
extern BOOL DSL_Shape_Parse_Static_Dimensions
                                (const char *shape,
                                 UINT64 *dimensions,
                                 UINT32 capacity,
                                 UINT32 *rank);
extern BOOL DSL_Shape_Format_Static_Dimensions
                                (const UINT64 *dimensions,
                                 UINT32 rank,
                                 char *buffer,
                                 size_t buffer_size);
extern BOOL DSL_Shape_Tensor_Compatible
                                (TY_IDX ty0,
                                 TY_IDX ty1,
                                 BOOL require_shape);
extern BOOL DSL_Shape_Has_Operator_Rule
                                (DSL_OPERATOR dsl_operator,
                                 UINT16 version);
extern BOOL DSL_Shape_Fact_From_Type (TY_IDX ty, DSL_SHAPE_FACT *fact);
extern DSL_SHAPE_INFERENCE_RESULT DSL_Shape_Infer_Operator
                                (const DSL_SHAPE_INFERENCE_INPUT *input,
                                 DSL_SHAPE_FACT *result);
extern DSL_SHAPE_CHECK_RESULT DSL_Shape_Check_Operator
                                (const DSL_SHAPE_OPERATOR_INPUT *input);
extern BOOL DSL_Shape_Analyze_PU (PU_Info *pu, WN *tree, FILE *diagnostic,
                                  DSL_SHAPE_SOLVER_RESULT *result);

#endif /* dsl_shape_INCLUDED */
