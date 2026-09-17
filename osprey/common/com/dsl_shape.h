/*
 * Copyright (C) 2026 Open64 Project
 */

#ifndef dsl_shape_INCLUDED
#define dsl_shape_INCLUDED

#include <stddef.h>

#include "defs.h"
#include "dsl_ir_image.h"
#include "dsl_opcode.h"
#include "symtab_idx.h"

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
extern DSL_SHAPE_CHECK_RESULT DSL_Shape_Check_Operator
                                (const DSL_SHAPE_OPERATOR_INPUT *input);

#endif /* dsl_shape_INCLUDED */
