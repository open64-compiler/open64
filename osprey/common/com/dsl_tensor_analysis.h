/*
 * Copyright (C) 2026 Open64 Project
 */

/*
 * AIO-3 PU-local semantic tensor facts derived from logical DSL contracts.
 * The analysis reads managed IR but adds no persistent image state.
 * Design: doc/AI_compiler_optimization_design_v0.1.md and
 * doc/AI-COMPILER-OPTIMIZATION-AIO3-SEMANTIC-TENSOR.md.
 */

#ifndef dsl_tensor_analysis_INCLUDED
#define dsl_tensor_analysis_INCLUDED

#include <stdio.h>

#include "defs.h"
#include "dsl_ir_image.h"
#include "dsl_opcode.h"
#include "dsl_tensor_evolution.h"
#include "symtab.h"

struct pu_info;
struct DSL_TENSOR_ANALYSIS;

typedef struct DSL_TENSOR_ANALYSIS DSL_TENSOR_ANALYSIS;
typedef UINT32 DSL_TENSOR_FACT_ID;
typedef UINT32 DSL_TENSOR_USE_FACT_ID;

#define DSL_TENSOR_FACT_INVALID_ID ((UINT32)0)
#define DSL_TENSOR_USE_FACT_INVALID_ID ((UINT32)0)
#define DSL_TENSOR_DIMENSION_COUNT_UNKNOWN (~(UINT32)0)

typedef enum {
    DSL_TENSOR_DIMENSION_UNKNOWN = 0,
    DSL_TENSOR_DIMENSION_STATIC = 1,
    DSL_TENSOR_DIMENSION_UNRESOLVED = 2
} DSL_TENSOR_DIMENSION_STATE;

typedef enum {
    DSL_TENSOR_OWNERSHIP_UNKNOWN = 0,
    DSL_TENSOR_OWNERSHIP_UNIQUE = 1,
    DSL_TENSOR_OWNERSHIP_SHARED = 2
} DSL_TENSOR_OWNERSHIP;

typedef enum {
    DSL_TENSOR_REUSE_UNKNOWN = 0,
    DSL_TENSOR_REUSE_UNUSED = 1,
    DSL_TENSOR_REUSE_SINGLE_USE = 2,
    DSL_TENSOR_REUSE_MULTIPLE_USE = 3
} DSL_TENSOR_REUSE_ROLE;

typedef enum {
    DSL_TENSOR_VALUE_ROLE_UNKNOWN = 0,
    DSL_TENSOR_VALUE_ROLE_CONSTANT = 1,
    DSL_TENSOR_VALUE_ROLE_MODEL_INPUT = 2,
    DSL_TENSOR_VALUE_ROLE_FORMAL = 3,
    DSL_TENSOR_VALUE_ROLE_INTERMEDIATE = 4,
    DSL_TENSOR_VALUE_ROLE_SYMBOL = 5
} DSL_TENSOR_VALUE_ROLE;

typedef enum {
    DSL_TENSOR_USE_ROLE_UNKNOWN = 0,
    DSL_TENSOR_USE_ROLE_GENERIC = 1,
    DSL_TENSOR_USE_ROLE_CONTRACTION_KID0 = 2,
    DSL_TENSOR_USE_ROLE_CONTRACTION_KID1 = 3,
    DSL_TENSOR_USE_ROLE_ACTIVATION = 4,
    DSL_TENSOR_USE_ROLE_WEIGHT = 5,
    DSL_TENSOR_USE_ROLE_BIAS = 6,
    DSL_TENSOR_USE_ROLE_SCALE = 7,
    DSL_TENSOR_USE_ROLE_MEAN = 8,
    DSL_TENSOR_USE_ROLE_VARIANCE = 9,
    DSL_TENSOR_USE_ROLE_QUERY = 10,
    DSL_TENSOR_USE_ROLE_KEY = 11,
    DSL_TENSOR_USE_ROLE_VALUE = 12,
    DSL_TENSOR_USE_ROLE_INDEX = 13,
    DSL_TENSOR_USE_ROLE_VIEW_SOURCE = 14,
    DSL_TENSOR_USE_ROLE_REDUCTION_SOURCE = 15,
    DSL_TENSOR_USE_ROLE_ELEMENTWISE_INPUT = 16
} DSL_TENSOR_USE_ROLE;

enum {
    DSL_TENSOR_FACT_COMPLETE_TYPE_CORE = 0x00000001,
    DSL_TENSOR_FACT_COMPLETE_SHAPE = 0x00000002,
    DSL_TENSOR_FACT_COMPLETE_PRODUCER = 0x00000004,
    DSL_TENSOR_FACT_COMPLETE_OWNERSHIP = 0x00000008,
    DSL_TENSOR_FACT_COMPLETE_CONSUMERS = 0x00000010,
    DSL_TENSOR_FACT_COMPLETE_ALL = 0x0000001f
};

typedef struct {
    DSL_TENSOR_FACT_ID id;
    DSL_TENSOR_EVOLUTION_NODE_ID semantic_root_id;
    DSL_IR_VALUE_ID value_id;
    TY_IDX descriptor_ty;
    TY_IDX element_ty;
    DSL_IR_NODE_ID producer_node_id;
    UINT32 producer_operator;
    UINT16 producer_version;
    UINT16 reserved0;
    INT32 rank;
    UINT32 dimension_state;
    UINT32 static_dimension_count;
    UINT32 dynamic_dimension_count;
    UINT32 ownership;
    UINT32 reuse_role;
    UINT32 value_role;
    DSL_TENSOR_USE_FACT_ID first_use_id;
    UINT32 use_count;
    UINT32 completeness;
    UINT32 reserved1;
} DSL_TENSOR_FACT_RECORD;

typedef struct {
    DSL_TENSOR_USE_FACT_ID id;
    DSL_TENSOR_FACT_ID tensor_fact_id;
    DSL_IR_NODE_ID consumer_node_id;
    UINT32 consumer_operator;
    UINT16 consumer_version;
    UINT16 operand_ordinal;
    UINT32 role;
    UINT32 shape_rule;
    UINT32 memory_behavior;
    UINT32 reserved;
} DSL_TENSOR_USE_FACT_RECORD;

extern DSL_TENSOR_ANALYSIS *DSL_tensor_analysis_create
                                (struct pu_info *pu,
                                 const DSL_TENSOR_EVOLUTION_GRAPH *graph,
                                 FILE *diagnostic);
extern void DSL_tensor_analysis_destroy (DSL_TENSOR_ANALYSIS *analysis);
extern BOOL DSL_tensor_analysis_build
                                (DSL_TENSOR_ANALYSIS *analysis,
                                 FILE *diagnostic);
extern BOOL DSL_tensor_analysis_verify
                                (const DSL_TENSOR_ANALYSIS *analysis,
                                 FILE *diagnostic);
extern BOOL DSL_tensor_analysis_is_complete
                                (const DSL_TENSOR_ANALYSIS *analysis);
extern void DSL_tensor_analysis_print
                                (FILE *file,
                                 const DSL_TENSOR_ANALYSIS *analysis);
extern UINT32 DSL_tensor_analysis_fact_count
                                (const DSL_TENSOR_ANALYSIS *analysis);
extern UINT32 DSL_tensor_analysis_use_count
                                (const DSL_TENSOR_ANALYSIS *analysis);
extern BOOL DSL_tensor_analysis_get_fact
                                (const DSL_TENSOR_ANALYSIS *analysis,
                                 DSL_TENSOR_FACT_ID id,
                                 DSL_TENSOR_FACT_RECORD *record);
extern BOOL DSL_tensor_analysis_get_use
                                (const DSL_TENSOR_ANALYSIS *analysis,
                                 DSL_TENSOR_USE_FACT_ID id,
                                 DSL_TENSOR_USE_FACT_RECORD *record);
extern BOOL DSL_tensor_analysis_find_fact
                                (const DSL_TENSOR_ANALYSIS *analysis,
                                 DSL_IR_VALUE_ID value_id,
                                 DSL_TENSOR_FACT_RECORD *record);
extern const char *DSL_tensor_dimension_state_name (UINT32 state);
extern const char *DSL_tensor_ownership_name (UINT32 ownership);
extern const char *DSL_tensor_reuse_role_name (UINT32 role);
extern const char *DSL_tensor_value_role_name (UINT32 role);
extern const char *DSL_tensor_use_role_name (UINT32 role);

#endif /* dsl_tensor_analysis_INCLUDED */
