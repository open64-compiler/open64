/*
 * Copyright (C) 2026 Open64 Project
 */

/*
 * AIO-1 runtime-only tensor identity and representation evolution graph.
 * Design: doc/AI_compiler_optimization_design_v0.1.md and
 * doc/AI-COMPILER-OPTIMIZATION-AIO1-TENSOR-EVOLUTION.md.
 */

#ifndef dsl_tensor_evolution_INCLUDED
#define dsl_tensor_evolution_INCLUDED

#include <stdio.h>

#include "defs.h"
#include "dsl_ir_image.h"
#include "symtab.h"

struct pu_info;
struct dsl_tensor_evolution_graph;

typedef struct dsl_tensor_evolution_graph DSL_TENSOR_EVOLUTION_GRAPH;
typedef UINT32 DSL_TENSOR_EVOLUTION_NODE_ID;
typedef UINT32 DSL_TENSOR_EVOLUTION_EDGE_ID;

#define DSL_TENSOR_EVOLUTION_NODE_INVALID_ID ((UINT32)0)
#define DSL_TENSOR_EVOLUTION_EDGE_INVALID_ID ((UINT32)0)

typedef enum {
    DSL_TENSOR_EVOLUTION_NODE_UNKNOWN = 0,
    DSL_TENSOR_EVOLUTION_NODE_SEMANTIC = 1,
    DSL_TENSOR_EVOLUTION_NODE_LOGICAL_LAYOUT = 2,
    DSL_TENSOR_EVOLUTION_NODE_DISTRIBUTED = 3,
    DSL_TENSOR_EVOLUTION_NODE_LOCAL_PHYSICAL = 4,
    DSL_TENSOR_EVOLUTION_NODE_TILE = 5,
    DSL_TENSOR_EVOLUTION_NODE_STAGED_BUFFER = 6,
    DSL_TENSOR_EVOLUTION_NODE_REGISTER_FRAGMENT = 7,
    DSL_TENSOR_EVOLUTION_NODE_INSTRUCTION_FRAGMENT = 8
} DSL_TENSOR_EVOLUTION_NODE_KIND;

typedef enum {
    DSL_TENSOR_EVOLUTION_TRANSFORM_UNKNOWN = 0,
    DSL_TENSOR_EVOLUTION_TRANSFORM_LOGICAL_LAYOUT = 1,
    DSL_TENSOR_EVOLUTION_TRANSFORM_SHARD = 2,
    DSL_TENSOR_EVOLUTION_TRANSFORM_PLACE = 3,
    DSL_TENSOR_EVOLUTION_TRANSFORM_LOCAL_LAYOUT = 4,
    DSL_TENSOR_EVOLUTION_TRANSFORM_TILE = 5,
    DSL_TENSOR_EVOLUTION_TRANSFORM_STAGE_BUFFER = 6,
    DSL_TENSOR_EVOLUTION_TRANSFORM_REGISTER_FRAGMENT = 7,
    DSL_TENSOR_EVOLUTION_TRANSFORM_INSTRUCTION_FRAGMENT = 8
} DSL_TENSOR_EVOLUTION_TRANSFORM_KIND;

typedef enum {
    DSL_TENSOR_EVOLUTION_NODE_FLAG_NONE = 0,
    DSL_TENSOR_EVOLUTION_NODE_PROVISIONAL = 0x00000001,
    DSL_TENSOR_EVOLUTION_NODE_SELECTED = 0x00000002
} DSL_TENSOR_EVOLUTION_NODE_FLAG;

typedef enum {
    DSL_TENSOR_EVOLUTION_EDGE_FLAG_NONE = 0,
    DSL_TENSOR_EVOLUTION_EDGE_SEMANTICS_PRESERVING = 0x00000001
} DSL_TENSOR_EVOLUTION_EDGE_FLAG;

typedef struct {
    DSL_TENSOR_EVOLUTION_NODE_ID id;
    UINT32 kind;
    ST_IDX owner_pu_st;
    DSL_IR_VALUE_ID semantic_value_id;
    TY_IDX descriptor_ty;
    DSL_TENSOR_EVOLUTION_NODE_ID semantic_root_id;
    UINT32 flags;
    UINT32 representation_descriptor_id;
    UINT32 reserved;
} DSL_TENSOR_EVOLUTION_NODE_RECORD;

typedef struct {
    DSL_TENSOR_EVOLUTION_EDGE_ID id;
    DSL_TENSOR_EVOLUTION_NODE_ID source_node_id;
    DSL_TENSOR_EVOLUTION_NODE_ID result_node_id;
    UINT32 transformation_kind;
    UINT32 flags;
    UINT32 reserved0;
    UINT32 reserved1;
    UINT32 reserved2;
} DSL_TENSOR_EVOLUTION_EDGE_RECORD;

extern DSL_TENSOR_EVOLUTION_GRAPH *DSL_Tensor_Evolution_Create
                                (struct pu_info *pu, FILE *diagnostic);
extern void DSL_Tensor_Evolution_Destroy
                                (DSL_TENSOR_EVOLUTION_GRAPH *graph);
extern BOOL DSL_Tensor_Evolution_Build_Semantic_Roots
                                (DSL_TENSOR_EVOLUTION_GRAPH *graph,
                                 FILE *diagnostic);
extern BOOL DSL_Tensor_Evolution_Add_Logical_Layout
                                (DSL_TENSOR_EVOLUTION_GRAPH *graph,
                                 DSL_TENSOR_EVOLUTION_NODE_ID source_node_id,
                                 UINT32 representation_descriptor_id,
                                 DSL_TENSOR_EVOLUTION_NODE_ID *result_node_id,
                                 DSL_TENSOR_EVOLUTION_EDGE_ID *edge_id,
                                 FILE *diagnostic);
extern BOOL DSL_Tensor_Evolution_Add_Distributed
                                (DSL_TENSOR_EVOLUTION_GRAPH *graph,
                                 DSL_TENSOR_EVOLUTION_NODE_ID source_node_id,
                                 UINT32 representation_descriptor_id,
                                 UINT32 transformation_kind,
                                 DSL_TENSOR_EVOLUTION_NODE_ID *result_node_id,
                                 DSL_TENSOR_EVOLUTION_EDGE_ID *edge_id,
                                 FILE *diagnostic);
extern BOOL DSL_Tensor_Evolution_Add_Local_Physical
                                (DSL_TENSOR_EVOLUTION_GRAPH *graph,
                                 DSL_TENSOR_EVOLUTION_NODE_ID source_node_id,
                                 UINT32 representation_descriptor_id,
                                 DSL_TENSOR_EVOLUTION_NODE_ID *result_node_id,
                                 DSL_TENSOR_EVOLUTION_EDGE_ID *edge_id,
                                 FILE *diagnostic);
extern BOOL DSL_Tensor_Evolution_Verify
                                (const DSL_TENSOR_EVOLUTION_GRAPH *graph,
                                 FILE *diagnostic);
extern void DSL_Tensor_Evolution_Print
                                (FILE *file,
                                 const DSL_TENSOR_EVOLUTION_GRAPH *graph);
extern ST_IDX DSL_Tensor_Evolution_Owner
                                (const DSL_TENSOR_EVOLUTION_GRAPH *graph);
extern UINT32 DSL_Tensor_Evolution_Node_Count
                                (const DSL_TENSOR_EVOLUTION_GRAPH *graph);
extern UINT32 DSL_Tensor_Evolution_Edge_Count
                                (const DSL_TENSOR_EVOLUTION_GRAPH *graph);
extern BOOL DSL_Tensor_Evolution_Get_Node
                                (const DSL_TENSOR_EVOLUTION_GRAPH *graph,
                                 DSL_TENSOR_EVOLUTION_NODE_ID id,
                                 DSL_TENSOR_EVOLUTION_NODE_RECORD *record);
extern BOOL DSL_Tensor_Evolution_Get_Edge
                                (const DSL_TENSOR_EVOLUTION_GRAPH *graph,
                                 DSL_TENSOR_EVOLUTION_EDGE_ID id,
                                 DSL_TENSOR_EVOLUTION_EDGE_RECORD *record);
extern BOOL DSL_Tensor_Evolution_Find_Semantic_Root
                                (const DSL_TENSOR_EVOLUTION_GRAPH *graph,
                                 DSL_IR_VALUE_ID value_id,
                                 DSL_TENSOR_EVOLUTION_NODE_RECORD *record);
extern const char *DSL_Tensor_Evolution_Node_Kind_Name (UINT32 kind);
extern const char *DSL_Tensor_Evolution_Transform_Kind_Name (UINT32 kind);

#endif /* dsl_tensor_evolution_INCLUDED */
