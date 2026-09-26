/*
 * Copyright (C) 2026 Open64 Project
 */

#include <string.h>
#include <vector>

#include "dsl_tensor_evolution.h"
#include "pu_info.h"
#include "strtab.h"

struct dsl_tensor_evolution_graph {
    PU_Info *pu;
    ST_IDX owner_pu_st;
    std::vector<DSL_TENSOR_EVOLUTION_NODE_RECORD> nodes;
    std::vector<DSL_TENSOR_EVOLUTION_EDGE_RECORD> edges;
};

static const char *DSL_tensor_evolution_node_kind_name[] = {
    "unknown",
    "semantic",
    "logical_layout",
    "distributed",
    "local_physical",
    "tile",
    "staged_buffer",
    "register_fragment",
    "instruction_fragment"
};

static const char *DSL_tensor_evolution_transform_kind_name[] = {
    "unknown",
    "logical_layout",
    "shard",
    "place",
    "local_layout",
    "tile",
    "stage_buffer",
    "register_fragment",
    "instruction_fragment"
};

static BOOL
DSL_Tensor_Evolution_Report
        (FILE *diagnostic, const char *message, UINT32 id)
{
    if (diagnostic != NULL)
        fprintf(diagnostic, "TensorEvolutionGraph error: %s id=%u\n",
                message, id);
    return FALSE;
}

static BOOL
DSL_Tensor_Evolution_Owner_Valid (ST_IDX owner_pu_st)
{
    return ST_IDX_level(owner_pu_st) == GLOBAL_SYMTAB &&
           ST_IDX_index(owner_pu_st) != 0 &&
           ST_IDX_index(owner_pu_st) < ST_Table_Size(GLOBAL_SYMTAB) &&
           ST_class(owner_pu_st) == CLASS_FUNC &&
           ST_pu(St_Table[owner_pu_st]) != PU_IDX_ZERO &&
           ST_pu(St_Table[owner_pu_st]) < PU_Table_Size();
}

static BOOL
DSL_Tensor_Evolution_Active
        (const DSL_TENSOR_EVOLUTION_GRAPH *graph)
{
    return graph != NULL && graph->pu != NULL &&
           Current_PU_Info == graph->pu &&
           DSL_Tensor_Evolution_Owner_Valid(graph->owner_pu_st) &&
           PU_Info_proc_sym(graph->pu) == graph->owner_pu_st &&
           Current_pu == &Pu_Table[ST_pu(St_Table[graph->owner_pu_st])];
}

static BOOL
DSL_Tensor_Evolution_Value_Owned
        (const DSL_TENSOR_EVOLUTION_GRAPH *graph,
         const DSL_IR_VALUE_RECORD &value)
{
    DSL_IR_VALUE_RECORD resolved;

    if (!DSL_Tensor_Evolution_Active(graph) ||
        value.id == DSL_IR_VALUE_INVALID_ID ||
        value.name == STR_IDX_ZERO || ST_IDX_index(value.st) == 0 ||
        ST_IDX_level(value.st) != CURRENT_SYMTAB ||
        ST_IDX_index(value.st) >= ST_Table_Size(CURRENT_SYMTAB) ||
        value.ty != ST_type(St_Table[value.st]))
        return FALSE;

    return DSL_IR_Image_Find_PU_Value
               (value.st, Index_To_Str(value.name),
                ST_name(St_Table[graph->owner_pu_st]), &resolved) &&
           resolved.id == value.id;
}

static BOOL
DSL_Tensor_Evolution_Descriptor_Valid (TY_IDX ty)
{
    return TY_IDX_index(ty) != 0 && TY_is_tensor_extension(ty) &&
           TY_tensor_is_canonical(ty);
}

const char *
DSL_Tensor_Evolution_Node_Kind_Name (UINT32 kind)
{
    return kind < sizeof(DSL_tensor_evolution_node_kind_name) /
                      sizeof(DSL_tensor_evolution_node_kind_name[0]) ?
           DSL_tensor_evolution_node_kind_name[kind] : "unknown";
}

const char *
DSL_Tensor_Evolution_Transform_Kind_Name (UINT32 kind)
{
    return kind < sizeof(DSL_tensor_evolution_transform_kind_name) /
                      sizeof(DSL_tensor_evolution_transform_kind_name[0]) ?
           DSL_tensor_evolution_transform_kind_name[kind] : "unknown";
}

DSL_TENSOR_EVOLUTION_GRAPH *
DSL_Tensor_Evolution_Create (PU_Info *pu, FILE *diagnostic)
{
    if (pu == NULL || Current_PU_Info != pu ||
        !DSL_Tensor_Evolution_Owner_Valid(PU_Info_proc_sym(pu)) ||
        Current_pu != &Pu_Table[ST_pu(St_Table[PU_Info_proc_sym(pu)])]) {
        DSL_Tensor_Evolution_Report(diagnostic,
                                    "program unit is not active", 0);
        return NULL;
    }

    DSL_TENSOR_EVOLUTION_GRAPH *graph =
        new DSL_TENSOR_EVOLUTION_GRAPH;
    graph->pu = pu;
    graph->owner_pu_st = PU_Info_proc_sym(pu);
    return graph;
}

void
DSL_Tensor_Evolution_Destroy (DSL_TENSOR_EVOLUTION_GRAPH *graph)
{
    delete graph;
}

BOOL
DSL_Tensor_Evolution_Find_Semantic_Root
        (const DSL_TENSOR_EVOLUTION_GRAPH *graph,
         DSL_IR_VALUE_ID value_id,
         DSL_TENSOR_EVOLUTION_NODE_RECORD *record)
{
    if (graph == NULL || value_id == DSL_IR_VALUE_INVALID_ID)
        return FALSE;
    for (UINT32 i = 0; i < graph->nodes.size(); ++i) {
        const DSL_TENSOR_EVOLUTION_NODE_RECORD &node = graph->nodes[i];
        if (node.kind == DSL_TENSOR_EVOLUTION_NODE_SEMANTIC &&
            node.semantic_value_id == value_id) {
            if (record != NULL)
                *record = node;
            return TRUE;
        }
    }
    return FALSE;
}

BOOL
DSL_Tensor_Evolution_Build_Semantic_Roots
        (DSL_TENSOR_EVOLUTION_GRAPH *graph, FILE *diagnostic)
{
    if (!DSL_Tensor_Evolution_Active(graph))
        return DSL_Tensor_Evolution_Report
                   (diagnostic, "program unit is not active", 0);
    if (!DSL_IR_Image_Validate(diagnostic))
        return FALSE;

    /*
     * A semantic root is the stable identity of one live tensor value. Later
     * representation choices branch from it; they never replace or retype it.
     */
    for (DSL_IR_VALUE_ID id = 1; id <= DSL_IR_Image_Value_Count(); ++id) {
        DSL_IR_VALUE_RECORD value;
        DSL_TENSOR_EVOLUTION_NODE_RECORD root;
        if (!DSL_IR_Image_Get_Value(id, &value))
            return DSL_Tensor_Evolution_Report
                       (diagnostic, "missing DSL value", id);
        if ((value.flags & DSL_IR_VALUE_FLAG_REDIRECTED) != 0 ||
            !DSL_Tensor_Evolution_Value_Owned(graph, value))
            continue;
        if (!TY_is_tensor_extension(value.ty))
            continue;
        if (!DSL_Tensor_Evolution_Descriptor_Valid(value.ty))
            return DSL_Tensor_Evolution_Report
                       (diagnostic, "tensor descriptor is not canonical", id);
        if (DSL_Tensor_Evolution_Find_Semantic_Root(graph, id, &root)) {
            if (root.descriptor_ty != value.ty)
                return DSL_Tensor_Evolution_Report
                           (diagnostic, "semantic root changed type", id);
            continue;
        }

        memset(&root, 0, sizeof(root));
        root.id = graph->nodes.size() + 1;
        root.kind = DSL_TENSOR_EVOLUTION_NODE_SEMANTIC;
        root.owner_pu_st = graph->owner_pu_st;
        root.semantic_value_id = value.id;
        root.descriptor_ty = value.ty;
        root.semantic_root_id = root.id;
        graph->nodes.push_back(root);
    }
    return DSL_Tensor_Evolution_Verify(graph, diagnostic);
}

BOOL
DSL_Tensor_Evolution_Add_Logical_Layout
        (DSL_TENSOR_EVOLUTION_GRAPH *graph,
         DSL_TENSOR_EVOLUTION_NODE_ID source_node_id,
         UINT32 representation_descriptor_id,
         DSL_TENSOR_EVOLUTION_NODE_ID *result_node_id,
         DSL_TENSOR_EVOLUTION_EDGE_ID *edge_id, FILE *diagnostic)
{
    if (result_node_id != NULL)
        *result_node_id = DSL_TENSOR_EVOLUTION_NODE_INVALID_ID;
    if (edge_id != NULL)
        *edge_id = DSL_TENSOR_EVOLUTION_EDGE_INVALID_ID;
    if (!DSL_Tensor_Evolution_Active(graph) || source_node_id == 0 ||
        source_node_id > graph->nodes.size() ||
        representation_descriptor_id == 0 || result_node_id == NULL ||
        edge_id == NULL)
        return DSL_Tensor_Evolution_Report
                   (diagnostic, "invalid logical layout input",
                    source_node_id);

    const DSL_TENSOR_EVOLUTION_NODE_RECORD &source =
        graph->nodes[source_node_id - 1];
    if (source.kind != DSL_TENSOR_EVOLUTION_NODE_SEMANTIC)
        return DSL_Tensor_Evolution_Report
                   (diagnostic, "invalid logical layout source",
                    source_node_id);
    for (UINT32 i = 0; i < graph->nodes.size(); ++i) {
        const DSL_TENSOR_EVOLUTION_NODE_RECORD &candidate = graph->nodes[i];
        if (candidate.kind == DSL_TENSOR_EVOLUTION_NODE_LOGICAL_LAYOUT &&
            candidate.semantic_root_id == source.semantic_root_id &&
            candidate.representation_descriptor_id ==
                representation_descriptor_id) {
            for (UINT32 j = 0; j < graph->edges.size(); ++j) {
                if (graph->edges[j].result_node_id == candidate.id) {
                    *result_node_id = candidate.id;
                    *edge_id = graph->edges[j].id;
                    return TRUE;
                }
            }
            return DSL_Tensor_Evolution_Report
                       (diagnostic, "logical layout node has no edge",
                        candidate.id);
        }
    }

    DSL_TENSOR_EVOLUTION_NODE_RECORD result;
    DSL_TENSOR_EVOLUTION_EDGE_RECORD edge;
    memset(&result, 0, sizeof(result));
    result.id = graph->nodes.size() + 1;
    result.kind = DSL_TENSOR_EVOLUTION_NODE_LOGICAL_LAYOUT;
    result.owner_pu_st = graph->owner_pu_st;
    result.semantic_value_id = source.semantic_value_id;
    result.descriptor_ty = source.descriptor_ty;
    result.semantic_root_id = source.semantic_root_id;
    result.flags = DSL_TENSOR_EVOLUTION_NODE_PROVISIONAL;
    result.representation_descriptor_id = representation_descriptor_id;

    memset(&edge, 0, sizeof(edge));
    edge.id = graph->edges.size() + 1;
    edge.source_node_id = source.id;
    edge.result_node_id = result.id;
    edge.transformation_kind =
        DSL_TENSOR_EVOLUTION_TRANSFORM_LOGICAL_LAYOUT;
    edge.flags = DSL_TENSOR_EVOLUTION_EDGE_SEMANTICS_PRESERVING;

    graph->nodes.push_back(result);
    graph->edges.push_back(edge);
    if (!DSL_Tensor_Evolution_Verify(graph, diagnostic)) {
        graph->edges.pop_back();
        graph->nodes.pop_back();
        return FALSE;
    }
    *result_node_id = result.id;
    *edge_id = edge.id;
    return TRUE;
}

BOOL
DSL_Tensor_Evolution_Add_Distributed
        (DSL_TENSOR_EVOLUTION_GRAPH *graph,
         DSL_TENSOR_EVOLUTION_NODE_ID source_node_id,
         UINT32 representation_descriptor_id, UINT32 transformation_kind,
         DSL_TENSOR_EVOLUTION_NODE_ID *result_node_id,
         DSL_TENSOR_EVOLUTION_EDGE_ID *edge_id, FILE *diagnostic)
{
    if (result_node_id != NULL)
        *result_node_id = DSL_TENSOR_EVOLUTION_NODE_INVALID_ID;
    if (edge_id != NULL)
        *edge_id = DSL_TENSOR_EVOLUTION_EDGE_INVALID_ID;
    if (!DSL_Tensor_Evolution_Active(graph) || source_node_id == 0 ||
        source_node_id > graph->nodes.size() ||
        representation_descriptor_id == 0 || result_node_id == NULL ||
        edge_id == NULL ||
        (transformation_kind != DSL_TENSOR_EVOLUTION_TRANSFORM_SHARD &&
         transformation_kind != DSL_TENSOR_EVOLUTION_TRANSFORM_PLACE))
        return DSL_Tensor_Evolution_Report
                   (diagnostic, "invalid distributed input", source_node_id);

    const DSL_TENSOR_EVOLUTION_NODE_RECORD &source =
        graph->nodes[source_node_id - 1];
    if (source.kind != DSL_TENSOR_EVOLUTION_NODE_SEMANTIC)
        return DSL_Tensor_Evolution_Report
                   (diagnostic, "invalid distributed source", source_node_id);
    for (UINT32 i = 0; i < graph->nodes.size(); ++i) {
        const DSL_TENSOR_EVOLUTION_NODE_RECORD &candidate = graph->nodes[i];
        if (candidate.kind == DSL_TENSOR_EVOLUTION_NODE_DISTRIBUTED &&
            candidate.semantic_root_id == source.semantic_root_id &&
            candidate.representation_descriptor_id ==
                representation_descriptor_id) {
            for (UINT32 j = 0; j < graph->edges.size(); ++j) {
                if (graph->edges[j].result_node_id == candidate.id &&
                    graph->edges[j].transformation_kind ==
                        transformation_kind) {
                    *result_node_id = candidate.id;
                    *edge_id = graph->edges[j].id;
                    return TRUE;
                }
            }
            return DSL_Tensor_Evolution_Report
                       (diagnostic, "distributed node has no matching edge",
                        candidate.id);
        }
    }

    DSL_TENSOR_EVOLUTION_NODE_RECORD result;
    DSL_TENSOR_EVOLUTION_EDGE_RECORD edge;
    memset(&result, 0, sizeof(result));
    result.id = graph->nodes.size() + 1;
    result.kind = DSL_TENSOR_EVOLUTION_NODE_DISTRIBUTED;
    result.owner_pu_st = graph->owner_pu_st;
    result.semantic_value_id = source.semantic_value_id;
    result.descriptor_ty = source.descriptor_ty;
    result.semantic_root_id = source.semantic_root_id;
    result.flags = DSL_TENSOR_EVOLUTION_NODE_PROVISIONAL;
    result.representation_descriptor_id = representation_descriptor_id;

    memset(&edge, 0, sizeof(edge));
    edge.id = graph->edges.size() + 1;
    edge.source_node_id = source.id;
    edge.result_node_id = result.id;
    edge.transformation_kind = transformation_kind;
    edge.flags = DSL_TENSOR_EVOLUTION_EDGE_SEMANTICS_PRESERVING;

    graph->nodes.push_back(result);
    graph->edges.push_back(edge);
    if (!DSL_Tensor_Evolution_Verify(graph, diagnostic)) {
        graph->edges.pop_back();
        graph->nodes.pop_back();
        return FALSE;
    }
    *result_node_id = result.id;
    *edge_id = edge.id;
    return TRUE;
}

BOOL
DSL_Tensor_Evolution_Add_Local_Physical
        (DSL_TENSOR_EVOLUTION_GRAPH *graph,
         DSL_TENSOR_EVOLUTION_NODE_ID source_node_id,
         UINT32 representation_descriptor_id,
         DSL_TENSOR_EVOLUTION_NODE_ID *result_node_id,
         DSL_TENSOR_EVOLUTION_EDGE_ID *edge_id, FILE *diagnostic)
{
    if (result_node_id != NULL)
        *result_node_id = DSL_TENSOR_EVOLUTION_NODE_INVALID_ID;
    if (edge_id != NULL)
        *edge_id = DSL_TENSOR_EVOLUTION_EDGE_INVALID_ID;
    if (!DSL_Tensor_Evolution_Active(graph) || source_node_id == 0 ||
        source_node_id > graph->nodes.size() ||
        representation_descriptor_id == 0 || result_node_id == NULL ||
        edge_id == NULL)
        return DSL_Tensor_Evolution_Report
                   (diagnostic, "invalid local physical input",
                    source_node_id);

    const DSL_TENSOR_EVOLUTION_NODE_RECORD &source =
        graph->nodes[source_node_id - 1];
    if (source.kind != DSL_TENSOR_EVOLUTION_NODE_SEMANTIC &&
        source.kind != DSL_TENSOR_EVOLUTION_NODE_LOGICAL_LAYOUT &&
        source.kind != DSL_TENSOR_EVOLUTION_NODE_DISTRIBUTED)
        return DSL_Tensor_Evolution_Report
                   (diagnostic, "invalid local physical source",
                    source_node_id);
    for (UINT32 i = 0; i < graph->nodes.size(); ++i) {
        const DSL_TENSOR_EVOLUTION_NODE_RECORD &candidate = graph->nodes[i];
        if (candidate.kind == DSL_TENSOR_EVOLUTION_NODE_LOCAL_PHYSICAL &&
            candidate.semantic_root_id == source.semantic_root_id &&
            candidate.representation_descriptor_id ==
                representation_descriptor_id) {
            for (UINT32 j = 0; j < graph->edges.size(); ++j) {
                if (graph->edges[j].result_node_id == candidate.id &&
                    graph->edges[j].transformation_kind ==
                        DSL_TENSOR_EVOLUTION_TRANSFORM_LOCAL_LAYOUT) {
                    *result_node_id = candidate.id;
                    *edge_id = graph->edges[j].id;
                    return TRUE;
                }
            }
            return DSL_Tensor_Evolution_Report
                       (diagnostic,
                        "local physical node has no matching edge",
                        candidate.id);
        }
    }

    DSL_TENSOR_EVOLUTION_NODE_RECORD result;
    DSL_TENSOR_EVOLUTION_EDGE_RECORD edge;
    memset(&result, 0, sizeof(result));
    result.id = graph->nodes.size() + 1;
    result.kind = DSL_TENSOR_EVOLUTION_NODE_LOCAL_PHYSICAL;
    result.owner_pu_st = graph->owner_pu_st;
    result.semantic_value_id = source.semantic_value_id;
    result.descriptor_ty = source.descriptor_ty;
    result.semantic_root_id = source.semantic_root_id;
    result.flags = DSL_TENSOR_EVOLUTION_NODE_PROVISIONAL;
    result.representation_descriptor_id = representation_descriptor_id;

    memset(&edge, 0, sizeof(edge));
    edge.id = graph->edges.size() + 1;
    edge.source_node_id = source.id;
    edge.result_node_id = result.id;
    edge.transformation_kind =
        DSL_TENSOR_EVOLUTION_TRANSFORM_LOCAL_LAYOUT;
    edge.flags = DSL_TENSOR_EVOLUTION_EDGE_SEMANTICS_PRESERVING;

    graph->nodes.push_back(result);
    graph->edges.push_back(edge);
    if (!DSL_Tensor_Evolution_Verify(graph, diagnostic)) {
        graph->edges.pop_back();
        graph->nodes.pop_back();
        return FALSE;
    }
    *result_node_id = result.id;
    *edge_id = edge.id;
    return TRUE;
}

BOOL
DSL_Tensor_Evolution_Add_Tile
        (DSL_TENSOR_EVOLUTION_GRAPH *graph,
         DSL_TENSOR_EVOLUTION_NODE_ID source_node_id,
         UINT32 representation_descriptor_id,
         DSL_TENSOR_EVOLUTION_NODE_ID *result_node_id,
         DSL_TENSOR_EVOLUTION_EDGE_ID *edge_id, FILE *diagnostic)
{
    /*
     * A tile is a provisional representation of the same semantic tensor.
     * Keep its canonical TY_IDX and value identity unchanged; AIO-9 owns the
     * runtime descriptor referenced by representation_descriptor_id.
     */
    if (result_node_id != NULL)
        *result_node_id = DSL_TENSOR_EVOLUTION_NODE_INVALID_ID;
    if (edge_id != NULL)
        *edge_id = DSL_TENSOR_EVOLUTION_EDGE_INVALID_ID;
    if (!DSL_Tensor_Evolution_Active(graph) || source_node_id == 0 ||
        source_node_id > graph->nodes.size() ||
        representation_descriptor_id == 0 || result_node_id == NULL ||
        edge_id == NULL)
        return DSL_Tensor_Evolution_Report
                   (diagnostic, "invalid tile input", source_node_id);

    const DSL_TENSOR_EVOLUTION_NODE_RECORD &source =
        graph->nodes[source_node_id - 1];
    if (source.kind != DSL_TENSOR_EVOLUTION_NODE_SEMANTIC &&
        source.kind != DSL_TENSOR_EVOLUTION_NODE_LOGICAL_LAYOUT &&
        source.kind != DSL_TENSOR_EVOLUTION_NODE_DISTRIBUTED &&
        source.kind != DSL_TENSOR_EVOLUTION_NODE_LOCAL_PHYSICAL)
        return DSL_Tensor_Evolution_Report
                   (diagnostic, "invalid tile source", source_node_id);
    for (UINT32 i = 0; i < graph->nodes.size(); ++i) {
        const DSL_TENSOR_EVOLUTION_NODE_RECORD &candidate = graph->nodes[i];
        if (candidate.kind == DSL_TENSOR_EVOLUTION_NODE_TILE &&
            candidate.semantic_root_id == source.semantic_root_id &&
            candidate.representation_descriptor_id ==
                representation_descriptor_id) {
            for (UINT32 j = 0; j < graph->edges.size(); ++j) {
                if (graph->edges[j].result_node_id == candidate.id &&
                    graph->edges[j].transformation_kind ==
                        DSL_TENSOR_EVOLUTION_TRANSFORM_TILE) {
                    *result_node_id = candidate.id;
                    *edge_id = graph->edges[j].id;
                    return TRUE;
                }
            }
            return DSL_Tensor_Evolution_Report
                       (diagnostic, "tile node has no matching edge",
                        candidate.id);
        }
    }

    DSL_TENSOR_EVOLUTION_NODE_RECORD result;
    DSL_TENSOR_EVOLUTION_EDGE_RECORD edge;
    memset(&result, 0, sizeof(result));
    result.id = graph->nodes.size() + 1;
    result.kind = DSL_TENSOR_EVOLUTION_NODE_TILE;
    result.owner_pu_st = graph->owner_pu_st;
    result.semantic_value_id = source.semantic_value_id;
    result.descriptor_ty = source.descriptor_ty;
    result.semantic_root_id = source.semantic_root_id;
    result.flags = DSL_TENSOR_EVOLUTION_NODE_PROVISIONAL;
    result.representation_descriptor_id = representation_descriptor_id;

    memset(&edge, 0, sizeof(edge));
    edge.id = graph->edges.size() + 1;
    edge.source_node_id = source.id;
    edge.result_node_id = result.id;
    edge.transformation_kind = DSL_TENSOR_EVOLUTION_TRANSFORM_TILE;
    edge.flags = DSL_TENSOR_EVOLUTION_EDGE_SEMANTICS_PRESERVING;

    graph->nodes.push_back(result);
    graph->edges.push_back(edge);
    if (!DSL_Tensor_Evolution_Verify(graph, diagnostic)) {
        graph->edges.pop_back();
        graph->nodes.pop_back();
        return FALSE;
    }
    *result_node_id = result.id;
    *edge_id = edge.id;
    return TRUE;
}

BOOL
DSL_Tensor_Evolution_Verify
        (const DSL_TENSOR_EVOLUTION_GRAPH *graph, FILE *diagnostic)
{
    if (!DSL_Tensor_Evolution_Active(graph))
        return DSL_Tensor_Evolution_Report
                   (diagnostic, "program unit is not active", 0);
    for (UINT32 i = 0; i < graph->nodes.size(); ++i) {
        const DSL_TENSOR_EVOLUTION_NODE_RECORD &node = graph->nodes[i];
        DSL_IR_VALUE_RECORD value;
        if (node.id != i + 1 || node.owner_pu_st != graph->owner_pu_st ||
            node.reserved != 0 ||
            !DSL_Tensor_Evolution_Descriptor_Valid(node.descriptor_ty) ||
            !DSL_IR_Image_Get_Value(node.semantic_value_id, &value) ||
            (value.flags & DSL_IR_VALUE_FLAG_REDIRECTED) != 0 ||
            value.ty != node.descriptor_ty ||
            !DSL_Tensor_Evolution_Value_Owned(graph, value))
            return DSL_Tensor_Evolution_Report
                       (diagnostic, "invalid evolution node", node.id);
        if (node.kind == DSL_TENSOR_EVOLUTION_NODE_SEMANTIC) {
            if (node.semantic_root_id != node.id || node.flags != 0 ||
                node.representation_descriptor_id != 0)
                return DSL_Tensor_Evolution_Report
                           (diagnostic, "invalid semantic root", node.id);
            for (UINT32 j = 0; j < i; ++j) {
                if (graph->nodes[j].kind ==
                        DSL_TENSOR_EVOLUTION_NODE_SEMANTIC &&
                    graph->nodes[j].semantic_value_id ==
                        node.semantic_value_id)
                    return DSL_Tensor_Evolution_Report
                               (diagnostic, "duplicate semantic root",
                                node.id);
            }
        } else if (node.kind == DSL_TENSOR_EVOLUTION_NODE_LOGICAL_LAYOUT ||
                   node.kind == DSL_TENSOR_EVOLUTION_NODE_DISTRIBUTED ||
                   node.kind == DSL_TENSOR_EVOLUTION_NODE_LOCAL_PHYSICAL ||
                   node.kind == DSL_TENSOR_EVOLUTION_NODE_TILE) {
            if (node.semantic_root_id == 0 ||
                node.semantic_root_id >= node.id ||
                node.semantic_root_id > graph->nodes.size() ||
                graph->nodes[node.semantic_root_id - 1].kind !=
                    DSL_TENSOR_EVOLUTION_NODE_SEMANTIC ||
                graph->nodes[node.semantic_root_id - 1].semantic_value_id !=
                    node.semantic_value_id ||
                node.flags != DSL_TENSOR_EVOLUTION_NODE_PROVISIONAL ||
                node.representation_descriptor_id == 0)
                return DSL_Tensor_Evolution_Report
                           (diagnostic, "invalid derived evolution node",
                            node.id);
            for (UINT32 j = 0; j < i; ++j) {
                if (graph->nodes[j].kind == node.kind &&
                    graph->nodes[j].semantic_root_id ==
                        node.semantic_root_id &&
                    graph->nodes[j].representation_descriptor_id ==
                        node.representation_descriptor_id)
                    return DSL_Tensor_Evolution_Report
                               (diagnostic, "duplicate derived evolution node",
                                node.id);
            }
        } else {
            return DSL_Tensor_Evolution_Report
                       (diagnostic, "unsupported evolution node", node.id);
        }
    }
    std::vector<UINT32> incoming(graph->nodes.size(), 0);
    for (UINT32 i = 0; i < graph->edges.size(); ++i) {
        const DSL_TENSOR_EVOLUTION_EDGE_RECORD &edge = graph->edges[i];
        if (edge.id != i + 1 || edge.source_node_id == 0 ||
            edge.result_node_id == 0 ||
            edge.source_node_id >= edge.result_node_id ||
            edge.result_node_id > graph->nodes.size() ||
            (edge.transformation_kind !=
                 DSL_TENSOR_EVOLUTION_TRANSFORM_LOGICAL_LAYOUT &&
             edge.transformation_kind !=
                 DSL_TENSOR_EVOLUTION_TRANSFORM_SHARD &&
             edge.transformation_kind !=
                 DSL_TENSOR_EVOLUTION_TRANSFORM_PLACE &&
             edge.transformation_kind !=
                 DSL_TENSOR_EVOLUTION_TRANSFORM_LOCAL_LAYOUT &&
             edge.transformation_kind !=
                 DSL_TENSOR_EVOLUTION_TRANSFORM_TILE) ||
            edge.flags !=
                DSL_TENSOR_EVOLUTION_EDGE_SEMANTICS_PRESERVING ||
            edge.reserved0 != 0 || edge.reserved1 != 0 ||
            edge.reserved2 != 0)
            return DSL_Tensor_Evolution_Report
                       (diagnostic, "invalid evolution edge", edge.id);
        const DSL_TENSOR_EVOLUTION_NODE_RECORD &source =
            graph->nodes[edge.source_node_id - 1];
        const DSL_TENSOR_EVOLUTION_NODE_RECORD &result =
            graph->nodes[edge.result_node_id - 1];
        if (((result.kind == DSL_TENSOR_EVOLUTION_NODE_LOGICAL_LAYOUT) !=
             (edge.transformation_kind ==
                  DSL_TENSOR_EVOLUTION_TRANSFORM_LOGICAL_LAYOUT)) ||
            ((result.kind == DSL_TENSOR_EVOLUTION_NODE_DISTRIBUTED) !=
             (edge.transformation_kind ==
                  DSL_TENSOR_EVOLUTION_TRANSFORM_SHARD ||
              edge.transformation_kind ==
                  DSL_TENSOR_EVOLUTION_TRANSFORM_PLACE)) ||
            ((result.kind == DSL_TENSOR_EVOLUTION_NODE_LOCAL_PHYSICAL) !=
             (edge.transformation_kind ==
                  DSL_TENSOR_EVOLUTION_TRANSFORM_LOCAL_LAYOUT)) ||
            ((result.kind == DSL_TENSOR_EVOLUTION_NODE_TILE) !=
             (edge.transformation_kind ==
                  DSL_TENSOR_EVOLUTION_TRANSFORM_TILE)) ||
            source.semantic_root_id != result.semantic_root_id ||
            source.semantic_value_id != result.semantic_value_id ||
            source.descriptor_ty != result.descriptor_ty ||
            ++incoming[edge.result_node_id - 1] != 1)
            return DSL_Tensor_Evolution_Report
                       (diagnostic, "inconsistent evolution edge", edge.id);
    }
    for (UINT32 i = 0; i < graph->nodes.size(); ++i) {
        if ((graph->nodes[i].kind ==
                 DSL_TENSOR_EVOLUTION_NODE_SEMANTIC && incoming[i] != 0) ||
            ((graph->nodes[i].kind ==
                  DSL_TENSOR_EVOLUTION_NODE_LOGICAL_LAYOUT ||
              graph->nodes[i].kind ==
                  DSL_TENSOR_EVOLUTION_NODE_DISTRIBUTED ||
              graph->nodes[i].kind ==
                  DSL_TENSOR_EVOLUTION_NODE_LOCAL_PHYSICAL ||
              graph->nodes[i].kind ==
                  DSL_TENSOR_EVOLUTION_NODE_TILE) &&
             incoming[i] != 1))
            return DSL_Tensor_Evolution_Report
                       (diagnostic, "invalid incoming edge count", i + 1);
    }
    return TRUE;
}

void
DSL_Tensor_Evolution_Print
        (FILE *file, const DSL_TENSOR_EVOLUTION_GRAPH *graph)
{
    if (file == NULL || graph == NULL)
        return;
    fprintf(file,
            "TensorEvolutionGraph: owner=<%u,%u,%s> nodes=%u edges=%u\n",
            ST_IDX_level(graph->owner_pu_st),
            ST_IDX_index(graph->owner_pu_st),
            DSL_Tensor_Evolution_Owner_Valid(graph->owner_pu_st) ?
                ST_name(St_Table[graph->owner_pu_st]) : "<invalid>",
            (UINT32)graph->nodes.size(), (UINT32)graph->edges.size());
    for (UINT32 i = 0; i < graph->nodes.size(); ++i) {
        const DSL_TENSOR_EVOLUTION_NODE_RECORD &node = graph->nodes[i];
        DSL_IR_VALUE_RECORD value;
        const char *value_name = "<invalid>";
        const char *dtype = "<pending>";
        const char *shape = "<pending>";
        if (DSL_IR_Image_Get_Value(node.semantic_value_id, &value) &&
            value.name != STR_IDX_ZERO)
            value_name = Index_To_Str(value.name);
        if (DSL_Tensor_Evolution_Descriptor_Valid(node.descriptor_ty)) {
            const char *candidate = TY_tensor_attribute
                                        (node.descriptor_ty,
                                         TY_TENSOR_SCHEMA_DTYPE);
            if (candidate != NULL)
                dtype = candidate;
            candidate = TY_tensor_attribute
                            (node.descriptor_ty, TY_TENSOR_SCHEMA_SHAPE);
            if (candidate != NULL)
                shape = candidate;
        }
        fprintf(file,
                "  [%u] kind=%s value=%u name=%s ty=%u "
                "dtype=%s shape=%s semantic_root=%u representation=%u "
                "flags=0x%x\n",
                node.id, DSL_Tensor_Evolution_Node_Kind_Name(node.kind),
                node.semantic_value_id, value_name,
                TY_IDX_index(node.descriptor_ty), dtype, shape,
                node.semantic_root_id, node.representation_descriptor_id,
                node.flags);
    }
    for (UINT32 i = 0; i < graph->edges.size(); ++i) {
        const DSL_TENSOR_EVOLUTION_EDGE_RECORD &edge = graph->edges[i];
        fprintf(file,
                "  edge[%u] source=%u result=%u transform=%s flags=0x%x\n",
                edge.id, edge.source_node_id, edge.result_node_id,
                DSL_Tensor_Evolution_Transform_Kind_Name
                    (edge.transformation_kind), edge.flags);
    }
}

ST_IDX
DSL_Tensor_Evolution_Owner (const DSL_TENSOR_EVOLUTION_GRAPH *graph)
{
    return graph == NULL ? ST_IDX_ZERO : graph->owner_pu_st;
}

UINT32
DSL_Tensor_Evolution_Node_Count (const DSL_TENSOR_EVOLUTION_GRAPH *graph)
{
    return graph == NULL ? 0 : graph->nodes.size();
}

UINT32
DSL_Tensor_Evolution_Edge_Count (const DSL_TENSOR_EVOLUTION_GRAPH *graph)
{
    return graph == NULL ? 0 : graph->edges.size();
}

BOOL
DSL_Tensor_Evolution_Get_Node
        (const DSL_TENSOR_EVOLUTION_GRAPH *graph,
         DSL_TENSOR_EVOLUTION_NODE_ID id,
         DSL_TENSOR_EVOLUTION_NODE_RECORD *record)
{
    if (graph == NULL || record == NULL || id == 0 ||
        id > graph->nodes.size())
        return FALSE;
    *record = graph->nodes[id - 1];
    return TRUE;
}

BOOL
DSL_Tensor_Evolution_Get_Edge
        (const DSL_TENSOR_EVOLUTION_GRAPH *graph,
         DSL_TENSOR_EVOLUTION_EDGE_ID id,
         DSL_TENSOR_EVOLUTION_EDGE_RECORD *record)
{
    if (graph == NULL || record == NULL || id == 0 ||
        id > graph->edges.size())
        return FALSE;
    *record = graph->edges[id - 1];
    return TRUE;
}
