/*
 * Copyright (C) 2026 Open64 Project
 */

/*
 * VHO runtime storage, construction and structural checks for one active PU's
 * tensor evolution graph. Borrow driver context; never switch PU or mutate
 * WN/ST/TY, mapped-image sections, .B layout or representation-selection policy.
 * Ownership: doc/DSL-BACKEND-OWNERSHIP-RELOCATION.md.
 * Graph semantics: doc/AI-COMPILER-OPTIMIZATION-AIO1-TENSOR-EVOLUTION.md.
 */

#include <string.h>
#include "../../common/ir_image/dsl_image_service.h"
#include <vector>

#include "dsl_tensor_evolution.h"
#include "pu_info.h"
#include "strtab.h"

struct DSL_TENSOR_EVOLUTION_GRAPH {
    PU_Info *pu;
    ST_IDX owner_pu_st;
    std::vector<DSL_TENSOR_EVOLUTION_NODE_RECORD> nodes;
    std::vector<DSL_TENSOR_EVOLUTION_EDGE_RECORD> edges;
};

static const char *DSL_tensor_evolution_node_kind_name_table[] = {
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

static const char *DSL_tensor_evolution_transform_kind_name_table[] = {
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

/*
 * Emit the graph diagnostic only when a stream is supplied and return FALSE.
 * The caller retains stream ownership; no graph or compiler state is changed.
 */
static BOOL
DSL_Tensor_Evolution_Report
        (FILE *diagnostic, const char *message, UINT32 id)
{
    if (diagnostic != NULL)
        fprintf(diagnostic, "TensorEvolutionGraph error: %s id=%u\n",
                message, id);
    return FALSE;
}

/*
 * Check that an owner ID names an in-range global function with a valid PU
 * entry before callers dereference that entry; read tables without changing PU.
 */
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

/*
 * Require the graph's borrowed PU, function owner and Current_pu to match the
 * driver's active context. Return FALSE on mismatch; never restore or switch PU.
 */
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

/*
 * Within an active graph, match the value's local ST/type/name and managed
 * PU-value lookup to its exact ID. Borrow all tables/strings and reject mismatches
 * without changing graph, symbols or image records.
 */
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

/*
 * Recognize a nonzero canonical tensor-extension TY for root/graph checks.
 * Read existing type state only; do not canonicalize or change the descriptor.
 */
static BOOL
DSL_Tensor_Evolution_Descriptor_Valid (TY_IDX ty)
{
    return TY_IDX_index(ty) != 0 && TY_is_tensor_extension(ty) &&
           TY_tensor_is_canonical(ty);
}

/*
 * Return a borrowed static node-kind spelling, or "unknown" for out-of-range
 * input; no graph, PU context, allocation or mutation is involved.
 */
const char *
DSL_tensor_evolution_node_kind_name (UINT32 kind)
{
    return kind < sizeof(DSL_tensor_evolution_node_kind_name_table) /
                      sizeof(DSL_tensor_evolution_node_kind_name_table[0]) ?
           DSL_tensor_evolution_node_kind_name_table[kind] : "unknown";
}

/*
 * Return a borrowed static transformation spelling, or "unknown" for
 * out-of-range input; no PU context, allocation or mutation is involved.
 */
const char *
DSL_tensor_evolution_transform_kind_name (UINT32 kind)
{
    return kind < sizeof(DSL_tensor_evolution_transform_kind_name_table) /
                      sizeof(DSL_tensor_evolution_transform_kind_name_table[0]) ?
           DSL_tensor_evolution_transform_kind_name_table[kind] : "unknown";
}

/*
 * Check Current_PU_Info, the global function symbol and Current_pu before
 * allocating empty graph vectors. Capture only the borrowed PU and owner ID;
 * rejected contexts allocate nothing and leave driver state unchanged.
 */
DSL_TENSOR_EVOLUTION_GRAPH *
DSL_tensor_evolution_create (PU_Info *pu, FILE *diagnostic)
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

/*
 * Delete the graph and its runtime vectors only; accept NULL and do not release
 * borrowed PU, symbols, types or managed-image records.
 */
void
DSL_tensor_evolution_destroy (DSL_TENSOR_EVOLUTION_GRAPH *graph)
{
    delete graph;
}

/*
 * Scan graph-owned nodes for the first semantic/value match, optionally copying
 * it. Reject NULL graph or invalid value ID; do not revalidate PU ownership or
 * change the output on a miss.
 */
BOOL
DSL_tensor_evolution_find_semantic_root
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

/*
 * Reset the optional output, resolve the local ST/name/owner identity and
 * canonical TY, then reuse a matching root or append a zeroed semantic record.
 * Reject redirected values and conflicting types before insertion; this path
 * does not post-verify the graph or catch allocation failures.
 */
BOOL
DSL_tensor_evolution_add_semantic_root
        (DSL_TENSOR_EVOLUTION_GRAPH *graph, DSL_IR_VALUE_ID value_id,
         TY_IDX descriptor_ty, DSL_TENSOR_EVOLUTION_NODE_ID *root_id,
         FILE *diagnostic)
{
    DSL_IR_VALUE_RECORD value;
    DSL_TENSOR_EVOLUTION_NODE_RECORD root;

    if (root_id != NULL)
        *root_id = DSL_TENSOR_EVOLUTION_NODE_INVALID_ID;
    if (!DSL_Tensor_Evolution_Active(graph) ||
        value_id == DSL_IR_VALUE_INVALID_ID ||
        !DSL_IR_Image_Get_Value(value_id, &value) ||
        !DSL_Tensor_Evolution_Value_Owned(graph, value) ||
        value.ty != descriptor_ty ||
        !DSL_Tensor_Evolution_Descriptor_Valid(descriptor_ty))
        return DSL_Tensor_Evolution_Report
                   (diagnostic, "invalid semantic root input", value_id);
    if ((value.flags & DSL_IR_VALUE_FLAG_REDIRECTED) != 0)
        return DSL_Tensor_Evolution_Report
                   (diagnostic, "redirected value is not a root", value_id);

    if (DSL_tensor_evolution_find_semantic_root(graph, value_id, &root)) {
        if (root.descriptor_ty != descriptor_ty)
            return DSL_Tensor_Evolution_Report
                       (diagnostic, "semantic root changed type", value_id);
        if (root_id != NULL)
            *root_id = root.id;
        return TRUE;
    }

    memset(&root, 0, sizeof(root));
    root.id = graph->nodes.size() + 1;
    root.kind = DSL_TENSOR_EVOLUTION_NODE_SEMANTIC;
    root.owner_pu_st = graph->owner_pu_st;
    root.semantic_value_id = value.id;
    root.descriptor_ty = value.ty;
    root.semantic_root_id = root.id;
    /* Explicit roots are interned by value; insertion does not rewrite the image. */
    graph->nodes.push_back(root);
    if (root_id != NULL)
        *root_id = root.id;
    return TRUE;
}

/*
 * Require active ownership, a semantic source and both reset outputs. Reuse a
 * matching root/representation node with an incoming edge; otherwise append
 * contiguous provisional node/edge IDs, preserve value/TY, verify, and pop that
 * pair on rejection. Do not validate the external representation descriptor.
 */
BOOL
DSL_tensor_evolution_add_logical_layout
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

    /* Publish a provisional pair; failed structural verification removes only it. */
    graph->nodes.push_back(result);
    graph->edges.push_back(edge);
    if (!DSL_tensor_evolution_verify(graph, diagnostic)) {
        graph->edges.pop_back();
        graph->nodes.pop_back();
        return FALSE;
    }
    *result_node_id = result.id;
    *edge_id = edge.id;
    return TRUE;
}

/*
 * Preflight active ownership, semantic source, SHARD/PLACE and reset outputs.
 * Reuse by root/representation only when its edge has the requested transform;
 * a mismatching existing node fails rather than appending. Otherwise append,
 * verify, and pop the new pair on rejection without changing semantic value/TY.
 */
BOOL
DSL_tensor_evolution_add_distributed
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

    /* Publish a provisional pair; failed structural verification removes only it. */
    graph->nodes.push_back(result);
    graph->edges.push_back(edge);
    if (!DSL_tensor_evolution_verify(graph, diagnostic)) {
        graph->edges.pop_back();
        graph->nodes.pop_back();
        return FALSE;
    }
    *result_node_id = result.id;
    *edge_id = edge.id;
    return TRUE;
}

/*
 * Accept semantic, logical-layout or distributed sources in the active graph.
 * Reuse the root/representation descendant with a local-layout edge, even when
 * that edge originates at another equivalent source. Otherwise append a
 * provisional pair, preserve value/TY, verify and pop the pair on rejection.
 */
BOOL
DSL_tensor_evolution_add_local_physical
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

    /* Publish a provisional pair; failed structural verification removes only it. */
    graph->nodes.push_back(result);
    graph->edges.push_back(edge);
    if (!DSL_tensor_evolution_verify(graph, diagnostic)) {
        graph->edges.pop_back();
        graph->nodes.pop_back();
        return FALSE;
    }
    *result_node_id = result.id;
    *edge_id = edge.id;
    return TRUE;
}

/*
 * Accept supported sources through local-physical form; reuse by semantic root
 * and tile representation with a TILE edge, without requiring the same source.
 * Otherwise append contiguous provisional IDs preserving value/TY, verify and
 * pop the new pair on rejection; leave tile policy to the VHO producer.
 */
BOOL
DSL_tensor_evolution_add_tile
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

    /* Publish a provisional pair; failed structural verification removes only it. */
    graph->nodes.push_back(result);
    graph->edges.push_back(edge);
    if (!DSL_tensor_evolution_verify(graph, diagnostic)) {
        graph->edges.pop_back();
        graph->nodes.pop_back();
        return FALSE;
    }
    *result_node_id = result.id;
    *edge_id = edge.id;
    return TRUE;
}

/*
 * Accept supported sources through tile form; reuse by root/representation
 * with a STAGE_BUFFER edge, without requiring the same source. Otherwise append
 * contiguous provisional IDs preserving value/TY, verify and pop the new pair
 * on rejection; do not select movement or fetch policy.
 */
BOOL
DSL_tensor_evolution_add_staged_buffer
        (DSL_TENSOR_EVOLUTION_GRAPH *graph,
         DSL_TENSOR_EVOLUTION_NODE_ID source_node_id,
         UINT32 representation_descriptor_id,
         DSL_TENSOR_EVOLUTION_NODE_ID *result_node_id,
         DSL_TENSOR_EVOLUTION_EDGE_ID *edge_id, FILE *diagnostic)
{
    /*
     * A staged buffer is a provisional movement representation of one
     * semantic tensor. AIO-10 owns the runtime descriptor; canonical TY_IDX
     * and DSL value identity stay unchanged.
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
                   (diagnostic, "invalid staged-buffer input",
                    source_node_id);

    const DSL_TENSOR_EVOLUTION_NODE_RECORD &source =
        graph->nodes[source_node_id - 1];
    if (source.kind != DSL_TENSOR_EVOLUTION_NODE_SEMANTIC &&
        source.kind != DSL_TENSOR_EVOLUTION_NODE_LOGICAL_LAYOUT &&
        source.kind != DSL_TENSOR_EVOLUTION_NODE_DISTRIBUTED &&
        source.kind != DSL_TENSOR_EVOLUTION_NODE_LOCAL_PHYSICAL &&
        source.kind != DSL_TENSOR_EVOLUTION_NODE_TILE)
        return DSL_Tensor_Evolution_Report
                   (diagnostic, "invalid staged-buffer source",
                    source_node_id);
    for (UINT32 i = 0; i < graph->nodes.size(); ++i) {
        const DSL_TENSOR_EVOLUTION_NODE_RECORD &candidate = graph->nodes[i];
        if (candidate.kind == DSL_TENSOR_EVOLUTION_NODE_STAGED_BUFFER &&
            candidate.semantic_root_id == source.semantic_root_id &&
            candidate.representation_descriptor_id ==
                representation_descriptor_id) {
            for (UINT32 j = 0; j < graph->edges.size(); ++j) {
                if (graph->edges[j].result_node_id == candidate.id &&
                    graph->edges[j].transformation_kind ==
                        DSL_TENSOR_EVOLUTION_TRANSFORM_STAGE_BUFFER) {
                    *result_node_id = candidate.id;
                    *edge_id = graph->edges[j].id;
                    return TRUE;
                }
            }
            return DSL_Tensor_Evolution_Report
                       (diagnostic,
                        "staged-buffer node has no matching edge",
                        candidate.id);
        }
    }

    DSL_TENSOR_EVOLUTION_NODE_RECORD result;
    DSL_TENSOR_EVOLUTION_EDGE_RECORD edge;
    memset(&result, 0, sizeof(result));
    result.id = graph->nodes.size() + 1;
    result.kind = DSL_TENSOR_EVOLUTION_NODE_STAGED_BUFFER;
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
        DSL_TENSOR_EVOLUTION_TRANSFORM_STAGE_BUFFER;
    edge.flags = DSL_TENSOR_EVOLUTION_EDGE_SEMANTICS_PRESERVING;

    /* Publish a provisional pair; failed structural verification removes only it. */
    graph->nodes.push_back(result);
    graph->edges.push_back(edge);
    if (!DSL_tensor_evolution_verify(graph, diagnostic)) {
        graph->edges.pop_back();
        graph->nodes.pop_back();
        return FALSE;
    }
    *result_node_id = result.id;
    *edge_id = edge.id;
    return TRUE;
}

/*
 * Validate active PU identity, contiguous nodes/edges, unique semantic and
 * derived keys, canonical value/TY joins and supported transform/result kinds.
 * Count incoming edges in temporary storage to require zero for roots and one
 * for derived nodes; diagnose and return FALSE without repairing the graph.
 */
BOOL
DSL_tensor_evolution_verify
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
                   node.kind == DSL_TENSOR_EVOLUTION_NODE_TILE ||
                   node.kind == DSL_TENSOR_EVOLUTION_NODE_STAGED_BUFFER) {
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
    /* Append-order edges must preserve one root identity and one parent per result. */
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
                 DSL_TENSOR_EVOLUTION_TRANSFORM_TILE &&
             edge.transformation_kind !=
                 DSL_TENSOR_EVOLUTION_TRANSFORM_STAGE_BUFFER) ||
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
            ((result.kind == DSL_TENSOR_EVOLUTION_NODE_STAGED_BUFFER) !=
             (edge.transformation_kind ==
                  DSL_TENSOR_EVOLUTION_TRANSFORM_STAGE_BUFFER)) ||
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
                  DSL_TENSOR_EVOLUTION_NODE_TILE ||
              graph->nodes[i].kind ==
                  DSL_TENSOR_EVOLUTION_NODE_STAGED_BUFFER) &&
             incoming[i] != 1))
            return DSL_Tensor_Evolution_Report
                       (diagnostic, "invalid incoming edge count", i + 1);
    }
    return TRUE;
}

/*
 * Print stored node/edge order and resolve available owner/value/type labels
 * from live borrowed tables, using existing invalid/pending fallbacks. Do not
 * activate a PU or verify the graph; NULL inputs produce no output.
 */
void
DSL_tensor_evolution_print
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
                node.id, DSL_tensor_evolution_node_kind_name(node.kind),
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
                DSL_tensor_evolution_transform_kind_name
                    (edge.transformation_kind), edge.flags);
    }
}

/*
 * Read the captured owner ID without PU validation; NULL yields ST_IDX_ZERO.
 * The returned ID borrows its meaning from the still-live symbol tables.
 */
ST_IDX
DSL_tensor_evolution_owner (const DSL_TENSOR_EVOLUTION_GRAPH *graph)
{
    return graph == NULL ? ST_IDX_ZERO : graph->owner_pu_st;
}

/*
 * Read the stored node count without ownership validation or mutation; NULL
 * yields zero. IDs remain graph-local, not managed-image indices.
 */
UINT32
DSL_tensor_evolution_node_count (const DSL_TENSOR_EVOLUTION_GRAPH *graph)
{
    return graph == NULL ? 0 : graph->nodes.size();
}

/*
 * Read the stored edge count without ownership validation or mutation; NULL
 * yields zero. This does not verify edge consistency.
 */
UINT32
DSL_tensor_evolution_edge_count (const DSL_TENSOR_EVOLUTION_GRAPH *graph)
{
    return graph == NULL ? 0 : graph->edges.size();
}

/*
 * Copy a stored node by one-based graph-local ID without revalidating the PU.
 * Require non-NULL graph/output and an in-range ID; failure preserves output.
 */
BOOL
DSL_tensor_evolution_get_node
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

/*
 * Copy a stored edge by one-based graph-local ID without revalidating the PU.
 * Require non-NULL graph/output and an in-range ID; failure preserves output.
 */
BOOL
DSL_tensor_evolution_get_edge
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
