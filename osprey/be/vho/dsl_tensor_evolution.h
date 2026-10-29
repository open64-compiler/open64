/*
 * Copyright (C) 2026 Open64 Project
 */

/*
 * VHO-owned AIO-1 contract for PU-local tensor representation evolution.
 * The caller owns graph lifetime and keeps borrowed PU/tables/image live;
 * explicit roots and provisional descendants add no .B rows or layout changes.
 * Ownership: doc/DSL-BACKEND-OWNERSHIP-RELOCATION.md.
 * Graph semantics: doc/AI-COMPILER-OPTIMIZATION-AIO1-TENSOR-EVOLUTION.md.
 */

#ifndef dsl_tensor_evolution_INCLUDED
#define dsl_tensor_evolution_INCLUDED

#include <stdio.h>

#include "defs.h"
#include "dsl_ir_image.h"
#include "symtab.h"

struct pu_info;
struct DSL_TENSOR_EVOLUTION_GRAPH;

typedef struct DSL_TENSOR_EVOLUTION_GRAPH DSL_TENSOR_EVOLUTION_GRAPH;
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

/*
 * Create a caller-owned empty VHO graph for the driver's active PU; borrow PU,
 * its tables and image identities until destruction. Return NULL with an optional
 * diagnostic on PU/context mismatch; never switch PU or mutate WHIRL.
 */
extern DSL_TENSOR_EVOLUTION_GRAPH *DSL_tensor_evolution_create
                                (struct pu_info *pu, FILE *diagnostic);
/*
 * Release graph-owned vectors and the handle (NULL is allowed). Callers must
 * first destroy analyses/plans borrowing this graph; PU, tables and WHIRL survive.
 */
extern void DSL_tensor_evolution_destroy
                                (DSL_TENSOR_EVOLUTION_GRAPH *graph);
/*
 * In the active owning PU, reuse or append a root for a live, non-redirected,
 * PU-owned value with the same canonical tensor TY. Reset optional root_id to
 * invalid before validation; preflight failure preserves graph records. A new ID
 * is node_count + 1; no whole-graph verification or allocation rollback is done.
 */
extern BOOL DSL_tensor_evolution_add_semantic_root
                                (DSL_TENSOR_EVOLUTION_GRAPH *graph,
                                 DSL_IR_VALUE_ID value_id,
                                 TY_IDX descriptor_ty,
                                 DSL_TENSOR_EVOLUTION_NODE_ID *root_id,
                                 FILE *diagnostic);
/*
 * For an active graph and semantic source, reuse a logical-layout descendant
 * keyed by root/kind/nonzero opaque representation ID, or append one node/edge.
 * Both ID outputs are required and reset to invalid. Verification failure pops
 * only the new pair; descriptor existence/legality and allocation rollback are
 * outside this structural service.
 */
extern BOOL DSL_tensor_evolution_add_logical_layout
                                (DSL_TENSOR_EVOLUTION_GRAPH *graph,
                                 DSL_TENSOR_EVOLUTION_NODE_ID source_node_id,
                                 UINT32 representation_descriptor_id,
                                 DSL_TENSOR_EVOLUTION_NODE_ID *result_node_id,
                                 DSL_TENSOR_EVOLUTION_EDGE_ID *edge_id,
                                 FILE *diagnostic);
/*
 * For an active graph and semantic source, reuse or append a distributed node
 * for a nonzero opaque representation ID and SHARD or PLACE transform. Require
 * and reset both outputs; reuse also requires a matching transform edge. New
 * pair verification failure removes that pair only; external descriptor legality
 * and allocation rollback remain the caller's responsibility.
 */
extern BOOL DSL_tensor_evolution_add_distributed
                                (DSL_TENSOR_EVOLUTION_GRAPH *graph,
                                 DSL_TENSOR_EVOLUTION_NODE_ID source_node_id,
                                 UINT32 representation_descriptor_id,
                                 UINT32 transformation_kind,
                                 DSL_TENSOR_EVOLUTION_NODE_ID *result_node_id,
                                 DSL_TENSOR_EVOLUTION_EDGE_ID *edge_id,
                                 FILE *diagnostic);
/*
 * For an active graph, derive local-physical form from semantic, logical-layout
 * or distributed input using a nonzero opaque representation ID. Require/reset
 * both outputs; reuse by root/kind/representation, or append contiguous IDs and
 * remove only that pair on verification failure. No external descriptor legality
 * or allocation rollback is provided.
 */
extern BOOL DSL_tensor_evolution_add_local_physical
                                (DSL_TENSOR_EVOLUTION_GRAPH *graph,
                                 DSL_TENSOR_EVOLUTION_NODE_ID source_node_id,
                                 UINT32 representation_descriptor_id,
                                 DSL_TENSOR_EVOLUTION_NODE_ID *result_node_id,
                                 DSL_TENSOR_EVOLUTION_EDGE_ID *edge_id,
                                 FILE *diagnostic);
/*
 * For an active graph, derive tile form from semantic, logical-layout,
 * distributed or local-physical input with a nonzero opaque representation ID.
 * Require/reset both outputs; reuse by root/kind/representation or append and
 * verify a pair. Verification rejection removes the pair only; allocation and
 * external tile-descriptor legality are not transactional here.
 */
extern BOOL DSL_tensor_evolution_add_tile
                                (DSL_TENSOR_EVOLUTION_GRAPH *graph,
                                 DSL_TENSOR_EVOLUTION_NODE_ID source_node_id,
                                 UINT32 representation_descriptor_id,
                                 DSL_TENSOR_EVOLUTION_NODE_ID *result_node_id,
                                 DSL_TENSOR_EVOLUTION_EDGE_ID *edge_id,
                                 FILE *diagnostic);
/*
 * For an active graph, derive staged-buffer form from semantic through tile
 * input with a nonzero opaque representation ID. Require/reset both outputs;
 * reuse by root/kind/representation or append and verify a pair. Only explicit
 * verification rejection rolls back the pair; external movement legality and
 * allocation failure handling remain outside this service.
 */
extern BOOL DSL_tensor_evolution_add_staged_buffer
                                (DSL_TENSOR_EVOLUTION_GRAPH *graph,
                                 DSL_TENSOR_EVOLUTION_NODE_ID source_node_id,
                                 UINT32 representation_descriptor_id,
                                 DSL_TENSOR_EVOLUTION_NODE_ID *result_node_id,
                                 DSL_TENSOR_EVOLUTION_EDGE_ID *edge_id,
                                 FILE *diagnostic);
/*
 * Read-only structural verification requires the driver to keep this graph's
 * PU/tables/image active. Check stored IDs, ownership, live canonical values,
 * root/derived identity and edge consistency; diagnose the first failure. Opaque
 * representation IDs are not a proof of target legality or profitability.
 */
extern BOOL DSL_tensor_evolution_verify
                                (const DSL_TENSOR_EVOLUTION_GRAPH *graph,
                                 FILE *diagnostic);
/*
 * Write runtime graph evidence using borrowed tables and image strings; caller
 * keeps them live and structurally usable. NULL file/graph is a no-op. This does
 * not verify active ownership, mutate records or publish a .B image.
 */
extern void DSL_tensor_evolution_print
                                (FILE *file,
                                 const DSL_TENSOR_EVOLUTION_GRAPH *graph);
/*
 * Read the captured owner ID without PU validation; NULL yields ST_IDX_ZERO.
 * The returned ID borrows its meaning from the still-live symbol tables.
 */
extern ST_IDX DSL_tensor_evolution_owner
                                (const DSL_TENSOR_EVOLUTION_GRAPH *graph);
/*
 * Read the stored node count without ownership validation or mutation; NULL
 * yields zero. IDs remain graph-local, not managed-image indices.
 */
extern UINT32 DSL_tensor_evolution_node_count
                                (const DSL_TENSOR_EVOLUTION_GRAPH *graph);
/*
 * Read the stored edge count without ownership validation or mutation; NULL
 * yields zero. This does not verify edge consistency.
 */
extern UINT32 DSL_tensor_evolution_edge_count
                                (const DSL_TENSOR_EVOLUTION_GRAPH *graph);
/*
 * Copy a stored node by one-based graph-local ID without revalidating the PU.
 * Require non-NULL graph/output and an in-range ID; failure preserves output.
 */
extern BOOL DSL_tensor_evolution_get_node
                                (const DSL_TENSOR_EVOLUTION_GRAPH *graph,
                                 DSL_TENSOR_EVOLUTION_NODE_ID id,
                                 DSL_TENSOR_EVOLUTION_NODE_RECORD *record);
/*
 * Copy a stored edge by one-based graph-local ID without revalidating the PU.
 * Require non-NULL graph/output and an in-range ID; failure preserves output.
 */
extern BOOL DSL_tensor_evolution_get_edge
                                (const DSL_TENSOR_EVOLUTION_GRAPH *graph,
                                 DSL_TENSOR_EVOLUTION_EDGE_ID id,
                                 DSL_TENSOR_EVOLUTION_EDGE_RECORD *record);
/*
 * Search stored semantic roots by value ID without requiring an active PU.
 * Optionally copy the first match; a NULL output is an existence query. Failure
 * leaves output unchanged and performs no graph or image mutation.
 */
extern BOOL DSL_tensor_evolution_find_semantic_root
                                (const DSL_TENSOR_EVOLUTION_GRAPH *graph,
                                 DSL_IR_VALUE_ID value_id,
                                 DSL_TENSOR_EVOLUTION_NODE_RECORD *record);
/*
 * Return a borrowed static node-kind spelling, or "unknown" for out-of-range
 * input; no graph, PU context, allocation or mutation is involved.
 */
extern const char *DSL_tensor_evolution_node_kind_name (UINT32 kind);
/*
 * Return a borrowed static transformation spelling, or "unknown" for
 * out-of-range input; no PU context, allocation or mutation is involved.
 */
extern const char *DSL_tensor_evolution_transform_kind_name (UINT32 kind);

#endif /* dsl_tensor_evolution_INCLUDED */
