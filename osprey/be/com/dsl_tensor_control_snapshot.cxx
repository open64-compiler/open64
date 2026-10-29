/*
 * Copyright (C) 2026 Open64 Project
 */

/*
 * be/com implements copied-control snapshot creation, append, sealing,
 * structural checks, and diagnostic printing for the driver-selected PU.
 * WOPT captures CFG facts; this sole snapshot lifecycle implementation never
 * activates another PU, chooses locality policy, or changes binary .B layout.
 * Design: doc/DSL-BACKEND-OWNERSHIP-RELOCATION.md and
 *  doc/AI-COMPILER-OPTIMIZATION-AIO4-LIFETIME-LOCALITY.md.
 */

#include <algorithm>
#include "../../common/ir_image/dsl_image_service.h"

#include "dsl_tensor_control_snapshot.h"
#include "dsl_tensor_control_snapshot_internal.h"
#include "pu_info.h"

/*
 * Report the existing locality diagnostic when a stream is supplied and
 * return FALSE for the caller's rejection path; mutate no compiler state.
 */
static BOOL
DSL_Tensor_Locality_Report
        (FILE *diagnostic, const char *message, UINT32 id)
{
    if (diagnostic != NULL)
        fprintf(diagnostic, "DSL tensor locality error: %s id=%u\n",
                message, id);
    return FALSE;
}

/*
 * Check that a copied owner is a live global function symbol with a valid
 * PU-table index before dereferencing its PU. Borrow current global tables.
 */
static BOOL
DSL_Tensor_Locality_Owner_Valid (ST_IDX owner_pu_st)
{
    return ST_IDX_level(owner_pu_st) == GLOBAL_SYMTAB &&
           ST_IDX_index(owner_pu_st) != 0 &&
           ST_IDX_index(owner_pu_st) < ST_Table_Size(GLOBAL_SYMTAB) &&
           ST_class(owner_pu_st) == CLASS_FUNC &&
           ST_pu(St_Table[owner_pu_st]) != PU_IDX_ZERO &&
           ST_pu(St_Table[owner_pu_st]) < PU_Table_Size();
}

/*
 * Require snapshot PU identity and owner symbol to match both driver active
 * PU globals. Read only; reject stale/wrong PU context without switching it.
 * This identity check alone does not establish CFG or node freshness.
 */
static BOOL
DSL_Tensor_Control_Active (const DSL_TENSOR_CONTROL_SNAPSHOT *snapshot)
{
    return snapshot != NULL && snapshot->pu != NULL &&
           Current_PU_Info == snapshot->pu &&
           DSL_Tensor_Locality_Owner_Valid(snapshot->owner_pu_st) &&
           PU_Info_proc_sym(snapshot->pu) == snapshot->owner_pu_st &&
           Current_pu ==
               &Pu_Table[ST_pu(St_Table[snapshot->owner_pu_st])];
}

/*
 * Find the first copied block with a nonzero ID; return borrowed vector
 * storage or NULL for invalid/missing input. Do not require sorting/activity;
 * subsequent vector mutation may invalidate the returned pointer.
 */
static const DSL_TENSOR_CONTROL_BLOCK *
DSL_Tensor_Control_Find_Block
        (const DSL_TENSOR_CONTROL_SNAPSHOT *snapshot, UINT32 block_id)
{
    if (snapshot == NULL || block_id == 0)
        return NULL;
    for (UINT32 i = 0; i < snapshot->blocks.size(); ++i) {
        if (snapshot->blocks[i].block_id == block_id)
            return &snapshot->blocks[i];
    }
    return NULL;
}

/*
 * Find the first copied position for a valid node ID; return borrowed vector
 * storage or NULL. Do not validate node liveness/activity; vector mutation may
 * invalidate the pointer, and callers own freshness checks.
 */
static const DSL_TENSOR_CONTROL_POSITION *
DSL_Tensor_Control_Find_Position
        (const DSL_TENSOR_CONTROL_SNAPSHOT *snapshot,
         DSL_IR_NODE_ID node_id)
{
    if (snapshot == NULL || node_id == DSL_IR_NODE_INVALID_ID)
        return NULL;
    for (UINT32 i = 0; i < snapshot->positions.size(); ++i) {
        if (snapshot->positions[i].node_id == node_id)
            return &snapshot->positions[i];
    }
    return NULL;
}

/*
 * Compare copied block IDs for sealing's deterministic sort without mutating
 * records; duplicate IDs compare equivalent and are rejected elsewhere.
 */
static BOOL
DSL_Tensor_Control_Block_Less
        (const DSL_TENSOR_CONTROL_BLOCK &left,
         const DSL_TENSOR_CONTROL_BLOCK &right)
{
    return left.block_id < right.block_id;
}

/*
 * Order positions by reverse postorder, statement order, then node ID.
 * Use the same strict order for sealing and verification; mutate neither input.
 */
static BOOL
DSL_Tensor_Control_Position_Less
        (const DSL_TENSOR_CONTROL_POSITION &left,
         const DSL_TENSOR_CONTROL_POSITION &right)
{
    if (left.reverse_postorder != right.reverse_postorder)
        return left.reverse_postorder < right.reverse_postorder;
    if (left.statement_order != right.statement_order)
        return left.statement_order < right.statement_order;
    return left.node_id < right.node_id;
}

/*
 * Allocate an empty, caller-owned snapshot for the driver's active PU.
 * Require matching Current_PU_Info and a valid global function symbol; return
 * NULL with an optional diagnostic on rejection. Borrow the PU until destruction;
 * full Current_pu matching is checked by subsequent snapshot operations.
 */
DSL_TENSOR_CONTROL_SNAPSHOT *
DSL_tensor_control_snapshot_create (PU_Info *pu, FILE *diagnostic)
{
    if (pu == NULL || Current_PU_Info != pu ||
        !DSL_Tensor_Locality_Owner_Valid(PU_Info_proc_sym(pu))) {
        DSL_Tensor_Locality_Report
            (diagnostic, "invalid active program unit", 0);
        return NULL;
    }
    DSL_TENSOR_CONTROL_SNAPSHOT *snapshot =
        new DSL_TENSOR_CONTROL_SNAPSHOT;
    snapshot->pu = pu;
    snapshot->owner_pu_st = PU_Info_proc_sym(pu);
    snapshot->sealed = FALSE;
    return snapshot;
}

/*
 * Release the snapshot and its copied vectors; NULL is allowed. The caller
 * must first retire every locality analysis borrowing it. Never free the PU.
 */
void
DSL_tensor_control_snapshot_destroy
        (DSL_TENSOR_CONTROL_SNAPSHOT *snapshot)
{
    delete snapshot;
}

/*
 * Copy one block into an active, unsealed snapshot. Reject malformed IDs,
 * flags, reserved fields, or a duplicate block ID without appending; dominance
 * references and duplicate reverse postorder values are checked at verification.
 */
BOOL
DSL_tensor_control_snapshot_add_block
        (DSL_TENSOR_CONTROL_SNAPSHOT *snapshot,
         const DSL_TENSOR_CONTROL_BLOCK *block, FILE *diagnostic)
{
    if (!DSL_Tensor_Control_Active(snapshot) || snapshot->sealed ||
        block == NULL || block->block_id == 0 ||
        block->reverse_postorder == 0 || block->reserved != 0 ||
        (block->flags & ~(DSL_TENSOR_CONTROL_BRANCH |
                          DSL_TENSOR_CONTROL_LOOP |
                          DSL_TENSOR_CONTROL_REGION |
                          DSL_TENSOR_CONTROL_EFFECT_BARRIER)) != 0)
        return DSL_Tensor_Locality_Report
                   (diagnostic, "invalid control block", block == NULL ?
                    0 : block->block_id);
    if (DSL_Tensor_Control_Find_Block(snapshot, block->block_id) != NULL)
        return DSL_Tensor_Locality_Report
                   (diagnostic, "duplicate control block", block->block_id);
    snapshot->blocks.push_back(*block);
    return TRUE;
}

/*
 * Copy one live DSL-node position into an active, unsealed snapshot. Reject
 * malformed fields, retired/missing nodes, or a duplicate node ID without
 * appending. Block existence and reverse-postorder agreement are checked later.
 */
BOOL
DSL_tensor_control_snapshot_add_position
        (DSL_TENSOR_CONTROL_SNAPSHOT *snapshot,
         const DSL_TENSOR_CONTROL_POSITION *position, FILE *diagnostic)
{
    DSL_IR_NODE_RECORD node;
    if (!DSL_Tensor_Control_Active(snapshot) || snapshot->sealed ||
        position == NULL || position->node_id == DSL_IR_NODE_INVALID_ID ||
        position->block_id == 0 || position->statement_order == 0 ||
        position->reverse_postorder == 0 || position->reserved != 0 ||
        !DSL_IR_Image_Get_Node(position->node_id, &node) ||
        (node.flags & DSL_IR_NODE_FLAG_RETIRED) != 0)
        return DSL_Tensor_Locality_Report
                   (diagnostic, "invalid control position",
                    position == NULL ? 0 : position->node_id);
    if (DSL_Tensor_Control_Find_Position(snapshot, position->node_id) != NULL)
        return DSL_Tensor_Locality_Report
                   (diagnostic, "duplicate control position",
                    position->node_id);
    snapshot->positions.push_back(*position);
    return TRUE;
}

/*
 * Read-check the active driver PU, ordered unique block IDs, unique block
 * reverse postorder, dominance references, and ordered live-node positions.
 * Does not require sealed state or detect arbitrary CFG staleness; return FALSE
 * with optional diagnostics on the first inconsistency, without mutation.
 */
BOOL
DSL_tensor_control_snapshot_verify
        (const DSL_TENSOR_CONTROL_SNAPSHOT *snapshot, FILE *diagnostic)
{
    if (!DSL_Tensor_Control_Active(snapshot))
        return DSL_Tensor_Locality_Report
                   (diagnostic, "control snapshot is not active", 0);
    /* Ordered copied IDs, not CFG reachability, define this structural check. */
    for (UINT32 i = 0; i < snapshot->blocks.size(); ++i) {
        const DSL_TENSOR_CONTROL_BLOCK &block = snapshot->blocks[i];
        if (block.block_id == 0 || block.reverse_postorder == 0 ||
            block.reserved != 0 ||
            (block.immediate_dominator != 0 &&
             DSL_Tensor_Control_Find_Block
                 (snapshot, block.immediate_dominator) == NULL) ||
            (block.immediate_postdominator != 0 &&
             DSL_Tensor_Control_Find_Block
                 (snapshot, block.immediate_postdominator) == NULL) ||
            (i != 0 &&
             snapshot->blocks[i - 1].block_id >= block.block_id))
            return DSL_Tensor_Locality_Report
                       (diagnostic, "invalid control block", block.block_id);
        for (UINT32 j = i + 1; j < snapshot->blocks.size(); ++j) {
            if (snapshot->blocks[j].reverse_postorder ==
                    block.reverse_postorder)
                return DSL_Tensor_Locality_Report
                           (diagnostic, "duplicate reverse postorder",
                            block.block_id);
        }
    }
    for (UINT32 i = 0; i < snapshot->positions.size(); ++i) {
        const DSL_TENSOR_CONTROL_POSITION &position = snapshot->positions[i];
        DSL_IR_NODE_RECORD node;
        if (position.node_id == DSL_IR_NODE_INVALID_ID ||
            position.statement_order == 0 ||
            position.reverse_postorder == 0 || position.reserved != 0 ||
            DSL_Tensor_Control_Find_Block
                (snapshot, position.block_id) == NULL ||
            DSL_Tensor_Control_Find_Block
                (snapshot, position.block_id)->reverse_postorder !=
                    position.reverse_postorder ||
            !DSL_IR_Image_Get_Node(position.node_id, &node) ||
            (node.flags & DSL_IR_NODE_FLAG_RETIRED) != 0 ||
            (i != 0 && !DSL_Tensor_Control_Position_Less
                           (snapshot->positions[i - 1], position)))
            return DSL_Tensor_Locality_Report
                       (diagnostic, "invalid control position",
                        position.node_id);
    }
    return TRUE;
}

/*
 * Sort blocks by ID and positions by reverse postorder, statement, and node ID,
 * then mark an active snapshot sealed and verify it. Failure after sorting leaves
 * it sorted and sealed; repeated calls verify without sorting or rollback.
 */
BOOL
DSL_tensor_control_snapshot_seal
        (DSL_TENSOR_CONTROL_SNAPSHOT *snapshot, FILE *diagnostic)
{
    if (!DSL_Tensor_Control_Active(snapshot))
        return DSL_Tensor_Locality_Report
                   (diagnostic, "control snapshot is not active", 0);
    /* Sealing commits order before validation; rejected snapshots stay sealed. */
    if (!snapshot->sealed) {
        std::sort(snapshot->blocks.begin(), snapshot->blocks.end(),
                  DSL_Tensor_Control_Block_Less);
        std::sort(snapshot->positions.begin(), snapshot->positions.end(),
                  DSL_Tensor_Control_Position_Less);
        snapshot->sealed = TRUE;
    }
    return DSL_tensor_control_snapshot_verify(snapshot, diagnostic);
}

/*
 * Print copied snapshot state without requiring an active PU or revalidating
 * it. A NULL stream or snapshot is ignored; retained PU symbols must remain
 * readable. Borrow all inputs and leave the snapshot and WHIRL unchanged.
 */
void
DSL_tensor_control_snapshot_print
        (FILE *file, const DSL_TENSOR_CONTROL_SNAPSHOT *snapshot)
{
    if (file == NULL || snapshot == NULL)
        return;
    fprintf(file, "DSLTensorControlSnapshot: owner=<%u,%u,%s> "
                  "blocks=%u positions=%u sealed=%s\n",
            ST_IDX_level(snapshot->owner_pu_st),
            ST_IDX_index(snapshot->owner_pu_st),
            DSL_Tensor_Locality_Owner_Valid(snapshot->owner_pu_st) ?
                ST_name(St_Table[snapshot->owner_pu_st]) : "<invalid>",
            (UINT32)snapshot->blocks.size(),
            (UINT32)snapshot->positions.size(),
            snapshot->sealed ? "yes" : "no");
    for (UINT32 i = 0; i < snapshot->blocks.size(); ++i) {
        const DSL_TENSOR_CONTROL_BLOCK &block = snapshot->blocks[i];
        fprintf(file, "  block[%u] rpo=%u idom=%u ipdom=%u loop=%u "
                      "region=%u flags=0x%x\n",
                block.block_id, block.reverse_postorder,
                block.immediate_dominator,
                block.immediate_postdominator, block.loop_depth,
                block.region_id, block.flags);
    }
    for (UINT32 i = 0; i < snapshot->positions.size(); ++i) {
        const DSL_TENSOR_CONTROL_POSITION &position = snapshot->positions[i];
        fprintf(file, "  position[%u] node=%u block=%u rpo=%u "
                      "statement=%u\n",
                i + 1, position.node_id, position.block_id,
                position.reverse_postorder, position.statement_order);
    }
}
