/*
 * Copyright (C) 2026 Open64 Project
 */

/*
 * Shared backend copied-control contract for the driver-selected active PU.
 * be/com owns snapshot lifetime and validation; WOPT owns CFG capture. No VHO
 * analysis dependency, WOPT object retention, or binary .B layout change.
 * Design: doc/DSL-BACKEND-OWNERSHIP-RELOCATION.md and
 *  doc/AI-COMPILER-OPTIMIZATION-AIO4-LIFETIME-LOCALITY.md.
 */

#ifndef dsl_tensor_control_snapshot_INCLUDED
#define dsl_tensor_control_snapshot_INCLUDED

#include <stdio.h>

#include "defs.h"
#include "dsl_ir_image.h"
#include "symtab_idx.h"

struct pu_info;
struct DSL_TENSOR_CONTROL_SNAPSHOT;
typedef struct DSL_TENSOR_CONTROL_SNAPSHOT DSL_TENSOR_CONTROL_SNAPSHOT;

typedef enum {
    DSL_TENSOR_CONTROL_NONE = 0,
    DSL_TENSOR_CONTROL_BRANCH = 0x0001,
    DSL_TENSOR_CONTROL_LOOP = 0x0002,
    DSL_TENSOR_CONTROL_REGION = 0x0004,
    DSL_TENSOR_CONTROL_EFFECT_BARRIER = 0x0008
} DSL_TENSOR_CONTROL_FLAG;

typedef struct {
    UINT32 block_id;
    UINT32 reverse_postorder;
    UINT32 immediate_dominator;
    UINT32 immediate_postdominator;
    UINT32 loop_depth;
    UINT32 region_id;
    UINT32 flags;
    UINT32 reserved;
} DSL_TENSOR_CONTROL_BLOCK;

typedef struct {
    DSL_IR_NODE_ID node_id;
    UINT32 block_id;
    UINT32 statement_order;
    UINT32 reverse_postorder;
    UINT32 reserved;
} DSL_TENSOR_CONTROL_POSITION;

/*
 * Allocate an empty, caller-owned snapshot for the driver's active PU.
 * Require matching Current_PU_Info and a valid global function symbol; return
 * NULL with an optional diagnostic on rejection. Borrow the PU until destruction;
 * full Current_pu matching is checked by subsequent snapshot operations.
 */
extern DSL_TENSOR_CONTROL_SNAPSHOT *DSL_tensor_control_snapshot_create
                                (struct pu_info *pu,
                                 FILE *diagnostic);
/*
 * Release the snapshot and its copied vectors; NULL is allowed. The caller
 * must first retire every locality analysis borrowing it. Never free the PU.
 */
extern void DSL_tensor_control_snapshot_destroy
                                (DSL_TENSOR_CONTROL_SNAPSHOT *snapshot);
/*
 * Copy one block into an active, unsealed snapshot. Reject malformed IDs,
 * flags, reserved fields, or a duplicate block ID without appending; dominance
 * references and duplicate reverse postorder values are checked at verification.
 */
extern BOOL DSL_tensor_control_snapshot_add_block
                                (DSL_TENSOR_CONTROL_SNAPSHOT *snapshot,
                                 const DSL_TENSOR_CONTROL_BLOCK *block,
                                 FILE *diagnostic);
/*
 * Copy one live DSL-node position into an active, unsealed snapshot. Reject
 * malformed fields, retired/missing nodes, or a duplicate node ID without
 * appending. Block existence and reverse-postorder agreement are checked later.
 */
extern BOOL DSL_tensor_control_snapshot_add_position
                                (DSL_TENSOR_CONTROL_SNAPSHOT *snapshot,
                                 const DSL_TENSOR_CONTROL_POSITION *position,
                                 FILE *diagnostic);
/*
 * Sort blocks by ID and positions by reverse postorder, statement, and node ID,
 * then mark an active snapshot sealed and verify it. Failure after sorting leaves
 * it sorted and sealed; repeated calls verify without sorting or rollback.
 */
extern BOOL DSL_tensor_control_snapshot_seal
                                (DSL_TENSOR_CONTROL_SNAPSHOT *snapshot,
                                 FILE *diagnostic);
/*
 * Read-check the active driver PU, ordered unique block IDs, unique block
 * reverse postorder, dominance references, and ordered live-node positions.
 * Does not require sealed state or detect arbitrary CFG staleness; return FALSE
 * with optional diagnostics on the first inconsistency, without mutation.
 */
extern BOOL DSL_tensor_control_snapshot_verify
                                (const DSL_TENSOR_CONTROL_SNAPSHOT *snapshot,
                                 FILE *diagnostic);
/*
 * Print copied snapshot state without requiring an active PU or revalidating
 * it. A NULL stream or snapshot is ignored; retained PU symbols must remain
 * readable. Borrow all inputs and leave the snapshot and WHIRL unchanged.
 */
extern void DSL_tensor_control_snapshot_print
                                (FILE *file,
                                 const DSL_TENSOR_CONTROL_SNAPSHOT *snapshot);

#endif /* dsl_tensor_control_snapshot_INCLUDED */
