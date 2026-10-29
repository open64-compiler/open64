/*
 * Private managed-row expansion checkpoint. The physical transaction owns
 * the wider rollback; this image-only journal neither snapshots arbitrary
 * old-row edits nor grants transformation or cross-PU authority.
 * Design: doc/WHIRL-DSL-INFRASTRUCTURE.md, persistent IR /
 * nonpersistent service separation. No mapped format change.
 */

#ifndef DSL_IMAGE_CHECKPOINT_INTERNAL_INCLUDED
#define DSL_IMAGE_CHECKPOINT_INTERNAL_INCLUDED

#include <stdio.h>
#include "../com/dsl_ir_image.h"

class WN;
struct pu_info;
typedef struct pu_info PU_Info;
typedef INT32 WN_MAP;

/* Runtime-only core-table watermarks and one source's flags, not copied rows
 * or an image-generation token. Valid only within the caller's same-image
 * append/redirect transaction while the saved prefixes retain their identity. */
typedef struct {
    UINT32 opcode_count;
    UINT32 node_count;
    UINT32 attribute_count;
    UINT32 value_count;
    UINT32 reference_count;
    DSL_IR_NODE_ID source_node_id;
    DSL_IR_VALUE_ID source_value_id;
    UINT32 source_node_flags;
    UINT32 source_value_flags;
} DSL_CKKS_IMAGE_SAVEPOINT;




/* Capture five core table counts and the bounded source/producer IDs and flags
 * after a result-backlink check. Require nonnull output and caller-validated
 * core identities; FALSE leaves output unchanged. No live image mutation or
 * reservation against intervening edits. This is not owner/purity/schema
 * verification or a full snapshot; the transaction owns scope and lifetime. */
extern BOOL DSL_IR_Image_CKKS_Save
                (DSL_IR_VALUE_ID source_value_id,
                 DSL_CKKS_IMAGE_SAVEPOINT *savepoint);
/* Undo only this core journal within its original image/transaction: saved
 * prefixes must still exist without unrelated edits or reused IDs. Reverse all
 * old-prefix references equal to the transaction's fresh replacement value
 * (INVALID skips reversal), restore in-range source flags, then truncate five
 * core tables. This is not an exact reference edit log and cannot reconstruct
 * deleted rows or undo arbitrary row changes. NULL is a no-op; stale/forged
 * savepoints are not checked and the void return provides no status. Caller
 * separately restores WNs/ABI/REGION/runtime/events/strings/symbols/metadata,
 * validates the combined rollback and handles its failure. No PU activation. */
extern void DSL_IR_Image_CKKS_Restore
                (const DSL_CKKS_IMAGE_SAVEPOINT *savepoint,
                 DSL_IR_VALUE_ID replacement_value_id);
/* Commit a checked pure-source redirect and lowered flags in managed rows;
 * FALSE is read-only. The owning physical transaction proves legality and
 * coordinates combined verification/rollback. */
extern BOOL DSL_IR_Image_Redirect_And_Lower_Value
                (DSL_IR_VALUE_ID source_value_id,
                 DSL_IR_VALUE_ID replacement_value_id);

#endif /* DSL_IMAGE_CHECKPOINT_INTERNAL_INCLUDED */
