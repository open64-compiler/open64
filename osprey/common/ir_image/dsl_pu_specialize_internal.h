/*
 * Copyright (C) 2026 Open64 Project
 *
 * Private common/ir_image managed-row clone services for an owning program-scope PU
 * transaction. The caller owns physical cloning, PU activation and whole-stage
 * failure handling; these helpers do not authorize cross-PU optimization or
 * provide a persistent-edit API. See
 * doc/FHE-SYNC6-PU-SPECIALIZATION-TRANSACTION.md.
 */

#ifndef dsl_pu_specialize_internal_INCLUDED
#define dsl_pu_specialize_internal_INCLUDED

#include "dsl_ir_image.h"

/* Caller-owned logical-ID correspondence; local ST ordinals are not remapped. */
typedef struct {
    DSL_IR_VALUE_ID source_value_id;
    DSL_IR_VALUE_ID clone_value_id;
} DSL_PU_CLONE_VALUE_PAIR;

/* Append watermarks, not row copies or a generation-checked transaction token.
 * Valid only within the same live image with original prefixes intact. */
typedef struct {
    UINT32 node_count;
    UINT32 attribute_count;
    UINT32 value_count;
    UINT32 reference_count;
    UINT32 formal_count;
    UINT32 pu_identity_count;
    UINT32 runtime_value_projection_count;
    UINT32 runtime_call_projection_count;
    UINT32 retired_formal_count;
    UINT32 retired_call_argument_count;
    UINT32 runtime_input_count;
    UINT32 runtime_input_binding_count;
    UINT32 runtime_input_call_count;
} DSL_PU_CLONE_IMAGE_SAVEPOINT;

/* Stage fresh logical rows for an already physically cloned PU, retaining local
 * ST ordinals in its separate symbol storage. Caller supplies distinct nonzero
 * same-level PU STs/names, an existing source identity, no clone identity, and
 * space for every value with exact metadata "owner_pu=<source_pu_name>".
 * The owning transaction must establish physical/owner legality and prevent
 * intervening image mutations. Only the supported callee-side row subset is
 * copied; this is not a complete call-graph/REGION/domain-state clone.
 * Non-NULL pair_count is zeroed even on rejection. Early FALSE leaves savepoint
 * and pairs untouched; after capture, FALSE trims recorded table suffixes but
 * may leave pairs partially written and strings interned. Consume pairs only
 * on TRUE, which publishes their count after core/formal/call validation. Keep
 * the savepoint for later stage rejection, not arbitrary in-place rollback.
 * Allocation failures are not caught and have no rollback guarantee. */
extern BOOL DSL_IR_Image_Clone_PU_Values
                (ST_IDX source_pu_st, const char *source_pu_name,
                 ST_IDX clone_pu_st, const char *clone_pu_name,
                 DSL_PU_CLONE_VALUE_PAIR *pairs, UINT32 pair_capacity,
                 UINT32 *pair_count,
                 DSL_PU_CLONE_IMAGE_SAVEPOINT *savepoint);
/* Discard append suffixes after a successful clone or its internal late FALSE.
 * Use only a captured same-image point with intact prefixes and no unrelated
 * appends; NULL is a no-op. Counts are not validated and cannot recreate rows.
 * Does not restore old-row edits, opcodes, strings, WNs, symbols, REGION/domain
 * stores, pair outputs or PU context. Caller owns other rollback and subsequent
 * verification; there is no status or verifier here. */
extern void DSL_IR_Image_Clone_PU_Restore
                (const DSL_PU_CLONE_IMAGE_SAVEPOINT *savepoint);
/* Replace a borrowed call association only when its expected pointer matches;
 * FALSE preserves the association and WNs remain caller-owned. */
extern BOOL DSL_Call_Image_Replace_Call_WN
                (DSL_CALLSITE_METADATA_ID id, const WN *expected,
                 WN *replacement);
/* Shift managed formal ordinals at/after a selected-PU insertion point;
 * caller coordinates physical formals, ABI rows and final verification. */
extern BOOL DSL_PU_Interface_Image_Shift_Formals
                (ST_IDX owner_pu_st, UINT32 first_ordinal,
                 UINT32 count);
/* Shift managed actual ordinals at/after the insertion point for one
 * callsite. Caller owns the corresponding physical CALL/ABI transaction and
 * final validation. */
extern BOOL DSL_Call_ABI_Image_Shift_Arguments
                (DSL_CALLSITE_METADATA_ID callsite_id,
                 UINT32 first_ordinal, UINT32 count);
/* Retarget one matching borrowed CALL association and callee identity after
 * transaction preflight. FALSE preserves state; WN ownership and wider
 * rollback remain with the caller. */
extern BOOL DSL_Call_Image_Retarget_Call_WN
                (DSL_CALLSITE_METADATA_ID callsite_id,
                 const WN *expected, WN *replacement,
                 ST_IDX new_callee);

#endif /* dsl_pu_specialize_internal_INCLUDED */
