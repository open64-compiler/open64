/*
 * Copyright (C) 2026 Open64 Project
 */

/*
 * Live opaque REGION handles and PU-local physical mechanisms owned by be/com.
 * Callers establish WN, pool and symbol-table scope; this backend-free support
 * remains direct-object usable by frontends/readers and introduces no optimizer
 * policy, code-generation dependency or .B layout. Persisted schema and store
 * lifetime/I/O remain with common/com and common/ir_image respectively.
 * Design: doc/COMMON-COM-DSL-PERSISTENCE-OWNERSHIP-AUDIT.md and
 * doc/WHIRL-DSL-INFRASTRUCTURE.md.
 */

#ifndef DSL_REGION_LIVE_INCLUDED
#define DSL_REGION_LIVE_INCLUDED

#include <stdio.h>
#include "../../common/com/dsl_region.h"
#include "../../common/ir_image/dsl_region_image.h"
#include "wn.h"

typedef struct dsl_region_runtime *DSL_REGION;

/*
 * Create a detached pragma REGION and register an owned runtime handle for
 * a caller-owned in-memory PU. Reject missing PU/name/version/tree state
 * before mutation; otherwise allocate blocks/classifier, intern the contract
 * and derive parent IDs/depth. Caller must supply a live compatible parent
 * and later attach/verify the tree; no parent-store or version-range proof,
 * allocation rollback, RID reconstruction or optimization is performed.
 */
extern DSL_REGION DSL_Region_Create (PU_Info *pu, DSL_REGION parent,
                                     const char *contract_name,
                                     UINT32 contract_version);

/*
 * Link a caller-supplied non-BLOCK statement last in a live region body.
 * Reject NULL/block input unchanged; caller must provide an unattached WN
 * and preserve strict tree ownership. This does not verify its semantics.
 */
extern BOOL DSL_Region_Append_Statement (DSL_REGION region, WN *statement);

/*
 * Attach a registered child last in its same-store parent body after checking
 * the recorded parent ID and direct duplicate attachment. Rejection leaves
 * links unchanged; caller must ensure neither WN is otherwise attached and
 * must maintain depth/tree invariants beyond these limited checks.
 */
extern BOOL DSL_Region_Append_Child (DSL_REGION parent, DSL_REGION child);

/*
 * Append a live registered region to its owning PU function body. NULL or
 * unregistered handles reject unchanged; caller supplies a valid FUNC_ENTRY
 * and unattached region, since duplicate attachment is not checked here.
 */
extern BOOL DSL_Region_Append_To_PU (DSL_REGION region);

/*
 * Append one region_metadata pragma with the region source position after
 * checking a nonempty alphanumeric/underscore/dot/hyphen key and no existing
 * binding. NULL/invalid/duplicate inputs reject before mutation; caller
 * retains strings, while shared strings/WN pool own newly created metadata.
 * Allocation failure has no rollback guarantee.
 */
extern BOOL DSL_Region_Set_Metadata (DSL_REGION region, const char *key,
                                     const char *value);

/*
 * Append a copied interface row to a registered region, checking only a
 * nonzero ST index and nonempty supported role mask. Caller supplies valid
 * symbol lifetime, ordinals, flags and uniqueness for later verification;
 * reported rejection changes nothing, allocation exceptions are uncaught.
 */
extern BOOL DSL_Region_Declare_Symbol (DSL_REGION region, ST_IDX st,
                                      UINT32 roles, UINT32 ordinal,
                                      UINT32 flags);

/*
 * Set the live region, its three BLOCKs and existing pragma statements to
 * the supplied source position. NULL rejects unchanged; body statements and
 * symbols are not visited, and zero positions are accepted at this layer.
 */
extern BOOL DSL_Region_Set_Source_Position (DSL_REGION region, SRCPOS spos);

/*
 * Borrow the runtime handle's physical WN, or return NULL for a NULL handle.
 * Caller must keep nonnull handles live; no ownership transfer or validation.
 */
extern WN *DSL_Region_WN (DSL_REGION region);

/*
 * Test pointer membership in this PU's registered region wrappers without
 * tree traversal or mutation; absent stores and NULL WNs return FALSE.
 */
extern BOOL DSL_Region_Is_Managed_WN (PU_Info *pu, const WN *wn);

/*
 * Remove one matching wrapper and all of its interface rows, leaving the
 * borrowed WN/tree untouched for caller-owned lowering. No match rejects
 * unchanged; consuming the last wrapper deletes the store, clears the PU
 * regions pointer and marks WT_REGIONS missing. Other handles survive.
 */
extern BOOL DSL_Region_Consume_WN (PU_Info *pu, const WN *wn);

/*
 * Verify the PU's registered store with structural and recognized domain
 * checks, treating no store as valid. Borrow its context and optional stream;
 * do not activate a PU, repair state, or prove terminal tree attachment.
 */
extern BOOL DSL_Region_Verify_PU (PU_Info *pu, FILE *diagnostic);

/*
 * Print stored region/metadata/interface facts to a borrowed FILE, requiring
 * live REGION trees and readable contract/pragma strings. NULL stream or no
 * store is a no-op; this does not validate, mutate, free or activate anything.
 */
extern void DSL_Region_Print_PU (FILE *file, PU_Info *pu);

#endif /* DSL_REGION_LIVE_INCLUDED */
