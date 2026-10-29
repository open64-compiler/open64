/*
 * Copyright (C) 2026 Open64 Project
 */

/*
 * Private PU-local REGION interface preflight/commit mechanisms owned by
 * be/com. Requests and compiler context are caller-owned; no phase policy,
 * code generation or new .B state is introduced. Clone/discard lifecycle is
 * declared by common/ir_image/dsl_region_image.h through the live header.
 * Design: doc/COMMON-COM-DSL-PERSISTENCE-OWNERSHIP-AUDIT.md and
 * doc/FHE-SYNC5-STANDARD-WHIRL-LOWERING-CONTRACT.md.
 */

#ifndef dsl_region_internal_INCLUDED
#define dsl_region_internal_INCLUDED

#include "dsl_region.h"

typedef struct {
    ST_IDX old_st;
    ST_IDX new_st;
} DSL_REGION_SYMBOL_REDIRECT;

/*
 * Count exact interface ST matches in this PU's store, returning zero for
 * no store or a zero-index symbol. Read only; this is not a tree use count.
 */
extern UINT32 DSL_Region_Symbol_Use_Count (PU_Info *pu, ST_IDX st);

/*
 * Read this PU's interface vector length, or zero without a store; no
 * validation, filtering or transfer of the store's owned rows occurs.
 */
extern UINT32 DSL_Region_Interface_Count (PU_Info *pu);

/*
 * Copy a one-based interface row to nonnull caller storage. Missing store,
 * invalid ID or NULL output returns FALSE without writes; identities remain
 * borrowed references to externally owned symbols and regions.
 */
extern BOOL DSL_Region_Get_Interface (PU_Info *pu, UINT32 id,
                                      DSL_REGION_INTERFACE_RECORD *record);

/*
 * Require a nonempty in-memory managed store/tree and caller-established
 * current PU ownership, then structural/domain checks, unique attached REGION
 * WNs, current-scope interface symbols and a body STID for each output role.
 * Read only and optionally diagnose; existence does not prove dominance or
 * semantic correctness, and this function never switches compiler scope.
 */
extern BOOL DSL_Region_Verify_Terminal_PU (PU_Info *pu, FILE *diagnostic);

/*
 * Run lowering-transition preflight in local scratch and discard it, with
 * no live row or WN mutation. Require current PU for context checks even for
 * empty requests; return FALSE for rejected requests without rollback work.
 */
extern BOOL DSL_Region_Can_Apply_Lowering_Transitions
                                (PU_Info *pu,
                                 const DSL_REGION_SYMBOL_REDIRECT *redirects,
                                 UINT32 redirect_count,
                                 const ST_IDX *pruned_inputs,
                                 UINT32 pruned_input_count);

/*
 * For a nonempty request, rebuild and verify scratch under the current PU,
 * then swap only the live interface vector. Reported FALSE leaves live rows
 * unchanged; an empty request succeeds without context checks. Caller owns
 * physical lowering and cross-service rollback; allocation failures are uncaught.
 */
extern BOOL DSL_Region_Apply_Lowering_Transitions
                                (PU_Info *pu,
                                 const DSL_REGION_SYMBOL_REDIRECT *redirects,
                                 UINT32 redirect_count,
                                 const ST_IDX *pruned_inputs,
                                 UINT32 pruned_input_count);

/*
 * Preflight a complete nonempty input-pruning request in disposable scratch.
 * Return candidate verification status without live mutations; caller must
 * maintain the current PU/symbol context and repeat preflight before commit.
 */
extern BOOL DSL_Region_Can_Prune_Input_Symbols
                                (PU_Info *pu, const ST_IDX *symbols,
                                 UINT32 symbol_count);

/*
 * Rebuild and verify the full nonempty input-pruning candidate, then swap
 * its interfaces into the registered store. Reported failure leaves live rows
 * unchanged; no WN/symbol mutation or cross-service rollback is provided.
 */
extern BOOL DSL_Region_Prune_Input_Symbols
                                (PU_Info *pu, const ST_IDX *symbols,
                                 UINT32 symbol_count);

/*
 * Preflight replacing every matching interface ST in a copied store and
 * verify it; absent stores succeed even for zero symbols. Existing stores
 * reject zero indices; no source-match, type, active-PU or role proof is added.
 * Live rows/WNs remain unchanged, including on candidate rejection.
 */
extern BOOL DSL_Region_Can_Redirect_Symbol (PU_Info *pu, ST_IDX old_st, ST_IDX new_st);

/*
 * Require a registered store and repeat redirect preflight before replacing
 * all matching interface STs in place. FALSE leaves rows unchanged; no match
 * may succeed, no physical WNs are rewritten, and no other service is rolled back.
 */
extern BOOL DSL_Region_Redirect_Symbol (PU_Info *pu, ST_IDX old_st, ST_IDX new_st);

#endif /* dsl_region_internal_INCLUDED */
