/*
 * Copyright (C) 2026 Open64 Project
 */

/*
 * common/com exposes process-local DSL contract metadata between domain IDs.
 * Registration publishes check/diagnostic names, not their execution or a
 * lowering implementation. Callers serialize access and retire contracts
 * before resetting referenced domains. IDs and borrowed strings last only
 * within a contract reset epoch; no mapped-image format is defined here.
 * Design: doc/WHIRL-DSL-INFRASTRUCTURE.md, item 29.
 */

#ifndef dsl_contract_INCLUDED
#define dsl_contract_INCLUDED

#include <stdio.h>

#include "defs.h"
#include "dsl_domain.h"

#define DSL_CONTRACT_INVALID_ID 0

#define DSL_CONTRACT_FLAG_EXPRESSION 0x00000001
#define DSL_CONTRACT_FLAG_REGION     0x00000002
#define DSL_CONTRACT_FLAG_STATE      0x00000004

#define DSL_CONTRACT_TRANSFORMER_ROTARY_EMBEDDING \
    "transformer.rotary_embedding"
#define DSL_CONTRACT_TRANSFORMER_ATTENTION "transformer.attention"
#define DSL_CONTRACT_TRANSFORMER_DECODER_LAYER "transformer.decoder_layer"
#define DSL_CONTRACT_TRANSFORMER_DECODE "transformer.decode"
#define DSL_CONTRACT_TRANSFORMER_KV_CACHE_STATE \
    "transformer.kv_cache_state"

typedef UINT32 DSL_CONTRACT_ID;

typedef struct {
    DSL_CONTRACT_ID id;
    DSL_DOMAIN_ID source_domain_id;
    DSL_DOMAIN_ID target_domain_id;
    const char *name;
    UINT32 version;
    UINT32 flags;
    UINT32 required_check_count;
    UINT32 diagnostic_code_count;
} DSL_CONTRACT_INFO;

/*
 * Free all owned contract/check/diagnostic strings and entries, leaving domains
 * unchanged. Borrowed strings and old ID meanings expire; IDs may be reused.
 */
extern void DSL_Contract_Registry_Reset (void);

/*
 * Register by exact name and source/target IDs, ignoring version for identity.
 * A duplicate returns the first matching ID without validating new domains or
 * replacing metadata. For a new entry, require a nonempty name and live domain
 * IDs; version (including zero) and flags are stored verbatim. Copy all strings;
 * null array pointers/entries yield empty strings for the requested counts.
 * Nonnull arrays must contain at least those counts of readable entries.
 * Rejected names/domains return zero before mutation. Allocation exceptions
 * are not converted to error IDs and have no exception-rollback guarantee.
 */
extern DSL_CONTRACT_ID DSL_Contract_Register
				    (const char *name,
				     DSL_DOMAIN_ID source_domain_id,
				     DSL_DOMAIN_ID target_domain_id,
				     UINT32 version,
				     UINT32 flags,
				     const char *const *required_checks,
				     UINT32 required_check_count,
				     const char *const *diagnostic_codes,
				     UINT32 diagnostic_code_count);
/*
 * Register by the same copying/ownership rules as Register, adding version to
 * identity and rejecting version zero even for a duplicate. An existing exact
 * version returns its ID without replacing metadata; a new version requires
 * live source/target domains. Validation errors leave the registry unchanged;
 * allocation exceptions have no rollback guarantee.
 */
extern DSL_CONTRACT_ID DSL_Contract_Register_Versioned
                                    (const char *name,
                                     DSL_DOMAIN_ID source_domain_id,
                                     DSL_DOMAIN_ID target_domain_id,
                                     UINT32 version,
                                     UINT32 flags,
                                     const char *const *required_checks,
                                     UINT32 required_check_count,
                                     const char *const *diagnostic_codes,
                                     UINT32 diagnostic_code_count);
/*
 * Return the first registered exact name/source/target match, not the newest
 * version; zero means absent (also for null/empty names). Read-only lookup
 * compares numeric domain IDs without revalidating their registry lifetime.
 */
extern DSL_CONTRACT_ID DSL_Contract_Find (const char *name,
					  DSL_DOMAIN_ID source_domain_id,
					  DSL_DOMAIN_ID target_domain_id);
/*
 * Find an exact name/source/target/version without mutation or domain checks;
 * return zero if absent. Version zero can find a legacy Register entry.
 */
extern DSL_CONTRACT_ID DSL_Contract_Find_Version
                                         (const char *name,
                                          DSL_DOMAIN_ID source_domain_id,
                                          DSL_DOMAIN_ID target_domain_id,
                                          UINT32 version);
/*
 * Find the highest registered numeric version for an exact name/source/target;
 * first registration wins a tie, zero means absent. This read-only query does
 * not revalidate domains or certify a version's gatekeeper/lowering support.
 */
extern DSL_CONTRACT_ID DSL_Contract_Find_Current
                                         (const char *name,
                                          DSL_DOMAIN_ID source_domain_id,
                                          DSL_DOMAIN_ID target_domain_id);
/*
 * Copy fields/counts for a live ID, borrowing its name until contract reset.
 * NULL output tests validity only. Invalid IDs return FALSE without writes.
 */
extern BOOL DSL_Contract_Get_Info (DSL_CONTRACT_ID id,
				   DSL_CONTRACT_INFO *info);
/* Read the number of entries in this reset epoch without changing the table. */
extern UINT32 DSL_Contract_Count (void);

/*
 * Inspect a zero-based registration ordinal with Get_Info's borrowing/null
 * rules. Out-of-range ordinals return FALSE and leave output untouched.
 */
extern BOOL DSL_Contract_At (UINT32 ordinal, DSL_CONTRACT_INFO *info);

/*
 * Borrow a check name at a zero-based ordinal until reset; do not free it.
 * Invalid IDs/ordinals return NULL. This query does not execute the check.
 */
extern const char *DSL_Contract_Required_Check_At (DSL_CONTRACT_ID id,
						  UINT32 ordinal);
/*
 * Borrow a diagnostic code at a zero-based ordinal until reset; do not free it.
 * Invalid IDs/ordinals return NULL; no diagnostic is emitted by this query.
 */
extern const char *DSL_Contract_Diagnostic_Code_At (DSL_CONTRACT_ID id,
						   UINT32 ordinal);
/*
 * Seed eight versioned contracts within an already registered transformer
 * domain, returning successful registrations including existing duplicates,
 * not the count newly inserted. Missing domain returns zero. This does not
 * install lowering or execute checks; the batch has no all-or-nothing rollback.
 */
extern UINT32 DSL_Contract_Register_Transformer_Decode (void);

/*
 * Print metadata/check/diagnostic names to a caller-owned stream, read-only.
 * NULL is a no-op; stream write failures are not reported by this API.
 */
extern void DSL_Contract_fprint_registry (FILE *f);

#endif /* dsl_contract_INCLUDED */
