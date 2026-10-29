/*
 * Copyright (C) 2026 Open64 Project
 */

/*
 * common/com exposes process-local DSL domain identity and parent metadata.
 * Callers coordinate registry lifetime and access; this interface neither
 * grants cross-PU transformation scope nor defines a mapped-image layout.
 * Names are registry-owned and IDs are meaningful only within a reset epoch.
 * Design: doc/WHIRL-DSL-INFRASTRUCTURE.md.
 */

#ifndef dsl_domain_INCLUDED
#define dsl_domain_INCLUDED

#include <stdio.h>

#include "defs.h"

#define DSL_DOMAIN_INVALID_ID 0

typedef UINT32 DSL_DOMAIN_ID;

typedef struct {
    DSL_DOMAIN_ID id;
    DSL_DOMAIN_ID parent_id;
    const char *name;
    UINT32 version;
    UINT32 flags;
} DSL_DOMAIN_INFO;

/*
 * Release all domain names and entries. Borrowed names and old ID meanings
 * expire; later registrations may reuse IDs. Callers must first retire or
 * reset dependent registries, such as contracts; this resets only domains.
 */
extern void DSL_Domain_Registry_Reset (void);

/*
 * Intern a nonempty, case-sensitive name, copying it for registry ownership.
 * A duplicate returns its old ID without checking or updating parent, version
 * or flags. A new entry requires parent_id to be zero (no parent) or live;
 * version and flags are stored verbatim. Null/empty names and invalid new
 * parents return zero without mutation. Allocation failures are not converted
 * to an error ID and have no exception-rollback guarantee.
 */
extern DSL_DOMAIN_ID DSL_Domain_Register (const char *name,
					  DSL_DOMAIN_ID parent_id,
					  UINT32 version,
					  UINT32 flags);
/* Test exact name membership without mutation; null/empty names are absent. */
extern BOOL DSL_Domain_Is_Registered (const char *name);

/* Find a case-sensitive name without mutation; return zero when absent/null. */
extern DSL_DOMAIN_ID DSL_Domain_Find (const char *name);

/* Borrow a name until reset, or return NULL for an invalid ID; do not free it. */
extern const char *DSL_Domain_Name (DSL_DOMAIN_ID id);

/*
 * Copy fields for a live ID, borrowing its name until reset. A null output
 * only tests validity. Invalid IDs return FALSE and leave output untouched.
 */
extern BOOL DSL_Domain_Get_Info (DSL_DOMAIN_ID id, DSL_DOMAIN_INFO *info);

/* Read the entry count in the current reset epoch without changing it. */
extern UINT32 DSL_Domain_Count (void);

/*
 * Inspect a zero-based registration ordinal using Get_Info's borrowing/null
 * rules. Out-of-range ordinals return FALSE without modifying output.
 */
extern BOOL DSL_Domain_At (UINT32 ordinal, DSL_DOMAIN_INFO *info);

/*
 * Print registration-order metadata to a caller-owned stream without changing
 * entries. NULL is a no-op; stream write failures are not reported by this API.
 */
extern void DSL_Domain_fprint_registry (FILE *f);

#endif /* dsl_domain_INCLUDED */
