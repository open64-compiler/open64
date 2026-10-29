/*
 * Copyright (C) 2026 Open64 Project
 */

/*
 * common/com implements the process-local, insertion-ordered domain registry.
 * It owns copied names, not WHIRL trees, and performs no optimization or
 * mapped-image changes. Callers serialize access and coordinate dependent
 * registry resets; allocation exceptions have no transactional recovery here.
 * Design: doc/WHIRL-DSL-INFRASTRUCTURE.md.
 */

#include <string.h>
#include <vector>

#include "dsl_domain.h"

struct DSL_DOMAIN_RECORD {
    DSL_DOMAIN_ID id;
    DSL_DOMAIN_ID parent_id;
    const char *name;
    UINT32 version;
    UINT32 flags;

    /* Initialize an unpublished record; registration supplies its owned name. */
    DSL_DOMAIN_RECORD() :
        id(DSL_DOMAIN_INVALID_ID),
        parent_id(DSL_DOMAIN_INVALID_ID),
        name(NULL),
        version(0),
        flags(0) {}
};

static std::vector<DSL_DOMAIN_RECORD> DSL_domain_registry;

/* Allocate a private name copy (NULL becomes empty) for registry ownership. */
static const char *
DSL_Domain_Save_Name (const char *name)
{
    const char *safe_name = name ? name : "";
    size_t len = strlen(safe_name) + 1;
    char *saved = new char[len];

    memcpy(saved, safe_name, len);
    return saved;
}

/* Check the one-based ID range in this epoch without touching any record. */
static BOOL
DSL_Domain_Valid_Id (DSL_DOMAIN_ID id)
{
    return id != DSL_DOMAIN_INVALID_ID && id <= DSL_domain_registry.size();
}

/*
 * Free every copied name before clearing IDs. Dependent registry retirement
 * remains caller-owned; clearing this vector does not repair their references.
 */
void
DSL_Domain_Registry_Reset (void)
{
    for (UINT32 i = 0; i < DSL_domain_registry.size(); ++i)
        delete [] DSL_domain_registry[i].name;

    DSL_domain_registry.clear();
}

/*
 * Preserve the first registration of a name. Reject ordinary validation errors
 * before allocation, then append an owning record with a one-based ID. This
 * path does not catch allocation exceptions or provide exception rollback.
 */
DSL_DOMAIN_ID
DSL_Domain_Register (const char *name, DSL_DOMAIN_ID parent_id,
		     UINT32 version, UINT32 flags)
{
    const char *safe_name = name ? name : "";
    DSL_DOMAIN_ID existing = DSL_Domain_Find(safe_name);

    if (safe_name[0] == '\0')
        return DSL_DOMAIN_INVALID_ID;

    /* Duplicate metadata is deliberately ignored before parent validation. */
    if (existing != DSL_DOMAIN_INVALID_ID)
        return existing;

    if (parent_id != DSL_DOMAIN_INVALID_ID && !DSL_Domain_Valid_Id(parent_id))
        return DSL_DOMAIN_INVALID_ID;

    DSL_DOMAIN_RECORD record;
    record.id = DSL_domain_registry.size() + 1;
    record.parent_id = parent_id;
    record.name = DSL_Domain_Save_Name(safe_name);
    record.version = version;
    record.flags = flags;
    DSL_domain_registry.push_back(record);

    return record.id;
}

/* Use the shared exact-name lookup; no registration or metadata update occurs. */
BOOL
DSL_Domain_Is_Registered (const char *name)
{
    return DSL_Domain_Find(name) != DSL_DOMAIN_INVALID_ID;
}

/* Scan owned names in registration order; normalize NULL to an absent name. */
DSL_DOMAIN_ID
DSL_Domain_Find (const char *name)
{
    const char *safe_name = name ? name : "";

    for (UINT32 i = 0; i < DSL_domain_registry.size(); ++i) {
        if (strcmp(DSL_domain_registry[i].name, safe_name) == 0)
            return DSL_domain_registry[i].id;
    }

    return DSL_DOMAIN_INVALID_ID;
}

/* Validate before indexing and return the owned string, never a new copy. */
const char *
DSL_Domain_Name (DSL_DOMAIN_ID id)
{
    if (!DSL_Domain_Valid_Id(id))
        return NULL;

    return DSL_domain_registry[id - 1].name;
}

/*
 * Validate before writing any output; copy scalars and the borrowed name only
 * when requested. A valid ID with NULL output still succeeds without mutation.
 */
BOOL
DSL_Domain_Get_Info (DSL_DOMAIN_ID id, DSL_DOMAIN_INFO *info)
{
    if (!DSL_Domain_Valid_Id(id))
        return FALSE;

    if (info != NULL) {
        const DSL_DOMAIN_RECORD &record = DSL_domain_registry[id - 1];
        info->id = record.id;
        info->parent_id = record.parent_id;
        info->name = record.name;
        info->version = record.version;
        info->flags = record.flags;
    }

    return TRUE;
}

/* Report vector size through the public count type; do not allocate or mutate. */
UINT32
DSL_Domain_Count (void)
{
    return DSL_domain_registry.size();
}

/* Bounds-check the zero-based ordinal, then share Get_Info's output contract. */
BOOL
DSL_Domain_At (UINT32 ordinal, DSL_DOMAIN_INFO *info)
{
    if (ordinal >= DSL_domain_registry.size())
        return FALSE;

    return DSL_Domain_Get_Info(DSL_domain_registry[ordinal].id, info);
}

/*
 * Emit a read-only registration-order dump, skipping NULL streams. The caller
 * owns stream lifetime and error handling; no I/O status is propagated here.
 */
void
DSL_Domain_fprint_registry (FILE *f)
{
    if (f == NULL)
        return;

    fprintf(f, "DSL Domain Registry: entries=%u\n", DSL_Domain_Count());
    for (UINT32 i = 0; i < DSL_domain_registry.size(); ++i) {
        const DSL_DOMAIN_RECORD &record = DSL_domain_registry[i];
        fprintf(f, "  [%u] id=%u name=%s parent=%u version=%u flags=0x%x\n",
	    i,
	    record.id,
	    record.name,
	    record.parent_id,
	    record.version,
	    record.flags);
    }
}
