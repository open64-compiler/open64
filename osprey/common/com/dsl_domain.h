/*
 * Copyright (C) 2026 Open64 Project
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

extern void DSL_Domain_Registry_Reset (void);
extern DSL_DOMAIN_ID DSL_Domain_Register (const char *name,
					  DSL_DOMAIN_ID parent_id,
					  UINT32 version,
					  UINT32 flags);
extern BOOL DSL_Domain_Is_Registered (const char *name);
extern DSL_DOMAIN_ID DSL_Domain_Find (const char *name);
extern const char *DSL_Domain_Name (DSL_DOMAIN_ID id);
extern BOOL DSL_Domain_Get_Info (DSL_DOMAIN_ID id, DSL_DOMAIN_INFO *info);
extern UINT32 DSL_Domain_Count (void);
extern BOOL DSL_Domain_At (UINT32 ordinal, DSL_DOMAIN_INFO *info);
extern void DSL_Domain_fprint_registry (FILE *f);

#endif /* dsl_domain_INCLUDED */
