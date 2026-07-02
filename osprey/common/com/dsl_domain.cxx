/*
 * Copyright (C) 2026 Open64 Project
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

  DSL_DOMAIN_RECORD() :
    id(DSL_DOMAIN_INVALID_ID),
    parent_id(DSL_DOMAIN_INVALID_ID),
    name(NULL),
    version(0),
    flags(0) {}
};

static std::vector<DSL_DOMAIN_RECORD> Dsl_domain_registry;

static const char *
DSL_Domain_Save_Name (const char *name)
{
  const char *safe_name = name ? name : "";
  size_t len = strlen(safe_name) + 1;
  char *saved = new char[len];

  memcpy(saved, safe_name, len);
  return saved;
}

static BOOL
DSL_Domain_Valid_Id (DSL_DOMAIN_ID id)
{
  return id != DSL_DOMAIN_INVALID_ID && id <= Dsl_domain_registry.size();
}

void
DSL_Domain_Registry_Reset (void)
{
  for (UINT32 i = 0; i < Dsl_domain_registry.size(); ++i)
    delete [] Dsl_domain_registry[i].name;

  Dsl_domain_registry.clear();
}

DSL_DOMAIN_ID
DSL_Domain_Register (const char *name, DSL_DOMAIN_ID parent_id,
		     UINT32 version, UINT32 flags)
{
  const char *safe_name = name ? name : "";
  DSL_DOMAIN_ID existing = DSL_Domain_Find(safe_name);

  if (safe_name[0] == '\0')
    return DSL_DOMAIN_INVALID_ID;

  if (existing != DSL_DOMAIN_INVALID_ID)
    return existing;

  if (parent_id != DSL_DOMAIN_INVALID_ID && !DSL_Domain_Valid_Id(parent_id))
    return DSL_DOMAIN_INVALID_ID;

  DSL_DOMAIN_RECORD record;
  record.id = Dsl_domain_registry.size() + 1;
  record.parent_id = parent_id;
  record.name = DSL_Domain_Save_Name(safe_name);
  record.version = version;
  record.flags = flags;
  Dsl_domain_registry.push_back(record);

  return record.id;
}

BOOL
DSL_Domain_Is_Registered (const char *name)
{
  return DSL_Domain_Find(name) != DSL_DOMAIN_INVALID_ID;
}

DSL_DOMAIN_ID
DSL_Domain_Find (const char *name)
{
  const char *safe_name = name ? name : "";

  for (UINT32 i = 0; i < Dsl_domain_registry.size(); ++i) {
    if (strcmp(Dsl_domain_registry[i].name, safe_name) == 0)
      return Dsl_domain_registry[i].id;
  }

  return DSL_DOMAIN_INVALID_ID;
}

const char *
DSL_Domain_Name (DSL_DOMAIN_ID id)
{
  if (!DSL_Domain_Valid_Id(id))
    return NULL;

  return Dsl_domain_registry[id - 1].name;
}

BOOL
DSL_Domain_Get_Info (DSL_DOMAIN_ID id, DSL_DOMAIN_INFO *info)
{
  if (!DSL_Domain_Valid_Id(id))
    return FALSE;

  if (info != NULL) {
    const DSL_DOMAIN_RECORD &record = Dsl_domain_registry[id - 1];
    info->id = record.id;
    info->parent_id = record.parent_id;
    info->name = record.name;
    info->version = record.version;
    info->flags = record.flags;
  }

  return TRUE;
}

UINT32
DSL_Domain_Count (void)
{
  return Dsl_domain_registry.size();
}

BOOL
DSL_Domain_At (UINT32 ordinal, DSL_DOMAIN_INFO *info)
{
  if (ordinal >= Dsl_domain_registry.size())
    return FALSE;

  return DSL_Domain_Get_Info(Dsl_domain_registry[ordinal].id, info);
}

void
DSL_Domain_fprint_registry (FILE *f)
{
  if (f == NULL)
    return;

  fprintf(f, "DSL Domain Registry: entries=%u\n", DSL_Domain_Count());
  for (UINT32 i = 0; i < Dsl_domain_registry.size(); ++i) {
    const DSL_DOMAIN_RECORD &record = Dsl_domain_registry[i];
    fprintf(f, "  [%u] id=%u name=%s parent=%u version=%u flags=0x%x\n",
	    i,
	    record.id,
	    record.name,
	    record.parent_id,
	    record.version,
	    record.flags);
  }
}
