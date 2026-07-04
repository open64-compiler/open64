/*
 * Copyright (C) 2026 Open64 Project
 */

#include <string.h>
#include <vector>

#include "dsl_contract.h"

struct DSL_CONTRACT_RECORD {
    DSL_CONTRACT_ID id;
    DSL_DOMAIN_ID source_domain_id;
    DSL_DOMAIN_ID target_domain_id;
    const char *name;
    UINT32 version;
    UINT32 flags;
    std::vector<const char *> required_checks;
    std::vector<const char *> diagnostic_codes;

    DSL_CONTRACT_RECORD() :
        id(DSL_CONTRACT_INVALID_ID),
        source_domain_id(DSL_DOMAIN_INVALID_ID),
        target_domain_id(DSL_DOMAIN_INVALID_ID),
        name(NULL),
        version(0),
        flags(0) {}
};

static std::vector<DSL_CONTRACT_RECORD> DSL_contract_registry;

static const char *
DSL_Contract_Save_String (const char *str)
{
    const char *safe_str = str ? str : "";
    size_t len = strlen(safe_str) + 1;
    char *saved = new char[len];

    memcpy(saved, safe_str, len);
    return saved;
}

static BOOL
DSL_Contract_Valid_Id (DSL_CONTRACT_ID id)
{
    return id != DSL_CONTRACT_INVALID_ID && id <= DSL_contract_registry.size();
}

static void
DSL_Contract_Free_Record (DSL_CONTRACT_RECORD &record)
{
    delete [] record.name;

    for (UINT32 i = 0; i < record.required_checks.size(); ++i)
        delete [] record.required_checks[i];
    record.required_checks.clear();

    for (UINT32 i = 0; i < record.diagnostic_codes.size(); ++i)
        delete [] record.diagnostic_codes[i];
    record.diagnostic_codes.clear();
}

void
DSL_Contract_Registry_Reset (void)
{
    for (UINT32 i = 0; i < DSL_contract_registry.size(); ++i)
        DSL_Contract_Free_Record(DSL_contract_registry[i]);

    DSL_contract_registry.clear();
}

DSL_CONTRACT_ID
DSL_Contract_Register (const char *name,
		       DSL_DOMAIN_ID source_domain_id,
		       DSL_DOMAIN_ID target_domain_id,
		       UINT32 version,
		       UINT32 flags,
		       const char *const *required_checks,
		       UINT32 required_check_count,
		       const char *const *diagnostic_codes,
		       UINT32 diagnostic_code_count)
{
    const char *safe_name = name ? name : "";
    DSL_CONTRACT_ID existing =
        DSL_Contract_Find(safe_name, source_domain_id, target_domain_id);

    if (safe_name[0] == '\0')
        return DSL_CONTRACT_INVALID_ID;

    if (existing != DSL_CONTRACT_INVALID_ID)
        return existing;

    if (!DSL_Domain_Get_Info(source_domain_id, NULL) ||
            !DSL_Domain_Get_Info(target_domain_id, NULL))
        return DSL_CONTRACT_INVALID_ID;

    DSL_CONTRACT_RECORD record;
    record.id = DSL_contract_registry.size() + 1;
    record.source_domain_id = source_domain_id;
    record.target_domain_id = target_domain_id;
    record.name = DSL_Contract_Save_String(safe_name);
    record.version = version;
    record.flags = flags;

    for (UINT32 i = 0; i < required_check_count; ++i)
        record.required_checks.push_back
            (DSL_Contract_Save_String(required_checks == NULL ? NULL :
				required_checks[i]));

    for (UINT32 i = 0; i < diagnostic_code_count; ++i)
        record.diagnostic_codes.push_back
            (DSL_Contract_Save_String(diagnostic_codes == NULL ? NULL :
				diagnostic_codes[i]));

    DSL_contract_registry.push_back(record);
    return record.id;
}

DSL_CONTRACT_ID
DSL_Contract_Find (const char *name,
		   DSL_DOMAIN_ID source_domain_id,
		   DSL_DOMAIN_ID target_domain_id)
{
    const char *safe_name = name ? name : "";

    for (UINT32 i = 0; i < DSL_contract_registry.size(); ++i) {
        const DSL_CONTRACT_RECORD &record = DSL_contract_registry[i];
        if (record.source_domain_id == source_domain_id &&
	record.target_domain_id == target_domain_id &&
	strcmp(record.name, safe_name) == 0)
            return record.id;
    }

    return DSL_CONTRACT_INVALID_ID;
}

BOOL
DSL_Contract_Get_Info (DSL_CONTRACT_ID id, DSL_CONTRACT_INFO *info)
{
    if (!DSL_Contract_Valid_Id(id))
        return FALSE;

    if (info != NULL) {
        const DSL_CONTRACT_RECORD &record = DSL_contract_registry[id - 1];
        info->id = record.id;
        info->source_domain_id = record.source_domain_id;
        info->target_domain_id = record.target_domain_id;
        info->name = record.name;
        info->version = record.version;
        info->flags = record.flags;
        info->required_check_count = record.required_checks.size();
        info->diagnostic_code_count = record.diagnostic_codes.size();
    }

    return TRUE;
}

UINT32
DSL_Contract_Count (void)
{
    return DSL_contract_registry.size();
}

BOOL
DSL_Contract_At (UINT32 ordinal, DSL_CONTRACT_INFO *info)
{
    if (ordinal >= DSL_contract_registry.size())
        return FALSE;

    return DSL_Contract_Get_Info(DSL_contract_registry[ordinal].id, info);
}

const char *
DSL_Contract_Required_Check_At (DSL_CONTRACT_ID id, UINT32 ordinal)
{
    if (!DSL_Contract_Valid_Id(id))
        return NULL;

    const DSL_CONTRACT_RECORD &record = DSL_contract_registry[id - 1];
    if (ordinal >= record.required_checks.size())
        return NULL;

    return record.required_checks[ordinal];
}

const char *
DSL_Contract_Diagnostic_Code_At (DSL_CONTRACT_ID id, UINT32 ordinal)
{
    if (!DSL_Contract_Valid_Id(id))
        return NULL;

    const DSL_CONTRACT_RECORD &record = DSL_contract_registry[id - 1];
    if (ordinal >= record.diagnostic_codes.size())
        return NULL;

    return record.diagnostic_codes[ordinal];
}

void
DSL_Contract_fprint_registry (FILE *f)
{
    if (f == NULL)
        return;

    fprintf(f, "DSL Contract Registry: entries=%u\n", DSL_Contract_Count());
    for (UINT32 i = 0; i < DSL_contract_registry.size(); ++i) {
        const DSL_CONTRACT_RECORD &record = DSL_contract_registry[i];
        fprintf(f,
	    "  [%u] id=%u name=%s source=%u target=%u version=%u flags=0x%x\n",
	    i,
	    record.id,
	    record.name,
	    record.source_domain_id,
	    record.target_domain_id,
	    record.version,
	    record.flags);

        for (UINT32 j = 0; j < record.required_checks.size(); ++j)
            fprintf(f, "      check[%u]=%s\n", j, record.required_checks[j]);

        for (UINT32 j = 0; j < record.diagnostic_codes.size(); ++j)
            fprintf(f, "      diagnostic[%u]=%s\n", j, record.diagnostic_codes[j]);
    }
}
