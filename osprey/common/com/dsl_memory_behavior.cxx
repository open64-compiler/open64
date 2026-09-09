/*
 * Copyright (C) 2026 Open64 Project
 */

#include <string.h>

#include "dsl_memory_behavior.h"
#include "strtab.h"

typedef struct {
    DSL_OPERATOR dsl_operator;
    UINT16 version;
    UINT16 operand_count;
    UINT32 operand_flags[3];
    UINT32 result_flags;
    INT16 related_operand;
} DSL_MEMORY_BEHAVIOR_SEED;

static const DSL_MEMORY_BEHAVIOR_SEED DSL_memory_behavior_seed[] = {
    { OPR_DSLTENSORCONST, 1, 0, { 0, 0, 0 },
      DSL_MEMORY_BEHAVIOR_FRESH_RESULT |
      DSL_MEMORY_BEHAVIOR_UNIQUE_OWNERSHIP, -1 },
    { OPR_DSLADD, 1, 2,
      { DSL_MEMORY_BEHAVIOR_READ, DSL_MEMORY_BEHAVIOR_READ, 0 },
      DSL_MEMORY_BEHAVIOR_FRESH_RESULT |
      DSL_MEMORY_BEHAVIOR_UNIQUE_OWNERSHIP, -1 },
    { OPR_DSLRESHAPE, 1, 1, { DSL_MEMORY_BEHAVIOR_READ, 0, 0 },
      DSL_MEMORY_BEHAVIOR_VIEW | DSL_MEMORY_BEHAVIOR_SHARED_OWNERSHIP, 0 },
    { OPR_DSLTRANSPOSE, 1, 1, { DSL_MEMORY_BEHAVIOR_READ, 0, 0 },
      DSL_MEMORY_BEHAVIOR_VIEW | DSL_MEMORY_BEHAVIOR_SHARED_OWNERSHIP, 0 },
    { OPR_DSLATTENTION, 2, 3,
      { DSL_MEMORY_BEHAVIOR_READ, DSL_MEMORY_BEHAVIOR_READ,
        DSL_MEMORY_BEHAVIOR_READ },
      DSL_MEMORY_BEHAVIOR_FRESH_RESULT |
      DSL_MEMORY_BEHAVIOR_UNIQUE_OWNERSHIP, -1 },
    { OPR_DSLSCATTER, 1, 3,
      { DSL_MEMORY_BEHAVIOR_MODIFY |
        DSL_MEMORY_BEHAVIOR_UNIQUE_OWNERSHIP,
        DSL_MEMORY_BEHAVIOR_READ,
        DSL_MEMORY_BEHAVIOR_READ },
      DSL_MEMORY_BEHAVIOR_INPLACE_UPDATE |
      DSL_MEMORY_BEHAVIOR_UNIQUE_OWNERSHIP, 0 }
};

BOOL
DSL_Memory_Behavior_Get_Contract
        (DSL_OPERATOR dsl_operator,
         UINT16 version,
         DSL_MEMORY_BEHAVIOR_CONTRACT *contract)
{
    if (contract == NULL)
        return FALSE;
    memset(contract, 0, sizeof(*contract));
    contract->related_operand = -1;

    for (UINT32 i = 0;
         i < sizeof(DSL_memory_behavior_seed) /
             sizeof(DSL_memory_behavior_seed[0]); ++i) {
        const DSL_MEMORY_BEHAVIOR_SEED &seed = DSL_memory_behavior_seed[i];
        if (seed.dsl_operator != dsl_operator || seed.version != version)
            continue;
        contract->dsl_operator = seed.dsl_operator;
        contract->version = seed.version;
        contract->operand_count = seed.operand_count;
        for (UINT32 kid = 0; kid < seed.operand_count; ++kid)
            contract->operand_flags[kid] = seed.operand_flags[kid];
        contract->result_flags = seed.result_flags;
        contract->related_operand = seed.related_operand;
        return TRUE;
    }
    return FALSE;
}

const char *
DSL_Memory_Behavior_Flag_Name (UINT32 flag)
{
    static const char *names[] = {
        "none", "read", "modify", "unique_ownership", "shared_ownership",
        "fresh_result", "view", "may_alias", "inplace_update", "consumes"
    };
    UINT32 bit = 0;
    while (flag > 1) {
        flag >>= 1;
        ++bit;
    }
    UINT32 index = flag == 0 ? 0 : bit + 1;
    return index < sizeof(names) / sizeof(names[0]) ?
           names[index] : "unknown";
}

BOOL
DSL_Tensor_Set_Unique_Ownership (ST_IDX st)
{
    if (ST_IDX_index(st) == 0 ||
        !TY_is_tensor_extension(ST_type(St_Table[st])))
        return FALSE;

    ST_tensor_bind_attribute
        (st, TY_tensor_schema_key_name(TY_TENSOR_SCHEMA_NO_ALIAS), "true");
    return TRUE;
}

BOOL
DSL_Tensor_Has_Unique_Ownership (ST_IDX st)
{
    const char *value;

    if (ST_IDX_index(st) == 0 ||
        !TY_is_tensor_extension(ST_type(St_Table[st])))
        return FALSE;

    value = ST_tensor_attribute
                (st, TY_tensor_schema_key_name(TY_TENSOR_SCHEMA_NO_ALIAS));
    return value != NULL && strcmp(value, "true") == 0;
}

ST_IDX
DSL_Tensor_Create_Result_Symbol
        (const char *name,
         TY_IDX ty,
         ST_SCLASS storage_class,
         ST_EXPORT export_class)
{
    if (!TY_is_tensor_extension(ty))
        return ST_IDX_ZERO;

    ST *st = New_ST();
    ST_Init (st, Save_Str(name == NULL ? "" : name), CLASS_VAR,
             storage_class, export_class, ty);
    ST_IDX st_idx = ST_st_idx(*st);
    Set_ST_is_temp_var(St_Table[st_idx]);
    if (!DSL_Tensor_Set_Unique_Ownership(st_idx))
        return ST_IDX_ZERO;

    return st_idx;
}
