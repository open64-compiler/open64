/*
 * Copyright (C) 2026 Open64 Project
 */

/*
 * common/com implements immutable memory-behavior seeds and tensor temporary/
 * no-alias attribute mechanisms in the caller's compiler-table context. The
 * owning phase supplies legality and lifetime; no graph analysis, storage
 * lowering, PU selection or mapped-image format change is performed here.
 * Design: doc/WHIRL-DSL-INFRASTRUCTURE.md, native result/ownership contract.
 */

#include <string.h>

#include "dsl_memory_behavior.h"
#include "strtab.h"

/* Private process-local seed, not a persisted table row. Only the reviewed
 * subset below has a memory contract; absence does not imply no memory effect. */
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

/* Initialize required output to an empty contract with no related operand,
 * then copy the exact seed and its trusted operand prefix. A missing seed
 * returns FALSE without restoring prior output. No registry consultation,
 * allocation or live-tree/effect/alias verification; NULL cannot probe support. */
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

/* Map zero or the highest set bit to an immutable name. The shift loop
 * does not validate a single-bit input: combined flags name only
 * their highest bit. Unknown high bits map to "unknown"; no writes or ownership
 * transfer, and this is not a complete-mask diagnostic formatter. */
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

/* Reject a null ordinal/non-tensor type, then bind the existing semantic
 * no-alias symbol attribute and return TRUE without independent readback.
 * Caller supplies resident, bounded ST/type context and legality of the claim;
 * this annotates, not proves, uniqueness. Binding can allocate table/string
 * state and no rollback or allocation-failure status is supplied here. */
BOOL
DSL_Tensor_Set_Unique_Ownership (ST_IDX st)
{
    if (ST_IDX_index(st) == 0 ||
        !TY_is_tensor_extension(ST_type(St_Table[st])))
        return FALSE;

    /* Preserve the tensor attribute protocol; baseline unique-memory flags
     * apply to a later pointer representation, not this tensor symbol. */
    ST_tensor_bind_attribute
        (st, TY_tensor_schema_key_name(TY_TENSOR_SCHEMA_NO_ALIAS), "true");
    return TRUE;
}

/* Read the current symbol annotation and compare exact lowercase "true".
 * Nonzero ST identities must already be valid in resident caller tables;
 * FALSE covers null ordinal, non-tensor or absent/different annotation.
 * Do not perform alias analysis, repair state or switch PU/symbol-table scope. */
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

/* Create a variable in the current ST scope after the tensor-kind gate,
 * intern its name, mark it temporary and request semantic unique ownership.
 * Invalid tensor kind returns zero before creation; late failure can retain
 * the new symbol/name/attributes, with no journal. Caller owns scope, alias
 * legality and declaration source-position propagation (not set explicitly
 * here). No canonical-type proof, WN, physical storage or runtime allocation. */
ST_IDX
DSL_Tensor_Create_Result_Symbol
        (const char *name,
         TY_IDX ty,
         ST_SCLASS storage_class,
         ST_EXPORT export_class)
{
    if (!TY_is_tensor_extension(ty))
        return ST_IDX_ZERO;

    /* The caller chose this scope and result role. From symbol allocation on,
     * there is no cleanup path that removes partially created table entries. */
    ST *st = New_ST();
    ST_Init (st, Save_Str(name == NULL ? "" : name), CLASS_VAR,
             storage_class, export_class, ty);
    ST_IDX st_idx = ST_st_idx(*st);
    Set_ST_is_temp_var(St_Table[st_idx]);
    if (!DSL_Tensor_Set_Unique_Ownership(st_idx))
        return ST_IDX_ZERO;

    return st_idx;
}
