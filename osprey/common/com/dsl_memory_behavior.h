/*
 * Copyright (C) 2026 Open64 Project
 */

/*
 * common/com exposes a fixed logical memory-contract inventory and shared
 * tensor-result symbol/attribute helpers. Caller phases own scope, alias
 * legality, source positions and lowering; these services neither select a PU
 * nor allocate runtime tensor storage or change binary WHIRL representation.
 * Design: doc/WHIRL-DSL-INFRASTRUCTURE.md, native result/ownership contract.
 */

#ifndef dsl_memory_behavior_INCLUDED
#define dsl_memory_behavior_INCLUDED

#include "defs.h"
#include "dsl_opcode.h"
#include "symtab.h"

#define DSL_MEMORY_BEHAVIOR_MAX_OPERANDS 8

typedef enum {
    DSL_MEMORY_BEHAVIOR_NONE = 0,
    DSL_MEMORY_BEHAVIOR_READ = 0x0001,
    DSL_MEMORY_BEHAVIOR_MODIFY = 0x0002,
    DSL_MEMORY_BEHAVIOR_UNIQUE_OWNERSHIP = 0x0004,
    DSL_MEMORY_BEHAVIOR_SHARED_OWNERSHIP = 0x0008,
    DSL_MEMORY_BEHAVIOR_FRESH_RESULT = 0x0010,
    DSL_MEMORY_BEHAVIOR_VIEW = 0x0020,
    DSL_MEMORY_BEHAVIOR_MAY_ALIAS = 0x0040,
    DSL_MEMORY_BEHAVIOR_INPLACE_UPDATE = 0x0080,
    DSL_MEMORY_BEHAVIOR_CONSUMES = 0x0100
} DSL_MEMORY_BEHAVIOR_FLAG;

typedef struct {
    DSL_OPERATOR dsl_operator;
    UINT16 version;
    UINT16 operand_count;
    UINT32 operand_flags[DSL_MEMORY_BEHAVIOR_MAX_OPERANDS];
    UINT32 result_flags;
    INT16 related_operand;
    UINT16 reserved;
} DSL_MEMORY_BEHAVIOR_CONTRACT;

/* Copy the exact seeded operator/version contract to required output. Clear
 * it first and set related_operand=-1, retaining that state on a missing seed;
 * NULL rejects without writes. This inventory is not a complete registry,
 * dynamic operand/effect/alias verifier, or permission to transform live IR. */
extern BOOL DSL_Memory_Behavior_Get_Contract
                                (DSL_OPERATOR dsl_operator,
                                 UINT16 version,
                                 DSL_MEMORY_BEHAVIOR_CONTRACT *contract);
/* Borrow a static spelling: zero maps to "none", otherwise name the highest
 * set bit if recognized, else "unknown". Combinations are not rejected or
 * printed as a full mask; callers wanting individual flags must split them.
 * Do not free the returned pointer; no contract/registry mutation. */
extern const char *DSL_Memory_Behavior_Flag_Name (UINT32 flag);
/* In the caller-established current symbol-table scope, create a tensor
 * CLASS_VAR temporary, copy name (NULL becomes empty), and bind no_alias=true.
 * Require coherent live type/ST/string tables and a legally unique result;
 * no canonical-descriptor, storage-class, alias or ownership proof is added.
 * Non-tensor TY returns zero before creation; a later failure can retain ST/
 * string/attribute allocations, without rollback. Caller must propagate source
 * position explicitly and retain table lifetime; no WN or runtime allocation. */
extern ST_IDX DSL_Tensor_Create_Result_Symbol
                                (const char *name,
                                 TY_IDX ty,
                                 ST_SCLASS storage_class,
                                 ST_EXPORT export_class);
/* Bind the semantic symbol attribute no_alias="true", not the baseline
 * pointer-only ST_PT_TO_UNIQUE_MEM flag. Zero ordinal/non-tensor rejects before
 * binding; every other nonzero ST requires a caller-valid resident scope/index.
 * TRUE follows the void binding call without readback, alias analysis or
 * allocation-exception rollback. Caller establishes that the claim is legal. */
extern BOOL DSL_Tensor_Set_Unique_Ownership (ST_IDX st);
/* Read exactly the bound symbol spelling "true" for a tensor ST. Zero ordinal,
 * non-tensor, absent or differently spelled value returns FALSE. Require valid
 * resident context for nonzero ST; this reads an annotation, not an independent
 * alias/liveness/physical ownership proof. No mutation or PU activation. */
extern BOOL DSL_Tensor_Has_Unique_Ownership (ST_IDX st);

#endif /* dsl_memory_behavior_INCLUDED */
