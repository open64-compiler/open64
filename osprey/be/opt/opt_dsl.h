/*
 * Copyright (C) 2026 Open64 Project
 */

#ifndef opt_dsl_INCLUDED
#define opt_dsl_INCLUDED

#include "defs.h"
#include "dsl_opcode.h"
#include "symtab_idx.h"

typedef UINT32 WOPT_DSL_SEMANTIC_INFO_ID;

#define WOPT_DSL_SEMANTIC_INFO_INVALID_ID 0

enum {
  WOPT_DSL_SEMANTIC_PURE = 1U << 0,
  WOPT_DSL_SEMANTIC_PROJECTABLE = 1U << 1
};

/*
 * Runtime-only semantic identity for a logical DSL CODEREP.  Source
 * positions, diagnostics, profiling, and reconstruction provenance do not
 * participate in this record because they must not inhibit WOPT value
 * equivalence.
 */
typedef struct {
  WOPT_DSL_SEMANTIC_INFO_ID id;
  DSL_OPERATOR logical_operator;
  UINT32 version;
  UINT32 flags;
  TY_IDX result_ty;
  UINT32 reserved;
  UINT64 canonical_attribute_hash;
  UINT64 operand_descriptor_hash;
  UINT64 effect_identity;
} WOPT_DSL_SEMANTIC_INFO;

typedef char WOPT_DSL_Semantic_Info_Id_Size_Check
    [sizeof(WOPT_DSL_SEMANTIC_INFO_ID) == 4 ? 1 : -1];
typedef char WOPT_DSL_Semantic_Info_Size_Check
    [sizeof(WOPT_DSL_SEMANTIC_INFO) == 48 ? 1 : -1];

extern void WOPT_DSL_Semantic_Info_Reset(void);
extern WOPT_DSL_SEMANTIC_INFO_ID WOPT_DSL_Semantic_Info_Intern
    (const WOPT_DSL_SEMANTIC_INFO *info);
extern BOOL WOPT_DSL_Semantic_Info_Get
    (WOPT_DSL_SEMANTIC_INFO_ID id, WOPT_DSL_SEMANTIC_INFO *info);
extern UINT32 WOPT_DSL_Semantic_Info_Hash
    (WOPT_DSL_SEMANTIC_INFO_ID id);
extern UINT32 WOPT_DSL_Semantic_Info_Count(void);

#endif /* opt_dsl_INCLUDED */
