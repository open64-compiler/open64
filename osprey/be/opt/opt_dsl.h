/*
 * Copyright (C) 2026 Open64 Project
 */

#ifndef opt_dsl_INCLUDED
#define opt_dsl_INCLUDED

#include "defs.h"
#include "dsl_opcode.h"
#include "symtab_idx.h"

class WN;

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
  TCON_IDX tensor_tcon_idx;
  UINT32 origin_node_id;
  UINT32 origin_result_value_id;
  UINT64 canonical_attribute_hash;
  UINT64 operand_descriptor_hash;
  UINT64 effect_identity;
} WOPT_DSL_SEMANTIC_INFO;

typedef char WOPT_DSL_Semantic_Info_Id_Size_Check
    [sizeof(WOPT_DSL_SEMANTIC_INFO_ID) == 4 ? 1 : -1];
typedef char WOPT_DSL_Semantic_Info_Size_Check
    [sizeof(WOPT_DSL_SEMANTIC_INFO) == 56 ? 1 : -1];

extern void WOPT_DSL_Semantic_Info_Reset(void);
extern WOPT_DSL_SEMANTIC_INFO_ID WOPT_DSL_Semantic_Info_Intern
    (const WOPT_DSL_SEMANTIC_INFO *info);
extern BOOL WOPT_DSL_Semantic_Info_Get
    (WOPT_DSL_SEMANTIC_INFO_ID id, WOPT_DSL_SEMANTIC_INFO *info);
extern UINT32 WOPT_DSL_Semantic_Info_Hash
    (WOPT_DSL_SEMANTIC_INFO_ID id);
extern UINT32 WOPT_DSL_Semantic_Info_Count(void);
extern BOOL WOPT_DSL_Algebraic_Safety_Allows
    (DSL_ALGEBRAIC_SAFETY safety, BOOL floating_point,
     BOOL reassociation_enabled);
extern BOOL WOPT_DSL_Import_Semantic_Info
    (const WN *wn, ST_IDX result_st, const char *owner_pu,
     WOPT_DSL_SEMANTIC_INFO *info, FILE *diagnostic);
extern BOOL WOPT_DSL_Create_Folded_Tensor_Info
    (const WOPT_DSL_SEMANTIC_INFO *origin, TCON_IDX result_tcon_idx,
     WOPT_DSL_SEMANTIC_INFO *result);
extern BOOL WOPT_DSL_Fold_Compact_Tensors
    (const WOPT_DSL_SEMANTIC_INFO *origin,
     const TCON_IDX *operand_tcon_idx, UINT32 operand_count,
     WOPT_DSL_SEMANTIC_INFO *result, FILE *diagnostic);
extern WN *WOPT_DSL_Emit_WN
    (const WOPT_DSL_SEMANTIC_INFO *info, const WN *original,
     ST_IDX result_st, WN **kids, UINT32 kid_count, FILE *diagnostic);

#endif /* opt_dsl_INCLUDED */
