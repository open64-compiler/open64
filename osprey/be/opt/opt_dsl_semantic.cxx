/*
 * Copyright (C) 2026 Open64 Project
 */

#include <vector>

#include "opt_dsl.h"

static std::vector<WOPT_DSL_SEMANTIC_INFO> WOPT_dsl_semantic_info;
static WOPT_DSL_DIVREM_TARGET_POLICY WOPT_dsl_divrem_target_policy;

static BOOL
WOPT_DSL_Semantic_Info_Equal(const WOPT_DSL_SEMANTIC_INFO *left,
                             const WOPT_DSL_SEMANTIC_INFO *right)
{
  return left->logical_operator == right->logical_operator &&
         left->version == right->version &&
         left->flags == right->flags &&
         left->result_ty == right->result_ty &&
         left->second_result_ty == right->second_result_ty &&
         left->tensor_tcon_idx == right->tensor_tcon_idx &&
         left->projection_kind == right->projection_kind &&
         left->canonical_attribute_hash ==
             right->canonical_attribute_hash &&
         left->operand_descriptor_hash ==
             right->operand_descriptor_hash &&
         left->effect_identity == right->effect_identity;
}

static UINT64
WOPT_DSL_Hash_Combine(UINT64 hash, UINT64 value)
{
  hash ^= value;
  hash *= 1099511628211ULL;
  return hash;
}

void
WOPT_DSL_Semantic_Info_Reset(void)
{
  WOPT_dsl_semantic_info.clear();
}

WOPT_DSL_SEMANTIC_INFO_ID
WOPT_DSL_Semantic_Info_Intern(const WOPT_DSL_SEMANTIC_INFO *info)
{
  if (info == NULL || info->logical_operator == OPR_DSLUNKNOWN ||
      info->version == 0)
    return WOPT_DSL_SEMANTIC_INFO_INVALID_ID;

  for (UINT32 i = 0; i < WOPT_dsl_semantic_info.size(); ++i) {
    if (WOPT_DSL_Semantic_Info_Equal
            (&WOPT_dsl_semantic_info[i], info))
      return i + 1;
  }

  WOPT_DSL_SEMANTIC_INFO copy = *info;
  copy.id = WOPT_dsl_semantic_info.size() + 1;
  WOPT_dsl_semantic_info.push_back(copy);
  return copy.id;
}

BOOL
WOPT_DSL_Semantic_Info_Get(WOPT_DSL_SEMANTIC_INFO_ID id,
                           WOPT_DSL_SEMANTIC_INFO *info)
{
  if (id == WOPT_DSL_SEMANTIC_INFO_INVALID_ID ||
      id > WOPT_dsl_semantic_info.size())
    return FALSE;
  if (info != NULL)
    *info = WOPT_dsl_semantic_info[id - 1];
  return TRUE;
}

UINT32
WOPT_DSL_Semantic_Info_Hash(WOPT_DSL_SEMANTIC_INFO_ID id)
{
  WOPT_DSL_SEMANTIC_INFO info;
  UINT64 hash = 1469598103934665603ULL;

  if (!WOPT_DSL_Semantic_Info_Get(id, &info))
    return 0;
  hash = WOPT_DSL_Hash_Combine(hash, info.logical_operator);
  hash = WOPT_DSL_Hash_Combine(hash, info.version);
  hash = WOPT_DSL_Hash_Combine(hash, info.flags);
  hash = WOPT_DSL_Hash_Combine(hash, info.result_ty);
  hash = WOPT_DSL_Hash_Combine(hash, info.second_result_ty);
  hash = WOPT_DSL_Hash_Combine(hash, info.tensor_tcon_idx);
  hash = WOPT_DSL_Hash_Combine(hash, info.projection_kind);
  hash = WOPT_DSL_Hash_Combine(hash, info.canonical_attribute_hash);
  hash = WOPT_DSL_Hash_Combine(hash, info.operand_descriptor_hash);
  hash = WOPT_DSL_Hash_Combine(hash, info.effect_identity);
  return (UINT32)(hash ^ (hash >> 32));
}

UINT32
WOPT_DSL_Semantic_Info_Count(void)
{
  return WOPT_dsl_semantic_info.size();
}

BOOL
WOPT_DSL_Algebraic_Safety_Allows
    (DSL_ALGEBRAIC_SAFETY safety, BOOL floating_point,
     BOOL reassociation_enabled)
{
  switch (safety) {
  case DSL_ALGEBRAIC_SAFETY_EXACT:
    return TRUE;
  case DSL_ALGEBRAIC_SAFETY_INTEGER:
    return !floating_point;
  case DSL_ALGEBRAIC_SAFETY_INTEGER_OR_FP_REASSOCIATE:
  case DSL_ALGEBRAIC_SAFETY_RELAXED_MATH:
    return !floating_point || reassociation_enabled;
  default:
    return FALSE;
  }
}

void
WOPT_DSL_Reset_DIVREM_Target_Policy(void)
{
  WOPT_dsl_divrem_target_policy.lowering_capability = FALSE;
  WOPT_dsl_divrem_target_policy.profitable = FALSE;
}

BOOL
WOPT_DSL_Set_DIVREM_Target_Policy
    (const WOPT_DSL_DIVREM_TARGET_POLICY *policy)
{
  if (policy == NULL)
    return FALSE;
  WOPT_dsl_divrem_target_policy = *policy;
  return TRUE;
}

BOOL
WOPT_DSL_Get_DIVREM_Target_Policy
    (WOPT_DSL_DIVREM_TARGET_POLICY *policy)
{
  if (policy == NULL)
    return FALSE;
  *policy = WOPT_dsl_divrem_target_policy;
  return TRUE;
}

BOOL
WOPT_DSL_DIVREM_Combination_Enabled(BOOL option_enabled)
{
  return option_enabled &&
         WOPT_dsl_divrem_target_policy.lowering_capability &&
         WOPT_dsl_divrem_target_policy.profitable;
}

BOOL
WOPT_DSL_Projectable_Info(const WOPT_DSL_SEMANTIC_INFO *info)
{
  return info != NULL &&
         info->logical_operator == OPR_DSLDIVREM &&
         (info->flags & WOPT_DSL_SEMANTIC_PROJECTABLE_ROOT) != 0 &&
         info->projection_kind == WOPT_DSL_PROJECTION_NONE;
}

BOOL
WOPT_DSL_Projection_Info(const WOPT_DSL_SEMANTIC_INFO *info)
{
  return info != NULL &&
         (info->logical_operator == OPR_DSLDIVPART ||
          info->logical_operator == OPR_DSLREMPART) &&
         (info->flags & WOPT_DSL_SEMANTIC_PROJECTION) != 0 &&
         (info->projection_kind == WOPT_DSL_PROJECTION_QUOTIENT ||
          info->projection_kind == WOPT_DSL_PROJECTION_REMAINDER);
}

BOOL
WOPT_DSL_Create_DIVREM_Semantics
    (const WOPT_DSL_SEMANTIC_INFO *standalone,
     WOPT_DSL_SEMANTIC_INFO *combined,
     WOPT_DSL_SEMANTIC_INFO *projection)
{
  WOPT_DSL_PROJECTION_KIND kind;

  if (standalone == NULL || combined == NULL || projection == NULL ||
      (standalone->logical_operator != OPR_DSLDIV &&
       standalone->logical_operator != OPR_DSLREM) ||
      standalone->version != 1 ||
      (standalone->flags & WOPT_DSL_SEMANTIC_PURE) == 0 ||
      standalone->result_ty == TY_IDX_ZERO)
    return FALSE;

  kind = standalone->logical_operator == OPR_DSLDIV ?
             WOPT_DSL_PROJECTION_QUOTIENT :
             WOPT_DSL_PROJECTION_REMAINDER;
  *combined = *standalone;
  combined->id = WOPT_DSL_SEMANTIC_INFO_INVALID_ID;
  combined->logical_operator = OPR_DSLDIVREM;
  combined->flags |= WOPT_DSL_SEMANTIC_PROJECTABLE |
                     WOPT_DSL_SEMANTIC_PROJECTABLE_ROOT;
  combined->flags &= ~WOPT_DSL_SEMANTIC_PROJECTION;
  combined->second_result_ty = standalone->result_ty;
  combined->projection_kind = WOPT_DSL_PROJECTION_NONE;

  *projection = *standalone;
  projection->id = WOPT_DSL_SEMANTIC_INFO_INVALID_ID;
  projection->logical_operator =
      kind == WOPT_DSL_PROJECTION_QUOTIENT ?
          OPR_DSLDIVPART : OPR_DSLREMPART;
  projection->flags |= WOPT_DSL_SEMANTIC_PROJECTABLE |
                       WOPT_DSL_SEMANTIC_PROJECTION;
  projection->flags &= ~WOPT_DSL_SEMANTIC_PROJECTABLE_ROOT;
  projection->second_result_ty = TY_IDX_ZERO;
  projection->projection_kind = kind;
  return TRUE;
}

BOOL
WOPT_DSL_Uncombine_Projection_Semantics
    (const WOPT_DSL_SEMANTIC_INFO *projection,
     WOPT_DSL_SEMANTIC_INFO *standalone)
{
  if (!WOPT_DSL_Projection_Info(projection) || standalone == NULL)
    return FALSE;
  *standalone = *projection;
  standalone->id = WOPT_DSL_SEMANTIC_INFO_INVALID_ID;
  standalone->logical_operator =
      projection->projection_kind == WOPT_DSL_PROJECTION_QUOTIENT ?
          OPR_DSLDIV : OPR_DSLREM;
  standalone->flags &= ~(WOPT_DSL_SEMANTIC_PROJECTABLE_ROOT |
                         WOPT_DSL_SEMANTIC_PROJECTION);
  standalone->second_result_ty = TY_IDX_ZERO;
  standalone->projection_kind = WOPT_DSL_PROJECTION_NONE;
  return TRUE;
}
