/*
 * Copyright (C) 2026 Open64 Project
 */

#include <string.h>
#include <vector>

#include "opt_dsl.h"

static std::vector<WOPT_DSL_SEMANTIC_INFO> WOPT_dsl_semantic_info;

static BOOL
WOPT_DSL_Semantic_Info_Equal(const WOPT_DSL_SEMANTIC_INFO *left,
                             const WOPT_DSL_SEMANTIC_INFO *right)
{
  return left->logical_operator == right->logical_operator &&
         left->version == right->version &&
         left->flags == right->flags &&
         left->result_ty == right->result_ty &&
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
      info->version == 0 || info->reserved != 0)
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
