/*
 * Copyright (C) 2026 Open64 Project
 */

#include <string.h>
#include <vector>

#include "dsl_opcode.h"

struct DSL_OPCODE_RECORD {
  DSL_OPCODE_ID id;
  DSL_DOMAIN_ID owner_domain_id;
  const char *name;
  UINT16 version;
  DSL_OPCODE_CATEGORY category;
  DSL_OPCODE_LEVEL level;
  mINT16 nkids;
  DSL_SHAPE_RULE shape_rule;
  DSL_EFFECT_MODEL effect_model;
  DSL_LOWERING_MODEL lowering_model;
  const char *diagnostic_prefix;
  UINT32 flags;

  DSL_OPCODE_RECORD() :
    id(DSL_OPCODE_INVALID_ID),
    owner_domain_id(DSL_DOMAIN_INVALID_ID),
    name(NULL),
    version(0),
    category(DSL_OPCODE_CATEGORY_EXECUTABLE),
    level(DSL_OPCODE_LEVEL_0_CORE),
    nkids(0),
    shape_rule(DSL_SHAPE_RULE_OPAQUE),
    effect_model(DSL_EFFECT_MODEL_PURE),
    lowering_model(DSL_LOWERING_MODEL_MARKER_ONLY),
    diagnostic_prefix(NULL),
    flags(0) {}
};

static std::vector<DSL_OPCODE_RECORD> Dsl_opcode_registry;

static const char *
DSL_Opcode_Save_String (const char *str)
{
  const char *safe_str = str ? str : "";
  size_t len = strlen(safe_str) + 1;
  char *saved = new char[len];

  memcpy(saved, safe_str, len);
  return saved;
}

static BOOL
DSL_Opcode_Valid_Id (DSL_OPCODE_ID id)
{
  return id != DSL_OPCODE_INVALID_ID && id <= Dsl_opcode_registry.size();
}

static BOOL
DSL_Opcode_Valid_Category (DSL_OPCODE_CATEGORY category)
{
  return category >= DSL_OPCODE_CATEGORY_EXECUTABLE &&
	 category <= DSL_OPCODE_CATEGORY_LOWERING_POLICY;
}

static BOOL
DSL_Opcode_Valid_Level (DSL_OPCODE_LEVEL level)
{
  return level >= DSL_OPCODE_LEVEL_0_CORE &&
	 level <= DSL_OPCODE_LEVEL_4_RUNTIME;
}

static BOOL
DSL_Opcode_Valid_Shape_Rule (DSL_SHAPE_RULE shape_rule)
{
  return shape_rule >= DSL_SHAPE_RULE_OPAQUE &&
	 shape_rule <= DSL_SHAPE_RULE_RUNTIME_GUARDED;
}

static BOOL
DSL_Opcode_Valid_Effect_Model (DSL_EFFECT_MODEL effect_model)
{
  return effect_model >= DSL_EFFECT_MODEL_PURE &&
	 effect_model <= DSL_EFFECT_MODEL_RUNTIME_EFFECT;
}

static BOOL
DSL_Opcode_Valid_Lowering_Model (DSL_LOWERING_MODEL lowering_model)
{
  return lowering_model >= DSL_LOWERING_MODEL_MARKER_ONLY &&
	 lowering_model <= DSL_LOWERING_MODEL_TARGET_SPECIFIC;
}

static void
DSL_Opcode_Free_Record (DSL_OPCODE_RECORD &record)
{
  delete [] record.name;
  delete [] record.diagnostic_prefix;
}

void
DSL_Opcode_Registry_Reset (void)
{
  for (UINT32 i = 0; i < Dsl_opcode_registry.size(); ++i)
    DSL_Opcode_Free_Record(Dsl_opcode_registry[i]);

  Dsl_opcode_registry.clear();
}

DSL_OPCODE_ID
DSL_Opcode_Register (DSL_DOMAIN_ID owner_domain_id,
		     const char *name,
		     UINT16 version,
		     DSL_OPCODE_CATEGORY category,
		     DSL_OPCODE_LEVEL level,
		     mINT16 nkids,
		     DSL_SHAPE_RULE shape_rule,
		     DSL_EFFECT_MODEL effect_model,
		     DSL_LOWERING_MODEL lowering_model,
		     const char *diagnostic_prefix,
		     UINT32 flags)
{
  const char *safe_name = name ? name : "";
  DSL_OPCODE_ID existing =
    DSL_Opcode_Find(owner_domain_id, safe_name, version);

  if (safe_name[0] == '\0' || version == 0)
    return DSL_OPCODE_INVALID_ID;

  if (existing != DSL_OPCODE_INVALID_ID)
    return existing;

  if (!DSL_Domain_Get_Info(owner_domain_id, NULL) ||
      !DSL_Opcode_Valid_Category(category) ||
      !DSL_Opcode_Valid_Level(level) ||
      !DSL_Opcode_Valid_Shape_Rule(shape_rule) ||
      !DSL_Opcode_Valid_Effect_Model(effect_model) ||
      !DSL_Opcode_Valid_Lowering_Model(lowering_model))
    return DSL_OPCODE_INVALID_ID;

  DSL_OPCODE_RECORD record;
  record.id = Dsl_opcode_registry.size() + 1;
  record.owner_domain_id = owner_domain_id;
  record.name = DSL_Opcode_Save_String(safe_name);
  record.version = version;
  record.category = category;
  record.level = level;
  record.nkids = nkids;
  record.shape_rule = shape_rule;
  record.effect_model = effect_model;
  record.lowering_model = lowering_model;
  record.diagnostic_prefix = DSL_Opcode_Save_String(diagnostic_prefix);
  record.flags = flags;

  Dsl_opcode_registry.push_back(record);
  return record.id;
}

DSL_OPCODE_ID
DSL_Opcode_Find (DSL_DOMAIN_ID owner_domain_id,
		 const char *name,
		 UINT16 version)
{
  const char *safe_name = name ? name : "";

  for (UINT32 i = 0; i < Dsl_opcode_registry.size(); ++i) {
    const DSL_OPCODE_RECORD &record = Dsl_opcode_registry[i];
    if (record.owner_domain_id == owner_domain_id &&
	record.version == version &&
	strcmp(record.name, safe_name) == 0)
      return record.id;
  }

  return DSL_OPCODE_INVALID_ID;
}

BOOL
DSL_Opcode_Get_Info (DSL_OPCODE_ID id, DSL_OPCODE_INFO *info)
{
  if (!DSL_Opcode_Valid_Id(id))
    return FALSE;

  if (info != NULL) {
    const DSL_OPCODE_RECORD &record = Dsl_opcode_registry[id - 1];
    info->id = record.id;
    info->owner_domain_id = record.owner_domain_id;
    info->name = record.name;
    info->version = record.version;
    info->category = record.category;
    info->level = record.level;
    info->nkids = record.nkids;
    info->shape_rule = record.shape_rule;
    info->effect_model = record.effect_model;
    info->lowering_model = record.lowering_model;
    info->diagnostic_prefix = record.diagnostic_prefix;
    info->flags = record.flags;
  }

  return TRUE;
}

UINT32
DSL_Opcode_Count (void)
{
  return Dsl_opcode_registry.size();
}

BOOL
DSL_Opcode_At (UINT32 ordinal, DSL_OPCODE_INFO *info)
{
  if (ordinal >= Dsl_opcode_registry.size())
    return FALSE;

  return DSL_Opcode_Get_Info(Dsl_opcode_registry[ordinal].id, info);
}

const char *
DSL_Opcode_Category_Name (DSL_OPCODE_CATEGORY category)
{
  switch (category) {
  case DSL_OPCODE_CATEGORY_EXECUTABLE:
    return "executable";
  case DSL_OPCODE_CATEGORY_DECLARATION:
    return "declaration";
  case DSL_OPCODE_CATEGORY_CONTRACT:
    return "contract";
  case DSL_OPCODE_CATEGORY_VERIFIER:
    return "verifier";
  case DSL_OPCODE_CATEGORY_LOWERING_POLICY:
    return "lowering_policy";
  }

  return "unknown";
}

const char *
DSL_Opcode_Level_Name (DSL_OPCODE_LEVEL level)
{
  switch (level) {
  case DSL_OPCODE_LEVEL_0_CORE:
    return "level0_core";
  case DSL_OPCODE_LEVEL_1_TENSOR:
    return "level1_tensor";
  case DSL_OPCODE_LEVEL_2_NUMERIC:
    return "level2_numeric";
  case DSL_OPCODE_LEVEL_3_NN_COMMON:
    return "level3_nn_common";
  case DSL_OPCODE_LEVEL_4_RUNTIME:
    return "level4_runtime";
  }

  return "unknown";
}

const char *
DSL_Shape_Rule_Name (DSL_SHAPE_RULE shape_rule)
{
  switch (shape_rule) {
  case DSL_SHAPE_RULE_OPAQUE:
    return "opaque";
  case DSL_SHAPE_RULE_IDENTITY:
    return "identity";
  case DSL_SHAPE_RULE_BROADCAST:
    return "broadcast";
  case DSL_SHAPE_RULE_CONTRACTION:
    return "contraction";
  case DSL_SHAPE_RULE_REDUCTION:
    return "reduction";
  case DSL_SHAPE_RULE_VIEW:
    return "view";
  case DSL_SHAPE_RULE_LAYOUT:
    return "layout";
  case DSL_SHAPE_RULE_RUNTIME_GUARDED:
    return "runtime_guarded";
  }

  return "unknown";
}

const char *
DSL_Effect_Model_Name (DSL_EFFECT_MODEL effect_model)
{
  switch (effect_model) {
  case DSL_EFFECT_MODEL_PURE:
    return "pure";
  case DSL_EFFECT_MODEL_VERIFIER_ONLY:
    return "verifier_only";
  case DSL_EFFECT_MODEL_DECLARATION_ONLY:
    return "declaration_only";
  case DSL_EFFECT_MODEL_LOWERING_POLICY:
    return "lowering_policy";
  case DSL_EFFECT_MODEL_RUNTIME_EFFECT:
    return "runtime_effect";
  }

  return "unknown";
}

const char *
DSL_Lowering_Model_Name (DSL_LOWERING_MODEL lowering_model)
{
  switch (lowering_model) {
  case DSL_LOWERING_MODEL_MARKER_ONLY:
    return "marker_only";
  case DSL_LOWERING_MODEL_CANONICAL_WHIRL:
    return "canonical_whirl";
  case DSL_LOWERING_MODEL_RUNTIME_CALL:
    return "runtime_call";
  case DSL_LOWERING_MODEL_INTRINSIC_SEQUENCE:
    return "intrinsic_sequence";
  case DSL_LOWERING_MODEL_TARGET_SPECIFIC:
    return "target_specific";
  }

  return "unknown";
}

void
DSL_Opcode_fprint_registry (FILE *f)
{
  if (f == NULL)
    return;

  fprintf(f, "DSL Opcode Registry: entries=%u\n", DSL_Opcode_Count());
  for (UINT32 i = 0; i < Dsl_opcode_registry.size(); ++i) {
    const DSL_OPCODE_RECORD &record = Dsl_opcode_registry[i];
    fprintf(f,
	    "  [%u] id=%u name=%s owner=%u version=%u category=%s level=%s "
	    "nkids=%d shape=%s effect=%s lowering=%s diagnostic_prefix=%s "
	    "flags=0x%x\n",
	    i,
	    record.id,
	    record.name,
	    record.owner_domain_id,
	    record.version,
	    DSL_Opcode_Category_Name(record.category),
	    DSL_Opcode_Level_Name(record.level),
	    record.nkids,
	    DSL_Shape_Rule_Name(record.shape_rule),
	    DSL_Effect_Model_Name(record.effect_model),
	    DSL_Lowering_Model_Name(record.lowering_model),
	    record.diagnostic_prefix,
	    record.flags);
  }
}
