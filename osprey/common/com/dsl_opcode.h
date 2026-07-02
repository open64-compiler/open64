/*
 * Copyright (C) 2026 Open64 Project
 */

#ifndef dsl_opcode_INCLUDED
#define dsl_opcode_INCLUDED

#include <stdio.h>

#include "defs.h"
#include "dsl_domain.h"

#define DSL_OPCODE_INVALID_ID 0
#define DSL_OPCODE_NKIDS_VARIADIC (-1)
#define DSL_OPCODE_NKIDS_PAYLOAD_DEFINED (-2)

typedef UINT32 DSL_OPCODE_ID;

typedef enum {
  DSL_OPCODE_CATEGORY_EXECUTABLE = 0,
  DSL_OPCODE_CATEGORY_DECLARATION = 1,
  DSL_OPCODE_CATEGORY_CONTRACT = 2,
  DSL_OPCODE_CATEGORY_VERIFIER = 3,
  DSL_OPCODE_CATEGORY_LOWERING_POLICY = 4
} DSL_OPCODE_CATEGORY;

typedef enum {
  DSL_OPCODE_LEVEL_0_CORE = 0,
  DSL_OPCODE_LEVEL_1_TENSOR = 1,
  DSL_OPCODE_LEVEL_2_NUMERIC = 2,
  DSL_OPCODE_LEVEL_3_NN_COMMON = 3,
  DSL_OPCODE_LEVEL_4_RUNTIME = 4
} DSL_OPCODE_LEVEL;

typedef enum {
  DSL_SHAPE_RULE_OPAQUE = 0,
  DSL_SHAPE_RULE_IDENTITY = 1,
  DSL_SHAPE_RULE_BROADCAST = 2,
  DSL_SHAPE_RULE_CONTRACTION = 3,
  DSL_SHAPE_RULE_REDUCTION = 4,
  DSL_SHAPE_RULE_VIEW = 5,
  DSL_SHAPE_RULE_LAYOUT = 6,
  DSL_SHAPE_RULE_RUNTIME_GUARDED = 7
} DSL_SHAPE_RULE;

typedef enum {
  DSL_EFFECT_MODEL_PURE = 0,
  DSL_EFFECT_MODEL_VERIFIER_ONLY = 1,
  DSL_EFFECT_MODEL_DECLARATION_ONLY = 2,
  DSL_EFFECT_MODEL_LOWERING_POLICY = 3,
  DSL_EFFECT_MODEL_RUNTIME_EFFECT = 4
} DSL_EFFECT_MODEL;

typedef enum {
  DSL_LOWERING_MODEL_MARKER_ONLY = 0,
  DSL_LOWERING_MODEL_CANONICAL_WHIRL = 1,
  DSL_LOWERING_MODEL_RUNTIME_CALL = 2,
  DSL_LOWERING_MODEL_INTRINSIC_SEQUENCE = 3,
  DSL_LOWERING_MODEL_TARGET_SPECIFIC = 4
} DSL_LOWERING_MODEL;

typedef struct {
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
} DSL_OPCODE_INFO;

extern void DSL_Opcode_Registry_Reset (void);
extern DSL_OPCODE_ID DSL_Opcode_Register
				(DSL_DOMAIN_ID owner_domain_id,
				 const char *name,
				 UINT16 version,
				 DSL_OPCODE_CATEGORY category,
				 DSL_OPCODE_LEVEL level,
				 mINT16 nkids,
				 DSL_SHAPE_RULE shape_rule,
				 DSL_EFFECT_MODEL effect_model,
				 DSL_LOWERING_MODEL lowering_model,
				 const char *diagnostic_prefix,
				 UINT32 flags);
extern DSL_OPCODE_ID DSL_Opcode_Find (DSL_DOMAIN_ID owner_domain_id,
				      const char *name,
				      UINT16 version);
extern BOOL DSL_Opcode_Get_Info (DSL_OPCODE_ID id,
				 DSL_OPCODE_INFO *info);
extern UINT32 DSL_Opcode_Count (void);
extern BOOL DSL_Opcode_At (UINT32 ordinal, DSL_OPCODE_INFO *info);
extern UINT32 DSL_Opcode_Register_Common_Substrate (void);
extern const char *DSL_Opcode_Category_Name (DSL_OPCODE_CATEGORY category);
extern const char *DSL_Opcode_Level_Name (DSL_OPCODE_LEVEL level);
extern const char *DSL_Shape_Rule_Name (DSL_SHAPE_RULE shape_rule);
extern const char *DSL_Effect_Model_Name (DSL_EFFECT_MODEL effect_model);
extern const char *DSL_Lowering_Model_Name
				(DSL_LOWERING_MODEL lowering_model);
extern void DSL_Opcode_fprint_registry (FILE *f);

#endif /* dsl_opcode_INCLUDED */
