/*
 * Copyright (C) 2026 Open64 Project
 */

#ifndef dsl_opcode_INCLUDED
#define dsl_opcode_INCLUDED

#include <stdio.h>

#include "defs.h"
#include "dsl_domain.h"

#define DSL_OPCODE_INVALID_ID 0
#define DSL_OPCODE_PROMOTION_INVALID_ID 0
#define DSL_OPCODE_NKIDS_VARIADIC (-1)
#define DSL_OPCODE_NKIDS_PAYLOAD_DEFINED (-2)

typedef UINT32 DSL_OPCODE_ID;
typedef UINT32 DSL_OPCODE_PROMOTION_ID;

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

typedef enum {
    DSL_OPCODE_PROMOTION_DOMAIN_ONLY = 0,
    DSL_OPCODE_PROMOTION_WRAPPER_TO_COMMON = 1,
    DSL_OPCODE_PROMOTION_PARTIAL = 2,
    DSL_OPCODE_PROMOTION_COMMON_NATIVE = 3
} DSL_OPCODE_PROMOTION_STATE;

typedef enum {
    DSL_CPROM_PROMOTION_CANDIDATE_MISSING_CROSS_DOMAIN_EVIDENCE = 0,
    DSL_CPROM_DOMAIN_SPECIFIC_SEMANTIC_LEAK = 1,
    DSL_CPROM_PROMOTED_OP_MISSING_SHAPE_FORMULA = 2,
    DSL_CPROM_PROMOTED_OP_MISSING_EFFECT_MODEL = 3,
    DSL_CPROM_DOMAIN_WRAPPER_REQUIRED = 4,
    DSL_CPROM_PROMOTION_WOULD_HIDE_GATEKEEPER_CHECK = 5,
    DSL_CPROM_LOWERING_CONFLICT_ACROSS_DOMAINS = 6,
    DSL_CPROM_TRAIT_MISMATCH_ACROSS_PROMOTION_SOURCES = 7,
    DSL_CPROM_COMMON_OP_VERSION_MISMATCH = 8,
    DSL_CPROM_DOMAIN_WRAPPER_MISSING_AFTER_PROMOTION = 9
} DSL_CPROM_DIAGNOSTIC;

typedef struct {
    DSL_OPCODE_ID id;
    DSL_DOMAIN_ID owner_domain_id;
    DSL_OPCODE_ID wrapper_target_id;
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

typedef struct {
    DSL_OPCODE_PROMOTION_ID id;
    DSL_OPCODE_ID source_opcode_id;
    DSL_OPCODE_ID promoted_opcode_id;
    DSL_OPCODE_PROMOTION_STATE state;
    UINT16 version;
    UINT32 required_common_semantics_count;
    UINT32 retained_wrapper_count;
    UINT32 required_verifier_check_count;
    UINT32 diagnostic_count;
    UINT32 flags;
} DSL_OPCODE_PROMOTION_INFO;

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
extern DSL_OPCODE_ID DSL_Opcode_Register_Domain_Wrapper
				(DSL_DOMAIN_ID owner_domain_id,
				 const char *name,
				 UINT16 version,
				 DSL_OPCODE_ID wrapper_target_id,
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
extern UINT32 DSL_Opcode_Register_Domain_Wrapper_Examples (void);
extern DSL_OPCODE_ID DSL_Opcode_Wrapper_Target (DSL_OPCODE_ID id);
extern void DSL_Opcode_Promotion_Registry_Reset (void);
extern DSL_OPCODE_PROMOTION_ID DSL_Opcode_Promotion_Register
				(DSL_OPCODE_ID source_opcode_id,
				 DSL_OPCODE_ID promoted_opcode_id,
				 DSL_OPCODE_PROMOTION_STATE state,
				 UINT16 version,
				 const char *const *required_common_semantics,
				 UINT32 required_common_semantics_count,
				 const char *const *retained_wrappers,
				 UINT32 retained_wrapper_count,
				 const char *const *required_verifier_checks,
				 UINT32 required_verifier_check_count,
				 const char *const *diagnostics,
				 UINT32 diagnostic_count,
				 UINT32 flags);
extern DSL_OPCODE_PROMOTION_ID DSL_Opcode_Promotion_Find
				(DSL_OPCODE_ID source_opcode_id,
				 DSL_OPCODE_ID promoted_opcode_id);
extern BOOL DSL_Opcode_Promotion_Get_Info
				(DSL_OPCODE_PROMOTION_ID id,
				 DSL_OPCODE_PROMOTION_INFO *info);
extern UINT32 DSL_Opcode_Promotion_Count (void);
extern BOOL DSL_Opcode_Promotion_At (UINT32 ordinal,
				     DSL_OPCODE_PROMOTION_INFO *info);
extern const char *DSL_Opcode_Promotion_Required_Common_Semantic_At
				(DSL_OPCODE_PROMOTION_ID id,
				 UINT32 ordinal);
extern const char *DSL_Opcode_Promotion_Retained_Wrapper_At
				(DSL_OPCODE_PROMOTION_ID id,
				 UINT32 ordinal);
extern const char *DSL_Opcode_Promotion_Required_Verifier_Check_At
				(DSL_OPCODE_PROMOTION_ID id,
				 UINT32 ordinal);
extern const char *DSL_Opcode_Promotion_Diagnostic_At
				(DSL_OPCODE_PROMOTION_ID id,
				 UINT32 ordinal);
extern UINT32 DSL_Opcode_Promotion_Register_Examples (void);
extern const char *DSL_Opcode_Check_Promotion
				(DSL_OPCODE_ID source_opcode_id,
				 DSL_OPCODE_ID promoted_opcode_id,
				 UINT16 common_version,
				 BOOL require_wrapper);
extern const char *DSL_Opcode_Category_Name (DSL_OPCODE_CATEGORY category);
extern const char *DSL_Opcode_Level_Name (DSL_OPCODE_LEVEL level);
extern const char *DSL_Shape_Rule_Name (DSL_SHAPE_RULE shape_rule);
extern const char *DSL_Effect_Model_Name (DSL_EFFECT_MODEL effect_model);
extern const char *DSL_Lowering_Model_Name
				(DSL_LOWERING_MODEL lowering_model);
extern const char *DSL_Opcode_Promotion_State_Name
				(DSL_OPCODE_PROMOTION_STATE state);
extern const char *DSL_CPROM_Diagnostic_Code
				(DSL_CPROM_DIAGNOSTIC diagnostic);
extern void DSL_Opcode_fprint_registry (FILE *f);
extern void DSL_Opcode_Promotion_fprint_registry (FILE *f);

#endif /* dsl_opcode_INCLUDED */
