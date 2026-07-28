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

/* Append-only logical operators exposed to DSL-aware compiler code. */
typedef enum {
    OPR_DSLUNKNOWN = 0,
    OPR_DSLTENSORCONST = 1,
    OPR_DSLADD = 2,
    OPR_DSLMATMUL = 3,
    OPR_DSLMODELINPUT = 4,
    OPR_DSLRELU = 5,
    OPR_DSLFLATTEN = 6,
    OPR_DSLRESIDUALADD = 7,
    OPR_DSLLINEAR = 8,
    OPR_DSLOUTPUTLOGITS = 9,
    OPR_DSLCONV2D = 10,
    OPR_DSLBATCHNORMINFER = 11,
    OPR_DSLMAXPOOL2D = 12,
    OPR_DSLGLOBALAVGPOOL2D = 13,
    OPR_DSLRESHAPE = 14,
    OPR_DSLTRANSPOSE = 15,
    OPR_DSLTOKENEMBEDDING = 16,
    OPR_DSLRMSNORM = 17,
    OPR_DSLROTARYEMBEDDING = 18,
    OPR_DSLATTENTION = 19,
    OPR_DSLSWIGLU = 20,
    OPR_DSLSCATTER = 21,
    OPR_DSLMUL = 22,
    OPR_DSLDIV = 23,
    OPR_DSLREM = 24,
    OPR_DSLDIVREM = 25,
    OPR_DSLDIVPART = 26,
    OPR_DSLREMPART = 27
} DSL_OPERATOR;

typedef enum {
    DSL_ALGEBRAIC_IDENTITY_NONE = 0,
    DSL_ALGEBRAIC_IDENTITY_ZERO = 1,
    DSL_ALGEBRAIC_IDENTITY_ONE = 2
} DSL_ALGEBRAIC_IDENTITY;

typedef enum {
    DSL_ALGEBRAIC_SAFETY_NEVER = 0,
    DSL_ALGEBRAIC_SAFETY_EXACT = 1,
    DSL_ALGEBRAIC_SAFETY_INTEGER = 2,
    DSL_ALGEBRAIC_SAFETY_INTEGER_OR_FP_REASSOCIATE = 3,
    DSL_ALGEBRAIC_SAFETY_RELAXED_MATH = 4
} DSL_ALGEBRAIC_SAFETY;

typedef enum {
    DSL_ALGEBRAIC_RELATION_FACTOR = 0,
    DSL_ALGEBRAIC_RELATION_DISTRIBUTE = 1
} DSL_ALGEBRAIC_RELATION;

enum {
    DSL_ALGEBRAIC_ALLOW_COMMUTATION = 1U << 0,
    DSL_ALGEBRAIC_ALLOW_REASSOCIATION = 1U << 1
};

enum {
    DSL_ALGEBRAIC_OPERAND_11 = 1U << 0,
    DSL_ALGEBRAIC_OPERAND_12 = 1U << 1,
    DSL_ALGEBRAIC_OPERAND_21 = 1U << 2,
    DSL_ALGEBRAIC_OPERAND_22 = 1U << 3,
    DSL_ALGEBRAIC_OPERAND_ALL =
        DSL_ALGEBRAIC_OPERAND_11 | DSL_ALGEBRAIC_OPERAND_12 |
        DSL_ALGEBRAIC_OPERAND_21 | DSL_ALGEBRAIC_OPERAND_22
};

typedef struct {
    DSL_OPERATOR dsl_operator;
    UINT16 version;
    DSL_OPERATOR swap_equivalent;
    DSL_ALGEBRAIC_IDENTITY identity;
    DSL_ALGEBRAIC_SAFETY commutation_safety;
    DSL_ALGEBRAIC_SAFETY reassociation_safety;
    UINT32 flags;
} DSL_ALGEBRAIC_INFO;

typedef struct {
    DSL_OPERATOR outer_operator;
    UINT16 outer_version;
    DSL_OPERATOR inner_operator;
    UINT16 inner_version;
    DSL_ALGEBRAIC_RELATION relation;
    DSL_ALGEBRAIC_SAFETY safety;
    UINT32 operand_mask;
} DSL_ALGEBRAIC_RELATION_INFO;

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
    DSL_OPERATOR dsl_operator;
    const char *logical_name;
    const char *stable_name;
    UINT16 version;
    DSL_OPCODE_CATEGORY category;
    DSL_OPCODE_LEVEL level;
    mINT16 nkids;
    DSL_SHAPE_RULE shape_rule;
    DSL_EFFECT_MODEL effect_model;
    DSL_LOWERING_MODEL lowering_model;
    const char *diagnostic_prefix;
    const char *attribute_schema;
    UINT32 flags;
} DSL_OPERATOR_INFO;

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
extern BOOL DSL_Operator_Get_Info (DSL_OPERATOR dsl_operator,
                                   DSL_OPERATOR_INFO *info);
extern BOOL DSL_Operator_Get_Info_Version (DSL_OPERATOR dsl_operator,
                                           UINT16 version,
                                           DSL_OPERATOR_INFO *info);
extern DSL_OPERATOR DSL_Operator_Find (const char *stable_name,
                                       UINT32 stable_name_len,
                                       UINT16 version);
extern DSL_OPERATOR DSL_Operator_Find_Current (const char *stable_name,
                                               UINT32 stable_name_len);
extern const char *DSL_OPERATOR_name (DSL_OPERATOR dsl_operator);
extern BOOL DSL_Operator_Get_Algebraic_Info
                                (DSL_OPERATOR dsl_operator,
                                 UINT16 version,
                                 DSL_ALGEBRAIC_INFO *info);
extern BOOL DSL_Operator_Get_Swap_Equivalent
                                (DSL_OPERATOR dsl_operator,
                                 UINT16 version,
                                 DSL_OPERATOR *swap_equivalent);
extern BOOL DSL_Algebraic_Should_Swap_Binary_Operands
                                (DSL_OPERATOR dsl_operator,
                                 UINT16 version,
                                 BOOL integer_operands,
                                 BOOL equivalent_descriptors,
                                 BOOL kid0_is_constant,
                                 BOOL kid1_is_constant,
                                 const char *kid0_key,
                                 const char *kid1_key);
extern UINT32 DSL_Algebraic_Relation_Count (void);
extern BOOL DSL_Algebraic_Relation_At
                                (UINT32 ordinal,
                                 DSL_ALGEBRAIC_RELATION_INFO *info);
extern BOOL DSL_Algebraic_Relation_Get
                                (DSL_OPERATOR outer_operator,
                                 UINT16 outer_version,
                                 DSL_OPERATOR inner_operator,
                                 UINT16 inner_version,
                                 DSL_ALGEBRAIC_RELATION relation,
                                 DSL_ALGEBRAIC_RELATION_INFO *info);
extern UINT32 DSL_Opcode_Register_Common_Substrate (void);
extern UINT32 DSL_Opcode_Register_Transformer_Domain (void);
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
