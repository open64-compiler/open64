/*
 * Copyright (C) 2026 Open64 Project
 */

/*
 * common/com exposes static, versioned logical operator semantics alongside
 * resettable process-local opcode/wrapper/promotion registries. Static strings
 * last for the process; dynamic strings and ID meanings expire on their owning
 * registry's reset. Callers coordinate access and retire dependent IDs before
 * domain reset. Metadata queries do not execute checks, lower IR or authorize
 * transformations; this header does not define mapped-image record layouts.
 * Design: doc/WHIRL-DSL-INFRASTRUCTURE.md;
 * reductions: doc/AI-COMPILER-NATIVE-REDUCTION-CONTRACT-v0.1.md.
 */

#ifndef dsl_opcode_INCLUDED
#define dsl_opcode_INCLUDED

#include <stdio.h>

#include "defs.h"
#include "dsl_domain.h"

#define DSL_OPCODE_INVALID_ID 0
#define DSL_OPCODE_PROMOTION_INVALID_ID 0
/* Arity metadata for a variable operand count, not a concrete kid count. */
#define DSL_OPCODE_NKIDS_VARIADIC (-1)
/* Arity must be interpreted by the payload contract rather than this sentinel. */
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
    OPR_DSLREMPART = 27,
    OPR_DSLCKKSADD = 28,
    OPR_DSLCKKSSUB = 29,
    OPR_DSLCKKSMUL = 30,
    OPR_DSLCKKSENCODE = 31,
    OPR_DSLCKKSROTATE = 32,
    OPR_DSLCKKSRESCALE = 33,
    OPR_DSLCKKSMODSWITCH = 34,
    OPR_DSLCKKSRELIN = 35,
    OPR_DSLCKKSBOOTSTRAP = 36,
    OPR_DSLREDUCESUM = 37,
    OPR_DSLSTABLESOFTMAX = 38,
    OPR_DSLMEANVARNORM = 39
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
    DSL_FUSION_ITERATION_OPAQUE = 0,
    DSL_FUSION_ITERATION_POINTWISE = 1,
    DSL_FUSION_ITERATION_CONTRACTION = 2,
    DSL_FUSION_ITERATION_REDUCTION = 3,
    DSL_FUSION_ITERATION_VIEW = 4
} DSL_FUSION_ITERATION_CLASS;

typedef enum {
    DSL_FUSION_INDEXING_OPAQUE = 0,
    DSL_FUSION_INDEXING_IDENTITY = 1,
    DSL_FUSION_INDEXING_BROADCAST = 2,
    DSL_FUSION_INDEXING_CONTRACTION = 3,
    DSL_FUSION_INDEXING_REDUCTION = 4,
    DSL_FUSION_INDEXING_VIEW = 5
} DSL_FUSION_INDEXING_CLASS;

enum {
    DSL_FUSIBILITY_NONE = 0,
    DSL_FUSIBILITY_PRODUCER = 1U << 0,
    DSL_FUSIBILITY_CONSUMER = 1U << 1,
    DSL_FUSIBILITY_CLUSTER_ANCHOR = 1U << 2,
    DSL_FUSIBILITY_EXACT_DESCRIPTOR = 1U << 3,
    DSL_FUSIBILITY_SINGLE_RESULT = 1U << 4,
    DSL_FUSIBILITY_SEMANTIC_PATTERN_REQUIRED = 1U << 5
};

/* Versioned static semantics used by generic, target-independent fusion. */
typedef struct {
    DSL_OPERATOR dsl_operator;
    UINT16 version;
    UINT16 reserved;
    UINT32 iteration_space;
    UINT32 operand_indexing;
    UINT32 flags;
} DSL_FUSIBILITY_INFO;

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

/*
 * Reset promotions first, then free opcode names/prefixes and entries. Dynamic
 * borrowed strings and ID meanings expire; numeric IDs may be reused. Domains
 * and static logical/algebraic/fusibility tables are not reset.
 */
extern void DSL_Opcode_Registry_Reset (void);

/*
 * Intern the exact owner/name/nonzero-version key, copying name and diagnostic
 * prefix (NULL prefix becomes empty). A duplicate preserves all old metadata
 * and bypasses owner/enum validation. New entries require a live owner and valid
 * category/level/shape/effect/lowering enums; nkids and flags are not checked.
 * Rejected input returns zero without mutation. Allocation exceptions are not
 * converted to error IDs and have no rollback guarantee.
 */
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
/*
 * Require a live target, then register using its semantic fields. Reusing an
 * existing key preserves its metadata: only an unset/same wrapper link can be
 * assigned. A different existing link returns zero unchanged. An unwrapped
 * duplicate can thus gain a link without refreshing its semantics. Targets
 * need not belong to common or be unwrapped; this API does not verify cycles.
 * Ordinary rejection leaves the registries unchanged; allocation exceptions
 * in registration have no rollback guarantee.
 */
extern DSL_OPCODE_ID DSL_Opcode_Register_Domain_Wrapper
                                (DSL_DOMAIN_ID owner_domain_id,
                                 const char *name,
                                 UINT16 version,
                                 DSL_OPCODE_ID wrapper_target_id,
                                 const char *diagnostic_prefix,
                                 UINT32 flags);
/*
 * Find an exact numeric owner/name/version key without owner revalidation or
 * mutation; null/empty names and missing keys return zero.
 */
extern DSL_OPCODE_ID DSL_Opcode_Find (DSL_DOMAIN_ID owner_domain_id,
                                      const char *name,
                                      UINT16 version);
/*
 * Copy a live entry's fields, borrowing name/prefix until opcode reset. NULL
 * output tests validity only; invalid IDs return FALSE without output writes.
 */
extern BOOL DSL_Opcode_Get_Info (DSL_OPCODE_ID id,
                                 DSL_OPCODE_INFO *info);
/* Read the dynamic opcode entry count without changing any table. */
extern UINT32 DSL_Opcode_Count (void);

/* Inspect a zero-based ordinal with Get_Info's null/borrowing/failure rules. */
extern BOOL DSL_Opcode_At (UINT32 ordinal, DSL_OPCODE_INFO *info);

/*
 * Copy the highest-version static seed for an operator, borrowing static
 * strings; NULL output tests presence. Unknown operators return FALSE without
 * writes. This is independent of dynamic registry population and reset.
 */
extern BOOL DSL_Operator_Get_Info (DSL_OPERATOR dsl_operator,
                                   DSL_OPERATOR_INFO *info);
/*
 * Query an exact static operator/version with Get_Info's borrowing/null rules.
 * No version upgrade occurs; a missing pair returns FALSE, output untouched.
 */
extern BOOL DSL_Operator_Get_Info_Version (DSL_OPERATOR dsl_operator,
                                           UINT16 version,
                                           DSL_OPERATOR_INFO *info);
/*
 * Match an exact static name byte span and nonzero version; the input must be
 * readable for its length, not necessarily NUL-terminated. Null/empty input or
 * no match returns OPR_DSLUNKNOWN without dynamic registration or mutation.
 */
extern DSL_OPERATOR DSL_Operator_Find (const char *stable_name,
                                       UINT32 stable_name_len,
                                       UINT16 version);
/*
 * Resolve a name span to the first static seed's operator, ignoring version;
 * this does not select a highest-version row. Use Get_Info on the returned
 * operator for that metadata. Find's input/absence rules apply without version.
 */
extern DSL_OPERATOR DSL_Operator_Find_Current (const char *stable_name,
                                               UINT32 stable_name_len);
/* Borrow a static logical spelling; invalid enum values use OPR_DSLUNKNOWN. */
extern const char *DSL_OPERATOR_name (DSL_OPERATOR dsl_operator);

/*
 * Copy an exact static algebraic trait; NULL output tests presence. Missing
 * pairs return FALSE without writes. Traits alone do not authorize a rewrite.
 */
extern BOOL DSL_Operator_Get_Algebraic_Info
                                (DSL_OPERATOR dsl_operator,
                                 UINT16 version,
                                 DSL_ALGEBRAIC_INFO *info);
/*
 * Require nonnull output and copy an exact static fusibility trait. On a
 * missing pair, zero the output and return FALSE (unlike algebraic lookup).
 * No candidate discovery, legality or profitability decision occurs here.
 */
extern BOOL DSL_Operator_Get_Fusibility_Info
                                (DSL_OPERATOR dsl_operator,
                                 UINT16 version,
                                 DSL_FUSIBILITY_INFO *info);
/* Borrow a static iteration-class spelling, or "unknown" outside its range. */
extern const char *DSL_fusion_iteration_class_name (UINT32 iteration_space);

/* Borrow a static indexing-class spelling, or "unknown" outside its range. */
extern const char *DSL_fusion_indexing_class_name (UINT32 operand_indexing);

/*
 * Query a commutation-enabled algebraic trait with a known swapped operator.
 * NULL output tests presence; failure leaves output untouched. This does not
 * check operand types/descriptors or establish that an actual swap is safe.
 */
extern BOOL DSL_Operator_Get_Swap_Equivalent
                                (DSL_OPERATOR dsl_operator,
                                 UINT16 version,
                                 DSL_OPERATOR *swap_equivalent);
/*
 * Read-only ordering predicate for caller-certified integer operands with
 * equivalent descriptors and a commutation-enabled integer-safe trait. Require
 * nonnull NUL-terminated keys. Put a sole constant on the right; otherwise swap
 * only for descending strcmp order. Failed prerequisites return FALSE; no WN
 * is changed and no floating-point reassociation permission is inferred.
 */
extern BOOL DSL_Algebraic_Should_Swap_Binary_Operands
                                (DSL_OPERATOR dsl_operator,
                                 UINT16 version,
                                 BOOL integer_operands,
                                 BOOL equivalent_descriptors,
                                 BOOL kid0_is_constant,
                                 BOOL kid1_is_constant,
                                 const char *kid0_key,
                                 const char *kid1_key);
/* Read the immutable algebraic relation count, independent of registry resets. */
extern UINT32 DSL_Algebraic_Relation_Count (void);

/*
 * Copy a zero-based static relation, or just test its ordinal with NULL output.
 * An invalid ordinal returns FALSE without writes; no rewrite is performed.
 */
extern BOOL DSL_Algebraic_Relation_At
                                (UINT32 ordinal,
                                 DSL_ALGEBRAIC_RELATION_INFO *info);
/*
 * Match both operator/version pairs and relation kind exactly in static data.
 * NULL output tests presence; missing keys return FALSE without writes. The
 * caller still establishes the relation's safety conditions before rewriting.
 */
extern BOOL DSL_Algebraic_Relation_Get
                                (DSL_OPERATOR outer_operator,
                                 UINT16 outer_version,
                                 DSL_OPERATOR inner_operator,
                                 UINT16 inner_version,
                                 DSL_ALGEBRAIC_RELATION relation,
                                 DSL_ALGEBRAIC_RELATION_INFO *info);
/*
 * Ensure common exists; register legacy v1 seeds plus common logical versions
 * >= 2. Count successful calls, including duplicates, not new entries. No
 * all-or-nothing rollback or automatic lowering installation is provided.
 */
extern UINT32 DSL_Opcode_Register_Common_Substrate (void);

/*
 * Seed common, ensure transformer under common when absent, then register its
 * logical versions >= 1. Return transformer successes, including duplicates;
 * prerequisite mutations remain on failure. No lowering is installed.
 */
extern UINT32 DSL_Opcode_Register_Transformer_Domain (void);

/*
 * Seed common, ensure ckks under common when absent, then register its logical
 * versions >= 1. Count CKKS successes including duplicates, not prerequisite
 * entries. No transactional batch rollback or cryptographic validation occurs.
 */
extern UINT32 DSL_Opcode_Register_CKKS_Domain (void);

/*
 * Seed common and example-domain wrappers, creating missing child domains;
 * also register CNN logical versions >= 2. Count wrapper/CNN successes including
 * duplicates, not common prerequisites. The batch is not atomic.
 */
extern UINT32 DSL_Opcode_Register_Domain_Wrapper_Examples (void);

/* Read a live entry's direct wrapper link; zero means invalid or unwrapped. */
extern DSL_OPCODE_ID DSL_Opcode_Wrapper_Target (DSL_OPCODE_ID id);

/*
 * Free promotion payload strings and entries only; opcode/domain tables remain.
 * Borrowed promotion strings and old promotion ID meanings expire.
 */
extern void DSL_Opcode_Promotion_Registry_Reset (void);

/*
 * Require live source/target opcode IDs, valid state and nonzero version even
 * for duplicates. Identity is the source/target pair only; a duplicate ignores
 * replacement metadata. New entries copy each counted string (null arrays or
 * entries become empty); nonnull arrays must have at least their stated counts
 * of readable entries. State/version/flags describe evidence, not a performed
 * promotion. Validation rejection returns zero unchanged; allocation exceptions
 * have no rollback guarantee.
 */
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
/* Find an exact numeric source/target pair without revalidation; zero if absent. */
extern DSL_OPCODE_PROMOTION_ID DSL_Opcode_Promotion_Find
                                (DSL_OPCODE_ID source_opcode_id,
                                 DSL_OPCODE_ID promoted_opcode_id);
/*
 * Copy live promotion scalar fields/counts; NULL output tests validity only.
 * Invalid IDs return FALSE without writes; opcode liveness is not rechecked.
 */
extern BOOL DSL_Opcode_Promotion_Get_Info
                                (DSL_OPCODE_PROMOTION_ID id,
                                 DSL_OPCODE_PROMOTION_INFO *info);
/* Read the dynamic promotion entry count without changing its reset epoch. */
extern UINT32 DSL_Opcode_Promotion_Count (void);

/* Inspect a zero-based promotion ordinal using Get_Info's null/failure rules. */
extern BOOL DSL_Opcode_Promotion_At (UINT32 ordinal,
                                     DSL_OPCODE_PROMOTION_INFO *info);
/* Borrow a semantic string until promotion reset; invalid ID/ordinal gives NULL. */
extern const char *DSL_Opcode_Promotion_Required_Common_Semantic_At
                                (DSL_OPCODE_PROMOTION_ID id,
                                 UINT32 ordinal);
/* Borrow a wrapper-name string until promotion reset; invalid indices give NULL. */
extern const char *DSL_Opcode_Promotion_Retained_Wrapper_At
                                (DSL_OPCODE_PROMOTION_ID id,
                                 UINT32 ordinal);
/* Borrow a check name until promotion reset, or NULL; no verifier is executed. */
extern const char *DSL_Opcode_Promotion_Required_Verifier_Check_At
                                (DSL_OPCODE_PROMOTION_ID id,
                                 UINT32 ordinal);
/* Borrow a code until promotion reset, or NULL; no diagnostic is emitted. */
extern const char *DSL_Opcode_Promotion_Diagnostic_At
                                (DSL_OPCODE_PROMOTION_ID id,
                                 UINT32 ordinal);
/*
 * Seed wrapper prerequisites and example promotion metadata, creating missing
 * source entries when targets exist. Count successful promotions including
 * duplicates; prerequisite additions are excluded. No batch rollback or IR
 * transformation is performed.
 */
extern UINT32 DSL_Opcode_Promotion_Register_Examples (void);

/*
 * Return static CPROM-001 for invalid IDs/absent pair evidence, CPROM-009 for
 * target version mismatch, then CPROM-010 if a required source wrapper is absent;
 * NULL means only these checks passed. No wrapper-target equality, semantic
 * payload, promotion state, shape/effect or domain legality proof is made.
 */
extern const char *DSL_Opcode_Check_Promotion
                                (DSL_OPCODE_ID source_opcode_id,
                                 DSL_OPCODE_ID promoted_opcode_id,
                                 UINT16 common_version,
                                 BOOL require_wrapper);
/* Borrow a static category spelling; invalid enum values yield "unknown". */
extern const char *DSL_Opcode_Category_Name (DSL_OPCODE_CATEGORY category);

/* Borrow a static level spelling; invalid enum values yield "unknown". */
extern const char *DSL_Opcode_Level_Name (DSL_OPCODE_LEVEL level);

/* Borrow a static shape-rule spelling; invalid enum values yield "unknown". */
extern const char *DSL_Shape_Rule_Name (DSL_SHAPE_RULE shape_rule);

/* Borrow a static effect spelling; invalid enum values yield "unknown". */
extern const char *DSL_Effect_Model_Name (DSL_EFFECT_MODEL effect_model);

/* Borrow a static lowering spelling; invalid enum values yield "unknown". */
extern const char *DSL_Lowering_Model_Name
                                (DSL_LOWERING_MODEL lowering_model);
/* Borrow a static promotion-state spelling, or "unknown" for invalid values. */
extern const char *DSL_Opcode_Promotion_State_Name
                                (DSL_OPCODE_PROMOTION_STATE state);
/* Borrow a static CPROM code, or "unknown"; this does not emit a diagnostic. */
extern const char *DSL_CPROM_Diagnostic_Code
                                (DSL_CPROM_DIAGNOSTIC diagnostic);
/*
 * Print dynamic opcode metadata in registration order to a caller-owned stream.
 * NULL is a no-op; no table mutation or stream error reporting is performed.
 */
extern void DSL_Opcode_fprint_registry (FILE *f);

/*
 * Print promotion metadata and payloads without executing checks or mutation.
 * The caller owns stream lifetime/errors; a NULL stream is a no-op.
 */
extern void DSL_Opcode_Promotion_fprint_registry (FILE *f);

#endif /* dsl_opcode_INCLUDED */
