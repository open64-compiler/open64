/*
 * Copyright (C) 2026 Open64 Project
 */

/*
 * common/com implements immutable logical/operator trait tables and resettable
 * opcode/wrapper/promotion metadata. Dynamic records own copied strings; static
 * queries neither depend on registration nor expire on reset. Callers coordinate
 * access and dependent lifetimes. This service does not lower WHIRL, execute
 * gatekeepers or change mapped-image layouts; allocation exceptions in dynamic
 * registration have no transactional recovery.
 * Design: doc/WHIRL-DSL-INFRASTRUCTURE.md;
 * reductions: doc/AI-COMPILER-NATIVE-REDUCTION-CONTRACT-v0.1.md.
 */

#include <string.h>
#include <vector>

#include "dsl_opcode.h"

/* Count compile-time table elements; this is not valid for decayed pointers. */
#define DSL_ARRAY_COUNT(a) (sizeof(a) / sizeof((a)[0]))

struct DSL_OPCODE_RECORD {
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

    /* Initialize an unpublished, unwrapped record with no owned strings. */
    DSL_OPCODE_RECORD() :
        id(DSL_OPCODE_INVALID_ID),
        owner_domain_id(DSL_DOMAIN_INVALID_ID),
        wrapper_target_id(DSL_OPCODE_INVALID_ID),
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

struct DSL_OPCODE_PROMOTION_RECORD {
    DSL_OPCODE_PROMOTION_ID id;
    DSL_OPCODE_ID source_opcode_id;
    DSL_OPCODE_ID promoted_opcode_id;
    DSL_OPCODE_PROMOTION_STATE state;
    UINT16 version;
    std::vector<const char *> required_common_semantics;
    std::vector<const char *> retained_wrappers;
    std::vector<const char *> required_verifier_checks;
    std::vector<const char *> diagnostics;
    UINT32 flags;

    /* Initialize an unpublished evidence record with empty owning vectors. */
    DSL_OPCODE_PROMOTION_RECORD() :
        id(DSL_OPCODE_PROMOTION_INVALID_ID),
        source_opcode_id(DSL_OPCODE_INVALID_ID),
        promoted_opcode_id(DSL_OPCODE_INVALID_ID),
        state(DSL_OPCODE_PROMOTION_DOMAIN_ONLY),
        version(0),
        flags(0) {}
};

static std::vector<DSL_OPCODE_RECORD> DSL_opcode_registry;
static std::vector<DSL_OPCODE_PROMOTION_RECORD> DSL_opcode_promotion_registry;

static const char *DSL_opcode_category_name[] = {
    "executable",
    "declaration",
    "contract",
    "verifier",
    "lowering_policy"
};

static const char *DSL_opcode_level_name[] = {
    "level0_core",
    "level1_tensor",
    "level2_numeric",
    "level3_nn_common",
    "level4_runtime"
};

static const char *DSL_shape_rule_name[] = {
    "opaque",
    "identity",
    "broadcast",
    "contraction",
    "reduction",
    "view",
    "layout",
    "runtime_guarded"
};

static const char *DSL_effect_model_name[] = {
    "pure",
    "verifier_only",
    "declaration_only",
    "lowering_policy",
    "runtime_effect"
};

static const char *DSL_lowering_model_name[] = {
    "marker_only",
    "canonical_whirl",
    "runtime_call",
    "intrinsic_sequence",
    "target_specific"
};

static const char *DSL_opcode_promotion_state_name[] = {
    "domain_only",
    "wrapper_to_common",
    "partial_promotion",
    "common_native"
};

static const char *DSL_operator_name[] = {
    "OPR_DSLUNKNOWN",
    "OPR_DSLTENSORCONST",
    "OPR_DSLADD",
    "OPR_DSLMATMUL",
    "OPR_DSLMODELINPUT",
    "OPR_DSLRELU",
    "OPR_DSLFLATTEN",
    "OPR_DSLRESIDUALADD",
    "OPR_DSLLINEAR",
    "OPR_DSLOUTPUTLOGITS",
    "OPR_DSLCONV2D",
    "OPR_DSLBATCHNORMINFER",
    "OPR_DSLMAXPOOL2D",
    "OPR_DSLGLOBALAVGPOOL2D",
    "OPR_DSLRESHAPE",
    "OPR_DSLTRANSPOSE",
    "OPR_DSLTOKENEMBEDDING",
    "OPR_DSLRMSNORM",
    "OPR_DSLROTARYEMBEDDING",
    "OPR_DSLATTENTION",
    "OPR_DSLSWIGLU",
    "OPR_DSLSCATTER",
    "OPR_DSLMUL",
    "OPR_DSLDIV",
    "OPR_DSLREM",
    "OPR_DSLDIVREM",
    "OPR_DSLDIVPART",
    "OPR_DSLREMPART",
    "OPR_DSLCKKSADD",
    "OPR_DSLCKKSSUB",
    "OPR_DSLCKKSMUL",
    "OPR_DSLCKKSENCODE",
    "OPR_DSLCKKSROTATE",
    "OPR_DSLCKKSRESCALE",
    "OPR_DSLCKKSMODSWITCH",
    "OPR_DSLCKKSRELIN",
    "OPR_DSLCKKSBOOTSTRAP",
    "OPR_DSLREDUCESUM",
    "OPR_DSLSTABLESOFTMAX",
    "OPR_DSLMEANVARNORM"
};

static const char *DSL_fusion_iteration_class_name_table[] = {
    "opaque", "pointwise", "contraction", "reduction", "view"
};

static const char *DSL_fusion_indexing_class_name_table[] = {
    "opaque", "identity", "broadcast", "contraction", "reduction", "view"
};

static const char *DSL_cprom_diagnostic_code[] = {
    "CPROM-001",
    "CPROM-002",
    "CPROM-003",
    "CPROM-004",
    "CPROM-005",
    "CPROM-006",
    "CPROM-007",
    "CPROM-008",
    "CPROM-009",
    "CPROM-010"
};

struct DSL_COMMON_OPCODE_SEED {
    const char *name;
    DSL_OPCODE_CATEGORY category;
    DSL_OPCODE_LEVEL level;
    mINT16 nkids;
    DSL_SHAPE_RULE shape_rule;
    DSL_EFFECT_MODEL effect_model;
    DSL_LOWERING_MODEL lowering_model;
    const char *diagnostic_prefix;
};

struct DSL_LOGICAL_OPERATOR_SEED {
    DSL_OPERATOR dsl_operator;
    const char *name;
    UINT16 version;
    DSL_OPCODE_CATEGORY category;
    DSL_OPCODE_LEVEL level;
    mINT16 nkids;
    DSL_SHAPE_RULE shape_rule;
    DSL_EFFECT_MODEL effect_model;
    DSL_LOWERING_MODEL lowering_model;
    const char *diagnostic_prefix;
    const char *attribute_schema;
};

struct DSL_DOMAIN_WRAPPER_SEED {
    const char *domain_name;
    const char *name;
    const char *target_name;
    const char *diagnostic_prefix;
};

struct DSL_OPCODE_PROMOTION_SEED {
    const char *source_domain;
    const char *source_name;
    const char *promoted_name;
    DSL_OPCODE_PROMOTION_STATE state;
    const char *common_semantic;
    const char *retained_wrapper;
    const char *verifier_check;
    const char *diagnostic;
};

/* Legacy v1 population order fixes dynamic IDs; it is not the logical table. */
static const DSL_COMMON_OPCODE_SEED DSL_common_opcode_seed[] = {
    { "common.module", DSL_OPCODE_CATEGORY_DECLARATION,
        DSL_OPCODE_LEVEL_0_CORE, DSL_OPCODE_NKIDS_PAYLOAD_DEFINED,
        DSL_SHAPE_RULE_OPAQUE, DSL_EFFECT_MODEL_DECLARATION_ONLY,
        DSL_LOWERING_MODEL_MARKER_ONLY, "DOPC_COMMON_MODULE" },
    { "common.function", DSL_OPCODE_CATEGORY_DECLARATION,
        DSL_OPCODE_LEVEL_0_CORE, DSL_OPCODE_NKIDS_PAYLOAD_DEFINED,
        DSL_SHAPE_RULE_OPAQUE, DSL_EFFECT_MODEL_DECLARATION_ONLY,
        DSL_LOWERING_MODEL_MARKER_ONLY, "DOPC_COMMON_FUNCTION" },
    { "common.region", DSL_OPCODE_CATEGORY_DECLARATION,
        DSL_OPCODE_LEVEL_0_CORE, DSL_OPCODE_NKIDS_PAYLOAD_DEFINED,
        DSL_SHAPE_RULE_OPAQUE, DSL_EFFECT_MODEL_DECLARATION_ONLY,
        DSL_LOWERING_MODEL_MARKER_ONLY, "DOPC_COMMON_REGION" },
    { "common.call", DSL_OPCODE_CATEGORY_EXECUTABLE,
        DSL_OPCODE_LEVEL_0_CORE, DSL_OPCODE_NKIDS_VARIADIC,
        DSL_SHAPE_RULE_OPAQUE, DSL_EFFECT_MODEL_RUNTIME_EFFECT,
        DSL_LOWERING_MODEL_MARKER_ONLY, "DOPC_COMMON_CALL" },
    { "common.return", DSL_OPCODE_CATEGORY_EXECUTABLE,
        DSL_OPCODE_LEVEL_0_CORE, DSL_OPCODE_NKIDS_PAYLOAD_DEFINED,
        DSL_SHAPE_RULE_OPAQUE, DSL_EFFECT_MODEL_RUNTIME_EFFECT,
        DSL_LOWERING_MODEL_MARKER_ONLY, "DOPC_COMMON_RETURN" },
    { "common.constant", DSL_OPCODE_CATEGORY_DECLARATION,
        DSL_OPCODE_LEVEL_0_CORE, 0,
        DSL_SHAPE_RULE_OPAQUE, DSL_EFFECT_MODEL_DECLARATION_ONLY,
        DSL_LOWERING_MODEL_MARKER_ONLY, "DOPC_COMMON_CONSTANT" },
    { "common.effect", DSL_OPCODE_CATEGORY_DECLARATION,
        DSL_OPCODE_LEVEL_0_CORE, DSL_OPCODE_NKIDS_PAYLOAD_DEFINED,
        DSL_SHAPE_RULE_OPAQUE, DSL_EFFECT_MODEL_DECLARATION_ONLY,
        DSL_LOWERING_MODEL_MARKER_ONLY, "DOPC_COMMON_EFFECT" },

    { "common.tensor", DSL_OPCODE_CATEGORY_DECLARATION,
        DSL_OPCODE_LEVEL_1_TENSOR, 0,
        DSL_SHAPE_RULE_OPAQUE, DSL_EFFECT_MODEL_DECLARATION_ONLY,
        DSL_LOWERING_MODEL_MARKER_ONLY, "DOPC_COMMON_TENSOR" },
    { "common.shape_of", DSL_OPCODE_CATEGORY_DECLARATION,
        DSL_OPCODE_LEVEL_1_TENSOR, 1,
        DSL_SHAPE_RULE_OPAQUE, DSL_EFFECT_MODEL_DECLARATION_ONLY,
        DSL_LOWERING_MODEL_MARKER_ONLY, "DOPC_COMMON_SHAPE_OF" },
    { "common.shape_formula", DSL_OPCODE_CATEGORY_DECLARATION,
        DSL_OPCODE_LEVEL_1_TENSOR, DSL_OPCODE_NKIDS_PAYLOAD_DEFINED,
        DSL_SHAPE_RULE_OPAQUE, DSL_EFFECT_MODEL_DECLARATION_ONLY,
        DSL_LOWERING_MODEL_MARKER_ONLY, "DOPC_COMMON_SHAPE_FORMULA" },
    { "common.shape_assert", DSL_OPCODE_CATEGORY_VERIFIER,
        DSL_OPCODE_LEVEL_1_TENSOR, DSL_OPCODE_NKIDS_PAYLOAD_DEFINED,
        DSL_SHAPE_RULE_RUNTIME_GUARDED, DSL_EFFECT_MODEL_VERIFIER_ONLY,
        DSL_LOWERING_MODEL_MARKER_ONLY, "DOPC_COMMON_SHAPE_ASSERT" },
    { "common.runtime_shape_guard", DSL_OPCODE_CATEGORY_VERIFIER,
        DSL_OPCODE_LEVEL_1_TENSOR, DSL_OPCODE_NKIDS_PAYLOAD_DEFINED,
        DSL_SHAPE_RULE_RUNTIME_GUARDED, DSL_EFFECT_MODEL_VERIFIER_ONLY,
        DSL_LOWERING_MODEL_MARKER_ONLY, "DOPC_COMMON_RUNTIME_SHAPE_GUARD" },
    { "common.reshape", DSL_OPCODE_CATEGORY_EXECUTABLE,
        DSL_OPCODE_LEVEL_1_TENSOR, 1,
        DSL_SHAPE_RULE_VIEW, DSL_EFFECT_MODEL_PURE,
        DSL_LOWERING_MODEL_MARKER_ONLY, "DOPC_COMMON_RESHAPE" },
    { "common.flatten", DSL_OPCODE_CATEGORY_EXECUTABLE,
        DSL_OPCODE_LEVEL_1_TENSOR, 1,
        DSL_SHAPE_RULE_VIEW, DSL_EFFECT_MODEL_PURE,
        DSL_LOWERING_MODEL_MARKER_ONLY, "DOPC_COMMON_FLATTEN" },
    { "common.transpose", DSL_OPCODE_CATEGORY_EXECUTABLE,
        DSL_OPCODE_LEVEL_1_TENSOR, 1,
        DSL_SHAPE_RULE_VIEW, DSL_EFFECT_MODEL_PURE,
        DSL_LOWERING_MODEL_MARKER_ONLY, "DOPC_COMMON_TRANSPOSE" },
    { "common.slice", DSL_OPCODE_CATEGORY_EXECUTABLE,
        DSL_OPCODE_LEVEL_1_TENSOR, 1,
        DSL_SHAPE_RULE_VIEW, DSL_EFFECT_MODEL_PURE,
        DSL_LOWERING_MODEL_MARKER_ONLY, "DOPC_COMMON_SLICE" },
    { "common.concat", DSL_OPCODE_CATEGORY_EXECUTABLE,
        DSL_OPCODE_LEVEL_1_TENSOR, DSL_OPCODE_NKIDS_VARIADIC,
        DSL_SHAPE_RULE_VIEW, DSL_EFFECT_MODEL_PURE,
        DSL_LOWERING_MODEL_MARKER_ONLY, "DOPC_COMMON_CONCAT" },
    { "common.pad", DSL_OPCODE_CATEGORY_EXECUTABLE,
        DSL_OPCODE_LEVEL_1_TENSOR, 1,
        DSL_SHAPE_RULE_VIEW, DSL_EFFECT_MODEL_PURE,
        DSL_LOWERING_MODEL_MARKER_ONLY, "DOPC_COMMON_PAD" },
    { "common.layout_cast", DSL_OPCODE_CATEGORY_EXECUTABLE,
        DSL_OPCODE_LEVEL_1_TENSOR, 1,
        DSL_SHAPE_RULE_LAYOUT, DSL_EFFECT_MODEL_PURE,
        DSL_LOWERING_MODEL_MARKER_ONLY, "DOPC_COMMON_LAYOUT_CAST" },
    { "common.contiguous", DSL_OPCODE_CATEGORY_EXECUTABLE,
        DSL_OPCODE_LEVEL_1_TENSOR, 1,
        DSL_SHAPE_RULE_LAYOUT, DSL_EFFECT_MODEL_PURE,
        DSL_LOWERING_MODEL_MARKER_ONLY, "DOPC_COMMON_CONTIGUOUS" },

    { "common.add", DSL_OPCODE_CATEGORY_EXECUTABLE,
        DSL_OPCODE_LEVEL_2_NUMERIC, 2,
        DSL_SHAPE_RULE_BROADCAST, DSL_EFFECT_MODEL_PURE,
        DSL_LOWERING_MODEL_RUNTIME_CALL, "DOPC_COMMON_ADD" },
    { "common.mul", DSL_OPCODE_CATEGORY_EXECUTABLE,
        DSL_OPCODE_LEVEL_2_NUMERIC, 2,
        DSL_SHAPE_RULE_BROADCAST, DSL_EFFECT_MODEL_PURE,
        DSL_LOWERING_MODEL_MARKER_ONLY, "DOPC_COMMON_MUL" },
    { "common.bias_add", DSL_OPCODE_CATEGORY_EXECUTABLE,
        DSL_OPCODE_LEVEL_2_NUMERIC, 2,
        DSL_SHAPE_RULE_BROADCAST, DSL_EFFECT_MODEL_PURE,
        DSL_LOWERING_MODEL_MARKER_ONLY, "DOPC_COMMON_BIAS_ADD" },
    { "common.matmul", DSL_OPCODE_CATEGORY_EXECUTABLE,
        DSL_OPCODE_LEVEL_2_NUMERIC, 2,
        DSL_SHAPE_RULE_CONTRACTION, DSL_EFFECT_MODEL_PURE,
        DSL_LOWERING_MODEL_RUNTIME_CALL, "DOPC_COMMON_MATMUL" },
    { "common.gemm", DSL_OPCODE_CATEGORY_EXECUTABLE,
        DSL_OPCODE_LEVEL_2_NUMERIC, DSL_OPCODE_NKIDS_VARIADIC,
        DSL_SHAPE_RULE_CONTRACTION, DSL_EFFECT_MODEL_PURE,
        DSL_LOWERING_MODEL_MARKER_ONLY, "DOPC_COMMON_GEMM" },
    { "common.linear", DSL_OPCODE_CATEGORY_EXECUTABLE,
        DSL_OPCODE_LEVEL_2_NUMERIC, DSL_OPCODE_NKIDS_VARIADIC,
        DSL_SHAPE_RULE_CONTRACTION, DSL_EFFECT_MODEL_PURE,
        DSL_LOWERING_MODEL_MARKER_ONLY, "DOPC_COMMON_LINEAR" },
    { "common.activation", DSL_OPCODE_CATEGORY_EXECUTABLE,
        DSL_OPCODE_LEVEL_2_NUMERIC, 1,
        DSL_SHAPE_RULE_IDENTITY, DSL_EFFECT_MODEL_PURE,
        DSL_LOWERING_MODEL_MARKER_ONLY, "DOPC_COMMON_ACTIVATION" },
    { "common.relu", DSL_OPCODE_CATEGORY_EXECUTABLE,
        DSL_OPCODE_LEVEL_2_NUMERIC, 1,
        DSL_SHAPE_RULE_IDENTITY, DSL_EFFECT_MODEL_PURE,
        DSL_LOWERING_MODEL_MARKER_ONLY, "DOPC_COMMON_RELU" },
    { "common.gelu", DSL_OPCODE_CATEGORY_EXECUTABLE,
        DSL_OPCODE_LEVEL_2_NUMERIC, 1,
        DSL_SHAPE_RULE_IDENTITY, DSL_EFFECT_MODEL_PURE,
        DSL_LOWERING_MODEL_MARKER_ONLY, "DOPC_COMMON_GELU" },
    { "common.silu", DSL_OPCODE_CATEGORY_EXECUTABLE,
        DSL_OPCODE_LEVEL_2_NUMERIC, 1,
        DSL_SHAPE_RULE_IDENTITY, DSL_EFFECT_MODEL_PURE,
        DSL_LOWERING_MODEL_MARKER_ONLY, "DOPC_COMMON_SILU" },
    { "common.softmax", DSL_OPCODE_CATEGORY_EXECUTABLE,
        DSL_OPCODE_LEVEL_2_NUMERIC, 1,
        DSL_SHAPE_RULE_REDUCTION, DSL_EFFECT_MODEL_PURE,
        DSL_LOWERING_MODEL_MARKER_ONLY, "DOPC_COMMON_SOFTMAX" },
    { "common.reduce", DSL_OPCODE_CATEGORY_EXECUTABLE,
        DSL_OPCODE_LEVEL_2_NUMERIC, 1,
        DSL_SHAPE_RULE_REDUCTION, DSL_EFFECT_MODEL_PURE,
        DSL_LOWERING_MODEL_MARKER_ONLY, "DOPC_COMMON_REDUCE" },
    { "common.reduce_sum", DSL_OPCODE_CATEGORY_EXECUTABLE,
        DSL_OPCODE_LEVEL_2_NUMERIC, 1,
        DSL_SHAPE_RULE_REDUCTION, DSL_EFFECT_MODEL_PURE,
        DSL_LOWERING_MODEL_MARKER_ONLY, "DOPC_COMMON_REDUCE_SUM" },
    { "common.reduce_mean", DSL_OPCODE_CATEGORY_EXECUTABLE,
        DSL_OPCODE_LEVEL_2_NUMERIC, 1,
        DSL_SHAPE_RULE_REDUCTION, DSL_EFFECT_MODEL_PURE,
        DSL_LOWERING_MODEL_MARKER_ONLY, "DOPC_COMMON_REDUCE_MEAN" },
    { "common.reduce_max", DSL_OPCODE_CATEGORY_EXECUTABLE,
        DSL_OPCODE_LEVEL_2_NUMERIC, 1,
        DSL_SHAPE_RULE_REDUCTION, DSL_EFFECT_MODEL_PURE,
        DSL_LOWERING_MODEL_MARKER_ONLY, "DOPC_COMMON_REDUCE_MAX" },
    { "common.window_reduce", DSL_OPCODE_CATEGORY_EXECUTABLE,
        DSL_OPCODE_LEVEL_2_NUMERIC, 1,
        DSL_SHAPE_RULE_REDUCTION, DSL_EFFECT_MODEL_PURE,
        DSL_LOWERING_MODEL_MARKER_ONLY, "DOPC_COMMON_WINDOW_REDUCE" },

    { "common.model_input", DSL_OPCODE_CATEGORY_DECLARATION,
        DSL_OPCODE_LEVEL_3_NN_COMMON, 0,
        DSL_SHAPE_RULE_OPAQUE, DSL_EFFECT_MODEL_DECLARATION_ONLY,
        DSL_LOWERING_MODEL_MARKER_ONLY, "DOPC_COMMON_MODEL_INPUT" },
    { "common.model_output", DSL_OPCODE_CATEGORY_DECLARATION,
        DSL_OPCODE_LEVEL_3_NN_COMMON, 1,
        DSL_SHAPE_RULE_OPAQUE, DSL_EFFECT_MODEL_DECLARATION_ONLY,
        DSL_LOWERING_MODEL_MARKER_ONLY, "DOPC_COMMON_MODEL_OUTPUT" },
    { "common.output_logits", DSL_OPCODE_CATEGORY_DECLARATION,
        DSL_OPCODE_LEVEL_3_NN_COMMON, 1,
        DSL_SHAPE_RULE_OPAQUE, DSL_EFFECT_MODEL_DECLARATION_ONLY,
        DSL_LOWERING_MODEL_MARKER_ONLY, "DOPC_COMMON_OUTPUT_LOGITS" },
    { "common.residual_add", DSL_OPCODE_CATEGORY_EXECUTABLE,
        DSL_OPCODE_LEVEL_3_NN_COMMON, 2,
        DSL_SHAPE_RULE_BROADCAST, DSL_EFFECT_MODEL_PURE,
        DSL_LOWERING_MODEL_MARKER_ONLY, "DOPC_COMMON_RESIDUAL_ADD" },
    { "common.residual_shape_check", DSL_OPCODE_CATEGORY_VERIFIER,
        DSL_OPCODE_LEVEL_3_NN_COMMON, 2,
        DSL_SHAPE_RULE_RUNTIME_GUARDED, DSL_EFFECT_MODEL_VERIFIER_ONLY,
        DSL_LOWERING_MODEL_MARKER_ONLY, "DOPC_COMMON_RESIDUAL_SHAPE_CHECK" },
    { "common.normalization_base", DSL_OPCODE_CATEGORY_EXECUTABLE,
        DSL_OPCODE_LEVEL_3_NN_COMMON, 1,
        DSL_SHAPE_RULE_REDUCTION, DSL_EFFECT_MODEL_PURE,
        DSL_LOWERING_MODEL_MARKER_ONLY, "DOPC_COMMON_NORMALIZATION_BASE" },
    { "common.fusion_group", DSL_OPCODE_CATEGORY_LOWERING_POLICY,
        DSL_OPCODE_LEVEL_3_NN_COMMON, DSL_OPCODE_NKIDS_VARIADIC,
        DSL_SHAPE_RULE_OPAQUE, DSL_EFFECT_MODEL_LOWERING_POLICY,
        DSL_LOWERING_MODEL_MARKER_ONLY, "DOPC_COMMON_FUSION_GROUP" },
    { "common.fusion_contract", DSL_OPCODE_CATEGORY_LOWERING_POLICY,
        DSL_OPCODE_LEVEL_3_NN_COMMON, 1,
        DSL_SHAPE_RULE_OPAQUE, DSL_EFFECT_MODEL_LOWERING_POLICY,
        DSL_LOWERING_MODEL_MARKER_ONLY, "DOPC_COMMON_FUSION_CONTRACT" },
    { "common.quantize", DSL_OPCODE_CATEGORY_EXECUTABLE,
        DSL_OPCODE_LEVEL_3_NN_COMMON, 1,
        DSL_SHAPE_RULE_IDENTITY, DSL_EFFECT_MODEL_PURE,
        DSL_LOWERING_MODEL_MARKER_ONLY, "DOPC_COMMON_QUANTIZE" },
    { "common.dequantize", DSL_OPCODE_CATEGORY_EXECUTABLE,
        DSL_OPCODE_LEVEL_3_NN_COMMON, 1,
        DSL_SHAPE_RULE_IDENTITY, DSL_EFFECT_MODEL_PURE,
        DSL_LOWERING_MODEL_MARKER_ONLY, "DOPC_COMMON_DEQUANTIZE" },
    { "common.requantize", DSL_OPCODE_CATEGORY_EXECUTABLE,
        DSL_OPCODE_LEVEL_3_NN_COMMON, 1,
        DSL_SHAPE_RULE_IDENTITY, DSL_EFFECT_MODEL_PURE,
        DSL_LOWERING_MODEL_MARKER_ONLY, "DOPC_COMMON_REQUANTIZE" },
    { "common.quantization_contract", DSL_OPCODE_CATEGORY_CONTRACT,
        DSL_OPCODE_LEVEL_3_NN_COMMON, DSL_OPCODE_NKIDS_PAYLOAD_DEFINED,
        DSL_SHAPE_RULE_OPAQUE, DSL_EFFECT_MODEL_DECLARATION_ONLY,
        DSL_LOWERING_MODEL_MARKER_ONLY, "DOPC_COMMON_QUANTIZATION_CONTRACT" },

    { "common.shard", DSL_OPCODE_CATEGORY_LOWERING_POLICY,
        DSL_OPCODE_LEVEL_4_RUNTIME, 1,
        DSL_SHAPE_RULE_RUNTIME_GUARDED, DSL_EFFECT_MODEL_LOWERING_POLICY,
        DSL_LOWERING_MODEL_MARKER_ONLY, "DOPC_COMMON_SHARD" },
    { "common.reshard", DSL_OPCODE_CATEGORY_LOWERING_POLICY,
        DSL_OPCODE_LEVEL_4_RUNTIME, 1,
        DSL_SHAPE_RULE_RUNTIME_GUARDED, DSL_EFFECT_MODEL_LOWERING_POLICY,
        DSL_LOWERING_MODEL_MARKER_ONLY, "DOPC_COMMON_RESHARD" },
    { "common.dispatch", DSL_OPCODE_CATEGORY_LOWERING_POLICY,
        DSL_OPCODE_LEVEL_4_RUNTIME, 1,
        DSL_SHAPE_RULE_OPAQUE, DSL_EFFECT_MODEL_LOWERING_POLICY,
        DSL_LOWERING_MODEL_MARKER_ONLY, "DOPC_COMMON_DISPATCH" },
    { "common.gather", DSL_OPCODE_CATEGORY_EXECUTABLE,
        DSL_OPCODE_LEVEL_4_RUNTIME, 2,
        DSL_SHAPE_RULE_VIEW, DSL_EFFECT_MODEL_PURE,
        DSL_LOWERING_MODEL_MARKER_ONLY, "DOPC_COMMON_GATHER" },
    { "common.scatter", DSL_OPCODE_CATEGORY_EXECUTABLE,
        DSL_OPCODE_LEVEL_4_RUNTIME, 3,
        DSL_SHAPE_RULE_VIEW, DSL_EFFECT_MODEL_RUNTIME_EFFECT,
        DSL_LOWERING_MODEL_MARKER_ONLY, "DOPC_COMMON_SCATTER" },
    { "common.reduce_scatter_intent", DSL_OPCODE_CATEGORY_LOWERING_POLICY,
        DSL_OPCODE_LEVEL_4_RUNTIME, 1,
        DSL_SHAPE_RULE_RUNTIME_GUARDED, DSL_EFFECT_MODEL_LOWERING_POLICY,
        DSL_LOWERING_MODEL_MARKER_ONLY, "DOPC_COMMON_REDUCE_SCATTER_INTENT" },
    { "common.all_reduce_intent", DSL_OPCODE_CATEGORY_LOWERING_POLICY,
        DSL_OPCODE_LEVEL_4_RUNTIME, 1,
        DSL_SHAPE_RULE_RUNTIME_GUARDED, DSL_EFFECT_MODEL_LOWERING_POLICY,
        DSL_LOWERING_MODEL_MARKER_ONLY, "DOPC_COMMON_ALL_REDUCE_INTENT" },
    { "common.all_gather_intent", DSL_OPCODE_CATEGORY_LOWERING_POLICY,
        DSL_OPCODE_LEVEL_4_RUNTIME, 1,
        DSL_SHAPE_RULE_RUNTIME_GUARDED, DSL_EFFECT_MODEL_LOWERING_POLICY,
        DSL_LOWERING_MODEL_MARKER_ONLY, "DOPC_COMMON_ALL_GATHER_INTENT" },
    { "common.all_to_all_intent", DSL_OPCODE_CATEGORY_LOWERING_POLICY,
        DSL_OPCODE_LEVEL_4_RUNTIME, 1,
        DSL_SHAPE_RULE_RUNTIME_GUARDED, DSL_EFFECT_MODEL_LOWERING_POLICY,
        DSL_LOWERING_MODEL_MARKER_ONLY, "DOPC_COMMON_ALL_TO_ALL_INTENT" },
    { "common.runtime_state_handle", DSL_OPCODE_CATEGORY_DECLARATION,
        DSL_OPCODE_LEVEL_4_RUNTIME, 0,
        DSL_SHAPE_RULE_OPAQUE, DSL_EFFECT_MODEL_DECLARATION_ONLY,
        DSL_LOWERING_MODEL_MARKER_ONLY, "DOPC_COMMON_RUNTIME_STATE_HANDLE" },
    { "common.runtime_guard", DSL_OPCODE_CATEGORY_VERIFIER,
        DSL_OPCODE_LEVEL_4_RUNTIME, DSL_OPCODE_NKIDS_PAYLOAD_DEFINED,
        DSL_SHAPE_RULE_RUNTIME_GUARDED, DSL_EFFECT_MODEL_VERIFIER_ONLY,
        DSL_LOWERING_MODEL_MARKER_ONLY, "DOPC_COMMON_RUNTIME_GUARD" },
    { "common.shape_bucket", DSL_OPCODE_CATEGORY_LOWERING_POLICY,
        DSL_OPCODE_LEVEL_4_RUNTIME, DSL_OPCODE_NKIDS_PAYLOAD_DEFINED,
        DSL_SHAPE_RULE_RUNTIME_GUARDED, DSL_EFFECT_MODEL_LOWERING_POLICY,
        DSL_LOWERING_MODEL_MARKER_ONLY, "DOPC_COMMON_SHAPE_BUCKET" },
    { "common.kernel_variant", DSL_OPCODE_CATEGORY_LOWERING_POLICY,
        DSL_OPCODE_LEVEL_4_RUNTIME, DSL_OPCODE_NKIDS_PAYLOAD_DEFINED,
        DSL_SHAPE_RULE_OPAQUE, DSL_EFFECT_MODEL_LOWERING_POLICY,
        DSL_LOWERING_MODEL_MARKER_ONLY, "DOPC_COMMON_KERNEL_VARIANT" },

    /* Appended to preserve runtime registry IDs of existing common seeds. */
    { "common.tensor_const", DSL_OPCODE_CATEGORY_EXECUTABLE,
        DSL_OPCODE_LEVEL_1_TENSOR, 0,
        DSL_SHAPE_RULE_OPAQUE, DSL_EFFECT_MODEL_PURE,
        DSL_LOWERING_MODEL_RUNTIME_CALL, "DOPC_COMMON_TENSOR_CONST" },
    { "common.mul", DSL_OPCODE_CATEGORY_EXECUTABLE,
        DSL_OPCODE_LEVEL_2_NUMERIC, 2,
        DSL_SHAPE_RULE_BROADCAST, DSL_EFFECT_MODEL_PURE,
        DSL_LOWERING_MODEL_MARKER_ONLY, "DOPC_COMMON_MUL" },
    { "common.div", DSL_OPCODE_CATEGORY_EXECUTABLE,
        DSL_OPCODE_LEVEL_2_NUMERIC, 2,
        DSL_SHAPE_RULE_BROADCAST, DSL_EFFECT_MODEL_PURE,
        DSL_LOWERING_MODEL_MARKER_ONLY, "DOPC_COMMON_DIV" },
    { "common.rem", DSL_OPCODE_CATEGORY_EXECUTABLE,
        DSL_OPCODE_LEVEL_2_NUMERIC, 2,
        DSL_SHAPE_RULE_BROADCAST, DSL_EFFECT_MODEL_PURE,
        DSL_LOWERING_MODEL_MARKER_ONLY, "DOPC_COMMON_REM" },
    { "common.stable_softmax", DSL_OPCODE_CATEGORY_EXECUTABLE,
        DSL_OPCODE_LEVEL_2_NUMERIC, 1,
        DSL_SHAPE_RULE_IDENTITY, DSL_EFFECT_MODEL_PURE,
        DSL_LOWERING_MODEL_MARKER_ONLY, "DOPC_COMMON_STABLE_SOFTMAX" },
    { "common.mean_variance_norm", DSL_OPCODE_CATEGORY_EXECUTABLE,
        DSL_OPCODE_LEVEL_2_NUMERIC, 3,
        DSL_SHAPE_RULE_IDENTITY, DSL_EFFECT_MODEL_PURE,
        DSL_LOWERING_MODEL_MARKER_ONLY, "DOPC_COMMON_MEAN_VAR_NORM" }
};

/* Exact-version logical contracts coexist; no query silently upgrades IR. */
static const DSL_LOGICAL_OPERATOR_SEED DSL_logical_operator_seed[] = {
    { OPR_DSLTENSORCONST, "common.tensor_const", 1,
        DSL_OPCODE_CATEGORY_EXECUTABLE, DSL_OPCODE_LEVEL_1_TENSOR, 0,
        DSL_SHAPE_RULE_OPAQUE, DSL_EFFECT_MODEL_PURE,
        DSL_LOWERING_MODEL_RUNTIME_CALL, "DOPC_COMMON_TENSOR_CONST",
        "value_kind;value" },
    { OPR_DSLADD, "common.add", 1,
        DSL_OPCODE_CATEGORY_EXECUTABLE, DSL_OPCODE_LEVEL_2_NUMERIC, 2,
        DSL_SHAPE_RULE_BROADCAST, DSL_EFFECT_MODEL_PURE,
        DSL_LOWERING_MODEL_RUNTIME_CALL, "DOPC_COMMON_ADD",
        "attr.broadcast_rule" },
    { OPR_DSLMATMUL, "common.matmul", 1,
        DSL_OPCODE_CATEGORY_EXECUTABLE, DSL_OPCODE_LEVEL_2_NUMERIC, 2,
        DSL_SHAPE_RULE_CONTRACTION, DSL_EFFECT_MODEL_PURE,
        DSL_LOWERING_MODEL_RUNTIME_CALL, "DOPC_COMMON_MATMUL",
        "attr.transpose_kid0;attr.transpose_kid1" },
    { OPR_DSLMATMUL, "common.matmul", 2,
        DSL_OPCODE_CATEGORY_EXECUTABLE, DSL_OPCODE_LEVEL_2_NUMERIC, 2,
        DSL_SHAPE_RULE_CONTRACTION, DSL_EFFECT_MODEL_PURE,
        DSL_LOWERING_MODEL_RUNTIME_CALL, "DOPC_COMMON_MATMUL_V2",
        "attr.transpose_kid0;attr.transpose_kid1;attr.batch_rule;"
        "attr.accum_dtype" },
    { OPR_DSLMODELINPUT, "common.model_input", 2,
        DSL_OPCODE_CATEGORY_EXECUTABLE, DSL_OPCODE_LEVEL_3_NN_COMMON, 0,
        DSL_SHAPE_RULE_OPAQUE, DSL_EFFECT_MODEL_PURE,
        DSL_LOWERING_MODEL_RUNTIME_CALL, "DOPC_COMMON_MODEL_INPUT_V2",
        "attr.input_ordinal" },
    { OPR_DSLRELU, "common.relu", 2,
        DSL_OPCODE_CATEGORY_EXECUTABLE, DSL_OPCODE_LEVEL_2_NUMERIC, 1,
        DSL_SHAPE_RULE_IDENTITY, DSL_EFFECT_MODEL_PURE,
        DSL_LOWERING_MODEL_RUNTIME_CALL, "DOPC_COMMON_RELU_V2", "" },
    { OPR_DSLFLATTEN, "common.flatten", 2,
        DSL_OPCODE_CATEGORY_EXECUTABLE, DSL_OPCODE_LEVEL_1_TENSOR, 1,
        DSL_SHAPE_RULE_VIEW, DSL_EFFECT_MODEL_PURE,
        DSL_LOWERING_MODEL_RUNTIME_CALL, "DOPC_COMMON_FLATTEN_V2",
        "attr.start_dim;attr.end_dim" },
    { OPR_DSLRESIDUALADD, "common.residual_add", 2,
        DSL_OPCODE_CATEGORY_EXECUTABLE, DSL_OPCODE_LEVEL_3_NN_COMMON, 2,
        DSL_SHAPE_RULE_BROADCAST, DSL_EFFECT_MODEL_PURE,
        DSL_LOWERING_MODEL_RUNTIME_CALL, "DOPC_COMMON_RESIDUAL_ADD_V2",
        "attr.broadcast_rule;attr.shape_check;attr.residual_path" },
    { OPR_DSLLINEAR, "common.linear", 2,
        DSL_OPCODE_CATEGORY_EXECUTABLE, DSL_OPCODE_LEVEL_2_NUMERIC, 3,
        DSL_SHAPE_RULE_CONTRACTION, DSL_EFFECT_MODEL_PURE,
        DSL_LOWERING_MODEL_RUNTIME_CALL, "DOPC_COMMON_LINEAR_V2",
        "attr.has_bias;attr.transpose_input;attr.transpose_weight;"
        "attr.weight_layout" },
    { OPR_DSLLINEAR, "common.linear", 3,
        DSL_OPCODE_CATEGORY_EXECUTABLE, DSL_OPCODE_LEVEL_2_NUMERIC, 2,
        DSL_SHAPE_RULE_CONTRACTION, DSL_EFFECT_MODEL_PURE,
        DSL_LOWERING_MODEL_RUNTIME_CALL, "DOPC_COMMON_LINEAR_V3",
        "attr.has_bias;attr.transpose_input;attr.transpose_weight;"
        "attr.weight_layout" },
    { OPR_DSLOUTPUTLOGITS, "common.output_logits", 2,
        DSL_OPCODE_CATEGORY_EXECUTABLE, DSL_OPCODE_LEVEL_3_NN_COMMON, 1,
        DSL_SHAPE_RULE_IDENTITY, DSL_EFFECT_MODEL_PURE,
        DSL_LOWERING_MODEL_RUNTIME_CALL, "DOPC_COMMON_OUTPUT_LOGITS_V2",
        "attr.semantic" },
    { OPR_DSLOUTPUTLOGITS, "common.output_logits", 3,
        DSL_OPCODE_CATEGORY_EXECUTABLE, DSL_OPCODE_LEVEL_3_NN_COMMON, 1,
        DSL_SHAPE_RULE_IDENTITY, DSL_EFFECT_MODEL_PURE,
        DSL_LOWERING_MODEL_RUNTIME_CALL, "DOPC_COMMON_OUTPUT_LOGITS_V3",
        "attr.semantic;attr.sequence_axis;attr.vocabulary_axis" },
    { OPR_DSLCONV2D, "cnn.conv2d", 2,
        DSL_OPCODE_CATEGORY_EXECUTABLE, DSL_OPCODE_LEVEL_3_NN_COMMON, 3,
        DSL_SHAPE_RULE_CONTRACTION, DSL_EFFECT_MODEL_PURE,
        DSL_LOWERING_MODEL_RUNTIME_CALL, "DOPC_CNN_CONV2D_V2",
        "attr.kernel_shape;attr.stride;attr.padding;attr.dilation;attr.groups;"
        "attr.input_layout;attr.weight_layout;attr.output_layout" },
    { OPR_DSLBATCHNORMINFER, "cnn.batch_norm_infer", 2,
        DSL_OPCODE_CATEGORY_EXECUTABLE, DSL_OPCODE_LEVEL_3_NN_COMMON, 5,
        DSL_SHAPE_RULE_IDENTITY, DSL_EFFECT_MODEL_PURE,
        DSL_LOWERING_MODEL_RUNTIME_CALL, "DOPC_CNN_BATCH_NORM_INFER_V2",
        "attr.epsilon;attr.training;attr.input_layout;attr.channel_axis" },
    { OPR_DSLMAXPOOL2D, "cnn.max_pool2d", 2,
        DSL_OPCODE_CATEGORY_EXECUTABLE, DSL_OPCODE_LEVEL_3_NN_COMMON, 1,
        DSL_SHAPE_RULE_REDUCTION, DSL_EFFECT_MODEL_PURE,
        DSL_LOWERING_MODEL_RUNTIME_CALL, "DOPC_CNN_MAX_POOL2D_V2",
        "attr.kernel_shape;attr.stride;attr.padding;attr.dilation;"
        "attr.ceil_mode" },
    { OPR_DSLGLOBALAVGPOOL2D, "cnn.global_avg_pool2d", 2,
        DSL_OPCODE_CATEGORY_EXECUTABLE, DSL_OPCODE_LEVEL_3_NN_COMMON, 1,
        DSL_SHAPE_RULE_REDUCTION, DSL_EFFECT_MODEL_PURE,
        DSL_LOWERING_MODEL_RUNTIME_CALL, "DOPC_CNN_GLOBAL_AVG_POOL2D_V2",
        "attr.output_size;attr.reduction_axes" },
    { OPR_DSLRESHAPE, "common.reshape", 1,
        DSL_OPCODE_CATEGORY_EXECUTABLE, DSL_OPCODE_LEVEL_1_TENSOR, 1,
        DSL_SHAPE_RULE_VIEW, DSL_EFFECT_MODEL_PURE,
        DSL_LOWERING_MODEL_RUNTIME_CALL, "DOPC_COMMON_RESHAPE_V1",
        "attr.target_shape" },
    { OPR_DSLTRANSPOSE, "common.transpose", 1,
        DSL_OPCODE_CATEGORY_EXECUTABLE, DSL_OPCODE_LEVEL_1_TENSOR, 1,
        DSL_SHAPE_RULE_VIEW, DSL_EFFECT_MODEL_PURE,
        DSL_LOWERING_MODEL_RUNTIME_CALL, "DOPC_COMMON_TRANSPOSE_V1",
        "attr.permutation" },
    { OPR_DSLTOKENEMBEDDING, "transformer.token_embedding", 1,
        DSL_OPCODE_CATEGORY_EXECUTABLE, DSL_OPCODE_LEVEL_3_NN_COMMON, 2,
        DSL_SHAPE_RULE_OPAQUE, DSL_EFFECT_MODEL_PURE,
        DSL_LOWERING_MODEL_RUNTIME_CALL,
        "DOPC_TRANSFORMER_TOKEN_EMBEDDING_V1",
        "attr.padding_idx;attr.bounds_policy" },
    { OPR_DSLRMSNORM, "transformer.rms_norm", 1,
        DSL_OPCODE_CATEGORY_EXECUTABLE, DSL_OPCODE_LEVEL_3_NN_COMMON, 2,
        DSL_SHAPE_RULE_REDUCTION, DSL_EFFECT_MODEL_PURE,
        DSL_LOWERING_MODEL_RUNTIME_CALL, "DOPC_TRANSFORMER_RMS_NORM_V1",
        "attr.axis;attr.epsilon;attr.accum_dtype" },
    { OPR_DSLROTARYEMBEDDING, "transformer.rotary_embedding", 1,
        DSL_OPCODE_CATEGORY_EXECUTABLE, DSL_OPCODE_LEVEL_3_NN_COMMON, 3,
        DSL_SHAPE_RULE_IDENTITY, DSL_EFFECT_MODEL_PURE,
        DSL_LOWERING_MODEL_RUNTIME_CALL,
        "DOPC_TRANSFORMER_ROTARY_EMBEDDING_V1",
        "attr.head_layout;attr.sequence_axis;attr.feature_axis;attr.pairing;"
        "attr.position_mode;attr.position_offset" },
    { OPR_DSLROTARYEMBEDDING, "transformer.rotary_embedding", 2,
        DSL_OPCODE_CATEGORY_EXECUTABLE, DSL_OPCODE_LEVEL_3_NN_COMMON, 4,
        DSL_SHAPE_RULE_IDENTITY, DSL_EFFECT_MODEL_PURE,
        DSL_LOWERING_MODEL_RUNTIME_CALL,
        "DOPC_TRANSFORMER_ROTARY_EMBEDDING_V2",
        "attr.head_layout;attr.sequence_axis;attr.feature_axis;attr.pairing;"
        "attr.position_mode" },
    { OPR_DSLATTENTION, "transformer.attention", 1,
        DSL_OPCODE_CATEGORY_EXECUTABLE, DSL_OPCODE_LEVEL_3_NN_COMMON, 3,
        DSL_SHAPE_RULE_CONTRACTION, DSL_EFFECT_MODEL_PURE,
        DSL_LOWERING_MODEL_RUNTIME_CALL, "DOPC_TRANSFORMER_ATTENTION_V1",
        "attr.execution_mode;attr.mask_mode;attr.head_layout;"
        "attr.query_heads;attr.kv_heads;attr.head_dim;attr.scale_mode;"
        "attr.softmax_axis;attr.softmax_accum_dtype;attr.cache_mode" },
    { OPR_DSLATTENTION, "transformer.attention", 2,
        DSL_OPCODE_CATEGORY_EXECUTABLE, DSL_OPCODE_LEVEL_3_NN_COMMON, 3,
        DSL_SHAPE_RULE_CONTRACTION, DSL_EFFECT_MODEL_RUNTIME_EFFECT,
        DSL_LOWERING_MODEL_RUNTIME_CALL, "DOPC_TRANSFORMER_ATTENTION_V2",
        "attr.execution_mode;attr.mask_mode;attr.head_layout;"
        "attr.query_heads;attr.kv_heads;attr.head_dim;attr.scale_mode;"
        "attr.softmax_axis;attr.softmax_accum_dtype;attr.cache_mode;"
        "attr.cache_sequence_axis" },
    { OPR_DSLSWIGLU, "transformer.swiglu", 1,
        DSL_OPCODE_CATEGORY_EXECUTABLE, DSL_OPCODE_LEVEL_3_NN_COMMON, 2,
        DSL_SHAPE_RULE_BROADCAST, DSL_EFFECT_MODEL_PURE,
        DSL_LOWERING_MODEL_RUNTIME_CALL, "DOPC_TRANSFORMER_SWIGLU_V1",
        "attr.activation" },
    { OPR_DSLSCATTER, "common.scatter", 1,
        DSL_OPCODE_CATEGORY_EXECUTABLE, DSL_OPCODE_LEVEL_4_RUNTIME, 3,
        DSL_SHAPE_RULE_VIEW, DSL_EFFECT_MODEL_RUNTIME_EFFECT,
        DSL_LOWERING_MODEL_RUNTIME_CALL, "DOPC_COMMON_SCATTER_V1",
        "attr.axis" },
    { OPR_DSLMUL, "common.mul", 1,
        DSL_OPCODE_CATEGORY_EXECUTABLE, DSL_OPCODE_LEVEL_2_NUMERIC, 2,
        DSL_SHAPE_RULE_BROADCAST, DSL_EFFECT_MODEL_PURE,
        DSL_LOWERING_MODEL_MARKER_ONLY, "DOPC_COMMON_MUL",
        "attr.broadcast_rule" },
    { OPR_DSLDIV, "common.div", 1,
        DSL_OPCODE_CATEGORY_EXECUTABLE, DSL_OPCODE_LEVEL_2_NUMERIC, 2,
        DSL_SHAPE_RULE_BROADCAST, DSL_EFFECT_MODEL_PURE,
        DSL_LOWERING_MODEL_MARKER_ONLY, "DOPC_COMMON_DIV",
        "attr.broadcast_rule" },
    { OPR_DSLREM, "common.rem", 1,
        DSL_OPCODE_CATEGORY_EXECUTABLE, DSL_OPCODE_LEVEL_2_NUMERIC, 2,
        DSL_SHAPE_RULE_BROADCAST, DSL_EFFECT_MODEL_PURE,
        DSL_LOWERING_MODEL_MARKER_ONLY, "DOPC_COMMON_REM",
        "attr.broadcast_rule" },
    { OPR_DSLDIVREM, "common.divrem", 1,
        DSL_OPCODE_CATEGORY_EXECUTABLE, DSL_OPCODE_LEVEL_2_NUMERIC, 2,
        DSL_SHAPE_RULE_BROADCAST, DSL_EFFECT_MODEL_PURE,
        DSL_LOWERING_MODEL_MARKER_ONLY, "DOPC_COMMON_DIVREM",
        "attr.broadcast_rule" },
    { OPR_DSLDIVPART, "common.divpart", 1,
        DSL_OPCODE_CATEGORY_EXECUTABLE, DSL_OPCODE_LEVEL_2_NUMERIC, 1,
        DSL_SHAPE_RULE_IDENTITY, DSL_EFFECT_MODEL_PURE,
        DSL_LOWERING_MODEL_MARKER_ONLY, "DOPC_COMMON_DIVPART", "" },
    { OPR_DSLREMPART, "common.rempart", 1,
        DSL_OPCODE_CATEGORY_EXECUTABLE, DSL_OPCODE_LEVEL_2_NUMERIC, 1,
        DSL_SHAPE_RULE_IDENTITY, DSL_EFFECT_MODEL_PURE,
        DSL_LOWERING_MODEL_MARKER_ONLY, "DOPC_COMMON_REMPART", "" },
    /* CKKS operators remain distinct from generic broadcast/algebraic rules. */
    { OPR_DSLCKKSADD, "ckks.add", 1,
        DSL_OPCODE_CATEGORY_EXECUTABLE, DSL_OPCODE_LEVEL_2_NUMERIC, 2,
        DSL_SHAPE_RULE_OPAQUE, DSL_EFFECT_MODEL_PURE,
        DSL_LOWERING_MODEL_RUNTIME_CALL, "DOPC_CKKS_ADD", "" },
    { OPR_DSLCKKSSUB, "ckks.sub", 1,
        DSL_OPCODE_CATEGORY_EXECUTABLE, DSL_OPCODE_LEVEL_2_NUMERIC, 2,
        DSL_SHAPE_RULE_OPAQUE, DSL_EFFECT_MODEL_PURE,
        DSL_LOWERING_MODEL_RUNTIME_CALL, "DOPC_CKKS_SUB", "" },
    { OPR_DSLCKKSMUL, "ckks.mul", 1,
        DSL_OPCODE_CATEGORY_EXECUTABLE, DSL_OPCODE_LEVEL_2_NUMERIC, 2,
        DSL_SHAPE_RULE_OPAQUE, DSL_EFFECT_MODEL_PURE,
        DSL_LOWERING_MODEL_RUNTIME_CALL, "DOPC_CKKS_MUL", "" },
    { OPR_DSLCKKSENCODE, "ckks.encode", 1,
        DSL_OPCODE_CATEGORY_EXECUTABLE, DSL_OPCODE_LEVEL_2_NUMERIC, 1,
        DSL_SHAPE_RULE_OPAQUE, DSL_EFFECT_MODEL_PURE,
        DSL_LOWERING_MODEL_RUNTIME_CALL, "DOPC_CKKS_ENCODE", "" },
    { OPR_DSLCKKSROTATE, "ckks.rotate", 1,
        DSL_OPCODE_CATEGORY_EXECUTABLE, DSL_OPCODE_LEVEL_2_NUMERIC, 1,
        DSL_SHAPE_RULE_OPAQUE, DSL_EFFECT_MODEL_PURE,
        DSL_LOWERING_MODEL_RUNTIME_CALL, "DOPC_CKKS_ROTATE",
        "attr.signed_steps;attr.key_id" },
    { OPR_DSLCKKSRESCALE, "ckks.rescale", 1,
        DSL_OPCODE_CATEGORY_EXECUTABLE, DSL_OPCODE_LEVEL_2_NUMERIC, 1,
        DSL_SHAPE_RULE_OPAQUE, DSL_EFFECT_MODEL_PURE,
        DSL_LOWERING_MODEL_RUNTIME_CALL, "DOPC_CKKS_RESCALE",
        "attr.levels;attr.target_scale_bits" },
    { OPR_DSLCKKSMODSWITCH, "ckks.modswitch", 1,
        DSL_OPCODE_CATEGORY_EXECUTABLE, DSL_OPCODE_LEVEL_2_NUMERIC, 1,
        DSL_SHAPE_RULE_OPAQUE, DSL_EFFECT_MODEL_PURE,
        DSL_LOWERING_MODEL_RUNTIME_CALL, "DOPC_CKKS_MODSWITCH",
        "attr.target_level" },
    { OPR_DSLCKKSRELIN, "ckks.relin", 1,
        DSL_OPCODE_CATEGORY_EXECUTABLE, DSL_OPCODE_LEVEL_2_NUMERIC, 1,
        DSL_SHAPE_RULE_OPAQUE, DSL_EFFECT_MODEL_PURE,
        DSL_LOWERING_MODEL_RUNTIME_CALL, "DOPC_CKKS_RELIN",
        "attr.key_id" },
    { OPR_DSLCKKSBOOTSTRAP, "ckks.bootstrap", 1,
        DSL_OPCODE_CATEGORY_EXECUTABLE, DSL_OPCODE_LEVEL_2_NUMERIC, 1,
        DSL_SHAPE_RULE_OPAQUE, DSL_EFFECT_MODEL_PURE,
        DSL_LOWERING_MODEL_RUNTIME_CALL, "DOPC_CKKS_BOOTSTRAP",
        "attr.target_level;attr.reason;attr.key_id" },
    { OPR_DSLREDUCESUM, "common.reduce_sum", 1,
        DSL_OPCODE_CATEGORY_EXECUTABLE, DSL_OPCODE_LEVEL_2_NUMERIC, 1,
        DSL_SHAPE_RULE_REDUCTION, DSL_EFFECT_MODEL_PURE,
        DSL_LOWERING_MODEL_MARKER_ONLY, "DOPC_COMMON_REDUCE_SUM_V1",
        "attr.axis;attr.keepdim;attr.accum_dtype;attr.reassociate" },
    { OPR_DSLSTABLESOFTMAX, "common.stable_softmax", 1,
        DSL_OPCODE_CATEGORY_EXECUTABLE, DSL_OPCODE_LEVEL_2_NUMERIC, 1,
        DSL_SHAPE_RULE_IDENTITY, DSL_EFFECT_MODEL_PURE,
        DSL_LOWERING_MODEL_MARKER_ONLY, "DOPC_COMMON_STABLE_SOFTMAX_V1",
        "attr.axis;attr.accum_dtype;attr.reassociate;"
        "attr.state_reformulation;attr.exp_semantics" },
    { OPR_DSLMEANVARNORM, "common.mean_variance_norm", 1,
        DSL_OPCODE_CATEGORY_EXECUTABLE, DSL_OPCODE_LEVEL_2_NUMERIC, 3,
        DSL_SHAPE_RULE_IDENTITY, DSL_EFFECT_MODEL_PURE,
        DSL_LOWERING_MODEL_MARKER_ONLY, "DOPC_COMMON_MEAN_VAR_NORM_V1",
        "attr.axis;attr.accum_dtype;attr.variance_kind;attr.epsilon_bits;"
        "attr.reassociate;attr.state_reformulation;attr.contract;attr.rsqrt_semantics" },
    /* Ordered reduction is explicit source meaning, never a v1 upgrade. */
    { OPR_DSLREDUCESUM, "common.reduce_sum", 2,
        DSL_OPCODE_CATEGORY_EXECUTABLE, DSL_OPCODE_LEVEL_2_NUMERIC, 1,
        DSL_SHAPE_RULE_REDUCTION, DSL_EFFECT_MODEL_PURE,
        DSL_LOWERING_MODEL_MARKER_ONLY, "DOPC_COMMON_REDUCE_SUM_V2",
        "attr.axis;attr.keepdim;attr.accum_dtype;attr.reassociate;attr.order;"
        "attr.identity_bits;attr.fp_environment;attr.nan_policy" },
    /* LN-REF-1 is explicit source meaning, never a normalization v1 upgrade.
     * Design: doc/AI-COMPILER-LAYERNORM-NATIVE-REFERENCE-CONTRACT-v0.1.md. */
    { OPR_DSLMEANVARNORM, "common.mean_variance_norm", 2,
        DSL_OPCODE_CATEGORY_EXECUTABLE, DSL_OPCODE_LEVEL_2_NUMERIC, 3,
        DSL_SHAPE_RULE_IDENTITY, DSL_EFFECT_MODEL_PURE,
        DSL_LOWERING_MODEL_MARKER_ONLY, "DOPC_COMMON_MEAN_VAR_NORM_V2",
        "attr.axis;attr.accum_dtype;attr.variance_kind;attr.epsilon_bits;"
        "attr.reassociate;attr.state_reformulation;attr.contract;attr.rsqrt_semantics;"
        "attr.mean_order;attr.mean_identity_bits;attr.mean_finalize;"
        "attr.count_conversion;attr.fp_environment;attr.nan_policy;attr.reference_binding" }
};

static const DSL_FUSIBILITY_INFO DSL_fusibility_info[] = {
    { OPR_DSLADD, 1, 0, DSL_FUSION_ITERATION_POINTWISE,
        DSL_FUSION_INDEXING_BROADCAST,
        DSL_FUSIBILITY_PRODUCER | DSL_FUSIBILITY_CONSUMER |
        DSL_FUSIBILITY_EXACT_DESCRIPTOR | DSL_FUSIBILITY_SINGLE_RESULT },
    { OPR_DSLMATMUL, 1, 0, DSL_FUSION_ITERATION_CONTRACTION,
        DSL_FUSION_INDEXING_CONTRACTION,
        DSL_FUSIBILITY_PRODUCER | DSL_FUSIBILITY_CONSUMER |
        DSL_FUSIBILITY_CLUSTER_ANCHOR |
        DSL_FUSIBILITY_EXACT_DESCRIPTOR | DSL_FUSIBILITY_SINGLE_RESULT },
    { OPR_DSLMATMUL, 2, 0, DSL_FUSION_ITERATION_CONTRACTION,
        DSL_FUSION_INDEXING_CONTRACTION,
        DSL_FUSIBILITY_PRODUCER | DSL_FUSIBILITY_CONSUMER |
        DSL_FUSIBILITY_CLUSTER_ANCHOR |
        DSL_FUSIBILITY_EXACT_DESCRIPTOR | DSL_FUSIBILITY_SINGLE_RESULT },
    { OPR_DSLRELU, 2, 0, DSL_FUSION_ITERATION_POINTWISE,
        DSL_FUSION_INDEXING_IDENTITY,
        DSL_FUSIBILITY_PRODUCER | DSL_FUSIBILITY_CONSUMER |
        DSL_FUSIBILITY_EXACT_DESCRIPTOR | DSL_FUSIBILITY_SINGLE_RESULT },
    { OPR_DSLRESIDUALADD, 2, 0, DSL_FUSION_ITERATION_POINTWISE,
        DSL_FUSION_INDEXING_BROADCAST,
        DSL_FUSIBILITY_SEMANTIC_PATTERN_REQUIRED |
        DSL_FUSIBILITY_EXACT_DESCRIPTOR | DSL_FUSIBILITY_SINGLE_RESULT },
    { OPR_DSLLINEAR, 2, 0, DSL_FUSION_ITERATION_CONTRACTION,
        DSL_FUSION_INDEXING_CONTRACTION,
        DSL_FUSIBILITY_PRODUCER | DSL_FUSIBILITY_CONSUMER |
        DSL_FUSIBILITY_CLUSTER_ANCHOR |
        DSL_FUSIBILITY_EXACT_DESCRIPTOR | DSL_FUSIBILITY_SINGLE_RESULT },
    { OPR_DSLLINEAR, 3, 0, DSL_FUSION_ITERATION_CONTRACTION,
        DSL_FUSION_INDEXING_CONTRACTION,
        DSL_FUSIBILITY_PRODUCER | DSL_FUSIBILITY_CONSUMER |
        DSL_FUSIBILITY_CLUSTER_ANCHOR |
        DSL_FUSIBILITY_EXACT_DESCRIPTOR | DSL_FUSIBILITY_SINGLE_RESULT },
    { OPR_DSLMUL, 1, 0, DSL_FUSION_ITERATION_POINTWISE,
        DSL_FUSION_INDEXING_BROADCAST,
        DSL_FUSIBILITY_PRODUCER | DSL_FUSIBILITY_CONSUMER |
        DSL_FUSIBILITY_EXACT_DESCRIPTOR | DSL_FUSIBILITY_SINGLE_RESULT }
};

/*
 * Select the greatest static version for an operator, keeping the first tie.
 * Return a borrowed process-lifetime seed or NULL; do not populate registries.
 */
static const DSL_LOGICAL_OPERATOR_SEED *
DSL_Operator_Seed (DSL_OPERATOR dsl_operator)
{
    const DSL_LOGICAL_OPERATOR_SEED *current = NULL;

    for (UINT32 i = 0; i < DSL_ARRAY_COUNT(DSL_logical_operator_seed); ++i) {
        if (DSL_logical_operator_seed[i].dsl_operator == dsl_operator &&
            (current == NULL ||
             DSL_logical_operator_seed[i].version > current->version))
            current = &DSL_logical_operator_seed[i];
    }

    return current;
}

/* Borrow the first exact static operator/version seed, or NULL without writes. */
static const DSL_LOGICAL_OPERATOR_SEED *
DSL_Operator_Seed_Version
        (DSL_OPERATOR dsl_operator,
         UINT16 version)
{
    for (UINT32 i = 0; i < DSL_ARRAY_COUNT(DSL_logical_operator_seed); ++i) {
        if (DSL_logical_operator_seed[i].dsl_operator == dsl_operator &&
            DSL_logical_operator_seed[i].version == version)
            return &DSL_logical_operator_seed[i];
    }

    return NULL;
}

/*
 * Reject absent/unknown seeds before output writes. Copy scalars plus borrowed
 * static strings, setting flags to zero; NULL output is a presence probe.
 */
static BOOL
DSL_Operator_Copy_Info
        (const DSL_LOGICAL_OPERATOR_SEED *seed,
         DSL_OPERATOR_INFO *info)
{
    if (seed == NULL || seed->dsl_operator == OPR_DSLUNKNOWN)
        return FALSE;

    if (info != NULL) {
        info->dsl_operator = seed->dsl_operator;
        info->logical_name = DSL_OPERATOR_name(seed->dsl_operator);
        info->stable_name = seed->name;
        info->version = seed->version;
        info->category = seed->category;
        info->level = seed->level;
        info->nkids = seed->nkids;
        info->shape_rule = seed->shape_rule;
        info->effect_model = seed->effect_model;
        info->lowering_model = seed->lowering_model;
        info->diagnostic_prefix = seed->diagnostic_prefix;
        info->attribute_schema = seed->attribute_schema;
        info->flags = 0;
    }

    return TRUE;
}

/* Select the highest static version, then share Copy_Info's output contract. */
BOOL
DSL_Operator_Get_Info
        (DSL_OPERATOR dsl_operator,
         DSL_OPERATOR_INFO *info)
{
    const DSL_LOGICAL_OPERATOR_SEED *seed = DSL_Operator_Seed(dsl_operator);

    return DSL_Operator_Copy_Info(seed, info);
}

/* Keep exact-version queries separate from current-version metadata selection. */
BOOL
DSL_Operator_Get_Info_Version
        (DSL_OPERATOR dsl_operator,
         UINT16 version,
         DSL_OPERATOR_INFO *info)
{
    const DSL_LOGICAL_OPERATOR_SEED *seed =
        DSL_Operator_Seed_Version(dsl_operator, version);

    return DSL_Operator_Copy_Info(seed, info);
}

/*
 * Match static seed name length/bytes and version exactly, with no allocation.
 * Reject null/empty names and version zero before reading caller name bytes.
 */
DSL_OPERATOR
DSL_Operator_Find
        (const char *stable_name,
         UINT32 stable_name_len,
         UINT16 version)
{
    if (stable_name == NULL || stable_name_len == 0 || version == 0)
        return OPR_DSLUNKNOWN;

    for (UINT32 i = 0; i < DSL_ARRAY_COUNT(DSL_logical_operator_seed); ++i) {
        const DSL_LOGICAL_OPERATOR_SEED &seed = DSL_logical_operator_seed[i];

        if (seed.dsl_operator == OPR_DSLUNKNOWN ||
            seed.version != version ||
            strlen(seed.name) != stable_name_len)
            continue;
        if (strncmp(seed.name, stable_name, stable_name_len) == 0)
            return seed.dsl_operator;
    }

    return OPR_DSLUNKNOWN;
}

/*
 * Resolve a name span in table order without filtering or ranking versions.
 * Return only operator identity; highest-version metadata is a separate query.
 */
DSL_OPERATOR
DSL_Operator_Find_Current
        (const char *stable_name,
         UINT32 stable_name_len)
{
    if (stable_name == NULL || stable_name_len == 0)
        return OPR_DSLUNKNOWN;

    for (UINT32 i = 0; i < DSL_ARRAY_COUNT(DSL_logical_operator_seed); ++i) {
        const DSL_LOGICAL_OPERATOR_SEED &seed = DSL_logical_operator_seed[i];

        if (seed.dsl_operator == OPR_DSLUNKNOWN ||
            strlen(seed.name) != stable_name_len)
            continue;
        if (strncmp(seed.name, stable_name, stable_name_len) == 0)
            return seed.dsl_operator;
    }

    return OPR_DSLUNKNOWN;
}

/* Range-check the logical spelling table and borrow its unknown fallback. */
const char *
DSL_OPERATOR_name (DSL_OPERATOR dsl_operator)
{
    return dsl_operator >= OPR_DSLUNKNOWN &&
           (UINT32)dsl_operator < DSL_ARRAY_COUNT(DSL_operator_name) ?
           DSL_operator_name[dsl_operator] :
           DSL_operator_name[OPR_DSLUNKNOWN];
}

static const DSL_ALGEBRAIC_INFO DSL_algebraic_info[] = {
    { OPR_DSLADD, 1, OPR_DSLADD, DSL_ALGEBRAIC_IDENTITY_ZERO,
        DSL_ALGEBRAIC_SAFETY_INTEGER,
        DSL_ALGEBRAIC_SAFETY_INTEGER_OR_FP_REASSOCIATE,
        DSL_ALGEBRAIC_ALLOW_COMMUTATION |
        DSL_ALGEBRAIC_ALLOW_REASSOCIATION },
    { OPR_DSLMUL, 1, OPR_DSLMUL, DSL_ALGEBRAIC_IDENTITY_ONE,
        DSL_ALGEBRAIC_SAFETY_INTEGER,
        DSL_ALGEBRAIC_SAFETY_INTEGER_OR_FP_REASSOCIATE,
        DSL_ALGEBRAIC_ALLOW_COMMUTATION |
        DSL_ALGEBRAIC_ALLOW_REASSOCIATION }
};

static const DSL_ALGEBRAIC_RELATION_INFO DSL_algebraic_relation[] = {
    { OPR_DSLADD, 1, OPR_DSLMUL, 1, DSL_ALGEBRAIC_RELATION_FACTOR,
        DSL_ALGEBRAIC_SAFETY_INTEGER_OR_FP_REASSOCIATE,
        DSL_ALGEBRAIC_OPERAND_ALL },
    { OPR_DSLMUL, 1, OPR_DSLADD, 1,
        DSL_ALGEBRAIC_RELATION_DISTRIBUTE,
        DSL_ALGEBRAIC_SAFETY_INTEGER_OR_FP_REASSOCIATE,
        DSL_ALGEBRAIC_OPERAND_ALL }
};

/* Copy an exact static trait if requested; a miss preserves caller output. */
BOOL
DSL_Operator_Get_Algebraic_Info
        (DSL_OPERATOR dsl_operator,
         UINT16 version,
         DSL_ALGEBRAIC_INFO *info)
{
    for (UINT32 i = 0; i < DSL_ARRAY_COUNT(DSL_algebraic_info); ++i) {
        if (DSL_algebraic_info[i].dsl_operator != dsl_operator ||
            DSL_algebraic_info[i].version != version)
            continue;
        if (info != NULL)
            *info = DSL_algebraic_info[i];
        return TRUE;
    }

    return FALSE;
}

/*
 * Unlike algebraic lookup, require output and clear it on a missing exact trait.
 * Only static semantic evidence is supplied; no fusion candidate is selected.
 */
BOOL
DSL_Operator_Get_Fusibility_Info
        (DSL_OPERATOR dsl_operator, UINT16 version,
         DSL_FUSIBILITY_INFO *info)
{
    if (info == NULL)
        return FALSE;
    for (UINT32 i = 0; i < DSL_ARRAY_COUNT(DSL_fusibility_info); ++i) {
        if (DSL_fusibility_info[i].dsl_operator == dsl_operator &&
            DSL_fusibility_info[i].version == version) {
            *info = DSL_fusibility_info[i];
            return TRUE;
        }
    }
    memset(info, 0, sizeof(*info));
    return FALSE;
}

/* Bounds-check and borrow a static iteration spelling, falling back to unknown. */
const char *
DSL_fusion_iteration_class_name (UINT32 iteration_space)
{
    return iteration_space <
             DSL_ARRAY_COUNT(DSL_fusion_iteration_class_name_table) ?
           DSL_fusion_iteration_class_name_table[iteration_space] : "unknown";
}

/* Bounds-check and borrow a static indexing spelling, falling back to unknown. */
const char *
DSL_fusion_indexing_class_name (UINT32 operand_indexing)
{
    return operand_indexing <
             DSL_ARRAY_COUNT(DSL_fusion_indexing_class_name_table) ?
           DSL_fusion_indexing_class_name_table[operand_indexing] : "unknown";
}

/*
 * Filter the exact algebraic trait by its commutation flag and known swap ID.
 * NULL output probes availability; actual operand safety remains caller-owned.
 */
BOOL
DSL_Operator_Get_Swap_Equivalent
        (DSL_OPERATOR dsl_operator,
         UINT16 version,
         DSL_OPERATOR *swap_equivalent)
{
    DSL_ALGEBRAIC_INFO info;

    if (!DSL_Operator_Get_Algebraic_Info
             (dsl_operator, version, &info) ||
        (info.flags & DSL_ALGEBRAIC_ALLOW_COMMUTATION) == 0 ||
        info.swap_equivalent == OPR_DSLUNKNOWN)
        return FALSE;
    if (swap_equivalent != NULL)
        *swap_equivalent = info.swap_equivalent;
    return TRUE;
}

/*
 * Require caller-supplied integer/descriptor facts and the integer-safe trait
 * before choosing constant-right or lexical key order. Return a decision only;
 * this function does not swap operands or duplicate simplifier rewrite rules.
 */
BOOL
DSL_Algebraic_Should_Swap_Binary_Operands
        (DSL_OPERATOR dsl_operator,
         UINT16 version,
         BOOL integer_operands,
         BOOL equivalent_descriptors,
         BOOL kid0_is_constant,
         BOOL kid1_is_constant,
         const char *kid0_key,
         const char *kid1_key)
{
    DSL_ALGEBRAIC_INFO info;

    if (!integer_operands || !equivalent_descriptors ||
        kid0_key == NULL || kid1_key == NULL ||
        !DSL_Operator_Get_Algebraic_Info
             (dsl_operator, version, &info) ||
        (info.flags & DSL_ALGEBRAIC_ALLOW_COMMUTATION) == 0 ||
        info.commutation_safety != DSL_ALGEBRAIC_SAFETY_INTEGER)
        return FALSE;

    /* Constant placement takes precedence over lexical canonical ordering. */
    if (kid0_is_constant != kid1_is_constant)
        return kid0_is_constant;
    return strcmp(kid0_key, kid1_key) > 0;
}

/* Return the immutable relation-table size, unaffected by dynamic reset. */
UINT32
DSL_Algebraic_Relation_Count (void)
{
    return DSL_ARRAY_COUNT(DSL_algebraic_relation);
}

/* Bounds-check a static ordinal before an optional copy; failure writes nothing. */
BOOL
DSL_Algebraic_Relation_At
        (UINT32 ordinal,
         DSL_ALGEBRAIC_RELATION_INFO *info)
{
    if (ordinal >= DSL_ARRAY_COUNT(DSL_algebraic_relation))
        return FALSE;
    if (info != NULL)
        *info = DSL_algebraic_relation[ordinal];
    return TRUE;
}

/*
 * Scan all relation key fields exactly, optionally copying the first match.
 * No safety predicate is executed and no caller output changes on a miss.
 */
BOOL
DSL_Algebraic_Relation_Get
        (DSL_OPERATOR outer_operator,
         UINT16 outer_version,
         DSL_OPERATOR inner_operator,
         UINT16 inner_version,
         DSL_ALGEBRAIC_RELATION relation,
         DSL_ALGEBRAIC_RELATION_INFO *info)
{
    for (UINT32 i = 0; i < DSL_ARRAY_COUNT(DSL_algebraic_relation); ++i) {
        const DSL_ALGEBRAIC_RELATION_INFO &candidate =
            DSL_algebraic_relation[i];
        if (candidate.outer_operator != outer_operator ||
            candidate.outer_version != outer_version ||
            candidate.inner_operator != inner_operator ||
            candidate.inner_version != inner_version ||
            candidate.relation != relation)
            continue;
        if (info != NULL)
            *info = candidate;
        return TRUE;
    }

    return FALSE;
}

static const DSL_DOMAIN_WRAPPER_SEED DSL_domain_wrapper_seed[] = {
    { "cnn", "cnn.linear", "common.linear", "DOPC_CNN_LINEAR_WRAPPER" },
    { "cnn", "cnn.max_pool2d", "common.window_reduce",
        "DOPC_CNN_MAX_POOL2D_WRAPPER" },
    { "cnn", "cnn.global_avg_pool2d", "common.reduce_mean",
        "DOPC_CNN_GLOBAL_AVG_POOL2D_WRAPPER" },
    { "cnn", "cnn.conv2d", "common.window_reduce",
        "DOPC_CNN_CONV2D_WRAPPER" },
    { "cnn", "cnn.batch_norm_infer", "common.normalization_base",
        "DOPC_CNN_BATCH_NORM_INFER_WRAPPER" },
    { "transformer", "transformer.q_projection", "common.linear",
        "DOPC_TRANSFORMER_Q_PROJECTION_WRAPPER" },
    { "cnn", "cnn.residual_add", "common.residual_add",
        "DOPC_CNN_RESIDUAL_ADD_WRAPPER" },
    { "transformer", "transformer.residual_add", "common.residual_add",
        "DOPC_TRANSFORMER_RESIDUAL_ADD_WRAPPER" }
};

static const DSL_OPCODE_PROMOTION_SEED DSL_opcode_promotion_seed[] = {
    { "cnn", "cnn.linear", "common.linear",
        DSL_OPCODE_PROMOTION_WRAPPER_TO_COMMON,
        "affine_projection", "cnn.linear", "classifier_head_contract",
        "CPROM-005" },
    { "cnn", "cnn.max_pool2d", "common.window_reduce",
        DSL_OPCODE_PROMOTION_WRAPPER_TO_COMMON,
        "windowed_max_reduction", "cnn.max_pool2d",
        "kernel_stride_padding_dilation", "CPROM-006" },
    { "cnn", "cnn.global_avg_pool2d", "common.reduce_mean",
        DSL_OPCODE_PROMOTION_WRAPPER_TO_COMMON,
        "spatial_mean_reduction", "cnn.global_avg_pool2d",
        "global_spatial_axes", "CPROM-006" },
    { "cnn", "cnn.batch_norm_infer", "common.normalization_base",
        DSL_OPCODE_PROMOTION_WRAPPER_TO_COMMON,
        "inference_normalization", "cnn.batch_norm_infer",
        "scale_bias_running_stats_epsilon", "CPROM-006" },
    { "transformer", "transformer.q_projection", "common.linear",
        DSL_OPCODE_PROMOTION_WRAPPER_TO_COMMON,
        "affine_projection", "transformer.q_projection",
        "head_layout_contract", "CPROM-005" },
    { "cnn", "cnn.residual_add", "common.residual_add",
        DSL_OPCODE_PROMOTION_WRAPPER_TO_COMMON,
        "residual_addition", "cnn.residual_add",
        "residual_shape_check", "CPROM-006" },
    { "transformer", "transformer.residual_add", "common.residual_add",
        DSL_OPCODE_PROMOTION_WRAPPER_TO_COMMON,
        "residual_addition", "transformer.residual_add",
        "residual_lineage_check", "CPROM-006" },
    { "cnn", "cnn.conv2d", "common.window_reduce",
        DSL_OPCODE_PROMOTION_PARTIAL,
        "windowed_contraction_base", "cnn.conv2d",
        "padding_stride_dilation_groups", "CPROM-006" },
    { "transformer", "transformer.attention", "common.matmul",
        DSL_OPCODE_PROMOTION_PARTIAL,
        "attention_matmul_softmax_sequence", "transformer.attention",
        "mask_position_head_layout_kv_cache", "CPROM-006" }
};

/* Allocate a record-owned copy, normalizing NULL to empty; failures propagate. */
static const char *
DSL_Opcode_Save_String (const char *str)
{
    const char *safe_str = str ? str : "";
    size_t len = strlen(safe_str) + 1;
    char *saved = new char[len];

    memcpy(saved, safe_str, len);
    return saved;
}

/* Check the one-based dynamic opcode range, without validating its owner. */
static BOOL
DSL_Opcode_Valid_Id (DSL_OPCODE_ID id)
{
    return id != DSL_OPCODE_INVALID_ID && id <= DSL_opcode_registry.size();
}

/* Check the one-based promotion range, not the liveness of its opcode fields. */
static BOOL
DSL_Opcode_Promotion_Valid_Id (DSL_OPCODE_PROMOTION_ID id)
{
    return id != DSL_OPCODE_PROMOTION_INVALID_ID &&
         id <= DSL_opcode_promotion_registry.size();
}

/* Accept only the declared contiguous category range; no registry mutation. */
static BOOL
DSL_Opcode_Valid_Category (DSL_OPCODE_CATEGORY category)
{
    return category >= DSL_OPCODE_CATEGORY_EXECUTABLE &&
         category <= DSL_OPCODE_CATEGORY_LOWERING_POLICY;
}

/* Accept the declared level range, without treating it as an optimization level. */
static BOOL
DSL_Opcode_Valid_Level (DSL_OPCODE_LEVEL level)
{
    return level >= DSL_OPCODE_LEVEL_0_CORE &&
         level <= DSL_OPCODE_LEVEL_4_RUNTIME;
}

/* Check shape-rule enum membership only, not any tensor's shape compatibility. */
static BOOL
DSL_Opcode_Valid_Shape_Rule (DSL_SHAPE_RULE shape_rule)
{
    return shape_rule >= DSL_SHAPE_RULE_OPAQUE &&
         shape_rule <= DSL_SHAPE_RULE_RUNTIME_GUARDED;
}

/* Check effect enum membership, without proving an operation's actual effects. */
static BOOL
DSL_Opcode_Valid_Effect_Model (DSL_EFFECT_MODEL effect_model)
{
    return effect_model >= DSL_EFFECT_MODEL_PURE &&
         effect_model <= DSL_EFFECT_MODEL_RUNTIME_EFFECT;
}

/* Check the lowering-model enum range, not availability of a lowering provider. */
static BOOL
DSL_Opcode_Valid_Lowering_Model (DSL_LOWERING_MODEL lowering_model)
{
    return lowering_model >= DSL_LOWERING_MODEL_MARKER_ONLY &&
         lowering_model <= DSL_LOWERING_MODEL_TARGET_SPECIFIC;
}

/* Check promotion-state enum membership; do not certify semantic promotion. */
static BOOL
DSL_Opcode_Valid_Promotion_State (DSL_OPCODE_PROMOTION_STATE state)
{
    return state >= DSL_OPCODE_PROMOTION_DOMAIN_ONLY &&
         state <= DSL_OPCODE_PROMOTION_COMMON_NATIVE;
}

/*
 * Delete the two owned strings before registry discard. Pointers are not reset,
 * so the same record must not be released twice or read as live afterward.
 */
static void
DSL_Opcode_Free_Record (DSL_OPCODE_RECORD &record)
{
    delete [] record.name;
    delete [] record.diagnostic_prefix;
}

/* Free owned evidence strings and empty vectors before the record is discarded. */
static void
DSL_Opcode_Promotion_Free_Record (DSL_OPCODE_PROMOTION_RECORD &record)
{
    for (UINT32 i = 0; i < record.required_common_semantics.size(); ++i)
        delete [] record.required_common_semantics[i];
    record.required_common_semantics.clear();

    for (UINT32 i = 0; i < record.retained_wrappers.size(); ++i)
        delete [] record.retained_wrappers[i];
    record.retained_wrappers.clear();

    for (UINT32 i = 0; i < record.required_verifier_checks.size(); ++i)
        delete [] record.required_verifier_checks[i];
    record.required_verifier_checks.clear();

    for (UINT32 i = 0; i < record.diagnostics.size(); ++i)
        delete [] record.diagnostics[i];
    record.diagnostics.clear();
}

/* Retire all promotion IDs/payloads, leaving dynamic opcode and static data live. */
void
DSL_Opcode_Promotion_Registry_Reset (void)
{
    for (UINT32 i = 0; i < DSL_opcode_promotion_registry.size(); ++i)
        DSL_Opcode_Promotion_Free_Record(DSL_opcode_promotion_registry[i]);

    DSL_opcode_promotion_registry.clear();
}

/*
 * Retire dependent promotions before releasing opcode strings and entries.
 * Domain lifetime and any other external ID holders remain caller-owned.
 */
void
DSL_Opcode_Registry_Reset (void)
{
    DSL_Opcode_Promotion_Registry_Reset();

    for (UINT32 i = 0; i < DSL_opcode_registry.size(); ++i)
        DSL_Opcode_Free_Record(DSL_opcode_registry[i]);

    DSL_opcode_registry.clear();
}

/*
 * Preserve first-writer metadata by returning a duplicate before owner/enum
 * checks. For a new key, validate those fields, copy strings and append a
 * one-based record. nkids/flags remain opaque; allocation failures are not
 * caught or rolled back, including partially allocated string payloads.
 */
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

    /* Duplicate lookup must not replace existing semantics or wrapper links. */
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
    record.id = DSL_opcode_registry.size() + 1;
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

    DSL_opcode_registry.push_back(record);
    return record.id;
}

/* Scan exact owner/version/name keys; do not revalidate domains during lookup. */
DSL_OPCODE_ID
DSL_Opcode_Find (DSL_DOMAIN_ID owner_domain_id,
                 const char *name,
                 UINT16 version)
{
    const char *safe_name = name ? name : "";

    for (UINT32 i = 0; i < DSL_opcode_registry.size(); ++i) {
        const DSL_OPCODE_RECORD &record = DSL_opcode_registry[i];
        if (record.owner_domain_id == owner_domain_id &&
        record.version == version &&
        strcmp(record.name, safe_name) == 0)
            return record.id;
    }

    return DSL_OPCODE_INVALID_ID;
}

/*
 * Validate before optional output assignment; copy fields and borrowed strings.
 * NULL output succeeds for a live ID without allocating or mutating records.
 */
BOOL
DSL_Opcode_Get_Info (DSL_OPCODE_ID id, DSL_OPCODE_INFO *info)
{
    if (!DSL_Opcode_Valid_Id(id))
        return FALSE;

    if (info != NULL) {
        const DSL_OPCODE_RECORD &record = DSL_opcode_registry[id - 1];
        info->id = record.id;
        info->owner_domain_id = record.owner_domain_id;
        info->wrapper_target_id = record.wrapper_target_id;
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

/*
 * Snapshot a live target's fields before registration can grow the vector.
 * Reused keys retain old metadata; reject conflicting links before writing
 * only wrapper_target_id. Do not require common ownership, matching semantics
 * on reused keys or acyclic wrapper chains. Registration exceptions propagate.
 */
DSL_OPCODE_ID
DSL_Opcode_Register_Domain_Wrapper (DSL_DOMAIN_ID owner_domain_id,
                                    const char *name,
                                    UINT16 version,
                                    DSL_OPCODE_ID wrapper_target_id,
                                    const char *diagnostic_prefix,
                                    UINT32 flags)
{
    DSL_OPCODE_INFO target_info;
    DSL_OPCODE_ID id;

    if (!DSL_Opcode_Get_Info(wrapper_target_id, &target_info))
        return DSL_OPCODE_INVALID_ID;

    id = DSL_Opcode_Register(owner_domain_id,
                           name,
                           version,
                           target_info.category,
                           target_info.level,
                           target_info.nkids,
                           target_info.shape_rule,
                           target_info.effect_model,
                           target_info.lowering_model,
                           diagnostic_prefix,
                           flags);

    if (!DSL_Opcode_Valid_Id(id))
        return DSL_OPCODE_INVALID_ID;

    /* An existing unwrapped key can gain a link, but never change a set link. */
    if (DSL_opcode_registry[id - 1].wrapper_target_id !=
        DSL_OPCODE_INVALID_ID &&
            DSL_opcode_registry[id - 1].wrapper_target_id != wrapper_target_id)
        return DSL_OPCODE_INVALID_ID;

    DSL_opcode_registry[id - 1].wrapper_target_id = wrapper_target_id;
    return id;
}

/* Expose the dynamic vector size through the public count type, read-only. */
UINT32
DSL_Opcode_Count (void)
{
    return DSL_opcode_registry.size();
}

/* Bounds-check the zero-based ordinal, then share Get_Info's output semantics. */
BOOL
DSL_Opcode_At (UINT32 ordinal, DSL_OPCODE_INFO *info)
{
    if (ordinal >= DSL_opcode_registry.size())
        return FALSE;

    return DSL_Opcode_Get_Info(DSL_opcode_registry[ordinal].id, info);
}

/*
 * For a nonnull NUL-terminated domain prefix, register matching dotted static
 * names at or above the version floor into the supplied domain. Count successes
 * including duplicates; no cross-entry rollback or domain-name/ID proof occurs.
 */
static UINT32
DSL_Opcode_Register_Logical_Domain
        (const char *domain_name,
         DSL_DOMAIN_ID domain_id,
         UINT16 minimum_version)
{
    size_t domain_length = strlen(domain_name);
    UINT32 registered = 0;

    for (UINT32 i = 0; i < DSL_ARRAY_COUNT(DSL_logical_operator_seed); ++i) {
        const DSL_LOGICAL_OPERATOR_SEED &seed = DSL_logical_operator_seed[i];
        if (seed.version < minimum_version ||
            strncmp(seed.name, domain_name, domain_length) != 0 ||
            seed.name[domain_length] != '.')
            continue;

        DSL_OPCODE_ID id = DSL_Opcode_Register
            (domain_id, seed.name, seed.version, seed.category, seed.level,
             seed.nkids, seed.shape_rule, seed.effect_model,
             seed.lowering_model, seed.diagnostic_prefix, 0);
        if (id != DSL_OPCODE_INVALID_ID)
            ++registered;
    }
    return registered;
}

/*
 * Ensure common, preserve legacy v1 seed order, then append logical versions
 * >= 2. Count successful registration calls, including repeated v1 seed keys;
 * keep prior additions on later failure rather than performing batch rollback.
 */
UINT32
DSL_Opcode_Register_Common_Substrate (void)
{
    DSL_DOMAIN_ID common_id = DSL_Domain_Find("common");
    UINT32 registered = 0;

    if (common_id == DSL_DOMAIN_INVALID_ID)
        common_id = DSL_Domain_Register("common", DSL_DOMAIN_INVALID_ID, 1, 0);

    if (common_id == DSL_DOMAIN_INVALID_ID)
        return 0;

    for (UINT32 i = 0; i < DSL_ARRAY_COUNT(DSL_common_opcode_seed); ++i) {
        const DSL_COMMON_OPCODE_SEED &seed = DSL_common_opcode_seed[i];
        DSL_OPCODE_ID id =
            DSL_Opcode_Register(common_id,
                                seed.name,
                                1,
                                seed.category,
                                seed.level,
                                seed.nkids,
                                seed.shape_rule,
                                seed.effect_model,
                                seed.lowering_model,
                                seed.diagnostic_prefix,
                                0);

        if (id != DSL_OPCODE_INVALID_ID)
            ++registered;
    }

    registered += DSL_Opcode_Register_Logical_Domain("common", common_id, 2);

    return registered;
}

/*
 * Seed prerequisites, then example child domains/wrappers and CNN versions >= 2.
 * Skip failed items and count successful wrapper/CNN calls including duplicates;
 * common prerequisite successes are not counted and no additions are rolled back.
 */
UINT32
DSL_Opcode_Register_Domain_Wrapper_Examples (void)
{
    DSL_DOMAIN_ID common_id;
    UINT32 registered = 0;

    DSL_Opcode_Register_Common_Substrate();
    common_id = DSL_Domain_Find("common");

    if (common_id == DSL_DOMAIN_INVALID_ID)
        return 0;

    for (UINT32 i = 0; i < DSL_ARRAY_COUNT(DSL_domain_wrapper_seed); ++i) {
        const DSL_DOMAIN_WRAPPER_SEED &seed = DSL_domain_wrapper_seed[i];
        DSL_DOMAIN_ID domain_id = DSL_Domain_Find(seed.domain_name);
        DSL_OPCODE_ID target_id;
        DSL_OPCODE_ID id;

        if (domain_id == DSL_DOMAIN_INVALID_ID)
            domain_id = DSL_Domain_Register(seed.domain_name, common_id, 1, 0);

        if (domain_id == DSL_DOMAIN_INVALID_ID)
            continue;

        target_id = DSL_Opcode_Find(common_id, seed.target_name, 1);
        id = DSL_Opcode_Register_Domain_Wrapper(domain_id,
                                            seed.name,
                                            1,
                                            target_id,
                                            seed.diagnostic_prefix,
                                            0);

        if (id != DSL_OPCODE_INVALID_ID)
            ++registered;
    }

    DSL_DOMAIN_ID cnn_id = DSL_Domain_Find("cnn");
    if (cnn_id != DSL_DOMAIN_INVALID_ID)
        registered += DSL_Opcode_Register_Logical_Domain("cnn", cnn_id, 2);

    return registered;
}

/*
 * Seed common and create transformer only if absent, then count matching logical
 * seed successes from v1 upward. Prerequisite mutations are not rolled back.
 */
UINT32
DSL_Opcode_Register_Transformer_Domain (void)
{
    DSL_Opcode_Register_Common_Substrate();
    DSL_DOMAIN_ID common_id = DSL_Domain_Find("common");
    DSL_DOMAIN_ID transformer_id = DSL_Domain_Find("transformer");

    if (common_id == DSL_DOMAIN_INVALID_ID)
        return 0;
    if (transformer_id == DSL_DOMAIN_INVALID_ID)
        transformer_id = DSL_Domain_Register
                             ("transformer", common_id, 1, 0);
    if (transformer_id == DSL_DOMAIN_INVALID_ID)
        return 0;
    return DSL_Opcode_Register_Logical_Domain
               ("transformer", transformer_id, 1);
}

/*
 * Seed common and create ckks only if absent; register its logical metadata from
 * v1 upward. Count only CKKS successes; do not validate ciphertext semantics.
 */
UINT32
DSL_Opcode_Register_CKKS_Domain (void)
{
    DSL_Opcode_Register_Common_Substrate();
    DSL_DOMAIN_ID common_id = DSL_Domain_Find("common");
    DSL_DOMAIN_ID ckks_id = DSL_Domain_Find("ckks");

    if (common_id == DSL_DOMAIN_INVALID_ID)
        return 0;
    if (ckks_id == DSL_DOMAIN_INVALID_ID)
        ckks_id = DSL_Domain_Register("ckks", common_id, 1, 0);
    if (ckks_id == DSL_DOMAIN_INVALID_ID)
        return 0;
    return DSL_Opcode_Register_Logical_Domain("ckks", ckks_id, 1);
}

/* Validate the source ID and read its direct link; do not follow wrapper chains. */
DSL_OPCODE_ID
DSL_Opcode_Wrapper_Target (DSL_OPCODE_ID id)
{
    if (!DSL_Opcode_Valid_Id(id))
        return DSL_OPCODE_INVALID_ID;

    return DSL_opcode_registry[id - 1].wrapper_target_id;
}

/*
 * Validate both opcode IDs, state and nonzero version before honoring an existing
 * pair. Otherwise copy counted evidence strings and append a one-based record;
 * NULL strings/arrays become empty entries. Payload/state/version do not enter
 * identity. Validation failure is read-only; allocation exceptions have no
 * cleanup/rollback guarantee and no IR transformation is performed.
 */
DSL_OPCODE_PROMOTION_ID
DSL_Opcode_Promotion_Register
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
                 UINT32 flags)
{
    DSL_OPCODE_PROMOTION_ID existing =
        DSL_Opcode_Promotion_Find(source_opcode_id, promoted_opcode_id);

    if (!DSL_Opcode_Valid_Id(source_opcode_id) ||
            !DSL_Opcode_Valid_Id(promoted_opcode_id) ||
            !DSL_Opcode_Valid_Promotion_State(state) ||
            version == 0)
        return DSL_OPCODE_PROMOTION_INVALID_ID;

    if (existing != DSL_OPCODE_PROMOTION_INVALID_ID)
        return existing;

    DSL_OPCODE_PROMOTION_RECORD record;
    record.id = DSL_opcode_promotion_registry.size() + 1;
    record.source_opcode_id = source_opcode_id;
    record.promoted_opcode_id = promoted_opcode_id;
    record.state = state;
    record.version = version;
    record.flags = flags;

    for (UINT32 i = 0; i < required_common_semantics_count; ++i)
        record.required_common_semantics.push_back
            (DSL_Opcode_Save_String(required_common_semantics == NULL ? NULL :
                              required_common_semantics[i]));

    for (UINT32 i = 0; i < retained_wrapper_count; ++i)
        record.retained_wrappers.push_back
            (DSL_Opcode_Save_String(retained_wrappers == NULL ? NULL :
                              retained_wrappers[i]));

    for (UINT32 i = 0; i < required_verifier_check_count; ++i)
        record.required_verifier_checks.push_back
            (DSL_Opcode_Save_String(required_verifier_checks == NULL ? NULL :
                              required_verifier_checks[i]));

    for (UINT32 i = 0; i < diagnostic_count; ++i)
        record.diagnostics.push_back
            (DSL_Opcode_Save_String(diagnostics == NULL ? NULL :
                              diagnostics[i]));

    /* Vector copies share payload pointers; registry retirement frees them. */
    DSL_opcode_promotion_registry.push_back(record);
    return record.id;
}

/* Scan the stored numeric source/target pair without live-opcode validation. */
DSL_OPCODE_PROMOTION_ID
DSL_Opcode_Promotion_Find (DSL_OPCODE_ID source_opcode_id,
                           DSL_OPCODE_ID promoted_opcode_id)
{
    for (UINT32 i = 0; i < DSL_opcode_promotion_registry.size(); ++i) {
        const DSL_OPCODE_PROMOTION_RECORD &record =
            DSL_opcode_promotion_registry[i];
        if (record.source_opcode_id == source_opcode_id &&
        record.promoted_opcode_id == promoted_opcode_id)
            return record.id;
    }

    return DSL_OPCODE_PROMOTION_INVALID_ID;
}

/*
 * Validate the promotion ID before optional scalar/count copies; misses do not
 * write and live opcode references are not rechecked.
 */
BOOL
DSL_Opcode_Promotion_Get_Info (DSL_OPCODE_PROMOTION_ID id,
                               DSL_OPCODE_PROMOTION_INFO *info)
{
    if (!DSL_Opcode_Promotion_Valid_Id(id))
        return FALSE;

    if (info != NULL) {
        const DSL_OPCODE_PROMOTION_RECORD &record =
            DSL_opcode_promotion_registry[id - 1];
        info->id = record.id;
        info->source_opcode_id = record.source_opcode_id;
        info->promoted_opcode_id = record.promoted_opcode_id;
        info->state = record.state;
        info->version = record.version;
        info->required_common_semantics_count =
            record.required_common_semantics.size();
        info->retained_wrapper_count = record.retained_wrappers.size();
        info->required_verifier_check_count =
            record.required_verifier_checks.size();
        info->diagnostic_count = record.diagnostics.size();
        info->flags = record.flags;
    }

    return TRUE;
}

/* Read promotion vector size through UINT32 without allocating or changing it. */
UINT32
DSL_Opcode_Promotion_Count (void)
{
    return DSL_opcode_promotion_registry.size();
}

/* Bounds-check a zero-based ordinal before delegating optional output copying. */
BOOL
DSL_Opcode_Promotion_At (UINT32 ordinal, DSL_OPCODE_PROMOTION_INFO *info)
{
    if (ordinal >= DSL_opcode_promotion_registry.size())
        return FALSE;

    return DSL_Opcode_Promotion_Get_Info
        (DSL_opcode_promotion_registry[ordinal].id, info);
}

/* Check ID and semantic ordinal before borrowing an owned string, or return NULL. */
const char *
DSL_Opcode_Promotion_Required_Common_Semantic_At
                (DSL_OPCODE_PROMOTION_ID id, UINT32 ordinal)
{
    if (!DSL_Opcode_Promotion_Valid_Id(id))
        return NULL;

    const DSL_OPCODE_PROMOTION_RECORD &record =
        DSL_opcode_promotion_registry[id - 1];
    if (ordinal >= record.required_common_semantics.size())
        return NULL;

    return record.required_common_semantics[ordinal];
}

/* Check ID and wrapper ordinal before borrowing a name; no link is followed. */
const char *
DSL_Opcode_Promotion_Retained_Wrapper_At
                (DSL_OPCODE_PROMOTION_ID id, UINT32 ordinal)
{
    if (!DSL_Opcode_Promotion_Valid_Id(id))
        return NULL;

    const DSL_OPCODE_PROMOTION_RECORD &record =
        DSL_opcode_promotion_registry[id - 1];
    if (ordinal >= record.retained_wrappers.size())
        return NULL;

    return record.retained_wrappers[ordinal];
}

/* Borrow a check name after both bounds tests; this does not execute verification. */
const char *
DSL_Opcode_Promotion_Required_Verifier_Check_At
                (DSL_OPCODE_PROMOTION_ID id, UINT32 ordinal)
{
    if (!DSL_Opcode_Promotion_Valid_Id(id))
        return NULL;

    const DSL_OPCODE_PROMOTION_RECORD &record =
        DSL_opcode_promotion_registry[id - 1];
    if (ordinal >= record.required_verifier_checks.size())
        return NULL;

    return record.required_verifier_checks[ordinal];
}

/* Borrow a stored diagnostic after bounds checks; do not emit or interpret it. */
const char *
DSL_Opcode_Promotion_Diagnostic_At
                (DSL_OPCODE_PROMOTION_ID id, UINT32 ordinal)
{
    if (!DSL_Opcode_Promotion_Valid_Id(id))
        return NULL;

    const DSL_OPCODE_PROMOTION_RECORD &record =
        DSL_opcode_promotion_registry[id - 1];
    if (ordinal >= record.diagnostics.size())
        return NULL;

    return record.diagnostics[ordinal];
}

/*
 * Seed wrapper prerequisites, synthesize missing source metadata where a target
 * exists and publish each example pair. Count successful promotions including
 * duplicates, leaving earlier/prerequisite additions intact on later failures.
 */
UINT32
DSL_Opcode_Promotion_Register_Examples (void)
{
    DSL_DOMAIN_ID common_id;
    UINT32 registered = 0;

    DSL_Opcode_Register_Domain_Wrapper_Examples();
    common_id = DSL_Domain_Find("common");
    if (common_id == DSL_DOMAIN_INVALID_ID)
        return 0;

    for (UINT32 i = 0; i < DSL_ARRAY_COUNT(DSL_opcode_promotion_seed); ++i) {
        const DSL_OPCODE_PROMOTION_SEED &seed = DSL_opcode_promotion_seed[i];
        DSL_DOMAIN_ID domain_id = DSL_Domain_Find(seed.source_domain);
        DSL_OPCODE_ID source_id;
        DSL_OPCODE_ID promoted_id;
        DSL_OPCODE_PROMOTION_ID id;
        const char *semantics[] = { seed.common_semantic };
        const char *wrappers[] = { seed.retained_wrapper };
        const char *checks[] = { seed.verifier_check };
        const char *diagnostics[] = { seed.diagnostic };

        if (domain_id == DSL_DOMAIN_INVALID_ID)
            domain_id = DSL_Domain_Register(seed.source_domain, common_id, 1, 0);

        if (domain_id == DSL_DOMAIN_INVALID_ID)
            continue;

        source_id = DSL_Opcode_Find(domain_id, seed.source_name, 1);
        promoted_id = DSL_Opcode_Find(common_id, seed.promoted_name, 1);

        if (source_id == DSL_OPCODE_INVALID_ID &&
        promoted_id != DSL_OPCODE_INVALID_ID)
            source_id = DSL_Opcode_Register(domain_id,
                                      seed.source_name,
                                      1,
                                      DSL_OPCODE_CATEGORY_EXECUTABLE,
                                      DSL_OPCODE_LEVEL_3_NN_COMMON,
                                      DSL_OPCODE_NKIDS_PAYLOAD_DEFINED,
                                      DSL_SHAPE_RULE_RUNTIME_GUARDED,
                                      DSL_EFFECT_MODEL_PURE,
                                      DSL_LOWERING_MODEL_MARKER_ONLY,
                                      seed.diagnostic,
                                      0);

        id = DSL_Opcode_Promotion_Register
            (source_id,
             promoted_id,
             seed.state,
             1,
             semantics,
             1,
             wrappers,
             1,
             checks,
             1,
             diagnostics,
             1,
             0);

        if (id != DSL_OPCODE_PROMOTION_INVALID_ID)
            ++registered;
    }

    return registered;
}

/*
 * Check live IDs/pair presence, target version, then optional source-link
 * presence in diagnostic priority order. NULL is not a complete legality proof:
 * payloads/state and equality of wrapper target to promoted ID are not checked.
 */
const char *
DSL_Opcode_Check_Promotion (DSL_OPCODE_ID source_opcode_id,
                            DSL_OPCODE_ID promoted_opcode_id,
                            UINT16 common_version,
                            BOOL require_wrapper)
{
    DSL_OPCODE_INFO promoted_info;

    if (!DSL_Opcode_Valid_Id(source_opcode_id) ||
            !DSL_Opcode_Valid_Id(promoted_opcode_id) ||
            DSL_Opcode_Promotion_Find(source_opcode_id, promoted_opcode_id) ==
        DSL_OPCODE_PROMOTION_INVALID_ID)
        return DSL_CPROM_Diagnostic_Code
            (DSL_CPROM_PROMOTION_CANDIDATE_MISSING_CROSS_DOMAIN_EVIDENCE);

    if (!DSL_Opcode_Get_Info(promoted_opcode_id, &promoted_info) ||
            promoted_info.version != common_version)
        return DSL_CPROM_Diagnostic_Code
            (DSL_CPROM_COMMON_OP_VERSION_MISMATCH);

    if (require_wrapper &&
            DSL_Opcode_Wrapper_Target(source_opcode_id) == DSL_OPCODE_INVALID_ID)
        return DSL_CPROM_Diagnostic_Code
            (DSL_CPROM_DOMAIN_WRAPPER_MISSING_AFTER_PROMOTION);

    return NULL;
}

/* Borrow a category spelling using an unsigned bounds check and static fallback. */
const char *
DSL_Opcode_Category_Name (DSL_OPCODE_CATEGORY category)
{
    UINT32 index = (UINT32) category;

    return index < DSL_ARRAY_COUNT(DSL_opcode_category_name) ?
         DSL_opcode_category_name[index] : "unknown";
}

/* Borrow a level spelling; out-of-range enum values map to static unknown. */
const char *
DSL_Opcode_Level_Name (DSL_OPCODE_LEVEL level)
{
    UINT32 index = (UINT32) level;

    return index < DSL_ARRAY_COUNT(DSL_opcode_level_name) ?
         DSL_opcode_level_name[index] : "unknown";
}

/* Borrow a shape spelling without inspecting tensor descriptors or shape facts. */
const char *
DSL_Shape_Rule_Name (DSL_SHAPE_RULE shape_rule)
{
    UINT32 index = (UINT32) shape_rule;

    return index < DSL_ARRAY_COUNT(DSL_shape_rule_name) ?
         DSL_shape_rule_name[index] : "unknown";
}

/* Borrow an effect spelling or static unknown, without effect analysis. */
const char *
DSL_Effect_Model_Name (DSL_EFFECT_MODEL effect_model)
{
    UINT32 index = (UINT32) effect_model;

    return index < DSL_ARRAY_COUNT(DSL_effect_model_name) ?
         DSL_effect_model_name[index] : "unknown";
}

/* Borrow a model spelling or static unknown, not a lowering implementation. */
const char *
DSL_Lowering_Model_Name (DSL_LOWERING_MODEL lowering_model)
{
    UINT32 index = (UINT32) lowering_model;

    return index < DSL_ARRAY_COUNT(DSL_lowering_model_name) ?
         DSL_lowering_model_name[index] : "unknown";
}

/* Borrow a promotion-state label without validating a promotion record. */
const char *
DSL_Opcode_Promotion_State_Name (DSL_OPCODE_PROMOTION_STATE state)
{
    UINT32 index = (UINT32) state;

    return index < DSL_ARRAY_COUNT(DSL_opcode_promotion_state_name) ?
         DSL_opcode_promotion_state_name[index] : "unknown";
}

/* Bounds-check and borrow a CPROM code or static unknown; never emit a message. */
const char *
DSL_CPROM_Diagnostic_Code (DSL_CPROM_DIAGNOSTIC diagnostic)
{
    UINT32 index = (UINT32) diagnostic;

    return index < DSL_ARRAY_COUNT(DSL_cprom_diagnostic_code) ?
         DSL_cprom_diagnostic_code[index] : "unknown";
}

/*
 * Print dynamic opcode records in registration order, skipping NULL streams.
 * No static registry seeding, mutation or stream-error propagation occurs.
 */
void
DSL_Opcode_fprint_registry (FILE *f)
{
    if (f == NULL)
        return;

    fprintf(f, "DSL Opcode Registry: entries=%u\n", DSL_Opcode_Count());
    for (UINT32 i = 0; i < DSL_opcode_registry.size(); ++i) {
        const DSL_OPCODE_RECORD &record = DSL_opcode_registry[i];
        fprintf(f,
            "  [%u] id=%u name=%s owner=%u version=%u category=%s level=%s "
            "wrapper_target=%u nkids=%d shape=%s effect=%s lowering=%s "
            "diagnostic_prefix=%s flags=0x%x\n",
            i,
            record.id,
            record.name,
            record.owner_domain_id,
            record.version,
            DSL_Opcode_Category_Name(record.category),
            DSL_Opcode_Level_Name(record.level),
            record.wrapper_target_id,
            record.nkids,
            DSL_Shape_Rule_Name(record.shape_rule),
            DSL_Effect_Model_Name(record.effect_model),
            DSL_Lowering_Model_Name(record.lowering_model),
            record.diagnostic_prefix,
            record.flags);
    }
}

/*
 * Print promotion records and owned evidence strings without executing checks.
 * Skip NULL streams; leave stream ownership and error handling to the caller.
 */
void
DSL_Opcode_Promotion_fprint_registry (FILE *f)
{
    if (f == NULL)
        return;

    fprintf(f, "DSL Opcode Promotion Registry: entries=%u\n",
          DSL_Opcode_Promotion_Count());
    for (UINT32 i = 0; i < DSL_opcode_promotion_registry.size(); ++i) {
        const DSL_OPCODE_PROMOTION_RECORD &record =
            DSL_opcode_promotion_registry[i];
        fprintf(f,
            "  [%u] id=%u source=%u promoted=%u state=%s version=%u "
            "flags=0x%x\n",
            i,
            record.id,
            record.source_opcode_id,
            record.promoted_opcode_id,
            DSL_Opcode_Promotion_State_Name(record.state),
            record.version,
            record.flags);

        for (UINT32 j = 0; j < record.required_common_semantics.size(); ++j)
            fprintf(f, "      common_semantic[%u]=%s\n", j,
              record.required_common_semantics[j]);

        for (UINT32 j = 0; j < record.retained_wrappers.size(); ++j)
            fprintf(f, "      retained_wrapper[%u]=%s\n", j,
              record.retained_wrappers[j]);

        for (UINT32 j = 0; j < record.required_verifier_checks.size(); ++j)
            fprintf(f, "      verifier_check[%u]=%s\n", j,
              record.required_verifier_checks[j]);

        for (UINT32 j = 0; j < record.diagnostics.size(); ++j)
            fprintf(f, "      diagnostic[%u]=%s\n", j, record.diagnostics[j]);
    }
}
