/*
 * Copyright (C) 2026 Open64 Project
 */

#include <string.h>
#include <vector>

#include "dsl_opcode.h"

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

static std::vector<DSL_OPCODE_RECORD> DSL_opcode_registry;

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

struct DSL_DOMAIN_WRAPPER_SEED {
  const char *domain_name;
  const char *name;
  const char *target_name;
  const char *diagnostic_prefix;
};

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
    DSL_LOWERING_MODEL_MARKER_ONLY, "DOPC_COMMON_ADD" },
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
    DSL_LOWERING_MODEL_MARKER_ONLY, "DOPC_COMMON_MATMUL" },
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
    DSL_LOWERING_MODEL_MARKER_ONLY, "DOPC_COMMON_KERNEL_VARIANT" }
};

static const DSL_DOMAIN_WRAPPER_SEED DSL_domain_wrapper_seed[] = {
  { "cnn", "cnn.linear", "common.linear", "DOPC_CNN_LINEAR_WRAPPER" },
  { "transformer", "transformer.q_projection", "common.linear",
    "DOPC_TRANSFORMER_Q_PROJECTION_WRAPPER" },
  { "cnn", "cnn.residual_add", "common.residual_add",
    "DOPC_CNN_RESIDUAL_ADD_WRAPPER" },
  { "transformer", "transformer.residual_add", "common.residual_add",
    "DOPC_TRANSFORMER_RESIDUAL_ADD_WRAPPER" }
};

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
  return id != DSL_OPCODE_INVALID_ID && id <= DSL_opcode_registry.size();
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
  for (UINT32 i = 0; i < DSL_opcode_registry.size(); ++i)
    DSL_Opcode_Free_Record(DSL_opcode_registry[i]);

  DSL_opcode_registry.clear();
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

  if (DSL_opcode_registry[id - 1].wrapper_target_id !=
	DSL_OPCODE_INVALID_ID &&
      DSL_opcode_registry[id - 1].wrapper_target_id != wrapper_target_id)
    return DSL_OPCODE_INVALID_ID;

  DSL_opcode_registry[id - 1].wrapper_target_id = wrapper_target_id;
  return id;
}

UINT32
DSL_Opcode_Count (void)
{
  return DSL_opcode_registry.size();
}

BOOL
DSL_Opcode_At (UINT32 ordinal, DSL_OPCODE_INFO *info)
{
  if (ordinal >= DSL_opcode_registry.size())
    return FALSE;

  return DSL_Opcode_Get_Info(DSL_opcode_registry[ordinal].id, info);
}

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

  return registered;
}

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

  return registered;
}

DSL_OPCODE_ID
DSL_Opcode_Wrapper_Target (DSL_OPCODE_ID id)
{
  if (!DSL_Opcode_Valid_Id(id))
    return DSL_OPCODE_INVALID_ID;

  return DSL_opcode_registry[id - 1].wrapper_target_id;
}

const char *
DSL_Opcode_Category_Name (DSL_OPCODE_CATEGORY category)
{
  UINT32 index = (UINT32) category;

  return index < DSL_ARRAY_COUNT(DSL_opcode_category_name) ?
	 DSL_opcode_category_name[index] : "unknown";
}

const char *
DSL_Opcode_Level_Name (DSL_OPCODE_LEVEL level)
{
  UINT32 index = (UINT32) level;

  return index < DSL_ARRAY_COUNT(DSL_opcode_level_name) ?
	 DSL_opcode_level_name[index] : "unknown";
}

const char *
DSL_Shape_Rule_Name (DSL_SHAPE_RULE shape_rule)
{
  UINT32 index = (UINT32) shape_rule;

  return index < DSL_ARRAY_COUNT(DSL_shape_rule_name) ?
	 DSL_shape_rule_name[index] : "unknown";
}

const char *
DSL_Effect_Model_Name (DSL_EFFECT_MODEL effect_model)
{
  UINT32 index = (UINT32) effect_model;

  return index < DSL_ARRAY_COUNT(DSL_effect_model_name) ?
	 DSL_effect_model_name[index] : "unknown";
}

const char *
DSL_Lowering_Model_Name (DSL_LOWERING_MODEL lowering_model)
{
  UINT32 index = (UINT32) lowering_model;

  return index < DSL_ARRAY_COUNT(DSL_lowering_model_name) ?
	 DSL_lowering_model_name[index] : "unknown";
}

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
