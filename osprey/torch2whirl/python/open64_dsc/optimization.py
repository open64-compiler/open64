"""Frontend-visible optimization traits for logical DSL operators.

The traits in this module are classification metadata only.  They make the
optimization opportunities around imported operator definitions reviewable by
Python tests while preserving the rule that actual WHIRL optimization and
lowering are owned by native common/com and VHO code.
"""

from __future__ import annotations

from dataclasses import dataclass
from types import MappingProxyType
from typing import Mapping, Sequence

from . import operators
from .mapping import common, transformer
from .mapping.contract import OperatorContract


FRONTEND_POLICY_CLASSIFICATION_ONLY = "classification_only"
NATIVE_OWNER_VHO_DSL = "native_vho_dsl"


@dataclass(frozen=True)
class OperatorOptimizationTraits:
    name: str
    version: int
    effect_model: str
    semantic_role: str
    shape_role: str
    candidate_passes: Sequence[str]
    native_owner: str = NATIVE_OWNER_VHO_DSL
    frontend_policy: str = FRONTEND_POLICY_CLASSIFICATION_ONLY


OptimizationTraits = Mapping[str, OperatorOptimizationTraits]


def _operator_traits(
    contract: OperatorContract,
    *,
    effect_model: str,
    semantic_role: str,
    shape_role: str,
    candidate_passes: Sequence[str],
) -> OperatorOptimizationTraits:
    return OperatorOptimizationTraits(
        contract.name,
        contract.version,
        effect_model,
        semantic_role,
        shape_role,
        tuple(candidate_passes),
    )


LLAMA2_MULTIPLE_PU_OPTIMIZATION_TRAITS: OptimizationTraits = MappingProxyType({
    common.LINEAR: _operator_traits(
        operators.LLAMA2_MULTIPLE_PU_OPERATORS[common.LINEAR],
        effect_model="pure",
        semantic_role="projection",
        shape_role="rank_preserving_or_last_dim_projecting",
        candidate_passes=(
            "weight_layout_canonicalization",
            "projection_chain_fusion",
        ),
    ),
    common.RESHAPE: _operator_traits(
        operators.LLAMA2_MULTIPLE_PU_OPERATORS[common.RESHAPE],
        effect_model="pure",
        semantic_role="layout_view",
        shape_role="metadata_only_shape_change",
        candidate_passes=(
            "reshape_transpose_folding",
            "layout_propagation",
        ),
    ),
    common.TRANSPOSE: _operator_traits(
        operators.LLAMA2_MULTIPLE_PU_OPERATORS[common.TRANSPOSE],
        effect_model="pure",
        semantic_role="layout_permutation",
        shape_role="metadata_only_axis_permutation",
        candidate_passes=(
            "transpose_cancel",
            "layout_propagation",
        ),
    ),
    common.RESIDUAL_ADD: _operator_traits(
        operators.LLAMA2_MULTIPLE_PU_OPERATORS[common.RESIDUAL_ADD],
        effect_model="pure",
        semantic_role="residual_merge",
        shape_role="shape_preserving_binary",
        candidate_passes=(
            "residual_add_scheduling",
            "broadcast_check_elision_after_gatekeeper",
        ),
    ),
    common.OUTPUT_LOGITS: _operator_traits(
        operators.LLAMA2_MULTIPLE_PU_OPERATORS[common.OUTPUT_LOGITS],
        effect_model="pure",
        semantic_role="logits_boundary",
        shape_role="vocabulary_axis_marker",
        candidate_passes=(
            "logits_boundary_recognition",
        ),
    ),
    transformer.RMS_NORM: _operator_traits(
        operators.LLAMA2_MULTIPLE_PU_OPERATORS[transformer.RMS_NORM],
        effect_model="pure",
        semantic_role="normalization",
        shape_role="shape_preserving_unary_with_scale",
        candidate_passes=(
            "norm_scale_fusion",
            "accumulation_policy_lowering",
        ),
    ),
    transformer.ROTARY_EMBEDDING: _operator_traits(
        operators.LLAMA2_MULTIPLE_PU_OPERATORS[
            transformer.ROTARY_EMBEDDING
        ],
        effect_model="pure",
        semantic_role="position_encoding",
        shape_role="shape_preserving_qk_transform",
        candidate_passes=(
            "rope_table_hoist",
            "rotary_pair_fusion",
        ),
    ),
    transformer.ATTENTION: _operator_traits(
        operators.LLAMA2_MULTIPLE_PU_OPERATORS[transformer.ATTENTION],
        effect_model="pure",
        semantic_role="attention",
        shape_role="bhsd_attention_reduction",
        candidate_passes=(
            "attention_projection_fusion",
            "mask_scale_softmax_fusion",
            "fused_attention_lowering",
        ),
    ),
    transformer.SWIGLU: _operator_traits(
        operators.LLAMA2_MULTIPLE_PU_OPERATORS[transformer.SWIGLU],
        effect_model="pure",
        semantic_role="activation_gating",
        shape_role="shape_preserving_binary_gate",
        candidate_passes=(
            "gate_up_projection_fusion",
            "activation_multiply_fusion",
        ),
    ),
})


def optimization_candidate_names(
    traits: OptimizationTraits,
) -> Sequence[str]:
    names = []
    for trait in traits.values():
        for candidate in trait.candidate_passes:
            if candidate not in names:
                names.append(candidate)
    return tuple(names)


__all__ = [
    "FRONTEND_POLICY_CLASSIFICATION_ONLY",
    "LLAMA2_MULTIPLE_PU_OPTIMIZATION_TRAITS",
    "NATIVE_OWNER_VHO_DSL",
    "OperatorOptimizationTraits",
    "OptimizationTraits",
    "optimization_candidate_names",
]
