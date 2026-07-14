"""Published ResNet operator contract consumed by torch2whirl.

The physical WHIRL representation is owned by common/com.  Python mirrors only
stable logical names, versions, arities, and attribute spellings.
"""

from __future__ import annotations

from dataclasses import dataclass
from typing import Mapping, Sequence

from . import cnn, common


@dataclass(frozen=True)
class OperatorContract:
    name: str
    version: int
    arity: int
    required_attrs: Sequence[str] = ()


_CONTRACTS = {
    common.MODEL_INPUT: OperatorContract(
        common.MODEL_INPUT,
        2,
        0,
        ("attr.input_ordinal",),
    ),
    common.TENSOR_CONST: OperatorContract(
        common.TENSOR_CONST,
        1,
        0,
        ("value_kind", "value"),
    ),
    common.ADD: OperatorContract(
        common.ADD,
        1,
        2,
        ("attr.broadcast_rule",),
    ),
    common.MATMUL: OperatorContract(
        common.MATMUL,
        1,
        2,
        ("attr.transpose_kid0", "attr.transpose_kid1"),
    ),
    common.RELU: OperatorContract(common.RELU, 2, 1),
    common.FLATTEN: OperatorContract(
        common.FLATTEN,
        2,
        1,
        ("attr.start_dim", "attr.end_dim"),
    ),
    common.RESIDUAL_ADD: OperatorContract(
        common.RESIDUAL_ADD,
        2,
        2,
        (
            "attr.broadcast_rule",
            "attr.shape_check",
            "attr.residual_path",
        ),
    ),
    common.LINEAR: OperatorContract(
        common.LINEAR,
        2,
        3,
        (
            "attr.has_bias",
            "attr.transpose_input",
            "attr.transpose_weight",
            "attr.weight_layout",
        ),
    ),
    common.OUTPUT_LOGITS: OperatorContract(
        common.OUTPUT_LOGITS,
        2,
        1,
        ("attr.semantic",),
    ),
    cnn.CONV2D: OperatorContract(
        cnn.CONV2D,
        2,
        3,
        (
            "attr.kernel_shape",
            "attr.stride",
            "attr.padding",
            "attr.dilation",
            "attr.groups",
            "attr.input_layout",
            "attr.weight_layout",
            "attr.output_layout",
        ),
    ),
    cnn.BATCH_NORM_INFER: OperatorContract(
        cnn.BATCH_NORM_INFER,
        2,
        5,
        (
            "attr.epsilon",
            "attr.training",
            "attr.input_layout",
            "attr.channel_axis",
        ),
    ),
    cnn.MAX_POOL2D: OperatorContract(
        cnn.MAX_POOL2D,
        2,
        1,
        (
            "attr.kernel_shape",
            "attr.stride",
            "attr.padding",
            "attr.dilation",
            "attr.ceil_mode",
        ),
    ),
    cnn.GLOBAL_AVG_POOL2D: OperatorContract(
        cnn.GLOBAL_AVG_POOL2D,
        2,
        1,
        ("attr.output_size", "attr.reduction_axes"),
    ),
}


def all_operator_contracts() -> Mapping[str, OperatorContract]:
    return dict(_CONTRACTS)


def operator_contract(name: str) -> OperatorContract:
    try:
        return _CONTRACTS[name]
    except KeyError as exc:
        raise NotImplementedError(f"unsupported mapped operator: {name}") from exc


def operator_version(name: str) -> int:
    return operator_contract(name).version


def operator_arity(name: str) -> int:
    return operator_contract(name).arity


def required_attrs(name: str) -> Sequence[str]:
    return operator_contract(name).required_attrs
