"""Gatekeeper verifier for high-level torch2whirl DSL modules."""

from __future__ import annotations

from typing import Dict, Mapping, Optional, Sequence, Tuple

from .mapping import cnn, common
from .module import WhirlModule, WhirlOperatorRecord, WhirlTensorTypeRecord


class WhirlVerificationError(ValueError):
    """Raised when a WHIRL DSL module violates frontend contracts."""


_SUPPORTED_DTYPES = {
    "bool",
    "float32",
    "float64",
    "int32",
    "int64",
}

_KNOWN_OPERATORS = {
    common.ADD,
    common.FLATTEN,
    common.LINEAR,
    common.MATMUL,
    common.OUTPUT_LOGITS,
    common.RELU,
    common.RESIDUAL_ADD,
    cnn.BATCH_NORM_INFER,
    cnn.CONV2D,
    cnn.GLOBAL_AVG_POOL2D,
    cnn.MAX_POOL2D,
}

_OPERATOR_ARITY = {
    common.ADD: 2,
    common.FLATTEN: 1,
    common.LINEAR: 3,
    common.MATMUL: 2,
    common.OUTPUT_LOGITS: 1,
    common.RELU: 1,
    common.RESIDUAL_ADD: 2,
    cnn.BATCH_NORM_INFER: 5,
    cnn.CONV2D: 3,
    cnn.GLOBAL_AVG_POOL2D: 1,
    cnn.MAX_POOL2D: 1,
}


def verify_module(module: WhirlModule) -> None:
    """Verify a high-level module before WHIRL artifact finalization."""

    tensor_types = _tensor_types_by_name(module.tensor_types)
    value_types = _value_types_by_name(module, tensor_types)
    _verify_tensor_descriptors(module.tensor_types)
    _verify_graph_operators(module.graph_operators, value_types)


def _tensor_types_by_name(
    tensor_types: Sequence[WhirlTensorTypeRecord],
) -> Dict[str, WhirlTensorTypeRecord]:
    result: Dict[str, WhirlTensorTypeRecord] = {}
    for tensor_type in tensor_types:
        if not tensor_type.name:
            raise WhirlVerificationError("tensor type name must not be empty")
        if tensor_type.name in result:
            raise WhirlVerificationError(
                f"duplicate tensor type: {tensor_type.name}"
            )
        result[tensor_type.name] = tensor_type
    return result


def _value_types_by_name(
    module: WhirlModule,
    tensor_types: Mapping[str, WhirlTensorTypeRecord],
) -> Dict[str, Optional[WhirlTensorTypeRecord]]:
    result: Dict[str, Optional[WhirlTensorTypeRecord]] = {}
    for value in module.values:
        if not value.name:
            raise WhirlVerificationError("value name must not be empty")
        if value.name in result:
            raise WhirlVerificationError(f"duplicate value: {value.name}")
        if value.value_kind == "absent_parameter" and not value.type_name:
            result[value.name] = None
            continue
        tensor_type = tensor_types.get(value.type_name)
        if tensor_type is None:
            raise WhirlVerificationError(
                f"value {value.name} references missing tensor type: "
                f"{value.type_name}"
            )
        result[value.name] = tensor_type
    return result


def _verify_tensor_descriptors(
    tensor_types: Sequence[WhirlTensorTypeRecord],
) -> None:
    for tensor_type in tensor_types:
        if tensor_type.dtype not in _SUPPORTED_DTYPES:
            raise WhirlVerificationError(
                f"tensor type {tensor_type.name} has unsupported dtype: "
                f"{tensor_type.dtype}"
            )
        if tensor_type.rank < 0:
            raise WhirlVerificationError(
                f"tensor type {tensor_type.name} has negative rank"
            )

        logical_shape = _parse_logical_shape(
            tensor_type.logical_shape,
            f"tensor type {tensor_type.name}",
        )
        if len(logical_shape) != tensor_type.rank:
            raise WhirlVerificationError(
                f"tensor type {tensor_type.name} rank {tensor_type.rank} "
                f"does not match logical shape {tensor_type.logical_shape}"
            )

        descriptor = tensor_type.descriptor
        for field in ("dtype", "rank", "logical_shape"):
            if field not in descriptor:
                raise WhirlVerificationError(
                    f"tensor type {tensor_type.name} missing descriptor "
                    f"field: {field}"
                )
        if descriptor.get("dtype") != tensor_type.dtype:
            raise WhirlVerificationError(
                f"tensor type {tensor_type.name} descriptor dtype mismatch"
            )
        if int(descriptor.get("rank", -1)) != tensor_type.rank:
            raise WhirlVerificationError(
                f"tensor type {tensor_type.name} descriptor rank mismatch"
            )
        if descriptor.get("logical_shape") != tensor_type.logical_shape:
            raise WhirlVerificationError(
                f"tensor type {tensor_type.name} descriptor shape mismatch"
            )


def _verify_graph_operators(
    graph_operators: Sequence[WhirlOperatorRecord],
    value_types: Mapping[str, Optional[WhirlTensorTypeRecord]],
) -> None:
    produced_types: Dict[str, Optional[WhirlTensorTypeRecord]] = {}
    for operator in graph_operators:
        if operator.name not in _KNOWN_OPERATORS:
            raise WhirlVerificationError(f"unknown operator: {operator.name}")
        expected_arity = _OPERATOR_ARITY[operator.name]
        if len(operator.kids) != expected_arity:
            raise WhirlVerificationError(
                f"{operator.name} expects {expected_arity} operands, "
                f"got {len(operator.kids)}"
            )

        operand_types = [
            _resolve_operand_type(kid, value_types, produced_types)
            for kid in operator.kids
        ]
        _verify_operator_contract(operator, operand_types)
        produced_types[operator.name] = _result_type(operator, operand_types)


def _resolve_operand_type(
    kid: str,
    value_types: Mapping[str, Optional[WhirlTensorTypeRecord]],
    produced_types: Mapping[str, Optional[WhirlTensorTypeRecord]],
) -> Optional[WhirlTensorTypeRecord]:
    if kid in value_types:
        return value_types[kid]
    if kid in produced_types:
        return produced_types[kid]
    raise WhirlVerificationError(f"operator operand does not resolve: {kid}")


def _verify_operator_contract(
    operator: WhirlOperatorRecord,
    operand_types: Sequence[Optional[WhirlTensorTypeRecord]],
) -> None:
    if operator.name == common.ADD:
        _require_attr(operator, "attr.broadcast_rule")
        if operator.attrs.get("attr.broadcast_rule") != "none":
            return
        _require_same_tensor_type(operator, operand_types)
        return

    if operator.name == common.RESIDUAL_ADD:
        _require_attr(operator, "attr.broadcast_rule")
        _require_attr(operator, "attr.shape_check")
        if operator.attrs.get("attr.broadcast_rule") != "none":
            raise WhirlVerificationError(
                "common.residual_add does not allow broadcast operands"
            )
        if operator.attrs.get("attr.shape_check") != "exact":
            raise WhirlVerificationError(
                "common.residual_add requires attr.shape_check=exact"
            )
        _require_same_tensor_type(operator, operand_types)


def _require_attr(operator: WhirlOperatorRecord, name: str) -> None:
    if name not in operator.attrs:
        raise WhirlVerificationError(
            f"{operator.name} missing required attribute: {name}"
        )


def _require_same_tensor_type(
    operator: WhirlOperatorRecord,
    operand_types: Sequence[Optional[WhirlTensorTypeRecord]],
) -> None:
    known_types = [operand_type for operand_type in operand_types
                   if operand_type is not None]
    if len(known_types) != len(operand_types):
        return
    first = known_types[0]
    for operand_type in known_types[1:]:
        if operand_type.dtype != first.dtype:
            raise WhirlVerificationError(
                f"{operator.name} operands have incompatible dtype: "
                f"{first.dtype} vs {operand_type.dtype}"
            )
        if operand_type.rank != first.rank:
            raise WhirlVerificationError(
                f"{operator.name} operands have incompatible rank: "
                f"{first.rank} vs {operand_type.rank}"
            )
        if operand_type.logical_shape != first.logical_shape:
            raise WhirlVerificationError(
                f"{operator.name} operands have incompatible shape: "
                f"{first.logical_shape} vs {operand_type.logical_shape}"
            )


def _result_type(
    operator: WhirlOperatorRecord,
    operand_types: Sequence[Optional[WhirlTensorTypeRecord]],
) -> Optional[WhirlTensorTypeRecord]:
    if not operand_types:
        return None
    if operator.name in {
        common.ADD,
        common.FLATTEN,
        common.LINEAR,
        common.MATMUL,
        common.OUTPUT_LOGITS,
        common.RELU,
        common.RESIDUAL_ADD,
        cnn.BATCH_NORM_INFER,
        cnn.CONV2D,
        cnn.GLOBAL_AVG_POOL2D,
        cnn.MAX_POOL2D,
    }:
        return operand_types[0]
    return None


def _parse_logical_shape(
    logical_shape: str,
    subject: str,
) -> Tuple[int, ...]:
    if not logical_shape.startswith("[") or not logical_shape.endswith("]"):
        raise WhirlVerificationError(
            f"{subject} has malformed logical shape: {logical_shape}"
        )
    body = logical_shape[1:-1]
    if not body:
        return ()
    try:
        shape = tuple(int(part) for part in body.split(","))
    except ValueError as exc:
        raise WhirlVerificationError(
            f"{subject} has non-integer logical shape: {logical_shape}"
        ) from exc
    if any(dim <= 0 for dim in shape):
        raise WhirlVerificationError(
            f"{subject} has non-positive logical shape: {logical_shape}"
        )
    return shape
