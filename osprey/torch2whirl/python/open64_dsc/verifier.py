"""Gatekeeper verifier for high-level torch2whirl DSL modules."""

from __future__ import annotations

import hashlib
from typing import Dict, Mapping, Optional, Sequence, Tuple

from .mapping import cnn, common
from .mapping.contract import (
    all_operator_contracts,
    operator_arity,
    required_attrs,
)
from .module import (
    WhirlModule,
    WhirlOperatorRecord,
    WhirlTensorPayloadRecord,
    WhirlTensorTypeRecord,
    WhirlValueRecord,
)


class WhirlVerificationError(ValueError):
    """Raised when a WHIRL DSL module violates frontend contracts."""


_SUPPORTED_DTYPES = {
    "bool",
    "float32",
    "float64",
    "int32",
    "int64",
}

_KNOWN_OPERATORS = set(all_operator_contracts())


def verify_module(module: WhirlModule) -> None:
    """Verify a high-level module before WHIRL artifact finalization."""

    tensor_types = _tensor_types_by_name(module.tensor_types)
    value_types = _value_types_by_name(module, tensor_types)
    _verify_tensor_descriptors(module.tensor_types)
    _verify_external_payload_records(module, tensor_types)
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


def _verify_external_payload_records(
    module: WhirlModule,
    tensor_types: Mapping[str, WhirlTensorTypeRecord],
) -> None:
    _verify_payload_record_set(module.tensor_payloads)
    payloads = {
        (payload.storage_file, payload.tensor_key): payload
        for payload in module.tensor_payloads
    }
    strict_payloads = bool(payloads)
    for value in module.values:
        if value.value_kind != "external_data":
            continue
        tensor_type = tensor_types.get(value.type_name)
        if tensor_type is None:
            raise WhirlVerificationError(
                f"external value {value.name} has no tensor type"
            )
        metadata = value.metadata
        for field in (
            "storage_format",
            "storage_file",
            "storage_tensor_key",
            "storage_byte_offset",
            "storage_byte_length",
            "storage_checksum",
            "storage_dtype",
            "storage_shape",
            "storage_layout",
        ):
            if field not in metadata:
                raise WhirlVerificationError(
                    f"external value {value.name} missing payload metadata: "
                    f"{field}"
                )
        if metadata["storage_format"] != "safetensors":
            raise WhirlVerificationError(
                f"external value {value.name} uses unsupported payload "
                f"format: {metadata['storage_format']}"
            )
        if metadata["storage_dtype"] != tensor_type.dtype:
            raise WhirlVerificationError(
                f"external value {value.name} payload dtype mismatch"
            )
        if metadata["storage_shape"] != tensor_type.logical_shape:
            raise WhirlVerificationError(
                f"external value {value.name} payload shape mismatch"
            )
        byte_offset = _metadata_int(value, "storage_byte_offset")
        byte_length = _metadata_int(value, "storage_byte_length")
        if byte_offset < 0:
            raise WhirlVerificationError(
                f"external value {value.name} has negative payload offset"
            )
        if byte_length <= 0:
            raise WhirlVerificationError(
                f"external value {value.name} has non-positive payload length"
            )
        if not strict_payloads:
            continue
        payload = payloads.get(
            (metadata["storage_file"], metadata["storage_tensor_key"])
        )
        if payload is None:
            raise WhirlVerificationError(
                f"external value {value.name} has no matching tensor payload"
            )
        _verify_external_payload_match(value, tensor_type, payload)


def _verify_payload_record_set(
    payloads: Sequence[WhirlTensorPayloadRecord],
) -> None:
    seen_keys = set()
    ranges_by_file: Dict[str, list[Tuple[int, int, str]]] = {}

    for payload in payloads:
        key = (payload.storage_file, payload.tensor_key)
        if key in seen_keys:
            raise WhirlVerificationError(
                f"duplicate tensor payload key: {payload.tensor_key}"
            )
        seen_keys.add(key)

        if payload.dtype not in _SUPPORTED_DTYPES:
            raise WhirlVerificationError(
                f"tensor payload {payload.tensor_key} has unsupported dtype: "
                f"{payload.dtype}"
            )
        if payload.byte_offset < 0:
            raise WhirlVerificationError(
                f"tensor payload {payload.tensor_key} has negative offset"
            )
        if payload.byte_length <= 0:
            raise WhirlVerificationError(
                f"tensor payload {payload.tensor_key} has non-positive length"
            )
        if payload.byte_length != len(payload.data):
            raise WhirlVerificationError(
                f"tensor payload {payload.tensor_key} data length mismatch"
            )
        checksum = hashlib.sha256(payload.data).hexdigest()
        if payload.checksum != checksum:
            raise WhirlVerificationError(
                f"tensor payload {payload.tensor_key} checksum mismatch"
            )

        ranges_by_file.setdefault(payload.storage_file, []).append(
            (
                payload.byte_offset,
                payload.byte_offset + payload.byte_length,
                payload.tensor_key,
            )
        )

    for ranges in ranges_by_file.values():
        previous_end = -1
        previous_key = ""
        for start, end, tensor_key in sorted(ranges):
            if start < previous_end:
                raise WhirlVerificationError(
                    "overlapping tensor payload ranges: "
                    f"{previous_key} and {tensor_key}"
                )
            previous_end = end
            previous_key = tensor_key


def _metadata_int(value: WhirlValueRecord, field: str) -> int:
    try:
        return int(value.metadata[field])
    except ValueError as exc:
        raise WhirlVerificationError(
            f"external value {value.name} has invalid payload integer: {field}"
        ) from exc


def _verify_external_payload_match(
    value: WhirlValueRecord,
    tensor_type: WhirlTensorTypeRecord,
    payload: WhirlTensorPayloadRecord,
) -> None:
    metadata = value.metadata
    if payload.dtype != tensor_type.dtype:
        raise WhirlVerificationError(
            f"external value {value.name} payload dtype mismatch"
        )
    if payload.logical_shape != tensor_type.logical_shape:
        raise WhirlVerificationError(
            f"external value {value.name} payload shape mismatch"
        )
    if payload.byte_offset != _metadata_int(value, "storage_byte_offset"):
        raise WhirlVerificationError(
            f"external value {value.name} payload offset mismatch"
        )
    if payload.byte_length != _metadata_int(value, "storage_byte_length"):
        raise WhirlVerificationError(
            f"external value {value.name} payload length mismatch"
        )
    if payload.checksum != metadata["storage_checksum"]:
        raise WhirlVerificationError(
            f"external value {value.name} payload checksum mismatch"
        )
    if payload.byte_length != len(payload.data):
        raise WhirlVerificationError(
            f"external value {value.name} payload data length mismatch"
        )


def _verify_graph_operators(
    graph_operators: Sequence[WhirlOperatorRecord],
    value_types: Mapping[str, Optional[WhirlTensorTypeRecord]],
) -> None:
    produced_types: Dict[str, Optional[WhirlTensorTypeRecord]] = {}
    for operator in graph_operators:
        if operator.name not in _KNOWN_OPERATORS:
            raise WhirlVerificationError(f"unknown operator: {operator.name}")
        expected_arity = operator_arity(operator.name)
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
    _require_contract_attrs(operator)

    if operator.name == common.ADD:
        if operator.attrs.get("attr.broadcast_rule") != "none":
            return
        _require_same_tensor_type(operator, operand_types)
        return

    if operator.name == common.RESIDUAL_ADD:
        if operator.attrs.get("attr.broadcast_rule") != "none":
            raise WhirlVerificationError(
                "common.residual_add does not allow broadcast operands"
            )
        if operator.attrs.get("attr.shape_check") != "exact":
            raise WhirlVerificationError(
                "common.residual_add requires attr.shape_check=exact"
            )
        _require_same_tensor_type(operator, operand_types)
        return

    if operator.name == common.MATMUL:
        _verify_matmul_contract(operator, operand_types)
        return

    if operator.name == common.LINEAR:
        _verify_linear_contract(operator, operand_types)
        return

    if operator.name == cnn.CONV2D:
        _verify_conv2d_contract(operator, operand_types)
        return

    if operator.name == cnn.BATCH_NORM_INFER:
        _verify_batch_norm_infer_contract(operator, operand_types)
        return

    if operator.name == cnn.MAX_POOL2D:
        _verify_max_pool2d_contract(operator, operand_types)
        return

    if operator.name == cnn.GLOBAL_AVG_POOL2D:
        _verify_global_avg_pool2d_contract(operator, operand_types)


def _require_attr(operator: WhirlOperatorRecord, name: str) -> None:
    if name not in operator.attrs:
        raise WhirlVerificationError(
            f"{operator.name} missing required attribute: {name}"
        )


def _require_contract_attrs(operator: WhirlOperatorRecord) -> None:
    for attr_name in required_attrs(operator.name):
        _require_attr(operator, attr_name)


def _verify_matmul_contract(
    operator: WhirlOperatorRecord,
    operand_types: Sequence[Optional[WhirlTensorTypeRecord]],
) -> None:
    transpose_lhs = _bool_attr(operator, "attr.transpose_kid0")
    transpose_rhs = _bool_attr(operator, "attr.transpose_kid1")
    lhs = _require_typed_operand(operator, operand_types, 0)
    rhs = _require_typed_operand(operator, operand_types, 1)
    _require_same_dtype(operator, (lhs, rhs))
    lhs_shape = _require_rank(operator, lhs, 2)
    rhs_shape = _require_rank(operator, rhs, 2)
    lhs_matrix = _effective_matrix_shape(lhs_shape, transpose_lhs)
    rhs_matrix = _effective_matrix_shape(rhs_shape, transpose_rhs)
    if lhs_matrix[1] != rhs_matrix[0]:
        raise WhirlVerificationError(
            "common.matmul operands have incompatible matrix dimensions: "
            f"{lhs.logical_shape} x {rhs.logical_shape}"
        )


def _verify_linear_contract(
    operator: WhirlOperatorRecord,
    operand_types: Sequence[Optional[WhirlTensorTypeRecord]],
) -> None:
    has_bias = _bool_attr(operator, "attr.has_bias")
    transpose_input = _bool_attr(operator, "attr.transpose_input")
    transpose_weight = _bool_attr(operator, "attr.transpose_weight")
    _require_attr(operator, "attr.weight_layout")
    value = operand_types[0]
    weight = _require_typed_operand(operator, operand_types, 1)
    bias = operand_types[2]
    if value is not None:
        _require_same_dtype(operator, (value, weight))
        value_shape = _require_rank(operator, value, 2)
        value_matrix = _effective_matrix_shape(value_shape, transpose_input)
    else:
        value_matrix = None
    weight_shape = _require_rank(operator, weight, 2)
    weight_matrix = _effective_matrix_shape(weight_shape, transpose_weight)
    if value_matrix is not None and value_matrix[1] != weight_matrix[0]:
        raise WhirlVerificationError(
            "common.linear input and weight dimensions are incompatible: "
            f"{value.logical_shape} x {weight.logical_shape}"
        )

    output_features = weight_matrix[1]
    if has_bias:
        if bias is None:
            raise WhirlVerificationError(
                "common.linear attr.has_bias=true requires a bias operand"
            )
        _require_same_dtype(operator, (value or weight, bias))
        bias_shape = _require_rank(operator, bias, 1)
        if bias_shape[0] != output_features:
            raise WhirlVerificationError(
                "common.linear bias shape does not match output features: "
                f"{bias.logical_shape} vs {output_features}"
            )
    elif bias is not None:
        raise WhirlVerificationError(
            "common.linear attr.has_bias=false requires an absent bias operand"
        )


def _verify_conv2d_contract(
    operator: WhirlOperatorRecord,
    operand_types: Sequence[Optional[WhirlTensorTypeRecord]],
) -> None:
    for attr_name in (
        "attr.kernel_shape",
        "attr.stride",
        "attr.padding",
        "attr.dilation",
        "attr.groups",
        "attr.input_layout",
        "attr.weight_layout",
        "attr.output_layout",
    ):
        _require_attr(operator, attr_name)
    if operator.attrs["attr.input_layout"] != "NCHW":
        raise WhirlVerificationError("cnn.conv2d requires input_layout=NCHW")
    if operator.attrs["attr.weight_layout"] != "OIHW":
        raise WhirlVerificationError("cnn.conv2d requires weight_layout=OIHW")
    if operator.attrs["attr.output_layout"] != "NCHW":
        raise WhirlVerificationError("cnn.conv2d requires output_layout=NCHW")

    value = _require_typed_operand(operator, operand_types, 0)
    weight = _require_typed_operand(operator, operand_types, 1)
    bias = operand_types[2]
    _require_same_dtype(operator, (value, weight))
    value_shape = _require_rank(operator, value, 4)
    weight_shape = _require_rank(operator, weight, 4)
    _pair_attr(operator, "attr.kernel_shape")
    _pair_attr(operator, "attr.stride")
    _pair_attr(operator, "attr.padding", allow_zero=True)
    _pair_attr(operator, "attr.dilation")
    groups = _positive_int_attr(operator, "attr.groups")
    if groups <= 0:
        raise WhirlVerificationError("cnn.conv2d groups must be positive")
    if value_shape[1] != weight_shape[1] * groups:
        raise WhirlVerificationError(
            "cnn.conv2d input channels do not match weight channels/groups: "
            f"{value_shape[1]} vs {weight_shape[1]}*{groups}"
        )
    if bias is not None:
        _require_same_dtype(operator, (value, bias))
        bias_shape = _require_rank(operator, bias, 1)
        if bias_shape[0] != weight_shape[0]:
            raise WhirlVerificationError(
                "cnn.conv2d bias shape does not match output channels: "
                f"{bias.logical_shape} vs {weight_shape[0]}"
            )


def _verify_batch_norm_infer_contract(
    operator: WhirlOperatorRecord,
    operand_types: Sequence[Optional[WhirlTensorTypeRecord]],
) -> None:
    if operator.attrs["attr.training"] != "false":
        raise WhirlVerificationError(
            "cnn.batch_norm_infer requires attr.training=false"
        )
    if operator.attrs["attr.input_layout"] != "NCHW":
        raise WhirlVerificationError(
            "cnn.batch_norm_infer requires input_layout=NCHW"
        )
    if operator.attrs["attr.channel_axis"] != "1":
        raise WhirlVerificationError(
            "cnn.batch_norm_infer requires channel_axis=1"
        )

    value = _require_typed_operand(operator, operand_types, 0)
    params = [
        _require_typed_operand(operator, operand_types, index)
        for index in range(1, 5)
    ]
    value_shape = _require_rank(operator, value, 4)
    channel_count = value_shape[1]
    for parameter in params:
        _require_same_dtype(operator, (value, parameter))
        parameter_shape = _require_rank(operator, parameter, 1)
        if parameter_shape[0] != channel_count:
            raise WhirlVerificationError(
                "cnn.batch_norm_infer parameter shape does not match "
                f"input channels: {parameter.logical_shape} vs {channel_count}"
            )


def _verify_max_pool2d_contract(
    operator: WhirlOperatorRecord,
    operand_types: Sequence[Optional[WhirlTensorTypeRecord]],
) -> None:
    for attr_name in (
        "attr.kernel_shape",
        "attr.stride",
        "attr.padding",
        "attr.dilation",
        "attr.ceil_mode",
    ):
        _require_attr(operator, attr_name)
    value = _require_typed_operand(operator, operand_types, 0)
    _require_rank(operator, value, 4)
    _pair_attr(operator, "attr.kernel_shape")
    _pair_attr(operator, "attr.stride")
    _pair_attr(operator, "attr.padding", allow_zero=True)
    _pair_attr(operator, "attr.dilation")
    _bool_attr(operator, "attr.ceil_mode")


def _verify_global_avg_pool2d_contract(
    operator: WhirlOperatorRecord,
    operand_types: Sequence[Optional[WhirlTensorTypeRecord]],
) -> None:
    _require_attr(operator, "attr.output_size")
    _require_attr(operator, "attr.reduction_axes")
    if operator.attrs["attr.reduction_axes"] != "spatial":
        raise WhirlVerificationError(
            "cnn.global_avg_pool2d requires reduction_axes=spatial"
        )
    value = _require_typed_operand(operator, operand_types, 0)
    _require_rank(operator, value, 4)
    _pair_attr(operator, "attr.output_size")


def _bool_attr(operator: WhirlOperatorRecord, name: str) -> bool:
    _require_attr(operator, name)
    value = operator.attrs.get(name)
    if value == "true":
        return True
    if value == "false":
        return False
    raise WhirlVerificationError(
        f"{operator.name} attribute {name} must be true or false"
    )


def _require_typed_operand(
    operator: WhirlOperatorRecord,
    operand_types: Sequence[Optional[WhirlTensorTypeRecord]],
    index: int,
) -> WhirlTensorTypeRecord:
    operand_type = operand_types[index]
    if operand_type is None:
        raise WhirlVerificationError(
            f"{operator.name} operand {index} must be a tensor"
        )
    return operand_type


def _require_rank(
    operator: WhirlOperatorRecord,
    tensor_type: WhirlTensorTypeRecord,
    rank: int,
) -> Tuple[int, ...]:
    if tensor_type.rank != rank:
        if operator.name == common.MATMUL and tensor_type.rank > 2:
            raise WhirlVerificationError(
                "common.matmul batched matmul is not supported yet"
            )
        raise WhirlVerificationError(
            f"{operator.name} operand {tensor_type.name} requires rank {rank}, "
            f"got {tensor_type.rank}"
        )
    return _parse_logical_shape(
        tensor_type.logical_shape,
        f"tensor type {tensor_type.name}",
    )


def _pair_attr(
    operator: WhirlOperatorRecord,
    name: str,
    allow_zero: bool = False,
) -> Tuple[int, int]:
    _require_attr(operator, name)
    parts = operator.attrs[name].split(",")
    if len(parts) != 2:
        raise WhirlVerificationError(
            f"{operator.name} attribute {name} must be a pair"
        )
    try:
        first, second = (int(parts[0]), int(parts[1]))
    except ValueError as exc:
        raise WhirlVerificationError(
            f"{operator.name} attribute {name} must contain integers"
        ) from exc
    if allow_zero:
        valid = first >= 0 and second >= 0
    else:
        valid = first > 0 and second > 0
    if not valid:
        raise WhirlVerificationError(
            f"{operator.name} attribute {name} has invalid pair: "
            f"{operator.attrs[name]}"
        )
    return (first, second)


def _positive_int_attr(operator: WhirlOperatorRecord, name: str) -> int:
    value = _int_attr(operator, name)
    if value <= 0:
        raise WhirlVerificationError(
            f"{operator.name} attribute {name} must be positive"
        )
    return value


def _require_same_dtype(
    operator: WhirlOperatorRecord,
    operand_types: Sequence[WhirlTensorTypeRecord],
) -> None:
    first = operand_types[0]
    for operand_type in operand_types[1:]:
        if operand_type.dtype != first.dtype:
            raise WhirlVerificationError(
                f"{operator.name} operands have incompatible dtype: "
                f"{first.dtype} vs {operand_type.dtype}"
            )


def _effective_matrix_shape(
    shape: Tuple[int, ...],
    transpose: bool,
) -> Tuple[int, int]:
    if transpose:
        return (shape[1], shape[0])
    return (shape[0], shape[1])


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
    if operator.name == common.FLATTEN:
        return _flatten_result_type(operator, operand_types)
    if operator.name == common.LINEAR:
        return _linear_result_type(operator, operand_types)
    if operator.name == cnn.CONV2D:
        return _conv2d_result_type(operator, operand_types)
    if operator.name == cnn.BATCH_NORM_INFER:
        return operand_types[0]
    if operator.name == cnn.MAX_POOL2D:
        return _max_pool2d_result_type(operator, operand_types)
    if operator.name == cnn.GLOBAL_AVG_POOL2D:
        return _global_avg_pool2d_result_type(operator, operand_types)
    if operator.name in {
        common.ADD,
        common.OUTPUT_LOGITS,
        common.RELU,
        common.RESIDUAL_ADD,
    }:
        return operand_types[0]
    return None


def _flatten_result_type(
    operator: WhirlOperatorRecord,
    operand_types: Sequence[Optional[WhirlTensorTypeRecord]],
) -> Optional[WhirlTensorTypeRecord]:
    value = operand_types[0]
    if value is None:
        return None
    shape = _parse_logical_shape(
        value.logical_shape,
        f"tensor type {value.name}",
    )
    start_dim = _int_attr(operator, "attr.start_dim")
    end_dim = _int_attr(operator, "attr.end_dim")
    rank = len(shape)
    if start_dim < 0:
        start_dim += rank
    if end_dim < 0:
        end_dim += rank
    if start_dim < 0 or end_dim < start_dim or end_dim >= rank:
        raise WhirlVerificationError(
            "common.flatten has invalid start/end dimensions: "
            f"{operator.attrs.get('attr.start_dim')}.."
            f"{operator.attrs.get('attr.end_dim')}"
        )

    flattened = 1
    for dim in shape[start_dim:end_dim + 1]:
        flattened *= dim
    result_shape = shape[:start_dim] + (flattened,) + shape[end_dim + 1:]
    logical_shape = _format_logical_shape(result_shape)
    return WhirlTensorTypeRecord(
        f"{operator.name}_result_type",
        value.handle,
        value.dtype,
        len(result_shape),
        logical_shape,
        {
            "dtype": value.dtype,
            "rank": len(result_shape),
            "logical_shape": logical_shape,
            "lineage": operator.name,
        },
    )


def _conv2d_result_type(
    operator: WhirlOperatorRecord,
    operand_types: Sequence[Optional[WhirlTensorTypeRecord]],
) -> Optional[WhirlTensorTypeRecord]:
    value = operand_types[0]
    weight = operand_types[1]
    if value is None or weight is None:
        return None
    value_shape = _parse_logical_shape(
        value.logical_shape,
        f"tensor type {value.name}",
    )
    weight_shape = _parse_logical_shape(
        weight.logical_shape,
        f"tensor type {weight.name}",
    )
    stride = _pair_attr(operator, "attr.stride")
    padding = _pair_attr(operator, "attr.padding", allow_zero=True)
    dilation = _pair_attr(operator, "attr.dilation")
    kernel = (weight_shape[2], weight_shape[3])
    output_h = _conv_output_dim(value_shape[2], kernel[0], stride[0],
                                padding[0], dilation[0])
    output_w = _conv_output_dim(value_shape[3], kernel[1], stride[1],
                                padding[1], dilation[1])
    return _synthetic_tensor_type(
        operator,
        value,
        (value_shape[0], weight_shape[0], output_h, output_w),
    )


def _max_pool2d_result_type(
    operator: WhirlOperatorRecord,
    operand_types: Sequence[Optional[WhirlTensorTypeRecord]],
) -> Optional[WhirlTensorTypeRecord]:
    value = operand_types[0]
    if value is None:
        return None
    value_shape = _parse_logical_shape(
        value.logical_shape,
        f"tensor type {value.name}",
    )
    kernel = _pair_attr(operator, "attr.kernel_shape")
    stride = _pair_attr(operator, "attr.stride")
    padding = _pair_attr(operator, "attr.padding", allow_zero=True)
    dilation = _pair_attr(operator, "attr.dilation")
    output_h = _conv_output_dim(value_shape[2], kernel[0], stride[0],
                                padding[0], dilation[0])
    output_w = _conv_output_dim(value_shape[3], kernel[1], stride[1],
                                padding[1], dilation[1])
    return _synthetic_tensor_type(
        operator,
        value,
        (value_shape[0], value_shape[1], output_h, output_w),
    )


def _global_avg_pool2d_result_type(
    operator: WhirlOperatorRecord,
    operand_types: Sequence[Optional[WhirlTensorTypeRecord]],
) -> Optional[WhirlTensorTypeRecord]:
    value = operand_types[0]
    if value is None:
        return None
    value_shape = _parse_logical_shape(
        value.logical_shape,
        f"tensor type {value.name}",
    )
    output_size = _pair_attr(operator, "attr.output_size")
    return _synthetic_tensor_type(
        operator,
        value,
        (value_shape[0], value_shape[1], output_size[0], output_size[1]),
    )


def _conv_output_dim(
    input_dim: int,
    kernel: int,
    stride: int,
    padding: int,
    dilation: int,
) -> int:
    output = ((input_dim + 2 * padding - dilation * (kernel - 1) - 1)
              // stride + 1)
    if output <= 0:
        raise WhirlVerificationError(
            "CNN operator produced non-positive output dimension"
        )
    return output


def _synthetic_tensor_type(
    operator: WhirlOperatorRecord,
    value: WhirlTensorTypeRecord,
    shape: Sequence[int],
) -> WhirlTensorTypeRecord:
    logical_shape = _format_logical_shape(shape)
    return WhirlTensorTypeRecord(
        f"{operator.name}_result_type",
        value.handle,
        value.dtype,
        len(shape),
        logical_shape,
        {
            "dtype": value.dtype,
            "rank": len(shape),
            "logical_shape": logical_shape,
            "lineage": operator.name,
        },
    )


def _linear_result_type(
    operator: WhirlOperatorRecord,
    operand_types: Sequence[Optional[WhirlTensorTypeRecord]],
) -> Optional[WhirlTensorTypeRecord]:
    value = operand_types[0]
    weight = operand_types[1]
    if value is None or weight is None:
        return None
    transpose_weight = _bool_attr(operator, "attr.transpose_weight")
    value_shape = _parse_logical_shape(
        value.logical_shape,
        f"tensor type {value.name}",
    )
    weight_shape = _parse_logical_shape(
        weight.logical_shape,
        f"tensor type {weight.name}",
    )
    weight_matrix = _effective_matrix_shape(weight_shape, transpose_weight)
    result_shape = (value_shape[0], weight_matrix[1])
    logical_shape = _format_logical_shape(result_shape)
    return WhirlTensorTypeRecord(
        f"{operator.name}_result_type",
        value.handle,
        value.dtype,
        len(result_shape),
        logical_shape,
        {
            "dtype": value.dtype,
            "rank": len(result_shape),
            "logical_shape": logical_shape,
            "lineage": operator.name,
        },
    )


def _int_attr(operator: WhirlOperatorRecord, name: str) -> int:
    _require_attr(operator, name)
    try:
        return int(operator.attrs[name])
    except ValueError as exc:
        raise WhirlVerificationError(
            f"{operator.name} attribute {name} must be an integer"
        ) from exc


def _format_logical_shape(shape: Sequence[int]) -> str:
    return "[" + ",".join(str(dim) for dim in shape) + "]"


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
