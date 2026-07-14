"""Python-side facade for native WHIRL builder handles."""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import Mapping, Optional, Sequence

from .backend import WhirlBackend, load_backend
from .mapping import cnn, common
from .mapping.contract import operator_version


_TYPE_DESCRIPTORS: dict[str, dict[int, dict[str, object]]] = {}
_VALUE_TYPES: dict[str, dict[int, "TensorTypeHandle"]] = {}
_RESULT_ORDINALS: dict[str, int] = {}


@dataclass(frozen=True)
class OpaqueHandle:
    value: int

    def __post_init__(self) -> None:
        if self.value <= 0:
            raise ValueError("Open64 builder handle must be positive")


@dataclass(frozen=True)
class TensorTypeHandle(OpaqueHandle):
    pass


@dataclass(frozen=True)
class SymbolHandle(OpaqueHandle):
    pass


@dataclass(frozen=True)
class ValueHandle(OpaqueHandle):
    pass


@dataclass(frozen=True)
class TensorConstantHandle(ValueHandle):
    tensor_type: int = 0
    symbol: int = 0
    metadata: Mapping[str, str] = field(default_factory=dict)
    descriptor: Mapping[str, object] = field(default_factory=dict)


@dataclass(frozen=True)
class OperatorHandle(ValueHandle):
    pass


@dataclass(frozen=True)
class ProgramUnitHandle(OpaqueHandle):
    pass


class WhirlBuilder:
    def __init__(self, backend: WhirlBackend):
        self._backend = backend
        backend_name = backend.backend_name()
        self._backend_key = backend_name
        self._type_descriptors = _TYPE_DESCRIPTORS.setdefault(backend_name, {})
        self._value_types = _VALUE_TYPES.setdefault(backend_name, {})

    def backend_name(self) -> str:
        return self._backend.backend_name()

    def tensor_type(
        self,
        name: str,
        dtype: str,
        rank: int,
        logical_shape: str,
        descriptor: Optional[Mapping[str, object]] = None,
    ) -> TensorTypeHandle:
        canonical = dict(descriptor or {})
        canonical.update({
            "kind": str(canonical.get("kind", "tensor")),
            "dtype": dtype,
            "rank": rank,
            "logical_shape": logical_shape,
        })
        canonical.pop("runtime_state", None)
        canonical.pop("lineage", None)
        tensor_type = TensorTypeHandle(
            self._backend.intern_tensor_type(name, canonical)
        )
        self._type_descriptors[tensor_type.value] = canonical
        return tensor_type

    def attach_tensor_descriptor(
        self,
        tensor_type: TensorTypeHandle,
        descriptor: Mapping[str, object],
    ) -> None:
        canonical = dict(descriptor)
        canonical.pop("runtime_state", None)
        canonical.pop("lineage", None)
        current = self._type_descriptors.get(tensor_type.value)
        if current is None or any(
            current.get(key) != value for key, value in canonical.items()
        ):
            raise RuntimeError("canonical tensor descriptor cannot be mutated")

    def begin_program(self) -> None:
        if not self._backend.begin_program():
            raise RuntimeError("failed to begin native builder program")
        self._value_types.clear()
        _RESULT_ORDINALS[self._backend_key] = 0

    def abort_program(self) -> None:
        self._backend.abort_program()
        self._value_types.clear()

    def verify_program(self) -> Mapping[str, object]:
        result = self._backend.verify_program()
        if not bool(result.get("valid", False)):
            diagnostic = str(result.get("diagnostic", ""))
            raise RuntimeError(
                diagnostic or "native DSL program verification failed"
            )
        return result

    def symbol(
        self,
        name: str,
        tensor_type: TensorTypeHandle,
    ) -> SymbolHandle:
        return SymbolHandle(
            self._backend.create_symbol(
                name,
                tensor_type.value,
            )
        )

    def attach_symbol_metadata(
        self,
        symbol: SymbolHandle,
        metadata: Mapping[str, str],
    ) -> None:
        if not self._backend.attach_symbol_metadata(
            symbol.value,
            metadata,
        ):
            raise RuntimeError("failed to attach symbol metadata")

    def minimal_program_unit(self, name: str) -> ProgramUnitHandle:
        return ProgramUnitHandle(
            self._backend.create_minimal_program_unit(name)
        )

    def register_source_file(
        self,
        program_unit: ProgramUnitHandle,
        path: str,
    ) -> int:
        return self._backend.register_source_file(program_unit.value, path)

    def set_value_source_position(
        self,
        value: ValueHandle,
        file_id: int,
        line: int,
        column: int = 0,
        statement_begin: bool = True,
        basic_block_begin: bool = False,
    ) -> None:
        if not self._backend.set_value_source_position(
            value.value,
            file_id,
            line,
            column,
            statement_begin,
            basic_block_begin,
        ):
            raise RuntimeError("failed to set value source position")

    def attach_value_metadata(
        self,
        value: ValueHandle,
        metadata: Mapping[str, str],
    ) -> None:
        if not self._backend.attach_value_metadata(value.value, metadata):
            raise RuntimeError("failed to attach value metadata")

    def attach_value_lineage(self, value: ValueHandle, lineage: str) -> None:
        if not self._backend.attach_value_lineage(value.value, lineage):
            raise RuntimeError("failed to attach value lineage")

    def value_result_symbol(self, value: ValueHandle) -> SymbolHandle:
        return SymbolHandle(self._backend.get_value_result_symbol(value.value))

    def value_type(self, value: ValueHandle) -> TensorTypeHandle:
        tensor_type = self._value_types.get(value.value)
        if tensor_type is None:
            tensor_type = TensorTypeHandle(self._backend.get_value_type(value.value))
            self._value_types[value.value] = tensor_type
        return tensor_type

    def append_program_unit_marker(
        self,
        program_unit: ProgramUnitHandle,
        marker: ValueHandle,
    ) -> None:
        self.append_program_unit_value(program_unit, marker)

    def append_program_unit_value(
        self,
        program_unit: ProgramUnitHandle,
        value: ValueHandle,
    ) -> None:
        append = getattr(
            self._backend,
            "append_program_unit_value",
            self._backend.append_program_unit_marker,
        )
        if not append(
            program_unit.value,
            value.value,
        ):
            raise RuntimeError("failed to append program unit value")

    def inspect_program_unit_markers(
        self,
        program_unit: ProgramUnitHandle,
    ) -> Sequence[Mapping[str, object]]:
        return self.inspect_program_unit_values(program_unit)

    def inspect_program_unit_values(
        self,
        program_unit: ProgramUnitHandle,
    ) -> Sequence[Mapping[str, object]]:
        inspect = getattr(
            self._backend,
            "inspect_program_unit_values",
            self._backend.inspect_program_unit_markers,
        )
        return inspect(program_unit.value)

    def tensor_constant(
        self,
        name: str,
        dtype: str,
        rank: int,
        logical_shape: str,
        value_kind: str,
        value: str,
    ) -> ValueHandle:
        tensor_type = self.tensor_type(
            f"{name}_type", dtype, rank, logical_shape
        )
        value_handle = ValueHandle(
            self._backend.create_tensor_constant(
                name,
                dtype,
                rank,
                logical_shape,
                value_kind,
                value,
            )
        )
        self._value_types[value_handle.value] = tensor_type
        return value_handle

    def model_input(
        self,
        name: str,
        tensor_type: TensorTypeHandle,
        input_ordinal: int,
    ) -> ValueHandle:
        if input_ordinal < 0:
            raise ValueError("model input ordinal must be non-negative")
        create_model_input = getattr(self._backend, "create_model_input", None)
        if create_model_input is None:
            raise self._value_source_capability_error(
                common.MODEL_INPUT,
                operator_version(common.MODEL_INPUT),
            )
        try:
            handle = create_model_input(
                name,
                tensor_type.value,
                input_ordinal,
            )
        except RuntimeError as exc:
            raise self._value_source_capability_error(
                common.MODEL_INPUT,
                operator_version(common.MODEL_INPUT),
            ) from exc
        if handle <= 0:
            raise self._value_source_capability_error(
                common.MODEL_INPUT,
                operator_version(common.MODEL_INPUT),
            )
        value = ValueHandle(handle)
        self._value_types[value.value] = tensor_type
        return value

    def external_tensor_constant(
        self,
        name: str,
        dtype: str,
        rank: int,
        logical_shape: str,
        role: str,
        storage_format: str,
        side_file: str,
        tensor_key: str,
        byte_offset: int,
        byte_length: int,
        checksum: str = "",
        layout: str = "contiguous",
    ) -> TensorConstantHandle:
        if byte_offset < 0:
            raise ValueError("external tensor byte offset must be non-negative")
        if byte_length <= 0:
            raise ValueError("external tensor byte length must be positive")
        if not side_file:
            raise ValueError("external tensor side file is required")
        if not tensor_key:
            raise ValueError("external tensor key is required")
        element_size = self._dtype_byte_size(dtype)
        if byte_offset % element_size != 0:
            raise ValueError("external tensor byte offset is misaligned")
        expected_length = self._static_tensor_byte_length(
            dtype,
            logical_shape,
        )
        if byte_length != expected_length:
            raise ValueError(
                "external tensor byte length does not match static shape"
            )
        if checksum and (
            len(checksum) != 64 or
            any(char not in "0123456789abcdefABCDEF" for char in checksum)
        ):
            raise ValueError("external tensor checksum must be 64 hex characters")

        descriptor = {
            "kind": "tensor",
            "dtype": dtype,
            "rank": rank,
            "logical_shape": logical_shape,
            "traits": role,
            "layout": layout,
            "sharding": "replicated",
            "placement": "side_file",
            "memory": "external_data",
            "quantization": "none",
        }
        tensor_type = self.tensor_type(
            f"{name}_type",
            dtype,
            rank,
            logical_shape,
            descriptor,
        )
        metadata = {
            "source_layer_name": name,
            "lowering_hint": "external_tensor_constant",
            "tensor_role": role,
            "storage_dtype": dtype,
            "storage_shape": logical_shape,
            "storage_layout": layout,
            "storage_format": storage_format,
            "storage_file": side_file,
            "storage_tensor_key": tensor_key,
            "storage_byte_offset": str(byte_offset),
            "storage_byte_length": str(byte_length),
            "storage_checksum": checksum,
        }
        create_external = getattr(
            self._backend,
            "create_external_tensor_constant",
            None,
        )
        if create_external is None:
            raise self._value_source_capability_error(
                common.TENSOR_CONST,
                operator_version(common.TENSOR_CONST),
                "external_data",
            )

        reference = {
            "storage_format": storage_format,
            "side_file": side_file,
            "tensor_key": tensor_key,
            "byte_offset": byte_offset,
            "byte_length": byte_length,
            "checksum": checksum,
        }
        try:
            value_handle = create_external(name, tensor_type.value, reference)
        except RuntimeError as exc:
            raise self._value_source_capability_error(
                common.TENSOR_CONST,
                operator_version(common.TENSOR_CONST),
                "external_data",
            ) from exc
        if value_handle <= 0:
            raise self._value_source_capability_error(
                common.TENSOR_CONST,
                operator_version(common.TENSOR_CONST),
                "external_data",
            )
        value = ValueHandle(value_handle)
        self._value_types[value.value] = tensor_type
        self.attach_value_metadata(value, metadata)
        self.attach_value_lineage(value, name)
        symbol = int(self._backend.get_value_result_symbol(value.value))
        return TensorConstantHandle(
            value.value,
            tensor_type.value,
            symbol,
            metadata,
            descriptor,
        )

    def _dtype_byte_size(self, dtype: str) -> int:
        if dtype in {"float64", "int64"}:
            return 8
        if dtype in {"float32", "int32"}:
            return 4
        if dtype in {"float16", "bfloat16", "int16"}:
            return 2
        if dtype in {"bool", "int8", "uint8"}:
            return 1
        raise ValueError(f"unsupported external tensor dtype: {dtype}")

    def _static_tensor_byte_length(
        self,
        dtype: str,
        logical_shape: str,
    ) -> int:
        if not logical_shape.startswith("[") or not logical_shape.endswith("]"):
            raise ValueError(
                f"invalid external tensor logical shape: {logical_shape}"
            )
        body = logical_shape[1:-1]
        element_count = 1
        if body:
            for part in body.split(","):
                dim = int(part)
                if dim < 0:
                    raise ValueError(
                        f"invalid external tensor dimension: {logical_shape}"
                    )
                element_count *= dim
        return element_count * self._dtype_byte_size(dtype)

    def _value_source_capability_error(
        self,
        opcode_name: str,
        version: int,
        value_kind: str = "",
    ) -> RuntimeError:
        suffix = f" {value_kind}" if value_kind else ""
        return RuntimeError(
            f"{self.backend_name()} backend does not support "
            f"{opcode_name}.v{version}{suffix}"
        )

    def operator(
        self,
        opcode_name: str,
        version: int,
        kids: Sequence[ValueHandle],
        attrs: Mapping[str, str],
        result_name: Optional[str] = None,
        result_type: Optional[TensorTypeHandle] = None,
    ) -> OperatorHandle:
        if not kids:
            raise ValueError("DSL operator requires at least one operand")
        create_with_result = getattr(
            self._backend, "create_operator_with_result", None
        )
        if create_with_result is None:
            raise self._operator_capability_error(opcode_name, version)
        if result_name is None:
            result_name = self._next_result_name(opcode_name)
        if result_type is None:
            result_type = self._infer_result_type(
                result_name, opcode_name, kids, attrs
            )
        try:
            handle = create_with_result(
                opcode_name,
                version,
                [kid.value for kid in kids],
                attrs,
                result_name,
                result_type.value,
            )
        except RuntimeError as exc:
            raise self._operator_capability_error(opcode_name, version) from exc
        if handle <= 0:
            raise self._operator_capability_error(opcode_name, version)
        result = OperatorHandle(handle)
        self._value_types[result.value] = result_type
        self.attach_value_lineage(result, opcode_name)
        return result

    def _operator_capability_error(
        self,
        opcode_name: str,
        version: int,
    ) -> RuntimeError:
        return RuntimeError(
            f"{self.backend_name()} backend does not support "
            f"{opcode_name}.v{version}"
        )

    def _next_result_name(self, opcode_name: str) -> str:
        ordinal = _RESULT_ORDINALS.get(self._backend_key, 0) + 1
        _RESULT_ORDINALS[self._backend_key] = ordinal
        return f"{opcode_name.replace('.', '_')}_{ordinal}"

    def _infer_result_type(
        self,
        result_name: str,
        opcode_name: str,
        kids: Sequence[ValueHandle],
        attrs: Mapping[str, str],
    ) -> TensorTypeHandle:
        input_type = self._value_types.get(kids[0].value)
        if input_type is None:
            raise RuntimeError("operator operand has no registered tensor type")
        descriptor = dict(self._type_descriptors[input_type.value])
        shape = self._parse_shape(str(descriptor["logical_shape"]))

        if opcode_name == "common.matmul" and len(kids) > 1:
            rhs_type = self._value_types[kids[1].value]
            rhs_shape = self._parse_shape(
                str(self._type_descriptors[rhs_type.value]["logical_shape"])
            )
            shape = (shape[0], rhs_shape[1])
        elif opcode_name == "common.flatten":
            start = int(attrs.get("attr.start_dim", "1"))
            end = int(attrs.get("attr.end_dim", "-1"))
            if end < 0:
                end += len(shape)
            flattened = 1
            for dimension in shape[start:end + 1]:
                flattened *= dimension
            shape = shape[:start] + (flattened,) + shape[end + 1:]
        elif opcode_name in {"cnn.conv2d", "cnn.max_pool2d"}:
            if len(shape) != 4:
                return input_type
            kernel = self._parse_pair(attrs["attr.kernel_shape"])
            stride = self._parse_pair(attrs["attr.stride"])
            padding = self._parse_pair(attrs["attr.padding"])
            dilation = self._parse_pair(attrs["attr.dilation"])
            channels = shape[1]
            if opcode_name == "cnn.conv2d":
                weight_type = self._value_types[kids[1].value]
                weight_shape = self._parse_shape(
                    str(self._type_descriptors[weight_type.value]["logical_shape"])
                )
                channels = weight_shape[0]
                kernel = (weight_shape[2], weight_shape[3])
            shape = (
                shape[0],
                channels,
                self._conv_dim(shape[2], kernel[0], stride[0],
                               padding[0], dilation[0]),
                self._conv_dim(shape[3], kernel[1], stride[1],
                               padding[1], dilation[1]),
            )
        elif opcode_name == "cnn.global_avg_pool2d":
            if len(shape) != 4:
                return input_type
            output = self._parse_pair(attrs.get("attr.output_size", "1,1"))
            shape = (shape[0], shape[1], output[0], output[1])
        elif opcode_name == "common.linear" and len(kids) > 1:
            weight_type = self._value_types[kids[1].value]
            weight_shape = self._parse_shape(
                str(self._type_descriptors[weight_type.value]["logical_shape"])
            )
            shape = (shape[0], weight_shape[0])

        descriptor["rank"] = len(shape)
        descriptor["logical_shape"] = self._format_shape(shape)
        return self.tensor_type(
            f"{result_name}_type",
            str(descriptor["dtype"]),
            len(shape),
            str(descriptor["logical_shape"]),
            descriptor,
        )

    @staticmethod
    def _parse_shape(shape: str) -> tuple[int, ...]:
        body = shape.strip()[1:-1]
        return tuple(int(value) for value in body.split(",")) if body else ()

    @staticmethod
    def _format_shape(shape: Sequence[int]) -> str:
        return "[" + ",".join(str(value) for value in shape) + "]"

    @staticmethod
    def _parse_pair(value: str) -> tuple[int, int]:
        parts = tuple(int(item) for item in value.split(","))
        return (parts[0], parts[0]) if len(parts) == 1 else (parts[0], parts[1])

    @staticmethod
    def _conv_dim(
        value: int,
        kernel: int,
        stride: int,
        padding: int,
        dilation: int,
    ) -> int:
        return ((value + 2 * padding - dilation * (kernel - 1) - 1)
                // stride + 1)

    def common_add(
        self,
        lhs: ValueHandle,
        rhs: ValueHandle,
        attrs: Optional[Mapping[str, str]] = None,
    ) -> OperatorHandle:
        return self.operator(
            common.ADD,
            operator_version(common.ADD),
            [lhs, rhs],
            attrs or {"attr.broadcast_rule": "none"},
        )

    def common_matmul(
        self,
        lhs: ValueHandle,
        rhs: ValueHandle,
        attrs: Optional[Mapping[str, str]] = None,
    ) -> OperatorHandle:
        return self.operator(
            common.MATMUL,
            operator_version(common.MATMUL),
            [lhs, rhs],
            attrs or {
                "attr.transpose_kid0": "false",
                "attr.transpose_kid1": "false",
            },
        )

    def common_residual_add(
        self,
        lhs: ValueHandle,
        rhs: ValueHandle,
        attrs: Optional[Mapping[str, str]] = None,
    ) -> OperatorHandle:
        return self.operator(
            common.RESIDUAL_ADD,
            operator_version(common.RESIDUAL_ADD),
            [lhs, rhs],
            attrs or {
                "attr.broadcast_rule": "none",
                "attr.shape_check": "exact",
                "attr.residual_path": "true",
            },
        )

    def common_linear(
        self,
        value: ValueHandle,
        weight: ValueHandle,
        bias: ValueHandle,
        attrs: Optional[Mapping[str, str]] = None,
    ) -> OperatorHandle:
        return self.operator(
            common.LINEAR,
            operator_version(common.LINEAR),
            [value, weight, bias],
            attrs or {
                "attr.has_bias": "true",
                "attr.transpose_input": "false",
                "attr.transpose_weight": "true",
                "attr.weight_layout": "OI",
            },
        )

    def common_relu(
        self,
        value: ValueHandle,
        attrs: Optional[Mapping[str, str]] = None,
    ) -> OperatorHandle:
        return self.operator(
            common.RELU,
            operator_version(common.RELU),
            [value],
            attrs or {},
        )

    def common_flatten(
        self,
        value: ValueHandle,
        attrs: Optional[Mapping[str, str]] = None,
    ) -> OperatorHandle:
        return self.operator(
            common.FLATTEN,
            operator_version(common.FLATTEN),
            [value],
            attrs or {
                "attr.start_dim": "1",
                "attr.end_dim": "-1",
            },
        )

    def common_output_logits(
        self,
        value: ValueHandle,
        attrs: Optional[Mapping[str, str]] = None,
    ) -> OperatorHandle:
        return self.operator(
            common.OUTPUT_LOGITS,
            operator_version(common.OUTPUT_LOGITS),
            [value],
            attrs or {"attr.semantic": "logits"},
        )

    def cnn_max_pool2d(
        self,
        value: ValueHandle,
        attrs: Optional[Mapping[str, str]] = None,
    ) -> OperatorHandle:
        return self.operator(
            cnn.MAX_POOL2D,
            operator_version(cnn.MAX_POOL2D),
            [value],
            attrs or {
                "attr.kernel_shape": "3,3",
                "attr.stride": "2,2",
                "attr.padding": "1,1",
                "attr.dilation": "1,1",
                "attr.ceil_mode": "false",
            },
        )

    def cnn_global_avg_pool2d(
        self,
        value: ValueHandle,
        attrs: Optional[Mapping[str, str]] = None,
    ) -> OperatorHandle:
        return self.operator(
            cnn.GLOBAL_AVG_POOL2D,
            operator_version(cnn.GLOBAL_AVG_POOL2D),
            [value],
            attrs or {
                "attr.output_size": "1,1",
                "attr.reduction_axes": "spatial",
            },
        )

    def cnn_conv2d(
        self,
        value: ValueHandle,
        weight: ValueHandle,
        bias: ValueHandle,
        attrs: Optional[Mapping[str, str]] = None,
    ) -> OperatorHandle:
        return self.operator(
            cnn.CONV2D,
            operator_version(cnn.CONV2D),
            [value, weight, bias],
            attrs or {
                "attr.kernel_shape": "3,3",
                "attr.stride": "1,1",
                "attr.padding": "0,0",
                "attr.dilation": "1,1",
                "attr.groups": "1",
                "attr.input_layout": "NCHW",
                "attr.weight_layout": "OIHW",
                "attr.output_layout": "NCHW",
            },
        )

    def cnn_batch_norm_infer(
        self,
        value: ValueHandle,
        scale: ValueHandle,
        bias: ValueHandle,
        running_mean: ValueHandle,
        running_var: ValueHandle,
        attrs: Optional[Mapping[str, str]] = None,
    ) -> OperatorHandle:
        return self.operator(
            cnn.BATCH_NORM_INFER,
            operator_version(cnn.BATCH_NORM_INFER),
            [value, scale, bias, running_mean, running_var],
            attrs or {
                "attr.epsilon": "1e-05",
                "attr.training": "false",
                "attr.input_layout": "NCHW",
                "attr.channel_axis": "1",
            },
        )


def load_builder(name: str) -> WhirlBuilder:
    return WhirlBuilder(load_backend(name))
