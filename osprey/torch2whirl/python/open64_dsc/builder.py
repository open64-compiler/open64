"""Python-side facade for native WHIRL builder handles."""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import Mapping, Optional, Sequence

from .backend import WhirlBackend, load_backend


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

    def backend_name(self) -> str:
        return self._backend.backend_name()

    def tensor_type(
        self,
        name: str,
        dtype: str,
        rank: int,
        logical_shape: str,
    ) -> TensorTypeHandle:
        return TensorTypeHandle(
            self._backend.create_tensor_type(
                name,
                dtype,
                rank,
                logical_shape,
            )
        )

    def attach_tensor_descriptor(
        self,
        tensor_type: TensorTypeHandle,
        descriptor: Mapping[str, object],
    ) -> None:
        if not self._backend.attach_tensor_descriptor(
            tensor_type.value,
            descriptor,
        ):
            raise RuntimeError("failed to attach tensor descriptor")

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

    def append_program_unit_marker(
        self,
        program_unit: ProgramUnitHandle,
        marker: ValueHandle,
    ) -> None:
        if not self._backend.append_program_unit_marker(
            program_unit.value,
            marker.value,
        ):
            raise RuntimeError("failed to append program unit marker")

    def inspect_program_unit_markers(
        self,
        program_unit: ProgramUnitHandle,
    ) -> Sequence[Mapping[str, object]]:
        return self._backend.inspect_program_unit_markers(program_unit.value)

    def tensor_constant(
        self,
        name: str,
        dtype: str,
        rank: int,
        logical_shape: str,
        value_kind: str,
        value: str,
    ) -> ValueHandle:
        return ValueHandle(
            self._backend.create_tensor_constant(
                name,
                dtype,
                rank,
                logical_shape,
                value_kind,
                value,
            )
        )

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

        tensor_type = self.tensor_type(
            f"{name}_type",
            dtype,
            rank,
            logical_shape,
        )
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
            "runtime_state": "static",
            "lineage": name,
        }
        self.attach_tensor_descriptor(tensor_type, descriptor)
        symbol = self.symbol(name, tensor_type)
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
        self.attach_symbol_metadata(symbol, metadata)

        value = self.tensor_constant(
            name,
            dtype,
            rank,
            logical_shape,
            "external_data",
            self._external_tensor_uri(
                storage_format,
                side_file,
                tensor_key,
                byte_offset,
                byte_length,
                checksum,
            ),
        )
        return TensorConstantHandle(
            value.value,
            tensor_type.value,
            symbol.value,
            metadata,
            descriptor,
        )

    def _external_tensor_uri(
        self,
        storage_format: str,
        side_file: str,
        tensor_key: str,
        byte_offset: int,
        byte_length: int,
        checksum: str,
    ) -> str:
        uri = (
            f"{storage_format}://{side_file}#{tensor_key}"
            f"?offset={byte_offset}&length={byte_length}"
        )
        if checksum:
            uri += f"&checksum={checksum}"
        return uri

    def operator(
        self,
        opcode_name: str,
        version: int,
        kids: Sequence[ValueHandle],
        attrs: Mapping[str, str],
    ) -> OperatorHandle:
        return OperatorHandle(
            self._backend.create_operator(
                opcode_name,
                version,
                [kid.value for kid in kids],
                attrs,
            )
        )

    def common_add(
        self,
        lhs: ValueHandle,
        rhs: ValueHandle,
        attrs: Optional[Mapping[str, str]] = None,
    ) -> OperatorHandle:
        return self.operator(
            "common.add",
            1,
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
            "common.matmul",
            1,
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
            "common.residual_add",
            1,
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
            "common.linear",
            1,
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
            "common.relu",
            1,
            [value],
            attrs or {},
        )

    def common_flatten(
        self,
        value: ValueHandle,
        attrs: Optional[Mapping[str, str]] = None,
    ) -> OperatorHandle:
        return self.operator(
            "common.flatten",
            1,
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
            "common.output_logits",
            1,
            [value],
            attrs or {},
        )

    def cnn_max_pool2d(
        self,
        value: ValueHandle,
        attrs: Optional[Mapping[str, str]] = None,
    ) -> OperatorHandle:
        return self.operator(
            "cnn.max_pool2d",
            1,
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
            "cnn.global_avg_pool2d",
            1,
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
            "cnn.conv2d",
            1,
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
            "cnn.batch_norm_infer",
            1,
            [value, scale, bias, running_mean, running_var],
            attrs or {
                "attr.epsilon": "1e-05",
                "attr.momentum": "0.1",
                "attr.training": "false",
                "attr.input_layout": "NCHW",
                "attr.channel_axis": "1",
            },
        )


def load_builder(name: str) -> WhirlBuilder:
    return WhirlBuilder(load_backend(name))
