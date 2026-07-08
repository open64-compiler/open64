"""Python-side facade for native WHIRL builder handles."""

from __future__ import annotations

from dataclasses import dataclass
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


def load_builder(name: str) -> WhirlBuilder:
    return WhirlBuilder(load_backend(name))
