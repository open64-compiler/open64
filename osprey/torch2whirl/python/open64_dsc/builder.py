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
class ValueHandle(OpaqueHandle):
    pass


@dataclass(frozen=True)
class OperatorHandle(ValueHandle):
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
