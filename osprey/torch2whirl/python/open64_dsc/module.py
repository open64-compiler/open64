"""In-memory representation returned by the Python export skeleton."""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import Dict, List, Mapping, Sequence

from .options import WhirlExportOptions


@dataclass(frozen=True)
class WhirlProgramUnitRecord:
    name: str
    handle: int

    def to_manifest(self) -> Mapping[str, object]:
        return {
            "name": self.name,
            "handle": self.handle,
        }


@dataclass(frozen=True)
class WhirlTensorTypeRecord:
    name: str
    handle: int
    dtype: str
    rank: int
    logical_shape: str
    descriptor: Mapping[str, object] = field(default_factory=dict)

    def to_manifest(self) -> Mapping[str, object]:
        return {
            "name": self.name,
            "handle": self.handle,
            "dtype": self.dtype,
            "rank": self.rank,
            "logical_shape": self.logical_shape,
            "descriptor": dict(self.descriptor),
        }


@dataclass(frozen=True)
class WhirlValueRecord:
    name: str
    handle: int
    type_name: str
    value_kind: str
    symbol_handle: int = 0
    metadata: Mapping[str, str] = field(default_factory=dict)

    def to_manifest(self) -> Mapping[str, object]:
        return {
            "name": self.name,
            "handle": self.handle,
            "type_name": self.type_name,
            "value_kind": self.value_kind,
            "symbol_handle": self.symbol_handle,
            "metadata": dict(self.metadata),
        }


@dataclass(frozen=True)
class WhirlOperatorRecord:
    name: str
    handle: int
    kids: Sequence[str]
    attrs: Mapping[str, str]

    def to_manifest(self) -> Mapping[str, object]:
        return {
            "name": self.name,
            "handle": self.handle,
            "kids": list(self.kids),
            "attrs": dict(self.attrs),
        }


@dataclass(frozen=True)
class WhirlModule:
    options: WhirlExportOptions
    model_name: str
    input_count: int
    entry_function: WhirlProgramUnitRecord
    operators: Sequence[str] = field(default_factory=list)
    tensor_types: Sequence[WhirlTensorTypeRecord] = field(default_factory=list)
    values: Sequence[WhirlValueRecord] = field(default_factory=list)
    graph_operators: Sequence[WhirlOperatorRecord] = field(default_factory=list)

    def to_manifest(self) -> Mapping[str, object]:
        return {
            "backend": self.options.backend,
            "entry": self.options.entry,
            "model_name": self.model_name,
            "input_count": self.input_count,
            "entry_function": self.entry_function.to_manifest(),
            "operators": list(self.operators),
            "tensor_types": [
                tensor_type.to_manifest()
                for tensor_type in self.tensor_types
            ],
            "values": [value.to_manifest() for value in self.values],
            "graph_operators": [
                operator.to_manifest()
                for operator in self.graph_operators
            ],
        }

    def metadata(self) -> Dict[str, object]:
        return dict(self.to_manifest())
