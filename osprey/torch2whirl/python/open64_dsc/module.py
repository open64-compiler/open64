"""In-memory representation returned by the Python export skeleton."""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import Dict, List, Mapping, Sequence

from .options import WhirlExportOptions
from .python_imports import PythonImportedCallable


@dataclass(frozen=True)
class WhirlProgramUnitRecord:
    name: str
    handle: int
    body_markers: Sequence[str] = field(default_factory=list)

    def to_manifest(self) -> Mapping[str, object]:
        return {
            "name": self.name,
            "handle": self.handle,
            "body_markers": list(self.body_markers),
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
class WhirlTensorPayloadRecord:
    storage_file: str
    tensor_key: str
    dtype: str
    logical_shape: str
    byte_offset: int
    byte_length: int
    checksum: str
    data: bytes = field(default=b"", repr=False)

    def to_manifest(self) -> Mapping[str, object]:
        return {
            "storage_file": self.storage_file,
            "tensor_key": self.tensor_key,
            "dtype": self.dtype,
            "logical_shape": self.logical_shape,
            "byte_offset": self.byte_offset,
            "byte_length": self.byte_length,
            "checksum": self.checksum,
        }


@dataclass(frozen=True)
class WhirlOperatorRecord:
    name: str
    handle: int
    kids: Sequence[str]
    attrs: Mapping[str, str]
    metadata: Mapping[str, str] = field(default_factory=dict)

    def to_manifest(self) -> Mapping[str, object]:
        return {
            "name": self.name,
            "handle": self.handle,
            "kids": list(self.kids),
            "attrs": dict(self.attrs),
            "metadata": dict(self.metadata),
        }


@dataclass(frozen=True)
class WhirlModule:
    options: WhirlExportOptions
    model_name: str
    input_count: int
    entry_function: WhirlProgramUnitRecord
    graph_source: str = "synthetic"
    operators: Sequence[str] = field(default_factory=list)
    tensor_types: Sequence[WhirlTensorTypeRecord] = field(default_factory=list)
    values: Sequence[WhirlValueRecord] = field(default_factory=list)
    tensor_payloads: Sequence[WhirlTensorPayloadRecord] = field(
        default_factory=list
    )
    graph_operators: Sequence[WhirlOperatorRecord] = field(default_factory=list)
    python_imports: Sequence[PythonImportedCallable] = field(
        default_factory=list
    )

    def to_manifest(self) -> Mapping[str, object]:
        return {
            "backend": self.options.backend,
            "entry": self.options.entry,
            "model_name": self.model_name,
            "input_count": self.input_count,
            "entry_function": self.entry_function.to_manifest(),
            "graph_source": self.graph_source,
            "operators": list(self.operators),
            "tensor_types": [
                tensor_type.to_manifest()
                for tensor_type in self.tensor_types
            ],
            "values": [value.to_manifest() for value in self.values],
            "tensor_payloads": [
                payload.to_manifest()
                for payload in self.tensor_payloads
            ],
            "graph_operators": [
                operator.to_manifest()
                for operator in self.graph_operators
            ],
            "python_imports": [
                imported.to_manifest()
                for imported in self.python_imports
            ],
        }

    def metadata(self) -> Dict[str, object]:
        return dict(self.to_manifest())
