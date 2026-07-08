"""Mock backend with the same shape as the future native _whirl module."""

from __future__ import annotations

from itertools import count
from pathlib import Path
from typing import Dict, Mapping, Sequence


_handle_counter = count(1)
_objects: Dict[int, Mapping[str, object]] = {}


def backend_name() -> str:
    return "mock"


def _new_handle(record: Mapping[str, object]) -> int:
    handle = next(_handle_counter)
    _objects[handle] = dict(record)
    return handle


def create_tensor_type(
    name: str,
    dtype: str,
    rank: int,
    logical_shape: str,
) -> int:
    if not name:
        raise RuntimeError("failed to create tensor type")
    if not dtype:
        raise RuntimeError("failed to create tensor type")
    if rank < 0:
        raise RuntimeError("failed to create tensor type")

    return _new_handle(
        {
            "kind": "tensor_type",
            "name": name,
            "dtype": dtype,
            "rank": rank,
            "logical_shape": logical_shape,
        }
    )


def attach_tensor_descriptor(
    tensor_type: int,
    descriptor: Mapping[str, object],
) -> bool:
    if tensor_type not in _objects:
        raise RuntimeError("failed to attach tensor descriptor")
    if _objects[tensor_type].get("kind") != "tensor_type":
        raise RuntimeError("failed to attach tensor descriptor")
    if not descriptor.get("dtype"):
        raise RuntimeError("failed to attach tensor descriptor")
    if int(descriptor.get("rank", -1)) < 0:
        raise RuntimeError("failed to attach tensor descriptor")

    record = dict(_objects[tensor_type])
    record["descriptor"] = dict(descriptor)
    _objects[tensor_type] = record
    return True


def create_tensor_constant(
    name: str,
    dtype: str,
    rank: int,
    logical_shape: str,
    value_kind: str,
    value: str,
) -> int:
    if not name:
        raise RuntimeError("failed to create tensor constant")
    if not dtype:
        raise RuntimeError("failed to create tensor constant")
    if rank < 0:
        raise RuntimeError("failed to create tensor constant")

    return _new_handle(
        {
            "kind": "tensor_constant",
            "name": name,
            "dtype": dtype,
            "rank": rank,
            "logical_shape": logical_shape,
            "value_kind": value_kind,
            "value": value,
        }
    )


def create_operator(
    opcode_name: str,
    version: int,
    kids: Sequence[int],
    attrs: Mapping[str, str],
) -> int:
    if not opcode_name:
        raise RuntimeError("failed to create operator")
    if version <= 0:
        raise RuntimeError("failed to create operator")
    for kid in kids:
        if kid not in _objects:
            raise RuntimeError("failed to create operator")

    return _new_handle(
        {
            "kind": "operator",
            "opcode_name": opcode_name,
            "version": version,
            "kids": list(kids),
            "attrs": dict(attrs),
        }
    )


def create_symbol(name: str, tensor_type: int) -> int:
    if not name:
        raise RuntimeError("failed to create symbol")
    if tensor_type not in _objects:
        raise RuntimeError("failed to create symbol")
    if _objects[tensor_type].get("kind") != "tensor_type":
        raise RuntimeError("failed to create symbol")

    return _new_handle(
        {
            "kind": "symbol",
            "name": name,
            "tensor_type": tensor_type,
        }
    )


def attach_symbol_metadata(
    symbol: int,
    metadata: Mapping[str, str],
) -> bool:
    if symbol not in _objects:
        raise RuntimeError("failed to attach symbol metadata")
    if _objects[symbol].get("kind") != "symbol":
        raise RuntimeError("failed to attach symbol metadata")

    record = dict(_objects[symbol])
    record["metadata"] = dict(metadata)
    _objects[symbol] = record
    return True


def finalize_mapped_image(path: str, module_manifest: Mapping[str, object]) -> bool:
    output_path = Path(path)
    if not str(output_path):
        return False

    lines = [
        "# open64_dsc mock WHIRL artifact",
        "format=mock",
        f"model_name={module_manifest.get('model_name', '')}",
        f"entry={module_manifest.get('entry', '')}",
        f"input_count={module_manifest.get('input_count', 0)}",
    ]

    operators = module_manifest.get("operators", ())
    if isinstance(operators, Sequence) and not isinstance(operators, str):
        for index, operator in enumerate(operators):
            lines.append(f"operator.{index}={operator}")

    tensor_types = module_manifest.get("tensor_types", ())
    if isinstance(tensor_types, Sequence) and not isinstance(tensor_types, str):
        for index, tensor_type in enumerate(tensor_types):
            if isinstance(tensor_type, Mapping):
                descriptor = tensor_type.get("descriptor", {})
                if not isinstance(descriptor, Mapping):
                    descriptor = {}
                lines.append(
                    f"tensor_type.{index}="
                    f"{tensor_type.get('name', '')}:"
                    f"{tensor_type.get('dtype', '')}:"
                    f"{tensor_type.get('logical_shape', '')}"
                )
                lines.append(
                    f"tensor_descriptor.{index}="
                    f"{descriptor.get('dtype', '')}:"
                    f"{descriptor.get('rank', '')}:"
                    f"{descriptor.get('logical_shape', '')}:"
                    f"{descriptor.get('lineage', '')}"
                )

    values = module_manifest.get("values", ())
    if isinstance(values, Sequence) and not isinstance(values, str):
        for index, value in enumerate(values):
            if isinstance(value, Mapping):
                metadata = value.get("metadata", {})
                if not isinstance(metadata, Mapping):
                    metadata = {}
                lines.append(
                    f"value.{index}="
                    f"{value.get('name', '')}:"
                    f"{value.get('type_name', '')}:"
                    f"{value.get('value_kind', '')}"
                )
                lines.append(
                    f"value_metadata.{index}="
                    f"{metadata.get('source_layer_name', '')}:"
                    f"{metadata.get('lowering_hint', '')}"
                )

    graph_operators = module_manifest.get("graph_operators", ())
    if (isinstance(graph_operators, Sequence) and
            not isinstance(graph_operators, str)):
        for index, operator in enumerate(graph_operators):
            if isinstance(operator, Mapping):
                kids = operator.get("kids", ())
                if not isinstance(kids, Sequence) or isinstance(kids, str):
                    kids = ()
                lines.append(
                    f"graph_operator.{index}="
                    f"{operator.get('name', '')}:"
                    f"{','.join(str(kid) for kid in kids)}"
                )

    output_path.write_text("\n".join(lines) + "\n", encoding="utf-8")
    return True
