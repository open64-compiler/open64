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


def create_model_input(name: str, tensor_type: int, input_ordinal: int) -> int:
    if not name:
        raise RuntimeError("failed to create model input")
    if tensor_type not in _objects:
        raise RuntimeError("failed to create model input")
    if _objects[tensor_type].get("kind") != "tensor_type":
        raise RuntimeError("failed to create model input")
    if input_ordinal < 0:
        raise RuntimeError("failed to create model input")

    return _new_handle(
        {
            "kind": "model_input",
            "name": name,
            "tensor_type": tensor_type,
            "input_ordinal": input_ordinal,
        }
    )


def create_external_tensor_constant(
    name: str,
    tensor_type: int,
    reference: Mapping[str, object],
) -> int:
    if not name:
        raise RuntimeError("failed to create external tensor constant")
    if tensor_type not in _objects:
        raise RuntimeError("failed to create external tensor constant")
    if _objects[tensor_type].get("kind") != "tensor_type":
        raise RuntimeError("failed to create external tensor constant")
    for field in (
        "storage_format",
        "side_file",
        "tensor_key",
        "byte_offset",
        "byte_length",
    ):
        if field not in reference:
            raise RuntimeError("failed to create external tensor constant")
    if int(reference.get("byte_offset", -1)) < 0:
        raise RuntimeError("failed to create external tensor constant")
    if int(reference.get("byte_length", 0)) <= 0:
        raise RuntimeError("failed to create external tensor constant")

    tensor_type_record = _objects[tensor_type]
    return _new_handle(
        {
            "kind": "external_tensor_constant",
            "name": name,
            "tensor_type": tensor_type,
            "dtype": tensor_type_record.get("dtype", ""),
            "rank": tensor_type_record.get("rank", 0),
            "logical_shape": tensor_type_record.get("logical_shape", ""),
            "reference": dict(reference),
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


def create_minimal_program_unit(name: str) -> int:
    if not name:
        raise RuntimeError("failed to create minimal program unit")

    return _new_handle(
        {
            "kind": "program_unit",
            "name": name,
            "body_markers": [],
        }
    )


def _marker_name(marker: int) -> str:
    record = _objects[marker]
    if record.get("kind") == "operator":
        return str(record.get("opcode_name", ""))
    return str(record.get("name", ""))


def _marker_annotation(marker: int) -> Mapping[str, object]:
    record = _objects[marker]
    if record.get("kind") == "operator":
        kid_names = []
        kids = record.get("kids", ())
        if isinstance(kids, Sequence) and not isinstance(kids, str):
            for kid in kids:
                kid_names.append(_marker_name(int(kid)))
        attrs = record.get("attrs", {})
        if not isinstance(attrs, Mapping):
            attrs = {}
        payload_fields = [
            f"kid{index}={kid_name}"
            for index, kid_name in enumerate(kid_names)
        ]
        payload_fields.extend(
            f"{name}={value}"
            for name, value in attrs.items()
        )
        return {
            "opcode": str(record.get("opcode_name", "")),
            "version": int(record.get("version", 0)),
            "payload": ";".join(payload_fields),
        }

    if record.get("kind") == "model_input":
        return {
            "opcode": "common.model_input",
            "version": 2,
            "payload": (
                f"name={record.get('name', '')};"
                f"attr.input_ordinal={record.get('input_ordinal', '')}"
            ),
        }

    if record.get("kind") == "external_tensor_constant":
        reference = record.get("reference", {})
        if not isinstance(reference, Mapping):
            reference = {}
        return {
            "opcode": "common.tensor_const",
            "version": 1,
            "payload": (
                f"name={record.get('name', '')};"
                f"dtype={record.get('dtype', '')};"
                f"rank={record.get('rank', '')};"
                f"shape={record.get('logical_shape', '')};"
                "value_kind=external_data;"
                f"storage_format={reference.get('storage_format', '')};"
                f"side_file={reference.get('side_file', '')};"
                f"tensor_key={reference.get('tensor_key', '')};"
                f"byte_offset={reference.get('byte_offset', '')};"
                f"byte_length={reference.get('byte_length', '')};"
                f"checksum={reference.get('checksum', '')}"
            ),
        }

    return {
        "opcode": "common.tensor_const",
        "version": 1,
        "payload": (
            f"name={record.get('name', '')};"
            f"dtype={record.get('dtype', '')};"
            f"rank={record.get('rank', '')};"
            f"shape={record.get('logical_shape', '')};"
            f"value_kind={record.get('value_kind', '')};"
            f"value={record.get('value', '')}"
        ),
    }


def append_program_unit_value(program_unit: int, value: int) -> bool:
    if program_unit not in _objects:
        raise RuntimeError("failed to append program unit value")
    if value not in _objects:
        raise RuntimeError("failed to append program unit value")
    if _objects[program_unit].get("kind") != "program_unit":
        raise RuntimeError("failed to append program unit value")
    if _objects[value].get("kind") not in {
        "operator",
        "tensor_constant",
        "model_input",
        "external_tensor_constant",
    }:
        raise RuntimeError("failed to append program unit value")

    record = dict(_objects[program_unit])
    body_markers = list(record.get("body_markers", ()))
    body_marker_annotations = list(record.get("body_marker_annotations", ()))
    body_markers.append(_marker_name(value))
    body_marker_annotations.append(dict(_marker_annotation(value)))
    record["body_markers"] = body_markers
    record["body_marker_annotations"] = body_marker_annotations
    _objects[program_unit] = record
    return True


def append_program_unit_marker(program_unit: int, marker: int) -> bool:
    return append_program_unit_value(program_unit, marker)


def inspect_program_unit_values(
    program_unit: int,
) -> Sequence[Mapping[str, object]]:
    if program_unit not in _objects:
        raise RuntimeError("failed to inspect program unit values")
    if _objects[program_unit].get("kind") != "program_unit":
        raise RuntimeError("failed to inspect program unit values")

    annotations = _objects[program_unit].get("body_marker_annotations", ())
    if not isinstance(annotations, Sequence) or isinstance(annotations, str):
        return []
    return [
        dict(annotation)
        for annotation in annotations
        if isinstance(annotation, Mapping)
    ]


def inspect_program_unit_markers(
    program_unit: int,
) -> Sequence[Mapping[str, object]]:
    return inspect_program_unit_values(program_unit)


def finalize_mapped_image(path: str, module_manifest: Mapping[str, object]) -> bool:
    output_path = Path(path)
    if not str(output_path):
        return False

    entry_function = module_manifest.get("entry_function", {})
    if not isinstance(entry_function, Mapping):
        entry_function = {}

    lines = [
        "# open64_dsc mock WHIRL artifact",
        "format=mock",
        f"model_name={module_manifest.get('model_name', '')}",
        f"entry={module_manifest.get('entry', '')}",
        f"entry_function={entry_function.get('name', '')}",
        f"graph_source={module_manifest.get('graph_source', '')}",
        f"input_count={module_manifest.get('input_count', 0)}",
    ]

    body_markers = entry_function.get("body_markers", ())
    if isinstance(body_markers, Sequence) and not isinstance(body_markers, str):
        for index, marker in enumerate(body_markers):
            lines.append(f"entry_body_marker.{index}={marker}")

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

    tensor_payloads = module_manifest.get("tensor_payloads", ())
    if (isinstance(tensor_payloads, Sequence) and
            not isinstance(tensor_payloads, str)):
        for index, payload in enumerate(tensor_payloads):
            if isinstance(payload, Mapping):
                lines.append(
                    f"tensor_payload.{index}="
                    f"{payload.get('storage_file', '')}:"
                    f"{payload.get('tensor_key', '')}:"
                    f"{payload.get('dtype', '')}:"
                    f"{payload.get('logical_shape', '')}:"
                    f"{payload.get('byte_offset', '')}:"
                    f"{payload.get('byte_length', '')}:"
                    f"{payload.get('checksum', '')}"
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
                attrs = operator.get("attrs", {})
                if not isinstance(attrs, Mapping):
                    attrs = {}
                attr_text = ",".join(
                    f"{name}={value}"
                    for name, value in sorted(attrs.items())
                )
                lines.append(f"graph_operator_attrs.{index}={attr_text}")

    output_path.write_text("\n".join(lines) + "\n", encoding="utf-8")
    return True
