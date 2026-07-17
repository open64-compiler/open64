"""Mock backend with the same shape as the future native _whirl module."""

from __future__ import annotations

from itertools import count
from pathlib import Path
from typing import Dict, Mapping, Sequence


_handle_counter = count(1)
_objects: Dict[int, Mapping[str, object]] = {}
_canonical_types: Dict[tuple[tuple[str, str], ...], int] = {}
_active_program_unit = 0


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


def intern_tensor_type(name: str, descriptor: Mapping[str, object]) -> int:
    canonical = {
        str(key): str(value)
        for key, value in descriptor.items()
        if key not in {"runtime_state", "lineage"} and value is not None
    }
    if not canonical.get("dtype") or int(canonical.get("rank", -1)) < 0:
        raise RuntimeError("failed to intern tensor type")
    key = tuple(sorted(canonical.items()))
    existing = _canonical_types.get(key)
    if existing is not None:
        return existing
    handle = _new_handle({
        "name": name,
        "descriptor": canonical,
        **canonical,
        "kind": "tensor_type",
    })
    _canonical_types[key] = handle
    return handle


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


def create_operator_with_result(
    opcode_name: str,
    version: int,
    kids: Sequence[int],
    attrs: Mapping[str, str],
    result_name: str,
    result_type: int,
) -> int:
    handle = create_operator(opcode_name, version, kids, attrs)
    record = dict(_objects[handle])
    record["name"] = result_name
    record["result_type"] = result_type
    _objects[handle] = record
    return handle


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


def attach_value_metadata(value: int, metadata: Mapping[str, str]) -> bool:
    if value not in _objects:
        raise RuntimeError("failed to attach value metadata")
    record = dict(_objects[value])
    record["metadata"] = dict(metadata)
    _objects[value] = record
    return True


def attach_value_lineage(value: int, lineage: str) -> bool:
    if value not in _objects or not lineage:
        raise RuntimeError("failed to attach value lineage")
    record = dict(_objects[value])
    record["lineage"] = lineage
    _objects[value] = record
    return True


def get_value_type(value: int) -> int:
    return int(_objects[value].get("result_type", _objects[value].get("tensor_type", 0)))


def get_value_result_symbol(value: int) -> int:
    if value not in _objects:
        raise RuntimeError("failed to get value result symbol")
    record = dict(_objects[value])
    symbol = int(record.get("result_symbol", 0))
    if symbol == 0:
        symbol = _new_handle({"kind": "symbol", "name": record.get("name", "")})
        record["result_symbol"] = symbol
        _objects[value] = record
    return symbol


def begin_program() -> bool:
    global _active_program_unit
    _objects.clear()
    _canonical_types.clear()
    _active_program_unit = 0
    return True


def abort_program() -> None:
    global _active_program_unit
    _objects.clear()
    _canonical_types.clear()
    _active_program_unit = 0


def create_minimal_program_unit(name: str) -> int:
    global _active_program_unit
    if not name:
        raise RuntimeError("failed to create minimal program unit")

    for handle, record in _objects.items():
        if record.get("kind") == "program_unit" and record.get("name") == name:
            _active_program_unit = handle
            return handle

    handle = _new_handle(
        {
            "kind": "program_unit",
            "name": name,
            "body_markers": [],
        }
    )
    _active_program_unit = handle
    return handle


def select_program_unit(program_unit: int) -> bool:
    global _active_program_unit
    if program_unit not in _objects:
        raise RuntimeError("failed to select program unit")
    if _objects[program_unit].get("kind") != "program_unit":
        raise RuntimeError("failed to select program unit")
    _active_program_unit = program_unit
    return True


def set_pu_source_identity(
    program_unit: int,
    canonical_definition_name: str,
    defining_module: str,
    defining_file: str,
    defining_line: int,
    flags: int,
) -> bool:
    select_program_unit(program_unit)
    if (
        not canonical_definition_name or
        not defining_file or
        defining_line <= 0
    ):
        raise RuntimeError("failed to set program unit source identity")
    record = dict(_objects[program_unit])
    if "source_identity" in record:
        raise RuntimeError("failed to set program unit source identity")
    record["source_identity"] = {
        "canonical_definition_name": canonical_definition_name,
        "defining_module": defining_module,
        "defining_file": defining_file,
        "defining_line": defining_line,
        "flags": flags,
    }
    _objects[program_unit] = record
    return True


def _source_position(
    file_id: int,
    line: int,
    column: int,
    statement_begin: bool,
    basic_block_begin: bool,
) -> tuple[int, int, int, bool, bool]:
    return (file_id, line, column, statement_begin, basic_block_begin)


def declare_pu_formal(
    program_unit: int,
    name: str,
    ordinal: int,
    tensor_type: int,
    file_id: int,
    line: int,
    column: int,
    statement_begin: bool,
    basic_block_begin: bool,
) -> int:
    select_program_unit(program_unit)
    if not name or tensor_type not in _objects:
        raise RuntimeError("failed to declare program unit formal")
    return _new_handle({
        "kind": "pu_formal",
        "program_unit": program_unit,
        "name": name,
        "ordinal": ordinal,
        "tensor_type": tensor_type,
        "source_position": _source_position(
            file_id, line, column, statement_begin, basic_block_begin
        ),
    })


def declare_pu_result(
    program_unit: int,
    name: str,
    ordinal: int,
    tensor_type: int,
    role: int,
    file_id: int,
    line: int,
    column: int,
    statement_begin: bool,
    basic_block_begin: bool,
) -> int:
    select_program_unit(program_unit)
    if not name or tensor_type not in _objects or role not in (1, 2):
        raise RuntimeError("failed to declare program unit result")
    return _new_handle({
        "kind": "pu_result",
        "program_unit": program_unit,
        "name": name,
        "ordinal": ordinal,
        "tensor_type": tensor_type,
        "result_type": tensor_type,
        "role": role,
        "source_position": _source_position(
            file_id, line, column, statement_begin, basic_block_begin
        ),
    })


def return_pu_values(program_unit: int, values: Sequence[int]) -> bool:
    select_program_unit(program_unit)
    for value in values:
        if value not in _objects:
            raise RuntimeError("failed to return program unit values")
    record = dict(_objects[program_unit])
    record["return_values"] = list(values)
    _objects[program_unit] = record
    return True


def create_pu_call(
    caller: int,
    callee: int,
    arguments: Sequence[int],
    result_names: Sequence[str],
    canonical_class_name: str,
    instance_path: str,
    context_identity: str,
    call_ordinal: int,
    file_id: int,
    line: int,
    column: int,
    statement_begin: bool,
    basic_block_begin: bool,
) -> int:
    select_program_unit(caller)
    if callee not in _objects or not result_names:
        raise RuntimeError("failed to create program unit call")
    for argument in arguments:
        if argument not in _objects:
            raise RuntimeError("failed to create program unit call")
        if _objects[argument].get("program_unit") not in {caller, None}:
            raise RuntimeError("failed to create program unit call")
    result_handles = [
        _new_handle({
            "kind": "pu_call_result",
            "program_unit": caller,
            "name": result_name,
            "result_type": _objects[arguments[0]].get("tensor_type", 0)
            if arguments else 0,
        })
        for result_name in result_names
    ]
    return _new_handle({
        "kind": "pu_call",
        "caller": caller,
        "callee": callee,
        "arguments": list(arguments),
        "result_names": list(result_names),
        "results": result_handles,
        "canonical_class_name": canonical_class_name,
        "instance_path": instance_path,
        "context_identity": context_identity,
        "call_ordinal": call_ordinal,
        "source_position": _source_position(
            file_id, line, column, statement_begin, basic_block_begin
        ),
    })


def get_pu_call_result(call: int, ordinal: int) -> int:
    if call not in _objects or _objects[call].get("kind") != "pu_call":
        raise RuntimeError("failed to get program unit call result")
    results = _objects[call].get("results", ())
    if not isinstance(results, Sequence) or ordinal >= len(results):
        raise RuntimeError("failed to get program unit call result")
    return int(results[ordinal])


def register_source_file(program_unit: int, path: str) -> int:
    if program_unit not in _objects or not path:
        raise RuntimeError("failed to register source file")
    record = dict(_objects[program_unit])
    files = list(record.get("source_files", ()))
    if path not in files:
        files.append(path)
    record["source_files"] = files
    _objects[program_unit] = record
    return files.index(path) + 1


def set_value_source_position(
    value: int,
    file_id: int,
    line: int,
    column: int,
    statement_begin: bool,
    basic_block_begin: bool,
) -> bool:
    if value not in _objects or file_id <= 0 or line < 0:
        raise RuntimeError("failed to set value source position")
    record = dict(_objects[value])
    record["source_position"] = (
        file_id, line, column, statement_begin, basic_block_begin
    )
    _objects[value] = record
    return True


def create_region(
    program_unit: int,
    parent_region: int,
    contract_name: str,
    contract_version: int,
) -> int:
    if program_unit not in _objects or not contract_name or contract_version <= 0:
        raise RuntimeError("failed to create region")
    if parent_region and parent_region not in _objects:
        raise RuntimeError("failed to create region")
    return _new_handle({
        "kind": "region",
        "program_unit": program_unit,
        "parent_region": parent_region,
        "contract_name": contract_name,
        "contract_version": contract_version,
        "values": [],
        "interfaces": [],
    })


def append_region_value(region: int, value: int) -> bool:
    if region not in _objects or value not in _objects:
        raise RuntimeError("failed to append region value")
    record = dict(_objects[region])
    values = list(record.get("values", ()))
    values.append(value)
    record["values"] = values
    _objects[region] = record
    return True


def append_program_unit_region(program_unit: int, region: int) -> bool:
    if program_unit not in _objects or region not in _objects:
        raise RuntimeError("failed to append program unit region")
    record = dict(_objects[program_unit])
    regions = list(record.get("regions", ()))
    regions.append(region)
    record["regions"] = regions
    _objects[program_unit] = record
    return True


def append_child_region(parent_region: int, child_region: int) -> bool:
    if parent_region not in _objects or child_region not in _objects:
        raise RuntimeError("failed to append child region")
    parent = dict(_objects[parent_region])
    children = list(parent.get("children", ()))
    children.append(child_region)
    parent["children"] = children
    _objects[parent_region] = parent
    child = dict(_objects[child_region])
    child["parent_region"] = parent_region
    _objects[child_region] = child
    return True


def declare_region_value(
    region: int, value: int, roles: int, ordinal: int, flags: int
) -> bool:
    if region not in _objects or value not in _objects or roles == 0:
        raise RuntimeError("failed to declare region value")
    record = dict(_objects[region])
    interfaces = list(record.get("interfaces", ()))
    interfaces.append((value, roles, ordinal, flags))
    record["interfaces"] = interfaces
    _objects[region] = record
    return True


def declare_state_object(
    program_unit: int, name: str, kind: int, flags: int
) -> int:
    if program_unit not in _objects or not name or kind < 1 or kind > 5:
        raise RuntimeError("failed to declare state object")
    if flags & ~0x1:
        raise RuntimeError("failed to declare state object")
    if any(
        record.get("kind") == "state_object"
        and record.get("program_unit") == program_unit
        and record.get("name") == name
        for record in _objects.values()
    ):
        raise RuntimeError("failed to declare state object")
    return _new_handle({
        "kind": "state_object",
        "program_unit": program_unit,
        "name": name,
        "state_kind": kind,
        "flags": flags,
    })


def add_state_effect(value: int, state: int, effect_kind: int) -> bool:
    if value not in _objects or state not in _objects:
        raise RuntimeError("failed to add state effect")
    if _objects[state].get("kind") != "state_object":
        raise RuntimeError("failed to add state effect")
    if effect_kind not in (1, 2):
        raise RuntimeError("failed to add state effect")
    record = dict(_objects[value])
    effects = list(record.get("state_effects", ()))
    effects.append((state, effect_kind))
    record["state_effects"] = effects
    _objects[value] = record
    return True


def declare_region_state(
    region: int,
    state: int,
    effect_kind: int,
    ordinal: int,
    flags: int,
) -> bool:
    if region not in _objects or state not in _objects:
        raise RuntimeError("failed to declare region state")
    if _objects[region].get("kind") != "region":
        raise RuntimeError("failed to declare region state")
    if _objects[state].get("kind") != "state_object":
        raise RuntimeError("failed to declare region state")
    if effect_kind not in (1, 2) or flags & ~(0x2 | 0x10):
        raise RuntimeError("failed to declare region state")
    record = dict(_objects[region])
    states = list(record.get("states", ()))
    states.append((state, effect_kind, ordinal, flags))
    record["states"] = states
    _objects[region] = record
    return True


def set_region_source_position(
    region: int,
    file_id: int,
    line: int,
    column: int,
    statement_begin: bool,
    basic_block_begin: bool,
) -> bool:
    return set_value_source_position(
        region, file_id, line, column, statement_begin, basic_block_begin
    )


def set_region_metadata(region: int, key: str, value: str) -> bool:
    if region not in _objects or not key:
        raise RuntimeError("failed to set region metadata")
    record = dict(_objects[region])
    metadata = dict(record.get("metadata", {}))
    metadata[key] = value
    record["metadata"] = metadata
    _objects[region] = record
    return True


def verify_program() -> Mapping[str, object]:
    program_units = [
        record for record in _objects.values()
        if record.get("kind") == "program_unit"
    ]
    valid = bool(program_units)
    values = [
        record for record in _objects.values()
        if record.get("kind") in {
            "operator",
            "tensor_constant",
            "model_input",
            "external_tensor_constant",
        }
    ]
    return {
        "valid": valid,
        "native_node_count": len(values),
        "result_symbol_count": len(values),
        "error_count": 0 if valid else 1,
        "diagnostic": "" if valid else "no program unit",
    }


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
        f"pu_mode={module_manifest.get('pu_mode', '')}",
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
                metadata = operator.get("metadata", {})
                if not isinstance(metadata, Mapping):
                    metadata = {}
                if metadata:
                    metadata_text = ",".join(
                        f"{name}={value}"
                        for name, value in sorted(metadata.items())
                    )
                    lines.append(
                        f"graph_operator_metadata.{index}={metadata_text}"
                    )

    output_path.write_text("\n".join(lines) + "\n", encoding="utf-8")
    return True
