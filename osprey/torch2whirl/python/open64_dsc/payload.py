"""Side-file tensor payload writer for torch2whirl exports."""

from __future__ import annotations

import json
import os
from pathlib import Path
import struct
import tempfile
from typing import Dict, Iterable, List, Mapping, Sequence

from .module import WhirlModule, WhirlTensorPayloadRecord


_SAFETENSORS_DTYPE = {
    "bool": "BOOL",
    "float32": "F32",
    "float64": "F64",
    "int32": "I32",
    "int64": "I64",
    "uint8": "U8",
}


def write_external_payloads(module: WhirlModule, artifact_path: str) -> None:
    """Write deterministic SafeTensors-style side files for module payloads."""

    if not module.tensor_payloads:
        return

    output_dir = Path(artifact_path).parent
    for storage_file, payloads in _group_payloads(module.tensor_payloads).items():
        _write_safetensors_file(
            output_dir / storage_file,
            module.model_name,
            payloads,
        )


def _group_payloads(
    payloads: Iterable[WhirlTensorPayloadRecord],
) -> Mapping[str, Sequence[WhirlTensorPayloadRecord]]:
    grouped: Dict[str, List[WhirlTensorPayloadRecord]] = {}
    for payload in payloads:
        grouped.setdefault(payload.storage_file, []).append(payload)
    return {
        storage_file: sorted(items, key=lambda item: item.byte_offset)
        for storage_file, items in sorted(grouped.items())
    }


def _write_safetensors_file(
    path: Path,
    model_name: str,
    payloads: Sequence[WhirlTensorPayloadRecord],
) -> None:
    header: Dict[str, object] = {
        "__metadata__": {
            "format": "open64_dsc_external_tensor_payload",
            "producer": "torch2whirl",
            "model_name": model_name,
        }
    }
    data_segments = []
    expected_offset = 0
    for payload in payloads:
        if payload.byte_offset != expected_offset:
            raise ValueError(
                "external tensor payload offsets must be contiguous: "
                f"{payload.tensor_key} starts at {payload.byte_offset}, "
                f"expected {expected_offset}"
            )
        if payload.byte_length != len(payload.data):
            raise ValueError(
                "external tensor payload byte length mismatch: "
                f"{payload.tensor_key} metadata says {payload.byte_length}, "
                f"data has {len(payload.data)}"
            )
        header[payload.tensor_key] = {
            "dtype": _safetensors_dtype(payload.dtype),
            "shape": _parse_logical_shape(payload.logical_shape),
            "data_offsets": [
                payload.byte_offset,
                payload.byte_offset + payload.byte_length,
            ],
            "open64_sha256": payload.checksum,
        }
        data_segments.append(payload.data)
        expected_offset += payload.byte_length

    header_bytes = json.dumps(
        header,
        sort_keys=True,
        separators=(",", ":"),
    ).encode("utf-8")
    payload_bytes = (
        struct.pack("<Q", len(header_bytes)) +
        header_bytes +
        b"".join(data_segments)
    )
    temp_path = None
    try:
        with tempfile.NamedTemporaryFile(
            prefix=f".{path.name}.",
            suffix=".tmp",
            dir=str(path.parent),
            delete=False,
        ) as temp_file:
            temp_path = Path(temp_file.name)
            temp_file.write(payload_bytes)
        os.replace(temp_path, path)
        temp_path = None
    finally:
        if temp_path is not None:
            temp_path.unlink(missing_ok=True)


def _safetensors_dtype(dtype: str) -> str:
    try:
        return _SAFETENSORS_DTYPE[dtype]
    except KeyError as exc:
        raise ValueError(f"unsupported SafeTensors dtype: {dtype}") from exc


def _parse_logical_shape(logical_shape: str) -> Sequence[int]:
    if not logical_shape.startswith("[") or not logical_shape.endswith("]"):
        raise ValueError(f"invalid tensor payload shape: {logical_shape}")
    body = logical_shape[1:-1]
    if not body:
        return []
    return [int(item) for item in body.split(",")]
