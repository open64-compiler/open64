#!/usr/bin/env python3
"""Extract authenticated folded stem bytes for the bounded C2 Conv oracle.

The output is local test evidence, never checked-in model data. Design:
doc/FHE-SYNC6-S6-0C-DETAILED-EXECUTION-PLAN.md.
"""

import argparse
import hashlib
import json
import struct
from pathlib import Path


def digest(data):
    """Return the canonical lowercase SHA-256 of exact bytes."""
    return hashlib.sha256(data).hexdigest()


def tensor_bytes(source, index, base, name, shape):
    """Read a typed and bounded SafeTensors slice by structural name."""
    row = index.get(name)
    if not isinstance(row, dict) or row.get("dtype") != "F32" or \
            row.get("shape") != shape:
        raise ValueError(f"wrong folded tensor type/shape: {name}")
    offsets = row.get("data_offsets")
    size = 4
    for extent in shape:
        size *= extent
    if not isinstance(offsets, list) or len(offsets) != 2 or \
            any(type(value) is not int for value in offsets) or \
            offsets[1] - offsets[0] != size or offsets[0] < 0 or \
            base + offsets[1] > len(source):
        raise ValueError(f"wrong folded tensor range: {name}")
    return source[base + offsets[0]:base + offsets[1]]


def main():
    """Verify the replay-pinned file hash, then publish a local test fixture."""
    parser = argparse.ArgumentParser()
    parser.add_argument("--payload", type=Path, required=True)
    parser.add_argument("--replay", type=Path, required=True)
    parser.add_argument("--output", type=Path, required=True)
    args = parser.parse_args()
    source = args.payload.read_bytes()
    replay = json.loads(args.replay.read_text(encoding="utf-8"))
    expected = replay["inputs"]["folded_payload"]
    if args.payload.name != expected["basename"] or \
            len(source) != expected["size"] or \
            digest(source) != expected["sha256"]:
        raise ValueError("folded payload differs from authenticated replay")
    if len(source) < 8:
        raise ValueError("truncated SafeTensors header")
    header_size = struct.unpack("<Q", source[:8])[0]
    if header_size == 0 or header_size > 16 * 1024 * 1024 or \
            8 + header_size > len(source):
        raise ValueError("invalid SafeTensors header")
    index = json.loads(source[8:8 + header_size])
    if not isinstance(index, dict):
        raise ValueError("invalid SafeTensors tensor index")
    base = 8 + header_size
    weights = tensor_bytes(source, index, base,
                           "entry_stem_conv_folded_weight", [16, 3, 3, 3])
    bias = tensor_bytes(source, index, base,
                        "entry_stem_conv_folded_bias", [16])
    args.output.mkdir(parents=True, exist_ok=True)
    fixture = args.output / "stem_folded_oihw_f32.bin"
    fixture.write_bytes(weights + bias)
    manifest = {
        "schema": "open64.fhe.sync6.conv-fixture.v1",
        "purpose": "clear_slot_oracle_only_no_whirl_emission",
        "folded_payload_sha256": expected["sha256"],
        "weight_sha256": digest(weights),
        "bias_sha256": digest(bias),
        "fixture_sha256": digest(weights + bias),
        "weight_shape": [16, 3, 3, 3],
        "bias_shape": [16],
    }
    (args.output / "stem_folded_manifest.json").write_text(
        json.dumps(manifest, indent=2) + "\n", encoding="utf-8")
    print(fixture)


if __name__ == "__main__":
    main()
