#!/usr/bin/env python3
"""Publish the complete authenticated plaintext family for 21 ResNet Convs.

This is the deterministic S6-0c C2 producer used before native CKKS expansion.
It writes ACE column-first feature rows, expanded folded biases, and explicit
stride-two compaction masks. Design:
doc/FHE-SYNC6-S6-0C-DETAILED-EXECUTION-PLAN.md and
doc/FHE-SYNC6-CKKS-IR-CONFORMANCE-GATE.md.
"""

import argparse
import hashlib
import json
import math
import os
import struct
import tempfile
from pathlib import Path

from fhe_ckks_conv_mask_budget import ace_row_bytes
from fhe_ckks_conv_payload_fixture import tensor_bytes
from fhe_ckks_stride_compaction_proof import active_mapping, bit_moves


SLOT_COUNT = 32768


def digest(data):
    """Return the lowercase SHA-256 used by replay and asset contracts."""
    return hashlib.sha256(data).hexdigest()


def require(condition, message):
    """Reject incomplete provenance or geometry before output publication."""
    if not condition:
        raise ValueError(message)


def load_authenticated_inputs(budget_path, payload_path, replay_path):
    """Load one replay-bound budget and its exact folded SafeTensors bytes."""
    replay_bytes = replay_path.read_bytes()
    replay = json.loads(replay_bytes)
    budget_bytes = budget_path.read_bytes()
    budget = json.loads(budget_bytes)
    payload = payload_path.read_bytes()
    expected = replay["inputs"]["folded_payload"]
    require(payload_path.name == expected["basename"] and
            len(payload) == expected["size"] and
            digest(payload) == expected["sha256"],
            "folded payload differs from authenticated replay")
    require(budget.get("schema") ==
            "open64.fhe.sync6.conv-plaintext-budget.v2" and
            budget.get("source_trace_sha256") ==
            replay["inputs"]["trace"]["sha256"] and
            budget.get("folded_payload_sha256") == expected["sha256"] and
            budget.get("physical_conv_definitions") == 13 and
            budget.get("folded_call_contexts") == 21 and
            budget.get("slot_count") == SLOT_COUNT,
            "Conv budget is not the certified 13-definition/21-context family")
    require(len(payload) >= 8, "folded payload header is truncated")
    header_size = struct.unpack_from("<Q", payload)[0]
    require(0 < header_size <= 16 * 1024 * 1024 and
            8 + header_size <= len(payload),
            "folded payload header length is invalid")
    index = json.loads(payload[8:8 + header_size])
    require(isinstance(index, dict), "folded payload index is invalid")
    return (replay, replay_bytes, budget, budget_bytes, payload, index,
            8 + header_size)


def find_folded_pair(row, payload, tensor_index, data_base):
    """Resolve exactly one folded weight/bias pair by shape and digest."""
    matches = []
    for name in tensor_index:
        if not name.endswith("_weight"):
            continue
        bias_name = name[:-6] + "bias"
        if bias_name not in tensor_index:
            continue
        try:
            weight = tensor_bytes(payload, tensor_index, data_base, name,
                                  row["weight_shape"])
            bias = tensor_bytes(payload, tensor_index, data_base, bias_name,
                                [row["output_shape"][1]])
        except ValueError:
            continue
        if digest(weight) == row["weight_sha256"] and \
                digest(bias) == row["bias_sha256"]:
            matches.append((name, weight, bias))
    require(len(matches) == 1,
            "folded Conv weight/bias identity is absent or ambiguous")
    return matches[0]


def f32_mask(indices):
    """Encode one full-slot binary plaintext mask as IEEE little-endian F32."""
    data = bytearray(SLOT_COUNT * 4)
    for index in indices:
        require(0 <= index < SLOT_COUNT, "mask slot lies outside CKKS packing")
        struct.pack_into("<f", data, index * 4, 1.0)
    return bytes(data)


def compaction_masks(width, channels):
    """Yield the selection and sequential bit-move masks in execution order."""
    require(width >= 4 and width & (width - 1) == 0 and
            channels >= 1 and channels & (channels - 1) == 0 and
            channels * width * width <= SLOT_COUNT,
            "stride-two packing geometry is unsupported")
    mapping = active_mapping(width, channels)
    yield {
        "mask_role": "stride2_selection",
        "stage_ordinal": 0,
        "mask_ordinal": 0,
        "signed_left_rotation": 0,
    }, f32_mask(set(mapping.values()))
    for ordinal, (source_bit, target_bit) in enumerate(
            bit_moves(width, channels), 1):
        selected = {
            current for original, current in mapping.items()
            if original & (1 << source_bit)
        }
        rotation = (1 << source_bit) - (1 << target_bit)
        yield {
            "mask_role": "stride2_move_selected",
            "stage_ordinal": ordinal,
            "mask_ordinal": 0,
            "source_bit": source_bit,
            "target_bit": target_bit,
            "signed_left_rotation": rotation,
        }, f32_mask(selected)
        yield {
            "mask_role": "stride2_move_complement",
            "stage_ordinal": ordinal,
            "mask_ordinal": 1,
            "source_bit": source_bit,
            "target_bit": target_bit,
            "signed_left_rotation": 0,
        }, f32_mask(set(range(SLOT_COUNT)) - selected)
        mapping = {
            original: current - rotation if current in selected else current
            for original, current in mapping.items()
        }
    half = width // 2
    require(set(mapping.values()) == set(range(channels * half * half)),
            "stride-two mask network does not produce dense NCHW slots")


def expanded_bias_bytes(bias, output_channels, width):
    """Expand one folded channel bias over every high-resolution output slot."""
    values = list(struct.iter_unpack("<f", bias))
    require(len(values) == output_channels and
            all(math.isfinite(value[0]) for value in values),
            "folded bias is non-finite or has the wrong channel count")
    plane = width * width
    data = bytearray(output_channels * plane * 4)
    for channel, (value,) in enumerate(values):
        packed = struct.pack("<f", value)
        begin = channel * plane * 4
        for offset in range(begin, begin + plane * 4, 4):
            data[offset:offset + 4] = packed
    return bytes(data)


def context_key(row):
    """Provide deterministic whole-model ordering independent of table scans."""
    return (row["owner_pu"], row["context"], row["callsite"],
            row["conv_node"], row["fold_id"])


def add_blob(asset, asset_hash, records, identity, kind, ordinal, metadata,
             data):
    """Append one exact byte range and its complete source/context provenance."""
    offset = asset.tell()
    asset.write(data)
    asset_hash.update(data)
    record = dict(identity)
    record.update({
        "kind": kind,
        "ordinal": ordinal,
        "dtype": "F32",
        "byte_offset": offset,
        "byte_length": len(data),
        "sha256": digest(data),
    })
    record.update(metadata)
    records.append(record)


def fsync_directory(path):
    """Make no-replace link publication durable on the containing filesystem."""
    descriptor = os.open(path, os.O_RDONLY)
    try:
        os.fsync(descriptor)
    finally:
        os.close(descriptor)


def publish(budget_path, payload_path, replay_path, asset_path, index_path,
            fail_after_records):
    """Build all 21 contexts and publish the index last as commit marker."""
    require(asset_path.name != index_path.name and
            asset_path.parent.resolve() == index_path.parent.resolve() and
            not asset_path.exists() and not index_path.exists(),
            "asset endpoints must be unused, distinct, and colocated")
    (replay, replay_bytes, budget, budget_bytes, payload, tensor_index,
     data_base) = load_authenticated_inputs(budget_path, payload_path,
                                            replay_path)
    contexts = budget.get("contexts")
    require(isinstance(contexts, list) and len(contexts) == 21,
            "Conv budget lacks exactly 21 contexts")
    asset_path.parent.mkdir(parents=True, exist_ok=True)
    asset_tmp = index_tmp = None
    published = []
    records = []
    context_rows = []
    asset_hash = hashlib.sha256()
    try:
        descriptor, asset_tmp = tempfile.mkstemp(
            prefix=asset_path.name + ".tmp.", dir=asset_path.parent)
        with os.fdopen(descriptor, "wb") as asset:
            for row in sorted(contexts, key=context_key):
                tensor_name, weight, bias = find_folded_pair(
                    row, payload, tensor_index, data_base)
                output_channels = row["output_shape"][1]
                width = row["input_shape"][2]
                kernel = row["weight_shape"][2]
                require(row["input_shape"] ==
                        [1, row["weight_shape"][1], width, width] and
                        row["weight_shape"] ==
                        [output_channels, row["weight_shape"][1],
                         kernel, kernel] and kernel in (1, 3) and
                        row["stride"] in ([1, 1], [2, 2]) and
                        output_channels * width * width <= SLOT_COUNT,
                        "Conv context geometry is outside the admitted recipe")
                identity = {
                    "fold_id": row["fold_id"],
                    "owner_pu": row["owner_pu"],
                    "source_node": row["conv_node"],
                    "source_value": row["source_value"],
                    "context_pu_identity": row["context"],
                    "context_callsite_id": row["callsite"],
                    "folded_weight_tcon": row["weight_tcon"],
                    "folded_bias_tcon": row["bias_tcon"],
                    "folded_tensor_name": tensor_name,
                }
                first = len(records)
                rows = list(ace_row_bytes(weight, row["weight_shape"], width))
                expected_rows = row["weight_shape"][1] * kernel * kernel
                require(len(rows) == expected_rows,
                        "ACE feature-row count disagrees with OIHW shape")
                for ordinal, data in enumerate(rows):
                    add_blob(asset, asset_hash, records, identity,
                             "ace_feature_row", ordinal,
                             {"shape": [output_channels, width, width]}, data)
                    if fail_after_records == len(records):
                        raise ValueError("injected complete Conv asset failure")
                bias_data = expanded_bias_bytes(bias, output_channels, width)
                add_blob(asset, asset_hash, records, identity,
                         "expanded_bias", 0,
                         {"shape": [output_channels, width, width]}, bias_data)
                if fail_after_records == len(records):
                    raise ValueError("injected complete Conv asset failure")
                mask_count = 0
                if row["stride"] == [2, 2]:
                    for ordinal, (metadata, data) in enumerate(
                            compaction_masks(width, output_channels)):
                        add_blob(asset, asset_hash, records, identity,
                                 "stride_compaction_mask", ordinal,
                                 dict(metadata, shape=[SLOT_COUNT]), data)
                        mask_count += 1
                        if fail_after_records == len(records):
                            raise ValueError(
                                "injected complete Conv asset failure")
                context_rows.append(dict(
                    identity,
                    source_stride=row["stride"],
                    high_resolution_shape=[1, output_channels, width, width],
                    kernel_shape=[kernel, kernel],
                    first_record=first,
                    record_count=len(records) - first,
                    feature_row_count=len(rows),
                    compaction_mask_count=mask_count,
                    folded_weight_sha256=row["weight_sha256"],
                    folded_bias_sha256=row["bias_sha256"],
                ))
            asset.flush()
            os.fsync(asset.fileno())
        counts = {}
        for record in records:
            counts[record["kind"]] = counts.get(record["kind"], 0) + 1
        require(len(context_rows) == 21 and
                len({context_key(row) for row in contexts}) == 21 and
                counts == {"ace_feature_row": 5691,
                           "expanded_bias": 21,
                           "stride_compaction_mask": 104},
                "complete Conv plaintext census changed")
        index = {
            "schema": "open64.fhe.sync6.conv-plaintext-family.v1",
            "status": "ckks_conformance_plaintext_input",
            "asset_basename": asset_path.name,
            "asset_byte_length": os.path.getsize(asset_tmp),
            "asset_sha256": asset_hash.hexdigest(),
            "source_replay_sha256": digest(replay_bytes),
            "source_binary_sha256": replay["inputs"]["binary"]["sha256"],
            "source_trace_sha256": replay["inputs"]["trace"]["sha256"],
            "folded_payload_sha256":
                replay["inputs"]["folded_payload"]["sha256"],
            "source_budget_sha256": digest(budget_bytes),
            "generator_sha256": digest(Path(__file__).read_bytes()),
            "slot_count": SLOT_COUNT,
            "physical_conv_definitions": 13,
            "conv_contexts": len(context_rows),
            "record_counts": counts,
            "contexts": context_rows,
            "records": records,
        }
        descriptor, index_tmp = tempfile.mkstemp(
            prefix=index_path.name + ".tmp.", dir=index_path.parent)
        with os.fdopen(descriptor, "w", encoding="utf-8") as output:
            json.dump(index, output, sort_keys=True, separators=(",", ":"))
            output.write("\n")
            output.flush()
            os.fsync(output.fileno())
        for temporary, final in ((asset_tmp, asset_path),
                                 (index_tmp, index_path)):
            os.link(temporary, final)
            published.append(final)
            fsync_directory(final.parent)
    except BaseException:
        for path in reversed(published):
            path.unlink()
        fsync_directory(asset_path.parent)
        raise
    finally:
        for path in (asset_tmp, index_tmp):
            if path is not None and os.path.exists(path):
                os.unlink(path)
    print(json.dumps({
        "asset": str(asset_path),
        "index": str(index_path),
        "asset_sha256": index["asset_sha256"],
        "asset_byte_length": index["asset_byte_length"],
        "record_counts": index["record_counts"],
    }, sort_keys=True))


def main():
    """Parse command-line endpoints and execute the atomic producer."""
    parser = argparse.ArgumentParser()
    parser.add_argument("--budget", type=Path, required=True)
    parser.add_argument("--payload", type=Path, required=True)
    parser.add_argument("--replay", type=Path, required=True)
    parser.add_argument("--asset-output", type=Path, required=True)
    parser.add_argument("--index-output", type=Path, required=True)
    parser.add_argument("--fail-after-records", type=int)
    args = parser.parse_args()
    if args.fail_after_records is not None and args.fail_after_records <= 0:
        parser.error("--fail-after-records must be positive")
    publish(args.budget, args.payload, args.replay, args.asset_output,
            args.index_output, args.fail_after_records)


if __name__ == "__main__":
    if struct.pack("=f", 1.0) != struct.pack("<f", 1.0):
        raise SystemExit("complete Conv assets require little-endian F32")
    main()
