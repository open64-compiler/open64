#!/usr/bin/env python3
"""Verify the complete SYNC-6 Conv plaintext family and range index.

This independent structural verifier is paired with
fhe_ckks_conv_all_asset.py. Design:
doc/FHE-SYNC6-S6-0C-DETAILED-EXECUTION-PLAN.md.
"""

import argparse
import hashlib
import json
from pathlib import Path

from fhe_ckks_stride_compaction_proof import bit_moves


def require(condition, message):
    """Reject incomplete, overlapping, or unauthenticated asset evidence."""
    if not condition:
        raise ValueError(message)


def file_digest(path):
    """Hash a potentially large asset without loading it into memory."""
    result = hashlib.sha256()
    with path.open("rb") as source:
        while True:
            data = source.read(1024 * 1024)
            if not data:
                break
            result.update(data)
    return result.hexdigest()


def record_digest(source, offset, length):
    """Hash one bounded indexed range from the shared plaintext asset."""
    source.seek(offset)
    remaining = length
    result = hashlib.sha256()
    while remaining:
        data = source.read(min(remaining, 1024 * 1024))
        require(data, "plaintext range ends beyond the asset")
        result.update(data)
        remaining -= len(data)
    return result.hexdigest()


def identity(record):
    """Return the exact source/context key shared by contexts and records."""
    return tuple(record[key] for key in (
        "fold_id", "owner_pu", "source_node", "source_value",
        "context_pu_identity", "context_callsite_id",
        "folded_weight_tcon", "folded_bias_tcon", "folded_tensor_name"))


def verify(asset_path, index_path):
    """Validate hashes, partitions, per-context rows, biases, and masks."""
    index = json.loads(index_path.read_text(encoding="utf-8"))
    require(index.get("schema") ==
            "open64.fhe.sync6.conv-plaintext-family.v1" and
            index.get("status") == "ckks_conformance_plaintext_input" and
            index.get("asset_basename") == asset_path.name and
            index.get("asset_byte_length") == asset_path.stat().st_size and
            index.get("asset_sha256") == file_digest(asset_path) and
            index.get("slot_count") == 32768 and
            index.get("physical_conv_definitions") == 13 and
            index.get("conv_contexts") == 21 and
            index.get("record_counts") == {
                "ace_feature_row": 5691,
                "expanded_bias": 21,
                "stride_compaction_mask": 104,
            }, "plaintext family header or whole-file digest is invalid")
    contexts = index.get("contexts")
    records = index.get("records")
    require(isinstance(contexts, list) and len(contexts) == 21 and
            isinstance(records, list) and len(records) == 5816 and
            len({identity(row) for row in contexts}) == 21,
            "plaintext context or record census is invalid")
    offset = 0
    with asset_path.open("rb") as asset:
        for ordinal, record in enumerate(records):
            require(record.get("ordinal") is not None and
                    record.get("dtype") == "F32" and
                    record.get("byte_offset") == offset and
                    isinstance(record.get("byte_length"), int) and
                    record["byte_length"] > 0 and
                    record["byte_length"] % 4 == 0 and
                    record_digest(asset, offset, record["byte_length"]) ==
                    record.get("sha256"),
                    f"plaintext record {ordinal} is malformed")
            offset += record["byte_length"]
    require(offset == asset_path.stat().st_size,
            "plaintext ranges do not partition the asset")
    stride_two = 0
    for context in contexts:
        first = context.get("first_record")
        count = context.get("record_count")
        require(isinstance(first, int) and isinstance(count, int) and
                first >= 0 and count > 0 and first + count <= len(records),
                "context record range is invalid")
        owned = records[first:first + count]
        require(all(identity(record) == identity(context)
                    for record in owned),
                "context range contains another context's plaintext")
        high_shape = context.get("high_resolution_shape")
        kernel = context.get("kernel_shape")
        require(isinstance(high_shape, list) and len(high_shape) == 4 and
                high_shape[0] == 1 and high_shape[2] == high_shape[3] and
                isinstance(kernel, list) and len(kernel) == 2 and
                kernel[0] == kernel[1] and kernel[0] in (1, 3),
                "context high-resolution geometry is invalid")
        output_channels, width = high_shape[1], high_shape[2]
        feature_rows = [row for row in owned
                        if row["kind"] == "ace_feature_row"]
        biases = [row for row in owned if row["kind"] == "expanded_bias"]
        masks = [row for row in owned
                 if row["kind"] == "stride_compaction_mask"]
        require(len(feature_rows) == context["feature_row_count"] and
                [row["ordinal"] for row in feature_rows] ==
                list(range(len(feature_rows))) and
                all(row["shape"] == [output_channels * width * width] and
                    row["byte_length"] == output_channels * width * width * 4
                    for row in feature_rows) and
                len(biases) == 1 and biases[0]["ordinal"] == 0 and
                biases[0]["shape"] == [output_channels * width * width] and
                biases[0]["byte_length"] == output_channels * width * width * 4,
                "feature-row or expanded-bias contract is invalid")
        if context["source_stride"] == [1, 1]:
            expected_masks = 0
        else:
            require(context["source_stride"] == [2, 2],
                    "source stride is outside the admitted recipe")
            expected_masks = 1 + 2 * len(bit_moves(width, output_channels))
            stride_two += 1
        require(context["compaction_mask_count"] == expected_masks and
                len(masks) == expected_masks and
                [row["ordinal"] for row in masks] ==
                list(range(expected_masks)) and
                all(row["shape"] == [32768] and
                    row["byte_length"] == 32768 * 4 for row in masks),
                "stride-compaction mask contract is invalid")
    require(stride_two == 4, "expected exactly four stride-two Conv contexts")
    print(json.dumps({
        "asset_sha256": index["asset_sha256"],
        "asset_byte_length": index["asset_byte_length"],
        "contexts": len(contexts),
        "records": len(records),
        "stride_two_contexts": stride_two,
    }, sort_keys=True))


def main():
    """Parse asset and index paths and run the independent verifier."""
    parser = argparse.ArgumentParser()
    parser.add_argument("--asset", type=Path, required=True)
    parser.add_argument("--index", type=Path, required=True)
    args = parser.parse_args()
    verify(args.asset, args.index)


if __name__ == "__main__":
    main()
