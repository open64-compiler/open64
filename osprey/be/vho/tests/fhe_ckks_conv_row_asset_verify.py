#!/usr/bin/env python3
"""Verify diagnostic ACE-style Conv rows against the authenticated budget.

Design: doc/FHE-SYNC6-CONV-MASK-ASSET-OPTIONS.md. This is test evidence,
not a mapped-image reader or an ACE plaintext decoder.
"""

import argparse
import hashlib
import json
from collections import defaultdict
from pathlib import Path


def digest(data):
    """Return the canonical SHA-256 of one byte slice."""
    return hashlib.sha256(data).hexdigest()


def identity(row):
    """Keep source definition and exact call context in one lookup key."""
    return (row["owner_pu"], row["source_node"], row["source_value"],
            row["context"], row["callsite"])


def verify(index_path, asset_path, report_path):
    """Reject changed bytes, missing rows, range gaps, and context mismatch."""
    index = json.loads(index_path.read_text(encoding="utf-8"))
    report = json.loads(report_path.read_text(encoding="utf-8"))
    asset = asset_path.read_bytes()
    if index["schema"] != "open64.fhe.sync6.ace-f32-row-index.v1" or \
            index["status"] != "diagnostic_not_mapped_ir" or \
            index["asset_basename"] != asset_path.name or \
            index["asset_sha256"] != digest(asset) or \
            index["asset_byte_length"] != len(asset) or \
            index["source_trace_sha256"] != report["source_trace_sha256"] or \
            index["folded_payload_sha256"] != report["folded_payload_sha256"]:
        raise ValueError("row asset header or whole-file digest mismatch")
    expected = {}
    for context in report["contexts"]:
        if not context["bounded_stride_one_recipe"]:
            continue
        key = (context["owner_pu"], context["conv_node"],
               context["source_value"], context["context"],
               context["callsite"])
        if key in expected:
            raise ValueError("duplicate budget context")
        expected[key] = context
    if len(expected) != 17 or len(index["rows"]) != 5211 or \
            len(asset) != report["ace_dense_f32_bytes_without_dedup"]:
        raise ValueError("row asset coverage or byte count mismatch")
    ordinals = defaultdict(set)
    offset = 0
    for row in index["rows"]:
        key = identity(row)
        context = expected.get(key)
        if context is None or row["dtype"] != "F32" or \
                row["shape"] != [context["ace_f32_row_length"]] or \
                row["folded_weight_tcon"] != context["weight_tcon"] or \
                row["folded_weight_sha256"] != context["weight_sha256"] or \
                type(row["feature_row"]) is not int or \
                row["feature_row"] < 0 or \
                row["feature_row"] >= context["ace_feature_rows"] or \
                row["feature_row"] in ordinals[key]:
            raise ValueError("row source/context/formal identity mismatch")
        length = context["ace_f32_row_length"] * 4
        if row["byte_offset"] != offset or row["byte_length"] != length or \
                row["sha256"] != digest(asset[offset:offset + length]):
            raise ValueError("row range or SHA-256 mismatch")
        ordinals[key].add(row["feature_row"])
        offset += length
    if offset != len(asset) or set(ordinals) != set(expected) or any(
            len(ordinals[key]) != context["ace_feature_rows"]
            for key, context in expected.items()):
        raise ValueError("missing, duplicate, or trailing ACE feature rows")
    return len(index["rows"]), len(asset), digest(asset)


def main():
    """Validate one retained diagnostic side-file family."""
    parser = argparse.ArgumentParser()
    parser.add_argument("--index", type=Path, required=True)
    parser.add_argument("--asset", type=Path, required=True)
    parser.add_argument("--report", type=Path, required=True)
    args = parser.parse_args()
    rows, size, sha = verify(args.index, args.asset, args.report)
    print(f"verified rows={rows} bytes={size} sha256={sha}")


if __name__ == "__main__":
    main()
