#!/usr/bin/env python3
"""Budget authenticated Conv plaintexts and optionally retain raw row evidence.

The managed-image joins and column-first mask rule follow
doc/FHE-SYNC6-S6-0C-DETAILED-EXECUTION-PLAN.md. This is a read-only cost
probe, not a backend producer, mapped-IR writer, or provider encode benchmark.
"""

import argparse
import hashlib
import json
import math
import os
import re
import struct
import sys
import tempfile
import time
from collections import Counter, defaultdict
from pathlib import Path


def sha256(data):
    """Hash exact bytes for source authentication and mask deduplication."""
    return hashlib.sha256(data).hexdigest()


def rows(pattern, trace):
    """Index the selected logical ir_b2a table by its printed row ID."""
    return {int(match.group(1)): match for match in re.finditer(pattern, trace, re.M)}


def tensor(source, index, base, name, shape, references):
    """Verify one F32 side-file slice against both its header and WHIRL URI."""
    item = index.get(name)
    if not isinstance(item, dict) or item.get("dtype") != "F32" or \
            item.get("shape") != shape:
        raise ValueError(f"wrong folded tensor descriptor: {name}")
    offsets = item.get("data_offsets")
    length = 4
    for extent in shape:
        length *= extent
    if not isinstance(offsets, list) or len(offsets) != 2 or \
            any(type(value) is not int for value in offsets) or \
            offsets[0] < 0 or offsets[1] - offsets[0] != length or \
            base + offsets[1] > len(source):
        raise ValueError(f"invalid folded tensor range: {name}")
    data = source[base + offsets[0]:base + offsets[1]]
    expected = references.get(name)
    if expected != (offsets[0], length, sha256(data)):
        raise ValueError(f"folded tensor URI/checksum mismatch: {name}")
    return data


def mask_hashes(weights, shape, height, slot_count):
    """Hash alternative signed-rotation groups for the clear-slot oracle."""
    output_channels, input_channels, _, _ = shape
    plane = height * height
    groups = defaultdict(list)
    rotations = set()
    live = 0
    for index, (weight,) in enumerate(struct.iter_unpack("<f", weights)):
        if not math.isfinite(weight):
            raise ValueError("nonfinite folded weight")
        if weight == 0:
            continue
        kx = index % 3
        ky = (index // 3) % 3
        ci = (index // 9) % input_channels
        oc = index // (9 * input_channels)
        rotation = (ci - oc) * plane + (ky - 1) * height + kx - 1
        groups[rotation].append((oc, ky, kx, weight))
        if rotation:
            rotations.add(rotation)
        live += 1
    digests = []
    for rotation in sorted(groups):
        mask = bytearray(slot_count * 8)
        for oc, ky, kx, weight in groups[rotation]:
            for y in range(height):
                iy = y + ky - 1
                if iy < 0 or iy >= height:
                    continue
                for x in range(height):
                    ix = x + kx - 1
                    if ix < 0 or ix >= height:
                        continue
                    offset = (oc * plane + y * height + x) * 8
                    if mask[offset:offset + 8] != b"\0" * 8:
                        raise ValueError("rotation-group output slot collision")
                    struct.pack_into("<d", mask, offset, weight)
        digests.append(sha256(mask))
    return live, len(rotations), digests, sorted(rotations)


def ace_row_bytes(weights, shape, height):
    """Yield exact column-first transformed F32 rows in feature order."""
    output_channels, input_channels, kernel_height, kernel_width = shape
    if kernel_height not in (1, 3) or kernel_width != kernel_height:
        raise ValueError("ACE row transform requires a square 1x1 or 3x3 kernel")
    feature_count = input_channels * kernel_height * kernel_width
    pad = kernel_height // 2
    plane = height * height
    values = [value for (value,) in struct.iter_unpack("<f", weights)]
    if len(values) != output_channels * feature_count:
        raise ValueError("folded weight byte count disagrees with OIHW shape")
    if any(not math.isfinite(value) for value in values):
        raise ValueError("nonfinite folded weight")
    for row in range(feature_count):
        data = bytearray(output_channels * plane * 4)
        for oc in range(output_channels):
            feature = (row + oc * kernel_height * kernel_width) % \
                feature_count
            ky, kx = divmod(feature % (kernel_height * kernel_width),
                            kernel_width)
            weight = values[oc * feature_count + feature]
            if weight == 0:
                continue
            for y in range(height):
                iy = y + ky - pad
                if not 0 <= iy < height:
                    continue
                for x in range(height):
                    ix = x + kx - pad
                    if 0 <= ix < height:
                        struct.pack_into("<f", data,
                                         (oc * plane + y * height + x) * 4,
                                         weight)
        yield bytes(data)


def ace_row_hashes(weights, shape, height):
    """Hash the selected ACE-style rows without retaining their bytes."""
    return [sha256(row) for row in ace_row_bytes(weights, shape, height)]


def emit_asset(asset_contexts, report, asset_path, index_path, report_path,
               fail_after_rows):
    """Publish diagnostic files with the index as the last commit marker."""
    endpoints = (asset_path, report_path, index_path)
    if len({path.resolve() for path in endpoints}) != 3 or \
            any(path.exists() for path in endpoints):
        raise ValueError("row asset endpoints must be distinct and unused")
    asset_path.parent.mkdir(parents=True, exist_ok=True)
    index_path.parent.mkdir(parents=True, exist_ok=True)
    report_path.parent.mkdir(parents=True, exist_ok=True)
    asset_tmp = index_tmp = report_tmp = None
    published = []
    try:
        handle, asset_tmp = tempfile.mkstemp(prefix=asset_path.name + ".tmp.",
                                             dir=asset_path.parent)
        index_rows = []
        digest = hashlib.sha256()
        offset = 0
        with os.fdopen(handle, "wb") as asset:
            for context, weights, shape, height, expected in sorted(
                    asset_contexts,
                    key=lambda item: (item[0]["owner_pu"],
                                      item[0]["context"],
                                      item[0]["callsite"],
                                      item[0]["conv_node"])):
                for ordinal, row in enumerate(ace_row_bytes(weights, shape,
                                                             height)):
                    row_hash = sha256(row)
                    if row_hash != expected[ordinal]:
                        raise ValueError("ACE row differs from authenticated budget")
                    asset.write(row)
                    digest.update(row)
                    index_rows.append({
                        "owner_pu": context["owner_pu"],
                        "source_node": context["conv_node"],
                        "source_value": context["source_value"],
                        "context": context["context"],
                        "callsite": context["callsite"],
                        "folded_weight_tcon": context["weight_tcon"],
                        "folded_weight_sha256": context["weight_sha256"],
                        "feature_row": ordinal,
                        "shape": [context["ace_f32_row_length"]],
                        "dtype": "F32",
                        "byte_offset": offset,
                        "byte_length": len(row),
                        "sha256": row_hash,
                    })
                    offset += len(row)
                    if fail_after_rows == len(index_rows):
                        raise ValueError("injected row asset failure")
            asset.flush()
            os.fsync(asset.fileno())
        if len(index_rows) != report["ace_feature_row_count"] or \
                offset != report["ace_dense_f32_bytes_without_dedup"]:
            raise ValueError("ACE row asset count or byte budget changed")
        index = {
            "schema": "open64.fhe.sync6.ace-f32-row-index.v1",
            "status": "diagnostic_not_mapped_ir",
            "asset_basename": asset_path.name,
            "asset_sha256": digest.hexdigest(),
            "asset_byte_length": offset,
            "source_trace_sha256": report["source_trace_sha256"],
            "folded_payload_sha256": report["folded_payload_sha256"],
            "rows": index_rows,
        }
        handle, index_tmp = tempfile.mkstemp(prefix=index_path.name + ".tmp.",
                                             dir=index_path.parent)
        with os.fdopen(handle, "w", encoding="utf-8") as output:
            json.dump(index, output, sort_keys=True, separators=(",", ":"))
            output.write("\n")
            output.flush()
            os.fsync(output.fileno())
        handle, report_tmp = tempfile.mkstemp(
            prefix=report_path.name + ".tmp.", dir=report_path.parent)
        with os.fdopen(handle, "w", encoding="utf-8") as output:
            output.write(json.dumps(report, indent=2) + "\n")
            output.flush()
            os.fsync(output.fileno())
        for temporary, final in ((asset_tmp, asset_path),
                                 (report_tmp, report_path),
                                 (index_tmp, index_path)):
            os.link(temporary, final)
            published.append(final)
    except BaseException:
        for path in reversed(published):
            path.unlink()
        raise
    finally:
        for path in (asset_tmp, report_tmp, index_tmp):
            if path is not None and os.path.exists(path):
                os.unlink(path)


def main():
    """Authenticate 21 contexts and emit a budget plus optional row evidence."""
    parser = argparse.ArgumentParser()
    parser.add_argument("--trace", type=Path, required=True)
    parser.add_argument("--payload", type=Path, required=True)
    parser.add_argument("--replay", type=Path, required=True)
    parser.add_argument("--output", type=Path, required=True)
    parser.add_argument("--asset-output", type=Path)
    parser.add_argument("--index-output", type=Path)
    parser.add_argument("--fail-after-rows", type=int)
    args = parser.parse_args()
    if (args.asset_output is None) != (args.index_output is None):
        parser.error("--asset-output and --index-output must be paired")
    if args.fail_after_rows is not None and (args.asset_output is None or
                                             args.fail_after_rows <= 0):
        parser.error("--fail-after-rows requires assets and a positive count")
    start = time.monotonic()
    replay = json.loads(args.replay.read_text(encoding="utf-8"))
    trace_bytes = args.trace.read_bytes()
    source = args.payload.read_bytes()
    for label, path, data in (("trace", args.trace, trace_bytes),
                              ("folded_payload", args.payload, source)):
        expected = replay["inputs"][label]
        if path.name != expected["basename"] or \
                len(data) != expected["size"] or \
                sha256(data) != expected["sha256"]:
            raise ValueError(f"{label} differs from authenticated replay")
    if len(source) < 8:
        raise ValueError("truncated SafeTensors file")
    header_size = struct.unpack_from("<Q", source)[0]
    if not 0 < header_size <= 16 * 1024 * 1024 or \
            header_size + 8 > len(source):
        raise ValueError("invalid SafeTensors header")
    index = json.loads(source[8:8 + header_size])
    base = 8 + header_size
    trace = trace_bytes.decode("utf-8")
    dispositions = {}
    for match in re.finditer(
            r"^  \[\d+\] source_node=node(\d+) source_operator="
            r"OPR_DSLCONV2D result=value(\d+)\([^)]*\) "
            r"owner_pu=(\S+) disposition=domain_wrapper", trace, re.M):
        key = (match.group(3), int(match.group(1)))
        if key in dispositions:
            raise ValueError("duplicate physical Conv identity")
        dispositions[key] = int(match.group(2))
    nodes = rows(
        r"^  \[(\d+)\] operator=OPR_DSLCONV2D version=\d+ "
        r"operands=\[([^]]+)\] attributes=\[([^]]+)\] result=value(\d+)",
        trace)
    values = rows(
        r"^  \[(\d+)\] kind=[^\n]*?tensor_descriptor="
        r"\{[^\n]*?shape=\[([0-9,]+)\]", trace)
    roles = {}
    for match in re.finditer(
            r"^  \[\d+\] kind=source_external_tensor role=(\S+) "
            r"[^\n]* source_tcon=(\d+) flags=", trace, re.M):
        tcon = int(match.group(2))
        if tcon in roles:
            raise ValueError("duplicate runtime TCON role")
        roles[tcon] = match.group(1)
    references = {}
    for match in re.finditer(
            r"safetensors://secure_resnet20\.fhe\.safetensors#([^?\s]+)"
            r"\?offset=(\d+)&length=(\d+)&checksum=([0-9a-f]{64})", trace):
        value = (int(match.group(2)), int(match.group(3)), match.group(4))
        if match.group(1) in references and references[match.group(1)] != value:
            raise ValueError("inconsistent folded tensor URI")
        references[match.group(1)] = value
    folds = list(re.finditer(
        r"^  \[(\d+)\] owner_pu=(\S+) conv=node(\d+) "
        r"batch_norm=node(\d+) context=(\S+) callsite=(\d+) "
        r"[^\n]* folded_weight=tcon(\d+) folded_bias=tcon(\d+) flags=",
        trace, re.M))
    if len(dispositions) != 13 or len(folds) != 21:
        raise ValueError("expected 13 physical Conv definitions and 21 folds")

    contexts = []
    mask_digests = set()
    ace_row_digests = set()
    asset_contexts = []
    all_rotations = set()
    seen = set()
    for fold in folds:
        fold_id, owner, node_id, _, context, callsite, weight_tcon, bias_tcon = \
            fold.groups()
        key = (owner, int(node_id))
        identity = (owner, int(node_id), int(callsite))
        if identity in seen or key not in dispositions:
            raise ValueError("missing or duplicate Conv/context identity")
        seen.add(identity)
        node = nodes[int(node_id)]
        if int(node.group(4)) != dispositions[key]:
            raise ValueError("Conv disposition/result mismatch")
        input_id = int(re.match(r"value(\d+)", node.group(2)).group(1))
        input_shape = [int(x) for x in values[input_id].group(2).split(",")]
        output_shape = [int(x) for x in values[dispositions[key]].group(2).split(",")]
        stride = tuple(int(x) for x in re.search(
            r"attr.stride=(\d+),(\d+)", node.group(3)).groups())
        kernel = tuple(int(x) for x in re.search(
            r"attr.kernel_shape=(\d+),(\d+)", node.group(3)).groups())
        weight_name = roles[int(weight_tcon)]
        bias_name = roles[int(bias_tcon)]
        weight_shape = [output_shape[1], input_shape[1], *kernel]
        weight = tensor(source, index, base, weight_name,
                        weight_shape, references)
        bias = tensor(source, index, base, bias_name,
                      [output_shape[1]], references)
        if any(not math.isfinite(value) for (value,) in
               struct.iter_unpack("<f", bias)):
            raise ValueError("nonfinite folded bias")
        row = {
            "fold_id": int(fold_id), "owner_pu": owner,
            "conv_node": int(node_id), "source_value": dispositions[key],
            "context": context, "callsite": int(callsite),
            "weight_tcon": int(weight_tcon), "bias_tcon": int(bias_tcon),
            "weight_sha256": sha256(weight), "bias_sha256": sha256(bias),
            "input_shape": input_shape, "output_shape": output_shape,
            "weight_shape": weight_shape, "stride": stride,
        }
        supported = stride == (1, 1) and kernel == (3, 3) and \
            input_shape[0] == output_shape[0] == 1 and \
            input_shape[2:] == output_shape[2:] and \
            input_shape[2] == input_shape[3] and \
            output_shape[1] * output_shape[2] * output_shape[3] <= 32768
        row["bounded_stride_one_recipe"] = supported
        if supported:
            live, keys, digests, rotations = mask_hashes(
                weight, weight_shape, input_shape[2], 32768)
            feature_rows = ace_row_hashes(weight, weight_shape, input_shape[2])
            if keys != len(digests) - 1:
                raise ValueError("zero-rotation mask or key census changed")
            row.update(live_terms=live, masks=len(digests),
                       signed_rotation_keys=keys,
                       dense_f8_bytes=len(digests) * 32768 * 8,
                       ace_feature_rows=len(feature_rows),
                       ace_f32_row_length=output_shape[1] *
                                          output_shape[2] * output_shape[3],
                       ace_dense_f32_bytes=len(feature_rows) * output_shape[1] *
                                           output_shape[2] * output_shape[3] * 4,
                       candidate_steps=5 * len(digests))
            mask_digests.update(digests)
            ace_row_digests.update(feature_rows)
            if args.asset_output is not None:
                asset_contexts.append((row, weight, weight_shape,
                                       input_shape[2], feature_rows))
            all_rotations.update(rotations)
        contexts.append(row)
    supported = [row for row in contexts if row["bounded_stride_one_recipe"]]
    excluded = [row for row in contexts if not row["bounded_stride_one_recipe"]]
    if len(supported) != 17 or len(excluded) != 4:
        raise ValueError("captured Conv coverage changed")
    if any(tuple(row["stride"]) != (2, 2) for row in excluded):
        raise ValueError("unsupported non-stride-two Conv changed coverage")
    by_shape = Counter(tuple(row["weight_shape"] + row["input_shape"][2:])
                       for row in supported)
    report = {
        "schema": "open64.fhe.sync6.conv-plaintext-budget.v2",
        "status": ("diagnostic_raw_row_asset_no_whirl_emission"
                   if args.asset_output is not None else
                   "read_only_no_mask_asset_or_whirl_emission"),
        "source_trace_sha256": sha256(trace_bytes),
        "folded_payload_sha256": sha256(source),
        "physical_conv_definitions": len(dispositions),
        "folded_call_contexts": len(contexts),
        "bounded_stride_one_contexts": len(supported),
        "excluded_stride_two_contexts": len(excluded),
        "slot_count": 32768,
        "selected_asset_format": "ace_column_first_raw_f32_feature_rows",
        "ace_feature_row_count": sum(row["ace_feature_rows"]
                                     for row in supported),
        "distinct_ace_feature_row_sha256_count": len(ace_row_digests),
        "ace_dense_f32_bytes_without_dedup": sum(
            row["ace_dense_f32_bytes"] for row in supported),
        "diagnostic_grouped_mask_format":
            "little_endian_ieee_binary64_32768_slots",
        "diagnostic_grouped_mask_count": sum(row["masks"]
                                             for row in supported),
        "distinct_grouped_mask_sha256_count": len(mask_digests),
        "grouped_dense_f8_bytes_without_dedup": sum(
            row["dense_f8_bytes"] for row in supported),
        "distinct_global_signed_rotation_keys": len(all_rotations),
        "candidate_execution_weighted_steps": sum(
            row["candidate_steps"] for row in supported),
        "largest_single_event_candidate_steps": max(
            row["candidate_steps"] for row in supported),
        "c1_event_step_limit": 65535,
        "c1_event_byte_limit": 4 * 1024 * 1024,
        "c1_actual_serialized_bytes": None,
        "provider_plaintext_encoding_seconds": None,
        "by_shape": [{"shape": list(shape), "contexts": count}
                     for shape, count in sorted(by_shape.items())],
        "contexts": contexts,
    }
    if args.asset_output is not None:
        emit_asset(asset_contexts, report, args.asset_output,
                   args.index_output, args.output, args.fail_after_rows)
    else:
        args.output.parent.mkdir(parents=True, exist_ok=True)
        args.output.write_text(json.dumps(report, indent=2) + "\n",
                               encoding="utf-8")
    print(json.dumps({key: value for key, value in report.items()
                      if key not in ("contexts", "by_shape")}, indent=2))
    print(f"clear mask reconstruction/hash seconds: "
          f"{time.monotonic() - start:.3f}", file=sys.stderr)


if __name__ == "__main__":
    if sys.byteorder != "little" or struct.calcsize("f") != 4:
        raise SystemExit("ACE-style F32 probe requires little-endian binary32")
    main()
