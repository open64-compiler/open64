#!/usr/bin/env python3
"""Check all four captured stride-two Convs against clear slot compaction.

This is authenticated, read-only S6-0c C2 evidence, not WHIRL emission or
CKKS execution. Design: doc/FHE-SYNC6-S6-0C-DETAILED-EXECUTION-PLAN.md.
"""

import argparse
import json
import math
import struct
import subprocess
import sys
import tempfile
from pathlib import Path

from fhe_ckks_conv_payload_fixture import digest, tensor_bytes
from fhe_ckks_stride_compaction_proof import compact


EXPECTED_CONTEXTS = {
    (4, 1, 16, 32, 32),
    (4, 3, 16, 32, 32),
    (7, 1, 32, 64, 16),
    (7, 3, 32, 64, 16),
}


def require(condition, message):
    """Reject incomplete or ambiguous fixture evidence before publication."""
    if not condition:
        raise ValueError(message)


def load_inputs(budget_path, trace_path, payload_path, replay_path):
    """Authenticate the report and exact folded SafeTensors family."""
    replay = json.loads(replay_path.read_text(encoding="utf-8"))
    budget = json.loads(budget_path.read_text(encoding="utf-8"))
    require(digest(trace_path.read_bytes()) ==
            replay["inputs"]["trace"]["sha256"],
            "source trace differs from authenticated replay")
    raw = payload_path.read_bytes()
    expected = replay["inputs"]["folded_payload"]
    require(payload_path.name == expected["basename"] and
            len(raw) == expected["size"] and
            digest(raw) == expected["sha256"],
            "folded payload differs from authenticated replay")
    require(budget.get("schema") ==
            "open64.fhe.sync6.conv-plaintext-budget.v2" and
            budget.get("source_trace_sha256") ==
            replay["inputs"]["trace"]["sha256"] and
            budget.get("folded_payload_sha256") == expected["sha256"] and
            budget.get("physical_conv_definitions") == 13 and
            budget.get("folded_call_contexts") == 21 and
            budget.get("excluded_stride_two_contexts") == 4 and
            budget.get("slot_count") == 32768,
            "Conv budget is not bound to the certified source family")
    with tempfile.TemporaryDirectory(prefix="fhe-stride-budget-") as temp:
        regenerated_path = Path(temp) / "budget.json"
        subprocess.run([
            sys.executable,
            str(Path(__file__).with_name("fhe_ckks_conv_mask_budget.py")),
            "--trace", str(trace_path), "--payload", str(payload_path),
            "--replay", str(replay_path), "--output", str(regenerated_path),
        ], check=True, capture_output=True, text=True)
        regenerated = json.loads(
            regenerated_path.read_text(encoding="utf-8"))
    require({key: value for key, value in budget.items()
             if key != "status"} ==
            {key: value for key, value in regenerated.items()
             if key != "status"},
            "Conv budget differs from authenticated source reconstruction")
    require(len(raw) >= 8, "truncated SafeTensors header")
    header_size = struct.unpack("<Q", raw[:8])[0]
    require(0 < header_size <= 16 * 1024 * 1024 and
            header_size + 8 <= len(raw), "invalid SafeTensors header")
    index = json.loads(raw[8:8 + header_size])
    require(isinstance(index, dict), "invalid SafeTensors index")
    return replay, budget, raw, index, header_size + 8


def find_folded_pair(row, raw, index, base):
    """Join one source-bound Conv identity to exactly one tensor pair."""
    weights = []
    for name in index:
        if not name.endswith("_weight"):
            continue
        bias_name = name[:-6] + "bias"
        if bias_name not in index:
            continue
        try:
            weight = tensor_bytes(raw, index, base, name,
                                  row["weight_shape"])
            bias = tensor_bytes(raw, index, base, bias_name,
                                [row["output_shape"][1]])
        except ValueError:
            continue
        if digest(weight) == row["weight_sha256"] and \
                digest(bias) == row["bias_sha256"]:
            weights.append((name, weight, bias))
    require(len(weights) == 1, "folded Conv tensor identity is ambiguous")
    name, weight, bias = weights[0]
    return (name, [value for (value,) in struct.iter_unpack("<f", weight)],
            [value for (value,) in struct.iter_unpack("<f", bias)])


def convolution_at(values, weights, bias, ci_count, width, kernel,
                   oc, y, x):
    """Evaluate one high-resolution OIHW output in direct NCHW order."""
    total = bias[oc]
    pad = kernel // 2
    for ci in range(ci_count):
        for ky in range(kernel):
            iy = y + ky - pad
            if iy < 0 or iy >= width:
                continue
            for kx in range(kernel):
                ix = x + kx - pad
                if ix < 0 or ix >= width:
                    continue
                weight_index = (((oc * ci_count + ci) * kernel + ky) *
                                kernel + kx)
                total += (weights[weight_index] *
                          values[ci * width * width + iy * width + ix])
    return total


def direct_stride_two(values, weights, bias, ci_count, width, kernel,
                      oc, output_y, output_x):
    """Evaluate source stride-two coordinates without the high-res helper."""
    pad = kernel // 2
    products = []
    for ky in range(kernel):
        source_y = 2 * output_y + ky - pad
        if source_y < 0 or source_y >= width:
            continue
        for kx in range(kernel):
            source_x = 2 * output_x + kx - pad
            if source_x < 0 or source_x >= width:
                continue
            for ci in range(ci_count):
                parameter = (((oc * ci_count + ci) * kernel + ky) *
                             kernel + kx)
                source_slot = ci * width * width + source_y * width + source_x
                products.append(weights[parameter] * values[source_slot])
    return math.fsum([bias[oc]] + products)


def certify_context(row, raw, index, base, slots):
    """Compare high-resolution Conv plus compaction with direct stride two."""
    input_shape = row["input_shape"]
    output_shape = row["output_shape"]
    weight_shape = row["weight_shape"]
    ci_count, width = input_shape[1], input_shape[2]
    oc_count, kernel = output_shape[1], weight_shape[2]
    require(input_shape == [1, ci_count, width, width] and
            output_shape == [1, oc_count, width // 2, width // 2] and
            weight_shape == [oc_count, ci_count, kernel, kernel] and
            row["stride"] == [2, 2] and kernel in (1, 3) and
            oc_count * width * width <= slots and
            row["bounded_stride_one_recipe"] is False,
            "stride-two shape or packing differs from the local proof")
    tensor_name, weights, bias = find_folded_pair(row, raw, index, base)
    require(all(math.isfinite(value) for value in weights + bias),
            "non-finite folded Conv parameter")
    source = [float((i * 17) % 43 - 21) / 32.0
              for i in range(ci_count * width * width)]
    highres = [0.0] * slots
    for oc in range(oc_count):
        for y in range(width):
            for x in range(width):
                highres[oc * width * width + y * width + x] = \
                    convolution_at(source, weights, bias, ci_count, width,
                                   kernel, oc, y, x)
    packed, network = compact(highres, width, oc_count, slots)
    half = width // 2
    max_error = 0.0
    for oc in range(oc_count):
        for y in range(half):
            for x in range(half):
                expected = direct_stride_two(
                    source, weights, bias, ci_count, width, kernel,
                    oc, y, x)
                offset = oc * half * half + y * half + x
                max_error = max(max_error, abs(packed[offset] - expected))
    require(max_error <= 1e-10 and
            all(value == 0.0 for value in packed[oc_count * half * half:]),
            "clear stride-two Conv and packed output disagree")
    return {
        "fold_id": row["fold_id"],
        "owner_pu": row["owner_pu"],
        "conv_node": row["conv_node"],
        "source_value": row["source_value"],
        "context": row["context"],
        "context_callsite_id": row["callsite"],
        "weight_tcon": row["weight_tcon"],
        "bias_tcon": row["bias_tcon"],
        "kernel": kernel,
        "input_shape": input_shape,
        "output_shape": output_shape,
        "tensor_name": tensor_name,
        "weight_sha256": row["weight_sha256"],
        "bias_sha256": row["bias_sha256"],
        "active_output_slots": oc_count * half * half,
        "max_abs_error": max_error,
        "sequential_mask_depth": network["symbolic_plaintext_mask_depth"],
        "sequential_mask_count": network["plaintext_mask_count"],
        "signed_rotation_keys": network["unique_signed_rotation_keys"],
    }


def check_symbolic_level_join(schedule_path, contexts):
    """Join both downsample branches to the approved static ReLU schedule."""
    raw = schedule_path.read_bytes()
    schedule = json.loads(raw)
    require(schedule.get("schema") == "open64.fhe.relu.ckks-schedule.v1" and
            schedule.get("status") == "approved_static_compiler_schedule" and
            schedule.get("total_multiplicative_depth") == 11,
            "unapproved or incompatible CKKS schedule")
    rows = schedule.get("contexts")
    require(isinstance(rows, list) and len(rows) == 19,
            "CKKS schedule lacks 19 contexts")
    by_route = {row["instance_path"]: row for row in rows}
    require(len(by_route) == len(rows), "duplicate CKKS route")
    joins = []
    for callsite, block, predecessor, first_relu in (
            (4, "layer2.0", "layer1.2.relu2", "layer2.0.relu1"),
            (7, "layer3.0", "layer2.2.relu2", "layer3.0.relu1")):
        pair = [row for row in contexts
                if row["context_callsite_id"] == callsite]
        require(len(pair) == 2 and {row["kernel"] for row in pair} == {1, 3},
                "downsample branches are incomplete")
        depths = {row["sequential_mask_depth"] for row in pair}
        require(len(depths) == 1, "normal and projection packing diverge")
        depth = depths.pop()
        require(predecessor in by_route and first_relu in by_route,
                "downsample source schedule route is absent")
        source_level = by_route[predecessor]["final_level"]
        refresh_level = by_route[first_relu]["post_refresh_level"]
        first_relu_final = by_route[first_relu]["final_level"]
        target = first_relu_final + depth
        final_level = target - 1 - depth
        require(source_level == 7 and refresh_level == 15 and
                first_relu_final == 4 and final_level == 3 and
                target == (18 if callsite == 4 else 17),
                "downsample symbolic level join disagrees")
        joins.append({
            "block": block,
            "callsite_id": callsite,
            "source_level": source_level,
            "capacity_refresh_reason": "DEPTH_EXHAUSTION",
            "proposed_refresh_target": target,
            "highres_conv_plaintext_multiply_depth": 1,
            "sequential_pack_depth_per_branch": depth,
            "normal_result_level": final_level,
            "projection_result_level": final_level,
            "relu1_post_refresh_level": refresh_level,
            "relu1_post_polynomial_level": first_relu_final,
        })
    return {
        "schedule_sha256": digest(raw),
        "status": "symbolic_only_scale_precision_keys_and_runtime_unproved",
        "blocks": joins,
    }


def main():
    """Publish deterministic read-only evidence only after all four pass."""
    parser = argparse.ArgumentParser()
    parser.add_argument("--budget", type=Path, required=True)
    parser.add_argument("--trace", type=Path, required=True)
    parser.add_argument("--payload", type=Path, required=True)
    parser.add_argument("--replay", type=Path, required=True)
    parser.add_argument("--ckks-schedule", type=Path, required=True)
    parser.add_argument("--output", type=Path, required=True)
    args = parser.parse_args()
    replay, budget, raw, index, base = load_inputs(
        args.budget, args.trace, args.payload, args.replay)
    contexts = [row for row in budget["contexts"]
                if not row["bounded_stride_one_recipe"]]
    identities = {(row["callsite"], row["weight_shape"][2],
                   row["input_shape"][1], row["output_shape"][1],
                   row["input_shape"][2]) for row in contexts}
    require(len(contexts) == 4 and identities == EXPECTED_CONTEXTS and
            len({row["fold_id"] for row in contexts}) == 4,
            "missing, duplicate, or unknown stride-two source context")
    results = [certify_context(row, raw, index, base, budget["slot_count"])
               for row in sorted(contexts, key=lambda item: item["fold_id"])]
    report = {
        "schema": "open64.fhe.sync6.stride-two-four-context-proof.v1",
        "status": "read_only_clear_slot_proof_no_whirl_or_runtime",
        "source_binary_sha256": replay["inputs"]["binary"]["sha256"],
        "source_trace_sha256": budget["source_trace_sha256"],
        "source_budget_sha256": digest(args.budget.read_bytes()),
        "folded_payload_sha256": budget["folded_payload_sha256"],
        "context_count": len(results),
        "contexts": results,
        "symbolic_level_join": check_symbolic_level_join(
            args.ckks_schedule, results),
    }
    args.output.parent.mkdir(parents=True, exist_ok=True)
    args.output.write_text(json.dumps(report, indent=2) + "\n",
                           encoding="utf-8")
    print(args.output)


if __name__ == "__main__":
    main()
