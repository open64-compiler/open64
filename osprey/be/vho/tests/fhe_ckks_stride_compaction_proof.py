#!/usr/bin/env python3
"""Read-only local stride compaction proof for the S6-0c C2 decision.

This models plaintext masks, ciphertext rotations, and adds on clear slots;
it creates no WHIRL or CKKS runtime operation. Design:
doc/FHE-SYNC6-S6-0C-DETAILED-EXECUTION-PLAN.md.
"""

import argparse
from array import array
import hashlib
import json
import math
import struct
from pathlib import Path

from fhe_ckks_conv_payload_fixture import digest, tensor_bytes


def mask_hash(indices, slots):
    """Hash the exact slot-order 0/1 bytes of one plaintext mask."""
    mask = bytearray(slots)
    for index in indices:
        mask[index] = 1
    return hashlib.sha256(mask).hexdigest()


def bit_moves(width, channels):
    """Return the ordered slot-bit moves used by the clear compaction proof."""
    spatial_bits = int(math.log(width, 2))
    channel_bits = int(math.log(channels, 2))
    return (
        [(1 + bit, bit) for bit in range(spatial_bits - 1)] +
        [(spatial_bits + 1 + bit, spatial_bits - 1 + bit)
         for bit in range(spatial_bits - 1)] +
        [(2 * spatial_bits + bit, 2 * spatial_bits - 2 + bit)
         for bit in range(channel_bits)]
    )


def active_mapping(width, channels):
    """Map every selected high-resolution slot to its initial slot."""
    half = width // 2
    return {
        oc * width * width + 2 * y * width + 2 * x:
        oc * width * width + 2 * y * width + 2 * x
        for oc in range(channels) for y in range(half) for x in range(half)
    }


def expected_mapping(width, original_indices):
    """Compute dense NCHW positions independently of the bit-move network."""
    half = width // 2
    return {
        original: oc * half * half + y * half + x
        for original in original_indices
        for oc, y, x in [(
            original // (width * width),
            (original % (width * width)) // width // 2,
            (original % width) // 2)]
    }


def compact(highres, width, channels, slots):
    """Delete the two spatial low bits with a bitwise mask/rotate network."""
    assert width >= 4 and width & (width - 1) == 0
    assert channels >= 1 and channels & (channels - 1) == 0
    assert channels * width * width <= slots == len(highres)
    half = width // 2
    mapping = active_mapping(width, channels)
    active = set(mapping.values())
    current = [value if index in active else 0.0
               for index, value in enumerate(highres)]
    stages = []
    for ordinal, (source_bit, target_bit) in enumerate(
            bit_moves(width, channels)):
        rotation = (1 << source_bit) - (1 << target_bit)
        selected_indices = {
            current_index for original, current_index in mapping.items()
            if original & (1 << source_bit)
        }
        assert selected_indices <= active
        selected = [value if index in selected_indices else 0.0
                    for index, value in enumerate(current)]
        other = [value if index not in selected_indices else 0.0
                 for index, value in enumerate(current)]
        moved = [selected[(index + rotation) % slots]
                 for index in range(slots)]
        next_mapping = {
            original: current_index - rotation
            if current_index in selected_indices else current_index
            for original, current_index in mapping.items()
        }
        assert all(0 <= index < slots for index in next_mapping.values())
        assert len(set(next_mapping.values())) == len(next_mapping)
        current = [left + right for left, right in zip(other, moved)]
        mapping = next_mapping
        active = set(mapping.values())
        stages.append({
            "ordinal": ordinal,
            "source_bit": source_bit,
            "target_bit": target_bit,
            "signed_left_rotation": rotation,
            "selected_mask_sha256": mask_hash(selected_indices, slots),
            "selected_mask_ones": len(selected_indices),
            "complement_mask_sha256": mask_hash(
                set(range(slots)) - selected_indices, slots),
            "complement_mask_ones": slots - len(selected_indices),
            "symbolic_level_after_if_mask_consumes_one": -(ordinal + 2),
        })
    expected = expected_mapping(width, mapping)
    assert mapping == expected
    assert active == set(range(channels * half * half))
    return current, {
        "width": width,
        "channels": channels,
        "slot_count": slots,
        "selection_mask_sha256": mask_hash(set(expected), slots),
        "selection_mask_ones": len(expected),
        "stage_count": len(stages),
        "plaintext_mask_count": 1 + 2 * len(stages),
        "unique_signed_rotation_keys": sorted({
            row["signed_left_rotation"] for row in stages}),
        "symbolic_plaintext_mask_depth": 1 + len(stages),
        "stages": stages,
    }


def compact_fused(highres, width, channels, slots, moves_per_stage=5):
    """Compose bounded bit moves into parallel masked-rotation diagonals."""
    assert width >= 4 and width & (width - 1) == 0
    assert channels >= 1 and channels & (channels - 1) == 0
    assert channels * width * width <= slots == len(highres)
    assert 1 <= moves_per_stage <= 7
    mapping = active_mapping(width, channels)
    current = list(highres)
    stages = []
    moves = bit_moves(width, channels)
    stage_count = (len(moves) + moves_per_stage - 1) // moves_per_stage
    base, extra = divmod(len(moves), stage_count)
    stage_sizes = [base] * (stage_count - extra) + [base + 1] * extra
    first = 0
    for stage_size in stage_sizes:
        before = mapping.copy()
        for source_bit, target_bit in moves[first:first + stage_size]:
            rotation = (1 << source_bit) - (1 << target_bit)
            mapping = {
                original: index - rotation
                if original & (1 << source_bit) else index
                for original, index in mapping.items()
            }
        assert len(set(mapping.values())) == len(mapping)
        assert all(0 <= index < slots for index in mapping.values())
        diagonals = {}
        for original, source_index in before.items():
            rotation = source_index - mapping[original]
            diagonals.setdefault(rotation, set()).add(source_index)
        assert len(diagonals) <= 1 << stage_size
        assert sum(len(indices) for indices in diagonals.values()) == \
            len(before)
        assert set().union(*diagonals.values()) == set(before.values())
        next_values = [0.0] * slots
        for rotation, indices in diagonals.items():
            for source_index in indices:
                next_values[(source_index - rotation) % slots] += \
                    current[source_index]
        current = next_values
        stages.append({
            "ordinal": len(stages),
            "first_bit_move": first,
            "bit_move_count": stage_size,
            "diagonal_count": len(diagonals),
            "diagonals": [
                {"signed_left_rotation": rotation,
                 "mask_sha256": mask_hash(indices, slots),
                 "mask_ones": len(indices)}
                for rotation, indices in sorted(diagonals.items())
            ],
        })
        first += stage_size
    expected = expected_mapping(width, mapping)
    assert mapping == expected
    assert all(value == 0.0 for value in current[len(mapping):])
    return current, {
        "stage_count": len(stages),
        "plaintext_mask_count": sum(stage["diagonal_count"]
                                    for stage in stages),
        "unique_signed_rotation_keys": sorted({
            diagonal["signed_left_rotation"]
            for stage in stages for diagonal in stage["diagonals"]
            if diagonal["signed_left_rotation"] != 0}),
        "symbolic_plaintext_mask_depth": len(stages),
        "stages": stages,
    }


def load_folded_projection(payload, replay):
    """Read the replay-authenticated call4 1x1 projection weight and bias."""
    raw = payload.read_bytes()
    expected = json.loads(replay.read_text(encoding="utf-8"))["inputs"][
        "folded_payload"]
    if payload.name != expected["basename"] or \
            len(raw) != expected["size"] or digest(raw) != expected["sha256"]:
        raise ValueError("folded projection payload is not replay-authenticated")
    if len(raw) < 8:
        raise ValueError("truncated SafeTensors header")
    header_length = struct.unpack("<Q", raw[:8])[0]
    if not 0 < header_length <= 16 * 1024 * 1024 or \
            header_length + 8 > len(raw):
        raise ValueError("invalid SafeTensors header")
    index = json.loads(raw[8:8 + header_length])
    if not isinstance(index, dict):
        raise ValueError("invalid SafeTensors index")
    base = 8 + header_length
    prefix = "call4_cnn_basic_block_downsample_conv_folded_"
    weight_bytes = tensor_bytes(raw, index, base, prefix + "weight",
                                [32, 16, 1, 1])
    bias_bytes = tensor_bytes(raw, index, base, prefix + "bias", [32])
    weights = array("f")
    bias = array("f")
    weights.frombytes(weight_bytes)
    bias.frombytes(bias_bytes)
    if any(not math.isfinite(value) for value in weights) or \
            any(not math.isfinite(value) for value in bias):
        raise ValueError("non-finite folded projection tensor")
    return weights, bias, {
        "folded_payload_sha256": expected["sha256"],
        "weight_sha256": digest(weight_bytes),
        "bias_sha256": digest(bias_bytes),
    }


def check_small():
    """Prove 4x4 even-position packing with two masks/rotations stages."""
    highres = [float(index + 1) for index in range(16)]
    actual, network = compact(highres, 4, 1, 16)
    expected = [highres[index] for index in (0, 2, 8, 10)]
    assert actual[:4] == expected and actual[4:] == [0.0] * 12
    assert network["unique_signed_rotation_keys"] == [1, 6]
    assert network["plaintext_mask_count"] == 5
    fused, fused_network = compact_fused(highres, 4, 1, 16)
    assert fused == actual
    assert fused_network["stage_count"] == 1
    assert fused_network["plaintext_mask_count"] <= 4
    return {"sequential": network, "fused": fused_network}


def check_captured(payload, replay):
    """Compare one captured projection against independent stride-two Conv."""
    weights, bias, provenance = load_folded_projection(payload, replay)
    width, channels, input_channels, slots = 32, 32, 16, 32768
    input_values = [float((index * 17) % 43 - 21) / 32.0
                    for index in range(input_channels * width * width)]
    highres = [0.0] * slots
    for oc in range(channels):
        for y in range(width):
            for x in range(width):
                total = float(bias[oc])
                for ci in range(input_channels):
                    total += float(weights[oc * input_channels + ci]) * \
                        input_values[ci * width * width + y * width + x]
                highres[oc * width * width + y * width + x] = total
    actual, network = compact(highres, width, channels, slots)
    fused, fused_network = compact_fused(highres, width, channels, slots)
    assert fused == actual
    shallow, shallow_network = compact_fused(
        highres, width, channels, slots, 7)
    assert shallow == actual
    half = width // 2
    max_error = 0.0
    for oc in range(channels):
        for oy in range(half):
            for ox in range(half):
                expected = float(bias[oc])
                for ci in range(input_channels):
                    source = ci * width * width + 2 * oy * width + 2 * ox
                    expected += float(weights[oc * input_channels + ci]) * \
                        input_values[source]
                index = oc * half * half + oy * half + ox
                max_error = max(max_error, abs(actual[index] - expected))
    assert max_error < 1e-12
    assert all(value == 0.0 for value in actual[channels * half * half:])
    assert network["stage_count"] == 13
    assert network["plaintext_mask_count"] == 27
    assert network["unique_signed_rotation_keys"] == [
        1, 2, 4, 8, 48, 96, 192, 384, 768, 1536, 3072, 6144, 12288]
    network["oracle_max_abs_error"] = max_error
    network["active_output_slots"] = channels * half * half
    network["highres_conv_plaintext_multiply_depth"] = 1
    network["symbolic_total_depth_from_input"] = (
        1 + network["symbolic_plaintext_mask_depth"])
    network["state_compatibility"] = (
        "unproved_scale_precision_and_residual_branch_alignment")
    network["provenance"] = provenance
    fused_network["oracle_max_abs_error"] = max_error
    fused_network["symbolic_total_depth_from_input"] = (
        1 + fused_network["symbolic_plaintext_mask_depth"])
    fused_network["provenance"] = provenance
    shallow_network["oracle_max_abs_error"] = max_error
    shallow_network["symbolic_total_depth_from_input"] = (
        1 + shallow_network["symbolic_plaintext_mask_depth"])
    shallow_network["provenance"] = provenance
    assert fused_network["stage_count"] == 3
    assert fused_network["plaintext_mask_count"] <= 96
    assert shallow_network["stage_count"] == 2
    assert shallow_network["plaintext_mask_count"] <= 192
    return {"sequential": network, "fused": fused_network,
            "fused_depth2": shallow_network}


def check_second_projection_shape():
    """Check the second shape against a deterministic stride-two Conv oracle."""
    width, channels, input_channels, slots = 16, 64, 32, 32768
    input_values = [float((index * 11) % 31 - 15) / 16.0
                    for index in range(input_channels * width * width)]
    weights = [float((index * 7) % 17 - 8) / 32.0
               for index in range(channels * input_channels)]
    bias = [float(index % 9 - 4) / 8.0 for index in range(channels)]
    highres = [0.0] * slots
    for oc in range(channels):
        for y in range(width):
            for x in range(width):
                highres[oc * width * width + y * width + x] = bias[oc] + sum(
                    weights[oc * input_channels + ci] *
                    input_values[ci * width * width + y * width + x]
                    for ci in range(input_channels))
    sequential, sequential_network = compact(highres, width, channels, slots)
    fused, network = compact_fused(highres, width, channels, slots)
    shallow, shallow_network = compact_fused(
        highres, width, channels, slots, 7)
    assert fused == sequential == shallow
    half = width // 2
    max_error = 0.0
    for oc in range(channels):
        for oy in range(half):
            for ox in range(half):
                expected = bias[oc] + sum(
                    weights[oc * input_channels + ci] *
                    input_values[ci * width * width + 2 * oy * width +
                                 2 * ox]
                    for ci in range(input_channels))
                index = oc * half * half + oy * half + ox
                max_error = max(max_error, abs(fused[index] - expected))
    assert max_error == 0.0
    assert network["stage_count"] == 3
    assert network["plaintext_mask_count"] <= 96
    assert shallow_network["stage_count"] == 2
    assert shallow_network["plaintext_mask_count"] <= 128
    network["oracle_max_abs_error"] = max_error
    network["fixture"] = "deterministic synthetic 1x1 projection, no model bytes"
    shallow_network["oracle_max_abs_error"] = max_error
    shallow_network["fixture"] = network["fixture"]
    return {"sequential": sequential_network, "fused": network,
            "fused_depth2": shallow_network}


def check_downsample_level_ledger(path, call4, second):
    """Compare explicit O0 capacity refresh with O1 packing candidates."""
    raw = path.read_bytes()
    schedule = json.loads(raw)
    if schedule.get("schema") != "open64.fhe.relu.ckks-schedule.v1" or \
            schedule.get("status") != "approved_static_compiler_schedule" or \
            schedule.get("total_multiplicative_depth") != 11:
        raise ValueError("unapproved or incompatible CKKS schedule")
    contexts = schedule.get("contexts")
    if not isinstance(contexts, list):
        raise ValueError("missing CKKS context schedule")
    levels = {row["instance_path"]: row for row in contexts}
    if len(levels) != len(contexts):
        raise ValueError("duplicate CKKS context route")
    transitions = (
        ("layer2.0", "layer1.2.relu2", "layer2.0.relu1", call4),
        ("layer3.0", "layer2.2.relu2", "layer3.0.relu1", second),
    )
    rows = []
    for block, predecessor, first_relu, pack in transitions:
        try:
            source_level = levels[predecessor]["final_level"]
            refresh_level = levels[first_relu]["post_refresh_level"]
            first_relu_final = levels[first_relu]["final_level"]
        except KeyError as exc:
            raise ValueError("missing downsample CKKS context") from exc
        o0_pack_depth = pack["sequential"]["symbolic_plaintext_mask_depth"]
        main_post_conv2 = first_relu_final - 1
        capacity_refresh_target = main_post_conv2 + 1 + o0_pack_depth
        projection_level = capacity_refresh_target - 1 - o0_pack_depth
        main_pre_refresh = capacity_refresh_target - 1 - o0_pack_depth
        optimized_pack_depth = pack["fused"]["symbolic_plaintext_mask_depth"]
        if source_level != 7 or refresh_level != 15 or \
                first_relu_final != refresh_level - 11 or \
                projection_level < 0 or \
                projection_level != main_post_conv2 or \
                main_pre_refresh != projection_level:
            raise ValueError("downsample symbolic level join failed")
        rows.append({
            "block": block,
            "predecessor": predecessor,
            "source_level": source_level,
            "o0_proposed_capacity_refresh_reason": "DEPTH_EXHAUSTION",
            "o0_proposed_capacity_refresh_target": capacity_refresh_target,
            "o0_capacity_refresh_contract": "symbolic_only_not_runtime_approved",
            "o0_sequential_pack_depth": o0_pack_depth,
            "main_conv1_before_refresh_level": main_pre_refresh,
            "relu1_post_refresh_level": refresh_level,
            "relu1_post_polynomial_level": first_relu_final,
            "main_conv2_level": main_post_conv2,
            "projection_level": projection_level,
            "projection_pack_depth": o0_pack_depth,
            "o1_fused_pack_depth_without_capacity_refresh":
                optimized_pack_depth,
            "join": "symbolic_level_only_scale_precision_and_keys_unproved",
        })
    return {"manifest_sha256": digest(raw), "blocks": rows}


def main():
    """Retain small and captured proof with exact mask hashes and caveats."""
    parser = argparse.ArgumentParser()
    parser.add_argument("--payload", type=Path, required=True)
    parser.add_argument("--replay", type=Path, required=True)
    parser.add_argument("--ckks-schedule", type=Path)
    parser.add_argument("--output", type=Path, required=True)
    args = parser.parse_args()
    result = {
        "schema": "open64.fhe.sync6.local-stride-compaction-proof.v1",
        "status": "read_only_clear_slot_proof_not_executable_ckks_ir",
        "mask_encoding": "sha256 of slot-order uint8 0-or-1 vector",
        "rotation_convention": "dest[s]=source[(s+r) mod slot_count]",
        "symbolic_state": "sequential branch masks consume one level per "
                          "move; fused parallel diagonal masks consume one "
                          "level per stage if jointly rescaled; component "
                          "count unchanged; exact scale/precision and "
                          "residual-branch alignment unproved",
        "small_4x4_to_2x2": check_small(),
        "captured_call4_1x1_projection": check_captured(
            args.payload, args.replay),
        "second_projection_shape_32x16x16_to_64x8x8":
            check_second_projection_shape(),
    }
    if args.ckks_schedule is not None:
        result["downsample_symbolic_level_ledger"] = \
            check_downsample_level_ledger(
                args.ckks_schedule,
                result["captured_call4_1x1_projection"],
                result["second_projection_shape_32x16x16_to_64x8x8"])
    args.output.parent.mkdir(parents=True, exist_ok=True)
    args.output.write_text(json.dumps(result, indent=2) + "\n",
                           encoding="utf-8")
    print(args.output)


if __name__ == "__main__":
    main()
