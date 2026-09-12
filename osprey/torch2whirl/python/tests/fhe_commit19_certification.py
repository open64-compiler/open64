"""Independently certify the focused SecureResNet20 SYNC-3 checkpoint."""

from __future__ import annotations

import argparse
import hashlib
import json
import math
from pathlib import Path
import re
import struct


def _sha256(path: Path) -> str:
    return hashlib.sha256(path.read_bytes()).hexdigest()


def _read_safetensors(path: Path) -> dict[str, tuple[list[int], bytes]]:
    data = path.read_bytes()
    if len(data) < 8:
        raise AssertionError(f"truncated SafeTensors file: {path}")
    header_size = struct.unpack("<Q", data[:8])[0]
    header_end = 8 + header_size
    header = json.loads(data[8:header_end].decode("utf-8"))
    tensors: dict[str, tuple[list[int], bytes]] = {}
    for name, record in header.items():
        if name == "__metadata__":
            continue
        if record["dtype"] != "F32":
            raise AssertionError(f"unexpected tensor dtype for {name}")
        begin, end = record["data_offsets"]
        raw = data[header_end + begin:header_end + end]
        shape = record["shape"]
        if len(raw) != math.prod(shape) * 4:
            raise AssertionError(f"unexpected tensor byte size for {name}")
        tensors[name] = (shape, raw)
    return tensors


def _float_values(tensor: tuple[list[int], bytes]) -> tuple[float, ...]:
    return tuple(item[0] for item in struct.iter_unpack("<f", tensor[1]))


def _check_pair(
    source: dict[str, tuple[list[int], bytes]],
    converted: dict[str, tuple[list[int], bytes]],
    output_prefix: str,
    conv_prefix: str,
    bn_prefix: str,
) -> None:
    weight_shape, _ = source[f"{conv_prefix}.weight"]
    weight = _float_values(source[f"{conv_prefix}.weight"])
    output_channels = weight_shape[0]
    channel_stride = len(weight) // output_channels
    conv_bias = (
        _float_values(source[f"{conv_prefix}.bias"])
        if f"{conv_prefix}.bias" in source
        else (0.0,) * output_channels
    )
    bn_weight = _float_values(source[f"{bn_prefix}.weight"])
    bn_bias = _float_values(source[f"{bn_prefix}.bias"])
    running_mean = _float_values(source[f"{bn_prefix}.running_mean"])
    running_var = _float_values(source[f"{bn_prefix}.running_var"])
    factors = tuple(
        bn_weight[channel] / math.sqrt(running_var[channel] + 1.0e-5)
        for channel in range(output_channels)
    )
    expected_weight = bytearray()
    for channel in range(output_channels):
        start = channel * channel_stride
        for value in weight[start:start + channel_stride]:
            expected_weight.extend(struct.pack("<f", value * factors[channel]))
    expected_bias = b"".join(
        struct.pack(
            "<f",
            bn_bias[channel] +
            (conv_bias[channel] - running_mean[channel]) * factors[channel],
        )
        for channel in range(output_channels)
    )
    converted_weight = converted[f"{output_prefix}_folded_weight"]
    converted_bias = converted[f"{output_prefix}_folded_bias"]
    if converted_weight[0] != weight_shape or converted_weight[1] != expected_weight:
        raise AssertionError(f"folded weight mismatch: {output_prefix}")
    if converted_bias[0] != [output_channels] or converted_bias[1] != expected_bias:
        raise AssertionError(f"folded bias mismatch: {output_prefix}")


def _section(text: str, heading: str, next_heading: str) -> str:
    try:
        return text.split(heading, 1)[1].split(next_heading, 1)[0]
    except IndexError as error:
        raise AssertionError(f"missing trace section: {heading}") from error


def _check_bn_payload(
    artifact_dir: Path, checkpoint: Path, range_manifest: dict
) -> None:
    authority = range_manifest["calibration_authority"]
    source_artifact = range_manifest["source_artifact"]
    if _sha256(checkpoint) != authority["trained_checkpoint_sha256"]:
        raise AssertionError("trained checkpoint hash differs from manifest")
    if _sha256(artifact_dir / "secure_resnet20.py") != \
            source_artifact["model_source_sha256"]:
        raise AssertionError("captured model source hash differs from manifest")
    if _sha256(artifact_dir / "secure_resnet20.B") != \
            source_artifact["binary_whirl_sha256"]:
        raise AssertionError("source binary WHIRL hash differs from manifest")
    if _sha256(artifact_dir / "secure_resnet20.safetensors") != \
            source_artifact["parameter_payload_sha256"]:
        raise AssertionError("source parameter payload hash differs from manifest")
    source = _read_safetensors(artifact_dir / "secure_resnet20.safetensors")
    converted = _read_safetensors(
        artifact_dir / "secure_resnet20.fhe.safetensors"
    )
    _check_pair(source, converted, "entry_stem_conv", "conv1", "bn1")
    block_paths = (
        "layer1.0", "layer1.1", "layer1.2",
        "layer2.0", "layer2.1", "layer2.2",
        "layer3.0", "layer3.1", "layer3.2",
    )
    for callsite, path in enumerate(block_paths, 1):
        prefix = f"call{callsite}_cnn_basic_block"
        _check_pair(
            source, converted, f"{prefix}_conv1",
            f"{path}.conv1", f"{path}.bn1",
        )
        _check_pair(
            source, converted, f"{prefix}_conv2",
            f"{path}.conv2", f"{path}.bn2",
        )
        if f"{path}.downsample.0.weight" in source:
            _check_pair(
                source,
                converted,
                f"{prefix}_downsample_conv",
                f"{path}.downsample.0",
                f"{path}.downsample.1",
            )
    if len(converted) != 42:
        raise AssertionError(f"expected 42 folded tensors, found {len(converted)}")


def _check_relu_evidence(
    text: str, range_manifest: dict, ckks_manifest: dict, digest: str
) -> None:
    identity_text = _section(
        text, "DSL PU Source Identity Table:", "DSL Callsite Metadata Table:"
    )
    identity_owner = {
        int(identity): owner
        for identity, owner in re.findall(
            r"^  \[(\d+)\] owner_pu=(<\d+,\d+>) ",
            identity_text,
            re.MULTILINE,
        )
    }

    range_text = _section(
        text,
        "FHE ReLU Context Range Table:",
        "FHE Context CKKS State Image:",
    )
    range_rows = re.findall(
        r"^  \[\d+\] profile=1 source_relu=value(\d+)\([^\n]+?\) "
        r"owner_pu=\S+ context_identity=(\d+) callsite=(\d+) "
        r"positive_bound=\S+ observed=\[[^\]]+\] out_of_range=reject "
        r"provenance=calibration_manifest_sha256=([0-9a-f]{64});"
        r"instance_path=(\S+) flags=0x1$",
        range_text,
        re.MULTILINE,
    )
    if len(range_rows) != 19:
        raise AssertionError(f"expected 19 ReLU ranges, found {len(range_rows)}")

    state_text = _section(
        text,
        "FHE Context CKKS State Table:",
        "------------ INCLUDE_DIRECTORIES ------------",
    )
    state_rows = re.findall(
        r"^  \[\d+\] owner_pu=\S+ source=value(\d+)\([^\n]+?\) "
        r"context_identity=(\d+)\([^\n]+?\) callsite=(\d+) "
        r"role=post_refresh state_version=1 encryption=\d+ scheme=ckks "
        r"class=ciphertext level=(\d+) scale_bits=56 components=2 "
        r"precision_bits=30 slots=32768 alignment_group=0 "
        r"layout=ckks\.packed pending=0x4 "
        r"bootstrap_reason=pre_relu_refresh flags=0x0$",
        state_text,
        re.MULTILINE,
    )
    if len(state_rows) != 19:
        raise AssertionError(
            f"expected 19 POST_REFRESH states, found {len(state_rows)}"
        )

    expected_ranges = {
        (
            context["source_relu_value_id"],
            context["context_pu_identity_id"],
            context["context_callsite_id"],
            context["instance_path"],
        )
        for context in range_manifest["contexts"]
    }
    observed_ranges = {
        (int(value), int(identity), int(callsite), instance)
        for value, identity, callsite, row_digest, instance in range_rows
        if row_digest == digest
    }
    if observed_ranges != expected_ranges:
        raise AssertionError("range manifest and WHIRL context identities differ")
    for context in range_manifest["contexts"]:
        identity = context["context_pu_identity_id"]
        if identity_owner.get(identity) != context["owner_pu_st"]:
            raise AssertionError("context identity is not callee/source owned")

    levels = {
        context["instance_path"]: context["post_refresh_level"]
        for context in ckks_manifest["contexts"]
    }
    state_keys = {
        (int(value), int(identity), int(callsite), int(level))
        for value, identity, callsite, level in state_rows
    }
    expected_states = {
        (
            context["source_relu_value_id"],
            context["context_pu_identity_id"],
            context["context_callsite_id"],
            levels[context["instance_path"]],
        )
        for context in range_manifest["contexts"]
    }
    if state_keys != expected_states:
        raise AssertionError("CKKS context-state schedule differs from manifest")


def certify(artifact_dir: Path, policy_dir: Path, checkpoint: Path) -> None:
    required = (
        "secure_resnet20.py",
        "secure_resnet20.B",
        "secure_resnet20.T",
        "secure_resnet20.safetensors",
        "secure_resnet20.fhe.B",
        "secure_resnet20.fhe.T",
        "secure_resnet20.fhe.safetensors",
        "secure_resnet20.fhe.conversion-report.txt",
        "conversion.log",
    )
    for name in required:
        if not (artifact_dir / name).is_file():
            raise AssertionError(f"missing Commit 19 artifact: {name}")

    range_path = policy_dir / "range-manifest.json"
    range_manifest = json.loads(range_path.read_text(encoding="ascii"))
    ckks_manifest = json.loads(
        (policy_dir / "ckks-schedule-manifest.json").read_text(encoding="ascii")
    )
    range_digest = _sha256(range_path)
    _check_bn_payload(artifact_dir, checkpoint, range_manifest)

    text = (artifact_dir / "secure_resnet20.fhe.T").read_text(
        encoding="utf-8", errors="replace"
    )
    report = (
        artifact_dir / "secure_resnet20.fhe.conversion-report.txt"
    ).read_text(encoding="ascii")
    log = (artifact_dir / "conversion.log").read_text(encoding="ascii")
    required_report = (
        "source_dispositions=46",
        "converted_dispositions=46",
        "rewritten_values=13",
        "folded_batch_norm=21",
        "context_folds=21",
        "converted_tensors=42",
        "relu_calibration=approved",
        f"relu_calibration_external_sha256={range_digest}",
        "relu_calibration_contexts=19",
    )
    if any(evidence not in report for evidence in required_report):
        raise AssertionError("conversion report is incomplete")
    required_log = (
        "FHE-RELU-CALIBRATION: contexts=19 states=19",
        "FHE-BN-CHECKPOINT: definitions=13 contexts=21 tensors=42",
        "pu=6 semantic_gates=12 passes=6 source=46 converted=46",
        "rewritten=13 batch_norm=21 approximations=1 errors=0",
    )
    if any(evidence not in log for evidence in required_log):
        raise AssertionError("checkpoint diagnostic summary is incomplete")

    if len(re.findall(r"^FUNC_ENTRY", text, re.MULTILINE)) != 6:
        raise AssertionError("converted checkpoint does not contain six PUs")
    if len(re.findall(r"^ VCALL ", text, re.MULTILINE)) != 9:
        raise AssertionError("converted checkpoint does not contain nine calls")
    if text.count("status=retired redirected_to=") != 13:
        raise AssertionError("exactly 13 BatchNorm values must be retired")
    if text.count("disposition=require_composite_approximation") != 11:
        raise AssertionError("all 11 reusable ReLU definitions need dispositions")
    if text.count("dsl.converted_from_value_id = ") != 42:
        raise AssertionError("converted tensor provenance is not exactly 42")
    for forbidden in (
        "OPR_DSLBOOTSTRAP", "OPR_DSLCKKS", "OPR_DSLSIHE", "dsc_fhe_"
    ):
        if forbidden in text:
            raise AssertionError(f"SYNC-4/runtime lowering leaked into SYNC-3: {forbidden}")

    stage_text = _section(
        text,
        "FHE Ordered Approximation Stage Table:",
        "FHE Composite Approximation Association Table:",
    )
    if re.findall(r" degree=(\d+) ", stage_text) != ["7", "15", "13"]:
        raise AssertionError("composite stage order is not 7,15,13")
    if sum(int(value) for value in re.findall(
        r" level_consumption=(\d+) ", stage_text
    )) != 11:
        raise AssertionError("composite stage depth is not 11")
    association_text = _section(
        text,
        "FHE Composite Approximation Association Table:",
        "FHE ReLU Context Range Table:",
    )
    if len(re.findall(r"^  \[\d+\] disposition=", association_text,
                      re.MULTILINE)) != 11:
        raise AssertionError("composite profile association count is not 11")
    _check_relu_evidence(text, range_manifest, ckks_manifest, range_digest)


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("artifact_dir", type=Path)
    parser.add_argument("--policy-dir", type=Path, required=True)
    parser.add_argument("--checkpoint", type=Path, required=True)
    args = parser.parse_args()
    certify(args.artifact_dir, args.policy_dir, args.checkpoint)
    print(f"certified SecureResNet20 SYNC-3 artifacts: {args.artifact_dir}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
