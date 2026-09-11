"""Collect identity-bound SecureResNet20 pre-ReLU calibration ranges."""

from __future__ import annotations

import argparse
import importlib.util
import json
from pathlib import Path
import sys
from typing import Optional, Sequence

import numpy
import torch

from open64_dsc.fhe_relu_calibration import (
    CalibrationSample,
    IdentityBoundReluCalibrationCollector,
    canonical_json_bytes,
    file_sha256,
    validate_calibration_manifest,
)


def _load_model(source: Path, checkpoint: Path):
    spec = importlib.util.spec_from_file_location(
        "secure_resnet20_calibration", source
    )
    if spec is None or spec.loader is None:
        raise RuntimeError(f"failed to load model source: {source}")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    model = module.create_model()
    try:
        stored = torch.load(checkpoint, map_location="cpu", weights_only=True)
    except TypeError:
        stored = torch.load(checkpoint, map_location="cpu")
    state = stored.get("state_dict", stored) if isinstance(stored, dict) else stored
    if not isinstance(state, dict):
        raise RuntimeError("checkpoint does not contain a state dictionary")
    model.load_state_dict(state, strict=True)
    return model.eval()


def _load_samples(path: Path) -> list[CalibrationSample]:
    with numpy.load(path, allow_pickle=False) as archive:
        if set(archive.files) != {"inputs", "sample_ids"}:
            raise RuntimeError(
                "sample archive must contain exactly inputs and sample_ids"
            )
        inputs = archive["inputs"]
        sample_ids = archive["sample_ids"]
        if inputs.dtype != numpy.float32 or inputs.ndim != 4 or \
                tuple(inputs.shape[1:]) != (3, 32, 32):
            raise RuntimeError("sample inputs must be float32 NCHW CIFAR tensors")
        if len(inputs) != len(sample_ids) or len(inputs) == 0:
            raise RuntimeError("sample IDs and inputs are incomplete")
        return [
            CalibrationSample(
                str(sample_id),
                (torch.from_numpy(inputs[index:index + 1].copy()),),
            )
            for index, sample_id in enumerate(sample_ids.tolist())
        ]


def _load_json(path: Path) -> dict:
    with path.open("r", encoding="ascii") as stream:
        value = json.load(stream)
    if not isinstance(value, dict):
        raise RuntimeError(f"expected a JSON object: {path}")
    return value


def _parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--model-source", type=Path, required=True)
    parser.add_argument("--checkpoint", type=Path, required=True)
    parser.add_argument("--checkpoint-identity", required=True)
    parser.add_argument("--samples", type=Path, required=True)
    parser.add_argument("--protocol", type=Path)
    parser.add_argument("--dataset-name", required=True)
    parser.add_argument("--dataset-version", required=True)
    parser.add_argument("--dataset-split", required=True)
    parser.add_argument("--sample-selection", required=True)
    parser.add_argument("--sample-seed", type=int, required=True)
    parser.add_argument("--preprocessing", type=Path, required=True)
    parser.add_argument("--identity-template", type=Path, required=True)
    parser.add_argument("--coefficient-manifest", type=Path, required=True)
    parser.add_argument("--binary-whirl", type=Path, required=True)
    parser.add_argument("--parameter-payload", type=Path, required=True)
    parser.add_argument("--safety-factor", type=float, default=1.0)
    parser.add_argument("--approved-by", default="")
    parser.add_argument("--output", type=Path, required=True)
    return parser


def main(argv: Optional[Sequence[str]] = None) -> int:
    args = _parser().parse_args(argv)
    template_manifest = _load_json(args.identity_template)
    coefficient_manifest = _load_json(args.coefficient_manifest)
    contexts = template_manifest.get("contexts")
    if not isinstance(contexts, list):
        raise RuntimeError("identity template lacks contexts")
    samples = _load_samples(args.samples)
    preprocessing_text = args.preprocessing.read_text(encoding="utf-8")
    protocol = _load_json(args.protocol) if args.protocol is not None else None
    if protocol is not None:
        if protocol.get("status") != "predeclared_before_acceptance":
            raise RuntimeError("calibration protocol is not predeclared")
        if protocol.get("authority", {}).get("open64_checkpoint_sha256") != \
                file_sha256(args.checkpoint):
            raise RuntimeError("calibration checkpoint differs from protocol")
        if protocol.get("preprocessing", {}).get("sha256") != \
                file_sha256(args.preprocessing):
            raise RuntimeError("calibration preprocessing differs from protocol")
        calibration = protocol.get("calibration", {})
        if calibration.get("archive_sha256") != file_sha256(args.samples):
            raise RuntimeError("calibration sample archive differs from protocol")
        if calibration.get("sample_ids") != [sample.sample_id for sample in samples]:
            raise RuntimeError("calibration sample order differs from protocol")
    dataset = protocol.get("dataset", {}) if protocol is not None else {}
    calibration = protocol.get("calibration", {}) if protocol is not None else {}
    authority = {
        "dataset_name": args.dataset_name,
        "dataset_version": args.dataset_version,
        "dataset_split": args.dataset_split,
        "dataset_files": dataset.get("files", [args.samples.name]),
        "dataset_sha256": dataset.get(
            "canonical_files_sha256", file_sha256(args.samples)
        ),
        "sample_selection": calibration.get(
            "selection", args.sample_selection
        ),
        "sample_seed": args.sample_seed,
        "preprocessing": preprocessing_text,
        "preprocessing_sha256": file_sha256(args.preprocessing),
        "trained_checkpoint_identity": args.checkpoint_identity,
        "trained_checkpoint_sha256": file_sha256(args.checkpoint),
        "acceptance_data_role": "disjoint_held_out",
        "out_of_range_policy": "reject",
        "approval_identity": args.approved_by or None,
        "calibration_protocol_sha256": (
            file_sha256(args.protocol) if args.protocol is not None else None
        ),
        "calibration_sample_archive_sha256": file_sha256(args.samples),
        "ace_onnx_sha256": (
            protocol.get("authority", {}).get("ace_onnx_sha256")
            if protocol is not None else None
        ),
        "training_provenance": (
            protocol.get("authority", {}).get("training_provenance")
            if protocol is not None else None
        ),
    }
    collector = IdentityBoundReluCalibrationCollector(
        _load_model(args.model_source, args.checkpoint),
        contexts,
        safety_factor=args.safety_factor,
    )
    manifest = collector.collect(
        samples,
        profile_name=coefficient_manifest["profile"]["name"],
        coefficient_manifest_sha256=file_sha256(args.coefficient_manifest),
        calibration_authority=authority,
        source_artifact={
            "binary_whirl_sha256": file_sha256(args.binary_whirl),
            "model_source_sha256": file_sha256(args.model_source),
            "parameter_payload_sha256": file_sha256(args.parameter_payload),
        },
        status="approved" if args.approved_by else "candidate_unapproved",
    )
    validate_calibration_manifest(
        manifest,
        expected_contexts=contexts,
        require_approved=bool(args.approved_by),
    )
    args.output.parent.mkdir(parents=True, exist_ok=True)
    args.output.write_bytes(canonical_json_bytes(manifest))
    print(f"wrote {len(contexts)} identity-bound ranges to {args.output}")
    if not args.approved_by:
        print("manifest remains candidate_unapproved; native binding is disabled")
    return 0


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except (OSError, RuntimeError, ValueError) as error:
        print(f"CFHECNN-RELU-003: {error}", file=sys.stderr)
        raise SystemExit(1)
