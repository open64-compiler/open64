"""Freeze the CIFAR-10 calibration and acceptance protocol for FHE Commit 19."""

from __future__ import annotations

import argparse
import hashlib
import json
from pathlib import Path
import sys
from typing import Optional, Sequence

import numpy


CIFAR_ARCHIVE_MD5 = "c32a1d4ab5d03f1284b67883e8d87530"
CIFAR_ARCHIVE_URL = "https://www.cs.toronto.edu/~kriz/cifar-10-binary.tar.gz"
MEAN = numpy.asarray((0.485, 0.456, 0.406), dtype=numpy.float32)
STD = numpy.asarray((0.229, 0.224, 0.225), dtype=numpy.float32)


def file_sha256(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as stream:
        for chunk in iter(lambda: stream.read(1024 * 1024), b""):
            digest.update(chunk)
    return digest.hexdigest()


def canonical_json_bytes(value: object) -> bytes:
    return (
        json.dumps(value, sort_keys=True, separators=(",", ":"), ensure_ascii=True)
        + "\n"
    ).encode("ascii")


def _read_batch(path: Path) -> tuple[numpy.ndarray, numpy.ndarray]:
    raw = numpy.fromfile(path, dtype=numpy.uint8)
    if raw.size == 0 or raw.size % 3073 != 0:
        raise RuntimeError(f"invalid CIFAR-10 binary batch: {path}")
    records = raw.reshape(-1, 3073)
    labels = records[:, 0].astype(numpy.int64)
    if numpy.any(labels < 0) or numpy.any(labels > 9):
        raise RuntimeError(f"invalid CIFAR-10 label in {path}")
    images = records[:, 1:].reshape(-1, 3, 32, 32)
    return images, labels


def _stratified_indices(
    labels: numpy.ndarray,
    count_per_class: int,
    seed: int,
    excluded: Optional[set[int]] = None,
) -> list[int]:
    random = numpy.random.default_rng(seed)
    excluded = excluded or set()
    selected = []
    for label in range(10):
        candidates = numpy.asarray([
            int(value) for value in numpy.flatnonzero(labels == label)
            if int(value) not in excluded
        ], dtype=numpy.int64)
        if len(candidates) < count_per_class:
            raise RuntimeError(f"class {label} lacks enough samples")
        selected.extend(
            int(value)
            for value in random.choice(
                candidates, size=count_per_class, replace=False
            )
        )
    return sorted(selected)


def _preprocess(images: numpy.ndarray) -> numpy.ndarray:
    values = images.astype(numpy.float32) / numpy.float32(255.0)
    return (values - MEAN.reshape(1, 3, 1, 1)) / STD.reshape(1, 3, 1, 1)


def _dataset_digest(file_rows: list[dict[str, object]]) -> str:
    return hashlib.sha256(canonical_json_bytes({"files": file_rows})).hexdigest()


def _parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--cifar-dir", type=Path, required=True)
    parser.add_argument("--archive", type=Path, required=True)
    parser.add_argument("--checkpoint", type=Path, required=True)
    parser.add_argument("--fixture-manifest", type=Path, required=True)
    parser.add_argument("--output-dir", type=Path, required=True)
    parser.add_argument("--seed", type=int, default=20260910)
    parser.add_argument("--calibration-per-class", type=int, default=100)
    parser.add_argument("--acceptance-per-class", type=int, default=100)
    parser.add_argument("--exclude-acceptance-protocol", type=Path)
    return parser


def main(argv: Optional[Sequence[str]] = None) -> int:
    args = _parser().parse_args(argv)
    archive_md5 = hashlib.md5(args.archive.read_bytes()).hexdigest()
    if archive_md5 != CIFAR_ARCHIVE_MD5:
        raise RuntimeError("official CIFAR-10 archive MD5 does not match")
    batch_paths = [args.cifar_dir / f"data_batch_{index}.bin" for index in range(1, 6)]
    test_path = args.cifar_dir / "test_batch.bin"
    if any(not path.is_file() for path in batch_paths + [test_path]):
        raise RuntimeError("CIFAR-10 binary batches are incomplete")

    train_parts = [_read_batch(path) for path in batch_paths]
    train_images = numpy.concatenate([part[0] for part in train_parts])
    train_labels = numpy.concatenate([part[1] for part in train_parts])
    test_images, test_labels = _read_batch(test_path)
    if len(train_images) != 50000 or len(test_images) != 10000:
        raise RuntimeError("canonical CIFAR-10 sample counts changed")

    calibration_indices = _stratified_indices(
        train_labels, args.calibration_per_class, args.seed
    )
    excluded_acceptance: set[int] = set()
    if args.exclude_acceptance_protocol is not None:
        prior = json.loads(
            args.exclude_acceptance_protocol.read_text(encoding="ascii")
        )
        for sample_id in prior.get("acceptance", {}).get("sample_ids", []):
            split, index = sample_id.split(":", 1)
            if split != "test":
                raise RuntimeError("excluded acceptance protocol is not test-only")
            excluded_acceptance.add(int(index))
    acceptance_indices = _stratified_indices(
        test_labels, args.acceptance_per_class, args.seed + 1,
        excluded_acceptance,
    )
    calibration_ids = [f"train:{index:05d}" for index in calibration_indices]
    acceptance_ids = [f"test:{index:05d}" for index in acceptance_indices]
    output = args.output_dir
    output.mkdir(parents=True, exist_ok=True)

    preprocessing = {
        "schema": "open64.fhe.cifar10-preprocessing.v1",
        "input_encoding": "uint8_rgb_planar_cifar_binary",
        "layout": "NCHW",
        "output_dtype": "float32",
        "scale": "x/255.0",
        "mean_rgb": [float(value) for value in MEAN],
        "std_rgb": [float(value) for value in STD],
        "resize": None,
        "crop": None,
        "augmentation": None,
        "source_contract": "pinned ANT ACE resnet_cifar.main.inc and cifar_reader.h",
    }
    preprocessing_path = output / "cifar10-preprocessing.json"
    preprocessing_path.write_bytes(canonical_json_bytes(preprocessing))

    calibration_path = output / "cifar10-train-calibration.npz"
    numpy.savez(
        calibration_path,
        inputs=_preprocess(train_images[calibration_indices]),
        sample_ids=numpy.asarray(calibration_ids, dtype="U11"),
    )
    file_rows = [
        {
            "path": path.name,
            "sha256": file_sha256(path),
            "sample_count": 10000,
        }
        for path in batch_paths + [test_path]
    ]
    fixture = json.loads(args.fixture_manifest.read_text(encoding="ascii"))
    checkpoint_sha256 = file_sha256(args.checkpoint)
    if checkpoint_sha256 != fixture.get("open64_checkpoint_sha256"):
        raise RuntimeError("fixture manifest and checkpoint SHA-256 disagree")

    protocol = {
        "schema": "open64.fhe.sync3-commit19-protocol.v1",
        "status": "predeclared_before_acceptance",
        "protocol_revision": 2 if excluded_acceptance else 1,
        "authority": {
            "ace_revision": fixture.get("ace_revision"),
            "ace_onnx_sha256": fixture.get("ace_onnx_sha256"),
            "open64_checkpoint_sha256": checkpoint_sha256,
            "training_provenance": "unknown_not_claimed",
        },
        "dataset": {
            "name": "CIFAR-10",
            "version": "official-python-binary-distribution",
            "source_url": CIFAR_ARCHIVE_URL,
            "archive_md5": archive_md5,
            "archive_sha256": file_sha256(args.archive),
            "canonical_files_sha256": _dataset_digest(file_rows),
            "files": file_rows,
            "license_policy": "local_hash-verified_input_not_redistributed",
        },
        "preprocessing": {
            "path": preprocessing_path.name,
            "sha256": file_sha256(preprocessing_path),
        },
        "calibration": {
            "source_split": "train",
            "selection": "class_stratified_without_replacement_then_global_index_order",
            "seed": args.seed,
            "samples_per_class": args.calibration_per_class,
            "sample_count": len(calibration_ids),
            "sample_ids": calibration_ids,
            "sample_order_sha256": hashlib.sha256(
                canonical_json_bytes({"sample_ids": calibration_ids})
            ).hexdigest(),
            "archive_path": calibration_path.name,
            "archive_sha256": file_sha256(calibration_path),
            "bound_rule": "max_abs_times_safety_factor:1.1",
            "out_of_range_policy": "reject",
        },
        "acceptance": {
            "source_split": "test",
            "selection": "class_stratified_without_replacement_then_global_index_order",
            "seed": args.seed + 1,
            "excluded_prior_acceptance_sample_count": len(excluded_acceptance),
            "excluded_prior_acceptance_protocol_sha256": (
                file_sha256(args.exclude_acceptance_protocol)
                if args.exclude_acceptance_protocol is not None else None
            ),
            "samples_per_class": args.acceptance_per_class,
            "sample_count": len(acceptance_ids),
            "sample_ids": acceptance_ids,
            "sample_order_sha256": hashlib.sha256(
                canonical_json_bytes({"sample_ids": acceptance_ids})
            ).hexdigest(),
            "thresholds": {
                "minimum_clear_top1_percent": 89.0,
                "published_ace_clear_reference_percent": 90.6,
                "maximum_clear_reference_difference_points": 3.0,
                "maximum_polynomial_top1_drop_points": 1.0,
                "minimum_prediction_agreement_percent": 98.0,
                "maximum_out_of_range_values": 0,
            },
            "report_only_metrics": [
                "maximum_logit_linf",
                "per_context_relu_linf",
                "per_context_relu_l2",
            ],
        },
    }
    protocol_path = output / "commit19-protocol.json"
    protocol_path.write_bytes(canonical_json_bytes(protocol))
    print(
        f"froze {len(calibration_ids)} train calibration and "
        f"{len(acceptance_ids)} held-out test samples"
    )
    return 0


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except (OSError, RuntimeError, ValueError) as error:
        print(f"CFHECNN-PREPROCESS-001: {error}", file=sys.stderr)
        raise SystemExit(1)
