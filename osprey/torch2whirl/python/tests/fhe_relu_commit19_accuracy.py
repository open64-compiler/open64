"""Run held-out clear and ACE-composite ReLU accuracy for FHE Commit 19."""

from __future__ import annotations

import argparse
import hashlib
import importlib.util
import json
import math
from pathlib import Path
import platform
import sys
from typing import Optional, Sequence

import numpy
import torch


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


def _load_json(path: Path) -> dict:
    value = json.loads(path.read_text(encoding="ascii"))
    if not isinstance(value, dict):
        raise RuntimeError(f"expected a JSON object: {path}")
    return value


def _load_model(source: Path, checkpoint: Path):
    spec = importlib.util.spec_from_file_location("secure_resnet20_accuracy", source)
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


def _read_test_batch(path: Path) -> tuple[numpy.ndarray, numpy.ndarray]:
    raw = numpy.fromfile(path, dtype=numpy.uint8)
    if raw.size != 10000 * 3073:
        raise RuntimeError("canonical CIFAR-10 test batch has the wrong size")
    records = raw.reshape(10000, 3073)
    return records[:, 1:].reshape(-1, 3, 32, 32), records[:, 0].astype(numpy.int64)


def _preprocess(images: numpy.ndarray, definition: dict) -> numpy.ndarray:
    if (
        definition.get("layout") != "NCHW"
        or definition.get("output_dtype") != "float32"
        or definition.get("scale") != "x/255.0"
        or definition.get("resize") is not None
        or definition.get("crop") is not None
        or definition.get("augmentation") is not None
    ):
        raise RuntimeError("predeclared CIFAR-10 preprocessing is unsupported")
    mean = numpy.asarray(definition["mean_rgb"], dtype=numpy.float32)
    std = numpy.asarray(definition["std_rgb"], dtype=numpy.float32)
    values = images.astype(numpy.float32) / numpy.float32(255.0)
    return (values - mean.reshape(1, 3, 1, 1)) / std.reshape(1, 3, 1, 1)


def _chebyshev_clenshaw(value: torch.Tensor,
                        coefficients: Sequence[float]) -> torch.Tensor:
    work = value.to(dtype=torch.float64)
    next_value = torch.zeros_like(work)
    next_next = torch.zeros_like(work)
    for coefficient in reversed(coefficients[1:]):
        current = 2.0 * work * next_value - next_next + coefficient
        next_next = next_value
        next_value = current
    return work * next_value - next_next + coefficients[0]


def _composite_relu(value: torch.Tensor, bound: float,
                    stages: Sequence[Sequence[float]]) -> torch.Tensor:
    normalized = value.to(dtype=torch.float64) / bound
    sign = normalized
    for coefficients in stages:
        sign = _chebyshev_clenshaw(sign, coefficients)
    result = 0.5 * value.to(dtype=torch.float64) * sign + 0.5 * value
    return result.to(dtype=value.dtype)


class _ReluEmulator:
    def __init__(self, model, contexts: Sequence[dict], stages):
        self._routes: dict[str, list[dict]] = {}
        self._stages = stages
        self._counts: dict[str, int] = {}
        self._stats: dict[str, dict[str, float | int]] = {}
        self._handles = []
        modules = dict(model.named_modules())
        for context in contexts:
            route = context["module_path"]
            self._routes.setdefault(route, []).append(context)
            self._stats[context["instance_path"]] = {
                "maximum_absolute_error": 0.0,
                "sum_squared_error": 0.0,
                "element_count": 0,
                "out_of_range_count": 0,
            }
        for route in self._routes:
            self._routes[route].sort(key=lambda row: row["invocation_ordinal"])
            if route not in modules:
                raise RuntimeError(f"range route is absent from model: {route}")
            self._handles.append(
                modules[route].register_forward_hook(self._hook(route))
            )

    def _hook(self, route: str):
        def apply(_module, inputs, _output):
            ordinal = self._counts.get(route, 0)
            contexts = self._routes[route]
            if ordinal >= len(contexts) or len(inputs) != 1:
                raise RuntimeError(f"unexpected ReLU invocation: {route}")
            context = contexts[ordinal]
            value = inputs[0]
            bound = float(context["bound_b"])
            result = _composite_relu(value, bound, self._stages)
            reference = torch.relu(value)
            error = (result - reference).to(dtype=torch.float64)
            stats = self._stats[context["instance_path"]]
            stats["maximum_absolute_error"] = max(
                float(stats["maximum_absolute_error"]),
                float(error.abs().max().item()),
            )
            stats["sum_squared_error"] = float(stats["sum_squared_error"]) + float(
                torch.sum(error * error).item()
            )
            stats["element_count"] = int(stats["element_count"]) + error.numel()
            stats["out_of_range_count"] = int(
                stats["out_of_range_count"]
            ) + int((value.abs() > bound).sum().item())
            self._counts[route] = ordinal + 1
            return result
        return apply

    def begin(self) -> None:
        self._counts.clear()

    def finish_batch(self) -> None:
        for route, contexts in self._routes.items():
            if self._counts.get(route, 0) != len(contexts):
                raise RuntimeError(f"missing ReLU invocation: {route}")

    def close(self) -> None:
        for handle in self._handles:
            handle.remove()

    def results(self) -> list[dict[str, object]]:
        output = []
        for route in sorted(self._stats):
            row = self._stats[route]
            count = int(row["element_count"])
            output.append({
                "instance_path": route,
                "maximum_absolute_error": row["maximum_absolute_error"],
                "l2_error": math.sqrt(float(row["sum_squared_error"])),
                "root_mean_squared_error": math.sqrt(
                    float(row["sum_squared_error"]) / count
                ),
                "element_count": count,
                "out_of_range_count": row["out_of_range_count"],
            })
        return output


def _coefficient_stages(manifest: dict) -> list[list[float]]:
    if manifest.get("status") != "approved_empirical_ace":
        raise RuntimeError("coefficient manifest is not approved")
    stages = manifest.get("stages")
    if not isinstance(stages, list) or [row.get("degree") for row in stages] != [7, 15, 13]:
        raise RuntimeError("coefficient stage order is invalid")
    return [[float(value) for value in row["source_decimals"]] for row in stages]


def _parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--model-source", type=Path, required=True)
    parser.add_argument("--checkpoint", type=Path, required=True)
    parser.add_argument("--cifar-test-batch", type=Path, required=True)
    parser.add_argument("--preprocessing", type=Path, required=True)
    parser.add_argument("--protocol", type=Path, required=True)
    parser.add_argument("--range-manifest", type=Path, required=True)
    parser.add_argument("--coefficient-manifest", type=Path, required=True)
    parser.add_argument("--batch-size", type=int, default=32)
    parser.add_argument("--output", type=Path, required=True)
    return parser


def main(argv: Optional[Sequence[str]] = None) -> int:
    args = _parser().parse_args(argv)
    protocol = _load_json(args.protocol)
    ranges = _load_json(args.range_manifest)
    coefficients = _load_json(args.coefficient_manifest)
    preprocessing = _load_json(args.preprocessing)
    if protocol.get("status") != "predeclared_before_acceptance":
        raise RuntimeError("acceptance thresholds were not frozen before execution")
    if file_sha256(args.checkpoint) != protocol["authority"]["open64_checkpoint_sha256"]:
        raise RuntimeError("acceptance checkpoint differs from predeclared protocol")
    if file_sha256(args.preprocessing) != protocol["preprocessing"]["sha256"]:
        raise RuntimeError("acceptance preprocessing differs from protocol")
    if ranges.get("status") != "approved":
        raise RuntimeError("identity-bound range manifest is not approved")

    images, labels = _read_test_batch(args.cifar_test_batch)
    ids = protocol["acceptance"]["sample_ids"]
    indices = [int(sample_id.split(":", 1)[1]) for sample_id in ids]
    selected = _preprocess(images[indices], preprocessing)
    selected_labels = labels[indices]
    model = _load_model(args.model_source, args.checkpoint)
    baseline_model = _load_model(args.model_source, args.checkpoint)
    stages = _coefficient_stages(coefficients)
    thresholds = protocol["acceptance"]["thresholds"]
    baseline_correct = 0
    polynomial_correct = 0
    agreement = 0
    maximum_logit_linf = 0.0

    baseline_predictions = []
    with torch.no_grad():
        for start in range(0, len(selected), args.batch_size):
            values = torch.from_numpy(selected[start:start + args.batch_size].copy())
            logits = model(values)
            baseline_predictions.append(logits.argmax(dim=1).numpy())
    baseline_predictions_array = numpy.concatenate(baseline_predictions)
    baseline_correct = int(numpy.sum(baseline_predictions_array == selected_labels))

    emulator = _ReluEmulator(model, ranges["contexts"], stages)
    try:
        with torch.no_grad():
            for start in range(0, len(selected), args.batch_size):
                values = torch.from_numpy(selected[start:start + args.batch_size].copy())
                emulator.begin()
                polynomial_logits = model(values)
                emulator.finish_batch()
                baseline_logits = baseline_model(values)
                polynomial_predictions = polynomial_logits.argmax(dim=1).numpy()
                expected_predictions = baseline_logits.argmax(dim=1).numpy()
                truth = selected_labels[start:start + len(polynomial_predictions)]
                polynomial_correct += int(numpy.sum(polynomial_predictions == truth))
                agreement += int(numpy.sum(polynomial_predictions == expected_predictions))
                maximum_logit_linf = max(
                    maximum_logit_linf,
                    float(torch.max(torch.abs(polynomial_logits - baseline_logits)).item()),
                )
    finally:
        emulator.close()

    sample_count = len(selected)
    baseline_top1 = 100.0 * baseline_correct / sample_count
    polynomial_top1 = 100.0 * polynomial_correct / sample_count
    prediction_agreement = 100.0 * agreement / sample_count
    top1_drop = baseline_top1 - polynomial_top1
    context_errors = emulator.results()
    out_of_range = sum(int(row["out_of_range_count"]) for row in context_errors)
    failures = []
    if baseline_top1 < thresholds["minimum_clear_top1_percent"]:
        failures.append("clear baseline is below the predeclared credibility floor")
    if abs(baseline_top1 - thresholds["published_ace_clear_reference_percent"]) > thresholds["maximum_clear_reference_difference_points"]:
        failures.append("clear baseline is inconsistent with the ACE reference")
    if top1_drop > thresholds["maximum_polynomial_top1_drop_points"]:
        failures.append("polynomial top-1 degradation exceeds the gate")
    if prediction_agreement < thresholds["minimum_prediction_agreement_percent"]:
        failures.append("prediction agreement is below the gate")
    if out_of_range > thresholds["maximum_out_of_range_values"]:
        failures.append("held-out activation exceeds an approved context bound")

    software_lock = {
        "python": platform.python_version(),
        "torch": torch.__version__,
        "numpy": numpy.__version__,
        "platform": platform.platform(),
        "collector": "fhe_relu_commit19_accuracy.py",
    }
    result = {
        "schema": "open64.fhe.relu.accuracy.v1",
        "status": "approved" if not failures else "rejected",
        "profile_name": ranges["profile_name"],
        "inputs": {
            "protocol_sha256": file_sha256(args.protocol),
            "dataset_sha256": protocol["dataset"]["canonical_files_sha256"],
            "dataset_test_batch_sha256": file_sha256(args.cifar_test_batch),
            "dataset_split": "held-out-test-class-stratified",
            "sample_order_sha256": protocol["acceptance"]["sample_order_sha256"],
            "preprocessing_sha256": file_sha256(args.preprocessing),
            "trained_checkpoint_sha256": file_sha256(args.checkpoint),
            "coefficient_manifest_sha256": file_sha256(args.coefficient_manifest),
            "range_manifest_sha256": file_sha256(args.range_manifest),
            "software_lock": software_lock,
            "software_lock_sha256": hashlib.sha256(
                canonical_json_bytes(software_lock)
            ).hexdigest(),
        },
        "predeclared_metrics": thresholds,
        "results": {
            "sample_count": sample_count,
            "clear_baseline_top1_percent": baseline_top1,
            "clear_polynomial_top1_percent": polynomial_top1,
            "polynomial_top1_drop_points": top1_drop,
            "prediction_agreement_percent": prediction_agreement,
            "maximum_logit_linf": maximum_logit_linf,
            "out_of_range_value_count": out_of_range,
            "context_errors": context_errors,
        },
        "failures": failures,
        "scope": (
            "clear CPU emulation only; no ciphertext or OpenFHE execution claim"
        ),
        "published_reference": {
            "source": "ANT ACE paper Table 11",
            "url": "https://ant-research.github.io/ace-compiler/assets/ACE_paper.pdf",
            "sample_count": 1000,
            "sample_identities_available": False,
            "clear_top1_percent": 90.6,
            "encrypted_top1_percent": 91.2,
            "role": "reference_only_not_reproducibility_evidence",
        },
    }
    args.output.parent.mkdir(parents=True, exist_ok=True)
    args.output.write_bytes(canonical_json_bytes(result))
    print(
        f"clear={baseline_top1:.3f}% polynomial={polynomial_top1:.3f}% "
        f"drop={top1_drop:.3f}pp agreement={prediction_agreement:.3f}% "
        f"out_of_range={out_of_range}"
    )
    if failures:
        raise RuntimeError("; ".join(failures))
    return 0


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except (OSError, RuntimeError, ValueError) as error:
        print(f"CFHECNN-ACCURACY-001: {error}", file=sys.stderr)
        raise SystemExit(1)
