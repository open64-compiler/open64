"""Run a prepared M2 generated-provider package on a remote Hopper host."""

from __future__ import annotations

import argparse
import csv
import hashlib
import json
import math
from pathlib import Path
import re
import statistics
import subprocess


HOST = re.compile(
    r"[A-Za-z0-9_][A-Za-z0-9_.-]*(?:@[A-Za-z0-9_][A-Za-z0-9_.-]*)?\Z")
REMOTE_PATH = re.compile(r"/[A-Za-z0-9_./-]+\Z")
PREPARED_REQUIRED = {
    "binary/matmul.o", "source/matmul.generated.cu",
    "source/generated_runtime.cu", "source/generated_runtime.h",
    "source/generated_benchmark.c", "source/kernel_control.cu",
    "source/runtime.c", "source/runtime.h",
    "source/open64_dsl_runtime_abi.h", "run_generated_gpu.py",
    "remote_generated.py", "plans/m2-selected-tile.json", "traces/compile.t",
    "receipt.json",
}
NATIVE_EVIDENCE = (
    "native-receipt.json", "measurements/raw.csv", "logs/nvidia-smi.log",
    "logs/nvcc-version.log", "logs/objcopy.log", "logs/runtime-cc.log",
    "logs/benchmark-cc.log", "logs/nvcc.log", "logs/link.log",
    "logs/run.log", "binary/matmul.link.o", "binary/runtime.o",
    "binary/benchmark.o", "binary/generated_runtime.o",
    "binary/matmul-generated", "logs/control-nvcc.log",
    "logs/control-run.log", "binary/kernel-control",
    "measurements/kernel-control.csv",
)


def kernel_control_summary(path: Path) -> dict[str, float | int]:
    with path.open(newline="", encoding="utf-8") as stream:
        reader = csv.DictReader(stream)
        if reader.fieldnames != ["sample", "variant", "elapsed_ms"]:
            raise RuntimeError("kernel control CSV header is invalid")
        rows = list(reader)
    times: dict[str, list[float]] = {
        "untiled_control": [], "selected_tile": []
    }
    if len(rows) != 60:
        raise RuntimeError("kernel control needs 30 paired samples")
    for index, row in enumerate(rows):
        sample = index // 2
        selected_first = sample % 2 == 1
        variant = ("selected_tile" if (index % 2 == 0) == selected_first
                   else "untiled_control")
        try:
            elapsed = float(row["elapsed_ms"])
            observed_sample = int(row["sample"])
        except (TypeError, ValueError) as exc:
            raise RuntimeError("kernel control row is malformed") from exc
        if (row["variant"] != variant or observed_sample != sample or
                not math.isfinite(elapsed) or elapsed <= 0):
            raise RuntimeError("kernel control sample order or time is invalid")
        times[variant].append(elapsed)
    direct = statistics.median(times["untiled_control"])
    tiled = statistics.median(times["selected_tile"])
    return {"samples_per_variant": 30,
            "untiled_control_median_ms": direct,
            "selected_tile_median_ms": tiled,
            "control_ratio_direct_over_tiled": direct / tiled}


def verify_manifest(root: Path, name: str) -> set[str]:
    manifest = root / name
    if not manifest.is_file():
        raise RuntimeError(f"missing {name}")
    entries: set[str] = set()
    for line in manifest.read_text(encoding="utf-8").splitlines():
        if "  " not in line:
            raise RuntimeError(f"malformed {name} row")
        expected, relative = line.split("  ", 1)
        path = root / relative
        try:
            path.resolve().relative_to(root.resolve())
        except ValueError as exc:
            raise RuntimeError(f"unsafe {name} entry: {relative}") from exc
        if (not re.fullmatch(r"[0-9a-f]{64}", expected) or
                not relative or relative in entries or
                Path(relative).is_absolute() or
                not path.is_file() or
                hashlib.sha256(path.read_bytes()).hexdigest() != expected):
            raise RuntimeError(f"invalid or changed {name} entry: {relative}")
        entries.add(relative)
    if not entries:
        raise RuntimeError(f"empty {name}")
    return entries


def verify_prepared(root: Path) -> None:
    entries = verify_manifest(root, "SHA256SUMS")
    if not PREPARED_REQUIRED.issubset(entries):
        raise RuntimeError("prepared M2 package is incomplete")
    receipt = json.loads((root / "receipt.json").read_text(encoding="utf-8"))
    handoff = json.loads((root / "plans/m2-selected-tile.json").read_text(
        encoding="utf-8"))
    if (receipt.get("schema") != "ai-matmul-m2-generated-gpu-package-v1" or
            receipt.get("status") != "native_gpu_execution_pending" or
            handoff.get("whirl_rewrite_applied") is not True or
            receipt.get("selected_tile_plan_id") != handoff.get(
                "selected_tile_plan_id") or
            receipt.get("selected_optimization_plan_id") != handoff.get(
                "selected_optimization_plan_id")):
        raise RuntimeError("prepared M2 package lineage is invalid")


def verify_native(root: Path) -> None:
    entries = verify_manifest(root, "native-SHA256SUMS")
    if not set(NATIVE_EVIDENCE).issubset(entries):
        raise RuntimeError("native GPU evidence is incomplete")
    receipt = json.loads((root / "native-receipt.json").read_text(
        encoding="utf-8"))
    handoff = json.loads((root / "plans/m2-selected-tile.json").read_text(
        encoding="utf-8"))
    if (receipt.get("schema") != "ai-matmul-m2-generated-gpu-run-v1" or
            receipt.get("status") != "gpu_correctness_verified_timing_uncalibrated" or
            receipt.get("generated_path_required_each_sample") is not True or
            receipt.get("fallback_samples") != 0 or
            receipt.get("samples") != 30 or
            receipt.get("selected_tile_plan_id") != handoff.get(
                "selected_tile_plan_id") or
            receipt.get("selected_optimization_plan_id") != handoff.get(
                "selected_optimization_plan_id") or
            receipt.get("kernel_control_origin") !=
                "validation_only_untiled_cuda" or
            receipt.get("kernel_control_verified") is not True or
            receipt.get("kernel_control") != kernel_control_summary(
                root / "measurements/kernel-control.csv") or
            receipt.get("same_target_baseline_verified") is not False or
            receipt.get("closes_m2_gate") is not False or
            receipt.get("source_sha256") != hashlib.sha256(
                (root / "source/matmul.generated.cu").read_bytes()).hexdigest() or
            receipt.get("object_sha256") != hashlib.sha256(
                (root / "binary/matmul.o").read_bytes()).hexdigest() or
            receipt.get("executable_sha256") != hashlib.sha256(
                (root / "binary/matmul-generated").read_bytes()).hexdigest()):
        raise RuntimeError("native GPU receipt contradicts the selected package")


def native_attempt_exists(root: Path) -> bool:
    return (any((root / "logs").glob("*.log")) or
            (root / "measurements/raw.csv").exists() or
            (root / "binary/matmul.link.o").exists() or
            (root / "native-receipt.json").exists() or
            (root / "native-SHA256SUMS").exists())


def execute(command: list[str]) -> None:
    subprocess.run(command, check=True)


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--artifact-dir", type=Path, required=True)
    parser.add_argument("--host", required=True, help="SSH host or user@host")
    parser.add_argument("--remote-dir", required=True,
                        help="new absolute remote artifact path")
    args = parser.parse_args()
    if (not HOST.fullmatch(args.host) or
            not REMOTE_PATH.fullmatch(args.remote_dir) or
            ".." in Path(args.remote_dir).parts):
        parser.error("host or remote path contains unsupported characters")
    root = args.artifact_dir.resolve()
    verify_prepared(root)
    if native_attempt_exists(root):
        parser.error("artifact already has a native attempt; create a new package")
    parent = str(Path(args.remote_dir).parent)
    execute(["ssh", args.host,
             f"mkdir -p {parent} && mkdir {args.remote_dir}"])
    execute(["rsync", "-a", f"{root}/", f"{args.host}:{args.remote_dir}/"])
    try:
        execute(["ssh", args.host,
                 f"python3 {args.remote_dir}/run_generated_gpu.py "
                 f"--artifact-dir {args.remote_dir} --arch sm_90"])
    finally:
        execute(["rsync", "-a", f"{args.host}:{args.remote_dir}/", f"{root}/"])
        verify_prepared(root)
    verify_native(root)
    print(f"retrieved and verified generated M2 GPU evidence: {root}")


if __name__ == "__main__":
    main()
