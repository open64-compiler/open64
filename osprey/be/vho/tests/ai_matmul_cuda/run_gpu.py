"""Compile and test a prepared artifact on a native Hopper-or-later GPU."""

from __future__ import annotations

import argparse
import csv
import hashlib
import json
import math
import os
from pathlib import Path
import shutil
import statistics
import subprocess


def sha256(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as stream:
        for block in iter(lambda: stream.read(1024 * 1024), b""):
            digest.update(block)
    return digest.hexdigest()


def capture(command: list[str], path: Path, cwd: Path) -> None:
    with path.open("w", encoding="utf-8") as stream:
        stream.write("command=" + json.dumps(command) + "\n")
        stream.flush()
        result = subprocess.run(command, cwd=cwd, stdout=stream,
                                stderr=subprocess.STDOUT, check=False)
        stream.write(f"exit_status={result.returncode}\n")
    if result.returncode:
        raise RuntimeError(f"command failed ({result.returncode}): {path}")


def validate_samples(rows: list[dict[str, str]], compiler_source: bool) -> None:
    if compiler_source:
        for variant in ("sync", "tma"):
            subset = [row for row in rows if row.get("variant") == variant]
            if (len(subset) != 30 or any(int(row["sample"]) != i or
                                         not math.isfinite(float(row["elapsed_ms"])) or
                                         float(row["elapsed_ms"]) <= 0
                                         for i, row in enumerate(subset))):
                raise RuntimeError(f"missing or malformed {variant} wall-clock samples")
        if len(rows) != 60:
            raise RuntimeError("unexpected wall-clock sample variant")
    elif len(rows) != 30 or any(int(row["sample"]) != i or
                                not math.isfinite(float(row["elapsed_ms"])) or
                                float(row["elapsed_ms"]) <= 0
                                for i, row in enumerate(rows)):
        raise RuntimeError("missing or malformed CUDA-event samples")


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--artifact-dir", type=Path, default=Path(__file__).resolve().parent)
    parser.add_argument("--arch", default="sm_90", choices=("sm_90", "sm_100", "sm_120"))
    args = parser.parse_args()
    root = args.artifact_dir.resolve()
    source = root / "source/matmul.generated.cu"
    source_hash = (root / "source.sha256").read_text(encoding="utf-8").split()[0]
    if sha256(source) != source_hash:
        parser.error("generated .cu does not match the prepared source hash")
    compiler_package = root / "compiler.json"
    compiler_metadata = (json.loads(compiler_package.read_text(encoding="utf-8"))
                         if compiler_package.exists() else None)
    if compiler_metadata is not None:
        harness = root / "source/benchmark.cu"
        handoff = json.loads((root / "plans/m2-selected-tile.json").read_text(
            encoding="utf-8"))
        if (compiler_metadata.get("schema") != "ai-matmul-compiler-gpu-package-v1"
                or compiler_metadata["source_sha256"] != source_hash
                or handoff.get("schema") != "ai-matmul-m2-handoff-v1"
                or handoff.get("whirl_rewrite_applied") is not False
                or handoff.get("source_sha256") != source_hash
                or handoff.get("selected_optimization_plan_id") !=
                    compiler_metadata["tile_plan_id"]
                or handoff.get("fetch_plan_id") !=
                    compiler_metadata["fetch_plan_id"]
                or sha256(harness) != compiler_metadata["harness_sha256"]
                or sha256(root / "traces/compile.t") !=
                    compiler_metadata["raw_trace_sha256"]
                or sha256(root / "plans/m2-selected-tile.json") !=
                    compiler_metadata["m2_handoff_sha256"]
                or sha256(root / "source/compiler-receipt.json") !=
                    compiler_metadata["compiler_receipt_sha256"]):
            parser.error("compiler-generated GPU package failed integrity checks")
    if (root / "receipt.json").exists():
        parser.error("artifact already has a completed GPU receipt; prepare a new one")
    nvcc = shutil.which("nvcc")
    smi = shutil.which("nvidia-smi")
    if nvcc is None or smi is None:
        parser.error("native CUDA toolkit (nvcc) and nvidia-smi are required")
    for name in ("binary", "logs", "measurements", "reports"):
        (root / name).mkdir(exist_ok=True)
    capture([smi, "--query-gpu=name,compute_cap,driver_version,memory.total",
             "--format=csv,noheader"], root / "reports/device.txt", root)
    device_line = (root / "reports/device.txt").read_text(encoding="utf-8").splitlines()[1]
    compute_cap = device_line.split(",")[1].strip()
    if int(compute_cap.split(".")[0]) < 9:
        parser.error(f"Hopper-or-later GPU required; device reports {compute_cap}")
    capture([nvcc, "--version"], root / "logs/nvcc-version.log", root)
    executable = root / "binary/matmul"
    compilation_source = (root / "source/benchmark.cu"
                          if compiler_metadata is not None else source)
    command = [nvcc, "-std=c++17", "-O3", f"-arch={args.arch}",
               "-Xptxas=-v", str(compilation_source)]
    if compiler_metadata is not None:
        command.append("-lcuda")
    command.extend(("-o", str(executable)))
    capture(command, root / "logs/nvcc.log", root)
    with (root / "measurements/raw.csv").open("w", encoding="utf-8") as out, \
            (root / "logs/run.log").open("w", encoding="utf-8") as err:
        result = subprocess.run([str(executable)], cwd=root, stdout=out,
                                stderr=err, check=False)
        err.write(f"exit_status={result.returncode}\n")
    if result.returncode:
        raise RuntimeError("CUDA correctness or benchmark failed; see logs/run.log")
    report = (root / "logs/run.log").read_text(encoding="utf-8")
    if compiler_metadata is not None:
        if (report.count("PASS shape=") != 2 or
                "variant=sync" not in report or "variant=tma" not in report):
            raise RuntimeError("sync and selected-TMA correctness checks must pass")
    elif report.count("PASS shape=") != 2:
        raise RuntimeError("both exact and edge correctness checks must pass")
    with (root / "measurements/raw.csv").open(newline="", encoding="utf-8") as stream:
        rows = list(csv.DictReader(stream))
    validate_samples(rows, compiler_metadata is not None)
    samples = [float(row["elapsed_ms"]) for row in rows]
    receipt = {
        "schema": "ai-matmul-cuda-validation-v1",
        "status": "standalone_cuda_adapter_measured",
        "compiler_generated_from_whirl": False,
        "closes_m2_generated_tile_gate": False,
        "measurement_scope": "kernel_only_cuda_events; excludes transfers and allocations",
        "source_path": "source/matmul.generated.cu",
        "source_sha256": source_hash,
        "plan": (json.loads((root / "plan.json").read_text(encoding="utf-8"))
                 if compiler_metadata is None else None),
        "target": device_line,
        "nvcc_sha256": sha256(Path(nvcc)),
        "compile_command": command,
        "correctness": report.strip(),
        "sample_count": len(samples),
        "median_ms": statistics.median(samples),
        "min_ms": min(samples),
        "max_ms": max(samples),
    }
    if compiler_metadata is not None:
        receipt.update({
            "schema": "ai-matmul-compiler-cuda-validation-v1",
            "status": "compiler_source_sync_and_tma_verified",
            "compiler_generated_from_whirl_analysis": True,
            "whirl_rewrite_applied": False,
            "closes_m3_prefetch_pipeline_gate": False,
            "selected_tile_plan_id": compiler_metadata["tile_plan_id"],
            "selected_fetch_plan_id": compiler_metadata["fetch_plan_id"],
            "compiler_trace_sha256": compiler_metadata["raw_trace_sha256"],
            "harness_sha256": compiler_metadata["harness_sha256"],
            "measurement_scope": "host_wall_launch_to_stream_completion; sync versus selected TMA; includes launcher setup and host tensor-map encoding; excludes transfers and allocations",
            "sample_count_per_variant": 30,
            "median_sync_ms": statistics.median(
                float(row["elapsed_ms"]) for row in rows if row["variant"] == "sync"),
            "median_tma_ms": statistics.median(
                float(row["elapsed_ms"]) for row in rows if row["variant"] == "tma"),
        })
        receipt.pop("plan", None)
    (root / "receipt.json").write_text(json.dumps(receipt, indent=2) + "\n", encoding="utf-8")
    files = sorted(p for p in root.rglob("*") if p.is_file() and p.name != "SHA256SUMS")
    (root / "SHA256SUMS").write_text(
        "".join(f"{sha256(p)}  {p.relative_to(root)}\n" for p in files), encoding="utf-8"
    )
    if sha256(source) != source_hash:
        raise RuntimeError("generated .cu changed during remote execution")
    print(f"CUDA validation artifact: {root}")


if __name__ == "__main__":
    main()
