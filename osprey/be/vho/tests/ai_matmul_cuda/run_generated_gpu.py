"""Link the applied Open64 PU to its selected CUDA tile and verify GPU execution."""

from __future__ import annotations

import argparse
import csv
import hashlib
import json
from pathlib import Path
import platform
import shutil
import statistics
import subprocess

from remote_generated import (NATIVE_EVIDENCE, kernel_control_summary,
                              native_attempt_exists, verify_prepared)


def digest(path: Path) -> str:
    return hashlib.sha256(path.read_bytes()).hexdigest()


def run(command: list[str], cwd: Path, log: Path) -> None:
    with log.open("w", encoding="utf-8") as stream:
        stream.write("command=" + json.dumps(command) + "\n")
        stream.flush()
        result = subprocess.run(command, cwd=cwd, stdout=stream,
                                stderr=subprocess.STDOUT, check=False)
        stream.write(f"exit_status={result.returncode}\n")
    if result.returncode:
        raise RuntimeError(f"command failed: {log}")


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--artifact-dir", type=Path, required=True)
    parser.add_argument("--arch", default="sm_90")
    args = parser.parse_args()
    if platform.system() != "Linux" or platform.machine() != "x86_64":
        parser.error("applied Open64 object requires native Linux/x86-64")
    root = args.artifact_dir.resolve()
    verify_prepared(root)
    if native_attempt_exists(root):
        parser.error("native attempt already exists; create a new package")
    for name in ("nvcc", "cc", "objcopy", "nvidia-smi"):
        if shutil.which(name) is None:
            parser.error(f"{name} is required for native validation")
    nvcc = shutil.which("nvcc")
    cc = shutil.which("cc")
    objcopy = shutil.which("objcopy")
    smi = shutil.which("nvidia-smi")
    assert nvcc and cc and objcopy and smi
    if args.arch != "sm_90":
        parser.error("the initial generated M2 package is certified for sm_90 only")
    handoff = json.loads((root / "plans/m2-selected-tile.json").read_text())
    if not handoff["whirl_rewrite_applied"]:
        parser.error("package does not contain an applied WHIRL rewrite")
    run([smi, "-q"], root, root / "logs/nvidia-smi.log")
    run([nvcc, "--version"], root, root / "logs/nvcc-version.log")
    run([objcopy, "--globalize-symbol=ai_matmul_m0",
         "binary/matmul.o", "binary/matmul.link.o"], root,
        root / "logs/objcopy.log")
    run([cc, "-std=c11", "-O2", "-fno-fast-math", "-ffp-contract=off",
         "-DAI_MATMUL_M2_RUNTIME", "-Isource", "-c", "source/runtime.c",
         "-o", "binary/runtime.o"], root, root / "logs/runtime-cc.log")
    run([cc, "-std=c11", "-O2", "-Isource", "-c",
         "source/generated_benchmark.c", "-o", "binary/benchmark.o"], root,
        root / "logs/benchmark-cc.log")
    run([nvcc, "-std=c++17", "-O3", "-arch=sm_90", "-Xptxas=-v",
         "-Isource", "-c",
         "source/generated_runtime.cu", "-o", "binary/generated_runtime.o"],
        root, root / "logs/nvcc.log")
    run([nvcc, "-Xcompiler=-no-pie", "binary/matmul.link.o", "binary/runtime.o",
         "binary/benchmark.o", "binary/generated_runtime.o", "-o",
         "binary/matmul-generated"], root, root / "logs/link.log")
    run([nvcc, "-std=c++17", "-O3", "-arch=sm_90", "-Xptxas=-v",
         "-Isource", "source/kernel_control.cu", "-o",
         "binary/kernel-control"], root, root / "logs/control-nvcc.log")
    raw = root / "measurements/raw.csv"
    with raw.open("w", encoding="utf-8") as stream:
        result = subprocess.run([str(root / "binary/matmul-generated")],
                                cwd=root, stdout=stream, stderr=subprocess.PIPE,
                                text=True, check=False)
    (root / "logs/run.log").write_text(
        f"exit_status={result.returncode}\n{result.stderr}", encoding="utf-8")
    if result.returncode:
        raise RuntimeError("generated path failed correctness or fell back")
    with raw.open(newline="", encoding="utf-8") as stream:
        rows = list(csv.DictReader(stream))
    if len(rows) != 30 or any(int(row["sample"]) != i or
                              int(row["elapsed_ns"]) <= 0
                              for i, row in enumerate(rows)):
        raise RuntimeError("generated path produced incomplete timing samples")
    times = [int(row["elapsed_ns"]) for row in rows]
    control_raw = root / "measurements/kernel-control.csv"
    with control_raw.open("w", encoding="utf-8") as stream:
        control_result = subprocess.run([str(root / "binary/kernel-control")],
                                        cwd=root, stdout=stream,
                                        stderr=subprocess.PIPE, text=True,
                                        check=False)
    (root / "logs/control-run.log").write_text(
        f"exit_status={control_result.returncode}\n{control_result.stderr}",
        encoding="utf-8")
    if control_result.returncode:
        raise RuntimeError("nonuniform-input kernel control failed")
    control = kernel_control_summary(control_raw)
    receipt = {
        "schema": "ai-matmul-m2-generated-gpu-run-v1",
        "status": "gpu_correctness_verified_timing_uncalibrated",
        "generated_path_required_each_sample": True,
        "fallback_samples": 0,
        "shape": [128, 128, 128],
        "dtype": "F32",
        "samples": len(times),
        "median_ns": statistics.median(times),
        "selected_tile_plan_id": handoff["selected_tile_plan_id"],
        "selected_optimization_plan_id": handoff["selected_optimization_plan_id"],
        "measurement_scope": "compiled PU plus CUDA allocations, transfers, launch and sync",
        "kernel_control_origin": "validation_only_untiled_cuda",
        "kernel_control_verified": True,
        "kernel_control": control,
        "same_target_baseline_verified": False,
        "closes_m2_gate": False,
        "source_sha256": digest(root / "source/matmul.generated.cu"),
        "object_sha256": digest(root / "binary/matmul.o"),
        "executable_sha256": digest(root / "binary/matmul-generated"),
    }
    (root / "native-receipt.json").write_text(json.dumps(receipt, indent=2) + "\n",
                                              encoding="utf-8")
    evidence = [root / path for path in NATIVE_EVIDENCE]
    (root / "native-SHA256SUMS").write_text("".join(
        f"{digest(path)}  {path.relative_to(root)}\n" for path in evidence),
        encoding="utf-8")
    print(json.dumps(receipt, indent=2))


if __name__ == "__main__":
    main()
