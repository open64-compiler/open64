"""Rerun the packaged compiled m0 PU on native Linux/x86-64."""

from __future__ import annotations

import argparse
import csv
import hashlib
import json
import platform
from pathlib import Path
import shutil
import statistics
import subprocess


def sha256(path: Path) -> str:
    return hashlib.sha256(path.read_bytes()).hexdigest()


def validate_samples(rows: list[dict[str, str]]) -> list[int]:
    if (len(rows) != 30 or
            any(int(row["sample"]) != index or int(row["elapsed_ns"]) <= 0
                for index, row in enumerate(rows))):
        raise ValueError("native m0 timing CSV is incomplete or malformed")
    return [int(row["elapsed_ns"]) for row in rows]


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--artifact-dir", type=Path, required=True)
    args = parser.parse_args()
    root = args.artifact_dir.resolve()
    if (root / "receipt.json").exists():
        parser.error("native receipt already exists; prepare a new artifact")
    if platform.system() != "Linux" or platform.machine() != "x86_64":
        parser.error("native Linux/x86-64 is required")
    cpuinfo = Path("/proc/cpuinfo").read_text(encoding="utf-8",
                                           errors="replace")
    if "VirtualApple" in cpuinfo or "QEMU" in cpuinfo:
        parser.error("emulated x86-64 cannot certify the native baseline")
    metadata = json.loads((root / "native.json").read_text(encoding="utf-8"))
    executable = root / "binary/matmul-m0"
    if (metadata.get("schema") != "ai-matmul-m0-native-package-v1" or
            sha256(executable) != metadata["executable_sha256"] or
            sha256(root / "binary/matmul.B") != metadata["input_whirl_sha256"] or
            sha256(root / "traces/matmul.T") != metadata["ir_trace_sha256"] or
            sha256(root / "provenance/emulated-receipt.json") !=
                metadata["source_receipt_sha256"]):
        parser.error("native package failed provenance checks")
    (root / "logs").mkdir(exist_ok=True)
    (root / "measurements").mkdir(exist_ok=True)
    (root / "reports").mkdir(exist_ok=True)
    cpu_lines = [line for line in cpuinfo.splitlines()
                 if line.startswith(("vendor_id", "model name"))]
    (root / "reports/cpu.txt").write_text("\n".join(cpu_lines[:2]) + "\n",
                                           encoding="utf-8")
    smi = shutil.which("nvidia-smi")
    if smi is not None:
        device = subprocess.run(
            [smi, "--query-gpu=name,compute_cap,driver_version,memory.total",
             "--format=csv,noheader"], text=True, capture_output=True,
            check=False)
        (root / "reports/device.txt").write_text(device.stdout + device.stderr,
                                                  encoding="utf-8")
    command = [str(executable)]
    with (root / "measurements/raw.csv").open("w", encoding="utf-8") as out, \
            (root / "logs/run.log").open("w", encoding="utf-8") as err:
        result = subprocess.run(command, cwd=root, stdout=out, stderr=err,
                                check=False)
        err.write(f"exit_status={result.returncode}\n")
    if result.returncode:
        raise RuntimeError("compiled m0 PU failed native correctness or timing")
    with (root / "measurements/raw.csv").open(newline="", encoding="utf-8") as stream:
        samples = validate_samples(list(csv.DictReader(stream)))
    receipt = {
        "schema": "ai-matmul-m0-native-v1",
        "status": "native_compiled_pu_correctness_and_timing_recorded",
        "review_accepted": False,
        "target": {"system": platform.system(), "machine": platform.machine(),
                   "platform": platform.platform(),
                   "cpu_identity": cpu_lines[:2]},
        "compiled_executable_sha256": metadata["executable_sha256"],
        "source_receipt_sha256": metadata["source_receipt_sha256"],
        "command": command,
        "numerical_check": "compiled PU benchmark validates all 16384 elements on every run",
        "sample_count": len(samples),
        "median_ns": statistics.median(samples),
        "min_ns": min(samples),
        "max_ns": max(samples),
        "measurement_scope": "full compiled PU; two splat constants plus direct matmul",
    }
    (root / "receipt.json").write_text(json.dumps(receipt, indent=2) + "\n",
                                       encoding="utf-8")
    files = sorted(path for path in root.rglob("*")
                   if path.is_file() and path.name != "SHA256SUMS")
    (root / "SHA256SUMS").write_text(
        "".join(f"{sha256(path)}  {path.relative_to(root)}\n" for path in files),
        encoding="utf-8")
    print(f"native m0 evidence ready for review: {root}")


if __name__ == "__main__":
    main()
