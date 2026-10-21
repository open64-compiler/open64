"""Build and measure an actual Open64 -O0 direct F32 matmul PU.

This runner is CPU-specific and requires a matching Linux/x86-64 Open64 build.
It never treats a Python or C-only stand-in as compiler performance evidence.
"""

from __future__ import annotations

import argparse
import csv
import hashlib
import json
import os
from pathlib import Path
import platform
import shutil
import statistics
import subprocess
import sys
import tempfile


HERE = Path(__file__).resolve().parent
REPO = HERE.parents[4]


def digest(path: Path) -> str:
    hasher = hashlib.sha256()
    with path.open("rb") as stream:
        for block in iter(lambda: stream.read(1024 * 1024), b""):
            hasher.update(block)
    return hasher.hexdigest()


def run(command: list[str], cwd: Path, log: Path, env: dict[str, str] | None = None) -> None:
    with log.open("w", encoding="utf-8") as stream:
        stream.write("command=" + json.dumps(command) + "\n")
        stream.flush()
        completed = subprocess.run(
            command, cwd=cwd, env=env, stdout=stream, stderr=subprocess.STDOUT,
            check=False,
        )
        stream.write(f"exit_status={completed.returncode}\n")
    if completed.returncode:
        raise RuntimeError(f"command failed ({completed.returncode}): {log}")


def checked_executable(path: Path) -> Path:
    if not path.is_file() or not os.access(path, os.X_OK):
        raise FileNotFoundError(f"required executable is missing: {path}")
    # Open64 dispatches by argv[0]; resolving opencc to driver changes language.
    return path


def cpu_identity() -> tuple[str, str]:
    cpuinfo = Path("/proc/cpuinfo").read_text(encoding="utf-8", errors="replace")
    fields = {}
    for line in cpuinfo.splitlines():
        if ":" in line:
            key, value = line.split(":", 1)
            if key.strip() in ("vendor_id", "model name"):
                fields[key.strip()] = value.strip()
    return fields.get("vendor_id", "unknown").strip(), fields.get("model name", "unknown").strip()


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--build-dir", type=Path, required=True)
    parser.add_argument("--output-dir", type=Path, required=True)
    parser.add_argument("--cc", default="cc")
    parser.add_argument("--allow-emulated-smoke", action="store_true")
    args = parser.parse_args()

    if sys.platform != "linux" or platform.machine() != "x86_64":
        parser.error("m0 execution requires Linux/x86-64; do not substitute host timing")
    cpu_vendor, cpu_model = cpu_identity()
    emulated = cpu_vendor == "VirtualApple" or "QEMU" in cpu_model
    if emulated and not args.allow_emulated_smoke:
        parser.error("emulated x86-64 is functional smoke only; pass --allow-emulated-smoke")
    build = args.build_dir.resolve()
    opencc = checked_executable(build / "osprey/targdir/driver/opencc")
    ir_b2a = checked_executable(build / "osprey/targdir/ir_tools/ir_b2a")
    cc_path = shutil.which(args.cc)
    if cc_path is None:
        parser.error(f"C compiler not found: {args.cc}")
    objcopy = shutil.which("objcopy")
    if objcopy is None:
        parser.error("objcopy is required to expose the frontend's local PU")

    output = args.output_dir.resolve()
    if output.exists():
        parser.error(f"output directory already exists: {output}")
    output.parent.mkdir(parents=True, exist_ok=True)
    stage = Path(tempfile.mkdtemp(prefix=".ai-matmul-m0-", dir=output.parent))
    try:
        source = stage / "source"
        binary = stage / "binary"
        traces = stage / "traces"
        measurements = stage / "measurements"
        logs = stage / "logs"
        for directory in (source, binary, traces, measurements, logs):
            directory.mkdir()
        for name in ("fixture.py", "runtime.c", "runtime.h", "benchmark.c"):
            shutil.copy2(HERE / name, source / name)

        python_path = os.pathsep.join(filter(None, (
            str(build / "osprey/targdir/torch2whirl/python"),
            str(REPO / "osprey/torch2whirl/python"),
            os.environ.get("PYTHONPATH", ""),
        )))
        env = dict(os.environ, PYTHONPATH=python_path)
        image = binary / "matmul.B"
        run([sys.executable, str(HERE / "fixture.py"), str(image)],
            stage, logs / "producer.log", env)
        if not image.is_file() or not image.stat().st_size:
            raise RuntimeError("producer did not emit binary WHIRL")
        trace = traces / "matmul.T"
        run([str(ir_b2a), "-st", "-src", str(image), str(trace)],
            stage, logs / "ir_b2a.log")
        if "common.matmul" not in trace.read_text(encoding="utf-8"):
            raise RuntimeError("separate-process WHIRL trace lacks common.matmul")

        run([str(opencc), f"-run-build={build}", "-O0", "-INLINE:none",
             "-c", "-keep", "matmul.B"], binary, logs / "opencc.log")
        obj = binary / "matmul.o"
        if not obj.is_file() or not obj.stat().st_size:
            raise RuntimeError("Open64 did not emit the direct matmul object")
        linked_obj = binary / "matmul.link.o"
        run([objcopy, "--globalize-symbol=ai_matmul_m0", str(obj),
             str(linked_obj)], stage, logs / "objcopy.log")
        executable = binary / "matmul-m0"
        run([cc_path, "-std=c11", "-O2", "-fno-fast-math",
             "-ffp-contract=off", "-I", str(REPO / "osprey/include"),
             str(source / "runtime.c"), str(source / "benchmark.c"),
             str(linked_obj), "-no-pie", "-o", str(executable)],
            stage, logs / "link.log")
        raw = measurements / "raw.csv"
        with raw.open("w", encoding="utf-8") as stream:
            completed = subprocess.run([str(executable)], cwd=stage,
                                       stdout=stream, stderr=subprocess.PIPE,
                                       text=True, check=False)
        (logs / "benchmark.log").write_text(
            f"command={json.dumps([str(executable)])}\n"
            f"exit_status={completed.returncode}\n{completed.stderr}",
            encoding="utf-8",
        )
        if completed.returncode:
            raise RuntimeError("benchmark or numerical verification failed")
        with raw.open(newline="", encoding="utf-8") as stream:
            rows = list(csv.DictReader(stream))
        if len(rows) != 30 or any(
            int(row["sample"]) != index or int(row["elapsed_ns"]) <= 0
            for index, row in enumerate(rows)
        ):
            raise RuntimeError("raw timing CSV is incomplete or malformed")

        samples = [int(row["elapsed_ns"]) for row in rows]
        receipt = {
            "schema": "ai-matmul-m0-cpu-v1",
            "status": ("functional_smoke_emulated" if emulated else
                       "correctness_qualified_cpu_baseline"),
            "measurement_quality": ("not_for_profitability_emulated" if emulated
                                    else "target_specific_cpu"),
            "shape": [128, 128, 128],
            "dtype": "F32",
            "scope": "full compiled PU: two splat constants plus direct matmul",
            "link_note": "objcopy globalizes only the test copy of the local PU symbol",
            "optimization": "-O0",
            "target": {"system": platform.system(), "machine": platform.machine(),
                       "platform": platform.platform(), "cpu_vendor": cpu_vendor,
                       "cpu_model": cpu_model},
            "compiler": {"opencc_sha256": digest(opencc),
                         "ir_b2a_sha256": digest(ir_b2a)},
            "sample_count": len(samples),
            "median_ns": statistics.median(samples),
            "min_ns": min(samples),
            "max_ns": max(samples),
            "ai_decision_trace": "unavailable_pre_m1",
            "numerical_check": "all 16384 elements exactly 128.0f on every run",
        }
        (stage / "receipt.json").write_text(
            json.dumps(receipt, indent=2, sort_keys=True) + "\n", encoding="utf-8"
        )
        files = sorted(path for path in stage.rglob("*") if path.is_file())
        (stage / "SHA256SUMS").write_text(
            "".join(f"{digest(path)}  {path.relative_to(stage)}\n" for path in files),
            encoding="utf-8",
        )
        stage.rename(output)
    except Exception:
        print(f"failed run retained for diagnosis: {stage}", file=sys.stderr)
        raise
    label = "m0 emulated functional smoke" if emulated else "m0 CPU baseline evidence"
    print(f"{label}: {output}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
