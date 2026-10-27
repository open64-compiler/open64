"""Retain actual compiler analytical selections, WHIRL/CUDA and host-only tests."""

import argparse
import difflib
import hashlib
import json
import os
from pathlib import Path
import shutil
import subprocess
import sys

from ai_opt_report import parse
from test_executable_pipeline import HERE, REPO, compile_case, run


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--build-dir", type=Path, required=True)
    parser.add_argument("--output-dir", type=Path, required=True)
    args = parser.parse_args()
    root, build = args.output_dir.resolve(), args.build_dir.resolve()
    root.mkdir(parents=True, exist_ok=False)
    env = dict(os.environ)
    env["PYTHONPATH"] = os.pathsep.join((str(build / "osprey/targdir/torch2whirl/python"),
                                       str(REPO / "osprey/torch2whirl/python")))
    cases = []
    specs = [(label, m, n, k, 100, None, True) for label, m, n, k in (
        ("direct", 1, 1, 1), ("direct-long-k", 1, 1, 17), ("tails", 3, 5, 7),
        ("multi-cta", 65, 67, 17), ("staged", 128, 128, 144))]
    specs += [("staged-low", 128, 128, 144, 20, None, True),
              ("staged-high", 128, 128, 144, 400, None, True),
              ("staged-relative", 128, 128, 144, 100, "relative", True),
              ("staged-explicit", 128, 128, 144, 100, "readiness", True),
              ("tma-off", 128, 128, 144, 100, None, False)]
    for label, m, n, k, latency, model, tma in specs:
        options = ["-Wb,-DSL:gemm_plan_search=joint", f"-Wb,-DSL:gemm_memory_latency={latency}"]
        if model:
            options.append(f"-Wb,-DSL:gemm_cost_model={model}")
        case = root / label
        result = compile_case(case, build, env, k, tma, options, (m, n))
        trace = (case / "gemm.t").read_text()
        joint = result["joint"]
        if joint["mode"]["model"] != (3 if model == "relative" else 4):
            raise RuntimeError("wrong selection model")
        if model != "relative":
            for kind in ("READINESS_PROFILE", "JOINT_READINESS"):
                rows = [line for line in trace.splitlines() if line.startswith(f"AI_OPT 7 {kind} ")]
                corrupt = rows[0].split()
                index = 4
                corrupt[index] = str(int(corrupt[index]) + 1)
                for broken in (trace.replace(rows[0] + "\n", ""), trace + rows[0] + "\n",
                               trace.replace(rows[0], " ".join(corrupt))):
                    try:
                        parse(broken)
                    except ValueError:
                        pass
                    else:
                        raise RuntimeError("missing/duplicate/corrupt readiness accepted")
        for name, original in (("before", "gemm.B"), ("after", "gemm.device.B")):
            shutil.copy2(case / original, case / f"gemm.{name}.B")
            run([str(build / "osprey/targdir/ir_tools/ir_b2a"), "-st", "-src",
                 f"gemm.{name}.B", f"gemm.{name}.T"], case, env, f"{name}-dump.log")
        (case / "gemm.before-after.diff").write_text("".join(difflib.unified_diff(
            (case / "gemm.before.T").read_text().splitlines(True),
            (case / "gemm.after.T").read_text().splitlines(True),
            fromfile="analysis-PU.before.T", tofile="device-PU.after.T")))
        cases.append(dict(label=label, **result))
    staged = next(case for case in cases if case["label"] == "staged")
    explicit = next(case for case in cases if case["label"] == "staged-explicit")
    if staged["joint"] != explicit["joint"]:
        raise RuntimeError("default/explicit readiness selection mismatch")
    case = root / "invalid-model"
    case.mkdir()
    shutil.copy2(root / "direct/gemm.B", case / "gemm.B")
    command = [str(build / "osprey/targdir/driver/opencc"), f"-run-build={build}", "-O3", "-noinline", "-S", "-keep",
               "-Wb,-DSL:gemm_plan_search=joint", "-Wb,-DSL:gemm_cost_model=typo",
               "-Wb,-DSL:cuda_whirl_output=bad.device.B", "gemm.B"]
    result = subprocess.run(command, cwd=case, env=env, text=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
    (case / "compiler.log").write_text(result.stdout)
    (case / "commands.log").write_text(json.dumps(command) + "\n")
    if result.returncode == 0 or "gemm_cost_model" not in result.stdout or (case / "bad.device.B").exists():
        raise RuntimeError("invalid model did not reject atomically")
    (root / "receipt.json").write_text(json.dumps(dict(schema="analytical-readiness-v1", cases=cases,
        invalid_model_rejected=True, native_gpu_verified=False, calibration="none"), indent=2) + "\n")
    (root / "SHA256SUMS").write_text("".join(
        f"{hashlib.sha256(path.read_bytes()).hexdigest()}  {path.relative_to(root)}\n"
        for path in sorted(root.rglob("*")) if path.is_file()))
    print(f"Analytical readiness compiler/source review passed: {root}")


if __name__ == "__main__":
    main()
