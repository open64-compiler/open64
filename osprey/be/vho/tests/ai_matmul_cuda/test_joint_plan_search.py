"""Retain joint plan decisions, CUDA/WHIRL, and host-only executable evidence."""

import argparse
import hashlib
import difflib
import json
import os
import re
from pathlib import Path
import subprocess
import shutil
import sys
import unittest

from ai_opt_report import parse, verify_joint
from test_executable_pipeline import HERE, REPO, compile_case, run


class JointTraceTest(unittest.TestCase):
    def fixture(self, long=False):
        steps = 64 if long else 1
        tile = dict(tile=2, plan=2, family="cuda_64", legality="proven", source_line=7,
                    grid=dict(ctas=4, k_iterations=steps, problem=(128, 128, 16 * steps), grid=(2, 2)),
                    cta=(64, 64, 16), shared=8192, flops=524288 * steps,
                    layout=dict(bank_work=0, a_layout="row_major"),
                    gmem=dict(a=16384 * steps, b=16384 * steps, c=65536, a_issues=0),
                    smem=dict(address=0, b_issues=0, b_layout="row_major"))
        rows = []
        for tma in (False, True):
            # Frozen independent oracle values for startup-only and steady state.
            compute, memory, sync, launch = ((32768 + 1229, 2112, 512, 0) if long and not tma else
                (32768 + 2318, 96, 516, 128) if long else
                (533, 96, 8, 0) if not tma else (554, 96, 12, 128))
            rows.append(dict(id=1 + int(tma), tile=2, tile_plan=2, fetch_plan=1 + int(tma),
                engine="tma_like" if tma else "demand", legality="proven", reason="none",
                compute=compute, memory=memory, communication=0, sync=sync, launch=launch,
                runtime=0, total=compute + memory + sync + launch,
                buffering=16384 if tma else 8192, capacity=232448, source_line=7))
        decision = dict(selected=2 if long else 1, fallback=1, count=2)
        baseline = dict(tile=1, family="baseline", legality="proven")
        return dict(budget=32, tiles=2, tma=1), rows, decision, [baseline, tile]

    def test_startup_and_steady_state_can_select_different_engines(self):
        for long in (False, True):
            result = verify_joint(*self.fixture(long))
            self.assertEqual(result["chosen"]["engine"], "tma_like" if long else "demand")
            self.assertEqual(result["fallback"]["engine"], "demand")

    def test_corrupt_complete_cost_is_rejected(self):
        mode, rows, decision, tiles = self.fixture()
        rows[0]["compute"] += 1
        rows[0]["total"] += 1
        with self.assertRaisesRegex(ValueError, "cost"):
            verify_joint(mode, rows, decision, tiles)

    def test_missing_alternative_or_fallback_is_rejected(self):
        for field in ("census", "fallback"):
            mode, rows, decision, tiles = self.fixture(True)
            if field == "census":
                rows.pop()
                decision["count"] -= 1
            else:
                decision["fallback"] = 2
            with self.assertRaises(ValueError):
                verify_joint(mode, rows, decision, tiles)

    def test_illegal_tma_layout_cannot_win(self):
        mode, rows, decision, tiles = self.fixture(True)
        tiles[-1]["smem"]["b_layout"] = "xor_columns"
        with self.assertRaisesRegex(ValueError, "lowerability"):
            verify_joint(mode, rows, decision, tiles)

    def test_partial_census_is_rejected(self):
        with self.assertRaisesRegex(ValueError, "census"):
            verify_joint(None, [], None, [])

    def test_joint_trace_needs_selection(self):
        with self.assertRaises(ValueError):
            parse("AI_OPT 7 JOINT_MODE joint 32 2 1 relative low model=1\n")


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--build-dir", type=Path, required=True)
    parser.add_argument("--output-dir", type=Path, required=True)
    args = parser.parse_args()
    root = args.output_dir.resolve()
    root.mkdir(parents=True, exist_ok=False)
    build = args.build_dir.resolve()
    env = dict(os.environ)
    env["PYTHONPATH"] = os.pathsep.join((str(build / "osprey/targdir/torch2whirl/python"),
                                       str(REPO / "osprey/torch2whirl/python")))
    cases = []
    for label, k, tma, extra in (
        ("short", 16, True, ()), ("long", 144, True, ()),
        ("edges", 135, True, ()), ("tma-off", 144, False, ()),
        ("layout-off", 144, True, ("-Wb,-DSL:cuda_smem_layout_search=OFF",)),
        ("applied-sync", 144, True, ("-Wb,-DSL:cuda_tile_apply=ON",)),
        ("repeat", 144, True, ()),
    ):
        result = compile_case(root / label, build, env, k, tma,
                              ("-Wb,-DSL:gemm_plan_search=joint", "-Wb,-DSL:gemm_cost_model=relative", *extra))
        case = root / label
        # This is semantic source -> newly created device PU, not an in-place
        # mutation of the analysis/host PU. Preserve both mapped boundaries.
        for name, original in (("before", "gemm.B"), ("after", "gemm.device.B")):
            shutil.copy2(case / original, case / f"gemm.{name}.B")
            run([str(build / "osprey/targdir/ir_tools/ir_b2a"), "-st", "-src",
                 f"gemm.{name}.B", f"gemm.{name}.T"], case, env, f"{name}-dump.log")
        (case / "gemm.before-after.diff").write_text("".join(difflib.unified_diff(
            (case / "gemm.before.T").read_text().splitlines(True),
            (case / "gemm.after.T").read_text().splitlines(True),
            fromfile="semantic-analysis-PU.before.T", tofile="created-device-PU.after.T")))
        joint = result["joint"]
        if not joint or joint["fallback"]["engine"] != "demand":
            raise RuntimeError("missing synchronous fallback")
        if label in ("short", "edges", "tma-off", "applied-sync") and joint["chosen"]["engine"] != "demand":
            raise RuntimeError("short/edge/disabled TMA did not retain synchronous execution")
        trace = (root / label / "gemm.t").read_text()
        lines = trace.splitlines()
        for kind in ("JOINT_CAND", "JOINT_SELECT"):
            broken = "\n".join(line for line in lines if not line.startswith("AI_OPT 7 " + kind + " "))
            try:
                parse(broken)
            except ValueError:
                pass
            else:
                raise RuntimeError("incomplete joint trace accepted")
        chosen = joint["selection"]["selected"]
        fallback = joint["selection"]["fallback"]
        for replacement in (f"{fallback} {chosen}", "99999 99999"):
            broken = trace.replace(f"JOINT_SELECT {chosen} {fallback}", "JOINT_SELECT " + replacement)
            if broken == trace:
                continue
            try:
                parse(broken)
            except ValueError:
                pass
            else:
                raise RuntimeError("corrupt joint decision accepted")
        cases.append(dict(label=label, **result))
    if cases[1]["joint"] != cases[-1]["joint"]:
        raise RuntimeError("joint search is nondeterministic")
    # Explicit baseline must preserve both plan evidence and emitted kernel.
    for label, options in (("default", ()), ("baseline", ("-Wb,-DSL:gemm_plan_search=baseline",))):
        compile_case(root / label, build, env, 144, False, options)
    def normalized_source(path):
        return re.sub(r" \* C file translated from WHIRL[^\n]*", " * C file translated from WHIRL <timestamp>", path.read_text())

    if normalized_source(root / "default/gemm.whirl.cu") != normalized_source(root / "baseline/gemm.whirl.cu"):
        raise RuntimeError("baseline mode changed generated CUDA")
    rejected = []
    for label, option, level in (("invalid", "typo", "-O3"), ("o0", "joint", "-O0"),
                                 ("no-output", "joint", "-O3")):
        case = root / label
        case.mkdir()
        run([sys.executable, str(HERE / "edge_matmul_fixture.py"), "gemm.B", "128", "128", "16"], case, env, "fixture.log")
        command = [str(build / "osprey/targdir/driver/opencc"), f"-run-build={build}", level,
                   "-noinline", "-c", "-keep", f"-Wb,-DSL:gemm_plan_search={option}"]
        if label != "no-output":
            command.append("-Wb,-DSL:cuda_whirl_output=gemm.device.B")
        command.append("gemm.B")
        result = subprocess.run(command, cwd=case, env=env, text=True,
                                stdout=subprocess.PIPE, stderr=subprocess.STDOUT, timeout=240)
        (case / "rejection.log").write_text(result.stdout)
        (case / "commands.log").write_text(json.dumps(command) + "\n")
        if result.returncode == 0 or "gemm_plan_search" not in result.stdout or (case / "gemm.device.B").exists():
            raise RuntimeError("invalid joint control did not reject atomically")
        rejected.append(label)
    case = root / "trace-off"
    case.mkdir()
    shutil.copy2(root / "short/gemm.B", case / "gemm.B")
    run([str(build / "osprey/targdir/driver/opencc"), f"-run-build={build}", "-O3",
         "-noinline", "-S", "-keep", "-Wb,-DSL:gemm_plan_search=joint",
         "-Wb,-DSL:ai_opt_trace=OFF", "-Wb,-DSL:cuda_tma_prefetch=ON",
         "-Wb,-DSL:cuda_whirl_output=gemm.device.B", "gemm.B"], case, env, "compiler.log")
    if not (case / "gemm.device.B").exists() or ((case / "gemm.t").exists() and
            "AI_OPT " in (case / "gemm.t").read_text()):
        raise RuntimeError("joint search bypassed trace option control")
    (root / "receipt.json").write_text(json.dumps(dict(schema="ai-joint-plan-v1", cases=cases,
        rejected=rejected, baseline_parity=True, native_cuda_verified=False), indent=2) + "\n")
    (root / "SHA256SUMS").write_text("".join(
        f"{hashlib.sha256(path.read_bytes()).hexdigest()}  {path.relative_to(root)}\n"
        for path in sorted(root.rglob("*")) if path.is_file()))
    print(f"Joint whole-plan review passed: {root}")


if __name__ == "__main__":
    main()
