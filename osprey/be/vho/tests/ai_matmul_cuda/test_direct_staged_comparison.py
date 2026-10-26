"""Audit D/S1/T2 costs and actual direct WHIRL kernels without a GPU."""

import argparse
import difflib
import hashlib
import json
import os
from pathlib import Path
import shutil
import subprocess
import sys
import unittest

from ai_opt_report import direct_sector_bytes, parse, verify_joint, render_program
from test_executable_pipeline import HERE, REPO, audit_source, compile_case, run


class DirectComparisonTest(unittest.TestCase):
    def fixture(self):
        tile = dict(tile=2, plan=2, family="cuda_64", legality="proven", source_line=9,
                    grid=dict(ctas=1, k_iterations=1, problem=(1, 1, 1), grid=(1, 1)),
                    cta=(8, 32, 8), shared=1280, flops=2, threads=256,
                    layout=dict(bank_work=0, a_layout="row_major", thread_columns=32),
                    gmem=dict(a=32, b=32, c=32, a_issues=0),
                    smem=dict(address=0, b_issues=0, b_layout="row_major"))
        rows = [dict(id=ident, tile=2, tile_plan=2, fetch_plan=ident, engine="demand",
                     kind=kind, legality="proven", reason="none", compute=1, memory=1,
                     communication=0, sync=sync, launch=0, runtime=0, total=2 + sync,
                     buffering=buffering, capacity=232448, source_line=9)
                for ident, kind, sync, buffering in ((1, "S1", 2, 1280), (2, "D", 0, 0))]
        return (dict(budget=48, tiles=2, tma=0, direct=1, model=2), rows,
                dict(selected=2, fallback=1, count=2),
                [dict(tile=1, family="baseline", legality="proven"), tile], {2: (32, 32, 32)})

    def test_warp_broadcast_and_contiguous_worst_alignment(self):
        self.assertEqual(direct_sector_bytes(8, 32, 8, 8, 32, 8, 1, 1, 256), (2048, 10240))
        self.assertEqual(direct_sector_bytes(1, 1, 1, 8, 32, 8, 1, 1, 256), (32, 32))

    def test_direct_can_win_without_becoming_staged_fallback(self):
        result = verify_joint(*self.fixture())
        self.assertEqual(result["chosen"]["kind"], "D")
        self.assertEqual(result["fallback"]["kind"], "S1")
        self.assertEqual(result["derived"][2]["pressure"], 0)

    def test_direct_cannot_claim_shared_storage(self):
        args = self.fixture()
        args[1][1]["buffering"] = 1280
        with self.assertRaisesRegex(ValueError, "cost|lowerability"):
            verify_joint(*args)

    def test_direct_sector_corruption_is_rejected(self):
        args = self.fixture()
        args[4][2] = (64, 32, 32)
        with self.assertRaisesRegex(ValueError, "direct sectors"):
            verify_joint(*args)

    def test_direct_is_not_a_staged_fallback(self):
        args = self.fixture()
        args[2]["fallback"] = 2
        with self.assertRaisesRegex(ValueError, "fallback"):
            verify_joint(*args)


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
    for label, m, n, k in (("direct", 1, 1, 1), ("direct-long-k", 1, 1, 17),
                           ("tails", 3, 5, 7), ("multi-cta", 65, 67, 17),
                           ("staged", 128, 128, 144)):
        case = root / label
        result = compile_case(case, build, env, k, True,
                              ("-Wb,-DSL:gemm_plan_search=joint",), (m, n))
        candidates, selection, _ = parse((case / "gemm.t").read_text())
        joint = selection["joint"]
        if joint["mode"].get("model") != 2 or not joint["direct_access"]:
            raise RuntimeError("D alternatives were not retained")
        if label == "direct" and (joint["chosen"]["kind"] != "D" or
                                   selection["pipeline_kernel"]["bytes"] != 0):
            raise RuntimeError("direct fixture did not select a zero-SMEM kernel")
        if joint["fallback"]["kind"] != "S1":
            raise RuntimeError("lost staged synchronous fallback")
        trace = (case / "gemm.t").read_text()
        bad = "\n".join(line for line in trace.splitlines() if not line.startswith("AI_OPT 7 DIRECT_ACCESS "))
        try:
            parse(bad)
        except ValueError:
            pass
        else:
            raise RuntimeError("missing direct access proof accepted")
        if joint["chosen"]["kind"] == "D":
            source, dump = (case / "gemm.whirl.cu").read_text(), (case / "gemm.dump.T").read_text()
            selected = next(row for row in candidates if row["plan"] == selection["selected"])
            try:
                audit_source(source.replace("{", "{ __syncthreads();", 1), dump, selected, False, True)
            except ValueError:
                pass
            else:
                raise RuntimeError("direct source admitted a staging barrier")
            changed = source.replace(f"q < {selected['cta'][2]}", "q < 999", 1)
            if changed == source:
                raise RuntimeError("direct source mutation target missing")
            try:
                audit_source(changed, dump, selected, False, True)
            except ValueError:
                pass
            else:
                raise RuntimeError("direct source admitted inconsistent reduction bounds")
        for name, original in (("before", "gemm.B"), ("after", "gemm.device.B")):
            shutil.copy2(case / original, case / f"gemm.{name}.B")
            run([str(build / "osprey/targdir/ir_tools/ir_b2a"), "-st", "-src",
                 f"gemm.{name}.B", f"gemm.{name}.T"], case, env, f"{name}-dump.log")
        (case / "gemm.before-after.diff").write_text("".join(difflib.unified_diff(
            (case / "gemm.before.T").read_text().splitlines(True),
            (case / "gemm.after.T").read_text().splitlines(True),
            fromfile="analysis-PU.before.T", tofile="created-device-PU.after.T")))
        cases.append(dict(label=label, **result))
    case = root / "mixed-pus"
    case.mkdir()
    run([sys.executable, str(HERE / "two_matmul_fixture.py"), "two.B", "--direct-first"], case, env, "fixture.log")
    run([str(build / "osprey/targdir/driver/opencc"), f"-run-build={build}", "-O3", "-noinline",
         "-c", "-keep", "-Wb,-DSL:cuda_legacy_emit=OFF", "-Wb,-DSL:gemm_plan_search=joint",
         "-Wb,-DSL:cuda_tma_prefetch=ON", "-Wb,-DSL:cuda_whirl_output=two.device.B", "two.B"], case, env, "compiler.log")
    run([str(build / "osprey/targdir/ir_tools/ir_b2a"), "-st", "-src", "two.device.B", "two.device.T"], case, env, "device-dump.log")
    run([str(build / "osprey/targdir/whirl2c/whirl2c"), "-cuda", "-CLIST:dotc_file=two.whirl.cu",
         "-fB,two.device.B", "two.cu"], case, env, "whirl2c.log")
    run([env.get("CXX", "c++"), "-x", "c++", "-std=c++11", "-O0", "-pthread", "-I", str(HERE / "cuda_stub"),
         "-c", "two.whirl.cu", "-o", "two.host-sim.o"], case, env, "host-compile.log")
    render_program(case / "two.t", case / "reports")
    first = parse((case / "reports/pu-0.t").read_text())[1]
    second = parse((case / "reports/pu-1.t").read_text())[1]
    if first["joint"]["chosen"]["kind"] != "D" or second["joint"]["chosen"]["kind"] != "S1":
        raise RuntimeError("mixed PU plans were conflated")
    case = root / "reject-legacy-template"
    case.mkdir()
    shutil.copy2(root / "direct/gemm.B", case / "gemm.B")
    command = [str(build / "osprey/targdir/driver/opencc"), f"-run-build={build}", "-O3", "-noinline",
               "-c", "-keep", "-Wb,-DSL:gemm_plan_search=joint", "-Wb,-DSL:cuda_tile_output=bad.cu",
               "-Wb,-DSL:cuda_whirl_output=bad.device.B", "gemm.B"]
    rejection = subprocess.run(command, cwd=case, env=env, text=True,
        stdout=subprocess.PIPE, stderr=subprocess.STDOUT, timeout=240)
    (case / "compiler.log").write_text(rejection.stdout)
    (case / "commands.log").write_text(json.dumps(command) + "\n")
    if rejection.returncode == 0 or (case / "bad.cu").exists() or (case / "bad.device.B").exists() or "disable cuda_legacy_emit" not in rejection.stdout:
        raise RuntimeError("legacy-template combination was not rejected atomically")
    (root / "receipt.json").write_text(json.dumps(dict(schema="ai-d-s1-t2-v1", cases=cases,
        mixed_pus=["D", "S1"], legacy_template_rejected=True,
        native_cuda_verified=False, gpu_correctness_verified=False), indent=2) + "\n")
    (root / "SHA256SUMS").write_text("".join(
        f"{hashlib.sha256(path.read_bytes()).hexdigest()}  {path.relative_to(root)}\n"
        for path in sorted(root.rglob("*")) if path.is_file()))
    print(f"D/S1/T2 compiler review passed: {root}")


if __name__ == "__main__":
    main()
