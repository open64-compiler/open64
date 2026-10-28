"""Retain native R2 reduction binding/alternative and actual PREOPT evidence.

Design: doc/AI-COMPILER-REDUCTION-NUMERICAL-BINDING-v0.1.md.
No production selection, executable lowering or GPU run is claimed.
"""
import argparse
import json
import os
from pathlib import Path
import subprocess
import sys

from reduction_binding_trace_report import audit, audit_preopt, write_reports


def review(build, output):
    """Cross the native builder/image boundary and the real backend pipeline."""
    output = output.resolve()
    output.mkdir(parents=True, exist_ok=False)
    (output / "logs").mkdir()
    commands = []
    env = dict(os.environ, LD_LIBRARY_PATH=str(build / "be") + ":" + str(build / "wopt"))

    def run(label, command, success=True):
        """Preserve exact command vectors and expected downstream diagnostics."""
        argv = [str(item) for item in command]
        result = subprocess.run(argv, env=env, stdout=subprocess.PIPE,
                                stderr=subprocess.PIPE, text=True)
        (output / "logs" / (label + ".stdout")).write_text(result.stdout)
        (output / "logs" / (label + ".stderr")).write_text(result.stderr)
        commands.append(dict(label=label, argv=argv, returncode=result.returncode))
        (output / "commands.json").write_text(json.dumps(commands, indent=2) + "\n")
        if (result.returncode == 0) != success:
            raise RuntimeError(label + ": " + result.stderr[-3000:] + result.stdout[-1000:])
        return result

    source = Path(__file__).with_name("dsl_reduction_binding_test.cxx")
    cases = []
    for label, version, width, chunk, budget in (
            ("ordered", 2, 17, 8, 32), ("budget-rejected", 2, 17, 8, 31),
            ("unbound-v1", 1, 17, 8, 32), ("unit", 2, 1, 8, 4),
            ("wide-tail", 2, 65, 16, 64)):
        case = output / label
        (case / "ascii").mkdir(parents=True)
        (case / "trace").mkdir()
        result = run(label + "-produce", [build / "ir_tools/dsl_reduction_binding_test",
            case / "reduction.B", case / "trace/query.t", version, width, chunk, budget])
        assert "FAIL" not in result.stdout
        native_assertions = result.stdout.count(" PASS")
        binding, alternatives = audit((case / "trace/query.t").read_text())
        assert int(binding["version"]) == version and int(binding["width"]) == width
        assert int(alternatives[1]["chunk"]) == min(width, chunk)
        run(label + "-reopen", [build / "ir_tools/ir_b2a", "-st", "-src",
            case / "reduction.B", case / "ascii/reduction.T"])
        text = (case / "ascii/reduction.T").read_text()
        assert "OPR_DSLREDUCESUM" in text and "attr.reassociate=false" in text
        assert ("attr.order=ascending_left_fold" in text) == (version == 2)
        modes = [("default", "on", None)]
        if label == "ordered":
            modes += [("on", "on", "on"), ("trace-off", "on", "off"), ("preopt-off", "off", None)]
        for mode, preopt, trace in modes:
            command = [build / "be/be", "-fB," + str(case / "reduction.B"),
                "-fo," + str(case / (mode + ".obj")), "-ft," + str(case / "trace" / (mode + ".t")),
                "-PHASE:w=on:c=off:wpath=" + str(build / "wopt"), "-O2",
                "-DSL:wopt=" + preopt, "-WOPT:cr_simp=on", "-tr25"]
            if trace:
                command += ["-DSL:ai_opt_trace=" + trace]
            result = run(label + "-" + mode, command + [source], success=False)
            assert ("OPR_DSLREDUCESUM.v%d has no executable VHO lowering route" % version) in result.stderr
            assert not (case / (mode + ".obj")).exists()
            partial = case / (mode + ".O")
            if partial.exists():
                partial.rename(case / (mode + ".incomplete-mapped-output"))
            raw = (case / "trace" / (mode + ".t")).read_text()
            if mode in ("trace-off", "preopt-off"):
                assert "AI_REDUCE_NUM " not in raw and "AI_REDUCE_ALT " not in raw
            else:
                after = audit_preopt(raw)
                assert {k: v for k, v in after.items() if k != "phase"} == {
                    k: v for k, v in binding.items() if k != "phase"}
        write_reports(case / "trace/query.t", case / "trace/default.t", case / "reports")
        cases.append(dict(case=label, version=version, width=width, chunk=chunk, budget=budget,
                          native_assertions=native_assertions, binding=binding, alternatives=alternatives))
    run("numerical-witness", [sys.executable, source.with_name("reduction_binding_numerics.py"),
                             "--output-dir", output / "numerical"])
    (output / "optimization.log").write_text(
        "# What it summarizes: all successful R2 source-binding and alternative-discovery reports.\n" +
        "".join("case=" + case["case"] + "\n" + (output / case["case"] / "reports/optimization.log").read_text()
                for case in cases))
    (output / "optimization-summary.log").write_text(
        "# What it summarizes: reduction numerical binding, recipe legality and remaining executable gates.\n"
        "native_cases=5 ordered_v2=4 unbound_v1=1 PREOPT=preserved trace_controls=verified\n"
        "recipes=direct_ordered,staged_ordered partition_tree=forbidden staging_budget=checked\n"
        "host_oracle_cases=13 host_recipe_checks=104 exact_integer_pair_checks=2048\n"
        "candidate_selection=not_run profitability=not_run transformation=none\n"
        "source_torch_sum_upgrade=forbidden target_arithmetic=unbound native_execution=not_run gpu=not_run\n")
    (output / "receipt.json").write_text(json.dumps(dict(schema=1, cases=cases,
        native_preopt_verified=True, target_bound=False, selected=False, transformation="none",
        native_execution=False, gpu=False), indent=2) + "\n")
    print("R2 reduction binding review passed: five native cases")


def main():
    """Choose a new output root; never overwrite an accepted receipt."""
    parser = argparse.ArgumentParser()
    parser.add_argument("--build-dir", required=True, type=Path)
    parser.add_argument("--output-dir", required=True, type=Path)
    args = parser.parse_args()
    review(args.build_dir.resolve(), args.output_dir)


if __name__ == "__main__":
    main()
