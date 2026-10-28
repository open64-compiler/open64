"""Retain direct reduction target/storage/event compiler-contract evidence.

Design: doc/AI-COMPILER-REDUCTION-DIRECT-BINDING-v0.1.md. No lowering or GPU.
"""
import argparse
import json
import os
from pathlib import Path
import subprocess

from reduction_binding_trace_report import audit_preopt
from reduction_direct_trace_report import audit, write_reports


def review(build, output):
    """Cross native source/image, direct query and real PREOPT boundaries."""
    output = output.resolve()
    output.mkdir(parents=True, exist_ok=False)
    (output / "logs").mkdir()
    commands = []
    env = dict(os.environ, LD_LIBRARY_PATH=str(build / "be") + ":" + str(build / "wopt"))

    def run(label, command, success=True):
        """Record both streams and the precise expected downstream rejection."""
        argv = [str(item) for item in command]
        result = subprocess.run(argv, env=env, stdout=subprocess.PIPE,
                                stderr=subprocess.PIPE, text=True)
        (output / "logs" / (label + ".stdout")).write_text(result.stdout)
        (output / "logs" / (label + ".stderr")).write_text(result.stderr)
        commands.append(dict(label=label, argv=argv, returncode=result.returncode))
        (output / "commands.json").write_text(json.dumps(commands, indent=2) + "\n")
        if (result.returncode == 0) != success:
            raise RuntimeError(label + ": " + result.stderr[-2500:] + result.stdout[-1000:])
        return result

    source = Path(__file__).with_name("dsl_reduction_direct_test.cxx")
    cases = []
    variants = (("hopper", "sm_90", 2, 17, "row_major"),
                ("hopper-a", "sm_90a", 2, 17, "row_major"),
                ("blackwell", "sm_100", 2, 17, "row_major"),
                ("blackwell-a", "sm_100a", 2, 17, "row_major"),
                ("unbound-v1", "sm_90", 1, 17, "row_major"),
                ("unit-width", "sm_90", 2, 1, "row_major"),
                ("wide", "sm_100", 2, 65, "row_major"),
                ("unsupported-layout", "sm_90", 2, 17, "column_major"))
    for label, arch, version, width, layout in variants:
        case = output / label
        (case / "ascii").mkdir(parents=True)
        (case / "trace").mkdir()
        result = run(label + "-produce", [build / "ir_tools/dsl_reduction_direct_test",
            case / "reduction.B", case / "trace/direct.t", arch, version, width, layout,
            case / "guard.bin"])
        assert "FAIL" not in result.stdout
        assertions = result.stdout.count(" PASS")
        rows = audit((case / "trace/direct.t").read_text())
        bound = version == 2 and layout == "row_major"
        assert len(rows) == (35 if bound else 1)
        assert rows[0]["recipe"] == version and rows[0]["width"] == width
        assert rows[0]["status"] == ("bound_runtime_obligations" if bound else
            ("source_binding_missing" if version == 1 else "unsupported_layout"))
        run(label + "-reopen", [build / "ir_tools/ir_b2a", "-st", "-src",
            case / "reduction.B", case / "ascii/reduction.T"])
        text = (case / "ascii/reduction.T").read_text()
        assert "OPR_DSLREDUCESUM" in text and "attr.reassociate=false" in text
        modes = [("default", "on", None)]
        if label == "hopper":
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
                assert "AI_REDUCE_NUM " not in raw
            else:
                after = audit_preopt(raw)
                for key in ("node", "input", "result", "rows", "width", "source_line"):
                    assert after[key] == str(rows[0][key])
                assert after["version"] == str(version)
        write_reports(case / "trace/direct.t", case / "trace/default.t", case / "reports")
        cases.append(dict(case=label, architecture=arch, version=version, width=width,
                          layout=layout, assertions=assertions, rows=len(rows),
                          bound_contracts=sum(row["status"] == "bound_runtime_obligations" for row in rows)))
    (output / "optimization.log").write_text(
        "# What it summarizes: all successful direct reduction contract reports and recorded refusals.\n" +
        "".join("case=" + case["case"] + "\n" + (output / case["case"] / "reports/optimization.log").read_text()
                for case in cases))
    (output / "optimization-summary.log").write_text(
        "# What it summarizes: direct ordered reduction target/storage/event contract coverage and open gates.\n"
        "native_cases=8 architecture_lookup=sm_90,sm_90a,sm_100,sm_100a PREOPT=source_preserved\n"
        "v1_upgrade=forbidden unsupported_layout=refused relative_ranges=checked ownership=one_thread_per_row\n"
        "events=dependency_dag_validated runtime_lease_and_event_enforcement=required\n"
        "candidate_selection=not_run profitability=not_run transformation=none\n"
        "lowering=not_implemented runtime_adapter=not_implemented PTX_SASS_certification=not_run gpu=not_run\n")
    (output / "receipt.json").write_text(json.dumps(dict(schema=1, cases=cases,
        runtime_enforcement=False, native_execution=False, selected=False, transformation="none",
        gpu=False), indent=2) + "\n")
    print("direct reduction binding review passed: eight native cases")


def main():
    """Choose a fresh review root; never replace previous artifacts."""
    parser = argparse.ArgumentParser()
    parser.add_argument("--build-dir", required=True, type=Path)
    parser.add_argument("--output-dir", required=True, type=Path)
    args = parser.parse_args()
    review(args.build_dir.resolve(), args.output_dir)


if __name__ == "__main__":
    main()
