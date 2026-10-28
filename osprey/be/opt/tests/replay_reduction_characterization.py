"""Derive R1 facts by compiling authenticated native images, without PyTorch.

Design: doc/AI-COMPILER-NATIVE-REDUCTION-CONTRACT-v0.1.md. The input receipt is
read-only; outputs are fresh and never replace earlier evidence.
"""
import argparse
import hashlib
import json
import os
from pathlib import Path
import subprocess
import sys
from reduction_trace_report import audit, audit_characterization, write_reports


def review(build, receipt, output):
    """Reopen five native inputs and audit real PREOPT-derived characterization."""
    output.mkdir(parents=True, exist_ok=False)
    (output / "logs").mkdir()
    checksums = dict((name, digest) for digest, name in
                     (row.split("  ", 1) for row in (receipt / "SHA256SUMS").read_text().splitlines()))
    env = dict(os.environ)
    env["LD_LIBRARY_PATH"] = str(build / "be") + ":" + str(build / "wopt")
    commands, cases = [], []

    def run(label, command, success=True):
        """Retain exact invocations and expected unsupported-lowering results."""
        command = [str(part) for part in command]
        result = subprocess.run(command, env=env, stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True)
        for suffix, text in (("stdout", result.stdout), ("stderr", result.stderr)):
            (output / "logs" / (label + "." + suffix)).write_text(text)
        commands.append(dict(label=label, argv=command, returncode=result.returncode))
        (output / "commands.json").write_text(json.dumps(commands, indent=2) + "\n")
        if (result.returncode == 0) != success:
            raise RuntimeError(label + ": " + result.stderr[-2000:])
        return result

    for label, rows, width in (("negative-axis", 3, 17), ("positive-axis", 3, 17),
                               ("unit", 1, 1), ("wide", 7, 65), ("driver", 3, 17)):
        relative = "native-complete/" + label + "/row_sum.B"
        data = (receipt / relative).read_bytes()
        assert hashlib.sha256(data).hexdigest() == checksums[relative]
        case = output / label
        (case / "ascii").mkdir(parents=True)
        (case / "trace").mkdir()
        image = case / "row_sum.B"
        image.write_bytes(data)
        run(label + "-reopen", [build / "ir_tools/ir_b2a", "-st", "-src", image, case / "ascii/row_sum.T"])
        for name, option in (("on", "on"), ("off", "off")) if label == "negative-axis" else (("on", "on"),):
            trace = case / "trace" / (name + ".t")
            result = run(label + "-preopt-" + name, [build / "be/be", "-fB," + str(image),
                "-fo," + str(case / (name + ".obj")), "-ft," + str(trace),
                "-PHASE:w=on:c=off:wpath=" + str(build / "wopt"), "-O2", "-DSL:wopt=on",
                "-DSL:ai_opt_trace=" + option, "-tr25", "row_sum.py"], False)
            assert "OPR_DSLREDUCESUM.v1 has no executable VHO lowering route" in result.stderr
            assert not (case / (name + ".obj")).exists()
            partial = case / (name + ".O")
            if partial.exists():
                partial.rename(case / (name + ".incomplete-mapped-output"))
            if name == "off":
                assert "AI_REDUCE" not in trace.read_text()
        write_reports(case / "trace/on.t", case / "reports", True)
        facts = audit_characterization((case / "trace/on.t").read_text(), True)
        assert len(facts) == 1 and (int(facts[0]["rows"]), int(facts[0]["width"])) == (rows, width)
        cases.append(dict(case=label, input_sha256=checksums[relative], facts=facts[0]))

    legacy_relative = "native-complete/negative-axis/trace/default.t"
    legacy = (receipt / legacy_relative).read_bytes()
    assert hashlib.sha256(legacy).hexdigest() == checksums[legacy_relative]
    (output / "legacy.t").write_bytes(legacy)
    assert not audit_characterization(legacy.decode())
    write_reports(output / "legacy.t", output / "legacy-reports")
    old_relative = "repository-source/osprey/be/opt/tests/reduction_trace_report.py"
    assert hashlib.sha256((receipt / old_relative).read_bytes()).hexdigest() == checksums[old_relative]
    run("old-reader-new-trace", [sys.executable, receipt / old_relative, "--trace",
        output / "negative-axis/trace/on.t", "--reports", output / "old-reader-reports"])
    assert len(audit((output / "negative-axis/trace/on.t").read_text())) == 1
    (output / "optimization-summary.log").write_text(
        "# What it summarizes: canonical-WHIRL reduction state/access/numerical facts and remaining optimization gates.\n"
        "P0/PREOPT characterized=5 entry_exit=identical input_provenance=authenticated_native_images\n"
        "state=per_row_fp32_sum accesses=logical numerical_permissions=strict alias=unproved\n"
        "finite=unproved overflow=unproved accuracy=unproved order=source_unresolved storage=unplanned\n"
        "legacy_readers=compatible legacy_facts=unavailable trace_off=verified\n"
        "candidate_selection=not_run profitability=not_run executable_transformation=none native_gpu=not_run\n")
    (output / "optimization.log").write_text(
        "# What it summarizes: successful semantic characterization reports for all five canonical native images.\n" +
        "".join("case=" + case["case"] + "\n" +
                (output / case["case"] / "reports/semantic.log").read_text() for case in cases))
    (output / "receipt.json").write_text(json.dumps(dict(schema="open64-r1-native-characterization-v1",
        input_receipt=str(receipt), cases=cases, derivation="canonical_whirl", executable_reduction=False,
        native_gpu_verified=False), indent=2) + "\n")
    print("five canonical-WHIRL characterization cases and historical trace compatibility passed")


def main():
    """Select existing build/input receipt and a fresh output directory."""
    parser = argparse.ArgumentParser()
    parser.add_argument("--build-dir", type=Path, required=True)
    parser.add_argument("--input-receipt", type=Path, required=True)
    parser.add_argument("--output-dir", type=Path, required=True)
    args = parser.parse_args()
    review(args.build_dir.resolve(), args.input_receipt.resolve(), args.output_dir.resolve())


if __name__ == "__main__":
    main()
