"""Python-produced native row-sum images, PREOPT traces and strict reports.

Design: doc/AI-COMPILER-NATIVE-REDUCTION-CONTRACT-v0.1.md. Every producer runs
in a fresh process. This certifies ingestion/preservation, not GPU execution.
"""
import argparse
import hashlib
import json
import os
from pathlib import Path
import subprocess
import sys


def produce(root, rows, width, axis, reject):
    """Emit via public frontend APIs or verify explicit rejection without .B."""
    import torch
    from open64_dsc import WhirlExportOptions, export_to_whirl, save_as_whirl
    from test_row_sum_capture_optional import Sum, Composed
    root.mkdir(parents=True, exist_ok=False)
    model = Sum(axis)
    value = (torch.arange(rows * width, dtype=torch.float32) % 7 - 3).reshape(rows, width)
    if reject == "fp64":
        value = value.double()
    elif reject == "keepdim":
        model = Sum(axis, True)
    elif reject == "axis":
        model = Sum(0)
    elif reject == "composed":
        model = Composed()
    image = root / "row_sum.B"
    try:
        module = export_to_whirl(model, (value,), WhirlExportOptions(backend="native"))
        if reject:
            raise AssertionError("unsupported row sum was accepted")
        save_as_whirl(module, str(image))
    except NotImplementedError as exc:
        if not reject or image.exists():
            raise
        (root / "rejection.txt").write_text(str(exc) + "\n")
        return
    reference = value.double().sum(dim=axis)
    actual = model(value)
    assert torch.equal(actual.double(), reference)
    metadata = module.graph_operators[0].metadata
    source_lines = Path(metadata["source_file"]).read_text().splitlines()
    source_line = int(metadata["source_line"])
    assert "return torch.sum" in source_lines[source_line - 1]
    (root / "manifest.json").write_text(json.dumps(module.to_manifest(), indent=2) + "\n")
    (root / "source-witness.json").write_text(json.dumps(dict(
        rows=rows, width=width, source_axis=axis, canonical_axis=1,
        source_file=metadata["source_file"], source_line=source_line,
        input=value.tolist(), source_result=actual.tolist(), fp64_reference=reference.tolist(),
        compiled_result="not_run", permissions=dict(reassociate=False)), indent=2) + "\n")


def review(build, root):
    """Run native ingestion and real backend checks, retaining every command."""
    root = root.resolve()
    root.mkdir(parents=True, exist_ok=False)
    (root / "logs").mkdir()
    repo = Path(__file__).resolve().parents[4]
    reporter = repo / "osprey/be/opt/tests/reduction_trace_report.py"
    commands = []
    env = dict(os.environ)
    env["LD_LIBRARY_PATH"] = str(build / "be") + ":" + str(build / "wopt")

    def run(label, command, success=True):
        """Preserve both streams, options and expected failure status."""
        command = [str(item) for item in command]
        result = subprocess.run(command, env=env, stdout=subprocess.PIPE,
                                stderr=subprocess.PIPE, text=True)
        (root / "logs" / (label + ".stdout")).write_text(result.stdout)
        (root / "logs" / (label + ".stderr")).write_text(result.stderr)
        commands.append(dict(label=label, argv=command, returncode=result.returncode))
        (root / "commands.json").write_text(json.dumps(commands, indent=2) + "\n")
        if (result.returncode == 0) != success:
            raise RuntimeError(label + ": " + result.stderr[-2000:])
        return result

    cases = []
    for label, rows, width, axis in (("negative-axis", 3, 17, -1),
                                    ("positive-axis", 3, 17, 1),
                                    ("unit", 1, 1, -1), ("wide", 7, 65, -1)):
        case = root / label
        run(label + "-produce", [sys.executable, __file__, "--produce", case,
            "--rows", rows, "--width", width, "--axis", axis])
        (case / "ascii").mkdir()
        (case / "trace").mkdir()
        run(label + "-reopen", [build / "ir_tools/ir_b2a", "-st", "-src",
            case / "row_sum.B", case / "ascii/row_sum.T"])
        text = (case / "ascii/row_sum.T").read_text()
        assert "OPR_DSLREDUCESUM" in text and "attr.axis=1" in text
        assert "OPR_DSLMODELINPUT" not in text and "OPR_DSLTENSORCONST" not in text
        witness = json.loads((case / "source-witness.json").read_text())
        for mode in (("default", "on", None), ("trace-off", "on", "off"),
                     ("preopt-off", "off", None)) if label == "negative-axis" else (("default", "on", None),):
            name, preopt, trace_option = mode
            trace = case / "trace" / (name + ".t")
            command = [build / "be/be", "-fB," + str(case / "row_sum.B"),
                "-fo," + str(case / (name + ".obj")), "-ft," + str(trace),
                "-PHASE:w=on:c=off:wpath=" + str(build / "wopt"), "-O2",
                "-DSL:wopt=" + preopt, "-WOPT:cr_simp=on", "-tr25"]
            if trace_option:
                command.append("-DSL:ai_opt_trace=" + trace_option)
            command.append(witness["source_file"])
            result = run(label + "-" + name, command, success=False)
            assert "OPR_DSLREDUCESUM.v1 has no executable VHO lowering route" in result.stderr
            assert not (case / (name + ".obj")).exists()
            partial = case / (name + ".O")
            if partial.exists():
                partial.rename(case / (name + ".incomplete-mapped-output"))
            if name != "default":
                assert "AI_REDUCE " not in trace.read_text()
        run(label + "-report", [sys.executable, reporter, "--trace", case / "trace/default.t",
                                 "--reports", case / "reports"])
        row = (case / "reports/semantic.log").read_text().splitlines()[-1].split()
        assert (int(row[2]), int(row[3]), int(row[-1])) == (rows, width, witness["source_line"])
        cases.append(dict(case=label, rows=rows, width=width, source_axis=axis,
                          source_line=witness["source_line"], native_preopt_verified=True))
    for rejection in ("fp64", "keepdim", "axis", "composed"):
        case = root / ("reject-" + rejection)
        run("reject-" + rejection, [sys.executable, __file__, "--produce", case,
                                    "--reject", rejection])
        assert (case / "rejection.txt").exists() and not (case / "row_sum.B").exists()
    driver = root / "driver"
    (driver / "ascii").mkdir(parents=True)
    run("driver-native", [build / "torch2whirl/torch2whirl",
        Path(__file__).with_name("models") / "building_blocks.py",
        "--model-factory", "RowReduction", "--sample-input", "shape:3,17",
        "--backend", "native", "-o", driver / "row_sum.B"])
    run("driver-reopen", [build / "ir_tools/ir_b2a", "-st", "-src",
        driver / "row_sum.B", driver / "ascii/row_sum.T"])
    assert "OPR_DSLREDUCESUM" in (driver / "ascii/row_sum.T").read_text()
    (root / "optimization-summary.log").write_text(
        "# What it summarizes: Python row-sum ingestion, canonical native PREOPT preservation and remaining execution gates.\n"
        "P0 ingestion=native verified_cases=4 source_line=preserved source_axes=-1,1 native_axis=1\n"
        "PREOPT entry_exit=identical trace_default=on trace_off=verified preopt_off=verified\n"
        "torch2whirl_driver=native_image_verified\n"
        "rejected_before_publication=fp64,keepdim,axis,composed\n"
        "candidate_selection=not_run profitability=not_run executable_transformation=none native_gpu=not_run\n")
    (root / "optimization.log").write_text(
        "# What it summarizes: all successful native semantic phase reports for the four Python row-sum cases.\n" +
        "".join("case=" + item["case"] + "\n" + (root / item["case"] / "reports/semantic.log").read_text()
                for item in cases))
    (root / "receipt.json").write_text(json.dumps(dict(
        schema="open64-r1-python-row-sum-v1", cases=cases, frontend_ingestion=True,
        compiler_driver_ingestion=True,
        native_preopt_verified=True, executable_reduction=False, native_gpu_verified=False), indent=2) + "\n")
    (root / "SHA256SUMS").write_text("".join(
        hashlib.sha256(path.read_bytes()).hexdigest() + "  " + str(path.relative_to(root)) + "\n"
        for path in sorted(root.rglob("*")) if path.is_file() and path != root / "SHA256SUMS"))
    print("native Python row-sum review passed: " + str(root))


def main():
    """Choose a fresh artifact location or a fresh-process producer case."""
    parser = argparse.ArgumentParser()
    parser.add_argument("--artifact-dir", type=Path, default=os.environ.get("OPEN64_DSL_TEST_ARTIFACT_DIR"))
    parser.add_argument("--build-dir", type=Path)
    parser.add_argument("--produce", type=Path)
    parser.add_argument("--rows", type=int, default=3)
    parser.add_argument("--width", type=int, default=17)
    parser.add_argument("--axis", type=int, default=-1)
    parser.add_argument("--reject", default="")
    args = parser.parse_args()
    if args.produce:
        produce(args.produce, args.rows, args.width, args.axis, args.reject)
    elif args.artifact_dir and args.build_dir:
        review(args.build_dir.resolve(), args.artifact_dir)
    else:
        parser.error("--build-dir and --artifact-dir required for review")


if __name__ == "__main__":
    main()
