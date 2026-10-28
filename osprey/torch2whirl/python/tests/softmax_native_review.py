"""Fresh-process Python/native stable-Softmax review, not compiled execution.

Design: doc/AI-COMPILER-NATIVE-SOFTMAX-CONTRACT-v0.1.md. Preserve source,
mapped WHIRL, actual PREOPT traces and numerical witnesses as separate evidence.
"""
import argparse
import hashlib
import json
import os
from pathlib import Path
import subprocess
import sys


def produce(root, rows, width, axis, reject):
    """Emit the exact source recipe or prove rejection before .B publication."""
    import torch
    from open64_dsc import WhirlExportOptions, export_to_whirl, save_as_whirl
    from models.building_block_reference import reference
    from row_semantic_report import acceptance
    from test_softmax_capture_optional import Stable, Opaque, Naive
    root.mkdir(parents=True, exist_ok=False)
    model = Stable(axis)
    value = ((torch.arange(rows * width, dtype=torch.float32) % 17) - 8).reshape(rows, width)
    if reject == "fp64":
        value = value.double()
    elif reject == "noncontiguous":
        value = torch.ones(width, rows).t()
    elif reject == "axis":
        model = Stable(0)
    elif reject == "opaque":
        model = Opaque()
    elif reject == "naive":
        model = Naive()
    elif reject:
        model = Stable(variant=reject)
    before, version = value.clone(), value._version
    image = root / "softmax.B"
    try:
        module = export_to_whirl(model, (value,), WhirlExportOptions(backend="native"))
        if reject:
            raise AssertionError("unsupported stable Softmax was accepted")
        save_as_whirl(module, str(image))
    except NotImplementedError as exc:
        if not reject or image.exists():
            raise
        assert torch.equal(value, before) and value._version == version
        (root / "rejection.txt").write_text(str(exc) + "\n")
        return
    assert torch.equal(value, before) and value._version == version
    actual = model(value)
    expected = torch.tensor(reference("softmax", (value.tolist(),)), dtype=torch.float64)
    error = acceptance(actual, expected)
    assert error["accepted"], error
    metadata = module.graph_operators[0].metadata
    source_line = int(metadata["source_line"])
    assert "return numerator / denominator" in Path(metadata["source_file"]).read_text().splitlines()[source_line - 1]
    (root / "manifest.json").write_text(json.dumps(module.to_manifest(), indent=2) + "\n")
    (root / "source.fx.txt").write_text(str(torch.fx.symbolic_trace(model).graph) + "\n")
    (root / "source-witness.json").write_text(json.dumps(dict(
        rows=rows, width=width, source_axis=axis, canonical_axis=1,
        source_file=metadata["source_file"], source_line=source_line,
        input=value.tolist(), source_result=actual.tolist(), fp64_reference=expected.tolist(),
        acceptance=error, compiled_result="not_run",
        permissions=dict(reassociate=False, state_reformulation=False),
        exp_semantics="source_unresolved", general_accuracy="unproved"), indent=2) + "\n")


def review(build, root):
    """Cross public APIs, mapped reader, real backend and strict trace auditor."""
    root = root.resolve()
    root.mkdir(parents=True, exist_ok=False)
    (root / "logs").mkdir()
    repo = Path(__file__).resolve().parents[4]
    reporter = repo / "osprey/be/opt/tests/softmax_trace_report.py"
    sys.path.insert(0, str(reporter.parent))
    from softmax_trace_report import audit
    commands = []
    env = dict(os.environ)
    env["LD_LIBRARY_PATH"] = str(build / "be") + ":" + str(build / "wopt")

    def run(label, command, success=True):
        """Record exact commands and both streams, including intended VHO rejects."""
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

    def inspect_case(label, rows, width, source_file, source_line=None):
        """Require native form, exact source location and paired PREOPT facts."""
        case = root / label
        (case / "ascii").mkdir()
        (case / "trace").mkdir()
        run(label + "-reopen", [build / "ir_tools/ir_b2a", "-st", "-src",
            case / "softmax.B", case / "ascii/softmax.T"])
        text = (case / "ascii/softmax.T").read_text()
        assert "OPR_DSLSTABLESOFTMAX" in text and "attr.axis=1" in text
        assert "attr.state_reformulation=false" in text and "attr.exp_semantics=source_unresolved" in text
        assert "OPR_DSLMODELINPUT" not in text and "OPR_DSLTENSORCONST" not in text
        modes = (("default", "on", None), ("explicit-on", "on", "on"),
                 ("trace-off", "on", "off"), ("preopt-off", "off", None))
        for name, preopt, trace_option in modes if label == "negative-axis" else modes[:1]:
            trace = case / "trace" / (name + ".t")
            command = [build / "be/be", "-fB," + str(case / "softmax.B"),
                "-fo," + str(case / (name + ".obj")), "-ft," + str(trace),
                "-PHASE:w=on:c=off:wpath=" + str(build / "wopt"), "-O2",
                "-DSL:wopt=" + preopt, "-WOPT:cr_simp=on", "-tr25"]
            if trace_option:
                command.append("-DSL:ai_opt_trace=" + trace_option)
            command.append(source_file)
            result = run(label + "-" + name, command, success=False)
            assert "OPR_DSLSTABLESOFTMAX.v1 has no executable VHO lowering route" in result.stderr
            assert not (case / (name + ".obj")).exists()
            partial = case / (name + ".O")
            if partial.exists():
                partial.rename(case / (name + ".incomplete-mapped-output"))
            if name in ("trace-off", "preopt-off"):
                assert not any(tag in trace.read_text() for tag in ("AI_SOFTMAX ", "AI_SOFTMAX_FACT "))
        run(label + "-report", [sys.executable, reporter, "--trace", case / "trace/default.t",
                                 "--reports", case / "reports"])
        facts = audit((case / "trace/default.t").read_text())
        assert len(facts) == 1
        fact = facts[0]
        assert (int(fact["rows"]), int(fact["width"])) == (rows, width)
        line = int(fact["source_line"])
        assert source_line is None or line == source_line
        assert "return numerator /" in Path(source_file).read_text().splitlines()[line - 1]
        if label == "negative-axis":
            assert audit((case / "trace/explicit-on.t").read_text()) == facts
        return fact

    cases = []
    for label, rows, width, axis in (("negative-axis", 3, 17, -1),
                                    ("positive-axis", 3, 17, 1),
                                    ("unit", 1, 1, -1), ("wide", 7, 65, -1)):
        case = root / label
        run(label + "-produce", [sys.executable, __file__, "--produce", case,
            "--rows", rows, "--width", width, "--axis", axis])
        witness = json.loads((case / "source-witness.json").read_text())
        fact = inspect_case(label, rows, width, witness["source_file"], witness["source_line"])
        cases.append(dict(case=label, rows=rows, width=width, source_axis=axis,
                          source_line=witness["source_line"], facts=fact))
    rejections = ("fp64", "noncontiguous", "axis", "opaque", "naive", "wrong-denominator",
                  "second-exp", "mutate", "extra", "offset")
    for rejection in rejections:
        case = root / ("reject-" + rejection)
        run("reject-" + rejection, [sys.executable, __file__, "--produce", case, "--reject", rejection])
        assert (case / "rejection.txt").exists() and not (case / "softmax.B").exists()
    driver = root / "driver"
    driver.mkdir()
    source = Path(__file__).with_name("models") / "building_blocks.py"
    run("driver-native", [build / "torch2whirl/torch2whirl", source,
        "--model-factory", "StableSoftmax", "--sample-input", "shape:3,17",
        "--backend", "native", "-o", driver / "softmax.B"])
    fact = inspect_case("driver", 3, 17, str(source))
    cases.append(dict(case="driver", rows=3, width=17, source_axis=-1,
                      source_line=int(fact["source_line"]), facts=fact))
    (root / "optimization-summary.log").write_text(
        "# What it summarizes: exact Python stable-Softmax ingestion, native PREOPT characterization and remaining gates.\n"
        "P0 ingestion=native verified_cases=5 source_line=division_site source_axes=-1,1 native_axis=1\n"
        "PREOPT entry_exit=identical state_access_numerical=canonical_whirl trace_default=on trace_off=verified preopt_off=verified\n"
        "torch2whirl_driver=native_image_and_preopt_verified\n"
        "source_fp32_vs_independent_fp64=4_samples_accepted general_accuracy=unproved compiled_execution=not_run\n"
        "rejected_before_publication=" + ",".join(rejections) + "\n"
        "candidate_selection=not_run profitability=not_run executable_transformation=none native_gpu=not_run\n")
    (root / "optimization.log").write_text(
        "# What it summarizes: all successful native semantic phase reports for four API and one driver Softmax cases.\n" +
        "".join("case=" + item["case"] + "\n" + (root / item["case"] / "reports/semantic.log").read_text()
                for item in cases))
    (root / "receipt.json").write_text(json.dumps(dict(
        schema="open64-r1-python-stable-softmax-v1", cases=cases,
        rejections=list(rejections), frontend_ingestion=True, compiler_driver_ingestion=True,
        native_preopt_verified=True, executable_softmax=False, native_gpu_verified=False), indent=2) + "\n")
    (root / "SHA256SUMS").write_text("".join(
        hashlib.sha256(path.read_bytes()).hexdigest() + "  " + str(path.relative_to(root)) + "\n"
        for path in sorted(root.rglob("*")) if path.is_file() and path != root / "SHA256SUMS"))
    print("native Python stable-Softmax review passed: " + str(root))


def main():
    """Choose a fresh review directory or a fresh-process producer case."""
    parser = argparse.ArgumentParser()
    parser.add_argument("--artifact-dir", type=Path)
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
