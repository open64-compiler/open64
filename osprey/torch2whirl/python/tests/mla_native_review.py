"""Native absorbed MLA producer and independent mapped-image review.

Design: MLA-INGESTION-PLAN.md. Each case is produced in its own process.
This validates ingestion and reopening, not executable lowering or GPU speed.
"""

import argparse
import hashlib
import json
import os
from pathlib import Path
import shutil
import subprocess
import sys


def produce(context, destination):
    """Save actual native WHIRL with runtime inputs and two declared results."""
    from open64_dsc import WhirlExportOptions, export_to_whirl, save_as_whirl
    from models.absorbed_mla import make_fixture
    from mla_standalone import check_case
    import torch
    from models.absorbed_mla import INPUT_NAMES

    root = Path(destination)
    root.mkdir(parents=True, exist_ok=False)
    result, inputs, expected, actual, _, census = check_case(context)
    model, _ = make_fixture(context)
    module = export_to_whirl(model, inputs, WhirlExportOptions(
        backend="native", model_name="absorbed_mla_" + context))
    formals = [value for value in module.values if value.value_kind == "formal"]
    assert len(formals) == 7 and not module.tensor_payloads
    assert all(value.value_kind in ("formal", "operator_result") for value in module.values)
    assert module.graph_source == "torch.fx"
    assert len(module.graph_operators) == 46
    save_as_whirl(module, str(root / "mla.B"))
    assert (root / "mla.B").read_bytes().startswith(b"\x7fELF")
    torch.save(dict(zip(INPUT_NAMES, inputs)), root / "inputs.pt")
    torch.save({"output": expected[0], "lse": expected[1]}, root / "reference.pt")
    torch.save({"output": actual[0], "lse": actual[1]}, root / "source-output.pt")
    (root / "module.json").write_text(json.dumps(module.to_manifest(), indent=2) + "\n")
    (root / "census.json").write_text(json.dumps(census, indent=2) + "\n")
    result.update(native_whirl="PASS", compiled_execution="NOT_RUN", runtime_formals=7,
                  result_names=["squeeze_4", "squeeze_5"], typed_operators=46,
                  input_sha256={name: hashlib.sha256(value.numpy().tobytes()).hexdigest()
                                for name, value in zip(INPUT_NAMES, inputs)})
    (root / "correctness.json").write_text(json.dumps(result, indent=2) + "\n")


def review(root, ir_b2a, driver=None):
    """Produce and reopen native MLA; verify ordinary tensor ops and contracts."""
    root = Path(root).resolve()
    root.mkdir(parents=True, exist_ok=False)
    (root / "logs").mkdir()
    commands = []

    def run(label, argv):
        result = subprocess.run([str(item) for item in argv], capture_output=True, text=True)
        (root / "logs" / (label + ".stdout")).write_text(result.stdout)
        (root / "logs" / (label + ".stderr")).write_text(result.stderr)
        commands.append({"label": label, "argv": [str(item) for item in argv],
                         "returncode": result.returncode})
        (root / "commands.json").write_text(json.dumps(commands, indent=2) + "\n")
        if result.returncode:
            raise RuntimeError(label + ": " + result.stderr[-5000:])

    cases = {}
    for context in ("short", "long"):
        destination = root / context
        run(context + "-produce", [sys.executable, __file__, "--produce", context,
                                   "--artifact-dir", destination])
        (destination / "ascii").mkdir()
        run(context + "-reopen", [ir_b2a, "-st", "-src", destination / "mla.B",
                                   destination / "ascii/mla.T"])
        trace = (destination / "ascii/mla.T").read_text()
        for token in ("OPR_DSLRESHAPE", "OPR_DSLTRANSPOSE", "OPR_DSLMATMUL",
                      "MSELECT", "MSUB", "MMGT", "OPR_DSLREDUCEMAX", "OPR_DSLREDUCESUM",
                      "OPR_DSLEXP", "OPR_DSLLOG", "attr.batch_rule=numpy",
                      "attr.value_bits=ff800000", "query_nope", "allowed",
                      "squeeze_4", "squeeze_5"):
            assert token in trace, "missing native evidence: " + token
        assert "OPR_DSLMODELINPUT" not in trace
        assert "OPR_DSLTENSORCONST" not in trace
        if driver:
            source = destination / "driver_fixture.py"
            source.write_text("from models.absorbed_mla import make_fixture\n"
                              "def create_model():\n"
                              "    return make_fixture(" + repr(context) + ")[0]\n"
                              "def open64_sample_inputs():\n"
                              "    return make_fixture(" + repr(context) + ")[1]\n")
            run(context + "-driver", [driver, "--backend", "native", "-o",
                                       destination / "driver.B", source])
            run(context + "-driver-reopen", [ir_b2a, "-st", "-src",
                    destination / "driver.B", destination / "ascii/driver.T"])
            driver_trace = (destination / "ascii/driver.T").read_text()
            assert "OPR_DSLLOG" in driver_trace and "squeeze_5" in driver_trace
        cases[context] = json.loads((destination / "correctness.json").read_text())
    source = root / "source"
    (source / "models").mkdir(parents=True)
    tests = Path(__file__).parent
    for name in ("mla_native_review.py", "mla_standalone.py",
                 "test_tensor_expression_capture_optional.py",
                 "test_tensor_expression_native_optional.py"):
        shutil.copyfile(tests / name, source / name)
    for name in ("absorbed_mla.py", "mla_reference.py", "__init__.py"):
        shutil.copyfile(tests / "models" / name, source / "models" / name)
    report = {"native_whirl": "PASS", "independent_ir_b2a_reopen": "PASS",
              "standalone_driver": "PASS" if driver else "NOT_RUN",
              "compiled_execution": "NOT_RUN", "gpu_validation": "NOT_RUN", "cases": cases}
    (root / "report.json").write_text(json.dumps(report, indent=2) + "\n")
    (root / "SHA256SUMS").write_text("".join(
        hashlib.sha256(path.read_bytes()).hexdigest() + "  " + str(path.relative_to(root)) + "\n"
        for path in sorted(root.rglob("*")) if path.is_file()))
    print(json.dumps(report, indent=2))


def main():
    """Honor explicit artifacts or the shared artifact root; never replace a run."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--produce", choices=("short", "long"))
    parser.add_argument("--artifact-dir", type=Path)
    parser.add_argument("--ir-b2a", type=Path, default=os.environ.get("OPEN64_IR_B2A"))
    parser.add_argument("--driver", type=Path)
    args = parser.parse_args()
    destination = args.artifact_dir
    if destination is None and os.environ.get("OPEN64_DSL_TEST_ARTIFACT_DIR"):
        destination = Path(os.environ["OPEN64_DSL_TEST_ARTIFACT_DIR"]) / "mla-native"
    if destination is None:
        parser.error("provide a fresh artifact directory")
    if args.produce:
        produce(args.produce, destination)
    elif args.ir_b2a:
        review(destination, args.ir_b2a, args.driver)
    else:
        parser.error("provide --ir-b2a or OPEN64_IR_B2A")


if __name__ == "__main__":
    main()
