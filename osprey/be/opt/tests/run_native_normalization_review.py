"""Retain reproducible native-contract/PREOPT evidence in a fresh directory.

Design: doc/AI-COMPILER-NATIVE-NORMALIZATION-CONTRACT-v0.1.md.
Run inside the configured Linux build environment; no Python ML dependencies.
"""
import argparse
import hashlib
import json
import os
from pathlib import Path
import subprocess
from normalization_trace_report import audit, write_reports


def run_review(build, output, variant="default"):
    """Invoke real native tools, retain raw outputs and audit intentional gates."""
    output = output.resolve()
    output.mkdir(parents=True, exist_ok=False)
    for name in ("ascii", "trace", "logs"):
        (output / name).mkdir()
    commands = []
    env = dict(os.environ)
    env["LD_LIBRARY_PATH"] = str(build / "be") + ":" + str(build / "wopt")

    def run(label, command, expected_success=True):
        """Record command and both streams, treating expected rejection explicitly."""
        command = [str(part) for part in command]
        result = subprocess.run(command, env=env, stdout=subprocess.PIPE,
                                stderr=subprocess.PIPE, text=True)
        (output / "logs" / (label + ".stdout")).write_text(result.stdout)
        (output / "logs" / (label + ".stderr")).write_text(result.stderr)
        commands.append(dict(label=label, argv=command, returncode=result.returncode))
        (output / "commands.json").write_text(json.dumps(commands, indent=2) + "\n")
        if (result.returncode == 0) != expected_success:
            raise RuntimeError(label + " unexpected result: " + result.stderr[-2000:])
        return result

    image = output / "normalization.B"
    run("cross-pu", [build / "ir_tools/dsl_normalization_contract_test", image, "cross-pu"])
    run("native-contract", [build / "ir_tools/dsl_normalization_contract_test", image] +
        ([] if variant == "default" else [variant]))
    run("mapped-reopen", [build / "ir_tools/ir_b2a", "-st", "-src", image,
                          output / "ascii/normalization.T"])
    source = Path(__file__).resolve().with_name("dsl_normalization_contract_test.cxx")
    for label, enabled, trace_enabled, level in (("preopt-on", "on", "on", "-O2"),
                                          ("default-trace", "on", None, "-O2"),
                                          ("trace-off", "on", "off", "-O2"),
                                          ("preopt-off", "off", "on", "-O2"),
                                          ("O0", "off", "on", "-O0"),
                                          ("O1", "off", "on", "-O1")):
        trace = output / "trace" / (label + ".t")
        command = [build / "be/be", "-fB," + str(image),
            "-fo," + str(output / (label + ".obj")), "-ft," + str(trace),
            "-PHASE:w=on:c=off:wpath=" + str(build / "wopt"), level,
            "-DSL:wopt=" + enabled, "-WOPT:cr_simp=on", "-tr25", source]
        if trace_enabled is not None:
            command.insert(-1, "-DSL:ai_opt_trace=" + trace_enabled)
        result = run(label, command, expected_success=False)
        if "OPR_DSLMEANVARNORM.v1 has no executable VHO lowering route" not in result.stderr:
            raise RuntimeError(label + " failed before the expected lowering gate")
        if label in ("trace-off", "preopt-off", "O0", "O1") and any(
                tag in trace.read_text() for tag in ("AI_NORM ", "AI_NORM_FACT ")):
            raise RuntimeError(label + " unexpectedly emitted normalization events")
        if (output / (label + ".obj")).exists():
            raise RuntimeError("unsupported lowering published an object")
        # The backend opens its mapped output before VHO; failed output is not
        # a finalized image and must never be presented as PREOPT evidence.
        partial = output / (label + ".O")
        if partial.exists():
            partial.rename(output / (label + ".incomplete-mapped-output"))
    rows = write_reports(output / "trace/preopt-on.t", output / "reports")
    expected_shape = {"default": (3, 17), "unit": (1, 1), "wide": (7, 65), "epsilon2": (3, 17)}[variant]
    if (len(rows) != 1 or (int(rows[0]["rows"]), int(rows[0]["width"])) != expected_shape or
            rows[0]["epsilon_bits"] != ("3a83126f" if variant == "epsilon2" else "3727c5ac")):
        raise RuntimeError("PREOPT changed the requested shape or epsilon")
    if audit((output / "trace/default-trace.t").read_text()) != rows:
        raise RuntimeError("default trace differs from explicit trace-on")
    ascii_text = (output / "ascii/normalization.T").read_text()
    if "OPR_DSLMEANVARNORM" not in ascii_text or "attr.reassociate=false" not in ascii_text:
        raise RuntimeError("mapped reader lost the normalization contract")
    products = ("be/be", "be/be.so", "wopt/wopt.so", "ir_tools/ir_b2a",
                "ir_tools/dsl_normalization_contract_test")
    receipt = dict(schema="open64-r1-native-normalization-v1", contracts=len(rows),
        native_preopt_verified=True, executable_normalization=False,
        default_trace_verified=True,
        frontend_ingestion=False, native_gpu_verified=False,
        products={name: hashlib.sha256((build / name).read_bytes()).hexdigest()
                  for name in products})
    (output / "receipt.json").write_text(json.dumps(receipt, indent=2) + "\n")
    (output / "SHA256SUMS").write_text("".join(
        hashlib.sha256(path.read_bytes()).hexdigest() + "  " + str(path.relative_to(output)) + "\n"
        for path in sorted(output.rglob("*")) if path.is_file() and path.name != "SHA256SUMS"))
    print("native normalization/PREOPT review passed: " + str(output))


def summarize_suite(output):
    """Audit all four retained traces and collect their successful phase logs."""
    variants = ("default", "unit", "wide", "epsilon2")
    paths = (output / "optimization.log", output / "optimization-summary.log")
    if any(path.exists() for path in paths):
        raise FileExistsError("normalization suite reports already exist")
    details = []
    shapes = {"default": (3, 17), "unit": (1, 1), "wide": (7, 65), "epsilon2": (3, 17)}
    for variant in variants:
        case = output / variant
        facts = audit((case / "trace/preopt-on.t").read_text())
        if (len(facts) != 1 or (int(facts[0]["rows"]), int(facts[0]["width"])) != shapes[variant] or
                facts[0]["epsilon_bits"] != ("3a83126f" if variant == "epsilon2" else "3727c5ac") or
                audit((case / "trace/default-trace.t").read_text()) != facts):
            raise RuntimeError("incomplete normalization suite")
        for mode in ("trace-off", "preopt-off", "O0", "O1"):
            if (any(tag in (case / "trace" / (mode + ".t")).read_text()
                    for tag in ("AI_NORM ", "AI_NORM_FACT ")) or
                    "OPR_DSLMEANVARNORM.v1 has no executable VHO lowering route" not in
                    (case / "logs" / (mode + ".stderr")).read_text()):
                raise RuntimeError("missing normalization control/lowering evidence")
        details.append("case=" + variant + "\n" + (case / "reports/semantic.log").read_text())
    paths[0].write_text(
        "# What it summarizes: complete successful normalization phase reports across four native contract variants.\n" +
        "".join(details))
    paths[1].write_text(
        "# What it summarizes: native normalization scope, numerical/trace coverage and remaining optimization gates.\n"
        "P0/PREOPT variants=default,unit,wide,epsilon2 contracts_verified=4 entry_exit=identical\n"
        "shape_cases=3x17,1x1,7x65 epsilon_bits=3727c5ac,3a83126f affine_broadcast=verified\n"
        "trace_default=on trace_off=verified preopt_off=verified O0_O1_without_dsl_preopt=lowering_rejected\n"
        "producer=cxx_native_fixture python_normalization_ingestion=not_connected\n"
        "candidate_selection=not_run profitability=not_run transformation=none execution_lowering=unsupported native_gpu=not_run\n")


def main():
    """Choose an existing build and a never-before-used artifact directory."""
    parser = argparse.ArgumentParser()
    parser.add_argument("--build-dir", type=Path, required=True)
    parser.add_argument("--output-dir", type=Path, required=True)
    parser.add_argument("--variant", choices=("default", "unit", "wide", "epsilon2"), default="default")
    parser.add_argument("--summarize-suite", action="store_true")
    args = parser.parse_args()
    if args.summarize_suite:
        summarize_suite(args.output_dir)
    else:
        run_review(args.build_dir.resolve(), args.output_dir, args.variant)


if __name__ == "__main__":
    main()
