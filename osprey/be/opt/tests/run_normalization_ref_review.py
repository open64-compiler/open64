"""Retain native LN-REF-1 admission, PREOPT and mapped-image evidence.

Design: doc/AI-COMPILER-LAYERNORM-NATIVE-REFERENCE-CONTRACT-v0.1.md.
This explicit producer is not Python source admission or executable lowering.
"""
import argparse
import hashlib
import json
import os
from pathlib import Path
import subprocess

from normalization_ref_trace_report import audit, write_reports
from normalization_trace_report import audit as audit_v1


VARIANTS = ("default", "unit", "wide", "epsilon2", "subnormal",
            "count-limit", "shared-affine", "mixed")
CONTROLS = (("preopt-on", "on", "on", "-O2"),
            ("default-trace", "on", None, "-O2"),
            ("trace-off", "on", "off", "-O2"),
            ("preopt-off", "off", "on", "-O2"),
            ("O0", "off", "on", "-O0"),
            ("O1", "off", "on", "-O1"))


def run_review(build, output, variant):
    """Run real tools and retain every invocation, raw trace and expected refusal."""
    output = output.resolve()
    output.mkdir(parents=True, exist_ok=False)
    for name in ("ascii", "trace", "logs", "source"):
        (output / name).mkdir()
    env = dict(os.environ, LD_LIBRARY_PATH=str(build / "be") + ":" + str(build / "wopt"))
    commands = []

    def run(label, argv, success=True):
        """Record both streams before judging an expected success or refusal."""
        argv = [str(item) for item in argv]
        result = subprocess.run(argv, env=env, capture_output=True, text=True)
        for stream in ("stdout", "stderr"):
            (output / "logs" / (label + "." + stream)).write_text(getattr(result, stream))
        commands.append(dict(label=label, argv=argv, returncode=result.returncode))
        (output / "commands.json").write_text(json.dumps(commands, indent=2) + "\n")
        if (result.returncode == 0) != success:
            raise RuntimeError(label + ": " + result.stderr[-3000:])
        return result

    source = Path(__file__).resolve().with_name("dsl_normalization_ref_test.cxx")
    (output / "source" / source.name).write_bytes(source.read_bytes())
    image = output / "ln_ref.B"
    producer = build / "ir_tools/dsl_normalization_ref_test"
    run("cross-pu", [producer, image, "cross-pu"])
    run("native-contract", [producer, image, variant])
    run("mapped-reopen", [build / "ir_tools/ir_b2a", "-st", "-src", image,
                          output / "ascii/ln_ref.T"])
    for label, enabled, trace_enabled, level in CONTROLS:
        trace = output / "trace" / (label + ".t")
        argv = [build / "be/be", "-fB," + str(image),
            "-fo," + str(output / (label + ".obj")), "-ft," + str(trace),
            "-PHASE:w=on:c=off:wpath=" + str(build / "wopt"), level,
            "-DSL:wopt=" + enabled, "-WOPT:cr_simp=on", "-tr25", source]
        if trace_enabled is not None:
            argv.insert(-1, "-DSL:ai_opt_trace=" + trace_enabled)
        result = run(label, argv, success=False)
        if "OPR_DSLMEANVARNORM.v2 has no executable VHO lowering route" not in result.stderr:
            raise RuntimeError(label + " failed before the intended lowering gate")
        if label not in ("preopt-on", "default-trace") and any(
                tag in trace.read_text() for tag in ("AI_NORM ", "AI_NORM_FACT ", "AI_NORM_REF ")):
            raise RuntimeError(label + " ignored trace/PREOPT controls")
        if (output / (label + ".obj")).exists():
            raise RuntimeError("unsupported lowering published an object")
        partial = output / (label + ".O")
        if partial.exists():
            partial.rename(output / (label + ".incomplete-mapped-output"))
    rows = write_reports(output / "trace/preopt-on.t", output / "reports")
    shape = {"unit": (1, 1), "wide": (7, 65), "count-limit": (1, 16777216)}.get(variant, (3, 17))
    expected_count = 3 if variant == "mixed" else 1
    epsilon = {"epsilon2": "3a83126f", "subnormal": "00000001"}.get(variant, "3727c5ac")
    if (len(rows) != expected_count or
            any((int(row["rows"]), int(row["width"])) != shape for row in rows) or
            rows[0]["epsilon_bits"] != epsilon or
            audit((output / "trace/default-trace.t").read_text()) != rows):
        raise RuntimeError("PREOPT changed the explicit reference contract")
    if variant == "shared-affine" and rows[0]["gamma"] != rows[0]["beta"]:
        raise RuntimeError("shared affine identity lost")
    if variant == "mixed":
        legacy = audit_v1((output / "trace/preopt-on.t").read_text())
        if len(legacy) != 1 or legacy[0]["rsqrt"] != "source_unresolved":
            raise RuntimeError("mixed v1 contract lost or silently upgraded")
    ascii_text = (output / "ascii/ln_ref.T").read_text()
    if not all(token in ascii_text for token in (
            "OPR_DSLMEANVARNORM", "attr.reference_binding=ln_ref_1",
            "attr.mean_order=ascending_left_fold", "attr.rsqrt_semantics=sqrt_rne_then_div_rne_f32")):
        raise RuntimeError("mapped reader lost reference semantics")
    products = ("be/be", "be/be.so", "wopt/wopt.so", "ir_tools/ir_b2a",
                "ir_tools/dsl_normalization_ref_test")
    receipt = dict(schema="open64-ln-ref1-native-v2", variant=variant,
        contracts=len(rows), native_preopt_verified=True, mapped_reopen_verified=True,
        default_trace_verified=True, producer="explicit_native_fixture",
        python_source_v2_admission=False, executable_normalization=False,
        target_arithmetic_certified=False, native_gpu_verified=False,
        products={name: hashlib.sha256((build / name).read_bytes()).hexdigest() for name in products})
    (output / "receipt.json").write_text(json.dumps(receipt, indent=2) + "\n")
    (output / "SHA256SUMS").write_text("".join(
        hashlib.sha256(path.read_bytes()).hexdigest() + "  " + str(path.relative_to(output)) + "\n"
        for path in sorted(output.rglob("*")) if path.is_file() and path.name != "SHA256SUMS"))
    print("LN-REF-1 native review passed: " + str(output))


def main():
    """Choose a fresh single-case or full-suite evidence directory."""
    parser = argparse.ArgumentParser()
    parser.add_argument("--build-dir", type=Path, required=True)
    parser.add_argument("--output-dir", type=Path, required=True)
    parser.add_argument("--variant", choices=VARIANTS, default="default")
    parser.add_argument("--suite", action="store_true")
    args = parser.parse_args()
    if not args.suite:
        run_review(args.build_dir.resolve(), args.output_dir, args.variant)
        return
    args.output_dir.mkdir(parents=True, exist_ok=False)
    for variant in VARIANTS:
        run_review(args.build_dir.resolve(), args.output_dir / variant, variant)
    (args.output_dir / "optimization.log").write_text(
        "# What it summarizes: all successful native LN-REF-1 phase reports across eight variants.\n" +
        "".join("case=" + variant + "\n" +
            (args.output_dir / variant / "reports/optimization.log").read_text() for variant in VARIANTS))
    (args.output_dir / "optimization-summary.log").write_text(
        "# What it summarizes: native v2 admission, PREOPT, mapped-image and control coverage; remaining gates.\n"
        "P0 native_variants=8 v2_contracts=10 mixed_v1_contracts=1 entry_exit=identical mapped_reopen=passed\n"
        "source=explicit_native_fixture source_equivalence=not_proved Python_source_matcher=v1_unchanged\n"
        "controls=explicit_on,default_on,trace_off,preopt_off,O0,O1 pass=true\n" +
        "".join("P%d status=disabled_by_policy reason=contract_only\n" % phase for phase in range(1, 12)) +
        "selection=none profitability=not_run transformation=none executable_lowering=unsupported target_certification=not_run GPU=not_run\n")


if __name__ == "__main__":
    main()
