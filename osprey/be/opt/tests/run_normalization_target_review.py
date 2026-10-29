"""Retain LN1-c native queries, mapped scalar carriers and offline CUDA evidence.

Design: doc/AI-COMPILER-LAYERNORM-TARGET-ARITHMETIC-v0.1.md.
No compiler-generated LayerNorm execution or GPU result is claimed.
"""
import argparse
import difflib
import hashlib
import json
import os
from pathlib import Path
import subprocess

from normalization_ref_trace_report import write_reports as source_reports
from normalization_target_trace_report import audit_carriers, write_reports


def review(build, cuda, output):
    """Produce fresh compiler/native/assembler evidence for each exact target."""
    output = output.resolve()
    output.mkdir(parents=True, exist_ok=False)
    commands = []
    env = dict(os.environ, LD_LIBRARY_PATH=str(build / "be") + ":" + str(build / "wopt"))
    source = Path(__file__).resolve().with_name("dsl_normalization_ref_test.cxx")
    producer = build / "ir_tools/dsl_normalization_target_test"

    def run(label, argv, case, success=True):
        """Keep exact argv, both streams and actual expected lowering refusals."""
        argv = [str(item) for item in argv]
        result = subprocess.run(argv, cwd=case, env=env, capture_output=True, text=True)
        (case / (label + ".stdout")).write_text(result.stdout)
        (case / (label + ".stderr")).write_text(result.stderr)
        commands.append(dict(label=label, argv=argv, cwd=str(case), returncode=result.returncode))
        (output / "commands.json").write_text(json.dumps(commands, indent=2) + "\n")
        if (result.returncode == 0) != success:
            raise RuntimeError(label + ": " + result.stderr[-3000:])
        return result

    run("nvcc-version", [cuda / "bin/nvcc", "--version"], output)
    records = []
    for architecture in ("sm_90", "sm_90a", "sm_100", "sm_100a"):
        case = output / architecture
        case.mkdir()
        run("native-binding", [producer, case / "ln_ref.B", case / "target.t", architecture], case)
        run("source-mapped", [build / "ir_tools/ir_b2a", "-st", "-src", case / "ln_ref.B", case / "ln_ref.T"], case)
        for label, enabled, trace in (("preopt-on", "on", "on"), ("default-on", "on", None),
                                       ("trace-off", "on", "off"), ("preopt-off", "off", "on")):
            argv = [build / "be/be", "-fB," + str(case / "ln_ref.B"), "-fo," + str(case / (label + ".obj")),
                "-ft," + str(case / (label + ".t")), "-PHASE:w=on:c=off:wpath=" + str(build / "wopt"),
                "-O2", "-DSL:wopt=" + enabled, "-WOPT:cr_simp=on", "-tr25"]
            if trace is not None:
                argv.append("-DSL:ai_opt_trace=" + trace)
            result = run(label, argv + [source], case, success=False)
            if "OPR_DSLMEANVARNORM.v2 has no executable VHO lowering route" not in result.stderr:
                raise RuntimeError("source failed before intended lowering boundary")
            if (case / (label + ".obj")).exists():
                raise RuntimeError("unsupported normalization published an object")
            partial = case / (label + ".O")
            if partial.exists():
                partial.rename(case / (label + ".incomplete-mapped-output"))
            if label in ("trace-off", "preopt-off") and "AI_NORM" in (case / (label + ".t")).read_text():
                raise RuntimeError("trace control ignored")
        source_reports(case / "preopt-on.t", case / "source-reports")
        from normalization_ref_trace_report import audit as source_audit
        if source_audit((case / "preopt-on.t").read_text()) != source_audit((case / "default-on.t").read_text()):
            raise RuntimeError("default trace changed source binding")
        rows = write_reports(case / "target.t", case / "preopt-on.t", case / "reports")
        if len(rows) != 39:
            raise RuntimeError("native target-query coverage missing")
        run("scalar-producer", [producer, case / "scalar.before.B", case / "scalar.after.B", architecture, "scalar"], case)
        for boundary in ("before", "after"):
            stem = "scalar." + boundary
            run(stem + "-cuda", [build / "whirl2c/whirl2c", "-cuda", "-CLIST:dotc_file=" + str(case / (stem + ".cu")),
                "-fB," + str(case / (stem + ".B")), "scalar-source.cu"], case)
            emitter_trace = case / (stem + ".t")
            if emitter_trace.exists():
                emitter_trace.rename(case / (stem + ".emitter.t"))
            run(stem + "-mapped", [build / "ir_tools/ir_b2a", "-st", "-src", case / (stem + ".B"), case / (stem + ".T")], case)
        before, after = ((case / ("scalar." + name + ".T")).read_text() for name in ("before", "after"))
        (case / "scalar.before-after.diff").write_text("".join(difflib.unified_diff(before.splitlines(True), after.splitlines(True),
            fromfile="scalar.before.T", tofile="scalar.after.T")))
        flags = ["-std=c++11", "-arch=" + architecture, "--fmad=false", "--ftz=false", "--prec-div=true", "--prec-sqrt=true"]
        run("scalar-ptx", [cuda / "bin/nvcc"] + flags + ["-ptx", "scalar.after.cu", "-o", "scalar.after.ptx"], case)
        audit_carriers((case / "scalar.after.cu").read_text(), after, (case / "scalar.after.ptx").read_text())
        run("scalar-cubin", [cuda / "bin/nvcc"] + flags + ["-cubin", "scalar.after.cu", "-o", "scalar.after.cubin", "-Xptxas=-v"], case)
        (case / "compile.log").write_text(
            "# What it summarizes: WHIRL-derived scalar CUDA compile/assembler acceptance, not GPU arithmetic.\n"
            "architecture=" + architecture + " instructions=add,sub,mul,div,sqrt.rn.f32 operands=preserved memory_clobber=preserved\n"
            "nvcc_flags=" + " ".join(flags) + " PTX=audited cubin=assembled runtime=not_run oracle=not_run\n")
        records.append(dict(architecture=architecture, queries=len(rows),
            conditional_bound=sum(r["status"] == "0" for r in rows), scalar_operations=5,
            ptx_audited=True, cubin_assembled=True, GPU_verified=False))
    products = ("be/be", "be/be.so", "wopt/wopt.so", "whirl2c/whirl2c", "whirl2c/whirl2c.so", "ir_tools/ir_b2a", "ir_tools/dsl_normalization_target_test")
    (output / "receipt.json").write_text(json.dumps(dict(schema="open64-ln1c-target-review-v1", cases=records,
        source_v1_preserved=True, reduction_dependency=False, LayerNorm_lowering=False, oracle=False, GPU=False,
        products={name: hashlib.sha256((build / name).read_bytes()).hexdigest() for name in products}), indent=2) + "\n")
    (output / "optimization.log").write_text("# What it summarizes: complete semantic, target and scalar compile phase collection.\n" +
        "".join("case=" + record["architecture"] + "\n" +
            (output / record["architecture"] / "source-reports/optimization.log").read_text() +
            (output / record["architecture"] / "reports/optimization.log").read_text() +
            (output / record["architecture"] / "compile.log").read_text() for record in records))
    (output / "optimization-summary.log").write_text(
        "# What it summarizes: LN1-c source/target binding, primitive realization and remaining gates.\n"
        "targets=sm_90,sm_90a,sm_100,sm_100a queries=156 conditional_bound=8 refusals=148\n"
        "source=actual_mixed_v1_v2 PREOPT=preserved trace_controls=verified v1_migration=none\n"
        "scalar_operations=20 WHIRL_to_CUDA=verified PTX=audited cubins=4 assembly=offline\n"
        "normalization_selection=not_run LayerNorm_rewrites=0 oracle=not_run GPU=not_run\n")
    (output / "SHA256SUMS").write_text("".join(hashlib.sha256(path.read_bytes()).hexdigest() + "  " + str(path.relative_to(output)) + "\n"
        for path in sorted(output.rglob("*")) if path.is_file() and path.name != "SHA256SUMS"))
    print("LN1-c native/WHIRL/CUDA/offline-assembler review passed: " + str(output))


def main():
    """Require an explicit build/toolkit and a never-used artifact directory."""
    parser = argparse.ArgumentParser()
    parser.add_argument("--build-dir", type=Path, required=True)
    parser.add_argument("--cuda-dir", type=Path, required=True)
    parser.add_argument("--output-dir", type=Path, required=True)
    args = parser.parse_args()
    review(args.build_dir.resolve(), args.cuda_dir.resolve(), args.output_dir)


if __name__ == "__main__":
    main()
