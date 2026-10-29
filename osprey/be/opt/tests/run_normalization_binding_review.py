#!/usr/bin/env python3
"""Retain LN1-e native source/target and integrated numerical evidence.

Design: doc/AI-COMPILER-LAYERNORM-INTEGRATED-BINDING-v0.1.md.
Native stage runs inside the offline Linux build image; numerical/audit stages
use the retained independent host checker. Neither stage lowers a kernel.
"""
import argparse
import json
import os
from pathlib import Path
import shutil
import subprocess

from normalization_binding_report import (require, sha256, join_contracts, invocations,
                                          bound_requests, product_hashes, write_reports, validate_variant)
from normalization_ref_trace_report import write_reports as source_reports
from normalization_target_trace_report import write_reports as target_reports

ARCHITECTURES = ("sm_90", "sm_90a", "sm_100", "sm_100a")
VARIANTS = ("default", "unit", "wide", "epsilon2", "subnormal", "mixed", "shared-affine")
NATIVE_BINARIES = {"dsl_normalization_evidence_test", "dsl_normalization_target_test", "ir_b2a", "be", "be.so", "wopt.so"}


def native(build, source, output):
    """Build full source/query/control/mapped provenance in new directories."""
    output.mkdir(parents=True, exist_ok=False)
    binaries = output / "binaries"
    binaries.mkdir()
    for path in ("ir_tools/dsl_normalization_evidence_test", "ir_tools/dsl_normalization_target_test",
                 "ir_tools/ir_b2a", "be/be", "be/be.so", "wopt/wopt.so"):
        shutil.copy2(build / path, binaries / Path(path).name)
    env = dict(os.environ, LD_LIBRARY_PATH=str(binaries))
    commands, cases = [], []

    def run(case, label, arguments, success=True):
        """Persist actual option vectors, streams and expected lowering refusal."""
        args = [str(x) for x in arguments]
        result = subprocess.run(args, env=env, cwd=case, capture_output=True, text=True, check=False)
        for stream in ("stdout", "stderr"):
            (case / "logs" / (label + "." + stream)).write_text(getattr(result, stream))
        commands.append(dict(label=label, case=str(case.relative_to(output)), argv=args, returncode=result.returncode))
        (output / "commands.json").write_text(json.dumps(commands, indent=2) + "\n")
        require((result.returncode == 0) == success, label + ": " + result.stderr[-2000:])
        return result

    for arch in ARCHITECTURES:
        for variant in VARIANTS:
            name = arch + "-" + variant
            case = output / name
            for directory in ("ascii", "trace", "logs"):
                (case / directory).mkdir(parents=True, exist_ok=False)
            image = case / "ln_ref.B"
            run(case, "producer", [binaries / "dsl_normalization_evidence_test", image, case / "target.t", arch, variant])
            require(image.read_bytes() == (case / "ln_ref.B.preserved.B").read_bytes(), "target queries mutated source")
            for input_name, output_name in (("ln_ref.B", "ln_ref.T"), ("ln_ref.B.preserved.B", "preserved.T")):
                run(case, "mapped-" + output_name, [binaries / "ir_b2a", "-st", "-src", case / input_name, case / "ascii" / output_name])
            for label, enabled, tracing in (("preopt-on", "on", "on"), ("default-trace", "on", None),
                                             ("trace-off", "on", "off"), ("preopt-off", "off", "on")):
                args = [binaries / "be", "-fB," + str(image), "-fo," + str(case / (label + ".obj")),
                        "-ft," + str(case / "trace" / (label + ".t")),
                        "-PHASE:w=on:c=off:wpath=" + str(binaries), "-O2", "-DSL:wopt=" + enabled,
                        "-WOPT:cr_simp=on", "-tr25"]
                if tracing is not None:
                    args.append("-DSL:ai_opt_trace=" + tracing)
                result = run(case, label, args + [source / "osprey/be/opt/tests/dsl_normalization_ref_test.cxx"], success=False)
                require("OPR_DSLMEANVARNORM.v2 has no executable VHO lowering route" in result.stderr and
                        not (case / (label + ".obj")).exists(), "unexpected executable-lowering outcome")
                partial = case / (label + ".O")
                if partial.exists():
                    partial.rename(case / (label + ".incomplete-mapped-output"))
            source_reports(case / "trace/preopt-on.t", case / "source-reports")
            target_reports(case / "target.t", case / "trace/preopt-on.t", case / "target-reports")
            facts, legacy, queries = join_contracts((case / "trace/preopt-on.t").read_text(),
                                                   (case / "target.t").read_text(), arch)
            validate_variant(facts, legacy, variant)
            cases.append(dict(name=name, architecture=arch, variant=variant,
                              source_contracts=len(facts), v1_refusals=len(legacy), target_queries=len(queries)))
    manifest = dict(schema="LN1-e-native-v1", cases=cases,
                    binaries={p.name: sha256(p) for p in sorted(binaries.iterdir())})
    (output / "native-manifest.json").write_text(json.dumps(manifest, indent=2) + "\n")
    return manifest


def numerical(root, host, producer):
    """Derive exact full-tensor fixtures from every actual admitted source node."""
    manifest = json.loads((root / "native-manifest.json").read_text())
    for case_info in manifest["cases"]:
        case = root / case_info["name"]
        facts, _, _ = join_contracts((case / "trace/preopt-on.t").read_text(),
                                     (case / "target.t").read_text(), case_info["architecture"])
        bound = invocations(facts)
        output = case / "numerical"
        output.mkdir(exist_ok=False)
        transcript = bound_requests(bound)
        (output / "requests.txt").write_text(transcript)
        result = subprocess.run([str(host)], input=transcript, text=True, capture_output=True, check=False)
        (output / "arithmetic.t").write_text(result.stdout)
        (output / "host.stderr").write_text(result.stderr)
        require(result.returncode == 0, "numerical witness failed: " + result.stderr)
        binding = dict(schema="LN1-e-binding-v1", architecture=case_info["architecture"], invocations=bound,
                       products=product_hashes(case), host_sha256=sha256(host), host_source_sha256=sha256(producer))
        (case / "binding.json").write_text(json.dumps(binding, indent=2, sort_keys=True) + "\n")
    return audit(root, host, producer, root / "reports")


def audit(root, host, producer, output):
    """Reaudit the full expected case census and every source/target/input/bit join."""
    manifest = json.loads((root / "native-manifest.json").read_text())
    expected = [a + "-" + v for a in ARCHITECTURES for v in VARIANTS]
    require(set(manifest) == {"schema", "cases", "binaries"} and set(manifest["binaries"]) == NATIVE_BINARIES and
            manifest["schema"] == "LN1-e-native-v1" and [c["name"] for c in manifest["cases"]] == expected,
            "incomplete/duplicate/foreign native case census")
    for name, digest in manifest["binaries"].items():
        require(Path(name).name == name and sha256(root / "binaries" / name) == digest, "native tool digest drift")
    output.mkdir(parents=True, exist_ok=False)
    results = []
    for info in manifest["cases"]:
        require(set(info) == {"name", "architecture", "variant", "source_contracts", "v1_refusals", "target_queries"},
                "unknown native case metadata")
        case = root / info["name"]
        facts, legacy, _ = join_contracts((case / "trace/preopt-on.t").read_text(),
                                          (case / "target.t").read_text(), info["architecture"])
        validate_variant(facts, legacy, info["variant"])
        result = write_reports(case, output / info["name"], producer, host)
        require(result["architecture"] + "-" + info["variant"] == info["name"] and
                all(result[k] == info[k] for k in ("source_contracts", "v1_refusals", "target_queries")), "native manifest claim drift")
        result.pop("origins")
        results.append(result)
    totals = {k: sum(r[k] for r in results) for k in
              ("source_contracts", "v1_refusals", "target_queries", "invocations", "rows", "state_comparisons")}
    summary = "# What it summarizes: complete LN1-e native/target/numerical census and remaining optimization gates.\n"
    summary += "LN1-e cases=" + str(len(results)) + " " + " ".join(k + "=" + str(v) for k, v in totals.items()) + "\n"
    summary += "source=v2_explicit target=conditional numerical=host_exact_MPFR v1=preserved_refused\n"
    summary += "COST none DECISION none TRANSFORM none LN2=open LN3=open LN4=open GPU=not_run image_execution=not_run\n"
    (output / "optimization-summary.log").write_text(summary)
    (output / "optimization.log").write_text("# What it summarizes: full successful source, target and numerical report collection.\n" +
        "".join((output / c["name"] / "optimization.log").read_text() for c in manifest["cases"]))
    record = dict(schema="LN1-e-audit-v1", cases=len(results), totals=totals, results=results,
                  source_v1_preserved=True, reduction_dependency=False, LayerNorm_lowering=False, GPU=False)
    (output / "audit.json").write_text(json.dumps(record, indent=2, sort_keys=True) + "\n")
    return record


def main():
    """Choose native production, numerical binding or independent retained audit."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--stage", choices=("native", "numerical", "audit"), required=True)
    parser.add_argument("--build", type=Path)
    parser.add_argument("--source", type=Path)
    parser.add_argument("--root", required=True, type=Path)
    parser.add_argument("--host", type=Path)
    parser.add_argument("--output", type=Path)
    args = parser.parse_args()
    if args.stage == "native":
        require(args.build is not None and args.source is not None, "native build/source required")
        result = native(args.build.resolve(), args.source.resolve(), args.root.resolve())
    else:
        require(args.host is not None, "independent host checker required")
        producer = Path(__file__).with_name("normalization_oracle_host.cxx")
        if args.stage == "numerical":
            result = numerical(args.root.resolve(), args.host.resolve(), producer)
        else:
            require(args.output is not None, "fresh audit output required")
            result = audit(args.root.resolve(), args.host.resolve(), producer, args.output.resolve())
    print(json.dumps({k: v for k, v in result.items() if k not in ("cases", "results", "binaries")}, sort_keys=True))


if __name__ == "__main__":
    main()
