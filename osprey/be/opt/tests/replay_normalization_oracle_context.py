#!/usr/bin/env python3
"""Authenticate/reopen LN1-c evidence accompanying the separate LN1-d oracle.

Design: doc/AI-COMPILER-LAYERNORM-ARITHMETIC-ORACLE-v0.1.md.
Uses retained native binaries in an offline Linux container; production sources
must still match their receipt. This does not execute a LayerNorm kernel.
"""
import argparse
import hashlib
import json
from pathlib import Path
import subprocess

from normalization_ref_trace_report import audit as audit_ref
from normalization_target_trace_report import audit, audit_carriers
from normalization_trace_report import audit as audit_v1


def sha256(path):
    """Authenticate exact retained/current content rather than trusting a path."""
    return hashlib.sha256(path.read_bytes()).hexdigest()


def require(condition, message):
    """Keep evidence gates active even if Python assertions are disabled."""
    if not condition:
        raise ValueError(message)


def replay(parent, source, output):
    """Reopen all mapped boundaries and rerun native producers and actual PREOPT."""
    parent, source, output = parent.resolve(), source.resolve(), output.resolve()
    output.mkdir(parents=True, exist_ok=False)
    verified = 0
    for record in (parent / "SHA256SUMS").read_text().splitlines():
        digest, name = record.split("  ", 1)
        require(sha256(parent / name) == digest, "parent checksum mismatch: " + name)
        verified += 1
    receipt = json.loads((parent / "receipt.json").read_text())
    for name, digest in receipt["source_hashes"].items():
        if not name.startswith("doc/"):
            require(sha256(source / name) == digest, "native source changed: " + name)
    commands, cases = [], []

    def run(label, arguments, success=True):
        """Retain exact argv/status/streams, including expected lowering refusal."""
        result = subprocess.run(arguments, text=True, capture_output=True, check=False)
        commands.append(dict(label=label, argv=arguments, returncode=result.returncode))
        (output / (label + ".stdout")).write_text(result.stdout)
        (output / (label + ".stderr")).write_text(result.stderr)
        (output / "commands.json").write_text(json.dumps(commands, indent=2) + "\n")
        if success:
            require(result.returncode == 0, label + ": " + result.stderr)
        return result

    base = ["docker", "run", "--rm", "--platform", "linux/amd64", "--network", "none",
            "-e", "PYTHONDONTWRITEBYTECODE=1", "-e", "LD_LIBRARY_PATH=/retained/binaries",
            "-v", str(parent) + ":/retained:ro", "-v", str(parent / "source") + ":/open64:ro",
            "-v", str(parent / "source") + ":/repo/open64:ro", "-v", str(output) + ":/out",
            "-w", "/out", "open64:integration-build-env"]
    for arch in ("sm_90", "sm_90a", "sm_100", "sm_100a"):
        case = output / arch
        case.mkdir()
        old = parent / "target" / arch
        for stem in ("ln_ref", "scalar.before", "scalar.after"):
            run(arch + "-" + stem, base + ["/retained/binaries/ir_b2a", "-st", "-src",
                f"/retained/target/{arch}/{stem}.B", f"/out/{arch}/{stem}.T"])
            require((case / (stem + ".T")).read_bytes() == (old / (stem + ".T")).read_bytes(), "full mapped dump changed")
        require(len(audit((old / "target.t").read_text(), (old / "preopt-on.t").read_text())) == 39, "target census changed")
        require(audit_ref((old / "preopt-on.t").read_text()) == audit_ref((old / "default-on.t").read_text()), "default trace drift")
        require("AI_NORM" not in (old / "trace-off.t").read_text(), "disabled trace emitted facts")
        audit_carriers((old / "scalar.after.cu").read_text(), (case / "scalar.after.T").read_text(), (old / "scalar.after.ptx").read_text())
        run(arch + "-producer", base + ["/retained/binaries/dsl_normalization_target_test",
            f"/out/{arch}/ln_ref.B", f"/out/{arch}/target.t", arch])
        for name in ("ln_ref.B", "target.t"):
            require((case / name).read_bytes() == (old / name).read_bytes(), "native producer drift: " + name)
        result = run(arch + "-preopt", base + ["/retained/binaries/be", f"-fB,/out/{arch}/ln_ref.B",
            f"-fo,/out/{arch}/replay.obj", f"-ft,/out/{arch}/preopt.t", "-PHASE:w=on:c=off:wpath=/retained/binaries",
            "-O2", "-DSL:wopt=on", "-WOPT:cr_simp=on", "-tr25", "-DSL:ai_opt_trace=on",
            "/open64/osprey/be/opt/tests/dsl_normalization_ref_test.cxx"], success=False)
        require(result.returncode != 0 and "OPR_DSLMEANVARNORM.v2 has no executable VHO lowering route" in result.stderr,
                "missing intended executable-lowering refusal")
        require(not (case / "replay.obj").exists(), "unsupported LayerNorm published an object")
        require(audit_ref((case / "preopt.t").read_text()) == audit_ref((old / "preopt-on.t").read_text()), "PREOPT semantic drift")
        cases.append(dict(architecture=arch, full_dumps_equal=3, native_queries=39,
                          fresh_producer_equal=True, actual_PREOPT=True, expected_lowering_refusal=True))
    for variant in ("default", "unit", "wide", "epsilon2"):
        case = output / ("v1-" + variant)
        case.mkdir()
        old = parent / "v1" / variant
        run("v1-" + variant, base + ["/retained/binaries/ir_b2a", "-st", "-src",
            f"/retained/v1/{variant}/normalization.B", f"/out/v1-{variant}/normalization.T"])
        require((case / "normalization.T").read_bytes() == (old / "ascii/normalization.T").read_bytes(), "v1 mapped dump changed")
        traces = list((old / "trace").glob("*preopt-on*"))
        require(len(traces) == 1, "v1 trace missing or ambiguous")
        audit_v1(traces[0].read_text())
    report = dict(parent_receipt=str(parent), parent_receipt_sha256=sha256(parent / "receipt.json"),
                  parent_files_verified=verified, unchanged_native_sources=True, cases=cases, v1_variants=4,
                  numerical_connection="separate specification witnesses; no compiled LayerNorm execution")
    (output / "replay-report.json").write_text(json.dumps(report, indent=2) + "\n")
    return report


def main():
    """Require explicit immutable parent, unchanged source and fresh output paths."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--parent", required=True, type=Path)
    parser.add_argument("--source", required=True, type=Path)
    parser.add_argument("--output", required=True, type=Path)
    args = parser.parse_args()
    print(json.dumps(replay(args.parent, args.source, args.output), indent=2))


if __name__ == "__main__":
    main()
