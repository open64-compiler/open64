#!/usr/bin/env python3
"""Audit MMA1's actual common inventory and authenticated SIMT evidence.

Design: doc/AI-COMPILER-MMA-COMPUTE-REALIZATION-DESIGN-v0.1.md.
This runner never selects a kernel, edits an image or claims GPU certification.
"""
import argparse
import hashlib
import json
from pathlib import Path
import shutil
import sys


def digest(path):
    """Hash retained input bytes, independently of producer metadata."""
    return hashlib.sha256(path.read_bytes()).hexdigest()


def audit_inventory(text):
    """Check exact recipe facts and inspection coverage without the C++ verifier."""
    groups = text.split("# What it summarizes:")[1:]
    if len(groups) != 4:
        raise ValueError("incomplete profile census")
    results = []
    for ident, group in enumerate(groups, 1):
        rows = {}
        for line in group.splitlines():
            fields = line.split()
            if fields[:2] == ["AI_COMPUTE", "1"]:
                rows.setdefault(fields[2], []).append(fields[3:])
        if set(rows) != {"PROFILE", "LIMITS", "CAND", "REPRESENTATION", "PROTOCOL", "RESOURCE", "FALLBACK"}:
            raise ValueError("missing inspection row")
        profile, = rows["PROFILE"]
        hopper = ident <= 2
        arch = "sm_90a" if hopper else "sm_100a"
        dtype = "f16" if ident % 2 else "bf16"
        variant = ("wgmma_m64n64k16_" if hopper else "tcgen05_m128n64k16_") + dtype + "_ss"
        expected = [str(ident), "1", arch, variant, "2" if hopper else "3",
                    "64" if hopper else "128", "64", "16", "128",
                    "128" if hopper else "1", "0" if hopper else "32",
                    "1" if hopper else "2", "1" if hopper else "2",
                    "32" if hopper else "0", "0" if hopper else "128",
                    "0" if hopper else "64", "800" if hopper else "806",
                    "12000" if hopper else "12080", "807", "12081",
                    "unknown", "unknown", "required", "required"]
        if profile != expected:
            raise ValueError("profile differs from independent pinned ISA inventory")
        limits, = rows["LIMITS"]
        if limits != [str(ident), "1", "16", "0" if hopper else "32",
                      "0" if hopper else "512", "1" if hopper else "2", "1" if hopper else "2",
                      "isa_documented", "encoding_required"]:
            raise ValueError("resource or descriptor limits differ")
        if len(rows["REPRESENTATION"]) != 4 or any(row[-1] != "27" for name in rows if name not in ("PROFILE", "LIMITS") for row in rows[name]):
            raise ValueError("representation coverage or final source line missing")
        candidate, = rows["CAND"]
        if candidate != ["1", "10", "1", "2", "3", "4", "5", "6", "7", "8", "9", "1", "planning_only", "27"]:
            raise ValueError("candidate identity or status differs")
        for index, representation in enumerate(rows["REPRESENTATION"]):
            semantic = str(index + 1) if index < 2 else "3"
            if representation != ["1", str(index), str(index + 11), semantic, str(index + 20),
                                  semantic, str(index + 30), "1" if hopper else "2",
                                  "5" if index < 2 else "0", "27"]:
                raise ValueError("representation identity or source differs")
        protocol, = rows["PROTOCOL"]
        if protocol != ["8", "1" if hopper else "2", "127" if hopper else "4091",
                        "1", "2", "3", "4", "5", "0" if hopper else "6",
                        "0" if hopper else "7", "0" if hopper else "8", "1", "required", "27"]:
            raise ValueError("completion requirements differ")
        resource, = rows["RESOURCE"]
        if resource != ["1", "128", "1020", "4096" if hopper else "6144", "49152", "128", "1024", "0" if hopper else "128",
                        "0" if hopper else "64", "0" if hopper else "64", "4", "unknown", "27"]:
            raise ValueError("resource requirements differ")
        fallback, = rows["FALLBACK"]
        if fallback != ["9", "simt", "6", "5" if ident % 2 else "6", "1", "0", "required_unimplemented", "27"]:
            raise ValueError("typed fallback confused with current F32 implementation")
        results.append(dict(profile=ident, architecture=arch, variant=variant,
                            capability="isa_documented", lowering="required",
                            native="required", timing="unknown"))
    if "276 malformed cases" not in text:
        raise ValueError("missing negative-test completion")
    return results


def audit_legacy(root, output):
    """Authenticate M9, then replay actual current readers without rerunning its producer."""
    receipt = json.loads((root / "receipt.json").read_text())
    if digest(root / receipt["primary_artifact"]) != receipt["primary_sha256"]:
        raise ValueError("legacy primary artifact mismatch")
    count = 0
    for line in (root / "SHA256SUMS").read_text().splitlines():
        expected, name = line.split(None, 1)
        relative = Path(name.lstrip("*"))
        if relative.is_absolute() or ".." in relative.parts or digest(root / relative) != expected:
            raise ValueError("legacy checksum mismatch: " + name)
        count += 1
    repo = Path(__file__).resolve().parents[4]
    sys.path.insert(0, str(repo / "osprey/be/vho/tests/ai_matmul_cuda"))
    from ai_opt_report import parse
    rankings = json.loads((root / "acceptance/rankings.json").read_text())
    for row in rankings:
        trace = root / "acceptance/occupancy" / row["label"] / "gemm.t"
        _, selection, _ = parse(trace.read_text())
        if selection["joint"]["chosen"]["id"] != row["new_id"] or selection["joint"]["chosen"]["total"] != row["new_cost"]:
            raise ValueError("SIMT compatibility ranking mismatch")
    for label in ("direct", "staged", "large-tail"):
        destination = output / "cached-simt" / label
        destination.mkdir(parents=True)
        source = root / "acceptance/occupancy" / label
        for name in ("gemm.t", "gemm.before.B", "gemm.before.T", "gemm.after.B", "gemm.after.T", "gemm.before-after.diff", "gemm.whirl.cu"):
            shutil.copy2(source / name, destination / name)
        summary = source / "reports/optimization-summary.log"
        shutil.copy2(summary, destination / summary.name)
    return dict(receipt=str(root), receipt_sha256=digest(root / "receipt.json"),
                verified_files=count, readiness_traces=len(rankings), evidence="cached_producer_fresh_reader_audit")


def main():
    """Write fresh audit evidence only to a new directory; preserve all earlier runs."""
    parser = argparse.ArgumentParser()
    parser.add_argument("--inventory-log", type=Path, required=True)
    parser.add_argument("--legacy-receipt", type=Path, required=True)
    parser.add_argument("--output", type=Path, required=True)
    args = parser.parse_args()
    args.output.mkdir(parents=True, exist_ok=False)
    profiles = audit_inventory(args.inventory_log.read_text())
    negative_audits = 0
    for old, new in (("sm_90a", "sm_90"), ("sm_100a", "sm_120a"),
                     ("unknown unknown", "0 0"), ("required_unimplemented", "implemented"),
                     ("isa_documented", "native_certified"),
                     ("AI_COMPUTE 1 CAND 1 10", "AI_COMPUTE 1 CAND 1 99"),
                     ("PROTOCOL 8 1 127 1 2 3", "PROTOCOL 8 1 127 1 2 2")):
        try:
            audit_inventory(args.inventory_log.read_text().replace(old, new))
        except ValueError:
            negative_audits += 1
        else:
            raise ValueError("auditor accepted malformed inspection stream")
    legacy = audit_legacy(args.legacy_receipt, args.output)
    shutil.copy2(args.inventory_log, args.output / "compute.log")
    report = dict(schema="open64-mma1-review-v1", profiles=profiles, negative_audits=negative_audits,
                  malformed_contracts=276, legacy=legacy, selection="unchanged_simt",
                  lowering=False, native_gpu_verified=False)
    (args.output / "report.json").write_text(json.dumps(report, indent=2) + "\n")
    (args.output / "summary.log").write_text(
        "# What it summarizes: MMA1 structural inventory, unresolved lowering/fallback and fresh audit of retained scalar GEMM evidence.\n"
        "profiles=4 contract_negatives=276 auditor_negatives=7 timing=unknown lowering=required native=required\n"
        "fallback=required_same_contract_f16_or_bf16_to_f32 current_f32_buffer_fallback=unavailable\n"
        f"legacy_files_verified={legacy['verified_files']} readiness_traces={legacy['readiness_traces']} selection=unchanged_simt producer=cached reader_audit=fresh\n"
        "MMA2=not_started MMA3=not_started MMA4_H=not_started MMA4_B=not_started MMA7=GPU_unavailable\n")
    print(f"MMA1 independent review passed: {args.output}")


if __name__ == "__main__":
    main()
