"""Authenticate the accepted SIMT baseline and prove MMA2 execution parity.

The old producer is not rerun or relabeled as MMA. Fresh current images and
traces are compared byte-for-byte; only the generated CUDA date may differ.
"""
import argparse
import difflib
import hashlib
import json
from pathlib import Path

from ai_opt_report import parse, render
from compute_shadow_report import source_summary


def digest(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


def cuda_body(text):
    return "".join(line for line in text.splitlines(True)
                   if not line.startswith(" * C file translated from WHIRL "))


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--legacy-receipt", type=Path, required=True)
    parser.add_argument("--current", type=Path, required=True)
    parser.add_argument("--output", type=Path, required=True)
    args = parser.parse_args()
    baseline, current, output = args.legacy_receipt.resolve(), args.current.resolve(), args.output.resolve()
    output.mkdir(parents=True, exist_ok=False)
    verified = 0
    for row in (baseline / "SHA256SUMS").read_text().splitlines():
        expected, name = row.split(None, 1)
        if digest(baseline / name) != expected:
            raise ValueError(f"baseline hash mismatch: {name}")
        verified += 1
    results = []
    for case in json.loads((current / "receipt.json").read_text())["cases"]:
        label = case["label"]
        old = baseline / "acceptance/occupancy" / label
        new = current / label
        trace = (new / "gemm.t").read_text()
        _, selection, _ = parse(trace)
        _, previous, _ = parse((old / "gemm.t").read_text())
        if selection != previous:
            raise ValueError(f"SIMT costs/selection changed: {label}")
        summary = source_summary(trace, selection["joint"]["chosen"]["id"])
        if "timing=unknown" not in summary:
            raise ValueError("missing explicit source timing/eligibility")
        destination = output / label
        destination.mkdir()
        hashes = {}
        for name in ("gemm.before.B", "gemm.after.B", "gemm.before.T", "gemm.after.T"):
            old_hash, new_hash = digest(old / name), digest(new / name)
            if old_hash != new_hash:
                raise ValueError(f"SIMT WHIRL changed: {label}/{name}")
            hashes[name] = new_hash
        for boundary in ("before", "after"):
            name = f"gemm.{boundary}.T"
            (destination / f"baseline-current.{boundary}.diff").write_text("".join(difflib.unified_diff(
                (old / name).read_text().splitlines(True), (new / name).read_text().splitlines(True),
                fromfile="M9/" + name, tofile="MMA2/" + name)))
        old_cuda, new_cuda = (old / "gemm.whirl.cu").read_text(), (new / "gemm.whirl.cu").read_text()
        (destination / "cuda.raw.diff").write_text("".join(difflib.unified_diff(
            old_cuda.splitlines(True), new_cuda.splitlines(True), fromfile="M9/gemm.whirl.cu", tofile="MMA2/gemm.whirl.cu")))
        if cuda_body(old_cuda) != cuda_body(new_cuda):
            raise ValueError("CUDA changed beyond translator date")
        hashes["cuda_without_date"] = hashlib.sha256(cuda_body(new_cuda).encode()).hexdigest()
        old_rows = "\n".join(row for row in (old / "gemm.t").read_text().splitlines() if row.startswith("AI_OPT "))
        new_rows = "\n".join(row for row in trace.splitlines() if row.startswith("AI_OPT "))
        (destination / "ai-opt.raw.diff").write_text("".join(difflib.unified_diff(
            old_rows.splitlines(True), new_rows.splitlines(True), fromfile="M9/AI_OPT", tofile="MMA2/AI_OPT")))
        if old_rows != new_rows:
            raise ValueError("existing trace schema/rows changed")
        render(new / "gemm.t", destination / "reports")
        results.append(dict(label=label, hashes=hashes, selected=selection["joint"]["chosen"]["id"],
                            kind=selection["joint"]["chosen"]["kind"], timing="unknown", action="none"))
    (output / "report.json").write_text(json.dumps(dict(schema="mma2-simt-parity-v1",
        baseline=str(baseline), baseline_files_verified=verified, cases=results,
        native_verified=False, typed_fallback_implemented=False), indent=2) + "\n")
    (output / "summary.log").write_text(
        "# What it summarizes: Authenticated old versus fresh MMA2 SIMT WHIRL/CUDA and selection parity.\n"
        f"baseline_files_verified={verified} cases={len(results)} images_and_dumps_identical={len(results) * 4} "
        "existing_ai_opt_rows=identical cuda_body=identical ignored_cuda_difference=translator_date_only "
        "mma_timing=unknown action=none native=unverified\n")
    print((output / "summary.log").read_text(), end="")


if __name__ == "__main__":
    main()
