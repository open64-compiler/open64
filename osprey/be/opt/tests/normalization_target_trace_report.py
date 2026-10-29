"""Audit LN1-c conditional target contracts, not numerical/GPU certification.

Design: doc/AI-COMPILER-LAYERNORM-TARGET-ARITHMETIC-v0.1.md.
"""
import argparse
from pathlib import Path
import re
import struct

from normalization_ref_trace_report import audit as audit_ref
from normalization_trace_report import audit as audit_v1

COLUMNS = ("pu node result recipe arch arch_id route toolkit ptx environment fp_options "
           "fast_math roundoff ieee source_match status stages count_bits reason source_line").split()
HEADER = "AI_NORM_TARGET 1 HEADER " + " ".join(COLUMNS)
ARCHITECTURES = {"sm_90": 1, "sm_90a": 2, "sm_100": 3, "sm_100a": 4}
REASONS = ("conditional_isa_carrier_binding", "source_unbound", "stale_source",
           "unknown_architecture", "unsupported_route", "unsupported_toolchain", "unsafe_environment")
INSTRUCTIONS = ("add.rn.f32", "sub.rn.f32", "mul.rn.f32", "div.rn.f32", "sqrt.rn.f32")


def audit(text, source_trace):
    """Recompute status precedence, stage evidence and exact native source joins."""
    source = []
    if "AI_NORM_REF 1 BIND " in source_trace:
        source.extend(audit_ref(source_trace))
    if "AI_NORM 1 FORM " in source_trace:
        source.extend(audit_v1(source_trace))
    joins = {(f["pu"], f["node"], f["result"]): f for f in source}
    seen, rows = False, []
    for line in text.splitlines():
        if not line.startswith("AI_NORM_TARGET "):
            continue
        if line == HEADER:
            seen = True
            continue
        fields = line.split()
        if not seen or fields[:3] != ["AI_NORM_TARGET", "1", "BIND"] or len(fields) != len(COLUMNS) + 3:
            raise ValueError("invalid normalization target schema")
        row = dict(zip(COLUMNS, fields[3:]))
        integers = [name for name in COLUMNS if name not in ("arch", "count_bits", "reason")]
        if any(not re.fullmatch(r"0|[1-9][0-9]*", row[name]) for name in integers):
            raise ValueError("noncanonical target numeric field")
        f = joins.get((row["pu"], row["node"], row["result"]))
        if not f or row["source_line"] != f["source_line"] or row["recipe"] != ("2" if "reference" in f else "1"):
            raise ValueError("target contract lacks exact canonical source")
        n = {key: int(row[key]) for key in integers}
        if any(value > 2**32 - 1 for value in n.values()):
            raise ValueError("target field exceeds native UINT32")
        if n["source_match"] not in (0, 1) or n["fast_math"] not in (0, 1):
            raise ValueError("invalid target boolean")
        arch = ARCHITECTURES.get(row["arch"], 0)
        floor = (12080, 807) if arch in (3, 4) else (12000, 800)
        status, stages, arch_id = 0, 0, 0
        if not n["source_match"]:
            status = 2
        elif n["recipe"] != 2:
            status = 1
        else:
            stages = 1
            if not arch:
                status = 3
            else:
                arch_id = arch
                if n["route"] != 1:
                    status = 4
                elif n["toolkit"] < floor[0] or n["ptx"] < floor[1]:
                    status = 5
                elif n["environment"] != 1 or n["fp_options"] or n["fast_math"] or n["roundoff"] or n["ieee"] > 1:
                    status = 6
                else:
                    stages = 3
        bits = struct.pack(">f", int(f["width"])).hex() if status == 0 else "00000000"
        if (n["status"], n["stages"], n["arch_id"], row["count_bits"], row["reason"]) != (
                status, stages, arch_id, bits, REASONS[status]):
            raise ValueError("invented target binding or refusal")
        rows.append(row)
    if not rows:
        raise ValueError("missing native target evidence")
    return rows


def audit_carriers(cuda, mapped, ptx):
    """Check the emitted operands/effects, mapped instructions and actual PTX.

    No approximate instruction is substituted; ptxas may internally expand the
    exact div/sqrt at SASS level, which this milestone does not certify.
    """
    if 'extern "C" __global__ void ln_ref_scalar_carriers' not in cuda:
        raise ValueError("missing scalar device PU")
    if cuda.count("__asm__ volatile") != 5 or cuda.count(': "memory"') != 5 or cuda.count('"l"(') != 14:
        raise ValueError("scalar operands/effects lost during emission")
    for instruction in INSTRUCTIONS:
        if cuda.count(instruction) != 1 or instruction not in mapped or instruction not in ptx:
            raise ValueError("rounded instruction missing: " + instruction)
    for text in (cuda, mapped, ptx):
        if any(token in text for token in (".ftz", "rsqrt.", "div.approx", "sqrt.approx", "fma.", "mad.")):
            raise ValueError("unsafe instruction replacement")


def write_reports(trace, source_trace, output):
    """Publish compact conventional logs only after independent target audit."""
    rows = audit(trace.read_text(), source_trace.read_text())
    output.mkdir(parents=True, exist_ok=False)
    target = "# What it summarizes: native LN-REF-1 source/target joins, instruction/environment eligibility and explicit refusals.\n"
    target += "".join("target=" + r["arch"] + " node=" + r["node"] + " status=" + r["reason"] +
        " stages=" + r["stages"] + " count_bits=" + r["count_bits"] + " source_line=" + r["source_line"] + "\n" for r in rows)
    target += "route=ptx_pointer_scalar_v1 instructions=" + ",".join(INSTRUCTIONS) + "\n"
    target += "environment=rne_gradual_no_traps nan_policy=class_only source_flags=unchanged compiler_contract=conditional\n"
    target += "actual_toolchain=separate_compile_report oracle=not_run storage_events=unbound plan_selection=not_run normalization_lowering=unsupported GPU=not_run\n"
    (output / "target-binding.log").write_text(target)
    (output / "optimization.log").write_text("# What it summarizes: complete successful target binding phase report.\n" + target)
    (output / "optimization-summary.log").write_text(
        "# What it summarizes: target contract/refusal counts and remaining numerical, planning and execution gates.\n" +
        "queries=" + str(len(rows)) + " conditional_bound=" + str(sum(r["status"] == "0" for r in rows)) +
        " refusals=" + str(sum(r["status"] != "0" for r in rows)) + " v1_migration=none\n" +
        "".join("P%d status=%s\n" % (p, "analyzed_no_rewrite" if p == 0 else "not_run_contract_only") for p in range(12)) +
        "LayerNorm_rewrites=0 scalar_carrier_fixture=separate oracle=not_run GPU=not_run\n")
    return rows


def main():
    """Replay actual raw traces without compiler linkage or ML dependencies."""
    parser = argparse.ArgumentParser()
    parser.add_argument("--trace", type=Path, required=True)
    parser.add_argument("--source-trace", type=Path, required=True)
    parser.add_argument("--reports", type=Path, required=True)
    args = parser.parse_args()
    write_reports(args.trace, args.source_trace, args.reports)


if __name__ == "__main__":
    main()
