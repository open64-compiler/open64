"""Audit native R1 verification events, not optimization or execution claims.

Design: doc/AI-COMPILER-NATIVE-REDUCTION-CONTRACT-v0.1.md.
"""
import argparse
from pathlib import Path

HEADER = "AI_REDUCE 1 HEADER phase node value rows width axis keepdim accum_dtype reassociate status source_line"


def audit(text):
    """Require complete entry/exit pairs with identical native contract facts."""
    pending = {}
    verified = []
    seen_header = False
    for line in text.splitlines():
        if not line.startswith("AI_REDUCE "):
            continue
        if line == HEADER:
            seen_header = True
            continue
        row = line.split()
        if not seen_header or len(row) != 14 or row[:3] != ["AI_REDUCE", "1", "FORM"]:
            raise ValueError("unsupported or malformed reduction trace row")
        phase = row[3]
        numbers = [int(row[i]) for i in (4, 5, 6, 7, 13)]
        if min(numbers) <= 0 or row[8:13] != ["1", "0", "float32", "0", "verified"]:
            raise ValueError("noncanonical reduction contract")
        key = tuple(numbers[:2])
        facts = tuple(row[4:])
        if phase == "preopt_enter":
            if key in pending:
                raise ValueError("duplicate reduction entry")
            pending[key] = facts
        elif phase == "preopt_exit":
            if pending.pop(key, None) != facts:
                raise ValueError("reduction exit differs from entry")
            verified.append(facts)
        else:
            raise ValueError("unknown PREOPT event")
    if pending or not verified:
        raise ValueError("missing complete PREOPT reduction evidence")
    return verified


def write_reports(trace, output):
    """Produce conventional readable logs only after a strict trace audit."""
    rows = audit(trace.read_text())
    output.mkdir(parents=True, exist_ok=False)
    detail = ["# What it summarizes: native row-sum contracts preserved by PREOPT; no executable reduction lowering."]
    detail.append("node value rows width axis keepdim accum_dtype reassociate status source_line")
    detail.extend(" ".join(row) for row in rows)
    (output / "semantic.log").write_text("\n".join(detail) + "\n")
    (output / "optimization-summary.log").write_text(
        "# What it summarizes: verified reduction contracts, phase coverage and remaining optimization gates.\n"
        "P0/PREOPT contracts_verified=" + str(len(rows)) + " entry_exit=identical\n"
        "candidate_selection=not_run profitability=not_run transformation=none\n"
        "execution_lowering=unsupported native_gpu=not_run\n")
    (output / "optimization.log").write_text(
        "# What it summarizes: complete successful phase-report collection for this native reduction contract slice.\n"
        + "\n".join(detail) + "\n")
    return rows


def main():
    """Expose trace auditing independently from the native acceptance runner."""
    parser = argparse.ArgumentParser()
    parser.add_argument("--trace", type=Path, required=True)
    parser.add_argument("--reports", type=Path, required=True)
    args = parser.parse_args()
    write_reports(args.trace, args.reports)


if __name__ == "__main__":
    main()
