#!/usr/bin/env python3
"""Audit mapped SYNC-6 residual-add evidence independently of the producer.

The input must be a separate-process ``ir_b2a -st -src`` trace plus the
checkpoint-owned residual report. The expected contract is documented in
doc/FHE-SYNC6-S6-0C-DETAILED-EXECUTION-PLAN.md and
doc/FHE-SYNC6-CKKS-IR-CONFORMANCE-GATE.md.
"""

from __future__ import annotations

import argparse
import json
import re
import sys
from pathlib import Path


LOWERED_RESIDUAL = re.compile(
    r"operator=OPR_DSLRESIDUALADD .*status=lowered "
    r"relation=ckks_expansion"
)
PHYSICAL_RESIDUAL = re.compile(r"^\s*OPR_DSLRESIDUALADD #", re.MULTILINE)
PHYSICAL_RELU = re.compile(r"^\s*OPR_DSLRELU #", re.MULTILINE)
RESULT_VALUE = re.compile(
    r"^\s*\[\d+\] kind=operator_result "
    r"name=(fhe_ckks_residual_s(\d+)_(align_(?:left|right)|add))\b",
    re.MULTILINE,
)
RESULT_STATE = re.compile(
    r"^\s*\[\d+\] value=value\d+"
    r"\((fhe_ckks_residual_s\d+_(?:align_(?:left|right)|add))\) "
    r"state_version=1 encryption=\d+ scheme=1 class=1 level=3 "
    r"scale_bits=56 components=2 precision_bits=30 slots=32768 "
    r"alignment_group=0 layout=ckks\.packed pending=0x0 "
    r"bootstrap_reason=none$",
    re.MULTILINE,
)
EVENT_HEADER = re.compile(r"CKKS Event Image: version=1 records=(\d+)")


def require(condition: bool, message: str) -> None:
    """Reject incomplete or contradictory evidence with one stable message."""

    if not condition:
        raise ValueError(message)


def parse_report(path: Path) -> dict[str, str]:
    """Read the small checkpoint report as unique ``key=value`` facts."""

    facts: dict[str, str] = {}
    for line in path.read_text(encoding="utf-8").splitlines()[1:]:
        if not line:
            continue
        require(line.count("=") == 1, f"malformed report line: {line}")
        key, value = line.split("=", 1)
        require(key not in facts, f"duplicate report key: {key}")
        facts[key] = value
    return facts


def audit(trace_path: Path, report_path: Path) -> dict[str, object]:
    """Cross-check source provenance, physical CKKS values, states, and report."""

    trace = trace_path.read_text(encoding="utf-8")
    lowered = LOWERED_RESIDUAL.findall(trace)
    physical = PHYSICAL_RESIDUAL.findall(trace)
    relu = PHYSICAL_RELU.findall(trace)
    values = RESULT_VALUE.findall(trace)
    states = RESULT_STATE.findall(trace)
    header = EVENT_HEADER.search(trace)
    require(len(lowered) == 9, "expected nine lowered residual source rows")
    require(not physical, "physical common.residual_add remains executable")
    require(len(relu) == 19, "expected nineteen still-live ReLU definitions")
    require(header is not None and int(header.group(1)) == 28943,
            "CKKS event census is not 28,943")

    names = [row[0] for row in values]
    add_ordinals = {int(row[1]) for row in values if row[2] == "add"}
    align_ordinals = {int(row[1]) for row in values if row[2] != "add"}
    require(len(names) == 16 and len(set(names)) == 16,
            "residual result names are missing or duplicated")
    require(len(add_ordinals) == 9, "expected nine residual add results")
    require(len(align_ordinals) == 7,
            "expected seven residual alignment results")
    require(align_ordinals < add_ordinals,
            "alignment result has no matching residual add")
    require(set(states) == set(names) and len(states) == 16,
            "residual result states are missing, duplicated, or nonconcrete")

    report = parse_report(report_path)
    expected_report = {
        "source_contexts": "9",
        "ckks_adds": "9",
        "ckks_modswitches": "7",
        "projection_joins_already_aligned": "2",
        "source_residual_nodes_live": "0",
    }
    require(report == expected_report, "residual report facts disagree")
    return {
        "ckks_event_count": 28943,
        "lowered_residual_sources": len(lowered),
        "residual_adds": len(add_ordinals),
        "residual_modswitches": len(align_ordinals),
        "live_residual_sources": len(physical),
        "live_relu_sources": len(relu),
        "add_static_ordinals": sorted(add_ordinals),
        "aligned_static_ordinals": sorted(align_ordinals),
    }


def main() -> int:
    """Parse command-line paths and print canonical review evidence as JSON."""

    parser = argparse.ArgumentParser()
    parser.add_argument("trace", type=Path)
    parser.add_argument("report", type=Path)
    args = parser.parse_args()
    try:
        result = audit(args.trace, args.report)
    except (OSError, UnicodeError, ValueError) as error:
        print(f"CFHEIR-RESIDUAL-AUDIT-001: {error}", file=sys.stderr)
        return 1
    print(json.dumps(result, sort_keys=True, indent=2))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
