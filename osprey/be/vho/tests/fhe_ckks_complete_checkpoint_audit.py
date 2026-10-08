#!/usr/bin/env python3
"""Audit the independently reopened complete S6-0c ResNet checkpoint.

This consumer reads only ``ir_b2a -st -src`` text and atomically published
reports/assets. It is independent of the producer implementation described in
doc/FHE-SYNC6-S6-0C-DETAILED-EXECUTION-PLAN.md, C7-C8.
"""

from __future__ import annotations

import argparse
import json
import re
import sys
from pathlib import Path


EVENT_HEADER = re.compile(r"CKKS Event Image: version=1 records=(\d+)")
FUNC_ENTRY = re.compile(r"^FUNC_ENTRY ", re.MULTILINE)
PHYSICAL_BOOTSTRAP = re.compile(
    r"^\s*OPR_DSLCKKSBOOTSTRAP #", re.MULTILINE
)
PHYSICAL_PRE_RELU_BOOTSTRAP = re.compile(
    r"^\s*OPR_DSLCKKSBOOTSTRAP #.*attr\.reason=PRE_RELU_REFRESH;",
    re.MULTILINE,
)
PHYSICAL_DEPTH_BOOTSTRAP = re.compile(
    r"^\s*OPR_DSLCKKSBOOTSTRAP #.*attr\.reason=DEPTH_EXHAUSTION;",
    re.MULTILINE,
)
PHYSICAL_CKKS = re.compile(r"^\s*OPR_DSLCKKS[A-Z0-9_]* #", re.MULTILINE)
EVENT_RESULT = re.compile(
    r"^  \[\d+\] owner=.* step=\d+ result=value(\d+) ", re.MULTILINE
)
CONCRETE_VALUE_STATE = re.compile(
    r"^  \[\d+\] value=value(\d+).* state_version=\d+ .*"
    r"level=-?\d+ scale_bits=\d+ components=\d+ precision_bits=\d+ "
    r"slots=\d+ ", re.MULTILINE
)
PHYSICAL_ROTATION = re.compile(
    r"^\s*OPR_DSLCKKSROTATE #.*attr\.signed_steps=(-?\d+);",
    re.MULTILINE,
)
REQUIRED_ROTATION = re.compile(
    r"^  \[\d+\] config=\d+ key_set=\S+ class=rotation "
    r"rotation=(-?\d+) ", re.MULTILINE
)


def require(condition: bool, message: str) -> None:
    """Reject incomplete or contradictory checkpoint evidence."""

    if not condition:
        raise ValueError(message)


def parse_report(path: Path) -> dict[str, str]:
    """Read one deterministic producer report as unique key/value facts."""

    lines = path.read_text(encoding="utf-8").splitlines()
    require(bool(lines), f"empty report: {path.name}")
    facts: dict[str, str] = {}
    for line in lines[1:]:
        if not line:
            continue
        require(line.count("=") == 1, f"malformed report line: {line}")
        key, value = line.split("=", 1)
        require(key not in facts, f"duplicate report key: {key}")
        facts[key] = value
    return facts


def physical_count(trace: str, operator: str) -> int:
    """Count executable physical WN occurrences, excluding logical tables."""

    return len(re.findall(
        rf"^\s*{re.escape(operator)} #", trace, flags=re.MULTILINE
    ))


def lowered_count(trace: str, operator: str) -> int:
    """Count retained logical sources marked as CKKS-expanded provenance."""

    return len(re.findall(
        rf"operator={re.escape(operator)} .*status=lowered "
        rf"relation=ckks_expansion", trace
    ))


def retired_count(trace: str, operator: str) -> int:
    """Count zero-motion or identity logical sources retired by redirection."""

    return len(re.findall(
        rf"operator={re.escape(operator)} .*status=retired "
        rf"redirected_to=value", trace
    ))


def require_report(actual: dict[str, str], expected: dict[str, str],
                   name: str) -> None:
    """Require all normative facts while permitting additive review fields."""

    for key, value in expected.items():
        require(actual.get(key) == value,
                f"{name} fact {key} is not {value}")


def audit(trace_path: Path, artifact_dir: Path) -> dict[str, object]:
    """Cross-check the full physical/logical census and published artifacts."""

    trace = trace_path.read_text(encoding="utf-8")
    header = EVENT_HEADER.search(trace)
    require(header is not None and int(header.group(1)) == 33367,
            "CKKS event census is not 33,367")
    require(len(PHYSICAL_CKKS.findall(trace)) == 33367,
            "physical CKKS WN census is not 33,367")
    event_results = {int(value) for value in EVENT_RESULT.findall(trace)}
    concrete_states = {
        int(value) for value in CONCRETE_VALUE_STATE.findall(trace)
    }
    require(len(event_results) == 33367,
            "CKKS event results are not 33,367 unique values")
    require(event_results <= concrete_states,
            "a CKKS event result lacks concrete value-specific state")
    require(len(FUNC_ENTRY.findall(trace)) == 10,
            "FUNC_ENTRY census is not ten")
    require("secure_resnet20.py" in trace,
            "source interleave does not name secure_resnet20.py")

    high_level = {
        "OPR_DSLCONV2D": 21,
        "OPR_DSLRESIDUALADD": 9,
        "OPR_DSLRELU": 19,
        "OPR_DSLGLOBALAVGPOOL2D": 1,
        "OPR_DSLLINEAR": 1,
    }
    for operator, expected_lowered in high_level.items():
        require(physical_count(trace, operator) == 0,
                f"live physical {operator} remains")
        require(lowered_count(trace, operator) == expected_lowered,
                f"lowered {operator} census disagrees")
    for operator in ("OPR_DSLFLATTEN", "OPR_DSLOUTPUTLOGITS"):
        require(physical_count(trace, operator) == 0,
                f"live physical {operator} remains")
        require(retired_count(trace, operator) == 1,
                f"retired {operator} census disagrees")
    require(physical_count(trace, "OPR_DSLBATCHNORM") == 0,
            "live physical OPR_DSLBATCHNORM remains")
    require(len(PHYSICAL_BOOTSTRAP.findall(trace)) == 23,
            "whole-model physical bootstrap census is not twenty-three")
    require(len(PHYSICAL_PRE_RELU_BOOTSTRAP.findall(trace)) == 19,
            "physical pre-ReLU bootstrap census is not nineteen")
    require(len(PHYSICAL_DEPTH_BOOTSTRAP.findall(trace)) == 4,
            "physical depth-exhaustion bootstrap census is not four")
    used_rotations = {int(value) for value in PHYSICAL_ROTATION.findall(trace)}
    required_rotations = {
        int(value) for value in REQUIRED_ROTATION.findall(trace)
    }
    require(used_rotations == required_rotations,
            "physical rotations and persisted rotation-key requirements differ")
    require("class=public rotation=0" in trace,
            "public-key requirement is absent")
    require("class=relinearization rotation=0" in trace,
            "relinearization-key requirement is absent")
    require("class=bootstrap rotation=0 " in trace and
            "bootstrap_profile=pre_relu_refresh_v1" in trace,
            "pre-ReLU bootstrap-key requirement is absent")

    stem = artifact_dir / "secure_resnet20.ckks_ops.B"
    paths = {
        "conv_asset": Path(f"{stem}.conv-plaintexts.f32"),
        "relu_asset": Path(f"{stem}.relu-scalars.f64"),
        "tail_asset": Path(f"{stem}.tail-plaintexts.f32"),
        "conv_report": Path(f"{stem}.conv-report.txt"),
        "residual_report": Path(f"{stem}.residual-report.txt"),
        "relu_report": Path(f"{stem}.relu-report.txt"),
        "tail_report": Path(f"{stem}.tail-report.txt"),
    }
    require(stem.is_file(), "published .ckks_ops.B is absent")
    for name, path in paths.items():
        require(path.is_file(), f"published {name} is absent")
    require(paths["conv_asset"].stat().st_size == 202620928,
            "Conv plaintext asset size disagrees")
    require(paths["relu_asset"].stat().st_size == 4712,
            "ReLU scalar asset size disagrees")
    require(paths["tail_asset"].stat().st_size == 1572864,
            "tail plaintext asset size disagrees")
    require(not list(artifact_dir.glob("*.tmp")),
            "published artifact family contains a temporary file")

    require_report(parse_report(paths["conv_report"]), {
        "contexts": "21",
        "source_definitions": "21",
        "feature_rows": "5691",
        "expanded_biases": "21",
        "stride_masks": "104",
        "ckks_operations": "28927",
        "side_asset_bytes": "202620928",
    }, "Conv report")
    require_report(parse_report(paths["residual_report"]), {
        "source_contexts": "9",
        "ckks_adds": "9",
        "ckks_modswitches": "9",
        "projection_joins_already_aligned": "0",
        "source_residual_nodes_live": "0",
    }, "residual report")
    require_report(parse_report(paths["relu_report"]), {
        "contexts": "19",
        "source_groups": "114",
        "recipe_scalars": "570",
        "ckks_operations": "4237",
        "source_relu_nodes_live": "0",
        "post_refresh_level_consumption": "12",
        "side_asset_bytes": "4712",
    }, "ReLU report")
    require_report(parse_report(paths["tail_report"]), {
        "global_average_pool_operations": "15",
        "linear_operations": "170",
        "tail_operations": "185",
        "whole_model_ckks_operations": "33367",
        "whole_model_bootstraps": "23",
        "pre_relu_refresh_bootstraps": "19",
        "depth_exhaustion_bootstraps": "4",
        "pool_nodes_live": "0",
        "flatten_nodes_live": "0",
        "linear_nodes_live": "0",
        "output_logits_nodes_live": "0",
        "side_asset_bytes": "1572864",
    }, "tail report")
    return {
        "func_entries": 10,
        "ckks_events": 33367,
        "bootstraps": 23,
        "pre_relu_refresh_bootstraps": 19,
        "depth_exhaustion_bootstraps": 4,
        "rotation_keys": len(required_rotations),
        "conv_operations": 28927,
        "residual_operations": 18,
        "relu_operations": 4237,
        "tail_operations": 185,
        "live_high_level_computations": 0,
    }


def main() -> int:
    """Parse evidence paths and print a canonical review summary."""

    parser = argparse.ArgumentParser()
    parser.add_argument("trace", type=Path)
    parser.add_argument("artifact_dir", type=Path)
    args = parser.parse_args()
    try:
        result = audit(args.trace, args.artifact_dir)
    except (OSError, UnicodeError, ValueError) as error:
        print(f"CFHEIR-COMPLETE-AUDIT-001: {error}", file=sys.stderr)
        return 1
    print(json.dumps(result, sort_keys=True, indent=2))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
