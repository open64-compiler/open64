"""Reference algorithm for the SYNC-4 context-sensitive ReLU schedule."""

from __future__ import annotations

import argparse
import hashlib
import json
from pathlib import Path
from typing import Any, Dict, Iterable, List, Optional

from fhe_sync4_capability_manifest import validate_manifest as validate_capability


PROFILE = "ace.chebyshev.sign.7x15x13.depth11.v2"
EVIDENCE_PROFILE = "ace.chebyshev.sign.7x15x13.depth11.v1"
OPERATION_KINDS = (
    "refresh",
    "normalize",
    "approx_stage",
    "approx_stage",
    "approx_stage",
    "reconstruct_relu",
)
STAGE_DEGREES = (7, 15, 13)
STAGE_CONSUMPTION = (3, 4, 4)


class MaterializationError(ValueError):
    """Raised when the SYNC-4 schedule cannot be constructed exactly."""


def _require(condition: bool, message: str) -> None:
    if not condition:
        raise MaterializationError(message)


def _sha256(path: Path) -> str:
    return hashlib.sha256(path.read_bytes()).hexdigest()


def _load(path: Path) -> Dict[str, Any]:
    value = json.loads(path.read_text(encoding="utf-8"))
    _require(isinstance(value, dict), f"{path.name} must contain an object")
    return value


def _contexts_by_path(records: Iterable[Dict[str, Any]], source: str) -> Dict[str, Dict[str, Any]]:
    result: Dict[str, Dict[str, Any]] = {}
    for record in records:
        _require(isinstance(record, dict), f"{source} context is invalid")
        path = record.get("instance_path")
        _require(isinstance(path, str) and path, f"{source} context has no route")
        _require(path not in result, f"{source} contains duplicate route {path}")
        result[path] = record
    return result


def _validate_mode(mode: str) -> None:
    _require(mode in {"auto", "on", "manual", "off"},
             f"unknown bootstrap mode {mode}")
    if mode == "off":
        raise MaterializationError(
            "CFHEMAT-RELU-003: bootstrap=off rejects surviving common.relu"
        )


def _validate_existing_manual_schedule(
    existing: Optional[Dict[str, Any]], expected: Dict[str, Any]
) -> Dict[str, Any]:
    _require(isinstance(existing, dict),
             "CFHEMAT-RELU-004: manual mode requires a pre-existing schedule")
    _require(existing.get("status") in
             {"complete_reference_schedule", "persisted_complete_schedule"},
             "CFHEMAT-RELU-004: manual schedule is incomplete")
    for field in (
        "schema",
        "bootstrap_mode",
        "profile_name",
        "context_count",
        "operation_count",
        "source_manifests",
        "refresh_level_counts",
        "operations",
    ):
        _require(existing.get(field) == expected.get(field),
                 f"CFHEMAT-RELU-004: manual schedule mismatch in {field}")
    return existing


def build_schedule(
    range_manifest_path: Path,
    ckks_manifest_path: Path,
    coefficient_manifest_path: Path,
    capability_manifest_path: Path,
    mode: str = "auto",
    existing_materialization: Optional[Dict[str, Any]] = None,
) -> Dict[str, Any]:
    ranges = _load(range_manifest_path)
    ckks = _load(ckks_manifest_path)
    coefficients = _load(coefficient_manifest_path)
    capability = _load(capability_manifest_path)
    validate_capability(capability)

    _require(ranges.get("status") == "approved", "range manifest is unapproved")
    _require(ranges.get("profile_name") == EVIDENCE_PROFILE,
             "range profile mismatch")
    _require(ckks.get("status") == "approved_static_compiler_schedule",
             "CKKS schedule is unapproved")
    _require(ckks.get("profile_name") == EVIDENCE_PROFILE,
             "CKKS profile mismatch")
    _require(coefficients.get("status") == "approved_empirical_ace",
             "coefficient manifest is unapproved")
    coefficient_profile = coefficients.get("profile")
    _require(isinstance(coefficient_profile, dict) and
             coefficient_profile.get("name") == EVIDENCE_PROFILE,
             "coefficient profile mismatch")
    _require(capability["profile"]["name"] == PROFILE,
             "capability profile mismatch")
    _require(capability["profile"]["coefficient_manifest_sha256"] ==
             _sha256(coefficient_manifest_path),
             "CFHEMAT-RELU-005: coefficient manifest hash mismatch")

    range_by_path = _contexts_by_path(ranges.get("contexts", []), "range manifest")
    ckks_by_path = _contexts_by_path(ckks.get("contexts", []), "CKKS schedule")
    _require(len(range_by_path) == 19, "exactly 19 approved ranges are required")
    _require(set(range_by_path) == set(ckks_by_path),
             "range and CKKS routes do not match")
    _validate_mode(mode)
    _require(mode == "manual" or existing_materialization is None,
             "pre-existing schedule is legal only with bootstrap=manual")

    stages = coefficients.get("stages")
    _require(isinstance(stages, list) and len(stages) == 3,
             "exactly three coefficient stages are required")
    _require([stage.get("degree") for stage in stages] == list(STAGE_DEGREES),
             "coefficient stage order is invalid")
    stage_hashes = [stage.get("coefficient_bytes_sha256") for stage in stages]
    _require(all(isinstance(value, str) and len(value) == 64
                 for value in stage_hashes),
             "coefficient stage hashes are invalid")

    operations: List[Dict[str, Any]] = []
    for path in sorted(range_by_path):
        range_record = range_by_path[path]
        ckks_record = ckks_by_path[path]
        bound = range_record.get("bound_b")
        refresh_level = ckks_record.get("post_refresh_level")
        _require(isinstance(bound, (int, float)) and bound > 0,
                 f"invalid positive bound for {path}")
        _require(refresh_level in {15, 17, 18},
                 f"invalid post-refresh level for {path}")
        identity = {
            "owner_pu_st": range_record.get("owner_pu_st"),
            "source_relu_value_id": range_record.get("source_relu_value_id"),
            "context_pu_identity_id": range_record.get("context_pu_identity_id"),
            "context_callsite_id": range_record.get("context_callsite_id"),
            "instance_path": path,
        }
        _require(identity["owner_pu_st"] and
                 isinstance(identity["source_relu_value_id"], int) and
                 isinstance(identity["context_pu_identity_id"], int) and
                 isinstance(identity["context_callsite_id"], int),
                 f"invalid context identity for {path}")

        levels = [
            refresh_level,
            refresh_level,
            refresh_level - 3,
            refresh_level - 7,
            refresh_level - 11,
            refresh_level - 11,
        ]
        _require(levels[-1] >= 0, f"negative final level for {path}")
        state_roles = (
            ("post_refresh", 1),
            ("post_operation", 1),
            ("post_operation", 2),
            ("post_operation", 3),
            ("post_operation", 4),
            ("result", 1),
        )
        for ordinal, kind in enumerate(OPERATION_KINDS):
            record: Dict[str, Any] = dict(identity)
            record.update(
                {
                    "operation_ordinal": ordinal,
                    "operation_kind": kind,
                    "profile_name": PROFILE,
                    "input_level": None if ordinal == 0 else levels[ordinal - 1],
                    "output_level": levels[ordinal],
                    "output_state_role": state_roles[ordinal][0],
                    "output_state_version": state_roles[ordinal][1],
                    "scale_bits": 56,
                    "component_count": 2,
                    "minimum_precision_bits": 30,
                    "slot_count": 32768,
                }
            )
            if ordinal == 0:
                record.update(
                    {
                        "bootstrap_reason": "pre_relu_refresh",
                        "bootstrap_mode": mode,
                    }
                )
            elif ordinal == 1:
                record["positive_bound_b"] = bound
            elif 2 <= ordinal <= 4:
                stage_index = ordinal - 2
                record.update(
                    {
                        "stage_ordinal": stage_index,
                        "degree": STAGE_DEGREES[stage_index],
                        "level_consumption": STAGE_CONSUMPTION[stage_index],
                        "coefficient_sha256": stage_hashes[stage_index],
                    }
                )
            else:
                record.update(
                    {
                        "level_consumption": 0,
                        "reconstruction": "0.5*x*sign(x/B)+0.5*x",
                    }
                )
            operations.append(record)

    _require(len(operations) == 19 * 6, "materialization must contain 114 rows")
    level_counts = {"15": 0, "17": 0, "18": 0}
    for record in operations:
        if record["operation_kind"] == "refresh":
            level_counts[str(record["output_level"])] += 1
    _require(level_counts == {"15": 16, "17": 1, "18": 2},
             "refresh level distribution is invalid")

    schedule = {
        "schema": "open64.fhe.sync4.relu-materialization-reference.v1",
        "status": "complete_reference_schedule",
        "bootstrap_mode": mode,
        "profile_name": PROFILE,
        "context_count": 19,
        "operation_count": len(operations),
        "source_manifests": {
            "ranges_sha256": _sha256(range_manifest_path),
            "ckks_schedule_sha256": _sha256(ckks_manifest_path),
            "coefficients_sha256": _sha256(coefficient_manifest_path),
            "capability_sha256": _sha256(capability_manifest_path),
        },
        "refresh_level_counts": level_counts,
        "operations": operations,
        "limitations": [
            "Reference algorithm only; native mapped-image persistence requires S4-1 main/common hooks.",
            "No runtime call, generated C, or ciphertext execution is represented.",
        ],
    }
    if mode == "manual":
        return _validate_existing_manual_schedule(existing_materialization, schedule)
    return schedule


def write_canonical(schedule: Dict[str, Any], output: Path) -> None:
    output.write_text(
        json.dumps(schedule, sort_keys=True, separators=(",", ":")) + "\n",
        encoding="utf-8",
    )


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--range-manifest", type=Path, required=True)
    parser.add_argument("--ckks-manifest", type=Path, required=True)
    parser.add_argument("--coefficient-manifest", type=Path, required=True)
    parser.add_argument("--capability-manifest", type=Path, required=True)
    parser.add_argument("--bootstrap", choices=("auto", "on", "manual", "off"),
                        default="auto")
    parser.add_argument("--existing-materialization", type=Path)
    parser.add_argument("--output", type=Path, required=True)
    arguments = parser.parse_args()
    existing = None
    if arguments.existing_materialization is not None:
        existing = json.loads(
            arguments.existing_materialization.read_text(encoding="utf-8")
        )
    schedule = build_schedule(
        arguments.range_manifest,
        arguments.ckks_manifest,
        arguments.coefficient_manifest,
        arguments.capability_manifest,
        arguments.bootstrap,
        existing,
    )
    arguments.output.parent.mkdir(parents=True, exist_ok=True)
    write_canonical(schedule, arguments.output)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
