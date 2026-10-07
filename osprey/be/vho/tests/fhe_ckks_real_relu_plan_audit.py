#!/usr/bin/env python3
"""Audit real ResNet ReLU event/materialization/state joins in ir_b2a output.

This checks retained inspection evidence, not executable CKKS WHIRL.
Design: doc/FHE-SYNC6-CKKS-IR-CONFORMANCE-GATE.md.
"""

import argparse
import json
import re
from collections import Counter, defaultdict
from pathlib import Path

from fhe_ckks_real_event_census import read_trace, require, sha256


SECTIONS = {
    "FHE ReLU Context Range Table:": "range",
    "FHE Context CKKS State Table:": "state",
    "FHE ReLU Context Operation Table:": "operation",
}
EXPECTED_KINDS = (
    "refresh", "normalize", "approx_stage", "approx_stage",
    "approx_stage", "reconstruct_relu",
)


def read_stage_depths(path):
    """Read the accepted profile's ordered level costs independently."""
    depths = {}
    in_stages = False
    for line in path.read_text(encoding="utf-8").splitlines():
        if line.startswith("FHE Ordered Approximation Stage Table:"):
            in_stages = True
            continue
        if in_stages and not line.startswith(" "):
            break
        if not in_stages or not line.startswith("  ["):
            continue
        ordinal = int(field(line, "ordinal", r"\d+"))
        require(ordinal not in depths and
                int(field(line, "profile", r"\d+")) == 1,
                "ambiguous approximation stage")
        require(int(field(line, "degree", r"\d+")) ==
                {0: 7, 1: 15, 2: 13}.get(ordinal) and
                field(line, "basis") == "chebyshev" and
                field(line, "evaluation") == "addition_chain" and
                field(line, "input_class") == "ciphertext" and
                field(line, "output_scale") == "preserve_input" and
                field(line, "output_components") == "relinearized_two" and
                int(field(line, "minimum_precision", r"\d+")) == 30,
                "stage is not the approved first-release ACE contract")
        depths[ordinal] = int(field(line, "level_consumption", r"\d+"))
    require(depths == {0: 3, 1: 4, 2: 4},
            "unexpected approved degree-7/15/13 depth contract")
    return depths


def field(line, name, pattern=r"[^ ]+"):
    match = re.search(r"\b" + re.escape(name) + r"=(" + pattern + r")", line)
    require(match is not None, f"missing {name} field")
    return match.group(1)


def read_plan_tables(path):
    ranges, states, operations = {}, {}, {}
    owners = {}
    section = None
    for line in path.read_text(encoding="utf-8").splitlines():
        owner = re.match(r"^FUNC_ENTRY <1,(\d+),([^>]+)>", line)
        if owner:
            owners[owner.group(2)] = (int(owner.group(1)) << 8) | 1
        if not line.startswith(" "):
            section = next((value for prefix, value in SECTIONS.items()
                            if line.startswith(prefix)), None)
            continue
        match = re.match(r"^  \[(\d+)\] ", line)
        if match is None or section is None:
            continue
        row_id = int(match.group(1))
        owner_name = field(line, "owner_pu")
        require(owner_name in owners, "plan row has unknown PU")
        source_field = "source_relu" if section == "range" else "source"
        key = (owners[owner_name],
               int(field(line, source_field, r"value\d+")[5:]),
               int(field(line, "context_identity", r"\d+")),
               int(field(line, "callsite", r"\d+")))
        if section == "range":
            require(row_id not in ranges, "duplicate range row")
            ranges[row_id] = (
                key, int(field(line, "profile", r"\d+")),
                int(field(line, "positive_bound", r"tcon\d+")[4:]))
        elif section == "state":
            require(row_id not in states, "duplicate state row")
            states[row_id] = {
                "key": key,
                "role": field(line, "role"),
                "level": int(field(line, "level", r"-?\d+")),
                "scale_bits": int(field(line, "scale_bits", r"-?\d+")),
                "components": int(field(line, "components", r"\d+")),
                "precision_bits": int(field(line, "precision_bits", r"-?\d+")),
                "slots": int(field(line, "slots", r"\d+")),
                "layout": field(line, "layout"),
                "encryption": int(field(line, "encryption", r"\d+")),
                "scheme": field(line, "scheme"),
                "value_class": field(line, "class"),
                "pending": int(field(line, "pending", r"0x[0-9a-fA-F]+"), 16),
                "reason": field(line, "bootstrap_reason"),
            }
        else:
            ordinal = int(field(line, "ordinal", r"\d+"))
            composite_key = key + (ordinal,)
            require(composite_key not in operations,
                    "duplicate context operation")
            operations[composite_key] = {
                "id": row_id,
                "kind": field(line, "kind"),
                "profile": int(field(line, "profile", r"\d+")),
                "stage": int(field(line, "stage", r"\d+")),
                "range": int(field(line, "range", r"\d+")),
                "input": int(field(line, "input_state", r"\d+")),
                "output": int(field(line, "output_state", r"\d+")),
                "parameter": int(field(line, "parameter_tcon", r"\d+")),
            }
    require(len(owners) == 6 and len(ranges) == 19 and
            len(states) == 114 and len(operations) == 114,
            "incomplete six-PU ReLU inspection tables")
    return ranges, states, operations


def audit(events, relu_values, ranges, states, operations, stage_depths):
    contexts = defaultdict(list)
    for event in events:
        if event["source_value_id"] in relu_values:
            key = (event["owner_pu_st"], event["source_value_id"],
                   event["context_pu_identity_id"],
                   event["context_callsite_id"])
            contexts[key].append(event["source_static_ordinal"])
    require(len(contexts) == 19, "missing or extra ReLU context")
    require(sum(map(len, contexts.values())) == 114,
            "ReLU event count is not 19 x 6")
    require(len(operations) == 114, "extra materialization operation")
    levels = Counter()
    for key, ordinals in contexts.items():
        ordinals.sort()
        require(ordinals == list(range(ordinals[0], ordinals[0] + 6)),
                "ReLU source ordinals are not consecutive")
        previous = 0
        range_id = None
        for ordinal, source_ordinal in enumerate(ordinals):
            require(source_ordinal == ordinals[0] + ordinal,
                    "source/materialization ordinal mismatch")
            row = operations.get(key + (ordinal,))
            require(row is not None and row["kind"] == EXPECTED_KINDS[ordinal],
                    "missing or reordered materialization operation")
            require(row["range"] in ranges and
                    ranges[row["range"]][:2] == (key, row["profile"]),
                    "materialization range identity mismatch")
            require(ranges[row["range"]][2] > 0,
                    "normalization bound is absent")
            require(row["output"] in states and
                    states[row["output"]]["key"] == key,
                    "materialization state identity mismatch")
            require(row["input"] == previous, "state chain is discontinuous")
            require(range_id is None or row["range"] == range_id,
                    "context range changed mid-chain")
            range_id = row["range"]
            previous = row["output"]
            state = states[previous]
            role = "post_refresh" if ordinal == 0 else (
                "result" if ordinal == 5 else "post_operation")
            require(state["role"] == role, "wrong context-state role")
            require(state["scale_bits"] == 56 and
                    state["components"] == 2 and
                    state["precision_bits"] >= 30 and
                    state["slots"] == 32768 and
                    state["layout"] == "ckks.packed",
                    "invalid ACE CKKS state payload")
            if ordinal > 0:
                prior = states[row["input"]]
                cost = stage_depths[ordinal - 2] if 2 <= ordinal <= 4 else 0
                require(prior["level"] - state["level"] == cost and
                        prior["encryption"] == state["encryption"],
                        "stage level or encryption transfer disagrees")
            require(row["stage"] ==
                    (ordinal - 1 if 2 <= ordinal <= 4 else 0),
                    "wrong approximation stage")
            require((row["parameter"] != 0) == (1 <= ordinal <= 4),
                    "wrong stage/normalization parameter")
            if ordinal == 1:
                require(row["parameter"] == ranges[row["range"]][2],
                        "normalization parameter is not the context bound")
            if ordinal == 0:
                require(state["pending"] & 4 and
                        state["reason"] == "pre_relu_refresh",
                        "refresh state is not a planned bootstrap")
                levels[state["level"]] += 1
            else:
                require(state["pending"] == 0,
                        "post-refresh operation remains pending")
    return levels


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--events", type=Path, required=True)
    parser.add_argument("--trace", type=Path, required=True)
    parser.add_argument("--output", type=Path, required=True)
    args = parser.parse_args()
    census = json.loads(args.events.read_text(encoding="utf-8"))
    require(census["schema"] == "open64.fhe.sync6.source-event-census.v1" and
            census["trace_sha256"] == sha256(args.trace),
            "event census is not bound to this trace")
    _, _, nodes = read_trace(args.trace)
    relu_values = {value for operator, value in nodes.values()
                   if operator == "OPR_DSLRELU"}
    ranges, states, operations = read_plan_tables(args.trace)
    stage_depths = read_stage_depths(args.trace)
    levels = audit(census["events"], relu_values,
                   ranges, states, operations, stage_depths)
    require(set(levels) == {15, 17, 18} and sum(levels.values()) == 19,
            "unexpected post-refresh target levels")

    malformed = dict(operations)
    key = next(iter(malformed))
    malformed[key] = dict(malformed[key], input=999)
    try:
        audit(census["events"], relu_values, ranges, states, malformed,
              stage_depths)
    except ValueError:
        pass
    else:
        raise ValueError("broken state chain was accepted")
    malformed = dict(ranges)
    row_id = next(iter(malformed))
    old_key, profile, bound = malformed[row_id]
    malformed[row_id] = (old_key[:3] + (99,), profile, bound)
    try:
        audit(census["events"], relu_values, malformed, states, operations,
              stage_depths)
    except ValueError:
        pass
    else:
        raise ValueError("wrong range context was accepted")
    malformed = dict(ranges)
    malformed[row_id] = (old_key, profile, bound + 1)
    try:
        audit(census["events"], relu_values, malformed, states, operations,
              stage_depths)
    except ValueError:
        pass
    else:
        raise ValueError("wrong normalization bound was accepted")
    malformed = {row_id: dict(state) for row_id, state in states.items()}
    stage_output = next(row["output"] for key, row in operations.items()
                        if key[4] == 3)
    malformed[stage_output]["level"] += 1
    try:
        audit(census["events"], relu_values, ranges, malformed, operations,
              stage_depths)
    except ValueError:
        pass
    else:
        raise ValueError("wrong stage level was accepted")
    malformed_depths = dict(stage_depths)
    malformed_depths[1] = 3
    try:
        audit(census["events"], relu_values, ranges, states, operations,
              malformed_depths)
    except ValueError:
        pass
    else:
        raise ValueError("wrong stage depth was accepted")

    report = {
        "schema": "open64.fhe.sync6.relu-plan-audit.v1",
        "status": "read_only_preflight_not_executable_ckks_ir",
        "event_census_sha256": sha256(args.events),
        "trace_sha256": sha256(args.trace),
        "relu_contexts": 19,
        "materialization_operations": 114,
        "post_refresh_target_levels": dict(sorted(levels.items())),
        "ordered_stage_level_consumption": stage_depths,
        "negative_checks": ["state_chain", "range_context",
                            "normalization_bound",
                            "stage_output_level", "stage_depth"],
    }
    args.output.parent.mkdir(parents=True, exist_ok=True)
    args.output.write_text(json.dumps(report, indent=2) + "\n", encoding="utf-8")
    print(f"ReLU contexts=19 materialization operations=114 levels={dict(levels)}")
    print(f"ReLU plan audit: {args.output}")


if __name__ == "__main__":
    main()
