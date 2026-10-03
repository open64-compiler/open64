#!/usr/bin/env python3
"""Derive exact CKKS PU specialization signatures from certified ResNet rows.

This is a read-only architecture check, not a PU cloning implementation.
Design: doc/FHE-SYNC6-CKKS-IR-CONFORMANCE-GATE.md.
"""

import argparse
import hashlib
import json
import re
from collections import defaultdict
from pathlib import Path

from fhe_ckks_real_event_census import require, sha256
from fhe_ckks_real_relu_plan_audit import read_plan_tables


def source_names_and_calls(path):
    names, instances = {}, {}
    in_call_table = False
    for line in path.read_text(encoding="utf-8").splitlines():
        entry = re.match(r"^FUNC_ENTRY <1,(\d+),([^>]+)>", line)
        if entry:
            names[(int(entry.group(1)) << 8) | 1] = entry.group(2)
        if line.startswith("DSL Callsite Metadata Table:"):
            in_call_table = True
            continue
        if in_call_table and not line.startswith(" "):
            in_call_table = False
        if in_call_table:
            call = re.match(r"^  \[(\d+)\].*\binstance=([^ ]+)", line)
            if call:
                instances[int(call.group(1))] = call.group(2)
    return names, instances


def specialization_census(events, states, operations, names, instances):
    source_order = {}
    for event in events:
        key = (event["owner_pu_st"], event["source_value_id"],
               event["context_pu_identity_id"],
               event["context_callsite_id"])
        ordinal = event["source_static_ordinal"]
        source_order[key] = min(source_order.get(key, ordinal), ordinal)

    contexts = defaultdict(list)
    for state in states.values():
        if state["role"] != "post_refresh":
            continue
        key = state["key"]
        require(key in source_order, "refresh state has no source event")
        chain = []
        for ordinal in range(6):
            operation = operations.get(key + (ordinal,))
            require(operation is not None and operation["output"] in states,
                    "incomplete ReLU executable-state signature")
            result = states[operation["output"]]
            require(result["key"] == key, "cross-context state in signature")
            chain.append((
                operation["kind"], operation["profile"],
                operation["stage"],
                operation["parameter"] if 2 <= ordinal <= 4 else 0,
                result["role"], result["encryption"], result["scheme"],
                result["value_class"], result["level"],
                result["scale_bits"], result["components"],
                result["precision_bits"], result["slots"],
                result["layout"], result["pending"], result["reason"],
            ))
        contexts[(key[0], key[3])].append((
            source_order[key], state["level"], tuple(chain)))
    require(sum(map(len, contexts.values())) == 19,
            "unexpected number of post-refresh contexts")

    owners = defaultdict(lambda: defaultdict(list))
    for (owner, callsite), ordered in contexts.items():
        require(owner in names, "unknown source PU")
        require(callsite == 0 or callsite in instances,
                "called context lacks instance path")
        ordered.sort()
        levels = tuple(level for _, level, _ in ordered)
        signature = tuple(chain for _, _, chain in ordered)
        owners[owner][signature].append({
            "callsite_id": callsite,
            "instance_path": "stem" if callsite == 0 else instances[callsite],
            "ordered_post_refresh_levels": list(levels),
        })
    source_pus = len(names)
    specialized_pus = sum(map(len, owners.values()))
    require(source_pus == 6 and specialized_pus == 9,
            "unexpected pinned-model specialization census")
    return {
        "source_pu_count": source_pus,
        "minimum_fixed_schedule_pu_count": specialized_pus,
        "additional_context_clones": specialized_pus - source_pus,
        "owners": [
            {
                "source_owner_pu_st": owner,
                "source_pu_name": names[owner],
                "variants": [
                    {
                        "relu_state_signature_sha256": hashlib.sha256(
                            json.dumps(signature, separators=(",", ":"))
                            .encode("utf-8")).hexdigest(),
                        "ordered_post_refresh_levels":
                            rows[0]["ordered_post_refresh_levels"],
                        "contexts": [
                            {key: value for key, value in row.items()
                             if key != "ordered_post_refresh_levels"}
                            for row in sorted(
                                rows, key=lambda row: row["callsite_id"])
                        ],
                    }
                    for signature, rows in sorted(variants.items())
                ],
            }
            for owner, variants in sorted(owners.items())
        ],
    }


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--events", type=Path, required=True)
    parser.add_argument("--trace", type=Path, required=True)
    parser.add_argument("--output", type=Path, required=True)
    args = parser.parse_args()
    census = json.loads(args.events.read_text(encoding="utf-8"))
    require(census["schema"] == "open64.fhe.sync6.source-event-census.v1" and
            census["trace_sha256"] == sha256(args.trace),
            "event census is not bound to the trace")
    names, instances = source_names_and_calls(args.trace)
    _, states, operations = read_plan_tables(args.trace)
    report = specialization_census(
        census["events"], states, operations, names, instances)
    malformed = {row_id: dict(state) for row_id, state in states.items()}
    altered = next(row_id for row_id, state in malformed.items()
                   if state["role"] == "post_refresh" and
                   state["key"][3] == 2)
    malformed[altered]["level"] = 16
    try:
        specialization_census(
            census["events"], malformed, operations, names, instances)
    except ValueError:
        pass
    else:
        raise ValueError("unreviewed CKKS context signature was accepted")
    report.update({
        "schema": "open64.fhe.sync6.context-signature-audit.v1",
        "status": "read_only_specialization_preflight",
        "event_census_sha256": sha256(args.events),
        "trace_sha256": sha256(args.trace),
        "bound_policy": "context_B_requires_typed_plaintext_formal_or_more_clones",
        "dynamic_level_abi": "not_approved",
        "negative_checks": ["unreviewed_context_signature"],
    })
    args.output.parent.mkdir(parents=True, exist_ok=True)
    args.output.write_text(json.dumps(report, indent=2) + "\n", encoding="utf-8")
    print("source PUs=6 fixed-schedule CKKS PUs=9 additional clones=3")
    print(f"signature audit: {args.output}")


if __name__ == "__main__":
    main()
