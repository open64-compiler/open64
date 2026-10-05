#!/usr/bin/env python3
"""Certify the source-origin ABI template before CKKS PU specialization.

This is a read-only preflight, not proof that specialized CKKS PUs or their
terminal lowering exist. Design: doc/FHE-SYNC6-CKKS-ABI-LOWERING-BOUNDARY.md.
"""

import argparse
import json
import re
from collections import Counter, defaultdict
from pathlib import Path

from fhe_ckks_real_event_census import read_trace, require, sha256


ABI_SYMBOLS = {
    1: "open64_fhe_conv2d_plain_v1",
    2: "open64_fhe_residual_add_v1",
    3: "open64_fhe_bootstrap_v1",
    4: "open64_fhe_relu_normalize_v1",
    5: "open64_fhe_relu_poly_stage_v1",
    6: "open64_fhe_relu_reconstruct_v1",
    7: "open64_fhe_average_pool_v1",
    8: "open64_fhe_layout_convert_v1",
    9: "open64_fhe_linear_plain_v1",
}
EXPECTED_STATIC = (13, 5, 11, 11, 33, 11, 1, 1, 1)
EXPECTED_DYNAMIC = (21, 9, 19, 19, 57, 19, 1, 1, 1)
SOURCE_TO_ABI = {
    10: (1,),
    7: (2,),
    5: (3, 4, 5, 5, 5, 6),
    13: (7,),
    6: (8,),
    8: (9,),
}
SOURCE_TO_LOGICAL = {
    10: "OPR_DSLCONV2D",
    7: "OPR_DSLRESIDUALADD",
    5: "OPR_DSLRELU",
    13: "OPR_DSLGLOBALAVGPOOL2D",
    6: "OPR_DSLFLATTEN",
    8: "OPR_DSLLINEAR",
}
SELECT = re.compile(r"^SELECT (\d+) (\d+) ([1-9]) \d+$")
EVAL = re.compile(r"^EVAL (\d+) \d+ \d+ \d+$")
GENERATED_CALL = re.compile(r"\b(open64_fhe_[a-z0-9_]+_v1)\(")


def check_origin(events, signatures, bounds, schedule, selections, nodes):
    """Map every source event to one route/variant and one ABI descriptor kind."""
    require(schedule["static_evaluations"] == 87 and
            schedule["dynamic_evaluations"] == 147 and
            len(events) == 147 and len(selections) == 147,
            "source or reference ABI event total changed")
    source = {}
    expected_kind = {}
    for row in schedule["records"]:
        kinds = SOURCE_TO_ABI.get(row["operator"])
        node = nodes.get(row["node"])
        require(kinds is not None and len(kinds) == row["static_count"],
                "source operator has no approved ABI v1 expansion")
        require(node == (SOURCE_TO_LOGICAL[row["operator"]], row["value"]),
                "schedule operator disagrees with reopened DSL node")
        for offset, kind in enumerate(kinds):
            ordinal = row["first_ordinal"] + offset
            require(ordinal not in source, "duplicate static source ordinal")
            source[ordinal] = (row["owner"], row["value"])
            expected_kind[ordinal] = kind
    require(set(source) == set(range(1, 88)), "static source ordinal gap")

    routes = {}
    owner_variants = defaultdict(list)
    for owner in signatures["owners"]:
        owner_st = owner["source_owner_pu_st"]
        require(owner_st not in owner_variants,
                "duplicate source PU in specialization plan")
        for variant_index, variant in enumerate(owner["variants"]):
            for context in variant["contexts"]:
                key = (owner_st, context["callsite_id"])
                require(key not in routes, "call context assigned twice")
                routes[key] = (variant_index, context["instance_path"])
            owner_variants[owner_st].append(variant)
    require(len(owner_variants) == 6 and
            sum(map(len, owner_variants.values())) == 9 and
            len(routes) == 10,
            "source PU, variant, or invocation census changed")

    bound_routes = {}
    for variant in bounds["variants"]:
        owner_st = variant["source_owner_pu_st"]
        for context in variant["contexts"]:
            key = (owner_st, context["callsite_id"])
            require(key not in bound_routes and key in routes and
                    routes[key][1] == context["instance_path"],
                    "bound route is missing, duplicated, or renamed")
            require(variant["signature_sha256"] ==
                    owner_variants[owner_st][routes[key][0]]
                    ["relu_state_signature_sha256"],
                    "bound route has wrong specialization signature")
            bound_routes[key] = context["bound_actuals"]
    require(set(bound_routes) == set(routes) and
            sum(map(len, bound_routes.values())) == 19,
            "approved ReLU bounds do not cover all source routes")

    visits = Counter()
    origin_rows = []
    for event in events:
        ordinal = event["source_static_ordinal"]
        key = (event["owner_pu_st"], event["context_callsite_id"])
        require(ordinal in source and
                source[ordinal] == (event["owner_pu_st"],
                                    event["source_value_id"]) and
                key in routes,
                "event has no exact source definition or variant route")
        visits[ordinal] += 1
        origin_rows.append((key, ordinal))
    require(len(origin_rows) == len(set(origin_rows)),
            "source event repeats in one invocation")

    by_ordinal = defaultdict(list)
    for expected_sequence, (sequence, ordinal, kind) in enumerate(
            selections, start=1):
        require(sequence == expected_sequence and
                ordinal in source and kind in ABI_SYMBOLS,
                "reference ABI sequence or operation is invalid")
        by_ordinal[ordinal].append(kind)
    require(set(by_ordinal) == set(source) and
            all(len(by_ordinal[ordinal]) == visits[ordinal] and
                set(by_ordinal[ordinal]) == {expected_kind[ordinal]}
                for ordinal in source),
            "ABI visits do not match routed source semantics")
    static = Counter(kinds[0] for kinds in by_ordinal.values())
    dynamic = Counter(kind for _, _, kind in selections)
    require(tuple(static[k] for k in ABI_SYMBOLS) == EXPECTED_STATIC and
            tuple(dynamic[k] for k in ABI_SYMBOLS) == EXPECTED_DYNAMIC,
            "ABI v1 operation census changed")

    # A CKKS clone will have a new physical owner, but these origin keys may
    # not change: they identify the single six-PU ABI call surface.
    return {
        "source_pu_count": len(owner_variants),
        "relu_signature_variant_lower_bound": 9,
        "invocation_count": len(routes),
        "approved_relu_bound_count": 19,
        "source_static_event_count": len(source),
        "source_dynamic_event_count": len(origin_rows),
        "abi_static_by_kind": [static[k] for k in ABI_SYMBOLS],
        "abi_dynamic_by_kind": [dynamic[k] for k in ABI_SYMBOLS],
        "origin_routes": [
            {"owner_pu_st": owner, "callsite_id": callsite,
             "variant_index": variant, "instance_path": path,
             "static_ordinals": sorted(ordinal for key, ordinal in origin_rows
                                       if key == (owner, callsite))}
            for (owner, callsite), (variant, path) in sorted(routes.items())
        ],
    }


def read_reference(trace):
    """Validate the independent SYNC-5 dynamic selector/evaluation sequence."""
    selections = []
    pending = None
    for line in trace.read_text(encoding="utf-8").splitlines():
        selected = SELECT.fullmatch(line)
        evaluated = EVAL.fullmatch(line)
        if selected:
            require(pending is None, "selector without evaluation")
            pending = tuple(map(int, selected.groups()))
        elif evaluated:
            require(pending is not None and int(evaluated.group(1)) ==
                    pending[0], "evaluation without matching selector")
            selections.append(pending)
            pending = None
    require(pending is None and len(selections) == 147,
            "incomplete SYNC-5 evaluation trace")
    return selections


def check_generated_c(path):
    """Count only the nine successful evaluation symbols in generated C."""
    counts = Counter(GENERATED_CALL.findall(path.read_text(encoding="utf-8")))
    require(tuple(counts[symbol] for symbol in ABI_SYMBOLS.values()) ==
            EXPECTED_STATIC and
            sum(counts[symbol] for symbol in ABI_SYMBOLS.values()) == 87,
            "reference generated C is not the canonical 87-call surface")


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    for name in ("events", "signatures", "bounds", "schedule", "source-trace",
                 "generated-c", "runtime-trace", "output"):
        parser.add_argument("--" + name, type=Path, required=True)
    args = parser.parse_args()
    inputs = {name.replace("-", "_"): getattr(args, name.replace("-", "_"))
              for name in ("events", "signatures", "bounds", "schedule",
                           "source-trace", "generated-c", "runtime-trace")}
    events = json.loads(args.events.read_text(encoding="utf-8"))
    signatures = json.loads(args.signatures.read_text(encoding="utf-8"))
    bounds = json.loads(args.bounds.read_text(encoding="utf-8"))
    schedule = json.loads(args.schedule.read_text(encoding="utf-8"))
    source_hash = sha256(args.source_trace)
    require(events["trace_sha256"] == source_hash and
            signatures["trace_sha256"] == source_hash and
            bounds["trace_sha256"] == source_hash and
            signatures["event_census_sha256"] == sha256(args.events) and
            bounds["event_census_sha256"] == sha256(args.events) and
            bounds["context_signatures_sha256"] == sha256(args.signatures) and
            events["schedule_sha256"] == sha256(args.schedule),
            "input evidence does not share one authenticated source")
    selections = read_reference(args.runtime_trace)
    _, _, nodes = read_trace(args.source_trace)
    check_generated_c(args.generated_c)
    audit = check_origin(events["events"], signatures, bounds,
                         schedule, selections, nodes)

    # Exercise the two most dangerous silent failures without touching the
    # retained positive inputs: a duplicated route and a shifted ABI visit.
    malformed = json.loads(json.dumps(signatures))
    malformed["owners"][1]["variants"][1]["contexts"][0]["callsite_id"] = 1
    swapped_kinds = list(selections)
    first_conv = next(i for i, (_, _, kind) in enumerate(swapped_kinds)
                      if kind == 1)
    first_add = next(i for i, (_, _, kind) in enumerate(swapped_kinds)
                     if kind == 2)
    conv_row, add_row = swapped_kinds[first_conv], swapped_kinds[first_add]
    swapped_kinds[first_conv] = (conv_row[0], conv_row[1], 2)
    swapped_kinds[first_add] = (add_row[0], add_row[1], 1)
    malformed_nodes = dict(nodes)
    first_node = schedule["records"][0]["node"]
    malformed_nodes[first_node] = ("OPR_DSLRELU", nodes[first_node][1])
    for bad_events, bad_signatures, bad_selections, bad_nodes in (
            (events["events"], malformed, selections, nodes),
            (events["events"], signatures,
             [(seq, ordinal + (1 if seq == 1 else 0), kind)
              for seq, ordinal, kind in selections], nodes),
            (events["events"], signatures, swapped_kinds, nodes),
            (events["events"], signatures, selections, malformed_nodes)):
        try:
            check_origin(bad_events, bad_signatures, bounds, schedule,
                         bad_selections, bad_nodes)
        except ValueError:
            pass
        else:
            raise ValueError("malformed route or ABI visit was accepted")

    report = {
        "schema": "open64.fhe.sync6.abi-origin-template.v1",
        "status": "read_only_source_origin_template_not_terminal_lowering",
        "input_sha256": {name: sha256(path) for name, path in inputs.items()},
        **audit,
        "pending_proofs": ["reopened_ckks_ops_binary", "clone_value_origin_join",
                           "ckks_group_to_descriptor_join",
                           "six_pu_generated_c_collapse"],
    }
    args.output.parent.mkdir(parents=True, exist_ok=True)
    args.output.write_text(json.dumps(report, indent=2) + "\n",
                           encoding="utf-8")
    print("source ABI template: 6 PUs, >=9 CKKS variants, "
          "87 static, 147 dynamic")
    print(f"origin audit: {args.output}")


if __name__ == "__main__":
    main()
