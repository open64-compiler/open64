#!/usr/bin/env python3
"""Audit typed-B formal/actual requirements from certified ResNet inspection.

This is read-only evidence for doc/FHE-SYNC6-CONTEXT-SPECIALIZATION-CONTRACT.md,
not a WHIRL clone or call-ABI rewrite.
"""

import argparse
import json
import re
from collections import defaultdict
from pathlib import Path

from fhe_ckks_real_event_census import read_trace, require, sha256
from fhe_ckks_context_signature_audit import source_names_and_calls


def bound_rows(path, names):
    """Read exact range identities and bound TCONs from the retained .T."""
    owners = {name: owner for owner, name in names.items()}
    rows = {}
    in_ranges = False
    for line in path.read_text(encoding="utf-8").splitlines():
        if line.startswith("FHE ReLU Context Range Table:"):
            in_ranges = True
            continue
        if in_ranges and not line.startswith(" "):
            break
        if not in_ranges or not line.startswith("  ["):
            continue
        fields = re.search(
            r"source_relu=value(\d+)\([^)]*\) profile=(\d+)", line)
        if fields is None:
            fields = re.search(
                r"profile=(\d+) source_relu=value(\d+)\([^)]*\)", line)
            require(fields is not None, "malformed range source/profile")
            profile, source = map(int, fields.groups())
        else:
            source, profile = map(int, fields.groups())
        owner_name = re.search(r"\bowner_pu=([^ ]+)", line)
        identity = re.search(r"\bcontext_identity=(\d+)", line)
        callsite = re.search(r"\bcallsite=(\d+)", line)
        bound = re.search(r"\bpositive_bound=tcon(\d+)", line)
        instance = re.search(r"\binstance_path=([^ ;]+)", line)
        require(all((owner_name, identity, callsite, bound, instance)),
                "incomplete bound range row")
        require(owner_name.group(1) in owners, "unknown range owner")
        key = (owners[owner_name.group(1)], source,
               int(identity.group(1)), int(callsite.group(1)))
        require(key not in rows, "duplicate bound context identity")
        require(profile > 0 and int(bound.group(1)) > 0,
                "invalid bound profile or TCON ID")
        rows[key] = {
            "profile_id": profile,
            "bound_tcon": int(bound.group(1)),
            "instance_path": instance.group(1),
        }
    require(len(rows) == 19, "expected exactly 19 bound range rows")
    return rows


def call_abi_inputs(path, names, lines=None):
    """Resolve dense input ordinals and the bound insertion slot per call."""
    inputs = defaultdict(list)
    in_abi = False
    if lines is None:
        lines = path.read_text(encoding="utf-8").splitlines()
    for line in lines:
        if line.startswith("DSL Call ABI Argument Table:"):
            in_abi = True
            continue
        if in_abi and not line.startswith(" "):
            break
        if not in_abi or not line.startswith("  ["):
            continue
        callsite_field = re.search(r"\bcallsite=(\d+)", line)
        callee = re.search(r"\bcallee=<1,(\d+)>", line)
        actual = re.search(r"\bactual=(\d+)", line)
        formal = re.search(r"\bformal=(\d+)", line)
        require(all((callsite_field, callee, actual, formal)),
                "incomplete call ABI row")
        callsite = int(callsite_field.group(1))
        owner = (int(callee.group(1)) << 8) | 1
        require(owner in names and int(actual.group(1)) == int(formal.group(1)),
                "call ABI callee or ordinal mismatch")
        inputs[callsite].append((owner, int(actual.group(1))))
    require(len(inputs) == 9, "expected nine block call ABI groups")
    result = {}
    for callsite, rows in inputs.items():
        owners = {owner for owner, _ in rows}
        ordinals = sorted(ordinal for _, ordinal in rows)
        require(len(owners) == 1 and ordinals == list(range(len(rows))),
                "call ABI input ordinals are not dense")
        result[callsite] = (next(iter(owners)), len(rows))
    return result


def interface_plan(events, relu_values, signatures, ranges, instances,
                   call_inputs):
    """Join each call signature to its two ordered ReLU bound actuals."""
    source_order = {}
    for event in events:
        if event["source_value_id"] not in relu_values:
            continue
        key = (event["owner_pu_st"], event["source_value_id"],
               event["context_pu_identity_id"],
               event["context_callsite_id"])
        ordinal = event["source_static_ordinal"]
        source_order[key] = min(source_order.get(key, ordinal), ordinal)
    require(set(source_order) == set(ranges),
            "bound rows and source ReLU contexts differ")
    by_callsite = defaultdict(list)
    for key, row in ranges.items():
        by_callsite[(key[0], key[3])].append((source_order[key], key, row))
    require(len(by_callsite) == 10, "expected root plus nine block calls")
    require(sum(len(rows) for rows in by_callsite.values()) == 19,
            "bound actual coverage is incomplete")

    result, covered = [], set()
    for owner in signatures["owners"]:
        owner_st = owner["source_owner_pu_st"]
        for variant in owner["variants"]:
            contexts = []
            for context in variant["contexts"]:
                callsite = context["callsite_id"]
                pair = (owner_st, callsite)
                require(pair in by_callsite and pair not in covered,
                        "missing or duplicate signature callsite")
                covered.add(pair)
                expected_path = ("stem" if callsite == 0 else
                                 instances.get(callsite))
                require(expected_path == context["instance_path"],
                        "callsite instance path mismatch")
                ordered = sorted(by_callsite[pair])
                require(len(ordered) == (1 if callsite == 0 else 2),
                        "wrong per-callsite ReLU bound count")
                if callsite != 0:
                    require(call_inputs.get(callsite, (None, None))[0] ==
                            owner_st, "call ABI does not target range owner")
                    insertion = call_inputs[callsite][1]
                else:
                    insertion = None
                actuals = []
                for bound_slot, (_, key, row) in enumerate(ordered):
                    require(row["instance_path"].startswith(expected_path),
                            "range instance path mismatch")
                    actuals.append({
                        "formal_role": f"relu_bound_{bound_slot}",
                        "bound_slot": bound_slot,
                        "callee_formal_ordinal": (
                            None if insertion is None else
                            insertion + bound_slot),
                        "source_relu_value_id": key[1],
                        "context_pu_identity_id": key[2],
                        "bound_tcon": row["bound_tcon"],
                        "profile_id": row["profile_id"],
                    })
                contexts.append({"callsite_id": callsite,
                                 "instance_path": expected_path,
                                 "existing_input_count": insertion,
                                 "bound_actuals": actuals})
            result.append({
                "source_owner_pu_st": owner_st,
                "signature_sha256": variant["relu_state_signature_sha256"],
                "required_bound_formal_count":
                    0 if contexts[0]["callsite_id"] == 0 else 2,
                "contexts": contexts,
            })
    require(covered == set(by_callsite), "unassigned bound callsite")
    require(len(result) == 9, "unexpected signature variant count")
    return result


def main():
    """Produce a hashed, read-only bound interface proposal and negatives."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--events", type=Path, required=True)
    parser.add_argument("--signatures", type=Path, required=True)
    parser.add_argument("--trace", type=Path, required=True)
    parser.add_argument("--output", type=Path, required=True)
    args = parser.parse_args()
    census = json.loads(args.events.read_text(encoding="utf-8"))
    signatures = json.loads(args.signatures.read_text(encoding="utf-8"))
    require(census["trace_sha256"] == sha256(args.trace) and
            signatures["trace_sha256"] == sha256(args.trace) and
            signatures["event_census_sha256"] == sha256(args.events),
            "bound audit inputs are not bound to one trace")
    names, instances = source_names_and_calls(args.trace)
    call_inputs = call_abi_inputs(args.trace, names)
    malformed_lines = args.trace.read_text(encoding="utf-8").splitlines()
    first_abi = next(i for i, line in enumerate(malformed_lines)
                     if line.startswith("DSL Call ABI Argument Table:")) + 1
    malformed_lines[first_abi] = malformed_lines[first_abi].replace(
        "actual=0 formal=0", "actual=1 formal=0", 1)
    try:
        call_abi_inputs(args.trace, names, malformed_lines)
    except ValueError:
        pass
    else:
        raise ValueError("nondense call ABI row was accepted")
    _, _, nodes = read_trace(args.trace)
    relu_values = {value for operator, value in nodes.values()
                   if operator == "OPR_DSLRELU"}
    ranges = bound_rows(args.trace, names)
    variants = interface_plan(census["events"], relu_values,
                              signatures, ranges, instances, call_inputs)
    malformed = dict(ranges)
    malformed.pop(next(iter(malformed)))
    try:
        interface_plan(census["events"], relu_values,
                       signatures, malformed, instances, call_inputs)
    except ValueError:
        pass
    else:
        raise ValueError("missing bound context was accepted")
    malformed = dict(ranges)
    key = next(iter(malformed))
    malformed[key] = dict(malformed[key], instance_path="wrong.path")
    try:
        interface_plan(census["events"], relu_values,
                       signatures, malformed, instances, call_inputs)
    except ValueError:
        pass
    else:
        raise ValueError("wrong bound instance path was accepted")
    malformed_inputs = dict(call_inputs)
    first_callsite = min(malformed_inputs)
    owner, count = malformed_inputs[first_callsite]
    malformed_inputs[first_callsite] = (owner + 256, count)
    try:
        interface_plan(census["events"], relu_values,
                       signatures, ranges, instances, malformed_inputs)
    except ValueError:
        pass
    else:
        raise ValueError("wrong call ABI callee was accepted")
    report = {
        "schema": "open64.fhe.sync6.bound-interface-audit.v1",
        "status": "read_only_preflight_not_native_call_abi",
        "trace_sha256": sha256(args.trace),
        "event_census_sha256": sha256(args.events),
        "context_signatures_sha256": sha256(args.signatures),
        "context_count": 19,
        "source_callsite_count": 9,
        "specialized_variant_count": len(variants),
        "called_bound_actual_count": 18,
        "entry_bound_constant_count": 1,
        "bound_formal_ty": "MTYPE_F8_by_value",
        "variants": variants,
        "negative_checks": ["missing_context", "instance_path_mismatch",
                            "call_abi_callee_mismatch", "call_abi_ordinal"],
    }
    args.output.parent.mkdir(parents=True, exist_ok=True)
    args.output.write_text(json.dumps(report, indent=2) + "\n",
                           encoding="utf-8")
    print("Bound interface: 19 contexts, 18 call actuals, 1 entry constant")
    print(f"Bound interface audit: {args.output}")


if __name__ == "__main__":
    main()
