#!/usr/bin/env python3
"""Certify the read-only SYNC-6 source-event census from retained SYNC-5 evidence.

The schedule is structured JSON. The call/PU and logical-node identities come
from the stable ir_b2a -st -src tables; this is an independent artifact audit,
not a substitute for the native mapped-image collector or CKKS expansion.
Design: doc/FHE-SYNC6-CKKS-IR-CONFORMANCE-GATE.md.
"""

import argparse
import hashlib
import json
import re
from collections import Counter, defaultdict
from pathlib import Path


IDENTITY = re.compile(r"^  \[(\d+)\] owner_pu=<1,(\d+)> definition=")
CALLSITE = re.compile(r"^  \[(\d+)\] owner_pu=<1,(\d+)> callee=<1,(\d+)>")
NODE = re.compile(r"^  \[(\d+)\] operator=(OPR_DSL[A-Z0-9]+) .* result=value(\d+)")
HEADINGS = {
    "DSL PU Source Identity Table:": "identity",
    "DSL Callsite Metadata Table:": "callsite",
    "DSL Node Table:": "node",
}


def global_st(index):
    # symtab_idx.h: make_ST_IDX(index, GLOBAL_SYMTAB=1).
    return (index << 8) | 1


def require(condition, message):
    if not condition:
        raise ValueError(message)


def sha256(path):
    digest = hashlib.sha256()
    with path.open("rb") as stream:
        for block in iter(lambda: stream.read(1024 * 1024), b""):
            digest.update(block)
    return digest.hexdigest()


def read_trace(path):
    identities, calls, nodes = {}, {}, {}
    section = None
    for line in path.read_text(encoding="utf-8").splitlines():
        if not line.startswith(" "):
            section = next((value for prefix, value in HEADINGS.items()
                            if line.startswith(prefix)), None)
            continue
        pattern = {"identity": IDENTITY, "callsite": CALLSITE,
                   "node": NODE}.get(section)
        match = pattern.match(line) if pattern else None
        if match is None:
            continue
        if section == "identity":
            row_id, symbol = map(int, match.groups())
            require(row_id not in identities and
                    global_st(symbol) not in identities.values(),
                    "duplicate PU source identity")
            identities[row_id] = global_st(symbol)
        elif section == "callsite":
            row_id, caller, callee = map(int, match.groups())
            require(row_id not in calls, "duplicate callsite")
            calls[row_id] = (global_st(caller), global_st(callee))
        else:
            row_id, operator, value = match.groups()
            row_id, value = int(row_id), int(value)
            require(row_id not in nodes, "duplicate DSL node")
            nodes[row_id] = (operator, value)
    require(bool(identities and nodes), "missing identity or logical-node table")
    require(sorted(identities) == list(range(1, len(identities) + 1)),
            "PU identity ID gap")
    require(sorted(calls) == list(range(1, len(calls) + 1)),
            "callsite ID gap")
    return identities, calls, nodes


def build_census(schedule, identities, calls, nodes):
    require(schedule["schema"] == "open64.fhe.sync5.runtime-schedule.v1",
            "unexpected schedule schema")
    owners = set(identities.values())
    identities_inv = {owner: row_id for row_id, owner in identities.items()}
    incoming = Counter()
    routes = defaultdict(list)
    for call_id, (caller, callee) in calls.items():
        require(caller in owners and callee in owners, "unknown call owner")
        incoming[callee] += 1
        routes[callee].append((identities_inv[callee], call_id))
    for owner in owners:
        if incoming[owner] == 0:
            routes[owner].insert(0, (identities_inv[owner], 0))

    # A direct callsite names one context. Recompute transitive multiplicity
    # so repeated execution through a nested caller cannot masquerade as one.
    remaining = incoming.copy()
    multiplicity = Counter({owner: 1 for owner in owners if remaining[owner] == 0})
    queue = [owner for owner in owners if remaining[owner] == 0]
    for owner in queue:
        for caller, callee in calls.values():
            if caller != owner:
                continue
            multiplicity[callee] += multiplicity[caller]
            remaining[callee] -= 1
            if remaining[callee] == 0:
                queue.append(callee)
    require(len(queue) == len(owners), "cyclic call graph")
    require(all(multiplicity[owner] == len(routes[owner]) for owner in owners),
            "nested call path is not representable by one direct callsite")
    records = schedule["records"]
    require(bool(records) and len(records) == len({row["node"] for row in records}),
            "empty or duplicated source definitions")
    ordinal = 1
    events = []
    relu_contexts = set()
    relu_nodes = set()
    for row in records:
        require(row["first_ordinal"] == ordinal, "static ordinal gap")
        count = row["static_count"]
        require(count > 0 and
                row["dynamic_count"] == count * row["multiplicity"],
                "invalid static/dynamic count")
        ordinal += count
        owner = row["owner"]
        require(owner in owners and
                row["multiplicity"] == multiplicity[owner],
                "call context does not match source multiplicity")
        operator, value = nodes[row["node"]]
        require(value == row["value"], "schedule/DSL result mismatch")
        for identity, call_id in routes[owner]:
            for source_ordinal in range(row["first_ordinal"], ordinal):
                events.append((owner, value, identity, call_id, source_ordinal))
            if operator == "OPR_DSLRELU":
                require(count == 6, "ReLU must have six static events")
                relu_nodes.add(row["node"])
                relu_contexts.add((owner, value, identity, call_id))
    require(ordinal - 1 == schedule["static_evaluations"],
            "static total mismatch")
    require(len(events) == schedule["dynamic_evaluations"],
            "dynamic total mismatch")
    require(len(events) == len(set(events)), "duplicate event identity")
    return events, len(relu_nodes), len(relu_contexts)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--schedule", type=Path, required=True)
    parser.add_argument("--trace", type=Path, required=True)
    parser.add_argument("--output", type=Path, required=True)
    args = parser.parse_args()
    schedule = json.loads(args.schedule.read_text(encoding="utf-8"))
    identities, calls, nodes = read_trace(args.trace)
    events, relu_definitions, relu_contexts = build_census(
        schedule, identities, calls, nodes)
    require((len(identities), len(calls), len(schedule["records"]),
             len(events), relu_definitions, relu_contexts) ==
            (6, 9, 32, 147, 11, 19), "unexpected SecureResNet census")

    # Exercise rejection without modifying either retained source artifact.
    malformed = json.loads(json.dumps(schedule))
    malformed["records"][1]["multiplicity"] += 1
    try:
        build_census(malformed, identities, calls, nodes)
    except ValueError:
        pass
    else:
        raise ValueError("wrong call multiplicity was accepted")
    malformed = json.loads(json.dumps(schedule))
    malformed["records"][1]["first_ordinal"] += 1
    try:
        build_census(malformed, identities, calls, nodes)
    except ValueError:
        pass
    else:
        raise ValueError("skipped source ordinal was accepted")
    malformed_calls = dict(calls)
    call_id = next(row_id for row_id, (_, callee) in calls.items()
                   if callee != identities[2])
    malformed_calls[call_id] = (identities[2], calls[call_id][1])
    try:
        build_census(schedule, identities, malformed_calls, nodes)
    except ValueError:
        pass
    else:
        raise ValueError("unrepresentable nested call path was accepted")
    malformed_nodes = dict(nodes)
    node_id = schedule["records"][0]["node"]
    operator, value = malformed_nodes[node_id]
    malformed_nodes[node_id] = (operator, value + 1)
    try:
        build_census(schedule, identities, calls, malformed_nodes)
    except ValueError:
        pass
    else:
        raise ValueError("schedule/DSL value mismatch was accepted")

    report = {
        "schema": "open64.fhe.sync6.source-event-census.v1",
        "status": "read_only_preflight_not_executable_ckks_ir",
        "schedule_sha256": sha256(args.schedule),
        "trace_sha256": sha256(args.trace),
        "pu_count": len(identities),
        "callsite_count": len(calls),
        "definition_count": len(schedule["records"]),
        "static_event_count": schedule["static_evaluations"],
        "dynamic_event_count": len(events),
        "relu_definition_count": relu_definitions,
        "relu_context_count": relu_contexts,
        "events": [dict(owner_pu_st=owner, source_value_id=value,
                        context_pu_identity_id=identity,
                        context_callsite_id=call_id,
                        source_static_ordinal=ordinal)
                   for owner, value, identity, call_id, ordinal in events],
    }
    args.output.parent.mkdir(parents=True, exist_ok=True)
    args.output.write_text(json.dumps(report, indent=2) + "\n", encoding="utf-8")
    print("source definitions=32 static=87 dynamic=147 relu definitions=11 contexts=19")
    print(f"event census: {args.output}")


if __name__ == "__main__":
    main()
