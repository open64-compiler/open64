"""Audit native R1 verification events, not optimization or execution claims.

Design: doc/AI-COMPILER-NATIVE-REDUCTION-CONTRACT-v0.1.md.
"""
import argparse
from pathlib import Path

HEADER = "AI_REDUCE 1 HEADER phase node value rows width axis keepdim accum_dtype reassociate status source_line"
FACT_COLUMNS = ("phase pu node input result input_ty result_ty rows width state identity update finalize "
                "real_merge merge_permission order reassociate reformulate finite overflow accuracy "
                "input_access output_access input_effect alias output_owner storage source_line").split()
FACT_HEADER = "AI_REDUCE_FACT 1 HEADER " + " ".join(FACT_COLUMNS)


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


def audit_characterization(text, require=False):
    """Check native characterization against FORM events and entry/exit equality.

    Legacy traces remain readable, but absence is reported, never synthesized
    into a new claim of WHIRL-derived state/access/numerical characterization.
    """
    audit(text)
    forms = [row.split() for row in text.splitlines() if row.startswith("AI_REDUCE 1 FORM ")]
    facts = []
    header = False
    fixed = dict(state="sum_f32_per_row", identity="0", update="fp32_add", finalize="identity",
                 real_merge="real_add", merge_permission="prohibited", order="source_unresolved",
                 reassociate="0", reformulate="not_granted", finite="unproved", overflow="unproved",
                 accuracy="unproved", input_access="X[r,k]", output_access="Y[r]",
                 input_effect="read_only", alias="may_alias", output_owner="per_row", storage="unplanned")
    for line in text.splitlines():
        if not line.startswith("AI_REDUCE_FACT "):
            continue
        if line == FACT_HEADER:
            header = True
            continue
        row = line.split()
        if not header or row[:3] != ["AI_REDUCE_FACT", "1", "FACT"] or len(row) != len(FACT_COLUMNS) + 3:
            raise ValueError("malformed native characterization")
        fact = dict(zip(FACT_COLUMNS, row[3:]))
        for name in ("pu", "node", "input", "result", "input_ty", "result_ty", "rows", "width", "source_line"):
            if int(fact[name]) <= 0:
                raise ValueError("invalid native characterization identity/domain")
        if (any(fact[name] != value for name, value in fixed.items()) or
                fact["input"] == fact["result"] or fact["input_ty"] == fact["result_ty"]):
            raise ValueError("unsupported numerical/access/state characterization")
        facts.append(fact)
    if not facts and not header and not require:
        return []
    if len(facts) != len(forms) or not facts:
        raise ValueError("incomplete native characterization")
    pending = {}
    completed = []
    for form, fact in zip(forms, facts):
        if [fact[name] for name in ("phase", "node", "result", "rows", "width", "source_line")] != [form[i] for i in (3, 4, 5, 6, 7, 13)]:
            raise ValueError("characterization disagrees with canonical FORM")
        key = (fact["node"], fact["result"])
        canonical = {name: value for name, value in fact.items() if name != "phase"}
        if fact["phase"] == "preopt_enter":
            if key in pending:
                raise ValueError("duplicate characterization entry")
            pending[key] = canonical
        elif fact["phase"] == "preopt_exit":
            if pending.pop(key, None) != canonical:
                raise ValueError("PREOPT changed reduction characterization")
            completed.append(canonical)
        else:
            raise ValueError("unknown characterization phase")
    if pending:
        raise ValueError("missing characterization exit")
    return completed


def write_reports(trace, output, require_characterization=False):
    """Produce conventional readable logs only after a strict trace audit."""
    text = trace.read_text()
    rows = audit(text)
    facts = audit_characterization(text, require_characterization)
    output.mkdir(parents=True, exist_ok=False)
    detail = ["# What it summarizes: native row-sum contracts preserved by PREOPT; no executable reduction lowering."]
    detail.append("characterization=" + ("canonical_whirl" if facts else "unavailable_legacy_trace"))
    for fact in facts:
        detail.extend([
            "state node=" + fact["node"] + " instances=" + fact["rows"] +
            " dtype=float32 identity=0 update=fp32_add finalize=identity real_merge=add merge_permission=prohibited",
            "access input=" + fact["input"] + " relation=X[r,k] r=[0," + fact["rows"] +
            ") k=[0," + fact["width"] + ") effect=read_only logical_reads=" +
            str(int(fact["rows"]) * int(fact["width"])) + " physical_traffic=unknown alias=may_alias",
            "result value=" + fact["result"] + " relation=Y[r] owner_obligation=per_row physical_storage=unplanned",
            "numerical order=source_unresolved reassociate=forbidden reformulate=not_granted finite=unproved overflow=unproved accuracy=unproved",
        ])
    detail.append("node value rows width axis keepdim accum_dtype reassociate status source_line")
    detail.extend(" ".join(row) for row in rows)
    (output / "semantic.log").write_text("\n".join(detail) + "\n")
    (output / "optimization-summary.log").write_text(
        "# What it summarizes: verified reduction contracts, phase coverage and remaining optimization gates.\n"
        "P0/PREOPT contracts_verified=" + str(len(rows)) + " entry_exit=identical\n"
        "state_access_numerical=" + ("canonical_whirl" if facts else "unavailable_legacy_trace") + "\n"
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
    parser.add_argument("--require-characterization", action="store_true")
    args = parser.parse_args()
    write_reports(args.trace, args.reports, args.require_characterization)


if __name__ == "__main__":
    main()
