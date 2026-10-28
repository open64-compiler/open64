"""Audit native stable-Softmax characterization, never execution or speedups.

Design: doc/AI-COMPILER-NATIVE-SOFTMAX-CONTRACT-v0.1.md.
"""
import argparse
from pathlib import Path

FORM_COLUMNS = ("phase node value rows width axis accum_dtype reassociate "
                "reformulate exp status source_line").split()
FACT_COLUMNS = ("phase pu node input result input_ty result_ty rows width recipe state "
                "stabilize exp finalize order reassociate reformulate finite overflow "
                "accuracy exception input_access output_access input_effect alias "
                "output_owner storage source_line").split()
FORM_HEADER = "AI_SOFTMAX 1 HEADER " + " ".join(FORM_COLUMNS)
FACT_HEADER = "AI_SOFTMAX_FACT 1 HEADER " + " ".join(FACT_COLUMNS)
FORM_FIXED = dict(axis="1", accum_dtype="float32", reassociate="0", reformulate="0",
                  exp="source_unresolved", status="verified")
FACT_FIXED = dict(recipe="max_exp_sum_div", state="max_f32,sum_f32",
    stabilize="subtract_row_max", exp="source_unresolved", finalize="exp_over_sum",
    order="source_unresolved", reassociate="0", reformulate="0", finite="unproved",
    overflow="unproved", accuracy="unproved", exception="source_unresolved",
    input_access="X[r,k]", output_access="Y[r,k]", input_effect="read_only",
    alias="may_alias", output_owner="per_row", storage="unplanned")


def _records(text, tag, columns, header, kind, fixed, positive):
    """Parse only this versioned positional schema and reject incomplete pairs."""
    records = []
    pending = {}
    seen = False
    for line in text.splitlines():
        if not line.startswith(tag + " "):
            continue
        if line == header:
            seen = True
            continue
        row = line.split()
        if not seen or row[:3] != [tag, "1", kind] or len(row) != len(columns) + 3:
            raise ValueError("malformed Softmax " + kind)
        fact = dict(zip(columns, row[3:]))
        if any(int(fact[name]) <= 0 for name in positive):
            raise ValueError("invalid Softmax identity/domain/source line")
        if any(fact[name] != value for name, value in fixed.items()):
            raise ValueError("noncanonical Softmax numerical/access/state fact")
        key = (fact["node"], fact.get("value", fact.get("result")))
        canonical = {name: value for name, value in fact.items() if name != "phase"}
        if fact["phase"] == "preopt_enter":
            if key in pending:
                raise ValueError("duplicate Softmax entry")
            pending[key] = canonical
        elif fact["phase"] == "preopt_exit":
            if pending.pop(key, None) != canonical:
                raise ValueError("Softmax exit differs from entry")
        else:
            raise ValueError("unknown Softmax phase")
        records.append(fact)
    if pending or not records:
        raise ValueError("missing complete Softmax evidence")
    return records


def audit(text):
    """Require identical PREOPT pairs and join every FACT with its FORM event."""
    forms = _records(text, "AI_SOFTMAX", FORM_COLUMNS, FORM_HEADER, "FORM", FORM_FIXED,
                     ("node", "value", "rows", "width", "source_line"))
    facts = _records(text, "AI_SOFTMAX_FACT", FACT_COLUMNS, FACT_HEADER, "FACT", FACT_FIXED,
                     ("pu", "node", "input", "result", "input_ty", "result_ty",
                      "rows", "width", "source_line"))
    if len(forms) != len(facts):
        raise ValueError("incomplete Softmax characterization")
    for form, fact in zip(forms, facts):
        if (any(form[name] != fact[name] for name in
                ("phase", "node", "rows", "width", "source_line")) or
                form["value"] != fact["result"] or fact["input"] == fact["result"]):
            raise ValueError("Softmax FACT disagrees with FORM")
    return [fact for fact in facts if fact["phase"] == "preopt_exit"]


def write_reports(trace, output):
    """Publish conventional phase logs only after all structural claims audit."""
    facts = audit(trace.read_text())
    output.mkdir(parents=True, exist_ok=False)
    detail = ["# What it summarizes: canonical-WHIRL stable-Softmax recipe, logical state/accesses and unresolved numerical obligations."]
    for fact in facts:
        detail.extend([
            "contract node=" + fact["node"] + " input=" + fact["input"] +
            " result=" + fact["result"] + " rows=" + fact["rows"] +
            " width=" + fact["width"] + " source_line=" + fact["source_line"],
            "state=max_f32,sum_f32 instances_per_stat=" + fact["rows"] +
            " max_identity=nonempty_row sum_identity=0 storage=unplanned",
            "recipe=max(X[r,:]);E=exp_fp32(X-m);l=sum_fp32(E);Y=E/l execution=unlowered",
            "access=X[r,k]:read_only,Y[r,k]:definition owner_obligation=per_row alias=may_alias physical_traffic=unknown",
            "numerical order=source_unresolved exp=source_unresolved exception=source_unresolved reassociate=forbidden reformulate=forbidden finite=unproved overflow=unproved accuracy=unproved",
            "alternatives=retain_exp,recompute_exp_same_reductions status=not_selected numerical_gate=required online_max_sum=permission_rejected",
        ])
    (output / "semantic.log").write_text("\n".join(detail) + "\n")
    (output / "optimization.log").write_text(
        "# What it summarizes: full successful phase-report collection for native stable-Softmax characterization.\n" +
        "\n".join(detail) + "\n")
    (output / "optimization-summary.log").write_text(
        "# What it summarizes: Softmax phase coverage, native verification and remaining gates.\n" +
        "P0/PREOPT contracts_verified=" + str(len(facts)) + " entry_exit=identical\n" +
        "state_access_numerical=canonical_whirl\n" +
        "candidate_selection=not_run profitability=not_run transformation=none\n" +
        "frontend_provenance=not_in_trace execution_lowering=unsupported native_gpu=not_run\n")
    return facts


def main():
    """Audit a preserved .t file without compiler or Python ML dependencies."""
    parser = argparse.ArgumentParser()
    parser.add_argument("--trace", type=Path, required=True)
    parser.add_argument("--reports", type=Path, required=True)
    args = parser.parse_args()
    write_reports(args.trace, args.reports)


if __name__ == "__main__":
    main()
