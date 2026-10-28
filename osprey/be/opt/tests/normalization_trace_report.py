"""Audit native centered population normalization, not execution or speedups.

Design: doc/AI-COMPILER-NATIVE-NORMALIZATION-CONTRACT-v0.1.md.
"""
import argparse
from pathlib import Path

FORM_COLUMNS = ("phase node value rows width axis variance epsilon_bits accum_dtype "
                "reassociate reformulate contract rsqrt status source_line").split()
FACT_COLUMNS = ("phase pu node input gamma beta result input_ty gamma_ty beta_ty result_ty "
                "rows width epsilon_bits recipe state denominator epsilon_position affine_order "
                "order rsqrt reassociate reformulate contract finite overflow accuracy exception "
                "input_access gamma_access beta_access output_access input_effect alias output_owner "
                "storage source_line").split()
FORM_HEADER = "AI_NORM 1 HEADER " + " ".join(FORM_COLUMNS)
FACT_HEADER = "AI_NORM_FACT 1 HEADER " + " ".join(FACT_COLUMNS)
FORM_FIXED = dict(axis="1", variance="population_centered", accum_dtype="float32",
                  reassociate="0", reformulate="0", contract="0",
                  rsqrt="source_unresolved", status="verified")
FACT_FIXED = dict(recipe="centered_population", state="mean_f32,variance_f32",
    epsilon_position="inside_rsqrt", affine_order="normalize_then_scale_then_bias",
    order="source_unresolved", rsqrt="source_unresolved", reassociate="0", reformulate="0",
    contract="0", finite="unproved", overflow="unproved", accuracy="unproved",
    exception="source_unresolved", input_access="X[r,k]", gamma_access="gamma[k]",
    beta_access="beta[k]", output_access="Y[r,k]", input_effect="read_only",
    alias="may_alias", output_owner="per_row", storage="unplanned")


def _records(text, tag, columns, header, kind, fixed, positive):
    """Check version, exact facts and complete identical PREOPT boundary pairs."""
    records, pending = [], {}
    seen = False
    for line in text.splitlines():
        if not line.startswith(tag + " "):
            continue
        if line == header:
            seen = True
            continue
        row = line.split()
        if not seen or row[:3] != [tag, "1", kind] or len(row) != len(columns) + 3:
            raise ValueError("malformed normalization " + kind)
        fact = dict(zip(columns, row[3:]))
        epsilon = fact["epsilon_bits"]
        if (any(int(fact[name]) <= 0 for name in positive) or len(epsilon) != 8 or
                any(c not in "0123456789abcdef" for c in epsilon) or
                not 0 < int(epsilon, 16) < 0x7f800000):
            raise ValueError("invalid normalization identity/domain/epsilon")
        if any(fact[name] != value for name, value in fixed.items()):
            raise ValueError("noncanonical normalization recipe or numerical permission")
        key = (fact["node"], fact.get("value", fact.get("result")))
        canonical = {name: value for name, value in fact.items() if name != "phase"}
        if fact["phase"] == "preopt_enter":
            if key in pending:
                raise ValueError("duplicate normalization entry")
            pending[key] = canonical
        elif fact["phase"] == "preopt_exit":
            if pending.pop(key, None) != canonical:
                raise ValueError("normalization exit differs from entry")
        else:
            raise ValueError("unknown normalization phase")
        records.append(fact)
    if pending or not records:
        raise ValueError("missing complete normalization evidence")
    return records


def audit(text):
    """Join every native state/access fact to its exact canonical FORM event."""
    forms = _records(text, "AI_NORM", FORM_COLUMNS, FORM_HEADER, "FORM", FORM_FIXED,
                     ("node", "value", "rows", "width", "source_line"))
    facts = _records(text, "AI_NORM_FACT", FACT_COLUMNS, FACT_HEADER, "FACT", FACT_FIXED,
                     ("pu", "node", "input", "gamma", "beta", "result", "input_ty",
                      "gamma_ty", "beta_ty", "result_ty", "rows", "width", "denominator", "source_line"))
    if len(forms) != len(facts):
        raise ValueError("incomplete normalization characterization")
    for form, fact in zip(forms, facts):
        if (any(form[name] != fact[name] for name in
                ("phase", "node", "rows", "width", "epsilon_bits", "source_line")) or
                form["value"] != fact["result"] or fact["denominator"] != fact["width"] or
                fact["result"] in (fact["input"], fact["gamma"], fact["beta"])):
            raise ValueError("normalization FACT disagrees with FORM/domain")
    return [fact for fact in facts if fact["phase"] == "preopt_exit"]


def write_reports(trace, output):
    """Publish conventional logs only after strict structural/numerical audit."""
    facts = audit(trace.read_text())
    output.mkdir(parents=True, exist_ok=False)
    detail = ["# What it summarizes: canonical-WHIRL centered population normalization, affine broadcasts and numerical obligations."]
    for f in facts:
        detail.extend([
            "contract node=" + f["node"] + " input=" + f["input"] + " gamma=" + f["gamma"] +
            " beta=" + f["beta"] + " result=" + f["result"] + " rows=" + f["rows"] +
            " width=" + f["width"] + " epsilon_bits=" + f["epsilon_bits"] + " source_line=" + f["source_line"],
            "recipe=mu=mean(X);d=X-mu;v=mean(d*d);q=rsqrt(v+epsilon);Y=((d*q)*gamma)+beta",
            "state=mean_f32,variance_f32 instances_per_stat=" + f["rows"] +
            " variance=population_centered denominator=" + f["width"] + " epsilon_position=inside_rsqrt storage=unplanned",
            "access=X[r,k],gamma[k],beta[k]:read_only;Y[r,k]:definition affine_broadcast=across_rows owner_obligation=per_row alias=may_alias physical_traffic=unknown",
            "numerical order=source_unresolved rsqrt=source_unresolved exception=source_unresolved reassociate=forbidden reformulate=forbidden contract=forbidden finite=unproved overflow=unproved accuracy=unproved",
            "alternatives=retain_centered,recompute_centered_same_means status=not_selected numerical_gate=required welford_or_raw_moments=permission_rejected",
        ])
    (output / "semantic.log").write_text("\n".join(detail) + "\n")
    (output / "optimization.log").write_text(
        "# What it summarizes: complete successful semantic phase-report collection for native normalization.\n" +
        "\n".join(detail) + "\n")
    (output / "optimization-summary.log").write_text(
        "# What it summarizes: normalization phase coverage and remaining numerical/executable gates.\n" +
        "P0/PREOPT contracts_verified=" + str(len(facts)) + " entry_exit=identical\n" +
        "state_access_numerical=canonical_whirl affine_broadcast=verified\n" +
        "candidate_selection=not_run profitability=not_run transformation=none\n" +
        "frontend_provenance=not_in_trace execution_lowering=unsupported native_gpu=not_run\n")
    return facts


def main():
    """Audit preserved .t without compiler linkage or ML dependencies."""
    parser = argparse.ArgumentParser()
    parser.add_argument("--trace", type=Path, required=True)
    parser.add_argument("--reports", type=Path, required=True)
    args = parser.parse_args()
    write_reports(args.trace, args.reports)


if __name__ == "__main__":
    main()
