"""Audit native LN-REF-1 binding, not source equivalence or execution.

Design: doc/AI-COMPILER-LAYERNORM-NATIVE-REFERENCE-CONTRACT-v0.1.md.
Preserve positional WOPT traces and do not reinterpret existing v1 records.
"""
import argparse
from pathlib import Path
import re
import struct

from normalization_trace_report import audit as audit_v1


COLUMNS = ("phase pu node x gamma beta result version rows width epsilon_bits count_bits "
           "reference mean_order mean_identity_bits mean_finalize count_conversion "
           "environment nan_policy rsqrt verified source_line").split()
HEADER = "AI_NORM_REF 1 HEADER " + " ".join(COLUMNS)
FIXED = dict(version="2", reference="ln_ref_1", mean_order="ascending_left_fold",
             mean_identity_bits="00000000", mean_finalize="sum_then_div_rne",
             count_conversion="exact_f32_from_static_n", environment="rne_gradual_no_traps",
             nan_policy="class_only", rsqrt="sqrt_rne_then_div_rne_f32", verified="verified")
POSITIVE = ("pu", "node", "x", "gamma", "beta", "result", "rows", "width", "source_line")


def audit(text):
    """Require canonical fields and one identical entry/exit pair per PU/node.

    Count conversion is independently recomputed, never trusted from the trace.
    Existing v1 events, when present, keep their own unchanged strict auditor.
    """
    pending, completed, facts = {}, set(), []
    seen_header = False
    for line in text.splitlines():
        if not line.startswith("AI_NORM_REF "):
            continue
        if line == HEADER:
            seen_header = True
            continue
        fields = line.split()
        if not seen_header or fields[:3] != ["AI_NORM_REF", "1", "BIND"] or len(fields) != len(COLUMNS) + 3:
            raise ValueError("malformed LN-REF-1 header/schema/binding")
        fact = dict(zip(COLUMNS, fields[3:]))
        if any(not re.fullmatch(r"[1-9][0-9]*", fact[name]) for name in POSITIVE):
            raise ValueError("noncanonical LN-REF-1 identity/domain/source")
        if any(fact[name] != value for name, value in FIXED.items()):
            raise ValueError("contradictory LN-REF-1 numerical binding")
        width, rows = int(fact["width"]), int(fact["rows"])
        if width > 2**24 or rows * width * 4 > 2**64 - 1:
            raise ValueError("unsupported LN-REF-1 count/footprint")
        epsilon = fact["epsilon_bits"]
        if not re.fullmatch(r"[0-9a-f]{8}", epsilon) or not 0 < int(epsilon, 16) < 0x7f800000:
            raise ValueError("invalid LN-REF-1 epsilon")
        count_bits = struct.pack(">f", width).hex()
        if fact["count_bits"] != count_bits:
            raise ValueError("LN-REF-1 count bits disagree with exact N")
        if fact["result"] in (fact["x"], fact["gamma"], fact["beta"]) or fact["x"] in (fact["gamma"], fact["beta"]):
            raise ValueError("impossible LN-REF-1 operand/result identities")
        key = (fact["pu"], fact["node"], fact["result"])
        canonical = {name: value for name, value in fact.items() if name != "phase"}
        if fact["phase"] == "preopt_enter":
            if key in pending or key in completed:
                raise ValueError("duplicate LN-REF-1 entry")
            pending[key] = canonical
        elif fact["phase"] == "preopt_exit":
            if pending.pop(key, None) != canonical or key in completed:
                raise ValueError("LN-REF-1 exit differs from entry")
            completed.add(key)
            facts.append(fact)
        else:
            raise ValueError("unknown LN-REF-1 phase")
    if pending or not facts:
        raise ValueError("missing complete LN-REF-1 evidence")
    if any(line.startswith(("AI_NORM 1 FORM ", "AI_NORM_FACT 1 FACT ")) for line in text.splitlines()):
        audit_v1(text)
    return facts


def write_reports(trace, output):
    """Publish conventional reports after audit; refuse existing destinations.

    Verified numerical source meaning never becomes a source-provenance,
    target, plan-selection or transformation claim in these reports.
    """
    text = trace.read_text()
    facts = audit(text)
    legacy = audit_v1(text) if "AI_NORM 1 FORM " in text else []
    output.mkdir(parents=True, exist_ok=False)
    detail = ["# What it summarizes: native LN-REF-1 v2 identity, ordered means, exact count, sqrt/div semantics and open execution obligations."]
    for fact in facts:
        detail.extend([
            "contract=common.mean_variance_norm.v2 reference=ln_ref_1 pu=" + fact["pu"] +
            " node=" + fact["node"] + " input=" + fact["x"] + " gamma=" + fact["gamma"] +
            " beta=" + fact["beta"] + " result=" + fact["result"] + " rows=" + fact["rows"] +
            " width=" + fact["width"] + " epsilon_bits=" + fact["epsilon_bits"] +
            " count_bits=" + fact["count_bits"] + " source_line=" + fact["source_line"],
            "means=ascending_left_fold identity_bits=00000000 finalize=sum_then_div_rne count=exact_f32_from_static_n square=separately_rounded population_denominator=" + fact["width"],
            "normalization=t=add32(v,epsilon);h=sqrt32(t);q=div32(+1,h);Y=add32(mul32(mul32(d,q),gamma),beta)",
            "environment=rne_gradual_no_traps nan_policy=class_only non_nan=exact_bits reassociate=forbidden reformulate=forbidden contract=forbidden",
            "access=X[r,k],gamma[k],beta[k]:read_only;Y[r,k]:definition affine_broadcast=across_rows alias=may_alias storage=unplanned output_owner=unassigned target=unbound finite=unproved overflow=unproved accuracy=unproved",
            "producer_provenance=not_in_trace v1_migration=none source_equivalence=not_claimed candidate_selection=not_run profitability=not_run transformation=none",
        ])
    for fact in legacy:
        detail.append("preserved_contract=common.mean_variance_norm.v1 pu=" + fact["pu"] +
            " node=" + fact["node"] + " result=" + fact["result"] +
            " order=source_unresolved rsqrt=source_unresolved exception=source_unresolved" +
            " migration=none source_line=" + fact["source_line"])
    semantic = "\n".join(detail) + "\n"
    (output / "semantic.log").write_text(semantic)
    target = ("# What it summarizes: remaining LN-REF-1 target arithmetic and executable-lowering gates.\n"
              "target_binding=not_run carriers=not_certified runtime_storage_events=unbound execution_lowering=unsupported native_gpu=not_run\n")
    (output / "target-binding.log").write_text(target)
    (output / "optimization.log").write_text(
        "# What it summarizes: complete LN-REF-1 semantic and target-gate report collection.\n" + semantic + target)
    v1_count = len(legacy)
    summary = ["# What it summarizes: LN-REF-1 native/PREOPT coverage, preserved v1 and open optimization/execution gates.",
               "contracts_verified=" + str(len(facts)) + " v1_contracts_verified=" + str(v1_count) + " entry_exit=identical arithmetic=bound_to_ln_ref_1",
               "producer_provenance=not_in_trace source_equivalence=not_claimed target_binding=not_run candidate_selection=not_run profitability=not_run transformation=none execution_lowering=unsupported native_gpu=not_run"]
    for phase in range(12):
        summary.append("P" + str(phase) + (" status=analyzed_no_rewrite reason=native_semantic_binding" if phase == 0 else
                       " status=disabled_by_policy reason=native_contract_only") + " selected=none rewrites=0")
    (output / "optimization-summary.log").write_text("\n".join(summary) + "\n")
    return facts


def main():
    """Replay a retained native WOPT trace without compiler or ML linkage."""
    parser = argparse.ArgumentParser()
    parser.add_argument("--trace", required=True, type=Path)
    parser.add_argument("--reports", required=True, type=Path)
    args = parser.parse_args()
    write_reports(args.trace, args.reports)


if __name__ == "__main__":
    main()
