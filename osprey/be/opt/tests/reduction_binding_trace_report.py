"""Strict R2 reduction binding/alternative auditor and concise phase reports.

Design: doc/AI-COMPILER-REDUCTION-NUMERICAL-BINDING-v0.1.md.
Legality discovery is not target certification, cost selection or transformation.
"""
import argparse
from pathlib import Path

NUM_COLUMNS = "phase pu node input result version rows width order identity_bits environment nan_policy binding source_line".split()
ALT_COLUMNS = "phase node candidate chunk scratch_bytes budget_bytes numerical storage target selected reason source_line".split()
NUM_HEADER = "AI_REDUCE_NUM 1 HEADER " + " ".join(NUM_COLUMNS)
ALT_HEADER = "AI_REDUCE_ALT 1 HEADER " + " ".join(ALT_COLUMNS)


def records(text, tag, kind, columns, header):
    """Require exact positional schemas, complete rows and no silent versions."""
    found, seen = [], False
    for line in text.splitlines():
        if not line.startswith(tag + " "):
            continue
        if line == header:
            seen = True
            continue
        fields = line.split()
        if not seen or fields[:3] != [tag, "1", kind] or len(fields) != len(columns) + 3:
            raise ValueError("malformed " + tag)
        found.append(dict(zip(columns, fields[3:])))
    if not found:
        raise ValueError("missing " + tag)
    return found


def validate_binding(row):
    """Only native v2 binds ordered arithmetic; native v1 remains unresolved."""
    for key in ("pu", "node", "input", "result", "rows", "width", "source_line"):
        if int(row[key]) <= 0:
            raise ValueError("nonpositive binding identity/domain/source")
    if row["input"] == row["result"]:
        raise ValueError("result identity aliases input")
    if row["version"] == "2":
        expected = ["ascending_left_fold", "00000000", "rne_gradual_no_traps", "class_only", "explicit"]
    elif row["version"] == "1":
        expected = ["source_unresolved", "semantic_zero", "source_unresolved", "source_unresolved", "missing"]
    else:
        raise ValueError("unknown source version")
    if [row[key] for key in ("order", "identity_bits", "environment", "nan_policy", "binding")] != expected:
        raise ValueError("invented or changed numerical binding")


def audit_preopt(text):
    """Require identical numerical binding at both actual PREOPT boundaries."""
    rows = records(text, "AI_REDUCE_NUM", "BIND", NUM_COLUMNS, NUM_HEADER)
    if len(rows) != 2 or [row["phase"] for row in rows] != ["preopt_enter", "preopt_exit"]:
        raise ValueError("complete PREOPT boundary pair required")
    for row in rows:
        validate_binding(row)
    if {k: v for k, v in rows[0].items() if k != "phase"} != {
            k: v for k, v in rows[1].items() if k != "phase"}:
        raise ValueError("PREOPT numerical binding drift")
    return rows[1]


def audit(text):
    """Cross-check all alternatives against native source meaning and budget."""
    bindings = records(text, "AI_REDUCE_NUM", "BIND", NUM_COLUMNS, NUM_HEADER)
    if len(bindings) != 1 or bindings[0]["phase"] != "r2_query":
        raise ValueError("one explicit query binding required")
    binding = bindings[0]
    validate_binding(binding)
    rows = records(text, "AI_REDUCE_ALT", "ALT", ALT_COLUMNS, ALT_HEADER)
    if len(rows) != 3 or [row["candidate"] for row in rows] != [
            "direct_ordered", "staged_ordered", "partition_tree"]:
        raise ValueError("complete unique alternative set required")
    budgets = {row["budget_bytes"] for row in rows}
    if len(budgets) != 1 or int(next(iter(budgets))) < 0:
        raise ValueError("one nonnegative scratch budget required")
    bound = binding["version"] == "2"
    for index, row in enumerate(rows):
        if (row["phase"] != "r2_query" or row["node"] != binding["node"] or
                row["source_line"] != binding["source_line"] or
                row["target"] != "unbound" or row["selected"] != "0"):
            raise ValueError("invented scope, target or selection")
        chunk, scratch, budget = (int(row[key]) for key in ("chunk", "scratch_bytes", "budget_bytes"))
        if index == 1:
            if not 0 < chunk <= int(binding["width"]) or scratch != 4 * chunk:
                raise ValueError("invalid staged footprint")
        elif chunk != 0 or scratch != 0:
            raise ValueError("nonstaged alternative has scratch")
        if index == 2:
            expected = ["forbidden", "not_evaluated", "reassociation_forbidden"]
        elif not bound:
            expected = ["unbound", "not_evaluated", "source_binding_missing"]
        elif scratch > budget:
            expected = ["legal_recipe", "budget_rejected", "scratch_budget_exceeded"]
        else:
            expected = ["legal_recipe", "budget_ok", "ordered_recipe_preserved"]
        if [row[key] for key in ("numerical", "storage", "reason")] != expected:
            raise ValueError("incorrect alternative legality/reason")
    return binding, rows


def write_reports(trace, preopt, output):
    """Summarize complete source/legality evidence without executable claims."""
    binding, rows = audit(trace.read_text())
    after = audit_preopt(preopt.read_text())
    if {k: v for k, v in binding.items() if k != "phase"} != {
            k: v for k, v in after.items() if k != "phase"}:
        raise ValueError("query/PREOPT identity mismatch")
    output.mkdir(parents=True, exist_ok=False)
    semantic = (
        "# What it summarizes: source-bound reduction arithmetic and actual PREOPT preservation.\n"
        "version={version} rows={rows} width={width} order={order} identity_bits={identity_bits} "
        "environment={environment} nan_policy={nan_policy} binding={binding} source_line={source_line}\n"
    ).format(**binding) + (
        "reassociate=forbidden source_v1_auto_upgrade=forbidden finite=unproved overflow=unproved "
        "real_sum_accuracy=unproved target_arithmetic=unbound\n")
    alternatives = "# What it summarizes: legal ordered recipes, scratch-budget rejection and forbidden partitioning.\n"
    for row in rows:
        alternatives += ("candidate={candidate} chunk={chunk} scratch_bytes={scratch_bytes} "
                         "budget_bytes={budget_bytes} numerical={numerical} storage={storage} "
                         "target={target} selected={selected} reason={reason} source_line={source_line}\n").format(**row)
    summary = (
        "# What it summarizes: R2 numerical-binding and legal-alternative coverage with remaining gates.\n"
        "PREOPT binding=preserved scope=active_whole_pu\n"
        "candidate_selection=not_run profitability=not_run transformation=none\n"
        "target_arithmetic=unbound alias_storage_events=required native_execution=not_run gpu=not_run\n")
    (output / "semantic.log").write_text(semantic)
    (output / "alternatives.log").write_text(alternatives)
    (output / "optimization.log").write_text(
        "# What it summarizes: all successful numerical-binding and alternative-discovery reports.\n" +
        semantic + alternatives)
    (output / "optimization-summary.log").write_text(summary)


def main():
    """Audit saved compiler traces into a new report directory."""
    parser = argparse.ArgumentParser()
    parser.add_argument("--trace", required=True, type=Path)
    parser.add_argument("--preopt", required=True, type=Path)
    parser.add_argument("--reports", required=True, type=Path)
    args = parser.parse_args()
    write_reports(args.trace, args.preopt, args.reports)


if __name__ == "__main__":
    main()
