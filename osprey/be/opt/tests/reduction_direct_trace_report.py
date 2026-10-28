"""Audit direct ordered-reduction ISA/storage/event contracts, not execution.

Design: doc/AI-COMPILER-REDUCTION-DIRECT-BINDING-v0.1.md.
"""
import argparse
from pathlib import Path

from reduction_binding_trace_report import audit_preopt

COLUMNS = ("node input result rows width recipe source_match contiguous arch instruction "
           "fp_options contract context in_alloc in_capacity in_offset in_align in_lease "
           "in_space out_alloc out_capacity out_offset out_align out_lease out_space mapping threads "
           "e0 e1 e2 e3 e4 e5 in_bytes out_bytes stages obligations status selected source_line").split()
HEADER = "AI_REDUCE_DIRECT 1 HEADER " + " ".join(COLUMNS)
MAXIMUM = (1 << 64) - 1


def events_valid(edges):
    """Independently use graph traversal, not the native transitive closure."""
    if any(value & ~63 for value in edges):
        return False
    reachable = []
    for start in range(6):
        visited, pending = set(), [index for index in range(6) if edges[start] & (1 << index)]
        while pending:
            node = pending.pop()
            if node == start:
                return False
            if node in visited:
                continue
            visited.add(node)
            pending.extend(index for index in range(6) if edges[node] & (1 << index))
        reachable.append(visited)
    return all(after in reachable[before] for before, after in ((0, 1), (1, 2), (2, 3), (2, 4), (3, 5)))


def expected(row):
    """Recompute the gate order and validated prefix from all recorded inputs."""
    stages, input_bytes, output_bytes = 0, 0, 0

    def result(reason):
        """Only a fully bound compiler contract carries runtime obligations."""
        return reason, stages, input_bytes, output_bytes, 15 if reason == "bound_runtime_obligations" else 0

    if row["source_match"] != 1:
        return result("stale_source")
    if row["recipe"] != 2:
        return result("source_binding_missing")
    stages = 1
    if row["arch"] not in (1, 2, 3, 4):
        return result("unknown_architecture")
    if row["instruction"] != 1:
        return result("unsupported_instruction")
    if row["fp_options"] != 0:
        return result("unsafe_fp_options")
    stages = 3
    if row["contiguous"] != 1:
        return result("unsupported_layout")
    if row["rows"] * row["width"] * 4 > MAXIMUM:
        return result("invalid_range")
    input_bytes, output_bytes = row["rows"] * row["width"] * 4, row["rows"] * 4

    def valid(prefix, size):
        """Prove only relative ranges and declared base-alignment guarantees."""
        alignment = row[prefix + "_align"]
        return (row[prefix + "_space"] == 1 and row[prefix + "_alloc"] > 0 and
                alignment >= 4 and alignment & (alignment - 1) == 0 and
                row[prefix + "_offset"] % 4 == 0 and
                row[prefix + "_offset"] + size <= row[prefix + "_capacity"])

    if not valid("in", input_bytes) or not valid("out", output_bytes):
        return result("invalid_range")
    if not row["contract"] or not row["context"] or row["in_lease"] != 1 or row["out_lease"] != 2:
        return result("runtime_lease_missing")
    if row["in_alloc"] == row["out_alloc"]:
        if row["in_capacity"] != row["out_capacity"]:
            return result("invalid_range")
        if max(row["in_offset"], row["out_offset"]) < min(
                row["in_offset"] + input_bytes, row["out_offset"] + output_bytes):
            return result("overlapping_ranges")
    stages = 7
    if row["mapping"] != 1 or row["threads"] < row["rows"]:
        return result("invalid_owner_mapping")
    if not events_valid([row["e%d" % index] for index in range(6)]):
        return result("invalid_event_order")
    stages = 15
    return result("bound_runtime_obligations")


def audit(text):
    """Reject invented stage/runtime/selection proofs and incomplete schemas."""
    rows, seen = [], False
    narrow = set(("node input result recipe source_match contiguous arch instruction fp_options "
                  "in_align in_lease in_space out_align out_lease out_space mapping "
                  "e0 e1 e2 e3 e4 e5 stages obligations selected source_line").split())
    for line in text.splitlines():
        if not line.startswith("AI_REDUCE_DIRECT "):
            continue
        if line == HEADER:
            seen = True
            continue
        fields = line.split()
        if not seen or fields[:3] != ["AI_REDUCE_DIRECT", "1", "BIND"] or len(fields) != len(COLUMNS) + 3:
            raise ValueError("malformed direct binding")
        raw = dict(zip(COLUMNS, fields[3:]))
        row = {key: value if key == "status" else int(value) for key, value in raw.items()}
        if any(value < 0 or value > ((1 << 32) - 1 if key in narrow else MAXIMUM)
               for key, value in row.items() if key != "status"):
            raise ValueError("invalid integer domain")
        if (any(row[key] == 0 for key in ("node", "input", "result", "rows", "width", "source_line")) or
                row["input"] == row["result"] or row["recipe"] not in (1, 2) or
                row["source_match"] not in (0, 1) or row["contiguous"] not in (0, 1) or row["selected"] != 0):
            raise ValueError("invalid source identity or invented selection")
        observed = tuple(row[key] for key in ("status", "stages", "in_bytes", "out_bytes", "obligations"))
        if observed != expected(row):
            raise ValueError("incorrect direct-binding proof or reason")
        rows.append(row)
    if not rows:
        raise ValueError("missing direct-binding evidence")
    return rows


def write_reports(trace, preopt, output):
    """Explain conditional contracts, actual source preservation and open gates."""
    rows = audit(trace.read_text())
    native = audit_preopt(preopt.read_text())
    for row in rows:
        if any(str(row[key]) != native[other] for key, other in (
                ("node", "node"), ("input", "input"), ("result", "result"),
                ("rows", "rows"), ("width", "width"), ("recipe", "version"), ("source_line", "source_line"))):
            raise ValueError("direct/PREOPT source mismatch")
    output.mkdir(parents=True, exist_ok=False)
    target = "# What it summarizes: documented scalar ISA binding and rejected compiler FP policies, not PTX/SASS certification.\n"
    storage = "# What it summarizes: exact symbolic tensor byte ranges, canonical allocation/lease obligations and row ownership.\n"
    events = "# What it summarizes: host dependency DAG and direct synchronous ownership protocol, not observed runtime events.\n"
    for index, row in enumerate(rows):
        target += ("query=%d arch=%d instruction=%d fp_options=%d stages=%d status=%s source_line=%d\n" %
                   (index, row["arch"], row["instruction"], row["fp_options"], row["stages"], row["status"], row["source_line"]))
        storage += ("query=%d input_bytes=%d output_bytes=%d in_alloc=%d in_offset=%d out_alloc=%d "
                    "out_offset=%d threads=%d obligations=%d status=%s source_line=%d\n" %
                    (index, row["in_bytes"], row["out_bytes"], row["in_alloc"], row["in_offset"],
                     row["out_alloc"], row["out_offset"], row["threads"], row["obligations"], row["status"], row["source_line"]))
        events += "query=%d edges=%s status=%s source_line=%d\n" % (
            index, ",".join(str(row["e%d" % i]) for i in range(6)), row["status"], row["source_line"])
    events += ("protocol=ready->launch->complete->consume->output_release;complete->input_release\n"
               "kernel=one_thread_per_row;guard_row_lt_R;load_k->add_k->next_k->final_store "
               "scratch_bytes=0 barriers=0 async_copies=0\n")
    summary = ("# What it summarizes: direct fallback contract binding, refusals and remaining execution gates.\n"
               "queries=%d bound_contracts=%d PREOPT=source_preserved\n" % (
                   len(rows), sum(row["status"] == "bound_runtime_obligations" for row in rows)))
    summary += ("runtime_obligations=canonical_allocations,live_leases,event_enforcement,launch_conformance\n"
                "candidate_selection=not_run profitability=not_run transformation=none\n"
                "runtime_enforcement=not_implemented lowering=not_implemented instruction_certification=not_run native_gpu=not_run\n")
    for name, content in (("target-binding.log", target), ("residency.log", storage),
                          ("fetch-pipeline.log", events), ("optimization-summary.log", summary)):
        (output / name).write_text(content)
    (output / "optimization.log").write_text(
        "# What it summarizes: all successful direct target/storage/event binding reports and recorded refusals.\n" +
        target + storage + events)


def main():
    """Audit immutable raw traces into a fresh report directory."""
    parser = argparse.ArgumentParser()
    parser.add_argument("--trace", type=Path, required=True)
    parser.add_argument("--preopt", type=Path, required=True)
    parser.add_argument("--reports", type=Path, required=True)
    args = parser.parse_args()
    write_reports(args.trace, args.preopt, args.reports)


if __name__ == "__main__":
    main()
