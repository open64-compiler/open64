"""Independently audit native LN2 plans, not runtime enforcement or speedups.

Design: doc/AI-COMPILER-LAYERNORM-STORAGE-EVENTS-v0.1.md.
No reduction planning imports. Recompute every range/resource/event gate from
actual native source and target rows; emit conventional condensed reports.
"""
import argparse
from pathlib import Path
import re

from normalization_ref_trace_report import audit as audit_source
from normalization_trace_report import audit as audit_v1
from normalization_target_trace_report import audit as audit_target, ARCHITECTURES

COLS = ("pu node result kind layout status target_status stages obligations runtime context ownership threads ctas tail stride shared "
        "participants barriers row_bytes required reserved limit quantum banks bank_word warp conflict required_barriers reason source_line").split()
HEADER = "AI_NORM_PLAN 1 HEADER " + " ".join(COLS)
HW = "arch arch_id profile revision capacity budget quantum alignment banks word warp threads source_line".split()
RANGES = "index allocation capacity offset alignment lease bytes source_line".split()
REASONS = ("conditional_storage_events", "target_refused", "unsupported_plan", "runtime_unbound", "invalid_range",
           "invalid_alias", "invalid_owner", "invalid_storage", "invalid_events", "unsupported_layout")
EVENTS = ("input_ready launch row_ready mean_done mu_visible centered_ready variance_done q_visible epilogue_done "
          "readers_done reuse_ready kernel_done result_visible input_release output_release").split()
MAX = 2**64-1
DERIVED = "stages obligations row_bytes required reserved limit quantum banks bank_word warp conflict required_barriers".split()


def require(condition, message):
    """Evidence gates remain active under optimized Python."""
    if not condition:
        raise ValueError(message)


def integers(words):
    """Reject narrowing, signed/leading-zero coercions and integer overflow."""
    require(all(re.fullmatch(r"0|[1-9][0-9]*", x) and int(x) <= MAX for x in words), "invalid plan integer")
    return [int(x) for x in words]


def power(n):
    """Power-of-two alignment is a guarantee, not a hint."""
    return n > 0 and n & (n-1) == 0


def ordered(edges):
    """Reject cycles/self/unknown nodes, then require conservative reachability."""
    n = len(EVENTS)
    if any(x >= 1 << n or x & 1 << i for i, x in enumerate(edges)):
        return False
    reach = list(edges)
    for k in range(n):
        for i in range(n):
            if reach[i] & 1 << k:
                reach[i] |= reach[k]
    return all(not reach[i] & 1 << i and (i == n-1 or reach[i] & 1 << (i+1)) for i in range(n))


def evaluate(r, ranges, edges, fact, hw, target_status):
    """Independently reconstruct validated prefixes and refusal precedence."""
    out = dict.fromkeys(DERIVED, 0)
    sizes = [0]*4

    def result(status):
        """Return a closed status plus only facts established before that gate."""
        return status, out, sizes

    if target_status:
        return result(1)
    out["stages"] = 1
    if r["kind"] not in (1, 2, 3): return result(2)
    if not r["runtime"] or not r["context"]: return result(3)
    if r["layout"] != 1: return result(9)
    n, rows = int(fact["width"]), int(fact["rows"])
    sizes[:] = [rows*n*4, n*4, n*4, rows*n*4]
    out["row_bytes"] = n*4
    for i, a in enumerate(ranges):
        if not (a["allocation"] and a["capacity"] and a["lease"] == (2 if i == 3 else 1) and
                power(a["alignment"]) and a["alignment"] >= 4 and a["offset"] % 4 == 0 and
                a["offset"] <= a["capacity"] and sizes[i] <= a["capacity"]-a["offset"]): return result(4)
    for i, a in enumerate(ranges):
        for j in range(i+1, 4):
            b = ranges[j]
            if a["allocation"] != b["allocation"]: continue
            if a["capacity"] != b["capacity"] or a["alignment"] != b["alignment"]: return result(5)
            if j == 3 and max(a["offset"], b["offset"]) < min(a["offset"]+sizes[i], b["offset"]+sizes[j]): return result(5)
    if fact["gamma"] == fact["beta"] and any(ranges[1][k] != ranges[2][k] for k in ("allocation", "offset")): return result(5)
    out["stages"] |= 2
    if not (r["ownership"] == 1 and r["ctas"] == rows and r["threads"] > 0 and
            r["threads"] % hw["warp"] == 0 and r["threads"] <= hw["threads"] and r["tail"] == 1): return result(6)
    out.update(stages=7, limit=min(hw["capacity"], hw["budget"]), quantum=hw["quantum"],
               banks=hw["banks"], bank_word=hw["word"], warp=hw["warp"])
    if r["kind"] == 1:
        if any(r[k] for k in ("stride", "shared", "barriers", "participants")): return result(7)
    else:
        if not (power(hw["alignment"]) and hw["quantum"] and hw["banks"] and hw["word"] == 4 and
                r["stride"] >= out["row_bytes"] and r["stride"] % hw["alignment"] == 0 and r["stride"] <= MAX-8): return result(7)
        out["required"] = r["stride"]+8
        if out["required"] > MAX-(hw["quantum"]-1): return result(7)
        out["reserved"] = ((out["required"]+hw["quantum"]-1)//hw["quantum"])*hw["quantum"]
        if r["shared"] != out["required"] or out["reserved"] > out["limit"]: return result(7)
        out["required_barriers"] = sum(1 << x for x in (4, 5, 7, 9)) | (1 << 2 if r["kind"] == 3 else 0)
        out["conflict"] = (min(n, hw["warp"])+hw["banks"]-1)//hw["banks"]
        if r["barriers"] != out["required_barriers"] or r["participants"] != r["threads"]: return result(8)
    out["stages"] |= 8
    if not ordered(edges): return result(8)
    out.update(stages=31, obligations=63)
    return result(0)


def audit(text, source):
    """Join every complete native block to the exact PREOPT fact and target row."""
    facts = audit_source(source)
    if "AI_NORM 1 FORM " in source: facts += audit_v1(source)
    joins = {tuple(f[k] for k in ("pu", "node", "result")): f for f in facts}
    lines = text.splitlines()
    require(lines and lines[0].split()[:3] == ["AI_NORM_HW", "1", "MODEL"], "missing typed hardware record")
    fields = lines[0].split()[3:]
    require(len(fields) == len(HW) and fields[0] in ARCHITECTURES, "invalid hardware schema")
    hw = dict(zip(HW, [fields[0]]+integers(fields[1:])))
    require(hw["arch_id"] == ARCHITECTURES[hw["arch"]] and hw["profile"] == (3 if hw["arch_id"] > 2 else 2) and
            all(hw[k] > 0 for k in HW[1:]) and power(hw["alignment"]) and power(hw["quantum"]), "invalid hardware facts")
    block = 4+4+len(EVENTS)
    require((len(lines)-1) % block == 0, "incomplete plan block")
    results = []
    for start in range(1, len(lines), block):
        chunk = lines[start:start+block]
        targets = audit_target("\n".join(chunk[:2]), source)
        require(len(targets) == 1 and targets[0]["arch"] == hw["arch"], "foreign/duplicate target")
        require(chunk[2] == HEADER and chunk[3].split()[:3] == ["AI_NORM_PLAN", "1", "BIND"], "unknown plan schema")
        fields = chunk[3].split()[3:]
        require(len(fields) == len(COLS), "invalid plan width")
        raw = dict(zip(COLS, fields))
        r = {k: v for k, v in zip([k for k in COLS if k != "reason"], integers([raw[k] for k in COLS if k != "reason"]))}
        wide = {"runtime", "context", "ctas", "stride", "shared", "row_bytes", "required", "reserved", "limit", "quantum"}
        require(all(v <= 2**32-1 for k, v in r.items() if k not in wide), "plan UINT32 overflow")
        identity = tuple(raw[k] for k in ("pu", "node", "result"))
        require(identity in joins and identity == tuple(targets[0][k] for k in ("pu", "node", "result")) and
                raw["source_line"] == joins[identity]["source_line"] and r["target_status"] == int(targets[0]["status"]), "invalid source/target join")
        ranges = []
        for i, line in enumerate(chunk[4:8]):
            fields = line.split()
            require(fields[:3] == ["AI_NORM_RANGE", "1", "RANGE"] and len(fields) == 3+len(RANGES), "invalid range schema")
            a = dict(zip(RANGES, integers(fields[3:])))
            require(all(a[k] <= 2**32-1 for k in ("index", "alignment", "lease", "source_line")), "range UINT32 overflow")
            require(a["index"] == i and a["source_line"] == r["source_line"], "invalid range identity")
            ranges.append(a)
        edges = []
        for i, line in enumerate(chunk[8:]):
            fields = line.split()
            require(fields[:3] == ["AI_NORM_EVENT", "1", "EDGE"] and len(fields) == 6, "invalid event schema")
            index, mask, lineno = integers(fields[3:])
            require(index == i and lineno == r["source_line"] and mask <= 2**32-1, "invalid event identity")
            edges.append(mask)
        status, expected, sizes = evaluate(r, ranges, edges, joins[identity], hw, r["target_status"])
        require(r["status"] == status and raw["reason"] == REASONS[status] and
                all(r[k] == expected[k] for k in DERIVED) and [a["bytes"] for a in ranges] == sizes,
                "invented status/resource/event proof")
        results.append(dict(raw, ranges=ranges, edges=edges))
    require(results, "missing plan queries")
    return results


def write_reports(trace, source, output):
    """Summarize actual alternatives, ranges/banks/events and open obligations."""
    rows = audit(trace.read_text(), source.read_text())
    output.mkdir(parents=True, exist_ok=False)
    alternatives = "# What it summarizes: native C0/C1/C2 source/target/storage/event eligibility and refusals, never profitability.\n"
    alternatives += "PLAN_COLUMNS pu node result kind reason stages obligations source_line\n"
    residency = "# What it summarizes: canonical byte ranges, leases, row/scalar arena, target capacity rounding and scalar bank mapping.\n"
    residency += "RANGE_COLUMNS pu node result kind ordinal allocation capacity offset alignment lease required_bytes source_line\n"
    pipeline = "# What it summarizes: ordered statistics, X-to-d generation lifetime, uniform visibility and reader-completion-before-reuse.\n"
    pipeline += "EVENT_INDEX " + " ".join(str(i)+"="+name for i,name in enumerate(EVENTS)) + "\n"
    for row in rows:
        identity = " ".join(row[k] for k in ("pu", "node", "result", "kind"))
        alternatives += "PLAN " + identity + " " + row["reason"] + " stages=" + row["stages"] + " obligations=" + row["obligations"] + " " + row["source_line"] + "\n"
        residency += "STORAGE " + identity + " " + " ".join(k+"="+row[k] for k in ("row_bytes", "required", "reserved", "limit", "quantum", "banks", "bank_word", "conflict")) + " " + row["source_line"] + "\n"
        for a in row["ranges"]:
            residency += "RANGE " + identity + " " + " ".join(str(a[k]) for k in RANGES) + "\n"
        pipeline += "EVENTS " + identity + " barriers=" + row["barriers"] + " participants=" + row["participants"] + " successors=" + ",".join(str(x) for x in row["edges"]) + " " + row["source_line"] + "\n"
    for name, text in (("alternatives.log", alternatives), ("residency.log", residency), ("fetch-pipeline.log", pipeline)):
        (output / name).write_text(text)
    (output / "optimization.log").write_text("# What it summarizes: full successful LN2 alternatives, residency and event report collection.\n"+alternatives+residency+pipeline)
    (output / "optimization-summary.log").write_text("# What it summarizes: conditional planning coverage, runtime/lowering obligations and no selection or transformation.\n"+
        f"queries={len(rows)} conditional_bound={sum(r['status']=='0' for r in rows)} refused={sum(r['status']!='0' for r in rows)}\n"+
        "runtime_enforcement=unproved grid_limit=unbound registers=unbound occupancy=unmodeled async=not_supported\n"+
        "COST none DECISION none TRANSFORM none v1_migration=none GPU=not_run\n")
    return rows


def main():
    """Reaudit raw positional trace with independent PREOPT source facts."""
    parser = argparse.ArgumentParser()
    parser.add_argument("--trace", required=True, type=Path)
    parser.add_argument("--source", required=True, type=Path)
    parser.add_argument("--output", required=True, type=Path)
    args = parser.parse_args()
    write_reports(args.trace, args.source, args.output)


if __name__ == "__main__":
    main()
