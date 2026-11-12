"""Audit native LNO partition facts; derive reports without inventing proofs.

Design: doc/AI-COMPILER-NATIVE-PARTITION-AXIS-v0.1.md.
This is a compile-time trace tool, not the deferred performance harness.
"""
from pathlib import Path

COLUMNS = ("phase pu region status instance_iv instance_offset contribution_iv "
           "contribution_offset instance_axis contribution_axis instances "
           "contributions recipe self_operand order source_line").split()
STATUSES = {"proved", "region_unsupported", "domain_unproved",
            "recurrence_unproved", "def_use_unproved", "access_unproved",
            "alias_unproved"}
RECIPES = {"unknown", "ordered_sum", "ordered_squared_sum"}


def audit(text):
    """Validate schemas, domain invariants and one complete summary per active PU."""
    rows, summaries, keys = [], {}, set()
    header = False
    for line in text.splitlines():
        if not line.startswith("AI_PARTITION "):
            continue
        parts = line.split()
        if len(parts) < 4 or parts[:2] != ["AI_PARTITION", "1"]:
            raise ValueError("unsupported partition trace")
        if parts[2] == "HEADER":
            if parts[3:] != COLUMNS:
                raise ValueError("partition header mismatch")
            header = True
        elif parts[2] == "FACT":
            if not header or len(parts[3:]) != len(COLUMNS):
                raise ValueError("partition row/header incomplete")
            row = dict(zip(COLUMNS, parts[3:]))
            if (row["phase"] != "lno_discovery" or row["status"] not in STATUSES or
                    row["recipe"] not in RECIPES or
                    any(not row[k].isdigit() for k in ("pu", "region", "source_line")) or
                    int(row["pu"]) <= 0 or int(row["region"]) <= 0):
                raise ValueError("partition identity/status invalid")
            key = (row["pu"], row["region"])
            if key in keys or row["pu"] in summaries:
                raise ValueError("duplicate or post-summary partition fact")
            keys.add(key)
            if row["status"] == "proved":
                numeric = COLUMNS[4:12] + ["self_operand"]
                if any(not row[k].isdigit() for k in numeric):
                    raise ValueError("proved partition has unknown values")
                if (sorted([int(row["instance_axis"]), int(row["contribution_axis"])]) != [0, 1] or
                        any(int(row[k]) <= 0 for k in ("instances", "contributions", "instance_iv", "contribution_iv")) or
                        any(int(row[k]) >= 2147483647 for k in ("instances", "contributions")) or
                        row["self_operand"] not in ("0", "1") or
                        row["recipe"] == "unknown" or row["order"] != "ordered" or
                        (row["instance_iv"], row["instance_offset"]) ==
                        (row["contribution_iv"], row["contribution_offset"])):
                    raise ValueError("proved partition domain invalid")
            elif (any(row[k] != "-" for k in COLUMNS[4:12]) or
                  row["self_operand"] != "-" or row["order"] != "unknown"):
                raise ValueError("refusal grants a partial proved domain")
            rows.append(row)
        elif parts[2] == "SUMMARY":
            if not header or len(parts) != 7 or any(not x.isdigit() for x in parts[3:]):
                raise ValueError("partition summary invalid")
            pu, total, proved, refused = parts[3:]
            if pu in summaries or int(pu) <= 0:
                raise ValueError("duplicate partition summary")
            matching = [r for r in rows if r["pu"] == pu]
            count = sum(r["status"] == "proved" for r in matching)
            if (int(total), int(proved), int(refused)) != (len(matching), count, len(matching) - count):
                raise ValueError("partition summary does not match facts")
            if sorted(int(r["region"]) for r in matching) != list(range(1, len(matching) + 1)):
                raise ValueError("partition region numbering incomplete")
            summaries[pu] = (int(total), int(proved), int(refused))
        else:
            raise ValueError("unknown partition record")
    if (header and not summaries) or any(row["pu"] not in summaries for row in rows):
        raise ValueError("partition summary missing")
    return rows


def write_reports(trace, output):
    """Publish concise semantic and aggregate reports in a fresh destination."""
    rows = audit(Path(trace).read_text())
    output = Path(output)
    output.mkdir(parents=True, exist_ok=False)
    semantic = ("# What it summarizes: native scalar recurrence instance/contribution axes, "
                "cardinalities, source lines and explicit proof refusals.\n" +
                "# AI_PARTITION schema=1 columns: " + " ".join(COLUMNS) + "\n" +
                "".join(" ".join(row[k] for k in COLUMNS) + "\n" for row in rows))
    (output / "semantic.log").write_text(semantic)
    (output / "optimization.log").write_text(
        "# What it summarizes: complete successful compile-time partition discovery reports.\n" + semantic)
    (output / "optimization-summary.log").write_text(
        "# What it summarizes: native axis recognition, refusals and unimplemented downstream gates.\n" +
        f"native_discovery candidates={len(rows)} proved={sum(r['status'] == 'proved' for r in rows)} " +
        f"refused={sum(r['status'] != 'proved' for r in rows)} origin=canonical_scalar_whirl\n" +
        "ownership_selection=not_run profitability=not_run transformation=none "
        "layernorm_recognition=not_implemented numerical_binding=not_inferred native_gpu=not_run\n")
    return rows
