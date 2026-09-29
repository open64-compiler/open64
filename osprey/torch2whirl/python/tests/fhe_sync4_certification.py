"""Independently certify a SYNC-4 ReLU materialization checkpoint."""

from __future__ import annotations

import argparse
from collections import Counter, defaultdict
from pathlib import Path
import re


HEADER_RE = re.compile(
    r"^FHE ReLU Materialization Image: version=1 "
    r"capabilities=0x00000001 contexts=(\d+) operations=(\d+)$",
    re.MULTILINE,
)
OPERATION_RE = re.compile(
    r"^  \[(\d+)\] owner_pu=(\S+) source=value(\d+)\([^\n]+?\) "
    r"context_identity=(\d+) callsite=(\d+) ordinal=(\d+) kind=(\S+) "
    r"profile=(\d+) stage=(\d+) range=(\d+) input_state=(\d+) "
    r"output_state=(\d+) parameter_tcon=(\d+) flags=0x([0-9a-f]+)$",
    re.MULTILINE,
)
REFRESH_STATE_RE = re.compile(
    r"^  \[(\d+)\] owner_pu=(\S+) source=value(\d+)\([^\n]+?\) "
    r"context_identity=(\d+)\([^\n]+?\) callsite=(\d+) role=post_refresh "
    r"state_version=1 encryption=\d+ scheme=ckks class=ciphertext "
    r"level=(\d+) scale_bits=56 components=2 precision_bits=(\d+) "
    r"slots=32768 alignment_group=0 layout=ckks\.packed pending=0x4 "
    r"bootstrap_reason=pre_relu_refresh flags=0x0$",
    re.MULTILINE,
)

EXPECTED_KINDS = (
    "refresh",
    "normalize",
    "approx_stage",
    "approx_stage",
    "approx_stage",
    "reconstruct_relu",
)
EXPECTED_STAGES = (0, 0, 1, 2, 3, 0)


def _read_report(path: Path) -> dict[str, str]:
    values: dict[str, str] = {}
    for line in path.read_text(encoding="utf-8").splitlines()[1:]:
        if "=" not in line:
            continue
        key, value = line.split("=", 1)
        values[key] = value
    return values


def certify_trace(trace_path: Path) -> dict[str, int]:
    text = trace_path.read_text(encoding="utf-8")
    header = HEADER_RE.search(text)
    if header is None:
        raise AssertionError("missing SYNC-4 materialization image header")
    if tuple(map(int, header.groups())) != (19, 114):
        raise AssertionError("materialization header is not 19 contexts / 114 ops")
    if len(re.findall(r"^FUNC_ENTRY ", text, re.MULTILINE)) != 6:
        raise AssertionError("materialized checkpoint does not contain six PUs")

    rows = []
    for match in OPERATION_RE.finditer(text):
        values = match.groups()
        rows.append(
            {
                "id": int(values[0]),
                "owner": values[1],
                "source": int(values[2]),
                "identity": int(values[3]),
                "callsite": int(values[4]),
                "ordinal": int(values[5]),
                "kind": values[6],
                "profile": int(values[7]),
                "stage": int(values[8]),
                "range": int(values[9]),
                "input": int(values[10]),
                "output": int(values[11]),
                "parameter": int(values[12]),
                "flags": int(values[13], 16),
            }
        )
    if len(rows) != 114 or [row["id"] for row in rows] != list(range(1, 115)):
        raise AssertionError("materialization operation IDs are not dense 1..114")
    if {row["output"] for row in rows} != set(range(1, 115)):
        raise AssertionError("materialization output states are not unique 1..114")

    refresh_states = [
        {
            "id": int(match.group(1)),
            "owner": match.group(2),
            "source": int(match.group(3)),
            "identity": int(match.group(4)),
            "callsite": int(match.group(5)),
            "level": int(match.group(6)),
            "precision": int(match.group(7)),
        }
        for match in REFRESH_STATE_RE.finditer(text)
    ]
    if len(refresh_states) != 19:
        raise AssertionError("expected exactly 19 POST_REFRESH.v1 states")
    if Counter(state["level"] for state in refresh_states) != \
            Counter({15: 16, 17: 1, 18: 2}):
        raise AssertionError("post-refresh levels are not distributed 16/1/2")
    if any(state["precision"] < 30 for state in refresh_states):
        raise AssertionError("post-refresh precision is below 30 bits")

    contexts: dict[tuple[str, int, int, int, int], list[dict[str, int | str]]] = \
        defaultdict(list)
    for row in rows:
        key = (
            str(row["owner"]), int(row["source"]), int(row["identity"]),
            int(row["callsite"]), int(row["range"]),
        )
        contexts[key].append(row)
    if len(contexts) != 19:
        raise AssertionError(f"expected 19 context schedules, found {len(contexts)}")
    if {key[4] for key in contexts} != set(range(1, 20)):
        raise AssertionError("materialization range IDs are not exactly 1..19")

    kind_counts: Counter[str] = Counter()
    stage_parameters: dict[int, set[int]] = defaultdict(set)
    refresh_outputs = set()
    for key, operations in contexts.items():
        operations.sort(key=lambda row: int(row["ordinal"]))
        if [row["ordinal"] for row in operations] != list(range(6)):
            raise AssertionError(f"context {key} does not have dense ordinals 0..5")
        kinds = tuple(str(row["kind"]) for row in operations)
        stages = tuple(int(row["stage"]) for row in operations)
        if kinds != EXPECTED_KINDS or stages != EXPECTED_STAGES:
            raise AssertionError(f"context {key} has a reordered materialization chain")
        if any(row["profile"] != 1 or row["flags"] != 0 for row in operations):
            raise AssertionError(f"context {key} has unexpected profile or flags")
        if operations[0]["input"] != 0 or operations[0]["parameter"] != 0:
            raise AssertionError(f"context {key} has an invalid refresh operation")
        if operations[1]["parameter"] == 0:
            raise AssertionError(f"context {key} lacks its normalization bound")
        if any(operations[index]["parameter"] == 0 for index in (2, 3, 4)):
            raise AssertionError(f"context {key} lacks a coefficient tensor")
        if operations[5]["parameter"] != 0:
            raise AssertionError(f"context {key} has an invalid reconstruction")
        refresh_outputs.add(operations[0]["output"])
        for index in (2, 3, 4):
            stage_parameters[index].add(int(operations[index]["parameter"]))
        for previous, current in zip(operations, operations[1:]):
            if previous["output"] != current["input"]:
                raise AssertionError(f"context {key} has disconnected state flow")
        kind_counts.update(kinds)

    expected_counts = Counter(
        refresh=19, normalize=19, approx_stage=57, reconstruct_relu=19
    )
    if kind_counts != expected_counts:
        raise AssertionError(f"unexpected operation census: {dict(kind_counts)}")
    if any(len(values) != 1 for values in stage_parameters.values()) or \
            len({next(iter(values)) for values in stage_parameters.values()}) != 3:
        raise AssertionError("contexts do not share three distinct stage tensors")
    if refresh_outputs != {state["id"] for state in refresh_states}:
        raise AssertionError("refresh operations do not target POST_REFRESH states")
    return {
        "pus": 6,
        "contexts": len(contexts),
        "operations": len(rows),
        "refreshes": kind_counts["refresh"],
    }


def certify_reports(binary_path: Path, mode: str) -> None:
    materialization = _read_report(
        Path(f"{binary_path}.materialization-report.txt")
    )
    capability = _read_report(Path(f"{binary_path}.capability-report.txt"))
    expected = {
        "bootstrap_mode": mode,
        "contexts": "19",
        "operations": "114",
        "refreshes": "19",
        "post_refresh_levels": "15:16,17:1,18:2",
        "profile": "ace.chebyshev.sign.7x15x13.depth11.v1",
        "stage_degrees": "7,15,13",
        "stage_level_consumption": "3,4,4",
    }
    for key, value in expected.items():
        if materialization.get(key) != value:
            raise AssertionError(f"materialization report mismatch for {key}")
    if capability.get("provider") != "ace-ant" or \
            capability.get("runtime") != "FHErt_ant" or \
            capability.get("scope") != "relu-materialization-only":
        raise AssertionError("provider capability report is inconsistent")


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("trace", type=Path)
    parser.add_argument("--binary", type=Path, required=True)
    parser.add_argument("--mode", choices=("auto", "on", "manual"), required=True)
    args = parser.parse_args()
    summary = certify_trace(args.trace)
    certify_reports(args.binary, args.mode)
    print(
        "FHE SYNC-4 certification passed: "
        f"pus={summary['pus']} contexts={summary['contexts']} "
        f"operations={summary['operations']} refreshes={summary['refreshes']}"
    )


if __name__ == "__main__":
    main()
