#!/usr/bin/env python3
"""Authenticate the S6-0c source family before executable CKKS planning.

This is independent artifact evidence, not the native mapped-image gate or
proof of a CKKS circuit. See doc/FHE-SYNC6-S6-0C-DETAILED-EXECUTION-PLAN.md.
"""

import argparse
import hashlib
import json
import math
import re
import struct
import subprocess
import sys
import tempfile
from pathlib import Path
from fhe_ckks_real_event_census import read_trace


REFERENCE = re.compile(
    r"safetensors://([^#?\s]+)#([^?\s]+)\?offset=(\d+)&length=(\d+)"
    r"&checksum=([0-9a-f]{64})")
URI = re.compile(r"safetensors://[^\s\";\])]+")
SOURCE_LINE = re.compile(r"^ LOC [1-9]\d* [1-9]\d* .+\S$", re.MULTILINE)
SOURCE_TEXT = re.compile(r"^ LOC 1 (\d+) (.*)$", re.MULTILINE)
CONFIG = re.compile(r"^FHE Compilation Configuration Table:\n"
                    r"((?:  \[\d+\] .+\n)+)", re.MULTILINE)
OWNER = re.compile(r"<1,(\d+)>")
ROLES = ("binary", "trace", "source", "source_payload", "folded_payload",
         "coefficient_manifest", "range_manifest", "schedule")


def require(condition, message):
    if not condition:
        raise ValueError(message)


def sha256(path):
    digest = hashlib.sha256()
    with path.open("rb") as stream:
        for block in iter(lambda: stream.read(1024 * 1024), b""):
            digest.update(block)
    return digest.hexdigest()


def check_embedded_manifest_hash(raw, manifest):
    embedded = manifest.get("manifest_sha256")
    require(isinstance(embedded, str) and
            re.fullmatch(r"[0-9a-f]{64}", embedded),
            "range manifest has no canonical-content hash")
    field = b'"manifest_sha256":"' + embedded.encode("ascii") + b'"'
    first = raw.find(field)
    require(first >= 0 and raw.find(field, first + 1) < 0,
            "range manifest canonical hash field is ambiguous")
    start, end = first, first + len(field)
    if start > 0 and raw[start - 1:start] == b",":
        start -= 1
    elif end < len(raw) and raw[end:end + 1] == b",":
        end += 1
    else:
        raise ValueError("range manifest canonical hash field is malformed")
    require(hashlib.sha256(raw[:start] + raw[end:]).hexdigest() == embedded,
            "range manifest canonical-content hash mismatch")


def safetensors_index(path):
    with path.open("rb") as stream:
        length_bytes = stream.read(8)
        require(len(length_bytes) == 8, f"truncated SafeTensors header: {path}")
        length = struct.unpack("<Q", length_bytes)[0]
        require(0 < length <= 16 * 1024 * 1024,
                f"invalid SafeTensors header length: {path}")
        header_bytes = stream.read(length)
        require(len(header_bytes) == length,
                f"truncated SafeTensors index: {path}")
        header = json.loads(header_bytes)
        require(isinstance(header, dict), f"invalid SafeTensors index: {path}")
        return header, 8 + length


def check_external_references(trace, payloads):
    content = trace.read_text(encoding="utf-8")
    references = set(REFERENCE.findall(content))
    require(references, "no external tensor references in trace")
    parsed_uris = {f"safetensors://{name}#{tensor}?offset={start}"
                   f"&length={size}&checksum={digest}"
                   for name, tensor, start, size, digest in references}
    require(set(URI.findall(content)) == parsed_uris,
            "unparsed or malformed external tensor URI")
    require({name for name, _, _, _, _ in references} == set(payloads),
            "external tensor side-file set does not match supplied payloads")
    indices = {name: safetensors_index(path) for name, path in payloads.items()}
    for name, tensor, start_text, size_text, digest in sorted(references):
        path = payloads[name]
        index, base = indices[name]
        start, size = int(start_text), int(size_text)
        require(size > 0 and tensor in index,
                f"missing or empty external tensor {name}#{tensor}")
        offsets = index[tensor].get("data_offsets")
        require(offsets == [start, start + size],
                f"external tensor range mismatch: {name}#{tensor}")
        with path.open("rb") as stream:
            stream.seek(base + start)
            data = stream.read(size)
        require(len(data) == size and hashlib.sha256(data).hexdigest() == digest,
                f"external tensor checksum mismatch: {name}#{tensor}")
    return len(references)


def check_config(trace_text):
    matches = CONFIG.findall(trace_text)
    require(len(matches) == 1, "missing or duplicated FHE configuration")
    rows = matches[0].splitlines()
    require(len(rows) == 1 and rows[0].startswith("  [1] "),
            "FHE configuration count or ID is invalid")
    fields = {}
    for part in rows[0][6:].split():
        require(part.count("=") == 1, "malformed FHE configuration field")
        key, value = part.split("=", 1)
        require(key and value and key not in fields,
                "duplicated FHE configuration field")
        fields[key] = value
    require(fields.get("scheme") == "ckks" and
            fields.get("ring_dimension") == "65536" and
            fields.get("depth") == "33" and
            fields.get("scale_bits") == "56" and
            fields.get("first_modulus_bits") == "60" and
            fields.get("slots") == "32768" and
            fields.get("bootstrap") == "auto",
            "FHE configuration differs from approved S6-0c source")
    return fields


def run_auditors(schedule, trace):
    scripts = Path(__file__).parent
    with tempfile.TemporaryDirectory(prefix="fhe-ckks-replay-") as temp:
        event_path = Path(temp) / "events.json"
        relu_path = Path(temp) / "relu.json"
        subprocess.run([sys.executable,
                        str(scripts / "fhe_ckks_real_event_census.py"),
                        "--schedule", str(schedule), "--trace", str(trace),
                        "--output", str(event_path)], check=True,
                       capture_output=True, text=True)
        subprocess.run([sys.executable,
                        str(scripts / "fhe_ckks_real_relu_plan_audit.py"),
                        "--events", str(event_path), "--trace", str(trace),
                        "--output", str(relu_path)], check=True,
                       capture_output=True, text=True)
        events = json.loads(event_path.read_text(encoding="utf-8"))
        relu = json.loads(relu_path.read_text(encoding="utf-8"))
    require((events["pu_count"], events["callsite_count"],
             events["static_event_count"], events["dynamic_event_count"],
             relu["relu_contexts"], relu["materialization_operations"]) ==
            (6, 9, 87, 147, 19, 114), "source census is incomplete")
    return events, relu


def check_ranges(path, coefficient_path, source_payload, events, trace):
    raw = path.read_bytes()
    manifest = json.loads(raw)
    check_embedded_manifest_hash(raw, manifest)
    require(manifest.get("schema") == "open64.fhe.relu.context-ranges.v1" and
            manifest.get("status") == "approved",
            "range manifest is not approved")
    require(manifest.get("coefficient_manifest_sha256") ==
            sha256(coefficient_path), "coefficient manifest hash mismatch")
    require(manifest.get("source_artifact", {}).get(
        "parameter_payload_sha256") == sha256(source_payload),
        "range manifest is bound to a different source payload")
    _, _, nodes = read_trace(trace)
    relu_values = {value for operator, value in nodes.values()
                   if operator == "OPR_DSLRELU"}
    expected = {(row["owner_pu_st"], row["source_value_id"],
                 row["context_pu_identity_id"], row["context_callsite_id"])
                for row in events["events"]
                if row["source_value_id"] in relu_values}
    actual = set()
    for row in manifest.get("contexts", []):
        owner = OWNER.fullmatch(row["owner_pu_st"])
        require(owner is not None, "invalid range owner syntax")
        key = ((int(owner.group(1)) << 8) | 1,
               row["source_relu_value_id"], row["context_pu_identity_id"],
               row["context_callsite_id"])
        bound = row["bound_b"]
        require(key not in actual and isinstance(bound, (float, int)) and
                math.isfinite(bound) and bound > 0 and
                bound >= row["observed_abs_max"] and
                row["outlier_count"] == 0 and row["nonfinite_count"] == 0,
                "invalid or duplicate approved ReLU context bound")
        actual.add(key)
    require(len(actual) == 19 and actual == expected,
            "approved ranges do not match exact live ReLU contexts")
    return len(actual)


def certify(paths):
    for role in ROLES:
        require(paths[role].is_file(), f"missing {role} input")
    trace_text = paths["trace"].read_text(encoding="utf-8")
    require(SOURCE_LINE.search(trace_text),
            "ir_b2a trace has no nonzero source interleave")
    source_lines = paths["source"].read_text(encoding="utf-8").splitlines()
    require(paths["source"].name in trace_text.splitlines()[0],
            "ir_b2a trace names a different source file")
    require(any(0 < int(line) <= len(source_lines) and
                content.strip() == source_lines[int(line) - 1].strip()
                for line, content in SOURCE_TEXT.findall(trace_text)),
            "ir_b2a source interleave does not match source bytes")
    config = check_config(trace_text)
    payloads = {paths["source_payload"].name: paths["source_payload"],
                paths["folded_payload"].name: paths["folded_payload"]}
    require(len(payloads) == 2, "source and folded payload names collide")
    reference_count = check_external_references(paths["trace"], payloads)
    events, relu = run_auditors(paths["schedule"], paths["trace"])
    range_count = check_ranges(paths["range_manifest"],
                               paths["coefficient_manifest"],
                               paths["source_payload"], events,
                               paths["trace"])
    return {
        "schema": "open64.fhe.sync6.source-replay.v1",
        "status": "read_only_preflight_not_executable_ckks_ir",
        "inputs": {role: {"basename": paths[role].name,
                          "size": paths[role].stat().st_size,
                          "sha256": sha256(paths[role])}
                   for role in ROLES},
        "source_pus": events["pu_count"],
        "source_config": config,
        "callsites": events["callsite_count"],
        "static_events": events["static_event_count"],
        "dynamic_events": events["dynamic_event_count"],
        "relu_contexts": range_count,
        "relu_plan_operations": relu["materialization_operations"],
        "external_tensor_references": reference_count,
    }


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    for role in ROLES:
        parser.add_argument("--" + role.replace("_", "-"), type=Path,
                            required=True)
    parser.add_argument("--output", type=Path, required=True)
    parser.add_argument("--verify", action="store_true",
                        help="compare with an already frozen replay report")
    args = parser.parse_args()
    paths = {role: getattr(args, role) for role in ROLES}
    report = certify(paths)
    if args.verify:
        require(args.output.is_file(), "frozen replay report is missing")
        require(json.loads(args.output.read_text(encoding="utf-8")) == report,
                "source family differs from frozen replay report")
    else:
        require(not args.output.exists(), "refusing to replace replay report")
        args.output.parent.mkdir(parents=True, exist_ok=True)
        args.output.write_text(json.dumps(report, indent=2) + "\n",
                               encoding="utf-8")
    print("S6-0c source replay: 6 PUs, 9 calls, 87/147 events, "
          f"19 contexts, {report['external_tensor_references']} tensors")


if __name__ == "__main__":
    try:
        main()
    except (ValueError, KeyError, OSError, subprocess.CalledProcessError) as error:
        print(f"CFHEIR-REPLAY-001: {error}", file=sys.stderr)
        sys.exit(1)
