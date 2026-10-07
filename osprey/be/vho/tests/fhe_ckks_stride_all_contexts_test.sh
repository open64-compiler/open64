#!/usr/bin/env bash
# Certify four replay-bound stride-two clear-slot contexts and fail-closed input checks.
# Design: doc/FHE-SYNC6-S6-0C-DETAILED-EXECUTION-PLAN.md.
set -euo pipefail

repo_root=$(cd "$(dirname "$0")/../../../.." && pwd)
out_dir=${1:?artifact directory required}
budget=${2:?authenticated Conv budget required}
trace=${3:?ir_b2a trace required}
payload=${4:?folded SafeTensors required}
replay=${5:?source replay required}
proof="$repo_root/osprey/be/vho/tests/fhe_ckks_stride_all_contexts_proof.py"
schedule="$repo_root/doc/fhe-policy/sync3-relu/ckks-schedule-manifest.json"

mkdir -p "$out_dir"
rm -f "$out_dir"/*.json "$out_dir"/*.log
for name in first repeated; do
    python3 "$proof" --budget "$budget" --trace "$trace" \
        --payload "$payload" --replay "$replay" \
        --ckks-schedule "$schedule" \
        --output "$out_dir/$name.json" >"$out_dir/$name.log" 2>&1
done
cmp "$out_dir/first.json" "$out_dir/repeated.json"

python3 - "$budget" "$out_dir" <<'PY'
import json
import sys
from pathlib import Path

source = json.loads(Path(sys.argv[1]).read_text(encoding="utf-8"))
out = Path(sys.argv[2])
cases = {}
changed = json.loads(json.dumps(source))
changed["source_trace_sha256"] = "0" * 64
cases["wrong-source-hash"] = changed
changed = json.loads(json.dumps(source))
stride = [row for row in changed["contexts"]
          if not row["bounded_stride_one_recipe"]]
stride[1]["fold_id"] = stride[0]["fold_id"]
cases["duplicate-fold-id"] = changed
changed = json.loads(json.dumps(source))
stride = [row for row in changed["contexts"]
          if not row["bounded_stride_one_recipe"]]
stride[0]["weight_sha256"] = "0" * 64
cases["wrong-weight-hash"] = changed
changed = json.loads(json.dumps(source))
stride = [row for row in changed["contexts"]
          if not row["bounded_stride_one_recipe"]]
stride[0]["bounded_stride_one_recipe"] = True
cases["missing-context"] = changed
changed = json.loads(json.dumps(source))
stride = [row for row in changed["contexts"]
          if not row["bounded_stride_one_recipe"]]
stride[0]["callsite"] = 999
cases["unknown-context"] = changed
for name, budget in cases.items():
    (out / f"{name}.json").write_text(
        json.dumps(budget, indent=2) + "\n", encoding="utf-8")
PY

for name in wrong-source-hash duplicate-fold-id wrong-weight-hash \
        missing-context unknown-context; do
    if python3 "$proof" --budget "$out_dir/$name.json" --trace "$trace" \
            --payload "$payload" --replay "$replay" \
            --ckks-schedule "$schedule" \
            --output "$out_dir/$name.output.json" \
            >"$out_dir/$name.log" 2>&1; then
        echo "tampered $name budget unexpectedly passed" >&2
        exit 1
    fi
    test ! -e "$out_dir/$name.output.json"
done

mkdir -p "$out_dir/tampered"
cp "$payload" "$out_dir/tampered/$(basename "$payload")"
printf x >>"$out_dir/tampered/$(basename "$payload")"
if python3 "$proof" --budget "$budget" --trace "$trace" \
        --payload "$out_dir/tampered/$(basename "$payload")" \
        --replay "$replay" --ckks-schedule "$schedule" \
        --output "$out_dir/tampered.output.json" \
        >"$out_dir/tampered.log" 2>&1; then
    echo "tampered folded payload unexpectedly passed" >&2
    exit 1
fi
test ! -e "$out_dir/tampered.output.json"

python3 - "$schedule" "$out_dir/wrong-schedule.json" <<'PY'
import json
import sys
from pathlib import Path

manifest = json.loads(Path(sys.argv[1]).read_text(encoding="utf-8"))
for row in manifest["contexts"]:
    if row["instance_path"] == "layer2.0.relu1":
        row["final_level"] = 5
Path(sys.argv[2]).write_text(json.dumps(manifest) + "\n", encoding="utf-8")
PY
if python3 "$proof" --budget "$budget" --trace "$trace" \
        --payload "$payload" --replay "$replay" \
        --ckks-schedule "$out_dir/wrong-schedule.json" \
        --output "$out_dir/wrong-schedule.output.json" \
        >"$out_dir/wrong-schedule.log" 2>&1; then
    echo "wrong CKKS level schedule unexpectedly passed" >&2
    exit 1
fi
test ! -e "$out_dir/wrong-schedule.output.json"

sha256sum "$out_dir/first.json" >"$out_dir/SHA256SUMS"
printf 'Four authenticated stride-two clear-slot contexts passed.\n'
printf 'Evidence: %s\n' "$out_dir/first.json"
