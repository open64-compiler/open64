#!/usr/bin/env bash

set -euo pipefail

script_dir="$(cd "$(dirname "$0")" && pwd)"
repo_root="$(cd "$script_dir/../../../.." && pwd)"
be="${OPEN64_FHE_COMMIT19_BE:-}"
ir_b2a="${OPEN64_FHE_COMMIT19_IR_B2A:-}"
input_binary="${OPEN64_FHE_COMMIT19_INPUT_BINARY:-}"
input_trace="${OPEN64_FHE_COMMIT19_INPUT_TRACE:-}"
input_source="${OPEN64_FHE_COMMIT19_INPUT_SOURCE:-}"
input_payload="${OPEN64_FHE_COMMIT19_INPUT_PAYLOAD:-}"
checkpoint="${OPEN64_FHE_COMMIT19_CHECKPOINT:-}"
range_manifest="${OPEN64_FHE_COMMIT19_RANGE_MANIFEST:-$repo_root/doc/fhe-policy/sync3-relu/range-manifest.json}"
policy_dir="${OPEN64_FHE_COMMIT19_POLICY_DIR:-$repo_root/doc/fhe-policy/sync3-relu}"
artifact_dir="${OPEN64_FHE_COMMIT19_ARTIFACT_DIR:-$repo_root/artifacts/fhe/sync3_commit19_certification}"
python="${OPEN64_FHE_COMMIT19_PYTHON:-python3}"

for executable in "$be" "$ir_b2a"; do
  if [[ ! -x "$executable" ]]; then
    echo "missing Commit 19 executable: $executable" >&2
    exit 1
  fi
done
for input in "$input_binary" "$input_trace" "$input_source" \
             "$input_payload" "$checkpoint" "$range_manifest"; do
  if [[ ! -f "$input" ]]; then
    echo "missing Commit 19 input: $input" >&2
    exit 1
  fi
done
if [[ "$(basename "$input_payload")" != "secure_resnet20.safetensors" ]]; then
  echo "Commit 19 payload must be named secure_resnet20.safetensors" >&2
  exit 1
fi

mkdir -p "$artifact_dir"
find "$artifact_dir" -mindepth 1 -maxdepth 1 -type f -delete
cp "$input_binary" "$artifact_dir/secure_resnet20.B"
cp "$input_trace" "$artifact_dir/secure_resnet20.T"
cp "$input_source" "$artifact_dir/secure_resnet20.py"
cp "$input_payload" "$artifact_dir/secure_resnet20.safetensors"
cp "$range_manifest" "$artifact_dir/approved-range-manifest.json"

range_sha256="$(sha256sum "$range_manifest" | awk '{print $1}')"
be_library_dir="$(dirname "$be")"
output="secure_resnet20.fhe.B"
phase_trace="secure_resnet20.fhe.vho.t"

printf '%s\n' \
  "$be -FHE:checkpoint=$output -FHE:convert=on:strict_o0=on:dump_before=on:dump_after=on:calibration_manifest=approved-range-manifest.json:calibration_sha256=$range_sha256 -ft,$phase_trace secure_resnet20.B" \
  "$ir_b2a -st -src secure_resnet20.fhe.B secure_resnet20.fhe.T" \
  "$python $script_dir/fhe_commit19_certification.py $artifact_dir --policy-dir $policy_dir --checkpoint $checkpoint" \
  >"$artifact_dir/commands.txt"

(
  cd "$artifact_dir"
  LD_LIBRARY_PATH="$be_library_dir${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}" \
    "$be" \
      -FHE:checkpoint="$output" \
      -FHE:convert=on:strict_o0=on:dump_before=on:dump_after=on:calibration_manifest=approved-range-manifest.json:calibration_sha256="$range_sha256" \
      -ft,"$phase_trace" \
      secure_resnet20.B >conversion.log 2>&1
  "$ir_b2a" -st -src secure_resnet20.fhe.B secure_resnet20.fhe.T
)

"$python" "$script_dir/fhe_commit19_certification.py" \
  "$artifact_dir" --policy-dir "$policy_dir" --checkpoint "$checkpoint" \
  >"$artifact_dir/independent-verifier.log" 2>&1

(
  cd "$artifact_dir"
  if LD_LIBRARY_PATH="$be_library_dir${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}" \
      "$be" -FHE:checkpoint=missing-manifest.fhe.B \
      -FHE:convert=on:strict_o0=on secure_resnet20.B \
      >missing-manifest.log 2>&1; then
    echo "missing-manifest conversion unexpectedly passed" >&2
    exit 1
  fi
  if [[ -e missing-manifest.fhe.B || -e missing-manifest.fhe.B.tmp ]] || \
      ! grep -Fq CFHECNN-RELU-003 missing-manifest.log; then
    echo "missing-manifest rejection did not fail atomically" >&2
    exit 1
  fi

  bad_digest="$(printf '0%.0s' {1..64})"
  if LD_LIBRARY_PATH="$be_library_dir${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}" \
      "$be" -FHE:checkpoint=bad-digest.fhe.B \
      -FHE:convert=on:strict_o0=on:calibration_manifest=approved-range-manifest.json:calibration_sha256="$bad_digest" \
      secure_resnet20.B >bad-digest.log 2>&1; then
    echo "bad-digest conversion unexpectedly passed" >&2
    exit 1
  fi
  if [[ -e bad-digest.fhe.B || -e bad-digest.fhe.B.tmp ]] || \
      ! grep -Fq CFHECNN-RELU-004 bad-digest.log; then
    echo "bad-digest rejection did not fail atomically" >&2
    exit 1
  fi

  "$python" - approved-range-manifest.json bad-route-manifest.json <<'PY'
import copy
import hashlib
import json
import sys

source, destination = sys.argv[1:]
with open(source, "r", encoding="ascii") as stream:
    manifest = json.load(stream)
manifest["contexts"][1]["module_path"] = "layer1.1.relu"
content = copy.deepcopy(manifest)
content.pop("manifest_sha256", None)
canonical = (
    json.dumps(content, sort_keys=True, separators=(",", ":"),
               ensure_ascii=True) + "\n"
).encode("ascii")
manifest["manifest_sha256"] = hashlib.sha256(canonical).hexdigest()
with open(destination, "wb") as stream:
    stream.write(
        (json.dumps(manifest, sort_keys=True, separators=(",", ":"),
                    ensure_ascii=True) + "\n").encode("ascii")
    )
PY
  bad_route_sha256="$(sha256sum bad-route-manifest.json | awk '{print $1}')"
  if LD_LIBRARY_PATH="$be_library_dir${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}" \
      "$be" -FHE:checkpoint=bad-route.fhe.B \
      -FHE:convert=on:strict_o0=on:calibration_manifest=bad-route-manifest.json:calibration_sha256="$bad_route_sha256" \
      secure_resnet20.B >bad-route.log 2>&1; then
    echo "bad-route conversion unexpectedly passed" >&2
    exit 1
  fi
  if [[ -e bad-route.fhe.B || -e bad-route.fhe.B.tmp ]] || \
      ! grep -Fq CFHECNN-RELU-005 bad-route.log; then
    echo "bad-route rejection did not fail atomically" >&2
    exit 1
  fi

  cp secure_resnet20.safetensors secure_resnet20.safetensors.saved
  "$python" - secure_resnet20.safetensors <<'PY'
from pathlib import Path
import sys

path = Path(sys.argv[1])
payload = bytearray(path.read_bytes())
payload[-1] ^= 1
path.write_bytes(payload)
PY
  set +e
  LD_LIBRARY_PATH="$be_library_dir${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}" \
    "$be" -FHE:checkpoint=changed-payload.fhe.B \
    -FHE:convert=on:strict_o0=on:calibration_manifest=approved-range-manifest.json:calibration_sha256="$range_sha256" \
    secure_resnet20.B >changed-payload.log 2>&1
  changed_payload_status=$?
  set -e
  mv secure_resnet20.safetensors.saved secure_resnet20.safetensors
  if [[ $changed_payload_status -eq 0 ]]; then
    echo "changed-payload conversion unexpectedly passed" >&2
    exit 1
  fi
  if [[ -e changed-payload.fhe.B || -e changed-payload.fhe.B.tmp ]] || \
      ! grep -Fq CFHECNN-RELU-007 changed-payload.log; then
    echo "changed-payload rejection did not fail atomically" >&2
    exit 1
  fi

  binary_sha256="$(sha256sum secure_resnet20.fhe.B | awk '{print $1}')"
  if LD_LIBRARY_PATH="$be_library_dir${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}" \
      "$be" -FHE:checkpoint=secure_resnet20.fhe.B \
      -FHE:convert=on:strict_o0=on:calibration_manifest=approved-range-manifest.json:calibration_sha256="$range_sha256" \
      secure_resnet20.B >stale-output.log 2>&1; then
    echo "stale-output conversion unexpectedly passed" >&2
    exit 1
  fi
  if [[ -e secure_resnet20.fhe.B.tmp ]] || \
      [[ "$(sha256sum secure_resnet20.fhe.B | awk '{print $1}')" != \
         "$binary_sha256" ]] || \
      ! grep -Fq CFHE-CHECKPOINT-006 stale-output.log; then
    echo "stale-output rejection changed the committed artifact" >&2
    exit 1
  fi

  sha256sum \
    secure_resnet20.B secure_resnet20.T secure_resnet20.py \
    secure_resnet20.safetensors secure_resnet20.fhe.B \
    secure_resnet20.fhe.T secure_resnet20.fhe.safetensors \
    secure_resnet20.fhe.conversion-report.txt secure_resnet20.fhe.vho.t \
    conversion.log independent-verifier.log >SHA256SUMS
)

echo "FHE SYNC-3 Commit 19 checkpoint certification passed"
echo "review artifacts: $artifact_dir"
