#!/usr/bin/env bash
#
# Certify the common REGION substrate and its pre-LNO ownership boundary.

set -euo pipefail

script_dir="$(cd "$(dirname "$0")" && pwd)"
repo_root="$(cd "$script_dir/../../../.." && pwd)"
contract_test="${OPEN64_DSL_CONTRACT_TEST:-$repo_root/build/osprey/targdir/ir_tools/dsl_builder_contract_test}"
lower_test="${OPEN64_DSL_LOWER_TEST:-$repo_root/build/osprey/targdir/be/dsl_lower_contract_test}"
ir_b2a="${OPEN64_IR_B2A:-$repo_root/build/osprey/targdir/ir_tools/ir_b2a}"
artifact_dir="${OPEN64_DSL_REGION_ARTIFACT_DIR:-${TMPDIR:-/tmp}/open64-region-substrate.$$}"

require_executable()
{
  if [[ ! -x "$1" ]]; then
    echo "missing executable: $1" >&2
    exit 1
  fi
}

require_text()
{
  local file="$1"
  local pattern="$2"

  if ! grep -Fq "$pattern" "$file"; then
    echo "missing REGION evidence '$pattern' in $file" >&2
    exit 1
  fi
}

reject_private_escape()
{
  local file="$1"

  if grep -Eq '(^|[[:space:]])(OPR_DSL|MDSL)([[:space:]]|$)' "$file"; then
    echo "private physical DSL escape leaked into $file" >&2
    exit 1
  fi
}

require_executable "$contract_test"
require_executable "$lower_test"
require_executable "$ir_b2a"

mkdir -p "$artifact_dir"
find "$artifact_dir" -mindepth 1 -maxdepth 1 -type f -delete

region_image="$artifact_dir/region_substrate.B"
region_trace="$artifact_dir/region_substrate.T"
lower_trace="$artifact_dir/region_substrate_lowered.T"

(cd "$(dirname "$contract_test")" && \
  OPEN64_DSL_LLAMA2_REGIONS_ONLY=1 \
  OPEN64_DSL_LLAMA2_REGION_ARTIFACT="$region_image" \
    "./$(basename "$contract_test")")

(cd "$(dirname "$ir_b2a")" && \
  "./$(basename "$ir_b2a")" -st -src "$region_image") \
  > "$region_trace"

for evidence in \
  "DSL REGION TABLE:" \
  "contract=transformer.prefill.v1" \
  "contract=transformer.decoder_layer.v1" \
  "METADATA module_path:TinyLlama.forward" \
  "VALUE ordinal=0" \
  "roles=0x1" \
  "roles=0xa" \
  "LOC 0 40"; do
  require_text "$region_trace" "$evidence"
done
reject_private_escape "$region_trace"

(cd "$(dirname "$lower_test")" && \
  OPEN64_DSL_REGION_SUBSTRATE_ONLY=1 \
  OPEN64_DSL_REGION_LOWER_TRACE="$lower_trace" \
    "./$(basename "$lower_test")")

require_text "$lower_trace" "WHIRL after managed DSL REGION body splicing"
require_text "$lower_trace" \
  "managed_regions=0 canonical_regions=1 WT_REGIONS=missing"
require_text "$lower_trace" "REGION"
reject_private_escape "$lower_trace"

echo "common REGION substrate compatibility fixture passed"
echo "review artifacts: $artifact_dir"
