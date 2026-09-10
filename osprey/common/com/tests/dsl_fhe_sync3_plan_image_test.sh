#!/usr/bin/env bash
#
# Preserve and inspect the SYNC-3 FHE conversion-planning image contract.

set -euo pipefail

script_dir="$(cd "$(dirname "$0")" && pwd)"
repo_root="$(cd "$script_dir/../../../.." && pwd)"
contract_test="${OPEN64_DSL_CONTRACT_TEST:-$repo_root/build/osprey/targdir/ir_tools/dsl_builder_contract_test}"
ir_b2a="${OPEN64_IR_B2A:-$repo_root/build/osprey/targdir/ir_tools/ir_b2a}"
artifact_dir="${OPEN64_DSL_FHE_SYNC3_ARTIFACT_DIR:-$repo_root/artifacts/fhe/sync3-plan}"
image="$artifact_dir/fhe_sync3_plan_contract.B"
trace="$artifact_dir/fhe_sync3_plan_contract.T"
command_log="$artifact_dir/commands.txt"
validation_log="$artifact_dir/validation.log"

for executable in "$contract_test" "$ir_b2a"; do
  if [[ ! -x "$executable" ]]; then
    echo "missing executable: $executable" >&2
    exit 1
  fi
done

mkdir -p "$artifact_dir"
find "$artifact_dir" -mindepth 1 -maxdepth 1 -type f -delete

printf '%s\n' \
  "OPEN64_DSL_FHE_SYNC3_PLAN_ONLY=1 OPEN64_DSL_FHE_SYNC3_ARTIFACT=$image $contract_test" \
  "$ir_b2a -st -src $image $trace" >"$command_log"

if ! (cd "$repo_root" && \
  OPEN64_DSL_FHE_SYNC3_PLAN_ONLY=1 \
  OPEN64_DSL_FHE_SYNC3_ARTIFACT="$image" \
    "$contract_test") >"$validation_log" 2>&1; then
  cat "$validation_log" >&2
  exit 1
fi
if ! (cd "$repo_root" && \
  "$ir_b2a" -st -src "$image" "$trace") >>"$validation_log" 2>&1; then
  cat "$validation_log" >&2
  exit 1
fi

for evidence in \
  'FHE Plan Image: version=1 capabilities=0x0000000f' \
  'FHE Conversion Disposition Table:' \
  'source_operator=OPR_DSLCONV2D' \
  'wrapper=fhe.cnn.conv2d.v1' \
  'source_operator=OPR_DSLRELU' \
  'disposition=require_approximation' \
  'FHE Approximation Contract Table:' \
  'polynomial=relu_minimax_degree3.v1' \
  'disposition=require_composite_approximation' \
  'composite_profile=1' \
  'FHE Composite Approximation Profile Image: version=1 capabilities=0x0000000f' \
  'profile=ace.chebyshev.sign.7x15x13.depth11.v1' \
  'FHE Ordered Approximation Stage Table:' \
  'ordinal=0 family=chebyshev basis=chebyshev degree=7' \
  'ordinal=1 family=chebyshev basis=chebyshev degree=15' \
  'ordinal=2 family=chebyshev basis=chebyshev degree=13' \
  'FHE Composite Approximation Association Table:' \
  'FHE ReLU Context Range Table:' \
  'out_of_range=reject provenance=fhe.sync-c.root-range.v1' \
  'FHE CKKS Value State Table:' \
  'level=<pending>' \
  'bootstrap_reason=pre_relu_refresh' \
  'FHE BatchNorm Fold Provenance Table:' \
  'context=FHEResNet.forward' \
  'dsl_builder_contract_test.cxx'; do
  if ! grep -Fq "$evidence" "$trace"; then
    echo "missing FHE SYNC-3 evidence '$evidence' in $trace" >&2
    exit 1
  fi
done

if grep -Fq 'OPR_DSL ' "$trace"; then
  echo "physical DSL escape tag leaked into $trace" >&2
  exit 1
fi

echo "FHE SYNC-3 planning-image fixture passed"
echo "review image: $image"
echo "review trace: $trace"
echo "review commands: $command_log"
echo "review diagnostics: $validation_log"
