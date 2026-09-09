#!/usr/bin/env bash
#
# Focused contract for the version-1 FHE tensor binding identity and the
# flags == 0 requirement.  The test drives the existing native builder
# contract binary in its dedicated v1 tensor binding mode.

set -euo pipefail

script_dir="$(cd "$(dirname "$0")" && pwd)"
repo_root="$(cd "$script_dir/../../../.." && pwd)"
contract_test="${OPEN64_DSL_CONTRACT_TEST:-$repo_root/build/osprey/targdir/ir_tools/dsl_builder_contract_test}"

if [[ ! -x "$contract_test" ]]; then
  echo "missing executable: $contract_test" >&2
  exit 1
fi

log="${OPEN64_DSL_FHE_BINDING_LOG:-$repo_root/artifacts/fhe/tensor_binding_v1/binding_v1.log}"
mkdir -p "$(dirname "$log")"

OPEN64_DSL_FHE_BINDING_ONLY=1 "$contract_test" >"$log" 2>&1

if ! grep -Fq "FHE tensor binding v1 identity contract passed" "$log"; then
  echo "FHE tensor binding v1 identity contract did not pass" >&2
  cat "$log" >&2
  exit 1
fi

echo "FHE tensor binding v1 identity contract passed"
echo "review log: $log"
