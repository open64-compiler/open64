#!/usr/bin/env bash
#
# Focused contract for failure-atomic FHE entry value insertion.  The test
# drives the existing native builder contract binary in its dedicated entry
# value atomicity mode and confirms that a rejected insertion leaves both the
# entry value table and the entry contract exactly as they were.

set -euo pipefail

script_dir="$(cd "$(dirname "$0")" && pwd)"
repo_root="$(cd "$script_dir/../../../.." && pwd)"
contract_test="${OPEN64_DSL_CONTRACT_TEST:-$repo_root/build/osprey/targdir/ir_tools/dsl_builder_contract_test}"

if [[ ! -x "$contract_test" ]]; then
  echo "missing executable: $contract_test" >&2
  exit 1
fi

log="${OPEN64_DSL_FHE_ENTRY_ATOMICITY_LOG:-$repo_root/artifacts/fhe/entry_value_atomicity/entry_atomicity.log}"
mkdir -p "$(dirname "$log")"

OPEN64_DSL_FHE_ENTRY_ATOMICITY_ONLY=1 "$contract_test" >"$log" 2>&1

if ! grep -Fq "FHE entry value insertion atomicity contract passed" "$log"; then
  echo "FHE entry value insertion atomicity contract did not pass" >&2
  cat "$log" >&2
  exit 1
fi

echo "FHE entry value insertion atomicity contract passed"
echo "review log: $log"
