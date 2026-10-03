#!/usr/bin/env bash
# Retain both primitive and composed PU-specialization WHIRL evidence.

set -euo pipefail

script_dir="$(cd "$(dirname "$0")" && pwd)"
repo_root="$(cd "$script_dir/../../../.." && pwd)"
producer="${OPEN64_DSL_PU_TRANSACTION_TEST:-$repo_root/build/osprey/targdir/ir_tools/dsl_pu_transaction_contract_test}"
ir_b2a="${OPEN64_IR_B2A:-$repo_root/build/osprey/targdir/ir_tools/ir_b2a}"
be="${OPEN64_BE:-}"
artifact_dir="${OPEN64_DSL_PU_TRANSACTION_ARTIFACT_DIR:-$repo_root/artifacts/dsl/pu-transaction}"

for executable in "$producer" "$ir_b2a"; do
  if [[ ! -x "$executable" ]]; then
    echo "missing executable: $executable" >&2
    exit 1
  fi
done

if [[ -n "$be" && ! -x "$be" ]]; then
  echo "missing executable: $be" >&2
  exit 1
fi

mkdir -p "$artifact_dir"
find "$artifact_dir" -mindepth 1 -maxdepth 1 -type f -delete

for mode in primitive apply; do
  image="$artifact_dir/pu_transaction_${mode}.B"
  trace="$artifact_dir/pu_transaction_${mode}.T"
  log="$artifact_dir/pu_transaction_${mode}.log"
  if [[ "$mode" == apply ]]; then
    args=(--apply)
  else
    args=()
  fi
  printf '%s\n' \
    "OPEN64_DSL_PU_TRANSACTION_ARTIFACT=$image $producer ${args[*]}" \
    "$ir_b2a -st -src $image $trace" >>"$artifact_dir/commands.txt"
  if [[ "$mode" == apply ]]; then
    printf '%s\n' "$producer --reopen $image" \
      >>"$artifact_dir/commands.txt"
  fi
  if ! OPEN64_DSL_PU_TRANSACTION_ARTIFACT="$image" \
       "$producer" "${args[@]}" >"$log" 2>&1; then
    cat "$log" >&2
    exit 1
  fi
  if ! "$ir_b2a" -st -src "$image" "$trace" >>"$log" 2>&1; then
    cat "$log" >&2
    exit 1
  fi
  if [[ "$mode" == apply ]]; then
    if ! "$producer" --reopen "$image" >>"$log" 2>&1; then
      cat "$log" >&2
      exit 1
    fi
    for root_evidence in '__dsl_root_bound' '1.500000000000000'; do
      if ! grep -Fq "$root_evidence" "$trace"; then
        echo "missing root evidence '$root_evidence' in $trace" >&2
        exit 1
      fi
    done
  fi
  if [[ "$(grep -Ec '^FUNC_ENTRY ' "$trace")" -ne 3 ]] ||
     [[ "$(grep -Ec 'VCALL .*pu_transaction_variant' "$trace")" -ne 2 ]] ||
     [[ "$(grep -Ec 'COMMENT .*__WHIRL_DSL_CALL__:callee=pu_transaction_variant' "$trace")" -ne 2 ]]; then
    echo "PU/call/comment census changed in $trace" >&2
    exit 1
  fi
  for evidence in \
    'bound_b' \
    '__dsl_arg_1_1' \
    '__dsl_arg_2_1' \
    '2.000000000000000' \
    '3.000000000000000' \
    'REGION id=1' \
    'DSL Call ABI Argument Table:' \
    'role=fhe.relu.bound' \
    'dsl_pu_transaction_contract_test.cxx'; do
    if ! grep -Fq "$evidence" "$trace"; then
      echo "missing evidence '$evidence' in $trace" >&2
      exit 1
    fi
  done
done

if [[ -n "$be" ]]; then
  rejected="$artifact_dir/no_policy.B"
  printf '%s\n' \
    "$be -DSL:pu_specialization_checkpoint=$rejected $artifact_dir/pu_transaction_apply.B # must reject without a registered policy" \
    >>"$artifact_dir/commands.txt"
  if "$be" -DSL:pu_specialization_checkpoint="$rejected" \
       "$artifact_dir/pu_transaction_apply.B" \
       >"$artifact_dir/no_policy.log" 2>&1; then
    echo "unregistered specialization policy unexpectedly succeeded" >&2
    exit 1
  fi
  if [[ -e "$rejected" || -e "$rejected.tmp" ]] ||
     ! grep -Fq 'no registered policy' "$artifact_dir/no_policy.log"; then
    echo "unregistered policy did not fail before output creation" >&2
    exit 1
  fi
fi

(cd "$artifact_dir" && sha256sum ./*.B ./*.T >SHA256SUMS)
echo "DSL PU transaction contract passed"
echo "review artifacts: $artifact_dir"
