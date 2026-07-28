#!/usr/bin/env bash
#
# Certify the enabled M7 DSL simplification surface and retain review evidence.

set -euo pipefail

script_dir="$(cd "$(dirname "$0")" && pwd)"
repo_root="$(cd "$script_dir/../../../.." && pwd)"
build_root="${OPEN64_BUILD_ROOT:-$repo_root/build/osprey/targdir}"
artifact_dir="${OPEN64_DSL_M7_ARTIFACT_DIR:-$repo_root/artifacts/m7-certification}"
builder="$build_root/ir_tools/dsl_builder_contract_test"
vho_test="$build_root/ir_tools/dsl_lower_contract_test"
ir_b2a="$build_root/ir_tools/ir_b2a"

require_executable()
{
  if [[ ! -x "$1" ]]; then
    echo "missing executable: $1" >&2
    exit 1
  fi
}

require_executable "$builder"
require_executable "$vho_test"
require_executable "$ir_b2a"

mkdir -p "$artifact_dir"
find "$artifact_dir" -mindepth 1 -maxdepth 1 -type f -delete

"$script_dir/dsl_builder_simplifier_control_test.sh" \
  > "$artifact_dir/construction-control.log" 2>&1
"$script_dir/dsl_canonicalization_contract_test.sh" \
  > "$artifact_dir/canonicalization.log" 2>&1
"$script_dir/dsl_tensor_fold_contract_test.sh" \
  > "$artifact_dir/tensor-fold-contract.log" 2>&1

OPEN64_DSL_SIMPLIFIER_ONLY=1 "$builder" \
  > "$artifact_dir/wn-simplifier.log" 2>&1

OPEN64_DSL_CONTRACT_TEST="$builder" \
OPEN64_IR_B2A="$ir_b2a" \
OPEN64_DSL_TENSOR_TCON_ARTIFACT_DIR="$artifact_dir/tensor-tcon" \
  "$script_dir/dsl_tensor_tcon_image_test.sh"

OPEN64_DSL_CONTRACT_TEST="$builder" \
OPEN64_IR_B2A="$ir_b2a" \
OPEN64_DSL_TENSOR_FOLD_M3_ARTIFACT_DIR="$artifact_dir/construction" \
  "$script_dir/dsl_tensor_fold_m3_image_test.sh"

mkdir -p "$artifact_dir/vho"
OPEN64_DSL_VHO_M4_ONLY=1 \
OPEN64_DSL_VHO_M4_ARTIFACT="$artifact_dir/vho/vho_simplification.B" \
  "$vho_test" > "$artifact_dir/vho/producer.log" 2>&1
"$ir_b2a" -st -src \
  "$artifact_dir/vho/vho_simplification.B" \
  "$artifact_dir/vho/vho_simplification.T"

OPEN64_BUILD_ROOT="$build_root" \
OPEN64_DSL_WOPT_ARTIFACT_DIR="$artifact_dir/wopt" \
  "$repo_root/osprey/be/opt/tests/dsl_wopt_driver_test.sh"

cat > "$artifact_dir/certification.txt" <<EOF
traditional_wn_simplifier=passed
dsl_construction_control=passed
dsl_canonicalization=passed
tensor_fold_contract=passed
folded_tcon_gatekeeper=passed
mapped_image_reopen=passed
construction_option_ab=passed
vho_fold_and_parent_revisit=passed
wopt_option_ab=passed
wopt_factorization=passed
wopt_divrem_target_declined=passed
ir_b2a_st_src=passed
EOF

echo "M7 DSL simplification certification passed"
echo "review artifacts: $artifact_dir"
