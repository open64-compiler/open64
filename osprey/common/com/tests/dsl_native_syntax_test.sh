#!/usr/bin/env bash
#
# Lightweight syntax regression fixture for the DSL native builder surface.
# This keeps Phase 2/3 checks runnable before a full Open64 toolchain build is
# available in the current worktree.

set -euo pipefail

script_dir="$(cd "$(dirname "$0")" && pwd)"
repo_root="$(cd "$script_dir/../../../.." && pwd)"

if [[ "$(uname -s)" == "Darwin" &&
      -z "${OPEN64_DSL_TEST_ALLOW_DARWIN:-}" ]]; then
  echo "skip: this native builder syntax fixture targets the Linux/Open64 "
  echo "GNU libstdc++ environment; run it in the Open64 Docker image."
  echo "Set OPEN64_DSL_TEST_ALLOW_DARWIN=1 to force a local macOS attempt."
  exit 0
fi

cxx="${CXX:-g++}"
python="${PYTHON:-python3}"
cxxstd="${OPEN64_DSL_TEST_CXXSTD:--std=gnu++98}"
python_include="$("$python" -c 'import sysconfig; print(sysconfig.get_paths()["include"])')"

cxxflags=(
  "$cxxstd"
  -DKEY
  -D__MIPS_AND_IA64_ELF_H
  -fsyntax-only
  -I"$python_include"
  -I"$repo_root/osprey/linux/include"
  -I"$repo_root/osprey/ir_tools"
  -I"$repo_root/osprey/be/opt"
  -I"$repo_root/osprey/be/vho"
  -I"$repo_root/osprey/common/com"
  -I"$repo_root/osprey/common/com/x8664"
  -I"$repo_root/osprey/common/util"
  -I"$repo_root/osprey/include"
  -I"$repo_root/osprey/libdwarf/libdwarf"
  -I"$repo_root/osprey/torch2whirl/python/native"
)

sources=(
  "osprey/be/opt/opt_dsl.cxx"
  "osprey/be/opt/opt_dsl_semantic.cxx"
  "osprey/be/opt/tests/dsl_wopt_bridge_test.cxx"
  "osprey/be/opt/tests/dsl_wopt_semantic_info_test.cxx"
  "osprey/be/vho/dsl_lower.cxx"
  "osprey/be/vho/dsl_opt.cxx"
  "osprey/be/vho/tests/dsl_lower_contract_test.cxx"
  "osprey/be/vho/tests/dsl_opt_contract_test.cxx"
  "osprey/be/vho/tests/dsl_runtime_abi_contract_test.cxx"
  "osprey/common/com/dsl_builder.cxx"
  "osprey/common/com/dsl_fhe.cxx"
  "osprey/common/com/dsl_fhe_plan.cxx"
  "osprey/common/com/dsl_fhe_plan_print.cxx"
  "osprey/common/com/dsl_fhe_print.cxx"
  "osprey/common/com/dsl_gatekeeper.cxx"
  "osprey/common/com/dsl_ir_image.cxx"
  "osprey/common/com/dsl_ir_rewrite.cxx"
  "osprey/common/com/dsl_region.cxx"
  "osprey/common/com/dsl_ir_print.cxx"
  "osprey/common/com/dsl_simp.cxx"
  "osprey/common/com/dsl_tensor_fold.cxx"
  "osprey/common/com/tests/dsl_builder_contract_test.cxx"
  "osprey/common/com/tests/dsl_builder_simplifier_control_test.cxx"
  "osprey/common/com/tests/dsl_canonicalization_contract_test.cxx"
  "osprey/common/com/tests/dsl_common_add_print_test.cxx"
  "osprey/common/com/tests/dsl_common_matmul_print_test.cxx"
  "osprey/common/com/tests/dsl_tensor_fold_contract_test.cxx"
  "osprey/torch2whirl/python/native/open64_dsc_native_bridge.cxx"
)

if [[ -f "$python_include/Python.h" ]]; then
  sources+=("osprey/torch2whirl/python/native/_whirl_module.cxx")
else
  echo "skip: Python.h not found under $python_include"
fi

for source in "${sources[@]}"; do
  "$cxx" "${cxxflags[@]}" "$repo_root/$source"
  echo "syntax ok: $source"
done

"$script_dir/dsl_builder_simplifier_control_test.sh"
"$script_dir/dsl_canonicalization_contract_test.sh"
"$script_dir/dsl_tensor_fold_contract_test.sh"
"$script_dir/dsl_operator_layout_test.sh"
bash "$repo_root/osprey/be/opt/tests/dsl_wopt_semantic_info_test.sh"

echo "DSL native syntax fixture passed"
