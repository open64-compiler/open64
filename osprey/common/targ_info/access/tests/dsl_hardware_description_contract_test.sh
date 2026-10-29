#!/usr/bin/env bash
# Build the actual target-description sources without any backend include path.
# Evidence is staging output; accepted milestone evidence is separately promoted
# under doc/REVIEW-ARTIFACT-RETENTION-POLICY.md, never cleaned by this fixture.
set -euo pipefail
script_dir="$(cd "$(dirname "$0")" && pwd)"
repo_root="$(cd "$script_dir/../../../../.." && pwd)"
artifact_dir="${OPEN64_DSL_HARDWARE_ARTIFACT_DIR:-$repo_root/artifacts/ai_optimization/hardware_description}"
case "$artifact_dir" in
  */artifacts/review|*/artifacts/review/*)
    echo "Refusing to write directly into immutable review evidence" >&2
    exit 2
    ;;
esac
mkdir -p "$artifact_dir"
cxx="${CXX:-g++}"
flags=(-std=gnu++98 -DKEY -D__MIPS_AND_IA64_ELF_H
  -I"$repo_root/osprey/linux/include" -I"$repo_root/osprey/ir_tools"
  -I"$repo_root/osprey/common/targ_info/access"
  -I"$repo_root/osprey/common/com" -I"$repo_root/osprey/common/com/x8664"
  -I"$repo_root/osprey/common/util" -I"$repo_root/osprey/include")
command=("$cxx" "${flags[@]}"
  "$repo_root/osprey/common/targ_info/access/dsl_memory_hierarchy.cxx"
  "$repo_root/osprey/common/targ_info/access/dsl_compute_profile.cxx"
  "$script_dir/dsl_hardware_description_contract_test.cxx"
  -o "$artifact_dir/hardware_contract")
printf '%q ' "${command[@]}" > "$artifact_dir/commands.txt"
printf '\n' >> "$artifact_dir/commands.txt"
"${command[@]}" > "$artifact_dir/build.log" 2>&1
for header in dsl_compute_format.h dsl_compute_profile.h dsl_hardware_performance.h dsl_memory_hierarchy.h; do
  "$cxx" "${flags[@]}" -x c++ -fsyntax-only \
    "$repo_root/osprey/common/targ_info/access/$header" >> "$artifact_dir/build.log" 2>&1
done
"$artifact_dir/hardware_contract" > "$artifact_dir/inventory.txt" 2> "$artifact_dir/diagnostics.txt"
tail -1 "$artifact_dir/inventory.txt"
printf 'hardware-description staging evidence: %s\n' "$artifact_dir"
