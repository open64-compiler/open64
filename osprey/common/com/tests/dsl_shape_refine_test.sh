#!/usr/bin/env bash

set -euo pipefail

script_dir="$(cd "$(dirname "$0")" && pwd)"
repo_root="$(cd "$script_dir/../../../.." && pwd)"
producer="${OPEN64_DSL_SHAPE_REFINE_TEST:-$repo_root/build/osprey/targdir/ir_tools/dsl_shape_refine_contract_test}"
ir_b2a="${OPEN64_IR_B2A:-$repo_root/build/osprey/targdir/ir_tools/ir_b2a}"
artifact_dir="${OPEN64_DSL_SHAPE_SP10_ARTIFACT_DIR:-${OPEN64_DSL_SHAPE_SP7_ARTIFACT_DIR:-$repo_root/artifacts/shape/sp7-pipeline}}"
before_image="$artifact_dir/shape_refine.before.B"
before_trace="$artifact_dir/shape_refine.before.T"
after_image="$artifact_dir/shape_refine.after.B"
after_trace="$artifact_dir/shape_refine.after.T"
trace_diff="$artifact_dir/shape_refine.before-after.diff"
driver_source="$repo_root/osprey/be/be/driver.cxx"
lower_source="$repo_root/osprey/be/vho/dsl_lower.cxx"
shape_source="$repo_root/osprey/be/vho/dsl_shape_refine.cxx"
opt_source="$repo_root/osprey/be/vho/dsl_opt.cxx"

for executable in "$producer" "$ir_b2a"; do
  if [[ ! -x "$executable" ]]; then
    echo "missing executable: $executable" >&2
    exit 1
  fi
done

mkdir -p "$artifact_dir"
find "$artifact_dir" -mindepth 1 -maxdepth 1 -type f -delete

printf '%s\n' \
  "OPEN64_DSL_SHAPE_SP10_BEFORE_ARTIFACT=$before_image $producer" \
  "$ir_b2a -st -src $before_image $before_trace" \
  "OPEN64_DSL_SHAPE_SP10_ARTIFACT=$after_image $producer" \
  "$ir_b2a -st -src $after_image $after_trace" \
  "diff -u $before_trace $after_trace" >"$artifact_dir/commands.txt"

(cd "$repo_root" && \
  OPEN64_DSL_SHAPE_SP10_BEFORE_ARTIFACT="$before_image" \
  "$producer") >"$artifact_dir/before-validation.log" 2>&1
(cd "$repo_root" && "$ir_b2a" -st -src "$before_image" "$before_trace") \
  >>"$artifact_dir/before-validation.log" 2>&1
(cd "$repo_root" && OPEN64_DSL_SHAPE_SP10_ARTIFACT="$after_image" \
  "$producer") >"$artifact_dir/validation.log" 2>&1
(cd "$repo_root" && "$ir_b2a" -st -src "$after_image" "$after_trace") \
  >>"$artifact_dir/validation.log" 2>&1
if diff -u "$before_trace" "$after_trace" >"$trace_diff"; then
  echo "shape refinement did not change the trace" >&2
  exit 1
elif [[ $? -ne 1 ]]; then
  echo "could not compare shape refinement traces" >&2
  exit 1
fi
cp "$after_image" "$artifact_dir/shape_refine.B"
cp "$after_trace" "$artifact_dir/shape_refine.T"

grep -Fq "SP10 before-refinement admission image passed" \
  "$artifact_dir/before-validation.log"
grep -Fq "SP7 shape refinement contract passed" \
  "$artifact_dir/validation.log"
grep -Fq "SP10 shape trigger contract passed" \
  "$artifact_dir/validation.log"
grep -Fq "DSL-SHAPE-INVALIDATE:" "$artifact_dir/validation.log"
grep -Fq "reason=operator_constraint_change" "$artifact_dir/validation.log"
grep -Fq "DSL-SHAPE-INVALIDATE-ERROR: trigger=12 name=unknown" \
  "$artifact_dir/validation.log"
for trigger in dsl_wopt fhe_conversion vho_dsl_optimization; do
  grep -Fq "reason=$trigger" "$artifact_dir/validation.log"
done
grep -Fq "logical_shape = [2,<pending>]" "$before_trace"
grep -Fq "shape_add" "$after_trace"
grep -Fq "shape_relu" "$after_trace"
grep -Fq "logical_shape = [2,3]" "$after_trace"
grep -Fq "shape.refine.v1" "$after_trace"
if grep -Fq "OPR_DSL " "$after_trace"; then
  echo "physical DSL escape tag leaked into $after_trace" >&2
  exit 1
fi

shape_line="$(grep -n 'VHO_DSL_Shape_Refine_Driver(current_pu, pu)' \
  "$driver_source" | head -1 | cut -d: -f1)"
wopt_line="$(grep -n 'Perform_Preopt_Optimization(pu, pu)' \
  "$driver_source" | head -1 | cut -d: -f1)"
fhe_line="$(grep -n 'VHO_FHE_Convert_Driver_With_Result' \
  "$driver_source" | head -1 | cut -d: -f1)"
dsl_lower_line="$(grep -n 'VHO_DSL_Lower_Driver (current_pu, pu)' \
  "$driver_source" | head -1 | cut -d: -f1)"
vho_line="$(grep -n 'VHO_Lower_Driver (current_pu, pu)' \
  "$driver_source" | head -1 | cut -d: -f1)"
preprocess_line="$(grep -n 'pu = Preprocess_PU(current_pu)' \
  "$driver_source" | head -1 | cut -d: -f1)"
if [[ -z "$shape_line" || -z "$wopt_line" || -z "$fhe_line" ||
      -z "$dsl_lower_line" || -z "$vho_line" ||
      -z "$preprocess_line" ]] ||
   (( shape_line >= wopt_line || shape_line >= fhe_line ||
      shape_line >= dsl_lower_line || shape_line >= vho_line )); then
  echo "DSL shape refinement driver ordering changed" >&2
  exit 1
fi
for evidence in \
  'DSL Shape Refinement after WOPT' \
  'DSL Shape Refinement after FHE Conversion'; do
  if ! grep -Fq "$evidence" "$driver_source"; then
    echo "missing SP7 driver evidence: $evidence" >&2
    exit 1
  fi
done
if ! grep -Fq 'VHO_DSL_Shape_Refinement_Is_Current' "$lower_source" ||
   ! grep -Fq 'VHO_DSL_Opt_Enabled_Stages_Invalidate_Shape' \
      "$lower_source"; then
  echo "missing SP7 defensive lowering evidence" >&2
  exit 1
fi
for evidence in \
  'VHO_DSL_SHAPE_TRIGGER_DSL_WOPT' \
  'VHO_DSL_SHAPE_TRIGGER_FHE_CONVERSION'; do
  if ! grep -Fq "$evidence" "$driver_source"; then
    echo "missing SP10 driver trigger: $evidence" >&2
    exit 1
  fi
done
if ! grep -Fq 'VHO_DSL_SHAPE_TRIGGER_VHO_DSL_OPTIMIZATION' \
     "$lower_source" ||
   ! grep -Fq 'return VHO_DSL_OPT_SHAPE_INVALIDATING_LOCAL' \
     "$opt_source" ||
   ! grep -Fq 'DSL-SHAPE-INVALIDATE-ERROR' "$shape_source"; then
  echo "missing SP10 structured trigger enforcement" >&2
  exit 1
fi

cat >"$artifact_dir/certification.txt" <<EOF
admission_then_strict=passed
disabled_check_only_no_mutation=passed
immutable_uniqued_tensor_type=passed
atomic_late_failure_rollback=passed
wn_st_value_retype=passed
region_interface_preserved=passed
generation_invalidation=passed
structured_trigger_names=passed
unknown_trigger_rejected_without_invalidation=passed
current_invalidation_call_sites_audited=passed
unclassified_shape_effect_defaults_invalidating=passed
stale_generation_rejected=passed
post_transform_revalidation=passed
driver_owned_per_pu_traversal=passed
shape_before_dsl_wopt=passed
shape_before_fhe_conversion=passed
shape_before_dsl_lowering=passed
shape_after_dsl_wopt=passed
shape_after_fhe_conversion=passed
shape_after_vho_dsl_optimization=passed
shape_before_language_vho=passed
ir_b2a_st_src=passed
before_after_binary_whirl=passed
before_after_trace_diff=passed
binary_layout_change=none
EOF

echo "SP7 shape refinement fixture passed"
echo "review before trace: $before_trace"
echo "review after trace: $after_trace"
echo "review diff: $trace_diff"
echo "review diagnostics: $artifact_dir/validation.log"
