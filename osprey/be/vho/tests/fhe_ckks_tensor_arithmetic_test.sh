#!/usr/bin/env bash
# Retain separate-process WHIRL evidence for tensor add and multiply.
# Design: doc/FHE-SYNC6-S6-0C-DETAILED-EXECUTION-PLAN.md.
set -euo pipefail

repo_root=$(cd "$(dirname "$0")/../../../.." && pwd)
build_root=${OPEN64_BUILD_ROOT:-$repo_root/build}
artifact_dir=${OPEN64_FHE_ARITH_ARTIFACT_DIR:-$repo_root/artifacts/fhe/ckks-tensor-arithmetic}
tool_dir="$build_root/osprey/targdir/ir_tools"
whirl2c="$build_root/osprey/targdir/whirl2c/whirl2c"
fragment="$repo_root/osprey/be/vho/tests/fhe_ckks_tensor_arithmetic_test.mk"
mkdir -p "$artifact_dir"
find "$artifact_dir" -mindepth 1 -maxdepth 1 -type f -delete
cp "$repo_root/osprey/be/vho/tests/fhe_ckks_tensor_arithmetic_test.cxx" \
  "$artifact_dir/fhe_ckks_tensor_arithmetic_test.cxx"
make -C "$tool_dir" -f Makefile -f "$fragment" \
  fhe_ckks_tensor_arithmetic_test ir_b2a \
  >"$artifact_dir/build.log" 2>&1

for mode in add add_plain add_capacity mul_plain mul_cipher; do
  before_image="$artifact_dir/tensor_$mode.before.B"
  before_trace="$artifact_dir/tensor_$mode.before.T"
  image="$artifact_dir/tensor_$mode.B"
  trace="$artifact_dir/tensor_$mode.T"
  "$tool_dir/fhe_ckks_tensor_arithmetic_test" "$mode" "$before_image" --before \
    >"$artifact_dir/$mode.before.producer.log" 2>&1
  "$tool_dir/ir_b2a" -st -src "$before_image" "$before_trace" \
    >"$artifact_dir/$mode.before.ir_b2a.log" 2>&1
  if [ "${mode#mul}" != "$mode" ]; then
    source_operator=OPR_DSLMUL
  else
    source_operator=OPR_DSLADD
  fi
  grep -Fq "operator=$source_operator version=1 operands=" "$before_trace"
  if grep -Fq 'status=lowered relation=ckks_expansion' "$before_trace" || \
     grep -Fq 'CKKS Event Image:' "$before_trace"; then
    echo "source baseline already contains CKKS expansion: $before_trace" >&2
    exit 1
  fi
  "$tool_dir/fhe_ckks_tensor_arithmetic_test" "$mode" "$image" \
    >"$artifact_dir/$mode.producer.log" 2>&1
  "$tool_dir/ir_b2a" -st -src "$image" "$trace" \
    >"$artifact_dir/$mode.ir_b2a.log" 2>&1
  grep -Fq 'CKKS Event Image: version=1 records=' "$trace"
  grep -Fq 'status=lowered relation=ckks_expansion' "$trace"
  grep -Fq "operator=$source_operator version=1 operands=" "$trace"
  grep -Fq 'FHE CKKS Value State Table:' "$trace"
  grep -Fq 'tensor_descriptor={kind=tensor,dtype=float32,rank=1,shape=[2]}' "$trace"
  grep -Eq '^ LOC 1 [1-9][0-9]* ' "$trace"
  if grep -Eq 'OPR_DSL[[:space:]]|MDSL[[:space:]]' "$trace"; then
    echo "physical DSL escape leaked into $trace" >&2
    exit 1
  fi
  test ! -e "$image.tmp"
  grep -Fq 'rejected mismatched CKKS layout without mutation' \
    "$artifact_dir/$mode.producer.log"
  grep -Fq 'clear two-slot algebra oracle passed' \
    "$artifact_dir/$mode.producer.log"
  for phase in before after; do
    if [ "$phase" = before ]; then
      candidate="$before_image"
    else
      candidate="$image"
    fi
    if (cd "$artifact_dir" && "$whirl2c" "$(basename "$candidate")") \
        >"$artifact_dir/$mode.$phase.whirl2c.log" 2>&1; then
      echo "whirl2c unexpectedly accepted $candidate" >&2
      exit 1
    fi
    grep -Fq 'CFHEMID-002:' "$artifact_dir/$mode.$phase.whirl2c.log"
    grep -Fq 'unlowered DSL/FHE node reached whirl2c' \
      "$artifact_dir/$mode.$phase.whirl2c.log"
    # whirl2c may emit an incomplete timestamped prologue before rejecting DSL.
    # Keep its diagnostic, not a misleading partial C translation.
    rm -f "${candidate%.B}.w2c.c" "${candidate%.B}.w2c.h"
  done
done
grep -Fq 'OPR_DSLCKKSADD' "$artifact_dir/tensor_add.T"
grep -Fq 'CKKS Event Image: version=1 records=1' "$artifact_dir/tensor_add.T"
grep -Fq 'value4(tensor_ckks_add) state_version=1 encryption=1 scheme=1 class=1 level=8 scale_bits=56 components=2' "$artifact_dir/tensor_add.T"
grep -Fq 'OPR_DSLCKKSENCODE' "$artifact_dir/tensor_add_plain.T"
grep -Fq 'OPR_DSLCKKSADD' "$artifact_dir/tensor_add_plain.T"
grep -Fq 'CKKS Event Image: version=1 records=2' "$artifact_dir/tensor_add_plain.T"
grep -Fq 'value5(tensor_ckks_plain_add) state_version=1 encryption=1 scheme=1 class=1 level=8 scale_bits=56 components=2 precision_bits=40 slots=8 alignment_group=0 layout=ckks.packed pending=0x0' "$artifact_dir/tensor_add_plain.T"
grep -Fq 'OPR_DSLCKKSBOOTSTRAP' "$artifact_dir/tensor_add_capacity.T"
grep -Fq 'attr.reason=DEPTH_EXHAUSTION' "$artifact_dir/tensor_add_capacity.T"
grep -Fq 'CKKS Event Image: version=1 records=2' "$artifact_dir/tensor_add_capacity.T"
grep -Fq 'value4(tensor_ckks_capacity_refresh) state_version=1 encryption=1 scheme=1 class=1 level=17 scale_bits=56 components=2 precision_bits=40 slots=8 alignment_group=0 layout=ckks.packed pending=0x0' "$artifact_dir/tensor_add_capacity.T"
grep -Fq 'value5(tensor_ckks_capacity_add) state_version=1 encryption=1 scheme=1 class=1 level=17 scale_bits=56 components=2 precision_bits=40 slots=8 alignment_group=0 layout=ckks.packed pending=0x0' "$artifact_dir/tensor_add_capacity.T"
grep -Fq 'rejected capacity reason and key mismatches without mutation' \
  "$artifact_dir/add_capacity.producer.log"
grep -Fq 'OPR_DSLCKKSENCODE' "$artifact_dir/tensor_mul_plain.T"
grep -Fq 'OPR_DSLCKKSMUL' "$artifact_dir/tensor_mul_plain.T"
grep -Fq 'OPR_DSLCKKSRESCALE' "$artifact_dir/tensor_mul_plain.T"
grep -Fq 'rejected unrepaired terminal multiplication' \
  "$artifact_dir/mul_plain.producer.log"
grep -Fq 'CKKS Event Image: version=1 records=3' "$artifact_dir/tensor_mul_plain.T"
grep -Fq 'value5(tensor_ckks_plain_mul) state_version=1 encryption=1 scheme=1 class=1 level=8 scale_bits=112 components=2 precision_bits=38 slots=8 alignment_group=0 layout=ckks.packed pending=0x1' "$artifact_dir/tensor_mul_plain.T"
grep -Fq 'value6(tensor_ckks_rescale) state_version=1 encryption=1 scheme=1 class=1 level=7 scale_bits=56 components=2 precision_bits=36 slots=8 alignment_group=0 layout=ckks.packed pending=0x0' "$artifact_dir/tensor_mul_plain.T"
grep -Fq 'OPR_DSLCKKSMUL' "$artifact_dir/tensor_mul_cipher.T"
grep -Fq 'OPR_DSLCKKSRELIN' "$artifact_dir/tensor_mul_cipher.T"
grep -Fq 'OPR_DSLCKKSRESCALE' "$artifact_dir/tensor_mul_cipher.T"
grep -Fq 'rejected unrepaired terminal multiplication' \
  "$artifact_dir/mul_cipher.producer.log"
grep -Fq 'rejected wrong relinearization key without mutation' \
  "$artifact_dir/mul_cipher.producer.log"
grep -Fq 'CKKS Event Image: version=1 records=3' "$artifact_dir/tensor_mul_cipher.T"
grep -Fq 'value4(tensor_ckks_cipher_mul) state_version=1 encryption=1 scheme=1 class=1 level=8 scale_bits=112 components=3 precision_bits=38 slots=8 alignment_group=0 layout=ckks.packed pending=0x3' "$artifact_dir/tensor_mul_cipher.T"
grep -Fq 'value5(tensor_ckks_relin) state_version=1 encryption=1 scheme=1 class=1 level=8 scale_bits=112 components=2 precision_bits=37 slots=8 alignment_group=0 layout=ckks.packed pending=0x1' "$artifact_dir/tensor_mul_cipher.T"
grep -Fq 'value6(tensor_ckks_rescale) state_version=1 encryption=1 scheme=1 class=1 level=7 scale_bits=56 components=2 precision_bits=36 slots=8 alignment_group=0 layout=ckks.packed pending=0x0' "$artifact_dir/tensor_mul_cipher.T"

printf '%s\n' \
  "make -C $tool_dir -f Makefile -f $fragment fhe_ckks_tensor_arithmetic_test ir_b2a" \
  "$tool_dir/fhe_ckks_tensor_arithmetic_test <mode> $artifact_dir/tensor_<mode>.B" \
  "$tool_dir/fhe_ckks_tensor_arithmetic_test <mode> $artifact_dir/tensor_<mode>.before.B --before" \
  "$tool_dir/ir_b2a -st -src $artifact_dir/tensor_<mode>.B $artifact_dir/tensor_<mode>.T" \
  "$tool_dir/ir_b2a -st -src $artifact_dir/tensor_<mode>.before.B $artifact_dir/tensor_<mode>.before.T" \
  "$whirl2c tensor_<mode>[.before].B (expected CFHEMID-002, no C emitted)" \
  >"$artifact_dir/commands.txt"
if command -v sha256sum >/dev/null 2>&1; then
  (cd "$artifact_dir" && sha256sum tensor_*.B tensor_*.T \
    fhe_ckks_tensor_arithmetic_test.cxx \
    *.producer.log *.ir_b2a.log *.whirl2c.log commands.txt >SHA256SUMS)
else
  (cd "$artifact_dir" && shasum -a 256 tensor_*.B tensor_*.T \
    fhe_ckks_tensor_arithmetic_test.cxx \
    *.producer.log *.ir_b2a.log *.whirl2c.log commands.txt >SHA256SUMS)
fi
echo "CKKS tensor arithmetic WHIRL roundtrip passed: $artifact_dir"
