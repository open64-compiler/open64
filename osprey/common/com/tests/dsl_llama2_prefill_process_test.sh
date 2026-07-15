#!/usr/bin/env bash
#
# Preserve process-boundary evidence for the tiny Llama 2 prefill contract.

set -euo pipefail

script_dir="$(cd "$(dirname "$0")" && pwd)"
repo_root="$(cd "$script_dir/../../../.." && pwd)"
build_dir="${OPEN64_BUILD_DIR:-$repo_root/build}"
mode="${OPEN64_DSL_LLAMA2_CERT_MODE:-full}"
artifact_dir="${OPEN64_DSL_LLAMA2_CERT_DIR:-$repo_root/artifacts/item28}"
producer="${OPEN64_DSL_CONTRACT_TEST:-$build_dir/osprey/targdir/ir_tools/dsl_builder_contract_test}"
ir_b2a="${OPEN64_IR_B2A:-$build_dir/osprey/targdir/ir_tools/ir_b2a}"
openpy="${OPEN64_OPENPY:-$build_dir/osprey/targdir/driver/openpy}"
frontend="${OPEN64_TORCH2WHIRL:-$build_dir/osprey/targdir/torch2whirl/torch2whirl}"
model="${OPEN64_DSL_LLAMA2_MODEL:-$repo_root/osprey/torch2whirl/python/tests/models/llama2_model.py}"

require_executable()
{
  if [[ ! -x "$1" ]]; then
    echo "missing executable: $1" >&2
    exit 1
  fi
}

require_file()
{
  if [[ ! -s "$1" ]]; then
    echo "missing or empty artifact: $1" >&2
    exit 1
  fi
}

require_text()
{
  local file="$1"
  local pattern="$2"

  if ! grep -Fq "$pattern" "$file"; then
    echo "missing artifact evidence '$pattern' in $file" >&2
    exit 1
  fi
}

reject_text()
{
  local file="$1"
  local pattern="$2"

  if grep -Eq "$pattern" "$file"; then
    echo "private or unlowered evidence '$pattern' leaked into $file" >&2
    exit 1
  fi
}

inspect_prefill()
{
  local image="$1"
  local trace="$2"

  "$ir_b2a" -st -src "$image" > "$trace"
  for evidence in \
    "operator=OPR_DSLTOKENEMBEDDING version=1" \
    "operator=OPR_DSLRMSNORM version=1" \
    "operator=OPR_DSLROTARYEMBEDDING version=1" \
    "operator=OPR_DSLATTENTION version=1" \
    "operator=OPR_DSLSWIGLU version=1" \
    "operator=OPR_DSLLINEAR version=3" \
    "operator=OPR_DSLOUTPUTLOGITS version=3" \
    "contract=transformer.prefill.v1" \
    "contract=transformer.decoder_layer.v1" \
    "storage_format = safetensors" \
    "storage_file = llama2.safetensors" \
    "value_kind=external_data" \
    "LOC 0"; do
    require_text "$trace" "$evidence"
  done
  reject_text "$trace" "(^|[[:space:]])OPR_DSL([[:space:]]|$)"
  reject_text "$trace" "(^|[[:space:]])MDSL([[:space:]]|$)"
}

case "$mode" in
  native|full)
    ;;
  *)
    echo "unknown certification mode: $mode (expected native or full)" >&2
    exit 1
    ;;
esac

require_executable "$producer"
require_executable "$ir_b2a"
mkdir -p "$artifact_dir"
find "$artifact_dir" -mindepth 1 -delete

OPEN64_DSL_LLAMA2_REGIONS_ONLY=1 \
OPEN64_DSL_LLAMA2_REGION_ARTIFACT="$artifact_dir/llama2.B" \
  "$producer" > "$artifact_dir/native-producer.log" 2>&1
require_file "$artifact_dir/llama2.B"
inspect_prefill "$artifact_dir/llama2.B" "$artifact_dir/llama2.T"

cat > "$artifact_dir/certification.txt" <<EOF
mode=$mode
native_builder=passed
gatekeeper_positive=passed
gatekeeper_stable_rejections=passed
mapped_image_reopen=passed
ir_b2a_st_src=passed
source_correlation=passed
external_tensor_references=passed
region_interfaces=passed
EOF

if [[ "$mode" == "native" ]]; then
  echo "native Llama 2 process-boundary preflight passed"
  echo "review artifacts: $artifact_dir"
  exit 0
fi

require_executable "$openpy"
require_executable "$frontend"
require_file "$model"

cp "$model" "$artifact_dir/llama2.py"
rm -f "$artifact_dir/llama2.B" "$artifact_dir/llama2.T"
(
  cd "$artifact_dir"
  "$openpy" -run-build="$build_dir" -keep -O0 llama2.py
) > "$artifact_dir/openpy.log" 2>&1

require_file "$artifact_dir/llama2.B"
require_file "$artifact_dir/llama2.safetensors"
require_file "$artifact_dir/llama2.t"
inspect_prefill "$artifact_dir/llama2.B" "$artifact_dir/llama2.T"

for runtime in \
  "__open64_dsl_transformer_token_embedding_v1" \
  "__open64_dsl_transformer_rms_norm_v1" \
  "__open64_dsl_transformer_rotary_embedding_v1" \
  "__open64_dsl_transformer_attention_v1" \
  "__open64_dsl_transformer_swiglu_v1"; do
  require_text "$artifact_dir/llama2.t" "$runtime"
done
reject_text "$artifact_dir/llama2.t" "OPR_DSL[A-Z]"

cat >> "$artifact_dir/certification.txt" <<EOF
python_frontend=passed
safetensors_side_file=passed
openpy_keep=passed
vho_dsl_lower_o0=passed
EOF

echo "full Llama 2 process-boundary certification passed"
echo "review artifacts: $artifact_dir"
