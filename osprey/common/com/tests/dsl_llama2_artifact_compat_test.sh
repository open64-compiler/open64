#!/usr/bin/env bash
#
# Inspect the staged Llama 2 DSL artifacts and optional older WHIRL images.

set -euo pipefail

script_dir="$(cd "$(dirname "$0")" && pwd)"
repo_root="$(cd "$script_dir/../../../.." && pwd)"
contract_test="${OPEN64_DSL_CONTRACT_TEST:-$repo_root/build/osprey/targdir/ir_tools/dsl_builder_contract_test}"
ir_b2a="${OPEN64_IR_B2A:-$repo_root/build/osprey/targdir/ir_tools/ir_b2a}"
work_dir="${OPEN64_DSL_LLAMA2_COMPAT_DIR:-${TMPDIR:-/tmp}/open64-llama2-compat.$$}"

require_executable()
{
  if [[ ! -x "$1" ]]; then
    echo "missing executable: $1" >&2
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
    echo "private physical DSL evidence '$pattern' leaked into $file" >&2
    exit 1
  fi
}

inspect_image()
{
  local image="$1"
  local trace="$2"

  "$ir_b2a" -st -src "$image" > "$trace"
  require_text "$trace" "Symbols:"
  require_text "$trace" "Types:"
}

require_executable "$contract_test"
require_executable "$ir_b2a"
mkdir -p "$work_dir"

OPEN64_DSL_LLAMA2_COMMON_ONLY=1 \
OPEN64_DSL_LLAMA2_COMMON_ARTIFACT="$work_dir/llama2_common_substrate.B" \
  "$contract_test"
OPEN64_DSL_LLAMA2_TRANSFORMER_ONLY=1 \
OPEN64_DSL_LLAMA2_TRANSFORMER_ARTIFACT="$work_dir/llama2_transformer_expressions.B" \
  "$contract_test"
OPEN64_DSL_LLAMA2_REGIONS_ONLY=1 \
OPEN64_DSL_LLAMA2_REGION_ARTIFACT="$work_dir/llama2_transformer_regions.B" \
  "$contract_test"

inspect_image "$work_dir/llama2_common_substrate.B" \
              "$work_dir/llama2_common_substrate.T"
inspect_image "$work_dir/llama2_transformer_expressions.B" \
              "$work_dir/llama2_transformer_expressions.T"
inspect_image "$work_dir/llama2_transformer_regions.B" \
              "$work_dir/llama2_transformer_regions.T"

common_trace="$work_dir/llama2_common_substrate.T"
transformer_trace="$work_dir/llama2_transformer_expressions.T"
region_trace="$work_dir/llama2_transformer_regions.T"

for evidence in \
  "operator=OPR_DSLRESHAPE version=1" \
  "operator=OPR_DSLTRANSPOSE version=1" \
  "operator=OPR_DSLLINEAR version=3" \
  "operator=OPR_DSLMATMUL version=2" \
  "operator=OPR_DSLOUTPUTLOGITS version=3"; do
  require_text "$common_trace" "$evidence"
done

for evidence in \
  "operator=OPR_DSLTOKENEMBEDDING version=1" \
  "operator=OPR_DSLRMSNORM version=1" \
  "operator=OPR_DSLROTARYEMBEDDING version=1" \
  "operator=OPR_DSLATTENTION version=1" \
  "operator=OPR_DSLSWIGLU version=1"; do
  require_text "$transformer_trace" "$evidence"
done

for evidence in \
  "DSL IR Image: version=1" \
  "DSL Opcode Descriptor Table:" \
  "DSL Node Table:" \
  "DSL Attribute Table:" \
  "DSL Value Table:" \
  "DSL Value Reference Table:" \
  "TensorDescriptorIR view:" \
  "REGION id=1 parent=0 depth=1 kind=0 contract=transformer.prefill.v1" \
  "REGION id=2 parent=1 depth=2 kind=0 contract=transformer.decoder_layer.v1" \
  "contract=transformer.prefill.v1" \
  "contract=transformer.decoder_layer.v1" \
  "METADATA module_path:TinyLlama.forward" \
  "METADATA module_path:layers.0" \
  "METADATA layer_ordinal:0" \
  "storage_format = safetensors" \
  "storage_file = llama2.safetensors" \
  "Tensor storage: placement = side_file (llama2.safetensors)" \
  "storage_tensor_key = tok_embeddings.weight" \
  "value_kind=external_data" \
  "LOC 0 40"; do
  require_text "$region_trace" "$evidence"
done

if [[ "$(grep -Fc "storage_format = safetensors" "$region_trace")" -ne 9 ]]; then
  echo "external tensor reference count changed in $region_trace" >&2
  exit 1
fi

for trace in "$common_trace" "$transformer_trace" "$region_trace"; do
  reject_text "$trace" "(^|[[:space:]])OPR_DSL([[:space:]]|$)"
  reject_text "$trace" "(^|[[:space:]])MDSL([[:space:]]|$)"
done

if [[ -n "${OPEN64_DSL_OLD_IMAGE:-}" ]]; then
  inspect_image "$OPEN64_DSL_OLD_IMAGE" "$work_dir/old_image.T"
  require_text "$work_dir/old_image.T" "FUNC_ENTRY"
  if grep -Fq "DSL REGION TABLE:" "$work_dir/old_image.T"; then
    echo "older image unexpectedly acquired a REGION table" >&2
    exit 1
  fi
fi

if [[ -n "${OPEN64_DSL_RESNET_IMAGE:-}" ]]; then
  inspect_image "$OPEN64_DSL_RESNET_IMAGE" "$work_dir/resnet.T"
  require_text "$work_dir/resnet.T" "common.model_input"
  require_text "$work_dir/resnet.T" "common.residual_add"
  reject_text "$work_dir/resnet.T" "(^|[[:space:]])OPR_DSL([[:space:]]|$)"
  reject_text "$work_dir/resnet.T" "(^|[[:space:]])MDSL([[:space:]]|$)"
fi

echo "Llama 2 artifact compatibility fixture passed"
echo "review artifacts: $work_dir"
