# Llama 2 Decode Observable Census

This document records frontend-only observable discovery for a deterministic
tiny Llama 2 single-token decode. It proposes contracts for native/common
review but does not allocate DSL opcodes, define WHIRL encodings, or implement
frontend lowering.

The already certified no-cache prefill path remains unchanged.

## Discovery Profile

| Property | Value |
| --- | --- |
| Vocabulary | 128 |
| Hidden size | 32 |
| Intermediate size | 88 |
| Decoder layers | 2 |
| Query heads | 4 |
| KV heads | 4 |
| Head dimension | 8 |
| Batch | 1 |
| Decode tokens | 1 |
| Input cache length | 3 |
| Output cache length | 4 |
| RoPE capacity | 8 positions |
| Token and position dtype | `int64` |
| Activation and cache dtype | `float32` |
| Cache layout | `[batch,kv_head,sequence,head_dim]` |
| Cache behavior | Functional append; input caches are not mutated |
| Attention score shape | `[1,4,1,4]` |
| Logits shape | `[1,1,128]` |

The fixture uses deterministic local parameters and nonzero deterministic
cache inputs. Repeated execution produces identical logits and caches. Each
output cache preserves the complete input prefix and appends one current-token
entry.

## Source-Model Input Protocol

The source module publishes `open64_sample_inputs()` in the exact positional
order of its `forward` signature:

```text
input_ids              int64[1,1]
cache_position         int64[1]       value 3
layer0_key_cache       float32[1,4,3,8]
layer0_value_cache     float32[1,4,3,8]
layer1_key_cache       float32[1,4,3,8]
layer1_value_cache     float32[1,4,3,8]
```

The result protocol is:

```text
logits                  float32[1,1,128]
layer0_updated_key      float32[1,4,4,8]
layer0_updated_value    float32[1,4,4,8]
layer1_updated_key      float32[1,4,4,8]
layer1_updated_value    float32[1,4,4,8]
```

The explicit position value equals the valid input-cache length. It selects
the RoPE row for the current token. This equality is a semantic invariant for
the append profile, not incidental compiler metadata.

## Observed State Semantics

1. **Read:** cached attention reads every valid K/V entry from the input cache.
2. **Append:** each layer concatenates current K/V at sequence axis 2.
3. **Update:** the returned K/V values replace the corresponding cache state
   for the next decode invocation; the source tensors are not modified.
4. **Position:** `cache_position` selects RoPE cosine and sine rows for both Q
   and K and must agree with the append location.
5. **Ordering:** each layer's updated cache is produced by that layer's
   attention and is returned with logits at the model boundary.
6. **Masking:** a single current token attending only to the old prefix plus
   itself requires no materialized causal mask in this profile.

The functional model makes state edges observable without deciding whether a
future native implementation uses region input/result symbols, CHI-like state
effects, indexed cache storage, or another reviewed abstraction.

## Semantic Operator Census

The normalized graph has 142 nodes. The ranges below are ordered, complete,
and non-overlapping. Low-level FX nodes remain grouped under their semantic
owner instead of being proposed automatically as DSL operators.

| FX ordinal | Representative names | Observed operation | Semantic owner | Native/common disposition |
| --- | --- | --- | --- | --- |
| `000` | `input_ids` | Token placeholder | model input | Reuse integer tensor model input |
| `001` | `cache_position` | Position placeholder | decode state | Needs reviewed position/state operand contract |
| `002-005` | `layer0_key_cache` through `layer1_value_cache` | K/V state placeholders | decode state | Needs cache descriptor and state identity |
| `006` | `token_embedding` | Embedding lookup | token embedding | Reuse certified embedding semantics at decode shape |
| `007-013` | `mul` through `mul_2` | RMSNorm decomposition | layer 0 attention norm | Reuse `transformer.rms_norm` semantic recovery |
| `014-016` | `layers_0_attention_wq`, `view`, `transpose` | Q projection and head layout | layer 0 attention | Reuse linear/layout contracts with decode shapes |
| `017-027` | RoPE constants, `index_select`, rotate-half operations | Position-selected query RoPE | layer 0 RoPE | Existing no-position RoPE contract is insufficient |
| `028-030` | `layers_0_attention_wk`, `view_1`, `transpose_1` | Current K projection | layer 0 attention | Reuse linear/layout contracts |
| `031-041` | RoPE constants through `add_2` | Position-selected key RoPE | layer 0 RoPE | Needs explicit position operand |
| `042-044` | `layers_0_attention_wv`, `view_2`, `transpose_2` | Current V projection | layer 0 attention | Reuse linear/layout contracts |
| `045` | `cat_2` | Append current K at axis 2 | layer 0 key-cache state | Needs cache append/update contract |
| `046` | `cat_3` | Append current V at axis 2 | layer 0 value-cache state | Needs cache append/update contract |
| `047-051` | `transpose_3` through `matmul_1` | Cached QK scores, scale, softmax, AV | layer 0 cached attention | Needs cached-attention shape/state contract |
| `052-055` | `transpose_4` through output projection | Restore hidden layout and O projection | layer 0 attention | Reuse layout and linear contracts |
| `056` | `add_3` | Attention residual | layer 0 decoder region | Reuse residual add |
| `057-063` | `mul_8` through `mul_10` | RMSNorm decomposition | layer 0 FFN norm | Reuse RMSNorm semantic recovery |
| `064-068` | gate/up/down projections, `silu`, `mul_11` | SwiGLU | layer 0 feed-forward | Reuse certified SwiGLU semantics at decode shape |
| `069` | `add_5` | Feed-forward residual | layer 0 decoder region | Reuse residual add |
| `070-076` | `mul_12` through `mul_14` | RMSNorm decomposition | layer 1 attention norm | Reuse RMSNorm semantic recovery |
| `077-079` | layer 1 WQ, reshape, transpose | Q projection and head layout | layer 1 attention | Reuse linear/layout contracts |
| `080-090` | layer 1 query RoPE sequence | Position-selected query RoPE | layer 1 RoPE | Needs explicit position operand |
| `091-093` | layer 1 WK, reshape, transpose | Current K projection | layer 1 attention | Reuse linear/layout contracts |
| `094-104` | layer 1 key RoPE sequence | Position-selected key RoPE | layer 1 RoPE | Needs explicit position operand |
| `105-107` | layer 1 WV, reshape, transpose | Current V projection | layer 1 attention | Reuse linear/layout contracts |
| `108` | `cat_6` | Append current K at axis 2 | layer 1 key-cache state | Needs cache append/update contract |
| `109` | `cat_7` | Append current V at axis 2 | layer 1 value-cache state | Needs cache append/update contract |
| `110-114` | `transpose_8` through `matmul_3` | Cached QK scores, scale, softmax, AV | layer 1 cached attention | Needs cached-attention shape/state contract |
| `115-118` | layout restore through layer 1 O projection | Restore hidden layout and O projection | layer 1 attention | Reuse layout and linear contracts |
| `119` | `add_9` | Attention residual | layer 1 decoder region | Reuse residual add |
| `120-126` | `mul_20` through `mul_22` | RMSNorm decomposition | layer 1 FFN norm | Reuse RMSNorm semantic recovery |
| `127-131` | layer 1 gate/up/down, SiLU and multiply | SwiGLU | layer 1 feed-forward | Reuse certified SwiGLU semantics at decode shape |
| `132` | `add_11` | Feed-forward residual | layer 1 decoder region | Reuse residual add |
| `133-139` | `mul_24` through `mul_26` | Final RMSNorm decomposition | model final norm | Reuse RMSNorm semantic recovery |
| `140` | `output` | Vocabulary projection | model logits | Reuse sequence-logits result with sequence 1 |
| `141` | `output_1` | Logits plus four updated caches | decode result bundle | Needs reviewed multi-result/state boundary abstraction |

## Existing Contracts Reusable Without Semantic Change

1. Integer model input for `input_ids` and floating tensor descriptors.
2. Token embedding, RMSNorm, rank-3 linear projections, residual add, SwiGLU,
   and sequence logits, subject to ordinary descriptor checks at sequence 1.
3. Reshape, transpose, batched matmul, multiplication, softmax, and external
   tensor payload publication as implementation constituents after semantic
   gatekeeping.
4. Decoder-layer region structure and source/module/layer metadata.
5. Opaque Python-facing value and region handles; Python still must not inspect
   WN layout or the physical DSL escape representation.

## Contracts Requiring Native/Common Review

The following are requirements, not opcode allocations:

1. A decode-region contract distinct from `transformer.prefill.v1`, with
   declared token/position/cache inputs and logits/updated-cache results.
2. A cache descriptor identifying K versus V role, layer identity, batch,
   KV-head count, valid sequence length, head dimension, sequence axis,
   capacity policy, dtype, layout, placement, and state identity.
3. A RoPE contract with `cache_position` as a semantic operand. Position must
   be range checked against RoPE capacity and related to cache update location.
4. Cached attention semantics relating query shape `[B,QH,1,D]`, cache shape
   `[B,KVH,L,D]`, score shape `[B,QH,1,L+1]`, and context shape
   `[B,QH,1,D]`.
5. Cache read and update effects that cannot be hidden in compiler metadata or
   represented as untyped attributes. The old and new state identities and
   ordering must remain visible through regions and future optimization.
6. A reviewed representation for the model result bundle. The fixture's flat
   Python tuple is observable source behavior, not a mandate for a generic
   multiple-result WHIRL operator.
7. Gatekeeper checks for cache-position/valid-length agreement, K/V pairing,
   per-layer ownership, shape compatibility, append-axis legality, RoPE range,
   and absence of unintended aliasing between distinct cache states.

## Promotion Guidance

Cache storage operations should not be promoted to the common substrate merely
because FX shows `torch.cat` or `torch.index_select`. Promotion requires equal
operand roles, result roles, attributes, descriptor rules, state/effect
semantics, failure behavior, and lowering obligations across domains. Generic
slice/concatenate operations may be reusable constituents, while KV-cache
identity and update ordering remain transformer-visible until proven otherwise.

## Deferred Profiles And Stable Rejections

This batch does not claim support for:

1. In-place indexed cache mutation, fixed-capacity overwrite, ring buffers,
   sliding-window attention, paged attention, or cache eviction.
2. Grouped-query or multi-query attention where query and KV head counts
   differ.
3. Multi-token decode, dynamic batch, dynamic cache length, or dynamic RoPE
   capacity.
4. Cache allocation, cross-device placement, quantized caches, beam-search
   cache reorder, speculative decoding, or tokenizer behavior.
5. Training, dropout, autograd-visible cache state, or network/licensed model
   dependencies.

Future frontend work should reject these forms explicitly until reviewed
contracts are published. It must not decompose them into a sequence that hides
state or silently reuse the no-cache attention contract.

## Retained Evidence

```text
python/tests/models/llama2_decode_model.py
python/tests/test_llama2_decode_capture_optional.py
python/tests/golden/llama2_decode_fx.txt
LLAMA2-DECODE-OPERATOR-CENSUS.md
```

The golden graph is machine checked for drift. The census test verifies that
the ordered semantic ranges cover every normalized FX node exactly once.

## Validation Evidence

Focused Linux PyTorch discovery:

```sh
docker run --rm --platform linux/amd64 \
  -v /path/to/open64:/src \
  -w /src/osprey/torch2whirl/python/tests \
  open64:torch2whirl-torch-test \
  python3 -m unittest -v test_llama2_decode_capture_optional
```

Result: 7 tests passed, covering the source input protocol, eager
repeatability, functional cache append, absence of input/model mutation,
normalized graph drift, cached-attention shapes, and census completeness.

Local dependency-light frontend suite:

```text
Ran 124 tests in 0.036s
OK (skipped=60)
```

Complete Linux torch2whirl Docker lane:

```text
PyTorch/FX capture: 47 tests passed
native Python ir_b2a smoke passed
driver native ir_b2a smoke passed
```

The full lane exercises the existing prefill tests and artifacts alongside the
new decode discovery tests. No prefill source, golden graph, frontend lowering,
native bridge, common/com source, DSL opcode, or WHIRL encoding changed in this
batch.
