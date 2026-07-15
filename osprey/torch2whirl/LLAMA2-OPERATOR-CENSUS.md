# Torch2whirl Llama 2 Operator Census

This file records the observable-discovery evidence for the tiny Llama 2
prefill fixture. It is a frontend discovery artifact only: it does not allocate
DSL opcodes, define WHIRL encodings, or authorize native lowering.

## Progress Dashboard

Progress is earned only when the checkpoint exit test passes.

| Checkpoint | Weight | Status | Review evidence |
| --- | ---: | --- | --- |
| D0: Record baseline and exact tiny configuration | 5% | complete | Baseline command and tiny profile below |
| D1: Implement deterministic tiny Llama 2 fixture | 20% | complete | `python/tests/models/llama2_model.py` |
| D2: Pass eager shape/dtype/repeatability tests | 15% | complete | Docker `python_torch_test` ran 32 tests OK |
| D3: Capture and normalize the real PyTorch graph | 20% | complete | `python/tests/golden/llama2_prefill_fx.txt` |
| D4: Complete semantic operator census | 25% | complete | One row per captured FX node below |
| D5: Add and pass graph/census drift tests | 10% | complete | Focused Llama test ran 6 tests OK; Docker torch lane ran 35 tests OK |
| D6: Publish main-infrastructure handoff | 5% | pending | Contract requests and blockers |

Current earned progress: 95%.

## D0 Baseline

Branch at kickoff: `codex/torch2whirl-python-fe`.

Commit at kickoff: `e07f0ea5`.

Required bootstrap commit: `cf291910 Plan observable Llama 2 ingestion`.
The bootstrap cherry-pick landed on this branch as `3113cfb9` after resolving
documentation-only conflicts in `AGENTS.md` and keeping the added infrastructure
reference document.

Baseline command:

```sh
make -C osprey/torch2whirl -f Makefile.gbase python_test
```

Baseline result:

```text
Ran 103 tests in 0.010s

OK (skipped=41)
```

## Tiny Profile

The first discovery fixture uses this static, dependency-light configuration:

| Property | Value |
| --- | --- |
| Vocabulary | 128 tokens |
| Hidden dimension | 32 |
| Decoder layers | 2 |
| Attention heads | 4 |
| KV heads | 4 |
| Head dimension | 8 |
| Batch size | 1 |
| Sequence length | 8 |
| Activation dtype | `float32` |
| Weight dtype | `float32` |
| Token dtype | `int64` |
| KV cache | Disabled |
| Dropout | None |
| Initialization | Deterministic local arithmetic pattern |
| Input | `input_ids` tensor with shape `[1,8]` |
| Output | Per-token logits with shape `[1,8,128]` |

Intentional differences from Meta's runtime implementation:

1. The fixture uses ordinary `torch.nn` modules only.
2. FairScale, CUDA-only state, distributed checkpointing, tokenizer logic, and
   licensed weights are excluded.
3. KV-cache allocation and mutation are excluded from the first prefill
   profile.
4. Shapes are static so graph drift can be reviewed deterministically before
   frontend lowering or native DSL contracts are added.

## Retained Review Artifacts

The D0 retained artifact is this census file. D1 adds the reviewable source
fixture at `python/tests/models/llama2_model.py`. D3-D5 will add the normalized
FX graph golden under `python/tests/golden`.

The D3 retained graph artifact is
`python/tests/golden/llama2_prefill_fx.txt`.

## D2 Eager Test Evidence

Local syntax and optional-skip checks:

```sh
PYTHONPYCACHEPREFIX=/private/tmp/open64-pycache python3 -m compileall -q \
  osprey/torch2whirl/python/tests
make -C osprey/torch2whirl -f Makefile.gbase python_test
```

Local result:

```text
Ran 106 tests in 0.008s

OK (skipped=44)
```

Torch Docker command:

```sh
OPEN64_TORCH2WHIRL_BUILD_DIR=/private/tmp/open64-torch2whirl-llama2 \
  sh osprey/torch2whirl/scripts/run_torch_docker_test.sh
```

Torch Docker result:

```text
Ran 32 tests in 2.128s

OK
```

The D2 tests verify the selected integer input dtype and shape, output logits
shape and dtype, repeatable inference, eval mode, absence of cache state, and
state-dict discoverability for embedding, attention, feed-forward, final norm,
and output tensors.

## Discovered Graph Forms

The normalized FX graph contains 129 nodes and uses these node forms:

1. `placeholder` for the token ID input.
2. `call_module` for embedding, linear projections, feed-forward projections,
   and output projection.
3. `get_attr` for RMSNorm weights, rotary `cos`/`sin` buffers, and causal mask.
4. `call_function` for arithmetic, `torch.mean`, `torch.rsqrt`, `torch.cat`,
   `torch.matmul`, `torch.softmax`, and `torch.nn.functional.silu`.
5. `call_method` for `view`, `transpose`, and `contiguous`.
6. `output` for the final logits return.

## Semantic Ambiguities

1. Token embedding remains visible as an `Embedding` `call_module`.
2. RMSNorm is decomposed into multiply, mean, epsilon add, reciprocal square
   root, multiply, and weight multiply.
3. RoPE is decomposed into linear projection reshape/transpose, `cos` and
   `sin` buffer reads, slices, negation, concat, multiplies, and add.
4. Causal masking appears as a static mask buffer plus add into attention
   scores.
5. Attention appears as Q/K/V linear projections, shape/layout methods, score
   matmul, scale multiply, mask add, softmax, value matmul, layout restore, and
   output projection.
6. SwiGLU appears as gate projection, SiLU, up projection, multiply, and down
   projection.
7. Decoder-layer boundaries are not explicit FX nodes; they must be recovered
   from source module paths or a reviewed region API.

## D4 Semantic Operator Census

This table has one row per normalized FX node. Classifications are frontend
discovery evidence for common/com review; they do not allocate opcodes or
define physical WHIRL representation.

| Ordinal | Node | FX op | Target | Observed type | Module | Semantic classification | Proposed contract | Review reason |
| ---: | --- | --- | --- | --- | --- | --- | --- | --- |
| 0 | `input_ids` | `placeholder` | `input_ids` | `int64[1,8]` | `-` | model input | transformer.input_tokens or typed common.model_input | Token IDs [1,8], int64. |
| 1 | `token_embedding` | `call_module` | `token_embedding` | `float32[1,8,32]` | `Embedding` | token embedding | transformer.token_embedding or reviewed common embedding/gather | Embedding remains visible as a module call. |
| 2 | `mul` | `call_function` | `_operator.mul` | `float32[1,8,32]` | `-` | layer0 attention RMSNorm decomposition | transformer.rms_norm | Recover semantic RMSNorm from arithmetic and weight path. |
| 3 | `mean` | `call_function` | `torch.mean` | `float32[1,8,1]` | `-` | layer0 attention RMSNorm decomposition | transformer.rms_norm | Recover semantic RMSNorm from arithmetic and weight path. |
| 4 | `add` | `call_function` | `_operator.add` | `float32[1,8,1]` | `-` | layer0 attention RMSNorm decomposition | transformer.rms_norm | Recover semantic RMSNorm from arithmetic and weight path. |
| 5 | `rsqrt` | `call_function` | `torch.rsqrt` | `float32[1,8,1]` | `-` | layer0 attention RMSNorm decomposition | transformer.rms_norm | Recover semantic RMSNorm from arithmetic and weight path. |
| 6 | `mul_1` | `call_function` | `_operator.mul` | `float32[1,8,32]` | `-` | layer0 attention RMSNorm decomposition | transformer.rms_norm | Recover semantic RMSNorm from arithmetic and weight path. |
| 7 | `layers_0_attention_norm_weight` | `get_attr` | `layers.0.attention_norm.weight` | `float32[32]` | `-` | layer0 attention RMSNorm decomposition | transformer.rms_norm | Recover semantic RMSNorm from arithmetic and weight path. |
| 8 | `mul_2` | `call_function` | `_operator.mul` | `float32[1,8,32]` | `-` | layer0 attention RMSNorm decomposition | transformer.rms_norm | Recover semantic RMSNorm from arithmetic and weight path. |
| 9 | `layers_0_attention_wq` | `call_module` | `layers.0.attention.wq` | `float32[1,8,32]` | `Linear` | layer0 Q projection | common.linear inside transformer.attention context | Linear module call preserves projection source path. |
| 10 | `view` | `call_method` | `view` | `float32[1,8,4,8]` | `-` | layer0 query RoPE decomposition | transformer.rotary_embedding | Reshape/transpose plus cos/sin/slice/concat arithmetic. |
| 11 | `transpose` | `call_method` | `transpose` | `float32[1,4,8,8]` | `-` | layer0 query RoPE decomposition | transformer.rotary_embedding | Reshape/transpose plus cos/sin/slice/concat arithmetic. |
| 12 | `layers_0_attention_rotary_cos` | `get_attr` | `layers.0.attention.rotary.cos` | `float32[1,1,8,8]` | `-` | layer0 query RoPE decomposition | transformer.rotary_embedding | Reshape/transpose plus cos/sin/slice/concat arithmetic. |
| 13 | `mul_3` | `call_function` | `_operator.mul` | `float32[1,4,8,8]` | `-` | layer0 query RoPE decomposition | transformer.rotary_embedding | Reshape/transpose plus cos/sin/slice/concat arithmetic. |
| 14 | `getitem` | `call_function` | `_operator.getitem` | `float32[1,4,8,4]` | `-` | layer0 query RoPE decomposition | transformer.rotary_embedding | Reshape/transpose plus cos/sin/slice/concat arithmetic. |
| 15 | `getitem_1` | `call_function` | `_operator.getitem` | `float32[1,4,8,4]` | `-` | layer0 query RoPE decomposition | transformer.rotary_embedding | Reshape/transpose plus cos/sin/slice/concat arithmetic. |
| 16 | `neg` | `call_function` | `_operator.neg` | `float32[1,4,8,4]` | `-` | layer0 query RoPE decomposition | transformer.rotary_embedding | Reshape/transpose plus cos/sin/slice/concat arithmetic. |
| 17 | `cat` | `call_function` | `torch.cat` | `float32[1,4,8,8]` | `-` | layer0 query RoPE decomposition | transformer.rotary_embedding | Reshape/transpose plus cos/sin/slice/concat arithmetic. |
| 18 | `layers_0_attention_rotary_sin` | `get_attr` | `layers.0.attention.rotary.sin` | `float32[1,1,8,8]` | `-` | layer0 query RoPE decomposition | transformer.rotary_embedding | Reshape/transpose plus cos/sin/slice/concat arithmetic. |
| 19 | `mul_4` | `call_function` | `_operator.mul` | `float32[1,4,8,8]` | `-` | layer0 query RoPE decomposition | transformer.rotary_embedding | Reshape/transpose plus cos/sin/slice/concat arithmetic. |
| 20 | `add_1` | `call_function` | `_operator.add` | `float32[1,4,8,8]` | `-` | layer0 query RoPE decomposition | transformer.rotary_embedding | Reshape/transpose plus cos/sin/slice/concat arithmetic. |
| 21 | `layers_0_attention_wk` | `call_module` | `layers.0.attention.wk` | `float32[1,8,32]` | `Linear` | layer0 K projection | common.linear inside transformer.attention context | Linear module call preserves projection source path. |
| 22 | `view_1` | `call_method` | `view` | `float32[1,8,4,8]` | `-` | layer0 key RoPE decomposition | transformer.rotary_embedding | Second RoPE instance shares rotary buffers but has separate result. |
| 23 | `transpose_1` | `call_method` | `transpose` | `float32[1,4,8,8]` | `-` | layer0 key RoPE decomposition | transformer.rotary_embedding | Second RoPE instance shares rotary buffers but has separate result. |
| 24 | `layers_0_attention_rotary_cos_1` | `get_attr` | `layers.0.attention.rotary.cos` | `float32[1,1,8,8]` | `-` | layer0 key RoPE decomposition | transformer.rotary_embedding | Second RoPE instance shares rotary buffers but has separate result. |
| 25 | `mul_5` | `call_function` | `_operator.mul` | `float32[1,4,8,8]` | `-` | layer0 key RoPE decomposition | transformer.rotary_embedding | Second RoPE instance shares rotary buffers but has separate result. |
| 26 | `getitem_2` | `call_function` | `_operator.getitem` | `float32[1,4,8,4]` | `-` | layer0 key RoPE decomposition | transformer.rotary_embedding | Second RoPE instance shares rotary buffers but has separate result. |
| 27 | `getitem_3` | `call_function` | `_operator.getitem` | `float32[1,4,8,4]` | `-` | layer0 key RoPE decomposition | transformer.rotary_embedding | Second RoPE instance shares rotary buffers but has separate result. |
| 28 | `neg_1` | `call_function` | `_operator.neg` | `float32[1,4,8,4]` | `-` | layer0 key RoPE decomposition | transformer.rotary_embedding | Second RoPE instance shares rotary buffers but has separate result. |
| 29 | `cat_1` | `call_function` | `torch.cat` | `float32[1,4,8,8]` | `-` | layer0 key RoPE decomposition | transformer.rotary_embedding | Second RoPE instance shares rotary buffers but has separate result. |
| 30 | `layers_0_attention_rotary_sin_1` | `get_attr` | `layers.0.attention.rotary.sin` | `float32[1,1,8,8]` | `-` | layer0 key RoPE decomposition | transformer.rotary_embedding | Second RoPE instance shares rotary buffers but has separate result. |
| 31 | `mul_6` | `call_function` | `_operator.mul` | `float32[1,4,8,8]` | `-` | layer0 key RoPE decomposition | transformer.rotary_embedding | Second RoPE instance shares rotary buffers but has separate result. |
| 32 | `add_2` | `call_function` | `_operator.add` | `float32[1,4,8,8]` | `-` | layer0 key RoPE decomposition | transformer.rotary_embedding | Second RoPE instance shares rotary buffers but has separate result. |
| 33 | `layers_0_attention_wv` | `call_module` | `layers.0.attention.wv` | `float32[1,8,32]` | `Linear` | layer0 V projection | common.linear inside transformer.attention context | Linear module call preserves projection source path. |
| 34 | `view_2` | `call_method` | `view` | `float32[1,8,4,8]` | `-` | layer0 value/head layout | transformer.attention operand descriptor | Value projection layout for attention operand. |
| 35 | `transpose_2` | `call_method` | `transpose` | `float32[1,4,8,8]` | `-` | layer0 value/head layout | transformer.attention operand descriptor | Value projection layout for attention operand. |
| 36 | `transpose_3` | `call_method` | `transpose` | `float32[1,4,8,8]` | `-` | layer0 value/head layout | transformer.attention operand descriptor | Value projection layout for attention operand. |
| 37 | `matmul` | `call_function` | `torch.matmul` | `float32[1,4,8,8]` | `-` | layer0 attention score/mask/softmax | transformer.attention | Score matmul, scale, causal mask add, and softmax are one semantic attention expression. |
| 38 | `mul_7` | `call_function` | `_operator.mul` | `float32[1,4,8,8]` | `-` | layer0 attention score/mask/softmax | transformer.attention | Score matmul, scale, causal mask add, and softmax are one semantic attention expression. |
| 39 | `layers_0_attention_causal_mask` | `get_attr` | `layers.0.attention.causal_mask` | `float32[1,1,8,8]` | `-` | layer0 attention score/mask/softmax | transformer.attention | Score matmul, scale, causal mask add, and softmax are one semantic attention expression. |
| 40 | `add_3` | `call_function` | `_operator.add` | `float32[1,4,8,8]` | `-` | layer0 attention score/mask/softmax | transformer.attention | Score matmul, scale, causal mask add, and softmax are one semantic attention expression. |
| 41 | `softmax` | `call_function` | `torch.softmax` | `float32[1,4,8,8]` | `-` | layer0 attention score/mask/softmax | transformer.attention | Score matmul, scale, causal mask add, and softmax are one semantic attention expression. |
| 42 | `matmul_1` | `call_function` | `torch.matmul` | `float32[1,4,8,8]` | `-` | layer0 attention score/mask/softmax | transformer.attention | Score matmul, scale, causal mask add, and softmax are one semantic attention expression. |
| 43 | `transpose_4` | `call_method` | `transpose` | `float32[1,8,4,8]` | `-` | layer0 attention context layout restore | transformer.attention result descriptor | Layout conversion from [B,H,S,D] back to [B,S,H]. |
| 44 | `contiguous` | `call_method` | `contiguous` | `float32[1,8,4,8]` | `-` | layer0 attention context layout restore | transformer.attention result descriptor | Layout conversion from [B,H,S,D] back to [B,S,H]. |
| 45 | `view_3` | `call_method` | `view` | `float32[1,8,32]` | `-` | layer0 attention context layout restore | transformer.attention result descriptor | Layout conversion from [B,H,S,D] back to [B,S,H]. |
| 46 | `layers_0_attention_wo` | `call_module` | `layers.0.attention.wo` | `float32[1,8,32]` | `Linear` | layer0 attention output projection | common.linear inside transformer.attention context | Linear module call preserves projection source path. |
| 47 | `add_4` | `call_function` | `_operator.add` | `float32[1,8,32]` | `-` | layer0 attention residual | common.residual_add inside transformer.decoder_layer | Exact-shape residual after attention output projection. |
| 48 | `mul_8` | `call_function` | `_operator.mul` | `float32[1,8,32]` | `-` | layer0 feed-forward RMSNorm decomposition | transformer.rms_norm | Recover semantic RMSNorm from arithmetic and weight path. |
| 49 | `mean_1` | `call_function` | `torch.mean` | `float32[1,8,1]` | `-` | layer0 feed-forward RMSNorm decomposition | transformer.rms_norm | Recover semantic RMSNorm from arithmetic and weight path. |
| 50 | `add_5` | `call_function` | `_operator.add` | `float32[1,8,1]` | `-` | layer0 feed-forward RMSNorm decomposition | transformer.rms_norm | Recover semantic RMSNorm from arithmetic and weight path. |
| 51 | `rsqrt_1` | `call_function` | `torch.rsqrt` | `float32[1,8,1]` | `-` | layer0 feed-forward RMSNorm decomposition | transformer.rms_norm | Recover semantic RMSNorm from arithmetic and weight path. |
| 52 | `mul_9` | `call_function` | `_operator.mul` | `float32[1,8,32]` | `-` | layer0 feed-forward RMSNorm decomposition | transformer.rms_norm | Recover semantic RMSNorm from arithmetic and weight path. |
| 53 | `layers_0_ffn_norm_weight` | `get_attr` | `layers.0.ffn_norm.weight` | `float32[32]` | `-` | layer0 feed-forward RMSNorm decomposition | transformer.rms_norm | Recover semantic RMSNorm from arithmetic and weight path. |
| 54 | `mul_10` | `call_function` | `_operator.mul` | `float32[1,8,32]` | `-` | layer0 feed-forward RMSNorm decomposition | transformer.rms_norm | Recover semantic RMSNorm from arithmetic and weight path. |
| 55 | `layers_0_feed_forward_gate_proj` | `call_module` | `layers.0.feed_forward.gate_proj` | `float32[1,8,88]` | `Linear` | layer0 SwiGLU decomposition | transformer.swiglu | Gate projection, SiLU, up projection, multiply, and down projection. |
| 56 | `silu` | `call_function` | `torch.nn.functional.silu` | `float32[1,8,88]` | `-` | layer0 SwiGLU decomposition | transformer.swiglu | Gate projection, SiLU, up projection, multiply, and down projection. |
| 57 | `layers_0_feed_forward_up_proj` | `call_module` | `layers.0.feed_forward.up_proj` | `float32[1,8,88]` | `Linear` | layer0 SwiGLU decomposition | transformer.swiglu | Gate projection, SiLU, up projection, multiply, and down projection. |
| 58 | `mul_11` | `call_function` | `_operator.mul` | `float32[1,8,88]` | `-` | layer0 SwiGLU decomposition | transformer.swiglu | Gate projection, SiLU, up projection, multiply, and down projection. |
| 59 | `layers_0_feed_forward_down_proj` | `call_module` | `layers.0.feed_forward.down_proj` | `float32[1,8,32]` | `Linear` | layer0 SwiGLU decomposition | transformer.swiglu | Gate projection, SiLU, up projection, multiply, and down projection. |
| 60 | `add_6` | `call_function` | `_operator.add` | `float32[1,8,32]` | `-` | layer0 feed-forward residual | common.residual_add inside transformer.decoder_layer | Exact-shape residual after feed-forward. |
| 61 | `mul_12` | `call_function` | `_operator.mul` | `float32[1,8,32]` | `-` | layer1 attention RMSNorm decomposition | transformer.rms_norm | Recover semantic RMSNorm from arithmetic and weight path. |
| 62 | `mean_2` | `call_function` | `torch.mean` | `float32[1,8,1]` | `-` | layer1 attention RMSNorm decomposition | transformer.rms_norm | Recover semantic RMSNorm from arithmetic and weight path. |
| 63 | `add_7` | `call_function` | `_operator.add` | `float32[1,8,1]` | `-` | layer1 attention RMSNorm decomposition | transformer.rms_norm | Recover semantic RMSNorm from arithmetic and weight path. |
| 64 | `rsqrt_2` | `call_function` | `torch.rsqrt` | `float32[1,8,1]` | `-` | layer1 attention RMSNorm decomposition | transformer.rms_norm | Recover semantic RMSNorm from arithmetic and weight path. |
| 65 | `mul_13` | `call_function` | `_operator.mul` | `float32[1,8,32]` | `-` | layer1 attention RMSNorm decomposition | transformer.rms_norm | Recover semantic RMSNorm from arithmetic and weight path. |
| 66 | `layers_1_attention_norm_weight` | `get_attr` | `layers.1.attention_norm.weight` | `float32[32]` | `-` | layer1 attention RMSNorm decomposition | transformer.rms_norm | Recover semantic RMSNorm from arithmetic and weight path. |
| 67 | `mul_14` | `call_function` | `_operator.mul` | `float32[1,8,32]` | `-` | layer1 attention RMSNorm decomposition | transformer.rms_norm | Recover semantic RMSNorm from arithmetic and weight path. |
| 68 | `layers_1_attention_wq` | `call_module` | `layers.1.attention.wq` | `float32[1,8,32]` | `Linear` | layer1 Q projection | common.linear inside transformer.attention context | Linear module call preserves projection source path. |
| 69 | `view_4` | `call_method` | `view` | `float32[1,8,4,8]` | `-` | layer1 query RoPE decomposition | transformer.rotary_embedding | Reshape/transpose plus cos/sin/slice/concat arithmetic. |
| 70 | `transpose_5` | `call_method` | `transpose` | `float32[1,4,8,8]` | `-` | layer1 query RoPE decomposition | transformer.rotary_embedding | Reshape/transpose plus cos/sin/slice/concat arithmetic. |
| 71 | `layers_1_attention_rotary_cos` | `get_attr` | `layers.1.attention.rotary.cos` | `float32[1,1,8,8]` | `-` | layer1 query RoPE decomposition | transformer.rotary_embedding | Reshape/transpose plus cos/sin/slice/concat arithmetic. |
| 72 | `mul_15` | `call_function` | `_operator.mul` | `float32[1,4,8,8]` | `-` | layer1 query RoPE decomposition | transformer.rotary_embedding | Reshape/transpose plus cos/sin/slice/concat arithmetic. |
| 73 | `getitem_4` | `call_function` | `_operator.getitem` | `float32[1,4,8,4]` | `-` | layer1 query RoPE decomposition | transformer.rotary_embedding | Reshape/transpose plus cos/sin/slice/concat arithmetic. |
| 74 | `getitem_5` | `call_function` | `_operator.getitem` | `float32[1,4,8,4]` | `-` | layer1 query RoPE decomposition | transformer.rotary_embedding | Reshape/transpose plus cos/sin/slice/concat arithmetic. |
| 75 | `neg_2` | `call_function` | `_operator.neg` | `float32[1,4,8,4]` | `-` | layer1 query RoPE decomposition | transformer.rotary_embedding | Reshape/transpose plus cos/sin/slice/concat arithmetic. |
| 76 | `cat_2` | `call_function` | `torch.cat` | `float32[1,4,8,8]` | `-` | layer1 query RoPE decomposition | transformer.rotary_embedding | Reshape/transpose plus cos/sin/slice/concat arithmetic. |
| 77 | `layers_1_attention_rotary_sin` | `get_attr` | `layers.1.attention.rotary.sin` | `float32[1,1,8,8]` | `-` | layer1 query RoPE decomposition | transformer.rotary_embedding | Reshape/transpose plus cos/sin/slice/concat arithmetic. |
| 78 | `mul_16` | `call_function` | `_operator.mul` | `float32[1,4,8,8]` | `-` | layer1 query RoPE decomposition | transformer.rotary_embedding | Reshape/transpose plus cos/sin/slice/concat arithmetic. |
| 79 | `add_8` | `call_function` | `_operator.add` | `float32[1,4,8,8]` | `-` | layer1 query RoPE decomposition | transformer.rotary_embedding | Reshape/transpose plus cos/sin/slice/concat arithmetic. |
| 80 | `layers_1_attention_wk` | `call_module` | `layers.1.attention.wk` | `float32[1,8,32]` | `Linear` | layer1 K projection | common.linear inside transformer.attention context | Linear module call preserves projection source path. |
| 81 | `view_5` | `call_method` | `view` | `float32[1,8,4,8]` | `-` | layer1 key RoPE decomposition | transformer.rotary_embedding | Second RoPE instance shares rotary buffers but has separate result. |
| 82 | `transpose_6` | `call_method` | `transpose` | `float32[1,4,8,8]` | `-` | layer1 key RoPE decomposition | transformer.rotary_embedding | Second RoPE instance shares rotary buffers but has separate result. |
| 83 | `layers_1_attention_rotary_cos_1` | `get_attr` | `layers.1.attention.rotary.cos` | `float32[1,1,8,8]` | `-` | layer1 key RoPE decomposition | transformer.rotary_embedding | Second RoPE instance shares rotary buffers but has separate result. |
| 84 | `mul_17` | `call_function` | `_operator.mul` | `float32[1,4,8,8]` | `-` | layer1 key RoPE decomposition | transformer.rotary_embedding | Second RoPE instance shares rotary buffers but has separate result. |
| 85 | `getitem_6` | `call_function` | `_operator.getitem` | `float32[1,4,8,4]` | `-` | layer1 key RoPE decomposition | transformer.rotary_embedding | Second RoPE instance shares rotary buffers but has separate result. |
| 86 | `getitem_7` | `call_function` | `_operator.getitem` | `float32[1,4,8,4]` | `-` | layer1 key RoPE decomposition | transformer.rotary_embedding | Second RoPE instance shares rotary buffers but has separate result. |
| 87 | `neg_3` | `call_function` | `_operator.neg` | `float32[1,4,8,4]` | `-` | layer1 key RoPE decomposition | transformer.rotary_embedding | Second RoPE instance shares rotary buffers but has separate result. |
| 88 | `cat_3` | `call_function` | `torch.cat` | `float32[1,4,8,8]` | `-` | layer1 key RoPE decomposition | transformer.rotary_embedding | Second RoPE instance shares rotary buffers but has separate result. |
| 89 | `layers_1_attention_rotary_sin_1` | `get_attr` | `layers.1.attention.rotary.sin` | `float32[1,1,8,8]` | `-` | layer1 key RoPE decomposition | transformer.rotary_embedding | Second RoPE instance shares rotary buffers but has separate result. |
| 90 | `mul_18` | `call_function` | `_operator.mul` | `float32[1,4,8,8]` | `-` | layer1 key RoPE decomposition | transformer.rotary_embedding | Second RoPE instance shares rotary buffers but has separate result. |
| 91 | `add_9` | `call_function` | `_operator.add` | `float32[1,4,8,8]` | `-` | layer1 key RoPE decomposition | transformer.rotary_embedding | Second RoPE instance shares rotary buffers but has separate result. |
| 92 | `layers_1_attention_wv` | `call_module` | `layers.1.attention.wv` | `float32[1,8,32]` | `Linear` | layer1 V projection | common.linear inside transformer.attention context | Linear module call preserves projection source path. |
| 93 | `view_6` | `call_method` | `view` | `float32[1,8,4,8]` | `-` | layer1 value/head layout | transformer.attention operand descriptor | Value projection layout for attention operand. |
| 94 | `transpose_7` | `call_method` | `transpose` | `float32[1,4,8,8]` | `-` | layer1 value/head layout | transformer.attention operand descriptor | Value projection layout for attention operand. |
| 95 | `transpose_8` | `call_method` | `transpose` | `float32[1,4,8,8]` | `-` | layer1 value/head layout | transformer.attention operand descriptor | Value projection layout for attention operand. |
| 96 | `matmul_2` | `call_function` | `torch.matmul` | `float32[1,4,8,8]` | `-` | layer1 attention score/mask/softmax | transformer.attention | Score matmul, scale, causal mask add, and softmax are one semantic attention expression. |
| 97 | `mul_19` | `call_function` | `_operator.mul` | `float32[1,4,8,8]` | `-` | layer1 attention score/mask/softmax | transformer.attention | Score matmul, scale, causal mask add, and softmax are one semantic attention expression. |
| 98 | `layers_1_attention_causal_mask` | `get_attr` | `layers.1.attention.causal_mask` | `float32[1,1,8,8]` | `-` | layer1 attention score/mask/softmax | transformer.attention | Score matmul, scale, causal mask add, and softmax are one semantic attention expression. |
| 99 | `add_10` | `call_function` | `_operator.add` | `float32[1,4,8,8]` | `-` | layer1 attention score/mask/softmax | transformer.attention | Score matmul, scale, causal mask add, and softmax are one semantic attention expression. |
| 100 | `softmax_1` | `call_function` | `torch.softmax` | `float32[1,4,8,8]` | `-` | layer1 attention score/mask/softmax | transformer.attention | Score matmul, scale, causal mask add, and softmax are one semantic attention expression. |
| 101 | `matmul_3` | `call_function` | `torch.matmul` | `float32[1,4,8,8]` | `-` | layer1 attention score/mask/softmax | transformer.attention | Score matmul, scale, causal mask add, and softmax are one semantic attention expression. |
| 102 | `transpose_9` | `call_method` | `transpose` | `float32[1,8,4,8]` | `-` | layer1 attention context layout restore | transformer.attention result descriptor | Layout conversion from [B,H,S,D] back to [B,S,H]. |
| 103 | `contiguous_1` | `call_method` | `contiguous` | `float32[1,8,4,8]` | `-` | layer1 attention context layout restore | transformer.attention result descriptor | Layout conversion from [B,H,S,D] back to [B,S,H]. |
| 104 | `view_7` | `call_method` | `view` | `float32[1,8,32]` | `-` | layer1 attention context layout restore | transformer.attention result descriptor | Layout conversion from [B,H,S,D] back to [B,S,H]. |
| 105 | `layers_1_attention_wo` | `call_module` | `layers.1.attention.wo` | `float32[1,8,32]` | `Linear` | layer1 attention output projection | common.linear inside transformer.attention context | Linear module call preserves projection source path. |
| 106 | `add_11` | `call_function` | `_operator.add` | `float32[1,8,32]` | `-` | layer1 attention residual | common.residual_add inside transformer.decoder_layer | Exact-shape residual after attention output projection. |
| 107 | `mul_20` | `call_function` | `_operator.mul` | `float32[1,8,32]` | `-` | layer1 feed-forward RMSNorm decomposition | transformer.rms_norm | Recover semantic RMSNorm from arithmetic and weight path. |
| 108 | `mean_3` | `call_function` | `torch.mean` | `float32[1,8,1]` | `-` | layer1 feed-forward RMSNorm decomposition | transformer.rms_norm | Recover semantic RMSNorm from arithmetic and weight path. |
| 109 | `add_12` | `call_function` | `_operator.add` | `float32[1,8,1]` | `-` | layer1 feed-forward RMSNorm decomposition | transformer.rms_norm | Recover semantic RMSNorm from arithmetic and weight path. |
| 110 | `rsqrt_3` | `call_function` | `torch.rsqrt` | `float32[1,8,1]` | `-` | layer1 feed-forward RMSNorm decomposition | transformer.rms_norm | Recover semantic RMSNorm from arithmetic and weight path. |
| 111 | `mul_21` | `call_function` | `_operator.mul` | `float32[1,8,32]` | `-` | layer1 feed-forward RMSNorm decomposition | transformer.rms_norm | Recover semantic RMSNorm from arithmetic and weight path. |
| 112 | `layers_1_ffn_norm_weight` | `get_attr` | `layers.1.ffn_norm.weight` | `float32[32]` | `-` | layer1 feed-forward RMSNorm decomposition | transformer.rms_norm | Recover semantic RMSNorm from arithmetic and weight path. |
| 113 | `mul_22` | `call_function` | `_operator.mul` | `float32[1,8,32]` | `-` | layer1 feed-forward RMSNorm decomposition | transformer.rms_norm | Recover semantic RMSNorm from arithmetic and weight path. |
| 114 | `layers_1_feed_forward_gate_proj` | `call_module` | `layers.1.feed_forward.gate_proj` | `float32[1,8,88]` | `Linear` | layer1 SwiGLU decomposition | transformer.swiglu | Gate projection, SiLU, up projection, multiply, and down projection. |
| 115 | `silu_1` | `call_function` | `torch.nn.functional.silu` | `float32[1,8,88]` | `-` | layer1 SwiGLU decomposition | transformer.swiglu | Gate projection, SiLU, up projection, multiply, and down projection. |
| 116 | `layers_1_feed_forward_up_proj` | `call_module` | `layers.1.feed_forward.up_proj` | `float32[1,8,88]` | `Linear` | layer1 SwiGLU decomposition | transformer.swiglu | Gate projection, SiLU, up projection, multiply, and down projection. |
| 117 | `mul_23` | `call_function` | `_operator.mul` | `float32[1,8,88]` | `-` | layer1 SwiGLU decomposition | transformer.swiglu | Gate projection, SiLU, up projection, multiply, and down projection. |
| 118 | `layers_1_feed_forward_down_proj` | `call_module` | `layers.1.feed_forward.down_proj` | `float32[1,8,32]` | `Linear` | layer1 SwiGLU decomposition | transformer.swiglu | Gate projection, SiLU, up projection, multiply, and down projection. |
| 119 | `add_13` | `call_function` | `_operator.add` | `float32[1,8,32]` | `-` | layer1 feed-forward residual | common.residual_add inside transformer.decoder_layer | Exact-shape residual after feed-forward. |
| 120 | `mul_24` | `call_function` | `_operator.mul` | `float32[1,8,32]` | `-` | final RMSNorm decomposition | transformer.rms_norm | Final model norm before logits projection. |
| 121 | `mean_4` | `call_function` | `torch.mean` | `float32[1,8,1]` | `-` | final RMSNorm decomposition | transformer.rms_norm | Final model norm before logits projection. |
| 122 | `add_14` | `call_function` | `_operator.add` | `float32[1,8,1]` | `-` | final RMSNorm decomposition | transformer.rms_norm | Final model norm before logits projection. |
| 123 | `rsqrt_4` | `call_function` | `torch.rsqrt` | `float32[1,8,1]` | `-` | final RMSNorm decomposition | transformer.rms_norm | Final model norm before logits projection. |
| 124 | `mul_25` | `call_function` | `_operator.mul` | `float32[1,8,32]` | `-` | final RMSNorm decomposition | transformer.rms_norm | Final model norm before logits projection. |
| 125 | `norm_weight` | `get_attr` | `norm.weight` | `float32[32]` | `-` | final RMSNorm decomposition | transformer.rms_norm | Final model norm before logits projection. |
| 126 | `mul_26` | `call_function` | `_operator.mul` | `float32[1,8,32]` | `-` | final RMSNorm decomposition | transformer.rms_norm | Final model norm before logits projection. |
| 127 | `output` | `call_module` | `output` | `float32[1,8,128]` | `Linear` | logits projection | common.linear plus common.output_logits boundary | Final projection to vocabulary logits. |
| 128 | `output_1` | `output` | `output` | `float32[1,8,128]` | `Linear` | model output | common.output_logits | Returned per-token logits [1,8,128]. |

D4 summary: embedding is visible as a module call; linear projections remain
module calls with source paths; RMSNorm, RoPE, causal masking, attention,
SwiGLU, residuals, and decoder-layer boundaries require semantic recovery
from node order, source paths, and tensor descriptors.

## D5 Drift Test Evidence

Focused Llama command:

```sh
docker run --rm \
  -v /Users/shinmingliu/.codex/worktrees/a2fe/open64:/src \
  -w /src \
  open64:torch2whirl-torch-test \
  sh -c 'PYTHONDONTWRITEBYTECODE=1 \
    PYTHONPATH=/src/osprey/torch2whirl/python:/src/osprey/torch2whirl/python/tests \
    python3 -m unittest \
    /src/osprey/torch2whirl/python/tests/test_llama2_capture_optional.py'
```

Focused result:

```text
Ran 6 tests in 0.255s

OK
```

Standard torch lane:

```sh
OPEN64_TORCH2WHIRL_BUILD_DIR=/private/tmp/open64-torch2whirl-llama2 \
  sh osprey/torch2whirl/scripts/run_torch_docker_test.sh
```

Standard torch result:

```text
Ran 35 tests in 2.198s

OK
```

The D5 tests compare the current normalized FX graph against
`python/tests/golden/llama2_prefill_fx.txt`, prove an altered graph string does
not match, and verify that every normalized node name has a census row.

## Infrastructure Contracts Needed

D0 requests no new native infrastructure. L0-L1 discovery remains independent
of common/com implementation.
