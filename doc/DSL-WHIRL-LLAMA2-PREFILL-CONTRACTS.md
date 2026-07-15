# DSL WHIRL Llama 2 Prefill Contracts

## Status And Purpose

This document publishes the first native DSL WHIRL contract for a static,
inference-only Llama 2 prompt evaluation.  It completes infrastructure plan
item 22.  It fixes the semantic surface that common/com and torch2whirl may
implement in later items; it does not itself change an enum, WN layout, mapped
image, reader, writer, gatekeeper, or lowering pass.

The evidence is the deterministic tiny Llama 2 fixture and 129-node FX census
on `codex/torch2whirl-python-fe` at commit `53bbfddf`.  The reference profile is:

```text
vocabulary=128
hidden_size=32
intermediate_size=88
decoder_layers=2
query_heads=4
kv_heads=4
head_dim=8
batch=1
sequence=8
token_dtype=int64
activation_dtype=float32
cache_mode=none
```

This is a normative contract for the supported profile.  Terms such as MUST,
MUST NOT, and REQUIRED identify compatibility requirements.

## Compatibility Rules

1. Existing `DSL_OPERATOR` values 0 through 13 and their released schemas MUST
   remain unchanged.
2. New logical values MUST be appended in the order published below.
3. A new semantic version of an existing stable name reuses that name's
   logical operator identity.  It does not relabel an old image.
4. Readers and gatekeepers MUST select a schema by stable name and exact
   version.  They MUST NOT silently interpret an older record with a newer
   schema.
5. Unknown names, unknown versions, missing attributes, extra operands, and
   unsupported profile features MUST receive stable rejection diagnostics.
6. Physical `OPR_DSL` remains a private escape tag.  Dumps, traces, diagnostics,
   and frontend APIs expose only the logical names in this document.
7. The fixed WN representation and existing DSL image tables remain unchanged
   by item 22.  Implementation work must use the established mapped-image and
   ELF mechanisms.

## Published Logical IDs

The next append-only `DSL_OPERATOR` values are reserved as follows:

| Value | Logical enum | Stable name | Version |
| ---: | --- | --- | ---: |
| 14 | `OPR_DSLRESHAPE` | `common.reshape` | 1 |
| 15 | `OPR_DSLTRANSPOSE` | `common.transpose` | 1 |
| 16 | `OPR_DSLTOKENEMBEDDING` | `transformer.token_embedding` | 1 |
| 17 | `OPR_DSLRMSNORM` | `transformer.rms_norm` | 1 |
| 18 | `OPR_DSLROTARYEMBEDDING` | `transformer.rotary_embedding` | 1 |
| 19 | `OPR_DSLATTENTION` | `transformer.attention` | 1 |
| 20 | `OPR_DSLSWIGLU` | `transformer.swiglu` | 1 |

The following additive schemas reuse existing logical identities:

| Logical enum | Stable name | New version | Reason |
| --- | --- | ---: | --- |
| `OPR_DSLLINEAR` | `common.linear` | 3 | Bias-free rank-generic projection |
| `OPR_DSLMATMUL` | `common.matmul` | 2 | Exact-prefix batched contraction |
| `OPR_DSLOUTPUTLOGITS` | `common.output_logits` | 3 | Per-token sequence logits |

The implementation must preserve the version-1 matrix-only matmul, version-2
ResNet linear, and version-2 classifier-logits contracts.

## Canonical Attribute Values

Attributes are typed operation parameters.  Their canonical text form in the
current DSL image is:

1. Boolean: `true` or `false`.
2. Signed integer: base-10 with an optional leading minus sign.
3. Unsigned integer: base-10 without a sign.
4. Floating point: finite base-10 text accepted by the common numeric parser.
5. Integer list: comma-separated canonical integers without brackets.
6. Enum: the lowercase token published by the owning schema.

Source module paths, FX node names, projection roles, layer ordinals,
diagnostic ownership, and lowering hints are compiler metadata.  They MUST NOT
be added to the opcode attribute set or tensor type equivalence.

## Existing Contracts Reused Unchanged

The first profile reuses these released contracts without a new version:

1. `common.model_input.v2` creates the `int64[B,S]` token value with
   `attr.input_ordinal=0`.
2. `common.tensor_const.v1` plus the external tensor reference contract carries
   embedding, normalization, projection, rotary-table, and output weights.
3. `common.residual_add.v2` represents both exact-shape residual additions in
   each decoder layer with `attr.broadcast_rule=none`,
   `attr.shape_check=exact`, and `attr.residual_path=true`.

External file paths, tensor keys, byte ranges, and checksums remain value
storage metadata and do not enter tensor type identity.

## Common Substrate Schemas

### `common.reshape.v1`

```text
kids:
  kid0 = input tensor
attributes:
  attr.target_shape = comma-separated positive dimensions
result:
  same dtype and semantic representation as kid0
  shape = attr.target_shape
shape rule: view
effect model: pure
lowering owner: common VHO DSL lowering
diagnostic prefix: DOPC_COMMON_RESHAPE_V1
```

The input and result element counts MUST match.  The first profile accepts only
static positive dimensions and no inferred `-1` dimension.  A reshape that
requires an unrepresented materialization is rejected rather than treated as
an identity.

### `common.transpose.v1`

```text
kids:
  kid0 = input tensor
attributes:
  attr.permutation = comma-separated zero-based axes
result:
  same dtype and semantic representation as kid0
  shape[i] = kid0.shape[attr.permutation[i]]
shape rule: view
effect model: pure
lowering owner: common VHO DSL lowering
diagnostic prefix: DOPC_COMMON_TRANSPOSE_V1
```

The permutation length MUST equal the rank and contain each axis exactly once.
The initial attention layout transitions use `0,2,1,3` between BSHD and BHSD.

### `common.linear.v3`

```text
kids:
  kid0 = activation [...,I]
  kid1 = weight [O,I]
attributes:
  attr.has_bias=false
  attr.transpose_input=false
  attr.transpose_weight=true
  attr.weight_layout=OI
result:
  shape = [...,O]
  dtype and semantic representation compatible with kid0
shape rule: contraction
effect model: pure
lowering owner: common VHO DSL lowering
diagnostic prefix: DOPC_COMMON_LINEAR_V3
```

Version 3 has exactly two kids and is intentionally bias-free.  It accepts a
static activation rank of at least two.  Projection identity such as Q, K, V,
O, gate, up, down, or logits is value lineage/compiler context and is not a
common linear attribute.  Activation and weight dtypes MUST match.

### `common.matmul.v2`

```text
kids:
  kid0 = tensor [...,M,K]
  kid1 = tensor [...,K,N] before transpose interpretation
attributes:
  attr.transpose_kid0 = true|false
  attr.transpose_kid1 = true|false
  attr.batch_rule=exact
  attr.accum_dtype=float32
result:
  exact common batch prefix followed by effective [M,N]
shape rule: contraction
effect model: pure
lowering owner: common VHO DSL lowering
diagnostic prefix: DOPC_COMMON_MATMUL_V2
```

Both operands MUST have rank at least two.  Version 2 requires equal batch
prefixes and does not broadcast batch dimensions.  Transpose flags apply only
to the final two dimensions.  The first profile uses:

```text
BHSD x BHSD, transpose_kid1=true  -> BHSS
BHSS x BHSD, transpose_kid1=false -> BHSD
```

### `common.output_logits.v3`

```text
kids:
  kid0 = logits [B,S,V]
attributes:
  attr.semantic=token_logits
  attr.sequence_axis=-2
  attr.vocabulary_axis=-1
result:
  identity descriptor relative to kid0
shape rule: identity
effect model: pure
lowering owner: model-boundary lowering
diagnostic prefix: DOPC_COMMON_OUTPUT_LOGITS_V3
```

The first profile requires rank 3 and positive B, S, and V.  Version 2 remains
the classifier-logits contract used by ResNet.

## Transformer Expression Schemas

### Token Input Decision

Item 22 does not allocate `transformer.input_tokens`.  The token value is
created by `common.model_input.v2` with `attr.input_ordinal=0` and is declared
as the first input of `transformer.prefill.v1`.  The prefill and embedding
gatekeepers jointly require `int64[B,S]` and enforce the vocabulary contract.
This avoids an identity wrapper value while retaining domain checks.

### `transformer.token_embedding.v1`

```text
kids:
  kid0 = token IDs [B,S], dtype int64
  kid1 = embedding weight [V,H], floating dtype
attributes:
  attr.padding_idx=none
  attr.bounds_policy=runtime_check
result:
  [B,S,H], dtype and representation derived from kid1
shape rule: gather after domain verification
effect model: pure
promotion state: partial promotion to common.gather
diagnostic prefix: DOPC_TRANSFORMER_TOKEN_EMBEDDING_V1
```

The vocabulary size is `V` from the weight descriptor and is not duplicated as
an attribute.  Every token value must satisfy `0 <= token < V`; static proof or
a retained runtime guard is required before lowering.  Padding indices are not
supported in version 1.

### `transformer.rms_norm.v1`

```text
kids:
  kid0 = activation [...,H]
  kid1 = scale [H]
attributes:
  attr.axis=-1
  attr.epsilon=<positive finite float>
  attr.accum_dtype=float32
result:
  identity shape and activation dtype relative to kid0
shape rule: reduction plus identity result
effect model: pure
promotion state: partial promotion to common.normalization_base
diagnostic prefix: DOPC_TRANSFORMER_RMS_NORM_V1
```

Scale length MUST equal the normalized dimension.  The gatekeeper preserves
the Llama RMS definition and MUST NOT accept mean subtraction, bias, training
state, or a different axis under version 1.

### `transformer.rotary_embedding.v1`

```text
kids:
  kid0 = Q or K activation [B,H,S,D]
  kid1 = cosine table [1,1,S,D]
  kid2 = sine table [1,1,S,D]
attributes:
  attr.head_layout=BHSD
  attr.sequence_axis=2
  attr.feature_axis=3
  attr.pairing=half_split
  attr.position_mode=zero_based_static
  attr.position_offset=0
result:
  identity descriptor relative to kid0
shape rule: identity after paired rotation
effect model: pure
promotion state: partial promotion after transformer verification
diagnostic prefix: DOPC_TRANSFORMER_ROTARY_EMBEDDING_V1
```

`D` MUST be positive and even.  Cosine and sine descriptors MUST cover exactly
the static sequence and feature dimensions and use a compatible floating
dtype.  Q and K use separate single-result nodes.  Interleaved pairing,
dynamic positions, partial rotary dimensions, and nonzero offsets require a
new version.

### Causal Mask Decision

Item 22 does not allocate a tensor-producing `transformer.causal_mask` node.
Version-1 masking is the required `attr.mask_mode=causal` policy of attention.
The frontend may recognize the fixture's canonical static triangular buffer
as evidence for that policy, but it MUST NOT pass an arbitrary tensor through
this contract.  Padding, additive user masks, and runtime masks require a new
version with an explicit operand and shape rules.

### `transformer.attention.v1`

```text
kids:
  kid0 = rotated query [B,H,S,D]
  kid1 = rotated key [B,H,S,D]
  kid2 = value [B,H,S,D]
attributes:
  attr.execution_mode=full_sequence
  attr.mask_mode=causal
  attr.head_layout=BHSD
  attr.query_heads=<positive integer>
  attr.kv_heads=<positive integer>
  attr.head_dim=<positive integer>
  attr.scale_mode=inverse_sqrt_head_dim
  attr.softmax_axis=-1
  attr.softmax_accum_dtype=float32
  attr.cache_mode=none
result:
  context [B,H,S,D]
shape rule: verified attention contraction
effect model: pure
promotion state: partial promotion to common.matmul/softmax/matmul
diagnostic prefix: DOPC_TRANSFORMER_ATTENTION_V1
```

All three descriptors MUST agree on B, H, S, D, activation dtype, and semantic
representation.  `query_heads`, `kv_heads`, and `head_dim` MUST equal H, H, and
D respectively.  Version 1 therefore rejects grouped-query attention.  The
gatekeeper verifies causal policy and scale before partial promotion.
No cache object, mask tensor, state effect, or hidden result is permitted.

### `transformer.swiglu.v1`

```text
kids:
  kid0 = gate projection [B,S,I]
  kid1 = up projection [B,S,I]
attributes:
  attr.activation=silu
result:
  silu(kid0) * kid1, shape [B,S,I]
shape rule: exact-shape elementwise
effect model: pure
promotion state: partial promotion to common.silu plus common.mul
diagnostic prefix: DOPC_TRANSFORMER_SWIGLU_V1
```

The gate, up, and later down projections remain visible as
`common.linear.v3`.  This mirrors the decision to keep attention projections
outside `transformer.attention` and gives VHO optimization direct access to
projection operations.  Version 1 rejects mismatched descriptors and any
activation other than SiLU.

## Region Contracts

Region contracts use the existing pointer-free DSL REGION image and do not
allocate `DSL_OPERATOR` values.

### `transformer.decoder_layer.v1`

The region body MUST preserve this semantic order:

```text
rms_norm(attention)
Q/K/V common.linear.v3 projections
head reshape/transpose
Q and K rotary_embedding
attention
context transpose/reshape
O common.linear.v3 projection
residual_add
rms_norm(feed_forward)
gate and up common.linear.v3 projections
swiglu
down common.linear.v3 projection
residual_add
```

The incoming activation is `INPUT` ordinal 0.  The final activation is
`OUTPUT|RESULT` ordinal 0.  Every value defined outside the region and used
inside it MUST be declared as an `INPUT` in deterministic first-use order.
No intermediate activation may escape without an explicit interface record.
Source metadata records the module path and layer ordinal; these fields do not
alter the region contract version.

### `transformer.prefill.v1`

Version 1 means all of the following without additional region attributes:

```text
execution_mode=full_sequence
mask_mode=causal
sequence_mode=static
cache_mode=none
```

The region body MUST contain token embedding, ordered decoder-layer regions,
final RMSNorm, bias-free logits projection, and
`common.output_logits.v3`.  Token IDs are `INPUT` ordinal 0.  The final logits
are `OUTPUT|RESULT` ordinal 0.  External values follow the same deterministic
first-use input rule as decoder layers.

The embedding vocabulary dimension and final logits vocabulary dimension MUST
match.  Hidden size, head count, head dimension, intermediate size, decoder
layer count, and sequence length are derived from canonical descriptors,
operator attributes, and region topology and MUST be mutually consistent.

The prefill region is a structural contract, not an expression operator.  It
does not hide the expression nodes in its body.  Cached prefill requires a new
version with declared state and effects.

## Gatekeeper Order

The native verifier applies checks in this order:

1. Image/header and exact name/version validity.
2. Direct-kid count, ordered value references, and result ownership.
3. Complete canonical tensor descriptors.
4. Operator attribute presence, type, spelling, and allowed enum values.
5. Per-operator dtype, rank, shape, layout, and representation rules.
6. Region interface, nesting, topology, source position, and result checks.
7. Profile exclusions and state/effect absence.
8. Promotion or lowering eligibility.

Failure at any level prevents binary finalization or VHO lowering.  The
frontend may diagnose earlier, but the C++ gatekeeper remains authoritative.

## Stable Rejection Classes

The first implementation must distinguish at least these diagnostic classes:

| Prefix | Rejection class |
| --- | --- |
| `DOPC_LLAMA_NAME_VERSION` | Unknown logical name or unsupported version |
| `DOPC_LLAMA_OPERAND` | Missing, extra, reordered, or unresolved kid |
| `DOPC_LLAMA_ATTRIBUTE` | Missing, malformed, or unsupported static attribute |
| `DOPC_LLAMA_DESCRIPTOR` | Dtype, rank, shape, layout, or representation mismatch |
| `DOPC_LLAMA_TOPOLOGY` | Invalid decoder or prefill region structure |
| `DOPC_LLAMA_TOKEN_BOUNDS` | Token IDs not proven or guarded within vocabulary |
| `DOPC_LLAMA_UNSUPPORTED_STATE` | KV cache, mutation, training state, or dropout |
| `DOPC_LLAMA_UNSUPPORTED_SHAPE` | Dynamic sequence or unsupported broadcasting |
| `DOPC_LLAMA_UNSUPPORTED_MASK` | Padding, arbitrary, or runtime mask in version 1 |
| `DOPC_LLAMA_UNSUPPORTED_GQA` | Query and KV head counts differ in version 1 |

Diagnostics include the logical operator/region name, version, source
position, module path when available, and the violated contract field.  They
must not expose the physical escape tag or mapped record address.

## Explicitly Deferred Features

The following features are not implied by version 1:

1. Tokenizer execution inside the graph.
2. Dynamic batch or sequence dimensions.
3. Padding or arbitrary attention masks.
4. Grouped-query or multi-query attention.
5. KV-cache allocation, append, update, or paged attention.
6. Training, dropout, random state, or mutable buffers.
7. Quantized weights or activations.
8. Tensor parallel, sharding, collectives, or distributed checkpoints.
9. Partial/interleaved/dynamic RoPE variants.
10. Licensed Llama weights or network-dependent model loading.

Each deferred feature requires an additive reviewed contract.  Existing v1
artifacts remain readable and retain their original semantics.

## Implementation Gates After Item 22

1. Append the published logical IDs without changing existing values.
2. Teach logical lookup to preserve multiple exact versions for stable common
   names.
3. Add builder result inference and gatekeeper checks per schema.
4. Add mapped-image write/reopen and malformed-record tests.
5. Print logical names, versions, attributes, descriptors, and regions through
   `ir_b2a -st -src` without physical escape evidence.
6. Add VHO DSL lowering only after the operator and region gatekeepers pass.
7. Migrate torch2whirl through opaque values and region handles.
8. Certify `llama2.B`, external tensor payloads, `llama2.T`, and `openpy -O0`
   across a producer/consumer process boundary.
