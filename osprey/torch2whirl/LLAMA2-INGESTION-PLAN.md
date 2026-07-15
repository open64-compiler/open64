# Torch2whirl Llama 2 Ingestion Plan

## Purpose

This is the torch2whirl-focused plan for ingesting a Llama 2 transformer model,
emitting native very-high-level DSL WHIRL, publishing external tensor data, and
producing a certified binary WHIRL artifact.  It is the focal point for
frontend implementation and peer discussion after the ResNet ingestion path.

Native operator definitions, tensor/type rules, regions, mapped-image tables,
gatekeeper behavior, logical printing, and VHO DSL lowering remain owned by
`doc/WHIRL-DSL-INFRASTRUCTURE.md`.  This plan consumes those contracts through
opaque builder handles and must not reproduce their physical representation in
Python.

## Architectural References

The initial semantic reference is Meta's Llama 2 implementation:

- `https://github.com/meta-llama/llama/blob/main/llama/model.py`
- `https://github.com/meta-llama/llama`

The Meta repository is a semantic reference, not a source dependency.  Its
FairScale, CUDA, distributed-checkpoint, and mutable KV-cache requirements are
not imported into the first compiler fixture.

Hugging Face Transformers is a later interoperability reference:

- `https://github.com/huggingface/transformers/blob/main/src/transformers/models/llama/modeling_llama.py`

The first certification does not require `transformers`, network access, a
tokenizer, or licensed Llama 2 weights.

## Ownership Boundary

Torch2whirl owns:

1. A dependency-light PyTorch Llama 2 fixture and deterministic tiny profile.
2. PyTorch graph capture and traversal.
3. Mapping captured operations and modules to published common and transformer
   DSL operator contracts.
4. Static opcode attribute extraction and observed tensor descriptors.
5. Source module, layer, FX node, and source-position context.
6. State-dict discovery and deterministic external tensor publication.
7. Calls through opaque native builder value and region handles.
8. Frontend diagnostics, unsupported-construct failures, and artifact tests.

Torch2whirl does not own logical opcode allocation, WN layout, symbol or type
table mutation, RID/region image tables, gatekeeper rules, ELF sections,
mapped-image finalization, VHO lowering, or backend dispatch.

## First Certified Profile

The first profile is a small, static-shape, inference-only Llama 2 prefill:

| Property | Initial value |
| --- | --- |
| Vocabulary | 128 or 256 tokens |
| Hidden dimension | 32 or 64 |
| Decoder layers | 2 |
| Attention heads | 4 |
| KV heads | Equal to attention heads initially |
| Batch size | 1 |
| Sequence length | 8 |
| Element type | `float32` for activations and weights |
| Token type | Supported integer tensor type |
| KV cache | Disabled |
| Weights | Deterministic local initialization |
| Input | `input_ids` tensor |
| Output | Per-token logits |

The fixture belongs at:

```text
osprey/torch2whirl/python/tests/models/__init__.py
osprey/torch2whirl/python/tests/models/llama2_model.py
```

It preserves Llama 2 computation while using ordinary `torch.nn` and tensor
operations.  It must not import FairScale or allocate CUDA-only state.

## Semantic Preservation Rules

1. Keep token embedding, RMSNorm, rotary position encoding, attention, SwiGLU,
   residual paths, decoder-layer boundaries, and output logits recognizable in
   very-high-level WHIRL.
2. Use common operators only after their domain-independent contracts are
   sufficient.  Implementation similarity alone is not promotion.
3. Keep attention domain-visible until mask semantics, head layout, scale,
   position encoding, and cache mode pass the transformer gatekeeper.
4. Keep RMSNorm domain-visible until epsilon, reduction axes, scale tensor,
   accumulation type, and result rules are verified.
5. Represent decoder layers and the complete transformer with structured
   regions when the common region substrate contract is available.
6. Keep static operation parameters in opcode attributes.  Keep module paths,
   node names, diagnostics, pass ownership, and lowering hints in compiler
   metadata.
7. Intern complete tensor descriptors in the `TY` domain.  Python passes
   descriptor requests and receives opaque handles.
8. Do not decompose semantically rich transformer operations merely to fit the
   operators already available from ResNet.
9. Do not represent tokenizer behavior as tensor computation.  A future
   tokenizer contract belongs at the model boundary.
10. Do not model mutable KV cache as ordinary metadata.  Decode support waits
    for abstract-state and effect contracts.

## Proposed Operator Boundary

The exact names, versions, arities, attributes, and promotion state are not
frontend-owned.  The infrastructure plan must publish them before native use.
The initial operator census should classify the following needs.

Common substrate candidates:

```text
common.model_input
common.tensor_const
common.gather or a reviewed common embedding primitive
common.reshape
common.transpose
common.slice
common.concat
common.add
common.mul
common.matmul with batched contraction support
common.linear
common.silu
common.softmax
common.residual_add
common.output_logits
```

Transformer-domain candidates:

```text
transformer.input_tokens
transformer.token_embedding
transformer.rms_norm
transformer.rotary_embedding
transformer.causal_mask
transformer.attention
transformer.swiglu
transformer.decoder_layer contract
transformer.prefill region contract
```

Domain operators may lower to common substrate operations only after domain
verification.  For example, verified attention may lower to batched matmul,
softmax, and batched matmul while retaining source and contract evidence.

## Prefill Region And Attention Expression

Prefill is an execution mode over a prompt, not one tensor arithmetic
primitive.  Represent it with an `OPR_REGION` carrying a logical, versioned
`transformer.prefill` contract.  Do not create a monolithic prefill expression
whose operands hide the model body.

The first contract is a stateless full-sequence prompt evaluation:

```text
OPR_REGION transformer.prefill.v1
  input:  token_ids [B,S]
  result: logits [B,S,V]
  attr.execution_mode=full_sequence
  attr.mask_mode=causal
  attr.sequence_mode=static
  attr.cache_mode=none
```

The region body preserves the high-level expression operators and nested
decoder-layer regions:

```text
transformer.token_embedding
OPR_REGION transformer.decoder_layer.v1
  transformer.rms_norm
  common.linear                 # Q projection
  common.linear                 # K projection
  common.linear                 # V projection
  transformer.rotary_embedding # rotated Q
  transformer.rotary_embedding # rotated K
  transformer.attention
  common.linear                 # output projection
  common.residual_add
  transformer.rms_norm
  transformer.swiglu
  common.residual_add
...
transformer.rms_norm
common.linear
common.output_logits
```

`transformer.attention.v1` is a pure, single-result expression inside the
region.  Its direct operands are the rotated query, rotated key, and value:

```text
context = transformer.attention.v1(
    kid0=rotated_query,
    kid1=rotated_key,
    kid2=value)
```

Its reviewed static attributes should cover at least:

```text
attr.execution_mode=full_sequence
attr.mask_mode=causal
attr.head_layout=B,H,S,D
attr.query_heads=<positive integer>
attr.kv_heads=<positive integer>
attr.head_dim=<positive integer>
attr.scale_mode=inverse_sqrt_head_dim
attr.softmax_axis=-1
attr.softmax_accum_dtype=float32
attr.cache_mode=none
```

The result descriptor is `[B,H,S,D]`.  Before partial promotion, the
transformer gatekeeper verifies Q/K/V rank and dimensions, batch and sequence
compatibility, query-head/KV-head compatibility, mask policy, head layout,
scale, softmax axis and accumulation type, and the absence of state effects
when `cache_mode=none`.  Only then may VHO lower the expression to common
batched matmul, mask application, softmax, and batched matmul.

Apply rotary position encoding separately to Q and K so each expression has
one result.  The two nodes share the position/frequency contract but do not
introduce an artificial multiple-result operator.

Production prefill commonly initializes the K/V cache for later decoding.
That behavior is deliberately absent from `transformer.prefill.v1`.  A cached
prefill contract must be an additive, versioned extension with declared cache
state, read/modify effects, ordering, and region results.  It must not silently
broaden version 1 or hide cache mutation in compiler metadata.

## First Subagent Batch: Observable Discovery

The first torch2whirl subagent handoff covers L0 and L1 only.  It is deliberately
independent of new common/com implementation so the frontend can discover the
real PyTorch graph while the main agent reviews the resulting requirements.

### Scope

The subagent owns these deliverables:

```text
osprey/torch2whirl/python/tests/models/__init__.py
osprey/torch2whirl/python/tests/models/llama2_model.py
osprey/torch2whirl/python/tests/test_llama2_capture_optional.py
osprey/torch2whirl/python/tests/golden/llama2_prefill_fx.txt
osprey/torch2whirl/LLAMA2-OPERATOR-CENSUS.md
```

The exact test and golden filenames may follow an existing local convention if
one is established during implementation, but the human-readable census and a
machine-checked graph-drift artifact are required.

This batch must not:

1. Allocate or change logical DSL operator IDs or versions.
2. Add physical WN encodings, mapped-image rows, or ELF sections.
3. Implement speculative common/com operators.
4. Lower attention, RMSNorm, RoPE, or SwiGLU in Python.
5. Add mandatory FairScale, Transformers, CUDA, network, tokenizer, or licensed
   checkpoint dependencies.
6. Claim native Llama 2 ingestion or `.B` certification.

### Progress Dashboard

Progress is earned only when a checkpoint exit test passes.  Do not report a
partial checkpoint as complete.

| Checkpoint | Weight | Initial status | Review evidence |
| --- | ---: | --- | --- |
| D0: Record baseline and exact tiny configuration | 5% | complete | Baseline tests and configuration in `LLAMA2-OPERATOR-CENSUS.md` |
| D1: Implement deterministic tiny Llama 2 fixture | 20% | complete | `python/tests/models/llama2_model.py` |
| D2: Pass eager shape/dtype/repeatability tests | 15% | complete | Docker `python_torch_test` ran 32 tests OK |
| D3: Capture and normalize the real PyTorch graph | 20% | pending | Preserved normalized graph artifact |
| D4: Complete semantic operator census | 25% | pending | Human-readable census with every node classified |
| D5: Add and pass graph/census drift tests | 10% | pending | Golden comparison and negative/drift test |
| D6: Publish main-infrastructure handoff | 5% | pending | Contract requests, unsupported cases, and blockers |

The batch is 100% complete only after D0-D6 pass.  The percentage is the sum of
completed checkpoint weights, not an estimate of effort spent.

### Required Status Updates

At kickoff and after every checkpoint, update this dashboard on the subagent
branch and report:

1. Completed checkpoint and cumulative percentage.
2. Files added or changed.
3. Exact commands run and their result.
4. Paths to the model, normalized graph, census, and any retained diagnostics.
5. Newly discovered PyTorch node forms or semantic ambiguities.
6. Infrastructure contracts requested from the main agent.
7. Current blockers, without adding a frontend workaround.

Use one reviewable commit for D0-D2, one for D3-D5, and one for D6 unless a
failing capture requires a smaller diagnostic commit.  Push each completed
commit so progress is visible outside the subagent session.

### D0-D2 Exit Contract

The fixture must use ordinary PyTorch operations, deterministic initialization,
and the first certified profile in this document.  Eager tests must verify:

1. Input IDs have the selected supported integer dtype and shape `[1,8]`.
2. Logits have shape `[1,8,vocab_size]` and the selected floating dtype.
3. Repeated inference with the same model and input is identical.
4. The model is in evaluation mode and performs no training mutation.
5. The first profile allocates or updates no KV cache.
6. All parameters are discoverable through the state dict.

### D3-D5 Exit Contract

Capture with the frontend mechanism that torch2whirl intends to support.  The
normalized graph artifact must remove unstable object addresses while
preserving node order, target, ordered dependencies, source/module context,
and observed tensor descriptors.

The census contains one row per captured node with:

```text
ordinal
framework node kind
framework target
source/module path
ordered operands
input and result dtype/rank/shape
static parameters
external tensor references
semantic classification
proposed DSL contract
reason for preserving, promoting, or rejecting the node
```

The census must distinguish semantic model constructs from incidental PyTorch
decomposition.  It must explicitly report how embedding, RMSNorm, Q/K/V
projection, RoPE, causal masking, attention, SwiGLU, residuals, decoder-layer
boundaries, and final logits appear in the captured graph.

### D6 Main-Agent Handoff

The final report groups requirements into:

1. Existing native contracts that can be reused unchanged.
2. Existing contracts that appear insufficient and why.
3. Proposed new common-substrate contracts.
4. Proposed transformer expression contracts.
5. Proposed prefill and decoder-layer region contracts.
6. Required tensor descriptor, source-position, external-data, or region APIs.
7. State/effect requirements deferred from the no-cache profile.
8. Unsupported graph forms and stable diagnostics.

The report is a proposal for common/com review, not authority to allocate an
opcode or change binary WHIRL.  The main agent uses it to close infrastructure
item 22 and open the reviewed native implementation gates.

## Frontend Queue

### L0: Freeze The Fixture And Reference Contract

- [ ] Add the tiny dependency-light Llama 2 model under `python/tests/models`.
- [ ] Record the exact model configuration and deterministic initialization.
- [ ] Add eager PyTorch tests for shape, dtype, repeatability, and inference
  mode.
- [ ] Document intentional differences from Meta's runtime implementation.
- [ ] Keep real checkpoints and tokenizers outside the repository.

Exit gate: peers can review one stable model without downloading dependencies
or licensed data.

### L1: Capture And Operator Census

- [ ] Capture the tiny model with the selected PyTorch graph mechanism.
- [ ] Produce a stable, reviewable census of call-module, call-function,
  call-method, view, indexing, and scalar operations.
- [ ] Associate every captured node with a proposed common operator,
  transformer operator, static constant, or explicit unsupported diagnostic.
- [ ] Separate compiler-required semantics from PyTorch implementation noise.
- [ ] Add a census drift test so PyTorch version changes cannot silently alter
  ingestion.

Exit gate: every node is classified before adding broad mappings.

### L2: Tokens, Embedding, And External Weights

- [ ] Emit `input_ids` through a typed model-input boundary.
- [ ] Publish embedding and projection weights through deterministic external
  tensor references and the existing safetensors side-file contract.
- [ ] Emit token embedding through the published transformer/common contract.
- [ ] Preserve integer index type and embedding result tensor descriptors.
- [ ] Reject unsupported index dtypes, out-of-contract ranks, and inconsistent
  vocabulary dimensions before finalization.

Exit gate: token IDs produce a native embedded activation without Python-owned
WHIRL structure.

### L3: RMSNorm And Rotary Position Encoding

- [ ] Map both RMSNorm instances in each decoder layer and the final RMSNorm.
- [ ] Preserve epsilon, axes, scale, accumulation type, and source-layer
  identity under the published contract.
- [ ] Map rotary position encoding without erasing paired-dimension and
  position semantics through premature view lowering.
- [ ] Verify rank, head dimension, position range, and descriptor propagation.

Exit gate: normalized Q/K inputs retain valid rotary semantics in native DSL
WHIRL.

### L4: Prefill Attention

- [ ] Emit Q, K, V, and output projections with ordered operand dependencies.
- [ ] Preserve batch, sequence, head, KV-head, and head-dimension meaning.
- [ ] Emit the pure `transformer.attention.v1` expression with a causal,
  full-sequence, no-cache contract.
- [ ] Map attention score contraction, mask application, softmax, value
  contraction, and output projection through the approved domain/common
  boundary.
- [ ] Reject KV-cache mutation, unsupported grouped-query attention, dynamic
  sequence shape, and unsupported masks in the first profile.

Exit gate: one complete native prefill-attention block passes the transformer
gatekeeper and is visible logically in `ir_b2a -st -src`.

### L5: SwiGLU, Residuals, And Decoder Layer

- [ ] Emit the gate and up projections, SiLU, elementwise multiply, and down
  projection while retaining the SwiGLU semantic contract.
- [ ] Preserve both residual additions and their exact-shape requirements.
- [ ] Create one decoder-layer region with declared inputs, results, source
  position, and a versioned transformer contract.
- [ ] Verify all values used outside the region are declared through its
  interface and use outer-owned no-alias result temporaries.

Exit gate: one decoder layer is a certified structured region rather than an
unstructured list of implementation-level operations.

### L6: Complete Tiny Llama 2 Artifact

- [ ] Emit embedding, both decoder layers, final RMSNorm, output projection,
  and output-logits boundary.
- [ ] Enclose the model in `transformer.prefill.v1`, preserve decoder-layer
  order, and retain nested decoder-layer regions.
- [ ] Run native verification before mapped-image finalization.
- [ ] Publish `llama2.B` and `llama2.safetensors` atomically.
- [ ] Reopen the artifact in a separate process.
- [ ] Preserve `llama2.T` from `ir_b2a -st -src llama2.B llama2.T` for human
  review.

Exit gate: the complete tiny prefill model is a stable, inspectable binary
WHIRL artifact after the Python producer exits.

### L7: Driver And O0 Certification

- [ ] Run the tiny fixture through `openpy -keep llama2_model.py`.
- [ ] Confirm the retained `.B` artifact is the same frontend boundary used by
  the standalone tool.
- [ ] Confirm `VHO_DSL_Lower_Driver()` consumes every executable transformer
  and common DSL value before canonical optimization.
- [ ] Preserve the logical post-lowering trace required by the driver contract.
- [ ] Prove no Python, PyTorch, frontend bridge, or backend CG dependency is
  required to consume the completed `.B` file.

Exit gate: static Llama 2 prefill completes the same `-O0` pipeline contract as
the stable ResNet path.

### L8: Optional Hugging Face Parity

- [ ] Add a separately gated test using a locally constructed tiny
  `LlamaConfig` with random weights and no network access.
- [ ] Pin the optional Transformers version in the Docker test environment.
- [ ] Compare eager results, captured semantic topology, descriptors, and
  external tensor mapping against the local fixture.
- [ ] Keep this lane optional; it must not become a production frontend
  dependency.

Exit gate: the local fixture and a widely used Llama frontend agree at the
published ingestion boundary.

### L9: Decode And KV Cache

- [ ] Start only after common abstract-state effects, region interfaces, and
  transformer cache contracts are approved.
- [ ] Represent cache storage as declared mutable state with read/modify edges,
  not tensor metadata.
- [ ] Add grouped-query attention, cache position, append/update behavior, and
  decode-shape verification incrementally.
- [ ] Keep prefill-only artifacts readable and behaviorally unchanged.

Exit gate: stateful decode is an additive, versioned extension to the certified
prefill profile.

## Main-Infrastructure Dependencies

| Frontend gate | Required infrastructure |
| --- | --- |
| L0-L1 | No new native operator implementation |
| L2 | Integer tensor inputs and reviewed embedding/gather contract |
| L3 | Native RMSNorm and rotary contracts, descriptor rules |
| L4 | Batched matmul, broadcast/mask, softmax, attention gatekeeper |
| L5 | Mul/SiLU plus structured region construction and verification |
| L6 | Complete mapped-image, printer, reader, and gatekeeper coverage |
| L7 | VHO DSL lowering and standard driver integration |
| L8 | No additional compiler contract unless parity finds a gap |
| L9 | Abstract-state effects and versioned KV-cache semantics |

Frontend work may proceed against capture/census tests while a required native
gate is under review.  Native certification must not claim completion until the
corresponding infrastructure contract and test are available.

## Artifact And Review Contract

The standard standalone review flow is:

```sh
torch2whirl llama2_model.py \
  --entry forward \
  --backend native \
  --sample-input int-shape:1,8 \
  --output llama2.B

ir_b2a -st -src llama2.B llama2.T
```

The final sample-input spelling is owned by the CLI implementation and must be
documented before L2 closes.  The output stem owns the artifact family:

```text
llama2.B
llama2.safetensors
llama2.T
```

Tests clean stale artifacts before a run, preserve the new artifacts in a host
mounted directory after success or failure, and report their paths for human
inspection.

## Real Checkpoint Policy

Licensed Llama 2 weights, tokenizers, and downloaded caches do not enter Git.
A recommended host location is:

```text
/Users/shinmingliu/work/open64/models/llama2/
```

Docker should mount that location read-only.  Real checkpoint testing begins
only after the deterministic tiny model passes the complete native artifact
and `-O0` gates.

## Change Discipline

1. Keep Python-facing values and regions opaque.
2. Do not inspect WN fields or decode the private physical DSL escape tag.
3. Do not add backend, CG, MLIR, FairScale, or mandatory Transformers
   dependencies.
4. Do not lower attention, RMSNorm, RoPE, or KV-cache semantics in Python.
5. Do not hide unsupported constructs behind generic operators or markers.
6. Do not add tokenizer or mutable-cache scope to the first prefill milestone.
7. Preserve source positions, external artifacts, and `ir_b2a -st -src` traces
   for review.
8. Preserve binary WHIRL compatibility and use only published native APIs.
9. Do not introduce tab characters.
