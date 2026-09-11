# FHE Cross-PU Shape and Encryption-State Propagation

Status: architectural and `-O0` execution contract

This document records the interprocedural metadata issue exposed by the
ResNet-20 SYNC-3 BatchNorm-fold work and specifies its resolution. It also
defines the intended relationship between XLA-style tensor shape propagation
and the later FHE-specific CKKS encryption-state propagation.

## Central Requirement

The center issue is preservation of **callee-specific data-value metadata**.
For every call argument, the compiler must retain a structural path from the
caller value to the exact value consumed inside the callee:

```text
source call context
  -> caller actual DSL_IR_VALUE_ID
  -> callsite and actual ordinal
  -> semantic argument role
  -> callee PU and formal ordinal
  -> callee formal DSL_IR_VALUE_ID
  -> callee operator operand
  -> callee result DSL_IR_VALUE_ID
```

Shape equality is necessary but is not sufficient. Two values may have the
same canonical tensor type and shape while representing different weights,
biases, BatchNorm statistics, call contexts, packing states, CKKS levels, or
provenance. Therefore:

- canonical `TY_IDX` and TensorDescriptorIR describe reusable tensor type
  identity;
- `DSL_IR_VALUE_ID` identifies a semantic value and owns value-specific facts;
- call ABI and PU-interface records bind caller values to callee values;
- source/context metadata remains associated with the value and callsite;
- CKKS runtime/planning state never becomes part of canonical tensor type
  identity.

No analysis may reconstruct this relationship from symbol names, local
`ST_IDX` values, argument order conventions, or tensor shape alone.

## Observed ResNet-20 Failure Mode

The captured model contains five context-specialized `ResNet20Block` PUs and
nine block callsites. The five structural signatures are reused as follows:

| Block context | Calls | Arguments per call | Call-argument rows |
| --- | ---: | ---: | ---: |
| 16-channel identity | 3 | 13 | 39 |
| 16-to-32 projection | 1 | 19 | 19 |
| 32-channel identity | 2 | 13 | 26 |
| 32-to-64 projection | 1 | 19 | 19 |
| 64-channel identity | 2 | 13 | 26 |
| **Total** | **9** | | **129** |

The 129 rows describe input bindings, not 129 independent call results. The
nine calls each have one result value. A shared callee body may therefore have
one formal value for a semantic role while several callers supply distinct
external payload values for that role.

The original call evidence identified the caller actual, callee formal
ordinal, and semantic role, but it could not safely recover the callee formal
`DSL_IR_VALUE_ID`. PU-local symbol indexes collide across local symbol tables,
and a backend pass cannot inspect a foreign PU's local symbols while another
PU is active. This made the following approaches invalid:

- matching names such as `running_var` or `conv_weight`;
- assuming a fixed ordinal is a particular CNN role;
- using shape as value identity;
- opening or dereferencing a foreign local symbol table;
- copying metadata from an arbitrary call context into a shared callee value.

## Structural Resolution

The required common substrate is an optional PU-interface identity table with
the mapping:

```text
(owner_pu_st, physical_formal_ordinal)
  -> (formal_value_id, formal_st, exact_formal_ty)
```

The table covers the complete physical interface, including input formals and
hidden result formals. Together with the call-ABI image, it supports this
owner-safe join:

```text
Call ABI row:
  (callsite_id, actual_ordinal)
  -> caller argument_value_id, callee_formal_ordinal, semantic_role

PU interface row:
  (callee_pu_st, callee_formal_ordinal)
  -> callee formal_value_id, formal_st, formal_ty
```

The joined callee value can then be followed through existing DSL value
references to the exact Conv or BatchNorm operand and its defining metadata.

Validation must prove:

1. The owner is an existing global function symbol with a valid PU.
2. The formal rows cover the complete physical FUNC_ENTRY interface in order.
3. Every formal value, symbol, and type belongs to the declared owner.
4. The physical formal symbol and recorded `TY_IDX` agree exactly.
5. A call-ABI actual and the corresponding formal have ABI-compatible types.
6. The same formal has one stable semantic role across all shared callsites.
7. Global queries are owner-safe; physical formal checks require the matching
   PU and local symbol table to be active.

Old artifacts may omit the optional table. They remain readable, but an FHE
transformation that requires callee-value identity must fail closed when the
mapping is absent.

### Infrastructure checkpoint

- PR #120 merged the generic call-ABI role image and transactional BatchNorm
  rewrite prerequisites.
- PR #121 merged at `e3494772` and contains commit `7d62194e`, which publishes
  the complete PU-interface formal-value identity contract described above.
- The FHE conversion branch must rebase onto that merged authority, recapture
  the model with complete input and hidden-result formal rows, and consume only
  the published query APIs.
- Shape and CKKS-state analysis implementation must not work around a missing
  PU-interface image by parsing names or opening foreign local symbol tables.

## XLA-Style Tensor Shape Propagation

### Scope

Shape propagation is a generic DSL/common analysis. Its first job is static
verification and inference of tensor geometry across operators, REGION
interfaces, and PU calls. It is not an FHE pass and it does not infer CKKS
level, scale, ciphertext size, bootstrap placement, or key requirements.

The analysis uses Open64 DSL identity tables for semantic binding. It may use
XLA-style operator shape functions and fixed-point constraint propagation, but
it must not import a second graph identity model into WHIRL.

### Shape fact

Each contextual value instance has a shape fact. Its analysis key is:

```text
ShapeKey = (context_identity, owner_pu_st, DSL_IR_VALUE_ID)
```

The root PU uses its entry context. A call edge derives a deterministic child
context from the caller context and callsite identity. The fact is:

```text
ShapeFact = {
  element_type,
  rank,
  dimensions[],
  dynamic_dimension_mask,
  layout,
  strides,
  traits,
  source_of_fact,
  confidence
}
```

`source_of_fact` is provenance, not part of tensor type equivalence. The shape
lattice is:

```text
uninitialized < partial < concrete
                         \-> conflict
```

Joining compatible partial facts refines knowledge. Joining incompatible
concrete facts produces a diagnostic conflict; it never silently picks one.

### Constraint construction

The analysis builds constraints from:

1. Canonical TensorDescriptorIR/TY facts for every value.
2. Logical operator shape functions and versioned attributes.
3. Call-ABI plus PU-interface bindings between caller actuals and callee
   formals.
4. Callee hidden-result formals and caller call-result values.
5. REGION input/output/result contracts.
6. Control-flow joins where admitted by the current WHIRL level.

For each call, actual-to-formal propagation is context-sensitive at the edge.
The analysis retains one fact per contextual callee formal. It may derive one
definition-level summary only when all incoming facts are compatible. A
conflict means the existing clone key is too broad or the call ABI is invalid;
the analysis must request a deterministic structural/type-signature clone or
reject the program. It must not overwrite the formal with the last caller's
metadata.

### Deterministic algorithm

```text
seed facts from canonical tensor descriptors and declared interfaces
enqueue all logical nodes, REGION boundaries, and call edges in stable ID order

while the worklist is not empty:
  remove the lowest stable work item
  read operand facts by DSL_IR_VALUE_ID
  apply the registered operator or boundary transfer function
  join inferred facts into result/formal/actual values
  if a fact becomes more precise:
    enqueue its users in stable ID order
  if a join conflicts:
    emit a source-linked diagnostic and fail closed

verify every required value is concrete
verify physical TY and logical shape evidence agree
persist only reviewed facts required by the mapped-image contract
```

The lattice is finite and facts only become more precise, so the worklist
terminates. The first ResNet-20 `-O0` profile has no dynamic-shape loop
requirement; unsupported recursive or widening-dependent cases fail closed.

### ResNet transfer functions

| Operator | Required shape rule |
| --- | --- |
| `cnn.conv2d` | Validate NCHW input, OIHW weight, channel/groups agreement, stride, dilation, and padding; compute output spatial dimensions. |
| `cnn.batch_norm_infer` | Preserve activation shape; require scale, bias, mean, and variance vectors to match output channels. |
| `common.relu` | Preserve shape and layout exactly. |
| `common.residual_add` | Require equal broadcast-free shapes for the first profile and preserve the common shape. |
| `cnn.max_pool2d` | Apply kernel/stride/padding output-size rule. |
| `cnn.global_avg_pool2d` | Reduce spatial dimensions according to the versioned result contract. |
| `common.flatten` | Preserve batch and multiply the flattened dimensions with overflow checks. |
| `common.linear` | Require the input feature dimension to match the weight contract and infer output features. |
| `common.output_logits` | Preserve classifier-result shape and validate the declared class dimension. |

### Placement

The `-O0` pipeline runs shape propagation twice where necessary:

1. **Source-shape gate:** after mapped-image and ordinary DSL validation, before
   FHE semantic conversion. This certifies call, REGION, Conv, BN, residual,
   pooling, and classifier geometry.
2. **Converted-shape gate:** after BatchNorm retirement and model adaptation,
   before CKKS-state propagation. This proves rewrites preserved result shape
   and that no live operator depends on retired BN-only values.

## FHE Encryption-State Propagation

### Scope and identity

Encryption-state propagation is FHE-owned and runs only after generic shape
facts and FHE entry/encryption contracts are valid. Its internal analysis key
is `(context_identity, owner_pu_st, DSL_IR_VALUE_ID, state_version)`. This is
essential because two calls of one shared PU can reach the same physical
formal at different CKKS levels or scales. It must not mutate canonical
`TY_IDX` or TensorDescriptorIR identity.

```text
CKKSState = {
  value_class,
  encryption_descriptor_id,
  encrypted_layout_id,
  slot_count,
  logical_to_slot_mapping,
  level,
  log2_scale,
  component_count,
  precision_estimate,
  alignment_group,
  required_rotation_set,
  pending_rescale,
  pending_relinearization,
  pending_bootstrap_reason,
  state_version,
  predecessor_state,
  provenance
}
```

Unknown numeric state uses the reviewed signed pending sentinel. Unknown means
not yet inferred; it is not permission to lower. A conflict is a gatekeeper
failure and is never persisted as an accepted state.

The current `.WHIRL.dsl_fhe_plan` v1 CKKS-state row is identified only by
`(value_id, state_version)`. A contextual analysis may collapse into that row
only when every represented call context has an identical accepted state. If
context states differ, the append-only `.WHIRL.dsl_fhe_context_state` image
records the complete state under the exact source-definition/call-context key.
It must never persist whichever callsite happened to be analyzed last or use
`state_version` as a callsite or approximation-stage ordinal. The normative
physical and validation contract is
`doc/FHE-SYNC3-CONTEXT-CKKS-STATE-CONTRACT.md`.

For the ACE composite ReLU path, `POST_REFRESH.v1` describes the target of the
planned mandatory pre-operation refresh. It is not evidence that bootstrap has
executed. Common infrastructure verifies one-to-one context-range association
and profile/config agreement; the FHE semantic gate verifies the approved ACE
levels, scale, component count, precision requirement, and pending-refresh
policy.

### Initialization

- Entry ciphertext input state comes from the FHE entry contract,
  EncryptionDescriptorIR, configuration, and packing policy.
- External weights and biases are encoded-plaintext states linked to immutable
  source payloads or converted folded payloads.
- Caller actual to callee formal state transfer uses the same call-ABI and
  PU-interface identity join as shape propagation.
- Retired BatchNorm values and verified dead BN ABI inputs are excluded from
  live FHE accounting, while their provenance remains inspectable.
- Every live value must already have a concrete compatible shape fact.

### Transfer rules

| Operation | CKKS-state transfer obligation |
| --- | --- |
| Add/residual add | Require compatible encrypted layout, level, and scale; create an alignment obligation when legal adjustment is required. |
| Ciphertext-plaintext multiply | Decrease available level according to the approved rescale policy; update scale and pending rescale. |
| Conv2d | Select the reviewed packing/metakernel plan from shape and layout; accumulate rotation keys, multiply/rescale cost, output layout, level, scale, and precision. |
| Pooling | Record rotation/sum and plaintext-scale obligations; reject unsupported max-pool semantics unless an approved replacement exists. |
| Flatten/reshape | Preserve cryptographic capacity when it is a legal layout reinterpretation; otherwise record an explicit layout conversion. |
| Linear | Select the reviewed encrypted matrix-vector plan and propagate rotations, level, scale, layout, and precision. |
| `common.relu` | Preserve source identity, attach the approved approximation contract, and record mandatory `relu_boundary` bootstrap planning before polynomial evaluation. |
| Output logits | Validate exportable ciphertext layout and bind the encrypted entry result. |

### Deterministic algorithm

```text
require successful source-shape and converted-shape gates
seed entry and encoded-plaintext states
enqueue executable nodes and call edges in stable ID order

while the worklist is not empty:
  remove the lowest stable work item
  resolve operands by exact DSL_IR_VALUE_ID
  resolve caller/callee state through call ABI and PU interface identity
  apply the versioned FHE transfer rule
  join the result with any existing value state
  record alignment, rotation, rescale, relinearization, and refresh obligations
  enqueue users when state becomes more precise
  fail closed on incompatible class, layout, level, scale, or descriptor facts

verify all live encrypted values have accepted states
verify all pending actions have a later owning lowering stage
persist accepted state versions and conversion provenance
```

The `-O0` rule is deterministic. It does not move, merge, deduplicate, or fuse
refresh boundaries. Those transformations require separate `-O1+` legality
and numerical-equivalence proofs.

### ReLU policy boundary

SYNC-3 may persist a structurally valid approximation obligation and CKKS
refresh reason, but it must not invent polynomial coefficients. The selected
ResNet architecture candidate is the ACE-compatible composite Chebyshev sign
profile with ordered degrees `7 -> 15 -> 13` and claimed depth 11. SYNC-4
requires certified stage coefficients, exact context-bound identity, clear and
model error, evaluation scheme, and CKKS depth/state authority. Only then may
the compiler materialize bootstrap, normalization, the ordered stages, and
ReLU reconstruction. A single cubic remains experimental and cannot substitute
for that evidence.

## Integrated `-O0` Execution Order

```text
binary very-high-level WHIRL
  -> mapped-image structural validation
  -> ordinary DSL/common and tensor gatekeeper
  -> call-ABI and PU-interface identity validation
  -> generic XLA-style shape propagation
  -> FHE entry/encryption semantic gatekeeper
  -> BatchNorm payload fold and value retirement
  -> converted-shape verification
  -> FHE CKKS-state propagation and conversion planning
  -> SYNC-3 converted .fhe.B, side payload, report, and .fhe.T
  -> SYNC-4 bootstrap plus approved ReLU polynomial materialization
  -> SIHE/CKKS lowering
  -> standard middle-WHIRL runtime calls
  -> whirl2c
  -> OpenFHE-linked executable
```

Shape propagation is introduced before CKKS planning because packing and
metakernel selection require certified dimensions. FHE state propagation runs
afterward because its facts are representation- and value-specific.

## Verification Requirements

| Area | Required tests |
| --- | --- |
| Identity | Local `ST_IDX` collisions, shared callee reuse, hidden result formal, wrong owner, absent interface image, and mismatched formal TY. |
| Shape | Conv/BN/pool/flatten/linear positives; rank, channel, dimension, attribute, REGION, call-result, and cross-context conflicts. |
| Context sensitivity | Equivalent contexts reuse one clone; incompatible shape signatures split deterministically or reject; value metadata is never last-writer-wins. |
| BN folding | 13 physical Conv/BN definitions and 21 call contexts; distinct folded payload values per context; source payload remains immutable. |
| Retirement | No live BN computation or BN-only formal use; retired rows remain traceable and are excluded from executable accounting. |
| CKKS state | Class, descriptor, layout, level, scale, alignment, rotations, pending actions, state versions, and source-to-result provenance. |
| Compatibility | Old optional-section absence, mapped reopen, malformed rows, independent `ir_b2a -st -src`, and no binary WHIRL revision change. |
| Publication | Converted side payload, report, and `.fhe.B` publish atomically; induced failure leaves no valid partial artifact. |

## Ownership and Staging

- Main/common owns generic value identity, call ABI, PU-interface records,
  tensor shape propagation services, mapped-image compatibility, and generic
  validation/printing hooks.
- The FHE task owns encryption-state semantics, FHE transfer rules, BatchNorm
  fold consumption, packing/metakernel planning, approximation obligations,
  reports, diagnostics, and FHE-specific tests.
- Shared opcode/type allocation, canonical type encoding, binary WHIRL layout,
  and common/com implementation remain main-owned and require their normal
  reviewed contract checkpoints.
- Neither analysis may use raw physical `OPR_DSL`, foreign local symbol-table
  access, or frontend reconstruction of native identities.
