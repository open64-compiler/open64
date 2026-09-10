# FHE SYNC-3 ResNet Conversion Contract

Status: active FHE-owned SYNC-3 implementation contract. Main/common planning,
rewrite, checkpoint, PU-interface, and composite-profile substrates through
PR #123 are merged. BatchNorm folding has positive certification; full
SecureResNet publication remains blocked by `CFHECNN-RELU-002`. This document
does not allocate opcodes, edit shared common/com files, insert bootstrap
boundaries, lower to SIHE/CKKS primitives, or lower to OpenFHE/runtime calls.

Authority:

- `doc/DSC_FHE_Compiler_Architecture_and_Integration_Plan_v0.10.docx`
  is the highest semantic authority; the repository copy has SHA-256
  `0018769c26b5a0bcd1bdfcbd85aa97b8bafea381d7640fbb9e2e81b0022013d9`.
- `doc/FHE-CONSOLIDATED-IMPLEMENTATION-PLAN.md`
- `doc/FHE-WHIRL-INTEGRATION-PLAN.md`
- `doc/FHE-SYNC1-NATIVE-CONTRACT.md`
- `artifacts/fhe/resnet20_capture/` from the merged SYNC-2 frontend PR

This contract is a narrowed C3 / SYNC-3 review checkpoint permitted by v0.10
Appendix F.1. It cannot override v0.10 or establish completion of Architecture
Phase 3 or focused milestone M4. C4 / SYNC-4 bootstrap-plus-polynomial
materialization and the remaining v0.10 execution evidence stay mandatory.

## Prerequisite Gate

SYNC-3 source implementation remains blocked until all of the following are
accepted:

1. Corrective SYNC-1 validation establishes one version-1 tensor-binding
   identity and failure-atomic FHE entry-value insertion.
2. PU ownership and exact source provenance are corrected, and SYNC-2 is
   recertified with fail-closed dependencies, exact count/absence assertions,
   independent reopen, and retained host-visible artifacts.
3. Main/common and FHE reviewers select one exact node-retirement contract for
   physical BatchNorm removal, including users, provenance, rollback,
   old-reader behavior, mapped reopen, and tree/image consistency.

## Scope

After the prerequisite gate closes, focused SYNC-3 plans the conversion of the
recertified ResNet-20/CIFAR-10 source-level WHIRL artifact into FHE-visible CNN
semantics while retaining reviewable evidence. It owns FHE gatekeeper checks,
BatchNorm folding, operator disposition, approximation-contract requirements
for encrypted `common.relu`, value-specific CKKS state planning metadata, and
retained conversion artifacts.

SYNC-3 explicitly does not materialize bootstrap, polynomial activation,
SIHE/CKKS arithmetic, runtime calls, generated C, OpenFHE provider logic,
optimized boundary movement, ReSBM, HPOLY/HPAO, or GPU/POLY lowering.
Focused SYNC-3 acceptance closes only this planning checkpoint, not v0.10
Architecture Phase 3 or M4.

## Phase Boundary

Proposed phase:

```text
secure_resnet20.B
  -> ordinary DSL/common gatekeeper
  -> call-ABI and PU-interface identity validation
  -> generic XLA-style tensor shape propagation
  -> FHE semantic gatekeeper
  -> VHO_FHE_Convert_Driver()
  -> converted-shape verification
  -> value-specific CKKS-state propagation
  -> FHE semantic gatekeeper, converted form
  -> secure_resnet20.fhe.B
  -> ir_b2a -st -src secure_resnet20.fhe.B secure_resnet20.fhe.T
  -> secure_resnet20.fhe.conversion-report.txt
```

`VHO_FHE_Convert_Driver()` is a VHO-level semantic adaptation phase. At `-O0`
it performs deterministic legality and adaptation only: BatchNorm folding,
source operator disposition, encrypted value-class propagation, required
approximation-contract attachment, residual obligations, and conversion
reporting.

The exact cross-PU identity and analysis algorithms are specified in
`doc/FHE-SHAPE-AND-ENCRYPTION-STATE-PROPAGATION.md`. Preserving
callee-specific data-value metadata is the central requirement: each caller
actual and semantic role must resolve through the callee formal ordinal to the
exact callee `DSL_IR_VALUE_ID`, operator operand, and result. Tensor shape,
local symbol index, or source name is never an identity substitute.

Generic shape propagation certifies source geometry before FHE adaptation and
reruns after BatchNorm retirement. FHE encryption-state propagation follows
the converted-shape gate and attaches CKKS facts to values without changing
canonical TensorDescriptorIR/TY identity.

## Gatekeeper Inputs

The FHE gatekeeper consumes existing mapped WHIRL/DSL state plus the SYNC-1 FHE
image. It must not inspect physical `OPR_DSL` internals or raw mapped-image
offsets.

| Input | Required evidence | Failure family |
| --- | --- | --- |
| Entry contract | Exactly one FHE entry for `SecureResNet20`; valid config; input/output/parameter counts match declared rows | `CFHE-ENTRY-*` |
| Entry values | One ciphertext input, one ciphertext output, 107 encoded-plaintext parameters; every row owned by `owner_pu=SecureResNet20` | `CFHE-ENTRY-*` |
| Encryption descriptors | Ciphertext descriptor for input/output; encoded-plaintext descriptor for parameters; scheme is `ckks`; no secret material | `CFHE-DESC-*`, `CFHE-SECRET-*` |
| Tensor bindings | Canonical TY/TensorDescriptorIR identity is preserved; FHE descriptor binding is present for boundary and parameter types | `CFHE-DESC-*`, `CFHE-TENSOR-*` |
| Source graph | Valid class-centric PUs, explicit call edges and REGION contracts, plus source positions for entry/calls/regions/parameter symbols | `CFHE-SRC-*`, `CFHECNN-REGION-*` |
| Operator census | Every reachable encrypted operator has one accepted disposition; definition counts and source-context counts are reported separately | `CFHECNN-OP-*` |
| External payloads | `secure_resnet20.safetensors` exists; keys, byte ranges, dtype, shape, and parameter roles are stable | `CFHE-PAYLOAD-*` |
| Options | Inference-only CKKS path, plaintext/encoded-plaintext parameter policy, backend policy, and bootstrap policy are visible but not materialized | `CFHE-OPTION-*` |

The generic gatekeeper validates relationships and semantics, not fixture
cardinality. Exact ResNet-20 counts are certification-profile assertions: 6
`FUNC_ENTRY`s, 9 `VCALL`s, 5 `cnn.basic_block.v1` REGIONs, 13 physical
`cnn.conv2d` and 13 physical `cnn.batch_norm_infer` definitions representing
21 source contexts each, 5 physical residual-add definitions representing 9
source contexts, and 11 physical `common.relu` definitions representing 19
source contexts. These source contexts are context sensitivity, not operator
or function versions.

## Stable Diagnostics

Diagnostic strings are stable test contracts. Each diagnostic includes the
logical operator name, value name or entry ordinal, owning PU, source file and
line when available, and a short remediation. Diagnostics must not print
secret-key material, ciphertext bytes, backend C++ object state, or physical
`OPR_DSL` internals.

| Code | Condition |
| --- | --- |
| `CFHE-ENTRY-001` | Missing FHE entry contract for requested FHE conversion. |
| `CFHE-ENTRY-002` | More than one FHE entry contract is present for first-release ResNet conversion. |
| `CFHE-ENTRY-003` | Entry value ordinal is duplicated or outside declared range. |
| `CFHE-ENTRY-004` | Entry value owner PU does not match entry contract owner. |
| `CFHE-DESC-001` | Boundary value lacks encryption descriptor. |
| `CFHE-DESC-002` | Tensor binding lacks canonical TensorDescriptorIR/TY evidence. |
| `CFHE-DESC-003` | Unsupported scheme for first release; expected CKKS. |
| `CFHE-SECRET-001` | Secret-key material, decrypt operation, or server-side key generation requested. |
| `CFHE-SRC-001` | Source-derived parameter or converted operator lacks source position. |
| `CFHE-PAYLOAD-001` | External plaintext parameter side file is missing or unreadable. |
| `CFHE-PAYLOAD-002` | External plaintext parameter key, dtype, shape, or byte range is inconsistent. |
| `CFHECNN-BN-001` | BatchNorm is not inference-mode and cannot be folded. |
| `CFHECNN-BN-002` | BatchNorm channel count does not match preceding convolution output channels. |
| `CFHECNN-BN-003` | BatchNorm epsilon/variance would produce non-finite folded parameters. |
| `CFHECNN-BN-004` | Context-specific folding would require divergent signatures for one shared compiler clone. |
| `CFHECNN-CONV-001` | Unsupported convolution layout, rank, groups, dilation, stride, or padding policy. |
| `CFHECNN-POOL-001` | Max/data-dependent pooling is unsupported without approved replacement policy. |
| `CFHECNN-RELU-001` | Encrypted `common.relu` lacks required approximation contract for SYNC-3 output. |
| `CFHECNN-RELU-002` | The selected composite ReLU profile is not completely represented and certified; no full-model checkpoint is published. |
| `CFHECNN-RESIDUAL-001` | Residual add has incompatible shape, layout, value class, scale, or level obligation. |
| `CFHECKKS-STATE-001` | Value-specific CKKS state is missing where conversion requires level/scale tracking. |
| `CFHE-LOWER-001` | FHE/SIHE/CKKS/runtime lowering was requested during SYNC-3 conversion. |

## BatchNorm-to-Conv Folding

Current certification status: the ReLU-free shared-PU profile certifies 13
physical retirements, 21 context folds, 42 converted tensors, atomic auxiliary
publication, and mapped-image reopen. The complete SecureResNet profile reaches
the same derived counts before failing closed at the unapproved ReLU policy;
see `FHE-RELU-DEGREE3-POLICY-DECISION.md`.

Fold `cnn.batch_norm_infer` into the immediately preceding legal convolution
when all legality checks pass:

1. BatchNorm is inference-only; training-mode state and mutation are rejected.
2. The input is the preceding `cnn.conv2d` result in the same REGION or entry
   sequence, with no intervening non-foldable user.
3. The convolution output channel count equals the BatchNorm scale, bias,
   running mean, and running variance lengths.
4. The convolution weight is an encoded plaintext external parameter.
5. The convolution bias is encoded plaintext, implicit zero, or absent with
   legal implicit-zero materialization.
6. BatchNorm scale, bias, running mean, and running variance are encoded
   plaintext parameters or buffers with stable side-file references.
7. Epsilon is finite and positive, and `sqrt(variance + epsilon)` is finite
   for every channel.
8. First release layouts are NCHW activations and OIHW convolution weights.
9. Source and provenance metadata for convolution, BatchNorm, and all folded
   parameters is retained.

Fold formula:

```text
inv_std[c] = 1 / sqrt(running_var[c] + epsilon)
folded_weight[c, ...] = conv_weight[c, ...] * bn_scale[c] * inv_std[c]
folded_bias[c] =
    (conv_bias[c] if present else 0) * bn_scale[c] * inv_std[c]
    + bn_bias[c]
    - running_mean[c] * bn_scale[c] * inv_std[c]
```

Payload requirements:

- Emit folded plaintext payloads in `secure_resnet20.fhe.safetensors` or an
  equivalent converted side file; do not mutate `secure_resnet20.safetensors`.
- Preserve original keys and byte ranges in provenance fields:
  `source_conv_weight`, `source_conv_bias`, `source_bn_scale`,
  `source_bn_bias`, `source_bn_mean`, and `source_bn_var`.
- Use deterministic folded keys such as
  `folded/<instance_path>/conv1.weight` and
  `folded/<instance_path>/conv1.bias`.
- Record dtype, logical shape, byte range, checksum policy, layout, and source
  constructor location for folded payloads.
- List every fold as accepted or rejected in the conversion report.

The certified ResNet artifact reuses signature-specialized block PUs. A
physical convolution/BatchNorm definition may therefore serve several source
instance contexts with different parameter payloads. Folding must preserve
that sharing correctly:

1. Rewrite the shared clone PU body and formal signature once for each
   structurally compatible convolution/BatchNorm pair.
2. Compute folded weight and bias payloads separately for every source call
   context.
3. Rewrite each caller's actual-parameter list to replace the original
   convolution and BatchNorm parameter actuals with that context's folded
   weight and bias actuals.
4. Retain clone reuse only when the rewritten structural and tensor signature
   remains identical. Split a compiler clone when those signatures diverge.
5. Report physical definition rewrites separately from source-context payload
   folds so the 13-definition/21-context distinction remains visible.

### ResNet physical-definition/context map

The certification profile derives the 13/21 counts from source structure; the
generic conversion pass must not contain these constants.

| Owning PU or clone | Call contexts | Conv/BN definition pairs | Context folds |
| --- | --- | ---: | ---: |
| `SecureResNet20` entry stem | entry stem | 1 | 1 |
| `1x16x32x32_to_1x16x32x32_stride1x1_identity` | `layer1.0`, `layer1.1`, `layer1.2` | 2 | 6 |
| `1x16x32x32_to_1x32x16x16_stride2x2_projection` | `layer2.0` | 3, including projection | 3 |
| `1x32x16x16_to_1x32x16x16_stride1x1_identity` | `layer2.1`, `layer2.2` | 2 | 4 |
| `1x32x16x16_to_1x64x8x8_stride2x2_projection` | `layer3.0` | 3, including projection | 3 |
| `1x64x8x8_to_1x64x8x8_stride1x1_identity` | `layer3.1`, `layer3.2` | 2 | 4 |
| **Total** | **entry plus 9 calls** | **13** | **21** |

Each physical pair is rewritten once. Each table cell in the context-fold
column represents a separately computed folded weight/bias pair, deterministic
converted side-file keys, one caller-owned pair of converted parameter values,
and one provenance association to the source callsite or entry context.

### Clone reuse and splitting rule

The post-fold clone key is semantic and deterministic. It contains:

- canonical Python definition and REGION contract identity/version;
- ordered surviving formal roles and exact formal `TY_IDX` values;
- exact return `TY_IDX` and result convention;
- convolution count, order, and structural attributes, including kernel,
  stride, padding, dilation, groups, activation/weight layout, and projection
  topology; and
- representation-level encryption descriptor requirements that affect the
  shared clone body.

Reuse one clone when this key is equal, even when folded bytes, side-file keys,
checksums, instance paths, call ordinals, source positions, or value-specific
CKKS state differ. Those are context/value facts and are excluded from clone
identity. An absent source convolution bias does not itself require a split
when conversion materializes the same typed folded-bias formal used by biased
contexts.

Split a clone only when the post-fold key differs: exact formal or return
`TY_IDX` mismatch, different surviving formal count/order, incompatible bias
materialization, different convolution/projection topology or attributes, or
a representation contract that changes the shared body. A malformed or
numerically illegal payload is not a reason to split: it rejects the affected
conversion and rolls back the complete artifact transaction.

### Native consumer transaction

The FHE pass consumes structured APIs only. For every external source value it
needs storage format, side-file path, tensor key, byte range, checksum, dtype,
logical shape, and layout without parsing marker text. Strings returned by a
lookup remain valid through the conversion transaction or until managed-image
reset, whichever occurs first.

One atomic rewrite transaction covers a physical clone and all of its source
call contexts. Before mutation it validates owners, callsite identities,
operand roles, exact formal/actual `TY_IDX` equality, source Conv/BN adjacency,
and every converted payload descriptor. On commit it must:

1. create caller-owned converted folded-weight and folded-bias values for each
   context, preserving source and instance metadata;
2. rewrite the clone formal list once and each caller actual list separately;
3. rewrite the Conv/BN definition pair so no executable standalone BatchNorm
   remains, while retaining both source node/value identities in disposition
   and fold-provenance evidence;
4. update physical WN, logical DSL records, symbols, call metadata, and plan
   associations consistently; and
5. leave canonical tensor types unchanged and keep value-specific CKKS state
   outside `TY_IDX` identity.

Any failed owner, callsite, type, operand, payload, or image validation rolls
back WN, managed rows, symbol/formal/actual changes, and converted-value
creation. The converted side file is written to a temporary path and is
published only after the IR transaction and final gatekeeper succeed. The
source `.safetensors` file is never modified.

### Focused shared-clone tests

- One two-caller clone with equal post-fold keys and different source payload
  bytes remains one PU, receives two caller-owned folded pairs, and records two
  context folds for one physical definition rewrite.
- Equal types with different instance paths, keys, checksums, call ordinals,
  source positions, or CKKS state do not split the clone.
- An absent source bias normalized to the common typed folded-bias formal does
  not split the clone.
- Different exact formal `TY_IDX`, formal count/order, projection topology, or
  convolution attributes produce deterministic context-specialized clones;
  equivalent contexts reuse the same resulting clone.
- One bad caller owner, callsite, actual type, side-file range, checksum, or
  payload shape rejects the whole transaction and leaves the original tree,
  image tables, call graph, side file, and clone set byte-for-byte unchanged.
- A second clean conversion produces identical clone names, folded keys,
  payload bytes, provenance rows, report ordering, and hashes.
- Independent graph traversal proves 13 physical definition rewrites map to
  21 context folds for ResNet-20, with no standalone executable BatchNorm and
  no fixture cardinality embedded in generic conversion code.

## Operator Disposition

| Source construct | SYNC-3 disposition |
| --- | --- |
| `cnn.conv2d` | Convert to a reviewed FHE-CNN convolution wrapper after legal BatchNorm folding; operands are encrypted activation plus encoded plaintext folded weight/bias. |
| `cnn.batch_norm_infer` | Fold into preceding convolution when legal; no standalone BatchNorm should survive in accepted `secure_resnet20.fhe.B`. |
| `common.residual_add` | Convert to FHE residual wrapper or attach residual alignment obligation; preserve residual lineage, source position, and context identity. |
| `cnn.global_avg_pool2d` | Convert to encrypted sum plus encoded plaintext scale plan; no max/min data-dependent decision. |
| `common.flatten` | Preserve as layout reinterpretation when packing legality proves no encrypted data movement; otherwise attach explicit layout obligation. |
| `common.linear` | Convert to encrypted matrix-vector/classifier wrapper with encoded plaintext weight/bias. |
| `common.output_logits` | Preserve encrypted output boundary and client export contract; output remains ciphertext. |
| `common.relu` | Preserve source identity and attach required approximation-contract reference. Do not replace it with bootstrap in SYNC-3. |
| `cnn.max_pool2d` | Reject in first ResNet FHE path unless an explicit reviewed replacement policy is supplied; the SYNC-2 ResNet-20 fixture does not require it. |
| Unsupported activation/control/mutation | Reject before conversion output is written. |

FHE-domain wrappers use the existing domain-wrapper registry when their
operator shape, effect, and lowering contract delegates to an existing
common/CNN semantic target. The wrapper's logical `fhe.cnn.*` name remains
first-class in diagnostics and ASCII output while its common target remains an
implementation detail available through `DSL_Opcode_Wrapper_Target()`. This
does not require a new `DSL_OPERATOR` enum value because SYNC-3 stores the
wrapper identity in the planning image while retaining the source-semantic WN.
The current registry alone does not create a native result-producing WN for an
operator without a `DSL_OPERATOR` value. A genuinely new executable semantic
operation still requires reviewed opcode allocation; SYNC-3 performs no such
allocation. The exact physical contract is
`doc/FHE-SYNC3-NATIVE-PLAN-CONTRACT.md`.

## ReLU Approximation Contract Fields

Every surviving encrypted `common.relu` needs an attached composite profile
and context range binding before SYNC-4 can insert bootstrap and polynomial
evaluation. The selected ResNet candidate is
`ace.chebyshev.sign.7x15x13.depth11.v1`.

| Field | Requirement |
| --- | --- |
| `contract_id` | Stable interned ID; invalid zero. |
| `source_operator` | `common.relu`, including logical opcode version and source value ID. |
| Source evidence | Use the existing source WN/ST/DST/value records; do not duplicate source-position fields in the approximation row. |
| `function` | `relu`. |
| `approximation_family` | Approved polynomial family, such as minimax, Chebyshev, or Taylor-by-policy. |
| `profile_id` | Stable name/version for the complete composition and reconstruction rule. |
| `ordered_stages` | Three Chebyshev stages with ordinals 0, 1, 2 and degrees 7, 15, 13. |
| `coefficient_payload` | One external or interned plaintext coefficient vector reference per stage, with exact binary64 checksum. |
| `coefficient_dtype` | Planning evidence dtype, normally `float64`, with backend lowering policy deferred. |
| `context_bound` | Positive `B` bound joined to exact ReLU value/context identity; normalization is `x/B`. |
| `valid_input_range` | Closed normalized range, initially `[-1,1]`; out-of-range behavior is reject. |
| `max_abs_error` | Certified stage, composed-sign, reconstructed-ReLU, and model-level error evidence. |
| `scale_policy` | Desired input/output scale relation for CKKS planning. |
| `level_policy` | Minimum level/depth budget consumed by polynomial evaluation. |
| `bootstrap_policy` | `auto`, `on`, `manual`, or `off` behavior inherited from entry config. |
| `requires_pre_refresh` | True for the first CKKS `-O0` path unless a manual boundary is already proven. |
| `provenance` | Source ReLU value, owning PU, context path, and conversion pass ID. |

SYNC-3 may attach or require the profile obligation. SYNC-4 materializes
bootstrap, normalization, ordered stage evaluation, and ReLU reconstruction.
Bootstrap restores capacity and does not compute ReLU. PR #123 added the
optional `.WHIRL.dsl_fhe_approx_profile` image without changing the existing
v1 approximation row. The accepted profile/stage/association/context contract
is `doc/FHE-SYNC-C-COMPOSITE-APPROXIMATION-CONTRACT.md`. Consumer enablement
still requires certified coefficient bytes, all 19 identity-bound context
ranges, model-level numerical evidence, and concrete CKKS state/depth proof.

## Value-Specific CKKS State

CKKS state must not mutate canonical TY/TensorDescriptorIR identity. It is a
value-specific attachment keyed by value ID plus state version.

| State | Purpose |
| --- | --- |
| `scheme` | CKKS only for first release. |
| `value_class` | Ciphertext, encoded plaintext, or clear metadata. |
| `level` | Current modulus-chain level or symbolic unknown before planning. |
| `scale_bits` | Current/logical scale. |
| `component_count` | Ciphertext component count; multiplication may widen before relinearization. |
| `precision_bits` | Estimated remaining precision. |
| `slot_count` | Resolved or policy-inherited slot count. |
| `layout_id` | Encrypted layout/packing identity, distinct from canonical tensor shape. |
| `alignment_group` | Residual-add compatibility group. |
| `pending_rescale` | Whether rescale is required before the next consumer. |
| `pending_relinearize` | Whether relinearization is required. |
| `pending_bootstrap_reason` | Reason such as `pre_relu_refresh`, not materialized until SYNC-4. |
| `key_requirements` | Rotation, relinearization, and bootstrap key references by key-set ID. |

## Retained Artifacts

SYNC-3 must retain:

```text
artifacts/fhe/resnet20_convert/
  secure_resnet20.py
  secure_resnet20.B
  secure_resnet20.T
  secure_resnet20.safetensors
  secure_resnet20.fhe.B
  secure_resnet20.fhe.T
  secure_resnet20.fhe.safetensors
  secure_resnet20.fhe.conversion-report.txt
  gatekeeper.log
  negative-diagnostics/
```

The conversion report must include:

- input artifact checksums and source commit;
- FHE entry/config summary;
- operator census before and after conversion;
- BatchNorm fold table;
- external payload rewrite table;
- ReLU approximation-contract table;
- residual alignment obligation table;
- value-specific CKKS state summary;
- accepted/rejected operation table; and
- diagnostics summary.

## Main/Common Hooks Needed

The FHE task needs these main/common contracts before or during SYNC-3
implementation:

1. A phase hook to invoke `VHO_FHE_Convert_Driver()` after optional DSL
   WOPT/Preopt and before `VHO_DSL_Lower_Driver()`.
2. A stable option surface for enabling FHE conversion and selecting strict
   `-O0` behavior while ignoring unrelated driver options.
3. Opaque read APIs for logical DSL opcode, version, operands, attributes,
   result symbol/TY, source position, and owning PU.
4. Opaque read APIs for FHE entry contracts, entry values, encryption
   descriptors, tensor bindings, and key requirements from the mapped image.
5. Reviewed write/update APIs for converted FHE-CNN domain wrappers. Reuse the
   domain-wrapper registry when the wrapper delegates to an existing semantic
   target; allocate no new enum solely to retain FHE domain identity.
6. A value-specific CKKS/FHE state attachment mechanism that does not mutate
   canonical TY/TensorDescriptorIR.
7. Approximation-contract interning/attachment hooks, or a reviewed temporary
   metadata carrier with stable `ir_b2a -st -src` spelling.
8. External tensor payload writer support for folded side-file payloads and
   deterministic payload references in `.T`.
9. A diagnostic registry or pass-through convention for `CFHE-*`,
   `CFHECNN-*`, `CFHELAYOUT-*`, `CFHECKKS-*`, and `CFHERT-*`.
10. Printer support for converted FHE-CNN disposition, folded-parameter
    provenance, approximation contracts, and value-specific CKKS state.

## Mapped-Image Staging Decision

Do not enlarge or reinterpret the exact-sized version-1 `.WHIRL.dsl_fhe`
image. SYNC-3 should add a separate optional fixed-row planning section,
`.WHIRL.dsl_fhe_plan` / `WT_DSL_FHE_PLAN`, for conversion
dispositions, approximation contracts, value-specific CKKS state, and fold
provenance. The section is omitted when empty, uses 8-byte section and row
alignment, contains no pointers or STL objects, and is copied through the
existing mapped-image service before the ELF mapping is released.

This additive section lets existing FHE-v1-aware readers continue to consume
the stable `.WHIRL.dsl_fhe` rows and ignore an unknown optional section.
Changing `.WHIRL.dsl_fhe` to version 2 would instead make those readers reject
the entire FHE image. Exact planning-row fields, sizes, capabilities, range
rules, reset behavior, printer headings, and malformed-image tests must be
published in a separate main/common implementation contract before source
coding begins.

The planning image records references to existing DSL value, TY, PU, payload,
and source evidence. It does not duplicate TensorDescriptorIR identity,
source positions, tensor payload bytes, or compiler metadata. Its records
distinguish a physical operator-definition rewrite from each source-context
payload fold.

## FHE-Owned Implementation

After the hooks above are reviewed, FHE owns:

1. FHE gatekeeper semantic checks and diagnostics.
2. ResNet-20 conversion driver logic.
3. BatchNorm fold legality and folded plaintext payload generation.
4. FHE-CNN disposition of convolution, residual add, global average pool,
   flatten, linear, output logits, and ReLU approximation-contract attachment.
5. Value-specific CKKS state propagation semantics needed for conversion.
6. Conversion report generation and retained artifact layout.
7. Negative tests and focused add, linear, convolution, residual-add, and ReLU
   diagnostic fixtures.

## Negative Test Matrix

| Test | Expected diagnostic |
| --- | --- |
| No FHE entry contract | `CFHE-ENTRY-001` |
| Two FHE entry contracts in first-release artifact | `CFHE-ENTRY-002` |
| Duplicate input/output/parameter ordinal | `CFHE-ENTRY-003` |
| Entry value from a non-entry PU | `CFHE-ENTRY-004` |
| Ciphertext boundary without encryption descriptor | `CFHE-DESC-001` |
| Parameter tensor without TensorDescriptorIR binding | `CFHE-DESC-002` |
| Non-CKKS scheme selected | `CFHE-DESC-003` |
| Secret-key/decrypt/server key-generation marker | `CFHE-SECRET-001` |
| Source-derived parameter with null/line-zero source | `CFHE-SRC-001` |
| Missing side-file or missing tensor key | `CFHE-PAYLOAD-001` |
| Payload dtype/shape/range mismatch | `CFHE-PAYLOAD-002` |
| Training-mode BatchNorm | `CFHECNN-BN-001` |
| BatchNorm channel mismatch | `CFHECNN-BN-002` |
| Non-finite folded BatchNorm parameter | `CFHECNN-BN-003` |
| Shared clone requires divergent rewritten signatures | `CFHECNN-BN-004` |
| Unsupported grouped or dilated convolution policy | `CFHECNN-CONV-001` |
| Unsupported max pooling | `CFHECNN-POOL-001` |
| Encrypted ReLU without approximation contract | `CFHECNN-RELU-001` |
| Residual shape/layout/value-class mismatch | `CFHECNN-RESIDUAL-001` |
| CKKS state missing where required | `CFHECKKS-STATE-001` |
| Runtime/OpenFHE lowering attempted during SYNC-3 | `CFHE-LOWER-001` |
