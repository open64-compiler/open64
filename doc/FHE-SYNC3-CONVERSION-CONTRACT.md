# FHE SYNC-3 ResNet Conversion Contract

Status: main/common staging contract for review after SYNC-2 merge. This
document does not allocate opcodes, edit shared common/com files, insert bootstrap
boundaries, lower to SIHE/CKKS primitives, or lower to OpenFHE/runtime calls.

Authority:

- `doc/FHE-CONSOLIDATED-IMPLEMENTATION-PLAN.md`
- `doc/FHE-WHIRL-INTEGRATION-PLAN.md`
- `doc/FHE-SYNC1-NATIVE-CONTRACT.md`
- `artifacts/fhe/resnet20_capture/` from the merged SYNC-2 frontend PR

## Scope

SYNC-3 converts the accepted ResNet-20/CIFAR-10 source-level WHIRL artifact
into FHE-visible CNN semantics while retaining reviewable evidence. It owns
FHE gatekeeper checks, BatchNorm folding, operator disposition, approximation
contract requirements for encrypted `common.relu`, value-specific CKKS state
planning metadata, and retained conversion artifacts.

SYNC-3 explicitly does not materialize bootstrap, polynomial activation,
SIHE/CKKS arithmetic, runtime calls, generated C, OpenFHE provider logic,
optimized boundary movement, ReSBM, HPOLY/HPAO, or GPU/POLY lowering.

## Phase Boundary

Proposed phase:

```text
secure_resnet20.B
  -> ordinary DSL/common gatekeeper
  -> FHE semantic gatekeeper
  -> VHO_FHE_Convert_Driver()
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
| `CFHECNN-CONV-001` | Unsupported convolution layout, rank, groups, dilation, stride, or padding policy. |
| `CFHECNN-POOL-001` | Max/data-dependent pooling is unsupported without approved replacement policy. |
| `CFHECNN-RELU-001` | Encrypted `common.relu` lacks required approximation contract for SYNC-3 output. |
| `CFHECNN-RESIDUAL-001` | Residual add has incompatible shape, layout, value class, scale, or level obligation. |
| `CFHECKKS-STATE-001` | Value-specific CKKS state is missing where conversion requires level/scale tracking. |
| `CFHE-LOWER-001` | FHE/SIHE/CKKS/runtime lowering was requested during SYNC-3 conversion. |

## BatchNorm-to-Conv Folding

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
does not require a new `DSL_OPERATOR` enum value. A genuinely new semantic
operation still requires reviewed opcode allocation; SYNC-3 performs no such
allocation.

## ReLU Approximation Contract Fields

Every surviving encrypted `common.relu` needs an attached approximation
contract before SYNC-4 can insert bootstrap and polynomial evaluation.

| Field | Requirement |
| --- | --- |
| `contract_id` | Stable interned ID; invalid zero. |
| `source_operator` | `common.relu`, including logical opcode version and source value ID. |
| Source evidence | Use the existing source WN/ST/DST/value records; do not duplicate source-position fields in the approximation row. |
| `function` | `relu`. |
| `approximation_family` | Approved polynomial family, such as minimax, Chebyshev, or Taylor-by-policy. |
| `polynomial_id` | Stable name/version for coefficient set. |
| `degree` | Non-negative integer degree; first release should use one fixed approved degree. |
| `coefficient_payload` | External or interned plaintext coefficient vector reference. |
| `coefficient_dtype` | Planning evidence dtype, normally `float64`, with backend lowering policy deferred. |
| `valid_input_range` | Closed numeric range assumed for approximation. |
| `max_abs_error` | Certified approximation error over the valid range. |
| `scale_policy` | Desired input/output scale relation for CKKS planning. |
| `level_policy` | Minimum level/depth budget consumed by polynomial evaluation. |
| `bootstrap_policy` | `auto`, `on`, `manual`, or `off` behavior inherited from entry config. |
| `requires_pre_refresh` | True for the first CKKS `-O0` path unless a manual boundary is already proven. |
| `provenance` | Source ReLU value, owning PU, context path, and conversion pass ID. |

SYNC-3 may attach or require the contract reference. SYNC-4 materializes
bootstrap and polynomial evaluation. Bootstrap restores capacity and does not
compute ReLU.

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
provisionally `.WHIRL.dsl_fhe_plan` / `WT_DSL_FHE_PLAN`, for conversion
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
| Unsupported grouped or dilated convolution policy | `CFHECNN-CONV-001` |
| Unsupported max pooling | `CFHECNN-POOL-001` |
| Encrypted ReLU without approximation contract | `CFHECNN-RELU-001` |
| Residual shape/layout/value-class mismatch | `CFHECNN-RESIDUAL-001` |
| CKKS state missing where required | `CFHECKKS-STATE-001` |
| Runtime/OpenFHE lowering attempted during SYNC-3 | `CFHE-LOWER-001` |
