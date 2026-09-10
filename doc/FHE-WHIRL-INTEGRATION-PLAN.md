# Open64 FHE WHIRL Integration Plan

Status: focused implementation plan for review  
Source starting points:
`/Users/shinmingliu/Documents/DSC_FHE_Compiler_Architecture_and_Integration_Plan_v0.2.docx`,
`doc/DSC_FHE_Compiler_Architecture_and_Integration_Plan_v0.10.docx`,
and `/Users/shinmingliu/open64/doc/FHE-DSL-INTEGRATION-PLAN.md`  
Related repository context: `AGENTS.md`, `doc/WHIRL-DSL-INFRASTRUCTURE.md`,
`doc/Open64_Domain_Specific_Compiler_IR_Design.md`,
`imported_docs/DSC_Master_Design_Doc_V0.17.docx`, and
`imported_docs/DSC_Master_Design_Doc_v0.9_chapter_7.md`

This plan starts the Open64 fully homomorphic encryption support effort. It is
intentionally a contract and coordination document, not an opcode-allocation
patch. The FHE effort owns FHE architecture analysis, domain semantics,
frontend and ingestion requirements, FHE gatekeeper behavior, FHE
optimization/lowering planning, runtime/library integration, and FHE-specific
tests. Shared/common opcode and type-system additions remain owned by the main
common/com task.

## Architecture Decision

The first FHE vertical slice targets full inference-only ResNet-20/CIFAR-10:
ciphertext input, ciphertext output, plaintext weights, and CKKS as the first
scheme. Smaller CNNs, micro-blocks, and vector programs are not earlier model
milestones. Deterministic add, linear, convolution, residual-add, and ReLU
fixtures remain mandatory diagnostic and unit-certification tests that support
the full model effort without delaying capture or planning for the complete
ResNet-20 graph.

Python declares encryption intent but does not perform encryption at compile
time. The compiler ingests ordinary CNN/ResNet semantics into very-high-level
WHIRL, verifies CNN and FHE contracts, adapts the graph into FHE-compatible CNN
semantics, lowers through encrypted tensor and scheme layers, and emits standard
middle-WHIRL calls for `whirl2c`.

```text
Python ResNet-20/CIFAR-10 model
  -> torch.export / FX / DSC capture
  -> open64_dsc.WhirlExportInterpreter
  -> native WHIRL builder
  -> binary very-high-level WHIRL
       CNN/ResNet/common operators
       TensorDescriptorIR
       EncryptionDescriptorIR
       FHEEntryContractIR
  -> opencc -x whirl
  -> FHE gatekeeper
  -> CNN-to-FHE model adaptation
  -> FHE-aware graph optimization
  -> encrypted tensor/vector planning
  -> scheme-independent HE planning
  -> CKKS planning
  -> middle-WHIRL standard calls
  -> whirl2c
  -> generated C + descriptor assets
  -> libdsc_fhe_cabi + selected FHE backend
  -> OpenFHE-linked executable
  -> encrypted CIFAR-10 request -> encrypted logits/result
```

The default implementation path lowers every FHE-specific node before
unmodified `whirl2c` sees the program. The generated C uses opaque handles and a
stable C ABI; backend C++ classes, CUDA types, ciphertext layouts, and key
objects remain hidden in `libdsc_fhe_cabi` and backend adapters.

### Reconciled Plan Conflicts

The updated ResNet-first architecture changes the prior plan in these places:

1. The old operator census and milestones staged through a small deterministic
   CNN and a ResNet micro-block before full ResNet-20. That is replaced by
   complete ResNet-20/CIFAR-10 as the first end-to-end target.
2. The old test ladder treated single add/linear, CNN micro-block, and ResNet
   micro-block as sequential model milestones. They are now focused
   diagnostics and certification fixtures only.
3. The old ReLU wording allowed a common activation wrapper and described the
   FHE ReLU policy as required only with an explicit approximation policy. The
   revised contract requires source ReLU to be `common.relu`; for the first
   CKKS path, each surviving `common.relu` has a mandatory refresh boundary at
   `-O0`.
4. The old bootstrap placement language was generic. The revised baseline is:
   bootstrap first, then the approved polynomial ReLU approximation.
   Bootstrap restores CKKS ciphertext capacity; it does not compute ReLU.

## Non-Negotiable Boundaries

1. Preserve existing binary WHIRL compatibility. Do not change WHIRL binary
   image layout, ELF section contracts, opcode encodings, type-kind encodings,
   or reader/writer behavior as part of this FHE planning slice.
2. Treat physical `OPR_DSL` and current carrier forms as private
   implementation details. FHE diagnostics, dumps, and pass code must use
   stable logical names.
3. Do not independently allocate common logical opcode enum values, modify
   shared type encodings, or edit shared opcode/type definitions. Publish
   handoff requests instead.
4. Represent FHE as semantic dimensions around shared operators and tensor
   types. Do not create a parallel encrypted tensor type universe.
5. Secret keys are never represented in WHIRL, generated C, descriptor assets,
   diagnostics, or server runtime state.
6. Domain operators must remain visible until FHE and source-domain gatekeepers
   finish. Do not lower CNN/FHE semantics into calls or intrinsics at ingestion.

## Source Availability Note

`WHIRL.pdf` was requested as architectural background, but no readable
`WHIRL.pdf` was found in this worktree or the accessible local tree during this
planning pass. Before any implementation that changes DSL node representation,
operator layout, type layout, mappings, binary image finalization, or ASCII
format, the responsible owner must locate and review the corresponding WHIRL
sections.

## Domain Census

### Source and Boundary Constructs

| Construct | Initial disposition | Owner | Notes |
| --- | --- | --- | --- |
| `@dsc.fhe.entry` / export option | FHE contract metadata | FHE frontend | Declares scheme, encrypted inputs/outputs, plaintext parameter policy, security level, backend constraints, and accuracy budget. |
| `CipherTensor[...]` Python annotation | Frontend declaration only | FHE frontend | Convenience syntax; native WHIRL still uses canonical tensor types plus FHE descriptors/traits. |
| FHE driver option family | Open64 driver option processing | FHE with driver review | Must follow Open64 convention: pass complete option set through phases and let each phase silently ignore unrelated options. |
| Model boundary contract | `FHEEntryContractIR` | FHE domain | Records input/output encryption policy, scheme, key-set expectations, parameter policy, security level, and conversion policy. |

### Tensor, Encryption, and Runtime State

| Construct | Initial disposition | Owner | Required semantics |
| --- | --- | --- | --- |
| Logical tensor type | Existing tensor `TY_IDX` / TensorDescriptorIR | Shared common/com | FHE must use the shared tensor type path and request fields through the common owner. |
| `EncryptedValue` trait | Tensor trait handoff | Shared common/com plus FHE semantics | Marks value confidentiality state; not a new type kind. |
| `PlaintextEncodedValue` trait | Tensor trait handoff | Shared common/com plus FHE semantics | Represents scheme-encoded plaintext weights/constants. |
| `ClearMetadata` trait | Tensor trait handoff | Shared common/com | Shape, layout, keys, cost, and policy metadata that may remain clear. |
| `EncryptionDescriptorIR` | FHE descriptor attached through tensor representation/traits | FHE domain, storage coordinated with common/com | Carries value class, scheme, security level, scale, level, slot count, packing layout, key-set handle, and boundary role. |
| `EncryptedTensorLayoutIR` | FHE planning descriptor | FHE domain | Records ciphertext count, slot mapping, gap bits, partitioning, packing density, scale/level/noise, rotations, bootstrap plan, and backend batch group. |
| `PackingPlanIR` | FHE planning descriptor | FHE domain | Fhelipe-style layout assignment, compaction, slot mapping, and compatibility state. |
| `RotationScheduleIR` | FHE planning descriptor | FHE domain | Rotation steps, Galois-key requirements, batching/fusion opportunities, and backend cost. |
| `BootstrapPlanIR` | FHE planning descriptor | FHE domain | Bootstrap regions, placement rationale, minimum-level constraints, and runtime requirements. |
| Key manifest | Runtime descriptor, not ordinary constant | FHE runtime | Evaluation keys may be referenced by opaque handles; secret keys are forbidden. |

### Operator Census

The first source model census is the complete deterministic
ResNet-20/CIFAR-10 inference graph. Focused add, linear, convolution,
residual-add, and ReLU fixtures are still required because they isolate
diagnostics, verify contracts, and produce compact review artifacts, but they
do not gate ResNet-20 capture or planning. The census must record framework node
kind, source context, operands/users, dtype/rank/shape, static parameters,
external tensors, proposed semantic disposition, and whether the operation must
remain domain-visible through gatekeeper verification.

| Source operation | Very-high-level WHIRL disposition | FHE adaptation disposition | First-slice policy |
| --- | --- | --- | --- |
| model input | `common.model_input` plus FHE boundary contract | encrypted ciphertext input | Required. |
| model output/logits | `common.model_output` / `common.output_logits` plus FHE boundary contract | encrypted ciphertext output | Required. |
| convolution | `cnn.conv2d` | `fhe.cnn.conv2d` then MetaKernel/HE lowering | Required for CNN slice. |
| batch norm inference | `cnn.batch_norm_infer` | Fold into conv weights/bias when legal | Required; reject unfused unsupported cases. |
| add / bias add | `common.add` / `common.bias_add` | HE add or plaintext bias add | Required. |
| residual add | `common.residual_add` plus CNN residual contract | FHE residual add with scale/layout alignment obligations | Required for ResNet slice. |
| ReLU | `common.relu` | Mandatory pre-ReLU bootstrap boundary followed by approved polynomial ReLU approximation | Required for ResNet-20; shared source semantics, not an FHE-domain opcode. |
| max pooling | `cnn.max_pool2d` | Average/declared approximation or reject | First slice may reject unless policy allows replacement. |
| average/global pooling | `common.window_reduce` / `cnn.global_avg_pool2d` | HE-compatible average or sum/scale sequence | Optional after conv/add path. |
| flatten/reshape | `common.flatten` / `common.reshape` | Layout reinterpretation or packing conversion | Required for classifier path. |
| linear/classifier | `common.linear` | encrypted matrix-vector / MetaKernel MVM | Required after conv path. |
| unsupported data-dependent control | unsupported diagnostic | reject | Required rejection. |
| training ops / mutation | unsupported diagnostic | reject | First release is inference-only. |

BatchNorm folding must account for the SYNC-2 shared, signature-specialized
PU structure. A compatible clone body/signature is rewritten once, while folded
weight and bias payloads are created per source call context. Callers rewrite
their actual parameter lists to pass the folded payloads for that context.
Compiler clones are split only if the rewritten structural/tensor signatures
diverge. Conversion reports distinguish physical definition rewrites from
source-context folds. The exact 13-definition/21-context census, post-fold
clone key, atomic rewrite obligations, and focused shared-clone tests are
specified in `doc/FHE-SYNC3-CONVERSION-CONTRACT.md`; payload bytes and
value-specific CKKS state are explicitly excluded from clone identity.

## Ingestion Path

Python remains a source-language frontend and artifact producer. It captures
the model with `torch.export`, FX, or a DSC DSL, classifies source operations,
and calls the native builder through opaque handles. The native builder owns
WHIRL node creation, symbol/type creation, TensorDescriptorIR attachment,
operator attributes, contracts, compiler metadata, mapped-image finalization,
and binary artifact compatibility.

Initial Python API requirements:

```python
@dsc.fhe.entry(
    scheme="ckks",
    encrypted_inputs=("x",),
    encrypted_outputs=("return",),
    parameter_policy="plaintext",
    security_level=128,
)
def forward(self, x: CipherTensor[1, 3, 32, 32]) -> CipherTensor[1, 10]:
    ...
```

Equivalent export options must be available for unmodified models:

```python
WhirlExportOptions(
    preserve_domain_ops=True,
    emit_tensor_descriptors=True,
    emit_contracts=True,
    fhe_mode="cnn_inference",
    fhe_scheme="ckks",
    fhe_bootstrap="auto",
    encrypted_inputs=["x"],
    encrypted_outputs=["return"],
    parameter_policy="plaintext",
    output_format="binary_whirl",
)
```

Ingestion exit criteria:

1. The binary `.B` artifact is consumable by `opencc -x whirl` without Python.
2. `ir_b2a -st -src` shows source interleaving, tensor descriptors, FHE
   contract markers, and stable logical operator names.
3. No FHE library calls are emitted during ingestion.
4. The first gatekeeper pass can reject missing or malformed FHE contracts
   before WOPT, LNO, CG, or `whirl2c`.

## Option Behavior

The first release publishes the bootstrap option as a compiler legality
contract, not a backend hint:

| Option | First-release behavior |
| --- | --- |
| `bootstrap=auto` | Default policy. Insert a pre-ReLU bootstrap boundary for every surviving `common.relu`, then evaluate the approved polynomial ReLU approximation. Other refreshes may be inserted deterministically when required for CKKS legality. |
| `bootstrap=on` | Same mandatory pre-ReLU behavior as `auto`; diagnostics should treat bootstrap as an explicit user-enabled capability. |
| `bootstrap=manual` | Insert no compiler-created ReLU refresh boundary. Every surviving `common.relu` must already have an explicit boundary, or compilation fails. |
| `bootstrap=off` | Forbid bootstrap. Reject any surviving `common.relu` in the first release, and reject any other path that cannot execute within the resolved modulus chain. |

A backend may later fuse bootstrap plus polynomial activation, but the logical
`common.relu`, approximation contract, bootstrap reason, source position, and
resulting CKKS state must remain visible in verification reports and
`ir_b2a -st -src` evidence.

## Gatekeeper Contracts

The FHE gatekeeper runs after structural DSL/TensorDescriptorIR validation and
before canonical lowering. It must validate both the source domain and the FHE
execution policy.

Mandatory checks:

1. Model is inference-only; training-only operators, mutable parameters, and
   data-dependent encrypted control flow are rejected.
2. Boundary policy is complete: encrypted inputs, encrypted outputs, plaintext
   parameters, scheme, security level, and key-set requirements are explicit.
3. Secret-key state is absent from WHIRL, constants, generated descriptors, and
   runtime-call operands.
4. TensorDescriptorIR is complete enough for FHE planning: element type, rank,
   logical shape, semantic role, lineage, layout, and encryption traits.
5. EncryptionDescriptorIR is complete enough for the selected stage: value
   class, scheme, scale/level binding policy, slot-count policy, key-set ID,
   and boundary role.
6. CNN contracts are still visible: convolution stride/pad/dilation/groups,
   image layout, batch-norm inference status, residual branch shape/lineage,
   classifier shape, and source metadata.
7. Unsupported nonlinearities and pooling are either paired with an explicit
   approximation policy or rejected.
8. Residual paths have compatible ciphertext state, layout group, scale/level
   obligations, and eventual alignment strategy.
9. Backend capability can satisfy scheme, security, ring degree, modulus chain,
   rotations, bootstrap needs, memory budget, serialization ABI, and target.
10. Every surviving ReLU is the common-substrate logical operator
    `common.relu`. FHE lowering may attach approximation and refresh contracts
    to it, but must not replace source ReLU with an FHE-domain ReLU opcode
    before the source semantics, approximation contract, source position, and
    CKKS state are inspectable.
11. Bootstrap policy is explicit. With `bootstrap=auto` or `bootstrap=on`, the
    first CKKS `-O0` path inserts a bootstrap immediately before every
    surviving `common.relu`. With `bootstrap=manual`, each surviving
    `common.relu` must already have an explicit refresh boundary. With
    `bootstrap=off`, a surviving `common.relu` is rejected in the first
    release.
12. Bootstrap restores CKKS ciphertext capacity. It does not compute ReLU; the
    approved polynomial approximation remains the activation semantics.
13. Diagnostics use stable families such as `CFHE-*`, `CFHECNN-*`,
    `CFHELAYOUT-*`, `CFHECKKS-*`, and `CFHERT-*`.

Generic gatekeeper checks validate semantic relationships, descriptor
completeness, security policy, and conversion legality. Exact ResNet-20
cardinality such as PU, callsite, REGION, convolution, BatchNorm, residual, or
ReLU counts belongs to the ResNet-20 certification profile and retained
artifact assertions, not to the generic gatekeeper.

## Optimization and Lowering Stages

The FHE pass pipeline should be staged so correctness is established before
cryptographic performance choices harden:

1. FHE model adaptation: classify ciphertext/plaintext/clear values; fold batch
   norm; preserve source `common.relu`; attach the approved polynomial
   approximation contract; replace or reject unsupported activations/pooling;
   create an FHE conversion report.
2. FHE-aware graph optimization: reduce multiplicative depth, remove redundant
   encrypted transforms, optimize residual paths, and preserve packing/fusion
   opportunities.
3. Encrypted tensor/vector planning: assign packing layouts, slot maps,
   ciphertext partitions, gap compaction, rotation schedules, residual
   alignment groups, and output layout compatibility.
4. MetaKernel planning: represent encrypted convolution and MVM plans with
   horizontal/vertical batching, block structure, rotation cost, and ciphertext
   utilization.
5. Scheme-independent HE planning: lower to HE semantics such as add, sub, mul,
   neg, rotate, encode, bootstrap, and runtime-validation/reference operations
   without committing to CKKS implementation details too early. For the first
   CKKS path, every surviving `common.relu` materializes as an explicit
   pre-activation refresh boundary followed by polynomial evaluation. This
   vocabulary should intentionally align with ACE `SIHE` where the semantics
   match.
6. CKKS planning: assign arithmetic operations, scale, level, batch size,
   modulus chain, rescale, upscale, modulus switch, relinearization, rotation,
   conjugation, monomial multiply, raise-mod, bootstrap placement, and
   ciphertext lifetime. At `-O0`, ReLU refresh boundaries are mandatory
   correctness materialization, not profitability optimization. At `-O1+`,
   movement, merging, deduplication, or fusion of those baseline boundaries
   requires legality, numerical-equivalence, scale/level, and provenance proof.
   This vocabulary should intentionally align with ACE `CKKS` where the
   semantics match; Open64 may keep longer source-facing names only when
   compatibility or readability requires it.
7. Library-call lowering: lower to standard WHIRL calls and descriptor tables
   accepted by `whirl2c`.
8. Optional native GPU path: later lower CKKS/POLY/RNS primitives to allocation
   and initialization, NTT/INTT, RNS extension, modulus up/down, decomposition,
   coefficient access, key switching, hardware modular arithmetic, fused
   polynomial operations, bootstrap, and CUDA/Triton backend code. This is not
   the MVP, but ACE `POLY` should be the first comparison point for naming and
   phase boundaries.

`VHO_FHE_Convert_Driver()` is the SYNC-3 model-adaptation boundary. It runs
after optional DSL WOPT/Preopt and before `VHO_DSL_Lower_Driver()`, so FHE
conversion sees the optimized very-high-level DSL graph but still completes
before generic DSL lowering hides source-domain contracts.

## Runtime and C ABI

The MVP runtime target is an opaque C ABI backed first by a mock/reference
backend and then OpenFHE/reference CKKS. GPU backends such as FIDESlib or a
Cheddar-like runtime should be added only after the CPU/mock path proves the
compiler boundary.

Representative handle model:

```c
typedef struct dsc_fhe_context_s*    dsc_fhe_context_t;
typedef struct dsc_fhe_model_s*      dsc_fhe_model_t;
typedef struct dsc_fhe_ciphertext_s* dsc_fhe_ciphertext_t;
typedef struct dsc_fhe_plaintext_s*  dsc_fhe_plaintext_t;
typedef struct dsc_fhe_keyset_s*     dsc_fhe_keyset_t;
```

Generated C should contain static operation descriptors and opaque runtime
calls, not backend object layouts. Plaintext weights may be emitted as external
binary assets, model-package sections, or preencoded backend caches. Any
key-dependent precomputation must be visible in the model/runtime manifest.

## Mapped-Image and `ir_b2a -st -src` Requirements

FHE artifacts must remain reviewable through the existing mapped WHIRL image
discipline.

Required visibility:

1. Stable logical names for CNN/common/FHE operators and contracts.
2. TensorDescriptorIR fields relevant to FHE: type core, shape, layout,
   semantic role, traits, lineage, and representation descriptor hooks.
3. EncryptionDescriptorIR summary: value class, scheme, security level,
   boundary role, scale/level binding state, slot-count state, packing layout
   ID, and key-set handle ID. Dumps must redact key material and ciphertext
   internals.
4. FHEEntryContractIR summary: encrypted inputs/outputs, parameter policy,
   approximation policy, bootstrap policy, backend policy, and
   diagnostic/certification status.
5. Conversion, layout, depth, bootstrap, and backend planning reports as
   reviewable side artifacts linked from the retained test artifact directory.
6. ReLU evidence: original `common.relu` source evidence through existing
   WN/ST/DST/value records, approximation contract ID, mandatory pre-ReLU
   bootstrap boundary or explicit manual boundary, bootstrap reason, and
   resulting CKKS state. Approximation records reference source evidence; they
   do not duplicate source-position fields.
7. `ir_b2a -st -src input.B input.T` naming using the input `.B` stem, with the
   original source path preserved when source interleaving is expected.

FHE must not change `.WHIRL.dsl_fhe` version 1 or add new binary sections or
type encodings without a reviewed common/com compatibility plan. SYNC-3
conversion-plan records should stage in a separate optional fixed-row
`.WHIRL.dsl_fhe_plan` section only after an exact physical contract is
published. If FHE descriptors initially use existing side-table or metadata
mechanisms, the dumps must still present a descriptor-shaped logical view.

The main/common physical review is
`doc/FHE-SYNC3-NATIVE-PLAN-CONTRACT.md`. The FHE implementation consumes its
opaque services and must not reinterpret the fixed rows or duplicate their
referenced WN/ST/DST/value, TY, callsite, FHE-v1, or TCON evidence.

## Compatibility Strategy

1. Keep older WHIRL readers deterministic: legacy images without FHE metadata
   remain valid, and images with required unlowered FHE contracts are rejected
   clearly by unaware paths rather than silently miscompiled.
2. Use stable string names and versions for all proposed FHE logical operators
   and descriptors. Exact registered versions are accepted; unknown or newer
   versions are rejected until migration code exists.
3. Treat common tensor type equivalence separately from FHE runtime state.
   Encryption state, scale/level, packing, key-set handles, and backend
   planning are representation/domain compatibility facts, not a new scalar
   type universe.
4. Run SYNC-3 FHE conversion after optional DSL WOPT/Preopt and before
   `VHO_DSL_Lower_Driver()`. Lower all FHE-specific semantics before LNO/CG
   and before `whirl2c` unless a later reviewed stage extends those consumers
   deliberately.
5. Keep `ir_a2b` and `ir_b2a` as compatibility gates for any text/binary
   surface exposed by tests.

### SYNC-0 Binary Compatibility Decision and Reader Matrix

SYNC-0 accepts an infrastructure-first compatibility rule: this FHE plan does
not change WHIRL binary image layout, ELF section contracts, opcode encodings,
type-kind encodings, or the private physical `OPR_DSL` abstraction. Any optional
FHE mapped-image section, fixed FHE record table, or reader/writer hook remains
a SYNC-1 common/com compatibility decision and must land with bounds checks,
legacy-image tests, and `ir_b2a -st -src` coverage.

| File/image class | Current reader behavior required at SYNC-0 | Future-aware reader behavior required after SYNC-1 |
| --- | --- | --- |
| Existing non-FHE `.B` without DSL/FHE metadata | Reopens unchanged; no empty FHE section is expected or required. | Same behavior; printer emits no empty FHE report. |
| Existing DSL `.B` with logical common/CNN operators | Existing logical DSL registry remains authoritative; physical `OPR_DSL` stays private. | Same behavior plus stronger diagnostics where FHE contracts are present. |
| ResNet-20 frontend `.B` with FHE contracts represented through reviewed existing carriers | Must be rejected by unaware FHE paths rather than silently compiled as ordinary WHIRL. | Reopens, verifies, and prints TensorDescriptorIR, FHE entry/encryption contracts, source positions, and stable logical names. |
| Future `.B` with optional FHE fixed-record section | Not introduced at SYNC-0. | Accepted only by readers that recognize the section version/capabilities; malformed first/count ranges are rejected before pass use. |
| Future `.B` with unknown FHE record version or required capability | Not introduced at SYNC-0. | Rejected with a precise unsupported-version or unsupported-capability diagnostic. |
| Middle-WHIRL `.B` after FHE runtime-call lowering | Contains only standard WHIRL calls, symbols, initializers, formals, results, status checks, and control flow. | Same behavior; any remaining FHE/SIHE/CKKS/HPOLY node is a verifier failure before `whirl2c`. |

## Tests and Review Artifacts

All tests that produce meaningful WHIRL evidence must retain review artifacts
until the next run replaces them. Each FHE test family should write to a clear
per-test directory containing the source fixture, `.B` file, `ir_b2a -st -src`
trace, phase traces, descriptor reports, diagnostics, generated C when
applicable, and runtime logs.

Initial validation ladder:

1. ResNet-20 capture census: export the complete deterministic
   ResNet-20/CIFAR-10 inference graph with `@dsc.fhe.entry`; verify
   `ir_b2a -st -src` shows class-centric PUs, CNN/common operators, source
   positions, FHE entry contract, and encrypted tensor traits.
2. Focused certification fixtures: deterministic add, linear, convolution,
   residual-add, and `common.relu` tests verify individual contract behavior,
   diagnostics, and retained artifacts. They support debugging the full model
   path but are not earlier model milestones.
3. Gatekeeper negative tests: missing scheme, missing encrypted output policy,
   secret-key marker, unsupported activation without approximation policy,
   training op, dynamic encrypted branch, malformed EncryptionDescriptorIR,
   missing manual ReLU refresh boundary, and `bootstrap=off` with surviving
   `common.relu`.
4. ReLU refresh certification: prove that `bootstrap=auto|on` inserts the
   pre-ReLU boundary before polynomial approximation at `-O0`; prove that
   `bootstrap=manual` requires an explicit boundary; prove that `bootstrap=off`
   rejects a surviving ReLU.
5. Runtime-call lowering fixture: lower deterministic add/linear to C ABI
   calls, run through `whirl2c`, compile generated C, link with mock runtime,
   and return an opaque ciphertext handle.
6. ResNet-20 FHE conversion certification: validate folded batch norm,
   convolution, residual alignment, common ReLU approximation contracts,
   pooling policy, classifier path, encrypted logits, conversion report, and
   converted FHE WHIRL evidence. Bootstrap refresh and generated C call
   sequence validation begin in later sync points.
7. `-O0` end-to-end acceptance: compile complete ResNet-20 binary WHIRL through
   FHE/CKKS lowering, `whirl2c`, generated-C compilation, and final
   OpenFHE-linked executable creation. The executable imports context,
   evaluation keys, and encrypted CIFAR-10 input, then produces encrypted
   logits/result without server-side secret-key use.
8. Optimized-planning certification: at `-O1+`, any movement, merging,
   deduplication, or fusion of baseline ReLU refresh boundaries must carry
   legality, numerical-equivalence, scale/level, and provenance proof against
   the `-O0` baseline.
9. GPU planning dry run: backend capability record, layout report, and cost
   model report without requiring native GPU POLY/RNS lowering.

Required reports:

1. Compilation report: operator coverage, rewrites, unsupported operations, and
   emitted runtime calls.
2. Encryption report: scheme, ring degree, modulus chain, scale, security
   level, and key requirements.
3. Layout report: ciphertext count, slot occupancy, gap/compaction decisions,
   rotations, and conversions.
4. Depth report: multiplicative depth, level by node, bootstrap locations, and
   residual alignment.
5. Accuracy report: plaintext source model, adapted plaintext model, and
   encrypted result comparison.
6. Runtime report: per-operation latency, memory, key memory, transfer cost,
   and backend utilization.
7. Certification report: gatekeeper status and stable diagnostics.

## Milestones

`/Users/shinmingliu/open64/doc/FHE-CONSOLIDATED-IMPLEMENTATION-PLAN.md`
is the coordination authority for milestone order, PR sequencing, retained
artifact families, and cross-task exit criteria. This local plan uses the same
SYNC vocabulary.

Cross-PU value identity, generic tensor shape propagation, and FHE-specific
CKKS-state propagation are specified in
`doc/FHE-SHAPE-AND-ENCRYPTION-STATE-PROPAGATION.md`. The central invariant is
that caller context, argument role, callee formal `DSL_IR_VALUE_ID`, callee
operator operand, and result value remain structurally connected. Shape or
`TY_IDX` equality must never substitute for callee-specific value metadata.

| Sync | Scope | FHE task exit criteria |
| --- | --- | --- |
| SYNC-0: Plan and contract reconciliation | Baseline freeze and handoff review | Consolidated plan is accepted; this plan marks common/CNN requests as reuse, extend, promote, or new; complete ResNet-20 operator census scope, FHE descriptors, option semantics, and handoff requests are frozen before implementation. |
| SYNC-1: Native API and image contract freeze | Shared native contracts before frontend certification | FHE record layouts, builder API requests, malformed-record rules, printer spelling, and negative-test matrix are finalized against main-owned common/type, mapped-image, and gatekeeper hooks. |
| SYNC-2: Frontend artifact certification | Complete ResNet-20 capture using merged opaque APIs | `artifacts/fhe/resnet20_capture/` retains source, weights, `.B`, `ir_b2a -st -src` `.T`, operator census, options, and gatekeeper log; every source ReLU is existing `common.relu`; Python invents no bootstrap or CKKS operators. |
| SYNC-3: ResNet FHE conversion review | FHE gatekeeper and CNN-to-FHE conversion | After identity validation, run generic XLA-style shape propagation, legal BatchNorm folding, converted-shape verification, and value-specific CKKS-state propagation. `secure_resnet20.fhe.B`, `.T`, side payload, and report show callee-value identity, 13 definition/21 context folds, operator dispositions, ReLU obligations, CKKS state, and stable diagnostics. |
| SYNC-4: ReLU `-O0` baseline certification | Mandatory pre-ReLU refresh and composite polynomial approximation | Certify the selected ACE-compatible Chebyshev sign profile with ordered degrees `7 -> 15 -> 13`, depth 11, exact coefficient bytes/checksums, identity-bound normalization ranges, model accuracy, and CKKS state. Then `bootstrap=auto|on` inserts one required pre-ReLU boundary per surviving `common.relu`; `manual` requires explicit compatible boundaries; `off` rejects surviving ReLU; no `-O0` movement, merging, deduplication, or profitability placement occurs. |
| SYNC-5: Middle-WHIRL and mock executable gate | Standard WHIRL boundary and mock runtime | `secure_resnet20.mid.B` and `.T` contain only standard WHIRL calls, formals, symbols, initializers, status checks, and control flow; `whirl2c` emits C that compiles and links with the mock FHE C ABI. |
| SYNC-6: End-to-end `-O0` acceptance | Complete OpenFHE ResNet path | Full ResNet-20 binary WHIRL lowers through FHE/CKKS, `whirl2c`, generated-C compilation, and OpenFHE provider link; the executable imports context/evaluation keys/encrypted CIFAR-10 input, uses no server-side secret key, and returns encrypted logits/result within budget. |
| SYNC-7: Optimized-versus-`-O0` proof | ReSBM, boundary movement/fusion, HPOLY/HPAO | Every optimized transform has an independent option and proves source semantics, approximation error, CKKS scale/level legality, key availability, provenance, and tolerance against the retained `-O0` baseline. |
| SYNC-8: Separate GPU architecture review | GPU capability/layout/cost and later POLY/RNS path | GPU work remains separate from the OpenFHE CPU/reference milestone; provider capability, target description, memory/lifetime, POLY/RNS contracts, toolchain, fallback, telemetry, and regression methodology are reviewed before implementation. |

### Actionable Work List by Sync Point

This checklist makes the coordination work explicit. The consolidated plan
remains authoritative when ownership or ordering questions arise.

| Sync | Main/common work | FHE task work | Exit handoff |
| --- | --- | --- | --- |
| SYNC-0 | Inventory existing common/CNN operators, tensor APIs, builder hooks, binary WHIRL constraints, and current `common.relu` / `OPR_DSLRELU` support. Classify requested contracts as reuse, extension, promotion, or new. | Publish the FHE operator/type handoff table; freeze ResNet-20-first scope, FHE descriptors, option semantics, ReLU refresh policy, and test strategy. | Accepted contract matrix, binary compatibility decision, and shared planning baseline. No opcode/type allocation or image coding starts before closure. |
| SYNC-1 | Add and certify the optional `.WHIRL.dsl_fhe` image, fixed row ABI, mapped-image read/write, printer, validation, and opaque builder APIs. Preserve old and non-FHE `.B` behavior. | Provide the semantic row proposal, deduplication keys, malformed-image cases, and post-merge consumption notes. Rebase after merge and consume only `DSL_FHE_*` / `DSL_Builder_*` APIs. | `doc/FHE-SYNC1-NATIVE-CONTRACT.md` controls physical layout and API names; FHE proposal text is retained only as semantic input. |
| SYNC-2 | Review shared operator/type/source/side-file/FHE table evidence. Fix common-owned printer or ownership-validation issues, including FHE entry-value local-symtab safety. Provide optional call-ABI and complete PU-interface identity records when cross-PU consumers require them. | Capture complete deterministic ResNet-20/CIFAR-10 with class-centric PUs, five context-specialized `ResNet20Block` compiler PUs, nine callsites, source positions, external weights, FHE contracts, argument roles, and complete formal/result identities. Preserve every source ReLU as existing `common.relu`; emit no Python bootstrap, CKKS, SIHE, or FHE conversion operators. | Retained capture family with source, `.B`, independent-process `.T`, side file, census, options, gatekeeper log, and enough identity evidence to join every caller actual to its exact callee value. |
| SYNC-3 | Provide driver phase hook, logical DSL/FHE read APIs, call-ABI and PU-interface queries, generic shape-analysis hooks, transactional rewrite/retirement, value-state attachment, checkpoint publication, diagnostics, and printer support. | Run generic shape certification; implement FHE gatekeeping, legal 13-definition/21-context BatchNorm folding, converted-shape verification, operator dispositions, ReLU approximation obligations, value-specific CKKS-state propagation, reports, and stable diagnostics. Preserve caller-to-callee value identity throughout. | The conversion and propagation contracts are satisfied; `secure_resnet20.fhe.B`, `.T`, converted payload, and report show identity joins, shapes, operator provenance, and CKKS-state disposition. |
| SYNC-4 | Preserve and print `common.relu` source, result, descriptor, composite-profile stages, context ranges, and provenance evidence through conversion. PR #123 supplies the append-only ordered-stage representation while preserving the v1 single-polynomial row. | Certify `ace.chebyshev.sign.7x15x13.depth11.v1`, then materialize mandatory pre-ReLU refresh, context normalization, three ordered Chebyshev stages, and ReLU reconstruction; enforce `auto|on`, `manual`, and `off` behavior. | Artifacts prove stage order, coefficient/manifest checksums, 19 identity-bound ranges, clear/model/CKKS error, depth 11, and no `-O0` boundary movement, merging, deduplication, or profitability placement. |
| SYNC-5 | Supply standard WHIRL call/result construction, unlowered-node gate, and assigned `whirl2c` integration edits. | Lower FHE/SIHE/CKKS constructs to standard runtime calls; publish stable mock FHE C ABI; implement mock provider and generated-C compile/link tests. | `secure_resnet20.mid.B`, `.T`, generated C, and mock-linked executable evidence contain only standard WHIRL at the `whirl2c` boundary. |
| SYNC-6 | Complete driver link flow, provider manifest consumption, and retained artifact expectations. | Implement the OpenFHE provider path, client provisioning, context/evaluation-key import, CKKS execution, encrypted CIFAR-10 input handling, and encrypted-logit result production. | Full `-O0` ResNet-20 binary WHIRL-to-OpenFHE executable path passes without server-side secret-key material. |
| SYNC-7 | Enable reviewed VHO/WOPT integration points and per-pass controls. | Add ReSBM and optional optimization passes for boundary movement, merging, deduplication, fusion, HPOLY/HPAO planning, and reports. Each transform must prove legality, numerical equivalence, CKKS scale/level correctness, key availability, provenance, and tolerance against the retained `-O0` baseline. | Optimized artifacts and reports compare cleanly against the `-O0` baseline, with each transform controlled independently. |
| SYNC-8 | Coordinate target-description, runtime, and integration expectations for GPU work. | Publish GPU capability, layout, cost, memory/lifetime, async execution, fallback, telemetry, and regression methodology. Defer native POLY/RNS and GPU lowering until review closes. | Separate GPU architecture review closes before any GPU-specific implementation enters the main FHE path. |

## SYNC-1 Native API and Image Contract Closure

Status: closed. PR #102 merged into `develop` at `8ba9ee31`, with
`doc/FHE-SYNC1-NATIVE-CONTRACT.md` as the authoritative native contract.

The precise FHE-owned SYNC-1 proposal is
`doc/FHE-SYNC1-NATIVE-IMAGE-API-PROPOSAL.md`. That proposal is retained as
semantic input and post-merge consumption notes, not as physical ABI text.

The reconciled main/common contract is
`doc/FHE-SYNC1-NATIVE-CONTRACT.md`. It is authoritative for `.WHIRL.dsl_fhe`,
`WT_DSL_FHE_IMAGE`, row sizes/alignment, Open64 index widths, tensor identity,
entry/encryption/key records, builder API names, mapped-image compatibility,
validation, and `ir_b2a -st -src` table headings.

### SYNC-1 Consumption Decisions

| Topic | Closed decision |
| --- | --- |
| Image carrier | Separate optional `.WHIRL.dsl_fhe` section with `WT_DSL_FHE_IMAGE`; existing `.WHIRL.dsl` is not extended. |
| Compatibility | Non-FHE writers omit the section; new readers treat absence as empty; legacy readers ignore unknown optional `SHT_MIPS_WHIRL` sections. |
| Row contract | Exact v1 sizes: header 64, config 64, entry contract 48, entry value 32, encryption descriptor 56, tensor binding 24, key requirement 48; section and rows are 8-byte aligned. |
| Index widths | `TY_IDX` and `ST_IDX` are 32-bit; `STR_IDX` is 64-bit. |
| Tensor identity | Canonical `TY_IDX` is the TensorDescriptorIR identity; no parallel TensorDescriptorIR ID is persisted. |
| FHE representation | EncryptionDescriptorIR is interned representation semantics and binds independently to canonical tensor TY. |
| CKKS value state | Level, scale, component count, and precision are deferred value-specific state; canonical TY is never mutated for them. |
| Frontend surface | SYNC-2 consumes only opaque `DSL_FHE_*` services and `DSL_Builder_*` wrappers through the native bridge. |
| Validation scope | `DSL_Builder_Verify_Program` invokes `DSL_FHE_Image_Validate`; deeper FHE gatekeeper diagnostics and unlowered-node gates remain later milestones. |
| Inspection | Stable headings are the six `FHE ... Table:` sections defined in `FHE-SYNC1-NATIVE-CONTRACT.md`. |

### SYNC-2 API-Binding Checkpoint

The FHE frontend binding checkpoint for SYNC-2 is:

1. The Python native bridge exposes opaque wrappers for the merged config,
   encryption descriptor, tensor binding, entry contract, entry value, key
   requirement, and value encryption descriptor query APIs.
2. Python calls pass declarative dictionaries and opaque handles only. They do
   not construct WN nodes, inspect TY/ST indexes, or write mapped-image rows.
3. Ordinary non-FHE ResNet exports remain unchanged and emit no FHE section.
4. The SYNC-2 ResNet-20 certification lane opts in to FHE boundary contracts
   and encrypted/encoded-plaintext representation descriptors after graph
   capture, before native verification/finalization.

Status: certified after PR #104 merged into `develop` (`e72ce709`). The FHE
branch consumes the merged PR #102, PR #103, and PR #104 substrate through the
torch2whirl native bridge and retains review evidence under
`artifacts/fhe/resnet20_capture/{secure_resnet20.py,secure_resnet20.B,secure_resnet20.T,secure_resnet20.safetensors,operator-census.txt,capture-options.txt,gatekeeper.log}`.
The captured model is a complete deterministic ResNet-20/CIFAR-10 inference
graph with `common.relu` preserved, no Python-created bootstrap/CKKS/SIHE/FHE
conversion operators, and external plaintext weights retained in the side file.
The native image contains the `SecureResNet20` entry PU plus five
signature-specialized `ResNet20Block` compiler PUs, nine explicit block
callsites, and five `cnn.basic_block.v1` REGION contracts inside the clone PUs.
FHE entry rows resolve through `owner_pu=SecureResNet20`; source-derived
external tensor parameters and implicit parameters have non-null constructor
source locations. The retained census records 11 reusable `common.relu` node
definitions representing 19 source-context ReLU uses.
The source definition remains `secure_resnet20.ResNet20Block.forward`; repeated
uses are represented as context-sensitive callsites and structural/type
signature clones, not operator or function-version changes.

### Deduplication and Equivalence Keys

| Object | Deduplication key | Not part of equivalence |
| --- | --- | --- |
| Compilation config | Scheme, security, ring dimension, depth policy, scale bits, first modulus bits, slots policy, key-switch policy, bootstrap policy, backend policy | Source option spelling, diagnostics, profile name |
| Encryption descriptor | Value class, scheme, config ID, CKKS state ID, encrypted layout ID, key-set ID, boundary role, confidentiality flags | Source symbol name, source line, pass provenance |
| FHE tensor binding | Canonical TensorDescriptorIR ID plus EncryptionDescriptorIR ID; value identity only when the descriptor is value-specific | Diagnostic labels, temporary names, selected backend implementation |
| Approximation contract | Source operator identity, approximated function, degree, coefficient identity, valid range, error budget | Test name, report path, runtime timing |
| CKKS value state | Level, scale, basis, component count, precision estimate, pending rescale/relinearization obligations | Source name, debug text |
| Key requirement | Key class, key-set ID, config ID, rotation offsets, bootstrap profile, relinearization requirement | Key file path spelling, secret-key provenance |
| Backend requirement | Provider ABI version, capabilities, target class, serialization format, memory policy | Host path, local installation prefix |

### Opaque Native Builder API Consumption

The frontend receives opaque handles only and consumes the exact merged names:

```c++
typedef UINT32 DSL_FHE_CONFIG_ID;
typedef UINT32 DSL_FHE_ENTRY_CONTRACT_ID;
typedef UINT32 DSL_FHE_ENTRY_VALUE_ID;
typedef UINT32 DSL_FHE_ENCRYPTION_DESCRIPTOR_ID;
typedef UINT32 DSL_FHE_TENSOR_BINDING_ID;
typedef UINT32 DSL_FHE_KEY_REQUIREMENT_ID;

DSL_FHE_CONFIG_ID
DSL_FHE_Intern_Compilation_Config(
    const DSL_FHE_COMPILATION_CONFIG_RECORD *record);

DSL_FHE_ENCRYPTION_DESCRIPTOR_ID
DSL_FHE_Intern_Encryption_Descriptor(
    const DSL_FHE_ENCRYPTION_DESCRIPTOR_RECORD *record);

DSL_FHE_TENSOR_BINDING_ID
DSL_Builder_Bind_FHE_Tensor_Descriptor(
    TY_IDX tensor_ty,
    DSL_FHE_ENCRYPTION_DESCRIPTOR_ID descriptor_id,
    UINT32 flags);

DSL_FHE_ENTRY_CONTRACT_ID
DSL_Builder_Attach_FHE_Entry_Contract(
    DSL_BUILDER_PROGRAM_UNIT pu,
    const DSL_FHE_ENTRY_CONTRACT_INFO *info);

DSL_FHE_ENTRY_VALUE_ID
DSL_Builder_Declare_FHE_Entry_Value(
    DSL_FHE_ENTRY_CONTRACT_ID entry,
    DSL_BUILDER_VALUE value,
    UINT32 ordinal,
    DSL_FHE_ENTRY_VALUE_ROLE role,
    const DSL_FHE_ENTRY_VALUE_INFO *info);

DSL_FHE_KEY_REQUIREMENT_ID
DSL_FHE_Intern_Key_Requirement(
    const DSL_FHE_KEY_REQUIREMENT_RECORD *record);

BOOL
DSL_Builder_Get_FHE_Value_Encryption_Descriptor(
    DSL_BUILDER_VALUE value,
    DSL_FHE_ENCRYPTION_DESCRIPTOR_RECORD *record);
```

`common.relu` construction must continue to use the existing common logical
operator path. FHE APIs may attach approximation and bootstrap provenance to
the value after conversion, but they must not create a second ReLU constructor.

### Malformed Record and Gatekeeper Test Matrix

| Test class | Expected result |
| --- | --- |
| Old non-FHE `.B` | Reopens unchanged and prints no empty FHE section. |
| New non-FHE `.B` | Does not emit an empty FHE section. |
| Bad FHE image magic or unsupported required capability | Reader rejects with precise version/capability diagnostic. |
| Invalid nonzero ID reference | Reader/gatekeeper rejects before pass use. |
| Out-of-range `first/count` span | Reader rejects before exposing records. |
| Duplicate FHE entry ordinal | Gatekeeper rejects the entry contract. |
| Missing encrypted input/output declaration | Gatekeeper rejects with boundary diagnostic. |
| Tensor binding without TensorDescriptorIR or EncryptionDescriptorIR | Gatekeeper rejects with descriptor diagnostic. |
| Secret-key material, decrypt op, or server key-generation request | Gatekeeper rejects; no redacted valid record is created. |
| `common.relu` without approximation contract in encrypted CKKS path | FHE conversion/gatekeeper rejects. |
| `bootstrap=manual` with surviving ReLU and no explicit refresh boundary | FHE gatekeeper rejects. |
| `bootstrap=off` with surviving ReLU | FHE gatekeeper rejects in the first CKKS release. |
| Unlowered FHE/SIHE/CKKS node before `whirl2c` | Lowering gate rejects before C emission. |

### `ir_b2a -st -src` Spelling Freeze

When records exist, the printer should use these section headings:

```text
FHE Compilation Configurations
FHE Entry Contracts
FHE Entry Values
FHE Encryption Descriptors
FHE Tensor Bindings
FHE Approximation Contracts
FHE CKKS Value States
FHE Key Requirements
FHE Backend Requirements
```

Each printed DSL expression or FHE record must use stable logical names,
record IDs, source position, result symbol/TY, TensorDescriptorIR ID,
EncryptionDescriptorIR ID, approximation contract ID when present, bootstrap
reason when present, and CKKS state ID when known. The printer must not expose
physical `OPR_DSL` payload details, secret-key material, ciphertext bytes, or
backend C++ object layouts.

### SYNC-1 Exit Evidence Checklist

Main/common implementation merged through PR #102 at `8ba9ee31`.  The FHE task
accepted the semantic split without blocking conflicts and must rebase on that
merged foundation before producing SYNC-2 artifacts.  The exact accepted
physical contract is `FHE-SYNC1-NATIVE-CONTRACT.md`.

1. Main infrastructure PR has merged first, or the implementation remains
   blocked at contract-only review.
2. FHE branch is rebased on updated `develop`; duplicate SYNC-0 patches are
   omitted.
3. Headers for the accepted API compile on the shared base.
4. Old non-FHE `.B` files reopen unchanged.
5. New non-FHE `.B` files do not emit empty FHE sections.
6. A minimal native FHE producer writes, reopens, verifies, and prints one FHE
   image with `ir_b2a -st -src`.
7. Negative malformed-record tests fail with stable diagnostics.
8. The exact artifact directory contains `.B`, `.T`, diagnostics, and command
   lines for review.

## Coordination Checkpoints

1. SYNC-0: main task reviews the handoff table below and publishes whether each
   requested operator/type contract is reuse, extension, promotion, or new.
2. SYNC-1: before native FHE files are implemented, confirm ownership, version,
   binary compatibility impact, fixed FHE records, `ir_b2a -st -src` spelling,
   and gatekeeper tests.
3. SYNC-2: before ResNet-20 capture is certified, frontend and common owners
   confirm merged opaque native builder handles for existing `common.relu`, CNN
   operators, FHE entry contracts, encryption descriptors, approximation
   contracts, source positions, and external plaintext weights.
4. SYNC-3 and SYNC-4: before FHE conversion and ReLU refresh certification
   close, verify conversion reports, ReLU provenance, approximation contracts,
   bootstrap policy behavior, and retained `.B`/`.T` artifacts.
5. SYNC-5: before FHE lowering reaches `whirl2c`, verify no custom logical
   operator remains in the generated middle-WHIRL path unless `whirl2c` support
   was deliberately reviewed.
6. SYNC-6: before OpenFHE/reference acceptance, review runtime ABI, key
   manifest, serialization format, link/provider manifest, and security
   diagnostic redaction policy.
7. SYNC-7 and SYNC-8: before optimized or GPU work, preserve the `-O0` baseline
   artifacts and review proof, backend capability, memory ownership, async
   execution, fallback, and telemetry contracts.

Pull requests follow the consolidated protocol: main infrastructure PRs merge
first, the FHE branch rebases on updated `develop`, duplicate patches are
omitted, focused and full-model tests rerun, and the FHE PR links retained
`.T` evidence.

## Frontend and Common Infrastructure Dependencies

The ResNet-first plan depends on these reviewed contracts before broad coding:

1. The frontend observes and emits opaque native handles only. It must not
   inspect `TY_IDX`, `ST_IDX`, WN fields, physical `OPR_DSL`, opcode enum
   values, mapped-image offsets, OpenFHE objects, or key material.
2. Common/com owns accepted logical operator contracts, including
   `common.relu`, mapped-image compatibility, stable binary/ASCII names,
   TensorDescriptorIR printing, gatekeeper hooks, and `ir_b2a -st -src`
   evidence.
3. The FHE task owns FHE entry and encryption semantics, approximation and
   bootstrap policy, FHE gatekeeper behavior, CKKS state planning, runtime ABI
   mapping, retained FHE artifacts, and FHE-specific tests.
4. ResNet-20 capture requires opaque builder APIs for model boundary values,
   external plaintext weights, source positions, tensor descriptors,
   encryption descriptors, `common.relu`, CNN operators, residual lineage, and
   approximation contract attachment.
5. No frontend-only substitute may encode bootstrap placement, CKKS state,
   TensorDescriptorIR equivalence, or WHIRL compatibility outside the native
   contracts reviewed by common/com.

## Main-Agent Handoff: Required Shared Opcode and Type Additions

The following table is a request for shared-contract review. It does not assign
enum values, change type encodings, or require binary layout changes by itself.

### SYNC-0 Contract Classification Matrix

SYNC-0 classifies each requested contract as reuse, extension, promotion, or
new. This matrix is the accepted handoff baseline for ownership and ordering;
it does not allocate enum values or authorize shared-file edits.

| Contract family | SYNC-0 classification | Owner | Decision |
| --- | --- | --- | --- |
| `common.model_input` | Reuse existing native contract | Main common/com | Reuse `OPR_DSLMODELINPUT` and `common.model_input.v2`; FHE supplies encrypted-boundary requirements. |
| `common.model_output` | Defer native promotion | Main common/com | Registry vocabulary exists, but ResNet-20 uses native `common.output_logits`; revisit only after a captured model requires generic model output. |
| `common.output_logits` | Reuse existing native contract | Main common/com | Reuse `OPR_DSLOUTPUTLOGITS`; ResNet-20 uses version 2 and FHE adds encrypted-output policy only. |
| `common.add` | Reuse existing common arithmetic contract | Main common/com | FHE supplies encrypted/plain legality, scale/level, and lowering rules. |
| `common.bias_add` | Defer native promotion | Main common/com | Registry vocabulary exists; first-slice conv/linear operands and BatchNorm folding preserve bias semantics without a new native operator. |
| `common.mul` | Reuse existing native contract | Main common/com | Reuse `OPR_DSLMUL`, version 1; FHE supplies depth, ciphertext/plaintext, and scale/level obligations. |
| `common.relu` / `OPR_DSLRELU` | Reuse and semantically strengthen existing registry contract | Main common/com | Current source already contains `OPR_DSLRELU` and logical `common.relu`; no second ReLU opcode or replacement enum is allowed. |
| `common.linear` | Reuse existing native contracts | Main common/com | Reuse `OPR_DSLLINEAR` versions 2 and 3; FHE supplies plaintext-weight and MVM/MetaKernel requirements. |
| `common.flatten` | Reuse existing native contract | Main common/com | Reuse `OPR_DSLFLATTEN`, version 2; FHE supplies encrypted-layout reinterpretation/conversion requirements. |
| `common.reshape` | Reuse existing native contract | Main common/com | Reuse `OPR_DSLRESHAPE`, version 1; FHE supplies packing/layout legality requirements. |
| `common.window_reduce` | Defer native promotion | Main common/com | Registry vocabulary exists; first-slice pooling remains native CNN operators through gatekeeping. |
| `common.residual_add` | Reuse and semantically strengthen existing native contract | Main common/com with CNN/FHE requirements | Reuse `OPR_DSLRESIDUALADD`, version 2; FHE supplies residual scale/level/layout alignment obligations. |
| `cnn.conv2d` | Reuse existing native contract | Main/common CNN owner | Reuse `OPR_DSLCONV2D`, version 2; FHE adapts reviewed CNN conv semantics to encrypted conv planning. |
| `cnn.batch_norm_infer` | Reuse existing native contract | Main/common CNN owner | Reuse `OPR_DSLBATCHNORMINFER`, version 2; FHE folds into plaintext weights/bias when legal. |
| `cnn.max_pool2d` | Reuse existing native contract | Main/common CNN owner | Reuse `OPR_DSLMAXPOOL2D`, version 2; FHE rejects or replaces only through explicit policy. |
| `cnn.global_avg_pool2d` | Reuse existing native contract | Main/common CNN owner | Reuse `OPR_DSLGLOBALAVGPOOL2D`, version 2; FHE lowers linear pooling through sum/scale sequence. |
| `fhe.entry_contract` | New FHE-owned contract using reviewed common attachment hooks | FHE task after SYNC-1 | Main owns generic attachment/image/printer hooks; FHE owns FHE semantics and verifier. |
| `fhe.encryption_descriptor` | New FHE-owned descriptor using reviewed TensorDescriptorIR attachment | FHE task after SYNC-1 | Main owns type/descriptor attachment mechanics; FHE owns value-class, scheme, key, and CKKS-state meaning. |
| `fhe.cnn.conv2d` | New FHE domain wrapper | FHE task after SYNC-3 | Created only after CNN/FHE gatekeeper accepts source `cnn.conv2d`. |
| `fhe.cnn.poly_activation` | New FHE domain wrapper | FHE task after SYNC-4 | Records approximation of existing `common.relu`; bootstrap remains refresh, not ReLU semantics. |
| `fhe.cnn.residual_add` | New FHE domain wrapper | FHE task after SYNC-3 | Created only from accepted `common.residual_add` plus FHE alignment obligations. |
| `sihe.*` arithmetic/encode/bootstrap | New FHE internal scheme-independent layer, ACE-aligned | FHE task after SYNC-3/SYNC-4 | Not common source semantics; allocation waits for reviewed FHE domain infrastructure. |
| `sihe.*_msg` validation calls | New optional validation-only layer, ACE-aligned | FHE task after validation design review | Runtime-validation artifacts only; not normal ingestion semantics. |
| `ckks.*` | New CKKS-specific lowering/planning layer, ACE-aligned | FHE task after SYNC-4 | Introduced only after CKKS state planning contracts are frozen. |
| `poly.*` / HPOLY/RNS | New optional later GPU/native-lowering layer | FHE task after SYNC-8 | Out of first OpenFHE CPU/reference milestone. |
| `dsc_fhe_*` runtime calls | New runtime ABI, not ingestion opcodes | FHE runtime task after SYNC-5 | Visible only after lowering to standard `OPR_CALL`. |

| Proposed stable name | Ownership | Version | Operands/results | Attributes | TensorDescriptorIR requirements | Effects | Verifier obligations | Lowering ownership | Compatibility impact |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| `common.model_input` | Common | v1 | Result tensor; optional source/boundary symbol | `name`, `index`, `boundary_role` | dtype, rank, shape, semantic role, lineage; may carry FHE traits via descriptor | Reads external input handle | Complete boundary descriptor; no secret-key material | Common lowers to ABI-specific input materialization; FHE wraps encrypted boundary | Stable common input marker; no FHE-only type encoding. |
| `common.model_output` | Common | v1 | Input tensor result value | `name`, `index`, `boundary_role` | dtype, rank, shape, semantic role, lineage; FHE output trait allowed | Writes external result handle | Output contract matches function boundary | Common lowers to return/store/ABI result; FHE wraps encrypted output | Stable common output marker. |
| `common.output_logits` | Common | v1 | Input tensor -> logits tensor or model output | `axis`, optional `class_count` | rank/shape and semantic role `logits` | Pure | Shape/classifier contract | Common or domain lowering after verification | Reusable across CNN/Transformer; FHE only adds encrypted output policy. |
| `common.add` | Common | existing/v1 expected | Two tensor/scalar operands -> result | `broadcast_rule`, optional `dtype_policy` | compatible dtype/shape/layout or explicit conversion; encryption trait propagated by FHE verifier | Pure | Type, shape, broadcast, encrypted/plaintext legality when FHE trait present | Common owns generic lowering; FHE owns HE add call selection | No new common semantics if existing contract suffices. |
| `common.bias_add` | Common | v1 | Activation tensor, bias tensor -> result | `axis`, `broadcast_rule`, `dtype_policy` | activation/bias shape compatibility; plaintext weight trait may apply | Pure | Bias shape and encryption value-class compatibility | Common generic; FHE lowers ciphertext+encoded plaintext add | Needed if not already accepted as common substrate. |
| `common.mul` | Common | v1 | Two tensor/scalar operands -> result | `broadcast_rule`, `dtype_policy` | compatible dtype/shape/layout; FHE tracks multiplicative depth | Pure | Type/shape plus FHE ciphertext/plaintext legality | Common generic; FHE lowers HE multiply/plain multiply | Reusable common numeric op; FHE adds scale/depth obligations. |
| `common.relu` / `OPR_DSLRELU` | Common | existing registry contract; reuse/strengthen | Input tensor -> result tensor | Existing ReLU attributes plus optional `approximation_contract_id` only after FHE conversion | dtype, rank, shape, lineage, source position; encrypted descriptor propagated by FHE verifier | Pure source semantics | Main task confirms existing contract is sufficient or publishes a compatible semantic strengthening; FHE requires approved approximation and first-release bootstrap policy for encrypted CKKS values | Common owns existing source operator contract; FHE owns bootstrap boundary and polynomial lowering | Reuse current `OPR_DSLRELU` and logical `common.relu`. Do not allocate a second ReLU operator or replacement enum value. |
| `common.linear` | Common | v1 | Input, weight, optional bias -> result | `transpose_weight`, `bias_axis`, `layout_contract` | input/weight/result shapes, semantic role, lineage, plaintext weight trait | Pure | Affine shape contract; encrypted input/plaintext weight legality | Common generic lowering; FHE MVM/MetaKernel lowering | Already identified common op; FHE requires descriptor hooks. |
| `common.flatten` | Common | v1 | Input tensor -> result | `start_dim`, `end_dim` | shape formula and lineage | Pure | Shape rewrite legality; no data-dependent change | Common generic; FHE may reinterpret layout or request packing conversion | Reusable common shape op. |
| `common.reshape` | Common | v1 | Input tensor -> result | `shape_formula`, `allow_copy` | shape formula, layout compatibility | Pure or representation conversion | Element-count compatibility; FHE packing/layout legality | Common generic; FHE owns encrypted layout conversion if needed | Reusable common shape op. |
| `common.window_reduce` | Common | v1 | Input tensor -> result | `window`, `stride`, `padding`, `reduction`, `dtype_policy` | shape/layout; semantic role | Pure | Reduction legality; FHE rejects unsupported max/min for CKKS | Common generic; FHE lowers average/sum policy | Common op must not hide CNN pooling contract before gatekeeper. |
| `common.residual_add` | Common plus domain wrapper | v1 | Main tensor, skip tensor -> result | `broadcast_rule=none`, `residual_policy` | equal/compatible shape, lineage group, layout | Pure | Residual lineage, shape, dtype, layout; FHE scale/level alignment | Common generic; FHE owns encrypted residual alignment and add | Must preserve domain residual metadata through common promotion. |
| `cnn.conv2d` | CNN domain | v1 | Input, weight, optional bias -> result | `kernel_shape`, `stride`, `padding`, `dilation`, `groups`, `data_layout`, `weight_layout` | feature-map/weight/result descriptors; plaintext weight trait allowed | Pure | CNN shape/layout/group contract before FHE adaptation | CNN/common lower after verification; FHE adapts to encrypted conv/MetaKernel | Domain-visible until gatekeeper; not a common FHE allocation. |
| `cnn.batch_norm_infer` | CNN domain | v1 | Input, scale, bias, mean, variance -> result | `epsilon`, `channel_axis`, `training_mode=false` | channel shape and parameter descriptors | Pure | Inference-only and foldability | CNN/FHE adaptation folds into conv when legal | Reject or fold before FHE lowering. |
| `cnn.max_pool2d` | CNN domain | v1 | Input -> result | `kernel`, `stride`, `padding`, `ceil_mode` | feature-map shape/layout | Pure | FHE policy must explicitly replace or reject | CNN/FHE adaptation owns replacement/rejection | Must remain visible for policy diagnostics. |
| `cnn.global_avg_pool2d` | CNN domain or common wrapper | v1 | Input -> result | `data_layout`, `keepdims` | feature-map shape/layout | Pure | Shape/layout and FHE average legality | FHE lowers to HE sum/scale sequence | Can lower through `common.window_reduce` after CNN gatekeeper. |
| `fhe.entry_contract` | FHE domain contract | v1 | Function/module-level contract, no value result | `scheme`, `encrypted_inputs`, `encrypted_outputs`, `parameter_policy`, `security_level`, `accuracy_budget`, `backend_policy` | References boundary tensor descriptors | Contract only | Complete FHE policy; no secret key | FHE gatekeeper consumes; lowering emits runtime manifest | Should use DSL contract/metadata path, not common opcode enum. |
| `fhe.encryption_descriptor` | FHE domain descriptor | v1 | Attached to tensor value/type descriptor | `value_class`, `scheme`, `scale`, `level`, `slot_count`, `packing_layout_id`, `key_set_id`, `boundary_role` | Extends representation/domain view; not type equivalence core | Descriptor only | Required fields by stage; redact key/ciphertext internals | FHE gatekeeper/planner consumes | Coordinate storage with common TensorDescriptorIR; avoid new type kind. |
| `fhe.cnn.conv2d` | FHE domain wrapper | v1 | Encrypted input, encoded plaintext weight, optional bias -> encrypted result | `source_op`, `packing_policy`, `metakernel_plan_id` | encrypted input/result; encoded plaintext weight; layout plan | Pure, consumes runtime key handles at lowering | Scheme legality, depth, rotations, key requirements | FHE lowers to SIHE/CKKS/runtime calls | Use domain-wrapper registry when delegating to accepted `cnn.conv2d`; no new `DSL_OPERATOR` enum solely for wrapper identity. |
| `fhe.cnn.poly_activation` | FHE domain wrapper | v1 | Encrypted input -> encrypted result | `source_activation=common.relu`, `polynomial_id`, `degree`, `approx_error_budget`, `refresh_boundary_id` | encrypted tensor, scale/depth state before and after bootstrap and polynomial evaluation | Pure activation approximation; depends on prior refresh boundary | Explicit approximation policy, accuracy budget, bootstrap policy proof, and source evidence through existing WN/ST/DST/value records | FHE lowers to bootstrap plus HE polynomial multiply/add chain at `-O0`; backend may fuse only with preserved evidence | Domain wrapper records approximation of `common.relu`; it is not the source ReLU semantic operator and does not duplicate source-position fields. |
| `fhe.cnn.residual_add` | FHE domain wrapper | v1 | Encrypted main, encrypted skip -> encrypted result | `source_residual_id`, `alignment_policy` | scale/level/layout alignment group | Pure | Branch compatibility, rescale/bootstrap obligations | FHE lowers to alignment plus HE add | Use domain-wrapper registry when delegating to accepted `common.residual_add`; no new `DSL_OPERATOR` enum solely for wrapper identity. |
| `sihe.add` | FHE scheme-independent domain | v1 | Cipher/plain operands -> cipher/plain result | `value_class_policy` | encryption descriptors complete | Pure runtime op after lowering | HE value-class legality | FHE lowering to CKKS/runtime | Not common; may be an internal FHE logical layer. |
| `sihe.sub` | FHE scheme-independent domain | v1 | Cipher/plain operands -> cipher/plain result | `value_class_policy` | encryption descriptors complete | Pure runtime op after lowering | HE value-class legality | FHE lowering to CKKS/runtime | Aligns with ACE `SIHE::sub`; not common. |
| `sihe.mul` | FHE scheme-independent domain | v1 | Cipher/plain operands -> cipher/plain result | `value_class_policy` | encryption descriptors, depth state | Pure runtime op after lowering | HE multiplication legality and depth accounting | FHE lowering to CKKS/runtime | Not common. |
| `sihe.neg` | FHE scheme-independent domain | v1 | Cipher operand -> cipher result | `value_class_policy` | encryption descriptor complete | Pure runtime op after lowering | HE value-class legality | FHE lowering to CKKS/runtime | Aligns with ACE `SIHE::neg`; not common. |
| `sihe.rotate` | FHE scheme-independent domain | v1 | Cipher operand -> cipher result | `rotation_steps`, `key_requirement` | packing layout and key-set handle | Pure runtime op after lowering | Rotation supported by key manifest/backend | FHE lowering to CKKS/runtime | Not common. |
| `sihe.encode` | FHE scheme-independent domain | v1 | Clear vector/constant, encoding policy -> plaintext result | `encoding_layout`, `scale_policy`, `level_policy` | plaintext/encoded descriptor, shape, slot-count policy | Pure runtime op after lowering | Encoding policy is explicit and backend-supported | FHE lowering to CKKS/runtime | Aligns with ACE `SIHE::encode`; not common. |
| `sihe.bootstrap` | FHE scheme-independent domain | v1 | Cipher operand -> cipher result | `bootstrap_policy`, `reason`, optional `source_activation_id` | encrypted descriptor, target level/scale obligations, key requirement | Pure runtime op after lowering, high cost | Backend and key support; for pre-ReLU boundaries, prove this refresh precedes the approved polynomial approximation | FHE lowering to CKKS/runtime | ACE includes SIHE bootstrap; first CKKS path uses it as mandatory pre-`common.relu` refresh, not as ReLU computation. |
| `sihe.*_msg` validation calls | FHE scheme-independent validation layer | v1 | Cipher/plain operands according to operation | `validation_policy` | test/runtime validation descriptors | Runtime-validation effect | Only emitted in validation builds or explicit runtime-validation paths | FHE validation lowering | Aligns with ACE `rotate_msg`, `add_msg`, `mul_msg`, `relu_msg`, and `bootstrap_msg`; not part of normal semantic ingestion. |
| `ckks.add` | FHE CKKS domain | v1 | Cipher/plain/float operands -> cipher/plain result | `value_class_policy` | CKKS scale/level state | Pure runtime op after lowering | Scale/level/value-class legality | FHE CKKS planner/runtime lowering | Aligns with ACE `CKKS::add`; CKKS-specific. |
| `ckks.sub` | FHE CKKS domain | v1 | Cipher/plain/float operands -> cipher/plain result | `value_class_policy` | CKKS scale/level state | Pure runtime op after lowering | Scale/level/value-class legality | FHE CKKS planner/runtime lowering | Aligns with ACE `CKKS::sub`; CKKS-specific. |
| `ckks.mul` | FHE CKKS domain | v1 | Cipher/plain/float operands -> cipher or widened cipher result | `value_class_policy`, `relinearize_policy`, `rescale_policy` | CKKS scale/level/depth state | Pure runtime op after lowering | Multiplication legality, depth, relinearization/rescale obligations | FHE CKKS planner/runtime lowering | Aligns with ACE `CKKS::mul`; CKKS-specific. |
| `ckks.neg` | FHE CKKS domain | v1 | Cipher operand -> cipher result | none or `value_class_policy` | CKKS scale/level state | Pure runtime op after lowering | Value-class legality | FHE CKKS planner/runtime lowering | Aligns with ACE `CKKS::neg`; CKKS-specific. |
| `ckks.rotate` | FHE CKKS domain | v1 | Cipher operand, rotation index -> cipher result | `rotation_steps`, `key_requirement` | CKKS packing layout and key-set handle | Pure runtime op after lowering | Rotation key/backend support | FHE CKKS planner/runtime lowering | Aligns with ACE `CKKS::rotate`; CKKS-specific. |
| `ckks.encode` | FHE CKKS domain | v1 | Clear vector, length, scale degree, level -> plaintext result | `encoding_layout`, `scale_degree`, `level` | encoded plaintext descriptor | Pure runtime op after lowering | Encoding parameter legality | FHE CKKS planner/runtime lowering | Aligns with ACE `CKKS::encode`; CKKS-specific. |
| `ckks.rescale` | FHE CKKS domain | v1 | Cipher -> cipher | `target_scale`, `modulus_policy` | CKKS scale/level state | Pure runtime op after lowering | Scale/level legality | FHE CKKS planner/runtime lowering | CKKS-specific; keep out of common. |
| `ckks.upscale` | FHE CKKS domain | v1 | Cipher, scale bits -> cipher | `scale_bits` | CKKS scale/level state | Pure runtime op after lowering | Target scale is legal | FHE CKKS planner/runtime lowering | Aligns with ACE `CKKS::upscale`; CKKS-specific. |
| `ckks.modswitch` | FHE CKKS domain | v1 | Cipher -> cipher | `modulus_policy` | CKKS level/modulus state | Pure runtime op after lowering | Modulus-chain legality | FHE CKKS planner/runtime lowering | Aligns with ACE `CKKS::modswitch`; CKKS-specific. |
| `ckks.relin` | FHE CKKS domain | v1 | Widened cipher/CIPHER3 -> cipher | `key_requirement` | key-set handle, level state | Pure runtime op after lowering | Relinearization key availability | FHE CKKS planner/runtime lowering | Use ACE spelling `relin` for lower-layer alignment; source docs may mention relinearization. |
| `ckks.bootstrap` | FHE CKKS domain | v1 | Cipher -> cipher | `bootstrap_plan_id`, `target_level`, `reason`, optional `source_activation_id` | bootstrap plan, key-set, scale/level before and after refresh | Pure runtime op after lowering, high cost | Backend and key support; security policy; for optimized movement/fusion, prove legality, numerical equivalence, scale/level validity, and provenance | FHE CKKS planner/runtime lowering | CKKS-specific refresh. Backend fusion with polynomial activation must preserve inspectable `common.relu` evidence. |
| `ckks.scale` | FHE CKKS domain | v1 | Cipher -> integer/metadata value | none | CKKS scale state | Pure query | Descriptor/runtime state is available | FHE CKKS analysis/runtime lowering | Aligns with ACE `CKKS::scale`; query op, not common arithmetic. |
| `ckks.level` | FHE CKKS domain | v1 | Cipher -> integer/metadata value | none | CKKS level state | Pure query | Descriptor/runtime state is available | FHE CKKS analysis/runtime lowering | Aligns with ACE `CKKS::level`; query op. |
| `ckks.batch_size` | FHE CKKS domain | v1 | Cipher -> integer/metadata value | none | slot-count/batch-size state | Pure query | Descriptor/runtime state is available | FHE CKKS analysis/runtime lowering | Aligns with ACE `CKKS::batch_size`; query op. |
| `ckks.raise_mod` | FHE CKKS domain | v1 | Cipher, target level -> cipher | `target_level` | CKKS level/modulus state | Pure runtime op after lowering | Raise-mod supported by plan/backend | FHE CKKS planner/runtime lowering | Aligns with ACE `CKKS::raise_mod`; CKKS-specific. |
| `ckks.mul_mono` | FHE CKKS domain | v1 | Cipher, monomial power -> cipher | `power` | CKKS polynomial/packing state | Pure runtime op after lowering | Monomial multiply legality | FHE CKKS planner/runtime lowering | Aligns with ACE `CKKS::mul_mono`; CKKS-specific. |
| `ckks.conjugate` | FHE CKKS domain | v1 | Cipher -> cipher | `key_requirement` | CKKS packing/key state | Pure runtime op after lowering | Conjugation key/backend support | FHE CKKS planner/runtime lowering | Aligns with ACE `CKKS::conjugate`; CKKS-specific. |
| `ckks.free` | FHE CKKS domain statement | v1 | Cipher handle/value | `lifetime_policy` | lowered runtime lifetime state | Runtime lifetime effect | Only after lifetime planning; no source-visible semantic effect | FHE CKKS/runtime lowering | Aligns with ACE `CKKS::free`; should appear only after lowering/lifetime insertion. |
| `poly.*` lower-layer operations | Optional FHE POLY/RNS domain | v1 | Polynomial/RNS operands/results | operation-specific | polynomial/RNS descriptor state | Pure or runtime/hardware effect by op | Only after CKKS-to-POLY lowering is selected | Native GPU/POLY lowering owner | Align with ACE `POLY` for NTT/INTT, mod up/down, decomp, key-switching, hardware modular ops, fused ops, and allocation/init. |
| `dsc_fhe_*` runtime calls | Runtime ABI, not ingestion opcode | ABI v1 | Opaque handles and descriptor pointers | C ABI operation descriptors | N/A after lowering | Runtime effects through opaque handles | Call signatures, error policy, no secret leakage | FHE runtime owner | Visible only after FHE lowering to standard `OPR_CALL`. |

## ACE Compiler Operator Alignment

ACE provides concrete registered operator domains for `NN`, `VECTOR`, `SIHE`,
`CKKS`, and `POLY`. Open64 should use ACE as a comparison point for lower FHE
vocabulary while preserving Open64's stronger frontend/gatekeeper contract
layer.

| Open64 plan layer | Open64 planned names | ACE names | Alignment decision |
| --- | --- | --- | --- |
| Boundary and FHE contracts | `fhe.entry_contract`, `fhe.encryption_descriptor` | No direct opcode equivalent identified | Keep Open64-specific. These are gatekeeper/planner contracts, not ACE-style arithmetic operators. |
| High-level tensor/CNN source semantics | `common.add`, `common.mul`, `common.relu`, `common.flatten`, `common.reshape`, `common.linear`, `common.window_reduce`, `common.residual_add`, `cnn.conv2d`, `cnn.max_pool2d`, `cnn.global_avg_pool2d` | ACE `NN`: `add`, `mul`, `flatten`, `reshape`, `gemm`, `average_pool`, `global_average_pool`, `max_pool`, `conv`, `relu` | Semantically close. Open64 should keep common-vs-domain ownership split; ACE names help validate coverage. ReLU belongs here as shared source semantics, not as an FHE-domain opcode. |
| FHE-CNN wrappers after adaptation | `fhe.cnn.conv2d`, `fhe.cnn.poly_activation`, `fhe.cnn.residual_add` | No direct named ACE equivalent identified; ACE lowers NN/vector toward FHE layers | Keep Open64-specific wrappers so CNN legality, approximation, residual alignment, and FHE policy remain visible before SIHE/CKKS lowering. |
| Encrypted vector/layout planning | Descriptor/plans plus possible later vector ops | ACE `VECTOR`: `add`, `mul`, `roll`, `slice`, `pad`, `reshape`, `read`, `roll_sum`, runtime-validation and reference ops | Open64 currently models most of this as descriptors/plans. If this becomes an operator layer, ACE `VECTOR` is the preferred naming reference. |
| Scheme-independent HE | `sihe.add`, `sihe.sub`, `sihe.mul`, `sihe.neg`, `sihe.rotate`, `sihe.encode`, `sihe.bootstrap`, `sihe.*_msg` validation ops | ACE `SIHE`: same core set plus `rotate_msg`, `add_msg`, `mul_msg`, `relu_msg`, `bootstrap_msg` | Align closely with ACE. Validation-message ops should remain optional validation/runtime-check artifacts, not normal ingestion semantics. |
| CKKS | `ckks.add`, `ckks.sub`, `ckks.mul`, `ckks.neg`, `ckks.rotate`, `ckks.encode`, `ckks.rescale`, `ckks.upscale`, `ckks.modswitch`, `ckks.relin`, `ckks.bootstrap`, `ckks.scale`, `ckks.level`, `ckks.batch_size`, `ckks.raise_mod`, `ckks.mul_mono`, `ckks.conjugate`, `ckks.free` | ACE `CKKS`: same list | Align closely with ACE. Prefer lower-layer spelling `ckks.relin`; documentation may still describe the semantic action as relinearization. |
| Optional POLY/RNS native path | `poly.*` lower-layer operations | ACE `POLY`: `alloc`, `free`, `init_ciph_*`, `ntt`, `intt`, `add`, `sub`, `mul`, `add_ext`, `sub_ext`, `mul_ext`, `rescale`, `rotate`, `decomp`, `decomp_modup`, `mod_up`, `mod_down`, `coeffs`, `set_coeffs`, `swk`, `pk*_at`, `hw_*`, fused ops, `mac`, `bconv`, `bswitch`, `raise_mod`, `alloc_n` | Do not include in MVP. Use ACE `POLY` as naming and phase-boundary reference when native GPU/POLY lowering is reviewed. |

## Open Questions

1. Which shared tensor trait mechanism should carry `EncryptedValue`,
   `PlaintextEncodedValue`, and `ClearMetadata` before the fixed descriptor
   table is finalized?
2. Should `EncryptionDescriptorIR` be represented as a FHE-owned descriptor
   referenced from TensorDescriptorIR, or as a general representation
   descriptor slot with FHE payload ownership?
3. How much of `common.window_reduce` should be accepted before CNN pooling
   contracts are stable?
4. What is the exact diagnostic-code registry policy for new `CFHE-*`
   families?
5. Where should retained FHE review artifacts live so Docker and local runs use
   the same host-visible directory convention?

## Immediate Next Actions

1. Main shared-contract owner reviews the handoff table and publishes accepted
   common/type contracts.
2. FHE frontend owner captures the complete deterministic ResNet-20/CIFAR-10
   graph and publishes its source/operator census before adding lowering
   behavior.
3. FHE owner creates focused add, linear, convolution, residual-add, and
   `common.relu` certification fixtures for diagnostics and contract evidence.
4. FHE owner drafts the first gatekeeper diagnostic list and negative-test
   matrix.
5. Runtime owner drafts `libdsc_fhe_cabi` ABI v1 around mock opaque handles.
6. Build owner confirms the `whirl2c` and final C++/CUDA-aware link path for
   generated C plus backend libraries.
