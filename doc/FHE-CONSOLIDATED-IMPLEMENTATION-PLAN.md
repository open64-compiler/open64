# Open64 FHE Consolidated Implementation Plan

Status: cross-task implementation and synchronization plan  
Primary model target: inference-only ResNet-20/CIFAR-10  
Baseline optimization level: `-O0` correctness pipeline  

This document coordinates two Open64 workstreams:

- **Main task**: shared WHIRL and common/com infrastructure.
- **Open64 FHE support task**: FHE semantics, ingestion, conversion, CKKS
  planning, runtime integration, and FHE-specific validation.

It consolidates the architecture in
`DSC_FHE_Compiler_Architecture_and_Integration_Plan_v0.10.docx`, the main-task
implementation detail in `FHE-DSL-INTEGRATION-PLAN.md`, and the FHE-task plan
in `FHE-WHIRL-INTEGRATION-PLAN.md`.

The repository document
`doc/DSC_FHE_Compiler_Architecture_and_Integration_Plan_v0.10.docx`
is the sole highest FHE semantic authority. The reviewed copy has SHA-256
`0018769C26B5A0BCD1BDFCBD85AA97B8BAFEA381D7640FBB9E2E81B0022013D9`.
This Markdown tracker may narrow architecture milestones into review
checkpoints, but it may not override v0.10 semantics, boundaries, or completion
criteria. In particular, focused C3 / SYNC-3 completion was not completion of
v0.10 Architecture Phase 3 or focused milestone M4. C4 / SYNC-4 is now
implementation-complete and locally certified; standard-WHIRL lowering and
runtime execution remain governed by SYNC-5 and SYNC-6.

The project-approved ACE override selects `FHErt_ant` for the first executable
provider and changes only provider selection and provider API mapping. It does
not override the v0.10 privacy boundary, threat model, evidence requirements,
stable C ABI boundary, or milestone exits. No downstream runtime decision or
handoff document may redefine those authorities.

## Shared Objective

The first end-to-end target is the complete ResNet-20/CIFAR-10 inference model:

```text
secure_resnet20.py
  -> torch2whirl capture
  -> binary very-high-level WHIRL
  -> FHE gatekeeper and model adaptation
  -> SIHE/CKKS correctness planning
  -> standard middle-WHIRL runtime calls
  -> whirl2c generated C
  -> C compilation and ACE ANT runtime provider link
  -> server executable with public context and evaluation-key import
  -> encrypted input -> encrypted logits -> client validation
```

Small add, linear, convolution, residual-add, and ReLU fixtures are mandatory
diagnostic tests. They are not earlier model milestones and must not delay
capture or planning of the complete ResNet-20 graph.

## Frozen Architectural Decisions

1. Python is a source frontend. Python-facing values remain opaque native
   handles and Python never inspects WN, TY, ST, mapped-image, or physical
   `OPR_DSL` representation.
2. Binary WHIRL is the frontend/process boundary. Existing mapped-image and ELF
   compatibility rules remain authoritative.
3. Common operators and tensor types are shared across neural-network domains.
   Domain contracts preserve additional CNN and FHE legality dimensions.
4. `common.relu` is the shared source-semantic ReLU operator. It must not be
   duplicated as a source-level FHE ReLU opcode.
5. The current tree already registers `OPR_DSLRELU` and logical `common.relu`
   contracts. The main task must review and reuse those published contracts,
   not allocate a replacement enum value.
6. For the first CKKS path, every surviving `common.relu` is a mandatory
   refresh boundary at `-O0`.
7. `bootstrap=auto` and `bootstrap=on` insert bootstrap immediately before the
   approved polynomial ReLU approximation. `bootstrap=manual` requires an
   explicit boundary. `bootstrap=off` rejects a surviving ReLU.
8. Bootstrap restores CKKS ciphertext capacity; it does not itself compute
   ReLU.
9. A backend may fuse bootstrap and polynomial activation, but the logical
   `common.relu`, approximation contract, bootstrap reason, source position,
   and resulting CKKS state remain inspectable.
10. At `-O1+`, moving, merging, deduplicating, or fusing baseline ReLU refresh
    boundaries requires semantic, numerical, scale/level, and provenance proof
    against the `-O0` result.
11. No FHE, SIHE, CKKS, HPOLY, or private DSL node may reach unmodified
    `whirl2c`. The lowering gate must produce standard WHIRL calls, symbols,
    initializers, and control flow.
12. The client or provisioner owns and retains the secret key. The server
    receives only a versioned public context, the evaluation, relinearization,
    rotation, and bootstrap material required by the certified plan, and
    ciphertext input. No server binary, runtime state, generated C object, or
    runtime API may contain, generate, decrypt with, or require a secret key.
    SYNC-6 and the first complete vertical slice require executable
    server-without-secret-key evidence. An embedded/local ACE harness may create
    ephemeral test keys and decrypt only as separate bring-up and numerical
    diagnostic evidence; it cannot close SYNC-6 or replace the client/server
    security gate.
13. Preserving callee-specific data-value metadata is a first-order correctness
    requirement. Every caller actual must remain structurally joined through
    call ABI and PU-interface identity to the exact callee formal
    `DSL_IR_VALUE_ID`, operator operand, and result value.
14. XLA-style shape propagation is a generic DSL/common analysis over Open64
    value identities. It certifies tensor geometry before FHE planning; shape
    equality does not replace value identity or provenance.
15. FHE encryption-state propagation runs after shape certification and model
    adaptation. It attaches value-specific CKKS layout, level, scale,
    precision, alignment, rotation, and pending-action facts without mutating
    canonical TensorDescriptorIR/TY identity.
16. Runtime-interface projection is a separate lowering step after successful
    shape and FHE-state propagation. It relates each unchanged canonical DSL
    value to an owner-local exact opaque handle TY/ST, including context-safe
    PU formals, caller actuals, hidden results, and call results. It must never
    retype canonical tensor identity into `void *` or a runtime handle.

The detailed algorithm and validation contract for decisions 13-16 is
`doc/FHE-SHAPE-AND-ENCRYPTION-STATE-PROPAGATION.md`.

The selected first executable provider and its API mapping are defined in
`doc/FHE-ACE-RTLIB-RUNTIME-DECISION.md`. The first provider is pinned ACE
`FHErt_ant`; a direct OpenFHE provider is optional later work and is no longer a
SYNC-6 exit requirement. The runtime decision is subordinate to v0.10 for the
privacy boundary, threat model, evidence, ABI boundary, and milestone exits.

The team-facing commit, test, artifact, and kickoff procedure for SYNC-4
through SYNC-6 is `doc/FHE-SYNC4-TO-SYNC6-TEAM-HANDOFF.md`.

## Workstream Ownership

| Area | Main task owns | FHE support task owns |
| --- | --- | --- |
| Common operators | Existing and new common-substrate contracts, versions, traits, effects, and stable names | FHE use and legality rules; requests changes through a handoff table |
| Tensor type system | TensorDescriptorIR identity, interning, representation attachment hooks, source positions, and generic compatibility | EncryptionDescriptorIR semantics, CKKS state meaning, and FHE compatibility rules |
| WHIRL node abstraction | Logical DSL operator APIs, private `OPR_DSL` boundary, builder primitives, and generic result ownership | FHE-specific builder wrappers only when they add validation or descriptor propagation |
| Binary image | Shared ELF identifiers, reader/writer hooks, bounds conventions, compatibility matrix, and generic printer integration | Fixed FHE record definitions, FHE table validation, and FHE-specific mapped-image tests after review |
| Gatekeeper | Generic invocation hooks and common/tensor checks | FHE entry, encryption, approximation, bootstrap, security, and CKKS legality checks |
| Frontend | Opaque native capability and builder interfaces | Python FHE surface, ResNet capture, operator census, diagnostics, and frontend tests |
| Driver | Phase insertion points, option propagation convention, `-keep`, artifact naming, and process boundaries | `-dsc-fhe-*` option semantics and FHE phase implementations |
| Lowering | Standard WHIRL call/formal/result construction and unlowered-node protection | CNN-to-FHE, SIHE-to-CKKS, runtime-call lowering, and conversion reports |
| Runtime | Generic driver link integration and provider manifest consumption | Stable FHE C ABI, mock provider, ACE `FHErt_ant` adapter, client/server key boundary, and runtime tests |
| Optimization | Shared WOPT/VHO extension rules and controls | FHE graph optimization, ReSBM, HPOLY/HPAO, and later GPU-specific planning |

Ownership is semantic, not merely directory-based. Before either task edits a
file that the other task may also need, the current synchronization checkpoint
must name one owner for that file and one PR. Concurrent edits to the same
shared integration file are prohibited.

## Shared File Coordination

| File family | Default editor | Coordination rule |
| --- | --- | --- |
| `osprey/common/com/dsl_opcode.*` | Main task | FHE task submits contract requests; main allocates or reuses stable identities |
| `osprey/common/com/dsl_builder.*` | Main task | Add generic hooks here; place reviewed FHE-specific behavior in focused modules |
| Tensor descriptor and symbol/type files | Main task | FHE task supplies semantic fields and verifier requirements before edits |
| `ir_bread.cxx`, `ir_bwrite.cxx`, `ir_reader.cxx`, `elf_whirl.h` | Main task | One compatibility PR owns section wiring and legacy-image tests |
| New `dsl_fhe.*` records and verifier modules | FHE task after SYNC-1 | Main reviews fixed layout and integration boundaries before implementation |
| `osprey/torch2whirl` FHE surface and tests | FHE task | Depends only on merged opaque native APIs; no physical WHIRL workarounds |
| `config_fhe.*`, VHO FHE passes, runtime, ACE ANT provider adapter | FHE task | Main reviews driver/pass placement and standard-WHIRL boundary at checkpoints |
| Existing driver and `whirl2c` integration files | Assigned at SYNC-5 | Only one workstream edits each shared file in a coordinated PR batch |

## SYNC-0 Main Accepted Contract Matrix

This matrix is source-verified against `origin/develop` at `1f87418b`. It
replaces provisional phrases such as "if existing" and closes main-task
ownership classification for the first ResNet-20 slice.

| Contract | Main decision | Existing native identity | First-slice rule |
| --- | --- | --- | --- |
| `common.model_input` | Reuse | `OPR_DSLMODELINPUT`, `common.model_input.v2` | Attach FHE entry/encryption descriptors without changing the source operator. |
| `common.model_output` | Defer native promotion | Registry vocabulary exists; no native `DSL_OPERATOR` is required by ResNet-20 | Use `common.output_logits` for the first classifier result. Revisit only after a captured model requires generic model output. |
| `common.output_logits` | Reuse | `OPR_DSLOUTPUTLOGITS`, versions 2 and 3 | ResNet uses the classifier-oriented version 2 contract; FHE adds encrypted-output policy. |
| `common.add` | Reuse | `OPR_DSLADD`, version 1 | FHE supplies ciphertext/plaintext legality and scale/level obligations. |
| `common.bias_add` | Defer native promotion | Registry vocabulary exists | ResNet conv/linear operands and BatchNorm folding carry first-slice bias semantics. Promote only from a later captured requirement. |
| `common.mul` | Reuse | `OPR_DSLMUL`, version 1 | FHE supplies multiplicative-depth and value-class rules. |
| `common.relu` | Reuse and semantically strengthen | `OPR_DSLRELU`, version 2 | No new ReLU enum. FHE attaches approximation/refresh provenance and owns bootstrap-plus-polynomial lowering. |
| `common.linear` | Reuse | `OPR_DSLLINEAR`, versions 2 and 3 | Accept the existing three-kid and two-kid forms; FHE adds plaintext-weight and encrypted-MVM rules. |
| `common.flatten` | Reuse | `OPR_DSLFLATTEN`, version 2 | FHE validates encrypted-layout reinterpretation or conversion. |
| `common.reshape` | Reuse | `OPR_DSLRESHAPE`, version 1 | FHE validates packing/layout legality. |
| `common.window_reduce` | Defer native promotion | Registry vocabulary exists | Use native `cnn.max_pool2d.v2` and `cnn.global_avg_pool2d.v2` until CNN gatekeeping completes. |
| `common.residual_add` | Reuse and semantically strengthen | `OPR_DSLRESIDUALADD`, version 2 | Preserve residual path/lineage; FHE adds layout, scale, and level alignment. |
| `cnn.conv2d` | Reuse | `OPR_DSLCONV2D`, version 2 | Remains CNN-visible through CNN and FHE gatekeepers. |
| `cnn.batch_norm_infer` | Reuse | `OPR_DSLBATCHNORMINFER`, version 2 | Fold into plaintext convolution parameters when legal and preserve provenance. |
| `cnn.max_pool2d` | Reuse | `OPR_DSLMAXPOOL2D`, version 2 | Explicitly replace or reject under the FHE pooling policy. |
| `cnn.global_avg_pool2d` | Reuse | `OPR_DSLGLOBALAVGPOOL2D`, version 2 | Lower to an HE-compatible sum/scale plan after gatekeeping. |
| `fhe.entry_contract` and `fhe.encryption_descriptor` | New FHE-owned records | None | Use reviewed generic attachment and mapped-image hooks; do not create a new WHIRL type kind. |
| `fhe.cnn.*` | New FHE-domain wrappers | None | Allocate only after SYNC-3 conversion contracts are accepted. |
| `sihe.*` and `ckks.*` | New FHE internal lowering layers | None | Allocate append-only after SYNC-1/SYNC-4 state and verifier contracts are frozen. |
| `poly.*` / HPOLY/RNS | Deferred new lower layer | None | Requires the separate SYNC-8 GPU/native-lowering review. |
| `dsc_fhe_*` calls | New runtime ABI, not DSL opcodes | Standard `OPR_CALL` after lowering | Publish only at SYNC-5 with ABI versioning and mock-runtime evidence. |

The main task also accepts the SYNC-0 binary compatibility decision: no WHIRL
revision, ELF section, opcode encoding, type-kind encoding, or reader behavior
changes occur at SYNC-0. Whether to add an optional fixed-record FHE image is a
SYNC-1 compatibility decision and must include reader/writer bounds checks,
legacy-image tests, capability/version handling, and `ir_b2a -st -src`
coverage in the same infrastructure PR.

## Milestone Map

| Stage | Main task activity | FHE task activity | Required synchronization |
| --- | --- | --- | --- |
| C0: Baseline freeze | Inventory existing common/CNN operators, tensor APIs, builder capabilities, and binary hooks | Freeze ResNet-20 operator census, FHE descriptors, option semantics, and handoff requests | **SYNC-0: Plan and contract reconciliation** |
| C1: Native contracts | Publish accepted common/type contracts; add generic attachment, image, printer, and gatekeeper hooks | Finalize fixed FHE records, FHE-specific builder API, malformed-record rules, and negative tests | **SYNC-1: Native API and image contract freeze** |
| C2: Full-model capture | Supply merged opaque builder capabilities and common operator evidence | Capture complete ResNet-20 with class-centric PUs, source positions, weights, FHE entry, and encrypted descriptors | **SYNC-2: Frontend artifact certification** |
| C3: FHE conversion | Supply driver hook, cross-PU value identity, generic shape analysis, transactional rewrites, value-state attachment, and artifact publication | Certify shapes; implement FHE gatekeeping, 13-definition/21-context BatchNorm folding, converted-shape checks, operator dispositions, CKKS-state propagation, and reports | **SYNC-3: ResNet FHE conversion review** |
| C4: ReLU correctness | Preserve and print `common.relu`; expose source/result/descriptor/composite-stage/range evidence | Certify the ACE-compatible `7 -> 15 -> 13` Chebyshev sign profile, then materialize mandatory pre-ReLU bootstrap, normalization, ordered stages, and reconstruction with all option modes | **SYNC-4: ReLU `-O0` baseline certification** |
| C5: Standard WHIRL boundary | Supply standard call/result construction, unlowered-node gate, and `whirl2c` integration point | Freeze the deterministic full-ResNet correctness schedule and runtime-call census, publish its complete operation/rotation/key-requirement manifest, and implement the FHE C ABI and mock provider against it | **SYNC-5: Middle-WHIRL and mock executable gate** |
| C6: CKKS IR then ACE ANT ResNet | Review shared CKKS logical operator/image contracts and certify the executable `.ckks_ops.B` graph before driver/provider work; then complete provider manifest consumption, transport, and retained artifacts | Materialize and verify all provider-independent CKKS steps, lower them through the stable C ABI, then admit `FHErt_ant`, execute the secretless server, and validate results on the client | **SYNC-6: CKKS IR gate followed by end-to-end `-O0` client/server acceptance** |
| C7: Optimized planning | Enable reviewed VHO/WOPT integration, common encrypted-iteration-space records, census verification, and per-pass controls | Implement selectable MetaKernel and Fhelipe planners, then add ReSBM, boundary movement/fusion, the dedicated SSAPRE-model HPAO-MU phase, HPAO-FM/HPAO-LM, and equivalence reports. HPAO-MD remains design TBD. | **SYNC-7A-E: layout-planner A/B proof and optimized-versus-`-O0` proof** |
| C8: GPU path | Coordinate NVIDIA runtime and target-description infrastructure | Add FHE GPU capability, layout, cost, and later native POLY/RNS plans | **SYNC-8: Separate GPU architecture review** |

## Highlighted Synchronization Points

### **SYNC-0: Plan And Contract Reconciliation**

Inputs:

- Architecture plan v0.10.
- Main-task `FHE-DSL-INTEGRATION-PLAN.md`.
- FHE-task `FHE-WHIRL-INTEGRATION-PLAN.md` and operator/type handoff table.
- Current source inventory, including existing `OPR_DSLRELU` and
  `common.relu` registry entries.

Joint decisions:

- Mark every requested operator as reuse, extend, promote, or new.
- Resolve common, CNN, FHE, SIHE, and CKKS ownership.
- Freeze operand/result contracts, static attributes, descriptor requirements,
  effects, source-position rules, verifier owner, and lowering owner.
- Identify whether any optional FHE image section changes the WHIRL revision.

Exit evidence:

- One accepted contract matrix with no unresolved ownership.
- One binary compatibility decision and reader matrix.
- Planning documents committed so both worktrees share the same baseline.

Merge rule: planning/contract PR first. No FHE opcode allocation or binary
image coding begins before this checkpoint closes.

### **SYNC-1: Native API And Image Contract Freeze**

Historical status: implementation merged through PR #102 at `8ba9ee31`; the
subsequent corrective checkpoint fixed version-1 tensor-binding identity and
failure-atomic rejected entry-value insertion before SYNC-3 implementation.
Those corrections and their then-current acceptance are complete historical
prerequisites, not changes still waiting to merge. The 2026-09-29 current
exact-snapshot SYNC-3 certification reopened the complete image through the
current reader and verifier, so the consumed SYNC-1 substrate is current.

Main task provides:

- Stable common operator identities and version lookup.
- TensorDescriptorIR/EncryptionDescriptorIR attachment boundary.
- Opaque builder extension points and source-position requirements.
- Shared mapped-image, printer, reader/writer, and gatekeeper hooks.

FHE task provides:

- Fixed-width FHE record layouts with invalid-zero IDs.
- Deduplication keys and semantic-equivalence rules.
- Capability/version fields, first/count range validation, and malformed-image
  tests.
- Exact FHE builder declarations and expected `ir_b2a -st -src` spelling.

Exit evidence:

- Headers compile on the shared base.
- Old non-FHE `.B` files reopen unchanged.
- A new non-FHE `.B` does not emit an empty FHE section.
- A minimal FHE native producer writes, reopens, verifies, and prints its image.

Merge rule: main infrastructure PR merges first. The FHE task rebases onto the
updated `develop`; duplicate cherry-picks are omitted.

### **SYNC-2: Frontend Artifact Certification**

Historical status: PR #104 merged into `develop`, the FHE branch rebased, and
the subsequent corrective checkpoint resolved PU ownership, exact source
provenance, fail-closed dependency and count/absence checks, and retained
artifact requirements before SYNC-3 implementation. The corrected capture and
its then-current acceptance are complete historical prerequisites, not work
still waiting to merge. The 2026-09-29 current exact-snapshot capture and
separate-process reopen exercise the consumed SYNC-2 substrate.

The first full trace exposed a PU-scope correctness gap in shared
infrastructure: FHE entry values carried valid PU-relative `ST_IDX` values,
but the global FHE printer resolved them through the last selected local symbol
table.  PR #104 corrected the issue without changing the FHE image layout,
record sizes, opcode contracts, or operator versions by enforcing the existing
managed `owner_pu` relation during construction and mapped-image reopen and by
printing stable stored value identity.  The frontend concurrently assigned
source positions to external and implicit parameter symbols through the opaque
value source-position API.

Main-side readiness at SYNC-2 entry:

| Capture requirement | Merged infrastructure evidence | Ownership now |
| --- | --- | --- |
| ResNet expression nodes | `common.relu`, `common.flatten`, `common.residual_add`, `common.linear`, and the reviewed CNN v2 operators are registered logical DSL operators with gatekeeper checks | FHE frontend binds existing operator IDs and versions |
| Class-centric procedures and calls | PU creation/selection, source identity, formals, returns, call results, and callsite identity use opaque builder handles | FHE frontend emits the reachable ResNet-20 PU graph |
| Source cross-reference | Source-file registration and value, symbol, callsite, and region source-position APIs feed existing WHIRL source records | FHE frontend supplies captured Python file/line/column evidence |
| Tensor parameters | Canonical TensorDescriptorIR types and external tensor constants preserve side-file names and ranges through symbol/TCON evidence | FHE frontend publishes deterministic SafeTensors keys and payloads |
| FHE boundary | Entry contracts, entry values, encryption descriptors, tensor bindings, and key requirements use the optional `.WHIRL.dsl_fhe` image | FHE frontend binds the merged opaque APIs |
| Inspection and verification | Builder verification invokes structural FHE image validation; `ir_b2a -st -src` prints logical DSL, tensor, symbol, PU/region, and FHE evidence | Main task reviews the independent-process `.T` artifact |

No additional common/com API gap is known at SYNC-2 entry.  A newly discovered
gap must be reported at the opaque API boundary; it must not be bypassed by
constructing WN, TY, ST, or mapped-image records in Python.

Required artifact family:

```text
artifacts/fhe/resnet20_capture/
  secure_resnet20.py
  secure_resnet20.B
  secure_resnet20.T
  secure_resnet20.safetensors
  operator-census.txt
  capture-options.txt
  gatekeeper.log
```

Acceptance checks:

- `secure_resnet20.T` is generated with `ir_b2a -st -src`.
- Class-centric PUs, source files/lines, CNN regions, residual relationships,
  common/CNN operators, FHE boundary contracts, tensor descriptors, encryption
  descriptors, and external plaintext weights are visible.
- Every source ReLU is `common.relu`.
- No bootstrap or CKKS operator is invented by Python ingestion.
- Python exits before an independent process reopens the `.B` file.

Historically recorded evidence, which does not replace current independent
verification of accessible bytes:

- FHE rows resolve to `owner_pu=SecureResNet20` with stable entry-PU names for
  `input0`, external parameters, and `common_output_logits_46`.
- Source-derived external tensor parameters and implicit parameters have
  non-null source locations from the defining Python constructors.
- The retained trace contains 6 `FUNC_ENTRY` records, 9 `VCALL` block
  callsites, and 5 `cnn.basic_block.v1` REGION contracts.
- The retained census distinguishes 11 reusable `common.relu` node definitions
  from 19 source-context ReLU uses across calls; those contexts are not
  operator or function versions.

Merge rule: frontend/FHE capture PR depends on merged native infrastructure.
The main task reviews the `.T` evidence before this checkpoint closes.

### **Corrective Checkpoint Before SYNC-3 Implementation**

Status: completed before SYNC-3 source implementation. This historical
checkpoint no longer blocks the project.

The existing C0-C8 and SYNC-0 through SYNC-8 numbering remains unchanged.
This checkpoint restores prerequisites already required by those stages:

1. Reconcile version-1 tensor-binding identity across interning, lookup,
   validation, and writing, with nonzero flags rejected before mutation.
2. Make FHE entry-value insertion failure-atomic.
3. Enforce REGION/value PU ownership before BLOCK mutation, including
   PU-local `ST_IDX` collision coverage.
4. Replace inferred ResNet source offsets with exact source mapping for
   definitions, call contexts, parameters, and results.
5. Recertify SYNC-2 with fail-closed dependencies, exact positive and forbidden
   counts, an independent `ir_b2a -st -src` reopen, and retained host-visible
   artifacts identified by commands, toolchain, hashes, and paths.
6. Select and accept one exact node-retirement contract for physical
   BatchNorm removal, including logical-image state, users, provenance,
   rollback, old-reader behavior, and reopen behavior.

Main/common corrective commits merge first. The FHE/frontend branch then
rebases and recertifies SYNC-2. Main/common and FHE reviewers must accept the
node-retirement contract before any SYNC-3 implementation begins.

### **SYNC-3: ResNet FHE Conversion Review**

Implementation status: Commit 19 was independently reviewed and merged through
PR #131 at `d424c00be487f884a9ae7fb5cfc688f39e96d279`. The merge tree is identical
to the accepted Commit 19 tree at
`93d80a6b79858ee5b7183f4426d3c94573b53e95`; no conflict resolution changed
the feature.

Historical verification status: a complete merged-tip run was recorded as
Pass on 2026-09-14.

Current independent verification status: **verified** by the 2026-09-29
exact-snapshot run retained at
`/private/tmp/open64-fhe-sync3-recertification-20260929/current-snapshot-certification`.
It reproduced 13 physical Conv/BN definition rewrites, 21 source-context folds,
42 converted tensors, 46 operator dispositions, 19 authenticated ReLU ranges,
and 19 matching context-specific CKKS states. SYNC-4 may consume this focused
planning checkpoint after the evidence update receives review.

The exact ACE bytes were project-approved from empirical ANT ACE evidence and
the coefficient profile is integrated. A pinned ACE-derived checkpoint and a
deterministic 5,000-image class-stratified CIFAR-10 training subset supplied the
19 approved ranges. A disjoint 1,000-image held-out test run historically
recorded 91.6%
clear accuracy, 91.5% polynomial accuracy, 0.1 percentage-point degradation,
99.9% prediction agreement, and zero out-of-range values against predeclared
gates. Original training provenance remains unknown and is not claimed.
The historical deterministic collector in
`doc/FHE-SYNC3-RELU-RANGE-CALIBRATION.md` was recorded as proving the 19-way
identity join, pre-ReLU observation algorithm, distribution evidence, bound
checks, and canonical manifest hashing. Its fixture is explicitly not
calibration.
PR #127 supplies authenticated runtime manifest selection, and the FHE-owned
RapidJSON consumer verifies exact bytes, approval, identity coverage, and
all-PU consumption before publishing. It also authenticates the active
SafeTensors whole-file digest against the approved model payload; a changed
payload fails natively before conversion. PR #130 supplies the context-state
image. The FHE pass records one callee-tagged `POST_REFRESH.v1` row per range,
with ACE levels 15, 17, or 18, scale 56, two components, precision 30, and a
pending pre-ReLU bootstrap reason. This is compiler/static schedule evidence,
not executed ACE ANT ciphertext inference.
Opcode allocation, bootstrap insertion, SIHE/CKKS primitive lowering, and
ACE ANT/runtime lowering remain outside this checkpoint.

The optional `.WHIRL.dsl_fhe_context_state` image defined by
`doc/FHE-SYNC3-CONTEXT-CKKS-STATE-CONTRACT.md` is merged and consumed. It
preserves shared procedure definitions while recording exact call-context CKKS
planning state. SYNC-4 remains responsible for materializing bootstrap,
normalization, the three Chebyshev stages, and ReLU reconstruction.

Required phase output:

```text
secure_resnet20.B
  -> ordinary DSL/common gatekeeper
  -> call-ABI and PU-interface identity validation
  -> generic XLA-style tensor shape propagation
  -> FHE semantic gatekeeper
  -> -FHE:checkpoint=secure_resnet20.fhe.B
  -> VHO_FHE_Convert_Driver_Try() for every selected PU
  -> converted-shape verification
  -> value-specific CKKS-state propagation
  -> FHE semantic gatekeeper, converted form
  -> complete managed-image validation
  -> standard Write_PU_Info()/Write_Global_Info() binary WHIRL path
  -> secure_resnet20.fhe.B
  -> ir_b2a -st -src secure_resnet20.fhe.B secure_resnet20.fhe.T
  -> secure_resnet20.fhe.conversion-report.txt
```

The checkpoint is conversion-only. It deliberately returns before ordinary
DSL and language VHO lowering and before WOPT/LNO/CG. Each converted PU is
written while its own local symbol table is active; the requested final path
is published atomically only after all PUs and complete managed images pass.
This main/backend-owned service prevents the FHE task from duplicating raw
PU traversal, symbol-table lifetime, mapped-image, or writer orchestration.

The cross-PU analysis spine is normative for the complete `-O0` project:

```text
caller actual value
  -> call-argument role
  -> callee formal ordinal
  -> callee formal DSL_IR_VALUE_ID
  -> callee operator operand and result
```

Generic shape propagation follows this identity before FHE conversion. After
BatchNorm folding and retirement, converted-shape verification proves geometry
is unchanged and dead BN-only inputs have no executable uses. FHE CKKS-state
propagation then uses the same identities to infer value class, packing,
layout, level, scale, precision, alignment, rotation keys, and pending actions.
None of these value-specific facts may be inferred by name, local `ST_IDX`,
argument-order convention, or shape equality alone. The full algorithm is in
`doc/FHE-SHAPE-AND-ENCRYPTION-STATE-PROPAGATION.md`.

Acceptance checks:

- BatchNorm folding is explicit and source-linked.
- Every caller actual resolves to the exact callee value through validated
  call-ABI and complete PU-interface records.
- Source and converted tensor shapes pass deterministic propagation and
  operator, REGION, call-formal, and call-result consistency checks.
- Conv2d, residual add, pooling, flatten, classifier, and encrypted logits have
  reviewed FHE dispositions.
- Every live encrypted value has accepted value-specific CKKS planning state;
  retired values and verified dead BN ABI inputs are excluded from live
  accounting.
- `common.relu` remains traceable to its approximation contract.
- The conversion report lists accepted, rewritten, and rejected operations.
- Missing scheme, illegal secret-key use, unsupported activation, training
  behavior, and malformed descriptors fail with stable diagnostics.

Main/common hooks requested for this checkpoint include the
`VHO_FHE_Convert_Driver()` phase hook, opaque logical-DSL and FHE-image read
APIs, reviewed converted-operator or annotation update APIs, value-specific
CKKS state attachment, approximation-contract attachment, folded side-file
payload writer support, diagnostic-code registry conventions, and printer
support for converted FHE-CNN provenance.

The main/common review preserves the exact version-1 `.WHIRL.dsl_fhe` image.
New conversion-plan evidence is staged in a separate optional fixed-row
`.WHIRL.dsl_fhe_plan` section after its row contract is published. FHE-CNN
identity should use the existing domain-wrapper mechanism when it delegates to
an existing common/CNN semantic target; wrapper identity alone does not
justify a new `DSL_OPERATOR` enum.

BatchNorm folding must respect the certified shared-PU representation. Rewrite
each compatible physical clone body/signature once, create folded payloads for
each source call context, and rewrite caller actuals accordingly. The report
must distinguish physical definition rewrites from context-specific payload
folds. Exact ResNet-20 counts are certification assertions, not generic
gatekeeper rules.

The exact physical review is
`doc/FHE-SYNC3-NATIVE-PLAN-CONTRACT.md`. It defines the four fixed record
families, opaque APIs, wrapper-registry boundary, sentinel rules, mapped-image
compatibility, and stable `ir_b2a -st -src` headings required before SYNC-3
implementation.

Focused SYNC-3 exit evidence may close only this conversion-planning review
checkpoint. It must prove the accepted dispositions, BatchNorm folds,
approximation obligations, CKKS planning state, diagnostics, compatibility,
and retained artifacts from the exact candidate. It must not claim v0.10
Architecture Phase 3 or M4 completion. Those milestones still require C4 /
SYNC-4 bootstrap-plus-polynomial materialization and the remaining v0.10
execution evidence.

### **SYNC-4: ReLU `-O0` Baseline Certification**

Normative transformation:

```text
common.relu(x)
  -> sihe.bootstrap(x, reason=relu_boundary)
  -> normalize(x, context_bound=B)
  -> composite_chebyshev_sign(stage_degrees=[7,15,13])
  -> fhe.cnn.poly_activation(0.5*x*sign(x/B) + 0.5*x,
                             source_activation=common.relu)
```

The selected architecture profile is
`ace.chebyshev.sign.7x15x13.depth11.v1`; executable SYNC-6 recaptures use
version 2 to identify the exact ACE addition-chain evaluator while reusing the
authenticated v1 coefficient/range evidence. Degree 3 is retained only as a future
experimental low-depth profile. The append-only composite-profile,
context-state, and materialization images preserve the ordered composition.
Exact coefficient bytes, all 19 identity-bound normalization ranges,
clear/model error, and CKKS depth/state enter through the certified SYNC-3
input and are revalidated by the SYNC-4 semantic consumer.

Required tests:

| Mode | Expected result |
| --- | --- |
| `bootstrap=auto` | Compiler inserts exactly one required pre-ReLU boundary per surviving ReLU before any additional depth-driven refresh |
| `bootstrap=on` | Same mandatory baseline, with backend bootstrap capability required |
| `bootstrap=manual` | Compilation succeeds only when each surviving ReLU already has an explicit compatible boundary |
| `bootstrap=off` | Compilation rejects every surviving ReLU in the first CKKS release |

Exit evidence:

- Focused ReLU `.B` and `.T` before and after FHE/CKKS planning.
- Candidate profile manifest and independent three-stage clear-evaluation
  report.
- Full ResNet depth/bootstrap report with source positions.
- Fused backend path, when present, retains the same logical evidence.
- No `-O0` movement, merging, deduplication, or profitability placement.

The exact context-sensitive materialization contract is
`FHE-SYNC4-RELU-MATERIALIZATION-CONTRACT.md`. Eleven reusable physical ReLU
definitions represent nineteen contexts, so S4 persists one six-operation
schedule per context and does not encode a context-specific bound or level in a
shared callee body. Physical call specialization and standard-WHIRL runtime
calls remain SYNC-5 work.

The main/common substrate now supplies the fixed materialization image,
atomic context transaction, per-PU partial validation,
`VHO_FHE_Materialize_Driver_Try()`, phase options, and the shared binary-last
checkpoint service. FHE-owned code registers provider policy and semantic
callbacks through that surface; it must not add raw driver, mapped-image, or
table manipulation. Exact coverage remains a program-finalizer requirement,
not a per-PU requirement.

The SYNC-4 provider-capability check covers only the operations materialized at
this checkpoint: the approved ReLU composite arithmetic, slot configuration,
bootstrap support, and post-bootstrap levels 15, 17, and 18. It must not claim
coverage of the complete CNN signed-rotation set. SYNC-5 owns production of the
complete deterministic correctness schedule and its operation, signed-rotation,
and key-requirement manifest.

Implementation status: **S4-1 through S4-6 implementation-complete and locally
certified on 2026-09-29.** The FHE semantic module authenticates the pinned ACE
ANT subset, materializes one dense six-operation schedule for each of 19
contexts, and enforces exact final coverage. The retained six-PU artifact at
`/private/tmp/open64-fhe-sync4-final` reopens with `ir_b2a -st -src` and proves
114 operations, 19 refreshes, and post-refresh levels `15:16,17:1,18:2`.
`auto`, `on`, and complete `manual` pass; `off`, missing-manual, bad hash, and
stale-output cases fail atomically. Independent recurrence-versus-Clenshaw
numerical-oracle and
dense normalized-ReLU checks certify the three-stage numerical policy.

This closes the SYNC-4 materialization-planning gate only. The output retains
logical materialization rows and source `common.relu` evidence; SYNC-5 owns
standard-WHIRL runtime-call lowering and the complete CNN schedule, and SYNC-6
owns linked ACE execution.

### **SYNC-5: Middle-WHIRL And Mock Executable Gate**

Required phase output:

```text
secure_resnet20.fhe.B
  -> SIHE/CKKS correctness planning
  -> secure_resnet20.ckks.B
  -> shape and context-sensitive FHE-state certification
  -> exact runtime-role selection
  -> program-level runtime-interface projection
  -> FHE runtime-call lowering
  -> secure_resnet20.mid.B
  -> whirl2c
  -> secure_resnet20.c
  -> mock-linked executable
```

Acceptance checks:

- Propagation is the semantic admission gate for projection: every live value
  has concrete compatible shape, value class, encryption descriptor, layout,
  level, scale, component, precision, and pending-action evidence.
- Canonical tensor `TY_IDX`, TensorDescriptorIR, DSL value, PU-interface, and
  call-ABI rows remain source provenance. A separate owner-qualified runtime
  projection records the exact ciphertext or plaintext handle TY/ST.
- PU inputs pass exact borrowed handles by value; hidden results use
  caller-owned null-initialized handles passed by pointer-to-handle; caller and
  callee projected TYs agree exactly across all nine block callsites.
- `secure_resnet20.mid.T` contains standard WHIRL calls, formals, results,
  symbols, initializers, status checks, and control flow only.
- The unlowered-node verifier rejects remaining FHE/SIHE/CKKS/HPOLY nodes.
- Result ciphertext handles use caller-owned no-alias temporaries.
- Generated C includes only the stable C ABI, not ACE ANT internal types.
- Source positions survive on generated calls and result stores.
- The full ResNet-20 correctness schedule and runtime-call census account for
  every model operation and publish the complete operation, signed-rotation,
  and evaluation-key requirements consumed by the mock and later ACE gate.
- The mock executable validates the exact published manifest rather than a
  provider-specific schedule inferred during SYNC-6.
- Any propagation or projection failure is checkpoint-terminal and publishes
  no `.mid.B`, generated C, or auxiliary schedule artifact.

Post-PR #156 audit checkpoint: the phase shell, one-for-one tensor projection,
descriptor selection, ABI v1, standalone mock, verified-dead formal/actual
pruning, root external-parameter promotion, and explicit model/coefficient
resource threading are merged. The FHE consumer now resolves exact
owner-qualified projected value handles and role-qualified program inputs, and
constructs a detached checked descriptor-select plus bootstrap standard-WHIRL
sequence without mutating the logical image.

PR #157 closed the final shared-contract dependency with the atomic native DSL
to standard-WHIRL lowering transaction. The FHE consumer now exercises it on
one complete source `common.relu`: six descriptor selections and the ordered
refresh, normalization, three polynomial-stage, and reconstruction calls
replace the executable native definition; one final store defines the exact
projected result handle; and the logical node/value remain inspectable as
`status=lowered` provenance. The model and three coefficient resources are
explicit program inputs. The remaining S5-2c work is FHE-owned expansion to
every admitted full-model operation, all-PU callback registration, exact
87-static/147-dynamic census verification, and atomic `.mid.B` publication.
The historical interface gap and its PR #156/#157 resolution are tracked in
`doc/FHE-SYNC5-RUNTIME-ENTRY-BINDING-GAP.md`.

The FHE semantic builder now covers all ABI v1 evaluation classes used by the
model: Conv2D/plain, residual add, the six-operation composite ReLU, average
pool, layout conversion, and linear/plain. This closes per-operation checked
call construction. It does not yet close the full-model gate: the production
callback must still derive static ordinals from the certified schedule, submit
complete per-PU atomic lowering arrays, and prove the 87/147 evaluation and
selector censuses before publishing `.mid.B`.

The canonical commit-by-commit SYNC-5 execution order, exact request counts,
negative tests, retained artifacts, and exit criteria are maintained in
`doc/FHE-SYNC5-NATIVE-OWNERSHIP-AUDIT.md` under **Reviewable Commit Sequence
And Acceptance Gates**. Its S5-A through S5-H gates control detailed status;
this consolidated plan remains the cross-milestone summary.

Merge rule: assign every shared driver/whirl2c file to one PR owner before
editing. The mock-runtime PR must not depend on an ACE runtime installation.

### **SYNC-6: CKKS IR Then ACE ANT `-O0` Client/Server Acceptance**

The first SYNC-6 gate is a provider-independent executable CKKS semantic
WHIRL image, `secure_resnet20.ckks_ops.B`, reopened as `.T` with
`ir_b2a -st -src`. It is later than the SYNC-4 CKKS materialization *plan*
and earlier than terminal SYNC-5-style standard runtime calls. It must expose
the complete per-context value-state, direct operation, key, rotation,
bootstrap, and provenance graph; see
`doc/FHE-SYNC6-CKKS-IR-CONFORMANCE-GATE.md`. Shared operator/image contracts
require main/common review before allocation. The existing SYNC-5 mock path
remains the regression reference. ACE library interaction begins only after
this CKKS IR gate and its stable-C-ABI lowering pass.
Do not add a second ACE-shaped mock to the S6-0 development path. Keep the
Open64 ABI mock as the sole runtime test double through CKKS IR and terminal
lowering. A focused ACE-call test double may be considered during final
provider-adapter integration only if a specific adapter contract needs it;
it must not become another required runtime layer.

The exact pinned ACE import/export gap is recorded in
`doc/FHE-SYNC6-ACE-ANT-ADMISSION-AUDIT.md`. It blocks later ACE admission and
the final client/server exit, not CKKS IR creation or mock-based verification.
The selected `-O0` terminal stage is the **CKKS2C early exit**, as specified with an
ACE-source comparison in `doc/FHE-SYNC6-ACE-CKKS-C-STAGING.md`. After the
complete CKKS image gate, Open64 generates provider-private C directly from
CKKS semantics, with no POLY/RNS or POLY2C pass. The frozen public
generated-C application keeps its nine
semantic operation kinds and 87/147 schedule. This follows ACE's direct
CKKS-to-C pattern but requires a new ANT-backed CKKS facade; the pinned ACE
ANT codegen itself runs through POLY-to-C. No second required mock layer is
added to the S6-0 development path. The backend flow is continuous after
the verified CKKS gate: the `openpy` option selects CKKS2C and exits for
the initial `-O0` release, or allows the same graph to continue through
CKKS-to-POLY lowering and POLY2C when that path is implemented. POLY2C is
not an `-O0` fallback for a missing CKKS2C operation or runtime capability.
The proposed `-FHE:codegen=ckks2c|poly2c` control remains a main/driver
contract to implement, not a currently accepted option.

The SYNC-6 implementation sequence is deliberately split at two shared
mutation contracts. A read-only grouped-expansion preflight is useful
evidence, but does not close either transaction or authorize `.ckks_ops.B`
publication. The six captured source PUs are not a fixed executable-PU
target: the accepted nineteen-context state evidence requires at least nine
fixed-schedule variants when B is a typed formal. The exact data-flow and
clone legality review are in
`doc/FHE-SYNC6-CONTEXT-SPECIALIZATION-CONTRACT.md`.

The detailed S6-0c producer, specialization, transaction, and mapped
artifact sequence is `doc/FHE-SYNC6-S6-0C-DETAILED-EXECUTION-PLAN.md`.

| Review gate | Owner and required change | Tests and exit evidence |
| --- | --- | --- |
| S6-0a atomic grouped expansion (merged PR #165) | Main/common publishes the owner-PU-safe one-source/many-CKKS-step transaction, nine logical operators, typed event rows, and F8 scalar kid1. Preflight covers groups, operands, source uses, call actuals, and REGION; late-failure rollback keeps the source as nonexecutable `LOWERED` provenance only on success. | Six-group mapped roundtrip and linked opcode, event-image, expansion, and PU-specialization regressions passed. These are structural tests, not a certified FHE circuit. |
| S6-0b executable context specialization | Main/common reviews a generic `IPO_CLONE`-based PU/interface/call transaction covering all DSL/FHE image ownership. FHE derives the complete CKKS signature and exact approved B data. Preferred ABI adds exact-TY plaintext B formals and per-callsite actuals; a static-B clone fallback requires separate review. Equivalent signatures reuse one clone, and source-definition identity survives. | Two equal-signature contexts reuse one clone with distinct bound actuals; incompatible levels split. Verify two independent B values per block call, no local-ST collision, complete formals/actuals/REGION/source positions, and rollback on failed clone or call retarget. The retained read-only ResNet audit proposes 18 caller actuals plus one entry-owned bound. |
| S6-0c complete FHE CKKS producer (Conv and residual add complete; remaining operators in progress) | FHE consumes only merged common transactions. A focused adapter preflights canonical tensor/encryption associations, concrete result states, exact key/target attributes, unary input-to-output transfer, and aligned add/sub/mul transfer with explicit multiply repair obligations, then binds one state per new step value; a later bind failure is terminal. The real ten-PU checkpoint expands all 21 specialized Conv definitions into 28,927 explicit CKKS operations using 5,691 authenticated feature rows, 21 expanded biases, and 104 logical masks. It then lowers all nine exact-shape residual joins to nine `ckks.add` operations plus seven explicit level-4 to level-3 `ckks.modswitch` operations; the two projection joins are already aligned. Semantic TCON interning shares byte-identical mask ranges without merging context provenance. At `-O0`, prioritize the simplest correct explicit CKKS semantics; reducing level consumption or fusing the stride packer is `-O1` work. ReLU, pooling, flatten, and linear expansion remain open. | Separate-process `ir_b2a -st -src` reopens the 230 MB checkpoint with 10 PUs, 28,943 CKKS events, zero physical Conv or residual-add nodes, exactly 21+9 lowered provenance rows, concrete residual states, nonzero source locations, and an atomically published 202,620,928-byte plaintext side asset plus residual report. Focused adapter, plan, asset, event, external-tensor, and REGION/call rollback tests pass. A stale destination rejects with `CFHEMAT-CHECKPOINT-006`, preserves the published hash, and leaves no `.tmp`. The full gate remains open: all 147 high-level context events mapped to executable CKKS steps, 19 explicit pre-ReLU bootstraps plus any separately certified capacity refreshes, no live common.relu or BN, complete key/rotation/depth/state report, and final all-operator `.ckks_ops.B` certification. |
| S6-0d CKKS2C early exit (blocked on S6-0c and shared transaction) | Preserve ABI v1's canonical six-PU public generated-C surface, 87 physical static callsites, and 147 ordered dynamic evaluations. FHE proves each specialized CKKS event group maps to one exact source-context ABI descriptor, never one public call per primitive. After the complete `.ckks_ops.B` gate, the `openpy`-selected CKKS2C early exit generates a separate provider-private C evaluator without POLY/RNS; the continuing backend route later lowers the same verified CKKS stage to POLY and POLY2C. Its ACE-shaped ANT facade requires review because pinned ACE ANT codegen currently selects POLY-to-C. Main/common reviews the driver selector, grouped terminal transaction, and event-image policy; see `FHE-SYNC6-CKKS-ABI-LOWERING-BOUNDARY.md` and `FHE-SYNC6-ACE-CKKS-C-STAGING.md`. | Reopen `.ckks_ops.B` to prove the reversible clone-to-source mapping. The private C emitter compiles against checked facade declarations, has exact asset/state/key/bootstrap provenance, and introduces no public primitive call. The unchanged public C links to the existing ABI mock and matches SYNC-5. A missing CKKS operation or ANT capability fails without POLY2C fallback; the selected `-O0` exit runs no POLY/POLY2C pass. Selection and future continuation tests prove exactly one exit, no implicit retry, and no final or temporary artifact on failure. |
| S6-1 ACE admission | ACE/provider owner publishes a new immutable pin with an evaluator-only constructor and versioned public context, evaluation-key, ciphertext-input, and ciphertext-output import/export. Open64 compares the complete frozen manifest before link or execution. | Fresh-process positive/negative transport tests and binary/dependency proof that the server cannot generate, retain, or use a secret key or decryptor. The current pin remains `CAPABILITY_MISSING`. |
| S6-2 to S6-6 client/server closure | Runtime owners implement broker, supervised secretless worker, client provisioning/transport, exact ACE execution, and failure containment in that order. | Full encrypted ResNet-20 inference returns ciphertext logits for client-only decryption; manifest, accuracy, resource, lifecycle, and adversarial negatives meet the existing SYNC-6 exit below. |

Do not stack FHE mutating code on unpublished main/common work. Main first
publishes and merges each generic transaction; the FHE branch then rebases
and certifies its consumer. The ten-PU static-B option is a bounded `-O0`
fallback, not an unreviewed substitution for the preferred nine-variant
typed-B contract or a claim that other CKKS operations cannot require more
specialization.

Target command shape:

```text
openpy -O0 -keep secure_resnet20.py \
  -dsc-fhe=cnn \
  -dsc-fhe-scheme=ckks \
  -dsc-fhe-bootstrap=auto \
  -dsc-fhe-backend=ace-ant \
  -o secure_resnet20.out
```

Required evidence:

- Original, FHE, SYNC-4 CKKS-plan, executable CKKS-operator, and terminal
  standard-call `.B`/`.T` pairs; the historical `.mid.B` stem is not a formal
  WHIRL-level assertion.
- Generated C, compile command, link command, pinned ACE provider manifest, and
  dependency inspection.
- Encryption configuration, key requirements, layout, depth/bootstrap,
  accuracy, memory, and runtime reports.
- A separate client/provisioner owns the secret key and retains evidence that it
  exported a versioned public context, only the required evaluation,
  relinearization, rotation, and bootstrap material, and encrypted input. Only
  that client may decrypt the returned ciphertext for validation.
- The server imports those versioned artifacts, contains no key-generation or
  decryption API or dependency, and returns encrypted logits without possessing
  secret-key material in its binary, runtime state, logs, or artifacts.
- The ACE capability manifest is compared field-for-field with the complete
  SYNC-5 operation, signed-rotation, and key-requirement manifest before
  execution.
- Exact `FHErt_ant` revision, build options, operation capabilities, and the 19
  bootstrap calls with post-refresh levels 15, 17, and 18.
- An embedded/local ACE harness may be retained for bring-up and numerical
  diagnosis, but its result is labeled non-closing evidence and is not a
  substitute for the client/server security test.

Exit: the ACE-ANT-linked server imports the certified public context,
evaluation-key family, and encrypted CIFAR-10 input, produces encrypted logits
within the declared CKKS and model-accuracy budgets, and is proven to have no
secret-key, key-generation, or decryption dependency. Versioned context,
evaluation-key, ciphertext-input, and ciphertext-output import/export are
mandatory for this exit. If the exact ACE pin cannot provide an evaluation-only
server context and the required transport, ACE remains the selected provider
but SYNC-6 is blocked pending a reviewed ACE patch and new immutable pin; the
threat model must not be weakened to declare completion.

### **SYNC-7: Optimized-Versus-`-O0` Proof**

Before enabling any ReSBM, HPOLY/HPAO, fusion, or refresh-boundary movement:

- Preserve the `-O0` artifact family as the correctness baseline.
- Give every optimization step an independent option switch.
- Retain phase `.B`/`.T` evidence and an optimization decision report.
- Prove source semantics, polynomial approximation error, CKKS scale/level
  legality, key availability, and result tolerance.
- Fall back to the `-O0` plan when proof or provider capability is missing.

The encrypted-layout portion of SYNC-7 is divided into these jointly reviewed
gates:

| Gate | Analysis and transformation | Required evidence |
| --- | --- | --- |
| **SYNC-7A: common baseline** | Freeze the gatekeeper-approved FHE-CNN graph, TensorDescriptorIR and encryption state, ring dimension, backend manifest, and all relevant options. | One pre-layout `.B`/`.T` image and manifest used without change by both planners. |
| **SYNC-7B: MetaKernel** | Analyze Conv/MVM decomposition and batching, immediately transform the kernel iteration space, then derive packing and masks. | Original/transformed domains, MetaKernel-unit decomposition, packed layout, rotation schedule, and independently recomputed census. |
| **SYNC-7C: Fhelipe** | Analyze the whole graph, assign dimension-bit/interleaved layouts, choose compaction and conversions, then materialize the resulting one-dimensional CKKS schedule. | Global layout decisions, compaction/conversion provenance, materialized schedule, and independently recomputed census. |
| **SYNC-7D: normalized comparison** | Canonicalize both outputs into the common encrypted layout and iteration-space interface and prove equivalent source semantics. | Per-value, per-operator, per-PU, and whole-program comparison with stable source identities. |
| **SYNC-7E: selected-plan handoff** | Select a plan under the reviewed policy and hand it to SIHE/CKKS, ReSBM, and HPOLY without changing its recorded provenance. | Selection rationale, fallback evidence, and retained downstream `.B`/`.T` checkpoints. |

Both planners must report the same metric definitions after transformation is
materialized and before ReSBM changes the graph: static rotation operations,
execution-weighted rotations when trip counts are known, unique signed rotation
offsets, total CKKS slots, active logical slots, gap/invalid slots, gap ratio,
peak/introduced/compacted gaps, masked slots, ciphertext count, packing density,
layout conversions, permutations, masks, and rotate-add reductions.

The canonical aggregate is `gap_slots = total_slots - active_slots`.
Padding and replicated slots are reported separately and are not counted as
active logical elements. A common checker recomputes rotation and gap totals
from each materialized WHIRL image; planner self-reported estimates alone do
not close SYNC-7B or SYNC-7C.

HPAO-MU follows the SSAPRE algorithmic model in a dedicated HPOLY phase; the
existing WOPT SSAPRE implementation remains unchanged. HPAO-MD implementation
is blocked until its analysis, legality, ordering, extended-basis lifetime, and
profitability design receives a separate review.

### **SYNC-8: Separate GPU Architecture Review**

GPU work does not silently extend the ACE ANT CPU/reference milestone. Before
native FHE GPU implementation, jointly review:

- provider capability and target-description schema;
- encrypted layout and device-memory ownership;
- host/device transfer and asynchronous lifetime effects;
- POLY/RNS operator and type contracts;
- kernel packaging/loading and NVIDIA toolchain boundary;
- fallback, telemetry, roofline, and regression methodology.

## Pull Request And Rebase Protocol

1. Close each synchronization point with retained artifacts and a short joint
   decision record.
2. When both tasks have changes, open the main infrastructure PR first.
3. Merge the main infrastructure PR into `develop`.
4. Rebase the FHE task branch onto updated `develop`.
5. Let Git omit duplicate patches already present through the infrastructure
   merge; do not preserve duplicate cherry-picks.
6. Rerun the FHE task's focused and full-model tests after the rebase.
7. Open or update the FHE PR with an explicit dependency on the infrastructure
   PR and links to retained `.T` evidence.
8. Do not combine unrelated cleanup or warning fixes with a synchronization PR.

## Joint Definition Of Done

The ResNet-first FHE project reaches its first complete milestone only when:

- full ResNet-20 ingestion produces valid binary very-high-level WHIRL;
- every value has complete tensor and encryption descriptors;
- every ReLU is represented by `common.relu` at the source-semantic level;
- `-O0` materializes the mandatory pre-ReLU bootstrap and approved polynomial
  approximation;
- all FHE/CKKS constructs lower to standard middle-WHIRL before `whirl2c`;
- generated C compiles and links against the stable FHE C ABI, the ACE ANT
  provider adapter, and pinned `FHErt_ant`;
- a separate client/provisioner retains the secret key and supplies only the
  certified public context, required evaluation-key material, and ciphertext;
- the server executable runs without Python and without key-generation,
  decryption, secret-key API, secret-key dependency, or secret-key runtime
  state;
- versioned context, evaluation-key, ciphertext-input, and ciphertext-output
  transport is validated across the client/server process boundary;
- encrypted output validates within declared numerical and accuracy budgets;
- old binary WHIRL compatibility tests continue to pass; and
- all reviewable artifacts remain in a host-visible directory after testing.

## Immediate Coordinated Queue

1. FHE owner: retain the certified ReLU-free BatchNorm checkpoint proving 13
   physical definition retirements, 21 context folds, and 42 converted tensors.
2. FHE and numerical-policy reviewers: review and explicitly approve the
   frozen ACE coefficient source, license, ordering, exact binary64 bytes,
   stage checksums, source revision, and manifest hash in the Commit 17
   package.
3. Frontend/model owner: first approve a trained checkpoint, immutable
   calibration split, preprocessing, bound estimator, safety margin, and
   outlier policy; then run the implemented collector for all 19 ReLU
   source contexts and bind each approved positive bound to exact Open64 value,
   PU identity, and callsite records. No name-only or default fallback is valid.
4. Completed infrastructure: PR #127 added the reviewed calibration-manifest
   path and expected SHA-256 conversion options, and PR #128 exposed bundled
   RapidJSON to backend consumers without a new linked-library dependency. The
   FHE consumer authenticates exact bytes before parsing and retains one
   selection across the all-PU checkpoint.
5. Numerical-policy reviewers: certify clear sign/ReLU error and pinned
   ResNet-20 logit/top-1 tolerances against named dataset hashes.
6. FHE/runtime owners: prove the depth-11 schedule and concrete post-refresh
   CKKS level, scale, component, and precision contracts.
7. FHE owner: calibration machinery and authenticated 19-context binding are
   implemented against the PR #123 opaque profile APIs. Activate those bindings
   for SecureResNet only after items 3, 5, and 6 are accepted, then complete
   value-specific CKKS planning state.
8. FHE owner: rerun the full six-PU SecureResNet checkpoint, publish auxiliary
   payload/report artifacts transactionally, and publish `.fhe.B` last.
9. Independent reviewer: reopen with `ir_b2a -st -src`, reconcile report and
   image counts, and close only focused SYNC-3. Bootstrap insertion and
   polynomial materialization remain SYNC-4 work.
