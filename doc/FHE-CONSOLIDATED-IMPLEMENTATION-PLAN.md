# Open64 FHE Consolidated Implementation Plan

Status: cross-task implementation and synchronization plan  
Primary model target: inference-only ResNet-20/CIFAR-10  
Baseline optimization level: `-O0` correctness pipeline  

This document coordinates two Open64 workstreams:

- **Main task**: shared WHIRL and common/com infrastructure.
- **Open64 FHE support task**: FHE semantics, ingestion, conversion, CKKS
  planning, runtime integration, and FHE-specific validation.

It consolidates the architecture in
`DSC_FHE_Compiler_Architecture_and_Integration_Plan_v0.9.docx`, the main-task
implementation detail in `FHE-DSL-INTEGRATION-PLAN.md`, and the FHE-task plan
in `FHE-WHIRL-INTEGRATION-PLAN.md`.

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
  -> C compilation and OpenFHE provider link
  -> server executable
  -> encrypted input -> encrypted logits
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
12. Secret keys never enter WHIRL, generated server C, server artifacts,
    diagnostics, or server runtime state.

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
| Runtime | Generic driver link integration and provider manifest consumption | Stable FHE C ABI, mock provider, OpenFHE adapter, key policy, and runtime tests |
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
| `config_fhe.*`, VHO FHE passes, runtime, OpenFHE provider | FHE task | Main reviews driver/pass placement and standard-WHIRL boundary at checkpoints |
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
| C3: FHE conversion | Supply driver phase hook and common/tensor legality services | Implement FHE gatekeeper, BatchNorm folding, model adaptation, approximation contracts, and conversion report | **SYNC-3: ResNet FHE conversion review** |
| C4: ReLU correctness | Preserve and print `common.relu`; expose source/result/descriptor evidence | Materialize mandatory pre-ReLU bootstrap and polynomial approximation with all option modes | **SYNC-4: ReLU `-O0` baseline certification** |
| C5: Standard WHIRL boundary | Supply standard call/result construction, unlowered-node gate, and `whirl2c` integration point | Implement runtime-call lowering, FHE C ABI, and mock provider | **SYNC-5: Middle-WHIRL and mock executable gate** |
| C6: OpenFHE ResNet | Complete driver link flow, provider manifest consumption, and retained artifact family | Implement OpenFHE provider, client provisioning, CKKS execution, and full ResNet validation | **SYNC-6: End-to-end `-O0` acceptance** |
| C7: Optimized planning | Enable reviewed VHO/WOPT integration and per-pass controls | Add ReSBM, boundary movement/fusion, HPOLY/HPAO, and equivalence reports | **SYNC-7: Optimized-versus-`-O0` proof** |
| C8: GPU path | Coordinate NVIDIA runtime and target-description infrastructure | Add FHE GPU capability, layout, cost, and later native POLY/RNS plans | **SYNC-8: Separate GPU architecture review** |

## Highlighted Synchronization Points

### **SYNC-0: Plan And Contract Reconciliation**

Inputs:

- Architecture plan v0.9.
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

Status: closed.  Main/common implementation merged through PR #102 at
`8ba9ee31`; FHE semantic review accepted the contract without blockers.  The
FHE task rebases on this merged contract before producing SYNC-2 artifacts.

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

Status: completed after PR #104 merged into `develop` and the FHE branch
rebased, regenerated the ResNet-20 artifact family, and passed final
post-rebase certification.

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

Final evidence:

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

### **SYNC-3: ResNet FHE Conversion Review**

Status: active contract preparation after PR #105 merged into `develop` at
`73d8ec0d`. The reviewable proposal is
`doc/FHE-SYNC3-CONVERSION-CONTRACT.md`. Implementation must remain limited to
the accepted SYNC-3 conversion scope: FHE gatekeeper, legal BatchNorm folding,
CNN-to-FHE disposition, ReLU approximation-contract attachment, value-specific
CKKS state evidence, and retained conversion artifacts. Opcode allocation,
bootstrap insertion, SIHE/CKKS primitive lowering, OpenFHE/runtime lowering,
and shared common/com edits require their reviewed checkpoints.

Required phase output:

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

Acceptance checks:

- BatchNorm folding is explicit and source-linked.
- Conv2d, residual add, pooling, flatten, classifier, and encrypted logits have
  reviewed FHE dispositions.
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

### **SYNC-4: ReLU `-O0` Baseline Certification**

Normative transformation:

```text
common.relu(x)
  -> sihe.bootstrap(x, reason=relu_boundary)
  -> fhe.cnn.poly_activation(..., source_activation=common.relu)
```

Required tests:

| Mode | Expected result |
| --- | --- |
| `bootstrap=auto` | Compiler inserts exactly one required pre-ReLU boundary per surviving ReLU before any additional depth-driven refresh |
| `bootstrap=on` | Same mandatory baseline, with backend bootstrap capability required |
| `bootstrap=manual` | Compilation succeeds only when each surviving ReLU already has an explicit compatible boundary |
| `bootstrap=off` | Compilation rejects every surviving ReLU in the first CKKS release |

Exit evidence:

- Focused ReLU `.B` and `.T` before and after FHE/CKKS planning.
- Full ResNet depth/bootstrap report with source positions.
- Fused backend path, when present, retains the same logical evidence.
- No `-O0` movement, merging, deduplication, or profitability placement.

### **SYNC-5: Middle-WHIRL And Mock Executable Gate**

Required phase output:

```text
secure_resnet20.fhe.B
  -> SIHE/CKKS correctness planning
  -> secure_resnet20.ckks.B
  -> FHE runtime-call lowering
  -> secure_resnet20.mid.B
  -> whirl2c
  -> secure_resnet20.c
  -> mock-linked executable
```

Acceptance checks:

- `secure_resnet20.mid.T` contains standard WHIRL calls, formals, results,
  symbols, initializers, status checks, and control flow only.
- The unlowered-node verifier rejects remaining FHE/SIHE/CKKS/HPOLY nodes.
- Result ciphertext handles use caller-owned no-alias temporaries.
- Generated C includes only the stable C ABI, not OpenFHE C++ types.
- Source positions survive on generated calls and result stores.

Merge rule: assign every shared driver/whirl2c file to one PR owner before
editing. The mock-runtime PR must not depend on OpenFHE installation.

### **SYNC-6: End-To-End `-O0` Acceptance**

Target command shape:

```text
openpy -O0 -keep secure_resnet20.py \
  -dsc-fhe=cnn \
  -dsc-fhe-scheme=ckks \
  -dsc-fhe-bootstrap=auto \
  -dsc-fhe-backend=openfhe \
  -o secure_resnet20.out
```

Required evidence:

- Original, FHE, CKKS, and middle-WHIRL `.B`/`.T` pairs.
- Generated C, compile command, link command, provider manifest, and dependency
  inspection.
- Encryption configuration, key requirements, layout, depth/bootstrap,
  accuracy, memory, and runtime reports.
- Client-generated context, public/evaluation keys, encrypted input, encrypted
  output, and client-side validation.
- No secret key in the server artifact family.

Exit: the OpenFHE-linked server executable consumes encrypted CIFAR-10 input
and produces encrypted logits within the declared CKKS and model-accuracy
budgets.

### **SYNC-7: Optimized-Versus-`-O0` Proof**

Before enabling any ReSBM, HPOLY/HPAO, fusion, or refresh-boundary movement:

- Preserve the `-O0` artifact family as the correctness baseline.
- Give every optimization step an independent option switch.
- Retain phase `.B`/`.T` evidence and an optimization decision report.
- Prove source semantics, polynomial approximation error, CKKS scale/level
  legality, key availability, and result tolerance.
- Fall back to the `-O0` plan when proof or provider capability is missing.

### **SYNC-8: Separate GPU Architecture Review**

GPU work does not silently extend the OpenFHE CPU/reference milestone. Before
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
- generated C compiles and links against the stable FHE C ABI and OpenFHE
  provider;
- the server executes without Python or secret-key material;
- encrypted output validates within declared numerical and accuracy budgets;
- old binary WHIRL compatibility tests continue to pass; and
- all reviewable artifacts remain in a host-visible directory after testing.

## Immediate Coordinated Queue

1. **SYNC-0:** Commit and share the three plan documents, then reconcile the
   FHE handoff table against the current source registry.
2. Main task: publish a reuse/extension decision for every requested common and
   CNN operator, explicitly recording that `common.relu` already exists.
3. FHE task: freeze the complete ResNet-20 operator census and classify every
   value as ciphertext, encoded plaintext, clear metadata, or illegal secret
   material.
4. Joint review: decide the exact relationship between TensorDescriptorIR and
   EncryptionDescriptorIR, including deduplication and type equivalence.
5. **SYNC-1:** Freeze fixed FHE records, opaque builder APIs, image capability
   rules, printer spelling, and negative-test matrix.
6. Main task: implement and merge shared native infrastructure.
7. FHE task: rebase, bind the merged APIs, and produce the SYNC-2 ResNet capture
   artifact family.
