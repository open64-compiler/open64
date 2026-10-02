# FHE SYNC-4 Through SYNC-6 Team Handoff

Status: the 2026-09-29 exact-snapshot SYNC-3 re-certification passed, and
SYNC-4 S4-1 through S4-6 are implementation-complete and locally certified.
Independent review and merge close SYNC-4; the next implementation gate is
SYNC-5 standard-WHIRL runtime-call lowering.

Current machine-readable stage-entry gate:

```text
SYNC3_CURRENT_VERIFICATION=VERIFIED_EXACT_SNAPSHOT
SYNC4_CONSUMPTION_BASIS=VERIFIED_2026_09_29_EXACT_SNAPSHOT
SYNC4_MAY_CONSUME_SYNC3=true
SYNC4_IMPLEMENTATION_STATUS=COMPLETE_LOCAL_CERTIFICATION
SYNC5_MAY_BEGIN_AFTER_SYNC4_REVIEW=true
```

These fields are the automation-facing state for this revision. They are bound
to the retained evidence and hashes in
`doc/FHE-SYNC3-COMMIT19-CERTIFICATION.md`; changing that evidence requires a
new atomic verification-state update.

## Purpose

This document is the operating handoff for moving the correctness-first Open64
FHE path from the historically certified SYNC-3 planning implementation through
an ACE `FHErt_ant`-linked ResNet-20 execution. It complements the authority
documents below and converts their stage gates into team assignments, review
boundaries, commit-sized work, tests, and retained evidence.

Each SYNC point is a release gate. A stage closes only when its implementation,
negative tests, retained artifacts, and independent review evidence pass. A
merged implementation alone does not close a stage.

## Authority Set

The team must read these files from the same `develop` revision before coding:

| Document | Role |
| --- | --- |
| `doc/DSC_FHE_Compiler_Architecture_and_Integration_Plan_v0.10.docx` | Sole highest FHE semantic authority; required SHA-256 `0018769C26B5A0BCD1BDFCBD85AA97B8BAFEA381D7640FBB9E2E81B0022013D9` |
| `doc/FHE-CONSOLIDATED-IMPLEMENTATION-PLAN.md` | Coordination authority, ownership, ordering, and joint exit criteria |
| `doc/FHE-WHIRL-INTEGRATION-PLAN.md` | FHE semantics, lowering stages, diagnostics, and artifacts |
| `doc/FHE-DSL-INTEGRATION-PLAN.md` | Complete frontend-to-runtime execution architecture |
| `doc/FHE-ACE-RTLIB-RUNTIME-DECISION.md` | Selected ACE ANT provider, ABI boundary, security scope, and runtime mapping |
| `doc/FHE-RUNTIME-C-ABI-V1-CONTRACT.md` | Sole public runtime ABI, schedule, transport, ownership, and failure contract for SYNC-5 and SYNC-6 |
| `doc/FHE-SYNC3-COMMIT19-CERTIFICATION.md` | Historical SYNC-3 Pass record and current re-certification requirements |
| `doc/FHE-SYNC3-CONTEXT-CKKS-STATE-CONTRACT.md` | Context-specific CKKS state and callee-identity contract |

At kickoff, verify the fixed v0.10 content hash above and record the active Git
baseline and subordinate-document blob IDs with:

```sh
git rev-parse HEAD
git rev-parse HEAD:doc/DSC_FHE_Compiler_Architecture_and_Integration_Plan_v0.10.docx
git rev-parse HEAD:doc/FHE-CONSOLIDATED-IMPLEMENTATION-PLAN.md
git rev-parse HEAD:doc/FHE-WHIRL-INTEGRATION-PLAN.md
git rev-parse HEAD:doc/FHE-DSL-INTEGRATION-PLAN.md
git rev-parse HEAD:doc/FHE-ACE-RTLIB-RUNTIME-DECISION.md
```

If any authority file changes later, the program shepherd reviews its diff and
records whether assignments or acceptance criteria change.

## Starting Boundary

SYNC-3 implementation is merged through PR #131. Its 2026-09-14 merged-tip Pass
record reports the following historical results; they were not rerun for this
documentation revision:

- six class-centric PUs;
- 13 physical Conv/BatchNorm definition rewrites;
- 21 call-context folds and 42 converted parameter tensors;
- 46 source and 46 converted dispositions;
- 19 authenticated ReLU ranges;
- 19 callee-tagged `POST_REFRESH.v1` context states;
- post-refresh levels 15, 17, and 18 with the approved 16/1/2 distribution;
- clear accuracy 91.6%, polynomial accuracy 91.5%, 0.1 percentage-point
  degradation, and 99.9% prediction agreement; and
- atomic `.fhe.B`, converted payload, report, diagnostics, and independent
  `ir_b2a -st -src` evidence.

This is conversion-planning evidence. It does not claim bootstrap
materialization, standard-WHIRL runtime-call lowering, generated C, or
`FHErt_ant` execution.

The current exact-snapshot certification is retained at
`/private/tmp/open64-fhe-sync3-recertification-20260929/current-snapshot-certification`.
Its source `.B`, converted `.fhe.B`, source-interleaved `.T` files, converted
payload, report, commands, diagnostics, negative logs, and `SHA256SUMS` passed
the repository's independent verifier and atomic-publication checks. The
source and parameter payload hashes match the approved model evidence; the
range manifest is rebound to the current additive-metadata `.B` snapshot.

The first executable provider is pinned ACE ANT `FHErt_ant` at commit
`fb76131171b9f82aa6387f84dd73684fba5277e8`. OpenFHE remains a later optional
provider and must not change WHIRL, generated C, or the public runtime ABI.

## Team Roles

Assign a named person and reviewer to every row before implementation starts.

| Role | Accountable work |
| --- | --- |
| Program shepherd | Stage board, dependencies, decisions, risks, evidence index, and final sign-off |
| Main/common owner | Shared operator/type contracts, mapped images, readers, writers, printers, and generic gates |
| FHE semantic owner | Bootstrap/approximation semantics, CKKS policy, conversion, and CFHE diagnostics |
| Lowering owner | FHE/SIHE/CKKS materialization and lowering to standard WHIRL |
| Runtime ABI owner | Versioned opaque C ABI, ownership, status, cleanup, and mock provider |
| ACE provider owner | Adapter from the Open64 ABI to pinned `FHErt_ant`, capability manifest, build, and licenses |
| Build/tool owner | Driver options, `whirl2c`, generated-C compilation, link flow, and dependency checks |
| Certification owner | Independent artifact, numerical, compatibility, diagnostic, and security-boundary review |

The author and certification reviewer for a commit must be different people.
Publish a shared-file ownership table before any shared file is edited.

## Non-Negotiable Rules

1. Preserve binary WHIRL compatibility and the private physical `OPR_DSL`
   abstraction.
2. Preserve `common.relu` as source semantics. Bootstrap restores CKKS
   capacity; it does not compute ReLU.
3. Keep canonical tensor type identity separate from value- and
   context-specific CKKS state.
4. Use reviewed opaque APIs for persistent image changes. FHE code must not
   mutate WN, ST, TY, or mapped-image internals directly.
5. Merge main/common prerequisites before dependent FHE work, then rebase and
   remove duplicate patches.
6. Generated C includes only the versioned Open64 C ABI. ACE C++ types, headers,
   keys, evaluator objects, and exceptions stay inside the provider adapter.
7. The provider is selected by build/runtime configuration and its authenticated
   manifest, never by changing source WHIRL semantics.
8. Failed runs publish no final artifact and leave no misleading temporary
   artifact.
9. Secret keys never enter WHIRL, generated model C, public compiler artifacts,
   diagnostics, Git, the server broker, or an ACE worker. A separate client or
   provisioner owns key generation, input encryption, output decryption, and
   secret-key retention. An embedded ACE harness is a non-gating diagnostic
   lane only.
10. MetaKernel, ReSBM, HPOLY, GPU, profitability, and optimized refresh movement
    remain outside SYNC-4 through SYNC-6.

## SYNC-4: ReLU O0 Materialization

Entry gate: closed by the retained 2026-09-29 exact-snapshot SYNC-3
certification. The exact S4-1 semantic proposal is
`doc/FHE-SYNC4-RELU-MATERIALIZATION-CONTRACT.md`; main/common accepted its
semantic direction. The shared physical image/API implementation and FHE
semantic consumer are complete on the SYNC-4 branch.

### Objective

Materialize the deterministic baseline:

```text
common.relu(x)
  -> mandatory pre-ReLU bootstrap
  -> normalize by identity-bound B
  -> ACE Chebyshev sign stages 7 -> 15 -> 13
  -> reconstruct 0.5*x*sign(x/B) + 0.5*x
```

No refresh movement, merging, deduplication, or profitability decision is
allowed at `-O0`.

The accepted S4-1 contract is
`doc/FHE-SYNC4-RELU-MATERIALIZATION-CONTRACT.md`. Main/common owns its fixed
image, managed APIs, reader/writer, inspection, and driver/checkpoint hooks.
The FHE-owned S4-2 static provider package is
`doc/fhe-policy/sync4-relu/package-index.json`; native exact-byte
authentication and all-PU consumption use the S4-1 driver hook.

### Commit Plan

| Commit | Coding scope | Required tests and evidence |
| --- | --- | --- |
| S4-1 Contract freeze | **Complete.** Publish exact bootstrap, composite-activation, state-transition, effect, diagnostic, and lowering contracts; name shared-file owners | Contract, mapped-image, malformed-image, old-reader, and reopen review passed |
| S4-2 ReLU-subset capability gate | **Complete.** Authenticate the materialized ReLU/bootstrap subset: target levels, slots, depth, normalization, and ordered 7/15/13 arithmetic | Pinned subset and revision/hash/bootstrap/primitive/level/slot/depth negatives passed |
| S4-3 Bootstrap materialization | **Complete.** Insert exactly one mandatory pre-ReLU refresh per approved context under `auto|on`; preserve reason, range, route, and state | Exactly 19 boundaries and `15:16,17:1,18:2` distribution reopen |
| S4-4 Composite approximation | **Complete.** Materialize normalization, ordered 7/15/13 stages, and reconstruction from approved TCON bytes and hashes | Exactly 114 rows; stage/hash/depth/state checks and independent numerical oracle passed |
| S4-5 Option modes and gate | **Complete.** Enforce `bootstrap=auto|on|manual|off` and exact final semantic verification | Auto/on/manual passed; missing-manual/off/hash failures published no partial output |
| S4-6 Full certification | **Complete locally.** Run the six-PU artifact through materialization and retain before/after evidence | `.B/.T`, source, reports, phase trace, commands, diagnostics, and SHA-256 manifest retained for review |

Retain evidence under `artifacts/fhe/sync4-relu-o0/`.

The FHE-owned S4-2 static provider package is
`doc/fhe-policy/sync4-relu/package-index.json`. Native authenticated option
transport and all-PU consumption are implemented through the reviewed S4-1
main/common hook.

SYNC-4 retained evidence is `/private/tmp/open64-fhe-sync4-final`. The
independent certifier is
`osprey/torch2whirl/python/tests/fhe_sync4_certification.py`. This evidence
closes materialization planning, not standard-WHIRL runtime lowering or ACE
execution.

### Exit Gate

- All 19 contexts have one approved bootstrap boundary and one complete
  composite activation.
- Logical source ReLU, approximation profile, range, bootstrap reason, source
  position, and resulting CKKS state remain inspectable.
- `auto`, `on`, valid `manual`, and negative `manual/off` behavior pass.
- The pinned `ace-ant` subset manifest admits every materialized
  ReLU/bootstrap operation. It does not claim complete-model admission.
- No provider-specific type or call has entered logical WHIRL.

Stop and request a main/common contract if an opcode, descriptor, context
query, rewrite, printer, or mapped-image service is missing.

## SYNC-5: Standard WHIRL And Mock Executable

### Objective

Lower the accepted plan to standard WHIRL and prove generated C against a mock
implementation of the stable ABI:

```text
secure_resnet20.fhe.B
  -> secure_resnet20.ckks.B
  -> runtime-call lowering
  -> secure_resnet20.mid.B
  -> whirl2c
  -> secure_resnet20.c
  -> mock-linked executable
```

### Commit Plan

| Commit | Coding scope | Required tests and evidence |
| --- | --- | --- |
| S5-1 Runtime ABI v1 implementation | Implement the public header and support code from the sole normative `FHE-RUNTIME-C-ABI-V1-CONTRACT.md`; do not create a second schema | C and C++ ABI compile tests; version/profile hashes; size/visibility checks; ownership and double-free negatives |
| S5-2a Propagation admission gate | Re-run/consume certified shape facts and context-sensitive FHE CKKS state after materialization; select one exact runtime role for every live value without changing canonical tensor identity | Missing/pending/conflicting class, descriptor, layout, level, scale, component, precision, slot, call-context, and pending-action negatives |
| S5-2b Runtime-interface projection | Persist the owner-qualified relation from unchanged source DSL values to exact ciphertext/plaintext handle TY/ST; migrate PU inputs, hidden results, caller actuals/results, prototypes, and return stores through the reviewed generic transaction | Two-PU and six-PU exact-TY tests; local ST collision; source positions; wrong owner/ordinal/class; partial-PU abort; mapped reopen and `ir_b2a -st -src` evidence |
| S5-2c Standard-call lowering | Lower the complete admitted FHE/SIHE/CKKS model to normal `OPR_CALL`, result stores, descriptors, checks, cleanup, and control flow using projected handles | Focused and full-model `.B/.T`; exact handle/formal/result/descriptor checks; canonical tensor rows retained only as non-executable provenance |
| S5-3 Unlowered-node gate | Reject every remaining FHE/SIHE/CKKS logical node before `whirl2c` | One retained-node negative per layer; stable diagnostic; no generated C on failure |
| S5-4 Mock provider | Implement deterministic semantics and failure injection behind ABI v1, including context/key/ciphertext envelope import/export and launcher/broker behavior | Full operation set; ownership, alias, transport, retry, cleanup, and worker-termination simulation |
| S5-5 Generated-C boundary | Teach build/driver flow to compile and link `whirl2c` output with the mock | Generated C contains no ACE/OpenFHE types; dependency inspection; ordinary WHIRL regression |
| S5-6 Full schedule and mock certification | Run complete ResNet-20 and freeze the successful call census, execution-expanded semantic schedule, operation descriptors, signed rotations, key requirements, complete provider-capability manifest, and separate lifecycle/failure transcripts | `.ckks.B/.T`, `.mid.B/.T`, generated C, mock executable, manifests, transcripts, logs, hashes, and independent census recomputation |

Post-PR #156 checkpoint: S5-1 and the standalone S5-4 ABI/mock contract are
merged, and the generic program-interface transaction now covers dead
canonical ABI pruning, root entry-parameter promotion, and explicit
model/coefficient resource threading. The FHE consumer resolves exact
projected and role-qualified handles and has certified detached
descriptor-select plus bootstrap standard-call construction.

PR #157 merged the reviewed atomic native-value lowering transaction. The FHE
consumer has certified one complete source-ReLU replacement: six selectors,
refresh, normalization, three polynomial stages, reconstruction, and a final
store to the exact projected output handle, with logical rows retained as
lowered provenance. S5-2c now proceeds as FHE-owned full-model operation
coverage, all-PU callback registration, exact census verification, and atomic
artifact publication. Do not substitute raw WN/image edits, globals,
uninitialized locals, name parsing, or projected dead BN parameters. The
exact history and current boundary are in
`FHE-SYNC5-RUNTIME-ENTRY-BINDING-GAP.md`.

Retain evidence under `artifacts/fhe/sync5-middle-whirl/`.

### Exit Gate

- `secure_resnet20.mid.B` contains standard WHIRL only.
- Shape/FHE-state propagation admits every projected value, and the runtime
  interface distinguishes exact ciphertext and plaintext handle types without
  mutating canonical TensorDescriptorIR/TY identity.
- `whirl2c` output compiles and links without ACE or OpenFHE installed.
- The mock executable validates call order, ownership, status, and cleanup.
- A deliberately retained custom node fails before C emission.
- ABI v1 and the complete full-model schedule/capability/key manifest are
  frozen before the ACE adapter is admitted.
- SYNC-6 must consume these artifacts exactly; it cannot infer a replacement
  schedule from provider capabilities.

## SYNC-6: ACE ANT O0 Client/Server Acceptance

### Objective

First create and certify the provider-independent executable CKKS semantic IR
described in `doc/FHE-SYNC6-CKKS-IR-CONFORMANCE-GATE.md`. Only after that IR
passes its mapped-image, state, source, and mock-lowering gates does SYNC-6
admit an ACE provider. Keep the stable ABI and certified SYNC-5 reference
artifacts unchanged; then replace the mock with the admitted ACE ANT provider
and execute the complete encrypted ResNet-20/CIFAR-10 path across the required
client/server trust boundary.

```text
FHE-CNN and context-sensitive materialization
  -> provider-independent executable CKKS WHIRL (.ckks_ops.B)
  -> final CKKS semantic gate and standard-call lowering
  -> secure_resnet20.c
  -> Open64 FHE ABI v1 broker
  -> one supervised worker per imported public context
  -> libopen64_fhe_ace_ant
  -> pinned FHErt_ant evaluation-only context
  -> ciphertext output envelope
  -> separate client validation
```

A separate client or provisioner owns the secret key, context/key provisioning,
input encryption, output import, and decryption. The server imports only the
versioned public context, evaluation/relinearization/rotation/bootstrap keys,
plaintext model assets, and ciphertext input. The broker and worker must have
no key-generation, secret-key import, or decrypting dependency. A local
embedded harness remains useful for bring-up but cannot close SYNC-6.

### Commit Plan

| Commit | Coding scope | Required tests and evidence |
| --- | --- | --- |
| S6-0a-d CKKS semantic IR | Review shared operator/image contracts; build opaque producer, full six-PU CKKS expansion, state verifier, mapped `.ckks_ops.B`/`.T`, and unchanged ABI/mock terminal lowering | Complete high-level-to-CKKS event map, 19 explicit refreshes, level/scale/key/rotation proof, old-reader/malformed tests, source-interleaved reopen, mock equivalence; details in the CKKS IR conformance gate |
| S6-0e ACE-shaped development mock | Build the Open64-owned ACE arithmetic adapter against a separate local implementation of the pinned ACE call shapes, without ACE headers or `FHErt_ant` in the development test | Exact function-shape checks, operation/ownership/error tests, no secret/dataset helper symbols, and later generated-C coverage through the stable ABI; this cannot replace the public-ABI mock or provider admission |
| S6-1 Exact-provider admission | Pin ACE revision, source/patch hash, compiler ABI, options, dependencies, licenses, and prove evaluation-only public-context, keyset, ciphertext import/export plus every frozen SYNC-5 capability | Reproducible build; field-for-field manifest comparison; revision/patch mismatch; missing import/export, operation, rotation, key, or level negatives |
| S6-2 Broker and supervised worker | Implement the ABI broker and one isolated ACE worker per public context; serialize calls within a context and expose no ACE object across IPC | Multiple-context isolation; launcher authorization; worker identity; no ACE symbols in generated C or broker |
| S6-3 Client provisioning and transport | Implement versioned public-context, non-secret keyset, plaintext-model, ciphertext input, and ciphertext output envelopes with authenticated session binding | Client/server roundtrip; digest/config/session mismatch; secret-key-class rejection before worker dispatch |
| S6-4 ACE schedule execution | Map the frozen SYNC-5 descriptors and schedule to ACE add, multiply, rotate, relinearize, rescale, bootstrap, ReLU stages, conv, residual, pool, flatten, linear, and logits operations | Full census equality; 19 bootstrap calls at levels 15x16, 17x1, 18x2; coefficient/range/layout/state checks |
| S6-5 Failure containment | Translate recoverable errors and fatal ACE assertion/abort/signal/IPC loss through ABI v1 without partial output; poison and reap failed contexts | Input preservation; cursor rollback; poisoned-handle cleanup; child termination/status translation; no silent replay |
| S6-6 Full client/server certification | Execute pinned ResNet-20 with the secretless server and compare client-decrypted results with certified baselines | Accuracy/error, operation counts, bootstrap distribution, memory, latency, precision, dependency closure, no-secret evidence, and complete artifact family |

The S6-1 exact-pin probe and its fail-closed result are recorded in
`doc/FHE-SYNC6-ACE-ANT-ADMISSION-AUDIT.md`. Evaluation-only context/key import
and ciphertext transport are not yet demonstrated. That blocks ACE provider
admission and S6-2 through S6-6, **not** S6-0 CKKS IR creation. The probe is
not a full S6-1 build/capability pass. Do not interact with the ACE library
from the compiler or generated program until S6-0 has certified its IR.
The separately tested ACE-shaped development mock is specified in
`doc/FHE-SYNC6-ACE-SHAPED-MOCK.md`; it removes ACE build/link requirements
from adapter development but does not change the final admission gate.

Retain evidence under `artifacts/fhe/sync6-ace-ant-client-server-o0/`.

### Required ACE Mapping

| Open64 service | ACE ANT service |
| --- | --- |
| Public context lifecycle | Import/reconstruct an authenticated evaluation-only context in one supervised worker; no provider key generation |
| Keyset import | Import only authenticated evaluation, relinearization, signed-rotation, and bootstrap material required by the frozen SYNC-5 manifest |
| Ciphertext input/output | Import/export versioned ciphertext envelopes without decrypting; `Prepare_input` and `Handle_output` are client-side diagnostic helpers only |
| Add | `Add_ciph`, `Add_plain`, scalar form as required |
| Multiply | `Mul_ciph`, `Mul_plain`, scalar form as required |
| Rotate | `Rotate_ciph` |
| Relinearize | `Relin` |
| Rescale/level change | `Rescale_ciph` and reviewed ACE level services |
| Bootstrap | `Bootstrap(result, input, level_after_bts)` |
| Conv/linear/pool | Compiler-selected rotate/multiply/add schedules |

### Exit Gate

- The exact pinned ACE source and provider manifest are reproducible.
- The generated C and ABI are unchanged from the SYNC-5 provider-independent
  boundary.
- The ACE provider passes every frozen SYNC-5 capability, key, rotation,
  schedule, import, and export requirement before execution.
- Runtime evidence contains exactly 19 bootstrap calls with the approved level
  distribution.
- Ciphertext results meet the predeclared numerical and accuracy thresholds.
- Failure tests leave no valid-looking final or temporary artifacts.
- The broker, worker, generated C, dependencies, runtime state, logs, and
  public artifacts contain no secret key or secret-key API dependency.
- The completion claim says ACE ANT client/server execution, not direct
  OpenFHE execution. An embedded local harness is separately labeled
  diagnostic and non-gating.

## PR And Rebase Order

Use infrastructure-first ordering inside every stage:

1. Contract or generic main/common PR.
2. Merge into `develop`.
3. Rebase the FHE semantic/lowering branch and remove duplicate work.
4. FHE implementation PR.
5. Merge into `develop`.
6. Rebase runtime/build work.
7. Runtime/provider PR.
8. Independent merged-tip certification and stage-close documentation PR.

Do not stack a dependent implementation on an unmerged shared-contract branch.

## Kickoff Procedure

1. Merge the documentation authority PR and start from a clean `develop`.
2. Publish or regenerate the complete SYNC-3 evidence bundle, run current
   independent re-certification, and record a Pass before implementation
   kickoff.
3. Run the deployability check below and attach its output to the kickoff issue.
4. Record the baseline commit and authority-document blob IDs.
5. Assign every team role, reviewer, and shared file.
6. Record S4-1 through S4-6 as locally complete and attach the retained
   certification family to the independent review.
7. After the reviewed SYNC-4 branch merges and merged-tip certification passes,
   create the SYNC-5 board and open its standard-WHIRL/runtime-ABI contract PR
   before dependent lowering implementation.
8. Reserve host-visible artifact roots for positive and negative runs.
9. Schedule independent review checkpoints at the SYNC-5 mock-executable gate
   and before SYNC-6 ACE provider execution.

## Deployability Check

Run from the Open64 repository root after checking out the handoff baseline:

```sh
set -eu

required_docs="
doc/DSC_FHE_Compiler_Architecture_and_Integration_Plan_v0.10.docx
doc/FHE-CONSOLIDATED-IMPLEMENTATION-PLAN.md
doc/FHE-WHIRL-INTEGRATION-PLAN.md
doc/FHE-DSL-INTEGRATION-PLAN.md
doc/FHE-ACE-RTLIB-RUNTIME-DECISION.md
doc/FHE-RUNTIME-C-ABI-V1-CONTRACT.md
doc/FHE-SYNC3-COMMIT19-CERTIFICATION.md
doc/FHE-SYNC3-CONTEXT-CKKS-STATE-CONTRACT.md
doc/FHE-SYNC4-RELU-MATERIALIZATION-CONTRACT.md
doc/FHE-SYNC4-TO-SYNC6-TEAM-HANDOFF.md
"

for path in $required_docs; do
  test -f "$path"
done

test "$(sha256sum doc/DSC_FHE_Compiler_Architecture_and_Integration_Plan_v0.10.docx | cut -d' ' -f1)" = \
  0018769c26b5a0bcd1bdfcbd85aa97b8bafea381d7640fbb9e2e81b0022013d9

rg -q "fb76131171b9f82aa6387f84dd73684fba5277e8" \
  doc/FHE-ACE-RTLIB-RUNTIME-DECISION.md
rg -q "FHErt_ant" doc/FHE-ACE-RTLIB-RUNTIME-DECISION.md
rg -q "sync6-ace-ant-client-server-o0" doc/FHE-SYNC4-TO-SYNC6-TEAM-HANDOFF.md
rg -q "SYNC4_CONSUMPTION_BASIS=VERIFIED_2026_09_29_EXACT_SNAPSHOT" \
  doc/FHE-SYNC4-TO-SYNC6-TEAM-HANDOFF.md
rg -q "SYNC4_MAY_CONSUME_SYNC3=true" \
  doc/FHE-SYNC4-TO-SYNC6-TEAM-HANDOFF.md
rg -q "SYNC4_IMPLEMENTATION_STATUS=COMPLETE_LOCAL_CERTIFICATION" \
  doc/FHE-SYNC4-TO-SYNC6-TEAM-HANDOFF.md
rg -q "SYNC5_MAY_BEGIN_AFTER_SYNC4_REVIEW=true" \
  doc/FHE-SYNC4-TO-SYNC6-TEAM-HANDOFF.md
rg -q "evaluation-only" doc/FHE-SYNC4-TO-SYNC6-TEAM-HANDOFF.md
rg -q "supervised worker" doc/FHE-SYNC4-TO-SYNC6-TEAM-HANDOFF.md
! rg -n "^## SYNC-6: End-To-End OpenFHE|^Retain evidence under .*sync6-openfhe-o0" \
  doc/FHE-SYNC4-TO-SYNC6-TEAM-HANDOFF.md \
  doc/FHE-CONSOLIDATED-IMPLEMENTATION-PLAN.md \
  doc/FHE-WHIRL-INTEGRATION-PLAN.md \
  doc/FHE-DSL-INTEGRATION-PLAN.md

git diff --check
```

Also verify the pinned ACE checkout separately:

```sh
test "$(git -C /path/to/ace-compiler rev-parse HEAD)" =   fb76131171b9f82aa6387f84dd73684fba5277e8

generated=/path/to/ace-compiler/fhe-cmplr/rtlib/ant/dataset/resnet20_cifar10_pre.onnx.inc
api=/path/to/ace-compiler/fhe-cmplr/rtlib/include/rt_ant/rt_ant.h
test -f "$api"
test "$(rg -c 'Bootstrap\(' "$generated")" = 19
test "$(rg -c 'Bootstrap\([^,]+,[^,]+, 15\)' "$generated")" = 16
test "$(rg -c 'Bootstrap\([^,]+,[^,]+, 17\)' "$generated")" = 1
test "$(rg -c 'Bootstrap\([^,]+,[^,]+, 18\)' "$generated")" = 2
```

The current evidence locator is
`/private/tmp/open64-fhe-sync3-recertification-20260929/current-snapshot-certification`.
Run `sha256sum -c SHA256SUMS` from that directory and inspect
`secure_resnet20.fhe.T` and the conversion report before kickoff. If the bundle
is moved, preserve the complete family and publish its new content-addressed
location; a digest list without the bytes is not sufficient.

## First Kickoff Record

The kickoff issue or meeting note must contain:

- Open64 baseline commit and authority-document blob IDs;
- pinned ACE revision and checkout location;
- named owners and independent reviewers;
- shared-file ownership table;
- merged SYNC-4 revision and S4-1 through S4-6 certification evidence;
- artifact roots and retention owner;
- provider manifest owner and license reviewer;
- explicit `SYNC4_CONSUMPTION_BASIS=VERIFIED_2026_09_29_EXACT_SNAPSHOT`;
- complete SYNC-5 schedule/call-census/capability/key manifest ownership;
- client, broker, supervised-worker, and no-secret-server owners;
- declared option modes, numerical thresholds, and stop conditions; and
- links to the SYNC-3 and SYNC-4 evidence bundles and the first SYNC-5
  contract review.

The team is ready to begin SYNC-5 after the SYNC-4 implementation and retained
evidence are independently reviewed, merged-tip certification passes, and the
SYNC-5 shared contracts have named owners.
