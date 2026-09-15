# FHE SYNC-4 Through SYNC-6 Team Handoff

Status: ready for team kickoff after this documentation PR merges

## Purpose

This document is the operating handoff for moving the correctness-first Open64
FHE path from the completed SYNC-3 planning artifact through an ACE
`FHErt_ant`-linked ResNet-20 execution. It complements the authority documents
below and converts their stage gates into team assignments, review boundaries,
commit-sized work, tests, and retained evidence.

Each SYNC point is a release gate. A stage closes only when its implementation,
negative tests, retained artifacts, and independent review evidence pass. A
merged implementation alone does not close a stage.

## Authority Set

The team must read these files from the same `develop` revision before coding:

| Document | Role |
| --- | --- |
| `doc/FHE-CONSOLIDATED-IMPLEMENTATION-PLAN.md` | Coordination authority, ownership, ordering, and joint exit criteria |
| `doc/FHE-WHIRL-INTEGRATION-PLAN.md` | FHE semantics, lowering stages, diagnostics, and artifacts |
| `doc/FHE-DSL-INTEGRATION-PLAN.md` | Complete frontend-to-runtime execution architecture |
| `doc/FHE-ACE-RTLIB-RUNTIME-DECISION.md` | Selected ACE ANT provider, ABI boundary, security scope, and runtime mapping |
| `doc/FHE-SYNC3-COMMIT19-CERTIFICATION.md` | Accepted SYNC-3 baseline and retained evidence |
| `doc/FHE-SYNC3-CONTEXT-CKKS-STATE-CONTRACT.md` | Context-specific CKKS state and callee-identity contract |

Do not copy document blob hashes into long-lived instructions. At kickoff,
record the active baseline with:

```sh
git rev-parse HEAD
git rev-parse HEAD:doc/FHE-CONSOLIDATED-IMPLEMENTATION-PLAN.md
git rev-parse HEAD:doc/FHE-WHIRL-INTEGRATION-PLAN.md
git rev-parse HEAD:doc/FHE-DSL-INTEGRATION-PLAN.md
git rev-parse HEAD:doc/FHE-ACE-RTLIB-RUNTIME-DECISION.md
```

If any authority file changes later, the program shepherd reviews its diff and
records whether assignments or acceptance criteria change.

## Starting Boundary

SYNC-3 is complete at the merged PR #131 implementation plus merged-tip
recertification recorded by the closure documentation. The accepted baseline
contains:

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
   diagnostics, or Git. The SYNC-6 local harness may own ephemeral test keys.
10. MetaKernel, ReSBM, HPOLY, GPU, profitability, and optimized refresh movement
    remain outside SYNC-4 through SYNC-6.

## SYNC-4: ReLU O0 Materialization

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

### Commit Plan

| Commit | Coding scope | Required tests and evidence |
| --- | --- | --- |
| S4-1 Contract freeze | Publish exact bootstrap, composite-activation, state-transition, effect, diagnostic, and lowering contracts; name shared-file owners | Contract review; existing WHIRL reopen; unknown version/profile negatives |
| S4-2 Provider capability gate | Define provider-independent capability manifest consumption and verify ACE ANT support for bootstrap target levels, rotations, depth, and slots | Valid pinned manifest; bad revision/hash; missing bootstrap; missing rotation; unsupported level |
| S4-3 Bootstrap materialization | Insert exactly one mandatory pre-ReLU refresh for every approved context under `auto|on`; preserve source position, reason, range, route, and state | Focused root/called contexts; exactly 19 boundaries; 16/1/2 target-level distribution; duplicate/missing state negatives |
| S4-4 Composite approximation | Materialize normalization, ordered 7/15/13 stages, and reconstruction from approved TCON bytes and hashes | Stage order, coefficient hash, depth 11, scale/level, malformed manifest, and numerical oracle tests |
| S4-5 Option modes and gate | Implement `bootstrap=auto|on|manual|off` and the post-pass semantic verifier | Positive auto/on/manual; missing manual boundary; off rejection; no surviving standalone live ReLU |
| S4-6 Full certification | Run the six-PU artifact through materialization and retain before/after evidence | `.B/.T`, phase trace, disposition/state report, command log, diagnostics, SHA-256 manifest |

Retain evidence under `artifacts/fhe/sync4-relu-o0/`.

### Exit Gate

- All 19 contexts have one approved bootstrap boundary and one complete
  composite activation.
- Logical source ReLU, approximation profile, range, bootstrap reason, source
  position, and resulting CKKS state remain inspectable.
- `auto`, `on`, valid `manual`, and negative `manual/off` behavior pass.
- The pinned `ace-ant` capability manifest admits every planned operation.
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
| S5-1 Runtime ABI v1 | Publish opaque context/key/plaintext/ciphertext handles, status values, ownership, lifetime, cleanup, and version negotiation | C and C++ ABI compile tests; size/visibility checks; ownership and double-free negatives |
| S5-2 Standard-call lowering | Lower FHE/SIHE/CKKS constructs to normal `OPR_CALL`, symbols, result stores, checks, and control flow | Focused operation `.B/.T`; source positions; exact TY/formal/result checks |
| S5-3 Unlowered-node gate | Reject every remaining FHE/SIHE/CKKS logical node before `whirl2c` | One retained-node negative per layer; stable diagnostic; no generated C on failure |
| S5-4 Mock provider | Implement deterministic semantics and failure injection behind ABI v1 | Add/mul/rotate/relinearize/rescale/bootstrap/activation contracts; cleanup and error propagation |
| S5-5 Generated-C boundary | Teach build/driver flow to compile and link `whirl2c` output with the mock | Generated C contains no ACE/OpenFHE types; dependency inspection; ordinary WHIRL regression |
| S5-6 Full mock certification | Run complete ResNet-20 through standard WHIRL and mock executable | `.ckks.B/.T`, `.mid.B/.T`, C, executable, logs, ABI manifest, hashes |

Retain evidence under `artifacts/fhe/sync5-middle-whirl/`.

### Exit Gate

- `secure_resnet20.mid.B` contains standard WHIRL only.
- `whirl2c` output compiles and links without ACE or OpenFHE installed.
- The mock executable validates call order, ownership, status, and cleanup.
- A deliberately retained custom node fails before C emission.
- ABI v1 is frozen before the ACE adapter is accepted.

## SYNC-6: ACE ANT O0 Functional Acceptance

### Objective

Keep the ABI and generated C unchanged, replace the mock with the ACE ANT
provider, and execute the complete encrypted ResNet-20/CIFAR-10 path.

```text
secure_resnet20.c
  -> libopen64_fhe_runtime
  -> libopen64_fhe_ace_ant
  -> pinned FHErt_ant
  -> local encrypted inference harness
```

SYNC-6 is functional and numerical acceptance for a local harness. Production
client/server secret-key separation is a later security milestone.

### Commit Plan

| Commit | Coding scope | Required tests and evidence |
| --- | --- | --- |
| S6-1 Provider manifest/build | Pin ACE revision, source hash, compiler ABI, options, dependencies, CKKS parameters, capabilities, rotations, notices, and licenses | Reproducible build; manifest hash; revision mismatch; missing dependency/capability negatives |
| S6-2 ACE adapter core | Map context, input/output, add, multiply, rotate, relinearize, rescale, and status handling to `FHErt_ant` | Focused ABI/provider pairs; ACE exception containment; handle/lifetime tests |
| S6-3 Bootstrap and activation | Map exact post-bootstrap levels and compiler-materialized 7/15/13 activation sequence | 19 bootstrap calls; levels 15x16, 17x1, 18x2; coefficient/range/state checks |
| S6-4 CNN metakernels | Implement or reuse reviewed rotate/multiply/add schedules for conv, linear, residual add, pooling, flatten, and logits | Focused clear/CKKS comparisons; rotation-key coverage; layout and payload checksum negatives |
| S6-5 Driver/link integration | Select `ace-ant`, consume the authenticated provider manifest, and link the unchanged generated C | Link/dependency report; no ACE symbols in generated C ABI; provider mismatch and stale-manifest rejection |
| S6-6 Full functional certification | Execute pinned ResNet-20 with the local encrypted harness and compare with certified baselines | Accuracy/error, operation counts, bootstrap distribution, memory, latency, precision, cleanup, and complete artifact family |

Retain evidence under `artifacts/fhe/sync6-ace-ant-o0/`.

### Required ACE Mapping

| Open64 service | ACE ANT service |
| --- | --- |
| Context lifecycle | `Prepare_context`, `Finalize_context` |
| Input/output | `Prepare_input`, ACE data services, local `Handle_output` |
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
- Runtime evidence contains exactly 19 bootstrap calls with the approved level
  distribution.
- Ciphertext results meet the predeclared numerical and accuracy thresholds.
- Failure tests leave no valid-looking final or temporary artifacts.
- Public artifacts contain no secret-key bytes.
- The completion claim says ACE ANT local functional execution, not direct
  OpenFHE execution or production server key separation.

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
2. Run the deployability check below and attach its output to the kickoff issue.
3. Record the baseline commit and authority-document blob IDs.
4. Assign every team role, reviewer, and shared file.
5. Create a SYNC-4 board with one item for each S4 commit and explicit
   dependency arrows.
6. Open the S4-1 contract PR first. Do not begin dependent implementation until
   its shared contracts are merged.
7. Reserve host-visible artifact roots for positive and negative runs.
8. Schedule independent review checkpoints after S4-2, S4-5, and S4-6.

## Deployability Check

Run from the Open64 repository root after checking out the handoff baseline:

```sh
set -eu

required_docs="
doc/FHE-CONSOLIDATED-IMPLEMENTATION-PLAN.md
doc/FHE-WHIRL-INTEGRATION-PLAN.md
doc/FHE-DSL-INTEGRATION-PLAN.md
doc/FHE-ACE-RTLIB-RUNTIME-DECISION.md
doc/FHE-SYNC3-COMMIT19-CERTIFICATION.md
doc/FHE-SYNC3-CONTEXT-CKKS-STATE-CONTRACT.md
doc/FHE-SYNC4-TO-SYNC6-TEAM-HANDOFF.md
"

for path in $required_docs; do
  test -f "$path"
done

rg -q "fb76131171b9f82aa6387f84dd73684fba5277e8" \
  doc/FHE-ACE-RTLIB-RUNTIME-DECISION.md
rg -q "FHErt_ant" doc/FHE-ACE-RTLIB-RUNTIME-DECISION.md
rg -q "sync6-ace-ant-o0" doc/FHE-SYNC4-TO-SYNC6-TEAM-HANDOFF.md
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

For the local SYNC-3 retained evidence, run `sha256sum -c SHA256SUMS` from its
artifact directory and inspect `secure_resnet20.fhe.T` and the conversion
report. A new team does not need the original local path; the shepherd must
publish or regenerate an access-controlled evidence bundle and record its
location in the kickoff issue.

## First Kickoff Record

The kickoff issue or meeting note must contain:

- Open64 baseline commit and authority-document blob IDs;
- pinned ACE revision and checkout location;
- named owners and independent reviewers;
- shared-file ownership table;
- S4-1 through S4-6 branch and PR sequence;
- artifact roots and retention owner;
- provider manifest owner and license reviewer;
- declared option modes, numerical thresholds, and stop conditions; and
- links to the SYNC-3 evidence bundle and the first S4-1 contract review.

The team is ready to code only after this record is complete and the S4-1
shared contract has an assigned main/common owner.
