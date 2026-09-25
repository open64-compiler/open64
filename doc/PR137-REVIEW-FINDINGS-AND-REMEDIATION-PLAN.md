# PR 137 Review Findings and Remediation Plan

## Document Status

This document records the findings from the frozen review of PR 137 and the
planned repair sequence at repair baseline
`6eada12d4a78623726711a2922642febf231174d`. The frozen findings and
per-commit verdicts remain anchored at reviewed head
`7cac0d86b6548805b53c05675ffb7e1dff9395fa`. They are not retroactively
changed by later repair-branch work.

At this baseline, BF-01 is implemented in `6eada12d...`, but its required
reference-lane validation is pending. No WP0-WP10 semantic repair commit has
landed, and every live or unverified semantic finding remains open. The
authority freeze, semantic repairs, exact-snapshot validation, document
reconciliation, and aggregate certification below are planned work.

This file is a normalized design-first baseline for local history
reconstruction. Earlier SHA256 witnesses establish that repair plans existed
before semantic implementation, but this file is not claimed to be a
byte-for-byte recovery of either witnessed file revision.

Current recommendation: do not approve PR 137 yet.

## 1. Frozen Scope and Pull Request Snapshot

The reviewed Git range is:

```text
590bdf3d58944cb93aafd89480a5590a863ded60..7cac0d86b6548805b53c05675ffb7e1dff9395fa
```

The base is excluded and the head is included. The range contains exactly 17
commits in one linear first-parent chain and no merge commits.

The following GitHub state is a dated snapshot observed on 2026-09-23. It is
not a statement about the current live PR state.

| Field | Observed value |
| --- | --- |
| PR | `open64-compiler/open64#137` |
| State | Open, non-draft, unmerged |
| Base | `develop` at `590bdf3d58944cb93aafd89480a5590a863ded60` |
| Head | `codex/dsl-shape-sp9-certification` at `7cac0d86b6548805b53c05675ffb7e1dff9395fa` |
| Merge mechanics | `mergeable=true`, `mergeable_state=clean`, `rebaseable=true` |
| Size | 17 commits, 42 files, 7,364 additions, 159 deletions |
| Reviews | None reported |
| Requested reviewers | None reported |
| Status contexts | None reported |
| Check runs | None reported |

Clean merge mechanics do not establish semantic correctness, compatibility,
or completion of the certification gates.

## 2. What the Pull Request Implements

PR 137 attempts to build a compiler-owned, per-PU tensor shape propagation
pipeline:

```text
frontend seed descriptors
  -> shared versioned shape rules
  -> per-PU check-only fixed-point analysis
  -> immutable canonical tensor type interning
  -> owner-scoped atomic WN, ST, and managed-value retyping
  -> strict verification
  -> invalidation and revalidation around shape-affecting phases
  -> stale-state rejection before DSL lowering
```

The work covers these major capabilities:

1. Immutable lookup-before-create interning for canonical tensor types.
2. A deterministic-intent, check-only per-PU shape solver.
3. Atomic refinement of WN, ST, and managed logical-value type projections.
4. Driver-owned per-PU and REGION scope rules, with cross-PU work deferred to
   an IPA-owned design.
5. Runtime-only generation tracking, invalidation, revalidation, and a
   pre-lowering currency gate.
6. Static, pending, anonymous-dynamic, PU-qualified symbolic, and
   symbol-plus-constant dimensions.
7. Expanded broadcast and batched-matmul inference.
8. SP9 certification prose and a backend startup correction.
9. Structured SP10 triggers and four shape-effect classifications.

The stated compatibility boundary avoids new WHIRL opcodes, a new `TY_KIND`,
new ELF or mapped-image sections, persisted generation state, a backend Python
dependency, and ordinary VHO cross-PU propagation.

## 3. Review Outcome

### 3.1 Verdict Counts

| Verdict | Count |
| --- | ---: |
| Pass | 5 |
| Partial | 11 |
| Fail | 1 |
| Merge-only | 0 |
| Total | 17 |

The only Fail is commit
`416afca580aabb71fcba96b44c80f2773198469f`, which implements atomic shape
refinement. The endpoint accepts and persists a prohibited non-shape canonical
identity change.

### 3.2 Stage Summary

| Stage | Commits | Verdict | Main reason |
| --- | --- | --- | --- |
| SP2-SP3 foundation | 1-2 | Partial | The interner is sound in the reviewed paths, but solver insertion-order independence and the complete solver matrix are not certified. |
| SP4-SP5 atomic refinement | 3-4 | Fail | Shape-only identity preservation fails, owner checking is not complete before mutation, and negative coverage is incomplete. |
| SP6 scope and ownership | 5-9 | Partial | The per-PU architecture is appropriate, but boundary validation and multi-PU runtime evidence are incomplete. |
| SP7 pipeline revalidation | 10-11 | Partial | Source topology is plausible, but the required linked behavioral matrix is not reviewer-accessible. |
| SP8 symbolic propagation | 12-14 | Partial | Externally reachable shape strings accept unproved foreign PU qualifiers. |
| SP9 certification | 15-16 | Partial | Focused checks pass, but the broad certification claim lacks accessible provenance. |
| SP10 trigger contract | 17 | Partial | Structured triggers exist, but pass registration cannot declare an effect and broad certification is not reviewable. |

### 3.3 Complete Commit Mapping

Verdicts apply to each commit at its own snapshot. A later repair changes the
endpoint risk but does not retroactively change an earlier commit verdict.

| No. | Commit | Subject | Plan mapping | Verdict | Endpoint finding state |
| ---: | --- | --- | --- | --- | --- |
| 1 | `62cc9e9eeaac02c0732ec7a684110e88a13476ed` | Intern immutable canonical tensor types | SP2 | Pass | No confirmed live interner defect. |
| 2 | `aa8c7a8f9af5c400a928ec305cda1d15e7e8437e` | Add check-only DSL shape solver | SP3 | Partial | Live insertion-order evidence gap; malformed known-rank matmul behavior was later repaired. |
| 3 | `f2298a00f8facd4073aab3beb0548a3dca128ea9` | Define atomic DSL shape retyping contract | SP4 | Partial | Live stale commit IDs and an all-PU rollback contradiction. |
| 4 | `416afca580aabb71fcba96b44c80f2773198469f` | Implement atomic DSL shape refinement | SP5 | Fail | Live canonical-identity, owner-preflight, boundary-validation, and negative-matrix gaps. |
| 5 | `e8ec686caf6cf1095663a20fa3048d47e5b57df4` | Clarify driver-owned per-PU shape refinement | SP6 | Partial | Architecture is aligned; runtime coverage and boundary evidence remain incomplete. |
| 6 | `d7c72d1a51672053b5761062cb04af52f381555c` | Clarify driver-owned REGION lifecycle | SP6 | Pass | No defect introduced; inherited SP6 gaps remain. |
| 7 | `8129fd985fcb4e5f960a0c9dfbd94c5f65bdc5a2` | Define Open64 compilation-scope ownership | SP6 governance | Pass | No confirmed live defect. |
| 8 | `0279f2db6403619d08405e4418e6780c59e2bcf5` | Align DSL tests with compilation scope | SP6 tests | Partial | Required executable multi-PU isolation fixture is absent. |
| 9 | `fd394610dbcae8d3ba5624a9acdff883057c9691` | Collect future IPA shape propagation work | SP6 deferral | Partial | Its snapshot had an EOF whitespace error; commit 17 repaired the endpoint. |
| 10 | `76106e123af5bb44ed10d0fd1ecb4b547a8507cd` | Integrate DSL shape revalidation with backend pipeline | SP7 | Partial | Live certification-provenance gap; no product regression was proved from the inspected topology. |
| 11 | `4a5e298fd1a3b38d723ad343d793e9009d2d501a` | Document PREOPT and WOPT canonicalization roles | SP7 governance | Partial | Narrow PREOPT consumer wording was repaired by commit 13. |
| 12 | `7050d2288657a1bba89844d96789786ad60ee2da` | Implement symbolic DSL shape propagation | SP8 | Partial | Live foreign-qualifier trust-boundary defect and design-authority conflict. |
| 13 | `feadda989a3ccf24ff7414991311ef6fecfdd15d` | Clarify PREOPT canonicalization consumers | SP8 governance | Pass | Repairs commit 11; no new endpoint defect. |
| 14 | `1c902dcaf7144a15edc0ae07fc6fff3a3357ab74` | Define optimization-level scope contracts | SP8 governance | Pass | No confirmed live defect. |
| 15 | `b1fd1ceec4ff919519678fa7d2efcdd12d5b6f10` | Complete SP9 tensor shape inference and certification | SP9 | Partial | Live certification-provenance gap; repairs commit 2's malformed matmul handling. |
| 16 | `f076d80203699af469993d0ce3e253c8bdc8325e` | Correct backend startup initialization for certification | SP9 dependency | Partial | Source correction passes syntax checks; runtime startup and model claims remain unverified. |
| 17 | `7cac0d86b6548805b53c05675ffb7e1dff9395fa` | Enforce the SP10 shape inference trigger contract | SP10 | Partial | Live effect-registration and certification gaps; repairs commit 9's endpoint whitespace issue. |

### 3.4 Governing Authority and Plan-Boundary Qualifications

The user-designated highest architecture and overall implementation authority
for this repair is
`doc/DSC_FHE_Compiler_Architecture_and_Integration_Plan_v0.10.md`. The root
`AGENTS.md` is an orthogonal and mandatory repository policy. `WHIRL.pdf` is
the normative architectural baseline for WHIRL semantics and the existing
mapped-image layout, readers, writers, and printers. If these authorities appear to
conflict, implementation must stop until they are reconciled; a focused plan
must not choose one silently.

The focused FHE implementation tracker, the shape propagation design and
implementation plan, and the retyping, symbolic, and trigger contracts are
subordinate plans. They may narrow and stage work, but they may not allocate a
shared representation, alter domain ownership, bypass a gatekeeper, broaden a
compilation scope, or advance a master-plan milestone independently. SP9 and
SP10 certification documents remain claims to reproduce, not self-proving
evidence. The master plan is an architecture proposal with proposed API and
option names; this repair applies its selected boundaries without pretending
that every proposed interface already exists.

The authority chain has four qualifications that the repair must preserve:

1. `WHIRL.pdf` was not present in the reviewed checkout. Concrete conformance
   involving WHIRL representation, mapping, reader, writer, or printer details
   was therefore not independently checked against that baseline.
2. The design has the live stage-state and cross-PU conflicts recorded in
   P2-07; the retyping contract has the live all-PU rollback contradiction in
   P3-01; and the implementation plan has the unresolvable commit references
   in P3-02.
3. PR 137 combines SP2 through SP10 even though the plan prescribes focused,
   serial milestone pull requests after SP2. The project must either record an
   explicit combined-PR waiver or apply every historical stage gate to the
   combined range.
4. This range itself adds substantial rules to `AGENTS.md`. Those endpoint
   rules are review subjects and cannot serve as self-authenticating evidence
   that earlier commits had already met them.

### 3.5 Master-Plan Milestone Crosswalk

The planned PR 137 repairs address shared shape infrastructure. They support,
but do not complete, the master plan's early milestones:

| PR 137 repair area | Master-plan relationship | Maximum permitted claim |
| --- | --- | --- |
| Tensor identity, shape rules, and solver contracts | Common Compiler Substrate; prerequisite support for Phase 0 and M0 | Provides input to contract review; does not complete M0. |
| Mapped tensor shape metadata and per-PU ownership | Infrastructure support for M1 metadata capture | Helps preserve and inspect M1 artifacts; does not complete M1. |
| Active-PU legality, malformed rejection, and stale-state gates | Infrastructure support for M2 validation-only driver behavior | Supplies shared verification machinery; does not implement or complete the FHE gatekeeper or M2. |
| Invalidation, revalidation, and SP7-SP10 certification | PR-specific pipeline correctness | Certifies only the repaired shape subsystem; does not complete M3-M8. |

ResNet-20/CIFAR-10 remains the master plan's first full-model vertical slice.
Llama cases in the PR 137 matrix are supplemental, claim-specific regressions
for already registered shape rules; they are not an earlier or alternative
architecture milestone.

## 4. Live Findings

Status terms used below:

- `Live` means the endpoint still contains the behavior or contradiction.
- `Unverified` means retained evidence or an independent rerun did not
  establish a claimed acceptance property. It does not, by itself, prove a
  runtime regression.
- `Historical, repaired` means a defect affected an earlier commit snapshot
  but a later in-range commit repaired the endpoint.

### 4.1 P1 Findings

#### P1-01: Atomic retype persists a non-shape canonical identity change

- Introducing commit: `416afca580aabb71fcba96b44c80f2773198469f`.
- Status: Live and reproduced at the frozen endpoint.
- Trigger: Submit a legal monotonic shape refinement whose sealed
  `refined_ty` changes a custom identity key, for example
  `shape_contract=v1` to `shape_contract=v2`.
- Impact: `DSL_IR_Refine_Native_Value_Types()` returns success and persists the
  wrong type in WN, ST, and managed logical-value projections. There is no
  rollback or diagnostic, even though SP4 permits only shape refinement.
- Endpoint cause: `DSL_IR_Retype_Type_Valid()` compares fixed descriptor
  fields, alignment, rank, and monotonic dimensions, but not the complete
  canonical identity KV set.
- Reachability qualification: the current first-party VHO planner derives its
  refined type from `expected_old_ty`, so its normal path preserves custom
  keys. The demonstrated trigger is a malformed request from a
  compiler-internal caller, not ordinary source input. This remains P1 because
  the exported common transaction API owns fail-closed request validation and
  silently commits a prohibited semantic identity change when that contract is
  violated.
- Reproduction:

  ```text
  OPEN64_DSL_SHAPE_IDENTITY_REPRO=1 ./dsl_shape_refine_contract_test
  identity_repro accepted=1 rollback_count=0 persisted=1 old_contract=v1 new_contract=v2
  ```

#### P1-02: SP7 completion evidence is not reviewer-accessible

- Introducing commit: `76106e123af5bb44ed10d0fd1ecb4b547a8507cd`.
- Status: Live, unverified acceptance claim.
- Trigger: Treat source-order greps, generated `passed` labels, or unavailable
  `/private/tmp` paths as proof of the complete SP7 matrix.
- Impact: Reviewers cannot establish actual linked behavior for optimization
  levels, transformation re-entry, FHE input state, stale lowering rejection,
  unrelated options, non-DSL WHIRL, checkpoint paths, or `whirl2c`.
- Endpoint evidence: the named broad artifacts are outside Git and absent in
  the review environment; the PR snapshot reported no status contexts or check
  runs. The tracked focused script is useful but does not execute the full
  matrix.
- Qualification: this is an acceptance and provenance blocker, not a proved
  runtime regression.

#### P1-03: SP9 full-certification provenance is absent

- Introducing commit: `b1fd1ceec4ff919519678fa7d2efcdd12d5b6f10`.
- Status: Live, unverified acceptance claim.
- Trigger: Accept the `Complete` claim without the excluded artifact family or
  an independent rerun.
- Impact: The model, FHE ownership, non-DSL WHIRL, previous/current reader,
  target, normal backend, and `whirl2c` lanes cannot be audited.
- Endpoint evidence: the tracked SP9 script runs a focused builder and
  `ir_b2a` lane, then writes fixed labels. The documented broad artifacts exist
  only under an unavailable local `/private/tmp` path, and no PR check supplied
  an alternative.
- Qualification: no independent arithmetic defect was confirmed in the
  inspected SP9 broadcast and matmul rules. The gap is evidence and provenance,
  not a proved broad runtime failure.

#### P1-04: SP10 certification artifacts and checks are not reviewable

- Introducing commit: `7cac0d86b6548805b53c05675ffb7e1dff9395fa`.
- Status: Live, unverified acceptance claim.
- Trigger: Treat the certification prose or source-grep-derived labels as proof
  of all SP10 runtime lanes.
- Impact: The full effect-class, invalid-trigger, generation, stale-state,
  non-DSL, target, reader, build, symbol-audit, model, and before/after WHIRL
  claims cannot be independently checked.
- Endpoint evidence: the only named bundle is outside Git and absent; the PR
  snapshot reported no check runs.
- Qualification: structured triggers and conservative invalidation properties
  were confirmed in source. The blocker is the unestablished completion gate,
  not proof that every runtime lane fails.

### 4.2 P2 Findings

#### P2-01: Fixed-point certification does not vary insertion order

- Introducing commit: `aa8c7a8f9af5c400a928ec305cda1d15e7e8437e`.
- Status: Live evidence gap.
- Trigger: An order-dependent solver change applied to equivalent graphs whose
  values, nodes, or constraints are inserted in different orders.
- Impact: Running the solver twice on the same constructed PU proves
  repeatability for one order, not insertion-order independence.
- Endpoint evidence: the fixture compares two analyses of the same object and
  still emits `deterministic_fixed_point=passed`; no permutation fixture was
  found.

#### P2-02: Foreign-owner logical values fail only after transient mutation

- Introducing commit: `416afca580aabb71fcba96b44c80f2773198469f`.
- Status: Live complete-preflight violation; final restoration is verified.
- Trigger: Use two PUs with colliding local `ST_IDX` values and submit a
  request for the first PU's logical value while the second PU is active.
- Impact: The foreign value passes preflight, mixed-owner projections are
  mutated, strict post-verification fails, and reverse rollback is required.
  This violates the contract that ownership rejection occurs before the first
  write.
- Endpoint outcome: the tested final state is restored. This is not evidence
  of persistent corruption.
- Reproduction:

  ```text
  OPEN64_DSL_SHAPE_FOREIGN_OWNER_REPRO=1 ./dsl_shape_refine_contract_test
  owner_repro accepted=0 rollback_count=1 restored=1 first_value=2 first_st=2 second_st=2
  ```

#### P2-03: SP5 negative and rollback coverage is incomplete

- Introducing commit: `416afca580aabb71fcba96b44c80f2773198469f`.
- Status: Live evidence gap.
- Trigger: A malformed request, projection disagreement, ownership collision,
  non-shape identity change, or failure at a journaled mutation point not
  represented by the current focused test.
- Impact: The existing happy path and one post-apply forced failure do not
  establish complete preflight, reverse rollback at every write boundary,
  generation stability, or repeated-failure behavior.
- Endpoint evidence: the missing canonical-identity and foreign-owner cases
  are both required by the contract and were independently reproduced.

#### P2-04: Active-PU boundary validation does not run on every success path

- Implementation introduced by:
  `416afca580aabb71fcba96b44c80f2773198469f`.
- Completion claim strengthened by:
  `e8ec686caf6cf1095663a20fa3048d47e5b57df4`.
- Status: Live.
- Trigger: A no-native, no-request, or already-shape-complete PU contains stale
  physical call ABI, formal, hidden-result, PU-interface, or REGION evidence
  that is not the retyped result.
- Impact: A successful shape-driver return does not guarantee that all required
  active-PU boundaries have been validated before downstream DSL phases.
- Endpoint evidence: fast paths and strict gatekeeper validation do not
  uniformly call `DSL_Call_ABI_Image_Validate_PU()`,
  `DSL_PU_Interface_Image_Validate_PU()`, and `DSL_Region_Verify_PU()`.
- Qualification: the missing calls were established statically; a focused
  corrupt-boundary runtime fixture still needs to be added.

#### P2-05: SP6 driver coverage and multi-PU isolation are not established

- Completion claim introduced by:
  `e8ec686caf6cf1095663a20fa3048d47e5b57df4`.
- Missing fixture explicitly recorded by:
  `0279f2db6403619d08405e4418e6780c59e2bcf5`.
- Status: Live evidence gap.
- Trigger: Treat source line-number checks as proof that every selected PU is
  independently processed.
- Impact: There is no executable proof that each selected PU gets one
  invocation, colliding local indexes do not alias, a failing PU does not
  mutate another PU, and success in one PU does not mask failure in another.
- Endpoint evidence: the certification script performs source-order checks but
  no focused multiple-PU runtime fixture exists.

#### P2-06: External shape strings accept foreign PU qualifiers

- Introducing commit: `7050d2288657a1bba89844d96789786ad60ee2da`.
- Status: Live and independently reproduced.
- Trigger: Supply a shape such as `[L@pu00000001]` while there is no owner or
  while active owner identity is `2`, without an authorized boundary import.
- Impact: Exact canonical symbol text is used as symbolic identity, so
  unrelated dimensions can be made equal for broadcast, matmul, or attention
  proofs.
- Endpoint cause: the parser validates syntax, qualifier length, hexadecimal
  format, and a nonzero payload, but not active-PU ownership or an explicit
  interface mapping. The path is reachable from Python `logical_shape` input
  and serialized tensor type shapes.
- Reproduction:

  ```text
  serialized_accepted=1 serialized_output=[L@pu00000001] foreign_accepted=1 foreign_output=[L@pu00000001]
  ```

#### P2-07: The authoritative design conflicts with endpoint stage state

- Materially exposed by:
  `7050d2288657a1bba89844d96789786ad60ee2da` and
  `7cac0d86b6548805b53c05675ffb7e1dff9395fa`.
- Status: Live documentation-authority conflict.
- Trigger: Use the design status and validation lists as the authoritative
  endpoint acceptance matrix.
- Impact: The design still reports SP5-era state, mixes current per-PU VHO
  obligations with future cross-PU IPA work, and says SP10 invalidation still
  needs review while other documents claim completion.
- Endpoint evidence: the design, implementation plan, retyping contract, and
  certification documents do not provide one internally consistent stage and
  scope model.

#### P2-08: Pass registration cannot declare the required shape effect

- Introducing commit: `7cac0d86b6548805b53c05675ffb7e1dff9395fa`.
- Status: Live API and extensibility gap.
- Trigger: Register a pass through `VHO_DSL_Opt_Register_Pass()` and attempt to
  declare preserving, monotonic-refining, local-invalidating, or
  boundary-invalidating behavior.
- Impact: The effect enum exists, but registration stores only the stage and
  pass pointer. A separate stage switch classifies every current and unknown
  stage as locally invalidating. The current fixed set is conservatively
  fail-closed, but the extension point cannot implement the normative
  per-transformation declaration protocol.
- Endpoint evidence: invalid and unknown effects default to invalidating, but
  no effect is associated with the registered pass.

### 4.3 P3 Findings

#### P3-01: The retyping contract retains an obsolete all-PU rollback test

- Contract introduced by:
  `f2298a00f8facd4073aab3beb0548a3dca128ea9`.
- Contradiction created by per-PU correction:
  `e8ec686caf6cf1095663a20fa3048d47e5b57df4`.
- Status: Live documentation contradiction.
- Trigger: Implement the requirement to fail a later PU and restore every
  earlier PU.
- Impact: That requirement implies an all-PU transaction, while the accepted
  scope model requires independent driver-owned per-PU transactions.
- Required interpretation: prove per-invocation atomicity, cross-PU isolation,
  and no valid final artifact publication after compilation failure. Do not
  retroactively roll back an already completed earlier PU in memory.

#### P3-02: SP1-SP3 plan commit IDs do not resolve

- Introducing commit: `f2298a00f8facd4073aab3beb0548a3dca128ea9`.
- Status: Live traceability defect.
- Trigger: Follow plan IDs `08c5eb48`, `eb8b7273`, and `0ef23ab9` in the local
  or queried GitHub repository.
- Impact: Reviewers cannot reproduce the plan's earliest completion history.
- Endpoint evidence: none of the three IDs resolves. The actual in-range SP2
  and SP3 commits are `62cc9e9eeaac02c0732ec7a684110e88a13476ed` and
  `aa8c7a8f9af5c400a928ec305cda1d15e7e8437e`; SP1 belongs to the excluded base
  history.

### 4.4 Post-Review Build Finding

#### BF-01: Copied common-source consumers lacked shape dependencies

- Discovery scope: post-review build validation; not part of the frozen 14
  live/unverified findings, the three historical findings, or the 5/11/1
  commit verdict counts.
- Status: implementation committed at
  `6eada12d4a78623726711a2922642febf231174d`; reference-lane validation pending.
- Cause: seven copied common-source lists compiled `dsl_ir_rewrite.cxx` without
  also compiling `dsl_gatekeeper.cxx` and `dsl_shape.cxx`, leaving
  `wgen42` and potentially related consumers link-incomplete.
- Repair: the two missing common/com translation units were added once to each
  of the seven copied source lists. All ten consumers of `dsl_ir_rewrite.cxx`
  were statically audited for one rewrite, gatekeeper, and shape object.
- Established evidence: an Ubuntu 24 diagnostic build, install, installed
  `opencc` C smoke, standalone frontend tests, and focused DSL checks passed
  with a separate uncommitted generic compatibility overlay.
- Remaining evidence: the required Ubuntu 20/LLVM 11 reference lane has not
  run. The modified Fortran and JFE source lists have static consistency
  evidence but no linked product validation in the retained run. BF-01 must
  therefore remain partially validated.
- Ownership boundary: the repair stays in common/com source dependencies and
  must not introduce Python, backend-code-generation, `DSL_Builder_*`, JsonCpp,
  or FHE runtime C-ABI dependencies into backend shared libraries.

## 5. Historical Findings Repaired Within the Range

These issues affect their introducing commit verdicts but are not endpoint
blockers.

| Priority | Introduced | Repaired | Historical issue | Endpoint state |
| --- | --- | --- | --- | --- |
| P2 | `aa8c7a8f9af5c400a928ec305cda1d15e7e8437e` | `b1fd1ceec4ff919519678fa7d2efcdd12d5b6f10` | Known malformed complete-rank matmul cases could remain pending instead of being rejected. | Repaired; retain a focused regression test. |
| P2 | `4a5e298fd1a3b38d723ad343d793e9009d2d501a` | `feadda989a3ccf24ff7414991311ef6fecfdd15d` | PREOPT consumer language was too WOPT-oriented. | Repaired by naming WOPT, LNO, and IPA where applicable. |
| P3 | `fd394610dbcae8d3ba5624a9acdff883057c9691` | `7cac0d86b6548805b53c05675ffb7e1dff9395fa` | Commit 9 introduced a blank line at EOF and failed its own `git diff --check`. | Repaired; the aggregate endpoint diff passes `git diff --check`. |

The commit 9 whitespace failure is historical only. It must not be reported as
a live endpoint blocker.

## 6. Validation State and Environment Limitation

### 6.1 Established by the Review

- Exact base, head, merge base, 17-commit count, linear topology, and zero
  merge commits.
- Aggregate endpoint `git diff --check` passes.
- Endpoint lightweight native syntax and operator-layout fixtures pass for the
  SP7 and SP10 source set.
- The exact SP9 snapshot passes its lightweight syntax and operator-layout
  fixture.
- Commit 16's backend startup source passes a focused GNU++98 syntax compile.
- The atomic-retype baseline focused fixture passes.
- The custom canonical-identity negative repro proves success, no rollback,
  and persistent semantic replacement.
- The foreign-owner repro proves rejection after mutation and complete reverse
  rollback of the observed final state.
- The symbolic parser repro proves acceptance of serialized and active-owner
  mismatched foreign qualifiers.
- Source review confirms structured SP10 trigger identities, invalid-trigger
  rejection, stable trigger names, a defensive generation check, and a
  conservative default-invalidating behavior.

### 6.2 Not Established

- A complete linked backend build and runtime matrix.
- Normal backend startup and ResNet `-O0` behavior.
- Complete `-O0`, `-O1`, `-O2`, `-O3`, and `-ipa` behavior.
- All model, attention, symbolic, FHE input-state, ownership, and re-entry
  lanes.
- Non-DSL WHIRL behavior with the stated previous and current readers.
- The full target matrix, checkpoint read/write, and `whirl2c` compatibility.
- Repeated multi-PU driver invocation and isolation.
- Complete SP5 preflight, failure-injection, rollback, and generation matrix.
- Runtime coverage for all four SP10 effect classes.
- Rebuilt `be`, `be.so`, `lw_inline`, and full symbol-boundary audits.
- Retained before/after `.B` and `.T` evidence with a raw WHIRL diff.

### 6.3 Build Environment Blocker

The review's linked build attempt failed before PR-focused binaries were built.
On Ubuntu 24 with GCC 13, the generated target-information program
`targ_si_gen` segfaulted at `Makefile.gbase:785` after a generated-code
warning. The review did not attribute that failure to PR 137. It explains why
the broad runtime matrix was not independently established, but it does not
make the missing gates pass.

### 6.4 Review Access and Coverage Limitations

- `WHIRL.pdf`, which repository policy declares to be the normative WHIRL
  baseline, was absent. The review could not independently compare concrete
  representation, mapped-image, reader, writer, or printer behavior with it.
- The `gh` command was unavailable, so the review used GitHub REST endpoints
  and the public PR page. SSH `git ls-remote upstream` could not authenticate;
  public REST branch and PR endpoints, configured local refs, and the exact PR
  commit list supplied the remote comparison instead.
- GitHub mergeability, review, status-context, and check-run values are a dated
  snapshot and must be refreshed before an approval decision.
- The author-referenced artifact root under
  `/private/tmp/open64-shape-sp5/artifacts/shape/` was absent, and no GitHub
  check provided a replacement bundle.
- Full linked historical builds were not run for every commit snapshot.
  Snapshot syntax, layout, source, and focused executable evidence must not be
  generalized into a complete historical runtime matrix.

### 6.5 Post-Review Build and Native-Lifecycle Update

The frozen review facts above remain historically accurate. Later focused
work established the following additional facts without changing the frozen
commit verdicts:

1. BF-01 was implemented at repair head `6eada12d...`. The Ubuntu 24 build
   succeeded only with a separately retained, uncommitted portability overlay;
   this is supplemental evidence, not the required Ubuntu 20/LLVM 11 lane.
2. The repeated native-export DST failure was tested symmetrically at PR head
   `7cac0d86...` and base `590bdf3d...`. Fresh-process A and B passed, while
   same-process AB and BA failed at the second export on both revisions with
   `Illegal attempt to start DST file-scope twice`.
3. The DST failure is therefore base-owned. No DST product fix belongs in the
   PR 137 repair branch. A full same-process native-suite gate either depends
   on a separate develop-based lifecycle fix or must use an explicit strict
   base/head differential policy; process isolation cannot be reported as a
   repair.
4. Individual process-boundary binary WHIRL production and inspection passed,
   but that does not establish a complete native component lifecycle or any
   master-plan milestone.

### 6.6 Repair Baseline at 6eada12d

The repair branch baseline is
`6eada12d4a78623726711a2922642febf231174d`. It contains the focused BF-01
source-list dependency closure and its English build/native lifecycle plan.
The required Ubuntu 20/LLVM 11 reference lane and touched Fortran/JFE linked
validation remain pending.

No WP0-WP10 semantic repair commit or exact-snapshot semantic repair evidence
is present at this baseline. The workstreams in Section 7 remain planned and
all live or unverified findings in Section 4 remain open. The frozen review
verdicts remain 5 Pass, 11 Partial, and 1 Fail, and the recommendation remains
do not approve yet.

## 7. Remediation Workstreams in Dependency Order

The workstreams below are ordered so that trust-boundary and transaction
correctness are fixed before broad certification is repeated.

Three explicit prerequisite nodes govern the sequence:

```text
A0 Master-plan and authority reconciliation -> WS1, WS3, WS4, and WS7
B0 Build dependency closure at 6eada12d -> WS8 and the clean-build gate
X1 Develop-owned native DST lifecycle -> full same-process native-suite gate

WS1 + WS4 -> WS2
WS3 -> WS5
WS2 + WS4 -> WS6
WS1-WS6 -> WS7 -> WS8
```

- A0 is blocking and not optional: every local contract must be reconciled
  with the master plan before implementation.
- B0 is implemented and has supplemental Ubuntu 24 evidence, but remains
  pending in the required Ubuntu 20/LLVM 11 reference lane. If all repairs stay
  on one aggregate branch, the focused build plan's diff-only gate applies to
  subrange `7cac0d86...6eada12d`, not to the final aggregate branch.
- X1 is base-owned and must not be implemented in this PR 137 repair series.
  It is either an external develop-based prerequisite or an explicitly
  retained differential limitation for PR approval and master-plan readiness.

### WS1: Make Atomic Retype Preflight Complete

Findings addressed: P1-01, P2-02, and part of P2-03.

Implementation approach:

1. Separate type-equivalence rules from shape-only update authorization. Reuse
   the tensor type owner's comparison services, but authorize a transaction to
   change only approved `logical_shape` dimensions with unchanged rank.
2. Require every other stored tensor/type/descriptor or domain state to remain
   unchanged, including unknown state. Traits, layout, placement, memory,
   quantization, encryption-descriptor references, semantic role, and lineage
   cannot be changed merely because some of them do not participate in type
   equivalence. Compiler metadata is not added to type equivalence; whether it
   is stored inside or outside the tensor descriptor, this transaction must
   leave it untouched.
3. Immediately after loading a logical value, call the existing owner-aware
   predicate and reject a value that does not belong to `owner_pu_st`.
4. Perform all checks before a journal is opened or any WN, ST, managed value,
   REGION, generation, or other projection is modified.
5. Preserve strict post-verification as defense in depth. A rank-changing
   flatten or reshape creates and types its distinct result value; it is not an
   exception that permits same-value SP5 retyping to change rank.

Likely files:

- `osprey/common/com/dsl_ir_rewrite.cxx`
- `osprey/common/com/symtab.cxx` and `symtab.h` if a reusable canonical
  identity helper is required
- `osprey/common/com/tests/dsl_builder_contract_test.cxx`
- `osprey/be/vho/tests/dsl_shape_refine_contract_test.cxx`
- `doc/WHIRL-DSL-SHAPE-RETYPING-CONTRACT.md`

Required positive tests:

- Retype one uniquely owned local native result and all direct reads while
  preserving identical non-shape identity.
- Leave an unrelated value that shares the old `TY_IDX` unchanged.
- Reuse an equivalent interned refined type without `Ty_tab` growth.
- Retype multiple independent values in one request array with all WN, ST, and
  managed-value projections agreeing.
- Preserve an eligible local REGION interface row byte-for-byte while its ST
  derives the refined type.
- Write, reopen, and inspect the successful artifact with
  `ir_b2a -st -src`.

Required negative tests:

- Custom canonical identity key changed, added, removed, and bound/unbound.
- Each fixed descriptor identity field changed independently.
- Each non-shape descriptor/domain field changed independently, including an
  unknown key and lineage or encryption-related reference.
- Invalid or inactive owner, invalid value ID, invalid old or refined type,
  and foreign logical value with colliding local `ST_IDX`.
- Duplicate value, duplicate result symbol, no-op, conflicting request, and
  stale expected type in the value, STID, ST, or any direct read.
- Multiple definitions, projection disagreement, address-taken or indirect
  use, alias escape, and an unknown symbol-bearing WN.
- Impure producer, state-effect row, tensor constant, model input, external
  payload, or TCON-backed value.
- Call actual or output, PU formal, hidden result, function return, prototype,
  `TYLIST`, and context-sensitive shared-callee participation.
- FHE tensor binding, unregistered auxiliary-image dependency, and inconsistent
  or unsupported REGION profile dependency.

Exit criteria:

- Every malformed request fails before the first write.
- Canonical-identity and foreign-owner repros become negative regressions.
- Rejections leave all observable projections and generations unchanged with
  `rollback_count=0`.
- Valid shape-only requests retain current successful behavior.

### WS2: Complete Failure Injection and Rollback Certification

Findings addressed: the remainder of P2-03.

Implementation approach:

1. First add deterministic test-only failure points after every existing
   journaled mutation position, without changing release semantics or replacing
   the current journal.
2. Snapshot WN, ST, logical values, REGION relationships, generation state,
   and relevant counters before each run.
3. Verify reverse rollback for every post-write failure and zero writes for
   every preflight failure.
4. Run repeated-failure cases to detect journal or generation leakage.
5. Change the production journal only if this executable matrix proves an
   exact-restoration defect. Reuse the existing transaction mechanism and add
   the smallest missing write record; do not create a parallel journal solely
   because the earlier certification was incomplete.

Likely files:

- `osprey/common/com/dsl_ir_rewrite.cxx`
- `osprey/common/com/dsl_ir_image.cxx` and headers if snapshot helpers are
  needed
- `osprey/be/vho/tests/dsl_shape_refine_contract_test.cxx`
- `osprey/common/com/tests/dsl_shape_refine_test.sh`

Required positive tests:

- Existing one-request and multi-request transaction success.
- A successful transaction after a prior injected failure.

Required negative tests:

- Failure after each WN read update, definition update, ST update, and managed
  value update.
- Validator-specific failures during strict image, gatekeeper, shared shape,
  and REGION postchecks.
- Repeated failure with stable type-table and generation counts.

Exit criteria:

- Every injected post-write failure restores the exact snapshot.
- Rollback counts match the actual journaled requests.
- No partial valid artifact is published.

### WS3: Enforce the Symbolic Qualifier Trust Boundary

Finding addressed: P2-06.

Implementation approach:

1. Separate syntax parsing and mapped-form preservation from semantic
   admission. A reader or printer may parse, retain, and display a named or
   qualified symbolic form without treating it as trusted equality evidence.
2. At solver, gatekeeper, retype, and other proof-use sites, require the active
   PU owner. An unqualified local symbol binds to that owner; a qualifier is
   admitted only when it matches that owner or a reviewed call/interface
   mapping supplies explicit source and destination ownership.
3. Use an existing host-independent mapped-image identity. Do not persist a
   pointer, process address, allocation order, or runtime-only owner table.
4. Keep canonical text equality only after provenance has been validated.
   Per-PU validation of the call/formal/result boundary is permitted and is not
   a cross-PU transformation; coordinated opposite-side refinement remains
   IPA-owned.

Likely files:

- `osprey/common/com/dsl_shape.cxx` and `dsl_shape.h`
- `osprey/common/com/dsl_builder.cxx`
- `osprey/common/com/tests/dsl_builder_contract_test.cxx`
- `osprey/common/com/tests/dsl_shape_symbolic_test.sh`
- `doc/WHIRL-DSL-SYMBOLIC-SHAPE-CONTRACT.md`
- Python/native bridge call sites under `osprey/torch2whirl/` if the API must
  distinguish untrusted frontend input from reviewed import

Required positive tests:

- Unqualified local symbol canonicalized to the active PU.
- Already-qualified current-PU symbol accepted in an explicitly defined local
  mode, if that behavior is retained.
- Authorized boundary carry with a matching interface mapping.
- Fresh-process write/reopen/print of the mapped symbolic form without trusting
  it for inference until owner admission succeeds.
- Static, pending, anonymous dynamic, and symbol-plus-constant forms remain
  valid.

Required negative tests:

- Zero-owner serialized foreign qualifier.
- Active-owner mismatch.
- Malformed, zero, truncated, and overflowed qualifiers.
- A syntactically valid qualifier with no authorized interface mapping.
- Two unrelated symbols cannot satisfy equality-sensitive rules through a
  forged common string.

Exit criteria:

- Both retained foreign-qualifier repro cases are rejected unless an explicit
  reviewed import context is supplied.
- Authorized boundary propagation remains possible and is provenance-checked.
- No unreviewed binary section or persisted constraint table is introduced.
- Owner-independent inspection remains possible, and fresh-process equality
  use still requires explicit owner proof.

### WS4: Centralize Active-PU Boundary Validation and Prove Isolation

Findings addressed: P2-04 and P2-05.

Implementation approach:

1. Define one active-PU boundary-validation operation covering call ABI,
   formal/result/return interface, and REGION relationships. Pass the selected
   PU identity and context explicitly; a common/com helper must not discover a
   broader compilation scope through global traversal.
2. Invoke it on admission and before every successful shape-driver return,
   including no-native, no-request, and already-complete paths.
3. Keep driver ownership of PU and REGION lifetime; do not traverse or activate
   another PU from VHO or common code.
4. Add a backend-selected multi-PU fixture with colliding local indexes and
   independent success/failure outcomes.
5. Keep this common shape gate distinct from the CNN and FHE domain gates. It
   must not consume, hide, or lower domain operators before those owners have
   validated them.

Likely files:

- `osprey/be/vho/dsl_shape_refine.cxx`
- `osprey/common/com/dsl_ir_rewrite.cxx`
- `osprey/common/com/dsl_gatekeeper.cxx`
- `osprey/common/com/dsl_region.cxx`
- `osprey/be/vho/tests/dsl_shape_refine_contract_test.cxx`
- `osprey/common/com/tests/dsl_shape_refine_test.sh`

Required positive tests:

- No-native, no-request, shape-complete, ordinary refinement, and REGION paths
  all execute the same boundary gate.
- Two selected PUs are each invoked exactly once and preserve independent
  state even with colliding local indexes.

Required negative tests:

- Stale call actual, formal, hidden result, return, PU-interface, and REGION
  rows whose global image structure remains otherwise valid.
- Failure in a later PU does not mutate an earlier PU or publish a valid final
  artifact.
- Success in one PU does not mask failure in another.

Exit criteria:

- Every successful return has passed all required active-PU validators.
- No fast path bypasses the boundary gate.
- The multi-PU fixture proves independent invocation and cross-PU isolation,
  without creating an all-PU transaction.

### WS5: Strengthen Solver Determinism and Retain Repaired Negatives

Finding addressed: P2-01 and the historical matmul issue.

Implementation approach:

1. Build semantically equivalent graphs using multiple value, node, and
   constraint insertion orders.
2. Normalize and compare complete fact sets, diagnostics, contradiction
   classifications, and refinement requests.
3. Retain explicit malformed-rank matmul tests for the repaired behavior.
4. Modify solver ordering only if these permutations expose a defect. Reuse
   the existing deterministic ordering and Open64 services where sufficient;
   do not add a parallel rule engine or change semantic priority merely to
   make the test output convenient.

Likely files:

- `osprey/common/com/tests/dsl_builder_contract_test.cxx`
- `osprey/common/com/tests/dsl_shape_solver_test.sh`
- `osprey/common/com/dsl_shape.cxx` only if the expanded tests expose a logic
  defect

Required positive tests:

- Add/relu and a multi-operator graph produce identical normalized results for
  every insertion permutation.
- Static and symbolic valid matmul cases remain stable.

Required negative tests:

- Known rank below two, incompatible rank, incompatible inner dimensions, and
  contradictory result seed.
- Equivalent contradictions emit stable diagnostics across insertion orders.

Exit criteria:

- `deterministic_fixed_point=passed` is emitted only after genuine permutation
  coverage.
- Repaired malformed matmul behavior has a permanent regression test.

### WS6: Bind Shape Effects to Registered Passes

Finding addressed: P2-08.

Implementation approach:

1. For every executable transformation, publish its minimum optimization
   level, maximum compilation scope, required canonical form, shape effect,
   invalidation/revalidation behavior, and controlling option.
2. Decide whether the local implementation stores effect metadata per pass or
   uses an intentionally closed, audited stage table. Per-pass storage is the
   recommended repair for the current extensibility gap, but it is not wording
   attributed to the master plan.
3. Treat the effect as lifecycle metadata, not permission to execute. `-O0`
   permits correctness verification and required semantic lowering only; it
   does not permit fusion, profitability rewrites, or broader-scope analysis.
4. A missing, invalid, or unknown declaration must prevent the pass from
   running and fail closed. Do not run an unknown pass merely by classifying it
   as invalidating.
5. Audit each current pass before assigning an effect, then migrate default
   registrations and all call sites atomically.

Likely files:

- `osprey/be/vho/dsl_opt.h`
- `osprey/be/vho/dsl_opt.cxx`
- `osprey/be/vho/tests/dsl_opt_contract_test.cxx`
- `osprey/be/vho/tests/dsl_shape_refine_contract_test.cxx`
- `osprey/common/com/tests/dsl_shape_refine_test.sh`
- `doc/WHIRL-DSL-SHAPE-PROPAGATION-DESIGN.md`
- `doc/WHIRL-DSL-SHAPE-PROPAGATION-IMPLEMENTATION-PLAN.md`

Required positive tests:

- Preserving pass leaves the generation current.
- Monotonic-refining pass performs the declared refinement and verification.
- Local-invalidating pass invalidates and revalidates the active PU.
- Boundary-invalidating pass follows the documented per-PU boundary policy.

Required negative tests:

- Invalid stage, null pass, duplicate registration, invalid effect, and unknown
  effect.
- A pass cannot run with a missing or mismatched declaration.
- Stale state remains rejected at the consumer boundary.
- A pass does not run below its declared optimization level or beyond its
  declared PU/REGION scope, and a shape effect never broadens that scope.

Exit criteria:

- Every registered transformation has one auditable execution and effect
  contract.
- Runtime invalidation behavior derives from that stored declaration.
- Unknown and invalid cases remain fail-closed and unexecuted.

### WS7: Reconcile the Normative Documents and Claims

Findings addressed: P2-07, P3-01, P3-02, and the claim wording in P1-02 through
P1-04.

Implementation approach:

1. Make every focused document explicitly subordinate to the master plan and
   preserve `AGENTS.md` and `WHIRL.pdf` as the orthogonal policy and
   representation constraints described in Section 3.4.
2. Update the shape design status to the actual repaired endpoint without
   claiming a master-plan milestone is complete.
3. Separate current per-PU VHO obligations from future IPA-owned cross-PU work.
4. Replace the obsolete all-PU rollback row with per-invocation atomicity,
   cross-PU isolation, and atomic artifact publication requirements.
5. Replace stale SP1-SP3 commit IDs with resolvable full hashes and identify SP1
   as excluded base history.
6. Record the deliberate deviation from the original one-stage-per-PR sequence
   or obtain an explicit project decision for the combined PR boundary.
7. Change `complete`, `certified`, and `passed` language to match actual
   retained evidence until the full rerun succeeds.
8. Keep the master plan's Section 6.4 FHE encryption/type propagation table in
   its FHE owner. Shape rule certification may verify shape compatibility but
   must not copy or become a second FHE propagation implementation.

Likely files:

- `doc/WHIRL-DSL-SHAPE-PROPAGATION-DESIGN.md`
- `doc/WHIRL-DSL-SHAPE-PROPAGATION-IMPLEMENTATION-PLAN.md`
- `doc/WHIRL-DSL-SHAPE-RETYPING-CONTRACT.md`
- `doc/WHIRL-DSL-SYMBOLIC-SHAPE-CONTRACT.md`
- `doc/WHIRL-DSL-SHAPE-SP9-CERTIFICATION.md`
- `doc/WHIRL-DSL-SHAPE-SP10-CERTIFICATION.md`
- `doc/IPA-DSL-SHAPE-PROPAGATION-TODO.md` if cross-links need correction

Required checks:

- Every cited commit resolves.
- Stage status, ownership, trigger effects, and acceptance matrices agree across
  all authoritative documents.
- Current per-PU requirements and future IPA requirements are not mixed.
- Every `passed` label maps to an executable asserted test and retained output.

Exit criteria:

- One internally consistent authority chain defines the repaired endpoint.
- No certification claim exceeds available evidence.
- The endpoint diff passes whitespace, no-tab, and link checks.

### WS8: Rebuild and Publish the Full Acceptance Matrix

Findings addressed: P1-02, P1-03, P1-04, and all remaining unverified gates.

Implementation approach:

1. Validate B0 from a clean repair commit in the required Ubuntu 20/LLVM 11
   reference lane. Retain the later Ubuntu 24 overlay run as supplemental
   diagnostic evidence, not as a substitute.
2. Rebuild affected libraries, backend binaries, IR tools, and shared
   consumers from a clean checkout of the repaired commit.
3. Run the complete SP7, SP9, and SP10 matrices, not only focused source and
   syntax checks.
4. Publish immutable, commit-pinned CI artifacts or an equivalently durable
   reviewer-accessible bundle.

Required lanes:

- Focused positive and negative tests from WS1 through WS6.
- `-O0`, `-O1`, `-O2`, `-O3`, and explicitly scoped `-ipa` behavior.
- ResNet-20 as the master-plan primary full-model case. Llama prefill, decode,
  symbolic attention, and multi-PU cases remain supplemental PR 137 rule
  regressions and must not be reported as master-plan milestone completion.
- FHE input-state and ownership behavior.
- Non-DSL WHIRL no-op behavior.
- Previous and current reader compatibility. Because this repair prohibits a
  representation change, mapped-image layout and structural readability may
  not be narrowed when a direction fails. Such a failure blocks the gate or
  requires a separate versioned-IR proposal.
- Required target configurations.
- Checkpoint read/write, normal backend startup, and `whirl2c`.
- Rebuild and symbol audit of `be`, `be.so`, `lw_inline`, and other affected
  consumers.
- Link validation for each supported touched copied-source product. Until the
  modified Fortran and JFE products run in a supported lane, describe their
  status as static source-list consistency only, not linked build closure.
- Before and after binary WHIRL for each transformation, independently reopened
  with `ir_b2a -st -src`, plus the raw diff.
- A full same-process native suite after external X1 is resolved, or a strict
  base/head differential result that remains explicitly blocking for native
  lifecycle and master-plan readiness. Subprocess isolation is not a repair.

Required artifact manifest:

- Exact source commit and dirty-state declaration.
- Exact commands and environment variables.
- Compiler, linker, operating-system, target, and container identities.
- Per-lane exit status and machine-readable summary.
- `.B`, `.T`, phase traces, diagnostics, model outputs, assembly where
  applicable, and symbol-audit outputs.
- SHA256 values for every retained file.

Exit criteria:

- All required lanes pass in a reproducible environment.
- Artifacts are accessible to reviewers and attached to required PR checks.
- No result depends only on a source grep or an unretained local path.
- Passing PR 137 shape certification is not labeled as completion of M0, M1,
  M2, or any later FHE milestone.

## 8. Project Decisions Required Before or During Repair

The master plan and repository policy already fix per-PU scope, no earlier-PU
rewind, no valid artifact publication after failure, compatibility, domain
ownership, and optimization-level limits. Those are constraints, not optional
project defaults. The following remaining design choices should be made
explicitly rather than encoded implicitly during implementation:

1. Shape-only authorization: define the exact `logical_shape` delta while
   requiring all other stored descriptor and domain state, including unknown
   state, to remain unchanged. Type-equivalence metadata remains a separate
   concern.
2. Symbol provenance API: decide how syntax parsing preserves mapped forms and
   what active-PU or reviewed boundary evidence is required before proof use.
3. Shape-effect storage: use per-pass registration as recommended here, or an
   intentionally closed and audited stage table. In either case every pass
   still needs the full optimization/scope/effect/control contract.
4. Boundary validator composition: define the single active-PU gate and its
   admission and successful-exit placement.
5. Certification delivery: choose required CI artifacts or another immutable,
   reviewer-accessible store. Local `/private/tmp` paths are not sufficient.
6. PR boundary: decide whether to keep the repaired SP2-SP10 work combined or
   split it, and record the waiver from the original serial milestone plan if
   it remains combined.
7. X1 disposition: land a separate develop-based DST lifecycle fix before the
   full native gate, or record the strict differential limitation without
   treating the native lifecycle or a master-plan milestone as complete.

## 9. Full Rerun and Approval Gate

PR 137, or a repaired successor, should be reconsidered for approval only when
all of the following are true:

1. P1-01 is fixed with complete shape-only authorization preflight.
2. Foreign logical values and foreign symbolic qualifiers are rejected before
   unauthorized mutation or proof use.
3. All successful shape-driver paths execute the full active-PU boundary gate.
4. The SP5 negative and per-write rollback matrix passes.
5. Insertion-order permutation and multi-PU isolation tests pass.
6. Each registered transformation has an enforced minimum optimization level,
   maximum scope, canonical-form prerequisite, shape effect, invalidation
   behavior, and control; missing or unknown declarations do not run.
7. All authoritative documents and commit references agree with the repaired
   implementation.
8. A clean supported build completes, including affected shared consumers and
   symbol audits; B0 passes the Ubuntu 20/LLVM 11 reference lane, and any
   touched but unbuilt Fortran/JFE product remains explicitly unverified.
9. The full SP7, SP9, and SP10 runtime and compatibility matrices pass.
10. Immutable artifacts contain exact commands, environment identity,
    checksums, `.B` and `.T` files, before/after evidence, diagnostics, and
    machine-readable results.
11. Required PR checks are present and green.
12. The repaired head passes aggregate and relevant per-commit
    `git diff --check`, no-tab checks, focused repros, linked tests, and a fresh
    review.
13. Reader/layout compatibility is not narrowed without an approved versioned
    migration, ResNet-20 remains the primary full-model case, and no shape
    result is promoted into an M0-M8 completion claim.
14. X1 is either resolved externally for the full native suite or retained as
    an explicit base-owned differential limitation; no DST product fix enters
    this repair branch.

Until those gates are complete, clean Git merge mechanics are insufficient and
the appropriate decision remains: do not approve yet.

## 10. Implementation Status

| Item | Current status |
| --- | --- |
| Frozen PR review | Complete at `7cac0d86...`; 5 Pass, 11 Partial, 1 Fail unchanged |
| Repair branch baseline | `6eada12d4a78623726711a2922642febf231174d` |
| A0 master-plan reconciliation | Authority order and the no-representation boundary are specified for review; acceptance is required before WP0 |
| B0 build dependency closure | Implemented at `6eada12d...`; Ubuntu 24 supplemental evidence retained; Ubuntu 20/LLVM 11 required lane pending |
| X1 repeated native-export DST lifecycle | Attributed to base; no product fix in this branch; external follow-up or strict differential policy pending |
| WP0 semantic freeze | Planned; no semantic freeze commit is present at this baseline |
| WS1-WS6 semantic and test repair | Planned; P1-01 and P2-01 through P2-08 remain open or unverified as classified in Section 4 |
| WS7 authority/claim reconciliation | Planned; P2-07, P3-01, P3-02, and the certification wording gaps remain open |
| WS8 full certification | Not started |

The recommendation remains: do not approve yet. At this baseline, no semantic
repair has reclassified any live or unverified endpoint finding, and no
WP0-WP10 exact-snapshot focused evidence has been established. B0, X1, the
planned workstreams, D8, and immutable aggregate reviewer-accessible
certification retain the boundaries described above.
