# PR 137 Concrete Repair Execution Plan

## Document Status

This document turns the frozen PR 137 findings into an executable repair plan.
The frozen review remains anchored at `7cac0d86...`; this design-only baseline
is reconstructed for repair endpoint
`6eada12d4a78623726711a2922642febf231174d`, after the B0 build dependency
closure and before WP0 or any semantic repair implementation. B0 is not a
semantic repair, and its required reference validation remains pending.

A0 and WP0 through WP10 are open or planned at this snapshot. This document
does not rewrite a frozen finding or commit verdict, change a broad
certification claim to complete, or approve PR 137. It is a semantic
reconstruction of the pre-WP0 plan state, not a claim of byte-identical
historical recovery.

Current recommendation: do not approve PR 137 until Gate 6 is complete.

## 1. Baseline, Scope, and Non-Goals

| Item | Value |
| --- | --- |
| Repair worktree | `/home/zikai/workspace/open64-workspace/open64-pr137-fixes` |
| Repair branch | `codex/pr137-review-fixes` |
| Reviewed base, excluded | `590bdf3d58944cb93aafd89480a5590a863ded60` |
| Reviewed PR head, included | `7cac0d86b6548805b53c05675ffb7e1dff9395fa` |
| Design-baseline repair endpoint | `6eada12d4a78623726711a2922642febf231174d` |
| Reviewed range | `590bdf3d58944cb93aafd89480a5590a863ded60..7cac0d86b6548805b53c05675ffb7e1dff9395fa` |
| Range shape | 17 linear commits, no merge commits |
| Post-review foundation | B0 build dependency closure implemented; required reference validation pending |
| Semantic implementation status | A0 and WP0-WP10 are open or planned; no semantic work package is complete at this snapshot |
| Initial approval status | Do not approve |

All implementation must preserve these boundaries unless a separate design
change is approved:

1. Shape refinement is owned by the compiler and is scoped to the active PU.
2. VHO, common code, and a REGION pass must not activate or traverse another
   PU. Coordinated cross-PU propagation remains future IPA work.
3. The repair must not add a WHIRL opcode, `TY_KIND`, ELF section, mapped-image
   record, persisted generation table, or Python dependency in the backend.
4. Existing binary WHIRL compatibility remains required. Any contrary need is
   a separate versioned-IR proposal and is outside this plan.
5. The repair must not add `DSL_Builder_*` or JsonCpp linkage to `be.so`.
6. A failed PU or pipeline may not publish a final artifact. It does not cause
   an already completed earlier PU to be rolled back in memory.
7. Canonical type interning is not a transactional write. Rollback restores WN,
   ST, managed-value, REGION, and generation references; an unused interned type
   may remain and must be reused by a repeated attempt.
8. Source grep, syntax-only compilation, and fixed `passed` labels do not count
   as linked runtime certification.

Non-goals are a generic cross-PU symbolic import protocol, IPA propagation,
new model features, performance tuning, unrelated backend modernization, and
repair of the Ubuntu 24/GCC 13 build outside a separately reviewed portability
change.

### 1.1 Governing Authority

The user-designated highest architecture and overall implementation authority
is `doc/DSC_FHE_Compiler_Architecture_and_Integration_Plan_v0.10.md`. The root
`AGENTS.md` is an orthogonal and mandatory repository policy. `WHIRL.pdf` is
the normative architectural baseline for WHIRL semantics and the existing
mapped-image layout, readers, writers, and printers. An apparent conflict among them stops
implementation until it is reconciled.

The focused FHE implementation plan, shape design and implementation plan,
stage contracts, this repair plan, and certification documents are
subordinate. They may stage or narrow work but may not alter the master plan's
domain ownership, phase order, optimization-level policy, or milestones. The
master plan contains proposed APIs and option names; this document must not
misrepresent them as implemented product interfaces.

### 1.2 Master-Plan Milestone Crosswalk

| Repair scope | Master-plan relationship | Maximum permitted claim |
| --- | --- | --- |
| WP0-WP1 and WP8 shared tensor/shape contracts | Common Compiler Substrate; supports Phase 0 and M0 review | Prerequisite evidence only; M0 is not complete. |
| WP2-WP6 mapped metadata, ownership, and gatekeeping | Supports M1 metadata capture and M2 validation-only behavior | Shared infrastructure only; M1 and M2 are not complete. |
| WP7 pipeline lifecycle and WP10 certification | PR 137 shape correctness | Does not implement or complete M3-M8. |

ResNet-20/CIFAR-10 remains the master plan's primary first full-model vertical
slice. Llama tests remain supplemental PR 137 regressions for existing shape
rules and claims. Shape certification must not duplicate the master plan's
Section 6.4 FHE encryption/type propagation table or be reported as FHE
milestone completion.

Command outlines in this document use these fixed local variables. CI replaces
the build and artifact roots with its mounted immutable paths and records the
resolved values in the manifest.

```sh
export OPEN64_REPAIR_SRC=/home/zikai/workspace/open64-workspace/open64-pr137-fixes
export OPEN64_REPAIR_BUILD=/home/zikai/workspace/open64-workspace/pr137-build/build
export OPEN64_REPAIR_ARTIFACTS=/home/zikai/workspace/open64-workspace/pr137-artifacts
```

Certification runs use a fresh clean checkout of the repaired commit. The
planning worktree itself is not evidence of a clean certification run.

Every command below is a planned command outline, not a command already
verified by this document. A path, target, environment selector, or helper
marked `New` or `Proposed` does not exist at the reviewed head and must be
implemented before that outline can run. Certification records the final exact
argv and working directory instead of copying an outline as evidence.

## 2. Decision Gate

The master plan and repository policy already fix per-PU ownership,
optimization-level limits, domain visibility, no earlier-PU rewind, atomic
artifact publication, and binary compatibility. Those are hard constraints,
not recommended defaults. The table separates remaining local design choices
from fixed policy and prerequisites.

| ID | Decision | Recommended default | Current status |
| --- | --- | --- | --- |
| D1 | Shape-only delta authorization | Permit only monotonic `logical_shape` dimension refinement with unchanged rank. Preserve every other stored tensor/type/descriptor and domain field, including unknown state. Keep type equivalence and update authorization separate. | Open; blocks WP1 |
| D2 | Symbol provenance | Preserve syntactically valid mapped forms for reading and printing, but require active-PU ownership or a reviewed host-independent interface mapping before equality/proof use. Coordinated opposite-side refinement remains IPA-owned. | Open; blocks WP6 |
| D3 | Pass execution/effect contract | Every pass declares minimum optimization level, maximum scope, required canonical form, effect, invalidation behavior, and control. Per-pass effect storage is recommended for the current API gap but is not mandated by the master plan. | Open; blocks WP7 |
| D4 | Active-PU boundary gate | Add one composed gate for call ABI, PU interface, and REGION validation. Run it at admission and before every successful shape-driver return. | Open; blocks WP2 |
| D5 | Multi-PU failure semantics | Keep independent per-PU transactions. A later failure does not rewind an earlier PU in memory, but it prevents final artifact publication. | Policy fixed |
| D6 | Certification storage | Publish commit-pinned immutable CI artifacts with a manifest and SHA256 checksums. A local `/private/tmp` path is never the sole evidence. | Decision pending |
| D7 | Delivery shape | Deliver six focused, stacked repair PRs as specified in Section 5. Record a waiver if the project instead keeps one combined SP2-SP10 PR. | Decision pending |
| D8 | Representation prerequisite | Obtain and review the relevant `WHIRL.pdf` baseline before approving type/representation, reader, writer, or printer claims. Until then, make no mapped-image change; a conflict with the master plan stops work for reconciliation. | Blocking prerequisite |

## 3. Dependency DAG and Execution Order

```text
A0 Master-plan and authority reconciliation
 |
 v
WP0 Semantic freeze
 |\
 | +--------------------> WP2 Active-PU boundary
 v                              |
WP1 Shape-only authorization    |
 |                              |
 +------> WP3 Complete preflight <---+
                 |
                 v
              WP4 Journal certification
                 |\
                 | +------> WP5 Multi-PU driver isolation
                 | +------> WP7 Pass execution/effects

WP0 + WP2 ------> WP6 Symbol trust boundary -> WP8 Solver/rule matrix

WP1 through WP8 -> WP9 Authority and traceability -> WP10 Certification

B0 Build closure at 6eada12d ---------------------> WP10 / Gate 4
X1 Develop-owned native DST lifecycle ------------> full native suite / M1 readiness
```

A0 and WP0 are blocking before semantic implementation. WP1 and WP2 may be
developed in parallel after WP0. WP6 depends on the explicit active-PU context
contract from WP2, and WP8 waits for WP6's final proof-use semantics. WP5 and
WP7 may proceed after WP4. WP9 may prepare claim wording early but cannot
record final repaired behavior until WP1 through WP8 are complete. WP10 is the
final integration work package.

B0 is already implemented at `6eada12d...`; its Ubuntu 24 evidence is
supplemental, and its Ubuntu 20/LLVM 11 reference lane remains pending. X1 is a
base-owned DST lifecycle issue and must not add product code to this repair
series. A full native-suite claim either waits for an external develop-based
X1 fix or remains explicitly limited by a strict base/head differential result.

## 4. Work Packages

### WP0: Freeze the Repair Semantics

- Findings: P2-07, P3-01, P3-02; policy prerequisites for P1-01, P2-06, and
  P2-08.
- Goal: establish one unambiguous per-PU, rank-preserving, fail-closed contract
  before code changes diverge.
- Dependencies: A0; D1-D4 block their owning semantic work packages; D5 is
  fixed policy; D6-D7 are delivery decisions that may be assigned to WP10 and
  Section 5 without blocking WP1-WP8; D8 blocks representation claims; no code
  dependency.
- Exact files and contracts:
  - `doc/WHIRL-DSL-SHAPE-RETYPING-CONTRACT.md`
  - `doc/WHIRL-DSL-SYMBOLIC-SHAPE-CONTRACT.md`
  - `doc/WHIRL-DSL-SHAPE-PROPAGATION-DESIGN.md`
  - `doc/WHIRL-DSL-SHAPE-PROPAGATION-IMPLEMENTATION-PLAN.md`
- Implementation:
  1. Make the master-plan authority and M0-M2 support-only crosswalk explicit
     in every governing shape document.
  2. State that SP5 changes approved `logical_shape` dimensions only, preserves
     rank, and preserves every other stored tensor/descriptor/domain field.
     Keep type equivalence separate from update authorization.
  3. State that syntax parsing may preserve mapped symbolic forms, while
     equality/proof use requires active-PU ownership or reviewed interface
     evidence.
  4. Replace all-PU rollback with per-invocation atomicity, cross-PU isolation,
     and no publication after compilation failure.
  5. Require the complete pass execution/effect contract. Record per-pass
     effect storage as the recommended local design, not master-plan text.
  6. Mark SP9 and SP10 repair certification as pending.
- Compatibility and risks: this is normative clarification. A disagreement is
  resolved at D1-D8, not hidden in implementation. No binary layout changes.
- Positive tests: all four documents describe identical owner, identity,
  rollback, and effect rules.
- Negative tests: reject wording that restores earlier PUs after a later
  failure, unconditionally accepts a qualified symbol, or treats a stage switch
  as a pass declaration.
- Command outline:

  ```sh
  git diff --check
  rg -n "later PU|all-PU|qualified symbol|shape effect|rank" \
    doc/WHIRL-DSL-SHAPE-*.md
  ```

- Artifacts: `wp0-document-diff.patch` and
  `wp0-contract-consistency.log`.
- Exit criteria: D1-D4 are confirmed before their owning semantic work; D6-D7
  have an explicit later disposition without blocking unrelated semantic work;
  D5 is applied as fixed policy; D8 remains a blocking prerequisite for
  representation claims. Deferral never authorizes
  an affected implementation PR to use an unconfirmed default. The four shape
  documents and the master-plan crosswalk contain no contradictory semantics.
- Suggested commit subject: `docs(shape): freeze PR137 repair semantics`

### WP1: Enforce Complete Shape-Only Authorization

- Findings: P1-01 and the identity part of P2-03.
- Goal: make `DSL_IR_Retype_Type_Valid()` authorize only a complete,
  rank-preserving `logical_shape` refinement before any transaction write,
  independently of the narrower type-equivalence relation.
- Dependencies: WP0 and D1.
- Exact files, functions, and types:
  - `osprey/common/com/symtab.cxx`: `Tensor_KV_Is_Type_Identity_Key()`,
    `Tensor_KV_Are_Equivalent()`, `TY_Tensor_Type_Matches_Refinement()`, and
    `TY_Intern_Refined_Tensor_Type()`.
  - `osprey/common/com/symtab.h`: public declaration for a narrow helper such as
    `TY_tensor_is_shape_only_refinement()`.
  - `osprey/common/com/dsl_ir_rewrite.cxx`:
    `DSL_IR_Retype_Type_Valid()`.
  - `osprey/common/com/tests/dsl_builder_contract_test.cxx`.
  - Relevant types: `TY_DSL_KV`, `TY_DSL_BIND_STATE`,
    `TY_TENSOR_EXTENSION_STORE`, `TY_TENSOR_TYPE_CORE_REFINEMENT`, and
    `TENSOR_DESCRIPTOR_RECORD`.
- Implementation:
  1. Reuse the tensor type owner's equivalence services, but add a separate,
     narrow shape-only authorization helper rather than treating equivalence as
     update authority.
  2. Compare `TY_KIND`, element type, alignment, canonical state, rank, and
     every non-`logical_shape` stored field. Unknown fields fail closed.
  3. For every KV, compare key presence, declared/bound state, and bound value.
     Traits, layout, placement, memory, quantization, semantic role, encryption
     descriptor references, and lineage remain unchanged even when a field is
     outside type equivalence. Compiler metadata is not added to type
     equivalence; whether stored inside or outside the tensor descriptor, it is
     untouched by this transaction.
  4. Permit `logical_shape` changes only when rank is unchanged and every known
     old dimension is preserved in the refined fact.
  5. Keep `TY_Intern_Refined_Tensor_Type()` lookup-before-create behavior. Match
     an interned candidate to the requested refined type only with the tensor
     owner's canonical type-equivalence service. Use shape-only authorization
     only for the old-to-requested delta, never as interning equality.
  6. Treat a rank-changing flatten or reshape as creation and typing of a
     distinct result value, never as an SP5 exception for same-value retyping.
- Compatibility and risks: an internal caller that previously supplied a
  malformed refined type will now fail closed. The first-party planner should
  remain compatible because it derives from `expected_old_ty`. The risk is an
  omitted custom KV or a metadata key incorrectly classified as identity.
- Positive tests: preserve `shape_contract=v1`; refine pending dimensions;
  preserve lineage, encryption/domain references, and out-of-type compiler
  metadata unchanged; reuse an existing refined type without
  `Ty_tab` growth; keep unrelated users of the old `TY_IDX` unchanged.
- Negative tests: custom KV changed, added, removed, declared-to-bound,
  bound-to-declared, and bound-value changed; independently change dtype,
  traits, layout, sharding, placement, memory, quantization, alignment, element
  type, kind, semantic role, lineage, encryption reference, unknown state, and
  rank.
- Command outline:

  ```sh
  make -C "$OPEN64_REPAIR_BUILD/osprey/targdir/ir_tools" \
    dsl_builder_contract_test dsl_shape_refine_contract_test
  "$OPEN64_REPAIR_BUILD/osprey/targdir/ir_tools/dsl_builder_contract_test"
  OPEN64_DSL_SHAPE_IDENTITY_REPRO=1 \
    "$OPEN64_REPAIR_BUILD/osprey/targdir/ir_tools/dsl_shape_refine_contract_test"
  ```

  `OPEN64_DSL_SHAPE_IDENTITY_REPRO` exists in the retained external review
  patch, not in the reviewed source tree. WP1 ports it as a repaired regression
  selector before running this outline.

- Artifacts: test stdout/stderr, old/refined type dumps, type-table counts, and
  the reversed identity repro result.
- Exit criteria: every non-shape difference is rejected before the first write;
  WN, ST, value, REGION, and generation state are unchanged; and
  `rollback_count=0`.
- Suggested commit subject:
  `fix(shape): enforce complete shape-only retype authorization`

### WP2: Centralize Active-PU Boundary Validation

- Findings: P2-04 and the boundary prerequisite for P2-02 and P2-05.
- Goal: make every successful shape-driver path prove the active PU's call ABI,
  PU interface, and REGION relationships.
- Dependencies: WP0 and D4.
- Exact files, functions, and types:
  - `osprey/common/com/dsl_ir_image.h`: proposed
    `DSL_IR_Image_Validate_Active_PU_Boundaries()`.
  - `osprey/common/com/dsl_ir_rewrite.cxx`:
    `DSL_Call_ABI_Image_Validate_PU()` and
    `DSL_PU_Interface_Image_Validate_PU()`.
  - `osprey/common/com/dsl_region.cxx` and
    `osprey/common/com/dsl_region.h`:
    `DSL_Region_Verify_PU()`.
  - `osprey/be/vho/dsl_shape_refine.cxx`:
    `VHO_DSL_Shape_Refine_Program_Unit()` and
    `VHO_DSL_Shape_Refinement_Mark_Current()`.
  - `osprey/be/vho/tests/dsl_shape_refine_contract_test.cxx`.
- Implementation:
  1. Compose the three existing validators in a single active-PU helper with a
     stable diagnostic order. Pass the selected PU identity and validation
     context explicitly; the common helper must not discover or enlarge scope
     by traversing global PU state.
  2. Run it at shape-driver admission, before every mark-current operation, and
     in retype pre- and post-validation. Define separate counters for the
     driver admission/exit gates and transaction defense-in-depth calls so the
     expected count is unambiguous.
  3. Cover no-native, refinement-disabled, no-request, already-complete,
     ordinary-refinement, and REGION paths.
  4. Validate only `Current_PU_Info` and `PU_Info_proc_sym(pu_info)`; do not
     traverse `PU_Info` or switch symbol tables.
  5. Keep this common shape gate separate from CNN and FHE domain gates. It
     validates shared shape and boundary evidence but does not consume, hide,
     lower, or duplicate domain semantics before their owning gatekeepers.
- Compatibility and risks: stale boundary evidence that was silently accepted
  will now reject the PU. This is intended fail-closed behavior. Repeated full
  scans may affect compile time, so measure without weakening the gate.
- Positive tests: each of the six success paths records exactly one driver
  admission and one driver successful-exit gate. An ordinary transaction also
  records its separately named retype precheck and postcheck; legal REGION and
  interface rows remain accepted.
- Negative tests: independently stale call actual, formal, hidden result,
  function return, PU interface, and REGION row; none may mark the generation
  current.
- Command outline:

  ```sh
  make -C "$OPEN64_REPAIR_BUILD/osprey/targdir/ir_tools" \
    dsl_shape_refine_contract_test
  "$OPEN64_REPAIR_BUILD/osprey/targdir/ir_tools/dsl_shape_refine_contract_test"
  OPEN64_DSL_SHAPE_TEST_CORRUPT_BOUNDARY=all \
    "$OPEN64_REPAIR_BUILD/osprey/targdir/ir_tools/dsl_shape_refine_contract_test"
  ```

  `OPEN64_DSL_SHAPE_TEST_CORRUPT_BOUNDARY` is a proposed test selector.

- Artifacts: per-path validator counters, diagnostics for each corrupt row, and
  generation snapshots.
- Exit criteria: all successful returns have executable evidence for the same
  composed gate, and no fast path bypasses it.
- Suggested commit subject:
  `fix(shape): validate active PU boundaries on every success path`

### WP3: Complete Owner-Safe Retype Preflight

- Findings: P2-02 and the remaining preflight portion of P2-03.
- Goal: reject every malformed, foreign, escaped, or boundary-participating
  request before opening or applying a mutation journal.
- Dependencies: WP1 and WP2.
- Exact files, functions, and types:
  - `osprey/common/com/dsl_ir_rewrite.cxx`:
    `DSL_IR_Image_Value_Belongs_To_PU()`, `DSL_IR_Retype_Preflight()`,
    `DSL_IR_Refine_Native_Value_Types()`, `DSL_IR_Retype_Scan_Tree()`, and
    `DSL_IR_RETYPE_JOURNAL`.
  - `osprey/common/com/dsl_ir_image.h`:
    `DSL_IR_VALUE_TYPE_REFINEMENT_REQUEST` and
    `DSL_IR_VALUE_TYPE_REFINEMENT_RESULT`.
  - `osprey/be/vho/tests/dsl_shape_refine_contract_test.cxx`.
- Implementation:
  1. Immediately after loading the logical value, call
     `DSL_IR_Image_Value_Belongs_To_PU(value, owner_pu_st)`.
  2. Normalize the complete request array by owner and value ID. Reject
     duplicate values, duplicate ST targets, no-ops, conflicts, and stale
     expected types before writes.
  3. Preallocate journal storage, then validate in this order: active owner,
     canonical type, logical owner, physical projections, logical references,
     call/interface/constants/extensions, REGION dependencies, and cross-request
     collisions.
  4. Extend the tree scan and image checks to reject second definitions, LDA or
     indirect use, aliases, unsupported symbol-bearing WN, impure/effectful
     producers, constants, model inputs, external payloads, TCON backing, FHE
     relations, and unsupported REGION profiles.
  5. Preserve strict post-validation as defense in depth.
- Compatibility and risks: compile-time scanning grows, and a formerly accepted
  unsupported internal request will be diagnosed earlier. No successful legal
  path should change.
- Positive tests: one and multiple independent requests; all direct LDID/STID,
  ST, and managed-value projections agree; eligible REGION rows stay unchanged;
  equivalent types are reused.
- Negative tests: invalid/inactive owner, foreign value with colliding local
  `ST_IDX`, invalid ID or type, duplicate/conflicting request, stale value/STID/
  ST/LDID, unsupported WN or alias, effectful producer, constant/input/payload,
  call actual/output, formal/hidden result/return/prototype/TYLIST, shared callee,
  FHE or auxiliary image, and inconsistent REGION relation.
- Command outline:

  ```sh
  make -C "$OPEN64_REPAIR_BUILD/osprey/targdir/ir_tools" \
    dsl_shape_refine_contract_test
  OPEN64_DSL_SHAPE_FOREIGN_OWNER_REPRO=1 \
    "$OPEN64_REPAIR_BUILD/osprey/targdir/ir_tools/dsl_shape_refine_contract_test"
  OPEN64_DSL_SHAPE_PREFLIGHT_MATRIX=1 \
    "$OPEN64_REPAIR_BUILD/osprey/targdir/ir_tools/dsl_shape_refine_contract_test"
  ```

  `OPEN64_DSL_SHAPE_FOREIGN_OWNER_REPRO` exists in the retained external review
  patch, not in the reviewed source tree. WP3 ports it as a repaired regression
  selector. `OPEN64_DSL_SHAPE_PREFLIGHT_MATRIX` is a proposed selector.

- Artifacts: a machine-readable case matrix with pre/post state hashes,
  diagnostic code, write count, rollback count, and generation values.
- Exit criteria: all preflight negatives report zero writes and
  `rollback_count=0`; the foreign-owner collision repro cannot transiently
  mutate either PU.
- Suggested commit subject:
  `fix(shape): complete owner-safe SP5 request preflight`

### WP4: Certify and Repair Rollback at Every Journaled Write

- Findings: P2-03.
- Goal: first certify the existing journal's exact reverse restoration after
  failure at every post-write position, while distinguishing it from zero-write
  preflight rejection; change production code only when a counterexample is
  proved.
- Dependencies: WP3.
- Exact files, functions, and types:
  - `osprey/common/com/dsl_ir_rewrite.cxx`: instrument the existing
    `DSL_IR_Retype_Apply()` journal first; repair its smallest demonstrated gap
    only if the matrix fails.
  - `osprey/common/com/dsl_ir_image.h`: result counters only if needed; no
    mapped-image field.
  - `osprey/be/vho/tests/dsl_shape_refine_contract_test.cxx`.
  - `osprey/common/com/tests/dsl_shape_refine_test.sh`.
  - Conditional runtime-only journal entry, only if existing records are
    insufficient:
    `DSL_IR_RETYPE_WRITE { kind, target, old_ty, new_ty, applied }`.
- Implementation:
  1. Inventory the current write records and rollback order for every LDID,
     definition, `ST_type`, and managed-value mutation.
  2. Add a test-only deterministic hook,
     `OPEN64_DSL_SHAPE_RETYPE_TEST_FAIL_AFTER_WRITE=N`, after each write.
  3. Add separate hooks for image, boundary, REGION, gatekeeper, and shared-shape
     post-validation failure.
  4. Snapshot all observable projections and generation state before each run
     and prove exact reverse restoration with the current implementation.
  5. Only if a failure is proved, extend the existing journal with the minimum
     missing target/old/new/applied state. Do not build a parallel transaction
     engine.
  6. Reuse, but do not attempt to delete, a type interned before transaction
     application.
- Compatibility and risks: failure hooks must be inert unless explicitly set.
  A partial revert of journal and preflight code is unsafe. An unreferenced
  interned type is acceptable; unbounded new types on repeated failure are not.
- Positive tests: single and multi-request success; success after an injected
  failure; binary write, reopen, and `ir_b2a -st -src` inspection.
- Negative tests: failure before write 1, after each write ordinal, at each
  request boundary, after the final request, at every post-validator, and on
  repeated runs.
- Command outline:

  ```sh
  OPEN64_DSL_SHAPE_RETYPE_TEST_FAIL_AFTER_WRITE=0 \
    "$OPEN64_REPAIR_BUILD/osprey/targdir/ir_tools/dsl_shape_refine_contract_test"
  OPEN64_DSL_SHAPE_RETYPE_TEST_FAIL_AFTER_WRITE=all \
  OPEN64_DSL_SHAPE_SP10_ARTIFACT_DIR="$OPEN64_REPAIR_ARTIFACTS/wp4" \
  OPEN64_DSL_SHAPE_REFINE_TEST="$OPEN64_REPAIR_BUILD/osprey/targdir/ir_tools/dsl_shape_refine_contract_test" \
  OPEN64_IR_B2A="$OPEN64_REPAIR_BUILD/osprey/targdir/ir_tools/ir_b2a" \
    osprey/common/com/tests/dsl_shape_refine_test.sh
  "$OPEN64_REPAIR_BUILD/osprey/targdir/ir_tools/ir_b2a" -st -src \
    "$OPEN64_REPAIR_ARTIFACTS/wp4/success.after.B" \
    "$OPEN64_REPAIR_ARTIFACTS/wp4/success.after.T"
  ```

- Artifacts: before/after snapshots, injection ordinal report, rollback order,
  `.B`, `.T`, and raw before-after diff.
- Exit criteria: for N writes, all N+1 boundary injections restore the exact
  snapshot; repeated failure leaks no journal, generation, or additional type;
  and failure publishes no final `.B`.
- Suggested commit subject:
  `fix(shape): make SP5 rollback exact at every journal write`

### WP5: Certify Driver-Owned Multi-PU Isolation

- Findings: P2-05 and P3-01.
- Goal: provide executable evidence that the backend invokes the per-PU entry
  once per selected PU and that local indices never create cross-PU mutation.
- Dependencies: WP2 through WP4 and D5.
- Exact files and functions:
  - `osprey/be/be/driver.cxx`: `Preprocess_PU()` and its
    `VHO_DSL_Shape_Refine_Driver()` call sites.
  - `osprey/be/vho/dsl_shape_refine.cxx`:
    `VHO_DSL_Shape_Refine_Driver()`.
  - `osprey/be/vho/tests/dsl_shape_refine_contract_test.cxx`.
  - `osprey/common/com/tests/dsl_multi_pu_artifact_test.sh`.
  - New `osprey/common/com/tests/dsl_shape_multi_pu_driver_test.sh`.
- Implementation:
  1. Build two named PUs that deliberately reuse local `ST_IDX` values.
  2. In the unit fixture, select each PU through the driver-owned context and
     call the same per-PU entry.
  3. In the pipeline fixture, run the real `be -O0` on a multi-PU `.B` and use
     PU names and counters to prove one invocation for each selected PU.
     Classify the exercised shape work as correctness-required verification or
     refinement needed for legal lowering; `-O0` must not run fusion,
     profitability rewriting, or cross-PU analysis.
  4. Test success/success, complete/refinable, and success/failure combinations.
  5. On later-PU failure, verify earlier state is not mutated and no validly
     named final backend or checkpoint output is published; do not invent an
     all-PU transaction. A harness may retain explicitly failure-labeled
     evidence, but must not call it a final `.B`.
- Compatibility and risks: the production driver should need no shape-owned PU
  traversal. Any new traversal or `Current_pu` switching is a design failure.
- Positive tests: two successful PUs, index collision, mixed complete/refined
  shapes, reopenable artifact, and exact invocation count.
- Negative tests: foreign request, later failure, success not masking failure,
  no cross-PU state delta, nonzero compiler exit, and absence of the validly
  named final backend or checkpoint artifact.
- Command outline:

  ```sh
  OPEN64_DSL_CONTRACT_TEST="$OPEN64_REPAIR_BUILD/osprey/targdir/ir_tools/dsl_builder_contract_test" \
  OPEN64_IR_B2A="$OPEN64_REPAIR_BUILD/osprey/targdir/ir_tools/ir_b2a" \
  OPEN64_DSL_MULTI_PU_DIR="$OPEN64_REPAIR_ARTIFACTS/wp5" \
    osprey/common/com/tests/dsl_multi_pu_artifact_test.sh
  OPEN64_BE="$OPEN64_REPAIR_BUILD/osprey/targdir/be/be" \
  OPEN64_DSL_TEST_ARTIFACT_DIR="$OPEN64_REPAIR_ARTIFACTS/wp5-driver" \
    osprey/common/com/tests/dsl_shape_multi_pu_driver_test.sh
  ```

- Artifacts: input and output `.B`/`.T`, per-PU logs and counters, state hashes,
  compiler exit status, and an atomic-publication report.
- Exit criteria: runtime evidence replaces source-order grep; each PU is
  independent and exactly once; failure never publishes a valid final image.
- Suggested commit subject:
  `test(shape): certify driver-owned multi-PU isolation`

### WP6: Separate Symbol Parsing from Owner-Aware Proof Use

- Findings: P2-06.
- Goal: preserve inspectable mapped symbolic forms while preventing untrusted
  serialized or frontend text from forging equality through a foreign PU
  qualifier.
- Dependencies: WP0, WP2, and D2.
- Exact files, functions, and types:
  - `osprey/common/com/dsl_shape.cxx` and
    `osprey/common/com/dsl_shape.h`:
    `DSL_Shape_Parse_Symbol()`, `DSL_Shape_Parse_Fact()`,
    `DSL_Shape_Normalize_Logical_Shape()`, `DSL_Shape_Fact_From_Type()`,
    `DSL_Shape_Solver_Add_Value()`, and `DSL_Shape_Collect_Constraints()`.
  - Proposed `DSL_Shape_Fact_From_Type_For_PU(owner, ty, fact)`.
  - `osprey/common/com/dsl_gatekeeper.cxx`:
    `DSL_GATEKEEPER_CONTEXT` and tensor admissibility checks.
  - `osprey/common/com/dsl_builder.cxx`.
  - `osprey/common/com/tests/dsl_builder_contract_test.cxx` and
    `osprey/common/com/tests/dsl_shape_symbolic_test.sh`.
  - `osprey/torch2whirl/` native bridge call sites only if required; its public
    Python API remains unchanged.
- Implementation:
  1. Keep syntax parsing and mapped-form preservation owner-independent. The
     reader and printer may represent a named or qualified form without
     treating it as trusted equality evidence.
  2. Introduce an owner-aware semantic-admission path. With an owner, bind an
     unqualified local symbol to that owner and admit an existing qualifier
     only if it matches or a reviewed call/interface mapping proves source and
     destination ownership.
  3. Make solver, gatekeeper, retype, and serialized type proof-use sites pass
     their active owner explicitly. A parse-only or print-only path never
     authorizes inference.
  4. Use an existing host-independent mapped-image owner identity. Never
     persist pointers, process addresses, allocation order, or a runtime-only
     provenance table.
  5. Do not add a generic cross-PU transformation. A per-PU pass may validate
     its visible call/formal/result mapping; coordinated opposite-side
     refinement remains future IPA work.
  6. Compare canonical symbol text only after semantic admission succeeds.
- Compatibility and risks: old artifacts with unexplained foreign qualifiers
  remain structurally readable and printable, but fail closed before proof use
  and require a release note. Local unqualified and same-owner shapes remain
  supported. No persisted constraint table or layout change is added.
- Positive tests: local unqualified, explicitly same-owner, reviewed boundary
  mapping, symbol-plus-constant, static, dynamic, pending, symbolic matmul, and
  symbolic attention; fresh-process `.B` reopen and `ir_b2a -st -src` printing
  before separate proof-use admission.
- Negative tests: owner zero with a name, active-owner mismatch, missing
  mapping, malformed/zero/short/long/overflow qualifier, and two unrelated
  symbols using a forged common string.
- Command outline:

  ```sh
  make -C "$OPEN64_REPAIR_BUILD/osprey/targdir/ir_tools" \
    dsl_builder_contract_test
  OPEN64_DSL_CONTRACT_TEST="$OPEN64_REPAIR_BUILD/osprey/targdir/ir_tools/dsl_builder_contract_test" \
  OPEN64_IR_B2A="$OPEN64_REPAIR_BUILD/osprey/targdir/ir_tools/ir_b2a" \
  OPEN64_DSL_SHAPE_SP8_ARTIFACT_DIR="$OPEN64_REPAIR_ARTIFACTS/wp6" \
    osprey/common/com/tests/dsl_shape_symbolic_test.sh
  make -C "$OPEN64_REPAIR_BUILD/osprey/targdir/torch2whirl" \
    python_native_test python_native_ir_tools_smoke
  ```

- Artifacts: parser case table, owner/provenance diagnostics, symbolic solver
  results, and frontend `.B`/`.T` if bridge code changes.
- Exit criteria: both retained foreign-qualifier repros fail at proof use unless
  reviewed owner evidence is present; parse/print inspection remains possible,
  all local SP8 forms remain valid, and owner identity is host-independent.
- Suggested commit subject:
  `fix(shape): bind symbolic qualifiers to the active PU`

### WP7: Make Pass Execution and Effects Auditable

- Findings: P2-08 and the executable portion of P1-04.
- Goal: make every registered pass declare a complete execution/effect contract
  and ensure that no subsequent pass observes stale shape state or executes
  outside its permitted optimization level and compilation scope.
- Dependencies: WP2, WP4, and D3.
- Exact files, functions, and types:
  - `osprey/be/vho/dsl_opt.h` and `osprey/be/vho/dsl_opt.cxx`:
    `VHO_DSL_OPT_SHAPE_EFFECT`, `VHO_DSL_Opt_Register_Pass()`,
    `VHO_DSL_Opt_Stage_Shape_Effect()`,
    `VHO_DSL_Opt_Shape_Effect_Invalidates()`,
    `VHO_DSL_Optimize_Program_Unit()`, and `VHO_DSL_OPT_RESULT`.
  - `osprey/be/vho/dsl_lower.cxx`: `VHO_DSL_Lower_Driver()`.
  - `osprey/be/vho/tests/dsl_opt_contract_test.cxx` and
    `osprey/be/vho/tests/dsl_lower_contract_test.cxx`.
  - `osprey/ir_tools/Makefile.gbase`.
- Implementation:
  1. Publish for each pass its minimum optimization level, maximum scope,
     required canonical form, controlling option, and validated shape effect.
     The effect is lifecycle metadata, not permission to execute.
  2. Recommended local design: store `{pass, effect, registered}` for each
     stage and extend `VHO_DSL_Opt_Register_Pass(stage, pass, effect)`. This
     storage choice is a repair recommendation, not text attributed to the
     master plan; an intentionally closed audited table remains a reviewable
     alternative.
  3. Validate the level, scope, canonical prerequisite, control, enum, stage,
     non-null pass, and duplicate registration. Audit every current pass before
     assigning an effect; do not blanket-label all passes merely to make them
     register.
  4. Require current shape state and an enabled in-scope pass at each entry.
     At `-O0`, run only correctness verification and required semantic lowering,
     never fusion or profitability transformation. Before executing a
     monotonic-refining, local-invalidating, or boundary-invalidating pass,
     invalidate with the matching structured trigger. A failed or partially
     applied pass leaves the generation stale, terminates the pipeline, and
     cannot publish output or feed another pass.
  5. After success: preserving leaves generation current; monotonic refining
     immediately refines under the symbolic-resolution trigger; local
     invalidation immediately refines under the VHO-optimization trigger; and
     boundary invalidation uses the PU/REGION restructuring trigger, completes
     the required boundary validation, and immediately refines before the next
     pass. Define and test whether the boundary gate is required both after the
     pass and after refinement; never omit WP2's final successful-exit gate.
  6. A missing, invalid, or unknown declaration prevents execution and emits a
     fail-closed diagnostic; it is not made safe merely by classifying it as
     invalidating.
  7. Remove aggregate pre-invalidation and end-only refinement that could let
     an adjacent pass consume stale facts.
- Compatibility and risks: all registration call sites change together.
  Generation and refinement frequency may increase. Missing, invalid, or
  unknown effect declarations must fail closed.
- Positive tests: all four effects, declared optimization/scope boundaries,
  fixed order, stable trigger identity, expected generation changes, and
  current state at the next pass.
- Negative tests: invalid stage/effect, null pass, duplicate, enabled but
  unregistered stage, effect mismatch, pass failure that leaves the generation
  stale, attempted next-pass execution after failure, below-level or
  out-of-scope execution, forbidden `-O0` optimization, and stale consumer.
- Command outline:

  ```sh
  make -C "$OPEN64_REPAIR_BUILD/osprey/targdir/ir_tools" \
    dsl_opt_contract_test dsl_lower_contract_test \
    dsl_shape_refine_contract_test
  "$OPEN64_REPAIR_BUILD/osprey/targdir/ir_tools/dsl_opt_contract_test"
  "$OPEN64_REPAIR_BUILD/osprey/targdir/ir_tools/dsl_lower_contract_test"
  ```

- Artifacts: registration table dump, per-pass generation timeline,
  invalidation/revalidation diagnostics, and four-effect test results.
- Exit criteria: one audited execution/effect contract controls each pass; all
  four classes have linked behavior evidence; unknown cases remain fail-closed
  and unexecuted; no declaration broadens PU/REGION scope.
- Suggested commit subject:
  `fix(vho): bind shape effects to pass registration`

### WP8: Make Solver Determinism and Rule Coverage Executable

- Findings: P2-01, historical H-01, and the arithmetic coverage portion of
  P1-03.
- Goal: prove insertion-order independence and executable positive, pending,
  and malformed behavior for every registered shape rule without duplicating
  FHE domain propagation owned by the master plan.
- Dependencies: WP6.
- Exact files, functions, and types:
  - `osprey/common/com/dsl_shape.cxx`:
    `DSL_SHAPE_SOLVER_VALUE`, `DSL_SHAPE_SOLVER_CONSTRAINT`,
    `DSL_Shape_Solver_Add_Value()`, `DSL_Shape_Constraint_Order()`,
    `DSL_Shape_Collect_Constraints()`, `DSL_Shape_Analyze_PU_Internal()`,
    `DSL_shape_rules`, and `DSL_Shape_Infer_Operator()`.
  - `osprey/common/com/tests/dsl_builder_contract_test.cxx`.
  - `osprey/common/com/tests/dsl_shape_solver_test.sh` and
    `osprey/common/com/tests/dsl_shape_sp9_certification_test.sh`.
- Implementation:
  1. Construct equivalent forward, reverse, and zigzag DAGs that vary value,
     node, and constraint insertion order while preserving logical identity and
     source position.
  2. Compare normalized fact sets, refinement requests, counters, diagnostic
     code/operator/source, and contradiction classification across permutations.
  3. Only if a mismatch is proved, define the smallest stable constraint/value
     ordering using existing solver and Open64 services. Preserve semantic
     priority; do not add a parallel rule engine.
  4. Give every registered operator/version a complete positive, the expected
     pending/refinable case, and a malformed negative. Retain rank-below-two,
     unequal known rank, bad contraction, and contradictory result matmul cases.
  5. Add a rule-owner matrix. Common shape rules certify logical shape only;
     CNN/FHE legality, encryption state, layout, scale, level, and keys remain
     with their master-plan owners. Do not copy Section 6.4 encryption/type
     propagation into `DSL_shape_rules` or its test oracle.
  6. Replace fixed `passed` labels with labels derived from asserted results.
- Compatibility and risks: stable ordering must not change semantic priority.
  Diagnostics may become more stable but should keep existing codes. The
  currently registered table is expected to contain 27 operator/version rules;
  the test must derive and assert the count from the table rather than silently
  accepting a changed count.
- Positive tests: add, arithmetic, relu/same/scatter, matmul v1/v2,
  residual/linear/logits, conv/batchnorm/pool, reshape/transpose,
  embedding/RMS/rotary/attention/SwiGLU, and symbolic cases across permutations.
  Domain-shaped fixtures assert shared shape results only and identify the
  separate CNN/FHE rule owner.
- Negative tests: malformed attributes/ranks/dimensions for every rule,
  contradictory seeds, historical malformed matmul cases, and stable
  diagnostics across permutations.
- Command outline:

  ```sh
  OPEN64_DSL_CONTRACT_TEST="$OPEN64_REPAIR_BUILD/osprey/targdir/ir_tools/dsl_builder_contract_test" \
  OPEN64_IR_B2A="$OPEN64_REPAIR_BUILD/osprey/targdir/ir_tools/ir_b2a" \
  OPEN64_DSL_SHAPE_SP3_ARTIFACT_DIR="$OPEN64_REPAIR_ARTIFACTS/wp8-solver" \
    osprey/common/com/tests/dsl_shape_solver_test.sh
  OPEN64_DSL_CONTRACT_TEST="$OPEN64_REPAIR_BUILD/osprey/targdir/ir_tools/dsl_builder_contract_test" \
  OPEN64_IR_B2A="$OPEN64_REPAIR_BUILD/osprey/targdir/ir_tools/ir_b2a" \
  OPEN64_DSL_SHAPE_SP9_ARTIFACT_DIR="$OPEN64_REPAIR_ARTIFACTS/wp8-sp9" \
    osprey/common/com/tests/dsl_shape_sp9_certification_test.sh
  ```

- Artifacts: normalized facts/refinements for each permutation, diagnostic
  diff, operator/version coverage JSON, and script logs.
- Exit criteria: all permutation outputs match; every registered rule has
  asserted positive and negative behavior; historical malformed matmul remains
  rejected; the rule-owner matrix contains no duplicate FHE propagation; no
  result is described as completion of M0-M8.
- Suggested commit subject:
  `test(shape): make solver order and operator coverage executable`

### WP9: Reconcile Authority, Claims, and Traceability

- Findings: P2-07, P3-01, P3-02, H-02, H-03, and claim wording associated with
  P1-02 through P1-04.
- Goal: make the governing documents describe the repaired implementation and
  only the evidence that actually exists.
- Dependencies: WP1 through WP8.
- Exact files:
  - `doc/WHIRL-DSL-SHAPE-PROPAGATION-DESIGN.md`
  - `doc/WHIRL-DSL-SHAPE-PROPAGATION-IMPLEMENTATION-PLAN.md`
  - `doc/WHIRL-DSL-SHAPE-RETYPING-CONTRACT.md`
  - `doc/WHIRL-DSL-SYMBOLIC-SHAPE-CONTRACT.md`
  - `doc/WHIRL-DSL-SHAPE-SP9-CERTIFICATION.md`
  - `doc/WHIRL-DSL-SHAPE-SP10-CERTIFICATION.md`
  - `doc/IPA-DSL-SHAPE-PROPAGATION-TODO.md` if cross-links require repair.
  - New `osprey/common/com/tests/dsl_shape_document_contract_test.sh`.
- Implementation:
  1. Publish one authority map headed by the master plan, with `AGENTS.md` as
     mandatory repository policy and `WHIRL.pdf` as the representation
     baseline; make every focused shape plan explicitly subordinate.
  2. Set design stage status to the repaired endpoint and separate current
     per-PU obligations from future IPA work.
  3. Replace the obsolete all-PU rollback row with per-PU atomicity, isolation,
     and no-final-artifact requirements.
  4. Replace stale IDs with full resolvable hashes:
     SP1 `c0293dacd16c0e50c0d1ee7f9ba2cd9599f20f70` in excluded base history,
     SP2 `62cc9e9eeaac02c0732ec7a684110e88a13476ed`, and
     SP3 `aa8c7a8f9af5c400a928ec305cda1d15e7e8437e`.
  5. Preserve the repaired PREOPT wording that names WOPT, LNO, and IPA where
     applicable.
  6. Record `/private/tmp` bundles only as unavailable historical claims, never
     as current evidence.
  7. Keep SP9/SP10 status at `repair certification pending` until WP10 passes.
  8. Map every `passed`, `complete`, and `certified` claim to an executable lane
     and retained artifact.
  9. Publish the M0-M2 support-only crosswalk and the common/CNN/FHE rule-owner
     matrix. Do not promote PR 137 shape evidence into FHE milestone completion.
- Compatibility and risks: documentation must not get ahead of code or CI.
  Rewriting historical commit verdicts is prohibited.
- Positive tests: every hash resolves; all documents agree on status, scope,
  effect API, and evidence; historical H-02 and H-03 remain repaired.
- Negative tests: no stale ID, all-PU rollback demand, sole `/private/tmp`
  evidence, WOPT-only PREOPT wording, source-grep-only pass claim, tab, or
  whitespace error.
- Command outline:

  ```sh
  git cat-file -e c0293dacd16c0e50c0d1ee7f9ba2cd9599f20f70^{commit}
  git cat-file -e 62cc9e9eeaac02c0732ec7a684110e88a13476ed^{commit}
  git cat-file -e aa8c7a8f9af5c400a928ec305cda1d15e7e8437e^{commit}
  git diff --check
  osprey/common/com/tests/dsl_shape_document_contract_test.sh
  ```

- Artifacts: link/hash report, claim-to-lane map, documentation contract log,
  and final document diff.
- Exit criteria: the authority chain is internally consistent, all claims are
  evidence-bounded, and documentation checks pass.
- Suggested commit subject:
  `docs(shape): reconcile authority and commit traceability`

### WP10: Rebuild and Publish SP7-SP10 Certification

- Findings: P1-02, P1-03, P1-04, and every remaining unverified acceptance
  obligation.
- Goal: run the full linked, compatibility, model, target, backend, and
  `whirl2c` matrix in a reproducible environment and publish immutable evidence.
- Dependencies: A0, B0 reference-lane validation, WP1 through WP9, D6, and D8.
  Full same-process native-suite completion also depends on the selected X1
  disposition.
- Exact files and targets:
  - `build-open64-docker.sh` and the repository-documented Ubuntu 20.04
    linux/amd64 build path.
  - `osprey/ir_tools/Makefile.gbase` contract-test targets.
  - Shape scripts under `osprey/common/com/tests/` and WOPT tests under
    `osprey/be/opt/tests/`.
  - `osprey/torch2whirl/Makefile.gbase`,
    `osprey/torch2whirl/scripts/run_torch_docker_test.sh`, and
    `osprey/torch2whirl/scripts/run_openpy_docker_test.sh`.
  - Built `be`, `be.so`, `inline`/`lw_inline`, `ir_b2a`, and `whirl2c` tools.
- Implementation:
  1. Create the 10 required checks in Section 7 and make branch protection
     require all of them.
  2. Capture the snapshot-provenance prerequisite, then build a clean repaired
     commit in the pinned environment in Section 8. Verify the already
     committed B0 source-list closure rather than reporting it as unimplemented.
  3. Run all focused contract tests and scripts with actual assertions.
  4. Run the reader, model, optimization-level, target, backend-symbol, FHE,
     non-DSL, checkpoint, and `whirl2c` matrices in Section 9.
  5. Capture transformation before/after `.B`, independently reopen both with
     `ir_b2a -st -src`, and retain the raw diff.
  6. Publish the immutable tree and manifest in Section 10.
  7. Preserve strict mapped-image structural readability. A failed reader
     direction cannot be waived by narrowing the claim without a separate
     approved versioned-IR migration.
  8. Run ResNet-20 as the master-plan primary full-model case. Keep Llama cases
     as supplemental PR-specific regressions and do not label the result as
     completion of M0-M8.
  9. Run linked validation for every supported touched copied-source product;
     retain Fortran/JFE as explicitly unverified until such a lane runs.
  10. Run the full same-process native suite only after external X1 is fixed,
      or retain a strict base/head differential result as an explicit limit.
      Do not use process isolation to manufacture a pass.
  11. Only after every mandatory lane passes, update SP9/SP10 from pending to
     complete in a separate evidence commit.
- Compatibility and risks: environment drift, unavailable old readers, and
  cross-target toolchain gaps must produce a failed or pending gate, not a
  broader claim. A source-only lane cannot substitute for linked runtime.
- Positive tests: all repaired shape cases, `-O0` through `-O3`, explicitly
  scoped `-ipa`, primary ResNet-20, supplemental Llama
  prefill/decode/multi-PU, FHE claim-specific fixtures, non-DSL, reader
  compatibility, x86_64 linked runtime, target syntax/layout, backend startup,
  symbol boundary, checkpointing, and valid `whirl2c`.
- Negative tests: stale shapes, invalid triggers/effects, pending/malformed
  shapes, foreign owners/qualifiers, later-PU failure, old-reader incompatibility
  where claimed, missing artifact, checksum mismatch, skipped lane, backend
  dependency leak, and partial output publication.
- Command outline:

  ```sh
  OPEN64_HOST_WORK_ROOT=/home/zikai/workspace/open64-workspace/pr137-build \
    ./build-open64-docker.sh \
    /home/zikai/workspace/open64-workspace/open64-pr137-fixes
  make -C "$OPEN64_REPAIR_BUILD/osprey/targdir/ir_tools" -j2 \
    dsl_builder_contract_test dsl_shape_refine_contract_test \
    dsl_opt_contract_test dsl_lower_contract_test \
    fhe_convert_contract_test dsl_wopt_bridge_test ir_b2a
  export OPEN64_DSL_CONTRACT_TEST="$OPEN64_REPAIR_BUILD/osprey/targdir/ir_tools/dsl_builder_contract_test"
  export OPEN64_DSL_SHAPE_REFINE_TEST="$OPEN64_REPAIR_BUILD/osprey/targdir/ir_tools/dsl_shape_refine_contract_test"
  export OPEN64_IR_B2A="$OPEN64_REPAIR_BUILD/osprey/targdir/ir_tools/ir_b2a"
  export OPEN64_BUILD_ROOT="$OPEN64_REPAIR_BUILD/osprey/targdir"
  export OPEN64_DSL_SHAPE_SP3_ARTIFACT_DIR="$OPEN64_REPAIR_ARTIFACTS/wp10/sp3"
  export OPEN64_DSL_SHAPE_SP10_ARTIFACT_DIR="$OPEN64_REPAIR_ARTIFACTS/wp10/sp10"
  export OPEN64_DSL_SHAPE_SP8_ARTIFACT_DIR="$OPEN64_REPAIR_ARTIFACTS/wp10/sp8"
  export OPEN64_DSL_SHAPE_SP9_ARTIFACT_DIR="$OPEN64_REPAIR_ARTIFACTS/wp10/sp9"
  export OPEN64_DSL_MULTI_PU_DIR="$OPEN64_REPAIR_ARTIFACTS/wp10/multi-pu"
  export OPEN64_DSL_WOPT_ARTIFACT_DIR="$OPEN64_REPAIR_ARTIFACTS/wp10/wopt"
  osprey/common/com/tests/dsl_native_syntax_test.sh
  osprey/common/com/tests/dsl_shape_solver_test.sh
  osprey/common/com/tests/dsl_shape_refine_test.sh
  osprey/common/com/tests/dsl_shape_symbolic_test.sh
  osprey/common/com/tests/dsl_shape_sp9_certification_test.sh
  osprey/common/com/tests/dsl_multi_pu_artifact_test.sh
  osprey/be/opt/tests/dsl_wopt_driver_test.sh
  ```

- Artifacts: the complete Section 10 tree, all logs and exit codes, binaries and
  symbol audits, source/model inputs, `.B`, `.T`, traces, diagnostics, assembly
  where applicable, generated C, diffs, manifest, and checksums.
- Exit criteria: all 10 required checks are green and non-skipped; every claim
  maps to an assertion, command, log, and checksum; reviewers can download the
  immutable bundle; B0 passes the required reference lane; reader compatibility
  is not narrowed; X1 is handled under the selected explicit policy; a fresh
  review finds no open PR 137 approval blocker. No exit criterion completes a
  master-plan milestone by implication.
- Suggested commit subjects:
  - `test(shape): add reproducible SP7-SP10 certification`
  - `docs(shape): record repaired certification evidence`

## 5. Existing Foundation and Six Focused Pull Requests

The six-PR split below is the recommended D7 delivery structure, not a
master-plan requirement. An approved aggregate delivery must preserve the same
dependency, evidence, and rollback boundaries.

| Unit | Work packages | Required internal commits | Merge prerequisite | Primary rollback unit |
| --- | --- | --- | --- | --- |
| B0 foundation | Post-review build closure | Existing `6eada12d...`: build plan plus seven copied source-list updates | Ubuntu 20/LLVM 11 reference lane before final approval; Ubuntu 24 remains supplemental | Revert `6eada12d...`; no semantic repair or DST product code |
| PR1: Canonical semantic guard | WP0, WP1 | Semantic freeze; shape-only authorization helper and tests | D1 confirmed; remaining decisions assigned to their owning later PR; D8 observed | Revert PR1; no mapped-layout change |
| PR2: Atomic transaction and boundary | WP2, WP3, WP4 | Boundary gate; complete preflight; journal certification and only proved repairs | PR1; full SP5 matrix green | Revert the whole PR; do not split journal from preflight |
| PR3: Driver-owned multi-PU isolation | WP5 | Unit fixture; real-backend driver fixture | PR2 | Revert PR3 without changing single-PU transaction code |
| PR4: Symbolic qualifier trust boundary | WP6 | Owner-aware parser/use sites; bridge adaptation only if needed | PR2; D2 confirmed | Revert PR4 and any dependent WP8 work; document artifact compatibility |
| PR5: Executable pass-effect contract | WP7 | Registration record/API; per-pass runtime behavior | PR2; D3 confirmed | Revert PR5 as one scheduling unit |
| PR6: Solver and certification closure | WP8, WP9, WP10 | Solver tests; docs pending; certification harness; final evidence docs | PR3-PR5 | Revert final evidence first, then harness/docs/solver in reverse order |

All six planned PRs are based on B0. If the repository instead keeps one
aggregate repair branch, the focused build plan's branch-diff condition is
evaluated on subrange `7cac0d86...6eada12d`; it cannot describe the later
semantic commits as part of the focused build diff.

Parallel execution is allowed as follows: PR1's WP1 and PR2's WP2 may be
developed concurrently on stacked branches after WP0; after PR2 stabilizes,
PR3, PR4, and PR5 may run independently; WP8 waits for PR4. PR6 is not merged
until PR3-PR5 are integrated into its certification head.

Global rollback order is PR6 -> PR5/PR4/PR3 -> PR2 -> PR1 -> B0. Reverting PR4 also
requires reverting WP8 in PR6. Reverting PR2 requires reverting PR3-PR5 and
the certification result. Never delete canonical `TY` entries during rollback;
restore references and generation state. Every compiler failure blocks final
artifact publication.

## 6. Findings-to-Tests Traceability

| Finding | State at reviewed head | Owning WP | Required executable evidence |
| --- | --- | --- | --- |
| P1-01 | Live, reproduced | WP1, WP3 | Custom KV changed/added/removed/state/value negatives; zero writes and rollback zero |
| P1-02 | Live unverified claim | WP10 | Linked SP7 optimization, re-entry, FHE, stale, non-DSL, checkpoint, and `whirl2c` lanes with retained evidence |
| P1-03 | Live unverified claim | WP8, WP10 | Rule, model, reader, target, backend, and `whirl2c` matrix with immutable manifest |
| P1-04 | Live unverified claim | WP7, WP10 | Four effects, invalid/unknown cases, generation/stale/revalidation, and full artifact lane |
| P2-01 | Live evidence gap | WP8 | Forward/reverse/zigzag value, node, and constraint permutations with identical normalized output |
| P2-02 | Live preflight violation; final state restored | WP3 | Colliding local `ST_IDX` foreign owner rejected with zero writes and rollback zero |
| P2-03 | Live evidence gap | WP3, WP4 | Complete preflight matrix plus every write/post-validator failure injection and repeated failure |
| P2-04 | Live | WP2 | Six success paths through one gate; stale actual/formal/hidden/return/interface/REGION negatives |
| P2-05 | Live evidence gap | WP5 | Real backend exact-once multi-PU invocation, collision isolation, later failure, and no publication |
| P2-06 | Live, reproduced | WP6 | Zero-owner, mismatched, unmapped, malformed, overflow, and forged-equality negatives |
| P2-07 | Live document conflict | WP0, WP9 | Document contract test for stage, per-PU/IPA scope, effects, and evidence wording |
| P2-08 | Live API gap | WP7 | Four effect classes and invalid/missing/mismatch/null/duplicate/invalid-stage cases |
| P3-01 | Live document conflict | WP0, WP5, WP9 | Per-PU atomicity and no-publication test; no earlier-PU in-memory rollback requirement |
| P3-02 | Live traceability defect | WP9 | `git cat-file -e` for the three full hashes and explicit SP1 excluded-base wording |
| H-01 malformed matmul | Historical, repaired | WP8 | Permanent rank-below-two, rank mismatch, bad contraction, and contradictory seed regressions |
| H-02 PREOPT wording | Historical, repaired | WP9 | Document check retains WOPT, LNO, and IPA where applicable |
| H-03 EOF whitespace | Historical, repaired | All WPs, WP9 | Per-commit `git diff-tree --check`, aggregate `git diff --check`, and no-tab check |
| BF-01 copied-source dependency closure | Post-review build finding; implementation present, reference validation pending | B0, WP10 | Ten-consumer inventory; `6eada12d...` subrange audit; Ubuntu 20/LLVM 11 linked build; explicit Fortran/JFE result |

Historical findings remain regressions and traceability obligations. They are
not relabeled as live endpoint defects.

The `State at reviewed head` column above remains frozen. At this design
baseline, all live or unverified semantic findings remain open. B0 changes
only build dependency closure and does not repair any semantic finding.
Historical findings remain historical and their frozen verdicts are not
recomputed from this plan.

## 7. CI Lane DAG and Required Checks

The exact check names and branch-protection layout below are the recommended D6
delivery mechanism, not master-plan text. If the project approves an equivalent
durable mechanism, it must preserve every dependency and evidence obligation in
this section and publish an explicit one-to-one crosswalk.

```text
snapshot-provenance prerequisite
  -> shape/static-policy
  -> shape/focused-syntax-layout
  -> shape/build-x86_64
       -> shape/linked-contracts
       -> shape/backend-symbol-boundary
       -> shape/reader-compat
       -> shape/model-matrix
            -> shape/optimization-pipeline
            -> shape/whirl2c
                 -> shape/artifact-integrity
```

The following 10 check names are required and must be branch-protection
contexts:

| Check | Required result |
| --- | --- |
| `shape/static-policy` | Exact clean source SHA and base/head relation from the prerequisite; per-commit and aggregate whitespace/no-tab checks, `bash -n`, document contract, and no source-grep-only pass |
| `shape/focused-syntax-layout` | Native syntax/operator-layout matrix for x8664, MIPS, MIPS-SL, KEY-generic, Loongson, and baseline with no skipped lane; no linked-runtime overclaim |
| `shape/build-x86_64` | Clean pinned Ubuntu 20.04 linux/amd64 build, native early `targ_info`, full build, and required tools/binaries |
| `shape/linked-contracts` | Direct execution of builder, lower, opt, refine, FHE, WOPT, solver, symbolic, rollback, multi-PU, and linked x86_64 contracts |
| `shape/backend-symbol-boundary` | Startup smoke, link closure, `DT_NEEDED`, defined/undefined symbol audit, and no builder/Json leak |
| `shape/reader-compat` | Four-cell frozen-base/current reader matrix with normalized comparison; no layout/readability narrowing without an approved versioned migration |
| `shape/model-matrix` | Primary ResNet-20; supplemental Llama/FHE-claim/non-DSL regressions; checkpoint, repeated runs, source/payload/`.B`/`.T` evidence, and multi-PU artifacts |
| `shape/optimization-pipeline` | `-O0` to `-O3`, supported IPA on/off, WOPT/FHE/VHO re-entry, stale rejection, unrelated options, and no publication on failure |
| `shape/whirl2c` | Actual valid conversion and C smoke, stale/pending rejection, post-lowering ordinary WHIRL, and no partial C/H output |
| `shape/artifact-integrity` | Complete manifest, unique lane results, source SHA match, full SHA256 verification, and no missing/skipped required lane |

Fail-closed policy:

1. Missing, skipped, cancelled, timed-out, or unreported required lanes fail.
2. A source grep, syntax compile, or generated label cannot satisfy a linked or
   runtime check.
3. The old head's expected-vulnerable repro is a negative control. It passes
   the provenance prerequisite only when the old defect reproduces and the
   opposite result; it never makes a repaired check green by itself.
4. An unsupported target runtime remains pending and cannot be reported as
   passed. A failed mapped-image reader/layout direction blocks the gate unless
   a separate versioned-IR migration is approved; its structural readability
   claim cannot simply be narrowed in this no-format-change repair.
5. A dirty source tree, mismatched source SHA, missing command, incomplete
   checksum set, unexplained binary dependency, or partial artifact fails C10.
6. Downstream results are not accepted when an upstream required dependency is
   red. SP9/SP10 remain pending until all mandatory checks are green.
7. All Bash certification scripts use `set -euo pipefail`. Required test
   commands do not use `continue-on-error`, `|| true`, or tool-missing skips.
   Cleanup may preserve failed logs, but cannot publish a failed output under a
   valid final artifact name.
8. A base-owned X1 failure is never converted into green by process isolation.
   The lane waits for the external fix or records the strict differential
   limitation and keeps native lifecycle/master-milestone readiness blocked.

## 8. Reproducible Build Baseline and Portability Diagnostic

The primary certification build is the focused B0 build plan's
repository-documented Ubuntu 20.04,
linux/amd64 path in `build-open64-docker.sh`. The container image digest,
installed package versions, configure arguments, and all build commands must
be pinned in the manifest. The script's ARM64 target-info pre-generation is a
documented macOS/Rosetta workaround; native linux/amd64 CI must still run an
early target-info gate appropriate to that build path.

B0 is not hypothetical: `6eada12d...` added `dsl_gatekeeper.cxx` and
`dsl_shape.cxx` once to seven copied common-source lists after auditing all ten
consumers. A later Ubuntu 24 build/install and smoke matrix passed with an
uncommitted generic compatibility overlay. This evidence is supplemental. The
Ubuntu 20/LLVM 11 reference lane is still required, and the touched Fortran and
JFE products remain linked-build unverified until a supported lane runs them.

Command outline for a direct clean linux/amd64 lane:

```sh
mkdir -p "$OPEN64_REPAIR_BUILD"
cd "$OPEN64_REPAIR_BUILD"
"$OPEN64_REPAIR_SRC/configure" \
  --build=x86_64-unknown-linux-gnu \
  --host=x86_64-unknown-linux-gnu \
  --target=x86_64-unknown-linux-gnu \
  --disable-jfe --disable-multilib --with-build-optimize=DEBUG
make -j1 targ_info
make -j2 build
make -j2 be.so be inline whirl2c ir_b2a
```

The exact valid make directory and target spelling selected by the configured
tree must be recorded in `commands/`; the binaries, including `lw_inline` when
produced by the inline build, must be listed by hash in the manifest.

The earlier Ubuntu 24/GCC 13 `targ_si_gen` observation remains a dated
diagnostic. The frozen review saw it segfault at `Makefile.gbase:785` after a
generated-code warning, before PR-focused binaries existed; later B0 validation
used a separate compatibility overlay and completed. Therefore:

1. Do not attribute that failure to PR 137 without a base-versus-head reproducer
   in the same environment.
2. Preserve Ubuntu 24 OS/compiler/linker versions, generator command, warning,
   core/backtrace if available, and base/head outcomes under
   `diagnostics/ubuntu24-gcc13-targ-si-gen/`.
3. Do not use the Ubuntu 24 failure to waive the Ubuntu 20.04 required build.
4. A portability fix, if needed, is a separate reviewed change; its evidence is
   supplementary to PR 137 certification.
5. Native linux/amd64 acceptance does not bypass the generators.
   `PREGENERATED_TARG_INFO` is limited to Rosetta, QEMU, or cross-build lanes
   and must match the same source SHA, target, and DEBUG/`Is_True_On`
   configuration, with producer architecture, container digest, and per-file
   checksums retained.

## 9. Compatibility and Runtime Matrix

| Matrix | Mandatory cases | Required evidence and qualification |
| --- | --- | --- |
| Focused contracts | Builder, lower, opt, refine, FHE convert, WOPT bridge, solver, symbolic, rollback, boundary, multi-PU | Linked executables, assertions, logs, state hashes, exit codes |
| Reader | Old reader/old `.B`, new reader/old `.B`, new reader/new `.B`, old reader/new `.B` | Old reader built from `590bdf3d...`; `ir_b2a -st -src`; normalized ops, TY/descriptors, PU count, section inventory; any structural readability/layout failure blocks this no-format-change repair or requires a separate versioned migration |
| Models | Primary ResNet-20; supplemental Llama prefill, decode, multi-PU, symbolic attention, FHE-claim/checkpoint fixtures; representative non-DSL C/C++ | Source, payload, `.B`, `.T`, phase traces, diagnostics, outputs, multiple-run stability; no M0-M8 completion claim |
| Optimization | ResNet `-O0`, `-O1`, `-O2`, `-O3`; Llama families at least `-O0`; explicit supported `-ipa` lane | Scope assertion, WOPT/FHE/VHO invalidation and revalidation, stale rejection, unrelated options ignored, failure no publication |
| Target | x8664, MIPS, MIPS-SL, KEY-generic, Loongson, and baseline syntax/layout; linked x86_64 runtime | Syntax/layout results are not linked target certification. Claim linked runtime only where the target toolchain actually ran |
| Backend | `be`, `be.so`, `inline`/`lw_inline`, `whirl2c.so`, `ir_b2a` | `file`, `ldd`, `readelf -h/-d/-Ws`, `nm -D -C`; frozen-base normalized dependency diff; zero `DSL_Builder_` and `Json::` leak |
| Startup/checkpoint | `be` no-input diagnostic, normal backend startup, FHE checkpoint write/read, no final image after failure | Logs, exit status, checkpoint hash, output publication report |
| `whirl2c` | Valid refined high-level DSL, stale/pending negative, post-lowering ordinary WHIRL | Actual generated `.c/.h`, log, hash, C syntax/link smoke where applicable; negative cases create no final C/H |
| Transform evidence | Every changed IR transformation | `case-id.before.B`, `case-id.after.B`, independently reopened `.T` files, raw diff, same input/options/target/scope/source mapping |
| Native lifecycle | Fresh-process A/B and same-process AB/BA; full `python_native_test` when X1 is available | Preserve the base/head 0/0/1/1 attribution; external develop fix or explicit differential limitation; never process-isolation-as-repair |
| Touched copied-source products | All ten consumers statically; every supported touched target linked | Ubuntu 20/LLVM 11 reference results; Fortran/JFE remain explicitly unverified until built |

Suggested frontend model commands are the existing `python_native_ir_tools_smoke`,
`resnet_native_ir_tools_smoke`, `llama2_prefill_native_ir_tools_smoke`,
`llama2_decode_native_ir_tools_smoke`, `llama2_multi_pu_native_ir_tools_smoke`,
and `fhe_resnet20_capture` targets, plus `run_openpy_docker_test.sh` for the
full `openpy -O0` path. The openpy orchestrator must be parameterized or
extended before claiming the broader optimization matrix.

The `whirl2c` command must execute, not merely appear in a log. A representative
outline is:

```sh
"$OPEN64_REPAIR_BUILD/osprey/targdir/whirl2c/whirl2c" \
  -TARG:abi=64 \
  -CLIST:src_file=resnet.py:dotc_file=resnet.w2c.c:doth_file=resnet.w2c.h \
  -fB,resnet.B resnet.py
```

## 10. Immutable Artifact Contract

Under the recommended D6 delivery mechanism, each required run publishes one
immutable root. An approved equivalent must retain the same source, authority,
claim, command, result, and payload provenance:

```text
artifacts/pr137-fixes/SOURCE_SHA/CI_RUN_ID/
  manifest.json
  MANIFEST.txt
  SHA256SUMS
  source/
    commit.txt
    base.txt
    status.txt
    diff.patch
    diff-stat.txt
  environment/
    os-release.txt
    compiler.txt
    linker.txt
    packages.txt
    container-image.txt
    container-inspect.json
    configure-argv.txt
  commands/
    snapshot-provenance.sh
    static-policy.sh
    focused-syntax-layout.sh
    build-x86_64.sh
    linked-contracts.sh
    backend-symbol-boundary.sh
    reader-compat.sh
    model-matrix.sh
    optimization-pipeline.sh
    whirl2c.sh
    artifact-integrity.sh
    environment.txt
  results/
    summary.json
    required-checks.json
    junit.xml
  lanes/
    snapshot-provenance/
    static-policy/
    focused-syntax-layout/
    build-x86_64/
    linked-contracts/
    backend-symbol-boundary/
    reader-compat/
    model-matrix/
    optimization-pipeline/
    whirl2c/
    artifact-integrity/
  diagnostics/
    ubuntu24-gcc13-targ-si-gen/
```

Every lane contains `stdout.log`, `stderr.log`, `exit-status.json`, its inputs,
and its retained outputs. Model and transformation subdirectories retain source,
payloads, `.B`, `.T`, phase traces, diagnostics, assembly, generated C/H, and
raw diffs as applicable.

`manifest.json` schema requirements:

- `schema_version`, `run_id`, `created_utc`, and immutable artifact URL.
- `source`: repository URL, base SHA, repaired SHA, branch, clean boolean,
  submodule state, source archive hash, frozen review head `7cac0d86...`, current
  repair head, and B0 commit `6eada12d...`.
- `authority`: master-plan identity and hash, applicable `AGENTS.md` identity
  and hash, `WHIRL.pdf` baseline identity and hash, and the focused subordinate
  plans used by the run.
- `claims`: explicit M0-M2 support-only mappings, an empty milestone-completion
  set, ResNet-20 primary-model status, and supplemental Llama claim scope.
- `environment`: OS, architecture, compiler, linker, libc, target, container
  name and immutable digest, package lock/list, configure arguments, and make
  parallelism.
- `commands`: ordered command ID, exact argv, working directory, environment
  delta, start/end time, exit status, and stdout/stderr paths.
- `lanes`: required check name, dependencies, status, assertion count, skipped
  count, and artifact paths.
- `files`: for every payload other than `manifest.json` and `SHA256SUMS`, the
  relative path, byte size, media type, SHA256, producing command, and semantic
  role. The detached `SHA256SUMS` entry authenticates `manifest.json` without a
  self-referential manifest hash.
- `readers`: old/new source SHA and binary SHA256.
- `targets` and `models`: exact identity, options, and result.
- `build_closure`: all ten copied-source consumers, the seven touched source
  lists, linked validation state, and explicit Fortran/JFE unverified state
  where those products did not run.
- `native_lifecycle`: X1 ownership, fresh-process A/B and same-process AB/BA
  outcomes, external-fix identity if present, and any strict differential
  limitation.

`SHA256SUMS` covers `manifest.json` and every payload file, but not itself. It
is generated only after lane publication is complete and is verified by
`shape/artifact-integrity`. Upload is atomic: a temporary
run name is finalized only after manifest and checksum verification. CI
retention and the immutable URL must allow reviewers to download the bundle;
a mutable local path is only a staging area.

The bundle proves only the named command- and assertion-backed claims. It
cannot override the authority order, turn supplemental Ubuntu 24 evidence into
the Ubuntu 20/LLVM 11 reference result, erase an unverified product, narrow a
reader/layout requirement, or imply completion of any master-plan milestone.

## 11. Gates 0 Through 6

| Gate | Completion rule | Current status |
| --- | --- | --- |
| Gate 0: Authority, baseline, and decisions | A0 authority map and M0-M2 support-only crosswalk accepted; worktree, frozen 17-commit range, current repair head, B0, and X1 recorded; old repros retained as negative controls; D1-D4 confirmed for their owning semantic work; D6-D7 have an approved or explicitly assigned disposition; D8 satisfied for representation claims | Not complete: A0, WP0, D1-D4, D6-D8, and representation claims remain open or pending |
| Gate 1: Trust boundaries and preflight | WP1-WP3 and WP6 complete; P1-01, P2-02, and P2-06 repros reversed; every preflight rejection has zero writes, rollback zero, and no global/table side effect | Not started |
| Gate 2: Atomic transaction and per-PU lifecycle | WP4-WP5 complete and WP2 fast paths remain green; the existing journal is first tested, any implementation change is backed by a proved defect, and every write/post-validator injection, repeated failure, recovery, and real multi-PU isolation pass | Not started |
| Gate 3: Solver and effects | WP7-WP8 complete; genuine insertion permutations, historical matmul regressions, and four registered effects green; invalid/unknown fail closed | Not started |
| Gate 4: Clean linked build | B0 subrange audit complete; `shape/static-policy` through `shape/backend-symbol-boundary` green in pinned Ubuntu 20.04/LLVM 11 linux/amd64; required binaries and every supported touched consumer start/link; Fortran/JFE are either run or remain explicitly unverified; no forbidden dependency | In progress: B0 is implemented and Ubuntu 24 evidence is supplemental; required reference lane is pending |
| Gate 5: Compatibility and models | Strict four-cell reader/layout matrix, primary ResNet-20, supplemental Llama claim-specific regressions, optimization, target qualification, non-DSL, FHE/checkpoint, and `whirl2c` checks green; X1 has an external fix or an explicit strict differential limitation; no runtime skip is relabeled as pass | Not started |
| Gate 6: Artifacts, documents, and final approval | WP9-WP10 complete; all 10 checks green; immutable evidence verifies; final SHA receives fresh PR status and plan-driven review; no live blocker remains; no FHE or M0-M8 completion is inferred | Not started |

Final approval is permitted only at Gate 6. Clean Git merge mechanics alone do
not satisfy any gate.

## 12. Execution Checklist and Status

### Decision Checklist

- [ ] D1 type equivalence and shape-only delta authorization confirmed as
      separate rules; every non-`logical_shape` field is unchanged.
- [ ] D2 mapped-form parsing and proof-use admission confirmed as separate
      rules with host-independent identity.
- [ ] D3 the complete execution/effect contract is confirmed; any per-pass
      storage choice is recorded as a local implementation choice.
- [ ] D4 composed active-PU boundary gate confirmed.
- [x] D5 per-PU rollback and no-publication semantics fixed by governing
      policy.
- [ ] D6 immutable CI artifact store confirmed.
- [ ] D7 six-PR delivery or combined-PR waiver confirmed.
- [ ] D8 normative WHIRL compatibility authority available.

### Prerequisite Checklist

- [ ] A0 authority reconciliation and M0-M2 support-only crosswalk accepted
      for the no-representation semantic slice; D8 still blocks representation
      claims.
- [x] B0 copied-source closure implemented at `6eada12d...`.
- [ ] B0 required Ubuntu 20/LLVM 11 reference lane complete; Fortran/JFE
      status explicit.
- [ ] X1 resolved externally or retained as a strict base/head differential
      limitation with affected readiness blocked.

### Implementation Checklist

- [ ] WP0 semantic contract frozen for the no-representation semantic slice.
- [ ] WP1 complete shape-only authorization implemented and verified.
- [ ] WP2 all success paths use the active-PU boundary gate and are verified.
- [ ] WP3 owner-safe zero-write preflight implemented and verified.
- [ ] WP4 existing journal certified and any proved defect repaired.
- [ ] WP5 real-backend multi-PU isolation focused gate complete.
- [ ] WP6 mapped-form parsing and proof-use admission enforced.
- [ ] WP7 complete pass execution/effect contract enforced.
- [ ] WP8 existing solver/rule behavior tested first; rule-owner matrix and
      all owned rule cases executable without duplicating FHE propagation.
- [ ] WP9 authority and claims reconciled.
- [ ] WP10 full certification and immutable publication complete.

### Design Baseline Status Table

| Item | Status | Evidence |
| --- | --- | --- |
| Frozen PR review | Complete at `7cac0d86...` | Original 17-commit verdict remains 5 Pass, 11 Partial, 1 Fail; it is not recomputed from the repair head |
| A0 | Open | Authority reconciliation and the M0-M2 support-only crosswalk must precede semantic implementation |
| B0 | Implemented; reference validation pending | `6eada12d...` updates seven source lists after a ten-consumer audit; Ubuntu 24 is supplemental; Ubuntu 20/LLVM 11 remains required |
| X1 | Base-owned external issue | Fresh-process A/B is 0/0 and same-process AB/BA is 1/1; no PR 137 product fix is planned; external follow-up or strict differential policy remains open |
| D1-D4 | Open | Each decision blocks its owning semantic work package |
| D6-D7 | Decision pending | Immutable delivery and final PR boundary remain open |
| D5 | Policy fixed | Per-PU independence and no final publication after pipeline failure follow the governing plans |
| D8 | Blocking prerequisite | Representation and compatibility claims wait for `WHIRL.pdf` review and reconciliation |
| WP0-WP10 | Open or planned | No semantic work package is complete at this design baseline |
| 10 required CI checks | Not started | Ubuntu 24 supplemental results are not required-check completion |
| Gate 0 | Not complete | A0, WP0, D1-D4, D6-D8, and representation claims remain open or pending |
| Gate 4 | In progress, reference lane blocked | B0 exists, but Ubuntu 20/LLVM 11 and supported touched-product linked results remain pending; Fortran/JFE are unverified |
| Gate 1 | Not started | WP1-WP3 and WP6 remain open |
| Gate 2 | Not started | WP2, WP4, and WP5 remain open |
| Gate 3, 5-6 | Not started | The approval recommendation remains unchanged |

This document is planning material. It records the existing B0 implementation
and the intended semantic repair sequence before WP0 begins. It creates no
semantic implementation, pull request, remote update, required reference-lane
result, broad certification artifact, milestone completion, or approval. The
aggregate recommendation remains: do not approve yet.
