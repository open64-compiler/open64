# WHIRL DSL Tensor Shape Propagation Implementation Plan

## Status

Execution plan for `WHIRL-DSL-SHAPE-PROPAGATION-DESIGN.md`. The architecture
document is authoritative for semantics; this document orders the coding work,
review gates, validation, and pull-request boundaries.

No implementation milestone may weaken binary WHIRL compatibility, mutate a
sealed tensor type in place, duplicate shape formulas in independent services,
or expose a partially retyped WHIRL program.

Progress through SP10 implementation:

- SP0 baseline inventory was consumed by the SP1 through SP3 implementation
  reviews.
- SP1 completed in commit `08c5eb48`.
- SP2 completed in commit `eb8b7273`.
- SP3 completed in commit `0ef23ab9`.
- SP4 is complete in `WHIRL-DSL-SHAPE-RETYPING-CONTRACT.md`.
- SP5 is complete on `codex/dsl-shape-sp5`.
- SP6 confirms the existing driver-owned per-PU lifecycle on
  `codex/dsl-shape-sp6-pu-driver`.
- SP7 integrates per-PU invalidation and revalidation with DSL WOPT, FHE
  conversion, the fixed VHO DSL optimization pipeline, checkpoint-only output,
  and final DSL lowering on `codex/dsl-shape-sp7-pipeline`.
- SP8 implements PU-scoped named dimensions, anonymous runtime dimensions,
  the `symbol+constant` expression slice, symbolic decode-attention checking,
  strict pending rejection, and mapped-image inspection on
  `codex/dsl-shape-sp8-symbolic`.
- SP9 adds reviewed NumPy broadcasting and symbolic batched-matmul inference,
  and completes the common, model, FHE, non-DSL, reader-compatibility, and
  target-matrix lanes on `codex/dsl-shape-sp9-certification`. Normal `-O0`
  backend and DSL-aware `whirl2c` certification are also complete after
  correcting the pre-existing backend static-initialization defects recorded
  in `WHIRL-DSL-SHAPE-SP9-CERTIFICATION.md`.
- SP10 publishes and enforces the Shape Inference Trigger Contract with
  structured runtime trigger identities, richer transformation shape effects,
  a current-call-site audit, and focused stale-generation tests. Certification
  is recorded in `WHIRL-DSL-SHAPE-SP10-CERTIFICATION.md`.

## Objective

Deliver compiler-owned tensor shape propagation that:

1. consumes frontend-provided TensorDescriptorIR seed facts;
2. applies versioned logical DSL operator shape functions;
3. reaches a deterministic fixed point within each PU while checking calls,
   returns, and REGION interfaces as explicit boundary contracts;
4. interns immutable and uniqued tensor `TY_IDX` records;
5. atomically rebinds every affected WHIRL projection;
6. runs before DSL optimization, FHE conversion, and DSL lowering;
7. leaves reviewable binary and `ir_b2a -st -src` evidence.

## Non-Goals For The First Delivery

- Do not add a new `TY_KIND`, opcode encoding, ELF section, or mapped-image
  record merely to record that shape inference ran.
- Do not implement graph-wide shape inference in torch2whirl or Python.
- Do not make the first solver a general symbolic theorem prover.
- Do not specialize shared PUs until the cross-context type-conflict policy is
  separately reviewed.
- Do not fold layout, placement, sharding, quantization, runtime state, or FHE
  state into a shape-only refinement without a separate proof.
- Do not replace the optional descriptor-propagation optimization with this
  mandatory legality phase. They have different responsibilities.

## Original Baseline

The implementation started from these observed conditions. SP1 through SP3
have changed the items that describe missing services, but the list remains as
the historical baseline against which those milestones were reviewed:

1. `DSL_Builder_Intern_Tensor_Type()` creates and seals a candidate tensor type
   before scanning `Ty_tab` for an equivalent type. Returning an earlier
   `TY_IDX` can therefore leave an unused duplicate candidate in the table.
2. Tensor equivalence and hashing already reach `KIND_TENSOR` through
   `TY_are_equivalent()` and `TY_tensor_hash()`, but the hash currently covers
   only a small subset of the canonical descriptor.
3. Exact shape parsing and operator-specific checks are embedded in
   `dsl_gatekeeper.cxx` for matmul, flatten, linear, convolution, BatchNorm,
   pooling, and exact-shape operators.
4. Logical shape is currently projected primarily through a string attribute.
   Most checks accept static positive integer dimensions only.
5. The managed DSL image, call ABI, PU-interface image, REGION service, and
   `dsl_ir_rewrite.cxx` provide the relationships and preflight patterns needed
   for later atomic retyping.
6. There is no `dsl_shape` common service, no
   `VHO_DSL_Shape_Refine_Driver()`, and no shape-refinement option today.

Every baseline claim must be reconfirmed against merged `develop` before its
milestone begins. The implementation branch must not be based on the older
dirty development worktree.

## Ownership And Files

### Common semantic engine

New files:

```text
osprey/common/com/dsl_shape.h
osprey/common/com/dsl_shape.cxx
```

Responsibilities:

- normalized static-shape parsing and formatting;
- dimension and rank facts;
- merge and contradiction operations;
- canonical expression services introduced by later milestones;
- versioned operator shape-function registry;
- constraint graph and worklist solver;
- check-only comparison against existing TensorDescriptorIR;
- diagnostics independent of VHO, Python, FHE, WOPT, and code generation.

### Tensor type uniquing

Primary files:

```text
osprey/common/com/symtab.h
osprey/common/com/symtab.cxx
osprey/common/com/dsl_builder.h
osprey/common/com/dsl_builder.cxx
```

The backend-safe canonical interner belongs in `symtab`, not in a frontend
builder API. `DSL_Builder_Intern_Tensor_Type()` becomes a frontend wrapper over
the same common service. The service must look up the complete canonical key
before allocating a `TY` record.

### Managed IR retyping

Primary implementation:

```text
osprey/common/com/dsl_ir_rewrite.cxx
osprey/common/com/dsl_ir_image.h
osprey/common/com/dsl_region.cxx
osprey/common/com/dsl_region.h
```

This work owns expected-old-type preflight, active-PU ownership, WN/ST/value
agreement, call ABI updates, PU formal updates, return relationships, REGION
interfaces, commit ordering, and rejection without mutation. Table-only
mutation helpers must remain private.

### VHO orchestration

New files:

```text
osprey/be/vho/dsl_shape_refine.h
osprey/be/vho/dsl_shape_refine.cxx
```

Existing integration files:

```text
osprey/be/be/driver.cxx
osprey/common/com/config_dsl.h
osprey/common/com/config_dsl.cxx
osprey/be/be/Makefile.gbase
osprey/ir_tools/Makefile.gbase
```

The VHO layer owns program traversal, active local-symbol-table coordination,
begin/PU/end lifetime, mutation-plan application, tracing, and final strict
verification. It does not own operator shape formulas.

## Execution Rules

1. Each milestone begins from merged `develop` and ends with a focused PR.
2. Extract existing behavior before adding new inference behavior.
3. Add check-only analysis before adding any mutation.
4. Preflight the complete request set before changing any WHIRL structure.
5. A failed phase leaves no partially retyped in-memory or binary artifact.
6. New common services must remain link-safe for `be.so`, `lw_inline`, and IR
   tools and must add no external library dependency.
7. New C/C++ files follow existing Open64 spacing and contain no tab
   characters. Make recipes may retain required tabs.
8. Every artifact validation uses `ir_b2a -st -src` and retains the `.B`, `.T`,
   source, phase trace, and diagnostics in a host-visible directory.

## Milestone Queue

### SP0: Baseline And Contract Inventory

Status: completed as the baseline for the SP1 through SP3 reviews.

Actions:

1. Rebase a clean implementation branch onto merged `develop`.
2. Inventory every shape parser, compatibility helper, and operator formula in
   `dsl_gatekeeper.cxx`, `dsl_builder.cxx`, and `dsl_lower.cxx`.
3. Produce a table keyed by logical operator and semantic version containing
   operand requirements, attributes, result formula, representation checks,
   and current diagnostics.
4. Inventory every physical and managed location that carries a tensor
   `TY_IDX`: WN, ST, DSL value, formal, call actual, return, constant, function
   type, `TYLIST`, and REGION interface.
5. Capture baseline test results and retained `.B`/`.T` artifacts for common
   add, matmul, ResNet, Llama prefill, and Llama decode.
6. Record current `Ty_tab` growth when the builder interns the same descriptor
   repeatedly. This becomes the uniquing regression baseline.

Deliverable:

- checked-in inventory in the execution plan or a focused companion document;
- no behavior change;
- baseline artifact manifest with absolute local paths.

Exit gate SP0:

- reviewers agree that the operator table covers every existing shape check;
- no shape formula is scheduled for deletion without a replacement test.

### SP1: Shared Check-Only Shape Service

Dependencies: SP0.

Status: completed in commit `08c5eb48`.

Actions:

1. Add `dsl_shape.h/.cxx` with normalized static dimension vectors, overflow
   checks, equality, exact-shape, identity, contraction, flatten, convolution,
   pooling, and reduction helpers.
2. Register shape functions by `(logical operator, semantic version)`.
3. Move existing gatekeeper formulas into the shared service without changing
   accepted or rejected programs.
4. Make the gatekeeper call the shared service in strict check-only mode.
5. Keep compatibility wrappers temporarily where a large mechanical move would
   obscure review; mark and remove them before SP3.

Initial operator set:

- identity and same-shape operators;
- `common.add`, `common.mul`, and `common.residual_add`;
- `common.matmul`;
- `common.linear` versions currently present in the registry;
- reshape, transpose, and flatten forms currently ingested;
- `cnn.conv2d`, inference BatchNorm, max pool, and global average pool.

Tests:

- old gatekeeper positive and negative cases remain byte-for-byte equivalent
  where diagnostics are part of the contract;
- unknown operator version rejects rather than borrowing another version;
- arithmetic overflow and malformed static-shape input reject;
- no Python, builder-state, FHE, WOPT, or CG dependency enters `dsl_shape`.

Exit gate SP1:

- one implementation supplies both gatekeeper checking and future inference;
- current binary artifacts reopen and validate without change.

PR boundary: shape-service extraction only.

### SP2: Immutable Canonical Tensor Type Interner

Dependencies: SP0. May proceed in parallel with SP1 after the canonical key is
reviewed.

Status: completed in commit `eb8b7273`.

Actions:

1. Define the normalized canonical key from the existing tensor type-equivalence
   contract.
2. Add a backend-safe common API for interning a complete descriptor or a
   shape-refined copy of an existing canonical tensor type.
3. Build a runtime canonical index from existing mapped tensor types.
4. Use hash lookup followed by full structural equality.
5. Select the lowest valid existing `TY_IDX` for legacy duplicate types without
   deleting or renumbering records.
6. Change `DSL_Builder_Intern_Tensor_Type()` to call the common interner.
7. Prove lookup-before-create behavior. No unused candidate may remain when an
   equivalent type already exists.

Tests:

- repeated equivalent requests return one `TY_IDX` with no `Ty_tab` growth;
- different shapes return different `TY_IDX` values;
- names, source data, lineage, and runtime/FHE state do not split canonical
  types;
- fields declared semantically significant do split types;
- mapped input rebuilds the index deterministically;
- legacy duplicate records remain readable and unchanged;
- all existing builder tensor tests pass.

Exit gate SP2:

- sealed tensor types cannot be mutated;
- builder and backend requests use one canonical service;
- no binary layout or reader/writer change.

PR boundary: canonical tensor interning and focused tests.

### SP3: Per-PU Static Solver In Check-Only Mode

Dependencies: SP1 and SP2.

Status: completed in commit `0ef23ab9`.

Actions:

1. Add rank and static-dimension facts plus `pending`, `complete`, and
   `contradiction` states.
2. Build one shape variable per tensor value in a PU.
3. Add constraints from seed descriptors, defining DSL nodes, result symbols,
   and local REGION interfaces.
4. Solve with a deterministic dependency worklist.
5. Compare inferred results with existing descriptors without modifying WHIRL.
6. Report unchanged, refinable, pending, unresolved, and contradictory values.
7. Define provisional stable diagnostics and counters for review.

Tests:

- insertion-order-independent fixed point;
- exact conflict diagnostics with source positions;
- add/mul, matmul, linear, reshape/transpose/flatten, and CNN result inference;
- non-DSL WHIRL is a no-op;
- no type-table or managed-image mutation.

Exit gate SP3:

- real ResNet and Llama artifacts can be analyzed in check-only mode;
- inferred complete descriptors agree with currently certified descriptors.

PR boundary: static solver and check-only evidence.

### SP4: Atomic Retyping Contract Review

Dependencies: SP3. This is a mandatory design gate before mutation code.

Status: completed by `WHIRL-DSL-SHAPE-RETYPING-CONTRACT.md`. The approved v1
slice is uniquely owned local native operator results; formal, return, call,
constant, function-type, and cross-PU changes remain check-only.

Actions:

1. Specify the exact request row: owner PU, value ID, expected old `TY_IDX`, and
   refined `TY_IDX`.
2. Define exhaustive preflight coverage for WN, ST, DSL values, node results,
   tensor constants, call ABI, PU formals, function types, `TYLIST`, returns,
   and REGION interfaces.
3. Define which references are authoritative and which are derived projections.
4. Define rollback and failure behavior for one active PU.
5. Define shared-symbol and shared-callee conflict policy.
6. Decide whether v1 supports only uniquely owned local results and fails closed
   for formal/function-type changes.
7. Review mapped-image and previous-reader consequences before implementation.

Recommended v1 restriction:

- mutate uniquely defined, non-address-taken local tensor result symbols first;
- require all uses and managed rows to agree with the expected old type;
- leave formal, return, and cross-PU refinement in check-only mode until SP6.

Exit gate SP4:

- common/com and VHO owners approve one preflight/commit protocol;
- no public table-only mutation helper is introduced;
- rejection cases and rollback tests are enumerated before coding.

PR boundary: documentation and test scaffolding only, if needed.

### SP5: Per-PU Canonical Refinement And VHO Driver

Dependencies: SP2, SP3, and approved SP4.

Status: completed on `codex/dsl-shape-sp5`.

Actions:

1. Implement the reviewed atomic retyping API in `dsl_ir_rewrite.cxx`.
2. Add `dsl_shape_refine.h/.cxx` with per-PU collection, solve, mutation-plan,
   preflight, commit, and strict post-verification.
3. Add `VHO_DSL_Shape_Refine_Driver()` in check-only and refine modes.
4. Add result counters and trace output for old/new type, value, PU, operator,
   constraint source, and iteration.
5. Add `-DSL:shape_refine=on|off` and
   `-DSL:dump_after_shape_refine=on|off` through `config_dsl`.
6. Keep shape refinement enabled by default at every optimization level.
7. When disabled, perform strict check-only validation rather than admitting an
   incomplete artifact.

Tests:

- one refined value receives a new or reused canonical `TY_IDX`;
- unrelated users retain the original `TY_IDX`;
- second identical refinement reuses the first refined type;
- failure in the last request leaves all earlier requests unchanged;
- WN, ST, DSL value, and REGION evidence agree after commit;
- disabled mode changes no IR and rejects required unresolved shapes.

Exit gate SP5:

- a single-PU binary artifact is refined, written through the standard WHIRL
  path, reopened separately, and inspected with `ir_b2a -st -src`;
- no partial validly named artifact survives a failed run.

PR boundary: per-PU atomic refinement and driver, without cross-PU mutation.

Completion evidence:

- `DSL_IR_Refine_Native_Value_Types()` implements complete-array preflight,
  fixed-width WN/ST/value commit, strict post-verification, and reverse-order
  rollback for eligible local pure results.
- `VHO_DSL_Shape_Refine_Driver()` runs before optional DSL WOPT, FHE
  conversion, and DSL lowering. Non-DSL PUs are unchanged.
- `-DSL:shape_refine=on|off` defaults on; off performs strict check-only
  validation. `-DSL:dump_after_shape_refine=on|off` retains the post-phase
  tree when requested.
- The focused test proves admission-versus-strict behavior, disabled-mode
  non-mutation, a forced late-failure rollback, immutable canonical type
  creation/reuse, direct WN/ST/value agreement, and REGION preservation.
- The retained `shape_refine.B` reopens in a separate `ir_b2a -st -src`
  process. Its trace preserves old pending TY 53, contains refined canonical
  TY 55 with shape `[2,3]`, and binds both refined result values to TY 55.
- No mapped-image row or ELF section changed. `be.so`, `be`, and `lw_inline`
  rebuild; `be.so` and `lw_inline` contain no `DSL_Builder_*` or `Json::`
  symbols.

Local review artifacts:

```text
/private/tmp/open64-shape-sp5/artifacts/shape/sp5-refinement/shape_refine.B
/private/tmp/open64-shape-sp5/artifacts/shape/sp5-refinement/shape_refine.T
/private/tmp/open64-shape-sp5/artifacts/shape/sp5-refinement/validation.log
/private/tmp/open64-shape-sp5/artifacts/shape/sp5-refinement/certification.txt
```

### SP6: Driver-Owned Per-PU Completion And Interface Verification

Dependencies: SP5.

Status: complete. The reviewed correction is that the backend driver, not the
shape pass, owns complete program traversal. `Preorder_Process_PUs()` selects
each PU and `Preprocess_PU()` invokes `VHO_DSL_Shape_Refine_Driver()` before
DSL WOPT, FHE conversion, DSL lowering, and ordinary VHO lowering.

Actions:

1. Keep shape analysis, immutable type interning, mutation, and rollback local
   to the active PU.
2. Let normal backend traversal provide complete PU coverage; do not add a
   second begin/PU/end traversal or reactivate another PU's local symbol table.
3. Validate call actual/formal, hidden result, return, PU-interface, and REGION
   rows as boundary contracts when the owning PU is active.
   REGION initialization, traversal, and finalization remain driver-owned;
   shape refinement only verifies rows whose type derives from the locally
   retyped ST.
4. Require frontend or earlier compiler phases to provide sufficient boundary
   seed descriptors. Fail closed when a PU cannot establish the descriptor
   state required by its next phase.
5. Leave multi-PU cloning, inlining, or coordinated signature mutation to the
   IPA transformation that creates that need. Require `-ipa`, IPA call-graph
   scope, and a separate contract; never perform it automatically from the VHO
   shape driver.

Tests:

- driver-source phase-order certification;
- one active-PU fixed point and atomic rollback;
- REGION interface preservation within the active PU;
- strict rejection of unresolved boundary descriptors;
- a multiple-PU fixture proving independent per-PU invocation and no
  caller/callee mutation, rather than testing interprocedural propagation;
- non-DSL and legacy WHIRL no-op behavior.

Exit gate SP6:

- every backend-selected PU encounters shape refinement at the beginning of
  VHO processing;
- no downstream DSL phase observes an unverified active PU;
- the shape service contains no competing PU traversal or all-PU journal.

Compilation-scope rule: complete backend coverage is a sequence of independent
per-PU invocations. It is not interprocedural analysis. Globally loaded tables
remain identity and boundary evidence only. Any future caller/callee shape
propagation must be introduced as an IPA-owned pass and execute only under
`-ipa`.

Test ownership follows the same rule. SP6 and SP7 test PU-local inference,
atomicity, boundary rejection, driver coverage, and preservation of other PUs.
They do not claim or test cross-PU propagation. Cross-PU shape tests belong to
the future IPA milestone that implements such behavior.

Retained SP6 review evidence:

```text
/private/tmp/open64-shape-sp5/artifacts/shape/sp6-pu-driver/shape_refine.B
/private/tmp/open64-shape-sp5/artifacts/shape/sp6-pu-driver/shape_refine.T
/private/tmp/open64-shape-sp5/artifacts/shape/sp6-pu-driver/validation.log
/private/tmp/open64-shape-sp5/artifacts/shape/sp6-pu-driver/certification.txt
```

PR boundary: driver-owned per-PU lifecycle clarification and certification.

### SP7: Backend Pipeline Integration And Invalidation

Dependencies: SP6.

Actions:

1. Add a defensive current-generation check in `VHO_DSL_Lower_Driver()`.
2. Conservatively rerun refinement after every executed DSL transformation that
   changes operators, operands, attributes, calls, returns, or REGIONs.
3. Define preservation/invalidation properties in the fixed DSL optimization
   pipeline after the conservative implementation is certified.
4. Ensure checkpoint-only and `whirl2c` paths follow the same legality rule.

Tests:

- `-O0` performs mandatory refinement;
- every optimization level sees the same legal descriptor state;
- canonicalization and algebraic simplification trigger revalidation;
- FHE conversion receives refined descriptors;
- stale-generation lowering rejects;
- unrelated driver options remain silently ignored by this phase.

Exit gate SP7:

- the normal backend pipeline cannot consume stale or contradictory tensor
  shapes;
- non-DSL and legacy WHIRL behavior remains unchanged.

Implemented SP7 contract:

1. Shape currency is a runtime-only per-PU generation state. It does not add a
   WHIRL table, ELF section, or persisted generation number.
2. Successful refinement, including strict check-only mode and a non-DSL PU,
   marks the active PU/tree generation current.
3. DSL WOPT invalidates before transformation and reruns refinement afterward.
   A successful FHE conversion pass invalidates and reruns refinement before
   checkpoint publication or ordinary lowering.
4. Every current fixed-pipeline VHO DSL stage is conservatively declared shape
   invalidating. The lowering driver invalidates before those stages, reruns
   refinement after execution, and requires current state immediately before
   native DSL lowering.
5. `whirl2c`-only processing receives the mandatory initial per-PU strict
   refinement/check. It does not run the skipped DSL transformations.
6. The current/stale check is defensive compiler state, not frontend metadata.
   It is scoped to the driver-selected PU and never traverses or mutates another
   PU.
7. The normative trigger taxonomy is recorded in the Shape Inference Trigger
   Contract in `WHIRL-DSL-SHAPE-PROPAGATION-DESIGN.md`. Seed creation,
   constraint mutation, value/boundary mutation, structural transformation,
   symbolic resolution, and shape-consuming phase boundaries are all covered.
   A new pass is shape invalidating until its registration proves preservation.
8. Inlining, cloning, specialization, outlining, and coordinated signature
   changes must use the same trigger contract when their owning driver is
   implemented. Their mention in the contract does not grant the current
   per-PU VHO driver cross-PU scope; interprocedural scheduling remains future
   `-ipa` work.

Retained SP7 review evidence:

```text
/private/tmp/open64-shape-sp5/artifacts/shape/sp7-pipeline/shape_refine.B
/private/tmp/open64-shape-sp5/artifacts/shape/sp7-pipeline/shape_refine.T
/private/tmp/open64-shape-sp5/artifacts/shape/sp7-pipeline/validation.log
/private/tmp/open64-shape-sp5/artifacts/shape/sp7-pipeline/certification.txt
```

PR boundary: driver, options, and invalidation integration.

### SP8: Symbolic And Runtime-Dynamic Dimensions

Dependencies: SP7 and a separate review of the open symbolic-design topics.

Status: complete for the reviewed v1 slice. The normative details are in
`WHIRL-DSL-SYMBOLIC-SHAPE-CONTRACT.md`.

Actions:

1. Implement the reviewed distinction among `<pending>`, anonymous dynamic
   `?`, named symbols, and canonical expressions.
2. Add the minimum expression subset required by Llama decode and published
   operator contracts.
3. Add shape assertions and runtime guards where static proof is unavailable.
4. Define printing, lowering, and mapped-image requirements before adding any
   new persisted table.
5. Add backward inference only where operator contracts require it.

SP8 resolves item 3 conservatively: no guard is fabricated. Unproved required
relationships fail closed until the reserved assertion/guard descriptors gain
a separately reviewed executable and lowering contract. No current published
operator requires backward inference.

Retained SP8 review evidence:

```text
/private/tmp/open64-shape-sp5/artifacts/shape/sp8-symbolic/shape_symbolic.B
/private/tmp/open64-shape-sp5/artifacts/shape/sp8-symbolic/shape_symbolic.T
/private/tmp/open64-shape-sp5/artifacts/shape/sp8-symbolic/contract.log
/private/tmp/open64-shape-sp5/artifacts/shape/sp8-symbolic/producer.log
/private/tmp/open64-shape-sp5/artifacts/shape/sp8-symbolic/certification.txt
```

Exit gate SP8:

- Llama decode sequence dimensions remain valid without substituting sample
  input sizes;
- `ir_b2a -st -src` distinguishes pending, dynamic, symbolic, and static facts;
- unresolved required facts fail before lowering.

PR boundary: symbolic model and its compatibility contract.

### SP9: Final Certification

Dependencies: SP7 for static certification; SP8 for dynamic certification.

Status: complete. See `WHIRL-DSL-SHAPE-SP9-CERTIFICATION.md` for the full
matrix, backend startup correction, and retained evidence.

Required matrix:

- common add/mul broadcasting;
- common matmul and linear;
- reshape, transpose, and flatten;
- CNN convolution, BatchNorm, pooling, residual add, and logits;
- ResNet multi-PU artifact;
- Llama prefill and decode artifacts;
- FHE conversion input without FHE-specific shape inference;
- non-DSL C/C++ binary WHIRL;
- prior binary WHIRL images and previous-reader behavior;
- x86-64 plus the established syntax/operator-layout target matrix.

Required retained evidence per model:

```text
<model>.B
<model>.T
<model>.shape-before.t
<model>.shape-after.t
<model>.shape-diagnostics.txt
commands.txt
SHA256SUMS
```

The `.T` file is produced with `ir_b2a -st -src`. Docker lanes use an explicit
host bind mount and clean the artifact family only at the start of the next run.

### SP10: Shape Inference Trigger Contract And Enforcement Audit

Dependencies: SP9.

Status: complete. The normative trigger taxonomy is published in
`WHIRL-DSL-SHAPE-PROPAGATION-DESIGN.md`; retained evidence and the enforcement
audit are recorded in `WHIRL-DSL-SHAPE-SP10-CERTIFICATION.md`.

Actions:

1. Audit every current shape-refinement and invalidation site against the
   trigger contract: initial per-PU admission, DSL WOPT/Preopt, FHE conversion,
   fixed-order VHO DSL optimization, and the pre-lowering currency gate.
2. Replace free-form invalidation reasons with a runtime-only structured trigger
   identity and stable diagnostic name. Do not add persisted metadata, a WHIRL
   section, or a binary compatibility dependency.
3. Require every newly registered transformation to declare shape preserving,
   monotonic refining, locally invalidating, or boundary invalidating. Unknown
   classifications remain invalidating by default.
4. Test that seed construction remains a frontend/admission responsibility and
   that authoritative graph-wide retyping occurs only in the compiler-owned
   per-PU pass.
5. Test current-generation rejection at every shape-consuming boundary covered
   by the current pipeline. Leave future inlining, cloning, specialization,
   outlining, and cross-PU IPA scheduling to their owning implementations, but
   require them to adopt this contract when introduced.

Exit gate SP10:

- every current trigger has an owner, structured identity, and focused test;
- an unclassified transformation cannot silently preserve stale shape state;
- no frontend, binary WHIRL, or cross-PU scope expansion is introduced; and
- the SP9 certification matrix remains unchanged and passing.

PR boundary: trigger contract, runtime-only trigger identities, call-site audit,
and focused stale-state tests.

## Pull-Request Sequence

| PR | Scope | Depends on |
| --- | --- | --- |
| P0 | Architecture design | none |
| P1 | Baseline inventory and shared check-only shape extraction | P0 |
| P2 | Lookup-before-create canonical tensor interner | P0; may run with P1 |
| P3 | Per-PU static solver in check-only mode | P1, P2 |
| P4 | Atomic retyping contract and focused substrate | P3, SP4 review |
| P5 | Per-PU VHO refinement driver and options | P4 |
| P6 | Driver-owned per-PU and REGION lifecycle certification | P5 |
| P7 | Transformation invalidation and static certification | P6 |
| P8 | Symbolic/runtime-dynamic model and certification | P7, design review |
| P9 | Broadcasting, symbolic matmul, and final certification | P8 |
| P10 | Trigger contract and enforcement audit | P9 |

P1 and P2 are the only planned concurrent coding streams. Later PRs modify
shared type/value relationships and should remain serial to keep review and
rollback reasoning tractable.

## Compatibility Gates For Every PR

1. `git diff --check` and no-tab scan pass.
2. No new `be.so` library dependency or frontend-builder symbol appears.
3. `be.so`, `be`, `lw_inline`, and affected IR tools rebuild where applicable.
4. Existing DSL native syntax and target-layout matrix passes.
5. Existing binary WHIRL reopens through the mapped-image reader.
6. `ir_b2a -st -src` shows logical DSL names and the same descriptor state used
   by verification and lowering.
7. No existing `TY_IDX` is renumbered, removed, or mutated after sealing.
8. No new binary section lands without a separate reviewed versioned contract.
9. Failed runs publish no final `.B` or misleading partial artifact.

## Active Queue

The per-PU SP0-SP10 implementation queue is complete. Future interprocedural
shape work is collected separately in
`doc/IPA-DSL-SHAPE-PROPAGATION-TODO.md`. That document is an incubating
research queue, not a dependency of the current per-PU implementation. It
becomes actionable only under `-ipa` after the IPA summary and call-graph
contracts are reviewed.

1. **IPA-S0 and IPA-S1 research.** May collect architecture evidence in
   parallel, but no cross-PU code begins before the IPA owners review the
   summary inventory and semantic transfer contract.

## Completion Criteria

The project is complete when:

1. Python supplies seed facts but performs no graph-wide compiler inference.
2. One common set of versioned shape functions serves admission, propagation,
   strict verification, and transformation revalidation.
3. Repeated equivalent refinement does not grow the tensor type table.
4. Existing canonical tensor types remain immutable and valid for unaffected
   users.
5. Per-PU retyping is atomic and owner-safe; any future cross-PU retyping is
   confined to an explicitly enabled IPA pass with call-graph scope.
6. Static ResNet and Llama prefill pass through the normal `-O0` pipeline with
   compiler-refined types.
7. Llama decode preserves reviewed dynamic or symbolic sequence semantics.
8. FHE and other domains consume the same refined TensorDescriptorIR without a
   private shape implementation.
9. Binary WHIRL compatibility, separate-process reopen, `ir_b2a -st -src`, and
   `whirl2c` evidence are certified.
10. Every deferred caveat in the architecture document is either resolved or
    remains behind an explicit fail-closed boundary.

## Related Documents

- `doc/WHIRL-DSL-SHAPE-PROPAGATION-DESIGN.md`
- `doc/WHIRL-DSL-SYMBOLIC-SHAPE-CONTRACT.md`
- `doc/WHIRL-DSL-TENSOR-TYPE-HANDLING.md`
- `doc/WHIRL-DSL-INFRASTRUCTURE.md`
- `doc/VHO-DSL-OPTIMIZATION-PLAN.md`
- `doc/WOPT-DSL-ADAPTATION-PLAN.md`
- `doc/Open64_Python_FE_Plan.md`
- `AGENTS.md`
