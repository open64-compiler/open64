# WHIRL DSL Tensor Shape Propagation Implementation Plan

## Status

Execution plan for `WHIRL-DSL-SHAPE-PROPAGATION-DESIGN.md`. The architecture
document is authoritative for semantics; this document orders the coding work,
review gates, validation, and pull-request boundaries.

No implementation milestone may weaken binary WHIRL compatibility, mutate a
sealed tensor type in place, duplicate shape formulas in independent services,
or expose a partially retyped WHIRL program.

The frozen PR range contains implementation work through SP10, but its repair
status is not complete:

- SP1 implementation exists in excluded base history. The short ID previously
  listed here does not resolve, so exact SP1 commit traceability is pending and
  no replacement hash is inferred.
- SP2 is implemented by
  `62cc9e9eeaac02c0732ec7a684110e88a13476ed`. Its frozen verdict remains Pass.
- SP3 is implemented by
  `aa8c7a8f9af5c400a928ec305cda1d15e7e8437e`. Its frozen verdict remains
  Partial because insertion-order independence is not yet executable evidence.
- SP4's document contract is updated by the PR 137 WP0 freeze, but SP5
  conformance to shape-only authorization and complete preflight is pending.
- SP5 implementation exists in the frozen range but retains P1-01, P2-02, and
  P2-03 repair obligations.
- SP6's driver-owned per-PU policy is retained; executable exact-once multi-PU
  invocation and isolation evidence remains pending.
- SP7-SP8 implementations exist, while reviewer-accessible pipeline evidence
  and the symbolic foreign-qualifier trust boundary remain pending.
- SP9 and SP10 documents record historical certification claims. Repair
  certification remains pending until the complete claim matrix is rerun with
  reviewer-accessible artifacts. P2-08 pass execution/effect registration also
  remains open.

The frozen 17-commit verdicts remain 5 Pass, 11 Partial, and 1 Fail. Later
repair work may change the repair endpoint, but never rewrites those historical
per-commit verdicts.

## Authority And Repair Boundaries

`DSC_FHE_Compiler_Architecture_and_Integration_Plan_v0.10.md` is the highest
architecture and overall implementation authority. `AGENTS.md` is mandatory
repository policy, and `WHIRL.pdf` is the representation baseline. This plan
is subordinate. Because `WHIRL.pdf` is absent from the current checkout, the
repair makes no opcode, `TY_KIND`, ELF section, mapped-image layout, node
encoding, reader/writer format, or printer-format change.

The existing shared `TENSOR` / `TY_TENSOR` model remains authoritative. Common
owns logical shape; CNN owns CNN semantics; FHE owns encryption, scale, level,
noise, and keys; CNN/FHE lowering owns encrypted layout and bootstrap planning.
Compiler metadata does not participate in tensor type equivalence. Domain
operators remain visible until their owning gatekeeper, adaptation, and
lowering phases complete.

The shape service is common legality infrastructure and does not become a
pipeline authority. It follows the v0.10 capture, high-level WHIRL, FHE
gatekeeper, CNN-to-FHE, FHE canonicalization, encrypted-layout, SIHE, CKKS,
HPOLY, middle-WHIRL, whirl2c, and C-ABI order. It also preserves the v0.10
optimization-level limits. ResNet-20/CIFAR-10 is the primary full-model case;
Llama and multi-PU cases are supplemental shape regressions. PR 137 repair is
at most M0-M2 supporting infrastructure and completes no master-plan milestone.

WP0 freezes D1-D4 as follows:

1. Canonical type equivalence is separate from same-value shape-only update
   authorization. Only monotonic `logical_shape` dimension refinement with
   unchanged rank is allowed; every other stored tensor, descriptor, domain,
   and unknown field remains unchanged.
2. Syntax parsing and printing may preserve symbolic text. Equality or proof
   use requires the active owner or a reviewed host-independent interface
   mapping; coordinated cross-PU propagation remains IPA-owned.
3. Every executable pass declares minimum optimization level, maximum scope,
   required canonical form, effect, invalidation/revalidation behavior, and
   controlling option. Missing, unknown, or mismatched contracts prevent
   execution. Per-pass effect storage is recommended local design, not a
   v0.10-mandated API.
4. One composed active-PU boundary gate takes explicit PU/context input and
   validates call ABI, PU interface, and REGION relationships at admission and
   before every successful shape-driver return. Common code never traverses or
   activates another PU.

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
2. Tensor equivalence and hashing already reach the shared tensor carrier,
   spelled `KIND_TENSOR` by the current implementation, through
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
agreement, active-PU REGION validation, local commit ordering, and rejection
without mutation. In v1, call ABI, PU formal, and return relationships are
check-only active-PU boundary contracts. Retyping does not change a signature,
the opposite side of a call boundary, or another PU. Table-only mutation
helpers must remain private.

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

The backend driver owns complete program traversal, active local-symbol-table
selection, begin/PU/end lifecycle, and per-PU service invocation. Common and
VHO shape code operate only as services over an explicitly supplied active
PU/context. They may preflight and apply an authorized local mutation plan,
trace it, and run strict verification, but they never traverse, reactivate, or
mutate another PU. They do not own operator shape formulas.

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
   source, phase trace, and diagnostics in a reviewer-accessible host-visible
   directory. Historical `/private/tmp` paths are claims to reproduce, not
   current evidence.

## Milestone Queue

### SP0: Baseline And Contract Inventory

Status: historical baseline for the SP1 through SP3 reviews. Current repair
traceability and evidence are governed by the authority section above.

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

Status: implemented in excluded base history; exact commit traceability is
pending because the previously cited short ID does not resolve.

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

Status: implemented by
`62cc9e9eeaac02c0732ec7a684110e88a13476ed`; the frozen commit verdict is
Pass and is not changed by this repair.

Actions:

1. Reuse the tensor owner's authoritative normalized canonical key and
   equivalence service; this shape plan does not define a parallel key or field
   policy.
2. Add a backend-safe common API for interning a complete descriptor or a
   shape-refined copy of an existing canonical tensor type.
3. Build a runtime canonical index from existing mapped tensor types.
4. Use hash lookup followed by the tensor owner's authoritative canonical
   equivalence check.
5. Select the lowest valid existing `TY_IDX` for legacy duplicate types without
   deleting or renumbering records.
6. Change `DSL_Builder_Intern_Tensor_Type()` to call the common interner.
7. Prove lookup-before-create behavior. No unused candidate may remain when an
   equivalent type already exists.

Tests:

- requests that the tensor owner's authoritative equivalence classifies as
  equivalent return one `TY_IDX` with no `Ty_tab` growth;
- requests that the owner equivalence distinguishes return different `TY_IDX`
  values;
- canonical interning delegates the complete equivalence decision, including
  Common-, CNN-, and FHE-owned state, to that owner service; this shape plan
  defines no excluded-field list and does not define or take over domain
  equivalence;
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

Status: implemented by
`aa8c7a8f9af5c400a928ec305cda1d15e7e8437e`; the frozen commit verdict is
Partial and insertion-order evidence remains pending.

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

Status: the normative v1 document is updated by the PR 137 WP0 freeze. Product
conformance is pending WP1-WP4 repair and evidence. The slice remains uniquely
owned local native operator results; formal, return, call, constant,
function-type, and cross-PU changes remain check-only.

Actions:

1. Specify the exact request row: owner PU, value ID, expected old `TY_IDX`, and
   refined `TY_IDX`.
2. Define exhaustive preflight coverage for WN, ST, DSL values, node results,
   tensor constants, call ABI, PU formals, function types, `TYLIST`, returns,
   and REGION interfaces.
3. Define which references are authoritative and which are derived projections.
4. Define rollback and failure behavior for one active PU.
5. Define shared-symbol and shared-callee conflict policy.
6. Keep v1 limited to uniquely owned local results and fail closed for
   formal/function-type changes. Cross-PU changes remain future IPA-owned work,
   not an SP6 extension.
7. Review mapped-image and previous-reader consequences before implementation.

Recommended v1 restriction:

- mutate uniquely defined, non-address-taken local tensor result symbols first;
- require all uses and managed rows to agree with the expected old type;
- leave formal, return, and cross-PU refinement in check-only mode until a
  separately reviewed IPA-owned extension exists.

Exit gate SP4:

- common/com and VHO owners approve one preflight/commit protocol;
- no public table-only mutation helper is introduced;
- rejection cases and rollback tests are enumerated before coding.

PR boundary: documentation and test scaffolding only, if needed.

### SP5: Per-PU Canonical Refinement And VHO Driver

Dependencies: SP2, SP3, and approved SP4.

Status: implementation exists in the frozen range. Repair is pending for
shape-only authorization, complete owner preflight, and the full rollback
matrix; this milestone is not certified complete.

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

Historical implementation claims requiring repaired tests and retained
reviewer-accessible evidence:

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

Historical local review paths, unavailable in the current review environment:

```text
/private/tmp/open64-shape-sp5/artifacts/shape/sp5-refinement/shape_refine.B
/private/tmp/open64-shape-sp5/artifacts/shape/sp5-refinement/shape_refine.T
/private/tmp/open64-shape-sp5/artifacts/shape/sp5-refinement/validation.log
/private/tmp/open64-shape-sp5/artifacts/shape/sp5-refinement/certification.txt
```

### SP6: Driver-Owned Per-PU Completion And Interface Verification

Dependencies: SP5.

Status: the driver-owned per-PU policy is accepted. Executable proof of
backend-selected exact-once invocation, colliding-index isolation, and
success/failure publication behavior remains pending; this milestone is not
certified complete. `Preorder_Process_PUs()` selects each PU and
`Preprocess_PU()` invokes `VHO_DSL_Shape_Refine_Driver()` before its local
shape consumers.

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

Historical SP6 local review paths, unavailable in the current review
environment:

```text
/private/tmp/open64-shape-sp5/artifacts/shape/sp6-pu-driver/shape_refine.B
/private/tmp/open64-shape-sp5/artifacts/shape/sp6-pu-driver/shape_refine.T
/private/tmp/open64-shape-sp5/artifacts/shape/sp6-pu-driver/validation.log
/private/tmp/open64-shape-sp5/artifacts/shape/sp6-pu-driver/certification.txt
```

PR boundary: driver-owned per-PU lifecycle clarification and certification.

### SP7: Backend Pipeline Integration And Invalidation

Dependencies: SP6.

Status: integration code exists, but P1-02 reviewer-accessible completion
evidence and P2-04 all-success-path boundary validation remain pending.

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

Historical endpoint behavior to retain while repairing its open gates:

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
   A new pass cannot execute until its registration supplies a complete,
   recognized, and matching contract. A valid contract then declares whether
   the pass preserves, invalidates, or refines shape state.
8. Inlining, cloning, specialization, outlining, and coordinated signature
   changes must use the same trigger contract when their owning driver is
   implemented. Their mention in the contract does not grant the current
   per-PU VHO driver cross-PU scope; interprocedural scheduling remains future
   `-ipa` work.

Historical SP7 local review paths, unavailable in the current review
environment:

```text
/private/tmp/open64-shape-sp5/artifacts/shape/sp7-pipeline/shape_refine.B
/private/tmp/open64-shape-sp5/artifacts/shape/sp7-pipeline/shape_refine.T
/private/tmp/open64-shape-sp5/artifacts/shape/sp7-pipeline/validation.log
/private/tmp/open64-shape-sp5/artifacts/shape/sp7-pipeline/certification.txt
```

PR boundary: driver, options, and invalidation integration.

### SP8: Symbolic And Runtime-Dynamic Dimensions

Dependencies: SP7 and a separate review of the open symbolic-design topics.

Status: implementation exists for the reviewed v1 syntax slice, but semantic
admission of foreign PU qualifiers remains a live repair item. Repair
certification is pending. The normative details are in
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

Historical SP8 local review paths, unavailable in the current review
environment:

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

Status: historical completion claim only. The referenced local artifacts are
not reviewer-accessible, so the full matrix and provenance remain pending
independent repair certification. See
`WHIRL-DSL-SHAPE-SP9-CERTIFICATION.md` for the claim to reproduce.

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

Status: structured trigger implementation exists, but complete repair
certification and the pass execution/effect registration contract remain
pending. The normative trigger taxonomy is published in
`WHIRL-DSL-SHAPE-PROPAGATION-DESIGN.md`;
`WHIRL-DSL-SHAPE-SP10-CERTIFICATION.md` records the historical claim to
reproduce, not reviewer-accessible proof.

Actions:

1. Audit every current shape-refinement and invalidation site against the
   trigger contract: initial per-PU admission, DSL WOPT/Preopt, FHE conversion,
   fixed-order VHO DSL optimization, and the pre-lowering currency gate.
2. Replace free-form invalidation reasons with a runtime-only structured trigger
   identity and stable diagnostic name. Do not add persisted metadata, a WHIRL
   section, or a binary compatibility dependency.
3. Require every executable transformation to declare its minimum optimization
   level, maximum scope, required canonical form, controlling option, and one
   of shape preserving, monotonic refining, locally invalidating, or boundary
   invalidating. A missing, invalid, unknown, or mismatched declaration
   prevents execution and fails closed; treating it as invalidating does not
   authorize it to run.
4. Test that seed construction remains a frontend/admission responsibility and
   that authoritative graph-wide retyping occurs only in the compiler-owned
   per-PU pass.
5. Test current-generation rejection at every shape-consuming boundary covered
   by the current pipeline. Leave future inlining, cloning, specialization,
   outlining, and cross-PU IPA scheduling to their owning implementations, but
   require them to adopt this contract when introduced.

Exit gate SP10:

- every current trigger has an owner, structured identity, and focused test;
- an unclassified or mismatched transformation cannot execute;
- no frontend, binary WHIRL, or cross-PU scope expansion is introduced; and
- the SP9 claim matrix is rerun and independently reviewable before either
  stage is marked repair-certified.

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

The frozen range contains implementation work through SP10, but the PR 137
repair queue remains open through WP10. SP9 and SP10 repair certification is
pending, and no historical stage claim is upgraded by this document update.
Future interprocedural shape work is collected separately in
`doc/IPA-DSL-SHAPE-PROPAGATION-TODO.md`. That document is an incubating
research queue, not a dependency of the current per-PU implementation. It
becomes actionable only under `-ipa` after the IPA summary and call-graph
contracts are reviewed.

1. **IPA-S0 and IPA-S1 research.** May collect architecture evidence in
   parallel, but no cross-PU code begins before the IPA owners review the
   summary inventory and semantic transfer contract.

## Shape-Subsystem Repair Completion Criteria

The repaired PR 137 shape subsystem is complete only when:

1. Python supplies seed facts but performs no graph-wide compiler inference.
2. One common set of versioned shape functions serves admission, propagation,
   strict verification, and transformation revalidation.
3. Repeated equivalent refinement does not grow the tensor type table.
4. Existing canonical tensor types remain immutable and valid for unaffected
   users.
5. Per-PU retyping is atomic and owner-safe; any future cross-PU retyping is
   confined to an explicitly enabled IPA pass with call-graph scope.
6. The primary ResNet-20 case passes the applicable normal `-O0` shape path
   with compiler-refined or independently verified types.
7. Supplemental Llama prefill and decode cases preserve the reviewed static,
   dynamic, or symbolic semantics without replacing ResNet-20 acceptance.
8. FHE and other domains consume the same refined TensorDescriptorIR without a
   private shape implementation.
9. Binary WHIRL compatibility, separate-process reopen, `ir_b2a -st -src`, and
   `whirl2c` evidence are certified.
10. Every deferred caveat in the architecture document is either resolved or
    remains behind an explicit fail-closed boundary.

These criteria certify only the PR 137 shape subsystem. They do not complete
M0-M8 or the full FHE architecture.

## Related Documents

- `doc/WHIRL-DSL-SHAPE-PROPAGATION-DESIGN.md`
- `doc/WHIRL-DSL-SYMBOLIC-SHAPE-CONTRACT.md`
- `doc/WHIRL-DSL-TENSOR-TYPE-HANDLING.md`
- `doc/WHIRL-DSL-INFRASTRUCTURE.md`
- `doc/VHO-DSL-OPTIMIZATION-PLAN.md`
- `doc/WOPT-DSL-ADAPTATION-PLAN.md`
- `doc/Open64_Python_FE_Plan.md`
- `AGENTS.md`
