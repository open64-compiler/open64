# WHIRL DSL Tensor Shape Propagation Design

## Status

Draft for architectural review. This document captures the current proposal
for compiler-owned tensor shape propagation and refinement. Immutable and
uniqued canonical tensor types are an adopted design requirement. The phase
boundary, API names, dimension-expression model, and WHIRL value-rebinding
protocol still require refinement before implementation begins. Nothing in
this draft allocates a new binary WHIRL section or establishes a released API
or ABI.

## Purpose

Very High Level WHIRL must carry enough tensor information for verification,
optimization, domain planning, implementation selection, and lowering. A
Python frontend can provide shapes observed in source models, sample inputs,
parameters, and buffers, but it must not become a second compiler that
duplicates every versioned DSL operator shape rule.

This design makes authoritative graph-wide shape propagation an Open64
compiler service. It defines:

1. The boundary between frontend seed facts and compiler inference.
2. A reusable shape-constraint engine in `osprey/common/com`.
3. A WHIRL phase driver in `osprey/be/vho`.
4. Cross-PU and structured-region propagation.
5. Canonical TensorDescriptorIR refinement without mutating sealed types.
6. Gatekeeper, optimization, binary compatibility, and inspection contracts.

## Design Principles

1. Shape propagation operates on logical DSL operators and TensorDescriptorIR,
   not Python objects or private physical `OPR_DSL` fields.
2. The gatekeeper, shape-refinement pass, and transformation verifiers share
   one set of versioned operator shape functions.
3. Shape refinement is mandatory legality work at `-O0` and above. It is not
   an optional profitability optimization.
4. Tensor shape is separate from layout, placement, sharding, memory,
   quantization, alignment preference, and value-specific runtime or FHE state.
5. Canonical tensor types are immutable. Refinement interns a new or existing
   `TY_IDX` and rebinds affected values atomically.
6. Valid symbolic or runtime-dynamic dimensions are complete semantic facts.
   An unresolved `<pending>` dimension is not.
7. Existing mapped-image and ELF WHIRL mechanisms remain authoritative. The
   first implementation should not require a new binary section.
8. A transformation that changes operands, attributes, results, calls,
   returns, or region interfaces must preserve shape facts or invalidate them
   and schedule refinement again.

## Responsibilities

### Python frontend and torch2whirl

The frontend supplies source-observed seed facts:

- sample-input dtype, rank, and logical shape;
- parameter and buffer dtype, rank, shape, and payload descriptor;
- operator operands and explicit source attributes;
- external tensor storage evidence;
- PU, formal, actual, return, callsite, class, instance, and source provenance;
- source positions and source-observed layout hints.

Frontend tests prove that these facts are present and locally well formed. They
must not duplicate graph-wide compiler shape algorithms.

### Native builder and admission gatekeeper

The native builder constructs TensorDescriptorIR seeds and managed DSL
relationships. The admission gatekeeper verifies:

- valid image rows and IDs;
- known logical operator and semantic version;
- operand count and required typed attributes;
- value, result symbol, ownership, and source-position consistency;
- locally well-formed dtype, rank, and dimension syntax;
- legal use of static, symbolic, runtime-dynamic, and pending dimensions.

Admission means that compiler analysis can safely consume the IR. It does not
claim that every result descriptor is maximally refined.

### Common shape engine

The semantic engine belongs in:

```text
osprey/common/com/dsl_shape.h
osprey/common/com/dsl_shape.cxx
```

It owns:

- the shape lattice and canonical dimension expressions;
- versioned operator shape-function registration and lookup;
- constraint creation;
- fixed-point solving;
- contradiction and unresolved-fact diagnostics;
- creation of canonical refined tensor descriptors;
- services shared by the gatekeeper, VHO, IR tools, and later optimizers.

It must not depend on Python, frontend builder state, WOPT classes, FHE
implementation code, code generation, or an external library.

### VHO phase driver

WHIRL traversal and phase orchestration belong in:

```text
osprey/be/vho/dsl_shape_refine.h
osprey/be/vho/dsl_shape_refine.cxx
```

The public spelling follows existing Open64 acronym conventions:

```c++
VHO_DSL_Shape_Refine_Driver
```

The driver owns:

- program and PU traversal;
- active local-symbol-table coordination;
- collection of WN, DSL image, call, return, and REGION relationships;
- preflight and application of refined `TY_IDX` bindings;
- phase statistics, tracing, and diagnostics;
- post-refinement strict gatekeeper invocation.

Individual operator shape formulas do not belong in the VHO driver.

## Pipeline Placement

The required logical order is:

```text
mapped binary WHIRL input
  -> DSL admission gate
  -> compiler-owned tensor shape refinement
  -> strict DSL gate
  -> optional DSL WOPT / Preopt
  -> shape refinement when invalidated
  -> strict DSL gate
  -> domain planning and conversion, including FHE
  -> optional VHO DSL optimization
  -> shape refinement when invalidated
  -> strict DSL gate
  -> VHO_DSL_Lower_Driver
  -> VHO_Lower_Driver
  -> canonical WHIRL optimization and code generation
```

The first backend invocation belongs in `osprey/be/be/driver.cxx`, before DSL
WOPT and before FHE conversion. Putting the first invocation only inside
`VHO_DSL_Lower_Driver()` would be too late because domain planning and
optimization already require authoritative shapes.

`VHO_DSL_Lower_Driver()` should retain a defensive check that the active shape
refinement generation is current before it consumes tensor descriptors.

## Shape Information Model

### Rank

Rank is either pending or known. A known rank fixes the number of dimension
slots even if individual extents remain symbolic or runtime dynamic.

### Dimension lattice

The initial abstract lattice is:

```text
pending
  -> anonymous runtime dynamic
  -> symbolic expression
  -> static positive extent

incompatible facts
  -> contradiction
```

This diagram expresses increasing precision, not an assertion that every
runtime dimension can become static. The exact relationship between anonymous
dynamic and named symbolic dimensions remains an open refinement topic.

### Canonical expressions

The solver initially needs canonical forms for:

- positive integer constants;
- named dimension symbols;
- equality classes;
- products used by reshape and flatten;
- addition, subtraction, multiplication, floor division, and ceiling division
  required by published convolution, pooling, and slicing contracts;
- ordered dimension vectors and permutations.

The implementation should provide the expressions needed by published DSL
operators. It should not begin as a general-purpose theorem prover.

## Immutable And Uniqued Tensor Types

### Adopted policy

A sealed canonical tensor `TY_IDX` is immutable. Shape refinement must never
change its rank, dimensions, element type, traits, or representation fields in
place. A canonical tensor type may already be referenced by unrelated values,
symbols, function interfaces, constants, calls, or REGION interfaces. Mutating
it would silently change the meaning of every such reference.

When inference proves a more precise shape for a value, the compiler shall:

1. Read the value's existing canonical tensor descriptor.
2. Copy the descriptor into a refinement candidate.
3. Replace only the type-core facts justified by the shape proof.
4. Preserve all unaffected canonical fields exactly.
5. Look up the complete canonical key before allocating a new `TY` record.
6. Reuse the existing `TY_IDX` when an equivalent canonical tensor type exists.
7. Otherwise append and seal one new canonical tensor `TY_IDX`.
8. Rebind only the affected value and its required WHIRL projections.

The old `TY_IDX` remains valid and continues to describe every unaffected user.
Refinement is therefore copy-on-write at the type level and value-specific at
the IR level.

### Canonical key and uniquing

The canonical key must follow the reviewed TensorDescriptorIR type-equivalence
contract. At minimum it includes the element `TY_IDX`, tensor kind, dtype,
rank, normalized logical dimensions, and every trait or representation field
that the current type contract declares semantically significant. Shape
refinement does not independently broaden or narrow that equivalence contract.

Names, source positions, diagnostics, pass ownership, lineage, profiling, and
value-specific runtime or FHE state do not participate in canonical type
identity. They remain associated with the value or compiler metadata.

Uniquing must use lookup-before-create behavior. A runtime canonical index may
hash the normalized key for efficiency, but a hash match must be confirmed by
complete structural equality. The index is runtime-only and must be rebuilt
deterministically from the mapped type and tensor-extension tables after binary
WHIRL input. If an older image already contains structurally duplicate tensor
types, the lowest valid existing `TY_IDX` should be selected as the canonical
representative for new refinements; the reader must not renumber or delete
persisted type records.

The common service is expected to provide an API conceptually equivalent to:

```c++
TY_IDX TY_Intern_Refined_Tensor_Type(
    TY_IDX base_ty,
    const TENSOR_TYPE_CORE_REFINEMENT *refinement,
    BOOL *created);
```

The spelling and parameter structure remain provisional. The important
contract is that it returns an existing equivalent `TY_IDX` whenever possible
and does not leave a duplicate candidate in the type table when it does so.

### WHIRL type-system caveat and deferred work

This policy is simpler in an SSA IR where an operation result has one direct
type association. WHIRL tensor type evidence can be projected through several
structures: `WN` type fields, result and formal `ST` entries, DSL value and
node records, function types and `TYLIST`, call ABI rows, PU interface rows,
returns, tensor constants, and REGION interfaces. Physical local `ST_IDX`
values also require the owning PU's symbol table to be active.

Consequently, obtaining a new or existing refined `TY_IDX` is only the first
part of refinement. Open64 must later define and certify a preflighted atomic
rebind transaction that updates every required projection without exposing a
partially retyped WHIRL program. It must also define what happens when one
shared symbol, formal, callee, or REGION interface is reached with incompatible
context-specific refinements. Possible resolutions include preserving a
symbolic type, materializing a uniquely owned temporary, cloning or
specializing a PU, or failing closed.

The exact transaction, rollback boundary, shared-symbol policy, and cross-PU
specialization rules are deliberately deferred design work. Until they are
reviewed, implementations may perform check-only inference but must not update
only one of the WN, ST, DSL image, call, return, or REGION representations.

## Versioned Operator Shape Functions

`DSL_SHAPE_RULE` remains the broad classification already stored with an
operator, such as identity, broadcast, contraction, reduction, view, layout,
or runtime guarded. It is not sufficient to define a complete formula.

Exact shape-function lookup uses:

```text
(logical DSL operator, semantic version)
```

A proposed common interface is:

```c++
typedef BOOL (*DSL_SHAPE_FUNCTION)
    (const DSL_SHAPE_OPERATOR_INPUT *input,
     DSL_SHAPE_CONSTRAINT_SET *constraints,
     DSL_SHAPE_DIAGNOSTIC *diagnostic);

extern BOOL DSL_Shape_Infer_Operator
    (const DSL_SHAPE_OPERATOR_INPUT *input,
     DSL_SHAPE_CONSTRAINT_SET *constraints,
     DSL_SHAPE_DIAGNOSTIC *diagnostic);
```

Built-in operators use a statically seeded registry. Domain extensions may use
a reviewed append-only registration service, but registration must not replace
or silently broaden an existing `(operator, version)` contract.

The current shape formulas in `dsl_gatekeeper.cxx` should be extracted into
this service. The gatekeeper then calls the same service in check-only mode.
Open64 must not maintain one inference implementation and a second independent
verification implementation.

## Constraint Graph

The program graph contains one shape variable for each tensor value plus rank
and dimension variables as needed. Constraints come from:

1. TensorDescriptorIR seed facts.
2. Versioned logical operator shape functions.
3. Result symbols and defining `STID` nodes.
4. PU input formals and hidden result formals.
5. Call actual-to-formal relationships.
6. Return and caller-result relationships.
7. REGION input, output, and live-out interfaces.
8. Shape assertions and reviewed runtime guards.

Source positions and compiler metadata identify diagnostics but do not
participate in shape equivalence.

## Fixed-Point Algorithm

### 1. Collect

Enumerate all logical DSL nodes and all managed inter-value relationships.
Resolve each value to its owner PU, `ST_IDX`, `TY_IDX`, TensorDescriptorIR, and
source evidence.

### 2. Build constraints

Invoke the exact operator shape function and add call, formal, return, and
REGION constraints. Reject unsupported versions rather than guessing a rule
from a similarly named operator.

### 3. Solve

Use a dependency worklist. A refinement of one rank or dimension schedules
only constraints that consume it. Merges are monotonic. Incompatible static
extents, ranks, dtypes, or representation facts produce a stable diagnostic.

### 4. Classify completion

At the fixed point, classify every result as:

- unchanged and complete;
- refined and complete;
- valid symbolic or runtime dynamic;
- pending but allowed until a named later phase;
- unresolved where the next phase requires completion;
- contradictory or malformed.

### 5. Construct a mutation plan

For each refined value, copy the existing canonical descriptor, replace only
the facts justified by inference, and invoke the canonical lookup-before-create
refinement service. Shape refinement must not alter representation fields
without a separate proof. The service reuses an equivalent `TY_IDX`; it creates
and seals a new one only when no equivalent canonical type exists.

### 6. Preflight

Before mutation, verify the expected old type and ownership across:

- physical WN result and operand references;
- result and formal symbols;
- DSL value and node records;
- call ABI argument rows;
- PU interface rows;
- return relationships;
- REGION interface rows.

### 7. Commit

Apply a complete preflighted request set through one common mutation API. The
API updates all physical and managed references together. It must not expose a
table-only mutation that can leave WN, ST, TY, and managed-image state
inconsistent.

### 8. Verify

Run the strict gatekeeper and managed-image validators. No downstream phase
may observe a partially refined descriptor family.

## Proposed Driver API

The exact signatures remain provisional:

```c++
typedef struct {
    UINT32 visited_node_count;
    UINT32 refined_value_count;
    UINT32 unchanged_value_count;
    UINT32 symbolic_value_count;
    UINT32 pending_value_count;
    UINT32 contradiction_count;
    UINT32 iteration_count;
} VHO_DSL_SHAPE_REFINE_RESULT;

extern BOOL VHO_DSL_Shape_Refine_Begin_Program
    (PU_Info *pu_tree, FILE *diagnostic);

extern BOOL VHO_DSL_Shape_Refine_Program_Unit
    (PU_Info *pu_info, WN **tree, FILE *diagnostic,
     VHO_DSL_SHAPE_REFINE_RESULT *result);

extern WN *VHO_DSL_Shape_Refine_Driver
    (PU_Info *pu_info, WN *tree);

extern BOOL VHO_DSL_Shape_Refine_End_Program
    (FILE *diagnostic, VHO_DSL_SHAPE_REFINE_RESULT *aggregate);
```

The begin/PU/end protocol is needed because shape relationships cross PU
boundaries while physical local symbols can be updated only with the owning
PU's local symbol table active.

`Begin_Program` builds and solves the global constraint graph from managed
records. The per-PU driver applies the solved plan while the correct PU is
active. `End_Program` proves complete PU coverage and final fixed-point
validity. This coordination state is runtime-only.

## Proposed Atomic Retyping API

The common layer needs a batch operation conceptually equivalent to:

```c++
typedef struct {
    ST_IDX owner_pu_st;
    DSL_IR_VALUE_ID value_id;
    TY_IDX expected_old_ty;
    TY_IDX refined_ty;
} DSL_IR_VALUE_TYPE_REFINEMENT_REQUEST;

extern BOOL DSL_IR_Refine_Native_Value_Types
    (PU_Info *pu_info,
     WN *tree,
     const DSL_IR_VALUE_TYPE_REFINEMENT_REQUEST *requests,
     UINT32 request_count,
     FILE *diagnostic);
```

This is an architectural sketch, not a frozen API. The implementation must
follow the established preflight-then-commit pattern used by reviewed DSL IR
rewrite services.

## Gatekeeper Modes

The existing strict verification behavior remains the compatibility default.
Add an explicit admission mode rather than silently weakening it:

```text
DSL_GATEKEEPER_ADMISSION
DSL_GATEKEEPER_STRICT
```

Admission permits reviewed pending or symbolic shape states. Strict mode
requires every fact needed by the next phase and compares the descriptor with
the shared operator shape-function result.

## Transformation Invalidation

Every DSL transformation must declare whether it:

- preserves all shape facts;
- refines facts monotonically;
- invalidates local result shapes;
- invalidates cross-PU or REGION constraints.

For the first implementation, conservatively rerun refinement after any
executed DSL transformation that changes the graph. Later, the fixed-order VHO
DSL pass registry may add explicit preservation and invalidation properties.
The existing optional descriptor-propagation stage is broader than mandatory
shape refinement and must not be used as its only implementation.

## Options And Debugging

Proposed controls are:

```text
-DSL:shape_refine=on|off
-DSL:dump_after_shape_refine=on|off
```

Shape refinement defaults on at every optimization level. Turning it off is a
triage mode: the compiler performs strict check-only verification and accepts
only an artifact whose descriptors are already complete. Turning it off must
not permit inconsistent or pending-required shapes to reach lowering.

The trace should report PU, value, old type, refined type, operator contract,
constraint source, fixed-point iteration, and diagnostic source position. A
retained binary artifact must be inspectable with `ir_b2a -st -src` using the
same refined TensorDescriptorIR consumed by lowering.

## Binary WHIRL Compatibility

The first implementation should express refined state by rebinding existing
DSL values and symbols to canonical tensor `TY_IDX` records. It should reuse
the existing DSL value, call ABI, PU interface, REGION, symbol, and type
records.

No new physical field should be added merely to record that inference happened.
Pass generation and dirty state are runtime compiler state. If later symbolic
expressions require a persisted normalized table, that table needs a separate
optional-section proposal with row sizes, alignment, reader/writer behavior,
mapped validation, old-reader behavior, printing, and migration tests.

## Inspection Contract

After refinement:

- `ir_b2a -st -src` prints the refined rank and logical shape in the type and
  symbol evidence;
- logical DSL nodes retain their operator names and versions;
- pending dimensions print as `<pending>` only when their continued presence
  is legal;
- symbolic and runtime-dynamic dimensions remain distinguishable;
- `whirl2c` and lowering consume the same refined types and do not recompute a
  private shape approximation;
- compiler metadata may identify the inference source but does not affect type
  equivalence.

## Initial Operator Coverage

The first vertical slice should support:

1. Identity and same-shape operators.
2. `common.add` and `common.mul` broadcast rules.
3. `common.matmul` contraction rules.
4. `common.linear` rank-generic projection.
5. `common.reshape`, `common.transpose`, and `common.flatten`.
6. `cnn.conv2d`, inference BatchNorm, max pool, and global average pool.

The next slice adds transformer batched matmul, RMSNorm, rotary embedding,
attention, and SwiGLU, followed by stateful decode relationships. Operator
coverage is version-specific; a new semantic version does not inherit the old
shape function automatically.

## Validation Plan

### Common engine tests

- static equality and contradiction;
- broadcasting, contraction, permutation, product, convolution, and reduction;
- symbolic and pending dimensions;
- deterministic fixed point independent of insertion order;
- unknown operator version rejection;
- descriptor interning and deduplication;
- repeated refinement returning the same canonical `TY_IDX` without growing
  the type table;
- one refined value leaving unrelated users of its original `TY_IDX`
  unchanged;
- mapped-image canonical-index reconstruction in the presence of legacy
  duplicate tensor types.

### VHO driver tests

- physical WN and managed-image agreement;
- batch preflight rollback;
- cross-PU actual/formal/return propagation;
- colliding local ST indices in different PUs;
- REGION input and result propagation;
- re-entry after cloning, inlining, outlining, and canonicalization;
- disabled-pass check-only behavior;
- no change for non-DSL WHIRL.

### Artifact tests

- mapped binary write and separate-process reopen;
- old-image readability;
- `ir_b2a -st -src` refined descriptor evidence;
- `whirl2c` consumption;
- stable diagnostics for malformed, contradictory, and unresolved shapes;
- retained before/after traces and no partial validly named `.B` output.

## Implementation Milestones

### S0: Contract extraction

Move existing shape parsing and formulas out of the gatekeeper into a shared
check-only common service. Preserve current behavior exactly.

### S1: Per-PU static inference

Implement the lattice and worklist for the initial common operators. Compare
inferred descriptors with existing descriptors without mutating WHIRL.

### S2: Canonical refinement

Add lookup-before-create canonical tensor-type uniquing, atomic type-rebinding
support, and the per-PU VHO driver. Certify static result refinement, reuse of
equivalent `TY_IDX` records, preservation of old shared types, and unchanged
binary layout.

### S3: Cross-PU and REGION fixed point

Add begin/PU/end program coordination, call/formal/return constraints, REGION
interfaces, complete preflight, and deterministic all-PU coverage.

### S4: Transformation re-entry

Integrate invalidation with DSL canonicalization, cloning, inlining, outlining,
VHO DSL optimization, WOPT adaptation, and domain conversion.

### S5: Symbolic and runtime-dynamic refinement

Finalize the symbolic-expression subset, runtime guards, shape assertions,
inspection syntax, and lowering requirements.

## Open Refinement Topics

The following decisions remain intentionally open:

1. Whether anonymous runtime dynamic and named symbolic dimensions are ordered
   in one lattice or represented as separate complete alternatives.
2. The minimum canonical dimension-expression language and overflow rules.
3. Whether shape constraints need a persisted optional image or can remain
   entirely reconstructible from operators, attributes, and descriptors.
4. The exact admission-gate API and whether incomplete result descriptors may
   be sealed canonical pending types.
5. The exact atomic retyping API and rollback boundary across multiple PUs.
6. How transformation passes report shape preservation, invalidation, and
   changed values without disrupting the existing fixed pipeline.
7. How runtime shape guards are represented and lowered when static or
   symbolic proof is unavailable.
8. How result descriptor refinement interacts with representation fields that
   become illegal after a shape change.
9. Whether backward inference from result constraints to operands is required
   in the first implementation or introduced after forward propagation.
10. The stable diagnostic numbering and pass trace format.
11. Whether explicit seed provenance needs a new structured compiler-metadata
    record. No additional torch2whirl API is required for the first static
    slice unless this distinction becomes necessary.

These topics must be reviewed before corresponding APIs or binary contracts
are treated as stable.
