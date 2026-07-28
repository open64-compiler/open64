# WHIRL DSL Tensor Constant Folding Plan

## Purpose

This project defines compile-time evaluation of DSL tensor operations whose
semantic inputs are constant. It is specialization, not ordinary expression
simplification: the compiler executes a reviewed tensor semantic operation
for a known input instance and publishes the resulting tensor constant.

At Very High Level DSL WHIRL, a tensor is a primitive value and a constant
tensor operation is a legitimate target constant operation. Tensor constant
folding must therefore extend Open64's target constant (`TCON`) package rather
than introduce an unrelated byte-buffer constant abstraction.

Traditional `wn_simp` remains responsible for algebraic identities and for
scalar constant folding through `Targ_WhirlOp()`. The tensor extension follows
the same model through the interface defined in
`doc/WHIRL-DSL-SIMPLIFICATION-PLAN.md`. This document owns the physical and
semantic storage contract for tensor TCONs; the simplification plan owns the
prepare, evaluate, and materialize interface used by WN and WOPT.

## Existing TCON Baseline

Open64's TCON design establishes the following rules:

- `TCON` is a fixed-size target-format constant record.
- Only the constant type may be queried directly outside the TCON package.
  Constant value representation is private.
- `Targ_WhirlOp()` applies a WHIRL operation to TCON operands and returns a
  TCON result.
- `Enter_tcon()` places a result in the mapped `Tcon_Table`.
- `New_Const_Sym()` associates a `TCON_IDX` with its complete `TY_IDX`.
- WOPT represents non-integer constants through constant symbols and
  `CK_RCONST`, and reuses `wn_simp_code.h` through CODEREP adapters.

This is the architectural baseline for tensor constants. A tensor TCON has the
same public role as a scalar TCON even though its value is too large to fit in
the fixed record.

Do not enlarge `TCON`. Its size is part of the existing mapped global symbol
table image. Do not add a native tensor `MTYPE` merely to implement this stage;
that would require a separately reviewed, versioned WHIRL change.

## Tensor TCON Escape Representation

The first compatible physical representation uses an existing legal string
TCON as the private escape carrier. Its sized character-array payload begins
with a fixed-layout tensor envelope:

```text
magic
version
envelope size
storage kind
flags and reserved fields
canonical TensorDescriptorIR TY_IDX
element count
logical byte count
required alignment
compact scalar TCON_IDX, when applicable
side-file or inline payload offsets and lengths
content checksum
```

The existing TCON table owns the carrier and its `TCON_IDX` is the stable
tensor-constant identity. The existing TCON character-array table owns the
sized envelope and any inline bytes. Both tables already participate in the
standard global-symbol-table mapped-image write and reopen path. No tensor
record is appended to the strict version-1 `.WHIRL.dsl` image, and no new ELF
section is required for this stage.

The envelope contains only explicitly sized scalars and Open64 table IDs. It
contains no host pointers or STL ownership. INLINE_DENSE may place arbitrary
bytes, including embedded NUL bytes, after the envelope; all offsets and
lengths are validated against the carrier's `TCON_str_len`. SIDE_FILE_DENSE
stores a relative location contract without a host pointer or absolute
producer path.

Older readers continue to see a legal `MTYPE_STRING` TCON and preserve its
sized bytes even though they do not interpret the tensor envelope. DSL-aware
compiler code must use `TCON_is_tensor()` and tensor TCON accessors; it must
not test `MTYPE_STR`, parse envelope fields directly, or inspect private TCON
value fields. Logical dumps print the tensor constant, never the physical
escape representation.

Runtime lookup, hashing, and deduplication indexes may accelerate access, but
they are derived state. Clearing them must not make a reopened tensor TCON
unqueryable. The gatekeeper validates envelope magic, version, record size,
reserved fields, descriptor identity, bounds, alignment, checksum, and
storage-kind-specific contracts directly from the mapped TCON image.

The escape is a compatibility mechanism, not the logical tensor constant
type. A future native tensor MTYPE may replace it only through a versioned
binary WHIRL proposal with reader, writer, printer, simplifier, WOPT, and
fallback migration coverage.

## Storage Kinds

Use a closed, versioned storage-kind enum. The initial kinds are:

| Storage kind | Meaning | Payload |
| --- | --- | --- |
| `TENSOR_TCON_ZERO` | Every logical element is semantic zero | No dense payload |
| `TENSOR_TCON_ONE` | Every logical element is semantic one | No dense payload |
| `TENSOR_TCON_SPLAT` | Every logical element equals one target-format scalar TCON | One `TCON_IDX` |
| `TENSOR_TCON_INLINE_DENSE` | Small dense target-format tensor | Sized bytes following the TCON envelope |
| `TENSOR_TCON_SIDE_FILE_DENSE` | Large dense target-format tensor | External tensor reference |

ZERO and ONE are specialized splats because they are common algebraic
identities and creation values. They support constant recognition without
materializing or scanning a huge payload. They mean semantic zero and semantic
one for the tensor dtype and quantization contract; they must not be
implemented as an unconditional byte `memset`. For example, a quantized
semantic zero may use a nonzero storage zero point.

A general SPLAT stores one scalar target-format `TCON_IDX`. Shape and element
count come from the TensorDescriptorIR. Reshape, broadcast, transpose, and
other transformations that preserve a uniform value should preserve the
compact representation whenever legal.

INLINE_DENSE is for bounded constants whose bytes are small enough to remain
in the mapped image under the active materialization policy. The bytes are in
target element format and logical tensor layout, not an arbitrary host-native
array representation. The TCON character-array table is binary-safe; normal
symbol-string APIs such as `Save_Str` are not suitable for this payload.

SIDE_FILE_DENSE is the normal representation for model parameters and large
folded results. An LLM weight matrix is a constant tensor even though its bytes
reside outside the `.B` file.

## Side-File Contract

The side-file reference contains:

```text
storage format
relative side-file name
tensor key
byte offset
byte length
required byte alignment
content checksum and checksum algorithm
flags and format revision
```

The filename is relative to the directory containing the `.B` file. Absolute
producer paths are not part of the binary IR contract. The offset and length
identify exactly one immutable byte range. The checksum identifies content,
not merely a filename, and participates in verification and constant
identity.

Offsets must satisfy the greater of:

- the tensor element alignment;
- the TensorRepresentationDescriptor alignment;
- the selected target load alignment; and
- any configured mapped-I/O alignment requirement.

Page-aligned offsets are preferred for large constants when doing so improves
mapped access. The tensor TCON package owns byte-order, sub-byte packing,
floating-point encoding, and target-format element reads and writes.
Evaluators must not reinterpret side-file bytes through host C++ scalar
pointers.

Side-file publication uses temporary output plus atomic replacement. A failed
fold must not leave a valid-looking constant record that refers to incomplete
bytes. The `.B` mapped image and all referenced side payloads form one artifact
family for validation and preservation.

## Constant Identity

Tensor constant identity includes:

- canonical TensorDescriptorIR identity;
- storage-independent semantic value;
- target element representation where it affects semantics; and
- quantization and layout contracts.

ZERO, ONE, and SPLAT identities use their scalar semantic value and canonical
descriptor. Dense identity uses a content checksum plus byte length and
descriptor, with byte comparison available to resolve checksum collisions.
The side-file pathname and byte offset are locations, not semantic identity.

The TCON constant-symbol merge mechanism should be extended so equivalent
tensor TCONs can share a constant symbol. `Enter_tcon()` itself currently
special-cases only predefined floating zero values and should not be assumed
to deduplicate every scalar or tensor TCON.

## Inspection Contract

`ir_b2a -st -src` prints logical evidence such as:

```text
TCON tensor:
  id: 42
  type: tensor<i32,[2,2]>
  storage: zero

TCON tensor:
  id: 87
  type: tensor<f16,[32000,4096]>
  storage: side_file
  file: llama2.safetensors
  key: model.embed_tokens.weight
  offset: 4096
  bytes: 262144000
  alignment: 4096
  checksum: sha256:...
```

Do not print the physical `MTYPE_STR` escape token in DSL-aware output. Do not
expand large dense tensors element by element unless an explicit diagnostic
option requests it.

## Comparison With Other Compiler Formats

The design adopts useful ideas without adopting another compiler's IR:

- ONNX external tensor data distinguishes inline and external bytes and
  records relative location, offset, length, and checksum. Its documentation
  recommends mmap-friendly offsets. Open64 should provide the same operational
  information in fixed typed records rather than a free-form string map.
- MLIR `DenseElementsAttr` recognizes one-element splats, while
  `DenseResourceElementsAttr` can refer to aligned resource-backed data.
  Open64 similarly keeps ZERO, ONE, and SPLAT compact and separates them from
  inline or side-file dense storage.
- XLA `Literal` combines shape with typed array data, provides all-value
  recognition such as `IsAll(0)` and `IsAll(1)`, and warns that printing
  multi-million-element literals can be very expensive. Open64 records type
  through TensorDescriptorIR, gives ZERO and ONE canonical forms, and keeps
  normal traces compact.
- TVM Relax represents a tensor constant with runtime tensor data and its
  constant folder evaluates eligible constant expressions. Open64 retains the
  useful typed-tensor and eligibility concepts, but publishes results through
  TCON, mapped WHIRL tables, and reviewable side-file references rather than a
  runtime-only tensor object.

Primary references:

- https://onnx.ai/onnx/repo-docs/ExternalData.html
- https://mlir.llvm.org/doxygen/classmlir_1_1DenseElementsAttr.html
- https://mlir.llvm.org/doxygen/mlir-c_2BuiltinAttributes_8h_source.html
- https://github.com/openxla/xla/blob/main/xla/literal.h
- https://tvm.apache.org/docs/reference/api/python/relax/relax.html

## Boundary

The initial pipeline is:

```text
constant tensor operands
  -> descriptor and payload validation
  -> target-format tensor TCON evaluator
  -> bounded materialization decision
  -> tensor TCON and typed constant symbol
  -> common.tensor_const result value
  -> side-file publication when required
  -> gatekeeper and ir_b2a inspection
```

The evaluator belongs to native Open64 TCON and DSL infrastructure. Python is
not required after the input `.B` file has been produced. Tensor constants and
external payload references continue using the existing mapped-image and
side-file contracts.

## Simplifier Integration Contract

The expression simplifier is the producer of specialization candidates and
the consumer of successful folded results.

Input from `doc/WHIRL-DSL-SIMPLIFICATION-PLAN.md` contains:

- logical operator and version;
- constant operand value references;
- canonical opcode attributes;
- complete result TensorDescriptorIR;
- numeric-policy and effect legality decisions;
- result symbol and source position; and
- lineage and metadata transfer instructions.

The folder never calls back into `wn_simp` while evaluating tensor elements.
It returns one tensor TCON for an ordinary operation or a reviewed pair for a
projectable operation such as tensor DIVREM. The caller enters each TCON,
creates or finds its typed constant symbol, publishes the canonical constant
and projection values, and revisits affected parent expressions.

This division prevents a cycle between algebraic rewriting and payload
evaluation:

```text
simplifier classifies
  -> folder evaluates once
  -> builder publishes constant
  -> simplifier revisits parents
```

Rejected specialization leaves the original logical expression unchanged and
records a structured reason such as unsupported evaluator, unresolved shape,
numeric-policy mismatch, or materialization budget.

The M0 handoff is frozen in `osprey/common/com/dsl_tensor_fold.h`.
`DSL_TENSOR_FOLD_CANDIDATE` is the common fixed-layout input for
construction-time WN simplification, the explicit VHO DSL pass, and adapted
WOPT. It carries logical operator/version, TCON operand references,
operand/result `TY_IDX` arrays, canonical `DSL_IR_ATTRIBUTE_RECORD` rows,
materialization policy, and flags. `DSL_TENSOR_FOLD_OUTPUT` is the bounded
result record: ordinary evaluators may return at most one
`DSL_TENSOR_FOLD_RESULT_TCON` slot, while a reviewed projectable evaluator
such as DIVREM may return at most two. M0 declares `Targ_DSL_WhirlOp()` and
contract probes only; real tensor payload evaluation, tensor TCON storage,
builder publication, and mapped-image changes begin in later milestones.

The shared rejection vocabulary distinguishes unsupported evaluator,
unsupported operator, malformed candidate, effectful operator, non-constant
operand, descriptor mismatch, unresolved shape, numeric-policy mismatch,
result budget, work budget, materialization policy, division by zero, target
capability, and profitability. Target capability and profitability remain
separate gates so an NVISA-style target that cannot lower or profit from a
combined DIVREM never receives one.

## Projectable Tensor DIVREM

Open64 WOPT already combines scalar integral DIV and REM through the
projectable form:

```text
DIVPART(DIVREM(a,b))
REMPART(DIVREM(a,b))
```

Tensor constant evaluation follows the same model. A registered tensor DIVREM
evaluator computes quotient and remainder TCONs in one reviewed traversal when
both projections are needed. DIVPART selects the quotient TCON and REMPART
selects the remainder TCON. When only one projection remains, WOPT may restore
and evaluate the standalone tensor DIV or REM instead.

The paired evaluator is bounded to exactly two results. It is not a general
multiple-result convention. Both result descriptors, storage decisions, work
budgets, and side-file publications are validated as one operation. Failure
must publish neither result.

Combination is enabled only when:

- DIV and REM have identical operand values and descriptors;
- signedness, division rounding, remainder sign, divide-by-zero, overflow, and
  trap behavior are identical;
- both operations are pure and effect-free;
- quotient and remainder result descriptors are complete;
- `WOPT_Enable_DIVREM` permits the transformation; and
- the active target cost contract says combined tensor computation is cheaper
  than separate operations.

The target condition is essential. Existing Open64 NVISA configuration
disables traditional DIVREM because combination has no advantage there.
Tensor GPU or library implementations must provide their own reviewed cost
evidence rather than assuming scalar target behavior.

## Semantic Requirements

A tensor operation may be specialized only when:

1. every semantic operand and static opcode attribute is constant;
2. the logical operator and version have a registered evaluator;
3. input and result TensorDescriptorIR values are complete enough to determine
   dtype, rank, shape, layout, quantization, and result size;
4. the operator is pure and does not read or modify runtime state;
5. integer overflow, floating-point mode, rounding, NaN, infinity, signed zero,
   and quantization behavior match the operator contract;
6. symbolic or `<pending>` dimensions have been resolved;
7. evaluation is deterministic and follows target-format constant semantics;
   and
8. the materialized result satisfies configured work and storage budgets.

Do not execute arbitrary runtime libraries, Python functions, target kernels,
or unreviewed imported code during constant folding.

## Materialization Policy

Folding can reduce runtime work while increasing `.B` or side-file size.
Provide separate limits for:

- maximum result elements;
- maximum result bytes;
- maximum evaluator work;
- maximum total bytes added per PU; and
- maximum total bytes added per compilation.

Large creation operations such as `tensor_zero` and `tensor_one` remain
compact semantic constants rather than expanding into payload bytes. The
policy distinguishes ZERO, ONE, general SPLAT, inline dense, and side-file
dense representation.

## Option And Debugging Contract

Add an independent control under the existing DSL option group:

```text
-DSL:tensor_const_fold=on|off
```

Add explicit budget options only when their implementation lands. Disabling
the stage must reproduce the same pre-specialization `.B` semantics. Retain
before/after `.B` files and matching `ir_b2a -st -src` traces.

## Initial Evaluator Set

Start with exact integer, same-shape operations:

1. `common.add`
2. `common.mul`
3. reshape/view operations that do not reorder payload elements
4. compact `tensor_zero`, `tensor_one`, and splat constants of any resolved
   shape, subject to descriptor and element-count validity

Defer convolution, matmul, transcendental operations, quantized operations,
randomness, stateful operators, and target-dependent kernels until their
numeric and resource contracts are reviewed.

## Required Test Matrix

The following cases are explicit completion requirements rather than examples
hidden under a general positive/negative test item.

### Eligibility and control

1. All operands constant and a registered evaluator produces a tensor TCON.
2. Unsupported opcode/version leaves the expression unchanged.
3. Symbolic or `<pending>` shape leaves the expression unchanged.
4. Effectful or runtime-state-dependent operation is rejected.
5. Numeric-policy mismatch is rejected.
6. `-DSL:tensor_const_fold=off` preserves the pre-fold binary behavior.
7. Element, byte, evaluator-work, per-PU, and compilation budgets reject
   materialization independently without changing semantics.

### Compact constants

1. ZERO and ONE remain compact for very large resolved shapes.
2. General SPLAT stores exactly one target-format scalar TCON.
3. Quantized semantic ZERO uses the declared zero point and is not assumed to
   be an all-zero byte pattern.
4. Reshape and broadcast preserve ZERO, ONE, or SPLAT when legal.
5. Transpose preserves a uniform compact constant without dense expansion.
6. `common.add` and `common.mul` cover compact/compact, compact/inline, and
   compact/side-file operand combinations.
7. Folded ZERO, ONE, and SPLAT results are recognized by the parent
   simplifier without scanning a dense payload.

### Inline and side-file storage

1. Values below, at, and above the inline threshold select the documented
   storage kind deterministically.
2. A large LLM-style parameter remains SIDE_FILE_DENSE and can be inspected,
   hashed, and used without eager whole-file loading.
3. Side-file ranges test valid relative filename, tensor key, offset, length,
   alignment, storage format, format revision, and checksum.
4. Missing file, missing key, truncated range, offset overflow, overlapping
   invalid range, insufficient alignment, checksum mismatch, and unsafe path
   traversal are rejected.
5. Publication failure leaves no valid-looking `.B` or side-file artifact
   family.
6. Equivalent dense values at different filenames or offsets compare equal
   according to content and descriptor, not location.
7. A deliberate checksum collision in a test double falls back to byte
   comparison before constants are merged.

### Target-format correctness

1. Signed and unsigned integer width, conversion, and overflow behavior match
   scalar `Targ_WhirlOp()` semantics.
2. Floating NaN, infinity, signed zero, and rounding behavior match the active
   operator contract before floating tensor evaluators are enabled.
3. Byte order, sub-byte element packing, and target element alignment are
   tested without host pointer reinterpretation.
4. TensorDescriptorIR differences in dtype, shape, layout, quantization, or
   representation prevent incorrect constant merging.
5. Shape-compatible but layout-different dense constants test both
   layout-sensitive and explicitly layout-insensitive operations.
6. Tensor DIVREM produces quotient and remainder TCONs matching independent
   DIV and REM evaluation for positive, negative, signed-minimum, unit,
   power-of-two, and invalid zero-divisor cases.
7. Tensor DIVREM accounts for the combined work and total output bytes of both
   results before publishing either result.

### Open64 integration and compatibility

1. `sizeof(TCON)` and existing TCON field offsets remain unchanged.
2. The string-TCON escape contains a valid sized TCON character-array
   reference and older tools can preserve or inspect it without failure.
3. DSL-aware diagnostics and `ir_b2a -st -src` hide the escape and print ZERO,
   ONE, SPLAT, INLINE_DENSE, or SIDE_FILE_DENSE logically.
4. Tensor TCON envelopes survive mapped-image write and reopen through the
   existing TCON table and TCON character-array table. Query and verification
   still succeed after all runtime-derived lookup state is cleared.
5. WN construction and adapted WOPT produce equivalent tensor TCONs and typed
   constant symbols for the same request.
6. `Enter_tcon()` and constant-symbol merging distinguish semantic identity
   from storage location.
7. Large constants remain compact in ordinary traces; element-by-element
   printing occurs only under an explicit diagnostic control.
8. Retained `.B`, side payload, and `ir_b2a -st -src` artifacts provide
   reviewable evidence for every storage kind.
9. The strict version-1 `.WHIRL.dsl` image is unchanged by tensor-TCON
   storage, and its existing reader still accepts files both with and without
   tensor constants.
10. Matching tensor DIV and REM share one projectable DIVREM in WOPT when
   enabled and profitable; differing operands or descriptors remain separate.
11. A sole live DIVPART or REMPART reconstructs standalone tensor DIV or REM,
    while two live projections retain the shared operation.
12. `-WOPT:divrem=on|off` and a target-declined combination produce equivalent
    results with reviewable structural differences.

This matrix incorporates the useful comparison scenarios:

- TVM-style eligibility, unchanged-expression fallback, materialization
  policy, and parent revisiting;
- OpenXLA-style typed literal, shape/layout sensitivity, ZERO/ONE recognition,
  and bounded diagnostic printing; and
- ONNX/MLIR-style external resource, alignment, checksum, and compact splat
  storage.

## Synchronized Milestones And Pull Requests

`WHIRL-DSL-SIMPLIFICATION-PLAN.md` is authoritative for the shared milestone
definitions and pull-request exit criteria. Tensor constant folding must
advance through the same `M0-M7` sequence; the Action Queue below is not an
independent implementation order.

| Milestone | Tensor-folding scope | Required simplifier state |
| --- | --- | --- |
| M0 | Contract action items 1, 2, and 4 | S0 contracts frozen |
| M1 | Mock evaluator for handoff testing | S1 and non-folding S2 foundation |
| M2 | Action items 3-7 and storage verification | Stable M1 handoff |
| M3 | Action items 8 and 10-12 for add/mul | S3 builder queue/publication hook |
| M4 | Reuse evaluator in explicit VHO pass | S4 canonicalization and revisiting |
| M5 | Reuse identical API from adapted WOPT | Foundational S8 WOPT admission |
| M6 | Action item 9, two-result DIVREM | Projectable S2/S8 complete |
| M7 | Remaining verification and expansion | S6/S7 certification |

The clean pull-request rule is:

1. Never publish a tensor-fold producer before the corresponding simplifier,
   VHO, or WOPT consumer can reject or consume its result safely.
2. Never publish a simplifier candidate producer that requires an evaluator
   absent from the same milestone or an already merged milestone.
3. M2 may land independently because it exposes tested tensor TCON
   construction/storage APIs without changing simplifier behavior. Its
   persistent owner is the existing TCON table plus TCON character-array
   table; `.WHIRL.dsl` version 1 remains unchanged.
4. M3 is the first pull request allowed to change actual DSL expression
   construction through tensor folding.
5. M6 must land as one coherent projectable-operation feature. Do not split
   DIVREM production, two-result folding, projection accounting, target
   capability checks, and uncombining across independently usable revisions.
6. Each pull request must retain the milestone-specific `.B`, side payload,
   and `ir_b2a -st -src` artifacts identified by the authoritative plan.

PR #94 completed the M2 storage side. The coordinated main integration calls
`DSL_Tensor_TCON_Rebuild_Derived_Cache()` after the existing global TCON and
TCON character-array tables are mapped, prints logical tensor constants from
both symbol and global TCON dumps, and hides the private `MTYPE_STRING`
carrier. Its retained `tensor_tcon.B` and `tensor_tcon.T` fixture covers a
compact ZERO and an aligned SIDE_FILE_DENSE reference with filename, byte
range, and checksum.

| Milestone | Status | Merge dependency | Review artifact |
| --- | --- | --- | --- |
| M0 | Merged through PR #89/#92 | None | Contract/API test report |
| M1 | Merged through PR #93 | M0 merged | Simplifier bridge traces |
| M2 | Merged through PR #94/#95 | M1 merged | Tensor TCON `.B` and `.T` |
| M3 | Merged through PR #96 | M2 merged | Enabled/disabled fold artifacts |
| M4 | Merged through PR #97 | M3 merged | `artifacts/m4-vho-simplification/vho_simplification.{B,T}` |
| M5 | WOPT compact add/mul bridge implemented; W7 admission remains | M4 merged | Shared-evaluator bridge passed; WOPT A/B pending W7 |
| M6 | Blocked by M5 | M5 merged | DIVREM gate/projection artifacts |
| M7 | Blocked by M6 | M6 merged | Full certification matrix |

Update both copies of the status table in the same documentation change.

## Action Queue

The milestone mapping above controls when each action may begin integration.

M3 initially evaluates compact integral ZERO, ONE, and SPLAT carriers. Their
scalar TCON is already in target format and is evaluated through the existing
Open64 target constant operation. Dense inline and side-file payloads remain
valid M2 constants, but M3 must reject them unless the evaluator can obtain
each element through a reviewed target-format accessor. It must never cast
their bytes to host integer pointers.

The compact M3 slice is implemented through `Targ_WhirlOp` and
`Targ_DSL_WhirlOp`. Construction-time publication is disabled by default and
subordinate to `Enable_WN_Simp`. M4 adds a fixed-layout VHO replacement
description, compact operand identification, integer-splat creation from a
canonical TensorDescriptorIR, and the explicit VHO consumer under
`-DSL:algebraic`. The consumer retains result ST/TY/SRCPOS and mapped-image
node/value identity, publishes `common.tensor_const`, and revisits parent
definitions without recursively entering the evaluator. Items below that
mention dense payloads, side-file evaluation, WOPT, or DIVREM remain future
work.

M4 atomic publication means that a node's logical opcode, active operands,
active attributes, payload, and result kind become visible as one coherent
mapped-image relationship set. Rewriting compacts and renumbers the
relationship tables so stale rows cannot survive in table-wide consumers or
`ir_b2a -st -src`. This does not promise rollback of Open64's global,
deduplicated TCON table: evaluator-created but ultimately unused TCON entries
are legal in the same way as other unreferenced constants. Only a successfully
published node and result-symbol binding makes a folded tensor constant
semantically reachable.

1. [x] Define a fixed-layout evaluator registry keyed by logical operator and
   version.
2. [x] Define the simplifier candidate-input and folded-result handoff APIs.
3. [x] Add tensor TCON creation, query, target-format compact-scalar access, hashing,
   comparison, printing, and verification APIs.
4. [x] Define result-size and evaluator-work budgets.
5. [x] Add the fixed-layout tensor envelope in a backward-compatible
   string-TCON carrier without changing `sizeof(TCON)`. Use `TCON_IDX` as the
   stable identity and the existing TCON character-array table as persistent
   byte ownership; any runtime lookup table is derived and rebuildable.
6. [x] Implement ZERO, ONE, and general SPLAT preservation.
7. [ ] Implement INLINE_DENSE and SIDE_FILE_DENSE readers independent of
   Python and backend CG.
8. [x] Implement exact integer `common.add` and `common.mul` for compact
   ZERO, ONE, and SPLAT carriers through target-format scalar operations.
9. [ ] After the projectable-operation contract is published, implement
   exact integer tensor DIVREM evaluation and quotient/remainder TCON
   projections.
10. [x] Publish compact folded constants atomically to the mapped image.
    Dense side-file evaluation and publication remain deferred.
11. [x] Revisit affected parent expressions after successful compact
   publication.
12. [x] Preserve source position, result symbol, lineage, and descriptor
   identity.
13. [ ] Add gatekeeper checks for carrier/record consistency, folded result
   descriptor, alignment, checksum, and payload size.
14. [ ] Add old-reader fallback, mapped-image roundtrip, WN, and WOPT tests.
15. [ ] Implement every group in the Required Test Matrix and maintain a
   coverage table by evaluator, storage kind, representation, and rejection
   reason.
16. [ ] Add deterministic option A/B tests for construction, explicit VHO
   folding, and adapted WOPT.
17. [x] Preserve compact-fold `.B` and `ir_b2a -st -src` evidence. Preserve
   side payload evidence when dense side-file folding is implemented.

## Non-Goals

- Scalar constant folding already owned by `wn_simp`.
- Algebraic identities that do not evaluate a tensor payload.
- Runtime partial evaluation with unknown inputs.
- Kernel selection, autotuning, or target execution.
- Changing the binary WHIRL layout merely to record that folding occurred.
