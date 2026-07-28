# WOPT Adaptation Plan for DSL WHIRL

## Purpose

This plan defines how WOPT will admit, represent, optimize, simplify, and emit
unlowered Very High Level DSL WHIRL expressions.

The objective is to make logical DSL operators first-class WOPT expressions
without exposing the physical `OPR_DSL` escape representation to WOPT
developers. Binary WHIRL compatibility remains unchanged: WN and binary WHIRL
continue using the existing escape and mapped-image records until an explicit
versioned binary-format change is approved.

This is main Open64 middle-end infrastructure work. Python frontends continue
to produce binary WHIRL through opaque native builder APIs and do not inspect
CODEREP, WN layout, or private DSL encoding.

## Architectural Boundary

The intended pipeline is:

```text
binary WHIRL / physical OPR_DSL
  -> mapped-image reader
  -> DSL gatekeeper
  -> WN-to-CODEREP DSL import
  -> first-class logical DSL CODEREP
  -> WOPT analyses, transformations, and simplification
  -> CODEREP-to-WN DSL emission
  -> physical OPR_DSL plus mapped-image relationships
```

`OPR_DSL` remains a private compatibility mechanism at the WN and binary file
boundaries. WOPT diagnostics, traces, hashing, matching, and optimization
logic operate on logical names such as `OPR_DSLADD`.

## Current WOPT Findings

### CODEREP operator capacity

`CODEREP` is defined in `osprey/be/opt/opt_htable.h`.

- `CK_OP` stores `_opr` as `OPERATOR _opr:8`.
- `CK_IVAR` also stores `_opr` as `OPERATOR _opr:8`.
- The field therefore represents at most 256 values.
- Native `OPERATOR_LAST` is currently 148.
- Only 107 numeric values remain between 149 and 255.
- `OPCODE` also reserves eight bits for its operator component.

Allocating the remaining native values directly to DSL operators is not a
scalable first-class WOPT design. It would also entangle runtime WOPT identity
with the binary WHIRL operator namespace.

### Observed x86-64 layout

DWARF information in the current Linux x86-64 WOPT build reports:

| Entity | Observed size |
| --- | ---: |
| `CODEREP` | 88 bytes |
| `STMTREP` | 112 bytes |
| `CK_OP` subrecord | 48 bytes |
| `CK_IVAR` subrecord | 40 bytes |
| `CK_OP::_opr` | 8 bits |
| `CK_IVAR::_opr` | 8 bits |

The `CK_OP` subrecord has alignment space before its first pointer. A wider
WOPT-local operator field may fit without increasing the 88-byte CODEREP on
this ABI. This is an observation, not a portable guarantee. Any layout change
must be compiled and measured for every supported target.

The historical disabled 48-byte `CODEREP` and 60/64-byte `STMTREP` assertions
in `opt_main.cxx` have been corrected for the measured x86-64 LP64 layout.
Active runtime assertions and compile-time guards in `opt_htable.cxx` require
`sizeof(CODEREP) == 88` and `sizeof(STMTREP) == 112`. The two records must be
reviewed together because DSL value expressions enter WOPT as CODEREPs while
DSL assignments, including `STID`, enter as STMTREPs.

An intentional expansion of either record must update its guards in the same
reviewed change after documenting the new field, measured target layouts,
memory-cost impact, stack/pool allocation behavior, and required regression
results. Other target layouts remain measurements to add, not assumptions
copied from x86-64.

### Existing stack and pool protocol

WOPT already follows the established temporary-node protocol:

- `Alloc_stack_cr` allocates temporary CODEREP views on the stack.
- `CR_Create` and related helpers initialize temporary expressions.
- `CODEMAP::Hash_Op` canonicalizes and finds or creates the retained CODEREP.
- `CXX_NEW_VARIANT` allocates retained CODEREPs and extra kid storage in the
  WOPT memory pool.
- Temporary stack inputs are discarded without memory-pool deletion.

DSL adaptation must preserve this protocol.

### Existing simplifier integration

WOPT already reuses the traditional Open64 simplifier:

- `opt_fold.h` defines `simpnode` as `CODEREP *`.
- It provides CODEREP implementations of the `SIMPNODE_*` accessors and
  constructors.
- `opt_fold.cxx` includes `wn_simp_code.h`.
- Simplifier-created expressions are sent through CODEREP hashing.

The authoritative WOPT controls are:

| Option | State | Purpose |
| --- | --- | --- |
| `-WOPT:cr_simp=on|off` | `WOPT_Enable_CRSIMP` | Enable CODEREP simplification |
| `-WOPT:fold2const=on|off` | `WOPT_Enable_Fold2const` | Invoke folding and simplification entry points |

`-OPT:wn_simp=on|off` controls construction-time WN simplification. It is not
the WOPT CODEREP simplifier switch.

DSL WOPT support must continue using the CODEREP instantiation of
`wn_simp_code.h`. It must not create an independent WOPT algebraic engine.

### Existing projectable-operation integration

WOPT already has a reusable optimization model for related multiple results:

- `Combine_Operations()` changes integral `DIV(a,b)` into
  `DIVPART(DIVREM(a,b))` and `REM(a,b)` into
  `REMPART(DIVREM(a,b))`.
- CODEREP hashing, CSE, and PRE can share the common `DIVREM(a,b)`.
- `opt_project.h` defines projectable-operation and projection queries.
- DCE counts live projection uses of projectable EPRE temporaries.
- CODEREP-to-WN emission calls `Uncombine_Operations()` so a sole surviving
  projection becomes an ordinary DIV or REM.
- `-WOPT:divrem` controls the transformation through
  `WOPT_Enable_DIVREM`.

Logical tensor DIV and REM must extend this framework instead of introducing a
separate WOPT reconstruction peephole. Target profitability remains part of
the contract. Traditional NVISA configuration disables DIVREM because it sees
no advantage. Its virtual ISA models separate DIV and REM operations, and its
`Expand_DivRem()` path is intentionally not implemented. The disable therefore
also prevents WOPT from sending an unsupported combined operation to CG.
Tensor targets must independently prove a combined lowering capability and
profitability before enabling this transformation. If either gate fails,
WOPT must retain standalone DIV and REM.

## Unsafe Current Admission

A physical `OPR_DSL` must not enter WOPT as an ordinary generic `CK_OP`
without logical decoding.

Current generic behavior is insufficient:

- `CODEMAP::Hash_op_and_canon` hashes `cr->Op()` and kid CODEREP IDs.
- `CODEREP::Match` compares `Op()`, kid count, kid pointers, and selected
  native-operator fields.
- `CODEREP::Print_node` obtains its name through `OPCODE_name`.
- `wn_simp_ftable.h` has a null simplifier entry for `OPR_DSL`.

The physical opcode does not carry logical DSL operator identity, version,
canonical attributes, TensorDescriptorIR identity, or the complete effect
contract. If these facts are omitted, two semantically different DSL
expressions with the same kids could be incorrectly treated as the same value.

The current compiler must therefore continue lowering DSL expressions before
ordinary WOPT admission until the import, representation, hash, equality, and
emission work in this plan is complete.

## Required WOPT Representation

### Logical operator identity

Introduce a WOPT-local logical operator representation for `CK_OP`.

Requirements:

1. Represent every existing native `OPERATOR`.
2. Represent the reviewed DSL operator namespace without consuming the
   remaining native operator values.
3. Distinguish native and DSL identities without manually decoding
   `OPR_DSL`.
4. Provide guarded APIs for native operator, DSL operator, name, category,
   version, and properties.
5. Reject narrowing conversions to the existing 8-bit native operator field.

A 16-bit or 32-bit field can satisfy the current identity requirement. The
implementation decision must account for namespace growth and layout results.
A 32-bit WOPT-local identity is preferred unless target layout measurements
show a material cost.

### Version and semantic information

Operator identity alone is not semantically complete. Each DSL CODEREP needs:

- logical DSL operator;
- operator contract version;
- canonical opcode attributes;
- result TensorDescriptorIR identity;
- relevant operand TensorDescriptorIR identities or verified references;
- effect and ownership contract;
- source and transformation lineage references where required for emission;
- mapped-image reconstruction reference.

Use a WOPT-owned fixed-layout semantic-info table indexed from CODEREP rather
than embedding STL containers in CODEREP. The table is runtime middle-end
state; it is reconstructed from the binary DSL IR image on import and used to
rebuild mapped-image relationships on emission.

### Hash and equality

For a pure DSL expression, value identity must include:

```text
logical operator
operator version
canonical opcode attributes
result TensorDescriptorIR identity
operand CODEREP identities
effect-relevant semantic identity
```

Source position, diagnostics, profiling data, and nonsemantic lineage do not
participate in value equivalence. They must still be preserved or merged when
WOPT reuses an existing CODEREP.

Effectful DSL operators must not enter ordinary expression CSE merely because
their semantic keys and kids match.

### Printing

CODEREP and WOPT trace printers must:

- print logical names such as `OPR_DSLADD`;
- print operator version when needed to distinguish contracts;
- expose canonical semantic attributes and result descriptor identity;
- never print the private physical `OPR_DSL` escape or image-record index as
  the operator identity.

## WN-to-CODEREP Import

Add an explicit DSL branch before generic expression handling in
`CODEMAP::Add_expr`.

The importer must:

1. recognize a native DSL WN through public DSL WN APIs;
2. obtain the logical opcode and version;
3. load and validate the associated DSL image record;
4. resolve the result TensorDescriptorIR and attributes;
5. verify that the gatekeeper has accepted the operator;
6. recursively import operand values;
7. create a temporary first-class DSL CODEREP;
8. hash it using DSL semantic identity; and
9. retain source and reconstruction information.

The importer must not teach generic WOPT code how to decode WN escape fields.

## WOPT Simplifier Path

### Preparation

For each logical DSL operator, query the algebraic registry and tensor
legality engine.

Only when semantics are equivalent may preparation project a DSL CODEREP to a
temporary native CODEREP view. For example:

```text
pure common.add.v1
integer dtype
identical logical shapes
no broadcasting
compatible representation
no runtime state or effects
  -> temporary native OPR_ADD CODEREP view
```

The projection is stack-local and is never published as the permanent
identity of the DSL expression.

### Existing engine

Run the existing CODEREP instantiation of `wn_simp_code.h`.

The engine continues to provide applicable canonicalization, constant folding,
identity, cancellation, reassociation, and factorization rules under existing
Open64 numeric and WOPT option controls.

### Postprocessing

Classify the simplifier result:

- no replacement;
- reuse of an existing operand or value;
- folded constant;
- rebuilt expression; or
- rejected replacement.

For a rebuilt DSL expression, restore logical operator identity, version,
attributes, TensorDescriptorIR, effects, and source relationships before
hashing the retained CODEREP.

A returned native expression may remain native only when the transformation
semantically lowers the DSL operator at an approved pipeline point. Algebraic
simplification by itself must not accidentally lower domain-visible DSL
semantics.

## Other WOPT Services

First-class operator identity is necessary but not sufficient. Review these
WOPT services before enabling DSL admission:

- expression hashing and CSE;
- copy propagation;
- value numbering;
- SSA PRE and SSAPRE;
- dead-code elimination;
- use counts and recursive deletion;
- alias, MU, CHI, and effect handling;
- loop and induction-variable recognition;
- type conversion and actual-data-size queries;
- profitability and expression-cost models;
- CODEREP copying and rehashing;
- debug and trace printing;
- CODEREP-to-WN emission.

Each service must either understand the reviewed DSL contract or conservatively
preserve the expression without transforming it.

## CODEREP-to-WN Emission

Intercept first-class DSL CODEREPs before generic `cr->Op()` emission.

The emitter must:

1. recover logical operator, version, operands, and semantic-info record;
2. create the native DSL WN through public DSL WN APIs;
3. restore attributes, result descriptor, effects, source position, and
   lineage;
4. recreate mapped-image DSL relationships;
5. preserve retained high-level comment projection where required;
6. emit logical DSL evidence in traces; and
7. run the DSL gatekeeper on the emitted tree in validation builds.

No new binary WHIRL operator allocation is required for this WOPT adaptation.

## Option and Debugging Contract

Preserve independent A/B controls:

1. `-OPT:wn_simp=off` disables construction-time WN simplification.
2. `-WOPT:cr_simp=off` disables CODEREP simplification.
3. `-WOPT:fold2const=off` disables the WOPT folding entry path.
4. DSL-specific WOPT controls may further disable a staged transformation but
   must not override these master controls.

Certification must retain separate artifact families and run:

```text
ir_b2a -st -src input.B input.T
```

The comparison matrix must distinguish:

- construction simplification on/off;
- VHO DSL simplification on/off;
- WOPT CODEREP simplification on/off; and
- WOPT folding on/off.

## Compatibility Requirements

1. Do not change WN layout or the binary 8-bit operator field.
2. Do not allocate the remaining native operator slots as the DSL namespace.
3. Preserve mapped-image and ELF section contracts.
4. Preserve scalar WOPT behavior when no DSL CODEREP is present.
5. Preserve existing WOPT option behavior and defaults.
6. Preserve logical DSL operator names, versions, attributes, descriptors, and
   effects across WN-to-CODEREP-to-WN conversion.
7. Unknown or unsupported DSL contracts must be diagnosed or preserved
   conservatively, never silently reinterpreted.
8. WOPT runtime structures may change without a binary WHIRL version bump, but
   their size and allocation effects must be measured and tested.

## Initial Vertical Slice

Use three operators to stage the integration:

1. `common.tensor_const.v1`
2. `common.add.v1`
3. `common.mul.v1`

The first optimization cases are:

```text
x + tensor_zero -> x
tensor_zero + x -> x
x * tensor_one  -> x
tensor_one * x  -> x
constant + constant -> folded tensor constant
```

Apply them initially only to pure, exact integer tensors with identical
descriptors and no broadcasting, quantization, sharding, placement mismatch,
or runtime state.

## Staged Action Queue

### M5 Progress

M5 is active on `codex/dsl-wopt-m5` after PR #97 merged. The W0/W1
batch selected a 32-bit WOPT-local semantic-info index and placed it in the
existing x86-64 `CK_OP` alignment space. Linux DWARF still reports
`sizeof(CODEREP) == 88`. The fixed-layout semantic record is runtime-only and
interns logical operator, version, canonical attributes, result descriptor,
operand descriptor identity, effects, and algebraic flags. Initialization,
copy, hash, equality, and logical printing now preserve that identity.

W3-W6 now form one guarded vertical slice for pure `common.tensor_const`,
`common.add`, and `common.mul`. `CODEMAP::Add_expr` resolves the result
symbol, mapped-image node, logical opcode/version, canonical attributes,
operand descriptor identity, result descriptor, effect identity, and compact
tensor TCON before the first hash operation. Developer dumps expose the
logical operator and semantic hashes without exposing physical `OPR_DSL`.
Eligible constant add/mul expressions call
`DSL_Tensor_Fold_Describe_Replacement`, the same evaluator/publication
service used by WN and VHO folding. The emitter intercepts logical DSL
CODEREPs before generic `WN_CreateExp*`, rebuilds them through the public DSL
WN API, rewrites the mapped-image node when a fold changes the operator, and
reruns the PU gatekeeper.

The native `dsl_wopt_bridge_test` constructs real builder values and proves
WN semantic import, shared tensor evaluation, folded tensor-constant
emission, mapped-image rewrite, and gatekeeper acceptance. The lightweight
semantic-info test separately proves operator/version/attribute/descriptor/
TCON identity and proves that reconstruction provenance does not inhibit
value numbering.

Broad optimizer admission is intentionally deferred to W7. The normal backend
still invokes `VHO_DSL_Lower_Driver` before WOPT, so driver reordering and
full `-WOPT:cr_simp` / `-WOPT:fold2const` binary A/B certification must wait
until CSE, copy propagation, PRE, DCE, type queries, effects, and profitability
have conservative DSL policies.

The same batch covers the non-behavioral W2 mechanics. `Init_op()` clears the
semantic index, `CODEREP::Copy()` preserves it, and existing stack allocation,
variant allocation, extra-kid storage, use counts, and rehashing continue to
operate on the unchanged CODEREP size. Guarded APIs distinguish logical DSL
identity from native `OPERATOR`; requesting a native operator from a logical
DSL CODEREP is an assertion failure. Hashing and equality include the interned
semantic identity, while native CODEREPs retain zero as the index and follow
their previous paths.

### W0: Baseline

- Capture current scalar WOPT simplifier tests and traces.
- Add a test proving unadapted physical `OPR_DSL` is rejected before generic
  WOPT hashing.
- Record target-specific CODEREP sizes and offsets.

### W1: Representation prototype

- Define the WOPT-local logical operator type.
- Define the fixed-layout DSL CODEREP semantic-info record and table.
- Prototype 16-bit and 32-bit layouts.
- Measure CODEREP size on supported targets.
- Add compile-time field-width, size, and offset guards.

### W2: CODEREP mechanics

- Update initialization, copy, stack allocation, variant allocation, extra-kid
  handling, use counts, and rehashing.
- Add guarded logical/native operator APIs.
- Keep existing native CODEREP behavior unchanged.

### W3: Import

- Decode DSL WNs before generic `CODEMAP::Add_expr` handling.
- Resolve semantic-info records and tensor descriptors.
- Add hash and equality support.
- Add negative tests for differing operator, version, attribute, descriptor,
  and effect identity.

Status: implemented for the M5 pure integer tensor vertical slice.

### W4: Printing and inspection

- Print logical DSL operators in CODEREP dumps and WOPT traces.
- Add stable version, attribute, and descriptor evidence.
- Prohibit physical escape details in developer-visible output.

Status: implemented for logical name/version, result type, canonical
attribute identity, operand descriptor identity, and effect identity.

### W5: Simplifier bridge

- Add stack-local DSL-to-native CODEREP preparation.
- Route eligible rules through `wn_simp_code.h`.
- Add DSL result postprocessing and rehashing.
- Honor `WOPT_Enable_CRSIMP` and `WOPT_Enable_Fold2const`.

Status: the compact tensor constant extension calls the common M3 evaluator
and honors both controls. Traditional scalar rules continue through the
existing CODEREP instantiation of `wn_simp_code.h`; broader DSL preparation
for traditional rules remains staged with W7 service review.

### W6: Emission

- Rebuild native DSL WNs through public APIs.
- Restore mapped-image records, source positions, and comment projection.
- Re-run the gatekeeper.
- Verify binary WHIRL roundtrip behavior.

Status: native reconstruction, mapped-image rewrite, comment projection
through the logical printer, source-position preservation on the owning
statement, and post-emission gatekeeper verification are implemented.
Process-boundary binary A/B certification remains paired with W7 driver
admission.

### W7: WOPT service audit

- Audit CSE, copy propagation, value numbering, PRE, DCE, effects, type
  queries, and profitability.
- Enable only services with reviewed DSL legality.
- Make unsupported services conservatively preserve DSL expressions.
- Extend context-sensitive control-flow simplification to logical DSL
  expressions after WN-to-CODEREP import. Reuse CFG dominance, value ranges,
  symbolic tensor-dimension facts, and branch constraints so a condition
  proven by an enclosing region or branch is removed by WOPT rather than by
  construction-time simplification.
- Add positive and negative tests for nested conditions, loop/range facts,
  symbolic shape bounds, unknown bounds, and effectful conditions.
- Extend redundant store/result-update elimination to DSL tensor values only
  after ordinary expression simplification. Require identical addressed
  value, TensorDescriptorIR compatibility, unique ownership or proven
  no-alias state, and an effect model proving that removing the update is
  observable-state neutral.
- Cover both direct self-assignment and cases exposed by simplification, such
  as a tensor value updated with itself plus a folded zero.
- Preserve MU, CHI, alias-class, source-position, and diagnostic contracts
  when an update cannot be removed.
- Generalize projectable-operation and projection queries to reviewed logical
  DSL contracts without exposing `OPR_DSL`.
- Combine matching tensor DIV and REM into logical DIVPART/REMPART projections
  of one tensor DIVREM under `WOPT_Enable_DIVREM`.
- Include logical operator/version, both result descriptors, attributes, and
  operand identities in projectable CODEREP hashing and equality.
- Extend DCE projection-use accounting and emission-time uncombining so one
  surviving tensor projection becomes standalone DIV or REM.
- Add enabled, disabled, one-projection, two-projection, mismatched-operand,
  descriptor-rejection, effect-rejection, and target-unprofitable tests.

### W8: Certification

- Run scalar WOPT regression tests.
- Run common/com and VHO DSL tests.
- Run warning and no-tab checks.
- Preserve `.B`, `.T`, phase traces, and diagnostics.
- Compare the full simplifier option matrix.
- Verify `ir_b2a -st -src` before and after WOPT.

## Exit Criteria

WOPT DSL admission is ready when:

1. unlowered DSL WNs become first-class logical CODEREPs;
2. hash and equality include complete semantic identity;
3. WOPT traces never expose the physical escape as the logical operator;
4. eligible DSL algebra uses the existing CODEREP simplifier;
5. option-controlled A/B comparisons are reproducible;
6. CODEREP-to-WN emission preserves all reviewed DSL semantics;
7. mapped-image binary compatibility remains unchanged;
8. scalar WOPT tests remain unchanged; and
9. retained `ir_b2a -st -src` artifacts demonstrate the complete path.

## Related Documents

- `doc/WHIRL-DSL-SIMPLIFICATION-PLAN.md`
- `doc/WHIRL-DSL-INFRASTRUCTURE.md`
- `doc/WHIRL-DSL-TENSOR-TYPE-HANDLING.md`
- `doc/VHO-DSL-OPTIMIZATION-PLAN.md`
- `doc/Open64_Domain_Specific_Compiler_IR_Design.md`
- `WHIRL.pdf`
