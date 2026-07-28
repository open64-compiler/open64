# WHIRL DSL Expression Simplification Plan

## Purpose

This document plans expression simplification for native DSL WHIRL at two
integration points:

1. simplification while a DSL expression is created; and
2. an explicit simplification pass over an existing Very High Level WHIRL
   tree.

This is shared IR infrastructure work. It belongs in `osprey/common/com` and
the VHO DSL optimization framework, independent of any particular Python
frontend or model family. Frontends such as `torch2whirl` remain consumers of
opaque builder APIs and do not participate in WHIRL simplification or inspect
the physical `OPR_DSL` representation.

The first objective is semantic correctness and a cohesive extension of the
existing Open64 simplifier. Profitability tuning and large domain-specific
rewrites come later.

## Existing Open64 Baseline

Open64 already provides two useful simplification modes:

- `WN_SimplifyExp1`, `WN_SimplifyExp2`, and `WN_SimplifyExp3` simplify WHIRL
  expressions as they are constructed.
- `WN_Simplify_Tree` performs a bottom-up simplification of an existing tree,
  while `WN_Simplify_Rebuild_Expr_Tree` rebuilds an expression whose children
  have already been simplified.

The implementation is primarily in:

- `osprey/common/com/wn_simp.h`
- `osprey/common/com/wn_simp.cxx`
- `osprey/common/com/wn_simp_code.h`
- `osprey/common/com/opcode.cxx`

The existing controls include `Enable_WN_Simp`, `Enable_Cfold_Aggressive`,
`Enable_Cfold_Reassociate`, floating-point roundoff controls, reciprocal and
square-root controls, and target-independent construction controls.

`Enable_WN_Simp` is enabled by default. The `OPT` option group exposes it as
`wn_simplify`, with `wn_simp` as the accepted abbreviation. This control is
not merely a tuning option: it is the master diagnostic switch for
construction-time WHIRL simplification. A developer must be able to compile
the same input with, for example, `-OPT:wn_simp=on` and
`-OPT:wn_simp=off`, retain both `.B` files, and compare matching
`ir_b2a -st -src` traces. The exact driver spelling should continue to follow
the existing Open64 option parser rather than introducing a DSL-only command
line convention.

### Operand-swap canonicalization

`OPCODE_commutative_op()` must not be interpreted as a complete algebraic
property query. Its practical role is to return the equivalent opcode after
swapping operands:

- genuinely commutative operators return the same opcode; and
- `GT`, `GE`, `LT`, and `LE` return the corresponding reversed relation.

For example, swapping the operands of `GT(a, b)` requires `LT(b, a)`. The
function name is therefore broader and more misleading than its actual
canonicalization role. DSL work must not use this function as the registry of
all commutativity, reassociation, or factorization properties.

### Existing factorization

Open64 already implements generalized factorization through `simp_factor()`
and `simp_factor_idty()` in `wn_simp_code.h`. It recognizes the common factor
in all four operand positions using `FACTOR_11`, `FACTOR_12`, `FACTOR_21`, and
`FACTOR_22`. Representative transformations include:

```text
z*x + z*y  -> z*(x+y)
x*z + y*z  -> (x+y)*z
z*x - z*y  -> z*(x-y)
z*x + z    -> z*(x+1)
```

The same framework is also used for selected bitwise and logical identities.
Arithmetic factorization requires aggressive simplification. Floating-point
factorization is rejected unless reassociation is enabled.

Factorization is a relationship between an outer operator and an inner
operator. It is not adequately represented by a single `factorable` bit on
either operator.

### TVM rule audit against `wn_simp`

A source-level comparison with Apache TVM confirms that the first two
representative arithmetic families are already traditional Open64 rules:

1. cancellation such as `x + (y - x) -> y`; and
2. factorization such as `x*y + x*10 -> (y+10)*x`.

`simp_add_sub()` handles the first family through three- and four-operand
reassociation followed by tree comparison and cancellation. It is enabled for
integer types and requires `Enable_Cfold_Reassociate` for floating-point
types. The cancellation search also requires `Enable_Cfold_Aggressive`.

`simp_factor()` and `simp_factor_idty()` handle the second family across all
four common-factor operand positions. They require
`Enable_Cfold_Aggressive`, and floating-point factorization additionally
requires `Enable_Cfold_Reassociate`.

`Enable_Cfold_Aggressive` is normally enabled by Open64 configuration unless
the user overrides `-OPT:fold_aggressive`. Consequently, the DSL
implementation must not duplicate these rules. If either rule fails for an
eligible DSL integer expression, investigate the logical-DSL preparation,
traditional opcode projection, option state, tree comparison, or
TensorDescriptorIR legality checks.

The TVM div/mod reconstruction family requires a more precise Open64
comparison. The traditional WN simplifier handles individual `DIV`, `MOD`,
and `REM` identities and power-of-two cases, but it does not directly
reconstruct:

```text
(x / c) * c + (x % c) -> x
x - (x / c) * c       -> x % c
```

Open64 WOPT already has a stronger infrastructure for related DIV and REM
computations. Under `WOPT_Enable_DIVREM`, `Combine_Operations()` transforms:

```text
DIV(a,b) -> DIVPART(DIVREM(a,b))
REM(a,b) -> REMPART(DIVREM(a,b))
```

The common `DIVREM(a,b)` CODEREP can then be shared by hashing, CSE, PRE, and
other WOPT services. `opt_project.h` defines this as a projectable operation:
the combined result is cheaper than separate DIV and REM computations, while
each projection is cheap. DCE counts live projections. During emission,
`Uncombine_Operations()` restores an ordinary DIV or REM when only one
projection survives.

Tensor DIV and REM must first extend this established projectable-operation
model. Do not introduce the TVM reconstruction identities as the first
independent DSL peephole. A later identity proposal may operate on explicit
tensor DIVREM projections only after its separate legality and profitability
case is proven.

## Architectural Decisions

### Preserve the two simplification points

Construction-time simplification and the explicit pass use the same semantic
rule engine and legality checks. They differ only in integration:

- the builder asks for simplification before publishing a newly created DSL
  value; and
- the pass walks existing DSL expression trees in a defined pipeline stage.

The explicit pass remains necessary because a useful pattern may appear only
after propagation, inlining, descriptor refinement, or another transformation.

### Reuse the traditional Open64 simplification engine

The traditional Open64 expression simplifier is the normative algebraic rule
engine for DSL operators whenever the traditional rule has equivalent
semantics. DSL expression simplification must extend that engine through its
intended abstraction boundary, not create an independent collection of
duplicate algebraic rewrites.

`wn_simp_code.h` can be instantiated for other compiler representations, as
WOPT does for `CODEREP`. That flexibility does not justify a new parallel DSL
expression representation. The established construction protocol for a WHIRL
producer is the preferred DSL integration:

1. construct a temporary physical WN view as a local variable;
2. initialize it with the traditional opcode, type, and operand information
   needed by the simplifier;
3. pass that physical view through the existing WN simplifier entry point;
4. discard the stack-local input without deletion or memory-pool ownership
   concerns; and
5. retain only WNs returned by the simplifier, which are allocated in the
   normal WN memory-pool space used by mapped-image finalization.

This protocol was part of the original Open64 development practice even though
its full internal documentation is not present in the open-source tree. The
current source preserves the surrounding contract: `WN_CreateExp1/2/3` call
`WN_SimplifyExp1/2/3`, use the original operands when no replacement is
returned, and repair required mappings on a successful replacement.

The required flow is:

1. **Preparation:** decode the logical DSL operator through public APIs,
   validate version, purity, TensorDescriptorIR compatibility, ownership,
   effects, broadcasting, and numeric policy; map it to an equivalent
   traditional opcode and initialize the temporary physical WN view.
2. **Traditional simplification:** let the existing Open64 rule engine perform
   canonicalization, constant folding, identity, cancellation, reassociation,
   factorization, and other applicable transformations under
   `Enable_WN_Simp` and its established controls.
3. **Postprocessing:** translate the simplifier result back into logical DSL
   values while preserving or rebuilding result symbols, source positions,
   descriptors, attributes, lineage, compiler metadata, value references, and
   mapped-image table relationships. Never publish a pointer to the temporary
   stack-local WN.

The DSL algebraic registry supplies logical-operator mapping, semantic
eligibility, and tensor-specific legality. It must not become a competing
general algebraic rule engine. A DSL-only rule is appropriate only when no
traditional Open64 rule has equivalent semantics; the reason and legality
contract must then be documented and tested.

Calling `WN_SimplifyExp1/2/3` directly on physical `OPR_DSL` nodes is not
sufficient because the escape operator and `MTYPE_M` do not identify the
logical DSL operation or its tensor semantics. Preparation must instead
project an eligible logical DSL expression into a temporary traditional WN
view. Postprocessing then restores complete DSL value state around the
pool-backed result.

### Operate on logical DSL operators

All simplifier APIs, diagnostics, traces, and tests use logical operators such
as `OPR_DSLADD` and `OPR_DSLMUL`. Simplifier code must not manually inspect or
decode the private physical `OPR_DSL` escape tag.

The logical operator registry remains authoritative for operator identity,
version, operand count, result rules, effect model, and algebraic contracts.

### Separate three kinds of algebraic information

1. **Single-operator properties** describe an operator, such as purity,
   operand-swap equivalence, identity value, or reassociation permission.
2. **Paired-operator relations** describe distributivity and factorization,
   such as multiplication under addition.
3. **Compilation policy** decides whether a valid transformation is enabled
   under the current floating-point, optimization-level, and profitability
   controls.

The registry states what may be legal. A compilation option may disable a
legal transformation, but must not make an illegal transformation legal.

### Canonicalize before pattern simplification

Expression canonicalization is part of the first DSL implementation. It places
equivalent logical expressions into a stable form so constant folding,
identity, cancellation, reassociation, factorization, CSE, and later pattern
matching do not need to recognize every operand permutation.

Canonicalization follows this order:

1. Canonicalize and simplify operands bottom-up.
2. Fold the expression immediately when every operand is constant.
3. Move a single constant to `kid1` when the logical operator has a legal
   swap-equivalent form.
4. When operands are swapped, replace ordered relations with their logical
   inverse-direction operator, such as `GT(a, b)` becoming `LT(b, a)`.
5. Place nonconstant operands of genuinely commutative operators into a
   deterministic structural order.
6. Reassociate nested expressions to expose adjacent constants only when the
   operator, dtype, and active numeric policy permit reassociation.
7. Apply identity, cancellation, distribution, and factorization rules to the
   resulting canonical form.

The deterministic structural order must be derived from stable logical IR
content, including logical operator identity and version, canonical attributes,
TensorDescriptorIR identity, constants, symbols, and recursively ordered
operands. It must not depend on pointer values, allocation order, unstable
table indices, or private `OPR_DSL` record indices. Source positions, lineage,
and compiler metadata are preserved but do not determine algebraic ordering.

Canonicalization is not permission to commute every binary operator.
`OPCODE_commutative_op()` remains a traditional WHIRL operand-swap helper, not
the DSL algebraic-property registry. DSL canonicalization must use the reviewed
swap-equivalent and reassociation contracts, and it must reject reordering when
effects, ownership, strict floating-point semantics, broadcasting, or tensor
representation make the transformation unsafe.

### Separate construction simplification from statement-level canonicalization

Construction first invokes the regular Open64 simplifier under
`-OPT:wn_simp`. After a complete DSL statement has been constructed, the
fixed-position VHO DSL canonicalization stage may make a second call over its
expression tree under `-DSL:canon`.

The second call is deliberately a postprocess. It may collect repeated integer
tensor terms and coefficients, for example:

```text
x + x                 -> 2 * x
x + 2 + 3 + 4 + x     -> 2 * x + 9
```

This is not a generally profitable scalar CPU canonical form: `x+x` may be
cheaper than materializing or executing multiplication by two. The
statement-level DSL form is useful only when tensor-operator semantics,
subsequent matching, or a reviewed cost model justifies it. Keep it out of the
traditional construction-time rule set. `-DSL:canon=off` must preserve the
post-construction baseline independently of `-OPT:wn_simp`.

The statement-level call still follows prepare, existing-engine, and
postprocess discipline. Constant folding already performed by `wn_simp` is
retained; the postprocess adds DSL tensor canonical form rather than replacing
traditional constant folding.

### Projectable tensor DIV and REM

Tensor DIV and REM must be represented as projections of one logical
projectable operation when both results can profitably share computation:

```text
common.div(a,b) -> common.divpart(common.divrem(a,b))
common.rem(a,b) -> common.rempart(common.divrem(a,b))
```

The names above describe the logical common-substrate roles; final operator
names and versions must be published through the normal opcode contract
review. The physical `OPR_DSL` escape remains private.

The tensor projectable contract must define:

- combined logical operator/version and its two projection
  operator/versions;
- quotient and remainder TensorDescriptorIR result rules;
- identical operand and signed-division conventions;
- divide-by-zero, signed-minimum divided by `-1`, overflow, and trap behavior;
- purity, effects, ownership, layout, sharding, placement, quantization, and
  runtime-state requirements;
- a target lowering capability that can consume the combined operation and
  both projections without reaching an unimplemented CG path;
- target cost evidence that combined computation is cheaper than two separate
  computations;
- projection use counting and dead-projection behavior; and
- separate-operation reconstruction when only one projection remains.

WOPT adaptation should generalize its existing projectable-operation queries
to logical DSL operators. It must continue honoring `-WOPT:divrem`. A target
may disable combination when it has no advantage, as the current NVISA target
does for traditional DIVREM.

NVISA is also a warning against treating this option as a cost-only switch.
Its target configuration disables `WOPT_Enable_DIVREM`, its virtual ISA
provides separate DIV and REM operations, and its `Expand_DivRem()` path is
intentionally not implemented. Allowing WOPT to create DIVREM for that target
could therefore reach a CG assertion rather than merely produce slower code.
Before enabling tensor DIVREM for any target, prove both of these independent
gates:

1. **Lowering capability:** CG, a runtime, or a selected kernel/library
   contract can consume the combined result and its projections correctly.
2. **Profitability:** producing both results is cheaper than independent DIV
   and REM after accounting for projection work, register pressure, output
   storage, and kernel or library launch costs.

If either gate fails, retain standalone tensor DIV and REM. Do not debug the
resulting downstream failure by weakening CG; first verify that the
projectable transformation was incorrectly admitted for the active target.

This is not an arbitrary multiple-result operator convention.
`common.divrem` is a specifically reviewed two-component projectable value,
modeled on the existing WHIRL exception. Only `common.divpart` and
`common.rempart` produce ordinary tensor values consumed by other
expressions.

### Preserve strict floating-point behavior

Integer algebra and strict floating-point algebra are not interchangeable.
Each rule must state its numeric safety class. At minimum the implementation
must distinguish:

- always exact;
- valid for integer semantics;
- valid when floating-point reassociation is enabled; and
- valid only under a separately reviewed relaxed-math condition.

Existing Open64 controls remain the baseline. DSL controls may refine those
rules for tensor dtypes but must not silently weaken them.

### Preserve complete tensor semantics

A replacement is legal only when the result's semantic tensor state remains
equivalent. The check must use structured descriptor fields, not an unordered
metadata key/value scan.

The relevant state consists of:

- `TensorTypeCore`: tensor kind, semantic dtype, rank, and logical shape;
- `TensorTraitSet`: semantic role and domain traits; and
- `TensorRepresentationDescriptor`: layout, strides, sharding, placement,
  memory properties, alignment, quantization, and runtime representation
  state.

`element_ty` and `dtype` are related but distinct. `element_ty` is the Open64
primitive storage/computation type represented through `TY_IDX`/`MTYPE`.
`dtype` is the DSL semantic element type and may include quantization or other
interpretation not expressible by a primitive `MTYPE`. Their mapping must be
compatible for a rewrite, but one must not be silently substituted for the
other.

`TensorLineageMetadata` and compiler metadata do not participate in tensor
type equivalence. They must nevertheless be transferred, merged, or updated
when a replacement is published so provenance, source context, diagnostics,
pass ownership, and profiling evidence are not lost.

### Respect effects and ownership

Initial algebraic simplification applies only to pure operators. A rule must
not remove, duplicate, or reorder an operator with runtime effects, state
updates, barriers, CHI-like effects, volatile behavior, or an unresolved
operand ownership contract.

Unique ownership and no-alias facts can improve legality, but they do not by
themselves authorize reassociation or duplication of an effectful operation.

### Preserve result identity at construction time

Classic scalar construction-time simplification can return an existing kid.
The DSL builder also owns named result symbols, source positions, tensor
descriptors, lineage, and value records. Therefore, returning a kid handle
directly is not always sufficient.

The simplifier result must distinguish:

- no replacement;
- reuse of an existing semantic value;
- creation of a folded constant;
- creation of a rebuilt logical expression; and
- rejection because the requested result identity cannot be preserved.

The builder then materializes or aliases the replacement through a reviewed
value-record mechanism while preserving the requested result symbol and its
source information. This protocol must not expose WN layout to the frontend.

## Proposed Infrastructure

### Algebraic contract registry

Add a fixed-layout, table-friendly algebraic contract associated with logical
DSL opcode identity and version. Do not use STL containers in mapped-image
records.

The contract should be able to express:

```text
operator property:
  operator/version
  operand-swap result operator
  identity kind
  reassociation safety class
  purity requirement

paired relation:
  outer operator/version
  inner operator/version
  relation kind: factor or distribute
  allowed operand-position mask
  numeric safety class
  descriptor compatibility rule
```

The final C++ names should follow existing Open64 conventions and keep
acronyms uppercase. Candidate query APIs are:

```cpp
BOOL DSL_Opcode_Get_Algebraic_Info(...);
BOOL DSL_Opcode_Get_Swap_Equivalent(...);
BOOL DSL_Algebraic_Relation_Get(...);
BOOL DSL_Algebraic_Rewrite_Is_Legal(...);
```

These are design sketches, not a frozen ABI. Public enum values and registry
record meanings become compatibility contracts once emitted in a binary IR
image or exposed in stable dumps.

### Shared simplifier result

Introduce an internal result object that communicates the rewrite without
making frontend code inspect WN nodes:

```text
status
replacement kind
logical replacement operator/version
replacement operands
replacement TCON result set, when applicable
tensor constant-folding candidate, when payload evaluation is required
result TensorDescriptorIR
metadata/lineage transfer action
diagnostic reason when rejected
```

The first implementation may keep this structure runtime-only. Adding a
binary table is not required merely to perform simplification.

### Construction-time builder hook

Place the hook in the native DSL builder immediately after:

1. logical opcode/version lookup;
2. operand and attribute validation;
3. result descriptor inference; and
4. effect and algebraic contract lookup.

Run it before allocating the final native DSL node and publishing its result
record. `DSL_Builder_Create_Operator` and
`DSL_Builder_Create_Operator_With_Result` remain the public frontend boundary
where possible.

Creation-time DSL simplification must be disabled when `Enable_WN_Simp` is
false so `-OPT:wn_simp=off` remains the master baseline and debugging control.
The shared builder may own a narrower stage enable/query API, but that API may
only further restrict simplification and must not override the master switch.

### Tensor constant-folding handoff

The expression simplifier does not evaluate arbitrary tensor payloads.
When preparation proves that all semantic operands and attributes are
constant, but the result requires tensor payload computation, return a
tensor-constant-folding candidate through the shared result object.

Tensor constants are target constants. The storage representation, compatible
TCON escape, ZERO/ONE/SPLAT forms, inline dense storage, side-file dense
storage, constant identity, and inspection rules are defined in
`doc/WHIRL-DSL-TENSOR-CONSTANT-FOLDING-PLAN.md`. This document owns only the
simplifier-facing interface and its WN/WOPT adapters.

The handoff contract is:

```text
DSL expression simplification
  -> identify all-constant tensor candidate
  -> tensor constant-folding specialization
  -> publish canonical common.tensor_const
  -> revisit affected parent expressions
```

The folder consumes the logical operator and version, constant operands,
canonical attributes, complete result TensorDescriptorIR, source/result
identity, and the numeric-policy decision already established by the legality
engine. It returns either:

- a tensor TCON using ZERO, ONE, SPLAT, inline dense, or side-file dense
  storage;
- a budget or unsupported-evaluator rejection; or
- a diagnostic for an invalid constant expression.

Successful publication uses the builder's ordinary tensor-constant and result
materialization APIs. It must not bypass symbol creation, source position,
lineage, gatekeeper checks, mapped-image tables, or atomic side-file
publication.

After a folded constant replaces the expression, enqueue its users for the
same bottom-up simplification logic. This permits a newly folded child to
expose an ordinary identity, cancellation, or DSL-specific rule in its parent
without embedding payload evaluation into `wn_simp`.

The controls are owned by the phase that performs the work:

- `-OPT:wn_simp` is the master switch for traditional construction
  simplification and construction-time DSL cleanup;
- a future `-DSL:tensor_const_fold` option may further restrict tensor
  specialization, but it must not re-enable construction-time work when
  `Enable_WN_Simp` is false;
- `-DSL:canon` controls statement-level VHO DSL canonicalization; and
- adapted WOPT continues to own WOPT-specific controls such as
  `-WOPT:divrem`.

No stage may re-enable another stage disabled by its owning option. A tensor
constant fold performed in a later non-construction phase must still use the
same fixed candidate/result records and rejection vocabulary.

### Tensor TCON operation interface

Model the evaluator after the existing target-constant entry point:

```cpp
TCON Targ_WhirlOp(OPCODE op, TCON kid0, TCON kid1, BOOL *folded);
```

The DSL extension accepts logical operator identity, arbitrary reviewed
operand count, canonical static attributes, one ordinary result type or the
two reviewed projectable result types, and an explicit materialization policy:

The M0 contract is declared in
`osprey/common/com/dsl_tensor_fold.h`. The fixed result bound is
`DSL_TENSOR_FOLD_MAX_RESULTS == 2`; ordinary evaluators publish at most one
result, and only a reviewed projectable evaluator such as DIVREM may publish
two. `DSL_TENSOR_FOLD_CANDIDATE` carries logical operator/version, TCON
operands, operand/result `TY_IDX` arrays, canonical fixed-row attributes, and
the materialization policy. `DSL_TENSOR_FOLD_OUTPUT` carries a status, a
structured rejection reason, and two bounded result slots.

The structured rejection vocabulary is:

- `reject_unsupported_evaluator`;
- `reject_unsupported_operator`;
- `reject_malformed_candidate`;
- `reject_effectful_operator`;
- `reject_non_constant_operand`;
- `reject_descriptor_mismatch`;
- `reject_unresolved_shape`;
- `reject_numeric_policy`;
- `reject_result_budget`;
- `reject_work_budget`;
- `reject_materialization_policy`;
- `reject_division_by_zero`;
- `reject_target_capability`; and
- `reject_profitability`.

`Targ_DSL_WhirlOp(const DSL_TENSOR_FOLD_CANDIDATE *,
DSL_TENSOR_FOLD_OUTPUT *)` is the representation-neutral evaluator entry
point. M0 declares and tests the contract only; it does not implement tensor
payload evaluation.

`Targ_DSL_WhirlOp()` is representation-neutral with respect to WN and CODEREP.
It receives TCON operands through the candidate record and returns one TCON
slot for an ordinary operation or two TCON slots for a specifically registered
projectable operation such as tensor DIVREM. It must not receive `WN *`,
`CODEREP *`, frontend builder handles, Python objects, or raw physical
`OPR_DSL` records. The exact logical operator and version select a registered
evaluator.

The three-step integration is:

1. **Prepare:** the WN or CODEREP adapter recognizes the logical DSL
   operation, obtains each constant symbol's TCON, validates the complete
   TensorDescriptorIR and numeric/effect policy, and forms canonical typed
   attributes.
2. **Apply engine:** call `Targ_DSL_WhirlOp()`. Tensor element arithmetic may
   reuse `Targ_WhirlOp()` so integer width, floating-point format, conversion,
   and exceptional-value behavior remain target-constant semantics.
3. **Postprocess:** on success, call `Enter_tcon()` for each result, associate
   each `TCON_IDX` with its result `TY_IDX` through `New_Const_Sym()`, and
   publish the representation-specific value or projection while restoring
   source position, result identity, lineage, metadata, and use/definition
   relationships.

The WN adapter publishes the logical `common.tensor_const` result through the
native builder. The WOPT adapter hashes the tensor constant through CODEMAP.
WOPT therefore needs a tensor-aware form equivalent to:

```cpp
CODEREP *CODEMAP::Add_tcon(TCON_IDX tcon_idx, TY_IDX result_ty);
```

The full `TY_IDX` parameter is necessary because a tensor's type is not
recoverable from the private physical TCON escape MTYPE. No new CODEREP
constant kind is required merely to carry a tensor TCON; the first
implementation should extend the existing constant-symbol/`CK_RCONST` path
unless hashing, matching, or value-numbering analysis proves that insufficient.

Unsupported operations return the unchanged expression. An invalid constant
expression produces a diagnostic. Budget rejection is not a semantic error.
The evaluator must not call back into `wn_simp`; after postprocessing, the
caller may enqueue the parent expression for another ordinary simplification
attempt.

### Explicit VHO DSL simplification pass

Add a separately controlled step to the ordered VHO DSL optimization pipeline.
Model its option control after WOPT: the pipeline order is fixed, while each
step can be enabled or disabled independently.

The pass performs a bottom-up walk, consults the same contracts and legality
engine as the builder, updates value definitions and uses, and leaves the tree
valid for the gatekeeper after every accepted rewrite.

The pass is target-independent and must not depend on backend code generation,
kernel selection, Triton, CUDA, or a runtime library.

### WOPT first-class logical DSL operators

WOPT already reuses the traditional simplifier in the intended way.
`opt_fold.cxx` defines `simpnode` as `CODEREP *`, supplies CODEREP accessors
and constructors, and includes `wn_simp_code.h`. Its constructors use
`Alloc_stack_cr`, send temporary CODEREPs through `CODEMAP::Hash_Op`, and keep
the hashed memory-pool result. DSL support must preserve this framework rather
than add a second WOPT algebraic engine.

The current physical representation is not sufficient for admitting
unlowered DSL nodes into WOPT:

- both `CK_OP` and `CK_IVAR` store `_opr` in an 8-bit bitfield;
- the native `OPERATOR` range currently ends at 148, leaving only 107 unused
  numeric values in that field;
- `OPCODE` also reserves eight bits for the operator;
- `CODEMAP::Hash_op_and_canon` hashes `cr->Op()` and operand CODEREP IDs;
- `CODEREP::Match` compares `Op()`, kid count, kids, and a small set of
  native-operator-specific fields;
- `CODEREP::Print_node` prints through `OPCODE_name`; and
- the generated simplifier dispatch table has no rule entry for physical
  `OPR_DSL`.

Consequently, WOPT must not import an `OPR_DSL` node as an ordinary `CK_OP`
and continue using only its physical opcode. That would lose logical operator
identity, version, attributes, and TensorDescriptorIR identity from hashing
and matching. Two different DSL operations with the same kids could then be
treated as the same value.

The recommended architecture is:

1. Keep the 8-bit physical `OPR_DSL` escape at the WN and binary WHIRL
   boundaries for backward compatibility.
2. Decode it at WN-to-CODEREP import into a first-class WOPT logical operator
   identity. WOPT optimization code must not inspect the physical escape or
   DSL image-record index.
3. Use a WOPT-local logical operator representation wide enough for the DSL
   namespace. Do not consume the 107 currently unused native operator values
   as the long-term DSL allocation scheme.
4. Carry the DSL operator version and a WOPT-owned semantic-info reference
   containing canonical attributes and TensorDescriptorIR identity.
5. Include logical operator, version, semantic-info identity, and kids in
   CODEREP hashing and equality.
6. Print logical names such as `OPR_DSLADD` in WOPT traces.
7. At CODEREP-to-WN emission, encode the logical operator back through the
   existing physical `OPR_DSL` escape and rebuild its mapped-image
   relationships.

The current x86-64 WOPT debug image reports `sizeof(CODEREP) == 88`,
`sizeof(CK_OP subrecord) == 48`, and an 8-bit `_opr`. The `CK_OP` subrecord
also has alignment space before its first pointer. A 16- or 32-bit WOPT-local
logical operator field may therefore fit without increasing `CODEREP` on this
ABI, but this is not an architectural guarantee. Any layout change must add
compile-time size and offset checks for every supported target and must verify
`Alloc_stack_cr`, `CXX_NEW_VARIANT`, `Copy`, and extra-kid allocation.

For simplification, a first-class DSL CODEREP remains logically visible in
WOPT. Preparation maps only an eligible, semantically equivalent operation,
such as integer `common.add`, to a temporary native `OPR_ADD` CODEREP view.
The existing CODEREP instantiation of `wn_simp_code.h` performs the rewrite.
Postprocessing then publishes a correctly hashed DSL CODEREP result, folded
constant, or reused value while preserving tensor semantic information.

The existing WOPT controls remain authoritative:

- `-WOPT:cr_simp=on|off` controls `WOPT_Enable_CRSIMP`; and
- `-WOPT:fold2const=on|off` controls `WOPT_Enable_Fold2const`.

DSL CODEREP simplification must honor both controls. `-OPT:wn_simp` remains
the construction-time WN simplifier control; it must not be mistaken for the
independent WOPT CODEREP simplifier switch. Debug certification should retain
`.B` and `ir_b2a -st -src` evidence for separately controlled frontend/VHO and
WOPT comparisons.

## Traditional Rule Preservation and DSL Migration

The DSL simplifier is additive. It must not remove, disable, narrow, or
otherwise change the existing simplification capability for traditional WHIRL
operators. Existing scalar expressions continue to use the complete
`wn_simp` rule set and its established option controls. Regression testing must
cover that behavior while the DSL path is introduced.

The first DSL implementation does not list every traditional Open64 rule
because a logical DSL operator does not automatically inherit scalar legality.
Each existing rule family must be inventoried and assigned to one of these
migration classes:

1. **Preserved scalar rule:** continue using the existing `wn_simp`
   implementation without modification for traditional WHIRL operators.
2. **Direct DSL reuse:** reuse the algebraic rule when logical opcode, dtype,
   TensorDescriptorIR, and effect checks prove identical semantics.
3. **Tensor-aware adaptation:** retain the algebraic intent but add shape,
   broadcasting, layout, sharding, placement, memory, alignment, quantization,
   ownership, and result-identity legality.
4. **Lowering-level rule:** apply only after a DSL operator has been lowered to
   the traditional WHIRL form for which the existing rule was designed.
5. **Inapplicable rule:** do not apply when the rule depends on scalar address,
   load/store, target, or exceptional-value semantics that the DSL operation
   does not possess.
6. **Deferred operator-specific rule:** require a separately reviewed semantic
   and profitability contract rather than extrapolating from a scalar rule.

The inventory must include at least these existing Open64 rule families:

- unary, binary, and ternary constant folding;
- addition, subtraction, negation, multiplication, and identities;
- division, remainder, modulo, reciprocal, square root, and power;
- minimum and maximum;
- bitwise operations, logical operations, and shifts;
- equality, inequality, ordered relations, and operand-swap canonicalization;
- conversion and bit extraction;
- `SELECT` and conditional forms;
- intrinsic simplification;
- reassociation, cancellation, distribution, and factorization; and
- address, `LDA`, `ILOAD`, `ISTORE`, and other memory-oriented rules.

This inventory is a continuing coverage requirement, not a request to enable
every rule for tensors. A rule becomes available to a DSL operator only after
its migration class and legality contract are recorded and tested. Rules that
remain scalar or lowering-only still count as preserved Open64 capability.

## First DSL Vertical Slice

The following list is the first native DSL implementation slice, not the full
Open64 simplification repertoire. Stage it conservatively while the broader
inventory above remains active and tracked.

1. Bottom-up expression canonicalization for pure, same-shape integer
   `common.add` and `common.mul`.
2. Handoff of all-constant tensor expressions to the separately controlled
   tensor constant-folding specialization project, followed by affected-parent
   simplification.
3. Constant-to-`kid1` normalization and deterministic structural operand
   ordering using DSL-specific swap-equivalent contracts.
4. Ordered-relation normalization, including the corresponding logical
   relation change when operands are swapped.
5. Exact identity rules such as integer `x + 0 -> x` and `x * 1 -> x`.
6. Annihilator rules such as integer `x * 0 -> 0`, after confirming that
   descriptor, effect, and exceptional-value semantics are preserved.
7. Reassociation to expose adjacent constants only for integer tensors
   initially.
8. Factorization of multiplication under addition/subtraction, using explicit
   paired-operator contracts and the four operand-position cases.
9. Floating-point forms only after strict-mode and reassociation tests are in
   place.

Do not automatically generalize scalar rules to broadcasting tensors,
quantized tensors, symbolic shapes, or domain operators. Each such extension
requires a legality rule.

Matrix multiplication deserves its own reviewed relations. For example,
`matmul(A, B) + matmul(A, C)` may admit a mathematical factorization, but its
shape requirements, floating-point order, layout effects, and profitability
are materially different from elementwise multiplication.

## OpenXLA Comparison

OpenXLA's
[`AlgebraicSimplifier`](https://github.com/openxla/xla/blob/c37fc6a383b870f43cef82280418fcefcc90b0f8/xla/hlo/transforms/simplifiers/algebraic_simplifier.h)
is useful as a breadth and safety reference. Its design reinforces several
requirements for this plan:

- algebraic simplification is governed by explicit options;
- shape and layout constraints are part of rewrite legality;
- floating-point-sensitive rewrites require policy controls;
- convolution, dot, reshape, broadcast, reduce, and other operators need
  operator-specific handlers; and
- profitability and target-sensitive decisions should not be confused with
  universally valid algebra.

Open64 should retain its own WHIRL construction-time simplifier model and VHO
pipeline rather than copying the HLO implementation. The useful lesson is the
discipline of explicit legality and policy, especially for tensor shapes and
layouts.

## Verification and Inspection

Every accepted rewrite must pass these checks:

1. Logical opcode/version and operand contracts are valid.
2. Input and result TensorDescriptorIR compatibility is proven.
3. Required dtype, rank, shape, broadcasting, layout, sharding, placement,
   memory, alignment, and quantization facts are preserved.
4. Effect, ownership, and alias constraints permit removal or reordering.
5. Numeric safety satisfies the active integer/floating-point policy.
6. Result symbol identity, source position, lineage, and compiler metadata are
   preserved according to the replacement protocol.
7. The DSL gatekeeper accepts the resulting PU.

`ir_b2a -st -src` must show only the final logical DSL operators and values. It
must not expose the physical `OPR_DSL` escape representation. Simplifier trace
output should identify the rule, logical operators, acceptance or rejection,
and a concise legality reason.

## Tests

Add focused tests under `osprey/common/com/tests` for both construction-time
and explicit-pass behavior.

Required test groups are:

1. Bottom-up canonicalization followed by constant folding and identity
   replacement.
2. Constant-to-`kid1` normalization.
3. Deterministic structural ordering across repeated runs and different
   allocation orders.
4. Operand-swap canonicalization, including reversed relational operators.
5. Reassociation enabled and disabled cases.
6. Four-position factorization and identity factorization.
7. Strict floating-point rejection and explicitly enabled reassociation.
8. Tensor descriptor mismatch rejection for dtype, rank, shape, layout,
   sharding, placement, memory, alignment, and quantization.
9. Effectful-operator rejection.
10. Result symbol, source position, lineage, and metadata preservation.
11. Builder simplification disabled baseline.
12. Gatekeeper, binary mapped-image roundtrip, and `ir_b2a -st -src` evidence.
13. Traditional scalar `wn_simp` regression coverage across the inventoried
    rule families, proving that the additive DSL path has not narrowed existing
    Open64 behavior.
14. Tensor projectable DIVREM combination and projection tests: matching DIV
    and REM share one logical DIVREM; mismatched operands do not; one live
    projection reconstructs a standalone operation; two live projections
    retain the combined operation; zero-divisor, signed-overflow risk,
    descriptor, effect, option, target-lowering-capability, and profitability
    guards are honored. An NVISA-style target with no combined lowering must
    never receive DIVREM even when the generic transformation is otherwise
    legal.
15. Statement-level coefficient collection such as `x+x -> 2*x`, with
    `-DSL:canon` enabled and disabled, while proving that the traditional
    scalar construction-time form is unchanged.
16. A folded tensor child exposing a parent identity or second constant fold,
    proving that affected users are revisited without recursive entry into the
    tensor evaluator.
17. Equivalent WN and WOPT inputs producing semantically identical TCON,
    TensorDescriptorIR, logical operator, and `ir_b2a` evidence.
18. Unsupported evaluator, unresolved shape, effectful operation, numeric
    policy mismatch, and materialization-budget rejection leaving the original
    expression unchanged with a structured reason.
19. Operator-specific legality tests for reshape and broadcast, followed by
    separately reviewed test groups before enabling reduce, dot/matmul, or
    convolution simplification.

Retain the `.B` and `.T` artifacts from representative tests in the designated
host-mounted artifact directory for review.

The comparative coverage above is intentional:

- TVM-derived cases exercise constant eligibility, parent revisiting,
  statement-level coefficient collection, and the DSL-specific div/rem family.
- OpenXLA-derived cases exercise shape/layout-sensitive legality, explicit
  policy controls, and operator-specific handlers instead of assuming that one
  generic elementwise rule covers reshape, broadcast, reduce, dot, or
  convolution.
- Open64-specific cases prove reuse of `wn_simp_code.h`, target-constant
  behavior, WN/WOPT parity, binary compatibility, and stable logical
  inspection.

## Compatibility Requirements

1. Existing scalar WHIRL simplification behavior must not change accidentally.
2. Existing DSL binary WHIRL remains readable when simplification is disabled
   or absent.
3. The first implementation should avoid changing WN layout, the physical
   `OPR_DSL` escape encoding, ELF section contracts, or tensor type encoding.
4. Runtime-only algebraic tables may precede any mapped-image representation.
5. If algebraic contracts later become part of the binary IR, add an explicit
   revision, reader fallback, printer support, migration path, and old/new
   compatibility tests.
6. Operator names, versions, algebraic properties, and paired relations are
   versioned semantic contracts. Changing them must not reinterpret an older
   binary image silently.

## Synchronized Implementation Milestones

This plan and `WHIRL-DSL-TENSOR-CONSTANT-FOLDING-PLAN.md` use the milestones
below as their shared implementation and pull-request boundaries. The `S0-S8`
stages and tensor-folding action numbers remain useful work breakdowns, but
they must not be implemented as two independent pipelines.

Each milestone starts from the merged result of the preceding milestone. A
pull request must contain only one milestone unless review explicitly approves
combining adjacent milestones. Every milestone must build and pass its own
tests without relying on unmerged code from a later milestone.

### M0: Freeze the shared contracts

**Simplifier work:** complete S0; publish the algebraic registry schema,
replacement-compatibility rules, rejection reasons, candidate/result handoff,
options, and trace vocabulary.

**Tensor-folding work:** complete action items 1, 2, and 4 at the design and
API-contract level; publish evaluator identity, bounded result records,
materialization budgets, and failure behavior.

**Exit criteria:** both plans describe the same fixed-layout candidate and
result records; ordinary operations return at most one result and reviewed
projectable operations return at most two; no WN, CODEREP, TCON, or mapped
image layout change is required.

**Pull request:** documentation, API declarations needed by compile tests, and
contract unit tests only. Do not add an evaluator or builder hook.

### M1: Bring up the shared simplifier foundation

**Simplifier work:** implement S1 and the non-folding portions of S2: runtime
algebraic registry, legality checks, rejection reasons, stack-local WN
preparation, traditional `wn_simp` invocation, and DSL postprocessing.

**Tensor-folding work:** provide a mock evaluator behind the M0 handoff so
candidate, success, rejection, and unchanged paths are tested without tensor
payload storage.

**Exit criteria:** a logical DSL expression can pass through prepare, existing
Open64 engine, and postprocess; `Enable_WN_Simp` disables the entire path; the
mock handoff proves simplifier code does not inspect tensor bytes.

**Pull request:** registry, legality, bridge, controls, traces, and focused
unit tests. No tensor TCON storage and no builder publication.

### M2: Bring up tensor TCON storage independently

**Simplifier work:** retain the M1 mock boundary; add no new rewrite.

**Tensor-folding work:** implement action items 3 through 7 and the applicable
parts of 13 through 15: tensor TCON escape records, ZERO/ONE/SPLAT,
INLINE_DENSE, SIDE_FILE_DENSE, target-format element access, identity,
printing, verification, and mapped-image reopen.

The M2 persistent representation uses a legal string TCON whose sized
character-array payload carries the fixed-layout tensor envelope and optional
inline bytes. `TCON_IDX` is the stable tensor-constant identity. The existing
TCON table and TCON character-array table own the mapped image; runtime lookup
or deduplication indexes are derived and rebuildable. Do not append tensor
rows to or bump the strict version-1 `.WHIRL.dsl` image for this milestone.

**Exit criteria:** tensor constants can be created, deduplicated, printed,
written, reopened, and verified without invoking the simplifier. Existing
`sizeof(TCON)`, scalar TCON behavior, and old-reader compatibility remain
unchanged.

**Pull request:** tensor constant representation and storage only, with
retained `.B`, side payload, and `ir_b2a -st -src` evidence.

### M3: Certify the first integrated folding path

**Simplifier work:** implement S3 for exact integer `common.add` and
`common.mul`; queue all-constant tensor candidates and preserve the normal
unsimplified node on rejection.

**Tensor-folding work:** implement action items 8, 10, 11, and 12 for the same
operators: evaluate through target-format operations, publish atomically, and
requeue affected parents.

**Exit criteria:** construction performs the complete sequence:

```text
prepare -> wn_simp -> DSL postprocess -> tensor-fold request
        -> tensor evaluator -> publish tensor_const -> revisit parents
```

Enabled and disabled runs produce equivalent results and reviewable structural
differences. Source position, result symbol, descriptor, lineage, and metadata
survive replacement. A rejected or failed fold leaves the original logical
expression and no partial constant artifact.

**Pull request:** the smallest real end-to-end vertical slice. Do not add the
explicit VHO pass, reassociation expansion, WOPT admission, or DIVREM.

### M4: Add the explicit VHO simplification pipeline step

**Simplifier work:** implement S4 and the reviewed integer portions of S5,
including deterministic canonicalization, statement-level coefficient
collection, fixed pipeline placement, and independently debuggable controls.

**Tensor-folding work:** reuse the M3 evaluator and publication service after
descriptor and constant propagation; revisit parents without recursively
entering the folder.

**Exit criteria:** construction-time and VHO-time folding share one legality,
evaluator, publication, and trace contract. `-OPT:wn_simp`, `-DSL:canon`, and
`-DSL:algebraic` A/B artifacts distinguish construction simplification, VHO
canonicalization/coefficient collection, and VHO tensor evaluation without
changing program results.

**Pull request:** VHO pipeline integration and canonicalization only.

### M5: Admit logical DSL expressions into WOPT

**Simplifier work:** implement the foundational portion of S8: logical
operator semantic information in CODEREP, hashing/equality, WN-to-CODEREP and
CODEREP-to-WN conversion, effect queries, logical printing, and reuse of the
CODEREP instantiation of `wn_simp_code.h`.

**Tensor-folding work:** prove adapted WOPT calls the same M3 tensor evaluator
API and creates constants semantically identical to WN/VHO folding.

**Exit criteria:** WOPT can optimize the M3 operator set without exposing the
physical `OPR_DSL` escape, changing CODEREP layout unexpectedly, or creating a
second tensor-folding implementation. `-WOPT:cr_simp` and
`-WOPT:fold2const` A/B artifacts are retained.

**Pull request:** WOPT admission and existing-rule reuse only. DIVREM remains
disabled until M6.

### M6: Add projectable tensor DIVREM

**Simplifier work:** complete the projectable-operation portion of S2 and S8:
logical DIVREM/DIVPART/REMPART contracts, combination, projection-use
accounting, CSE/PRE identity, and emission-time uncombining.

**Tensor-folding work:** implement action item 9 and its test matrix: bounded
two-result evaluation, atomic publication, and standalone DIV or REM recovery
when only one projection remains.

**Exit criteria:** target lowering capability and profitability are separate
mandatory gates. An NVISA-style target with separate DIV/REM and no
`Expand_DivRem()` support never receives a combined operation. Option-on,
option-off, target-declined, one-projection, and two-projection artifacts are
semantically equivalent and structurally reviewable.

**Pull request:** the complete DIVREM feature across registry, simplifier,
tensor evaluator, WOPT projectability, gatekeeper, and tests. Do not merge a
partial producer without all required consumers.

### M7: Expand and certify

**Simplifier work:** complete S6 and S7 for each newly reviewed operator,
symbolic shape, broadcasting, quantization, and placement case.

**Tensor-folding work:** complete remaining action items 13 through 17 and the
full comparative test matrix.

**Exit criteria:** every enabled rule has positive, rejection, option A/B, WN,
VHO, and WOPT coverage as applicable. Binary compatibility, warnings, no-tab
checks, gatekeeper verification, mapped-image reopen, and retained
`ir_b2a -st -src` evidence all pass.

**Pull requests:** one independently reviewable operator family or semantic
capability per pull request. Do not use M7 as a miscellaneous cleanup batch.

### Milestone status

| Milestone | Status | Merge dependency | Review artifact |
| --- | --- | --- | --- |
| M0 | Merged through PR #89/#92 | None | Contract/API test report |
| M1 | Merged through PR #93 | M0 merged | Simplifier bridge traces |
| M2 | Merged through PR #94/#95 | M1 merged | Tensor TCON `.B` and `.T` |
| M3 | Merged through PR #96 | M2 merged | Enabled/disabled fold artifacts |
| M4 | Merged through PR #97 | M3 merged | `artifacts/m4-vho-simplification/vho_simplification.{B,T}` |
| M5 | Active on `codex/dsl-wopt-m5`; W0/W1 foundation in progress | M4 merged | WOPT A/B artifacts |
| M6 | Blocked by M5 | M5 merged | DIVREM gate/projection artifacts |
| M7 | Blocked by M6 | M6 merged | Full certification matrix |

Update this table when a milestone starts, when its pull request opens, and
when it merges. The next milestone must not begin integration work against an
unmerged predecessor; design and isolated test preparation may proceed without
publishing dependent source changes.

M0 is ready for review because the shared tensor-fold candidate and result
contracts, evaluator identity, bounded result policy, and rejection vocabulary
are frozen and tested by the contract/API tests. The canonicalization registry,
option ownership, and builder integration remain M1 work.

## Implementation Status

The M0 tensor-folding handoff is implemented in
`osprey/common/com/dsl_tensor_fold.{h,cxx}`. It recognizes the reviewed
`common.add.v1` evaluator identity and defines fixed-layout candidates,
bounded results, policies, and structured rejection reasons. The target hook
remains non-folding by default; no tensor TCON is created and no builder
behavior changes.

The first M1 preparation is implemented:

- `common.add.v1` and appended `common.mul.v1` have runtime-only algebraic
  contracts;
- paired factor/distribute relations record all four operand positions;
- integer tensor operands with equivalent semantic descriptors are placed in a
  deterministic order, with a lone constant normalized to `kid1`;
- canonical keys include logical operator/version, sorted opcode attributes,
  semantic TensorDescriptorIR fields, and recursively ordered operands;
- compiler metadata, source context, and lineage do not participate;
- the builder-local control can further restrict work but cannot override a
  disabled `Enable_WN_Simp` master control.

The remaining M1 foundation is implemented:

- `dsl_simp.{h,cxx}` validates logical operator/version, purity, canonical
  TensorDescriptorIR equivalence, integer numeric policy, and the active
  `Enable_WN_Simp` master control;
- preparation initializes a temporary stack-local WN with the equivalent
  traditional `OPR_ADD` or `OPR_MPY` opcode and disposable projected kids;
- application invokes `WN_SimplifyExp2`, preserving its established
  consume/delete ownership convention;
- postprocessing classifies an unchanged result, retained `kid0`/`kid1`, or a
  new pool-backed WN without publishing a tensor replacement;
- stable traces expose `OPR_DSLADD`/`common.add.v1` and
  `OPR_DSLMUL`/`common.mul.v1`, never the physical DSL escape tag; and
- the tensor-fold mock is explicit, resettable, runtime-only, and disabled by
  default. It exercises success, structured rejection, budget, and unchanged
  paths without reading tensor payload bytes.

The scalar operand projection remains test-only in M1. Production tensor
projection and builder publication remain disabled until M2 establishes
tensor TCON storage and M3 integrates the first real folding path.

The M2 tensor TCON storage implementation merged through PR #94. The
coordinated main integration rebuilds the derived tensor TCON cache after the
standard global TCON tables are mapped, links the tensor TCON service into
Open64 phases that use the shared symbol-table implementation, and makes
symbol-table and global-TCON dumps print the logical tensor constant instead
of its private string-TCON carrier. The retained M2 fixture reopens
`tensor_tcon.B` through `ir_b2a -st -src` and records compact ZERO and
SIDE_FILE_DENSE constants in `tensor_tcon.T`.

M3 begins with exact integral `common.add.v1` and `common.mul.v1` over compact
ZERO, ONE, and SPLAT tensor TCONs. These forms already carry target-format
scalar TCON values and therefore reuse traditional Open64 target constant
operations without reinterpreting tensor bytes in host format. INLINE_DENSE
and SIDE_FILE_DENSE operands without reviewed target-format element extraction
must return a structured non-constant/materialization rejection and preserve
the original logical operator.

The compact M3 slice is now implemented. The builder hook is disabled by
default, remains subordinate to `Enable_WN_Simp`, invokes the traditional
three-step simplifier bridge before `Targ_DSL_WhirlOp`, publishes a
`common.tensor_const` result, and permits the folded value to feed a parent
fold. Dense payload evaluation remains deferred.

The M4 VHO slice is implemented after the released descriptor- and
constant-propagation stage positions. `-DSL:canon` controls deterministic
operand order and the reviewed integer tensor coefficient rewrite
`x+x -> 2*x`; the coefficient is a real compact tensor TCON and logical
`common.tensor_const`. `-DSL:algebraic` independently controls all-constant
tensor evaluation through the same `Targ_DSL_WhirlOp` service used by M3.
Both rewrites update the physical WN and logical DSL image together while
retaining the original result node/value identity, ST, TY, metadata, lineage,
and statement SRCPOS. The retained M4 artifact reopens through
`ir_b2a -st -src` without changing the `.WHIRL.dsl` row layout or version.

## Staged Action List

### S0: Baseline and contract review

- Inventory existing scalar rules and controls used by the initial DSL rules.
- Define exact TensorDescriptorIR replacement compatibility.
- Define numeric safety classes and effect prerequisites.
- Record the distinction between swap-equivalent canonicalization and true
  commutativity.
- Define the stable structural ordering key and canonicalization sequence.

### S1: Runtime algebraic registry

- Add fixed-layout operator-property and paired-relation records.
- Seed only the reviewed common operators.
- Add guarded name/query/print APIs and registry unit tests.

### S2: Shared legality engine and DSL extension

- Implement descriptor, effect, ownership, and numeric-policy checks.
- Implement structured rejection reasons suitable for traces and tests.
- Keep the engine independent of WN physical encoding.
- Define the stack-local physical WN preparation helper required to invoke the
  existing WN simplifier without publishing or pool-managing the temporary
  input.
- Map logical DSL operator/version contracts to traditional simplifier
  operators without exposing the physical `OPR_DSL` escape tag.
- After traditional simplification, invoke a separate DSL-specific rule lane.
- Record tensor DIV, REM, DIVREM, DIVPART, and REMPART as a reviewed
  projectable-operation family rather than an independent reconstruction
  peephole.
- Trace whether a result came from the traditional engine or the DSL-specific
  extension.
- Define the shared result status and fixed handoff record for tensor
  constant-folding candidates.

### S3: Construction-time simplification

- Add the builder hook and replacement/materialization protocol.
- Route applicable algebraic rules through the traditional Open64
  simplification engine after DSL preparation.
- Honor `Enable_WN_Simp` as the master switch and remove or subordinate the
  provisional independent builder control.
- Discard each stack-local input view after the simplifier call and publish
  only pool-backed simplifier results or the normal unsimplified DSL node.
- Implement bottom-up integer canonicalization before constant and identity
  rules.
- Add constant-to-`kid1`, ordered-relation, and deterministic structural
  normalization.
- Preserve result symbol, source position, lineage, and metadata.
- Send all-constant tensor payload candidates to the tensor constant-folding
  service. The current compact M3 slice evaluates ZERO, ONE, and SPLAT
  integral `common.add.v1` and `common.mul.v1`; unsupported dense forms retain
  the original logical operator.
- Allow a stage-specific option to disable the hook only as a further
  restriction; it must never re-enable work disabled by `Enable_WN_Simp`.

### S4: Explicit VHO DSL pass

- Add the fixed-position, independently controlled pipeline step.
- Walk logical DSL expression definitions bottom-up.
- Reuse the S2 legality engine and S3 rewrite implementation.
- Run statement-level coefficient collection only after regular construction
  simplification and only under `-DSL:canon`.
- Keep `x+x -> 2*x` and related polynomial forms out of the scalar
  construction-time path unless a separate target profitability decision
  approves them.
- Invoke tensor constant folding after descriptor and constant propagation
  have completed, publish the resulting `common.tensor_const`, and revisit
  affected parent expressions under their existing simplifier controls.
- Treat publication atomicity as semantic mapped-image visibility: compact
  relationship tables before exposing the replacement node, while permitting
  ordinary unreferenced entries in Open64's global deduplicated TCON table.
- Re-run the gatekeeper after the pass in validation builds and tests.

### S5: Reassociation and factorization

- Enable inherited integer reassociation through the traditional simplifier
  bridge.
- Verify inherited multiplication/addition factorization through all four
  operand positions; do not duplicate `simp_factor()` or
  `simp_factor_idty()`.
- Add strict floating-point negative tests before enabling any floating-point
  form.

### S6: Tensor and domain expansion

- Add broadcasting-aware elementwise rules.
- Review symbolic-shape, quantized, and placement-sensitive cases.
- Add operator-specific rules only when their legality and profitability are
  documented.

### S7: Certification

- Run common/com unit tests and warning/no-tab checks.
- Produce binary and ASCII artifacts with `ir_b2a -st -src`.
- Compare retained simplification-enabled and disabled `.B` artifact families
  produced with `-OPT:wn_simp=on` and `-OPT:wn_simp=off`.
- Verify existing Open64 scalar simplifier and binary compatibility tests.
- Complete the comparative test groups in the Tests section and retain a
  coverage table mapping each enabled rule to its positive, rejection, option
  A/B, WN, and WOPT cases.

### S8: WOPT logical DSL integration

- Define the WOPT-local logical operator and semantic-info representation.
- Prove CODEREP size, offsets, stack allocation, variant allocation, and copy
  behavior on supported targets.
- Decode physical `OPR_DSL` during WN-to-CODEREP import and encode it during
  CODEREP-to-WN emission.
- Include DSL operator, version, attributes, and TensorDescriptorIR identity
  in hash and equality.
- Add logical CODEREP printing and effect-aware optimization queries.
- Route eligible DSL algebra through the existing CODEREP instantiation of
  `wn_simp_code.h`.
- Add `-WOPT:cr_simp` and `-WOPT:fold2const` A/B artifact tests.
- Extend the existing `Combine_Operations()`, projectable-operation queries,
  DCE projection-use accounting, and `Uncombine_Operations()` model to the
  reviewed logical tensor DIVREM family.
- Honor `WOPT_Enable_DIVREM`, target lowering capability, and target
  profitability; retain standalone tensor DIV or REM when any gate declines
  combination or only one projection is useful.

## Ownership and Coordination

This plan is owned by the main WHIRL infrastructure effort. No frontend
subagent is required to implement S0 through S8. Once the opaque builder
behavior and controls are stable, frontend work may add capability detection,
option forwarding, and end-to-end model tests without learning the
simplifier's WN representation.

## Non-Goals for the Initial Bring-Up

- Backend instruction selection or target-specific kernel choice.
- Large graph fusion, quantization insertion, or domain lowering.
- E-graph or unrestricted equality-saturation infrastructure.
- Replacing WOPT, Preopt, IPA, or their established optimization pipelines.
- Changing binary WHIRL merely to record that a simplification occurred.
- Treating compiler metadata as part of tensor type equivalence.
