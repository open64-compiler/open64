# WHIRL DSL Operator Operand Memory Behavior

## Purpose

This document defines the portable ownership, alias, and memory-behavior
vocabulary for Very High Level DSL WHIRL operators.  In plain language, these
contracts state whether an operator reads an operand, modifies its state,
creates new storage, or returns a value related to existing storage.

These are source-level semantic declarations.  They are not `POINTS_TO`, alias
classes, or WOPT `MU` and `CHI` nodes.  The common declarations survive binary
WHIRL and are verified before lowering.  Backend analysis derives its normal
Open64 structures from the verified declarations.

## Ownership Boundary

The portable declarations belong in `osprey/common/com` because frontends,
`ir_b2a`, the gatekeeper, VHO optimization, and the backend all need the same
meaning.  Derived backend analysis remains in `osprey/be/com` and
`osprey/be/opt`.

| Responsibility | Source location | Status |
| --- | --- | --- |
| C/C++ `restrict` type qualifier | `osprey/common/com/symtab_idx.h` | Existing |
| Symbol points to unique memory | `osprey/common/com/symtab_defs.h`, `ST_PT_TO_UNIQUE_MEM` | Existing |
| Tensor result `no_alias=true` | `osprey/common/com/symtab.h`, `symtab.cxx`, `dsl_builder.cxx` | Existing DSL staging |
| Pointer-free state and effect rows | `osprey/common/com/dsl_ir_image.h`, `dsl_ir_image.cxx` | Implemented item-18 slice |
| Portable operand/result behavior vocabulary | `osprey/common/com/dsl_memory_behavior.h`, `dsl_memory_behavior.cxx` | Implemented item 18 |
| Builder and gatekeeper integration | `osprey/common/com/dsl_builder.{h,cxx}`, `dsl_gatekeeper.{h,cxx}` | Implemented item 18 vertical slice |
| Backend points-to facts | `osprey/be/com/opt_points_to.h`, `opt_points_to_non_template.cxx` | Existing; remains backend-owned |
| Alias manager and restricted map | `osprey/be/com/opt_alias_mgr.{h,cxx}` | Existing; remains backend-owned |
| WOPT `MU`/`CHI` construction | `osprey/be/opt/opt_alias_analysis.cxx` | Existing; consumes lowered evidence |

Do not move `POINTS_TO`, `ALIAS_MANAGER`, restricted maps, or WOPT SSA node
classes into `common/com`.  They contain derived, mutable, backend-specific
analysis state.  Item #18 promotes only the semantic inputs needed to derive
those structures.

## Attribute Vocabulary

The following names are logical contracts.  Their numeric enum values must be
append-only once published in a binary image, registry, test, or ASCII dump.

| Attribute | Applies to | Meaning |
| --- | --- | --- |
| `READ` | Operand or abstract state | The operator observes the incoming value or state and does not modify it. |
| `MODIFY` | Operand or abstract state | The operator may change the same logical state; lowering creates an old-state use and new-state definition. |
| `UNIQUE_OWNERSHIP` | Value or state | No other live value may access the same owned storage within the declared scope. |
| `SHARED_OWNERSHIP` | Value or state | Storage may have other owners; transformations must remain conservative unless alias analysis proves more. |
| `FRESH_RESULT` | Result | The operator creates storage that does not alias any operand or other live value. |
| `VIEW_OF_KID(n)` | Result | The result is a non-owning view of operand `kid(n)` and shares its underlying storage. |
| `MAY_ALIAS_KID(n)` | Result | The result may share storage with operand `kid(n)`; no stronger relationship is promised. |
| `INPLACE_UPDATE_KID(n)` | Result and operand | The operation modifies operand `kid(n)` and the result denotes the updated version of that storage. |
| `CONSUMES_KID(n)` | Operand | Ownership transfers to the operation and the old operand cannot be used afterward. Deferred until def-use and escape rules are reviewed. |

`UNIQUE_OWNERSHIP` resolves whether another value can alias the storage.  It
does not, by itself, determine whether an operator reads or modifies that
storage.  `READ` or `MODIFY` must still come from the versioned operator
contract or verified analysis of the operator body.

## Contract Representation

Each versioned logical operator should publish ordered operand behavior and a
result-storage relationship.  A conceptual contract is:

```text
operator_memory_contract {
  operand[kid0] = READ | UNIQUE_OWNERSHIP
  operand[kid1] = READ
  result = FRESH_RESULT | UNIQUE_OWNERSHIP
}
```

The frontend may provide capture evidence, but it must not create unchecked
ownership strings.  The C++ builder resolves the versioned contract, the
gatekeeper verifies it against value definitions, views, region interfaces,
and state identity, and the mapped image records the verified conclusion.

When an operator's behavior depends on a static operation parameter, each
allowed behavior must be part of the versioned operator contract.  Compiler
metadata such as source names or lowering hints cannot change alias or
ownership semantics.

## Examples

### Functional tensor addition

```text
result = common.add(kid0, kid1)

kid0:  READ
kid1:  READ
result: FRESH_RESULT | UNIQUE_OWNERSHIP
```

Neither operand is modified.  The result temporary has unique ownership and
may be freely renamed or forwarded until it escapes.

### Tensor view

```text
result = common.reshape(kid0)

kid0:  READ
result: VIEW_OF_KID(0) | SHARED_OWNERSHIP
```

For a `common.reshape` version whose contract defines a view, the result is not
fresh storage.  A modification through either alias may be visible through the
other, so the gatekeeper must not attach `no_alias=true` to the view result.

### Functional cache append

```text
new_cache = transformer.cache_append(old_cache, key, value)

old_cache: READ | UNIQUE_OWNERSHIP
key:       READ
value:     READ
new_cache: FRESH_RESULT | UNIQUE_OWNERSHIP
```

The old cache remains unchanged.  A later reviewed contract may use
`CONSUMES_KID(0)` to permit storage reuse when def-use and escape analysis prove
that `old_cache` is dead.

### In-place cache update

```text
new_cache = transformer.cache_update(old_cache, position, key, value)

old_cache: MODIFY | UNIQUE_OWNERSHIP
position:  READ
key:       READ
value:     READ
new_cache: INPLACE_UPDATE_KID(0) | UNIQUE_OWNERSHIP
```

The old and new cache names are versions of the same logical state.  The
verified `MODIFY` edge lowers to an old-state use and new-state definition.

### Common scatter vertical slice

```text
result = common.scatter(kid0, kid1, kid2)

kid0:  MODIFY | UNIQUE_OWNERSHIP
kid1:  READ
kid2:  READ
result: INPLACE_UPDATE_KID(0) | UNIQUE_OWNERSHIP
```

`common.scatter.v1` is the first implemented stateful contract.  Its abstract
state rows cover runtime-status reads and random-state or mutable-buffer
modifications.  VHO lowering expresses those rows with standard WHIRL
by-reference parameters, allowing backend call alias processing to construct
its normal `MU`/`CHI` representation.

### Opaque library operation

An opaque Python or library call cannot infer memory behavior from operand
definitions alone.  It requires a reviewed, versioned call contract.  Without
one, the gatekeeper must reject the operation or classify its memory behavior
conservatively.

## Before And After DSL Enhancement

| Concern | Baseline Open64 | After DSL enhancement |
| --- | --- | --- |
| Source declaration | C/C++ type qualifiers and symbol flags such as `restrict` and `ST_PT_TO_UNIQUE_MEM` | The same baseline declarations plus versioned DSL operand, result, ownership, and state contracts in `common/com` |
| Tensor result ownership | Not a baseline primitive-type requirement | Fresh DSL tensor result symbols carry verified unique ownership and `no_alias=true` |
| View relationship | Reconstructed from lowered address expressions and points-to analysis | Preserved explicitly as `VIEW_OF_KID(n)` before lowering |
| In-place update | Inferred from stores, calls, and alias analysis | Declared as `MODIFY` and `INPLACE_UPDATE_KID(n)`, then verified against the operator contract |
| Abstract runtime state | Usually represented indirectly by memory, calls, globals, or runtime conventions | Named state objects and ordered `READ`/`MODIFY` rows survive mapped binary WHIRL |
| Binary inspection | Symbol/type tables and lowered WHIRL memory operations | `ir_b2a -st -src` also prints logical ownership and abstract-state evidence |
| Backend alias representation | `POINTS_TO`, restricted maps, alias classes | Unchanged; constructed from lowered verified common declarations |
| WOPT state edges | `MU`/`CHI` derived from canonical WHIRL memory behavior | Still derived by WOPT, with VHO lowering preserving the DSL contract as standard WHIRL evidence |
| Unknown behavior | Conservative call and memory assumptions | Conservative unless a versioned DSL contract and gatekeeper proof establish stronger behavior |

## Lowering Mapping

The intended handoff is:

```text
common/com operator memory contract
  -> gatekeeper proof
  -> VHO DSL lowering
  -> standard ST, TY, WN call, and parameter evidence
  -> be/com POINTS_TO and alias attributes
  -> WOPT MU/CHI construction
```

The initial mapping is:

| Verified common fact | Lowered Open64 evidence | Expected backend result |
| --- | --- | --- |
| `READ` state edge | Addressable state identity passed as a read-only reference | `MU` use, no state `CHI` |
| `MODIFY` state edge | Addressable state identity passed as a modifying reference | Old-state `MU` and new-state `CHI` |
| `UNIQUE_OWNERSHIP` | Unique-memory symbol or equivalent canonical ownership evidence | `POINTS_TO::Unique_pt()` and/or `No_alias()` when legal |
| `FRESH_RESULT` | New no-alias result temporary | Independent points-to identity |
| `VIEW_OF_KID(n)` | Canonical address/view relationship to operand storage | Shared based symbol and overlapping points-to relationship |

The exact adapter must be tested against existing WOPT behavior.  It must not
instantiate backend `POINTS_TO`, `MU_NODE`, or `CHI_NODE` objects in the
frontend or store them in the binary WHIRL image.

## Verification Requirements

The gatekeeper must reject:

1. A state effect attached to an operator version declared statically pure.
2. `UNIQUE_OWNERSHIP` when a live view, shared owner, or escaped reference is
   known.
3. `VIEW_OF_KID(n)` or `MAY_ALIAS_KID(n)` with an invalid operand ordinal.
4. `INPLACE_UPDATE_KID(n)` without `MODIFY` on the same operand.
5. `FRESH_RESULT` combined with a view or alias relationship.
6. Conflicting ownership assertions for the same value or state scope.
7. A frontend assertion that disagrees with the versioned operator contract.

When proof is unavailable, the compiler must omit the stronger attribute and
use conservative alias behavior.  It must not silently assume unique ownership.

## Compatibility Rules

1. The vocabulary is additive and enum values are append-only after publication.
2. Existing `.WHIRL.dsl` version-1 rows remain unchanged.
3. The optional `.WHIRL.dsl_effects` section may be absent; absence means no
   declared abstract-state objects or effects.
4. Existing C/C++ `restrict`, symbol flags, `POINTS_TO`, alias rules, and WOPT
   behavior remain valid.
5. Older tools may ignore the additive DSL effect section while continuing to
   see structurally legal WHIRL trees.
6. `ir_b2a -st -src`, the gatekeeper, mapped-image tests, and WOPT alias tests
   are required gates for each implemented attribute.
