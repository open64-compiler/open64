# FHE SYNC-5 Standard WHIRL Lowering Contract

## Purpose

This contract defines the generic `common/com` transaction used by an admitted
domain lowering pass to replace executable native DSL definitions with standard
WHIRL statements.  It is the IR mutation boundary needed by FHE SYNC-5, but it
is intentionally not FHE-specific.  Domain policy remains in the owning VHO
pass; this service validates identities, ownership, effects, runtime-handle
relations, source positions, and atomic tree/image mutation.

The transaction does not add a mapped-image section or change an existing row
size.  Existing DSL node and value rows remain immutable logical provenance.
Their previously reserved flag words record that the corresponding native WN
definition has been lowered out of the executable tree.

## Representation

The following append-only flag values are assigned:

| Record | Flag | Meaning |
| --- | --- | --- |
| `DSL_IR_NODE_RECORD` | `DSL_IR_NODE_FLAG_LOWERED` | The logical node remains as provenance, but its native WN definition is no longer executable. |
| `DSL_IR_VALUE_RECORD` | `DSL_IR_VALUE_FLAG_LOWERED` | The logical value is represented at runtime by exactly one reviewed relation. |

`LOWERED` is mutually exclusive with the existing `RETIRED`/`REDIRECTED`
state.  A lowered node must be pure, own the lowered result value, and have no
live logical consumer.  Lowered nodes are excluded from
`DSL_IR_Image_Executable_Node_Count()`.

The runtime relation is a tagged union derived from existing managed tables:

1. `runtime_value_projection`: a computed logical value maps to exactly one
   `DSL_RUNTIME_VALUE_PROJECTION_RECORD` with `LOCAL_VALUE` binding.
2. `root_promoted_input`: an external tensor constant maps to exactly one
   `DSL_RUNTIME_INPUT_RECORD(SOURCE_EXTERNAL_TENSOR)` and one matching
   `DSL_RUNTIME_INPUT_BINDING_RECORD(ROOT_PROMOTED_SOURCE)`.

Zero matching relations and simultaneous projection/input relations are both
invalid.  No private metadata string is used to reconstruct the relation.

## Transaction API

`DSL_IR_Lower_Native_Values_To_Standard_Blocks()` accepts a complete request
array for one active PU through a `PU_Info *`. The `PU_Info` is the sole
ownership and tree authority; the lowering service derives the active PU root,
each native definition's containing block, and the final result-handle
definition from `WN_last(standard_block)`. Requests therefore cannot provide
conflicting roots, parent blocks, or result STIDs. Request order is not
semantic; commit order follows the physical definition order in the PU tree.

The transaction is implemented in
`osprey/common/com/dsl_ir_lower.cxx`. Generic value and payload transactions
remain in `dsl_ir_rewrite.cxx`; runtime projection planning and reconstruction
belong to `dsl_runtime_interface.cxx`; program-wide ABI retirement, input
promotion, and final interface verification belong to
`dsl_program_interface.cxx`. Standard-WHIRL replacement legality, preflight,
commit ordering, and post-verification belong exclusively to the lowering
file. Private active-PU predicates and runtime-interface journals are shared
only through `dsl_ir_transaction_internal.h` and
`dsl_runtime_interface_internal.h`; verified-dead program inputs use the
private batch REGION interface transaction in `dsl_region_internal.h`. None of
these headers is a producer API. The runtime API uses `PU_Info *` as its single
physical context. This source-level ownership split does not change binary
WHIRL.

### Computed Standard Block

`DSL_IR_NATIVE_LOWER_COMPUTED_STANDARD_BLOCK` requires:

- one native, pure DSL definition and exact logical operator/version;
- one detached standard-WHIRL `BLOCK`;
- no native DSL WN or source tensor-result ST use in that block;
- exact source position on every inserted statement;
- exactly one final `STID` to the relation's existing projected handle ST/TY;
- complete closure of physical and logical consumers within the same request
  array, except admitted call-interface projections.

On commit, the standard statements are inserted immediately before the native
definition, the native definition is removed, and the logical node/value rows
are marked lowered.

### Promoted Source Elision

`DSL_IR_NATIVE_LOWER_PROMOTED_SOURCE_ELISION` requires an exact external tensor
constant, its TCON evidence, and its unique root-promoted runtime input/binding.
It accepts no replacement block and no result `STID`.  Commit removes only the
executable native tensor-constant definition; the promoted formal and logical
source evidence remain.

This mode supports entry-owned weight and bias values without inventing a fake
callsite or a computed-value projection.

## Preflight And Atomicity

The complete request array is preflighted before any tree or table mutation.
Preflight rejects:

- inactive/wrong PU ownership or a definition outside the supplied PU tree;
- unknown, mismatched, effectful, already retired, or already lowered nodes;
- non-unique result ownership, address-taking, non-prunable REGION interface
  use, state effects, or another physical definition;
- unlowered physical/logical consumers outside the transaction;
- missing, ambiguous, owner-mismatched, ST/TY-mismatched runtime relations;
- duplicate values, nodes, definitions, replacement blocks, or result STIDs;
- replacement statements with missing/wrong source positions;
- a replacement block that writes the wrong handle or does not end in the
  single designated handle `STID`.

After preflight, commit consists only of operations whose inputs and targets
were validated.  Failed preflight leaves the WN tree and every managed table
unchanged.  Postconditions are assertions because a failure after commit would
indicate an internal compiler defect, not a recoverable input error.

Verified-dead formal retirement may remove matching REGION rows only through a
complete batch transaction. Every removed row must be a plain
`DSL_REGION_VALUE_INPUT` with no state flags; output, result, inout, abstract
state, or ownership rows reject the entire batch. Preflight copies the complete
interface set, removes the requested dead inputs, and runs the normal generic
and profile-specific REGION verifier before any mutation. Commit swaps only
the verified interface vector. REGION nodes, live interfaces, metadata, source
positions, and retired-formal provenance remain intact. Final per-PU
program-interface verification requires each retired symbol to be absent from
both executable WN and the loaded REGION interface.

Each caller-owned result handle passed through a by-reference output `PARM`
must be initialized by exactly one canonical null `STID` in the contiguous
statement sequence immediately before its call. The initializer uses the
handle's exact TY and machine type and inherits the call source position. The
complete call/result set is proven during the existing no-mutation preflight;
commit inserts the initializers with the rebuilt call before retiring the old
call. Per-PU and mapped-image verification require both the output `LDA` and
its matching null initialization, so a missing, nonzero, mistyped, duplicated,
or source-position-mismatched initializer fails closed.

## Mapped Image And Compatibility

The binary representation remains DSL image version 1.  New readers validate:

- known and mutually exclusive node/value flags;
- pure lowered nodes paired with lowered result values;
- no live logical reference to a lowered value;
- exactly one structurally valid runtime relation per lowered value.

Program-interface evolution is applied in PU scope because the backend reads,
mutates, and writes one PU at a time. Process-local commit evidence therefore
controls reader-side physical validation during the transaction: a PU becomes
eligible only after its own `DSL_Program_Interface_Apply_PU()` succeeds. Rows
already published for an earlier PU do not cause an untouched later PU to be
validated against its future interface. The eligibility set is reset with the
managed image and is never written to binary WHIRL. Loading a published mapped
program-interface image marks the complete image committed, restoring strict
per-PU validation for every PU during reopen. This ordering is implemented by
the private `dsl_program_interface_internal.h` service and does not weaken the
mapped-image compatibility gate.

The normal reader loads the DSL image and the existing runtime/program
interface pair, then validates the cross-section relation.  A current reader
reopens and prints the artifact.  An immediately previous reader may reopen the
physical WHIRL sections but must fail closed when it encounters the formerly
reserved lowered flag; it must not silently treat the removed native node as
executable.

`ir_b2a -st -src` prints logical evidence without exposing physical
`OPR_DSL` storage details:

```text
status=lowered relation=runtime_value_projection projection=<id>
status=lowered relation=root_promoted_input runtime_input=<id> runtime_binding=<id>
```

The executable tree remains ordinary WHIRL and preserves source-line evidence
for every inserted statement.

## Ownership Boundary

`common/com` owns only the generic representation, validation, transaction,
mapped-image compatibility, and logical printing described here.  The FHE VHO
pass owns:

- selection of admitted FHE operations;
- exact runtime ABI calls and arguments;
- ciphertext/plaintext policy and CKKS-state legality;
- construction of each detached standard-WHIRL block;
- complete-PU request collection and invocation;
- post-lowering unlowered-node diagnostics.

The generic service does not choose providers, runtime symbols, algorithms,
keys, levels, scales, or lowering schedules.

## Certification

The focused producer test contains one PU with all of the following in one
transaction:

- a two-node computed `common.add.v1` chain;
- rank-4 external weight and rank-1 external bias values;
- two computed runtime projections;
- two root-promoted input relations;
- deliberately permuted requests to prove deterministic tree-order commit.

The test proves failed partial-chain preflight leaves the tree unchanged,
persists the successful artifact through the normal mapped-image writer, and
reopens it with `ir_b2a -st -src`.  The retained `.T` must show four lowered
relations, two standard result-handle `STID`s, both promoted formals, the
rank-4/rank-1 tensor descriptors, and no executable native definitions for the
four lowered sources.

The separate two-PU program-interface fixture applies the caller first and
proves only that PU is commit-eligible, applies the callee second, then writes
and reopens the completed artifact. The current reader must validate both PUs
on mapped reopen; malformed or incomplete published interfaces still fail
closed. The trace also retains the caller-owned result-handle zero
initializations immediately before both rewritten calls.
