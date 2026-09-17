# WHIRL DSL Tensor Shape Retyping Contract

## Status

Accepted SP4 design contract for the first shape-refinement mutation slice.
This document closes the atomic-retyping design gate in
`WHIRL-DSL-SHAPE-PROPAGATION-DESIGN.md`. It defines the protocol that SP5 may
implement. It does not add a public API, mutate WHIRL, or change a mapped-image
layout.

The first implementation is intentionally narrower than the complete shape
propagation architecture. It retypes only uniquely owned local tensor results.
Formal, return, call, constant, and cross-PU refinement remains check-only
until SP6 or a separately reviewed extension.

## Decision

Canonical tensor types are immutable. A shape proof may select a different
canonical `TY_IDX`, but it must never edit the old `TY` or tensor descriptor in
place. Retyping changes the association between one logical value and a
canonical type.

The first mutation service shall use a complete request-array transaction:

1. Validate every request and every affected WHIRL projection.
2. Build a rollback journal without changing the program.
3. Reject the complete array if any request is unsupported or inconsistent.
4. Commit only after complete-array preflight succeeds.
5. Run strict post-verification.
6. Restore every changed reference if commit or post-verification fails.

No public helper may update only a WN, ST, DSL value row, REGION relationship,
or other table. Table-only commit helpers remain private to the transaction
implementation.

## Request Contract

The proposed runtime-only request row is:

```c++
typedef struct {
    ST_IDX owner_pu_st;
    DSL_IR_VALUE_ID value_id;
    TY_IDX expected_old_ty;
    TY_IDX refined_ty;
} DSL_IR_TENSOR_RETYPE_REQUEST;
```

The exact API spelling remains private until SP5 review, but these fields and
their meanings are fixed for the first implementation:

- `owner_pu_st` is the global function symbol for the active owner PU.
- `value_id` is the stable logical value identity in the managed DSL image.
- `expected_old_ty` protects the transaction from stale analysis results.
- `refined_ty` is an already interned, sealed canonical tensor type.

The transaction does not intern types. Shape analysis and the canonical type
service produce `refined_ty` before retyping preflight begins. An equal old and
new type is not a mutation request and shall be omitted by the planner.

Canonical type interning is a monotonic service outside the reference-update
transaction. A failed terminal compiler phase may leave an unreferenced
canonical type in memory, but it may not publish an output artifact and may
not leave any WN, ST, or managed-image reference changed. A subsequent request
must reuse that equivalent type rather than allocate another one.

## Semantic Authority

The canonical `TY` plus its TensorDescriptorIR extension is authoritative for
the meaning of one `TY_IDX`. It is immutable after sealing.

For a logical DSL value, the managed `DSL_IR_VALUE_RECORD` is the stable value
identity anchor. The owner PU and local `ST` are the physical storage anchor.
Neither is trusted in isolation during preflight. The expected old type must
agree across every explicit type-bearing projection before any change occurs.

References that contain only `DSL_IR_VALUE_ID` or `ST_IDX` derive their type
through the value or symbol. They are validated for identity and ownership but
are not rewritten when the ID remains unchanged.

## Reference Inventory

| Projection | Type evidence | SP5 local-result action |
| --- | --- | --- |
| Canonical tensor `TY` and descriptor | `TY_IDX` identity | Never mutate; validate old and refined types. |
| DSL logical value | `DSL_IR_VALUE_RECORD.ty` | Rewrite through a private commit helper. |
| Result symbol | `ST_type` | Rewrite while the owner local symtab is active. |
| Defining assignment | `WN_ty(OPR_STID)` | Rewrite the one native result definition. |
| Direct reads | `WN_ty(OPR_LDID)` | Rewrite every exact read of the result ST. |
| Native DSL expression | `MTYPE_M`, logical opcode, operands | Validate producer and operands; no tensor `TY_IDX` field to change. |
| DSL node result link | `result_value_id` | Validate; ID-only row remains unchanged. |
| DSL operand references | `value_id` | Validate users and physical LDIDs; ID-only rows remain unchanged. |
| REGION interface | `ST_IDX` | Validate owner and row; type derives from the rewritten ST and the row stays byte-identical. |
| Call ABI row | `argument_value_id` plus physical pointer types | Reject participation in v1. |
| PU formal image | `formal_ty`, `formal_st`, `formal_value_id` | Reject participation in v1. |
| Function prototype and `TYLIST` | return, input, and hidden-result types | Reject participation in v1. |
| Hidden result formal and call output | pointer-to-result type | Reject participation in v1. |
| Tensor TCON and external tensor reference | `descriptor_ty` and payload size | Reject constants and external-data results in v1. |
| FHE tensor binding | `tensor_ty` | Reject when the old type has an FHE tensor binding. |
| FHE or domain rows keyed by value ID | value identity plus domain invariants | Reject unless the extension registers a reviewed shape-retype participant. |
| State/effect image | node and state IDs | Producer must be pure and have no state-effect rows. |
| Source, lineage, and compiler metadata | IDs and strings | Preserve unchanged; none participates in type identity. |

This inventory is exhaustive for the current managed DSL infrastructure.
Future optional images that duplicate a type or impose shape-dependent
semantics must register with the transaction before values referenced by that
image can be retyped.

## SP5 Eligibility

Every request in the first implementation must satisfy all of these rules:

1. The supplied PU is active and `owner_pu_st` identifies that exact PU.
2. `value_id` names one live `DSL_IR_VALUE_OPERATOR_RESULT` owned by the PU.
3. The value has one local `CLASS_VAR`, `SCLASS_AUTO`, temporary result ST.
4. The result has unique tensor ownership and is not address-saved or
   address-passed.
5. The value has exactly one `OPR_STID` definition whose kid is one native,
   registered, pure logical DSL operator.
6. The value is not redirected or retired and its producer has no state-effect
   rows.
7. Every executable use is a direct `OPR_LDID` of the exact result ST. Any
   `OPR_LDA`, second write, indirect use, alias escape, or unclassified
   symbol-bearing WN rejects the request.
8. The value does not participate in a call actual, PU formal, hidden result,
   function return, function prototype, or `TYLIST` relationship.
9. The value is not a tensor constant, model input, external-data value, or
   TCON-backed value.
10. The value and old type are not covered by an auxiliary type binding or
    shape-dependent domain image without a registered transaction participant.
11. `expected_old_ty` exactly equals the DSL value type, result ST type,
    defining STID type, and every direct LDID type.
12. Both types are sealed canonical tensor types. `refined_ty` preserves the
    complete canonical descriptor except for shape-core facts justified by
    the solver.
13. The element type, dtype, tensor kind, traits, layout, sharding, placement,
    memory, quantization, alignment contract, and every other non-shape
    canonical field are exactly equal.
14. The request array has no duplicate value, duplicate result ST, or
    conflicting expected/refined type pair.

`common.tensor_const` and `common.model_input` are seed authorities. They are
checked against inference but are not rewritten by the first mutation slice.

## REGION Policy

A current `DSL_REGION_INTERFACE_RECORD` carries an `ST_IDX`, roles, ordinal,
and flags, but no duplicate `TY_IDX`. A supported local result may therefore
cross a managed REGION interface without rewriting that row.

Preflight must enumerate every interface row for the result ST, prove that the
row belongs to the active PU and current REGION image, and retain its identity
in the journal. Post-verification must prove the same rows still refer to the
same ST and that `DSL_Region_Verify_PU()` succeeds with the new `ST_type`.

Retyping does not change REGION ownership, input/output role, ordinal, unique
ownership, state role, or contract version. A REGION profile that adds an
explicit type or shape constraint later becomes a registered transaction
participant; until then, an unsupported profile dependency rejects.

## Preflight Protocol

SP5 shall perform the following steps without mutation:

1. **Normalize requests.** Sort by `(owner_pu_st, value_id)`, reject invalid
   IDs, duplicates, no-op requests, and conflicting result symbols.
2. **Validate owner.** Prove the active `Current_pu`, `Current_PU_Info`, local
   symtab, supplied PU, and global owner function ST all agree.
3. **Validate canonical types.** Check old and refined tensor descriptors,
   canonical sealing, exact non-shape equivalence, and legal monotonic shape
   refinement.
4. **Resolve logical identity.** Resolve the value, producer node, opcode
   descriptor, result link, local ST, and one native STID definition.
5. **Scan the complete physical tree.** Classify every WN that carries the
   result ST. Record the defining STID and all LDID reads. Reject LDA, a second
   definition, pointer materialization, unknown symbol-bearing operators, and
   uses outside the supplied PU tree.
6. **Scan managed DSL rows.** Validate the value record, node result link,
   operand-reference users, effects, call ABI, PU interface, and call/result
   relationships.
7. **Scan constants and extensions.** Reject TCON/external constants and any
   auxiliary image requiring an unregistered update, including an FHE tensor
   binding on the old type.
8. **Scan REGION interfaces.** Record every ID-only interface row and prove it
   derives the type through the exact result ST.
9. **Build the journal.** Record each writable address, record kind, owner,
   expected old `TY_IDX`, and refined `TY_IDX`. Reserve all storage now; commit
   must allocate nothing.
10. **Cross-request validation.** Prove no journal entry is written twice and
    no request changes a projection validated as immutable by another request.

Any failed step rejects the entire request array and leaves all structures
unchanged.

## Commit And Rollback

The compiler is single-threaded while the transaction runs. No verifier,
printer, callback, or pass may observe the intermediate commit state.

After complete preflight, commit applies only journaled fixed-width `TY_IDX`
updates:

1. direct LDID `WN_ty` fields;
2. the defining STID `WN_ty` field;
3. the local result `ST_type` field;
4. the private managed DSL value `ty` field.

The order is an implementation detail because no observer may run during the
commit. The journal nevertheless records the old value for every write.
Commit must not allocate, intern a type, grow a table, change an ID, or invoke
an extension callback that can discover new work.

Strict post-verification then runs:

- complete DSL image validation;
- active-PU gatekeeper validation;
- physical WN/ST/value agreement;
- REGION verification;
- the shared shape checker in strict mode;
- request-specific proof that every journaled projection now has
  `refined_ty` and every unrelated value still has its original type.

If a commit write or postcondition fails, rollback restores journal entries in
reverse order and reruns the old-state structural checks. The transaction
returns failure only after the old program is restored. A rollback failure is
an internal compiler error and the compiler must terminate without publishing
an artifact.

## Per-PU Atomicity And Driver Ownership

SP5 implements one active-PU transaction. Its success or failure is fully
atomic in memory for that PU.

The backend driver already processes one PU at a time and restores the correct
local symbol table before calling the VHO phases. SP6 therefore does not add an
all-PU shape transaction. For each PU, refinement follows this contract:

1. the driver selects and loads the PU;
2. the shape service solves the active PU before mutation;
3. the SP5 transaction preflights and commits that PU atomically;
4. strict gatekeeper and REGION checks run before downstream DSL phases;
5. failure rolls back the active PU and terminates compilation;
6. normal driver traversal repeats the process for the next PU.

Call ABI and PU-interface records are boundary contracts, not permission for
the shape service to reactivate or mutate another PU. A future transformation
that changes multiple PU signatures must define its own coordinated mutation
contract. Failed compilation must not publish a validly named output artifact.

REGION follows the same ownership rule. The backend driver owns REGION
initialization, traversal, and finalization for the selected PU. The local
retype transaction may validate an interface row and update the ST from which
that row derives its type, but it does not own REGION processing and may not
visit another PU's REGION state.

## Shared Symbols And Shared Callees

Sharing one immutable old `TY_IDX` does not imply shared value ownership. A
supported local result receives `refined_ty`; unrelated values continue to
reference the old type.

The first transaction rejects:

- one ST represented by multiple logical values;
- one logical value represented by multiple result STs;
- global, common, static, formal, or externally visible result symbols;
- context-specific demands on one callee formal or hidden result;
- any request that would require a caller actual, pointer type, PU prototype,
  or `TYLIST` change.

For shared callees, the solver remains check-only. Compatible contexts may be
proved without mutation. Incompatible contexts must preserve an already valid
symbolic type, use a separately approved clone/specialization mechanism, or
fail closed. SP5 must not silently choose one caller's shape.

## Diagnostics

SP5 should reserve stable diagnostics in this family:

| Diagnostic | Meaning |
| --- | --- |
| `DSL-SHAPE-RETYPE-001` | Malformed, duplicate, or no-op request. |
| `DSL-SHAPE-RETYPE-002` | Owner PU or active local symtab mismatch. |
| `DSL-SHAPE-RETYPE-003` | Expected old type disagrees with a projection. |
| `DSL-SHAPE-RETYPE-004` | Refined type changes a non-shape canonical field. |
| `DSL-SHAPE-RETYPE-005` | Unsupported physical use or alias escape. |
| `DSL-SHAPE-RETYPE-006` | Unsupported formal, call, return, constant, or auxiliary relation. |
| `DSL-SHAPE-RETYPE-007` | Conflicting requests or shared-callee demand. |
| `DSL-SHAPE-RETYPE-008` | Commit postcondition failed and rollback ran. |

Diagnostics should include owner PU, value ID, ST name/index, old and refined
`TY_IDX`, logical operator/version, and the rejecting projection when known.
They must use logical DSL operator names and must not expose physical
`OPR_DSL` escape encoding.

## Compatibility And Inspection

The first transaction adds no opcode, flag, record, ELF section, or binary
layout. It changes existing `TY_IDX` references only after preserving all
existing reader/writer invariants.

The old canonical type remains in the type table. The refined canonical type
is written through the existing TY and tensor descriptor tables. Existing WN,
ST, DSL value, and REGION rows retain their physical sizes and IDs. A previous
reader therefore sees an ordinary, internally consistent tensor type
association and does not need to understand that shape propagation selected
it.

`ir_b2a -st -src` must show the refined tensor descriptor in the type and
symbol evidence and the same logical DSL node/value identity as before. The
transaction must not add a private trace-only marker to the binary image.

Before/after phase traces should report the owner PU, value ID, old type,
refined type, old shape, refined shape, and proof source. Compiler metadata and
source positions remain unchanged.

## SP5 Test Matrix

### Positive cases

1. Retype one uniquely owned local native result and all of its direct LDIDs.
2. Leave an unrelated value that shares the old `TY_IDX` unchanged.
3. Reuse an already interned equivalent refined type without `Ty_tab` growth.
4. Retype two independent values in one request array.
5. Preserve a local REGION interface row byte-for-byte while its ST derives the
   refined type.
6. Write, reopen, and inspect the artifact with `ir_b2a -st -src`.

### Preflight rejection cases

1. Invalid owner, value ID, old type, refined type, or inactive PU.
2. Duplicate value, duplicate ST, no-op, or conflicting request.
3. Stale expected old type in the value, STID, ST, or one LDID.
4. Refined dtype, element type, layout, placement, memory, quantization,
   trait, alignment, or representation mismatch.
5. Multiple definitions, LDA/address-taken use, indirect use, alias escape, or
   unknown symbol-bearing WN.
6. Impure producer or state-effect row.
7. Tensor constant, model input, external payload, or TCON-backed value.
8. Call actual, call output, PU formal, hidden result, function return,
   prototype, or `TYLIST` participation.
9. FHE tensor binding or unregistered auxiliary image dependency.
10. Cross-PU owner mismatch and two PUs with colliding local `ST_IDX` values.
11. Inconsistent or unsupported REGION profile dependency.
12. Context-sensitive shared-callee conflict.

Every rejection test snapshots WN, ST, DSL image, REGION image, and type
references before the call and proves byte-for-byte reference equality after
failure.

### Commit and rollback cases

1. Inject a failure before the first write and prove no mutation.
2. Inject a failure after each journal write position and prove full rollback.
3. Inject strict gatekeeper, shape-checker, DSL-image, and REGION
   postcondition failures and prove restoration.
4. Fail the last request in a multi-request array and prove earlier requests
   are restored.
5. In SP6, fail a later PU and prove every earlier PU is restored.
6. Prove no validly named `.B` or partial temporary artifact is published on
   any failure.

## Implementation Ownership

SP5 implementation belongs in `osprey/common/com/dsl_ir_rewrite.cxx`, with
the complete transaction as the only supported mutation surface. Any image or
REGION setters required by commit remain private translation-unit services.

`osprey/be/vho/dsl_shape_refine.cxx` owns solver-result selection, request
construction, active-PU invocation, tracing, and strict phase verification. It
does not edit WN, ST, or managed tables directly.

The shared `osprey/common/com/dsl_shape.cxx` engine owns shape proof and
monotonicity checks. It does not own physical mutation.

## SP4 Exit Decision

SP4 approves the protocol above for SP5 implementation with these boundaries:

- local uniquely owned native operator results are supported;
- REGION rows that derive type through the same ST are supported without row
  mutation;
- formals, calls, returns, constants, function types, `TYLIST`, cross-PU
  changes, and unregistered auxiliary images remain check-only;
- complete-array preflight and rollback are mandatory;
- no public table-only mutation helper is permitted;
- mapped-image layout and previous-reader behavior remain unchanged.
