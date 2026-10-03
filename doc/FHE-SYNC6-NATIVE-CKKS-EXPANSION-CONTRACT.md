# SYNC-6 Native CKKS Expansion Contract

Status: staged main/common contract. The nine logical CKKS v1 operators and
their unchanged mapped DSL image carrier are implemented in
`osprey/common/com/dsl_opcode.{h,cxx}`. The optional typed event image,
mapped reader/writer, gatekeeper, and `ir_b2a -st -src` inspection are
implemented. A focused image fixture proves two executable steps and one
lowered source. `DSL_IR_Can_Expand_Native_Value_To_CKKS_Events` checks a
complete request without reserving symbols or mutating the PU. The staged
`DSL_IR_Expand_Native_Value_To_CKKS_Events` implementation now exercises a
six-group cross-group dependency, explicit final replacement, a late failure
after four groups, call-ABI and REGION rollback, and separate-process mapped
reopen. These are structural infrastructure fixtures, not the FHE semantic
producer or a certified ResNet artifact. CKKS state binding and full ResNet
certification remain pending.

## Boundary

The producer runs in the active PU before terminal standard-call lowering. It
must use a common/com transaction, not `DSL_Builder_*`, raw `OPR_DSL` fields,
or direct table insertion. The transaction takes one existing executable
native DSL definition and an ordered array of static-event groups. Each group
has a nonzero source static ordinal and an ordered list of replacement logical
steps. A step has direct operands from either an existing owner-PU DSL value
or an earlier step in the same atomic request, including a step in an earlier
event group. Every result has its own canonical tensor TY, result ST, native
STID, DSL node/value/reference rows, and source position. A TY is never
mutated to express a new CKKS level or scale.

The request must identify the expected source node/value/operator/version,
the active PU, every source static event, one or more call contexts that share
the same executable state/schedule signature, and the exact group/step whose
result replaces the original source result. Each persisted event identity is:

```text
(owner_pu_st, source_value_id, context_pu_identity_id,
 context_callsite_id, source_static_ordinal)
```

`source_static_ordinal` is nonzero. It distinguishes separate static
evaluation events that share one source value and call context, including the
six consecutive ReLU-evaluation events in the accepted SYNC-5 schedule.
Within that exact event key, step ordinals are zero-based, unique, and dense
`0..N-1`, matching the existing FHE materialization and coverage convention.
The ReLU refresh, normalize, three polynomial stages, and reconstruction are
six groups in one source-definition transaction; the source is retired only
after all groups and their cross-group operands preflight. The final
replacement value is named explicitly, not inferred from array position or
operator name.

## Atomicity And Ownership

1. Before mutation, prove that `PU_Info` owns the active local symtab and the
   source STID/image value, that the expected logical opcode/version matches,
   and that the source has unique no-alias result ownership.
2. Preflight all event groups, strictly increasing nonzero static ordinals,
   zero-based dense per-group step ordinals, step operators, exact static
   attribute schemas, canonical TYs, owner-PU operand values, prior-step
   references across groups, result names, and nonzero source positions.
   Reject forward references, cycles, and an unmaterialized final selection.
3. Preflight the complete source-use redirection, including physical LDIDs,
   return/call actuals, REGION interfaces, managed value references, and
   effect relationships. Reject unsupported address-taken or escaping uses,
   conflicting writes, and a source whose effect model is not replaceable.
4. Construct detached physical nodes and commit the ordered result STIDs,
   logical image rows, use redirection to the explicitly selected final
   result, and one source retirement as one active-PU transaction. A rejected
   request leaves the tree, symbols, and managed tables unchanged. Do not
   expose an image-only persistent-edit API.
   The commit implementation must have an explicit undo path for every
   fallible post-preflight operation, including call ABI, REGION, physical
   WN links, symbols, and managed rows. A late verification failure must
   return through that rollback path; a post-mutation assertion is not an
   atomicity mechanism. The read-only preflight is separately testable but
   cannot reserve names or prevent another mutation between calls.
   Rollback savepoints include string interning, local symbols, tensor
   metadata/KV rows, DSL opcode/node/attribute/value/reference rows, and
   CKKS event rows. Physical uses, call-ABI arguments, REGION interfaces,
   and the detached source definition are restored before those savepoints
   are trimmed. A failed rollback is terminal rather than returning control
   to a pass with uncertain IR.
5. Retain the old source node/value as nonexecutable provenance with the
   existing `DSL_IR_NODE_FLAG_LOWERED` and `DSL_IR_VALUE_FLAG_LOWERED` pair.
   The original source operand rows remain intact. The operand-targeted
   `RETIRED`/`REDIRECTED` flags are not suitable for a newly generated final
   value and must not be repurposed. No backend pass may count the source as
   a second CKKS operation. A failed later CKKS state bind is terminal for
   the checkpoint: no retry in the mutated PU and no final `.B` publication.
6. The transaction returns every new node/value/ST identity by event group
   and step ordinal, not WN layout details, so the FHE pass can bind existing
   per-value CKKS state and verify key, rotation, level, scale, precision,
   components, and slots separately.

Context identity follows the callee/source-definition PU. For a called
context, the callsite's callee is `owner_pu_st`, while its caller remains an
independently valid function ST. The same source PU/value may participate in
multiple call contexts. Equal executable state/schedule signatures may reuse
one physical WN sequence while retaining separate context-event relation
rows; different signatures require deterministic whole-PU specialization or
a separately proved parameterized ABI. A specialized clone must retain a
typed link from each cloned static ordinal to its original source ordinal;
neither call order nor a renamed source symbol is that proof.

## Persisted Event Relation

Existing planning rows do not provide a typed join from every high-level
static event to each executable CKKS result. In particular,
`DSL_FHE_MATERIALIZATION_OPERATION_RECORD` is ReLU-context-specific and has
`operation_ordinal`, but no source static ordinal or executable result
value/node ID. `DSL_IR_VALUE_RECORD.metadata` is an untyped string and is
not a substitute. The initial table audit therefore calls for a reviewed
append-only optional ELF relation with exact row size, 8-byte alignment,
capability/version, and reserved-field validation. Before allocating a
section number, check the remaining program/runtime interface rows for a
semantically equivalent typed join; do not overload an existing row's
published meaning. The relation is metadata about executable values, not a
parallel WN or a replacement for direct kids.

The accepted v1 candidate is `WT_DSL_CKKS_EVENT` (the next unused optional
WHIRL section, `0x2c`), with a 32-byte header and 64-byte fixed row. The row
contains, in order, 32-bit `id`, `owner_pu_st`, `source_value_id`,
`source_node_id`, `context_pu_identity_id`, `context_callsite_id`,
`source_static_ordinal`, `step_ordinal`, `result_value_id`, `result_node_id`,
`origin_owner_pu_st`, `origin_source_value_id`, `origin_static_ordinal`,
`flags`, and two zero-validated reserved words. The sole v1 flag marks the
one final source replacement per exact source/context across all its event
groups. In an unspecialized PU, origin owner/value/ordinal equal source
owner/value/ordinal. A specialized PU uses the origin triplet as a typed
link to the pre-clone source event. Unknown flags and nonzero reserves fail
closed; no source name, payload path, or runtime pointer enters the row.

Mapped-image validation must prove source and result IDs exist, result nodes
are executable logical CKKS operators owned by the stated PU, the retained
source is `LOWERED` and nonexecutable with no live WN or DSL uses, context
routes are valid, and ordinals
are unique and dense per event. At `-O0`, one result value cannot claim two
different static events within the same exact context. It must not demand
global uniqueness of a result value across different call contexts when a
reviewed same-signature shared PU is reused. The FHE gatekeeper separately
proves complete coverage against the independently counted source-event set,
including all 147 accepted dynamic events; common/com must not hard-code that
model-specific count.

`LOWERED` has two exclusive relation families: existing runtime-handle
lowering and CKKS event expansion. A lowered source cannot claim both. The
reader loads this optional event image before validating legacy runtime
relations; images without the section retain the old path. A previous
reader of a CKKS-bearing image fails closed at the unknown logical opcode
descriptor. The first retained image fixture removes a source ReLU WN and
records encode/refresh results directly to certify the image contract; it
does not stand in for the atomic producer API required below.

## Review Gates

- A six-group same-PU ReLU fixture proves cross-group prior-step operands,
  direct WN kids, separate result ST/value IDs, one source lowering,
  redirection to reconstruction, and `ir_b2a -st -src` inspection after
  mapped reopen.
- Negatives cover wrong active PU with colliding local ST indices, wrong
  expected source, wrong TY/owner, forward step reference, duplicate or zero
  static ordinal, duplicate or skipped zero-based step ordinal, a result
  claimed by two static events in one context, and a late REGION/use
  conflict. Rejection must leave WN, symbol, and image counts unchanged.
- A two-context fixture proves both same-signature reuse and different-state
  specialization without collapsing source-event identities.
- The accepted ResNet source-event census requires at least nine fixed-schedule
  executable PU variants. This count assumes normalization bound `B` is a
  typed plaintext formal/actual; if a static bound must specialize the body,
  a tenth variant may be required. Whole-PU cloning, formal append, caller
  actual rewrite, and managed-image re-ownership form a separate generic
  transaction; `IPO_CLONE` alone does not supply that contract.
- Old WHIRL remains readable. An older reader of the new optional relation
  must either ignore it without misinterpreting executable CKKS nodes or fail
  closed at the DSL gatekeeper; document and test the observed behavior.
- Full `.ckks_ops.B` certification and terminal C ABI lowering remain FHE
  task gates after the shared transaction and relation land.
