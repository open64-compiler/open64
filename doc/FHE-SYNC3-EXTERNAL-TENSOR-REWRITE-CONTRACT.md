# FHE SYNC-3 External Tensor Rewrite Contract

## Purpose

This contract closes the main/common payload boundary needed for real
BatchNorm-to-Conv folding. It provides backend-safe typed access to existing
external tensor constants and an owner-aware batch transaction for publishing
converted side-file tensor values and redirecting call actuals.

The service is generic DSL infrastructure. It does not depend on FHE semantic
code, `DSL_Builder_*`, Python, torch2whirl, backend code generation, or a
specific side-file implementation.

## Published APIs

```c++
BOOL DSL_IR_Image_Get_External_Tensor_Reference(
    ST_IDX owner_pu_st,
    DSL_IR_VALUE_ID value_id,
    DSL_IR_EXTERNAL_TENSOR_REFERENCE *reference);

BOOL DSL_IR_Materialize_External_Tensor_Values(
    ST_IDX owner_pu_st,
    const DSL_IR_EXTERNAL_TENSOR_MATERIALIZATION_REQUEST *requests,
    UINT32 request_count,
    DSL_IR_EXTERNAL_TENSOR_MATERIALIZATION_RESULT *results);
```

`DSL_IR_Image_Get_External_Tensor_Reference()` returns a borrowed typed view of
the existing DSL value, ST metadata, and canonical tensor TY. It validates:

- selected and owning PU identity;
- `common.tensor_const.v1` and `external_data` semantics;
- result value, result ST, and canonical `TY_IDX` consistency;
- storage format, side-file path, tensor key, byte range, and checksum syntax;
- dtype, rank, logical shape, layout, placement, and memory descriptor facts;
- exact static tensor byte size and element-aligned byte offset; and
- agreement between the structured fields and the logical storage URI.

Callers receive structured fields and do not parse ST metadata or payload
strings themselves. The returned strings remain owned by the existing Open64
string and tensor tables. A caller must not retain those borrowed fields across
owner-PU table mutation, mapped-image reset, or program reset.

Checksum validation at this common/com boundary covers syntax and agreement
between structured metadata and the logical storage URI. It does not read the
side file or prove that the checksum describes its current payload bytes. The
FHE producer and semantic gatekeeper own payload digest verification.

## Batch Materialization

`DSL_IR_Materialize_External_Tensor_Values()` preflights the complete request
array before creating a symbol, WN, image row, or call replacement. Each
request names an existing owner-PU tensor-constant source and creates one
caller-owned `common.tensor_const.v1` value with:

- the exact canonical tensor `TY_IDX`;
- a side-file dense tensor TCON whose descriptor, path, range, and logical byte
  size agree with the external tensor reference;
- source position on the new STID and result symbol;
- copied source/instance/context metadata where a source value exists;
- `dsl.converted_from_value_id` provenance;
- immutable original source payload and source value; and
- stable logical `ir_b2a -st -src` evidence.

The request's named `source_policy` is fail-closed. Its zero/default value,
`DSL_IR_MATERIALIZE_SOURCE_EXTERNAL_ONLY`, preserves the original contract.
`DSL_IR_MATERIALIZE_SOURCE_EXTERNAL_OR_IMPLICIT_ZERO` additionally admits an
exact, pure `common.tensor_const.v1` constant whose value kind is
`implicit_zero`. The implicit-zero value must be the existing physical actual,
have the exact owner and canonical `TY_IDX`, and agree with the output's dtype,
shape, and byte length. It legitimately has no source checksum. The resulting
external-data value must have a nonempty checksum, valid tensor TCON, and valid
side-file byte range. `dsl.converted_from_value_id` continues to identify the
implicit-zero source; BatchNorm inputs remain separately identified by the
fold-provenance row.

When `call` is non-null, the request must identify one existing read-only,
passed-not-saved, by-reference actual and its expected source value. Commit
replaces that actual with the new same-`TY_IDX` value. When `call` is null, the
new value is inserted at the supplied entry-owned anchor without inventing a
fake callsite. This supports the ResNet stem Conv/BatchNorm context.

Duplicate names and duplicate call-actual targets in one transaction are
rejected. Invalid owner, type, range, checksum, TCON, insertion anchor, or
expected actual rejects the entire array during preflight. The focused test
proves that a failure in the second request leaves DSL node/value counts, the
local ST table, and the earlier call actual unchanged.

## Shared-PU Rule

For the certified ResNet profile, BatchNorm folding changes payload bytes but
does not change formal roles, formal order, `TY_IDX`, return convention, Conv
attributes, or projection topology. Therefore:

- rewrite each compatible shared callee body once;
- materialize weight and bias payloads for each source call context;
- redirect caller actuals while retaining the same callee signature; and
- do not split a clone solely because payload keys, ranges, checksums,
  instance metadata, call metadata, or CKKS value state differ.

Retained BatchNorm formals and actuals are permitted as dead ABI inputs during
this compatibility stage. The FHE verifier must prove that they have no
remaining executable use and the conversion report must distinguish them from
a surviving BatchNorm computation. Clone splitting remains fail-closed when
the reviewed structural ABI/body clone key actually diverges.

## Atomicity Boundary

The request array is the in-PU preflight/commit unit. Weight and bias for a
context must be submitted in the same batch. FHE body rewrite/retirement must
be validated before committing caller payload requests. The existing all-PU
FHE checkpoint remains the file publication transaction: a later semantic,
body-rewrite, writer, or verification failure publishes neither the final
`.fhe.B` nor a partial temporary artifact. The converted side payload must use
the same temporary/publication discipline and may be renamed only after the
final all-PU semantic and image gate succeeds.

This is artifact atomicity, not general in-memory rollback for an arbitrary
continued conversion. A failure after materialization is terminal for the
conversion-only process: it must not retry or continue transforming the
mutated in-memory image.

The call-ABI extension below adds one optional ELF section. It does not change
an existing mapped-image row, WHIRL opcode, TY encoding, binary revision, or
the `.WHIRL.dsl` v1 row sizes.

## Durable Call ABI Roles

The optional `.WHIRL.dsl_call_abi` image records the semantic relationship
between a call argument and a callee formal without retaining caller-local WN
pointers or parsing source variable names. Its v1 header is 24 bytes and each
argument row is 32 bytes. Row identity is `(callsite_id, actual_ordinal)`;
zero IDs, `UINT32_MAX` ordinals, unknown flags, duplicate identities, and
trailing or truncated images are rejected.

Each row records `argument_value_id`, `callee_formal_ordinal`, and a stable
structural semantic role. Role names are versioned by the producer/domain
contract and use lowercase dot-separated identifiers, for example
`cnn.basic_block.conv1.weight`. They describe structural paths, not Python or
source variable spellings. Equivalent contexts that call one shared PU must
agree on the role assigned to a given callee formal ordinal.

Validation proves that the callsite's callee agrees, both ordinals are in
range, the argument value is caller-owned and matches the physical call
actual's ST and `TY_IDX`, and the callee formal has that exact `TY_IDX`.
Per-PU validation requires that PU's local symbol table and `Current_pu` to be
active; a mismatched active PU is rejected before any local ST is dereferenced.
Consumers may enumerate rows, query by `(callsite_id, actual_ordinal)`, or
query by `(callee_pu_st, callee_formal_ordinal)`. Borrowed records must not be
retained across managed-table mutation or reset.

Batch materialization updates `argument_value_id` in the same preflight/commit
unit as the physical call actual. A rejected request array changes neither the
call nor the ABI table. Original provenance remains in
`dsl.converted_from_value_id` and does not enter ABI-row identity.

## Durable PU Formal Values

The call-ABI row identifies a callee formal ordinal but does not identify the
callee-owned DSL value representing that formal. Local `ST_IDX` values may
collide between PUs, and free-form value metadata is not a structural owner
relation. Backend consumers must not activate a callee while rewriting a
caller merely to recover that relation.

The optional `.WHIRL.dsl_pu_interface` image supplies the missing immutable
relation. Its v1 header is 24 bytes and each formal row is 32 bytes. A row maps
`(owner_pu_st, formal_ordinal)` to `formal_value_id`, `formal_st`, and the exact
canonical `formal_ty`. The ordered rows cover the complete physical
`FUNC_ENTRY` interface: input formals followed by hidden result formals. IDs
and symbol/type indices are fixed-width existing WHIRL carriers; flags and
reserved fields are zero in v1. Duplicate owner and ordinal, value, or symbol
identities are rejected. The owner must resolve to an existing global function
ST with a valid PU entry, so orphan rows cannot evade per-PU verification.

`DSL_PU_Interface_Image_Find_Formal()` is globally usable after mapped-image
load and requires no active callee symbol table. Per-PU validation still
requires the matching `Current_pu` and local symbol table, then proves every
physical formal is present exactly once and agrees with the `FUNC_ENTRY`
`IDNAME`, formal or formal-reference storage class, value row, symbol, and
exact `TY_IDX`. `ir_b2a -st -src` prints the table for review.

The section is append-only and optional. Existing artifacts without it retain
the v1 call-ABI behavior and load with an empty PU-interface table. Existing
sections, call-ABI rows, opcode/type encodings, and the WHIRL binary revision
are unchanged. New producers emit a formal row from the opaque PU-formal
builder operation before creating call-role rows.

## Native Value Redirection And Retirement

`DSL_IR_Redirect_And_Retire_Native_Value()` supports the narrow pure-expression
case required to retire a folded BatchNorm result. The caller supplies the
owner PU, function root, containing BLOCK, replacement and retiring STIDs,
their managed value IDs, the expected logical operator/version, and the
operand ordinal that names the replacement.

Preflight requires:

- both definitions are in the same BLOCK and the replacement precedes the
  retiring STID, so it dominates every accepted use;
- every executable use occurs after the retiring STID;
- the retiring result symbol has one definition and unique ownership;
- the registered logical operator is pure and has no state-effect rows;
- the replacement operand agrees in the physical WN, logical operand row,
  result ST, and exact canonical `TY_IDX`;
- uses are direct LDID reads, including nested BLOCK/REGION bodies; and
- no LDA/address-taken use, second STID/write, alias escape, call-ABI argument,
  or unsupported managed relationship exists.

Commit redirects physical LDID reads, managed DSL value references, and
managed REGION interface ST references, then removes the retiring STID from
the executable tree. FHE disposition and fold rows remain provenance and are
not redirected. The logical node and value rows remain visible for inspection:
the node carries `DSL_IR_NODE_FLAG_RETIRED`, the value carries
`DSL_IR_VALUE_FLAG_REDIRECTED`, and the redirect target is derived from the
retired node's recorded replacement operand ordinal. No existing reserved row
field is reclassified.

REGION interface redirection has its own no-mutation preflight. It applies the
candidate ST replacement to a copied interface set and runs the same generic
and contract-profile verifier used after mapped reopen. A duplicate symbol,
role/ordinal conflict, or profile violation rejects retirement before any WN,
REGION, or DSL row changes.

Gatekeeper and `ir_b2a -st -src` distinguish total logical rows from executable
nodes. Mapped reopen preserves the retired/redirected evidence and validates
that the derived target remains well formed. The immediately previous
same-revision reader ignores the unknown optional `.WHIRL.dsl_call_abi`
section, reopens both fixtures, and passes its traditional WHIRL verifier. Its
per-PU DSL gatekeeper also accepts the non-retirement fixture because the
physical tree and the existing DSL image remain consistent. For a
retirement-bearing image, its program-level DSL gatekeeper fails closed with a
physical/logical node-count mismatch; it does not silently treat the retained
logical row as executable. Such images therefore require the updated DSL-aware
gatekeeper even though their physical WHIRL remains readable by the previous
tool.

## Focused Evidence

`osprey/common/com/tests/dsl_external_tensor_rewrite_test.sh` retains:

```text
artifacts/fhe/external-tensor-rewrite/
  external_tensor_rewrite.B
  external_tensor_rewrite.T
  commands.txt
  validation.log
```

The trace contains two real PUs, distinct rank-4 weight and rank-1 bias types,
two calls sharing one unchanged callee signature, per-context converted
weight/bias actuals, entry-owned stem weight/bias values, source-link metadata,
side-file tensor TCONs, and the rewritten `OPR_CALL` actuals. Negative checks
cover failure in the second member of a weight/bias batch, duplicate names,
duplicate call targets, invalid types, bad ranges, malformed checksums, wrong
owners, and invalid tensor TCONs without partial table or call mutation.
