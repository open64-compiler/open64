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
request names an existing owner-PU external tensor source and creates one
caller-owned `common.tensor_const.v1` value with:

- the exact canonical tensor `TY_IDX`;
- a side-file dense tensor TCON whose descriptor, path, range, and logical byte
  size agree with the external tensor reference;
- source position on the new STID and result symbol;
- copied source/instance/context metadata where a source value exists;
- `dsl.converted_from_value_id` provenance;
- immutable original source payload and source value; and
- stable logical `ir_b2a -st -src` evidence.

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

No mapped-image row, ELF section, WHIRL opcode, TY encoding, or binary revision
is added. New logical node/value/ST metadata records use the existing managed
tables and mapped-image writer/reader path.

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
