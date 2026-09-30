# FHE SYNC-5 Dynamic Descriptor Selection Contract

Status: accepted main/FHE coordination contract for SYNC-5 runtime lowering

## Purpose

Class-centric WHIRL PUs are intentionally reusable. A single static runtime
call in one shared PU may execute in several source-model contexts, while the
corresponding `open64_fhe_operation_desc_v1` records differ by expanded
sequence index, visit index, value identities, layout, CKKS state, and source
context. SecureResNet has 87 static evaluation callsites and 147 dynamic
events.

Generated code must therefore select the descriptor for the current inference
session. It must not embed one dynamic descriptor in a shared PU, add hidden
invocation-context formals, or use a process-global/static counter. Those
alternatives either select the wrong event or violate concurrent-session,
retry, ownership, and reset semantics.

## Public ABI Addition

The FHE-owned ABI v1 header publishes this append-only control call:

```c
open64_fhe_status_v1 open64_fhe_operation_desc_select_v1(
    open64_fhe_model_v1_t model,
    open64_fhe_ciphertext_v1_t anchor,
    uint32_t static_ordinal,
    uint32_t operation_kind,
    const open64_fhe_operation_desc_v1 **out_desc);
```

The `anchor` is ABI operand zero for the selected dynamic event. For example,
it is the convolution input, residual main path, bootstrap/normalization/stage
input, reconstruction refreshed input, or pool/layout/linear input. Selection
validates the current session event against the anchor; the following
evaluation still validates every operand.

Generated C and WHIRL contain only the static callsite ordinal and operation
kind. The model-package/model owns the immutable descriptor and payload.
`out_desc` receives a borrowed pointer valid until model destruction.

## Reservation State Machine

Selection executes through the existing context queue and uses the inference
session carried by `anchor`:

1. Validate the output pointer first and set it to `NULL` before lower-priority
   checks.
2. Validate model and anchor handles, ownership, active context/session,
   current expanded event, static ordinal, operation kind, anchor identity,
   configuration, model identity, and exact context CKKS state.
3. On success, return the immutable borrowed descriptor and reserve the
   current event. Selection does not advance the session cursor.
4. The immediately following evaluation must use that exact descriptor and
   the expected operands. Successful output publication clears the
   reservation and advances the cursor exactly once.

A second selection while a reservation is outstanding returns recoverable
`CALL_ORDER_MISMATCH` and changes neither reservation nor cursor. Evaluation
with the wrong descriptor pointer, kind, ordinal, or operands has the same
result, allowing the exact reserved call or cleanup to follow.

An exact reserved evaluation that reaches a recoverable provider failure
publishes no output, clears the reservation, and leaves the cursor unchanged;
retry begins with a new selection. Fatal provider termination clears the
reservation and poisons the context/session under the existing ABI rules.
Releasing the reserved anchor or destroying its model, session, or context
returns `BUSY`. Different inference sessions remain independent.

Descriptor selection is a control call. It is excluded from the 87-static /
147-dynamic evaluation census and has its own exact 87/147 selector census.

## WHIRL Lowering

The generic `VHO_FHE_Build_Standard_Call()` service already represents both
calls without an FHE-specific WN extension:

```text
selected_descriptor = NULL
status = open64_fhe_operation_desc_select_v1(
    model, anchor, static_ordinal, operation_kind, &selected_descriptor)
if status != OK
    <ordinary-WHIRL failure path>

result = NULL
status = open64_fhe_<operation>_v1(
    model, operands..., selected_descriptor, &result)
if status != OK
    <ordinary-WHIRL failure path>
```

The descriptor local is an exact pointer to
`open64_fhe_operation_desc_v1`; its output slot is the corresponding
pointer-to-pointer TY. Both calls retain the source position of the original
logical operation. The selector status check immediately precedes the
evaluation output initialization and call. No hidden PU formal, global
cursor, new DSL opcode, new mapped-image section, or common/com API is needed.

The FHE semantic lowerer owns mapping logical operations to ABI symbols,
static ordinals, operation kinds, and operand zero. The generic VHO helper
owns checked ordinary-WHIRL function types, symbols, parameters, output slots,
status capture, failure control flow, and source positions.

## Verification And Evidence

The main-owned focused test must prove:

- selector followed by evaluation in one PU;
- exact model, ciphertext, descriptor, and output pointer TY identities;
- static ordinal and operation kind are ordinary constant arguments;
- evaluation loads the descriptor local written by selection;
- output locals are initialized to `NULL` and every status is checked;
- source positions survive on both calls and created symbols;
- the PU formal list is unchanged and no global cursor variable appears; and
- generic runtime-lowering phase behavior remains unchanged.

FHE-owned ABI/mock tests additionally prove reservation, retry, concurrent
session, release/destroy `BUSY`, status-priority, borrowed-lifetime, and exact
87/147 selector-census behavior. Full certification retains `.mid.B`, its
`ir_b2a -st -src` trace, generated C, mock transcript, schedule/descriptor
artifacts, and hashes.
