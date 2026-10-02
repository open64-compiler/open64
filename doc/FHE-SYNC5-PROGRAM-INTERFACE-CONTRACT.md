# FHE SYNC-5 Program Interface Evolution Contract

Status: accepted main/common contract for staged implementation

## Purpose

The SYNC-5 runtime-interface projection converts existing canonical tensor
values and formals into opaque runtime handles. Full SecureResNet lowering also
needs three operations that the published `.WHIRL.dsl_runtime_interface` v1
cannot represent honestly:

1. remove verified-dead canonical formals and their caller actuals;
2. promote a root-PU local external tensor into a launcher-supplied handle
   formal; and
3. thread runtime-only resources such as the model and ReLU coefficient
   handles through reused PUs.

This contract keeps `.WHIRL.dsl_runtime_interface` v1 unchanged. It adds one
optional, append-only fixed-row section that records program-interface
evolution separately from canonical provenance and canonical value projection.
The section is generic: common/com records and verifies identity, ownership,
types, roles, and physical relationships; VHO/FHE decides which inputs are
dead, which values are launcher parameters, and which resources are required.

## Compilation Scope

The complete plan is prevalidated before mutation. The backend driver then
selects each PU in normal preorder and applies only that PU's portion while its
local symbol table and map table are active. No local `WN*`, `ST*`, or borrowed
query result survives a PU transition.

Per-PU commit is atomic after complete program-plan preflight. A later PU
failure is terminal for that checkpoint process, and binary-last checkpoint
publication provides artifact atomicity. The API does not claim general
whole-program in-memory rollback or retry after a post-commit failure.

## Physical Image

Reserve `WT_DSL_PROGRAM_INTERFACE` value `0x2b` and ELF section
`.WHIRL.dsl_program_interface`. Version 1 uses 8-byte section alignment and
these exact rows:

| Record | Size | Purpose |
| --- | ---: | --- |
| `DSL_PROGRAM_INTERFACE_IMAGE_HEADER` | 64 | Counts for the five row tables, flags, and reserved words |
| `DSL_RETIRED_FORMAL_RECORD` | 48 | Immutable reference to a canonical PU formal removed from executable ABI |
| `DSL_RETIRED_CALL_ARGUMENT_RECORD` | 48 | Immutable reference to a matching canonical caller actual removed from executable ABI |
| `DSL_RUNTIME_INPUT_RECORD` | 64 | Launcher input/resource identity and exact handle TY |
| `DSL_RUNTIME_INPUT_BINDING_RECORD` | 48 | Owner-PU handle formal created for a launcher input or threaded role |
| `DSL_RUNTIME_INPUT_CALL_RECORD` | 48 | Caller-binding to callee-binding flow through one callsite |

All row IDs are nonzero `UINT32`. All flags and reserved fields not defined by
v1 are zero. `STR_IDX`, `TY_IDX`, `ST_IDX`, `TCON_IDX`, source value IDs,
callsite IDs, formal IDs, and call-argument IDs retain their existing carrier
widths and meanings.

### Header

The 64-byte header contains, in order:

```text
magic, version,
retired_formal_count, retired_call_argument_count,
runtime_input_count, runtime_input_binding_count,
runtime_input_call_count, flags,
reserved[8]
```

### Retired Formal Row

The 48-byte row contains:

```text
id, pu_formal_id, owner_pu_st, formal_value_id,
formal_st, formal_ty, old_formal_ordinal, retirement_reason,
semantic_role, flags, reserved
```

Version 1 accepts only `VERIFIED_DEAD_INPUT` as `retirement_reason`. Result
formals cannot be retired. The referenced PU-interface row remains unchanged
and inspectable. Its effective physical ordinal is absent; each later live
formal's physical ordinal is its old ordinal minus the number of earlier
retired formals in the same PU.

The old ordinal remains the canonical provenance ordinal in every existing
PU-interface and runtime-interface v1 row. It is never rewritten to the
compacted ordinal. The retirement relation supplies the canonical-to-effective
join used by the combined validator, including shifted live input formals and
shifted hidden result formals.

### Retired Call-Argument Row

The 48-byte row contains:

```text
id, call_argument_id, callsite_id, argument_value_id,
old_actual_ordinal, old_callee_formal_ordinal,
retirement_reason, flags, semantic_role, reserved[2]
```

The referenced call-ABI row remains unchanged and inspectable. Every retired
callee formal must have exactly one matching retired argument for every call to
that callee. Live actual and formal ordinals compact deterministically by the
number of earlier retired entries.

Existing call-ABI and runtime-interface v1 call rows retain their old canonical
actual and callee-formal ordinals. The effective physical actual ordinal is the
old actual ordinal minus earlier retired actuals at that callsite; the
effective callee ordinal is obtained through the callee's retired-formal join.
The combined validator uses both joins and requires them to agree. It never
overwrites canonical v1 ordinals.

### Runtime Input Row

The 64-byte row contains:

```text
id, input_kind, source_owner_pu_st, source_value_id,
source_st, source_ty, source_tcon, flags,
stable_role, handle_ty, reserved[5]
```

Version 1 input kinds are:

- `SOURCE_EXTERNAL_TENSOR`: all source identity fields are nonzero and name
  one live, admitted external-data tensor value owned by the root PU. Its
  external reference, checksum, side-file range, TCON, descriptor TY, source,
  context, and lineage remain authoritative in their existing tables. The
  compiler obtains `source_tcon` from the validated `tensor_tcon` field of
  `DSL_IR_Image_Get_External_Tensor_Reference()` and never parses owner-local
  ST metadata.
- `OPAQUE_RUNTIME_RESOURCE`: source fields are zero. The stable role and exact
  opaque pointer TY define the input. `fhe.model` uses this kind.
- `TENSOR_TCON_RESOURCE`: source value/ST are zero; `source_ty` and
  `source_tcon` identify one canonical tensor constant. The three ReLU
  coefficient handles use this kind.

The generic layer validates source identity and type form but does not reserve
FHE role strings. The FHE gatekeeper requires the exact initial roles
`fhe.model` and `fhe.relu.coefficient.stage0`, `.stage1`, and `.stage2`.

Input identity is unique by source owner/value for source external tensors and
by stable role for runtime/TCON resources. Unknown kinds fail closed.

Side-file-dense TCONs keep large plaintext tensors and typed CKKS auxiliary
tensor payloads outside `.B`; WHIRL carries only the validated identity,
descriptor, path, range, and digest evidence. Opaque key bundles such as a
runtime-native rotation-key store remain `OPAQUE_RUNTIME_RESOURCE` values
unless a separately reviewed contract gives them a canonical tensor
representation.

### Runtime Input Binding Row

The 48-byte row contains:

```text
id, owner_pu_st, runtime_input_id,
handle_st, handle_ty, final_formal_ordinal,
binding_kind, flags, semantic_role, reserved[2]
```

Version 1 binding kinds are:

- `ROOT_PROMOTED_SOURCE`: a root launcher formal for one
  `SOURCE_EXTERNAL_TENSOR` input;
- `ROOT_RUNTIME_RESOURCE`: a root launcher formal for one opaque or TCON
  resource; and
- `THREADED_FORMAL`: a formal in a called PU that receives a handle from its
  caller. Its `runtime_input_id` is zero because a reused callee formal may
  receive different context-owned source tensors at different callsites.

Root bindings require a nonzero input ID and an owner PU with no incoming
callsite. Threaded bindings require input ID zero. The handle ST is local to the
owner PU, has the exact handle TY, source position, `SCLASS_FORMAL`, and no
canonical tensor identity. Roles are unique within one owner PU.

Live canonical formals retain their relative order after dead-formal
compaction. New bindings follow them in a request-order-independent canonical
order: closed binding-kind order, then `semantic_role` bytes, then stable
runtime-input identity. Duplicate sort keys reject. The producer may submit
any request-array order; stored `final_formal_ordinal` and image order must be
identical after canonical sorting.

### Runtime Input Call Row

The 48-byte row contains:

```text
id, callsite_id,
caller_owner_pu_st, caller_final_formal_ordinal,
callee_owner_pu_st, callee_final_formal_ordinal,
final_actual_ordinal, final_callee_formal_ordinal,
handle_ty, flags, semantic_role
```

The caller and callee fields are explicitly the bindings' derived effective
`final_formal_ordinal`, not plan indexes, image-row ordinals, or canonical
ordinals. Together with the owner they select exactly one binding row. The
callsite must connect those exact PUs. Both bindings and the physical
actual/formal use the same exact handle TY. The actual is a by-value,
read-only, passed-not-saved load of the caller binding. New runtime input
actuals follow compacted live canonical actuals in the same canonical order as
callee bindings.

The call row `semantic_role` equals the callee slot/binding role. Caller and
callee roles need not be equal: several context-specific promoted-weight roles
may feed one generic shared-callee weight slot. Caller binding identity carries
the context-specific source.

This relation supports both fixed resources and context-specific weights: two
root callsites may pass different promoted source bindings to the same shared
callee role without cloning the callee or assigning one false source identity
to its formal.

## In-Memory Plan And APIs

Publish request structures corresponding to each row. Requests carry stable
source IDs and role strings rather than local pointers. Binding requests also
carry a nonzero `SRCPOS` used for every created ST and rewritten statement.
Input and binding request indexes are zero-based plan references; persistent
IDs are allocated only during successful commit.

```text
DSL_PROGRAM_INTERFACE_PLAN
  retired_formals[]
  retired_call_arguments[]
  runtime_inputs[]
  runtime_input_bindings[]
  runtime_input_calls[]

DSL_Program_Interface_Plan_Validate(program_plan,
                                    runtime_projection_plan,
                                    diagnostic)

DSL_Program_Interface_Apply_PU(active_pu,
                               program_plan,
                               runtime_projection_plan,
                               diagnostic,
                               result)

DSL_Program_Interface_Validate_PU(active_pu, diagnostic)
DSL_Program_Interface_Validate_Lowered_PU(active_pu, diagnostic)
```

`DSL_Program_Interface_Apply_PU` is the coordinated replacement for invoking
the old projection service directly when program-interface evolution is
present. The existing `DSL_Runtime_Interface_Apply_PU` remains unchanged and
valid for plans with no retirements, promotions, or runtime-only resources.

The combined service:

1. validates all canonical retirement, projection, input, binding, and call
   requests before the first tree mutation;
2. builds the final formal and call-actual layouts in detached storage;
3. creates exact source-positioned handle STs;
4. rebuilds the active `FUNC_ENTRY`, PU prototype, and owned calls once;
5. records canonical projections in `.WHIRL.dsl_runtime_interface` v1;
6. records retirements and explicit runtime input flow in the new section; and
7. commits the active PU tree only after every PU-local check succeeds.

A mapped-image producer must call `Initialize_Special_Global_Symbols()` after
`Read_Global_Info()` and before either interface `Apply_PU` service. This
restores the predefined `MTYPE_To_TY` mapping from the file's TY table; a
missing void TY otherwise creates an invalid function-prototype return slot.
Both apply services reject an uninitialized predefined void TY before any
tree or table mutation. The producer runs `Verify_SYMTAB` on the completed
image before `Write_Global_Info`, in addition to the existing mapped-reopen
and `ir_b2a -st -src` checks.

All existing runtime-projection completeness rules are evaluated over the
effective live canonical set. Retired PU formals and retired call arguments
must be absent from the runtime projection plan. Every nonretired canonical
formal and argument still requires exactly one runtime-interface v1
projection. Existing v1 projection rows retain canonical ordinals; the
combined service uses the retirement join when constructing and validating
their effective physical positions. The standalone
`DSL_Runtime_Interface_Plan_Validate` remains unchanged for plans with no
program-interface evolution.

The result reports retired formals/actuals, promoted inputs, threaded bindings,
rewritten calls/returns, and canonical projections separately.

For the first FHE ResNet-20 consumer, the prepared six-PU plan contains 48
verified-dead BatchNorm formals, 80 matching call actuals, 48 runtime inputs,
92 bindings, 76 runtime-input call edges, 248 value projections, and 58 call
projections. The staged transaction prunes only retired plain REGION input
rows; it retains each `cnn.basic_block.v1` REGION, its live inputs, result,
contract, source, and metadata. Each caller-owned hidden-result handle is
zero-initialized at the call source position immediately before its output
`LDA`/call; per-PU and mapped-reopen validation require that exact pair.
The retained S5-E `fhe_program_interface_full_model_test.sh` reopens the
completed binary using `ir_b2a -st -src` and rejects a terminal PU-5 failure
without publishing a final or temporary binary. This certifies interface
evolution only; executable DSL operation lowering and production checkpoint
publication remain later SYNC-5 stages.

## Deadness And Ownership Proof

A formal is prunable only when all of these hold:

- the request exactly matches one existing input PU-interface row;
- every incoming callsite has exactly one matching call-ABI argument and one
  retirement request;
- the formal ST has no executable LDID, LDA, STID, ISTORE, call, return,
  REGION-interface, DSL value-reference, state/effect, or runtime-projection
  use outside the removed formal/actual positions;
- the corresponding argument values have no other executable use that the
  retirement would invalidate;
- the source value is not a result, effectful value, unique live output, or
  launcher-visible input; and
- the active local symbol table belongs to the supplied PU before any local ST
  lookup.

Address-taken or ambiguous alias use rejects v1. Common/com checks structural
deadness only; the FHE pass owns the proof that a value became dead after BN
folding and supplies the requests.

## Initialization And Resource Flow

The runtime-input graph must prove that every generated handle formal has an
initialized launcher origin:

- every runtime input has exactly one root binding;
- a root binding has no incoming runtime-input-call row;
- no runtime-input-call may target a root binding;
- every `THREADED_FORMAL` has exactly one runtime-input-call for every incoming
  source-language callsite to its owner PU;
- every caller binding used by a runtime-input-call is either root-bound or is
  itself completely threaded from every incoming callsite;
- every binding is reachable from a root binding through call rows; and
- recursive/cyclic runtime-resource threading is rejected in v1.

This is a dataflow proof, not a naming convention. A local handle ST with no
rooted path is invalid even if its role string resembles a valid resource.

## Validation

Global image validation checks row sizes, counts, IDs, flags, reserved fields,
unique keys, valid global function owners, exact source rows, callsite edges,
type identity, role strings, and exact one-to-one cross-table coverage:

- each retired formal maps to all and only its incoming retired arguments;
- each runtime input maps to exactly one root binding;
- each threaded binding maps to exactly one runtime-input call for every
  incoming callsite;
- each runtime-input call maps to exact caller/callee bindings and their
  effective physical ordinals; and
- all nonretired canonical formals/arguments map to exactly one unchanged v1
  projection interpreted through the canonical-to-effective ordinal join.

Mapped images are fully validated in temporary storage before managed tables
are reset or copied. Any rejected load preserves all prior managed tables.
Because `.WHIRL.dsl_program_interface` and
`.WHIRL.dsl_runtime_interface` form one semantic interface, the global binary
reader obtains both section views and calls
`DSL_Program_Runtime_Interface_Images_Load_Mapped`. The paired loader parses
both candidates, validates the complete retirement/projection join, and only
then resets and copies either managed table. A valid replacement program view
paired with an incompatible runtime view therefore leaves both previously
installed table families byte-for-byte unchanged. The individual mapped
loaders remain available for focused section tests and legacy images that do
not contain program-interface evolution; they make only per-section atomicity
claims.

Per-PU validation proves:

- physical formals equal compacted live canonical projections followed by the
  recorded explicit bindings;
- physical call actuals equal compacted live canonical projections followed by
  recorded runtime-input calls;
- no retired ST or argument remains executable;
- every binding ST has exact owner, class, TY, ordinal, and source position;
- no projected input is represented by an uninitialized local; and
- owner-local ST indexes are interpreted only under the active PU.

`Validate_Lowered_PU` additionally requires that source external tensor
definitions promoted to launcher inputs are themselves retired/nonexecuting
and have no remaining executable canonical tensor use. Immutable DSL-image,
external-tensor, checksum, side-file, source, context, and lineage provenance
remains inspectable. The intermediate structural validator permits executable
definitions until FHE semantic call lowering consumes and retires them through
the reviewed generic service.

## Compatibility

No existing row, opcode, TY kind, WN layout, or section changes. Existing
canonical PU-interface, call-ABI, external-tensor, and runtime-interface rows
remain immutable provenance. A current reader without the new section may
reopen ordinary physical WHIRL but must fail closed at the old program/runtime
gatekeeper because live-row completeness no longer matches the compacted
physical ABI. The immediately previous reader is a required negative test.

## Required Tests

1. Two-PU pruning with two callers, at least two retired formals, and exact
   ordinal compaction.
2. Rejection without mutation for executable, REGION, return, DSL-reference,
   address-taken, and incomplete-call retirement cases.
3. Root promotion of two external tensors with distinct canonical TYs and one
   exact plaintext-handle TY.
4. Root model plus three coefficient resources threaded through a shared
   callee reached by multiple callsites.
5. Two PUs with colliding local ST indexes and exact source positions.
6. Invalid final request in a multi-request plan proving no WN, ST, TY, or
   managed-table mutation.
7. Permuted request arrays producing byte-identical final ordinals and image
   rows, plus shifted live input, hidden-result, and call-actual coverage.
8. Rooted-resource negatives for missing incoming edges, extra incoming edges,
   call-to-root edges, and cyclic threading.
9. Mapped-image reopen, malformed size/count/ID/type/owner/ordinal/role
   negatives, and rejected-load no-mutation.
10. `ir_b2a -st -src` evidence for retired provenance, promoted source links,
   resource roles, final formal ordinals, and call threading.
11. Immediately previous reader fail-closed behavior.
12. Final lowered verification proving promoted source definitions are
    retired, no executable canonical tensor use remains for projected or
    retired values, and no runtime-handle local is uninitialized.
