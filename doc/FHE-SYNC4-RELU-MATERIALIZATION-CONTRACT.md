# FHE SYNC-4 ReLU Materialization Contract

Status: S4-1 through S4-6 are implementation-complete and locally certified on
2026-09-29. The main/common image/API contract, FHE semantic consumer, all four
bootstrap modes, independent artifact verifier, and six-PU retained evidence
are present. Independent review and merge remain the stage-close governance
step. This document does not allocate a new DSL logical operator.

## Purpose

SYNC-4 turns the accepted context-sensitive ReLU plan into an inspectable,
provider-independent materialization schedule. It does not yet emit runtime
calls, generated C, or provider object code.

The fixed `-O0` semantic sequence is:

```text
common.relu(x)
  -> mandatory pre-ReLU refresh
  -> normalize by the approved context bound B
  -> Chebyshev stage degree 7
  -> Chebyshev stage degree 15
  -> Chebyshev stage degree 13
  -> reconstruct 0.5*x*sign(x/B) + 0.5*x
```

The accepted coefficient/range profile was
`ace.chebyshev.sign.7x15x13.depth11.v1`. Executable recaptures use
`ace.chebyshev.sign.7x15x13.depth11.v2` to record the exact pinned ACE
addition-chain evaluator. No movement, merging,
deduplication, or profitability placement is legal at `-O0`.

## Central Context Requirement

Eleven reusable physical `common.relu` definitions represent nineteen source
call contexts. Each context has its own normalization bound and one of the
approved post-refresh levels 15, 17, or 18. Therefore SYNC-4 must not encode a
single bound or level in the shared callee body, and it must not clone PUs merely
to persist planning evidence.

The materialization identity is exactly:

```text
(owner_pu_st,
 source_relu_value_id,
 context_pu_identity_id,
 context_callsite_id,
 operation_ordinal)
```

Root context uses `context_callsite_id=0`. Called context uses the established
callee-centric invariant:

```text
identity.owner_pu_st == owner_pu_st == callsite.callee_pu_st
```

The callsite owner remains the caller. Numeric table IDs validate the persisted
join but do not select policy. Stable `instance_path` selects the approved
range and target refresh level before the rows are created.

## Representation Decision

SYNC-4 uses one new optional append-only mapped-image section named
`.WHIRL.dsl_fhe_materialization`; main/common owns the `WT_*` value, physical
field names, layout assertions, reader/writer, printer, and compatibility
implementation.

`WT_DSL_FHE_MATERIALIZATION=0x29` is assigned by the reviewed implementation
candidate. It becomes a published binary compatibility contract when that
implementation merges.

No new `DSL_OPERATOR` enum is required for S4. The existing physical
`common.relu` remains a source-semantic/provenance anchor. A complete context
schedule makes it non-standalone for the SYNC-4 semantic gate. SYNC-5 owns
creation of standard-WHIRL runtime calls and any required context
specialization.

Absence of the optional section remains valid for pre-SYNC-4 artifacts. A
SYNC-4 materialization checkpoint fails closed when the section is absent or
incomplete. Older readers may ignore the unknown optional section; they cannot
claim the SYNC-4 checkpoint because they do not implement its semantic gate.

## Proposed Fixed Image

Every section and row starts at 8-byte alignment. All IDs use zero as invalid.
Rows contain no pointers. Version 1 is exact-sized and append-only.

### Header: 64 bytes

| Field | Type | Contract |
| --- | --- | --- |
| `magic` | `UINT32` | section magic |
| `version` | `UINT32` | 1 |
| `header_size` | `UINT32` | 64 |
| `record_kind_count` | `UINT32` | 1 |
| `capabilities` | `UINT32` | context schedule capability only |
| `flags` | `UINT32` | zero in v1 |
| `operation_count` | `UINT32` | number of 64-byte rows |
| `context_count` | `UINT32` | number of complete six-row contexts |
| `reserved[8]` | `UINT32[8]` | all zero |

Version 1 requires `operation_count == context_count * 6`; both values must
also equal the counts derived from the validated coverage set and rows.

### Context operation row: 64 bytes

| Field | Type | Contract |
| --- | --- | --- |
| `id` | `UINT32` | contiguous one-based ID |
| `owner_pu_st` | `ST_IDX` | callee/source-definition PU |
| `source_relu_value_id` | `DSL_IR_VALUE_ID` | live `common.relu` source result |
| `context_pu_identity_id` | `DSL_PU_SOURCE_IDENTITY_ID` | callee source identity |
| `context_callsite_id` | `DSL_CALLSITE_METADATA_ID` | zero only for root |
| `operation_kind` | `UINT32` | closed enum below |
| `operation_ordinal` | `UINT32` | dense 0 through 5 |
| `profile_id` | `DSL_FHE_COMPOSITE_PROFILE_ID` | accepted profile |
| `stage_id` | `DSL_FHE_APPROX_STAGE_ID` | nonzero only for stage rows |
| `range_id` | `DSL_FHE_CONTEXT_RANGE_ID` | exact context range |
| `input_state_id` | `DSL_FHE_CONTEXT_CKKS_STATE_ID` | zero only for refresh source |
| `output_state_id` | `DSL_FHE_CONTEXT_CKKS_STATE_ID` | exact context state |
| `parameter_tcon` | `TCON_IDX` | bound for normalize, coefficients for stage |
| `flags` | `UINT32` | zero in v1 |
| `reserved0` | `UINT32` | zero |
| `reserved1` | `UINT32` | zero |

Closed operation kinds:

| Ordinal | Kind | Required association |
| --- | --- | --- |
| 0 | `REFRESH` | reason `PRE_RELU_REFRESH`; output is `POST_REFRESH.v1` |
| 1 | `NORMALIZE` | `parameter_tcon` equals the context's positive B |
| 2 | `APPROX_STAGE` | profile stage ordinal 0, degree 7 |
| 3 | `APPROX_STAGE` | profile stage ordinal 1, degree 15 |
| 4 | `APPROX_STAGE` | profile stage ordinal 2, degree 13 |
| 5 | `RECONSTRUCT_RELU` | exact `0.5*x*sign(x/B) + 0.5*x`; zero additional depth |

`parameter_tcon` is zero for refresh and reconstruction. Stage coefficient
bytes remain owned by the existing stage TCON and SHA-256 contract; the row
must reference that exact TCON rather than duplicate bytes.

## State Chain

Existing context-state rows carry the CKKS payload. They are not canonical TY
identity and must never mutate a canonical tensor type.

For each context, the required chain is:

| Materialized operation | Output context-state role/version |
| --- | --- |
| refresh | `POST_REFRESH.v1` |
| normalize | `POST_OPERATION.v1` |
| degree 7 | `POST_OPERATION.v2` |
| degree 15 | `POST_OPERATION.v3` |
| degree 13 | `POST_OPERATION.v4` |
| reconstruction | `RESULT.v1` |

Each operation after refresh consumes the preceding row's output state. The
source refresh input state may be zero because its precondition is represented
by the source value, encryption descriptor, pending bootstrap action, and
bootstrap reason already certified in SYNC-3.

The generic structural validator checks identity, chain continuity, exact TY
independence, profile/range/stage joins, flags, reserved fields, and one complete
six-row schedule per materialized context. The FHE semantic gate additionally
checks the ACE-specific levels, scale 56, two components, minimum precision 30,
32768 slots, depth 11, and reconstruction policy.

The merged profile contract is authoritative: its ordered stage
`level_consumption` values are `{3,4,4}` and sum to the depth-11 `App_relu`
body. ACE performs normalization before that body as an encoded `1/B`
plaintext multiplication followed by one rescale. Consequently the complete
post-refresh path consumes 12 levels: one for normalization and eleven for
the profile. The final degree-13 stage already includes the reconstruction
cost assigned by the profile, so `RECONSTRUCT_RELU` consumes zero additional
level and its `RESULT.v1` state has the same level as `POST_OPERATION.v4`.
For a post-refresh level `L`, the six outputs are therefore
`L, L-1, L-4, L-8, L-12, L-12`. Materialization must not reinterpret the
profile as `{3,4,3}+1` or treat normalization as level-neutral.

## Complete-Context Invariants

When the optional section is present, the machine-verifiable coverage set is
exactly every callee-tagged context range whose unique approximation
association points to a
`DSL_FHE_DISPOSITION_REQUIRE_COMPOSITE_APPROXIMATION` disposition. No other
context may appear. For every context in that exact set:

1. exactly six operation rows exist with dense ordinals 0 through 5;
2. exactly one `POST_REFRESH.v1` state exists;
3. stage rows refer to the same profile and its ordered stage IDs;
4. normalize refers to the range's exact positive-bound TCON;
5. operation input/output states form one acyclic chain;
6. no operation belongs to an unknown or duplicate context;
7. every row uses the callee-identity relation;
8. the source value is a live `common.relu` result before materialization;
9. every live source ReLU is either covered for all of its contexts or rejected;
10. no context is moved, merged, deduplicated, or omitted at `-O0`.

The source node must be registered as pure `common.relu`; operand 0 remains
derivable through its existing node/value-reference record. Profile and range
must resolve exactly one existing approximation association and its
`REQUIRE_COMPOSITE_APPROXIMATION` disposition. `NORMALIZE` uses the exact
`range.positive_bound_tcon`. Stage rows use profile stage ordinals 0, 1, and 2
and their exact coefficient TCONs. `RECONSTRUCT_RELU` has
`parameter_tcon=0`. Every referenced state repeats the same
owner/source/identity/callsite key and required role/version.

The existing disposition
`DSL_FHE_DISPOSITION_REQUIRE_COMPOSITE_APPROXIMATION` remains the source
semantic association. No new disposition value is needed solely to say that a
complete materialization section is present.

## Bootstrap Modes

| Mode | SYNC-4 behavior |
| --- | --- |
| `auto` | create one refresh row at ordinal 0 for every approved context |
| `on` | same baseline as `auto`, with provider bootstrap capability required |
| `manual` | validate-only: accept only when the input already contains one complete compatible six-row schedule for every required context; insert no rows or states; otherwise `CFHEMAT-RELU-004` |
| `off` | reject every surviving/materialized source ReLU with `CFHEMAT-RELU-003` |

Version 1 does not infer manual execution from `POST_REFRESH.v1`, whose merged
meaning remains a pending target planning state. A route-name list and a newly
created refresh row are also insufficient evidence. The manual producer must
have persisted the complete exact schedule before this pass begins; the pass
only validates it.

## Provider-Subset Capability Gate

S4-2 validates only the materialized ReLU subset. It does not certify the full
CNN rotation schedule, convolution MetaKernel, or all SYNC-5 runtime calls.

The authenticated provider manifest must prove:

- provider name and pinned revision;
- manifest schema/version and exact-byte SHA-256;
- CKKS configuration `N=2^16`, `Q0=60`, and scale bits 56;
- slot count at least 32768;
- bootstrap support for target levels 15, 17, and 18;
- multiplication, addition, plaintext/scalar multiplication, rescale,
  relinearization, and the pinned ACE BSGS-style Chebyshev addition-chain
  needed by the stages;
- composite depth at least 11;
- accepted coefficient/profile manifest hashes; and
- no logical materialization operation requires a secret key or decryption.
  The runtime server no-secret-key boundary is certified separately in SYNC-6.

The first provider manifest is pinned ACE ANT `FHErt_ant` at revision
`fb76131171b9f82aa6387f84dd73684fba5277e8`. Provider identity belongs in the
capability evidence/report, not in logical operator or tensor identity.

The initial reviewed input is
`doc/fhe-policy/sync4-relu/ace-ant-subset-capability.json`, bound by
`doc/fhe-policy/sync4-relu/package-index.json`. It distinguishes the requested
logical configuration (`Q0=60`, scale 56) from the generated ANT program's
provider-internal fields (51/46); the latter must not overwrite logical CKKS
state.

## Phase Boundary And Atomic Publication

Materialization runs once per active PU while the backend driver owns that PU's
local symtab and maptab. It is not an interprocedural or all-PU transformation.
A program-level checkpoint finalizer validates complete coverage and publishes
managed images but must not mutate any PU tree.

A materialization-only checkpoint consumes a previously certified `.fhe.B` in
a separate process. It must skip `VHO_FHE_Convert_Driver` entirely:

```text
certified .fhe.B
  -> per-PU VHO_FHE_Materialize_Driver_Try
  -> program coverage/image finalizer
  -> materialized checkpoint .B
```

`-FHE:materialization_checkpoint=<path>` is mutually exclusive with the
conversion checkpoint option. Checkpoint mode skips ordinary DSL lowering and
later backend phases after materialization. It must not rerun or silently alter
BN folding, range calibration, or profile selection. A future combined
non-checkpoint pipeline may invoke conversion and materialization drivers in
order, but they remain separate phases with separate counters.

The checkpoint publishes its materialization report and capability report as
auxiliary artifacts, closes the binary temporary, publishes auxiliaries
atomically, and publishes the materialized `.B` last as the commit marker.
Failure removes every temporary and every auxiliary final created by that run.

For `auto|on`, one context transaction reuses the already-certified
`POST_REFRESH.v1` row and inserts exactly five new context states
(`POST_OPERATION.v1-v4` and `RESULT.v1`) plus six operation rows. It changes
neither table on failure. Every inserted state uses the same
owner/source/identity/callsite key and the profile configuration's encryption
descriptor. Operation input/output IDs form the exact chain. `manual` performs
no insertion.

## Main/Common Hooks Required

Main/common ownership is required for:

1. the final `WT_*` value and fixed mapped-image implementation;
2. image add/intern/find/count/get/reset/validate/load/print services;
3. reader, writer, ELF, `ir_b2a -st -src`, and malformed-image coverage;
4. generic cross-section validation against values, callsites, ranges,
   profiles, stages, and context states;
5. a dedicated per-PU `VHO_FHE_Materialize_Driver_Try`
   registration/result/checkpoint boundary and a program finalizer;
6. a generic `VHO_FHE_Checkpoint` artifact service reused by compatibility
   wrappers for the existing conversion checkpoint, rather than a second
   transaction implementation;
7. phase-owned config options for materialization checkpoint, bootstrap mode,
   provider-manifest path, and lowercase exact-byte SHA transport to every PU
   and the program finalizer;
8. an opaque transaction that inserts six operation rows plus five new states,
   reuses `POST_REFRESH.v1`, and rolls back both tables without mutation; and
9. a semantic query that lets SYNC-5 enumerate complete schedules without
   depending on an active callee local symbol table.

The FHE task must not implement these by raw table, WN, ST, TY, mapped-image,
reader/writer, or ELF manipulation.

The first infrastructure slice publishes
`DSL_FHE_Materialization_Intern_Complete_Context()` as the atomic producer
transaction and `DSL_FHE_Materialization_Find()` plus count/get services as
the consumer surface. It completes items 1 through 4, 8, and 9 above.

The second infrastructure slice publishes the remaining items 5 through 7:

- `VHO_FHE_Materialize_Driver_Try()` and opaque registration/result APIs in
  `osprey/be/vho/fhe_materialize.{h,cxx}`;
- a generic binary-last transaction in
  `osprey/be/vho/fhe_checkpoint.{h,cxx}`, with compatibility wrappers for the
  existing conversion checkpoint;
- `-FHE:materialize`, `-FHE:materialization_checkpoint`,
  `-FHE:bootstrap`, `-FHE:provider_manifest`, and
  `-FHE:provider_sha256`; and
- `DSL_FHE_Plan_Image_Validate_Partial()` for per-PU construction, while
  `DSL_FHE_Plan_Image_Validate()` retains exact program coverage at the final
  checkpoint boundary.

The driver processes materialization under the active PU's local symbol table,
writes that PU before releasing its scope, and performs the registered
program finalizer only after all PUs and complete managed-image validation.
Conversion and materialization checkpoint modes are mutually exclusive.

## FHE-Owned Work

After the hooks merge, the FHE task owns:

- provider-subset manifest parsing and semantic capability checks;
- stable route-to-context joins and option-mode policy;
- creation of the six-row schedule and intermediate CKKS state payloads;
- ACE profile/state/depth semantic verification;
- deterministic reports and diagnostics;
- focused numerical oracle tests; and
- full six-PU certification with retained `.B`, `.T`, traces, reports,
  commands, diagnostics, and SHA-256 manifest.

## Stable Diagnostics

| Diagnostic | Meaning |
| --- | --- |
| `CFHEMAT-001` | FHE-bearing input requested materialization without registered semantic support |
| `CFHEMAT-002` | malformed or incomplete materialization image |
| `CFHEMAT-CAP-001` | capability manifest missing, unauthenticated, or malformed |
| `CFHEMAT-CAP-002` | provider lacks a required ReLU-subset primitive or configuration |
| `CFHEMAT-RELU-001` | missing or inconsistent range/profile/context state |
| `CFHEMAT-RELU-002` | duplicate, missing, reordered, or disconnected materialization operation |
| `CFHEMAT-RELU-003` | `bootstrap=off` with a surviving source ReLU |
| `CFHEMAT-RELU-004` | `bootstrap=manual` without an exact compatible boundary |
| `CFHEMAT-RELU-005` | coefficient, profile, range, route, or provider hash mismatch |
| `CFHEMAT-CHECKPOINT-001` | program coverage or final semantic validation failed |
| `CFHEMAT-CHECKPOINT-002` | atomic artifact publication failed |

Diagnostics use source positions from existing WN/ST/DST/value evidence. The
materialization rows do not duplicate source locations.

## S4 Commit Exit Criteria

- S4-1: this contract and the main/common physical contract are reviewed.
- S4-2: the pinned provider-subset manifest and all negative capability tests
  pass.
- S4-3: exactly 19 refresh rows reopen with target levels distributed 16/1/2
  across 15/17/18.
- S4-4: exactly 19 complete six-row schedules reopen and pass the clear
  numerical oracle.
- S4-5: `auto`, `on`, valid `manual`, missing-manual, and `off` behavior pass;
  no ReLU is standalone from a complete context schedule.
- S4-6: the six-PU materialized checkpoint and complete retained evidence
  family pass independent review.

Completion evidence:

- S4-1: the fixed image, atomic complete-context transaction, partial per-PU
  validation, exact final validation, mapped reader/writer/printer, and
  materialization checkpoint driver are implemented;
- S4-2: exact-byte SHA-256 authentication admits the pinned ACE ANT
  ReLU/bootstrap subset and rejects malformed, mismatched, or insufficient
  capability manifests;
- S4-3: the six-PU checkpoint contains exactly 19 refresh operations with
  target levels distributed `15:16,17:1,18:2`;
- S4-4: all 19 contexts contain the dense six-operation sequence `refresh`,
  `normalize`, stages `7`, `15`, `13`, and `reconstruct_relu`; direct
  direct Chebyshev recurrence and the independent Clenshaw numerical oracle
  agree within `2e-12`, and the
  20,001-point normalized ReLU oracle has maximum error at most `7.24e-4`;
- S4-5: `auto`, `on`, and complete `manual` pass; `off`, missing-manual, and
  bad provider hash fail with stable diagnostics and no published artifact;
- S4-6: the exact six-PU input publishes `.B` last, reopens in a separate
  `ir_b2a -st -src` process, and passes the independent artifact certifier.

The retained local evidence is `/private/tmp/open64-fhe-sync4-final`. This
checkpoint materializes an inspectable provider-independent correctness
schedule. It does not lower the schedule to standard-WHIRL runtime calls or
claim `FHErt_ant` execution; those are SYNC-5 and SYNC-6 gates.
