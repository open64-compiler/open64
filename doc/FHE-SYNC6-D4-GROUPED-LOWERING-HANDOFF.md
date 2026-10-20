# S6-0d D4 Grouped Lowering Handoff

Status: FHE read-only join complete; main/common mutation contracts required.

This handoff narrows the remaining D4 ownership boundary after the accepted
S6-0c image was reopened and joined to the frozen public ABI v1 schedule. It is
read together with `FHE-SYNC6-S6-0D-DETAILED-EXECUTION-PLAN.md`,
`FHE-SYNC6-CKKS-ABI-LOWERING-BOUNDARY.md`, and
`FHE-RUNTIME-C-ABI-V1-CONTRACT.md`.

## Certified FHE Input

The read-only FHE join consumes the immutable mapped images and proves:

| Property | Certified count |
| --- | ---: |
| Specialized input PUs | 10 |
| Canonical public PUs | 6 |
| Canonical source definitions | 32 |
| Public ABI static descriptors | 87 |
| Dynamic public bindings | 147 |
| Private CKKS group bindings | 146 |
| Representation-only flatten bindings | 1 |

The 147th binding is `common.flatten`, a zero-motion representation view. It
has no CKKS primitive group. Creating a synthetic group for it would falsify
the CKKS event image.

Canonical PUs are keyed by structured source-definition identity plus their
complete ordered formal TY signature. Dynamic source instances are keyed by
owner, source node/value, context PU identity, and root/callsite identity.
Canonical source families additionally use stable value name, logical
operator/version, and result TY. Representative dispositions authorize the
public source family; private groups enumerate its dynamic instances.

## Main/Common Request 1: Grouped Native Lowering

Publish one generic atomic transaction that accepts, for one active PU:

- the complete ordered set of internal native definitions and DSL value IDs;
- the separately proved group output and final source replacement value;
- expected logical operator/version and owner for every member;
- one detached, already checked standard-WHIRL block producing the public
  runtime handle; and
- the runtime projection/relation used by the existing standard lowering.

Preflight must derive the PU tree and containing blocks from `PU_Info`, then
prove unique definitions, exact TY/owner identity, closed internal WN and DSL
uses, dominance, source positions, REGION/interface/call obligations, one
permitted external final-result use set, and an unescaped group output. It must
reject a member used outside the group, an extra output, LDA/alias escape,
mixed owner, wrong result, duplicate member, or malformed standard block.

Commit must insert the standard block once, redirect only permitted final
uses, remove all member definitions, update logical status atomically, and
return owner-qualified results for every member. All preflight rejection must
leave tree and images unchanged. No call allocation or other fallible work may
remain after physical mutation begins.

The existing `DSL_IR_Lower_Native_Values_To_Standard_Blocks()` is not this
transaction: it maps each native value to one projected handle and would
fabricate public calls or projections for internal CKKS values.

### Proposed public contract

The transaction is generic and belongs beside `dsl_ir_lower.cxx`, not in FHE
code. The final names may follow the common/com audit, but the information and
atomicity below are required:

```c
typedef struct {
    WN *native_definition;
    DSL_IR_NODE_ID node_id;
    DSL_IR_VALUE_ID value_id;
    DSL_OPERATOR expected_operator;
    UINT16 expected_version;
    UINT16 reserved;
} DSL_IR_NATIVE_GROUP_MEMBER_REQUEST;

typedef struct {
    ST_IDX owner_pu_st;
    const DSL_IR_NATIVE_GROUP_MEMBER_REQUEST *members;
    UINT32 member_count;
    DSL_IR_VALUE_ID group_output_value_id;
    DSL_IR_VALUE_ID semantic_source_value_id;
    WN *standard_block;
    DSL_IR_LOWER_RELATION relation;
} DSL_IR_NATIVE_GROUP_LOWER_REQUEST;

typedef struct {
    ST_IDX owner_pu_st;
    DSL_IR_VALUE_ID semantic_source_value_id;
    DSL_IR_VALUE_ID retired_group_output_value_id;
    UINT32 retired_member_count;
    UINT32 inserted_statement_count;
    ST_IDX handle_st;
    TY_IDX handle_ty;
} DSL_IR_NATIVE_GROUP_LOWER_RESULT;

extern BOOL DSL_IR_Lower_Native_Value_Groups_To_Standard_Blocks(
    PU_Info *pu_info,
    const DSL_IR_NATIVE_GROUP_LOWER_REQUEST *requests,
    UINT32 request_count,
    FILE *diagnostic,
    DSL_IR_NATIVE_GROUP_LOWER_RESULT *results);
```

`PU_Info` is the sole tree and owner authority. The request's `owner_pu_st`
is an expected identity, not a second source of tree ownership. Member arrays,
standard blocks, and relation records are borrowed for the call. The complete
request array is preflighted before mutation. Success consumes each detached
standard block; failure consumes none and leaves caller result storage
unchanged.

The result intentionally names the semantic source and retired private output
separately. The source is the public ABI event being reconstructed; the group
output is the last internal CKKS value whose executable uses are redirected to
the standard runtime handle. Internal group members do not acquire public
runtime projections.

### Required common/com tests

1. One three-member group with a final result use succeeds and inserts one
   standard block.
2. Two groups in one PU commit together and return owner-qualified results.
3. Reordered member lists are accepted only when the physical/dataflow graph
   proves the same closed group; duplicate members are rejected.
4. An internal member read outside the group, second external output, `LDA`,
   call-ABI escape, REGION-interface escape, wrong result, wrong owner, mixed
   blocks, or mismatched logical operator rejects without mutation.
5. A malformed second request proves complete-array rollback, including WN,
   DSL flags, projections, and detached-block ownership.
6. Mapped reopen prints one lowered semantic relation and retained provenance
   for every retired private member without executable CKKS definitions.

## Main/Common Request 2: Canonical-PU Collapse

Publish one whole-program atomic transaction that consumes a reviewed mapping
from specialized owner PUs to canonical representative PUs. It must:

- require exact structured definition identity and formal/result ABI equality;
- redirect every physical call and managed callsite/call-ABI reference;
- prove no address-taken, REGION, interface, value, source-identity, or other
  managed reference remains to a retiring PU;
- retire redundant FUNC_ENTRY/PU state without reusing local `ST_IDX` values;
- preserve source/callsite provenance needed by `ir_b2a -st -src`; and
- commit all affected owners together or leave the whole program unchanged.

The FHE pass provides the six-family representative map and the authenticated
147-binding plan. It does not rewrite global/local symbol tables directly.

### Proposed public contract

This is a generic whole-program service because AI frontends can produce the
same context-specialized clone family. It belongs in a focused common/com
program-rewrite file rather than `dsl_ir_rewrite.cxx`.

```c
typedef struct {
    ST_IDX specialized_owner_pu_st;
    ST_IDX canonical_owner_pu_st;
    DSL_PU_SOURCE_IDENTITY_ID specialized_identity_id;
    DSL_PU_SOURCE_IDENTITY_ID canonical_identity_id;
} DSL_PROGRAM_CANONICAL_PU_REQUEST;

typedef struct {
    UINT32 canonical_pu_count;
    UINT32 retired_pu_count;
    UINT32 rewritten_call_count;
    UINT32 rewritten_callsite_count;
    UINT32 rewritten_interface_count;
} DSL_PROGRAM_CANONICAL_PU_RESULT;

extern BOOL DSL_Program_Canonicalize_Equivalent_PUs(
    PU_Info *program,
    const DSL_PROGRAM_CANONICAL_PU_REQUEST *requests,
    UINT32 request_count,
    FILE *diagnostic,
    DSL_PROGRAM_CANONICAL_PU_RESULT *result);
```

The mapping contains every specialized PU exactly once; a representative maps
to itself. The transaction derives formal/result ABIs and structured source
identity from managed records and physical PUs. It rejects caller-supplied
claims that disagree. Preflight spans the complete program before the first
physical or image mutation. Success preserves the representative's body and
rewrites callers and managed references; it never merges two executable
bodies or reuses a local `ST_IDX` from a retiring PU.

### Required common/com tests

1. Two equivalent clones and one root caller collapse to one representative;
   both physical calls and call/call-ABI records target it.
2. Equivalent definition strings with different formal or hidden-result TYs
   reject.
3. Address-taken PU symbols, REGION references, unexpected value ownership,
   mismatched source identity, or an unlisted clone reject atomically.
4. A failure in the last mapping leaves all earlier PUs, calls, global STs,
   and managed tables unchanged.
5. Mapped reopen proves the canonical FUNC_ENTRY census, preserved source
   positions, and absence of references to retired PUs.

## Main/Common Request 3: Terminal Event-Image Policy

Use the minimal compatibility rule for `-O0`: omit the optional
`.WHIRL.dsl_ckks_events` section from terminal standard-WHIRL output after one
common/fhe transaction proves complete consumption. Do not revise its v1 rows
or add a terminal-status image merely to preserve evidence already retained
by the immutable `.ckks_ops.B`, lowered logical provenance, and authenticated
join report.

### Proposed public contract

```c
typedef struct {
    ST_IDX owner_pu_st;
    DSL_IR_VALUE_ID semantic_source_value_id;
    const DSL_CKKS_EVENT_ID *event_ids;
    UINT32 event_count;
    DSL_IR_VALUE_ID group_output_value_id;
} DSL_CKKS_TERMINAL_GROUP_REQUEST;

typedef struct {
    UINT32 owner_pu_count;
    UINT32 group_count;
    UINT32 event_count;
} DSL_CKKS_TERMINAL_CONSUMPTION_RESULT;

extern BOOL DSL_CKKS_Event_Image_Consume_Terminal(
    const DSL_CKKS_TERMINAL_GROUP_REQUEST *groups,
    UINT32 group_count,
    FILE *diagnostic,
    DSL_CKKS_TERMINAL_CONSUMPTION_RESULT *result);
```

The service is runtime-only state in common/fhe. It verifies every managed
event appears exactly once, its owner/source/output agrees with the lowered
logical relation, no executable event result definition remains, and the
complete structural/FHE gate passes. Only then does the writer omit the
optional section. A rejected or incomplete request leaves normal v1
validation and writing behavior unchanged. Reset and checkpoint abort clear
the runtime-only terminal state.

### Required common/fhe tests

1. Complete two-PU consumption validates, writes no event section, and reopens
   with an empty event image plus intact lowered provenance.
2. Missing, duplicate, unknown, wrong-owner, wrong-source, or still-live event
   requests reject without changing writer behavior.
3. Abort/reset after successful in-memory consumption restores the default
   state and cannot leak suppression into the next compilation.
4. A previous reader reopens the terminal file because the optional section is
   absent; the original `.ckks_ops.B` remains unchanged and reopenable.

## Main/Common Reviewable Commit Sequence

| Commit | Owned files | Acceptance boundary |
| --- | --- | --- |
| M1 grouped lowering | focused `dsl_ir_group_lower.{h,cxx}`, public declarations, linked transaction tests | Generic one-PU all-group preflight/commit/rollback; no FHE schedule assumptions. |
| M2 canonical PU rewrite | focused `dsl_program_canonicalize.{h,cxx}`, linked multi-PU tests | Whole-program clone collapse with exact ABI/source identity and mapped reopen. |
| M3 terminal event consumption | focused `common/fhe` service, writer/gate integration, mapped compatibility tests | Complete-consumption proof suppresses only the optional event section; reset and malformed paths fail closed. |

Each new `.h` and `.cxx` file must carry a purpose comment, references to this
handoff and the owning design documents, and function-level contract comments
as required by `AGENTS.md`. No commit changes an opcode, TY identity, existing
mapped row, WHIRL revision, or public FHE C ABI.

## FHE Work After Merge

After all three contracts merge, the FHE task will:

1. Rebase and consume only the public main/common APIs.
2. Re-run the focused join and cached 6/32/87/147 census.
3. Build the 87 checked standard-WHIRL call blocks with the existing SYNC-5
   semantic runtime lowering service.
4. Invoke grouped lowering and clone collapse, then run the structural and FHE
   gatekeepers.
5. Reopen the public binary with `ir_b2a -st -src` and prove six FUNC_ENTRYs,
   87 static ABI evaluations, no executable CKKS nodes, and the reviewed image
   disposition.
6. Generate public C, compile/link/run it against the existing ABI mock, and
   compare all 147 ordered successful evaluations with the accepted SYNC-5
   schedule.
7. Prove malformed join, grouped-lowering, collapse, and publication failures
   leave no final or temporary artifacts.

No opcode, tensor TY, public ABI symbol, or existing mapped-image row changes
are requested by this handoff.
