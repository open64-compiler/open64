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

## Main/Common Request 3: Terminal Event-Image Policy

Choose and publish one compatibility rule for `.WHIRL.dsl_ckks_events` after
all executable CKKS definitions are removed:

1. omit the optional section from terminal standard-WHIRL output while the
   immutable `.ckks_ops.B` and authenticated join report retain provenance; or
2. add a reviewed append-only terminal-status contract with writer, mapped
   reader, validator, printer, previous-reader, and malformed-image tests.

The current v1 validator requires live event result node/value rows. FHE code
must not clear, reinterpret, or privately patch those rows.

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
