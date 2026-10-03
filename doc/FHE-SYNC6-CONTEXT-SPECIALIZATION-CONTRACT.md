# SYNC-6 CKKS Context Specialization Contract

Status: FHE consumer proposal for main/common review. No clone or new formal
API is implemented by this document. See
`FHE-SYNC6-CKKS-IR-CONFORMANCE-GATE.md` for the full CKKS IR gate.

## Why a PU clone is required

The captured SecureResNet20 has six source PUs and nine explicit block calls.
The accepted ReLU plan has 19 source contexts, with post-refresh levels 15
(16 contexts), 17 (one), and 18 (two). Three shared block source PUs are
called with incompatible *ordered executable* CKKS schedules:

| Source owner | Callsite group | Ordered post-refresh levels |
| --- | --- | --- |
| PU 51 | 1, 2 | 15, 15 |
| PU 51 | 3 | 15, 18 |
| PU 53 | 5 | 15, 15 |
| PU 53 | 6 | 15, 18 |
| PU 55 | 8 | 15, 15 |
| PU 55 | 9 | 15, 17 |

A single physical DSL value in one shared PU cannot carry two fixed output
levels. The current trace therefore proves a lower bound of nine executable
PUs: six original signature groups plus three additional context clones.
The final count must be recalculated from *all* CKKS events, not frozen from
these ReLU-only signatures. If each normalization bound `B` is embedded in the
body as a context constant, same-level callsites may also diverge; the
observed upper candidate is ten PUs. That is not permission to merge
incompatible bounds or states.

## Preferred executable representation

Specialize a source PU by its complete ordered CKKS operation, operand-TY,
layout, value-state, key, and level signature. Stable source-definition
identity stays attached to every clone; call ordinal is never the clone key.
Reuse one clone for equivalent signatures. The approved context-specific
normalization bound `B` is an explicit typed plaintext input formal when its
value changes without changing the circuit signature. Every caller passes its
own approved `B` actual; the callee uses that formal as a direct operand of
the normalization steps. Merely attaching `B` as image metadata is
insufficient to define executable behavior.

The formal/actual TY must match exactly under the existing native call ABI.
The compiler must choose and document whether the formal is an F8 scalar or
a canonical plaintext tensor of the exact packed shape used by `ckks.encode`.
Conversion from a scalar to packed plaintext must be an explicit CKKS step,
not an implicit runtime repair. The bound's source is the authenticated
range TCON for the exact `(source ReLU value, context PU identity, callsite)`;
its origin and byte identity remain inspectable. A callee with two ReLUs may
need two independently bound formals unless equality of their approved bounds
is proved. No source operand, canonical TY, or approved range row is mutated
to accommodate the clone.

## Existing Open64 service and missing transaction

`osprey/be/com/clone.h` exposes `IPO_CLONE::New_Clone` and `Clone_Tree` for
traditional PU/symtab/map cloning. DRA's `DRA_Add_Clone` in
`osprey/be/be/dra_clone.cxx` demonstrates global function ST/PU creation,
local symtab preservation, DST clone origin, PU_Info insertion, and map-table
lifetime. This machinery should be reused where legal. It does **not** by
itself copy or re-own DSL node/value rows, operand references, REGION
interfaces, PU formal rows, callsite and call-ABI rows, FHE plan/range/state
rows, or origin-to-clone event associations. It is not a complete public
transaction for this work.

`DSL_Program_Interface_Apply_PU` is a useful precedent for atomic interface
evolution, but its published runtime-input kinds describe external tensor or
opaque/TCON resources and its formal bindings use runtime handles. No
reviewed API currently promises to append a typed plaintext `B` formal to a
new CKKS clone, create the matching caller actual, and retarget the callsite
while preserving all DSL/FHE identity tables. Do not overload a resource
handle row or rewrite WN/ST/TY directly in the FHE pass.

Main/common is asked for a generic owner-safe *program* transaction with:

1. Read-only preflight over all requested source PU signatures and callsites;
   validate nonrecursive/ordinary-entry clone legality, active local symtabs,
   existing call/formal TY agreement, source identities, and exact bound TCONs.
2. Deterministic clone creation/reuse keyed by the complete executable
   signature, with new global function ST, PU_Info, local ST map, WN map,
   source DST, and canonical source-definition identity.
3. Complete DSL/FHE image duplication or origin association for each cloned
   definition/value/REGION/formal and each rewritten callsite. Cloned IDs
   must be unique, owned by the new PU, and never reinterpreted via a
   colliding local ST_IDX. Preserve source value and static-event ordinals as
   separate provenance, not as cloned identity.
4. Typed `B` formal insertion and per-callsite actual construction from the
   exact approved range TCON, with new PU interface and call-ABI rows. Prove
   every call has one actual for every new formal, exact TY equality, and no
   stale call to an incompatible signature.
5. A staged commit or complete rollback guarantee across WN, ST, PU_Info,
   local symtabs, maps, DST, and every managed image. A failed request must
   leave the source program unchanged and publish no partial `.B`.
6. Opaque result maps from each `(source PU, source value/static event,
   callsite context)` to the cloned PU/value/event, so FHE can invoke the
   reviewed atomic CKKS expansion API without guessing local WN/ST IDs.

These are required semantics, not prescribed function names or row layouts.
Main/common owns the exact API and physical compatibility review. The FHE
consumer owns the signature derivation, accepted B selection, CKKS legality,
and post-transaction semantic gate.

## Certification tests

- Equal complete signatures reuse one clone; a level/key/layout difference
  splits it. Different call ordinals alone do not split it.
- Same-signature contexts with distinct B values share a clone only when the
  B formals/actuals differ correctly and produce distinct normalization
  operands. Two ReLUs in one body retain two independent bounds.
- A changed formal TY, missing actual, wrong caller/callee owner, bad TCON,
  recursive/alternate-entry PU, or unmapped DSL value rejects before commit.
- Inject failures at clone creation, formal insertion, call retarget, and
  image publication; verify tree, ST, PU_Info, and every image remain equal to
  the preflight snapshot and no final/temporary `.B` survives.
- Separate-process `ir_b2a -st -src` shows source and clone function entries,
  call edges, exact formals/actuals, source positions, origin-to-clone map,
  CKKS value states, and all 19 bound contexts. Six source PUs remain
  inspectable; executable PU count is measured rather than asserted as six.
- Recompute the 147 source-context events through the origin map and prove
  full one-to-many CKKS step coverage, 19 pre-ReLU refreshes, and no live
  executable `common.relu` or CNN BatchNorm.

The read-only evidence today is
`fhe_ckks_context_signature_audit.py` plus its hashed
`secure_resnet20.context-signatures.json`. The companion
`fhe_ckks_bound_interface_audit.py` joins the exact 19 range identities to
those signatures, yielding 18 proposed per-callsite bound actuals, one
entry-owned bound constant, and two proposed bound formals for each called
variant. Its two independent retained capture families agree, and its
missing-context and instance-path negatives fail closed. These audits prove
the need for specialization and the proposed data flow, not a native
transaction, a chosen formal TY, or a certified `.ckks_ops.B`.
