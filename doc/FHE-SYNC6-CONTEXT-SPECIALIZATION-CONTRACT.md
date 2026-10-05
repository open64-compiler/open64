# SYNC-6 CKKS Context Specialization Contract

Status: approved architecture direction; generic program-scope PU cloning,
typed scalar formals/actuals, root TCON materialization, and a separate
checkpoint landed in PR #164. The FHE policy and executable CKKS expansion
are not certified. See
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

The v1 formal/actual is scalar `MTYPE_F8` by value, with exact TY equality
under the existing native call ABI.
Conversion from a scalar to packed plaintext must be an explicit CKKS step,
not an implicit runtime repair. The bound's source is the authenticated
range TCON for the exact `(source ReLU value, context PU identity, callsite)`;
its origin and byte identity remain inspectable. A callee with two ReLUs may
need two independently bound formals unless equality of their approved bounds
is proved. No source operand, canonical TY, or approved range row is mutated
to accommodate the clone.

## Existing Open64 service and FHE consumer

`osprey/be/com/clone.h` exposes `IPO_CLONE::New_Clone` and `Clone_Tree` for
traditional PU/symtab/map cloning. DRA's `DRA_Add_Clone` in
`osprey/be/be/dra_clone.cxx` demonstrates global function ST/PU creation,
local symtab preservation, DST clone origin, PU_Info insertion, and map-table
lifetime. This machinery should be reused where legal. It does **not** by
itself copy or re-own DSL node/value rows, operand references, REGION
interfaces, PU formal rows, callsite and call-ABI rows, FHE plan/range/state
rows, or origin-to-clone event associations. It is not a complete public
transaction for this work.

`DSL_Program_Interface_Apply_PU` remains a useful precedent for interface
evolution, but its runtime-handle rows are not plaintext `B` inputs. The
merged `DSL_PU_Transaction_Apply_Resident` service now clones/reuses PUs,
inserts typed scalar formals before hidden results, routes calls with
caller-owned exact-TCON actuals, and returns owner-qualified clone value
maps. `DSL_PU_Transaction_Root_Constant_Active` covers the one entry-owned
bound. `DSL_PU_Transaction_Scalar_TCON_Active` proves exact root and caller
TCON provenance after mapped reopen. The FHE pass must use these APIs, not
rewrite WN/ST/TY or overload a resource-handle row.

The accepted generic owner-safe *program* transaction provides:

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
5. Read-only preflight before mutation, followed by a dedicated-process
   apply. A failure after apply starts is terminal for that worker; it must
   publish no partial `.B` and cannot retry the mutated in-memory program.
6. Opaque result maps from each `(source PU, source value/static event,
   callsite context)` to the cloned PU/value/event, so FHE can invoke the
   reviewed atomic CKKS expansion API without guessing local WN/ST IDs.

The FHE consumer still owns full executable signature derivation, approved
`B` selection, CKKS legality, source-event provenance, and the post-apply
semantic gate. The generic transaction alone does not prove those facts.

## Certification tests

- Equal complete signatures reuse one clone; a level/key/layout difference
  splits it. Different call ordinals alone do not split it.
- Same-signature contexts with distinct B values share a clone only when the
  B formals/actuals differ correctly and produce distinct normalization
  operands. Two ReLUs in one body retain two independent bounds.
- A changed formal TY, missing actual, wrong caller/callee owner, bad TCON,
  recursive/alternate-entry PU, or unmapped DSL value rejects before commit.
- Inject failures at clone creation, formal insertion, call retarget, and
  image publication; verify preflight rejection leaves state unchanged and
  terminal apply failure leaves no final/temporary `.B`.
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
variant. The audit now joins all 129 captured call-ABI argument rows and
proves exact insertion ordinals: callsites 1/2/3/5/6/8/9 append bounds at
formals 13/14; projection callsites 4/7 append at 19/20, before hidden
results. Its two independent retained capture families agree, and missing
context, instance-path, callee, and ABI-ordinal negatives fail closed. These
audits prove the FHE plan inputs; they do not yet create a native ResNet
variant or certify `.ckks_ops.B`.
