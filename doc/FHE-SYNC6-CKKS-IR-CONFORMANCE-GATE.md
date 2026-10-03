# SYNC-6 CKKS Semantic IR Conformance Gate

Status: design/implementation handoff; no new opcode or binary row allocated.

The provider-independent CKKS semantic IR must be created and certified before
the Open64 compiler or generated program interacts with ACE `FHErt_ant`.
`doc/FHE-ACE-RTLIB-RUNTIME-DECISION.md` records the pinned ACE import/export
gap, but that gap blocks provider execution only. It does not block this IR
work. This sequencing refines the SYNC-6 handoff without weakening the final
secretless client/server acceptance requirement in
`doc/DSC_FHE_Compiler_Architecture_and_Integration_Plan_v0.10.md`.

## Existing Boundaries

The certified SYNC-4 `secure_resnet20.ckks.B` contains context-bound refresh,
normalization, three polynomial stages, reconstruction, and CKKS state
*planning records*. It is not a complete executable CKKS primitive graph.
The SYNC-5 `secure_resnet20.mid.B` contains standard WHIRL calls to the stable
FHE C ABI. Its historical `.mid.B` stem does not establish a formal WHIRL
level, and those calls are already past the proposed CKKS optimization
boundary. Both artifacts remain retained regression references; neither is
renamed or misrepresented as the new conformance checkpoint.

The new checkpoint is provisionally named `secure_resnet20.ckks_ops.B` and
`secure_resnet20.ckks_ops.T`. It sits after FHE-CNN/layout/materialization and
before terminal standard-call lowering:

```text
source/CNN and FHE conversion
  -> encrypted layout and context-sensitive materialization
  -> scheme-independent HE, then explicit CKKS semantic operations
  -> CKKS state/key/rotation/precision verifier and .ckks_ops.B checkpoint
  -> optional FHE VHO/IPA, then re-verify and freeze schedule
  -> standard WHIRL FHE C ABI calls, whirl2c, provider adapter
  -> ACE ANT only after separate provider admission
```

## Conformance Contract

1. Represent every executable encrypted step with a logical, provider-neutral
   operation and explicit direct operands/results. At minimum, the reviewed
   census must cover ciphertext/plaintext add and multiply, encode/plaintext
   materialization, signed rotation, rescale, level/modulus alignment,
   relinearization, bootstrap, and the arithmetic needed for the accepted
   Chebyshev evaluation and reconstruction. Query-only scale/level facts may
   remain typed state rather than executable operations. A CNN operation may
   remain as provenance but may not be the sole executable representation.
2. Each result has a stable DSL value identity and separately associated
   encryption descriptor, canonical tensor TY/layout, value-specific CKKS
   level/scale/components/precision/slots, exact key and signed-rotation
   requirements, source/context identity, and transformation provenance.
   A level or scale change creates a new value/state; it never mutates the
   canonical tensor TY. The eleven reusable ReLU definitions retain all
   nineteen distinct call contexts and their approved bounds and target
   post-refresh levels.
   A shared callee body cannot hardcode one context's bound or target level.
   Prefer deterministic specialization keyed by the whole-PU executable CKKS
   state/schedule signature, including post-refresh levels. Equivalent
   signatures reuse one compiler PU; a context bound B may remain a typed
   plaintext formal when it does not change circuit structure. The review
   must prove exact caller/callee identity and ABI preservation. Do not clone
   merely to store planning metadata, use call ordinal as a clone key, or
   silently conflate the nineteen uses. A fully parameterized shared body is
   an alternative only after proving dynamic-level ABI, per-context
   executable-state semantics, and terminal lowering. One global DSL value
   identity cannot carry three incompatible executable levels.
3. The mandatory pre-ReLU bootstrap is an explicit CKKS operation with
   `PRE_RELU_REFRESH` reason. Bootstrap does not compute ReLU. Normalize,
   ordered degree-7/15/13 stages, and reconstruction are explicit downstream
   operations. No implicit provider refresh, hidden rescale, or automatic
   level repair may substitute for a missing IR operation at `-O0`.
4. Lower every one of the 147 certified dynamic high-level evaluation events
   into one or more CKKS steps while retaining a complete event-to-step
   provenance map. The CKKS step count is not presumed to equal 147.
   Recompute operation, signed-rotation, key, depth, and output-state counts
   from the materialized graph, not from planner estimates or ACE behavior.
5. The verifier proves operand/result representation and TY compatibility;
   per-context call/formal/actual ownership; layout and slot legality;
   scale/level/precision transitions and available modulus depth; exact
   bootstrap target levels 15/17/18 where required; key and rotation
   availability; no unresolved pending action; no unlowered executable CNN,
   SIHE, or `common.relu` node at the CKKS checkpoint; and complete source
   provenance. A rejected PU or context leaves no valid-looking final `.B`.
6. The checkpoint is mapped binary WHIRL, reopened in another process with
   `ir_b2a -st -src`. It prints logical CKKS operators, direct operands,
   context-specific states, key/rotation requirements, and source positions.
   Existing readers, non-FHE artifacts, and the SYNC-5 mock path retain their
   established compatibility behavior.

## Ownership And Review Sequence

| Slice | Owner | Reviewable result |
| --- | --- | --- |
| S6-0a semantic census/physical contract | Main/common owns append-only logical registry contracts and a generic owner-PU-safe atomic one-to-many native DSL value expansion API; reuse physical `OPR_DSL` and existing DSL image/inspection tables. FHE supplies semantic operands, state rules, and tests. | Accepted handoff table, typed event relation, and a staged transaction with late-failure rollback. Read-only preflight alone does not close this gate. |
| S6-0b opaque producer and per-value state | FHE task consumes only reviewed backend-safe common APIs and binds existing FHE CKKS value-state records to each new result value. | Focused add/sub/mul/rotate/rescale/relin/bootstrap `.B`/`.T` fixtures with source and state evidence. |
| S6-0c full ResNet expansion | Main/common first publishes the generic atomic clone/formal/call transaction; FHE then expands the six source PUs, certified high-level schedule, and 19 context-specific ReLU sequences, specializing by complete executable signature. | Clone-aware `.ckks_ops.B`/`.T` with measured PU count, exact B formal/actual or separately reviewed static-B evidence, origin-to-clone/event-to-step maps, key/rotation/depth census, and independent numerical checks. |
| S6-0d gate and terminal lowering | FHE task verifies the complete IR and lowers it through the existing stable C ABI; main reviews standard-WHIRL boundary. | Negative malformed-state/ownership/depth/key tests and generated-C/mock equivalence to SYNC-5. |
| S6-1 and later | ACE provider and runtime owners, after S6-0 certification. | New exact ACE pin/capability admission, then broker/worker/client-server execution. |

`osprey/common/com/dsl_opcode.h` currently has no published CKKS logical
operator family. This document requests a reviewed shared/common handoff;
the FHE task must not assign enum values, edit physical `OPR_DSL`, or change
the binary format independently. The existing `DSL_WN_Create_Native` path
already creates direct kids, and `.WHIRL.dsl` opcode/node/attribute/value/
reference tables and `ir_b2a` already persist and print logical identities.
Reuse those facilities rather than creating a parallel CKKS WN universe. The
private physical escape remains private.

The frontend-only `DSL_Builder_*` API is not a backend expansion interface.
`DSL_IR_Rewrite_Native_Value` replaces one definition and cannot implement a
one-to-many CKKS expansion. Main/common must publish an owner-PU-safe atomic
transaction that preflights an expected source definition, typed existing and
prior-step operands, ordered logical specs/attributes/results, canonical TY,
source position, and output identity; then creates the result STIDs, physical
native DSL nodes, image rows, and references together. On failure it leaves
the tree and logical tables unchanged. Its result returns all new DSL value
IDs so FHE can bind the already published CKKS value-state record to each
distinct executable value. Any later state-binding failure is terminal for
the checkpoint; no partially mutated image is retried or published. Complete
high-level-event-to-step provenance must be machine-checkable: reuse typed
existing rows if they can express the join, otherwise review an append-only
typed relation, never encode it only in metadata strings.

The FHE-owned process-local preflight in
`osprey/be/vho/fhe_ckks_event_coverage.{h,cxx}` checks that an independently
counted set of source events has unique owner/source-value/static-ordinal/
context identities, at least one result per event, and dense per-event step
ordinals. It accepts the same reusable PU value identity in different call
contexts. A companion process-local adapter expands existing SYNC-5 static
schedule fields over exact root/callsite routes; a nested path whose
execution multiplicity exceeds those representable routes fails closed.
It preflights route identity, static-ordinal uniqueness, capacity, and count
before writing the caller's event array. The focused 147-event test is
synthetic and proves only these algorithms; it does not certify the ResNet
event census, CKKS legality, or persisted provenance. The real producer must
derive the input schedule and routes from the existing managed tables, call
this preflight before the common transaction, and check the mapped image
again after reopen.
`osprey/be/vho/fhe_ckks_source_events.{h,cxx}` now performs that read-only
join using `VHO_FHE_Runtime_Static_Schedule_Prepare` and the DSL call image,
before any CKKS node is created. Its focused linked fixture substitutes the
table functions and proves exact root/two-callsite ReLU identities and
no-partial-output failures; it is not yet a mapped SecureResNet run. The
independent artifact auditor
`osprey/be/vho/tests/fhe_ckks_real_event_census.py` joins the retained
six-PU `ir_b2a -st -src` identity/node tables with the structured SYNC-5
schedule. It verifies 6 PUs, 9 callsites, 32 source definitions, 87 static
events, 147 context-expanded events, 11 ReLU definitions, and 19 ReLU
contexts, including negative multiplicity, ordinal, nested-call, and
source-value checks. Its hashed JSON output is read-only evidence, not an
executable CKKS `.B`, a mapped-image gate, or proof of CKKS state legality.
The native collector and common transaction still require a real mapped
six-PU certification after the main-owned expansion API lands. The
existing static schedule cannot be recomputed after a high-level source is
retired. Specialization of a shared PU also needs the reviewed typed link
from clone static ordinal to original source ordinal before this event list
can certify the final context-sensitive executable graph.

The FHE-owned read-only join in
`osprey/be/vho/fhe_ckks_relu_plan.{h,cxx}` resolves each scheduled ReLU
source event to its exact materialization row, context range, and output
CKKS planning state. It requires the six-part refresh/normalize/three-stage/
reconstruction chain, dense static-event order, exact owner/context IDs,
state-input continuity, stage/parameter identities, and a pending
`PRE_RELU_REFRESH` bootstrap target. Its Linux linked fixture covers two
contexts and rejects broken state, range, stage, reason, and owner evidence
without changing output. The real ResNet contract has 19 contexts and 114
such source events; the join is not yet an executable CKKS step producer or
a mapped six-PU certification. The independent
`fhe_ckks_real_relu_plan_audit.py` checks the retained six-PU `-st -src`
trace against the hashed 147-event census: all 114 materialization rows
join to their exact context/range/state chain, with post-refresh target
levels 15 (16 contexts), 17 (1), and 18 (2). This is inspection evidence,
not a replacement for the native mapped-image and CKKS-state gate.

The retained context-state evidence also proves that the six captured source
PUs cannot remain six fixed-schedule executable CKKS PUs. For the same
source block PU, callsites 1/2 versus 3 need ordered post-refresh levels
`(15,15)` versus `(15,18)`; callsite 5 versus 6 has the same split; and
callsite 8 versus 9 needs `(15,15)` versus `(15,17)`. The hashed
`fhe_ckks_context_signature_audit.py` report derives three additional
context-specialized clones, giving a **minimum of nine executable PUs**
if each context bound B is passed as an explicit typed plaintext formal.
That formal, its per-caller actual, exact TY, and origin PU/value/static-event
identity require reviewed native support; the concrete consumer contract and
existing Open64 clone-service audit are in
`FHE-SYNC6-CONTEXT-SPECIALIZATION-CONTRACT.md`. If B instead becomes a static
per-context constant, the nine call contexts may need one clone each,
giving up to ten total PUs. The final count is a certification result, not
a fixed acceptance assumption; metadata-only B selection and an unproved
dynamic-level ABI are not permitted.

### Proposed Shared Contract Census

All rows below are proposed v1 *logical* names, not allocated enum values.
Main/common owns any shared registry, physical WN, mapped-image, and builder
contract. The FHE task owns CKKS legality, state propagation, schedule
expansion, and terminal ABI lowering. Canonical TY remains the tensor identity;
cipher/plain class and mutable level/scale live in encryption/value-state
records, never in a new binary type kind. An operation is pure with respect to
source-language state but may consume keys and allocate provider values after
terminal lowering; the verifier must not infer numerical algebra laws from
that source-level purity.

| Logical operation | Operand/result contract and required attributes | Verifier and lowering handoff |
| --- | --- | --- |
| `common.tensor_const.v1` (reuse) | Tensor payload -> plain constant/encoded operand; exact TY, encoding, payload digest. | Reuse existing constant identity; FHE proves source bytes and CKKS encoding state before `ckks.encode` or terminal plaintext construction. |
| `ckks.add.v1` (new candidate) | Two cipher/plain-typed values -> one value; exact operand class, layout, scale/level-match policy, no broadcast. | Do not reuse broadcast-capable/algebraically simplified `common.add.v1` for an executable CKKS step. FHE verifies alignment and lowers to exact add primitive. |
| `ckks.sub.v1` (new candidate) | Two cipher/plain-typed values -> one value; exact operand class, layout, scale/level-match policy. | Chebyshev evaluation needs subtraction (pinned `fhe-cmplr/rtlib/ant/ckks/src/chebyshev_impl.c` calls `Sub_ciphertext`); retain it explicitly unless an add-plus-negate expansion independently proves identical CKKS scale, depth, and key effects. |
| `ckks.mul.v1` (new candidate) | Cipher-cipher or cipher-plain -> one value; operation class, output component/scale policy. | Do not reuse `common.mul.v1`, whose current registry contract is marker-only and permits generic algebraic handling. FHE accounts for multiplicative depth and explicit downstream rescale/relin. |
| `ckks.encode.v1` (new candidate) | Plain tensor plus encoding descriptor -> packed plain; slots, layout, scale, level, source payload digest. | FHE proves capacity/shape and byte identity; common owns logical node contract if accepted. |
| `ckks.rotate.v1` (new candidate) | Cipher and signed nonzero rotation -> cipher; signed index, layout, key identity. | FHE checks slots, exact rotation key, state preservation or declared transition. |
| `ckks.rescale.v1` (new candidate) | Cipher -> cipher; exact consumed level count and target scale. | FHE checks modulus-chain availability, precision floor, and result state. |
| `ckks.modswitch.v1` (new candidate) | Cipher -> cipher; exact target level. | FHE checks direction, scale/precision, and absence of undeclared repair. |
| `ckks.relin.v1` (new candidate) | Widened cipher -> two-component cipher; key identity. | FHE checks input components, key class, and output components. |
| `ckks.bootstrap.v1` (new candidate) | Cipher -> refreshed cipher; target level, slots, reason, bootstrap key identity. | FHE checks all 19 pre-ReLU boundaries and per-context 15/17/18 targets; bootstrap has no ReLU result semantics. |

The accepted polynomial stages, normalization, and reconstruction lower to
ordered uses of these arithmetic operations with exact coefficient/asset
references and stage provenance. The census is intentionally minimal for the
first ResNet path; conjugation, generic ciphertext-ciphertext matrix
operations, and POLY/RNS operators require a separate demonstrated need.
Main/common must resolve direct-kid shapes, attribute schema, atomic value
construction API, printer names, and compatibility strategy before the FHE
implementation writes any executable CKKS node.

## Test Ladder And Exit

- Focused deterministic fixtures for each CKKS operation and invalid
  operand/state/key/rotation case, including input preservation on rejection.
- Two-context tests with equal signatures proving clone reuse, and different
  signatures proving deterministic whole-PU specialization, exact call ABI,
  typed B formal behavior, and distinct executable value levels.
- Full six-source-PU ResNet census with a verified executable clone count:
  147 high-level events completely mapped to CKKS steps; 19 explicit
  pre-ReLU bootstraps with targets
  15 x 16, 17 x 1, 18 x 2; no standalone executable ReLU.
- Separate-process `.ckks_ops.B` mapped reopen and `ir_b2a -st -src` with
  nonzero source positions; old/non-FHE compatibility and malformed-image
  tests; no tabs and `git diff --check`.
- Terminal lowering and mock-provider comparison uses unchanged ABI v1.
  This proves compiler lowering, not ACE ciphertext execution.

The CKKS IR gate closes only when the complete artifact and verifier pass.
The ACE source finding in `doc/FHE-SYNC6-ACE-ANT-ADMISSION-AUDIT.md` remains an
explicit later provider gate. It must not be used to skip CKKS IR work, and
CKKS IR success must not be reported as SYNC-6 client/server completion.
