# SYNC-6 CKKS-to-ABI v1 Lowering Boundary

Status: proposed S6-0d contract for main/common and FHE review. No terminal
lowering from a complete `secure_resnet20.ckks_ops.B` is certified yet.

## Fixed Contracts

The executable CKKS IR carries individual `ckks.add`, `ckks.sub`, `ckks.mul`,
`ckks.encode`, `ckks.rotate`, `ckks.rescale`, `ckks.modswitch`, `ckks.relin`,
and `ckks.bootstrap` nodes. The frozen public C ABI v1 instead exposes nine
ResNet semantic evaluation operations. It has no public call for each CKKS
primitive. The reviewed SYNC-5 source schedule has 87 static semantic events
and 147 execution-weighted events. Adding primitive C calls or changing ABI v1
to make CKKS lowering easy is not an S6-0d option.

The typed `.WHIRL.dsl_ckks_events` relation retains source-definition,
origin-definition, context, static-event, step, and explicit final-result
identities. A source event may have many CKKS steps but contributes exactly
one public ABI evaluation call. The provider implementation of that call is
responsible for the certified internal circuit. This is not permission to
skip CKKS state, key, depth, layout, or numerical-equivalence checks before
the call is emitted.

## Proposed Read-Only Gate

For every source-context semantic event, before any terminal mutation:

1. Join the exact `(origin owner PU, origin source value, origin static
   ordinal, context identity, callsite)` to one approved ABI v1 descriptor by
   persisted identity. The ABI operation kind, descriptor hash, and ordinal
   must be read from that descriptor, not inferred from CKKS step position or
   equated to the CKKS static ordinal.
2. Require one nonempty, dense, ordered CKKS step group with a unique
   `DSL_CKKS_EVENT_FINAL_RESULT` row. Its marked result, not the last row by
   convention, is the only ABI-visible value. Every other step result may be
   used inside the group but must not escape to another group, call actual,
   return, REGION interface, or external projection.
3. Verify every step's logical operator, direct operands, owner, source
   position, canonical TY, encryption descriptor, value-specific CKKS state,
   explicit repair actions, key requirements, and typed event provenance.
   The source/result and context image gates remain mandatory.
4. Prove the complete ABI descriptor's input/output identity, tensor/layout,
   payload, and semantic-operation contract against the CKKS group. A group
   that differs from the frozen descriptor fails closed; a matching ordinal
   or operation name alone is insufficient.
5. Count every approved source semantic event exactly once. Distinguish the
   87 source-origin static events and 147 dynamic visits from the number of
   physical callsites in specialized executable PUs. Do not silently reuse
   the SYNC-5 six-PU physical-callsites assertion after cloning.

The existing S6-0c read-only coverage and value-state adapters prove parts
of items 2 and 3. They are not a whole-program ABI binding gate.

The SYNC-5 runtime producer cannot be selected unchanged for this input:
`VHO_FHE_Runtime_Static_Schedule_Prepare()` skips lowered source definitions
and rejects live CKKS operators without a high-level schedule rule;
`VHO_FHE_Runtime_Operation_Plan_Prepare_PU()` recognizes high-level CNN
definitions, not CKKS event groups. The S6-0d FHE pass should reuse its
checked standard-call construction and ABI/mock contract after group proof,
but must build a distinct explicit group-to-descriptor plan. Changing a
counter or forcing CKKS nodes through the old operator switch is not proof.

## Shared Mutation Gap

`DSL_IR_Lower_Native_Values_To_Standard_Blocks()` is a one-native-value to
one projected-runtime-handle transaction. A CKKS event group has several
internal values but one final ABI-visible handle. Applying the old API to
every internal value would fabricate projections or emit extra ABI calls.
S6-0d therefore needs a reviewed, generic, owner-PU atomic *grouped lowering*
transaction from main/common, unless an existing shared service is shown to
satisfy the following same contract:

- Preflight all group member definitions, typed event rows, unique final
  result, internal-use closure, owner/local-symbol identity, source positions,
  interface/REGION/call obligations, and one final runtime projection.
- Accept one detached checked standard-WHIRL block for the whole group and
  produce one ABI v1 evaluation call with the existing status/output guards.
- Remove every group's native CKKS definition from the executable tree, keep
  its logical rows as nonexecutable provenance, and redirect only the final
  result's permitted external uses. The event-row disposition requires the
  separate compatibility decision below.
- Commit tree/image/interface edits together or roll back completely on
  failure. Return each member's lowered identity so the FHE pass can verify
  the committed group without parsing WN internals.

This request changes no opcode, canonical tensor TY, or public C ABI.
Main/common owns the generic transaction and mapped-image compatibility;
FHE owns CKKS-group legality and ABI descriptor selection.

The current `DSL_CKKS_Event_Image_Validate()` requires every event result
node/value to have `flags == NONE`. It will reject a post-lowering image that
retains these rows while marking their results `LOWERED`. Main/common must
choose an explicit terminal-artifact policy before mutation: either omit the
optional CKKS event section from standard-WHIRL output and retain the
separately reopened `.ckks_ops.B` plus authenticated cross-artifact report, or
publish a reviewed append-only status/capability rule with matching writer,
mapped reader, printer, gatekeeper, old-reader, and malformed-image tests.
The FHE task must not clear or reinterpret the existing rows itself.

## Static-Callsite Decision

Context specialization is required for fixed CKKS levels and typed bounds.
That can create more physical generated-C callsites than the original six-PU
SYNC-5 graph, even when source-origin events remain 87 and dynamic visits
remain 147. Main-side review recommends an exact six-PU runtime
wrapper/collapse with descriptor-selected context behavior to preserve ABI
v1's physical 87-static/147-dynamic generated-C census; project approval and
the reversible clone-to-source mapping proof are pending. Merely relabeling
clone callsites as source-origin callsites is not sufficient. No representation
may alter the ordered 147 successful ABI evaluations or public ABI v1.

## Exit Evidence

The final S6-0d checkpoint requires a complete, separately reopened
`.ckks_ops.B` input; positive and malformed group/descriptor/state/key/depth
tests; all-PU atomic lowering; generated C linked to the existing ABI mock;
comparison against the accepted SYNC-5 semantic schedule; and rejection
without final or temporary artifacts. Use `ir_b2a -st -src` on both sides of
the terminal boundary. The present focused state-transfer tests do not meet
that exit.

Related authority: `FHE-RUNTIME-C-ABI-V1-CONTRACT.md`,
`FHE-SYNC6-CKKS-IR-CONFORMANCE-GATE.md`,
`FHE-SYNC6-NATIVE-CKKS-EXPANSION-CONTRACT.md`, and
`FHE-SYNC5-STANDARD-WHIRL-LOWERING-CONTRACT.md`.
