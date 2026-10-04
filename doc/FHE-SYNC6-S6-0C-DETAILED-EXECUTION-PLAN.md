# S6-0c Detailed Execution Plan: Complete CKKS Semantic IR

Status: execution in progress, not certification. S6-0a and the generic
resident whole-PU transaction are merged. C0 has a retained, independently
rerun source-family artifact replay; C1 has a structured canonical event-plan
serializer and linked whole-PU grouping fixture. Neither is yet the native
program policy or a complete real circuit producer. No
`secure_resnet20.ckks_ops.B` exists. The normative gate is
`FHE-SYNC6-CKKS-IR-CONFORMANCE-GATE.md`.

## Boundary And Inputs

The current six-source-PU replay input is the retained
`/private/tmp/open64-fhe-sync6-s6-0c/artifacts/source/secure_resnet20.interfaced-initialized.B`
(SHA-256 `23907ba96478b78ca7bb771655cb655d94fdae09b6648e3bb26f28a3d544dca9`).
This identifies one test input, not a hard-coded compiler rule. Its replay
manifest must pin the source, folded side payload, coefficient package,
approved range manifest, CKKS config, tool revisions, and all input hashes.
The earlier `secure_resnet20.fhe.B` lacks the ReLU context-operation rows
and is not an interchangeable input.

S6-0c ends at provider-independent executable CKKS semantic WHIRL,
`secure_resnet20.ckks_ops.B`, reopened in another process as
`secure_resnet20.ckks_ops.T` with `ir_b2a -st -src`. It does not lower to
the frozen C ABI, run the ABI mock, link ACE, or claim encrypted inference.
Those are S6-0d and later. The ACE provider import/export gap cannot block
this provider-independent checkpoint.

The pinned source census is six PUs, nine block calls, 87 static high-level
evaluation events, and 147 execution-weighted source-context events. There
are 11 reusable `common.relu` definitions in 19 contexts and 114 ReLU
planning-operation rows. CKKS primitive count must be measured from the
produced graph; it need not equal 87, 114, or 147. ReLU-only state evidence
implies **at least nine** executable PU variants with typed B formals. The
final number is measured after *all* event plans exist.

## Non-Negotiable Invariants

1. Derive source identities from managed value, callsite, PU identity,
   source-static-ordinal, materialization, range, and context-state tables.
   Preserve the origin separately when cloning. Never parse names or infer
   identity from a PU-local `ST_IDX` or call ordinal.
2. Serialize a canonical, versioned *complete executable event plan* for
   each source/context event. Its bytes include ordered CKKS operations,
   direct operands, group outputs, TY/layout, each result state, keys,
   signed rotations, and typed B formal role. Whole-PU variant equality
   compares full bytes; SHA-256 is only a review fingerprint. Exclude
   source positions, names, diagnostics, and per-caller B TCON bytes from
   equality only when exact formal/actual binding is proved.
3. Specialize complete source PUs before executable mutation. Equal
   whole-PU signatures reuse one variant; any incompatible operation,
   layout, key, rotation, level, precision, or operand TY splits it. Use
   exact F8 B formals/actuals for called variants and an entry-owned TCON
   for the root. Retain source definition and clone origin.
4. Expand one source definition into ordered CKKS groups/steps through the
   merged transaction and bind a value-specific CKKS state to **every**
   result. An intermediate group output can feed a later group only under
   the reviewed schedule. `DSL_CKKS_EVENT_FINAL_RESULT` means **one** final
   source/context replacement across all its groups, not one per static
   ABI event group. Group outputs remain separate evidence for S6-0d.
5. Represent bootstrap, rotation, encode, arithmetic, rescale, modswitch,
   and relinearization explicitly where required. No hidden state repair,
   metadata standing in for a real operand, provider call, or mutation of
   a canonical tensor TY is acceptable at `-O0`.
6. Read-only preflight rejects before mutation. A failure after resident
   program apply or state binding starts is terminal for that checkpoint
   process. Discard its temporary output; never retry the mutated image.

## Reviewable Commit Sequence

The labels below are proposed review slices, not Git hashes. Each slice
must pass its focused tests and retain the named evidence before the next.
Any missing main/common contract gets a separate prerequisite review/PR.

Current checkpoint: `fhe_ckks_replay_gate.py` authenticates the exact input
family, recomputes the existing source-event and ReLU auditors, validates all
149 referenced SafeTensors slices, and writes an immutable-comparison replay
report. A fresh separate-process `ir_b2a -st -src` of the retained input
reproduces its `.T` byte-for-byte. Changed folded bytes fail with
`CFHEIR-REPLAY-001` and no report. This is independent artifact preflight;
the report also pins the printed CKKS config. Its historical `backend`
field is source metadata, not provider admission for this checkpoint;
the native C0 policy gate is still to be registered. C1's
`fhe_ckks_plan_bytes.{h,cxx}` serializes structured steps, operands, group
outputs, TY/layout, proposed result state, keys, signed rotations, asset
digest and typed B role in bounded little-endian v1 bytes. The structured
variant API groups these bytes and its 147-event fixture still yields the
nine-ReLU lower bound and a tenth variant on non-ReLU change. The actual
ResNet step plans do not yet exist, so C1 is machinery, not complete
semantic-plan certification.

| Slice | FHE-owned work | Focused exit evidence |
| --- | --- | --- |
| C0: replay/input gate | Add a deterministic input manifest and native read-only gate joining six-PU images, 87/147 events, 19 contexts, source/payload/coefficient hashes, and FHE/DSL validators. Reject the older `.fhe.B` and unsupported config before planning. | Stable hashed census, `-st -src` with six FUNC_ENTRYs/nine calls/nonzero source interleave; wrong hash or missing context rejects without output. |
| C1: canonical plan serializer | Define a bounded structured per-event CKKS step plan and versioned byte encoding for operation/operand/group-output/TY/layout/state/key/rotation/B-role facts. Validate before encoding; opaque producer-supplied bytes alone are not a legal plan. | Equal semantics encode identically despite allocation/name order; changed non-ReLU operation/state/key/rotation changes bytes. Missing field, bad operand, duplicate event, or truncated encoding rejects. Retain decoded plan and hashes. |
| C2: non-ReLU recipes | Generate CKKS step DAGs for Conv, residual add, average pool, flatten/layout, and linear. Before coding Conv, freeze the O0 encrypted-layout mapping against the governing architecture and record its exact column/row iteration order, packing, signed rotations, plaintext masks/weights, accumulation, and bias from verified folded bytes. The previously discussed ACE column-first schedule is a candidate, not an implicit MetaKernel/SYNC-7 optimizer admission. Unsupported dimensions/layouts fail closed; do not silently substitute im2col. | Per-operator clear tensor/slot oracle with nontrivial inputs; 13 Conv definitions/21 contexts, nine residual contexts, pool/flatten/linear, context-specific payloads and rotation-key census. Invalid payload/layout/slot negatives. |
| C3: ReLU recipe | In each of 19 contexts generate `ckks.bootstrap` with `PRE_RELU_REFRESH` and target 15/17/18, actual B materialization/encode/normalization, approved ordered degree-7/15/13 Chebyshev/Clenshaw stages, and reconstruction. Bootstrap does not compute ReLU. | 19 refreshes with targets 15x16/17x1/18x2, stage depth 3+4+4=11, coefficient bytes/order/hash, and independent clear polynomial oracle. Wrong B/reason/target/stage/context rejects. |
| C4: state/requirement propagation | Transfer exact descriptor/config, cipher/plain class, TY/layout/slots, level/scale/components/precision, pending actions, key class and signed rotation through every proposed result. Insert explicit alignment/rescale/relin/encode steps when the reviewed recipe requires them. Recompute depth/keys/rotations from the DAG. | Unary/binary and cross-operator positives; missing key, wrong rotation, depleted level, low precision, mismatched state/TY/slot, forward reference, or unresolved action rejects before mutation. Retain operation/key/rotation/depth report. |
| C5: whole-PU policy | Group byte-identical complete PU plans, register with `DSL_PU_Transaction_Register_Policy`, map variants to source PU/value/static ordinal, add B formals, route nine calls and bind 18 called B actuals plus one root B TCON. Use owner-qualified clone-value results. Measure variant count after C2-C4. | Equal contexts reuse, changed non-ReLU plan splits, two ReLUs keep two B formals, and 129 existing call-ABI roles remain correct. Missing actual, wrong owner/TY, incomplete signature or duplicate route rejects before apply. Retain variant/call/origin report. |
| C6: expansion/all-PU gate | Turn reviewed plans into `DSL_CKKS_EXPANSION_REQUEST`s and call `VHO_FHE_CKKS_Expand_And_Bind_States` for resident variants. Preserve source/group ordinals and returned value IDs. Verify 147 source-context events have complete step coverage, unique source finals, no live Conv/BN/`common.relu`, and concrete state/source position for every live result. | Two-PU shared-callee and full six-source-PU tests; late expansion/binding/image failure publishes nothing. Retain before/after traces and event-to-step/origin-to-clone maps. |
| C7: mapped checkpoint | Use the existing whole-program checkpoint to write all executable PUs and managed images only after C6. Atomically publish `.ckks_ops.B`; reopen with separate-process `ir_b2a -st -src` and original source pathname. | Retain `.B`, `.T`, source/payload references, phase trace, commands, diagnostics, JSON census and SHA256SUMS. Inspect logical CKKS nodes/direct kids, states, keys/rotations, typed B ABI, origin and source lines. Induced failure leaves no final or `.tmp`. |
| C8: independent certification | Compare mapped result with input manifest, independent 87/147 census, 19-context policy, clear tensor/slot and polynomial oracles, and fresh state/depth/key/rotation calculation. Run full native syntax/layout and backend-link lanes. | Signed-off report with measured PU/primitive counts, all event/group/final links, 19 bootstraps, no live high-level computation, negative results and artifact hashes. State explicitly that C ABI lowering/provider execution are untested. |

## Ownership And Review Checkpoints

FHE producer code belongs under `osprey/be/vho/fhe_ckks_*.{h,cxx}` and
focused tests. It consumes `DSL_IR_Expand_Native_Value_To_CKKS_Events`
through the FHE state-binding adapter and the generic
`DSL_PU_Transaction_{Register_Policy,Preflight_Resident,Apply_Resident}`
service through its registered policy. Shared logical operators, WN
construction, mapped images/ELF, printer/gatekeeper, clone transaction, and
checkpoint writer remain main/common-owned. FHE must not mutate raw
WN/ST/TY or mapped rows to bridge a missing API.

1. After C1, review canonical encoding and equality exclusions,
   particularly per-caller B bytes versus typed B role.
2. Before C2, reconcile and freeze the O0 Conv layout/iteration policy;
   the unresolved Fhelipe O0 baseline proposal and later MetaKernel O2
   proposal cannot silently change it.
   After C2-C4, review that Conv recipe, exact ReLU DAG,
   numerical oracle, and depth/key/rotation census before mutation.
3. After C5, review measured variant signatures, B routes, source-to-clone
   identities and complete call ABI.
4. After C7-C8, inspect retained separate-process `.T`, all negative
   publication evidence, and independent report before closure or S6-0d.

If a shared image, clone, checkpoint, or `ir_b2a` service cannot express a
required fact, pause C5-C7 for a narrow main-owned contract request. Do not
invent a local physical opcode or bypass the binary WHIRL boundary.

## Test Matrix And Exit

Run focused serializer, Conv mapping, ReLU, state-transfer, signature,
expansion, and checkpoint tests first; then the native syntax/target-layout
matrix, backend builds, and real six-source-PU Docker lane. Prove `be.so`
and `lw_inline` have no `DSL_Builder_*` or `Json::` symbol dependency.
Retain all evidence through a host bind mount and clean the stage directory
at the *start* of its next run.

Negatives include changed payload, missing/duplicate context/event, bad
Conv layout, wrong coefficient/B, unavailable rotation/bootstrap/relin key,
invalid level/scale/precision, cross-PU value, incomplete B ABI, signature
collision, late clone/expansion failure, and malformed mapped image. Every
failure leaves no final or temporary `.ckks_ops.B`.

S6-0c closes only when the independently reopened `.ckks_ops.B` proves
complete executable steps/provenance/states, 19 explicit pre-ReLU refreshes,
no live high-level Conv/BN/ReLU, exact typed B dataflow, and measured
key/rotation/depth/PU-variant census. It neither certifies the frozen
SYNC-5 C ABI mapping nor admits ACE `FHErt_ant`.
