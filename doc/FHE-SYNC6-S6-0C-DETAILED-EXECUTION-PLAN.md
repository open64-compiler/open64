# S6-0c Detailed Execution Plan: Complete CKKS Semantic IR

Status: execution in progress, not certification. S6-0a and the generic
resident whole-PU transaction are merged. C0 has a retained, independently
rerun source-family artifact replay; C1 has a structured canonical event-plan
serializer and linked whole-PU grouping fixture. Neither is yet the native
program policy or a complete real circuit producer. No
`secure_resnet20.ckks_ops.B` exists. The normative gate is
`FHE-SYNC6-CKKS-IR-CONFORMANCE-GATE.md`.

C2's first bounded column-first Conv recipe is implemented and checked
against both a nonuniform stem-like fixture and the replay-authenticated
folded stem weight/bias bytes. Its slot oracle covers every active output
and rejects unsupported stride, insufficient slots, invalid weights, and
tampered recipe metadata without output mutation. This is a local recipe
test, not materialized CKKS WHIRL, runtime execution, or certification of
the remaining projection/stride/channel contexts.

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

The C2 focused evidence is retained under
`/private/tmp/open64-fhe-sync6-s6-0c/artifacts/focused/conv-recipe/`:
`run.log`, `stem_folded_manifest.json`, and the local extracted folded
float32 fixture. Extraction requires the replay-pinned whole-file
SafeTensors SHA-256 and exact tensor shape/dtype/ranges. No payload
bytes are checked into Git. The current recipe admits only batch-one,
square, 3x3 stride-one same Conv with all input/output channels packed in
one 32768-slot ciphertext. The captured stride-two projection Convs are
explicitly outside this first case. C2 remains open until real event-plan
serialization, bounded full Conv coverage, and materialized IR census.
The same independent tensor-to-slot oracle now also passes captured
call4 `32x16x16 -> 32x16x16` folded Conv2 (9,216 live terms, 566 signed
rotation keys, 8,192 output slots) and call7 `64x8x8 -> 64x8x8` folded
Conv2 (36,864 live terms, 1,142 keys, 4,096 output slots). Their exact
folded-byte hashes are retained in separate local manifests beside the
test log. These are proof of the fixed stride-one recipe over three
captured sizes, not graph-wide CKKS state or key certification.

### C2 Stride-Two Mapping Decision

The current recipe's rotation is constant per `(output_channel,
input_channel, kernel_y, kernel_x)` only because stride-one same Conv has
equal packed input/output spatial indexing. For a stride-two Conv with
input width `IW`, output width `OW`, input channel `ci`, output channel
`oc`, kernel coordinate `(ky,kx)`, and padding `(ph,pw)`, a dense
channel-major output slot `(oc,oy,ox)` needs the input-minus-output
rotation

```text
r = ci*IH*IW - oc*OH*OW
    + oy*(stride_h*IW - OW) + ox*(stride_w - 1)
    + (ky-ph)*IW + (kx-pw)
```

For a one-channel `4x4 -> 2x2` stride-two `1x1` Conv with zero padding,
the four output slots `(oy,ox)=(0,0),(0,1),(1,0),(1,1)` require rotations
`0,1,6,7` respectively. One term-wide rotation cannot produce the four
correct source slots. The first recipe must reject this shape; extending
its constant-rotation formula would silently compute the wrong Conv.

The following is a deterministic *unoptimized direct-position cost
census*, not an executable plan or key inventory. It assumes one separate masked
rotate/multiply contribution per valid output coordinate and folded
OIHW term. `signed keys` deduplicates the resulting nonzero signed
offsets; runtime rotation-key generation may further canonicalize them.

| Captured projection shape | Kernel | OIHW terms | Direct position masks | Distinct signed offsets | Max offsets per term |
| --- | ---: | ---: | ---: | ---: | ---: |
| `16x32x32 -> 32x16x16` | `1x1` | 512 | 131,072 | 23,551 | 256 |
| `16x32x32 -> 32x16x16` | `3x3` | 4,608 | 1,131,008 | 24,049 | 256 |
| `32x16x16 -> 64x8x8` | `1x1` | 2,048 | 131,072 | 12,031 | 64 |
| `32x16x16 -> 64x8x8` | `3x3` | 18,432 | 1,083,392 | 12,153 | 64 |

The bounded candidate for review is **high-resolution Conv followed by
an explicit local stride compaction**. Perform the ordinary constant-
rotation stride-one convolution into its sparse/high-resolution plane,
then select and pack the even spatial coordinates into the declared dense
output layout using explicit masks, rotations, and verified CKKS state
transitions. For the two captured shapes the temporary high-resolution
outputs occupy 32,768 and 16,384 slots respectively, within the selected
32,768-slot ciphertext. The high-resolution Conv would have 4,608 terms
and at most 422 signed offsets for the first `3x3` projection, and 18,432
terms and at most 854 for the second; **compaction cost, key set, depth,
and correctness are not yet measured or approved**. A fixed compaction
algorithm must pass an independent tensor-to-slot oracle, exact rotation/
mask/state census, and residual-layout compatibility before adoption.

Keeping sparse/gapped output layout across following operators would need
graph-wide layout conversion and is outside this provisional C2 recipe.
Per-position direct masking has the measured million-plus contribution
matrix above and is not admitted as an implicit fallback. MetaKernel or
another general layout planner remains a separately reviewed later design.
If local compaction cannot be bounded without hidden CKKS state repair or
new per-shape exceptions, stop C2 for an architecture decision; do not
mark all ResNet Convs covered or produce `secure_resnet20.ckks_ops.B`.
The table and counterexample are reproducible with
`osprey/be/vho/tests/fhe_ckks_conv_stride_cost.py`; its JSON output is
retained as review evidence, not a compiler planning image.

#### Read-Only Local Compaction Probe

`osprey/be/vho/tests/fhe_ckks_stride_compaction_proof.py` implements a
clear-slot bit-compaction proof without modifying WHIRL. A selection mask
keeps the even `(y,x)` positions of a high-resolution output. For each
remaining `x`, `y`, then channel bit, it uses complementary plaintext
masks, one signed left rotation, and an add to move the selected branch
from its original bit weight to the dense-output bit weight. The mask
generation rule, exact slot-order mask SHA-256 values, cardinalities,
and rotation at every stage are retained in
`/private/tmp/open64-fhe-sync6-s6-0c/artifacts/focused/conv-recipe/stride-compaction-proof.json`.
No per-output-position rotate/mask contribution is emitted.

The `4x4 -> 2x2` case uses one selection plus two bit-move stages:
five plaintext masks, signed rotations `{1,6}`, and a symbolic three
mask-multiplication levels. The replay-authenticated call4 `1x1` folded
projection `16x32x32 -> 32x16x16` matches an independent direct stride-two
tensor Conv for every one of 8,192 active output slots (maximum clear
absolute error `0`). The high-resolution temporary occupies 32,768
slots. Compaction uses one selection plus 13 bit-move stages: 27
plaintext masks, 13 rotations/adds, and signed keys
`{1,2,4,8,48,96,192,384,768,1536,3072,6144,12288}`. If each
complementary plaintext-mask pair consumes one level after rescale, the
compaction alone consumes **14 sequential levels**, on top of the
high-resolution Conv's plaintext multiply. Each mask/rotate/add must
carry matching scale/level; component count remains two only under the
usual cipher-by-plaintext/rotate/add contract. These are symbolic
transitions, not executed CKKS evidence or a proven precision budget.

The result's dense channel-major `32x16x16` slot order matches the
source projection's declared NCHW output. In the retained source `.T`,
the projection `cnn_conv2d_11`, normal-path `cnn_conv2d_16`, and
`common_residual_add_18` all use canonical result `T<93>`; the
`common.residual_add` contract has `shape_check=exact`. This proves a
*layout/type* join, not ciphertext
level/scale/precision compatibility of the two residual paths. The
14-level compaction is too costly to admit without checking the actual
context-specific post-ReLU capacity and any explicit alignment steps.
No implicit bootstrap or state repair is allowed. **Stop C2 stride-two
emission here** pending a separately reviewed lower-depth local packer,
an approved state schedule, or a general layout planner decision. The
proof does not complete C2 or authorize `.ckks_ops.B` publication.

| Slice | FHE-owned work | Focused exit evidence |
| --- | --- | --- |
| C0: replay/input gate | Add a deterministic input manifest and native read-only gate joining six-PU images, 87/147 events, 19 contexts, source/payload/coefficient hashes, and FHE/DSL validators. Reject the older `.fhe.B` and unsupported config before planning. | Stable hashed census, `-st -src` with six FUNC_ENTRYs/nine calls/nonzero source interleave; wrong hash or missing context rejects without output. |
| C1: canonical plan serializer | Define a bounded structured per-event CKKS step plan and versioned byte encoding for operation/operand/group-output/TY/layout/state/key/rotation/B-role facts. Validate before encoding; opaque producer-supplied bytes alone are not a legal plan. | Equal semantics encode identically despite allocation/name order; changed non-ReLU operation/state/key/rotation changes bytes. Missing field, bad operand, duplicate event, or truncated encoding rejects. Retain decoded plan and hashes. |
| C2: non-ReLU recipes | First certify one fixed, deterministic ACE-style column-first Conv correctness recipe with explicit NCHW/OIHW domain limits, packing, signed rotations, plaintext masks/weights, accumulation, and bias. Use verified folded bytes and a clear tensor-to-slot oracle. This provisional recipe is neither the selected Fhelipe O0 baseline nor MetaKernel/SYNC-7 optimization. Only then expand to the 13 Conv definitions/21 contexts and residual/pool/flatten/linear if the recipe remains bounded. Unsupported cases fail closed; do not silently substitute im2col. | One representative nontrivial Conv and an unsupported-case negative first; then per-operator oracles, measured materialized-IR rotation/key/state census, and exact external payload joins. Stop for review if graph-wide layout assignment/conversion, hidden state repair, or a growing special-case matrix appears. |
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
2. C2 uses a provisional fixed ACE-style column-first Conv recipe for
   correctness only. Its first review is one nontrivial Conv plus a negative;
   it does not decide the unresolved Fhelipe O0 baseline proposal or admit
   MetaKernel O2. If the fixed recipe needs graph-wide layout decisions,
   unexpected state repair, or proliferating cases, stop and quantify the
   gap rather than adding workarounds. After C2-C4, review the bounded Conv
   recipe, exact ReLU DAG,
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
