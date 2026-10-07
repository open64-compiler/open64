# S6-0c Detailed Execution Plan: Complete CKKS Semantic IR

Status: execution in progress, with the full Conv and residual-add subgraphs
certified as CKKS-conforming. S6-0a and the generic resident whole-PU transaction are
merged. C0 has a retained, independently rerun source-family artifact replay;
C1 has a structured canonical event-plan serializer and linked whole-PU
grouping fixture. The 2026-10-06 Conv checkpoint described below closes the
Conv portion of C2 and C6/C7. The 2026-10-07 residual checkpoint closes all
nine exact-shape residual joins. The circuit remains incomplete: ReLU,
pooling, flatten, and linear still require executable CKKS
expansion. The normative gate is `FHE-SYNC6-CKKS-IR-CONFORMANCE-GATE.md`.
The end-to-end copy-on-write parameter path, from immutable source SafeTensors
through derived plaintext side assets and explicit `ckks.encode` nodes, is
documented for implementers in
`FHE-SYNC6-RAW-TO-CKKS-PLAINTEXT-FLOW.md`.

## Conv CKKS Conformance Checkpoint

The real ten-PU specialized ResNet artifact now lowers all 21 physical Conv
definitions to explicit CKKS semantic WHIRL. The all-PU materialization
checkpoint reports 5,691 authenticated column-first feature rows, 21 expanded
biases, 104 logical stride-compaction masks, and 28,927 CKKS operations. A
separate rebuilt `ir_b2a -st -src` process reopens the published binary and
proves:

- 10 `FUNC_ENTRY`s and nonzero `secure_resnet20.py` source locations;
- zero executable physical `OPR_DSLCONV2D` nodes;
- exactly 21 retained Conv image rows marked `status=lowered` with
  `relation=ckks_expansion`;
- 28,927 executable physical CKKS nodes; and
- context-owned external row/bias/mask references to the atomically published
  plaintext side asset.

Retained evidence is under
`/private/tmp/open64-fhe-sync6-s6-0c/artifacts/conv-ckks-materialized-final/`.
The principal hashes are:

| Artifact | SHA-256 |
| --- | --- |
| `secure_resnet20.ckks_ops.B` | `f7c4c3eff5dbff0af6d1fb5aecf149ae0c8fbaffb4a697db68842e575d1d6bd6` |
| `secure_resnet20.ckks_ops.B.conv-plaintexts.f32` | `50772c653e1c4a5fbd6ff253a5799d418a4f2fa8f61f55ffa389fae391f24e2e` |
| `secure_resnet20.ckks_ops.T` | `58cd605273b1b4b14f46b2b78ef194cdbb98bbc97e0e6c2c6a3edbe02c953dd8` |

The side asset is 202,620,928 bytes. Tensor TCON interning is semantic, so
byte-identical masks in different Conv contexts intentionally share one
canonical physical range. The producer must use the physical range returned
by the interned TCON and omit duplicate bytes; it must not retain a later
candidate offset that disagrees with the canonical TCON. Generated values
remain context-distinct through geometry/variant/stage provenance even when
their bytes and TCON are shared.

The grouped-expansion fault-injection regression also proves that a late
failure restores physical reads, call ABI values, REGION interfaces, managed
DSL/CKKS rows, strings, symbols, and runtime projections. Runtime projection
redirection is strict when the optional runtime-interface image is present;
its absence remains legal for pre-projection fixtures. The stale-destination
negative rejects with `CFHEMAT-CHECKPOINT-006`, preserves the published
binary hash, and leaves no `.tmp` file.

This artifact name records the intended stage, not completion of every model
operator. Its trace still contains 19 executable `common.relu` definitions,
9 residual adds, one global-average pool, flatten, and linear. It therefore
must not be used as the final S6-0c gate or passed to CKKS2C yet.

## Residual-Add CKKS Conformance Checkpoint

The post-Conv producer now resolves each resident `common.residual_add.v2`
from its managed node/value identity and reads the latest concrete state of
both redirected operands. It requires exact result/operand TY, descriptor,
layout, scale, slots, alignment group, and key-set compatibility. A branch at
a higher modulus level is changed only by an explicit `ckks.modswitch`; the
final join is one `ckks.add`. No implicit rescale, bootstrap, type mutation,
or provider call is permitted.

The real ten-PU checkpoint proves nine residual joins. The seven identity
shortcuts arrive at level 4 while their Conv branches arrive at level 3, so
each has one visible level-4 to level-3 modswitch. The two projection joins
arrive already aligned at level 3 and need only the add. All nine results have
scale 56, two components, precision 30, 32,768 slots, and no pending action.
The source residual rows remain as lowered provenance; no physical
`OPR_DSLRESIDUALADD` remains executable.

Separate-process `ir_b2a -st -src` evidence is retained under
`/private/tmp/open64-fhe-sync6-residual-add/materialized/`. It contains 28,943
CKKS events: the certified 28,927 Conv events plus seven modswitches and nine
adds. The residual report records `contexts=9`, `ckks_adds=9`,
`ckks_modswitches=7`, `projection_joins_already_aligned=2`, and
`source_residual_nodes_live=0`. Principal hashes are:

`osprey/be/vho/tests/fhe_ckks_residual_checkpoint_audit.py` independently
checks those report facts against the mapped logical/physical node census,
the 16 concrete result-state rows, the nine stable source ordinals, the
28,943-event header, and the intentionally still-live 19 ReLU definitions.
Missing or contradictory evidence rejects with
`CFHEIR-RESIDUAL-AUDIT-001`.

| Artifact | SHA-256 |
| --- | --- |
| `secure_resnet20.ckks_ops.B` | `9f8f314dc681697372ed4f56cf0dae9189a19fe5f98121103c255289293a7b03` |
| `secure_resnet20.ckks_ops.B.conv-plaintexts.f32` | `50772c653e1c4a5fbd6ff253a5799d418a4f2fa8f61f55ffa389fae391f24e2e` |
| `secure_resnet20.ckks_ops.B.residual-report.txt` | `ee9942c25711cd91b4190bbb2c03879e332f20c2a0ea0705e6a590ad5f05b2de` |
| `secure_resnet20.ckks_ops.T` | `bb235f4942ef2c9ae2cc61e417e693226cb5ccf702c01c1b166683c609b130a6` |

This remains a bounded checkpoint. The trace intentionally retains 19
physical ReLUs plus global-average pool, flatten, linear, and output-logits
source semantics. It must not enter CKKS2C until the complete C6/C7 gate
passes.

Two C2 main/common prerequisites are implementation-complete and accepted by
the FHE consumer. The source-derived typed-row transaction merged through PR
#173. The source-free generated-mask transaction merged through PR #174 at
`0325ce74`. The first FHE-owned admission fixture consumes both services and
reopens mapped row/mask evidence. This does not close C2; the producer must
still materialize and verify the complete real-model recipes.

C2's first bounded column-first Conv recipe is implemented and checked
against both a nonuniform stem-like fixture and the replay-authenticated
folded stem weight/bias bytes. Its slot oracle covers every active output
and rejects unsupported stride, insufficient slots, invalid weights, and
tampered recipe metadata without output mutation. This is a local recipe
test, not materialized CKKS WHIRL, runtime execution, or certification of
the remaining projection/stride/channel contexts.

The next C2 preparation slice retains one diagnostic raw F32 side file and
per-row index for the 17 replay-authenticated stride-one contexts. It checks
all 5,211 row ranges/hashes, repeats byte-for-byte, and rejects mid-write
failure without final files. These are host evidence only: the Python test
fixture is not a backend producer, and the row values are not yet in WHIRL.
The generic rank-4-source to rank-1-result value transaction requested in
`FHE-SYNC6-TYPED-ROW-VALUE-HANDOFF.md` remains main/common-owned. Its reviewed
implementation merged through PR #173 and is now the authoritative typed-row
service for the FHE producer.
The direct bit-move stride packer below is the simpler `-O0` semantic
candidate. Fused-diagonal packing is retained only as `-O1` research.
The direct packer now has a provider-independent explicit CKKS event-plan
constructor for every captured Conv shape. It is not yet bound to all live
model identities or published as mapped WHIRL.

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
   Variant-local external Conv rows are constants, not formals: include their
   ordered transform identities and authenticated content digests in plan
   equality, and split variants whenever those bytes differ. Before
   materialization, prove each caller-owned folded source is the exact
   call-ABI actual for that Conv context.
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

The next FHE-owned slice constructs canonical executable event plans for all
captured Conv shape families. `fhe_ckks_conv_plan.{h,cxx}` emits explicit
input duplication, row rotation, external plaintext encode, plaintext
multiply, rescale, accumulation, bias encode, and bias add operations. For
stride-two source operators it first evaluates the admitted high-resolution
1x1 or 3x3 Conv, then applies the accepted sequential selection/bit-move
network with an explicit `DEPTH_EXHAUSTION` capacity refresh. No provider call
or implicit state repair is hidden in the plan. The plan is accepted by the
same canonical serializer used for whole-PU variant equality, and
`fhe_ckks_conv_expand.{h,cxx}` is the only adapter to the reviewed native CKKS
expansion/state-binding transaction.

The focused plan census is:

| Captured family | Canonical steps | Encoded plaintexts | Final level |
| --- | ---: | ---: | ---: |
| stride-one `3 -> 16`, width 32, 3x3 | 147 | 28 | input - 1 |
| stride-one `16 -> 16`, width 32, 3x3 | 722 | 145 | input - 1 |
| stride-one `32 -> 32`, width 16, 3x3 | 1,442 | 289 | input - 1 |
| stride-one `64 -> 64`, width 8, 3x3 | 2,882 | 577 | input - 1 |
| stride-two `16 -> 32`, width 32, 1x1 | 190 | 44 | 3 |
| stride-two `16 -> 32`, width 32, 3x3 | 830 | 172 | 3 |
| stride-two `32 -> 64`, width 16, 1x1 | 264 | 58 | 3 |
| stride-two `32 -> 64`, width 16, 3x3 | 1,544 | 314 | 3 |

`Encoded plaintexts` counts feature rows, the expanded bias, and
stride-compaction masks. The four stride-two plans consume 27, 27, 25, and 25
compaction masks respectively. This closes the per-shape algorithm and
state-transition construction requirement; it does not close C2/C3
publication. The remaining producer work is to derive and atomically publish
every context-owned row, bias, and mask value, specialize shared PUs by
complete plan bytes, remap clone-local provenance/state, execute native
expansion for all 21 contexts, and reopen the resulting six-PU family.
Retained plan evidence is under
`/private/tmp/open64-fhe-sync6-s6-0c/artifacts/focused/conv-plan/`.

The complete C2 plaintext family is now produced and independently verified
for all 21 Conv contexts. The native read-only collector joins each dynamic
source event to its live `OPR_DSLCONV2D.v2` node, exact callee-identity
BatchNorm-fold provenance, folded weight/bias TCONs, canonical tensor shapes,
and source attributes. It preserves source stride while normalizing the
executable Conv recipe to high-resolution stride one; legal `padding=0,0` for
1x1 projections remains distinct from nonzero shape/stride/group invariants.
The side asset contains 5,691 ACE column-first F32 feature rows, 21 expanded
F32 biases, and 104 explicit F32 stride-compaction masks. Its 5,816 byte
ranges are contiguous and individually hashed in a deterministic index bound
to the source replay, source `.B`/`.T`, folded payload, Conv budget, source
node/value/context/callsite, and folded TCON identities. The retained asset is
209,436,672 bytes with SHA-256
`1902378db8d8a9a607616e7f85005b9892d160d0be5deb741e9fc3ed7d648c88`.
Repeated generation is byte-identical; injected mid-write failure publishes
no endpoint; modified bytes fail whole-file and range verification. This
closes plaintext asset completeness for C2, but the values are not yet
materialized into mapped WHIRL and no Conv source node has yet been replaced.
Retained evidence is under
`/private/tmp/open64-fhe-sync6-s6-0c/artifacts/focused/conv-all-plaintext-test/`.

A separate FHE-owned native arithmetic fixture now exercises the actual
WHIRL expansion and state-binding transaction on canonical `float32[2]`
tensors. It lowers exact-shape `common.add.v1` to one `ckks.add.v1`,
adds an explicit `ckks.encode -> ckks.add` case for plaintext tensor input,
lowers `common.mul.v1` with a raw tensor constant to `ckks.encode ->
ckks.mul -> ckks.rescale`, and lowers ciphertext-ciphertext
`common.mul.v1` to `ckks.mul -> ckks.relin -> ckks.rescale`. The four
mapped `.B` files reopen in a separate
`ir_b2a -st -src` process with lowered source provenance and concrete
value-state rows. An independent two-slot clear algebra oracle matches the
four ordered CKKS step sequences; it does not execute encryption. Layout
mismatch, an unrepaired terminal multiply, and a
wrong relin key reject before native mutation. The retained focused traces
are under `/private/tmp/open64-fhe-sync6-s6-0c/artifacts/focused/tensor-arithmetic/`.
These certify four small operator transformations, not full ResNet event
planning, Conv assets, runtime arithmetic, or `-O0` numerical accuracy.

The same producer now retains a fifth, separate capacity-refresh fixture.
It lowers a `common.add` whose left ciphertext is level 7 with pending
`DEPTH_EXHAUSTION` into explicit `ckks.bootstrap -> ckks.add`. The refresh
targets level 17 and is followed by an aligned level-17 addition; the
mapped `.B` reopens with the bootstrap reason, key requirement, source
provenance, and both value states visible in `ir_b2a -st -src`. Mismatched
reason or key rejects before native mutation. This validates the FHE
adapter's generic reason/state/key plumbing only. The fixture reuses the
existing `pre_relu_refresh_v1` key profile as a planning requirement; it
does not certify an ACE provider bootstrap at this capacity boundary. It
does not approve the
two proposed ResNet stride-two refreshes, their numerical tolerance,
provider target support, or the full packing network. Retained evidence is
under `/private/tmp/open64-fhe-sync6-s6-0c/artifacts/focused/tensor-arithmetic-capacity/`.

The producer-side recipe now also constructs one plaintext diagonal mask
at a time by grouping every live OIHW term with the same *signed rotation*.
It rejects overlapping coefficients at the same output slot; the
independent grouped rotate/mask/add clear oracle matches the tensor
oracle for all three captured shapes. The stem has 162 masks from 432
live terms; call4 has 567 from 9,216; call7 has 1,143 from 36,864.
This remains an independent numerical oracle, not the selected external
asset layout. The selected ACE-aligned path serializes one transformed
feature row at a time as little-endian F32, in output-channel/spatial-column
order. The focused C++ test checks exact row permutation, positive-zero
padding, and byte encoding. Neither representation has emitted WHIRL.

#### Full-Model ACE-Style Row Budget And Stop Decision

The read-only `fhe_ckks_conv_mask_budget.py` (report schema
`open64.fhe.sync6.conv-plaintext-budget.v2`) joins each BN-fold provenance
row to its Conv disposition, source/result tensor descriptors, and runtime
input `source_tcon` role. It authenticates the retained `.T` and folded
SafeTensors whole-file hashes against the source replay, then verifies each
F32 weight/bias slice against its printed URI offset, length, and checksum.
Owner PU, source Conv node/value, and callsite remain the identity; tensor
names are used only after the managed TCON-to-role join to locate bytes.
There are 13 physical Conv definitions and 21 folded call contexts; the
bounded stride-one 3x3 recipe covers 17, while four stride-two contexts
remain excluded. No mask side file or WHIRL is produced by this probe.

| Folded Conv shape, input HxW | Contexts | F32 rows/context | Total rows | Raw F32 bytes |
| --- | ---: | ---: | ---: | ---: |
| `16x3x3x3`, `32x32` | 1 | 27 | 27 | 1,769,472 |
| `16x16x3x3`, `32x32` | 6 | 144 | 864 | 56,623,104 |
| `32x32x3x3`, `16x16` | 5 | 288 | 1,440 | 47,185,920 |
| `64x64x3x3`, `8x8` | 5 | 576 | 2,880 | 47,185,920 |
| **Supported stride-one total** | **17** | | **5,211** | **152,764,416** |

Rows are exactly `C_in * 9`, each of length `C_out * H * W`; the selected
raw F32 total is about 145.69 MiB before an index, provider encoding, or
alignment. All 5,211 rows are byte-distinct in this authenticated capture.
The budget probe reconstructs and hashes every row from the
replay-authenticated folded bytes. The former signed-rotation-group
alternative has 10,386 masks and would occupy 1.268 GiB as F32 or
2.536 GiB as F8 at 32,768 slots. Its 5,715-step C1 serializer probe was
only a capacity test with placeholder operators/states and is **not** the
ACE-row execution schedule. The subsequent native Conv plan fixture now
certifies the legal per-row rotation/encode/multiply/rescale/accumulation
sequence and its concrete level/scale/component/precision tuples for all eight
captured shape families. Full-model owner/context binding and mapped-image
publication remain open.

**Proceed with external F32 rows, not lazy runtime derivation.** Keep the
raw slices and bounded index outside `.B`, authenticate each row and whole
file, and publish them transactionally before `.ckks_ops.B`. The mapped IR
must bind each rank-1 F32 external value to its exact source Conv/context
and make it the direct operand of explicit `ckks.encode`. The merged same-TY
materialization API cannot transform a rank-4 folded weight into rank-1 row
values. Main/common implemented and merged a separate atomic typed
external-constant service through PR #173. Consume only its returned
owner-qualified value IDs during mapped publication. The ACE worker's
`RT_DATA_WRITER`
envelope and `Pt_from_msg` encoding are S6-0d adapter concerns, not a
reason to link ACE in this provider-independent stage. The four stride-two
contexts, C3 evaluator identity, and C4 state alignment remain separate.

Retained evidence:
`/private/tmp/open64-pr166-ace-aligned/conv-budget/full-model-mask-budget.json`
and `/private/tmp/open64-fhe-sync6-s6-0c/artifacts/focused/plan-bytes/run.log`.
The budget test repeats the full analysis byte-for-byte and rejects a
changed folded payload without publishing a report.
The selected F32 row contract and exact shared API gap are in
`FHE-SYNC6-CONV-MASK-ASSET-OPTIONS.md` and
`FHE-SYNC6-TYPED-ROW-VALUE-HANDOFF.md`. The retained diagnostic index is
`/private/tmp/open64-fhe-sync6-row-assets/first/ace-conv-rows.index.json`;
its raw F32 side file is adjacent and is not checked into Git.

After PRs #173 and #174 merged, the FHE-owned
`fhe_ckks_conv_assets.{h,cxx}` admission layer became the only C2 entry point
for these generic transactions. It proves the exact folded rank-4 F32 OIHW
source, bounded recipe, dense feature-row ordinal sequence, rank-1 F32 result
length, fixed transformation identity, and exact authenticated
geometry/variant digests before delegating mutation. It does not create WN,
ST, TY, TCON, or mapped rows directly. A linked fixture atomically rejects a
bad row ordinal and wrong geometry digest, then materializes all nine rows for
a legal `2x1x3x3`, `4x4` Conv plus two source-free masks. Separate-process
`ir_b2a -st -src` shows owner-qualified typed-row lineage, generated-mask
geometry/variant provenance, external ranges, canonical TYs, and nonzero
source positions. Retained evidence is under
`/private/tmp/open64-fhe-sync6-s6-0c/artifacts/focused/conv-assets/`.
This is a bounded native integration fixture, not the 5,211-row ResNet
materialization, an executable Conv DAG, or CKKS state/key certification.

The next FHE-owned clear-schedule check follows ACE's **unblocked** feature
row loop after input duplication. For every row it records the signed
spatial/channel rotation, raw F32 coefficient row, and distinct nonzero
rotation requirements, then clear-evaluates rotate/multiply/accumulate plus
bias. The independent OIHW tensor oracle matches all active outputs for the
synthetic stem and replay-authenticated stem, call1 Conv2, call4 Conv2, and
call7 Conv2 fixtures. Their input-copy counts are 7, 2, 2, and 2;
distinct row/duplication signed rotations are 32, 144, 288, and 576.
These four fixtures cover every admitted stride-one channel/width family.
The retained trace is
`/private/tmp/open64-fhe-sync6-row-schedule/run.log`. This does not yet
serialize legal CKKS steps or prove scale, level, precision, encoded
plaintext state, or runtime key availability. ACE fast blocking remains
outside the O0 correctness schedule. The copy count follows ACE's power-of-two
slot-cap rule; a `Cin=1, Cout=32, H=W=32` boundary uses 32 copies, not 33.

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

The `-O0` candidate for review is **high-resolution Conv followed by
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
read-only `fhe_ckks_stride_all_contexts_proof.py` now authenticates the
source trace, folded payload, and independently regenerated Conv budget,
then checks all four captured stride-two contexts: both 1x1 projections
and both 3x3 normal-path Convs at callsites 4 and 7. High-resolution
Conv followed by the sequential compaction network matches an independent
direct stride-two NCHW oracle with zero clear-slot error in all four
contexts. The first shape has 8,192 active output slots and 14 symbolic
mask levels; the second has 4,096 and 13. Duplicate, missing, or unknown
context, wrong trace/weight hash, and changed payload reject without a report.
Joining both branches to the approved static CKKS schedule proposes one
`DEPTH_EXHAUSTION` refresh at level 18 for `layer2.0` and level 17 for
`layer3.0`; the respective normal and projection outputs meet at symbolic
level 3. A changed schedule level rejects before report publication.
The retained proof and negative logs are under
`/private/tmp/open64-fhe-sync6-s6-0c/artifacts/focused/stride-all-four/`.
This proves clear geometry only, not generated mask values in WHIRL.

For the first shape, 14-level compaction exceeds the level-7 input
capacity; correctness requires an explicit reviewed capacity refresh,
not an unrecorded level reduction.
The simple `-O0` proposal refreshes the block input once before the normal
and projection branches fork, then applies the same sequential bit-move
network to both stride-two Convs. The existing generic reason
`DEPTH_EXHAUSTION` identifies this separate boundary; it is not one of the
19 mandatory `PRE_RELU_REFRESH` boundaries and must not be merged with one.
The read-only schedule join below proposes post-refresh levels 18 and 17
for `layer2.0` and `layer3.0`, respectively. The FHE unary transfer now
admits an explicit `DEPTH_EXHAUSTION` bootstrap with matching reason/state
and key preflight; a separate mapped fixture proves bootstrap followed by
addition. This does not certify the actual two ResNet refresh boundaries,
provider target levels, precision, or numerical accuracy. No implicit
bootstrap or state repair is allowed. **Stop C2 stride-two emission here**
pending capacity, precision, key, and numerical certification of this
simple schedule. The
proof does not complete C2 or authorize `.ckks_ops.B` publication.

#### O1 Fused-Diagonal Research

The same ordered bit moves can be composed in bounded groups. For
each group, partition its selected input slots by their exact cumulative
signed rotation. Multiply the input by one 0/1 plaintext mask per partition,
rotate each masked ciphertext once, then add all terms. Every selected source
slot belongs to exactly one partition and every selected output slot has
exactly one predecessor. This is a linear masked-rotation transform, not a
change to Conv semantics or an implicit bootstrap. The first group includes
the even-position selection, so it needs no separate selection level.

The read-only `fhe_ckks_stride_compaction_proof.py` compares this fused
transform both with the original 13-move network and an independent
stride-two Conv oracle. It balances 13 moves as `4+4+5` for the replay-
authenticated call4 `16x32x32 -> 32x16x16` projection: 16, 16, and 32
diagonals, 64 exact masks, 61 distinct nonzero signed rotations, and three
symbolic plaintext-mask levels. With the preceding high-resolution Conv
multiply, the symbolic total is four levels, versus fifteen for the
sequential proof. The other captured projection shape
`32x16x16 -> 64x8x8` uses `4+4+4`: 48 masks, 45 nonzero rotations, and
three packing levels; its oracle uses deterministic synthetic 1x1 weights,
not model bytes. The `4x4 -> 2x2` counterexample composes into four masks
and one packing level. Exact per-stage mask SHA-256 values, rotations,
cardinalities, source-family hash, and zero-error clear checks are retained
in `/private/tmp/open64-fhe-sync6-s6-0c/artifacts/focused/conv-recipe/stride-compaction-o0-vs-o1-proof.json`.

The explicit shallower candidate groups up to seven moves: call4 uses
`6+7`, with 192 masks and 190 nonzero signed rotations, and the other
projection shape uses `6+6`, with 128 masks and 126 rotations. Both have
two symbolic packing levels and match the same clear oracles exactly.
At 32,768 slots and F32 mask storage, these alternatives require about
24 MiB and 16 MiB of uncompressed mask bytes respectively, before any
runtime encoding or key material. They trade extra assets and rotation
keys for one level of capacity. Neither fused grouping is selected for
`-O0` emission; both require later `-O1` equivalence, state, and
profitability review against the straightforward baseline.

The optional schedule-manifest join in the same proof pins
`doc/fhe-policy/sync3-relu/ckks-schedule-manifest.json` at SHA-256
`27fe104aa5a159baefd0255c82e0c9193c1ecdf28b73f97e2f8830bb1444ae62`.
For both `layer2.0` and `layer3.0`, the preceding block's approved ReLU
output is level 7. The proposed `-O0` `DEPTH_EXHAUSTION` bootstrap of the
shared block input targets level 18 and 17 respectively. One high-resolution
Conv plaintext multiply plus 14 or 13 sequential packing levels then leaves
both the normal Conv1 and projection branches at level 3. The main branch
separately performs its mandatory pre-ReLU refresh to level 15, evaluates
the approved depth-11 ReLU to level 4, and uses one level for Conv2, also
reaching level 3 before residual add. This is a **symbolic level join**, not
proof of supported extra bootstrap targets, compatible scales, noise,
precision, plaintext encoding, or available keys. It adds two proposed
capacity refreshes to the 19 mandatory pre-ReLU refreshes; acceptance must
measure their numerical effect and preserve both reasons in the IR.

The fused three-stage network can reach level 3 from the level-7 input
without that extra capacity refresh, but reducing depth is an `-O1`
optimization question. It does not displace the simpler `-O0` recipe.

Neither path is **an executable CKKS schedule** yet. For `-O0`, C4 must
prove the explicit extra bootstrap, all 14/13 individual mask/rotate/add
transitions, result scales and precision, key availability, and residual
alignment. C2 must validate transformed row and mask assets and
producer-side typed value binding. The fused group's parallel masks,
rotations, and rescale remain a later optimization proof. Until the `-O0`
gates pass, reject stride-two emission; neither clear-slot oracle alone
authorizes `secure_resnet20.ckks_ops.B` publication.

The weight-derived typed-row API is not a mask-materialization API: these
0/1 masks come from proved slot geometry, not an external rank-4 weight or
implicit-zero source. Main/common has implemented and FHE has accepted a
separate backend-safe transaction for source-free generated external tensor
constants with canonical rank-1 F32 TY, side-file TCON/range/checksum,
active-PU insertion and source position, and stable
geometry/stage/diagonal provenance. It merged through PR #174 and is consumed
only through the FHE admission layer above. FHE owns mask bytes, variant
binding, and `ckks.encode` state/key checks.
`FHE-SYNC6-GENERATED-MASK-GEOMETRY-MANIFEST.md` defines the canonical
manifest bytes, exact geometry digest, variant-signature digest, and
stage/diagonal coverage that the FHE producer must validate before invoking
that transaction.
Do not attach false `converted_from` weight lineage, insert raw WN/ST nodes,
or embed the full F32 masks in `.B`. PRs #173 and #174 provide the only native
mutation paths for source-derived rows and source-free masks, respectively.

| Slice | FHE-owned work | Focused exit evidence |
| --- | --- | --- |
| C0: replay/input gate | Add a deterministic input manifest and native read-only gate joining six-PU images, 87/147 events, 19 contexts, source/payload/coefficient hashes, and FHE/DSL validators. Reject the older `.fhe.B` and unsupported config before planning. | Stable hashed census, `-st -src` with six FUNC_ENTRYs/nine calls/nonzero source interleave; wrong hash or missing context rejects without output. |
| C1: canonical plan serializer | Define a bounded structured per-event CKKS step plan and versioned byte encoding for operation/operand/group-output/TY/layout/state/key/rotation/B-role facts. Validate before encoding; opaque producer-supplied bytes alone are not a legal plan. | Equal semantics encode identically despite allocation/name order; changed non-ReLU operation/state/key/rotation changes bytes. Missing field, bad operand, duplicate event, or truncated encoding rejects. Retain decoded plan and hashes. |
| C2: non-ReLU recipes | Use the selected ACE-aligned column-first, raw-F32 transformed feature rows as external plain operands of explicit `ckks.encode`. Join every row to verified folded bytes and exact source/call context; retain the independent signed-rotation clear oracle. Prove row-indexed rotations, accumulation, bias, and state before expanding the 21 specialized Conv definitions. Do not silently substitute input im2col, fast blocking, or runtime mask derivation. Then cover residual/pool/flatten/linear. Unsupported cases fail closed. | **Conv and residual portions complete:** PRs #173/#174 provide native row/mask creation; the real ten-PU checkpoint materializes 5,691 rows, 21 biases, 104 logical masks, and 28,927 Conv CKKS operations, then seven explicit level alignments and nine residual adds. Separate-process inspection proves zero live Conv or residual-add nodes and preserves 21+9 lowered provenance rows. Pool/flatten/linear remain open. |
| C3: ReLU recipe | In each of 19 contexts generate `ckks.bootstrap` with `PRE_RELU_REFRESH` and target 15/17/18, actual B materialization/encode/normalization, the pinned ACE BSGS-style Chebyshev addition-chain for ordered degree-7/15/13 stages, and reconstruction. Bootstrap does not compute ReLU. The original ciphertext and normalized ciphertext are distinct recipe roots, matching ACE `App_relu(input_ct0,input_ct1)`. | 19 refreshes with targets 15x16/17x1/18x2, profile depth allocation 3+4+4=11, coefficient bytes/order/hash, exact addition-chain DAG, and independent direct-Chebyshev oracle. Wrong B/reason/target/stage/context rejects. Legacy Clenshaw-tagged planning artifacts fail closed and must be recaptured before executable expansion. |
| C4: state/requirement propagation | Transfer exact descriptor/config, cipher/plain class, TY/layout/slots, level/scale/components/precision, pending actions, key class and signed rotation through every proposed result. Insert explicit alignment/rescale/relin/encode steps when the reviewed recipe requires them. Recompute depth/keys/rotations from the DAG. | Unary/binary and cross-operator positives; missing key, wrong rotation, depleted level, low precision, mismatched state/TY/slot, forward reference, or unresolved action rejects before mutation. Retain operation/key/rotation/depth report. |
| C5: whole-PU policy | Group byte-identical complete PU plans, register with `DSL_PU_Transaction_Register_Policy`, map variants to source PU/value/static ordinal, add B formals, route nine calls and bind 18 called B actuals plus one root B TCON. Use owner-qualified clone-value results. Measure variant count after C2-C4. | Equal contexts reuse, changed non-ReLU plan splits, two ReLUs keep two B formals, and 129 existing call-ABI roles remain correct. Missing actual, wrong owner/TY, incomplete signature or duplicate route rejects before apply. Retain variant/call/origin report. |
| C6: expansion/all-PU gate | Turn reviewed plans into `DSL_CKKS_EXPANSION_REQUEST`s and call the native CKKS expansion/state adapter for resident variants. Preserve source/group ordinals and returned value IDs. Verify 147 source-context events have complete step coverage, unique source finals, no live Conv/BN/`common.relu`, and concrete state/source position for every live result. | **Conv and residual portions complete:** all 21 specialized Conv definitions and all 9 resident residual definitions expand transactionally and redirect downstream reads. The residual slice adds 16 explicit CKKS events and proves 9/7 add/alignment coverage. The complete all-operator event gate remains open for ReLU, pool, flatten, and linear. |
| C7: mapped checkpoint | Use the existing whole-program checkpoint to write all executable PUs and managed images only after the selected expansion slice validates. Atomically publish `.ckks_ops.B`; reopen with separate-process `ir_b2a -st -src` and original source pathname. | **Conv plus residual checkpoint complete:** retained `.B`, `.T`, source positions, side payload and reports reopen cleanly; 21 Conv and 9 residual rows are lowered, no physical Conv/residual remains, and 28,943 CKKS events are mapped. Final all-operator C7 evidence remains open, and induced/stale publication failures must continue to leave no final or `.tmp`. |
| C8: independent certification | Compare mapped result with input manifest, independent 87/147 census, 19-context policy, clear tensor/slot and polynomial oracles, and fresh state/depth/key/rotation calculation. Run full native syntax/layout and backend-link lanes. | Signed-off report with measured PU/primitive counts, all event/group/final links, 19 mandatory pre-ReLU bootstraps plus any separately admitted and reason-tagged capacity refreshes, no live high-level computation, negative results and artifact hashes. State explicitly that C ABI lowering/provider execution are untested. |

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
2. C2 selects ACE-style external F32 transformed rows and explicit
   `ckks.encode` for the O0 Conv plaintext path. The independent grouped
   mask oracle remains a correctness cross-check, not the asset format.
   The rank-4 source to rank-1 row binding merged through PR #173 and the
   source-free generated-mask transaction merged through PR #174. The bounded
   FHE admission fixture consumes both without raw IR mutation. This does not
   admit ACE fast blocking or MetaKernel O2. If the row-indexed recipe needs
   graph-wide layout work,
   unexpected state repair, or proliferating cases, stop and quantify the
   gap. After C2-C4, review the complete Conv/ReLU DAG and depth/key/rotation
   census before mutation.
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
Its next mandatory `-O0` consumer is the `openpy`-selected CKKS2C early
exit, which emits a provider-private C evaluator alongside the unchanged
public ABI-v1 application. Without that early exit, the same verified CKKS
stage later continues through CKKS-to-POLY lowering and POLY2C. POLY2C is
not an S6-0c failure fallback; see
`doc/FHE-SYNC6-ACE-CKKS-C-STAGING.md`.
