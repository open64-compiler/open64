# MLA width-64 and width-32 kernel walkthrough v0.1

This annotation release expands the unchanged v0.17 coverage-v2 plans.
The owning generator emits the same explanations into readable CUDA and real
WHIRL COMMENT nodes. Normal whirl2c prints those nodes in mapped GPU CUDA.
Selection, recipes, manifests, executable CUDA and resource counts stay fixed.

## Follow the six stages

Both long files perform query absorption; fused local attention; stable tuple
merge; value projection; output store; LSE store. The readable
register_fused_attention_to helper exposes serial arithmetic. The mapped
tensor_*_op_33 and tensor_*_op_4129 functions contain actual 128-thread local
and merge CTAs. Named tensor views allocate no copied/transposed buffers.

The review shape is B=1,H=3,L=8193,R=16. Width is tokens per local chunk, not
threads per block. Width 64 uses 129 chunks/head and 387 local CTAs; width 32 uses
257 chunks/head and 771 local CTAs. Both end with one real token at index 8192.

## Coverage gains and why the profiles differ

The 132-SM/two-CTA profile has a 264-CTA goal. The old width 128 plan supplied
195 CTAs, only 195/264 of that policy goal. Width 64 supplies 387 and width 32
supplies 771, so both get capped coverage 264/264. Fewer chunks break the tie
in favor of 64, limiting scratch, fixed CTA work and merge contributions.

The four-CTA/SM review profile requests 528 CTAs. Width 64 has only 387, so width 32
wins with 771. The 8-SM/two-CTA profile still selects 1024: 27 CTAs already meet
its 16-CTA goal, with fewer chunks than any narrower full-coverage plan.

These are static policy coverage counts. They do not measure GPU utilization,
achieved occupancy or whole-call latency.

## Local scoring, padding and private accumulation

A full width 64 chunk uses scoring tids 0..63 (two warps); width 32 uses 0..31
(one warp). Other tids write neutral-infinity scores and do not read source
cache/query data for those padding tokens. Mask-false real tokens can also be
neutral. Every thread enters the same four-warp maximum and denominator folds.

The score array is always rounded to 128 F32 slots at these widths. It is reused
in place for exp(score-m_c) and normalized local probabilities. Local exp and
normalization loops visit all 128 slots, including neutral padding. Padding
therefore avoids source dot work but still has selected shared-array/reduction
work. Compiler folding/predication can change the hardware cost of those slots.

All four warps do useful latent-column work for R=16: warp 0 owns columns 0..3,
warp 1 owns 4..7, warp 2 owns 8..11, warp 3 owns 12..15. Each lane has four private
F32 sums and reuses one normalized weight across its four column updates.

For width 64, lane l visits local tokens l and l+32 on a full chunk. Lane 0
takes 0,32; lane 31 takes 31,63. For width 32, each lane visits only its own
token 0..31. Each sum then uses shuffle offsets 16,8,4,2,1; lane 0 stores four
completed columns. Tails guard source accesses, while every lane still
participates in required full-mask shuffle rounds.

Five CTA barriers remain per local CTA: publish warp maxima; publish the
thread-zero maximum; publish warp denominator sums; publish the completed
denominator; publish normalized weights. Register accumulation needs no shared
cache-tile publish/release barriers.

## Compare source work and allocations

All numbers below refer to the three-head 8193-token review, independent of
which profile chooses the width. Logical row/token positions total 24579.

| Quantity | Width 128 baseline | Width 64 | Width 32 |
| --- | ---: | ---: | ---: |
| Chunks/head |65|129|257|
| Local CTAs |195|387|771|
| Shared score slots/CTA |128|128|128|
| Local shared bytes/CTA |544|544|544|
| Nominal local exp slots across grid |24960|49536|98688|
| Geometric padding slots |381|24957|74109|
| Local CTA barrier phases across grid |975|1935|3855|
| Partial tuple bytes |14040|27864|55512|
| Real merge tuple count/row |65|129|257|
| Allocated merge weight slots |128|256|384|
| Merge shared bytes/CTA |544|1056|1568|
| Total global temporary bytes |14508|28332|55980|

The local slot calculation is rows*chunks*round_up(width,128).
Geometric padding subtracts rows*L and excludes any extra neutral real slots
caused by masks. It means unused slots beyond each chunk's assigned span, not
only positions beyond global L. For example, slot 64 in chunk 0 of width 64 is
padding there, while global token 64 belongs to chunk 1 and is scored once there.
These are source-loop counts, not measured exp instructions.
Barrier phases count CTA rendezvous across the local grid; they are not a
sequential sum of GPU time, because CTAs can overlap.

Valid allowed-token dot work covers the same context at all widths.
Narrower chunks repeat fixed reduction/publication work, nearly double and
quadruple the local shared-slot visits compared with 128, and create more tuples.
No global score or probability buffers return.

## Larger stable merge

Each chunk stores [m_c,d_c,z_c[0..R-1]], where z_c is locally normalized.
For 18 F32 values per row and three rows, each chunk adds 216 global scratch bytes.

The merge still has three CTAs, one per row, and five required CTA barriers.
It obtains m=max(m_c), masses t_c=d_c*exp(m_c-m), D=sum(t_c), then
z=sum((t_c/D)*z_c) and LSE=m+log(D). Empty chunks contribute zero mass before
the exponential product. An all-empty row returns zero latent and negative-infinite LSE.
Increasing chunk count changes FP32 grouping
but leaves the selected numerical permission and guards unchanged.

For C=129, statistics tid 0 reads chunks 0,128; other tids read one chunk.
For C=257, tid 0 reads 0,128,256; other tids read two chunks.
After weight publication, vector lane 0 visits:
C=129:0,32,64,96,128 (five terms);
C=257:0,32,64,96,128,160,192,224,256 (nine terms).
Other vector lanes visit four or eight chunks respectively. Each warp merges
four latent columns with private sums and the same full-warp trees.

The physical merge allocates 256 slots for 129 weights and 384 slots for 257.
Only real C slots are written and processed. Uninitialized allocation padding
is never read, normalized or exponentiated. This differs from local score
padding, which is explicitly written neutral and visited by local loops.

More tuples mean more global stores/loads, rescaling exps and vector terms.
Merge CTA count remains three and launch count remains six for the whole call.
The merge can become a larger runtime fraction even while local CTA coverage
improves. Larger merge allocations also consume more shared-memory budget;
existing all-stage eligibility checks remain necessary.

## Evidence and remaining work

This release must preserve recipes, full candidate ledgers, launch manifests,
executable CUDA tokens, reference output bytes and ptxas resources from v0.17.
Cross-width output comparison uses the expanded FP64 oracle tolerances
rtol 2e-5/atol 2e-6; exact-byte comparison applies to the same width before/after
this annotation change. It does not require width 32 and 64 to match each other
bit-for-bit.

Keep all 42 saved profile/scenario references, four focused native paths,
legacy policy checks, source-token parity, COMMENT payload equality and CUDA
compilation. Retain native comments.before/after.B with independent matching
ir_b2a -st -src .T traces and raw annotation diffs.

The resource/coverage heuristic is not calibrated instruction-delay or stall
prediction. Real GPU correctness, Compute Sanitizer, whole-call latency,
production shapes/precisions and performance calibration remain pending.
AOT, explicit profile provenance, older selectable policies and transfer
roles TBD remain unchanged.
