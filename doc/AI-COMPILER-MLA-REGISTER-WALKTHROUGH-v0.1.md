# MLA register accumulation walkthrough v0.1

This walkthrough describes the unchanged v0.13 selected algorithm. The v0.14
delivery expands generated explanations in readable CUDA and real WHIRL COMMENT
nodes; normal whirl2c emits those comments in mapped CUDA. It changes no selected
recipe, launch geometry, storage contract or executable CUDA tokens.

## Read the short file in stage order

The readable short file has five stages: query absorption; complete fused
attention; value projection; output store; LSE store. Named reshape and transpose
views allocate no copy. The register_fused_attention_to helper is a serial
arithmetic review; the mapped tensor_*_op_33 function contains the actual
thread/warp coordinates, shared arrays, private scalar sums and typed calls.

For B=1,H=3,L=127,R=16 there are three attention CTAs, one per head. Each
128-thread CTA computes masked latent and RoPE score contractions, reduces the
maximum, exponentiates on the stable origin, reduces the denominator, and
normalizes weights in shared memory. The same 128-F32 array is reused for
scores, exponentials and probabilities. Eight F32 statistics slots bring
selected shared storage to 544 bytes.

The accumulation loop assigns warp w columns group*16+w*4+j, j=0..3.
For R=16 there is one group: warps own columns 0..3,4..7,8..11,12..15.
Lane l visits local tokens l,l+32,... . Its four sums update as

    p = shared_probability[t]
    a[j] = a[j] + p * cache[batch, begin+t, group*16+4*w+j]

All four a[j] values are private to this lane. Lane zero of warp zero visits
tokens 0,32,64,96; lane 31 visits 31,63,95 and never reads token 127. The score
reduction's padded slot is neutral. Each of the four sums uses shuffle offsets
16,8,4,2,1; lane zero writes its warp's completed columns. Invalid columns still
participate in the full-mask trees, with guarded cache reads and output writes.

The resulting bundle is [z[0..R-1],LSE]. Projection converts the latent
expectation to the output value space. No partial scratch or merge is needed.

## Why the tile barriers disappear

The old staged path collectively filled a 32x4 shared cache tile. A warp could
consume entries loaded by another warp, so a publish barrier was needed.
A release barrier also prevented the next tile from overwriting entries that
other warps still used.

The register path reads cache values directly into lane-private arithmetic.
There is no shared cache tile to publish, release or overwrite. That dependency
removal makes the two tile barriers unnecessary. Deleting them from the old
staged algorithm would be unsafe.

The remaining barriers protect genuine cross-warp dependencies:

| Call | Required visibility |
| --- | --- |
| 1 | Four warp maximum summaries before thread zero folds them |
| 2 | Thread zero's maximum before exponential evaluation |
| 3 | Four warp denominator summaries before thread zero folds them |
| 4 | Thread zero's denominator before normalization |
| 5 | Normalized weights before latent accumulation |

After call 5 the weight array is immutable. Private sums communicate only
through the full-warp shuffle trees. No additional CTA barrier is needed.
Five means dynamic calls per participating thread, not five hardware barrier
IDs. A weight is reused across four columns in one lane's group; other warps
still read it for their own column groups.

For R=16 the staged count was 5+2*ceil(R/4)*ceil(span/32).
Short span 127 therefore used 37 calls; the register path uses five.

## Read the long file in stage order

Long has six stages: query absorption; fused local attention; stable tuple
merge; value projection; output store; LSE store. For L=8193,W=128 there are
ceil(8193/128)=65 chunks and 3*65=195 local-attention CTAs. Chunk c covers
[c*128,min((c+1)*128,8193)). The last chunk is token 8192 only.

Each CTA writes [m_c,d_c,z_c[0..R-1]], where d_c is its stable exponential sum
and z_c is already divided by d_c. The offset is (c*B*H+row)*(R+2).
The merge has three CTAs, one per row, and reads only completed tuples:

    m = max_c m_c
    t_c = d_c * exp(m_c-m)
    D = sum_c t_c
    alpha_c = t_c / D
    z = sum_c alpha_c * z_c
    LSE = m + log(D)

Empty chunks take t_c=0 before the exponential product; this avoids a spurious
0*infinity for very negative global maxima. Source safe-maximum/denominator
guards remain. Fully masked rows produce zero latent and negative-infinite LSE.

Max/D use tids 0..64 for the 65 real chunks; other tids contribute neutrals.
The vector merge uses lane-strided chunks: lane 0 takes 0,32,64; lane 1 takes
1,33; lane 31 takes 31,63. Four register sums per lane, the same full-mask trees,
and five statistic/publication barriers remain. A one-token chunk has d_c=1
when allowed and finite, but its global mass still depends on its score.

## Separate the two performance choices

Register ownership alone reduces the staged W=256 full-chunk count from 69
barrier calls to five. Choosing W=128 separately increases chunks 33 -> 65
and local CTAs 99 -> 195. Both register widths use five calls per local CTA.
Smaller chunks repeat that fixed overhead more often; their benefit is more
schedulable work and shorter local spans, not removal of the five barriers.

Long partial scratch grows 7128 -> 14040 bytes; total global temporaries grow
7596 -> 14508 bytes. More tuple stores/loads, rescaling exponentials and merge
contributions follow. Total token score-dot work is unchanged. Merge parallelism
remains one CTA per row, so it can become a larger fraction of runtime.
The one-token tail pays fixed synchronization/tree overhead for little work.

Short local shared storage falls 1056 -> 544 bytes, long local storage
1568 -> 544. Direct cache loads can lose staging locality; four independent
sums may increase registers. Retained nvcc sm_90 reports show registers
36 -> 40 short, 32 -> 39 long local, and 25 -> 38 merge, without spills.
This annotation release must reproduce those executable/resource facts.

More CTAs can help when the grid underfills the GPU or lacks scheduling work.
After saturation, extra merge work and register pressure may outweigh that
benefit. Select using whole-call latency including projections and launches.
No usable GPU was available for the retained evidence: hardware speedup,
occupancy, race/synchronization sanitization and production tuning remain open.

## Evidence and maintenance

The generator owns the explanations; hand editing generated CUDA is unnecessary.
Retain native comments.before/after.B, independently reopened matching .T traces
with ir_b2a -st -src and the raw diff. Native annotation changes COMMENT nodes,
strings and metadata while leaving emitted executable tokens unchanged.

Validate exact payload equality between readable and mapped CUDA, prior-source
token parity, identical recipes/manifests, reference outputs and nvcc resources.
Use the saved fourteen expanded FP64 oracles at rtol 2e-5, atol 2e-6 and the
focused unequal-shape/tail/comment-mutation tests. Prior broad implementation
validation is retained separately rather than reported as rerun for annotation.

AOT, the explicit v5 numerical permission, older selectable staged recipes and
transfer roles TBD remain unchanged. Earlier immutable outputs are preserved.
