# MLA source walkthrough and structural performance comparison

November 5, 2026. This guide supplements the generated walkthrough in the
readable CUDA, real WHIRL COMMENT nodes and normal whirl2c GPU CUDA. The v0.10
annotation release keeps the v0.9 executable kernels. Performance comparisons
refer to the earlier fused/split thread-per-output schedule, preserved in v0.8.
The algorithm is specified in
[cooperative lowering v0.3](AI-COMPILER-TENSOR-KERNEL-LOWERING-v0.3.md).
Owner, successor, reviewer and approving lead remain TBD.

## Read the files in stage order

The readable files show named tensors and selected helpers. Their helpers execute
lane/tree arithmetic serially for review. The mapped files contain the actual
CUDA coordinates, shared declarations, shuffle calls and barriers from WHIRL.
Every stage's COMMENT now explains its root formula, inputs, ownership, tails,
previous mapping, expected benefit and added costs. The overview lists all stages.

For each head, q' = q_nope * key_up absorbs the key projection into the query.
Compute score[t] = alpha*(dot(q',cache[t]) + dot(q_rope,rope_cache[t])), and apply
the mask. Find the row maximum m; use the source finite fallback for an entirely
masked row. Compute d = sum_t exp(score[t]-m), using the source safe divisor.
Then z[r] = sum_t (exp(score[t]-m)/safe_d)*cache[t,r]. Finally project z through
value_up to produce the per-head output; write LSE = m+log(d) for a nonempty row,
otherwise -infinity. This fixture has already-applied RoPE/KV normalization and
no final cross-head projection or cache append.

| Meaning | Short stage | Long stage |
| --- | --- | --- |
| Absorb query through key_up, K=8 | 1 | 1 |
| Latent-cache score contraction, K=16 | 2 | 2 |
| RoPE score contraction, K=4 | 3 | 3 |
| Mask/scale and maximum | 4 | 4 partials, 5 merge |
| Stable exponentials and denominator | 5 | 6 partials, 7 merge |
| Normalized weighted-latent contraction | 6 | 8 partials, 9 merge |
| Project latent through value_up, K=16 | 7 | 10 |
| Write projected output | 8 | 11 |
| Compute/write LSE | 9 | 12 |

Scores remain materialized. Pointwise/view producers are fused into their
consumers; lazy named values are not extra arrays. The small-K score/projection
contractions retain the earlier scalar mapping. Neither warp reductions nor the
shared tile removes those stages or their launch/score-traffic costs.

## Short context: 127 tokens

Maximum and denominator use one warp per head/output. Lane l traverses
l,l+32,l+64,l+96 where valid: at most four terms rather than 127 serial terms.
Then all lanes shuffle with offsets 16,8,4,2,1 and full mask 0xffffffff; only lanes
with lane+d<32 combine at each step. Lane zero writes the result. A CTA has four
warps, so three head outputs use three warps and the fourth remains neutral.
The five rounds are extra communication/combine work, not a 32x latency promise.

The weighted latent has three rows and sixteen columns. A CTA owns one row and
four adjacent latent columns, one warp per column. The K=127 segment has four
token tiles, with eight CTA barriers per thread along that path. Its last tile
contains 31 tokens; the final lane is padded. The grid grows from one block to
twelve blocks, exposing additional independent CTAs.

## Long context: 8193 tokens

The old and new schedules both use width 256 and 33 chunks. Chunks 0..31 contain
256 tokens each; chunk 32 contains only token 8192. A full chunk now gives each
lane eight terms before the five-round tree. This improves the within-chunk
mapping; it does not newly replace an 8193-term serial loop.

Partial storage remains partial[c*N+i]. Maximum and denominator have N=3 and
therefore 99 partial slots each. Their partial grids grow from one block to 25.
Weighted latent has N=48 and 1584 partial slots; its grid grows from thirteen
blocks to 396. A full staged chunk has eight shared tiles and sixteen CTA barriers
per thread. The final one-token chunk has one shared tile and two barriers;
all inactive lanes/warps still reach barriers and shuffles.

The separate merge kernel reads completed chunk-major partials. With 33 chunks,
lane zero accumulates chunks 0 and 32; lanes 1..31 take their matching chunk.
The tree combines them into lane zero. Maximum is merged before exponentials;
denominator is merged before probability-weighted latent products; latent is
merged before projection. These are sums of already normalized weighted-latent
partials, not an online-softmax merge needing exponential rescaling.

## Shared-memory walkthrough

For a token tile starting at t and columns starting at j0:

1. Threads 0..31 evaluate one probability/left value per valid token and stage
   A[32]. Four output warps reuse each staged value.
2. Thread u loads cache[t+u/4,j0+u%4], zero-filling invalid tokens/columns.
   Store it at B[4*(u/4)+((u%4) XOR ((u/4)/8))].
3. Every thread reaches the publication barrier. Warp w / lane l reads A[l]
   and B[4*l+(w XOR (l/8))], and adds its product when valid.
4. Every thread reaches the release barrier before A/B can be overwritten.
   After all tiles, perform the warp tree and let valid lane zero store.

A[32]+B[128] uses 640 bytes, aligned to 16 bytes. The XOR layout distributes
producer stores and consumer reads across distinct banks for the assumed
32-bank F32 layout. It changes storage coordinates, not the logical products.
A shuffle is register exchange, not shared-memory publication; the CTA barriers
carry that ordering. See NVIDIA's
[CUDA 12.8 synchronization and shuffle contract](https://docs.nvidia.com/cuda/archive/12.8.0/cuda-c-programming-guide/index.html).

## Expected gain and practical limits

| Structural change | Short | Long |
| --- | ---: | ---: |
| Full segment serial terms per output, old | 127 | 256 within a chunk |
| Lane-local terms, new, before five shuffle rounds | At most 4 | 8 within a full chunk |
| Weighted-left expression evaluations, old | 6096 | 393264 |
| Weighted-left expression evaluations, staged | 1524 | 98316 |
| Weighted partial/direct grid, old -> new | 1 -> 12 | 13 -> 396 |
| Total launches, old -> new | 9 -> 9 | 12 -> 12 |

The fourfold drop in weighted-left evaluations can reduce repeated probability
exp/normalization work. It does not imply fourfold total MLA speedup or HBM-byte
reduction: right-operand products remain, previous loads may already hit cache,
and the denominator also evaluates exponentials. Additional threads, shuffles,
shared loads/stores and barriers consume resources. More CTAs can spread work
across more SMs, but also add scheduling work. Long reductions generally offer
more work to amortize these costs; small contexts/tails may regress.

If improved stages occupied fraction f of old elapsed time and have net speedup s,
whole-call speedup is 1/((1-f)+f/s), assuming all other costs stay constant. Include
collective/staging/scheduling costs in s. For example, f=1/2 and s=2 would yield
4/3 total speedup; these are illustrative assumptions, not measured values.
This follows the scope limitation described by NVIDIA's
[Amdahl analysis](https://docs.nvidia.com/cuda/cuda-c-best-practices-guide/index.html#strong-scaling-and-amdahls-law).
A calibrated static architecture model could supply estimates; this generator
has no calibrated timing evidence and therefore prints counts, not latency claims.

The v0.9 compile receipt reports 38 registers short / 36 long for the staged
weighted kernels, 640 bytes shared, and no spills. Those facts do not establish
occupancy or speed. The fixtures use B=1,H=3,R=16, with L=127/8193; they expose
correctness/tails and are not production-size tuning workloads. Hardware
execution, sanitizer checks, profiling and v0.8-v0.9 timing remain pending a usable
GPU. AOT and the earlier fallback are preserved.

## Annotation validation and retention

The updated producer supplies the same text to readable CUDA and WN_CreateComment.
Normal whirl2c emits those nodes through its existing COMMENT handler. There is
no new emitter policy, IR representation, arithmetic, mapping or recipe change.
The new harness can refresh explanations from an unchanged selected recipe and
compare both generated views with the preceding annotated release.

Require recipe/launch equality, identical executable CUDA tokens versus v0.9,
canonical COMMENT equality across views, and identical executable tokens across
native before/after COMMENT checkpoints. Retain .B images, matching ir_b2a -st
-src .T traces, raw diffs and the old tools needed to inspect older canonical text.
Rerun saved reference checks and source compilation, and publish fresh versioned
outputs and an immutable receipt. Preserve older receipts and unrelated edits.
