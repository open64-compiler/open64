# MLA register broadcasts and removed fences, v0.1

v0.22 expands automatic explanations for v0.21's coverage-v4 one-warp local
kernel. Selected algorithms, recipes, launches, arithmetic and resource facts
remain unchanged. VHO descriptions feed identical real WHIRL COMMENT payloads
to readable CUDA and normal-whirl2c CUDA. This walkthrough supplements
AI-COMPILER-MLA-WARP-REGISTER-STATISTICS-v0.1.md.

## Follow one lane's values

A 32-thread CTA owns one row/chunk. Lane l owns token chunk*32+l and retains
its masked score in attention_masked_score. Masked or out-of-range positions
keep negative infinity and skip source dot reads. All lanes reach collectives.

The maximum tree uses accumulator/next_value scratch registers at offsets
16,8,4,2,1. Lane0 is guaranteed to have the complete reduction; other lanes can
have partial results. __shfl_sync(full_mask,accumulator,0,32) copies lane0's
complete maximum into each lane's attention_raw_maximum register. Each derives
attention_maximum, the finite-safe copy. Raw and safe maxima remain distinct:
the raw maximum belongs in the partial tuple; the safe value shifts exponentials.

Each lane computes exp(score-safe_max) into attention_weight. The denominator
tree reuses accumulator/next_value without overwriting that private exponential.
A second lane0 broadcast writes the completed denominator into
attention_denominator in every lane. Each then divides its preserved exponential
by denominator>0?denominator:1. attention_weight now holds an immutable private
probability, separate from the later column-reduction scratch.

| Phase | Live private state | Register exchange |
| --- | --- | --- |
| Score | score | none |
| Maximum | score; raw/safe maximum | five down-shuffles, one lane0 broadcast |
| Denominator | exponential; denominator; raw maximum | five down-shuffles, one lane0 broadcast |
| Normalize | probability; raw maximum; denominator | none |
| Latent groups | immutable probability; four resettable sums | four five-round trees per group |

At R=16, group0 computes columns0..3, then groups1..3 compute4..7,8..11,12..15.
Lane l uses its same p_l for each cache column of its token. Only the weighted
product sums cross lanes, using full-mask shuffle trees. No shared probability
array is needed. Lane0 stores valid reduced columns; invalid columns suppress
memory/stores but do not suppress collective calls.

## Why each old memory fence disappears

| Old fence | Shared producer/consumer dependency | Current dependency |
| --- | --- | --- |
| Maximum publication | lane0 stores shared maximum; other lanes load it | full-mask register tree and lane0 broadcast |
| Denominator publication | lane0 stores shared denominator; other lanes load it | register tree and lane0 broadcast; private exponential survives |
| Probability publication | lanes store shared probabilities before latent reads | each lane retains and reads its own probability across all groups |

The first two broadcasts communicate completed scalar results. They do not
supply general shared-memory ordering. The third fence disappears because
width=threads=32 proves one token/probability per lane, and latent ownership
keeps that lane as the consumer in every group. No cross-lane shared write/read
edge remains. This is valid under independent thread scheduling: private data
dependencies stay within a lane, and cross-lane dependencies use explicit
synchronized register collectives. It does not rely on implicit warp lockstep.

All 32 non-exited lanes must reach the same full-mask collective calls. A masked
lane still supplies -infinity to max and zero mass to the sum. A tail lane skips
memory work but still shuffles. Source lane0, width32 and mask0xffffffff are
required even when only one token is valid. A different block size, wider
per-lane token span, shared cache staging or changed ownership invalidates this
proof. The 64-thread path and 128-thread merge retain their required barriers.

## Small mathematical example

Take scores 0 and log(3), with all other lanes masked. The max tree and broadcast
give m=log(3) in every lane. Exponentials are e0=1/3 and e1=1. The sum tree and
second broadcast give D=4/3 everywhere. Lane0 retains p0=1/4; lane1 retains
p1=3/4. For a cache column with values 2 and 10, the latent product tree yields
z_c=(1/4)*2+(3/4)*10=8. Other columns reuse those same private probabilities.
This illustrates real arithmetic; actual F32 operations use the existing policy.

For an empty chunk, raw maximum=-infinity, safe maximum=0, all exponentials=0,
denominator=0 and probabilities=0 using the safe divisor1. All lanes still
participate. A finite allowed one-token tail has denominator1 and local latent
equal to that cache vector. Its global importance remains score-dependent.
The separate merge preserves zero-mass guards and exponential rescaling of
[raw_m_c,d_c,normalized_z_c] tuples. It does not average chunk latents uniformly.

## WHIRL, generated source and validation

Each broadcast is an ordinary effectful F4 CALL with arguments U4 mask,
F4 value,I4 source_lane,I4 width. Its return-preg is read immediately into the
selected scalar. Normal whirl2c prints direct assignments from __shfl_sync.
The readable serial helper emulates lane registers through arrays; actual
mapped CUDA contains no shared arrays in this local kernel.

The annotation release must preserve recipe bytes, launch manifests and
executable CUDA tokens, with exact explanation equality across readable source,
reopened native COMMENT nodes and normal-whirl2c CUDA. Retain genuine bare/
annotated .B images, independent same-stem -st -src traces and their raw diffs.
New explanation strings/metadata are intentional; executable changes are not.

Focused native reference tests cover ordinary and wide rank21 columns with
forward/reverse worker order and malformed-product rejection. Fresh readable
FP64 checks, strict sm_90 compilation and executable parity justify reuse of
v0.21's content-addressed full-length mapped reference evidence. The compiled
rank16 kernel remains32 registers, zero shared bytes and zero spills. It still
has80 latent down-shuffles, ten statistics down-shuffles and two broadcasts;
zero memory fences does not mean zero synchronization or zero communication.
GPU execution/timing remains unavailable, and no measured speedup is claimed.
Earlier immutable packets/tools are retained for their original canonical
annotations. Ownership, successor, reviewer and approving lead remain TBD.
