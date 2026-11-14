# Fused MLA walkthrough: local softmax, stable merge and expected gains

November 5, 2026. The v0.12 annotation release preserves the v0.11 executable
kernels and extends their generated explanations in readable CUDA, real WHIRL
COMMENT nodes and normal whirl2c CUDA. This guide accompanies those comments;
[the fused design](AI-COMPILER-FUSED-MLA-v0.1.md) remains the algorithm contract.
Owner, successor, reviewer and approving lead remain TBD.

## Follow the selected stages

| Stage | Short L=127 | Long L=8193 |
| --- | --- | --- |
| Absorb key projection into query | 1 | 1 |
| Compute scores, softmax and weighted latent | 2: complete row | 2: chunk-local |
| Rescale and merge chunk distributions | Not needed | 3 |
| Project latent through value_up | 3 | 4 |
| Write final output | 4 | 5 |
| Write LSE from fused bundle | 5 | 6 |

The readable files use named serial review helpers. The mapped files show the
actual GPU coordinates, shared arrays, shuffle calls and barriers from WHIRL.
The query absorption and output projection remain independent contractions.
The final latent/LSE bundle is [z[0..R-1],LSE], stride R+1; LSE is loaded from
that bundle, without recalculating scores or softmax.

For each permitted token, compute the scaled sum of a latent-cache dot product
and a RoPE dot product. Scores go to shared memory, not global score arrays.
The same shared slots then hold exponentials and normalized probabilities.
Four full warps reduce maxima and denominators across tokens. Lane zero writes
a partial per warp; a CTA barrier precedes thread zero's four-partial fold, and
another publishes the result. After normalization, each full warp owns one latent
column in a group of four, and cache values are staged in 32x4 tiles.

For short context, a CTA covers a whole head, so this local distribution is
already the global distribution. For long context, width 256 gives 33 chunks,
with a final one-token chunk; a separate merge is essential.

## Derive the long-context rescaling

For a finite, nonempty chunk c, define

    m_c = max_{t in c} s_t
    d_c = sum_{t in c} exp(s_t-m_c)
    z_c = sum_{t in c} [exp(s_t-m_c)/d_c] * cache[t].

z_c is already locally normalized. d_c measures the shifted exponential mass;
it is not usually the token count. The tuple [m_c,d_c,z_c] is stored at
(chunk*B*H+row)*(R+2), with R vector components after the two scalars.

Set m=max_c m_c. Moving every chunk onto this common exponential origin gives

    t_c = d_c*exp(m_c-m), D=sum_c t_c.

The key identity is

    sum_t exp(s_t-m)*cache[t] = sum_c t_c*z_c.

Consequently the correct merge is z=sum_c (t_c/D)*z_c and LSE=m+log(D).
The unshifted partition equals exp(m)*D, but the implementation never evaluates
exp(m), avoiding the associated overflow. This is the stable rescaling identity
underlying tiled attention; see the
[FlashAttention algorithm](https://arxiv.org/html/2205.14135v2#S3.SS1).
Our kernel merges normalized local vectors, rather than unnormalized numerators.

A simple scalar example illustrates why a sum or average is wrong:

| Chunk | m_c | d_c | z_c | Rescaled mass t_c |
| --- | ---: | ---: | ---: | ---: |
| 0 | 0 | 1 | 2 | 1/2 |
| 1 | log(2) | 1 | 8 | 1 |

Here m=log(2), D=3/2, and the final weights are 1/3 and 2/3. The merged latent is
6 and LSE=log(3). A sum gives 10 and an unweighted average gives 5. These are
illustrative values, not a timing measurement or the fixture's actual scores.

For m and D, thread tid visits chunks tid,tid+128,... and four warp partials are
folded by thread zero. With C=33, tids 0..32 initially own one chunk each; the
others are neutral. For the latent-vector merge, a warp instead reduces one
column: lane zero visits chunks 0 and 32, lanes 1..31 take their matching chunk.
The five shuffle rounds complete each column.

An allowed finite one-token tail has d_c=1 and z_c=its cache vector; its final
weight still depends on relative score mass, not 1/33. An empty chunk has d_c=0
and z_c=0. Select zero for its rescaled mass before forming an exponential
product: otherwise a safe local maximum of zero and a very negative global
maximum could form 0*exp(0-m)=0*infinity. All-empty rows retain zero output and
negative-infinite LSE using the source maximum/denominator guards.

## Expected improvement and costs

Comparisons are with the preceding cooperative implementation that retained the
two global score buffers, not with the preceding annotation-only release.

| Structural fact | Short | Long |
| --- | ---: | ---: |
| Launches, preceding -> fused | 9 -> 5 | 12 -> 6 |
| Global temporary bytes, preceding -> fused | 3528 -> 468 | 204240 -> 7596 |
| Removed score allocation bytes | 3048 | 196632 |
| Prior nominal token exp evaluations | 1905 | 122895 |
| Fused valid-token exp evaluations | 381 | 24579 |
| Fused shared exp slots including padding | 384 | 25344 |
| Additional merge rescaling exp evaluations | 0 | At most 99 |

For R=16, the preceding path recomputed the exp in one denominator pass and four
weighted-latent column groups. The fused path computes probabilities once and
shares them across all sixteen latent columns. Padding, chunk rescaling,
compiler folding and predication mean these are source-level counts, not actual
instruction timings or a fivefold runtime speedup. Allocation footprint also
differs from memory traffic: eliminated score stores/reloads may have hit caches
in the prior kernel. Cache values still serve scoring and weighted accumulation.

The prior weighted-latent stage had 12/396 CTAs; fused local attention has 3/99,
now doing scores and softmax too. Less parallelism can offset saved work. Short
contexts may benefit from fewer launches, whereas long contexts may gain more
from saved traffic/exponentials; neither latency outcome has been measured.

Local attention requires five CTA barriers for maximum, denominator and normalized
probability publication, plus two per cache tile per column group. This gives 37
barrier calls per thread in the short CTA, 69 in a full long chunk and 13 in its
one-token tail. The merge has five uniform CTA barriers plus register shuffles.
Shared storage is 1056 bytes short, 1568 bytes long partial and 544 bytes merge.
The preceding compilation records 36/32/25 registers respectively, without spills;
these figures do not establish occupancy or latency.

If improved stages occupied old-time fraction f and achieve net speedup s,
including new overhead, whole-call speedup is 1/((1-f)+f/s) when other costs stay
constant. Illustratively, f=1/2 and s=2 imply 4/3 overall speedup. No calibrated
f, s or architecture timing estimate is attached to the current plan. Hardware
correctness, sanitizers, profiling and production-size workload testing remain
open because the CUDA probe reports error 35 and zero usable devices.

## Generated-comment verification

The owning VHO service derives stage and comparison descriptions from the source
and selected facts. Both source views use identical payloads; no optimization or
layout choice is added to the emitter. The v0.12 refresh changes comments only.
Require recipe/launch equality, readable/mapped executable-token identity versus
v0.11, COMMENT equality across views, unchanged resources and reference results.
Retain native before/after COMMENT snapshots, matching ir_b2a -st -src traces,
raw diffs, producer tools and a new immutable receipt. Older canonical comments
continue to use their retained producer/auditor. All earlier outputs are preserved.
