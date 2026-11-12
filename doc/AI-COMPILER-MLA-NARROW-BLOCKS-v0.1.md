# MLA narrow block mapping and implementation, v0.1

Status: implemented AOT physical mapping; native/reference and CUDA compilation
validation recorded in the v0.19 delivery receipt. Device execution and latency
certification remain pending. This amendment supersedes the fixed 128-thread
assumption for newly selected narrow local kernels in the v0.18 walkthrough.
Earlier designs, recipes, releases and retained evidence remain available.

## Selected architecture and policy

VHO selects the physical schedule before native WHIRL construction. The
`auto-attention` default is now `coverage-v3`. It retains the v2 candidate widths
32,64,128,...,4096 and coverage/fewer-chunk ranking, and additionally selects:

| Kernel | Threads | Warps | Score/weight capacity | Selected static shared bytes |
| --- | ---: | ---: | ---: | ---: |
| Local split, width 64 | 64 | 2 | 64 floats | 288 |
| Local split, width 32 | 32 | 1 | 32 floats | 160 |
| Other local / direct | 128 | 4 | round span to 128 | 4*(capacity+8) |
| Chunk tuple merge | 128 | 4 | round chunk count to 128 | 4*(capacity+8) |

The eight-float statistics allocation retains its existing slots; CUDA's
optimizer can remove unused bytes. Report ptxas allocations separately from
these conservative selected sizes. Query/value projections and result stores
retain their existing mapping. Short rows still take the legal direct plan when
L<=256; they split only if direct resource admission fails.

`VHO_TENSOR_KERNEL.threads` is the owning phase's per-kernel decision. Native
metadata `tensor.kernel.block_x`, all-stage occupancy admission, shared-memory
allocation, body construction, native reopening audit and the host/device launch
manifest consume that same value. The source-program `threads=128` field and
original operation records remain the baseline mapping of source operations;
actual launches must use the audited per-kernel manifest.

The v6 recipe envelope remains unchanged. The versioned `coverage-v3` policy and
candidate `block_x` fields seal geometry through canonical recomputation and the
recipe fingerprint. `--chunk-policy coverage-v1` and `coverage-v2` reproduce the
older 128-thread realizations. A manual `stream-attention --split-width N`
override intentionally emits the existing v5 128-thread policy. Re-run automatic
selection to obtain v3 narrow blocks; changing only a launch dimension is invalid.

## Reductions and synchronization

All blocks contain complete 32-lane warps. Every lane executes full-mask
shuffle-down trees at offsets 16,8,4,2,1, including masked positions and token or
column tails. No lane exits early before a collective.

For 64 threads, each warp publishes one maximum summary. A CTA barrier publishes
both summaries; thread zero folds exactly two in ascending warp order and a
second CTA barrier publishes the result. The denominator repeats this pair.
Normalized-probability publication adds a fifth CTA barrier. There are fewer
participants, summaries and neutral scoring slots, but still five CTA barriers.

For 32 threads, lane zero directly publishes the warp's maximum/safe maximum or
denominator. An unconditional `__syncwarp(0xffffffffU)` follows each publication.
A third warp fence publishes normalized shared probabilities before accumulation.
There are zero CTA barriers in the local kernel. Explicit fences provide memory
ordering under independent thread scheduling; removing synchronization merely
because there is one warp would be unsafe. The third fence is retained as the
canonical probability-publication boundary, without claiming it is the minimum
possible synchronization for every subsequent ownership scheme.

The fences are ordinary effectful typed WHIRL CALLs: `void __syncwarp(U4)`.
Existing normal whirl2c emits them. No opcode, TY-kind, binary section, backend
library dependency, JIT or runtime device discovery is introduced.

## Register ownership and numerical contract

For T selected threads and W=T/32 warps, warp w owns latent columns
`group*(4*W)+4*w+j`, j=0..3. There are ceil(R/(4*W)) groups. Each lane l visits
local tokens l,l+32,..., reuses each shared probability for four private sums,
and performs four full-warp trees. Lane zero writes valid columns. Column bounds
guard memory and stores, never shuffles. For R=16 this takes two groups at 64
threads and four at 32 threads, versus one at 128. All R columns remain covered;
simply reducing blockDim without changing ownership would silently omit columns.

Fused score, mask, stable softmax and latent accumulation still write only
chunk tuples [m_c,d_c,z_c], where z_c is locally normalized. The 128-thread merge
uses t_c=d_c*exp(m_c-m), D=sum(t_c), z=sum((t_c/D)*z_c), LSE=m+log(D).
Zero-mass chunks select zero before rescaling exp; nonfinite maximum and
nonpositive denominator fallbacks are retained. All-empty rows produce zero
latent output and negative-infinity LSE. No global score/probability buffers.

F32 regrouping remains explicitly authorized at O3. Reference checks use the
independent expanded FP64 attention at rtol=2e-5, atol=2e-6. Compare numerical
results to that oracle, rather than requiring different chunk partitions to
produce identical F32 bytes. Readable source and mapped source for the same
selected plan must agree bitwise under the documented strict floating flags.

## Work and resource accounting

For B=1,H=3,L=8193,R=16:

| Local width | Chunks | Local CTAs | Old/new block | Old/new exp slots | Old/new padding slots | Partial scratch |
| --- | ---: | ---: | --- | --- | --- | ---: |
| 64 | 129 | 387 | 128 / 64 | 49536 / 24768 | 24957 / 189 | 27864 bytes |
| 32 | 257 | 771 | 128 / 32 | 98688 / 24672 | 74109 / 93 | 55512 bytes |

Padding here means geometric shared-array slots beyond each chunk's real token
span, independently of masks. Full narrow chunks have no block padding; the
one-token final chunk still has neutral lanes. Useful score dot work remains
the same. The merge still has three 128-thread row CTAs, reading 129 or257 tuples,
with selected shared sizes1056 or1568 bytes. Total temporary bytes remain28332
or55980 respectively, with six total launches. The usual 127-token short case
retains five launches and468 bytes; the 8-SM review envelope keeps width1024.

Occupancy admission uses the actual selected threads for every kernel while
preserving the supplied profile's register/shared allocation quanta. In
particular, a four-warp register allocation quantum can keep estimated allocated
registers unchanged for a one-warp block. Do not infer improved residency from
thread count alone, or infer a device's enabled SMs from `sm_90`.

Expected savings are less padded exp/shared work and smaller synchronization
participation, with cross-warp statistics folds eliminated at32. Costs include
more serial column groups, fewer independent warps per CTA, unchanged tuple
traffic and merge cost. Wider real workloads may favor the old realization;
coverage is a planning heuristic, not a calibrated whole-call latency objective.

## Implementation order and acceptance

1. Add selected per-kernel threads; seal v3 geometry and ledger; recompute
   all-stage resource admission using the existing occupancy service.
2. Materialize narrow shared extents, statistics reductions, typed warp fences
   and warp-count-dependent latent column groups in native WHIRL.
3. Audit selected launch, shared allocation, typed collectives and complete body
   fingerprints after native reopening. Reject stale or forged products.
4. Print readable selected-source helpers from this plan; normal whirl2c emits
   actual mapped CALLs and WHIRL COMMENT payloads without selecting policy.
5. Model32/64/128 participants in the synchronized host runner. Check forward
   and reverse worker order, unequal ranks, masked/empty chunks, large finite
   inputs, batch/head distinctions, and uneven one-token tails against FP64.
6. Retain genuine device.before/after.B/T and raw diffs plus annotation-only
   COMMENT checkpoints. Require exact explanation payload equality across
   readable CUDA, reopened WHIRL and normal whirl2c CUDA. Keep v1/v2 native
   audits and source regeneration byte-compatible.
7. Compile readable and mapped CUDA for sm_90, retain ptxas registers/shared/
   spills and GPU probe results; publish fresh v0.19 artifacts, checksums,
   exact tools/source/commands, immutable receipt and isolated retained-tool
   replay. Device sanitizer and synchronized GPU timing gate performance claims.

Invalidation: block size or warp ownership changes invalidate native images,
launch manifests, shared/occupancy facts, fingerprints, source views and
explanations. Shape, resource profile or policy changes require reselection;
W2C must never reuse or adjust stale geometry. Comment-only annotation preserves
executable tokens and has its own independently reopened before/after boundary.

Risks: incomplete column coverage; divergent full-mask collectives; missing
shared publication fences; stale launch dimensions; optimistic register quantum
assumptions; merge footprint exceeding the resource envelope; and reduced local
parallelism outweighing padding savings. The focused positive and negative
checks target these contracts. Real GPU race/synchronization and performance
certification remain explicit follow-up milestones. Primary owner, successor,
independent reviewer and approving lead remain TBD as requested.
