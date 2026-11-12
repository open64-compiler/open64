# MLA warp-owned partials, v0.1

The v0.25 implementation changes the width64 local kernel from three CTA
publication phases to one. VHO owns the PU-local selection and native WHIRL
construction at O3 with explicit allow-reassociate. The AOT default policy is
coverage-v6, in the unchanged open64.tensor-kernel.v6 recipe envelope.
coverage-v5 reproduces the prior three-barrier realization; policies1..4,
manual stream-attention, width32, wider/direct kernels and the separate128-thread
inter-chunk merge retain their existing algorithms. No opcode, TY-kind, binary
layout, mapped-image reader/writer, be.so dependency or JIT is introduced.

## Changed ownership and mathematical contract

Each warp owns its32 tokens and ALL latent columns. Thread t scores token
chunk*64+t once; no score dot is duplicated. Within warp w, compute

    m_w = max_{t in w} s_t
    d_w = sum_{t in w} exp(s_t - m_w)
    z_w[j] = sum_{t in w} (exp(s_t - m_w) / d_w) * cache[t,j]

Raw m_w is preserved; only arithmetic uses the established finite-safe maximum
fallback. A zero denominator uses divisor1. Thus an empty warp publishes
[-infinity,0,zero_vector]. Scores, exponentials and normalized probabilities
stay private in their producing lanes. Full-mask shuffle-down trees and lane0
broadcasts compute each warp's maximum and denominator without CTA barriers.

Both warps process ceil(R/4) column groups, four private accumulators per lane.
Each lane consumes only its own token and normalized probability; column trees
reduce32 products rather than each lane's sum of two products. Lane0 writes
its warp's complete tuple to shared partials[w*(R+2)+component].

One unconditional CTA barrier publishes both immutable tuples. Both leaders
then combine their raw maxima in order0,1 and use stable rescaling:

    m = max(m_0,m_1)
    t_w = 0                       if d_w == 0, BEFORE evaluating exp
          d_w * exp(m_w - m)      otherwise
    D = (0 + t_0) + t_1
    alpha_w = t_w / (D > 0 ? D : 1)
    z[j] = (0 + alpha_0*z_0[j]) + alpha_1*z_1[j]

Finite-safe copies preserve the existing nonfinite fallback behavior. Testing
d_w before exp avoids 0*exp(0-m) NaNs for an empty warp next to a very negative
nonempty warp. Both leaders independently compute the coefficients and broadcast
alpha0 then alpha1 within their own warp. Thread tid writes columns tid,tid+64,...;
thread0 writes m,D. Output remains the existing [raw_m_c,d_c,normalized_z_c]
chunk tuple. The unchanged inter-chunk merge consumes it normally.

This is exact in real arithmetic. It introduces an additional stable hierarchical
reduction and changes F32 rounding/underflow behavior. Explicit reassociation
permission and reference tolerances rtol2e-5/atol2e-6 are required. Do not claim
bitwise equality with coverage-v5; require bitwise parity between the new
readable emulation and synchronized execution of the new mapped native code.

## Ordering proof and barriers

The old maximum and denominator CTA barriers disappear because their producers
and consumers now belong to one warp and communicate through typed full-mask
register collectives. The probability publication barrier disappears because
normalized probabilities never cross lanes or warps. Instead, all cross-warp
dependencies are collected into the complete tuple publication.

Each shared element has one producing warp and one lane0 writer. All tuple
stores precede the sole barrier; all reads of either tuple follow it. No tuple
slot is overwritten or reused. Independent warp scheduling therefore cannot
clobber an unread statistic or expose an incomplete latent vector. After the
barrier, coefficient communication is register-only and final columns have
unique thread ownership, so no additional CTA barrier is needed.

All64 threads reach the barrier; all32 lanes per warp reach every full-mask
shuffle, even for masked/tail tokens or column padding. A35-token tail partitions
32+3, a one-token tail1+0. Empty warps still publish neutral tuples. The remaining
barrier cannot simply be deleted: the output reads both producers' shared tuples.
A zero-CTA alternative would require another ownership/communication design,
such as duplicated score work or separate32-thread CTAs with a different merge.

## Resources and tradeoffs

Shared storage is2*(R+2) floats, or8*(R+2) bytes:144 at R16 versus272 before;
56 at R5,184 at R21,312 at R37. It is smaller below R32, equal at R32 and larger
above R32. Candidate admission, rounded occupancy and native metadata must use
the actual rank. The256-byte rounded allocation at R16 can differ from the old
384-byte allocation under the supplied128-byte quantum, but occupancy can still
be limited by registers, warps, blocks or threads. Inspect actual backend results.

Token/cache products remain64*R per full chunk, but each column now has two warp
reductions. At R16 latent shuffle executions rise80 to160 per CTA. Static
broadcast sites rise2 to4, executing in both warps. Both leaders add two guarded
rescaling exponentials and coefficient divisions. Shared tuple reads/writes and
register lifetimes change. Fewer barriers do not establish lower GPU latency;
coverage-v5 remains available for workload comparison. Global partial storage,
chunk count and launches are unchanged when the selected width remains64.

## Implementation ordering and invalidation

1. Extend the sealed versioned selection with coverage-v6 and mode
   warp_owned_partials. Enumerate/admit the exact rank-dependent footprint.
2. Select the transient flag only for register local stage1/mapping3,
   width=threads=64. Explicitly clear it on copied inter-chunk merge entries.
3. Construct warp-local statistics/probabilities, complete immutable shared
   tuples, one unconditional CTA barrier, stable merge, typed broadcasts and
   disjoint final column stores in native WHIRL. Use existing ARRAY, DO_LOOP,
   scalar intrinsic, CALL and immediate return-preg protocols.
4. Seal and independently audit resource/mode/binding/body fingerprints.
   Reject overlapping tuple strides, missing publication, wrong collectives,
   forged candidate footprint/mode and stale policy metadata.
5. Emit the selected readable helper and identical automatic explanations into
   readable CUDA, real WHIRL COMMENT nodes and normal-whirl2c output. The source
   emitter prints the selected algorithm and does not choose ownership.
6. Validate native mapped execution in both host worker orders, new-versus-FP64
   oracles, new-readable/native bitwise parity, masks, tails, unequal batch/head
   dimensions, R21/R37 column tails and large finite/very separated scores.
   Compile all selected profiles for sm_90; retain registers/shared/spill logs.
7. Verify policies1..5 with retained images/recipes and unchanged executable
   bodies for other mappings, then publish a fresh immutable v0.25 packet with
   genuine before/after B/T, raw diffs, tools/source/commands/hashes and replay.

Any change to token/column ownership, width/block size, tuple layout, guards,
reduction order or target resources invalidates the ordering/numerical proof,
resource ledger, plan seal, generated native body, launch metadata and comments.
Recompute these in VHO before emission. Driver PU scope and lower-level
optimization boundaries remain unchanged. GPU execution, race-sanitizer and
whole-call timing remain hardware-gated; host/compile evidence does not certify
GPU performance. Transfer owner, successor, reviewer and approving lead remain
TBD; transfer acceptance remains open.
