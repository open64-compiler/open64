# MLA two-warp broadcast statistics, v0.1

v0.23 extends the register-broadcast realization to split local width64 with
64 threads. It reduces five CTA barrier phases to three, while preserving the
coverage-v4 width32 register-only path, direct attention and128-thread merge.
The new AOT selection policy is coverage-v5; earlier policies remain explicit
reproduction options. The owning phase is PU-local VHO physical lowering.

## Selected contract

VHO_TENSOR_KERNEL.two_warp_statistics is true only for coverage-v5, local
mapping3/stage1, threads64, split_width64 and register latent accumulation.
The one-warp register_statistics flag remains restricted to width32. Both flags
are initialized and explicitly cleared on copied merge entries. Source types,
logical tensor values and partial tuples retain their existing contracts.

The v6 recipe envelope is unchanged. The candidate statistics field seals
mode two_warp_register_broadcast for width64, warp_register_broadcast for32,
and shared for other candidates. Native metadata and launch manifests expose
the same mode. Resource admission uses4*(64+4)=272 selected shared bytes for64,
zero for32, and existing footprints for wider/direct/merge kernels. All-stage
thread, register, shared-memory, scratch and native capacity bounds still apply.
A manual stream-attention override retains its existing v5/128-thread behavior.

## Register and shared ownership

Thread t scores token chunk*64+t. Each warp reduces its32 scores through the
existing full-mask shuffle-down tree. Scores remain private across that tree.
The shared allocation is one probability array of64 F32 values plus one
four-element F32 summary array:

| Summary index | Writer | Meaning | Reader |
| --- | --- | --- | --- |
|0|warp0 lane0|warp0 maximum|lane0 of both warps|
|1|warp1 lane0|warp1 maximum|lane0 of both warps|
|2|warp0 lane0|warp0 denominator sum|lane0 of both warps|
|3|warp1 lane0|warp1 denominator sum|lane0 of both warps|

The two maxima remain immutable while denominator slots2 and3 are produced.
Only normalized probabilities are written to the64-slot array; scores and
unnormalized exponentials stay lane-private. Final raw/safe maximum and
denominator live in registers after their broadcasts.

## Three CTA phases and the ordering proof

1. Each warp leader stores its maximum in summaries[warp]. CTA barrier1
   publishes both. Each warp's lane0 then independently folds slots0 and1,
   starting at negative infinity, in the same F32 order as the old thread0
   fold. Each broadcasts the identical complete raw maximum inside its own
   warp using __shfl_sync(full_mask,value,0,32). Every lane derives the
   finite-safe copy while retaining the raw maximum for tuple semantics.
2. Each lane computes and preserves exp(score-safe_max), then performs the
   existing warp sum tree. Each leader stores its partial at[2+warp]. CTA
   barrier2 publishes both partials. Both leaders fold slots2 and3 from +0
   in the original order and broadcast the completed denominator locally.
3. Each lane normalizes its private exponential using the positive-denominator
   fallback, stores probabilities[tid], and reaches CTA barrier3. Both column
   warps then consume the complete immutable64-element probability array.

The old final-maximum and final-denominator publication barriers disappear
because complete statistics are never stored to shared memory for all-lane
loads. Two source broadcast sites execute in both warps: four warp-level
broadcasts per CTA. Warp-relative source0 denotes thread0 or32 respectively.
A32-lane shuffle cannot communicate directly across the two warps.

Separate summary slots are essential. After barrier1, warp0 may read both
maxima and compute its denominator while warp1 has not yet read a maximum.
If warp0 overwrote a maximum slot with its denominator, warp1 could read the
wrong value. Keeping maxima in[0,1] and denominators in[2,3] removes that race
without another release barrier. Probability storage is a distinct allocation,
so a fast warp's normalization cannot overwrite a delayed denominator read.

The third barrier remains necessary: each latent warp visits tokens lane and
lane+32, consuming probabilities written by the other warp. Register broadcasts
supply register communication, not general shared-memory ordering. All64 threads
reach each CTA barrier; all32 lanes per warp reach every full-mask collective,
including masked/tail lanes. No implicit warp lockstep is assumed. The source
and tests distinguish dynamic barrier phases from ptxas barrier-resource counts.

## Numerical and column contracts

Latent columns remain group*8+warp*4+j, j0..3, with ceil(R/8) groups. At R16,
group0 assigns0..3 to warp0 and4..7 to warp1; group1 assigns8..11 and12..15.
Four private sums reset per group, and each warp's lane l visits tokens l and
l+32 when valid. Full-mask column trees and lane0 guarded stores are unchanged.
R21 tests cover the final partially valid column group. A35-token tail covers
a partially active second score warp; a one-token tail leaves the second warp
empty but still participating in all phases and useful for latent columns.

Tuples remain [raw_m_c,d_c,normalized_z_c], with the existing finite-maximum,
positive-divisor and zero-mass guards. Finite allowed one-token tails have d_c1
and their cache vector as z_c. Empty chunks keep d_c0,z_c0. The separate merge
retains stable exponential rescaling and its five CTA barriers; query/value
projection and result stages are unchanged. Global score/probability buffers
are not introduced, and scratch/launch counts do not change at fixed width.

For B1,H3,L8193,R16, width64 still uses129 chunks,387 local CTAs,27864 partial
bytes and28332 total temporary bytes. The shared footprint changes288->272.
The sm_90 rank16 compile reports40 registers,272 shared bytes and no spills;
previously40 registers/288 shared bytes. Width32 remains32 registers/zero shared
bytes. The supplied128-byte shared allocation quantum rounds both272 and288
to384, so the planning envelope does not infer a residency gain from this small
footprint difference. Gains are expected from fewer phases and shared accesses,
with costs from duplicate leader folds, broadcasts and private scalar lifetimes.
These are compile/structural facts; whole-call GPU speedup is not measured.

## Implementation and validation gates

1. Version/seal the decision and footprint; construct and clear transient flags
   in the owning schedule before native allocation.
2. Materialize private score/exp and complete-statistic registers, disjoint
   shared summaries, two existing typed broadcast CALLs and three CTA barriers.
   Each F4 return-preg is captured immediately. No new opcode, TY-kind, binary
   section, be.so dependency, JIT or runtime device query is introduced.
3. Verify metadata, resources, typed collectives and body fingerprints after
   independent native reopening. Reject wrong broadcast mask/source/width/type,
   missing calls, missing barriers, aliased denominator/max slots, stale mode
   metadata and forged candidate statistics/footprints.
4. Print the selected two_warp_register_fused_attention_to readable helper.
   It emulates the same F32 two-summary fold and per-column tree; native WHIRL
   owns the physical memory/barrier schedule. Automatic descriptions must match
   real WHIRL COMMENT and normal-whirl2c CUDA payloads exactly.
5. Run focused then full native/reference tests, forward/reverse worker order,
   batch/head distinctions, R21 columns, mixed/empty warps, large finite values
   and tails. Compile sm_90 source with strict floating flags and retain spills/
   resource logs. Preserve genuine device.before/after.B/T and raw diffs, plus
   bare/annotated COMMENT checkpoints. Constrain unaffected executable bodies
   after normalizing only policy-derived kernel names; reproduce policies1..4.
6. Publish a fresh v0.23 receipt with committed source, tools, exact commands,
   references, checksums and isolated retained-tool replay. Preserve older
   immutable packets and unrelated repository edits.

Changes to width, block size, slot aliasing, column/token ownership, numerical
rules or resource profile invalidate the plan, ordering proof, resource/launch
facts, body fingerprint and comments. New scalar WN/ST/TY entries, changed
shared arrays, two CALLs and policy metadata explain intentional raw-trace churn.
Reference tolerance remains rtol2e-5/atol2e-6. CPU/reference correctness and CUDA
compilation do not certify device execution or performance. GPU execution,
sanitizer and timing remain hardware-gated. Transfer roles remain TBD.
