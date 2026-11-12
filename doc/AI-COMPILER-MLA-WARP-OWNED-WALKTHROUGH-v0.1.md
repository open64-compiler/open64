# MLA one-barrier walkthrough and convergence, v0.1

v0.26 expands the automatic explanation of the v0.25 coverage-v6 width64 local
kernel. It changes comments only. The selected algorithm, recipe, launch
manifest, memory layout, arithmetic and executable tokens remain v0.25.
The controlling implementation specification is
AI-COMPILER-MLA-WARP-OWNED-PARTIALS-v0.1.md. PU-local VHO generates one explanation
payload for readable CUDA, real WHIRL COMMENT nodes and normal-whirl2c CUDA.

## Warp-local value flow

For row b*H+h and chunk c, thread32*w+l scores token64*c+32*w+l. Each of two
warps owns32 tokens and all R latent columns. Scores use the projected-query
and latent-cache dot plus the RoPE dot, followed by the selected scale; source
dots retain ascending F32 K order. Masked/tail lanes carry negative infinity
and skip source score reads. Scores and exponentials stay in lane registers.

Each warp reduces its raw maximum m_w with full-mask shuffle-down offsets
16,8,4,2,1, then broadcasts lane0's result. Source0 denotes physical thread0
or32 in its own warp. Each lane derives the finite-safe maximum and computes
e_t=exp(s_t-safe_m_w). Scratch registers reduce d_w while e_t is preserved.
The second broadcast provides the completed denominator; attention_weight
becomes p_t=e_t/(d_w>0?d_w:1). This probability is normalized over the warp,
not yet the64-token chunk. For finite nonempty scores,1<=d_w<=32; the
exponential mass d_w is not generally the token count.

Both warps visit ceil(R/4) four-column groups. Each lane uses only its own
probability and token cache values. Four private sums reset per group; warp
trees reduce the32 products for each column into a normalized expectation
z_w[j]. At R16, BOTH warps cover columns0..3,4..7,8..11,12..15, with distinct
32-token sets. Total token/cache products remain64*R per full chunk.

## Shared tuples and one publication

Each leader stores [raw_m_w,d_w,z_w[0..R-1]] in its own immutable shared tuple.
The indexing is w*(R+2)+component. At R16:

| Slots | Contents | Producer |
| --- | --- | --- |
|0,1|m0,d0|warp0 lane0|
|2..17|z0[0..15]|warp0 lane0|
|18,19|m1,d1|warp1 lane0|
|20..35|z1[0..15]|warp1 lane0|

There are36 floats/144 bytes; these CTA-local tuples introduce no additional
global buffer. A fast warp may finish its tuple while the other still computes
columns, but it reaches the sole unconditional CTA barrier before reading
any tuple. All stores precede that barrier, all shared reads follow it, and
no slot is overwritten afterward. This removes any read/reuse race.

The old maximum and denominator CTA barriers disappear because each softmax
is warp-local. The old probability barrier disappears because probabilities
never cross lanes or warps. Complete tuples collect all cross-warp dependencies
at one publication. A shuffle transfers warp registers; it does not replace a
general shared-memory fence. All64 threads reach the barrier, and all32 lanes
per warp reach every full-mask collective despite masks/tails/column padding.

## Stable merge and why the masses matter

For each warp, d_w*z_w[j] is its unnormalized weighted sum relative to m_w.
Multiplying by exp(m_w-m), where m=max(m0,m1), changes that sum to a common
exponential origin. Thus t_w=d_w*exp(m_w-m) is the common-origin mass, and

    D = (0+t0)+t1
    alpha_w = t_w/(D>0?D:1)
    z[j] = (0+alpha0*z0[j])+alpha1*z1[j]

This equals the64-token expectation in real arithmetic. For finite maxima,
m_w-m<=0, avoiding exp of large positive raw scores. Finite-safe copies retain
the existing fallback semantics. If d_w==0, branch to zero mass BEFORE exp.
For example a nonempty warp with m=-1000 beside an empty warp must not evaluate
the latter's fallback exp(0-(-1000)) then multiply infinity by zero.

Both leaders intentionally recompute the same scalar merge in order0,1.
They broadcast alpha0 and alpha1 within their own warp. This avoids another
shared statistic publication and another CTA barrier. Thread tid writes final
columns tid,tid+64,...; thread0 writes raw m,D. At R16 only threads0..15 store
columns, but all64 still participate in broadcasts. Four static broadcast
sites execute in each warp: local max, local denominator, alpha0,alpha1.

A numerical example uses one cache column. Warp0 has one allowed score0/value2;
warp1 has two allowed scores log(3)/values10,14. Their summaries are
(0,1,2) and (log(3),2,12). Common masses are1/3,2, coefficients1/7,6/7, and
z=74/7. Direct token weights1,3,3 give(2+30+42)/7=74/7. Averaging the local
expectations equally would give7 and is incorrect. Actual F32 trees/exp/division
use the existing reassociation permission and reference tolerances.

A35-token tail partitions32+3. A one-token tail partitions1+0: the empty warp
produces its own zero latent vector and never consumes the other warp's
probability. Both empty tuples yield raw maximum negative infinity, safe
maximum0, denominator0 and zero latent. A finite allowed single token yields
D1 and its cache vector. The global chunk tuple remains
[raw_m_c,d_c,normalized_z_c]; the separate128-thread inter-chunk merge retains
its own stable rescaling and five CTA barriers.

## Verification and performance scope

Require byte-identical recipes, equal launch manifests, unchanged readable
executable bodies and mapped executable tokens versus v0.25. Regenerate six
profile/context views, independently reopen genuine bare/annotated B files
using ir_b2a -st -src, retain raw T diffs and compare exact explanation payloads
across all three representations. Run42 fresh readable FP64 reference checks,
two fresh native/reference cases, and12 selected-profile sm_90 compilations.
Compare backend resource reports to v0.25. Reuse the42 prior full-shape mapped
outputs only after prior receipt integrity and executable-parity verification;
label these as reused, not fresh executions. Retain tools/source/commands/hashes
and verify isolated replay before publishing a new immutable v0.26 package.

The prior implementation's74 unique tests and13 CUDA compilations remain prior
evidence. Rank16 compilation uses40 registers,144 shared bytes and no spills.
The one-barrier algorithm doubles latent warp-level down-shuffles80->160 at
R16 and adds rescaling/broadcast work. Shared storage8*(R+2) exceeds the old272
bytes above R32. No GPU speedup follows from barrier count or host simulation.

## Convergence and stopping criteria

Freeze the v0.25 one-barrier algorithm; finish this v0.26 comment-only update
as the final documentation gate. Do not introduce another ownership, tiling,
barrier or instruction change as part of this walkthrough. One publication
barrier is the floor for the current nonduplicating two-producer shared-tuple
ownership: deleting it leaves cross-warp reads unordered.

When a usable GPU is available, run one bounded comparison of retained
coverage-v5/three-barrier64, coverage-v6/one-barrier64, and the32-thread candidate
across a declared matrix of representative supported short/long contexts,
shapes, masks and tails. Hold numerical policy, inputs, compiler flags and
timing method constant. Measure whole-call latency as the primary outcome,
local-kernel latency as diagnosis, and resource usage/correctness as gates.
Use repeated measurements to distinguish benefit from timing noise.

Converge by retaining the measured winner for each workload range, or keep the
previous implementation where the new one loses or no improvement is resolved.
Any dispatch/selection changes require a separate measured implementation step;
this comment update changes none. Further kernel work requires an identified
bottleneck and a testable benefit hypothesis. Fewer barriers/instructions alone
are not a stopping or acceptance metric. Without GPU access, close the
implementation/documentation milestone with host/compiler evidence, while
leaving GPU correctness, race-sanitizer, performance selection and timing open.
Transfer owner, successor, independent reviewer and approving lead remain TBD.
