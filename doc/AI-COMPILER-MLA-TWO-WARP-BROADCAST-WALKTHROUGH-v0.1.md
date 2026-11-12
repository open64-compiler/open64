# MLA two-warp register walkthrough, v0.1

This v0.24 annotation update explains the v0.23 coverage-v5 width64 local
kernel. VHO emits the explanation automatically into readable optimized CUDA,
real WHIRL COMMENT nodes and normal-whirl2c CUDA. Recipes, executable code,
launch manifests and numerical policy remain those of v0.23. The implementation
contract is AI-COMPILER-MLA-TWO-WARP-BROADCAST-STATISTICS-v0.1.md.

## Ownership and separate summary slots

A CTA contains two full 32-lane warps. Thread t scores local token t; the
64 scores and their unnormalized exponentials stay in private registers.
Shared memory contains probabilities[64] and summaries[4], totaling272 bytes of F32
storage. Summary slots0,1 contain warp maxima; slots2,3 contain warp
exponential sums. Each slot has one writer, the corresponding warp's lane0.
Both leaders read both maximum slots and both sum slots at their respective
phases. The probability array is a separate allocation.

The separation prevents a read/write race after the maximum publication
barrier. Both warps can leave barrier1, but warp0 can finish reading maxima,
compute its exponential sum and store that sum while warp1 pauses before
reading a maximum. Writing the sum to slot2 leaves maxima0,1 intact. Reusing
slot0 would let the delayed warp1 read a sum as a maximum. Barrier1 orders
pre-barrier stores before later reads; it does not order all post-barrier
reads before all post-barrier writes. Reusing maximum slots would require
another synchronization phase under this ownership scheme. Likewise,
normalization writes only probabilities and cannot clobber a delayed sum read.

## Register broadcasts and three publication phases

For scores s_t, each warp reduces its maximum using shuffle-down offsets
16,8,4,2,1. Each leader writes summaries[warp]. Barrier1 makes both writes
visible before either leader loads both. Both leaders independently compute
the complete maximum m in the same F32 fold order, then call
__shfl_sync(0xffffffff,m,0,32). Source0 denotes lane0 of the executing warp:
physical thread0 for warp0 and thread32 for warp1. It never selects a source
in another warp. A reduction tree alone does not deliver the complete result
to every lane; the broadcast does. Each lane retains the raw maximum and
finite-safe maximum in registers, then computes its private exponential e_t.

Each lane preserves e_t while scratch registers perform its warp sum tree.
The leaders write sums at summaries[2+warp]. Barrier2 publishes both sums
before either leader reads them. Both leaders fold slots2 then3 from +0 in
the original F32 order and broadcast the completed denominator D within their
own warp. Final m and D need no shared-memory final-statistic publication.
These broadcasts remove the old final-maximum and final-denominator CTA
barriers, reducing five local phases to three. Two static broadcast sites
execute in each warp, producing four warp-level broadcasts per CTA.

Each lane computes p_t=e_t/(D>0?D:1), writes probabilities[t], and reaches
barrier3. Warp w owns columns group*8+w*4+j for j0..3. Lane l in BOTH column
warps visits tokens l and l+32, so each warp reads probabilities written by
the other. For example warp0 lane0 uses p0 and p32 for columns0..3, while
warp1 lane0 uses them for columns4..7. Barrier3 therefore remains necessary.
Four private latent sums reset per column group, and the immutable shared
probability array can be reread without additional barriers in that loop.

| Remaining phase | Shared producer | Consumer protected by the barrier |
| --- | --- | --- |
|1|Both warp maxima|Both leaders loading maxima0,1|
|2|Both warp exponential sums|Both leaders loading sums2,3|
|3|All64 normalized probabilities|Both column warps reading tokens0..63|

A 32-lane shuffle provides register communication within one warp; it does
not order general shared-memory communication across warps. Removing any of
these three barriers requires a different communication/ownership algorithm
and a fresh ordering proof. All64 threads reach all CTA barriers; all32 lanes
of each warp reach every full-mask shuffle. Tail and mask conditions guard
memory/arithmetic rather than collective participation.

## Tails, tuple semantics and performance scope

For a35-token tail, warp0 has32 score tokens and warp1 has3. Both leaders
still fold both summaries. For a one-token tail, warp1 contributes neutral
statistics but must still participate and compute its valid latent columns
from p0 after barrier3. Empty chunks have raw maximum negative infinity,
finite-safe maximum0, denominator0 and normalized latent0. Finite allowed
one-token chunks have denominator1 and their cache vector as local latent.

The local tuple remains [raw_m_c,d_c,normalized_z_c]. The separate128-thread
merge preserves stable rescaling d_c*exp(m_c-m), with zero-mass guards before
exp, and retains its five CTA barrier sites. This walkthrough concerns the
64-thread local kernel, not that merge. Chunk count, launches and global
partial-tuple traffic are unchanged.

The expected opportunity is reduced waiting and shared statistic traffic.
Duplicated two-summary folds, broadcasts and scalar register lifetimes cost
instructions/resources. The retained sm_90 rank16 compile has40 registers,
272 shared bytes and no spills, equal to the v0.23 implementation; both272
and the old288-byte footprint round to384 under the supplied128-byte quantum.
Consequently a residency gain cannot be inferred from the raw16-byte reduction.
No device speedup is claimed. ptxas's barrier-resource count is distinct from
the three dynamic publication phases.

## Review and reproducibility

Regenerate all six profile/context views from byte-identical v0.23 recipes.
Independently reopen genuine bare/annotated B files with ir_b2a -st -src,
retain raw T diffs, and verify exact explanation payload equality among
readable CUDA, COMMENT nodes and normal-whirl2c CUDA. Require unchanged launch
manifests, readable executable bodies and mapped executable tokens. Compile
all12 readable/mapped sm_90 sources and compare resource reports. Run42 fresh
readable FP64-reference cases plus two fresh native mapped/reference cases
covering one-token and partially active second-warp tails and R21 columns.
Reuse the42 prior full-shape mapped outputs only with verified prior receipt
and executable parity; do not report them as fresh executions. Retain source,
tools, commands, hashes and an isolated replay in a new immutable v0.24 packet.

The prior implementation's69 tests remain prior evidence. GPU execution,
race-sanitizer validation and timing remain hardware-gated. Owner, successor,
independent reviewer and approving lead remain TBD; transfer acceptance is open.
