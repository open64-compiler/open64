# MLA 64-thread and 32-thread walkthrough, v0.1

This comment-only amendment explains the v0.19 physical kernels. v0.20 expands
the VHO-generated explanations and republishes readable CUDA, real native
WHIRL COMMENT statements and normal-whirl2c CUDA. Selected recipes, arithmetic,
launch geometry and numerical contracts remain unchanged. The implementation
contract remains AI-COMPILER-MLA-NARROW-BLOCKS-v0.1.md.

## Follow the stages

Query absorption produces the projected query. One fused local kernel then
computes scores, mask, stable local softmax and normalized latent accumulation.
Each CTA owns a (row,chunk); row=block%rows, chunk=block/rows. For B=1,H=3,
L=8193,R=16, width64 has129 chunks/387 CTAs, width32 has257 chunks/771 CTAs.
Each writes [m_c,d_c,z_c] at (chunk*rows+row)*(R+2). A separate128-thread merge
forms the global latent/LSE, followed by the retained value projection/stores.
The ordinary127-token short case remains direct with128 threads and no merge.

## Token scoring and padding

At64 threads, warp0 scores local tokens0..31 and warp1 scores32..63. At32
threads, the single warp scores0..31. Scoring uses thread id, while each latent
column warp later visits tokens lane,lane+32,... for its own columns.

| Same-width comparison | Width64 | Width32 |
| --- | ---: | ---: |
| Old / new threads per local CTA |128 /64|128 /32|
| Old / new score capacity, floats |128 /64|128 /32|
| Old / new exp-loop slots, all rows/chunks |49536 /24768|98688 /24672|
| Old / new geometric padding slots |24957 /189|74109 /93|
| Selected local shared bytes |288|160|

Real row/token positions are3*8193=24579 in either variant. The new full chunks
have no block padding. The final chunk starts at8192 and contains one token;
its63 or31 neutral score slots per row account for all remaining padding.
Mask-false positions are separate from geometric padding. Neutral slots skip
source dot reads but remain in collectives. Source-loop counts do not establish
executed instruction counts or elapsed time; compiler folding/predication matters.

The shared array is reused in place for score, exp(score-safe_max), and normalized
probability. Eight selected statistics floats retain raw max[4], safe max[5]
and denominator[6]. Two-warp folds additionally use[0]/[1]. A one-warp block skips
that fold. Selected footprints are4*(64+8)=288 and4*(32+8)=160 bytes. The v0.19
sm_90 compilation reports40 registers/288 shared bytes for width64,35/156 for
width32; CUDA removed one unused statistics slot in the latter. These are
compile reports, not runtime occupancy measurements.

## Publication and synchronization

Both use complete32-lane shuffle trees, full mask0xffffffff, offsets16,8,4,2,1.
Each lane accumulates token contributions; lane0 obtains its warp's result.

| Phase | Two-warp block64 | One-warp block32 |
| --- | --- | --- |
| Max summaries | CTA barrier publishes two warp summaries | No cross-warp fold |
| Final max | Thread0 folds two; CTA barrier publishes raw/safe max | Lane0 publishes; full-mask warp fence |
| Denominator summaries | CTA barrier publishes two warp sums | No cross-warp fold |
| Final denominator | Thread0 folds two; CTA barrier publishes denominator | Lane0 publishes; full-mask warp fence |
| Normalized probabilities | CTA barrier publishes immutable weights | Full-mask warp fence |

Width64 retains five CTA barrier phases with fewer participating threads.
Width32 has three warp publication fences and zero local CTA barriers. Every
lane reaches them even for all-masked chunks, column tails or a one-token tail.
Explicit __syncwarp memory ordering is required under independent thread
scheduling. The third fence retains the canonical probability-publication
contract; this does not claim minimal possible fences for all ownership schemes.
The128-thread merge independently retains its five CTA barriers.

## Column groups and the cost of fewer warps

Each warp owns four F32 lane-private sums. Column=group*(4*warps)+warp*4+j,
j=0..3. At each group, zero the sums, traverse valid tokens lane,lane+32,...,
reduce four sums, store valid columns from lane0, and advance the group. No
shared cache tile or latent-accumulation barrier exists. Column bounds guard
memory/stores, never collective participation. Probabilities remain immutable.

| Block, R=16 | Group0 | Group1 | Group2 | Group3 |
| --- | --- | --- | --- | --- |
|64 threads|warp0:0..3; warp1:4..7|warp0:8..11; warp1:12..15|—|—|
|32 threads|warp0:0..3|warp0:4..7|warp0:8..11|warp0:12..15|

A64-thread warp's lane0 accumulates tokens0,32; lane31 accumulates31,63.
A32-thread warp's lane l accumulates token l once per column group. Both warps
in the64-thread block revisit all64 probabilities for different columns;
scoring ownership should not be confused with latent-column ownership.

A group executes four five-round trees:20 warp-level shuffle calls. For R=16,
128 threads have4 warps*1 group*20=80 calls/block;64 have2*2*20=80;32 have
1*4*20=80. Per warp, the sequence becomes20,40 or80 calls respectively. These
counts exclude the max/denominator trees. Less padding and fewer barriers do
not remove the latent reduction's aggregate tree work; more groups put more
work on each warp's sequential path. The CTA grid stays fixed at a fixed width.
A four-warp register allocation quantum can also keep allocated registers/CTA
unchanged despite fewer launched warps. CTA-goal coverage is not achieved warp
occupancy. Compare whole-call GPU time with the retained128-thread candidate.

## Tails, merge and numerical checks

An allowed finite one-token tail has local d_c=1 and z_c=its cache vector.
Its global weight still depends on the score relative to other chunks. A masked
tail has d_c=0,z_c=0 but still executes the same fences and group loops.
The merge computes m=max m_c, t_c=d_c*exp(m_c-m) for nonempty chunks,
D=sum t_c, z=sum(t_c/D)*z_c and LSE=m+log(D). Select zero before exp for empty
chunks to avoid0*infinity. All-empty rows return zero latent and-infinity LSE.
There is no new global score buffer, merge launch or tuple allocation.

The annotation gate requires identical recipes/manifests and executable CUDA
tokens relative to v0.19, exact explanation payload equality across all views,
real annotation-only comments.before/after.B and independent ir_b2a -st -src
traces/raw diffs, fresh readable-reference checks and focused native reference
execution. v0.19's content-addressed mapped/reference evidence is reusable only
after source-token parity succeeds. Fresh sm_90 compilation checks comment
lexing and retains resource/spill logs. GPU execution/timing remains unavailable;
no speedup is claimed. Roles and transfer acceptance retain their earlier TBD
status. Older immutable packets are preserved, including their original tools:
canonical explanation audits require the matching annotation producer.
