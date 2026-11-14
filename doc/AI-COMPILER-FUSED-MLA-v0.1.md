# Fused absorbed MLA without global score buffers

Version v0.1, November 5, 2026. Implements the next AOT candidate after
[cooperative MLA v0.3](AI-COMPILER-TENSOR-KERNEL-LOWERING-v0.3.md).
Primary owner, successor, independent reviewer and approving lead: TBD.
Independent review and role assignment remain transfer-acceptance blockers.

## Result and limits

The selected physical schedule fuses latent and RoPE score contractions,
mask/scale, stable softmax and weighted-latent accumulation in one CTA per
head/chunk. Short context runs a complete head in one CTA. Long context emits
compact chunk-local statistics and normalized latent vectors, then merges them
with stable exponential rescaling. Both global score tensors disappear.

Query absorption remains a separate contraction, as do value projection and
external output/LSE stores. There is no JIT dependency, async pipeline, tensor
instruction, persistent CTA, or claimed production tuning threshold.

| Existing fixture | Prior cooperative | Fused score/softmax/latent |
| --- | ---: | ---: |
| Short L=127 launches | 9 | 5 |
| Short global temporary bytes | 3528 | 468 |
| Long L=8193 launches | 12 | 6 |
| Long global temporary bytes | 204240 | 7596 |
| Short score allocation bytes | 3048 | 0 |
| Long score allocation bytes | 196632 | 0 |

Global temporary counts include query/projection intermediates and attention
bundles, excluding inputs, outputs, shared storage and test guards. These are
structural facts, not measured latency gains. The fixtures have B=1,H=3,R=16 and
are correctness workloads, not full production DeepSeek configurations.

## Admission and scope

The existing PU-local VHO tensor lowerer owns selection, verification and WN
materialization. The standalone driver owns source/device PU contexts, source
mapping, checkpoints, recipe parsing and launch metadata. No service traverses
another PU or mutates the source tensor image. This extends the established
physical lowering; general be_driver/High-WHIRL/LNO integration remains a
separate milestone under the existing compiler architecture.

Admission proves the complete 48-operation captured topology (46 tensor ops and
two result stores), every dataflow edge, intermediate shape/dtype, constant
mask/finite/divisor guard, transpose and reduction axis. It derives operands
from formal roles and edges, never model/function/value names. Compatible
static B/H/L/R/Dr/Dn/Dv are permitted. Equivalent but differently captured graphs
are deliberately rejected by this initial closed recognizer. Unsupported graphs
retain the prior v3 candidate when selected without this option.

The candidate requires fused cooperative O3 and explicit numerical permission.
O0/O1/O2, unselected fusion, strict/mismatched policy, nonfinite scale, altered
numerical guards, wrong topology and unsupported shared capacity are rejected
before device mutation/publication. Existing operator/version/attribute, dense
layout, alias, source-position and byte-bool rules still apply.

Unsplit L is at most 4096. Split width is 2..4096; chunk count is at most 4096.
Derived final/partial allocations also obey the existing 16777216-element bound.
These resource limits are correctness/admission limits, not cost-model decisions.

## Local CTA algorithm

A CTA has 128 threads (four full warps) and owns one (batch,head,chunk).
Thread t visits local tokens t,t+128,... in a capacity rounded to 128. Guard reads
against the real token interval. For an allowed token, compute

    s_t = alpha * (sum_r q_absorbed[r]*cache[t,r]
                   + sum_j q_rope[j]*rope_cache[t,j]).

Each dot retains ascending scalar F32 order. Mask-false/padded tokens become
negative infinity. Scores are written only to shared memory. Four warp trees
reduce local maxima; lane zero publishes one maximum per warp. A CTA barrier
precedes thread zero's four-value fold and another publishes its result.

Retain raw m_c for the tuple. Use the source finite fallback m_safe=0 if m_c is
nonfinite. Overwrite shared scores with exp(s_t-m_safe), reduce their sum d_c with
the same CTA protocol, divide by the source safe denominator once in shared
memory, then synchronize before reuse. Thus p_c(t)=exp(s_t-m_safe)/safe(d_c).

Four warps process four latent columns per column group. For each 32-token tile,
all threads cooperatively stage 32x4 cache values using the existing XOR layout.
A publish barrier precedes reads and a release barrier precedes overwrite. Warp
lanes accumulate p_c(t)*cache[t,r] with stride 32, then shuffle offsets
16,8,4,2,1 with full mask. Valid lane zero writes each local latent coordinate.
Column and token tails still participate in every collective.

Normalizing before the local vector product avoids large unnormalized FP32
latent sums. No probability tensor is written globally. Cache values are still
read for both scoring and weighted accumulation; this does not eliminate all KV
traffic or replace scalar dot products with tensor instructions.

Short direct output is [z[0..R-1],LSE] per row, stride R+1. Fully masked rows use
zero latent and negative-infinite LSE. Projection and LSE consumers read logical
views of that physical bundle; neither copy nor score recomputation is required.

## Long-context stable merge

For W=256 and L=8193, C=33: 32 full chunks and a final one-token chunk. Partial
storage is chunk-major, with (c*B*H+row)*(R+2) indexing

    [m_c, d_c, z_c[0..R-1]],

where z_c is normalized by its own chunk's denominator. A plain sum of z_c is
incorrect. The separate merge CTA for each row computes

    m = max_c m_c
    D = sum_c d_c * exp(m_c - m)
    a_c = d_c * exp(m_c - m) / D
    z = sum_c a_c * z_c
    LSE = m + log(D).

Use the source finite-max and positive-denominator fallbacks. An empty chunk
(d_c=0) takes zero before evaluating its exponential factor. This prevents
0*exp(0-m) from producing NaN when surviving scores are very negative. All-empty
rows remain zero latent / negative-infinite LSE. The max/D folds use full-CTA
reductions; shared normalized a_c weights are reused across all latent columns.
A warp per column accumulates chunks with stride 32 and finishes with the tree.

The formulas are the partitioned stable-softmax identity, related to the
IO-aware tiled attention approach in
[FlashAttention](https://arxiv.org/abs/2205.14135). This implementation stores a
whole bounded chunk's scores in shared memory and merges normalized chunk
vectors; it does not claim the paper's full implementation or performance.

## WHIRL, source and numerical contracts

The original tensor DAG is preserved. The detached v4 recipe seals
fuse_attention=true and attention_numerics=fp32-local-softmax-rescaled-merge.
The plan creates physical bundle allocations, skips the two score roots and
separate max/denominator roots, and redirects logical latent/LSE reads to the
bundle. Only the owning lowering makes those decisions. The readable printer
consumes the selected bundle/roots; it does not discover attention or select
scheduling. Its serial helpers reproduce the native F32 grouping for review.

All device computation is ordinary DO_LOOP/ARRAY, IF, scalar math, existing
shared-symbol attributes and typed effectful CUDA CALLs with immediate return
preg capture. No WN opcode, TY kind, binary layout, ELF section, frontend API or
be.so dependency is changed. Normal whirl2c prints the resulting kernels.
No subsequent optimization may move collectives or change their participation.

Kernel metadata records selected mapping, shared bytes and tuple layout.
Shared capacity S=ceil(W/128)*128 yields 4*(S+128+8) bytes for local attention;
merge uses S=ceil(C/128)*128 and 4*(S+8). For these fixtures that is 1056 bytes
short, 1568 long partial, 544 long merge. The normalized bundle and partial tuple
have distinct meanings and cannot be interchanged by a host adapter.

This option explicitly permits F32 regrouping and local normalization followed
by rescaled merging. It does not promise source F32 bit equality; check finite
results against the expanded FP64 reference at rtol=2e-5, atol=2e-6 and check
required infinity/masking behavior. No global fast-math is enabled. CUDA
compilation uses FMA contraction and FTZ disabled as in the preceding evidence.

Descriptions of all new decisions are generated as real WHIRL COMMENT nodes
and consumed verbatim by both source views. They explain shared score lifetime,
normalization, tails, rescaling, bundle indexing, guards, scope and costs.
Annotation-only checkpoints must preserve emitted executable tokens. Older
canonical explanations continue to use their retained producer/auditor.

## Invalidation, implementation order and acceptance

1. Prepare/verify the active source PU without mutation. Select the old fused
   cooperative candidate first, then explicitly request score/softmax fusion.
2. Prove the closed graph and numerical/resource contract; derive bundles and
   the five/six-stage physical plan. Refuse unsupported cases before allocation.
3. Driver creates device PUs and captures before. Materialize native scalar,
   shared, reduction and rescaling operations in each active PU. Verify types,
   source/binding/prototype joins, body fingerprint, launch and tuple metadata.
4. Generate COMMENTS; validate and capture after. Also capture before/after
   native COMMENT insertion around the already materialized executable body.
5. Independently reopen .B images with ir_b2a -st -src and retain raw diffs.
   A separate prior-to-fused diff compares construction candidates; it is not
   an in-place rewrite checkpoint. Emit normal whirl2c CUDA and audited launches.
6. Execute the readable review and synchronized mapped host paths against saved
   expanded FP64 oracles. Compile both with NVCC and retain resource reports.
7. Publish a focused commit and immutable v0.11 receipt. Retain tools, sources,
   inputs, oracles, native images/traces, diffs and exact updated file list.

Any DAG, dimension, mask/guard, scale, chunk width, numerical policy, bundle
layout, shared bound, barrier or launch change invalidates the physical products,
explanations and dependent source. No persistent alias/SSA/LNO analysis is reused
across the detached materialization boundary. Future integrated passes must
invalidate those analyses within their driver-owned PU scope.

Focused gates cover the existing 14 short/long scenarios; B=2,H=2,R=5 column and
batch tails; L=1/65/129; W=33 uneven chunks; all-masked heads; empty chunks with
very negative scores; large finite cache values that would overflow unnormalized
vectors; renamed symbols; O0/O1/O2 and permission boundaries; altered semantic
guards/scale/topology; and mutation of masks, barriers, prototypes, bounds,
resources, tuple layout or COMMENTS. Host reverse-order launches must match.

A usable GPU is still needed for hardware correctness, Compute Sanitizer
memcheck/racecheck/synccheck, occupancy/latency and end-to-end speed comparisons.
Fewer global bytes, exponentials and launches are expected benefits, not measured
speedup. Risks include shared/barrier costs, register pressure, less CTA-level
parallelism than the four-column schedule, scalar score-dot cost, F32 rounding,
closed-pattern coverage and incomplete be_driver integration. Retain the previous
AOT candidate and select by measured or calibrated-model workload profitability.
