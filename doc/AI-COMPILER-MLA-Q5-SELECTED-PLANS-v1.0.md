# MLA Q5 complete strict plans and selected High lowering v1.0

November 10, 2026. Extends [Q4](AI-COMPILER-MLA-Q4-QUANTIZED-MOVEMENT-v1.0.md)
and [Q3](AI-COMPILER-MLA-Q3-SERVING-v1.0.md). Existing specifications remain
historical material. Owner, successor, reviewer and approving lead: TBD.

## Delivered algorithm and scope

Q5 qualifies executable **synchronous scalar High WHIRL algorithms** for the
existing standalone Q1 buffer/status ABI. The separate space
`mla-q5-strict-plans-v1-17` contains the materialized source baseline and sixteen
strict three-pass plans: W={32,64,128,256}, G={1,2,4,8}. Q4's one-pass cache-service
hypotheses and scores remain unchanged and analysis-only. They are not promoted
to executable plans. The floating fake-quantization control retains the baseline.

The new plans recognize Q2's closed 38-operation attention tail and Q3's actual
four-component encoded/scale source joins. Supported S8 and even-low offset-U4
chains include native gather and functional-update forms. Exact decode/reshape/
gather SSA, common index vector, scale/pool storage, returned-copy order and
removed-value consumer closure must agree. Unrecognized or escaping chains
refuse optimization and keep the baseline. No model-name, dtype-only, phase or
context-length heuristic establishes legality.

Selected construction operates on one active PU in an isolated native compiler
process. The input must be genuine source-form Q1 High, with its exact native
and semantic identity, no previously applied Q5 tag and no live dependence
manager. This round does not add automatic quantized application inside LNO's
legacy selected-FP32 transaction. The resulting selected `.B` goes through the
normal backend, PREOPT and structured memory lowering, then ordinary whirl2c.
GPU mapping, PTX/device execution qualification and TMA remain open; the emitted
host C is an executable algorithm oracle, not a mapped GPU kernel.

## Ordered three-pass algorithm

For each batch element, head group and query, initialize disjoint per-row raw
maxima, denominators and latent accumulators. Each pass traverses token tiles
in increasing order. Each tile decodes the latent and rotary streams once and
is then reused by all valid heads in the group. Tail token/head checks precede
all loads. Prefixes of the now-dead decoded-cache scratch arrays hold the tile;
the complete Q1 formal ABI and its disjoint storage contract are retained.

1. Compute each score with the original ascending latent and rotary dot-product
   folds, source add and alpha multiplication, then mask selection. Update the
   global raw maximum with the source NaN-propagating comparison. After all
   tiles, derive the original finite maximum (zero when nonfinite).
2. Recompute the same rounded score, form `exp(score-safe_max)` and accumulate
   the denominator in original token order. After all tiles, preserve the
   source `denominator > 0 ? denominator : 1` guard.
3. Recompute score/weight, divide each token's weight by the safe denominator
   before multiplication, and accumulate each latent column in original token
   order. Keep masked weighted multiplications: `0 * nonfinite` is observable
   in the source output. Do not elide these operations based on masks or Q3's
   active/valid declarations.

Query/value projections, source quantization, functional cache updates,
statistics/log/finite guards and all six returned values remain executable.
No global score/probability or full decoded-cache buffer is produced by the
optimized attention region. Input caches remain immutable; untouched pool rows
are copied by the retained source functional operations. Source status failures
still require discarding every partial writable result.

For each decoded stream, a scale is loaded once per token into an explicit F32
local. An S8 byte is sign-extended and converted before the original scale
multiplication. Each U4 byte is loaded once into a U4 local, split into its low
and high nibbles, converted, offset by eight in F32, multiplied by its paired
scale and stored into the corresponding two tile columns. A paged token uses
the original checked index SSA and guards its slot before access. P remains
allocator layout; W remains compute tiling.

This tiling changes independent row interleaving but preserves the order of
every F32 dot, max, denominator and weighted-latent fold. It uses three passes
and no split-normalization/rescaling merge. It therefore introduces no new
floating-point reassociation permission. Strict IEEE/roundoff controls remain
required. Parallel split reductions and warp trees need a separate numerical
contract and are not silently enabled by an old Q4 descriptor.

## Complete-plan estimation and convergence

The existing bounded fair portfolio, deterministic proposers/ties, shared lazy
cache and global proposal/unique/time budgets are reused. Exploration receives
only copied Q1 source and Q3 snapshot records, explicit GPU resource/timing and
hierarchy profiles, plus an explicit positive log-warp cost. It performs no
WN/ST/TY construction, compilation, execution, profiling or serving observation.
Q3 export/revalidation remains outside the loop.

The Q5 model covers every surviving source operation and the full attention
algorithm. It charges source quantization/checked conversions, functional
scatter scans, index arithmetic, gather accesses, both projections, ordered
attention arithmetic, max/denominator guards, exp/div/log, result copies and
all modeled launches. Source matmul/reduction contribution counts and
pointwise broadcast reads follow the materialized Q1 loops. RNE/guard work
uses declared conservative scalar envelopes. Reshape aliases allocate no work.

Optimized plans charge three cache/scale/dequant passes per query/head group,
original per-token index loads/checks, three score computations, two exp passes,
per-token normalization and ordered latent updates. The explicit target model
reserves the retained tile, padded head resources, rounded registers/SMEM and
synchronous publication/consume/release costs. No async overlap or free shared
prefix residency is assumed. This physical placement is a future GPU-mapping
obligation; the host oracle uses ordinary ABI scratch. The strict CTA owns all
its token tiles, so occupancy uses batch/query/head-group CTAs, not a falsely
parallel token-split grid.

All timing, cache-hit, sector/amplification, shared-conflict and resource values
remain explicit synthetic/estimated/measured inputs. A complete algorithm cost
estimate is not an instruction-exact prediction or measured GPU time. Unsupported
legality, resources, arithmetic overflow or scratch capacity makes a candidate
ineligible. The finite best is the best evaluated admitted model score; no
claim of global GPU optimality follows. Unsupported relative-gap/economic
certificates remain refused. Same-ID model/resource changes alter the full key.

The producer freezes Q3 once, explores, revalidates and returns one selected
plan with full source/model/control/binding provenance. It does not compile
alternatives. Reference qualification happens after the fixed plan has been
constructed. Any later experimental retuning begins another estimation phase;
regression is never invoked from a proposal callback.

## Native application, checkpoints and analyses

`tensor_kernel_lower q5-key input.B keys.json` reports the exact source/native
identity. A canonical `open64.q5.selected-high.v1` request carries W/G,
source/native keys, Q3 binding key and selected-model/control provenance.
`q5-apply input.B plan.json before.B after.B manifest.json --opt-level=3`
requires the explicit whole-PU O3 transformation contract. Lower levels,
malformed/stale requests and existing outputs refuse before construction.
These compiler commands consume a fixed plan; they implement no serving/AOT
variant selector or JIT compiler.

Before application, capture/revalidate the existing owned source High facts.
Write the before checkpoint, invalidate the epoch, and construct exactly one
selected body. Fixed constants are copied from source WNs. Disable the normal
construction-time simplifier during staging to avoid interning new folded
constants; checked byte-conversion bounds reuse their exact source bits.
Verify ordinary native tree identity/ownership and the no-DSL gatekeeper before
publishing the selected root and after image. Verify that construction appends no global type or TCON entries; scalar and
address types and exact F32 constants are reused from the admitted source.
New local loop/accumulator/COMMENT symbols are visible in the raw diff. No
mapped layout, opcode, type-kind or reader/writer contract changes.

A reopened PU is reader/driver-owned. Its checkpoint helper does **not** call
`DSL_Builder_Select_PU`. It copies the active PU's subsection state in a child,
retains tree/symbol/REGION semantics and drops unavailable optimizer subsections
in that copy, matching the established Q1 standalone rebind protocol. It uses
the existing mapped writer and no-replace temporary/link publication. Native
scalar-type caches are initialized through the normal backend service after
reading the global symbol table. The compiler process owns all newly appended
state; a construction failure terminates it without changing the original input
or publishing an after image. The before image remains reviewable. This is
process-isolated application, not in-process LNO rollback.

No live DU/alias/access/dependence/reduction manager is imported into this
application process. All copied source facts expire before mutation. Selected
images carry the fixed Q5 plan and accepted native key. Normal backend processing
rebuilds its own analyses for the selected image; Q1 PREOPT sessions explicitly
do not recapture source-form facts from Q5's changed loops. The existing specific
COMMENT restoration path regenerates the selected explanation at function entry.
The general PREOPT COMMENT repair remains separate.

## Q3 execution and fallback

The native serving wrapper keeps the qualified source program alongside the
existing Q3 registry. A Q5 study requires a live prepared ticket and exact Q3
snapshot; all ownership, page/scale version, parent publication and lifetime
checks remain enforced. The selected entry is invoked only after matching its
source and original snapshot key and revalidating that snapshot. Q3 Run then
rechecks controls/parent and retains its status, output-scale, synchronous
completion and atomic complete-bundle publication gates. The owner remains
responsible for qualifying that callback against the exact compiled plan/ABI,
as in Q3; a caller-supplied function pointer is not a code certificate.

The compiled High algorithm remains parameterized over actual buffers/pages;
no address, physical page, phase label or active length is baked into it from a
cost snapshot. A different invocation needs its own valid Q3 lease; archived
cost facts grant no lifetime. Failed selection/application/qualification does
not modify the original source file and cannot publish a cache successor.
The materialized baseline remains an eligible executable fallback. Unknown or
unimplemented encodings retain it; packed input is never cast into an old
FP32 materializer.

## Qualification and open gates

Qualification retains genuine `.B` inputs and before/after `.B/.T` images,
raw diffs, selected-plan/cost/identity manifests, pre-/post-PREOPT boundaries,
actual whirl2c C and compiled host libraries. Both selected High and normal
backend output must match the original source, PyTorch and the independent
reference, with exact cache/scale outputs, status guards, input immutability
and canaries. Coverage includes S8/U4, dense/paged/append, prefill/decode/verify,
head/token tails, shared prefixes, no-work declarations, stale native/binding
requests, nonfinite/invalid inputs and the floating fallback. Additional genuine
8193-token captures exercise many tiles and partial allocator pages.

Existing Q3/Q4, source-only Q1, legacy selected-reference, search/convergence,
PREOPT invalidation and backend-consumer dependency checks remain required.
Host evidence cannot qualify GPU launches, resource allocation, TMA or measured
speedup. Q6 owns device mapping/async dequantization and ordering; Q7 owns actual
GPU correctness/performance. Personnel acceptance roles remain TBD.

**AOT selection design remains deferred by explicit user direction.** Separately
qualified prefill/decode AOT variants remain the architectural direction. This
revision defines no selector location, phase-option parser, threshold,
packaging/selector ABI or dispatch policy.

An ancillary emitter fix is required for U4 qualification: ordinary C now uses
whirl2c's existing precise unsigned extract-bits handler, preserving the WHIRL
bit offset and size. The old unary helper spelling lost those payload fields.
CUDA already used this handler; its behavior is unchanged. The scalar host
oracle disables both SIMD and vector intrinsic conversion with the existing
`-LNO:simd=0:vintr=0` controls, so it needs no vendor vector-math library.


Initial software qualification passes 315 original/selected serving checks,
54 additional long-input checks and 128 detached layout/codec contract checks.
Thirteen selected applications (ten original cases, zero-active and two genuine
8193-token inputs) produce 26 High/backend host libraries. All 156 returned
values match the original ordered native source exactly; the selected results
also pass PyTorch and independent-reference checks, status negatives, nonfinite
cases, input immutability and canaries. The short synthetic envelope selects
W32/G4, the explicitly different long latency envelope selects W256/G4, and the
floating control selects the baseline. These are qualification profiles, not
measured device choices or an AOT selection policy.

The existing regression passes 570 Q3/Q4 checks, 309 detached Q4 assertions,
37 control and 144 MR2 checks, 66 Q2 cases, seven selected/native tests and two
PREOPT invalidation cases. Fifteen source-only backend configurations/270 base
results reproduce all 75 Q4 traces and ten executable keys exactly. be.so
NEEDED and undefined imports remain unchanged; adapter and backend consumers
remain link-closed. Full raw images/diffs and independent final-path replay
receipts retain the precise evidence and limits.

The shared hierarchy record retains Q4-specific fields in its full identity.
Q5 charges the original index-SSA loads, arithmetic and guards in its complete
memory/scalar model; it does not additionally apply Q4's separate page-lookup
service penalty. Model predictions remain envelopes requiring calibration.
