# Fused tensor kernels and parallel split reductions for MLA

Version v0.2, November 5 2026. Primary owner, successor, independent reviewer,
and approving lead: TBD. Transfer acceptance remains open. This specification
extends the preserved [executable baseline](AI-COMPILER-TENSOR-KERNEL-LOWERING-v0.1.md).

## Result and scope

The focused AOT pipeline selects pure producer fusion for the short MLA
fixture and fusion plus contiguous split reductions for the long fixture.
All arithmetic, indexing, chunk bounds, scratch accesses, and CUDA coordinates
are ordinary WHIRL before normal `whirl2c -cuda` emission. The CUDA emitter and
backend shared library are unchanged by this milestone.

| Fixture | Baseline kernels | Selected kernels | Baseline temporary bytes | Selected temporary bytes |
| --- | ---: | ---: | ---: | ---: |
| Short, L=127 | 34 | 9 | 27,542 | 3,528 |
| Long, L=8193 | 34 | 12 | 1,664,940 | 204,240 |

These are launch counts and allocated storage, not measured speedups. Selected
storage includes split scratch and excludes inputs, outputs, and test guards.
There is no universal length threshold or automatic runtime dispatch policy.
The caller selects the short or long AOT recipe explicitly. The shape-specialized
symbols allow both variants to coexist in a single compiled CUDA module.

This is producer fusion across pointwise/indexing operations into contractions,
reductions, and result stores. Contractions and reductions remain materialization
boundaries. It is not a single fused attention kernel or a FlashAttention pipeline.
Scores still have global intermediates. Shared-memory staging, warp reductions,
tensor instructions, and asynchronous copies are subsequent work.

## Ownership, continuity, and admission

`be/vho/tensor_kernel_lower.cxx` owns PU-local semantic selection and WN creation.
`ir_tools/tensor_kernel_driver.cxx` owns source selection, fresh device PU creation,
scope changes, checkpoints, JSON parsing, and launch-manifest publication.
The source `.B` and all original operations/attributes remain unchanged.

The existing VHO AIO-5 fusion service was inspected: its implemented discovery
matches matmul/bias/activation and residual/activation patterns in native images.
It does not execute arbitrary scalar producer indexing through reshape,
transpose, masking, exp/log, and reduction boundaries. This extension stays
inside the existing tensor lowering service and selects its physical expansion;
it does not add another DSL simplifier or alter the common fusion records.
Existing WN constructors/simplification and the established binary reader/writer
are reused. Integrating this selected physical expansion with AIO-5 candidate
costing is a later driver-integration task.

The existing numerical-accumulation verifier was also inspected. It requires
target/instruction and precision-evidence IDs for its compute-instruction plans.
These scalar SIMT chunks have no such selected compute instruction. No fabricated
IDs or precision evidence are supplied. Reassociation permission is instead an
explicit focused-recipe policy; instruction-backed integration must connect the
established numerical contracts when those target instructions are introduced.

The baseline closed operator/version/attribute, dtype, dense layout, static
shape, effect, and storage checks still apply. No additional DSL opcode, WN
layout, type kind, mapped section, tensor trait, or frontend dependency is added.
The source graph is a pure source-order DAG in one admitted PU. No cross-PU
analysis, IPA, persistent SSA, LNO dependence cache, or model-name recognition
is used. This service does not claim general LNO loop-transformation coverage.

Both fusion and splitting require O3. O0/O1/O2 keep the original serial semantic
lowering. Fusion preserves each scalar expression's operation order; ordinary
contraction/reduction loops remain ascending. Splitting additionally requires
explicit floating-point reassociation permission. Global fast-math remains off.

## Selected schedule and producer fusion

The source recipe remains complete. A deterministic detached schedule identifies
each executable root and its boundary inputs. Every matmul, reduce_max, and
reduce_sum is a materialization root; external result stores are also roots.
Pure pointwise, fill, reshape, copy, and transpose producers are recursively
inlined into those roots, stopping at runtime inputs or another materialized
root. Transpose changes explicit address arithmetic instead of writing a
transposed allocation. Reshape preserves flat dense indexing.

Every expression is a fresh WN tree with one owner. Expanded producer traversal
is bounded to 4096 visits per root and diagnosed before allocation if exceeded.
No algebraic reassociation is performed by this traversal. Some producers are
recomputed across consumers, so lower launch/storage counts do not prove a
runtime improvement. The first launch pins all source input bindings, including
unused shape-only fill operands, to preserve the packed input ABI.

Each mapped work item owns one distinct output slot. Inputs are read-only and
all temporary/result allocations are separate canonical dense storage. O3 uses
128-thread blocks and explicit guarded CUDA coordinates. Kernel launches execute
in recorded order on one stream. There are no intra-kernel inter-thread reads,
barriers, atomics, or ordering assumptions.

## Split mathematics and numerical contract

Let a reduction width be K, selected chunk width W, and C=ceil(K/W). Chunk c
covers [cW,min((c+1)W,K)). Each (output element, chunk) is one independent
CUDA work item. Scratch and work items are chunk-major: partial[c*N+output],
where N is the number of output elements. Neighboring output coordinates remain
together, including contiguous latent-rank cache accesses. A subsequent kernel
reduces the C partials in increasing chunk order. Sum and dot-product partials
start at +0; maximum partials start at negative infinity and preserve NaN
propagation using the baseline maximum rule.

For a sum, partial[c]=sum(x[k], k in chunk c), then result=sum(partial[c]).
For a dot product, partial[c]=sum(a[k]*b[k], k in chunk c). Each partial's
ascending loop retains source product order. The outer merge changes the
addition grouping. The explicit `--allow-reassociate` option authorizes this
change; tolerance-based numerical acceptance does not imply bitwise equivalence.
Products, partials, merges, and outputs remain F32. FMA contraction and FTZ are
disabled in the certification CUDA build.

For stable MLA softmax, the existing global maximum stage is split first:

    m = max_c max_{k in chunk c} score[k]
    denominator = sum_c sum_{k in chunk c} exp(score[k] - m)
    latent[r] = sum_c sum_{k in chunk c} probability[k] * cache[k,r]

The denominator and weighted latent product have their own partial-and-merge
pairs. The global maximum completes before exp/sum, and the global denominator
completes before normalized weighted accumulation. This retains the source's
stable global normalization; it is not a local-softmax merge approximation.
All-masked rows follow the existing isfinite/select guards: zero output and
negative-infinite LSE. Empty masked chunks contribute the appropriate neutral
value. The original alpha remains 1/sqrt(D_nope+D_rope).

For L=8193 and W=256, C=33 and the final chunk contains one element. The three
eligible reductions are maximum, denominator sum, and weighted latent matmul.
Maximum and sum each expose 3*33=99 partial work items instead of 3 serial
outputs; the rank-16 latent product exposes 3*16*33=1584 instead of 48.
The maximum inner context loop becomes 256 iterations, followed by a 33-partial
merge. No context-wide serial loop remains in this selected fixture.

## Handoff, exit contract, and invalidation

Unoptimized recipes retain `open64.tensor-kernel.v1`. Selected recipes use v2,
adding `fuse`, `split_width`, and `reassociation` to the closed schema. Canonical
seals cover the complete source facts and policy. They detect stale handoffs;
they are not cryptographic authentication. Artifact SHA256 identifies source
and generated images. A nonzero width requires fusion and explicit permission;
allowed widths are 2..4096. Scratch is bounded by the existing 16M-element limit.

Device PUs record the original root identity, source node/version, selected
fusion and split stage/width, work-item count, launch dimensions, argument and
scratch bindings, and executable-body fingerprint. Merge IDs are root+4096;
source operation counts are capped at 4096 to keep identities disjoint.
Auditing independently reopens the image, checks specialization-qualified names,
prototypes, body, policy, bindings, order, and count before publishing launches.
Mutated loop bounds, launch bindings, prototypes, or policy must be rejected.

The exit contract is scalar pointer formals and ordinary ARRAY, loads/stores,
DO_LOOP, IF/SELECT, integer coordinate arithmetic, and pure math intrinsics.
There are no native DSL expressions or implicit missing CUDA decisions.
Source/device images are independently reopened with `ir_b2a -st -src`, with
matching-stem traces and raw before/after differences retained. The focused
driver creates new PUs rather than mutating the original source PU.

Changing graph, descriptors, numerical permission, chunk width, level, or target
invalidates the schedule and every downstream device image, launch audit,
generated CUDA, cubin, correctness result, and resource observation. Source
analyses remain valid because source IR is unchanged. New PUs have no inherited
alias, dependence, SSA, feedback, or cost analysis. An integrated be_driver
pipeline must rebuild applicable analyses after creation; it must not reuse
facts from the source or an earlier selected schedule.

## Reproduction and acceptance

```sh
tensor_kernel_lower prepare mla.B baseline.json --verify 3
tensor_kernel_lower optimize baseline.json selected.json --fuse
# For the long-context recipe instead:
tensor_kernel_lower optimize baseline.json selected-long.json --fuse \
  --split-width 256 --allow-reassociate
tensor_kernel_lower device selected.json device.before.B device.after.B
tensor_kernel_lower audit device.after.B selected.json launch.json
whirl2c -cuda -TARG:abi=n64 -O0 \
  -CLIST:dotc_file=kernels.cu -CLIST:doth_file=kernels.h device.after.B
```

`mla_kernel_review.py --optimize` certifies short fusion and long splitting,
with O0 unchanged. Seven input scenarios cover original masks, one masked head,
all masked, tail-only valid data, empty first chunk, equal scores, and large
finite scores. Both output and LSE use the independently expanded scalar FP64
oracle at rtol=2e-5, atol=2e-6. Host execution substitutes only CUDA qualifiers
and coordinates; arithmetic is unmodified emitted CUDA. Reversed work-item
order must give identical host results, supplementing the disjoint-write proof.
Canaries and unchanged-input checks cover every allocation. Scoped ASan/UBSan
runs cover the changed compiler files and generated host execution.

Generic controls cover fused boolean/subtraction/fill, unused-input ABI,
broadcasted matmul, uneven split tails, NaN maximum, level gates, missing
reassociation permission, malformed policies, and no-output rejection. CUDA
certification compiles four SM90 variants and the combined 21-entry optimized
short/long module. Compiled resource data is evidence of compilation only.

`validate_mla_gpu.py` discovers all saved oracle cases and executes only the
real CUDA launchers on a compatible Linux x86_64 GPU host with CUDA runtime
available. It exits 77 for unavailable hardware and never substitutes CPU.
No usable device exists in the current environment (probe error 35); device
accuracy, race checking, timing, crossover tuning, and production performance
acceptance remain open. No throughput claim follows from host simulation.

## Next implementation milestones and risks

1. Run saved launchers and GPU memory/race checks on SM90 hardware, preserving
   numerical and diagnostic evidence for every scenario. Close device accuracy
   before accepting the optimization for production.
2. Measure launch time, register/spill use, occupancy, memory traffic, and total
   latency against the preserved 34-kernel baseline. Tune chunk width and
   determine crossover by workload; retain baseline fallback on regressions.
3. Integrate selected schedule ownership into be_driver: source gatekeeper,
   PREOPT canonicalization, VHO semantic admission/selection, canonical loops,
   PREOPT/LNO dependence and tiling as applicable, structured memory lowering,
   GPU mapping, post-map validation, then CUDA/PTX emission. Rebuild analyses
   at each invalidating boundary. Do not move scheduling policy into whirl2c.
4. Fuse across contractions/reductions with shared-memory or warp primitives
   only after their explicit WHIRL contracts, synchronization proof, target
   limits, numerical policy, fallback, and device validation exist.

Main risks are producer recomputation, larger expressions/register pressure,
limited occupancy for small output counts, extra split launches/scratch, changed
sum rounding, and unmeasured GPU behavior. The current implementation preserves
an explicit unoptimized recipe and both earlier source/reference CUDA lanes.
