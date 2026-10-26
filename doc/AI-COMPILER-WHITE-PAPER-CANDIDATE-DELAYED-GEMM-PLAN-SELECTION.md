# Delayed Joint Plan Selection for GPU GEMM

Status: white-paper candidate, not a novelty or performance claim
Date: 2026-10-22
Scope: SMEM cache-blocking and its interaction with GMEM access, register reuse, and movement scheduling
Design source: `AI_compiler_optimization_design_v0.3.md`, Section 6A.3B; hardware detail design, Sections 4-5

## Abstract

GPU GEMM optimization often presents shared-memory staging, register tiling,
coalescing, and asynchronous movement as successive steps. Their costs are
coupled. A coalesced direct-load kernel can already benefit from L1/L2; a
staged kernel saves some repeated requests but adds shared-memory traffic,
barriers, capacity pressure, and possible bank conflicts. Register tiling can
change those terms again. We propose retaining legal alternatives as typed
plans through movement scheduling, selecting only after a target-aware
whole-plan cost comparison, and materializing one selected kernel in WHIRL.
The research question is whether this delayed decision improves candidate
ranking and measured GEMM performance under a bounded compile-time budget.

## 1. Motivation and evidence

In [Boehm's CUDA GEMM worklog](https://siboehm.com/articles/22/CUDA-MMM),
Kernel 2 changes lane-to-output ownership for GMEM coalescing without
physically repacking A or B. Kernel 3 stages 32-by-32 A/B tiles in SMEM,
using a population and a pre-overwrite barrier per reduction tile. The blog
reports only a moderate incremental gain after the coalesced kernel and
observes strong L1 behavior and later SMEM-load pressure. Its later register
tiles change the economic value of the same staging decision. These are
results on the blog's hardware and kernels, not Open64 measurements.

[TVM MetaSchedule](https://tvm.apache.org/docs/deep_dive/tensor_ir/tutorials/meta_schedule.html)
already treats schedule choices as a search space, while
[Triton's matmul tutorial](https://triton-lang.org/main/getting-started/tutorials/03-matrix-multiplication.html)
uses configurable tile parameters and autotuning. We must therefore not
claim that retaining alternatives or tuning tiles is new. The proposed
distinctive question is whether a common, persistent plan IR can postpone
the *joint* SMEM-residency, A/B copy-layout, register-fragment, and P8
movement-schedule choice through a single whole-plan legality/cost gate while
preserving source-to-WHIRL decision evidence. Prior-art review is still open.

Open64 LNO's `osprey/be/lno/model.cxx` and `cache_model.cxx` are local design
antecedents for resource and cache modeling. LNO's sequential L1-before-L2
blocking choice is also a warning: delaying only the final rewrite does not
help if an earlier pass irreversibly prunes the better combination.

## 2. Proposed representation and decision

For each semantic GEMM and target profile, construct a bounded family of
tuples:

```text
(GMEM representation, CTA/warp/thread tile, A/B GMEM copy maps,
 A/B SMEM layouts or no-SMEM, register fragment and consumer maps,
 P8 movement event graph, CTA order, resource ledger, edge policy)
```

Keep at least these comparable candidates when legal:

| Family | A/B residency and movement | Distinguishing cost |
| --- | --- | --- |
| D | Direct/cache-served GMEM loads | Requested sectors, cache hits, register demand; no A/B SMEM slots or staging barriers. |
| S1 | One A/B SMEM slot, synchronous loads | Reuse versus SMEM read/write traffic, bank behavior, dynamic barriers, CTA residency. |
| SN | Multiple A/B slots, async copy or TMA | Overlap versus setup, barriers, extra slots, occupancy and L1 carveout. |

A physical GMEM repack is a separate representation candidate with its own
allocation, conversion traffic, and amortization horizon. Changing the
warp-lane access map alone is not a repack. A and B can have different
cooperative copy maps, layouts, and, where verified, stage depths.

Apply legality before cost: semantic/numerical contract, dependence and
tile-range alias facts, unique C ownership, alignment and boundary policy,
barrier uniformity, target transfer rules, bank/layout compatibility,
per-CTA SMEM, per-SM residency, registers, and safe slot lifetimes. Prune
only with recorded proof or a declared bounded-search budget. The latter is
a heuristic omission, not proof that an omitted candidate is inferior.

The P8 event graph names `acquire`, `issue`, `completion`, `wait`,
`compute-read`, and `release` for each A/B slot. It determines whether
prefetch hides latency and whether a slot can be overwritten. Cost the
critical path rather than subtracting a fixed overlap fraction. Count
dynamic K-tile barriers (approximately two per tile in the simple S1 loop,
subject to proven edge elision) instead of one constant pair for a whole
GEMM. Compare legal plans at P9 using compute, exposed movement,
communication, synchronization, launch, layout conversion, spills,
occupancy, grid waves, and uncertainty. Retain D or a verified S1 kernel as
fallback. Rewrite only the winner into a device WHIRL PU.

## 3. Experimental control and trace

Registered driver spelling: `-Wb,-DSL:gemm_plan_search=baseline|joint`.
`baseline` is the default and preserves the current supported selection
path. Initial `joint` compares existing S1/two-slot-TMA pairs before tile
commitment; D and `cp.async` remain future extensions. The exact executable
subset, cost model and compile-time fallback are specified in
`AI-COMPILER-OPTIMIZATION-JOINT-GEMM-PLAN-SEARCH.md`. Joint requires `-O3` and
device-WHIRL output and cannot override host-provider capability, precision
permission, target legality, or the fallback. This implementation is not
evidence of novelty or measured speedup.

The `.t` source trace and derived `tile.log`, `fetch-pipeline.log`, and
`optimization-summary.log` must record option mode and budget, target-profile
version, stable plan lineage, legal rejections, per-level traffic assumptions,
bank/resource costs, dynamic barriers, critical-path overlap, cost units and
provenance, selection reason, and the actual WHIRL rewrite. A selected
candidate without a matching rewrite is not a successful optimization.

## 4. Evaluation needed before publication

1. Compare `baseline` and `joint` at equal compilation-time/search budgets.
   Include D, S1, and SN only where each is truly implemented and legal.
2. Test shape families: small/large M/N/K, odd edges, narrow K, tall/skinny,
   and alignment/stride variants. Keep dtype and numerical contract fixed
   for the first strict-F32 SIMT experiment.
3. Use separate analytical/estimated and measured columns. Validate
   requested sectors, L1/L2/HBM bytes, SMEM bank conflicts and throughput,
   registers, CTA residency, barrier counts, and achieved runtime on each
   target. No GPU host is presently available for this experiment.
4. Compare against a fixed sequential staging decision and, where feasible,
   representative TVM/Triton schedules under matched shape/dtype/hardware
   conditions. Do not compare a strict-F32 SIMT path against FP16 tensor-core
   throughput as though they were the same workload.
5. Report search-space size, prune reasons, calibration error, ranking
   error, wins/losses, compilation overhead, and cases where fallback wins.
   A joint mode that only adds compile time or cannot predict the winner
   falsifies the practical value claim.

## 5. Open design questions

- Which common IR fields are stable across CPU, NVIDIA, AMD, and FHE
  workloads, and which belong in a NVIDIA target extension?
- How many A/B layout and stage combinations can be explored without
  making compile-time cost unacceptable for JIT inference?
- Which cache scenarios and uncertainty margin prevent a direct-load
  candidate from being rejected on an optimistic SMEM estimate?
- Can independent A/B stage depths be expressed and lowered safely in
  high-level WHIRL, and can the emitted CUDA/PTX be audited against them?
- What hardware counters and artifact receipts establish that the model's
  chosen plan, rather than downstream compiler recovery, caused the gain?

This file is a rationale and evaluation scaffold. Implementation remains
governed by the main design and milestone plan; publication requires prior
art review, a working option, verified WHIRL lowering, and measured results.
