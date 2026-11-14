# KV Reuse-Group Planning Design v0.1

November 12 2026 | Adopted design; KVR0-KVR5 implementation gates open

This is the standalone KV reuse-group amendment to the
[AI optimization design](AI_compiler_optimization_design_v0.3.md).
It applies the existing [common execution-planning framework](AI-COMPILER-COMMON-EXECUTION-PLANNING.md)
and [numerical accumulation contracts](AI-COMPILER-NUMERICAL-ACCUMULATION-DESIGN-v0.1.md).
The [implementation queue](AI-COMPILER-OPTIMIZATION-IMPLEMENTATION-PLAN.md#kv-reuse-group-implementation-increment)
defines action, acceptance, reporting and certification gates.

This document preserves the KV strategy previously reviewed in v0.5 section
8.4 without requiring the v0.4/v0.5 snapshots or compiler commits not yet
present in develop.
It introduces no executable support, target timing table, compiler option or
binary WHIRL schema. For this strategy, selected algorithm/tile/staging loops
are materialized in High WHIRL; Mid lowers selected memory operations, Low
realizes target mapping, and CUDA/PTX emission consumes that same selected
WHIRL. Emitters do not choose a group, precision or schedule.

The precision requirement is self-contained: FP8 operands require admitted
source encoding or explicit quantization authorization; FP16 or FP32 reduction
state requires its own numerical and target admission, with FP32 the default
when the accepted source contract does not specify otherwise. Initial
Attention statistics and output accumulation remain FP32. Preserve source
output types and explicit arithmetic, including FP64 where required; never
silently narrow source tensors. Existing supported APIs remain unchanged.

## 1. Terms And Objective

The objective is minimum complete Attention invocation latency, not maximum
reuse, tile size, stage count or occupancy in isolation. Compact KV storage
reduces bytes; temporal reuse requires scheduling multiple legal consumers
before the resident tile is released. A larger sequence tile amortizes transfer
and synchronization overhead but does not by itself increase uses per element.

| Term | Definition |
| --- | --- |
| KV cache | Persistent key/value tensors for prior tokens, normally in device global memory; distinct from a transient SMEM tile. |
| KV tile | A bounded token range of one identified KV head, with separate K/V dimensions, views, masks, representations and scale objects. |
| Query consumer | One logical query-position/query-head output and its independent score, online-Softmax and output state. |
| KV reuse group | A plan-local set of query consumers that use a common KV tile in one proven sharing scope before its storage is overwritten. Membership does not change model head grouping. |
| Reuse scope | Execution participants permitted to access the allocation and join its readers. Initial scope is one CTA; different CTAs do not implicitly share ordinary SMEM. |
| Reuse factor | Useful consumer-element references divided by distinct staged elements, reported separately for K and V with masks/tails and a fixed comparison boundary. Not a claimed HBM speedup. |
| Representation path | HBM encoding -> transferred payload/scales -> resident encoding -> optional converted/packed compute operands, with explicit lineage and conversions. |
| Slot generation | One logical tile's contents in a reusable physical staging slot; allocation identity, slot and generation distinguish it from later contents. |
| Final-reader join | Completion of every access to a storage range, including asynchronous compute/copy and aliases, before a conflicting overwrite. |

Prefill may group query positions with compatible KV ranges, retaining causal
and other masks. GQA/MQA decode may group query heads only when source semantics
map them to the same KV head. Ordinary single-token decode with one query per
KV head offers no extra query-row reuse merely from compression. Unrelated
requests have distinct KV identities: increasing batch size adds parallel work,
not shared-KV reuse. Cross-request prefix sharing is a later guarded case,
requiring identical admitted post-transform KV values, positions, representation,
scales, ownership and immutable range/epoch, not merely equal token strings.

No candidate assumes SMEM persistence across kernel launches or future generated
tokens. L2 residency is a separate conditional opportunity; neither a smaller
working set nor a prefix-cache hit proves an L2 hit. Cross-CTA cluster sharing
requires an independently admitted target protocol and is outside the first slice.

## 2. Precision-Aware Storage And Resource Allocation

Apply the precision policy before profitability: FP8 operands only with source
or quantization authorization; FP32 remains the initial Softmax statistics and
output-accumulation state. No FP8 reduction, implicit narrowing, new GQA semantics
or unapproved reduction reordering follows from consumer grouping. Changing to
online Softmax or a sequence-split merge needs an independently accepted
numerical realization; mathematical equivalence alone is insufficient.

For uniform stage shapes define S as resident slots, Bt as tokens per tile,
Dk/Dv as K/V head dimensions, and bK/bV as resident bytes per element:

```text
payload_bytes = S * Bt * (Dk * bK + Dv * bV)
peak_live_bytes = max_over_legal_event_cuts(sum(distinct live physical ranges))
SMEM_reservation = target_allocation_round(arena_extent + target_overhead)
```

Use checked integer arithmetic and actual layouts. The payload formula excludes
scales, padding, raw/converted buffers, barriers, query storage, score/workspace
and epilogue scratch. Account for each in the allocation/event graph. Arena
extent includes alignment and packing holes; occupancy uses target-rounded
reservation, not peak logical liveness alone. Derive RF requirements separately,
including per-consumer maximum, denominator, output vector, instruction fragments,
conversion/address temporaries and spills. Narrow operands do not automatically
halve physical register allocation.

At Bt=128 and Dk=Dv=128, one compact K/V pair is 64 KiB in FP16/BF16 or 32 KiB
in FP8; two slots are 128 or 64 KiB, respectively, before other storage. These
are arithmetic examples, not admitted target allocations or measured results.

Generate at least these representation alternatives where numerically and
instruction-legally supported:

| Path | Resource/cost obligation |
| --- | --- |
| FP8 HBM -> FP8 SMEM -> admitted FP8 compute | Preserve compact staging; include scales, packing, instruction arithmetic and any partial-sum promotion. |
| FP8 HBM -> FP16/BF16 SMEM -> admitted wider compute | Save transferred payload bytes, but charge expanded staging and conversion; do not claim FP8 SMEM capacity. |
| FP8 resident payload -> bounded wider conversion buffer | Charge coexistence and reader lifetimes of raw, scales and converted tiles; reuse conversion only across consumers for which it is valid. |
| Source-compatible wider baseline | Retain the original admitted formats, arithmetic and output contract as fallback; do not fabricate a fallback with different precision semantics. |

Freed space may support more query consumers, a larger token tile, deeper
prefetch or additional resident CTAs. These are competing complete alternatives,
not additive benefits to credit simultaneously. Recompute lane/address mapping,
issued sectors, vector alignment, SMEM banks/swizzle and instruction layouts for
each encoding; cache compression is not itself a coalescing transformation.

## 3. Consumer Lifetime And P8 Schedule

Each consumer owns independent numerical state. Share K/V and compatible scale
objects, not one Softmax denominator or output accumulator across queries.
The following conceptual schedule assumes an admitted streaming-attention
realization; source order/masks and numerical obligations remain binding:

```text
retain Q[group] and initialize independent accepted state for each consumer
for tile in admitted_KV_order:
    slot, generation = selected_storage_assignment(tile)
    acquire_after_previous_conflicting_readers_complete(slot, generation)
    issue_payload_and_required_scales(tile, slot, generation)
    wait_for_copy_completion_and_required_visibility(slot, generation)
    prepare_selected_compute_representation_and_publish_if_required()
    for consumer in group:                 # may map to concurrent warps
        apply_exact_bounds_and_mask(consumer, tile)
        scores = accepted_QK(consumer, resident_K)
        update_independent_accepted_softmax_state(consumer, scores)
        accumulate_accepted_PV(consumer, resident_V)
    join_all_accesses_to_payload_scales_and_conversion_buffers()
    release_ranges_for_later_generation_overwrite()
drain_outstanding_operations_and_finalize_each_consumer()
```

Prefetch another tile only into a legally free selected slot; the joint plan
defines prologue, steady state, drain and lookahead. TMA ready is not QK/PV
completion. K, V, scale, conversion and barrier ranges can have different last
readers. Release them separately only with a target-supported proof; otherwise
retain the conservative whole-group completion join. Include outstanding async
copies and alias views, uniform barrier participation and finite arrival counts.
Empty/masked consumers cannot skip required collective participation. Zero work,
partial pages, tails and fewer tiles than slots must not issue invalid transfers.
Guard source page/scale identity and lifetime against eviction, update or remap
until all source reads finish. Runtime buffer ownership is an explicit consumer
of the selected plan, not an assumption introduced by the CUDA emitter.

## 4. Joint Profitability And Phase Ownership

Boundedly enumerate query-position/head group size, KV tile dimensions,
representation path, SMEM layout, stage count, movement engine and partition.
Compare group-size-one/small-group baselines, larger reuse groups and legal
sequence-split alternatives. Split plans include partial-state workspace,
launches and the numerically admitted rescale/merge. More reuse can increase
RF/SMEM reservation, reduce CTA residency or total CTA count, add synchronization
and reduce parallelism; low-batch decode may prefer sequence splitting.

For the same admitted source result, build a resource-constrained event schedule:

```text
predicted_latency = makespan(launches + transfers + conversions + QK + Softmax
                            + PV + waits/joins + stores + optional_split_merge)
```

Events have dependency, shared-engine and capacity constraints. Charge scale
traffic, repeated preparation, instruction arithmetic/promotion, SMEM service,
register/local traffic, invocation overhead and full/tail waves. Credit overlap
only for an executable schedule. Do not add a separate overlap discount to a
makespan that already accounts for it. Distinguish logical demand, issued-sector
traffic, L2 service and modeled HBM traffic; repeated CTA loads are not all HBM
misses. Without cache evidence use declared bounds/unknowns, not invented hits.
Compare complete precision-specific Roofline bounds as diagnostics, not as a
replacement for schedule latency. Unknown target rates cannot certify a winner;
reuse the existing uncertainty policy and source-compatible fallback.

| Phase | Required decision/input and ownership |
| --- | --- |
| P0/P1 | AI Attention semantics establish query/KV head mapping, masks, numerical acceptance, immutable source ranges and possible consumer reuse; existing canonical access/alias analysis supplies proofs. |
| P3/P4/P6 | Keep encoding, placement and proposed consumer ownership separate; determine legal sharing scope and provisional physical/storage alternatives. No implicit cross-device/CTA reuse. |
| P7 | Propose reuse-group membership and query/KV tiles together in CommonTilePlanIR/Work Partition; carry alternatives and per-path resource demand. |
| P8 | Jointly refine movement, conversion, access lifetimes, final-reader joins, slots/generations and real overlap. Re-evaluate group/resource feasibility. |
| P9 | Select only a complete legal, lowerable, costed plan against the baseline. High WHIRL materializes selected loops, storage and events; Mid/Low realize them without changing the group or precision. |
| P10/P11 | Guard runtime shapes/head mapping/page and scale epochs/launch resources; record applicability and feedback without claiming unexecuted speedup. |

Common Compiler Substrate requirements are policy-free group membership and
range identities, Work Partition, representation lineage, storage reservations,
access/completion joins, event scopes, cost provenance and guards, carried by
existing CommonTilePlanIR, TensorEvolutionGraph, CommonMemoryResidencyIR,
CommonFetchPlanIR/CommonPipelineIR, CommonParallelizationIR and plan/cost records.
Attention head-sharing meaning, masks, KV page/update epochs, prefix identity,
Softmax state and quantization/numerical permissions are AI-domain-specific.
Owning phases derive/prove/cost/select; common/com neither discovers groups nor
ranks them. Driver-established analysis scope and selected-WHIRL authority remain binding.
Any eventual persisted schema change needs separate compatibility review.

Extend existing option-controlled `.t` records and phase reports through a
reviewed positional schema, not per-row JSON or new mandatory log files. Reports
must identify group/consumer/source-line joins, numerical/representation paths,
useful reuse versus actual traffic, peak live versus reserved storage, final
reader events, residency/waves, complete costs, unknowns, selected reason and
actual WHIRL actions. Every log retains its first-line "What it summarizes"
contract. [KVR0-KVR5](AI-COMPILER-OPTIMIZATION-IMPLEMENTATION-PLAN.md#kv-reuse-group-implementation-increment)
define the phase-specific report and acceptance gates.

Research inspiration: [FlashAttention-2](https://crfm.stanford.edu/2023/07/17/flash2.html)
motivates query partitioning over shared KV;
[Flash-Decoding](https://crfm.stanford.edu/2023/10/12/flashdecoding.html)
motivates the competing sequence-parallel plan;
[FlashAttention-3](https://tridao.me/blog/2024/flash3/)
motivates explicit low-precision and asynchronous schedules. The reusable
plan-level group contract, ownership map, KVR queue and delayed complete-plan
selection above are DSC design proposals. These sources do not establish Open64
implementation, H20-specific rates or performance; native certification remains
separate and the deferred PERF framework is not resumed.
