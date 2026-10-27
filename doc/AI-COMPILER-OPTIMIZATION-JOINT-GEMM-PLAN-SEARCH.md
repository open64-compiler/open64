# Joint GEMM Profitability And Lowering

Date: 2026-10-25. Scope: compiler-side, PU-local, contiguous F32 GEMM.
Owners: VHO candidate/selection policy; backend driver device-image publication;
`whirl2c -cuda` emission. No binary WHIRL schema changes.

2026-10-26 proposal: the
[bubble-model experiment](AI-COMPILER-OPTIMIZATION-BUBBLE-MODEL.md)
will compare the current model-3 winner against a dependency/resource cycle
prediction on the same D/S1/T2 census. Shadow mode preserves actual selection
and existing legality/launch restrictions. No bubble option or cycle selector
is implemented. The model-1/S1-T2 description below is historical; current
D/model-2 and movement-resource/model-3 amendments are in the companion
comparison and movement-resource designs.

## Control And Supported Search Space

`-Wb,-DSL:gemm_plan_search=baseline|joint` is registered. Omission means
`baseline`, preserving tile-first selection and existing TMA request behavior.
`joint` requires `-O3` and `cuda_whirl_output`; invalid values and unsupported
optimization/output combinations diagnose before device-image publication.

Joint mode defers P7 selection, keeps the generated compact, padded-A,
coalesced-thread-map and XOR-B siblings, and enumerates each proven tile with:

- One-slot synchronous cooperative staging, including predicated edges.
- Two-slot TMA, when `cuda_tma_prefetch=ON` permits it. TMA is an alternative,
  not an instruction to force TMA. Exact compact tiles, distance one, the P8
  event-order contract and the conservative 48 KiB static-SMEM gate apply.

The bounded space is at most 16 tile records and 32 tile/movement pairs.
The tile baseline is a semantic traffic comparator, not an implemented direct
GMEM kernel. It is not entered as a lowerable joint pair. Tile-legality
rejections remain in `TILE_CAND`; every pair for each proven nonbaseline tile
appears in `JOINT_CAND`, including rejected TMA siblings. Budget exhaustion
diagnoses; it does not silently prune a potentially better pair.

No direct/cache-served kernel, `cp.async`, new tile dimensions, precision
change, physical GMEM repack or runtime multiversion dispatcher is claimed.
Those remain extensions of the broader white-paper/design space.

## Algorithm And Fallback

```text
generate bounded P7 tile/layout alternatives, without selecting
for each proven nonbaseline tile in stable order:
    construct synchronous and permitted TMA P8 records
    apply shared executable preflight before cost selection
    compute complete comparable costs for both schedules
    retain each pair's identity, rejection and cost in the trace
select minimum (complete cost, stable enumeration ordinal)
retain the minimum legal synchronous pair as compile-time fallback
finalize the winning tile lineage and reconstruct its selected P8 records
copy selected numeric records to the driver-owned handoff
materialize only the selected device PU and verify P8 operation ordering
publish image atomically; emit actual transformation/proof report
```

The fallback remains a reviewed plan in the complete candidate census; if
TMA is unsupported, disabled, or less profitable, synchronous execution can
win directly. This is **compile-time plan fallback**, not a second emitted
kernel or a runtime recovery mechanism. TMA-enabled joint host application
uses synchronous-only enumeration until the TMA host-provider ABI is built.
A successful cost gate never authorizes unsupported runtime launch behavior.

The planner and lowerer call the same executable preflight. Operand alias,
ownership and locality checks also apply to the synchronous joint baseline.
Failure to find a legal, completely costed synchronous pair rejects joint
selection instead of committing a partial transformation.

## Complete Cost Model 1

Historical model 1 is retained below; D/S1/T2 model 3 extends it with independent
movement-resource legality. The default joint selector is now model 4,
[analytical readiness](AI-COMPILER-OPTIMIZATION-ANALYTICAL-READINESS.md), using
explicit assumed delivery latency/service, consumer timing and executable
two-slot schedule boundaries. `gemm_cost_model=relative` retains model 3.
The new model uses reference cycles throughout a census, not a mixture with
these historical relative scores. No simulator is required by compilation.

All amounts are low-confidence **relative units**, not nanoseconds, FLOP/s,
or performance measurements. The old AIO-10 transaction score is not added to
the P7 traffic score. Each complete pair is scored once over the whole GEMM.

Define `G=ceil(M/BM)*ceil(N/BN)` CTAs, `I=ceil(K/BK)` reduction tiles, and
`Q=G*I`. Let `C=max(1,FLOPs/1024)+SMEM_address_work/1024`.
Integer divisions below truncate. All arithmetic is checked for overflow.

| Quantity | Definition |
| --- | --- |
| Raw operand movement `R` | `max(1,(A_sector_bytes+B_sector_bytes)/1024)` for synchronous copies. For legal compact TMA, use whole-GEMM A/B payload bytes with CTA replication, divided by 1024. |
| Hidden movement `H` | Zero synchronously. For two-slot TMA, `min(R/Q,C/Q)*(I-1)*G`. Only the steady-state window earns overlap; the first reduction tile remains exposed. |
| Local work `L` | `bank_conflict_work/32 + A_load_issue_work/32 + B_load_issue_work/32`. Retained conservatively for the staged consumer. |
| Exposed memory `U` | `R-H+C_store_sector_bytes/1024+L`. Output traffic is not hidden by the input pipeline. |
| Storage `S` | One or two physical A/B staging allocations, including layout padding; add 16 bytes for TMA ready barriers. |
| Pressure `P` | `(C+U)*S/shared_capacity_bytes`, a fractional-capacity heuristic. This is not an occupancy, spill or grid-wave prediction. |
| Synchronization `Y` | `2*Q` synchronous barriers; `2*Q+G` for TMA publication/wait/release planning. TMA's extra CTA publication is charged explicitly. |
| Setup `J` | `32*G` for TMA, zero synchronously. This is an explicit provisional coefficient, not a measured CUDA descriptor/launch latency. |
| Total `T` | `(C+P)+U+communication_unhidden+Y+J+runtime_selection`. Communication and runtime selection are zero in this single-device static slice. |

Overlap is not credited from stage count alone. It requires a lowerable
two-slot schedule, is bounded by per-tile compute and movement work, and
preserves startup/drain. Logical barrier and issue counts are not a prediction
of PTX instruction latency. An exact schedule-event critical-path model,
register-file-limited residency, cache hit rates, bandwidth/latency calibration,
tensor-map setup amortization and native GPU measurements remain open.
Roofline records remain diagnostic; their nanoseconds are never added to
relative plan costs or used to override the joint cost minimum.

## Trace And Reports

The existing option-controlled `.t` stream adds positional v7 records:

```text
JOINT_MODE joint budget tile_count tma_permitted relative low model=1
JOINT_CAND ordinal tile_id tile_plan fetch_plan engine legality reason compute memory_unhidden communication_unhidden sync setup runtime total buffer_bytes capacity_bytes source_line
JOINT_SELECT selected_ordinal synchronous_fallback_ordinal count selection_reason=lowest_complete_joint_cost
```

The compiler emits one authoritative `TILE_SELECT` only after whole-plan
selection. Tile-only costs stay inspectable but cannot override that choice.
`FETCH_CAND` records for the winning tile retain their historical per-stage
diagnostics; those are explicitly not the joint profitability score.

The report tool independently recomputes complete cost terms, lowerability,
full pair coverage, deterministic ties, the winner and synchronous fallback.
Missing/corrupt records or contradictions with selected P8/WHIRL evidence fail.

| Report | Required summary |
| --- | --- |
| `tile.log` | Tile census, tile-only cost assumptions, and final joint-selected tile. |
| `fetch-pipeline.log` | Winning tile's engines, stages, diagnostic movement estimates, and actual P8 rewrite/proof. |
| `physical-plan.log` | Complete pair census, per-term whole-plan costs, legal rejection reasons, selection and synchronous fallback identities. |
| `optimization-summary.log` | Mode, model version, pair count, selected tile/engine/total, fallback and actual transformation/proof status. |
| `optimization.log` | Full content of all phase reports in phase order. |

All report files retain their first-line `What it summarizes` convention.

## Validation And Follow-On Work

`test_joint_plan_search.py` retains short/long/irregular-K, TMA-OFF,
layout-search-OFF, synchronous host application, repeat/determinism,
explicit/default-baseline parity, invalid-option and low-optimization tests.
Every successful case retains input/device WHIRL, independent dumps, canonical
CUDA, raw `.t`, derived reports and host-only reference simulation. Negative
audits remove or corrupt the decision/candidate/fallback evidence.

The existing executable-P8 suite still exercises actual two-slot lowering,
including multiple parity wraps, independently of whether joint costing
selects TMA for these fixtures. No NVCC, GPU correctness or speedup acceptance
is implied. The next cost-model revisions must compare native measurements
against these explicit assumptions rather than tune coefficients to force
TMA selection.
