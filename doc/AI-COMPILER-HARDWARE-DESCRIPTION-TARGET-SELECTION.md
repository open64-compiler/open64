# Hardware Description IR And Target Selection

Version: v0.2, 2026-10-26. Status: implementation and review contract.
Scope: immutable runtime descriptions; PU-local VHO clients. No binary WHIRL
layout change, proprietary NVIDIA compiler data, GPU probing or new library.

Occupancy follow-on: [resource planning](AI-COMPILER-OPTIMIZATION-OCCUPANCY.md)
adds runtime-only resource capacities and explicit estimated allocation
granularities. Current registry revision2 preserves performance values while
adding a mixed-provenance resource envelope; joint model6 consumes it.
Revision1/model5 receipts below remain historical, not current producer output.

## Principle

Use familiar NVIDIA `sm_*` option values externally. Resolve each exact value
to our own versioned Hardware Description IR and its table contents. Hardware
latencies, throughput and resource parameters belong to that description,
not private constants in a tiling or movement profitability algorithm.

Hardware description records, lookup and structural validation are
**Common Compiler Substrate**. AI GEMM candidate construction, timing
recurrence, profitability, overrides and transformation are VHO-owned
**AI-domain-specific consumers**. Other compiler domains may reuse the same
description without inheriting GEMM policies.

## Defined Terms

| Term | Meaning |
| --- | --- |
| Architecture key | Exact user spelling such as `sm_90`; suffixes are significant. |
| Family profile | Existing Hopper/Blackwell memory-tier, bank and movement-capability envelope. |
| Hardware Description IR | Runtime record binding an architecture key, family profile, named/revisioned quantitative table, units and provenance. |
| Performance payload | Resource/rate/latency fields consumed by the analytical evaluator. |
| Reference cycle | Unit of the declared estimated model, not a physical GPU cycle or convertible nanosecond duration. |
| Path startup latency | Fixed delivery delay for a particular engine/path, excluding byte-transfer service. |
| Transfer service | Transferred bytes divided by declared delivery rate. Not another startup-latency charge. |
| Explicit override | User-requested replacement in a caller-owned description copy; never a mutation of built-in data. |
| Device/SKU refinement | Future board/partition/clock-specific configuration beyond architecture capability. Not provided by `sm_*` alone. |

## Interface And Registry

```text
-Wb,-DSL:gpu_arch=sm_90
-Wb,-DSL:gemm_plan_search=joint
-Wb,-DSL:gemm_cost_model=readiness
```

Omitting `gpu_arch` preserves the existing `sm_90` CUDA review target.
`DSL_hardware_description_find_arch` performs exact lookup;
`DSL_hardware_description_get_arch` returns a caller-owned validated copy.
No numeric-prefix parsing, suffix stripping or nearest-target fallback.

| Exact option value | Family table | Quantitative table | Current CUDA WHIRL emission |
| --- | --- | --- | --- |
| `sm_90` | Hopper | `sm90_estimated_v2` | Existing generic F32 D/S1/T2 subset |
| `sm_90a` | Hopper | `sm90a_estimated_v2` | Same generic subset; no newly implemented architecture-specific instructions |
| `sm_100` | Blackwell | `sm100_estimated_v2` | Planning description only; emission rejected |
| `sm_100a` | Blackwell | `sm100a_estimated_v2` | Planning description only; emission rejected |

Each value has its own immutable row and identity. Initial estimated payloads
deliberately preserve the former synthetic envelope; identical numbers do
not imply equal real hardware performance. Table selection does not create
lowering support. Unknown names, unsupported suffixes and planning-only
emission requests fail before selected-image publication.

The common registry is in `osprey/common/com/dsl_memory_hierarchy.cxx`, beside
the existing family tables, not in the optimizer. The standalone policy-free
payload is in `dsl_hardware_performance.h`. Both are runtime-only; existing
tier/capability records, enums and mapped-image contracts remain compatible.

## Table Contents And Provenance

| Payload field | Scope and meaning |
| --- | --- |
| `sms` | Device-wide planning-wave width; currently an estimated reduced configuration, not a claimed physical SKU SM count. |
| `warp_lanes` | Lane grouping used to derive candidate warp count. |
| `memory_bytes_per_cycle` | Aggregate modeled delivery service across an active wave. |
| `scalar_ops_per_cycle` | Per-SM scalar F32 arithmetic service, not Tensor Core performance. |
| `local_issues_per_cycle` | Local operand/address/bank-work service denominator. |
| `copy_elements_per_cycle` | Synchronous scalar staging-copy service denominator. |
| `latency_hiding_warps` | Estimated limit on the evaluator's coarse D/S1 inter-warp hiding allowance. |
| `demand_latency` | Global-to-register demand-path startup. |
| `staged_latency` | Synchronous global-to-SMEM operand delivery startup. |
| `tma_latency` | TMA global-to-SMEM startup. |
| `store_latency` | Output-store completion startup. |
| `publication_cycles` | Initial staged-data publication macro cost. |
| `wait_cycles` | Consumer acquisition/readiness wait macro overhead, excluding exposed delivery wait. |
| `release_cycles` | Slot-release synchronization macro overhead. |
| `operand_issue_cycles` | Issue overhead per A/B transfer; the schedule has two operands. |

The row carries architecture, family ID, table name/revision, unit and
provenance. Initial provenance is `estimated`; **these are not NVIDIA latency
specifications, measurements, or calibrated Hopper/Blackwell performance**.
Do not copy timing numbers into an optimizer or imply that selecting an SM
name obtains NVIDIA's internal machine model. `latency_class` remains an
ordinal and is never interpreted as cycles.

Existing family capacity tables remain inherited planning envelopes, not a
complete SKU description. In particular, an `sm_*` key does not identify HBM
capacity, a full device's SM count or a physical clock. Future device
refinement and measured data must carry provenance and applicable conditions;
they must resolve through this service rather than create a second authority.

## Resolution And Consumption

```text
driver option spelling -> exact common description lookup
    -> resolve family tier/bank/movement capability tables
    -> copy description into active-PU planning scope
    -> apply explicitly requested diagnostic overrides
    -> derive legal complete tile-plus-movement candidates
    -> derive consumer work using description rates
    -> choose per-engine startup and output path timing
    -> evaluate readiness recurrence using description rates/times
    -> select complete plan and reconstruct with identical description
    -> transform WHIRL; emitter prints selected semantics
```

No optimization, candidate pruning or selection belongs in common lookup.
The pure evaluator accepts a complete payload and has no target defaults.
Two slots, two operands and the four-variable max-plus state are algorithm
structure, not hardware timing constants. One-CTA/SM is the current model
restriction, not a hardware capability inferred from the table.

`-Wb,-DSL:gemm_memory_latency=N` remains a diagnostic compatibility override.
Its value is used **only when explicitly supplied**, including explicit zero.
It replaces demand/staged/TMA/output startup in the local copy and records
`latency_override=1`. Without the option, every startup comes from the row;
there is no option-owned default delay. Rate divisors must be positive;
zero timing is a valid declared estimate, not an unknown-value sentinel.

For the existing generic F32 subset, selected family/plan semantics still
drive WHIRL and emission. Exact SM/table identity is retained in trace;
before introducing suffix-specific instructions or direct PTX, persist the
necessary target contract in WHIRL using the existing compatibility process.
Do not reconstruct missing target meaning from the emitter command line.

## Trace And Reports

Joint model 5 replaces new production `READINESS_PROFILE assumed_v1` output
with positional records from the resolved Hardware Description IR:

```text
HW_DESC architecture table family revision unit provenance latency_override
HW_PERF sms warp_lanes memory_bytes_per_cycle scalar_ops_per_cycle local_issues_per_cycle copy_elements_per_cycle latency_hiding_warps demand_latency staged_latency tma_latency store_latency publication_cycles wait_cycles release_cycles operand_issue_cycles
```

Keep `JOINT_READINESS` per-candidate records, source lines, independent
resource legality, exact candidate coverage, complete cost/winner and S1
fallback checks. Audit description identity/family, units/provenance, rate
denominators and override consistency before independently recomputing costs.
Unknown/corrupt/duplicate/incomplete hardware records fail closed. Historical
model-4 receipts remain readable through an explicitly legacy-only adapter.

`tile.log` and `fetch-pipeline.log` begin, after their mandatory summary and
scope lines, with description identity and HW_PERF columns/values. State
that legacy tile-only scores are diagnostics before displaying them.
`physical-plan.log` gives complete analytical costs. The summary records the
architecture, table/revision, provenance, override state and selected plan.
Retain immutable prior receipts; changing a report never rewrites history.

## Acceptance And Open Work

Required: lookup/suffix/unknown-name tests; failed lookup preserves outputs;
override isolation and explicit-zero coverage; changed-rate/latency recurrence
oracles; wrong-family rejection; default/explicit `sm_90` parity; generic
`sm_90a` review; Blackwell planning-only emission rejection; trace-off and
baseline controls; independent trace/report replay; before/after WHIRL and
CUDA host-only correctness. No native GPU or performance claim follows.

Open: measured/profile-import provenance per field, physical clock domains,
cache-path conditions and finite queues, SKU refinements, occupancy, Blackwell
lowering certification, and confidence/range-aware selection. Initial table
values are centralized estimates, not a completed hardware characterization.

Public naming references: [NVCC target options](https://docs.nvidia.com/cuda/cuda-compiler-driver-nvcc/)
and [NVIDIA compute capabilities](https://developer.nvidia.com/cuda/gpus).
We independently implement the registry and its data; no proprietary
compiler implementation or timing table is imported.

## Acceptance Evidence

Compiler checkpoint `1f793c9e`: 104 Python tests, 2340 varied-parameter literal
schedule oracles, thirteen compiler/source configurations, explicit zero and
override isolation, default/explicit SM90 parity, SM90a generic F32 review,
unknown/suffix/planning-only rejection, legacy D/S1/T2 and baseline/trace
controls, linked common/VHO adapter contracts and backend symbol checks pass.
Historical model-4 raw trace was authenticated and audited with the new tool.

The default common row preserves T2 #3 for 128x128x144 at 31336 reference
cycles. This refactoring changes target-data ownership, not claimed physical
performance or calibration. Generated CUDA host simulations report zero error.

1281 checksum-verified files are preserved immutably at:

```text
/Users/shinmingliu/open64/artifacts/review/ai-optimization/m8/hardware-description/sha256-b1370aae64cc600d9cb9b2fc6b58dbc357a793be4efe4e9f38542cbc912bc859/
```

Review `acceptance/hardware/staged/reports/phases/tile.log`, `gemm.t`,
`gemm.whirl.cu` and the before/after WHIRL in that case directory. After
promotion, native `ir_b2a -st -src` reopening of the durable `.B` reproduced
the retained `.T` byte-for-byte; independent report replay reproduced the
summary exactly. No prior receipt was changed or removed.

The impact selector's unknown-path full-FHE fallback was reviewed: this is
an additive runtime-description API and AI timing/control change, not a
generic materializer, persisted record or FHE semantic transformation change.
Focused production builds, native adapter contracts, actual compiler/source
cases and legacy compatibility checks cover the changed boundary. No unrelated
full ResNet certification or native GPU certification is claimed.
