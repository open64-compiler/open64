# H3 movement and reuse-aware costing v1.0

2026-11-12. H3 consumes the [H2 owned facts](AI-COMPILER-H2-OWNED-MOVEMENT-FACTS-v1.0.md)
and immutable H1 H20 profiles. It adds a separately versioned physical-unit
model, finite request-wide estimate state and owning-family adapters. Existing
WHIRL opcode/tensor-trait semantics, numerical contracts, COMMENT generation,
selected-only transformation and compatible fallback remain authoritative.
This is a conditional analytical model, not GPU execution or calibration.

## 1. Main choice: physical units and explicit missing inputs

The existing GEMM, MLA and LN models use reference cycles. H20 import provides
physical rates and dependent latencies in seconds/nanoseconds. H3 keeps those
models readable and computes a separate estimate in **nanoseconds**; it never
adds ns to cycles or converts a scalar GEMM rate into Tensor Core throughput.
Existing scalar F32/SIMT recipes are the admitted scope. Unsupported compute
mechanisms require their own typed work/rate adapter and capability gate.

H1 physical rates do not specify every instruction/event/queue input. An
explicit, digest-pinned H3 supplement supplies issue service, CTA barrier,
shuffle, warp fence, LN inverse-standard-deviation latency, outstanding fetch
capacity, cache assumptions and a spill-request scenario. Unknown required
input refuses H3 selection. An explicit zero spill estimate is a **model
assumption**, not inferred from register fit; actual source spills remain
unknown. Nonzero modeled spill requests are paid in requests/L2/HBM service.

All estimates are labeled estimated. Synthetic resources or rates force a
synthetic-input label even if another input calls itself measured. Declared
measured inputs do not establish measured kernel latency, a confidence interval
for real hardware, correctness on the GPU or a speedup.

## 2. Import and option contract

The backend driver imports once, after H1 device/profile admission and before
native input or phase activity. Both options are required together:

```sh
openpy -O3 -Wb,-DSL:gpu_device=h20_96gb \
  -Wb,-DSL:gpu_profile=/path/h20.json \
  -Wb,-DSL:gpu_profile_sha256=GPU_PROFILE_SHA256 \
  -Wb,-DSL:gpu_profile_toolchain=EXPECTED_TOOLCHAIN \
  -Wb,-DSL:h3_model=/path/movement-model.json \
  -Wb,-DSL:h3_model_sha256=MOVEMENT_MODEL_SHA256 model.py
```

Use `h20_141gb` and its separately bound profile for that variant. A model
supplement is at most 64 KiB and has exactly these fields:

| Field | Meaning |
| --- | --- |
| schema | `open64.movement-cost-model.v1` |
| id | Bounded printable model identifier |
| device_binding | Exact H1 `DSL_GPU_PLAN_SNAPSHOT::Cache_key()`; includes device, partition, resources, execution/conditions/toolchain and workload budget |
| provenance | `synthetic`, `estimated` or `measured-input` |
| objective | `worst`, `cold`, `warm`, `capacity` or `contention` |
| transaction_bytes | Explicit aggregate transaction envelope quantum: 32, 64 or 128 bytes |
| queue_per_sm | Positive finite outstanding complete fetch groups per SM, not sectors or warps |
| warm_hit_ppm | Conditional read-hit assumption, 0–1,000,000; requires H2 immutable layout/alias/access/lifetime proof and fitting footprint |
| contention_ppm | Declared service slowdown scenario, 1,000,000–8,000,000; never inferred from counters or added as a measured interference fact |
| issue_per_sm | Positive issued-operation equivalents/second/SM under the v1 adapter; does not mean Tensor Core FLOP/s |
| barrier_ns, shuffle_ns, fence_ns | Positive latency/service inputs for the explicit v1 collective events |
| rsqrt_ns | Positive latency for the LN-REF-1 inverse-standard-deviation operation under the admitted route |
| spill_request_bytes_per_cta | Explicit nonnegative request estimate over one CTA; null/missing is incomplete, not zero |
| site_limit | 1–128 new estimates per owned H2 fact identity |
| request_limit | 1–1,024 new estimates across the complete driver request and all strategy runs/PUs |
| memory_limit | 4 KiB–64 MiB conservative detached storage charge; insufficient limits refuse exploration |
| deadline_us | Positive cooperative host deadline, at most one hour, starting at request configuration |

Duplicate/unknown/missing fields, coercions, unsupported versions, wrong pins
and wrong bindings refuse before native input. The full H1 context binds clock,
power, ECC, cache-regime and workload conditions. A different condition/profile
requires a new matching supplement and fresh estimates. The supplement is the
external calibration **input/output format**; H3 performs no calibration run.
Current tests produce explicitly synthetic examples, not real H20 constants.
Hardware-side collection and qualified calibration remain H0/H7 work.

No supplement preserves the existing architecture-only and imported-H1 paths.
Explicit H3 requests require an imported profile. Import validation can occur
at O0, but H3 optimization/fact consumption remains O3 and PU-local.

## 3. Owned recipes and byte accounting

The common engine accepts only detached H2 facts plus a phase-owned work
recipe. A plan binds live allocation bytes, capability, executable status and
all stage counts. Unknown live storage is not zero. Each stage describes CTA
count, threads, register/shared reservation assumptions, fetch steps/depth,
request bins, shared work, SIMT/issue counts, dependent chains, special
operations and collectives. Full framed model/recipe content supplements
compact hashes; cached facts are compared by complete H2 content.

Cache-eligible read requests and conservative write-through requests are
separate bins. The latter may include unclassified reads from an older aggregate
recipe: they receive no hit credit and are not mislabeled as source writes.
The source H2 useful/read/write ledger remains untouched. Transaction rounding
is an explicit aggregate per-CTA/per-bin model, **not an affine coalescing
proof**. Existing padding envelopes remain request estimates, not instructions
to read inactive elements.

Cold, warm, capacity-exceeding and contention ledgers retain requested bytes,
modeled transactions, L2/HBM/shared bytes and modeled spill requests. All modeled
transactions reach L2; L1 is conservatively omitted. Cold/capacity have no hit
credit. Warm credit applies only to proved immutable reuse and is removed when
its footprint exceeds the conservative imported L2 capacity. Source tensor
names, matching shapes or the existence of a cache trait alone grant no credit.
Contention is a declared scenario; matching measured interference is not
manufactured by multiplying an already observed result.

## 4. Physical service and finite execution

For a stage, let S be visible SMs, R admitted resident CTAs/SM and C its CTAs.
Use A=min(S,C), P=ceil(C/A), W=ceil(C/(S·R)). Register/shared allocation
rounding and resource/budget admission reuse H1, independently of old scenario
occupancy. Larger depth/group/tile can reduce R and lose despite fewer requests.

Global HBM and L2 service use aggregate device bytes divided by one device
rate. They are never multiplied by S or R. Per-SM compute, issue, shared and
dequantization service use P CTAs of work, including finite coverage/tails;
more residents do not multiply a physical SM's throughput.

For queue capacity Q and declared depth D, effective outstanding groups are
bounded by `min(A·Q, min(C,S·R)·D)`. Memory readiness rounds are the maximum of
global queue rounds and `W·ceil(steps/D)`. Transfer time is the maximum of
serial HBM+L2 service and dependent fetch-readiness time. This joins service
and latency once rather than adding a second memory-startup charge.

Compute/issue time is the maximum of SIMT throughput, issue throughput and
ordered add/multiply chain constraints. Shared service, ordered special work,
collectives and launch/setup are separately paid. Stages serialize without
speculative compute/TMA overlap credit. CTA barriers follow the actual existing
GEMM D/S1/T2 recipe: direct has none, S1 has its per-step barriers, and T2 its
initial publication plus per-step release. Async setup is separate from fetch
readiness. Full/tail partitions of one MLA kernel pay one launch, not one launch
per partition. Startup, steady work, release/drain and finite queues are thus
visible constraints, not an arbitrary percentage discount.

The six reported upper components—transfer, compute/issue, local, special,
collective, launch—sum to the reported upper total. Lower/upper values describe
rate/latency corners **within this declared model**, not a rigorous real-GPU
latency bound. Ranking uses the ceiling of the chosen scenario upper ns, or
the worst of the four declared scenarios. Deterministic ordinal ties remain.

## 5. Actual family integration

| Owner | H3 integration and executable boundary |
| --- | --- |
| GEMM VHO | Reuses existing admitted tile/fetch and readiness **count** projections; discards their durations for H3. Joint D/S1/T2 comparisons use H2 storage/reuse and H20 resources. Complete physical comparison selects one existing pair; incomplete/budget cases keep the existing qualified planner/fallback. Fixed pair reconstruction and fresh H2/device validation precede emission. |
| MLA LNO/VHO | Reuses existing M3 portfolio/proposers/catalog and MR2 work/traffic counts, with separate H3 ranks. G1 remains the executable family. G2/G4/G8 can receive physical estimates from the modeled MR2 catalog but remain analysis-only. Exact tail partitions and shared/collective costs remain visible. Only the selected G1 enters the existing atomic High transaction. |
| LayerNorm WOPT | Reuses fresh LN2 source/range/event admission and existing centered ordered graph counts. H3 records are separate from old `costs[]` cycle fields. Complete comparisons require a robust candidate upper advantage over baseline lower before replacing C0. Incomplete physical comparison retains C0 or the existing provider refusal. |
| Reduction WOPT | Fresh R2 direct/staged H2 capture feeds a two-recipe physical comparison, with checked row/chunk/stride/tail/shared requests and ordered accumulator work. Nominal choice remains analysis-only under the R2 contract; this does not add the missing general H6 executable staged materializer. Existing direct guard/fallback remains. |

All four projections reuse their existing semantic/numerical admission. General
unsupported canonical High routes remain the original open gate. Imported
quantized Q4/Q5 search adaptation still requires its explicit serving/complete
plan adapter; H3 does not bypass that refusal with a Q3 inventory or a selected
Q6 staging trace. Q3 generations/publication and H1d runtime authority remain
unchanged. AOT selection remains deferred.

## 6. Finite search and application

One driver-owned `DSL_H3_SEARCH` spans the compilation request. Strategies do
not reset site/request budgets. The existing M3 finite portfolios remain the
proposal mechanism; H3 adds a shared compatible estimate cache and global
accounting for other admitted family consumers. Each lookup consumes a proposal;
new estimates (including incomplete ones) consume unique budget once. Proposals
are additionally bounded by 16×request_limit; repeated refusals leave the
admitted proposal census saturated at that cap. Cache hits retain unique budget
but cannot evade the deadline/proposal limit.

The memory charge conservatively covers copied H2 strings/records, full model
and recipe bindings, cached scenarios and container overhead. Eight MiB is
reserved for the existing bounded catalog/transient work. LN graph expansion
is further limited before construction by a conservative 1,024-byte/job charge
within that region and its existing job cap. This bounds H3 detached planning
storage, not compiler/native-analysis RSS. Legacy fallback retains its own
existing finite controls.

Deadlines are checked before and after atomic estimates; they cannot preempt a
single estimate. A fully completed estimate at the deadline can remain an
incumbent. A later proposal stops. MLA retains the best fully qualified explored
incumbent or ordinary LNO. GEMM/LN require complete comparison as described above.
Cycle target, relative-gap and economic-conversion stopping are refused for
H3 rather than silently reusing cycle thresholds as ns. Exhaustion, budgets,
seeded strategies and zero-threshold empirical plateau remain available.

Capture/admit source outside exploration. Candidate evaluation uses owned
records only: no WN/ST/TY, native transformation, compiler subprocess, device
probe, reference execution, timing benchmark or profiler. The monotonic host
clock measures compiler deadline accounting only. Before selected application,
recapture actual source/proof/epoch/profile; re-estimate just the fixed MLA
winner and compare its complete model/recipe binding and physical score. Existing
rollback, invalidation/rebuild, High exit and structured-memory/GPU/PTX bridge
remain in their owners. The selected High COMMENT records the H3 supplement
identity and estimate status; the PREOPT COMMENT workaround is preserved.

## 7. Qualification, risks and transfer

Tests use the production C++98 engine/importer with fatal ASan/UBSan, handwritten
traffic/ns oracles, component reconciliation, capacity/queue/depth/residency
counterexamples, spill payment, one-launch partition accounting, full key
changes, unknown/overflow/stale inputs and exact finite budget exhaustion.
Native tests cover both H20 variants, read-only WOPT B/T pairs, GEMM/MLA selected
before/after/full diffs and actual WHIRL-derived host reference execution.
Default/no-model, O-level, missing-profile-component, malformed pin and budget
fallback remain separate checks. Existing short/long references and Q3/Q4
regressions execute outside exploration. Shared dependency and symbol closure
protect the backend/frontend boundary.

Main risks are bad calibration/provenance, aggregate transaction assumptions,
unknown real spills or compiler register allocation, insufficient cache/queue
modeling, overly large per-item work graphs and unsupported materializers.
Pinned complete inputs, explicit assumptions, bounded work, separate capability,
fresh selected validation and fallback limit these risks; H7 hardware execution,
race checking, separate timing/profiling and calibration are still required.
There is no global-optimum or universal-speedup claim.

Primary owner, successor, independent reviewer and approving lead remain TBD;
formal transfer acceptance remains blocked on those roles. Immutable evidence
belongs under `artifacts/review/ai-optimization/research/h3-movement-cost/`, with
exact source/producer/input hashes, commands, genuine B/T/raw diffs and final
read-only reopen. Earlier documents and receipts are preserved.
