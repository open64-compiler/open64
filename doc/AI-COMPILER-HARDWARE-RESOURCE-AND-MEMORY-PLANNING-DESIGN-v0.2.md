# Hardware resource and memory planning v0.2: H20 and four operator families

Date: 2026-11-12. Status: proposed next design phase; no H20 measurement or new
compiler implementation is claimed. Baseline inspected: review-series commit
08d91bf8a590725fa682f34156dfc87e44d9b6c4. Publication and per-PR acceptance are
tracked separately in [the review index](AI-COMPILER-REVIEW-SERIES-v1.0.md).
Primary owner, successor, independent reviewer and approving lead remain TBD.

This is a versioned extension of [resource/memory design v0.1](AI-COMPILER-HARDWARE-RESOURCE-AND-MEMORY-PLANNING-DESIGN-v0.1.md),
[common execution planning](AI-COMPILER-COMMON-EXECUTION-PLANNING.md), and
[optimization design v0.8](AI_compiler_optimization_design_v0.8.md).
Their compatibility and numerical contracts remain in force. The extension
covers GEMM, MLA, LayerNorm and Reduction. Its delivery queue is the
[H20 implementation plan](AI-COMPILER-H20-MEMORY-HIERARCHY-IMPLEMENTATION-PLAN-v1.0.md).
Earlier versions remain intact; this document incorporates them by reference.

## 1. Main design choice

**Select a complete, workload-specific data-supply and compute schedule using a
measured device profile.** Jointly choose work ownership, reuse scope, layout,
tile sizes, transfer mechanism, buffering, compute mapping and resource
reservation before changing WHIRL. Optimize complete-call latency and the
declared serving objective. Physical bandwidth utilization is diagnostic.

| Choice | Decision | Consequence |
| --- | --- | --- |
| Target identity | Separate exact ISA capability, device/partition resources, and execution calibration | H20 is a device profile, never an invented sm_20 architecture or an H100 timing alias |
| Shared framework | Reuse Work Partition, Resource Lifetime and Execution Events across all four families | Share traffic/resource/event machinery; retain operation-specific mathematics and legality |
| Memory hierarchy | Keep direct/cache-served, register-retained and staged alternatives until complete costing | Shared memory and TMA are eligible mechanisms, not mandatory placement |
| Reuse versus coverage | Jointly search tile/head grouping, stage depth and independent CTA coverage | A larger reuse group can lose because it reduces parallelism or increases spills/merges |
| Numerical behavior | Source order, rounding, accumulator type, quantization and permitted reformulations are hard constraints | A fast tree, Welford, online softmax or Tensor Core path requires its own permission and proof |
| Search/application | Read-only estimation, then one selected-plan transaction and qualification | Candidate exploration allocates no WN/ST/TY and runs no compiler, device probe or benchmark |
| Feedback | Calibrate externally and import immutable evidence into later compilations | AOT remains the baseline; the deferred AOT selection policy is not implemented here |

### 1.1 Why H20 changes the balance

NVIDIA lists H20 as Hopper/compute capability 9.0 in its
[MIG product table](https://docs.nvidia.com/datacenter/tesla/mig-user-guide/supported-gpus.html).
Its [NIM supported configurations](https://docs.nvidia.com/nim/large-language-models/1.14.0/models.html)
include both H20 96GB and H20 141GB. The user confirmed **H20 96 GB** for
this workstream on 2026-11-12. Live partition, available SMs, usable bytes,
clocks and topology have not yet been collected; nominal capacity is not a
substitute for the visible resource envelope.
No nominal bandwidth, SM count, Tensor Core rate or H200 specification is
adopted as a measured H20 fact.

For a fixed numerical path, the roofline crossover is I_star = P_eff / B_eff.
Increasing bandwidth relative to compute lowers that crossover: more workloads
can reach their compute limit. It does not make every workload more
memory-bound. Small launches, ordered reductions, dequantization, special
functions, SMEM service and register pressure can become the limiting resource.

Illustration only: if one comparison supplies 4.0 TB/s and another 3.35 TB/s,
the equal-byte, purely HBM-bound ratio is 4.0/3.35 = 1.194. These are assumed
inputs, not a server measurement or a cross-GPU speedup claim. Eliminating
redundant traffic can offer a different gain; a compute-limited path can offer
little benefit from the bandwidth difference. Use like-for-like dense precision
and useful operation counts; sparse marketing rates are a separate contract.

## 2. Starting point: what the inspected code supports

| Area | Inspected evidence | Required next step |
| --- | --- | --- |
| Hardware descriptions | common/targ_info/access/dsl_memory_hierarchy.cxx has explicit estimated rows, including an sms value of 4 in its synthetic envelope | Import real device/partition resources and calibrated service data through a separately versioned extension; preserve historical rows |
| GEMM | Bounded D/S1/T2 and MMA5-H realizations exist; MMA5-H timing is unknown, and its two-slot batch schedule does not overlap delivery with compute across batches | Qualify the existing products, then implement a genuine steady-state pipeline and persistent accumulator lifetime as separate selected recipes |
| FP32 MLA reuse | tensor_attention_movement.inc advertises G1 as verified-v8 and G2/G4/G8 as analysis-only | Implement a grouped-head materializer before grouped estimates can win |
| Quantized MLA | Q6c emits byte-addressed bulk async copies; it has serial prepare/finalize stages, three source-ordered passes and few head-owner lanes | Establish device correctness first; parallelize independent work and qualify any changed arithmetic separately |
| LayerNorm | LN3/LN4 supply analytical costs, selected device WHIRL and guarded execution contracts | Measure actual resource/spill/carrier behavior; preserve centered ordered statistics and exact finalization |
| Reduction | R2 direct/staged numerical, storage and runtime guard bindings exist; the direct-binding document leaves selected executable lowering open | Build and certify executable realizations without treating contract binding as a finished fast kernel |
| GPU testing | Combined GEMM/MLA runner, worker/API, result validator and isolated profiling exist | Reuse their job/result protocol; add LayerNorm/Reduction as explicit suites with their own reference contracts |

These are bounded source observations, not claims that all review slices are
merged, all operators have production routing, or existing kernels have passed
on the newly found servers. Current names containing pipeline or tma do not
establish measured overlap.

Controlling evidence:
[MMA5-H](AI-COMPILER-MMA5-H-HOPPER-PIPELINE.md),
[MR2](AI-COMPILER-MLA-MR2-MOVEMENT-COSTING-v1.0.md),
[Q6c](AI-COMPILER-MLA-Q6C-GPU-LOWERING-v1.0.md),
[LN3](AI-COMPILER-LAYERNORM-PROFITABILITY-v0.1.md), and
[R2 direct binding](AI-COMPILER-REDUCTION-DIRECT-BINDING-v0.1.md).

## 3. Hardware and calibration contract

### 3.1 Three identities

1. **ISA contract:** exact architecture suffix, instruction/format capabilities,
   PTX/toolkit requirements and numerical behavior. A mechanism needs both a
   supported lowering and a successful device qualification. Compute capability
   9.0 alone does not authorize an sm_90a WGMMA recipe.
2. **Resource envelope:** discovered model/variant, active device or MIG
   partition, visible SM count, registers, shared-memory limits and carveouts,
   L2, launch/cluster limits, available memory and device locality.
3. **Execution profile:** measured or explicitly estimated service, startup,
   dependent latency, issue rate, queue/concurrency behavior and interaction
   data under a recorded clock/power/ECC/cache/toolchain environment.

The driver resolves these once, validates compatibility and supplies immutable
copies to phase-owned clients. common/targ_info owns policy-free descriptions,
lookup and structural validation. Probing, clock observation and calibration
run in an external qualification process. be.so gains no CUDA/NVML dependency.
Per-field unknown remains unknown; absence is not zero or infinite capacity.

### 3.2 Proposed profile contents

| Group | Required contents |
| --- | --- |
| Identity/provenance | Schema and model revision, device/partition class, observation time, producer and input digests, toolchain, units, confidence/interval and known/unknown status per field |
| HBM/L2/load-store | Read/write/mixed sustained service by size and concurrency, sector amplification, cache regime, demand latency, load/store issue bottlenecks and finite request saturation |
| SMEM/registers | Actual default/opt-in capacity, bank/layout behavior, register allocation granularity, vector service, spills/local-memory traffic and reservation limits |
| Compute | Separate SIMT, admitted MMA precision/shape, exp/div/sqrt, conversions, unpacking and dequantization; service and dependent latency are distinct |
| Transfer/pipeline | Cooperative/vector copy, byte-bulk async, optional tensor-map setup and transfer, barriers/waits/proxy ordering, startup/drain and transfer/compute interference |
| Invocation/topology | Launch and event cost, graph replay only if separately admitted, host/device and peer paths, NUMA/PCIe/NVLink topology and buffer placement |

Every rate records its numerator, scope and calibration boundary. Pair physical
bytes/second with physical service bytes, or use a matching useful-byte model;
do not charge transaction amplification twice through both inflated traffic
and an already penalized useful-byte rate. Separate fitted startup from byte
service before combining them. A measured end-to-end transfer latency cannot
also receive its own byte-service term a second time.

Use bytes, operations and physical nanoseconds or bytes/second in a new measured
schema; do not reinterpret existing reference_cycles. A legacy adapter must
declare a clock domain and conversion with applicable conditions, or refuse.
Profile imports bind the measured conditions and model version. Code
compatibility keys and performance-evidence keys remain distinct: UUID belongs
in evidence/binding, not semantic tensor type equivalence.

The schema and any future profile-import option are proposed, not registered
by this design. Existing exact sm_* lookup and diagnostic overrides keep their
meaning. A fabricated higher bandwidth cannot certify an unavailable lowering.

## 4. Common traffic and schedule model

### 4.1 Separate byte ledgers

For each operand, output, scale, index/page map, spill and intermediate, record:

- Unique useful payload and valid-domain bytes.
- Requested accesses, including repeated passes and partial tuple traffic.
- Projected issued transactions with active masks, alignment and tails.
- Scenario-dependent L2 and HBM service, plus SMEM traffic and local spills.
- Transfer preparation, address generation, synchronization and compute work.

A global-memory address space does not imply an HBM transaction. Cache warmth
is an explicit scenario; an L2 hint or measured hit ratio grants no ownership,
lifetime or guaranteed residency. Reserve padded capacity for all static
participants while charging useful work only for valid elements.

Necessary service bounds for candidate j include:

~~~text
T_floor(j) >= max(F(j)/P_eff, Q_HBM(j)/B_HBM, Q_L2(j)/B_L2,
                 Q_SMEM(j)/B_SMEM, memory_issue(j)/R_issue,
                 longest_required_dependency_chain(j))
~~~

This is a lower bound, not an overlap prediction. The nominal evaluator
schedules the operation's event DAG on shared, finite engine resources,
including preparation, dequantization, epilogue, launch, startup/drain and
waves. Charge all CTAs against one device HBM service; never multiply peak
bandwidth by CTA count. Do not add an independent memory-wait penalty already
present on the critical path. Co-running compute/copy measurements bound
interference; arbitrary perfect overlap is not a default.

### 4.2 Resource and pipeline choice

For candidate depth D and layout:

~~~text
S_CTA = raw_slots + decoded_slots + operands + epilogue + barriers + padding
resident_CTAs <= min(floor(threads_per_SM/threads_per_CTA),
                     floor(warps_per_SM/warps_per_CTA), CTA_limit,
                     floor(RF_capacity/rounded_RF_reservation),
                     floor(SMEM_capacity/rounded_SMEM_reservation))
~~~

All S_CTA terms are byte extents, not slot counts. Apply a capacity quotient
only for nonzero reservation; unknown is not zero-use/unlimited.
Apply both per-block and per-SM limits, opt-in/carveout rules, register
allocation granularity and barrier/cluster limits. Occupancy is a feasibility
and service input, not an automatic speed multiplier.

The rough prefetch-distance condition (D-1)*T_consumer >= L_ready motivates
candidates; event/resource simulation decides feasibility and cost. Higher
bandwidth can shorten the byte-service part of L_ready and reduce useful
buffering depth. More stages may add no overlap while reducing residency.
Little's law, outstanding_bytes approximately B*L, is a device-wide diagnostic
bounded by actual queues and participants, not an allocation request.

Distinguish raw-slot depth from decoded-slot depth. Q6's two raw slots and
single decoded tile can overlap next raw transfer with current consumption;
overlapping next dequantization with current reads needs another decoded
generation or a proved register-consumption boundary. Application threads
perform the Q6 path's dequantization; its byte-bulk copy performs transport.

The selected protocol distinguishes acquire, transfer issue, raw readiness,
decode/publication, compute, all-reader completion and slot reuse. Exact
barrier transaction bytes count the asynchronous operations they join; logical
payload and synchronous fragments are separate accounting. Predicted ready
times never authorize a reuse or replace an explicit visibility/completion edge.

### 4.3 Joint reuse/coverage example

For batch B, H heads, context L, latent width C, Q query rows, and admitted
within-CTA tiles of G heads and U query rows (default U=1):

~~~text
head_groups = ceil(H/G)
query_groups = ceil(Q/U)
latent_requested ~= element_bytes * B * L * C * head_groups
                    * query_groups * sweeps
partial_CTAs = B * query_groups * head_groups * ceil(L/chunk_tokens)
partial_bytes ~= tuple_bytes_per_head * B * Q * H
                 * ceil(L/chunk_tokens)
~~~

Add rotary keys, queries, masks, scales, page mappings, output projections and
merge reads/writes explicitly. The partial-buffer equation applies to the
admitted split family, not every MLA recipe. U>1 reuse requires an implemented
query-tiled schedule; it is not credited to the current per-query Q6 mapping.
Q6's present non-split mapping is a separate family with its own ordered
three-pass work.

G=4 can reduce logical latent requests by about four versus G=1 under the same
sweep/layout, while producing fewer independent CTAs. HBM reduction can be
smaller or absent when G=1 already benefits from L2 reuse. Co-search G, chunk
size, query tiling, retention and resource layout; never multiply a nominal
head-reuse factor directly into a speedup.

## 5. Operator-specific choices

| Family/workload | First choices to compare | Primary limit to check |
| --- | --- | --- |
| Large dense GEMM | Joint output/K tile, register accumulator lifetime, coalesced operands, cooperative versus qualified async pipeline, epilogue reuse | Compute balance, operand service, RF/SMEM pressure and prologue/drain |
| Skinny/batched GEMM | More independent output tiles, smaller tiles, grouped/persistent work if legal, split-K only with numerical permission and paid merge | Wave underfill, launch/merge overhead and extra operand traffic |
| MLA with small query count | Fused score/softmax/latent accumulation when permitted; within-CTA cross-head cache reuse; context splitting when coverage needs it | Cache service, exp/reduction latency, head-group coverage and tuple merge |
| MLA with many queries | Query/head/token tiles reusing cache across query rows; admitted matrix compute paths | Arithmetic intensity and compute rate, masks, temporary state and capacity |
| LayerNorm | Vector/cooperative loads, row reuse, register/SMEM-retained centered values, fused pointwise epilogue where legal | Statistics dependency chain, special functions, spills and row coverage |
| Reduction | Coalesced streaming, local accumulation, warp/CTA trees or split reductions only when permitted; fuse producers where legal | Memory service versus ordered recurrence, synchronization and partial traffic |

### 5.1 GEMM

Retain direct and one-slot synchronous baselines. Qualify the bounded MMA5-H
path on the exact device, then add persistent accumulators across K and an
issue-next/compute-current steady state as new recipes with explicit lifetime
proofs. Tensor-map copies, producer/consumer specialization and persistent
output scheduling are distinct candidates with paid setup and reservation.
SIMT remains the strict F32 fallback; FP16/BF16, TF32, FP8 and sparse execution
require their respective numerical and format contracts.

Large tiles can improve reuse while losing output parallelism. Split-K adds a
merge and changes accumulation order. Epilogue fusion must account for its
live operands and output traffic. TMA is most attractive when repeated,
regular transfers amortize setup; short K or irregular tails may favor direct
or cooperative delivery. The [CUTLASS GEMM discussion](https://docs.nvidia.com/cutlass/latest/media/docs/cpp/efficient_gemm.html)
supports treating tile size, reuse, parallelism and scheduling jointly.

### 5.2 MLA: two existing semantic paths

The FP32 v8 online-softmax/split family already requires reassociation
permission. MR2's G>1 estimates do not qualify a materializer. Build the
within-CTA grouped implementation with per-head stable tuples and exact
shared-slot consumer joins before enabling its capability bit.

The quantized Q6 path preserves its source F32 order. First audit whole-cache
and scale result copies, independent preparation/finalization work, and
redundant per-lane run construction. Eliminate a copy only with a source
alias/effect/lifetime proof and a compatible Q3 publication contract. An
unchanged result view, copy-on-write update or in-place append is not admitted
by merely declaring the KV trait: old versions and external readers must
remain valid. Current disjoint-buffer guards cannot be silently relaxed; an
in-place provider needs a separately versioned serving contract. Then
parallelize remaining independent work and compare bulk, cooperative and
direct encoded loads. Then evaluate additional raw or
decoded slots, head grouping and smaller column layouts. The old 48 KiB
envelope and the R512/rotary64 decoded-tile refusal must become explicit
resource tests; relaxing a number is not an opt-in/resource proof.

Online softmax, warp/token splitting, reassociated dot products or approximate
math on that Q6 source need a separately authorized numerical alternative.
For long contexts, compare one-sweep retention against repeated tiles and
split/merge overhead. For short contexts, prioritize launch count and useful
work per CTA. Quantization's byte saving must exceed unpack/scale/dequant,
extra SMEM and synchronization cost; faster HBM can reduce that saving's
value. An optional dequantized cache mirror must preserve the original
quantization semantics and Q3 generations and pay construction/lifetime cost.

Query count and context length are separate axes. Query count 1 does not
prove serving decode; speculative decode can have multiple queries. Retain
Q2's unknown phase and explicit serving evidence. This design does not choose
the deferred AOT prefill/decode selection mechanism.

### 5.3 LayerNorm and Reduction

Maintain strict-order and reassociation-permitted lanes as distinct numerical
contracts. Source-order-unresolved Reduction v1 is not silently promoted to
ordered v2. A warp tree, multi-CTA split, atomics, Welford, sum-of-squares or
reciprocal/rsqrt substitution must satisfy its own permission and oracle.

For strict paths, improve independent row ownership, coalesced cooperative
transport, retention, address generation and redundant round trips while
preserving accumulation order. Stage a row tile only if the transport/reuse
benefit exceeds synchronization and reservation cost. For permitted tree
paths, select items/thread, warp/CTA shape and partial ownership from complete
cost, with deterministic merge rules where required.

LayerNorm compares direct rereads, centered-value retention and epilogue
fusion; large rows can make retention spill or reduce row coverage. Inspect
actual scalar-carrier/local-memory code and generated register use before
crediting all logical temporaries as registers. A new register-operand
arithmetic recipe needs instruction-conformance checks, not an assumed
equivalence to the current pointer-carrier implementation.

## 6. Changes to the AI optimization phases

| Existing phase | H20 design update | Owning scope and output |
| --- | --- | --- |
| AI-P0 semantic analysis | Preserve numerical rules, tensor roles, query/context geometry and provenance of inferred KV traits | VHO/WOPT/LNO within driver-selected PU or REGION; verified semantic facts |
| AI-P1 lifetime/locality | Derive owned ranges, alias, access/version lifetimes, reuse multiplicity and Q3 publication joins | Phase-owned analysis; independent value/storage/reservation facts |
| AI-P2 semantic alternatives/fusion | Enumerate verified fused, retained and permitted recurrence alternatives | Domain owner retains original algorithm and explicit permission |
| AI-P3 logical layouts | Keep original storage representation; add costed packed/transposed/page-aware views | Logical layout alternatives with construction and semantic obligations |
| AI-P4 placement | First qualify one device/partition; record host/local/peer location explicitly | Existing placement contracts; no implicit multi-GPU transformation |
| AI-P5 communication | Account required copies/publication; keep peer/RDMA/collective traffic separate | Existing communication owner, scope and effects |
| AI-P6 physical residency | Propose direct/L2-scenario/SMEM/RF choices and raw/decoded allocations | Runtime-only reservations and safe-reuse proof, never a guaranteed L2 placement |
| AI-P7 tiling | Jointly enumerate output/reduction/query/head tiles, transaction maps and resource occupancy | LNO canonical-loop services plus operation-specific planners |
| AI-P8 movement | Co-design prefetch distance, cooperative/bulk/tensor-map mechanism, waits and consumer release | Event DAG, exact generations, access completion and progress proof |
| AI-P9 selection/application | Select complete legal candidate, atomically apply once and verify High exit | Phase owner; invalidation, rebuild, COMMENT and fallback |
| AI-P10 runtime interface | Export resources, profile compatibility and existing guards for compiled products | Existing buffer/provider/Q3 authority; AOT selection policy stays deferred |
| AI-P11 feedback | Import separately collected timing, resource and counter evidence for later AOT work | External collector and profile importer; no device activity in search |

P7.0-P7.11 stay useful projections of one candidate. No individual subphase
irreversibly fixes a tile, cache policy or buffer depth before P8/P9 sees its
complete cost. Reuse existing LNO dependence/alias/DU and Wolf-style unimodular
transformations where admitted. A Cohen-style polyhedral or hybrid search
provider is a separately implemented/qualified alternative, not assumed
available merely from this design.
Paged indirect gathers retain explicit non-affine obligations; they do not
acquire affine legality by being called a tensor.

## 7. WHIRL and backend-driver ordering

1. Resolve exact ISA, resource/profile identities, optimization level and
   compilation scope. Verify the Very High WHIRL domain operation and tensor
   contracts before exposing a common algorithm. WHIRL opcodes define
   operation semantics; TensorDescriptorIR and tensor traits carry semantic
   tensor properties. Ingestion preserves these contracts for algorithmic
   lowering and subsequent target-specific planning.
2. PREOPT establishes canonical common High WHIRL and preserves/rebinds
   semantic/cache facts. Keep DO_LOOP and OPR_ARRAY indexing available to LNO.
3. At the canonical LNO snapshot boundary, capture owned loop/access/storage
   identities and epochs. Validate numerical, model and Q3/serving bindings;
   derive dependence/locality facts and detached candidate descriptors.
   Reuse the current Lnoptimizer hook in osprey/be/lno/lnopt_main.cxx: after
   successful full array-dependence graph construction and enabled-path checks,
   before Hoist_Varying_Lower_Bounds and competing legacy transformations.
   The initial root remains FUNC_ENTRY with admitted loop subsets; arbitrary
   REGION-root entry, autopar/Lego/MP combinations and incomplete/Too_Messy
   coverage remain refused until separately implemented and tested.
4. Execute bounded read-only estimation. Revalidate the selected plan against
   the live snapshot. Atomically generate the selected multi-level loops,
   SMEM staging/prefetch DO_LOOPs, logical buffers and event semantics in High
   WHIRL, retaining COMMENT explanations and the existing PREOPT workaround.
5. Invalidate and rebuild parent/loop, DU, alias, dependence, access, resource,
   selected-plan and report facts affected by the transaction. Preserve the
   managed success path's exclusion from competing ordinary LNO scheduling,
   with validated CG-dependence export and ordinary cleanup. Rebind
   post-PREOPT facts after any subsequent canonicalization. Rollback restores
   the prior valid tree and analyses; stale epochs never authorize a commit.
6. Verify High exit before structured memory lowering: all selected logical
   buffers have valid extents/layout/ownership; accesses are in bounds;
   transfer/compute/readers have readiness and reuse joins; all numerical,
   target, tail, profile and Q3 obligations are bound; unsupported operations
   retain an admitted fallback or diagnose before emission.
7. Mid WHIRL lowers structured memory operations and standard ARRAY/control
   forms to concrete addresses, transfers, conversion/dequant operations,
   guards and synchronization while preserving the selected contract.
8. Low WHIRL binds selected CTA/warp/lane layouts and target instructions,
   including barrier/async-proxy/compute completion. Open64 target-specific
   lowering realizes the selected layout, memory and synchronization contract.
9. PTX and whirl2c print the selected native semantics and COMMENT nodes.
   ptxas resource reports and device qualification validate actual registers,
   spills, shared bytes and compatibility. Late failure selects the existing
   fallback; it does not start benchmarking/lowering every losing candidate.

O0 retains straightforward semantic lowering; O1 stays basic-block local;
O2 stays PU-local within its existing contracts; the proposed joint loop/
memory search is O3 and explicitly controlled. Cross-PU work requires IPA.
No schema/option in this proposal changes native encoding or adds a library
dependency to be.so.

## 8. Two-phase convergence and feedback

Keep the existing bounded portfolios, cache of identical estimates, global
evaluation/time/memory budgets, deterministic seed/tie-break and all existing
termination reasons. Add a versioned, explicit candidate census and record
feasible, incomplete and unimplemented states separately. A modeled family
without a materializer cannot become the executable incumbent.

Phase 1 consumes immutable source/fact/profile/permission keys and computes
only estimates. Default phase-2 budget per admitted site and round is one new
selected realization plus its correctness/race/resource checks and comparison
to an already qualified
baseline, within the compilation request's total application budget. The
current managed whole-PU route initially remains one admitted site per PU;
additional sites need fresh, nonstale snapshots after each accepted mutation.
A failed selection falls back. Offline qualification or a later compilation
can start another explicitly budgeted round with a fresh profile;
it is not hidden work inside the candidate loop.

Conclude best estimated in the declared space under the supplied evidence,
with the explored fraction, uncertainty, stop reason and unimplemented
families disclosed. Model ranking is not a global-optimum claim. Hardware
measurement validates the selected product and calibrates future rounds.

## 9. Server qualification and performance evidence

H0 must discover and qualify the confirmed H20 96 GB servers before claiming
advantage. Other H20 variants need separate resource/profile compatibility.
Use dedicated GPU leases, idempotent jobs, cancellation and automatic result
packaging in the existing worker/API. Record heterogeneous devices and MIG
partitions individually; per-board HBM is not the sum over eight boards.

Run separate correctness, sanitizer, ordinary paired timing and profiling
jobs/lanes. Measure kernel, complete GPU invocation and end-to-end host time
with clear residency/transfer boundaries. Repeat paired comparisons under a
declared cache and clock regime; retain uncertainty and tail latency.
Evaluate bandwidth saturation against a measured matching access-pattern
ceiling, including a compute-coincident ceiling when needed.

Report the existing three performance groups:

1. Supply-related issue starvation: measured issue/eligible activity and
   sampled dependency/barrier reasons, with denominators and supported
   instruction context. A warp-stall fraction is not automatically the
   percentage of wall time the entire GPU waits for data.
2. Path service: physical HBM/L2/SMEM traffic and utilization where supported;
   distinguish useful_bytes/time from hardware bytes/time. The former can
   exceed physical bandwidth through reuse. Match counter bytes and duration
   within their own collection window.
3. Local/peer/remote service: combine validated allocation/placement facts,
   topology and supported link/path counters. Link traffic does not uniquely
   attribute a kernel's waiting cycles. Unknown attribution stays unknown.
   DSM traffic within a cluster is on one GPU, not remote-board traffic.

Query metric availability on the installed Nsight version; retain raw reports,
replay/cache-control settings and missing-counter reasons. Do not combine
multi-pass counters with an unrelated timing denominator. Treat the
[Nsight profiling guide](https://docs.nvidia.com/nsight-compute/ProfilingGuide/index.html)
as the interpretation baseline. Calibration/profiling remain separate from
ordinary timing and from compiler search.

## 10. Priorities, risks and decision gates

Start with inventory and baseline correctness, then profile import and common
traffic/event costing. Bring up Reduction/LayerNorm transport and numeric lanes,
GEMM's complete steady-state pipeline, and MLA grouped/source-ordered families
as focused implementation slices. Qualify each selected product throughout;
release qualification is a final gate, not the first hardware test.

| Risk | Required control |
| --- | --- |
| Higher bandwidth exposes compute or recurrence limits | Use measured per-path compute/service balance; keep direct and fewer-stage candidates |
| Reuse reduces independent work | Include CTA coverage, wave tails, register pressure and merge traffic |
| Quantization/dequantization becomes compute-limited | Charge scales, unpack, conversion, decoded storage and full invocation |
| Synthetic profile treated as H20 | Separate versions/units/provenance; reject mismatched or incomplete evidence |
| Numerical reformulation sneaks in through scheduling | Distinct source permissions, native contracts and exact/tolerance-specific oracles |
| Async overwrite or barrier deadlock | Typed generations, exact byte counts, participant joins, progress and sanitizer qualification |
| Serving page-size advice breaks allocation/lifetime | Recommend through a serving contract; never silently rewrite its page allocator |
| Profiling or cache state changes the answer | Separate timing/profiling and declare cold/warm/contended cases |
| Local optimization regresses complete inference | Include projections, prepare/finalize, transfers, launches and representative workload weights |

Paged allocation uses byte-address/alignment/stride, packed payload and scale
geometry, valid-page bounds and legal transfer runs. Choose serving token-block
size jointly with fragmentation, tail waste, lookup cost and reuse. A GPU cache
sector is not a serving token block; larger/aligned blocks do not guarantee
hits. Tensor-map descriptors and cross-CTA multicast/DSM are later qualified
mechanisms. AOT phase selection, PREOPT COMMENT repair and OpenCTI remain
separate deferred work.

## 11. Primary references and evidence policy

- [NVIDIA Hopper tuning guide, CUDA 12.8](https://docs.nvidia.com/cuda/archive/12.8.1/hopper-tuning-guide/index.html):
  async transport, cluster facilities and occupancy guidance; query SKU limits.
- [PTX ISA 8.7](https://docs.nvidia.com/cuda/archive/12.8.1/parallel-thread-execution/index.html):
  pinned instruction and synchronization contracts, including exact target
  suffixes. Current Q6 gates can remain stricter than instruction minima.
- [CUDA memory optimization guidance](https://docs.nvidia.com/cuda/cuda-c-best-practices-guide/index.html#coalesced-access-to-global-memory):
  coalescing and transaction efficiency; measured cache service remains separate.
- [CUTLASS efficient GEMM](https://docs.nvidia.com/cutlass/latest/media/docs/cpp/efficient_gemm.html):
  hierarchical blocking and pipeline/scheduling tradeoffs.
- [cuBLAS/cuBLASLt](https://docs.nvidia.com/cuda/cublas/index.html):
  baseline algorithm, layout and compute-type configuration.
- [Nsight Compute profiling guide](https://docs.nvidia.com/nsight-compute/ProfilingGuide/index.html):
  metric semantics, cache effects and collection/replay overhead.

The proposed algorithms, cost equations, milestones and conditional numerical
examples are Open64 design decisions/inferences, not vendor performance claims.
H20 peak bandwidth and arithmetic throughput remain unbound until H0 supplies
the actual device evidence.
