# MMA Compute Realization And SIMT Fallback

Version v0.1 | 2026-10-27 | MMA0 approved for bounded MMA1 implementation

MMA1 now defines runtime-only compute contracts and four exact planning-only
recipes. See the [implementation and pinned target inventory](AI-COMPILER-MMA1-TARGET-INVENTORY.md).
MMA2 now has a bounded [analytical shadow model](AI-COMPILER-MMA2-SHADOW-MODEL.md)
with explicit unknown production timing and synthetic-only oracle scenarios.
It cannot change SIMT execution. MMA3 typed fallback, guarded promotion and
all native lowering/certification remain open.

## 1. Decision And Current Boundary

Treat matrix-multiply-accumulate (MMA) as a **compute realization of a semantic
contraction**, selected jointly with representation, tiling, placement,
movement, storage, numerical policy and epilogue. Preserve a legal SIMT
realization. TMA changes operand delivery; it does not implement MMA.

This is a DSC design, not NVIDIA's compiler algorithm or timing model.
MMA1's target inventory is a structural capability contract, with no new
option, executable instruction or persistent schema. Current F32 D/S1/T2 kernels
use scalar multiplication/addition; the numerical-contract prototype is not
native Tensor Core lowering. Blackwell registry rows remain planning-only.

Controlling contracts: [common execution planning](AI-COMPILER-COMMON-EXECUTION-PLANNING.md),
[hardware/resource planning](AI-COMPILER-HARDWARE-RESOURCE-AND-MEMORY-PLANNING-DESIGN-v0.1.md),
[hardware-description ownership](AI-COMPILER-HARDWARE-DESCRIPTION-TARGET-SELECTION.md),
[analytical readiness](AI-COMPILER-OPTIMIZATION-ANALYTICAL-READINESS.md),
[occupancy](AI-COMPILER-OPTIMIZATION-OCCUPANCY.md) and
[numerical accumulation](AI-COMPILER-NUMERICAL-ACCUMULATION-DESIGN-v0.1.md).
This amendment extends their future search space; it does not relabel their
accepted scalar receipts as MMA evidence.

## 2. Terms Before Use

| Term | Definition |
| --- | --- |
| Semantic contraction | Logical indexed product/reduction, independent of instruction family; a matmul may require many hardware instructions. |
| SIMT | Single-instruction, multiple-thread execution; here the scalar/vector arithmetic alternative, not a synonym for Tensor Core MMA. |
| Compute realization | Concrete instruction family, shapes, participant/fragment mapping, operand sources, accumulator and completion protocol implementing a contraction. |
| Instruction atom | One supported instruction variant with logical `(MI, NI, KI)` extents; distinct from a CTA tile and SMEM `BK`. |
| Warp / warpgroup | A warp contains 32 threads; Hopper WGMMA operates collectively on a four-warp, 128-thread warpgroup. |
| CTA | Cooperative thread array, also called a CUDA thread block. Execution participation and storage reservation must name their CTA/group scope. |
| RF / SMEM / TMEM | Register file; software-managed shared memory; target-specific tensor memory. TMEM is a distinct allocation/access resource, not an alias for either RF or SMEM. |
| MMA issue / completion | Initiating matrix work / establishing that its required accesses or results are complete. Issue is not completion. |
| Operand-ready / slot-reusable | Producer data is available for a read / all readers of an old slot generation are finished so it may be overwritten. These are different proofs. |
| Useful / executed work | Arithmetic contributing to requested results / all executed arithmetic, including padding and explicitly modeled software decomposition. Count a multiply plus add as two FLOPs for floating GEMM accounting. |
| Service interval / latency | Resource occupancy or sustainable initiation spacing / delay until a dependent result is available; distinct hardware-description fields. |
| Numerical contract | Source/output formats, contraction and regrouping permissions, input conversion, internal/outer accumulation evidence and acceptance policy. |
| Certified / review-only | Verified at the stated native target and numerical/performance scope / compiler-side artifacts without that certification. Neither is inferred from source syntax alone. |

## 3. Target Realizations And Evidence

The following is a bounded target roadmap, not a capability matrix for every
NVIDIA product. Exact instruction/type/shape/layout availability comes from
versioned target records, not a family-name test.

| Realization | Storage and execution contract | Proposed initial scope |
| --- | --- | --- |
| SIMT | Explicit scalar/vector arithmetic; register accumulation and selected D/S1/T2 delivery. | Existing F32 path stays the default. Add a same-contract fallback before enabling a new source dtype. |
| Warp MMA | `mma.sync`-family fragments mapped across a warp; register operands and accumulator. | Optional intermediate/reference realization, not a prerequisite for WGMMA and not the SIMT fallback. [N3] |
| Hopper WGMMA | Warpgroup participation; B in SMEM, A in SMEM or registers; register accumulator and asynchronous compute. | First recipe: one consumer warpgroup, dense F16/BF16 inputs, FP32 accumulation, one CTA, fully waited SMEM staging. Add pipelined delivery only after this recipe is verified. [N2] |
| Blackwell SM100-class `tcgen05` | Target-specific issuer protocol, SMEM/TMEM operand paths and TMEM accumulator. | First recipe: explicit `sm_100a`, dense F16/BF16, FP32 accumulation, `cta_group::1`; one owned TMEM allocation with complete teardown. [N4] |
| Other Blackwell paths | SM120 has separate warp-MMA recipes; it must not inherit the SM100 TMEM recipe. | Out of the initial lowering scope. New exact targets require independent profiles and tests. [N3, N5] |

For the baseline target contract, WGMMA requires `sm_90a`; initial
`tcgen05` targets `sm_100a`. PTX/toolchain versions and any later family-specific
target support must be explicit. Neither `sm_90` nor `sm_100` silently gains
architecture-specific instructions; do not strip `a`/`f` suffixes. [N1, N6]

TF32 reduces FP32 input precision even when storage/result remain FP32.
Preserving storage dtype therefore does not establish numerical equivalence.
The ISA's accumulation/rounding rules must be checked per instruction. [N1]
Hopper FP8 partial-sum promotion and Blackwell NVFP4/MXFP4 scale/accumulation
remain separate follow-ons governed by the numerical design, not initial
F16/BF16 implementation prerequisites.

## 4. IR, Ownership And Phase Integration

`CommonComputeRealizationIR` is a proposed analysis/plan view, not an allocated
logical opcode or a new mapped-image table. Reuse existing records where they
carry the same facts; review concrete representation separately.

| Classification / owner | Required record contents or responsibility |
| --- | --- |
| Common Compiler Substrate: compute realization view | Semantic region/value IDs; family/variant identity; instruction atom; execution participants; operand/accumulator/result representation IDs; numerical-contract ID; dependence, completion and fallback references; plan invalidation identity. |
| Common Compiler Substrate: CommonTilePlanIR / RepresentationDescriptor | CTA/warp/instruction covering, logical-to-physical fragment maps, boundary policy and useful/executed work. Do not replace a tensor descriptor with an opaque fragment name. |
| Common Compiler Substrate: resource/lifetime/pipeline/guard records | RF/SMEM/TMEM or other extensible resource demands, allocation scope/quanta, typed issue/read-complete/result-complete events, alias ranges, generations, runtime predicates and same-contract fallback. |
| Common Compiler Substrate: CandidateCostIR / hardware description | Rate/latency/resource payloads, units, evidence/ranges and model revision. Structural validation is common; selection policy is not. |
| NVIDIA target adapter | Exact SM/PTX/toolkit capability, legal instruction variants, fragment/descriptor layouts, proxy/fence/commit/wait rules, TMEM allocation/access rules and emitter obligations. |
| AI-domain-specific | Attention masks/normalization/KV contracts; MoE routing, imbalance and expert ownership; model accuracy/quantization acceptance. Generic contraction mapping is not an AI-only arithmetic contract. |
| PU-local VHO P6-P9 | Characterize canonical WHIRL, enumerate compatible tuples, prove legality, derive resources/events, cost/select, materialize and postverify. No cross-PU traversal; IPA owns interprocedural work. |

Persist required executable target meaning in selected WHIRL before emission.
Use established symbol/type attributes, high-level loops and reviewed
operations/intrinsics; require a verifier and compatibility review for any
missing carrier. A pointer or unstructured annotation is not a substitute
for executable MMA semantics. `whirl2c -cuda` prints the chosen operations,
using validated target wrappers/inline PTX if required; it does not tensorize
scalar loops or invent a completion protocol. The eventual PTX path consumes
the same WHIRL, not an independent kernel template.

| Phase | Additional action and required summary |
| --- | --- |
| P0-P2 | Identify contractions and numerical permissions; preserve non-contraction state and verified algebraic alternatives. Summarize eligible regions and reasons other regions remain SIMT/reductions. |
| P3/P6 | Propose compatible payload/scale/fragment layouts and resource ownership. Summarize conversion/repacking, alignment, storage and lifetime obligations. |
| P7, including P7.11 | Enumerate instruction atom, CTA/warp covering, accumulator shape and boundary strategy with each compute/movement pair. Summarize every legality rejection and useful/executed work. |
| P7.4/P8 | Derive operand-ready, MMA issue/completion, accumulator read/merge and slot-release events jointly. Summarize uncovered waits and which completion proof protects every reused range. |
| P9 | Compare complete same-contract plans, retain SIMT, materialize selected WHIRL and audit actual actions. Summarize old/new winner, fallback, total/components, uncertainty and decision reason. |
| P10/P11 | Guard exact target/layout/shape requirements; join native numerical, instruction, resource and timing evidence. Summarize selected runtime variant and remaining certification gaps. |

The six fixtures contain contractions in GEMM (one), Attention (two) and MoE
(two batched contractions). These counts are semantic, not MMA instruction
counts. Row reduction, Softmax and LayerNorm keep their natural state/elementwise
realizations; do not reformulate them as matrix operations without separate
equivalence and profitability evidence. Generic native ingestion for the
additional fixtures is still open. See the
[fixture census](../osprey/torch2whirl/BUILDING-BLOCK-INGESTION-PLAN.md).

## 5. Legality Gate Before Profitability

Every failed or unknown mandatory obligation rejects that candidate, not
its legal siblings. Predicted performance cannot repair failed legality.

| Gate | Required proof / rejection example |
| --- | --- |
| Semantic | Canonical contraction domain/access/alias/effect facts; exact output coverage and unique ownership; correct alpha/beta/epilogue if present. Initial scope remains standalone `C=A*B`, no fusion, sparse inference or split-K. |
| Numerical | Source operand/output types, allowed reduction order, contraction, conversion/rounding, saturation/subnormal/nonfinite policy and internal/outer accumulation evidence. Bitwise scalar-order requirements reject an unproved MMA recipe even with unchanged input types. |
| Precision permission | Default preserve forbids implicit F32-to-TF32/F16/BF16/FP8/FP4 arithmetic. Future `gemm_precision=allow_narrow` plus a region acceptance contract may admit, never force, a specific conversion. Already-low-precision inputs still need accumulation/order legality. No invented strict-FP32 emulation recipe. |
| Target/toolchain | Exact feature set and suffix, PTX version, assembler/toolkit and selected instruction variant. An architecture table or CUTLASS recipe is not evidence our emitter supports it. |
| Shape/representation | Exact allowed atom, CTA covering, fragment distribution, major modes, descriptor encodings, alignment/strides and operand source tiers. Arbitrary scalar XOR/padded SMEM layouts cannot be reused unless the MMA descriptor accepts them. |
| Boundaries | Prove permitted padded values, valid loads and masked stores, or select an explicit scalar edge kernel with all launches costed. Padding zeros is not automatically safe for every nonfinite/strict-FP contract. Never predicate away required collective participants. Initially support exact-divisible recipes; retain SIMT for tails. |
| Participation | Uniform required warp/warpgroup participation, correct issuer election, group/CTA IDs and launch shape; reject unsupported cluster/CTA-pair recipes rather than approximating them. |
| Access ordering | Producer visibility/proxy ordering, instruction-specific fences, MMA issue-to-completion and accumulator read/merge ordering; join every outstanding reader before operand overwrite. TMA readiness or a CTA barrier alone does not prove asynchronous compute completion. |
| Allocation/progress | Aligned nonoverlapping live ranges, target RF/SMEM/TMEM limits, barrier state, outstanding-group/queue limits and an acyclic progress protocol. Allocation, waits, deallocation and allocation-permit release must follow the exact target recipe; no blocking resource acquisition after consumers hold a circular dependency. |
| Lowerability/guards | Fully implemented WHIRL construction, verifier, emitter and launch ABI; every runtime-only condition has an executable pre-launch guard and a lowerable same-contract fallback. No partial selected-image publication on failure. |

Hopper completion protocol must explicitly capture register-order fences,
commit groups and waits. Blackwell must explicitly capture TMEM allocation,
MMA completion, result-read completion and eventual deallocation/permit
handling; a TMA-only event graph is insufficient. Target rules, not source
statement order, define these obligations. [N1, N2, N4]

**Fallback is semantic, not merely a label.** Current D/S1/T2 implementations
are F32 only. A new F16/BF16 source path first needs a verified SIMT path with
the same input/output and permitted accumulation semantics; the existing F32
kernel cannot simply be called on differently encoded buffers. For an
authorized narrowed candidate, retain the original-source-precision plan and
buffers, not only a scalar computation on already-quantized values. Pre-launch
guard failure may dispatch fallback; do not claim recovery from an arbitrary
asynchronous GPU fault by rerunning a possibly partially written output.

## 6. Proposed Hardware Description Extensions

No private delay constants and no advertised peak speedup multiplier. Each
field needs scope, units, revision, provenance, applicability and unknown state.
An SM key selects capability; a separate SKU/partition/clock profile supplies
device performance. Unknown is not zero. Existing estimated scalar rates may
not stand in for Tensor Core timing.

| Payload | Proposed additions |
| --- | --- |
| Instruction capability | Exact variant/type/shape/layout/source-tier table; feature/PTX/toolkit predicates; collective and completion recipe ID. |
| Tensor execution | Per-SM initiation/service rate by variant; issue/retire overhead; result latency and accumulator-dependence initiation interval; finite outstanding groups and resource-sharing identity. |
| Operand delivery | Existing GMEM sectors/cache-path assumptions and TMA service, plus descriptor/fragment preparation, RF/SMEM-to-compute service and alignment/bank restrictions. Do not assign SIMT bank penalties to descriptor MMA without a target model. |
| Accumulator/epilogue | Register fragment counts on Hopper; TMEM physical layout, legal allocation quanta/capacity/scope, alloc/free and load/store service/completion on SM100; conversion/scale/promotion and output paths. |
| Reservations | RF per thread/warp and allocation partitions, SMEM reservation/carveout, barrier/outstanding limits; TMEM reservation and CTA-pair/cluster constraints when supported. |
| Evidence/uncertainty | Specified structural facts separately from estimated timing, measured calibration conditions, ranges and validated domain; missing required rates prohibit automatic MMA promotion. |

RF demand is derived per participating thread/warp, including operands,
accumulators, address/descriptors, predicates, producer/control state and
epilogue temporaries; allocation rounding follows the target profile, not an
average across unequal producer/consumer groups. On SM100, count the real
TMEM lane/column layout and overlapping accumulator/scale generations, not
just `BM*BN*4` bytes; TMEM does not eliminate epilogue register demand.

For the initial independent-CTA recipe, extend the existing residency estimate:

```text
R = min(block_limit, thread_limit, warp_limit, RF_limit, SMEM_limit,
        TMEM_limit_if_used, other_target_reservation_limits)
```

Only use such a quotient when the target resource has a valid per-CTA
reservation model. SMEM capacity is configured availability, not the published
maximum. TMEM access/allocation instructions and packing can impose additional
hard constraints even when byte totals fit. `cta_group::2`, clusters,
persistent schedules and producer/consumer warp specialization require their
own placement/reservation model; reject them in the first recipe. Native
register/spill/occupancy results later validate estimates, not the reverse.

## 7. Complete-Plan Profitability Model

### 7.1 Work, Scope And Comparability

For one `M x K` by `K x N` contraction:

```text
F_useful = 2*M*N*K
N_mma = sum_over_actual_output_tiles_and_segments(atom_instances)
F_executed_mma = sum_over_instances(2*MI*NI*KI)
tile_efficiency = F_useful / F_executed_contraction_work
```

Count padding, edge kernels and every software product decomposition.
Report scale, conversion, activation, merge and epilogue operations separately.
For batched/grouped contractions sum actual domains; do not assume a uniform
MoE expert load. Useful FLOPs are comparable; padded work and MMA instruction
count measure different things. Precision must satisfy the same accepted
source contract before costs are compared.

Define two explicit boundaries:

```text
T_kernel = completion(selected_resource_constrained_event_plan)
T_invocation = unhidden_pack_or_quantize + descriptor_or_workspace_setup
             + host_launch_or_dispatch + T_kernel
             + unhidden_extra_kernels_and_transfers
T_amortized = reusable_one_time_preparation / validated_reuse_count
            + per_invocation_completion
```

Charge original-data reads, scale traffic, temporary buffers and restoration
where required. Reuse credit requires a lifetime/invalidation and ownership
contract; do not amortize mutable activations as immutable weights. Compare
all plans at the same boundary. Existing model6's issue slot is not a measured
host launch cost, and its reference cycles are not nanoseconds.

### 7.2 Service And Readiness

For each modeled resource `r`, derive work `W_r` and service rate `B_r` from
the same target profile. Resources include device memory bandwidth, SMEM/TMEM
paths, Tensor Core execution, scalar epilogue/merge work and shared instruction
issue. Shared resources share one service budget across resident CTAs.

```text
service_r = W_r / B_r                         # consistent declared units
ideal_resource_floor = max_r(service_r)
compute_floor_on_SM = max(F_executed_mma_on_SM / tensor_FLOPs_per_unit_on_SM,
                          N_mma_on_SM / tensor_issue_rate_on_SM)
```

Evaluate compute work on the busiest modeled SM in each residency round;
device-memory service uses the round's aggregate bytes and device-wide rate.
Do not divide whole-grid work by one SM's throughput. The last maximum is a
diagnostic floor, not the complete compute latency.
Account for accumulator dependence, instruction startup/completion, finite
in-flight groups and output drain. Throughput and latency are not synonyms;
do not substitute `N_mma * full_result_latency` when documented pipelining is
legal, or use peak FLOPs alone when a dependence chain stalls issue.

Extend analytical readiness with typed compute events:

```text
for event e in a deterministic legal order of the symbolic schedule:
    start(e) = max(required_input_ready_times(e),
                   accumulator_dependency_ready(e),
                   participating_actor_ready(e),
                   resource_reservation_ready(e), slot_or_queue_ready(e))
    reserve each actual service resource for its modeled service interval
    completion(e) = target_completion_rule(start(e), service(e), latency(e))
    expose only the dependencies and access completions this rule guarantees
T_kernel = final_output_completion - first_kernel_event
```

This is a proposed static, resource-constrained analytical schedule, not a
SASS simulator or a proof of actual device scheduling. Distinguish internal
accumulator forwarding from completion visible to scalar code. A group can
release an input slot only after a target-supported reader-completion join;
otherwise conservatively wait for full MMA completion. Prove ordering using
event contracts, never predicted timestamps. Compress repeated schedule
regions analytically where valid; retain an independently tested literal
oracle. Bound search/evaluation complexity and diagnose exhausted budgets.

Model6 residency rounds remain a baseline approximation: aggregate per-SM
work and shared bandwidth across the active CTAs; **do not multiply hardware
throughput by occupancy**. Recompute reservations and full/tail rounds for
each compute realization. Native allocation and CTA-placement uncertainty
remain explicit. Unsupported scheduling patterns are not costable by silently
reusing the scalar recurrence.

### 7.3 Accounting, Bubbles And Roofline

Keep a reconciled attribution of the modeled makespan:

```text
T_kernel = compute_service + unhidden_operand_or_accumulator_movement
         + unhidden_synchronization + unhidden_issue_or_control
         + unhidden_epilogue_and_output
```

Assign overlap intervals once under a fixed attribution convention; parallel
engine busy-time counters may sum above makespan and are separate diagnostics.
Do not subtract another overlap credit or add a readiness wait twice. Report
operand-starvation, accumulator-dependence, in-flight-limit, allocation,
resource-contention and epilogue/drain waits separately; these explain why an
MMA path can lose despite faster arithmetic. Attribution is not causal stall
measurement from hardware.

Roofline remains a **lower-bound diagnostic**, never another additive cost:

```text
AI_j = F_useful / bytes_j
useful_compute_ceiling = tensor_or_SIMT_peak * tile_efficiency
useful_rate_ceiling = min(useful_compute_ceiling, min_j(B_j * AI_j))
T_floor = max(max_e(F_executed_on_engine_e / compatible_peak_e),
              max_j(bytes_j / B_j), other_unavoidable_service_floors)
```

The simple useful-rate ceiling describes a homogeneous compute realization;
mixed MMA/scalar tails also need separate engine work/ceilings and the event
graph's kernel ordering. Omit zero-traffic tiers from intensity division.
Each `j` names a consistent tier/path and scope. Issued GMEM-sector bytes are
not measured HBM misses; label proxy and cache assumptions rather than mixing
them with an HBM-only roofline. Include scales, repacking, TMEM reads and
output traffic at their proper tiers; report useful versus executed FLOP
rates. Check `T_kernel >= T_floor` for the same work/boundary/profile and
explicitly include scalar epilogue ceilings when relevant. A faster compute
engine can expose *more* delivery bubbles; the remedy may be a different tile,
layout or pipeline, not a larger advertised FLOP rate.

### 7.4 Selection And Uncertainty

Retain the cheapest legal, lowerable same-contract SIMT plan `S`. Proposed
automatic promotion requires a complete comparable cost and a declared
minimum benefit `rho`, not a fixed undocumented multiplier:

```text
promote MMA plan P only if U(T_invocation(P)) * (1 + rho) < L(T_invocation(S))
```

`L/U` are declared scenario/range envelopes, not statistical confidence bounds
without evidence. Evaluate shared assumptions consistently across candidates.
If bounds are missing or overlap, keep SIMT and report the nominal MMA ranking
as shadow evidence; retain deterministic ties and every rejection reason.
Use `rho=0` for an exact deterministic model comparison test, not as a claim
that estimated hardware costs are exact. The actual policy/margin needs review
and calibration before automatic promotion. Small GEMMs can lose to MMA setup,
padding and epilogue; large ones can remain memory- or synchronization-bound.

## 8. Candidate Algorithm And WHIRL Actions

```text
facts = characterize_one_canonical_contraction(active_PU)
contract = resolve_source_numerical_and_effect_contract(facts)
SIMT = enumerate_complete_compatible_SIMT_plans(facts, contract)
require at least one verified same-contract fallback before enabling MMA
evaluate all legal SIMT plans at the same cost boundary/profile as MMA

for recipe in bounded_exact_target_compute_recipes:
    for representation, tile, mapping in compatible_P6_P7_choices(recipe):
        check semantic, numerical, target, shape and participant obligations
        if failed_or_unknown: record rejection; continue
        compute = bind(recipe, representation, tile, mapping, contract)
        for movement in compatible_delivery_choices(recipe, tile):
            plan = join(compute, layout, movement, accumulation, epilogue)
            assign RF/SMEM/TMEM and construct typed P8 events
            prove bounds, alias/lifetime, completion, progress and launch guards
            if not proved or not lowerable: record rejection_or_shadow; continue
            evaluate complete same-boundary latency, resources and uncertainty
            audit service floors and cost reconciliation; retain candidate

fallback = cheapest_legal_SIMT(SIMT)
promotable = complete_lowerable_MMA_meeting_reviewed_benefit_rule(fallback)
winner = min(promotable, by=(upper_cost, nominal_cost, stable_ordinal))
         if promotable is nonempty else fallback
if mode == shadow: report hypothetical ranking; emit unchanged SIMT WHIRL
else:
    materialize winner and required fallback/guard ABI from the same plan
    postverify actual instruction/count/fragment/event/resource obligations
    atomically publish selected WHIRL; emit CUDA/PTX only from that WHIRL
```

D/S1/T2 names continue to describe the existing scalar implementations; they
do not automatically form a Cartesian product with every MMA. For example,
WGMMA cannot read its B operand directly from the existing D GMEM path.
Synchronous SMEM staging can feed an asynchronous MMA with explicit waits;
TMA is optional when the complete legal non-TMA path is faster.

Proposed `-Wb,-DSL:gemm_compute=simt|shadow|auto` is **not registered**.
Initial default is `simt`; `shadow` must preserve executable WHIRL, while
`auto` is gated on lowerable recipes, a verified fallback and the reviewed
profitability policy. Existing `gemm_plan_search` still controls joint search;
MMA shadow/auto would require joint/-O3/device-output gates. The separately
proposed `gemm_precision=preserve|allow_narrow` controls arithmetic permission,
not hardware selection. Default preserve plus F32 fixtures must stay SIMT
unless an independently verified strict-source-compatible recipe exists.
No force-MMA switch may bypass legality. Codegen-test fixtures can request a
specific legal plan for manual review without claiming automatic selection.

## 9. Trace, Reports And Acceptance

Extend the compact WOPT `.t` convention with a reviewed versioned schema;
do not silently reinterpret current `AI_OPT 7`/model6 records. Option controls
and default-on development tracing remain. Candidate source line is last.
Proposed records (column headers stated once):

```text
COMPUTE_CAND id region family variant MI NI KI inputs accumulator output contract legality reason source_line
COMPUTE_RESOURCE id rf smem tmem participants outstanding resident_ctas limiters source_line
COMPUTE_COST id tensor_service scalar_service operand_wait acc_wait queue_wait epilogue total lower upper unit confidence source_line
COMPUTE_SELECT selected simt_fallback reason boundary profile model source_line
COMPUTE_ACTION id mma_ops fences commits waits allocs frees merges stores proof source_line
```

Action counts mean actual generated WHIRL structure; distinguish static nodes
from symbolic dynamic execution counts. A rejected/shadow candidate has no
transformation action. Profile records must expose required rates/ranges and
provenance for independent recomputation; unknown is printed as unknown.

| Report | What the tool must summarize |
| --- | --- |
| `semantic.log` | Contraction boundaries, source dtype/order/contraction permissions and ineligible regions. |
| `tile.log` | SIMT/MMA identities, instruction/CTA/warp tiles, boundary waste, complete compatibility/rejection census and source lines. |
| `residency.log` | RF/SMEM/TMEM physical demands/reservations, limiters, lifetimes, native allocation discrepancies and spills when available. |
| `fetch-pipeline.log` | Operand and accumulator readiness, proxy/fence/commit/wait obligations, slot reuse proofs, queue limits and exposed waits. |
| `physical-plan.log` | Same-boundary full costs, numerical rejection, uncertainty, old/shadow/emitted winner, SIMT fallback, selection reason and actual WHIRL actions. |
| `optimization-summary.log` | Chosen compute/movement/precision/target, predicted versus measured status, fallback, rejected alternatives, completed/skipped phases and next blocker. |
| `optimization.log` | Full phase report collection under the existing aggregation convention, including non-success status through the summary. |

Every phase log begins `# What it summarizes:`. Preserve raw `.t`, source,
before/after `.B/.T`, selected plan, WHIRL-derived `.cu`, subsequent PTX/SASS,
toolchain/profile identity, numerical results, baseline, timing samples,
checksums and immutable receipts. Keep kernel-only and invocation timing
separate; measured feedback cannot rewrite an old estimated receipt.

MMA0 is approved for MMA1's bounded inventory. Before implementation beyond
that inventory, review the legality/uncertainty policy,
first source dtypes, exact target recipes and resource/event carriers. Then:

- Prove negative numerical/target/shape/layout/participation cases and atomic
  rejection; preserve current default SIMT artifacts and historical traces.
- Independently audit literal versus compressed analytical schedules, resource
  sharing, pending-group limits, deadlock negatives, cost sums and lower bounds.
- Retain manual WHIRL/CUDA review and assembler checks without needing a GPU;
  no dummy scalar adapter may stand in for actual MMA instruction evidence.
- Later certify per-target native instructions, allocation/spills, numerical
  edge cases, baseline-versus-MMA timings and model sensitivity. No GPU is
  currently allocated, so native accuracy/performance remains open.
- For FP8/FP4, require the deferred numerical/scale/promotion milestones first;
  do not inherit Hopper's precision caveat or timing penalty onto Blackwell.

## 10. Primary References And Provenance

Consulted 2026-10-27. Online `latest` documentation is mutable; implementation
must pin the exact ISA/toolkit/CUTLASS revision in target profiles/receipts.
The architecture mechanisms below are reference-derived; candidate IR,
analytical equations, conservative promotion policy and milestone sequence
are Open64/DSC proposals. No NVIDIA internal cost tables are imported.

- [N1] [PTX ISA 9.4](https://docs.nvidia.com/cuda/parallel-thread-execution/index.html): matrix instruction semantics and target notes, WGMMA and `tcgen05` ordering. Check the selected variant rather than treating this broad manual as a guarantee for every SM.
- [N2] [CUTLASS WGMMA programming guide](https://docs.nvidia.com/cutlass/latest/media/docs/pythonDSL/guides/mma/wgmma_programming.html): Hopper operand, accumulator, participant and completion mechanisms.
- [N3] [CUTLASS warp MMA programming guide](https://docs.nvidia.com/cutlass/latest/media/docs/pythonDSL/guides/mma/wmma_programming.html): warp fragments and distinct instruction support, including SM120 paths.
- [N4] [CUTLASS tcgen05 programming guide](https://docs.nvidia.com/cutlass/4.5.2/media/docs/pythonDSL/mma_docs/tcgen05_programming.html): SM100 TMEM/dataflow/allocation mechanisms, not our cost algorithm.
- [N5] [CUTLASS Blackwell functionality](https://docs.nvidia.com/cutlass/latest/media/docs/cpp/blackwell_functionality.html): instruction/format and dispatch recipes. Library tile support is not the exhaustive hardware legality set or an Open64 implementation claim.
- [N6] [NVCC target options](https://docs.nvidia.com/cuda/cuda-compiler-driver-nvcc/): exact architecture/family feature selection.
- [N7] [Hopper tuning guide](https://docs.nvidia.com/cuda/hopper-tuning-guide/index.html), [Blackwell tuning guide](https://docs.nvidia.com/cuda/blackwell-tuning-guide/index.html): capacity and occupancy constraints; SM100 and SM120 are not interchangeable descriptions.
