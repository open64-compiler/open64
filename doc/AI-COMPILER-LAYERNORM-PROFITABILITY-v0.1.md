# LN3 Complete LayerNorm Costs And Gated Selection

Version v0.1 | 2026-10-28 | Bounded native analytical service; no executable rewrite

## 1. Scope And Ownership

Compare source-preserving C0/C1/C2 implementations of explicit LN-REF-1 v2.
Source v1 stays unresolved and cannot acquire a cost-selected executable plan.
This extends [LN2 storage/events](AI-COMPILER-LAYERNORM-STORAGE-EVENTS-v0.1.md)
and the [runtime guard bridge](AI-COMPILER-LAYERNORM-RUNTIME-GUARDS-v0.1.md),
without depending on reduction coding or waiting for a GPU host.

| Building block | Owner / contract |
| --- | --- |
| Native semantic/target/storage binding | Existing WOPT APIs recharacterize the exact member of the driver-selected whole active PU. |
| Scalar execution profile | Policy-free, runtime-only optional Hardware Description IR extension in `common/com`; quantities, bounds and provenance, never selection. Existing description/image layouts are unchanged. |
| Recipe, occupancy, readiness, costs, comparison | `be/opt/opt_dsl_normalization_cost.h/.cxx`; the owning phase retains all optimization policy. |
| Candidate/plan records | Existing AIO-1/AIO-2 `DSL_tensor_evolution` and `DSL_opt_plan` services, not another registry. Candidates realize the same semantic Y root; X-to-Y is not falsely modeled as an identity-preserving layout edge. |
| Selected guard | Fresh export of the selected native plan, still containing outstanding runtime/lowering obligations. |
| Independent model oracle | Python literal program builder and frontier-scan scheduler; no native scheduler calls or reduction imports. |

`WOPT_DSL_select_normalization_plan` takes three comparable plan requests,
one versioned timing envelope, fixed per-candidate RF estimates, expansion
budget and minimum-benefit margin. It returns all candidate costs, nominal
winner, gated analytical choice, fallback, common-record IDs and selected guard.
The whole service is read-only. IDs are verified query-local identities, not
live retained registries or mapped executable state.
This initial comparison fixes common runtime bindings, target, layout and launch
geometry across the three retention choices; thread-count/layout search is a
separate expansion, not an unreported part of this bounded candidate space.

The normal Python matcher remains v1. The ordinary backend path still diagnoses
unselected v2 normalization rather than silently choosing a kernel. LN3 is an
explicit planning API, not a new default driver transformation or option.
[LN4's explicit lowering API](AI-COMPILER-LAYERNORM-EXECUTABLE-LOWERING-v0.1.md)
now revalidates approved selection and materializes authoritative device WHIRL;
default request/profile plumbing and a production runtime provider remain open.

## 2. Terms And Comparison Boundary

| Term | Definition |
| --- | --- |
| Complete analytical cost | Every work, delivery, arithmetic, control, synchronization and invocation term in this closed recipe has an explicit input or derived value. Not complete GPU simulation or measured time. |
| Reference cycle | Common assumed time unit within one identified profile; not a calibrated physical SM clock cycle. AIO-2 stores relative units with low confidence/target-model evidence. |
| Service | Engine occupation for bytes or instruction issue; other work can use the engine after service ends. |
| Completion latency | Time until data/result is usable. Memory startup follows byte service; scalar completion is `max(issue, instruction latency)`, not issue plus an already inclusive latency. |
| Dependency readiness | All data/visibility/predecessor work complete before an operation may begin. |
| CTA slot | A fixed resident reservation on one SM, limited by blocks, threads, warps, rounded RF and SMEM. Later rows assigned that slot wait for its prior CTA to finish. |
| Carrier traffic | Private scratch packaging around the existing pointer-scalar ASM interface, including stores/loads used to pass operands and recover results. Not silently promoted to registers. |
| Issued sector | Transaction projection from warp addresses and guaranteed alignment. It is not a measured HBM transaction. |
| Model envelope | Optimistic/nominal/pessimistic numeric bounds plus per-field origin. The resulting schedule bounds describe this declared model, not physical hardware confidence. |

Compare the same **device-resident, input-ready invocation** through result
visibility. Inputs/output allocations and upstream host transfers already exist;
request queuing and downstream consumer duration are outside the boundary.
Charge runtime guard/setup, launch, workspace preparation and required publication.
SMEM reservation and private-frame requirements remain entry/lowering obligations;
no hidden host allocation is assumed free. Lifecycle consumer joins from LN2-d
remain necessary for safe release but are not application-independent latency
terms in this comparison.

## 3. Hardware Inputs And Unknowns

`DSL_SCALAR_EXECUTION_PROFILE` has version, exact architecture ID, description
revision, model ID, nineteen numeric triples and per-field provenance:

| Field | Scope / meaning |
| --- | --- |
| Global rate | Aggregate device-global read/write bytes per reference cycle; external tensors and all-miss private carrier traffic share this engine. |
| Shared rate | Per-SM bytes/cycle, charged with the LN2 bank-conflict bound. |
| Compute rate | Per-SM scalar active lanes/cycle; minimum one issue cycle for a nonempty operation. Not Tensor Core peak. |
| Global/carrier/shared latency | Respective startup after byte service. No cache-hit or spill-elimination guarantee. |
| Add/sub, multiply, division, sqrt latency | Instruction-start to dependent-result readiness; division and sqrt remain distinct. |
| Division/sqrt issue cycles | Separate per-SM issue occupation for those instruction sequences; not borrowed from ordinary scalar peak throughput. Zero explicitly means no extra minimum beyond scalar issue, not a missing field. |
| Control cycles | Index/predicate/loop/pointer-address service on the shared per-SM scalar/control engine. |
| Barrier cycles | Per-SM synchronization-engine service for the required uniform CTA barrier. |
| Guard, launch, workspace, publication | Explicit invocation-boundary costs, including known-zero workspace if supplied. |
| Sector bytes | Fixed power-of-two transaction projection, 4..256 bytes; all three corners must agree. |

Each field has ascending low/nominal/high numeric values. Duration evaluation
reverses the rate corner. Provenance is unknown, estimated, measured input, or
synthetic experiment. Unknown fields must have zero payload but are **not zero
cost**. Explicit known-zero durations are permitted. Invalid intervals,
provenance, version, architecture/revision or denominators refuse comparison.

The new built-in lookup returns an explicitly **incomplete** extension for each
exact `sm_90`, `sm_90a`, `sm_100`, `sm_100a` description. Missing arithmetic,
barrier, cache/carrier and invocation timing is not guessed from GEMM rates.
Existing capacity/resource envelopes remain unchanged and explicitly estimated;
their four-SM synthetic envelope is not an H200 or Blackwell SKU description.
Field provenance and numeric assumptions are printed in the raw trace. Even
measured inputs cannot switch this analysis-only service into production lowering.

## 4. Exact Closed Recipe

Both statistics retain ascending FP32 folds, sum-then-divide finalization,
separately rounded centered square, epsilon addition, sqrt then reciprocal
division. Every row has **three division operations and one sqrt**. No tree,
Welford, reassociation, contraction, reduced precision, MMA, async or TMA appears.

```text
C0: lane-zero mean over X
    lane-zero variance over X, recomputing d
    lane-zero epilogue over X, recomputing d; other lanes idle

C1: lane-zero mean over X -> publish mu -> uniform barrier
    cooperative X -> d -> shared row -> uniform barrier
    lane-zero variance over shared d -> publish q -> uniform barrier
    cooperative shared d, gamma, beta -> Y -> reader-completion barrier

C2: cooperative X -> shared row -> uniform barrier
    lane-zero mean over shared X -> publish mu -> uniform barrier
    cooperative shared X -> d in SAME arena -> uniform barrier
    lane-zero variance over shared d -> publish q -> uniform barrier
    cooperative shared d, gamma, beta -> Y -> reader-completion barrier
```

Cooperative work uses independent warp actors. `k=tile*T+warp*W+lane` covers
valid elements exactly once. Every group executes predicate/control service;
invalid lanes execute no memory/arithmetic. All warps, including wholly inactive
ones, participate at the join/barrier. Mu/q are stored once each and broadcast
once per active warp, hoisted across that warp's feature iterations.

| Ledger for R rows of N features | C0 | C1 | C2 |
| --- | --- | --- | --- |
| External logical bytes | `24RN` | `20RN` | `16RN` |
| Scalar lane operations, counting div/sqrt separately in timing | `R(8N+5)` | `R(7N+5)` | `R(7N+5)` |
| Row SMEM bytes | 0 | `12RN` | `24RN` |
| Additional mu/q SMEM bytes | 0 | `R*(8+8*ceil(min(N,T)/W))` | Same as C1 |
| Uniform barriers | 0 | `4R` | `5R` |

Project external sectors from each actual contiguous active-lane interval,
including range offsets and row strides. Where base sector alignment is not
known, take the worst projection over bases satisfying the declared guarantee.
No cache-hit discount is applied. The reference envelope treats these sectors
and private carrier sectors as all-miss global service; it deliberately does
not claim realistic cache/HBM behavior.

The closed pointer-carrier route packages register-held scalar values through
sector-aligned, lane-coalesced F32 private scratch. A binary operation charges
two input stores, two ASM loads, result store and unpack load; sqrt charges
one input store/load and result store/unpack load. These accesses share the
device-global byte engine in this pessimistic model. This is an explicit future
lowering contract, **not proof of generated local-memory placement or spills**.
LN4 must implement/verify it or invalidate and recost the plan with a better
explicit register carrier. Retained rows are SMEM, never uncosted RF rows.

RF reservations are fixed per-thread analytical estimates. Reuse the existing
pure `DSL_gemm_occupancy` resource-estimation helper, not GEMM's arithmetic,
readiness cost formula or selector. It changes no phase scope or state. Round per-warp
registers with the common register quantum and warps with the common warp
quantum. Resident CTAs are the minimum block/thread/warp/RF/available-SMEM
limit. Add the common **per-CTA** shared overhead to the LN2-rounded arena,
then round with the resource quantum (distinct from the LN2 arena quantum).
Unknown RF estimates block ranking; known resource-infeasible candidates are
rejected and do not prevent comparing the remaining legal complete candidates.
An infeasible C0 cannot serve as fallback.

## 5. Schedule And Complete Costs

Build a finite DAG of explicit control, memory, scalar, barrier and join jobs.
Ordered lane-zero operations depend on their preceding operation; warp actors
remain independent until the required visibility/reader join. A row maps
deterministically to a resident slot/SM; later waves reuse a slot only after
its previous CTA's completion. SM underfill is represented by absent work,
not an arbitrary occupancy multiplier.

This fixed assignment is a deterministic analytical assumption, not a promise
of programmable SM affinity or the GPU's actual CTA dispatch order. Bounds and
rankings apply to this declared model; native scheduling/calibration must test
that assumption before physical performance claims.

```text
for each job whose data predecessors have scheduled completion times:
    ready = latest predecessor completion
    start = max(ready, required engine's service end)
choose the smallest (start, stable_job_id)
service_end = start + byte_or_issue_service(job)
finish = service_end + memory_startup(job)
         or start + max(issue_service, scalar_latency)
release engine at service_end, not finish
enqueue successors when their predecessor times are all known
```

One device-global bandwidth engine is shared by external and carrier traffic.
Each SM has scalar/control, shared-byte and synchronization engines. All rates,
latencies and overheads come from the explicit hardware envelope. Independent
warps/CTAs can overlap service and delivery; no second traffic-score overlap
credit is subtracted.

Propagate separate service-end and completion events. A deterministic critical
predecessor carries exclusive time attribution; walking the selected critical
path gives exactly:

```text
T_total = compute + memory_unhidden + communication_unhidden
        + sync_control_publication + launch_workspace + guard
communication_unhidden = 0       # single-device contract
```

Resource waits lead back to the operation actually occupying that resource;
memory startup is not double-charged as an independent synchronization penalty.
The nominal decomposition reconciles to the same scheduled total.

Independent envelope bounds avoid assuming that greedy list scheduling is
monotone under timing changes:

```text
lower = max(fast-corner dependency path,
            fast-corner per-engine total service + setup) + publication
upper = slow-corner setup + sum(all slow-corner job durations) + publication
```

The serial upper bound deliberately sacrifices overlap and can retain C0 even
when C1 wins nominally. It is a safe bound for this finite analytical model,
not a physical GPU confidence interval. All u64 arithmetic is checked.
Expansion is bounded by the request and a compiled one-million-job ceiling;
budget exhaustion is an explicit incomplete result, not a zero-cost candidate.
Scalable symbolic/repeated schedules beyond that expansion budget are a later
model extension; a large LayerNorm cannot silently inherit a small-shape cost.

## 6. Cost Gate And Common Records

```text
rebind and verify all three comparable source/target/storage/event requests
refuse invalid API, scope, member or cross-plan invocation mismatch unchanged
derive every legal candidate's complete cost or explicit unknown/refusal
if C0 is unavailable: no selection, no executable fallback
if any still-eligible candidate has unknown/invalid/budget/overflow cost:
    no cost selection; preserve only conditional C0 planning fallback
else:
    nominal = minimum (nominal_total, C0/C1/C2 order)
    robust = alternatives with upper + margin < lower(C0), checked arithmetic
    selected = minimum nominal cost among robust alternatives, else C0
    record ANALYSIS_ONLY candidates/costs/plans and deterministic selection
    freshly export selected native guard
```

Do not examine only the nominal winner: a different alternative may have a
robust benefit when the nominal winner's bounds overlap C0. Ties retain C0;
margin addition cannot wrap. Every retained candidate references C0 through
the existing common fallback service. Refusals remain visible separately.

Selections are conditional analyses, never runtime authorization, instruction
certification or automatic performance promotion. Preserve the request/profile
and selected analytical recipe for LN4 revalidation; a runtime guard alone is
not an executable schedule. No source-v1 promotion or mapped state is added.

## 7. Diagnostics And Verification

| Report | Required summary |
| --- | --- |
| `physical-plan.log` | Every C0/C1/C2 status, resource count, traffic/arithmetic/control/barrier ledger, lower/nominal/upper, reconciled components, common IDs, nominal winner, selected/fallback and exact reason. |
| `bubble.log` | Modeled critical-path memory/compute/control exposure and finite-residency/carrier facts; not measured GPU waiting cycles. |
| `roofline.log` | Candidate-specific scalar-operation intensity over all-miss global+carrier sectors, common-profile memory/scalar service lower bounds versus scheduled kernel time; division/sqrt separately scheduled. Not Tensor Core FLOPs or measured HBM bandwidth. |
| `optimization.log` | Full successful contents of all three phase reports. |
| `optimization-summary.log` | Complete/incomplete/refused and nominal/gated census; built-in timing incomplete, estimated resource envelope, analysis-only selection, preserved v1, TRANSFORM none and open execution/provider/GPU gates. |

All logs begin `# What it summarizes:`. Raw `.t` records use compact positional
model/resource/cost/selection/selected-guard schemas with native source line
last. Model field IDs, units/provenance and reason legends are explicit.
New tools independently join actual PREOPT facts, native LN2 plans and guard
bytes; a Python frontier-scan oracle differs from the native heap/requeue
scheduler and propagates component vectors instead of native backpointers.

Native review adds single-row and 129-row cases to exercise robust C1 benefit
and later residency waves, without changing existing source coordinates or
v1 semantics. Synthetic profile scenarios exercise C0/C1/C2 preferences, ties,
uncertainty/margin, each missing/invalid timing field, resource pressure,
capacity/budget/source refusals and overflow. Native quiet/trace parity,
table immutability, original/preserved `.B`, full separate-process `.T` and
PREOPT trace controls remain mandatory. Sanitizers cover the new scheduler
and native fixture, not the entire compiler. Retain immutable receipts and
independent final-path replay before indexing.

## 8. Next Gate

Bounded LN3 acceptance, implementation `d5da1c1d`: 40 native cases, 728
comparisons / 2,184 candidate records, 1,100 complete costs, 80 full mapped
dumps and 160 actual PREOPT controls. The independent oracle agrees, all 108
focused tests and 40 scoped sanitizer cases pass, and three shared-service
regressions plus 136 host guard invocations pass. Immutable final-path replay
reproduces cost/source/guard/report bytes and repeats validation. The
[progress ledger](AI-COMPILER-OPTIMIZATION-PROGRESS.md) records primary and
replay identities. Synthetic experiments cover C0, C1 and C2 gated choices;
the built-in incomplete profile correctly selects none.

LN4 must prove the selected algorithm's executable WHIRL, scalar-carrier
storage/arithmetic, loops/predicates, uniform barriers, actual registers,
entry manifest, launch and runtime binding. A CUDA provider, native numerical
execution and timing/calibration remain open. Improving cache/carrier models,
new parallel statistics or async/TMA paths requires explicit new contracts,
not interpretation of this bounded analytical result as GPU certification.
