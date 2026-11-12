# AI compiler optimization search methodology v1.0

2026-11-12 architecture terminology correction:
[native WHIRL contract](AI-COMPILER-WHIRL-OPCODE-AND-TENSOR-TRAIT-CONTRACT-v1.0.md)
governs opcode properties, tensor traits, phase legality and scheduling
services. Embedded earlier revisions include these editorial corrections;
historical implementation status and acceptance evidence retain their original
scope.

November 7, 2026 | Adopted design; production extensions remain planned

This document defines how Open64 concludes optimization search, compares search
strategies, and admits a selected implementation. It extends the implemented
[MLA two-phase contract](AI-COMPILER-MLA-TWO-PHASE-SEARCH-v0.1.md) and governs new
work under [design v0.7](AI_compiler_optimization_design_v0.7.md). The companion
[implementation plan](AI-COMPILER-OPTIMIZATION-SEARCH-IMPLEMENTATION-PLAN-v1.0.md)
separates existing evidence from future acceptance gates. Earlier architecture,
source numerical permissions, AOT defaults and binary WHIRL compatibility remain
binding. This is a documentation revision, not a new compiler implementation.

## 1. Decision and completion contract

The objective is to select a useful implementation within an explicit search
contract. Proving a global GPU optimum is not a release or convergence
requirement. A search is complete when its declared stopping policy terminates,
the best eligible explored candidate is retained, and the report states the
scope and limitations of the result. Deployment acceptance is a separate gate.

Every search contract identifies source/IR generation, compilation scope,
numerical policy, workload or workload distribution, target/device resources,
candidate-space version, model/profile versions, objective, enabled strategies,
budgets, stopping rules, tie policy and selected-plan validation requirements.
Changing any of these produces a new search identity; it does not retroactively
improve the evidence for an older result.

Keep two incumbents: the best estimated eligible candidate in the current
search, and the previously accepted executable baseline. A candidate can replace
the first immediately after estimation. It replaces the second only after the
applicable correctness, resource and deployment gates pass. A newly implemented
kernel is a candidate; declaring it the baseline does not establish convergence.

## 2. Two phases and the cost of exploration

Phase 1 performs candidate proposal, legality/resource preflight, estimation,
strategy comparison and stopping on detached plans. It reads an immutable
snapshot of canonical IR facts. It does not rewrite executable IR, allocate
candidate WN/ST/TY/DST entries, emit candidate B/CUDA/PTX, invoke a backend,
compile kernels or execute a regression suite per candidate. Existing compiler
analysis can inspect the active PU; its immutable summary is built once and
its cost is reported separately from candidate evaluation.

The standalone MLA tool demonstrates zero native compiler initialization during
exploration. An integrated backend already has native tables and a current PU;
its corresponding guarantee is zero candidate-induced native construction and
no executable-IR mutation, not zero process-wide initialization. Snapshot,
estimator and integrity-replay work have separate counters.

Phase 2 seals one selected plan, applies its High WHIRL transformations, checks
the High exit contract, lowers memory operations and GPU mapping, and validates
the resulting implementation. Generating a readable optimized source view after
selection and before physical lowering is permitted. It is a view of the fixed
plan, not an additional optimizer. The lowered WHIRL remains the authoritative
source for device CUDA emitted by whirl2c.

For N unique estimated candidates and K selected materialization attempts:

    old work = N * (estimate + transform + compile + correctness)
    new work = snapshot + N * estimate + K * (transform + compile + correctness)

K is normally one per workload/target contract. Multiple stage kernels and
ordinary versus COMMENT-checkpoint construction of the same plan are not new
candidate configurations; report their actual construction counts separately.
New transformation rules still need development-time correctness tests before
their parameterized configurations enter normal estimation-only search.

## 3. Objective, candidate space and numerical meaning

Let S be the finite, versioned set of candidate descriptors and F the subset
admitted by numerical, dependence, capacity and resource checks. For workload x,
device envelope d and model m, choose:

    c_est = argmin over c in F intersect explored of T_hat(c; x, d, m)

T_hat is an estimate of the complete operation or declared pipeline boundary,
including projections, copies, attention, partial tuples, merges and launches.
Do not compare one candidate's attention stage to another candidate's whole
call. For a declared workload distribution use sum_x weight(x)*T_hat(c;x,d,m),
with normalized weights and the same boundary for every candidate. Independent
short/long variants remain independently selectable unless a dispatch objective
explicitly includes both, their selection overhead and code/storage limits.

Candidate descriptors can name an admitted algorithm family, affine schedule,
tile/chunk sizes, thread/warp ownership, staging/prefetch plan, memory layout,
and numerical realization. A descriptor is searchable only when its legality,
costing and selected-plan materializer exist. Listing a theoretical combination
does not make it a supported implementation. Rejected and unvisited entries
remain distinguishable from eligible entries; partial rejected costs cannot win.

The existing MLA space contains 12 descriptors: direct; coverage-v6 widths
32, 64, 128, 256, 512, 1024, 2048 and 4096; shared-statistics width 32 and width
64; and disjoint-statistics width 64. The width-64 coverage-v6 realization is
the one-barrier warp-owned tuple algorithm. All retain the current F32 contract.
New precision, tensor-core or algorithm families require a new admitted space.

Architecture options supply instruction and resource facts where known. They
do not by themselves fix cache behavior, effective bandwidth, contention,
operating clocks or all pipeline stalls. Explicit static latency/throughput
profiles can support estimation without JIT or execution. Unknown inputs remain
unknown. Synthetic, estimated, measured-input and calibrated-model provenance
are distinct; compatible units are required before comparing or summing costs.

## 4. Permitted conclusions

| Result | Required evidence | Limit |
| --- | --- | --- |
| Best estimated among explored candidates | Minimum complete estimate among visited eligible entries | Unvisited entries and model error remain |
| Best estimated in the declared space | Every descriptor admitted/rejected and every eligible estimate compared | Minimum of this model over this space only |
| Best measured among tested candidates | Comparable hardware measurements for those implementations | Tested set, workloads and device conditions only |
| Within an estimator gap | A valid lower bound on every eligible model cost and the stated gap test | No physical GPU guarantee |
| Within a hardware performance bound | Independently justified hardware lower bound with applicable assumptions | Only its explicit semantic and hardware scope |

The initial estimator has L_model = 5*launch_cycles because every current
realization contains at least five modeled launches and nonnegative additional
costs. If best_est <= (1+epsilon)*L_model, its relative gap to the model optimum
is at most epsilon. A model or search-space change invalidates that bound until
reproved. This is not a hardware latency lower bound.

No report uses global_GPU_optimum without a proof covering its stated universe.
A probabilistic confidence claim additionally requires a calibrated uncertainty
provider, assumptions, validation data and an explicit error probability.
Deterministic synthetic scores do not provide such a posterior. Missing proof
or uncertainty support disables those stopping modes; it does not prevent a
budgeted, plateau-based or exhaustive search from completing.

NVIDIA's cuDNN documentation provides useful scope terminology: its heuristics
rank configurations by expected performance, and autotuning measures listed
configurations for a particular problem/device. This design adopts similarly
explicit scope; it does not infer a universal NVIDIA optimality guarantee.
[cuDNN Graph API heuristics](https://docs.nvidia.com/cudnn/latest/developer/graph-api/#heuristics)

## 5. Search strategy portfolio

Strategy determines which descriptor to propose next. Stopping policy determines
when to end a run. The existing prototype supplies exhaustive stable traversal,
seeded random traversal without replacement, structural local search with
global restarts, and a sequential portfolio of those three. All use one model
and one cache of unique candidate estimates. The portfolio winner is the minimum
eligible estimated cost across the union of runs, with stable candidate-ID ties.

Enable every supported strategy when its overhead fits the declared budget.
Retain each strategy's proposal sequence, incumbent trajectory, cache hits,
restarts, first-best proposal and termination reason. Running more strategies
does not require lowering each strategy's winner. Phase 2 receives only the
portfolio winner. Future beam, evolutionary or Bayesian providers are optional
extensions with separate admission, overhead and evidence gates.

For production, distinguish per-strategy proposal limits, total proposal limits,
unique-estimate limits and optional elapsed-time limits. A cache hit consumes a
proposal but not another unique estimate. The existing max_evaluations field is
a per-run proposal cap despite its historical spelling. An explicit global cap
and fair interleaving require a versioned future options/report extension; they
are not implemented by the current sequential portfolio.

Comparing strategies has two different uses. Within one workload, select the
best candidate any run finds. Across workloads, compare search methods using
equal proposal or unique-estimation budgets, explicit cache treatment and total
search cost. The current representative_strategy field identifies a tied-best
run with the earliest first-best proposal; it is not proof that this method is
fastest or strongest in general. A strategy benchmark must neutralize execution
order/cache benefits or charge a common virtual estimate cost, and include
multiple seeds and held-out workloads before changing defaults.

## 6. Stopping policy and convergence

The current tool chooses one stopping mode plus its bounded per-run budget.
Production may add explicit combinations of supported predicates; this requires
a schema revision. A hard safety budget always limits work. With a finite
declared space, complete union coverage establishes declared-space completion
even when individual strategies stopped earlier. Record all satisfied
predicates at the terminal snapshot and a stable primary termination reason.

| Mechanism | Decision | Meaning and availability |
| --- | --- | --- |
| Exhaustion | Every declared descriptor has a final admission/estimate result | Implemented; best estimated in the declared space |
| Budget | Proposal, unique-estimate or elapsed-time limit reached | Per-run proposal limit implemented; additional global limits planned |
| Plateau | No gain greater than minimum_gain_cycles for patience proposals | Implemented empirical stagnation; does not prove no better candidate |
| Estimated target | Best estimate reaches the declared target | Implemented; target met under this model |
| Estimator relative gap | best_est-L_model <= epsilon*L_model | Implemented model certificate with checked arithmetic |
| Cost-benefit bound | remaining_invocations*(best_est-L_model) <= evaluation_cost | Implemented conservative bound after compatible unit conversion |
| Probabilistic improvement | Calibrated probability of worthwhile unseen improvement below a declared threshold | Unavailable; uncertainty provider required |
| Hardware bound | Selected measured result meets a valid hardware-bound gap | Unavailable; independent proof and measurements required |

Plateau includes rejected proposals and cache hits under the existing rule;
minimum_gain affects stagnation reset, not whether a lower-cost incumbent is
retained. An estimated target is not a measured latency target. The cost-benefit
rule compares an upper bound on total remaining modeled deployment benefit with
the cost of another declared unit of exploration; host time and device cycles
must not be mixed without a recorded conversion. A time-limit stop retains the
actual visited prefix; it need not be byte-identical across executions.

For the current 12-entry MLA space, use complete estimation as the default:
portfolio, stop=exhausted, max_evaluations=12 per run and seed=17. It has at most
36 proposals, 12 unique estimates and 24 cache hits when every run exhausts.
This is inexpensive and removes traversal quality from final model selection.
For enlarged spaces, configure budgets and a useful stopping predicate before
search; record whether the result is complete or only best among explored.

Do not call a plateau, new incumbent, budget expiry or implementation freeze a
global convergence proof. A search may finish successfully with a qualified
result and deployment validation pending. It resumes only under an explicit
continuation budget or a new contract/model/space, preserving the old report.

## 7. Selected-plan acceptance and bounded recovery

The handoff binds source/IR generation, numerical permissions, target resources,
model/profile inputs, candidate-space version, complete explored ledger, selected
candidate and the stopping result. The standalone v7 recipe already carries
sealed search evidence and an opaque native search identity. Production typed
records must provide equivalent identity without JSON parsing in optimizer
policy or a new unapproved be.so dependency.

Apply only the selected plan. Recheck destination legality, expected generation
and resource preconditions before mutation. A failed application restores the
original IR and analyses or fails closed before publication. A correctness
failure is a compiler/rule defect: retain diagnostics, quarantine the affected
rule for investigation and keep the accepted baseline. Do not silently test the
next candidate until one happens to pass.

A late resource mismatch may trigger an explicitly bounded reselection after
correcting the resource/model snapshot. Reuse candidate descriptors from the
retained ledger, invalidate affected admission/cost results and re-estimate them
before reselection. Default future policy permits at most one additional
selected configuration, for two total attempts across the optimization request,
including revised snapshots; no automatic retry exists in the current standalone
tool. Each attempt has a distinct report and reason. Exhaustion retains the baseline
or reports unsupported lowering. Whirl2c and the backend cannot silently choose
a new algorithm, width, precision, ownership or stage pipeline.

Selected correctness validation covers the reference scenarios, tails, masks,
numerical guards, race/synchronization obligations and emitted-code fidelity.
Actual registers, shared memory and spills are observations, not estimates.
Deployment performance acceptance compares the selected implementation with the
accepted baseline using an advance-declared whole-call metric, improvement and
regression limits, measurement conditions and noise policy. A failure to beat
the baseline retains the baseline and records model disagreement; it does not
start an unbounded transformation loop. Without hardware, record compiler/host
acceptance and GPU performance pending. Never infer best measured from one
selected candidate's correctness or successful CUDA compilation.

## 8. WHIRL architecture and scheduling boundary

| Level | Required ownership |
| --- | --- |
| Very High | Domain operators, domain legality and numerical contracts; promote to common algorithms after admission |
| High | Canonical common IR, DO_LOOP/OPR_ARRAY indexing, PREOPT canonicalization, LNO access/dependence facts; detached scheduling search followed by selected multi-level tiling, SMEM staging and prefetch DO_LOOP construction |
| Mid | Realize selected structured memory operations, address calculations, copy issue/completion, visibility, synchronization and buffer reuse |
| Low | Realize fixed CTA/warp/lane mapping, address spaces, register/fragment constraints and target operations, comparable in purpose to GPU-specific lowering such as TTGIR |
| PTX | CG/NVISA instruction expansion and PTX emission, followed by separately verified assembly/runtime packaging |

An affine candidate may be a Wolf unimodular map U*i with integer U and
det(U)=+1 or -1, preserving the integer lattice. It must still preserve ordered
dependences. General polyhedral scheduling admits statement-specific affine
timestamps theta_s(i)=A_s*i+b_s constrained by lexicographic dependence order;
it can express a wider space than one shared invertible square matrix. Tiling,
strip mining and expanded schedule dimensions require their own representation
and legality, not the assertion that every transformation is unimodular.
An optional hybrid proposes unimodular schedules first and wider affine
schedules within the same detached budget. Neither proposal method mutates
WHIRL. Nonaffine or unsupported cases retain an admitted existing recipe or a
diagnostic; no dependency proof is invented to expand the space.

The scheduling interface consumes stable loop/access IDs, canonical bounds,
dependence/alias facts and their generation, numerical permissions, storage
obligations and target/model snapshots. It returns detached plans with complete
costs, legality reasons and a selected application recipe. Only the owning phase
applies that recipe. A phase exit verifier checks allowed logical operators,
forms, types and conditional legality using WHIRL opcode/version contracts,
tensor descriptors, tensor traits and explicit contract predicates. The owning
Open64 phase checks dependence, alias, numerical and target obligations; an
unknown obligation requires a verified guard/fallback or refusal.

The High exit verifier requires explicit loop bounds/tails and tensor indexing;
legal selected dependence order; explicit storage/address-space obligations;
staging/prefetch issue, completion, visibility and reuse order; fixed mapping
constraints; complete numerical/mask semantics; and no unresolved domain
operators or missing executable decisions. Mid/Low lowering can refine how a
fixed operation is realized but cannot reopen algorithm selection.

Applying a schedule invalidates affected access/dependence/reuse analyses, DU,
alias/SSA facts, loop summaries, placement/resource estimates and empirical
feedback according to the actual rewrite. Rebuild through the owning PREOPT,
LNO or WOPT services before their next consumer; preserve only proven unaffected
facts. Read-only estimates do not invalidate executable-IR analysis. Model-only
updates invalidate rankings and bound certificates, even with unchanged IR.
Driver-owned active-PU/REGION scope and opt-level controls remain mandatory:
this loop search requires O3 and explicit reassociation where applicable.

## 9. Evidence, calibration and handoff

Retain source and compiler/tool identities, contract snapshots, candidate-space
census, component estimates/rejections, per-strategy trajectories, budget use,
termination predicates, winner identity and claim scope. Separately retain
selected B/T before-and-after images, raw ir_b2a -st -src diffs, readable CUDA,
normal-whirl2c CUDA, matching COMMENT payloads, actual resources and reference
results. Count unique estimates, proposals, cache hits, selected configurations,
stage materializations, correctness scenarios, compilations and GPU executions.

Calibration is a separate offline activity using an explicit representative
corpus and held-out data; it is not per-candidate execution in Phase 1. Estimate
sensitivity across declared timing/resource scenarios is also estimation-only.
Neither sensitivity nor a measured input parameter makes an entire model
calibrated. Model updates create a new snapshot and rerank detached candidates
before another bounded selected-only validation. The broader feedback framework
remains governed by its existing PF gates; this document does not implement it.

The v0.27 receipt from commit 1abdd5957623c54a3dea54b6af95e0d493de09a9 is the
existing standalone baseline: 24 unique estimates across two fixtures; zero
Phase-1 native initialization/materialization; two selected configurations;
38 compiler/search tests, 34 compatibility cases, 14 readable and 14 mapped-host
reference cases, and four CUDA compilations. It uses a synthetic model; GPU
execution and production be_driver integration remain open. This revision does
not rerun or upgrade that historical evidence.

The minimum transferable package includes this decision record, space/model
definitions, source and exact commands, selected examples, invariants, test
matrix, unresolved risks, immutable hashes and recovery/replay instructions.
Primary owner, successor, independent reviewer and approving lead remain TBD
as requested; technical completion and personnel transfer acceptance are
separate statuses. Preserve every existing immutable review receipt.
