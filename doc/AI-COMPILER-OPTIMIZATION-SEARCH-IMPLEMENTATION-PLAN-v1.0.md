# AI compiler optimization search implementation plan v1.0

November 7, 2026 | New production plan; no new source implementation claimed

Authority: [search methodology v1.0](AI-COMPILER-OPTIMIZATION-SEARCH-METHODOLOGY-v1.0.md)
and [optimization design v0.7](AI_compiler_optimization_design_v0.7.md). This plan
decomposes the open S7 production/calibration gate in the
[standalone MLA plan](AI-COMPILER-MLA-TWO-PHASE-IMPLEMENTATION-PLAN-v1.0.md).
S0-S6 remain the completed prototype baseline; the gates below do not rename
them or imply H0-H8/PF0-PF7 completion. Work remains AOT, opt-in, O3 and PU-local.
No new compiler-option spelling, opcode, type kind or binary layout is reserved
by this document.

## 1. Baseline and immediate sequence

Commit 1abdd5957623c54a3dea54b6af95e0d493de09a9 implements detached MLA
estimation, exhaustive/random/local portfolio search, six stopping modes and
the fixed v7 selected-recipe handoff in the standalone tool. It already lowers
and tests only the selected configuration per fixture. Retain the v0.27 receipt
and its 38 passing tests, 34 compatibility cases, 14+14 reference checks and
four CUDA compilations as historical evidence, with synthetic model and no GPU
execution. This plan adds concrete production integration and evaluation gates.

First execute M1, the actual driver/analysis boundary audit. Then implement M2
read-only snapshot plumbing and M3 search controls before M4 selected schedule
application and M5 verified lowering boundaries. Run M6 focused compiler
acceptance before closing host integration. M7 native GPU acceptance and M8
search-strategy evaluation need hardware/data; they do not block completion of
an honestly scoped estimation-only search. M9 closes production/transfer only
with its required evidence. No additional MLA kernel micro-optimization is a
prerequisite for this methodology.

## 2. Milestones, outputs and acceptance

| Gate | Status and dependency | Concrete output and acceptance |
| --- | --- | --- |
| M0 specification | Complete in this documentation revision | Versioned methodology, main design and new plan; scoped result labels, selected-only rule, recovery policy and status/evidence distinctions |
| M1 driver and analysis audit | Open; M0 | Call/IR-level map for the current driver and LNO entry/exit actions; active PU/REGION lifetimes, current generation/analysis APIs, optimization controls and device-PU ownership. Identify the canonical High point before destructive lowering; do not select a hook by function name alone |
| M2 immutable search snapshot | Open; M1 | Typed source/loop/access/dependence/numerical/target/model snapshot, stable identities and change detection; reuse detached admission/occupancy/checked arithmetic. Prove no candidate-induced native construction or executable-IR mutation in an initialized backend |
| M3 portfolio and convergence service | Prototype subset complete; production work open after M2 | Reuse existing providers; add explicitly versioned global/proposal/unique/time caps, fair scheduling and composite stopping when needed. Preserve the existing CLI/schema. Reports retain all predicates, per-run trajectories and scoped union completeness; complete 12-entry estimation remains default |
| M4 selected High application | Open; M2/M3 | Typed fixed-plan admission; preflight current generation/legality/resources; atomic application through owning High services; invalidation/rebuild manifest and selected-only counters. Failed application publishes no partial output; correctness defects do not trigger silent candidate trials |
| M5 High exit and Mid/Low bridge | Open; M1/M4 | Verify structured selected algorithm, loops/ARRAY, storage, SMEM/prefetch obligations and numerical semantics; Mid lowers memory operations, Low realizes fixed GPU mapping, CG emits PTX. Readable/COMMENT/mapped-source explanations agree; emitter cannot reopen scheduling |
| M6 compiler integration acceptance | Open; M2-M5 | Focused positive/negative, level/scope, rollback, stale-plan and compatibility evidence; selected short/long before/after B/T/diffs and reference checks. Development-rule tests are independent of candidate iteration. No GPU performance claim from host results |
| M7 offline calibration and native acceptance | Open; M6 and GPU | Explicit calibration corpus and held-out shapes, versioned timing/model provenance, selected-only GPU correctness/sanitizer/resource checks and paired baseline timing. Declare noise, improvement and regression limits before measuring; preserve baseline on failure |
| M8 strategy evaluation and optional providers | Open; M3; GPU optional for model-only study | Equal-budget, cache-neutral, multiseed comparisons of candidate quality and search overhead on held-out workloads. Separate estimated from measured regret. Admit hybrid affine, beam/evolutionary/Bayesian providers only with contracts and evidence; no probabilistic stopping without calibration |
| M9 production and transfer acceptance | Open; M6/M7 plus chosen M8 scope | Opt-in production path, explicit baseline/fallback behavior, immutable reproducible packet, scoped performance results and closed required H/PF dependencies. Owners/reviewer/lead remain TBD; personnel handoff acceptance remains open until assigned |

## 3. Actual source work map and phase ordering

The backend driver source is `osprey/be/be/driver.cxx`; be_driver is the component
name, not a separate be_driver.cxx in this checkout. The current source has
Preprocess_PU, Backend_Processing, LNO_Processing and
Do_WOPT_and_CG_with_Regions. Preprocess_PU runs optional DSL PREOPT/shape work,
DSL lowering and VHO_Lower_Driver; Backend_Processing enters LNO_Processing
before Post_LNO_Processing and WOPT/CG. LNO_Processing dispatches to
Perform_Loop_Nest_Optimization or the PREOPT-only path. These are inspected
anchors, not evidence that the proposed search has been integrated there.

The required production sequence is:

1. Driver establishes the active PU/REGION, target/resource/profile snapshots
   and optimization/numerical controls. Very High gatekeeping discharges
   domain legality before promotion. Preserve unrelated DSL/FHE paths.
2. Promote to common High algorithms and canonical DO_LOOP/OPR_ARRAY indexing.
   Use PREOPT's established canonicalization service. In M1 inspect VHO and
   LNO lowering action masks so the required form survives to its consumers.
3. Under LNO ownership obtain current bounds, access, alias/dependence and
   reuse facts. Snapshot their generation and immutable search inputs. A
   direct hook after LNO_Processing is insufficient unless M1 proves that
   required High forms and analysis lifetimes still exist there.
4. Run detached strategy proposals, lowerability preflight and estimates.
   Stop under the declared policy, select the best eligible estimate across
   all runs and seal the selected plan. Emit no candidate native artifacts.
5. Recheck the current snapshot, apply only the selected High schedule,
   multi-level tiling, staging and prefetch DO_LOOPs, rebuild invalidated
   analyses, then verify the High exit contract. Driver registers any selected
   generated device PUs through its explicit ownership service; a PU-local
   optimizer does not traverse or switch into other PUs itself.
6. Lower selected structured memory operations in Mid. Realize fixed GPU
   mapping and target operations in Low. Respect existing REGION/WOPT/CG
   contracts; the LOWER_TO_CG route must not see unresolved tensor/domain
   semantics or missing synchronization. Native PTX is the production backend
   objective; normal whirl2c CUDA remains a review/validation consumer.
7. Audit actual resources, selected identity, source/COMMENT fidelity and
   correctness. Accept or retain the existing executable baseline according
   to the declared deployment gate. Preserve instrumentation and FEEDBACK
   verification at their established boundaries.

M1 must document any conflict between this order and existing destructive
lowering. Resolve it through H0-H8's reviewed phase boundaries, rather than
moving algorithm selection into Mid/Low or duplicating LNO dependence analysis.

| Source/service | Work responsibility |
| --- | --- |
| `osprey/be/be/driver.cxx` | Scope, controls, snapshot lifetime, selected output publication, device-PU registration and bounded recovery |
| `osprey/be/vho/tensor_attention_search.h/.cxx` | Reuse existing typed detached MLA costing/providers; generalization requires a reviewed owning-phase API, not policy in common/com |
| `osprey/be/vho/tensor_kernel_lower.h/.cxx` | Existing detached plan admission and selected materialization; adapt to typed production facts while preserving historical policies |
| `osprey/ir_tools/tensor_kernel_driver.cxx` and `tensor_attention_search_json.inc` | Standalone archival JSON/admission/replay; preserve CLI and v1-v7 compatibility; do not copy JSON policy into be.so |
| `osprey/be/lno` and PREOPT/WOPT services | Canonical loop/access/dependence facts, schedule application and analysis rebuild in the correct scope |
| `osprey/be/com/wn_lower.cxx` and target CG/NVISA | Audit action masks, ordered memory and mapping realization; implement only fixed selected semantics |
| `osprey/be/vho/tensor_kernel_explain.cxx` and whirl2c | Retain generator-owned explanations and selected metadata fidelity; no new optimizer in the emitter |
| Existing FEEDBACK/physical profile services | Govern later empirical import/calibration by PF gates; no new generic telemetry framework in this increment |

## 4. Interfaces and versioned data

M2/M3 define typed contracts for snapshot creation, proposal, admission, estimate,
stop evaluation, winner selection, sealed-plan application and result reporting.
Use existing WHIRL opcode/type/tensor traits and verifier predicates. No MLIR
operation-interface/traits adoption is needed. New backend interfaces follow
the repository's naming and function-contract conventions; an outline here is
not an approved symbol list or library dependency.

The production snapshot includes stable source/loop/access identities, source
hash/IR generation, active scope, dependence/alias generations, numerical
permissions, workload objective and normalization, candidate-space ID, target
resource and model/profile identities. Candidate costs include all stages and
units; rejections carry reasons; unvisited records carry no fabricated cost.
Cache keys include every input that can affect legality or cost. Model-only
changes invalidate rankings/bounds; IR/resource changes invalidate admission too.

A future options/report revision distinguishes per-run proposals, total
proposals, unique estimates and elapsed time, names the composition of stopping
predicates and records every trigger. Keep the current single-mode
open64.mla-search-options.v1 behavior unchanged. The current
representative_strategy remains descriptive; do not rename it as a universal
strategy winner. Typed production plans and future reports need equivalent
binding to the sealed standalone evidence without changing binary WHIRL layout.

M4's application result records input/output generation, changed loops/accesses/
buffers, analyses preserved with reasons, analyses invalidated/rebuilt, selected
identity, actual construction counts and failure/rollback status. Empirical
latency/resource observations remain attached to measured code; they cannot be
copied as observations of a new candidate. Frequency FEEDBACK propagation and
physical-performance evidence have separate preservation rules.

## 5. Test and experiment matrix

| Contract | Required focused evidence |
| --- | --- |
| Estimation isolation | Same executable IR/table generations before/after Phase 1; zero candidate WN/ST/TY creation; no B/CUDA/PTX/object output or backend/test invocation. Existing snapshot initialization counted separately |
| Model arithmetic | Independent component/count checks; unit mismatch, unknown timing, overflow, no eligible candidate, resources on every stage, tails/merge/copy/projection costs and proof of estimator-bound applicability |
| Portfolio fairness | Same legal space/model; stable seed/ties; exact cache/proposal/unique counts; per-run and union minima; each enabled strategy receives its declared opportunity unless an explicit global limit terminates the portfolio |
| Stopping | Exhaustion, insufficient budget, no early eligible incumbent, plateau/minimum-gain semantics, target, estimator gap and compatible-unit cost-benefit; proposed composite/global/time cases under their new schema; unavailable evidence modes rejected |
| Invalidation | Stale IR/dependence/numerical/target/model/space snapshot rejection; correct analysis rebuild after selected transformations; model-only update reranks without IR mutation |
| Selected application | K=1 ordinarily; one winner with multiple stages distinguished from candidate count; injected failure restores original state or fails before publication; bounded resource retry logged; correctness defect retains baseline and fails acceptance |
| High exit/lowering | Domain/common forms, DO_LOOP/ARRAY bounds, affine legality, SMEM/prefetch ordering, storage lifetime and numerical contracts; Mid/Low cannot change width/algorithm; unsupported semantics diagnose |
| Controls and compatibility | O0 semantic-only and opt-out unchanged; pass admitted only at its declared O3/scope boundary; active-PU-only mutation; standalone old schemas/policies and ir_b2a/whirl2c compatibility |
| Selected correctness | Short/long, mask/tail/all-masked scenarios, stable rescaling/merge, FP64 oracle, canaries/immutable inputs and forward/reverse workers; actual selected CUDA compilation and exact COMMENT payload parity |
| Native acceptance | Selected GPU reference and synchronization/sanitizer checks; actual registers/shared/spills; matched baseline/selected whole-call timing, raw samples/conditions and declared acceptance thresholds |
| Strategy study | At equal budgets report final estimated quality, anytime trajectories, proposal/unique/time overhead, restarts and success across seeds/workloads. Use exhaustive model optimum where tractable; measured regret requires actual comparable measurements |

Reuse impact-selected rule tests once per changed transformation/compiler
contract. Do not wrap that suite around every estimate. Documentation-only work
uses preservation, link/version/status consistency and diff checks; it does not
rerun expensive compiler or GPU tests. A new native IR rewrite retains genuine
before/after .B, same-stem .T from separate ir_b2a -st -src processes, and the raw
diff with the original source available for cross-reference.

## 6. Recovery, acceptance and closure

The implementation must distinguish search_complete, selected_correctness_passed,
GPU_performance_pending and deployment_accepted conceptually; these are proposed
status concepts, not changes to existing receipt fields. A completed exhaustive
estimation run closes its search regardless of later hardware availability.
Correctness-only evidence cannot close GPU performance acceptance.

Declare deployment metric, baseline identity, improvement/regression limits,
noise policy and device conditions before M7 timing. Accept a selected variant
only under those criteria. A slower result retains the baseline, records model
error and closes the current attempt. A subsequent model revision is a new
bounded estimation experiment. The future resource-recovery default is two
total selected attempts; correctness failure stops acceptance and is never
treated as ordinary candidate pruning. Current standalone retry remains manual.

| Risk | Control and closure evidence |
| --- | --- |
| Synthetic model misranks | Explicit provenance, sensitivity/held-out calibration, paired selected/baseline timing; preserve accepted baseline |
| Finite space misses a better algorithm | Publish space/version and rejected/unvisited census; add admitted families as new bounded experiments |
| Cache order misleads strategy comparison | Equal-budget cache-neutral or virtual-cost study, multiple seeds and held-out workloads |
| Driver hook loses required High IR | M1 action-mask/lifetime audit, canonical-form check before search and verified exit before Mid |
| Late resource mismatch | Actual register/shared/spill audit, invalidation and bounded selected retry with immutable reports |
| Proof/uncertainty overclaim | Distinct estimated/measured/bound conclusions; reject unsupported probabilistic or hardware certificates |
| Search overhead erases benefit | Separate snapshot/proposal/estimate costs, explicit budgets and dimensionally valid amortization |
| Stale analysis or hidden cross-PU mutation | Generation-bound admission, owning-service rebuild, driver-owned PU/device registration and rollback tests |

Every gate retains a completion record with exact commit/tools/options, input
and model digests, result scope, commands, evidence links and open limitations.
Publish a new content-addressed review receipt for new executable milestones;
never replace v0.27 or earlier evidence. A documentation-only revision records
text validation and commit identity without manufacturing new GPU/IR evidence.

The minimum transferable package includes the specification, current status and
source work map, reproducible baseline, exact tests/commands, selected readable
and native examples, failure/recovery cases, risks and remaining gates. Primary
owner, successor, independent reviewer and approving lead are TBD. Keep
personnel transfer acceptance open while making all technical artifacts usable.

## 7. Existing standalone runbook and worked example

Use the retained v0.27 packet on Linux x86_64, from its root directory, with
fresh output paths. These existing commands exercise the current prototype;
they do not stand in for the proposed production be_driver path:

```sh
tools/tensor_kernel_lower explore-attention selected/long/source.recipe.json exploration.json \
  --gpu-profile gpu_profile.json --cost-profile cost-profile.json \
  --search-options search-options.json --allow-reassociate
tools/tensor_kernel_lower select-attention exploration.json selected.json
tools/tensor_kernel_lower schedule-source selected.json optimized.cu
tools/tensor_kernel_lower device selected.json device.before.B device.after.B
tools/ir_b2a -st -src device.before.B device.before.T
tools/ir_b2a -st -src device.after.B device.after.T
tools/tensor_kernel_lower audit device.after.B selected.json launch.json
tools/whirl2c -cuda -TARG:abi=n64 -O0 \
  -CLIST:dotc_file=mapped.cu -CLIST:doth_file=mapped.h device.after.B
```

The packet's README/replay.py retain environment and reference-test instructions.
For source cross-reference, make the retained compiler source available at the
original /src pathname recorded in DST, as the replay procedure does. Preserve
the before/after images and raw trace diff; do not substitute a textual mock IR.

Under the saved synthetic profile, the L=127 fixture selects direct attention
with 128 threads; L=8193 selects width 256 with 128 threads. Each fixture
estimates the 12-entry space, and only its single portfolio winner reaches
materialization/reference checks. These are scoped example selections, not a
universal context-length threshold. A different admissible timing/resource
profile can produce a different winner without changing any kernel algorithm.

The retained primary artifact identifies the baseline receipt as
`ai-optimization/research/mla-two-phase-search/sha256-d04d548cf12fd000b607b4aa50902a403a74bb911c6d6c54f6323779d2166f4b/`
under the primary checkout's `artifacts/review/` root. Verify its SHA256SUMS
before reuse. The existing `Open64_MLA_Two_Phase_Search_CUDA_v0.27.zip` is the
portable delivery; this documentation revision does not replace it.
