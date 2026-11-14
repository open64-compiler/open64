# AI compiler optimization search implementation plan v1.3

November 7, 2026 | M2b owned facts, identities and mutation epochs implemented

Adopt the [M2b snapshot contract](AI-COMPILER-SEARCH-M2B-OWNED-SNAPSHOT-v1.0.md).
This dated amendment supersedes the M2b-open status in the preserved v1.2
baseline below. The existing architecture and two-phase methodology remain:
estimation explores owned facts; only the selected plan may transform WHIRL.

| Gate | Current status and next acceptance |
| --- | --- |
| M2a | Completed bounded native common-High entry and canonical admission; general VHO/provider routing stays open |
| M2b | Implemented at the qualified active-FUNC_ENTRY LNO boundary: deep-owned loop/access/full-graph/DU/alias facts, source namespace and semantic path identities, effective context records, distinct PU activations and mutation epochs; live recapture rejects covered source/analysis changes |
| M2c | Next: integrate the read-only search observer, verify complete source-manifest/resource/model bindings and zero candidate-induced WN/ST/TY changes, separate snapshot cost from exploration cost |
| M3 | Production portfolio/convergence controls remain open; standalone estimation baseline is retained |
| M4 | Selected-only schedule application remains open; check epoch/context and recapture before application, notify before mutation, atomically apply/rollback, rebuild analyses and preserve managed schedules through later LNO |
| M5-M9 | High exit/Mid-Low-device orchestration, complete compiler/GPU/search evaluation and transfer gates remain open; the existing long post-LNO image issue is still an M6 prerequisite |

The normal driver owns the active PU. Lnoptimizer creates its epoch, captures
after M2a admission with the full dependence graph/access/DU/alias facts live,
then expires it before Hoist_Varying_Lower_Bounds. The remainder of legacy LNO
cannot consume it as current. Owned records remain readable after map/graph/pool
cleanup. Any future consumer must notify all mutations, rebuild native analyses
and capture again; current revalidation is a defense in depth, not a substitute
for that contract. No stale snapshot authorizes transformations.

Stable loop/access identities are scoped to the full canonical source key and
unit key, with semantic paths independent of native addresses and map numbers.
A compact FNV review namespace is accompanied by complete equality keys.
Source positions/comments do not contribute executable ordinals. This does not
promise identity through arbitrary canonicalization or source changes.

Numerical IEEE/roundoff and native target ABI/ISA are copied; the external tensor
manifest, GPU resources and cost model remain explicitly unbound. The snapshot
grants no reassociation, parallel independence or selected schedule legality.
M2c/M3 must close these external-binding obligations before production search.

Focused validation uses a separate test LNO build for deliberate live-fact
perturbations, repeated mapped reopens, stale/closed/cross-PU rejection and
post-cleanup owned reads. The production path keeps the existing opt-in and
fallback. The common-High and native-partition compatibility suites retain
normal executable-image parity and host/source-reference checks. No GPU or
performance conclusion follows from this milestone.

The complete v1.2 plan is preserved verbatim below as the historical M1/M2a
baseline. Its M2b future-tense statements are superseded by this amendment; its
remaining architecture, milestones, tests and risks continue to apply.

---

# AI compiler optimization search implementation plan v1.2

November 7, 2026 | M2a native High entry and admission complete; owned snapshot next

Authority: [search methodology v1.0](AI-COMPILER-OPTIMIZATION-SEARCH-METHODOLOGY-v1.0.md)
and [optimization design v0.7](AI_compiler_optimization_design_v0.7.md). The [M1 audit](AI-COMPILER-SEARCH-M1-DRIVER-LNO-AUDIT-v1.0.md) records the inspected source baseline; [consolidated plan v0.9](AI-COMPILER-OPTIMIZATION-IMPLEMENTATION-PLAN-v0.9.md) adopts these findings. This plan
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

M1 is complete as a static source audit. M2a now supplies the explicit native
High-artifact MLA entry and read-only canonical admission described in the
[M2a contract](AI-COMPILER-SEARCH-M2A-HIGH-ADMISSION-v1.0.md). Next implement M2b
owned snapshot/epoch and M2c snapshot observer isolation at the identified LNO boundary.
Then implement M3 search controls before M4 selected schedule application and
M5 verified lowering boundaries. Run M6 focused compiler
acceptance before closing host integration. M7 native GPU acceptance and M8
search-strategy evaluation need hardware/data; they do not block completion of
an honestly scoped estimation-only search. M9 closes production/transfer only
with its required evidence. No additional MLA kernel micro-optimization is a
prerequisite for this methodology.

## 2. Milestones, outputs and acceptance

| Gate | Status and dependency | Concrete output and acceptance |
| --- | --- | --- |
| M0 specification | Complete in this documentation revision | Versioned methodology, main design and new plan; scoped result labels, selected-only rule, recovery policy and status/evidence distinctions |
| M1 driver and analysis audit | Complete: static source audit | Snapshot gap inside Lnoptimizer after first successful full dependence graph and enabled-path checks, before Hoist_Varying_Lower_Bounds. Audited graph/access/DU/alias cleanup, effective controls, FUNC_ENTRY scope, MAINOPT/CG masks and device-PU handoffs; no hook implemented. See M1 audit |
| M2 immutable search snapshot | M2a bounded native entry complete; M2b/M2c open | M2a: implemented unoptimized source recipe -> signed-loop/affine-ARRAY High B -> ordinary PREOPT/LNO, with canonical form/coverage refusal and unchanged fallback. Automatic general VHO/provider route selection is future driver integration. M2b: deep-copy covered access/dependence/DU/alias facts with driver/LNO epoch and stable IDs. M2c: read-only observer at lnopt_main.cxx:1360 with zero candidate-induced IR/table mutation and explicit skip reasons. Initial scope is active FUNC_ENTRY with admitted loop subsets |
| M3 portfolio and convergence service | Prototype subset complete; production work open after M2 | Reuse existing providers; add explicitly versioned global/proposal/unique/time caps, fair scheduling and composite stopping when needed. Preserve the existing CLI/schema. Reports retain all predicates, per-run trajectories and scoped union completeness; complete 12-entry estimation remains default |
| M4 selected High application | Open; M2/M3 | Typed fixed-plan admission; preflight current generation/legality/resources; atomic application through owning High services; invalidation/rebuild manifest, selected-only counters and exclusion of later competing LNO transformations on managed nests. Failed application publishes no partial output; correctness defects do not trigger silent candidate trials |
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
anchors. M2a now installs the read-only form/coverage admission described below;
the owned snapshot and search service remain unimplemented.

M1 resolves the snapshot location to `lnopt_main.cxx:1360`: after the first
successful `Build_Array_Dependence_Graph` and disabled/autopar-only exits, before
`Hoist_Varying_Lower_Bounds`. Both full LNO dependences and access/parent maps
are live with PREOPT DU/alias. The earlier access-only partition hook is not a
complete scheduling boundary. At driver return, LNO graphs/maps/pools are gone
and the wrapper has deleted DU/alias; surviving `Current_Dep_Graph` is the
reduced CG representation. MAINOPT already lowers ARRAY before final CG.

This is a qualified canonical-High boundary: M2 must admit actual loop/memory
forms and graph coverage. Early short-loop unrolling and other preparation can
precede it; unsigned-loop canonicalization currently follows it. Ordinary DSL
runtime lowering can remove the tensor algorithm before LNO, so common-High
producer/lineage support is an explicit entry prerequisite. Snapshot generation
cannot rely on VHO shape generations or borrowed partition pointers alone.
Initial supported scope is FUNC_ENTRY, not arbitrary REGION-root or IPA scope.

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

M1 documents the conflicts in its audit. M2/M4 must resolve producer admission,
early transforms, analysis ownership and the selected-path barrier through the
reviewed High boundaries. Keep required cleanup/CG-graph export and unrelated
loop processing; do not move algorithm selection into Mid/Low, retain stale
LNO pointers or duplicate the dependence engine.

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


## 8. M1 findings and revised M2/M4 acceptance

The controlling evidence is [M1 audit v1.0](AI-COMPILER-SEARCH-M1-DRIVER-LNO-AUDIT-v1.0.md),
source baseline `0de7c6ad7bce7142c6e66bcac3560e176cc5ab27`. M1 closure is a
source-inspection result; it does not certify a runtime hook or High pipeline.

| Next gate | Work order and required evidence |
| --- | --- |
| M2a producer/form admission | Identify admitted common-High loop producer and semantic lineage before the existing DSL runtime lowering. Initially admit good affine DO_LOOP/ARRAY nests with proved memory coverage; distinguish already unrolled/missing loops, bad-memory/Too_Messy/call/barrier cases, unsupported unsigned forms, MP/Lego/autopar and non-FUNC_ENTRY roots. Retain ordinary behavior on refusal. If search must preserve early loops, separately implement/test their preparation deferral; do not move graph building speculatively |
| M2b snapshot and lifetime | Consume full LNO DEPV_ARRAY_ARRAY_GRAPH and current access/parent/DU/alias facts at the qualified 1360 boundary. Copy owned loop/access/dependence/numerical/resource records; no WN/map/graph/manager pointers escape. Define a driver/LNO mutation epoch plus stable source joins; VHO shape generation alone is insufficient. Rebind/validate before selected application |
| M2c observer-only integration | Install an opt-in read-only collector before any search/application. Gate on effective post-backoff O3, Run_lno, LNO_enabled/LNO_Opt, numerical permission, supported scope and successful graph coverage. Record explicit skips. Prove input tree/tables unchanged by snapshot/candidate work; charge earlier analysis preparation separately. Keep trace-off and option-off behavior unchanged |
| M4 selected continuation | While facts are live, apply the selected plan under active-PU ownership or return a fully owned handoff with revalidation. Exempt managed nests from subsequent reversal, scalarization, fusion/fission/SNL, tiling, memlib and prefetch decisions unless explicitly admitted as selected realization. Preserve mandatory cleanup/export/feedback and unrelated nests. Rebuild affected facts and verify High exit before MAINOPT ARRAY lowering |
| M5 device publication | Generalize the existing numeric selected-plan registry only through a reviewed MLA handoff. Driver constructs device PUs after selection; normalization's program checkpoint remains a separate route, not proof of LNO integration. Preserve phase/scope lifetimes and atomic selected artifact publication |

M2/M4 focused tests must include: complete affine coverage; signed canonical and
unsupported unsigned/overflow bounds; small loops affected by early unrolling;
no loops/missing DU; full graph failure and graph-success with Has_Bad_Mem or
unrepresented accesses; skip/disabled/PREOPT-only and post-Olimit backoff;
trace-off parity; active-PU isolation with multiple independent PUs; stale epoch
and analysis rebuild after selected mutation; cleanup after early refusal; and
selected schedule preservation across the remaining LNO path. REGION-root,
MP/Lego/autopar and IPA support stay refused until separately admitted.

The owned snapshot observer and selected transformations remain future work.
The M1 audit ran source/excerpt/hash, ordering, link and documentation checks only. It changed
no compiler source and produced no new B/T/GPU execution evidence. Original
v1.0 remains available, and v0.27 retains its original host/compiler-only scope.


## 9. M2a completion and remaining production work

[M2a implementation contract](AI-COMPILER-SEARCH-M2A-HIGH-ADMISSION-v1.0.md)
controls the new `tensor_kernel_lower high` native-artifact entry. The driver
constructs a fresh host PU with an explicit disjoint-storage ABI; it does not
rewrite the original source/caller interface. The saved absorbed MLA algorithm
reaches ordinary VHO/PREOPT/LNO as signed DO_LOOP and affine ARRAY operations,
without tensor runtime calls or GPU mapping. Default DSL/runtime and existing
selected-device paths retain their behavior. Broader automatic VHO/provider
routing is not claimed by this entry path.

`LNO_DSL_admit_common_high` runs immediately before Hoist_Varying_Lower_Bounds
under effective O3/LNO and existing partition_axis opt-in controls. It observes
canonical signed static unit loops, explicit nonvolatile affine memory and full
graph coverage; it refuses unsupported scope/control/memory forms without
mutating executable WHIRL. MP/Lego/autopar paths do not receive this new
admission. The report explicitly grants neither an owned snapshot nor schedule
legality. M1's numerical line references describe its audited revision; the
function anchors remain the integration identity after this insertion.

Focused evidence: both source graphs contain 48 operations/55 values/41 unique
storage buffers and reach an admitted LNO boundary (59 surviving loops and 87
memory operations in each fixture). Eight ordinary-C form cases admit the two
affine/ordered-fold cases and refuse six unsupported cases. On/off/default,
trace-off, O0-O2 and LNO opt=0 checks preserve fallback behavior; default native
output also matches the retained pre-change LNO component byte-for-byte.

Fourteen High-image host executions cover all saved scenarios against FP64
oracles with canaries and immutable inputs. Seven additional short-context
post-LNO checks pass. Host pre/post bitwise equality is recorded as an
observation, while required numerical acceptance uses the saved source oracle
tolerances. Ordinary whirl2c requires the retained legacy host-math spelling
adapter; its post-LNO C lane disables unsupported x86 SIMD source spellings.
Native default-path byte comparisons retain the normal controls.

Existing impact-selected tensor-lowering/search regressions pass (38 tests),
as does the native partition regression (13 cases, five proved/eight refused).
No GPU execution or performance result is claimed. M2b and M2c now follow this
bounded form-admission entry; M4 still needs selected-schedule ownership and
analysis rebuild, and M5 still owns general device-PU orchestration.

A pre-existing long-context post-LNO CG dependence-graph reopen failure remains
an explicit M6 prerequisite: the retained baseline generates the identical
portable .N image and normal whirl2c fails in Depgraph_Read/OPCODE_make_op for
both. This is not counted as a successful post-LNO execution. Retain the
identical baseline/new images, hashes and both diagnostics when reproducing;
repair the defect before closing general mapped post-LNO acceptance. M2a does
not change binary WHIRL or dependence-image layout to work around it.
