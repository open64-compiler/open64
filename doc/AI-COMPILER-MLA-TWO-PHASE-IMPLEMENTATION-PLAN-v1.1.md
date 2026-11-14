# MLA two-phase implementation plan v1.1

November 8, 2026 | Cross-head cache reuse priority with selected-only validation

Authority: [MLA two-phase design v0.2](AI-COMPILER-MLA-TWO-PHASE-SEARCH-v0.2.md),
[reuse design v1.0](AI-COMPILER-MLA-DATA-MOVEMENT-AND-REUSE-v1.0.md)
and [production plan v1.9](AI-COMPILER-OPTIMIZATION-SEARCH-IMPLEMENTATION-PLAN-v1.9.md).
The complete v1.0 plan follows verbatim. S0-S6 historical prototype acceptance is
preserved. The compiler-integration portion of its former S7 is now covered by
bounded M1-M6; native GPU/calibration acceptance remains open under M7.

## New prioritized work

| Gate | Implementation and acceptance |
| --- | --- |
| MR0 | Characterize the fixed M6 baseline on a correct launch/allocation harness. Record complete-call timing separately from cache/traffic/resource profiling, with explicit warm/cold and mask/head/batch/shape cases. No hardware means measured closure stays open |
| MR1 | Capture owned head-invariant cache/access/version and traffic/lifetime facts under LNO scope. Prove no source/ST/TY mutation; distinguish requested global, HBM, cache/SMEM, spills, projections and partial/merge work |
| MR2 | Add a separately versioned pure bounded G/W/column/layout/cost catalog and hierarchy profile. Preserve current G=1 variants and scores. Include resource/coverage/sweep tradeoffs; unsupported grouped realizations remain analysis-only. Keep global strategy budgets, full cache identities and re-proved stopping bounds |
| MR3 | Develop the synchronous intra-CTA grouped rule, then enable only its verified parameter domain. Reuse native High transaction/bridge/production services. Keep per-head masks/collectives and normalized partials, explicit staging/release, source precision and existing 48-KiB launch constraints. Rule-specific fixtures are separate from active candidate searches |
| MR4 | For the one selected eligible winner, run native/reference/guard/COMMENT and required GPU sanitizer/resource checks, then paired whole-call timing. Preserve baseline if unprofitable or refused. Cache traffic savings alone are insufficient |
| MR5 | Consider async/TMA/buffering, cluster sharing or authorized compute/precision families only after separate model, legality, ABI, materializer and backend gates; retain synchronous/scalar fallback |
| MR6 | Evaluate strategies with common budgets and cache treatment across held-out regimes, publish a scoped transfer package and obtain independent acceptance; roles remain TBD |

MR0 and MR1 feed model development; analytical MR2 can proceed on explicit
estimated inputs without claiming hardware calibration. Production eligibility
requires MR1/MR2/MR3; deployment additionally requires MR4. Optional MR5 does not
block the initial synchronous family. Detailed tests/dependencies/risks are in
the production plan; no milestone is closed by this documentation update.

The compiler search protocol remains collection once -> detached estimation and
convergence -> one fixed eligible winner -> atomic selected High application ->
verified Mid/Low/PTX -> selected correctness/resource/performance acceptance.
The compiler remains AOT; profiling jobs are outside active estimation. Do not
compile/regression-test each proposed G/W combination or add an implicit fallback
candidate loop. Keep changed-input/epoch/profile/layout/ownership refusals,
analysis invalidation/rebuild and no-replace publication.

Explain actual cache reuse, G/head tails, one/two sweeps, placement and barriers,
per-head normalized merge, traffic provenance and resource/coverage tradeoffs
through generated readable CUDA, WHIRL COMMENT, whirl2c CUDA and PTX. Preserve
M6 dependence recovery, ordinary host/G=1 behavior, the PREOPT COMMENT workaround,
independent references, tolerances, canaries and immutable inputs.

---

# MLA two-phase implementation plan v1.0

November 6, 2026. Authority: AI-COMPILER-MLA-TWO-PHASE-SEARCH-v0.1.md.
This plan supersedes freeze-based convergence and per-candidate physical
validation as a search workflow. Preserve existing algorithms, AOT baseline,
precision permissions, binary WHIRL compatibility and PU scope.

| Gate | Work and acceptance |
| --- | --- |
|S0: detached contracts|Version the explicit cost/options/report and fixed selected-recipe schemas. Reuse existing source capture, schedule admission, checked arithmetic and occupancy; keep JSON standalone-driver-owned.|
|S1: estimation-only census|Enumerate12 implemented family/width records; apply all-stage legality/capacity/resource gates, compute complete additive-wave costs and retain each component/rejection. Native compiler contexts and materialization counters remain zero.|
|S2: strategy/stop portfolio|Provide exhaustive, random, local multistart and shared-cache portfolio traversal. Separate budget/plateau/target/estimator-gap/cost-benefit stopping. Verify deterministic orders, equal per-run budgets, no duplicate candidate costing, selected minimum and accurately scoped claims. Probabilistic/hardware certificates stay unavailable without evidence.|
|S3: fixed handoff|Admit the complete report by cheap deterministic replay, seal one v7 selected recipe and bind native metadata. No per-kernel re-exploration; explicit overrides invalidate/clear the fixed plan. Reject stale/forged source/model/resource/census/winner/options.|
|S4: selected-only materialization|Feed only the winner to existing device/device-explain, independent native audit and normal-whirl2c. Preserve automatic CUDA/WHIRL comments; annotate search identity and estimated status. The emitter cannot select a different realization.|
|S5: compiler regression|Test pure exploration, numeric models, strategies/stops, rejection/rollback boundaries and native zero-allocation dependency evidence. Run focused existing-rule regression once for the changed compiler, rather than once per estimated candidate.|
|S6: winner verification|Explore short/long fixtures, select one winner per fixture, materialize each winner's stages, retain B/T/diffs and run all required saved reference scenarios on that configuration only. Compile selected CUDA, record resources and confirm commentary/metadata fidelity. Keep input, report and candidate counts alongside actual materialization/test counts.|
|S7: production integration and calibration|Open. Integrate the services under actual be_driver-owned High-phase scope and lifetime, resource snapshots, analysis invalidation and atomic selected High transformations. Calibrate instruction/cache/pipeline assumptions and validate native GPU correctness/performance when hardware is available. No claim of this work from standalone prototype results.|

Dependency order: S0 -> S1 -> S2 -> S3 -> S4 -> S5/S6; S7 requires a separate
integration/calibration milestone. The bounded current space permits exact
estimator enumeration, not global optimality over all algorithms or real GPU
latency. A new best candidate updates the incumbent, not the search boundary.

The standalone driver sequence is source capture once -> explore-attention
(JSON only) -> select-attention (fixed JSON only) -> device (selected B images)
-> ir_b2a -st -src -> audit -> whirl2c -> selected compilation/reference tests.
Replaying report integrity during command admission is estimation-only; only
one selected configuration reaches physical construction. Distinguish plans
explored, unique estimates, cache hits, selected configurations, native stage
kernels, correctness scenarios, compiled outputs and GPU executions.

Production be_driver ordering must capture target/contract/generation facts
under the active PU before candidate enumeration. High-level analysis and
proof records support detached alternatives; only the winning schedule mutates
canonical loops, storage and prefetch. Rebuild invalidated analyses, verify
High exit, then invoke existing Mid/Low/backend consumers. A failed selected
rewrite rolls back/rejects; it cannot silently select an emitter fallback.

Risks and controls: uncalibrated rankings (explicit provenance/units and no GPU
claim); omitted algorithm families (versioned finite census); phase leakage
(zero-context counters, no native dependencies in estimator and file audits);
resource/model drift (sealed snapshots and actual-resource report); incorrect
convergence inference (distinct termination labels); nondeterminism (explicit
seed/ties); overflow (checked arithmetic); numeric reassociation (O3 permission);
stale fixed selection (canonical replay and native identity binding); redundant
regressions (selected-only workflow and impact-selected compiler-rule checks).

Publish a fresh v0.27 review packet with source, tools, exact commands, estimates,
winning recipes, before/after images/traces, reference results, resource logs,
counts and SHA256 manifest. Preserve v0.26 and prior receipts and unrelated
worktree edits. Transfer roles remain TBD; GPU and integration gates stay open.

S0-S6 completion evidence for this standalone slice:10 search-specific and28
impact-selected compiler tests pass without skips;34 historical policy cases
retain native audit, recipe reselection and readable-source parity. The saved
short/long workflow performs24 unique estimates and selects two configurations,
with zero native initialization/materialization during exploration. Four selected
CUDA source compilations and14 readable plus14 mapped-host reference scenarios
pass. Actual shared allocations match selected stage estimates; register counts
fit the supplied envelope and no spills occur. These are host/compiler results;
S7 and GPU correctness/performance remain open. The immutable v0.27 receipt
retains exact commands, counts, sources and resource comparisons.
