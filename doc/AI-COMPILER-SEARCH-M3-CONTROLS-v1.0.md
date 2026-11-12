# M3 production High-WHIRL search controls v1.0

November 7, 2026 | Implemented bounded MLA estimation service

For compiler invocation, complete configuration generation, defaults and
troubleshooting, see the [compiler user guide](AI-COMPILER-HIGH-SEARCH-USER-GUIDE.md).

## 1. Authority and scope

This contract extends [M2c](AI-COMPILER-SEARCH-M2C-OBSERVER-v1.0.md) and adopts
[methodology v1.1](AI-COMPILER-OPTIMIZATION-SEARCH-METHODOLOGY-v1.1.md).
It closes M3 for the explicit common-High MLA route and its existing fixed
12-descriptor space. It does not close M4 selected-plan application, M5 High
exit/lowering, M7 GPU acceptance, M8 comparative strategy evaluation or M9
transfer. Primary owner, successor, independent reviewer and approving lead
remain TBD.

Architecture remains Very High domain operators -> High common algorithm,
DO_LOOP/OPR_ARRAY, LNO dependence/DU/alias analysis and schedule planning ->
Mid structured memory operations -> Low GPU mapping -> PTX. Wolf unimodular
and Cohen polyhedral scheduling, multi-level tiling, SMEM staging and prefetch
loops remain High responsibilities. M3 adds search policy at the existing
canonical High snapshot; it does not prematurely lower a candidate or route
general VHO operations. See the preserved design and methodology for affine
schedule mathematics and optional hybrid providers.

The search reads owned, generation-bound source/loop/access/dependence facts.
Native preparation, binding checks and snapshot collection occur once, outside
the candidate loop. Exploration uses detached values only. Candidate WN/ST/TY
construction, native transformations, B/CUDA/PTX emission, backend invocations
and regression tests are prohibited inside exploration. M3 produces a scoped
estimate report, not an application-authorizing schedule or executable winner.

## 2. Production route and backward compatibility

Use the normal backend driver with effective O3/LNO, admitted common High,
and the numerical reassociation permission required by the existing MLA model:

```text
-LNO:high_search=on:high_search_bindings=/absolute/path/binding-v2.json
```

This opt-in does not require -DSL:partition_axis=on. The latter still controls
the existing partition observer and legacy environment adapter.
-DSL:ai_opt_trace=on with the existing -tr25 -tt25:1 enables the review trace;
disabling tracing suppresses reports without changing native output.

The binding file must use open64.lno-high-search.v2. It retains every complete
M2c source/analysis/numerical/native producer ABI/GPU/model field and adds the
search_controls object below. A partial document is not a usable configuration.

| Mode | Behavior |
| --- | --- |
| Explicit high_search=on | Read only high_search_bindings; require v2 and all binding joins |
| Explicit high_search=off | Disable production and environment search; ordinary LNO |
| Unspecified high_search | Preserve partition_axis + OPEN64_HIGH_SEARCH_BINDINGS adapter; v1 still accepted, v2 may be supplied |
| Binding path alone | Does not opt in |
| Invalid mode, absent path, malformed/stale/unbound input | Diagnostic when tracing; ordinary LNO, no candidate application |
| O0/O1/O2, disabled LNO or excluded scope | Existing driver admission/fallback preserved |

The v1 root/schema and standalone CLI remain unchanged. Legacy sequential
portfolio semantics still record 36 visits, 12 estimates and 24 cache hits for
a complete default run. New v2 scheduling may visit fewer candidates because
complete union coverage terminates the portfolio immediately.

Production options are file-scope, cannot be changed by a pragma, and use
private option-descriptor storage in common/com/config_lno.cxx. No LNO_FLAGS
layout field, WHIRL opcode/type kind/image format or reader/writer change is
introduced. Native LNO reads the existing public option-descriptor API;
the detached closure has no access to that API. A source-matched before/after
be.so audit finds no exported/undefined dynamic-symbol change from the option
addition and no new library dependency. The old cached backend was behind
already committed source; its unrelated export additions on rebuilding are
recorded separately, rather than attributed to M3.

## 3. Complete version-two control envelope

All 15 fields are required; unknown, duplicate, ill-typed, coerced, oversized
and missing fields are refused. Existing bounded parser, depth, node, UTF-8 and
embedded-NUL checks apply recursively. Integers must be unsigned JSON integers.

```json
{
  "schema": "open64.mla-search-controls.v2",
  "space": "mla-family-width-space-v1",
  "mode": "estimate",
  "global_proposal_limit": 36,
  "global_unique_limit": 12,
  "global_time_limit_us": 0,
  "runs": [
    {"id": "local", "strategy": "local", "seed": 17, "proposal_limit": 12},
    {"id": "random", "strategy": "random", "seed": 17, "proposal_limit": 12},
    {"id": "exhaustive", "strategy": "exhaustive", "seed": 17, "proposal_limit": 12}
  ],
  "stopping": ["exhausted", "budget"],
  "patience": 12,
  "minimum_gain_cycles": 0,
  "target_cycles": 0,
  "gap_ppm": 10000,
  "remaining_invocations": 0,
  "evaluation_cost_cycles": 0,
  "economic_conversion": null
}
```

"Global" means shared across strategy runs for one bound active-PU search
invocation. It does not impose a compilation-wide or process-wide cap across
PUs, compiler jobs or future searches.

| Control | Bound and meaning |
| --- | --- |
| global_proposal_limit | 1..192; every visit, including cache hits and rejected candidates |
| global_unique_limit | 1..12; each first estimate or resource rejection, globally cached |
| global_time_limit_us | 0 disables; otherwise up to 3,600,000,000 microseconds of search service time |
| runs | 1..16; ordered, independently seeded; unique IDs of 1..96 alphanumeric/underscore/hyphen/dot characters |
| strategy / seed / proposal_limit | local, random or exhaustive / 0..2^32-1 / 1..12 |
| stopping | 1..6 distinct supported names; hard caps and exhaustion always apply |
| patience / minimum_gain_cycles | 1..12 / 0..10^9; per-run empirical stagnation |
| target_cycles / gap_ppm | 0..10^18 (positive when target enabled) / 0..10^6 |
| remaining_invocations / evaluation_cost_cycles | 0..10^12 / 0..10^18; positive with cost-aware stopping |

Duplicate providers with distinct IDs are supported; they receive independent
run state and share estimates. Unsupported spaces, providers, apply mode or
uncalibrated probabilistic-confidence stopping are refused. Optional hybrid
affine/polyhedral providers remain future work requiring legality, estimator
and materializer contracts, not names accepted speculatively by the parser.

## 4. Portfolio, cache and incumbent invariants

Keep independent visited sets, RNG states, local anchors, restart counters,
staleness counters and incumbents for each run. The existing local-neighbor,
random and exhaustive proposers and one evaluator/catalog are reused. Each
active run receives one atomic proposal per round; stopped runs are skipped.
A candidate is never visited twice within a run. Shared estimates are immutable
for this binding; no persistent cross-binding cache is introduced.

A first visit charges one proposal and one unique estimate. A repeated visit
by another run charges one proposal and one cache hit. Rejected candidates
remain evaluated/ineligible with a reason; unevaluated entries remain distinct.
The unique cap stops the entire portfolio, including possible future cache
hits. Every finite complete union requires exactly 12 unique evaluations.

Retain the lowest estimated total whole-operation cost among eligible
evaluated candidates; break ties by smaller catalog ordinal. Each run retains
its own best and first-best trial. minimum_gain_cycles controls only whether
stagnation resets: it never prevents a smaller estimate from becoming incumbent.
Every control value, ordered run/seed/limit and stopping name contributes to
the full control identity, alongside the existing source/resource/model keys.
A fixed seed and no deadline produce reproducible event trajectories. A live
deadline can change the visited prefix; elapsed time is reported, not included
as an input identity or treated as deterministic.

## 5. Convergence and simultaneous predicates

Hard proposal/unique/time caps and full declared-space exhaustion are mandatory
even if omitted from stopping. Enabled semantic predicates compose with OR.
Plateau is local to a run; the global service continues other active runs.

Let B be the best eligible explored whole-operation estimate in reference
cycles and L = 5 * launch_cycles the existing conservative additive-model
lower bound. L is a bound within this estimator only. It is not a physical
latency lower bound or a proof of native schedule legality.

| Stop | Exact meaning and result scope |
| --- | --- |
| exhausted | All 12 descriptors evaluated/rejected: best estimated in declared space, or no eligible estimated candidate there |
| budget | Any hard cap reached: best estimated among explored, or none yet |
| plateau | After an incumbent exists, stop that run when patience successive proposals fail to improve its previous best by more than minimum_gain_cycles; empirical only |
| target | B <= target_cycles; estimated target reached |
| relative-gap | B >= L and B-L <= floor(L * gap_ppm / 10^6); estimator-gap certificate |
| cost-aware | B >= L and B-L <= floor(C / N), with positive remaining calls N and explicitly converted exploration cost C; estimator-benefit bound |

For the last predicate, N*(B-L) <= C is evaluated by division to avoid overflow.
It bounds the maximum modeled benefit available over the remaining invocation
horizon against a caller-declared cost. It does not infer host time from device
cycles, automatically learn N, or measure pipeline efficiency.

When cost-aware is enabled, economic_conversion must be an exact object:

```json
{
  "schema": "open64.search-cost-conversion.v1",
  "host_microseconds": 1000,
  "reference_cycles_per_microsecond": 1000,
  "provenance": "estimated"
}
```

C must equal the checked product of these two positive quantities.
host_microseconds <= 10^12 and reference_cycles_per_microsecond <= 10^9;
overflow/mismatch is rejected. Provenance is synthetic, estimated or
measured-input. This label records caller evidence; the compiler does not
authenticate a measurement or reinterpret the GPU arch clock as a host cost.
When conversion is null, conversion values are zero/empty in the typed API.
No calibrated confidence stopping is implemented.

All predicates satisfied at the terminal accounting snapshot are retained.
The primary global reason uses fixed priority: declared_space_complete,
global_proposal_budget, global_unique_budget, global_time_budget,
estimated_target_reached, estimator_gap_certificate, estimator_benefit_bound,
all_runs_stopped. Per-run predicates retain run_space_exhausted,
run_proposal_budget and empirical_plateau; globally interrupted runs carry the
global primary reason as their termination. Completeness takes precedence in
the conclusion, regardless of a simultaneous cap.

The optional monotonic deadline starts before detached admission/catalog setup,
is checked before each atomic proposal and at terminal accounting, and includes
all runs, cache lookups and estimates. It is cooperative: an in-flight estimate
and bounded bookkeeping can overrun the deadline. Native snapshot collection,
file decoding/binding checks, integrity recapture and trace IO are separately
accounted outside this search limit. There is no unsafe asynchronous native
preemption. Clock failure/rollback refuses the service and leaves its output
argument unchanged; normal LNO remains available.

A valid bounded search with no incumbent returns its actual counters,
has_winner=false and winner=none. It never fabricates catalog entry zero.
Native observation records search_without_eligible_candidate and uses ordinary
LNO. A complete report still proves only finite estimator coverage; no global
GPU optimum or superior measured runtime is claimed.

## 6. Native state, phase ordering and analysis lifetime

1. The existing be_driver owns the PU; PREOPT/common-High admission and full
   LNO access/dependence/DU/alias construction run normally.
2. At the M1/M2 snapshot boundary after the first successful full dependence
   graph and before Hoist_Varying_Lower_Bounds, the existing owned snapshot is
   captured, generation-bound and admitted. Production or legacy opt-in opens
   the observer; effective O3/LNO/scope and numerical gates still apply.
3. Decode/validate full binding plus controls; verify the native producer
   recipe witness, ABI, source/analysis context, numerical permission, GPU
   envelope and model. Record physical tree/ST/TY state around observation.
4. Run detached fair exploration. No candidate native entry point, code
   generator, native factory, GPU runtime or test callback is reachable in
   the audited production closure.
5. Recompare physical native state and recapture covered analyses. Suppress
   invalid results on mismatch. Keep collection/validation/exploration/isolation
   accounting separate. No result can authorize application.
6. Expire the snapshot before the first legacy LNO mutation, continue normal
   LNO and destroy native maps/graph/pools normally. Owned records survive;
   they cannot be consumed as current after epoch expiration.

M4 must freshly admit the selected plan against current source/epoch/context,
prove schedule and numerical legality, notify mutations before changing native
state, apply atomically/rollback, rebuild affected analyses and establish
ownership of selected nests. A report from this milestone is insufficient.
High exit verification, structured memory lowering, GPU mapping and PTX remain
later gates; neither CUDA emission nor low lowering may reopen the search.

## 7. Implementation map and validation

| File | Responsibility |
| --- | --- |
| common/com/config_lno.cxx | Private production opt-in and binding path descriptors |
| be/lno/lnopt_main.cxx | Existing snapshot boundary, independent production request gate |
| be/lno/dsl_high_observer_native.cxx | Native option/binding adapter, physical guard and v2 event/report printing |
| be/lno/dsl_high_observer_json.cxx | Strict v1/v2 decoding into owned values |
| be/lno/dsl_high_observer.h/.cxx | Pure binding admission and detached service dispatch |
| be/vho/tensor_attention_search.h/.cxx | Shared catalog/proposers/evaluator, v1 compatibility and typed v2 controls/results |
| be/lno/tests/dsl_high_search_control_test.cxx | Native-free scripted-clock unit acceptance |
| be/lno/tests/run_high_search_control_review.py | Production flags, ledger recomputation, fallback/state/closure evidence |
| be/lno/tests/run_high_observer_review.py | Legacy observer acceptance and strengthened closure exclusions |

The test clock exists only in a separately compiled pure test object. It is
absent from production symbols and cannot be selected by options or JSON.
The five production detached objects are audited against native factories,
compiler globals, option lookup and test-clock references.

Acceptance includes 37 pure checks; short/long default and repeat observations;
12 native production scenarios; 19 refusal/disabled cases; O0/O1/O2/disabled-LNO,
path-only and explicit-off precedence; actual event-based recomputation of
incumbents, counts, cache charging, fair scheduling and union coverage; legacy
v1 census/standalone agreement; current and pinned-backend fallbacks; and mapped
B/T reopens with no candidate artifacts. Existing tensor/search, common-High,
snapshot/lifecycle and native-partition suites remain compatibility gates.
The delivery receipt records exact results, source commit, command logs,
tools, raw image comparisons and checksums.

Strict numerical-policy images are required to be raw-byte identical.
Reassociation-enabled comparisons retain all raw differences and classify only
the known, native-ABI-confirmed trailing padding of CG DEP_STRUCT; all meaningful
bytes must match, and deliberate Is_Must corruption is rejected. No writer
change or silent normalization is made. The older baseline-identical long
post-LNO CG dependence-image reopen defect remains an M6 prerequisite, not a
successful long execution. Host reference checks certify host semantics only.

## 8. Risks, remaining milestones and transfer

- The fixed additive model and supplied resources can rank candidates
  incorrectly on hardware. Report estimated scope; M7/M8 require calibration,
  held-out workloads, noise-aware timing and independent strategy comparisons.
- Round-robin shares cache and proposal budgets deliberately; it is not the
  cache-neutral, equal-budget experimental protocol needed to declare one
  strategy superior. Default complete union removes estimation search regret
  within this finite space, not model error.
- Cooperative time caps bound work admission, not hard wall-clock latency.
  M4 must separately bound selected transformation/validation cost.
- Private generic descriptors avoid a new be.so external API but production
  requires the rebuilt option table; the pinned old backend remains a fallback
  comparator, not a parser for the new flags.
- Binding failures, invalid controls, no incumbent or unsupported forms preserve
  normal LNO. A correctness defect in future selected application must not
  silently trigger a candidate trial loop.
- M4 selected-only application, M5 High exit/Mid-Low/PTX, M6 full native
  integration, M7 GPU acceptance, M8 strategy evaluation and M9 personnel
  transfer remain open. No automatic generic VHO routing or new provider is
  implied. Roles remain TBD and transfer acceptance remains blocked by those
  assignments and later evidence.
