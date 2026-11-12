# AI Compiler Performance Measurement And Regression Plan

Version: v0.1

Date: 2026-10-29

Status: Draft for review; contracts, harness, dashboard and skill below are planned,
not newly implemented or GPU-validated by this document.

## 1. Purpose And Ownership

Build a continuously extensible way to answer four questions:

1. Is the generated program correct under its declared numerical contract?
2. Is the selected optimization faster than an admitted, comparable baseline?
3. How close is it to an appropriate hardware bound, and why is time exposed?
4. Did a compiler change regress correctness, performance, coverage or selection?

Use three separately maintained building blocks: a measurement specification,
an executable harness, and a procedural skill. The regression dashboard is a
view of harness evidence, not a fourth independent measurement implementation.
This plan owns their staged delivery. Existing optimization designs remain
authoritative for semantics, legality, profitability and WHIRL transformation.

Related documents:

- [Optimization implementation plan](AI-COMPILER-OPTIMIZATION-IMPLEMENTATION-PLAN.md).
- [Common execution planning](AI-COMPILER-COMMON-EXECUTION-PLANNING.md).
- [Compilation and execution modes](OPENPY-COMPILATION-AND-EXECUTION-MODES-DESIGN-v0.1.md).
- [Review artifact retention policy](REVIEW-ARTIFACT-RETENTION-POLICY.md).

Performance harness generation remains opt-in. The earlier `-perf-harness`
spelling is provisional, default OFF, not a newly registered driver option.
Neither normal standalone applications nor service JIT requests should acquire
benchmark dependencies, test inputs or timing policy implicitly.

## 2. Definitions And Evidence Levels

| Term | Definition |
| --- | --- |
| Workload | Semantic operation/region, domains, shapes, types, layouts, strides, aliases, input distribution and numerical/environment contract. |
| Provider | Adapter that builds or invokes one implementation of the workload, exposes completion and exports evidence. |
| Baseline | Reviewed, immutable implementation and measurement protocol admitted for a specified comparison class. It is not simply yesterday's fastest sample. |
| Candidate | One complete compiler plan, including compute, layout, ownership, residency, movement, synchronization, tails, epilogue and dispatch. |
| Comparison class | Implementations satisfying the same workload, numerical contract, hardware/environment compatibility and timing boundary. |
| Timing boundary | Exact operations included between start and completion; allocation, packing, copies, compilation and dispatch inclusion are explicit. |
| Receipt | Content-addressed record connecting source, compiler/options, selected WHIRL, emitted code, executable, target, inputs, protocol and results. |
| Selector regret | Measured selected-plan latency divided by the best measured latency among the tested, legal, comparable plans. Not a global optimum claim. |
| Sustained ceiling | Independently measured service rate from a named calibration workload and protocol; distinct from a specification peak. |

Evidence states are `planned`, `offline_verified`, `native_correctness_verified`,
`native_timing_verified` and `native_profile_verified`. Attach them per result
and per capability; they are not a single project-wide maturity label.
Mock timers, synthetic CSV and analytical cycles may test the harness but must
never create native performance evidence. No GPU is currently allocated.

## 3. Research-Informed Reuse Decisions

Research below motivates the design; adapter choices and contracts are Open64
proposals. Verify licenses, dependencies, revisions and semantic compatibility
before adopting code. Pin each adopted revision; mutable links are discovery
references, not experiment identities. Research reviewed 2026-10-29.

| Source | Observed capability and proposed use | Boundary or caution |
| --- | --- | --- |
| [Google/JAX benchmarking](https://docs.jax.dev/en/latest/benchmarking.html) | Separates compilation, transfer and execution, and requires waiting for asynchronous work. Adopt explicit completion and timing boundaries. | Host enqueue time alone is not device execution latency. |
| [NVIDIA NVBench](https://github.com/NVIDIA/nvbench) | Native CUDA timing, parameter sweeps, cold-L2 and batched measurements. Evaluate as an optional native measurement backend. | Cold and batch results are different protocols. Supplied byte counts are not automatically measured DRAM traffic. No linkage into `be.so`. |
| [Apache TVM MetaSchedule](https://tvm.apache.org/docs/reference/api/python/meta_schedule.html) | Separate builder/runner interfaces, including RPC execution. Reuse the separation for offline build and later remote measurement. | Learn the interface, not a requirement to import TVM or its search policy. Apache project identity is distinct from individual affiliations. |
| [OpenAI gpt-oss](https://github.com/openai/gpt-oss) and [referenced Triton-kernels MLP benchmark](https://github.com/triton-lang/triton/blob/main/python/triton_kernels/bench/bench_mlp.py) | gpt-oss references optimized Triton MoE kernels. The MLP benchmark warms up before Proton profiling and invokes roofline analysis. Study composite workloads and profile attribution. | Triton is a separate project. Profile-derived work/traffic metadata is not, without inspection, a measured HBM-counter result; model inference speed does not prove individual compiler decisions. |
| [Anthropic performance take-home](https://github.com/anthropics/original_performance_takehome) | Optimization on a simulated accelerator, trace inspection and explicit protection against modifying verification tests. Adopt independent, protected verifiers and immutable experiment conditions. | Simulator cycles are not Hopper/Blackwell timings. This is not an inference-service benchmark. |
| [Hugging Face kernels-benchmarks](https://github.com/huggingface/kernels-benchmarks) | Separates typed input/reference/comparison interfaces from implementations. Adopt a workload/provider contract. | Experimental repository; generic `allclose` cannot replace exact ordered arithmetic or an approved error policy. |
| [Hugging Face Optimum-Benchmark](https://github.com/huggingface/optimum-benchmark) | Multi-backend scenarios and latency/throughput/memory/energy measurement. Evaluate for later model-level adapters. | README labels it work in progress/not production ready. Not the authoritative compiler harness. |
| [Meta/PyTorch TritonBench](https://github.com/meta-pytorch/tritonbench) | Operator inputs and alternative implementations support provider comparisons. Study adapter organization and workload coverage. | Pin providers and numerical policies; do not make PyTorch a middle-end dependency. |
| [Stanford KernelBench](https://github.com/ScalingIntelligence/KernelBench) | Workload catalog and correctness-plus-speed evaluation. Learn failure-inclusive coverage reporting. | A kernel-generation benchmark, not proof of compiler superiority; audit references and admission tests before importing cases. |

Do not attempt a full benchmark-framework survey before delivery. PERF0 records
a bounded adoption decision for native timing, provider interfaces and result
validation; later sources enter the decision log without restructuring the plan.

## 4. Architecture And Reuse Boundaries

```text
versioned workload + protocol + admitted baseline
  -> compiler/build adapters -> immutable offline package
  -> native runner -> independent numerical verifier
  -> uninstrumented paired timing -> separate profile/calibration runs
  -> validated measurement records + compiler trace/plan identities
  -> text reports + regression dashboard + reviewed feedback proposals
```

| Building block | Owns | Must not own |
| --- | --- | --- |
| Measurement specification | Comparison identity, arithmetic acceptance, timing boundaries, metric formulas, evidence and regression policy. | Optimization selection or emitter heuristics. |
| Repository harness | Build/run/import/verify/report operations, provider adapters, raw samples and receipt validation. | A second CUDA optimization implementation or silent semantic relaxation. |
| Compiler and phase report tool | Candidate legality/cost/selection and actual WHIRL actions; stable IDs/source locations and existing phase logs. | Claiming native execution from an analytical estimate. |
| Dashboard | Validated-result comparisons, filtering, trends, alerts and evidence links using the common report library. | Independently recalculating metrics in browser code or editing baselines. |
| Procedural skill | Choosing tests, invoking repository tools, interpreting evidence, triage and publication checklists. Proposed name: `open64-optimization-performance`. | Duplicated timing math, hidden thresholds or automatic acceptance of a faster incorrect result. |

First inventory and extend existing services:

- `osprey/be/vho/tests/ai_matmul_cuda/ai_opt_report.py`: compact trace parsing
  and phase summaries. Preserve its phase status and header conventions.
- `osprey/be/vho/tests/ai_matmul_cuda/remote_generated.py` and associated
  runners: package integrity, remote transport, native receipts and paired
  timing checks. Preserve existing schemas; introduce versioned extensions.
- Existing M0 controls, typed SIMT fixtures, numerical oracles and mapped-image
  tests: admit each for its actual contract, not by filename or past success.

Their existence does not certify a complete baseline or the new harness.
PERF0 must name reusable functions, incompatible assumptions and focused tests
before choosing final shared-tool locations. Do not create a parallel report
parser, remote transport or baseline implementation merely for a new directory.

## 5. Workload And Baseline Registry

Give every workload a stable ID and a versioned case manifest. Separate semantic
identity from physical realization so alternate tilings can share a workload.
Record source/native discovery evidence; importing a known framework operator
does not certify discovery from canonical scalar WHIRL.
Record input-generator revision, seeds and input hashes where applicable;
separate tuning/calibration inputs from held-out evaluation distributions.

| Expansion stage | Workloads | Required distinctions |
| --- | --- | --- |
| W0 | Standalone GEMM: small/large, square/rectangular, divisible/tail, aligned/strided admitted cases. | Current FP32 SIMT and separately admitted typed/MMA contracts; D/S1/T2 only where executable and legal. No fusion implied. |
| W1 | Ordered reduction, stable Softmax, centered LayerNorm reference; RMSNorm after admission. | Exact order versus authorized tree/reformulation, epsilon/rsqrt/exp rules, axes/domains, exceptional inputs and environment. |
| W2 | Producer-consumer regions, GEMM epilogues, normalization/reduction fusion. | Same region input/output semantics; compare complete fused and independently optimized unfused regions, including temporary traffic and launch costs. |
| W3 | Attention and MoE. | Attention masks/state reformulation; MoE expert distribution, routing, ragged domains and ownership. No favorable-only routing distribution. |
| W4 | Model/service execution, initially an SGLang integration experiment. | Prefill/decode, concurrency, batching, sequence distribution, KV pressure, TTFT/inter-token latency, throughput at a declared SLO, and quality. |

For each workload keep three separate references where available: an independent
numerical oracle; an admitted own-compiler executable baseline; and external
library/compiler providers. A high-precision oracle is not necessarily a fast
baseline, and a vendor kernel need not satisfy an exact ordered contract.
External providers that differ numerically appear in a separate comparison
class, never silently in the exact-semantics speedup aggregate.

Freeze baseline source/binary, flags, numerical policy and manifest. A native
comparison remeasures that frozen baseline alongside the current candidate on
the current compatible host; historical timings remain a separate trend.
Baseline replacement is an explicit reviewed change with old/new IDs, rationale
and overlap measurements where possible. Keep old receipts; never auto-rebase
away a regression. Numerical baseline changes require a new comparison class.

## 6. Measurement Protocol And Admission

1. Verify package checksums, source/WHIRL/executable lineage, target support,
   guards, inputs, workload identity and baseline compatibility before running.
2. Verify correctness independently, including nonuniform/adversarial data,
   tails, repeated invocation and mutation of inputs between invocations.
   Protect reference/verifier files and protocol from candidate-side changes.
3. Compile and warm up outside steady-state timing. Record compile, search,
   autotune, module-load and first-request costs separately, with cache state.
4. Use device completion-aware timing for kernel/region latency; host wall
   timing for declared end-to-end scope. Include all contributing streams and
   required synchronization, not only the last conveniently timed kernel.
5. Run interleaved or balanced/randomized paired baseline/candidate blocks;
   reset mutable inputs outside the declared timed interval unless reset is
   application work. Record cache preparation and replay state per sample.
6. Keep ordinary launches and graph replay separate; keep cold-cache and
   steady-state batches separate. Account explicitly for repetitions per
   timed batch. Never report a batch-average tail as a per-request p99.
7. Preserve raw samples, exclusions with reasons, sample count, estimator,
   uncertainty method and stopping budget. Provisional starting protocol:
   30 paired blocks after declared warmup, medians and a paired-block bootstrap
   confidence interval. PERF0 reviews this policy; cap time and mark unresolved
   noise rather than sampling until a desired conclusion appears.
8. Collect counters/profiles in separate runs. Relate them to the same code,
   inputs, launch and environment; do not substitute instrumented timing for
   the uninstrumented performance result.
9. Record GPU SKU/SM suffix, device/MIG partition, memory capacity, toolkit,
   driver, clocks/power policy, sharing, thermal state and relevant host/runtime
   versions. No automatic privileged clock changes. Refuse incompatible pairs;
   unknown or unstable conditions downgrade confidence explicitly.
10. Validate imported results and publish a new immutable receipt atomically.
    Hash checks prove identity, not trustworthiness of an arbitrary remote
    producer; retain runner identity and review its execution provenance.

Admission failures, wrong results, timeouts, OOM, unsupported plans, disabled
phases, missing baselines and noisy timing remain visible. They must not become
zero-time samples or disappear from suite denominators.

## 7. Metrics And Roofline Contracts

Latency and correctness are primary. For a comparable pair, report speedup
`T_baseline / T_candidate`, percentage latency change
`100 * (T_candidate / T_baseline - 1)`, and uncertainty. Report both current
paired comparisons and historical trends, labeled separately.

For a homogeneous compute class and a named memory level L:

```text
F_use       = useful algorithmic floating-point operations
F_exec      = executed-work estimate/count, including redundant/padded work
Q_L         = traffic bytes at level L, with provenance
AI_L        = F_exec / Q_L
P_exec      = F_exec / T_native
P_use       = F_use / T_native
R_L         = min(P_compute_ceiling, B_L_ceiling * AI_L)
roof_fraction_exec = P_exec / R_L
roof_fraction_use = P_use / ((F_use / F_exec) * R_L)
```

These fractions are equivalent accounting views when definitions match; always
show the work inflation `F_exec / F_use`. The homogeneous formula is conditional,
not a complete latency predictor. Mixed SIMT/MMA/SFU workloads need separately
attributed work and service bounds; do not sum FLOPs then compare against a
single Tensor Core peak. Track exp/rsqrt and integer work separately as needed.
For memory-oriented operators prioritize latency, elements/s and byte service;
low TFLOP/s need not indicate poor performance.

Specify FMA counting, data/accumulator types, dense/sparse authorization and
actual compute realization. Never compare SIMT FP32 against an FP4/sparse MMA
ceiling. Mark count provenance as logical, compiler-estimated, instruction-
derived or hardware-observed; native timing alone does not measure FLOP count.

Keep logical tensor bytes, issued access/sector estimates and hardware counter
traffic separate. Candidate layouts, cache behavior, fusion, precision and
recomputation can change intensity, so do not calculate one universal intensity
and reuse it for all plans. A reusable SKU/type ceiling table still needs a
versioned source and environment compatibility check. Reuse ceiling calibration
only within its admitted environment; recalibrate after invalidating changes.

Report specification-peak and independently calibrated sustained rooflines
separately. A measured-time/model-byte roofline is a hybrid estimate, not a
counter-measured HBM roofline. Zero/missing bytes, absent ceilings or incompatible
definitions yield `not_available`, never an invented efficiency or division by
zero. Above-roof observations trigger a definitions/counter audit, not a claim
that hardware limits were exceeded. Communication, launch, synchronization and
operand readiness remain explicit costs beyond the simple roofline bound.

Compare predictions to measurements only in compatible units. Uncalibrated
reference cycles support ranking diagnostics, not nanosecond error claims.
For selector regret include all tested legal comparable plans, the test budget,
excluded cases and untested alternatives. Feed calibration back through reviewed
hardware-profile revisions; the dashboard does not silently retune production.

## 8. Regression Dashboard Design

Start with generated static HTML plus a text summary, both backed by the same
validated records. This supports local review and later CI publication without
a server/database dependency. It is a planned interface, not implemented UI.

### 8.1 First Screen

```text
Baseline: <id>   Candidate: <commit>   Suite/protocol: <versions>
Target/environment: <fingerprint>     Evidence age: <time>
FAIL 2 correctness | REGRESS 3 | WARN 4 | PASS 25 | NO GPU 8 | NO BASELINE 2

Workload / Case       Correctness   Latency change [CI]  Roof fraction  Regret  Evidence
GEMM / tail ...       PASS          ...                  ...            ...     <receipt>
Reduction / ...       ...           ...                  ...            ...     <receipt>
Softmax / ...         ...           ...                  ...            ...     <receipt>
LayerNorm / ...       ...           ...                  ...            ...     <receipt>
```

Numbers above are layout placeholders, not observed results. First-screen
priority: correctness/guard failures, hard regressions, evidence/coverage loss,
then warnings and improvements. Display counts with denominators, including
disabled, failed, unsupported and unmeasured cases. Never let an aggregate
speedup hide an individual correctness failure or lost workload.

Filter by commit, baseline, target/SKU, workload, dtype/numerical class,
optimization, provider, timing scope and cache policy. Drill down from a cell to
raw samples, baseline/candidate IDs, code and WHIRL diffs, selection rationale,
phase logs, numerical report, profile and immutable receipt. Preserve source
file/line references and plan IDs for compiler debugging.

### 8.2 Status And Threshold Policy

| Status | Admission and display rule |
| --- | --- |
| FAIL | Wrong result, failed enforced guard, corrupted evidence or execution failure. Always visible; excluded from valid speedups but counted as failure. |
| REGRESS | Comparable native evidence confidently exceeds the reviewed practical latency/resource regression limit. |
| WARN | Potential regression with overlapping uncertainty, noisy timing, stale calibration or nonfatal coverage drift; triage required. |
| PASS / IMPROVE | Correct, comparable evidence passes the reviewed policy; improvement needs both practical magnitude and uncertainty support. |
| NO GPU / NO BASELINE / INCOMPARABLE / DISABLED | Explicit neutral states with reasons; never green performance passes. |

Use text/icons as well as color. Proposed initial latency policy for PERF0
review: a 5% practical limit with a 95% confidence interval; REGRESS when the
lower interval bound exceeds +5%, WARN when the estimate exceeds it but evidence
is inconclusive. PASS when the upper bound is within the limit; otherwise WARN.
IMPROVE requires the upper bound below a separately approved improvement limit.
These are proposals, not established acceptance thresholds. Store the approved
policy/version per comparison class, including absolute-time noise floors for
very short kernels. A policy change creates a new report revision, not altered
historical raw evidence. Define suite-level multiple-comparison handling and
confirmation runs before enabling release-blocking alerts.

Show per-case deltas and geometric mean speedup only over matched valid cases,
alongside coverage and failure counts. Keep kernel-only, region, application
and service metrics in separate panels. Service p99/SLO and resource regressions
need their own policies; a median kernel threshold does not govern them.

### 8.3 Regression Triage And Automation

Triage links follow: changed source/options/target -> candidate discovery ->
legality -> complete costs -> selection -> actual WHIRL transformation ->
emitted code/resources -> runtime/profile. Show whether a regression follows
a changed plan or occurs with the same plan, and whether cost predictions or
only native behavior changed. Permit grouping by suspected owning phase without
asserting causality from correlation alone.

Add history for latency, coverage, roof fraction, selector regret, compile/search
cost and resource use. Annotate baseline/toolchain/hardware changes as timeline
boundaries. Retain new results even when incomparable rather than connecting a
misleading trend line across that boundary.

Offline CI validates schemas, dashboard rendering, evidence links and failure
classification. Native CI can later run a bounded sentinel suite per change and
broader scheduled suites when hardware exists. Alerts and release blocking are
configured only after PERF0 policy approval and native stability validation;
this plan does not schedule a job, enable notifications or provision a GPU.

## 9. Artifacts And Reports

Follow the existing `artifacts/review/<domain>/<sync>/<milestone>/<receipt-id>/`
policy; choose the AI performance domain/sync labels in PERF0. Stage elsewhere,
then no-replace publish a fresh content-addressed receipt. Never delete or
overwrite review artifacts to refresh a chart. Dashboard indexes are rebuildable
derived views; only indexes/current pointers may advance after validation.

Retain workload/protocol/baseline manifests, exact commands/environment,
numerical oracle/results, raw timing CSV, optional raw counter/profile files,
compiler/options/target profiles, source, selected `.B`, independent
`ir_b2a -st -src` `.T`, relevant before/after images and diff, `.cu`, host/runtime
sources, available PTX/SASS/resources, binaries, raw `.t`, phase logs,
`SHA256SUMS` and receipt. Missing optional artifacts have explicit reasons.
Preserve phase `.t` under a noncolliding name before producing `.T` on
case-insensitive filesystems.

Use versioned manifests/receipts for structured metadata and CSV for samples.
Do not replace compact positional compiler traces with repetitive JSONL.
New reports start with `# What it summarizes:` followed by the description;
column schema/version follows once, then concise rows with stable IDs.

| Proposed report | What it summarizes |
| --- | --- |
| `performance-summary.log` | Evidence level, correctness, baseline/comparability, latency/speedup/uncertainty, coverage and native verification gaps. |
| `baseline.log` | Oracle and executable identities, admission, numerical/timing classes, baseline changes and refusal reasons. |
| `timing.log` | Boundaries, warmup/cache/replay protocol, paired estimator, noise/exclusions and raw sample locations. |
| `roofline-measured.log` | Native timing and separately attributed work/traffic/ceilings, theoretical versus sustained bounds, ratios and missing counters. Hybrid estimates explicitly labeled. |
| `selection-quality.log` | Predicted ranking, tested candidates, measured ranking/regret, budget, uncertainty and selected-plan changes. |
| `regression-summary.log` | Baseline/candidate/policy versions, failed/regressed/warned/unmeasured cases, deltas and evidence links. |

Preserve analytical `roofline.log`, existing phase logs and
`optimization-summary.log`; link them by receipt/region/candidate/plan IDs.
Optimization action reports summarize compiler activity; performance reports
summarize execution evidence. Do not concatenate native timing into a compiler
trace or change existing `optimization.log` aggregation semantics implicitly.

## 10. Delivery Queue

All implementation gates below are open. This document completes a draft plan,
not their acceptance. Existing optimization milestones continue independently.

| Gate | Deliverable and dependency | Acceptance and report/tool obligation | GPU? |
| --- | --- | --- | --- |
| PERF0: contract/reuse review | Freeze v1 workload/protocol/result/baseline schemas, target compatibility and numerical classes; inspect existing report/runner reuse; select optional native backend and dashboard policy. | Reviewed examples for valid, unknown, failed and incomparable cases; dependency/license inventory; exact command/option implementation status. `baseline.log` explains admission/refusal. | No |
| PERF1: offline package/report vertical slice | Extend existing GEMM packaging/import validation; share metric/status library; first static dashboard and text report from explicitly synthetic fixtures. Depends on PERF0. | Detect tampered hashes, absent/duplicate samples, invalid/nonfinite times, mismatched workload/target, unsupported schemas and missing baseline. Round-trip exact metadata; dashboard never labels synthetic timing native. Preserve source/WHIRL/trace links. | No |
| PERF1-S: maintained skill | Create and validate `open64-optimization-performance` around real PERF1 commands; keep repository logic authoritative. | A new engineer can package, inspect, import and triage fixtures without undocumented steps; unsupported native steps stop with clear gaps. No skill installation into protected paths without appropriate permission. | No |
| PERF2: native GEMM paired timing | Numerical admission then paired frozen-baseline/current measurement with optional NVBench backend; reusable local/remote runner. Depends on PERF1 and available hardware. | Retain raw data, uncertainty, first-run/steady-state separation and exact target/environment. Verify input changes and all completion paths. `performance-summary.log` makes only admitted native claims. | Yes |
| PERF3: counters, calibration and selector quality | Bind native profile runs, independently sustained ceilings and measured tested-candidate ranking. Depends on PERF2. | Counter/estimate provenance, unit/compute-class checks, above-roof diagnostics, plan coverage and regret. `roofline-measured.log` and `selection-quality.log` explain disagreements with analytical costs. | Yes; offline validation first |
| PERF4: workload/region expansion | Add reduction, Softmax and normalization as executable contracts become available; then fusion, Attention and MoE. Depends on PERF1 contracts; native claims on PERF2. | Each family supplies Section 11 extension record, independent oracle, compatible baselines and negative cases; region timing includes all work. Dashboard shows missing/unsupported cases instead of silently shrinking suite. | Mixed |
| PERF5: regression operations | Harden the PERF1 dashboard with native history, confirmed alerts and bounded CI suites. Depends on stable PERF2 data and approved policy. | UI/status fixture tests, no-baseline/no-GPU states, baseline transition boundaries, repeated-run noise study, confirmed regression injection and recovery, immutable links. No auto-baseline refresh. | Mixed |
| PERF6: model/service and cross-domain adapters | SGLang experiment plus later FHE integration through generic records; no compiler-core dependency. | Full-model/service latency-SLO and quality protocols; FHE correctness/security/runtime contracts and appropriate units remain domain-owned. Do not force ciphertext work into an AI TFLOP/s aggregate. | Target-dependent |

First actionable slice after review: PERF0 and then PERF1 on one GEMM case,
including its regression-dashboard row. Do not wait for a GPU, all operator
implementations or a complete web application to establish identity and evidence
rules. Keep native timing gates explicitly open until hardware is available.

## 11. Extension Record For Each New Optimization

Add one record per family in the registry, rather than copying the whole plan:

```text
ID / revision / owner / status:
Optimization and phase / minimum optimization level / compilation scope:
Semantic characterization and native discovery evidence:
Numerical contract / authorized alternatives / independent oracle:
Cases and fixed suite denominator / tails / adversarial inputs:
Complete candidate dimensions / lowerability and guard requirements:
Baseline IDs / external providers / comparison classes:
Timing boundaries / cache and replay policy / search budget:
Useful and executed work definitions / byte levels / compute ceiling classes:
Trace records / source-line and plan IDs / actual transformation evidence:
Reports and required dashboard cells / regression policy:
Offline tests / native numerical tests / timing and profile requirements:
Evidence receipts / unknowns / next gate / acceptance owner:
```

When extending a schema, keep stable field meaning and units. Add optional
fields for compatible extensions, bump schema versions for changed semantics,
and test old receipt readers or explicitly refuse unsupported versions.
Unknown facts remain unknown, not zero. Version thresholds and suite membership
separately from tool versions so historical results stay interpretable.

## 12. Open Decisions And Change Ledger

| ID | Decision required | Resolution gate |
| --- | --- | --- |
| D01 | NVBench adapter versus extension of the current CUDA-event runner; dependency/version/license cost. | PERF0; retain one common measurement contract either way. |
| D02 | Baseline choices per numerical class, including when an exact ordered vendor comparison is unavailable. | PERF0 and each workload admission. |
| D03 | Minimum samples, confidence estimator, noise floor, multiplicity and release thresholds. | PERF0 proposal; PERF2 stability study before release blocking. |
| D04 | Shared tool locations, result schema names and retention domain/sync labels. | PERF0 after existing-service inventory. |
| D05 | Profiler/counter availability on rented/shared/MIG hosts and acceptable missing-evidence behavior. | PERF3; never block basic timing on optional counters. |
| D06 | Dashboard publication access, CI hardware budget and alert owner. | PERF5; default local/offline until configured. |
| D07 | SGLang provider boundary and FHE-specific acceptance/metrics. | PERF6, reviewed with each runtime/domain owner. |

At each gate closure update its status, accepted receipt/commit, unresolved
limitations and next action in this plan and the main implementation plan.
Append a dated change-ledger row for substantive contract or policy changes;
do not overwrite the rationale for a previous decision.

| Version | Change | Evidence/implementation impact |
| --- | --- | --- |
| v0.1, 2026-10-29 | Initial research-informed measurement, baseline, roofline, extensible delivery queue, skill and regression-dashboard draft. | Documentation only; no new driver option, harness, dashboard, skill, scheduled run or GPU result. |
