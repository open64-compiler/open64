# GEMM Bubble Prediction And Candidate-Selection Experiment

Date: 2026-10-26. Status: design proposal for algorithm review; no simulator,
new option, cycle-based selection or changed candidate winner is implemented
by this document. Scope: one active PU, `-O3`, fixed-shape contiguous F32
GEMM, existing lowerable D/S1/T2 plans. No fusion, new precision permission,
runtime variants or binary WHIRL format change.

## 1. Intent And Success Criterion

Minimize the latency of completing a GEMM. Resource utilization is a means,
not the objective: reduce operand demand through reuse, deliver remaining
demand efficiently, and issue movement early enough that useful computation
does not wait. More allocated memory, resident warps, transfer bandwidth or
pipeline stages is not automatically better.

Hypothesis: a dependency- and resource-constrained schedule model can explain
and improve candidate ranking beyond the existing relative overlap formula.
Delayed transformation lets us evaluate complete alternatives without first
rewriting the authoritative WHIRL. The initial experiment holds the candidate
census, legality, executable lowering and input semantics fixed, changing only
the diagnostic model. Report changed AND unchanged winners; do not force a
ranking change to demonstrate progress.

The longer-term hardware/software co-design objective is to reduce reliance
on broad empirical search and expose WHY a hardware change helps a plan.
This is not a claim to eliminate measurement or outperform Triton autotuning.
An uncalibrated model can be systematically wrong. Native validation and
optional targeted benchmarking remain evidence and calibration mechanisms.

## 2. Terms And Timing Contract

| Term | Definition |
| --- | --- |
| D / S1 / T2 | Existing direct GMEM/register plan, one-slot synchronous SMEM plan, and two-slot TMA plan, respectively. No Tensor Core computation is implied. |
| Event | A bounded instance of an operation or reviewed operation group, with actor, source identity, predecessors, resource demands and completion tokens. |
| Actor | Warp, warp group or asynchronous engine permitted to advance independently by the actual implementation. Do not invent independent producer/consumer warps. |
| Reference cycle | One cycle in the explicitly named model clock domain. Convert memory/engine service into this domain; clock ratios and units are profile inputs. |
| Latency | Time from issue to the named result/completion token becoming ready. Different results may have different ready times. |
| Initiation interval | Minimum spacing between accepted operations on a resource; distinct from completion latency. A pipelined operation need not occupy its issue resource until completion. |
| Service demand | Bytes, transactions or instruction work consuming a modeled shared resource. Aggregate service cannot exceed its bandwidth/issue envelope. |
| Operand-ready time | Completion time of the producer and any required visibility/synchronization event. Async instruction issue is not data readiness. |
| Dependency wait | Time an actor cannot advance because a required value, token or synchronization condition is unresolved. It is not necessarily compute-unit idle time. |
| Issue bubble | Unused issue capacity on a named compute resource, while compatible unfinished GEMM computation remains assigned or admissible to that resource. A full initiation slot is not a bubble. |
| Execution idle | Unused execution-service capacity, modeled separately from issue bubbles. An outstanding long-latency operation need not keep every compute lane busy. |
| Completion latency | Final required output-completion time minus the declared start boundary. Distinguish device-kernel completion from host-to-host application latency. |
| Scenario | A coherent set of cache-service, timing, scheduling and residency assumptions shared by all candidates in one comparison. Scenario extrema are not proven statistical bounds. |

For a specific load, let `t_load` be issue time, `latency` its modeled result
latency and `t_consumer_without_edge` the consumer's earliest issue time when
only that dependency is ignored under the stated scheduling assumptions:

```text
available_distance = t_consumer_without_edge - t_load
dependency_delay = max(0, t_load + latency - t_consumer_without_edge)
```

This is an explanatory dependency diagnostic, not an additive kernel cost.
Removing an edge can change scheduling; use a defined counterfactual replay
for an exact modeled comparison or label a frozen-schedule estimate as such.
Do not derive elapsed cycles by counting assembly instructions.

For an interior tile with equal input element size `b`, input bytes are
`b*BK*(BM+BN)` and useful work is `2*BM*BN*BK`. Their ratio is
`2*BM*BN/(b*(BM+BN))`: increasing `BK` alone does not improve ideal input
reuse. It changes transfer granularity, synchronization amortization,
storage and overlap. Include tails, CTA replication and output traffic in
the actual plan, not only this illustrative interior formula.

## 3. Inputs, Ownership And Existing Open64 Services

P7 provides topology, lane ownership, access/layout alternatives and resource
ledgers. P8 supplies real movement, acquire/issue/wait/release and reuse
dependencies. P9 owns complete-plan comparison, diagnostic ranking and any
later selection. P11 owns native feedback, not retroactive legalization.
VHO analysis remains active-PU-local and read-only until the existing selected
plan commitment point. The driver still owns device-image publication.

Use existing CommonTilePlanIR, CommonFetchPlanIR, CommonPipelineIR,
MemoryHierarchyDescriptorIR, ComputeResourceIR and CandidateCostIR contracts.
Proposed reusable event/cost evidence may be runtime-only common records;
`common/com` owns structure, IDs, verification and printing, never scheduling
policy or profitability. GEMM work counting and NVIDIA instruction/memory
adapters remain domain/target owned. Do not expose CUDA or simulator types in
public common headers or add backend shared-library dependencies.

Before coding, audit these integration anchors:

- `osprey/be/lno/cache_model.h` and `.cxx`: locality and hidable/non-hidable
  memory-cycle reasoning; retain the distinction between cache policy and
  explicitly managed staging buffers.
- `osprey/be/cg/cg_dep_graph.h` and `.cxx`: operation dependencies and latency
  arcs; `cg_swp_sched.cxx`: dependency/resource-constrained scheduling.
- `osprey/be/cg/cg_sched_est.h` and `.cxx`: critical-length and resource-cycle
  estimates using `TI_RES_COUNT`.

Reuse established facilities where applicable. These CG services consume
target operations and BB/TN state, not provisional tensor plans; an early
WHIRL event adapter must document that representation mismatch instead of
fabricating GPU operations or linking VHO to a new independent pass manager.
Do not claim the existing x86/MIPS timing tables describe NVIDIA hardware.

Each hardware profile must identify revision, provenance and confidence for:
instruction/result latency, initiation interval, issue width and compatibility,
scheduler-to-warp assignment policy, register/SMEM allocation granularity,
resident block/warp limits, memory-path latency and aggregate service rate,
transaction/bank rules, outstanding-request limits, async-engine queues,
barrier behavior, clock domains and applicable launch overhead.

Architectural limits, synthetic assumptions and measurements are separate
evidence classes. Missing required facts yield `unknown`, never zero. An
explicit assumed-residency scenario permits exploratory simulation but is not
a native occupancy prediction. Keep cache-hit/miss scenarios separate;
source-requested sectors are not automatically HBM traffic. A synthetic
profile must not be labeled a calibrated H200 or Blackwell profile.

Validate profile identity against the candidate's target and instruction
family. A timing scenario cannot override architectural legality or invent
resources in the emitted kernel. Future co-design runs record both fixed-plan
sensitivity and reoptimized results per hardware revision, rechecking legality
and regenerating the legal census when the hardware envelope changes. The
first same-census shadow experiment is narrower than that full design space.
Any future result-cache identity must include workload/layout/numerical and
candidate identities, controls, hardware/profile/model/scenario revisions and
the lowering/toolchain revision; compile-artifact and measurement caches are
separate. Stale evidence must not silently survive a hardware revision.

## 4. Candidate And Event-Graph Algorithm

Initially evaluate the existing deterministic candidate census (currently at
most 48 tile/movement pairs). Use the shared executable preflight and retain
the existing best legal S1 compile-time fallback. Do not enable a candidate
that the current host/provider mode cannot launch. Do not introduce deeper
stages, warp specialization, clusters or persistent scheduling as hypothetical
performance credit before their lowering exists.

```text
COMPARE_PLANS(active_pu, controls, profile, scenarios):
    require canonical input, active-PU scope and -O3
    census = existing_joint_enumeration_without_mutation(active_pu)
    current = existing_model3_selection(census)  # unchanged cost units
    require current and the existing legal S1 fallback
    freeze census, source/plan hashes, controls and model/profile revisions
    for candidate in census in stable ordinal order:
        if not shared_executable_preflight(candidate, controls):
            record existing rejection; continue
        graph = BUILD_EVENTS(candidate, profile)
        for scenario in scenarios:
            if graph or required profile inputs are incomplete:
                record UNKNOWN(candidate, scenario, missing_facts); continue
            result = SIMULATE(graph, profile, scenario, event_budget)
            check complete coverage and compatible Roofline/resource bounds
            record result or explicit UNKNOWN/INVALID_MODEL diagnostic
    for scenario in scenarios:
        if every legal candidate has a complete, consistent result:
            hypothetical = argmin(completion_cycles, stable_ordinal)
            compare hypothetical with current using SAME cycle model
            record changed/unchanged winner, margin and causes
        else:
            record incomplete comparison; do not claim a global winner
    emit shadow comparison and retain current as the actual selection
    return current to the existing transactional WHIRL lowering path

BUILD_EVENTS(candidate, profile):
    obtain a read-only schedule recipe from the same plan/lowering contract
    instantiate bounded CTA, warp and K-step events, including edge masks
    map actual address calculations, load instructions and F32 arithmetic
    add value dependencies and instruction-order restrictions per actor
    add alias/range hazards and accumulator recurrence dependencies
    for each staging slot and reuse generation:
        add acquire -> issue -> completion/visibility -> wait -> consume
        add consumer completion -> release -> next acquire/overwrite
    add CTA barriers and required async completion dependencies
    include stores, prologue, epilogue, startup and drain
    attach transaction/bank demand, issue resources and completion tokens
    verify geometry, token generations, capacities and acyclic expansion
    return graph, or UNKNOWN if expansion exceeds the reviewed budget
```

The recipe adapter must match emitted WHIRL operations and actor behavior.
It is diagnostic, not a second CUDA emitter or a speculative mutation of the
authoritative PU. Account for address/control work explicitly or declare it
unmodeled; no full-latency claim from an unexplained compute macro-event.
Larger graphs may use compressed periodic schedules only after equivalence
checks against fully expanded fixtures. Never rank partially simulated plans.

## 5. Resource-Constrained Simulation Algorithm

```text
SIMULATE(graph, profile, scenario, budget):
    state = initialize_empty_queues_calendars_tokens_and_round_robin_policy()
    t = 0
    while not all required output events have completed:
        enforce checked arithmetic, event/time budget and forward progress
        process all completions at t; publish tokens and release resources
        if all required output events have completed: break
        admit pending CTAs using scenario residency and actual allocation rules
        repeat:
            ready = actor_heads_with_resolved_dependencies_and_visibility(t)
            legal = filter_by_issue_slots_banks_queues_and_resource_limits(ready)
            chosen = deterministic_target_policy(legal, state)
            if chosen is empty: break
            for event in chosen in stable order, rechecking shared capacity:
                reserve initiation/service capacity and outstanding queue entry
                completion = schedule_service_and_result_readiness(event, t)
                enqueue completion; advance actor only as semantics permit
        next_t = earliest completion, admission or issue/resource state change > t
        if next_t does not exist: return UNKNOWN(deadlock_or_missing_model_rule)
        integrate resource service and classify idle/issue opportunities [t,next_t)
        record actor dependency waits separately; t = next_t
    return output completion time, event timeline, resource/cause summaries
```

Memory service must conserve bandwidth across concurrent requests. A request
can have fixed access latency, transferred service and queue delay; each
component is charged once according to the declared memory model. Do not
give every outstanding transaction independent full-device bandwidth, or
both charge transfer duration and include it again inside a latency constant.
Model finite queues and backpressure. SMEM conflicts affect service, not just
allocation bytes. Schedule independent warps only on their assigned resources;
an idle warp is not an idle SM, and an issuing address operation does not
prove the F32 compute pipeline is productive.

For each compute resource and time interval, separate: useful service,
unused capacity with pending compatible computation, and no assigned demand
(including grid underfill/final waves). Classify unused issue opportunities
by a documented priority: operand/visibility dependency, synchronization or
slot ownership, competing issue/queue resource, then front-end/control.
Preserve multiple blockers as detail but assign only one primary bucket per
opportunity; explicit `other/unknown` covers unmodeled cases. Report per-unit
cycle-equivalents separately from kernel elapsed cycles. Never sum overlapping
warp waits, resource bubbles or critical-path causes into completion latency.

The simulator's output completion time replaces the old overlap formula
inside a future cycle model. It is NOT added to relative model-3 costs, a
second memory-hidden credit, or a Roofline bound. Kernel and application
boundaries must match; host allocation/transfers are outside the first kernel
experiment and remain explicitly excluded, not free.

## 6. Review And Selection Controls

Proposed, NOT registered:

```text
-Wb,-DSL:gemm_bubble_model=off|shadow|select
-Wb,-DSL:gemm_bubble_profile=<versioned-profile-path>
```

`off` is the compatibility default; `shadow` is the first implementation.
Both shadow and future select require `gemm_plan_search=joint` and the existing
level/output controls. Parse/validate the profile through reviewed existing
structured-data facilities, not a new `be.so` dependency. Invalid controls
diagnose before publication; missing modeling facts produce unknown evidence.

`select` is reserved until separately reviewed. It must require complete
comparisons, the same executable legality/fallback gate, consistent units,
and a declared confidence policy. A possible conservative rule is that the
challenger's largest modeled time beats the incumbent's smallest modeled
time by a positive configured margin across the declared uncertainty set.
Those scenario extrema are not hardware guarantees. Otherwise retain the
incumbent with `uncertain_ranking`; do not silently promote a nominal shadow
winner. Explicit synthetic-profile selection is test-only, not native tuning.
Recheck the selected plan before transactional WHIRL materialization; failure
must preserve existing no-partial-publication behavior.

## 7. Experiment, Trace And Tool Summaries

The experiment has two independent obligations: synthetic hand-calculated
oracles prove the algorithm; real compiler candidates show whether the new
model changes a ranking. A synthetic reversal is not evidence that our current
GEMM winner changed or that GPU execution became faster.

Use existing tail `3x5x7` and staged `128x128x144` cases, then bounded shapes
covering small grids, edges and multiple K steps. Run identical candidate
censuses through model 3 and the shadow model. For each hardware revision or
scenario sweep report candidate coverage, old winner, hypothetical winner,
actual emitted winner, modeled incumbent/challenger cycle times, margin,
dominant cause and ranking sensitivity. If nothing changes, explain why;
do not tune assumptions merely to manufacture a reversal.

Keep the option-controlled WOPT `.t` stream and existing AI trace mask. The
following positional rows are PROPOSED additions to AI_OPT v7, with a
separately identified bubble model revision; do not emit them until producer
and auditor compatibility tests exist. Every row ends with the input line.

```text
BUBBLE_MODEL model_id profile_id census_id mode scenario_count source_line
BUBBLE_PROFILE profile_id revision reference_clock provenance confidence source_line
BUBBLE_SCENARIO scenario_id profile_id assumption_set_id source_line
BUBBLE_COST candidate_id scenario_id status completion_cycles issue_bubble_cycle_equivalents actor_wait_cycles confidence reason source_line
BUBBLE_WAIT candidate_id scenario_id unit_id cause begin end blocked_event producer_event source_line
BUBBLE_COMPARE scenario_id old_id shadow_id emitted_id incumbent_cycles shadow_cycles reason source_line
```

Use `NA` for unavailable fields, not zero. Model/profile/scenario identities
resolve to retained immutable manifests; overflow, unknown, inconsistent and
budget-exhausted results carry reasons, never fabricated cycle values. Detail
rows may be separately enabled/bounded without suppressing the full summary
or changing the simulation. `BUBBLE_WAIT` covers attributed compute issue
opportunities, not all actor waits; event evidence separately retains actor
state and capacity multiplicities. Do not overload authoritative JOINT_SELECT
with a hypothetical winner or reuse a model-3 total column for cycles.

| Log | What the analyzer must summarize |
| --- | --- |
| `tile.log` | Reuse/traffic/topology and occupancy assumptions that explain differences between candidates; existing actual selection stays authoritative. |
| `fetch-pipeline.log` | Data-ready versus consumer-demand timing, stage ownership, queue backpressure, fill/drain and residual waits. |
| `physical-plan.log` | Complete candidate coverage, legality/resources, model-3 score separately from scenario cycles, both rankings, actual selection and reason. |
| `bubble.log` (new, proposed) | Per-candidate/scenario completion and compute issue/execution utilization, wait causes, unknown facts, sensitivity and no-double-count accounting. |
| `optimization-summary.log` | Old/shadow/emitted winners, changed/unchanged/incomplete outcome, model confidence and native-validation status. |
| `optimization.log` | Include completed phase report bodies, including `bubble.log` when run; summary records skipped/disabled/unknown states. |

Each report starts with `What it summarizes: ...`. The tool must independently
validate event precedence, issue/service capacity, buffer-generation lifetime,
completion totals and winner/tie rules from retained evidence, not trust the
compiler's selected row. Reject duplicate/missing candidates and manifests.
Store profile/scenario manifests, event graph/timeline, raw `.t`, all reports,
sources, before/after `.B/.T`, diffs and checksums in a new immutable review
receipt. Shadow on/off must preserve selected WHIRL and CUDA semantics.

## 8. Algorithm Oracles And Native Follow-Up

Required deterministic tests: load-use slack with a hand-known wait; one
waiting warp covered by another versus all assigned warps blocked; initiation
interval distinct from latency; finite memory bandwidth/queues; accumulator
recurrences; bank conflicts; all barrier participants; T2 slot generation and
release-after-consumption; fill/drain; grid underfill; output completion;
overlapping causes; unknown facts; deadlock; overflow; event-budget failure;
determinism and scenario-dependent ranking reversals. More stages must not
create unlimited bandwidth, and no asynchronous credit goes to synchronous
code. Preserve no-optimization/option/scope and multi-PU isolation tests.

Later native refinement maps candidate/event/source identity to emitted SASS,
using `nvdisasm`/`cuobjdump`, and imports actual register/spill/instruction
evidence. Assembly can refine dependencies and scheduled work but cannot by
itself reveal cache hits, contention or exact execution cycles. Compare
unprofiled kernel timing separately from sampled scheduler/memory counters;
report prediction error, rank agreement and regret against measured tested
alternatives. Do not label sampled warp stalls as exact idle-cycle totals.
No NVIDIA host is required for synthetic/compiler-only work; GPU calibration
and native correctness/performance acceptance remain open.

## 9. Research Inspiration Versus DSC Proposal

- [DeepGEMM SM90 heuristics](https://github.com/deepseek-ai/DeepGEMM/blob/main/csrc/jit_kernels/heuristics/sm90.hpp)
  motivate explicit stage/traffic/wave reasoning; they are not this simulator.
- [Triton autotune API](https://triton-lang.org/main/python-api/generated/triton.autotune.html)
  documents empirical configuration evaluation and optional pruning/modeling.
  Our goal is less blind search, not a claim that Triton has no analysis.
- [Nsight Compute Profiling Guide](https://docs.nvidia.com/nsight-compute/ProfilingGuide/index.html)
  distinguishes eligible/issuing warps, pipelines and sampled stall evidence.
- [CUDA Binary Utilities](https://docs.nvidia.com/cuda/cuda-binary-utilities/index.html)
  provide the later binary inspection path, not a hardware timing oracle.

The event adapter, bounded shadow experiment, confidence-gated selection and
compact audit contract above are DSC design proposals. Resource-constrained
scheduling and delayed transformation have established compiler precedents;
no novelty or superiority claim follows from adopting them.
