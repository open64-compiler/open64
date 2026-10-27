# Analytical GEMM Readiness Model

Version: v0.2, 2026-10-26. Owner: PU-local VHO P7/P8 joint selection at -O3.
This is a DSC design proposal and initial implementation, not a published
NVIDIA timing model. It replaces traffic-score overlap credit for joint
search without changing candidate legality, WHIRL image format, or lowering.

## Intent And Ownership

Minimize GEMM completion latency by balancing reuse, delivered bandwidth,
consumer work, and exposed operand-readiness waits. Preserve alternatives
until complete costs are available, then materialize only the selected plan
in WHIRL. Source emission remains a consumer of that WHIRL, not a selector.

VHO owns the pure analytical evaluator and the P7 ledger adapter. Common IR
continues to carry policy-free plans and cost records. No simulator, Python,
GPU, new library dependency, or cross-PU traversal is required by compilation.

## Terms Before Use

| Term | Definition |
| --- | --- |
| D | Direct GMEM-to-register scalar F32 implementation, without staging. |
| S1 | One-slot synchronous A/B SMEM staging and scalar F32 consumption. |
| T2 | Two-slot TMA A/B staging with explicit wait and release ordering. |
| Reduction step | One scalar K iteration for D; one BK tile for S1/T2. |
| Consumer time C | Padded scalar arithmetic plus modeled local operand/address service per step. Not Tensor Core throughput. |
| Transfer service S | Aggregate input bytes for the active wave divided by shared delivery bandwidth, rounded up. |
| Fixed latency L | Delivery startup after issue/service; excludes transferred-byte service. |
| Ready time | Issue/engine-service completion plus fixed latency. |
| Prefetch window | Adjacent consumer work plus release/wait boundaries available to cover a refill; diagnostic, not a second overlap discount. |
| Exposed wait | Clock advancement at a required readiness edge beyond the consumer clock. Already included once in completion. |
| Grid wave | Up to the Hardware Description IR's SM count in CTAs, with one CTA per SM in this initial model; waves execute sequentially. |
| Reference cycle | Unit of the explicit assumed profile, not a measured GPU cycle prediction. |

## Controls And Profile

`-Wb,-DSL:gemm_plan_search=joint` defaults to `gemm_cost_model=readiness`.
`-Wb,-DSL:gemm_cost_model=relative` preserves joint model 3 exactly.
Baseline/non-joint compilation does not change. Existing -O3, executable
preflight, target, host-provider and trace gates still apply.

`-Wb,-DSL:gpu_arch=sm_90` selects the exact built-in Hardware Description IR.
The default preserves `sm_90`; `sm_90a` is distinct. The common registry also
provides Blackwell planning rows, but CUDA emission for those remains rejected.
See [target selection](AI-COMPILER-HARDWARE-DESCRIPTION-TARGET-SELECTION.md).

All quantitative values now come from that description: per-path startup,
store completion, publication/wait/release/issue costs, SM/wave count, warp
width and delivery/compute/local/copy rates. The evaluator and adapter have
no private hardware delay/rate defaults. Initial common table values preserve
the earlier synthetic envelope and are marked `estimated`; **centralization
does not turn them into measured NVIDIA specifications**.

`-Wb,-DSL:gemm_memory_latency=N` (0..1000000) is now an explicitly supplied
diagnostic override of memory startup fields in a local description copy.
There is no option-owned default delay. Explicit zero is distinguishable from
omission and recorded in the trace. Built-in tables are never mutated.

The historical model-4 `assumed_v1` profile and old results below remain
readable for immutable receipt review; new production traces use model 5.

## Ledger Adapter

Derive traffic from the existing audited source-warp GMEM ledger (D/S1)
or replicated payload ledger (T2), including output sectors. Divide whole
GEMM traffic by CTA/step count, rounding upward; tails therefore use average
traffic, not an exact last-CTA timing prediction. Consumer arithmetic uses
the full padded CTA tile. Local load issues, bank-conflict excess, and address
work are divided by the description's local issue service. S1 also pays
scalar copy work using the description's staging service rate.
The initial local-work aggregate conservatively includes copy bank excess
for T2; separate consumer/copy breakdown is a future precision improvement.

Register/SMEM/thread limits remain hard executable legality gates. The old
fractional-capacity pressure penalty is not added to a time estimate. Native
register allocation, register-granularity occupancy and multi-CTA residency
are not modeled. The one-CTA/SM envelope is explicit, not an occupancy proof.

## Schedule Algorithm

```text
for each legal or rejected complete candidate in stable enumeration order:
    derive CTA count, step count, consumer time, input/output bytes, copy work
    cost full hardware-description waves and one possible partial wave
    D/S1:
        exposed_startup = ceil(L / min(warps_per_CTA, hardware_hiding_limit))
        step = C + input_service + exposed_startup + scalar_copy
        S1 additionally pays two barrier macros per step
    T2:
        issue initial min(2, steps) A/B tile pairs
        serialize their transfer service; record separate ready times
        publish initial slots
        for each step with a later refill:
            clock = max(clock, next_slot_ready) + wait + C + release
            clock += issue_pair
            service_end = max(service_end, clock) + input_service
            refill_ready = service_end + L
            rotate next/following slots
        consume and release final one/two slots without a phantom refill
    drain output transfer completion
select minimum complete total, then stable candidate ordinal
retain cheapest legal S1 as compile-time fallback
rebuild winner using identical controls; only then transform WHIRL
```

D/S1's inter-warp allowance is an explicit coarse latency-hiding assumption,
not a claim of no hiding, and not an instruction-level proof. D uses actual K
steps; S1 uses BK steps. T2 has no invented independent producer warp: its
issue delay advances the consumer clock. The two-slot slot-release edge
prevents overwriting data still being consumed.

Repeated T2 boundaries form a four-variable max-plus recurrence over consumer
clock, next-ready, following-ready, and transfer-service end. A constant-size
matrix power computes it in O(log K) time. This is analytical recurrence
evaluation, not an event queue or source-warp instruction simulation. Tests
compare the accelerated result against a separate literal step oracle.

## Cost Accounting And Gates

```text
total = consumer_compute + exposed_movement_and_copy + synchronization
        + transfer_issue
```

For T2, exposed movement is recurrence completion minus compute, barriers,
and issue; output service/startup is then added. Startup, drain, refill and
bandwidth serialization occur once. Diagnostic wait is not added again.
Communication and runtime-selection terms are zero in this static device
slice. The historical `launch` cost slot carries transfer-issue overhead,
not CUDA host launch or tensor-map creation. Host application latency remains
outside this kernel-boundary model. Roofline is a separate lower-bound
diagnostic, never an additive cost or an unexplained winner override.

All candidates in one census use the same profile and cycle unit. Checked
overflow or malformed input fails closed before selected WHIRL publication;
there is no partial fallback to relative scores within a readiness census.
Analytical selection is assumption-sensitive, not confidence-certified. Users
can retain legacy relative selection and compare latency sensitivity. Tiny
predicted differences must not be reported as measured gains.

## Trace And Tool Requirements

Existing option-controlled `.t` records retain source lines and stable IDs.
Joint model 5 uses `cycles low` and records the resolved hardware description.
Models 1..4 remain readable without changing historical evidence.

```text
HW_DESC architecture table family revision unit provenance latency_override
HW_PERF sms warp_lanes memory_bytes_per_cycle scalar_ops_per_cycle local_issues_per_cycle copy_elements_per_cycle latency_hiding_warps demand_latency staged_latency tma_latency store_latency publication_cycles wait_cycles release_cycles operand_issue_cycles
JOINT_READINESS id CTAs steps C input_bytes output_bytes copy warps delivery window wait waves total source_line
```

`JOINT_CAND` remains the authoritative cost/legality census. The trace tool
must recompute the adapter, recurrence, resource legality, winner, fallback
and exact readiness-record coverage; reject missing, duplicate or corrupt
records. `physical-plan.log` and `optimization-summary.log` must name the
profile, assumptions, selected/fallback IDs, and explain readiness versus
legacy traffic-score selection. Each phase log retains its first-line
"What it summarizes" convention. Keep raw `.t`, reports, before/after WHIRL,
CUDA, commands and checksums in the milestone artifact.

## Validation And Follow-Up

Required: literal recurrence oracles (one/two/many steps, latency and bandwidth
limits), overflow/malformed tests, independent trace recomputation, legacy
parity, actual compiler winner reconstruction, emitted WHIRL/CUDA host-only
correctness, trace-off and invalid-option atomicity. No GPU speedup or native
TMA certification is inferred from these checks.

Next: measured target profiles and confidence/range-aware choice, exact tail
classes, multi-CTA residency, distinct issue/latency models for real native
instructions, cache and queue effects, and native launch costs. Use the
[shadow simulator](AI-COMPILER-OPTIMIZATION-BUBBLE-MODEL.md) as an independent
diagnostic and GPU measurements as calibration/validation, not as a mandatory
compiler dependency or a synthetic ground truth.

## Historical Model-4 Compiler Results

Current model-5 acceptance and immutable evidence are in
[Hardware Description IR acceptance](AI-COMPILER-HARDWARE-DESCRIPTION-TARGET-SELECTION.md#acceptance-evidence).
The built-in row preserves the initial default numerical result while
removing private timing/rate defaults. Thirteen target/override/source-review
configurations and 104 tests pass; none imply measured GPU performance.

The compiler/source review exercises ten analytical/relative configurations
including five shapes, latency 20/100/400, explicit/default model parity and
TMA disabled. The following are actual compiler selections, not simulator
shadow winners. Candidate IDs retain the stable original census order.

| GEMM | Legacy relative winner | Analytical winner at L=100 | Analytical total |
| --- | --- | --- | --- |
| 1 x 1 x 1 | D #2 | D #2 | 192 |
| 1 x 1 x 17 | D #2 | D #2 | 1648 |
| 3 x 5 x 7 | D #2 | D #2 | 856 |
| 65 x 67 x 17 | D #8 | S1 #1 | 8061 |
| 128 x 128 x 144 | S1 #12 | T2 #3 | 31336 |

Totals are assumed-profile reference cycles, **not measurements**. For the
last shape, the analytical costs of best S1 #1 and old incumbent S1 #12 are
43237 and 47989 respectively. Never compare these numerical values directly
with relative model 3's 9279 score. Turning TMA off selects S1 #1.

T2 #3 remains selected at latency 20/400, with totals 31176/31936. Unlike the
source-warp simulator, this coarse model does not switch that shape back to
S1 at latency 20. Retain this disagreement as a calibration question, not a
reason to tune coefficients to reproduce the simulator. Copy/consumer issue,
bank/address work and inter-warp hiding assumptions differ. This is a
complete-plan change, not an isolated prefetch speedup claim.

Validation includes 102 Python tests, 2340 literal T2 recurrence oracles,
large-K and malformed/overflow checks; independent trace ledger/cost/winner
audits; actual before/after WHIRL reopen, CUDA schedule inspection, and
host-only reference execution with zero reported error. GPU timing,
native correctness, PTX/SASS fidelity and target calibration remain open.

## Historical Model-4 Review Evidence

Compiler checkpoint: `de7256b7`. The immutable review contains 1119
checksum-verified files: acceptance commands/build and symbol logs, source
snapshots, ten analytical/relative configurations, legacy D/S1/T2 and controls,
linked AIO-10 malformed/resource contracts, raw traces, reports, before/after
WHIRL and generated CUDA. Durable root:

```text
/Users/shinmingliu/open64/artifacts/review/ai-optimization/m7/analytical-readiness/sha256-9e4d7678b42ff5cfbb543b82d12ac55f39061935fdb371d39046cf09e934b5d0/
```

Start with `acceptance/analytical/staged/reports/optimization-summary.log`,
then `reports/phases/physical-plan.log`, `gemm.t`, `gemm.whirl.cu`, and the
`gemm.before/after.B/.T` and `gemm.before-after.diff` in the same case directory.
After promotion, the durable staged `.B` was independently reopened with
`ir_b2a -st -src` and matched the retained `.T` byte-for-byte. Fresh raw-trace
report replay also reproduced the retained summary exactly.

Impact review overrides the FHE selector's unknown-AI-path full-model fallback:
the new production calculation is joint-only, with no FHE producer/generic
materializer/IR-format change. Focused production builds, native contracts,
old/new source-review runs and independent audits exercise the changed
contracts. Unrelated full ResNet certification was not rerun. No existing
durable receipt was overwritten, pruned or removed.
