# AIO-2 Candidate, Plan, Legality, And Cost Contract

## Status

Completed on 2026-09-24. This is the implementation contract for the second
check-only AI optimization milestone. AIO-2 is runtime-only, per-PU, and
additive. It does not change executable WHIRL, TensorDescriptorIR,
mapped-image sections, ELF numbering, frontend APIs, or lowering behavior.

## Open64 Design Continuity

Open64 LNO delays loop transformation until dependence, reuse, memory, loop,
and target-resource information are available. AIO-2 preserves that discipline
for tensor optimization: a legal alternative remains representational until a
complete plan has explicit legality and comparable cost evidence.

The initial common service does not reuse an LNO record because existing LNO
cost structures are loop- and phase-specific. Later target cost producers may
adapt LNO and target information into this common contract; they must not make
the common layer depend on LNO-private types.

## Ownership And Lifetime

1. One `DSL_OPT_PLAN_CONTEXT` belongs to one active PU and one verified
   `TensorEvolutionGraph`.
2. The normal VHO per-PU driver will eventually own both lifetimes. AIO-2 tests
   invoke the services directly before that orchestration hook is introduced.
3. Candidate, cost, plan, and membership records are immutable after insertion.
   Selection changes only the context's selected-plan result.
4. Records use stable graph-local and DSL-image IDs. They own no `WN *`,
   `ST *`, `CODEREP *`, frontend object, or borrowed string.
5. Cross-PU plans remain future IPA work.

## Candidate Contract

An optimization candidate records its kind, logical DSL node, source and
result TensorEvolutionGraph nodes, parent candidate, legality, rejection
reason, deterministic ordering key, and flags. Candidate IDs are contiguous
and local to the context.

The first candidate is the baseline. Every refinement names an earlier parent,
uses that parent's result as its source, and remains tied to the same logical
DSL operation and semantic tensor root. AIO-2 reserves common candidate kinds
without defining their later phase-specific payloads.

## Legality Contract

Legality and rejection reason are structured and must agree:

- `proven` requires reason `none`;
- `unknown` requires `incomplete_analysis`; and
- `rejected` requires a concrete failure such as ownership, descriptor,
  effect, resource, or malformed-reference rejection.

Plan legality is whole-plan legality. A proven plan may contain only proven
candidates. Legality alone never makes a plan selectable.

## Cost Contract

`CandidateCostIR` records six latency terms:

```text
T_total = T_compute
        + T_memory_unhidden
        + T_communication_unhidden
        + T_sync
        + T_launch
        + T_runtime_selection
```

Every known term has an amount, unit, confidence, and evidence source. A known
zero therefore remains distinct from an unknown term. Unknown is represented
only by the complete unknown tuple and is never interpreted as zero.

A cost is complete only when all six terms are known, use one comparable unit,
and sum without overflow. The record also names a nonzero target-profile ID.
Selection compares only complete costs for the requested target profile.

## Plan And Fallback Contract

A plan owns an ordered, duplicate-free candidate membership list, one cost,
whole-plan legality, deterministic ordering key, flags, and an optional earlier
fallback plan. The first plan is the proven, completely costed baseline. Every
non-baseline plan names an earlier proven and completely costed fallback for
the same target profile.

Selection scans plans in stable ID order and chooses the lowest complete cost.
Equal costs retain the earlier deterministic plan. Rejected, target-mismatched,
and incomplete-cost plans remain inspectable but cannot be selected.

## Search Budgets

Candidate and plan budgets are hard per-context limits. Exhaustion fails the
attempt before record insertion, preserves all prior IDs, records the relevant
exhaustion state, and never widens compiler scope. Ordering keys must increase
strictly, preventing a producer from publishing nondeterministic insertion
order under one context.

## First Vertical Slice

The fixed-shape `common.matmul` fixture creates:

```text
candidate 1: baseline, legality proven
candidate 2: provisional tiled alternative, legality proven
plan 1: baseline, complete relative cost, selected
plan 2: tiled alternative, unknown memory cost, fallback plan 1
```

The tiled plan remains unselected because one target cost is unknown. This is
an analysis result, not a tiled implementation and not a performance claim.

## Inspection And Certification

`DSL_Opt_Plan_Print()` emits deterministic candidate, term, plan, fallback,
and selection evidence. The service is intentionally absent from `ir_b2a`
because AIO-2 is runtime-only.

Certification requires independent repeated runs with identical graph and plan
traces; unknown-cost, malformed-cost, malformed-membership, target-mismatch,
post-selection mutation, and budget-exhaustion negatives; byte-identical
before/after `.B` files and `ir_b2a -st -src` traces; and successful `be.so`
and `lw_inline` link-closure checks.

The completed certification produced two candidates, two costs, and two plans.
The baseline cost is complete at 155 relative units. The tiled candidate has a
known compute estimate of 70 but an unknown unhidden-memory term, so its total
remains unknown and plan 1 is selected. Two independent planning processes
produced identical graph and plan traces. The no-plan and planned `.B` files
are byte-identical, as are their `ir_b2a -st -src` traces.

The retained local evidence family is
`artifacts/ai_optimization/aio2_opt_plan/`. It contains before, after, and
repeated-run binary WHIRL and ASCII traces, graph and plan traces, diagnostics,
commands, backend dependency/symbol evidence, and SHA-256 checksums.
