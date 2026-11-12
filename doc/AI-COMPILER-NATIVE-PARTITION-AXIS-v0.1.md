# Native Partition-Axis Discovery

Version: v0.1

Date: 2026-10-30

## Purpose And Scope

Implement the first bounded R1-ND1 discovery slice: derive independent instance
and contribution axes from ordinary scalar WHIRL, without a framework operator,
`normalized_shape`, tensor metadata, dimension/parameter names or a largest-extent
heuristic. This is compiler analysis, not the deferred performance framework.

For `X[I,K]`, I denotes independently initialized statistics/output instances;
K denotes the values contributing to each ordered statistic. Logical tensor
axis numbers come from their access relations, not loop position alone.
Thus `X[I,K]` has instance axis 0 / contribution axis 1, while `X[K,I]` has
instance axis 1 / contribution axis 0. Equal extents do not change this rule.

Identifying I/K does not select threads/warps/CTAs, authorize a parallel reduction
tree, classify a complete LayerNorm/RMSNorm recipe or bind target arithmetic.
Those are subsequent legality, numerical, lowerability and profitability gates.

## Ownership And Existing Services

The implementation belongs to LNO because it consumes canonical loop/access
facts in the active driver-selected PU. It runs after LNO builds access maps,
before short-loop unrolling and subsequent locality transformations. The normal
LNO driver first invokes PREOPT and supplies DU/alias services. Ordinary C CFG
loops may therefore become canonical DO_LOOP nodes through existing services;
the frontend need not emit a special normalization or reduction operator.

| Existing service | Reuse |
| --- | --- |
| `WN_LOOP_LowerBound`, `WN_LOOP_UpperBound`, `WN_LOOP_Increment` | Inspect exact canonical bounds and step. Do not use `WN_LOOP_TripCount` here because it allocates/folds trees and temporarily enables simplification. |
| `REDUCTION_MANAGER` | Recognize the scalar additive recurrence with a private temporary map. Do not modify the reassociation-controlled global reduction manager or FP options. |
| PREOPT `DU_MANAGER` | Prove recurrence reaching definitions and nonescaping uses; follow one pure contributor temporary through an exact DU edge. No parallel expression-substitution engine. |
| LNO `ACCESS_ARRAY` / `ACCESS_VECTOR` | Map instance/contribution loop depths to distinct logical input dimensions; prove output ownership and full rectangular coverage. No second affine parser. |
| `Valid_alias` / `Aliased` | Require proved input/output nonaliasing, including union-overlap refusal. No assumption that distinct field names imply distinct allocations. |

No common/com optimization policy, cross-PU traversal, new dependency, native
DSL opcode or mapped-image layout change is introduced. Common records and
existing v1/v2 explicit normalization contracts remain unchanged.

## Bounded Admission Algorithm

```text
discover(active_PU, current_DU, current_alias, current_LNO_maps):
    require active FUNC_ENTRY and established driver/map lifetimes
    build private scalar reduction-recognition map
    for outer counted nest in active_PU:
        require body = [private +0.0f initialization, inner loop, output store]
        derive positive static I4 zero-based unit-step domains I and K
        require distinct induction-variable identities, including PREG offsets
        derive R=|I| and N=|K| with checked bounds; do not inspect names
        require inner update = state + contribution, preserving self operand
        optionally follow one preceding pure local temporary through exact DU
        require contribution = input load or product of identical input loads
        prove state definitions are initialization/update and uses do not escape
        obtain affine input/output relations from LNO access vectors
        match I and K to distinct input axes with coefficient 1, offset 0
        reject other loop coefficients, symbolic/nonlinear terms and bad extents
        prove output[I] gives unique full instance coverage
        require known nonaliasing input/output and nonvolatile accesses
        publish borrowed read-only fact, or explicit refusal with no permission
    release private recognition map; preserve the original WHIRL
```

The first admitted forms are static rank-2 FP32 input and rank-1 FP32 output,
ordered sum or squared-input sum with private positive-zero initialization,
ascending unit-step I4 loops, complete rectangular dimensions and direct
affine accesses. A direct local contributor temporary is supported through DU.
Exact ADD operand position is retained because semantic discovery must not
silently change exceptional-value behavior. Source arithmetic/environment
binding remains separate even when these axes are proved.

Dynamic bounds, nonunit steps, partial/predicated coverage, offsets/skew,
arbitrary flattening, multiple feature axes, indirect/ragged accesses, observable
global state, incomplete DU/alias facts, additional effects and other arithmetic
remain explicit refusals. A singleton loop eliminated before this observation
point cannot be reconstructed from a dimension-name assumption. Additional
canonical forms require separate reviewed extensions, not guessed axes.

`LNO_DSL_discover_partition_axes` returns the proved count and can invoke a
phase-local read-only visitor with borrowed facts, including source operations,
I/K extents and access axes. Only `status=proved` establishes the domains.
Consumers may not mutate through the visitor or retain facts after source,
DU, alias or access-map mutation; there is no persisted planner handoff yet.

## Controls And Trace

- Implemented control: `-DSL:partition_axis=on`; default OFF.
- Minimum level: `-O3`, with LNO enabled and its optimization level positive.
  No discovery at `-O0`, `-O1` or `-O2`, even if LNO is explicitly requested.
- Compiler drivers use the standard backend forwarding convention, for example
  `-Wb,-DSL:partition_axis=on`. The acceptance runner records exact direct-backend
  command vectors against the newly built libraries and an installed C frontend.
- Existing `-DSL:ai_opt_trace` and TP_WOPT2 `AI_OPT_TRACE_FLAG=0x00800000` control
  reporting. This phase does not allocate a second trace bit. Default AI tracing
  remains on under the existing policy; trace OFF changes no analysis decision.
- `AI_PARTITION 1` positional HEADER/FACT/SUMMARY rows report active PU, region,
  status, both induction ST/offset identities, input instance/contribution axes,
  R/N, recipe and actual self-operand position/order, with source line last.
  Refusals use `-` for unproved axes/cardinalities, not misleading zero values.

`osprey/be/lno/tests/partition_axis_trace_report.py` verifies the schema,
invariants and per-PU totals and produces `semantic.log`, `optimization.log` and
`optimization-summary.log`. Each begins with `# What it summarizes:`. Reports
separate recognition from candidate selection, profitability, transformation
and native execution. They are compile-time debugging tools, not a performance
dashboard implementation.

## Acceptance And Remaining Work

Compiler-side acceptance: thirteen native cases, five proved domains and eight
explicit refusals; seven option/trace/level modes; four trace-auditor tests;
existing default v1/v2 normalization regression lanes pass. No frontend/runtime
dependency appears in the rebuilt LNO/backend shared-library undefined symbols.

The focused review runner compiles thirteen ordinary C functions using the
installed frontend, consumes their binary WHIRL with actual PREOPT/LNO, and
independently reopens source and resulting images with `ir_b2a -st -src`.

Expected positive cases: row sum, transposed/column sum, squared sum, equal
extents with misleading variable names, and an explicit contributor temporary.
Expected refusals: dynamic count, step two, broadcast output, union overlap,
different-input product, observable global accumulator, partial coverage and
conditional contributors. Parser tests reject malformed/invented proofs and
inconsistent summaries.

Controls cover default-OFF, explicit disabled, trace-OFF and O0/O1/O2. Enabled,
disabled, default and trace-OFF O3 runs must produce identical `.N` mapped images
and empty independent ASCII diffs. This proves this query is read-only; it does
not claim that traditional LNO itself performs no transformations.

Retain source `.B`, all O3 `.N` images, independently reopened `.T`, raw phase
`.t`, empty analysis comparison diff, reports, commands, product hashes and a
receipt under the established artifact conventions. No CUDA emission, speedup
or native GPU claim belongs to this slice.

Remaining R1-ND1 work includes additional canonical forms/domain combinations
and broader fact-query coverage. R1-ND2/ND3 must connect mean/centered-variance/
inverse-scale/affine def-use and broadcast relations to classify LayerNorm or
RMSNorm from native programs. R1-ND4 must then bind the exact source arithmetic
and carry proved domains into ownership/storage/movement candidates, complete
cost comparison and selected lowering. This slice does not close those gates.
