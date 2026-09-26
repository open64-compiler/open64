# AIO-12 Runtime Variant And Guard Contract

## Purpose

AIO-12 implements the first AI-P10 runtime-variant slice from
`AI_compiler_optimization_design_v0.1.md`. It converts one selected AIO-11
physical implementation into a certified guarded fast variant plus an
unconditional conservative fallback. The first slice is PU-local,
runtime-only, and analysis-only. It does not rewrite WN, add a mapped-image
section, change an opcode or type encoding, or add a runtime ABI.

The initial vertical slice uses the fixed-shape rank-2 F4
`common.matmul.v1` fixture from AIO-11:

- fast variant: the reviewed NVIDIA cuBLASLt physical implementation;
- guards: `kid0` and `kid1` runtime buffer alignment are each at least 16
  bytes;
- fallback variant: the reviewed Open64 direct implementation;
- policy: evaluate the selected fast variant and follow its explicit fallback
  on the first failed required guard.

This is the generic skeleton for later symbolic-shape, alias, ownership,
residency, communication-epoch, and serving-state guards.

## Why Runtime Alignment Is Not Tensor Type Alignment

The fixture's canonical tensor `TY_IDX` has F4 element alignment. That is a
static type property and remains immutable and uniqued. It does not prove that
a particular runtime tensor buffer begins at a 16-byte address.

The first AIO-12 guard therefore consumes a runtime observation about an
operand buffer. It does not strengthen the canonical tensor type, alter
TensorDescriptorIR equivalence, or authorize the fast implementation without
checking the actual runtime value. A missing or insufficient observation takes
the conservative fallback.

This separation avoids two unsound shortcuts:

1. treating a favorable allocation seen in one run as a permanent type fact;
2. allowing a provider alignment assumption to leak into the direct fallback.

## Open64 Design Continuity

The design follows Open64 guarded loop multiversioning:

1. construct independently legal fast and conservative alternatives;
2. represent the uncertain property as an explicit bounded-cost predicate;
3. execute the aggressive alternative only when the predicate proves its
   assumptions for the current runtime values;
4. retain an unconditional fallback whose legality does not depend on the
   predicate.

AIO-12 reuses AIO-2 `OptimizationPlanIR` for candidate, legality, cost,
fallback, and deterministic static selection. The guard overhead is recorded
in the existing `DSL_OPT_COST_RUNTIME_SELECTION` term. It reuses AIO-11
physical implementation identities rather than cloning provider descriptions.

## Compilation Scope And Ownership

`DSL_RUNTIME_VARIANT_ANALYSIS` is a VHO-owned analysis context for the active
PU and requires the owning PU's local symbol table to be active. It consumes:

- the active PU;
- its TensorEvolutionGraph;
- its verified AIO-11 physical-plan analysis;
- an explicit AIO-12 control record.

The service does not inspect another PU. Cross-PU profitability, specialization,
or guard summaries require explicit IPA design and are not inferred here.

The IR definition and construction service lives in:

```text
osprey/common/com/dsl_runtime_variant.h
osprey/common/com/dsl_runtime_variant.cxx
```

The optimization logic lives in:

```text
osprey/be/vho/dsl_runtime_variant_opt.h
osprey/be/vho/dsl_runtime_variant_opt.cxx
```

Common/com owns the stable records, bulk constructor, structural verifier,
generic accessors, and generic printer. VHO owns physical-plan fact capture,
target capability checks, candidate and cost construction, static selection,
semantic verification, and guard evaluation. Public common records contain
stable IDs and fixed scalar fields. VHO's working vectors and plan contexts
remain runtime analysis storage and never become WHIRL table records.

## RuntimeVariantIR

One `DSL_RUNTIME_VARIANT_SITE_RECORD` identifies a logical DSL node/value and
the AIO-11 physical site it refines. Its variants are contiguous and include:

- exactly one baseline variant;
- zero or more certified guarded variants;
- one statically selected variant policy entry;
- explicit fallback IDs.

`DSL_RUNTIME_VARIANT_RECORD` retains:

- physical implementation identity;
- contiguous guard range;
- fallback variant;
- AIO-2 candidate and plan identities;
- physical, guard, and total cost;
- deterministic variant identity;
- baseline, guarded, certified, and selected state.

For this slice the baseline is inserted first and has no guard or fallback.
The cuBLASLt variant has two required guards and falls back directly to the
baseline. A guard chain cannot supply legality to either implementation; both
must already be independently proven legal by AIO-11.

## RuntimeGuardIR

`DSL_RUNTIME_GUARD_RECORD` separates the predicate from runtime observations.
It identifies:

- guard kind and comparison;
- operand and optional dimension ordinal;
- required value;
- evaluation cost;
- failure action;
- owning variant.

The enum reserves reviewed forms for operand alignment, shape equality, and
shape divisibility. The first implementation accepts only
`operand_alignment at_least`. Publishing enum names does not make the deferred
shape guards executable; their producer and validation rules must be reviewed
before use.

`DSL_RUNTIME_GUARD_OBSERVATION` is borrowed for one evaluation. A missing
alignment observation is a false predicate and selects fallback. Duplicate or
malformed observations fail closed instead of choosing a variant.

## Build And Evaluation Flow

VHO preparation:

1. Verify active-PU ownership, TensorEvolutionGraph, and AIO-11 analysis.
2. Require a selected, available, semantics-preserving cuBLASLt
   `common.matmul.v1` implementation with a proven direct fallback.
3. Verify both operand references and tensor descriptors.

VHO plan construction followed by common IR creation:

1. Add the direct baseline variant and AIO-2 baseline plan.
2. Add the cuBLASLt variant with `kid0` and `kid1` alignment guards.
3. Add the complete guard cost to `runtime_selection`.
4. Select among complete proven plans through AIO-2.
5. Pass the completed records to the policy-free common RuntimeVariantIR
   constructor and run both structural and VHO semantic verification.

VHO guard evaluation:

1. Start from the statically selected variant.
2. Evaluate required guards in ordinal order and accumulate actual predicate
   cost.
3. Return the selected physical implementation when all guards pass.
4. Follow the explicit fallback on the first false guard.
5. Reject a missing fallback, malformed observation, duplicate observation,
   or cycle.

Evaluation does not invoke a provider and does not mutate WHIRL. Executable
conditional dispatch belongs to a later reviewed application milestone.

## Verification Invariants

The verifier requires:

1. Active PU, owner ST, graph, and physical analysis agree.
2. Site/node/value/physical-site identities agree exactly.
3. The baseline is direct, proven, guard-free, and unconditional.
4. The fast implementation is the selected reviewed cuBLASLt matmul plan.
5. Each guard references an existing operand in ordinal order.
6. Required alignment is a power of two and matches the control contract.
7. Guard costs sum exactly into the variant and AIO-2
   `runtime_selection` cost.
8. Fast fallback points to the site's baseline variant.
9. Exactly one variant carries selected state when policy selection is enabled.
10. Variant identities, table ranges, AIO-2 plans, and costs reproduce
    deterministically.

## Optimization-Level Contract

- `-O0`: no runtime variants; straightforward direct lowering remains the
  baseline.
- `-O1` and `-O2`: outside this first target-plan slice.
- `-O3`: PU-local target planning may build certified guarded variants.
- `-ipa`: future explicit cross-PU summaries may inform variant construction;
  AIO-12 does not infer them automatically.

## Compatibility

This slice is runtime-only analysis:

- no WN, opcode, `TY_KIND`, symbol, ELF section, mapped-image row, frontend
  API, runtime ABI, or WHIRL version changes;
- source `.B` remains the binary frontend boundary;
- existing and previous readers see the same legal WHIRL;
- `ir_b2a -st -src` output remains byte-identical before and after analysis.

Persisted variants, if later justified, require a separate versioned image,
reader/writer/printer, old-reader behavior, and migration review.

## Certification

The retained G15 lane proves:

- one site, two variants, and two required alignment guards;
- fast cuBLASLt selection for alignments 16 and 32;
- direct fallback when `kid0` alignment is 8;
- guard costs 2+2 appear as `runtime_selection=4` in AIO-2;
- duplicate runtime observations fail closed;
- disabled generation creates no runtime records;
- an application request is rejected because this slice is analysis-only;
- repeated runtime analysis is byte-identical;
- before/G15/repeat `.B` and `ir_b2a -st -src` traces are byte-identical.

Review artifacts are retained under the configured host artifact directory,
conventionally `artifacts/ai_optimization/aio12_runtime_variant/`, and are
cleaned at the beginning of the next run.

## Deferred Work

- executable guarded dispatch and atomic WHIRL application;
- symbolic shape equality/divisibility observations;
- alias, ownership, residency, HBM, communication, and workload-state guards;
- multi-variant chains, policy hysteresis, and profile-driven hit rates;
- telemetry binding and realized-cost feedback in AIO-13;
- persisted runtime variants;
- IPA summaries and cross-PU specialization.
