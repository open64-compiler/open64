# AI Compiler Optimization Phase Ownership Audit

## Purpose

This audit applies the project ownership rule to AIO-0 through AIO-13:

- `common/com` defines IR records and stable IDs, constructs or interns those
  records, checks structural integrity, and provides generic access and
  printing;
- VHO captures PU-local WHIRL facts, discovers candidates, checks semantic and
  target legality, constructs costs, selects plans, and applies VHO
  transformations;
- WOPT owns decisions requiring CFG, SSA, CODEREP, value numbering, PRE, or
  other optimizer state;
- LNO owns canonical-loop analysis and transformation;
- IPA owns analysis or transformation whose compilation scope crosses a PU.

The location of an IR schema does not determine where an optimization decision
is made. A common constructor receives already-decided record content from the
owning phase.

## Classification Rules

The following are valid common responsibilities:

- fixed record, enum, and stable-ID definitions;
- append, intern, bulk-create, copy, and lookup APIs;
- structural range, identity, reserved-field, and graph-integrity checks;
- generic logical printing and mapped-image support when persistence is
  reviewed.

The following must not remain in common:

- traversing an active PU to discover semantic facts;
- matching optimization patterns or forming candidates from WHIRL;
- deciding semantic, target, resource, or profitability legality;
- assigning estimated optimization costs from target or workload facts;
- selecting a candidate or plan;
- evaluating runtime policy or applying a transformation.

## Phase Audit

| Milestone | Current service | Finding | Required owner/action |
| --- | --- | --- | --- |
| AIO-0 | Documentation and baseline artifacts | Conforms | No optimization implementation exists. Keep inventory and evidence in documentation/tests. |
| AIO-1 | `dsl_tensor_evolution` | Needs split | Common may retain graph records, add/intern APIs, structural verification, access, and printing. `DSL_tensor_evolution_build_semantic_roots()` scans the active DSL image and must move to VHO fact capture. The later add-layout/distributed/local/tile/staged-buffer calls must receive phase-decided content rather than decide it. |
| AIO-2 | `dsl_opt_plan` | Needs split | Common may retain candidate/cost/plan records and construction. `DSL_opt_plan_select()` is an optimization decision and must move to a VHO/OPT selection service. Selection verification may structurally check a recorded result but must not choose it. |
| AIO-3 | `dsl_tensor_analysis` | Needs split | Records, construction, structural verification, access, and printing remain common. `DSL_tensor_analysis_build()` scans PU values, nodes, operands, and descriptors and must move to VHO. |
| AIO-4 | `dsl_tensor_locality` | Needs split | Snapshot/locality record storage remains common. PU control-position capture, lifetime/reuse-distance/access analysis, byte estimates, and locality classification move to VHO unless a later part explicitly requires WOPT CFG/SSA state. |
| AIO-5 | `dsl_fusion_candidate` | Needs split | Fusion candidate/member/boundary IR remains common. Pattern matching, boundary discovery, semantic legality, cost construction, and plan selection move to VHO. WOPT may later own CFG/SSA-enabled fusion support through a separate phase adapter. |
| AIO-6 | `dsl_layout_candidate` | Needs split | Layout descriptor/site/alternative IR remains common. Alternative discovery, conversion legality, cost construction, and selection move to VHO. |
| AIO-7 | `dsl_distributed_candidate` | Needs split | Placement, sharding, alias-range, communication-epoch, and intent records remain common. PU-local derivation, legality, communication cost, and selection move to VHO; cross-PU inference is deferred to explicit IPA. |
| AIO-8 | `dsl_residency_candidate` and `dsl_memory_hierarchy` | Partial | Static target memory-hierarchy descriptors and common residency records conform. Residency candidate formation, capacity/resource legality, cost construction, and selection in `dsl_residency_candidate` move to VHO. |
| AIO-9 | `dsl_tile_candidate` | Needs split | Tile site/plan/stage records remain common. Matmul shape capture, target/resource legality, tile-family generation, cost construction, and selection move to VHO initially; canonical-loop realization belongs to LNO when introduced. |
| AIO-10 | `dsl_fetch_pipeline` | Needs split | Fetch/pipeline records remain common. Movement-plan generation, overlap estimates, barrier/resource legality, cost construction, and selection move to VHO. |
| AIO-11 | `dsl_physical_plan` | Needs split | Provider capability and physical-plan records remain common. Provider candidate discovery, capability matching, cost construction, and implementation selection move to VHO. Existing executable lowering in `be/vho/dsl_lower.cxx` is correctly phase-owned. |
| AIO-12 | `dsl_runtime_variant` plus `dsl_runtime_variant_opt` | Conforms after ownership correction | Common owns RuntimeVariantIR creation and structural services. VHO owns fact capture, capability checks, candidate/cost construction, selection, semantic verification, and guard evaluation. |
| AIO-13 | Not implemented | Boundary specified | Telemetry records and generic construction may be common. Feedback collection, profile interpretation, cost-model updates, and re-selection belong to the consuming phase; cross-PU aggregation requires explicit IPA/runtime design. |

## Test Ownership

Directory placement of a test is not itself an architectural contract, but the
test should follow the implementation owner after migration:

- pure record construction, structural verifier, access, printer, and
  mapped-image tests stay under `common/com/tests`;
- fact-harvesting, legality, costing, selection, and VHO transformation tests
  move under `be/vho/tests`;
- CFG/SSA/CODEREP tests belong under `be/opt/tests`;
- canonical-loop tests belong with LNO;
- cross-PU tests are added only with explicit IPA support.

Existing combined producer fixtures may remain temporarily as compatibility
tests while each split lands, but they must link the phase implementation
explicitly and must not be used as evidence that decision logic belongs in
common.

## Migration Order

### Ownership M1: Selection Core

Move `DSL_opt_plan_select()` policy to a phase-owned selection API while
preserving candidate, cost, plan, and recorded-selection IR. Update callers
without changing selected plans or retained artifacts.

### Ownership M2: Fact Capture

Split AIO-1, AIO-3, and AIO-4. Common retains TensorEvolutionGraph,
TensorAnalysisIR, TensorControlSnapshotIR, and TensorLocalityIR construction;
VHO creates their contents from the active PU.

### Ownership M3: Semantic Alternatives

Split AIO-5 through AIO-8 candidate discovery, legality, cost, and selection
into VHO. Preserve every common record and deterministic trace.

### Ownership M4: Target Planning

Split AIO-9 through AIO-11 tile, fetch/pipeline, and physical implementation
decisions into VHO. Preserve static target-description records in common and
keep executable lowering in VHO/LNO as appropriate.

### Ownership M5: Certification

For every migrated milestone:

1. compare before/after selected records and optimization traces;
2. prove `.B` and `ir_b2a -st -src` compatibility where the phase is
   analysis-only;
3. preserve transformation before/after traces where the phase applies WHIRL;
4. rebuild `be.so`, `be`, and `lw_inline` and repeat the dependency audit;
5. move or split tests according to implementation ownership;
6. reject common APIs that accept phase controls such as `select_plans` or
   `apply_transformation` after their migration completes.

## Compatibility Constraint

This is a source-ownership refactor. It must not change logical DSL operator
contracts, public record values, selected-plan behavior, binary WHIRL, mapped
image layout, runtime ABI, or retained milestone semantics. Any later record
or binary change requires its own compatibility review.
