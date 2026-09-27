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
| AIO-1 | `dsl_tensor_evolution` plus `dsl_tensor_evolution_opt` | Conforms after Ownership M4 | Common owns graph records, explicit add APIs, verification, access, and printing. VHO scans the active DSL image and decides which live tensor values become semantic roots. Later representation add calls continue to receive phase-decided content. |
| AIO-2 | `dsl_opt_plan` | Needs split | Common may retain candidate/cost/plan records and construction. `DSL_opt_plan_select()` is an optimization decision and must move to a VHO/OPT selection service. Selection verification may structurally check a recorded result but must not choose it. |
| AIO-3 | `dsl_tensor_analysis` plus `dsl_tensor_analysis_opt` | Conforms after Ownership M4 | Common owns tensor fact/use records, construction, verification, access, and printing. VHO scans PU values, nodes, operands, descriptors, and operator contracts to derive fact contents. |
| AIO-4 | `dsl_tensor_locality` plus `dsl_tensor_locality_opt` and `opt_dsl_locality` | Conforms after Ownership M4 | Common owns control-snapshot and locality records with construction, verification, access, and printing. WOPT owns CFG-derived control-position capture. VHO owns lifetime, reuse-distance, access, byte-cost, critical-path, and locality derivation from the copied snapshot and tensor facts. |
| AIO-5 | `dsl_fusion_candidate` plus `dsl_fusion_candidate_opt` | Conforms after Ownership M3 | Common owns policy-free fusion candidate/member/boundary records, bulk construction, structural verification, access, and generic printing. VHO owns pattern matching, boundary discovery, semantic legality, cost construction, selection, and semantic verification. WOPT may later own CFG/SSA-enabled fusion support through a separate phase adapter. |
| AIO-6 | `dsl_layout_candidate` plus `dsl_layout_candidate_opt` | Conforms after Ownership M3 | Common owns policy-free layout descriptor/site/alternative records and structural services. VHO owns alternative discovery, conversion legality, cost construction, selection, graph overlays, and semantic verification. |
| AIO-7 | `dsl_distributed_candidate` plus `dsl_distributed_candidate_opt` | Conforms after Ownership M3 | Common owns policy-free placement, sharding, alias-range, communication-epoch, and intent records plus structural services. VHO owns PU-local derivation, legality, communication cost, selection, and semantic verification; cross-PU inference remains deferred to explicit IPA. |
| AIO-8 | `dsl_residency_candidate`, `dsl_residency_candidate_opt`, and `dsl_memory_hierarchy` | Conforms after Ownership M3 | Common owns static target memory-hierarchy descriptors and policy-free residency records with structural services. VHO owns candidate formation, capacity/resource legality, cost construction, selection, graph overlays, and semantic verification. |
| AIO-9 | `dsl_tile_candidate` plus `dsl_tile_candidate_opt` | Conforms after Ownership M2 | Common owns tile site/plan/stage records, bulk construction, structural verification, access, stable names, and generic printing. VHO owns matmul shape capture, target/resource legality, tile-family generation, costs, selection, and semantic verification. Canonical-loop realization remains reserved for LNO. |
| AIO-10 | `dsl_fetch_pipeline` plus `dsl_fetch_pipeline_opt` | Conforms after Ownership M2 | Common owns fetch/pipeline records, bulk construction, structural verification, access, stable names, and generic printing. VHO consumes `CommonTilePlanIR` and owns movement-plan generation, overlap estimates, barrier/resource legality, costs, selection, and semantic verification. |
| AIO-11 | `dsl_physical_plan` plus `dsl_physical_plan_opt` | Conforms after Ownership M1 | Common owns provider capability and CommonPhysicalPlanIR records, bulk construction, structural verification, access, and generic printing. VHO owns provider candidate discovery, capability/legality checks, cost construction, implementation selection, semantic verification, and executable lowering. |
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

The migration is consumer-first. Moving the common AIO-2 selector first would
force its existing common callers to depend backward on VHO. Instead, move
those consumers to their owning phase before relocating selection policy.

### Ownership M1: Physical Implementation Planning

Completed. Common retains provider capability and physical-plan IR; VHO owns
provider candidate discovery, capability/legality checks, cost, selection, and
semantic verification. Existing executable lowering remains in VHO. The
linked AIO-11 and AIO-12 lanes prove byte-identical analysis-only WHIRL and
unchanged downstream runtime-variant behavior.

### Ownership M2: Tile And Fetch Planning

Completed. AIO-9 and AIO-10 tile-family and fetch/pipeline decisions reside in
`be/vho/dsl_tile_candidate_opt.{h,cxx}` and
`be/vho/dsl_fetch_pipeline_opt.{h,cxx}`. Policy-free
`CommonTilePlanIR` and `CommonFetchPlanIR/CommonPipelineIR` containers remain
in common with structural services. AIO-10 consumes the neutral tile IR and
AIO-11 consumes both neutral IRs, so downstream phases do not reach through a
VHO analysis implementation. Static target-description records remain common,
and canonical-loop realization remains reserved for LNO. Linked AIO-9 through
AIO-12 lanes prove unchanged selected records and downstream behavior.

### Ownership M3: Semantic Alternatives

Completed. AIO-5 through AIO-8 candidate discovery, legality, cost, selection,
and semantic verification reside in `be/vho/*_candidate_opt.{h,cxx}`. Common
retains neutral `FusionPlanIR`, `LogicalLayoutIR`, `DistributedPlanIR`, and
`ResidencyPlanIR` containers with construction, structural verification,
access, and generic printing. AIO-9 through AIO-12 consume those common IR
handles instead of depending on the producing VHO analyses. Linked AIO-5
through AIO-12 lanes preserve deterministic decisions, analysis-only WHIRL,
and `ir_b2a -st -src` evidence.

### Ownership M4: Fact Capture

Completed. `dsl_tensor_evolution_opt`, `dsl_tensor_analysis_opt`, and
`dsl_tensor_locality_opt` own active-PU semantic-root, tensor fact/use, and
lifetime/locality derivation. Common retains TensorEvolutionGraph,
TensorAnalysisIR, TensorControlSnapshotIR, and TensorLocalityIR construction,
verification, access, and printing. CFG-derived snapshot capture remains in
WOPT's `opt_dsl_locality` adapter. Linked AIO-1 through AIO-12 lanes prove
unchanged analysis decisions and binary WHIRL; AIO-5 through AIO-12 `.B` files
and normalized `ir_b2a -st -src` traces match Ownership M3 evidence.

### Ownership M5: Selection Core And Certification

After no common caller remains, move `DSL_opt_plan_select()` policy to a
phase-owned selection API while preserving candidate, cost, plan, and
recorded-selection IR. Then:

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
