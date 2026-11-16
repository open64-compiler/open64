# AI compiler optimization design v0.14

This additive revision preserves [v0.13](AI_compiler_optimization_design_v0.13.md)
and records [H6 LayerNorm/Reduction](AI-COMPILER-H6-NORM-REDUCTION-v1.0.md)
with [plan v1.7](AI-COMPILER-H20-MEMORY-HIERARCHY-IMPLEMENTATION-PLAN-v1.7.md).
H2 owns source/storage/access/version/lifetime evidence; H3 costs detached
finite candidates under immutable H20 96/141 resource and execution profiles.
Only the freshly revalidated selected winner reaches native construction.

LayerNorm reuses centered ordered LN4 with C0/C1/C2 retention at narrower CTA
sizes. Reduction adds cooperative contiguous shared panels and explicit
publish/readers-release around an ascending F32 fold. Neither changes the
source arithmetic into a parallel tree. Source useful bytes, algorithmic
requests and modeled transactions remain separate. Optional explicit H3
transaction envelopes preserve legacy default keys/costs and pay real carrier,
shared, tail, occupancy and synchronization work without free spill credit.

Production LayerNorm requests opt in with `-DSL:h6_memory_hierarchy=on` and
publish the selected launch separately from the input request width. Reduction
native APIs require an explicit host/caller launch contract; its old row-thread
runtime guard is provenance, not authority for a cooperative launch. General
canonical discovery and new serving adapters remain gated. COMMENT explanations
are generated in native WHIRL and printed by whirl2c.

Very High domain operators, High common algorithms/DO_LOOP/OPR_ARRAY and
owned dependence/scheduling, Mid structured memory, Low GPU mapping and PTX
retain their boundaries. WHIRL opcodes and tensor traits remain the architecture;
no MLIR interface/trait system or binary image/runtime ABI change is introduced.
Hardware correctness/races/resources/performance remain H0/H7 obligations.
AOT selection and OpenCTI remain deferred; personnel transfer roles remain TBD.
