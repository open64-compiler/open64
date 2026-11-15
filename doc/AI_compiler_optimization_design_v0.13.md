# AI compiler optimization design v0.13

This additive revision preserves [v0.12](AI_compiler_optimization_design_v0.12.md) and records [H4 GEMM hierarchy v1.0](AI-COMPILER-H4-GEMM-HIERARCHY-v1.0.md) with [plan v1.5](AI-COMPILER-H20-MEMORY-HIERARCHY-IMPLEMENTATION-PLAN-v1.5.md). H4 consumes existing common GEMM semantic/locality admission, H2 owned source/reuse facts, H3 physical estimates and immutable H20 device profiles. It does not broaden unsupported High-loop admission or relocate optimization into an emitter.

The bounded catalog adds staged B scalar retention across per-thread output rows. Source useful/global bytes are unchanged; fewer shared consumer reads and extra RF reservation are modeled together. Canonical ordered F32 folds, persistent accumulators, shared/TMA readiness, all-consumer release and safe slot reuse remain authoritative. Only a fixed revalidated winner is materialized in the existing driver-owned device PU. Candidate exploration creates no WN/ST/TY. Explicit COMMENT explanations remain part of WHIRL and generated source.

Very High domain operators, High common algorithms and DO_LOOP/OPR_ARRAY/dependence/scheduling, Mid structured memory, Low GPU mapping and backend PTX keep their established boundaries. Runtime-only planning flags are not new WHIRL opcodes, tensor traits or mapped-image layouts. Hardware qualification, later materializers and AOT serving selection retain their explicit gates.


## H5 MLA executable cross-head cache reuse

[H5 specification](AI-COMPILER-H5-MLA-HIERARCHY-v1.0.md) and [plan v1.6](AI-COMPILER-H20-MEMORY-HIERARCHY-IMPLEMENTATION-PLAN-v1.6.md) add a versioned O3/H2/H3/H20-gated executable panel16 family. G2/G4 warps share latent/rotary cache panels within a CTA, with explicit publication/all-consumer release, disjoint local scores/weights and unchanged normalized split tuples/stable merge. Original G1 remains eligible under the same finite physical-unit comparison. Only one selected plan is revalidated and materialized; estimation allocates no WN/ST/TY. H20 96/141 bindings, COMMENT explanations, analysis invalidation/rollback and fallback remain. This does not enable archived MR2 hypotheses, quantized serving adaptation or AOT selection. H0/H7 GPU correctness/races/ISA/resources/performance remains open; software checks establish no measured speedup. Earlier versions are preserved.
