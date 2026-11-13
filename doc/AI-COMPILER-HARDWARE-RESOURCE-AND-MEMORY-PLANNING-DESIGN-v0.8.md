# Hardware resource and memory planning v0.8

This additive revision preserves [v0.7](AI-COMPILER-HARDWARE-RESOURCE-AND-MEMORY-PLANNING-DESIGN-v0.7.md) and incorporates [H4 GEMM hierarchy v1.0](AI-COMPILER-H4-GEMM-HIERARCHY-v1.0.md). H20 96/141 identities remain distinct complete resource/execution/budget dependencies; `sm_90` compatibility alone grants no board bandwidth or residency claim.

H4 compares an original staged consumer with an additional TN-scalar B retention envelope using the same H3 count adapter. Shared reads and instruction issue work can fall while register allocation rounding reduces residency. The model pays these effects explicitly; it never treats fewer shared reads as fewer source/global bytes or declares real spills zero. Separate H3 supplements remain pinned external inputs, and actual ptxas allocation, cache behavior, performance and hardware race evidence remain H0/H7 work.

Owning PU source admission precedes estimation; fixed-winner source/model/profile/resource/geometry/layout/score revalidation precedes native construction. Missing proof or incomplete comparison retains the historical planner/fallback. Finite request budgets, numerical policy, WHIRL opcode/tensor descriptors, COMMENT explanations and existing driver lifetime/publication remain controlling contracts.


## H5 MLA executable cross-head cache reuse

[H5 specification](AI-COMPILER-H5-MLA-HIERARCHY-v1.0.md) and [plan v1.6](AI-COMPILER-H20-MEMORY-HIERARCHY-IMPLEMENTATION-PLAN-v1.6.md) add a versioned O3/H2/H3/H20-gated executable panel16 family. G2/G4 warps share latent/rotary cache panels within a CTA, with explicit publication/all-consumer release, disjoint local scores/weights and unchanged normalized split tuples/stable merge. Original G1 remains eligible under the same finite physical-unit comparison. Only one selected plan is revalidated and materialized; estimation allocates no WN/ST/TY. H20 96/141 bindings, COMMENT explanations, analysis invalidation/rollback and fallback remain. This does not enable archived MR2 hypotheses, quantized serving adaptation or AOT selection. H0/H7 GPU correctness/races/ISA/resources/performance remains open; software checks establish no measured speedup. Earlier versions are preserved.
