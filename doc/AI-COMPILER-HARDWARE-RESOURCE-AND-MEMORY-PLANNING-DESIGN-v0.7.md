# Hardware resource and memory planning v0.7

This additive revision preserves [v0.6](AI-COMPILER-HARDWARE-RESOURCE-AND-MEMORY-PLANNING-DESIGN-v0.6.md) and incorporates [H4 GEMM hierarchy v1.0](AI-COMPILER-H4-GEMM-HIERARCHY-v1.0.md). H20 96/141 identities remain distinct complete resource/execution/budget dependencies; `sm_90` compatibility alone grants no board bandwidth or residency claim.

H4 compares an original staged consumer with an additional TN-scalar B retention envelope using the same H3 count adapter. Shared reads and instruction issue work can fall while register allocation rounding reduces residency. The model pays these effects explicitly; it never treats fewer shared reads as fewer source/global bytes or declares real spills zero. Separate H3 supplements remain pinned external inputs, and actual ptxas allocation, cache behavior, performance and hardware race evidence remain H0/H7 work.

Owning PU source admission precedes estimation; fixed-winner source/model/profile/resource/geometry/layout/score revalidation precedes native construction. Missing proof or incomplete comparison retains the historical planner/fallback. Finite request budgets, numerical policy, WHIRL opcode/tensor descriptors, COMMENT explanations and existing driver lifetime/publication remain controlling contracts.
