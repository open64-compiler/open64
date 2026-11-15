# H20 memory hierarchy implementation plan v1.5

This additive revision preserves [v1.4](AI-COMPILER-H20-MEMORY-HIERARCHY-IMPLEMENTATION-PLAN-v1.4.md) and its earlier plans. [H4 GEMM v1.0](AI-COMPILER-H4-GEMM-HIERARCHY-v1.0.md) defines the current focused executable delivery. The original plan's H5 GEMM heading corresponds to this workstream's current H4 name; historical Reduction/LayerNorm and MLA gates are unchanged.

| H4 phase | Delivery and gate |
| --- | --- |
| Count/admit | Exact padded staged consumer work and B reuse; H2 proofs, O3 active-PU and H20/H3/native-output admission |
| Explore | Original D/S1/T2 plus bounded S1R/T2R; consistent physical costing, additional RF reservation and imported occupancy; shared finite H3 budgets; no candidate native construction |
| Reconstruct | One selected recipe, full source/model/resource/geometry/layout/score validation; legacy winner on incomplete comparison |
| Materialize | Scalar B loads once per q/thread-column, reused across TM rows; persistent ordered F32 accumulators and existing publication/wait/release/refill/drain preserved |
| Inspect/qualify | Generated COMMENT and WHIRL-derived CUDA; genuine source-interleaved before/after/full diffs; host reference, negatives, fallback and link-boundary evidence |
| Hardware gate | H0/H7 device correctness/races, actual registers/spills, independent timing/profiling and calibrated prediction comparison remain open |

This slice does not grant tensor-core arithmetic permissions, introduce deeper TMA, packing/persistent output scheduling or close unsupported general High-LNO GEMM admission. Those remain separately scoped follow-ups. H5 MLA reuse/materializers, missing LN/Reduction routes and imported quantized H3 adaptation keep their explicit gates. AOT serving selection, OpenCTI integration and transfer role assignments remain deferred/TBD.
