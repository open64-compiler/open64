# AI compiler optimization design v0.15

This additive revision preserves [v0.14](AI_compiler_optimization_design_v0.14.md)
and adopts [H7 qualification](AI-COMPILER-H7-H20-QUALIFICATION-v1.0.md) and
[plan v1.8](AI-COMPILER-H20-MEMORY-HIERARCHY-IMPLEMENTATION-PLAN-v1.8.md).
H7 consumes only frozen selected products and owned source/profile/model/launch
bindings. It does not create alternatives, mutate WHIRL, tune, JIT, update models
or execute candidates during estimation. WHIRL opcodes/tensor traits and existing
Very High/High/Mid/Low/PTX boundaries remain authoritative; no MLIR assumption.

Four H20 result profiles separate F32 GEMM, full absorbed MLA, ordered LayerNorm
and Reduction. Actual device/variant/resource eligibility and independent
correctness/sanitizer evidence precede unprofiled wall/event timing. Optional
counter/timeline processes remain separate from benchmark and search. Existing
combined package, shared receiver and GS1/GS2 owners retain orchestration,
physical GPU leases, cancellation, partial collection and typed return review.
Old inner scripts/contracts remain byte-identical. COMMENTs remain in copied
native and generated device source.

Observed H20 name/UUID/capacity/full-device/resource facts are distinct from
synthetic H3 bindings and SM90 code compatibility. Missing baselines/calibration
stay explicit; no ratio or speedup acceptance is invented. Real H20 execution,
additional TMA launch-provider qualification and model acceptance remain gates.
AOT serving selection, PREOPT COMMENT repair and OpenCTI remain deferred; transfer
roles remain TBD. Earlier material and review receipts remain immutable.
