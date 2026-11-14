# AI compiler optimization design v0.11

2026-11-12. This additive revision preserves [v0.10](AI_compiler_optimization_design_v0.10.md)
and incorporates [H2 owned facts](AI-COMPILER-H2-OWNED-MOVEMENT-FACTS-v1.0.md)
and [H3 physical costing](AI-COMPILER-H3-MOVEMENT-COSTING-v1.0.md).
The current execution roadmap is [H20 plan v1.4](AI-COMPILER-H20-MEMORY-HIERARCHY-IMPLEMENTATION-PLAN-v1.4.md).

Very High WHIRL retains domain operators. High WHIRL retains the common
algorithm, DO_LOOP/OPR_ARRAY tensor indexing, LNO dependence analysis and
scheduling, multi-level tiling and explicit staging/prefetch generation.
Structured memory lowering follows at Mid WHIRL, GPU mapping at Low WHIRL,
and PTX at the backend. H3 changes profitability within existing admitted
routes; it does not move a lowering boundary or add a canonical algorithm.
WHIRL opcodes and existing tensor descriptors/traits retain their established
semantic roles. Derived work/reuse/resource/lifetime/model facts are owned
planning records with explicit validity, not a new operation trait system.

Search remains formally two-phase: bounded detached estimation and convergence,
then native transformation/reference qualification of the selected executable
plan. H3 gives the existing strategy portfolio a physical ns comparison with
H20 profile bindings and request-wide finite budgets. Reference-cycle scenarios
remain separately readable. Missing physical inputs, unsupported materializers
or expired/stale facts preserve an admitted fallback. COMMENT explanations and
selected revalidation retain the source of each decision.

Sufficiently calibrated compile-time information can estimate service,
dependencies and finite pipeline effects; measurement is not required inside
search. Model predictions remain hypotheses until separate hardware correctness,
race, timing and profiling qualification. No global GPU optimum, universal
kernel winner or measured speedup follows from analytical search. General
unsupported family routes, later materializers, real H20 qualification and
AOT serving selection retain their explicit outstanding gates.
