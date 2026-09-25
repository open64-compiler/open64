# AIO-5 High-Level Fusion Candidate Contract

## Status

Completed on 2026-09-24 and generalized in the following focused milestone.
AIO-5 is a runtime-only, check-only, per-PU implementation of AI-P2. It
recognizes reviewed high-level tensor patterns and trait-compatible generic
producer-consumer clusters, records their legality and boundaries, and
constructs complete AIO-2 plan alternatives without rewriting executable
WHIRL.

The architectural rationale, performance mechanisms, input and candidate
contracts, uncertainty boundary, downstream consumers, fallback, and required
review evidence are defined in Section 3.3 of
`AI_compiler_optimization_design_v0.1.md`.

## Scope And Ownership

`DSL_FUSION_CANDIDATE_ANALYSIS` belongs to one active PU and borrows the
verified AIO-1 `TensorEvolutionGraph`, AIO-3 semantic tensor facts, and AIO-4
lifetime/locality facts for that PU. It owns only runtime records and one
independent AIO-2 plan context per fusion site. It owns no `WN *`, `ST *`,
`CODEREP *`, frontend object, or mapped-image row.

One AIO-2 plan context represents one alternative tree rooted at one semantic
result. Giving every fusion site its own context preserves the AIO-2 rule that
one baseline anchors all refinements in that context; unrelated sites are not
forced into one candidate tree.

## Initial Patterns

The first vertical slice recognizes:

```text
common.matmul -> common.add(bias) -> common.relu
common.residual_add -> common.relu
```

The matcher operates on logical DSL operators and managed value references.
It does not decode the physical `OPR_DSL` escape representation. Each site
records ordered operator members, external inputs, the final output, and every
intermediate materialization as an alternative fusion cut.

## Generic Discovery

AIO-5 builds the basic skeleton of the optimization process. It proves how a
per-PU analysis discovers a candidate, records members and boundaries,
consumes semantic and locality evidence, classifies legality, constructs
baseline and alternative plans, assigns cost and fallback evidence, performs
selection, honors independent controls, and remains check-only. The amount of
supporting code reflects that reusable skeleton; it does not mean that the
long-term fusion strategy is to hand-code every operator sequence.

The scalable follow-up implements the first hybrid slice. Static, versioned
operator traits record iteration-space class, operand-indexing class,
producer/consumer eligibility, cluster anchors, exact-descriptor requirements,
single-result structure, and semantic-pattern requirements. They are queried
by logical operator and version, so a later version does not silently inherit
an earlier operator's fusion contract.

Generic discovery walks producer-consumer edges backward from a maximal
eligible sink in deterministic value-ID order. Growth stops at a contraction
anchor, the caller's member budget, an unregistered or semantic-only operator,
multiple eligible producers, effects, or an unsupported result structure.
Legality then reuses the same exact descriptor, single-use, ownership,
same-block, REGION, size, and resource proof used by explicit patterns.

This first slice intentionally rejects or stops before fan-out, diamonds,
multiple results, reductions, views, recomputation, and REGION crossings.
Those cases require additional trait fields and profitability policy; they are
not inferred from a shape-rule name. Explicit patterns remain for semantic
compounds, stronger legality, and provider or library alternatives. Both paths
publish through the same AIO-2 candidate, plan, cost, fallback, and selection
services.

Generic candidates consume AIO-6 compatibility as a separate proven, unknown,
or rejected layout fact. It is advisory in this check-only slice because the
baseline canonical representation remains legal; selecting a noncanonical
layout later must make compatibility a hard legality predicate. Later
placement, communication, tiling, and physical-kernel phases refine the same
provisional candidate rather than replacing it with an unrelated decision.

## Legality

A proven candidate requires all of the following:

1. every member has the registered pure effect model and no managed state
   effect row;
2. the non-matmul add operand is a constant, formal, or symbol tensor rather
   than a model-input or arbitrary computed activation masquerading as bias;
3. every intermediate has exactly one expected next-member consumer;
4. intermediate result descriptors exactly match the final canonical tensor
   descriptor;
5. eliminated intermediates have proven unique ownership and exact same-block
   lifetime/reuse evidence;
6. materialization size and boundary live-range growth are statically known;
7. live-range growth fits the caller-supplied resource limit; and
8. the candidate does not cross a branch, loop, REGION, effect, or unresolved
   alias boundary.

Descriptor, effect, ownership, and resource failures are structured
rejections. Conservative control flow, unknown tensor extent, or an absent
resource limit produces `unknown/incomplete_analysis`; it is never interpreted
as legal or as zero cost.

`OPR_REGION`, source positions, operator attributes, TensorDescriptorIR, and
domain gatekeeper evidence remain attached to the unchanged source nodes.
AIO-5 records stable node/value IDs and does not copy or reinterpret their
provenance.

## Materialization And Live Range Evidence

`FusionCandidateIR` records:

- eliminated intermediate count and bytes;
- external input and final output boundaries;
- alternative cut boundaries at each eliminated materialization;
- conservative live-range growth in statements and bytes;
- exact-descriptor, broadcast-bias, and REGION-preservation constraints;
- descriptor, effect, and resource states; and
- the AIO-2 baseline/fusion candidate, cost, plan, fallback, and selection IDs.

Unknown numeric evidence prints as `<unknown>`, not zero.

## Plan Cost

The initial cost is a deterministic relative score, not a latency prediction.
It uses baseline-policy compute, memory, and launch terms and static-analysis
fusion terms. A proven fusion has a complete six-term AIO-2 cost record and a
baseline fallback. Rejected or incomplete fusion has an unknown memory term,
so it cannot be selected.

Later layout, placement, communication, residency, tiling, and target-resource
phases refine or supersede this score. AIO-5 does not claim that an early
selected fusion remains profitable after those constraints are known.

## Controls

The runtime control separates:

- candidate generation;
- plan selection for a target profile; and
- transformation application.

It also independently enables semantic patterns and generic clusters and caps
the deterministic generic member count. The compatibility entry point keeps
the original semantic-pattern defaults; the layout-aware entry point accepts a
verified AIO-6 analysis without transferring ownership.

Candidate-only mode generates and verifies alternatives without selecting one.
Selection mode may select a complete legal plan. Transformation application is
intentionally unsupported and rejected in AIO-5. A later focused milestone
must introduce mutation, post-rewrite verification, and before/after IR review
evidence under its own control.

## Binary Compatibility

AIO-5 changes no WHIRL node, opcode, type, symbol, mapped-image section, ELF
number, reader/writer rule, frontend API, or lowering behavior. Before, after,
and repeated `.B` files and `ir_b2a -st -src` traces are byte-for-byte
identical. Fusion and plan traces are retained separately for review.

## Validation

The focused contract covers:

1. legal matmul-bias-activation and residual-activation sites;
2. the same ordinary contraction/pointwise chain discovered without a
   sequence-specific matcher;
3. versioned trait lookup, semantic-only exclusion, deterministic member
   budget, and AIO-6 compatibility consumption;
4. deterministic members, boundaries, eliminated bytes, and live-range growth;
5. candidate-only and selected-plan controls;
6. semantic-bias mismatch, descriptor mismatch, effect barrier, resource
   rejection, and resource-unknown cases;
7. explicit rejection of transformation application;
8. complete selected fusion costs and baseline fallbacks;
9. repeated-build determinism and no WHIRL/table mutation;
10. byte-identical binary and ASCII WHIRL before and after analysis;
11. active-PU ownership and wrong-PU rejection;
12. Open64 target syntax and logical opcode-layout checks; and
13. `be.so`, `be`, and `lw_inline` dependency closure.

## Next Boundary

Extend traits only with reviewed semantics for reductions, views, multi-use,
recomputation, and numerical constraints. A selected noncanonical layout must
become a child alternative with hard compatibility, conversion cost, and
fallback evidence. Executable fusion remains deferred.
