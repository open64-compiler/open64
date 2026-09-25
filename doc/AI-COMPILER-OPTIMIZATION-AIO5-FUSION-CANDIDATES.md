# AIO-5 High-Level Fusion Candidate Contract

## Status

Completed on 2026-09-24. AIO-5 is a runtime-only, check-only, per-PU
implementation of AI-P2. It recognizes reviewed high-level tensor patterns,
records their legality and boundaries, and constructs complete AIO-2 plan
alternatives without rewriting executable WHIRL.

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
2. deterministic members, boundaries, eliminated bytes, and live-range growth;
3. candidate-only and selected-plan controls;
4. semantic-bias mismatch, descriptor mismatch, effect barrier, resource
   rejection, and resource-unknown cases;
5. explicit rejection of transformation application;
6. complete selected fusion costs and baseline fallbacks;
7. repeated-build determinism and no WHIRL/table mutation;
8. byte-identical binary and ASCII WHIRL before and after analysis;
9. active-PU ownership and wrong-PU rejection;
10. Open64 target syntax and logical opcode-layout checks; and
11. `be.so`, `be`, and `lw_inline` dependency closure.

## Next Boundary

AIO-6 introduces logical layout alternatives. It may refine a fusion site with
layout constraints, but it must create child alternatives rather than mutate
the released AIO-5 candidate meaning. Executable fusion remains deferred.
