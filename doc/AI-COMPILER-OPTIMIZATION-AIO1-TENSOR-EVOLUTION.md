# AIO-1 TensorEvolutionGraph Contract

## Status

Completed on 2026-09-24. This document defines the first check-only
TensorEvolutionGraph milestone. The service is runtime-only, per-PU, and
additive. It does not change executable WHIRL, mapped-image sections, ELF
section numbering, tensor-type identity, or the DSL frontend API.

## Purpose

TensorDescriptorIR describes one canonical tensor value representation.
Optimization needs to reason about several possible descendants of that value,
including layout, placement, shard, tile, staged-buffer, register-fragment,
and instruction-fragment forms. TensorEvolutionGraph preserves their common
semantic identity without mutating the original tensor type or prematurely
selecting one alternative.

AIO-1 establishes only semantic roots. Later milestones may append evolution
nodes and edges after candidate and plan ownership is defined.

## Ownership And Lifetime

1. One graph belongs to one active `PU_Info` and its global function `ST_IDX`.
2. Creation, construction, verification, and queries that inspect PU-local
   values require that PU's local symbol table to be active.
3. The graph owns no WN, ST, TY, DSL-image, or source strings. It stores stable
   IDs and canonical `TY_IDX` references only.
4. The VHO per-PU driver will eventually own graph lifetime. AIO-1 tests call
   the service directly before that orchestration hook is introduced.
5. Cross-PU reasoning remains an explicit future IPA summary problem.

## Node Contract

Each immutable node records:

- graph-local contiguous node ID;
- representation kind;
- owner-PU function `ST_IDX`;
- original semantic `DSL_IR_VALUE_ID`;
- immutable canonical descriptor `TY_IDX`;
- semantic-root node ID; and
- flags and reserved fields.

The representation kinds are semantic, logical layout, distributed, local
physical, tile, staged buffer, register fragment, and instruction fragment.
Only semantic nodes are legal in AIO-1.

A semantic root must reference a live, non-redirected DSL tensor value owned
by the active PU. Its descriptor must be a sealed canonical tensor type. Its
semantic-root ID is its own node ID. Every live tensor value has at most one
root.

## Edge Contract

The API reserves graph-local edge IDs and transformation names for logical
layout, sharding, placement, local layout, tiling, staged buffers, register
fragments, and instruction fragments. AIO-1 publishes enumeration only and
requires the edge set to remain empty. This keeps the root schema reviewable
until AIO-2 defines candidate and complete-plan ownership for alternatives.

## Determinism

Semantic roots are discovered by ascending existing `DSL_IR_VALUE_ID`.
Repeated construction is idempotent and preserves node IDs. The service does
not sort by names, pointer values, table addresses, or frontend creation
objects.

## Validation

Verification requires:

1. the graph owner is the active PU;
2. node IDs are contiguous;
3. all AIO-1 nodes are semantic roots;
4. owner, DSL value, local ST, and canonical TY agree;
5. redirected values are absent;
6. semantic value IDs are unique; and
7. the AIO-1 edge set is empty.

Expected misuse, including null/inactive PU creation and cross-PU graph use,
fails without mutating the graph or WHIRL.

## Inspection

`DSL_Tensor_Evolution_Print()` emits deterministic phase evidence containing
the owner, node and edge counts, node kind, value ID/name, canonical TY index,
dtype, logical shape, semantic root, and flags. The graph is intentionally not
printed by `ir_b2a`: AIO-1 is runtime-only and must not leak into binary WHIRL.

## Certification

The focused test builds a fixed-shape `common.matmul`:

```text
kid0   tensor<float32, [2,3]>
kid1   tensor<float32, [3,4]>
result tensor<float32, [2,4]>
```

It proves three deterministic roots, idempotent construction, exact type/value
identity, per-PU ownership with colliding local indices, and rejection under
the wrong active PU. Independent before/after processes emit binary WHIRL with
and without graph construction; the `.B` files and `ir_b2a -st -src` traces
must be byte-for-byte identical.

The completed validation produced:

- three roots and zero edges for `kid0`, `kid1`, and `matmul_result`;
- invalid-ID, out-of-range, inactive-PU, and graph-reset coverage;
- identical SHA-256 hashes for the before/after `.B` pair;
- identical SHA-256 hashes for the before/after `.T` pair;
- a passing full DSL native syntax and six-target operator-layout matrix;
- successful `be.so` and `lw_inline` rebuilds; and
- no `DSL_Builder_*` or `Json::` symbol leakage into either backend binary.

The retained local evidence family is
`artifacts/ai_optimization/aio1_tensor_evolution/`. It includes the two binary
WHIRL images, both `ir_b2a -st -src` traces, the runtime graph trace, commands,
backend dependency/symbol evidence, diagnostics, and SHA-256 checksums.
