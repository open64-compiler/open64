# AIO-6 Logical Layout Alternatives

## 1. Purpose

AIO-6 implements the first check-only AI-P3 logical-layout vertical slice. It
creates immutable layout alternatives for a semantic tensor, connects those
alternatives to `TensorEvolutionGraph`, evaluates compatibility and conversion
cost, and routes them through the shared AIO-2 candidate/plan service.

This milestone does not rewrite executable WHIRL, choose target-local storage,
or allocate a persistent tensor type for an unselected alternative.

## 2. Representation Contract

`CommonLayoutIR` is represented by runtime-only fixed records owned by the
active PU analysis:

- a descriptor identifies the canonical source `TY_IDX`, layout kind, rank,
  alignment, axis-map range, and block-map range;
- axis rows define a complete permutation from source axes to result axes;
- block rows define optional per-axis logical blocking factors;
- site rows identify one semantic tensor root and its alternative range;
- alternative rows connect a descriptor to a provisional
  `TensorEvolutionGraph` node and AIO-2 candidate/plan IDs.

Descriptors are immutable after interning and are uniqued by canonical source
`TY_IDX`, layout kind, complete axis map, and complete block map. Record IDs
are dense, deterministic, and local to the active analysis.

## 3. Tensor Type Discipline

The canonical tensor `TY_IDX` remains the semantic type anchor. A provisional
layout node carries that source `TY_IDX` plus a nonzero runtime representation
descriptor ID. AIO-6 must not call `TY_Intern_Tensor_Type()` for every candidate
because rejected candidates would pollute persistent type tables and alter a
check-only binary WHIRL artifact.

When a later transformation selects and applies an alternative, that phase
must build the refined immutable tensor descriptor and call
`TY_Intern_Tensor_Type()`. Canonical interning then reuses an identical type or
creates one new `TY_IDX`; it must never mutate the original tensor type.

## 4. Initial Alternatives

For each selected rank-two-or-greater intermediate tensor, AIO-6 may create:

1. `permuted`: exchange the final two logical axes;
2. `blocked`: retain axis order and block the final two static dimensions by a
   configurable factor when both dimensions are exactly divisible.

`packed_head` and domain-provided layouts are reserved by the common enum but
are not generated in this milestone. Row-major remains the executable baseline
already represented by the semantic tensor descriptor; it is not duplicated
as an alternative.

## 5. Compatibility And Cost

Compatibility is conservative:

- `proven`: the value has exact same-block lifetime, all consumers are pure,
  and every consumer is representation-transparent under the initial
  `layout` or unary `identity` shape-rule contract;
- `rejected`: an effect boundary or effectful consumer prevents safe logical
  substitution;
- `unknown`: REGION/control boundaries, incomplete lifetime evidence, or a
  non-layout consumer require later analysis.

Static object size gives an exact conversion-volume estimate of one full read
plus one full write. AIO-6 does not reinterpret bytes as relative cost, cycles,
or time. Until a target bandwidth model converts that volume into a comparable
AIO-2 unit, the alternative plan's unhidden-memory term remains unknown and
the plan cost remains incomplete. Every site has an explicit baseline plan;
every alternative plan names the baseline as its fallback. Selection therefore
retains the baseline, keeping this milestone observational and check-only.

## 6. Ownership And Scope

The driver processes one PU at a time, and AIO-6 follows that compilation
scope. Layout descriptors, sites, alternatives, graph overlays, and plan
contexts are owned by the active PU and destroyed with the analysis. Cross-PU
layout propagation belongs to a future explicit IPA scope and is not inferred
automatically.

## 7. Compatibility

AIO-6 adds no mapped-image table, ELF section, opcode, persistent type kind, or
reader/writer contract. Runtime-only descriptor IDs never appear in `.B`
files. The certification contract requires byte-identical before/after/repeat
binary WHIRL and byte-identical `ir_b2a -st -src` output.

If persistence is introduced later, it requires a separately reviewed optional
image contract, mapped-reader validation, logical printing, old-reader
behavior, and an explicit binary compatibility decision.

## 8. Certification

The first fixture is a `common.matmul.v1` result consumed by
`common.relu.v2` with shape `[16,16]`. It proves:

- one site with permuted and 8-by-8 blocked alternatives;
- immutable descriptor interning and exact axis/block rows;
- logical-layout TensorEvolutionGraph nodes and semantics-preserving edges;
- proven compatibility and a 2048-byte read-plus-write conversion estimate;
- baseline fallback and deterministic plan selection;
- no change to `TY_Table`, DSL image node/value counts, `.B`, or `.T`;
- conservative effect, REGION, unknown-shape, invalid-control, and wrong-PU
  behavior.

Human review evidence is retained under
`artifacts/ai_optimization/aio6_logical_layout/`.
