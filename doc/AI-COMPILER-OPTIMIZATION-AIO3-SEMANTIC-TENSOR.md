# AIO-3 Semantic Tensor Analysis Contract

## Status

Completed on 2026-09-24. This document defines the first check-only AI-P0
semantic tensor analysis. AIO-3 is runtime-only, per-PU, and additive. It does
not mutate WHIRL, canonical tensor types, TensorDescriptorIR, symbols, DSL
image tables, mapped-image sections, ELF numbering, frontend APIs, or lowering
behavior.

## Purpose

Later AI optimization phases need a common view of each tensor's semantic
role, producer, consumers, type core, ownership, and reuse without erasing the
logical CNN, Transformer, FHE, or other domain operation that established the
fact. AIO-3 attaches that view to AIO-1 semantic roots while retaining exact
logical operator and version identities.

The analysis is deliberately factual rather than profitable. It does not
create an optimization candidate, choose a plan, compute a target cost, infer
a tensor shape, or rewrite an operation.

## Ownership And Scope

1. One `DSL_TENSOR_ANALYSIS` belongs to one active `PU_Info` and one verified
   `DSL_TENSOR_EVOLUTION_GRAPH`.
2. The normal per-PU VHO driver will eventually own both lifetimes. Cross-PU
   analysis remains IPA work.
3. Analysis records own no `WN *`, `ST *`, `CODEREP *`, Python object, or
   borrowed string.
4. Fact and use IDs are contiguous, deterministic, and local to the analysis.
5. Building the same analysis twice is idempotent. Rebuilding it from the same
   IR produces identical records and traces.

## Tensor Facts

Each AIO-1 semantic root receives one immutable `DSL_TENSOR_FACT_RECORD` with:

- the semantic root and managed DSL value IDs;
- canonical descriptor and element `TY_IDX` values;
- dtype, rank, and shape certainty derived from TensorDescriptorIR;
- exact producer node, logical operator, and semantic version;
- value role: constant, model input, formal, intermediate, or symbol;
- ownership: unique, shared, or explicitly unknown;
- reuse role: unused, single-use, or multiple-use;
- a contiguous range of consumer-use facts; and
- structured completeness bits.

The record does not store source names or metadata as semantic keys. Names are
read only while printing review evidence.

## Consumer Facts

Each managed operand reference produces one `DSL_TENSOR_USE_FACT_RECORD` with
the exact consumer node, logical operator, version, operand ordinal, shape
rule, memory behavior, and semantic operand role.

The first role vocabulary covers contraction operands, activation, weight,
bias, normalization scale/mean/variance, attention query/key/value, embedding
index, view source, reduction source, and elementwise input. Classification is
based on registered operator/version contracts and operand ordinals. Unknown
operators retain a generic role; the analysis never guesses from an ST name,
Python variable name, source-layer name, or compiler metadata.

## Structured Sources

AIO-3 reuses existing Open64 and DSL services:

- `TensorEvolutionGraph` supplies one root per live tensor value;
- canonical `TY_IDX` and TensorDescriptorIR supply element type, dtype, rank,
  and logical shape;
- the DSL opcode descriptor supplies logical operator, version, shape rule,
  and effect model;
- the managed value-reference table supplies consumers and operand ordinals;
- the PU-interface image identifies formals; and
- `dsl_memory_behavior` plus the tensor no-alias assertion supplies ownership
  and operand access behavior.

No parallel metadata table or image section is introduced.

## Shape And Completeness

A fully static canonical shape records the exact static rank and zero dynamic
dimensions. A shape that is not accepted by the existing static-shape parser
records `unresolved` with unknown static and dynamic counts. It does not invent
a symbolic or runtime-dynamic interpretation.

Mandatory fact classes are type core, shape, producer, ownership, and
consumers. Missing classes remain visible in the completeness mask and produce
a diagnostic. `DSL_Tensor_Analysis_Is_Complete()` distinguishes a complete
analysis from a structurally valid analysis containing unresolved facts.

This is intentionally compatible with compiler-owned shape refinement. A
later refinement pass may replace an unresolved canonical type for a value;
AIO-3 is then rebuilt for that PU and observes the refined type without
changing its own schema.

## Domain Preservation

The analysis copies exact logical operator IDs and versions from the managed
opcode descriptor. It does not substitute a promoted common operation for a
domain operation. Inspection therefore reports `cnn.conv2d.v2` and
`transformer.rms_norm.v1`, while common matmul remains `common.matmul.v1`.

Promotion, implementation selection, and lowering may consume these facts
later, but only after the owning domain gatekeeper has completed.

## Inspection And Certification

`DSL_Tensor_Analysis_Print()` emits deterministic per-PU fact and use traces.
The printer uses stable logical opcode names from the managed descriptor and
does not expose physical `OPR_DSL` escape encoding. Because the analysis is
runtime-only, its records must not appear in `ir_b2a` output.

The focused certification contains three PUs:

1. `common.matmul.v1` with contraction kid0/kid1 roles;
2. a ResNet-style `cnn.conv2d.v2` with activation/weight/bias roles; and
3. a Llama-style `transformer.rms_norm.v1` with activation/weight roles.

It also proves:

- incomplete pending shape is diagnosed and not reported complete;
- analysis fails closed when its PU is not active;
- source metadata changes do not change fact or use records;
- before, analyzed, and repeated binary WHIRL files are byte-identical;
- their `ir_b2a -st -src` traces are byte-identical; and
- repeated semantic-analysis traces are byte-identical.

The retained local evidence family is
`artifacts/ai_optimization/aio3_semantic_tensor/`. It contains before, after,
and repeated binary WHIRL and ASCII traces, deterministic semantic-analysis
traces, diagnostics, commands, backend dependency/symbol evidence, and
SHA-256 checksums.
