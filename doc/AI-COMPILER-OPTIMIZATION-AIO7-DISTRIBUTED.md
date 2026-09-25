# AIO-7 Placement, Sharding, And Communication Contract

## Purpose

AIO-7 establishes the first common, target-independent representation for
distributed tensor ownership and the communication implied by that ownership.
It covers AI-P4 placement/sharding and the initial AI-P5 communication
derivation. The implementation is runtime-only, check-only, and PU-local.

This milestone builds the optimization skeleton. It does not apply sharding,
insert executable collectives, select a runtime provider, create a new tensor
type, or change binary WHIRL.

## Ownership Model

`DSL_DISTRIBUTED_DESCRIPTOR_RECORD` identifies one immutable alternative for a
semantic tensor:

- placement kind;
- sharding kind and shard axis;
- ownership class;
- device count;
- an associated distributed-alias record.

The alias record owns a contiguous family of per-device tile ranges. A range
records device ordinal, logical axis, half-open bounds, and whether the bounds
are exact or unknown. The alias records whether ranges are proven disjoint,
known to overlap, or unknown.

The first alternatives are:

| Alternative | Ownership | Initial range interpretation |
| --- | --- | --- |
| Replicated | replicated | every device owns the same full range |
| Axis shard | disjoint | devices own non-overlapping chunks of one axis |
| Partial reduction | reduced | devices own overlapping partial results |
| Migration | migrated | one destination device owns the moved full range |

Unknown shape or control information remains explicit. A static axis that is
not divisible by the device count rejects the simple equal-shard alternative.

## Communication Epoch And Intent

Each candidate site has one communication epoch spanning the producer and its
known consumers. AI-P5 derives communication from the ownership alternative:

| Sharding | Logical intent |
| --- | --- |
| Replicated | AllGather |
| Axis | Scatter |
| Partial reduction | AllReduce |
| Migrated | Peer copy |

These names describe logical obligations. They do not select NCCL, MPI, RDMA,
send/receive lowering, topology algorithms, or executable WHIRL operators.
Later stages may refine an intent when they have target topology, layout,
residency, fusion, and schedule evidence.

Communication derivation has an independent control. When disabled, ownership
alternatives remain inspectable, no communication rows are created, and their
legality remains unknown rather than pretending communication is free.

## Tensor Evolution And Plans

Each distributed alternative creates a provisional distributed node in the
runtime-only TensorEvolutionGraph. Replication and migration use a `place`
edge; axis and partial-reduction alternatives use a `shard` edge. All preserve
the semantic root, logical value identity, and canonical tensor `TY_IDX`.

Every site uses AIO-2 OptimizationPlanIR:

- one unchanged baseline candidate and plan;
- one analysis-only plan per ownership alternative;
- explicit legality and rejection reason;
- deterministic ordering;
- complete or incomplete cost terms;
- baseline fallback for every alternative.

The initial cost model uses a relative compute term, known communication bytes,
and a small synchronization term. It is deliberately low confidence. Target
bandwidth, topology, overlap, launch latency, memory capacity, and measured
profiles belong to later cost-model work.

## Legality

The first slice proves an alternative only when:

- the producer's registered effect model is pure and no state-effect row exists;
- lifetime/control analysis is exact within one block;
- disjoint/migrated ownership has proven unique alias ownership;
- equal axis shards have a static dimension divisible by device count;
- partial reduction is attached to a contraction shape rule;
- communication volume and communication derivation are available.

Effects reject the candidate. REGION boundaries, symbolic dimensions, unknown
disjointness, unknown size, and disabled communication derivation produce
unknown legality. This distinction is retained in the plan and trace.

## Scope And Compatibility

All create, build, verify, query, and print operations require the owning PU to
be active with its local symbol table. The analysis never traverses another
PU. Cross-PU placement belongs to an explicit future `-ipa` summary and is not
an implicit extension of ordinary compilation scope.

The records use runtime vectors and are not mapped-image contracts. AIO-7 adds
no ELF section, reader/writer row, opcode, type-kind, persistent tensor
descriptor, or frontend API. The executable WN tree and managed DSL image are
unchanged. A future persistence decision must be append-only and separately
reviewed.

## Controls

`DSL_DISTRIBUTED_CONTROL` independently controls:

- candidate generation;
- communication derivation;
- plan selection;
- transformation application;
- target profile and focus value;
- device, site, and alternative budgets;
- replication, axis-sharding, partial-reduction, and migration alternatives.

Transformation application is rejected in this milestone. Generation-off is a
valid empty analysis. Selection-off still builds complete inspectable plans.

## Certification

The focused `common.matmul -> common.relu` fixture uses one `[8,8]` float32
intermediate on two devices. It demonstrates four alternatives, seven ranges,
four communication intents, and deterministic AIO-2 selection. The simple
relative model selects the axis shard; the other alternatives retain the
baseline fallback.

The test also covers:

- replicated, disjoint, reduced, and migrated ownership;
- exact communication kinds and byte volumes;
- unknown symbolic shape;
- rejected non-divisible equal shard;
- effect and REGION classification;
- independent communication disable;
- generation/application controls;
- active-PU ownership and local-table scope;
- deterministic before/after/repeat analysis;
- byte-identical `.B` and `ir_b2a -st -src` output.

Review artifacts are retained under
`artifacts/ai_optimization/aio7_distributed/` and cleaned at the start of the
next run rather than the end of the current run.

## Deferred Work

- topology-aware collective refinement and target latency models;
- ReduceScatter, AllToAll, send/receive, and RDMA alternatives;
- communication elimination or relocation after fusion;
- local memory residency and capacity integration in AI-P6;
- executable collective insertion and implementation selection;
- mapped-image publication, if later justified;
- explicit `-ipa` placement summaries and cross-PU planning.
