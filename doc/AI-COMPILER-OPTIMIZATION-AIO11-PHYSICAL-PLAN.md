# AIO-11 Physical Plan And Implementation Selection Contract

## Purpose

AIO-11 implements the first selector-only AI-P9 vertical slice. It combines the
selected AIO-9 tile and AIO-10 fetch/pipeline plan with reviewed implementation
capabilities, constructs complete physical alternatives, and selects one plan
through the AIO-2 `OptimizationPlanIR` service.

This milestone establishes the decision boundary before executable lowering.
It is runtime-only and PU-local: it does not rewrite WN, emit a provider call,
generate target instructions, allocate target storage, or add mapped-image
rows. Provider-specific lowering follows one implementation family at a time.

## Open64 Services Reused

- `TensorEvolutionGraph` supplies the semantic result identity carried through
  the preceding planning phases.
- `CommonTilePlanIR` supplies the selected hierarchical tile, resource, and
  target-profile evidence.
- `CommonFetchPlanIR` and `CommonPipelineIR` supply selected movement,
  buffering, barrier, wait, and unhidden-memory-cost evidence.
- `OptimizationPlanIR` supplies immutable candidates, costs, legality,
  fallback, deterministic selection, and plan budgets.
- Logical DSL opcode descriptors and canonical tensor `TY_IDX` values provide
  the exact operator version, element type, and rank matched by a capability.

The selector does not replace target lowering, the Open64 runtime interface,
LNO dependence analysis, or provider libraries. It prepares a complete,
reviewable choice for those established boundaries.

## CommonPhysicalPlanIR

Each physical site identifies one logical DSL result and the selected AIO-9
and AIO-10 plans. Its implementation alternatives record:

- implementation kind and provider;
- reviewed capability ID and target profile;
- tile, fetch, and schedule identities;
- a stable identity derived from the complete selection tuple;
- compute, unhidden-memory, synchronization, launch, and total relative cost;
- legality and rejection reason;
- AIO-2 candidate, cost, plan, selected-plan, and fallback identities;
- baseline, availability, semantic-preservation, provisional, and selection
  flags.

The records use process-lifetime C++ containers. They are compiler analysis
state, not binary WHIRL tables.

## Capability Registry

The first reviewed registry distinguishes:

| Provider | Initial contract | Availability |
| --- | --- | --- |
| Open64 direct | `common.matmul.v1`, F4, rank 2 | Built-in baseline |
| Open64 generated | `common.matmul.v1`, F4, rank 2, Hopper or Blackwell | Built-in planning alternative |
| NVIDIA cuBLASLt | `common.matmul.v1`, F4, rank 2, Hopper or Blackwell | Optional runtime |
| NVIDIA cuDNN | `cnn.conv2d.v2`, F4, rank 4, Hopper or Blackwell | Optional runtime |
| Triton | `common.matmul.v1`, F4, rank 2, Hopper or Blackwell | Optional runtime |
| Existing PTX | `common.matmul.v1`, F4, rank 2, Hopper or Blackwell | Optional runtime |

A capability is an exact, versioned declaration. A provider name alone is not
permission to select it. Operator, version, dtype, rank, target profile, and
runtime availability must all match. Target lowering must revalidate concrete
layout, address-space, alignment, ABI, library, and instruction constraints.

## Selection And Fallback

The direct Open64 implementation is the mandatory baseline and fallback. Every
nonbaseline implementation is a complete AIO-2 plan whose fallback points to
that baseline. Selection compares legal complete plans by deterministic cost
and ordering rules. Provider mismatch and unavailable provider are explicit
rejections rather than silent omission.

The initial cost terms are deterministic, low-confidence relative estimates.
They exercise the selection architecture and do not claim measured latency,
throughput, occupancy, or superiority over a provider implementation.

`-O0` constructs only the direct baseline. It performs no optional search and
does not select a generated or external implementation.

## Application Boundary

The control contains an `apply_selected_plan` field so generation, selection,
and application remain independently debuggable. This selector-first slice
requires that field to be zero and rejects an application request. Therefore:

- the source `.B` never gains an early runtime or provider call;
- selected provider names appear only in analysis evidence;
- executable WN, logical DSL nodes, values, symbols, and tensor types remain
  unchanged;
- before, after, and repeated binary/ASCII artifacts remain byte-identical.

The next implementation-family milestone must define an atomic prepare,
apply, postprocess, and verify transaction. It must preserve a valid fallback,
revalidate the concrete ABI and target, and publish transformed WHIRL only
after the complete PU passes verification.

## Scope And Compatibility

All APIs require the owning PU and its local symbol table to be active. No
other PU is inspected. Cross-PU provider summaries or implementation selection
belong to an explicit IPA design.

AIO-11 changes no logical opcode value, WN layout, canonical tensor type,
symbol table, ELF section, mapped-image row, frontend API, runtime ABI, or
binary WHIRL version. It adds two append-only AIO-2 rejection names for
provider mismatch and provider unavailability.

## Certification

The focused fixed-shape F4 `common.matmul -> common.relu` producer proves:

- the selector consumes exact selected AIO-9 and AIO-10 plan identities;
- direct, generated, cuBLASLt, and Triton alternatives coexist as complete
  plans;
- deterministic selection chooses the lowest-cost proven plan;
- unavailable cuBLASLt and mismatched cuDNN are rejected and fall back;
- generated-kernel selection remains independently testable;
- `-O0` emits only the direct baseline;
- application requests fail closed;
- repeated analysis and all `.B`/`.T` artifacts are byte-identical;
- no provider call or provider name leaks into executable `ir_b2a -st -src`
  WHIRL evidence.

Review evidence is retained under the host artifact directory selected by the
test, conventionally `artifacts/ai_optimization/aio11_physical_plan/`, and is
cleaned at the beginning of the next run.

## Deferred Work

- generated Open64 loop/kernel construction and atomic WN rewrite;
- one reviewed cuBLASLt, cuDNN, Triton, or existing-PTX lowering family;
- concrete provider ABI, handle, workspace, stream, error, and fallback calls;
- exact layout, alignment, address-space, target-instruction, and resource
  validation;
- measured or autotuned cost evidence and AIO-12 runtime variants;
- mapped-image publication, if later justified;
- explicit IPA summaries and cross-PU physical-plan selection.
