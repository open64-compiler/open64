# AIO-11 Physical Plan And Implementation Selection Contract

## Purpose

AIO-11 implements the first selector-only AI-P9 vertical slice. It combines the
selected AIO-9 tile and AIO-10 fetch/pipeline plan with reviewed implementation
capabilities, constructs complete physical alternatives, and selects one plan
through the AIO-2 `OptimizationPlanIR` service.

The selector milestone establishes the decision boundary before executable
lowering. Its analysis is runtime-only and PU-local: it does not rewrite WN,
generate target instructions, allocate target storage, or add mapped-image
rows. The first reviewed application slice now consumes that decision during
normal VHO DSL lowering for `common.matmul.v1` selected to NVIDIA cuBLASLt.

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

Each implementation family must define an atomic prepare, apply, postprocess,
and verify transaction. It must preserve a valid fallback, revalidate the
concrete ABI and target, and publish transformed WHIRL only after the complete
PU passes verification. The first family below establishes that pattern.

## First Runtime Provider Family

The first application family is deliberately narrow: fixed-shape rank-2 F4
`common.matmul.v1` on the reviewed Hopper or Blackwell cuBLASLt capability.
The source `.B` remains logical DSL WHIRL. Selection remains in-memory analysis
state. Only the normal VHO DSL lowering boundary consumes the selected plan.

Lowering follows the established Open64 preparation, engine, and
postprocessing structure:

1. Preparation verifies the complete physical analysis, PU ownership, unique
   semantic-node sites, exact logical opcode/version, reviewed capability,
   provider availability, semantic-preservation flag, provider-owned schedule,
   and direct fallback before WN mutation.
2. Application lowers a copied PU tree through the existing VHO DSL lowering
   engine. The owning `PU_Info` adopts the copy only after lowering and
   canonical verification succeed; a late failure discards it and leaves the
   original tree active. The engine emits
   `__open64_dsl_matmul_physical_v1` instead of the direct matmul ABI for the
   selected site. Other DSL operations continue through their existing
   lowering routes.
3. Postprocessing proves that every selected provider site produced exactly
   one provider call and that no executable DSL carrier remains.

An unsupported selected implementation, including the current generated-kernel
alternative, fails during preparation and leaves the WN tree unchanged.
An unavailable cuBLASLt capability selects the direct baseline and emits the
existing `__open64_dsl_matmul_v1` call.

### Runtime ABI

The provider call receives the existing two tensor handles, result tensor
descriptor, and matmul flags, followed by a pointer to a fixed 48-byte
`OPEN64_DSL_PHYSICAL_PLAN_V1` descriptor. The descriptor carries:

- ABI version, header size, and total size;
- selected provider and reviewed capability ID;
- target profile and schedule;
- direct fallback provider;
- semantic-preservation and fallback flags;
- deterministic physical-plan identity;
- zero reserved storage.

The runtime owns CUDA context, stream, workspace, library handles, provider
setup, failure detection, and direct fallback execution. The compiler does not
link cuBLASLt or another NVIDIA library into `be.so`. This milestone certifies
compiler lowering and binary WHIRL evidence only; it does not claim that a
CUDA runtime adapter executed cuBLASLt or achieved any performance result.

The runtime ABI is append-only. The original direct matmul entry point remains
unchanged for `-O0`, unavailable-provider fallback, and callers that do not
supply an applied physical plan.

## Scope And Compatibility

All APIs require the owning PU and its local symbol table to be active. No
other PU is inspected. Cross-PU provider summaries or implementation selection
belong to an explicit IPA design.

AIO-11 changes no logical opcode value, WN layout, canonical tensor type, ELF
section, mapped-image row, frontend API, or binary WHIRL version. The selector
adds two append-only AIO-2 rejection names. The application slice adds an
append-only runtime ABI entry point and a compiler-generated read-only plan
symbol; it does not alter an existing ABI entry point or require a new binary
WHIRL section.

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

The G14 application lane additionally proves:

- the high-level input `.B/.T` retains logical `OPR_DSLMATMUL` evidence;
- selected cuBLASLt lowering emits one five-argument
  `__open64_dsl_matmul_physical_v1` call and one 48-byte
  `.dsl_physical_plan` symbol;
- unavailable cuBLASLt lowering emits the original four-argument direct call;
- unsupported generated-kernel application fails before WN mutation;
- an induced late lowering failure preserves the original PU tree;
- repeated G14 `.B/.T` output is byte-identical;
- the standard mapped-image reader reopens both provider and fallback output;
- a retained unified diff exposes the exact before/after WHIRL transformation.

Review evidence is retained under the host artifact directory selected by the
test, conventionally `artifacts/ai_optimization/aio11_physical_plan/`, and is
cleaned at the beginning of the next run.

## Deferred Work

- generated Open64 loop/kernel construction and atomic WN rewrite;
- an executable CUDA runtime adapter and actual cuBLASLt library binding;
- concrete CUDA handle, workspace, stream, error, and fallback execution;
- generated-kernel, cuDNN, Triton, and existing-PTX lowering families;
- exact layout, alignment, address-space, target-instruction, and resource
  validation;
- measured or autotuned cost evidence and AIO-12 runtime variants;
- mapped-image publication, if later justified;
- explicit IPA summaries and cross-PU physical-plan selection.
