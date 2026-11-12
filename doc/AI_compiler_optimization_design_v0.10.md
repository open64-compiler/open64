# AI compiler optimization design v0.10

2026-11-12 | Explicit device selection in H1

This revision extends [v0.9](AI_compiler_optimization_design_v0.9.md) by
reference. Adopt [resource/memory design v0.3](AI-COMPILER-HARDWARE-RESOURCE-AND-MEMORY-PLANNING-DESIGN-v0.3.md)
and [H20 implementation plan v1.1](AI-COMPILER-H20-MEMORY-HIERARCHY-IMPLEMENTATION-PLAN-v1.1.md).
They make exact H20 96 GB and 141 GB selection a concrete H1 deliverable.
The new gpu_device option remains planned, not registered by this document.

## Decision and current limitation

Require `-DSL:gpu_device=h20_96gb` and `-DSL:gpu_device=h20_141gb` in H1.
Keep gpu_arch as the exact instruction-set selector. Resolve one immutable
context with separate ISA, device, visible resource/partition, calibration and
workload-memory-budget identities before search, then supply it to every
GEMM, MLA, LayerNorm and Reduction consumer through its owning phase.

Current architecture-only sm_90 lookup cannot distinguish H20 from H200.
Its existing quantitative row is an explicitly synthetic envelope, not a
specification of either device. Preserve generic architecture behavior with
honest provenance; it cannot be reported or qualified as device-specific
optimization. An explicit device/profile mismatch is a diagnostic, not an
invitation to guess another board or borrow its rates.

## Required planning behavior

| Concern | Requirement |
| --- | --- |
| 96 GB versus 141 GB | Distinct device keys/resource identities; validate actual model and partition rather than inferring identity from capacity |
| HBM feasibility | Use an explicit usable budget and aggregate live allocations/reservations; nominal capacity is not an allocation guarantee |
| Local scheduling | Use actual RF/SMEM/SM/cache and service inputs; larger HBM alone does not imply a larger tile or deeper pipeline |
| Binding | Device/resource/profile/model/budget inputs participate in dependent snapshots, cache keys, selected-plan checks, guards and reports |
| Unknown or mismatched data | Missing facts cannot justify a dependent candidate; incompatible explicit inputs diagnose, while independently admitted compatible fallback remains available |
| Evidence | Code compatibility and performance qualification are separate; shared ISA cannot transfer H200 measurements to H20 |

Two-phase search remains unchanged: no per-candidate native mutation, device
probe, compilation, timing or correctness execution. Only the eligible selected
plan is applied and qualified, preserving numerical/Q3 contracts, COMMENT,
rollback, analysis rebuilding and High/Mid/Low/PTX boundaries. The
[native WHIRL contract](AI-COMPILER-WHIRL-OPCODE-AND-TENSOR-TRAIT-CONTRACT-v1.0.md)
continues to govern opcode, tensor-trait and phase ownership.

The user has confirmed 96 GB servers; no access, inventory or measurement has
yet been supplied. Supporting the 141 GB option/schema is not a claim of
141 GB hardware acceptance. AOT selection policy, broad PREOPT COMMENT repair
and OpenCTI remain deferred.
