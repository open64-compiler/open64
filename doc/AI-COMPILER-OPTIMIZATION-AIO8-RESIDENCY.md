# AIO-8 Memory Residency And Hierarchy Contract

## Purpose

AIO-8 implements the first check-only AI-P6 vertical slice. It describes a
target-independent memory hierarchy, imports concrete capabilities through
typed target-profile adapters, and builds per-tensor residency alternatives
through the existing AIO-2 candidate, legality, cost, plan, selection, and
fallback services.

The implementation is runtime-only and PU-local. It does not allocate storage,
promise a cache hit, insert a transfer, alter a tensor type, rewrite executable
WHIRL, or add a mapped-image record.

## MemoryHierarchyDescriptorIR

`dsl_memory_hierarchy.{h,cxx}` owns closed enums and fixed typed records for:

- system, pinned-host, HBM, L2, shared-memory, register, and remote tiers;
- system, device, SM, CTA, and thread scopes;
- capacity, allocation granularity, alignment, relative latency class, cache,
  software-management, spill, and assumption flags;
- CPU baseline, NVIDIA Hopper, and NVIDIA Blackwell profile adapters.

Hot queries use `DSL_Memory_Hierarchy_Get_Profile`,
`DSL_Memory_Hierarchy_Get_Tier`, and `DSL_Memory_Hierarchy_Find_Tier`. They do
not parse free-form option strings, JSON, or frontend metadata.

The NVIDIA values used by this first adapter follow the public NVIDIA tuning
guides. Hopper uses 80 GiB HBM, 50 MiB L2, 227 KiB maximum shared memory per
thread block, and 255 32-bit registers per thread. Blackwell compute
capability 10.0 uses 180 GiB HBM, 126 MiB L2, the same 227 KiB shared-memory
limit, and the same per-thread register limit. The generic CPU baseline marks
its L2/register envelopes as profile assumptions rather than claiming a
particular processor.

References:

- NVIDIA Hopper Tuning Guide:
  https://docs.nvidia.com/cuda/hopper-tuning-guide/
- NVIDIA Blackwell Tuning Guide:
  https://docs.nvidia.com/cuda/blackwell-tuning-guide/

## CommonMemoryResidencyIR

`dsl_residency_candidate.{h,cxx}` owns one immutable descriptor per residency
alternative. A descriptor records:

- canonical source tensor `TY_IDX`;
- target profile, tier identity, kind, and scope;
- allocation-rounded required bytes and tier capacity;
- allocation granularity and minimum alignment;
- promotion, demotion, spill, and eviction policy;
- capacity/lifetime evidence flags.

Each alternative creates a provisional `local_physical` node and
`local_layout` edge in the runtime-only TensorEvolutionGraph. The semantic
root, logical value identity, and canonical tensor type remain unchanged.

## Legality

The first slice classifies alternatives as follows:

- static allocation-rounded size larger than a known capacity is rejected with
  `resource`;
- effect-bearing lifetime is rejected with `effect`;
- shared/register residency without proven unique ownership is rejected with
  `ownership`;
- shared/register residency whose lifetime is not exact within one basic block
  is rejected with `descriptor`;
- symbolic size or unknown capacity remains `unknown` with
  `incomplete_analysis`;
- otherwise the alternative is proven legal for check-only planning.

For an L2 cache, `proven` means only that the object can fit within the modeled
capacity. It is not a promise that the cache retains the object. Eviction,
contention, and measured hit behavior remain later cost and telemetry work.

## Cost And Selection

Every site contains an unchanged baseline plan. Residency alternatives use the
target profile's relative latency class as low-confidence target-model evidence
and retain the baseline as fallback. The relative value is not cycles or time.
Later resource, tiling, pipeline, occupancy, and measured models may refine or
supersede it.

Transformation application is prohibited. Selection identifies the currently
preferred analysis-only plan; it does not allocate the selected tier.

## Scope And Compatibility

All creation, build, verification, query, and print operations require the
owning PU and local symbol table to be active. The analysis never traverses
another PU. Cross-PU residency belongs to an explicit IPA design.

AIO-8 changes no opcode, type kind, symbol-table contract, ELF section,
reader/writer row, or frontend API. The binary WHIRL and `ir_b2a -st -src`
traces remain byte-identical before and after analysis.

## Certification

The focused `common.matmul -> common.relu` fixture proves:

- six Hopper alternatives: system, pinned host, HBM, L2, shared, register;
- explicit promotion, demotion, spill, and eviction policies;
- deterministic AIO-2 selection and TensorEvolutionGraph overlays;
- CPU, Hopper, and Blackwell typed profiles;
- a 64 MiB L2 candidate rejected on Hopper's 50 MiB L2 and accepted on
  Blackwell's 126 MiB L2;
- exact `resource`, `effect`, `descriptor`, and `incomplete_analysis` reasons;
- generation/application controls and PU-local ownership;
- deterministic before/after/repeat analysis;
- byte-identical `.B` and `ir_b2a -st -src` evidence.

Review artifacts are retained under
`artifacts/ai_optimization/aio8_residency/` and cleaned at the start of the
next run rather than at the end of the current run.

## Deferred Work

- live runtime/device capability import and driver option wiring;
- simultaneous capacity accounting across overlapping live tensors;
- cache associativity, contention, persistence windows, and measured hit rate;
- occupancy-aware shared/register budgets and spill-cost refinement;
- physical layout/swizzle and tile-specific residency from AIO-7/P7;
- executable allocation, promotion, demotion, spill, eviction, and transfer;
- mapped-image publication, if later justified;
- explicit IPA residency summaries and cross-PU planning.
