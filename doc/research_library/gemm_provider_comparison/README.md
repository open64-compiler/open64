# GEMM Provider Comparison Research Library

## Purpose

This directory preserves reusable research about cuBLASLt, CUTLASS, and
DeepGEMM. The reports separate public evidence from Open64 design decisions so
future studies can add another GEMM library, code generator, or target without
rewriting the AI optimization architecture.

These reports are research inputs. They do not publish WHIRL operators,
mapped-image rows, runtime ABIs, provider IDs, or default selection policy.
Normative Open64 decisions remain in:

- [AI compiler optimization design](../../AI_compiler_optimization_design_v0.1.md)
- [AI compiler optimization implementation plan](../../AI-COMPILER-OPTIMIZATION-IMPLEMENTATION-PLAN.md)
- [Open64 NVIDIA GEMM and cuBLASLt report](../../OPEN64-NVIDIA-GEMM-CUBLASLT-INTEGRATION-TECH-REPORT.md)

The target-specific NVIDIA runtime, Hopper/Blackwell optimization, and runtime
validation plans remain separate working documents until their own review and
publication. This research library does not make those unpublished plans a
dependency of its evidence or conclusions.

## Design Focus: Compiler-Library Co-Design

This research library supports a focused **compiler-library co-design**
effort. In the first stage, mature and high-performance libraries help shape
the compiler. Their semantic requirements, layout constraints, scaling modes,
epilogues, resource controls, package formats, and runtime behavior reveal the
facts that Open64 must represent explicitly. The compiler should learn from
those successful designs without making any one provider's private types or
implementation choices part of the common IR.

The longer-term objective is repeatable automation for adapting a new library.
A provider description should declare its capabilities and map stable Open64
semantic, layout, tile, pipeline, fusion, package, and runtime fields to its
own interface. Common tooling should then generate or check adapter structure,
legality tables, diagnostics, argument mapping, capability tests, retained
traces, and benchmark-harness registration. Human review remains responsible
for semantic equivalence, numerical behavior, cost-model policy, and ABI or
binary publication.

Adding a new library must not break compatibility with an existing library.
The common operator meaning, TensorDescriptorIR identity, global execution
plan, kernel-invocation plan, fallback behavior, and previously published
provider contracts remain stable. New capability and provider records are
append-only and versioned. Provider-specific extensions stay behind adapters,
and the shared certification matrix reruns all existing provider lanes before
a new adapter can affect default selection.

The positional statement is:

> Open64 uses compiler-library co-design to learn from strong libraries,
> express their reusable requirements in provider-neutral compiler IR, and
> automate new-library integration without weakening semantic, binary, ABI,
> fallback, or behavioral compatibility with existing providers.

This is a two-way relationship. Libraries inform compiler representation and
cost modeling; the compiler supplies libraries with better shape, layout,
fusion, placement, tiling, and workload-context information. The result should
be a stronger compiler and more effective use of each library, rather than a
monolithic compiler or a collection of unrelated special-case calls.

## Report Set

1. [Published Performance And Optimization Strategy](01-PERFORMANCE-AND-OPTIMIZATION-STRATEGY.md)
   compares the evidence each project publishes and the mechanisms each uses.
2. [Roofline And Deterministic Tuning](02-ROOFLINE-AND-DETERMINISTIC-TUNING.md)
   defines how Open64 should diagnose and improve provider performance without
   making exhaustive search the normal compiler algorithm.
3. [Open64 IR To Provider Contract Mapping](03-OPEN64-IR-TO-PROVIDER-CONTRACT-MAPPING.md)
   maps Open64 semantic and physical-plan facts to each provider's parameters
   and records the missing append-only contracts.

## Evidence Policy

Each report distinguishes:

- **public fact**: documented by an upstream project or vendor;
- **inference**: a conclusion drawn from public interfaces or source;
- **Open64 proposal**: a design direction requiring normal review; and
- **measurement required**: a claim that must be reproduced with the Open64
  benchmark harness before it affects provider selection.

Absolute performance numbers are meaningful only with the exact GPU, clocks,
toolkit, provider revision, dtype, accumulation mode, layout, shape, epilogue,
workspace, cache protocol, and measurement method. A speedup against one
project's private or specially tuned baseline is not a general ranking against
the other providers.

## Adding Another Provider

Use the same three-report questions:

1. What public performance evidence exists, and is it comparable?
2. What optimization mechanisms and controllable parameters does it expose?
3. Where does it sit relative to the roofline for the Open64 workload set?
4. Which Open64 semantic, layout, tile, fetch, fusion, target, and runtime facts
   map directly to its interface?
5. Which required facts are missing from Open64 or cannot be controlled by the
   provider?
6. Is the binding provider-heuristic, compiler-constrained, or compiler-exact?
7. What deterministic fallback remains when the provider is unavailable?
8. Which adapter declarations, tests, and argument mappings can common tooling
   generate, and which semantic decisions still require human review?
9. Does the full compatibility matrix prove that previously supported
   providers retain their operator meaning, ABI, fallback, and behavior?

Add a dated evidence snapshot rather than silently replacing an earlier
conclusion when an upstream library changes.

## Source Snapshot

Research snapshot: 2026-09-28.

Primary sources:

- [NVIDIA cuBLAS documentation](https://docs.nvidia.com/cuda/cublas/)
- [NVIDIA CUTLASS documentation](https://docs.nvidia.com/cutlass/latest/)
- [CUTLASS GEMM API 3.x](https://docs.nvidia.com/cutlass/latest/media/docs/cpp/gemm_api_3x.html)
- [CUTLASS Profiler](https://docs.nvidia.com/cutlass/latest/media/docs/cpp/profiler.html)
- [CUTLASS GEMM measurement methodology](https://docs.nvidia.com/cutlass/latest/media/docs/cpp/gemm_performance_measurement_methodology_guidelines.html)
- [DeepGEMM repository](https://github.com/deepseek-ai/DeepGEMM)
- [NVIDIA Matmul Heuristics](https://docs.nvidia.com/cuda/nvidia-matmul-heuristics/)
