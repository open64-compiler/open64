# cuBLASLt, CUTLASS, And DeepGEMM

## Published Performance And Optimization Strategy

Status: research report, snapshot 2026-09-28.

## 1. Executive Conclusion

There is no defensible universal public ranking of cuBLASLt, CUTLASS, and
DeepGEMM. Their public results use different GPU generations, dtypes, shapes,
epilogues, revisions, baselines, cache conditions, and measurement protocols.
The correct conclusion is about their design positions:

| Provider | Design position | Main strength | Main limitation for Open64 |
| --- | --- | --- | --- |
| cuBLASLt | Closed, highly tuned NVIDIA library with descriptor-driven operation definition and heuristic algorithm selection | Broad production coverage, stable runtime API, fused epilogues, mature heuristics | Internal tile and schedule are mostly provider-owned; an opaque algorithm choice is not an Open64-exact kernel plan |
| CUTLASS | Open compositional CUDA C++ templates and generated kernels | Exact control of layout, tile, cluster, stages, MMA, mainloop, epilogue, and scheduler | Large configuration space, compile-time/code-size cost, and responsibility for kernel selection and packaging |
| DeepGEMM | Specialized JIT GEMM implementation centered on low precision and DeepSeek workload shapes | Strong FP8/FP4 and grouped/MoE specialization, TMA/multicast/persistent scheduling, narrow workload focus | Upstream interfaces and dependencies evolve quickly; published comparisons are workload-specific and often use an internally tuned CUTLASS baseline |

Open64 should treat all three as physical implementation providers under one
semantic GEMM contract. None of them replaces high-level partitioning,
communication derivation, tensor placement, or the global execution planner.

## 2. Public Performance Evidence

### 2.1 cuBLASLt

NVIDIA documents cuBLASLt as a flexible matmul interface with operation and
matrix-layout descriptors, epilogues, workspace preferences, algorithm
heuristics, algorithm checks, and explicit execution. NVIDIA recommends
querying heuristics once and reusing the selected result to avoid repeated
host-side selection overhead.

Public documentation establishes capability and selection behavior, but it
does not provide one fixed cross-library table proving cuBLASLt wins every
shape. Performance varies with toolkit release because the implementation and
heuristics are delivered as part of the CUDA library stack.

Evidence classification:

- broad production capability: public fact;
- exact internal schedule for an opaque algorithm: not public contract;
- superiority over CUTLASS or DeepGEMM for every Open64 shape: measurement
  required.

### 2.2 CUTLASS

CUTLASS publishes architecture-specific performance plots and states that its
device-wide GEMM kernels can approach theoretical peak utilization. Its
profiler prints exact problem, dtype, layout, alpha/beta, split-K, CTA tile,
warp tile, instruction tile, stages, runtime, bandwidth, and GFLOP/s. It can
also use cuBLAS as a correctness reference.

CUTLASS documentation warns that instantiating every kernel can produce tens
of thousands of variants, long build time, large binaries, and linker stress.
The normal Open64 path therefore should generate a small analytically selected
family, not compile the full profiler search space.

Evidence classification:

- near-peak results for documented configurations: public project evidence;
- exact reproducibility: requires matching CUTLASS/CUDA revision and target;
- best result for an arbitrary application shape: measurement required.

### 2.3 DeepGEMM

DeepGEMM publishes H800 results for shapes motivated by DeepSeek-V3/R1. Its
public tables include normal and grouped GEMMs, achieved TFLOP/s, effective
bandwidth, and speedup against an internally optimized CUTLASS baseline. The
project also acknowledges that some shapes perform less well.

Those results demonstrate that aggressive specialization can materially
improve selected low-precision and grouped workloads. They do not establish a
general speedup over current cuBLASLt, every CUTLASS revision, every GPU, or
every GEMM shape. The exact baseline and workload set must stay attached to
the claim.

Evidence classification:

- strong results on the published H800 shape set: public project evidence;
- speedup over the project's internal CUTLASS baseline: public but
  baseline-specific;
- general ranking on H200/Hopper or Blackwell Open64 workloads: measurement
  required.

## 3. Optimization Strategy Comparison

### 3.1 cuBLASLt Strategy

cuBLASLt separates the semantic problem description from algorithm discovery:

1. describe compute type, scale type, transpose modes, pointer mode, epilogue,
   and auxiliary operands;
2. describe A, B, C, and D dimensions, dtype, order, leading dimension, batch
   count, and batch stride;
3. set preferences such as maximum workspace and alignment;
4. request heuristic algorithm candidates;
5. validate or initialize a selected algorithm; and
6. execute on a CUDA stream with caller-provided workspace.

Its optimization strength comes from NVIDIA's internal kernel inventory and
heuristics. Open64 can constrain the problem and rank returned candidates, but
must not claim that its own CTA/warp plan exactly controls an opaque algorithm.

### 3.2 CUTLASS Strategy

CUTLASS exposes the physical hierarchy directly:

- problem shape and operand strides;
- threadblock and cluster shape;
- mainloop collective;
- TiledMma and instruction atom;
- pipeline stage count;
- cp.async or TMA movement;
- warp-specialized, cooperative, ping-pong, or persistent schedule;
- epilogue collective; and
- host-side device adapter and runtime arguments.

This makes CUTLASS the closest match to an exact Open64 local-kernel plan.
Open64 can derive a kernel from its tile, fetch, target, and fusion IR and use
CUTLASS as the implementation substrate. The cost is that Open64 must control
configuration pruning, compilation, package identity, and runtime loading.

### 3.3 DeepGEMM Strategy

DeepGEMM specializes around low-precision model workloads and uses a compact
problem description plus a selected JIT configuration. Public source exposes
concepts including block M/N/K, stages, cluster or multicast behavior,
persistent scheduling, grouped layouts, masks, and scale layouts.

Its performance strategy emphasizes:

- FP8 and newer low-precision data paths;
- block scaling and scale-layout handling;
- TMA-based movement and multicast;
- warp specialization and persistent execution;
- grouped GEMM for MoE-style workloads;
- JIT specialization to the target and shape family; and
- heuristic selection over a deliberately specialized configuration space.

Open64 should map provider-neutral facts to DeepGEMM descriptors and configs.
It must not import Python/PyTorch dependencies into `be.so`; an acceptable
adapter needs a reviewed native boundary or must remain unavailable.

## 4. Comparative Strengths

| Requirement | cuBLASLt | CUTLASS | DeepGEMM |
| --- | --- | --- | --- |
| Broad production GEMM coverage | Strong | Strong but application assembles/builds kernels | Focused |
| Opaque provider heuristic | Native strength | Optional tooling, not defining model | Used inside specialized JIT flow |
| Exact compiler-owned tile/schedule | Limited | Strong | Strong where config API is stable |
| Fused epilogues | Broad documented set | Highly compositional | Provider-specific set |
| FP8 | Supported by target/toolkit combinations | Explicit types and kernels | Central use case |
| FP4/block scaling | New target/toolkit dependent | Explicit Blackwell-oriented support | Current specialized support evolves rapidly |
| Grouped/MoE GEMM | Toolkit-dependent support | Grouped kernels and profiler support | Central use case |
| Compile/JIT burden | Low for caller | Potentially high | JIT and cache required |
| Best fit in Open64 | Production provider heuristic | Exact generated-kernel backend | Specialized low-precision/grouped provider |

## 5. Open64 Research Conclusion

The responsible Open64 strategy is not to crown one provider globally. It is
to preserve one full GEMM semantic contract, derive a bounded legal candidate
set, and choose among:

- cuBLASLt when broad production support and provider heuristics are valuable;
- CUTLASS when Open64 needs exact control over the local kernel hierarchy;
- DeepGEMM when a reviewed low-precision or grouped workload matches its
  specialization; and
- an Open64 baseline when no optional provider is legal or available.

Performance claims enter selection policy only after reproduction with the
shared Open64 correctness and benchmark harness.

## 6. Primary Sources

- [cuBLAS and cuBLASLt documentation](https://docs.nvidia.com/cuda/cublas/)
- [CUTLASS overview and published performance](https://docs.nvidia.com/cutlass/latest/overview.html)
- [CUTLASS GEMM API 3.x](https://docs.nvidia.com/cutlass/latest/media/docs/cpp/gemm_api_3x.html)
- [CUTLASS Profiler](https://docs.nvidia.com/cutlass/latest/media/docs/cpp/profiler.html)
- [DeepGEMM repository and published performance tables](https://github.com/deepseek-ai/DeepGEMM)
