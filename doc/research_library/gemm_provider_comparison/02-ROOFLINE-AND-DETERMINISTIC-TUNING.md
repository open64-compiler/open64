# cuBLASLt, CUTLASS, And DeepGEMM

## Roofline Analysis And Deterministic Compiler Tuning

Status: research report, snapshot 2026-09-28.

## 1. Purpose

This report defines how Open64 should determine whether a provider result is
close to the applicable hardware limit and how to improve it without making
exhaustive search the normal compiler algorithm.

The roofline is a diagnostic bound, not a provider ranking. The same GEMM can
move between bandwidth-bound, latency/launch-bound, occupancy-bound, and
compute-bound regimes as M, N, K, dtype, batching, fusion, workspace, and
placement change.

## 2. Common GEMM Accounting

For

```text
D = alpha * op(A) * op(B) + beta * C
```

the conventional dense GEMM operation count is approximately:

```text
FLOPs = 2 * M * N * K
```

The minimum data movement depends on dtype, beta, epilogue, reuse, and cache
behavior. A simple lower-bound model is:

```text
bytes_min = bytes(A) + bytes(B) + bytes(D)
          + (beta != 0 ? bytes(C) : 0)
          + scale_bytes
          + epilogue_aux_bytes
```

Arithmetic intensity is:

```text
AI = FLOPs / measured_or_modeled_bytes
```

The roofline bound is:

```text
performance_bound = min(peak_compute_for_active_dtype,
                        sustainable_bandwidth * AI)
```

For low-precision tensor-core operations, peak compute must match the exact
input dtype, accumulation mode, sparsity mode, instruction family, and target.
For grouped or small-M GEMMs, launch latency, scheduling imbalance, and poor
wave quantization may dominate before either roof is reached.

## 3. Evidence Required For A Fair Comparison

Every result must retain:

- GPU SKU, compute capability, power/clocks, and memory mode;
- driver, CUDA toolkit, provider, and provider revision;
- M/N/K, batch/group distribution, transpose, layout, strides, and alignment;
- A/B/C/D dtype, accumulation type, scale mode/layout, alpha, beta, and
  numerical contract;
- epilogue and auxiliary operands;
- workspace limit and actual workspace;
- warmup, iteration count, buffer rotation, cache-control protocol, and stream;
- kernel-only time and separately reported host/device transfer time;
- achieved TFLOP/s, measured DRAM/L2 traffic, occupancy/resource facts, and
  launch gaps; and
- selected Open64 plan, provider algorithm/config, fallback, and hashes.

Results lacking these facts are useful leads, not selection-policy evidence.
The CUTLASS measurement guide specifically recommends checking cache behavior,
stable clocks, and gaps between launches.

## 4. Provider Position Relative To The Roofline

### 4.1 cuBLASLt

cuBLASLt often provides a strong production reference because NVIDIA controls
the kernel inventory and heuristic. If it is below the relevant roofline,
Open64 should first diagnose:

- unsupported or suboptimal layout/order/leading dimensions;
- insufficient alignment promise;
- workspace too small for strong algorithms;
- an epilogue or scale mode that narrows the algorithm set;
- split-K or reduction restrictions;
- small problem or launch-latency regime;
- heuristic cache or repeated-query overhead; and
- toolkit/device mismatch.

The tuning action is to improve semantic descriptors, layouts, alignment,
workspace, fusion boundary, batching/grouping, and bounded heuristic ranking.
Open64 must not pretend to tune undocumented internal CTA/warp details.

### 4.2 CUTLASS

CUTLASS can approach the compute roof when tile, cluster, MMA, stages,
movement, schedule, epilogue, and occupancy match the target and shape. If it
falls short, Open64 can act directly on:

- CTA/cluster tile and wave quantization;
- warp/warpgroup and instruction shape;
- stage count and shared-memory capacity;
- register pressure and occupancy;
- TMA versus cp.async/vector movement;
- multicast and cluster topology;
- persistent, cooperative, or ping-pong schedule;
- split-K and reduction cost;
- epilogue fusion and store efficiency; and
- edge-tile predication and alignment.

CUTLASS exposes enough detail for a compiler-exact plan, but exhaustive
instantiation is expensive. Analytical pruning is mandatory.

### 4.3 DeepGEMM

DeepGEMM is strongest when the workload matches its specialized low-precision
and grouped shape families. Below-roofline diagnosis should examine:

- small-M utilization and persistent scheduling;
- block M/N/K and number of resident waves;
- stage count, TMA pipeline, and multicast;
- scale granularity and scale-layout movement;
- grouped workload balance and masking;
- output/epilogue traffic;
- JIT configuration and cache identity; and
- whether the shape is outside the provider's intended workload family.

An unmatched workload should be rejected or fall back rather than triggering
an unbounded search.

## 5. Deterministic Open64 Selection Algorithm

Open64 should use the following ordered process:

1. **Semantic legality**: require exact GEMM, dtype, scale, layout, epilogue,
   numerical, effect, ownership, and result contracts.
2. **Target legality**: filter by compute capability, instruction family,
   alignment, shared memory, registers, cluster support, workspace, and
   provider/toolkit capability.
3. **Shape classification**: classify large compute-bound, bandwidth-bound,
   small-M/latency, batched, grouped, masked, split-K, or edge-heavy regimes.
4. **Analytical candidate generation**: derive a small set from problem
   geometry, hardware hierarchy, data reuse, coalescing, occupancy, and
   epilogue compatibility.
5. **Dominance pruning**: remove candidates dominated in predicted time,
   workspace, compile cost, or portability.
6. **Provider ranking**: ask cuBLASLt, CUTLASS/NVIDIA heuristics, or DeepGEMM
   heuristics only for the bounded legal frontier.
7. **Bounded calibration**: measure a small top-K set only when confidence is
   insufficient and compilation policy permits it.
8. **Certification**: verify numerical results, package identity, guards, and
   fallback before publication.
9. **Telemetry refinement**: update cost evidence without changing semantic
   legality or the identity of an already certified plan.

This is responsible compiler selection. Exhaustive search remains a research
or offline characterization tool, not the default compilation strategy.

## 6. Diagnostic Decision Table

| Symptom | Likely cause | Compiler action |
| --- | --- | --- |
| Low TFLOP/s and near-saturated DRAM bandwidth | Bandwidth-bound | Increase reuse/fusion, improve layout/coalescing, reduce conversion and epilogue traffic |
| Low bandwidth and low compute utilization | Latency, dependency, launch gap, or insufficient parallelism | Batch/group work, persistent schedule, overlap transfers, inspect global execution plan |
| High register use and low occupancy | Tile or fusion too large | Reduce tile/live range, alter warp schedule, split epilogue when profitable |
| Shared-memory capacity rejection | Too many stages or oversized tile | Reduce stages/tile, change movement engine, choose another provider plan |
| Large edge overhead | Poor divisibility/predication | Add guarded shape variant or edge kernel; do not corrupt the main tile |
| Split-K loses performance | Reduction/workspace/synchronization dominates | Use larger K tile, fewer splits, alternate reduction, or no split-K |
| cuBLASLt heuristic result underperforms | Descriptor/workspace/heuristic limitation | Query bounded alternatives, improve constraints, compare exact CUTLASS lane |
| DeepGEMM underperforms outside published shape family | Specialization mismatch | Reject provider and use cuBLASLt, CUTLASS, or Open64 fallback |

## 7. Required Open64 Telemetry

Global-plan telemetry:

- host/device and peer-transfer bytes/time;
- collective/reduction time;
- stream idle time and dependency stalls;
- dispatch and launch gaps; and
- overlap achieved versus planned.

Kernel-plan telemetry:

- kernel duration and achieved TFLOP/s;
- measured DRAM/L2 traffic and arithmetic intensity;
- occupancy, registers, shared memory, waves, and cluster utilization;
- tensor-core/instruction utilization;
- epilogue and workspace cost; and
- provider algorithm/config identity.

Keeping these layers separate prevents a global communication bottleneck from
being misdiagnosed as a local kernel-tile problem.

## 8. Primary Sources

- [CUTLASS GEMM performance measurement methodology](https://docs.nvidia.com/cutlass/latest/media/docs/cpp/gemm_performance_measurement_methodology_guidelines.html)
- [CUTLASS Profiler](https://docs.nvidia.com/cutlass/latest/media/docs/cpp/profiler.html)
- [CUTLASS GEMM heuristics](https://docs.nvidia.com/cutlass/latest/media/docs/cpp/heuristics.html)
- [NVIDIA Matmul Heuristics](https://docs.nvidia.com/cuda/nvidia-matmul-heuristics/)
- [cuBLASLt documentation](https://docs.nvidia.com/cuda/cublas/)
- [DeepGEMM repository](https://github.com/deepseek-ai/DeepGEMM)
