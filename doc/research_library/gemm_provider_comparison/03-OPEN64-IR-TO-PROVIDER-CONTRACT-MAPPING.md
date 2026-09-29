# cuBLASLt, CUTLASS, And DeepGEMM

## Open64 IR To Provider Parameter And Configuration Mapping

Status: research report and proposed mapping, snapshot 2026-09-28.

## 1. Purpose And Boundary

This report maps Open64's AI optimization information to the three provider
interfaces. It distinguishes current Open64 records from proposed append-only
extensions. It does not authorize a binary WHIRL or runtime ABI change.

The binding point is AI-P9 after semantic verification and after the global
execution planner has assigned device placement and communication. The local
kernel planner receives one kernel-dispatch contract and returns one or more
certified `KernelInvocationPlanIR` alternatives plus a fallback.

## 2. Current Open64 Baseline

The current baseline includes:

- logical `common.matmul.v1` and `common.matmul.v2` contracts;
- a `common.gemm` name that still requires a complete executable semantic
  contract before it can represent full BLAS GEMM;
- TensorDescriptorIR and canonical tensor `TY_IDX` identities;
- logical-layout, tile, fetch/pipeline, fusion, residency, candidate, plan,
  cost, and target-profile records;
- `DSL_PHYSICAL_PROVIDER_NVIDIA_CUBLASLT` but no equivalent reviewed CUTLASS
  or DeepGEMM provider identity;
- a fixed 48-byte `OPEN64_DSL_PHYSICAL_PLAN_V1`; and
- `__open64_dsl_matmul_physical_v1(kid0, kid1, result_desc, flags, plan)`.

The existing physical plan carries provider, capability, target profile,
schedule, fallback provider, flags, identity, and reserved fields. It cannot be
reinterpreted as a complete GEMM provider configuration.

## 3. Common Semantic Mapping

Full GEMM semantics are:

```text
D = alpha * op(A) * op(B) + beta * C
```

| Open64 fact | cuBLASLt | CUTLASS | DeepGEMM |
| --- | --- | --- | --- |
| A/B/C/D value and descriptor | Matrix layout descriptors and pointers | Tensor references/strides and runtime arguments | `GemmDesc` tensor/problem fields |
| M/N/K and batch/group dimensions | Matrix descriptors and matmul operation | Problem shape | `GemmDesc` problem/group shape |
| transpose/conjugate | Matmul descriptor attributes | Layout/stride and problem construction | Descriptor major/layout fields where supported |
| alpha/beta | Pointer mode and scale parameters | Epilogue arguments | Descriptor/runtime arguments where supported; otherwise reject |
| input/output dtype | CUDA data and compute types | element and accumulator template types | descriptor dtype/config family |
| accumulation dtype | compute type | accumulator and MMA type | provider accumulation contract |
| numerical/FP contract | compute/precision attributes and algorithm capability | math operator, instruction, and compile options | provider mode/config; unsupported modes reject |
| result TensorDescriptorIR | D layout plus output contract | output tensor/epilogue contract | output descriptor |

`common.matmul` maps only to the subset `alpha=1`, `beta=0`, no C, and no
unrepresented epilogue. Provider bindings must not infer full GEMM semantics
from a two-input matmul node.

## 4. Layout And Representation Mapping

| Open64 fact | cuBLASLt | CUTLASS | DeepGEMM |
| --- | --- | --- | --- |
| logical matrix order | `CUBLASLT_MATRIX_LAYOUT_ORDER` | layout tags or CuTe layout | major/layout field |
| leading dimension | matrix-layout leading dimension | stride object | descriptor stride/layout |
| batch count and stride | matrix-layout batch attributes | problem mode and batched strides | descriptor batch/group layout |
| alignment | matrix descriptor and preference promises | template/runtime alignment | descriptor/config legality |
| grouped offsets | grouped matrix layout | grouped GEMM arguments | grouped layout |
| grouped mask/routing | grouped attributes where supported | grouped kernel metadata | masked/grouped fields |
| scale tensor and granularity | matrix scale mode and scale pointers | scale element/layout and epilogue/mainloop arguments | scale layout and scale tensors |
| output scale/amax | operation descriptor attributes where supported | epilogue arguments | provider output-scale contract |

Required Open64 extensions are structured matrix order, leading dimensions,
batch strides, grouped offsets/masks, scale association, scale granularity,
scale layout, and output-scale semantics. These facts must not be encoded as
free-form metadata strings.

## 5. Tile, Pipeline, And Schedule Mapping

| Open64 plan field | cuBLASLt | CUTLASS | DeepGEMM |
| --- | --- | --- | --- |
| problem M/N/K | Descriptor dimensions | problem shape | `GemmDesc` |
| CTA M/N/K | Heuristic evidence or algorithm capability, not exact control | tile shape | `GemmConfig` block M/N/K |
| cluster M/N/K | Algorithm capability/result where exposed | cluster shape | cluster/multicast configuration |
| warp M/N/K | Generally provider-owned | TiledMma/warpgroup construction | config/runtime implementation fact |
| instruction M/N/K | Algorithm capability where exposed | MMA atom/instruction shape | provider implementation/config family |
| stage count | Algorithm capability/config where exposed | collective stage count | config stage count |
| movement engine | Provider-owned | cp.async/TMA mainloop schedule | TMA/pipeline configuration |
| multicast | Provider-owned | cluster/TMA multicast | multicast configuration |
| persistent schedule | Provider-owned algorithm | tile scheduler/kernel schedule | persistent/JIT config |
| warp-specialized/ping-pong/cooperative | Provider-owned | explicit schedule tags | provider config when supported |
| split-K/reduction | algorithm config/preferences | universal mode and reduction kernel | supported provider mode or reject |
| edge policy | provider kernel behavior | predication/residue handling | masking/shape-specific behavior |

Binding interpretation:

- cuBLASLt is normally `PROVIDER_HEURISTIC` or `COMPILER_CONSTRAINED`;
- CUTLASS can be `COMPILER_EXACT` because the compiler selects concrete kernel
  construction parameters; and
- DeepGEMM can be `COMPILER_CONSTRAINED` or `COMPILER_EXACT` depending on the
  stability and completeness of the reviewed `GemmConfig` boundary.

## 6. Fusion And Epilogue Mapping

| Open64 fusion fact | cuBLASLt | CUTLASS | DeepGEMM |
| --- | --- | --- | --- |
| bias | epilogue mode and bias pointer | collective epilogue argument | supported fused path or reject |
| activation | supported epilogue enum/aux fields | compositional epilogue | supported provider path or separate kernel |
| output conversion | compute/scale/D descriptor | epilogue output operator | provider output contract |
| auxiliary output | epilogue auxiliary pointer/descriptor | epilogue visitor/output | provider-specific capability |
| residual/beta*C | C/D and beta semantics | epilogue source tensor | provider semantics or reject |
| quantize/dequantize scales | scale modes and pointers | mainloop/epilogue scale tensors | native scale-aware path |

Fusion is selected before provider binding, but a candidate remains legal only
if the provider preserves exact operation order, dtype conversion, rounding,
special-value, and auxiliary-output semantics. Unsupported fusion must split at
a reviewed boundary or choose another provider.

## 7. Runtime And Package Mapping

| Open64 contract | cuBLASLt | CUTLASS | DeepGEMM |
| --- | --- | --- | --- |
| provider identity | cuBLASLt and CUDA toolkit | CUTLASS/CUDA/source revision | DeepGEMM/source/JIT revision |
| package identity | library algorithm/config identity | CUBIN/FATBIN/PTX and entry checksum | JIT module/cache/config checksum |
| workspace | preference limit and device pointer | kernel/adapter workspace | JIT/runtime workspace |
| stream | runtime executor | runtime executor | runtime executor |
| target | CUDA device capability | architecture tag and built target | supported architecture/config |
| fallback | alternate algorithm/provider/Open64 baseline | alternate package/provider/baseline | alternate config/provider/baseline |

The runtime executor owns CUDA contexts, handles, streams, events, memory,
module loading, JIT cache, collectives, launch, and result movement. No CUDA
handle, CUTLASS C++ type, or DeepGEMM runtime object belongs in `common/com`.

## 8. Required Append-Only Open64 Extensions

1. Complete executable `common.gemm` operands and attributes.
2. Add stable CUTLASS and DeepGEMM provider identities.
3. Add reviewed FP8 E4M3/E5M2, FP4, scale-factor, and accumulator types.
4. Add structured scale association, granularity, layout, and output scale.
5. Add order, leading dimensions, batch strides, grouped offsets, and masks.
6. Add cluster M/N/K, warp K, split-K/reduction, MMA/WGMMA family, alignment,
   and edge policy.
7. Add persistent, warp-specialized, ping-pong, cooperative, and
   provider-owned schedule kinds.
8. Add ordered epilogue, auxiliary tensor, workspace, and temporary-storage
   contracts.
9. Add provider-heuristic, compiler-constrained, and compiler-exact modes.
10. Add provider/toolkit/target/package/configuration hashes to telemetry.
11. Add an append-only `OPEN64_DSL_GEMM_EXECUTION_V1` rather than changing the
    fixed 48-byte physical-plan v1.

Any persistent extension requires exact-width records, alignment, current and
previous-reader behavior, mapped-load validation before mutation, logical
`ir_b2a -st -src` printing, malformed-image tests, and deterministic fallback.

## 9. Ownership And Implementation Order

| Owner | Work |
| --- | --- |
| `common/com` | Provider-neutral enums/records, creation/query, structural validation, and printing |
| VHO AI optimizer | Populate facts, form/prune candidates, evaluate cost, select provider/binding mode/fallback |
| target/provider adapter | Translate one selected plan into vendor descriptors or kernel construction arguments |
| runtime executor | Package/load/launch, workspace, streams, transfers, collectives, fallback, telemetry |

Implementation order:

1. `AIO-7E`: global execution-plan contract;
2. `AIO-11K`: kernel invocation and package contract;
3. `AIO-11G0`: provider-neutral full GEMM execution contract;
4. `AIO-11G1`: cuBLASLt adapter;
5. `AIO-11G2`: CUTLASS adapter;
6. `AIO-11G3`: DeepGEMM adapter; and
7. `AIO-11G4`: common correctness, roofline, and fallback certification.

## 10. Primary Sources

- [cuBLASLt documentation](https://docs.nvidia.com/cuda/cublas/)
- [CUTLASS GEMM API 3.x](https://docs.nvidia.com/cutlass/latest/media/docs/cpp/gemm_api_3x.html)
- [CUTLASS argument and profiler documentation](https://docs.nvidia.com/cutlass/latest/media/docs/cpp/profiler.html)
- [DeepGEMM repository](https://github.com/deepseek-ai/DeepGEMM)
- [Open64 AI optimization design](../../AI_compiler_optimization_design_v0.1.md)
- [Open64 AI optimization implementation plan](../../AI-COMPILER-OPTIMIZATION-IMPLEMENTATION-PLAN.md)
