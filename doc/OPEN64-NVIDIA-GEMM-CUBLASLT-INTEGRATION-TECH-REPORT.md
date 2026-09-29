# Open64 NVIDIA GEMM and cuBLASLt Integration Technical Report

## Document Status

This report consolidates the technical inquiries and conclusions from the
September 28, 2026 discussion about Open64 matmul/GEMM optimization, NVIDIA
H200 capacity, cuBLAS and cuBLASLt, compiler/runtime decomposition, device-code
packaging, and the relationship between compiler tiling and cuBLASLt algorithm
configuration.

The report is explanatory and architectural. It does not by itself publish a
new WHIRL binary contract, runtime ABI, provider ABI, or persistent execution
plan format. Items labeled **Decision** restate an already established Open64
direction. Items labeled **Recommendation** require implementation review.
Items labeled **Deferred** identify work that should not be implied by the
current implementation.

Related project documents:

- [AI compiler optimization design](AI_compiler_optimization_design_v0.1.md)
- [AI compiler optimization implementation plan](AI-COMPILER-OPTIMIZATION-IMPLEMENTATION-PLAN.md)
- [GEMM provider comparison research library](research_library/gemm_provider_comparison/README.md)

The target-specific Hopper/Blackwell optimization, NVIDIA runtime integration,
and runtime validation plans remain separate working documents pending their
own review and publication. This report explains their intended ownership
boundaries without making an unpublished file part of this report's contract.

## Executive Summary

Open64 should preserve `common.matmul` and `common.gemm` as logical tensor
operations until semantic, shape, numeric, ownership, and domain checks are
complete. Target optimization may then select one of several physical
implementations:

```text
logical common.matmul/common.gemm
  -> Open64-generated NVIDIA kernel
  -> cuBLAS GEMM
  -> cuBLASLt provider plan
  -> existing PTX/cubin kernel
  -> another reviewed provider
```

cuBLASLt is a strong local GEMM provider, but it is not the global Open64
planner. It can accept or discover algorithm configurations involving output
tiles, pipeline stages, split-K, reduction schemes, CTA swizzling, inner MMA
shapes, cluster shapes, layouts, alignment, workspace, and fused epilogues.
It does not decide model partitioning, device placement, cross-GPU reduction,
tensor residency, host/device movement, or graph-wide optimization.

The architecture should therefore have three explicit ownership layers:

1. The **global compiler planner** analyzes WHIRL, selects legal plans, assigns
   work and data, and emits a stable provider-neutral execution description.
2. The **local kernel compiler or provider** realizes each local operation as
   generated PTX/cubin or as a validated vendor-library plan such as
   cuBLASLt.
3. The **native runtime executor** owns CUDA contexts, streams, memory,
   transfers, module loading, provider calls, collectives, synchronization,
   fallback, and telemetry.

Generated Python may be useful as a reviewable reference projection of the
execution plan. It must not become the authoritative optimization plan or the
only executable path. The canonical plan and runtime ABI should remain native
and independent of Python.

## 1. Scope of the Morning Review

The discussion answered these related questions:

1. What has Open64 implemented phase by phase using matmul as the driving test?
2. What matrix sizes can an NVIDIA H200 hold, and what actually limits them?
3. What does cuBLASLt provide, and what inputs and constraints does it require?
4. How is cuBLASLt different from conventional cuBLAS?
5. Should Open64 avoid a monolithic compiler by separating global planning,
   local kernel generation/provider selection, and runtime execution?
6. Is generated Python a suitable representation for the global plan while
   cubin represents local kernels?
7. Can Open64's tiling decisions configure cuBLASLt?

These questions belong to one design problem: deciding what Open64 owns and
what may be delegated to an NVIDIA library without losing compiler-visible
semantics, optimization evidence, reproducibility, or portability.

## 2. Current Open64 Matmul Optimization Ladder

The current AI optimization architecture uses fixed phase order `AI-P0`
through `AI-P11`. The initial implementation deliberately uses one fixed-shape
`common.matmul` in one PU so that representation, analysis, selection,
transformation, and evidence can mature before larger models amplify errors.

### 2.1 AIO-0: Baseline inventory

The initial work identified the existing Open64 services that new AI
optimization must preserve and reuse:

- WHIRL and TensorDescriptorIR for operator and tensor semantics;
- mandatory tensor shape refinement;
- VHO for fixed-order per-PU orchestration;
- PREOPT and WOPT for canonicalization and established scalar optimization;
- LNO for loop, dependence, locality, and tiling infrastructure;
- target information for architectural capability;
- feedback infrastructure for measured evidence; and
- mapped binary WHIRL plus `ir_b2a -st -src` for process-boundary review.

**Decision:** The AI optimizer is not an independent compiler island. It wraps
and extends existing Open64 mechanisms where their semantics apply.

### 2.2 AIO-1: TensorEvolutionGraph identity

The fixed matmul begins with three semantic tensor identities:

```text
kid0: A
kid1: B
result: C
```

Tensor evolution records preserve each value's semantic origin as later phases
consider layout, placement, sharding, memory tier, tiles, staged buffers,
register fragments, and instruction fragments. A physical candidate must not
become an unrelated value with lost lineage.

### 2.3 AIO-2: Candidates, plans, legality, and cost

The first vertical slice constructs:

- a baseline candidate;
- a provisional tiled candidate;
- legality evidence;
- a cost record whose unknown target terms remain explicitly unknown; and
- a conservative baseline selection while the tiled plan is incomplete.

The WHIRL remains unchanged in this slice. This proves deterministic analysis
and plan accounting before mutation.

### 2.4 AI-P0: Semantic tensor analysis

The semantic phase establishes the facts later transformations must preserve:

- logical operator and version;
- operand and result TensorDescriptorIR identities;
- shape, dtype, rank, and transpose semantics;
- numeric mode;
- source and owner PU;
- effects and alias/ownership constraints; and
- domain meaning that cannot be hidden by premature lowering.

For a rank-2 matmul after applying transpose attributes:

```text
A: [M, K]
B: [K, N]
result: [M, N]
```

The shared reduction dimension must agree. Incompatible or unresolved
dimensions fail or remain pending according to the operator's reviewed shape
contract.

### 2.5 AI-P1: Lifetime, reuse, and locality

This phase determines when A, B, and the result are live, how often input data
may be reused, and what locality opportunities exist. It supplies evidence for
later placement and tile plans. It does not itself choose an NVIDIA kernel.

### 2.6 AI-P2: Fusion

The fusion phase constructs and filters candidates around matmul and adjacent
operations. The current work provides the optimization skeleton and explicit
legality/profitability boundaries. It does not claim that a complete universal
pattern catalog exists.

Longer-term candidate discovery should combine:

- operator-contract rules;
- graph properties;
- effect, use-count, and ownership analysis;
- shape and layout compatibility;
- target capability;
- bounded search; and
- measured profitability.

### 2.7 AI-P3: Logical layout alternatives

Logical layout matters because it determines which dimensions are contiguous,
which accesses coalesce, whether vector loads are legal, how operands map to
MMA instructions, and how much layout conversion is required. This phase
keeps logical alternatives separate from target-specific physical layouts.

### 2.8 AI-P4 and AI-P5: Placement, sharding, and communication

These phases construct placement and sharding candidates and derive the
communication they require. For matmul:

- splitting M creates disjoint row regions of the result;
- splitting N creates disjoint column regions;
- splitting K creates partial results that require reduction; and
- 2-D or 3-D decompositions introduce broadcasts, gathers, reductions, or
  reduce-scatter operations.

This is global planner work. A local cuBLASLt call cannot replace it.

### 2.9 AI-P6: Memory hierarchy and residency

The planner considers host memory, pinned memory, HBM, L2, shared memory,
registers, and architecture-specific resources. It rejects candidates whose
capacity, lifetime, or ownership requirements cannot be satisfied.

### 2.10 AI-P7: Hierarchical tiling

The matmul plan is refined through reviewable tiling levels rather than one
opaque "tile" number. Candidate information includes:

- output and reduction macro-tiles;
- device and cluster partitioning;
- CTA/block tiles;
- warp or warpgroup tiles;
- per-thread result tiles;
- shared-memory tiles;
- register and instruction fragments;
- vector widths;
- edge handling;
- occupancy, register, and shared-memory estimates; and
- deterministic tuning keys.

The `G0` through `G*` artifact ladder exposes these transformations. The
Hopper/Blackwell plan currently defines stages from the post-PREOPT baseline
through naive materialization, coalescing, shared-memory blocking, thread and
warp tiling, vectorization, resource balancing, autotuning, asynchronous
movement, and architecture-specific scheduling.

### 2.11 AI-P8: Fetch, prefetch, and asynchronous pipelines

This phase consumes the selected tile relationships and creates legal movement
plans:

- demand loads;
- vector loads;
- asynchronous copies;
- TMA-like transfers;
- stage count and buffering;
- issue, arrival, wait, and barrier points; and
- fallback behavior.

It must not rediscover a disconnected tile plan.

### 2.12 AI-P9: Physical implementation selection

This is the phase where a complete legal matmul plan can choose among:

- Open64-generated loops or GPU kernels;
- a baseline runtime provider;
- cuBLAS or cuBLASLt;
- cuDNN where the logical operation is appropriate;
- reviewed Triton or PTX kernels; or
- another registered provider.

Provider selection is a physical decision. It must not rewrite the source
`.B` artifact into an early vendor call before semantic optimization finishes.

### 2.13 AI-P10: Runtime variants

The compiler may certify multiple implementations guarded by shape,
alignment, target, or workload-state conditions. Every fast path requires a
correct fallback, and runtime selection may choose only among statically
certified alternatives.

### 2.14 AI-P11: Telemetry and feedback

Measured latency, occupancy, traffic, launch cost, and variant hit rate can
alter future profitability decisions. Measurement must never retroactively
change architectural legality or the meaning of a previously certified plan.

## 3. GEMM Semantics Versus Matmul

Matmul and GEMM should not be conflated.

The conventional GEMM equation is:

```text
D = alpha * op(A) * op(B) + beta * C
```

where `op(A)` and `op(B)` may be identity or transpose, and in a future complex
contract may include conjugate transpose.

The established `common.matmul` subset corresponds conceptually to:

```text
alpha = 1
beta = 0
no semantic read of C
no fused epilogue
```

The broader GEMM contract must explicitly preserve A, B, C, D, alpha, beta,
transpose, compute type, scale type, and out-of-place versus in-place
semantics. A performance experiment that computes only `A*B` must not be
reported as full GEMM when beta and C are part of the stated contract.

## 4. H200 Matrix Capacity

### 4.1 There is no single maximum matrix dimension

The largest matrix an H200 can process depends on:

- element type;
- number of simultaneously resident operands;
- in-place or out-of-place result;
- workspace;
- allocator fragmentation;
- CUDA context and runtime reservations;
- additional live model tensors; and
- whether the operation is partitioned or streamed.

The H200 SXM and NVL product family is advertised with 141 GB of HBM3e per GPU.
That is a capacity bound, not a promise that all 141 GB is available to one
allocation.

### 4.2 General storage formula

For row-major or column-major dense GEMM with no packing, storage is
approximately:

```text
bytes = sizeof(A) * M*K
      + sizeof(B) * K*N
      + sizeof(C) * M*N
      + sizeof(D) * M*N
      + workspace
      + runtime reserves
```

If `beta=0` and C is not materialized, its term can be removed. If C and D are
legally identical, one result-sized allocation can be removed. Packing,
quantization scales, auxiliary epilogue output, or split-K accumulation adds
storage.

### 4.3 Square FP32 examples

For square `N x N` FP32 matmul with A, B, and one output:

```text
bytes = 3 * 4 * N^2 = 12 * N^2
```

An idealized 141 GB bound gives:

```text
N <= floor(sqrt(141,000,000,000 / 12))
N <= approximately 108,397
```

This is not a practical operating point. A more reviewable aligned example is:

```text
N = 102,400
one FP32 matrix = 41,943,040,000 bytes
three matrices = 125,829,120,000 bytes
```

That consumes about 125.8 decimal GB before workspace and runtime overhead.

For out-of-place FP32 GEMM with separate A, B, C, and D:

```text
bytes = 16 * N^2
idealized N <= approximately 93,808
```

Practical certification should use a configured memory budget below physical
capacity and report every allocation. Larger logical matrices require
partitioning, streaming, multiple GPUs, or external storage.

### 4.4 Compiler consequence

Capacity calculation belongs in memory-plan legality. It must account for the
entire live set and selected provider workspace, not merely ask whether one
matrix fits. The same logical GEMM may have different legal plans on H200 and
Blackwell because capacity, bandwidth, compute, cluster, and instruction
capabilities differ.

## 5. cuBLAS Functionality

cuBLAS implements the traditional Basic Linear Algebra Subprograms on NVIDIA
GPUs. Its scope includes:

- BLAS Level 1 scalar/vector operations;
- BLAS Level 2 matrix/vector operations;
- BLAS Level 3 matrix/matrix operations;
- GEMM and mixed-precision `GemmEx` variants;
- batched and strided-batched families;
- triangular, symmetric, Hermitian, and rank-update operations;
- stream and pointer-mode controls; and
- selected helper and extension functions.

The conventional interface normally exposes one routine per mathematical
operation, such as `cublasSgemm`, `cublasGemmEx`, or `cublasStrsm`.

cuBLAS remains the simpler choice when Open64 needs a conventional supported
BLAS operation and does not need cuBLASLt's detailed GEMM descriptor and
algorithm-selection surface.

## 6. cuBLASLt Functionality

cuBLASLt is a narrower, descriptor-driven library focused on flexible GEMM.
"Lightweight" describes the focused API scope; it does not mean that its GEMM
capabilities are weaker.

### 6.1 Operation description

`cublasLtMatmulDesc_t` describes matters such as:

- transpose or conjugate-transpose behavior;
- compute and scalar scale types;
- pointer mode;
- numerical implementation constraints;
- batch-invariance requirements where supported;
- epilogue selection;
- bias and auxiliary pointers; and
- quantization and scaling controls.

### 6.2 Matrix layouts

Each A, B, C, and D matrix has a `cublasLtMatrixLayout_t` describing:

- element type;
- rows and columns;
- leading dimension;
- layout order;
- batch count and batch stride;
- pointer-array or grouped mode where supported; and
- specialized plane, packing, or low-precision details.

### 6.3 Mixed precision and scaling

Storage, compute, accumulator, output, and scale types can be configured
separately for supported combinations. Current hardware/library combinations
cover conventional floating point, BF16/FP16, TF32, FP8, FP4, integer, and
complex cases with target-specific restrictions.

Mixed precision is not unique to cuBLASLt because cuBLAS has `GemmEx`.
cuBLASLt makes the configuration and algorithm constraints more explicit and
programmable.

### 6.4 Fused epilogues

Depending on hardware, datatype, layout, and batch mode, cuBLASLt can combine
GEMM with selected operations such as:

- bias addition;
- ReLU;
- GELU;
- bias plus activation;
- activation auxiliary output;
- activation gradients;
- bias-gradient reductions;
- scaling and output conversion; and
- selected amax production.

These capabilities do not silently enlarge the semantics of
`common.matmul` or `common.gemm`. Open64 must first prove the corresponding
fusion and numeric contract, then select a compatible provider epilogue.

### 6.5 Algorithm discovery and validation

cuBLASLt allows a caller to:

- request heuristic candidates;
- impose a workspace limit;
- inspect candidate workspace and `wavesCount`;
- query algorithm capabilities;
- configure supported algorithm attributes;
- validate the configured algorithm against descriptors and device; and
- reuse the selected configuration for repeated calls.

The core flow is:

```text
operation and matrix descriptors
  -> preference and workspace constraints
  -> cublasLtMatmulAlgoGetHeuristic
  -> inspect/configure candidates
  -> cublasLtMatmulAlgoCheck
  -> cublasLtMatmul
```

### 6.6 Batched and grouped execution

cuBLASLt supports strided batching and selected pointer-array/grouped forms.
Grouped GEMM and advanced low-precision batch behavior remain constrained by
CUDA version, compute capability, layout, alignment, and epilogue support.
Open64 must model these as explicit target/provider capabilities rather than
assuming they are universally available.

### 6.7 Supporting matrix transformations

`cublasLtMatrixTransform` supports selected matrix copying, transposition,
scaling, conversion, combination, and layout-preparation operations. It is not
a general graph-level tensor transformation compiler.

## 7. cuBLAS and cuBLASLt Comparison

| Topic | cuBLAS | cuBLASLt |
| --- | --- | --- |
| Main purpose | Broad BLAS implementation | Programmable GEMM provider |
| Operation scope | BLAS Levels 1, 2, and 3 plus extensions | GEMM, fused GEMM processing, and supporting matrix transforms |
| Interface style | Operation-specific functions | Descriptor-driven `cublasLtMatmul` |
| Vector and matrix-vector APIs | Yes | No general BLAS-1/2 family |
| TRSM/TRMM/SYRK/HERK and similar | Yes | Not the primary cuBLASLt surface |
| Mixed precision | Yes, including `GemmEx` | Rich descriptor and scaling model |
| Layout control | Conventional BLAS arguments and extensions | Explicit descriptor for every operand/result |
| Fused epilogues | More limited | Bias, activations, gradients, auxiliary outputs, scaling, subject to support |
| Algorithm selection | Primarily library selected | Heuristic query, capability inspection, configuration, and validation |
| Workspace | API-specific | Central algorithm preference/result property |
| Best Open64 role | Standard BLAS operation provider | Tunable local GEMM provider |

Neither interface is a whole-model planner. Multi-GPU BLAS services are
separate facilities such as cuBLASXt or cuBLASMp, and they still do not replace
Open64's graph, semantic, placement, and optimization responsibilities.

## 8. What cuBLASLt Requires from Open64

Before asking cuBLASLt to choose an algorithm, the compiler/runtime must know:

1. M, N, K, batch, and transpose/conjugation semantics.
2. A, B, C, and D element and physical layout descriptions.
3. Compute, accumulator, output, and scale types.
4. Alpha and beta values and pointer mode.
5. Whether C and D alias or are distinct.
6. Epilogue and auxiliary-buffer requirements.
7. Pointer, stride, leading-dimension, and alignment guarantees.
8. Maximum workspace.
9. Target GPU and CUDA/cuBLASLt version.
10. Numeric, determinism, and reproducibility requirements.
11. Batch/group mode and its target-specific restrictions.
12. A deterministic fallback if no candidate is valid.

Open64 owns the proof that these inputs agree with logical WHIRL semantics.
cuBLASLt validates only its provider configuration; it cannot prove that the
compiler selected the correct source-level operation.

## 9. Tiling: Open64 Versus cuBLASLt

### 9.1 The critical distinction

The word "tile" describes multiple nested decisions:

```text
model/device partition
  -> GEMM macro-subproblem
     -> CTA or thread-block tile
        -> warp/warpgroup tile
           -> thread/register tile
              -> MMA instruction fragment
```

Open64 may reason about every level. cuBLASLt configures only the local GEMM
algorithm that it executes.

### 9.2 Open64 workload tiling

Open64's workload tiling decides:

- which device owns a tile;
- which M/N regions produce disjoint result regions;
- whether K partitioning creates partial results;
- when reduction, all-reduce, reduce-scatter, gather, or broadcast is needed;
- where data resides;
- how host/device and peer transfers overlap;
- how many local GEMM calls are issued; and
- how tile dependencies are scheduled.

For example, an Open64 macro-plan may assign a `4096 x 4096 x 8192`
subproblem to one GPU. That is the problem passed to cuBLASLt; it is not a
single cuBLASLt thread-block tile.

### 9.3 cuBLASLt local kernel tiling

Inside one invocation, cuBLASLt exposes algorithm-specific controls including:

| Compiler preference | cuBLASLt configuration |
| --- | --- |
| Local output tile | `CUBLASLT_ALGO_CONFIG_TILE_ID` |
| Pipeline stages | `CUBLASLT_ALGO_CONFIG_STAGES_ID` |
| Local K parallelism | `CUBLASLT_ALGO_CONFIG_SPLITK_NUM` |
| Partial-result reduction | `CUBLASLT_ALGO_CONFIG_REDUCTION_SCHEME` |
| Output-tile traversal | `CUBLASLT_ALGO_CONFIG_CTA_SWIZZLING` |
| MMA geometry | `CUBLASLT_ALGO_CONFIG_INNER_SHAPE_ID` |
| CTA cluster topology | `CUBLASLT_ALGO_CONFIG_CLUSTER_SHAPE_ID` |
| Algorithm-specific control | `CUBLASLT_ALGO_CONFIG_CUSTOM_OPTION` |

The tile enumeration includes dimensions such as `32x32`, `64x64`, `64x128`,
`128x64`, and `128x128`, but an algorithm supports only its advertised subset.
Tile compatibility also depends on dtype, layouts, epilogue, alignment,
workspace, GPU architecture, and library version.

### 9.4 Correct binding sequence

```text
selected Open64 physical plan
  -> construct cuBLASLt operation/layout descriptors
  -> request heuristic candidates
  -> inspect each candidate's supported tile/stage/split/cluster capabilities
  -> apply compatible Open64 preferences
  -> validate with cublasLtMatmulAlgoCheck
  -> execute or try the next candidate/fallback
```

A successful algorithm check reports compatibility, required workspace, and
wave count. Runtime pointer alignment and other dynamic requirements still
have to hold at execution.

### 9.5 Matching policy

**Recommendation:** Define three provider-matching policies:

```text
AUTO
  Let the provider choose among legal algorithms.

PREFER
  Prefer Open64's local tile and pipeline choices but allow another supported
  provider configuration when it is better or the preference is unavailable.

REQUIRE
  Demand the specified provider configuration. If it is unavailable, use a
  different implementation provider or fail according to the compiled plan.
```

`PREFER` is the reasonable general default. `REQUIRE` is useful for controlled
experiments, replay, and detailed compiler/provider comparison.

### 9.6 Split-K boundary

cuBLASLt split-K is a local algorithm feature. Open64 may map a local K split
to `SPLITK_NUM` and a supported reduction scheme. A K split across devices is
different: each device produces a partial result and the Open64 execution plan
must schedule the required collective or peer reduction.

### 9.7 Generated-kernel alternative

If Open64's selected detailed tile, movement, or instruction schedule cannot be
expressed by cuBLASLt, the compiler should not distort the plan to resemble an
unsupported provider option. It may instead select the Open64-generated
PTX/cubin path, subject to its legality and profitability evidence.

## 10. Recommended Non-Monolithic Architecture

### 10.1 Global compiler planner

The global planner belongs in Open64 C++ phases such as VHO, LNO, WOPT, and
IPA according to compilation scope. It owns:

- semantic graph and tensor identities;
- shapes and numeric contracts;
- fusion and graph transformation;
- placement and sharding;
- macro-tiling and distributed decomposition;
- communication derivation;
- memory residency and lifetime;
- provider-neutral task dependencies;
- legal fallback variants; and
- cost and telemetry relationships.

Normal VHO/WOPT/LNO work remains PU-local. Cross-PU planning occurs only in an
explicit IPA scope and uses summaries rather than stale local pointers.

### 10.2 Local kernel compiler or provider

Each local task is realized by one reviewed implementation family:

```text
OPEN64_GENERATED_CUBIN
OPEN64_GENERATED_PTX
OPEN64_FATBINARY
CUBLAS_PROVIDER_PLAN
CUBLASLT_PROVIDER_PLAN
CUDNN_PROVIDER_PLAN
REGISTERED_EXISTING_KERNEL
```

For generated kernels, the local compiler owns scheduling, instruction
selection, PTX emission, `ptxas`, cubin/fatbin packaging, entry ABI, launch
requirements, and retained disassembly/resource evidence.

For cuBLASLt, the provider adapter owns descriptor construction, algorithm
query/configuration, workspace, caching, invocation, and provider diagnostics.

### 10.3 Native runtime executor

The runtime owns dynamic resources and execution:

- target discovery and capability checks;
- CUDA context and stream lifetime;
- device and pinned-host allocation;
- H2D, D2H, and peer transfers;
- generated module loading and symbol lookup;
- cuBLAS/cuBLASLt provider calls;
- NCCL or other collective invocation;
- events, dependencies, and synchronization;
- CUDA Graph capture or launch where selected;
- fallback dispatch; and
- telemetry collection.

The runtime executes a certified plan. It does not perform graph optimization
or repair an illegal compiler decision.

### 10.4 Comparison with NVIDIA's environment

This layered design is comparable to NVIDIA's software organization:

- frameworks and distributed libraries partition and coordinate workloads;
- cuBLASLt, cuDNN, CUTLASS, Triton, or generated CUDA provide local kernels;
- CUDA runtime/driver services manage memory, streams, modules, and launches;
- NCCL provides collectives; and
- CUDA Graphs can package repeated launch dependencies.

Open64's distinctive value is keeping the semantic operation, transformation
history, legality, source relationship, optimization plan, and binary WHIRL
evidence under one compiler architecture instead of relying on disconnected
framework conventions.

## 11. Python, Cubin, and Runtime Outputs

### 11.1 Python as an optional plan projection

Generated Python can be valuable for:

- human inspection;
- rapid bring-up;
- test orchestration;
- comparing plan intent with framework behavior;
- invoking the native runtime through a narrow binding; and
- reproducing task order and guards during early development.

It should not own:

- optimization decisions;
- tensor legality;
- fallback correctness;
- raw WHIRL interpretation;
- provider algorithm semantics; or
- the only copy of the execution plan.

**Decision:** Python remains a frontend and optional review/control projection.
The middle end and executable runtime must remain independent of Python.

### 11.2 Cubin as a local implementation artifact

Generated cubin is suitable for target-specific local kernels. It provides
reproducible architecture-specific machine code when built with pinned tools
and options. A PTX fallback may be packaged separately for development or
forward compatibility, but performance certification should name and hash the
actual target cubin.

Vendor-library providers such as cuBLASLt do not give Open64 an owned cubin.
They should instead produce a provider plan and runtime selection record.

### 11.3 Proposed artifact family

**Recommendation:** Use an artifact family conceptually like:

```text
model.B
model.T
model.execplan
model.plan.py
model.runtime-manifest
kernels/<kernel>.<target>.cubin
kernels/<kernel>.ptx
kernels/<kernel>.device.json
providers/<operation>.provider.json
```

The exact persistent `model.execplan` format is **Deferred**. It requires a
separate compatibility review. It must not be introduced casually as an ad hoc
file or unreviewed WHIRL section.

`model.plan.py` is generated from the canonical plan and may call the same
native runtime used by the eventual standalone executable. It is never the
authoritative source from which correctness is reconstructed.

### 11.4 End-to-end control flow

```text
Python model
  -> torch2whirl
  -> logical binary WHIRL model.B
  -> gatekeeper and shape refinement
  -> PREOPT/canonicalization
  -> AI-P0 ... AI-P11 according to optimization level
  -> selected provider-neutral physical plan
  -> per-task implementation:
       generated WHIRL -> NVISA -> PTX -> ptxas -> cubin
       or cuBLASLt provider configuration
  -> canonical native execution plan
  -> optional generated Python projection
  -> native runtime executor
       allocate/transfer
       load cubin or bind provider
       launch local work
       perform collectives
       return results
       collect telemetry
```

## 12. Provider Plan Versus Persistent WHIRL

The compiler should persist portable intent and compatibility contracts, not a
raw opaque cuBLASLt algorithm object.

NVIDIA documents `cublasLtMatmulAlgo_t` as restorable for use with the same
cuBLAS library version. That is too narrow for Open64's standing binary WHIRL
compatibility rule.

The durable compiler/provider contract should describe:

- target capability requirements;
- M/N/K and batch contract;
- layouts, strides, alignment, and dtypes;
- numeric and epilogue semantics;
- workspace limit;
- preferred tile, stages, split-K, cluster, and matching policy;
- determinism requirements;
- provider and generated-kernel alternatives; and
- fallback order.

At runtime, a cache may retain the resolved opaque algorithm keyed by:

- GPU architecture and device capability;
- CUDA and cuBLASLt version;
- operation and layout descriptor identity;
- pointer-alignment class;
- workspace budget; and
- provider preference identity.

That cache is replaceable runtime state, not logical WHIRL semantics.

## 13. Correctness and Performance Protocol

### 13.1 Correctness before performance

Every provider and generated stage must first prove:

- shape and type legality;
- alpha, beta, transpose, C/D, and epilogue semantics;
- no out-of-bounds access;
- expected NaN/infinity behavior;
- numeric error within the active contract;
- identical source inputs and output comparison protocol; and
- deterministic artifact identity for the measured configuration.

### 13.2 Fair cuBLASLt comparison

Generated and cuBLASLt cases should use:

- the same A, B, C, and D buffers when contracts permit;
- the same shapes, dtypes, alpha, beta, and transpose modes;
- the same stream and synchronization boundary;
- equivalent warm-up;
- separately reported workspace;
- excluded one-time module load and heuristic search for steady-state timing;
- included setup cost in a separately named end-to-end metric; and
- pinned CUDA, driver, cuBLASLt, device, and power/clock conditions.

cuBLASLt is a matched numerical and performance comparator, not necessarily a
bit-exact host oracle. Bit-exact claims require identical compiler/library
options and a proven reproducibility configuration.

### 13.3 Roofline evidence

For every GEMM stage, report:

- conventional GEMM work, normally `2*M*N*K` operations;
- measured and lower-bound bytes moved;
- arithmetic intensity;
- achieved FLOP/s;
- applicable compute and bandwidth roofs;
- percentage of the limiting roof;
- percentage of matched cuBLASLt performance;
- occupancy, register, shared-memory, spill, and launch evidence; and
- the next limiting resource and proposed improvement.

### 13.4 Retained review artifacts

Keep source, `.B`, `ir_b2a -st -src` `.T`, `G*` checkpoints, `whirl2c`, plan
trace, PTX, cubin/fatbin, `ptxas` logs, provider manifest, launch records,
performance report, diagnostics, commands, and checksums in a host-mounted
artifact directory. Clean it before the next run, not after the current run.

## 14. Proposed Integration Stages

### Stage 1: Reference execution boundary

1. Keep logical `common.matmul`/`common.gemm` unchanged through semantic
   optimization.
2. Define one provider-neutral local task contract in memory.
3. Execute one generated H200 cubin with a reviewed PTX fallback.
4. Execute one matched cuBLASLt provider case.
5. Use generated Python only as a reference plan projection.
6. Retain complete correctness and performance artifacts.

### Stage 2: Shared native runtime

1. Move allocation, transfers, module loading, provider invocation, and
   synchronization into a native runtime.
2. Make the Python projection call this runtime rather than duplicate it.
3. Add deterministic fallback and provider diagnostics.

### Stage 3: Canonical execution plan

1. Review a versioned provider-neutral execution plan contract.
2. Make both the native executable and Python projection consume that plan.
3. Keep opaque cuBLASLt algorithm state outside the persistent contract.

### Stage 4: Multi-device proof

1. Add a two-device partitioned GEMM.
2. Start with a simple M or N split for disjoint outputs.
3. Add a K-split case with an explicit NCCL reduction.
4. Distinguish global partition cost from local cuBLASLt performance.

### Stage 5: Repeated execution and telemetry

1. Add optional CUDA Graph execution with a stream-based fallback.
2. Feed measured local and communication costs into AI-P11.
3. Permit feedback to change future profitability, never legality.

## 15. Decisions, Recommendations, and Deferred Questions

### 15.1 Decisions

1. Logical DSL tensor semantics remain visible until gatekeeping and
   architecture-independent optimization complete.
2. The global planner, local implementation provider, and runtime executor are
   separate ownership layers.
3. cuBLASLt is a local GEMM provider, not a graph or multi-device planner.
4. Open64 tiling may guide cuBLASLt, but global and provider-internal tiles are
   distinct.
5. Python may project a plan for review but is not the authoritative middle
   end or runtime.
6. Generated target cubin and cuBLASLt provider selection are different
   implementation families.
7. Opaque version-specific cuBLASLt algorithm state must not become stable
   logical WHIRL semantics.

### 15.2 Recommendations requiring review

1. Define `AUTO`, `PREFER`, and `REQUIRE` provider matching policies.
2. Define a provider-neutral local task and execution-plan model.
3. Build one common native runtime used by generated Python and native drivers.
4. Add explicit cuBLASLt capability translation for tile, stage, split-K,
   reduction, swizzle, inner shape, cluster shape, alignment, and workspace.
5. Add runtime caching keyed by device, library version, descriptors, and
   provider preference identity.
6. Use an aligned H200 capacity test while keeping capacity and performance
   tests separate.

### 15.3 Deferred

1. Persistent execution-plan file format and compatibility policy.
2. Any new mapped WHIRL section for physical provider plans.
3. Stable generated-Python plan API.
4. Multi-node planning and failure recovery.
5. Complete cuBLASLt grouped-GEMM support matrix.
6. Blackwell-specific FP4 and cluster/CTA controls in the first provider slice.
7. Automatic choice between generated cubin, cuBLASLt, CUTLASS, and Triton
   across all shapes.
8. Embedding fatbins into the host executable instead of side packaging.

## 16. Review Questions

The next design review should answer:

1. What is the minimum provider-neutral local task contract?
2. Which existing WHIRL mapping or plan record can carry selected physical
   intent without introducing a premature binary format?
3. Which tiling fields are hard requirements versus preferences?
4. What conditions permit a cuBLASLt heuristic to override an Open64 preferred
   tile?
5. Which numeric and determinism modes require `REQUIRE` behavior?
6. Where is runtime algorithm-cache identity stored and invalidated?
7. Does the first multi-device test split M/N or K?
8. What exact artifact is the commit marker for cubin, provider manifest, and
   host executable publication?
9. Which CUDA and cuBLASLt versions define the first Hopper and Blackwell
   certification matrix?
10. What is the first concrete case where generated Open64 code is expected to
    outperform or provide semantics unavailable from cuBLASLt?

## 17. NVIDIA References

1. NVIDIA, [cuBLAS documentation](https://docs.nvidia.com/cuda/cublas/).
   Defines the cuBLAS, cuBLASXt, cuBLASLt, and cuBLASDx API families; BLAS
   Level 1/2/3 behavior; cuBLASLt descriptors; heuristics; capabilities;
   algorithm configuration; epilogues; batching; and matrix transforms.
2. NVIDIA, [cuBLASLt matmul algorithm configuration attributes](https://docs.nvidia.com/cuda/cublas/#cublasltmatmulalgoconfigattributes-t).
   Defines tile, stage, split-K, reduction, CTA-swizzle, inner-shape, cluster,
   and custom algorithm controls.
3. NVIDIA, [cuBLASLt algorithm checking](https://docs.nvidia.com/cuda/cublas/#cublasltmatmulalgocheck).
   Defines compatibility checking, workspace reporting, and wave-count
   reporting for a configured algorithm.
4. NVIDIA, [H200 Tensor Core GPU](https://www.nvidia.com/en-us/data-center/h200/).
   Provides the H200 product memory-capacity and bandwidth specifications used
   for capacity examples.
5. NVIDIA, [CUDA Driver API](https://docs.nvidia.com/cuda/cuda-driver-api/).
   Defines context, module, function, memory, stream, and kernel-launch services
   for the proposed native executor.
6. NVIDIA, [NCCL user guide](https://docs.nvidia.com/deeplearning/nccl/user-guide/docs/).
   Defines collectives needed when global partitioning requires cross-device
   reduction or redistribution.
7. NVIDIA, [CUDA Graphs](https://docs.nvidia.com/cuda/cuda-programming-guide/04-special-topics/cuda-graphs.html).
   Describes repeated dependency-graph execution that may be selected after the
   native stream executor is correct.

## Conclusion

cuBLASLt should be treated as a powerful, configurable local GEMM provider
inside Open64's broader AI optimization architecture. It can consume many of
the local consequences of Open64's tiling work, but it cannot replace the
semantic analysis, global partitioning, communication, memory planning,
provider comparison, or runtime orchestration that make the plan complete.

The strongest architecture is therefore not monolithic and not delegated
entirely to Python or a vendor library:

```text
Open64 WHIRL planner
  -> provider-neutral selected plan
     -> generated cubin or validated cuBLASLt configuration
        -> native runtime execution and telemetry
```

This organization preserves Open64's inspectable compiler model while making
practical use of NVIDIA's highly optimized GEMM implementation and tuning
surface.
