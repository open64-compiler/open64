# Open64 AI Compilation, Packaging, And Runtime Architecture

Version v0.1 | Architecture skeleton | Draft for continuous refinement

## 1. Purpose

This document defines the end-to-end architecture that turns an AI source
model into executable work on one or more accelerators. It sits above the
individual frontend, WHIRL infrastructure, optimization, target-lowering,
runtime-provider, and validation plans. Those documents retain their detailed
contracts; this document defines how their outputs connect and who owns each
decision.

The complete path is:

```text
source model or DSL
  -> frontend ingestion
  -> binary Very High Level DSL WHIRL
  -> semantic gatekeeper and shape refinement
  -> architecture-independent optimization
  -> global execution planning
  -> local kernel planning
  -> target lowering and device-code generation
  -> AOT, late-compilation, or hybrid executable package
  -> host executable and runtime manifest
  -> serving shell or application driver
  -> Open64 runtime executor
  -> target device provider
  -> accelerator execution
  -> attributed telemetry
```

The architecture must support NVIDIA GPUs and TPUs without making either
target's compiler, runtime handle types, executable format, or serving system
part of the provider-neutral Open64 contract.

## 2. Architectural Principles

1. Binary Very High Level WHIRL is the compiler frontend boundary. It remains
   inspectable with the standard Open64 tools and does not depend on a live
   Python interpreter after frontend completion.
2. Binary WHIRL and executable device packages are different artifacts.
   WHIRL preserves compiler semantics and plans; a device package contains or
   references target-executable implementations selected after lowering.
3. Common WHIRL infrastructure defines stable IR records, construction and
   query services, mapped-image behavior, validation, and inspection. It does
   not decide fusion, placement, tiling, provider selection, package selection,
   or runtime policy.
4. VHO, WOPT, LNO, IPA, AI optimization phases, target planners, and runtime
   policy components own analysis and decisions at their declared compilation
   scopes.
5. Global execution planning, local kernel planning, device compilation, and
   runtime execution remain separate components connected by stable artifacts.
6. The runtime may choose only among compiler-certified variants. It must not
   repair an illegal tensor layout, ABI, numerical contract, launch shape, or
   communication plan.
7. AOT versus late compilation is a deployment and package policy orthogonal
   to the Open64 optimization level.
8. Provider-specific handles such as CUDA contexts, CUDA streams, cuBLASLt
   descriptors, PyTorch tensors, JAX arrays, PJRT handles, and SGLang KV-cache
   objects must not appear in provider-neutral WHIRL or package records.
9. Frontend, middle-end, backend, packager, runtime, provider, and serving
   failures must remain distinguishable through stable diagnostics.
10. Compatibility, deterministic selection, atomic artifact publication, and
    reviewable evidence are part of correctness rather than packaging details.

## 3. Component Stack And Ownership

| Component | Scope | Primary responsibility | Reviewable product |
| --- | --- | --- | --- |
| Source frontend | One source model and reachable definitions | Capture source semantics, seed tensor facts, source positions, imports, calls, and explicit attributes | Binary Very High Level DSL WHIRL plus external tensor payloads |
| Common WHIRL infrastructure | Stable compiler representation | Define logical operators, TensorDescriptorIR, plans, package descriptors, mapped-image records, verifiers, and printers | Compatible `.B` image and `ir_b2a -st -src` evidence |
| VHO and AI optimizer | Active PU by default; cross-PU only under `-ipa` | Shape refinement, canonicalization, fusion, layout, placement, communication, memory, tiling, pipeline, and variant planning | `OptimizationPlanIR`, `GlobalExecutionPlanIR`, and `KernelInvocationPlanIR` |
| Global execution planner | Active execution graph | Assign devices, communication, dependencies, reductions, synchronization, buffer lifetime, and kernel-dispatch order | Provider-neutral execution DAG |
| Local kernel planner | One semantic kernel invocation or fused REGION | Select local layout, residency, tiling, fetch, pipeline, numerical mode, provider, implementation, workspace, and fallback | Certified local variants and package requirements |
| Target backend | One selected target implementation | Lower kernel WHIRL and produce target portable or native device code | PTX, target-native code, provider configuration, or another reviewed target input |
| Compiler driver | Complete invocation | Propagate options, order host/device phases, invoke external target tools, link host code, and retain evidence | Phase logs, objects, executables, and package inputs |
| Package publisher | One coherent artifact family | Construct manifests, bind plans to images, calculate checksums, and publish atomically | Executable package and runtime manifest |
| Runtime executor | One certified global plan | Materialize buffers, transfers, dependencies, collectives, reductions, variant choices, dispatch, and telemetry | Results and execution trace |
| Device provider | One target runtime | Discover devices, validate capabilities, load or late-compile packages, launch kernels, and expose target telemetry | Loaded executable identity and provider diagnostics |
| Serving adapter | Online request population | Tokenization, admission, batching, KV-cache policy, sampling, cancellation, and service API | Runtime batches and service-level telemetry |

No component inherits another component's authority merely because both are
linked into one process. In particular, a serving system does not define
WHIRL semantics, and a runtime provider does not become an optimizer.

## 4. End-To-End Compiler Flow

### 4.1 Frontend ingestion

The frontend captures source-observed facts and creates first-class logical DSL
operators through opaque native builder APIs. It records sample-input and
parameter shapes, source positions, reachable PUs, explicit attributes,
external data references, and source-language provenance. It does not perform
target scheduling or reproduce compiler-owned graph-wide shape propagation.

The frontend publishes a binary WHIRL artifact before backend execution. The
artifact must reopen in a separate process and pass the appropriate gatekeeper.

### 4.2 Semantic preparation

At the beginning of each backend PU lifetime, Open64 performs mandatory
validation and direct semantic preparation:

```text
read mapped binary WHIRL
  -> select active PU and local symbol table
  -> DSL gatekeeper
  -> tensor shape propagation and immutable type refinement
  -> canonicalization and PREOPT preparation when enabled
```

This work follows normal Open64 compilation scope. Cross-PU inference or
transformation occurs only under explicit IPA ownership.

### 4.3 Architecture-independent optimization

The AI-P0 through AI-P10 pipeline constructs and selects legal plans under the
requested optimization level. The optimizer records semantic and physical
decisions in provider-neutral plans. It does not load device modules or hold
runtime provider objects.

AI-P9 binds each local invocation to a generated-kernel requirement or a
provider-library configuration. AI-P10 defines certified variants, guards,
fallback order, and selection policy. AI-P11 consumes attributed measurements
after execution; it does not give the runtime permission to invent plans.

### 4.4 Target lowering

The selected local plan is lowered to a provider implementation. A generated
kernel may pass through target WHIRL, target instruction selection, and a
portable device representation before native device compilation. A provider
library binding may instead produce a validated configuration and parameter
contract without producing compiler-owned native code.

For NVIDIA generated kernels, the expected initial path is:

```text
selected kernel plan
  -> kernel WHIRL
  -> NVISA lowering
  -> PTX
  -> optional ptxas AOT compilation
  -> cubin or multi-image fatbin
```

For TPU, the corresponding provider path may consume a reviewed lowering
artifact and produce a topology- and shape-specific executable through a PJRT
or equivalent provider boundary. The generic architecture does not require the
NVIDIA and TPU portable inputs to share a physical format.

### 4.5 Package publication

The publisher combines compiler plans, target images or provider
configurations, ABI records, manifests, and checksums into one coherent
artifact family. The package may remain external to the host executable. A
later embedding choice must not change logical WHIRL semantics or the stable
device-kernel ABI.

Final package publication is atomic. A failed compiler, target tool, package
validation, or checksum step must not leave an apparently valid final package.

### 4.6 Runtime execution

The runtime loads a `GlobalExecutionPlanIR`, resolves its referenced local
plans and packages, binds live buffers, evaluates certified guards, and asks
the selected device provider to load and execute each implementation. Runtime
measurements remain attributable to the plan, package, provider, target,
device, and selected variant.

## 5. Compilation Scope And Optimization Levels

| Mode | Maximum optimization scope | End-to-end behavior |
| --- | --- | --- |
| `-O0` | Mandatory legality and straightforward lowering | Validate, refine required shapes, select baseline implementations, package them, and execute without optional optimization |
| `-O1` | Basic block | Add local canonicalization and low-risk cleanup without whole-PU plan search |
| `-O2` | One PU and CFG | Enable PU-level semantic graph optimization and plan selection |
| `-O3` | Canonical loops and one-PU target plan | Enable memory hierarchy optimization, parallelization, tiling, async movement, and guarded target variants |
| `-ipa` | Multiple PUs | Explicitly enable interprocedural summaries and reviewed cross-PU refinement or transformation |

AOT and late compilation do not alter these scopes. For example, `-O0` may
produce an AOT cubin or a portable PTX fallback, and `-O3` may use either AOT
or hybrid packaging. The package mode changes when target-native code becomes
available, not which semantic optimization is legal.

## 6. Authoritative Artifacts

| Artifact | Producer | Consumer | Required identity |
| --- | --- | --- | --- |
| Binary DSL WHIRL `.B` | Frontend or compiler checkpoint | Gatekeeper, VHO, optimizer, backend | WHIRL/image revisions, source identity, managed-table integrity |
| External tensor payload | Frontend or transformation | Runtime or later compiler phase | Exact path/key/range/type/layout/checksum contract |
| `GlobalExecutionPlanIR` | Global planner | Runtime executor | Stable plan ID, target constraints, referenced kernel-plan IDs |
| `KernelInvocationPlanIR` | Local planner | Backend, packager, runtime | Stable plan ID, ABI, layouts, workspace, numerical contract, fallback |
| Portable device image | Target backend | Device provider or AOT tool | Provider, code version, minimum capability, checksum |
| Native device image | AOT compiler or provider compiler | Device provider | Exact target, compiler identity, options, ABI, checksum |
| Provider configuration | Local planner/provider adapter | Runtime provider | Provider/version, validated descriptors, algorithm/configuration identity |
| Executable package manifest | Package publisher | Runtime executor and provider | Schema, package ID, image order, capabilities, checksums, ABI |
| Host executable/runtime manifest | Driver and linker | Application or serving shell | Global plan, packages, runtime/provider versions |
| Execution trace and telemetry | Runtime and providers | Review, profiler, AI-P11 | Plan, package, image, target, device, driver, variant, measurement scope |

Binary WHIRL continues to use Open64's mapped-image and ELF framework. Device
package construction is a later compiler-output activity and does not redefine
the WHIRL file mechanism.

## 7. Kernel Package Contract

`KernelPackageDescriptorIR` is the provider-neutral package contract. It must
describe at least:

- package schema and device-kernel ABI versions;
- logical operator, fused REGION, and selected local-plan identity;
- provider and target family;
- kernel entry symbol or provider operation identity;
- argument order, size, address space, alignment, and result convention;
- input/output TensorDescriptorIR and physical layout requirements;
- workspace, launch, synchronization, and numerical contracts;
- required target capabilities and topology properties;
- native, portable, container, or provider-configuration images;
- deterministic image and fallback order;
- image sizes, checksums, compiler identities, and build options; and
- runtime guards and stable failure actions.

The generic code-kind model is:

```text
portable compilation input
native executable
multi-image container
provider configuration
```

Provider-specific records refine these categories. NVIDIA may use PTX, cubin,
fatbin, or a cuBLASLt configuration. A TPU provider may use a provider compile
input, a topology- and shape-specific executable, or a library configuration.

## 8. AOT, Late, And Hybrid Compilation

### 8.1 AOT mode

AOT mode produces named target-native images before package publication. It is
the required mode for reproducible target-performance certification because
the exact native image, compiler version, options, and checksum are known.

An AOT package may contain more than one native variant. Selection still
requires a deterministic target and capability match at runtime.

### 8.2 Late-compilation mode

Late mode publishes a provider-supported portable input and invokes the target
provider's compiler after discovering the actual device, topology, shape
bucket, or runtime environment. The provider owns the target compiler call;
the generic runtime owns policy, package identity, diagnostics, and telemetry.

Late compilation must record:

- portable-image checksum and format version;
- target device and topology identity;
- provider compiler or driver version;
- complete compilation options;
- information and error logs;
- compilation latency;
- resulting executable identity when the provider exposes it; and
- cache policy and cache-key inputs.

Late compilation may change startup latency and generated performance. It must
not be silently substituted when the selected plan requires a certified AOT
image.

### 8.3 Hybrid mode

Hybrid mode packages native images for reviewed targets plus a portable
fallback for compatible future targets. This is the recommended deployment
form when forward compatibility is required.

The selection order is:

```text
exact certified native image
  -> explicitly allowed compatible native image
  -> explicitly allowed portable fallback and late compilation
  -> reviewed provider-library fallback
  -> stable failure
```

### 8.4 Explicit runtime linking

Runtime device linking is a distinct optional capability. It is needed only
when a real use case requires multiple device objects, provider libraries,
device LTO input, or runtime specialization across separately compiled units.
It is not part of the first whole-kernel portable-image fallback.

## 9. Runtime Executor Contract

The runtime executor owns provider-neutral execution:

```text
load and validate manifest
  -> select certified global and local variants
  -> allocate or import buffers
  -> schedule transfers and communication
  -> establish dependency and completion tokens
  -> request provider package load or late compilation
  -> bind arguments
  -> launch kernels or provider operations
  -> execute collectives and reductions
  -> retrieve or expose results
  -> report attributed telemetry
```

The runtime requires abstract buffers, queues, completion events, collectives,
and executables. It must not define these abstractions in CUDA-specific terms.
For example, the generic contract carries an execution queue and completion
token rather than a public `cudaStream_t` and `cudaEvent_t`.

The runtime caches loaded executables by at least package checksum, selected
image, provider, device, context or execution environment, and entry identity.
Module loading or late compilation must not occur inside each measured kernel
iteration.

## 10. Device Provider Contract

Every device provider must support a reviewed subset of these operations:

1. Enumerate devices and query capabilities.
2. Describe topology and communication paths.
3. Create or attach to an execution environment.
4. Allocate, import, export, and release opaque buffers.
5. Perform asynchronous host/device and device/device transfers.
6. Load a compatible native image.
7. Compile a supported portable input when policy permits.
8. Resolve an entry or provider operation.
9. Validate launch, workspace, and numerical requirements.
10. Launch work with abstract dependencies and completion tokens.
11. Execute or bind collectives.
12. Return target-specific diagnostics through stable Open64 categories.
13. Report load, compilation, transfer, launch, kernel, and synchronization
    timing separately.
14. Unload packages and release provider resources.

Provider runtime code belongs in a runtime library or plugin. It must not add
target runtime dependencies to `be.so` or shared consumers such as
`lw_inline`.

## 11. NVIDIA Provider Mapping

The initial NVIDIA provider maps the generic contract as follows:

| Generic concept | NVIDIA realization |
| --- | --- |
| Portable device input | PTX |
| Native executable | Cubin |
| Multi-image container | Fatbin |
| AOT compiler | `ptxas` and reviewed device-link/fatbinary tools |
| Late compiler | CUDA Driver PTX JIT |
| Optional runtime linker | `nvJitLink` or reviewed Driver API linking |
| Module loader | CUDA Driver Module or Library Management API |
| Execution queue | CUDA stream hidden behind provider handle |
| Completion token | CUDA event or graph dependency hidden behind provider handle |
| Collectives | NCCL or another reviewed NVIDIA-capable provider |
| Provider library | cuBLASLt, CUTLASS-based package, DeepGEMM, or another reviewed binding |

When no compatible cubin exists, the provider may request CUDA Driver JIT only
if the package contains compatible PTX and the selected policy permits late
compilation. A cubin-only package without a compatible image fails cleanly.

The NVIDIA integration details, including module loading, argument marshaling,
launch, package naming, retained logs, and performance certification, remain in
`NVIDIA-DSL-RUNTIME-INTEGRATION-PLAN.md`.

## 12. TPU Provider Requirements

TPU support is a design input to the generic interface, not a later CUDA
emulation layer. A TPU provider must be able to key executable selection or
late compilation by:

- TPU generation and supported operations;
- slice and device-mesh topology;
- ICI and inter-host communication properties;
- tensor sharding and axis-to-mesh mapping;
- static shape or certified shape bucket;
- dtype and numerical contract;
- compiler/plugin identity; and
- collective and memory requirements.

The TPU provider may use PJRT or another reviewed provider boundary. Open64
does not adopt the provider's compiler IR as its middle-end IR merely because
the provider accepts that form. Open64 WHIRL, optimization plans, and package
contracts remain authoritative.

Runtime variants should support static shape buckets so a serving scheduler
can select an existing TPU executable before requesting late compilation.
Topology-specific collectives and executable caching are provider duties, but
their logical communication and dependency requirements originate in the
Open64 global execution plan.

## 13. Serving And Application Boundary

The first serving integration may use SGLang as a replaceable outer shell:

```text
client
  -> SGLang request scheduling, batching, KV policy, and sampling
  -> Open64 serving adapter
  -> Open64 runtime executor
  -> NVIDIA or TPU device provider
```

SGLang is not the Open64 compiler or runtime ABI. The adapter exchanges stable
batch, tensor, KV-cache interoperability, cancellation, and result contracts.
Python objects and SGLang-private scheduler structures do not enter WHIRL or
the provider-neutral runtime.

NVIDIA Dynamo or Triton may later replace or surround the serving shell without
changing Open64 kernel packages. Likewise, a TPU-oriented serving shell may use
the same Open64 serving and device-provider contracts with a different target
provider.

The compiler/runtime advantage comes from combining online scheduler state
with statically certified compiler variants. The scheduler supplies live batch,
shape, cache, and load state; it does not invent kernel layouts or schedules.

## 14. Compatibility And Versioning

The architecture has separate compatibility contracts:

1. Binary WHIRL and mapped-image compatibility.
2. Logical DSL operator and TensorDescriptorIR compatibility.
3. Global and local plan schema compatibility.
4. Device-kernel ABI compatibility.
5. Executable-package schema compatibility.
6. Provider API compatibility.
7. Serving-adapter compatibility.
8. Telemetry-schema compatibility.

A change in one contract does not automatically require changes in all others.
For example, adding a Blackwell cubin to a package must not change binary DSL
WHIRL. Changing a kernel argument ABI requires a new ABI identity and matching
package entry, but does not justify silently reinterpreting an existing entry.

Old readers and runtimes must either consume a new optional capability safely
or fail closed with a stable diagnostic. Unknown package images, target
features, guards, and numerical modes must never be guessed.

## 15. Options And Driver Behavior

The driver passes the complete option set through the compiler pipeline. Each
phase consumes its own options and silently ignores unrelated options.

The final spelling remains subject to driver-option review, but the option
model must distinguish:

- optimization level and IPA scope;
- target provider and target architecture set;
- AOT, late, or hybrid package mode;
- portable fallback generation;
- permission or prohibition of late compilation;
- AOT and late compiler options;
- package output and installation location;
- retained artifact level; and
- runtime tracing and telemetry controls.

The selected package mode and target set must appear in retained driver and
package evidence. No environment-dependent fallback may remain invisible.

## 16. Diagnostics And Review Artifacts

Stable diagnostic categories must distinguish:

- malformed or incompatible WHIRL;
- optimizer or plan failure;
- target lowering failure;
- AOT device-compiler failure;
- package validation or publication failure;
- no compatible native image;
- missing or prohibited portable fallback;
- late compiler unavailable or rejected input;
- provider ABI or entry mismatch;
- launch-resource violation;
- runtime transfer, collective, launch, or synchronization failure; and
- serving-adapter failure.

Validation retains, as applicable:

```text
source fixture
binary WHIRL
ir_b2a -st -src output
phase traces
global and local plan dumps
portable device code
native device images
device-compiler and JIT logs
package manifest and checksums
host link command and executable identity
runtime selection trace
provider launch trace
numerical outputs
performance and telemetry reports
```

Artifacts remain grouped by test, target, and stage. Failed runs retain useful
logs but do not publish final package or executable names.

## 17. Validation Matrix

The generic certification matrix includes:

1. Baseline `-O0` execution through one provider.
2. Exact AOT native-image selection.
3. Multi-target native-image selection.
4. Hybrid package selection of native code when present.
5. Permitted portable fallback and late compilation.
6. Late-compilation prohibition and stable failure.
7. Missing, incompatible, corrupted, or checksum-invalid image rejection.
8. Kernel ABI, layout, numerical, workspace, and launch validation.
9. AOT and late-compiled numerical-contract equivalence.
10. Separate load/JIT, transfer, kernel, collective, and end-to-end timing.
11. Runtime cache reuse without compilation inside measured iterations.
12. Atomic package publication and cleanup on induced failure.
13. Serving-adapter cancellation, batching, and result correctness.
14. Provider conformance for NVIDIA and TPU.
15. Reopening compiler artifacts through standard Open64 inspection tools.

Performance certification names the exact native executable and target. A
late-compiled result may have its own separately identified measurements but
does not inherit an AOT performance claim.

## 18. Staged Bring-Up

### Stage AR-0: Contract publication

Publish the provider-neutral package, runtime, provider, serving, diagnostic,
and artifact contracts. Reconcile them with the existing AI-P9 and AI-P10
records without changing binary WHIRL layout prematurely.

### Stage AR-1: NVIDIA AOT vertical slice

Compile one `common.matmul` kernel to PTX and an H200-targeted cubin. Publish a
package manifest, load it through the NVIDIA provider, execute it, and retain
correctness and performance evidence.

### Stage AR-2: NVIDIA hybrid package

Add a reviewed PTX fallback, force both native and JIT paths, certify stable
selection and diagnostics, and separate compilation/load time from kernel
time.

### Stage AR-3: Provider-library alternatives

Bind the same provider-neutral GEMM contract to cuBLASLt, CUTLASS, and
DeepGEMM implementations. Preserve exact configuration, package, fallback, and
telemetry identity.

### Stage AR-4: Serving adapter

Connect SGLang through the stable Open64 serving adapter. Begin with fixed
prefill/decode shape buckets and explicit KV-cache ownership. Do not expose
SGLang-private or CUDA-private objects through the Open64 ABI.

### Stage AR-5: TPU provider prototype

Use the same package/runtime contracts to compile or load one fixed-shape TPU
kernel or subgraph. Certify shape-bucket, topology, sharding, and executable
cache identities and record any generic interface leakage discovered by the
second provider.

### Stage AR-6: Distributed execution

Execute a global plan containing multiple devices, communication, reduction,
and dependencies. Keep online serving orchestration replaceable and preserve
compiler-owned global plan evidence.

## 19. Open Design Questions

1. Which package records belong in mapped binary WHIRL and which remain in the
   external executable manifest?
2. What is the first stable C ABI for `Open64ServingEngineV1`,
   `DeviceProviderV1`, and KV-cache interoperability?
3. Which portable input will the first TPU provider accept without making that
   format the Open64 middle-end IR?
4. How are runtime-generated native images represented when the provider does
   not expose their bytes?
5. When is explicit runtime device linking justified beyond whole-kernel load?
6. How are package signatures and deployment trust established in addition to
   integrity checksums?
7. Which telemetry is stable compiler feedback and which remains
   provider-private profiling data?
8. How are serving-level and compiler-level variants coordinated without
   allowing the serving scheduler to bypass compiler legality?

## 20. Related Documents

- `AI_compiler_optimization_design_v0.1.md` defines AI-P0 through AI-P11,
  optimization IR, candidate selection, cost, and plan construction.
- `AI-COMPILER-OPTIMIZATION-IMPLEMENTATION-PLAN.md` stages the optimizer and
  provider-neutral plan implementation.
- `Open64_Python_FE_Plan.md` defines Python source ingestion and the binary
  WHIRL frontend boundary.
- `WHIRL-DSL-INFRASTRUCTURE.md` defines native DSL WHIRL representation,
  mapped-image compatibility, verification, and inspection.
- `WHIRL-DSL-TENSOR-TYPE-HANDLING.md` defines tensor type and descriptor
  management.
- `WHIRL-DSL-SHAPE-PROPAGATION-DESIGN.md` defines compiler-owned tensor shape
  inference and immutable type refinement.
- `VHO-DSL-OPTIMIZATION-PLAN.md` defines the architecture-independent VHO DSL
  optimization pipeline.
- `VHO-DSL-PARALLELIZATION-PLAN.md` defines high-level parallel planning.
- `CUDA-HOPPER-BLACKWELL-MATMUL-OPTIMIZATION-PLAN.md` owns generated NVIDIA
  kernel contents and target scheduling.
- `NVIDIA-DSL-RUNTIME-INTEGRATION-PLAN.md` owns NVIDIA AOT tools, packages,
  loading, launch, and runtime integration.
- `OPEN64-NVIDIA-GEMM-CUBLASLT-INTEGRATION-TECH-REPORT.md` records the first
  GEMM provider and generated-kernel study.
- `DSL-RUNTIME-VALIDATION-AND-BENCHMARK-HARNESS-PLAN.md` defines reusable
  correctness, performance, and regression evidence.
