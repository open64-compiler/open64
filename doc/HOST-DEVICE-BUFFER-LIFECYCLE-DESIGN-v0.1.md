# Host and Device Buffer Lifecycle for AI and FHE

Version: v0.1
Date: 2026-10-25
Scope: host storage, execution storage, data transfer, and invocation ownership
Status: common design contract with a synchronous GEMM review implementation

The compiler selects the computation and its buffer requirements. The host
dispatch or service binds live inputs, and the runtime prepares the required
execution buffers before launching the selected computation. A configurable
target provider performs the actual allocation and transfer operations.

## 1. Terms and building blocks

| Term or building block | Responsibility |
| --- | --- |
| Host buffer | CPU-accessible input or output storage, resolved through a tensor handle and descriptor. |
| Execution buffer | Storage accessible to the executor: NVIDIA GPU global memory or another target's memory. |
| Buffer requirement | Compiler-selected extent, layout, alignment, access/alias rules, placement, and lifetime; no live address. |
| Host dispatch PU | Generated WHIRL procedure unit (PU) that binds arguments and invokes the selected plan. A service can bind through the callable entry instead. |
| TargetBufferService | Generic runtime service for buffer import, allocation, transfer, and release. |
| Target provider | Configured target/device implementation of those operations, validated against the plan's capabilities. |
| Runtime executor | Resolves the selected entry, establishes readiness, launches computation, and tracks completion. |

The compiler owns the buffer and execution plan. The application or service
owns borrowed inputs and caller-provided outputs. The runtime owns allocations
it creates until release or an explicit ownership transfer to the caller.

## 2. Where A, B, and C live on the host

For the reference operation `C = A x B`, A has shape `M x K`, B has shape
`K x N`, and C has shape `M x N`. Let `E` be the element size in bytes.

| Tensor | Host storage and initialization | Contiguous payload size |
| --- | --- | --- |
| A | Application, loader, preceding operation, or service supplies initialized storage and its descriptor. | `M * K * E` |
| B | Supplied like A; persistent weights may stay allocated across invocations. | `K * N * E` |
| C | Caller supplies writable storage, or the host runtime allocates it. No initial value is needed. | `M * N * E` |

Addresses are determined at runtime. Descriptors establish shape,
representation, strides, extent, and ownership. Strided or packed data require
their actual storage extents; the formulas above assume contiguous elements.
The F32 review runtime uses `calloc` and resolves payloads from tensor handles.

## 3. Allocation and movement before launch

The standalone host dispatch invokes the following sequence through the
runtime. A service entry uses the same contract with its existing buffers.

| Step | Action | Responsible building block |
| --- | --- | --- |
| 1 | Validate plan/entry, target capabilities, descriptors, and ownership. Resolve A/B/C bindings. | Host dispatch or service binding, executor, and TargetBufferService |
| 2 | Import compatible execution-resident A; otherwise allocate execution A and transfer its host contents. | TargetBufferService through the configured target provider |
| 3 | Apply the same allocation or import and transfer decision to B. | TargetBufferService through the configured target provider |
| 4 | Import execution C or allocate it. No host C upload is needed for `C = A x B`. | TargetBufferService through the configured target provider |
| 5 | Establish readiness of A/B and completion of prior C use. Bind execution handles as arguments. | Runtime executor and target provider |
| 6 | Launch the selected device PU or evaluator with the plan's entry, arguments, and configuration. | Runtime executor through the target launch provider |

The NVIDIA provider may use `cudaMalloc` and `cudaMemcpy`; these calls stay
inside that provider. The generic host dispatch calls TargetBufferService and
the executor. Other targets supply their own callbacks.

"Transfer of control" means launching or invoking the selected executor.
A GPU launch can return to the host before the kernel completes. Readiness
must therefore be enforced by completed synchronous copies or explicit queue
dependencies. The implemented buffer-service V1 uses synchronous operations.

Allocate and transfer at the planned lifetime boundary, usually an invocation
or cached binding, rather than per GPU thread block or reduction tile.
GPU global-to-shared staging happens later inside the device computation.

## 4. Completion and release

After computation completes, copy C to host memory only when the output
contract requests a host result. Otherwise return or retain its execution
buffer handle. Release runtime-owned temporary allocations after their last
use; imported buffers remain with their original owner. Cancellation and
partial failure must complete outstanding work before storage is reused or
released. An operation that reads an existing C, such as accumulation into C,
must explicitly add its input dependency and any required upload.

## 5. Adoption by the FHE compiler

For FHE, A/B denote encrypted inputs or evaluation operands and C the encrypted
result. The domain compiler supplies ciphertext representation, storage
extent, parameter/context identity, level/scale where applicable, and
evaluation-key requirements. F32 matrix formulas do not determine these sizes.

The FHE provider imports compatible objects or performs a domain-approved
representation transfer; copying a C++ object containing pointers does not
create a device ciphertext. A CPU evaluator may use host storage directly.
An accelerator provider transfers ciphertext payloads and approved
public/evaluation material. Server execution needs no encryption, decryption,
or secret-key transfer.

## 6. Current code and integration boundary

The selected host-WHIRL generated-matmul call carries the selected plan and
tensor arguments. Its fixed-shape review adapter now uses TargetBufferService
for host A/B/C staging. The generic service also supports borrowed execution
buffers; this path has host-mock tests. Standalone host-dispatch generation,
service binding, and native CUDA execution remain implementation gates.

Implementation references:

- [TargetBufferService C contract](../osprey/include/open64_target_buffer_service.h)
- [Generic buffer binding implementation](../osprey/libopen64rt/target_buffer_service.c)
- [NVIDIA review adapter](../osprey/be/vho/tests/ai_matmul_cuda/generated_runtime.cu)
- [Compilation and execution modes](OPENPY-COMPILATION-AND-EXECUTION-MODES-DESIGN-v0.1.md)
