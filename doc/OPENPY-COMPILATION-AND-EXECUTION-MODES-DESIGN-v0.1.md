# OpenPy Compilation and Execution Modes Design

Version: v0.1
Status: cross-domain design proposal; option spellings and most packaging/runtime paths are not implemented
Date: 2026-10-23
Revision: 2026-10-25, target-neutral device-buffer allocation and ownership contract
Scope: AI and FHE programs compiled by `openpy`; standalone application, service JIT, and performance testing
Companions: `OPEN64-AI-COMPILATION-AND-RUNTIME-ARCHITECTURE.md`, `FHE-CONSOLIDATED-IMPLEMENTATION-PLAN.md`, and `FHE-O2-ACE-RESBM-METAKERNEL-INTEGRATION-PLAN.md`

## 1. Decision and terminology

`openpy` should offer three **user-facing compilation profiles** over one
semantic compiler pipeline. The default completed driver experience should
be a runnable standalone application, like a conventional `cc ... -o a.out`
workflow. A service can instead request a callable compiled package without
a standalone host program. Performance testing is an explicit driver request
that creates a separate benchmark harness with a qualified baseline; it is
never injected into either normal application or service execution.

| Profile | Program-specific host dispatch | Product | Main consumer |
| --- | --- | --- | --- |
| Standalone application | Yes: generated host PU/entry plus a thin launcher | Linked executable and manifest; default name `a.out` when linking is implemented | Developer or batch runner |
| Service JIT | No standalone host dispatch PU or `main`; callable entry/package metadata only | Versioned compiled package or in-process handle | Long-lived inference/evaluation service |
| Performance testing | A separate generated measurement driver, not a modification of the application host PU | Benchmark executable, baseline/selected packages, and report | Compiler developer or performance CI |

| Concept | Meaning |
| --- | --- |
| **Analysis PU** | WHIRL scope containing the operation or graph region the optimizer examines. It need not become CPU code. |
| **Device PU** | Selected kernel body in WHIRL; the source of device code. |
| **Host dispatch PU** | Optional generated CPU wrapper for standalone/AOT applications. |
| **Inference service** | Owns requests, batching, buffers, dispatch, and results; it is not a compiler PU. |

The analysis PU is the optimizer's *input scope*, not a promise of a matching
host executable. A selected plan can materialize a device PU and, when the
product needs it, a plan-specific host dispatch PU. The inference service is
an external runtime system: it owns request lifecycle, batch formation, and
application-visible buffer policy. The Open64 runtime owns the resources it
allocates to execute a certified plan. Borrowed service buffers require a
versioned ownership and lifetime contract; neither side may silently free
or move the other's storage. Service **dispatch** means forwarding a chosen
request batch to model execution; Open64 **dispatch** means invoking one of
the compiler-certified entries under its guards. The two decisions must not
be conflated.

**Host PU** means a program-specific *orchestrating* WHIRL procedure that
binds inputs, evaluates certified runtime guards, invokes plan-selected work,
and returns outputs. It is distinct from a device kernel PU and from a CPU
evaluator function. In FHE, service JIT may still compile CPU evaluator code
or generated C for encrypted operations; omitting a standalone host PU does
not mean omitting all host-side code. The generic runtime/provider owns
resource setup, handles, module/context loading, launch or call mechanics,
and cleanup. It does not invent a new program-specific plan.

**Service JIT** here means invoking Open64 compilation from a service to
obtain a callable program/package, possibly keyed by certified shape or
parameter buckets. It is different from provider late compilation (for
example, PTX-to-native compilation when a device image is loaded) and from
FHE planning algorithms sometimes called JIT bootstrap placement. These
three events have separate controls, logs, caches, and timing scopes.

## 2. Invariants shared by all profiles

1. The frontend publishes binary very-high-level WHIRL as the semantic
   process boundary; the compiler reopens and verifies it. Python is a
   frontend, not a required middle-end or serving dependency.
2. The selected `OptimizationPlanIR` and domain plan are authoritative.
   Host dispatch, service entry, harness, and target emitters consume the
   same certified plan and entry ABI. No source template is a second plan.
3. `common/com` supplies domain-neutral identity, descriptors, plan/package
   records, verification, and inspection, not AI/FHE policy. VHO/LNO/WOPT/IPA
   and domain phases retain their established optimization scopes.
4. Optimization level and IPA scope are independent of profile. `-O0`
   performs mandatory legality and direct lowering; `-ipa` alone authorizes
   cross-PU analysis. Profile selection does not silently add fusion,
   tiling, bootstrap movement, or precision changes.
5. AOT/native, provider late compilation, and hybrid package policy are
   independent of profile. A standalone executable may use a late-compiled
   image; a service may load an AOT image. Both choices remain visible.
6. A published entry has a versioned argument/result ABI, exact plan ID,
   provider/target capability contract, resource limits, guards, fallback,
   and package checksum. Unknown versions or unavailable capabilities fail
   closed; no runtime repair of illegal layout, CKKS state, or numerical mode.
7. Intermediate and final artifacts publish atomically. Failure must not
   leave a partial `.B`, manifest, executable, or service cache entry with a
   valid final name. Trace and diagnostic evidence remain available.
8. Compiler estimates, correctness results, and measured performance are
   separate evidence classes. Neither a successful compile nor a CPU/mock
   run is a GPU/FHE throughput claim.

## 3. Common production pipeline and ownership

```text
source/DSL and assets
  -> frontend -> binary WHIRL .B and source map
  -> gatekeeper, shape and domain-state refinement
  -> scoped optimization and selected plan
  -> target lowering and certified callable implementation(s)
  -> package manifest, ABI, guards, fallback and checksums
  -> profile-specific product (application, service package, or harness)
  -> generic runtime -> domain/provider adapter -> execution
  -> attributed trace and feedback
```

The **compiler driver** owns option validation, phase order, output product,
linking, package publication, and retention. The **domain compiler** owns
semantic legality and numerical/security policy. The **target backend** owns
instruction/code realization. The **runtime executor** owns certified
variant selection, its own allocations, borrowed-buffer leases, dependencies,
and execution lifetime. Its `TargetBufferService` owns the target-neutral
buffer contract; the selected **provider adapter** implements allocation,
transfer, and release with target-specific facilities such as CUDA or ACE
ANT. The **application or service** owns user requests and input/output
policy. The **harness** owns experimental controls and measurements only.

One package record must join the semantic entry, selected plan, compiled
entry symbol, ABI version, target/provider requirements, source and compiler
identity, native/portable/configuration images, fallback order, and checksum.
The host PU, service loader, and harness refer to this record by identity;
none reconstructs tile factors, CKKS parameters, or launch shapes from an
unrelated file. The package can remain external to an executable; embedding
is a later distribution choice, not a semantic distinction.

### 3.1 Minimal callable contract and lifetimes

The concrete C ABI needs a separate review, but its semantic operations are
fixed across profiles:

| Operation | Inputs | Output and ownership |
| --- | --- | --- |
| `compile` | Source or `.B` identity, entry, target, optimization/domain policy, bucket constraints | Immutable, checked package and entry descriptor; compiler owns temporary state only |
| `load` | Package, provider environment, capability/public-context identity | Runtime-owned loaded-entry handle; may perform separately reported provider late compilation |
| `invoke` | Loaded entry, typed input handles, runtime context, queue/dependencies | Result/completion or stable failure; caller retains input ownership, runtime owns internal buffers |
| `release` | Loaded entry and completed executions | Provider resources returned only after in-flight work is finished |

The entry descriptor specifies argument order and types, result ownership,
memory/address-space and alignment rules, shape or parameter guards, package
and plan IDs, fallback order, and ABI version. Persist **identities and
contracts**, not process-local pointers. A package does not contain a CUDA
stream, ACE evaluator object, secret key, raw context pointer, or service
request. The runtime validates the bound live objects against the entry on
each load/invocation. A standalone host PU calls this interface after its
launcher binds inputs; service JIT calls the same interface from the service.
The harness invokes two separately loaded entries through it.

#### TargetBufferService: allocation and binding

`TargetBufferService` is the generic runtime building block for device or
accelerator-visible buffers. The compiler emits a **buffer requirement**, not
an allocation call or a process-local pointer: tensor role (input, output,
scratch), bytes or guarded shape expression, dtype/layout, target memory tier,
alignment, access/alias permissions, lifetime, and whether borrowing is
legal. The selected plan and entry descriptor carry those requirements. At
invocation, the executor binds each requirement to a validated borrowed
buffer or asks `TargetBufferService` to acquire a runtime-owned buffer. No
compiler-generated PU contains `cudaMalloc` or another provider allocator.

| Operation | Generic contract | Ownership rule |
| --- | --- | --- |
| `import` | Validate a caller-owned buffer handle against target, tier, size, alignment, layout, access, and completion dependencies | Borrow only; never free or silently relocate it |
| `allocate` | Acquire a target-visible buffer for a plan-declared result, staging object, or scratch requirement | Runtime owns it until release or an explicit ownership transfer to the caller |
| `transfer` | Move or make data available between declared tiers when the plan and caller policy require it | Record direction, bytes, dependencies, and completion; no implicit host round trip |
| `release` | Return a runtime-owned buffer after its final dependent operation completes | Reject release of borrowed storage; preserve results whose ownership was transferred to the caller |

The executor selects a provider implementation from the package's target
and capability contract plus the live device/runtime configuration. Its
configuration includes target kind and device identity, available memory
tiers, allocator/stream policy, capacity and alignment limits, and provider
ABI version. The provider must validate these requirements before exposing
a buffer handle or launch pointer. The NVIDIA implementation may use CUDA
allocation and copy APIs (for example, `cudaMalloc` and `cudaMemcpy`); those
names belong only in the CUDA provider, not in common WHIRL, the host PU, or
the cross-domain ABI. A CPU or FHE provider supplies its own implementation
of the same contract. Changing providers cannot change the selected tensor
layout, numerical/security semantics, or ownership rules.

For standalone execution, the launcher binds application inputs, and the
generated host dispatch PU passes plan-bound buffer requirements and handles
to this service. Host-resident A/B may require provider allocations and
transfers; a device-resident input can be imported without a redundant copy.
The output C is either bound to validated caller storage or allocated by the
runtime and returned with explicit ownership. For service JIT, the service
usually imports its existing A/B, output, and KV buffers; only plan-declared
scratch or an explicitly requested output may be runtime-allocated. Failure,
cancellation, fallback, and reuse all wait for the recorded completion event
before a borrowed or owned buffer is released or rebound.

For the first synchronous F32 GEMM implementation, the compiler-selected
generated-matmul call in the host WHIRL PU carries the plan identity, tensor
descriptor, and arguments. The runtime derives the checked A/B/C byte counts
from those dimensions and routes the following sequence through
`TargetBufferService`:

```text
validate selected plan, shape, provider, and A/B/C binding descriptors
import device-resident A/B or allocate target A/B and transfer host inputs
import device-resident C or allocate target C
invoke the selected device PU with those target buffer handles
on completion, transfer C only when the caller requested host output
release runtime-owned buffers; leave imported buffers with their owner
```

This sequence is one invocation-level host action, not a CUDA allocation
inside each CTA, K tile, or device PU. P4/P6 establish legal placement and
residency, P7 fixes the GMEM access map, and P8 describes movement within the
device hierarchy. The host-to-device transfer requirement and its lifetime
are finalized with the selected execution plan before host-call lowering;
the provider chooses the concrete allocation/copy implementation at runtime.
The first reviewed path may be synchronous, but completion and ownership must
be explicit so an asynchronous provider can be added without changing WHIRL.
The current fixed-shape CUDA adapter is a validation fixture, not the final
standalone host-PU generator or inference-service buffer policy.

### 3.2 Building blocks to build or adopt

| Building block | Owner and decision | Build or adopt | First deliverable / gate |
| --- | --- | --- | --- |
| Analysis PU and semantic verifier | Open64 frontend, WHIRL gatekeeper, VHO/LNO/WOPT/IPA | Adopt existing WHIRL scopes; extend domain facts | Reopened `.B`/`.T`, source and value identity, domain legality |
| Selected plan and candidate trace | Open64 optimizer and AI/FHE phases | Extend current plan/trace work | Stable plan ID, legal alternatives, cost/selection and rewrite evidence |
| Device PU and target emitter | Open64 kernel planner/backend | Extend selected GEMM WHIRL slice; build target lowering | One selected device image joined to its plan and verified ABI |
| Host dispatch PU generator | Open64 compiler driver/lowering | Build for standalone product | Plan-derived runtime calls; no independent kernel-selection policy |
| Package publisher and callable-entry ABI | Open64 driver/common contract | Build | Atomic manifest, checksums, guards, entry and fallback identity |
| Runtime executor | Open64 runtime library | Build | Load/invoke/release, certified variant selection, dependencies, typed buffers |
| TargetBufferService | Open64 runtime contract; selected provider supplies the implementation | Build target-neutral allocate/import/transfer/release interface | Plan-declared buffer requirements, provider configuration, ownership/lifetime checks, and no unrequested host copy |
| NVIDIA launch and buffer provider | CUDA adapter behind Open64 runtime | Build adapter; adopt CUDA driver/runtime facilities | Device capability check, module load, CUDA buffer/stream/event operations, launch and errors |
| FHE execution provider | FHE runtime integration | Adopt ACE `FHErt_common`/`FHErt_ant` through a reviewed adapter | Public C ABI projection and secretless server lifecycle proof |
| AI inference service | AI serving integration | Adopt SGLang for request orchestration; build an Open64 adapter | One SGLang-managed batch invokes a certified Open64 entry without copying through host memory by default |
| FHE service shell | FHE deployment owner | Build or adopt separately; do not infer SGLang suitability | Ciphertext/public-context requests, key-domain isolation, encrypted results |
| Service compilation and cache | Open64 service-facing compiler API | Build | Content-addressed compile/load, concurrent-key deduplication, rollback and eviction |
| Performance harness and telemetry | Open64 driver plus domain test owner | Build; reuse existing `.t` report convention | Opt-in paired baseline/selected evidence and explicit no-hardware result |

The existing selected synchronous GEMM device-WHIRL sidecar and generated
provider-call ABI are partial compiler evidence, not a complete CUDA runtime,
host dispatch generator, service adapter, or `a.out` pipeline. The table is
an implementation inventory, not a claim that every row already exists.

## 4. Profile A: standalone application

**Intent.** A developer compiles and launches the result directly. Once
driver linking is complete, the ordinary form should be conceptually:

```text
openpy -O3 model.py -o a.out
./a.out <application inputs>
```

The proposed driver generates a program-specific host dispatch PU/entry
from the selected execution plan and a small launcher `main`. The dispatch
contains only plan-derived order, guards, argument binding, certified
fallback choice, and calls into the stable runtime interface. The launcher
parses the declared input/output contract and invokes that entry. Device
kernels remain separate device PUs/packages. Do not put CUDA context setup,
FHE provider internals, sampling policy, or benchmark iterations in the
generated dispatch PU.

For the first tiled AI GEMM, the selected plan is the common parent of both
executable PUs:

```text
compiler-selected plan
  +-- device PU: tiled GEMM kernel
  +-- host PU: plan-specific dispatch
        -> TargetBufferService and runtime load/launch APIs
        -> selected device entry
```

The host PU carries the selected plan and package identity, binds A/B/C and
the verified launch contract, and calls the runtime APIs. `TargetBufferService`
resolves declared allocation or import requirements through the configured
target provider; the executor resolves the selected device entry, launch,
and completion. The host PU does not reproduce the GEMM tile loop or choose a
new kernel; the device PU contains the selected tile computation. Both PUs
must be traceable to the same plan ID, and the package manifest must verify
their entry/ABI join before linking or loading. A source-only `.cu` sidecar
without the selected device PU does not satisfy this completed contract.

An application may contain several host PUs where source semantics require
them; `-ipa` may summarize them but the driver may not assume a single PU.
The linker must resolve the selected runtime/provider libraries and record
their identities. A load or capability failure returns a stable diagnostic
and nonzero exit rather than silently switching to an unrecorded kernel.

For AI, the host dispatch binds tensor inputs, selected graph and local
kernel plans, device packages, and output handles. A generated matmul
provider call is program-specific host code; the CUDA launch adapter is a
reusable provider implementation. For FHE, a standalone **server**
application accepts ciphertext and public/evaluation material and returns
ciphertext. Encryption, secret-key generation, and decryption belong to a
separate client or provisioner, not the server launcher. A local all-in-one
debug fixture may exist but is not a production security acceptance test.

**Acceptance.** Reopen the binary WHIRL, verify selected host/device or FHE
evaluator entries, link and run the executable, exercise input/guard/fallback
and failure paths, retain source-to-entry and plan-to-package joins, and
compare outputs against the declared correctness oracle. For FHE, prove
the server binary and process have no secret-key/decryptor dependency.

## 5. Profile B: service JIT

**Intent.** A long-lived service obtains a callable plan for a shape,
target, or FHE-parameter bucket without generating a standalone `main` or
program-specific application driver. The service supplies the request
environment and calls the runtime executor through a stable, versioned
service entry. The compiler returns a verified package/entry descriptor;
it does not become the service scheduler.

The selected device or evaluator entry and its plan/package identity are
unchanged from the standalone contract. What changes is the caller: the
service binds live requests through the callable interface rather than
executing a compiler-generated standalone host dispatch PU.

The service-facing sequence is:

```text
compile request (source/WHIRL ID, bucket, target, policy)
  -> validate and compile or cache hit
  -> atomically publish certified package and callable entry
  -> service binds live inputs and public/runtime context
  -> generic executor selects a certified variant and executes
  -> service receives outputs, status, and attributed telemetry
```

The compile request must specify or resolve exact source/WHIRL hash,
frontend/compiler schema, plan and numerical policy, target/provider,
bucket guards, optimization level, package policy, and permitted fallback.
The service cache key also includes code/ABI versions and relevant provider
and target versions. A cache hit is accepted only after manifest, checksum,
guard, and capability validation. Concurrent requests for the same key
should share one compile/publication operation; cancellation cannot expose
an incomplete cache entry. Eviction waits for all live executions and
releases provider resources through the runtime.

JIT compilation latency, provider late-compilation latency, package load,
transfer, execution, and service queueing are separate metrics. The service
can choose among certified variants using live batch/load state; it cannot
create a new layout, precision mode, CKKS chain, or unverified host wrapper.
Compile-time failures may fall back only to an explicitly certified package
with a compatible contract.

For AI, SGLang owns admission, batching, KV-cache policy, and sampling; its
adapter passes batch, sequence, KV-cache, and SLO state across the Open64
entry contract. For FHE, the service
supplies a resolved **public** parameter/context fingerprint and evaluation
key capability, never a secret key. The plan's ring/chain/layout,
bootstrap, and key requirements are certified before materialization.
The selected ACE `FHErt_ant` path currently needs a reviewed server-only,
reentrant context lifecycle before a multi-tenant service-JIT claim; an
in-process compiled package does not erase that gate.

### 5.1 SGLang as the AI inference service

Adopt SGLang as the first AI **service framework**, not as a compiler PU or
the Open64 runtime ABI. Its documented server/engine and scheduler organize
requests, batches, and model-forward dispatch. Open64 contributes compiled
entries and a provider-neutral execution contract. The first integration
should be an explicit adapter at a reviewed model-runner or operator-call
boundary; the exact SGLang extension point and version must be pinned by a
small upstream-source prototype, not assumed to be a stable public plugin.

```text
SGLang request API / tokenizer
  -> SGLang scheduler, batch and KV-cache policy
  -> reviewed SGLang model-runner/operator call site
  -> Open64-SGLang adapter: tensors, bucket, stream and lifetime contract
  -> Open64 compile/cache or certified package lookup
  -> Open64 runtime executor -> NVIDIA provider -> selected device PU
  -> completion and output handles -> SGLang sampling/results
```

The adapter carries batch shape, dtype/layout, prefill/decode phase, tensor
and KV-block descriptors, selected stream/dependency tokens, and package
identity. SGLang keeps ownership of request queues, sampling, and the KV
cache allocator/policy; Open64 imports those buffers through
`TargetBufferService` under explicit lifetime and alias rules and may
allocate plan-declared scratch through the selected provider.
The bridge must document pointer/address-space validity, stream ordering,
completion before reuse or eviction, cancellation, and error propagation.
No default host round trip or duplicate KV-cache manager is acceptable.

The service may choose a *certified* variant using live batch, sequence and
cache-pressure state, while Open64 guards and package metadata define which
variants are legal. SGLang cannot rewrite a kernel plan; Open64 cannot
silently change scheduling or KV ownership. A compile miss may run a named
compatible certified fallback only if the service policy permits it; record
compile, provider late-JIT, load, execution, and queue time separately.
Begin with one `common.matmul` call and correctness/stream-lifetime tests,
then widen to graph and distributed execution. This adapter is proposed,
not implemented, and SGLang is not selected as the FHE service framework.

**Acceptance.** Exercise compile/miss/hit, concurrent same-key requests,
version mismatch, guarded bucket miss, failed compile rollback, cancellation,
eviction with in-flight calls, and exact runtime attribution. FHE also
requires no-secret server execution and context-isolation evidence.

## 6. Profile C: performance testing

**Intent.** Produce a separate executable experiment to compare the selected
optimization against an independently certified baseline. This is a driver
option, not default compilation behavior and not a substitute for ordinary
correctness testing. The provisional existing design spelling is
`-perf-harness` (not registered). It requires standalone delivery for the
first implementation; a service may benchmark the same package through a
separate offline invocation without embedding timing loops in production.

The harness records the selected and baseline **plan and package IDs** before
running. Comparable means same target, shape/bucket, dtype and numerical
contract, physical input layout, input distribution, memory-residency
policy, provider versions, and measurement scope. The baseline cannot be a
CPU fallback or validation-only CUDA control when claiming GPU optimization
speedup. In FHE, it must share the same security level, ring/chain,
precision/error acceptance, key capabilities, encrypted inputs, provider
qualification, and client/server boundary. An accepted O0/Fhelipe/ANT
baseline must be named by its pinned implementation and manifest, not
inferred from `-O0` alone.

Run correctness first and keep the result independent of timing. Warm both
implementations, use paired repeated measurements with fixed workload and
explicit order/randomization policy, and retain samples rather than only a
ratio. Report compile, provider late-JIT, load, setup, transfer,
kernel/evaluator, synchronization, and end-to-end scopes separately.
Exclude setup and JIT from a steady-state execution interval, but report
their cold-start cost. Profiler-counter runs have their own scope because
instrumentation can change timing. Any speedup names the baseline,
selected image, target, measurement scope, statistic, sample count, and
uncertainty. Missing baseline, hardware, or required key setup yields
`not_measured` or `not_comparable`, never a fabricated improvement.

The FHE harness is explicitly **split**: the server/evaluator receives
only ciphertext and public/evaluation keys; a separate client/test process
may hold ephemeral secret material to encrypt inputs and decrypt outputs
for correctness. No secret, raw private input, or decrypted result enters
compiler traces, generated server C, package manifests, or retained
performance artifacts. Server compute time and optional client-to-client
end-to-end latency are reported separately.

**Acceptance.** Reject unqualified baselines, prove harness generation is
OFF by default, verify correctness before timing, retain paired raw samples
and environment manifest, and report explicit no-hardware/no-key outcomes.
An instrumented harness must not alter the application or service package
hash of the selected program.

## 7. Driver controls and compatibility

These are **proposed semantics**, not a claim of registered options. Preserve
the existing provisional `-perf-harness` spelling unless driver review
changes it. A possible delivery selector is `-execution=standalone|service-jit`,
default `standalone` once complete linking is available. Performance testing
is `-execution=standalone -perf-harness`; internally it is the third profile,
while delivery and harness generation remain orthogonal concepts.

| Control | Default | Meaning | Invalid or gated use |
| --- | --- | --- | --- |
| `-execution=standalone|service-jit` | `standalone` (proposed) | Select application link or callable package | Diagnose unknown value; service JIT has no `main`/standalone host dispatch |
| `-perf-harness` | OFF | Emit separate benchmark product | First release requires standalone and a qualified baseline; no implicit fallback |
| `-O0` through `-O3`, `-ipa` | Existing driver policy | Scope and optimization | Profile cannot broaden scope |
| Target and package policy | Explicit/default target | Provider, architecture, AOT/late/hybrid images | Unsupported target or late-compile prohibition fails closed |
| Domain policy | Domain defaults | AI numerical/layout policy or FHE security/CKKS policy | Must not be supplied by generic launcher or inferred from mode |
| `-keep` and trace controls | Existing policy | Retain compiler and review evidence | Do not enable benchmark work or alter semantics |

`openpy` currently reaches backend output, but the complete default `a.out`,
service-JIT package interface, and `-perf-harness` are design targets, not
current driver features. No driver option should be accepted as a no-op.
Product names and final option syntax require driver review. The user-facing
diagnostic should say what product is unsupported, not imply success because
an intermediate `.c` or `.cu` exists.

## 8. Artifacts, diagnostics, and telemetry

Each invocation or service cache entry retains a review receipt, with
content-addressed identity where feasible:

```text
source and permitted external assets
input .B, selected/checkpoint .B, ir_b2a -st -src .T
compiler .t and phase reports; selected/baseline plan IDs
host dispatch .B/.T when generated; device/evaluator .B/.T when applicable
generated C/CUDA/PTX or provider configuration, native image if produced
package manifest, checksums, ABI and provider/target version
driver/link commands, executable or callable-entry identity
runtime selection/launch or evaluator trace and separate timing scopes
correctness evidence; performance samples only in the opt-in profile
```

Standalone records the application host-PU-to-entry join. Service JIT records
the compile request, cache key, cache hit/miss, callable-entry identity, and
actual variant. Performance testing records both baseline and selected
lineages, experimental controls, and explicit `not_measured` reasons. FHE
receipts additionally record public-context and key-requirement **IDs or
hashes**, resolved parameter fingerprint, ACE rtlib source/header/build
identity, `LIB_ANT`, and lifecycle mode; they never retain secret keys.

Diagnostics distinguish source/gatekeeper, plan/legality, target lowering,
package/link, service cache, runtime/provider, security-context, numerical
validation, and measurement-comparability failures. The `.t` source trace
and its summary remain about compiler decisions; a harness report must not
retroactively label an estimated cost as a measurement.

## 9. AI and FHE adoption crosswalk

| Contract | AI first use | FHE adoption requirement |
| --- | --- | --- |
| Semantic image | `common.matmul` and tensor descriptors | FHE/CKKS semantic image and value-specific state; same mapped-image discipline |
| Program entry | Host dispatch over selected graph/kernel calls | Secretless server entry over certified CKKS evaluator/calls; separate client |
| Provider | CUDA launch adapter and selected kernel package | Public ACE rtlib C ABI, `FHErt_common` plus `FHErt_ant`, `LIB_ANT` first |
| Domain legality | Shape, layout, dtype, numerical mode, resource guards | Encryption state, resolved parameters, chain/scale, key capability, bootstrap policy, security boundary |
| Service JIT key | Model/shape/target/provider/variant identity | Also parameter/security fingerprint, evaluation-key capability, ACE rtlib/build identity; never secret material |
| Performance baseline | Independently certified same-target kernel/provider | Accepted same-security CKKS implementation and provider; no mock or plaintext baseline claim |
| Evidence | Device-PU/host-PU join, CUDA image, launch trace | CKKS-to-C/ABI mapping, server no-secret audit, public context and lifecycle trace |

The shared mode driver and package schema should live in the common driver
and runtime boundary. Domain extensions own AI workload/serving state or FHE
parameter, key, and privacy policy. `common/com` must not import CUDA or ACE
types. FHE's current CKKS2C early exit is a target lowering route after a
verified CKKS image, **not** a fourth execution profile. Its provider-private
evaluator C can be packaged for either standalone server or service JIT.

## 10. Implementation order and acceptance gates

1. **Contract lock:** publish entry/package ABI, product/option matrix,
   host-PU versus evaluator-PU distinction, and FHE no-secret invariants.
   Add driver tests that reject unsupported modes rather than silently
   producing a partial product.
2. **Standalone AI slice:** from selected matmul plan create/reopen the host
   dispatch and device images, link the runtime/provider and a thin `main`,
   run `a.out`, and retain before/after WHIRL and source-to-package joins.
3. **Standalone FHE slice:** consume the certified CKKS image and approved
   CKKS2C/ACE ANT interface; link a secretless server executable and separate
   client, then prove encrypted-input to encrypted-output operation and
   client-side correctness. Do not weaken SYNC-6 gates to reuse AI tooling.
4. **Service JIT slice:** publish the same compiled implementation as a
   callable package without `main` or standalone host dispatch. Validate
   cache/version/guard/cancellation/rollback; FHE additionally closes ANT
   context reentrancy and tenant/key-domain isolation before deployment.
5. **SGLang adapter slice:** pin one upstream SGLang version and call site;
   bridge one scheduled matmul batch to the Open64 callable entry. Verify
   borrowed-buffer lifetime, stream completion, cancellation, fallback,
   cache hit/miss, numerical result, and request-level trace joins. Expand
   beyond the one-operator prototype only after this contract passes.
6. **Performance slice:** register `-perf-harness` only with certified
   baseline resolution and separate correctness/timing scopes. Start with
   same-target AI GEMM, then FHE server/client measurement under one resolved
   parameter/security contract. Keep no-hardware results explicit.

Every gate has a compiler driver test, independently reopened binary WHIRL
evidence, ABI/link test, negative failure test, and retained artifact receipt.
GPU performance needs an actual target; FHE deployment needs the reviewed
server-only lifecycle. A design document or source-only build does not close
either acceptance gate.

## 11. Open decisions

1. Final driver spelling for delivery and output paths; provisional
   `-perf-harness` must remain explicit and OFF by default.
2. Exact shared callable-entry ABI and whether service JIT returns an
   in-process handle, an external package, or both.
3. Which package descriptors belong in mapped WHIRL versus an external
   manifest, without breaking binary compatibility.
4. Whether the standalone launcher is always generated or may be supplied
   by a user application while retaining generated program dispatch.
5. FHE server-only ACE ANT lifecycle, reentrancy, key/context isolation,
   and secure artifact retention before multi-tenant service admission.
6. First qualified AI and FHE performance baselines and the target hosts
   that will provide measured evidence.

## 12. Source and design anchors

- `osprey/driver/lang_defs.c`: `openpy` language registration and current
  backend-end boundary; not yet proof of an `a.out` path.
- `osprey/include/open64_dsl_runtime_abi.h`: current AI tensor and generated
  matmul plan/provider call contracts.
- `OPEN64-AI-COMPILATION-AND-RUNTIME-ARCHITECTURE.md`: package, runtime,
  provider, AOT/late/hybrid, and provisional performance-harness contracts.
- `FHE-CONSOLIDATED-IMPLEMENTATION-PLAN.md`: FHE process boundary,
  secretless server rule, CKKS gate, and runtime ownership.
- `FHE-O2-ACE-RESBM-METAKERNEL-INTEGRATION-PLAN.md`: ACE ANT public C ABI,
  provider lock, source/build manifest, and unresolved lifecycle gate.
- [CUDA Driver Module Management](https://docs.nvidia.com/cuda/cuda-driver-api/cuda_driver_api/group__CUDA__MODULE.html): provider-specific evidence that a portable image may be loaded with JIT options; it does not define OpenPy's service-JIT contract.
- [SGLang serving source](https://github.com/sgl-project/sglang/blob/main/python/sglang/srt/entrypoints/http_server.py): upstream server/engine, tokenizer, scheduler and detokenizer process organization; use as integration evidence, not an Open64 ABI specification.
- [SGLang kernel registry](https://github.com/sgl-project/sglang/blob/main/python/sglang/kernels/README.md): upstream operator/backend registration evidence; the proposed Open64 adapter still needs a version-pinned prototype.
