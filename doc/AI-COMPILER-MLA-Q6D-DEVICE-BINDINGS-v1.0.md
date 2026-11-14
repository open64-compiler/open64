# MLA Q6d device bindings specification v1.0

November 10, 2026. Continues [Q6c GPU lowering](AI-COMPILER-MLA-Q6C-GPU-LOWERING-v1.0.md).
Q6d implements a CUDA Driver binding owner over the existing Q3 registry.
NVIDIA numerical/race/performance qualification remains Q7. AOT selection,
phase selection and serving-engine packaging ABI remain deferred.

## Ownership and reuse

`VHO_MLA_CUDA`, declared in `be/vho/tensor_serving_cuda.h` and implemented in
`libopen64rt/mla_serving_cuda.cxx`, is an explicit optional embedding product.
It owns its module, device allocations, pinned host staging, ready/completion/
consumer events and bookkeeping. Context and producer/execution/consumer
streams are borrowed and must outlive their uses. It never switches context,
uses a default stream, creates a kernel, invokes search or starts a JIT.

The existing Q3 registry remains the single authority for allocation
identities/generations, input/output/scratch/status leases, source observation,
parent-version comparison and atomic publication. Q6d adds a versioned explicit
memory service and a private split-execution boundary to that registry. Host
constructors, memory-kind checks, callback API and publication records retain
their existing contracts. The public bundle-facts layout is unchanged.

The LN4 CUDA adapter was reviewed for current-context checks, cubin ownership,
actual pointer bounds, loaded function resources and retryable cleanup. Its
fixed four-argument normalization guard/registry is incompatible with Q3's full
MLA source ABI, paired cache/scale generations and paged parent publication.
Q6d therefore reuses Q3's authority and those established Driver patterns rather
than introducing a second MLA registry or changing the LN4 contract. Existing
buffer-service v1 explicitly requires synchronous operations; Q6d does not
silently reinterpret it as an asynchronous service.

The optional archive is `libmla_serving_cuda.a`; it uses a registry object
without test-only counter APIs. It is an internal project embedding component
using the existing owned source-contract services, not a standalone SDK or an
engine-specific package. The review library supplies test-only Driver symbols;
production linkage supplies CUDA Driver symbols. No CUDA SDK/library, JsonCpp,
Python or new binding API is added to be.so. Ordinary compiler builds do not
require the optional CUDA target.

## Code authorization and fresh device invocation

`VHO_MLA_CUDA_CODE` is a trusted owner's fixed-code record: owned source program,
source/mapping identities, three native Low keys, actual emitted entry names,
launch/shared extents and cubin size/SHA-256. The owner must authorize the full
association against all three successful Q6c native audits and the assembled
image digest. A claimed key or image hash alone does not establish compiler
provenance. The SHA service hashes an owned immutable cubin copy before load;
PTX text/fatbinary input is refused. Native ELF cubin loading, sm_90 observations
and CUDA >=12.8 enforce this implementation's AOT baseline.

Entry names bind the actual emitted native declarations. Source-program class
names are not necessarily the final ABI: canonical High may rename the entry
`open64_quantized_high`. All three names must share their approved prefix and
prepare/attention/finalize suffixes. Runtime validation checks source semantics,
fixed CTA geometry, exact pointer-parameter count/offsets/sizes, static shared
bytes, register bounds, binary architecture, reported PTX virtual target, cluster requirements and
positive residency. These observations do not establish numerical correctness
or that the selected configuration remains the fastest for a new request.
The Driver PTX virtual-target attribute is not treated as the PTX ISA .version;
ISA/toolchain provenance belongs to the approved Q6c artifact.

The host snapshot recorded during Q6c compilation is provenance, **not a device
lease**. `Begin` explicitly instantiates this already chosen code against a
fresh Q3 device observation and stores that ticket's exact Q4 snapshot identity.
`Submit` revalidates it before arming execution. The fixed source geometry,
encoding, page policy, program ABI and approved kernel decisions must match;
current buffers, CUDA allocation IDs, registry generations, content versions,
page maps, source controls and parent are reobserved. This operation neither
selects an algorithm nor substitutes a new performance winner. No frozen host
allocation identity is imported into device authority.

## Device memory and input readiness

Only adapter-owned `cuMemAlloc` storage is admitted in this initial contract.
There is no borrowed engine-allocation/managed/peer-memory import extension.
Unsupported memory ownership retains the existing source fallback.

Each allocation has both a Q3-issued handle/generation and an observed CUDA
`CU_POINTER_ATTRIBUTE_BUFFER_ID`. Type, owning context/device, base, extent and
buffer ID are checked before access and launch. Same-address reuse refuses
rather than freeing/writing a replacement allocation. Address-space domains
keep independent device contexts separate from the host registry. Numeric
device addresses are never dereferenced by Q3 host code.

Cooperative `Write` first obtains Q3's unpinned/exclusive mutation permission,
then advances the device content version before potentially dirty I/O. A ready
event is recorded after producer-stream upload and joined by the execution
stream. The operation completes before returning success. Inputs remain on the
device across later invocations; Q3 publication does not copy full KV payloads
to the host. Source observation copies bounded metadata, masks, page/token/
slot controls, status and needed contiguous scale spans. It validates scales
only at source-read positions, preserving poison in unused page padding.

Every transfer uses adapter-owned pinned host staging. H2D takes a copy before
queueing; D2H never targets a caller stack/vector asynchronously. On error, the
staging record and affected device storage remain owned until the exact
originating stream generations and execution stream successfully drain. A CUDA
error is not proof of completion. Failed event destruction/free retains its
handle for retry. Reads refuse active writers; retirement refuses all pending
or published leases. Successful unregister followed by failed physical free
retains a cleanup record and does not unregister twice.

## Submission, completion and cancellation

1. `Begin` acquires Q3 shared inputs and exclusive outputs/scratch/status and
   freezes actual device facts. Page/scale/control validation precedes use.
2. `Submit` reobserves that snapshot and enters Q3's private running state.
   Status is initialized pending, an owned completion event is reserved, and
   prepare -> attention -> finalize launches are issued exactly once on the
   explicit execution stream. Parameters are the original Q3 formal order.
3. Record an owned completion event after the last queued stage. Events are
   never re-recorded for another generation. Querying NOT_READY publishes
   nothing and leaves the ticket and every allocation/module lease live.
4. On event success, or after successful recovery drain, read actual status and
   reobserve controls/scales/generations. Q3 then performs its existing parent
   comparison and atomically publishes all four returned cache/scale buffers
   with mapping/metadata and valid lengths. Competing completions against one
   parent can publish at most one successor. No partial tuple becomes visible.
5. A launch/status/control/scale/parent failure denies publication. Once drain
   is proven, writable versions advance and the failed ticket expires. If
   kernel or subsequent observation I/O drain is uncertain, retain the running
   ticket, buffers, module and pinned host staging; Complete/Drain can retry.

Prepared cancellation expires its ticket without launching. Submitted cancel
permanently denies publication and waits/queries/drains the actual queued work
before releasing leases. Partial launch failure can return a cleanup-only live
ticket. The caller must inspect liveness and cancel/complete it; it cannot run
a source fallback concurrently against still-pinned buffers. A safe fresh source
fallback remains available after cancellation/drain. There is no automatic
algorithm retry, dispatch threshold or invisible fallback launch.

## Consumer and publication lifetime

`Reader` acquires Q3's whole-bundle reader, resolves actual bounded device views
under that lease, verifies the consumer stream context/generation and enqueues
its wait on the immutable producer completion event. Imported bundles are
already synchronously ready. Cache payloads, scales, page mappings and metadata
remain one immutable generation tuple.

The first `Reader_done` seals the consumer sequence with its own event. The
caller must enqueue no further use afterward. Pending/error preserves the
reader and therefore all bundle allocations. A successful event or explicit
consumer-stream drain releases the reader exactly once. Consumer event-record,
query, destruction and drain failures are retryable; no host boolean substitutes
for observed completion. Retiring a bundle refuses readers and pending parent
users. Retired completed jobs release their events; failed destruction keeps
cleanup records for Drain/Destroy rather than growing unbounded event history.

`Destroy` refuses live jobs/readers/bundles/registered allocations. Once cleanup
starts, an unload/resource failure leaves a cleanup-only object. The caller
must retain it and the borrowed CUDA context/streams for retry. Object destruction
requires external exclusion of every concurrent API caller. Context loss or
illegal external reclamation may prevent recovery; this contract never claims
that an error permits freeing unknown work. Owner cooperation is required:
registered storage and borrowed context/streams cannot be freed/recycled by
external code while their leases are live.

## Validation and preserved compiler material

`run_mla_q6d_review.py` drives the actual optional adapter against a test-only
CUDA Driver implementation compiled with the real SDK declarations. Synthetic
device addresses have no host backing at those virtual addresses. The model
queues uploads/downloads and each actual Q6c-emitted kernel separately; it
requires explicit event/stream progress. The kernel simulator executes every
physical CTA lane and Q6c's delayed bulk-copy protocol. This is functional
lifetime/reference evidence, not an NVIDIA hardware or timing model.

Coverage includes dense/paged/append S8/U4, both transports, head/token tails,
zero active queries and genuine 8193-token cases; six outputs against selected
source, PyTorch and independent scalar references; immutable inputs/canaries;
pending publication/teardown refusal; duplicate submission/completion; parent
CAS; source status; partial stage launch; unknown launch and I/O drains;
context/stream generation/capture; code authorization and loaded ABI/resources;
CUDA allocation-ID reuse; consumer event/drain failure and cleanup retries.

The Q3/Q4 regression suite, pure estimation controls and native Q5 full traces
remain required. Native optimizer, be_driver emission, CUDA generator and
WHIRL representation are unchanged in Q6d. The retained Q6c .B/.T, annotated
.cu and .ptx are authenticated and reused verbatim; all COMMENT explanations
and the existing PREOPT workaround remain. No general COMMENT repair is added.
The review source exposes only a test embedding ABI; it does not freeze a new
serving-engine parser or package format.

Build the optional archive/review product with the existing Open64 tool flags
and `CUDA_HOME=/cuda`. The tracked review uses OPEN64_Q6D_SOURCE,
OPEN64_Q6D_PREVIOUS, OPEN64_Q6D_TOOLS and OPEN64_Q6D_OUTPUT directory parameters.
Retain source hashes, exact command logs, SDK identity, archive/import audits,
reference/lifecycle results and an independently replayed immutable receipt.

Driver semantics are checked against NVIDIA's [CUDA 12.8 event API](https://docs.nvidia.com/cuda/archive/12.8.0/cuda-driver-api/group__CUDA__EVENT.html)
and the retained 12.8 SDK declarations for context/stream IDs, pointer attributes,
resource/parameter queries and asynchronous copies. Event success/stream drain
and owner-held generations are distinct from caller-recorded snapshot strings.

## Remaining gates

Q7 still requires real NVIDIA Driver/module execution, memory/race checking,
numerical acceptance (including device math), failure-path testing where the
hardware permits it, resource observations and performance measurements.
No NVIDIA host is available; neither the model nor offline code assembly is
reported as GPU execution or speedup. Q6c's <=48 KiB shared-memory envelope,
strict per-head folds and initial serial prepare/finalize remain. Larger latent
layouts remain refused until a new layout/resource contract is qualified. Device
control/scale observation currently synchronizes its bounded copies and may
serialize admission behind earlier stream work. Overlapped admission through
host control mirrors needs a separately qualified version/event contract.

AOT selection stays deferred: no prefill/decode selector, runtime threshold,
automatic variant choice, JIT policy or serving-engine packaging ABI. Primary
owner, successor, independent reviewer and approving lead remain TBD; transfer
acceptance still requires those assignments.
