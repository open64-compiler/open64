# LN-REF-1 Runtime Guards And Enforcement Bridge

Version v0.1 | 2026-10-28 | LN2-d, independently of reduction coding

## 1. Scope And Evidence

LN2-d exports the freshly rebound conditional C0/C1/C2 plan and provides an
independent runtime-only admission/lease/host-event bridge. It does not select
a plan, prove profitability, lower an executable normalization PU, or certify
a GPU kernel. Source v1 remains unchanged and cannot export this guard. Guard
ABI v1 is **not** source contract v1; only explicit LN-REF-1 source v2 enters.

Implemented boundaries:

| Concept | Definition / owner |
| --- | --- |
| Analysis PU | Driver-selected whole active WHIRL PU. WOPT recharacterizes the exact member assignment; it never traverses other PUs. |
| Conditional plan | WOPT's read-only checked source/target/storage/event request and binding. No prior binding is accepted as authority. |
| Guard | Pointer-free snapshot exported by `WOPT_DSL_export_normalization_guard`; runtime must still validate and bind it. |
| Allocation role | Compiler-side canonical backing name. It is not an address, ST/WN identity, or live runtime handle. |
| Root | Provider-verified WHOLE allocation with capacity, device and a canonical backing key shared by all virtual aliases. |
| Lease | Invocation-scoped immutable input or exclusive output subrange held by the runtime registry. External accesses/free/rebind must cooperate. |
| Entry manifest | Trusted adapter's assertion that a specific selected entry implements the **exact** exported source, arithmetic, layout, ownership, storage and event contract. Not native instruction certification. |
| Provider limits | Actual device/compiled-entry launch limits supplied by the trusted adapter, including grid, threads/register-resource admission, and total shared reservation. Not invented memory latency constants. |
| Host observation | Successful/failed adapter invocation, guarded state change or lease lifecycle event in raw `AI_NORM_RUN` trace. |
| Internal event | Per-CTA phase/barrier/generation requirement (nodes 2..10) declared in the guard. Host drain is not observation of each internal event. |

Production files: WOPT `opt_dsl_normalization_plan.h/.cxx`, standalone
`include/open64_normalization_runtime.h`, and runtime-only
`libopen64rt/normalization_runtime.cxx`. The bridge reuses
`OPEN64_TARGET_BUFFER_SERVICE` V1 synchronous import validation without changing
that ABI. No reduction planner, runtime, test or receipt is imported or edited.
The domain-specific ordered-mean/generation protocol cannot be expressed by
the existing generic synchronous buffer service alone. No speculative generic
event framework or second reduction implementation is introduced.

## 2. Export And ABI

The exporter calls the native source/target/plan binder afresh. Invalid
scope/member/pointers return false without changing output or trace. A valid
refused plan returns false, preserves output, and emits a compact `REFUSE`
only when the caller passes its existing option-controlled trace stream.
Success exports one initialized guard; trace-off performs identical checks.
No plan selection, WN/ST/TY mutation, new option/trace bit or mapped record.

The guard is 83 host-native `uint64_t` words (664 bytes); all fields including
provenance participate in exact manifest equality. It contains no pointers,
compiler-owned types, strings or padding. Runtime checks the ABI version and
size. The raw `.guard` fixture is same-ABI review evidence, **not** a portable
network/persistent-image format. Review hosts explicitly require little endian.

| Guard fields | Binding |
| --- | --- |
| `abi_version,size,source_version,source_flags,pu,node` | Guard version 1, explicit source v2, unchanged source flags/identity. TARGET_UNBOUND and other source facts are not erased by a conditional plan. |
| `operands[3],result,operand_types[3],result_type` | Native X/gamma/beta/Y values and exact TY identities. Types remain compiler identities, not runtime pointer types. |
| `rows,width,epsilon_bits,source_line` | Exact static domain, positive finite binary32 epsilon and source coordinate. |
| `architecture,route,toolkit,ptx,environment,fp_options` | Exact common hardware ID, conditional toolchain envelope, scalar-pointer route, RNE/gradual/no-traps/class-only environment, all unsafe options off. |
| `kind,layout,contract,context,ownership,threads,ctas,tail` | C0/C1/C2, contiguous F32 rows, runtime identity/context, one CTA/row with lane-zero ordered statistics, valid-before-access/arithmetic tails. |
| `stride,shared,reserved_shared,shared_limit,shared_quantum` | One row arena plus distinct mu/q, checked target reservation; C0 uses zero scratch. |
| `participants,barriers,banks,bank_word,warp,bank_conflict` | Required uniform barriers and common-target scalar contiguous bank facts. Current four-profile retained-layout minimum alignment is 16 bytes. |
| `obligations` | Original six outstanding plan obligations (63), not a runtime authority grant or a claim that they have all been discharged. |
| `ranges[4]` | X, gamma, beta, Y: role, WHOLE capacity, byte offset, base alignment, lease, exact accessed byte length. |
| `before[15]` | Complete immediate-successor DAG. Reject cycles, unknown/self edges or missing conservative reachability. |

Guard trace: `AI_NORM_GUARD 1 EXPORT 83 <83 positional words> source_line`.
The duplicated final source line preserves the trace convention. Tools provide
the column legend once, audit native source/target/plan/range/DAG joins and
compare the exact exported bytes. Native TY identities are opaque auditor
facts, not independent type reconstruction from samples. V1 and oversized
retained plans produce `REFUSE status source_line`, never guard bytes.

## 3. Admission And Canonical Storage

The runtime is a separately built C-ABI/C++11 library. The optional independent
build is `make -f osprey/libopen64rt/tests/Makefile.normalization SOURCE=<osprey>`
from a fresh output directory. It creates `libopen64_normalization_runtime.a`
and the runtime-only test. It is not linked into WOPT/backend or automatically
installed by the legacy runtime makefile. Driver/provider packaging is later work.

Registration uses the existing service's `validate_import`, plus a trusted
WHOLE-root resolver to identify virtual aliases. Pointer inequality is not
noalias. Duplicate backing keys or overlapping registered address roots are
refused. Context-local handles are minted monotonically and never reused;
closed/retired/foreign IDs stay invalid. Registry capacity is 64 roots and 64
live tickets; exhaustion refuses rather than silently evicting live state.

Admission snapshots guard, manifest and root handles; checks exact manifest
agreement, source/target/schema arithmetic, capacities, checked footprints,
offsets/alignment, context/device/architecture, live roots and actual provider
grid/thread/shared limits. All four role/root equalities must agree in **both**
directions. Shared gamma/beta value IDs require identical range bindings.
Shapes/layout are the guarded typed raw-buffer API `[R,N],[N],[N],[R,N]`; this
API does not ingest untrusted framework tensor descriptors. A framework adapter
must validate its actual descriptors before binding those buffers.

Under one context lock, acquire all three immutable read leases and the Y
exclusive lease atomically. Inputs may overlap each other; Y cannot overlap
any input. Across invocations, overlapping reads are legal, while any overlap
with an outstanding write is refused. Adjacent nonoverlapping views of one
root are structurally legal. Out-of-band free/rebind/access and two independent
registries managing the same active storage are outside the cooperative API
contract; the service owner must route such accesses through one registry.

No callback/launch or ticket publication occurs on admission refusal. Supplied
output ticket remains unchanged. There is no implicit fallback: a caller may
separately admit a legal C0 guard or diagnose refusal. This never grants C0
permissions from a refused C1/C2 plan.

## 4. Host Protocol And Failure Safety

The full 15-node DAG remains required. Host API transitions are only:

```text
0 input_ready -> 1 launch -> 11 kernel_done/drain -> 12 result_visible/consumer_join
             -> 13 input_release -> 14 output_release -> close
```

`READY` waits input visibility; `LAUNCH` issues the exact selected entry once;
`DRAIN` waits every launched CTA; `PUBLISH` joins result consumers. Release
removes registry leases, not device allocations. A callback's work starts only
after successful predecessor completion; a `begin_<phase>` row is recorded
before callback entry, completion only after it succeeds. An already completed
event is idempotent and does not reissue work.

Nodes 2..10 are **not** host-callable. Drain records bit 11 only, not synthetic
bits for row-ready, mean, mu visibility, centered row, variance, q visibility,
epilogue/readers/reuse. The adapter manifest promises that its selected kernel
enforces those obligations per row/CTA; executable WHIRL and later certification
must establish the promise. A CTA barrier is never used as host/inter-CTA
visibility. Within generated phase code, all predecessor work must finish
before any next-phase work begins; reporting ordered completion timestamps
alone is not sufficient. C2 must preserve per-element read-before-overwrite,
all mean readers complete before X-generation reuse, and all d readers complete
before arena reuse. Runtime success does not independently prove these facts.

Adapter calls run unlocked with the ticket busy. Reentrant and concurrent
transitions/abort/close refuse while busy. No callback may throw. Context/entry
objects outlive calls; destruction and root free/rebind are externally synchronized.
After a failed launch, work may already be queued: never retry launch, publish
or release; only drain and abort are permitted. Failed waits retain leases.
Abort after a launch requires successful drain; abort after consumer entry also
requires a successful consumer join. Before launch, abort is safe. Close requires
all leases released/aborted. This layer never frees device memory. A failed
consumer join can be retried but its effects must obey the adapter contract.

Production contexts accept only production entries (flags 0). Explicit host-test
contexts/entries (flag 1) cannot cross that boundary. A production manifest is
still a trusted assertion, not a cryptographic kernel attestation; no production
CUDA normalization provider is delivered by this milestone.

## 5. Verification And Reports

Native review covers all four exact targets and eight source variants, actual
PREOPT controls and full mapped reader inspections. Fresh export queries also
run across every earlier LN2 legality/refusal test. Runtime tests consume actual
native guard bytes, not a Python-fabricated contract. They deliberately do not
compute normalization, avoiding a second executable kernel source of truth.

Retain paired `.B`, separate full `ir_b2a -st -src` `.T` directories, source
snapshots, binaries/library, native `plan.t`/`guard.t`, raw host `.runtime.t`,
commands, checksums and immutable receipts. Independently replay final retained
paths before indexing. Do not confuse native input source lines with mock test
locations; registry-only rows without an invocation use line 0.

| Report | Required summary |
| --- | --- |
| `guard-binding.log` | Exact export column legend, source/target/plan joins, per-case exports/refusals; declarations versus exported checks. |
| `runtime-guards.log` | Per-guard host observations/checks/refusals, callback/order/lease/failure evidence and explicit host-mock scope. |
| `optimization.log` | Complete successful guard-binding and runtime-guard report contents. |
| `optimization-summary.log` | Whole census, preserved v1, observed host order/leases, internal events provider-declared not observed; COST/DECISION/TRANSFORM none; executable lowering/provider/GPU open. |

Every log begins `# What it summarizes:`. Raw host rows are
`AI_NORM_RUN 1 EVENT seq ticket action status host_done leases host_test source_line`.
Each admitted ticket first emits `BIND ticket 83 <guard words> source_line`,
`LIMIT ticket max_ctas max_threads max_shared flags source_line`, and four
`RANGE ticket index role runtime_root backing_key capacity offset alignment bytes lease source_line`
rows under the same prefix/version. Tools join these actual bindings to the
native export and validate the limits/ranges before counting admission.
Positive sibling-output/adjacent-range host fixtures intentionally mutate only
backing bindings; reports count them separately from exact native-export
admissions. They are runtime structural tests, not additional native exports.
The independent auditor rejects malformed schemas/counts, changed source/plan
facts, inconsistent bytes, invented internal completion, early callback work,
missing begin/completion pairs, failures that release leases, and missing cases.

## 6. Next Gates

LN3 must compare complete legal C0/C1/C2 costs including pointer-scalar traffic,
ordered critical path, row scans, barriers, tail work, resources and bubble
costs. Neither export nor runtime admission is a profitability decision.
LN4 must lower the selected complete plan into authoritative WHIRL and prove
entry manifest, actual launch/register/shared binding and internal phase/barrier
conformance. A real service adapter consumes these contracts; host mocks do
not establish device arithmetic, resource allocation, numerical correctness,
performance, or GPU certification. Independent LN1-d/e numerical evidence
remains a separate acceptance layer; all source-v1 permissions stay unchanged.
