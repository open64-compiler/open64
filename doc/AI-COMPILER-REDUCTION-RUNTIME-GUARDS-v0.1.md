# Direct Ordered Reduction Runtime Guards

Version v0.1 | 2026-10-27 | R2-c runtime enforcement, independent of MMA1

## 1. Scope And Evidence Levels

This increment connects the [conditional direct binding](AI-COMPILER-REDUCTION-DIRECT-BINDING-v0.1.md)
to an additive, C-compatible guard descriptor and a separately linked runtime
component. WOPT revalidates current native WHIRL before export. Runtime admission
checks actual registered allocation roots, tensor descriptors, a trusted entry
manifest and atomic range leases. An event state machine enforces readiness,
launch, completion, consumer join and release.

Implemented and exercised with a **host-mock adapter**. This is real admission,
lease and state-machine execution, not a GPU run. No reduction kernel is selected,
no executable reduction transformation is added, and VHO still diagnoses the
missing lowering. Production CUDA root resolution, launch/wait/consumer adapters,
selected host/device WHIRL calls, instruction certification and GPU numerical
validation remain open. The explicit ordered v2 contract is unchanged; Python
`torch.sum` v1 is neither upgraded nor granted a new numerical permission.

## 2. Terms And Ownership

| Concept | Meaning / owner |
| --- | --- |
| Guard | Owned numeric snapshot of a freshly validated WOPT binding; not executable IR, a cost choice, or a mapped-image record. |
| Role ID | Symbolic input/output storage identity in the compiler contract. At admission it maps to runtime-minted root handles. |
| Backing-object key | Trusted provider identity shared by virtual aliases of one underlying allocation. It is not inferred from pointer inequality. |
| Canonical root | Whole allocation extent validated by the provider; subviews use one root handle plus byte offsets. |
| Registry | One authoritative live-root and lease ledger for cooperating accesses in a provider context. |
| Ticket | Runtime-minted, nonreused invocation identity owning an immutable guard, entry, subviews and event state. |
| Entry manifest | Trusted adapter assertion that its selected entry implements this exact source/target/launch contract; not PTX/SASS proof. |
| Snapshot lease | Shared read access to the exact input interval; no writer may overlap until input release. |
| Exclusive lease | Output interval reserved against all unrelated readers/writers through consumer join and output release. |
| Drain | Successful adapter completion wait proving even an ambiguously failed launch has no outstanding device accesses. |
| Consumer join | Wait for every authorized consumer; it must not resubmit consumption on retry. |

Common/com continues to own native semantics and policy-free target identity.
WOPT owns export over the active whole PU without tree/table mutation. Runtime
owns live handles, leases, event transitions and cleanup. A trusted target adapter
owns actual allocation/context authority, selected device entry, stream/event
operations and consumers. Program-specific host dispatch eventually emits calls
to this runtime boundary; a service JIT can invoke it directly. No MMA profile,
hardware table, intrinsic or physical WHIRL layout changes.

## 3. Compiler-To-Runtime Wiring

`WOPT_DSL_export_reduction_direct_guard` reconstructs a request from the owned
binding, resolves its architecture through the existing immutable lookup and
re-runs native characterization/binding on the current active-PU member. Stale
source, refused status, missing stage/obligation bits, inconsistent byte counts,
unsafe arithmetic and invalid events cannot export. Failure leaves output
untouched. Named-field copying avoids compiler padding, WN/TY pointers and an
architecture-string lifetime dependency.

`OPEN64_REDUCTION_GUARD` is an additive **process-local ABI**, version and size
checked; it is not a portable file format or a new tensor descriptor. Retained
`guard.bin` files are same-build integration evidence, not user deployment files.
The existing `OPEN64_DSL_TENSOR_DESCRIPTOR_V1` and `TargetBufferService` V1 layouts
and synchronous allocation/transfer/release behavior remain unchanged.

```text
native canonical ordered reduction WHIRL
  -> WOPT direct binding + outstanding obligations
  -> fresh native revalidation -> owned guard
  -> runtime admit(guard, trusted selected-entry manifest,
                   live root handles, existing tensor descriptor blobs)
  -> guarded ticket -> adapter events -> release -> close
```

The focused producer exports and the standalone runtime consumes the **same
native-generated descriptor**. Production driver/host-PU wiring is deliberately
not claimed: there is not yet a selected executable reduction plan to dispatch.
The exported obligation mask remains 15; individual runtime invocations report
their checks/events instead of rewriting the compiler contract to mask zero.
Manifest agreement does not discharge actual kernel-code conformance.

## 4. Admission Algorithm

```text
register(root_view):
    validate device/context/import with existing buffer service
    resolve trusted whole-root extent and backing-object key
    reject duplicate key, overlapping registered extent, overflow or bad device
    mint context-local nonreused root handle

admit(guard, entry, input_root, output_root, X_blob, Y_blob):
    snapshot guard and entry; verify ABI, exact source v2, scalar ISA, FP policy
    require matching architecture/context, exact entry manifest and test mode
    validate bounded descriptor blobs: F32, static replicated device contiguous
    require X[R,N], Y[R], checked 4*R*N and 4*R bytes, canonical strides if supplied
    validate every DAG edge, acyclicity, row ownership and launch coverage
    lock registry:
        resolve live roots; match role equality, exact capacities and base alignment
        bind checked subranges; reject input/output overlap
        reject any overlap involving an exclusive lease on another live ticket
        atomically acquire input snapshot AND output exclusive leases
        mint ticket; publish immutable invocation snapshot
    refusal: no ticket, no callback, no launch, no automatic fallback
```

Read/read sharing is legal. Disjoint intervals in one canonical root are legal.
Distinct virtual addresses are not noalias: the new root resolver must return
the same backing-object key for aliases, which the registry refuses to register
twice. Import descriptors' `NO_ALIAS` flag grants no proof. Capacity equality is
intentionally conservative for this fixed guard; larger/different roots require
rebinding rather than silently weakening the contract.

The initial implementation supports 64 live roots and 64 live tickets, fails
closed at capacity, and allocates no per-transition objects. Root/ticket IDs are
never reused, even across runtime instances; exhaustion fails closed. Registry
state is mutex protected; callbacks run unlocked, with a per-ticket busy flag.
Same-ticket reentry/concurrent transition is refused; concurrent admissions
atomically acquire both leases. Context destruction is externally synchronized
and refuses live objects.

## 5. Event And Failure Protocol

| Index | Event | Adapter effect / release rule |
| --- | --- | --- |
| 0 | `ready` | Wait for complete input readiness; failure is retryable or safely abortable before launch. |
| 1 | `launch` | Issue the selected entry once, only after readiness. Mark attempted before calling adapter. |
| 2 | `complete` | Wait for device completion/drain; failures retain both leases. |
| 3 | `consume` | Join every designated consumer after completion; failure retains output storage and is retryable as a join, not resubmission. |
| 4 | `input_release` | Unpin the input interval after completion and all additional incoming edges. |
| 5 | `output_release` | Unpin output after consumer join and all additional incoming edges. |

Required closure: `0 < 1 < 2 < 3 < 5` and `2 < 4`. **All extra supplied
edges are enforced**, not just the minimum. Completed events are idempotent;
callbacks are not repeated. A wait failure does not mark the event complete.

A failed launch may already have queued work. It cannot be retried or followed
by output publication or CPU fallback. A successful `complete` may prove drain
even though launch did not report success; only abort cleanup follows. Failed
drain keeps both leases pinned. Before launch, abort is safe; after any launch
attempt it requires successful completion, and after consumer join has started
it additionally requires successful consumer join. Abort discards unpublished
output and releases leases. Close refuses busy/pinned tickets. Root retirement
refuses any live lease; freeing/rebinding remains the owning provider's job.

## 6. Trust Boundaries And Remaining Guards

The provider must own or validate the whole-root extent, real device/context and
backing identity; `validate_import` alone has no such canonicalization contract,
so a separate required `OPEN64_REDUCTION_ROOT_RESOLVER` supplies it without
changing V1. All relevant accesses in that provider context must use the same
authoritative registry or equivalent coordination. Out-of-band raw CUDA frees,
writes, aliased mappings or a dishonest adapter cannot be prevented by this
cooperative runtime. Registration/retirement/external free of one root must be
externally serialized. Context/adapter/trace lifetimes outlive outstanding calls;
callbacks must not throw. Kernel manifest conformance remains trusted until
selected-WHIRL and emitted-instruction certification are available.

Host-test entries have an explicit flag and cannot enter production contexts.
Production mode is an adapter contract, not an automatic certification badge.
This milestone adds no CUDA dependency and no runtime linkage in `be.so`,
`wopt.so` or `lw_inline`. The runtime is separately compiled with C++11/pthreads;
it is not silently added to the historical `libopen64rt.a` default build.

## 7. Trace, Tool Summary And Review Artifacts

Existing compiler AI trace controls remain unchanged. The caller supplies an
optional export trace; runtime tracing has an independent optional `FILE*`.
Turning either off must change no admission/enforcement behavior. No new trace
bit or implicit weakening option. Raw trace is positional, source line last:

```text
AI_REDUCE_GUARD 1 HEADER node input result contract arch rows width obligations selected source_line
AI_REDUCE_GUARD 1 EXPORT ...
AI_REDUCE_RUN 1 HEADER seq ticket action status done leases source_line
AI_REDUCE_RUN 1 BIND_HEADER ticket contract arch in_root out_root in_offset in_bytes out_offset out_bytes e0 e1 e2 e3 e4 e5 source_line
AI_REDUCE_RUN 1 BIND ...
AI_REDUCE_RUN 1 EVENT ...
```

`done` uses event bits 0..5; `leases` uses input=1/output=2. Status codes are
OK=0, invalid=1, conflict=2, order=3, busy=4, backend=5, failed=6, capacity=7.
Ticket zero denotes non-admitted operations. Source line zero denotes operations
without a source-bound ticket. Sequence numbers apply to EVENT rows.

The independent auditor validates the exported source identity against the
direct trace (already joined to actual PREOPT), replays every ticket/DAG/event,
checks masks, failed-launch recovery and terminal release, and rejects forged
success evidence. `runtime-guard.log` summarizes admission, refusals, lease/event
enforcement and recovery. `optimization.log` collects successful reports and
recorded refusals. `optimization-summary.log` explicitly reports host-mock proof,
selected=false, transformation=none and open lowering/CUDA/certification gates.
Each derived log starts with `# What it summarizes:`.

Retain source `.B`, independently reopened source-interleaved `.T`, actual
PREOPT `.t`, native export trace, `guard.bin`, runtime `.t`, commands/build logs,
test receipts, source snapshots and hashes under immutable
`r2-reduction-runtime-guards` review receipts. No transformation occurs; do not
fabricate before/after IR. Negative tests cover stale exports, descriptor bounds,
ISA/options, alias roots/ranges, read/write conflicts, event order, failed launch/
drain/consumer waits, safe abort, idempotence, reentry and racing admissions.
Six supported native guard cases are run with trace on and off; C/C++ header,
warning-clean builds, runtime sanitizers and prior compiler/report regressions
are separate evidence lanes. None substitutes for GPU numerical certification.

## 8. Next Execution Slices

Follow-up: the [R2-c2 staged binding](AI-COMPILER-REDUCTION-STAGED-BINDING-v0.1.md)
now defines compiler-side scratch/copy/reuse/tail obligations. The runtime in
this document remains direct-only; its guard ABI does not admit staged plans.

1. Bind staged ordered reduction copy visibility, slot reuse, tails and budget;
   reuse this runtime boundary rather than introducing a second ownership model.
2. Complete direct/staged costs and explicit profitability-gated plan selection.
3. Materialize selected high-level WHIRL loops and host/service calls; preserve
   before/after images and make emitters consume that one executable truth.
4. Implement real target root/launch/event adapters and inspect strict-add code;
   then certify numerical behavior, resources and performance on an allocated GPU.

## 9. Accepted Bounded Evidence

Eight native fixtures pass 320 assertions, retain 212 direct query rows and
export six supported guards; v1 and unsupported layout cannot export. Each of
the six guards executes 176 host-runtime assertions with trace on/off, both in
Linux and under native macOS ASan/UBSan. The six normal traces replay 78 admitted/
closed tickets, 222 refused admissions and 30 injected backend failures. All
37 trace/auditor tests pass, together with prior numerical-binding/row contracts,
focused WOPT/common/FHE compatibility and existing buffer-service tests.
Backend and inline symbol checks find no new runtime dependency.

The first Docker/QEMU sanitizer lane returned an inability-to-inspect-thread-
registers warning and did not pass the strict trace-on/off output comparison.
Its log is retained, not reported as successful sanitizer evidence. Native
macOS ASan/UBSan runs supply the successful sanitizer lane. No GPU/PTX/SASS or
production adapter certification is claimed.

Durable evidence is indexed under
`/Users/shinmingliu/open64/artifacts/review/ai-optimization/research/r2-reduction-runtime-guards/`.
The primary guard SHA-256 is
`7ecb546cd16894febd2d271e262a02b2eb87e5f352d2b0adba4cc5a81cedfc3c`.
Raw runtime rows distinguish rejected early events (status 3, unchanged leases),
input release before consumer join (input unpinned/output pinned), ambiguous
failed launch and failed drain (both pinned), and final safe abort/release.
