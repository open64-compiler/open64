# LN4 Host Dispatch And Service Execution Consumer

Version v0.1 | 2026-10-29 | Runtime composition, not a new kernel producer

## Terms And Ownership

| Term | Meaning |
| --- | --- |
| Execution consumer | Runtime-only owner connecting one selected compiled entry to buffer bindings and the existing CUDA adapter. One active invocation at a time; independent consumers may share one registry. |
| Shared registry | Caller-owned normalization runtime for the actual device/context. Owns canonical roots and range leases across consumers, not a private per-dispatch lease map. |
| Root request | Whole host allocation, or a whole device allocation plus its existing shared-registry handle. X/gamma/beta/Y subranges come exclusively from the selected guard. |
| Owned root | Temporary device allocation for a host root. The consumer registers it once and retires it before release. |
| Borrowed root | Already registered device allocation. The consumer never retires/frees it. A read-only snapshot checks its identity; only admission grants a lease. |
| Canonical role | Compiler-side allocation name. Equal roles must identify one identical whole request/root; distinct overlapping whole host roots refuse conservatively. |
| Readiness hook | Caller-owned idempotent join for input producers. The consumer waits here before copying any host input bytes. |
| Consumer hook | Caller-owned idempotent join for result users. Host Y copyback occurs first; retries never recopy an already successful output transfer. |
| Quiesce hook | Caller-owned join for all possibly outstanding buffer/hook operations after a failure or cancellation. Failure pins all remaining state. |

The compiled descriptor/guard is immutable and authoritative. This layer neither
selects a plan nor generates CUDA or host WHIRL. It is a generic runtime building
block for a future compiler-generated host dispatch PU and a service JIT caller.
It does not create compiler sessions, CUDA contexts/streams or performance-test
harnesses. Source v1 and reduction coding remain unchanged.

| Execution mode | Consumer handoff |
| --- | --- |
| Standalone application | Program-specific host dispatch passes host whole-root views; this runtime block owns temporary device storage and guard-sized transfers. Host-PU call emission is a later compiler step. |
| Service JIT | Service passes registered device roots and the shared registry; it owns requests/batching, buffers and producer/consumer joins. No generated host PU is required. |
| Performance testing | An explicitly requested harness may reuse either path, but must distinguish kernel timing from allocation/transfer/end-to-end timing and provide a baseline. This layer does not silently create that harness. |

## API And Algorithm

`open64_normalization_execution_open` borrows the shared registry, copies service/
hook descriptors, and opens the existing exact-entry CUDA adapter. Registry,
services, hooks, context/stream and trace outlive the consumer. Open normally
preserves output on failure; failed module rollback returns a cleanup-only handle.

```text
begin(root_requests[4]):
    require idle; validate whole extents, offsets/alignment and role equality
    validate borrowed device handles through a read-only registry snapshot
    commit active state before any allocation; partial failure stays cancellable
    for each unique host role:
        allocate one whole device root without reading host bytes
        retain allocation even on failure; register only a valid owned root
    for each unique device role:
        borrow its existing handle and whole view; never acquire ownership
    adapter.admit(shared_registry, root_handles)  // acquire all leases atomically

resume():
    if a prior operation failed: require quiesce before retry
    READY: verify admitted subviews equal bound roots + guard offsets
           wait for input producers
           copy only X/gamma/beta valid host subranges into owned device roots
    LAUNCH: existing adapter issues the selected kernel once
    DRAIN: existing adapter joins the selected stream
    PUBLISH: copy only valid Y back if host-bound; join result consumers
    INPUT_RELEASE, OUTPUT_RELEASE: existing registry drops leases
    adapter.close; retire owned roots; release owned buffers; return idle

cancel():
    drain if launch was attempted (including ambiguous failure)
    join PUBLISH if a consumer/copyback was started; never invent completion
    require quiesce; abort ticket; adapter.close
    retire owned roots before release; retry failed retire/free without duplication

run(requests): begin; resume
destroy(): refuse active state; unload adapter; never destroy the shared registry
```

Failed launch is not reissued; cancel drains and aborts. Failed READY, DRAIN,
PUBLISH or cleanup can be retried with `resume`/`cancel` as applicable. Cancellation
after copyback cannot undo host-visible results. Callers retain host loans until
the consumer returns idle; imported roots stay under the original owner's lifetime.
All operations are serialized by a nonblocking atomic operation token; callback
reentry returns BUSY without attempting to relock a nonrecursive mutex. Object
destruction/lifetimes require external synchronization. Do not call
the base runtime directly on consumer-owned tickets.
Input host contents remain immutable for the full loan, including retries.
Deployment must synchronize registration/free/rebind and provide buffer/hook
services safe for concurrent consumers; the operation token protects only one
consumer, not the provider's global allocation state.

## Reused Building Blocks And Boundary Choices

Use existing TargetBufferService allocation/transfer/validation/release callbacks,
normalization registry and exact-entry CUDA adapter. Do not invent CUDA memory APIs
inside the compiler. Deployment still supplies a reviewed target provider, real
SHA-256 hook, producer/consumer joins and quiescence hook.

Whole-root grouping plus partial-failure quarantine cannot use the old eager
`bind_input`/`bind_output` helpers unchanged: those copy whole inputs or immediately
release failed allocations. This consumer instead reserves through the same
provider callbacks, performs guard-sized copies only after lease admission, and
uses the existing release helper only after quiescence/retirement. It is not an
independent allocation policy or parallel target service implementation.

Distinct host roles whose whole spans overlap refuse, including read-only aliases;
canonicalize intentional sharing in the request/guard first. This is a bounded,
explicit consumer restriction, not a claim that all host aliasing is illegal.
Device alias/lease validation remains the shared registry's responsibility.
Exact capacities/alignment/context and guard-bound range rules are not loosened.

## Trace And Verification

`AI_NORM_EXEC 1` rows use a consumer ID, sequence and source line last. Record
binding ownership/roles/registry handles, input and output copy extents, adapter
events, quiescence, retirement and release/refusals. `execution.log` begins
`# What it summarizes:` and must explain host versus imported-device paths,
which work completed, what remains pinned and why retry/cancel is required.
Collect it with compiled-entry/provider logs in `optimization.log`; summary must
separate dispatch/copy/lease enforcement, simulated test writes and real GPU
numerical/performance certification.

Reuse the authenticated compiler/entry receipts and their 193 mapped before/after
inspections; this runtime-only step does not change WHIRL. Test actual composition
with API mocks: all C0/C1/C2/four targets, host and borrowed device roots, mixed
placement, shared affine roles, exact offsets/canaries, cross-consumer conflicts,
stale handles, allocation/transfer/readiness/launch/drain/consumer/quiesce/free
failures, retries, cancellation and trace-off/reentry. Mock kernels may write a
sentinel to check movement; this is not LayerNorm numerical or GPU verification.

Next compiler work is selected host-PU call emission and driver/service packaging.
General compiler-session management and native GPU certification remain separate.

## Implemented Acceptance

The actual consumer, shared registry and CUDA adapter pass 22 scenarios for each
of 36 inherited selections: 792 scenarios, 30,224 consumer checks and 10,152
CUDA-API mock checks. Tests include independent outputs sharing input leases,
exclusive-output conflicts, partial/empty allocations, prelaunch cancellation,
cleanup-only open handles and true concurrent/reentrant calls. Host/mixed/device
paths preserve offsets/canaries; retries skip completed input/copyback work.

The old adapter and base-runtime suites pass 19,860 and 11,392 checks; all 72
raw regression traces match the authenticated prior entry receipt exactly.
All 129 focused Python tests pass. Leak-checked ASan/UBSan passes the same
consumer and regression matrices; 360 package/guard/trace/report files match
normal execution byte-for-byte. Provider rows are extracted from the consumer
trace into `provider.log`; reports can be reaudited without rerunning producers.

The only compiler-visible header change is an additive runtime snapshot
declaration. Rebuilding its seven BE/WOPT object consumers produces identical
bytes and no new runtime-symbol dependency; C/C++ guard ABI checks pass. The
authenticated parent compiler binary/images and all 193 mapped inspections
are reused. This is not a fresh WHIRL transformation or numerical/GPU execution.
Implementation `e72d3f4c` has an accepted final-path source rebuild/replay of
both runtime modes and all 129 focused tests. All 720 package/guard/raw-trace/
report comparisons are exact without normalization. The header compatibility
proof is authenticated from the primary receipt, not rerun by this replay.
The [progress ledger](AI-COMPILER-OPTIMIZATION-PROGRESS.md) records the derived
receipt and final-path replay. Review indexes/current links advance only after
successful replay; prior receipts remain immutable.
