# LN4 Compiled Entry And CUDA Provider Binding

Version v0.1 | 2026-10-29 | Bounded cubin adapter; GPU certification separate

## Ownership And Terms

| Term | Definition and owner |
| --- | --- |
| Selected contract | WOPT's source-v2 LN-REF-1 guard, plan/tree seals and RF reservation; WHIRL remains executable truth. |
| Compiled manifest | Trusted deployment descriptor binding that contract to one kernel name, exact cubin SHA-256, compiler-reported registers/static shared and modeled resident-CTA floor. A native C descriptor, not a mapped-WHIRL extension or portable wire ABI. |
| Package issuer | Offline tool that joins the retained request/selection/action/reopen evidence, emitted source and compiler resource report. Hashes identify bytes, not trusted authors or numerical certification. |
| CUDA adapter | Separate optional runtime component that owns one loaded module, resolves the exact entry, checks actual resources and dispatches through the existing normalization registry. No compiler or reduction linkage. |
| Deployment hooks | Trusted SHA-256, input-readiness and result-consumer services. They cannot throw; the SHA service hashes the adapter-owned cubin copy. Hooks/context/stream outlive the adapter. |
| Ticket | Existing runtime-owned atomic input/read and output/write leases. The adapter retains each ticket until successful close; module destruction refuses while any ticket remains. |

The existing driver-review manifest remains review-only and unchanged. Packaging
does not lower code or choose plans. The issuer must be trusted; editing both
code and its digest is not prevented by hashing. C0/C1/C2 have the same four
64-bit pointer formals X/gamma/beta/Y and no dynamic shared allocation.
The review packager produces `entry.json`, a C++11 native-descriptor initializer
`entry.h`, exact `kernel.cubin` and an explicitly host-native `selected.guard`
fixture. It rejects assembly with spills or more than the current 16-byte
private-carrier stack reservation. Neither the guard fixture nor the initializer
is a new portable deployment/wire format.

## Admission Algorithm

```text
open(trusted manifest, cubin bytes, caller context/stream, hooks):
    validate bounded descriptor, exact selected-entry name, v2/static contract
    copy cubin; require SHA256(owned copy) == manifest.cubin_sha256
    require initialized CUDA >= 12.8 and caller context current
    require explicit stream belongs to that context and expected device
    require exact compute capability (9.0 or 10.0); module load checks suffix ISA
    load the owned cubin and resolve only manifest.kernel
    query compiled registers/static shared, binary version, max threads
    require exact compiled resources and RF <= planning reservation
    require no mandatory cluster; four pointer parameters and no fifth
    query occupancy at selected threads and zero dynamic shared
    require occupancy >= modeled resident-CTA floor; otherwise return for recost
    publish ready adapter only after all checks

admit(adapter, runtime, canonical roots):
    runtime atomically validates exact guard, provider limits and range leases
    keep private adapter/runtime/ticket association; never expose a raw entry

perform(event, invocation):
    require original CUDA context/stream still current and invocation bound
    READY: deployment hook waits for inputs (no kernel work)
    LAUNCH: check actual device-pointer context/type/device/extent
            launch exact entry with subrange X/gamma/beta/Y pointers,
            (rows,1,1) grid, (threads,1,1) block, dynamic shared = 0
    DRAIN: synchronize original stream, including ambiguous failed launches
    PUBLISH: deployment hook waits for result-consumer completion

close(adapter, runtime, ticket):
    forget association only after existing runtime close succeeds
destroy(adapter):
    refuse live tickets; unload only in the original current context
```

No implicit C0 retry or resource-driven plan substitution. A changed resource
observation returns a refusal for explicit recost/recompilation. Occupancy is a
resource check, not proof that the timing profile is calibrated to the device.
Readiness/cache behavior and GPU accuracy/performance remain separate gates.

## Lifetime And Failure Rules

Externally synchronize open/destroy and all object lifetimes. Runtime APIs keep
their existing busy/idempotence/drain rules. Use adapter admit/close wrappers;
closing a ticket directly through the base API can leave a conservative adapter
reference that blocks destruction, never permit an early unload. The caller must
not destroy/rebind streams, contexts or live registered allocations out of band.
Callbacks run without the adapter mutex, permitting safe runtime reentry.

If open acquired a module and rollback unload fails, return a cleanup-only
adapter handle with failure status. No admission is allowed on it; retry destroy
in the original context. Failed destroy preserves the handle. Failed launch
retains leases until drain/abort; consumer failure retains output until joined.
Production and HOST_TEST registry flags must match. Mock CUDA calls are not GPU
execution, and successful packaging never changes `gpu_verified` to true.
The SHA-256 callback is a trusted deployment service to avoid linking compiler
or cryptographic policy into this optional adapter. Tests use an exact-byte
oracle for the known, independently Python-SHA256-checked cubin, not a production
hash implementation. A real integration must supply its reviewed crypto hook.

## Evidence And Reports

`compiled-entry.log` must summarize issuer/evidence identity, selected source and
entry, hashes, compiled RF/static shared, resident floor and remaining gates.
`provider.log` summarizes live context/device/resource observations, admission,
launch/drain status and module/ticket lifetime. Both begin `# What it summarizes:`.
Compact `AI_NORM_CUDA 1` records keep source line last. Preserve original `.t`,
requests/binding, `.cu`, cubin/PTX, full `.B`/`-st -src` `.T` and raw diff through
an authenticated parent receipt. No compiler producer rerun is required when its
inputs and code are unchanged. New runtime/package evidence is a derived receipt.
Rows use `provider seq action status a b source_line`. `attribute` carries the
CUDA attribute ID and observed value; `resources` gives RF/static shared and
`resident` gives observed/modeled CTA counts. Module-load/resolve/cuda-error
statuses are raw CUresult; other actions use the existing normalization status
codes. Failed parameter, device, occupancy and module stages remain distinguishable.

## CUDA Mapping

Use the CUDA 12.8 driver API, not a guessed launch ABI:
[module loading and lookup](https://docs.nvidia.com/cuda/archive/12.8.1/cuda-driver-api/group__CUDA__MODULE.html),
[function attributes, parameter layout and launch](https://docs.nvidia.com/cuda/archive/12.8.1/cuda-driver-api/group__CUDA__EXEC.html),
[occupancy](https://docs.nvidia.com/cuda/archive/12.8.1/cuda-driver-api/group__CUDA__OCCUPANCY.html).
`SHARED_SIZE_BYTES` is static shared only; launch dynamic shared stays zero.
`MAX_THREADS_PER_BLOCK` is entry/device dependent. Actual cubin load checks
accelerated suffix compatibility; compute capability alone does not prove it.

## Acceptance Queue

1. Package exact selected artifacts without changing v1, driver JSON or WHIRL.
2. Compile the real adapter against retained CUDA 12.8 headers; link host tests
   to deterministic CUDA API mocks, not replacement adapter logic.
3. Exercise all C0/C1/C2 selections, resource/context/digest/parameter failures,
   exact launch arguments, ambiguous launch/drain, consumers and live-module pins.
4. Authenticate parent mapped evidence and audit derived trace/package outputs;
   retain focused normal/sanitizer runs separately from GPU certification.
5. Native driver/module/pointer/launch verification and paired GPU numerical/
   performance measurements remain explicitly open until a GPU is available.

## Implementation And Compiler-Side Acceptance

The optional `Makefile.normalization_cuda` builds a runtime-only static library
from `normalization_cuda.cxx` and the existing normalization registry. Link an
application against the CUDA Driver library; the compiler backend gains no CUDA,
JSON or runtime dependency. It is not installed by the legacy runtime makefile.

The 36 inherited selections cover C0 4 / C1 4 / C2 28 across four exact targets.
The completed host-mock matrix passes 19,860 adapter checks, 11,392 independent
base-runtime checks and 124 focused Python tests. ASan/UBSan with leak checking
passes the same runtime matrix; all 288 package/guard/raw-trace/report files
match normal execution byte-for-byte. Refusals cover resource/context/image/
parameter mismatches, unsupported descriptors, failed rollback/unload, ambiguous
launch/drain and failed consumers; callbacks test reentry and module pins.
Trace-off dispatch follows the identical contract and ordered host protocol.

The compiler producer is unchanged. The authenticated LN4 request receipt keeps
all 193 mapped images/full source-interleaved dumps plus before/after diffs,
device CUDA/PTX/cubins and numerical evidence. New acceptance is runtime/package
evidence, not a fresh compiler transformation or GPU numerical/performance run.
See the progress ledger for derived immutable receipts and final-path replay.

Follow-on, 2026-10-29: the
[host/service execution consumer](AI-COMPILER-LAYERNORM-EXECUTION-CONSUMER-v0.1.md)
now composes this adapter with shared registry roots, target-buffer callbacks,
host copies and readiness/consumer/quiescence hooks. It is a runtime library,
not selected host-PU call emission or an installed deployment provider. Those
compiler/driver integration steps, reviewed deployment services, general
compiler-session lifecycle and native GPU certification remain separate.
