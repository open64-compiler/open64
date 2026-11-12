# LN4 Driver And JIT Request/Profile Binding

Version v0.1 | 2026-10-28 | Explicit compiler checkpoint, not runtime launch

## Boundary And Ownership

This integrates the [LN4 selected-device contract](AI-COMPILER-LAYERNORM-EXECUTABLE-LOWERING-v0.1.md)
without changing v1, reduction code or binary WHIRL layouts. It is opt-in:
`-DSL:normalization_request=<JSON>:normalization_output=<fresh.B>:gpu_arch=sm_90`.
Require `-O3`, `-TARG:abi=n64`, and
`-OPT:roundoff=0:IEEE_arithmetic=1:fast_math=off`. O0/O1/O2 refuse this
performance-oriented shared-memory/parallel realization rather than silently
enlarging their optimization scope. Ordinary compilation remains unchanged.

| Concept | Definition and owner |
| --- | --- |
| Request | Owned source snapshot, target/environment declarations, provisional backing ranges, scalar timing profile and RF/budget estimates. Not proof of live buffers or a selected plan. |
| Analysis PU | Driver-selected source procedure. PREOPT verifies canonical form; WOPT finds exactly one requested member and revalidates every source identity. No cross-PU inference. |
| Driver checkpoint | Driver retains resident PUs and owns device PU creation, context switching, mapped-image writing and publication. |
| Typed JIT boundary | `WOPT_DSL_NORMALIZATION_REQUEST` plus the WOPT driver API. A service may construct this request directly; the same prepare gate is used after canonicalization. This is not yet a compiler session manager or runtime adapter. |
| Selected handoff | Existing LN4 pointer-free numeric plan/guard/profile/cost, prepared only after a complete legal LN3 selection. Same-process/native contract, not a persisted binary ABI. |
| Device PU | Empty kernel created by the driver, then filled by the existing active-scope LN4 materializer. WHIRL remains the only executable truth. |
| Output image | Analysis PUs plus the selected device PU in one ordinary mapped image. Keeping all source interfaces avoids invalid partial copies of managed DSL metadata. CUDA emission filters device entries. |
| Runtime contract | Provisional context/root/range/lease claims. Production providers must independently enforce them; compiler binding never discharges runtime obligations. |

The WOPT library publishes callbacks through Open64's existing init/weak-pointer
mechanism. Header-only repository RapidJSON is compiled inside WOPT only; no
new backend shared-library dependency or frontend builder dependency is added.
Compiler-session admission precedes PREOPT: host rounding must be nearest and
SSE hosts must independently have nearest rounding, masked exceptions and
disabled FTZ/DAZ. Checking x87 `fegetround()` alone is insufficient. The service's FP
state is never changed by admission. A JIT caller must establish a supported
session before decoding/preparing; this API is not a session initializer.
JIT integration still enters through the binary-WHIRL frontend boundary and
derives its source snapshot with native characterization, not Python/debug-name
inference. Host ports beyond the tested x86 Linux compiler environment need
separate FP/session validation.

## Request Schema

`open64.layernorm.request.v1` is a closed JSON object, bounded to 1 MiB.
Reject duplicate/unknown/missing keys, invalid UTF-8, embedded NUL, negative,
floating or overflowing integer fields, extra array elements and malformed JSON.
Decode is transactional and context-free; it does not create optimization records.
Native source IDs bind to the accompanying image, not to a portable source name.

| Member | Exact content |
| --- | --- |
| `schema` | `open64.layernorm.request.v1` |
| `source` | All fields of native normalization characterization: recipe, flags, owner_pu, node_id, operand_value_ids[3], result_value_id, operand_types[3], result_ty, rows, width, epsilon_bits, source_line. No partial identity matching. |
| `target` | architecture, route, toolkit, ptx, environment, fp_options. Architecture must equal the explicit driver `gpu_arch` option and the profile architecture. Toolchain numbers remain declarations until offline compilation. |
| `invocation` | runtime_contract_id, context_id, launched_ctas, threads_per_cta, ranges[4] ordered X/gamma/beta/Y. Each range has allocation_id, allocation_bytes, offset_bytes, base_alignment, lease. |
| `profile` | version, arch_id, description_revision, model_id, fields[19]. Each ordered field has id, provenance and bounds[lower, nominal, upper], using `dsl_hardware_performance.h` field IDs and units. |
| `registers_per_thread` | Three upper reservations in C0/C1/C2 order, not compiled RF measurements. |
| `max_jobs`, `margin` | Per-candidate DAG expansion budget and minimum robust benefit in reference cycles. |

Profile provenance: 0 unknown, 1 estimated, 2 caller-supplied measured, 3 synthetic.
The compiler preserves these declarations; it does not authenticate measurements.
Unknown built-in timing, stale architecture revisions, invalid intervals, budget
exhaustion and arithmetic overflow never produce a selected executable plan.
Synthetic fixtures must remain visibly synthetic, not installed hardware defaults.

## Algorithm

```text
driver:
    require paired explicit request/output options, fresh output, O3/n64
    forbid conflicting checkpoint/device-output modes
    load WOPT through its existing component protocol
    read bounded JSON; decode into owned typed request
    require gpu_arch == request.architecture
    load and retain PUs through the existing resident-program service
    select exactly the requested analysis PU; establish its scope/map/context
    initialize REGION state; invoke existing PREOPT; finalize REGION state
    WOPT.prepare(active canonical PU, request):
        find exactly one native member matching node_id
        rebind the full expected source snapshot; v1 remains ineligible
        derive fixed C0/C1/C2 storage/event recipes from target and geometry
        evaluate complete LN3 costs, select, and revalidate LN4 handoff
        require n64, strict arithmetic and supported static-shared reservation
        return selected numeric handoff and actual source position
    create/select a new empty kernel PU without resetting source metadata
    isolate CLI before-image writing in a child; standard writing consumes SSA
    materialize and verify using existing LN4 APIs
    retain after image including unchanged analysis PUs and selected device PU
    publish only complete artifacts without replacing existing paths
```

Use an output such as `selected.after.B`; the companion is
`selected.before.B` and the review binding is `selected.after.B.manifest.json`.
The whole-program resident loader supports flat PUs only; nested PUs refuse.
Input images with existing device entries or unsupported auxiliary PU sections
(dependence/prefetch/alias/feedback/REGION maps, obsolete sections) refuse before
optimization rather than risking stale offsets or dropped payloads. Ordinary
tree/symbol/SSA sections are supported. All retained physical-formal interfaces
are validated before analysis and after kernel-row insertion; a source file
with formals but no managed coverage needs a separate interface-completion path.
The CLI snapshot uses POSIX fork, whereas typed JIT preparation does not fork
or perform file I/O. This is not an in-process concurrent service compiler.
New device scopes explicitly zero their reserved local-table row zero before
writing. `New_Scope` reserves those nonsemantic rows but does not initialize
them; the writer includes their storage. This avoids allocator-dependent bytes
without changing table layout or normalizing any existing input scope.

Storage/event derivation stays in WOPT: one CTA per row, ordered lane-zero
statistics, contiguous cooperative tails, C1 retained d, C2 X-to-d reuse,
four/five uniform CTA barriers and the conservative event chain. The request
cannot force a selected candidate, omit barriers or grant reassociation.

The first driver path accepts selected static arenas up to 48 KiB. Larger
static/dynamic/opt-in paths need explicit resource binding; do not substitute a
different candidate after selection. Actual assembled RF/shared/resource
admission remains a separate compilation/provider gate.

## Trace And Review Artifacts

The existing `-DSL:ai_opt_trace` and TP_WOPT2 `0x00800000` control compact `.t`
rows. `AI_NORM_REQUEST 1 BIND/SELECT/PUBLISH` joins native source, architecture,
profile model/revision, selected kind, handoff and final device image. Source
line is last. Existing LN3 cost/selection and LN4 materialization/action rows
remain authoritative. Trace-off cannot change selection or mapped code.
Publication-path bytes outside ASCII alphanumerics and `/._-~` are percent
encoded, so spaces/control bytes cannot change positional columns.

`request.log` starts with `# What it summarizes:` and must report request
admission/refusal, exact source/target/profile identity, timing provenance,
optimization-level/environment gates, selected candidate/cost/reason, publication
and remaining runtime/assembly obligations. Collect it with `physical-plan.log`,
`lowering.log` and full `optimization.log`; `optimization-summary.log` must
distinguish disabled, refused, selected, materialized, offline-compiled and
GPU-unverified states rather than interpreting a selected cost as execution.

Retain source inputs, request JSON, full `.t`, before/after `.B`, independent
`ir_b2a -st -src` `.T` and raw diff, emitted `.cu`, compiler streams and command
lines. Reopen the image in another process and verify the actual device body;
include mixed v1/v2, trace-off, malformed/stale/unknown profiles, low optimization
levels, aliases, wrong target/ABI/FP controls, existing output and independent
PU controls. GPU validation and service-session concurrency remain open.

Host exception-mask testing must check readback: the current x86 Linux emulation
ignores attempts to unmask SSE exceptions (`1f00` reads back as `1f80`). Such a
negative test is reported UNSUPPORTED, not passed. The production gate still
requires masked exceptions. Rounding, FTZ/DAZ, pointer-layout and unsafe-compiler
option refusal tests remain executable on this host.

## Compiler-Side Acceptance

The reviewed implementation passes 36 selected cases across `sm_90`, `sm_90a`,
`sm_100` and `sm_100a`: C0 4, C1 4, C2 28. All 66 refusal cases pass, including
v1, unknown/stale profiles, all 19 unknown timing fields, bad ranges/aliases,
levels, ABI/FP controls, incomplete formal metadata, existing device images,
nonempty auxiliary sections and occupied/dangling output paths. Trace-off
preserves images and binding manifests; whitespace paths retain trace columns.
All 193 retained images have full independent `-st -src` dumps.

All 36 CUDA kernels emit from WHIRL, compile offline to PTX/cubin and fit their
declared RF/static-shared reservations. The emitted-code host adapter passes
288 datasets / 26,240 exact-oracle output checks. All 117 focused Python tests
pass. Scoped ASan/UBSan passes 36 native request cases, 36 backend driver cases,
36 mapped reopens, 49 decoder inputs and 36 emitted-code host kernels with the
same output checks. Sanitized images/bindings match the ordinary artifacts
byte-for-byte. This is not whole-compiler sanitizer or GPU verification.

Five focused shared-contract regressions pass. Two broader fixtures (generic
program-interface evolution and production-native mapped publication) fail
identically when only the pre-change gatekeeper object is substituted; their
isolation evidence is retained, and neither is counted as a passing regression.
The optional pre-existing WOPT/VSA unresolved symbol is unchanged. Backend
consumers remain link-closed and dynamic library dependencies are unchanged.
Immutable retention/final-path replay is recorded in the progress ledger.

Implementation `236f52eb` is accepted with final-path replay: 193 image pairs
and 36 PTX/cubin pairs match byte-for-byte. Full dumps/reports normalize only
recorded artifact paths and the CUDA banner; raw `.t` equality compares stable
AI rows while retaining complete diagnostics. A separately retained comparator
correction handles the intentionally invalid-UTF-8 JSON fixture as exact bytes;
it does not rerun or reinterpret compiler results. Scoped sanitizer evidence is
authenticated from the primary package, not rerun in this replay. The
[progress ledger](AI-COMPILER-OPTIMIZATION-PROGRESS.md) records both immutable
receipt identities and the next production-provider gate.
