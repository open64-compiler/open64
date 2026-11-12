# LN4 Selected Executable LayerNorm WHIRL

Version v0.1 | 2026-10-28 | Compiler-side bounded lowering

## Contract And Ownership

This extends [LN3 comparison](AI-COMPILER-LAYERNORM-PROFITABILITY-v0.1.md)
without changing source v1, binary WHIRL layouts or reduction code. Only explicit
LN-REF-1 v2 is eligible. There is no tree/Welford/approximate arithmetic path.

| Building block | Owner and responsibility |
| --- | --- |
| Source revalidation and selection | WOPT on the driver's active whole analysis PU; recompute LN3 and require exact agreement with the expected complete result. |
| Lowering handoff | Transient numeric recipe, exact guard, profile and selected costs; a corruption seal is not a security authority or mapped IR extension. |
| Device PU lifecycle | Driver creates/selects an empty kernel PU and supplies a valid source position; the lowering service never switches PU/symbol-table scope. |
| Executable materialization | WOPT lowering service constructs the selected ordered algorithm in that active device PU using standard WHIRL. |
| CUDA emission | Existing `whirl2c -cuda` prints the mapped device WHIRL, including kernel/shared attributes, loops, accesses, barriers and exact scalar ASM. It does not choose the algorithm. |
| Runtime | Existing guard/lease bridge plus an eventual production CUDA adapter enforce actual roots, subrange pointers, entry/grid/thread/shared limits and completion. A compiler artifact is not launch authorization. |

The analysis PU is preserved as provenance; this API materializes a separate
selected device implementation, not a host-side source replacement. Four pointer
formals are **X, gamma, beta, Y subrange starts**, not allocation bases. Canonical
backing offsets are applied once by the runtime/provider, never again in the
kernel. Input aliases are permitted; no restrict qualifiers are inferred.

LN3 common records remain analysis-only. LN4's explicit prepare API is the
reviewed transition to a lowering handoff, not a runtime execution grant. Any
later source/profile/geometry/environment change invalidates that handoff and
requires preparation again; the device builder must not restore the analysis
PU to perform hidden cross-PU analysis. The native driver fixture uses a fresh
device translation unit so analysis DSL formal metadata cannot leak into its
ordinary pointer-formal interface.

## Algorithm

```text
prepare(active analysis PU, source member, request, expected LN3 result):
    recharacterize/rebind and recompute complete C0/C1/C2 selection
    require byte-exact expected result, selected kind, complete selected cost
    copy exact selected guard, profile, cost and resource estimates to handoff
    preserve source image, tables, v1 and trace-off behavior

driver creates/selects an empty kernel PU
materialize(active device PU, handoff, device source position):
    preflight handoff integrity, target, strict FP controls, empty scope, bounds
    create four F32 pointer formals; one CTA owns one row
    create explicit thread/block coordinates and ordered high-level DO_LOOPs
    C0: lane zero performs mean, centered variance and direct epilogue
    C1: publish mean; cooperatively retain d; variance; publish q; epilogue
    C2: stage X; mean; publish mean; overwrite X with d; variance; q; epilogue
    keep every required CTA barrier outside lane/valid-element branches
    record actual body fingerprint, counts and action trace
driver writes standard mapped image; reopen, verify and translate with -cuda
```

## Storage And Arithmetic

One statically allocated shared F32 arena covers the LN2 reserved size. The row
occupies `[0, stride/4)`, mu is at `stride/4` and q at `stride/4+1`; padding is
not read. C2 overwrites X only after the ordered mean and mu-publication barrier.
C1 has four uniform barriers and C2 five, including final reader completion.
C0 has no shared arena or barriers. Entirely inactive warps still join barriers.

Every floating arithmetic operation uses the existing exact `*.rn.f32`
pointer-scalar ASM primitive. Three per-thread private F32 carrier slots package
operands/result; binary operations perform two stores, two ASM loads, result
store and unpack load, while sqrt uses four accesses. This realizes the LN3
carrier protocol, not a guarantee of physical cache misses, spill counts or
register placement. The target assembler may improve placement; analytical
timing remains conditional and must be calibrated/recosted for that result.
All division/sqrt remain explicit, FTZ/fast-math/contraction are unauthorized.

Mu and q are loaded once per active thread before cooperative feature loops,
allowing bank broadcast within each warp. Invalid elements perform no memory or
floating arithmetic. Global row indices and byte offsets use 64-bit arithmetic.
The initial lowering bounds rows to signed-I4 grid coordinates and retains the
existing exact count bound; unsupported geometry refuses before allocation.

## Verification And Reports

`lowering.log` must identify prepared source/target/choice, actual kernel and
formals, shared layout, loops, scalar carriers, barrier order, body fingerprint
and remaining obligations. `compile.log` records actual CUDA/PTX/cubin toolchain
results and flags, not GPU execution. `numerical.log` compares WHIRL-derived
CUDA control/access code under an explicitly substituted host scalar/CTA test
adapter against the independent exact-rational oracle. It is not GPU evidence.
`optimization.log` collects successful phase content; every log starts with
`# What it summarizes:`. Native compact `.t` rows retain source line last.

Before/after `.B` and full `ir_b2a -st -src` `.T` are mandatory for source and
device boundaries. Reopen the mapped device in a separate native process, check
the actual tree against its retained construction fingerprint, and reject
mutated barriers, arithmetic, addresses or qualifiers. The fingerprint proves
identity with construction, not algorithm correctness; independent structural
audits, host numerical execution and offline target assembly supply separate
evidence. No trace event is mislabeled as a source rewrite or GPU completion.

Offline compiled registers must not exceed the selected planning reservation,
and static shared bytes must match the selected reserved arena. Otherwise
publication/admission requires recosting, not a successful resource label. The
review entry manifest records subrange ABI, grid/block, dynamic/static shared,
compiled RF, exact guard and authoritative device-image digest. It is an input
to eventual provider construction, not an implemented CUDA launch adapter.

## Bounded Acceptance

Implementation `fa2e0b1e` passes 65 native cases / 45 selected kernels, 220 full
mapped dumps and 260 actual PREOPT controls. Native reopen, quiet materialization
parity and body/attribute/bound/effect mutations pass. All 45 WHIRL-derived
kernels compile to PTX/cubin with RF/shared-resource admission checks. The
emitted-code host adapter passes 360 cases / 43,464 independent-oracle output
checks. All 113 focused tests, 65 scoped compiler sanitizer cases, 45 sanitized
host kernels, 45 native guard invocations / 14,236 host checks and three shared
regressions pass. No whole-compiler sanitizer coverage is claimed.

Immutable final-path replay repeats these checks with retained source, compiler
binaries and CUDA 12.8.93 toolkit. Images, traces, guards, reports, PTX and cubins
are byte-identical; only the CUDA banner timestamp is normalized. The
[progress ledger](AI-COMPILER-OPTIMIZATION-PROGRESS.md) records primary/replay
identities. `physical-plan.log`, `bubble.log`, `roofline.log`, `lowering.log`,
`compile.log` and `numerical.log` are collected in `optimization.log`.

## Remaining Integration Gates

The explicit API requires real caller-supplied complete profiles and binding;
it does not silently turn unknown built-in timing or Python/source v1 into an
optimized kernel. Default driver dispatch, production CUDA adapter, actual
compiled RF admission, GPU numerical/timing verification and calibrated cache/
dispatch models remain separately reported. An altered carrier, launch geometry
or numerical recipe requires a fresh cost/binding decision.
Validation uses fresh `n64` compiler contexts. General in-process JIT integration
must explicitly establish the target data layout and compiler FP environment;
the explicit device builder is not a general service entry point. Wider
static/dynamic shared-memory mechanisms and opt-in configuration require their
own target/resource binding rather than assuming every legal SMEM reservation
can be emitted as one static CUDA array.
