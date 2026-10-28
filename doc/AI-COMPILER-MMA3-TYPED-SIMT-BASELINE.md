# MMA3 Typed SIMT Baseline

## Contract And Scope

Provide a native, active-PU lowering service for rank-two row-major F16 or
BF16 input buffers and F32 output, with explicit F32 product and accumulation.
The caller supplies an accepted `DSL_NUMERICAL_CONTRACT_RECORD`, positive
static M/N/K, disjoint output storage, owning PU, generation and acceptance
identity. A/B may alias each other; neither may overlap output. The initial
service is an independent-CTA direct SIMT baseline, not a profitability
selector, typed TMA/staging implementation or native MMA implementation.

Reuse the existing U2 scalar storage carrier for both 16-bit formats; the
numerical contract distinguishes their interpretation. Widen each loaded
element exactly, never reinterpret a 16-bit buffer as an F32 buffer. Accumulate
in increasing K order from positive F32 zero and write F32 output. There is
no padding, low-precision partial sum, implicit source narrowing or narrowing
of output. Even when reassociation is permitted, this baseline does not use
it. Volatile private F32 product/accumulator objects make the separate product
rounding and each accumulation step explicit and prohibit contraction across
the product store. Rounding is round-to-nearest-even with gradual underflow;
fast-math/FTZ execution is outside this contract. NaNs/infinities and signed
zeros follow that arithmetic; NaN payload propagation and floating exception
flags are not promised. Exact bit widening is tested separately.

The source baseline is a canonical serial M/N/K WHIRL function with the same
symbols and numerical contract. SIMT materialization replaces only its body:
row=blockIdx.y; column=blockIdx.x*threads+threadIdx.x; guarded output ownership;
the same increasing-K scalar arithmetic. The native source fixture and lowerer
use Open64 symbol/type, WN construction, source position and mapped-image
services. No independent CUDA source template performs this transformation.
Launch with exactly the declared X thread count, unit Y/Z thread dimensions
and unit Z grid dimension. X/Y blocks cover ceil(N/threads) by M; excess X/Y
blocks and edge lanes are guarded. Coordinates widen to I8 before arithmetic.
Callers supply naturally aligned, sufficiently sized buffers. The validated
implementation requires the existing 64-bit pointer ABI; other pointer widths
are rejected before address construction.

This API is the native compiler baseline needed by later MMA recipes. It
does not widen `common.matmul.v1/v2` result-dtype rules, infer new Python/FX
capture contracts or enable typed dispatch through the existing F32 D/S1/T2
planner. Those existing paths remain unchanged and continue rejecting the
wrong storage/precision. Connecting a native MMA recipe to this baseline and
its launch/guard ABI belongs to MMA4/MMA6, not an invented runtime fallback.

## WHIRL Continuity

No new operator, mtype, type kind, escape encoding, mapped-image table/section
or runtime library ABI. Use standard U2 ILOAD, integer bit operations and
control flow for widening, F4 TAS for the resulting IEEE bit representation,
F4 MPY/STID/LDID/ADD for arithmetic, and F4 ISTORE for output. Each operand is
a fresh WN tree. Constructors retain the standard `wn_simp_code.h` behavior
and honor its master switch; there is no private expression simplifier.

The checkout does not contain the referenced `WHIRL.pdf`. Reviewed the Open64
[WHIRL specification](https://www.mcs.anl.gov/OpenAD/open64A.pdf), sections
1.13 (CVT changes representation while preserving value; TAS preserves bits),
1.15 (typed arithmetic/selection), 1.5/1.6 (structured loops and loads/stores)
and the current WN/type implementation. Do not treat a numeric C cast as TAS.
The CUDA emitter maps an explicit U4-to-F4 TAS to `__uint_as_float`; this is
target spelling of an existing WHIRL operation, not a lowering decision.
The host test shim uses memcpy for that builtin. No new backend dependency.
The ordinary simplifier may turn masked shifts into U4 EXTRACT_BITS. Reuse
the existing extraction translator, including field bounds, and emit CUDA U4
shifts with the existing `whirl2c.h` unsigned/masked-count semantics rather
than references to undefined host helpers. Non-CUDA translation is unchanged,
apart from correcting host-int overflow in the extraction mask calculation.

The gatekeeper now distinguishes ordinary raw-pointer/scalar formals from
tensor or registered source/runtime-projected formals. Only the former lack
DSL tensor escape restrictions; tensor types, registered interfaces and all
projected formals retain those restrictions. This is required for canonical
typed source bodies to pass the ordinary backend, not a DSL admission bypass.
Every complete source body ends with canonical RETURN; O2/O3 execution tests
guard against losing the final output store through malformed function exits.

F16 widening normalizes subnormal mantissas with integer operations, preserves
sign, and maps exponent/mantissa bits to F32. BF16 widening shifts all sixteen
bits into the high half of F32. No floating arithmetic is used in conversion,
so input subnormals/zeros and NaN bit patterns are not accidentally rounded.

## Verification And Evidence

Preflight rejects unknown/mixed/narrowed contracts, bad scope/identity, wrong
buffer/temporary types, private-symbol aliasing, invalid coordinates, malformed
geometry and unsafe address extents. Failure leaves caller output and input
tree untouched. The caller owns insertion/publication after structural
verification. Postchecking compares the complete canonical body, including
loads, byte scaling, widening, K order, product boundary, accumulator, guards
and final store, against the same accepted source contract. It does not infer
correctness from a trace label or numerical tolerance alone.

Retain independently reopenable before/after `.B`, `ir_b2a -st -src` `.T`,
full diffs, WHIRL-derived `.cu`, source/numerical reports, malformed mutations,
host numerical evidence and commands. Cover both formats, exact-divisible and
tail shapes, underfilled/extra threads, exhaustive sixteen-bit widening,
cancellation, subnormals, signed zeros, infinities, NaNs, overflow and cases
that distinguish F32 accumulation from 16-bit accumulation or FMA. Compile
the actual emitted sources at multiple host optimization levels. This is
compiler/host evidence, not GPU instruction, allocation or timing certification.

Rebuild backend shared consumers and inspect symbols. Re-audit the immutable
MMA2 F32 receipt against fresh F32 D/S1/T2 production artifacts: old cost rows,
selection, full reopened WHIRL text and CUDA bodies must stay identical. Raw
image hashes/difference offsets are retained, not normalized or claimed equal
when pre-existing unprinted storage bytes differ. Timing remains unknown;
no MMA promotion or compute-selection option is added. Commit locally only.

## Validated Local Boundary

The final source is based on merged MMA2 PR196, develop `a1153671`.

| Gate | Result |
| --- | --- |
| Real same-PU serial/SIMT mapped artifacts | 18 configurations, both formats; exact/tail/multi-CTA/long-K/wide-CTA, simplifier on/off and O0/O3 construction. |
| Actual emitted-code/backend execution | 88 runs: 72 WHIRL-derived CUDA-host O0/O3 runs and 16 Open64 O0/O1/O2/O3 native-kernel runs. |
| Numerical coverage | All 65536 encodings per format widened exactly; 12 adversarial patterns on non-exhaustive cases, immutable inputs, output guards and a BF16 witness separating rounded product/add from FMA. |
| Malformed source/body coverage | 468 preflight rejections and 216 changed-body rejections; rejected materialization/inspection preserves caller output and postchecking does not grow symbol tables. |
| Host regressions | 117 tests pass on Python 3.9.6. |
| F32 compatibility | Authenticated all 1064 MMA2 receipt files; 15 fresh compiler configurations have 30 byte-identical full `-st -src` dumps, unchanged cost/selection/compute/shadow rows and unchanged CUDA bodies except translator date. Raw images have recorded nonprinted byte differences, not a byte-equality claim. |
| Reader compatibility | 16 independent processes: six old-reader/new-F32, six new-reader/old-F32 and four old-reader/new-typed; all full dumps identical. |
| Existing related contracts | MMA1 four profiles/276 negatives, MMA2 496 schedules/1488 oracle corners, scalar accumulation before/after, scalar PU ABI, managed runtime projection and native-to-standard lowering pass. |
| Shared boundaries | Rebuilt `be.so`, `be`, `lw_inline`, `whirl2c`, `ir_b2a` and linked native tests; no `DSL_Builder_*`/`Json::` symbols in backend/shared consumer; syntax/operator-layout matrix and six-case miniature CKKS2C checkpoint pass. |

Two isolated existing builder modes still fail: production-native mapped-image
finalization and program-interface projection dominance. Both reproduce with
the unmodified develop gatekeeper object and identical diagnostics. The
monolithic builder invocation also has a DST double-initialization failure;
passing isolated managed-runtime/standard-lowering/ABI tests are not a claim
that this full suite passes. No unrelated failure is repaired here.

Impact selection is conservative for unclassified MMA files. The only shared
gatekeeper change is unmanaged canonical formal classification; the managed
interface tests, syntax/layout matrix, reader compatibility, shared-link gates
and miniature checkpoint are the focused evidence. This is not full FHE model
recertification. Native GPU arithmetic, FTZ controls, launch/resource allocation,
instruction assembly and timing remain unverified.

Representative BF16 after-image evidence: guarded ownership (`IF`), two
`U4U2ILOAD`/`F4TAS` pairs, one `F4MPY`, volatile product and accumulator stores,
one `F4ADD`, one `F4ISTORE`, and explicit `RETURN`. The matching source has U2
input pointers, F4 output and no hidden low-precision accumulation or runtime
matrix kernel.

## Durable Acceptance Record

Local implementation commit: `558fcb8b73e9dbf587862e7a7ca4851ea735f5e8` on
`codex/ai-mma3-typed-simt`, from develop `a1153671`. No push or PR.

Primary immutable receipt (1790 authenticated files):

`/Users/shinmingliu/open64/artifacts/review/ai-optimization/mma3/typed-simt/sha256-6830985cb5a1e8999730a5904c20ad9f840683d7eb1b9a85de5b2b8b98aaaaa2/`

- [Acceptance summary](../artifacts/review/ai-optimization/mma3/typed-simt/sha256-6830985cb5a1e8999730a5904c20ad9f840683d7eb1b9a85de5b2b8b98aaaaa2/summary.log)
- [BF16 guarded SIMT trace, with symbols and source](../artifacts/review/ai-optimization/mma3/typed-simt/sha256-6830985cb5a1e8999730a5904c20ad9f840683d7eb1b9a85de5b2b8b98aaaaa2/acceptance/typed/bf16-tails/typed.after.T)
- [F16 same-source before/after diff](../artifacts/review/ai-optimization/mma3/typed-simt/sha256-6830985cb5a1e8999730a5904c20ad9f840683d7eb1b9a85de5b2b8b98aaaaa2/acceptance/typed/f16-tails/typed.before-after.diff)
- [Typed source/precision/fetch/physical review](../artifacts/review/ai-optimization/mma3/typed-simt/sha256-6830985cb5a1e8999730a5904c20ad9f840683d7eb1b9a85de5b2b8b98aaaaa2/acceptance/typed/bf16-tails/reports/optimization.log)
- [Authenticated MMA2/F32 compatibility and raw-byte differences](../artifacts/review/ai-optimization/mma3/typed-simt/sha256-6830985cb5a1e8999730a5904c20ad9f840683d7eb1b9a85de5b2b8b98aaaaa2/acceptance/parity/report.json)

These relative links resolve from the primary checkout; implementation
worktrees must use the absolute durable root above, not a temporary copy.

Independent durable replay receipt:

`/Users/shinmingliu/open64/artifacts/review/ai-optimization/mma3/reopen/sha256-e30cd38736eb53464a1c68ac768762046e9c2f233ab9e1542fd4b94a42be703d/`

Replay authenticates all 1790 primary files, independently re-audits all 18
source cases, reruns the copied native producer for both formats, reopens four
fresh images with both `-st` and `-src`, and executes four freshly emitted CUDA
host kernels. Native traces, mapped images and full reopened dumps match;
no original build directory is mounted. Scratch launcher aliases restore the
standard `whirl2c_be -> be`/shared-library topology from retained binaries;
the replay receipt retains the exact setup script. Both receipts are indexed
under the primary durable review root. Native GPU/timing remains unknown.
