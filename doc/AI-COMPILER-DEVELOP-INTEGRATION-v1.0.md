# AI compiler develop integration v1.0

November 13, 2026. This qualifies the affected integration boundaries for
[PR #268](https://github.com/open64-compiler/open64/pull/268), which promotes
the previously merged compiler review stack into `develop`. It preserves the
separate H20 review series and its existing immutable evidence.

The integration joins reviewed head `1dd178d10ecf686a95eda0c845b1fc806faf7953`
with develop `cacfce35de2c2423ecb07b882c8785afe907d5c5`. The conflict is in
`AI-COMPILER-OPTIMIZATION-IMPLEMENTATION-PLAN.md`: retain the complete
KVR0-KVR5 increment from develop together with all precision, feedback, WHIRL
architecture and historical queues in the compiler stack. No queue is discarded
or relabeled as implemented by the merge.

## Driver and test changes

The driver retains develop's image-gated CKKS2C terminal route and grouped,
all-PU atomic checkpoint publication. Unavailable POLY2C and invalid inputs
remain explicit refusals. The AI route retains LNO-owned estimation-only
search, native-state isolation, a single selected application, High/Mid/Low
verification, COMMENT explanations and existing host fallback. Protected search,
application, emitter and native-image implementation paths are unchanged from
the reviewed compiler head. No native layout or backend dependency is added.

Four production regression cases forward `-FHE:codegen=ckks2c|poly2c` on
non-FHE short/long MLA. Each preserves the complete host WHIRL view, publishes
one selected AI product and invokes no search in the selected helper. It never
enters the FHE terminal route.

The FHE route harness now uses numbered `.phase.t` paths. Input-stem `.t`
otherwise collides with canonical `.T` inspection files on a case-insensitive
host mount. Record the actual option vectors and recheck nonempty inspections
at the end, including after failed backend invocations. This fixes test-evidence
preservation; it does not change PREOPT COMMENT behavior.

Function-contract comments describe driver-owned output retirement and phase
lifetime. Rebuilding after those comments produces byte-identical `be` and
`be.so`; the completed native producer evidence therefore remains applicable.

## Affected qualification

| Boundary | Evidence |
| --- | --- |
| Active-source builds | Rebuilt backend, native tools/bridges and serving review library, WOPT, LNO, whirl2c, lw_inline, driver/openpy and native Python extension in isolated Linux/amd64 builds |
| Python tests | 196 pass: 70 shared/package/worker, 29 API/client/server, 51 Q7, 11 selector, 18 capture, 13 native search/reference and four partition-report tests; native lanes execute without skips |
| Bounded detached search | 37 search-control and 144 movement checks; rebuilt detached-object closure has zero native allocation/context/emission references |
| Production MLA | 28 scalar/legacy reference executions, 14 fallback cases, 10 manifest refusals, two malformed dependence-image refusals and multi-PU isolation; selected High/Mid/Low/PTX and generated CUDA COMMENT checks |
| Foreign-option isolation | Four short/long x CKKS2C/POLY2C option-forwarding cases; selected helper search count zero and complete host text unchanged |
| Quantized native routes | Dense int8 decode and paged int4 prefill source routes, reference outputs, canaries, immutable inputs and post-PREOPT fact rebinding; validation remains separate from compiler search |
| CKKS2C checkpoint | Two-PU miniature transaction: six atomic-publication cases and four artifact families |
| Cached FHE route/closure | Fresh 10-PU, 33,367-event route; 87 static descriptors and 147 dynamic bindings; terminal binary byte-identical to accepted D4; mock positive and four negative modes, occupied-output refusal, no POLY fallback and openpy option forwarding |
| Independent native inspection | 81 separate-process `ir_b2a -st -src` reopens; original archived model source mounted for cached inputs. Fourteen source-interleave corrections retain full raw diffs and require every native IR row unchanged |
| Required consumer boundary | be.so, lw_inline and selected bridge link closed; be.so/lw_inline have no DSL_Builder_/Json:: symbol leakage |

A broader audit sees the pre-existing optional OPEN64 VSA hook
`ETABLE::VSA_bottom_up_stmt` unresolved in the WOPT/LNO plugin context. The
same undefined symbol exists in the authenticated retained baseline. This
integration adds none, and actual WOPT/LNO production loading passes. General
VSA eager-link certification is not claimed or repaired here; the existing
required backend/shared-consumer gates remain strict.

## Evidence continuity and limits

The impact selector chooses backend/link, miniature checkpoint, route-option,
mapped-reopen, closure and selector lanes with no unimplemented required lane.
The cached complete CKKS input, D4/D5/D6 metadata and MLA input receipts are
authenticated before reuse. No ResNet materializer is rerun. Exploratory
infrastructure attempts remain distinguishable from accepted final checks.

Exact commands, input/product hashes, mapped binaries, source-interleaved
traces, full diffs and reports are retained under the primary review root at
`ai-optimization/research/pr268-develop-integration/`. The immutable receipt
and final-path audit identities belong in the PR description; `current` is a
convenience rather than an artifact identity.

This is affected software integration qualification. It does not certify every
historical milestone anew or establish NVIDIA execution/performance, encrypted
numerical inference or ACE provider admission. The authenticated ACE report
still says `CAPABILITY_MISSING`. AOT serving selection, OpenCTI and PREOPT
COMMENT repair remain deferred.
