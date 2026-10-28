# AI Compiler Optimization Progress

Snapshot: 2026-10-21, reviewed against base commit `e17a2c23`
Scope: Open64 AI optimization, with standalone rank-2 F32 GEMM as the first
generated-kernel case. This is a running status ledger, not a replacement for
the [implementation plan](AI-COMPILER-OPTIMIZATION-IMPLEMENTATION-PLAN.md) or
the [versioned design](AI_compiler_optimization_design_v0.3.md).

## Current position

2026-10-28 R2-c2 staged ordered reduction binding:
WOPT now binds a synchronous bit-copy realization using one disjoint padded
CTA-shared slot per row-thread. Complete-copy visibility precedes ordered reads;
last-read release precedes next-generation reuse. Column tails and partial CTA
owners use exact counts/guards; one positive-zero FP32 accumulator persists
across chunks. Source v1, unsupported visibility/async/cooperative copies,
multiple slots, unsafe reuse and padded tails are refused.
Eight native cases pass 539 assertions and audit 254 binding rows (12 conditional
successes, 242 refusals) plus 181 exact slice plans. Forty-six host row witnesses
match the independent integer binary32 oracle. All 42 trace/auditor tests pass,
including five staged tests and 2,050 event-graph comparisons. Direct binding/
runtime, numerical recipes, Softmax/normalization, WOPT/common/FHE focused and
backend/inline link-boundary regressions pass.
See [staged binding detail](AI-COMPILER-REDUCTION-STAGED-BINDING-v0.1.md).
Target resource data remains the existing estimated planning envelope; staged
mask 255 remains outstanding. No runtime ABI or MMA1/profile/schema change,
selected WHIRL transformation, native copy execution or GPU claim. Next is
complete legal direct/staged costs and explicit profitability-gated selection;
staged runtime adaptation and selected lowering remain separate open gates.

2026-10-27 R2-c1 direct ordered reduction runtime enforcement:
fresh native binding export now feeds a separate runtime guard ABI. The runtime
reuses the tensor descriptor and buffer-service import checks, requires trusted
canonical backing-root resolution, acquires input/output range leases atomically,
and enforces all readiness/launch/completion/consumer/release edges. Failed launch
requires proven drain before abort; failed waits retain storage. No implicit
fallback or result publication after ambiguous launch failure.
Eight native cases pass 320 assertions; six exported v2 guards each pass 176
host-runtime assertions with trace on/off, repeated under native ASan/UBSan.
The six normal runtime traces contain 78 admitted/closed tickets, 222 rejected
admissions and 30 injected backend failures; 37 trace/auditor tests pass.
Prior reduction, Softmax, normalization, WOPT/common/FHE focused regressions,
buffer-service V1 compatibility and backend/inline symbol boundaries pass.
Linux/QEMU ASan register-inspection warning is retained as an unsuccessful lane;
native macOS sanitizer runs pass instead. No GPU is available.
See [runtime guard detail](AI-COMPILER-REDUCTION-RUNTIME-GUARDS-v0.1.md).
This is host-mock runtime enforcement, not production CUDA adaptation or a
selected executable reduction. Next: staged copy/reuse binding, complete costs,
selected WHIRL loops/dispatch, then target adapter and native certification.
MMA1, hardware profiles, existing ABI layouts and Python v1 semantics are untouched.

2026-10-27 R2-c direct ordered fallback binding:
WOPT now validates an explicit scalar ISA/compiler-policy contract using the
existing exact Hopper/Blackwell architecture lookup, without MMA profile
dependencies. Canonical buffer-object/subrange contracts, checked FP32 byte
footprints, lease kinds, linear row ownership and six-role host event DAGs
are validated read-only against freshly derived native source facts.
Eight native fixtures execute 274 assertions and audit 212 direct-query rows;
18 are conditionally bound contracts, while all refusals retain explicit
reasons. Actual PREOPT preserves the source contract. The trace/auditor suite
passes 34 tests, including eight new direct-binding tests and 2,048 independent
event-graph comparisons. Prior reduction/Softmax/normalization and focused
common/FHE/WOPT regressions and backend/inline symbol checks pass.
See [direct target/storage/event binding](AI-COMPILER-REDUCTION-DIRECT-BINDING-v0.1.md).
Runtime allocation/lease/event/launch obligations remain required; no adapter,
guard discharge, selected lowering, PTX/SASS certification or GPU run is claimed.
The existing NVISA strict-add intrinsic route is recorded for later reuse.
Next: provider enforcement/guard wiring, staged binding and complete legal-plan
cost comparison before selected WHIRL lowering. MMA1 is untouched.

2026-10-27 R2 reduction numerical-binding increment:
explicit native `common.reduce_sum.v2` now fixes positive-zero initialization,
ascending-index FP32 left fold, RNE/gradual/no-trap environment and NaN-class
observations. Existing Python `torch.sum` still emits unbound v1; no silent
conversion or sample-based numerical permission. Canonical native facts and
version survive actual PREOPT import/emission.
A read-only native query reports direct ordered and staged ordered recipes,
per-active-row scratch requirements/capacity rejection and forbidden partition
trees. Five native cases, 104 host recipe checks, 2,048 independent integer-oracle
pair checks and seven new audit/numerical tests pass. Existing row contracts,
cached Python reduction images and focused common/FHE/WOPT checks pass; backend
and inline symbol boundaries remain closed.
See [the detailed binding and realization design](AI-COMPILER-REDUCTION-NUMERICAL-BINDING-v0.1.md).
This is bounded R2 progress, not production selection or executable lowering:
target arithmetic, alias/storage/events, complete cost modeling and selected
high-level WHIRL loops remain next. MMA1, its profiles and Python source
semantics are unchanged. No GPU run or native reduction accuracy claim.

2026-10-27 R1 Python centered affine normalization increment:
the shared exact source matcher now connects to `common.mean_variance_norm.v1`.
Three runtime formals, explicit result/return, binary32 epsilon and final-addition
source location are preserved through mapped reopen and actual PREOPT. Source
preflight rejects changed recipes, effects, affine broadcasts and invalid
epsilon conversion before evaluation. No model-name equivalence is assumed.
Six native API/driver cases and sixteen prepublication rejects pass; ten focused
source tests cover strict numerical/structural/descriptor boundaries. Five
independent FP64 comparisons are CPU source witnesses, not native-node execution.
Existing `.t`, positional FORM/FACT, report headers and immutable artifact
conventions are retained. See the
[connection and reproduction](AI-COMPILER-NATIVE-NORMALIZATION-CONTRACT-v0.1.md#python-ingestion-increment-2026-10-27).
R1 remains bounded/partial: general forms, numerical binding, executable
alternatives, profitability and selected lowering remain open. No MMA1, native
backend, hardware-profile or GPU change is claimed.
Regression evidence: 13 isolated native API tests, both prior native row
producers, native mapped smoke and 19 trace-auditor tests pass. Full source
suite: 288 tests, 14 skips, one unchanged FHE authority DOCX digest failure;
no unrelated FHE document/assertion is changed. The detail document links the
fresh immutable `r1-python-normalization` receipt and reviewable `.t`/`.T`.

2026-10-27 R1 native mean/variance normalization increment:
`common.mean_variance_norm.v1` now carries the centered population affine
FP32 recipe with three direct X/gamma/beta operands, exact epsilon bits and
strict reassociation/state-reformulation/contraction prohibitions. PREOPT
derives state, affine accesses/broadcasts, denominator/epsilon and numerical
unknowns from canonical WHIRL. Actual PREOPT exposed and now covers the old
two-kid DSL emitter limit; the existing bridge preserves ternary identity.
Four native variants pass 54 checks each, including foreign operands,
rollback, read-only/unchanged-output queries and ternary round trips.
Nineteen trace-auditor and 21 source numerical tests, focused common/FHE/WOPT
compatibility, five historical row-sum replays and symbol-boundary checks pass.
The [normalization detail](AI-COMPILER-NATIVE-NORMALIZATION-CONTRACT-v0.1.md)
identifies source-interleaved WHIRL, controlled compact .t, phase/suite reports
and the immutable receipt. No Python normalization ingestion, executable
normalization, MMA1/profile change or GPU certification is claimed.

2026-10-27 R1 Python-to-native stable-Softmax increment:
the exact whole-graph max/subtract/exp/sum/divide matcher now emits a runtime
input formal, explicit same-shape result/return and native v1 operation.
Structural/descriptor preflight precedes ShapeProp; the shared analysis matcher
checks source edges, not model names. Existing row-sum emission is reused
through common frontend plumbing. Strict numerical prohibitions and unknowns
are unchanged. API/driver PREOPT traces preserve the actual Python division
line; native trace provenance and producer-certified ingestion are kept distinct.
Eight new source tests, five native/PREOPT cases, ten native rejection cases,
four independent FP64 source witnesses, thirteen isolated native API tests,
native smoke and focused row-sum/numerical/capture/auditor regressions pass.
Full source suite: 278 tests, fourteen skips and the unchanged FHE authority
DOCX digest failure. The [ingestion detail and immutable receipt](AI-COMPILER-NATIVE-SOFTMAX-CONTRACT-v0.1.md#python-ingestion-increment-2026-10-27)
retain images, source, raw traces, numerical witnesses and phase reports.
Native normalization, executable selection/lowering and GPU validation remain
open; MMA1 and native C++ contracts/profiles are untouched.

2026-10-27 R1 native stable-Softmax increment:
`common.stable_softmax.v1` now represents static FP32 `[R,N] -> [R,N]`
max/subtract/exp/sum/divide as a native typed unary operation. Shared builder,
shape and gatekeeper checks reject schema/type/ownership violations before
allocation. PREOPT preserves it and derives logical row statistics/accesses
from canonical WHIRL; no Python/debug metadata supplies the facts.
Strict sum/state-reformulation prohibitions and unresolved exp/order/exception,
finite/overflow/accuracy/alias/storage remain explicit. Compact option-controlled
FORM/FACT events and audited semantic/aggregate/summary logs preserve existing
reduction schemas. The [detail design](AI-COMPILER-NATIVE-SOFTMAX-CONTRACT-v0.1.md)
defines the contract, tests, review evidence and remaining gates.
Python Softmax ingestion, native normalization, executable lowering and GPU
validation remain open. MMA1 profiles and implementation are untouched.
Validation passes: 41 native assertions, twelve trace-auditor tests, 21 CPU
semantic/numerical tests, five historical row-sum image replays and focused
WOPT/common/FHE compatibility and link checks. The detail design identifies the
immutable receipt and its seven durable mapped/report replays.

2026-10-27 R1 canonical-WHIRL characterization increment:
bounded row-sum state/access/numerical facts now derive from verified native
assignments, canonical TYs and managed operand/result identities. PREOPT
rederives the snapshot at entry/exit; trace-off does not disable derivation.
The report distinguishes logical read/definition domains and real-arithmetic
merge from physical traffic, alias proofs and permitted FP32 algorithms.
Reassociation remains forbidden; finite/overflow/accuracy/order/storage gaps
remain explicit. Existing FORM rows are unchanged; additive FACT rows and
strict historical-compatible audit retain the trace/artifact convention.
See the [native characterization detail](AI-COMPILER-NATIVE-REDUCTION-CONTRACT-v0.1.md#canonical-whirl-characterization-2026-10-27).
MMA1, executable reduction lowering and GPU certification remain separate.
Validation: 33 native assertions, five authenticated image replays, six audit
tests, trace-off and old/new-reader compatibility, WOPT semantic/driver
regressions and rebuilt symbol boundaries pass in an isolated build. The
detail document links the retained WHIRL, native facts, trace and reports.

2026-10-27 R1 Python-to-native row-sum increment:
the exact three-node `torch.sum` matcher now emits a runtime formal, rank-1
result and return using the existing native v1 contract and explicit-result
builder. Negative/positive source axes normalize to 1; strict FP32/no-
reassociation semantics and the actual Python call-site are preserved.
Four native producer/PREOPT cases, a C++ driver-produced image, five new
source tests, thirteen isolated native API tests, native artifact smoke and
91 capture regressions plus driver smoke pass. Full source suite: 270 tests,
14 skips, one unchanged FHE authority-digest failure; the known aggregate
native DST reset issue is retained separately. See the
[ingestion design and receipt](AI-COMPILER-NATIVE-REDUCTION-CONTRACT-v0.1.md#python-ingestion-increment-2026-10-27).
General native characterization, Softmax/LayerNorm and executable reduction
lowering remain open. No MMA1, GPU or profitability claim is added.

2026-10-27 R1 native reduction increment:
[Native reduction contract v0.1](AI-COMPILER-NATIVE-REDUCTION-CONTRACT-v0.1.md)
adds a typed, pure `common.reduce_sum.v1` for static positive FP32 `[R,N] -> [R]`,
axis 1, keepdim false, FP32 accumulation and no reassociation permission.
Builder preflight, shape checks, gatekeeper and WOPT opaque-unary import/emission
share that contract. Actual PREOPT verifies it on entry and exit without
choosing a reduction tree. Compact option-controlled `.t` events and audited
`semantic.log`/aggregate/summary preserve the existing review conventions.
This is native representation/preservation, not source ingestion, executable
reduction lowering, numerical GPU validation or completion of R1-d. MMA1
compute contracts/profiles are untouched.
Validation: 24 native contract assertions including cross-PU rejection;
actual PREOPT/default/on/off traces, mapped reopen, strict report audit,
focused common/WOPT/shape/lowering regressions and backend symbol gates pass.
The detail document links the immutable receipt and records three legacy
builder-harness limitations reproduced with pre-change common sources.

2026-10-27 R1 frontend-analysis increment:
[Row semantic characterization](AI-COMPILER-ROW-SEMANTIC-CHARACTERIZATION-v0.1.md)
now recognizes exact reduction, stable Softmax and affine LayerNorm dataflows
and derives domains, accesses, numerical/state obligations and conditional
realizations. CPU FP32 witnesses compare with independent FP64 references;
permission, domain and numerical failures are retained separately. This is
source analysis only: no native WHIRL ingestion/transformation, cost selection,
MMA profile or GPU claim. R1-d canonical-WHIRL characterization remains open.
Validation: 21 new tests, 37 focused tests and 86 torch-enabled capture tests
plus driver smoke pass. The 12-case report retains 30 accepted samples,
11 permission rejections, 9 numerical rejections and 2 domain rejections.
Full Docker suite: 265 tests, 14 skips, one unchanged FHE authority-digest
failure. The detail design links the immutable source-analysis receipt.

2026-10-27 design-only increment:
[MMA compute realization and SIMT fallback v0.1](AI-COMPILER-MMA-COMPUTE-REALIZATION-DESIGN-v0.1.md)
proposes exact Hopper WGMMA and Blackwell SM100 tcgen05 recipes, numerical and
resource/event legality, schedule-aware costs, uncertainty-aware promotion,
report obligations and a staged execution queue. Current kernels remain scalar
F32; no native MMA, new hardware profile, option registration or speedup is
claimed. SIMT remains the initial default and required same-contract fallback.

2026-10-27 frontend-fixture increment: six isolated CPU FP32 Python programs
now cover GEMM, row sum, stable Softmax, affine LayerNorm, causal Attention and
routed MoE. Independent scalar FP64 references, real FX/shape/argument census,
repeatability/no-mutation checks and explicit mock-ingestion boundary tests
are implemented. See [fixture contracts and next gates](../osprey/torch2whirl/BUILDING-BLOCK-INGESTION-PLAN.md).
This completes the fixture/capture portion of R1, not canonical WHIRL
characterization, generic optimization, dynamic ragged MoE, or GPU certification.
Sixteen focused tests, all 65 torch-enabled capture regressions and the driver
smoke pass. The full 244-test Docker suite has one unchanged FHE authority-digest
failure and 14 skips; the host also lacks torch/numpy. The ingestion plan links
the checksum-verified source/capture receipt and documents these limitations.

2026-10-27 architecture/research addition:
[Common execution planning](AI-COMPILER-COMMON-EXECUTION-PLANNING.md) adopts
the colleague's Work Partition, Resource Lifetime and Execution Events
framework while retaining upstream computation characterization and verified
algebraic alternatives. P8's access-lifetime/physical-packing enhancement is
specified, not generically implemented. The
[R0 four-case experiment](research_library/building_block_planning/README.md)
passes 21 tests, 32 synthetic comparisons, 16 decisions and nine host numeric
checks. Slow memory changes the preferred normalization algorithm; expensive
launches change the preferred reduction partition. These are research-model
preferences, not GPU speedups or new backend support. R1-R7 remain open in
the implementation plan; existing bounded GEMM/native status below is unchanged.

Occupancy increment (2026-10-26, after merged PR193): model6 implements
table-owned capacity/rounding envelopes and PU-local D/S1/T2 residency-round
costs. Compute and movement rates stay shared; zero-residency estimates reject
only the affected sibling. Compact traces/report audits expose rounded
demands, every capacity quotient, tied limiters, full/tail rounds, underfill
and old/new winners. The 104 baseline tests plus five new tests pass, including
5535 literal D/S1/T2 schedule oracles alongside the retained 2340 T2 cases.
Final acceptance passed: checkpoint `b19e22e0`, fifteen compiler/source
configurations, linked contracts and dependency checks. All fourteen readiness
winners remain unchanged; seven candidate costs change in each large-grid
case. 1384 files were verified after immutable promotion; six durable mapped
reopens and three complete report replays match. Native occupancy/GPU
certification remains open. See [occupancy contract](AI-COMPILER-OPTIMIZATION-OCCUPANCY.md).

Evidence:
`/Users/shinmingliu/open64/artifacts/review/ai-optimization/m9/occupancy/sha256-663139e18ca2cdd6aba88ea9c8b056958c4b3aa46dd9f2866c897b8e15a154b9/`.

Historical hardware-description increment (2026-10-26): exact `gpu_arch=sm_90/sm_90a`
resolves into our own common Hardware Description IR. Model 5 removes private
delay/rate constants from analytical evaluation and ledger adaptation; all
path startup, store, sync, issue, service and wave parameters come from named
tables. The legacy latency option applies only when explicitly supplied.
Blackwell rows exist for planning but unsupported CUDA emission fails closed.
`tile.log` now shows architecture/table/revision/provenance before old
diagnostic scores. 104 tests, 2340 varied-parameter oracles, thirteen compiler/
source configurations, legacy controls, common/VHO contracts and symbol checks
pass. These tables still carry uncalibrated estimates.
See [target selection](AI-COMPILER-HARDWARE-DESCRIPTION-TARGET-SELECTION.md).

Checkpoint `1f793c9e`: 1281 files checksum-verified after immutable promotion;
durable WHIRL reopening and report replay match. Evidence:
`/Users/shinmingliu/open64/artifacts/review/ai-optimization/m8/hardware-description/sha256-b1370aae64cc600d9cb9b2fc6b58dbc357a793be4efe4e9f38542cbc912bc859/`.

Historical analytical readiness increment (2026-10-26): the compiler's opt-in joint path
now defaults to a schedule-aware reference-cycle model instead of traffic-score
overlap credit. D/S1/T2 share an explicit assumed profile; T2 models preload,
ready waits, consume/release/refill and drain in O(log K). Relative model 3
remains available. This is separate from the downstream simulator below.
102 unit tests and 2340 literal recurrence oracles pass; ten compiler/source
configurations pass independent trace/WHIRL/CUDA and host-only checks.
128x128x144 now emits T2 #3 instead of relative S1 #12; its modeled total
31336 compares with best synchronous S1 #1 at 43237 in the same assumed unit.
65x67x17 emits S1 #1 instead of D #8; three tiny/tail shapes retain D #2.
Low-latency selection differs from the shadow simulator and remains an
explicit calibration question, not evidence of simulator or GPU accuracy.
No native GPU timing or calibrated ranking is claimed. See the
[analytical design](AI-COMPILER-OPTIMIZATION-ANALYTICAL-READINESS.md).

Compiler checkpoint `de7256b7`: final backend/link checks, legacy D/S1/T2,
controls/baseline parity and linked AIO-10 contracts pass. 1119 files were
checksum-verified after immutable promotion; durable `.B` reopen and raw-trace
summary replay match retained results. Evidence:
`/Users/shinmingliu/open64/artifacts/review/ai-optimization/m7/analytical-readiness/sha256-9e4d7678b42ff5cfbb543b82d12ac55f39061935fdb371d39046cf09e934b5d0/`.

Design-review increment (2026-10-26): a
[bubble-prediction algorithm](AI-COMPILER-OPTIMIZATION-BUBBLE-MODEL.md)
and B0-B5 queue are proposed inside FP32 analytic refinement. First compare
the same legal D/S1/T2 census in shadow mode, reporting operand readiness,
finite-resource waits and old/shadow/emitted winners without changing actual
selection. The initial read-only source-warp simulator, synthetic cycle
ranking and analyzer-generated `bubble.log` are now implemented; see design
Section 10. Production VHO integration/options, selection and calibration
remain pending. No native speedup is claimed. See the
[implementation queue](AI-COMPILER-OPTIMIZATION-IMPLEMENTATION-PLAN.md#bubble-model-experiment).

Initial bubble replay: 96 unit tests pass. Five authenticated cases across
three synthetic profiles produce 11 changed and four unchanged hypothetical
winners. For 128x128x144, S1 remains best at latency 20, while T2 wins at
100/400; at 100, modeled completion is 79512 versus 72630 reference cycles.
The actual S1 compiler selection remains unchanged. See bubble design
Sections 10-11 for source-expansion assumptions, limitations and full results.
Checkpoint `f352ccbd`: 168 independent event/interval audits and 331-file
durable checksum verification. Evidence:
`/Users/shinmingliu/open64/artifacts/review/ai-optimization/m6/bubble-replay/sha256-767a1d13e8e7d1ea1b1aef1d22b218386015d938a22158a1c65ca68df7c55460/`.
The tail event evidence was independently reopened after promotion.

Movement-resource increment (2026-10-26, after merged PR 192): joint P7
resource deferral and independent P8 D/S1/T2 resource ledgers are implemented.
The shared lowering preflight recomputes the ledger; joint model 3 traces and
the report tool audit all movement siblings and summarize selected resources.
Constrained native tests prove that staged SMEM/register rejection does not
reject a legal D sibling, including zero available SMEM. Native registers,
spills, occupancy, grid waves and GPU calibration remain unknown. See the
[movement-resource design](AI-COMPILER-OPTIMIZATION-MOVEMENT-RESOURCES.md).
The next compiler package is analytic resource/profitability refinement, not
FP8 support or unconditional movement rewriting.
Validation: 77 unit tests, D/S1/T2 integration, joint controls/baseline parity,
executable P8 and linked AIO-10 contracts pass; eleven malformed common
records are rejected. Generated-source host simulations report zero error.
Producer checkpoint `ef7cebce`; SHA256-verified immutable evidence:
`/Users/shinmingliu/open64/artifacts/review/ai-optimization/m5/movement-resources/sha256-792700b3a528cd2cf07ac04097e0b437f42d0ca6b27a1f78d4f458bd1de10872/`.
Retained device-WHIRL reopen and trace-report replay reproduce the review
outputs. No existing review receipt was modified or removed.

Priority update (2026-10-25): further partial-sum promotion/numerical work is
**deferred until explicit FP8 support begins**. Preserve the prototype below;
production optimization remains fixed-shape F32 D/S1/T2. The next proposed
compiler package is movement-aware candidate/resource discovery, followed by
resource/profitability refinement and independently gated movement/launch
extensions. See
[remaining FP32 compiler work](AI-COMPILER-OPTIMIZATION-IMPLEMENTATION-PLAN.md#remaining-fp32-multilevel-tiling-compiler-work).
GPU certification remains open and separate from compiler-side source review.

Numerical-contract increment (2026-10-25): common runtime records and an
active-PU VHO segment planner now have a verified scalar F32 WHIRL adapter.
The linked fixture retains same-PU before/after images, direct CUDA emission,
raw accumulation trace and audited reports. This is not a native Hopper FP8
accuracy fix: Hopper is planning-only, Blackwell is separate and unsupported,
and production D/S1/T2 behavior is unchanged. See
[Numerical Accumulation Design, Section 7](AI-COMPILER-NUMERICAL-ACCUMULATION-DESIGN-v0.1.md#7-implemented-contract-slice).

Checkpoint `f606852c`: 71 Python tests, 45 scalar K/interval combinations,
14 malformed contracts, four altered WHIRL sequences, the production backend
build and D/S1/T2 regression pass. Review evidence is retained at
`/Users/shinmingliu/open64/artifacts/review/ai-optimization/m4/accumulation-contract/sha256-ca49313ac3fa783e29803efe7137fd92b4660cdf64bfdd6671af77eac99f19e8/`.

Latest compiler-side increment (2026-10-25, after PR 191): joint model 2
compares actual D/S1/T2 alternatives. D is generated from WHIRL with guarded
global/register loads, no A/B SMEM and no staging barriers. It has separate
warp-sector evidence, unknown cache service and no overlap credit. The S1
fallback remains distinct even when D wins. See
[D/S1/T2 design](AI-COMPILER-OPTIMIZATION-D-S1-T2-COMPARISON.md).
Native correctness and calibration remain open without NVIDIA hardware.

Verified compiler checkpoint: `be85a5c0`; 64 Python unit tests, direct/staged
and mixed-PU integration, joint-search controls, executable P8 regressions,
six malformed native direct contracts and two native sector-count oracles
pass. Host simulations have zero reference error. D wins the 3x5x7 case
among 19 pairs; S1 wins 128x128x144. The scores remain relative,
low-confidence and uncalibrated. Durable review evidence:
`/Users/shinmingliu/open64/artifacts/review/ai-optimization/m3/d-s1-t2-comparison/sha256-e90e39c0a17ac15ac58ba41338cb4ca774d5da21a9d70bb138857bf42d845918/`.

Previous compiler-side increment (after PR 190): registered
`gemm_plan_search=baseline|joint`, default `baseline`. Joint mode defers tile
commitment until bounded synchronous/TMA whole-plan comparison and lowers the
winner through the existing device-WHIRL path. It retains a synchronous
compile-time fallback and records the complete census, costs and decision in
the raw trace and `physical-plan.log`. This does not enable runtime TMA launch
integration or claim calibrated performance. The exact supported subset is in
[joint plan search](AI-COMPILER-OPTIMIZATION-JOINT-GEMM-PLAN-SEARCH.md).
Historical snapshots below retain their original milestone context.

The AIO-0 through AIO-13 planning and analysis queue is marked complete in
the implementation plan. Most slices produce check-only candidate, legality,
cost, or feedback evidence. AIO-11 also has an executable rank-2 F4
(four-byte float, not FP4) matmul
rewrite to a generic cuBLASLt provider ABI. **That provider path is not an
Open64-generated tiled GEMM.** The separate F32 path now has a selected CUDA
tile source, a trace/report tool, and an opt-in generated-provider WHIRL
rewrite and a separate selected synchronous device-WHIRL image. It does not
yet have a linked CUDA provider, numerical GPU result, native PTX, or measured
baseline-versus-generated-tile result.

The compiler-side source-emission slice adds opt-in `whirl2c -cuda`
plumbing and a target-neutral `ST_ATTR_DEVICE_SYMBOL` contract. The attribute
distinguishes kernel/device function and global/shared/constant data, and
survives symbol-table serialization without reading the x86-64 host `ST`
TLS bitfield as NVISA memory space. The translator emits `__global__`,
`__device__`, `__shared__`, and `__constant__` from this contract, while still
recognizing legacy NVISA space bits when no attribute is present. Clone,
IPA merge, verification, and symbol printing are updated. Syntax checks with
the Linux build flags pass; an isolated native contract binary and rebuilt
`ir_b2a` pass the dedicated mapped-image test, preserving kernel, global,
and shared attributes across `.B`/`.T`. An older reader labels the new kind
unknown, so mixed-version toolchains are unsupported. This groundwork is now
followed by a selected synchronous GEMM device PU in a separate mapped image.
The existing M2 `.cu` remains legacy-template output; `whirl2c -cuda` emits
a second source from the device PU.
The checksum-verified symbol contract images are retained in
`artifacts/ai_optimization/m2-device-symbol-contract/run-001/` and `run-002/`.
The latter includes both a host PU and a device-attributed PU. The backend
driver filters out host PUs under `-cuda`, suppresses host global declarations,
and rejects an image with no eligible device PU. The expanded `run-010/`
contract has a nonempty device `DO_LOOP` and shared-memory store; isolated
rebuilt binaries round-trip the `.B`, reject a separate host-only image, emit
a device-only `.cu` with C-linkage kernel naming, and pass a host
C++ syntax check with CUDA qualifiers stubbed. CUDA mode emits primitive
scalar aliases without the host `whirl2c.h`. This earlier run is a translation
fixture, not the selected GEMM kernel or an NVCC test. The selected-kernel
evidence in `m2-whirl-kernel/run-008/` includes a complete compiler exit,
device `.B`, WHIRL-derived CUDA, legacy CUDA, trace, and a 256-thread CPU CTA
simulation with zero error against reference and between the two kernels.
`run-009/` verifies WHIRL-only selection with legacy emission disabled and
the trace/report labeled `device_whirl_sidecar`.
`run-010/` binds a selected host rewrite and device image to the same trace,
retains `ir_b2a` and `whirl2c -cuda` outputs, and passes host CTA parity with
zero error. Its receipt is a compiler-side checkpoint, not M2 acceptance.
In `run-011/`, VHO stages the selected tile and the backend driver publishes
the device image after host-PU traversal. The `WHIRL_KERNEL` trace row follows
`TILE_ACTION`; the artifact runner checks that order and retains the same
zero-error CTA parity. A two-matmul-PU fixture confirms the current one-kernel
limit fails before image publication. This is driver-owned publication of a
separate image, not registration in the main program PU tree.
In `run-012/`, the driver also owns the constructed `PU_Info` and mapped-image
writer. VHO retains plan selection and device-PU construction. Both applied
and selection-only paths pass round-trip and host CTA parity; the latter is
zero-error. The image remains separate from the main program PU tree.
`run-013/` now verifies two selected PUs in one device image, distinct
`whirl2c -cuda` entries, v1/v2 host-plan descriptors and provider calls,
a linked host-only execution smoke, and per-PU plus aggregate optimization
reports. The v2 ordinal-to-entry resolution is a package/runtime obligation;
`run-014/` verifies non-divisible M/N/K device WHIRL with selective A/B
zero-fill and C-store guards, a K-only case with no C guard, uniform barriers,
an `EDGE_PLAN` trace, and zero-error multi-CTA host parity. Native CUDA, PTX,
and GPU measurement remain unverified. `run-015/` adds an explicitly marked
v2 relative cost model for valid edge-load bytes and dynamic CTA-by-K-tile
barrier count. It changes the all-tail c128 estimate from 5267 to 4942 while
retaining that selection; it is not a measured speedup.

**Next acceptance gate:** publish `m0-baseline` for the direct fixed-shape F32
matmul, with correctness, target identity, raw timings, source, binary `.B`,
separate-process `ir_b2a -st -src` `.T`, commands, and checksums. Do not infer
performance progress from a selected check-only plan.

An `m0` CPU execution runner now exists under
[`osprey/be/vho/tests/ai_matmul_m0`](../osprey/be/vho/tests/ai_matmul_m0/README.md).
It executes the compiler-produced `-O0` PU through a minimal test runtime.
The first Linux/x86-64 Docker run on Apple Silicon passed numerical and
artifact checks, but was explicitly labeled an emulated functional smoke;
its timings are not accepted performance evidence. Native target measurement
and publication to the immutable review namespace remain open.

A standalone CUDA validation bundle is also available under
[`osprey/be/vho/tests/ai_matmul_cuda`](../osprey/be/vho/tests/ai_matmul_cuda/README.md).
Its explicit AIO-9-shaped `cuda_64` plan and generated `.cu` are preserved at
`/Users/shinmingliu/open64/artifacts/review/ai-optimization/matmul-f32-v1/hopper/validation-prototype/run-001/`.
The bundle is ready for remote compilation and measurement, but no native GPU
result is recorded yet. It is template-generated validation code, not a
cost-selected WHIRL transformation, and does not close `m2`.

An opt-in Open64 backend path now emits a **cost-selected** CUDA source
sidecar with `-Wb,-DSL:cuda_tile_output=<new-file.cu>`. The fixed F32 matmul
produced `cuda_128` under AIO-9/AIO-2 selection, and the source, input `.B`,
text `.T`, option-off/on logs, and checksums are preserved at
`/Users/shinmingliu/open64/artifacts/review/ai-optimization/matmul-f32-v1/hopper/compiler-source/run-004/`.
This is the first readable compiler-selected kernel source, but WHIRL still
lowers the direct CPU fallback. The lowered `.I` is byte-identical with the
option off. GPU compilation, numerical verification,
resource measurement, and application of the selected tile remain open.

The next immutable run, `/Users/shinmingliu/open64/artifacts/review/ai-optimization/matmul-f32-v1/hopper/compiler-source/run-005/`,
adds raw `traces/compile.t` and validated `reports/phases/tile.log`,
`reports/optimization.log`, and `reports/optimization-summary.log` beside
the same selected `.cu`. Its decision trace shows baseline, `cuda_64`, and
`cuda_128` costs before selecting plan 3. Its action row explicitly records
source-sidecar emission and **no tile WHIRL rewrite**. This closes the
source-review trace gap for P7 only, not the complete `m1` gate.
The AI decision trace option now defaults ON for this source path and may be
disabled with `-Wb,-DSL:ai_opt_trace=OFF`. The immutable `run-006/` artifact
contains the same trace and derived reports without `-ttOP2` in its compiler
command; a separate OFF check emitted the `.cu` but no `.t` file.
The subsequent immutable `run-007/` artifact uses trace schema v2: every
candidate row ends with the Python source line. The fixed fixture now gives
the matmul result its actual source position, and the artifact runner checks
that the reported line matches the `common_matmul` call (`fixture.py:28`).
The immutable `run-008/` report extends the `tile.log` header with the exact
current compute, baseline/tiled global-memory traffic, and total-score
formulas. It explicitly labels `memory_unhidden` as a traffic proxy rather
than estimated unhidden latency; no runtime calibration is implied.
Trace schema v3 records `selection_reason=lowest_complete_relative_cost` in
the raw `TILE_SELECT` row and carries it into `tile.log` and
`optimization-summary.log`. This names the observed AIO-9 decision rule; it
does not claim that the relative score predicts GPU latency.
The immutable `run-009/` compiler-source artifact validates this v3 record,
the derived reports, and a receipt carrying the same reason; its
`SHA256SUMS` check passes. The selected plan remains `cuda_128`, and the
action still reports source emission without a tile WHIRL rewrite.

The immutable `run-010/` artifact adds a first compile-time Roofline aid.
Schema v4 records the H100 SXM published FP32/HBM ceilings once, then per-plan
FLOPs, modeled global bytes, and ideal compute, cold-HBM, and maximum bounds.
The selector may prefer a legal alternative only within roughly 3% of the
lowest relative cost and with at least roughly 5% lower bound. This is an
uncalibrated tie-break heuristic, not a latency estimate or a measured gain.
For the fixed 128x128x128 GEMM, `cuda_128` remains the relative-cost winner;
its 63 ns ideal bound is below `cuda_64`'s 98 ns. Neither kernel has been
validated on a Hopper GPU, and actual HBM traffic remains unknown. Blackwell
still uses relative cost alone because no matching ceiling is supplied.
The `run-011/` artifact uses the rebuilt selector. The latest `run-012/`
also reports arithmetic intensity, bandwidth needed to reach the FP32 peak,
and the limiting Roofline resource. Its raw trace, generated `.cu`, reports,
and summary are checksum-verified.

An opt-in P8 source-emission path now consumes the AIO-10 selected fetch plan
for the exact F32 tile. The checksum-verified immutable `run-015/` artifact
selects a
two-stage `tma_like` plan (64 KiB A/B staging), emits 2-D TMA tensor-map
loads with stage barriers and a runtime-guarded launch wrapper, and records
P8 candidates, selection, and source action in trace schema v5 and
`fetch-pipeline.log`. Candidate 4 wins by `lowest_complete_relative_cost`;
its raw/hidden/unhidden movement scores are 1024/512/512 relative units,
not measured time. The Open64 backend rebuild and 16 Python report tests pass.
This is **source-only**, not GPU-compiled, not a WHIRL
rewrite, and not a measured prefetch improvement; `m3` remains open pending
native compile, correctness, instruction-path inspection, and same-target
comparison with the synchronous tile.

The digest-named `m1-trace-report/sha256-c3e7ff97ea9a49ef7b01897e1edb16cd19f9f93ca1acfaf50b9b5c93f81ae69c/`
checkpoint adds schema v7 P0-P11 coverage, source-linked AIO-3 semantic tensor
facts and AIO-4 locality facts, a separate option-off `.t`, all twelve derived
phase logs for both runs, and explicit selected/unselected tile labels. Its
`SHA256SUMS` verification passes. This verifies the trace/report implementation
but is not an accepted `m1` receipt because native `m0` is still missing.
The compiled direct PU also passes numerical checks in
`m0-baseline/emulated-smoke-001/`; its x86 execution was emulated on this host,
so its timing is not target profitability evidence or an accepted `m0` receipt.
The same executable and WHIRL evidence are retained in
`m0-baseline/native-ready-001/` with a native x86-64 runner; no native receipt
has been recorded.
On 2026-10-21, M1's digest-named checkpoint passed its `SHA256SUMS` audit.
Replaying both raw traces through the current `ai-opt-report` reproduced
their archived phase logs, complete logs, and summaries byte for byte. The
option-off replay reports no selected tile; the option-on replay retains
three P7 candidates and selected plan 3. This closes the local report
reproducibility check, not M1 acceptance: `native-ready-001/native.json`
still says `native_verified=false`, and no reviewed native M0 receipt exists.
The archived M1 trace predates the additive `TILE_GRID` rows; a fresh compiler
checkpoint will be needed to review those rows, without changing the archived
receipt.
The selected compiler `.cu` is retained in `compiler-source/run-018/` with an
explicit TMA-versus-fallback launch flag. The exact same source bytes and
compiler trace are packaged in `compiler-gpu-validation/run-006/` with a
nonuniform-input correctness and same-device sync/TMA wall-clock timing harness.
The source tile dimensions, thread geometry, shared-memory bytes, plan ID,
and problem shape are checked against the selected trace before publication.
The new `plans/m2-selected-tile.json` records the selected plan, cost and
resource estimates, source line, fetch plan, and source/trace/WHIRL hashes;
it is carried into the GPU package but records no executable rewrite. The
native run is pending; the local host has no `nvcc` or `nvidia-smi`.

| Outcome metric | Latest accepted value | Evidence |
| --- | --- | --- |
| Target and GEMM shape | Not recorded | `m0-baseline` receipt pending. |
| Direct F32 baseline latency | Not measured in an accepted receipt | `m0-baseline` pending. |
| Generated tiled F32 latency and speedup | Not available | `m2-multilevel-tiling` pending. |
| Selected tile, SMEM per CTA, and CTA residency | Not available for an emitted kernel | `m2` compiled-resource check pending. |
| AI decision-trace phase coverage | P0-P11 explicit coverage for the standalone matmul source slice; P7/P8 candidate, selection, and source-sidecar action | Digest-named `m1-trace-report` checkpoint; native `m0` predecessor pending. |
| Predicted-versus-measured cost error | Not available | Cost calibration follows m0-m3. |

### Status key

| State | Meaning in this ledger |
| --- | --- |
| Designed | Contract or option is written, but no implementation evidence is claimed. |
| Check-only | Analysis records exist; executable WHIRL is unchanged for that slice. |
| Provider rewrite | An executable ABI/provider selection exists, but it is not a generated kernel. |
| Generated and verified | A selected compiler-generated transformation has before/after IR, numerical, and launch/resource verification. |
| Measured | Correctness-qualified baseline and candidate have comparable target-specific timing evidence. |

## Matmul execution gates

These rows mirror the [Current Matmul Execution Queue](AI-COMPILER-OPTIMIZATION-IMPLEMENTATION-PLAN.md#current-matmul-execution-queue).
An unchecked gate is open even if a related check-only AIO slice is complete.

| Gate | Status on 2026-10-25 | Evidence required to close | Next action |
| --- | --- | --- | --- |
| `m0-baseline` | Open; compiled-PU native package ready | Direct F32 correctness passes in emulation; source, `.B`/`.T`, executable, commands, and hashes are retained in `emulated-smoke-001/`. `native-ready-001/` preserves the exact executable for fresh native correctness and timing. Emulated timing is not target evidence. | Run on the intended native x86-64 target and review the returned receipt. |
| `m1-trace-report` | Trace/report implementation verified; acceptance open | Schema v7 emits P0-P11 coverage, P0 semantic facts, P1 locality facts, and an option-off trace. `ai-opt-report` validates decisions and produces all twelve phase logs, the complete log, and the summary. The digest-named checkpoint retains both raw `.t` files, reports, commands, and checksums; malformed/incomplete-trace tests pass. The receipt explicitly says `m1_accepted=false`. | Obtain an accepted native `m0-baseline` predecessor, then review and accept the m1 receipt; broaden other phase details as each optimizer is implemented. |
| Tile costing prerequisite | Open | Baseline plus at least two M/N/K tile families with explicit units, target resources, confidence, rejection reasons, and stable plan IDs. | Refine AIO-9 check-only plans into costable full candidates. |
| `m2-multilevel-tiling` | Open; synchronous exact/edge device-source parity and two-PU compiler binding verified, native GPU pending | `m2-applied/run-007/` retains the host generated-provider rewrite and plan traces; `m2-whirl-g14/run-007/` retains host before/after WHIRL. `m2-generated-runtime/run-009/` packages the GPU validation path. `m2-whirl-kernel/run-008/` adds a separate selected device WHIRL image, translated CUDA, and zero-error CPU CTA parity. `run-009/` verifies legacy-OFF translation. `run-010/` binds the selected host rewrite, trace, device image, emitted CUDA, and zero-error parity in one SHA256-checked receipt. `run-011/` verifies driver-boundary publication and trace order; its two-PU negative fixture records the former limit. `run-012/` moves device-`PU_Info` ownership and mapped-image writing to the driver. `run-013/` retains a two-PU device image, v1/v2 host ABI bindings, host mock execution, raw `.t`, phase logs, and checksums. `run-014/` retains all-axis and K-only predicated device WHIRL, selective A/B zero-fill and C guards, `EDGE_PLAN`, host-only multi-CTA parity, and checksums. `run-015/` retains v2 edge-load and dynamic-barrier cost decisions with legacy-trace compatibility. | Implement package/runtime v2 ordinal resolution, model sector transactions and exposed latency, then validate NVISA/PTX and native CUDA resources/correctness/timing on Hopper against an accepted same-target baseline. Production tensor-provider integration remains separate. |
| `m3-prefetch-pipeline` | Open; native validation package prepared | Selected P7.4/P8 staging and fallback are in the source sidecar and trace. `compiler-gpu-validation/run-006/` preserves the M2 handoff and is ready to compare selected TMA and synchronous paths with correctness and 30 wall-clock launch-to-completion samples per path, including host launcher setup. No native compile or timing has been run, and WHIRL is unchanged. | Run the package on a Hopper-or-later GPU, then integrate the selected plan into executable WHIRL after m2. |
| Cost calibration | Open | Predicted-versus-measured compute, memory, sync, launch, and occupancy terms, with provenance and model error. | Calibrate from accepted m0-m3 measurements. |

The tile trace now also records each candidate's CTA grid and K-iteration
count. For the 128x128x128 fixture, `cuda_64` exposes four CTAs while the
selected `cuda_128` exposes one. Neither SM-level occupancy nor underfilled
grid throughput is in the current cost equation, and the published-peak
Roofline bound cannot settle this comparison. Treat the present selection as
low-confidence until native Hopper correctness and paired timings are
available; do not infer a performance win from the compiler artifact.
The isolated backend rebuild and fresh `m2-applied/run-007/` trace confirm
these rows are compiler-emitted, not only report-test fixtures. Its SHA256
manifest, option-on/off report checks, and compiled-PU mock-launch smoke pass.
`m2-generated-runtime/run-009/` passes prepared-package integrity checks;
it has not been submitted to a native GPU.

## Phase maturity

The AIO rows describe the first certified vertical slice, not full feature
completion. The authoritative phase contracts remain in the implementation
plan and their linked AIO documents.

| Phase | Current maturity | Missing executable or measured evidence |
| --- | --- | --- |
| AI-P0 semantic tensor | AIO-3 check-only; v7 trace records tensor roles, shapes, dtypes, and source lines | Broaden eligible-region and unresolved-semantic reporting beyond the standalone matmul. |
| AI-P1 lifetime and locality | AIO-4 check-only; v7 trace records lifetimes, reuse, alias, working set, and traffic estimates | Connect facts to the generated tile's legal live ranges and cost. |
| AI-P2 fusion | AIO-5 check-only | Out of scope for the first standalone matmul; report `not_applicable`. |
| AI-P3 logical layout | AIO-6 check-only; synchronous GEMM realizes P7-selected A and B SMEM layouts and compares two thread-to-GMEM C-store maps | Extend to strided/repacked GMEM views and calibrated sector/bank evidence. |
| AI-P4 placement and sharding | AIO-7 check-only | Out of scope for first single-device matmul; report coverage. |
| AI-P5 communication | AIO-7 check-only | Out of scope for first single-device matmul; report coverage. |
| AI-P6 residency | AIO-8 check-only | Verify compiled SMEM/register usage and CTA residency against the selected plan. |
| AI-P7 multi-level tiling | AIO-9 candidate/selection plus opt-in generated-provider rewrite; A/B SMEM and 8x8 versus 16x2 GMEM lane maps lower into WHIRL | Integrate multi-stage/TMA WHIRL plans; calibrate source-sector, bank, occupancy, and resource use; execute on a GPU. |
| AI-P8 fetch and pipeline | D direct-register, S1 synchronous and T2 exact two-slot TMA device WHIRL; bounded joint comparison | Native GPU acceptance, D/TMA host-provider integration and `cp.async` remain open. |
| AI-P9 physical plan | AIO-11 F4 provider rewrite; F32 generated-provider call linked to a fixed-shape review adapter and mock-tested | Verify the actual CUDA launch, fallback diagnostics, and production tensor-provider boundary. |
| AI-P10 runtime variants | AIO-12 check-only guarded variant evidence | Defer executable variants until the kernel and fallback are certified. |
| AI-P11 feedback | AIO-13 check-only telemetry join | Add reviewed profile transport and measured feedback after executable work. |

## Options and observability

Joint-plan checkpoint `0084f7fe`: 59 unit tests plus joint-search,
executable-P8 and AIO-10 regressions pass. For 128x128x144, 16 complete pairs
select synchronous XOR-B tile 6 at relative cost 9279; legal compact TMA costs
11646. All host simulations have zero reference error. Native CUDA and GPU
performance remain unverified. The immutable evidence, including the raw
decision trace, generated CUDA and discussion PDF, is retained at:
`/Users/shinmingliu/open64/artifacts/review/ai-optimization/m3/joint-gemm-plan-search/sha256-4d4174f64dcf7706275132574f6d592b49aed3f743b0c18a0e5c21a551dd9dfa/`.

Keep registered switches distinct from proposals. The
[backend option registry](../osprey/common/com/config_dsl.cxx) is the source for registered
`-DSL` names; the Python [export options](../osprey/torch2whirl/python/open64_dsc/options.py)
are frontend settings, not backend optimization controls.

| Control | Status | Monitoring implication |
| --- | --- | --- |
| `-O0` through `-O3`, `-ipa` | Driver modes; AI scope contract in the implementation plan | `-O0` is the direct baseline; the first target tile belongs at `-O3`. Do not infer end-to-end certification from mode acceptance. |
| `-DSL:shape_refine`, `-DSL:quant`, `-DSL:fusion`, `-DSL:parallel`, `-DSL:impl_select` and related stage controls | Registered in `config_dsl.cxx` | A stage switch is not proof of an applied GPU optimization. `-DSL:quant` does not authorize GEMM precision loss. |
| `-keep`, `-DSL:dump_after_lower`, `-ft,<file>` | Driver/backend artifact controls used by the openpy lane | Useful for retained baseline `.t` and lowered output; not the proposed AI decision trace. |
| `-Wb,-ttOP2:0x00800000` | Implemented for the standalone matmul source slice | Schema v7 covers P0-P11, records P0/P1 facts, and retains P7/P8 decisions; `ai-opt-report` rejects missing or contradictory coverage. Other workload scopes still need coverage. |
| `-Wb,-DSL:ai_opt_trace=ON|OFF` | Registered; ON by default during AI optimization development | ON activates the trace bit even when CUDA source emission is off; the standalone matmul records why P7/P8 did not run. OFF suppresses AI decision rows. |
| `-Wb,-DSL:cuda_tile_output=<new-file.cu>` | Registered opt-in source sidecar | Emits one cost-selected F32 tile source for a straight-line PU. Does not apply a WHIRL rewrite or close `m2`. |
| `-Wb,-DSL:cuda_whirl_output=<new-file.B>` | Registered opt-in device-image sidecar | Emits a complete synchronous F32 GEMM device PU for exact-divisible fixed shapes after cost selection; `whirl2c -cuda` translates the image. Independent of legacy source emission. No native PTX claim. |
| `-Wb,-DSL:cuda_tile_apply=ON` | Registered, OFF by default; requires `-O3`, `-noinline`, one matmul, a legacy source or device-WHIRL output, and synchronous movement | Rewrites the host matmul to `__open64_dsl_matmul_generated_v1`; P7/P9 trace rows report one rewrite. The fixed-shape review adapter links and mock-tests this call; native GPU and production-provider integration remain open. |
| `-Wb,-DSL:cuda_smem_layout_search=ON\|OFF` | Registered, default ON | Adds B column-XOR siblings before full-plan selection; OFF keeps compact B, existing A alternatives, and bank analysis. Synchronous F32 only; no TMA swizzle authorization. |
| `-Wb,-DSL:gemm_plan_search=baseline\|joint` | Registered, default baseline; joint requires `-O3` and device-WHIRL output | Bounded lowerable tile/layout plus movement costs; `.t` records winner and compile-time synchronous fallback. TMA ON permits, not forces, TMA. Host application is still synchronous-only. |
| `-trOP2` | Existing IR-dump selector | Separate from the proposed AI decision-trace mask. |
| `-DSL:gemm_precision=preserve|allow_narrow` | Proposed; not registered | Default preserves source precision; narrowing also needs a numerical contract and full-plan legality/profitability. |
| `-DSL:gemm_schedule=auto|conventional|persistent` | Proposed; not registered | Do not expose until a persistent kernel is implemented and verified. |

The target-specific SMEM ledger, hard resource gate, narrow-precision policy,
and P7/P8 movement sequence are in the
[hardware planning detail design](AI-COMPILER-HARDWARE-RESOURCE-AND-MEMORY-PLANNING-DESIGN-v0.1.md).

## Evidence and update rule

Accepted matmul receipts use the AI-specific immutable namespace defined by
the implementation plan:
`/Users/shinmingliu/open64/artifacts/review/ai-optimization/matmul-f32-v1/<target-id>/<milestone>/sha256-<digest>/`.
That is the planned publication path, not an assertion that a receipt is
present in this checkout. Legacy `artifacts/ai_optimization/` evidence stays
separate and must not be deleted or relabeled as an accepted receipt.

After each accepted milestone, update this ledger in the same change that
updates the implementation queue:

1. Record review date, repository commit, target, milestone status, and an
   exact receipt path or link. Do not close a row using only a design note or
   a check-only candidate trace.
2. Link raw `.t`, derived `optimization-summary.log` and `tile.log` when
   available, before/after `.B`/`.T` for rewrites, numerical result, and raw
   timing samples. Keep estimated cost separate from measured latency.
3. State the selected plan ID, baseline ID, decision reason, rewrite result,
   verification result, and next blocker. For skipped phases, record the
   explicit disabled/not-applicable/no-eligible reason.
4. Preserve prior receipt links and add one dated change-log entry; do not
   overwrite an accepted receipt or silently mark all phases complete.

## Change log

| Date | Change | Evidence |
| --- | --- | --- |
| 2026-10-25 | Post-PR189 executable P8 compiler slice: selected one-slot synchronous and exact two-slot TMA schedules are materialized in WHIRL, with structural loop/order checks and per-slot generation invariants. CUDA helpers implement individual operations, not scheduling. The report validates published `PIPELINE_KERNEL` evidence and reports P8 device transformation. 53 unit tests pass; one/two/three/five/nine-tile delayed-copy simulations have zero reference error and matching issued/completed/released counts. Corrupt source/trace, unsupported ABI/edges, two-TMA-PU, synchronous applied, SMEM, edge, multi-PU, legacy TMA and AIO-10 regressions pass. Native NVCC/PTX/GPU and TMA launch-provider integration remain open. Added architectural and function-entry contracts to `dsl_lower.cxx`, with coverage testing. | Producer `5fb45858`; [durable receipt](/Users/shinmingliu/open64/artifacts/review/ai-optimization/m3/executable-p8/sha256-46791c6a18371b6766acf3f30d006974d1de8a9d4b18302dee8b717ecd1b903d/receipt.json), [TMA CUDA](/Users/shinmingliu/open64/artifacts/review/ai-optimization/m3/executable-p8/sha256-46791c6a18371b6766acf3f30d006974d1de8a9d4b18302dee8b717ecd1b903d/executable-p8/tma-k144/gemm.whirl.cu), [summary](/Users/shinmingliu/open64/artifacts/review/ai-optimization/m3/executable-p8/sha256-46791c6a18371b6766acf3f30d006974d1de8a9d4b18302dee8b717ecd1b903d/executable-p8/tma-k144/reports/optimization-summary.log). |
| 2026-10-25 | Post-PR188 synchronous SMEM package: target bank model, per-operand allocation ledger, A/B copy/read bank rounds with broadcasts, B column-XOR alternatives and explicit `COST_MODEL 5` selection. Selected addresses lower into WHIRL and canonical CUDA. Short K selects 16x2/XOR (score 2010); long K selects padded-A 8x8/XOR (7209), versus compact-B 16x2 with search OFF (9528). These are relative estimates, not speedups. 52 unit tests plus short/long/OFF, corrupt-trace, all-tail/K-tail/I4, multi-PU, AIO-9 Hopper/Blackwell and compact-TMA-sidecar regressions pass. Native CUDA/GPU and async WHIRL acceptance remain open. | Producer `7efdf8da`; [durable receipt](/Users/shinmingliu/open64/artifacts/review/ai-optimization/m3/smem-planning/sha256-0dbd3c72e90e8036f947036821ad9bb3f5c8191189c2cc3f04b3431a738a3534/receipt.json), [CUDA](/Users/shinmingliu/open64/artifacts/review/ai-optimization/m3/smem-planning/sha256-0dbd3c72e90e8036f947036821ad9bb3f5c8191189c2cc3f04b3431a738a3534/compiler-review/source/matmul.whirl.cu), [summary](/Users/shinmingliu/open64/artifacts/review/ai-optimization/m3/smem-planning/sha256-0dbd3c72e90e8036f947036821ad9bb3f5c8191189c2cc3f04b3431a738a3534/compiler-review/reports/optimization-summary.log). |
| 2026-10-20 | Initial ledger from implementation-plan queue and current option registry. No m0-m3 receipt is linked here. | [Implementation plan](AI-COMPILER-OPTIMIZATION-IMPLEMENTATION-PLAN.md#current-matmul-execution-queue), [registered DSL options](../osprey/common/com/config_dsl.cxx). |
| 2026-10-20 | Added an executable `-O0` F32 matmul runner and validated its compiler path under x86-64 emulation; `m0` remains open pending native timing and accepted publication. | [Runner and limitations](../osprey/be/vho/tests/ai_matmul_m0/README.md). |
| 2026-10-20 | Prepared a source-preserving Hopper CUDA validation bundle with remote compile/run/retrieval tooling. GPU execution and compiler lowering remain open. | [CUDA bundle contract](../osprey/be/vho/tests/ai_matmul_cuda/README.md). |
| 2026-10-20 | Added opt-in compiler-selected CUDA source emission; `cuda_128` source and option-off/on evidence retained. WHIRL application and native GPU verification remain open. | [Compiler source bundle](../osprey/be/vho/tests/ai_matmul_cuda/README.md). |
| 2026-10-21 | Added opt-in selected-tile WHIRL application and a 64-byte generated-provider plan ABI. Normal `-O3` option-off/on traces and matched binary-WHIRL before/after views verify the compiler rewrite; the CUDA runtime provider and native measurement remain open. | `m2-applied/run-005/`, `m2-whirl-g14/run-007/`, [compiler runner](../osprey/be/vho/tests/ai_matmul_cuda/README.md). |
| 2026-10-21 | Added the fixed-shape M2 review runtime and strict native validation package. The actual rewritten PU links and reaches a mock launch hook with the expected descriptor. GPU compilation, execution, and same-target comparison remain open. | `m2-applied/run-006/`, `m2-generated-runtime/run-003/`, [runtime bundle](../osprey/be/vho/tests/ai_matmul_cuda/README.md). |
| 2026-10-21 | Added a dedicated remote handoff for the applied M2 package, with input and native-output manifests, failure-log retrieval, no-reuse policy, and host-only integrity tests. The transport has not been exercised against a remote GPU. | `m2-generated-runtime/run-006/`, [remote runner](../osprey/be/vho/tests/ai_matmul_cuda/remote_generated.py). |
| 2026-10-21 | Rebuilt the M2 backend in an isolated temporary clone, regenerated an applied artifact with `TILE_GRID` rows, verified its manifest and compiled-PU mock launch, and prepared a new native GPU package. The selected one-CTA grid remains an unpriced cost-model limitation. | `m2-applied/run-007/`, `m2-generated-runtime/run-009/`. |
| 2026-10-21 | Added a validation-only untiled CUDA control with nonuniform correctness checks and 30 paired CUDA-event samples to separate tile-kernel timing from full-PU transfer/allocation time. It is not an Open64 baseline and has not run on a GPU. | `m2-generated-runtime/run-008/`, [kernel control](../osprey/be/vho/tests/ai_matmul_cuda/kernel_control.cu). |
| 2026-10-21 | Verified a nonempty device WHIRL loop and shared-memory store through mapped-image round-trip and isolated rebuilt `whirl2c -cuda`; retained the C-linkage `.cu`, checked host-only rejection, and passed a host C++ syntax check. The selected GEMM PU remains unmaterialized. | `m2-device-symbol-contract/run-010/`, [device contract](../osprey/common/com/tests/device_symbol_image_test.sh). |
| 2026-10-21 | Materialized the selected synchronous GEMM as a separate device WHIRL image. `whirl2c -cuda` emits the full kernel; a 256-thread CPU CTA simulation matches reference and legacy source with zero maximum error. Also verified selection and translation with the legacy source disabled and its trace/report corrected. This is not native PTX or GPU acceptance. | `m2-whirl-kernel/run-008/` and `run-009/`, [parity harness](../osprey/be/vho/tests/ai_matmul_cuda/test_whirl_cuda_parity.sh). |
| 2026-10-21 | Added one-run M2 provenance for the selected host rewrite and device kernel. The artifact keeps relative trace paths, device `.B`/`.T`, WHIRL-derived and legacy CUDA, hashes, and zero-error host CTA parity. Driver registration and native CUDA validation remain open. | `m2-whirl-kernel/run-010/`, [compiler runner](../osprey/be/vho/tests/ai_matmul_cuda/README.md). |
| 2026-10-21 | Moved selected-device-image publication to the backend driver after host-PU traversal; VHO stages the numeric tile plan. The trace records publication after tile action, `run-011/` retains a passing one-PU artifact, and a two-PU fixture confirms explicit rejection without an output image. Main-tree device-PU registration, native PTX, and GPU validation remain open. | `m2-whirl-kernel/run-011/`, [two-PU fixture](../osprey/be/vho/tests/ai_matmul_cuda/two_matmul_fixture.py). |
| 2026-10-22 | The driver now receives the constructed device `PU_Info` and writes its mapped image at program boundary. Applied and selection-only paths pass `ir_b2a`, `whirl2c -cuda`, and host CTA parity. The new receipt is SHA256-checked; main-tree registration, multi-kernel binding, native PTX, and GPU validation remain open. | `m2-whirl-kernel/run-012/`, [compiler runner](../osprey/be/vho/tests/ai_matmul_cuda/README.md). |
| 2026-10-24 | Replaced the one-kernel compiler-side limit with an ordered device-PU registry and atomic multi-PU image. Two selected host PUs emit v1/v2 provider calls, with a versioned v2 device-entry ordinal. Reopened both WHIRL kernels, emitted both CUDA entries, linked and executed a host-only mock, and rendered per-PU and aggregate optimization reports from `.t`. The SHA256-checked receipt is not GPU evidence; package/runtime ordinal resolution, native PTX, and GPU validation remain open. | `m2-whirl-kernel/run-013/`, [multi-PU test](../osprey/be/vho/tests/ai_matmul_cuda/test_multi_device_whirl.py). |
| 2026-10-22 | Design decision: performance-harness generation is opt-in at the `openpy` driver, default OFF. It requires a certified same-target baseline and separates correctness from paired kernel-only and end-to-end timing. The provisional `-perf-harness` option and executable are not implemented; no GPU speedup is claimed. | [end-to-end architecture](OPEN64-AI-COMPILATION-AND-RUNTIME-ARCHITECTURE.md#15-options-and-driver-behavior), [M2 plan](AI-COMPILER-OPTIMIZATION-IMPLEMENTATION-PLAN.md#current-matmul-execution-queue). |
| 2026-10-25 | Device WHIRL now handles non-divisible M/N/K synchronous F32 tiles with axis-selective A/B zero-fill and C-store guards, uniform barriers, and an I4 index-safety gate. All-tail 130x133x135 source-only/applied and K-only 128x128x135 host-simulated parity show zero error against reference and legacy CUDA; an I4-unsafe 50000x50000x128 case is rejected before device publication. `EDGE_PLAN` is validated in `.t` and summarized in `tile.log` and `optimization-summary.log`; exact and two-PU regressions pass. This is not NVCC, PTX, GPU correctness, or speedup evidence. | `m2-whirl-kernel/run-014/`, [edge test](../osprey/be/vho/tests/ai_matmul_cuda/test_device_whirl_edges.py). |
| 2026-10-25 | A `COST_MODEL 2` trace row now identifies valid predicated A/B load bytes and dynamic CTA-by-K-tile barrier counts in the relative tile score. The parser recomputes those terms for every candidate and still renders older unmarked v7 traces with the prior formula. The all-tail c128 relative estimate falls from 5267 to 4942 while selection stays c128; host parity and exact/two-PU regressions pass. This is cost-model evidence, not hardware speedup evidence. | `m2-whirl-kernel/run-015/`, [tile candidate model](../osprey/be/vho/dsl_tile_candidate_opt.cxx). |
| 2026-10-25 | P7 now compares compact A/B against padded-transposed A/compact B for synchronous F32 c128 tiles. `COST_MODEL 3` adds a low-confidence A-bank-serialization proxy and `TILE_LAYOUT` records the fixed linear GMEM copy map, A stride, and estimated bank work. The selected layout changes both legacy `.cu` and device WHIRL addressing; edge and exact host-CTA parity, mock host binding, multi-PU, and compact TMA-source regressions pass. GMEM sector modeling, B alternatives, async staging, NVCC/PTX, and native GPU measurements remain open. | `m2-whirl-kernel/run-016/`, [layout lowering](../osprey/be/vho/dsl_lower.cxx), [edge fixture](../osprey/be/vho/tests/ai_matmul_cuda/test_device_whirl_edges.py). |
| 2026-10-25 | Added a compiler-only CUDA review gate independent of GPU availability. `run-018/` retains the selected device WHIRL, canonical `whirl2c -cuda` output, raw trace, tile/summary reports, host CTA parity, and a plan-bound `plans/cu-review.json`. The selected c128 synchronous kernel uses padded-transposed A (stride 129), row-major B, two CTA barriers per K tile, and 32,896 bytes of shared staging. This is ready for manual algorithm review before GMEM planning; native CUDA compilation and performance remain unverified. | `m2-whirl-kernel/run-018/`, [artifact runner](../osprey/be/vho/tests/ai_matmul_cuda/compile_source.py). |
| 2026-10-25 | First contiguous-F32 GMEM plan: source-warp 32-byte sector requests for A/B loads and C stores, aligned/worst F32 base residues, and shared-A issue work now enter the explicit low-confidence `COST_MODEL 4` gate. A 16x2 thread map is selected at 128x128x32, reducing modeled C-store sectors from 524,288 to 147,456 bytes; padded 8x8 stays selected at 128x128x128. Both WHIRL-derived CUDA kernels match reference and legacy under host CTA simulation. Compact A shared allocation was corrected to `BM*BK`. Edge/I4, two-PU, AIO-9, and TMA-sidecar regressions pass. Native PTX transactions, GPU correctness, and speedup remain unverified. | `m2-whirl-kernel/run-028/`, `run-024/`, [GMEM selection test](../osprey/be/vho/tests/ai_matmul_cuda/test_gmem_coalescing.py). |
| 2026-10-25 | M2-B1 review-adapter slice: target-neutral buffer service with validated import, allocation, transfer, and release; CUDA-specific callbacks stay in the NVIDIA adapter. Host-mock operations show one A/B upload and C download per invocation, no copy/free for borrowed device buffers, cleanup after failed allocation or transfer, and retry after failed release. A fresh applied compiler receipt retains the selected host/device WHIRL and `.cu`; its native-review package includes the service ABI/source and passes manifest verification. This does not yet produce a standalone host dispatch PU or establish NVCC/GPU correctness. | `m2-buffer-binding/compiler-run-001/`, `run-005/`, `package-007/`, [buffer service contract](../osprey/include/open64_target_buffer_service.h). |
