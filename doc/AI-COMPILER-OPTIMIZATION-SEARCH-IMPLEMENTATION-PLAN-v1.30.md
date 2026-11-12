# AI compiler optimization search implementation plan v1.30

2026-11-12 architecture terminology correction:
[native WHIRL contract](AI-COMPILER-WHIRL-OPCODE-AND-TENSOR-TRAIT-CONTRACT-v1.0.md)
governs opcode properties, tensor traits, phase legality and scheduling
services. Embedded earlier revisions include these editorial corrections;
historical implementation status and acceptance evidence retain their original
scope.

November 11, 2026 | GS1 persistent GPU testing worker

Adopt [GS1 worker contract v1.0](GPU-TEST-WORKER-GS1-v1.0.md).
The complete v1.29 plan follows verbatim; original packages remain immutable.

## GS1 implementation and evidence

| Gate | Implemented responsibility / remaining acceptance |
| --- | --- |
| GS1a state | Host-bound SQLite requests/registry/jobs/events/artifacts, persistent principal-scoped idempotency, typed admission and immutable source/transport bindings |
| GS1b ownership | Atomic claims and non-expiring physical GPU leases; CPU jobs without GPU leases, distinct-GPU concurrency, per-job locks and gated child launch before execution |
| GS1c supervision | Original registered runner, cooperative cancellation/SIGINT/SIGTERM, preparation-inclusive execution budget and shared bounded collection/review grace; raw original process exits preserved |
| GS1d recovery | Existing collectors/receiver, immutable closed artifacts, independent collection/review/verdict states; worker-loss/orphan reconciliation without rerunning execution, conservative GPU quarantine and explicit proven operator release |
| GS1e software qualification | Focused real-subprocess/SQLite/fake-device tests, existing combined/native/shared receiver regressions, actual released-packet check-only replay and extracted worker-companion closure checks |
| GS3 hardware qualification | Pending real NVIDIA host: sm_90/tool query compatibility, full reference/sanitizer execution, timing/profiling isolation and device cleanup/recovery; CPU controls are not hardware evidence |

The worker implements local administrative commands, not the proposed HTTP
service or remote client. GS2 is next and reuses these state/worker boundaries.
The original combined archive is unchanged. Correctness checks and separate
wall/device timing and profiling stay owned by the original packet. Failed or
uncertain evidence is not synthesized into acceptance. Two-phase compiler search,
Q3 lifetimes/publication, COMMENT explanations and fallback remain unchanged.
OpenCTI integration stays deferred until modernization completes; AOT selection,
JIT and GS4 new-source admission remain deferred. Roles remain TBD.

---

## Preserved v1.29 plan

# AI compiler optimization search implementation plan v1.29

November 11, 2026 | Packaging closure and parallel GPU testing service

Adopt [packaging closure v1.0](GPU-TEST-PACKAGING-CLOSURE-v1.0.md) and
[server API/usability v0.1](GPU-TEST-SERVER-API-v0.1.md), including its
[OpenAPI contract](GPU-TEST-SERVER-API-v0.1.openapi.json).
The complete v1.28 plan follows verbatim. Earlier releases remain immutable.

## Current decisions and status

- Defer OpenCTI integration until its modernization completes and the applicable
  compatibility gates pass. Do not make GPU server work depend on OpenCTI.
- CP0-CP5 software delivery is complete for combined packet
  `b070675941adcfb2d55a791f7ce87c874c2b444bd165092c644cedc3b155d032`,
  archive SHA-256 `849d123c1801e8bdd7142fe2f8f84ee51138a5c439df4072075c10c91d43ba8d`.
  CP6 remains pending NVIDIA hardware. Keep the released archive unchanged.
- Pursue GPU server API definition and operator/client ease of use concurrently
  within AI optimization. GS0 is a documentation contract, not a running service.
  GS1/GS2 can proceed with CPU/fake-worker tests while CP6/GS3 await a GPU host.
- First support exact registered combined packets. New compiler-source admission
  is GS4 and needs server-owned workload/oracle/baseline bindings; preserve the
  scaffold ABI and separately review full Q7 candidate semantics.

| Gate | Implementation and acceptance |
| --- | --- |
| GS0 definition | Job/capability/packet/event/cancel/artifact API, typed request and identity model, state/verdict separation, CLI workflow and limits; validate companion references/examples and preserve existing release identities |
| GS1 worker | Existing runner/collector/validator adapter; persistent state, exclusive GPU lease, deadline/drain and collection grace; check-only parity, ordinary failure, cancellation and interrupted-worker tests |
| GS2 service/client | Authenticated durable idempotent submission, polling and downloads; doctor/submit/wait/fetch/review flow, reconnect without duplicate work, ownership/quota/corruption tests |
| GS3 NVIDIA | Actual complete combined run, hardware/resource binding, sanitizer coverage, separate timing/profiling and GPU drain/recovery; no synthetic qualification |
| GS4 source candidates | Reviewed source admission and immutable workload/baseline/corpus registry; authenticated source snapshot for receiver; prohibit oracle/harness replacement, qualify workload-specific and full-Q7 boundaries separately |
| GS5 usability | Fresh operator installs/preflights; another user submits, reconnects and retrieves/reviews success/failure evidence with a single command and actionable missing-evidence reports |

The existing two-phase estimation/search isolation, selected-only lowering,
COMMENT explanations, Q3 publication/lifetime rules, correctness and fallback
contracts remain unchanged. AOT selection/JIT remain deferred; roles stay TBD.

---

## Preserved v1.28 plan

# AI compiler optimization search implementation plan v1.28

November 11, 2026 | Combined GEMM and MLA NVIDIA test delivery

Adopt [combined package specification v1.0](GPU-TEST-COMBINED-PACKAGE-v1.0.md).
The complete v1.27 plan follows verbatim. Earlier releases remain immutable.

| Gate | Deliverable / acceptance |
| --- | --- |
| CP0 composition | One outer source archive with pinned, unchanged aligned scaffold and Q7 subpackets; no new kernel, oracle, ABI or search decisions |
| CP1 single command | Sequential existing GEMM/BF16-MLA/Q7 entry points, independent continuation, shared GPU visibility, original Q7 dependency gates and first failure preserved |
| CP2 measurement | Existing separate wall/event batches; profiling off by default, opt-in independent PM/counter/trace children; workload scopes/FLOPs/ratios stay distinct |
| CP3 always-return | Attempt native Q7 collection and one outer return tar after success/failure/timeout/catchable signals; preserve partial raw data and environment without duplicate native artifacts |
| CP4 receiver | Authenticate outer transport/source/file inventory and budgets; safely unpack data only; delegate original shared/Q7 audits, bind profile identities and GPU facts, flag incomplete/failing evidence |
| CP5 software delivery | Focused control/adversarial tests, extracted-source integrity and single-command/receiver replay; fresh versioned tar/checksum/README and immutable receipt |
| CP6 NVIDIA qualification | Actual CUDA build/device correctness, required race lanes, timing and optional profiling on capability-9.0 hardware; no host-only GPU claims |

Q7's missing GPU baseline and full-boundary FLOP formula stay unknown. Roles TBD.
Keep native COMMENT explanations, two-phase search isolation, Q3 publication,
correctness and source fallback unchanged. AOT selection/JIT stay deferred.

---

## Preserved v1.27 plan

# AI compiler optimization search implementation plan v1.27

November 11, 2026 | Shared GPU results and validator

Adopt [shared results specification v1.0](GPU-TEST-SHARED-RESULTS-v1.0.md).
The complete v1.26 plan follows verbatim. Earlier archives/receipts are preserved.

| Gate | Implementation and acceptance |
| --- | --- |
| SR0 contracts | Distinct GEMM, BF16 attention-core and full quantized MLA/Q3 profile identities; existing oracles/tolerances and ABIs preserved |
| SR1 common format | One dependency-free schema/math module, raw wall/device sample arrays, source/corpus/session bindings, useful FLOPs and candidate/baseline ratio or explicit unknown |
| SR2 measurement | Separate wall/event batches in Q7; retain distinct operation scopes; profiling off by default in both, explicit opt-in independent processes; no compiler search or profiler timings in acceptance |
| SR3 shared validator | One CLI, trusted scaffold source/corpus checks and complete Q7 native audit delegation; recreate shared envelopes, reject ratio/sample/numerical/PM/native-gate contradictions |
| SR4 delivery | Version Q7 v1.4, scaffold v2.0 and unified receiver v2.0; byte-identical shared modules; unchanged kernels/inputs/oracles/runtime; extracted-package and unavailable-device/return replay |
| SR5 NVIDIA acceptance | Actual CUDA build/execution, six-output Q7 and BF16/GEMM correctness, required race lanes, unprofiled timing, optional counter/PM collection; no host-only GPU claim |

Q7 has no matched GPU baseline and no approved full-boundary useful-FLOP formula;
both remain explicit unknowns. AOT selection, dispatch and JIT remain deferred.
The existing COMMENT workaround and fallback/lease/publication rules remain.
Roles TBD. Compiler production does not need to rerun for this reporting change.

---

## Preserved v1.26 plan

# AI compiler optimization search implementation plan v1.26

November 11, 2026 | Q7 diagnostic performance measurements

Adopt [Q7 profiling specification v1.0](AI-COMPILER-MLA-Q7-PROFILING-v1.0.md).
This is an additive harness milestone. The complete v1.25 plan follows verbatim.

## Q7h profiling implementation and qualification

| Gate | Implementation / acceptance evidence |
| --- | --- |
| P0 evidence model | Versioned compute-wait, data-path and data-origin groups; measured, derived, proxy, estimate and unknown labels; scope, units, denominator and source counters; unknown numerical values are null |
| P1 isolated capture | Separate profile process after numerical/sanitizer/timing qualification; one fixed selected Q3 request bracketed by Driver profiler APIs; setup, guard exercises, reference transfers and warmup outside capture; no CUDA timing events or benchmark samples |
| P2 counter capability | Query installed NCU metrics; exact allowlist, original version/query/stdout/stderr; native report and raw base-unit CSV; explicit cache-flush/replay/clock policy; unavailable metrics/permissions are diagnostic gaps |
| P3 movement/origin | Per-stage HBM read/write, L2 traffic/utilization, supported device-scoped NVLink/PCIe bytes; host-interface snapshots; topology and optional independent Nsight Systems trace. Local/peer/remote exposed wait remains unknown without causal dependency evidence |
| P4 receiver | Recompute rates and issue inactivity, check stage names/order against authenticated code metadata, child references/device identity and profile isolation; reject forged causal attribution, labels, units, nonfinite values, duplicate rows, extra kernels, failed-profile measurements and mixed timing |
| P5 portable release | New versioned execution and receiver archives; preserve generated CUDA/PTX/WHIRL/cubins, runtime and oracles byte-for-byte; CPU contract/process tests, unavailable-tool/device replay and archive round-trip; original archives immutable |
| P6 NVIDIA acceptance | Run on capability 9.0 hardware with required Driver/CUDA; inspect actual counter support, native reports, all three stage launches, reference/race checks and benchmark samples; no real-device qualification is inferred from synthetic fixtures |

Diagnostic availability has a separate status. Missing counters do not change
existing numerical or benchmark measurements; contradictory evidence is a
validation failure. Primary timing never incorporates profiler durations.
There is no matched GPU baseline in this packet, so no speedup ratio is claimed.
No compiler search, candidate materialization, runtime dispatch, JIT or AOT
selection is added. General PREOPT COMMENT repair remains separate. Roles TBD.

---

## Preserved v1.25 plan

# AI compiler optimization search implementation plan v1.25

November 10, 2026 | Q7 portable NVIDIA qualification harness

Adopt [Q7 qualification specification v1.0](AI-COMPILER-MLA-Q7-QUALIFICATION-v1.0.md).
The complete v1.24 plan follows verbatim. Original documents and receipts remain intact.

## Q7 preparation and hardware acceptance

| Gate | Deliverable / required evidence |
| --- | --- |
| Q7a portable delivery | Authenticate full Q6d/Q6c receipts; retain all 14 fixed compiler-generated CUDA/PTX/cubins, COMMENT, Low B/full traces, typed inputs and three independent oracles; tar with SHA256SUMS and external checksum |
| Q7b actual binding | Reuse Q6d/Q3 with actual context and three explicit streams; link-only SDK stub, no model symbols; unchanged legacy model entries; no new be.so dependencies/exports |
| Q7c device correctness | Six outputs, fixed tolerances, guards, immutable inputs, fresh Q3 observations, whole publication/consumer contents, prepared cancellation, duplicate refusal and actual source guard; default both address residues including long/tail/zero-active cases |
| Q7d race qualification | Separate real-GPU memcheck/racecheck/synccheck/initcheck children; require bulk/proxy/shared instrumentation capabilities, clean summaries and exact child matrix; missing/older tools are pending coverage |
| Q7e performance | Correctness before/after warmup/sampling; persistent buffers/fresh requests; host and explicitly scoped stream spans; raw samples/device facts/clocks/load retained; no profiled times mixed into normal results |
| Q7f portable replay | Rebuild source closure and all generated code offline; run controls, upstream Q6d regression and unavailable-device refusal; preserve source bytes/identities and review receipt; no GPU claim without hardware |
| Q7g returned evidence | Friend/operator returns complete reports/logs, tar identity and recorded device/tool environment; review numerical/race/resource/performance results before hardware acceptance |

The current machine has no NVIDIA GPU. Q7a/b/f can complete here; c/d/e/g
remain actual-device gates. Compiler-only, Driver-model or unavailable-device
reports never certify GPU correctness, races or speedup. Retained native cubins
are the only authorized executable images; compile checks do not silently
substitute toolchain-dependent rebuilt images. The host binding rebuild retains
pure compiler object/be.so dependencies and is not a complete Open64 rebuild.

Preserve COMMENT, Q3 lifetimes and parent publication, estimation-only search,
selected-only lowering and source fallback after safe drain. AOT selection,
phase dispatch, JIT and serving-engine import/packaging remain deferred. Roles
remain TBD. Original immutable evidence is never overwritten.

---

## Preserved v1.24 plan

# AI compiler optimization search implementation plan v1.24

November 10, 2026 | Q6d device execution and Q3 publication bindings

Adopt [Q6d device binding specification v1.0](AI-COMPILER-MLA-Q6D-DEVICE-BINDINGS-v1.0.md).
The complete v1.23 plan follows verbatim. Earlier documents/receipts remain intact.

## Q6d implementation and acceptance

| Gate | Implementation and evidence |
| --- | --- |
| Authority reuse | Extend Q3 with explicit device I/O and private split completion; preserve host callback and public facts ABI, allocation generations, leases, source/page/scale observation and atomic parent publication |
| Code admission | Owner-approved three Q6c native keys and digest association; owned native-cubin hash before load; actual emitted entry names (including canonical renames); exact source, launch, loaded resources and full pointer ABI |
| Device storage | Owned cuMemAlloc buffers; actual context/device/type/base/extent/buffer ID; Q3 generation and content versions; no host dereference of device addresses; refuse same-address reuse |
| Readiness and completion | Explicit nondefault producer/execution streams with observed context/stream IDs, capture refusal; owned ready event for uploads and one completion event after ordered prepare/attention/finalize launches |
| Fresh runtime binding | Instantiate already-selected code against actual device Q3 facts and exact fresh snapshot; compilation's host snapshot is provenance only; no new search or optimality claim |
| Publication | NOT_READY retains every lease; actual completion/status/control/scale/parent recheck gates one atomic four-buffer successor; competing parent completions have one winner |
| Consumers | Whole-bundle device reader and consumer-stream wait; immutable ready event; seal consumer sequence once; release after actual event/drain; retain unknown lifetimes and cleanup failures |
| Failure and fallback | Duplicate launch refuses; partial launch/cancel denies publication; uncertain I/O/kernel drain retains staging/module/buffers/ticket; safe source fallback only after cancellation/drain; retry physical cleanup exactly once |
| Qualification | Real SDK compile, delayed Driver with opaque device addresses and actual Q6c kernels, source/PyTorch/scalar references, paged/append/tails/zero-active/8193-token inputs, ABI/authorization/generation/consumer/parent-CAS negatives |
| Preservation | Authenticate unchanged Q6c WHIRL/CUDA/PTX/COMMENT material, Q3/Q4/search regressions, Q5 trace equality, no new be.so dependencies/exports, explicit optional archive without mock/test-counter APIs, immutable closed final-path replay |

Q6d qualification is software/driver-model evidence. Q7 remains NVIDIA execution,
race/memory/numerical/resource/performance validation; there is no GPU execution
or speedup claim. Owned device allocation is the current boundary; a vendor
engine borrowed-allocation import requires a separately reviewed ownership ABI.

Preserve two-phase search and source fallback. Q6c's resource/layout and strict
numerical limitations remain. AOT selection, phase selector/parser, dispatch
threshold, JIT and serving-engine packaging ABI stay deferred. General PREOPT
COMMENT retention repair stays separate. Personnel roles remain TBD.

---

## Preserved v1.23 plan

# AI compiler optimization search implementation plan v1.23

November 10, 2026 | Q6c selected GPU realization

Adopt [Q6c GPU lowering specification v1.0](AI-COMPILER-MLA-Q6C-GPU-LOWERING-v1.0.md).
The complete v1.22 plan follows verbatim. Existing master documents and receipts
remain intact.

## Q6c implementation and acceptance

| Gate | Implementation / required evidence |
| --- | --- |
| Source admission | Exact reopened Q6b/Q5/control/Q3/layout identities, explicit O3, one active PU, no live dependence graph; refuse stale or unsupported inputs without another candidate |
| Global dependencies | Split actual native statements into ordered prepare, attention and finalize launches; uniform status guards; original formal order/scratch and functional cache updates retained |
| Ownership | One attention CTA per batch/query/head-group; 32G threads; cooperative token dequantization, one ordered fold owner per head, padded lanes reach collectives |
| Memory and instructions | Native runtime residue partition, exact synchronous heads/tails or whole fallback, aligned 16-byte bulk middles, bulk-only expect counts, proxy fences at init/reuse, slot phases, arrive/wait, CTA raw/decoded publication, consumer join and invalidation |
| Phase boundaries | Native High -> standard ARRAY/SCF memory lowering -> Mid -> physical coordinate/entry mapping -> Low -> whirl2c CUDA and bounded NVISA PTX; seal and independently reopen each level |
| Consumers | Existing sealed-Low backend driver verifies before rewrites; emitter only prints selected semantics; sm_90 nvcc/ptxas/device-link checks; no new be.so API/library dependency |
| Correctness | Fresh Q3-bound callback, original source plus PyTorch/scalar references, all-lane delayed-copy CTA model, reverse schedule, shifted address residues, guards/canaries/nonfinite/tails/8193-token inputs |
| Refusal and retention | Mutated wait/collective/bulk/binding refused by audit and CUDA driver; injected application failure publishes no accepted Low family; full .B/.T, raw diffs, annotated .cu/.ptx, logs, hashes and independent final-path replay |

The delivered workers are compile/link-qualified and host-model checked. Q6d
still owns real device leases/readiness/completion/publication and Q7 owns
NVIDIA execution/race/numerical/performance evidence. No GPU eligibility or
speedup follows from this milestone. The 48 KiB envelope still refuses larger
unsupported layouts. Preparation/finalization remain serial initial mappings;
strict head-owned token folds intentionally limit parallelism.

Keep estimation-only search and selected-only transformation. Model complete
pipeline costs before automatic async/sync comparisons; explicit transport
qualification is not performance search. Keep COMMENT explanations and the
existing PREOPT workaround; general COMMENT retention repair stays separate.
AOT selection stays deferred, including phase selector/parser, runtime threshold,
dispatch policy and packaging ABI. Personnel roles remain TBD.

---

## Preserved v1.22 plan

# AI compiler optimization search implementation plan v1.22

November 10, 2026 | Q6b selected native staging and dequantization

Adopt [Q6b native staging v1.0](AI-COMPILER-MLA-Q6B-NATIVE-STAGING-v1.0.md).
The complete v1.21 plan follows verbatim. Existing master documents and earlier
receipts remain intact.

## Q6b implementation and acceptance

| Gate | Implementation and evidence |
| --- | --- |
| Selected admission | Exact optimized Q5 native/source/descriptor/Q3 identity, canonical control read once, explicit O3, supported resources, one active PU and no live dependence manager; refuse without alternate-plan lowering |
| Native representation | Real DO_LOOP/OPR_ARRAY High WHIRL; aligned shared-role arena; typed TAS pointer views; explicit versioned acquire/copy/wait/publication/consumer-completion/release/drain CALLs with conservative effects; no binary-format or opcode change |
| Exact source behavior | Reuse Q1/Q5 builder, source guards and F32 constants; common physical run splits for all four payload/scale streams; S8/U4 dequantization; source-ordered three passes, guarded tails/heads, masked nonfinite behavior; prefix/projections/cache updates/returns preserved |
| Pipeline ownership | One or two raw slots and one decoded tile; wait current, prefetch next into other slot, decode/publish/consume current, release after all consumers; drain before leaving each logical work unit; check all source indices before pending transfers |
| Mutation and analysis boundary | Driver-owned before/after snapshots in an isolated compiler process; expire epoch and do not reuse Q1 source facts for selected loops; new locals/TYs/private declarations visible in full diff; source TCON reuse; injected post-build refusal leaves input/before only |
| Native verification | Reopen and check full native/source/layout/control/Q3 bindings, shared inventory, private operation versions, pointer-view types and formal cache traits; altered waits/publication/consumer/release or TAS type reject |
| Comments and consumers | Generate walkthrough as actual entry COMMENT; extend only the established PREOPT restoration workaround; ordinary backend consumes the High body and rebuilds analyses; whirl2c prints actual High/backend code and comments |
| References | Delayed-copy/poison/range/lifetime host model linked only to review libraries; strict High and backend output comparisons against original source, PyTorch and independent reference through Q3; genuine short/8193-token cases, zero-active/nonfinite/guard inputs and canaries |
| Isolation and compatibility | Standalone Q6 builder instantiation reuses the existing private High builder; no new be.so dependency/API or frontend leakage; recheck source-only/selected Q5 traces and search controls; immutable evidence and independent final-path replay |

Q6b's result contains abstract staging operations. Its manifest explicitly
requires an operation implementation/lowering and denies GPU execution
eligibility. The host review model is not a production runtime. A Q3-qualified
host callback retains the original exact source/snapshot join and publication
rules; no device allocation/event authority is inferred.

Q6c remains the next implementation gate: convert native staging through
structured memory lowering, physical thread/CTA/shared-memory mapping and
CUDA/PTX. The owning lowering phase must split actual aligned bulk transfers
and synchronous fragments, calculate hardware transaction bytes, and emit
complete barrier/proxy/wait/consumer/reuse/drain protocols. The emitter prints
those selected native decisions; it must not invent them.

Q6d then binds real device allocations, page/index maps, cache/scale generations,
readiness and completion to atomic result publication. Complete pipeline costing
must precede any automatic comparison of new pipelines with Q5. Q5's synchronous
score does not certify the async W/G as optimal. Q7 remains hardware execution,
race/memory checking and measured performance; no Hopper host is available.

Two-phase search, correctness fallback and COMMENT explanations remain. AOT
selection design stays deferred: no phase selector/parser, runtime threshold,
dispatch policy or packaging ABI. General PREOPT COMMENT repair remains separate.
Primary owner, successor, independent reviewer and approving lead remain TBD.

---

## Preserved v1.21 plan

# AI compiler optimization search implementation plan v1.21

November 10, 2026 | Q6a selected quantized GPU-staging contracts

Adopt [Q6 staging specification v1.0](AI-COMPILER-MLA-Q6-GPU-STAGING-v1.0.md).
The complete v1.20 plan follows verbatim; earlier material remains intact.

## Q6 progress and remaining work

Q6a implements detached selected-only placement admission, exact four-stream
byte partitioning and independent pipeline lifetime verification. A genuine
Q5 selected High image must pass native/source/descriptor/Q3-identity checks.
One query/head-group owns ordered token folds; resource checks include both
raw slots, decoded storage and barrier state. Unsupported resources/controls
preserve the source fallback. No WN/ST/TY changes or candidate exploration
are performed by this boundary.

Q6 is **in progress**, not complete. The Q6a output is a contract report,
not a kernel. Complete Q6b native staging with before/after binary evidence,
Q6c memory/GPU/PTX realization, and Q6d actual device readiness/completion
and Q3-compatible publication binding as specified in the new document.
Only then qualify emitted selected kernels against both references. Actual
NVIDIA execution/race/performance evidence remains Q7/hardware-gated.

Keep pipeline cost/resource qualification separate: a Q5 synchronous model
winner does not automatically stay best after adding raw slots, barriers or
async overlap. Do not introduce per-candidate materialization or hardware
observation during exploration. Model any new pipeline family before selection
and reference-test only the chosen complete legal plan.

For decode, overlap transfer of tile n+1 with dequantization/computation of
n. Track raw completion, decoded publication and last-consumer release
separately. Split physical page discontinuities; use exact aligned bulk middles
and synchronous fragments without overreads. Preserve paired scale/index
joins, strict three-pass arithmetic, status and fallback. An async transfer
does not by itself hide dequantization or guarantee a bubble-free kernel.

AOT selection design stays deferred. Q3 host bindings remain host-only;
no copied snapshot or alignment report grants GPU launch authority. Keep
COMMENT explanations and the established PREOPT workaround. Owner/successor/
reviewer/approving lead remain TBD. No quantized GPU speedup is claimed.

---

## Preserved v1.20 plan

# AI compiler optimization search implementation plan v1.20

November 10, 2026 | Q5 complete strict plans and selected-only High lowering

Adopt [Q5 specification v1.0](AI-COMPILER-MLA-Q5-SELECTED-PLANS-v1.0.md).
The complete v1.19 plan follows verbatim. Earlier documents are preserved.

## Q5 work and acceptance

| Gate | Implementation and acceptance |
| --- | --- |
| Complete source legality | Closed attention tail; exact S8/U4 payload, scale and index SSA; consumer closure, update/result preservation and private tile capacities; floating fake control retains baseline |
| Complete costs | Separately versioned strict three-pass family; source maintenance/guards, projections, attention arithmetic, normalization/statistics, movement and copies all charged. Explicit log cost. Existing fair portfolio/cache/global budgets; no native work during exploration |
| Selected construction | One fixed source/native-bound plan, explicit whole-PU O3 command, no live input analyses; ordered global folds, shared decoded tile across heads, no score/probability/full decoded-cache intermediates |
| Native checkpoint correctness | Reopened PU uses active driver-owned child snapshot protocol, not builder selection; discard unavailable optimizer subsections only in copied PU; initialize scalar type caches; preserve input and before image on failed construction |
| Analyses/explanations | Expire original owned facts; normal backend rebuilds analyses; do not recapture source-form Q1 facts from selected loops. Restore selected WHIRL COMMENT at function entry and emit it through whirl2c |
| Q3 execution | Source/snapshot join before selected callback; all existing real-buffer/page/scale-generation/status/lifetime/completion/publication checks retained; no dynamic dispatch or baked-in page/phase facts |
| Qualification | Selected High and backend output against source/PyTorch/independent reference, canaries and guards. Ten prior source cases plus genuine 8193-token inputs, stale/refused requests, unknown/floating baseline, legacy Q3/Q4/search/consumer regression. Immutable receipt and final-path replay |

This round delivers synchronous scalar High algorithms in the existing source
buffer ABI, applied in an isolated native compiler process. It does not add
an automatic quantized LNO mutation hook or qualify a GPU worker. The old
selected-FP32 LNO transaction is preserved. Normal backend/PREOPT/memory lowering
consumes the selected `.B`; Q6 owns actual device/async mapping and Q7 owns GPU
correctness/performance. Process-isolated failure retains the source fallback;
no in-process rollback claim is made.

Q4's cache-only one-pass hypotheses remain archived analysis studies. Q5
introduces a different qualified strict three-pass family and a complete cost
model. It preserves F32 reduction order without silently enabling reassociation.
Parallel split reductions require their own numerical/materializer contract.

**AOT selection design remains deferred.** No phase CLI option parser,
dispatch location, selection threshold, policy or packaging ABI is added.
Owners/successor/reviewer/approving lead remain TBD. General PREOPT COMMENT
retention repair stays separate. No GPU speedup is claimed without a GPU.

---

## Preserved v1.19 plan

# AI compiler optimization search implementation plan v1.19

November 10, 2026 | Q4 quantized/paged cache-service costing

Adopt [Q4 specification v1.0](AI-COMPILER-MLA-Q4-QUANTIZED-MOVEMENT-v1.0.md).
The complete v1.18 plan follows verbatim. Existing master documents are retained.

## Q4 implementation and acceptance

| Gate | Concrete implementation and acceptance |
| --- | --- |
| Live evidence export | Q3 prepared-ticket reobservation, parent/generation validation and owned snapshot; exact freshness revalidation outside exploration. Diagnostics alone never authorize consumption |
| Movement model | S8/U4/floating-control payload and paired scales; allocator-page/compute-tile intersections; source maintenance, conversion/unpack, decoded global/shared traffic, padded resources and split tuple traffic; no free shared-prefix or zero-work credit |
| Explicit uncertainty | Unknown GPU alignment gets a conservative sector envelope. Fixed declared cold/warm hierarchy scenarios; no architecture-implied bandwidth, cache hits or async/TMA overlap. Reports separate logical/physical/valid/active facts |
| Search integration | New exact 17-entry analysis-only catalog uses existing bounded fair portfolio/shared cache. Legacy MR2 identities/scores and selected routes preserved. Unsupported gap/economic certificate stops refused |
| Application boundary | Model incumbent is a cache-service hypothesis, never an executable plan or total MLA latency. Q5 must complete legality, timing, materializer admission and selected-only reference checks. No per-candidate lowering/regression |
| Explanations | Q4 obligations join existing High/PREOPT/whirl2c source COMMENT explanations; detached candidate reports explain proposed movement. Keep general PREOPT COMMENT repair separate |
| Validation | Focused native arithmetic/resource/budget/negative tests; actual Q3 host-bindings across ten source cases, stale export checks, reference execution; full Q1 production comments/executable parity, controls/MR2/legacy fallback/consumer closure; immutable receipt and final-path replay |

Q4 currently estimates cache service, excluding attention/merge arithmetic and
projections. This scope is deliberate and recorded in every study report;
its incumbent cannot bypass Q5 with an existing FP32 kernel recipe. The
source-only production compiler has no live Q3 engine witness. No model winner
is automatically dispatched or materialized. Unknown/ineligible studies retain
existing source behavior and fallback, without packed-input reinterpretation.

Q5: qualify complete synchronous quantized plans and legal selected-only
High WHIRL application with rollback, fresh analyses and both references.
Q6: qualify asynchronous TMA/dequantization, device layout/alignment, barriers,
completion and cache/scale publication. Q7: actual GPU correctness/profiling.
No Hopper access is available; no measured speedup is claimed.

**AOT selection design remains deferred by explicit user direction.** Preserve
separately qualified prefill/decode AOT variants as the design direction;
implement no selector, phase option parser, packaging ABI, threshold or policy.
Existing two-phase search, COMMENT workaround, numerics and fallback remain
intact. Owner/successor/reviewer/approving lead are TBD.

---

## Preserved v1.18 plan

# AI compiler optimization search implementation plan v1.18

November 10, 2026 | Q3 native serving binding and phase-specialized AOT direction

Adopt [Q3 serving binding v1.0](AI-COMPILER-MLA-Q3-SERVING-v1.0.md) and the new
[phase-specialized AOT direction v0.1](AI-COMPILER-MLA-PHASE-SPECIALIZATION-v0.1.md).
The complete v1.17 plan follows verbatim as historical material.

## Adopted phase-specialized direction; selection deferred

The compiler should support separately qualified prefill and decode AOT
specializations, with compiler options declaring their intended phase contracts.
Code alone cannot reliably identify serving intent in our current graphs.
Q=1 prefill tails, Q>1 verification, context length and quantization remain
explicit counterexamples to a phase heuristic. Each variant must qualify its
own geometry, numerics, representation, memory/lifetime and materializer rules.

Per the user's instruction, **the AOT selection mechanism will be designed
later**. This revision chooses no dispatcher location, packaging, selection
ABI, threshold or policy, and implements no phase CLI flag. The earlier
illustrative option spelling is not a stable or implemented parser contract.
Q3 provides correctness foundations and does not select a variant. Do not close
this open design item with a label, callback, publication CAS or length test.

## Q3 implementation and acceptance gates

| Gate | Concrete implementation and acceptance |
| --- | --- |
| Source ABI descriptor | Reuse native Q1/Q2 admission; derive actual root and cache/scale copy joins, page/index formula, pool/table/slot shapes and legal P. Pure schema discovery changes no native state |
| Live allocation binding | Register actual host spans with registry-issued allocation generations/content versions. Check full disjoint ABI, status and metadata; reject overlaps, stale/foreign handles, unsupported memory/stride/layout and premature writes/reclamation |
| Workload/page joining | Read actual metadata, mask, table, tokens and slots under leases. Validate Q2 facts, page-local byte bounds and tails, independent scale domains, unique append slots and retained-prefix safety. P remains allocator layout, not tile W |
| Publication/lifetime | Atomic initial owner-ready import; shared input/exclusive writable leases; status-sentinel synchronous callback; completion/control/scale and parent-CAS gates; complete four-component successor publication; old-reader pinning and explicit reclamation |
| Compiler/explanation boundary | Runtime registry stays outside be.so and search. Native High/post-PREOPT comments describe unbound compile-time state and runtime obligations; existing whirl2c prints them. No binary layout/opcode/interface or fallback change |
| Qualification | Ten actual bound source entries/reference checks; version/ABA/page/scale/alias/prefix/refusal/teardown tests; real concurrent callbacks and single-winner publication; complete Q1 production matrix, Q2/search/control/movement/native invalidation and legacy/consumer audits |
| Durable review | Commit explicit verified paths; retain B/T/raw diffs, C/host libraries, allocation/publication reports, metadata cases, exact commands and hashes; independently replay from final immutable receipt before indexing |

The delivered adapter is host-synchronous and source-compatible. It does not
claim a vendor engine deployment, GPU pointer/event binding, asynchronous
publication, shared cross-registry variant ownership or hardware qualification.
Initial import is the external owner's readiness root; subsequent version and
lifetime enforcement is native. Unknown/incomplete bindings refuse or retain a
compatible generic route, without partial publication or representation casts.

Q4 can consume eligible detached movement facts after the corresponding live
bindings are valid. Q5 owns selected quantized realization and independent
prefill/decode algorithm/materializer qualification. Q6 owns TMA/dequantization
and device-ordering rules; Q7 owns GPU correctness/performance. AOT selection
is an explicit separate deferred design task, not implicitly part of Q3.
Existing two-phase search, budgets/convergence, selected-only application,
correctness checks, COMMENT workaround and fallback remain intact. Personnel
and independent review roles remain TBD.

---

## Preserved v1.17 plan

# AI compiler optimization search implementation plan v1.17

November 10, 2026 | Q2 phase and workload inference

Adopt [Q2 workload specification v1.0](AI-COMPILER-MLA-Q2-WORKLOAD-v1.0.md).
The complete v1.16 plan follows verbatim as historical material. Its Q2-open
statements describe the pre-Q2 boundary, not an engine adapter delivered here.

## Q2 implementation and acceptance

| Gate | Concrete result and acceptance |
| --- | --- |
| Source geometry | Pure VHO service reuses Q1 admission and recognizes connected attention/normalization/output dataflow. Copy capacities, source identities, cache ancestors and update-item counts. Unknown/multiple/unrecognized domains remain explicit unknown; names, dtype and Q never select phase |
| Invocation semantics | Typed versioned per-request witness validates source identity, active query/valid/history lengths, segment offsets, positions, exact mask bits, causal/window visibility and logical append order. Known labels require bound evidence and consistency; fixture intent remains unknown. Mixed, ragged, verification, Q=1 tails and zero active queries are explicit |
| Freshness | Revalidation compares complete input/result seals. Source-only production records live in existing Q1 native owned snapshots and expire/rebuild across real PREOPT and later mutation. No serving pointer/generation claim is manufactured |
| Native explanations | Emit Q2 source geometry/unknown-phase COMMENT at High construction; regenerate through the existing Q1 post-PREOPT workaround; preserve actual whirl2c explanations and existing guards/numerics |
| Isolation and fallback | Analysis is read-only, outside search. Unknown workload evidence cannot reject valid Q1 compilation. No candidate, dispatch, launch elision, JIT, runtime scan or packed-input reinterpretation is enabled |
| Qualification | Native positive/unknown/refusal cases, repeated-inference/native-state checks, retained source execution for altered masks, full Q1 production matrix, exact executable/legacy compatibility, controls and selected-reference tests, shared backend closure. Retain raw B/T diffs and immutable final-path replay |

The implementation supports conditional inference under a native invocation
owner's binding contract. Current standalone source B carries no such runtime
witness: its production phase correctly remains unknown even when geometry is
known. Review JSON exercises the native API and is explicitly not engine
buffer authority. Q3 remains responsible for binding actual serving buffers,
page layout, cache/scale versions, allocation generations, physical append
slots, concurrency, publication and runtime fallback. The old BHSD decode
profile remains intact; it is not reused as a false MLA/shared-latent proof.

Q4 can later consume eligible geometry and proven Q3 facts for data-movement
costing. Q5 realizes only the selected legal synchronous quantized plan and
runs correctness afterward; Q6 owns async TMA/dequantization; Q7 owns real
device correctness/performance. None is enabled by a serving phase label.
General PREOPT COMMENT retention remains separately deferred. Existing AOT,
two-phase search, convergence/global budgets, fallback and prior materializers
remain unchanged. Personnel/independent review gates and roles remain TBD.

---

## Preserved v1.16 plan

# AI compiler optimization search implementation plan v1.16

November 10, 2026 | Production Q1 entry routing and PREOPT rebinding

Adopt [Q1 production specification v1.0](AI-COMPILER-MLA-Q1-PRODUCTION-v1.0.md).
This revision preserves v1.15 below verbatim. Statements there that production
routing/rebinding are open describe the earlier standalone-tool milestone.

## Implementation milestones and acceptance

| Milestone | Implementation and acceptance |
| --- | --- |
| P1 entry admission | Normal be consumes original native quantized/paged source B under explicit buffer-v1 ABI. Require standalone scope and strict numerics, preflight before construction, preserve ordinary refusal/default behavior. No model-name dispatch or external prepare tool |
| P2 semantic integration | Reuse native Q1 source capture and High builder in backend. Driver owns resident PU scope and writer selection. Preserve source interface records in process; use existing terminal standard writer for physical output. O0 remains semantic-only |
| P3 persistent joins | Versioned bounded canonical program metadata, full source seal and per-operation induction metadata survive mapped I/O. No COMMENT-based proof. Unknown/lost joins remain unknown; stale or malformed bindings refuse |
| P4 real PREOPT lifecycle | Observe before actual PREOPT, invalidate, install returned root, annotate, bind fresh actual sites using unique live DU resolution where available, verify read-only capture, invalidate before next mutation. Handle LNO, proactive second PREOPT and driver preopt-only paths |
| P5 correctness and isolation | Ten native inputs, O0/O1/O2/O3 representative coverage, trace-off and preopt-only. Execute real whirl2c High/post-PREOPT/final scalar output against both references with guards/canaries. Negative metadata/alias/epoch/join tests, legacy exact traces/keys, existing estimation-only tests, backend symbol/dependency/consumer audit |
| P6 durable review | Commit source/tests/specification with explicit paths, preserve unrelated user edits and previous versions. Retain actual source/High/PREOPT/output B/T, raw diffs, generated C, host libraries, manifests, tool/source hashes, exact commands, logs, refusals and immutable parent input receipt. Independently replay from final receipt without original build tree before index/current publication |

P1-P4 are implemented; P5-P6 acceptance is recorded by the retained validation
and final-path replay receipts, not by this plan alone. The new route is automatic
within an explicitly opted-in standalone physical ABI; it is not an arbitrary
call-graph ABI conversion. Failure to derive fresh facts must not fail otherwise
valid ordinary code or silently authorize an optimization.

## Remaining work after this integration

Q2 must derive proved/unknown serving phase and workload contracts. Q3 must bind
allocator/page layout, cache and scale generations, ranges, concurrent readers,
publication and runtime fallback with a versioned adapter. Q4 must use these
facts for movement/reuse costing, preserving unknown traffic. Q5 must realize
only the selected legal synchronous quantized plan and run correctness after
application. Q6 owns async TMA/dequantization and pipeline legality; Q7 owns
real device correctness and performance. Independent M7-M9/MR3-MR6 gates and
personnel acceptance remain open. AOT, data movement/reuse priority, two-phase
search, global convergence budgets, COMMENT workaround and existing fallback
remain unchanged. No global optimum or unmeasured GPU speedup is claimed.

The existing x86 SIMD-to-C emitter limitation is not expanded into a Q1 task:
compile the default SIMD route separately, use scalar LNO for final whirl2c
execution tests, and retain actual scalar High/PREOPT code for algorithm review.
General PREOPT COMMENT preservation remains a separate effort requested by the
user. Owner/successor/reviewer/approving lead remain TBD.

---

## Preserved v1.15 plan

# AI compiler optimization search implementation plan v1.15

November 10, 2026 | Q1 guarded common High and owned cache facts

Adopt [Q1 High/owned-facts specification v1.0](AI-COMPILER-MLA-Q1-HIGH-OWNED-FACTS-v1.0.md).
The complete v1.14 plan follows unchanged as historical material. Its statement
that the Q1 semantic High/provider exit was open describes the November 9
source-only milestone; this revision records the new explicitly admitted path.

## Current Q1 implementation boundary

The opt-in native source -> detached semantic recipe -> fresh common High route
now expands the admitted ten source graphs using signed DO_LOOP/ARRAY, proper
byte storage/promoted arithmetic, strict F32 encoder/attention order, guarded
page/index operations and fresh functional updates. The final status argument
reports runtime refusal. It is O0 semantic lowering, outside candidate search.

The LNO-owned observer copies native storage/access/loop identities, shapes,
strides, payload bytes, source/scale local versions, functional-successor origin
and lexical lifetime endpoints. Existing epochs and complete native identity
bind the observation. Indirect/conditional memory remains explicit; dependence
legality, allocator generations and cross-head reuse authority remain unknown.

The implementation and focused qualification are in the new specification and
retained packet. Actual whirl2c-emitted host C is compiled and compared with
source and the independent expanded reference; source-only interpretation is
not the evidence for High semantics. Mutation/alias/scale/lifetime refusals and
legacy fallback are separate checks. No GPU performance claim is added.

Qualification: ten compiled High cases/sixty results; 767 owned storage roots,
1761 static memory sites and 37 indirect sites; seven malformed-handoff
refusals; seven focused legacy/search tests; 181 standalone control/movement
assertions; two legacy and four Q1 backend form checks. Legacy full native
traces and typed executable identities agree; raw image byte determinism is
not claimed. Shared backend dependencies/undefined symbols are unchanged.

## Next phase ordering and acceptance

| Gate | Status and concrete next work |
| --- | --- |
| Q1 source/native contracts | Existing complete source receipt preserved; no reader/writer or mapped layout change |
| Q1 semantic High/provider | Explicit tool route implemented and host-qualified; native facts remain structural and invocation-local |
| Production Q1 integration | Still open: versioned be_driver source/ABI route, canonical operation joins surviving PREOPT, fresh post-PREOPT observer lifetime and explicit refusal trace. Do not broaden affine dependence admission or silently replace the existing fallback |
| Q2 workload/phase | Derive proved/unknown prefill/decode facts from source/runtime contracts; Q=1 and byte dtype are insufficient alone |
| Q3 serving binding | Bind runtime status/fallback, paged allocator layout/alignment, cache/scale generation, valid ranges, unique updates, concurrent readers and publication. Honor buffer ABI or implement a separately reviewed adapter |
| Q4 costing | Consume owned conditional/indirect access and scale/version facts with serving proof. Keep unknown traffic unknown; retain estimation-only exploration and global budgets |
| Q5 selected realization | Apply only the selected legal synchronous quantized plan; rebuild invalidated facts and run reference regression. Scalar Q1 correctness does not authorize reassociation/grouped reuse |
| Q6/Q7 | Async TMA/dequantization legality/resource pipelines, then real device correctness and performance; no Hopper machine currently available |

AOT remains the baseline. Existing search space, models, controls, convergence
strategies, atomic selected-plan application, COMMENT workaround and fallback
remain unchanged. Primary owner, successor, reviewer and approving lead stay TBD.

---

## Preserved v1.14 plan

# AI compiler optimization search implementation plan v1.14

November 9, 2026 | Q1 native source ingestion and inferred cache traits

Adopt [design v0.8](AI_compiler_optimization_design_v0.8.md),
[reuse specification v1.1](AI-COMPILER-MLA-DATA-MOVEMENT-AND-REUSE-v1.1.md)
and [methodology v1.5](AI-COMPILER-OPTIMIZATION-SEARCH-METHODOLOGY-v1.5.md).
The full v1.8 plan still follows verbatim as historical material. This revision
preserves the MR1/MR2 baseline. Q0 source capture, census and contract definition
are complete. Q1 native source ingestion/inference is delivered in the scope
below; its canonical High/owned-provider exit remains open. Q2-Q7 remain planned.
There is no executable quantized optimization, engine paging adapter, phase
inference, TMA implementation or new hardware performance evidence.

## Q1 native source implementation update — November 9, 2026

Adopt [Q1 native ingestion and inference v1.0](AI-COMPILER-MLA-Q1-NATIVE-INGESTION-v1.0.md)
and [frontend ingestion plan v1.1](../osprey/torch2whirl/MLA-QUANTIZED-PAGED-INGESTION-PLAN-v1.1.md).
v1.13, its Q0 definitions and every historical appendix remain preserved.

All ten bounded quantized/paged/fake sources now produce genuine .B/.T. Seven
logical IDs49-55 and six additive v2 schemas preserve old meanings and physical
WHIRL layout. New source schemas have no executable lowering capability. Exact
native construction and independent mapped checks validate carrier/type/shape/
attribute/effect contracts, including pure scatter.v2 without modifying edges.
Source pack/unpack joins and integral-conversion proofs prevent accidental
reinterpretation of arbitrary bit operations or truncating/wrapping casts.

A native source-frontend query runs on copied logical records before native PU
products. It derives content roles, decoder encoding and paired scale SSA from
actual QDQ/contraction/gather/update uses. The mapped observer repeats the query
on actual reopened operators. These are source facts, not inferred serving phase,
physical storage equality, publication/lifetime facts or owned High reuse seals.
Fake quantization gets no packed descriptor; incoming decoder use does not prove
an external producer used RNE127/RNE7. Missing evidence remains unknown and
contradictory declarations or stale scale-update joins refuse.

The ten native programs have 731 operation nodes and 98 rederived source facts.
All six results per case agree at zero tolerances with eager source under the
mapped review interpreter and retain the independent Q0 expanded-reference
checks. The 14 new native tests include legacy boundary checks; the combined
focused lane passes 37 tests. The 139-test torch lane passes. The 355-test native
Python lane has 347 pass, seven skips and the unchanged FHE authority-digest
failure. Syntax/physical-opcode checks, old source-image reopens, FP32 short/long
source-to-High compatibility and 181 search/movement assertions pass. Backend
shared dependencies and undefined symbols are unchanged. No GPU evidence exists.

Q1 source ingestion requested in this round is delivered. Do not silently close
the broader Q1 High-provider acceptance: its next step must lower these admitted
source operators straightforwardly into the common High algorithm and derive
owned access, scale-version and memory facts there. Retain before/after B/T and
raw diffs, byte/promoted arithmetic and indirect page boundaries, canonical-loop
admission, read-only fact collection and conservative unknown/alias behavior.
This unoptimized boundary must precede Q3/Q4 loop bindings. Q2 phase/workload
analysis may proceed from the source contracts in parallel. Q5 still owns the
selected synchronous quantized optimization, Q6 async pipelines and Q7 hardware
acceptance. Source-only inference must not enable any of them.

Existing source/fact keys, MR2 binding/control/model versions, G1 eligibility,
search census, budgets and selected-plan atomic application remain unchanged.
Capture/oracle/profiling are outside estimation-only exploration. The old FP32
path still materializes High; the old family refuses quantized input before a
recipe is published. Q1 provides no native executable quantized fallback yet;
that fallback must accept the actual representation when its gate is implemented.
COMMENT/PREOPT behavior and AOT as baseline remain unchanged. Transfer roles TBD
and M7-M9/MR3-MR6 hardware/independent gates remain open.

## Q0 implementation update (historical) — November 9, 2026

Adopt the [Q0 audited operator census](AI-COMPILER-MLA-Q0-OPERATOR-CENSUS-v1.0.md),
[native requirements v1.0](AI-COMPILER-MLA-Q0-NATIVE-CONTRACTS-v1.0.md),
[serving fixture contract v1.0](AI-COMPILER-MLA-Q0-SERVING-CONTRACT-v1.0.md)
and [frontend ingestion plan](../osprey/torch2whirl/MLA-QUANTIZED-PAGED-INGESTION-PLAN-v1.0.md).
They supplement the quantized/paged/TMA specification without changing its
pipeline, two-phase search or compatibility rules. v1.12 remains historical.

Ten real standalone CPU tensor programs cover signed-byte and packed-U4
quantization, dense/paged read, functional append, multi-query prefill/verification
and fake quantization. Each captures/replays FX and normalized serialized ATen;
795 nodes per representation are fully classified. The independent expanded
K/V reference checks output/LSE and exact cache/scale state for retained finite
cases. Q=1 prefill-tail and decode have identical graphs: phase evidence must
remain independent. No source cache trait annotation supplies hidden facts.

The audit requires explicit typed quantization, pack/unpack, gather and integer
index contracts. Functional index_copy needs pure common.scatter.v2; existing
modifying scatter.v1 is preserved. gather's marker alone is insufficient.
No logical IDs or new schemas are registered by Q0. Review-only serving v1
fixes byte order, scale layout, page translation, private append/version joins,
publication/lifetime and unknown-phase semantics, and denies GPU/TMA alignment
or native capability. It is not an engine adapter.

Q0 is complete for source census plus versioned contract definition. Its prior
original-native B/T acceptance is explicitly deferred to Q1/Q3: production
native/mock ingestion still refuses all ten graphs before .B publication.
Source .pt2 receipts are not WHIRL. Fresh FP32 short/long .B/.T reopen in separate
processes and retain cache traits, qualifying the old bridge/fallback. A
multi-export diagnostic's long image reader was killed (-9); isolated producer
and reader pass. Same-process native lifetime robustness is not repaired here.

Twelve focused and 139 torch capture tests pass. The 341-test Python lane has
313 pass, 27 skips and the unchanged FHE v0.10 authority-digest failure. Retain
this qualification. No new candidate exploration, native table mutation in
search, optimization transformation, TMA, PTX or GPU performance is implemented.
Q1 is the next implementation gate; Q2/Q3/Q5 and their unsupported-path tests
must precede eligible quantized optimization. MR0 measurement, MR3/MR4, M7-M9
and TBD independent transfer roles remain open.

## Status, priority and dependency order

M1-M6 remain complete only for their documented absorbed-MLA compiler/host/
assembly scope. M6 is the production fallback and source of the fixed baseline.
The legacy production catalog retains 12 scalar-F32 G=1 descriptors; the new
MR2 census adds 24 analysis-only grouped descriptors. M7 GPU
correctness/performance and baseline calibration, M8 strategy/provider study and
M9 independent transfer remain open. MR1 now has an implemented initial dense-affine owned-reuse provider; its exact proof scope and fallback boundary are recorded below. MR2 now has an initial detached hierarchy/reuse model and a capability-gated census; MR0 and MR3-MR6 remain open.
Q0-Q7 below extend the fact/model/materializer/pipeline/acceptance boundaries
without renumbering or silently closing any old gate. The preceding
[v1.11 plan](AI-COMPILER-OPTIMIZATION-SEARCH-IMPLEMENTATION-PLAN-v1.11.md)
remains unchanged. Quantized/paged inference and TMA currently have no eligible
production family in this lane.

Prioritize reusable cache traffic and cross-head latent reuse before further
unmotivated barrier micro-tuning. Select for complete latency and retain G=1;
do not impose a minimum-byte winner. Choose G, chunk W, column tiles, ownership
and layout jointly because fewer head groups also means fewer independent CTAs.

    M6 baseline -> MR0 measurement + MR1 owned facts
               -> MR2 detached census/model + MR3 verified selected rule
               -> MR4 selected GPU acceptance
               -> MR5 optional pipeline/compute families
               -> MR6 equal-budget study and transfer evidence

MR1 and analytical MR2 can progress without a GPU using explicit estimated
inputs; they cannot claim MR0 measured closure or enable an unimplemented G>1
winner. MR0 is also the start of existing M7 baseline GPU acceptance. MR3 rule
fixtures are not a candidate-by-candidate exploration loop. Production eligibility
requires MR1/MR2/MR3; deployment acceptance additionally requires MR4.

## MR1 implementation update — November 8, 2026

The [MR1 owned reuse implementation](AI-COMPILER-MLA-MR1-OWNED-REUSE-v1.0.md)
completes the initial provider: stable formal/storage/view identities, actual
affine cache read sets, head-invariance, incoming/defined versions, complete
copy value flow and conservative source liveness. The old source/fact keys,
v2 bindings, v8 recipes, G=1 estimator and kernel catalog remain compatible.
New owned-memory identities participate in live revalidation. Reports distinguish
native High request counts, fused logical projections and unknown hardware
traffic. Missing traits can use graph-derived roles only with the same native
proofs. Unsupported forms retain existing G=1 search/application.

The provider accepts dense flat accesses and either complete reduction loops
or complete unrolled consumer slices. Multi-store cache-copy producers remain
unproved in v1, with an explicit, correctness-checked fallback. This is a bounded
provider completion, not a claim that every affine program has a reuse proof.
No new grouped candidate or materializer is enabled. MR2 must bind these facts
to its new model/schema and MR3 must implement selected physical ownership and
staging. MR0 GPU characterization remains unavailable without a GPU.

## MR2 implementation update — November 8, 2026

The [MR2 costing specification](AI-COMPILER-MLA-MR2-MOVEMENT-COSTING-v1.0.md)
records the new opt-in binding v3, hierarchy-profile v1, controls v3 and finite
36-entry census. Twelve implemented G1 variants are re-costed under the same
explicit cold/warm profile as 24 analysis-only G={2,4,8} staging/retention ideas.
Grouping is never production-eligible without MR3. Costs distinguish logical
requests, sector envelopes, assumed L2/HBM traffic, SMEM, tuples, merges,
resource rounding, coverage, synchronization and launches. The synchronous
model uses existing readiness/occupancy services and does not assume async
overlap, target-derived hit rates, free spills or padded-head reuse.

A compatible G1 seed consumes the first shared proposal/estimate. The existing
portfolio/cache/global-budget engine handles the larger bounded census;
unsupported legacy relative-gap/economic certificates are refused. The selected
G1 uses a versioned full proof/profile/descriptor seal through the existing
atomic High transaction and M6 route. Old v2/v8 behavior stays reproducible.
Only that selected plan is lowered and checked. Missing MR1 proof disables the
new model with ordinary LNO fallback; old G1 search remains independently usable.

MR3 must now implement and independently validate the proposed grouped physical
ownership, tile lifetime, head/token/column tails, neutral masks and actual
register/shared/barrier requirements before capability eligibility changes.
MR4 must calibrate and accept physical performance on representative hardware;
synthetic estimates cannot close that gate. MR0 measured profiling remains open.

## Quantized/paged cache and TMA implementation sequence — November 9, 2026

Adopt the [quantized/paged/TMA specification v1.0](AI-COMPILER-MLA-QUANTIZED-PAGED-CACHE-AND-TMA-v1.0.md).
Q0 source/census/contracts and Q1 native source ingestion are delivered as
qualified above. Q1 High-provider exit and Q2-Q7 are **open**, not accepted
by the MR1/MR2 results. No old milestone is renumbered. The new work refines the MR1 fact,
MR2 model, MR3 rule, MR5 pipeline and MR4/M7 acceptance boundaries.

    current MR1/MR2 and accepted FP32 G1 baseline
        -> Q0 source census and versioned native/serving contracts
        -> Q1 quantized-cache inference + Q2 workload/phase facts
        -> Q3 paging/allocator ABI and guarded admission
        -> Q4 detached movement/dequantization model and capability census
        -> Q5 synchronous quantized selected rule and reference correctness
        -> Q6 TMA/dequantization selected rule and native boundaries
        -> Q7 selected GPU correctness/performance and transfer
    MR0 fixed-probe measurement/calibration proceeds separately when available

Q1/Q2 may proceed in parallel after Q0. Q3 needs their representation/effect
requirements; Q4 may develop analytical fixtures in parallel but cannot cost
unknown formats/layouts as valid. Q5 can start with G1 independently of pending
MR3 grouped work. G>1 reuse requires MR3's corresponding qualified rule; TMA
does not bypass it. Q6 is the prioritized async provider in MR5, not permission
to enable unverified hardware instructions. Existing synchronous MR3 work and
FP32 deployment need not wait for optional quantization or TMA.

| Gate | Concrete implementation, scope and module ownership | Acceptance / go-no-go |
| --- | --- | --- |
| Q0 census and contract review (source/definition scope complete) | Frontend captures a real small quantized MLA and paged/cache-update graph, including scale/zero-point paths, and ordinary/multi-query source forms. Reuse existing descriptor, effect, prefill/decode and memory/readiness contracts; review normative WHIRL rules. Publish exact schema and capability compatibility table, numerical oracle and supported first formats | Retain source/operator census and original Torch programs; quantized mapped B/T deferred to Q1/Q3 because ingestion refuses. Retain fresh FP32 B/T compatibility evidence. Distinguish actual packed storage from fake quantization; identify unsupported nodes. No enum/layout/trait guesses from model names. Source-to-native APIs, printer/verifier/mapped-image and ordinary fallback obligations agreed before implementation |
| Q1 quantized-cache facts (native source delivered; High provider open) | Native ingestion preserves encoding, quantization operands and lineage; VHO/LNO derives role/storage/scale-version/read-domain facts using owned snapshot services. Keep semantic type properties separate from dynamic scale/page/request state | Positive undeclared/declared roles, contradictory declarations, opaque/erased producers, fake quantization, aliases, group requantization and stale metadata tests. Exact unpack/dequantize oracle cases for each admitted format; no WN/ST/TY/DST mutation during collection; legacy FP32 admission unchanged |
| Q2 workload and phase facts | Owning source/ABI validators derive per-request query/KV lengths, positions, masks, append order and segment bounds; import trusted serving phase metadata with consistency guards. Support unknown/mixed without a dtype/length heuristic | Q=1 prefill tail, decode with short/long history, speculative Q>1, prefix reuse, chunked prefill and mixed/ragged request fixtures. Contradictory metadata rejected or routed safely. No automatic prefill admission into the current single-query materializer; below-O3 and dispatch/guard boundaries tested |
| Q3 paged storage and allocator ABI | Serving engine and native frontend define page-size legal set, pool/strides/auxiliary layout, page table, validity/publication/COW/reclamation and allocation generations. High admission proves page-local source ranges and target alignment, distinguishing indirect page selection from affine in-page offsets | Nonmonotonic/scattered pages, partial/empty pages, separate metadata alignments, group/page boundaries, invalid table entries, page/pointer ABA and concurrent update refusal. P versus W explicit; compiler cannot resize a live cache. Dense equivalent and page-gather reference checks, target-specific 1D/tensor TMA alignment distinctions |
| Q4 detached costing and census | Extend the existing MR2 descriptors, fixed-profile scenario accounting, occupancy and readiness/event services under new exact versions. Cost packed/decoded/scales/page-table traffic, conversion, segmentation/descriptor/setup, slots, dispatch, barriers, occupancy and prologue/drain | Analytical count/overflow and resource fixtures; same profile for all eligible entries; compatible synchronous quantized baseline charged first. Global shared-cache/proposal/unique/time controls and deterministic ties. No native allocation/lowering/emission/compiler/GPU/reference dependencies in exploration. Unsupported TMA/group/format rules excluded; new gap/economic certificates disabled pending proof |
| Q5 synchronous quantized rule | Existing atomic selected High transaction realizes one fixed admitted quantized/paged plan. Start with G1; explicit page lookup, decode loops, scale guards and source precision. A separately admitted generic/reference path handles unsupported optimized families | Selected-only attention checks versus actual quantized source, exact component tests, canonical High admission, all masked/poison/tail/head/query cases, before/after B/T/raw diffs, COMMENT/whirl2c parity, resource refusal/stale seal and rollback. No raw packed bytes sent to FP32 fallback; no new error tolerance or global decoded buffer silently introduced |
| Q6 TMA/dequantization rule | Owner-selected raw/decoded slot graph; explicit issue/completion/conversion/publication/consume/release in High. Fixed Mid memory lowering, Low GPU/target mapping, PTX and whirl2c. Driver/serving ABI supplies tensor-map/launch setup and lifetime | Per-target copy/descriptor/swizzle/alignment, byte/arrival/phase and proxy-order validation; one-slot baseline and multi-slot prologue/drain/reuse cases. Correctness for early exits, scale versions, async consumers and reclamation. Native High/Mid/Low/PTX/launch/source seals, process isolation, completion gating, stale-field refusal and rollback. Hardware-unqualified rule remains analysis-only |
| Q7 device and transfer acceptance | MR4/M7 fixed baseline and one estimated selected winner per declared case. Independently validate the pipeline and compare whole-call latency under held-out page/shape/phase/cache regimes. MR6/M8 equal-budget methodology and M9 transfer remain separate | GPU reference correctness, race/memory sanitizer and paired timing with declared thresholds; actual resources/traffic/conversion stalls versus estimates. Include engine dispatch/setup/segmentation and relevant allocation effects. Failures retain compatible accepted baseline. Publish immutable reproduction/transfer package; personnel roles TBD remain explicit acceptance gaps |

### Exact source work map

The paths below are current extension points, not a claim that new support is
already present. Reuse existing phase services; publish any new API through Q0.

| Owner | Current files / implementation responsibility |
| --- | --- |
| Source ingestion | `osprey/torch2whirl/python/open64_dsc/tensor_ingestion.py`, `tensor_traits.py` and native frontend APIs: preserve real quantization/scale/state/page census through existing opaque native builder boundary |
| Common contracts | `osprey/common/com/dsl_tensor_traits.h`, descriptor facilities, `dsl_contract.cxx`, `dsl_gatekeeper.cxx`: policy-free schema/encoding/effect/structural rules. Existing rank-4 BHSD cache-state profile is not silently redefined as MLA pages |
| VHO semantics | `osprey/be/vho/tensor_kernel_lower.*`, `tensor_attention_pattern.inc`, `tensor_schedule.cxx`: separate admitted quantized/multi-query family recognition and detached fixed schedule; preserve current closed FP32 family |
| LNO analysis/search | `osprey/be/lno/dsl_high_snapshot*`, `dsl_high_reuse*`, `dsl_high_observer*`, `lnopt_main.cxx`; `osprey/be/vho/tensor_attention_movement.*`, `tensor_attention_search.*`: owned proofs, guarded bindings and model/census. Reuse `dsl_gemm_readiness.h` / `dsl_gemm_occupancy.h` and existing memory/lifetime services |
| Selected transaction / driver | `osprey/be/lno/dsl_high_apply.cxx`, `osprey/be/vho/tensor_high_lower.cxx`, `tensor_high_selected.inc`, `tensor_selected_key.h`, `osprey/be/be/driver.cxx`: one atomic fixed High application, analysis invalidation/rebuild, runtime/launch obligations and fallback |
| Native backend / emitters | `osprey/ir_tools/tensor_selected_bridge_driver.inc`, `osprey/be/vho/tensor_selected_bridge.cxx`, `osprey/be/cg/NVISA/tensor_selected_ptx.cxx`, `osprey/be/whirl2c`: same sealed semantics at every level, supported copy/conversion/barrier operations and explanatory COMMENT emission |
| Validation / serving | Existing `osprey/be/lno/tests` review runners and frontend fixtures plus the explicitly identified serving adapter: inference, protocol, model and selected-rule fixtures; an engine connection is required before claiming real paged allocator integration |

### be_driver and native-boundary ordering

1. Verify Very High domain/encoding/effect/page contracts and retain quantization
   lineage before semantic lowering; preserve original source/mapped images.
2. Establish the source algorithm and canonical High loop/memory form. Reuse the
   current qualified post-PREOPT access/DU/alias/full-dependence snapshot point.
3. Harvest Q1-Q3 owned facts once, including scale/page versions, geometry and
   runtime obligations. Account validation separately; reject ambiguous joins.
4. Explore Q4 detached complete plans. Bind one engine page ABI and one declared
   profile. Cache entries, hard budgets and materializer eligibility apply to
   every proposal; compilation, correctness and profiling are unavailable here.
5. Revalidate one eligible selected plan and all original input/proof/model/
   capability/guard identities. Stage native page/transfer/dequantization/event
   semantics atomically. Rebuild invalidated analyses and verify High exit.
6. Lower structured memory/control in Mid; bind fixed owners/copy/barrier/
   target conversion instructions in Low; verify and emit native PTX/whirl2c CUDA.
   Neither backend nor emitter reselects P/W/G, precision or stage depth.
7. The driver/serving adapter establishes tensor-map/update visibility, page/
   scale publication and lifetime, checks runtime guards, and dispatches an
   already compiled compatible path. No live-cache page-size rewrite or JIT.
8. Outside search, execute selected correctness/resource checks and required
   hardware acceptance. Failure preserves a representation-compatible accepted
   path; no unbounded next-candidate materialize/test loop.

### Mandatory regression and refusal families

| Family | Required distinguishing cases |
| --- | --- |
| Semantic inference | Undeclared/declared/contradictory cache role, true/fake quantization, erased lineage and opaque calls; physical versus logical dimensions and scale groups |
| Phase/geometry | Per-request Q rather than aggregate batch size; Q=1 prefill tail; Q>1 speculation; existing prefix during prefill; mixed/ragged batches; unknown and stale engine labels |
| Storage/versions | Packed bytes and scale/zero-point/table versions, append versus group requantization, read/write aliases, page-table remap, sharing/COW and recycled page IDs |
| Alignment/bounds | P*r alignment and padded/interleaved alternatives; P != W; page/column/quant-group tails; physical scatter, 1D versus tensor-copy constraints, invalid pointer/descriptor/table entries |
| Protocol/resource | Raw-ready != decoded-ready, slot/barrier generations, expected bytes/arrivals, overwrite and reclamation, async arithmetic consumers, all-inactive participants, prologue/drain, 48-KiB/resource rounding and register refusal |
| Numerics | Exact supported packed decode/scale cases, rounding/saturation/nonfinite/invalid scale semantics; unchanged source quantized attention tolerances; unquantized quality tracked separately |
| Isolation/fallback | No WN/ST/TY/DST changes or candidate tool calls in Phase 1; legacy v1/v2/v3/v8 behavior; selected-only counts; stale proof/profile/page/format/pipeline seal, rollback and compatible generic/serving fallback |
| Native evidence | Original/selected B/T with -st -src, raw diffs, selected High/Mid/Low/PTX/CUDA/launch joins and matching COMMENT explanations; offline hardware results explicitly separate |

Correctness checks belong to component/rule development and the selected
implementation. They are not a second hidden phase inside every estimated
candidate. Tests must demonstrate that unsupported quantized input is never
reinterpreted as FP32; when no semantic fallback is available, stable rejection
is the correct outcome.

### Contract versioning, evidence and closure

Preserve MR2's binding v3, hierarchy v1, descriptor v1, controls v3, 36-entry
census, selected schema v2, v8 fixed recipe and materializer registry as released
contracts. Q0/Q4 must choose and publish new exact versions for quantization,
workload/guard, paging, async event and launch fields before code accepts them.
No currently rejected field becomes an ignored hint. No capability string can
authorize an unimplemented materializer.

The evidence package must label source/format/profile/target and toolchain,
logical versus estimated versus measured traffic/cost, completed versus
analysis-only rules, compatible fallback and missing hardware. Current MR1/MR2
host/compiler results are baseline regressions, not evidence for Q0-Q7.
Keep original/selected artifacts and raw diffs, exact commands/oracles,
invalidation/refusal logs, profile/counter provenance and assigned reviewer/
successor roles. See the specification's transfer package contract.

No new compiler implementation, quantized/paged ingestion, phase certificate,
TMA kernel, GPU correctness or speedup is delivered by this documentation
revision. The next concrete implementation milestone is **Q0**.

## Milestones and concrete acceptance

| Gate | Owner/scope and work | Required evidence / go-no-go |
| --- | --- | --- |
| MR0 baseline characterization | Driver-owned fixed launch/allocation harness and offline M7 profiling. Exercise short/long, batch/head/latent/rotary shapes and masks within admitted source contracts. Keep toy H=3/C=16 evidence distinct from representative larger-head/rank workloads; capture a new census before widening unsupported source forms | Baseline native/host/GPU correctness first, then paired event timing and separately collected HBM/L2/SMEM requests/transactions, occupancy, registers/spills, stalls/special/compute use. Pin device/driver/tool/counter map, clocks, cache preparation, concurrency, precision and model inputs. No device means no measured result; no automatic memory-bound conclusion |
| MR1 owned reuse and traffic facts | LNO active-PU canonical snapshot; reuse existing access/dependence/alias/epoch and memory/resource/lifetime services. Derive exact head-invariant read sets/storage versions and owned traffic/liveness records | Positive shared-cache and negative head-dependent/aliased/updated/strided cases; original tree/ST/TY and analyses preserved by collection; no native pointer escapes. Distinguish useful/global/transaction/HBM/cache/SMEM/spill/partial traffic and one/two sweeps. Byte counts/units/provenance and overflow/refusal fixtures |
| MR2 bounded detached model/census | VHO/LNO-owned pure descriptors, proposed G={1,2,4,8} with bounded W/column/layout choices; reuse occupancy/readiness math. Version the hierarchy profile, candidate/report/selected-plan/binding contracts and capability registry; leave v2/v8 and old scores intact | Analytical and checked-count fixtures for tails, pass count, cache scenarios, score/tuple storage, spills, resource rounding, coverage, launches and merges. Deterministic portfolio/cache/budget/tie tests. Prove candidate exploration has no WN/ST/TY/DST or emission/compiler/GPU/test dependencies. Unsupported materializers are analysis-only and cannot win. Re-cost all eligible entries under one profile; reprove stopping bounds |
| MR3 selected synchronous grouped rule | Owning High selected transaction plus driver-owned fresh worker contexts. Implement the finite verified G domain with synchronous CTA-local cache staging, explicit per-head ownership/collectives and normalized tuple ABI. First preserve existing numerical/precision and source/fallback ABI | New-rule positive/negative fixtures independent of an active search: G=1 equivalence, G>1 with head/token/column tails, H<G, distinct and all-masked neighboring heads, nonfinite/extreme scores, poisoned padding, repeated slot use, guarded buffers and immutable inputs. Prove cache/layout/lifetime/48-KiB-launch constraints and capacity. Native B/T/diffs; High/Mid/Low/COMMENT/whirl2c/PTX/launch coherence; assembly/device link. Verify rollback and stale-input/group/profile/layout/ownership refusals, M6 legacy graph and ordinary host fallback. Enable G>1 production eligibility only after this gate and MR2 pass |
| MR4 selected hardware acceptance and calibration | M7 fixed baseline and one selected winner per declared workload/profile. Correctness/race/sanitizer checks precede performance selection; measure the declared whole call | Predeclare reference tolerances, warm/cold/cache scenarios, sample/noise policy, improvement/non-regression thresholds and workload weights. Record actual resources and per-level traffic plus paired timing. Calibrate on fixed probes, test held-out shapes/masks/contention. Rejection keeps accepted G=1/host baseline; any retry is explicit/capped, never a scan through all candidates. More bytes can win if latency is lower; fewer bytes alone cannot close this gate |
| MR5 movement/compute extensions | Q0-Q7 prioritize quantized/paged TMA/dequantization; separately admit cross-CTA/cluster sharing or Tensor Core layouts/authorized precision | Complete target/ordering/visibility/slot lifetime, launch ABI and numerical contracts; verified model/materializer/native backend before eligibility. Extend source census when necessary; no generic ISA/precision claim from arch lookup. Retain synchronous and scalar baselines. These are optional future families, not prerequisites for initial grouped synchronous acceptance |
| MR6 strategy comparison and transfer | M8 equal-budget study, followed by M9 independent review | Declare candidate/model/capability universe and cache treatment; compare portfolio strategies under common budgets across held-out workload/cache regimes. Publish scoped estimates versus measured results, selected-only counts, regressions/risks and immutable reproduction package. Roles remain TBD; no global GPU optimum or personnel acceptance without evidence |

## Driver and High-exit integration order

1. Capture original source/storage/access/alias/dependence/numerical/target and
   resource/profile identities at the existing qualified active-PU boundary.
   Prove head invariance and copy only owned facts; account for collection once.
2. Explore detached complete plans under existing global controls. Emit a ledger
   with G/W/tiles/layout/sweeps/ownership, per-level traffic assumptions and
   resource/coverage costs. Unsupported concepts are visibly analysis-only.
   Evaluate the compatible G=1 baseline under the new model first and charge it
   once to the shared budget; insufficient remaining budget keeps that baseline.
3. Select one eligible estimated winner. Revalidate original input, reuse proof,
   model/catalog/capability and controls before the atomic High transaction.
   Persist every new semantic field in a reviewed versioned seal/recipe; do not
   overload old chunk-policy numbers or rely on the emitter to infer grouping.
4. Rebuild invalidated analyses. High workers must expose real cache staging,
   group/head loops and producer/consumer completion, preserving the current
   normalized partial ABI and original host output. Verify the High exit before
   native Mid memory/control lowering and fixed Low mapping. Initial attention
   grid is B*ceil(H/G)*ceil(L/W), with correct direct/tail specialization.
5. The M6 consumer verifies the fixed handoff and completion. Mid/Low/PTX and
   whirl2c consume G/W/layout/pipeline unchanged. Maintain original-input and
   worker keys, no-replace publication, child isolation and ordinary fallback.
6. Validate only the selected implementation's required reference/resource/GPU
   cases and paired performance. Preserve an accepted executable baseline
   separately from the estimated incumbent. Record selected failures and any
   authorized bounded retry without changing the declared search conclusion.

## Required regression and comment contracts

Keep existing M4 rollback/native isolation, M5 selected variants, M6 production/
legacy dependence recovery and fallback evidence. Impact-select compiler-rule
regressions once for the changed rule, not for every estimated descriptor.
Retain source and selected B/T with `-st -src`, raw transformation diffs and
source/plan hashes. No new GPU claims can be derived from the old M6 host suite.

Comments in readable optimized CUDA, WHIRL COMMENT nodes, whirl2c output and
PTX must identify G/valid heads, shared c/r tiles, actual sweep count, placement,
producer/consumer barriers and overwrite safety, head-local statistics/tails,
normalized tuple merge and remaining traffic. State whether a byte number is
logical, modeled or measured. Keep the current PREOPT COMMENT workaround; its
general repair remains a separate effort. Metadata-only claims cannot substitute
for cache reuse actually present in native WHIRL.

## Risks, stop decisions and transfer

Retain original model/resource/source inputs and G=1 if baseline cache hits
already serve the duplicate requests, larger groups underfill SMs, register or
SMEM pressure causes spills/residency loss, or extra staging/barriers outweigh
reuse. Keep compute mapping visible because current scalar-F32 throughput may
limit the kernel. If held-out results do not justify the grouped family, report
that scoped result and continue calibration/layout/compute work; do not force
its deployment or hide regressions by changing tolerances or workload weights.

Schema/model/candidate changes invalidate affected caches and gap certificates.
Budget/coverage/plateau/target/economic stopping remains available as supported;
new bounds need explicit proof. Measurements occur outside active estimation.
AOT and versioned offline profiles remain the baseline; no JIT requirement or
runtime adaptive search is introduced. Guarded dispatch among precompiled,
qualified geometry/representation paths is permitted under Q2/Q3; engine phase
labels cannot bypass bounds, numerical or synchronization contracts.

Primary owner, successor, independent reviewer and approving lead remain TBD.
The minimum transferable package includes facts/schema/model/capability versions,
commands, fixed baseline, selected artifacts/comments, reference and sanitizer
results, timing/counter provenance, complete search/refusal/count ledgers,
held-out evaluation, limitations and immutable receipt. Design adoption alone
is not implementation, GPU acceptance or independent transfer completion.

---

# AI compiler optimization search implementation plan v1.8

November 8, 2026 | M6 selected production integration and legacy dependence-image compatibility

Adopt [M6 contract v1.0](AI-COMPILER-SEARCH-M6-PRODUCTION-ROUTE-v1.0.md).
The complete v1.7 plan follows verbatim as historical material; this section
controls M6 status and the previously open long-context reopen prerequisite.

| Gate | Current implementation and acceptance |
| --- | --- |
| M1-M5 | Retained bounded common-High admission, owned snapshot/epoch, estimation-only observer and controlled search, atomic selected application, verified High/Mid/Low and native scalar sm_90 PTX |
| M6 | Driver-integrated opt-in selected route for admitted absorbed MLA. Exact original input binding survives M4 rebuild. Capture the active High exit before MAINOPT; fresh child lowering consumes that exact source. Publish each independent PU only with a completion manifest, preserve ordinary host output on every refusal. Repair writer dead-vertex compaction and reader compatibility for immutable legacy zero-offset dependence images without changing the binary ABI |
| M7 | Open: actual GPU numerical/sanitizer/race checks, launch/allocation acceptance, measured resources, paired performance and offline calibration |
| M8 | Open: equal-budget strategy evaluation, optional providers and scoped search conclusions |
| M9 | Open: broader production/independent transfer. Primary owner, successor, reviewer and approving lead remain TBD |

Implementation ordering:

1. Read file-scope production controls through existing option descriptors.
   Search remains O3/LNO/PU-local and estimation-only under the existing budgets.
2. M4 commits one winner, rebuilds analyses, reinserts its exact COMMENT and
   seals both rebuilt High identity and the original complete typed input.
3. Immediately after LNO, be_driver verifies the accepted key, source scope and
   controls. It reserves a fresh output directory, captures only this active PU
   with the native writer in a copy-on-write child, then execs the explicit
   native bridge tool with an argv array. No shell, extra candidate, parent
   WN/ST/TY mutation, public backend phase API or library dependency is added.
4. The helper reuses the strict binding decoder and exact key spelling. Verify
   original source/facts/unit/function/target/recipe/storage/resource/model/
   controls/numerics, then reuse M5 High staging, Mid ARRAY/SCF lowering, Low GPU
   binding and PTX emission under fresh driver-owned worker contexts.
5. Produce launch/storage metadata and commented algorithm-review CUDA. Publish
   native files with no-replace links and the completion manifest last; failed
   work remains private staging. Ordinary host compilation continues unchanged.
   Independent PUs have independent products; no caller/callee rewrite or IPA
   scope is inferred. whirl2c remains a verified Low consumer.
6. At normal mapped-image finalization, omit graph vertices absent from the
   freshly written tree offset map and remap incident edges. In the reader,
   compact legacy zero-offset vertices with the same edge rule and bound graph
   payload reads. Preserve valid image layout and surviving constraints.
7. Certify native before/after files, full symbol/source traces and raw diffs;
   old-reader failure/current-reader recovery, exact native recovered graphs,
   old-reader acceptance of corrected outputs and scalar host reference checks;
   automatic short/long publication, manifest refusals, helper/publication
   fallback, level/selection boundaries, multiple-PU isolation and COMMENTs.
   Recheck M4 rollback and M5 native assembly/link/CUDA gates. Keep source-matched
   symbols/dependency audits and immutable replay evidence.

Controls: `-LNO:high_search=on:high_apply=on:high_search_bindings=<v2.json>:`
`high_device_output=<fresh-directory>:high_device_tool=<explicit-native-tool>`.
All new routing is off by default. Unsupported scopes or targets retain the
host route. Existing explicit M5 CLI handoffs remain compatible; production
requires the additional original-input seal. AOT remains the baseline.

The long-context dependence-image prerequisite is repaired for both regenerated
outputs and immutable legacy inputs. Existing valid images keep their vertex
numbering and constraints. The scalar host test uses the repository vector-math
runtime when LNO selects vexpf; unsupported SIMD source printing is a separate
limitation. No GPU correctness, global optimum or speedup claim follows from
compiler/host/assembler acceptance. General PREOPT COMMENT-loss repair remains
separate; the M4 workaround is preserved.

---

# AI compiler optimization search implementation plan v1.7

November 7, 2026 | M5 verified High exit and bounded native device bridge

Adopt [M5 contract v1.0](AI-COMPILER-SEARCH-M5-DEVICE-BRIDGE-v1.0.md) and
[methodology v1.3](AI-COMPILER-OPTIMIZATION-SEARCH-METHODOLOGY-v1.3.md).
M5 implements an explicit selected-only bridge for the compiler-produced MLA
route. The normal selected backend output or immediate M4 checkpoint is a
required verified input. Phase 1 estimation and global convergence are unchanged.

| Gate | Current status / next acceptance |
| --- | --- |
| M1-M4 | Existing bounded entry/snapshot/observer/search and atomic High application retained; accepted tree now carries a complete source/binding equality seal |
| M5 | Bounded implementation: verified selected source; fixed-policy v8 handoff; High owner formals, staging and synchronization; native ARRAY/SCF Mid lowering; Low GPU/entry binding; native scalar sm_90 PTX and linked math ABI; CUDA-only driver preserves sealed Low; native COMMENT/source explanations |
| M6 | Open beyond this route: general production routing/scopes, full integration acceptance and ordinary nonselected long dependence-image reopen issue |
| M7 | Open: real GPU numerical/sanitizer checks, actual resource/performance measurement and offline model calibration |
| M8-M9 | Open: strategy/optional-provider evaluation and production/independent transfer; roles remain TBD |

Implementation order is now concrete:

1. LNO admits/applies one winner, rebuilds analyses and reinserts its exact
   COMMENT after PREOPT. Bind the accepted native tree, closed-DAG witness and
   source/resource/model/control/numerical decisions. Default estimate-only and
   every original rollback/fallback gate remain active.
2. The bridge driver independently maps the actual accepted High source in a
   child process. Compare the full key and original semantic recipe; read the
   fixed width/policy; refuse unsupported architecture or changed lineage before
   any device construction. No search entry point is called.
3. Under fresh driver-owned PU contexts, reuse the existing selected staging and
   explanation generator, then abstract block/thread ownership into explicit U4
   formals before publishing verified High. Publish shared allocation and
   copy/compute/release DO_LOOPs, tuple layouts and typed synchronization.
4. Use existing WN_Lower LOWER_ARRAY|LOWER_SCF, with normal backend scope/map
   lifetimes, for Mid addresses and branch control. Retire prior analysis keys.
5. Bind only owner inputs/entry ABI for Low; verify native symbol tables, full
   level key, launch/argument joins and complete shared inventory. Publish Low
   only after every selected worker can emit native PTX.
6. Emit PTX from Low native nodes; ptxas assembles and nvlink links two math-only
   device helpers. Separately emit/compile CUDA through whirl2c for parity. The
   ordinary backend driver verifies sealed Low before bypassing VHO/ABI/address
   rewrites in this CUDA-only consumer; the printer rechecks it.
7. Certify before/after B/T/diffs, source preservation, deterministic mapped
   re-emission, native mutation refusals, host collective oracles/reverse order,
   native PTX assembly/link and separate CUDA compile. Publish immutable evidence
   before declaring the bounded gate complete. GPU acceptance stays M7.

The supported staging baseline is synchronous one-slot copy/compute/release.
No selected descriptor currently requests async/TMA or look-ahead prefetch;
those need explicit producer/legality/completion contracts before support.
The native PTX subset does not replace the historical general NVISA backend.

New source ownership is recorded in the M5 contract. No WHIRL binary format,
public backend phase ABI or be.so dependency/export change is required. The
legacy independent source lane and v1-v7 recipes remain available. v8 preserves
fixed physical policy without invoking automatic selection during handoff.

M4 High source ABI remains intact; device splitting has an explicit allocation/
launch ABI from verified Low metadata. No emitter recreates a missing schedule.
The general PREOPT COMMENT-loss repair remains separately deferred.

Acceptance counts, source/binary identities, commands, actual compiler resource
logs and remaining limits are recorded in the M5 receipt. No device timing or
GPU correctness is inferred from host or assembler evidence. Personnel roles
remain TBD, so independent transfer acceptance remains open.

The complete v1.6 plan follows verbatim. This amendment supersedes its M5-open
status only for the bounded explicit source/driver/backend route above.

---

# AI compiler optimization search implementation plan v1.6

November 7, 2026 | M4 bounded selected High application and rollback

Adopt [M4 contract v1.0](AI-COMPILER-SEARCH-M4-ATOMIC-HIGH-v1.0.md) and
[methodology v1.2](AI-COMPILER-OPTIMIZATION-SEARCH-METHODOLOGY-v1.2.md).
M4 implements the explicit compiler-produced common-High MLA route with a
separate high_apply opt-in. M3 remains estimation-only; its default behavior
and schemas/CLI are unchanged. One eligible best explored candidate is freshly
admitted and staged, with exact source/context/resource/model/control binding,
closed-DAG numerical/partition proof, rounded capacity rechecks and private
tuple-storage bounds. Unsupported materialization retains fallback.

| Gate | Current status / next acceptance |
| --- | --- |
| M1-M3 | Completed bounded canonical entry, owned snapshot/epoch, detached observer and globally budgeted production convergence |
| M4 | Implemented bounded selected High algorithm, original analysis preservation, independent rebuild via PREOPT_DUONLY/LNO services, atomic root/manager handoff, table/metadata/map-ID rollback, selected ownership barrier and fixed metadata |
| M5 | Next: fresh fixed-plan admission and verified High exit; add selected SMEM/staging/prefetch obligations, Mid structured memory operations, Low GPU mapping and PTX; no search in emitters |
| M6 | Open: full subsequent pipeline ownership, broader scope/routing and the ordinary long post-LNO dependence-image reopen defect |
| M7-M9 | Open: GPU correctness/performance/calibration, strategy/optional-provider evaluation and independent transfer; roles TBD |

Selected High implements chunk-local max/softmax/latent accumulation and the
stable tuple merge, with signed DO_LOOP/ARRAY, preserved comments and ordinary
whirl2c review source. The value projection folds its prescribed transpose
index permutation. Scalar review execution deliberately recomputes scores
across two passes; physical warp/SMEM realization remains M5. Capacity refusal
does not try a second candidate.

The source tree and original analyses remain live until a complete staged
rebuild and fresh snapshot verify. Recoverable failures restore source,
ST/TY/metadata/string state, map allocator IDs and live analyses; the old epoch
stays expired. Success transfers fresh manager ownership to lnodriver, exports
CG dependences and bypasses competing LNO, including final simplification,
before normal cleanup. Public backend entry ABI and WHIRL image layout remain
unchanged; private options add no be.so exports or library dependencies.

Acceptance includes actual selected before/after B/T/diffs, reference
execution of native whirl2c views and normal output, width-32/64 policy cases,
rollback at four boundaries, stale/non-best selection refusals, no-incumbent,
scope/level/off gates and unchanged M3/legacy regressions. Receipt records exact
results. Compiler/host evidence is not GPU timing or device correctness.
The original legacy long image issue remains separately qualified.

The complete v1.5 plan is preserved verbatim below. This amendment supersedes
its M4-open status only for the bounded producer/ABI/materializer described
above; later High-exit/device/transfer obligations remain open.

---

# AI compiler optimization search implementation plan v1.5

November 7, 2026 | M3 production controls and globally budgeted convergence

Adopt [M3 control contract v1.0](AI-COMPILER-SEARCH-M3-CONTROLS-v1.0.md) and
[methodology v1.1](AI-COMPILER-OPTIMIZATION-SEARCH-METHODOLOGY-v1.1.md).
M3 is implemented for the explicit common-High MLA route. Its production
-LNO:high_search=on and high_search_bindings opt-in validates a strict v2
source/resource/model/control document, then runs the existing three providers
in a fair, shared-cache portfolio. It preserves v1 environment/standalone
behavior and all existing fallback/level/scope gates.

| Gate | Current status / next acceptance |
| --- | --- |
| M1-M2c | Completed bounded canonical entry, owned snapshot/epochs and read-only binding/isolation baseline |
| M3 | Completed bounded service: independent per-run controls, global proposal/unique/cooperative-time limits, composite convergence, explicit conversion evidence, event/trajectory/predicate accounting, no-incumbent fallback; no candidate native transformation |
| M4 | Next: sealed selected-plan admission; recheck current source/epoch/resources and prove schedule/numerical legality; atomic selected-only application/rollback; notify mutation, rebuild analyses and protect managed nests |
| M5 | Open: verify High exit before Mid memory operations, Low GPU mapping and PTX; emit explanatory COMMENT/source views from the fixed plan |
| M6 | Open: full selected native integration, stale-plan/rollback/ownership acceptance and repair of baseline long post-LNO dependence-image reopen defect |
| M7-M9 | Open: GPU correctness/performance/calibration, equal-budget strategy evaluation/optional providers and independent transfer; roles TBD |

Global budgets apply across runs of one bound PU search invocation. Proposal
hits and rejected candidates count; unique estimates are cached once. A
deadline checks before each atomic proposal and can overrun by one estimate
plus bounded bookkeeping. Snapshot, binding and integrity checks are separately
reported. Default search stops at all 12 descriptors covered rather than
finishing redundant visits. Bounded search without an incumbent reports none
and continues ordinary LNO.

Target, relative-gap and explicitly converted cost-benefit predicates are
model-scoped. Plateau is empirical and per run. Hard caps and union exhaustion
cannot be disabled by naming different stops. All terminal predicates and
each run's trajectory/incumbent survive reporting; no calibrated confidence,
global GPU-optimum claim or candidate materialization is introduced.

Acceptance retains 37 pure-service checks, 12 native control scenarios and
19 refused/disabled configurations, both context traces/repeats, actual ledger
and cache/union reconciliation, native state guards, source-matched option ABI
audit, legacy observer and existing compiler/host regression suites. Exact
counts/results and immutable receipts accompany the milestone. No GPU timing
or GPU correctness is inferred. No new WHIRL binary layout or writer change;
the existing padding classification and long reopen limitation remain explicit.

Private option descriptors change be.so's option table, with no additional
source-matched exported/undefined dynamic symbols, library dependency or
LNO_FLAGS layout. Existing old backend comparison remains separate. M3 does
not select a production profile automatically or create a general VHO provider
route; GPU/model/source bindings remain explicit and fully checked.

The complete v1.4 plan is preserved verbatim below. This amendment supersedes
its M3-open status only for the bounded implemented route. Its M4-M9,
architecture, hardware and transfer obligations continue to apply.

---

# AI compiler optimization search implementation plan v1.4

November 7, 2026 | M2c read-only observer and complete binding validation

Adopt [M2c observer contract v1.0](AI-COMPILER-SEARCH-M2C-OBSERVER-v1.0.md).
M2c is implemented for the explicit native common-High MLA entry. It validates
complete source/analysis keys, the native producer recipe witness and formal
ABI, effective numerical permission, GPU capacities and cost model before
running the existing detached 12-entry portfolio. It applies no winner.

| Gate | Revised status and next acceptance |
| --- | --- |
| M2a/M2b | Completed bounded entry/admission and owned snapshot/epoch baseline; M2c adds fast_math to exact numerical binding |
| M2c | Completed bounded read-only MLA observer: complete versioned bindings, reused detached planner/estimator, audited native-free dependency closure, physical tree/ST/TY equality and analysis revalidation, explicit fallback/skip diagnostics and separately charged collection/validation/exploration/isolation |
| M3 | Next: production controls/profile route, global proposal/unique/time budgets and portfolio/convergence integration; existing prototype strategies/stops remain available without new policy in M2c |
| M4 | Open: fresh selected-plan admission, schedule/numerical legality, atomic application/rollback, mutation notification, rebuild and ownership of selected nests |
| M5-M9 | Open: verified High exit/Mid-Low/PTX and general device routing; complete compiler/GPU/search evaluation and personnel transfer |

The offline OPEN64_HIGH_SEARCH_BINDINGS adapter is opt-in through the existing
partition_axis/O3/LNO gates. Missing, stale, incomplete or malformed input leaves
ordinary LNO in place. Old valid High images without a native recipe witness
remain supported and skip search. No opcode, type kind, image layout,
reader/writer behavior or be.so dependency changes. This is a bounded MLA
observer, not general VHO routing or a globally optimal GPU scheduler.

Validation retains two contexts with 12 estimates each, exact standalone census
agreement, 28 refusal cases, model/resource updates and native-state isolation.
Existing 38 tensor/search tests, 21 snapshot checks/lifecycle, 21 host/reference
executions, eight form cases and 13 partition cases remain regression gates.
Strict compatibility still requires raw byte equality. Reassociation-enabled
image comparisons also classify an existing DEP_STRUCT trailing-padding defect
under a native-verified ABI, retaining every raw difference and comparing all
meaningful bytes exactly; no writer change or silent normalization is made.
The older long post-LNO graph reopen defect remains an M6 prerequisite.

The complete v1.3 plan is preserved verbatim below as historical baseline. This
amendment supersedes its M2c-open and unbound-context status only for the bounded
validated observer route. M3-M9, broader routing, legality and GPU/transfer
obligations continue to apply. Owners/reviewer/lead remain TBD.

---

# AI compiler optimization search implementation plan v1.3

November 7, 2026 | M2b owned facts, identities and mutation epochs implemented

Adopt the [M2b snapshot contract](AI-COMPILER-SEARCH-M2B-OWNED-SNAPSHOT-v1.0.md).
This dated amendment supersedes the M2b-open status in the preserved v1.2
baseline below. The existing architecture and two-phase methodology remain:
estimation explores owned facts; only the selected plan may transform WHIRL.

| Gate | Current status and next acceptance |
| --- | --- |
| M2a | Completed bounded native common-High entry and canonical admission; general VHO/provider routing stays open |
| M2b | Implemented at the qualified active-FUNC_ENTRY LNO boundary: deep-owned loop/access/full-graph/DU/alias facts, source namespace and semantic path identities, effective context records, distinct PU activations and mutation epochs; live recapture rejects covered source/analysis changes |
| M2c | Next: integrate the read-only search observer, verify complete source-manifest/resource/model bindings and zero candidate-induced WN/ST/TY changes, separate snapshot cost from exploration cost |
| M3 | Production portfolio/convergence controls remain open; standalone estimation baseline is retained |
| M4 | Selected-only schedule application remains open; check epoch/context and recapture before application, notify before mutation, atomically apply/rollback, rebuild analyses and preserve managed schedules through later LNO |
| M5-M9 | High exit/Mid-Low-device orchestration, complete compiler/GPU/search evaluation and transfer gates remain open; the existing long post-LNO image issue is still an M6 prerequisite |

The normal driver owns the active PU. Lnoptimizer creates its epoch, captures
after M2a admission with the full dependence graph/access/DU/alias facts live,
then expires it before Hoist_Varying_Lower_Bounds. The remainder of legacy LNO
cannot consume it as current. Owned records remain readable after map/graph/pool
cleanup. Any future consumer must notify all mutations, rebuild native analyses
and capture again; current revalidation is a defense in depth, not a substitute
for that contract. No stale snapshot authorizes transformations.

Stable loop/access identities are scoped to the full canonical source key and
unit key, with semantic paths independent of native addresses and map numbers.
A compact FNV review namespace is accompanied by complete equality keys.
Source positions/comments do not contribute executable ordinals. This does not
promise identity through arbitrary canonicalization or source changes.

Numerical IEEE/roundoff and native target ABI/ISA are copied; the external tensor
manifest, GPU resources and cost model remain explicitly unbound. The snapshot
grants no reassociation, parallel independence or selected schedule legality.
M2c/M3 must close these external-binding obligations before production search.

Focused validation uses a separate test LNO build for deliberate live-fact
perturbations, repeated mapped reopens, stale/closed/cross-PU rejection and
post-cleanup owned reads. The production path keeps the existing opt-in and
fallback. The common-High and native-partition compatibility suites retain
normal executable-image parity and host/source-reference checks. No GPU or
performance conclusion follows from this milestone.

The complete v1.2 plan is preserved verbatim below as the historical M1/M2a
baseline. Its M2b future-tense statements are superseded by this amendment; its
remaining architecture, milestones, tests and risks continue to apply.

---

# AI compiler optimization search implementation plan v1.2

November 7, 2026 | M2a native High entry and admission complete; owned snapshot next

Authority: [search methodology v1.0](AI-COMPILER-OPTIMIZATION-SEARCH-METHODOLOGY-v1.0.md)
and [optimization design v0.7](AI_compiler_optimization_design_v0.7.md). The [M1 audit](AI-COMPILER-SEARCH-M1-DRIVER-LNO-AUDIT-v1.0.md) records the inspected source baseline; [consolidated plan v0.9](AI-COMPILER-OPTIMIZATION-IMPLEMENTATION-PLAN-v0.9.md) adopts these findings. This plan
decomposes the open S7 production/calibration gate in the
[standalone MLA plan](AI-COMPILER-MLA-TWO-PHASE-IMPLEMENTATION-PLAN-v1.0.md).
S0-S6 remain the completed prototype baseline; the gates below do not rename
them or imply H0-H8/PF0-PF7 completion. Work remains AOT, opt-in, O3 and PU-local.
No new compiler-option spelling, opcode, type kind or binary layout is reserved
by this document.

## 1. Baseline and immediate sequence

Commit 1abdd5957623c54a3dea54b6af95e0d493de09a9 implements detached MLA
estimation, exhaustive/random/local portfolio search, six stopping modes and
the fixed v7 selected-recipe handoff in the standalone tool. It already lowers
and tests only the selected configuration per fixture. Retain the v0.27 receipt
and its 38 passing tests, 34 compatibility cases, 14+14 reference checks and
four CUDA compilations as historical evidence, with synthetic model and no GPU
execution. This plan adds concrete production integration and evaluation gates.

M1 is complete as a static source audit. M2a now supplies the explicit native
High-artifact MLA entry and read-only canonical admission described in the
[M2a contract](AI-COMPILER-SEARCH-M2A-HIGH-ADMISSION-v1.0.md). Next implement M2b
owned snapshot/epoch and M2c snapshot observer isolation at the identified LNO boundary.
Then implement M3 search controls before M4 selected schedule application and
M5 verified lowering boundaries. Run M6 focused compiler
acceptance before closing host integration. M7 native GPU acceptance and M8
search-strategy evaluation need hardware/data; they do not block completion of
an honestly scoped estimation-only search. M9 closes production/transfer only
with its required evidence. No additional MLA kernel micro-optimization is a
prerequisite for this methodology.

## 2. Milestones, outputs and acceptance

| Gate | Status and dependency | Concrete output and acceptance |
| --- | --- | --- |
| M0 specification | Complete in this documentation revision | Versioned methodology, main design and new plan; scoped result labels, selected-only rule, recovery policy and status/evidence distinctions |
| M1 driver and analysis audit | Complete: static source audit | Snapshot gap inside Lnoptimizer after first successful full dependence graph and enabled-path checks, before Hoist_Varying_Lower_Bounds. Audited graph/access/DU/alias cleanup, effective controls, FUNC_ENTRY scope, MAINOPT/CG masks and device-PU handoffs; no hook implemented. See M1 audit |
| M2 immutable search snapshot | M2a bounded native entry complete; M2b/M2c open | M2a: implemented unoptimized source recipe -> signed-loop/affine-ARRAY High B -> ordinary PREOPT/LNO, with canonical form/coverage refusal and unchanged fallback. Automatic general VHO/provider route selection is future driver integration. M2b: deep-copy covered access/dependence/DU/alias facts with driver/LNO epoch and stable IDs. M2c: read-only observer at lnopt_main.cxx:1360 with zero candidate-induced IR/table mutation and explicit skip reasons. Initial scope is active FUNC_ENTRY with admitted loop subsets |
| M3 portfolio and convergence service | Prototype subset complete; production work open after M2 | Reuse existing providers; add explicitly versioned global/proposal/unique/time caps, fair scheduling and composite stopping when needed. Preserve the existing CLI/schema. Reports retain all predicates, per-run trajectories and scoped union completeness; complete 12-entry estimation remains default |
| M4 selected High application | Open; M2/M3 | Typed fixed-plan admission; preflight current generation/legality/resources; atomic application through owning High services; invalidation/rebuild manifest, selected-only counters and exclusion of later competing LNO transformations on managed nests. Failed application publishes no partial output; correctness defects do not trigger silent candidate trials |
| M5 High exit and Mid/Low bridge | Open; M1/M4 | Verify structured selected algorithm, loops/ARRAY, storage, SMEM/prefetch obligations and numerical semantics; Mid lowers memory operations, Low realizes fixed GPU mapping, CG emits PTX. Readable/COMMENT/mapped-source explanations agree; emitter cannot reopen scheduling |
| M6 compiler integration acceptance | Open; M2-M5 | Focused positive/negative, level/scope, rollback, stale-plan and compatibility evidence; selected short/long before/after B/T/diffs and reference checks. Development-rule tests are independent of candidate iteration. No GPU performance claim from host results |
| M7 offline calibration and native acceptance | Open; M6 and GPU | Explicit calibration corpus and held-out shapes, versioned timing/model provenance, selected-only GPU correctness/sanitizer/resource checks and paired baseline timing. Declare noise, improvement and regression limits before measuring; preserve baseline on failure |
| M8 strategy evaluation and optional providers | Open; M3; GPU optional for model-only study | Equal-budget, cache-neutral, multiseed comparisons of candidate quality and search overhead on held-out workloads. Separate estimated from measured regret. Admit hybrid affine, beam/evolutionary/Bayesian providers only with contracts and evidence; no probabilistic stopping without calibration |
| M9 production and transfer acceptance | Open; M6/M7 plus chosen M8 scope | Opt-in production path, explicit baseline/fallback behavior, immutable reproducible packet, scoped performance results and closed required H/PF dependencies. Owners/reviewer/lead remain TBD; personnel handoff acceptance remains open until assigned |

## 3. Actual source work map and phase ordering

The backend driver source is `osprey/be/be/driver.cxx`; be_driver is the component
name, not a separate be_driver.cxx in this checkout. The current source has
Preprocess_PU, Backend_Processing, LNO_Processing and
Do_WOPT_and_CG_with_Regions. Preprocess_PU runs optional DSL PREOPT/shape work,
DSL lowering and VHO_Lower_Driver; Backend_Processing enters LNO_Processing
before Post_LNO_Processing and WOPT/CG. LNO_Processing dispatches to
Perform_Loop_Nest_Optimization or the PREOPT-only path. These are inspected
anchors. M2a now installs the read-only form/coverage admission described below;
the owned snapshot and search service remain unimplemented.

M1 resolves the snapshot location to `lnopt_main.cxx:1360`: after the first
successful `Build_Array_Dependence_Graph` and disabled/autopar-only exits, before
`Hoist_Varying_Lower_Bounds`. Both full LNO dependences and access/parent maps
are live with PREOPT DU/alias. The earlier access-only partition hook is not a
complete scheduling boundary. At driver return, LNO graphs/maps/pools are gone
and the wrapper has deleted DU/alias; surviving `Current_Dep_Graph` is the
reduced CG representation. MAINOPT already lowers ARRAY before final CG.

This is a qualified canonical-High boundary: M2 must admit actual loop/memory
forms and graph coverage. Early short-loop unrolling and other preparation can
precede it; unsigned-loop canonicalization currently follows it. Ordinary DSL
runtime lowering can remove the tensor algorithm before LNO, so common-High
producer/lineage support is an explicit entry prerequisite. Snapshot generation
cannot rely on VHO shape generations or borrowed partition pointers alone.
Initial supported scope is FUNC_ENTRY, not arbitrary REGION-root or IPA scope.

The required production sequence is:

1. Driver establishes the active PU/REGION, target/resource/profile snapshots
   and optimization/numerical controls. Very High gatekeeping discharges
   domain legality before promotion. Preserve unrelated DSL/FHE paths.
2. Promote to common High algorithms and canonical DO_LOOP/OPR_ARRAY indexing.
   Use PREOPT's established canonicalization service. In M1 inspect VHO and
   LNO lowering action masks so the required form survives to its consumers.
3. Under LNO ownership obtain current bounds, access, alias/dependence and
   reuse facts. Snapshot their generation and immutable search inputs. A
   direct hook after LNO_Processing is insufficient unless M1 proves that
   required High forms and analysis lifetimes still exist there.
4. Run detached strategy proposals, lowerability preflight and estimates.
   Stop under the declared policy, select the best eligible estimate across
   all runs and seal the selected plan. Emit no candidate native artifacts.
5. Recheck the current snapshot, apply only the selected High schedule,
   multi-level tiling, staging and prefetch DO_LOOPs, rebuild invalidated
   analyses, then verify the High exit contract. Driver registers any selected
   generated device PUs through its explicit ownership service; a PU-local
   optimizer does not traverse or switch into other PUs itself.
6. Lower selected structured memory operations in Mid. Realize fixed GPU
   mapping and target operations in Low. Respect existing REGION/WOPT/CG
   contracts; the LOWER_TO_CG route must not see unresolved tensor/domain
   semantics or missing synchronization. Native PTX is the production backend
   objective; normal whirl2c CUDA remains a review/validation consumer.
7. Audit actual resources, selected identity, source/COMMENT fidelity and
   correctness. Accept or retain the existing executable baseline according
   to the declared deployment gate. Preserve instrumentation and FEEDBACK
   verification at their established boundaries.

M1 documents the conflicts in its audit. M2/M4 must resolve producer admission,
early transforms, analysis ownership and the selected-path barrier through the
reviewed High boundaries. Keep required cleanup/CG-graph export and unrelated
loop processing; do not move algorithm selection into Mid/Low, retain stale
LNO pointers or duplicate the dependence engine.

| Source/service | Work responsibility |
| --- | --- |
| `osprey/be/be/driver.cxx` | Scope, controls, snapshot lifetime, selected output publication, device-PU registration and bounded recovery |
| `osprey/be/vho/tensor_attention_search.h/.cxx` | Reuse existing typed detached MLA costing/providers; generalization requires a reviewed owning-phase API, not policy in common/com |
| `osprey/be/vho/tensor_kernel_lower.h/.cxx` | Existing detached plan admission and selected materialization; adapt to typed production facts while preserving historical policies |
| `osprey/ir_tools/tensor_kernel_driver.cxx` and `tensor_attention_search_json.inc` | Standalone archival JSON/admission/replay; preserve CLI and v1-v7 compatibility; do not copy JSON policy into be.so |
| `osprey/be/lno` and PREOPT/WOPT services | Canonical loop/access/dependence facts, schedule application and analysis rebuild in the correct scope |
| `osprey/be/com/wn_lower.cxx` and target CG/NVISA | Audit action masks, ordered memory and mapping realization; implement only fixed selected semantics |
| `osprey/be/vho/tensor_kernel_explain.cxx` and whirl2c | Retain generator-owned explanations and selected metadata fidelity; no new optimizer in the emitter |
| Existing FEEDBACK/physical profile services | Govern later empirical import/calibration by PF gates; no new generic telemetry framework in this increment |

## 4. Interfaces and versioned data

M2/M3 define typed contracts for snapshot creation, proposal, admission, estimate,
stop evaluation, winner selection, sealed-plan application and result reporting.
Use WHIRL opcode/version contracts, TY/TensorDescriptorIR tensor traits and
phase-owned verifier predicates. Derived dependence, scheduling and resource
facts remain in owned planning records with explicit validity epochs. Backend
service APIs follow the repository's naming and function-contract conventions; an outline here is
not an approved symbol list or library dependency.

The production snapshot includes stable source/loop/access identities, source
hash/IR generation, active scope, dependence/alias generations, numerical
permissions, workload objective and normalization, candidate-space ID, target
resource and model/profile identities. Candidate costs include all stages and
units; rejections carry reasons; unvisited records carry no fabricated cost.
Cache keys include every input that can affect legality or cost. Model-only
changes invalidate rankings/bounds; IR/resource changes invalidate admission too.

A future options/report revision distinguishes per-run proposals, total
proposals, unique estimates and elapsed time, names the composition of stopping
predicates and records every trigger. Keep the current single-mode
open64.mla-search-options.v1 behavior unchanged. The current
representative_strategy remains descriptive; do not rename it as a universal
strategy winner. Typed production plans and future reports need equivalent
binding to the sealed standalone evidence without changing binary WHIRL layout.

M4's application result records input/output generation, changed loops/accesses/
buffers, analyses preserved with reasons, analyses invalidated/rebuilt, selected
identity, actual construction counts and failure/rollback status. Empirical
latency/resource observations remain attached to measured code; they cannot be
copied as observations of a new candidate. Frequency FEEDBACK propagation and
physical-performance evidence have separate preservation rules.

## 5. Test and experiment matrix

| Contract | Required focused evidence |
| --- | --- |
| Estimation isolation | Same executable IR/table generations before/after Phase 1; zero candidate WN/ST/TY creation; no B/CUDA/PTX/object output or backend/test invocation. Existing snapshot initialization counted separately |
| Model arithmetic | Independent component/count checks; unit mismatch, unknown timing, overflow, no eligible candidate, resources on every stage, tails/merge/copy/projection costs and proof of estimator-bound applicability |
| Portfolio fairness | Same legal space/model; stable seed/ties; exact cache/proposal/unique counts; per-run and union minima; each enabled strategy receives its declared opportunity unless an explicit global limit terminates the portfolio |
| Stopping | Exhaustion, insufficient budget, no early eligible incumbent, plateau/minimum-gain semantics, target, estimator gap and compatible-unit cost-benefit; proposed composite/global/time cases under their new schema; unavailable evidence modes rejected |
| Invalidation | Stale IR/dependence/numerical/target/model/space snapshot rejection; correct analysis rebuild after selected transformations; model-only update reranks without IR mutation |
| Selected application | K=1 ordinarily; one winner with multiple stages distinguished from candidate count; injected failure restores original state or fails before publication; bounded resource retry logged; correctness defect retains baseline and fails acceptance |
| High exit/lowering | Domain/common forms, DO_LOOP/ARRAY bounds, affine legality, SMEM/prefetch ordering, storage lifetime and numerical contracts; Mid/Low cannot change width/algorithm; unsupported semantics diagnose |
| Controls and compatibility | O0 semantic-only and opt-out unchanged; pass admitted only at its declared O3/scope boundary; active-PU-only mutation; standalone old schemas/policies and ir_b2a/whirl2c compatibility |
| Selected correctness | Short/long, mask/tail/all-masked scenarios, stable rescaling/merge, FP64 oracle, canaries/immutable inputs and forward/reverse workers; actual selected CUDA compilation and exact COMMENT payload parity |
| Native acceptance | Selected GPU reference and synchronization/sanitizer checks; actual registers/shared/spills; matched baseline/selected whole-call timing, raw samples/conditions and declared acceptance thresholds |
| Strategy study | At equal budgets report final estimated quality, anytime trajectories, proposal/unique/time overhead, restarts and success across seeds/workloads. Use exhaustive model optimum where tractable; measured regret requires actual comparable measurements |

Reuse impact-selected rule tests once per changed transformation/compiler
contract. Do not wrap that suite around every estimate. Documentation-only work
uses preservation, link/version/status consistency and diff checks; it does not
rerun expensive compiler or GPU tests. A new native IR rewrite retains genuine
before/after .B, same-stem .T from separate ir_b2a -st -src processes, and the raw
diff with the original source available for cross-reference.

## 6. Recovery, acceptance and closure

The implementation must distinguish search_complete, selected_correctness_passed,
GPU_performance_pending and deployment_accepted conceptually; these are proposed
status concepts, not changes to existing receipt fields. A completed exhaustive
estimation run closes its search regardless of later hardware availability.
Correctness-only evidence cannot close GPU performance acceptance.

Declare deployment metric, baseline identity, improvement/regression limits,
noise policy and device conditions before M7 timing. Accept a selected variant
only under those criteria. A slower result retains the baseline, records model
error and closes the current attempt. A subsequent model revision is a new
bounded estimation experiment. The future resource-recovery default is two
total selected attempts; correctness failure stops acceptance and is never
treated as ordinary candidate pruning. Current standalone retry remains manual.

| Risk | Control and closure evidence |
| --- | --- |
| Synthetic model misranks | Explicit provenance, sensitivity/held-out calibration, paired selected/baseline timing; preserve accepted baseline |
| Finite space misses a better algorithm | Publish space/version and rejected/unvisited census; add admitted families as new bounded experiments |
| Cache order misleads strategy comparison | Equal-budget cache-neutral or virtual-cost study, multiple seeds and held-out workloads |
| Driver hook loses required High IR | M1 action-mask/lifetime audit, canonical-form check before search and verified exit before Mid |
| Late resource mismatch | Actual register/shared/spill audit, invalidation and bounded selected retry with immutable reports |
| Proof/uncertainty overclaim | Distinct estimated/measured/bound conclusions; reject unsupported probabilistic or hardware certificates |
| Search overhead erases benefit | Separate snapshot/proposal/estimate costs, explicit budgets and dimensionally valid amortization |
| Stale analysis or hidden cross-PU mutation | Generation-bound admission, owning-service rebuild, driver-owned PU/device registration and rollback tests |

Every gate retains a completion record with exact commit/tools/options, input
and model digests, result scope, commands, evidence links and open limitations.
Publish a new content-addressed review receipt for new executable milestones;
never replace v0.27 or earlier evidence. A documentation-only revision records
text validation and commit identity without manufacturing new GPU/IR evidence.

The minimum transferable package includes the specification, current status and
source work map, reproducible baseline, exact tests/commands, selected readable
and native examples, failure/recovery cases, risks and remaining gates. Primary
owner, successor, independent reviewer and approving lead are TBD. Keep
personnel transfer acceptance open while making all technical artifacts usable.

## 7. Existing standalone runbook and worked example

Use the retained v0.27 packet on Linux x86_64, from its root directory, with
fresh output paths. These existing commands exercise the current prototype;
they do not stand in for the proposed production be_driver path:

```sh
tools/tensor_kernel_lower explore-attention selected/long/source.recipe.json exploration.json \
  --gpu-profile gpu_profile.json --cost-profile cost-profile.json \
  --search-options search-options.json --allow-reassociate
tools/tensor_kernel_lower select-attention exploration.json selected.json
tools/tensor_kernel_lower schedule-source selected.json optimized.cu
tools/tensor_kernel_lower device selected.json device.before.B device.after.B
tools/ir_b2a -st -src device.before.B device.before.T
tools/ir_b2a -st -src device.after.B device.after.T
tools/tensor_kernel_lower audit device.after.B selected.json launch.json
tools/whirl2c -cuda -TARG:abi=n64 -O0 \
  -CLIST:dotc_file=mapped.cu -CLIST:doth_file=mapped.h device.after.B
```

The packet's README/replay.py retain environment and reference-test instructions.
For source cross-reference, make the retained compiler source available at the
original /src pathname recorded in DST, as the replay procedure does. Preserve
the before/after images and raw trace diff; do not substitute a textual mock IR.

Under the saved synthetic profile, the L=127 fixture selects direct attention
with 128 threads; L=8193 selects width 256 with 128 threads. Each fixture
estimates the 12-entry space, and only its single portfolio winner reaches
materialization/reference checks. These are scoped example selections, not a
universal context-length threshold. A different admissible timing/resource
profile can produce a different winner without changing any kernel algorithm.

The retained primary artifact identifies the baseline receipt as
`ai-optimization/research/mla-two-phase-search/sha256-d04d548cf12fd000b607b4aa50902a403a74bb911c6d6c54f6323779d2166f4b/`
under the primary checkout's `artifacts/review/` root. Verify its SHA256SUMS
before reuse. The existing `Open64_MLA_Two_Phase_Search_CUDA_v0.27.zip` is the
portable delivery; this documentation revision does not replace it.


## 8. M1 findings and revised M2/M4 acceptance

The controlling evidence is [M1 audit v1.0](AI-COMPILER-SEARCH-M1-DRIVER-LNO-AUDIT-v1.0.md),
source baseline `0de7c6ad7bce7142c6e66bcac3560e176cc5ab27`. M1 closure is a
source-inspection result; it does not certify a runtime hook or High pipeline.

| Next gate | Work order and required evidence |
| --- | --- |
| M2a producer/form admission | Identify admitted common-High loop producer and semantic lineage before the existing DSL runtime lowering. Initially admit good affine DO_LOOP/ARRAY nests with proved memory coverage; distinguish already unrolled/missing loops, bad-memory/Too_Messy/call/barrier cases, unsupported unsigned forms, MP/Lego/autopar and non-FUNC_ENTRY roots. Retain ordinary behavior on refusal. If search must preserve early loops, separately implement/test their preparation deferral; do not move graph building speculatively |
| M2b snapshot and lifetime | Consume full LNO DEPV_ARRAY_ARRAY_GRAPH and current access/parent/DU/alias facts at the qualified 1360 boundary. Copy owned loop/access/dependence/numerical/resource records; no WN/map/graph/manager pointers escape. Define a driver/LNO mutation epoch plus stable source joins; VHO shape generation alone is insufficient. Rebind/validate before selected application |
| M2c observer-only integration | Install an opt-in read-only collector before any search/application. Gate on effective post-backoff O3, Run_lno, LNO_enabled/LNO_Opt, numerical permission, supported scope and successful graph coverage. Record explicit skips. Prove input tree/tables unchanged by snapshot/candidate work; charge earlier analysis preparation separately. Keep trace-off and option-off behavior unchanged |
| M4 selected continuation | While facts are live, apply the selected plan under active-PU ownership or return a fully owned handoff with revalidation. Exempt managed nests from subsequent reversal, scalarization, fusion/fission/SNL, tiling, memlib and prefetch decisions unless explicitly admitted as selected realization. Preserve mandatory cleanup/export/feedback and unrelated nests. Rebuild affected facts and verify High exit before MAINOPT ARRAY lowering |
| M5 device publication | Generalize the existing numeric selected-plan registry only through a reviewed MLA handoff. Driver constructs device PUs after selection; normalization's program checkpoint remains a separate route, not proof of LNO integration. Preserve phase/scope lifetimes and atomic selected artifact publication |

M2/M4 focused tests must include: complete affine coverage; signed canonical and
unsupported unsigned/overflow bounds; small loops affected by early unrolling;
no loops/missing DU; full graph failure and graph-success with Has_Bad_Mem or
unrepresented accesses; skip/disabled/PREOPT-only and post-Olimit backoff;
trace-off parity; active-PU isolation with multiple independent PUs; stale epoch
and analysis rebuild after selected mutation; cleanup after early refusal; and
selected schedule preservation across the remaining LNO path. REGION-root,
MP/Lego/autopar and IPA support stay refused until separately admitted.

The owned snapshot observer and selected transformations remain future work.
The M1 audit ran source/excerpt/hash, ordering, link and documentation checks only. It changed
no compiler source and produced no new B/T/GPU execution evidence. Original
v1.0 remains available, and v0.27 retains its original host/compiler-only scope.


## 9. M2a completion and remaining production work

[M2a implementation contract](AI-COMPILER-SEARCH-M2A-HIGH-ADMISSION-v1.0.md)
controls the new `tensor_kernel_lower high` native-artifact entry. The driver
constructs a fresh host PU with an explicit disjoint-storage ABI; it does not
rewrite the original source/caller interface. The saved absorbed MLA algorithm
reaches ordinary VHO/PREOPT/LNO as signed DO_LOOP and affine ARRAY operations,
without tensor runtime calls or GPU mapping. Default DSL/runtime and existing
selected-device paths retain their behavior. Broader automatic VHO/provider
routing is not claimed by this entry path.

`LNO_DSL_admit_common_high` runs immediately before Hoist_Varying_Lower_Bounds
under effective O3/LNO and existing partition_axis opt-in controls. It observes
canonical signed static unit loops, explicit nonvolatile affine memory and full
graph coverage; it refuses unsupported scope/control/memory forms without
mutating executable WHIRL. MP/Lego/autopar paths do not receive this new
admission. The report explicitly grants neither an owned snapshot nor schedule
legality. M1's numerical line references describe its audited revision; the
function anchors remain the integration identity after this insertion.

Focused evidence: both source graphs contain 48 operations/55 values/41 unique
storage buffers and reach an admitted LNO boundary (59 surviving loops and 87
memory operations in each fixture). Eight ordinary-C form cases admit the two
affine/ordered-fold cases and refuse six unsupported cases. On/off/default,
trace-off, O0-O2 and LNO opt=0 checks preserve fallback behavior; default native
output also matches the retained pre-change LNO component byte-for-byte.

Fourteen High-image host executions cover all saved scenarios against FP64
oracles with canaries and immutable inputs. Seven additional short-context
post-LNO checks pass. Host pre/post bitwise equality is recorded as an
observation, while required numerical acceptance uses the saved source oracle
tolerances. Ordinary whirl2c requires the retained legacy host-math spelling
adapter; its post-LNO C lane disables unsupported x86 SIMD source spellings.
Native default-path byte comparisons retain the normal controls.

Existing impact-selected tensor-lowering/search regressions pass (38 tests),
as does the native partition regression (13 cases, five proved/eight refused).
No GPU execution or performance result is claimed. M2b and M2c now follow this
bounded form-admission entry; M4 still needs selected-schedule ownership and
analysis rebuild, and M5 still owns general device-PU orchestration.

A pre-existing long-context post-LNO CG dependence-graph reopen failure remains
an explicit M6 prerequisite: the retained baseline generates the identical
portable .N image and normal whirl2c fails in Depgraph_Read/OPCODE_make_op for
both. This is not counted as a successful post-LNO execution. Retain the
identical baseline/new images, hashes and both diagnostics when reproducing;
repair the defect before closing general mapped post-LNO acceptance. M2a does
not change binary WHIRL or dependence-image layout to work around it.
