# AI compiler optimization search implementation plan v1.12

November 9, 2026 | Quantized-cache, workload/phase, paging and TMA design extension

Adopt [design v0.8](AI_compiler_optimization_design_v0.8.md),
[reuse specification v1.1](AI-COMPILER-MLA-DATA-MOVEMENT-AND-REUSE-v1.1.md)
and [methodology v1.5](AI-COMPILER-OPTIMIZATION-SEARCH-METHODOLOGY-v1.5.md).
The full v1.8 plan still follows verbatim as historical material. This revision
preserves the MR1/MR2 baseline and adds planned Q0-Q7 work; it adds no quantized,
paged, phase-detection or TMA implementation or new performance evidence.

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
All Q0-Q7 work below is **planned**, not implemented or accepted by the MR1/MR2
results. No old milestone is renumbered. The new work refines the MR1 fact,
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
| Q0 census and contract review | Frontend captures a real small quantized MLA and paged/cache-update graph, including scale/zero-point paths, and ordinary/multi-query source forms. Reuse existing descriptor, effect, prefill/decode and memory/readiness contracts; review normative WHIRL rules. Publish exact schema and capability compatibility table, numerical oracle and supported first formats | Retain source/operator census and original mapped B/T. Distinguish actual packed storage from fake quantization; identify unsupported nodes. No enum/layout/trait guesses from model names. Source-to-native APIs, printer/verifier/mapped-image and ordinary fallback obligations agreed before implementation |
| Q1 quantized-cache facts | Native ingestion preserves encoding, quantization operands and lineage; VHO/LNO derives role/storage/scale-version/read-domain facts using owned snapshot services. Keep semantic type properties separate from dynamic scale/page/request state | Positive undeclared/declared roles, contradictory declarations, opaque/erased producers, fake quantization, aliases, group requantization and stale metadata tests. Exact unpack/dequantize oracle cases for each admitted format; no WN/ST/TY/DST mutation during collection; legacy FP32 admission unchanged |
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
   conversion target semantics in Low; verify and emit native PTX/whirl2c CUDA.
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
Use existing WHIRL opcode/type/tensor traits and verifier predicates. No MLIR
operation-interface/traits adoption is needed. New backend interfaces follow
the repository's naming and function-contract conventions; an outline here is
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
