# H20 memory-hierarchy implementation plan v1.0

Date: 2026-11-12. Status: design phase started; implementation milestones below
are planned. The user confirmed H20 96 GB servers. No endpoint, live
inventory, calibration or device run has been obtained by this work. Owner, successor, independent reviewer and approving
lead: TBD. Role assignment blocks transfer acceptance, not this design work.

Controlling design:
[hardware/resource/memory planning v0.2](AI-COMPILER-HARDWARE-RESOURCE-AND-MEMORY-PLANNING-DESIGN-v0.2.md).
This follows [search plan v1.31](AI-COMPILER-OPTIMIZATION-SEARCH-IMPLEMENTATION-PLAN-v1.31.md)
without copying or overwriting its historical milestones. Scope is GEMM, MLA,
LayerNorm and Reduction on the confirmed H20 96 GB servers, with actual
device/partition resources still to be discovered. The inspected
baseline is review-stack commit 08d91bf8; each implementation must rebase onto
the accepted owners/contracts before coding. Existing PR review continues
independently. A tip build does not accept every predecessor milestone.

## 1. Delivery decisions and dependency graph

Build one shared resource/traffic/event foundation with operation-specific
legality, schedules and numerical oracles. Keep source-order-preserving and
reassociation-permitted variants separate. Keep AOT compilation, detached
estimation, selected-only transformation and guarded fallback.

~~~text
H0 inventory + baseline correctness + initial characterization
  -> H1 versioned device/resource/calibration import
  -> H2 owned movement/reuse/event contracts
  -> H3 complete estimation + isolated calibration integration
  -> H4R Reduction and H4N LayerNorm
  -> H5 GEMM steady state and H6 MLA reuse/staging
  -> H7 workload qualification and release evidence
  -> H8 optional cluster reuse after its own evidence gate
~~~

H4R/H4N, H5 and H6 can progress as independent focused contributions after
their relevant H1-H3 contracts are available. Device correctness starts in H0
and accompanies every selected implementation; H7 consolidates acceptance.
If H0 finds a broken baseline, close the minimal correctness/compatibility
slice before using it for performance comparison.

No milestone is complete merely because an estimator, native image, source
emitter or host simulation passes. Each table below lists a distinct gate.
The existing numerical, publication, scope and fallback contracts stay active.

## 2. H0 — Discover the servers and qualify the starting point

**Owner:** external test worker and NVIDIA harness; no compiler transformation.
**Inputs:** user-supplied connection/worker endpoint when available, pinned
compiler archive/commit, actual operator baselines and numerical contracts.

Deliverables:

1. Versioned inventory per physical GPU or MIG instance: model and variant,
   UUID/PCI identity, host OS/CPU architecture and runtime ABI, visible
   memory/SMs, resource/carveout and launch limits,
   driver/toolkit/ptxas/Nsight/sanitizer versions, clocks/power/ECC, topology,
   process/partition visibility and unsupported-query reasons. Peer transfer
   probes additionally require a supported job contract and leases for every
   participating GPU; otherwise retain peer service as unknown.
2. Compile/load/execute probes for the exact selected mechanisms and numeric
   formats. Generic sm_90 compatibility and sm_90a WGMMA qualification remain
   distinct. Probe byte-bulk async separately from tensor-map transfer.
3. Frozen source, compiler, PTX/CUDA/cubin, baseline library version and input
   identities for each operator. Record which compiler routes are bounded,
   standalone, under review or missing.
4. Initial read/write/mixed HBM, L2-sensitive, vector-copy, SMEM, latency/
   concurrency, scalar/special-function, dequant, launch and transfer probes.
   Record raw data; final calibrated models belong to H1/H3.

Existing integration locations:
osprey/be/lno/tests/prepare_mla_q7.py, build_mla_q7.py, run_mla_q7.py,
gpu_test_worker.py, gpu_test_http.py, gpu_test_client.py, gpu_test_results.py,
and gpu_scaffold/server. Extend by explicit suite registration/adapters after
reading the scaffold's own guide/specification; preserve adoption scripts,
single-command packaging, cancellation, leases and result validation.

Bootstrap read-only commands before a structured collector exists:

~~~sh
nvidia-smi -L
nvidia-smi -q -x
nvidia-smi topo -m
nvcc --version
ncu --version
compute-sanitizer --version
~~~

These are operator instructions, not commands already executed on a server.
Record each command's exit status; an absent tool/field is unknown, not a
successful empty inventory. The planned CUDA/Driver resource probe collects
attributes that nvidia-smi does not expose. Do not change clocks or power
limits as part of automatic discovery.

**Gate:** inventory and environment digest established; baseline numerical
checks pass on device; requested instruction probes either pass or explicitly
disable their family. Reproducible failures are retained and packaged.

## 3. H1 — Device/SKU/partition and measured-profile extension

**Owners/files:** common/targ_info/access/dsl_memory_hierarchy.{h,cxx},
dsl_hardware_performance.h, dsl_compute_profile.{h,cxx}; profile import and
option resolution under the existing driver/config ownership.

Deliver a versioned runtime-only extension, leaving the current description
structs, estimated rows and reference_cycles interpretation intact:

- Exact architecture capability identity; independent resource-envelope and
  execution-profile keys with supported materializer/toolchain bindings.
- Physical units, per-field provenance, conditions, uncertainty and explicit
  unknown values; separate per-device and per-SM quantities.
- Immutable import with content digests and strict schema validation.
  Do not add GPU probing or CUDA/NVML/JSON-library linkage to be.so.
- A proposed profile-import control only after the contract is reviewed.
  Document its owner and complete option propagation; unrelated phases
  continue ignoring it. No new option is implemented by this document.

Tests: old table/override parity; complete suffix matching; wrong SKU, MIG
partition, schema, clock/unit, precision or toolchain rejection; missing/
duplicate/overflowed fields; false provenance; import isolation; changed
measurement changing the profile key; unknown costs refusing selection.
Test code-legality compatibility separately from performance-evidence reuse.

**Gate:** pure lookup/import and consumer contract tests pass; affected products
are link-closed; existing provider behavior and native format are unchanged.

## 4. H2 — Owned movement, reuse and lifetime/event contracts

**Owners/files:** LNO dsl_high_snapshot*, dsl_high_reuse*, dsl_high_observer*;
VHO tensor_high_lower*, dsl_tile_candidate*, dsl_fetch_pipeline*,
dsl_physical_plan* and operator-owned source-to-High adapters and planners. common/com holds policy-free identities/records
only when its established applicability contract permits them.

First admit or generate each family's canonical common High algorithm under
its verified domain/numerical contract, with DO_LOOP/OPR_ARRAY forms that LNO
can analyze. Extend the current bounded MLA route explicitly for other
families; R2 binding or a domain name does not supply a missing executable
algorithm. Unsupported forms retain an admitted fallback or diagnose before
emission. This preparation is outside per-candidate estimation and needs its
own before/after and optimization-level checks.

Deliver separate ledgers for useful payload, requests, transactions,
L2/HBM/SMEM service, spills, scale/page/index work and partial results. Extend
the existing Work Partition/Resource Lifetime/Execution Events views with
raw/decoded slots, generations, exact producer/consumer joins and alias ranges.
Bind Q3 cache/scale/page/publication facts for MLA. Keep all source facts
owned and validatable after native analysis lifetime ends.

The same records support GEMM operands, MLA latent/rotary caches, LayerNorm
rows/statistics and Reduction accumulators. Their matchers and algebra remain
operation-specific. WHIRL opcodes define operation semantics;
TensorDescriptorIR and tensor traits carry semantic tensor properties.
Existing Open64 analyses, phase-owned legality checks and verifiers establish
admissibility. Derived scheduling, resource and lifetime facts belong to
owned planning records with explicit validity epochs.

Tests: same-shaped different storage; changed views/strides; stale cache,
scale and page generations; append/publication ordering; unknown alias;
head-dependent cache addressing; raw-ready versus decoded-ready versus
readers-complete; tails and inactive participants; overflow; stale mutation
epochs; access reuse after cleanup and recapture before selected application.

**Gate:** owned facts and event validation refuse unsupported proofs; the
read-only observer leaves WN/ST/TY/DST and native analysis images unchanged.
No new transformation is implied by a fact or estimated reuse opportunity.

## 5. H3 — Shared costing, calibration and finite search

**Owners/files:** VHO dsl_gemm_readiness.h, dsl_gemm_occupancy.h,
dsl_physical_plan_opt*, tensor_attention_search*, tensor_attention_movement*,
tensor_quantized_movement*; WOPT opt_dsl_normalization_cost* and reduction
consumers through reviewed adapters; LNO observer/control integration.

Deliver:

1. Operation-specific instruction/event work with one common traffic/resource
   accounting vocabulary. Version adapters instead of assigning new meaning
   to old model fields or borrowing GEMM scalar rates for Tensor Cores.
2. Global HBM/L2, per-SM SMEM/compute/issue, startup/drain, waves and finite
   outstanding-work constraints. Distinguish dependency latency and engine
   service; include measured interference only under matching conditions.
3. Joint tile/group/chunk/layout/stage alternatives. Candidate census carries
   executable capability separately from modeled eligibility.
4. External calibration output with cold, warm, capacity-exceeding and
   contention conditions. Import once before search; no measurement inside it.
5. Deterministic portfolios with existing global budgets, estimate caching,
   stop reasons and best-estimated-in-declared-space conclusion.

Initial proposed seed catalog, to be versioned and pruned by legality:

| Family | Bounded seed axes |
| --- | --- |
| Shared movement | Direct/cooperative/bulk; one to three raw stages; one or two decoded stages; supported aligned layouts |
| GEMM | Start with existing implemented tile/compute recipes; add only reviewed tile/depth/persistent-accumulator siblings |
| MLA | G=1/2/4/8; existing legal chunk widths including 32/64; permitted query tiles and column retention; source-order families kept distinct |
| LayerNorm | Direct/centered retained; admitted block/vector widths, row tiles and pointwise fusion |
| Reduction | Ordered row owner or explicitly permitted tree; block/vector/items-per-thread and separately admitted split family |

These axes are a design seed, not an instruction to enumerate their full
Cartesian product. Default initial policy: at most 128 new estimated candidates
per site across all strategies, a request-wide total of 1,024 new estimates,
64 MiB live detached search storage, deterministic ties and an explicit
deadline supplied by the compilation service. Count shared cached estimates
once. A deadline hit preserves the best fully legal
incumbent or existing fallback. Budget values are proposed controls, not
already registered options or promises of sufficient search coverage.

Unknown model inputs, unavailable lowering and unsupported numerical forms
remain visible refusals. Default new-materialization budget is one selected
candidate per admitted site and round, with a separate request-wide selected
application cap; reuse a qualified baseline. The initial managed route stays
one admitted site per PU. Later multi-site application must recapture/rebind
after each mutation while retaining the request-wide budget. A failed native/
resource/correctness check falls back and produces evidence for a separately
budgeted future round.

Tests: independent traffic and event-schedule oracles; device-wide bandwidth
sharing; useful-versus-physical byte/rate consistency and no startup/service
double counting; counterexamples where larger tiles/groups/depth lose; cache-scenario
changes; unknown/overflow; finite queue/coverage effects; exact global budget
exhaustion; estimate-cache identity; no native allocation or subprocess/device
activity during exploration; existing control/fallback parity.

**Gate:** complete model components reconcile; old schema results remain
readable; selected application is isolated from estimation; synthetic versus
measured results are labeled at every report boundary.

## 6. H4R/H4N — Reduction and LayerNorm delivery

### H4R: executable Reduction first principles

**Owners:** opt_dsl_reduction*, opt_dsl_reduction_staged*, existing runtime
buffer/guard services, selected High lowering and native emission consumers.

Close the direct-binding document's executable-lowering gap with an ordered
fallback and device/reference evidence. Add coalesced cooperative transport
and independent row ownership without changing the fold. Add register/warp/
CTA and split-merge alternatives only under their numerical contracts.
Producer fusion must preserve side effects and source rounding.

### H4N: LayerNorm retention and arithmetic delivery

**Owners:** opt_dsl_normalization_{plan,cost,lower,request}*, normalization_driver*,
existing normalization runtime adapters and selected High/memory consumers.

Qualify current C0/C1/C2 on H20. Characterize source-ordered statistics and
actual private-carrier/register/spill traffic. Implement row/layout/vector
improvements and guarded centered-value retention first. A register-operand
arithmetic recipe or tree/Welford alternative is a separate semantic and target
conformance slice; do not silently substitute it for LN-REF-1.

Both suites cover awkward widths, few/many rows, tails, alias/range guards,
NaN/Inf/subnormal/signed-zero behavior under the actual source policy,
strict-order negative controls and separately authorized tolerance paths.
Use a matching explicit reference; CUB/PyTorch/library reductions are only
performance comparators when their numerical contracts are compatible.
Exact ordered and unresolved source versions retain distinct treatment.

**Gate per selected realization:** before/after genuine WHIRL, independent
reopen, exact/tolerance-specific reference result, device correctness and race
checks, resource report, complete-call timing and fallback. Transfer efficiency
does not excuse a numerically inadmissible reduction.

## 7. H5 — GEMM steady-state hierarchy and compute supply

**Owners:** VHO dsl_physical_plan_opt*, dsl_fetch_pipeline_opt*, dsl_wgmma*,
selected WHIRL builders and existing whirl2c/NVISA consumers.

Qualify current SIMT and explicitly admitted MMA recipes. Introduce persistent
accumulator lifetime across K, then issue-next/compute-current overlap with
exact operand-ready, MMA-complete and safe-slot-reuse boundaries. These are
separate from MMA5-H's existing two-slot batch behavior.

Cost output/K tile, direct/cooperative/async delivery, layouts, packing,
epilogue and residency together. Add producer/consumer specialization,
descriptor-based tensor-map copies or persistent output scheduling only after
their individual setup/progress/resource contracts. Preserve strict F32;
F16/BF16/TF32/FP8 choices require source permission and matching oracles.

Cases: aligned and ragged M/N/K, short/long K, skinny/batched GEMM, transposed/
strided contracts actually supported, beta/output-accumulation policy,
working sets inside/outside L2, warm/cold inputs and precision-specific modes.
Use a pinned, same-device cuBLASLt configuration with recorded workspace,
layout, math mode and algorithm; include repack and setup at the declared
boundary rather than hiding them.

**Gate:** one selected schedule has native before/after and instruction/
resource audits, device/reference and race evidence, useful steady state
where claimed, and a complete latency comparison. The optimizer may keep
direct delivery or fewer stages.

## 8. H6 — MLA cross-head reuse and quantized staging

### H6a: FP32 grouped-head materialization

**Owners:** dsl_high_reuse*, tensor_attention_movement*, tensor_high_selected.inc,
tensor_attention_device.inc and selected-plan application/bridge consumers.

Turn MR2's G>1 analysis-only families into explicit within-CTA schedules,
shared tile ownership and per-head stable tuple merges. Capability metadata
changes only after independent native and reference verification. Co-search
G/chunk/query/column choices; account for full head-slot reservation, reduced
CTA coverage, rotary/query loads and partial/merge traffic.

### H6b: Q6 strict-path efficiency and pipelining

**Owners:** tensor_high_q6.inc, tensor_quantized_{plan,gpu,device,stage}*,
tensor_serving*, libopen64rt/mla_serving_cuda.cxx and Q3 bindings.

First account for complete cache/scale result copies. Prove copy elimination,
view publication or copy-on-write legality before changing them; source old
versions, external aliases and Q3 generations must survive. Current disjoint
buffers remain mandatory unless a new serving contract is separately reviewed.
Then parallelize remaining independent prepare/finalize outputs and explicitly
publish shared physical-run construction where legal. Compare direct/cooperative/
byte-bulk movement, raw and decoded depth separately, and finer decoded
column layouts with correct pass/lifetime costs. Preserve source-ordered
folds; online/warp/split alternatives require separately admitted permissions.
Test the old 48 KiB envelope and the decoded R512/rotary64 refusal explicitly.
Query actual opt-in limits and combine all live arenas/barriers before any
larger reservation is accepted.

### H6c: page and scale locality

Bind alignment, payload/scale strides and valid physical runs to Q3 page maps,
cache/scale generations and publication lifetimes. Report serving token-block
recommendations, fragmentation and tail costs through an explicit contract.
Do not rewrite allocator page size or infer prefill/decode merely from cache
traits, quantization or query count. Preserve unknown phase cases and keep
the AOT phase-selection mechanism deferred.

Cases: short/long context crossed with query count and head count; latent/
rotary dimensions; source precision and authorized numeric alternatives;
dense/paged, packed U4/S8 and scale layouts actually supported; fragmented/
nonconsecutive pages; head/token tails; cache warmth, append/verify, stale
generations and cancellation/completion publication. All partial tuples,
projections, cache updates, launches and preparation are in the invocation
measurement boundary.

**Gate:** selected-only transformation, complete Q3-compatible device
publication and retained COMMENT explanations survive; correctness/race and
resource checks pass; predicted reuse is validated with actual traffic where
available. No HBM saving is inferred solely from reduced logical requests.

## 9. H7 — Four-suite hardware qualification and release

Extend the current combined package with explicit LayerNorm and Reduction suite
adapters. Preserve one run command, suite-adoption scripts, common job/progress/
cancellation, GPU leases and automatic result packaging on failure. Use the
shared validator. Additive evidence must be versioned; if current strict
parsers reject new fields/kinds, provide a reviewed new schema and adapter.
Do not relabel a reduction as GEMM or fork an incompatible result format.

Every case records numerical contract, input/producer hashes, baseline and
candidate IDs, selected schedule/profile, resource requirements, cache regime,
correctness and sanitizer status, ordinary timing, optional counter evidence,
and known/estimated/unknown status. Schema acceptance does not certify speed.

Qualification protocol:

1. Check device baseline and candidate numerical results before performance.
2. Run race/memory/synchronization checking separately; retain missing-tool
   status without converting it to PASS.
3. Run paired ordinary timings under a stable lease/environment, randomizing
   order where appropriate. Report kernel, GPU-invocation and end-to-end time.
   Tiny kernels need enough repetitions for timer resolution; graph timing is
   a separately labeled execution mode.
4. Profile fixed products separately. Query available counters and record
   collection/replay/cache controls. Physical traffic uses the profiler's
   matching duration; ordinary latency uses the timing lane.
5. Validate the three existing evidence groups: supply-related issue activity,
   path service, and local/peer/remote attribution. Preserve unknowns and
   denominator semantics; never turn sampled warp stalls into universal
   whole-device wait-time percentages.
6. Compare representative workloads and model prediction error, including
   cases where extra reuse, quantization or staging loses. Publish fallback
   decisions and failed selections as useful outcomes.

Proposed acceptance policy: require the selected plan to beat its same-boundary
baseline by the declared margin under paired measurement uncertainty, or keep
the baseline. An initial review margin of 5% can be used as a project policy,
not a hardware constant. Declare workload weights and latency/SLO constraints
before comparison. Report held-out cases and tail behavior. No universal
percentage of peak bandwidth is required for compute/launch/ordered paths.

**Gate:** independently validated result archive, exact reproducibility record,
resource/profile compatibility, no unexplained supported-case regression, and
documented remaining unknowns. No global-optimum or universal H20 speedup claim.

## 10. H8 — Optional cluster/DSM/multicast reuse

Start only if H0-H7 show a remaining cross-CTA supply bottleneck and the
actual device/toolchain qualifies the mechanism. Add cluster residency and
participant progress, DSM access and lifetime, descriptor/multicast setup and
layout requirements as a separate complete candidate. Compare against
within-CTA and cache-served baselines. Fewer HBM requests may lose to cluster
synchronization and reduced scheduling flexibility.

This is one-GPU cluster work. Multi-board collectives, NVLink/RDMA compiler
lowering and cross-PU optimization need their existing separate contracts.
Topology measurement alone does not authorize them. H8 is optional and is not
a prerequisite for the first useful H20 release.

## 11. Per-change certification and implementation boundaries

The initial joint loop/memory search runs at O3 at the existing Lnoptimizer
full-graph hook, with active FUNC_ENTRY/PU and admitted loop subsets. Arbitrary
REGION roots and current autopar/Lego/MP exclusions stay refused until a
separate entry/analysis extension is tested. Preserve O0 semantic lowering,
O1 local behavior and O2 PU-local contracts. Cross-PU changes require IPA. Match the scope of tests
to the actual analysis/transformation.

For every selected IR change:

- Capture a real immediate before.B and after.B; reopen each in a separate
  process with ir_b2a -st -src to matching before.T/after.T and retain raw diff.
- Preserve source files and COMMENT explanations in High, later WHIRL and
  whirl2c CUDA, including numerical assumptions, ownership, partial/merge
  rules, waits, generations, resource choices and validation status.
- Check atomic refusal/rollback, recapture, analysis invalidation/rebuild,
  post-PREOPT rebinding, High exit, Mid structured memory lowering and Low
  GPU mapping/PTX. Cache actual resource reports to the exact selected code.
- Check no frontend-builder/CUDA library leakage into be.so and shared-consumer
  closure when affected. No unapproved library or binary layout changes.
- Promote accepted evidence to new immutable review receipts and keep earlier
  receipts. One failed experiment never overwrites its baseline.

Late resource failure uses fallback. Re-estimation with corrected resource
data starts a separately budgeted compilation round. Candidate exploration
does not acquire permission to compile and benchmark losers.

## 12. Focused PR and handoff plan

Keep one coherent contract change per PR: inventory/result adapter; profile
schema/import; owned facts; traffic/resource oracle; bounded selection;
one operation's selected materializer; one memory/GPU bridge; and device
qualification. Documentation and archived outputs do not need a broad native
producer rerun unless their producer inputs change.

The minimum transferable package contains:

1. Decision record and rejected alternatives, numerical/serving contracts,
   phase/scope/ownership diagram and exact supported/refused cases.
2. Source/patch/commit, baseline and selected artifacts, tool/environment and
   input digests, resource/profile/model/candidate identities and search budgets.
3. One-command run/return/validate instructions, independent oracles and
   negative cases, native before/after traces/diffs, annotated CUDA/PTX and
   ordinary timing separated from profiling.
4. Reproduction and acceptance checklist, known failures/unknowns, rollback/
   fallback procedure, outstanding risks and named transfer roles.

AOT selection design, PREOPT COMMENT preservation repair and OpenCTI remain
deferred. Existing COMMENT workarounds, Q3 authority, service APIs and fallback
routes remain. The 2026-11-12 request starts this design and proposed H20
qualification queue; it does not mark the historical PERF0-PERF6 dashboard
work implemented or reopen unrelated framework integration.
