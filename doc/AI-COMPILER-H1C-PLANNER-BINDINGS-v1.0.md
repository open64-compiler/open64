# H1c immutable device bindings in planner owners v1.0

2026-11-12 | Driver scope, owned snapshots, dependency keys and selected acceptance

This extends [H1b import](AI-COMPILER-H1B-GPU-PROFILE-IMPORT-v1.0.md) under
[H20 plan v1.1](AI-COMPILER-H20-MEMORY-HIERARCHY-IMPLEMENTATION-PLAN-v1.1.md).
It implements compiler-side consumers for both exact H20 variants. Profiles in
validation are synthetic sentinels; neither board capacity nor calibration is
inferred from the variant spelling. H1d host/serving qualification remains open.

## Main design choice

Resolve/import once in the driver, publish a borrowed const context for its
synchronous phase lifetime, and copy owned values at each planner boundary.
Candidates receive only detached records. They never reopen the profile, probe
hardware, create WN/ST/TY, compile a kernel, run correctness or collect timing.
The driver clears its borrow before destroying the H1b owner.

The backend-owned `be/com/dsl_gpu_plan.h` adapter contains an owned
`DSL_GPU_PLAN_SNAPSHOT` and a POD `DSL_GPU_PLAN_STAMP`. The snapshot copies typed
facts, evidence, identities and section digests. The POD stamp fits existing
memset-initialized compiler-only contracts. Neither is a mapped WHIRL row or a
runtime ABI. Common target records remain policy-free; each phase retains its
source legality, scope, numerical permissions, cost selection and transformation.

Architecture-only requests have empty device keys and preserve old key spelling
and behavior. An explicit device without a file retains H1a identity-only
planning: its unknown resources do not acquire invented values, but its identity
is sealed so a later device change cannot revive a selected binding. Imported
profiles additionally constrain represented resource and budget requirements.

## Dependency boundaries

| State | Key/use | Invalidation |
| --- | --- | --- |
| Source and native analyses | Existing source/fact keys and activation/mutation epoch | Existing mutation, DU, alias, dependence and rebuild rules |
| Resource and usable budget | H1b resource/budget digests in Legality_key | Device, partition, inventory generation, limits or budget changes invalidate capacity admission |
| Complete evidence/cost input | Resource/execution/budget/context digests in Cache_key | Calibration-only changes invalidate cost reuse, ranking evidence and selected acceptance |
| Driver's current context | Fresh stamp compared with captured stamp | Refuse stale selection before mutation, guard export, staged slicing or device construction |

Input-byte changes conservatively invalidate complete acceptance even when
numeric values happen to match. Reordering behavior follows H1b. Calibration-only
changes preserve the resource/budget legality key when those dependencies are
unchanged. Equality does not depend on struct padding. A failed freshness check
cannot be repaired merely by recomputing a key from the stale copied request.

The existing MLA lazy cache is invocation-local. Each invocation owns one frozen
input and its cache; reports and the selected production-input seal now include
the new dependency keys. No new persistent cache or candidate callback is added.

## Four owning consumers

| Owner | Implemented H1c use |
| --- | --- |
| VHO GEMM | Capture before joint search; filter detached tile/fetch pairs against imported CTA reservations and aggregate A/B/C storage; independently recheck the fixed winner before output. Pending device handoff owns a snapshot and checks it against the current context before constructing another device PU. The admitted tile/fetch pair survives reconstruction by semantic specification, with complete common selection census and site/IR consistency. Compatible direct demand can supply the synchronous fallback when imported limits exclude staged siblings. Infeasible explicit output requests diagnose as user refusals and preserve prior output. |
| LNO MLA | Capture device evidence with common High bindings; join the observer GPU envelope to exact ISA, visible SMs and non-overstated imported limits. Each proposed schedule checks declared register/shared CTA demand and aggregate distinct source ABI buffers. The selected High tuple reuses already-counted private backing; its scratch-policy limit remains separately checked. Search/resource/model/production keys bind evidence. Selected application independently revalidates the live context through High recapture before native mutation. |
| WOPT LayerNorm | Bound source/target/plan and selection records carry POD stamps. Imported thread/shared capacity, allocation quantum, visible SMs, RF and aggregate canonical backing capacities constrain existing legality and analytical occupancy. Preparation compares expected/current stamps; fixed lowering validates current evidence, footprint and CTA reservations before allocation/verification. |
| WOPT Reduction | Direct binding checks imported ISA and distinct backing allocations against usable budget. Staged binding additionally uses imported thread/shared limits and quantum, retaining explicit declared other-shared reservations. Direct guard export and staged slicing reject stale stamps before native rebinding. No missing RF/launch proof is invented. |

Allocation quanta and warp topology require exact bounds: a smaller uncertain quantum cannot prove a safe reservation. RF reservations preserve the existing analytical per-warp rounding and groups of four allocated warps; they do not infer an allocation rule from the H20 name.

Resource refusal does not grant runtime obligations. Imported shared-memory
checks use the default CTA limit; this milestone does not silently discharge
opt-in configuration. Declared register reservations are analytical upper
requirements, not a proof of actual allocation, spills or achieved occupancy.
LayerNorm and Reduction retain their existing bank/layout and event contracts.
Their native/source, numerical and Q3/publication obligations are unchanged.

Aggregate storage uses represented distinct canonical backing allocations.
It is conservative about unused capacity and source ABI buffers, checks overflow,
and uses the independently proved usable budget. It does not account for
unrepresented serving allocations or guarantee a future allocation; those remain
explicit runtime/serving requirements.

## Physical service and legacy cost models

The existing GEMM/MLA/LayerNorm models use `reference_cycles`. H1b's physical
rates and nanosecond latencies cannot silently replace their instruction costs.
H1c therefore makes two uses explicit:

1. Imported resources/budgets constrain eligibility and represented occupancy.
2. Physical HBM mixed-service bounds are queried separately in nanoseconds:
   `lower_ns = bytes / rate_upper * 1e9`,
   `upper_ns = bytes / rate_lower * 1e9`.

The service interval describes one engine component under the declared rate
bounds. It is not an interval for complete kernel latency: cache misses,
transactions, instruction dependencies, synchronization and overlap still need
their respective models. Unknown physical rates remain unknown, with output
parameters preserved on a failed query. No zero-cost or infinite-capacity default
is created. Changing a supplied rate changes this component and the cost key.

Existing complete cycle-scenario rankings remain labelled as such. Reports
keep declared measured, estimated, synthetic and unknown physical evidence
distinct; a declared measurement is not hardware attestation. GEMM reports
an A/B/C payload-service component, MLA reports per-stage declared traffic, and
LayerNorm/Reduction report represented traffic separately. These are
resource-constrained generic analytical plans, not physically calibrated H20
profitability results. Complete physical-unit models, broader memory hierarchy
optimizations and hardware performance qualification remain later work; H1c
introduces no universal performance claim.

## Selected lowering and fallback

Only the selected MLA plan enters the existing atomic High transaction.
`tensor.high.selected.device` binds the copied evidence into the existing
selected-key seal using additive compiler metadata. Omitted-device seals retain
their old spelling. Source COMMENT explanations, rollback, analysis rebuild and
verified High exit remain in force.

The external M6 MLA device-helper process currently lacks a transport for the
resolved H1b context. The production driver checks selected/current identity and
retains the ordinary host route with `device_profile_bridge_transport_pending`
for a device-bound handoff. It does not discard evidence to obtain a generic
CUDA/PTX product. AOT variant selection remains deferred. H1d must define and
validate that process/host/serving transport before claiming device-bound
end-to-end GPU acceptance. Standalone quantized Q4/Q5 exploration likewise refuses
an imported device snapshot until its serving-specific profile adaptation is
bound; ordinary existing quantized routes remain available without that claim.

No-device paths and O0 semantic lowering retain existing behavior. Missing
facts make the dependent optimized candidate ineligible; ordinary compatible
semantic fallback remains available. A user explicitly requesting a CUDA image
with no admitted GEMM candidate receives a refusal before image publication.
Malformed imports still fail at H1b's earlier boundary.

## Validation and reproducibility

The focused checks exercise production APIs, not a second planner:

- C++98 snapshot/import contracts for both variants, ownership after context
  cleanup, explicit unknowns, register/shared overflow, physical-rate effects,
  identity-only freshness and independent calibration/resource/budget/partition
  changes; host ASan/UBSan is a separate lane.
- Actual GEMM backend success, tight/unknown-resource refusals with prior output
  preservation, and no-device `.B`/`.T` parity with the retained H1b compiler.
- Actual LNO common High capture, detached resource/cost/key consumers,
  native/tree/ST/TY/analysis isolation and detached-object symbol audits; selected
  High before/after mapped images, source-interleaved traces/diffs, whirl2c output
  and guarded host correctness against the saved MLA reference.
- Actual WOPT LayerNorm and direct/staged Reduction queries, stale selected
  preparation/guard/slice refusals with unchanged outputs, restored-context
  success, native-table preservation, imported limits and partial facts.
- Existing H1a/H1b import controls, estimation-only search regressions and
  shared-library dependency/consumer closure. No CUDA/NVML dependency is added.

Use fresh output directories. The native backend review authenticates its
retained H1b input receipt before reuse. Every compiler trace uses `ir_b2a -st
-src` with original source paths available. Review receipts are published under
the primary artifact root with complete checksums and no-replace promotion.
These checks are host/compiler evidence, not GPU execution or H20 calibration.

## Sanitizer validation correction

The earlier H1b sanitizer runner relied on process status and could accept
recoverable UBSan diagnostics from the vendored RapidJSON stack's capacity
check. H1c makes sanitizer errors fatal and checks captured diagnostics as well.
The narrow vendored fix checks available capacity without null/out-of-allocation
pointer arithmetic and defines empty-stack size/capacity without null subtraction.
The negative-index accessor test also uses an integer API boundary instead of
constructing an invalid enum value, so sanitizer checking covers a defined refusal.
The corrected import and planner suites must pass before sanitizer acceptance;
old receipts remain immutable historical evidence and are superseded for this
sanitizer claim by the new clean run.

## Focused results

The final review exercises 18 owned planner-profile fixtures across the two
variants and 97 strict import cases with fatal ASan/UBSan checks. Native
acceptance includes five ordinary GEMM successes, four resource/budget refusals,
and a staged-to-direct pair reconstruction regression. Eight WOPT owner cases
cover known, partial, tight-budget and unknown imports. Both native LNO observers
prove WN/ST/TY/analysis isolation; selected High is applied once per variant and
its whirl2c host view matches the saved FP64 reference. The largest absolute
errors are about 3.26e-9 and 7.55e-7 for the two output groups.

A separate detached 8,193-token, rank-64 fixture proves that an exact source-backing budget
admits existing High tuple reuse and a legal wider candidate, excludes narrow
widths that fail the selected private-tuple capacity proof, while a realization requiring additional
scratch cannot fit that same budget. This is accounting/estimator evidence,
not a long-context GPU execution result. No-device GEMM B/T parity, H1a/H1b
controls, two existing estimation-only search tests and shared-library closure
complete the selected validation. Retained reports give exact commands, binary
identities and before/after traces.
