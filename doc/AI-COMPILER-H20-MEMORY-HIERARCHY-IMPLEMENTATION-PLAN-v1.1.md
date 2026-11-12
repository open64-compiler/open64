# H20 memory hierarchy implementation plan v1.1

2026-11-12 | H1 exact device selection deliverable

H1b implementation update: [immutable resource/profile import v1.0](AI-COMPILER-H1B-GPU-PROFILE-IMPORT-v1.0.md)
now resolves pinned external facts for both H20 variants into sealed,
driver-owned contexts. Strict schema/quantity/provenance/binding checks,
separate resource/execution/budget digests, explicit unknowns and failure
preservation are implemented. Architecture-only/H1a behavior and generic
fallback remain unchanged. H1c consumer/cache/selected-plan integration and
H1d product/serving qualification remain open; imported facts are not yet
inputs to the existing generic planner. The earlier H1a status below records
its original scope.

H1a implementation update: [exact option/identity admission](AI-COMPILER-H1A-GPU-DEVICE-SELECTION-v1.0.md)
now implements both H20 device keys, baseline sm_90 compatibility, device-only
effective ISA, early refusals and generic-path reporting. Focused native and
backend parity checks pass. H1b-H1d remain open; this is not resource/profile
import, device-specific planner integration or GPU qualification. H1a's explicit
device/ISA admission is conservatively sm_90-only pending separate sm_90a
qualification; architecture-only behavior is preserved.

This revision replaces the H1 section of
[plan v1.0](AI-COMPILER-H20-MEMORY-HIERARCHY-IMPLEMENTATION-PLAN-v1.0.md)
under [resource/memory design v0.3](AI-COMPILER-HARDWARE-RESOURCE-AND-MEMORY-PLANNING-DESIGN-v0.3.md).
H0 and H2-H8 retain their earlier scope and gates, with the H1 resolved-context
dependency made explicit below. No implementation gate is closed by publishing
this plan. The user has H20 96 GB servers; access, inventory and measurements
remain pending. H20 141 GB is an additional required selection/profile-contract
case, not a newly available or qualified server. Transfer roles remain TBD.

## 1. H1 mandatory outputs

H1 must implement and test exact `-DSL:gpu_device=h20_96gb` and
`-DSL:gpu_device=h20_141gb` selection through the existing backend option group.
The current `gpu_arch=sm_90` lookup selects a synthetic architecture envelope
and cannot alone distinguish H20 from H200. Device-specific selection and
reporting must not inherit that limitation.

| Slice | Owners and concrete work | Exit evidence |
| --- | --- | --- |
| H1a option and registry contract | common/util/config_dsl.h/.cxx registers gpu_device; existing driver/config plumbing propagates the full option set. common/targ_info/access/dsl_memory_hierarchy and related description/profile owners provide exact immutable device keys and separate resource/calibration revisions. Publish exact versioned fields/import contract before implementation | Positive 96/141 selection, exact spelling, unsupported device and incompatible ISA diagnostics; omitted-device generic parity; no new be.so dependency or native format change |
| H1b immutable resolution and import | Existing driver/scope owners resolve ISA, device, partition/resource, calibration and workload-budget inputs once. Validate schema, units, provenance, toolchain/materializer applicability and all identities; authoritative defaults and imports leave missing facts unknown | Resource-only and calibrated fixtures, corrupt/wrong-device/partition/unit/digest refusals, immutable import isolation and explicit unknown handling |
| H1c four-family consumers and bindings | Existing VHO/WOPT/LNO adapters consume the shared context in their owned scope. Update snapshot/search keys, legality/cost dependencies, selected-plan guards and reports; common records remain policy-free. Broader four-family lowering stays in H2/H4-H6 | Each existing admitted family path or a focused owner-level fixture proves its resource/cost queries use the resolved context. No implied new executable algorithm or GPU acceptance |
| H1d guard and qualification boundary | Existing host/serving requirement and result owners carry resource/budget/profile bindings and compatible fallback obligations; keep Q3 generations/publication and COMMENT explanations | Mismatch/unknown/stale-budget checks; architecture-only reports cannot claim device-specific tuning or qualification; code compatibility tested separately from performance-evidence reuse |

Concrete source anchors are
[config_dsl.cxx](../osprey/common/util/config_dsl.cxx),
[dsl_lower.cxx](../osprey/be/vho/dsl_lower.cxx),
[driver.cxx](../osprey/be/be/driver.cxx),
[dsl_memory_hierarchy.cxx](../osprey/common/targ_info/access/dsl_memory_hierarchy.cxx),
[dsl_high_snapshot.h](../osprey/be/lno/dsl_high_snapshot.h), and the existing
operator-owned planning/profile adapters. Reuse their extension points;
this table registers no new symbol, JSON schema or mapped-image row itself.

Audit existing raw-option checks as well as lookup calls. For example, the
driver's bounded LN4 request currently checks gpu_arch presence. Device-only
selection must reach a validated effective ISA before such target checks;
preserve the request's other paired-option, O3, scope and checkpoint guards.
Do not merely register gpu_device while downstream consumers continue using
the old architecture-only description or reject its absence as a raw string.

## 2. Option resolution and required cases

The exact behavior is governed by design v0.3. In particular:

1. gpu_device selects a device identity and reviewed baseline ISA if gpu_arch
   is absent; an explicitly supplied architecture must be compatible. Keep
   exact suffix checks and separate instruction-family qualification.
2. Resolve the physical variant independently of visible partition resources,
   execution calibration and the workload's usable allocation budget. Require
   identity evidence beyond memory capacity; do not parse 96gb as literal
   allocatable GiB. No H200/H100 alias or invented H20 data is permitted.
3. Preserve old architecture-only defaults, structs, estimated rows and
   reference_cycles interpretation. Such a run has no implied device identity.
   Explicit device requests cannot silently degrade into synthetic
   device-specific optimization or qualification.
4. Valid but incomplete profiles leave dependent candidates ineligible. An
   independently valid compatible semantic fallback may remain available.
   Malformed/conflicting explicit inputs diagnose before search/publication;
   runtime resource/budget failures follow existing guarded fallback/refusal.

## 3. Focused validation matrix

| Case | Required assertion |
| --- | --- |
| Same sm_90, h20_96gb versus h20_141gb | Distinct device/resource identities and keys; each consumer receives the selected input, even if identical justified inputs yield identical code |
| Different HBM budgets only | Aggregate live-storage eligibility can change; CTA SMEM/register limits and local tile sizes are not automatically enlarged |
| Different resource/service fixtures | Controlled sentinel limits/rates affect the relevant capacity, coverage or complete-cost calculation; no unrelated numerical transformation is enabled |
| H200 profile under either H20 key | Reject profile applicability despite shared ISA or a matching nominal memory label; no H200 throughput or bandwidth substituted |
| Missing/unknown device and exact spelling | Omitted device preserves generic parity and honest labels; unknown values, m90 aliases and incompatible explicit architectures diagnose |
| Device supplied without gpu_arch | Resolve the reviewed baseline ISA; do not enable an unqualified architecture-specific mechanism |
| MIG/partition and model mismatch | Validate model, partition identity, visible resources and budget independently; reduced visible memory is not automatically a wrong physical board |
| Missing fields, overflow or bad provenance | Unknown dependent costs cannot win; malformed schema, units, revisions and digests fail without changing source or prior outputs |
| Changed device/resource/budget after selection | Reject stale admission/plan/guard bindings before commit; preserve source, analyses and accepted fallback |
| Calibration-only change | Invalidate cost cache/ranking/stopping bounds and evidence bindings; preserve code-legality facts only when all their dependencies remain valid |
| Estimation-only exploration | Resolve once; all strategies share immutable facts and global budgets; prove no candidate WN/ST/TY mutation, probe, compile, correctness run or timing |
| Selected application and fallback | Preserve original source, numerical/Q3 identities, COMMENT, rollback, analysis rebuild and verified High exit; fallback has independently valid resource assumptions |
| Reporting and qualification | Print requested/resolved ISA/device/resource/partition/profile/model/budget identities and provenance; synthetic architecture estimates cannot certify an H20/H200 result |
| Compatibility and scope | Old no-device review artifacts and option behavior preserved; O0 semantic-only and established O1/O2/O3/IPA scopes remain unchanged |

Fixture numbers must be explicitly synthetic test inputs, not published H20
specifications. Parser/import/consumer tests for both variants do not count as
device execution on both variants. Any changed native transformation must retain
the existing before/after B/T/diff evidence; record-only additions use the
corresponding structural/compatibility tests. Keep the implementation PRs small
and retain their independent evidence.

## 4. H1 completion and downstream dependencies

H1 closes only when H1a-H1d and the applicable focused matrix pass, all affected
shared-library consumers remain link-closed, the versioned contract is reviewed,
and each changed file passes the repository compliance gate. Source evidence
must show gpu_device is actually registered and reaches consumers; a design
document or unused registry entry does not satisfy this milestone.

H2 captures the resolved identities with owned movement/reuse facts. H3 includes
them in admission, caching, full costs and convergence dependencies. H4-H6 use
the context through their existing legality and selected-materialization owners.
H7 binds actual inventory and results to the compiled requirements. No H1 test
may silently mark H0/H7 hardware qualification complete, particularly for the
unavailable 141 GB variant.

The sequence remains immutable resolution -> read-only estimation -> selected
transaction -> correctness/resource qualification. A late failure does not
start an unbounded candidate-lowering loop. Native WHIRL opcode/tensor-trait
ownership, COMMENT, Q3, fallback, AOT baseline and deferred AOT selection,
PREOPT COMMENT repair and OpenCTI remain unchanged.

Generic builds may still be correctness-tested and timed on an identified
device through H0/H7. Report actual product/conditions and measured evidence;
do not describe that measurement as proof that an architecture-only synthetic
model was device-calibrated or chose the best device-specific plan.
