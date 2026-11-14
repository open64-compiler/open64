# Appendix PN Physical nonidealities and Open64 feedback optimization

Revision 0.1 | November 4 2026 | Architecture adopted for implementation planning

## PN1 Decision and scope

Extend Open64's existing FEEDBACK infrastructure to carry the empirical evidence needed by AI compiler optimization. Physical resource configuration, effective execution capacity and operating conditions must participate in complete-plan profitability. The existing opcode system, tensor type/descriptor traits, numerical contracts, dependence proofs and WHIRL level boundaries remain authoritative. This appendix defines requirements and proposed extensions; it does not certify an implemented collector, calibrated model or GPU optimization.

Feedback is a service used throughout compilation, not a single late optimization pass. VHO, PREOPT/WOPT, LNO, fusion, memory and parallel planners, target lowering, CG and runtime variant selection need different views of the same attributed evidence. AI-P11 remains the logical telemetry/reporting phase; importing valid prior-run feedback before an earlier phase is necessary and does not reorder AI-P0 through AI-P11. LLVM feedback infrastructure is not the reference architecture for this work.

The preceding High WHIRL amendment remains binding. Very High WHIRL owns domain operators and gatekeeping. High WHIRL owns common algorithm expansion, DO_LOOP/OPR_ARRAY access/dependence reasoning, Wolf/unimodular and optional Cohen/polyhedral schedules, multi-level tiles, SMEM staging and executable prefetch DO_LOOP generation. Mid WHIRL lowers selected memory operations. Low WHIRL realizes GPU execution mapping before PTX generation. Physical feedback ranks legal alternatives; it cannot authorize an otherwise illegal reduction order, alias assumption, barrier, target instruction or numerical algorithm.

## PN2 Investigation record and evidence limits

The discussion in H100 Warp Relationship identified two mechanisms requiring a better compiler cost model: clustered execution can have less usable concurrency than the enabled SM count suggests, and memory-traffic changes can alter sustained compute frequency under power constraints. The following source-specific observations are retained separately from the compiler requirements derived from them.

| Quantity | Evidence and interpretation |
| --- | --- |
| 144 SMs | NVIDIA's full GH100 design description: 8 GPCs, 9 TPCs per GPC, 2 SMs per TPC. A die-design capacity, not an H100 SXM launch resource count. |
| 132 SMs | NVIDIA's H100 SXM5 product configuration. PCIe and other products require their own descriptors. |
| 132 versus 120 | Kuruppu et al. report 132 unique SM IDs per iteration for cluster sizes 1/2 and 120 for sizes 4/8 under their controlled residency conditions. |
| 124 across iterations | Their cluster-4/8 runs observe 124 unique SM IDs across all iterations. This union differs from per-iteration participation and inferred concurrent capacity. |
| Performance and power | The paper reports up to 5.30 percent FlashAttention improvement from its wave-aware strategy and explains frequency gains from reduced movement under TDP limits. These are experimental results, not guarantees. |

Sources: NVIDIA Hopper Architecture In-Depth and Kuruppu et al., Demystifying the Impact of Physical Non-Idealities on Kernel Performance, arXiv:2610.37466v2, November 3 2026. All numerical findings above are attributed literature evidence; this documentation update performs no reproduction.

Do not hard-code 120 or 124 in an sm_90/sm_90a architecture table. Do not extrapolate the H100 result to H200, GH200, Blackwell, Rubin, a MIG partition or another H100 device. A fully enabled hypothetical GH100 packing calculation is an analytical illustration, not measured hardware evidence. An inferred physical skyline has device- and experiment-specific provenance and is optional research evidence, not a required public hardware fact.

The distinction to retain is designed resources, enabled resources, cluster-eligible participation across runs, and effective simultaneous capacity for a particular kernel/configuration. Even without a recovered skyline, a calibrated effective-capacity function can support useful planning. A collection method must state which of these quantities it actually establishes.

## PN3 Separate architecture resources and execution profiles

Use three descriptions with different ownership and validity lifetimes. Reuse the existing hardware-description and target-selection records where compatible; the names in this appendix describe contracts rather than reserving new C++ types or binary sections.

| Description | Contents | Lifetime and authority |
| --- | --- | --- |
| Architecture contract | Exact ISA target and suffix, numerical/instruction capabilities, synchronization semantics, generic resource constraints and supported lowering routes | Pinned target/toolchain capability definition; governs instruction and representation legality |
| Device resource description | Vendor/model/SKU, device identity, enabled SMs, visible memory, partition/MIG configuration, launch/cluster constraints and configured power/clock policy | Queried or provisioned device/configuration snapshot; changes on reconfiguration |
| Calibrated execution profile | Effective cluster capacity by kernel resources and cluster shape, scheduling/wave evidence, sustained clocks, latency/traffic distributions and power response | Empirical evidence under a precise code/workload/environment fingerprint; may become stale |

The architecture target selects instruction compatibility. It does not identify the product's enabled-resource distribution or measured performance. NVIDIA's compiler documentation lists Hopper sm_90/sm_90a and separate Blackwell and Rubin target families; support and suffix semantics must be checked against the actual toolkit. Never infer a physical skyline from the target suffix. Pin the selected toolkit and architecture table revision in every package and profile.

Keep Device Physical Calibration Profile and Kernel Execution Feedback as separate views within Open64's feedback service. The first groups relatively stable device/configuration experiments; the second attributes observations to exact generated kernels and workloads. Capacity and power response can still depend on the kernel and environment, so a device-level record must declare those dependencies rather than claim a universal constant. A profile is not part of tensor type equivalence or immutable ISA semantics.

CUDA occupancy queries provide useful kernel/configuration bounds. In particular, cudaOccupancyMaxPotentialClusterSize and cudaOccupancyMaxActiveClusters are relevant adapter inputs. NVIDIA documents that runtime scheduling can make calculated cluster occupancy unachievable. Preserve the API result separately from measured capacity and never present an occupancy query as a recovered floorsweeping map or a hardware execution guarantee.

## PN4 Feedback data to collect

Collect information that can change a compiler decision, with a minimal first set and optional diagnostic tiers. Every field has an origin tag: specified capability, device query, compiler-derived fact, direct observation, empirical inference or model prediction. Unknown and unsupported fields are explicit states, not numeric zero. Metric names below are logical schema fields; exact NVIDIA counter names must be selected and pinned by the collector's capability inventory.

| Priority and group | Required data | Producer and decision enabled |
| --- | --- | --- |
| P0 identity | Device UUID or approved local identity, SKU, exact ISA target, enabled SM count, visible partition/MIG identity, driver/runtime/toolkit versions | CUDA/device-management adapter; profile compatibility and conservative resource planning |
| P0 workload and code | Shape/dtype/layout, semantic/numerical-contract identity, source region/plan/variant IDs, kernel code hash and compiler revision/options | Compiler plus launch manifest; join evidence to the exact candidate |
| P0 launch | Grid/block/cluster dimensions, actual launched CTA count, cluster count, dynamic SMEM, stream and concurrency class | Launch manifest/runtime; compare mappings and execution waves |
| P0 kernel resources | Registers per thread with block size, static/dynamic SMEM per CTA, spills/local bytes if available and relevant function attributes | Compiler/assembler/function attributes; explain occupancy bounds rather than blaming floorsweeping |
| P0 capacity bound | Supported cluster configurations and kernel-specific predicted active clusters, API result/status and limiting assumptions | Occupancy adapter; capability/resource screening |
| P0 calibration | Measured or inferred effective active-cluster capacity, controlled residency class, sample count and interval; optional per-run SM-ID participation sets | Dedicated calibration; estimate scheduling capacity and its uncertainty |
| P0 timing | Unprofiled device execution-time samples, warmup/repetition policy, launch/batch identity and timing scope | CUDA-event or reviewed target timing adapter; complete-plan comparison |
| P0 derived waves | Model version, input CTA/cluster counts, effective-capacity evidence, predicted waves and final-wave utilization | Compiler analytical model; compare tile and cluster choices without relabeling predictions as counters |
| P0 operating conditions | Power limit, clock policy, observed SM/memory-clock statistics where available, sampling interval, thermal/concurrency context | NVML or supported telemetry adapter; scope timing and clock-response reuse |
| P1 memory | HBM/L2 read/write traffic, hit behavior and supported TMA/DSMEM movement metrics, units and collection scope | Nsight Compute/CUPTI or reviewed adapter; refine locality, multicast and prefetch costs |
| P1 utilization | Achieved occupancy, active-warps/SM-activity measures, residency and relevant stall categories | Supported profiler metrics; distinguish capacity, latency, queue and scheduling limitations |
| P1 power outcome | Sampled power/energy window, temperature, clock-limiting indicators and energy attribution method | Device telemetry; compare power response with uncertainty and shared-device caveats |

The P0 set is a set of logical groups, not a promise that twelve scalar values are sufficient. For a particular comparison, required join keys, resource facts and observations must all be available. Without controlled calibration, retain an explicit unknown effective-capacity estimate and the conservative baseline. Profiling support may be unavailable due to device, toolkit, permissions or execution mode; report the missing capability and the decisions it prevents.

Do not confuse achieved warp occupancy with SM utilization. A union of SM IDs across a launch does not prove simultaneous residency. Per-SM clock64 timestamps do not supply a common global timeline without a justified synchronization/calibration method; use a reviewed common-time observation method when deriving concurrency. Instrumented probes perturb execution, so quantify their overhead and keep uninstrumented timing as a separate lane.

Device-level power/clock samples may be too coarse to isolate a short kernel. Collect a controlled steady-state batch with recorded sampling windows and concurrent activity. Any energy-per-kernel estimate must state integration, idle/background treatment and attribution uncertainty. Do not divide an unrelated board-power sample by a microsecond kernel time and label the result measured kernel energy.

## PN5 Cluster capacity wave and power models

Bind the actual launch before evaluating waves. Let CTA_count be the actual grid product, cluster_size the product of cluster dimensions, and cluster_count = CTA_count / cluster_size for an admitted divisible launch. Padding, masked work and tail CTAs still consume launch resources and must be counted; invalid grid/cluster compatibility is rejected before costing.

Let active_clusters_bound be the compatible occupancy-query result. Let effective_clusters(plan, device, environment) be a calibrated estimate or interval with independent provenance. For uniform-duration work with bulk waves, a first approximation is:

    predicted_waves = ceil(cluster_count / effective_clusters)
    final_wave_utilization = remaining_clusters / effective_clusters

Use final_wave_utilization = 1 when a nonempty grid exactly fills its last wave; zero work has zero waves and no division. Capacity must be positive for a nonempty launch. Distinguish these approximations from observed schedules. Persistent kernels, irregular work, heterogeneous CTA durations and concurrent launches require a work/queue model or explicit refusal of the bulk-wave model.

Only under an established one-active-CTA-per-SM calibration may CTA capacity be expressed as a participating-SM count. For general kernels, cluster_size times effective_clusters is concurrent CTA capacity, not necessarily a count of distinct active SMs. Never multiply enabled SMs by an assumed occupancy ratio and call the result a measured floorsweeping limit.

Illustrative model test with a one-CTA-per-SM reference capacity of 132 and a calibrated clustered capacity of 120: 240 CTAs require two bulk waves in either model; 256 require two versus three. These are arithmetic checks of the model inputs, not a new measurement or a universal decision rule. A wave-equalized configuration can still lose because of synchronization, transfers, bank conflicts, occupancy or instruction costs.

Couple compute and movement costs when calibrated evidence supports a frequency response. The scheduled execution model should evaluate compute-service times using a plan/environment-conditioned sustained-rate estimate, along with memory-service, issue, wait, release and synchronization events. Reduced movement can affect both exposed memory time and attainable compute rate. Keep those causal terms distinct and avoid adding a separate power bonus after compute time was already adjusted. The existing Roofline bound remains diagnostic and is not another additive latency term.

If no validated clock-response model exists, retain the prior estimate with an uncertainty interval; do not manufacture a frequency gain from saved bytes. Frequency-ratio normalization is permitted only as an explicitly assumed model with an applicable instruction mix and operating range. Empirical latency remains an observation of one complete variant/context, not a decomposable cost for arbitrary new variants.

Compare numerical-equivalent variants at default DVFS and, where supported and separately authorized for the test host, controlled clocks/power limits. Record the observed conditions in both lanes. Changing hardware clock or power settings is a later experiment action, not part of this documentation update. Keep kernel timing without profiler replay distinct from counter collection, including cache-control/replay effects and the measurement order.

## PN6 Open64 FEEDBACK reuse and extensions

The current AI worktree contains the following source anchors. They establish existing services to extend; they do not establish that GPU physical feedback is already supported.

| Existing source | Confirmed service and extension obligation |
| --- | --- |
| osprey/be/com/fb_whirl.h and fb_whirl.cxx | FEEDBACK, per-WN feedback association, Query/Annot methods, transformation/lowering/clone updates, Verify and Verify_and_guess. Preserve existing ownership and consistency behavior. |
| osprey/common/com/fb_freq.h | FB_FREQ exact, guessed, unknown, uninitialized and error states. Preserve frequency algebra and units; do not store latency, watts or bandwidth in this type. |
| osprey/common/com/fb_info.h and fb_info.cxx | Existing invocation/branch/loop/call and related profile records. Audit file and ABI consumers before any extension. |
| osprey/be/com/fb_cfg.h and fb_cfg.cxx | FB_CFG and frequency consistency verification. Extend empirical-data validation separately from count conservation. |
| osprey/be/com/wn_instrument.h and wn_instrument.cxx | WN_Instrument and WN_Annotate for established profile phases. Device collection uses a reviewed adapter while Open64 retains identity, import and consumption ownership. |
| osprey/common/com/instr_reader.h and osprey/common/instrument/instr_reader.cxx | Profile-file ingestion anchor; compatibility and schema dispatch must be reviewed before publishing an extension. |
| osprey/be/lno/snl_trans.cxx | LNO_FB_Inv_Interchange, LNO_FB_MP_Tile and loop-frequency update mechanisms. Preserve them for frequency feedback; performance distributions need separate validity rules. |
| osprey/be/be/driver.cxx | Instrumentation/annotation before VHO, LNO, WOPT and CG, with FEEDBACK verification at selected boundaries. Import and analysis lifetimes remain driver-established and PU-scoped. |

Introduce a context-tagged empirical-observation extension under this service's ownership. A compatible versioned companion profile is the initial design preference when it avoids changing legacy profile or mapped-WHIRL layouts. It is an Open64 FEEDBACK ingestion/storage extension, not an autonomous optimizer or a second authoritative telemetry system. A schema decision still requires an audit of readers, writers, consumers and deployment packaging before implementation.

The proposed observation contract contains schema version, observation ID, semantic and transformed region identities, source/operator and plan/variant identities, kernel-code digest, profile phase, metric and unit, raw samples or distribution summary, sample count/window, origin, confidence/uncertainty, workload/numerical binding, exact architecture, device/partition snapshot, software stack, power/clock/thermal/concurrency context, timestamp, producer version, dependencies and validity predicate. Measured data, derived facts, predictions and selected actions occupy distinct record kinds.

The reader validates schema, dimensions, units, finite values where required, count/window consistency and key joins before publishing an immutable accepted snapshot. Unknown is distinct from zero. Duplicate or conflicting records require an explicit merge policy; samples from incompatible contexts cannot be silently averaged. Timestamp alone neither proves freshness nor invalidates a profile whose full dependency fingerprint remains compatible.

Stable provenance must survive mapped-image reopen and legal transformations. WN pointers and raw WN_MAP indices are process-local references, not external profile keys. Reuse existing semantic/plan lineage and authenticated image identities; if they cannot identify a profile binding after reopen, reject the binding instead of inventing a new persisted WHIRL encoding. Common/com may define neutral records and structural access; the owning optimization phase and FEEDBACK service retain harvesting, interpretation and selection policy.

Source-level interface sketches for import, context validation, region queries, transformation notification and snapshot invalidation are requirements for the future implementation review. No new API name or command-line spelling is registered by this appendix. Names proposed in the source chat such as gpu-device/gpu-profile remain conceptual; implementations must follow Open64's phase-owned option registry and full-option propagation convention.

## PN7 Transformation propagation and invalidation

Frequency conservation and performance preservation are separate claims. Existing FEEDBACK update routines may preserve count information when their mathematical preconditions hold. A latency/traffic/clock observation is normally bound to exact code and execution conditions and does not become valid for transformed code simply because execution counts can be propagated.

| Change or operation | Frequency and semantic evidence | Physical performance evidence |
| --- | --- | --- |
| Clone or multiversion | Reuse established scaling/guard accounting; unknown split proportions remain guessed or unknown | Retain source observation as provenance; new variant needs an exact code/context match or a tagged prediction |
| Loop permutation or tiling | Apply reviewed LNO frequency updates and rebuild affected dependence/access facts | Invalidate latency, traffic, residency and wave binding for the changed variant; recompute launch-derived facts |
| Fusion, layout, staging or prefetch rewrite | Preserve numerical/effect contracts and update execution mappings | Recompute resources/events/cost; previous movement, power and timing samples are not measurements of the new code |
| High-to-Mid or Low lowering | Carry source/plan correspondence; verify the existing FEEDBACK lowering rules | Bind finalized code/resource hashes before interpreting prior kernel observations |
| Device, partition or cluster configuration change | ISA/numerical facts may remain valid only under their own capability contracts | Reject incompatible capacity/latency profiles; requery resources and recalibrate as required |
| Power policy, thermal or concurrency change | Does not itself alter source semantics | Revalidate operating-condition predicates; use unknown/conservative costs outside the measured envelope |
| Reader/tool/model version change | Preserve compatible source/count records under explicit schema rules | Invalidate affected derived/model snapshots without discarding reusable raw observations |

Each transformation declares preserved record kinds, remapping/scaling rules and invalidation dependencies. An absent claim means invalidation. Recompute dependent predictions after the owning transaction commits; a failed transaction restores its prior analysis/profile binding or leaves no published valid state. FEEDBACK::Verify continues checking its established consistency contract; an empirical-profile verifier additionally checks provenance, units and context without claiming that count consistency proves performance-model accuracy.

## PN8 Compiler phase integration and bounded replanning

At program setup, the driver selects the architecture and device-resource snapshots and admits compatible prior feedback. Before each active-PU consumer, bind only the applicable region evidence in that phase's scope. Domain gatekeeping and source numerical binding precede performance interpretation. No observation can replace missing correctness or target-capability proof.

| Phase or boundary | Use of physical feedback and required action |
| --- | --- |
| AI-P0/P1 | Import workload distributions, reuse/context evidence and confidence without changing tensor type identity or reconstructing domain meaning from timings |
| AI-P2 through P6 | Keep legal fusion/layout/placement/residency alternatives; account for profile applicability, communication effects and resource envelopes |
| AI-P7/P8 | Jointly evaluate tile/cluster, storage, multicast/prefetch, event and buffering alternatives with resource-dependent capacity and uncertainty |
| AI-P9 | Compare complete lowerable plans against the certified baseline; bind selected calibration/model snapshots and reasons; materialize selected High WHIRL and invalidate/recompute affected facts |
| High exit | Require current plan/proof generations and explicit profile compatibility. Missing feedback may reduce optimization, but never weakens correctness or structural exit requirements |
| Mid/Low/CG | Preserve selected schedule and lineage, realize memory and GPU mapping, obtain finalized resource/code evidence and validate capability; do not silently choose a different algorithm |
| AI-P10 | Generate compatible guarded variants with declared device/shape/context predicates; runtime selects only among certified packages |
| AI-P11 and next compile | Attribute observations, reject stale/malformed joins, update future models and report prediction error; never retroactively alter a completed decision's evidence |

If finalized register/SMEM/target information contradicts the selected estimate, the driver may request a bounded replan at the owning High stage. Identify the invalidated plan, restore a valid High checkpoint, rerun affected analyses and selection, then repeat the exit gates. Set a deterministic attempt/candidate budget and retain the certified fallback. This is controlled recompilation from a checkpoint, not arbitrary pass reordering or a backend silently changing a selected tile.

Schedule application should consume the selected plan plus architecture/device/profile snapshot IDs and current IR generation. It returns new loop/access/buffer correspondence, updated analysis bindings and a record of preserved/invalidated feedback. The lowering legality contract checks destination representation and target support independently of profile profitability. No MLIR opcode-interface/trait layer is needed; WHIRL opcode contracts and tensor descriptors remain the representation authority.

## PN9 Runtime selection safety and reproducibility

Runtime dispatch may choose only statically certified variants whose ISA, device/partition, shape, numerical, memory, launch and required synchronization predicates hold. Performance predicates guide preference among those compatible variants. If a preferred profile is missing or stale, choose the compatible conservative variant; an ISA/ABI mismatch is not repaired by falling back to another incompatible binary. A failure after launch follows the existing completion/drain protocol before any fallback or publication.

Recalibration runs outside latency-critical request execution unless a separately reviewed policy authorizes otherwise. Runtime telemetry is imported through the Open64 feedback adapter with sample budgets and explicit collection mode. Store device identity only in its authorized local/profile scope and support a stable local alias for reports. A deployment package records code/target/toolchain/numerical contracts and the profile applicability envelope, not a promise that every device of the same model has the same hidden topology.

## PN10 Implementation and acceptance requirements

The companion implementation increment defines PF0 through PF7. They extend the existing H/AIO/MMA/normalization queues without renumbering their completion records. All new implementation gates remain open; the source inventory in PN6 is documentation evidence only. The previously deferred PERF framework, dashboard and broad benchmark implementation remain deferred until explicitly resumed. This work specifies how later collectors and models must behave; it does not start those implementations.

Focused tests must distinguish designed/enabled/per-run/union resources, one-CTA-per-SM and multi-CTA occupancy, API bounds and measurements, bulk-wave and persistent kernels, zero/tail launches, grid/cluster validity, strict numerical contracts, metric units, unknowns, malformed keys and incompatible profiles. Synthetic wave arithmetic is independent of GPU execution and must not certify the hardware effect.

Add profile propagation tests around real existing FEEDBACK/LNO services, legacy profile reader compatibility, mapped-reopen identity binding, failure injection, atomic profile publication and trace-off/check-only invariants. Inspect before/after WHIRL when future code changes a schedule. Build/link audits apply when implementation or dependencies change; this documentation update invalidates no producer receipt and requires no compiler/GPU rerun.

Native calibration must establish correctness before timing, control kernel resources and collection conditions, separate unprofiled timing from replay/counter lanes, retain raw samples, compare held-out shapes/devices and report uncertainty and prediction errors. Retain both wins and losses. Report an unverified or unavailable lane explicitly. No particular percentage improvement or H100 capacity is an acceptance target across other products.

## PN11 Risks and controls

| Risk | Required control |
| --- | --- |
| ISA target mistaken for device calibration | Three descriptions with distinct validity keys; exact target/suffix and toolkit binding |
| 124-SM union mistaken for 120 simultaneous capacity | Separate per-run participation, union and concurrency fields plus collection method |
| Ordinary occupancy blamed on floorsweeping | Controlled resource class, API-bound record and independent calibrated estimate |
| Physical metrics placed in FB_FREQ or tensor types | Separate empirical record kinds under Open64 FEEDBACK ownership |
| Profile survives a layout/tile/code change incorrectly | Code/plan/context fingerprints, transformation-specific invalidation and exact-match checks |
| Saved bytes automatically converted into frequency gains | Calibrated plan-conditioned model, uncertainty and controlled DVFS comparison |
| Counter collection distorts latency or cache state | Distinct unprofiled timing and profiled lanes with explicit replay/cache policy |
| Early tile choice cannot survive finalized resources | Bounded driver-owned replanning from a retained High checkpoint |
| New telemetry becomes a competing optimizer | Reuse existing FEEDBACK ingestion, identity, phase and verification services; adapters collect evidence only |
| Unavailable hardware delays ordinary compilation | Valid no-profile path and certified compatible baseline; optional calibration capabilities |

## PN12 Sources and discussion continuity

- Source discussion: H100 Warp Relationship, https://chatgpt.com/c/6aeb7041-2934-83e8-989c-2df9c95f9536, including its physical-nonideality design amendment and subsequent clarification of feedback fields. It supplies requirements; technical facts are checked against the primary sources below.
- Kuruppu et al., Demystifying the Impact of Physical Non-Idealities on Kernel Performance, arXiv:2610.37466v2, November 3 2026: https://arxiv.org/html/2610.37466v2 . See Table 1 for per-iteration versus all-iteration participation and Sections 4 through 7 for the experimental scope.
- NVIDIA Hopper Architecture In-Depth, full GH100 and H100 SXM5/PCIe resource descriptions: https://developer.nvidia.com/blog/nvidia-hopper-architecture-in-depth/
- NVIDIA CUDA occupancy API, including cluster-size/active-cluster queries and achievable-occupancy caveat: https://docs.nvidia.com/cuda/cuda-runtime-api/group__CUDART__OCCUPANCY.html
- NVIDIA compiler driver, target and suffix definitions. Moving documentation inspected November 4 2026; pin the actual deployment toolkit: https://docs.nvidia.com/cuda/cuda-compiler-driver-nvcc/index.html
- NVIDIA NVML reference, device/configuration and telemetry adapter requirements: https://docs.nvidia.com/deploy/nvml-api/latest/
- NVIDIA Nsight Compute Profiling Guide, metric meaning, replay and collection effects: https://docs.nvidia.com/nsight-compute/ProfilingGuide/index.html
- Open64 local source anchors are listed in PN6. The inspected AI worktree revision is 1c978db8; read-only inspection did not modify those sources. Existing feedback services, rather than another compiler framework, define the implementation continuity requirement.
