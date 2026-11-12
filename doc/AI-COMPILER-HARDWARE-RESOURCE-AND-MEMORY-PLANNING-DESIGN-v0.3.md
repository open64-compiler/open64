# Hardware resource and memory planning v0.3

2026-11-12 | Concrete device selection and architecture-only safeguards

This revision extends [v0.2](AI-COMPILER-HARDWARE-RESOURCE-AND-MEMORY-PLANNING-DESIGN-v0.2.md)
by reference. It makes device selection an H1 requirement under the
[implementation plan v1.1](AI-COMPILER-H20-MEMORY-HIERARCHY-IMPLEMENTATION-PLAN-v1.1.md).
The native [opcode and tensor-trait contract](AI-COMPILER-WHIRL-OPCODE-AND-TENSOR-TRAIT-CONTRACT-v1.0.md),
two-phase search, numerical permissions, Q3 bindings, COMMENT explanations,
scope and fallback remain authoritative. This document specifies future work;
it does not implement an option, modify native WHIRL or qualify a device.

## 1. Existing limitation and required correction

The current architecture lookup cannot distinguish H20 from H200 when both
requests supply only `gpu_arch=sm_90`. The registered option is in
[config_dsl.cxx](../osprey/common/util/config_dsl.cxx), and
[dsl_lower.cxx](../osprey/be/vho/dsl_lower.cxx) resolves its exact architecture
name through the common hardware-description service. The selected
`sm90_estimated_v2` row in
[dsl_memory_hierarchy.cxx](../osprey/common/targ_info/access/dsl_memory_hierarchy.cxx)
explicitly contains synthetic quantities, including an SM count of four.
It is neither an H20 nor an H200 hardware specification. Separate callers may
already supply bounded detached resource inputs; the architecture name alone
does not provide them.

Thus a shared architecture-only request can use the same planning envelope
for different boards. This is a device-selection gap, not evidence that their
physical resources or performance are equal. The prior design separated ISA,
resource and calibration identities but did not make their user selection a
concrete H1 delivery gate. This revision closes that planning omission.

## 2. H1 option contract

H1 must register the following new backend option through the existing DSL
option group and complete Open64 driver propagation:

~~~text
-Wb,-DSL:gpu_device=h20_96gb
-Wb,-DSL:gpu_device=h20_141gb
~~~

These are exact device keys. `gpu_arch` retains its current exact `sm_*`
meaning; `m90` is not an alias for `sm_90`. Examples of the intended H1 CLI are:

~~~sh
# Planned H1 syntax; gpu_device is not implemented at this revision.
openpy -O3 -Wb,-DSL:gpu_arch=sm_90 -Wb,-DSL:gpu_device=h20_96gb model.py
openpy -O3 -Wb,-DSL:gpu_arch=sm_90 -Wb,-DSL:gpu_device=h20_141gb model.py
~~~

| Input | Meaning and rule |
| --- | --- |
| gpu_arch | Exact ISA compatibility and instruction legality; an explicit incompatible architecture/device pair is rejected |
| gpu_device | Exact board-variant identity and lookup of its separately versioned resource defaults; it never supplies measured server performance by itself |
| Resource/partition input | Actual visible SMs, per-SM/per-CTA limits, L2, total visible HBM and topology, with provenance and unknowns; partition identity refines the board identity |
| Execution profile input | Versioned measured or explicitly estimated rates, latency and conditions bound to the selected device/partition and model |
| Workload memory budget | Explicit usable allocation budget and reservations for this program/serving context, separate from physical HBM capacity |

The device key resolves a reviewed baseline ISA when gpu_arch is omitted;
an explicitly supplied gpu_arch must be compatible. Neither a device name nor
a compute capability grants an architecture-specific instruction mechanism
without its implementation and qualification. H1 does not introduce an H200
device option: such support requires its own registry entry and qualification.

Do not identify a board by memory size alone. The 96gb/141gb tokens are stable
variant names, not a claim that all nominal bytes are allocatable or that a
reported byte count must equal an integer number of GiB. Model/variant identity,
partition attributes and physical quantities require independently validated
input. Do not populate missing H20 fields from H200, H100 or a larger-memory
H20 variant. Unestablished fields remain unknown.

## 3. Resolve once and bind all consumers

The driver/config owner resolves ISA, device, resource/partition, execution
profile and workload-budget inputs once before candidate exploration. Common
targ_info owns policy-free descriptions, exact lookup and structural checks;
owning phases validate applicability, legality and profitability. External
inventory/calibration supplies evidence through the versioned import contract.
No automatic CUDA/NVML probing or new library dependency enters be.so.

The immutable resolved context contains independent ISA, device and resource
revisions, partition identity, profile/model provenance and digests, supported
materializer/toolchain bindings, and budget/guard bindings. It is owned data,
not live device state, a tensor trait or a mutable global table row.

| Consumer | Required use of the resolved context |
| --- | --- |
| Domain/common preparation and snapshot capture | Preserve device/resource/contract identity without premature target-kernel selection; capture in the active driver-established scope |
| GEMM, MLA, LayerNorm and Reduction planners | Query shared resolved fields; avoid private board-name conditionals and raw architecture-to-rate shortcuts |
| Candidate cache, budgets and convergence | Key on every device/resource/profile/budget input affecting admission or cost; distinguish evidence identity from code compatibility |
| Selected High application and exit | Revalidate bindings, storage/lifetime/resource/numerical obligations and required guards before commit; retain rollback and analysis rebuild |
| Mid/Low, PTX and whirl2c | Realize and print fixed selected semantics; preserve required resources and compatibility constraints without reselecting the plan |
| Host/serving contract and qualification | Carry independently checkable actual-resource and usable-budget requirements, Q3 publication/lifetime obligations and precise evidence bindings |

A changed device, partition, resource limit or budget invalidates affected
admission and plan/guard bindings. A calibration-only change invalidates costs,
rankings and stopping bounds; unchanged code-legality facts can survive only
when their full dependencies remain valid. Source mutation still follows the
existing activation/mutation and analysis-epoch rules.

## 4. How resources change optimization

Capacity feeds aggregate live-storage feasibility: weights, resident KV cache,
outputs, scratch, optional mirrors, concurrent allocations and reservations.
Bandwidth and service data feed movement/reuse profitability. SM count,
RF/SMEM capacities, per-block limits and cache behavior feed coverage, tile,
group, layout and buffering decisions. Keep these quantities independent.

A 141 GB capacity label does not justify larger CTA tiles or SMEM buffers.
If two valid profiles differ only in HBM budget, local kernels may legitimately
remain identical while residency, scratch or recomputation plans differ.
Different service/resource data can justify different schedules. Memory-budget
evidence must remain conservative and subject to existing runtime checks;
observed free memory during compilation is not a future allocation guarantee.
This revision does not design the deferred AOT variant-selection mechanism or
authorize a new serving batch-size/page-size policy.

## 5. Architecture-only behavior and fallback

| Situation | Required behavior |
| --- | --- |
| gpu_device omitted | Preserve the current generic architecture route and old review artifacts; label its quantities as architecture-only/synthetic or explicitly supplied estimates, with no H20/H200 identity or qualification claim |
| Exact device selected with incomplete resource/cost facts | Keep unknowns explicit; a device-specific candidate requiring missing facts is ineligible. Use an independently admitted compatible semantic fallback or diagnose |
| Unknown device, incompatible ISA or conflicting device/profile identity | Diagnose before search/output publication; do not guess, alias a nearby board or silently drop the explicit request |
| H20 request with H200 measurements | Reject applicability of that profile; shared ISA or nominal capacity cannot transfer calibration or performance qualification |
| Selected plan later fails resource/budget guards | Follow the existing compatible fallback/refusal contract and preserve prior accepted output; no substitution of a synthetic row as device evidence |

A validated generic kernel may be code-compatible with multiple devices.
That fact does not make their timing, memory budget or qualification evidence
interchangeable. Performance reports must print both requested and resolved
identities, provenance, unknowns and fallback reasons. Architecture-only
estimates may support their declared generic experiment; they cannot certify
or be reported as an H20-specific or H200-specific optimized result.

An external test may still execute a generic build on an identified H20 and
report actual correctness or timing for that product and those conditions.
Such evidence does not retroactively calibrate the synthetic compiler inputs
or establish that the compiler selected an H20-specific optimum. Existing
generic baseline qualification therefore remains available.

Two-phase search is unchanged: immutable resolution precedes read-only
estimation, and only the eligible selected plan is atomically materialized and
qualified. No candidate iteration probes devices, compiles kernels or performs
timing/correctness trials. Actual device acceptance remains an external gate.
