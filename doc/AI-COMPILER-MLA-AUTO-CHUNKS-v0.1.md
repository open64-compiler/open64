# MLA automatic compile-time chunk selection v0.1

The existing fused register MLA path gains an optional offline selection pass.
It chooses a static chunk width from context shape and an explicit GPU/partition
resource envelope before native WHIRL construction. AOT remains baseline.
Manual v1–v5 recipes and explicit stream-attention widths retain their behavior.

## Invocation and scope

    tensor_kernel_lower auto-attention input.json selected.json \
      --gpu-profile gpu.json --allow-reassociate
    tensor_kernel_lower device selected.json device.before.B device.after.B
    tensor_kernel_lower audit device.after.B selected.json launch.json
    tensor_kernel_lower schedule-source selected.json optimized.cu

The input must already prove the closed absorbed-attention topology at O3 with
the existing fused/cooperative numerical permission. The pass enables register
accumulation and emits v6. The focused standalone driver owns this integration;
this milestone does not claim an additional opencc/be_driver option or runtime
device-discovery integration.

The VHO pass consumes only the active source PU's copied facts. It does not
traverse other PUs, allocate native tables during search, invoke nvcc, time a
kernel, probe a device or require JIT compilation. Unknown dynamic dimensions
remain outside this static admitted topology.

Architecture and device resources are separate inputs. sm_90 does not imply
a particular number of enabled SMs, SKU, partition or carveout. The initial
schema accepts sm_90/sm_90a identities through the existing hardware-description
registry. It never borrows that registry's synthetic SM-count/timing defaults.

## GPU profile

The example tests/mla_gpu_profile_sm90_review.json is an explicitly supplied
132-SM review estimate, not a discovered or certified physical device.
Its architecture is sm_90, register estimate is 48 32-bit registers/thread,
requested local residency is two CTAs/SM, and partial scratch budget is 1 MiB.
Its supplied shared planning budget is 64 KiB/SM. No universal H100 SM-count
rule is introduced.

open64.mla-gpu-profile.v1 requires exactly:
schema, id, architecture, provenance, sms, registers_per_thread_estimate,
target_ctas_per_sm, scratch_budget_bytes and resources. The provenance is
provided-capacities-estimated-registers. resources contains all twelve fields of
the existing DSL_HARDWARE_RESOURCE_RECORD:

- max_blocks, max_warps, max_threads, max_threads_per_cta;
- registers_per_sm, max_registers_per_thread, register_quantum,
  register_warp_quantum;
- shared_capacity, shared_budget, shared_quantum, shared_reserved.

The selector reuses DSL_hardware_resources_valid and DSL_gemm_occupancy.
It does not duplicate their allocation-rounding or capacity formulas.
The supplied register estimate is used uniformly for all selected stages,
including projections, to check the planning envelope. It is not a measured
native allocation guarantee; ptxas resources and runtime occupancy remain
separate evidence. Profiles require explicit positive capacities, bounded
integers and a known architecture. Missing fields, coerced strings, duplicate
keys, unknown schema/architecture and invalid capacities reject before output.

## Deterministic coverage-v1 policy

Candidate direct attention is considered when L<=4096. Split candidates have
widths 128,256,512,1024,2048,4096 below L. For width W:

    C = ceil(L/W)
    local_CTAs = B*H*C
    local_shared = 4*(round_up(W,128)+8)
    merge_shared = C>1 ? 4*(round_up(C,128)+8) : 0
    partial_scratch = C>1 ? 4*C*B*H*(R+2) : 0

The direct case uses W=L, C=1 and stores selected split_width=0.
Each candidate must pass the existing native schedule/capacity admission,
all-stage estimated resource residency, and the partial-scratch budget.
The merge's shared footprint and every projection/copy stage are checked,
not just local attention. Native capacity and expansion bounds remain intact,
including the 4096-chunk merge limit and physical allocation limits.

The occupancy helper rounds registers and shared allocations, includes reserved
shared memory, and applies block, warp, thread, register and shared limits.
If estimated local resident CTAs/SM is q, define

    goal = sms * min(target_ctas_per_sm, q)
    coverage = min(local_CTAs, goal) / goal

Rank feasible candidates by greatest coverage, then fewest chunks, then stable
width order. Fractions compare by bounded integer cross multiplication.
Thus candidates meeting coverage prefer the smallest tuple/merge overhead;
when none meet it, select the highest coverage rather than rejecting useful
underfilled work. This is a coverage/resource heuristic, not a calibrated
instruction-delay/stall cost model or proof of minimum latency.

If L<=256 and direct attention fits, preserve it to avoid short-row split/merge
overhead. This threshold and candidate set are explicit versioned policy.
A direct plan that does not fit may still be replaced by a legal split.
If no candidate fits the native/GPU/scratch envelope, fail without a partial
recipe; do not silently ignore missing resources or numerical permission.

For the review B=1,H=3,L=8193,R=16 fixture, the 132-SM/two-CTA envelope selects
128: its 195 CTAs provide greater coverage than wider chunks. For the same
shape and an 8-SM envelope, width 1024 provides 27 CTAs, reaches the 16-CTA goal,
and avoids the extra tuples of narrower candidates. Short L=127 remains direct.

Sensitivity tests also use B=2,H=2,R=5,L=8193:
132 SMs -> width 128; 8 SMs -> 2048; reducing the supplied register file to
8192 registers -> 4096; limiting shared budget to 1024 bytes -> 128;
limiting partial scratch to 4096 bytes -> 256. These are explicit synthetic
envelope experiments, not real device benchmark claims.

## Sealing, native audit and explanatory comments

v6 adds one closed chunk_selection record to the v5 recipe. It contains:
schema, policy, complete profile, selected_width, short_direct and the full
candidate ledger. Each row records width, chunks, CTAs, local/merge shared
bytes, scratch bytes, estimated residency, coverage numerator/goal, eligibility
and rejection reason. Recompute the complete record during admission and
require canonical equality; resealing a forged winner/ledger cannot authorize it.

The source-scope selection is carried into the selected physical plan before
synthetic storage is appended. Comment generation reads that carried result;
it does not rerun source admission on augmented physical bindings or choose
another width. Native kernel symbols seal the complete recipe. Each kernel
also carries audited tensor.kernel.chunk_selection metadata. The manifest
exports the same complete selection record and each kernel's selection identity.

The canonical overview prints GPU/profile provenance, goal/ranking equations,
all candidate rows and selected width. Existing ownership, five-barrier,
tail, stable [m_c,d_c,z_c] merge and numerical explanations remain. The same
payload becomes real WHIRL COMMENT nodes and appears in readable CUDA and
normal whirl2c mapped CUDA. No WHIRL opcode, TY-kind, binary image/ELF layout
or be.so dependency is added. The policy reuses the existing occupancy service;
CUDA emitters neither search nor infer resource policy.

## Override, invalidation and acceptance

    tensor_kernel_lower stream-attention selected.json manual.json \
      --split-width 256 --allow-reassociate

An explicit width clears automatic policy/profile and emits the established
manual v5 contract. It does not retain a misleading stale automatic ledger.
auto-attention may be rerun with another supplied envelope.

Changing shape, topology, mask semantics, numerical permission, profile,
resource estimate, scratch budget or policy invalidates the choice and all
downstream fingerprints, native kernels, launch manifests, resources and
COMMENT payloads. Re-select, materialize and audit together. Register-estimate
changes may affect the choice even with identical context length. Measured
post-backend resources can refine a later supplied profile, preserving their
separate provenance; they are not retroactively implied by a static estimate.

Validation includes deterministic/resource-sensitive winners, short direct
behavior, two split widths executing native WHIRL-derived CUDA on synchronized
host workers against expanded FP64 references, manual override, O0 rejection,
invalid/infeasible profiles, forged ledger/winner rejection, native metadata
mutation audits, WHIRL/comment equality and prior fused/cooperative regressions.
Reference tolerances stay rtol 2e-5, atol 2e-6 with masking/infinity requirements.
Retain both 132-SM and 8-SM short/long scenarios, native before/after .B/.T,
raw diffs, comments-only checkpoints, exact tools, profile files and nvcc
resource logs. Run hardware/reference and sanitizer/performance acceptance
when a usable target GPU is available. No GPU speedup is inferred from coverage.

Primary owner, successor, independent reviewer and approving lead remain TBD
as requested; formal transfer acceptance remains open.
