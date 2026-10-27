# FP32 GEMM Occupancy-Aware Analytical Planning

Version: v0.1. Scope: O3 PU-local joint D/S1/T2 planning after PR #193.
Status: bounded compiler-only implementation accepted; native gates remain open.

## Definitions And Ownership

| Term | Definition |
| --- | --- |
| Logical register demand | Existing live F32 value estimate, not ptxas allocation; excludes unknown address/control temporaries, spills and allocation reuse. |
| Allocation quantum | Table-owned planning rounding unit; not an instruction or executable WHIRL attribute. |
| Resident CTA estimate R | Minimum of block, warp, thread, register and configured SMEM capacity quotients. This is NOT certified native occupancy. |
| Residency round | Up to S*R CTAs, distributed as evenly as possible across S modeled SMs. Actual CUDA placement is unknown. |
| q | Maximum CTAs on one SM in a round: ceil(active/S). |
| Full/tail round | Full uses S*R CTAs; tail uses the nonzero remainder. |
| Grid underfill | Empty modeled CTA slots in the last round; separately report active SMs and peak active warps. |
| Legacy counterfactual | Same traffic, work, rates and latency evaluated with one CTA/SM; not a timing measurement. |

Common/com owns immutable, policy-free resource records and structural checks.
VHO derives demand, residency and schedule costs within the active PU. No
binary WHIRL layout/opcode changes. The selected lowered WHIRL is the sole
executable authority; whirl2c emits it without selecting a plan.

## Hardware Facts Versus Planning Assumptions

The [Hopper tuning guide](https://docs.nvidia.com/cuda/hopper-tuning-guide/index.html)
documents 64 warps, 65536 32-bit registers, 32 blocks, 228 KiB maximum SMEM per
SM, 255 registers/thread, and a 1 KiB SMEM reservation per block. The
[Blackwell guide](https://docs.nvidia.com/cuda/blackwell-tuning-guide/index.html)
documents the same capacities for CC10.0; this does not cover CC12.x.
The [compute capability table](https://docs.nvidia.com/cuda/cuda-programming-guide/05-appendices/compute-capabilities.html)
defines thread/block limits. Exact sm_90/sm_90a gates remain; Blackwell rows
remain planning-only, not newly supported emission.

Rev2 resource envelope uses documented capacity maxima, but a conservative
64 KiB configured SMEM budget (a supported carveout, NOT a promised runtime
setting). Register rounding 256 registers/warp, four-warp register allocation
groups and 128-byte SMEM rounding are explicit ESTIMATED planning assumptions;
they are not claimed as verified Hopper/Blackwell allocation contracts. Native
occupancy APIs/ptxas must replace/check them before certification. Unmodeled
register partition constraints and launch carveout are additional uncertainty.

| Resource payload fields | Initial value | Provenance / role |
| --- | --- | --- |
| max_blocks, max_warps, max_threads | 32, 64, 2048 | Documented architecture maxima; threads=warps*32. |
| max_threads_per_cta | 1024 | Architectural block-thread limit. |
| registers_per_sm, max_registers_per_thread | 65536 words, 255 words | Documented 32-bit-register maxima. |
| register_quantum, register_warp_quantum | 256 words/warp, 4 warps | Estimated allocation assumptions, not verified native granularity. |
| shared_capacity | 233472 bytes | Documented maximum; not configured availability. |
| shared_budget | 65536 bytes | Conservative configured-envelope assumption; runtime carveout unverified. |
| shared_quantum | 128 bytes | Estimated allocation rounding. |
| shared_reserved | 1024 bytes/CTA | Documented CUDA reservation. |

Existing four-SM performance envelope and reference-cycle rates remain synthetic,
not a SKU, physical clock or NVIDIA performance specification. No new private
delay/rate constants are allowed. Unknown/zero denominators reject evaluation.

## Algorithm

```text
warps = ceil(threads / lanes)
register_warps = round_up(warps, register_warp_quantum)
registers_per_warp = round_up(ceil(logical_register_bytes/4)*lanes,
                             register_quantum)
registers_per_CTA = register_warps * registers_per_warp
shared_per_CTA = round_up(active_shared_bytes + reserved_shared_bytes,
                          shared_quantum)
limits = [max_blocks, floor(max_warps/warps), floor(max_threads/threads),
          floor(registers_per_SM/registers_per_CTA),
          floor(configured_shared_bytes/shared_per_CTA)]
set thread/register quotient to zero if the per-CTA/per-thread cap is exceeded
if shared_per_CTA == 0, shared quotient is max_blocks (no SMEM constraint)
R = min(limits); report ALL ties as limiting-resource bits
R == 0: reject this candidate, preserve sibling plans; legacy cost is
    diagnostic only and cannot authorize selection
arithmetic invalid: reject evaluation; leave outputs untouched
capacity = S*R; full = grid/capacity; tail = grid%capacity
for each nonempty full/tail class:
    q = ceil(active/S)
    # Never multiply bandwidth or per-SM throughput by residency!
    consumer = single_CTA_consumer*q
    copy = single_CTA_copy*q
    wait/release/issue/publication service = original service*q
    aggregate_input_service = ceil(single_CTA_bytes*active/device_bandwidth)
    exposed D/S1 startup = ceil(latency/min(warps*q,hiding_warp_limit))
    run existing two-slot T2 recurrence with aggregate services,
        or synchronous D/S1 schedule; charge output bytes and one drain
    accumulate complete compute + unhidden movement + sync + issue
compare all complete legal plans; preserve synchronous fallback
materialize only selected plan into WHIRL
```

This is an analytical lockstep approximation, not a proof of CUDA scheduling,
latency hiding, or an upper/lower bound on native duration. q scales shared
per-SM service work, not throughput. Input bytes are serialized through ONE
device-wide bandwidth. No free concurrent TMA engine, extra producer warp,
cache credit or native occupancy guarantee is invented. R informs planning,
not a guarantee that native code is legal without spilling. Resource-model
failure cannot authorize selection; existing synchronous fallback remains.

## Trace Contract

AI_OPT schema7 remains positional; joint cost model6 explicitly versions the
new semantics. Models3/4/5 remain historical parser inputs. HW_DESC revision2
and HW_RESOURCE include resource schema1, mixed_estimated provenance and all
rounding/capacity parameters. JOINT_OCCUPANCY includes rounded demands, each
capacity quotient, R, resident warps, full/tail rounds, unused tail slots,
active tail SMs, legacy cost and final source line. JOINT_READINESS carries
model6 round costs. Reports audit every field independently, full census,
cost components, selected and fallback plans; emit old/new winners even if
unchanged. All logs begin 'What it summarizes'.

The report also derives D-specific issued-sector traffic/Roofline diagnostics
from the audited DIRECT_ACCESS rows: useful FLOPs, total issued input/output
bytes, ideal compute service, ideal byte service and their maximum. Intensity
is FLOPs/issued bytes. These floors use the same shared model rates and do
not add another cost or assert HBM availability/cache hits. Capacity warps and
grid-limited peak warps are explicitly distinguished in the summary.

## Acceptance And Remaining Work

Compiler checkpoint `b19e22e0` on `codex/ai-gemm-occupancy`, based on merged
develop `0ef7845d` (PR193). Design-first commit: `cb47643b`. No push or PR.

Fresh acceptance: 109 Python tests (104 baseline plus five new), 2340 legacy
T2 and 5535 new literal D/S1/T2 oracle cases, Linux builds of both standalone
contracts, affected backend/IR-tool/lw_inline rebuilds, linked resource gates,
fifteen compiler/source configurations, legacy D/S1/T2 and controls/baseline/
trace-off checks, malformed traces, exact target/option rejection and backend
symbol boundaries. All phase logs retain the summary-first header. The
impact selector's unknown-path FHE escalation was reviewed: changes are
runtime resource records and AI-local cost/trace consumers, not FHE semantic
materializers, image transactions or persisted layouts. Focused tests replace
an unrelated whole-model ResNet rerun; no FHE full-model pass is claimed.

Historical model4/model5 raw traces are reused from authenticated M7/M8
receipts, with fresh independent parsing/report audits. Current compiler
artifacts are freshly produced, not inherited certification.

### Actual Ranking Results

All fourteen readiness configurations retain their legacy one-CTA/SM winner.
The fifteenth configuration exercises unchanged relative model3. Do not imply
a selection improvement or GPU speedup from this result.

| Shape M x N x K | Old / new winner | Old / new cost (estimated reference cycles) |
| --- | --- | --- |
| 1 x 1 x 1 | D #2 / D #2 | 192 / 192 |
| 1 x 1 x 17 | D #2 / D #2 | 1648 / 1648 |
| 3 x 5 x 7 | D #2 / D #2 | 856 / 856 |
| 65 x 67 x 17 | S1 #1 / S1 #1 | 8061 / 8061 |
| 128 x 128 x 144 | T2 #3 / T2 #3 | 31336 / 31336 |
| 512 x 512 x 32 | S1 #7 / S1 #7 | 168684 / 168684 |
| 513 x 257 x 17 | S1 #7 / S1 #7 | 118929 / 118929 |

Seven examined candidate costs change in each large-grid case. Examples for
512x512x32: S1 #1 269664 -> 267618, D #2 2684992 -> 2671240,
T2 #3 241312 -> 239312. They do not beat S1 #7. Small grids use at most one
CTA/SM, so no extra residency credit appears. The selected large-grid S1 #7
is SMEM-limited to R=1 (33792 rounded bytes/CTA in a 65536-byte envelope), so
its cost also remains unchanged. For 513x257x17 its grid has three full
rounds plus a three-CTA tail and one unused slot. Staged T2 #3 estimates R=3
but its four-CTA grid has only four peak active warps per SM, not its
twelve-warp residency capacity.

### Durable Evidence

1384 checksum-verified files, published by no-replace atomic promotion:

```text
/Users/shinmingliu/open64/artifacts/review/ai-optimization/m9/occupancy/sha256-663139e18ca2cdd6aba88ea9c8b056958c4b3aa46dd9f2866c897b8e15a154b9/
```

Reviewer entry points beneath that receipt:

- `acceptance/rankings.json`: all changed/unchanged old/new winners and costs.
- `acceptance/occupancy/staged/gemm.t`: raw positional model6 trace.
- `acceptance/occupancy/staged/reports/optimization-summary.log`: concise selection.
- `acceptance/occupancy/staged/reports/phases/tile.log`: resource rounding/limits and D issued-traffic diagnostics.
- `acceptance/occupancy/staged/reports/phases/physical-plan.log`: complete costs, fallback and actual WHIRL actions.
- `acceptance/occupancy/staged/gemm.whirl.cu`: selected T2 CUDA from WHIRL.
- `acceptance/occupancy/large-tail/gemm.whirl.cu`: selected S1 CUDA.
- `acceptance/occupancy/direct/gemm.whirl.cu`: selected D CUDA.
- Each case retains `gemm.before.B/.T`, `gemm.after.B/.T` and `gemm.before-after.diff`.
- `acceptance/commands.json`, `acceptance/logs/`, `acceptance/source/` and receipts/checksums retain reproduction inputs.

Post-promotion: all 1384 checksums match. D, S1 and T2 before/after images
were reopened from the final receipt in six separate `ir_b2a -st -src`
processes; all text matches. Three raw-trace report replays reproduce the
summary, aggregate, tile and physical-plan logs byte-for-byte. The immutable
receipt was mounted read-only. Disposable post-check command evidence is at
`artifacts/ai_optimization/m9-occupancy/durable-reopen-001/receipt.json` in
the implementation checkout. Earlier `run-001`, `run-002`, `accepted-001`
are development staging, not accepted receipts; only `accepted-002` was promoted.

### Residual Scope

Native register allocation, partition-aware occupancy, runtime carveout,
cache/queue calibration, dynamic SMEM, launch integration and GPU performance
certification remain open. No fusion, low-precision or new movement family.
Next bounded compiler packages remain separately gated: optional actual vector/
async-copy movement, selected D/T2 host-provider launch integration, and closure
of the supported-subset review matrix. This resource milestone does not claim
completion of every optional multilevel-tiling extension.
