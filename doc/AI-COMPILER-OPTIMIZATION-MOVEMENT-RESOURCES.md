# Movement-Aware GEMM Candidate Resources

Date: 2026-10-26. Scope: PU-local, -O3, fixed-shape contiguous F32 GEMM under
`gemm_plan_search=joint`. Baseline search remains unchanged. This extends
`AI-COMPILER-OPTIMIZATION-D-S1-T2-COMPARISON.md`; it does not add tile sizes,
numerical permissions, runtime variants or a binary WHIRL format.

## Analysis And Selection Boundary

P7 retains semantic ownership, effects, locality, geometry and thread-count
checks. In joint mode only, mark its tile resource ledger deferred: SMEM and
the staged register estimate describe a comparator, not the feasibility of
all possible movements. A proven deferred tile is an eligible topology, not
an executable plan. Do not select or lower it without the P8 whole-plan gate.

For each retained topology, construct S1, compact-layout D, and permitted T2
in stable order, including rejected movement siblings. P8 independently
computes and verifies the resources of each movement before profitability
selection. Capacity rejection must not remove another sibling. Preserve
semantic/alias rejection; resource deferral never makes unsafe inputs legal.
Retain the existing bounded census and best legal S1 compile-time fallback.
If no legal S1 exists, joint selection still fails rather than inventing an
unverified fallback or silently treating D as the synchronous staged plan.

## Resource Ledger

`BM`, `BN` and `BK` are the CTA's output-row, output-column and reduction
tile extents. A CTA is the cooperating CUDA thread block. `TM` and `TN` are
the output rows and columns owned by one thread; `TM*TN` is its accumulator
element count. A staging slot is one physical A/B tile pair in shared memory.

| Term | Definition |
| --- | --- |
| Register bytes/thread | Provisional F32 value-storage estimate, not measured native registers. D uses `4*(TM*TN+2)` for accumulators and current A/B scalar operands. S1/T2 retain the conservative staged estimate `4*(TM*TN+TM+TN)`. |
| Register capacity/thread | Existing target register-tier byte capacity. This is a coarse planning gate, not proof of spill-free allocation. |
| Active SMEM bytes | D: zero. S1: one physical A/B stage including layout padding. T2: two physical stages plus the emitted 16-byte ready-barrier array. |
| SMEM limit | Minimum of target per-CTA capacity and the current 48 KiB static-emission envelope. No dynamic-SMEM opt-in is assumed. |
| Threads/CTA | Output ownership geometry, checked against the existing 1024-thread envelope. |

CommonFetchPlanIR stores the generic byte/count ledger and its version;
common verification checks internal consistency and proven-plan capacity
bounds without selecting a target or optimization. VHO owns formulas,
target/lowering limits, rejection classification and recomputation. Device
construction consumes the verified selected plan; `whirl2c` does not plan.
Records remain runtime-only. No frontend dependency or cross-PU analysis is
introduced. Planning adds overlays only; executable mutation occurs after
selection through the existing transactional publication path.

Address/index/control registers, allocation granularity, actual compiler
allocation/spills, resident CTAs/warps and grid waves are not predicted by
this ledger. They remain the next resource-model refinement. Do not claim
occupancy or native performance from these estimates.

## Trace And Report Contract

Keep AI_OPT v7 and introduce joint model 3 for mandatory movement-resource
evidence. Cost formulas are unchanged from model 2. Emit one compact row for
every joint candidate, with input line last:

```text
JOINT_RESOURCE joint_id register_bytes register_capacity active_smem_bytes smem_limit threads thread_limit source_line
```

The tool independently checks formulas, ledger coverage, capacity-based
legality/rejection, complete costs, deterministic winner and S1 fallback.
`physical-plan.log` summarizes each sibling's resources and rejection;
`optimization-summary.log` includes the selected resource ledger and model
limitations. Keep existing model-1/model-2 trace compatibility.

## Required Verification

- Constrained synthetic topology: staged SMEM rejection with legal zero-SMEM
  D; register budget between the D and staged estimates; independent D
  register rejection; T2's additional slot/barriers; unsafe ownership and
  invalid thread geometry must still reject before movement selection.
- Common malformed-record negatives and VHO recomputation negatives.
- Trace negatives for missing/duplicate/corrupt resource rows, false legality
  and missing candidates. Baseline, option/level boundaries and deterministic
  joint selection remain covered by the existing regression suites.
- Preserve raw `.t`, before/after WHIRL, generated `.cu`, phase/aggregate
  logs and receipts. No GPU acceptance gate is closed by these checks.

## Compiler Checkpoint

All 77 Python unit tests pass, along with the D/S1/T2 integration suite,
joint-search option/level controls and baseline parity, executable P8
synchronous/TMA ordering and host simulations, and the linked AIO-10
Hopper/Blackwell/CPU/unsafe-distance/unchanged-image suite. The native producer
rejects eleven malformed common records and additionally checks constrained
movement budgets, missing/altered resource proof, four safety negatives and
two invalid P7 deferral controls. Host numerical errors remain zero.

The existing 3x5x7 example still selects D among 19 pairs at relative cost 4;
128x128x144 still selects S1 at 9279. No new production tile family or measured
speedup is claimed. The source-review harness stops the compiler driver at
`-S`; generated CUDA is separately host-compiled and simulated. Native CUDA,
PTX/SASS, actual register/spill allocation and GPU acceptance remain open.

Producer checkpoint: `ef7cebce`. Immutable evidence (1007 checksummed files,
including raw traces, before/after `.B/.T`, diffs, `.cu`, audited reports,
commands, build/restoration logs, source snapshots and binary fingerprints):
`/Users/shinmingliu/open64/artifacts/review/ai-optimization/m5/movement-resources/sha256-792700b3a528cd2cf07ac04097e0b437f42d0ca6b27a1f78d4f458bd1de10872/`.
The retained tail-case device image was independently reopened from that
read-only receipt with `ir_b2a -st -src` and reproduced the retained dump;
replaying its `.t` reproduced `optimization-summary.log` exactly.
