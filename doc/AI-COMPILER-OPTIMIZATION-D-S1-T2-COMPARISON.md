# D/S1/T2 GEMM Comparison

Date: 2026-10-25. Compiler-side extension of the model-1 joint-search
checkpoint in `AI-COMPILER-OPTIMIZATION-JOINT-GEMM-PLAN-SEARCH.md`.
Scope: the existing PU-local, fixed-shape contiguous F32 P7 family. No new
binary WHIRL layout, opcode, numerical permission or CUDA language extension.

## Plans And Selection

| Kind | A/B movement and residency | Active storage and synchronization |
| --- | --- | --- |
| D | Guarded global loads into registers; register accumulation | Zero A/B SMEM, zero staging barriers, zero async overlap credit |
| S1 | Existing one-slot cooperative synchronous A/B staging | One physical staging allocation, publication/release barriers |
| T2 | Existing exact compact two-slot TMA | Two physical allocations plus ready barriers, verified P8 ordering |

`gemm_plan_search=joint` now uses model 2 and at most 48 pairs from 16 tile
records. Each proven nonbaseline tile contributes S1, a D sibling when its
A/B layout is compact, and T2 when permitted by `cuda_tma_prefetch`. D is
not duplicated for padded-A or XOR-B siblings, whose staging layouts do not
change direct execution. Enumeration is stable: S1, optional D, optional T2.
The winner minimizes complete legal cost, then enumeration ordinal. The
fallback is the best legal **S1** pair even when D wins; D is not silently
substituted for the staged fallback.

The tile's SMEM fields describe the hypothetical staged alternative; the
selected movement plan determines active physical allocation. A D record
explicitly overrides allocation to zero. Reports show selected A/B storage
as zero rather than displaying the unused staged ledger as actual storage.

This increment does not resurrect P7 seeds already rejected for resources,
generate new tile dimensions, or produce a runtime fallback dispatcher. The
inherited P7 register estimate remains conservative and uncalibrated.

## Direct WHIRL Algorithm

For each CTA, use the selected output tile and thread ownership map:

```text
initialize each thread's TM*TN accumulator
for base = 0 .. K step BK:
    for q = 0 .. BK:
        if base + q < K:
            for i = 0 .. TM:
                row = CTA_row + thread_row + i
                av = row < M ? A[row, base+q] : 0
                for j = 0 .. TN:
                    col = CTA_col + thread_col + j
                    if row < M and col < N:
                        acc[i,j] += av * B[base+q,col]
guard and store each valid output element
```

A is loaded once per `q/i` and reused across `j`. B is loaded per `q/i/j`;
no additional B-register reuse is claimed. The K guard encloses operand
loads; row/column guards prevent invalid addresses. Reduction order for
valid outputs remains increasing K, with ordinary F32 multiply/add WHIRL.

VHO constructs ordinary `DO_LOOP`, `IF`, `ILOAD`, `ISTORE` and scalar
expressions. It creates no A/B shared symbols, no synchronization symbol and
no movement-operation calls for D. Structural preflight and the existing I4
index proof run before driver-owned atomic image publication. `whirl2c -cuda`
only emits the selected device PU; there is no independent D CUDA template.

## Cost And Traffic

S1/T2 keep the explicit whole-GEMM model-1 formulas. D uses:

```text
compute = max(1, FLOPs/1024)
memory_unhidden = max(1, (A_direct_sector_bytes+B_direct_sector_bytes)/1024)
                  + C_store_sector_bytes/1024
communication_unhidden = sync = setup = runtime_selection = pressure = 0
total = compute + memory_unhidden
```

All quantities remain relative low-confidence scores. D has no SMEM bank,
SMEM address, staged-load-issue or capacity-pressure charge. It does not
receive a cache-hit discount or overlap credit.

The planner counts distinct 32-byte sectors **for each warp instruction**
under D's actual register map. It accounts for A broadcast, repeated B issues
across `i`, predication, CTA replication, full/tail classes, K iterations and
the worst F32-aligned base residue for each input. It does not union requests
across different instructions or CTAs to manufacture reuse. The generated
tile origins are multiples of eight words, making class multiplicities
preserve sector residues. Overflow checks precede cost publication.

These are source-requested sectors, not measured HBM traffic. Cache service,
compiler instruction combining, spills, achieved residency, loop-control
cost and native latency/bandwidth remain uncalibrated. Existing `TILE_CAND`
Roofline values are staged-comparator diagnostics, not D's traffic bound;
the summary labels this explicitly for a D winner.

## Compatibility And Controls

- Default `gemm_plan_search=baseline` behavior is preserved.
- Joint mode still requires `-O3` and `cuda_whirl_output`.
- If a legacy template output path is requested, joint mode diagnoses the
  unsupported combination before publication; use `cuda_legacy_emit=OFF`
  and translate the selected WHIRL image. Baseline template parity remains.
- Generated-host application is still S1-only. It explicitly enumerates
  neither D nor T2 until the host-provider plan ABI supports those residency
  contracts; the mode record exposes this restriction as `direct=0`.
- The new `DSL_FETCH_PLAN_FLAG_DIRECT` is runtime-only common planning state.
  Common verification rejects direct plans with staging bytes, barriers,
  async flags, staged evolution or non-register destinations.
- D retains one logical demand stage in CommonPipelineIR, not a physical
  staging slot. The D tag, zero allocation and no-barrier contract are
  authoritative. Device function ABI remains three F32 pointers.

## Trace And Tool Contract

Keep the compact v7 stream; distinguish the extended cost model explicitly:

```text
JOINT_MODE joint 48 tile_count tma_permitted relative low model=2 direct=0|1
JOINT_CAND ordinal tile tile_plan fetch_plan engine legality reason compute memory_unhidden communication_unhidden sync setup runtime total buffer_bytes capacity_bytes D|S1|T2 source_line
DIRECT_ACCESS ordinal A_sector_bytes B_sector_bytes C_sector_bytes cache=unknown
JOINT_SELECT winner S1_fallback count selection_reason=lowest_complete_joint_cost
```

In joint mode, `FETCH_CAND` adds the residency tag; baseline mode retains its
existing field count. D's publication proof uses
`direct_load_compute_store`, zero storage, and `abi=direct_ptr_v1`;
S1/T2 keep their existing ordering/ABI contracts. `EDGE_PLAN` records
guarded A/B loads and `barriers=none` for D, not zero-fill staging.
The tool continues to accept prior model-1 traces.

| Report | Required summary |
| --- | --- |
| `tile.log` | Tile geometry/layout comparators and the final selected tile; tile costs remain diagnostic in joint mode. |
| `fetch-pipeline.log` | Explicit D/S1/T2 kind, active allocation, actual guarded loads or staging schedule; never describe D as a ping-pong pipeline. |
| `physical-plan.log` | Complete census, direct warp-sector evidence, whole costs, rejection reasons, selected kind and S1 fallback. |
| `optimization-summary.log` | Selected kind/cost, zero SMEM for D, unknown cache service, actual device proof, fallback and remaining native-validation gate. |
| `optimization.log` | Complete phase-log content, including nonselected/rejected alternatives. |

The parser independently reproduces D's request map and all complete costs,
checks full candidate coverage and deterministic selection, and rejects a D
fallback masquerading as S1. Multi-PU summaries use the complete joint cost,
not the selected tile's preliminary staged score.

## Verification And Remaining Work

Unit tests include analytically known broadcast/coalescing counts, a D
winner with distinct S1 fallback, corrupted sectors, illegal allocation and
fallback substitution. The native AIO-10 producer rejects six malformed D
records and preserves its existing target/unchanged-image contracts.

`test_direct_staged_comparison.py` retains actual selected `.B/.T/.cu`, raw
`.t`, reports, commands and host-only numerical simulation for tiny,
longer-K, non-square tails, multi-CTA and staged-winning fixtures. It also
checks mixed D/S1 PU publication and source compilation, missing sector
evidence, inserted barriers and corrupted reduction bounds. Before/after
images explicitly compare semantic analysis input with the newly created
device PU; they are not an in-place host-PU checkpoint.

Native CUDA compilation, GPU correctness and calibration remain pending.
`cp.async`, actual vector instructions, larger search spaces, independent
D-specific register/resource discovery, runtime variants and TMA host launch
integration are separate milestones. No GPU speedup is claimed here.
