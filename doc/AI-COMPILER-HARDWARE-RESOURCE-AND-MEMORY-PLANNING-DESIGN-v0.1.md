# AI Compiler Hardware Resource and Memory Planning Design

Version: v0.1
Status: design proposal for peer review
Scope: standalone dense GEMM first; Hopper and Blackwell are reference targets
Revision: 2026-10-22, delayed SMEM cache-blocking alternatives and opt-in joint-plan search
Companion: `AI_compiler_optimization_design_v0.3.md` and
`AI-COMPILER-OPTIMIZATION-IMPLEMENTATION-PLAN.md`; rationale candidate:
`AI-COMPILER-WHITE-PAPER-CANDIDATE-DELAYED-GEMM-PLAN-SELECTION.md`

## 1. Architecture and decision boundary

### 1.1 Notation

| Term | Definition in this document |
| --- | --- |
| GEMM, M/N/K | Matrix multiplication `C[M,N] = A[M,K] * B[K,N]`; M/N are output dimensions and K is the reduction dimension. |
| GPU, SM, CTA, warp | Graphics processor; streaming multiprocessor; cooperating thread block; NVIDIA group of 32 threads executing together. |
| SIMT | Single-instruction, multiple-thread execution. A lane is one thread's position in a warp. |
| GMEM, HBM | Device-global address space; high-bandwidth physical device memory. GMEM requests may be served by cache, not necessarily HBM. |
| L1, L2, SMEM | First-/second-level GPU caches; explicitly managed on-chip shared memory. L1 and SMEM share a configurable physical resource on reference targets. |
| Per-CTA SMEM footprint | All simultaneously reserved static and dynamic shared-memory bytes for one CTA, including operand slots, padding, barriers, and other live objects. It is not the GEMM-wide footprint. |
| Per-SM SMEM capacity | Shared-memory partition available to all CTAs resident on one SM under the selected carveout. This is distinct from the per-CTA launch limit. |
| Static / dynamic SMEM | Compile-time `__shared__` allocation / launch-sized `extern __shared__` allocation. Their combined footprint must fit the target's per-block limit. |
| Residency | Number of CTAs or clusters that can execute concurrently on an SM or device given SMEM, registers, threads, and other target limits. |
| Legal / profitable gate | A hard proof that a plan can execute correctly / a separate cost decision that it should replace the baseline. Legal does not imply profitable. |
| TMEM | Blackwell tensor memory, a distinct on-chip storage tier for some tensor-core accumulators. |
| TMA | Tensor Memory Accelerator, the Hopper-and-later asynchronous bulk transfer facility. |
| WGMMA, `tcgen05` | Hopper warp-group and Blackwell tensor-core matrix-multiply instruction families. |
| IR, WHIRL, PU | Intermediate representation; Open64's WHIRL IR; one program unit/function in an established compiler scope. |
| LNO, VHO, IPA | Open64 loop-nest optimizer; very-high-level optimizer; interprocedural analysis. |
| P7.4, P8 | Shared-memory tile/prefetch-hint subphase; subsequent fetch, prefetch, and asynchronous-movement phase in the companion design. |
| CUDA, CUTLASS | NVIDIA's GPU programming platform; NVIDIA's GEMM/kernel library used here as implementation evidence. |

The compiler should preserve a semantic GEMM, its tensor identity, and a set
of candidate physical plans until it can evaluate the complete plan. The
selected plan, not an early tiling pass, is the input to code transformation.
For the first implementation, GEMM is standalone and fusion is excluded.

```text
semantic GEMM and tensor descriptors
    -> existing GMEM views, alias ranges, and legal representations
    -> tile, SIMT ownership, layout, and resource-allocation candidates
    -> input-movement and compute-schedule candidates
    -> target legality + common cost terms + target cost extensions
    -> one selected physical plan with a baseline fallback
    -> atomic generated-kernel lowering and verification
    -> .t trace, phase logs, correctness, and measured feedback
```

There are two planning steps, but they are not independent commitments.
**Resource allocation planning** proposes where each live tile and accumulator
would reside. **Movement scheduling** proposes when transfers, waits, compute,
and releases would occur. A schedule can invalidate an allocation (for
example, needing another slot); the cost gate evaluates the pair. Physical
allocation and executable instructions appear only after selection.

The model distinguishes three nesting levels that must not share the term
"ping-pong": (1) A/B input slots rotated across K tiles, (2) output-tile
assignment across CTAs, and (3) CUTLASS Hopper persistent ping-pong, in which
two consumer warp groups work on different output tiles. TMA input staging is
orthogonal to persistent output scheduling [N1, N2].

### 1.2 Provenance and design stance

Open64 LNO supplies the historical design lesson: model footprints, memory
levels, resource cycles, and prefetch before transforming loops. Its MHD level
records include effective capacity, line size, miss penalties, associativity,
and overlap factors; its compute model compares operation, memory, and issue
resource bounds [O1, O2]. LNO also explicitly notes a limitation: it chooses
L1 blocking before L2 blocking, so an L1 choice can hurt L2 [O3]. We retain
the holistic, delayed-decision principle but **do not** copy that sequential
commitment or equate a CPU cache with explicitly managed GPU SMEM.

The CUDA/CUTLASS facts in this document are reference evidence. The IR,
candidate search, cost gate, and Open64 lowering sequence are **design
proposals**, not claims that current Open64 already generates such kernels.

## 2. Target resource model

The visible allocation hierarchy is not simply "HBM -> SMEM -> registers".
L2 and the unified L1/shared resource affect performance; Blackwell TMEM
adds a distinct accumulator tier for `tcgen05`. Capacities and throughput must
come from a target-specific profile or device query, not a generic "Hopper"
or "Blackwell" constant. H200 and Blackwell SKUs must remain distinguishable.

| Resource | Required description | Why the plan needs it |
| --- | --- | --- |
| GMEM / HBM | Existing A/B/C allocation, strides, alignment, transaction footprint, bandwidth estimate | Determines coalescing, transfer bytes, and optional repacking value. |
| L2 and L1 | Capacity assumptions, cache policy, reuse across CTAs, carveout interaction | HBM bytes are not necessarily equal to GMEM request bytes. Cache residency is not a guaranteed placement. |
| SMEM | Default and opt-in per-CTA limits; per-SM capacity and supported carveouts; static/dynamic split; allocation granularity, alignment, banks, and swizzle | Constrains A/B slots, barriers, epilogue storage, launch legality, occupancy, and bank conflicts. |
| Register file | Registers per thread and per SM, allocation granularity, spill estimate | Constrains SIMT fragments, Hopper WGMMA accumulators, and resident CTAs. |
| TMEM (Blackwell) | Capacity, allocation unit, ownership, accumulator lifetime | Constrains `tcgen05` accumulator and optional extra accumulator slots. |
| Compute units | SIMT FMA and legal MMA families, input/accumulator/output formats, instruction shapes, issue/throughput, precision contract | Determines whether strict F32 uses SIMT or a separately authorized tensor-core path. |
| Transfer engines | Demand/vector load, async copy, TMA, issue cost, alignment, descriptor and barrier rules | Determines available movement schedules and possible overlap. |
| Execution resources | SM count, warps/CTA, CTAs/SM, CTA clusters, barriers, scheduler support | Determines occupancy, wave effects, and persistent feasibility. |

"Existing A/B/C allocation" means a validated live buffer binding, not a
`cudaMalloc` emitted by the tile planner. The selected plan declares buffer
requirements; `TargetBufferService` in
`OPENPY-COMPILATION-AND-EXECUTION-MODES-DESIGN-v0.1.md` imports or allocates
them through the configured target provider. The NVIDIA provider may use CUDA
APIs, while a different target uses its own allocator and transfer mechanism.
This ownership boundary is separate from planning per-CTA SMEM slots and
optional cost-gated GMEM repacking.
The host-call transformation must retain the selected plan's buffer
requirements so the runtime can import existing device storage or allocate
and transfer from host storage once per invocation. P7's lane map does not
authorize a host-to-device copy on its own; P4/P6 residency and the selected
execution plan determine whether that copy is needed.

The current `DSL_MEMORY_TIER_RECORD` and movement-capability records are a
starting point, but they lack an explicit SMEM bank model, target bandwidth,
SIMT lane map, per-SM resource allocation, and TMEM tier [O4]. NVIDIA
documents 32 shared-memory banks, with successive 32-bit words mapping to
successive banks; bank conflicts can serialize accesses [N3]. TMA swizzle and
its supported layout constraints must be checked against the selected MMA or
SIMT consumer layout [N4, N5].

### 2.1 Precision capability is not precision permission

The target profile must expose its supported precision ladder, including the
smallest **accelerated input formats it actually implements**, not prescribe
the smallest format for every GEMM.
For example, compute capability 9.0 Hopper supports FP8 Tensor Core inputs
but not native FP4 Tensor Core inputs; Blackwell SM100 adds FP6/FP4 paths,
including block-scaled FP4. Do not infer one Blackwell SKU's instruction or
capacity envelope from the family name alone [N10, N13].

For every supported `(operand format, instruction family)` pair, the hardware
description must provide input packing; scale format/granularity and layout
when applicable; legal MMA tile shapes and accumulation/output formats;
alignment and K-divisibility requirements; GMEM-to-SMEM movement and swizzle
rules; register/SMEM/TMEM footprints; and calibrated throughput and setup
costs. This information guides candidate generation and P7/P8 profitability.
It does not authorize conversion of a source tensor to fewer bits.

The proposed compiler option is
`-DSL:gemm_precision=preserve|allow_narrow`, with `preserve` as the default.
Only `allow_narrow` authorizes compiler-initiated lower-bit candidates. It
does **not** force the narrowest format: the source representation and
conservative kernel remain eligible. The target and numerical contracts
determine whether a particular FP8, MXFP4, NVFP4, or other format is a
candidate. An unsupported format or instruction path must be reported, not
silently treated as legal. This option is not implemented yet.
The existing `-DSL:quant` switch controls a quantization stage but is not
by itself authorization to change GEMM's numerical format [O8].

For a higher-precision source, each allowed narrowed candidate also needs
an explicit numerical acceptance contract for the region: permitted error
or validated quantization recipe, scaling/rounding and saturation behavior,
accumulation and output type, and the scope of validation. No such contract
means no compiler-initiated narrowing, even in `allow_narrow` mode.
Already-quantized source operands may use matching hardware without a new
precision-changing conversion. Model-level accuracy requirements remain
AI-domain-specific; the common candidate records carry option provenance,
source/selected formats, legality evidence, and fallback without owning the
AI acceptance policy or deciding that precision loss is acceptable.

## 3. Definitions used by the schedule

These terms are defined here before the movement sequence. "Allocation" in a
candidate is a proposed resource reservation; "allocation" in generated code
is a concrete shared-memory, register, or TMEM object.

| Term | Precise meaning |
| --- | --- |
| GMEM operand view | Addressable slice of existing A or B in global memory for one CTA output tile. It is normally not a new allocation. |
| GMEM repacking | Optional creation of a new global allocation with a different layout. This is a separate, cost-gated transformation, not A/B staging. |
| CTA output tile | `BM x BN` part of C owned by a CTA or cluster; `BK` is one reduction step. |
| SIMT access map | Mapping from lane/warp/thread to tensor elements for GMEM load, SMEM store/read, compute, and output store. |
| Source-warp memory issue | One scalar load or store from each active lane at the same source-level instruction point; PTX optimization may change its instruction shape. |
| GMEM sector request | A distinct 32-byte segment touched by one source-warp issue; repeated requests count again. It is not an observed L2 miss or HBM transfer [N3]. |
| Base-alignment case | Offset of an operand's first F32 element within a 32-byte sector, from zero through seven words. The planner retains the aligned case and costs the independent worst case for A, B, and C. |
| Operand staging buffer | On-chip A or B storage consumed by a K step: `A_smem` and `B_smem` in the reference paths. |
| Operand buffer slot | Independently reusable position in an operand staging buffer. A logical stage has an A slot and a B slot, possibly with distinct sizes. |
| SMEM allocation ledger | Per-CTA list of SMEM objects with actual padded extent, alignment, offset, static/dynamic class, live interval, and owner. Proven non-overlapping intervals may share storage. |
| Input pipeline stage | K-tile index, its A/B slots, and readiness/ownership state. It is not a third data buffer. |
| Register operand fragment | Optional per-thread A/B values used by an instruction or inner-loop reuse. |
| Accumulator | Partial C output updated across K steps; registers for SIMT and Hopper WGMMA, TMEM for Blackwell `tcgen05` [N6, N7]. |
| Accumulator slot | Additional live output accumulator used to overlap two output tiles; independent of A/B slot count. |
| Epilogue staging buffer | Optional temporary used to convert or rearrange accumulator values before GMEM store. |
| TMA tensor map | Opaque transfer descriptor for a multidimensional GMEM/SMEM layout; it does not contain tile data [N5]. |
| Producer / consumer | Agent that fills an operand slot / agent that reads the filled slot. They may be different warp groups. |
| Acquire / release | Ownership events making a slot writable / declaring all reads complete so it can be overwritten. |
| Ready / empty | Per-slot completion state: data has arrived / prior consumer has released the slot. |
| Prefetch distance | K-tile distance from transfer issue to first consumption. It must respect slot depth and alias safety. |
| Prologue / steady state / drain | Initial fill, overlapped iterations, and final completion/epilogue. |
| Compute bubble | Time when a legal compute issue could have happened but stalls on data, resources, or synchronization. |
| Output-tile scheduler | Assignment of `BM x BN` output tiles to CTAs; separate from the inner K-tile movement schedule. |

The A/B GMEM representation controls spatial locality through the
lane-to-address map; CTA reuse and SMEM staging control temporal reuse.
Neither merely putting A/B in GMEM nor allocating SMEM slots guarantees
coalescing or reuse [N3, N1].

Implementation note (2026-10-25): the synchronous F32 GEMM slice retains a
fixed linear GMEM copy map but now compares compact row-major A with
transposed A padded to stride `BM+1`; B stays row-major. For `BK=32`, the
padded A copy uses distinct banks across each 32-lane GMEM-to-SMEM store,
while compute reads by distinct row groups avoid the compact layout's
same-bank addresses. The selector charges an explicitly low-confidence
bank-conflict-work proxy and lowers the selected A indices into WHIRL. It
does not model bank latency. The TMA sidecar remains compact-only.

Compiler-side GMEM planning update (2026-10-25): the first contiguous F32
slice compares two CTA thread-to-output maps without repacking A, B, or C.
Non-F32 element types are excluded from this sector-cost slice until their
transaction width and legal lowering are modeled explicitly.
The original `TM=8,TN=8` map assigns adjacent lanes output columns eight
apart. The new `TM=16,TN=2` map uses 512 threads and assigns adjacent lanes
columns two apart. Both retain linear A/B copy loops and the same
`BM=BN=128,BK=32` CTA tile. P7 records each map in `CommonTilePlanIR`;
VHO prices the complete candidate before selected device-WHIRL construction.
The same generic WHIRL loop lowering emits either map from the selected
thread dimensions. No independent CUDA-template decision is made. For the
exact contiguous case, full A/B warp loads already request the minimum four
32-byte sectors for 32 adjacent F32 values on aligned bases. A new physical
GMEM representation is not justified by this case; the C-store map is the
profitable search axis. Strided views may change that conclusion.

For each candidate, enumerate the source-level A, B, and C warp issues.
Within an issue, count distinct 32-byte sectors among valid lanes; masked
edge lanes request none. Aggregate full and edge CTA/K classes instead of
enumerating every CTA. CTA origins are multiples of eight F32 elements for
these tile seeds, so their sector residue is invariant within each class.
Evaluate each F32 base alignment from zero through seven words. The aligned
total is diagnostic; the cost uses the independent worst case for each
operand. Baseline traffic remains an optimistic useful-byte lower bound
because no baseline CUDA lane map exists. `TILE_GMEM` and `COST_MODEL 4` in
`.t` expose A/B/C sector bytes and shared-A load issue work. The relative
memory score uses `floor((A+B+C worst sector bytes)/1024)` plus low-confidence
`floor(bank_conflict_work/32)` and `floor(smem_a_load_issues/32)` terms; it
does not predict nanoseconds.

On the exact 128x128x32 fixture, the 16x2 candidate lowers modeled C-store
sector requests from 524,288 to 147,456 bytes and wins the complete relative
cost gate. On 128x128x128, its extra shared-A load issues outweigh that
sector reduction under the current proxy, so padded 8x8 remains selected.
Both emitted WHIRL-derived `.cu` paths pass host CTA parity. The compact A
WHIRL declaration was corrected from `BK*BK` to `BM*BK` elements when this
alternative first exercised it. These are source-model and host-simulation
results, not native PTX transactions, cache traffic, or GPU speedup. General
strided operands, repacked allocations, cache residency, and CTA scheduling
remain later GMEM extensions.

## 4. Candidate construction: allocation, then schedule

### 4.1 Resource-allocation plan

For each numerically permitted and target-legal
`(BM, BN, BK, operand format, instruction family)` alternative, record:

1. Existing A/B/C GMEM views: dimensions, strides, alignment, ownership,
   edge policy, and alias ranges. Record repacked GMEM as a separate candidate.
2. CTA/warp/lane-to-element maps for loads, SMEM access, compute fragments,
   and stores. Compute GMEM transaction utilization and SMEM bank conflicts.
3. A direct/cache-served path with no A/B SMEM slot, plus separately proposed
   one-slot synchronous and target-legal multi-slot staging paths. For staged
   paths record distinct A/B copy maps and SMEM layouts, bytes per slot,
   slot counts, padding, swizzles, alignment, CTA/cluster scope, and lifetimes.
4. Accumulator shape, precision, storage tier, live count, and epilogue
   storage. A second accumulator is not implied by two A/B slots.
5. Barrier and pipeline state, warp roles, register and SMEM footprint,
   residency ceiling, and target capability evidence.

For a dense F32 SIMT example, the unpadded A/B footprint of `S` paired
slots is `S * (BM*BK + BK*BN) * 4` bytes. The real footprint is the sum of
**separately rounded** A and B allocations, plus barriers, padding, and any
epilogue storage. Shared-memory fit must be checked per CTA, while resident
CTA count uses per-SM shared memory, registers, warps, and architectural
limits. A shared-memory carveout preference is a hint, not extra capacity.

#### 4.1.1 SMEM allocation ledger and maximum footprint

There is no maximum SMEM allocation attached to the mathematical GEMM. The
limit applies to each emitted CTA/cluster variant. The full problem M/N/K
sets grid and K-loop counts; only chosen tile dimensions and concurrently
live objects set per-CTA SMEM. Reusing an A/B slot for the next K tile does
not accumulate one allocation per K iteration.

For each candidate, first compute a logical ledger, then a physically packed
ledger. For paired A/B stage `s`, the unpadded starting point is
`A_s = BM * BK * sizeof(A_storage)` and
`B_s = BK * BN * sizeof(B_storage)`. The target layout supplies padded
strides, swizzle extent, and required alignment separately for A and B. The
ledger also includes barrier/phase state, optional scale or metadata tiles,
epilogue scratch, and any other SMEM user. The accumulator is charged to its
actual tier (registers or TMEM for the reference paths), not automatically
to SMEM.

```text
S_requested = packed_size({A_slots, B_slots, barriers, metadata,
                           epilogue, scratch})
S_requested = S_static + S_dynamic
S_accounted = target_residency_charge(S_requested)
```

Pack static and dynamic storage separately. Packing may reuse an offset only
when the selected event graph proves the objects' live intervals do not
overlap, including asynchronous transfer and MMA completion. A slot cannot
be reclaimed merely because its TMA issue or consumer instruction was
launched. The ledger records both peak bytes and
the concrete offsets that lowering will use. `S_requested` is checked against
the per-block limit; `S_accounted` includes any target allocation rounding
used for residency. The emitted kernel must be
rechecked because final compilation can add static SMEM or change resources.
TMA writes into previously reserved SMEM; it does not allocate or evict it.

As a sizing example, FP16 `BM=BN=128`, `BK=64` uses 16 KiB for A and 16 KiB
for B per unpadded paired stage. One, two, and three live stages therefore
start at 32, 64, and 96 KiB per CTA respectively, **before** padding,
barriers, metadata, and epilogue storage. For FP4, packed operand bytes may
shrink while scale/metadata tiles and instruction-layout constraints grow;
the full ledger, not a datatype multiplier, determines fit [N1, N10, N13].
CUTLASS Blackwell's `StageCountAutoCarveout` explicitly reserves epilogue
shared storage before deriving mainloop stage count, a useful precedent for
this full-footprint rule [N13].

P7.4 should carry `smem_prefetch_hint`, reuse distance, TMA eligibility,
expected overlap window, and buffer-depth feasibility. These are advisory
facts; P8 makes the movement schedule. Current AIO-9 only records a combined
`shared_bytes_per_stage` and a maximum stage count [O5].

The direct path is not a degenerate SMEM stage. It has its own A/B warp-load
transactions, L1/L2 hit assumptions, register demand, and no A/B allocation or
staging barriers. For staged paths, count per-operation GMEM requested sectors,
cache-level bytes, SMEM writes and consumer reads, bank conflicts/broadcasts,
layout-conversion work, and dynamic synchronization separately. The same CTA
and register tile may rank differently under those complete movement plans.

### 4.2 Movement-schedule plan

For each allocation candidate, P8 builds at least the following legal
alternatives where supported: demand/synchronous, one-slot staged,
multi-slot async-copy, and multi-slot TMA. A schedule is a typed event graph
with `acquire`, `issue`, `completion`, `wait`, `compute-read`, and `release`
edges for each operand slot. The graph also records K-loop order, stage
count, prefetch distance, warp roles, barrier scope/phase, and short-K drain.

TMA belongs **before** final profitability selection. A one-slot synchronous
kernel is the bring-up and fallback milestone; two-slot TMA is the principal
Hopper/Blackwell overlap candidate. It is not automatically superior: small K,
irregular strides, setup cost, barriers, or occupancy loss may favor another
schedule. TMA transfers are GMEM-to-SMEM; they do not promise that each byte
comes from HBM instead of cache [N4].

P8 schedules each surviving direct or staged alternative; it does not pick
SMEM residency on availability alone. Model the full event critical path,
including `ceil(K/BK)` iterations and their dynamic barriers. The one-slot
synchronous path normally has an arrival barrier and a pre-overwrite barrier
per K tile; prove any first/last-iteration elision. For async stages, prove
arrival, wait, and release ordering before crediting overlap. P9 chooses the
lowest credible complete legal plan with a conservative fallback.

The proposed `-Wb,-DSL:gemm_plan_search=baseline|joint` option defaults to
`baseline`. `joint` requests a bounded cross-product of tile, copy map, A/B
SMEM layout, register fragment, and movement-schedule alternatives, including
the direct path. It is not registered or implemented; until supported, the
driver must reject `joint` explicitly. It never bypasses legality, numerical
permission, the rewrite gate, or the trace contract. This control allows an
equal-budget comparison of delayed joint choice against the current selector
without claiming a performance result in advance.

The current AIO-10 records engine, nominal stage count, issue/wait points,
and aggregate buffering bytes, but does not allocate A/B slots or emit an
executable event graph [O6]. Its current exact-edge-only TMA hint and fixed
three-stage preference are analysis heuristics, not hardware laws.

### 4.3 Plan records and ownership

| Record | Information added or consumed | Policy owner |
| --- | --- | --- |
| `TensorEvolutionGraph` and tensor/object descriptors | Stable A/B/C identity, GMEM views, tile and staged-buffer lineage | Common record; PU-local facts from VHO. |
| `CommonLayoutIR`, `CommonPlacementIR`, `CommonMemoryResidencyIR` | Existing representation, optional repacking, GMEM/SMEM layout, residence scope | Common record; VHO and explicit IPA scope decide. |
| `CommonAccessPatternIR`, `CommonParallelizationIR` | Lane/warp/CTA ownership and address maps, transaction and bank-conflict evidence | Common record; VHO/target analysis decides. |
| `CommonTilePlanIR` | `BM/BN/BK`, instruction family, separate A/B allocation descriptors, accumulator descriptor | Common record; P7 candidate policy in VHO. |
| `CommonFetchPlanIR`, `CommonPipelineIR` | Per-operand transfer, slot ownership, event graph, stage depth, prefetch distance, barriers | Common record; P8 scheduling policy in VHO. |
| `OptimizationCandidateIR`, `OptimizationPlanIR`, `CandidateCostIR` | Complete tuple, legality, cost evidence, selection, fallback | Common record; VHO owns selection policy. |
| Target extension | CUDA tensor maps, swizzles, SMEM banks, WGMMA/`tcgen05`, cluster/CLC details | NVIDIA target adapter and code generator. |

All new records are provisional until selection. Persisting them in binary
WHIRL is a separate compatibility decision, not a prerequisite for the first
generated kernel. A common record names a fact; it does not own a PU-local
optimization decision.

`CommonTilePlanIR` should carry the domain-neutral allocation ledger,
static/dynamic split, target-limit snapshot, requested residency, and
measured-versus-estimated evidence tags. The NVIDIA extension supplies
carveout choices, bank/swizzle rules, TMA and instruction restrictions, and
CUDA launch attributes; none of those become generic IR policy.

### 4.4 Alias and lifetime requirements

The scheduler must prove that each GMEM tile read is valid until its transfer
finishes and that no SMEM slot is overwritten until all consumers, including
asynchronous MMA reads, are finished. Alias ranges should be tile ranges, not
only whole-tensor IDs. Distributed/MPI ownership is a separate, wider-scope
question; it cannot be inferred from a CTA-local slot protocol. A remote shard
handoff must complete before a local GMEM view becomes fetchable. The selected
plan records both proofs without conflating their scopes.

## 5. Legality and profitability gate

The selection unit is a **complete tuple**:

```text
(representation, precision/scale format, tile, SIMT map, A/B allocation,
 movement schedule, compute family, epilogue, output-tile schedule,
 runtime guard)
```

### 5.1 Hard legality gate

First reject any precision-changing candidate unless `allow_narrow` is active
and the region has numerical acceptance evidence. Then reject
before costing if semantics, precision, alias ranges, resource fit,
TMA descriptor/layout/alignment, barrier phase, cluster topology, or output
ownership are unproven. A forced diagnostic mode may narrow the candidate
set, but must never bypass legality. For SMEM, the proof has these parts:

1. Build the complete per-CTA ledger from the selected tile **and** P8 event
   graph. Verify all offsets, alignments, layout extents, ownership, and
   asynchronous live intervals. P7.4's provisional `shared_bytes_per_stage`
   is insufficient for this final test.
2. Check `S_requested = S_static + S_dynamic` against the target's per-block
   limit and the allowed dynamic allocation for the compiled kernel. Above
   the portable 48 KiB per-block threshold, use the supported dynamic-SMEM
   opt-in path; never assume a carveout preference raises the per-block hard
   limit [N10, N11].
3. Establish at least one resident CTA with the target's per-SM SMEM
   partition, registers, threads, barrier and cluster constraints. A
   requested minimum residency is a performance constraint for costing, not
   automatically a correctness requirement. The exact resource accounting
   and allocation granularity come from the target adapter.
4. After emission, compare the compiled kernel's static SMEM and register
   usage with the plan and validate the requested dynamic launch bytes and
   device attributes. A mismatch reopens legality and may select the
   unchanged fallback; a cost estimate cannot override failed admission.

The CUDA runtime reports opt-in per-block capacity and permits
`cudaFuncAttributeMaxDynamicSharedMemorySize` subject to
`sharedSizeBytes + maxDynamicSharedMemorySize <= cudaDevAttrMaxSharedMemoryPerBlockOptin`.
The preferred SMEM carveout is a hint; the driver can choose another legal
partition [N8, N11]. The compiler
owns slot layout and synchronization; the runtime/hardware admits CTAs and
reserves the physical shared-memory partition. Neither automatically grows
or spills a live SMEM slot when it is full.

### 5.2 Profitability gate

Unknown target costs cannot be silently treated as zero. For a legal
candidate, estimate actual CTA/SM residency, grid waves, L1 capacity lost
to carveout, register pressure/spills, SMEM bank behavior, and the overlap
obtained by the selected stage depth. As a quick necessary bound for `R`
resident CTAs, `R * S_accounted <= effective_SMEM_per_SM`; this is not a complete
occupancy calculation. Once a real kernel exists, use
`cudaOccupancyMaxActiveBlocksPerMultiprocessor` with its intended dynamic
SMEM bytes and, when solving for a requested CTA count,
`cudaOccupancyAvailableDynamicSMemPerBlock` [N12]. Cluster plans also need
cluster-aware occupancy checks. Occupancy is an input to latency prediction,
not an objective to maximize independently.
For a narrow-format plan, include quantization/repacking and scale-factor
traffic, scale layout conversion, any extra metadata buffers, the chosen
accumulation/epilogue path, and workload reuse of converted operands. Compare
the entire plan against the unchanged source-format baseline; choosing the
fewest bits or the highest advertised MMA throughput alone is not the gate.

For a legal candidate, use explicit units and confidence:

```text
T_plan = T_launch + T_scheduler + T_prologue
       + T_compute_effective + T_memory_unhidden
       + T_communication_unhidden + T_sync + T_epilogue
       + T_drain + T_runtime_guard

T_memory_unhidden = max(0, T_movement_exposed - T_overlap_credit)
T_communication_unhidden = max(0, T_comm_exposed - T_comm_overlap_credit)
T_compute_effective = compute_work / attained_compute_rate
```

The attained rate is limited by instruction family, issue throughput,
occupancy, register spills, wave quantization, and any SMEM bank conflicts.
Movement cost distinguishes useful bytes from physical transactions, L2 hits
from HBM misses where evidence exists, TMA issue/setup cost, and barrier
waiting. `T_overlap_credit` is bounded by legal concurrent work and measured
or calibrated latency; it is not the entire transfer latency by default.
An extra A/B stage earns credit only for transfer latency it actually hides;
charge its additional SMEM footprint against residency, L1 carveout, and
barrier overhead. Compare one-, two-, and deeper-stage plans on the same
problem/target envelope instead of using a fixed stage preference.
Compare against an unchanged direct baseline and include compilation or
descriptor setup overhead when the workload's reuse count makes it material.

The plan record keeps rejected candidates and reasons. The selected plan
must exceed the baseline by a confidence-aware margin before a rewrite; a
tie or missing evidence retains the baseline. Post-run telemetry should
calibrate, not retroactively legalize, a plan.

## 6. Transformation and CUDA lowering contract

The CUDA-facing lowering is a **target adapter proposal**. Open64 must first
establish a real generated-kernel or reviewed provider emission path. A plan
record, an ABI declaration, or a dispatch to an external library is not by
itself proof of an Open64-generated tiled kernel. In this checkout,
`apply_selected_plan` is disabled for AIO-11, while
`__open64_dsl_matmul_physical_v1` is an ABI boundary [O7].

Lower only the selected plan. Construct a detached kernel body, verify it,
then atomically replace the semantic matmul or retain the baseline. Do not
change binary WHIRL layout merely to carry a temporary scheduling object.
Use stable plan IDs through selection, transformation, trace, and artifacts.

| Plan item or event | CUDA-facing realization | Verification point |
| --- | --- | --- |
| GMEM A/B/C views | Kernel parameters with pointers, shapes, strides; device-side CTA tile coordinates | Address bounds, alias contract, alignment, output ownership. |
| A/B SMEM allocation | Static or dynamic `__shared__` storage with explicit offsets/alignment and layout indexing | Separate A/B slot sizes, total CTA bytes, per-SM occupancy. |
| SIMT map | `threadIdx`, warp/lane math, predicated loads, SMEM stores, and FMA ownership | Coalescing and no duplicate/missing output writes. |
| One-slot load/ready | Cooperative GMEM loads and stores, then CTA synchronization | No read before load; no divergent CTA barrier. |
| TMA descriptor | Host-side `cuTensorMapEncodeTiled` for supported multidimensional views; pass descriptor to kernel | Device capability, dimensions, stride, alignment, swizzle, descriptor lifetime. |
| TMA issue | Device-side `cuda::ptx::cp_async_bulk_tensor` or equivalent verified target intrinsic | Selected A/B slot and coordinates; no unsupported path silently chosen. |
| TMA completion | Shared-memory barrier with expected transaction bytes and phase-aware wait | Wait covers both A and B arrivals before compute. |
| Slot release | Consumer-to-producer handoff after all SMEM reads, including async MMA, complete | No overwrite while a consumer still reads. |
| Compute | SIMT FP32 for first case; FMA contraction only when source semantics permit it; separately legal WGMMA or `tcgen05` paths later | Precision contract, accumulator location, completion before reuse/store. |
| Epilogue | Convert/rearrange if needed, then bounded GMEM store | Numeric result and unique output ownership. |
| Launch envelope | Device query, compiled static-SMEM/register check, exact dynamic-SMEM launch bytes, opt-in when required, grid/cluster parameters | Per-block admission and requested residency; fallback on unsupported launch. |

For multidimensional TMA, the CUDA Driver API defines
`cuTensorMapEncodeTiled`; the CUDA programming guide documents the
`cuda::ptx::cp_async_bulk_tensor` device path and shared-memory-barrier
completion [N4, N5]. The adapter must use the exact supported signature for
its CUDA toolkit and target, rather than encoding an invented universal API.
The runtime API provides `cudaFuncSetAttribute` for an opt-in dynamic shared
memory limit and a **preferred** shared-memory carveout [N11]. Cluster launch
setup is only needed for a selected cluster plan, not every TMA plan.
The emitted A/B offsets and dynamic launch size must equal the selected
ledger. No kernel should be reported as transformed until launch admission,
slot safety, and numerical result have been verified.

For the first strict-F32 GEMM, TMA may stage F32 operands while SIMT FP32
operations perform compute, with contraction governed by source semantics.
TMA eligibility does not grant permission to replace strict F32 arithmetic
with TF32. A tensor-core path needs an independent precision
and instruction legality proof. Hopper WGMMA uses register accumulators;
Blackwell `tcgen05` uses TMEM accumulators [N6, N7].

### 6.1 Executable milestone order

1. Certify a direct baseline, trace switch, and report parser.
2. Select and emit one F32 SIMT tiled kernel with one A/B SMEM slot; inspect
   generated IR and instructions and measure against the direct baseline.
3. Generate two-slot TMA and non-TMA alternatives from the same tile plan;
   prove barrier/slot safety and select with the common cost gate.
4. Calibrate predicted compute, movement, sync, and occupancy terms with
   retained measurements. Do not report an analysis-only plan as an achieved
   optimization.
5. Only after the source-format path is certified, admit option-authorized
   narrow formats with region-specific numerical evidence. Retain the
   source-format fallback and measure whole-plan latency and numerical error
   separately for each target and shape bucket.

For every milestone, `tile.log` summarizes tile and resource alternatives,
including each candidate's requested/accounted SMEM bytes, per-block limit, feasible
residency, and legality rejection reason. `fetch-pipeline.log` summarizes A/B
allocations, engine, stage depth, slot schedule, waits, and predicted
hidden/unhidden transfer time. `optimization-summary.log` reports enabled,
skipped, rejected, selected, transformed, and verified phases, with the
selected-vs-baseline cost and whether launch/resource verification passed.
For precision-changing alternatives, `tile.log` also records source/selected
formats, the compiler-option mode, numerical-contract identity, scale-tile and
effective-byte assumptions, and the exact numerical, target, or profitability
rejection reason. `optimization-summary.log` distinguishes an allowed but
unselected format from an applied precision-changing rewrite.
Each phase log begins with its agreed `What it summarizes` line. The raw
`.t` remains the machine-readable source of truth. The proposed WOPT-style
AI trace switch is documented in the implementation plan; it is not claimed
to be active today.

## 7. Optional persistent GEMM schedule

Persistence is an **output-tile assignment** choice. A conventional CTA
computes one output tile and exits; a persistent CTA or cluster gets further
output tiles from a tile scheduler within the same kernel invocation.
CUTLASS Hopper has persistent cooperative and persistent ping-pong schedules.
In the former, two consumer warp groups divide one output tile; in the latter,
they work on different output tiles so one group's epilogue can overlap the
other's MMA [N1]. This is distinct from rotating two A/B input slots.

| Axis | Candidate values | Independent cost or constraint |
| --- | --- | --- |
| Output-tile assignment | Conventional, static persistent, dynamic persistent | Grid waves, tile imbalance, scheduler cost, CTA prologue amortization. |
| Consumer organization | Single, cooperative same tile, different-tile ping-pong | Register/TMEM footprint, barriers, epilogue/MMA overlap. |
| A/B input pipeline | One-slot synchronous, async copy, N-slot TMA | SMEM, descriptor/setup cost, hidden memory time. |

Static persistence can assign subsequent tiles with local arithmetic; it can
lose when actual available SMs differ from the assumed grid. Blackwell CLC
supports a dynamic persistent scheduler with extra query/pipeline machinery
[N9]. This is target-specific, not a generic Open64 scheduling primitive.
Persistent scheduling can amortize **per-CTA** startup and overlap epilogue
with MMA; it does not eliminate a CUDA kernel launch for one GEMM. It can lose
on too few tiles, costly synchronization, excess resource use, or imbalance.

The proposed future user option is
`-DSL:gemm_schedule=auto|conventional|persistent` (not implemented).
`auto` compares all certified alternatives; `conventional` excludes
persistent candidates; `persistent` restricts the experiment to legal
persistent candidates and reports that the conventional baseline was
excluded. The option never forces an illegal plan. The first strict-F32 SIMT
milestone need not implement CUTLASS's Hopper WGMMA persistent ping-pong.
Expose the option only when a persistent implementation exists and is
verified. The same candidate IDs, legality gate, cost terms, fallback, and
trace schema apply to persistent and non-persistent schedules.

## 8. Current Open64 boundary and review questions

`CommonTilePlanIR` currently has tile shapes, aggregate shared bytes per
stage, register estimate, and a maximum stage count. `CommonFetchPlanIR` has
movement-engine alternatives, stage/slot ordinals, nominal issue/wait points,
and aggregate buffering bytes. `CommonPhysicalPlanIR` can compare plans but
does not apply the selected GPU transformation [O5, O6, O7]. These are useful
planning foundations, **not** evidence of actual A/B staging or TMA emission.
Likewise, the proposed precision option is not an implemented
compiler option or a claim that Open64 currently converts GEMM operands.

The new descriptors should be domain-neutral immutable records under
`common/com`; VHO owns PU-local fact gathering, candidate generation,
legality, cost, and selection; target lowering owns CUDA-specific emission.
Any cross-PU representation or repacking decision belongs to explicit IPA
summaries and scope, not a VHO pass that happens to see global tables.

Peer review should resolve: exact GPU emission route; whether A and B may
have independent slot depths; a calibrated bank-conflict and L2 model;
dynamic descriptor ownership and reuse; precision choices for Hopper and
Blackwell; and whether a future persistent option belongs at `-DSL` or in a
dedicated backend schedule group. None blocks the one-slot generated F32
milestone.

## 9. References

Open64 source evidence:

- [O1] `osprey/common/com/config_cache.h`, `MHD_LEVEL`.
- [O2] `osprey/be/lno/model.cxx`, resource-bound model.
- [O3] `osprey/be/lno/cache_model.cxx`, "Blocking for L1, then L2".
- [O4] `osprey/common/com/dsl_memory_hierarchy.h`, current tier and movement records.
- [O5] `osprey/common/com/dsl_tile_candidate.h` and `osprey/be/vho/dsl_tile_candidate_opt.cxx`.
- [O6] `osprey/common/com/dsl_fetch_pipeline.h` and `osprey/be/vho/dsl_fetch_pipeline_opt.cxx`.
- [O7] `osprey/be/vho/dsl_physical_plan_opt.cxx`, `osprey/be/vho/dsl_lower.cxx`, and `osprey/include/open64_dsl_runtime_abi.h`.
- [O8] `osprey/common/com/config_dsl.cxx`, current `-DSL:quant` stage switch.

NVIDIA primary documentation:

- [N1] [CUTLASS Efficient GEMM in CUDA](https://docs.nvidia.com/cutlass/latest/media/docs/cpp/efficient_gemm.html).
- [N2] [CUTLASS 3.x GEMM API](https://github.com/NVIDIA/cutlass/blob/main/media/docs/cpp/gemm_api_3x.md).
- [N3] [CUDA SIMT kernel memory access](https://docs.nvidia.com/cuda/cuda-programming-guide/02-basics/writing-cuda-kernels.html).
- [N4] [CUDA asynchronous copies and TMA](https://docs.nvidia.com/cuda/cuda-programming-guide/04-special-topics/async-copies.html).
- [N5] [CUDA Driver API tensor maps](https://docs.nvidia.com/cuda/cuda-driver-api/cuda_driver_api/group__CUDA__TENSOR__MEMORY.html).
- [N6] [CUTLASS Hopper WGMMA data flow](https://docs.nvidia.com/cutlass/latest/media/docs/pythonDSL/guides/mma/wgmma_programming.html).
- [N7] [CUTLASS Blackwell tcgen05 data flow](https://docs.nvidia.com/cutlass/latest/media/docs/pythonDSL/guides/mma/tcgen05_programming.html).
- [N8] [CUDA advanced kernel programming and L1/shared-memory balance](https://docs.nvidia.com/cuda/cuda-programming-guide/03-advanced/advanced-kernel-programming.html).
- [N9] [CUTLASS Blackwell cluster launch control](https://docs.nvidia.com/cutlass/4.3.3/media/docs/cpp/blackwell_cluster_launch_control.html).
- [N10] [CUDA compute-capability resource limits](https://docs.nvidia.com/cuda/cuda-programming-guide/05-appendices/compute-capabilities.html).
- [N11] [CUDA runtime function attributes and SMEM opt-in](https://docs.nvidia.com/cuda/cuda-runtime-api/cuda_runtime_api/group__CUDART__EXECUTION.html).
- [N12] [CUDA runtime occupancy API](https://docs.nvidia.com/cuda/cuda-runtime-api/cuda_runtime_api/group__CUDART__OCCUPANCY.html).
- [N13] [CUTLASS Blackwell SM100 GEMMs](https://docs.nvidia.com/cutlass/latest/media/docs/cpp/blackwell_functionality.html).

## 10. End-to-end pseudocode

This pseudocode is intentionally target-neutral. The named synchronization
operations are semantic events, not literal CUDA signatures. The concrete
adapter must implement and verify them for its selected instruction path.

```text
# Compile time: retain alternatives until complete cost selection.
formats = {source_format}
if compiler_option.gemm_precision == allow_narrow:
    formats += numerically_permitted_target_formats(region, target)
for format in formats:
    if not prove_numerical_and_target_format_legality(format, region, target):
        reject_format_with_reason(format)
        continue
    representation = propose_operand_representation(format, source_operands)
    for tile in legal_tile_families(BM, BN, BK, format, target):
        gA, gB, gC = GMEM_views_and_conversion_plan(representation, tile)
        access = derive_SIMT_maps_and_SMEM_layouts(gA, gB, gC, tile)
        for schedule in {one_slot_sync, two_slot_TMA, other_legal_schedules}:
            allocation = propose(
                A_smem[schedule.A_slots], B_smem[schedule.B_slots],
                accumulator[format], scale_tiles[format], optional_epilogue_buffer)
            events = build_prologue_K_loop_drain(allocation, schedule)
            ledger = pack_live_SMEM_objects(allocation, events, target)
            if not prove_semantics_alias_and_happens_before(events):
                reject_with_reason(format, tile, schedule)
            elif not prove_target_resource_and_launch_legality(ledger, target):
                reject_with_reason(format, tile, schedule)
            else:
                residency = estimate_resource_limited_residency(ledger, target)
                cost = estimate_complete_plan(representation, tile, access,
                                              allocation, events, ledger, residency)
                retain_candidate(format, tile, events, ledger, cost)
selected = choose_profitable_plan_or_baseline()
emit_atomically_and_verify_launch_resources_and_result(selected)
```

```text
# First executable path: one A slot and one B slot per CTA.
A_smem[1], B_smem[1] = allocate_shared(selected_layouts)
acc = initialize_accumulator()
for k in 0 .. K_tiles-1:
    cooperative_load(gA(k), A_smem[0])
    cooperative_load(gB(k), B_smem[0])
    CTA_wait_until_loads_visible()
    compute_tile(A_smem[0], B_smem[0], acc)
    wait_until_compute_has_finished_reading_slot(0)
    CTA_wait_before_overwrite(0)
store_output(acc)
```

```text
# Hopper/Blackwell candidate: two A/B input slots with TMA prefetch.
A_smem[2], B_smem[2] = allocate_shared(TMA_compatible_layouts)
ready[2], empty[2] = initialize_pipeline_state()
acc = initialize_accumulator()

function prefetch(k, slot):
    producer_wait_empty(empty[slot])
    expect_TMA_bytes(ready[slot], bytes_A(k) + bytes_B(k))
    issue_TMA(gA(k) -> A_smem[slot], ready[slot])
    issue_TMA(gB(k) -> B_smem[slot], ready[slot])
    # The issuing thread does not make data ready; completion does.

if K_tiles > 0: prefetch(0, 0)             # Prologue
for k in 0 .. K_tiles-1:
    slot = k mod 2
    consumer_wait_ready(ready[slot])       # Both operands arrived.
    if k + 1 < K_tiles:
        prefetch(k + 1, (k + 1) mod 2)     # Other slot fills during compute.
    compute_tile(A_smem[slot], B_smem[slot], acc)
    wait_until_compute_has_finished_reading_slot(slot)
    consumer_release(empty[slot])          # Safe to refill this slot.
wait_until_accumulator_complete()          # Drain
store_output(acc)
```

```text
# Optional outer output-tile scheduler; reuse the selected inner K schedule.
while output_tile_scheduler.has_work():
    tile = output_tile_scheduler.next()
    acc = initialize_accumulator_for(tile)
    execute_selected_A_B_K_pipeline(tile, acc)
    epilogue_and_store(tile, acc)

# Hopper CUTLASS-style persistent ping-pong is a different alternative:
# consumer_group_0 and consumer_group_1 receive DIFFERENT output tiles;
# one group's epilogue may overlap the other's MMA. An ordered producer
# protocol feeds both groups. It still needs its own A/B input pipeline.
```

## Synchronous SMEM Planning Package (2026-10-25)

This implemented slice plans scalar-F32 staging for the existing bounded GEMM
families. It does not close the later multi-stage async/TMA WHIRL milestone.
SMEM means CTA-local shared memory; a bank round is one service round for
distinct 32-bit words requested from the same bank by a source warp instruction.
The Hopper/Blackwell profile adapter supplies 32 banks, four bytes per word,
and 32 lanes per warp. Identical-word **loads** are broadcasts, not conflicts.
These rules follow the [NVIDIA CUDA Best Practices Guide, shared memory](https://docs.nvidia.com/cuda/cuda-c-best-practices-guide/index.html#shared-memory).
The compiler's relative weighting is our design proposal, not NVIDIA guidance.

| Planned object or action | Representation and legality | Cost/report obligation |
| --- | --- | --- |
| A staging allocation | Separate static `__shared__` array: `BM*BK*4` bytes for row-major, or `BK*(BM+1)*4` for padded-transposed A | Record bytes, stride, fill-store and compute-load conflicts |
| B staging allocation | Separate static `__shared__` array: `BK*BN*4` bytes; compact or column-XOR addressing | Record bytes, fill-store and compute-load conflicts; same mapping on fills, zero-fill and reads |
| Accumulator | `TM*TN` F32 elements per thread; not part of the A/B SMEM allocation | Retain register estimate and resource gate; final register allocation remains backend work |
| Lifetime and reuse | One synchronous slot per operand; fill, uniform CTA barrier, compute, uniform CTA barrier, reuse | Two dynamic barriers per CTA/K tile; no overlap claim |
| Full-plan search | Keep original A-layout/thread-map choices and add B-XOR siblings before selection | Compare complete low-confidence relative scores; transform only the selected plan |

For `BN` a power of two in `[32,1024]`, define
`physical_col = col ^ (col / 32)` and
`B_shared_index = q*BN + physical_col`. The high column-group bits affect only
the low five bits, so this is a bijection within each 32-column group. GMEM
indices, tensor ownership and host/device transfers do not change. This is
a **shared-memory representation change**, not a global tensor re-layout.
The VHO producer emits the selected mapping into ordinary WHIRL integer
address expressions; `whirl2c -cuda` only prints them. Legacy emission remains
an option-controlled parity baseline.

For each source warp issue, count unique physical words per bank and charge
`max(words_per_bank)-1` excess rounds. Sum separately for A/B fill stores and
A/B compute loads. Edge loads zero-fill all staging positions, so shared
work includes the full physical tile even when GMEM requests are predicated.
For these layouts changing `q` rotates all bank numbers equally; count one
compute `q`, then scale by `BK*ceil(K/BK)` and CTA count. B loads remain inside
the `i,j` loops, so their issue count includes `TM*TN`; do not assume future
backend hoisting or vectorization.

`COST_MODEL 5` replaces the former A-only bank approximation:

```text
compute = max(1, floor(2*M*N*K/1024)) + floor(smem_address_work/1024)
memory_unhidden = max(1, floor(worst_A_B_C_sector_bytes/1024))
                + floor(total_excess_bank_rounds/32)
                + floor(A_compute_warp_issues/32)
                + floor(B_compute_warp_issues/32)
sync = 2*ceil(M/BM)*ceil(N/BN)*ceil(K/BK)
smem_address_work = 2*(B_fill_warp_issues+B_compute_warp_issues) for XOR;
                    zero for compact B
```

The extra address term approximates the shift/divide and XOR. These are
dimensionless source-work proxies, not calibrated cycles or latency.
Copy base issue work, occupancy, instruction lowering, shared bandwidth,
cache behavior and overlap are not fully priced. Capacity and thread/resource
legality remain hard gates. Layout search is controlled by
`-Wb,-DSL:cuda_smem_layout_search=ON|OFF` (default ON); OFF suppresses the new
B-XOR alternatives, not shared analysis or existing A alternatives. TMA
sidecar planning stays compact-only until its distinct swizzle contract is
represented and lowered in WHIRL.

The raw `.t` uses the existing AI trace bit and adds a positional record:

```text
AI_OPT 7 TILE_SMEM site tile plan banks word_bytes b_layout a_bytes b_bytes a_copy_excess b_copy_excess a_read_excess b_read_excess b_read_issues address_work
```

The report tool must independently recompute these fields, verify the A/B
allocation sum, reject altered rows, and summarize each candidate's layout,
four conflict terms, issue/address work, complete cost, selection reason and
actual WHIRL action in `tile.log`. Preserve `.t`, input and device `.B/.T`,
WHIRL-derived `.cu`, legacy parity source, option-OFF comparison, receipts
and checksums. Compiler/host-CTA tests do not establish native GPU correctness
or performance. Later work must integrate stage depth, TMA swizzling,
issue/wait/reuse hazards and occupancy into the same explicit plan gate.
