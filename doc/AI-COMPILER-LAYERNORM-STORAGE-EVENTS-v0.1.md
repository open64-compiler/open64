# LN-REF-1 Storage And Event Planning

Version v0.1 | 2026-10-28 | Bounded native LN2 planning implemented; no selection or lowering

## 1. Scope And Ownership

Consume the explicit version-2 source, conditional target/environment and
numerical contracts established by [LN1-e](AI-COMPILER-LAYERNORM-INTEGRATED-BINDING-v0.1.md).
The new WOPT-owned `WOPT_DSL_bind_normalization_plan` API rederives active-PU
source and target meaning before checking provisional storage/event requests.
It does not alter source WHIRL, promote v1, choose a profitable plan, allocate
device memory, emit a kernel or discharge runtime obligations.

| Building Block | Owner / use |
| --- | --- |
| `opt_dsl_normalization_plan.h/.cxx` | Owning WOPT phase: exact source/target rebinding, plan legality, canonical ranges/leases, ownership/tails, capacity/banks and event reachability. |
| Existing normalization target API | Reuse exact source snapshot checks, strict FP/environment gates and architecture lookup. No duplicated source matcher or target binder. |
| `dsl_memory_hierarchy` | Reuse typed hardware profile, per-CTA SMEM tier, alignment/granularity and shared-bank model. Common metadata remains policy-free. |
| Existing residency/fetch IR | Architectural reuse point for later candidate integration. Current GEMM-oriented fetch stage records cannot prove ordered means, X/d generation lifetimes or complete invocation lease edges. No false structural-verification claim or mapped-row extension is added. |
| Native fixture and strict auditor | Real queries, source checkpoints, actual PREOPT, trace-on/off parity, independent range/resource/event recomputation and durable reports. |
| Runtime provider and selected lowerer | Future consumers must enforce the explicit obligations and materialize the selected WHIRL operations. No reduction API or runtime is linked by this milestone. |

Request plus returned binding describe a conditional plan. The binding alone
is not an executable authority token; a future selector/lowerer must retain
and revalidate the complete request against current source. Normal compilation
does not yet automatically invoke this planning API. No option, WHIRL layout,
operator encoding or emitter policy is changed.

## 2. Terms Before Use

| Term | Definition |
| --- | --- |
| Canonical allocation ID | Runtime-provided storage-object identity within a nonzero context and invocation contract, not a tensor value ID, symbol or sampled address. Distinct IDs promise disjoint backing. |
| Byte range | Half-open `[offset, offset+required_bytes)` within a declared allocation; checked without overflowing addition. |
| Immutable read lease | X/gamma/beta contents stay valid and unchanged through all invocation readers. Input ranges may overlap. |
| Exclusive output lease | Y has a unique writer per valid element and does not overlap any input range. |
| Physical layout contract | Contiguous row-major FP32 X/Y and contiguous FP32 affine vectors. It is explicitly supplied, not inferred from shape alone. |
| Row stride | Aligned physical size of one N-element scratch row, including optional trailing padding. One active row per CTA initially. |
| Shared arena | Scratch row at offset zero, mu at `stride`, q at `stride+4`; distinct four-byte scalar slots. Arena base must meet the common tier alignment. |
| Generation | A logical use of the same physical bytes. C2 stores original X as generation 0 and centered d as generation 1. |
| Requested / reserved SMEM | Arena bytes / requested bytes rounded to the common tier's allocation granularity. Logical death does not release part of a CTA allocation. |
| Completion / visibility | All relevant accesses have finished / intended consumers may observe their values. Issue alone establishes neither. |
| Reuse ready | All old-generation readers are complete, so an eventual new use may acquire the storage. It does not mean a reuse transformation occurred. |
| Conditional binding | The supplied contract is internally consistent. Actual allocations, leases, events, launch limits/registers and selected lowering remain unproved. |

## 3. Three Provisional Plans

All plans preserve LN-REF-1's ascending positive-zero statistics folds, exact
count, separate FP32 operations, sqrt-then-divide and exceptional-value policy.
There is one CTA per row and lane zero owns both statistics in source order.
The closed scalar mapping uses `k = threadIdx.x + j*threads_per_cta` for
cooperative work; no tree reduction, MMA or numerical reassociation is implied.

| Plan | Data path | Scratch / participation |
| --- | --- | --- |
| C0 direct | Owner scans X for mu, scans X for centered-square variance, then rereads X and recomputes d for epilogue. | No row/scalar SMEM arena; other CTA lanes idle. Scalar/RF/local allocation is not yet bound. |
| C1 retain d | Owner computes mean from X; valid lanes center X into d; owner reads d for variance; valid lanes read d for epilogue. | One padded FP32 d row plus separate shared mu/q slots. |
| C2 retain X then d | Valid lanes load X into row storage; owner computes mean; each lane reads its own old X before overwriting with d; owner computes variance; valid lanes perform epilogue. | One physical row, two nonconcurrent generations, plus separate shared mu/q slots. No second row allocation is charged. |

X, gamma, beta and Y require exact checked extents `4*R*N`, `4*N`, `4*N` and
`4*R*N`. All ranges have natural FP32 alignment and appropriate leases. The
same allocation ID must agree on capacity and base alignment. Y may share a
backing allocation only through a disjoint subrange; all output/input overlaps
refuse. Identical gamma/beta value IDs require the same backing ID and offset.
No in-place output, arbitrary strides or concurrent mutation is admitted.

Tail policy predicates before invalid accesses **and arithmetic**. Padding X
with zero then subtracting mu does not provide an identity contribution for
the variance. Count conversion remains exact N, never padded width. All CTA
lanes, including inactive tail lanes, participate in required barriers.

## 4. Capacity And Bank Model

For retained plans:

```text
row_bytes = 4*N
stride >= row_bytes; stride % common_shared_minimum_alignment == 0
requested = stride + 8                         // mu and q are distinct
reserved = round_up_checked(requested, common_shared_allocation_granularity)
planning_limit = min(common_shared_tier_capacity, hardware_resource_shared_budget)
require declared_shared_bytes == requested and reserved <= planning_limit
```

All arithmetic is checked for UINT64 overflow. The current four exact target
profiles declare a 65,536-byte planning budget and a 256-byte common-tier
reservation quantum; these are existing profile data, not new measured device
limits. Do not confuse the conservative budget with maximum opt-in SMEM or
the hardware resource record's separate SM allocation quantum. This milestone
does not change or calibrate either table. N=17 uses stride 80, requested 88
and reserved 256 bytes. N=2^24 remains a valid source count: C0 is conditionally
storage-eligible while full-row C1/C2 exceed this budget.

The bank model is read from the same common profile. Retained plans require a
four-byte bank word and use contiguous scalar per-lane loads/stores; the worst
different-address multiplicity is `ceil(min(N,warp_lanes)/bank_count)`. Current
profiles yield one. Owner-only statistics have no parallel bank conflict;
mu/q same-address reads are broadcasts, not different-address conflicts.
This describes the closed mapping only, not vector/swizzle layouts, bank
latency, occupancy, GMEM sectors, cache hits or measured service rate.

Scalar overlay, RF row retention/spills, multiple rows per CTA, chunked staging,
async/TMA delivery, distributed statistics and alternative work ownership are
separate extensions. They must not be encoded by metadata that bypasses this
request's closed contract.

## 5. Events, Scopes And Generation Safety

The request supplies immediate-successor masks over these fixed events:

```text
input_ready -> launch -> row_ready -> mean_done -> mu_visible
 -> centered_ready -> variance_done -> q_visible -> epilogue_done
 -> readers_done -> reuse_ready -> kernel_done -> result_visible
 -> input_release -> output_release
```

The verifier rejects unknown bits, self edges, cycles and any missing required
reachability. Extra transitive edges are allowed. This intentionally
conservative chain is not an enumeration of every possible legal schedule.

The row portion is instantiated for every CTA/row, not one shared cross-CTA
buffer. `kernel_done` joins all CTAs. Input readiness, launch, kernel completion,
result visibility and lease releases are invocation/runtime obligations; a CTA
barrier does not establish inter-CTA or host visibility.

| Boundary | Required meaning |
| --- | --- |
| Row ready | C2's cooperative X stores complete and are CTA-visible; logical readiness/no staging for C0/C1. |
| Mean done / mu visible | All ordered mean reads finish before original X may be overwritten; owner writes shared mu before the retained-plan visibility barrier. |
| Centered ready | Every valid lane reads its own old X and writes d; no other original-X reader remains. All d stores complete and become visible before variance reads. |
| Variance done / q visible | Owner completes bound variance/epsilon/sqrt/div sequence, then publishes q before epilogue consumers. |
| Readers done / reuse ready | Every d reader and epilogue writer completes before any future conflicting row generation; this is a reuse permission, not a physical deallocation. |
| Kernel done / result visible | All Y writes complete and provider establishes consumer visibility before lease release/publication. |

C1 requires uniform CTA barriers at mu-visible, centered-ready, q-visible and
readers-done. C2 additionally requires row-ready. C0 uses owner sequencing and
no CTA barriers/SMEM. The request must name exactly the required barrier mask
and all CTA participants. Future lowering must prove these properties in the
actual generated WHIRL, including no nonuniform early return. A declared event
mask by itself is not observed runtime enforcement.

## 6. Gates, Traces And Required Summaries

Gate order: native source/target, plan kind, nonzero runtime/context, physical
layout, ranges/leases, aliasing, owner/launch/tails, arena/capacity/banks,
barrier participants and DAG. Invalid scope/member/null calls leave output and
trace untouched. Valid refusals publish checked-prefix facts and zero runtime
obligations. Bound returns all six outstanding obligations: canonical storage,
live leases, event enforcement, launch conformance, register allocation and
selected lowering. The mask is not a grant of runtime authority.

Native `AI_NORM_PLAN`, `AI_NORM_RANGE` and `AI_NORM_EVENT` positional rows retain
the source line last and are paired with actual `AI_NORM_TARGET` queries.
`AI_NORM_HW` records the typed common profile; its last coordinate names the
fixture's model-print statement, not an input operation. Existing trace policy
remains caller-controlled; NULL-trace planning gives the same decisions.

| Report | What it summarizes |
| --- | --- |
| `alternatives.log` | Every C0/C1/C2 query, source/target join, refusal or conditional binding, validated prefix and remaining obligations. |
| `residency.log` | Canonical ranges/leases, row and scalar arena, reservation/budget, bank mapping and alignment/capacity refusals. |
| `fetch-pipeline.log` | Statistics order, C2 X/d generations, completion/visibility edges, required barrier participants, tail/reuse safety and event refusals; no async claim. |
| `optimization.log` | Full collection of successful planning reports, not a selected optimization or transformation. |
| `optimization-summary.log` | Coverage and bound/refused counts; runtime/launch/register/occupancy/async gaps; COST/DECISION/TRANSFORM none and v1 unchanged. |

Every `.log` starts `# What it summarizes:`. The independent auditor joins real
PREOPT source and target rows, recomputes every gate/resource result and event
closure, and refuses truncated/forged/narrowed records. Mapped before/after
planning images must be byte-identical, with full `ir_b2a -st -src` dumps.
Native fixtures also check table/symbol/type immutability, trace-off parity,
invalid-API nonpublication and actual PREOPT on/default/off controls.

## 7. Validation And Next Gates

The initial matrix has 32 cases: eight variants across all four exact targets,
including count-limit, mixed v1/v2 and shared affine inputs. It contains 324
native queries, 124 conditional bindings and 200 expected refusals, with 64
paired source checkpoints/dumps. It tests every event edge removal, cycles,
self/unknown edges, range overflow/alignment/leases, output overlap, canonical
metadata mismatch, owner/tails, capacity, partial barriers, stale source and
unsafe FP requests. Read-only input overlap, redundant edges and adjacent
output subranges have positive native witnesses. V1 never gains a plan.

Eleven new auditor tests complement the prior 85 oracle/join/trace tests.
All 32 cases also pass an isolated ASan/UBSan build of the new planner and
fixture. Legacy linked objects are not sanitizer-instrumented; leak detection
is disabled for process-lifetime compiler pools. Explicit zero allocator fill
keeps legacy mapped-image padding comparable (ASan's default poison otherwise
changes padding bytes). This is not uninitialized-read detection or GPU testing.
Planning performs no arithmetic transformation; the LN1-e numerical receipts
are retained/rechecked as separate evidence, not relabeled as GPU results.
Durable receipts must retain the rebuilt WOPT/backend driver, native fixtures,
sources, raw `.t`, source `.B`/`.T`, commands, hashes and independent final-path
replay before indexing.

Next LN2-d: export and enforce runtime-checkable ranges, leases, events,
actual device/grid limits and selected launch contracts. Then LN3 compares
complete lowerable plans with explicit costs, uncertainty and fallbacks.
LN4 materializes the chosen storage/access/synchronization in WHIRL and verifies
CUDA emission from that image. No free overlap, finite-only arithmetic repair,
automatic C0 selection or GPU certification follows from LN2 planning.
