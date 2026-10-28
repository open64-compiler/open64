# MMA5-H: Bounded Hopper Pipeline And Joint Candidates

## Scope And Defaults

Start from merged MMA4-H PR198, updated `develop` at `ab3afb7d`. VHO owns an
explicit O3 active-PU API, not a new production option. The verified MMA3
F16/BF16 source and existing F32 behavior remain default SIMT. Target timing
remains **unknown**. Synthetic diagnostic ranking never authorizes executable
selection, including a non-overlapping cost envelope.

The implemented increment is a bounded two-slot **batch pipeline**: one
128-thread consumer warpgroup, exact `sm_90a`, 64x64x16 shared/shared atoms,
FP32 accumulator/output, M/N multiples of 64 and positive K divisible by 16.
MMA4 numerical-acceptance freedoms, source identity, alias/storage,
canonical-source verification and lower-level rejection rules remain gates.
There is no F32 narrowing, general frontend hookup or runtime dispatch.

Alternatives are original one-slot cooperative MMA4, two-slot cooperative
packing, and two-slot **one-dimensional bulk TMA** with explicit raw-to-packed
conversion. This is not a tensor-map/transpose recipe: no host tensor-map
descriptor or new kernel formal ABI is required. The old F32 tensor-map CUDA
template is not reused as a second executable source of truth.

This finite-group schedule does not overlap delivery with compute across
batches, promise latency hiding, or keep the fragment persistently in registers
across the K loop. Those remain future refinements, not inferred by `whirl2c`
or the model. Both delivery alternatives and actual two-group compute are
implemented; arbitrary-depth and producer/consumer specialization are not.

## Actual Storage And Protocol

| Choice | Shared storage | Compute and private storage |
| --- | --- | --- |
| MMA4 cooperative | 4096 bytes | One fully waited atom; 32 private F4 values per thread. |
| Two-slot cooperative | 8192 bytes | Two 2048-byte packed A slots and two B slots; up to two committed groups use the same accumulator registers inside one ASM carrier. |
| Two-slot bulk TMA | 16400 bytes | Above packed slots plus 8192 raw bytes and two eight-byte barriers. Per-slot raw A is 64x16 row-major; raw B is 16x64 row-major. |

Each batch covers up to two K steps:

1. Cooperative delivery writes packed slots using ordinary U2 loads/stores.
   TMA elects thread zero to initialize active barriers, proxy-publish their
   initialization, expect 4096 bytes per slot, and issue 64 aligned 32-byte A
   row copies plus 16 aligned 128-byte B copies. Both slots are issued before
   either is waited. The absent odd-tail slot is never copied or consumed.
2. The TMA issuer polls acquire phase-zero completion, invalidates each
   completed barrier, and joins the CTA before raw reads/repacking. A barrier
   is reinitialized only after its prior generation completed and was
   invalidated. **TMA readiness is not MMA completion.**
3. Every thread proxy-publishes packed writes and joins the CTA. The MMA4
   K-major packing, LBO=128, SBO=256 and no-swizzle fragment mapping are
   unchanged. Slot one starts 2048 bytes after slot zero in each allocation.
4. One volatile/memory-clobber ASM carrier loads the private fragment, performs
   the register fence, and issues MMA/commit for each active slot. The same
   32 F32 PTX accumulators carry both dependent updates. There are at most two
   committed compute groups. A uniform one-group branch handles odd tails.
5. Actual `wgmma.wait_group.sync.aligned 0` precedes every fragment store.
   A subsequent CTA reader join precedes packed-slot overwrite, next raw copy
   or accumulator reset. The epilogue preserves unique FP32 output ownership.

Wrong block dimensions, excess X/Y blocks, nonunit grid Z and unaligned inputs
uniformly perform no collective or output work. The caller chooses the retained
SIMT kernel **before launch** if target, policy, shape, storage, resource or
launch obligations do not hold. An invalid launch is not a successful result
or a hidden in-kernel fallback.

Bulk `.shared::cta.global` additionally requires PTX 8.6 and CUDA 12.8.
The retained CUDA 12.8.1 compiler checks both formats; generic `sm_90` is not
admitted as WGMMA support. Mechanism reference: NVIDIA [PTX 8.7 bulk-copy and
mbarrier rules](https://docs.nvidia.com/cuda/archive/12.8.1/parallel-thread-execution/index.html#data-movement-and-conversion-instructions-cp-async-bulk).

## WHIRL Continuity And Verification

Reuse MMA4's ASM constructor, exact-tree comparison, geometry, packing,
fragment mapping, source positions and scalar control-flow builders.
`dsl_wgmma_pipeline_opt.h` exposes runtime-only request/binding/result records.
No new opcode, intrinsic number, type kind, ELF section or binary image layout
is added. WHIRL owns every literal copy, barrier, compute and wait; the existing
CUDA printer only emits it.

Tree ownership, direct kids, effect completeness and structured BLOCK/DO/IF
rules remain intact. `WHIRL.pdf` is absent from this checkout; review uses the
[Open64 WHIRL specification](https://www.mcs.anl.gov/OpenAD/open64A.pdf) and
actual current constructors, never historical ABI sizes. Postchecks cover
exact instructions, all operands/effects, byte-scaled addresses, list links,
generation/acceptance and all guard/packing expressions. Shared WN children
reject. Verification creates no symbol records; an inspection comment is
identity evidence, not an executable action.

Materialization verifies borrowed SIMT before constructing detached work,
postchecks the complete new tree, then publishes once. Rejection leaves the
source and caller output unchanged. Launch/dispatch remains caller-owned;
there are no IPA facts or cross-PU mutations.

## Complete Conditional Model

Reuse MMA2's explicit ranged `DSL_COMPUTE_MODEL_RECORD`, checked arithmetic,
and `DSL_gemm_occupancy`, not peak-rate guesses or another occupancy service.
Four alternatives share one accepted typed source: actual MMA3 GRID SIMT,
one-slot MMA4, two-slot cooperative, and two-slot bulk TMA. Their launch grids
differ as their bodies require; compare complete same-result invocations,
not one SIMT CTA with one 64x64 MMA CTA.

Register reservation is caller-supplied per thread (at least 256 bytes),
including fragment, epilogue, addressing, descriptors and copy temporaries.
Tests use a conservative 384 bytes, not a measured allocation claim. Round RF
and SMEM using the existing ledger, configured residency caps and finite
compute-group capacity. TMEM is zero. Insufficient resources are ineligible;
absent resource description is unknown, not infinite capacity.

The conditional schedule serializes shared, compute, issue, synchronization,
latency and preparation services within each SM. Global traffic shares one
device bandwidth. No per-CTA bandwidth multiplication, latency hiding or
memory/compute overlap is assumed; result latency is charged per atom.
Conservative private-fragment traffic is charged to device/local service,
not shared: 32768 bytes per batch plus initialization/final reads. TMA includes
raw writes/reads, repacking, all 80 issues per atom and readiness joins.
Preparation duration includes descriptor/barrier lifecycle and non-bandwidth
address/packing overheads. SIMT includes per-output loads, both source
conversions and separately rounded FP32 work. Partial columns, residency
rounds, epilogue and launch are included.

Eight terms reconcile exactly: device traffic, shared, compute, issue, CTA
sync, exposed latency, preparation/epilogue, invocation launch. Corners follow
MMA2's reversed-rate convention. Missing any model input yields `known=0`;
zero arrays are unavailable storage, **not zero-cost estimates**. Malformed
ranges, overflow or bounded-analysis-budget failure preserves output. These
budgets bound analysis, not native supported problem size.

Nominal ranking is diagnostic. A bounded candidate's upper cost must be
strictly below every eligible alternative's lower cost. Candidate zero means
retain SIMT, also for missing/overlapping evidence. `emitted_family` is always
SIMT, even with separated synthetic bounds. Explicit materialization is an
independent caller request, not automatic model-based promotion.

## Validation And Remaining Gates

The review runner retains actual before/after `.B`, separate `ir_b2a -st -src`
`.T`, raw diffs, CUDA/PTX/cubin, compiler allocation reports, commands and five
phase logs. Host checks execute emitted guards, addressing, repacking and
epilogue against independent dense original buffers. Only audited assembly is
replaced by explicit test-only deferred-copy/completed-atom models; this is not
GPU or ISA arithmetic simulation. Python independently recomputes complete
cost/occupancy/ranking corners. Event removal tests distinguish raw readiness,
async readers, finite groups, result access and reuse.

The final producer staging run is retained at
`/private/tmp/open64-mma5-h-review-20261027/acceptance-verified/`; the durable
receipt below is the authoritative accepted copy. It covers 20
F16/BF16 and cooperative/TMA shape-delivery cases, 40 before/after native
CUDA kernels, 80 host geometry/protocol runs, 1100 preflight negatives, 2474
tree mutations, 210 independent emitted-source corruptions and 660 independent
model cost corners. The model stress also passes 216 scenarios and 1728 cost
corners. The source-interleaved TMA example
`delivery-2-format-5-one/hopper.after.T` contains the real bulk copies, the
CTA-scoped acquire wait, and the WGMMA carrier; its emitted PTX compiles with
CUDA 12.8.1 for `sm_90a`. An earlier rejected PTX spelling and assembler log
remain at `/private/tmp/open64-mma5-h-review-20261027/acceptance-final/` for
diagnostic history; the accepted receipt above is from the corrected rerun.

Native GPU arithmetic, runtime launch/resource behavior, calibration and
kernel/invocation timing remain **unverified/unknown**. No performance or
absence-of-local-memory claim follows. Existing full-builder/FHE gaps are not
repaired or waived. MMA4-B, generalized cross-batch overlap and MMA6 automatic
promotion remain separate work. Local commit only; no push or PR requested.

## Durable Acceptance Record

Implementation checkpoint `913eeb6222033437a70cf89bcf3b3bef5890d8e1` is based on
merged PR198, `ab3afb7d`. The primary immutable receipt is:

`/Users/shinmingliu/open64/artifacts/review/ai-optimization/mma5-h/hopper-pipeline/sha256-918b858dd003233d329f71c90bc3b6fe1444826414d06dfd4579eb77ba727419/`

It retains 3000 authenticated files: accepted pipeline artifacts, exact
committed source snapshots, producer/reader/emitter/backend binaries,
NVIDIA-checksummed CUDA 12.8.1 archives, commands, phase reports and logs.
Fresh compatibility runs cover fifteen F32, eighteen typed SIMT and ten MMA4
configurations. All 86 full `ir_b2a -st -src` dumps match the authenticated
MMA4 receipt; CUDA bodies match except translator dates, and F32
cost/selection/compute/shadow rows remain identical. Raw image byte differences
are retained without normalization or byte-equality claims. The typed suite
passes 88 execution runs; the original MMA4 suite passes forty host checks and
twenty CUDA compiles. Backend dependencies remain the same five libraries;
frontend-builder and JsonCpp symbols remain absent from shared consumers.

- [Acceptance summary](/Users/shinmingliu/open64/artifacts/review/ai-optimization/mma5-h/hopper-pipeline/sha256-918b858dd003233d329f71c90bc3b6fe1444826414d06dfd4579eb77ba727419/summary.log)
- [BF16 TMA odd-tail source-interleaved WHIRL](/Users/shinmingliu/open64/artifacts/review/ai-optimization/mma5-h/hopper-pipeline/sha256-918b858dd003233d329f71c90bc3b6fe1444826414d06dfd4579eb77ba727419/acceptance/pipeline/delivery-2-format-6-odd/hopper.after.T)
- [Default and MMA4 compatibility report](/Users/shinmingliu/open64/artifacts/review/ai-optimization/mma5-h/hopper-pipeline/sha256-918b858dd003233d329f71c90bc3b6fe1444826414d06dfd4579eb77ba727419/acceptance/compatibility/report.json)

The independently published replay receipt is:

`/Users/shinmingliu/open64/artifacts/review/ai-optimization/mma5-h/reopen/sha256-3fdf625491fd85825dd51628683cf94f100c104023eb584d860dcc3ea7e849db/`

It authenticates the primary files, reaudits all twenty pipeline cases and 660
model corners, reruns copied producers for F16 cooperative and BF16 TMA
odd-tail cases, and obtains four byte-identical full dumps through both current
and prior readers. Four CUDA kernels recompile and four host geometry/protocol
checks pass. Networking is disabled; the original build and working source
tree are not mounted. The retained C++ fixture snapshot must be mounted at
`/open64` to honor the DST source pathname. The replay command records that
alias. A prior incomplete invocation and its missing-source diagnostic remain
retained; no accepted dump omits source interleave. Final-path verification
authenticates 3066 primary/replay files and reopens four images identically.

[Independent replay verification](/Users/shinmingliu/open64/artifacts/review/ai-optimization/mma5-h/reopen/sha256-3fdf625491fd85825dd51628683cf94f100c104023eb584d860dcc3ea7e849db/verification.json)

The primary checkout's local `artifacts/review/INDEX.md` and `index.json` index
both accepted receipts and the retained earlier candidate. No generated
artifact is added to Git. GPU arithmetic, runtime resources, native timing,
automatic promotion and cross-batch overlap remain outside this acceptance.
