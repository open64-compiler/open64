# Cooperative MLA lowering: warp reductions and shared staging

Version v0.3, November 5, 2026. Extends the preserved
[v0.2 fusion and split design](AI-COMPILER-TENSOR-KERNEL-LOWERING-v0.2.md)
and [native COMMENT contract](AI-COMPILER-TENSOR-SCHEDULE-SOURCE-v0.2.md).
Primary owner, successor, independent reviewer and approving lead: **TBD**.
Transfer acceptance remains open until those roles and independent review are assigned.

## Implemented behavior

The AOT focused lowerer can select a cooperative physical schedule explicitly.
Short context retains nine launches; long context retains twelve, including
three partial/merge pairs. Scores remain materialized across the maximum,
denominator and weighted-latent stages. This milestone improves the work inside
those boundaries; it does not implement one attention kernel or an async pipeline.

| Root | Cooperative schedule | Shared storage per CTA |
| --- | --- | ---: |
| Maximum and denominator sum | One output per full warp, four warps per CTA | 0 |
| Weighted latent matmul, full K at least 32 | Four neighboring output columns per CTA, one warp per column, K tiles of 32 | 640 bytes |
| Merge of cooperative partials | One output per full warp, lane-strided chunk traversal | 0 |
| Smaller contractions and result stores | Existing guarded thread-per-output schedule | 0 |

This is an explicit candidate, with the earlier v2 schedule retained as the
fallback. There is no measured universal length threshold. The driver requires
fused O3 and explicit floating-point reassociation permission even for an unsplit
short reduction. O0/O1/O2 keep their established semantic lowering. The selection
uses operator semantics and dimensions rather than model or symbol names.

## Warp algorithm and numerical contract

For an output i, chunk c covers [b,e) = [cW,min((c+1)W,K)). For a direct reduction,
b=0 and e=K. Lane l accumulates indices b+l, b+l+32, ... below e. Initialize sums
and dots at +0, maxima at negative infinity. The lane-local loop retains F32
operations. All 32 lanes then execute shuffle-down steps with offsets
16,8,4,2,1 and mask 0xffffffff. At offset d, lane l combines the source value from
l+d only if l+d<32. Lane zero alone writes the output. Invalid output warps carry
neutral values and still execute every shuffle. No lane exits before a collective.

A maximum combines x with y by selecting y when y is NaN or y>x, otherwise x.
This preserves NaN propagation, without promising a particular NaN payload or
signed-zero choice under reassociation. Sum/dot grouping differs from the earlier
ascending scalar loop. Correctness means agreement with the independent FP64
reference within rtol=2e-5 and atol=2e-6, plus exact required infinity/masking
behavior. Byte equality is checked between the new review executor and the new
mapped host execution, not between old and new reduction groupings.

Long-context scratch remains partial[c*N+i]. A separate, ordered kernel launch
waits for all partials; within the merge, lanes traverse c=l,l+32,... and use the
same warp tree. Maximum merges still precede denominator partials, denominator
merges still precede normalized weighted-latent partials, and the latent merge
precedes the value projection. There is no chunk-local substitute for the global
normalization. Fully masked finite-input rows retain output zero and LSE -infinity.

## Shared tile and ownership

Let the output matrix have n columns. A CTA owns one matrix row and a four-column
tile, with columns j0+warp. There are ceil(n/4) tiles per row; stage-one split
CTAs also carry chunk c. All 128 threads have identical token-loop bounds.

For each K tile of 32 positions:

1. The first warp evaluates 32 left-operand values, including any fused
   probability expression, and writes shared A[32]. This expression is evaluated
   once per token per four-column tile, instead of once per output column.
2. All threads cooperatively load B for token k=thread/4 and column=thread%4.
   Adjacent threads load adjacent logical columns. Write the selected swizzled
   shared index 4*k + (column XOR floor(k/8)) into B[128]. Invalid token/column
   slots are zero-filled without reading their source address.
3. An unconditional CTA barrier publishes both arrays. Warp w reads A[lane]
   and B[4*lane+(w XOR floor(lane/8))], and accumulates its product if valid.
4. A second unconditional CTA barrier completes readers before overwriting
   either array. After all tiles, the full-warp reduction writes lane zero.

Both arrays are F32, aligned to 16 bytes. Total static shared memory is
(32+128)*4=640 bytes. On a 32-bank F32 memory layout, both the producer warp's
stores and each consumer warp's reads map to distinct banks: the XOR uses the
upper two bits of the local K coordinate to permute the column bank bits.
The selected index expression is explicit WHIRL, rather than an emitter heuristic.
No asynchronous copies, double buffering, tensor instructions, or overlap claim
is made. Inactive column warps still reach both barriers and every shuffle.

## Native representation, scope and phase order

The existing PU-local VHO tensor service constructs the physical schedule. Its
current standalone driver establishes fresh device PUs and invokes it for one
active PU at a time. The source tensor image is read-only. This is an incremental
physical-lowering implementation; general be_driver/LNO schedule integration
remains a separate milestone under the existing architectural plan.

The focused phase order is:

1. Reopen and verify source tensor WHIRL; admit the closed operator, shape,
   layout, dtype, effect and source-mapping contracts without mutation.
2. Select the existing fused O3 DAG, optional split width, and explicit cooperative
   schedule. Seal the detached recipe with reassociation permission and identity.
3. Establish the device translation unit and empty kernel PUs. Capture the native
   before checkpoint immediately before executable materialization.
4. Materialize ordinary DO_LOOP/ARRAY, guarded scalar operations, CUDA coordinates,
   disjoint partial stores, shared arrays and typed collective CALLs in each PU.
5. Validate kernel body, prototype, argument/storage/launch joins and resource
   metadata. Reject native DSL remnants. No subsequent optimizing pass may move
   calls, change participation, or infer independence across shared accesses.
6. Insert canonical explanatory COMMENT statements; validate again. Capture the
   native after checkpoint. An additional device-explain boundary captures the
   same executable body immediately before and after COMMENT insertion.
7. Reopen each image independently with ir_b2a -st -src. Re-audit the final image;
   obtain launch metadata from that image. Emit CUDA through normal whirl2c.
8. Compile the AOT CUDA and launch in recorded stream order after checking grid,
   block and shared-memory resource limits. Validate numerical results.

Shared arrays reuse KIND_ARRAY, constant ARB bounds, SCLASS_AUTO and the existing
DEVICE_SYMBOL_SHARED attribute. Collectives reuse typed ordinary external CALLs
with DEVICE_SYMBOL_FUNCTION. Shuffle prototype is F4(U4,F4,U4,I4); barrier is
V(void). Shuffle CALLs are immediately followed by a simple STID of the return
preg -1, following WHIRL.pdf's high-level return protocol. CALL effects remain
conservative. There is no new opcode, binary layout, type kind, ELF section,
frontend dependency, backend-library dependency or CUDA-emitter extension.

The arithmetic-only readable source executes the same selected lane/tree grouping
serially; its helper does not simulate GPU parallelism. Actual shared storage and
collectives exist in the mapped WHIRL and normal whirl2c CUDA. Both source views
receive the same explanatory payloads. COMMENT insertion alone must leave emitted
executable tokens identical. Shared expressions are explained as CTA-local reuse,
not as globally materialized tensors.

## Interface and invalidation

The closed detached recipe schema v3 adds cooperative=true and requires
reassociation=allow. v1/v2 continue to decode without cooperative selection. The
new command is:

```sh
tensor_kernel_lower cooperate fused.json cooperative.json --allow-reassociate
tensor_kernel_lower device cooperative.json device.before.B device.after.B
tensor_kernel_lower audit device.after.B cooperative.json launch.json
tensor_kernel_lower schedule-source cooperative.json optimized.cu
```

The audited launch adds mapping (0 thread, 1 warp, 2 staged) and shared_bytes.
Geometry is computed once by the lowering service and reused by verification
and explanation. For mapping 1 grid=ceil(work_items/4). For mapping 2 grid equals
rows*ceil(n/4), multiplied by chunk count for partials. Every CTA has 128 threads.

Changing dimensions, split width, mapping, numerical permission, producer DAG,
builtin types, bounds or symbol attributes invalidates the recipe/body/launch
join. Rebuild physical WHIRL, explanations, launch manifest and generated CUDA.
The body fingerprint includes call identity/flags/operands, shared address space,
alignment, array element type and constant bounds, in addition to existing
executable structure and COMMENT text. It detects stale or mutated products;
it is not a security signature or a substitute for compiler semantic tests.
Changed/missing COMMENTS fail the existing canonical explanation check. Older
annotated receipts retain their own producer/auditor for canonical-text replay.

## Validation, evidence and remaining gates

The deliverable retains source inputs, saved independent FP64 oracles, v2 baseline,
v3 recipes, native before/after images and matching source-interleaved traces,
raw diffs, COMMENT-only snapshots, generated readable and mapped CUDA, actual
build tools, compiler logs, tests and checksums. Baseline-to-cooperative diffs
compare separately constructed schedules; device.before/after are the actual
empty-PU-to-materialized checkpoints. Do not confuse those two evidence types.

Focused cases cover both MLA lengths (127 and 8193), seven masking/score scenarios
each, K=65 and n=5 with unequal broadcast batches, split W=33 with an uneven tail,
33-wide reductions and inactive output warps, NaN maximum, reversed CTA/thread
startup order, guard bytes and unchanged inputs. Negative controls change masks,
collective prototypes, a barrier, shared bounds, resource metadata and COMMENTS;
all must reject publication. Lower-level and missing-permission cases must reject
cooperative selection before producing device checkpoints.

The synchronized host simulator runs 128 persistent threads per kernel, with
per-warp exchange barriers, CTA barriers and bounded divergence diagnostics. It
checks concurrency semantics on the host; it does not model GPU scheduling or
latency. Both source views are compiled by NVCC for sm_90, and ptxas resource logs
are retained. Device execution is attempted only when the CUDA probe succeeds.
A machine without a usable CUDA device leaves hardware correctness, racecheck,
occupancy, effective bandwidth and latency acceptance outstanding.

Risks are increased CTA counts and synchronization overhead for small reductions,
continued score traffic and producer recomputation, resource/occupancy effects,
F32 reassociation, and incomplete integration with general compiler scheduling.
Before enabling a default runtime policy, measure v2 and v3 end-to-end on the
same GPU/workloads, including launch overhead and all merges. Keep v2 when v3
loses. AOT remains the baseline; JIT is not required for this selection.

CUDA participation and synchronization follow NVIDIA's
[CUDA 12.8 C++ Programming Guide](https://docs.nvidia.com/cuda/archive/12.8.0/cuda-c-programming-guide/index.html):
all lanes named by a shuffle mask participate, and shuffle exchange is not a
shared-memory publication barrier. The two explicit CTA barriers carry that role.
