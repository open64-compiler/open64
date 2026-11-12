# Executable native tensor lowering for absorbed MLA

Version v0.1, November 5 2026. Primary owner, successor, independent reviewer
and approving lead: TBD. This is the first executable AOT baseline following
[reference CUDA](AI-COMPILER-TENSOR-SOURCE-v0.1.md) and the
[native tensor expression contract](AI-COMPILER-NATIVE-TENSOR-EXPRESSION-CONTRACT-v0.1.md).

## Outcome and acceptance boundary

The focused native driver consumes an actual source `.B`, prepares a bounded
PU-local recipe, then invokes a VHO-owned service in fresh driver-created
device PUs. Those PUs contain ordinary scalar WHIRL, DO_LOOP, OPR_ARRAY,
loads/stores and pure math intrinsics. Existing `whirl2c -cuda` translates the
mapped device `.B` to executable `__global__` CUDA kernels. No attention
algorithm is implemented in a CUDA template or inferred in the emitter.

For the absorbed MLA fixtures, 46 source operations and two result copies
become 34 kernels and 14 shape-only views. Both L=127 and L=8193 are compiled
statically. This baseline uses the same generic operation decomposition for
both sizes; their compile-time dimensions, loop bounds, grid sizes and storage
requirements differ. It does not introduce a measured context crossover,
runtime multiversion dispatcher, short-context fusion or long-context split
softmax. AOT remains the production direction.

The initial implementation is an explicit focused pipeline, not automatic
`be_driver` selection. No whole-backend rebuild or universal tensor-lowering
coverage is claimed. The reusable service lives in `be/vho`; the focused driver
alone owns JSON, frontend PU creation, snapshots and file-level iteration.
There is no new JsonCpp or `DSL_Builder_*` dependency in `be.so`.

## IR and phase ownership

1. Source admission requires one exact active native source PU. The normal
   gatekeeper validates native expression contracts. Preparation copies
   types, shapes, operation versions, direct operands, attributes and source
   positions without creating or mutating WN, ST or TY entries.
2. The detached recipe has no WHIRL pointers or live analysis references. Its
   canonical checksum detects stale/corrupted handoffs; the checksum is not a
   security signature. The retained source-image SHA256 supplies artifact
   identity. Changing the source, descriptors, numerical policy, optimization
   level or mapping requires fresh preparation and a new evidence cohort.
3. The driver establishes a fresh device translation unit and empty kernel PUs.
   It retains their before image using the normal writer in an isolated child.
   Writing consumes subsection state, so an ordinary parent write would not be
   a valid reusable checkpoint. The VHO service never switches PU scope.
4. Semantic materialization creates explicit scalar accesses and ordered
   contractions/reductions. Rank/axis arithmetic is represented by ordinary
   integer WHIRL; dense flat addressing uses explicit 1-D OPR_ARRAY nodes.
   Every expression has one parent. Original source statements remain in the
   preserved provenance image; they are not falsely reported as rewritten.
5. At O0/O1/O2, each operation has one thread and a serial output DO_LOOP.
   At O3, explicit baseline mapping assigns one independent output element to
   each thread in 128-thread blocks. Each contraction/reduction still has an
   ascending serial inner loop. The independence proof is the admitted pure
   operation's disjoint output elements, fresh output allocation and read-only
   operands; there is no floating-point reduction reassociation.
6. The driver publishes the device after image. An independent native process
   reopens it, verifies the actual body, prototype, source binding, argument
   joins and launch metadata, then derives the launch manifest from that image.
   `whirl2c` prints the selected executable meaning without selecting a plan.

This focused service does not consume or preserve PREOPT SSA, LNO dependence,
alias, feedback or cost analyses from the source PU. Source analyses remain
valid because the source is unchanged. Newly created device PUs have no such
analyses; a later integrated pipeline must build their applicable analyses
before optimization. Future fusion, scheduling, tiling or staging must use
their owning High-WHIRL/PREOPT/LNO contracts and invalidate affected facts.

The long-term ordering remains Very High domain expansion; High common
algorithmic IR, canonical loops and array indexing; dependence analysis and
Wolf/Cohen scheduling; multi-level tiling, SMEM staging and prefetch loops;
Mid structured memory lowering; Low GPU mapping; CUDA/PTX emission. This
baseline establishes executable primitive semantics and an explicit element
mapping, without claiming those later optimization phases are complete.

## Admitted source and numerical policy

The service accepts the reference lane's F32/bool operators, exact versions and
attributes: reshape/transpose v1; numpy-broadcast matmul v3; add/sub/mul/div,
ordered greater, select, exp/log, isfinite, logical_not and fill_like v1;
last-axis reduce_sum v3 and reduce_max v1. Reduction keepdim is explicit.
Rank is 1–16, extents are positive, and each value has at most 16M elements.
The interface is static, dense logical/row-major storage. Explicit strides,
other physical layouts, state, calls, nested PUs, control flow, mutation,
redefinition, unsupported operators/versions and unknown attributes refuse.
Additional tensor traits, sharding, quantization, memory/placement contracts or
other representation fields are rejected rather than silently discarded.
Only established identity, dense layout, alias facts and provenance fields are
admitted by this initial capability boundary. Bool inputs use canonical 0/1 bytes.

Shape views alias the same dense allocation because this pure subset exposes
no writes to inputs, views or already-defined temporaries. A final result copy
preserves the caller's separate output allocation. Transposes materialize a
new logical dense value. Bool values use U1 bytes, not a guessed CUDA C++
object representation. No restrict qualifier or physical input alias promise
is inferred. The supplied adapter allocates all canonical buffers separately.

FP32 accumulation order remains explicit; no FMA, FTZ, fast math or split
reduction is authorized. Review compilation uses `--fmad=false --ftz=false
--prec-div=true --prec-sqrt=true`. Exp/log retain source_unresolved and are
checked numerically against the independent expanded FP64 oracle, not promised
bitwise equal across host libm, PyTorch and CUDA. Maximum propagates NaNs;
isfinite uses ordered abs(x)<=FLT_MAX. MLA masking and all-masked behavior stay
in the actual graph: zero output and negative-infinity LSE.

The CUDA emitter adds only target spelling: F4EXP/F4LOG map to expf/logf,
F4 ABS maps to fabsf, and nonfinite F32 TCON values print a binary32
interpretation expression. Ordinary C and finite constants retain their
existing paths. These are printer capabilities, not new WHIRL opcodes,
math-policy choices, graph fusions or binary-layout changes.

## Launch ABI and execution

Each device kernel has ordinary by-value pointer formals, one per operand and
one for its result. Device names are qualified by the sealed specialization so
short/long and optimization-level variants can coexist in an AOT module.
Index and address arithmetic use 64-bit integers. Selected
grid/block, operation ordinal/version/node, and complete argument storage
bindings are persisted on the globally unique owning kernel ST. Argument keys
include the formal ordinal. Raw local ST_IDX values repeat across PUs and are
therefore unsuitable as process-wide argument-metadata identities.

The reopened image supplies the manifest's kernel order, dimensions, dtype,
canonical allocation ID, input/output ordinal and byte counts. The source
recipe is used to validate those facts, not to invent missing device facts.
Body fingerprints detect altered bounds, arithmetic or accesses. They detect
corruption and stale products; they do not prove correctness or authentication.

The generated host adapter contains allocation, transfer, launch and result IO
only. It launches all operations in recorded order on the same CUDA stream,
checks launch errors and completion, and queries actual thread/grid limits.
No inter-block synchronization is needed within a kernel. Completion of each
prior stream operation establishes the next operation's input visibility.
Input allocations remain read-only; 16-byte buffer canaries detect out-of-range
writes in the review adapter. The unoptimized baseline retains all temporary
allocations and does not reuse lifetimes or stage shared memory.

## Commands and reproducible evidence

```sh
make -C "$BUILD/osprey/targdir/ir_tools" tensor_kernel_lower SKIP_DEP_BUILD=1
tensor_kernel_lower prepare mla.B recipe.json --verify 3
tensor_kernel_lower device recipe.json device.before.B device.after.B
ir_b2a -st -src device.before.B device.before.T
ir_b2a -st -src device.after.B device.after.T
tensor_kernel_lower audit device.after.B recipe.json launch.json
whirl2c -cuda -TARG:abi=n64 -O0 \
  -CLIST:dotc_file=kernels.cu -CLIST:doth_file=kernels.h device.after.B
nvcc -std=c++17 -arch=sm_90 --fmad=false --ftz=false \
  --prec-div=true --prec-sqrt=true -cubin kernels.cu -o kernels.cubin
```

`mla_kernel_review.py` performs this chain for short/long and O0/O3, retains
native before/after `.B`, matching independent `.T`, raw unified differences,
source image/dumps, recipes, audited manifests, unmodified `.cu`, host/GPU
adapter sources, cubins, linked launchers, commands, diagnostics, reports and
checksums. Host simulation substitutes only CUDA coordinate/qualifier support;
kernel arithmetic is the unchanged emitted source. It is not GPU execution.
The same packed inputs can be passed to the linked CUDA launcher on a GPU host.

`validate_mla_gpu.py` executes all 12 retained cases with Python's standard
library, the linked launchers and saved FP64 oracle JSON files. On the GPU host:

```sh
python3 validate_mla_gpu.py --artifact-dir /path/to/review \
  --output-dir /path/to/fresh-gpu-validation
```

It checks byte counts, finite tolerances and infinity signs, and records kernel
errors and results. Unavailable devices return status GPU_UNAVAILABLE and exit
77; a failed launch/result returns FAIL. Neither is converted to a GPU pass.

`test_tensor_kernel_lower.py` exercises malformed dimensions, dtypes, versions,
attributes, aliases, use-before-definition, stale launch/recipe identities,
duplicate JSON keys, trailing content, unsupported source/layouts, no-overwrite,
ordinary unrelated programs, O0/O1/O2/O3 boundaries, byte booleans, unequal
batch broadcasting and NaN maximum. Device construction also checks rejected
zero source positions, unsafe FP controls and nonempty destinations without
mutating tables. Reopened-body and binding mutations must reject publication.

## Milestones and risks

The implemented milestone is baseline native semantic lowering, explicit
element mapping, mapped-device auditing and source emission. Compiler/host
verification and CUDA compilation are separate acceptance lanes from actual
device execution. The local runtime probe reports no usable CUDA device; GPU
numerical acceptance and timings remain open until a GPU host executes the
retained launchers. Named ownership and independent review remain TBD.

Certification passes 21 focused tests, 12 direct FP64-oracle host cases and
the same 12 cases under scoped address/undefined-behavior sanitizers. The four
short/long O0/O3 specializations compile to sm_90 cubins and linked launchers.
The two O3 variants also compile together as a 68-entry AOT CUDA module.
Each reopened after image has 34 device PUs and 81 ARRAY addresses; O0 has
41 DO_LOOP nodes and O3 has seven ordered contraction/reduction loops. Input
images remain unchanged. The older authenticated LayerNorm kernel retains
identical CUDA text apart from the timestamp and compiles with the rebuilt
printer. Backend binaries remain byte-identical to the prior receipt.

The main performance limits are 34 launches, full intermediate traffic,
materialized transposes, and serial per-output reductions/contractions. Long
context increases output-element concurrency but still leaves the softmax
row reduction and latent accumulation serial within each output. These are
explicit targets for the next High-WHIRL optimization milestones: fuse legal
short paths, select and verify long-context split/merge attention under a new
numerical contract, then add tiling/staging and calibrated resource planning.
No speedup, dispatch crossover, occupancy benefit or production-model accuracy
is inferred from this baseline's correctness and compilation checks.
