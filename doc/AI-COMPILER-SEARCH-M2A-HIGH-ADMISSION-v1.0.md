# M2a common High algorithm and canonical admission v1.0

November 7, 2026 | Implemented native-artifact entry; focused validation passed

Authority: [search methodology](AI-COMPILER-OPTIMIZATION-SEARCH-METHODOLOGY-v1.0.md)
and [M1 boundary audit](AI-COMPILER-SEARCH-M1-DRIVER-LNO-AUDIT-v1.0.md). This slice
provides a faithful common High WHIRL algorithm that the ordinary backend can
read, canonicalize with PREOPT and admit with LNO's actual analyses. It does
not implement the owned M2b snapshot, search or M4 selected-path isolation.

## Entry and fallback boundary

The explicit native path is source common-operator B -> existing prepare recipe
-> semantic High B -> ordinary backend/PREOPT/LNO. The new standalone `high`
command uses a validated unoptimized recipe and constructs one empty host PU
owned by the driver. It never rewrites another PU or changes the source tensor
function's ABI. Normal DSL runtime/provider lowering and all existing device,
fusion and search commands remain the default routes. Automatic selection of
this High route inside general VHO/runtime/provider orchestration is future
driver integration, not claimed by this bounded native-artifact entry.

```sh
tensor_kernel_lower prepare mla.B mla.recipe.json --verify 3
tensor_kernel_lower high mla.recipe.json mla.before.B mla.after.B mla.buffers.json
ir_b2a -st -src mla.before.B mla.before.T
ir_b2a -st -src mla.after.B mla.after.T
be -fB,mla.after.B -fo,mla.o -ft,mla.phase.t \
  -PHASE:l=on:w=off:c=off:lpath=LNO_DIRECTORY:wpath=WOPT_DIRECTORY \
  -O3 -DSL:partition_axis=on -tr25 -tt25:1 \
  -OPT:IEEE_arith=3:roundoff=0 SOURCE_PATH
```

Outputs require fresh paths. Physical/fused/selected recipes are rejected before
native construction; use the original common-operator source/recipe instead.
Existing malformed-source rejection still applies. Failed High preflight
leaves tables and the empty target unchanged. Admission refusal in LNO is an
observation only: it does not delete code, request runtime substitution or
change normal fallback optimization. No new compiler option is introduced;
the existing opt-in partition-axis analysis control enables the additional
versioned AI_HIGH form report. Its existing AI_PARTITION record format stays
unchanged. With the option off, neither new analysis nor trace is requested.

## Semantic High producer

`VHO_tensor_materialize_high` in `be/vho/tensor_high_lower.cxx` consumes the
existing typed VHO_TENSOR_PROGRAM. It reuses whole-program/schema/shape/storage
admission and expands the current F32/bool common primitives: copy, reshape,
transpose, NumPy broadcast arithmetic/comparison/select, fill, exp/log, isfinite,
logical-not, batched matmul and trailing-axis sum/max reductions. No new logical
opcode, tensor type, native mapped section or runtime library is needed.

Each output tensor coordinate has a signed I8 zero-based ascending unit DO_LOOP.
ARRAY subscripts are affine sums of coordinates times static row-major strides;
broadcast dimensions use zero coordinates and transpose permutes coordinates.
Matmul has an explicit ordered contribution loop. Reductions initialize scalar
F32 state then update it in source order, retaining NaN/infinity guards and the
existing stable-MLA source DAG. No GPU coordinates, fusion, split reduction,
tiling, prefetch, shared-memory decision or reassociation is introduced.

One formal pointer represents each unique storage allocation, in increasing
storage-ID order. Shape-only aliases use the same formal and perform no copy.
The caller must supply correctly sized, aligned, mutually disjoint storage
buffers, including temporary allocations; pointer types explicitly carry that
restriction. This is a new explicit review/entry ABI, not an implicit conversion
of the source tensor-object ABI. `mla.buffers.json` contains the full source
recipe, storage order and buffer contract. The saved MLA graphs have 48 semantic
operations, 55 values and 41 unique storage formals.
M2b must bind that full manifest to the native image/source identity before
using its logical shapes or numerical contracts; explanatory comments and
function names alone cannot authorize a schedule.

Comments explain operation/storage identity, aliases, signed loops, affine
indexing and ordered reductions. Statements and declarations carry real source
positions, and source remains available for ir_b2a -st -src. Before/after images
capture the genuine empty target and expanded algorithm using the existing
mapped writer. The semantic High producer runs once before candidate search;
it is not trial materialization of physical candidates in Phase 1.

## LNO admission

`LNO_DSL_admit_common_high` is invoked at M1's full-graph boundary inside
Lnoptimizer, before Hoist_Varying_Lower_Bounds, under effective O3, Run_lno,
LNO_enabled/LNO_Opt and the existing opt-in analysis control. PREOPT DU/alias,
parent/access maps and the full DEPV_ARRAY_ARRAY_GRAPH are borrowed only for
this synchronous walk. The observer does not allocate WN/ST/TY, mutate maps,
change loops or retain analysis pointers.

Admission requires an active FUNC_ENTRY, built DU, alias and full graph. Each
remaining loop must have signed I4/I8 zero origin, positive bounded constant
extent, LT or LE comparison and ascending unit step. Reuse WN_LOOP queries;
never normalize unsigned or dynamic loops inside the observer. It refuses
unsupported loop effects and Has_Bad_Mem, opaque/volatile or nonaffine indirect
memory, unrepresented global scalar memory, unknown intrinsics/control and
memory operations lacking a vertex in the full graph. Known scalar exp/log
intrinsic expressions and the non-executable frontend preamble marker are allowed. Every indirect load/store must retain ARRAY
and non-Too_Messy access information. Return-only/loop-free code is not admitted.

Positive admission proves form and memory coverage only. It does not prove a
particular schedule, dependence preservation, parallelism or numerical
reassociation. The trace says `snapshot=false schedule_legality=not_proved`.
M2b must copy stable owned facts and bind a mutation epoch; M4 must revalidate
and apply only the selected plan. Ordinary LNO still proceeds after this
observer, so later transforms are not advertised as a frozen selected schedule.

Early short-loop unrolling may remove loops; initial admission covers surviving
prepared nests. Missing DU/no loops/disabled/skip/failed graph paths do not
publish this positive report. MP/Lego/autopar and arbitrary REGION-root search
integration remain outside the admitted production search contract. This
observer grants no scope expansion when an existing driver path performs other
lowering. The original ordinary LNO behavior remains authoritative on refusal.

## Focused evidence and limits

`be/lno/tests/run_common_high_review.py` uses real saved short/long MLA source B,
the ordinary backend and independent mapped reopen. It compares on/off/default/
trace-off output images and the retained pre-change LNO baseline byte-for-byte.
Ordinary C fixtures exercise affine loops and ordered folds plus dynamic,
unsigned, strided, indirect, volatile and call refusals. O0/O1/O2 and LNO opt=0
must not emit admission reports and retain their existing behavior.

The High image is translated by ordinary whirl2c, compiled and executed for all
seven saved scenarios per context against the existing FP64 oracles, with
guarded disjoint allocations and immutable inputs. The host adapter header
maps legacy infF/F4EXP/F4LOG names to standard C math; it adds no algorithm.
An additional post-LNO C lane disables SIMD because ordinary whirl2c cannot
print every x86 vector REPLICATE operator. Default native LNO is still compared
without that control change. Exact pre/post host parity is recorded separately
from the required FP64 numerical tolerances, not presumed.

A pre-existing long-context post-LNO CG dependence-graph reopen failure is
tracked separately: both retained and rebuilt LNO produce the same affected
image; normal whirl2c fails in Depgraph_Read/OPCODE_make_op. The harness must
verify baseline identity and the same baseline diagnostic before reporting this
limitation. Unexpected failures remain test failures. This does not invalidate
the High image's numerical checks or actual PREOPT/LNO admission, and it is not
reported as successful post-LNO execution. Repair belongs to the later mapped
integration acceptance gate; M2a does not alter dependence-image encoding.

Impact-selected existing tensor-lowering and native partition tests verify
default behavior and linkage. Retain sources/tools, commands, source-mapped
B/T/diffs, source and resource identities, positive/refusal traces, numerical
results and the known reopen diagnostic. Publish an immutable new receipt;
preserve every previous bundle. GPU execution/performance, automatic runtime
dispatch, owned snapshot generations and selected schedule application remain
open. Transfer roles remain TBD.
