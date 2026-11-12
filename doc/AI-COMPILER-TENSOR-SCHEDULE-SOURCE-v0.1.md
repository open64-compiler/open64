# Readable CUDA for the selected tensor schedule

Version v0.1, November 5 2026. This is an executable review view of the
[selected fusion/split schedule](AI-COMPILER-TENSOR-KERNEL-LOWERING-v0.2.md),
after VHO selection and before expansion into scalar device WHIRL.

## Boundary and purpose

The earlier `whirl2c -cuda -tensor-source` mode prints the original tensor graph
before optimization. Normal `whirl2c -cuda` prints the selected mapped device
image after scalar lowering. This additional view fills the review gap: it
shows the selected stage cuts, fused producers, and partial/merge structure
without expanded coordinate arithmetic.

The focused driver command is:

```sh
tensor_kernel_lower schedule-source selected.json optimized.cu
```

`selected.json` is the existing sealed v2 recipe produced by `optimize --fuse`
and, for the long fixture, `--split-width 256 --allow-reassociate`. The driver
validates it and obtains exactly the same `VHO_tensor_schedule` result used by
device materialization and auditing. The new read-only printer consumes that
plan; it chooses no fusion boundaries, chunk dimensions, target, launch, or
attention algorithm. Unselected baseline recipes are rejected.

There is no persisted optimized tensor `.B` at this boundary in the current
focused pipeline. The view is a projection of the detached selected schedule,
not a claim that a new optimized High WHIRL image has been written. The native
source image and independently audited scalar device image remain the IR
provenance and execution artifacts. This milestone changes neither image,
the VHO optimization policy, nor any mapped-image or logical-opcode contract.

## Readable source contract

Each generated `.cu` exposes one `extern "C"` host/device review function with
the source input/result pointer ABI. Names retain sanitized source spellings
and unique value IDs. Literal bits remain exact, with readable F32 comments.
Every selected stage has its kernel ID, source node/operator/version, work-item
count, and, where applicable, selected chunk width/count and scratch layout.

Named `const auto` producers are **lazy views**, not allocated intermediate
tensors. The printer stops traversal at the boundary operands supplied by VHO.
Each view borrows its named operands, which remain in scope until the selected
stage completes. Only materialized stage outputs allocate `TENSOR` storage;
externally supplied results use the caller's pointers. The helper header
contains generic address/broadcast/transpose semantics so the `.cu` retains
tensor-level notation.

The visible execution operations are `matmul_to`, `reduce_to`,
`split_matmul_to`, `split_reduce_to`, `merge_to`, and `materialize_to`.
Split helpers consume the selected width/count and chunk-major scratch layout.
Their inner order, outer merge order, F32 node results, NaN maximum behavior,
and masking match the selected ordinary-WHIRL construction. The existing
reference header supplies tensor shapes, bit conversion, IEEE predicates,
broadcast indexing, and batch indexing; these services are reused.

The review executor evaluates those stages serially on host or device. Its
loops make the schedule executable for checking; they do not implement CUDA
launches or reproduce the production performance. Actual mapped GPU kernels
continue to come from the selected device `.B` through normal `whirl2c -cuda`.
Changing a review view does not change the compiler's selected plan or backend.

## Implementation and compatibility

- `be/whirl2c/tensor_schedule_source.cxx/.h`: read-only selected-plan printer.
- `be/whirl2c/open64_tensor_schedule.cuh`: generic lazy-view and stage executor.
- `ir_tools/tensor_kernel_driver.cxx`: validated plan handoff and atomic,
  no-replace source publication.
- `be/vho/tests/tensor_schedule_review.py`: numerical, mapped-product join,
  compilation, deterministic output, and no-overwrite evidence.

Only the standalone focused tool links the new printer. `be.so`, normal
`whirl2c.so`, frontend libraries, and existing source-mode dispatch are unchanged.
No Python logic generates compiler IR or selects an optimization. The printer
uses detached C++ facts and introduces no live PU access or scope switching.

The generated source and headers require C++17. Place both
`open64_tensor_schedule.cuh` and the unchanged `open64_tensor_reference.cuh`
on the include path. For example:

```sh
g++ -x c++ -std=c++17 -O1 -ffp-contract=off -fno-fast-math \
  -shared -fPIC -I. optimized.cu -o optimized.so
nvcc -std=c++17 -arch=sm_90 --fmad=false --ftz=false \
  --prec-div=true --prec-sqrt=true -dc -I. optimized.cu -o optimized.cuda.o
```

## Validation and invalidation

The source-view stage IDs must exactly equal the ordered kernel IDs audited
from the actual selected device `.B`. The short fixture has 9 stages; long has
12, including three partial/merge pairs with W=256 and C=33. Seven retained
scenarios per context check original masks, one masked head, all masked,
tail-only valid data, empty first chunk, equal scores, and large finite scores.
Both output and LSE are checked against the saved independent expanded FP64
oracle at rtol=2e-5, atol=2e-6. The generated host outputs must also match the
existing lowered host outputs byte for byte. All buffers have guards and all
inputs must remain unchanged. NVCC separately compiles both host/device files.

Generic tests cover non-MLA subtraction, byte booleans, unused fill input ABI,
broadcast matmul, uneven split tails, and NaN maximum; selected-view results
must equal the ordinary lowered results. Additional controls reject baseline
recipes and stale policy/source handoffs without publishing output. Existing
lowering and reference-emitter regression tests remain in the focused suite.

Source/printer/header changes invalidate source-view generation and its host
and NVCC checks. A schedule or numerical-policy change also invalidates the
device-product join and all parity results. Presentation alone does not
invalidate unchanged production IR or its earlier correctness evidence; no
new IR transformation or GPU performance result is claimed.

GPU execution remains NOT_RUN in the available environment. A compiled
host/device review function and host parity are separate from GPU validation.
Owner, successor, independent reviewer, and approving lead remain TBD under
the existing MLA transfer requirements.
