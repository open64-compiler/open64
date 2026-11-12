# WHIRL-derived tensor reference CUDA

Version v0.1, November 5 2026. Owner, successor, independent reviewer and
approving lead: TBD. This document controls `be/whirl2c/tensor_source.cxx`,
`open64_tensor_reference.cuh` and the MLA CUDA review harness. It extends the
[native tensor contract](AI-COMPILER-NATIVE-TENSOR-EXPRESSION-CONTRACT-v0.1.md)
and [MLA ingestion plan](../osprey/torch2whirl/MLA-INGESTION-PLAN.md).

## Deliverable and phase boundary

The user-selected pre-lowering output is compilable reference CUDA. The native
mapped `.B` is the semantic source of truth. `whirl2c -cuda -tensor-source`
reopens it using Open64's normal mapped-image reader and native gatekeeper,
then prints the admitted tensor expressions as serial C++17 host/device
functions. Python, FX, input tensor payloads and model source are unnecessary
for source emission. Operator names, versions, operands, result types and
attributes come from native WHIRL and its typed image tables.

This mode preserves operation boundaries, input formals and output references.
It does not recognize MLA by name. Each function has a deterministic
`open64_reference_<PU-name>` name; its parameters are fixed-shape `TENSOR`
values passed by reference. Arguments use symbol identities rather than
untrusted source identifiers. Sanitized PU-name collisions reject the file.
The reference header must be supplied beside the generated source or through
the compiler include path. It contains generic primitive semantics, with no
model-specific attention implementation.

The function is callable from host C++ and CUDA device code. It does not
provide a `__global__` entry point, host launcher or short/long dispatch.
A caller can supply an explicit launch wrapper when performing GPU validation.
The large local value arrays deliberately favor inspectability; long-context
device compilation is not evidence of suitable device stack use or speed.

The existing `whirl2c -cuda` path continues to translate selected device PUs
after lowering. Reference emission never invents a tile, split reduction,
shared-memory plan, pipeline, launch geometry or architecture resource budget.
No normal backend option implicitly enables reference emission.

## CLI and build

Build the focused tools in a configured Linux Open64 build tree. Existing
common reader objects and libcomutil/libjson dependencies must be available;
normal configured builds can build these dependencies first.

```sh
make -C "$BUILD/osprey/targdir/ir_tools" whirl2c SKIP_DEP_BUILD=1
"$BUILD/osprey/targdir/ir_tools/ir_b2a" -st -src short.B short.before.T
"$BUILD/osprey/targdir/ir_tools/whirl2c" -cuda -tensor-source \
  -CLIST:dotc_file=short.reference.cu short.B
"$BUILD/osprey/targdir/ir_tools/ir_b2a" -st -src short.B short.after.T
nvcc -std=c++17 -arch=sm_90 -dc --fmad=false --ftz=false \
  --prec-div=true --prec-sqrt=true -I "$SRC/osprey/be/whirl2c" \
  short.reference.cu -o short.reference.o
```

The focused `whirl2c` uses the actual wrapper source and the new sibling
`whirl2c_tensor_source` companion. Its ordinary backend branch requires the
existing `whirl2c_be` and associated components from a full build. The full
whirl2c makefile builds and installs the companion and reference header alongside
the wrapper. The header remains an explicit source dependency supplied with review bundles.
The reference companion consumes `-cuda` (or `-CLIST:cuda=ON`),
`-tensor-source`, `-CLIST:dotc_file=...`, and a positional image or `-fB,...`.
Unknown CLIST options and multiple inputs reject. Options owned by other
Open64 phases are silently ignored, following normal option propagation.
Default output is `<input>.reference.cu`.

The output must be a fresh path. All PUs are validated and buffered before any
source is published. A temporary file in the output directory is flushed,
closed, atomically linked without replacement, and removed from staging.
Existing paths, symlinks and input aliases cannot be overwritten. A late-PU
failure publishes no partial `.cu`. There is no transformed output image;
retain the original `.B` and prove its digest and independent `.T` unchanged.

## Admitted semantics

This version accepts pure straight-line native tensor definitions in BLOCKs,
whole-tensor LDID/STID, tensor input/result formals and a final RETURN. It
rejects offsets, calls, control flow, nested PUs, input mutation, repeated
definitions, output reads, state results, undefined operands, unsupported
operators/versions and incomplete results. Multiple top-level PUs are allowed
when each meets the same contract. The native gatekeeper runs before the
emitter's narrower capability checks.

| Operation | Version | Reference semantics |
| --- | --- | --- |
| reshape | 1 | Same flat logical elements in the explicit result shape |
| transpose | 1 | Materialized logical permutation |
| matmul | 3 | Numpy prefix broadcast, explicit F32 accumulation, no transpose flags |
| add/sub/mul/div/greater | 1 | Numpy broadcast; greater is ordered and returns bool |
| select | 1 | Boolean mask with numpy broadcast for all operands |
| exp/log/isfinite/logical_not | 1 | Same-shape primitive operations |
| fill_like | 1 | Exact admitted binary32 literal, including infinity |
| reduce_sum | 3 | Static last-axis F32 sum with the declared keepdim |
| reduce_max | 1 | Static last-axis maximum with NaN propagation |

Attributes must exactly match the supported schema and values; arbitrary
extra attributes are not ignored. Positive static F32/bool shapes have rank
at most 16 and at most 16M elements per value. Logical views materialize values
in this reference representation, without claiming optimized physical layout
or aliasing. Bool storage is C++ bool; the host review checks actual artifact
input bytes. There is no public cross-language ABI promise for this container.

F32 reductions and contractions use explicit serial accumulation. Review
compilation disables FMA and fast math. Exp/log retain `source_unresolved`:
this is a reference realization checked numerically, not a bitwise guarantee
across PyTorch, host libm and CUDA math. Original graph masking determines
all-masked behavior: zero output and negative-infinity LSE. The emitter does
not fuse softmax, change the original scale or reassociate the graph into
split attention. Signed-zero and exceptional-value behavior beyond the
tested primitive boundary require a stronger future numerical contract.

## Implementation order and acceptance

1. Produce native tensor `.B` with seven runtime inputs and two MLA results;
   independently reopen with `ir_b2a -st -src`. Completed in native milestone.
2. Reopen the same `.B` through the source companion, run native gatekeeper,
   enforce the admitted subset, then publish reference `.cu`. No compiler
   analysis or WHIRL mutation is needed, so no analysis invalidation occurs.
3. Compile the actual emitted source as host C++ and execute retained tensors;
   compare outputs and LSE to the independent expanded-key/value FP64 oracle.
   Repeat with one all-masked head and all heads masked.
4. Compile the identical emitted source with NVCC for `sm_90`, retaining
   commands, diagnostics, object files and tool identities. Keep GPU execution
   status separate. Verify unchanged input `.B` and before/after `.T`.
5. Independently implement executable common-IR lowering, scheduling and GPU
   mapping in their owning phases. Retain changed before/after `.B` and `.T`
   at each real transformation, with analysis invalidation/recomputation.
6. Run normal `whirl2c -cuda` on the selected lowered image. Add the explicit
   host launch and resource contract, device execution, oracle comparisons
   and performance characterization before accepting an optimized MLA kernel.

Steps 1–4 are the current reference-source milestone. Steps 5–6 remain open.
The future `be_driver` pipeline remains: Very High domain expansion; High
common algorithmic IR and DO_LOOP/OPR_ARRAY construction; dependence analysis
and legal scheduling; multi-level tiling and staging/prefetch loops; Mid
structured memory lowering; Low GPU mapping; normal CUDA or PTX emission.
Reference emission is an observation of an earlier checkpoint, not an
additional lowering phase and not a substitute for that pipeline.

## Tests, evidence and risks

`mla_cuda_review.py` retains both context images (L=127 and L=8193), independent
before/after reader traces, the emitted `.cu`, header, input/reference tensors,
binary execution inputs/results, host runner sources/binaries, CUDA objects,
commands, diagnostics, report and checksums. Acceptance uses rtol=2e-5,
atol=2e-6, and exact infinity sign on empty rows. No timing claim follows.

`test_tensor_source_optional.py` checks an unrelated native program, unsupported
native relu, late failure in a second PU, truncated input, and unchanged normal
wrapper dispatch. `tests/tensor_reference_test.cxx` checks unequal broadcast
batch axes, nontrivial transpose, keepdim=false reduction, NaN maximum,
finite predicates, fill bits and boolean selection. The MLA harness additionally
checks no overwrite, no input alias, missing CUDA flag and unknown options.

Primary risks are the deliberately large local value footprint, host/device
math differences, bounded operator coverage, and future optimized transforms
changing accumulation order. GPU compilation alone does not validate launch,
stack capacity, memory behavior, correctness on a device or performance.
Named ownership and independent review remain TBD transfer-acceptance items.
