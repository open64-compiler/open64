# Tensor API and LLM DSL Comparison

Status: research note, July 2026.

This report compares tensor-related APIs in PyTorch and kernel/compiler DSLs
commonly used around LLM implementation work: Triton, NVIDIA CuTe/CUTLASS CuTe
DSL, JAX Pallas, and Apache TVM TensorIR.  The emphasis is on tensor semantic
state that matters to a compiler IR: dtype, shape, layout, strides, memory
space, aliasing/storage identity, dynamic shape constraints, and lowering
ownership.

## Executive Summary

PyTorch is a high-level eager and graph-capture tensor framework.  Its tensor
API treats a tensor as a user-visible multidimensional value with dtype, device,
layout, storage, strides, autograd state, and mutation/view behavior.  It is
rich enough to describe model semantics but deliberately hides most hardware
tiling and memory hierarchy details.

Triton is a kernel DSL.  Its `tl.tensor` is not the same abstraction as a
PyTorch tensor.  It represents an N-dimensional block of scalar values or
pointers inside one SPMD program instance.  Global tensors are usually passed as
pointers plus shape/stride arguments, or represented through
`tl.tensor_descriptor` / block pointers.  Triton exposes masks, boundary checks,
cache modifiers, tensor-core dot operations, and launch-grid program IDs.
The original 2019 MAPL/PLDI Triton paper is especially relevant here: it
describes Triton as a language and compiler centered on tile, meaning
statically shaped multidimensional sub-arrays, and expresses tensor programs as
operations over parametric tile variables.

CuTe/CUTLASS CuTe DSL is lower-level and more layout-centric than Triton.  Its
core tensor model composes an engine, usually a pointer or iterator, with a
layout.  Shape, stride, memory space, alignment, tiling, partitioning, and
hardware atoms are first-class.  This makes CuTe especially close to the
representation needed by dense GEMM and attention kernels, but farther from
ordinary model-level tensor semantics.

JAX Pallas occupies a middle position: users write JAX-like custom kernels that
are mapped over a grid with explicit block specs.  It keeps high-level JAX
tracing ergonomics while exposing kernel-level block/tile behavior for GPU and
TPU backends.  It is experimental and changing quickly.

TVM TensorIR is a compiler IR and scheduling DSL rather than a Python tensor
object API.  It represents primitive tensor programs with buffers, blocks,
spatial/reduction axes, read/write regions, and schedulable transformations.
It is the closest comparison point for low-level scheduling and lowering, but
not the first TVM IR normally produced from PyTorch.  TVM's current recommended
PyTorch path imports `torch.export.ExportedProgram` into Relax, a graph-level
IRModule, and later introduces TensorIR functions through legalization.

For Open64 DSL WHIRL, no single system should be copied directly.  A useful
TensorDescriptorIR should keep PyTorch-like semantic identity at the frontend
boundary, Triton/CuTe-like layout and memory facts before kernel lowering, and
TVM-like explicit scheduling/lowering contracts without exposing private
storage encodings as public operator semantics.  For `whirl2c`, the closest
short-term goal is not TensorIR-style C-like loops; it is a readable C-extension
projection over still-high-level DSL tensor operators and tile views.

## Feature Matrix

| Feature | PyTorch | Triton | CuTe/CUTLASS CuTe DSL | JAX Pallas | TVM TensorIR |
| --- | --- | --- | --- | --- | --- |
| Primary role | Model framework and tensor runtime | Python GPU kernel DSL | Python/C++ CUDA layout and kernel DSL | JAX custom kernel language | Tensor-program IR and scheduler |
| Tensor unit | `torch.Tensor` user value | `tl.tensor` block value or pointer block | Tensor = engine/pointer composed with layout | JAX array blocks via `BlockSpec` and refs | Buffer plus block/axis regions |
| Main abstraction level | Model/operator level | Program-instance block level | Thread/data hierarchy and tiled kernel level | Blocked custom-kernel level | Compiler IR and schedule level |
| Dtype | `torch.dtype`, broad framework dtype set including float, integer, bool, complex, sparse-adjacent support | `tl.dtype`; explicit casts, type promotion, tensor-core dtype restrictions | Pointer/value dtype plus hardware atom dtype constraints | JAX dtypes through tracing and backend lowering | Buffer dtype and scalar expression dtype |
| Device or target | `torch.device` such as CPU, CUDA, MPS, XPU, XLA, meta | Kernel target is backend GPU; program sees pointers and launch grid | NVIDIA GPU/CUDA focused; memory space is explicit | GPU and TPU oriented, backend dependent | Target selected by TVM lowering and codegen |
| Shape model | Tensor sizes, dynamic shape capture through `torch.export` specs | Compile-time block shapes plus runtime shape arguments and pointer descriptors | Static and dynamic integer tuples, hierarchical shapes | Grid/block specs, JAX shape tracing | Buffer shapes, loop extents, block axes |
| Tile representation | Usually a view/slice/chunk of a tensor, not a distinct core tensor kind | A `tl.tensor` block is commonly the tile operand/result inside one program instance; block pointers and descriptors name global-memory tiles | Tile is first-class through layout algebra, tilers, partitioning, subtensors, `TiledMma`, and `TiledCopy` | `BlockSpec` maps grid indices to tiled array refs; kernel operands/results are per-invocation blocks | Tiling is represented by scheduled loop/block transformations, buffer regions, and tensorization patterns |
| Kernel/input form | Framework call receives full tensors; tile slicing is expressed by tensor ops | Kernel signature receives pointers, dimensions, strides, and `tl.constexpr` block sizes; tile variables are built with `program_id`, `arange`, pointer arithmetic, `load`, block ops, and `store` | Kernel receives tensors/pointers/layouts; tilers and layouts derive tile tensors | Kernel call uses JAX arrays plus `BlockSpec` / `GridSpec` | Relax receives graph tensors; TensorIR functions receive buffers and scalar shape/stride-like parameters |
| Layout model | `torch.layout`, commonly strided; sparse layouts exist separately | Pointer arithmetic, strides, block pointers, tensor descriptors | Layout is a core abstraction mapping coordinates to offsets | Block specs map grid/program IDs to array slices | Buffers, regions, layouts, schedules |
| Strides | User-visible `stride()` for strided tensors | Explicit stride arguments or descriptor fields | First-class layout stride tuples | Implicit through JAX arrays and block specs | Buffer strides and access regions |
| Storage identity | Tensor has storage and view semantics | Pointers and descriptors, no framework storage object | Engine/pointer owns data access; layout maps coordinates | Refs to input/output blocks | Buffer data pointers and regions |
| Mutability | In-place ops marked by underscore; autograd constraints apply | `tl.store`, atomics, descriptor stores | Explicit memory copy/store and tensor element access | Kernel refs and stores | Buffer stores in IR |
| Autograd | Integrated autograd and graph capture | No built-in autograd; used beneath frameworks | No built-in autograd | JAX transform integration is the surrounding ecosystem, but custom kernels need rules/constraints | No autograd at TensorIR level |
| Compilation | Eager, `torch.compile`, `torch.export`, AOT graph capture | `@triton.jit` kernels lowered by Triton compiler/MLIR stack | `@cute.jit` and `@cute.kernel`, AST rewrite plus tracing, MLIR/PTX flow | JAX tracing to backend-specific lowering | TVMScript/IRModule to scheduled TIR and codegen |
| Dynamic shapes | Export supports dynamic dims and constraints; dtype/device are static metadata | Runtime scalar shape parameters with compile-time block meta-parameters | Dynamic JIT args plus `Constexpr` compile-time args; layouts may mix static/dynamic integers | JAX tracing and block/grid specs | Symbolic extents and schedule transforms |
| Memory hierarchy | Mostly abstracted; device visible but shared/register details hidden | Global pointers, masks, cache modifiers, block pointers, tensor descriptors, dot | Global/shared/register/thread layouts, copy atoms, MMA atoms, tiling/partitioning | Backend-specific memory behavior, especially TPU/GPU guides | Explicit buffer scopes, local/shared allocation, tensorization |
| LLM kernel fit | Model authoring, export, graph capture, high-level ops | Excellent for fused attention, matmul, softmax, layernorm, quant kernels | Excellent for GEMM/attention kernels needing hardware-level tiling | Good for JAX TPU/GPU custom kernels | Strong for whole-operator scheduling, autotuning, codegen |
| Bounds/masking | High-level ops validate shapes; masks are values | Masked load/store, boundary checks, padding options | Layout/tiling discipline plus explicit predicates as needed | Block specs and backend-specific constraints | Region analysis and explicit predicates |
| Developer-visible tile debug or trace | Tensor print plus `size`, `stride`, `storage_offset`; FX graph and tensor metadata can include shape/stride | `TRITON_INTERPRET=1`, Python `print(tl.tensor)`, `static_print`, `device_print`, MLIR/LLVM dumps | Printed tensor/layout forms such as `((_8,_4),Rest)` and debug mode with line info, stack traces, trace-time verification | `BlockSpec`, `GridSpec`, `debug_print`, `interpret=True`, `debug=True`, JAX IR dumps | TVMScript blocks/regions, `sch.mod`, `sch.show()`, and `sch.trace.show()` schedule history |
| Interop | Native framework center; DLPack and extension paths | Called from Python/frameworks, often consuming PyTorch pointers | DLPack and direct PyTorch tensor argument support in CuTe DSL | Native to JAX, usable through JAX ecosystem | Imports models and lowers to runtime modules |
| Initial PyTorch-connected IR position | Source framework tensor graph before import | Usually hand-authored or generated kernel body called below framework graph | Usually hand-authored or generated kernel/layout body called below framework graph | JAX traced kernel body below framework graph | PyTorch imports first to Relax `IRModule`; TensorIR appears after `LegalizeOps` / lowering |
| Best comparison to Open64 DSL WHIRL | Frontend semantic tensor facts | Logical DSL operator plus block kernel lowering | Tensor layout, placement, tiling, memory descriptors | Traced custom kernel boundary | Relax is closer to high-level DSL projection; TensorIR is closer to post-lowering scheduling |

## Tensor API Notes by DSL

### PyTorch

PyTorch tensors are multidimensional values with a single dtype.  Each tensor
has dtype, device, and layout attributes.  Dense tensors use strided storage:
storage owns the data while tensor metadata supplies the multidimensional view.
The user can inspect sizes, strides, dtype, device, layout, storage aliasing,
requires-grad state, and mutation behavior.

Important design points:

- Tensor data is dynamic at runtime; dtype, device, and other metadata are
  normally static for export.
- `torch.export` normalizes programs to functional ATen operators, lifts module
  state such as parameters and buffers out of the graph, records shape
  constraints, and validates assumptions.
- This is the richest source of model-level tensor semantics, but it is not a
  hardware layout DSL.  Shared memory, tensor-core tiles, warp layouts, and
  cache behavior belong below the PyTorch abstraction.
- Tiles appear as ordinary tensor views or operators such as slicing,
  narrowing, splitting, unfolding, chunking, or reshaping.  A tile can be an
  operand or result in the Python program, but PyTorch does not give it a
  separate compiler-level tile type with memory-hierarchy ownership.

### Triton

Triton kernels are Python functions compiled by Triton.  The fundamental
in-kernel data structure is `triton.language.tensor`, an N-dimensional block of
values or pointers.  Most `tl.*` operations consume and produce these block
tensors.  Global tensors are usually modeled as base pointers plus shape,
stride, and block offsets, or by `tl.tensor_descriptor` / block pointers.

Important design points:

- Program instances are indexed with `tl.program_id(axis)` over a launch grid.
- `tl.load` and `tl.store` expose masks, fallback values, boundary checks,
  padding, cache modifiers, eviction policy, and volatile semantics.
- `tl.dot` is an explicit tensor-core-oriented block matrix multiply operation
  with dtype and precision constraints.
- Triton’s tensor shape is usually a compile-time block shape, not the full
  model tensor extent.  The full extent is carried separately as scalar
  arguments or descriptors.
- In Triton, a tile is often exactly the `tl.tensor` block produced by
  `tl.load`, transformed by block operations, and consumed by `tl.store`,
  `tl.dot`, reductions, or elementwise fusion.  The tile shape is normally a
  `tl.constexpr` meta-parameter, while its position in the full tensor is
  derived from `program_id`, offsets, masks, and strides.
- The 2019 Triton paper's input model is important for `whirl2c`: full tensors
  enter as external memory objects, while the language exposes parametric tile
  variables as the developer-visible compute operands.  Current Triton Python
  has the same practical form: kernel arguments are pointers, dimensions,
  strides, and compile-time block sizes, then `tl.arange` and pointer arithmetic
  form block pointers whose `tl.load` results are first-class tile values.

### CuTe/CUTLASS CuTe DSL

CuTe is built around layouts and tensors.  A CuTe tensor composes an engine,
such as a pointer or iterator, with a layout.  The layout maps logical
coordinates to physical offsets; the engine provides the actual data access.
CuTe DSL brings this model into Python with JIT decorators while retaining
consistency with CuTe C++ concepts.

Important design points:

- Layout is a first-class algebra over shapes, strides, tiling, composition,
  and partitioning.
- Tensor metadata includes type, shape, memory space, layout, pointer
  alignment, and hierarchical thread/data structure.
- CuTe DSL exposes hardware atoms such as MMA and copy atoms, plus tiled
  operations like `TiledMma` and `TiledCopy`.
- Framework interop uses DLPack or direct PyTorch tensor acceptance in newer
  paths; CuTe tensors can also be constructed directly from pointers and
  layouts.
- CuTe is the strongest example of tile-as-structured-tensor.  A tile is a
  tensor view produced by layout tiling, zipped division, local tiling, or
  partitioning.  Because a CuTe tensor is engine plus layout, a tile can carry
  both logical shape and physical layout through copy, MMA, register, shared
  memory, and global memory transformations.

### JAX Pallas

Pallas is a JAX extension for writing custom GPU and TPU kernels.  It aims to
combine fine-grained kernel control with JAX tracing and `jax.numpy`-style
ergonomics.  It is experimental and changes frequently.

Important design points:

- `pallas_call`, `BlockSpec`, and `GridSpec` describe how arrays are partitioned
  across kernel invocations.
- It targets GPU and TPU backends, including TPU-specific guides for
  pipelining, matrix multiplication, block-sparse computation, and distributed
  behavior.
- Pallas is best understood as a custom-kernel layer inside the JAX ecosystem,
  not as a standalone tensor runtime like PyTorch.
- A Pallas tile is mainly specified by `BlockSpec`: the block shape and index
  map define which slice of a larger array a kernel invocation receives or
  updates.  The tile participates as a kernel operand/result through refs rather
  than as a separate persistent framework tensor object.

### TVM TensorIR

TensorIR is a primitive tensor-program IR in Apache TVM.  It represents tensor
programs using functions, buffers, loops, blocks, block axes, and explicit
read/write regions.  Scheduling transforms mutate the IR directly.

Important design points:

- TensorIR buffers are declared with shape, dtype, and optional memory scope.
- Blocks capture spatial/reduction axes, access regions, and computation
  bodies; this makes dependency analysis and scheduling explicit.
- Schedule primitives express transformations such as tiling, vectorization,
  thread binding, tensorization, and locality management.
- TensorIR is closest to compiler-middle-end representation.  It is less a
  user tensor API and more a verifiable, transformable statement IR.
- In TensorIR, a tile is usually the result of schedule transformations over
  loops, blocks, and buffer regions.  It is represented structurally by split
  loop axes, block bindings, read/write subregions, allocated local/shared
  buffers, and tensorization intrinsics rather than by a new tensor value class.

### TVM Relax and PyTorch import position

For PyTorch-connected TVM flows, TensorIR is not usually the first TVM-side IR.
The recommended PyTorch frontend takes a `torch.export.ExportedProgram` and
converts it to a Relax `IRModule`.  TVM also supports importing a PyTorch FX
`GraphModule` into Relax.  In both cases, the imported result is graph-level
Relax, not already a scheduled TensorIR loop program.

TVM's documentation describes Relax functions as high-level graph structure for
an end-to-end model or subgraph, while TensorIR `PrimFunc`s are low-level tensor
programs used for operators or fused layers.  The `LegalizeOps` pass converts
Relax operator calls to `call_tir` and adds corresponding TensorIR functions,
creating a mixed Relax + TensorIR module.

This matters for Open64: TensorIR is useful evidence for what a lowered,
schedulable kernel IR should expose, but it is too close to loop-level C to be
the model for early `whirl2c` DSL output.  The early `whirl2c` projection should
look more like a high-level graph/operator dialect with tensor and tile
descriptors, and only later, after DSL lowering, should it move toward
TensorIR-like loops, buffer regions, and explicit schedules.

## Tile Representation

A tile is not just a shape annotation.  For LLM kernels it can be an operand,
an intermediate, or a result of an operator: a query/key/value block, a
matmul fragment, a softmax row block, a KV-cache page, a shared-memory staging
tile, or a register fragment consumed by tensor-core instructions.  The key
design question is whether the system represents the tile as:

- a view over a larger tensor;
- a block value local to one program instance;
- a tensor whose layout encodes tiling and partitioning;
- a scheduled buffer region in an IR;
- a hardware fragment tied to MMA/copy instructions.

PyTorch mostly represents tiles as views or ordinary tensor values.  This is
good for semantic capture but insufficient for preserving warp, shared-memory,
or tensor-core contracts.

Triton makes the program-instance block central.  A tile is often the value
loaded into a `tl.tensor`, and it can flow through arithmetic, `tl.dot`,
reductions, masks, and stores as a normal operand/result.  Full-tensor identity
is external: base pointer, strides, shape arguments, descriptors, and offsets
connect the block tile back to the model tensor.

CuTe treats tiles as layout-derived tensors.  Tiling and partitioning transform
one tensor into subtensors whose layouts encode logical coordinates, physical
offsets, and hierarchy.  This is powerful for Open64 because it separates the
semantic tensor from a derived tile view while preserving enough structure for
copy atoms, MMA atoms, and memory-space lowering.

Pallas exposes tiles through grid and block specifications.  The programmer
describes how each invocation maps to a slice of an input or output array; the
kernel body works on refs to those tiled regions.

TVM TensorIR represents tiles as scheduled IR structure.  A tile is visible in
split loops, block axes, buffer access regions, and local/shared allocations.
This makes legality checks and dependence analysis more explicit than in a
pure tensor-value API.

TVM Relax, by contrast, normally shows high-level tensor operators before
legalization.  A PyTorch-imported matmul tile would not initially appear as
split loops or shared-memory buffers.  It would remain closer to tensor-level
dataflow until passes choose operator implementations and schedules.

For Open64 DSL WHIRL, tile representation should likely be explicit but derived:
the full tensor descriptor owns semantic identity, while a tile descriptor or
tile view records origin tensor, origin region, tile shape, layout/stride,
memory space, ownership, boundary/padding policy, and lowering role.  That
lets a tile be an operand or result without confusing a temporary tile view
with the full model tensor type.

## Tile Example and Debug Visibility

Consider one 16x32 x 32x16 tile of a larger 64x64 matrix multiply:

```text
C[0:16, 0:16] += A[0:16, 0:32] @ B[0:32, 0:16]
```

The full tensors are `A[64,64]`, `B[64,64]`, and `C[64,64]`.  The tile operands
are shaped `A_tile[16,32]` and `B_tile[32,16]`; the tile result is
`C_tile[16,16]`.  The important compiler facts are not only those shapes, but
also where the tile starts in the full tensor, the stride/layout used to reach
elements, whether out-of-bounds padding is needed, which memory space holds the
tile, and whether the tile is a register fragment, shared-memory staging tile,
or global-memory view.

### PyTorch visible form

PyTorch presents the tile as an ordinary tensor view or slice:

```python
A_tile = A[0:16, 0:32]
B_tile = B[0:32, 0:16]
C_tile = A_tile @ B_tile
print(A_tile.shape, A_tile.stride(), A_tile.storage_offset())
```

A developer sees values with normal tensor printing, and can inspect
`shape`, `stride`, and `storage_offset`.  In FX or export-style graph work,
the developer-visible form is usually an operator such as `getitem`, `slice`,
`view`, or `matmul` plus tensor metadata.  FX debugging supports graph printing,
tabular graph output, generated Python code, and node formatting with tensor
metadata.  This is strong for semantic visibility but weak for hardware tile
identity: nothing in the view says "this is a warp tile" or "this tile lives in
shared memory."

Example debug evidence:

```text
A_tile: shape=torch.Size([16, 32]), stride=(64, 1), storage_offset=0
FX node: call_function[target=operator.getitem](...)
tensor_meta: shape=[16,32], dtype=float32, stride=(64,1)
```

### Triton visible form

Triton usually makes the tile the block value inside one program instance:

```python
pid_m = tl.program_id(0)
pid_n = tl.program_id(1)
offs_m = pid_m * 16 + tl.arange(0, 16)
offs_n = pid_n * 16 + tl.arange(0, 16)
offs_k = tl.arange(0, 32)

a = tl.load(A + offs_m[:, None] * stride_am + offs_k[None, :] * stride_ak)
b = tl.load(B + offs_k[:, None] * stride_bk + offs_n[None, :] * stride_bn)
c = tl.dot(a, b)
```

The input form is deliberately not "pass a tensor object and let the compiler
discover the tile."  In current Triton matmul tutorials, the kernel signature
names base pointers, matrix dimensions, strides, and block meta-parameters:

```python
@triton.jit
def matmul_kernel(
        a_ptr, b_ptr, c_ptr,
        M, N, K,
        stride_am, stride_ak,
        stride_bk, stride_bn,
        stride_cm, stride_cn,
        BLOCK_SIZE_M: tl.constexpr,
        BLOCK_SIZE_N: tl.constexpr,
        BLOCK_SIZE_K: tl.constexpr):
    ...
```

Inside the body, those inputs are converted into tile variables:

```python
offs_am = pid_m * BLOCK_SIZE_M + tl.arange(0, BLOCK_SIZE_M)
offs_bn = pid_n * BLOCK_SIZE_N + tl.arange(0, BLOCK_SIZE_N)
offs_k = tl.arange(0, BLOCK_SIZE_K)
a_ptrs = a_ptr + offs_am[:, None] * stride_am + offs_k[None, :] * stride_ak
b_ptrs = b_ptr + offs_k[:, None] * stride_bk + offs_bn[None, :] * stride_bn
a = tl.load(a_ptrs)
b = tl.load(b_ptrs)
accumulator = tl.dot(a, b, accumulator)
```

That is the key design point: the signature preserves explicit low-level
memory control, while the body talks in tile values.  The OpenAI Triton 1.0
writeup says Triton treats input/output tensors as pointers, precisely to keep
control over memory access patterns such as block-sparse tensors, while
exposing block operations inside each program instance.

The older Triton-C syntax in Listing 1 of the 2019 paper is even more relevant
to `whirl2c` output than current Python syntax.  It is intentionally C-like, but
not ordinary C.  The listing shows a matrix multiply kernel with these visible
DSL constructs:

- `tunable` tile-size parameters such as `TM`, `TN`, and `TK`;
- a C-style kernel signature over raw tensor pointers and dimensions;
- one-dimensional index tiles derived from the SPMD program range;
- two-dimensional pointer tiles for `A`, `B`, and `C`;
- two-dimensional value tiles for loaded operands and the accumulator;
- boolean mask tiles for boundary conditions;
- tile intrinsics such as dot product and transpose;
- predicated write-back using a mask tile.

In other words, the listing does not hide the tile behind loops.  It names tile
indices, pointer tiles, value tiles, mask tiles, and accumulator tiles directly.
It also puts the element type in the declaration, for example `float A[TM,TK]`
or `bool check_a[TM,TK]`.  That is precisely the output style `whirl2c` can
learn from: retain C-like readability, but make DSL tensor/tile constructs
explicit and unapologetic.

An adapted `whirl2c` projection could follow the Listing 1 shape without
claiming to be compilable C:

```c
const tunable int BM = {16, 32, 64, 128};
const tunable int BN = {16, 32, 64, 128};
const tunable int BK = {8, 16};

int32 TILE_IDX row[BM] = GLOBAL_RANGE(0);
int32 TILE_IDX col[BN] = GLOBAL_RANGE(1);
int32 TILE_IDX kidx[BK] = RANGE(0, BK);

float32 *TILE_PTR A_ptr[BM, BK] =
    TILE_PTR_VIEW(A, row[:, newaxis], kidx);
float32 *TILE_PTR B_ptr[BN, BK] =
    TILE_PTR_VIEW(B, col[:, newaxis], kidx);
float32 TILE C_acc[BM, BN] = ZERO(float32);

for (int32 k = K; k >= 0; k -= BK) {
    bool MASK A_mask[BM, BK] = row < M && kidx < k;
    bool MASK B_mask[BN, BK] = col < N && kidx < k;
    float32 TILE A_tile[BM, BK] = LOAD(A_ptr, A_mask, 0);
    float32 TILE B_tile[BN, BK] = LOAD(B_ptr, B_mask, 0);
    C_acc = OPR_DSLDOT.v1(A_tile, OPR_DSLTRANS.v1(B_tile), C_acc);
    A_ptr = ADVANCE(A_ptr, BK);
    B_ptr = ADVANCE(B_ptr, BK);
}

float32 *TILE_PTR C_ptr[BM, BN] =
    TILE_PTR_VIEW(C, row[:, newaxis], col);
bool MASK C_mask[BM, BN] = row < M && col < N;
STORE(C_ptr, C_acc, C_mask);
```

This preserves the same conceptual structure as Triton-C Listing 1: a function
has ordinary tensor pointer inputs, but the body's primary values are tile
indices, tile pointers, masks, tile operands, and tile results.  For Open64 this
suggests `whirl2c` should print tile declarations and tile operations as
first-class DSL WHIRL evidence instead of lowering them prematurely into scalar
loops.

The developer-visible tile is the `tl.tensor` block `a`, `b`, or `c`.  In
interpreter mode, `TRITON_INTERPRET=1` runs kernels through a Python/NumPy
interpreter and allows Python `print(tensor)` or `pdb`; for individual values,
the docs describe inspecting `tensor.handle.data[idx]`.  Compile-time and
runtime debug operators include `static_print`, `static_assert`,
`device_print`, and `device_assert`.  Compiler developers can also dump MLIR or
LLVM IR.

Example debug evidence:

```text
program_id=(0,0)
a: tl.tensor shape=(16,32), dtype=float32
c: tl.tensor shape=(16,16), produced by tl.dot
```

### CuTe visible form

CuTe represents the tile as a tensor produced by layout algebra:

```cpp
Tensor gmem = make_tensor(ptr, make_shape(64, 64));
auto tiler = Shape<_16,_32>{};
Tensor gmem_tiled = zipped_divide(gmem, tiler);  // ((_16,_32),Rest)
Tensor tile = gmem_tiled(_, 0);                  // ((_16,_32))
```

The developer-visible form is especially direct: tensor and layout prints show
hierarchical shape and stride forms such as `((_8,_4),Rest)` or
`((_16,_32),Rest)`.  The CuTe documentation describes tiling, slicing, and
partitioning as tensor operations; slicing returns subtensors with a new
iterator and a new layout.  CuTe DSL debug mode (`CUTE_DSL_DEBUG=1`) enables
line info, fuller stack traces, optimization warnings, trace-time operation
verification, and per-launch argument validation.  `CUTE_DSL_LINEINFO=1` gives
Python-to-PTX/SASS correlation.

Example debug evidence:

```text
gmem_tiled: ((_16,_32),Rest)
tile: ((_16,_32))
layout: shape=(_16,_32), stride=(64,1)
```

### Pallas visible form

Pallas makes the tile visible through block specs and refs:

```python
in_specs = [
    pl.BlockSpec(block_shape=(16, 32), index_map=lambda i, j: (i, 0)),
    pl.BlockSpec(block_shape=(32, 16), index_map=lambda i, j: (0, j)),
]
out_specs = pl.BlockSpec(block_shape=(16, 16), index_map=lambda i, j: (i, j))
```

The kernel receives refs whose shapes are determined by the corresponding
`BlockSpec`.  The developer sees tile shape and placement in `BlockSpec`,
`GridSpec`, and the kernel refs.  Pallas exposes `debug_print` for printing from
inside a kernel, `interpret=True` to run through the JAX interpretation path
for debugging, and `debug=True` to print intermediate forms while Pallas
processes the kernel.  JAX can also dump staged IR through `JAX_DUMP_IR_TO`.

Example debug evidence:

```text
BlockSpec(block_shape=(16, 32), index_map=...)
x_ref shape in kernel: (16,32)
pl.debug_print("tile {}", x_ref[...])
```

### TVM TensorIR visible form

TensorIR makes the tile visible as scheduled loop/block structure and buffer
regions:

```python
for i0, j0, k0 in T.grid(4, 4, 2):
    for ii, jj, kk in T.grid(16, 16, 32):
        with T.sblock("C"):
            vi = T.axis.spatial(64, i0 * 16 + ii)
            vj = T.axis.spatial(64, j0 * 16 + jj)
            vk = T.axis.reduce(64, k0 * 32 + kk)
            T.reads(A[vi, vk], B[vk, vj])
            T.writes(C[vi, vj])
```

The developer-visible tile is not a separate object.  It appears in loop split
factors, block axis bindings, `T.reads`/`T.writes` regions, allocated local or
shared buffers, and tensorization intrinsics.  TVM documents schedule tracing:
developers can print `sch.mod`, call `sch.show()`, and inspect history with
`sch.trace.show()`, which records scheduling steps such as `split`, `reorder`,
`reverse_compute_at`, and reduction decomposition.

Example debug evidence:

```text
l4, l5 = sch.split(loop=l2, factors=[None, 16])
T.reads(A[i0*16 + ii, k0*32 + kk], B[k0*32 + kk, j0*16 + jj])
T.writes(C[i0*16 + ii, j0*16 + jj])
```

### Open64 implication

The example suggests that a WHIRL tile should not be only a comment on a full
tensor.  It should be visible enough to act as an operand or result:

```text
tile_view A_tile:
  origin=A
  origin_region=[0:16, 0:32]
  tile_shape=[16,32]
  element_ty=float32
  layout=strided(row_major), strides=[64,1]
  memory_space=global|shared|register
  boundary=none|mask|zero|nan
  role=matmul.lhs
```

That keeps PyTorch-style semantic tensor identity separate from Triton/CuTe
tile execution facts and TVM-style schedule legality facts.

## Similarities

All systems represent at least dtype and multidimensional shape.  All systems
need some way to distinguish logical tensor extents from physical storage or
access layout.  All systems also need a boundary between user/model semantics
and target-specific lowering decisions.

The LLM kernel DSLs converge on a common lower-level vocabulary:

- block/tile shapes;
- explicit strides or layouts;
- memory space and placement;
- vectorized or tensor-core operations;
- bounds masks or region constraints;
- JIT/AOT specialization around static meta-parameters;
- interop with framework tensors.

## Key Differences

PyTorch is value-semantic and user-facing.  It answers "what tensor value does
the model compute?" better than "which warp owns this tile?"

Triton is block-program oriented.  It answers "what does each program instance
load, compute, and store?" better than "what is the full model-level tensor
identity?"

CuTe is layout and hardware hierarchy oriented.  It answers "how is this
logical tensor partitioned through memory spaces, thread layouts, and hardware
atoms?" better than "what framework graph produced this tensor?"

Pallas is framework-integrated custom-kernel oriented.  It answers "how can a
JAX program express a backend-specific kernel while staying inside tracing?"

TVM TensorIR is compiler-transform oriented.  It answers "what buffer regions
does this block read/write, and what legal schedules/lowerings can transform
it?"

## Implications for Open64 TensorDescriptorIR and whirl2c

Open64 should treat TensorDescriptorIR as a semantic tensor value descriptor,
not merely a C type spelling or a pointer wrapper.  The descriptor should keep
these groups separate:

- Type core: element type, dtype, rank, logical shape.
- Representation: layout, strides, memory placement, alignment, quantization,
  external data, and runtime state.
- Compiler metadata: source names, diagnostics, pass ownership, lowering hints,
  and profiling data.
- Lowering facts: tiling, memory-space mapping, tensor-core eligibility, and
  target-specific scheduling choices.

The PyTorch lesson is that dtype/device/layout/shape metadata must be visible
and validated at the frontend boundary.  The Triton and CuTe lesson is that
kernel lowering needs more precise block, stride, alignment, memory hierarchy,
and tensor-core contract data than a high-level framework tensor normally
contains.  The TVM lesson is two-stage: Relax shows that a framework-connected
compiler benefits from a high-level graph/operator IR first, while TensorIR
shows that verification and transformation are easier once read/write regions,
axes, and schedule ownership become explicit.

For `whirl2c` specifically, the safest early behavior is a diagnostic
projection: print tensor carrier declarations, tile views, and logical DSL
operator names in a stable, reviewable C-extension syntax, while leaving binary
WHIRL and `ir_b2a -st -src` as the normative compatibility gates.  Until DSL
WHIRL is lowered to C-equivalent WHIRL, `whirl2c` should not pretend the output
is ordinary compilable C.

An early output style should therefore prefer a form like:

```c
float32 TENSOR /* tensor[64,64], layout=row_major */ A;
float32 TILE /* origin=A, region=[0:16,0:32], shape=[16,32],
                    strides=[64,1], memory=global, role=matmul.lhs */ A_tile;
C_tile = OPR_DSLMATMUL.v1(A_tile, B_tile)
         /* result_tile=[0:16,0:16], accum=float32 */;
```

That is closer in spirit to Relax plus Triton/CuTe tile visibility than to
TensorIR.  A later lowered `whirl2c` path can emit loop nests, buffer regions,
and C-like statements once the DSL has been converted to canonical WHIRL.

Triton's input form suggests a particularly useful `whirl2c` convention:
separate external tensor identity from tile variables.  The function-level
projection can list full tensor descriptors and strides, while the body creates
tile views with explicit origin, region, and block shape before feeding those
tiles to logical operators:

```c
float16 TENSOR /* tensor[M,K], strides=[stride_am,stride_ak] */ A;
float16 TILE A_blk =
    TILE_VIEW(A,
        /* origin_region=[pid_m*BM:pid_m*BM+BM, k:k+BK],
           shape=[BM,BK], boundary=mask, memory=global */);
acc = OPR_DSLDOT.v1(A_blk, B_blk, acc)
      /* tile_result=[BM,BN], accum=float32 */;
```

This gives reviewers the Triton-like information they actually need: the
external tensor contract, the tile derivation contract, and the logical
tile-level operator.  It avoids pretending that early `whirl2c` has already
chosen C loops, shared-memory placement, or a final target schedule.

## Source Links

- PyTorch tensor overview: https://docs.pytorch.org/docs/stable/tensors.html
- PyTorch tensor attributes: https://docs.pytorch.org/docs/stable/tensor_attributes
- PyTorch tensor stride: https://docs.pytorch.org/docs/stable/generated/torch.Tensor.stride.html
- PyTorch storage model: https://docs.pytorch.org/docs/stable/storage
- PyTorch FX debugging and graph display: https://docs.pytorch.org/docs/stable/fx.html
- PyTorch FX TensorMetadata: https://docs.pytorch.org/docs/main/generated/torch.fx.passes.shape_prop.TensorMetadata.html
- PyTorch `torch.export`: https://docs.pytorch.org/docs/stable/user_guide/torch_compiler/export/api_reference.html
- PyTorch export programming model: https://docs.pytorch.org/docs/main/user_guide/torch_compiler/export/programming_model.html
- Triton documentation: https://triton-lang.org/main/index.html
- Triton language API: https://triton-lang.org/main/python-api/triton.language.html
- Triton tensor API: https://triton-lang.org/main/python-api/generated/triton.language.tensor.html
- Triton load/store and tensor descriptors: https://triton-lang.org/main/python-api/generated/triton.language.load.html
- Triton debugging: https://triton-lang.org/main/programming-guide/chapter-3/debugging.html
- Triton MAPL/PLDI 2019 paper page: https://research.ibm.com/publications/triton-an-intermediate-language-and-compiler-for-tiled-neural-network-computations
- Triton MAPL/PLDI 2019 paper PDF: https://www.eecs.harvard.edu/~htk/publication/2019-mapl-tillet-kung-cox.pdf
- OpenAI Triton 1.0 programming model and matmul examples: https://openai.com/index/triton/
- Triton matrix multiplication tutorial: https://triton-lang.org/main/getting-started/tutorials/03-matrix-multiplication.html
- NVIDIA CuTe DSL overview: https://docs.nvidia.com/cutlass/latest/media/docs/pythonDSL/overview.html
- NVIDIA CuTe DSL introduction: https://docs.nvidia.com/cutlass/latest/media/docs/pythonDSL/cute_dsl_general/dsl_introduction.html
- NVIDIA CuTe DSL debugging: https://docs.nvidia.com/cutlass/latest/media/docs/pythonDSL/cute_dsl_general/debugging.html
- NVIDIA CuTe C++ tensor docs: https://docs.nvidia.com/cutlass/latest/media/docs/cpp/cute/03_tensor.html
- NVIDIA CuTe C++ layout docs: https://docs.nvidia.com/cutlass/latest/media/docs/cpp/cute/01_layout.html
- JAX Pallas: https://docs.jax.dev/en/latest/pallas/index.html
- JAX Pallas `debug_print`: https://docs.jax.dev/en/latest/_autosummary/jax.experimental.pallas.debug_print.html
- JAX export and IR dumps: https://docs.jax.dev/en/latest/export/export.html
- TVM TensorIR: https://tvm.apache.org/docs/deep_dive/tensor_ir/index.html
- TVM TensorIR transformation tracing: https://tvm.apache.org/docs/deep_dive/tensor_ir/tutorials/tir_transformation.html
- TVM Relax frontend and PyTorch importers: https://tvm.apache.org/docs/reference/api/python/relax/frontend.html
- TVM importing models from PyTorch: https://tvm.apache.org/docs/how_to/tutorials/import_model.html
- TVM IRModule import and `LegalizeOps`: https://tvm.apache.org/docs/get_started/tutorials/ir_module.html
- TVM Relax VM architecture: https://tvm.apache.org/docs/arch/relax_vm.html
- TVM design and architecture, Relax and TensorIR roles: https://tvm.apache.org/docs/arch/
- TVMScript: https://tvm.apache.org/docs/arch/tvmscript.html
