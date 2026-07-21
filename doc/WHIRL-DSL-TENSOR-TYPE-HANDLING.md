# WHIRL DSL Tensor Type Handling

## Purpose

This document defines how Open64 represents, creates, canonicalizes, verifies,
persists, prints, and lowers tensor types in Very High Level WHIRL. It is the
focused engineering reference for tensor type work. The broader architectural
direction remains in `Open64_Domain_Specific_Compiler_IR_Design.md` and
`WHIRL-DSL-INFRASTRUCTURE.md`.

The design follows four rules:

1. A tensor is a first-class WHIRL `TY_IDX`, not untyped operator metadata.
2. Facts needed to interpret tensor computation belong to TensorDescriptorIR.
3. Source context and compiler policy do not participate in tensor type
   equivalence.
4. A type or alignment field is a correctness guarantee, not an unsupported
   optimization promise.

## Current Representation

The native representation uses `TY_KIND = KIND_TENSOR` with `MTYPE_M` and
`TY_size = 0`. The zero size means physical storage has not been selected at
Very High Level WHIRL. It does not mean the logical tensor contains zero
elements.

Each tensor `TY_IDX` is associated with a tensor extension record containing:

- the owning `TY_IDX`;
- the element `TY_IDX`;
- rank;
- the head and count of its descriptor attributes.

`TY_Create_Tensor_Extension_Type()` remains an additive compatibility entry
point using a `KIND_STRUCT` carrier. New native DSL construction should use
`TY_Create_Tensor_Type()` or the higher-level builder APIs. Both carriers use
the same tensor extension queries and descriptor rules.

The principal definitions are in:

| Concern | Location |
| --- | --- |
| `KIND_TENSOR`, `TY` and table layouts | `osprey/common/com/symtab_defs.h` |
| Tensor schemas and public `TY`/`ST` APIs | `osprey/common/com/symtab.h` |
| Tensor table and canonicalization implementation | `osprey/common/com/symtab.cxx` |
| Frontend-safe construction APIs | `osprey/common/com/dsl_builder.h` and `.cxx` |
| Gatekeeper checks | `osprey/common/com/dsl_gatekeeper.cxx` |
| Logical DSL value image | `osprey/common/com/dsl_ir_image.h` and `.cxx` |
| Logical printing | `osprey/common/com/dsl_ir_print.cxx` |
| Binary WHIRL mapped-image movement | `osprey/common/com/ir_bcom.cxx` and `ir_bread.cxx` |

## Semantic Layers

TensorDescriptorIR is the semantic tensor value state visible to DSL-aware
compiler passes. Its architectural decomposition is:

| Layer | Current contents |
| --- | --- |
| `TensorTypeCore` | kind, dtype, rank, logical shape |
| `TensorTraitSet` | semantic role and domain traits |
| `TensorRepresentationDescriptor` | layout, sharding, placement, memory, quantization, runtime state |
| `TensorLineageMetadata` | provenance and transformation history |

The current `TENSOR_DESCRIPTOR_RECORD` is a fixed projection over the `TY_IDX`
tensor extension and its KV entries. Its component IDs reserve the shape of a
future more strongly normalized table design; they do not currently introduce
separate binary sections for every component.

Compiler metadata remains separate. Source layer names, diagnostics, pass
ownership, lowering hints, profiles, and source positions belong to `ST_IDX`,
WN mappings, or compiler metadata tables. They must not silently create a new
tensor type.

Opcode attributes are also separate. Parameters such as convolution stride,
padding, attention axis, and matmul transpose mode describe a particular
operation. They are not tensor descriptor fields unless analysis proves and
publishes a resulting tensor representation fact.

## Creation And Canonicalization

Frontends should normally create tensor types through:

```c++
TY_IDX DSL_Builder_Intern_Tensor_Type(
    const char *name,
    TY_IDX element_ty,
    const DSL_BUILDER_TENSOR_DESCRIPTOR *descriptor);
```

The builder performs this sequence:

1. Validate kind, dtype, rank, shape, and element type.
2. Create a native tensor `TY_IDX`.
3. Attach type-core, trait, and representation attributes.
4. Seal the descriptor as canonical and immutable.
5. Search existing canonical tensor types using normal `TY` equivalence while
   ignoring names.
6. Return the existing `TY_IDX` when an equivalent descriptor already exists.

The current canonical type intentionally excludes `runtime_state` and lineage
from interning. Those fields can vary with execution or provenance without
changing the reusable tensor type. A later schema may promote a field into type
equivalence only through an explicit compatibility review.

After `TY_tensor_seal()`, descriptor mutation is rejected. Code that needs a
different semantic type must request or intern another descriptor instead of
changing a canonical `TY_IDX` in place.

Lower-level construction remains available for staged inference:

```c++
TY_IDX tensor_ty = DSL_Builder_Create_Tensor_Type_Core(
    name, element_ty, &type_core);
DSL_Builder_Attach_Tensor_Descriptor(tensor_ty, &descriptor);
```

This path may contain pending attributes before sealing. Every required field
must be bound before gatekeeper verification or computation relies on it.

## Values, Symbols, And Ownership

A tensor type does not allocate tensor data. A DSL expression returns a value,
and an enclosing `STID` assigns that value to a temporary tensor symbol:

```text
STID result_temp : tensor_ty
  OPR_DSLADD kid0, kid1
```

`DSL_Builder_Create_Tensor_Result_Symbol()` creates a `CLASS_VAR`, marks it as
`ST_IS_TEMP_VAR`, and records unique ownership through the tensor symbol
attribute `no_alias=true`. This is a semantic no-alias contract for the result
value. It is not the baseline pointer-only `ST_PT_TO_UNIQUE_MEM` flag.

Physical allocation, addressability, and storage class selection remain
deferred. Lowering must preserve unique ownership when it selects an opaque
runtime handle, stack object, heap allocation, external buffer, or target
kernel ABI.

## Static, Symbolic, And Dynamic Shapes

Rank and dimension values have separate completeness states. A tensor shape
must distinguish:

| Dimension form | Example | Meaning |
| --- | --- | --- |
| Static | `128` | Compile-time constant positive extent |
| Symbolic | `L` | Named extent with stable identity and constraints |
| Symbolic expression | `L+1` | Expression over symbols and constants |
| Runtime dynamic | `S_runtime` | Symbol whose value is supplied by a verified runtime descriptor |
| Pending | `<pending>` | Inference has not supplied a usable dimension yet |

Pending is not a valid substitute for symbolic. A pending dimension makes the
descriptor incomplete for computations that require it. A symbolic dimension
is complete when its identity, scope, and required constraints are present,
even when its numeric value is unavailable at compile time.

### Adopted pending-shape representation

The current staging decision is to represent an extent that shape inference
has not determined as a tagged pending dimension. `ir_b2a -st -src` prints that
state as `<pending>`. The text is an ASCII projection only; `<pending>` is not
stored as the dimension value in TensorDescriptorIR.

The internal representation shall distinguish rank completeness from the state
of each dimension:

```c++
enum TENSOR_RANK_STATE {
    TENSOR_RANK_PENDING = 0,
    TENSOR_RANK_KNOWN
};

enum TENSOR_DIMENSION_KIND {
    TENSOR_DIM_PENDING = 0,
    TENSOR_DIM_STATIC,
    TENSOR_DIM_ANONYMOUS_DYNAMIC,
    TENSOR_DIM_SYMBOL,
    TENSOR_DIM_EXPRESSION
};

struct TENSOR_DIMENSION_RECORD {
    TENSOR_DESCRIPTOR_ID tensor_descriptor;
    UINT32 ordinal;
    TENSOR_AXIS_ROLE role;
    TENSOR_DIMENSION_KIND kind;
    UINT32 payload_id;
};
```

`payload_id` identifies a constant, shape symbol, or canonical dimension
expression according to `kind`. It is zero for a pending dimension or an
anonymous dynamic dimension. These are fixed indexed records suitable for the
WHIRL table and mapped-image model; the enums and field layout require the
normal binary compatibility review before implementation.

The corresponding ASCII projections are:

| Internal state | `ir_b2a` projection | Meaning |
| --- | --- | --- |
| rank pending | `<pending>` | Shape inference has not determined rank |
| `TENSOR_DIM_PENDING` | `<pending>` | This extent is not yet determined |
| `TENSOR_DIM_STATIC(128)` | `128` | Compile-time extent |
| `TENSOR_DIM_ANONYMOUS_DYNAMIC` | `?` | Valid runtime extent without identity |
| `TENSOR_DIM_SYMBOL` | `S_req` | Named symbolic extent |
| `TENSOR_DIM_EXPRESSION` | `S_req+1` | Derived symbolic extent |

For this implementation stage, `<pending>` is the required unresolved-shape
notation. There is no current model requirement that justifies implementing
named symbols such as `S_req`. The tagged design reserves `SYMBOL` and
`EXPRESSION` kinds so they can be introduced later without confusing a valid
symbolic dimension with an incomplete pending one.

Examples:

```text
rank not determined       : rank=<pending>, logical_shape=<pending>
rank 3, extents pending   : tensor<float>[<pending>,<pending>,<pending>]
partially inferred        : tensor<float>[1,<pending>,128]
fully inferred            : tensor<float>[1,32,128]
```

A pending dimension may exist while a frontend or inference pass constructs a
descriptor. It must not be sealed as a canonical tensor type or pass the
gatekeeper when an operation requires a complete shape. The mapped-image
framework may preserve pending records for staged tools, but a certified
frontend `.B` artifact must satisfy the completeness required by its published
operator contracts.

The current implementation stores `logical_shape` as a string and most builder
and gatekeeper shape parsers accept only positive integer dimensions. It
therefore supports static shape semantics today. A string such as `[B,L,D]`
may be retained for diagnostics, but it is not yet a compiler-understood
symbolic representation and must not pass symbolic legality checks merely
because the string is present.

### Future symbolic refinement: Llama 2 decode

The current stage may retain unresolved decode extents as `<pending>`. If a
future model requirement justifies named symbolic dimensions, the sequence
length before appending the current token is a natural symbol `L`:

```text
token_ids       : tensor<int64>[B,1]
cache_position  : tensor<int64>[1] where value(cache_position[0]) = L
key_cache       : tensor<float>[B,Hkv,L,D]
value_cache     : tensor<float>[B,Hkv,L,D]
query           : tensor<float>[B,Hq,1,D]
updated_key     : tensor<float>[B,Hkv,L+1,D]
updated_value   : tensor<float>[B,Hkv,L+1,D]
attention       : tensor<float>[B,Hq,1,D]
```

The decode contract also needs constraints such as:

```text
B > 0
L >= 0
D > 0
Hq > 0
Hkv > 0
cache_position = L
L < rope_capacity
updated_cache.sequence = cache.sequence + 1
```

For the currently certified profile, `Hq = Hkv`. Grouped-query attention may
later permit `Hq` to be a multiple of `Hkv`; that relationship belongs to the
versioned attention contract rather than to an untyped shape string.

Using one stable symbol identity for `L` lets the gatekeeper prove that the key
and value caches agree, RoPE uses the correct position, functional append grows
the cache by one, and attention observes the appended sequence. Repeating the
text `L` in unrelated scopes must not accidentally make two dimensions equal.

### Planned symbolic representation

Symbolic shape support should follow WHIRL table discipline rather than place
a process-local expression graph or nested STL container inside a persistent
record. The planned representation should use fixed indexed records for:

1. Dimension symbols, including stable ID, name, owner PU/REGION, and flags.
2. Dimension expressions, including opcode and fixed operand IDs.
3. Tensor shape dimensions, mapping tensor descriptor and ordinal to an
   expression ID.
4. Shape constraints, including equality, inequality, divisibility, and
   optional diagnostic ownership.

The minimum expression set should be deliberately small:

```text
constant, symbol, add, subtract, multiply-by-constant,
floor-divide-by-constant, minimum, maximum
```

New operations should be added only when an ingested model and verifier need
them. Expression records must be canonicalized so equivalent expressions can
be compared without reparsing text. Symbol equivalence is based on stable
identity and scope, not only on `STR_IDX` spelling.

The current `logical_shape` string remains the human-readable projection. For
a symbolic descriptor, `ir_b2a -st -src` should print both the shape and its
constraints deterministically:

```text
logical_shape = [B,Hkv,L+1,D]
dimension_symbols = B,Hkv,L,D
constraints = B>0,Hkv>0,L>=0,D>0
```

### Planned builder APIs

The API should preserve opaque handles at the Python boundary. A possible
native surface is:

```c++
DSL_DIM_SYMBOL DSL_Builder_Declare_Dimension_Symbol(
    DSL_BUILDER_PROGRAM_UNIT pu,
    DSL_BUILDER_REGION region,
    const char *name,
    UINT32 flags);

DSL_DIM_EXPR DSL_Builder_Dimension_Constant(INT64 value);
DSL_DIM_EXPR DSL_Builder_Dimension_Symbol(DSL_DIM_SYMBOL symbol);
DSL_DIM_EXPR DSL_Builder_Dimension_Add(
    DSL_DIM_EXPR kid0,
    DSL_DIM_EXPR kid1);

BOOL DSL_Builder_Set_Tensor_Symbolic_Shape(
    TY_IDX tensor_ty,
    const DSL_DIM_EXPR *dimensions,
    UINT32 rank);

BOOL DSL_Builder_Add_Shape_Constraint(
    DSL_BUILDER_REGION region,
    DSL_SHAPE_CONSTRAINT_KIND kind,
    DSL_DIM_EXPR kid0,
    DSL_DIM_EXPR kid1);
```

These names are a design sketch, not published APIs. Before implementation,
the binary tables, versioning, mapped-image reader/writer behavior, ASCII
printing, canonicalization, and compatibility migration must be reviewed
together.

### Canonicalization and lowering

Two symbolic tensor descriptors are equivalent only when their dimension
expressions and symbol identities are equivalent under the published scoping
rules. Compiler metadata such as the original Python variable name does not
affect that equivalence.

Lowering may specialize a symbolic descriptor when constraints and call-site
facts bind symbols to constants. Otherwise it must pass the required runtime
dimension values through the selected descriptor or kernel ABI. No pass may
silently replace a symbolic extent with the sample-input extent used during FX
capture.

## Logical Axes And Physical Storage

Rank says how many logical axes a tensor has. It does not, by itself, say what
an axis means or how its elements are arranged in memory. A complete tensor
description must keep three related facts distinct:

| Fact | Example | Semantic owner |
| --- | --- | --- |
| Dimension extent | axis 2 has extent `L` | `TensorTypeCore` / `TensorShapeDescriptor` |
| Axis role | axis 2 is the cached-token sequence | `TensorTraitSet` / tensor axis semantics |
| Physical mapping | logical axis 2 maps to physical axis 2 with stride `D * sizeof(element)` | `TensorRepresentationDescriptor` |

This separation follows the master design: logical shape describes dimensions
such as request, head, sequence, and head dimension, while runtime or physical
layout describes dense order, pages, blocks, strides, and allocation handles.
An axis role is semantic information. It must not be inferred only from its
ordinal or from a one-letter dimension name.

### Transformer axis examples

The same semantic role can occur at different ordinals in different tensors:

| Tensor | Logical shape | Axis roles in ordinal order |
| --- | --- | --- |
| Token IDs | `[B,S]` | batch, token-sequence |
| Hidden state | `[B,S,H]` | batch, token-sequence, hidden-feature |
| Query in BHSD form | `[B,Hq,Sq,D]` | batch, query-head, query-sequence, head-feature |
| Key/value cache in BHSD form | `[B,Hkv,L,D]` | batch, KV-head, cached-sequence, head-feature |
| Logits | `[B,S,V]` | batch, token-sequence, vocabulary |

During one-token Llama 2 decode, token IDs have sequence extent `1`, while the
cached-sequence axis has runtime-evolving extent `L` and the updated cache has
extent `L+1`. The sequence role is axis 1 for token IDs and hidden states, but
axis 2 for a BHSD cache. Gatekeeper checks must compare semantic roles and
dimension expressions rather than assume that equal ordinals have equal
meaning.

The layout name is part of the contract. A cache declared as BSHD instead of
BHSD has logical shape `[B,L,Hkv,D]`; its cached-sequence role is axis 1. An
operator attribute such as `cache_sequence_axis=2` may describe one operator
contract today, but it is not a substitute for reusable tensor axis semantics.

### Addressing and storage order

For a strided dense view, the address of logical element `(i0, ..., in)` is:

```text
address = base + byte_offset + sum(ik * byte_stride[k])
```

The logical axis records determine what each `ik` means. The representation
descriptor determines physical order and `byte_stride[k]`. In a contiguous
row-major BHSD cache, the `D` axis is contiguous, followed by sequence, head,
and batch. A transpose may change the logical-to-physical mapping without
moving data; a materialized layout conversion creates new storage. A paged KV
cache keeps the same logical cached-sequence axis but maps it through block
tables and in-page offsets instead of one affine dense stride.

Consequently, an axis role does not imply contiguity. The compiler must not
assume that tokens are adjacent merely because an axis has the token-sequence
role. Dense, strided, blocked, tiled, packed, and paged representations require
their own physical mapping contracts.

### Planned normalized axis representation

The current implementation has rank, a human-readable `logical_shape` string,
and representation attributes such as layout. It does not yet have the adopted
`TENSOR_DIMENSION_RECORD` table or a compiler-understood table that assigns a
semantic role and physical mapping to every axis. The fixed-record design
should associate each tensor descriptor and axis ordinal with:

1. A dimension-expression ID.
2. A versioned semantic axis role.
3. An optional physical-axis ordinal.
4. An optional byte-stride expression or non-affine layout reference.
5. Flags for broadcast, reduction, sharding, or runtime-evolving behavior.

The record must use indexed WHIRL tables, stable enum values, mapped-image
movement, deterministic printing, and canonicalization rules. It must not use
nested process-local containers. Dimension extent and semantic role participate
in semantic compatibility. Physical order and strides participate in
representation compatibility and should create a distinct canonical tensor
type only when the published type-equivalence rules require them to do so.

A possible native API surface is:

```c++
BOOL DSL_Builder_Set_Tensor_Axis(
    TY_IDX tensor_ty,
    UINT32 ordinal,
    DSL_TENSOR_AXIS_ROLE role,
    DSL_DIM_EXPR extent);

BOOL DSL_Builder_Set_Tensor_Axis_Stride(
    TY_IDX tensor_ty,
    UINT32 ordinal,
    DSL_DIM_EXPR byte_stride);

BOOL DSL_Builder_Set_Tensor_Layout(
    TY_IDX tensor_ty,
    DSL_TENSOR_LAYOUT layout);
```

These are design sketches, not published APIs. The initial role vocabulary
should cover batch, token sequence, cached sequence, query head, KV head,
hidden feature, head feature, vocabulary, channel, spatial height, and spatial
width. New roles and enum values require the same compatibility discipline as
DSL operator names and versions.

## Alignment Model

Alignment is not one number with one owner. The compiler must distinguish the
following contracts:

| Alignment | Meaning | Owner | Type equivalence |
| --- | --- | --- | --- |
| Element-required | Minimum legal alignment of one element | element `TY_IDX` | Yes |
| Tensor-required | Minimum guaranteed for every object of the tensor type | tensor `TY_align` | Yes |
| Object-guaranteed | Stronger alignment proved for one allocation or external object | symbol/allocation or value representation | No, unless guaranteed for the whole type |
| View-effective | Alignment remaining after a slice, offset, or subview | tensor value/view analysis | No |
| Target-preferred | Alignment desired by a vector, accelerator, or kernel implementation | target lowering policy | No |
| Selected-access | Alignment used by the emitted load/store operation | lowered WHIRL or target instruction | No |

### Current required-alignment rule

`TY_Create_Tensor_Type()` and the compatibility tensor extension constructor
set:

```c++
Set_TY_align(tensor_ty, TY_align(element_ty));
```

Therefore an `int32` or `float32` tensor has required alignment 4, while an
`int64` or `float64` tensor has required alignment 8. This replaces the former
implicit one-byte alignment without changing the `TY` record layout.

Symbolic dimensions do not weaken this minimum: `tensor<float32>[B,L,D]` still
has required alignment 4 because alignment is derived from the element type,
not from the eventual extent. Symbolic shape may prevent compile-time byte-size
calculation and allocation, but it does not make the element alignment unknown.

This rule is deliberately conservative. Existing WHIRL lowering and data
layout code consume `TY_align` as a correctness guarantee. Setting it to 16,
32, 64, or another hardware preference would be unsound unless every object of
that type actually satisfies the stronger boundary.

### Preferred hardware-load alignment

Target lowering should compute a preferred alignment from the selected
instruction or library/kernel ABI. It may use that preference only after
proving all of the following:

1. The allocation or external buffer base has sufficient alignment.
2. The tensor view offset preserves that alignment.
3. Strides keep every vectorized access legal.
4. The selected instruction and target ABI accept the resulting address.

For a view at byte offset `O`, its effective power-of-two alignment cannot be
stronger than both the base guarantee and the largest power of two dividing
`O`. A reshape that preserves storage may preserve alignment; a slice or
transpose must be analyzed rather than assumed.

When the stronger proof is unavailable, lowering must use an unaligned access,
a narrower legal access, a copy into aligned storage, or a runtime/kernel path
whose ABI accepts the actual alignment.

External tensor constants currently verify that byte offsets are aligned to
the element size and that byte lengths match the static tensor size. This is
enough for the tensor-required alignment but does not prove vector or cache-line
alignment. A future external-buffer contract should carry and verify a base
alignment guarantee before target lowering relies on one.

### Planned alignment APIs

Future work should introduce APIs with explicit semantics rather than
overloading `TY_align`:

```c++
UINT32 TY_tensor_required_alignment(TY_IDX tensor_ty);
UINT32 ST_tensor_guaranteed_alignment(ST_IDX tensor_st);
UINT32 DSL_Tensor_Value_Effective_Alignment(DSL_BUILDER_VALUE value);
UINT32 DSL_Target_Preferred_Tensor_Alignment(
    const DSL_TENSOR_ACCESS_REQUIREMENT *access);
```

Names and ownership require review before implementation. In particular,
target-preferred alignment is lowering policy, not TensorDescriptorIR semantic
identity.

## External Tensor Data

External tensor constants use a tensor `TY_IDX` plus an external reference:

- storage format;
- side-file path;
- tensor key;
- byte offset and length;
- optional checksum.

The descriptor supplies dtype, rank, and shape. The builder verifies static
byte size, element-aligned offset, nonzero length, overflow, and checksum form.
Storage location details are attached to the result symbol as compiler/runtime
metadata and do not change the canonical tensor type.

The binary WHIRL artifact remains independently readable after Python exits.
External payload bytes remain in their named side file and are referenced by
the WHIRL value contract.

## Binary WHIRL Image

Tensor support uses the existing mapped-image and ELF WHIRL framework. The
global symbol-table image contains fixed table entries for:

- `SHDR_TY_TENSOR_EXT`;
- `SHDR_ST_TENSOR_METADATA`;
- `SHDR_TENSOR_DSL_KV`.

The writer places these tables in the global symbol-table image, and the reader
maps or transfers them into the corresponding Open64 tables. This is not an
independent object-stream format.

The tensor alignment correction uses the pre-existing alignment bits in
`TY_IDX`; it adds no section and changes no record size. Older binary WHIRL
files whose tensor types carry alignment 1 remain readable. Newly created
tensor types use element-required alignment. Compatibility tests must cover
both old-image readability and new-image inspection.

## Verification

Gatekeeper verification should reject a tensor value when:

- its `TY_IDX` is missing or is not a tensor extension;
- required descriptor fields are absent or pending;
- dtype disagrees with the element `TY_IDX`;
- rank or static shape is malformed;
- a symbolic shape lacks stable dimension identities or required constraints;
- an axis ordinal is outside the tensor rank, lacks a required semantic role,
  or disagrees with the operator's versioned axis contract;
- a physical-axis mapping is not a legal permutation or its stride/layout
  contract is inconsistent with the logical shape;
- an unresolved pending dimension reaches an operation that requires a
  complete symbolic or static shape;
- operator operands violate shape, dtype, layout, or domain contracts;
- external data offset or length violates its published storage contract;
- a result expected to have unique ownership lacks that assertion;
- lowering would rely on alignment stronger than the compiler has proved.

The initial alignment regression checks that a tensor type's required
alignment equals its element type alignment. Future target-access tests should
separately cover aligned, unaligned, sliced, and externally backed tensors.

## Inspection

`ir_b2a -st -src` is the human-review interface. For tensor values it should
show:

- the tensor `TY_IDX`, element type, rank, and required alignment;
- each logical axis ordinal, semantic role, extent expression, physical
  mapping, and stride when present;
- TensorDescriptorIR fields;
- symbols and unique-ownership evidence;
- logical operators and source positions;
- external data references when present.

Pending rank and extents must appear as `<pending>`. The printer must derive
that spelling from internal state tags and must not expose numeric sentinels,
empty string-table entries, or table implementation details. The surrounding
field or axis position distinguishes a pending rank from a pending extent.

Example evidence for a `float32` tensor is:

```text
Variable of type input0_type (#53, KIND_TENSOR)
    Alignment: 4 bytes
input0_type : TENSOR element .predef_F4 (#10) align 4
```

The `.B` file stem must also be used for the `.T` output, and the original
source path must remain available so `-src` can interleave source statements.

### whirl2c Diagnostic Projection

`whirl2c` is part of the Open64 inspection and review surface for DSL WHIRL,
but early DSL output is a C-like diagnostic projection rather than a normative
source program that must compile.  The authoritative artifact remains binary
WHIRL, and `ir_b2a -st -src` remains the compatibility gate for mapped-image,
symbol-table, source-correlation, and logical DSL evidence.

For tensor declarations, `whirl2c` should spell the carrier as
`TENSOR` and attach a comment projection derived from
TensorDescriptorIR facts on the `TY_IDX`.  The projection may show element type,
rank, shape, layout, placement, memory, quantization, runtime state, lineage,
and similar descriptor fields.  It must not create a new ABI type contract,
change tensor type equivalence, add a binary section, or expose private tensor
table storage details.

For native DSL expressions, `whirl2c` should use the logical DSL accessor APIs
and stable logical names, matching the `ir_b2a` rule that ordinary dumps do not
print the physical `OPR_DSL` escape tag, `OPC_MDSL`, `MDSL`, or private record
IDs.  Exact formatting can evolve with shared printer helpers, but the first
requirement is reviewable, deterministic evidence that keeps domain operators
visible until gatekeeper verification and lowering consume them.

## Change Checklist

Any tensor type handling change should answer these questions:

1. Is the fact type-wide, object-specific, value-specific, operation-specific,
   compiler-only, or target-only?
2. Does it participate in canonical type equivalence?
3. Is it a guaranteed property or only a preference?
4. Which builder and query APIs own it?
5. How is it represented in the mapped binary WHIRL image?
6. How does `ir_b2a -st -src` expose it?
7. What does the gatekeeper reject?
8. Are old binary images still readable?
9. Are common-substrate and domain-specific legality checks preserved?
10. Do regression tests prove both semantics and inspection output?
11. If dimensions are symbolic, where are symbol identity, scope, expressions,
    constraints, and runtime bindings represented and verified?
12. For every axis, are logical extent, semantic role, and physical storage
    mapping represented separately and printed deterministically?
