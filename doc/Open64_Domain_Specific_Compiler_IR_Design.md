---
marp: true
theme: default
paginate: true
backgroundColor: #ffffff
---
# Domain Specific Compiler IR Master Design

This document is an incremental coding plan for introducing a domain-specific
compiler IR layer on top of WHIRL.  The design should land in stages so the
compiler remains buildable and each stage has a clear validation point.

## Guiding Model

The DSL IR layer should preserve domain intent without forcing an early WHIRL
lowering decision.  WHIRL remains the executable IR backbone, while DSL
extensions add semantic information that later passes can lower to canonical
WHIRL, runtime calls, target intrinsics, or target-specific machine types.
This plan is aligned with the Word design document
`DSC_Master_Design_Doc_v0.10.docx`, especially the Chapter 7 Python
DSL ingestion architecture, but stages the namespace-heavy architecture
carefully for the existing Open64 code base.

The current staging model is:

```text
Python model / DSL
  -> torch.export / FX / DSC capture graph
  -> WhirlExportInterpreter
  -> C++ WHIRL builder extension
  -> binary very-high-level WHIRL artifact
  -> first-class domain/common operators
  -> TensorDescriptorIR-completing attributes and compiler metadata
  -> VHO-adjacent DSL lowering
  -> canonical High/Mid/Low WHIRL
```

The Python process is a source-language frontend and import-time normalization
layer.  It must not become the compiler middle end.  The C++ WHIRL builder owns
WHIRL node creation, symbol and type table construction, tensor descriptor
registration, opcode attributes, contracts, metadata, and mapped binary image
finalization.  The binary WHIRL artifact is the boundary between Python capture
and the Open64 pipeline.

The long-term type model is:

```text
Common tensor type:
  TY_KIND = KIND_TENSOR
  semantic tensor attributes

Tensor object symbol:
  ST_IDX
  compiler metadata such as source context, diagnostics, and lowering hints

Target-lowered tensor fragment:
  target-specific MTYPE or ABI representation
  e.g. NVIDIA Tensor Core WMMA/MMA fragment
```

`TensorDescriptorIR` is owned by the `TY` domain.  A producer first completes
the descriptor, including its element type, rank, logical shape, traits, and
representation facts, and then asks the type system to canonicalize it.  The
canonicalization operation hashes and compares the complete descriptor,
returns the existing `TY_IDX` for an equal descriptor, or installs one new
`KIND_TENSOR` entry.  A canonical descriptor is frozen: clients must not append
or mutate identity-bearing attributes after it enters the type-uniqueness
table.  Thus equal tensor descriptors share one `TY_IDX`, while unequal
descriptors denote distinct tensor types.

The current implementation is only a staging foundation for that rule.
`Hash_ty_tensor_table` exists, but its hash currently covers only element type
and rank, and its equivalence test covers only element type, rank, and
attribute count.  Counting attributes is not descriptor equivalence.  The
canonical API must compare normalized key/value contents and must replace the
incremental create-then-mutate pattern with build, seal, and intern operations.
Compatibility wrappers may retain the old API temporarily, but must intern the
completed descriptor before exposing it as canonical.

Source position is not tensor state or auxiliary compiler metadata.  For a
statement-level WN, the authoritative source position is the existing 64-bit
`SRCPOS` value in the statement WN slot accessed historically through
`WN_Set_Linenum`, `WN_Get_Linenum`, and `WN_linenum`.  DSL statement wrappers,
including stores, regions, pragmas, and barriers, must propagate that value.
Source module paths and graph-node names may remain compiler metadata for
diagnostics, but they do not replace the WN source position.

The long-term opcode model is:

```text
Domain opcode namespace
  -> domain contract and gatekeeper verification
  -> optional common opcode wrapper/lowering
  -> target/runtime lowering
```

The Common Compiler Substrate owns domain-neutral compiler and tensor
semantics.  Domain namespaces own domain-specific ABI, gatekeeper checks,
conformance tests, and diagnostics.  CNN and Transformer domains may therefore
share opcodes such as `common.linear`, `common.residual_add`,
`common.layout_cast`, and `common.dispatch`, while preserving wrappers such as
`cnn.linear`, `cnn.conv2d`, `transformer.q_projection`, and
`transformer.attention` when domain-specific meaning matters.

Implementation similarity is not semantic equivalence.  For example, CNN
convolution and Transformer attention may both lower to tensor contractions,
but they must remain visible as domain operations until their padding, layout,
mask, position, runtime-state, and ABI contracts have been verified.

## Continuous IR Tool Compatibility

Every stage must keep `osprey/ir_tools/ir_a2b.cxx` working.  This source covers
both the ASCII-to-binary `ir_a2b` flow and the binary-to-ASCII `ir_b2a` flow, so
it is a compatibility gate for the public WHIRL node, symbol-table, ASCII
reader/printer, and binary IR image assumptions used by existing tools.

Open64 binary IR is not a portable interchange encoding.  The binary IR file is
an image of the compiler's in-memory WHIRL-related data structures, and the
reader relies on mapping that image back into memory, including mmap-based
paths.  The DSL plan must therefore preserve binary image layout compatibility
within Open64's existing assumptions instead of introducing a new
cross-platform file format.

Rules:

1. Stages must not introduce required DSL syntax that `ir_a2b` cannot preserve,
   ignore safely, or diagnose deterministically.
2. Early DSL metadata should use legal existing WHIRL constructs, such as
   `OPR_COMMENT` markers and side tables keyed by existing IDs.
3. Any future binary IR image layout change, especially `TY_KIND = KIND_TENSOR`,
   must be accompanied by ASCII reader/printer, binary image writer, binary
   image reader, dumper, and verifier updates before that stage exits.
4. `ir_a2b` should be part of each stage's smoke test: ASCII WHIRL containing
   existing constructs must still move through `ir_a2b` and `ir_b2a`, and
   DSL-marked WHIRL must either survive that path or fail with a clear
   unsupported-feature diagnostic.

## Staged Driver Artifact Boundary

The first DSL ingestion workflow should be a two-step artifact flow:

```text
torch2whirl model.py -o model.B
opencc -x whirl model.B ...
```

`torch2whirl` is the Python-facing frontend command.  It owns Python model
capture, graph normalization, source mapping, and calls into the native WHIRL
builder to produce a binary very-high-level WHIRL artifact.  Its output is a
compiler artifact, not an in-process Python object that the Open64 middle end
must understand.

`opencc -x whirl model.B ...` is the Open64 compiler invocation over that
artifact.  This preserves the same architectural split used by other Open64
frontends: source-language ingestion finishes before the middle-end pipeline
starts.  It also makes validation easier because developers can inspect
`model.B` with `ir_b2a -st`, compare mapped-image sections, and reproduce
compiler behavior without re-running Python capture.

The combined driver mode is deferred:

```text
opencc -frontend=torch2whirl model.py ...
```

That mode should be only a driver convenience wrapper around the same artifact
boundary.  It must not become a special in-memory bypass around binary WHIRL
finalization, `ir_b2a` visibility, TensorDescriptorIR persistence, or gatekeeper
validation.  The combined mode can start only after the two-step flow proves
that the WHIRL artifact boundary is stable, inspectable, and compatible with
existing Open64 IR tools.

Near-term driver requirements:

1. `torch2whirl` writes a binary very-high-level WHIRL file.
2. The file is inspectable with `ir_b2a -st`.
3. TensorDescriptorIR state, opcode attributes, contracts, compiler metadata,
   and source mapping survive the artifact boundary.
4. `opencc -x whirl` consumes the artifact without importing Python.
5. Combined `opencc -frontend=torch2whirl ...` mode remains deferred until the
   two-step path is validated.

## Namespace Staging Policy

The Word design uses namespace examples such as
`ant::domain::transformer::rl_posttraining::rollout` to describe hierarchical
domain ownership.  In this Open64 implementation plan, those names should first
be represented as semantic domain identifiers, for example
`ant.domain.transformer.rl_posttraining.rollout`, and registered through a
domain registry.  They should not immediately become C++ namespaces in the
common WHIRL implementation.

Some newer Open64 islands already use C++ namespaces, for example `clang2whirl`
and WSSA utilities.  The core WHIRL and symbol-table layers, especially
`osprey/common/com`, still expose mostly global C/C++ APIs and are included by
old frontends, backends, IPA, and tools.  The DSL IR design may use namespaces
in new leaf components, but namespace adoption must be staged carefully and
must not force a broad common-core API migration.

Rules:

1. Do not wrap existing `common/com` headers or exported WHIRL APIs in a new
   namespace.
2. New common-core APIs should follow existing naming style, such as
   `WN_Create_DSL_Comment` and `TY_tensor_bind_attribute`.
3. Namespace-based C++ classes may be introduced first in new leaf modules,
   adapters, or experimental DSL lowering components.
4. Any namespaced implementation that must be called from old Open64 code must
   expose a small global wrapper API.
5. Namespace migration of existing code is a separate refactor and should not be
   mixed with tensor type-system or DSL lowering changes.
6. Domain hierarchy should be stored as registry data before it is mirrored as
   C++ namespace structure.
7. Use dot-separated domain IDs in metadata and diagnostics first; add C++
   namespace wrappers only after the registry contracts are stable.

The tensor symbol-table additions follow the same rule deliberately:
`TY_tensor_*` APIs operate on TensorDescriptorIR-completing attributes owned by
`TY_IDX`, and `ST_tensor_*` APIs operate on compiler metadata owned by
`ST_IDX`.  These
functions remain global `common/com` APIs so old Open64 components can call
them without adopting a new C++ namespace.  A future namespace facade may wrap
these functions, but the ABI-facing common-core entry points should remain
prefix-grouped in the existing Open64 style.

## Stage 0: Infrastructure Markers

Goal: Provide a low-risk way to preserve DSL intent in existing WHIRL files.

Coding changes:

1. Add DSL marker helpers over `OPR_COMMENT`.
2. Use a reserved marker prefix such as `__WHIRL_DSL__:`.
3. Provide helper APIs to create, detect, and extract DSL marker payloads.
4. Document marker format and lowering expectations.

Primary files:

```text
osprey/common/com/wn.h
osprey/common/com/wn.cxx
doc/WHIRL-DSL-INFRASTRUCTURE.md
```

Validation:

1. Existing WHIRL readers continue to see legal `OPR_COMMENT` nodes.
2. DSL-aware code can detect markers through the new helper API.
3. No WHIRL operator enum or binary format change is required.
4. New helper APIs follow existing global `WN_*` naming instead of introducing
   namespaces in common WHIRL headers.
5. `ir_a2b` continues to accept ASCII WHIRL with ordinary comments, and DSL
   markers are still represented as legal `OPR_COMMENT` nodes.

Exit criteria:

1. A frontend can emit a DSL marker.
2. A later pass can identify the marker without parsing arbitrary comments.
3. ASCII-to-binary conversion remains unchanged for non-DSL input.

## Stage 1: Python DSL Frontend Emits Very-High WHIRL

Goal: Make the Python DSL ingestion path create binary very-high-level WHIRL
with first-class domain/common operators rather than plain High WHIRL or early
intrinsic-call placeholders.

### Chapter 7 Ingestion Boundary Design Note

The Chapter 7 architecture in `DSC_Master_Design_Doc_v0.10.docx` defines Python
as a source-language frontend for DSL authoring, model capture, and import-time
normalization.  Python may use `torch.export`, FX, a dedicated DSC Python DSL,
or another graph capture mechanism, but it must produce a compiler artifact and
must not remain in the Open64 middle-end path.

The first Python package boundary is `open64_dsc`.  It should be thin: capture
the user model, classify graph nodes, preserve source context, and call the
native WHIRL builder.  It should expose a small API surface such as
`export_to_whirl`, `save_as_whirl`, `WhirlExportOptions`, and a
`WhirlExportInterpreter`.  The readable WHIRL dump is diagnostic output; the
binary very-high-level WHIRL artifact is the compiler input.

The C++ WHIRL builder boundary owns Open64 IR construction.  That boundary owns
module/function creation, WHIRL node creation, `TY` and `ST` table creation,
TensorDescriptorIR attachment, opcode attributes, contracts, compiler metadata,
and mapped binary image finalization.  Python must not implement the WHIRL
object model or directly own Open64 table layout.  The builder-facing API should
create first-class DSL operators through names such as `CreateOperator`; it
should not expose intrinsic creation as the frontend construction API.

The artifact boundary is a mapped WHIRL binary image finalized by the design
component named `WhirlMappedImageFinalizer`.  This name is intentional: the
builder is populating Open64-compatible mapped IR state and finalizing it, not
performing semantic format conversion or object-stream encoding.  Tensor descriptors,
opcode attributes, contracts, source metadata, weights/constants, and
version/capability records may be represented by side tables or extension
sections, but architecturally they are part of the compiler IR image.

The ingestion artifact must preserve first-class domain/common operators long
enough for gatekeeper verification, TensorDescriptorIR validation, opcode
attribute validation, target-independent operator optimization, common-substrate
cleanup, and parallelization planning.  Intrinsic ops, runtime calls, library
calls, and target kernels are lowering products introduced by later compiler
passes after semantic legality and optimization opportunities have been
processed.

`mpl2whirl` is not the active DSL ingestion path.  It remains parked until MPL
tensor syntax and its MIR type representation are explicitly defined.  The
first staged workflow is therefore two-step and inspectable:

```text
torch2whirl model.py -o model.B
opencc -x whirl model.B ...
```

The later combined driver mode may dispatch the Python frontend through
`opencc`, but only after the binary WHIRL artifact boundary is stable:

```text
opencc -frontend=torch2whirl model.py ...
```

### First ResNet Inference Vertical Slice

The first Python ingestion target should be a ResNet inference path.  This
vertical slice is deliberately CNN-heavy because it exercises tensor type
creation, symbol creation, constant/parameter lifting, domain gatekeeper checks,
residual lineage, common activation/projection operators, and mapped binary
artifact generation without requiring Transformer runtime state.

The initial operator list is:

| Operator | Domain role | Operand policy | Opcode attributes | TensorDescriptorIR / metadata ownership |
| --- | --- | --- | --- | --- |
| `cnn.conv2d` | CNN domain executable | `kid0` input tensor, `kid1` weight tensor, optional bias | `kernel_shape`, `stride`, `padding`, `dilation`, `groups`, `data_layout`, `weight_layout` | Tensor descriptors own dtype, rank, logical shape, layout, sharding, placement, quantization, and runtime state; compiler metadata owns source layer name and lowering hints. |
| `cnn.batch_norm_infer` | CNN domain executable | activation tensor, scale, bias, mean, variance | `epsilon`, `momentum_policy`, `axis`, `training=false` | Descriptor state records representation/layout; metadata records checkpoint/source parameter names. |
| `common.relu` | Common executable | activation tensor | optional `inplace_allowed=false` and activation policy | Descriptor state propagates dtype/shape/layout; metadata records source node/layer. |
| `cnn.max_pool2d` | CNN domain executable | activation tensor | `kernel_shape`, `stride`, `padding`, `dilation`, `ceil_mode` | Domain verifier owns pooling semantics before lowering to `common.window_reduce`. |
| `common.residual_add` | Common executable with residual contract | `kid0` main path tensor, `kid1` skip path tensor | `broadcast_rule=none`, optional `residual_policy` | Descriptor state records compatible dtype/shape/layout; TensorLineageMetadata preserves residual identity. |
| `cnn.global_avg_pool2d` | CNN domain executable | activation tensor | `axes`, `keepdims`, `data_layout` | Domain verifier owns image-axis interpretation before lowering to reduction/common substrate. |
| `common.flatten` | Common executable | tensor | `start_axis`, `end_axis` | Descriptor state records new logical shape and lineage from prior tensor. |
| `common.linear` | Common executable | input tensor, weight tensor, optional bias | `transpose_weight`, `bias_axis`, `layout_contract` | Descriptor state records projection shape/layout; metadata records source fully connected layer. |
| `common.output_logits` | Common declaration | logits tensor | `class_axis`, optional `activation=none` | Descriptor state records final output shape/dtype; metadata records model output name. |

The slice intentionally keeps CNN-specific operators as domain operators until
their domain gatekeeper has validated image layout, padding, stride, dilation,
groups, pooling policy, and batch-normalization inference semantics.  Lowering
may then produce common substrate operators such as `common.window_reduce`,
`common.matmul`, `common.gemm`, or canonical WHIRL/runtime calls, but those are
not the ingestion representation.

### CNN Structured Regions

ResNet `BasicBlock`, `Bottleneck`, and shortcut semantics shall be represented
with the existing WHIRL `OPR_REGION` structure rather than by treating a bare
`OPR_BLOCK` as a tensor-valued operator.  This follows the established WHIRL
use of regions for semantically governed statement scopes, including parallel
regions produced by automatic parallelization.

The initial CNN form uses `REGION_KIND_PRAGMA`.  Its exit block is empty, its
pragma block declares the CNN contract, and its body block contains the ordered
DSL computation.  The contract identifies at least the domain construct and
version, BasicBlock or Bottleneck kind, input value, result value, identity or
projection shortcut kind, and optional ResNet stage and block ordinal.  Source
module paths remain compiler metadata.  Static parameters such as convolution
stride, padding, dilation, and groups remain attributes of the corresponding
operators inside the region.

Construction should follow the automatic-parallelization precedent in
`be/lno/ara_loop.cxx`: form the body, call `WN_CreateRegion`, populate
`WN_region_pragmas`, propagate source positions, parentize the completed tree,
and insert the region into its parent block.  The first pragma is the stable
region classifier; later pragmas refine the contract.  Each region receives a
stable region ID.  LNO also creates and links an RID because it constructs the
region inside an active optimizer.  The Python layer must not inspect region
internals, but the native `common/com` builder creates the region's common RID
state and managed WN-to-RID relation.  Stable region rows use the reserved
`WT_REGIONS` PU subsection.  Mapped rows contain IDs, kinds, parent relations,
and PU-tree-relative WN offsets rather than process pointers.  Opening the image
restores the managed relation and creates the in-memory RID objects.

Reusable RID identity and hierarchy, region construction, pragma-list access,
declared symbol interfaces, structural verification, logical inspection, and
body-splicing mechanics belong in `common/com` as a common WHIRL region
substrate.  They do not belong in `common/util`, because they operate directly
on WN layout, WHIRL pragmas, source positions, symbol tables, and mapped IR
invariants.  Derived optimizer levels, alias and preg boundary analysis, region
bounds, and target lowering remain in `be/region`.  Migration from backend
helpers must be staged behind compatibility wrappers and differential LNO tests
so automatic parallelization behavior does not change.

`OPR_REGION` is statement-level and does not itself produce a tensor value.
Its interface instead declares repeatable symbol bindings with the provisional
roles `INPUT`, `OUTPUT`, `INOUT`, and `RESULT`.  `RESULT` is an output with the
additional meaning that it carries a logical result of the region; a region may
declare more than one.  An input is defined outside and referenced inside.  An
output is defined inside and may be referenced outside.  An `INOUT` symbol has
both roles.  Any value defined inside the region and used outside must appear
in the declared output interface.

The enclosing region or PU owns each output/result symbol and its logical
storage, analogous to a caller-owned structure-return location or C++ reference
output parameter.  The inner region stores its result into that designated
symbol.  For a DSL operator result, the outer owner creates a complete tensor
temporary and asserts `no_alias=true`; the inner region does not allocate an
independent result object.  Physical tensor-memory allocation remains a later
lowering decision.

The mapped region image therefore includes a fixed-row, repeatable region
symbol-interface table keyed by stable region ID and ordinal.  Each row carries
an `ST_IDX`, role bits, and compatibility flags; it carries no pointer or STL
ownership.  The existing RID `used_in`, `def_in_live_out`, and preg boundary
sets remain derived optimizer facts and can later verify or refine the declared
interface.  They are not substituted for the source-level declaration.

This interface is intentionally an abstraction rather than a final SSA model.
The DSL middle-end may later translate region inputs and outputs into SSA uses,
definitions, block arguments, or merge values.  Until that design is proven,
clients use the common region-interface API and must not depend on the physical
table layout.  A bare nested `OPR_BLOCK` is not a semantic boundary because
normal block insertion can splice its statements into the enclosing block.

The CNN gatekeeper must verify that the region body implements its declared
block and shortcut contract before the region is dissolved or its operators
are promoted to the common substrate.  Identity shortcuts must consume the
region input directly.  Projection shortcuts must contain and identify the
required projection computation.  `ir_b2a` must display the logical CNN region
and pragma contract without exposing private DSL record indices or escape-tag
details.  Region dissolution should follow the established MP-lowering pattern
of reconnecting the body statement list to the parent block, while preserving
definitions, uses, source positions, and the region result temporary.

All static operation parameters remain opcode attributes.  Examples include
`kernel_shape`, `stride`, `padding`, `dilation`, `groups`, `epsilon`,
`ceil_mode`, `start_axis`, `end_axis`, and `class_axis`.  Tensor semantic state
is represented through TensorDescriptorIR and its split components:
TensorTypeCore, TensorTraitSet, TensorRepresentationDescriptor, and
TensorLineageMetadata.  Compiler metadata is reserved for source context,
diagnostics, pass ownership, lowering hints, checkpoint/source parameter names,
and profiling information.

### Values, Tensor Access, Effects, And Ordering

Do not introduce a generic multiple-result facility as the default answer to
every operation that appears to produce more than one observable fact.  Each
case must first be classified by its WHIRL semantics:

1. An ordinary pure DSL expression produces one value with one canonical
   tensor `TY_IDX`.
2. A tensor plus one or more indices denotes tensor access.  Its abstraction
   should follow High WHIRL `OPR_ARRAY`: a base, dimension information, and
   ordered index expressions form an address expression, which is then used by
   the appropriate load or store.  Indices are operands, not secondary
   results.
3. A value defined inside a region and consumed outside is represented through
   the declared region symbol interface and caller-owned result temporary.
4. Runtime status, mutable buffers, random-generator state, communication
   state, and similar observations are memory/state effects.  They are not
   ordinary tensor results merely because a source framework returns them.
5. Ordering and visibility requirements use WHIRL barrier semantics.  A state
   effect and a barrier are related but are not interchangeable.

The VHO DSL middle end shall model abstract state using an HSSA-style
interface.  A possible read of an abstract state is a `MU`-like use.  A
possible modification is a `CHI`-like old-state operand and new-state result.
Virtual state objects group accesses that may interact, following WOPT virtual
symbols and alias analysis.  The VHO API must remain independent of WOPT's
physical `AUX_ID`, `CODEREP`, `MU_NODE`, and `CHI_NODE` classes; lowering into
those backend structures occurs when DSL HSSA/WOPT form is built.

This direction follows Chow, Chan, Liu, Lo, and Streich, *Effective
Representation of Aliases and Indirect Memory Operations in SSA Form*, CC
1996, pages 253-267, DOI `10.1007/3-540-61053-7_66`.  HSSA uses virtual
variables plus `MU` and `CHI` nodes to expose indirect-memory dependences while
controlling version growth.  Open64 WOPT is the concrete implementation
reference for the DSL design.

WHIRL already provides `OPR_FORWARD_BARRIER` and `OPR_BACKWARD_BARRIER` with a
variable list of affected references.  WOPT conservatively maps a barrier to
may-use and may-def information so prohibited memory motion cannot cross it.
DSL synchronization and collective ordering should reuse this abstraction
where its semantics fit.  A side-effecting operator can require `MU`/`CHI`
without requiring a barrier; synchronization can require both.

### Fused Regions And Dispatch

Operator fusion and graph capture are represented as a logical `FUSED`
contract on the common `OPR_REGION` substrate.  The region body contains the
ordered logical DSL expressions, its declared interface identifies externally
visible inputs and results, and its effect and barrier contracts constrain
legal transformation.  A CUDA Graph is one possible target realization of a
certified fused region; it is not the common VHO representation.

Library and kernel dispatch contracts are a separate major design topic.  They
must eventually cover semantic versions, supported operators and fused
patterns, tensor types and representations, workspace ownership, alias and
effect behavior, synchronization, asynchronous completion, error reporting,
target ABI, capability versioning, and deterministic implementation selection.
Do not encode these decisions as compiler metadata or settle them incidentally
while introducing regions, HSSA effects, or canonical tensor types.

Coding changes:

1. Treat Python capture through `torch.export`, FX, or a dedicated DSC Python DSL
   as the first planned DSL ingestion path.
2. Add a thin `open64_dsc` Python package boundary with `export_to_whirl`,
   `save_as_whirl`, `WhirlExportOptions`, and a `WhirlExportInterpreter`.
3. Add a C++ WHIRL builder extension boundary that owns module, function, type,
   symbol, tensor descriptor, operator, attribute, contract, metadata, and mapped
   image construction.
4. Emit first-class very-high-level operators such as `common.add`,
   `common.matmul`, `cnn.conv2d`, and `cnn.residual_add` at ingestion time.
5. Keep operator-to-intrinsic, runtime-call, library-call, or target-kernel
   translation as a later explicit lowering phase.
6. Finalize the artifact through a mapped binary WHIRL image boundary, named
   `WhirlMappedImageFinalizer` in the design, using the existing ELF section and
   mapped-image mechanisms.
7. Keep `mpl2whirl` parked until MPL tensor syntax and its MIR type
   representation are explicitly defined.

Primary files:

```text
open64_dsc/                         # future Python package boundary
libopen64_whirl_builder.so          # future C++ builder library boundary
osprey/common/com/dsl_opcode.h
osprey/common/com/dsl_opcode.cxx
osprey/common/com/symtab.h
osprey/common/com/symtab.cxx
doc/Open64_Domain_Specific_Compiler_IR_Design.md
```

Validation:

1. The frontend boundary can produce a binary very-high-level WHIRL artifact
   without requiring Python in the Open64 middle-end path.
2. Imported operators remain first-class domain/common operators through the
   gatekeeper and target-independent optimization window.
3. Tensor descriptors, opcode attributes, contracts, and compiler metadata are
   preserved in the mapped binary image boundary.
4. Intrinsic ops and runtime calls appear only after an explicit lowering pass.
5. The initial driver workflow is two-step and inspectable:
   `torch2whirl model.py -o model.B`, then `opencc -x whirl model.B ...`.

Exit criteria:

1. A Python DSL or exported graph can be mapped to first-class very-high-level
   WHIRL operators with tensor descriptors and source metadata.
2. Open64 can consume the binary WHIRL artifact without the original Python
   interpreter.
3. Existing non-DSL frontends remain unchanged.
4. `mpl2whirl` is not modified speculatively for this stage.

## Stage 2: DSL Type Extension Registry

Goal: Allow DSL-specific types, tensor attributes, and compiler metadata to
exist before WHIRL lowering is chosen.

Coding changes:

1. Add a side-table keyed by `TY_IDX` for DSL type extensions and tensor
   attributes.
2. Introduce tensor as the first extension kind.
3. Support delayed binding for `TY_IDX` tensor attributes and `ST_IDX` compiler
   metadata.
4. Keep the current carrier type compatible with existing WHIRL type records.
5. Keep common type-extension APIs in global Open64 style; avoid namespace-only
   interfaces in `common/com`.

Primary files:

```text
osprey/common/com/symtab.h
osprey/common/com/symtab.cxx
doc/WHIRL-DSL-INFRASTRUCTURE.md
```

Validation:

1. A frontend can create or mark a tensor extension type.
2. Attribute keys can be declared before values are known.
3. Tensor attributes and compiler metadata can be bound later by type inference,
   DSL analysis, placement analysis, alias analysis, or pass planning.
4. Type extension state resets with symbol-table side tables.
5. `ir_a2b` still sees the carrier as an existing legal `TY_KIND`, so ASCII and
   binary conversion do not require tensor-specific binary IR image changes yet.

Exit criteria:

1. Tensor semantic data can be represented without choosing aggregate WHIRL,
   descriptors, runtime handles, or target fragments.
2. Existing type records remain compatible with `ir_a2b` and `ir_b2a`.

## Stage 3: Domain Registry And Semantic Namespace IDs

Goal: Implement the Word design's namespace hierarchy as data first, avoiding a
premature C++ namespace migration.

Coding changes:

1. Add a domain descriptor structure with fields for domain ID, parent domain,
   version, owned operations, owned types, verifier hooks, pass hooks, and
   diagnostic prefix.
2. Represent namespace identity as strings such as
   `ant.domain.transformer.inference`.
3. Add registry APIs using existing Open64 global naming conventions, for
   example `DSL_Register_Domain`, `DSL_Find_Domain`, and
   `DSL_Domain_Inherits`.
4. Add sub-domain inheritance metadata without using C++ inheritance.
5. Allow DSL markers and tensor attributes to carry a `domain` key that references
   a registered domain ID.
6. Add diagnostics for unknown domain IDs and invalid parent relationships.

Primary files:

```text
osprey/common/com/dsl_domain.h
osprey/common/com/dsl_domain.cxx
osprey/common/com/wn.h
osprey/common/com/symtab.h
doc/Open64_Domain_Specific_Compiler_IR_Design.md
doc/WHIRL-DSL-INFRASTRUCTURE.md
```

Validation:

1. A domain such as `ant.domain.transformer` can be registered.
2. A sub-domain such as `ant.domain.transformer.inference` can declare its
   parent and inherited contracts.
3. Domain lookup works without requiring any C++ namespace.
4. Bad parent references produce deterministic diagnostics.
5. Domain IDs carried in markers or metadata do not change `ir_a2b` behavior
   unless the stage explicitly adds matching reader/writer support.

Exit criteria:

1. Namespace hierarchy from the Word design is represented as compiler data.
2. Common Open64 code still exposes global C-style APIs.
3. `ir_a2b` remains independent of namespaced DSL implementation classes.

## Stage 4: Contract Registry And Gatekeeper Metadata

Goal: Add the contract-carrying part of the Word design without yet building a
full gatekeeper.

Coding changes:

1. Add a contract descriptor with source domain, target domain, required checks,
   diagnostic codes, and version.
2. Add global APIs such as `DSL_Register_Contract` and `DSL_Find_Contract`.
3. Allow DSL markers or tensor attributes to reference contract IDs.
4. Add a verifier stub that reports which contracts are present, missing, or
   unresolved.
5. Keep verifier hooks as function pointers or global wrapper callbacks first;
   do not require namespaced C++ classes.

Primary files:

```text
osprey/common/com/dsl_domain.h
osprey/common/com/dsl_domain.cxx
osprey/be/vho/*
doc/Open64_Domain_Specific_Compiler_IR_Design.md
```

Validation:

1. A contract can be registered between two domain IDs.
2. The compiler can list required contracts for a domain boundary.
3. Missing contracts are reported before lowering.

Exit criteria:

1. The gatekeeper architecture has a concrete metadata representation.
2. Namespaced implementation remains optional and isolated.

## Stage 5: Common Compiler Substrate Opcode Architecture

Goal: Introduce the Chapter 6 opcode ownership model as compiler data before
lowering domain operations into canonical WHIRL or target-specific forms.

The opcode substrate should classify IR constructs by responsibility, not only
by runtime behavior:

```text
Executable op:
  Performs dataflow or numerical computation.
  Examples: common.matmul, common.add, cnn.conv2d, transformer.attention

Declaration op:
  Declares model boundary, structure, or tensor role.
  Examples: common.model_input, cnn.input_image, transformer.input_tokens

Contract op:
  Declares ABI, semantic, layout, state, or policy obligations.
  Examples: cnn.preprocess_contract, transformer.tokenizer_contract

Verifier op:
  Represents a check that may produce diagnostics or generated tests.
  Examples: common.shape_assert, cnn.residual_shape_check,
            transformer.logprob_replay_check

Lowering-policy op:
  Guides compiler control or target selection without changing model semantics.
  Examples: common.fusion_contract, cnn.conv_algorithm_choice,
            common.shape_bucket
```

Common opcodes should be organized into levels.  A level is a registry
classification for how foundational or semantically specialized an operator is;
it is not a WHIRL opcode number, execution order, pass order, or lowering
priority.  Lower levels are more universal substrate concepts that many
domains can share directly.  Higher levels carry more model, compiler-policy,
runtime, or distributed-execution intent and are more likely to be wrapped by
domain-specific operators before lowering.

The level characterization is:

```text
Level 0: Core IR structure
  Program structure and side-effect framing.

Level 1: Tensor, shape, and layout
  Tensor identity, shape facts, symbolic shape formulas, layout transforms,
  and view/materialization requirements.

Level 2: Numeric tensor ops
  Domain-neutral tensor computation such as elementwise math, contraction,
  activation, and reduction.

Level 3: Neural-network common ops
  Common model semantics such as boundaries, residuals, normalization,
  quantization, and fusion contracts.

Level 4: Parallel and runtime common ops
  Distribution, collective intent, runtime state, dispatch, guards, and
  kernel variant selection.
```

The initial level assignment is:

```text
Level 0: Core IR structure
  common.module, common.function, common.region, common.call,
  common.return, common.constant, common.effect

Level 1: Tensor, shape, and layout
  common.tensor, common.shape_of, common.shape_formula,
  common.shape_assert, common.runtime_shape_guard, common.reshape,
  common.flatten, common.transpose, common.slice, common.concat,
  common.pad, common.layout_cast, common.contiguous

Level 2: Numeric tensor ops
  common.add, common.mul, common.bias_add, common.matmul, common.gemm,
  common.linear, common.activation, common.relu, common.gelu, common.silu,
  common.softmax, common.reduce, common.reduce_sum, common.reduce_mean,
  common.reduce_max, common.window_reduce

Level 3: Neural-network common ops
  common.model_input, common.model_output, common.output_logits,
  common.residual_add, common.residual_shape_check,
  common.normalization_base, common.fusion_group, common.fusion_contract,
  common.quantize, common.dequantize, common.requantize,
  common.quantization_contract

Level 4: Parallel and runtime common ops
  common.shard, common.reshard, common.dispatch, common.gather,
  common.scatter, common.reduce_scatter_intent, common.all_reduce_intent,
  common.all_gather_intent, common.all_to_all_intent,
  common.runtime_state_handle, common.runtime_guard, common.shape_bucket,
  common.kernel_variant
```

The first implementation should seed a deliberately small `common.v0`
operator set.  This set is the contract to implement before adding broad
domain coverage.  It should be large enough to express shared CNN and
Transformer substrate behavior, but small enough that every operator has an
explicit shape rule, effect model, and lowering responsibility.

### Initial Common Substrate Operator Set

| Opcode | Category | Level | Required operands / attributes | Purpose |
| --- | --- | --- | --- | --- |
| `common.tensor` | declaration | 1 | `TY_IDX`, element type, rank, shape attributes | Names a semantic tensor value or tensor-bearing symbol. |
| `common.shape_of` | declaration | 1 | tensor operand | Produces the symbolic or concrete shape descriptor for a tensor. |
| `common.shape_formula` | declaration | 1 | formula payload, input shape refs | Records a derived shape before all dimensions are concrete. |
| `common.shape_assert` | verifier | 1 | condition payload, diagnostic message | Requires a shape fact before lowering or code generation. |
| `common.runtime_shape_guard` | verifier | 1 | runtime condition, fallback policy | Emits or records a dynamic guard for partially symbolic shape. |
| `common.reshape` | executable | 1 | tensor, target shape | Changes tensor view/descriptor without changing element values. |
| `common.transpose` | executable | 1 | tensor, permutation | Reorders dimensions. |
| `common.slice` | executable | 1 | tensor, offsets, sizes, strides | Extracts a tensor region. |
| `common.concat` | executable | 1 | tensor list, axis | Concatenates compatible tensors. |
| `common.pad` | executable | 1 | tensor, per-axis padding, pad value | Pads tensor boundaries. |
| `common.layout_cast` | executable | 1 | tensor, source layout, target layout | Converts between layouts such as NCHW, NHWC, packed, or blocked. |
| `common.contiguous` | executable | 1 | tensor, layout | Materializes or asserts contiguous storage for a layout. |
| `common.add` | executable | 2 | `kid0` tensor/scalar, `kid1` tensor/scalar, broadcast rule | Elementwise addition. |
| `common.mul` | executable | 2 | `kid0` tensor/scalar, `kid1` tensor/scalar, broadcast rule | Elementwise multiplication. |
| `common.bias_add` | executable | 2 | tensor, bias tensor, axis/layout | Adds bias with explicit broadcast axis. |
| `common.matmul` | executable | 2 | `kid0`, `kid1`, transpose flags, batch dims | Matrix multiply or batched matrix multiply. |
| `common.gemm` | executable | 2 | `kid0`, `kid1`, optional bias, alpha/beta | Lowering-oriented generalized matrix multiply. |
| `common.linear` | executable | 2 | input, weight, optional bias, layout contract | Domain-neutral affine projection. |
| `common.relu` | executable | 2 | tensor | ReLU activation. |
| `common.gelu` | executable | 2 | tensor, approximation mode | GELU activation. |
| `common.silu` | executable | 2 | tensor | SiLU activation. |
| `common.softmax` | executable | 2 | tensor, axis, stability policy | Softmax over an explicit axis. |
| `common.reduce_sum` | executable | 2 | tensor, axes, keepdims | Sum reduction. |
| `common.reduce_mean` | executable | 2 | tensor, axes, keepdims | Mean reduction. |
| `common.reduce_max` | executable | 2 | tensor, axes, keepdims | Max reduction. |
| `common.window_reduce` | executable | 2 | tensor, window, stride, padding, reduce kind | Shared base for pooling-style reductions after domain verification. |
| `common.model_input` | declaration | 3 | symbol, tensor type, boundary name | Declares model input boundary. |
| `common.model_output` | declaration | 3 | symbol/value, tensor type, boundary name | Declares model output boundary. |
| `common.output_logits` | declaration | 3 | tensor, class/token axis | Marks logits without binding to CNN or Transformer vocabulary. |
| `common.residual_add` | executable | 3 | `kid0`, `kid1`, residual lineage metadata | Adds residual path while preserving residual identity for diagnostics. |
| `common.residual_shape_check` | verifier | 3 | `kid0` shape, `kid1` shape, policy | Verifies residual operands are compatible before lowering. |
| `common.normalization_base` | executable | 3 | tensor, axes, epsilon, scale/bias flags | Shared base for layer/rms/batch-style normalization after domain checks. |
| `common.quantize` | executable | 3 | tensor, scale, zero point, target dtype | Converts from high precision to quantized representation. |
| `common.dequantize` | executable | 3 | tensor, scale, zero point, source dtype | Converts from quantized representation to high precision. |
| `common.requantize` | executable | 3 | tensor, source scale, target scale/dtype | Converts between quantized representations. |
| `common.fusion_group` | lowering-policy | 3 | region/op list, legality payload | Groups ops as a fusion candidate without changing semantics. |
| `common.fusion_contract` | lowering-policy | 3 | fusion group, legality constraints | Records required fusion legality checks. |
| `common.shard` | lowering-policy | 4 | tensor, partition spec, mesh/device spec | Describes partitioning intent. |
| `common.reshard` | lowering-policy | 4 | tensor, source shard, target shard | Describes redistribution intent. |
| `common.dispatch` | lowering-policy | 4 | op/region, target/runtime key, variant constraints | Selects runtime or target lowering path. |
| `common.gather` | executable | 4 | tensor, indices, axis | Gathers indexed slices. |
| `common.scatter` | executable | 4 | tensor, indices, updates, axis, combine mode | Scatters updates into a tensor. |
| `common.all_reduce_intent` | lowering-policy | 4 | tensor, reduction kind, group | Records collective all-reduce intent. |
| `common.all_gather_intent` | lowering-policy | 4 | tensor, axis/group | Records collective all-gather intent. |
| `common.reduce_scatter_intent` | lowering-policy | 4 | tensor, reduction kind, shard spec | Records collective reduce-scatter intent. |
| `common.runtime_state_handle` | declaration | 4 | state kind, ABI key | Represents opaque runtime state without exposing domain ABI. |
| `common.runtime_guard` | verifier | 4 | condition payload, action | Records a runtime legality or availability guard. |
| `common.shape_bucket` | lowering-policy | 4 | shape expression, bucket policy | Groups dynamic shapes for dispatch or specialization. |
| `common.kernel_variant` | lowering-policy | 4 | target key, variant ID, constraints | Names a selected or candidate implementation variant. |

The initial set intentionally excludes semantically rich domain operators:

| Domain opcode | Common substrate relationship |
| --- | --- |
| `cnn.conv2d` | Domain verifier owns padding, stride, dilation, groups, and image layout; lowering may use `common.window_reduce`, `common.matmul`, or `common.gemm` style substrate ops only after verification. |
| `cnn.pool2d` | Domain verifier owns pooling policy; lowering may use `common.window_reduce`. |
| `transformer.attention` | Domain verifier owns mask, head layout, positional semantics, and KV-cache ABI; lowering may use `common.matmul`, `common.softmax`, and `common.dispatch`. |
| `transformer.layer_norm` / `transformer.rms_norm` | Domain verifier owns exact axis/epsilon/checkpoint contract; lowering may use `common.normalization_base`. |
| `cnn.preprocess_contract` / `transformer.tokenizer_contract` | Contract content remains domain-specific; only the contract mechanism is common. |

Every `common.v0` operator descriptor must include:

1. Stable string ID, for example `common.add`.
2. Category: `executable`, `declaration`, `contract`, `verifier`, or
   `lowering_policy`.
3. Level: 0 through 4.
4. Version: start with `1`.
5. Operand count policy: fixed, variadic, or payload-defined.
6. Required tensor attribute keys.
7. Shape rule kind: identity, broadcast, contraction, reduction, view,
   layout, runtime-guarded, or opaque.
8. Effect model: pure, verifier-only, declaration-only, lowering-policy, or
   runtime-effect.
9. Lowering obligation: canonical WHIRL, runtime call, intrinsic sequence,
   target-specific object, or marker-only until a later pass.
10. Diagnostic prefix for missing traits, unsupported shape, or illegal
    domain promotion.

### DSL Opcode Compatibility Guarantee

The common substrate registry must preserve backward compatibility for every
public DSL opcode enum, string operator name, category, level, trait enum, shape
rule enum, effect enum, and promotion-state enum.  DSL opcodes are compiler IR
contracts, not local implementation details.  Once an opcode name or enum value
has appeared in a released IR file, test, dump, or domain registry, it must not
be reused for a different meaning.

Compatibility rules:

1. Treat the stable string ID, such as `common.add`, as the primary external
   identity.  Numeric enum values and table handles are implementation details
   unless explicitly included in binary WHIRL image contracts.
2. Never rename or delete a public operator name in place.  If a name changes,
   keep the old name as an alias that resolves to the new descriptor and emits
   an optional deprecation diagnostic.
3. Never reorder public enum values after release.  Add new enum values only at
   the end, and reserve tombstones for removed experimental values.
4. Every opcode descriptor has an explicit version.  Incompatible semantic
   changes require a new version, for example `common.add` version `2`, not a
   silent mutation of version `1`.
5. Keep old descriptor versions loadable.  A reader may lower an old version
   through a compatibility path, reject it with a deterministic diagnostic, or
   map it through a versioned migration rule; it must not reinterpret it
   silently.
6. Persist aliases and version migrations in the opcode registry, not in ad hoc
   frontend code.
7. Dumps should print the stable string ID and version, not just the numeric
   table handle.
8. Tests must cover lookup by current name, lookup by deprecated alias, unknown
   name diagnostics, old-version handling, and enum append-only behavior.
9. WHIRL `OPERATOR` and `OPCODE` enums remain unchanged in this stage.  DSL
   opcode compatibility is maintained in `dsl_opcode.*` side tables and marker
   metadata, preserving `ir_a2b` and `ir_b2a` behavior.

### Example: Creating `common.add`

The DSL opcode API should live in `osprey/common/com/dsl_opcode_core.h`,
`osprey/common/com/dsl_opcode.h`, and `osprey/common/com/dsl_opcode.cxx`.
This mirrors the existing `opcode_core.h`, `opcode.h`, and `opcode.cxx` split
without changing WHIRL `OPERATOR` or `OPCODE`.

```c++
/* dsl_opcode_core.h */
typedef mUINT32 DSL_OPCODE;

enum DSL_OPCODE_CATEGORY {
  DSL_OPC_EXECUTABLE = 0,
  DSL_OPC_DECLARATION = 1,
  DSL_OPC_CONTRACT = 2,
  DSL_OPC_VERIFIER = 3,
  DSL_OPC_LOWERING_POLICY = 4
};

enum DSL_OPCODE_LEVEL {
  DSL_OPC_LEVEL_0_CORE = 0,
  DSL_OPC_LEVEL_1_TENSOR = 1,
  DSL_OPC_LEVEL_2_NUMERIC = 2,
  DSL_OPC_LEVEL_3_NN_COMMON = 3,
  DSL_OPC_LEVEL_4_RUNTIME = 4
};

enum DSL_SHAPE_RULE {
  DSL_SHAPE_RULE_OPAQUE = 0,
  DSL_SHAPE_RULE_BROADCAST = 1,
  DSL_SHAPE_RULE_CONTRACTION = 2,
  DSL_SHAPE_RULE_REDUCTION = 3,
  DSL_SHAPE_RULE_VIEW = 4
};

enum DSL_EFFECT_MODEL {
  DSL_EFFECT_PURE = 0,
  DSL_EFFECT_VERIFIER_ONLY = 1,
  DSL_EFFECT_DECLARATION_ONLY = 2,
  DSL_EFFECT_LOWERING_POLICY = 3,
  DSL_EFFECT_RUNTIME = 4
};
```

```c++
/* dsl_opcode.h */
DSL_OPCODE DSL_Register_Opcode(const char *domain,
                               const char *name,
                               UINT16 version,
                               DSL_OPCODE_CATEGORY category,
                               DSL_OPCODE_LEVEL level,
                               mINT16 nkids,
                               DSL_SHAPE_RULE shape_rule,
                               DSL_EFFECT_MODEL effect_model);

DSL_OPCODE DSL_OPCODE_make_op(const char *domain,
                              const char *name,
                              UINT16 version);

BOOL DSL_OPCODE_is_valid(DSL_OPCODE opc);
const char *DSL_OPCODE_domain(DSL_OPCODE opc);
const char *DSL_OPCODE_name(DSL_OPCODE opc);
UINT16 DSL_OPCODE_version(DSL_OPCODE opc);
mINT16 DSL_OPCODE_nkids(DSL_OPCODE opc);
const char *DSL_OPCODE_kid_name(DSL_OPCODE opc, INT kid);
```

```c++
/* common.v0 registry seed */
DSL_OPCODE common_add =
  DSL_Register_Opcode("common",
                      "add",
                      1,
                      DSL_OPC_EXECUTABLE,
                      DSL_OPC_LEVEL_2_NUMERIC,
                      2,
                      DSL_SHAPE_RULE_BROADCAST,
                      DSL_EFFECT_PURE);

/* Later lookup follows OPCODE_make_op style: assert if invalid. */
DSL_OPCODE opc = DSL_OPCODE_make_op("common", "add", 1);

Is_True(DSL_OPCODE_nkids(opc) == 2,
        ("common.add must have kid0 and kid1"));
Is_True(strcmp(DSL_OPCODE_kid_name(opc, 0), "kid0") == 0,
        ("common.add kid0 name mismatch"));
Is_True(strcmp(DSL_OPCODE_kid_name(opc, 1), "kid1") == 0,
        ("common.add kid1 name mismatch"));
```

For `temp = add(a, b)`, the common substrate opcode descriptor owns the
operation identity and static operation parameters:

```text
opcode: common.add version 1
category: executable
level: 2
kids:
  kid0: a
  kid1: b
output:
  temp
attributes:
  broadcast_rule: broadcast
  dtype_policy: same_or_declared_cast
shape_rule:
  broadcast
effect_model:
  pure
```

Tensor value state remains outside opcode attributes:

```c++
TensorDescriptorIR *a_desc = compiler.getTensorDescriptor(a);
TensorDescriptorIR *b_desc = compiler.getTensorDescriptor(b);
TensorDescriptorIR *temp_desc = compiler.getTensorDescriptor(temp);

TensorTypeCore *temp_core = compiler.getTensorTypeCore(temp);
TensorRepresentationDescriptor *temp_repr =
  compiler.getTensorRepresentation(temp);
TensorLineageMetadata *temp_lineage =
  compiler.getTensorLineage(temp);
```

Static operation parameters such as `broadcast_rule` belong to
`op.getAttribute(name)`.  Source context, diagnostic ownership, lowering hints,
pass history, and profile data belong to `op.getMetadata(name)`.

### Printing `common.add` Through `ir_b2a`

The first printing goal is to make `ir_b2a` expose common-substrate opcode
identity without requiring a WHIRL `OPERATOR` or binary IR image change.  The
underlying WHIRL tree must remain legal and printable by the existing
`ir_reader.cxx` path.  The DSL opcode printer should therefore augment the
existing print stream with stable DSL annotation text rather than replacing
`OPCODE_name(WN_opcode(wn))`.

Current `ir_b2a` flow:

```text
osprey/ir_tools/ir_a2b.cxx
  ir_b2a(...)
    -> fdump_tree(...)
       -> ir_put_stmt / ir_put_expr
          -> ir_put_wn(...)
             -> prints OPCODE_name(WN_opcode(wn))
```

For `temp = common.add(a, b)`, the compatible WHIRL fallback may still print as
ordinary WHIRL:

```text
LDID <a>
LDID <b>
ADD
STID <temp>
```

The augmented print should add a stable DSL line or inline comment that records
the common-substrate descriptor:

```text
COMMENT "__WHIRL_DSL__:opcode:common.add:v1:kid0=a;kid1=b;attr.broadcast_rule=broadcast"
LDID <a>
LDID <b>
ADD
STID <temp>
```

or, for debug dumps that are not intended for `ir_a2b` round-trip:

```text
ADD  # dsl_opcode=common.add.v1 category=executable level=2 kids=(kid0,kid1)
```

Round-trip-safe output must prefer the `OPR_COMMENT` form because existing
ASCII readers already understand comments.  Inline debug decoration may be
enabled only under an explicit dump/debug option and must not become the
default `ir_b2a` interchange format.

Implementation plan:

1. Keep `ir_put_wn` printing the original WHIRL opcode name exactly as it does
   today for all non-DSL nodes.
2. Add a helper in `dsl_opcode.h`, for example:

```c++
BOOL DSL_WN_Has_Opcode (const WN *wn);
DSL_OPCODE DSL_WN_opcode(const WN *wn);
void DSL_fprint_opcode_annotation (FILE *f, const WN *wn);
```

3. Store the DSL opcode association outside the WHIRL opcode field, using an
   `OPR_COMMENT` marker, side table, or `WN_MAP` during staging.  Do not add
   fields to `WN` for this stage.
4. In `ir_reader.cxx`, add one guarded call near `ir_put_wn` output:

```c++
if (DSL_WN_Has_Opcode (wn))
  DSL_fprint_opcode_annotation (ir_ofile, wn);
```

   The default round-trip mode should emit the annotation as a legal
   `OPR_COMMENT` record before the marked statement or expression group.
5. For `common.add`, the annotation must print stable string identity and
   version, not the numeric `DSL_OPCODE` handle:

```text
__WHIRL_DSL__:opcode:common.add:v1:kid0;kid1;broadcast_rule=broadcast
```

6. `ir_a2b` must be able to read the printed file without knowing the DSL
   opcode registry.  If the registry is available, a later validation pass can
   resolve the comment back to `DSL_OPCODE_make_op("common", "add", 1)`.
7. Add smoke tests:
   - non-DSL WHIRL prints byte-for-byte the same as before,
   - a `common.add` marker survives `ir_b2a -> ir_a2b -> ir_b2a`,
   - unknown DSL opcode names remain printable as comments and produce
     deterministic diagnostics only when a DSL validation pass runs,
   - debug decoration mode never becomes the default interchange format.

Coding changes:

1. Add an opcode descriptor structure with fields for opcode name, category,
   common level, owning domain ID, version, trait requirements, effect model,
   shape model, verifier hook, lowering hook, and diagnostic prefix.
2. Add registry APIs using existing Open64 global naming style, for example
   `DSL_Register_Opcode`, `DSL_Find_Opcode`, `DSL_Opcode_Category`, and
   `DSL_Opcode_Owner`.
3. Seed the registry with the initial `common.*` substrate levels listed above.
4. Add domain wrapper support so a domain opcode can reference a common opcode
   while retaining domain metadata.
5. Represent opcode references in DSL markers and tensor attributes using string
   IDs first, without changing WHIRL operator enums.
6. Add a dump/debug path that prints opcode category, owner, common level, and
   wrapper target.
7. Keep all common-core APIs global; optional C++ namespace facades may be
   added later over the registry.

Primary files:

```text
osprey/common/com/dsl_opcode.h
osprey/common/com/dsl_opcode.cxx
osprey/common/com/dsl_domain.h
osprey/common/com/wn.h
osprey/common/com/symtab.h
doc/Open64_Domain_Specific_Compiler_IR_Design.md
doc/WHIRL-DSL-INFRASTRUCTURE.md
```

Validation:

1. `common.linear`, `common.residual_add`, `common.layout_cast`, and
   `common.dispatch` can be registered and queried.
2. Domain wrappers such as `cnn.linear` and `transformer.q_projection` can
   point to `common.linear` without losing domain-specific metadata.
3. Opcode categories distinguish executable ops from declaration, contract,
   verifier, and lowering-policy ops.
4. Unknown opcode IDs produce deterministic diagnostics.
5. `ir_a2b` still sees legal WHIRL because opcode IDs are carried through
   markers or side metadata at this stage.

Exit criteria:

1. The compiler has a concrete common opcode registry independent of C++
   namespaces.
2. Domain wrappers can preserve ABI and gatekeeper metadata while sharing common
   op semantics.
3. No WHIRL operator enum or binary IR image change is required for this stage.

## Stage 6: Opcode Promotion Registry And Policy

Goal: Implement Chapter 6 promotion policy so domain authors have an explicit
path for deciding when an operation remains domain-specific, lowers partially,
or becomes a common substrate opcode.

Promotion criteria:

1. Cross-domain evidence exists, or the construct is demonstrably
   domain-neutral.
2. Observable behavior can be specified without domain vocabulary.
3. Required tensor traits are not tied to one domain ABI.
4. Shape rules are expressible through common tensor shape descriptors and
   shape constraints.
5. Side effects are absent or expressible through common effect metadata.
6. Lowering can be reused across multiple backends or target families.
7. Domain-specific contracts can wrap the common op instead of redefining it.
8. Promotion does not hide any compatibility check required by a domain
   gatekeeper.

Non-promotion criteria:

1. The op carries domain-specific ABI meaning, such as
   `cnn.preprocess_contract` or `transformer.tokenizer_contract`.
2. Correctness depends on domain-specific runtime state, such as
   `transformer.kv_cache_append` or `transformer.linear_state_update`.
3. Shape formulas depend on domain interpretation, such as CNN
   padding/stride/dilation or Transformer mask and position semantics.
4. The op requires domain-specific conformance tests.
5. Promotion would hide a required gatekeeper check.
6. Implementation similarity does not imply semantic equivalence, such as CNN
   group convolution versus Transformer MoE grouped execution.

Partial promotion should be the default for semantically rich domain ops:

```text
cnn.conv2d
  verify: padding, stride, dilation, image layout, groups
  lower: common.windowed_contraction -> common.matmul_like/common.gemm

transformer.attention
  verify: mask, position encoding, head layout, runtime-state ABI
  lower: common.matmul -> common.softmax -> common.matmul

cnn.pool2d
  verify: window, padding, ceil/floor behavior
  lower: common.window_reduce

transformer.rms_norm / transformer.layer_norm
  verify: norm axis, epsilon, checkpoint mapping
  lower: common.normalization_base
```

Coding changes:

1. Add a promotion descriptor with fields for source domain op, promoted common
   op, required common semantics, retained domain wrappers, required verifier
   checks, version, and diagnostics.
2. Add registry APIs such as `DSL_Register_Opcode_Promotion`,
   `DSL_Find_Opcode_Promotion`, and `DSL_Check_Opcode_Promotion`.
3. Add explicit promotion states:
   `domain_only`, `wrapper_to_common`, `partial_promotion`, and
   `common_native`.
4. Add compatibility versioning so changes to `common.*` semantics do not
   silently invalidate domain wrappers.
5. Add stable promotion diagnostics:

```text
CPROM-001 PromotionCandidateMissingCrossDomainEvidence
CPROM-002 DomainSpecificSemanticLeak
CPROM-003 PromotedOpMissingShapeFormula
CPROM-004 PromotedOpMissingEffectModel
CPROM-005 DomainWrapperRequired
CPROM-006 PromotionWouldHideGatekeeperCheck
CPROM-007 LoweringConflictAcrossDomains
CPROM-008 TraitMismatchAcrossPromotionSources
CPROM-009 CommonOpVersionMismatch
CPROM-010 DomainWrapperMissingAfterPromotion
```

6. Seed examples from CNN and Transformer:

```text
cnn.linear + transformer.q_projection:
  promote common.linear
  keep wrappers for classifier-head, checkpoint, and head-layout contracts

cnn.residual_add + transformer.residual_add:
  promote common.residual_add
  preserve residual lineage and shape checks

cnn.layout_cast + transformer.layout_cast:
  promote common.layout_cast
  preserve domain layout contracts

cnn.conv2d + transformer.attention:
  do not fully promote
  lower partially after domain verification

cnn.preprocess_contract + transformer.tokenizer_contract:
  promote only the common contract mechanism
  keep contract content domain-specific
```

Primary files:

```text
osprey/common/com/dsl_opcode.h
osprey/common/com/dsl_opcode.cxx
osprey/common/com/dsl_domain.h
osprey/common/com/dsl_domain.cxx
osprey/be/vho/*
doc/Open64_Domain_Specific_Compiler_IR_Design.md
```

Validation:

1. Promotion registry accepts common promotion examples such as
   `cnn.residual_add` and `transformer.residual_add` to
   `common.residual_add`.
2. Promotion rejects `transformer.tokenizer_contract` as common contract
   content while allowing reuse of the common contract mechanism.
3. Partial promotion records required verifier checks before lowering.
4. Version mismatch and missing wrapper cases produce CPROM diagnostics.
5. Domain wrappers remain visible to the DSL lowering pass until gatekeeper
   checks are complete.

Exit criteria:

1. The compiler can explain why an opcode is common, domain-only, wrapped, or
   partially promoted.
2. Promotion decisions are explicit, versioned, and testable.
3. CNN and Transformer can share common opcodes without losing gatekeeper
   semantics.

## Stage 7: Tensor Type System Stabilization

Goal: Settle the common tensor type contract before changing WHIRL binary IR
image type records.

Coding changes:

1. Define the required tensor fields:
   `element_ty`, `rank`, `shape`, `layout`, `strides`, `memory_space`,
   `quantization`, and `sparsity`.
2. Define which fields can remain symbolic or pending.
3. Define type equivalence rules for tensor types.
4. Define tensor attributes such as `layout`, `sharding`, `placement`,
   `memory`, `quantization`, runtime state, semantic traits, and lineage; these
   complete TensorDescriptorIR semantics.
5. Define compiler metadata such as source context, diagnostics, pass owner,
   lowering hints, and profile data; these attach to `ST_IDX` or use sites and
   do not create new tensor types.
6. Define diagnostics for unresolved required tensor attributes.

Primary files:

```text
doc/Open64_Domain_Specific_Compiler_IR_Design.md
doc/WHIRL-DSL-INFRASTRUCTURE.md
osprey/common/com/symtab.h
osprey/common/com/symtab.cxx
```

Validation:

1. Tensor attributes have documented required and optional fields.
2. Delayed fields have explicit binding points.
3. Type equivalence rules are clear enough to implement and exclude ordinary
   compiler metadata.

Exit criteria:

1. It is clear whether two tensor types are equivalent, compatible, or require
   conversion.
2. Lowering passes can trust the tensor attribute contract.

### Tensor Descriptor Binary Image Compatibility Note

The tensor compatibility contract is driven first by fields that participate in
computation, verifier legality, lowering, memory interpretation, or ABI.  A
free-form key-value table is useful for extensibility, but it is not a suitable
primary representation for hot compatibility checks such as deciding whether
two tensor operands can feed `common.add`, `common.matmul`,
`common.residual_add`, or a domain wrapper.

Architecturally, TensorDescriptorIR remains a layered model:

```text
TensorTypeCore
  kind, element type, rank, logical shape

TensorRepresentationDescriptor
  layout, strides/contiguity, memory space, placement, sharding,
  quantization, sparsity, runtime-state dependency

TensorTraitSet
  semantic role, domain traits, mutability/aliasing traits, lineage
```

### DSL result ownership and no-alias contract

Every value-producing DSL operator assigns its result to a tensor-typed
temporary symbol.  That result symbol has unique memory ownership and the
semantic attribute `no_alias=true` before the computation is verified or
lowered.  The storage denoted by one live result symbol therefore does not
overlap storage denoted by another live tensor symbol.

#### Special DSL tensor result temporary

The left-hand side of the result assignment is a special compiler-created
temporary with this required conjunction of properties:

```text
ST_class             CLASS_VAR
ST_IS_TEMP_VAR        set
ST_type               tensor TY_IDX
symbol attribute      no_alias=true
addressability        no LDA or address escape before lowering
definition shape      STID result_temp <- OPR_DSL(...)
use shape             LDID result_temp
```

This is a semantic specialization of the existing Open64 temporary symbol, not
a new `ST_CLASS`, storage-class encoding, or symbol-table image layout.  The
construction API is `DSL_Builder_Create_Tensor_Result_Symbol()`.  The builder
must establish the complete conjunction when the result temporary is declared;
passes must not infer it later merely from a generated name.

The `no_alias=true` assertion means that the result temporary has unique
ownership.  It is not permitted to overlap another live tensor result, and no
address-bearing WHIRL operation may create an alias before representation
lowering.  The gatekeeper verifies this assertion before DSL computation is
lowered.

The CUDA/PTX virtual-register model is the precedent: an instruction result is
carried by a uniquely named, non-addressable temporary, while physical register
allocation and spilling are deferred.  Likewise, a very-high-level DSL tensor
result may be defined by `STID` and used by `LDID`, but `LDA`, passing by
address, and other address escapes are illegal before lowering.  The DSL
gatekeeper must enforce this rule.

`no_alias` is value/symbol state.  It is not part of tensor type equivalence,
because multiple symbols may share one tensor `TY_IDX` while having distinct
ownership.  It is not an opcode attribute or compiler metadata.  Allocation
and placement may remain deferred even though ownership is already complete.

The very-high-level tensor symbol must not use baseline
`ST_PT_TO_UNIQUE_MEM` directly.  Existing WSSA gives that flag a pointer-typed
contract.  When lowering materializes a pointer representation, it must
preserve the DSL `no_alias=true` fact and may translate it to
`ST_PT_TO_UNIQUE_MEM` when the baseline pointer requirements are satisfied.

### Logical operator names hide the physical escape

The limited 8-bit WHIRL operator field is extended internally through one
physical escape tag.  That storage mechanism is not part of the compiler-facing
DSL model.  Compiler developers see and manipulate logical operators such as
`OPR_DSLADD`; they do not see `OPR_DSL` followed by a record-ID lookup.

The required abstraction is:

```text
public compiler view       OPR_DSLADD(kid0, kid1)
logical API                DSL_WN_operator(), DSL_OPERATOR_name()
private WN storage         escape tag plus logical node-record reference
```

The private row may contain the stable opcode identity, semantic version,
attributes, and result metadata, but only common DSL accessors, the mapped-image
boundary, and the low-level verifier decode it.  `ir_b2a`, ordinary internal
traces, diagnostics, frontends, optimizers, and lowering passes print or consume
the logical operator name directly.  They must never expose the physical escape
tag or internal record index.

This separation of concern is the reason for the DSL API: changing the physical
encoding must not require changes to compiler passes or visible IR vocabulary.

Physically, the binary IR image should store the hot compatibility state in one
fixed-layout tensor descriptor table.  The three layers are logical views over
one ordered record, not three unrelated binary tables.  This preserves Open64's
TABLE discipline: direct indexing, fixed offsets for common checks, and no
unordered key scan in verifier or lowering hot paths.

The first descriptor record should be shaped like this conceptually:

```c
typedef struct {
  TY_IDX element_ty;
  UINT16 rank;
  UINT16 flags;

  TENSOR_SHAPE_ID shape;
  TENSOR_LAYOUT_ID layout;
  TENSOR_STRIDES_ID strides;
  TENSOR_MEMORY_SPACE_ID memory_space;
  TENSOR_PLACEMENT_ID placement;
  TENSOR_SHARDING_ID sharding;
  TENSOR_QUANT_ID quantization;
  TENSOR_SPARSITY_ID sparsity;
  TENSOR_RUNTIME_STATE_ID runtime_state;
  TENSOR_TRAIT_SET_ID traits;
  TENSOR_LINEAGE_ID lineage;
} TENSOR_DESCRIPTOR_RECORD;
```

IDs inside the descriptor may point to auxiliary tables only when the payload is
variable-sized or naturally interned, for example symbolic shape expressions,
layout parameters, sharding meshes, quantization parameters, sparsity encodings,
or large trait sets.  Those auxiliary tables are not a substitute for the
fixed-layout descriptor; they are payload stores referenced by typed slots.

The existing key-value tensor extension table remains as a compatibility and
extension mechanism:

1. Migration bridge for existing `TY_tensor_*attribute*` storage.
2. Debug and dump representation.
3. Experimental fields that are not yet part of the compatibility hot path.
4. Compiler metadata that must not affect tensor type equivalence.

It must not be required for core compatibility checks.  A verifier should be
able to compare typed fields such as `element_ty`, `rank`, `shape`, `layout`,
`sharding`, `quantization`, and `memory_space` without scanning string keys.

Compatibility tiers:

1. Type equivalent: same tensor kind, element type, rank, and logical shape.
2. Representation compatible: type equivalent, and layout, strides, memory
   space, placement, sharding, quantization, sparsity, and runtime state are
   equal or have an explicit conversion.
3. Lowering compatible: representation compatible, and every required field is
   bound enough for target code generation.
4. Domain compatible: lowering compatible, and domain-required semantics such
   as residual lineage, mask meaning, image layout, classifier-head contracts,
   or projection/head-layout contracts remain visible through attributes,
   wrappers, or promotion records.

Binary image compatibility rules:

1. Existing non-tensor `TY_KIND` records remain readable without change.
2. Current `KIND_STRUCT` tensor carriers remain the compatibility fallback until
   `KIND_TENSOR` reader, writer, printer, verifier, and `ir_a2b` support land
   together.
3. A future tensor descriptor section must be versioned or guarded by section
   table count so older readers fail deterministically instead of silently
   misreading records.
4. `ir_bread` must accept legacy images that have no tensor descriptor table.
5. `ir_bwrite` must write tensor descriptors only when required computational
   fields are representable.
6. Before `KIND_TENSOR` lands, `ir_b2a -st` must print tensor information from
   the descriptor-shaped side-table view.  After `KIND_TENSOR` lands, the same
   fields must become canonical tensor type dump fields.
7. `ir_a2b` must either reconstruct equivalent tensor descriptors from ASCII or
   reject unsupported tensor records with a deterministic diagnostic.
8. Compiler metadata such as source context, diagnostics, pass owner, lowering
   hints, profiling, and pass-local ownership must not participate in tensor
   type equivalence.

### Tensor Descriptor Code Action List

1. Add a source-level `TENSOR_DESCRIPTOR_RECORD` sketch in `symtab.h` or a new
   common/com header, with typed fields only and no behavior change.
2. Add enum or typedef placeholders for descriptor payload IDs:
   `TENSOR_SHAPE_ID`, `TENSOR_LAYOUT_ID`, `TENSOR_STRIDES_ID`,
   `TENSOR_MEMORY_SPACE_ID`, `TENSOR_PLACEMENT_ID`, `TENSOR_SHARDING_ID`,
   `TENSOR_QUANT_ID`, `TENSOR_SPARSITY_ID`, `TENSOR_RUNTIME_STATE_ID`,
   `TENSOR_TRAIT_SET_ID`, and `TENSOR_LINEAGE_ID`.
3. Add accessor prototypes that expose logical views over the single physical
   record:
   `TY_tensor_type_core`, `TY_tensor_representation`, and
   `TY_tensor_trait_set`.
4. Keep the current key-value attribute table as the backing store until the
   fixed descriptor table exists.  Do not change compiler behavior in this
   step.
5. Add compatibility-check API prototypes:
   `TY_tensors_type_equivalent`, `TY_tensors_representation_compatible`, and
   `TY_tensors_lowering_compatible`.
6. Add diagnostics for missing fixed-slot fields before lowering.
7. Extend `Print_tensor_dsl_symtab` to group existing key-value fields under the
   future descriptor slots, making migration gaps visible.  This is the
   pre-`KIND_TENSOR` `ir_b2a -st` path: the carrier may still be `KIND_STRUCT`,
   but the dump is descriptor-shaped.
8. Add side-table based descriptor-view accessors that read from the existing
   key-value table and return typed-slot answers where fields are bound.  This
   enables verifier experiments without changing the core `TY_KIND` enum.
9. Run `dsl_ir_tools_smoke_test.sh` before and after any patch that reserves or
   changes binary image tensor descriptor sections.
10. Only after the source-level probe and descriptor-shaped dump are reviewed,
    reserve the binary image section/table name for the single descriptor
    table.
11. Add `KIND_TENSOR` only when the type printer, `ir_b2a -st` canonical tensor
    dump, binary reader, binary writer, ASCII reader/printer plan, verifier, and
    `KIND_STRUCT` fallback behavior are implemented in the same staged change.

## Stage 8: Promote Tensor To A First-Class WHIRL Type

Goal: Move from staging carrier types to a first-class common tensor type.

Coding changes:

1. Add `KIND_TENSOR` to `TY_KIND`.
2. Add tensor-aware accessors and verifier checks.
3. Decide whether tensor attributes remain side-table based or become part of
   the binary IR image type-extension records.
4. Update type printing and dumps.
5. Update type equivalence and IPA type merge logic.
6. Update WHIRL reader/writer versioning if binary IR image records change.

Primary files:

```text
osprey/common/com/symtab_defs.h
osprey/common/com/symtab_access.h
osprey/common/com/symtab.h
osprey/common/com/symtab.cxx
osprey/common/com/symtab_verify.cxx
osprey/common/com/ir_bread.cxx
osprey/common/com/ir_bwrite.cxx
osprey/ir_tools/ir_a2b.cxx
osprey/ipa/common/*type* or type merge files
```

Validation:

1. Existing scalar, array, struct, pointer, and function type behavior is
   unchanged.
2. Tensor types dump and reload correctly.
3. IPA merge either preserves tensor attributes or rejects incompatible tensor
   types with a useful diagnostic.
4. `ir_a2b` can read ASCII tensor type records and produce a binary IR image
   containing tensor type records, and `ir_b2a` can print that image back to
   ASCII, or this stage is not considered complete.

Exit criteria:

1. Tensor is represented by `TY_KIND = KIND_TENSOR`.
2. The staging `KIND_STRUCT` carrier can be treated as compatibility fallback.
3. ASCII reader/printer, binary image writer, binary image reader, dumper, and
   verifier support for `KIND_TENSOR` land together.

## Stage 9: DSL Lowering Pass

Goal: Consume very-high WHIRL DSL markers and tensor attributes before
standard optimization phases require canonical WHIRL.

Coding changes:

1. Add a VHO-adjacent DSL lowering pass.
2. Scan function bodies for DSL markers.
3. Validate required tensor attributes.
4. Lower supported DSL features to canonical WHIRL, runtime calls, or target
   intrinsics.
5. Report unlowered required markers.
6. Add a `-VHO:dsl_lower` or equivalent option.
7. If the lowering implementation uses C++ namespaces internally, expose a
   global driver entry point callable from existing VHO phase code.
8. Lowering pass registration should consume domain IDs and contract IDs from
   the registry before it depends on C++ namespace classes.

Primary files:

```text
osprey/be/vho/*
osprey/common/com/config_vho.h
osprey/common/com/config_vho.cxx
osprey/driver/OPTIONS
```

Validation:

1. Required DSL markers are either consumed or diagnosed.
2. Lowered functions no longer require DSL-only semantics.
3. Existing VHO lowering still sees legal WHIRL.
4. `ir_a2b` can still convert already-lowered WHIRL, and unlowered DSL marker
   input has a deterministic policy: preserve as comments or reject with a
   clear diagnostic.

Exit criteria:

1. A simple tensor operation can lower through the DSL pass.
2. Unsupported tensor attributes produce a clear error before WOPT/LNO/CG.

## Stage 10: Optional C++ Namespace Facade

Goal: Introduce C++ namespace organization only after domain registry and
contract metadata are stable.

Coding changes:

1. Add optional leaf-layer wrappers such as
   `namespace ant::domain::transformer`.
2. Keep common-core APIs global and stable.
3. Provide adapters from namespaced C++ classes to global registration calls.
4. Avoid moving existing `common/com` declarations into namespaces.
5. Gate the facade behind build settings if needed.

Primary files:

```text
osprey/dsl/*
osprey/be/vho/*
osprey/ir_tools/*
```

Validation:

1. The compiler can build and run without the namespace facade.
2. Namespaced adapters register the same domain IDs and contracts as the global
   API path.
3. No existing Open64 include path is forced to import a new namespace.

Exit criteria:

1. C++ namespaces become organizational sugar over stable registry data, not the
   source of semantic truth.

## Stage 11: Target-Specific Tensor Lowering

Goal: Map common tensor semantics to target-specific hardware forms only after
legality is known.

Coding changes:

1. Add target hooks for tensor lowering decisions.
2. For NVIDIA, introduce a target-specific representation for Tensor Core
   WMMA/MMA fragments if needed.
3. Keep general `MTYPE_TENSOR` out of the common DSL/source level.
4. Add target legality checks for shape, dtype, layout, and fragment size.
5. Lower valid cases to intrinsics or target-specific IR.
6. Lower invalid cases to runtime/library fallback or report diagnostics.

Primary files:

```text
osprey/common/com/NVISA/*
osprey/be/cg/*
osprey/be/vho/*
osprey/common/intr/*
```

Validation:

1. Common tensor types do not imply target-specific fragments prematurely.
2. NVIDIA Tensor Core fragments appear only after target lowering.
3. CPU targets continue to lower tensor operations through aggregate, runtime,
   or library paths.

Exit criteria:

1. One target-specific tensor kernel path is demonstrably separated from the
   common tensor type system.

## Stage 12: Tests And Diagnostics

Goal: Make the feature maintainable and debuggable.

Coding changes:

1. Add unit-style tests for DSL marker APIs.
2. Add tensor type extension tests.
3. Add Python DSL ingestion and binary WHIRL builder boundary regression inputs
   with DSL markers, first-class operators, tensor descriptors, opcode
   attributes, contracts, and compiler metadata.
4. Add dump output checks for tensor types and DSL markers.
5. Add negative tests for missing required tensor attributes.
6. Add domain registry and contract registry tests.
7. Add opcode registry and promotion registry tests.
8. Add tests proving domain IDs and opcode IDs do not require C++ namespaces.
9. Add CPROM diagnostic tests for invalid or unsafe promotion.
10. Add `ir_a2b`/`ir_b2a` smoke tests for non-DSL input, DSL-marker input, and,
   once enabled, first-class tensor type records.

Primary locations:

```text
test/
osprey/ir_tools/
doc/
```

Validation:

1. Tests cover marker creation, type extension declaration, delayed binding,
   lowering consumption, and diagnostics.
2. Dumps explain tensor attributes clearly enough for compiler debugging.
3. `ir_a2b` remains green for legacy WHIRL and has deterministic behavior for
   DSL-marked WHIRL.

Exit criteria:

1. A developer can modify tensor attribute handling and catch regressions before
   backend lowering.
2. `ir_a2b` remains a required green check for every staged landing.

## Migration Rules

1. Do not replace `KIND_STRUCT` staging carriers until `KIND_TENSOR` binary IR
   image layout, ASCII form, and equivalence are designed.
2. Do not introduce common `MTYPE_TENSOR` for source-level DSL tensors.
3. Target-specific tensor fragment `MTYPE`s are allowed only after lowering.
4. Keep DSL metadata explicit and inspectable.
5. Every stage must preserve legal WHIRL for components that are not yet
   DSL-aware.
6. Do not introduce namespaces into existing common-core headers as part of DSL
   IR work; add namespaced code only behind global compatibility wrappers.
7. Treat namespace hierarchy as semantic registry data first and C++ namespace
   syntax second.
8. Do not make the gatekeeper depend on C++ namespace availability.
9. Do not land a staged change unless `osprey/ir_tools/ir_a2b.cxx` either keeps
   working unchanged or is updated in the same stage with matching tests.
10. Do not promote an opcode into `common.*` unless the promotion registry can
    prove that domain-specific ABI and gatekeeper checks remain visible through
    wrappers or partial-promotion rules.
11. Do not lower semantically rich domain ops such as `cnn.conv2d` or
    `transformer.attention` into common tensor contractions before domain
    verification has certified their contracts.
12. Treat Python as a source-language frontend only; the Open64 middle end must
    consume a binary WHIRL artifact without depending on the original Python
    interpreter.
13. Do not use intrinsic ops, runtime calls, or target libraries as the primary
    ingestion representation for domain/common operators.  They are products of
    explicit lowering after gatekeeper verification and target-independent
    optimization.
14. Keep the binary artifact model aligned with Open64's mapped IR image design.
    The V0.9 design name for the artifact boundary is
    `WhirlMappedImageFinalizer`, reflecting direct mapped-image finalization.

## Near-Term Coding Queue

Completed:

1. Add iteration APIs for `TY_IDX` tensor attributes and `ST_IDX` tensor
   metadata, not just key lookup.
   Implemented by commit `18491fc0` with `TY_tensor_attribute_count`,
   `TY_tensor_attribute_at`, `ST_tensor_metadata_count`, and
   `ST_tensor_metadata_at`.
2. Add a small tensor schema enum for common keys.
   Implemented by commit `ac7f0bf6` with `TY_TENSOR_SCHEMA_KEY`,
   `TY_tensor_schema_key_name`, and enum-based tensor attribute / metadata
   accessors.
3. Add a first VHO scanner that reports unconsumed DSL markers.
   Implemented by commit `57a4ebcd` with `VHO_Scan_Unconsumed_DSL_Markers`,
   `VHO_Has_Unconsumed_DSL_Markers`, and
   `VHO_fprint_unconsumed_DSL_markers`.
4. Add diagnostics for unbound required tensor attributes.
   Implemented by commit `1b9ff531` with
   `TY_tensor_unbound_required_attribute_count`,
   `TY_tensor_has_required_attributes`, and
   `TY_tensor_fprint_unbound_required_attributes`.
5. Add `dsl_domain.h` and `dsl_domain.cxx` with string-based domain registry
   APIs.
   Implemented by commit `10888e75` with `DSL_Domain_Register`,
   `DSL_Domain_Find`,
   `DSL_Domain_Get_Info`, `DSL_Domain_At`, and
   `DSL_Domain_fprint_registry`.
6. Add a small contract registry stub that models source domain, target domain,
   required checks, and diagnostic codes.
   Implemented by commit `97e5a842` with `DSL_Contract_Register`,
   `DSL_Contract_Find`, `DSL_Contract_Get_Info`,
   `DSL_Contract_Required_Check_At`, and
   `DSL_Contract_Diagnostic_Code_At`.
7. Add `dsl_opcode.h` and `dsl_opcode.cxx` with category, owner, level,
   version, shape, effect, and lowering metadata.
   Implemented by commit `291e88c2` with `DSL_Opcode_Register`,
   `DSL_Opcode_Find`, `DSL_Opcode_Get_Info`, enum-name helpers, and
   `DSL_Opcode_fprint_registry`.
8. Seed the common opcode registry with Level 0 through Level 4 substrate
   opcodes.
   Implemented by commit `7a9fa050` with
   `DSL_Opcode_Register_Common_Substrate`, which registers the planned
   `common.*` operator descriptors under the runtime `common` domain.
9. Add domain wrapper examples for `cnn.linear`,
   `transformer.q_projection`, `cnn.residual_add`, and
   `transformer.residual_add`.
   Implemented by commit `607ddd02` with
   `DSL_Opcode_Register_Domain_Wrapper`,
   `DSL_Opcode_Register_Domain_Wrapper_Examples`, and wrapper target
   metadata in `DSL_OPCODE_INFO`.
10. Add the opcode promotion registry and CPROM diagnostics.
    Implemented by commit `ce4c4728` with
    `DSL_Opcode_Promotion_Register`,
    `DSL_Opcode_Promotion_Register_Examples`,
    `DSL_Opcode_Check_Promotion`, promotion-state names, and stable
    `CPROM-001` through `CPROM-010` diagnostic codes.
11. Add an `ir_a2b`/`ir_b2a` smoke test fixture before expanding binary IR
    image type records.
    Implemented by commit `d7378bd6` with
    `osprey/common/com/tests/dsl_ir_tools_smoke_test.sh`, which generates a
    fresh `.B` file, verifies `ir_b2a` output, and records the current
    deterministic `ir_a2b` compatibility gate.
12. Add a source-level tensor descriptor record probe without behavior change.
    Implemented in the current working tree with typed placeholder IDs,
    `TENSOR_DESCRIPTOR_RECORD`, `TENSOR_DESCRIPTOR_RECORD_Init`, and
    `TY_get_tensor_descriptor_record`.  The record is a fixed-layout source
    view over the existing tensor extension and key-value attribute backing
    store; it does not add `KIND_TENSOR`, reserve a binary section, or change
    binary reader/writer behavior.
13. Add descriptor-shaped `ir_b2a -st` side-table dumping.
    Implemented in the current working tree by extending
    `Print_tensor_dsl_symtab` to show a `TensorDescriptorIR view` grouped into
    `TensorTypeCore`, `TensorTraitSet`, `TensorRepresentationDescriptor`, and
    `TensorLineageMetadata`.  The dump still uses the existing tensor extension
    and key-value backing store, preserves the raw key-value view, makes missing
    fixed slots visible as `<missing>`, and does not add `KIND_TENSOR`.
14. Add native C++ ingestion-readiness fixtures before Python bindings.
    Implemented in the current working tree with
    `osprey/common/com/tests/dsl_ingestion_fixture.h`.  The fixture simulates
    future Python builder calls from native C++ tests by creating staged tensor
    types, symbols, tensor constants, `common.add`, and `common.matmul`, while
    attaching opcode attributes as static operation parameters and source
    context/lowering hints through compiler metadata.  It does not introduce
    Python, pybind, packaging, driver integration, native WHIRL tensor opcodes,
    `KIND_TENSOR`, or binary image sections.
15. Sketch the minimal C++ builder-facing API without changing compiler
    behavior.
    Implemented in the current working tree with
    `osprey/common/com/dsl_builder.h`.  The header defines the native builder
    boundary for tensor type core creation, TensorDescriptorIR attachment,
    symbol creation, first-class DSL operator creation, contract attachment,
    compiler metadata attachment, and mapped image finalization.  It documents
    possible future C++ facade names while keeping Open64-style API names as
    the first public contract, and it explicitly keeps intrinsic creation out
    of the frontend construction API.
16. Add the smallest source-level binary image probe after the descriptor-shaped
    dump is reviewed.
    Implemented in the current working tree by reserving
    `WT_DSL_TENSOR_DESCRIPTOR` / `.WHIRL.dsl_tensor_descriptor` in
    `osprey/include/sys/elf_whirl.h` and adding
    `osprey/common/com/dsl_ir_image.h` as a source-level description of the
    future fixed-row tensor descriptor image.  This probe reserves only the
    vocabulary for a future combined TensorDescriptorIR image section; it does
    not add `KIND_TENSOR`, does not connect to `ir_bread.cxx` or
    `ir_bwrite.cxx`, and does not change current binary reader/writer behavior.
17. Add `KIND_TENSOR` as the first integrated type-system change.
    Implemented in the current working tree by adding `KIND_TENSOR = 7` while
    preserving all existing `TY_KIND` values and keeping `KIND_LAST = 8`.  The
    native tensor TY path uses `TY_Create_Tensor_Type`, records element type,
    rank, and TensorDescriptorIR slots in the existing tensor extension side
    table, and leaves `TY_Create_Tensor_Extension_Type` available as the
    `KIND_STRUCT` carrier fallback.  `Kind_Name`, `TY::Print`,
    `ir_b2a -st`-visible descriptor dumping, verification, type hashing, and
    type equivalence now understand native tensor TYs.  The binary table layout
    is unchanged; existing reader/writer paths continue to move TY records and
    side-table-backed staged tensor descriptors without adding a new emitted
    tensor descriptor ELF section yet.
18. Add the Chapter 7 Python DSL ingestion design note before coding the
    frontend.
    Implemented in this plan by adding the `Chapter 7 Ingestion Boundary Design
    Note` under Stage 1.  The note defines `open64_dsc` as a thin Python
    capture/interpreter package, assigns WHIRL node creation, symbol/type table
    ownership, TensorDescriptorIR attachment, opcode attributes, contracts,
    compiler metadata, and mapped image finalization to the native C++ builder
    boundary, names `WhirlMappedImageFinalizer` as the artifact-finalization
    component, preserves first-class operators through gatekeeper and
    target-independent optimization, and states that `mpl2whirl` is not the
    active DSL ingestion path.
19. Define the first ResNet inference vertical-slice operator list in the
    opcode registry plan.
    Implemented in this plan by adding the `First ResNet Inference Vertical
    Slice` section under Stage 1.  The slice includes `cnn.conv2d`,
    `cnn.batch_norm_infer`, `common.relu`, `cnn.max_pool2d`,
    `common.residual_add`, `cnn.global_avg_pool2d`, `common.flatten`,
    `common.linear`, and `common.output_logits`.  It characterizes operand
    policy, static opcode attributes, TensorDescriptorIR ownership, compiler
    metadata ownership, and the rule that CNN-specific operators remain domain
    operators until domain gatekeeper verification completes.
20. Add the staged driver artifact boundary plan.
    Implemented in this plan by adding the `Staged Driver Artifact Boundary`
    section.  The first workflow is explicitly two-step:
    `torch2whirl model.py -o model.B`, then `opencc -x whirl model.B ...`.
    The combined `opencc -frontend=torch2whirl ...` mode is deferred until the
    binary WHIRL artifact boundary is stable, inspectable with `ir_b2a -st`,
    and consumable by Open64 without importing Python.

Parked:

1. Add a sample MPL tensor marker path once MPL tensor syntax is identified.
   Status: precondition not met.  The current `mpl2whirl` path translates
   Maple primitive, pointer, array, struct, union, and function types, but no
   first-class MPL tensor syntax or tensor type kind has been identified in the
   checked-in source.  Do not add speculative `mpl2whirl.cxx` marker hooks;
   resume this item only after the MPL tensor source syntax and MIR type
   representation are explicitly defined.

Active queue:

The current near-term planning queue is complete.  Start the next queue only
after reviewing the completed plan items and choosing whether the next phase is
opcode-registry implementation, native builder API implementation, binary image
persistence, or Python frontend scaffolding.
