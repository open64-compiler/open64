# DSL WHIRL CNN Region Contracts

This document introduces CNN region contracts to Open64 compiler developers
who may not be familiar with neural-network model structure. It complements
`WHIRL-DSL-INFRASTRUCTURE.md`, which owns the common REGION, RID, builder,
mapped-image, verifier, and inspection mechanisms. This document owns the
domain meaning and compiler opportunities built on those mechanisms.

The first detailed subject is the proposed `cnn.bottleneck.v1` contract. This
document should later include `cnn.basic_block.v1`, shortcut, ResNet stage, and
other CNN region contracts so their relationships remain visible.

## Three Different Entities

The name "Bottleneck" appears at three levels:

1. A source model may define a Python `torch.nn.Module` named `Bottleneck`.
2. PyTorch executes or captures the tensor operations in that module.
3. Open64 may preserve the verified semantics as an `OPR_REGION` with the
   logical contract `cnn.bottleneck.v1`.

The source class name is frontend evidence, not proof. The native CNN
gatekeeper certifies the REGION topology, operands, attributes, tensor types,
shortcut, and result before the contract can be trusted.

`cnn.bottleneck.v1` is an Open64 contract name. It is not a PyTorch opcode and
does not share version numbering with ResNet V1, ResNet V1.5, pretrained
weights, or model releases.

## Bottleneck in ResNet-50

TorchVision constructs ResNet-50 from Bottleneck blocks in four stages with
block counts `[3, 4, 6, 3]`, for a total of 16 blocks. A typical block has a
three-convolution main path and a shortcut path:

```text
input
  |
  +----------------------------- shortcut -------------------+
  |                                                          |
  +-> conv2d 1x1 -> batch_norm -> relu                       |
      -> conv2d 3x3 -> batch_norm -> relu                    |
      -> conv2d 1x1 -> batch_norm                            |
                                                             |
                     residual_add(main, shortcut) <-----------+
                                  |
                                 relu
                                  |
                                result
```

The first `1x1` convolution selects an internal channel width. The `3x3`
convolution performs spatial computation. The last `1x1` convolution expands
or projects to the output width. TorchVision's standard Bottleneck has an
expansion factor of four.

The shortcut is an identity when its input already matches the result. It is a
projection when channel count, stride, or another required representation fact
differs. A projection commonly contains a `1x1` convolution and normalization.

TorchVision places downsampling stride in the middle `3x3` convolution and
calls this ResNet V1.5 behavior. The original ResNet description placed that
stride in the first `1x1` convolution. The contract and verifier must preserve
this domain-semantic distinction.

Reference implementation:
`https://github.com/pytorch/vision/blob/main/torchvision/models/resnet.py`.

## What PyTorch Preserves

In eager execution, `Bottleneck` is an ordinary `torch.nn.Module`. Calling it
runs its convolution, normalization, activation, shortcut, and addition
operations through the PyTorch dispatcher.

Default FX tracing normally traces through a user-defined Bottleneck while
treating standard `torch.nn` children such as `Conv2d` as leaf modules. The
graph therefore contains the internal operations, not one atomic Bottleneck
operation. The tracer can still observe the module path while entering and
leaving `Bottleneck.forward()`.

`torch.export` produces a more normalized and flattened tensor graph. Source
and module metadata may remain, but the Python module hierarchy is not the
primary computational structure.

Torch2whirl records the enclosing module path and kind while that information
is available. It sends opaque region and value requests to the native builder.
Python does not construct a WN, RID, `ST_IDX`, or mapped-image row.

## Proposed WHIRL Contract

The native builder creates a standard `OPR_REGION` with:

- `REGION_KIND_PRAGMA`;
- an ordered body of first-class CNN and common operations;
- contract name and version `cnn.bottleneck.v1`;
- stable region and optional parent IDs;
- statement-level source position;
- an ordered symbol interface; and
- an empty exits block for this first CNN form.

The physical REGION is statement-level and does not itself return a tensor.
The enclosing scope owns a unique result symbol. The region declares that
symbol as `OUTPUT|RESULT` and stores the final activation into it. This model
supports the planned no-alias result temporary without treating `OPR_REGION`
as an expression operator.

A normal interface begins as:

```text
INPUT:
  activation entering the block

OUTPUT | RESULT:
  final activation leaving the block
```

The current mapped role bits are `INPUT = 0x1` and
`OUTPUT | RESULT = 0xa`. Every external value consumed by the region needs a
defined input or constant-ownership interpretation. Every value defined in the
region and consumed outside it must appear in the output interface.

## Meaning of Version 1

The `.v1` suffix versions the Open64 semantic contract. Version 1 fixes the
interface meaning, required topology, result semantics, legal operator order,
tensor compatibility rules, static attribute interpretation, verifier, and
lowering expectations.

Optional backward-compatible information should not automatically create
version 2. A new version is appropriate when an existing consumer would
otherwise misinterpret the region. Architecturally different forms should
usually use different names, such as `cnn.preact_bottleneck.v1` or
`cnn.inverted_bottleneck.v1`, rather than unrelated Bottleneck version numbers.

## Gatekeeper Requirements

The first `cnn.bottleneck.v1` gatekeeper should verify at least:

1. One principal activation input and one `OUTPUT|RESULT` activation.
2. Three main-path convolutions with compatible `1x1`, `3x3`, `1x1` roles.
3. Required normalization and activation ordering.
4. A `common.residual_add` combining the main path and correct shortcut.
5. Identity shortcut legality when no projection exists.
6. Projection legality when shape, channels, stride, layout, or other
   representation facts require it.
7. Compatible dtype, rank, logical shape, layout, placement, sharding, and
   quantization state at the residual merge.
8. Static kernel, stride, padding, dilation, and group parameters remain opcode
   attributes.
9. Region-crossing values agree with the declared symbol interface.
10. Contract version, source position, IDs, and parent/depth relations are
    valid.

The frontend may propose the contract from source structure. It cannot make an
arbitrary operation sequence valid merely by naming it Bottleneck.

## Compiler Knowledge and Value

A verified Bottleneck region tells the compiler more than a flat operation
list:

- which operations form one architectural block;
- which path is the main path and which is the shortcut;
- whether the shortcut is identity or projection;
- where downsampling and channel expansion occur;
- which value is the externally visible result;
- which values cross the block boundary;
- which parameters belong to each block instance;
- how blocks are nested within a source stage; and
- where the block came from in `model.py`.

This knowledge can support:

- whole-block fusion and scheduling;
- shortcut-aware legality and shape checking;
- layout, placement, sharding, and memory planning;
- quantization and dequantization placement;
- region-local redundancy analysis;
- architecture-independent parallelization;
- compiler-library codesign and kernel dispatch selection;
- stable optimization boundaries and cost-model features; and
- diagnostics that relate transformed WHIRL to the source model.

The contract preserves domain intent for VHO optimization. It does not require
every optimization to be Bottleneck-specific. A pass may use the contract for
legality, transform the body with common analyses, and retain or update the
contract as required.

## Relation to model.py and Loop Structure

For a source hierarchy such as:

```text
layer3.0  Bottleneck
layer3.1  Bottleneck
layer3.2  Bottleneck
layer3.3  Bottleneck
layer3.4  Bottleneck
layer3.5  Bottleneck
```

binary WHIRL can preserve six ordered `cnn.bottleneck.v1` regions, module
paths, source positions, parameter symbols, interfaces, and dataflow. This is
enough to recover an equivalent repeated-block organization and propose a
containing ResNet-stage region.

It does not prove that the source used a Python `for` statement. The same
sequence may come from `nn.Sequential`, explicit calls, or a construction-time
loop. FX and export commonly unroll static Python iteration, so the original
iteration variable and syntax may no longer exist.

The compiler must also account for block-specific parameters such as
`layer3.0.conv1.weight` and `layer3.1.conv1.weight`. Replacing consecutive
regions with a runtime WHIRL loop requires a valid indexed parameter model and
proof that the transformation preserves semantics. Otherwise, the ordered
region sequence is the correct representation.

A future `cnn.resnet_stage.v1` region could summarize block contract, block
count, stage input/output channels, first-block stride, and shortcut pattern.
That stage contract would be a better decision point for loop formation,
parallel execution, fusion, or deliberate unrolling.

## Inspection Form

`ir_b2a -st -src` should expose logical evidence without requiring physical
table indices or runtime pointers:

```text
DSL REGION TABLE:
REGION id=7 parent=3 depth=2 kind=0 contract=cnn.bottleneck.v1
  VALUE ordinal=0 st=<2,41> roles=0x1 flags=0x0
  VALUE ordinal=1 st=<2,73> roles=0xa flags=0x0
```

The standard REGION body remains visible with source-interleaved operators.
The logical table supplies stable contract and interface evidence for human
review and regression tests.

## VHO Lifetime and Canonical Lowering

CNN contract regions are semantic VHO scopes, not independent CG compilation
units. They remain intact in binary VHO WHIRL while the DSL gatekeeper and
architecture-independent VHO passes can use block topology, interfaces, and
source context. `VHO_DSL_Lower_Driver` then lowers the contained operators and
splices the body of each managed DSL region into its enclosing block before
standard VHO, WOPT, LNO, or CG.

This rule is deliberately selective. It recognizes a region through the
common DSL region registry; it does not flatten historical MP, EH, or ordinary
pragma regions. The mapped `WT_REGIONS` view never occupies
`PU_Info_regions_ptr`, which remains reserved for the backend's runtime `RID*`
contract. Backend `REGION_Initialize` continues to derive its private RID tree
from canonical `OPR_REGION` nodes.

The retained artifacts demonstrate both sides of the boundary:

- `ir_b2a -st -src model.B model.T` shows `cnn.basic_block.v1`, its standard
  REGION body, source positions, and the logical DSL REGION table.
- `openpy -O0 -keep model.py` writes `model.t` after DSL lowering; managed CNN
  regions have been spliced away and standard REGION initialization sees only
  the function RID before CG.
- An independent `-O3 -apo` fixture retains the historical MP RID,
  `PARALLEL_DO`, shared-array, and reduction evidence with identical baseline
  and candidate runtime output.

## Implementation Status

`cnn.basic_block.v1` is the implemented first vertical slice. The common
REGION/RID service, pointer-free `WT_REGIONS` image, opaque builder APIs,
source positions, verifier, logical inspection path, and contract-selective
VHO body splicing are available. The complete `openpy -O0 -keep` path produces
the binary VHO artifact, source-aware ASCII inspection, lowering trace, and
object file.

`cnn.bottleneck.v1` remains planned. Completion requires:

1. A centrally registered CNN contract definition.
2. A ResNet-50 or focused Bottleneck ingestion fixture.
3. Frontend preservation of Bottleneck module scope.
4. Native topology, shortcut, interface, and tensor compatibility checks.
5. Binary write/reopen and `ir_b2a -st -src` evidence.
6. Negative tests for malformed topology and incompatible shortcut results.
7. VHO lowering and optimization ownership rules.

The contract becomes a compatibility commitment only after these gates are
reviewed and the version 1 semantics are fixed.
