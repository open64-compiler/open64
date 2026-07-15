# Native ResNet WHIRL Operator Plan

## Purpose

This is the main-agent plan for extending `common/com`, the native WHIRL
builder, the DSL gatekeeper, binary image inspection, and VHO DSL lowering so a
torch2whirl producer can emit a complete ResNet inference graph.

The work proceeds concurrently with
`osprey/torch2whirl/RESNET-INGESTION-PLAN.md`.  This document owns the compiler
contract.  The torch2whirl plan consumes that contract through opaque builder
handles and must not reproduce WHIRL internals in Python.

## Certified Artifact

A ResNet `.B` file is certified only when all of the following hold:

1. The native torch2whirl backend writes the artifact through the existing
   mapped-image and ELF WHIRL framework.
2. A separate `ir_b2a -st` process reads the artifact and displays the complete
   logical ResNet graph, symbol/type tables, tensor descriptors, opcode
   attributes, value ownership, and all DSL image tables.
3. The output exposes logical operator names and never exposes `OPR_DSL`,
   `OPC_MDSL`, `MDSL`, or private record indices.
4. `DSL_Gatekeeper_Verify_Program()` accepts every PU before output and again
   before lowering.
5. Every tensor result is a complete TensorDescriptorIR value carried by a
   unique, non-addressable, `no_alias=true` temporary.
6. Immutable parameters and BatchNorm state resolve through a checked external
   tensor side-file manifest; model inputs are runtime values, not constants.
7. `opencc -x whirl -O0` completes mandatory VHO DSL lowering and proves that
   no executable native, EVAL, or XPRAGMA DSL carrier reaches standard VHO,
   WOPT, LNO, or CG.  High-level `OPR_COMMENT` projections remain legal.
8. Existing baseline WHIRL and older files without a DSL section remain
   readable and unchanged.

## Shared Native Contract

All logical operator enum values and names are append-only compatibility
contracts.  Static operation parameters are typed opcode attributes.  Tensor
semantic state belongs in TensorDescriptorIR.  Source names, diagnostics,
pass ownership, lowering hints, and profile information remain compiler
metadata.

The three released native operators retain version 1.  The ten ResNet names
already had released marker-only version-1 descriptors, so their native
contracts are appended as version 2 instead of changing version 1 in place.

| Stable operator | Native version | Kids | Required attributes | Result rule | Initial `-O0` lowering |
| --- | ---: | ---: | --- | --- | --- |
| `common.model_input` | 2 | 0 | `attr.input_ordinal` | Declared input descriptor | Runtime input handle |
| `common.tensor_const` | 1 | 0 | `value_kind`, `value` | Declared constant descriptor | Splat or external-data handle |
| `cnn.conv2d` | 2 | 3 | kernel, stride, padding, dilation, groups, input/weight/output layout | Convolution formula | Versioned runtime call |
| `cnn.batch_norm_infer` | 2 | 5 | epsilon, training=false, input layout, channel axis | Input shape/layout | Versioned runtime call |
| `common.relu` | 2 | 1 | none | Input descriptor | Versioned runtime call |
| `cnn.max_pool2d` | 2 | 1 | kernel, stride, padding, dilation, ceil mode | Pooling formula | Versioned runtime call |
| `common.residual_add` | 2 | 2 | broadcast=none, shape_check=exact, residual_path=true | Exact operand descriptor | Verify, then lower to add |
| `cnn.global_avg_pool2d` | 2 | 1 | output size, reduction axes | Spatial dimensions become one | Versioned runtime call |
| `common.flatten` | 2 | 1 | start dimension, end dimension | Collapsed logical dimensions | Owning runtime result |
| `common.linear` | 2 | 3 | bias, input/weight transpose, weight layout | Batch dimensions plus output features | Versioned linear call |
| `common.output_logits` | 2 | 1 | semantic role | Input descriptor plus output role | Owning output handle |

The physical WN representation remains private.  The existing value-oriented
frontend APIs remain stable wherever possible:

```text
DSL_Builder_Create_Operator
DSL_Builder_Append_PU_Value
DSL_Builder_Count_PU_Values
DSL_Builder_Get_PU_Value
DSL_Builder_Finalize_Mapped_Image
```

## Main-Agent Ownership

The main agent owns:

1. Append-only logical operator IDs and table-driven descriptors.
2. Native result-producing WN construction and direct operand kids.
3. Result type inference and TensorDescriptorIR construction.
4. No-alias result symbols and address-escape enforcement.
5. Fixed-row opcode, node, attribute, value, and value-reference records.
6. Binary mapped-image revision, writer, reader, and compatibility behavior.
7. Logical tree and `ir_b2a -st` printing.
8. Gatekeeper rules and stable diagnostics.
9. Versioned runtime ABI declarations and VHO DSL lowering.
10. Native C++ regression fixtures and baseline WHIRL protection.

The main agent does not own PyTorch graph capture, Python module loading,
safetensors file generation, or Python CLI behavior.

## Implementation Queue

### M0: Freeze Operator Contracts

- [x] Append the logical enum values without renumbering released values.
- [x] Add every operator to the authoritative seed with version, arity,
  category, level, shape rule, effects, lowering model, attribute schema, and
  diagnostic prefix.
- [x] Add contract tests that lock enum values, stable names, and schemas.
- [x] Publish the exact attribute spelling and defaults to the torch2whirl plan.

Exit gate: torch2whirl can emit stable names and attributes without depending
on unfinished physical construction.

### M1: Model Inputs And External Parameters

- [x] Make `common.model_input` a native value source with a complete declared
  descriptor and runtime input ordinal.
- [x] Extend `common.tensor_const` so `splat` and `external_data` are distinct,
  verified value kinds.  Do not parse an external URI as a scalar.
- [x] Define the version-1 external tensor reference: storage format, side-file
  identity, tensor key, byte offset, byte length, checksum, dtype, shape, and
  layout.
- [x] Verify bounds, alignment, dtype, shape, and checksum metadata before
  artifact finalization.
- [x] Add runtime ABI entry points for input and external immutable handles.

Exit gate: a native PU can consume a model input and immutable ResNet weights
without compatibility carriers.

G1 completed on 2026-07-12.  The builder and gatekeeper independently check
input ordinals, structured external references, unsigned range overflow,
dtype alignment, static shape byte size, checksum form, representation, and
the URI projection consumed by the versioned runtime ABI.  The producer owns
checking the actual side-file size and computing its digest before C++
finalization; C++ owns validating that the supplied facts are mutually
consistent in WHIRL.  The linked lowering fixture proves that both value
sources pass the gatekeeper, lower to distinct runtime calls, and capture
their returned handles in unique PREGs.  Duplicate input ordinals and tampered
external metadata are rejected.

### M2: Simple ResNet Values

- [x] Add native `common.relu`.
- [x] Add native `common.flatten` with checked dimension normalization.
- [x] Add native `common.residual_add` with exact-shape verification.
- [x] Add native `common.output_logits` and preserve its semantic output role.
- [x] Populate all fixed-row image records from the same construction calls.
- [x] Add logical dump, mapped-image roundtrip, malformed-input, and lowering
  tests for each operator.

Exit gate: a residual fragment can be built, verified, printed, reopened, and
lowered without marker-only values.

G2 completed on 2026-07-12.  The generic builder now creates all four logical
version-2 operators as native result-producing expressions.  ReLU,
residual-add, and output-logits preserve the input descriptor while producing
distinct no-alias values.  Flatten normalizes negative or positive dimensions,
checks a static shape, and creates a new descriptor with the collapsed shape
and `common.flatten` lineage.  The gatekeeper independently verifies exact
residual semantics, the logits role, and the flatten result formula.

The producer-style fixture writes a real `.B`, and a separate `ir_b2a -st`
process reopens it and displays all four logical names, typed attributes,
direct value references, `[4]` flatten result, and `no_alias=true` symbols.
The dump retains high-level comment projections, uses result symbol names for
intermediate operands, and does not expose `OPR_DSL` or `MDSL`.  Mandatory
lowering emits version-1 runtime ABI calls and captures all five returned
handles, including the model input, in distinct pointer-sized PREGs.

### M3: Linear Layer

- [x] Add native `common.linear` with input, weight, and bias kids.
- [x] Verify rank, feature compatibility, transpose flags, and weight layout.
- [x] Infer the complete output descriptor.
- [x] Keep linear visible through VHO optimization; do not decompose it during
  ingestion.
- [x] Add an `-O0` runtime-call lowering.  A later optional pass may select a
  library call or decompose it to matmul and add.

Exit gate: the ResNet classifier tail reaches an owning logits result.

### M4: CNN Operators

- [x] Add native `cnn.conv2d` with full static parameter verification and the
  NCHW/OIHW native version-2 shape formula.
- [x] Add native `cnn.batch_norm_infer`; reject training mode and missing
  running statistics.
- [x] Add native `cnn.max_pool2d` with floor/ceil shape semantics.
- [x] Add native `cnn.global_avg_pool2d` with explicit spatial axes.
- [x] Preserve CNN operators until their domain checks complete; do not promote
  or fuse them during ingestion.
- [x] Add versioned runtime calls as the mandatory `-O0` routes.

Exit gate: a ResNet stem, basic block, downsample block, and pooling tail each
pass native construction, gatekeeping, binary inspection, and lowering.

### M5: Whole-Graph Gatekeeper

- [x] Verify topological definitions and direct operand `LDID`s across the full
  model.
- [x] Verify every result descriptor, required typed attribute, domain contract,
  and external tensor reference.
- [x] Reject result address escape, duplicate definition, unsupported version,
  missing operator, stale image row, and source-metadata/type confusion.
- [ ] Check that result shapes observed by torch2whirl agree with the C++ shape
  functions; C++ remains authoritative.

Exit gate: finalization refuses malformed ResNet graphs and writes no partial
artifact.

### M6: Binary Inspection And Lowering

- [x] Extend logical printing for every new operator and attribute schema.
- [x] Confirm all new rows use the existing optional DSL ELF section and current
  revision policy.
- [x] Lower every verified result to a distinct pointer-sized PREG handle.
- [x] Preserve source positions, compiler metadata, descriptor constants, and
  requested comment projections.
- [x] Run the canonical-boundary scan before standard language VHO.

Exit gate: the complete model survives write/read in a separate process and
`opencc -x whirl -O0` leaves no executable DSL carrier.

### M7: Main-Agent Certification

- [x] Build a C++ ResNet-shaped producer independent of Python.
- [x] Write and reopen the complete `.B` image.
- [x] Check `ir_b2a` and `ir_b2a -st` for every operator and DSL table.
- [x] Run valid, malformed, unknown-version, address-escape, old-image, and
  baseline tests.
- [x] Run `opencc -x whirl -O0` through object generation.  Linking remains a
  runtime implementation concern.
- [x] Publish the exact build and validation commands for the torch2whirl
  certification run.

### M8A: Promote A Common WHIRL Region Substrate

- [ ] Inventory region services into WN-structural and optimizer-RID groups;
  document the ownership boundary before moving code.
- [ ] Add a narrowly scoped `common/com` region API for construction, primary
  and supporting pragmas, structural verification, source-position
  propagation, region IDs, and safe body splicing.
- [ ] Promote stable RID identity, hierarchy, and centrally managed WN-to-RID
  lookup into `common/com`; retain the existing global map interface as a
  compatibility wrapper during migration.
- [ ] Bring up the reserved `WT_REGIONS` subsection as a fixed-row mapped-image
  table containing stable region IDs, parent IDs, kinds, depths, and
  PU-tree-relative WN offsets.  Never place `WN*` or `RID*` addresses in a row.
- [ ] Add a repeatable region symbol-interface table with `ST_IDX`, ordinal,
  compatibility flags, and provisional `INPUT`, `OUTPUT`, `INOUT`, and
  `RESULT` role bits.  Keep its physical rows private behind common APIs.
- [ ] Implement the common API as wrappers over the existing `OPR_REGION`,
  `WN_CreateRegion`, and block utilities.  Do not change WN layout, region-kind
  encodings, or existing public entry points.
- [ ] Add common/com tests for empty and populated pragma regions, first-pragma
  classification, parent/statement links, declared symbol interfaces,
  mapped-image roundtrip, logical printing, and body splicing.
- [ ] Migrate the CNN builder first so the new API is exercised without
  changing LNO.
- [ ] Migrate pure structural queries now pulled from `be/region` by
  `common/com`; remove backend includes from common code where behavior can be
  preserved exactly.
- [ ] Convert one LNO automatic-parallelization construction path at a time,
  retaining its dependence updates, derived boundary sets, and parentization
  sequence while redirecting RID creation and lookup through common services.
- [ ] Compare pre-migration and post-migration LNO WHIRL dumps and run
  automatic-parallelization tests after every converted call site.
- [ ] Keep optimizer levels, derived alias/preg boundary sets, region bounds,
  and backend lowering in `be/region`; use them to check or refine the declared
  common symbol interface rather than making them mapped-image contracts.

Exit gate: frontend and backend clients share one common region and RID service,
the mapped image preserves stable region and declared symbol-interface rows,
LNO output is unchanged, and backend-derived boundary analysis remains intact.

### M8B: Preserve CNN Block Structure With WHIRL Regions

- [ ] Define an append-only logical pragma contract for CNN BasicBlock,
  Bottleneck, input, result, shortcut kind, stage, and block ordinal.
- [ ] Represent each structured CNN block as an `OPR_REGION` with
  `REGION_KIND_PRAGMA`, an empty exits block, a CNN contract pragma block, and
  an ordered operator body block.
- [ ] Model construction after LNO automatic parallelization: make the first
  pragma the classifier, propagate line numbers, assign a region ID, parentize
  the completed region, and insert it into the enclosing statement block.
- [ ] Have the native builder populate the managed common RID and region-symbol
  tables.  Python continues to see only opaque value and region handles.
- [ ] Keep convolution, normalization, activation, and residual operations as
  first-class expression-level DSL nodes inside the region.
- [ ] Have the enclosing region or PU create each complete tensor result symbol
  with `no_alias=true`, declare it as an `OUTPUT|RESULT`, and let the inner
  region store into that caller-owned location.  Do not treat `OPR_REGION` or
  `OPR_BLOCK` as a tensor-valued expression.
- [ ] Require every value defined in a region and consumed outside it to appear
  in the declared output interface.  Defer conversion to SSA definitions,
  block arguments, and merge values until the DSL middle-end SSA stage.
- [ ] Have torch2whirl preserve source module hierarchy and propose BasicBlock,
  Bottleneck, identity-shortcut, and projection-shortcut contracts.
- [ ] Have the C++ CNN gatekeeper certify each proposed contract from region
  topology, operand identity, operator attributes, and tensor descriptors.
- [ ] Print the logical CNN region and its contract in `ir_b2a` while hiding
  private pragma and DSL image indices.
- [ ] Dissolve the region only after CNN gatekeeping and any whole-block VHO
  optimization; reconnect its body using the established MP-region splicing
  pattern, then promote eligible leaf operations to the common substrate.
- [ ] Add mapped-image and lowering tests for identity BasicBlock, projection
  BasicBlock, Bottleneck, malformed shortcut, mismatched result, and an older
  ResNet image without structured regions.

Exit gate: the binary VHO WHIRL image retains and exposes certified ResNet
block and shortcut semantics without changing the expression-level contracts
already used by the leaf operators.

## Concurrent Handoff Gates

| Gate | Main-agent deliverable | torch2whirl work unblocked |
| --- | --- | --- |
| G0 | Stable names, versions, arities, and attributes | Final Python mapping tables and negative tests |
| G1 | Complete: native model-input and checked external-data values | Native parameters, inputs, and safetensors references |
| G2 | Complete: ReLU, flatten, residual-add, output-logits | Native residual block and classifier boundary |
| G3 | Complete: native linear | Complete classifier tail |
| G4 | Complete: native CNN operators | Complete native ResNet graph |
| G5 | Complete on the main side: whole-graph gatekeeper and image rows | Certified `.B` generation and `ir_b2a -st` test |
| G6 | Complete: mandatory `-O0` lowering | `opencc -x whirl` certification |

No gate permits torch2whirl to inspect WN fields, symbol tables, DSL image rows,
or physical operator encodings.  A gate is complete only when its native C++
contract tests pass.

## Change Discipline

1. Keep binary WHIRL compatibility and append-only public enums.
2. Do not add tabs.
3. Do not introduce backend or CG dependencies into torch2whirl.
4. Describe binary handling only in terms of WHIRL mapped images and ELF
   sections.
5. Keep `OPR_COMMENT` projections until an explicit retirement decision.
6. Keep MLIR out of the DSL compiler-development path.
7. Update this plan after each gate, including the exact tests that passed.

## Post-ResNet Semantic Priorities

The next infrastructure work is ordered by semantic dependency rather than by
the order in which framework APIs expose values:

1. Canonicalize complete tensor descriptors in the `TY` domain. Equal frozen
   descriptors must return one `TY_IDX`; descriptor hash and equivalence must
   compare normalized contents rather than attribute counts.
2. Preserve source position in the existing statement-WN `SRCPOS` slot for
   stores, regions, pragmas, and barriers. Source names remain diagnostic
   metadata and are not a substitute for `SRCPOS`.
3. Classify apparent extra results one operator at a time. Tensor indexing is a
   tensor-access abstraction modeled after High WHIRL `OPR_ARRAY`, not a
   generic multiple-result operator.
4. Define a VHO HSSA effect interface over virtual state with `MU`-like reads
   and `CHI`-like modifications, then map it to existing WOPT alias and
   points-to infrastructure.
5. Reuse WHIRL forward/backward barriers for ordering and visibility. Keep
   barrier semantics distinct from state effects even when an operation needs
   both.
6. Represent fusion and graph capture through a logical `FUSED` contract on
   `OPR_REGION`. Treat CUDA Graph as a possible later implementation choice.
7. Keep library and kernel dispatch contracts as a dedicated design workstream
   covering capability, ABI, workspace, alias/effect, synchronization, error,
   and version contracts.

This ordering supersedes any earlier implication that a generic multi-result
facility should be brought up before tensor access, state effects, and region
interfaces have been classified.

## Current Validation Record

The G0/G1 implementation passed the following Linux/amd64 checks on
2026-07-12:

```text
osprey/common/com/tests/dsl_native_syntax_test.sh
dsl_common_add_print_test
dsl_lower_contract_test
make be
```

The syntax fixture includes the x8664, MIPS, MIPS little-endian, generic KEY,
Loongson, and baseline physical-operator layout matrix.  The backend build
emitted only pre-existing missing-return warnings outside the files changed
for G0/G1.

G2 additionally passed:

```text
dsl_builder_contract_test
dsl_lower_contract_test
ir_b2a -st simple_resnet_contract_test.B simple_resnet_contract_test.ir
make ir_b2a
make be
```

The G2 backend build emitted the same pre-existing missing-return warning in
`be/com/comp_driver.h`; the changed G2 sources were warning-free.

G3-G6 and main-agent M7 completed on 2026-07-13.  The independent C++ producer
builds an 18-value ResNet-shaped graph with a model input, eight external
parameter/state tensors, convolution, inference BatchNorm, ReLU, residual add,
max pooling, global average pooling, flatten, linear, and logits.  The
gatekeeper recomputes all operator result shapes, permits parameter placement
to differ from activation placement, rejects a deliberately corrupted
convolution result, and accepts the restored graph.  Mandatory lowering emits
one versioned runtime call and one distinct pointer-sized result PREG per value.

The builder now emits standard compile-unit and subprogram DST ownership for
its PU, and `TY::Verify()` recognizes the append-only `KIND_TENSOR` structural
contract.  The driver accepts explicit `-x whirl` as an alias for its existing
`S_B` WHIRL input path.  Descriptor and scalar initializer symbols are marked
initialized so code generation places their nonzero data outside `.bss`.

The final Linux/amd64 validation lane was:

```text
bash osprey/common/com/tests/dsl_native_syntax_test.sh
dsl_runtime_abi_contract_test
OPEN64_DSL_RESNET_TEST_ARTIFACT=complete_resnet_contract_test.B dsl_lower_contract_test
ir_b2a -st complete_resnet_contract_test.B complete_resnet_contract_test.ir
opencc -run-build=/build -x whirl -O0 -c complete_resnet_contract_test.B -o complete_resnet_contract_test.o
nm -u complete_resnet_contract_test.o
```

The retained `.B` and `-st` output expose every logical operator, typed
attribute, tensor descriptor, external-data reference, direct value edge, and
`no_alias=true` result without exposing the private physical escape tag.  The
object contains unresolved references only to the expected version-1 DSL
runtime entry points.  The build-tree `-run-build` option is validation setup,
not part of the installed compiler interface.

The only remaining M5 checkbox is the cross-producer comparison against shapes
reported by torch2whirl.  It depends on the concurrent frontend producer and
does not block the completed main-side contract.
