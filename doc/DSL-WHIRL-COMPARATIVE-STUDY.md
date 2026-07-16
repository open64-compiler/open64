# DSL WHIRL Comparative Study

## Purpose

This note compares DSL WHIRL with related compiler systems and research.  Its
purpose is not to claim that every individual mechanism is new.  It identifies
which mechanisms have established precedent, where those systems make
different tradeoffs, and which combination may constitute DSL WHIRL's unique
value.

The comparison is organized by system family.  StableHLO and VHLO are treated
as part of the MLIR/OpenXLA family, not as independent compiler
infrastructures.

## Comparison Criteria

Each system is considered along the following dimensions:

1. Domain and namespace organization.
2. Operator identity, schema, and versioning.
3. Type, shape, layout, and effect completeness.
4. Rules for sharing or promoting operators across domains.
5. Preservation of domain semantics during optimization.
6. Artifact compatibility and independent producer/consumer operation.
7. Verification before lowering.
8. Region, control-flow, source-position, and diagnostic support.
9. Architecture-independent optimization and compiler-library codesign.
10. Cost of adding a new domain and maintaining its contracts.

## System Families

### MLIR And OpenXLA

MLIR provides extensible dialects, operations, types, attributes, regions,
rewrite patterns, type conversion, and legality-driven dialect conversion.
StableHLO is an MLIR dialect that specifies a portable high-level tensor
operation set.  VHLO is its add-only, versioned compatibility dialect used by
the StableHLO portable-artifact path.  CHLO contains higher-level operations
that must be legalized to StableHLO to receive StableHLO compatibility
guarantees.

Pros:

1. Strong infrastructure for defining and composing dialects.
2. Declarative operation definitions and generated verification support.
3. Progressive lowering can preserve high-level semantics for multiple levels.
4. StableHLO provides a written operation specification and broad framework
   interoperability.
5. VHLO establishes strong precedent for append-only versions of operations,
   types, and attributes.
6. Conversion targets can express static or dynamic legality.

Cons or different tradeoffs:

1. Legal dialect conversion does not by itself prove that two independently
   designed domain operators have identical semantics.
2. StableHLO compatibility applies to artifacts produced through its defined
   compatibility APIs, not arbitrary MLIR text or bytecode containing other
   dialects.
3. VHLO captures versions for artifact compatibility, but it is not a general
   registry explaining why a domain operation may be promoted into a shared
   substrate.
4. CHLO demonstrates that higher-level convenience semantics may sit outside
   the stable compatibility boundary until explicitly legalized.
5. The flexibility of independently evolving dialects can create duplicated or
   nearly equivalent operators unless promotion policy is governed separately.

Lesson for DSL WHIRL:

Adopt append-only logical schemas and explicit legality checks.  Add a stronger
promotion contract that records exact source and destination versions,
retained domain checks, and the evidence that semantics are equivalent.

### ONNX

ONNX defines an extensible computation graph with operator sets identified by
domain and version.  An operator is identified by its domain, operation name,
and introduction version.  Published stable operator syntax and semantics are
immutable; a semantic change produces a new operator version and operator-set
version.

Pros:

1. Clear separation among IR version, operator-set version, and model version.
2. Domain-qualified operator sets support standard and vendor extensions.
3. Operator schemas are implementation-independent contracts.
4. The checker and version-conversion infrastructure make compatibility a
   visible ecosystem concern.
5. The model explicitly declares the operator sets required by its graph.

Cons or different tradeoffs:

1. ONNX is principally an interchange graph rather than a complete optimizing
   compiler IR spanning source constructs through machine code.
2. Operator-set conversion describes compatibility between published schemas;
   it does not establish a general domain-to-common promotion proof.
3. Domain-specific source structure, compiler ownership, lowering state, and
   optimization annotations are not its primary abstraction.
4. Implementations commonly translate ONNX into another internal IR before
   substantial optimization.

Lesson for DSL WHIRL:

Use a similarly precise logical identity such as
`(domain, operator, version)`, while retaining WHIRL's compiler-level symbols,
types, regions, source positions, optimization mappings, and continuous
lowering pipeline.

### Relay And TVM

Relay is a typed functional IR for machine-learning programs.  Its operator
type relations constrain operand and result types and shapes.  TVM combines
graph-level transformations such as fusion with tensor and target-specific
lowering.

Pros:

1. Type relations make operator type and shape requirements executable.
2. Functional representation supports analysis and graph transformation.
3. TVM demonstrates productive separation between graph optimization and
   target-level tensor implementation.
4. Operator fusion and hardware mapping are first-class optimization goals.

Cons or different tradeoffs:

1. Persistent, append-only per-operator compatibility is not Relay's central
   contract.
2. Domain wrapper retention and promotion evidence are not its primary
   organizing mechanism.
3. Interoperation with an established general-purpose compiler IR generally
   requires another lowering boundary.
4. Source-language regions, traditional symbol tables, alias information, and
   mature interprocedural infrastructure differ from Open64's model.

Lesson for DSL WHIRL:

Make shape and type relations part of the gatekeeper contract, and preserve
high-level fusion opportunities, while integrating them with canonical WHIRL
types, symbols, regions, Preopt, IPA, and later lowering.

### Translation Validation With Alive2

Alive2 uses bounded translation validation to check refinement between LLVM IR
before and after a transformation.

Pros:

1. It checks an actual transformation rather than trusting the rewrite merely
   because it matched.
2. Counterexamples make incorrect transformations diagnosable.
3. It provides strong evidence that verification can be deployed in a real
   optimizing compiler workflow.

Cons or limits for this project:

1. Alive2 models LLVM's lower-level scalar and memory semantics, not the full
   tensor, shape, layout, quantization, sharding, or numerical contracts needed
   here.
2. Bounded proof does not replace schema review, domain legality checks, or
   cross-version artifact testing.
3. Floating-point approximations and implementation-defined numerical accuracy
   require an explicitly chosen equivalence relation.

Lesson for DSL WHIRL:

Promotion records should eventually support machine-checkable proof
obligations.  Initial stages can use gatekeeper checks, reference evaluation,
positive and negative tests, and differential execution before attempting a
tensor-aware translation validator.

### Equality Saturation With E-Graphs

Equality saturation represents many equivalent expressions together and
selects an implementation according to a cost model.

Pros:

1. It avoids committing prematurely to one rewrite sequence.
2. It is well suited to algebraic exploration, fusion alternatives, and
   compiler-library selection.
3. Extensible analyses can carry facts through equivalence classes.

Cons or limits for this project:

1. An e-graph assumes its rewrite rules are sound; it does not establish
   operator-contract equivalence by itself.
2. Effects, aliasing, regions, dynamic shapes, and floating-point behavior make
   unrestricted equivalence rules unsafe.
3. Equality saturation is an optimization technique, not an artifact
   compatibility or operator-governance system.

Lesson for DSL WHIRL:

Use equality saturation only after logical operators, versions, tensor
descriptors, effects, and promotion preconditions are explicit.  Promotion
governance decides which equalities are legal; an e-graph may then explore
their profitable combinations.

## Comparative Summary

| Property | MLIR/OpenXLA | ONNX | Relay/TVM | DSL WHIRL target |
| --- | --- | --- | --- | --- |
| Multiple domains | Dialects | Operator-set domains | Extensible operators | Domain registries plus common substrate |
| Stable op versions | VHLO versions StableHLO elements | Domain/op/version schemas | Not the primary contract | Append-only logical operator versions |
| Cross-domain promotion | Conversion and rewrite legality | Version conversion, not promotion | Operator rewrites | Versioned promotion registry with retained checks |
| Type and shape checking | Types, verifiers, inference | Schemas and checker | Type relations | Canonical tensor TY plus gatekeeper relations |
| Domain wrapper retention | Possible, pipeline-specific | Not a central mechanism | Not a central mechanism | Required until domain obligations are discharged |
| Persistent artifact | MLIR bytecode and StableHLO portable artifacts | Protocol-buffer model | Framework-specific | Existing ELF mapped WHIRL image plus additive tables |
| Existing native compiler continuum | Lowers through dialects to LLVM/targets | Usually imported elsewhere | Own compiler stack | VHO through Open64 middle end and backend |
| Source regions and symbols | Regions and symbols | Graph-oriented | Functional graph | WHIRL REGION, RID, ST, TY, SPOS, maps, and IPA |
| Transformation proof | Legality and verifiers | Checker and conversion tests | Type checking and tests | Gatekeeper now; proof obligations and differential validation planned |

## Proposed Unique Value

No individual item below should be claimed as novel without further literature
review.  The prospective contribution is their integration in one compiler
contract:

1. **Versioned, verified operator promotion.** A domain operation is promoted
   only when exact source and target versions agree on operands, results,
   tensor semantics, attributes, numerical behavior, effects, aliasing, and
   failure behavior.
2. **Promotion distinct from lowering.** Exact promotion, constrained wrapper
   promotion, decomposition, and target lowering are different recorded
   decisions.
3. **Retained domain obligations.** A shared implementation does not erase CNN,
   Transformer, layout, mask, checkpoint, or runtime-state checks.  Required
   wrappers remain visible until the domain gatekeeper discharges them.
4. **Logical operators decoupled from physical opcode scarcity.** The private
   `OPR_DSL` escape tag preserves the existing WN opcode field while compiler
   APIs, diagnostics, and traces expose only logical operators such as
   `OPR_DSLADD`.
5. **Compatibility inside a mature compiler image.** DSL tables extend the
   existing ELF mapped-image WHIRL framework while preserving old-image
   readability and the established symbol, type, region, source, mapping,
   interprocedural, and lowering infrastructure.
6. **Canonical semantic tensor identity.** Deduplicated TensorDescriptorIR
   state belongs to the TY domain and is separated from opcode attributes and
   compiler metadata.
7. **Independent frontend boundary.** Python captures source semantics and
   produces a binary WHIRL artifact; the Open64 middle end reopens that artifact
   without a Python runtime dependency.
8. **High-level optimization before hardware commitment.** Fusion,
   quantization, architecture-independent parallelization, Preopt, IPA, and
   compiler-library codesign operate while domain and tensor semantics remain
   available.

The concise research characterization is:

> DSL WHIRL combines append-only operator contracts with domain-aware,
> version-specific promotion inside a backward-compatible, continuously
> lowerable production compiler IR.

## Costs And Risks

The study must present the disadvantages of the DSL WHIRL approach as clearly
as its benefits:

1. Retrofitting a mature fixed-layout IR requires stricter compatibility
   engineering than starting a new dialect infrastructure.
2. Logical operator registries, promotion records, gatekeepers, printers, and
   mapped-image tables create maintenance obligations.
3. Domain wrappers can proliferate unless ownership and retirement rules are
   enforced.
4. Tensor-aware semantic equivalence is difficult, especially for floating
   point, quantization, dynamic shapes, state, and target-dependent accuracy.
5. Compatibility with an old reader does not imply that the old compiler can
   execute an unknown logical operation; rejection and lowering boundaries
   must be deterministic.
6. MLIR, StableHLO, ONNX, and TVM have larger existing ML ecosystems and more
   external tooling.
7. Extending VHO Preopt, IPA, alias analysis, and optimization control to DSL
   operators is substantial engineering work.

## Research Questions And Evaluation

The following questions can turn the design into a defensible comparative
evaluation:

1. Does the promotion registry prevent semantic loss that legality-only
   rewriting would admit?
2. How many CNN and Transformer operators become exact common operators,
   constrained wrappers, decompositions, or domain-only operators?
3. Can old binary WHIRL inputs and existing LNO REGION inputs remain unchanged
   while new DSL artifacts roundtrip through `ir_b2a -st -src` and `ir_a2b`?
4. Does retaining regions, source positions, and domain wrappers improve
   diagnostics and optimization decisions on ResNet and Llama 2?
5. What compile-time and image-size cost is introduced by logical DSL tables,
   tensor descriptors, RID maps, and promotion records?
6. Can reference evaluation or differential execution validate promoted
   tensor operators across versions, dtypes, shapes, and edge cases?
7. Does VHO compiler-library codesign produce competitive kernel selection
   without losing the ability to retarget the same binary WHIRL input?

Required evaluation artifacts should include positive promotion cases,
deliberate near-match rejection cases, version-mismatch cases, old-reader
compatibility tests, preserved `ir_b2a -st -src` traces, and end-to-end ResNet
and Llama 2 results.

## Primary References

1. [MLIR: Scaling Compiler Infrastructure for Domain Specific
   Computation](https://research.google/pubs/mlir-scaling-compiler-infrastructure-for-domain-specific-computation/),
   CGO 2021.
2. [MLIR Dialect Conversion](https://mlir.llvm.org/docs/DialectConversion/).
3. [StableHLO Specification](https://openxla.org/stablehlo/spec).
4. [VHLO Dialect](https://openxla.org/stablehlo/vhlo).
5. [StableHLO Compatibility](https://openxla.org/stablehlo/compatibility).
6. [ONNX IR Specification](https://onnx.ai/onnx/repo-docs/IR.html).
7. [ONNX Versioning](https://onnx.ai/onnx/repo-docs/Versioning.html).
8. [Relay: A High-Level Compiler for Deep
   Learning](https://arxiv.org/abs/1904.08368).
9. [TVM: An Automated End-to-End Optimizing Compiler for Deep
   Learning](https://arxiv.org/abs/1802.04799).
10. [Alive2: Bounded Translation Validation for
    LLVM](https://web.ist.utl.pt/nuno.lopes/pubs.php?id=alive2-pldi21), PLDI
    2021.
11. [egg: Fast and Extensible Equality
    Saturation](https://popl21.sigplan.org/details/POPL-2021-research-papers/23/egg-Fast-and-Extensible-Equality-Saturation),
    POPL 2021.
