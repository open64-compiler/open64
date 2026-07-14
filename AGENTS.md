# Codex Project Guide

This repository is Open64 with active work around very-high-level WHIRL, DSL
metadata, tensor descriptors, and a future Python frontend ingestion path.
Use this file as the compact default context. Pull the larger design documents
only when the current task needs detail.

## High-Level Direction

1. Preserve existing Open64 and WHIRL compatibility unless a task explicitly
   requires a versioned IR change.
2. Binary WHIRL file backward compatibility is a standing requirement. Do not
   change WHIRL binary image layout, ELF section contracts, opcode encodings,
   type-kind encodings, or reader/writer behavior unless the task explicitly
   calls for a versioned IR change with a documented migration path.
3. Binary WHIRL uses Open64's mapped-image and ELF framework. The binary writer
   finalizes table and tree images into their ELF sections, and the reader maps
   those images back into the compiler address space. Do not describe or
   implement this as object-stream encoding/decoding or format conversion.
4. Treat `WHIRL.pdf` as the normative architectural baseline for WHIRL
   semantics. Before changing DSL node representation, operators, opcode
   encoding, kid layout, types, mappings, printing, or mapped-image finalization,
   review the corresponding definitions and design objectives in `WHIRL.pdf`.
5. Design DSL WHIRL as an additive extension of the existing WHIRL model.
   Preserve established rules such as strict tree structure, direct operand
   kids, operator/result/descriptor semantic completeness, continuous
   lowering, mapping-table annotations, and standard binary/ASCII inspection.
   Any intentional departure must be explicitly justified, versioned when
   necessary, and covered by compatibility tests.
6. Maintain enough architectural detail for the DSL extensions to become an
   appendix to `WHIRL.pdf`. The appendix should distinguish normative semantic
   contracts from transitional carriers and historical ABI details, and map
   each extension to its operator definition, node layout, allowed WHIRL
   levels, type rules, verifier requirements, lowering path, binary format,
   and `ir_b2a` representation.
7. Do not copy historical physical sizes or ABI assumptions from `WHIRL.pdf`
   without checking the current Open64 implementation. Use the document for
   semantic intent and verify concrete layouts against the current source,
   target ABI, binary reader, and binary writer.
8. Treat Python as a source-language frontend and ingestion layer only. The
   Open64 middle end must consume a binary WHIRL artifact without depending on
   the Python interpreter.
9. Keep WHIRL table construction, symbol/type creation, TensorDescriptorIR
   attachment, opcode attributes, contracts, compiler metadata, mapped image
   finalization, and binary IR compatibility in C++.
10. Make ingestion emit first-class DSL/common operators or stable DSL markers,
   not early intrinsic-call placeholders, runtime calls, or target kernels.
11. Run a gatekeeper/verifier before lowering so malformed very-high-level WHIRL
   does not silently reach downstream optimization passes.
12. Do not treat the MPL frontend path as a Very High Level WHIRL ingestion
   path. MPL is incomplete reference code for porting another IR roughly
   equivalent to Middle Level WHIRL; it has no equivalent feature set for Very
   High Level WHIRL. `mpl2whirl` remains a valid reference for baseline Open64
   symbol/type tables, scope and PU ownership, mapped-image finalization, and
   binary reader/writer mechanics unless a specific incompatibility is
   demonstrated and documented.
13. Follow the coding guidelines below for every touched file.

## Coding Guidelines

1. Do not introduce tab characters in any file touched in this Open64 project.
2. Use spaces for indentation and alignment.
3. Makefiles may use literal tabs only where required by standard make recipe
   conventions.
4. When editing existing Open64 C/C++ code, match the surrounding indentation
   style with spaces rather than reformatting unrelated code.

## Driver And Phase Option Convention

1. Preserve Open64's command-line option propagation model. The driver passes
   the user's complete option set through the compiler pipeline.
2. Each compiler phase selects and processes only the options applicable to
   that phase and silently ignores options owned by other phases.
3. A phase must not reject a command merely because unrelated Open64 options
   are present. New phases, including `torch2whirl`, must follow this convention
   instead of requiring the driver to remove every unrelated option.
4. For Python frontend option processing, use `clang2whirl` or the existing C
   frontend as the reference implementation pattern for receiving the complete
   option set, selecting frontend-owned options, and silently ignoring options
   owned by other phases.

## Current Architecture Target

```text
Python model / DSL
  -> torch.export / FX / DSC capture graph
  -> open64_dsc.WhirlExportInterpreter
  -> open64_dsc._whirl native extension
  -> libopen64_whirl_builder
  -> binary very-high-level WHIRL
  -> opencc -x whirl
```

The Python package should coordinate capture and graph traversal. The native
builder should own all WHIRL-specific construction.

## DSL And Tensor Rules

1. DSL frontends may attach structured metadata with `OPR_COMMENT` nodes using
   the reserved prefix `__WHIRL_DSL__:<domain>:<feature>:<payload>`.
2. Older tools should still see legal WHIRL. DSL-aware passes must validate and
   consume required markers before WOPT, LNO, CG, and other canonical-WHIRL
   phases.
3. Tensor identity currently stages through an existing `TY_IDX` carrier plus a
   tensor extension side table. Do not force a new WHIRL operator enum, opcode
   enum, or binary type-kind change unless the stage explicitly calls for it.
4. TensorDescriptorIR carries semantic tensor value state: element type, rank,
   shape, layout, strides, traits, lineage, placement, and related facts.
5. Compiler metadata carries source context, diagnostics, pass ownership,
   lowering hints, and profiling data. Do not mix compiler metadata into tensor
   type equivalence unless a later schema explicitly promotes it.
6. Avoid nested STL containers inside WHIRL-style table records that may later
   need to be dumped, reloaded, compared, or persisted in a binary IR image.
7. Domain operators must remain domain-visible until domain gatekeeper checks
   complete. Do not hide CNN/Transformer ABI, layout, padding, stride, mask,
   head-layout, or similar legality checks through premature promotion or
   lowering.
8. Treat physical `OPR_DSL` as a private WN escape tag. DSL-aware compiler
   code, diagnostics, ASCII dumps, and traces must use logical operator APIs
   and names such as `OPR_DSLADD`; they must not expose or manually decode the
   physical escape tag or its internal record index.

## IR Compatibility And Staging Rules

1. Keep `ir_a2b` and `ir_b2a` as compatibility gates. Any staged IR change must
   preserve behavior for existing WHIRL, or update those tools and matching
   tests in the same staged change.
2. The binary WHIRL artifact is the frontend boundary. Avoid special in-memory
   bypasses from Python into the compiler pipeline; artifacts should be
   inspectable with `ir_b2a -st` before combined driver integration.
3. Run gatekeeper verification before canonical lowering. Missing tensor
   attributes, invalid operand compatibility, unknown contracts, unsupported
   opcodes, and domain legality failures should be diagnosed before WOPT, LNO,
   or CG.
4. Public DSL opcode enum values, string names, categories, levels, traits,
   shape rules, effect models, and promotion states are compatibility contracts
   once they appear in IR files, tests, dumps, or registries.
5. TensorDescriptorIR carries semantic tensor value state. Compiler metadata
   carries source context, diagnostics, pass ownership, lowering hints,
   profiling, and source names. Compiler metadata must not affect tensor type
   equivalence.
6. Do not add `KIND_TENSOR` or new binary tensor descriptor sections casually.
   First-class tensor type or tensor descriptor binary-section changes must land
   with printer, `ir_b2a -st`, binary reader, binary writer, ASCII
   reader/printer plan, verifier, and fallback behavior.
7. `torch2whirl`, Python bindings, and the native frontend bridge must not
   depend on backend code generation. Backend, runtime, and kernel lowering
   happen after gatekeeper verification.

## Near-Term Coding Priorities

1. Stabilize native DSL/common infrastructure before adding Python package
   code.
2. Add or maintain native tests under `osprey/common/com/tests` for tensor
   types, tensor constants, `common.add`, `common.matmul`, and `ir_b2a -st`
   visibility.
3. Keep the first builder API narrow. Python-facing bindings should pass opaque
   handles and values; C++ should create real WHIRL objects.
4. Use existing mapped-image / ELF WHIRL mechanisms for binary artifacts before
   inventing any new file format.
5. Defer combined `opencc -frontend=torch2whirl ...` driver integration until
   the binary WHIRL artifact boundary is stable and inspectable.

## Debugging And Triage

When investigating runtime or optimization failures, establish a baseline and
add one condition at a time:

1. First make `-O0` work, then compare `-O1`, `-O2`, `-O3`, and finally `-ipa`.
2. If `-O0` works but `-O1` fails, suspect CG.
3. If `-O1` works but `-O2` fails, suspect WOPT.
4. If `-O2` works but `-O3` fails, suspect LNO.
5. If `-O*` works but `-O* -ipa` fails, suspect IPA.
6. After identifying a component, reduce by file, then procedure, then specific
   optimization. Use binary search where possible.

## Reference Docs

Load these only when the task needs the detail:

1. `Open64_Python_FE_Plan.md` - compact roadmap for Python ingestion and native
   WHIRL builder work.
2. `doc/WHIRL-DSL-INFRASTRUCTURE.md` - DSL marker, tensor extension, and
   lowering infrastructure details.
3. `doc/VHO-DSL-OPTIMIZATION-PLAN.md` - optional architecture-independent VHO
   optimization, parallelization, Preopt/IPA, and compiler-library codesign.
4. `doc/HOW-TO-TRIAGE-RUNTIME-FAILURE-OPEN64.md` - runtime failure and
   optimization triage method.
5. `doc/Open64_Domain_Specific_Compiler_IR_Design.md` - large staged design;
   search within it instead of loading it whole.
6. `imported_docs/DSC_Master_Design_Doc_v0.9_chapter_7.md` - Python DSL
   ingestion architecture background.
7. `osprey/clang2whirl/README.md` and
   `osprey/clang2whirl/NAMING_CONVENTION.md` - use only for clang2whirl work.
8. `WHIRL.pdf` - normative WHIRL architecture and semantic baseline. Consult
   the relevant operator, node-layout, level, type, mapping, and ASCII-format
   sections before designing DSL extensions. The planned DSL appendix must
   remain consistent with this document or explicitly document extensions.

Treat MPL-related sources as reference material only. Do not use them as
evidence for the Very High Level WHIRL DSL frontend or Python ingestion design.

Most `*.md` files under GCC `config/` directories are machine-description
inputs, not general project guidance.
