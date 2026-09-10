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

## Open64 Design Continuity

1. When new DSL infrastructure overlaps an established Open64 service, first
   study and reuse that service's intended extension points, controls, data
   structures, and tests. Preparation and postprocessing for DSL semantics are
   acceptable; an independent parallel implementation requires a documented
   incompatibility and explicit design review.
2. DSL expression simplification must use the traditional Open64
   `wn_simp_code.h` rule engine whenever the traditional rule has equivalent
   semantics. Follow the established physical-WN protocol: prepare a
   stack-local WN view for the simplifier, discard that temporary input after
   the call, and retain only simplifier results allocated in the normal WN
   memory-pool space. Tensor descriptor, effect, ownership, and logical-opcode
   checks belong in preparation; DSL result symbols, metadata, lineage, and
   mapped-image relationships belong in postprocessing.
3. Do not duplicate traditional constant folding, identity, reassociation,
   cancellation, or factorization rules in a separate DSL rule engine. Add a
   DSL-only rule only when no traditional rule has equivalent semantics, and
   document and test that distinction.
4. Revisit this continuity principle during design reviews for new DSL
   services. The concrete reuse mechanism may differ by subsystem, but the
   review must identify the existing Open64 design being preserved or explain
   why it cannot be reused.
5. When WOPT admits unlowered DSL expressions, decode the physical `OPR_DSL`
   boundary representation into a first-class logical CODEREP identity.
   CODEREP hashing, equality, printing, effects, and simplification must include
   the DSL operator, version, canonical attributes, and TensorDescriptorIR
   identity. Continue using WOPT's existing `CODEREP` instantiation of
   `wn_simp_code.h`; do not add a parallel WOPT simplifier.

## Backend Shared-Library Dependencies

1. Do not add a new library dependency to `be.so` without explicit design and
   build approval. This includes direct linkage and source-level references
   that create new defined or undefined symbols in the shared library.
2. Before approving a dependency, identify every consumer of `be.so`, including
   `lw_inline`, backend plugins, and standalone tools, and prove that each
   consumer remains link-closed on every supported build configuration.
3. Prefer an existing Open64 service or an already compatible header-only
   facility when structured parsing or another utility is needed in a backend
   pass. Do not introduce ad hoc parsing to avoid the dependency review.
4. Validation for an approved dependency change must rebuild `be.so` and its
   shared consumers, inspect their defined and undefined symbols, and document
   platform, static/shared, licensing, version, and binary-distribution impact.
5. Frontend-only libraries and interfaces, including `DSL_Builder_*`, must not
   leak into `be.so`. For example, an FHE backend may parse an authenticated
   JSON manifest with the repository's compatible header-only RapidJSON
   facility, but must not add JsonCpp linkage to `be.so` without approval.

## Coding Guidelines

1. Do not introduce tab characters in any file touched in this Open64 project.
2. Use spaces for indentation and alignment.
3. Makefiles may use literal tabs only where required by standard make recipe
   conventions.
4. When editing existing Open64 C/C++ code, match the surrounding indentation
   style with spaces rather than reformatting unrelated code.
5. Builder interfaces that create a symbol must make every reasonable attempt
   to set its declaration source position with `Set_ST_Srcpos()`. When a
   builder creates both a defining WN and a result ST, propagate the same
   complete source position, including file, line, and column, to both objects.

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
   inspectable with `ir_b2a -st -src` before combined driver integration.
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
   with printer, `ir_b2a -st -src`, binary reader, binary writer, ASCII
   reader/printer plan, verifier, and fallback behavior.
7. `torch2whirl`, Python bindings, and the native frontend bridge must not
   depend on backend code generation. Backend, runtime, and kernel lowering
   happen after gatekeeper verification.

## Domain Ingestion Staging

Use this workflow when adding a model family or a new domain to `openpy`:

1. Add a small, deterministic, dependency-light source model fixture.
2. Capture the real frontend graph and produce a reviewable operator census.
3. Classify every captured operation as a common substrate operation, domain
   expression, region contract, state/effect, compiler metadata, constant, or
   explicit unsupported case.
4. Review and publish stable native names, versions, operands, attributes,
   descriptor rules, effects, gatekeeper checks, and lowering ownership in the
   WHIRL infrastructure plan.
5. Implement the native builder, mapped-image, reader/writer, logical printer,
   gatekeeper, and lowering support required by the published contract.
6. Migrate the frontend from census/mock handling to opaque native APIs. The
   frontend must not learn WN layout or private physical DSL encodings.
7. Certify the binary artifact, external data, `ir_b2a -st -src` trace, and
   `openpy -O0` path across a process boundary.

Do not allocate DSL opcodes or broaden native WHIRL contracts solely from an
expected model architecture. First capture a representative source model and
review its semantic operator census. Conversely, do not force a captured model
into existing operators when doing so would erase domain semantics. Frontend
capture discovers the requirements; common/com review remains authoritative
for native contracts and binary WHIRL behavior.

Maintain a separate frontend ingestion plan for each substantial domain or
model family and link it to `doc/WHIRL-DSL-INFRASTRUCTURE.md`. The frontend plan
owns capture, mapping, artifacts, and diagnostics; the infrastructure plan owns
native representation, compatibility, verification, inspection, and lowering.

## Reviewable Test Artifacts

1. Human review of compiler artifacts is part of the development and
   validation process. Tests that produce meaningful WHIRL evidence should
   retain the binary `.B` file, `ir_b2a -st -src` output, requested phase `.t`
   traces, side payloads, and relevant diagnostics after the test exits.
2. Clean the designated artifact directory at the start of the next run, not
   at the end of the current run. A completed run must leave its evidence
   available until it is replaced by a later run.
3. Docker tests must write reviewable artifacts through an explicit host bind
   mount. Do not leave the only copy in a container filesystem or a container
   temporary directory that disappears when Docker exits.
4. Keep artifact families in clearly named per-test or per-stage directories
   so one smoke test does not erase another test's evidence.
5. Failed runs must not leave partial output with the name of a valid `.B`
   artifact. Preserve failure logs and diagnostics when useful, while keeping
   artifact publication atomic.
6. At the end of validation, report the absolute host paths of retained
   artifacts so reviewers can inspect them directly.
7. Generated review artifacts are normally local build evidence. Do not add
   them to Git unless the task explicitly requests checked-in golden files or
   review fixtures.
8. When completing a task that produces or validates a compiler trace, include
   a directly viewable link to the retained trace in the final report. Show a
   short representative excerpt or summarize the concrete evidence it contains;
   do not report trace-based validation as complete using only a pass/fail
   statement.
9. Use both `-st` and `-src` whenever running `ir_b2a` for validation or human
   review. Preserve or mount the original source file at the pathname recorded
   in the binary WHIRL DST so `-src` can interleave source statements with the
   IR. If the active `ir_b2a` build does not yet support `-src`, treat that as a
   tooling gap to fix; do not silently omit source cross-reference evidence.
10. The `ir_b2a` output must use the input `.B` file's stem, for example
   `ir_b2a -st -src resnet.B resnet.T`. On case-insensitive filesystems where
   `resnet.T` collides with a driver-produced `resnet.t`, preserve the phase
   trace under a descriptive non-colliding name such as `resnet.vho.t` before
   producing `resnet.T`.

## Near-Term Coding Priorities

1. Stabilize native DSL/common infrastructure before adding Python package
   code.
2. Add or maintain native tests under `osprey/common/com/tests` for tensor
   types, tensor constants, `common.add`, `common.matmul`, and
   `ir_b2a -st -src`
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
7. Treat `-OPT:wn_simplify` and its abbreviation `-OPT:wn_simp` as the master
   WHIRL construction-time simplifier control. When triaging a simplifier
   regression, preserve separate `.B` files with simplification enabled and
   disabled, produce matching `ir_b2a -st -src` traces, and compare the
   resulting WHIRL. New DSL construction-time simplification must honor
   `Enable_WN_Simp`; a DSL-specific control may further restrict a stage but
   must not override the master switch.

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
5. `doc/TORCH2WHIRL-WHIRL-DSL-API-CONTRACT.md` - torch2whirl to common/com
   DSL builder API inventory and subagent/main-agent API creation protocol.
6. `doc/Open64_Domain_Specific_Compiler_IR_Design.md` - large staged design;
   search within it instead of loading it whole.
7. `imported_docs/DSC_Master_Design_Doc_v0.9_chapter_7.md` - Python DSL
   ingestion architecture background.
8. `osprey/clang2whirl/README.md` and
   `osprey/clang2whirl/NAMING_CONVENTION.md` - use only for clang2whirl work.
9. `WHIRL.pdf` - normative WHIRL architecture and semantic baseline. Consult
   the relevant operator, node-layout, level, type, mapping, and ASCII-format
   sections before designing DSL extensions. The planned DSL appendix must
   remain consistent with this document or explicitly document extensions.

Treat MPL-related sources as reference material only. Do not use them as
evidence for the Very High Level WHIRL DSL frontend or Python ingestion design.

Most `*.md` files under GCC `config/` directories are machine-description
inputs, not general project guidance.
