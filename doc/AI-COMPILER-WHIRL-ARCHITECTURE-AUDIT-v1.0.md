# AI compiler WHIRL architecture documentation audit

Version v1.0 | 2026-11-12 | Documentation correction completed

The maintained design and implementation-plan set now states its architecture
through WHIRL opcode/version contracts, TensorDescriptorIR/tensor traits and
phase-owned Open64 analysis and verifier services. The governing
[native contract](AI-COMPILER-WHIRL-OPCODE-AND-TENSOR-TRAIT-CONTRACT-v1.0.md)
defines the representation, phase-exit legality and scheduling boundaries.
This audit changes documentation only and adds no compiler implementation,
API, option, dependency or GPU qualification claim.

## Scope and method

The audit screened all 295 existing Markdown/text/reStructuredText documents
under doc and imported_docs in the isolated design checkout at baseline
31073ef7. It reviewed matched paragraphs in context and inspected the native
opcode/fusibility, tensor-trait, High snapshot and detached search contracts.
A supplemental repository-wide documentation scan found two frontend-plan
references. Six standalone historical Markdown outputs and eleven retained
Word files were also screened; the Word scan included body/table paragraphs
and other word/*.xml parts. No access failure occurred for these files.

Search covered MLIR labels, operation-interface/trait language and common
framework API names, plus native-looking conversion-target, scheduling-service
and fusibility-trait terminology. Classification distinguished architectural
prescriptions, unnecessary exclusions, native static opcode properties and
external comparisons. This is a documentation audit, not a dependency or
runtime certification of the entire compiler.

## Findings and corrections

| Finding | Correction and coverage |
| --- | --- |
| Repeated exclusion sentences | Replaced with affirmative native representation and legality ownership in all 32 search implementation plans v1.0-v1.31, all 11 optimization plans v0.6-v0.16, the reuse/quantized-cache/Q1 documents and relevant design revisions |
| Conversion-target-style terminology | All six search methodologies v1.0-v1.5 and design v0.7/v0.8 now specify native phase exit verifiers; unknown obligations require a verified guard/fallback or refusal |
| Generic scheduling API ambiguity | The governing contract separates immutable snapshot capture, detached estimation, selected transactional application, analysis invalidation/rebuild and qualification; interfaces are owning-phase service APIs |
| Fusibility described as operator traits | AIO-5 and the twelve living/embedded implementation-plan copies now say exact opcode/version properties; actual tensor properties remain in descriptors/traits and site legality remains phase-owned |
| Prescriptive MLIR-style staged lowering | Five design revisions now describe High/Mid/Low WHIRL lowering directly and place empirical qualification outside estimation-only search |
| Low mapping comparison inside requirements | Removed TTGIR comparisons from the affected Q1 and methodology summary requirements; explicit external comparison tables remain |
| Frontend ingestion exclusion | MLA-INGESTION-PLAN now names existing WHIRL opcode and tensor-descriptor/trait inputs and phase-owned backend decisions |
| Shared DSL direction and constant storage | Native representation and typed-record requirements replace unnecessary framework-relative wording; technical comparison sections remain |

The pre-edit passages did not establish a required MLIR operation-interface
class hierarchy. Most explicitly excluded adoption, but their repetition and
framework-relative wording obscured the intended native contract. The source
fusibility table is an existing opcode/version metadata lookup, not evidence
that per-operation behavior is encoded in tensor types. Its API names and
source identifiers are unchanged.

There are 65 edited Markdown documents and two new Markdown documents: this
audit and the governing contract. Dated correction notes make the terminology
edits to embedded older revisions explicit. Historical milestone completion,
acceptance, numerical permissions and implementation limitations retain their
original scope. The H20 plan and memory design corrected in 31073ef7 remain
unchanged in this audit.

## Retained comparisons and originals

External descriptions remain in DSL-WHIRL-COMPARATIVE-STUDY and
TENSOR-DSL-API-COMPARISON, in the design series' compiler-comparison sections,
and in the tensor-constant plan's comparative evidence. These describe other
systems or cite their documentation. They are not requirements to implement
MLIR interfaces, dialects, type conversion or rewriting machinery in Open64.
The LLAMA2 ingestion plan's dependency prohibition is also retained: it states
a dependency boundary rather than an adoption assumption.

Six standalone historical Markdown outputs are retained unchanged:

- AI-COMPILER-WHIRL-ARCHITECTURE-AMENDMENT-v1.0.md
- AI-COMPILER-OPTIMIZATION-IMPLEMENTATION-PLAN-v0.4.md
- AI-COMPILER-OPTIMIZATION-IMPLEMENTATION-PLAN-v0.5.md
- AI-COMPILER-PHYSICAL-NONIDEALITIES-AND-FEEDBACK-v0.1.md
- AI_compiler_optimization_design_v0.4.md
- AI_compiler_optimization_design_v0.5.md

These live at the task's outputs root. Their architectural interpretation is
governed by the new native contract; they are historical snapshots, not newly
corrected editions. In particular, the v0.5 plan and feedback appendix contain
older exclusion sentences, and design v0.4/v0.5 contains the former staged-
lowering prescription. Immutable packaged source copies, retained receipts,
review PR worktrees and the older dirty compiler worktree were not rewritten.

The following Word originals were accessible and are retained unchanged.
The new contract governs current design work; this audit does not publish a
replacement Word master or claim these binaries were edited or rendered.

| Retained file | Relevant result |
| --- | --- |
| `doc/DSC_FHE_Compiler_Architecture_and_Integration_Plan_v0.10.docx` | No search hits |
| `doc/DSC_FHE_Compiler_Architecture_and_Integration_Plan_v0.9.docx` | No search hits |
| `doc/AI_compiler_optimization_design_v0.3.docx` | Earlier staged-lowering recommendation and external comparisons; apply the native contract |
| `imported_docs/DSC_Master_Design_Doc_V0.17.docx` | External ecosystem descriptions; no operation-interface adoption requirement found |
| `outputs/Open64_MLA_AOT_Implementation_Plan_v0.2.docx` | No search hits |
| `outputs/Open64_MLA_Multiversioning_Design_Specification_v0.2.docx` | MLA-016 contains an older exclusion sentence; apply the native phase/trait contract |
| `outputs/Open64_MLA_Multiversioning_Design_Specification_v0.1.docx` | MLA-016 contains an older exclusion sentence; apply the native phase/trait contract |
| `outputs/AI_compiler_optimization_design_v0.4.docx` | Earlier staged-lowering recommendation and external comparisons; apply the native contract |
| `outputs/AI_compiler_optimization_design_v0.5.docx` | Earlier staged-lowering recommendation and schedule-application exclusion; apply the native contract |
| `outputs/DSC_Master_Design_Doc_V0.19.docx` | Schedule-application paragraph contains an older exclusion; other hits are external ecosystem discussion |
| `outputs/DSC_Master_Design_Doc_V0.18.docx` | External ecosystem descriptions; no operation-interface adoption requirement found |

## Validation and limits

Validation checks every new local link, exact changed-file scope, whitespace,
remaining framework-interface/trait prescriptions, preserved external comparison
paragraphs, unchanged native source contracts and retained Word checksums.
Per-file review checks semantic ownership, planning-fact lifetime, phase/scope,
two-phase search, selected-only mutation, numerical guards, COMMENT and fallback.
Frontend documentation also receives its required backend-isolation scan.
All 75 new local link occurrences resolve. All 48 unchanged external-comparison
paragraphs match their prior text. The frontend scan finds four existing
documentation/guidance references and no source or build dependency. The scan
also records 17 pre-existing unresolved link occurrences in older design/plan
copies, referring to the historical v0.5 design, v0.5 implementation plan and
feedback appendix. These are separate from the new links; unrelated historical
link repairs are outside this correction.

Compiler builds, native transformation runs, GPU execution and performance
testing are not required for this documentation-only change. No existing
acceptance or hardware evidence is upgraded. AOT selection policy, broad
PREOPT COMMENT preservation and OpenCTI remain deferred.

## Edited file inventory

The two new files are
[the native contract](AI-COMPILER-WHIRL-OPCODE-AND-TENSOR-TRAIT-CONTRACT-v1.0.md)
and this audit. All 65 edited files are listed below, relative to the repository
root. This inventory distinguishes actual edits from historical originals.

- `doc/AI-COMPILER-MLA-DATA-MOVEMENT-AND-REUSE-v1.0.md`
- `doc/AI-COMPILER-MLA-DATA-MOVEMENT-AND-REUSE-v1.1.md`
- `doc/AI-COMPILER-MLA-Q1-HIGH-OWNED-FACTS-v1.0.md`
- `doc/AI-COMPILER-MLA-QUANTIZED-PAGED-CACHE-AND-TMA-v1.0.md`
- `doc/AI-COMPILER-OPTIMIZATION-AIO5-FUSION-CANDIDATES.md`
- `doc/AI-COMPILER-OPTIMIZATION-IMPLEMENTATION-PLAN-v0.10.md`
- `doc/AI-COMPILER-OPTIMIZATION-IMPLEMENTATION-PLAN-v0.11.md`
- `doc/AI-COMPILER-OPTIMIZATION-IMPLEMENTATION-PLAN-v0.12.md`
- `doc/AI-COMPILER-OPTIMIZATION-IMPLEMENTATION-PLAN-v0.13.md`
- `doc/AI-COMPILER-OPTIMIZATION-IMPLEMENTATION-PLAN-v0.14.md`
- `doc/AI-COMPILER-OPTIMIZATION-IMPLEMENTATION-PLAN-v0.15.md`
- `doc/AI-COMPILER-OPTIMIZATION-IMPLEMENTATION-PLAN-v0.16.md`
- `doc/AI-COMPILER-OPTIMIZATION-IMPLEMENTATION-PLAN-v0.6.md`
- `doc/AI-COMPILER-OPTIMIZATION-IMPLEMENTATION-PLAN-v0.7.md`
- `doc/AI-COMPILER-OPTIMIZATION-IMPLEMENTATION-PLAN-v0.8.md`
- `doc/AI-COMPILER-OPTIMIZATION-IMPLEMENTATION-PLAN-v0.9.md`
- `doc/AI-COMPILER-OPTIMIZATION-IMPLEMENTATION-PLAN.md`
- `doc/AI-COMPILER-OPTIMIZATION-SEARCH-IMPLEMENTATION-PLAN-v1.0.md`
- `doc/AI-COMPILER-OPTIMIZATION-SEARCH-IMPLEMENTATION-PLAN-v1.1.md`
- `doc/AI-COMPILER-OPTIMIZATION-SEARCH-IMPLEMENTATION-PLAN-v1.10.md`
- `doc/AI-COMPILER-OPTIMIZATION-SEARCH-IMPLEMENTATION-PLAN-v1.11.md`
- `doc/AI-COMPILER-OPTIMIZATION-SEARCH-IMPLEMENTATION-PLAN-v1.12.md`
- `doc/AI-COMPILER-OPTIMIZATION-SEARCH-IMPLEMENTATION-PLAN-v1.13.md`
- `doc/AI-COMPILER-OPTIMIZATION-SEARCH-IMPLEMENTATION-PLAN-v1.14.md`
- `doc/AI-COMPILER-OPTIMIZATION-SEARCH-IMPLEMENTATION-PLAN-v1.15.md`
- `doc/AI-COMPILER-OPTIMIZATION-SEARCH-IMPLEMENTATION-PLAN-v1.16.md`
- `doc/AI-COMPILER-OPTIMIZATION-SEARCH-IMPLEMENTATION-PLAN-v1.17.md`
- `doc/AI-COMPILER-OPTIMIZATION-SEARCH-IMPLEMENTATION-PLAN-v1.18.md`
- `doc/AI-COMPILER-OPTIMIZATION-SEARCH-IMPLEMENTATION-PLAN-v1.19.md`
- `doc/AI-COMPILER-OPTIMIZATION-SEARCH-IMPLEMENTATION-PLAN-v1.2.md`
- `doc/AI-COMPILER-OPTIMIZATION-SEARCH-IMPLEMENTATION-PLAN-v1.20.md`
- `doc/AI-COMPILER-OPTIMIZATION-SEARCH-IMPLEMENTATION-PLAN-v1.21.md`
- `doc/AI-COMPILER-OPTIMIZATION-SEARCH-IMPLEMENTATION-PLAN-v1.22.md`
- `doc/AI-COMPILER-OPTIMIZATION-SEARCH-IMPLEMENTATION-PLAN-v1.23.md`
- `doc/AI-COMPILER-OPTIMIZATION-SEARCH-IMPLEMENTATION-PLAN-v1.24.md`
- `doc/AI-COMPILER-OPTIMIZATION-SEARCH-IMPLEMENTATION-PLAN-v1.25.md`
- `doc/AI-COMPILER-OPTIMIZATION-SEARCH-IMPLEMENTATION-PLAN-v1.26.md`
- `doc/AI-COMPILER-OPTIMIZATION-SEARCH-IMPLEMENTATION-PLAN-v1.27.md`
- `doc/AI-COMPILER-OPTIMIZATION-SEARCH-IMPLEMENTATION-PLAN-v1.28.md`
- `doc/AI-COMPILER-OPTIMIZATION-SEARCH-IMPLEMENTATION-PLAN-v1.29.md`
- `doc/AI-COMPILER-OPTIMIZATION-SEARCH-IMPLEMENTATION-PLAN-v1.3.md`
- `doc/AI-COMPILER-OPTIMIZATION-SEARCH-IMPLEMENTATION-PLAN-v1.30.md`
- `doc/AI-COMPILER-OPTIMIZATION-SEARCH-IMPLEMENTATION-PLAN-v1.31.md`
- `doc/AI-COMPILER-OPTIMIZATION-SEARCH-IMPLEMENTATION-PLAN-v1.4.md`
- `doc/AI-COMPILER-OPTIMIZATION-SEARCH-IMPLEMENTATION-PLAN-v1.5.md`
- `doc/AI-COMPILER-OPTIMIZATION-SEARCH-IMPLEMENTATION-PLAN-v1.6.md`
- `doc/AI-COMPILER-OPTIMIZATION-SEARCH-IMPLEMENTATION-PLAN-v1.7.md`
- `doc/AI-COMPILER-OPTIMIZATION-SEARCH-IMPLEMENTATION-PLAN-v1.8.md`
- `doc/AI-COMPILER-OPTIMIZATION-SEARCH-IMPLEMENTATION-PLAN-v1.9.md`
- `doc/AI-COMPILER-OPTIMIZATION-SEARCH-METHODOLOGY-v1.0.md`
- `doc/AI-COMPILER-OPTIMIZATION-SEARCH-METHODOLOGY-v1.1.md`
- `doc/AI-COMPILER-OPTIMIZATION-SEARCH-METHODOLOGY-v1.2.md`
- `doc/AI-COMPILER-OPTIMIZATION-SEARCH-METHODOLOGY-v1.3.md`
- `doc/AI-COMPILER-OPTIMIZATION-SEARCH-METHODOLOGY-v1.4.md`
- `doc/AI-COMPILER-OPTIMIZATION-SEARCH-METHODOLOGY-v1.5.md`
- `doc/AI_compiler_optimization_design_v0.1.md`
- `doc/AI_compiler_optimization_design_v0.3.md`
- `doc/AI_compiler_optimization_design_v0.6.md`
- `doc/AI_compiler_optimization_design_v0.7.md`
- `doc/AI_compiler_optimization_design_v0.8.md`
- `doc/AI_compiler_optimization_design_v0.9.md`
- `doc/RESNET-NATIVE-WHIRL-PLAN.md`
- `doc/WHIRL-DSL-INFRASTRUCTURE.md`
- `doc/WHIRL-DSL-TENSOR-CONSTANT-FOLDING-PLAN.md`
- `osprey/torch2whirl/MLA-INGESTION-PLAN.md`
