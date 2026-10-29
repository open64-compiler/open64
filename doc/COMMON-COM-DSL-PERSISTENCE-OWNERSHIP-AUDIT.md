# common/com DSL Persistence And Ownership Audit

Status: the classification below records source `da6f4fbd` (2026-10-29).
The dedicated-file relocation and mixed-owner splits are implemented and
focused validation is complete, with the fixture limitations recorded in the
relocation ledger. Runtime analyses/plans live with VHO,
copied control/live REGION mechanisms with be/com, managed image services with
common/ir_image, CKKS events with common/fhe, options with common/util and
hardware descriptions with common/targ_info/access. See
[the relocation ledger](DSL-BACKEND-OWNERSHIP-RELOCATION.md) for current paths,
retained common-service exceptions and validation gates. No binary layout or
executable behavior changes are intended.

## Scope And Authority

The original inventory below covers **DSL-related code only**, not a relocation of historical
non-DSL Open64 infrastructure. It includes all 64 top-level `dsl_*` production
files in `osprey/common/com`, DSL fragments in 21 shared files, and a separate
inventory of 80 related test/support files. Target-specific legacy directories
contain no additional matches under the recorded DSL/tensor discovery rule.
The former 40-file/942-comment-gap count was a presence backlog, not an owner
or persistence classification. It is not the scope boundary of this audit.

The current JSON policy and fresh auditor output enumerate 22 retained files,
58 relocated/extracted files and mixed extraction origins, with 78 classifier
self-tests and the same 34 persisted definitions. The 64-file counts and
original receipt in this document describe the pre-move classification, not
a claim that all 64 files remain in common/com today. Current relocation does
not promote runtime records to persisted rows or change the original stored
schema contracts.

Persistence and service ownership are separate axes. An independent optimizer
analysis record does **not** belong in `common/com` merely because it is named
`IR` or `RECORD`, immutable or pointer-free. A claim that it is persistent still
requires actual binary-WHIRL evidence. However, shared IR services and their
transient helper descriptions do not need to be stored rows to belong here.

The explicit clarification is equally important: **`dsl_builder.{h,cxx}` stays
in `common/com` as the interface for persistent-data construction.** This audit
also distinguishes direct persistent ST/TY/WN construction/access interfaces
from independent analysis infrastructure. Temporary constructor arguments and
private builder state are not falsely classified as stored rows. The builder
role is not permission to put optimization decisions into the builder.

**Shared WHIRL canonicalization and constant folding, including tensor types
and tensor data, belong in `common/com`.** They are callable during ingestion
as well as later phases. Keep the existing WHIRL simplifier, `dsl_simp`, shared
tensor folding/carrier services, type canonicalization, local shape/type
predicates and supporting semantic traits with those services. Their temporary
inputs/results, decoded views and private caches remain nonpersistent; that is
not a relocation reason. Independent PU analysis, candidate discovery, costing,
profitability, plan selection and phase-specific transformations remain with
their compilation-scope owners. This corrects the earlier overbroad proposal
to move ingestion-time simplification/folding merely because its helpers are
transient; it does not change the stored-record count.

Retain `dsl_opcode` and `dsl_domain` registries as services supporting WHIRL
definition, and retain `dsl_gatekeeper` as the common WHIRL integrity-checking
service. This includes their related implementation data and helpers, not just
the public declarations. Generic unconsumed-DSL integrity scans stay with the
common IR services as well. Validation is not optimization plan selection.

Retain `dsl_telemetry_profile` as a **general telemetry data service intended to
support data beyond the compiler session**. Phase-specific profile interpretation,
freshness checks, cost construction and feedback-driven choices remain VHO-owned.
The current code stores a caller-owned profile/vector and prints text; it has no
telemetry `.B` image section or implemented cross-session storage in this module.
That implementation fact is separate from the intended service/data lifetime.
This classification correction neither adds storage nor promotes its current
profile structs into the count of `.B`-preserved rows.

**Hardware descriptions belong in the existing `osprey/common/targ_info`
owner**, which already supplies CPU target descriptions and access services.
GPU/accelerator memory hierarchy, performance/resource capacity and immutable
compute-instruction descriptions should extend that owner, not create a second
hardware-description service in `common/com` or an optimizer. This confirms
`dsl_memory_hierarchy.{h,cxx}` and `dsl_hardware_performance.h` for relocation
there. Mixed compute-realization files require a definite split: hardware/ISA
profile vocabulary, static `DSL_compute_profiles` and exact lookup to
`common/targ_info`; PU-specific plans/protocol joins and optimization choices
remain phase-owned. Referencing hardware does not turn an analytical cost model
or a software-provider capability table into a hardware description.

The existing generic-versus-FHE and PU/REGION/IPA scope rules still apply.
Generic tensor services do not become FHE-specific because an FHE caller uses
them; persisted CKKS event semantics do remain FHE-owned. No service receives
authority to activate another PU or perform cross-PU optimization by relocation.

## Method And Meaning Of Persistence

The classification follows the actual mapped-image/ELF writer and reader,
including carrier storage. It does not infer persistence from a C++ name,
`sizeof` assertion, numeric ID, flat struct, version number, printer, or trace.

1. Locate all dedicated files and shared DSL/tensor fragments from tracked
   sources, rejecting uncovered families rather than silently defaulting.
2. Extract explicit record/enum definitions, typedefs and function occurrences;
   authenticate the independent comment/function census against every dedicated
   source hash. Declaration and definition occurrences count separately.
3. Trace positive stored-record evidence to writer, reader and existing section
   or symtab/carrier boundary. Trace AIO planning objects to their caller-owned
   vectors and existing phase producers instead of assuming they are images.
4. Record current include consumers, owning phase, transient lifetime and proposed
   action. Mixed files require a split; file-level classification is not a
   recommendation to move an entire shared legacy translation unit.

The located inventory contains 208 explicit record definitions: **34 are stored
in `.B` and 174 are not stored as `.B` records**, including 20 builder
construction-state/input definitions. This is a storage-boundary count, not a
claim that every other record's semantic data must expire with a compiler
session; telemetry's intended cross-session lifetime is classified separately.
The 34 consist of 31 definitions in
dedicated DSL files plus three tensor table rows in `symtab.h`. Aliases of the
same row do not increase the count. The dedicated files contain 1,280 function
sites after excluding 49 macro definitions misidentified by the comment scanner
(including three core-schema sentinels). Shared files add 283 lexical function sites. The include
index records 441 sites across 222 consumer files, not a resolved call graph.
No function's executable code is itself part of a `.B` file: function labels
describe the data/service contract it implements.

This is a source ownership classification, not 1,280 fresh function-contract
reviews or a runtime test certificate. Lexical extraction covers textual
conditional branches but not implicit C++ members or macro-expanded ASTs.
The complete located type/function details and source snippets are retained in
the machine inventory; exact helper cuts need AST/link review before migration.

## Positive Persistent Inventory

| Existing storage boundary | Actual stored definitions | Decision |
| --- | --- | --- |
| `.WHIRL.dsl` | `DSL_IR_IMAGE_HEADER`, `DSL_IR_OPCODE_DESCRIPTOR_RECORD`, `DSL_IR_NODE_RECORD`, `DSL_IR_ATTRIBUTE_RECORD`, `DSL_IR_VALUE_RECORD`, `DSL_IR_VALUE_REFERENCE_RECORD` | Retain schema in `dsl_ir_image.h`. |
| `.WHIRL.dsl_effects` | `DSL_EFFECT_IMAGE_HEADER`, `DSL_STATE_OBJECT_RECORD`, `DSL_STATE_EFFECT_RECORD` | Retain schema. |
| `.WHIRL.dsl_calls` | `DSL_CALL_IMAGE_HEADER`, `DSL_PU_SOURCE_IDENTITY_RECORD`, `DSL_CALLSITE_METADATA_RECORD` | Retain schema. |
| `.WHIRL.dsl_call_abi` | `DSL_CALL_ABI_IMAGE_HEADER`, `DSL_CALL_ARGUMENT_RECORD` | Retain schema. |
| `.WHIRL.dsl_pu_interface` | `DSL_PU_INTERFACE_IMAGE_HEADER`, `DSL_PU_FORMAL_RECORD` | Retain schema. |
| `.WHIRL.dsl_runtime_interface` | `DSL_RUNTIME_INTERFACE_IMAGE_HEADER`, `DSL_RUNTIME_VALUE_PROJECTION_RECORD`, `DSL_RUNTIME_CALL_PROJECTION_RECORD` | Retain schema; runtime meaning does not make these transient. |
| `.WHIRL.dsl_program_interface` | `DSL_PROGRAM_INTERFACE_IMAGE_HEADER`, `DSL_RETIRED_FORMAL_RECORD`, `DSL_RETIRED_CALL_ARGUMENT_RECORD`, `DSL_RUNTIME_INPUT_RECORD`, `DSL_RUNTIME_INPUT_BINDING_RECORD`, `DSL_RUNTIME_INPUT_CALL_RECORD` | Retain schema. |
| `WT_REGIONS` PU subsection | `DSL_REGION_IMAGE_HEADER`, `DSL_REGION_IMAGE_RECORD`, `DSL_REGION_INTERFACE_RECORD` | Extract schema from mixed `dsl_region.h`; retain stored IDs/flags/offsets exactly. |
| `.WHIRL.dsl_ckks_event` | `DSL_CKKS_EVENT_IMAGE_HEADER`, `DSL_CKKS_EVENT_RECORD` | Persistent but FHE-specific: relocate schema and related store/inspection to `common/fhe`, preserving the section. |
| Global symtab `SHDR_TY_TENSOR_EXT`, `SHDR_ST_TENSOR_METADATA`, `SHDR_TENSOR_DSL_KV` | `TY_TENSOR_EXTENSION_STORE`, `ST_TENSOR_METADATA_STORE`, `TY_DSL_KV` | Retain row definitions and persistent access/construction boundary. |
| TCON `SHDR_TCON` plus string payload `SHDR_STR` | `DSL_TENSOR_TCON_ENVELOPE` (also aliased as `DSL_TENSOR_TCON_RECORD`) | Retain existing 128-byte carrier schema/storage-kind encoding; do not invent a tensor-constant ELF section. |

For the first seven families, `ir_bwrite.cxx` writes the exact header/row types
and `ir_bread.cxx` selects the corresponding mapped loader. The source witness
tool checks every claimed type in its writer body. The region writer copies
the header, offset-adjusted region rows and typed interface vector; its loader
maps those same row types. The CKKS writer emits its two exact types.

`ir_bcom.cxx` writes the three tensor extension/metadata/KV tables with their
three SHDR kinds; `ir_bread.cxx` transfers those tables from the mapped image.
Their C++ `SEGMENTED_ARRAY` containers are not themselves dumped object layouts.

The tensor carrier builder copies the envelope into a character payload,
creates a `Host_To_Targ_String` TCON and interns it. The ordinary TCON and
TCON-string image path preserves it; a decoded stack record and a cache entry
are not additional persisted records. Side-file bytes themselves remain
external: the carrier preserves their reference/range/checksum, not the file.

Encoded symbolic definitions are a separate category. In particular,
`DSL_OPERATOR`, opcode category/level, shape/effect/lowering model values,
image IDs/enums/flags/version/size constants, region roles/flags and tensor
binding-state values support stored fields. Preserve their values even when
their runtime lookup implementation moves. Include guards and size-check
typedefs are compile-time scaffolding, not extra data records.

## Complete Dedicated-File Classification

Paths in this table are relative to `osprey/common/com`. Each `{h,cxx}` entry
means both actual files; `_internal.h` entries are explicitly included. The
reviewed policy manifest lists all 64 paths individually, with no unassigned
file. Proposed owner directories exist already; proposed split filenames are
design choices, not files created by this audit.

| Current file family | Persistence and important contents | Owner/action |
| --- | --- | --- |
| `dsl_ir_image.h` | 25 stored headers/rows plus supporting symbolic schema; no service declarations | Keep. Existing store/loader/print logic is already in `common/ir_image`. |
| `dsl_builder.{h,cxx}` | Persistent-construction interface; 20 transient input/private-state record definitions, borrowed handles, source/DST tracking and registries | **Keep both files**, as directed. Retain emission/finalization and construction lifecycle; delegate analysis, do not broaden it. |
| `dsl_ckks_event.{h,cxx}`, `dsl_ckks_event_internal.h` | Two stored types; transient table container, owner/context/event/result keys and maps | Move with FHE owner to `common/fhe`; not VHO-only, because readers/printers consume the image. |
| `dsl_region.{h,cxx}`, `dsl_region_internal.h` | Three stored types; `dsl_region_runtime`, `DSL_REGION_STORE`, handles, WN/PU associations, symbol-redirection and lowering-transition scratch are transient | Split retained image schema, `common/ir_image` store/mapping support and backend-free `be/com` physical PU/REGION mechanisms. |
| `dsl_tensor_fold.{h,cxx}` | One stored envelope; creation-info, evaluator identity, policy, candidate, result-slot, output, mock, value, replacement context/result and cache are transient | Keep shared tensor constant folding, carrier construction/access and private helper/cache data in `common/com`, callable during ingestion. VHO/WOPT retain phase-specific optimization policy. |
| `dsl_opcode.{h,cxx}` | Encoded vocabulary plus nonpersistent static semantics and opcode/wrapper/promotion registration objects | Keep the complete WHIRL definition/registry service and related data/helpers in `common/com`; no registry split proposed. |
| `dsl_domain.{h,cxx}` | Dynamic domain record/INFO, copied names, epoch-local IDs | Keep the WHIRL domain-definition registry in `common/com`; do not confuse its registration IDs with mapped descriptor IDs. |
| `dsl_contract.{h,cxx}` | Owning records, seeds, INFO and required-check/diagnostic vectors | Registry owner in `common/ir_image`; projected contract names/versions in WNs or region rows may persist, not these registry objects. |
| `dsl_memory_behavior.{h,cxx}` | Static seed and contract-output objects are transient; three `DSL_Tensor_*` symbol creation/annotation access APIs act on persisted ST/metadata | Keep in `common/com`: shared memory/effect semantics support construction/canonicalization eligibility, without claiming that the inventory is stored or constitutes alias proof. |
| `dsl_gatekeeper.{h,cxx}` | Verification context/results/modes/decode profile; no stored rows | Keep the common WHIRL integrity service in `common/com`, including its scratch descriptions. Builder/driver controls scope; this does not authorize optimization. |
| `dsl_shape.{h,cxx}` | Operator input, facts, inference input, solver value/constraint/result, refinement and visitor are transient | Shared type/operator predicates and local shape inference stay in `common/com` as ingestion/type services; PU fixed-point/refinement discovery stays phase-owned in `be/vho`. Caller owns mutation and scope. |
| `dsl_simp.{h,cxx}` | Candidate/result and disposable physical-WN projections are transient | Keep the shared ingestion-callable bridge in `common/com`, preserving `wn_simp_code.h` reuse and the master switch. No backend-phase dependency is needed. |
| `dsl_tensor_evolution.{h,cxx}` | PU graph and node/edge representation records are runtime-only | Move records, graph storage, add/get/verify/print APIs to `be/vho`, alongside existing `dsl_tensor_evolution_opt`. |
| `dsl_opt_plan.{h,cxx}` | Budget, candidate/input, cost/input/term, plan/input/member, recorded selection and owning context vectors are runtime-only | `be/vho`, beside existing selection owner; recording a selection does not create an image. |
| `dsl_tensor_analysis.{h,cxx}`, `dsl_tensor_analysis_internal.h` | Semantic fact/use records, owning analysis, completeness/generation state | `be/vho`, with existing fact derivation; pointers to persistent rows do not persist the analysis. |
| `dsl_tensor_locality.{h,cxx}`, `dsl_tensor_locality_internal.h` | Control block/position/snapshot and locality fact/use/analysis | Shared copied-control snapshot types/services in `be/com`; locality analysis in `be/vho`; actual CFG capture remains `be/opt`. Avoid a WOPT-to-VHO link dependency. |
| `dsl_fusion_candidate.{h,cxx}` | Fusion sites, members, boundaries, create-info and container | `be/vho`, beside existing discovery/legality/cost/selection. |
| `dsl_layout_candidate.{h,cxx}` | Layout descriptor/axis/block/site/alternative, create-info and container | `be/vho`; not the stored TY tensor-representation attributes. |
| `dsl_distributed_candidate.{h,cxx}` | Placement/alias/range/site/alternative, communication epochs/intents, create-info and container | `be/vho` for current PU-local plans; future cross-PU inference requires explicit IPA. |
| `dsl_residency_candidate.{h,cxx}` | Residency descriptor/site/alternative, create-info and container | `be/vho`; target facts remain separate from capacity/profitability decisions. |
| `dsl_tile_candidate.{h,cxx}` | Tile site/plan/stage, create-info and container | `be/vho` planning owner; canonical-loop realization remains `be/lno`. |
| `dsl_fetch_pipeline.{h,cxx}` | Fetch sites/plans/records, pipeline stages, resources, create-info and container | `be/vho`, alongside movement/overlap/barrier planning; no new writer. |
| `dsl_physical_plan.{h,cxx}` | Software-provider capability inventory, physical site/implementation, create-info and container | Provider planning remains `be/vho`; library/kernel availability is not automatically hardware description data, even when records reference a target profile. |
| `dsl_runtime_variant.{h,cxx}` | Variant sites/variants/guards, create-info and container | `be/vho`; emitted guard WNs may persist, these planning objects do not. |
| `dsl_telemetry_profile.{h,cxx}` | General telemetry header/records, create-info and owning profile/vector; intended data lifetime extends beyond compiler sessions | Keep storage/construction/structural checks/access/printing in `common/com`. VHO retains freshness/interpretation and optimization policy. Current in-memory/text implementation is not a `.B` telemetry section or implemented cross-session storage. |
| `dsl_accumulation.{h,cxx}` | Numerical contract, accumulation plan and segment records | `be/vho`, with numerical planning/verification/inspection. No numerical-plan image section exists here. |
| `dsl_compute_realization.{h,cxx}` | Mixed immutable hardware/ISA recipe vocabulary/profiles and PU-specific realization/fallback/protocol/resource/link records | Split static profile vocabulary/table/exact lookup to `common/targ_info`; keep PU realization joins/validation/inspection in `be/vho`. Do not relocate the mixed pair wholesale. Preserve exact profile identities/support states. |
| `dsl_compute_model.{h,cxx}` | Unknown/known ranges, provenance and analytical/synthetic model records | `be/vho` shadow-model owner; unknown timing stays explicit, never a zero-rate or measured-performance claim. |
| `dsl_typed_simt.{h,cxx}` | Typed baseline acceptance record | `be/vho` with typed SIMT lowering; lowered F16/BF16 WN/ST/TY persistence is distinct from this transient request. |
| `dsl_memory_hierarchy.{h,cxx}` | Hardware description, hierarchy profile, tier, shared-bank and movement-capability records | Relocate with related lookup/validation to existing `common/targ_info`, extending CPU target-description ownership for GPU/accelerator hardware. VHO retains occupancy/cost/selection policy. |
| `dsl_hardware_performance.h` | Runtime performance/resource capacity payloads and structural inline predicates | Relocate to `common/targ_info` with hardware descriptions; not persisted allocations, measured timing or calibration. |

Earlier AIO Ownership M1-M5 separated optimization decisions from policy-free
storage. That work was useful and is not reversed. However, its completion
criterion did **not** prove those retained runtime storage objects persistent.
The directory-admission follow-up proposes moving independent nonpersistent
optimizer records and their related services together. It retains shared IR
canonicalization/folding and their helper data in common, without moving
optimization policy back there.

## Shared-File DSL Fragments

The shared inventory preserves source hashes and every DSL/tensor-matching line.
It does not classify entire historical files as DSL or propose moving unrelated
symbol-table, WN, option, opcode, debug or printing infrastructure wholesale.

| Current shared files | DSL classification and proposed boundary |
| --- | --- |
| `symtab.h`, `symtab.cxx`, `symtab_defs.h`, `symtab_verify.cxx` | Keep stored tensor/KV rows, encoded vocabulary and shared tensor construction/canonicalization/access services in `common/com`, including their transient canonical descriptor/refinement inputs, INFO, attribute-copy scratch and canonical index/cache. `TENSOR_DESCRIPTOR_RECORD` stays a derived service view, not a separately written table. |
| `wn.h`, `wn.cxx` | Keep encoded native/marker meaning, shared constructors/logical accessors, borrowed decoded views, and `VHO_UNCONSUMED_DSL_SCAN` with its four generic integrity scan/print helpers in `common/com`. Their temporary counters are not persistent fields, and their prefix does not make them an optimizer pass. |
| `ir_bcom.cxx`, `ir_bread.{h,cxx}`, `ir_bwrite.{h,cxx}` | DSL writer/mapped-reader machinery belongs to `common/ir_image`, not a schema record. Preserve row vocabulary in `common/com`, image bytes, optional sections, cleanup/reset order and backend-free physical validation linkage. |
| `ir_reader.cxx`, `ttype.cxx` | DSL/tensor logical inspection belongs with image/type inspection services; retain persistent semantics and logical names, never expose private physical-tag details. |
| `strtab.cxx` | Ordinary string bytes are persistent. Private CKKS count save/restore is transient store support; existing declaration is in `common/util/dsl_strtab_checkpoint.h`. Keep store ownership and private access coherent; do not export raw string tables or pretend the savepoint is a stored row. |
| `config_dsl.{h,cxx}`, DSL registration in `config.cxx` | Options/globals/descriptor table and trace bits are process controls, not image schema. Shared option infrastructure (`common/util`) owns registration/storage, while VHO/WOPT own option meaning. Preserve the existing include-registration arrangement until a reviewed build move. |
| `opcode_gen`, `opcode_gen_core.{h,cxx}` | Physical DSL slot/properties are compatibility vocabulary; generator execution/name/property tables are build/service logic. Preserve every target slot and generator gate; no wholesale legacy generator move is part of this proposal. |
| `wn_simp_ftable.h` | Keep the DSL adapter hook with the traditional common WHIRL canonicalization/folding engine, callable during ingestion. Do not move or duplicate it merely because adapter data is transient. |

## Consumer And Migration Constraints

The inventory retains explicit include sites and source hashes across `osprey`.
These locate reverse consumers, not a resolved linker or preprocessor graph.
Important edges reviewed for the proposal are:

- Builder, gatekeeper, reader and backend phases share shape/contract/region/
  tensor-constant services. A blanket move to `be/vho` would create backward
  frontend/reader dependencies. Use backend-free `be/com` or `common/ir_image`
  mechanisms and keep phase policy in its established owner.
- WOPT's `opt_dsl_locality.cxx` constructs the copied control snapshot. Its
  plain copied-data contract must not become a dependency on VHO analysis code.
- The existing physical PU transaction, rewrite/retype/lower, REGION and
  builder direct-object users must remain link-closed; filenames alone do not
  establish a library boundary.
- Static hardware/recipe inventories must not move target selection, cost
  estimation or native-emission decisions into an image reader or emitter.
- No new `be.so` library dependency, `DSL_Builder_*` leakage, JSON dependency,
  implicit PU activation, name/value renumbering or `.B` version change is
  authorized by this audit.

Recommended staged implementation order, subject to an explicit next request:

1. Keep the builder and shared canonicalization/folding/type services, including
   tensor carriers and their helper data. Extract region schema as needed,
   preserving every stored field/ID/flag/version/size. Move CKKS event schema
   and related implementation to `common/fhe` under the existing domain rule.
2. Retain opcode/domain definition registries, common integrity checks and the
   general telemetry service. Review only remaining independently classified
   storage/mapped mechanisms, preserving builder/reader consumers before
   touching phase-only records. Do not infer a move from process-local state.
3. Move AIO transient fact/graph/plan families with their existing VHO owners;
   split the WOPT-compatible control snapshot first. Maintain include/link
   direction and move related tests rather than duplicating definitions.
4. Relocate memory hierarchy and hardware performance/resource descriptions
   to `common/targ_info`. Split immutable compute hardware/ISA profiles from
   PU realization state first, preserving lookup/build direction. Analytical
   cost models, numerical plans and software-provider selection remain with
   their actual logic owners. Preserve SIMT default and explicit unknown timing.
5. Keep shared WN/TY construction/canonicalization views, simplifier hooks and
   integrity checks with their common services. Audit independent option fragments
   separately; do not couple this to a historical common/com reorganization.

Each actual move requires its own source-equivalence, API/symbol/build closure,
focused malformed/rollback tests and independently reopened `ir_b2a -st -src`
evidence. Existing accepted receipts may be reused only when their complete
producer inputs match. This inventory does not pre-certify those future moves.

## Reproduction And Validation

The reviewed path/owner/decision manifest is
[`common-com-dsl-ownership-policy.json`](common-com-dsl-ownership-policy.json).
The reproducer is
[`tools/common_com_dsl_ownership_audit.py`](tools/common_com_dsl_ownership_audit.py).
It requires a fresh independent comment/function census from the retained
`audit_dsl_comments.py` tool; its complete input identity is recorded in output.

```sh
python3 /path/to/retained/audit_dsl_comments.py \
  --root "$PWD" --output /private/tmp/common-com-comment-census-fresh
PYTHONDONTWRITEBYTECODE=1 python3 doc/tools/common_com_dsl_ownership_audit.py \
  --root "$PWD" \
  --comment-audit /private/tmp/common-com-comment-census-fresh/audit.json \
  --output /private/tmp/common-com-dsl-ownership-fresh
```

Output includes `inventory.json`, `files.csv`, `entities.csv` and raw shared
ctags evidence. JSON carries source hashes, located type/function snippets,
positive persistence witnesses, explicit include consumers, shared fragments,
test inventory and limitations. Public declaration coverage and claimed stored
type coverage are checked independently of family assignment.

Validation is documentation/auditor-only: parser and critical mixed-role
self-tests, source hash/coverage checks, writer/reader witness checks, impact
selection, no-tab scan and `git diff --check`. No compiler producer, full model,
GPU/performance or fresh binary compatibility result is claimed. No new `.B`
or trace is produced for this no-code-change classification.

Durable evidence is retained under the primary checkout's
`artifacts/review/code-quality/dsl-ownership/common-com-classification/`, with
content-addressed inventory, source snapshots, commands, compliance and immutable
receipt/checksums. The review indexes identify the accepted full digest. No
source move, push or pull request is included.
