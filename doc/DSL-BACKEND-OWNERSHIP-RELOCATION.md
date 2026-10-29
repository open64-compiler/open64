# DSL Backend Ownership Relocation

Status: all reviewed dedicated DSL families have been relocated or split by
their logic owners; focused linked validation is complete with the existing
fixture limitations recorded below. The source
classification and authority remain in
`COMMON-COM-DSL-PERSISTENCE-OWNERSHIP-AUDIT.md` and its JSON policy. This does
not relocate unrelated historical Open64 infrastructure or claim that every
shared legacy file is comment-complete.

## Completed Source Moves

The following pairs move unchanged in executable content from
`osprey/common/com` to `osprey/be/vho`, alongside their existing optimizers:

- `dsl_accumulation.{h,cxx}`: numerical permission/evidence, segmentation
  verification and planning-only printing.
- `dsl_compute_model.{h,cxx}`: analytical range/configuration descriptions and
  structural verification. Unknown timing remains explicit, not a zero rate.
- `dsl_typed_simt.{h,cxx}`: F16/BF16 source acceptance with explicit FP32
  product, partial, outer and output formats.

These are caller-owned runtime descriptions, not `.B` row definitions. The
move introduces no new symbols or library dependency and changes no WN/ST/TY
encoding, section, writer, reader or numerical acceptance behavior. Contract
comments now identify the backend owner, borrowing, validation and no-output
failure boundaries. The stale claim that F16/BF16 have no typed lowering is
removed; the segmented accumulation API still rejects them deliberately.

Backend Make source groups and explicit `ir_tools` prerequisites follow the new
owner. Standalone `ir_tools` retains its existing direct structural-support
object for accumulation; it does not link the VHO optimizer into the reader.
Basename includes resolve through existing backend include directories. No
forwarding headers remain in `common/com`.

The JSON policy's `groups` continues to enumerate current `common/com` files.
`relocated_groups` retains the old/new paths, classification and witnesses for
completed moves, so previously reviewed families are not silently forgotten.
The original classification receipt remains an immutable historical inventory.

## Hardware Description Batch

The hardware side now lives in `osprey/common/targ_info/access`, extending the
existing shared target-description API owner rather than creating an optimizer
registry:

- `dsl_memory_hierarchy.{h,cxx}` and `dsl_hardware_performance.h` move whole,
  with contracts reviewed against exact lookup, output preservation, immutable
  family envelopes and the deliberately limited structural predicates.
- `dsl_compute_profile.{h,cxx}` extracts the profile ID, enums, row vocabulary,
  static bounded ISA/toolkit table and exact lookup from `dsl_compute_realization`.
- `dsl_compute_format.h` extracts the existing `DSL_ACCUMULATION_FORMAT` enum
  from the VHO accumulation header. Both hardware and VHO consumers include
  this dependency-light vocabulary; target headers do not include backend
  plan, PU, optimizer or provider interfaces. Numeric values are unchanged.

The remaining `dsl_compute_realization` pair now lives in VHO and contains
phase-facing runtime records and structural joins. It no longer owns hardware
tables. VHO analytical models and software-provider capabilities do
not move into targ_info. These source moves deliberately retain the existing
direct-object linkage in backend/tools: no new library, CPU libtarginfo archive
membership, shared-library export contract or installation interface is added.

All quantities, synthetic/estimated provenance, exact suffix behavior, unknown
states and conservative support labels remain unchanged. Hardware inventory
lookup does not certify lowering or native execution; CAPACITY_EXACT in an
existing family row does not discover a physical device or identify its SKU.
The standalone hardware fixture builds only common target sources, with no
backend include path, and retains explicit-field inventory output for parity.

## Runtime Families And Mixed-Owner Splits

These complete runtime families now live with their existing VHO logic owner;
contracts were reviewed against their actual implementations during the moves:

- `dsl_tensor_evolution`, `dsl_opt_plan`, `dsl_tensor_analysis` and its private
  header.
- `dsl_fusion_candidate`, `dsl_layout_candidate`, `dsl_distributed_candidate`,
  `dsl_residency_candidate`, `dsl_tile_candidate`, `dsl_fetch_pipeline`.
- `dsl_physical_plan` and `dsl_runtime_variant`. Keep software-provider
  capability facts with planning; do not label them hardware descriptions.

Mixed families are split rather than wholesale moved:

- `dsl_shape`: shared local type/operator predicates and inference remain in
  `common/com`; `be/vho/dsl_shape_analysis` owns PU-wide fixed-point analysis
  and refinement discovery. A common private fact header shares unchanged
  equality/merge predicates. The retained general gatekeeper invokes the
  check-only solver through its existing integrity boundary; frontend tools
  link this backend-free support directly, not through `be.so`.
- `dsl_tensor_locality`: VHO locality analysis; shared copied control snapshot
  mechanisms in `be/com`; CFG capture remains in WOPT.
- `dsl_compute_realization`: runtime plan/protocol/resource joins live in VHO;
  its immutable hardware side lives in targ_info.
- `dsl_region`: persisted schema stays in `common/com`; managed mapped-image
  storage/I/O belongs in `common/ir_image`; live PU/REGION mutation in `be/com`.

General telemetry stays in `common/com`; `dsl_plan_identity.h` separates its
stable plan/site/variant IDs from VHO-owned stores. This adds no stored row.
Shared builders,
canonicalization/folding, opcode/domain registries and integrity verification
also stay. Hardware-only descriptions are in common/targ_info/access, not be.
CKKS event schema/services live in `common/fhe`, the managed contract registry
in `common/ir_image`, and the shared option declarations/textual table include
in `common/util`. `config_dsl.cxx` remains included by `config.cxx`, not a new
independently linked translation unit. Existing direct-object support remains
available to readers and frontends without adding a backend-library dependency.

## Validation Gates

Each batch requires executable-token equivalence, exact owner/build-path
checks, architectural and per-function semantic comment review, and hygiene.
For numerical/model leaves, rebuild `be.so`, `be`, `lw_inline`, `ir_b2a` and
the six numerical/shadow/typed-SIMT/WGMMA contract executables. Check shared
library dependencies and symbol closure, malformed/rejected contracts, source
interleave, and mapped images before accepting the batch. A path move does not
authorize rerunning full-model production or claiming GPU performance.

Retain source snapshots, build commands/logs, linked tools, raw `.B`/`.T`,
comparisons and checksummed receipts under the durable review root. Raw
reserved-table-byte differences must be explained, not normalized away to
manufacture byte equality. Record actual results and limitations in the receipt.

Numerical/model leaf batch results: all six complete non-comment token streams
match the pre-move source; all twelve declaration/definition contracts were
reviewed against their implementations. The linked numerical, shadow-model,
typed-SIMT and WGMMA fixtures pass. Fourteen before/after `.B` pairs are
byte-identical and 56 independent cross-reader reopens produce equal
source-interleaved dumps. Native syntax and the six-configuration physical
operator layout matrix pass. Backend defined/undefined dynamic symbol sets and
all five shared-library dependencies match; `be` and `lw_inline` are loader
closed with their matching backend. This is Linux x86-64 DEBUG host/compiler
evidence, not GPU execution, performance or cross-platform certification.

## Remaining-Owner Batch Results

The current policy covers 80 dedicated production files: 22 retained in
common/com and 58 at their reviewed owners, including the preceding numerical
batch. The fresh source proof accounts for all 440 original function bodies,
210 schema units, 85 initializers, 13 generated macro definitions and 47 macro
invocations across the remaining 19 families. There are no unmatched original
units or unexplained additions. Three REGION helpers become hidden cross-unit
bridges; the single registry remains image-owned. The added `WN_MAP` prerequisite
alias is identical to the existing Open64 alias. No persisted encoding changes.

All 60 moved/split production files have reviewed architecture/function
contracts. Thirteen include-only consumers preserve their other tokens; their
228 pre-existing lexical comment findings are outside this relocation's
whole-file cleanup scope. This does not certify all repository DSL comments or
resolve unrelated deferred one-statement comparator follow-up work.

Linux DEBUG validation rebuilds backend/tools/shared consumers, the WOPT
locality adapter, and native Python frontend. Ninety-three AIO modes, forty
isolated builder modes, the combined builder suite, shape refinement and PU
transaction clone/apply/reopen pass. Native Python runs 292 tests with one
skip; its separate-process artifact smoke passes. Hardware field
inventories are equal, and standalone moved-header consumers pass. Fourteen
numerical before/after images are byte-identical with 56 matching cross-reader
reopens. All 67 REGION/CKKS/ABI/shape/frontend images also pass previous-reader
acceptance and exact source-interleaved comparison with the current reader,
using the same source-root working directory. SIMT stays default and
all 216 pipeline-model stress scenarios keep timing unknown.

Backend dynamic identities are unchanged: 45,385 definitions, 268 imports and
the same five shared libraries. `be` and `lw_inline` are loader-closed; neither
they nor `be.so` acquire frontend builder or JsonCpp symbols. The three new
cross-unit REGION bridges are local/hidden, not a new dynamic export contract.

Known limitations are retained, not silently counted as passes: the isolated
multiple-PU builder mode fails mapped finalization in both pre-relocation and
current builds, despite the combined suite passing. Existing lower-test link
recipes omit optimizer dependencies; the physical-plan fixture uses an explicit
staging-only object override, and the unrelated lower target is not certified.
An optional duplicate whole-model text inspection was stopped and kept as
partial output; no full-model rematerialization or fresh whole-model audit is
claimed. Focused evidence crosses the changed ownership/link boundaries without
altering existing executable bodies to repair unrelated failures.
