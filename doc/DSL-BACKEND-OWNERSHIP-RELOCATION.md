# DSL Backend Ownership Relocation

Status: staged implementation, beginning with the numerical/model leaf batch.
The full relocation is not complete. The source classification and authority
remain in `COMMON-COM-DSL-PERSISTENCE-OWNERSHIP-AUDIT.md` and its JSON policy.

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

## Remaining Backend Work

Move these complete runtime families to their existing VHO logic owner,
reviewing every contract and dependency together with each staged move:

- `dsl_tensor_evolution`, `dsl_opt_plan`, `dsl_tensor_analysis` and its private
  header.
- `dsl_fusion_candidate`, `dsl_layout_candidate`, `dsl_distributed_candidate`,
  `dsl_residency_candidate`, `dsl_tile_candidate`, `dsl_fetch_pipeline`.
- `dsl_physical_plan` and `dsl_runtime_variant`. Keep software-provider
  capability facts with planning; do not label them hardware descriptions.

Mixed families require an actual implementation split, not a wholesale move:

- `dsl_shape`: retain shared local type/operator predicates and inference in
  `common/com`; move PU-wide fixed-point analysis/refinement discovery to VHO.
- `dsl_tensor_locality`: VHO locality analysis; shared copied control snapshot
  mechanisms in `be/com`; CFG capture remains in WOPT.
- `dsl_compute_realization`: VHO runtime plan/protocol/resource joins;
  immutable ISA/profile vocabulary, static profiles and exact lookup in the
  existing `common/targ_info` hardware owner.
- `dsl_region`: persisted schema stays in `common/com`; managed mapped-image
  storage/I/O belongs in `common/ir_image`; live PU/REGION mutation in `be/com`.

General telemetry stays in `common/com`; separate its stable plan/site/variant
identities from VHO-owned stores before moving their headers. Shared builders,
canonicalization/folding, opcode/domain registries and integrity verification
also stay. Hardware-only `dsl_memory_hierarchy` and
`dsl_hardware_performance` moves belong to `common/targ_info`, not `be`.

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
