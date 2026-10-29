# DSL IR Persistence And Owning-Logic Inventory

Status: full `dsl_ir_image.{h,cxx}` owner split implemented in one batch,
including its directly related nonpersistent service closure (2026-10-28).
The controlling plan is the
[persistent IR / service separation](WHIRL-DSL-INFRASTRUCTURE.md#persistent-ir--nonpersistent-service-separation).
This is not a repository-wide common/com ownership or comment certification.

## Persistent Boundary

`osprey/common/com/dsl_ir_image.h` is now 611 lines of schema only: 25 stored
headers/rows, 18 persistent ID aliases, 15 persistent enums and 61 schema
macros (119 retained contracts in total). It has no function declarations,
transient request/view/journal definitions, WN/PU forward declarations or
reverse includes of service headers. `common/com/dsl_ir_image.cxx` is removed;
its actual managed-store implementation lives in `common/ir_image`.

| Existing ELF section | Retained record family |
| --- | --- |
| `.WHIRL.dsl` | Core header, opcode descriptors, nodes, attributes, values, references |
| `.WHIRL.dsl_effects` | Effect header, state objects, state effects |
| `.WHIRL.dsl_calls` | Call header, PU source identities, callsite metadata |
| `.WHIRL.dsl_call_abi` | ABI header, arguments |
| `.WHIRL.dsl_pu_interface` | PU-interface header, formals |
| `.WHIRL.dsl_runtime_interface` | Runtime header, value/call projections |
| `.WHIRL.dsl_program_interface` | Program header, retired formals/arguments, runtime inputs/bindings/calls |

The manifest links each family to its existing writer, loader and ELF section
name definition. No record fields, layout constants, IDs, enums, flags,
versions, optional-section rules or reader/writer behavior change.
Runtime/program-interface rows remain persistent despite their runtime meaning.

## Current Owning Logic

| Owner | Actual data and implementation |
| --- | --- |
| `common/ir_image/dsl_image_service.h`, `dsl_ir_image.cxx` | One authoritative managed store, segmented containers, mapped views, live call-WN associations, resets, row services and structural validation. |
| `common/ir_image/dsl_lowered_relation.h`, `dsl_ir_print.cxx` | Derived lowered-row summaries and logical inspection; the new image-query alias preserves the legacy typedef's C++ linkage and layout. |
| `common/ir_image/dsl_image_checkpoint_internal.h`, `dsl_pu_specialize_internal.h` | Private image-only append/flag checkpoints and logical row cloning; implementations stay beside the private managed tables. No physical journal or raw table is exported. |
| `be/com/dsl_external_tensor.h`, `dsl_ir_rewrite.cxx` | External facts, typed/generated provenance, copied source snapshots/handles, active-PU materialization, native definition lookup, rewrite and retirement. |
| `be/com/dsl_pu_interface_verify.h`, `dsl_ir_rewrite.cxx` | Backend-free physical PU/call ABI verification, shared by readers/builders and backend phases. |
| `be/com/dsl_runtime_interface*`, `dsl_program_interface*` | ABI requests/results, return-site scratch, plan fingerprints, committed-PU evidence, active-PU mutation and verification. |
| `be/com/dsl_ir_lower*`, `dsl_ir_group_lower.cxx`, `dsl_ir_retype*` | Lowering/group/retype requests, scans/journals, standard-block preflight, mutation, postcheck and existing rollback. |
| `be/com/dsl_ir_rewrite_queries_internal.h`, `dsl_ir_transaction_internal.h` | Shared physical read-only queries with internal linkage and active-PU predicates. Two trivial comparator methods are explicitly inline in private headers. |
| `be/vho/dsl_ckks_expand*` | CKKS expansion requests/steps, physical operands/use scans, fault state, preflight/commit/verification and combined rollback. |
| `common/util/dsl_strtab_checkpoint.h` | Existing private string checkpoint contract; the established string store still implements save/restore in `strtab.cxx`, with no exposed raw store or new library. |

The proposed `common/whirl_pu` directory was not introduced: existing `be/com`
is the shared backend-free physical-PU mechanism owner. Moving source there
does not authorize optimization policy, PU activation or CG dependencies.
All prior frontend/reader direct-object consumers are preserved.

After actual algorithm review, the inventory's initial phase-owned designation
of **image-only** clone/CKKS savepoints is refined: the count/flag journal and
its trim/copy methods belong to the managed-store owner. They remain private,
not persistent IR or a phase-wide transaction API. Physical transactions still
own WNs, symbols, REGION, PU context and combined rollback. Count-only limits,
partial output, shared-string appends and uncaught allocation failures are not
silently upgraded to full atomic rollback.

The existing image-to-source-reset and image-to-program-commit-state calls
retain their original backend-free object boundary and ordering. No callback
registry, duplicated store, weakened structural/active-PU validator, or reverse
schema-to-phase include is introduced. Lowered-row queries remain available
to printers/readers without importing physical lowering requests.

## Complete Manifest And Reproduction

- [Current CSV](dsl-ir-ownership-inventory.csv): every located occurrence.
- [Current JSON](dsl-ir-ownership-inventory.json): 907 occurrences, 100 complete
  structures (25 persistent, 75 nonpersistent), 566 method occurrences,
  implementation/reference locations, source hashes, token/body hashes and
  positive persistence evidence. The expansion from the original three files
  reflects direct companion closure, not new persisted records.
- [Extractor](tools/dsl_ir_ownership_inventory.py): nine implementation ASTs,
  all owner headers, lexical structure/header reconciliation and independent
  ctags reconciliation. Twelve primitive tests pass; five macro-generated
  accessors and one split-line getter remain explicit AST-only cases.
- [Relocation oracle](tools/dsl_ir_owner_split_check.py): independently parses
  before/after sources, reconciles every original inventory name, compares all
  351 function-definition bodies and 119 retained contracts, and rejects phase
  includes/foreign WN context in the schema. Four tokenizer tests pass.

Run the extractor into a fresh directory:

```sh
python3 doc/tools/dsl_ir_ownership_inventory.py \
  --root "$PWD" --output /private/tmp/dsl-owner-inventory-fresh
python3 doc/tools/dsl_ir_owner_split_check.py \
  --before-root /path/to/pre-split-source --after-root "$PWD" \
  --output /private/tmp/dsl-owner-equivalence-fresh
```

Both accept `--cxx` and repeatable `--extra-arg` for the host toolchain. The
retained receipt records exact commands; the macOS analysis uses GNU libstdc++
15 headers and existing macOS compatibility includes. Linux linked validation
uses the existing Docker build environment. Neither tool edits compiler code.
Inventory and token equality alone are not runtime certification.

## Validation Results And Limits

This section preserves the original relocation run. The subsequent fixes and
fresh merged-PR results are recorded separately in
[DSL image ownership split and fixes](DSL-IR-OWNERSHIP-SPLIT-VALIDATION.md);
the earlier program/DST failure records below are not the current PR status.

- All 351 compared definitions retain their tokenized bodies. The only allowed
  spelling normalization is the image-summary typedef alias in the printer.
  All 119 retained schema contracts are token-identical.
- Reader/producers, backend executable/shared library, `lw_inline`, physical
  clone harness and native Python extension link. Relocated public DSL symbol
  sets and backend library dependencies are compared independently.
- Six configuration/18 syntax-layout compilations, the complete native syntax
  fixture, linked PU clone, and eight impactful-selector self-tests pass.
- Eight of nine focused native lanes pass before and after: shape/retype with
  forced rollback, runtime ABI, external materialization/rewrite, FHE native
  rewrite, standard lowering, grouped lowering, dead constants and CKKS
  expansion (including REGION/runtime cases and rollback).
- The program-interface fixture fails identically on independently rebuilt
  before/after sources at existing projection-dominance/active-PU checks.
  Default-builder and Python runtime suites also reproduce the existing
  `dwarf_DST_producer.cxx:443` double file-scope assertion on both versions.
  These failures remain visible; they are not fixed in a behavior-preserving
  owner move or reported as passing suites.
- All 17 paired `ir_b2a -st -src` traces are byte-identical, with empty retained
  relocation diffs. Sixteen of 18 `.B` files are identical. The other two each
  differ in one byte in reserved local ST index zero of the empty root table:
  file offsets `0x541` and `0x711`. `New_Scope` reserves that row without
  initialization (`symtab.cxx`), and the existing writer copies the table.
  Relevant DSL rows/sections and every meaningful local ST row remain identical.
  Full raw differences and table attribution are retained, not normalized away.
- The independently rebuilt pre-split reader accepts all 18 emitted post-split
  images. The image from the failing program fixture is diagnostic evidence,
  not certification that its later physical contract passed.

Durable evidence is published under the primary checkout's
`artifacts/review/code-quality/dsl-image-ownership/full-pair-split/`, with
before/after binaries and traces, raw diffs, source snapshots, compile/rollback
logs, source-equivalence receipts, per-file compliance review and checksums.
The review index points to the exact immutable receipt.

No full-model, GPU, timing/performance or static-build result is claimed.
`WHIRL.pdf` is absent; this purely mechanical move makes no representation,
layout or version change and cannot approve a future schema design. Unrelated
legacy comment gaps and unrelated pre-existing worktree changes are preserved.
There is no remaining staged owner slice for this pair. No commit or PR is
created by this batch.
