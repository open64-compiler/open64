# DSL Rewrite Matching-Owner Completion

Status: remaining PR #203 implementation decomposition validated, 2026-10-28.
Incremental baseline: `90b038bd5ee30e6133c8d651f5a274ef45a901ef`;
PR base remains updated `develop`, `ecbb857fc26a83dd35c7800845b654890f0dffbd`.
This supplements [the original PR validation](DSL-IR-OWNERSHIP-SPLIT-VALIDATION.md),
not a second ownership design or a new binary WHIRL format.

## Completed Boundaries

| Matching owner | Implementation responsibility |
| --- | --- |
| `osprey/be/com/dsl_external_tensor.{h,cxx}` | External references/facts, the single process-local snapshot/generation store, source capture/reset, typed/generated provenance, active-PU materialization and runtime-input source checks. Implementation: 1,731 lines. |
| `osprey/be/com/dsl_pu_interface_verify.{h,cxx}` | Structural runtime-value/TY/TCON predicates, formal-row validation and physical call/formal ABI checks. Implementation: 392 lines. |
| `osprey/be/com/dsl_pu_interface_verify_internal.h` | Four existing structural predicate declarations and the relocated explicitly inline formal comparator. No new exported implementation or state. |
| `osprey/be/com/dsl_ir_rewrite.{h,cxx}` | Native definition lookup, logical rewrite, redirect and retirement only. Implementation reduced from 2,625 to 546 lines. |

All moved executable bodies, public names/signatures/linkage, diagnostics, reset
ordering, active-PU assumptions, rejection behavior and allocation-failure
limits are preserved. Callers still select PU context and optimization policy;
these shared services neither activate another PU nor gain CG dependencies.
The shared private read-only query header remains shared, with no duplicate
snapshot store, forwarding implementation or callback registry.

Every moved function contract was reviewed against the actual code. Comments
now distinguish optional mixed-case hexadecimal checksums from mandatory
lowercase SHA-256 checks, packed TY bounds from descriptor sealing, recorded
formal identity from physical proof, and cleared result arrays from unchanged
outputs. Materialization still has no general allocation-failure journal.
These are documentation corrections, not changed executable contracts.

Both new implementations are included in all nine existing direct-object
source-list consumers and the native Python object's link list. Syntax dispatch,
private-header impact selection and ownership extraction include the new files.
The regenerated inventory has 912 occurrences, 100 structures (25 persistent,
75 nonpersistent) and 570 method occurrences. Four private declarations and
one include guard explain the increase; no persisted contracts were added.

## Fresh Validation

| Gate | Result |
| --- | --- |
| Independent source oracle | All 351 function definitions and 119 retained schema contracts unchanged; exact matching owners asserted for eleven critical entry points; four tokenizer tests pass |
| Inventory reconciliation | Eleven implementation ASTs, lexical/header and ctags reconciliation; twelve extractor tests pass |
| Linked products | Rebuilt `be`/`be.so`, `lw_inline`, reader, builder/shape/clone tools and native Python extension |
| Focused native suites | All nine lanes pass on baseline and current: shape/retype rollback, runtime ABI, program ABI, external rewrite, FHE rewrite, standard/grouped lowering, dead constants and CKKS expansion |
| Additional linked fixtures | Combined builder, physical clone, FHE SYNC-1, three Llama families, production-native, view retirement and three independent two-PU DST programs pass |
| Mapped compatibility | 28 before/after image pairs; 112 independent reopens (both readers on both sets); all 28 full trace pairs identical for both readers |
| Binary equality | 26 pairs byte-identical; two one-byte reserved-local-ST-zero exceptions attributed below, not normalized away |
| Syntax/layout/checkpoint | Complete native syntax, six target operator-layout configurations and six CKKS2C publication cases pass |
| Shared-library boundary | All dynamic defined/undefined symbol sets and `DT_NEEDED` lists unchanged for `be.so` and `lw_inline`; loader closes; no `DSL_Builder_*`, `Json::` or frontend `DST_init_mem` leak |
| Full native Python | 292 run, 291 passed, one explicit skip (`ACE_COMPILER_ROOT` unset), no failures/errors |
| Selector/compliance | Ten selector tests pass, including private verifier header and seven frontend source-list subcases; no unknown changed paths; per-file manual contract review, no added tabs and `git diff --check` |

Baseline tools were authenticated against all 834 checksum entries of the
previous immutable PR receipt before use. Identical fixtures ran against that
baseline and rebuilt current tools. Every image is independently reopened
using `ir_b2a -st -src`, with original producer source mounted at `/open64`.
Raw `.B` pairs, complete `.T` pairs and empty unified diffs are retained under
`evidence/parity/<lane>/<stem>/`. No textual normalization substitutes for raw
comparison. Existing shape-refinement transformation before/after images and
rollback evidence are also retained in their own lane.

The two terminal grouped-lowering fixtures differ at offsets `0x541` and
`0x711`, respectively. ELF section extents and local table headers locate each
byte in the sole reserved ST row zero of the empty second-PU local table.
`New_Scope` reserves that entry without initializing it and the existing writer
copies the table. All other bytes, meaningful ST rows, DSL sections and both
readers' complete dumps match. `parity/reserved-row-attribution.json` retains
the before/after values and independently parsed table ranges.

Representative external-tensor trace evidence contains owner-qualified
`external_tensor_caller` and `external_tensor_callee` formals/calls, sealed
float32 tensor descriptors, folded constants and generated row/mask provenance.
FHE evidence retains a side-file tensor reference with a recorded checksum,
F4 element type, four elements, 16 bytes and four-byte alignment; that dump
alone is not payload-content authentication. DST fixtures retain source
interleave and two PUs per program, without the earlier double-initialization
failure. Both reader versions preserve this evidence exactly.

## Retained Evidence And Reproduction

Durable family (Git-ignored, local host evidence rather than public downloads):

```text
/Users/shinmingliu/open64/artifacts/review/code-quality/dsl-image-ownership/rewrite-owner-completion/
  sha256-7efab6233c9c2feae75a18532d32e409dedfc814b4ccd1bb322450742e60d2ff/
```

The identity is the retained `tools/dsl_builder_contract_test` SHA-256. The
receipt records the committed producer scope, exact source/tool hashes,
baseline identity, commands, raw diagnostics, per-file compliance and results.
Old receipts remain immutable. After no-replace promotion, final-path replay
verifies checksum membership, repeats independent reader comparisons,
combined/clone fixtures and the complete Python suite with read-only retained
inputs and no network. Its separate immutable result is referenced by the
review index; the receipt itself is never edited to add replay results.

Exact build commands appear in `evidence/build-*-final.log` and
`evidence/build-python.log`; focused driver commands are in `run-focused.sh`.
`compare_images.py` reproduces the four-way mapped matrix, and
`evidence/oracle-final-command.json` records host compiler/header arguments.
For example, with source mounted at `/open64` and the configured build at
`/build`, the owning tools are built using:

```sh
make -C /build/osprey/targdir/be -j8
make -C /build/osprey/targdir/lw_inline -j8
make -C /build/osprey/targdir/ir_tools -j8 SKIP_DEP_BUILD=1 \
  ir_b2a dsl_builder_contract_test dsl_shape_refine_contract_test \
  dsl_pu_transaction_contract_test
```

The conservative selector still flags full S6-0c certification for generic
transaction paths. Source-body/schema equivalence and focused mapped/rollback
evidence establish that this source decomposition changes no executable
contract; this pass does not claim or rerun full-model certification. No new
representation is designed (`WHIRL.pdf` is absent), no static/release/platform
matrix beyond the reported checks is certified, and no external ACE execution,
GPU correctness, numerical improvement or performance result is claimed.
Legacy comment gaps outside the touched owner closure remain separate work.
