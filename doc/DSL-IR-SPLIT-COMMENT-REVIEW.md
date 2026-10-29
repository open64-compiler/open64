# DSL IR Split: Comment Contract Review

Scope: all 32 production files in the ownership inventory at `aa6fc25a`.
This follow-up to PR #203 changes production comments only. The user explicitly
deferred the one-statement member-function placement rule; the three remaining
cases are recorded below, not silently certified as compliant or relocated.

## Review Method And Findings

The actual PR checkout's 719-line `AGENTS.md`, applicable ancestor/nested
instruction inventory, workflow skill and validation/retention policies were
read in full before the review. The instruction SHA-256 is
`c88247b15706fc7af7222088f502a5eab1bd1e1630df068550ba45f5c6b9dec5`.
Each declaration, definition, helper, constructor and comparator was checked
against its implementation and callers, not merely against comment presence.
File introductions, controlling design references and non-obvious algorithm
boundaries were also reviewed. Existing names, linkage and ownership remain.

The material corrections distinguish shared metadata from physical active-PU
proof; preflight failure from partially published commit state; count journals
from general exception rollback; digest spelling from payload authentication;
borrowed output lifetime from copied facts; and structured-order checks from
full CFG/dominance proofs. Runtime validation does not prove every return or
initialization property. Program input reuse compares existing counts rather
than every row. Shape retyping compares the explicitly listed descriptor facts,
not every runtime-state or lineage field. Comments now state these limits;
this change does not strengthen or otherwise alter executable behavior.

## Per-File Review

Paths below are repository-relative. “Retained” means reviewed and accurate
without a source edit, not omitted from the review.

| File | Outcome and contract focus |
| --- | --- |
| `osprey/common/com/dsl_ir_image.h` | Retained: schema-only rows, extents, versions and ELF families; no process-local API introduced |
| `osprey/common/ir_image/dsl_ir_image.cxx` | Corrected verifier/snapshot owner references; reviewed append/load validation, optional outputs, paired-image preflight, flags and bounded savepoints |
| `osprey/common/ir_image/dsl_image_service.h` | Corrected implementation owner reference; reviewed mapped row/query/load/save/restore contracts and failure effects |
| `osprey/common/ir_image/dsl_image_checkpoint_internal.h` | Retained: bounded count/flag journal, no generation identity or arbitrary-row restoration |
| `osprey/common/ir_image/dsl_pu_specialize_internal.h` | Distinguished early rejection from shifts retained on final validation failure |
| `osprey/common/ir_image/dsl_lowered_relation.h` | Corrected nonpersistent result/mode ownership and local-value projection terminology |
| `osprey/common/ir_image/dsl_ir_print.cxx` | Removed unsupported owner/alias-proof guarantee from local symbol printing; reviewed trusted-image and partial-output boundaries |
| `osprey/common/util/dsl_strtab_checkpoint.h` | Documented pointer/index checkpoint, borrow/reset limits and hash rebuild, not universal stale-point rejection |
| `osprey/be/com/dsl_ir_rewrite.cxx` | Clarified preflight rejection versus asserted commit invariants; reviewed replacement/retirement and retained temporary allocation effects |
| `osprey/be/com/dsl_ir_rewrite.h` | Documented definition lookup limits and retirement/view transaction preconditions |
| `osprey/be/com/dsl_ir_rewrite_queries_internal.h` | Corrected internal-linkage copies, metadata owner lookup and call-actual lookup's missing independent ABI/TY proof |
| `osprey/be/com/dsl_external_tensor.cxx` | Corrected parser acceptance, snapshot lifetime/generation wrap, digest checks, provenance validation and materialization boundaries |
| `osprey/be/com/dsl_external_tensor.h` | Expanded generated provenance/query validation contracts; reviewed ordinary/typed/generated requests and output effects |
| `osprey/be/com/dsl_pu_interface_verify.cxx` | Distinguished shared record bounds, represented canonical rows and active caller/callee physical validation; no inactive-PU activation |
| `osprey/be/com/dsl_pu_interface_verify.h` | Stated canonical metadata and incomplete runtime/initialization validation boundaries |
| `osprey/be/com/dsl_pu_interface_verify_internal.h` | Corrected TCON bounds predicate versus tensor-kind/payload proof |
| `osprey/be/com/dsl_ir_lower.cxx` | Corrected current-PU identity, scratch/output mutation, native definition/relation lookup and terminal commit/assertion limits |
| `osprey/be/com/dsl_ir_group_lower.cxx` | Corrected backend-free owner, closure/use scans and detached-block consumption; documented final rejection/commit boundary |
| `osprey/be/com/dsl_ir_lower.h` | Specified borrowed requests, detached blocks, output preservation/clearing and Boolean versus asserted failures |
| `osprey/be/com/dsl_ir_lower_internal.h` | Narrowed current-PU and standard-block scan guarantees to actual checks |
| `osprey/be/com/dsl_ir_retype.cxx` | Documented exact descriptor comparisons, conservative auxiliary-family exclusion and recorded type-journal rollback limits |
| `osprey/be/com/dsl_ir_retype.h` | Removed overbroad nonshape-equivalence claim; retained explicit output and postcheck rollback contracts |
| `osprey/be/com/dsl_ir_transaction_internal.h` | Retained: bounded global function identity and `Current_pu` check, not resident locals/tree proof |
| `osprey/be/com/dsl_runtime_interface.cxx` | Corrected first-match lookup, partial scratch, handle class creation, commit limits and post-lowering validation scope |
| `osprey/be/com/dsl_runtime_interface.h` | Retained: global plan versus active-PU apply, late failure/output effects and incomplete CFG/return validation already explicit |
| `osprey/be/com/dsl_runtime_interface_internal.h` | Corrected borrowed lookup, initialization/role limits and parent-block/return scratch contracts |
| `osprey/be/com/dsl_program_interface.cxx` | Corrected fingerprint multiset/cache placement, tie keys, count-only input reuse, commit publication and structured-order initialization checks |
| `osprey/be/com/dsl_program_interface.h` | Removed unsupported physical-retirement and validation-time fingerprint cache claims |
| `osprey/be/com/dsl_program_interface_internal.h` | Distinguished eligibility/cache state from physical local-PU correctness |
| `osprey/be/vho/dsl_ckks_expand.cxx` | Corrected attribute spelling versus semantics, partial scratch, restricted ordering and coordinated rollback/exception boundaries |
| `osprey/be/vho/dsl_ckks_expand.h` | Expanded request/result ownership, early preservation versus late clearing, fatal restoration failure and test-fault lifetime |
| `osprey/be/vho/dsl_ckks_expand_internal.h` | Stated exact persistent fault-stage behavior and reset responsibility |

## Validation And Compatibility

- All 32 files have identical complete non-comment token sequences to the
  accepted producer `aa6fc25a`: 28 comment-edited files and four retained files.
  Four lexer self-tests pass. No API, body, linkage, include, layout, opcode,
  version, ELF section or reader/writer token changes; `.B` compatibility remains.
- Fresh Linux GNU C++98 syntax checks pass for 123 translation units; the six
  target operator/layout configurations pass. Ten impact-selector tests pass.
- Regenerated AST/lexical/ctags inventory: 912 occurrences, 570 method occurrences,
  100 structures (25 persistent); twelve extractor self-tests pass. Hashes,
  source locations and reference sites match the new source comments.
- Scoped structural audit: all 32 architectural comments/design references,
  577 lexical function candidates, zero missing or conditional attachments.
  Lexical candidate counts and 570 AST method occurrences are different scopes;
  neither presence count substitutes for the manual contract review above.
- All 1,521 checksum entries and exact membership of the accepted producer
  receipt were freshly authenticated before cached binary reuse. The complete
  non-comment input proof supports reuse for this comment-only change, not
  certification of a newly rebuilt compiler or new optimization behavior.
- Fresh executions using those certified unchanged binaries: nine focused
  native lanes, view retirement, combined builder and clone pass. The current
  native Python suite runs 292 tests: 291 pass, one explicit ACE-source skip.
  Six checkpoint publication cases are freshly compiled and pass separately.
- Fresh `ir_b2a -st -src` reopens: 112 reader executions over 28 retained
  before/after pairs; all complete raw traces equal their authenticated
  expectations. Original DST source paths are mounted. No normalization hides
  differences. The prior 26 byte-identical pairs and two reserved-ST-zero-byte
  exceptions remain historical producer evidence, not a new binary claim.
- `git diff --check`, complete reviewed-file no-tab checks, generated inventory
  equality and per-file compliance checks pass. The selector has no unknown
  files or unavailable lanes. Its path-only full-S6 suggestion is not rerun:
  the token proof invalidates no producer contract. No full-model/GPU/performance,
  new dependency, release/static or previously unavailable ACE claim is made.

## Deferred One-Statement Rule

User-authorized follow-up, with executable definitions unchanged:

1. `DSL_PROGRAM_RETIRED_FORMAL_INDEX_LESS::operator()` in
   `osprey/be/com/dsl_program_interface.cxx`.
2. `DSL_PROGRAM_RETIRED_CALL_INDEX_LESS::operator()` in the same file.
3. `DSL_IR_LOWER_JOURNAL_LESS::operator()` in
   `osprey/be/com/dsl_ir_lower.cxx` (identified after the initial two-case question).

These need an owner-private header and explicit `inline` in a later code-move
change, with linkage/ownership validation. File-private free functions remain
outside that rule. This report certifies the scoped comment review, not
repository-wide comment coverage or satisfaction of the deferred placement rule.

## Durable Evidence

New immutable local receipt family:

```text
/Users/shinmingliu/open64/artifacts/review/code-quality/dsl-image-ownership/split-comment-review/
  sha256-0f5bc9bb48de52b74c2bdaaafd227066be37daf4f9b212a5d6241806a5b981e0/
```

The digest identifies `evidence/token-equivalence.json`. The receipt retains
before/after source, patch, all per-file/function review entries, syntax/layout
logs, inventory ASTs, exact commands, authenticated dependency identities,
focused artifacts and complete fresh mapped traces. A separate checksummed
final-path replay is indexed only after no-replace promotion and verification.
Prior receipts remain untouched. See `receipt.json` and the review-root indexes
for the final commit and replay identity.

Representative source-interleaved FHE trace:
`cached-replay/current/after/fhe-sync1/fhe_sync1_contract.T`.
It records side-file storage, element `F4`, four elements, 16 bytes and four-byte
alignment, plus logical `OPR_DSLTENSORCONST` and the checksum-bearing side URI.
That textual checksum is metadata, not payload-content authentication.
