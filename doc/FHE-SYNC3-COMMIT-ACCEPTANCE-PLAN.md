# FHE SYNC-3 Commit Implementation, Verification, and Acceptance Plan

Status: execution plan for review. This document does not close any milestone.

Planning baseline: `ebc6e9a13cf92f41c40405461853d2f648954789`.

Short-term objective: complete only the focused C3 / SYNC-3 review gate without
implementing C4 / SYNC-4 behavior. This checkpoint is not completion of
Architecture Phase 3 or focused milestone M4 in the authoritative v0.10 plan.

## Authority and relationship to the main plan

This plan is subordinate to, and must remain consistent with, the following
sources in this order:

1. `doc/DSC_FHE_Compiler_Architecture_and_Integration_Plan_v0.10.docx` is the
   authoritative architecture and semantic plan. The repository copy has
   SHA-256
   `0018769c26b5a0bcd1bdfcbd85aa97b8bafea381d7640fbb9e2e81b0022013d9`.
   No earlier version is authoritative for this plan.
2. `doc/FHE-CONSOLIDATED-IMPLEMENTATION-PLAN.md` is the focused implementation
   and coordination tracker. It may divide an architecture milestone into
   smaller checkpoints, but it may not override v0.10 semantics, boundaries, or
   completion criteria.
3. `doc/FHE-SYNC3-CONVERSION-CONTRACT.md` provides the currently proposed
   focused C3 conversion semantics, diagnostics, BatchNorm folding, operator
   disposition, approximation contracts, CKKS state, and retained artifacts.
4. `doc/FHE-SYNC3-NATIVE-PLAN-CONTRACT.md` provides the proposed fixed-row
   planning-image layout, native APIs, mapped-image behavior, printing,
   compatibility, and native implementation stages.
5. Repository `AGENTS.md` files provide coding, source-position, testing,
   artifact, and process requirements.

The findings from review range `a0815358..d45a853a` supply corrective evidence,
but they do not replace the main plan. This document supersedes the earlier
candidate commit outline only as an execution and acceptance procedure. It does
not renumber or redefine C0-C8 or SYNC-0 through SYNC-8.

### v0.10 phase boundary and the meaning of SYNC-3

The names in the focused tracker must not be confused with the architecture
phase names in v0.10:

- v0.10 Architecture Phase 3 and focused milestone M4 require the full
  ResNet-20 CNN adaptation, including BatchNorm folding, the
  `common.relu` bootstrap-plus-polynomial conversion, pooling, residual and
  classifier handling, a reviewable report, and execution under the reference
  CKKS path.
- v0.10 Appendix F.1 permits the Markdown implementation plan to narrow an
  architecture milestone into smaller review checkpoints. The consolidated
  plan uses that permission to split preparation and materialization: C3 /
  SYNC-3 freezes and persists conversion decisions and legality evidence, while
  C4 / SYNC-4 materializes the mandatory pre-ReLU bootstrap and approved
  polynomial activation.
- Therefore, closing this focused SYNC-3 is only an intermediate gate on the
  path to v0.10 Phase 3 and M4. Neither this document nor commit 19 may claim
  that v0.10 Phase 3 or M4 is complete. C4 / SYNC-4 remains mandatory, not
  optional or cancelled.
- If a focused tracker or contract conflicts with v0.10, v0.10 wins and
  implementation stops until the subordinate document is reconciled.

### Commit-by-commit v0.10 traceability

The table classifies why each proposed commit is allowed. "Direct" means v0.10
states the deliverable or invariant. "Enabling" means the commit repairs or
implements repository machinery required to produce v0.10 evidence without
violating its boundaries. "Narrowed" means it is a deliberately incomplete
checkpoint permitted by Appendix F.1 and must not be described as completion
of the containing architecture milestone.

| Commit | v0.10 anchor | Classification and loyalty condition |
| --- | --- | --- |
| 1 | Appendix F.1, F.4, and F.5 | Enabling: restores truthful status and review gates; it must name v0.10 as the authority and must not change architecture semantics. |
| 2 | Sections 6.4 and 15.4; Appendix F.2 | Enabling: makes shared tensor/encryption identity deterministic and mapped-image evidence verifiable without creating a separate FHE tensor universe. |
| 3 | Section 15.4; Appendix F.2 | Enabling: failure-atomic publication prevents a failed build from masquerading as a valid retained artifact. |
| 4 | Section 16.1 step 1; Appendix F.2 | Enabling: preserves class-centric PU ownership needed by the complete ResNet-20 capture and later conversion. |
| 5 | Sections 15.4 and 16.1 step 1 | Direct: preserves exact source positions and source-linked review evidence. |
| 6 | Appendix F.3 milestone M1 and F.4 action 2 | Direct: recertifies the full ResNet-20 binary capture before adding conversion behavior. |
| 7 | Section 7.2; Appendix F.2 | Enabling: freezes how rewritten nodes retire while keeping source-domain operators visible through both gatekeepers. No private replacement representation is allowed. |
| 8 | Sections 15.4 and 17.1; Appendix F.5 | Enabling: adds reviewed planning records needed for inspectable reports. Exact storage remains subject to review and may not be invented privately. |
| 9 | Section 15.4; Appendix F.2 and F.5 | Enabling: persists reviewed plan evidence through the existing mapped-image framework with compatibility tests. |
| 10 | Section 15.4; Appendix F.3 | Direct: independently reopenable binary and ASCII evidence is a milestone-exit requirement. |
| 11 | Section 7.2; Appendix F.2 | Enabling: uses logical owner-aware APIs so conversion does not expose or manually decode private physical `OPR_DSL` carriers. |
| 12 | Section 7.2 steps 3-8; Appendix F.2 | Enabling: makes BatchNorm and model-adaptation rewrites atomic while preserving gatekeeper visibility and provenance. |
| 13 | Sections 3 and 7; Section 16.1 steps 3 and 8 | Direct/enabling: provides the FHE option and phase entry required to run the reviewed conversion stage, without starting later lowering. |
| 14 | Sections 7.2 and 15.1 | Direct: implements the mandatory FHE semantic gatekeeper and stable positive/negative diagnostics. |
| 15 | Section 7.2 step 3; Section 16 Phase 3 | Direct: performs source-linked legal BatchNorm folding for ResNet-20. |
| 16 | Section 7.2 steps 4-8; Sections 15.1 and 17.1 | Direct/narrowed: records a reviewed disposition for every operator, but does not yet claim executable CKKS conversion. |
| 17 | Section 7.2 step 4; Sections 11.4 and 15.1 | Narrowed: preserves `common.relu`, its approved approximation contract, refresh reason, and value-state obligations. Materialization is deliberately left to mandatory C4 / SYNC-4. |
| 18 | Sections 15.4 and 17.1 | Direct: emits source-linked conversion, depth, approximation, and diagnostic evidence from actual compiler state. |
| 19 | Appendix F.1 and F.3 | Narrowed: certifies only the focused C3 / SYNC-3 evidence gate. Its subject, status update, and report must explicitly avoid claiming v0.10 Phase 3 or M4 completion. |

This mapping is a necessary planning proof, not proof that an implementation
commit is correct. Each frozen candidate must still pass its own claim,
negative, compatibility, and retained-evidence gates below.

## Non-negotiable repository language gate

Every new or modified item intended to enter the Open64 Git repository must be
written entirely in English and must not contain Chinese text, text in another
language, or emoji characters. This applies to:

- source code identifiers, comments, strings, and diagnostics;
- test names, fixture labels, expected output, and generated textual evidence;
- documentation, manifests, filenames, and directory names;
- commit subjects, commit bodies, branch-visible decision records, and PR text
  copied into the repository.

All newly added textual content and pathnames must use printable ASCII plus
normal line endings. Tabs are prohibited by the repository rules except where
a Make recipe requires a literal tab. A manual language review is still
required because an ASCII check alone cannot prove that prose is English.

The Chinese companion to this plan is review material outside the Git
repository at
`../review-output/a08153-d45a85/next-steps/FHE-SYNC3-COMMIT-ACCEPTANCE-PLAN.zh-CN.md`.
It must never be copied into, staged in, or committed to the Open64 repository.

Before a candidate commit may be accepted, its parent-to-commit diff must pass
all of these language checks:

1. Every added pathname and every added textual line contains only permitted
   ASCII bytes.
2. A human review confirms that all added prose is English.
3. The commit subject and body pass the same ASCII and English checks.
4. `git diff --check <parent> <commit>` reports no whitespace errors.
5. No new tracked binary or generated artifact is accepted unless the main plan
   explicitly requires it; extracted names and metadata from any approved
   binary fixture must also be English.

Any language-gate failure is a submission blocker, even when functional tests
pass.

## Meaning of implement, verify, and accept

Each numbered item below produces one candidate Git commit. The required tests
normally land in the same candidate commit as the behavior they prove. A
documentation-only item contains document consistency checks instead of product
tests.

"Verify the commit" means an independent acceptance cycle against the frozen
candidate hash. It does not normally mean creating a second Git commit that
merely says that verification passed. A verification-only commit is allowed
only when it adds independent tests or evidence and changes no product
behavior; it cannot retroactively make an earlier incorrect claim true.

"Accept the commit" is a review gate, not an empty approval commit. Acceptance
means that the frozen candidate is eligible to be submitted for its planned PR.
It does not authorize an automatic push, merge, or milestone closure.

## Per-commit workflow

Every numbered commit follows this sequence.

### 1. Define the claim before editing

Record the exact main-plan obligation, allowed files and owner, observable
success result, required negative cases, compatibility boundary, and explicit
exclusions. The commit subject must use a verb whose strength matches the
evidence. `Fix`, `enforce`, `complete`, and `certify` are literal claims.

### 2. Build one reviewable candidate

Keep one central behavior per commit. Include focused tests with the
implementation. Do not mix formatting, unrelated warning cleanup, generated
artifacts, or later-stage work into the diff. Product behavior and its tests
must remain buildable at that commit.

### 3. Freeze the candidate hash

Record the full commit hash and parent hash. Review only `parent..candidate` and
the surrounding invariants needed to understand the change. Any amendment or
fix creates a new candidate hash and invalidates the previous verification
record.

### 4. Verify the exact snapshot

Use an isolated detached worktree or archive; never switch, reset, clean, or
stash the user's checkout. Run tests appropriate to the claim:

- repair: the focused test is red on the parent and green on the candidate;
- ownership or validation: positive, negative, collision, and unchanged-state
  tests;
- serialization: write, close the producer, reopen in an independent process,
  validate, and print;
- atomic rewrite: inject failures at preparation, application, and
  postprocessing boundaries and compare pre/post state;
- compatibility: old artifacts, new non-feature artifacts, and the new feature
  artifact;
- certification: exact cardinalities, forbidden behavior, source provenance,
  retained artifacts, and fail-closed tool availability.

When practical, temporarily remove the essential fix from the isolated
candidate and confirm that its focused test fails. A test that remains green
after the behavior is disabled is not proof.

### 5. Record durable evidence

The acceptance record must name the candidate and parent hashes, commands,
toolchain identity, environment, exit codes, test results, retained artifact
paths, SHA-256 values, limitations, and reviewer verdict. Local review evidence
belongs outside the Git repository under:

```text
../review-output/a08153-d45a85/sync3-execution/<ordinal>-<short-hash>/
```

Compiler artifacts required by `AGENTS.md` remain in their designated
host-visible artifact family until the next run replaces them. Large generated
artifacts are not added to Git. A small English decision record or manifest may
be committed only when the governing plan requires it.

### 6. Apply the submission gate

A candidate is ready for submission only when all of the following are true:

- its implementation matches its literal commit claim;
- it advances the governing plan in the required order and stays within the
  current stage;
- focused positive, negative, rollback, and compatibility tests required by
  the claim pass at the exact candidate hash;
- required tools are present and no required check soft-skips;
- retained evidence is reproducible and belongs to the candidate hash;
- the language gate passes;
- the diff contains only the declared scope and preserves user changes;
- self-review assigns `Pass`, with no unresolved P0, P1, or claim-blocking P2
  finding.

`Partial`, `Fail`, missing evidence, or an unavailable required environment
means "not ready for submission." A later repair does not change the verdict of
the earlier hash. After a failed candidate is revised, the complete applicable
gate runs again for the new hash.

After each accepted commit, stop, report its diff, evidence, limitations, and
verdict, and wait for authorization before starting the next numbered item.

## Commit and PR sequence

| PR | Purpose | Commits |
| --- | --- | --- |
| PR-A | Repair reopened main/common invariants | 1-4 |
| PR-B | Repair provenance and recertify SYNC-2 | 5-6 |
| PR-C | Close the SYNC-3 node-retirement contract | 7 |
| PR-D | Implement main/common SYNC-3 infrastructure | 8-13 |
| PR-E | Implement and certify the focused FHE SYNC-3 conversion-planning gate | 14-19 |

### Current PR-E execution status

| Commit | Implementation | Verification | Remaining gate |
| --- | --- | --- | --- |
| 14: semantic gatekeeper | Complete in the FHE-owned branch | Focused semantic contract test passes after rebasing onto PR #123 | Final full-model integration rerun |
| 15: BatchNorm folding | Complete | ReLU-free six-PU certification proves 13 physical definition retirements, 21 context folds, and 42 converted tensors with independent payload verification | Final integration rerun at the accepted PR-E tip |
| 16: operator dispositions | Complete for the ReLU-free positive path; ReLU remains fail-closed | The positive artifact records 36 source and 36 converted dispositions | Composite ReLU dispositions require certified policy evidence |
| 17: ReLU profile and CKKS planning | Exact ACE coefficient profile and identity-bound range collector implemented; model enablement blocked | Empirically approved bytes/stage hashes are interned; deterministic pre-ReLU collection validates all 19 persisted identities, bounds, tail evidence, and manifest hashes | Approved trained checkpoint/CIFAR-10 calibration inputs, main-owned manifest transport, predeclared accuracy acceptance, and concrete OpenFHE state/depth proof |
| 18: reports and diagnostics | Complete for BatchNorm certification and ReLU-policy rejection | Atomic payload/report publication and range-specific `CFHECNN-RELU-003` rejection are retained | Full composite-profile report evidence |
| 19: full ResNet-20 SYNC-3 evidence | Blocked | No ReLU-bearing `.fhe.B` is published | Completion of commit 17 evidence followed by the full acceptance lane |

PR #123 closes the physical composite-profile representation dependency only.
It does not authorize numeric ACE coefficients, default context bounds, or
placeholder CKKS states. The conversion pass must continue to fail closed until
all commit 17 evidence is reviewed as one exact policy tuple.

The current Commit 17 decision package is
`doc/FHE-SYNC3-RELU-POLICY-APPROVAL-PACKAGE.md`, with machine-readable evidence
under `doc/fhe-policy/sync3-relu/`. It is an incomplete model-policy checkpoint,
not a completion claim. The exact coefficient profile is approved and may be
interned; model disposition/context rows remain prohibited until range
evidence exists, and `CFHECNN-RELU-003` remains required.

The identity-bound collector and approval validator are specified by
`doc/FHE-SYNC3-RELU-RANGE-CALIBRATION.md`. Their deterministic fixture validates
the complete 19-context join but is not trained-model evidence. Real binding
also awaits a reviewed manifest path/hash in `VHO_FHE_CONVERT_OPTIONS`.

PR-A must merge first. PR-B then rebases and recertifies SYNC-2. PR-C must be
accepted by both main/common and FHE reviewers before PR-D implementation.
PR-D merges before PR-E rebases and consumes its opaque APIs.

## PR-A: reopened main/common invariants

### Commit 1: `docs: reopen pre-SYNC3 corrective gates`

Implementation:

- Add this execution plan.
- Reconcile the consolidated plan and both SYNC-3 contracts so that they name
  v0.10 as the semantic authority and describe C3 / SYNC-3 as a narrowed review
  checkpoint rather than completion of v0.10 Phase 3 or M4.
- Correct the consolidated plan's SYNC-1, SYNC-2, and SYNC-3 status language.
- Add the pre-SYNC-3 corrective gate and update the coordinated queue.
- Preserve all existing stage numbers, ownership, ABI, and image contracts.

Verification:

- Compare the status, prerequisites, queue, and exit evidence in every affected
  plan section.
- Compare the focused C3 exclusions with v0.10 Sections 7, 11, 15, and 16 and
  Appendix F; confirm that deferred C4 work remains explicitly mandatory.
- Confirm that no sentence claims completion while a reviewed blocker remains.
- Confirm that only documentation changed and that all repository text passes
  the English/ASCII gate.

Ready for submission when:

- the main plan describes SYNC-1 implementation as merged but corrective
  validation reopened, SYNC-2 certification as reopened, and SYNC-3 as blocked
  on those corrections plus the node-retirement contract;
- v0.10 is the declared highest authority and no document equates focused
  SYNC-3 closure with v0.10 Phase 3 or M4 completion;
- no C0-C8 responsibility or later-stage scope changed;
- the diff has no code, ABI, opcode, or mapped-image change.

### Commit 2: `Fix FHE tensor binding v1 identity`

Implementation:

- Require `flags == 0` for version 1 tensor bindings.
- Use `(tensor_ty, encryption_descriptor_id)` consistently in interning,
  lookup, validation, and writing.
- Reject unknown flags before mutation or publication.

Verification:

- Show the new focused test failing on the parent and passing on the candidate.
- Test duplicate insertion of the same key, invalid nonzero flags, lookup,
  malicious mapped duplicates, validation before write, and reopen of a legal
  FHE-v1 image.
- Prove that a failed add/write leaves the managed image unchanged.
- Run `osprey/common/com/tests/dsl_fhe_sync1_image_test.sh` and the applicable
  native syntax/contract suite.

Ready for submission when:

- producer, finder, validator, writer, and reader agree on one version-1
  identity;
- no producer can create an image that its own validator rejects;
- old legal FHE-v1 and non-FHE artifacts still reopen unchanged.

### Commit 3: `Make FHE entry value insertion failure-atomic`

Implementation:

- Validate owner, role, ordinal, descriptor, and range before changing
  `first_entry_value_id` or count.
- Commit table insertion and entry-contract range update as one operation, or
  roll both back.

Verification:

- On an empty contract, reject a foreign-PU value, then immediately validate,
  finalize, write, and reopen the unchanged image.
- Compare serialized and logical state before and after every rejected case.
- Test the legal first insertion, contiguous ranges, duplicate ordinals, bad
  roles, and invalid descriptors.
- Demonstrate parent red and candidate green for the foreign-PU empty-contract
  regression.

Ready for submission when:

- every failure preserves both the table and entry contract exactly;
- legal first insertion and range construction still work;
- the complete SYNC-1 FHE image suite passes.

### Commit 4: `Enforce REGION value PU ownership`

Implementation:

- Resolve REGION ownership through an opaque common API.
- Reject cross-PU value append before selecting a PU or changing a BLOCK.
- Keep Python/frontend code unaware of WN, ST, and mapped-image internals.

Verification:

- Test a legal same-PU append and a foreign-PU rejection.
- Construct different PUs with colliding PU-local `ST_IDX` values and prove the
  foreign value is still rejected.
- Compare tree and image snapshots before and after failure.
- Run the multi-PU artifact, builder contract, native syntax, and relevant
  frontend bridge tests.

Ready for submission when:

- owner identity, not a local numeric coincidence, controls acceptance;
- failure has no tree, image, selected-PU, or registry side effect;
- no public API leaks physical WHIRL representation to Python.

PR-A acceptance requires commits 1-4 to pass individually, the full applicable
native contract suite to pass at the PR tip, and a separate merge-mechanics
review after integration.

## PR-B: provenance repair and SYNC-2 recertification

### Commit 5: `Preserve exact ResNet-20 source provenance`

Implementation:

- Replace fixed `line + offset` synthesis with AST/FX metadata or an explicit
  source map derived from the fixture.
- Propagate file, line, and column to defining WNs, result STs, PUs, callsites,
  REGIONs, external parameters, and implicit parameters through opaque APIs.

Verification:

- Maintain an independent expected source map for the deterministic fixture.
- Assert exact file, line, and column for the stem, five clone bodies, nine call
  contexts, nineteen ReLU contexts, parameters, and results.
- Verify the same positions in an independently generated
  `ir_b2a -st -src` trace.
- Add perturbation coverage so inserting unrelated source lines updates the
  derived locations instead of preserving stale arithmetic offsets.

Ready for submission when:

- no tested provenance assertion is presence-only or merely nonzero;
- the source map and independent trace agree exactly;
- the frontend still uses only opaque native builder capabilities.

### Commit 6: `fhe: recertify ResNet-20 SYNC-2 capture`

Implementation:

- Make PyTorch, the native extension, `ir_b2a`, and all required tools
  fail-closed dependencies of certification.
- Add exact census, forbidden-operation, provenance, process-boundary, artifact,
  and manifest checks.
- Retain the complete host-visible SYNC-2 artifact family.

Verification:

- Assert the plan's exact physical-definition and source-context counts,
  including 6 PUs, 9 callsites, 5 REGIONs, 13 convolution definitions, 13
  BatchNorm definitions, 11 ReLU definitions, and the corresponding 21, 21,
  and 19 source contexts.
- Assert absence of ingestion-created bootstrap, SIHE, CKKS arithmetic, and FHE
  conversion operators.
- Exit Python before an independent process runs `ir_b2a -st -src`.
- Deliberately hide each required tool in turn and confirm that certification
  fails rather than skips.
- Record source hash, commands, toolchain, artifact paths, and SHA-256 values.

Ready for submission when:

- all certification assertions pass without a soft skip;
- the retained `.B`, `.T`, payload, census, options, and diagnostic artifacts
  are complete and reviewable;
- the main task accepts the evidence before SYNC-2 is described as completed
  again.

PR-B acceptance requires a rebase onto merged PR-A, rerunning the focused
provenance test and full SYNC-2 certification at the rebased hashes, and
recording new checksums for regenerated artifacts.

## PR-C: SYNC-3 physical contract closure

### Commit 7: `docs: define DSL node retirement for FHE conversion`

Implementation:

- Select and fully specify either an explicit node delete/remap API with
  provenance remapping or a versioned non-executable source/tombstone record.
- Define result users, producer IDs, dispositions, fold rows, source positions,
  physical counts, consumer traversal, old-reader behavior, rollback, reopen,
  and tree/image consistency.
- Update the conversion contract, native plan contract, and consolidated plan
  consistently. Do not implement SYNC-4 behavior.

Verification:

- Walk the contract through BLOCK-local BN removal, entry-sequence BN removal,
  shared clone contexts, failed rewrite rollback, mapped reopen, old-reader
  behavior, and `ir_b2a` output.
- Check every field, sentinel, identity, range, and API name against the two
  accepted SYNC-3 contracts.
- Confirm that physical-node counts and retained source provenance cannot count
  the same retired node as executable.

Ready for submission when:

- main/common and FHE review accept one unambiguous representation;
- every scenario has defined success, failure, rollback, and inspection
  behavior;
- no implementation must invent an unstated ABI or ownership rule.

PR-D may not start until commit 7 is accepted and merged.

## PR-D: main/common SYNC-3 infrastructure

### Commit 8: `Add FHE SYNC-3 planning image records`

Implementation:

- Add `dsl_fhe_plan.{h,cxx}` with the header and four managed record families.
- Implement initialization, add/intern/get/find/reset, IDs, sentinels, and
  structural validation.
- Preserve the specified 64-byte header, 56-byte disposition row, 64-byte
  approximation row, 64-byte CKKS row, 64-byte BN-fold row, and 8-byte
  alignment.

Verification:

- Compile-time size/alignment assertions and runtime initialization tests.
- Positive and negative tests for sequential IDs, zero sentinels, ranges,
  duplicate identities, unknown bits, reserved fields, and every STR/TCON/DSL/
  FHE/PU/callsite cross-reference.
- Verify approximation interning uses every semantic field except ID and that
  CKKS identity is exactly `(value_id, state_version)`.
- Verify reset releases all managed state and returns an empty valid image.

Ready for submission when:

- every published field and invariant has executable coverage;
- invalid rows cannot become visible through add, intern, find, or validate;
- no pointers or STL-owned objects enter fixed rows.

### Commit 9: `Persist FHE SYNC-3 planning image`

Implementation:

- Add `WT_DSL_FHE_PLAN` and ELF reader, writer, and reset wiring.
- Omit an empty section, use checked size arithmetic, validate before writing,
  and copy mapped rows into managed storage before unmap.
- Leave version-1 `.WHIRL.dsl_fhe` byte layout unchanged.

Verification:

- Round-trip all four row kinds through producer close and independent reopen.
- Test absent section, truncation, overflow, trailing bytes, unknown version,
  capabilities, flags, reserved fields, IDs, and references.
- Reopen old 0.33, non-FHE 0.34, and FHE-v1 0.34 artifacts.
- Compare the existing FHE-v1 section byte-for-byte where the fixture permits.
- Prove copied plan rows remain valid after the ELF mapping is released.

Ready for submission when:

- the compatibility matrix passes and malformed images fail before pass use;
- empty/non-FHE writers emit no plan section;
- existing FHE-v1 readers and images retain their documented behavior.

### Commit 10: `Print and reopen FHE SYNC-3 planning image`

Implementation:

- Add the four stable logical printer headings and row details.
- Add a producer that writes all row kinds, exits, and is consumed by an
  independent `ir_b2a -st -src` process.

Verification:

- Assert headings are absent for an empty image and present exactly once for a
  populated image.
- Verify logical operators, wrapper names, source names, contexts, folded TCON
  evidence, and `<pending>` values.
- Reject output containing physical `OPR_DSL`, mapped offsets, pointers, key
  material, ciphertext bytes, or backend objects.
- Use stable field fragments rather than a brittle whole-file golden when that
  provides equivalent precision.

Ready for submission when:

- independent reopen reproduces all logical records and source links;
- output is stable, complete, English-only, and contains no forbidden internal
  or secret data;
- old/no-plan printer output remains unchanged except for intentionally added
  conditional sections.

### Commit 11: `Add owner-aware native DSL rewrite lookup`

Implementation:

- Add producer-only attachments plus
  `DSL_IR_Image_Find_Definition_Value()` and atomic
  `DSL_IR_Rewrite_Native_Value()` support.
- Validate owner PU, logical operator/version, result ST/TY, operands,
  attributes, source position, and image identity before mutation.
- Preserve node ID, value ID, result ST/TY, source position, and lineage.

Verification:

- Test correct lookup/rewrite and wrong owner, local-index collision, operator,
  version, ST, TY, attributes, operands, and ambiguous/missing record cases.
- Inject failure in prepare, apply, and postprocess and compare WN tree, logical
  image, registry, and metadata snapshots byte-for-byte or field-for-field.
- Verify success copies borrowed templates into normal WN pool storage and no
  borrowed lifetime escapes.

Ready for submission when:

- lookup never guesses from a PU-local index;
- every failed rewrite is observably atomic;
- successful rewrite preserves all stable identities and ownership.

### Commit 12: `Retire folded DSL nodes atomically`

Implementation:

- Implement exactly the node-retirement model accepted in commit 7.
- Update BatchNorm removal, result-user rewiring, producer relationships,
  disposition/fold provenance, PU signatures, call actuals, and tree/image state
  as one transaction.

Verification:

- Test BN retirement in a BLOCK and in an entry sequence.
- Test shared clone contexts, context-specific payload rewrites, and the v1
  divergent-signature rejection.
- Inject failure before and after each mutation boundary and prove complete
  rollback with no dangling value, user, node, or plan reference.
- Reopen the result and run ordinary plus converted-form gatekeepers.
- Count physical executable nodes separately from retained provenance records.

Ready for submission when:

- accepted output contains no executable standalone folded BN;
- all users and context provenance resolve after reopen;
- success and failure both preserve the contract's tree/image invariants.

### Commit 13: `Add FHE conversion options and VHO phase hook`

Implementation:

- Add independently controlled `config_fhe.{h,cxx}` options.
- Insert `VHO_FHE_Convert_Driver()` after optional DSL WOPT/Preopt and before
  `VHO_DSL_Lower_Driver()`.
- Process only FHE-owned options and ignore unrelated Open64 options.

Verification:

- Record phase traces for enabled, disabled, `-O0`, no-FHE-section, and
  unrelated-option cases.
- Assert exactly one invocation at the required point and a true no-op when FHE
  conversion is not requested or no FHE image exists.
- Confirm no early LNO/CG entry and no bootstrap, polynomial, SIHE/CKKS,
  runtime, or generated-C work.

Ready for submission when:

- phase order and option ownership are deterministic;
- disabled/non-FHE compilation behavior is unchanged;
- the hook exposes only the reviewed opaque interfaces required by PR-E.

PR-D acceptance requires commits 8-13 to pass individually, native contract
implementation stages 1-5 to pass at the PR tip, compatibility artifacts to
reopen, and the main/common PR to merge before PR-E rebases.

## PR-E: focused FHE SYNC-3 conversion planning and certification

PR-E must also satisfy
`doc/FHE-SHAPE-AND-ENCRYPTION-STATE-PROPAGATION.md`. Its central correctness
condition is preservation of the exact caller-actual to callee-formal
`DSL_IR_VALUE_ID` relationship. Generic tensor shape propagation runs before
FHE state propagation; neither analysis may substitute shape, names, or local
symbol indexes for callee-specific value identity.

### Commit 14: `Add SYNC-3 FHE semantic gatekeeper`

Implementation:

- Validate entry/configuration, descriptors, tensor bindings, source graph,
  payloads, operator relationships, and options for source and converted form.
- Validate the call-ABI/PU-interface join and run deterministic source-shape
  propagation before accepting FHE semantics.
- Emit the stable `CFHE-*`, `CFHECNN-*`, and `CFHECKKS-*` diagnostic families.
- Keep exact ResNet counts in a certification profile, not generic validation.

Verification:

- Run one minimal accepted fixture and one negative fixture for every diagnostic
  specified by the conversion contract.
- Assert diagnostic code, logical name, owner, source location, remediation,
  and absence of secret/internal data.
- Prove malformed input fails before any valid output image is published.
- Confirm generic validation accepts legal non-ResNet cardinalities.

Ready for submission when:

- every stable diagnostic condition is executable and deterministic;
- source and converted modes enforce their distinct legal forms;
- gatekeeper failure is fail-closed and artifact publication is atomic.

### Commit 15: `Fold ResNet BatchNorm into convolution payloads`

Implementation checkpoint: the FHE branch now derives and rewrites all 13
physical Conv/BatchNorm definitions and materializes 21 context-specific
weight/bias pairs (42 tensors). A ReLU-free six-PU certification fixture
publishes and reopens the converted payload, report, `.fhe.B`, and
`ir_b2a -st -src` `.fhe.T`; an independent double-precision oracle verifies
every folded tensor exactly. Full SecureResNet reaches the separate
the coefficient-policy gate, now superseded by range-specific
`CFHECNN-RELU-003`, and publishes no partial artifact.

Implementation:

- Implement inference-only BN legality and the accepted folding formula.
- Support legal implicit-zero bias and deterministic folded payload keys.
- Write a new converted SafeTensors payload without modifying the source file.
- Rewrite each compatible physical clone once and each source call context's
  actual payloads separately; reject v1 signature divergence.
- Resolve every semantic parameter role to its exact callee formal value;
  never use symbol-name parsing, foreign local symbol tables, or shape-only
  matching.
- Run converted-shape verification after body rewrite and BN retirement; prove
  retained BN-only ABI inputs have no executable uses.

Verification:

- Compare folded weights, bias, and end-to-end Conv+BN output against an
  independent double-precision oracle that does not call production folding
  code.
- Cover bias present/absent, epsilon, channel mismatch, NCHW/OIHW layout,
  unsupported groups/dilation/stride/padding, NaN/Inf, missing keys, bad ranges,
  and dtype/shape mismatches.
- Verify source payload SHA-256 is unchanged and converted keys, ranges, bytes,
  and SHA-256 are deterministic across two clean runs.
- Verify 13 physical definition rewrites and 21 source-context fold rows in the
  certified profile, while generic code contains no fixed fixture counts.

Ready for submission when:

- numerical results meet the declared tolerance for every positive fixture;
- every negative case emits its stable diagnostic and publishes no valid
  converted artifact;
- no executable standalone BatchNorm survives and all provenance remains
  resolvable.

### Commit 16: `Record SYNC-3 operator conversion dispositions`

Implementation:

- Record exactly one accepted disposition for every reachable source node.
- Cover convolution, folded BN, residual add, global average pool, flatten,
  linear/classifier, encrypted output, ReLU, and unsupported rejection.
- Use the existing domain-wrapper registry without allocating a new
  `DSL_OPERATOR` solely for wrapper identity.

Verification:

- Test each supported construct and the unsupported activation, control,
  mutation, and pooling cases.
- Reject duplicate, missing, wrong-owner, wrong-result, unresolved-wrapper,
  wrong-version, and wrong-target rows.
- Traverse the source graph independently and compare its reachable node set to
  the disposition set.
- Assert physical definition and source-context counts are reported separately.

Ready for submission when:

- the set comparison proves one and only one disposition per reachable node;
- wrapper names resolve to their reviewed targets;
- rejected conversion creates diagnostics/report rows but no valid planning
  image row.

### Commit 17: `Record ReLU approximation and CKKS planning state`

Policy checkpoint: PR #123 (`d513ea61`) has merged the append-only composite
profile image and opaque APIs. The ResNet baseline is the Chebyshev sign
profile `ace.chebyshev.sign.7x15x13.depth11.v1`, not a single degree-3
polynomial. Candidate coefficient review may begin in this commit, but accepted
planning rows remain blocked until the coefficient bytes, all 19 context range
bindings, model accuracy, and CKKS state/depth evidence are reviewed. The
compiler continues to fail closed before full-model publication in the
interim.

Evidence checkpoint: the approval package records project approval of the
exact ACE source revision, direct Chebyshev coefficient convention, binary64
bytes, and per-stage hashes from empirical ANT ACE evidence, plus the 19
Open64 context keys. It deliberately records null measurements and a blocked
status rather than treating the deterministic synthetic fixture as calibration
or accuracy evidence. The full-model diagnostic is `CFHECNN-RELU-003`.

Implementation:

- Intern an approved composite profile and its ordered stage contracts, then
  attach each surviving encrypted `common.relu` disposition and exact context
  range binding to that profile.
- Add value-specific versioned CKKS state, alignment groups, pending actions,
  and explicit pending-bootstrap reasons without changing canonical tensor or
  encryption descriptor identity.
- Propagate CKKS state through call edges and operators by exact
  `DSL_IR_VALUE_ID`, after successful shape certification, using the transfer
  rules in `doc/FHE-SHAPE-AND-ENCRYPTION-STATE-PROPAGATION.md`.
- Record required pre-refresh planning only; do not materialize SYNC-4 work.

Verification:

- Validate each stage's coefficient rank and `degree + 1` cardinality, stage
  order `[7,15,13]`, finite range/error, canonical binary64 checksum,
  scale/depth/bootstrap policy, and complete semantic interning.
- Test contiguous state versions beginning at one, duplicate rejection,
  pending sentinels, unknown bits, descriptor scheme/value-class agreement,
  residual alignment, and latest-state lookup.
- Prove all 19 certified ReLU contexts remain traceable while compatible
  contexts may share one semantic contract.
- Assert absence of materialized bootstrap, polynomial activation, SIHE/CKKS
  arithmetic, runtime calls, and new source-level FHE ReLU opcodes.

Ready for submission when:

- every surviving encrypted ReLU has a valid source-linked composite profile
  and positive context range binding;
- every required value has a valid CKKS state and all cross-references reopen;
- the output contains planning evidence only and remains inside SYNC-3.

### Commit 18: `Emit SYNC-3 conversion reports and diagnostics`

Implementation:

- Generate before/after census, BN fold, payload rewrite, ReLU contract,
  residual obligation, CKKS state, accepted/rejected operation, and diagnostic
  tables from actual IR and planning-image state.
- Publish successful artifacts atomically and retain failed-run diagnostics
  without a misleading valid `.fhe.B` filename.

Verification:

- Independently compare report rows and counts with reopened IR, plan tables,
  payload indexes, and gatekeeper logs.
- Run conversion twice from clean inputs and compare normalized reports and
  checksums for determinism.
- Corrupt each input family and verify the expected stable diagnostic, failed
  status, and absence of a published valid output.
- Scan for hard-coded certification prose and fixture counts in generic report
  generation paths.

Ready for submission when:

- every report statement is derived from executable state and cross-checks
  against an independent consumer;
- success and failure artifact sets are unambiguous and complete;
- reports contain no secrets, internal physical encodings, non-English text, or
  emoji.

### Commit 19: `fhe: certify ResNet-20 SYNC-3 planning evidence`

Implementation:

- Add the fail-closed full-profile certification driver and small English
  decision/manifest record required to close only the focused SYNC-3 evidence
  gate.
- Run and retain the complete conversion artifact family.
- Update SYNC-3 status to completed only in the candidate that contains all
  executable certification checks; if verification fails, that candidate must
  not be submitted and its status claim must be revised before a new hash is
  tested.

Verification:

- Execute the exact phase chain:

```text
secure_resnet20.B
  -> ordinary DSL/common gatekeeper
  -> FHE source gatekeeper
  -> VHO_FHE_Convert_Driver()
  -> FHE converted-form gatekeeper
  -> secure_resnet20.fhe.B
  -> independent ir_b2a -st -src
  -> conversion report
```

- Prove exact certified cardinalities, including 13 physical Conv/BN rewrites,
  21 source-context folds, and 19 source-context ReLU associations, while
  keeping those counts out of generic gatekeepers.
- Prove the selected composite profile retains three inspectable ordered
  Chebyshev stages, depth 11, exact coefficient and manifest checksums, and 19
  identity-bound normalization ranges. A candidate-only profile cannot close
  commit 19.
- Prove every target operator has an accepted disposition, every required CKKS
  value has state, no standalone BN survives, and source/context provenance is
  complete.
- Prove absence of materialized bootstrap, polynomial evaluation, SIHE/CKKS
  arithmetic lowering, runtime calls, generated C, OpenFHE provider behavior,
  optimization, and GPU work.
- Reopen old/non-FHE/FHE-v1 artifacts and the new artifact in independent
  processes.
- Deliberately remove every required tool once and confirm fail-closed behavior.
- Retain `.B`/`.T`, original and folded payloads, report, gatekeeper logs,
  negative diagnostics, commands, toolchain identity, source hash, absolute
  host paths, and SHA-256 values.

Ready for submission when:

- every SYNC-3 acceptance check in the consolidated plan and both contracts is
  tied to reproduced evidence from the exact candidate hash;
- all prior commits and PR-tip integration gates remain green after the final
  rebase;
- the independent review verdict is `Pass` with no missing required
  validation;
- the decision record does not claim SYNC-4 or the project's later joint
  definition of done, and does not claim v0.10 Architecture Phase 3 or M4
  completion.

## Merge review and milestone closure

Each merge commit is reviewed separately from its feature commits. The review
must identify both parents, inspect both parent diffs and `--remerge-diff`,
verify expected topic-tree equivalence, and explain any conflict resolution.
The conclusion has two parts:

```text
Merge mechanics: Pass, Partial, or Fail
Integrated feature state: inherited commit and PR verdicts
```

A mechanically clean merge does not rescue a failing feature commit. After
every rebase or merge, rerun the focused tests affected by the new base. At the
PR-E tip, rerun the complete SYNC-3 certification.

SYNC-3 closes only after commit 19 and the merged PR-E tip both satisfy their
exact-snapshot gates, the main task reviews the retained artifacts, and the
English decision record accurately identifies the accepted source and evidence.

## Explicitly deferred work

The following work is prohibited in this focused C3 plan and remains assigned
to later focused checkpoints. Deferral here does not make the work optional:
v0.10 still requires the ReLU bootstrap-plus-polynomial path and reference CKKS
execution before Architecture Phase 3 / M4 can be reported complete.

- bootstrap insertion and polynomial evaluation;
- SIHE/CKKS primitive lowering;
- runtime C ABI lowering, `whirl2c`, and generated C;
- OpenFHE provider integration and encrypted end-to-end execution;
- ReSBM, HPOLY/HPAO, optimized boundary movement, and profitability work;
- GPU, POLY, RNS, device-memory, and architecture-specific work.

Passing a later-stage experiment cannot substitute for any SYNC-3 acceptance
criterion.

## Immediate execution rule

The next implementation action is commit 1 only. After its candidate hash is
frozen, verify it using this plan, publish the acceptance record, report the
result, and stop. Do not begin commit 2 in the same batch.
