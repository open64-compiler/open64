# Impactful Test Strategy

Status: normative development and review policy for Open64 FHE work.

## Problem Statement

The purpose of testing is to detect regressions that a source change can
plausibly cause, with enough independent evidence to support review. Running
the largest available model after every edit is not automatically safer. It
can hide weak focused coverage, delay feedback by an hour or more, and spend
engineering time re-proving behavior that the edit cannot reach.

An **impactful test** is the smallest sound test that crosses the changed
contract boundary. Test selection must be conservative: uncertain impact
broadens the selected set. It must not be indiscriminate: documentation,
auditor, printer, planner, transaction, materializer, and whole-program changes
do not all invalidate the same evidence.

This strategy follows the compiler-testing split used by LLVM: focused IR
regressions exercise transformations continuously, while whole-program tests
form a separate, more expensive certification layer. Open64 keeps its existing
test scripts and Make build; this policy does not require adopting another
build system.

## Mandatory Rules

1. Classify the changed files before starting any test expected to exceed ten
   minutes. Use
   `osprey/be/vho/tests/fhe_impactful_test_selector.py` as the checked-in,
   conservative starting point.
2. Run the smallest selected test that executes the changed contract. Unknown
   production code selects full certification rather than being ignored.
3. Before launching a full-model run, state the invalidated contract, the
   reason focused evidence is insufficient, and the expected duration.
4. Do not rerun an expensive producer when its complete content-addressed
   input fingerprint is unchanged. Reuse its immutable artifact and rerun only
   the downstream printer or auditor that changed.
5. A focused pass does not waive the final milestone certification gate. A
   full-model pass does not replace focused rollback, malformed-input, and
   semantic-oracle tests.
6. Documentation-only and test-auditor-only changes never invalidate a
   previously published binary producer artifact.
7. Every selected-but-unimplemented lane is a visible test-infrastructure gap.
   It must not silently fall back to an unrecorded manual judgment.

## Test Tiers

| Tier | Purpose | Target feedback | Typical evidence |
| --- | --- | --- | --- |
| T0 | Hygiene and affected compilation | under 2 minutes | `git diff --check`, no-tab check, changed translation units |
| T1 | Focused semantic/transaction regression | under 10 minutes | recipe oracle, state transfer, rollback, malformed input |
| T2 | Miniature multi-PU checkpoint | under 15 minutes | real transaction, side assets, mapped reopen, atomic failure |
| T3 | Cached full-artifact inspection | under 10 minutes | existing `.B` to fresh `.T`, independent audit |
| T4 | Full-model certification | expensive/manual | complete ResNet producer, atomic publication, separate reopen |

T2 must contain one representative stride-one Conv, one stride-two projection,
one residual alignment, one composite ReLU, and pool/flatten/linear across a
real caller/callee boundary. It uses the production transaction and checkpoint
lifecycle, but it does not reproduce all 33,367 events. Until that fixture is
implemented, the selector reports `miniature-checkpoint` as a required gap for
materializer and generic-transaction changes.

## Change-To-Test Matrix

| Changed contract | Required per-edit evidence | Final-boundary evidence |
| --- | --- | --- |
| Markdown only | T0 | none |
| Auditor only | T0 plus audit retained `.T` | none |
| `ir_b2a`/printer only | T0 plus retained `.B` to fresh `.T` and audit | compatibility reopen when format-visible |
| Pure recipe or event-plan code | T0 plus focused oracle/negative tests | full model only at milestone or PR head |
| Per-value state transfer | T0 plus focused state and transaction tests | miniature checkpoint |
| One operator materializer | focused planner plus T2 | one T4 run at final PR head |
| Generic native rewrite/image transaction | rollback, mapped reopen, T2 | one T4 run at final PR head |
| Reader/writer/ELF/image layout | mapped malformed/legacy matrix | T4 and previous-reader reopen |
| Checkpoint lifecycle/publication | focused atomic success/failure and T2 | one T4 run at final PR head |
| Make or link wiring | rebuild affected products and symbol boundary | no T4 unless producer bits changed |
| Frontend bridge/capture | affected native/Python capture fixtures | full capture only when graph/artifact changes |
| Unknown production source | selected focused lanes plus T4 | T4 required |

Path matching alone is not proof of impact. The selector combines a reviewed
semantic manifest with Git's changed-file set. Build dependency expansion can
use the existing compiler-generated `.d` files; the current Linux build has
hundreds of them, including every FHE materializer. Header changes therefore
select reverse compile consumers even when the implementation path did not
change directly.

## Content-Addressed Evidence

Expensive evidence is reusable only through explicit receipts:

1. **Producer receipt:** input `.B` hash, `be` hash, provider-manifest hash,
   complete option vector, producer-impact source hashes, output `.B` hash,
   side-asset/report hashes, start/end time, and exit status.
2. **Trace receipt:** producer `.B` hash, `ir_b2a` hash, exact `-st -src`
   arguments, source-file hash, and `.T` hash.
3. **Audit receipt:** `.T` hash, auditor hash, policy/expectation version, audit
   result, and normalized measured census.

Changing an auditor invalidates only the audit receipt. Changing `ir_b2a`
invalidates the trace and audit receipts, but not the producer receipt.
Changing a materializer, native transaction, input artifact, provider
manifest, or producer binary invalidates all three.

## SYNC-6 Measured Baseline

The 2026-10-08 S6-0c closure run demonstrates why the tiers are necessary:

| Evidence | Observed wall time |
| --- | ---: |
| Tail-plan focused test | 1.33 seconds |
| ReLU event-plan focused test | 3.10 seconds |
| CKKS expansion-state focused test | 1.75 seconds |
| Independent complete-checkpoint audit | 4.40 seconds |
| Full ten-PU ResNet materialization | about one hour on amd64 emulation |

The full artifact remains essential at the final producer boundary. It is not
the default response to a planner, test, auditor, printer, or documentation
edit. For the current S6-0c artifact, the independent T3 audit checks ten PUs,
33,367 event/result/state joins, 23 reason-tagged bootstraps, 954 rotation-key
requirements, all operator dispositions, all side assets, and zero live
high-level computations in seconds.

## Selector And CI Contract

The selector reads
`osprey/be/vho/tests/fhe_impactful_test_manifest.json`. It accepts the Git diff
or explicit changed paths and emits selected lanes, reasons, commands, and any
required lane that is not yet automated. The default is informational;
`--check-ready` fails when a selected lane is still marked `planned`.

CI should always run one small selector job and use its output to dispatch
lanes. Do not make path-filtered workflows themselves required checks: a
skipped workflow can remain pending, and platform diff limits can omit a
relevant path. Unknown files cause conservative escalation in the selector.

T1 and T2 lanes may run in parallel. T4 must be serialized per artifact family
and should run once per final PR head, on demand, or nightly on `develop`.
Changing only documentation or downstream evidence consumers must reuse the
matching producer receipt rather than launch T4 again.

## Review Checklist

- Changed files and reverse dependencies are listed.
- Every changed semantic contract maps to at least one focused test.
- Selected tests execute the actual production API, not a duplicate helper.
- Negative tests prove rollback or fail-closed behavior at the changed edge.
- Any reused artifact has a matching producer/trace/audit receipt.
- A T4 run states the invalidation reason and expected cost before launch.
- PR text distinguishes focused evidence, cached-artifact evidence, and fresh
  whole-model certification.

## External References

- LLVM Testing Guide: https://llvm.org/docs/TestingGuide.html
- LLVM `lit` selection and timing: https://llvm.org/docs/CommandGuide/lit.html
- Bazel reverse-dependency query model:
  https://bazel.build/versions/7.2.0/query/language
- GitHub Actions path-filter behavior:
  https://docs.github.com/en/actions/reference/workflows-and-actions/workflow-syntax
