# S6-0d CKKS2C Detailed Execution Plan

Status: active. PR #178 closed S6-0c at merged `develop` commit `f8d9f29d`.
S6-0d starts from the separately reopenable, gatekeeper-approved
`secure_resnet20.ckks_ops.B`; it does not rematerialize Conv, residual, ReLU,
or tail semantics.

Current checkpoint: **D1 implemented and focused-certified.** The private v1
facade, deterministic event-plan emitter, all-nine-primitive fixture,
fail-closed negatives, generated-C compilation/link, and backend link boundary
pass. D2-D6 remain open; no whole-image private C or S6-0d terminal artifact is
claimed yet.

## Objective

The first `-O0` terminal path emits a provider-private C evaluator directly
from verified CKKS-semantic WHIRL and exits before CKKS-to-POLY. The same
immutable CKKS image separately produces the frozen public ABI-v1 application
surface. These sibling outputs are joined by authenticated source-event,
context, group, and descriptor identity and publish atomically.

Correctness and inspectability take precedence over reducing CKKS levels or
primitive count. Such reductions belong to `-O1+`. An unsupported operation,
state, key, asset, group, or provider capability fails closed; CKKS2C never
falls back implicitly to POLY2C.

Normative companion documents:

- `FHE-SYNC6-ACE-CKKS-C-STAGING.md`
- `FHE-SYNC6-CKKS-ABI-LOWERING-BOUNDARY.md`
- `FHE-SYNC6-CKKS-IR-CONFORMANCE-GATE.md`
- `FHE-RUNTIME-C-ABI-V1-CONTRACT.md`
- `IMPACTFUL-TEST-STRATEGY.md`

## Milestone Testing Plan

This section is normative for every S6-0d commit. The per-commit test lists
below identify semantic cases; this section controls **when** those tests run,
which existing evidence may be reused, and when an expensive model run is
actually justified.

### Testing principles

1. Run
   `python3 osprey/be/vho/tests/fhe_impactful_test_selector.py --base origin/develop`
   before selecting tests. Record the changed files, selected lanes, and any
   unimplemented required lane in the commit or PR report.
2. Run T0 hygiene and every selected focused lane on each commit. Do not run a
   larger lane merely because it exists.
3. Reuse the content-addressed S6-0c artifact for read-only collector, emitter,
   printer, audit, grouping, and driver work. Those changes do not invalidate
   CKKS materialization.
4. A changed auditor invalidates only its audit receipt. A changed emitter
   invalidates generated C and its compile receipt, not the input `.B`. A
   changed public-group join invalidates public generated C and its mock result,
   not the private emitter or S6-0c producer.
5. A test expected to exceed ten minutes may start only after the work report
   states the exact invalidated contract, why focused/cached evidence is
   insufficient, the input hashes, and the estimated duration.
6. Do not run the call-only CKKS facade stub. It is a compile/link fixture, not
   a second runtime mock. Run the existing public ABI mock at D4 and the actual
   admitted ANT adapter only at D6.
7. Failed publication tests use fresh destinations and must prove that neither
   final names nor `.tmp` files survive. A stale-destination test must also
   prove the pre-existing final checksum is unchanged.

### Reusable S6-0c baseline

The accepted producer boundary is the PR #178 ten-PU artifact. Reuse is
permitted only when the local file hashes match this receipt:

| Artifact | Accepted SHA-256 |
| --- | --- |
| `secure_resnet20.ckks_ops.B` | `d7bccc537c422b2266add7437040cac14ef7b33af93c9f5e65eb7ebb5f89876f` |
| `secure_resnet20.ckks_ops.T` | `7f97cca89b20ff1654ff6486010a086d61b9cabe1ec5e8048188045a59dda629` |
| Conv plaintext side payload | `50772c653e1c4a5fbd6ff253a5799d418a4f2fa8f61f55ffa389fae391f24e2e` |
| ReLU plaintext side payload | `725e7d741be3ac8b58c624aae6871d79e468fe7eb81d7cde5f2bfc02f29372ca` |
| Tail plaintext side payload | `44d0ca870a1c879c95ba36886fef26fcef640e6c77286b99ea6d9319ebead81c` |
| Independent audit JSON | `d47f51c9626e2e3d16f21e578df6b4467147c8b87ca27c258179532991d55074` |

The retained local family used during development is
`/private/tmp/open64-fhe-sync6-fresh-pipeline/final-s6-0c-v8/`. A missing local
copy is an artifact-retrieval problem, not automatic permission to rerun the
one-hour producer.

Fresh S6-0c materialization is required only when a change can alter the input
CKKS graph or its persisted contracts: a CKKS materializer, expansion/native
transaction, CKKS event/state/key/image writer, source capture, authenticated
plaintext asset producer, or input model/configuration change. Changes confined
to CKKS2C collection, C emission, output publication, public-group joining,
driver selection, facade implementation, tests, auditors, printers, or docs do
not trigger it.

### S6-0d lanes and budgets

| Lane | Purpose | Expected budget | When it runs |
| --- | --- | ---: | --- |
| S6D-T0 | Selector, `git diff --check`, no-tab scan, JSON/schema checks, affected compilation | under 2 minutes | Every commit |
| S6D-T1 | Deterministic primitive emitter, malformed plans, generated-C compile/link | under 2 minutes | D1 and emitter/facade changes |
| S6D-T2 | Focused two-PU image-to-plan collector with mapped reopen and malformed joins | under 10 minutes | D2 and collector/image-query changes |
| S6D-T3 | Miniature multi-PU checkpoint and atomic auxiliary publication | under 15 minutes | D3-D5 and lifecycle/grouped-transaction changes |
| S6D-T4 | Cached complete S6-0c `.B` collection/emission/audit; no rematerialization | target under 10 minutes | D2-D5 at review boundaries |
| S6D-T5 | Public ABI-v1 generated C compile/run against the existing SYNC-5 mock | target under 10 minutes | D4 and public-group changes |
| S6D-T6 | `openpy` option propagation and exactly-one-exit miniature lane | target under 15 minutes | D5 and driver/config changes |
| S6D-T7 | ANT facade compile, capability admission, failure containment, resource/key policy | environment-dependent; predeclare if over 10 minutes | D6 adapter changes |
| S6D-T8 | Complete S6-0d ResNet codegen/publication/reopen certification | expensive and serialized; only after explicit impact proof | A change invalidates the matching complete-certification receipt, or closure has no matching receipt |

S6D-T8 consumes the accepted `.ckks_ops.B`; it is not another S6-0c
materialization. Full ciphertext client/server inference is a separate provider
acceptance run and must not be bundled into every compiler PR.

A PR head, release candidate, or D6 label does **not** by itself select S6D-T8.
Before selecting it, the change report must identify at least one altered input
to the complete-certification receipt and explain why T1-T7 plus cached T4
cannot establish the affected property. Qualifying inputs include the complete
all-PU collector/emitter behavior, cross-PU group join, atomic terminal
publication, `openpy` early-exit orchestration, a facade operation exercised by
ResNet, or an authenticated model/asset/configuration input. Documentation,
tests, auditors, printers, diagnostics, compile-only declarations, or a focused
operation not exercised by the retained model do not qualify.

S6-0d closure requires a valid S6D-T8 receipt, not necessarily a newly executed
S6D-T8. The receipt may be inherited when its input `.B`, side assets, compiler
and adapter binaries, option vector, relevant production-source hashes, and
expected census all match. The closure report must show that comparison.

### Commit-to-lane matrix

| Commit | Mandatory lanes | Explicitly not run by default |
| --- | --- | --- |
| D1 facade/emitter | S6D-T0, S6D-T1, affected `be.so`/`be` link and forbidden-symbol check | T2-T8; no ResNet producer |
| D2 image collector | S6D-T0, S6D-T2, S6D-T4, affected backend build | T3, T5-T8; no ResNet producer |
| D3 publication | S6D-T0, S6D-T3, cached S6D-T4 publication census | T5-T8; no ResNet producer |
| D4 public ABI join | S6D-T0, S6D-T3, S6D-T4, S6D-T5 | T6-T8; no ResNet producer |
| D5 driver exit | S6D-T0, S6D-T3, S6D-T4, S6D-T5, S6D-T6 | T7-T8; no ResNet producer |
| D6 ANT/closure | S6D-T0 plus affected T1-T7; S6D-T8 only when the impact proof shows that the current receipt is absent or invalidated | Do not rerun T8 merely because this is D6, a PR head, or a closure review |

### Planned test commands

The command is mandatory once its owning commit introduces the production
path. A missing script remains a visible selector status of `planned`; it may
not be replaced silently with a full model run.

| Lane | Command or artifact action |
| --- | --- |
| S6D-T0 | `python3 osprey/be/vho/tests/fhe_impactful_test_selector_test.py`; selector command above; `git diff --check` |
| S6D-T1 | `sh osprey/be/vho/tests/fhe_ckks2c_emit_test.sh /private/tmp/open64-fhe-impactful/ckks2c-emitter` |
| S6D-T2 | `sh osprey/be/vho/tests/fhe_ckks2c_collect_test.sh /private/tmp/open64-fhe-impactful/ckks2c-collector` |
| S6D-T3 | `sh osprey/be/vho/tests/fhe_ckks2c_checkpoint_test.sh /private/tmp/open64-fhe-impactful/ckks2c-checkpoint` |
| S6D-T4 | Verify accepted hashes, reopen cached `.B`, emit private C/report, compile, and run the independent complete-image audit |
| S6D-T5 | `sh osprey/be/vho/tests/fhe_ckks2c_group_join_test.sh /private/tmp/open64-fhe-impactful/ckks2c-group-join` |
| S6D-T6 | `sh osprey/driver/tests/openpy_fhe_ckks2c_exit_test.sh /private/tmp/open64-fhe-impactful/ckks2c-driver` |
| S6D-T7 | `sh osprey/libopen64fhe/tests/fhe_ckks2c_ant_admission_test.sh /private/tmp/open64-fhe-impactful/ckks2c-ant` |
| S6D-T8 | When selected by documented impact proof, run the retained `openpy -O0 -keep -FHE:codegen=ckks2c` ResNet command, followed by separate-process `ir_b2a -st -src`, C compile/link, ABI-mock comparison, hashes, and negative cleanup audit; otherwise verify and cite the matching receipt |

### Per-commit report template

Every S6-0d checkpoint report must include:

```text
Changed contract:
Selected lanes and selector output:
Reused artifact hashes:
Fresh producer required: yes/no, with reason:
Commands and wall times:
Positive evidence:
Negative/rollback evidence:
Retained absolute artifact paths:
Deferred lanes and why:
```

This report is part of review evidence. “All FHE tests passed” without the
selected-lane rationale is insufficient.

## Reviewable Commit Sequence

### D1: Freeze the CKKS2C facade and emitter core

**Code**

- Add the versioned provider-private `open64_fhe_ckks2c_facade.h` contract.
- Add a bounded, deterministic, read-only emitter over certified
  `VHO_FHE_CKKS_EVENT_PLAN` objects.
- Emit one single-output evaluator per group with explicit borrowed source and
  bound lookup, authenticated plaintext asset digest, expected CKKS result
  state, key identity, bootstrap reason, status propagation, result ownership,
  and cleanup.
- Support exactly `ckks.encode`, `add`, `sub`, `mul`, `rotate`, `rescale`,
  `modswitch`, `relin`, and `bootstrap`, version 1. Reject everything else.
- Do not add driver routing, mapped-image mutation, POLY operations, or a
  ciphertext runtime implementation.

**Tests**

- Deterministic output and unchanged caller output on rejection.
- One fixture covering all nine primitives, source and bound operands, state,
  keys, authenticated assets, and pre-ReLU bootstrap.
- Compile and link the generated C against a call-only facade stub. Do not run
  this stub as a second mock runtime.
- Reject an unsupported operator and a mismatched result-state attribute.
- Rebuild `be.so` after the focused lane because Make wiring changes; verify no
  `DSL_Builder_*` or `Json::` backend symbols.

**Exit**

The checked generated C is a deterministic product of a valid event plan and
cannot compile with an undeclared primitive. It is not yet a whole-image
CKKS2C artifact.

### D2: Collect exact groups from reopened CKKS WHIRL

**Code**

- Add a read-only all-PU collector joining `DSL_CKKS_EVENT_RECORD`, logical
  node/value/operand/attribute rows, value-specific state, key requirements,
  external tensor references, source identities, and callsites.
- Reconstruct each group in step order and compare its canonical plan bytes to
  the retained materialization evidence.
- Derive stable C symbols from authenticated group identity, never transient
  table allocation alone.
- Keep the complete input image immutable.

**Tests**

- Focused multi-PU event image with direct, called, source, prior-step, bound,
  and external-asset operands.
- Missing/duplicate/reordered step, wrong owner/context, bad final result,
  state mismatch, unknown key, bad asset checksum, and changed ID allocation
  negatives.
- Cached S6-0c `.B` to private C generation plus a deterministic census and
  source/event/state/key report. No full materializer rerun is required.

**Exit**

Every emitted function maps to one complete certified group, and all 33,367
events are consumed exactly once without changing the input `.B`.

### D3: Add transactional private-output publication

**Code**

- Register private C, facade manifest, group join report, and compile commands
  as checkpoint auxiliary artifacts.
- Validate all PUs and the complete emitter census before publication.
- Publish auxiliaries with the established no-replace transaction and retain
  the binary/checkpoint commit marker last.
- Keep generated C free of direct file I/O and embedded tensor arrays.

**Tests**

- Focused success, stale destination, write failure, compile failure, missing
  PU, and signal/abort cleanup.
- Prove no final or temporary artifact survives any failed prepublication
  gate.

**Exit**

The complete private CKKS evaluator family and its provenance report publish
as one transaction.

### D4: Build the CKKS-group to public ABI-v1 join

**Code**

- Join specialized CKKS groups back to canonical six-PU source definitions,
  87 static callsites, and 147 execution-weighted visits.
- Reuse the checked standard-call construction from SYNC-5 after proving one
  descriptor-selected public semantic event owns each private group.
- Consume the main/common grouped lowering transaction once its exact contract
  is merged. Do not lower each primitive into a public call.
- Apply the reviewed terminal CKKS-event image disposition; FHE code must not
  clear or reinterpret mapped rows itself.

**Tests**

- Miniature multi-PU checkpoint and complete cached-image join audit.
- Reject duplicate/escaping group outputs, altered descriptors, wrong source
  order, changed multiplicity, and any public primitive call.
- Generated public C must compile/link/run with the existing ABI mock and match
  the SYNC-5 schedule and observable result identity.

**Exit**

Private C and public ABI-v1 C are independently derived siblings with a complete
authenticated join and no ABI change.

### D5: Add the continuous backend selector

**Code**

- Add the reviewed `-FHE:codegen=ckks2c|poly2c` contract and propagate it
  through normal `openpy` option handling.
- Select exactly once after the CKKS conformance gate. `ckks2c` emits and exits;
  `poly2c` continues toward CKKS-to-POLY when that path exists.
- The initial `openpy -O0` production invocation selects `ckks2c` explicitly.
- Never retry the other route after failure.

**Tests**

- Option parsing/forwarding with unrelated Open64 options.
- Exactly-one-exit traces, no POLY entry under CKKS2C, and no fallback on an
  unsupported primitive or provider capability.
- `openpy -keep -O0` retains the CKKS input, private/public C, reports, logs,
  and hashes.

**Exit**

The driver exposes one continuous CKKS-to-C/POLY staging path with the reviewed
early exit.

### D6: Admit the ANT adapter and close S6-0d

**Code**

- Implement the facade over an immutable, capability-audited ANT ACE pin.
- Keep context, asset, ciphertext, and evaluation-key import explicit and the
  server secretless.
- Map all facade operations, including context-specific bootstrap target levels
  15/17/18, without hidden state repair.

**Tests**

- Compile the complete evaluator and public application.
- First compare the public application through the existing Open64 ABI mock;
  run the ACE-shaped adapter only at this terminal provider gate.
- Certify capability admission, key/bootstrap support, resource cleanup,
  failure containment, and absence of server secret material.
- Retain `.ckks_ops.B`, `ir_b2a -st -src` `.T`, both C families, side assets,
  reports, commands, diagnostics, and hashes.

**Exit**

S6-0d is complete when `openpy -O0` selects CKKS2C, the complete ResNet-20 C
family publishes atomically, the frozen public ABI schedule agrees with
SYNC-5, and all negative failures leave no valid-looking output. Full encrypted
client/server numerical acceptance remains the following SYNC-6 provider
milestone unless performed as part of this gate.

## Ownership And Coordination

| Area | Owner |
| --- | --- |
| CKKS operation legality, plan reconstruction, private emitter semantics, facade consumption | FHE task |
| `openpy`/backend selector and phase order | main/driver, jointly reviewed |
| Generic grouped native lowering and mapped-image terminal policy | main/common |
| Frozen public ABI-v1 descriptors and mock equivalence | FHE task using merged SYNC-5 contracts |
| ANT capability admission and adapter | FHE task with explicit provider review |

No S6-0d commit may allocate an opcode, change canonical tensor identity,
change a mapped row, or broaden the public C ABI without a separate reviewed
main/common contract.
