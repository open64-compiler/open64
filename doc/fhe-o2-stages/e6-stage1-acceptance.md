# O2-E6 Stage 1 Integration and Acceptance Detailed Execution Plan

Status: Proposed detailed plan; certification is locked until all entry gates are accepted

Plan version: 0.1

Date: 2026-09-15

Engineering stage: `O2-E6`

Governing milestones: final `S1.LAYOUT` and `S1.9`

Exit gate: `O2-E6-EXIT`

## 1. Authority and Metadata

### 1.1 Governing authority

This plan is subordinate to the following authorities, in order:

1. Explicit accepted user decisions and repository invariants in `AGENTS.md`.
2. The accepted master architecture and its accepted successor or amendment.
3. Accepted architecture decision records within their stated scope.
4. `doc/FHE-O2-ACE-RESBM-METAKERNEL-INTEGRATION-PLAN.md`, version 1.6.
5. `doc/FHE-O2-DETAILED-EXECUTION-PLAN.md`, plan set version 0.2, for navigation.
6. Accepted detailed plans and exit bundles for O2-E0 through O2-E5C.
7. This detailed stage plan.
8. Locked papers and source artifacts within their declared claim boundaries.

The currently recorded master baseline is commit `ee1dc6382246c58f49a3097157a8c4e8ff2440c8`.

The recorded master Markdown SHA-256 is `7EF644E9BECEFB541102514B83656C78756ECA71398A771A2A5160FC10B0CB25`.

The recorded master DOCX SHA-256 is `0018769C26B5A0BCD1BDFCBD85AA97B8BAFEA381D7640FBB9E2E81B0022013D9`.

The pending ownership decision is `doc/adr/FHE-O0-O2-PLANNER-OWNERSHIP.md`.

The accepted replacement master and ADR identifiers, versions, commits, and hashes must replace pending references before certification begins. A pending or contradictory authority row makes E6 `Unverified`.

### 1.2 Stage role and ownership

O2-E6 is certification-only. It begins after O2-E5C and introduces no production algorithm, support extension, record semantics, runtime mapping, or optimization.

A defect found here returns to its owning E0-E5C stage. The corrected stage and every transitively affected evidence bundle must be accepted again before E6 restarts from clean roots.

The proposed owner is the O2 Stage 1 integration and release owner.

Required independent reviewers are:

- architecture and O0 qualification;
- common/com serialization and compatibility;
- MetaKernel and slot-oracle correctness;
- CKKS state and cryptography;
- ReSBM and raw-graph-oracle correctness;
- materialization and backend verification;
- ACE rtlib/ANT runtime and security;
- build and `be.so` dependency closure;
- correctness and numerical quality;
- cost model, performance, and statistics;
- evidence completeness and release decision.

No producer may be the sole reviewer of its own evidence.

### 1.3 Entry gate

Certification may begin only when:

- `PRE-O2-LOCK-EXIT=Accepted` is current;
- `O2-O0Q-001=Qualified` is current;
- `O2-E1-EXIT=Accepted` through `O2-E5C-EXIT=Accepted` are all current;
- every consumed bundle independently reopens and passes its own verifier;
- the full accepted O0 baseline and Stage 1 support matrices are available;
- the pinned test host and ACE rtlib/ANT provider are available;
- the Stage 1 protocol is accepted before any measurement;
- no authority, qualification, source, schema, runtime, cost, or protocol lock is stale.

Planning text may be reviewed before these gates close. No certification run may be accepted, and no E7A or E7B work may start, before E6 exits `Accepted`.

### 1.4 Consumed evidence IDs

The E6 manifest binds at least:

- `O2-O0Q-001`, `O2-S10-001`, `O2-P2-001`, `O2-S11-001`, and `O2-S12-001`;
- `O2-S13-001`, `O2-S14-001`, and the applicable early `O2-LAYOUT-001` evidence;
- `O2-S15A-001`, `O2-S16-001`, `O2-S17-001`, and `O2-S17-PROTECTED`;
- `O2-S15B-001`, `O2-S18-001`, and accepted E5A/E5B/E5C handoff IDs;
- final `O2-LAYOUT-001` and `O2-S19-001` evidence produced by this stage.

### 1.5 Stale triggers and strict O0 ancestry

E6 stops if any of these changes:

- accepted master or ADR text affecting Stage 1 semantics or ownership;
- the O0 qualification ID or any bound fingerprint;
- Open64, Fhelipe, ACE rtlib, ANT, fixture, model, weight, or dataset revision;
- baseline pass/config/support/protected-DP manifest;
- O2 source, diagnostic, support, oracle, tie-rule, or cost-model lock;
- requested configuration, fixed `N`, or parameter-family definition;
- record schema, capability, serializer, reader, verifier, or normalizer;
- canonical graph, layout, slot, transformed-data, state, placement, or key identity;
- provider public-header allowlist, symbol allowlist, build flags, or library hash;
- runtime-created Q/P/CRT/bootstrap profile behavior;
- machine, toolchain, governor, thread, NUMA, workload, seed, sample, or statistic protocol.

Every profile must descend from the same current qualified O0 ancestry tuple. A stale O0 lock invalidates all candidate plans, comparisons, measurements, and decisions that name it. Targeted requalification is mandatory; no reviewer waiver is permitted.

## 2. Objective and User-Visible Capability

O2-E6 certifies that the implemented Stage 1 MetaKernel and ReSBM increments integrate correctly above the complete qualified mature Fhelipe O0 baseline and execute through the accepted ACE rtlib/ANT public C boundary.

The decision is exactly one of:

- `Accepted`: all required gates passed and the report states `O2 Stage 1 accepted`;
- `Rejected`: an accepted contract was violated and the defect is assigned;
- `Unverified`: authority, environment, provider, evidence, measurement, or review is missing.

Accepted unlocks independent O2-E7A FHEFusion and O2-E7B HPOLY/HPAO work. It does not accept complete O2, bare `-O2`, HPAO-MD, or O3.

## 3. Scope and Boundaries

### 3.1 In scope

- Revalidate every entry fingerprint before each campaign.
- Build Open64 and pinned ACE rtlib/ANT inputs from clean roots.
- Prove one strict qualified O0 ancestor for every profile.
- Run baseline, layout, component, combined, increments-off, and whole-fallback profiles.
- Finish the full `S1.LAYOUT` census and comparison.
- Isolate layout selection, MetaKernel, and ReSBM factors.
- Run fixed-resolved and auto-remaining families separately.
- Verify serialization, producer exit, independent reopen, and determinism.
- Verify graph, layout, state, placement, action, key, provider, and source identities.
- Verify clear and decrypted correctness against independent oracles.
- Validate the frozen ANT cost model on its pre-split holdout set.
- Run pre-registered paired focused and complete ResNet-20 performance trials.
- Exercise unsupported, stale, corrupt, security, compatibility, and failure paths.
- Verify standard WHIRL, generated C, compile, link, load, execute, and cleanup.
- Verify no private FHE/O2/HPOLY operation reaches `whirl2c`.
- Verify the ACE rtlib/ANT public-header and symbol boundary.
- Verify no server secret key, decryptor, or retained private material.
- Verify no unauthorized ACE rtlib dependency is added to `be.so`.
- Publish one immutable, hash-complete acceptance bundle and E7 unlock decision.

### 3.2 Explicit non-goals

E6 does not:

- add, repair, tune, or replace a production algorithm;
- create a missing E0-E5C implementation or independent oracle;
- widen MVM, Conv, CKKS, loop, call, or provider support;
- change a diagnostic, tolerance, threshold, or protocol after observing results;
- tune a model with holdout cases;
- change requested `N` or silently choose a larger ring dimension;
- introduce FHEFusion, HPOLY, HPAO, HPAO-MD, or O3 behavior;
- claim paper-exact behavior for artifact-backed MetaKernel or ReSBM behavior;
- substitute a reduced Fhelipe or legacy greedy/JIT baseline;
- publish a mixture of O0 and O2 plan ownership after failure;
- add ACE compiler AIR or private compiler headers to generated C;
- add a direct or indirect ACE rtlib dependency to `be.so`;
- persist a secret key, decryptor, or secret-derived private material;
- accept a meaningful `.B` without a same-stem `.T`;
- declare `O2 complete`, `v0.10 O2 complete`, or bare `-O2` accepted.

### 3.3 Inherited invariants

- Binary WHIRL is the process boundary.
- Each producer exits before an independent consumer reopens output.
- Open64 owns CKKS-and-above semantic plan truth.
- ACE rtlib/ANT is a checked one-way execution projection.
- Provider identity is `LIB_ANT`, not `LIB_ACE`.
- Runtime libraries are `FHErt_common` plus `FHErt_ant`.
- Generated C uses no private ACE compiler or AIR ABI.
- Requested `N` exists before canonicalization and never changes; `S=N/2`.
- Canonical `MulCC` maps to `Mul_ciph3` then adjacent `Relin` exactly once.
- Ordinary ciphertext SSA values have two components.
- Manual, protected, and pre-ReLU boundaries preserve identity and state.
- ReSBM replaces only permitted baseline-DP automatic sites.
- No unsupported private FHE/O2/HPOLY node reaches `whirl2c`.
- Logical and ANT-expanded key requirements are verified separately.
- Ciphertext bytes are not a correctness oracle.
- Failure cannot publish a valid-looking partial plan.
- Each meaningful `.B` has a same-stem `.T` from `ir_b2a -st -src`.

### 3.4 Support scopes

| Scope | E6 responsibility |
| --- | --- |
| `shared-o0-o2-comparison-required` | Run O0 and O2 with the same input, fixed `N`, provider, and protocol. |
| `o0-handoff-required` | Revalidate and consume the qualified baseline without reimplementing it. |
| `o2-core-only` | Certify required MetaKernel, ReSBM, and Open64 extension behavior. |
| `o2-extension-only` | Certify only a separately accepted row; otherwise reject it. |
| `fallback-required` | Prove whole-profile return to complete qualified O0. |

## 4. Dependency and Build Graphs

### 4.1 Implementation dependency

```text
current O2-O0Q-001
  -> E1 claims, fixtures, diagnostics, and oracles
  -> E2 records and atomic identity transaction
  -> E3 graph, transfer, effect, and verifier substrate
  -> E4 MetaKernel selected layout and slot proof
  -> E5A canonical pre-ReSBM state
  -> E5B ReSBM core, protected adapter, and actions
  -> E5C final parameters, keys, ANT projection, and execution
  -> E6 isolated integration and Stage 1 acceptance
  -> E7A and E7B may start
```

E6 has no bypass around this graph. A missing gate yields `Rejected` or `Unverified`; it never becomes an E6 implementation task.

### 4.2 Clean build and link discipline

Each campaign uses newly empty build and artifact roots. It may not reuse an unbound object, generated header, cached plan, transformed weight, key manifest, or runtime library.

The build proof records:

- source commits and dirty-state reports;
- compiler, assembler, linker, build system, Python, and libc versions;
- all Open64 configure/build flags and rebuilt targets;
- separately rebuilt `FHErt_common` and `FHErt_ant` targets;
- installed public-header and library hashes;
- `be.so` and known-consumer defined/undefined symbol inventories;
- generated-C compile/link commands and loader resolution;
- absence of an unauthorized `be.so` ACE rtlib dependency;
- absence of private ACE compiler/AIR headers.

An incremental build cannot replace a failed clean build. E6 changes only certification manifests, runners, schemas, and decisions. A production code delta sends the work to the owning earlier stage and invalidates the campaign.

### 4.3 Compiler pass order

Implementation order and pass order are separate:

```text
qualified canonical Open64 FHE graph
  -> FHEFusion disabled
  -> qualified Fhelipe or selected MetaKernel layout
  -> recompute canonical pre-ReSBM CKKS state
  -> import baseline DP and protected-site provenance
  -> optional ReSBM replacement of replaceable automatic sites
  -> verify post-ReSBM state
  -> finalize CKKS parameters, security, profiles, and keys
  -> HPOLY/HPAO disabled
  -> standard WHIRL and generated C
  -> ACE rtlib/ANT execution
```

Pass traces, not commit history, prove this order.

### 4.4 External environment

Acceptance performance uses one pinned Ubuntu 22.04 x86_64 host and one accepted ACE rtlib/ANT manifest. Cross-machine speed ratios are forbidden.

The protocol records machine, OS, kernel, CPU, memory, governor, threads, NUMA, compiler, linker, loader, and runtime hashes.

`ir_b2a` must support `-st` and `-src`, and DST-recorded sources must remain available. Otherwise E6 is `Unverified`.

## 5. Profile and Execution Contract

### 5.1 Required profiles

These are evidence identities, not newly accepted public option spellings.

| Profile | Exact content | Purpose |
| --- | --- | --- |
| `BASE-O0` | Qualified mature Fhelipe layout/lowering/state/rescale and protected baseline DP | Sole ancestry, correctness, fallback, and performance baseline. |
| `LAYOUT-FHELIPE` | Qualified Fhelipe layout followed by the same baseline DP contract | Layout census reference. |
| `LAYOUT-MKR` | MetaKernel layout followed by the same baseline DP algorithm/config/protected policy | Layout-only cell; ReSBM off. |
| `S1-MKR` | MetaKernel on, ReSBM off, same baseline DP algorithm/config | MetaKernel single factor. |
| `S1-RESBM` | Fhelipe layout fixed; ReSBM replaces only permitted automatic sites | ReSBM single factor. |
| `S1-FULL` | MetaKernel plus permitted ReSBM replacement | Combined Stage 1 candidate. |
| `S1-INCREMENTS-OFF` | O2 increments off; all qualified O0 passes and DP remain on | Baseline-preserving off control. |
| `S1-WHOLE-BASELINE-FALLBACK` | Forced supported O2 rejection returns to `BASE-O0` | Failure-atomic fallback control. |

FHEFusion, HPOLY, HPAO, HPAO-MD, and O3 controls are disabled in all profiles.

### 5.2 O0 ancestry ledger

Each profile records:

```text
o0_qualification_id
o0_source_commit
fhelipe_source_pass_config_hash
canonical_input_hash
requested_configuration_hash
fixed_n
protected_site_manifest_hash
baseline_dp_manifest_hash
record_schema_hash
ace_rtlib_revision_and_header_hashes
ant_build_and_library_hashes
workload_and_data_hashes
```

All fields except the explicitly isolated Stage 1 decision must match across a comparison family. Another baseline, stale lock, reduced Fhelipe pass list, or legacy greedy/JIT placement rejects the profile.

### 5.3 Stage and pass isolation

- Layout profiles start from byte-identical canonical input.
- Layout profiles use the same fixed `N`, intent, provider, and protected sites.
- Layout profiles rerun the same baseline DP after their distinct layouts.
- ReSBM is off in layout-only and `S1-MKR` cells.
- `S1-RESBM` holds the qualified Fhelipe layout fixed.
- `S1-FULL` enables only accepted MetaKernel and ReSBM increments.
- Increments-off normalizes to the complete `BASE-O0` contract.
- Fallback publishes only O0 identities plus a frozen fallback reason.
- No placement sites are reused across changed graphs or layouts.
- Unknown frequency is not zero; unreachable zero requires proof.

### 5.4 Parameter families

Every required profile runs separately as:

1. `fixed-resolved`: identical `CKKSResolvedParameterIR`, provider, profile domain, transformed-constant source, and fixed `N`.
2. `auto-remaining`: independently resolve only remaining parameters from identical constraints while fixed `N` remains unchanged.

Auto-remaining never substitutes for fixed-resolved. A larger required `N` rejects the current compilation and may produce only a recommendation diagnostic.

### 5.5 Workload matrix

Required positives include locked Figure 4, MVM1/MVM2, accepted MVM/Conv rotation reductions, 1x1/3x3/5x5 Conv, add/mul/rotate/mask, ReLU, residual joins, phis, fanout, critical edges, accepted calls and zero-level loops, plus complete ResNet-20.

Required boundaries include exact and one-over capacity, protected and replaceable sites, fixed and automatic remaining parameters, zero/nonzero gaps, profile input-range limits, and below/equal/above ReSBM scale thresholds.

Required negatives include unsupported Conv rows, `q_w!=q`, non-uniform/multi-level ReSBM actions, multiplication SCCs, unknown trips/frequency, recursion, indirect calls, stale/corrupt records, provider mismatch, and failure injection.

### 5.6 Correctness and structural gates

| Test class | Hard gate |
| --- | --- |
| Pure double layout/MVM/Conv | `abs_error <= 1e-12`, `rel_error <= 1e-12`; NaN/Inf fail. |
| Focused encrypted | Elementwise `abs_error <= 1e-4`, `rel_error <= 1e-6`. |
| Application logits | Max absolute `<=1e-3`; max relative `<=1e-4` for reference magnitude `>=1e-2`; top-1 exact. |
| Required-zero slots | Decoded absolute value `<=1e-8`. |
| Predicted error | Every observed error is within its persisted bound. |

Nonzero gap/junk sentinels must not contaminate valid or required-zero output. Ciphertext-byte equality is forbidden as an oracle.

Exact structural agreement is required for MetaKernel candidates/slots/ties, ReSBM regions/cuts/actions/costs, fixed `N`, CKKS states, protected sites, logical keys, ANT-expanded keys, Q/P/CRT/profiles, and `MulCC -> Mul_ciph3 -> Relin`.

### 5.7 Cost and performance gates

Cost uses integer nanoseconds and checked/saturating sums. Calibration and holdout IDs are hash-split before fitting.

- For operation medians at least `1 us`, holdout median absolute percentage error is `<=20%` and 95th percentile is `<=35%`.
- For smaller operations, absolute prediction error is `<=2 us`.
- Wrong-order rate is `<=10%` for sufficiently separated holdout pairs.
- Every component count has an independent operational explanation.

Focused tests use 5 warmups and 30 paired samples. Full ResNet-20 uses 1 warmup and 5 paired samples. Pairing alternates `AB/BA` from seed `0x4f325331`. No timeout or outlier is deleted.

Using 10,000 fixed-seed paired resamples, Stage 1 passes performance only when the upper 95% bound for `S1-FULL / BASE-O0` is `<1.05` on every mandatory workload and the geometric mean of workload median ratios is `<=1.00`.

At least one locked MVM and one locked Conv reduce total logical rotations versus `BASE-O0` under fixed parameters. Paper speedups are not thresholds.

## 6. Interface, Provider, and Evidence Contracts

### 6.1 Authoritative inputs

| Input | Owner | E6 use | Mutation rule |
| --- | --- | --- | --- |
| Qualified O0 bundle | O0 producer/E0 qualifier | Sole baseline and ancestry root | Read-only; changed lock requires requalification. |
| Canonical graph/config | P0/E3 | Common input, fixed `N`, intent, lineage | Read-only. |
| Layout and census | E4 | Layout profiles and MetaKernel selection | Read-only. |
| Pre-ReSBM state | E5A | Placement input | Read-only. |
| ReSBM/protected result | E5B | Placement profiles | Read-only. |
| Final CKKS/keys/materialization | E5C | Executable Stage 1 system | Read-only. |
| ACE rtlib/ANT manifest | P1a/P1b/E5C | Provider and projection identity | Read-only/hash-bound. |
| Protocol/cost model | E1/performance owner | Frozen decision rules | Read-only after preregistration. |

E6 executes, compares, and rejects these inputs. It cannot edit or republish semantic plan decisions.

### 6.2 Acceptance manifest schema

The normalized manifest contains:

- schema version, campaign ID, authority hashes, and accepted decisions;
- all consumed exit/evidence hashes and the strict O0 ancestry tuple;
- source commits, dirty-state reports, toolchain, flags, targets, and build hashes;
- support, diagnostics, oracle, cost, and protocol hashes;
- exact profile-to-pass expansion and parameter-family identity;
- requested configuration, fixed `N`, provider ID, and runtime hashes;
- generated-C include and symbol inventories;
- workload/model/weight/dataset identities, seeds, bounds, and sample rules;
- per-profile source, plan, mid-WHIRL, generated-C, executable, and output hashes;
- correctness, structure, security, cost, performance, and first-diagnostic results;
- reviewer identities, decision, date, retention path, and exact wording.

Unknown required fields, duplicate profile IDs, placeholders, and moving references fail closed. The manifest is evidence, not a second plan truth.

### 6.3 Provider and public ABI

The execution provider is exactly:

```text
ACE rtlib public C surface
provider_id = LIB_ANT
libraries = FHErt_common + FHErt_ant
```

Generated C may include only the accepted public allowlist: `common/rtlib.h`, `common/rt_api.h`, `common/common.h`, `rt_ant/rt_ant.h`, `rt_ant/rt_api.h`, and reviewed public ANT CKKS/LPOLY/POLY declarations.

The audit rejects `LIB_ACE`, private ACE compiler/AIR headers, C++ provider classes, STL types, exceptions, and non-allowlisted symbols. Runtime-created parameters must equal the persisted Open64 plan.

Server-evaluation processes receive only public/evaluation material. Secret keys and decryptors remain ephemeral in isolated client/test processes and never enter WHIRL, generated C, manifests, logs, or retained bundles.

### 6.4 Serialization, reopen, and `whirl2c`

For every required profile:

1. Produce and atomically publish source/plan WHIRL in a clean producer.
2. Exit and destroy producer memory.
3. Reopen source and plan in a new process and validate every identity/fingerprint.
4. Materialize standard WHIRL solely from the reopened selected plan.
5. Exit and independently reopen materialized WHIRL.
6. Run WHIRL/DSL/FHE/CKKS verifiers.
7. Generate same-stem `.T` evidence using `ir_b2a -st -src`.
8. Prove no private FHE/O2/HPOLY operation remains.
9. Run `whirl2c`, compile, link, load, execute, and clean up through ANT.

Producer-memory inspection is not reopen evidence. A lower-case phase `.t` trace uses a descriptive non-colliding name before `.T` generation on case-insensitive filesystems.

### 6.5 Determinism and compatibility

Two clean campaigns must match normalized plans, ancestry, diagnostics, transformed-plaintext hashes, logical keys, ANT key manifests/fingerprints, materialized WHIRL, generated C, and normalized outputs. Random ciphertext/key bytes are excluded.

Legacy WHIRL retains accepted behavior. Feature-absent input fabricates no O2 result. Unknown required versions fail closed. `ir_a2b` and `ir_b2a` remain compatibility gates. Provider drift invalidates rather than silently migrates a plan.

### 6.6 Decision schema

| Decision | Meaning | E7A/E7B |
| --- | --- | --- |
| `Accepted` | All entry, hard, numeric, cost, performance, evidence, and review gates pass. | Unlocked. |
| `Rejected` | Implemented input or behavior violates an accepted contract. | Locked; defect returns to owner. |
| `Unverified` | Required authority, tool, host, provider, artifact, measurement, or reviewer is missing. | Locked. |

There is no `Accepted with exceptions` state.

## 7. Detailed Work Breakdown

| Task ID | Concrete work | Dependency | Tests/evidence | Owner/reviewer | Merge/exit rule |
| --- | --- | --- | --- | --- | --- |
| `E6-W01` | Freeze authority, evidence, ancestry, and stale triggers. | Governance | Lock audit | Stage/architecture | No pending or moving input. |
| `E6-W02` | Reopen/verify every E0-E5C bundle. | Implementation | Prerequisite report | Integration/independent | Every gate current. |
| `E6-W03` | Clean-build Open64 and ACE rtlib/ANT. | Build | Logs and hashes | Build/dependency | No unbound reuse. |
| `E6-W04` | Audit `be.so`, consumers, headers, and symbols. | Build/security | Symbol/include reports | Build/security | No unauthorized dependency. |
| `E6-W05` | Expand profiles to exact pass controls. | Pass order | Isolation trace | Integration/pass owners | One factor per cell. |
| `E6-W06` | Prove strict O0 ancestry. | Governance | Ancestry diff | E0/release | All non-isolated fields match. |
| `E6-W07` | Complete Fhelipe/MetaKernel layout census. | Integration | `O2-LAYOUT-001` | Layout/slot oracle | All required rows pass. |
| `E6-W08` | Run `S1-MKR`. | Integration | Plan, DP, output | MetaKernel/oracle | ReSBM absent. |
| `E6-W09` | Run `S1-RESBM`. | Integration | Regions/actions/sites | ReSBM/oracle | MetaKernel absent. |
| `E6-W10` | Run `S1-FULL`. | Integration | Full executable evidence | Stage/all pass owners | Both increments verified. |
| `E6-W11` | Run off and fallback controls. | Failure | Baseline diff/reason | Reliability/O0 | No mixed/reduced plan. |
| `E6-W12` | Run both parameter families and numeric oracles. | Verification | Outputs/error bounds | Correctness/crypto | All tolerances pass. |
| `E6-W13` | Audit state, keys, provider, security, and private ops. | Verification | Structural/secret reports | Runtime/security | Exact and leak-free. |
| `E6-W14` | Run stale/corrupt/unsupported/failure/compatibility cases. | Verification | Diagnostics/rejected images | Reliability/independent | Fail closed atomically. |
| `E6-W15` | Validate cost model on holdout. | Verification | Predictions/counters/errors | Cost/performance | Thresholds pass. |
| `E6-W16` | Run paired focused and ResNet-20 performance. | Verification | Samples/CI | Performance/statistics | Non-regression passes. |
| `E6-W17` | Audit generated C and execute with ANT. | Pass order | C/binary/loader/run | Materializer/runtime | Public ABI and correct output. |
| `E6-W18` | Reproduce from clean roots. | Verification | Two bundles/diff | Evidence/independent | Determinism passes. |
| `E6-W19` | Publish hash-complete bundle. | Exit | Bundle/retention | Release/evidence | No missing artifact. |
| `E6-W20` | Sign decision and E7 unlock. | Exit | Decision/unlock JSON | Review board | Unlock only if Accepted. |

## 8. Proposed Commit and Review Sequence

All commits map to governing groups `S1-LAYOUT` and `S1-14`. No commit contains a production algorithm change.

1. `e6-contract`: manifest schema, ancestry contract, profile matrix, diagnostic references, and initially failing certification tests.
2. `e6-clean-build-audit`: clean-build recording, `be.so` symbol comparison, public-header scan, and runtime hashes.
3. `e6-profile-isolation`: exact baseline/layout/component/full/off/fallback expansion and isolation checks.
4. `e6-correctness-security-compatibility`: positive, negative, compatibility, reopen, generated-C, runtime, and secret audits.
5. `e6-cost-performance`: frozen holdout validator, paired runner, raw-sample schema, and statistic checks.
6. `e6-layout-evidence`: final `O2-LAYOUT-001` assembly and review only.
7. `e6-stage1-evidence`: `O2-S19-001`, signed decision, and E7 unlock only.

Tests land with or before runners. Generated campaigns are not committed unless an accepted contract names a golden. Production fixes land in the owning E1-E5C series and force clean E6 reruns.

## 9. Proposed Verification Commands

All commands below are proposed future target commands. They are exact planned interfaces and do not claim that scripts, fixtures, targets, or artifacts exist.

### 9.1 Entry, ancestry, clean build, and dependency gate

Test ID: `O2-E6-001`

```text
python3 osprey/be/vho/tests/run_e6_stage1_certification.py --mode ancestry-and-clean-build --build-dir build/o2-e6-clean --qualification test-artifacts/o2/O2-O0Q-001/qualification-id.json --manifest testdata/fhe_o2/acceptance-stage1/SHA256SUMS --protocol testdata/fhe_o2/acceptance-stage1/protocol.json --artifacts test-artifacts/o2/O2-E6-001
```

Pass: all entry locks are current; roots begin empty; required targets rebuild; source/runtime hashes match; every profile has one O0 ancestor; `be.so` has no unauthorized ACE rtlib dependency. Zero warmups, one build, 60-minute timeout.

### 9.2 Full layout comparison

Test ID: `O2-LAYOUT-001`

```text
python3 osprey/be/vho/tests/run_fhe_layout_ab_comparison.py --build-dir build/o2-e6-clean --source-lock testdata/fhe_o2/source-lock.json --manifest testdata/fhe_o2/layout-ab/SHA256SUMS --artifacts test-artifacts/o2/O2-LAYOUT-001
```

Pass: identical inputs/intents/provider/sites; same baseline DP after each layout; ReSBM off; exact slot/tensor oracle; complete census; increment-off preserves the mature baseline. Seed `0x4c41594f`, dimensions `1..32`, two structural runs within 300 seconds.

### 9.3 Profile isolation

Test ID: `O2-E6-002`

```text
python3 osprey/be/vho/tests/run_e6_stage1_certification.py --mode profile-isolation --build-dir build/o2-e6-clean --manifest testdata/fhe_o2/acceptance-stage1/SHA256SUMS --protocol testdata/fhe_o2/acceptance-stage1/protocol.json --artifacts test-artifacts/o2/O2-E6-002
```

Pass: all eight profiles match the frozen pass matrix, single-factor cells change one factor, Stage 2 is absent, and O0 ancestry is identical.

### 9.4 Correctness, structure, security, and runtime

Test ID: `O2-E6-003`

```text
python3 osprey/be/vho/tests/run_e6_stage1_certification.py --mode correctness-runtime --build-dir build/o2-e6-clean --manifest testdata/fhe_o2/acceptance-stage1/SHA256SUMS --protocol testdata/fhe_o2/acceptance-stage1/protocol.json --provider LIB_ANT --artifacts test-artifacts/o2/O2-E6-003
```

Pass: parameter families remain separate; structure, state, Relin, slots, protected sites, keys, and numerics pass; generated C compiles/links/runs through public `FHErt_common` and `FHErt_ant`; server has no secret/decryptor; no private op reaches `whirl2c`.

### 9.5 Negative, failure, security, and compatibility

Test ID: `O2-E6-004`

```text
python3 osprey/be/vho/tests/run_e6_stage1_certification.py --mode negative-security-compatibility --build-dir build/o2-e6-clean --manifest testdata/fhe_o2/acceptance-stage1/SHA256SUMS --protocol testdata/fhe_o2/acceptance-stage1/protocol.json --artifacts test-artifacts/o2/O2-E6-004
```

Pass: every negative emits its frozen first diagnostic before complete publication; legacy/feature-absent inputs preserve accepted behavior; fallback publishes no mixed plan.

### 9.6 Cost-model validation

Test ID: `O2-E6-005`

```text
python3 osprey/be/vho/tests/run_e6_stage1_certification.py --mode cost-model --build-dir build/o2-e6-clean --manifest testdata/fhe_o2/acceptance-stage1/SHA256SUMS --protocol testdata/fhe_o2/acceptance-stage1/protocol.json --cost-model testdata/fhe_o2/cost/ant-v1.json --artifacts test-artifacts/o2/O2-E6-005
```

Pass: calibration/holdout split is intact, components are independently explained, and all error/order thresholds pass.

### 9.7 Paired performance

Test ID: `O2-E6-006`

```text
python3 osprey/be/vho/tests/run_e6_stage1_certification.py --mode paired-performance --build-dir build/o2-e6-clean --manifest testdata/fhe_o2/acceptance-stage1/SHA256SUMS --protocol testdata/fhe_o2/acceptance-stage1/protocol.json --provider LIB_ANT --artifacts test-artifacts/o2/O2-E6-006
```

Protocol: seed `0x4f325331`; focused 5 warmups/30 paired samples/20-minute timeout; ResNet-20 1 warmup/5 paired samples/3-hour timeout; alternating `AB/BA`; 10,000 resamples; no deletion.

Pass: every workload confidence bound and geometric-mean gate passes, and raw samples plus mandatory counters are retained.

### 9.8 Governing Stage 1 acceptance

Test ID: `O2-S19-001`

```text
python3 osprey/be/vho/tests/run_o2_stage1_acceptance.py --build-dir build/o2-e6-clean --manifest testdata/fhe_o2/acceptance-stage1/SHA256SUMS --protocol testdata/fhe_o2/acceptance-stage1/protocol.json --artifacts test-artifacts/o2/O2-S19-001
```

Pass: all governing Section 15 gates, accepted S1 gates, full `S1.LAYOUT`, required profiles, focused cases, and complete ResNet-20 pass on the pinned host/provider.

### 9.9 WHIRL and evidence audit

Test ID: `O2-E6-007`

```text
ir_b2a -st -src test-artifacts/o2/O2-S19-001/S1-FULL/materialized.o2.mid.B test-artifacts/o2/O2-S19-001/S1-FULL/materialized.o2.mid.T
python3 osprey/be/vho/tests/run_e6_stage1_certification.py --mode evidence-audit --build-dir build/o2-e6-clean --manifest testdata/fhe_o2/acceptance-stage1/SHA256SUMS --protocol testdata/fhe_o2/acceptance-stage1/protocol.json --artifacts test-artifacts/o2/O2-E6-007
```

Apply the same-stem command to every meaningful `.B`. Pass: source cross-reference works, hashes are complete, no private op reaches `whirl2c`, generated-C/runtime evidence is complete, and secret scan is empty.

### 9.10 Reproduction and decision

Test ID: `O2-E6-008`

```text
python3 osprey/be/vho/tests/run_e6_stage1_certification.py --mode reproduce-and-decide --build-dir build/o2-e6-reproduce --reference test-artifacts/o2/O2-S19-001 --manifest testdata/fhe_o2/acceptance-stage1/SHA256SUMS --protocol testdata/fhe_o2/acceptance-stage1/protocol.json --artifacts test-artifacts/o2/O2-E6-008
```

Pass: the clean reproduction matches deterministic normalized fields, reviewers sign one decision, and `e7-unlock.json` unlocks only for `Accepted`.

### 9.11 Verification matrix

| Test | Inputs | Expected | Bounds/platform | Evidence | Owner/pass rule |
| --- | --- | --- | --- | --- | --- |
| `O2-E6-001` | Authority/O0Q/E1-E5C locks | Clean build and ancestry | One pinned build, 60 min | Build/hashes/symbols | Build/architecture; exact current locks. |
| `O2-LAYOUT-001` | Qualified input/two layouts | Exact census/semantics | `0x4c41594f`, dims 1..32 | Plans/census/`.B`/`.T` | Layout/oracle; all rows. |
| `O2-E6-002` | Eight profiles | One-factor isolation | Frozen matrix | Controls/ancestry diff | Integration; exact expansion. |
| `O2-E6-003` | Focused/ResNet-20 | Structural/numeric/runtime pass | Pinned `LIB_ANT` | Plan/C/binary/output | Correctness/runtime; all hard gates. |
| `O2-E6-004` | Negative/compatibility corpus | Exact fail closed | One per case, 30 min | Diagnostics/rejected images | Reliability/security; no partial plan. |
| `O2-E6-005` | Frozen holdout | Cost gates | Pinned host | Counters/errors | Cost/performance; all thresholds. |
| `O2-E6-006` | Paired profiles/workloads | Performance gates | `0x4f325331`, fixed protocol | Samples/CI | Performance; no deletion. |
| `O2-S19-001` | All Stage 1 hashes | All acceptance gates | Pinned host/provider | Full bundle | Release board; unanimous required signoff. |
| `O2-E6-007` | All WHIRL/C artifacts | Evidence/public ABI clean | `ir_b2a -st -src` | `.B`/`.T`/C/scans | Evidence/security; complete. |
| `O2-E6-008` | Clean reproduction | Determinism/decision | Same environment | Diff/decision/unlock | Independent release reviewer. |

## 10. Negative, Failure, Security, and Compatibility Matrix

| Case | Required behavior | Decision effect |
| --- | --- | --- |
| O0 qualification stale | Stop and require targeted requalification. | `Unverified`; E7 locked. |
| E1-E5C lock changed | Invalidate transitively affected evidence. | No acceptance publication. |
| Another O0 ancestor | Reject profile. | `Rejected`. |
| Off removes Fhelipe pass/DP | Reject reduced baseline. | `Rejected`. |
| Layout-only enables ReSBM | Reject isolation failure. | `Rejected`. |
| ReSBM-only changes layout | Reject isolation failure. | `Rejected`. |
| Stage 2 pass appears | Reject stage contamination. | `Rejected`. |
| Fixed `N` missing/changed | Fail before publication. | `Rejected`. |
| Larger `N` required | Diagnose only; do not mutate. | No O2 plan accepted. |
| Corrupt/stale `.B` | Reject on independent reopen. | No execution. |
| Legacy/feature-absent WHIRL | Preserve accepted behavior; fabricate no O2 plan. | Compatibility pass/fail. |
| Unknown required capability | Reject before use. | No execution. |
| Unsupported row enters core | Reject or whole-profile fallback. | No mixed plan. |
| Unsupported ReSBM/SCC/loop | Reject before ReSBM publication. | No O2 placement. |
| Protected site changes | Reject plan. | No materialization. |
| Relin missing/delayed/duplicate | Reject state/materialization. | No runtime claim. |
| `Mul_ciph` plus explicit Relin | Reject provider mapping. | No acceptance. |
| Provider is not `LIB_ANT` | Reject provider identity. | No release execution. |
| ACE rtlib drift | Invalidate dependent evidence. | `Unverified` pending requalification. |
| Runtime Q/P/CRT/profile mismatch | Reject without plan mutation. | No execution acceptance. |
| Private ACE header/symbol | Reject ABI leak. | `Rejected`. |
| Private FHE/O2/HPOLY op at `whirl2c` | Reject lowering boundary. | `Rejected`. |
| `be.so` gains rtlib dependency | Stop for explicit dependency review. | Not accepted. |
| Server gets secret/decryptor | Reject and quarantine. | `Rejected`. |
| Artifact contains secret material | Reject and quarantine. | `Rejected`. |
| Unknown frequency costed as zero | Reject cost plan. | `Rejected`. |
| Cost/security arithmetic saturates | Reject profitability claim. | No acceptance. |
| Publication failure | Roll back staged output. | No valid-looking `.B`. |
| Fallback retains O2 record/site | Reject mixed fallback. | `Rejected`. |
| Two clean campaigns differ | Reject determinism. | Neither accepted. |
| `ir_b2a -src` unavailable | Do not omit evidence. | `Unverified`. |
| Pinned host unavailable | Correctness may remain evidence only. | `Unverified` for E6. |
| Required sample times out | Retain; never delete. | Evaluated as failure. |
| Cost/performance threshold fails | Preserve raw evidence and assign owner. | `Rejected`. |
| Independent reviewer missing | Preserve unsigned candidate only. | `Unverified`. |

Exact first diagnostics come from the accepted E1 catalog; E6 allocates none.

## 11. Evidence and Retention

The accepted bundle contains at least:

```text
stage1-acceptance-id.json  master-and-adr-lock.json
qualification-reference.json  consumed-exit-bundles.json  o0-ancestry-ledger.json
source-lock.json  support-matrix.json  diagnostics.json
protocol.json  environment.json  clean-build-manifest.json
open64-build.log  ace-rtlib-ant-build.log  runtime-library-hashes.json
be-symbol-diff.txt  be-consumer-link-closure.json  public-header-allowlist-report.json
profile-matrix.json  pass-isolation-report.json  requested-configuration.json
fixed-resolved-family.json  auto-remaining-family.json
BASE-O0/input.fhe.B  BASE-O0/input.fhe.T
BASE-O0/selected-plan.B  BASE-O0/selected-plan.T
BASE-O0/materialized.o0.mid.B  BASE-O0/materialized.o0.mid.T
LAYOUT-FHELIPE/layout-plan.B  LAYOUT-FHELIPE/layout-plan.T
LAYOUT-MKR/layout-plan.B  LAYOUT-MKR/layout-plan.T
S1-MKR/selected-plan.B  S1-MKR/selected-plan.T
S1-RESBM/selected-plan.B  S1-RESBM/selected-plan.T
S1-FULL/input.fhe.B  S1-FULL/input.fhe.T
S1-FULL/canonical.fhe.B  S1-FULL/canonical.fhe.T
S1-FULL/selected-plan.B  S1-FULL/selected-plan.T
S1-FULL/materialized.o2.mid.B  S1-FULL/materialized.o2.mid.T
S1-FULL/materialized.o2.mid.lowering.t
layout-and-slot-map.json  layout-census.json
metakernel-artifact-oracle.json  metakernel-paper-differential.json
ckks-state-trace.json  resbm-regions.json  resbm-actions.json  protected-sites.json
logical-key-requirements.json  ant-key-manifest.json  provider-capabilities.json
transformed-plaintext-manifest.json  generated.c  generated-program
generated-c-includes.txt  generated-c-symbols.txt  build-and-link.log
loader-resolution.json  runtime-execution.log  runtime-cleanup.json
decoded-output.json  oracle-output.json  correctness-report.json
security-report.json  secret-scan.json  negative-and-compatibility-tests.json
failure-atomicity.json  cost-model-validation.json  raw-samples.csv  metrics.json
performance-confidence-intervals.json  normalized-reproduction-diff.json
review-signatures.json  stage-decision.md  e7-unlock.json
SHA256SUMS
```

Every `.T` is made from its same-stem `.B` with `ir_b2a -st -src`. The lower-case `.t` is a phase trace, not a substitute.

Failed evidence is retained at least 30 days, milestone evidence 180 days, and accepted O0/E6 bundles for repository lifetime. Clean the designated artifact root at the start of the next run, never at the end of the current run.

Publication is atomic. Failed runs may retain logs and rejected inputs but no partial artifact under a valid accepted `.B` name.

## 12. Exit Decision

### 12.1 Accepted

`O2-E6-EXIT=Accepted` requires:

- all entry gates and fingerprints current;
- strict common O0 ancestry for every profile;
- clean Open64 and ACE rtlib/ANT builds;
- no unauthorized `be.so` dependency or private header;
- full `O2-LAYOUT-001`, `O2-S19-001`, and every E6 sub-gate passed;
- all single-factor, combined, off, and fallback cells passed;
- fixed-resolved and auto-remaining families separate with fixed `N`;
- zero independent MetaKernel/ReSBM structural mismatch;
- serialization, reopen, compatibility, corruption, and determinism passed;
- all numerical and predicted-error gates passed;
- generated C compiled, linked, loaded, ran, and cleaned up using `FHErt_common`, `FHErt_ant`, and `LIB_ANT`;
- no private FHE/O2/HPOLY operation reached `whirl2c`;
- no server secret/decryptor/private material or artifact secret;
- cost and paired-performance gates passed;
- complete hash inventory, retention, and required signatures.

Exact wording:

```text
O2 Stage 1 accepted.
```

This unlocks E7A and E7B only.

### 12.2 Rejected

`Rejected` names the first failed test/diagnostic, profile, parameter family, ancestry/provider identities, owning E1-E5C stage or E6 runner, retained reproduction, correction, and invalidated evidence.

E7A/E7B remain locked. Corrections require upstream reacceptance and a clean E6 rerun.

### 12.3 Unverified

`Unverified` means required authority, current qualification, toolchain, host, provider, `ir_b2a -src`, reopen, measurement, artifact, or reviewer evidence is missing.

It is not provisional acceptance. E7A/E7B remain locked; no synthetic or narrower workload may replace missing evidence.

## 13. Rollback, Invalidation, and Stop Rules

Rollback disables all Stage 1 increments and returns to complete current `BASE-O0`. It cannot retain a MetaKernel layout, ReSBM site, reduced baseline, legacy JIT placement, stale plan/key/data, changed `N`, or weakened security/provider rule.

Invalidation propagates from changed input to dependent plans, states, placements, keys, C, measurements, and decisions. The manifest records the first changed input, affected IDs, and targeted requalification path.

Stop and return to the named owner or architecture review if:

- O0 ancestry is missing, stale, or non-unique;
- an E1-E5C feature/oracle is incomplete;
- certification needs a production or support-scope change;
- a single-factor cell cannot isolate its pass;
- off/fallback cannot retain full Fhelipe baseline and DP;
- fixed `N` cannot be preserved;
- a private O2/HPOLY operation must reach `whirl2c`;
- generated C needs a private ACE header or symbol;
- execution cannot use exactly `FHErt_common`, `FHErt_ant`, and `LIB_ANT`;
- runtime parameters disagree with Open64 plan;
- exact `Mul_ciph3 -> Relin` cannot be proved;
- `be.so` needs an unapproved ACE rtlib dependency;
- server evaluation needs a secret/decryptor;
- reopen needs producer memory;
- an extension lacks a frozen row/oracle;
- cost/security saturates or uses unknown frequency;
- a threshold would need post-result relaxation;
- a required artifact or reviewer is unavailable.

## 14. Exact E7A and E7B Unlock

`e7-unlock.json` contains the E6 decision/campaign, authority hashes, O0 ancestry, E1-E5C exits, Stage 1 locks, selected/materialized plan hashes, fixed `N`, provider/runtime identity, report hashes, isolation/fallback hashes, reviewers, retention path, and separate E7A/E7B booleans.

Both booleans are true only for `O2-E6-EXIT=Accepted`. E7A and E7B may then proceed independently and in parallel under their own entry gates.

Each E7 consumer must independently reopen E6 evidence, validate every fingerprint, treat Stage 1 as immutable, reject stale E6 before work, preserve fixed `N`/Relin/protected sites/provider, and replan affected downstream state after a legal graph change.

E6 does not authorize HPAO-MD. E7B retains `design-pending, implementation-disabled` until a separate accepted design and plan amendment.

## 15. Requirement Traceability

| Requirement | O2 milestone | Work items | Verification | Evidence |
| --- | --- | --- | --- | --- |
| Complete O0 first/current ancestry | `O2-O0Q-001` | `W01/W02/W06` | `O2-E6-001/002` | Qualification/ancestry ledger |
| E6 only after E5C | `S1.9` | `W02/W10` | Entry audit, `O2-S19-001` | Consumed exits/acceptance ID |
| Preserve Fhelipe baseline | `S1.LAYOUT/S1.9` | `W06/W07/W11` | `O2-LAYOUT-001`, `O2-E6-002` | Pass matrix/fallback diff |
| MetaKernel isolation | `S1.3/S1.4/S1.9` | `W07/W08` | `O2-LAYOUT-001`, `O2-E6-003` | Census/slot oracle |
| ReSBM/protected isolation | `S1.6/S1.7/S1.9` | `W09/W10` | `O2-E6-003`, `O2-S19-001` | Regions/actions/sites |
| Fixed `N`/CKKS state | `S1.5A/S1.5B/S1.9` | `W12/W13` | `O2-E6-003/004` | Config/state/parameters |
| Immediate Relin | `S1.5A/S1.8/S1.9` | `W13/W17` | `O2-E6-003` | State/generated calls |
| Public ANT boundary | `S1.5B/S1.8/S1.9` | `W03/W04/W17` | `O2-E6-001/003` | Headers/symbols/loader/run |
| No server secret | `S1.8/S1.9` | `W13/W17` | `O2-E6-003/004` | Key census/secret scan |
| No `be.so` dependency | `S1.5B/S1.8/S1.9` | `W03/W04` | `O2-E6-001` | Symbol/link closure |
| No private op at `whirl2c` | `S1.8/S1.9` | `W13/W17` | `O2-E6-003/007` | Census/`.T` |
| Same-stem `.B`/`.T` | `S1.LAYOUT/S1.9` | `W07/W19` | `O2-E6-007` | WHIRL families |
| Correctness | `S1.9` | `W12` | `O2-E6-003`, `O2-S19-001` | Decoded/oracle outputs |
| Cost/performance | `S1.9` | `W15/W16` | `O2-E6-005/006` | Errors/samples/CI |
| Three decisions and E7 unlock | `S1.9` | `W20` | `O2-E6-008` | Decision/unlock JSON |

Every accepted row resolves through accepted master section/version/hash, ADR decision, governing milestone/test, E6 work item, exact proposed command, retained artifact, and reviewer decision.

A row containing `pending`, an unknown owner, moving reference, placeholder hash, or stale qualification prevents `O2-E6-EXIT=Accepted`.
