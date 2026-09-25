# O2-E0 Detailed Execution Plan: Complete O0 Baseline Qualification

Status: planning draft; execution is blocked until `PRE-O2-LOCK-EXIT` and all
O0 producer handoffs are accepted

Plan version: 0.1

Date: 2026-09-15

Engineering stage: `O2-E0`

Formal gate: `O2-O0Q-001`

Exit gate: `O2-E0-EXIT`

Allowed terminal results: `Qualified`, `Rejected`, or `Unverified`

## 1. Purpose and Authority

This stage independently determines whether the complete O0 baseline is a
reproducible, executable, and sufficient foundation for O2. It is an O2
consumer qualification of an externally produced O0 bundle. It is not an O0
implementation phase, an O0 repair queue, or the first O2 optimization phase.

`O2-O0Q-001=Qualified` is the only condition that may unlock O2-E1 and every
later O2 implementation stage. Every required qualification row must pass in
one current qualification. A partial pass, waived row, skipped test, provisional
baseline, or `Unverified` result does not unlock any O2 work.

Drafting this document is governance work. Implementing the qualification
harness starts only after the complete O0 producer candidate bundle exists.
No O2 source lock, fixture, independent algorithm oracle, production planner,
record extension, identity plan, or provider-free algorithm work may begin as
part of this stage.

### 1.1 Governing snapshot

| Authority | Recorded snapshot | Qualification use |
| --- | --- | --- |
| Repository rules | `AGENTS.md`, SHA-256 `90375338975E4D0F8BC2747D35506DD4F5EC69139C18B39251DF8B17FFB7255D` | Binary WHIRL, frontend boundary, verifier, `be.so`, evidence, and coding invariants |
| Current recorded master | `DSC_FHE_Compiler_Architecture_and_Integration_Plan_v0.10.md`, commit `ee1dc6382246c58f49a3097157a8c4e8ff2440c8`, SHA-256 `7EF644E9BECEFB541102514B83656C78756ECA71398A771A2A5160FC10B0CB25` | Current architecture authority while the successor is pending |
| Current master companion | `DSC_FHE_Compiler_Architecture_and_Integration_Plan_v0.10.docx`, SHA-256 `0018769C26B5A0BCD1BDFCBD85AA97B8BAFEA381D7640FBB9E2E81B0022013D9` | Companion authority; its accepted successor must agree with the Markdown form |
| Pending decision | `doc/adr/FHE-O0-O2-PLANNER-OWNERSHIP.md`, including `FRZ-01..09` | Must be accepted and hash-locked before this stage starts |
| Governing O2 plan | `FHE-O2-ACE-RESBM-METAKERNEL-INTEGRATION-PLAN.md`, version 1.6; accepted commit and hash required at activation | Sections 0, 2, 4, 6-9, 11-13, 15, 17, and 18 govern this stage |
| Detailed execution index | `FHE-O2-DETAILED-EXECUTION-PLAN.md`, plan set version 0.2 | Non-normative navigation; the O2 plan Sections 12.3-12.5 define stage structure and evidence rules |

The O2 plan and execution-index versions above identify the snapshots used to
draft this plan. `PRE-O2-LOCK` must bind them to accepted committed hashes
documents. It must also replace the current master references with the accepted
successor section, version, commit, and hashes. A pending or mismatched authority
reference prevents `PRE-O2-LOCK-EXIT` and therefore prevents E0 execution.

### 1.2 Formal milestone mapping

| E0 responsibility | Governing milestone or decision |
| --- | --- |
| Consume the complete O0 producer result | `F1-PREP`, `P0`, `P1a`, `P1b`, `F1-IMPL`, `F1-ACCEPT` |
| Qualify the complete bundle independently | `O2-O0Q-001` |
| Preserve the strict start boundary | O2 plan Section 13 and decision `Complete O0 before O2` |
| Validate mature Fhelipe baseline ownership | `FRZ-01..07` |
| Validate ACE rtlib and ANT runtime boundary | `FRZ-08..09`, P1a, and P1b |
| Unlock the next engineering stage | `O2-E0-EXIT`, then O2-E1/S1.0 |

### 1.3 Ownership and reviewer independence

| Role | Responsibility |
| --- | --- |
| O2 qualification owner | Owns the runner, evidence normalization, result classification, and final E0 decision; does not repair O0 product code |
| O0 handoff owner | Supplies the accepted complete producer bundle and repairs O0 defects returned by E0 |
| Common/com reviewer | Verifies authoritative record ownership, compatibility, mapped-image behavior, and independent reopen |
| Runtime and build reviewer | Verifies the ACE rtlib public C boundary, ANT build, generated-C link closure, and absence of an unreviewed `be.so` dependency |
| Cryptography and security reviewer | Verifies fixed `N`, Q/P/CRT/profile agreement, state transitions, key separation, lifecycle mode, and secret scans |
| Independent evidence reviewer | Reproduces the qualification from the immutable input manifest and is not the sole producer signatory for the rows reviewed |
| Architecture reviewer | Confirms that the accepted master/ADR and the qualification inputs remain consistent |

The O0 producer may explain its evidence but may not be the sole reviewer of the
same evidence. The qualification runner author may not be the sole approver of
the runner's result. The final decision records named owner and reviewer
signoffs for every domain.

## 2. Objective, Capability, and Boundaries

### 2.1 Objective

Establish, from an independently reproducible bundle, that all foundations
needed by O2 are present and coherent:

- one accepted mature Fhelipe layout/lowering/state/rescale/protected-DP O0
  baseline;
- deterministic all-PU canonical binary WHIRL with exact slot semantics;
- one authoritative configuration, layout, CKKS state, placement, protected
  site, and logical-key record family;
- a source-locked ACE rtlib public C interface with `FHErt_common` and
  `FHErt_ant`, provider ID `LIB_ANT`;
- exact Open64-to-ANT parameter, action, and key projection;
- executable focused workloads and the locked complete ResNet-20 workload;
- failure-atomic publication, stable rejection diagnostics, backward
  compatibility, and independent process reopen; and
- an immutable O2 consumer input digest that contains no O2 planning decision.

The only capability produced at exit is a signed, reproducible qualification
decision for the exact O0 bundle. In scope are independent replay of all O0
handoffs, identity and structural checks, focused and ResNet-20 execution,
security and compatibility checks, a read-only consumer digest, immutable
evidence, and failure routing.

### 2.2 Explicit non-goals

- Implementing, changing, patching, or narrowing O0 behavior.
- Creating an O2 candidate plan, selected plan, identity plan, or O2 record.
- Exercising P2/S1.1 identity-plan publication or materialization.
- Locking O2 MetaKernel/ReSBM claims, fixtures, diagnostics, cost tables, or
  independent algorithm oracles; those begin in O2-E1.
- Adding an O2 optimizer, private WHIRL operator, new WHIRL image, or new
  backend-to-rtlib dependency.
- Treating `CKKS_PARAMS`, provider memory, or the consumer digest as a second
  compilation truth.
- Accepting a reduced, synthetic, legacy greedy/JIT, or partially disabled
  baseline as a substitute for the complete accepted O0 baseline.
- Claiming `O2 Stage 1 accepted`, `O2 complete`, or master-plan reconciliation.

### 2.3 Inherited invariants

1. Binary WHIRL is the process boundary and uses Open64 mapped-image and ELF
   mechanisms.
2. Every meaningful `.B` has a same-stem `.T` produced by
   `ir_b2a -st -src` after the producer exits.
3. Python may coordinate tests but is not a middle-end runtime dependency.
4. Requested `N` is fixed before canonicalization and is never changed by a
   resolver, baseline pass, provider, or qualification tool.
5. Every canonical `MulCC` has a transient three-component result followed
   immediately by mandatory `Relin`; ordinary ciphertext SSA values have two
   components.
6. The canonical ANT mapping is `Mul_ciph3` followed by adjacent `Relin`.
   Mapping to fused `Mul_ciph` and then emitting another `Relin` is invalid.
7. Manual, policy-mandated, and protected pre-ReLU bootstrap boundaries are
   preserved under the accepted ADR. Baseline DP owns only its permitted
   automatic sites.
8. Generated C uses only the accepted public ACE rtlib header and symbol
   allowlist and selects `LIB_ANT`, not `LIB_ACE`.
9. No server-evaluation process or retained artifact contains a secret key,
   decryptor, or secret-key-derived private material.
10. No new direct or source-level ACE rtlib dependency is added to `be.so`
    without the repository-required explicit design and link-closure approval.
11. Required malformed input is rejected by the semantic gatekeeper before
    lowering, WOPT, LNO, or CG.
12. Failed publication never leaves a partial file with the name of a valid
    `.B` artifact.

### 2.4 Support scope

E0 consumes all `o0-handoff-required` rows and executes every
`shared-o0-o2-comparison-required` row needed to establish the baseline side of
future comparisons. It does not implement `o2-core-only` or
`o2-extension-only` rows. A `fallback-required` label is validated only as an
O0 full-profile fallback contract; E0 never constructs a mixed O0/O2 fallback.

Every consumed row must name its source authority, positive boundary, negative
boundary, first diagnostic, oracle, capability, owner, fallback, and acceptance
test. A circular row such as "supported if the implementation accepts it" is
invalid.

## 3. Entry Contract and Hard Stop

### 3.1 Required entry inputs

E0 may start only when all of the following are present and accepted:

1. `PRE-O2-LOCK-EXIT` with accepted master, ADR, option truth table, baseline
   manifest, runtime contract, and record ownership.
2. A complete `F1-PREP` source/pass/config/support/license/toolchain bundle.
3. Accepted P0 canonicalization, slot classification, and negative evidence.
4. Accepted P1a ACE rtlib public-header/symbol/build and lifecycle contract.
5. Accepted P1b ANT provider, exact parameter projection, runtime context, key
   separation, and execution evidence.
6. Accepted `F1-IMPL` mature Fhelipe pipeline and protected-DP evidence.
7. Accepted `F1-ACCEPT` focused and complete ResNet-20 execution bundle.
8. A pinned supported host, container or reproducible environment, toolchain,
   ACE rtlib/ANT build, datasets, weights, inputs, and decryption test client.
9. Complete hashes with no placeholder, moving branch, unbounded source input,
   skipped required row, or provisional result.

The runner performs an intake check before any expensive work. Missing or
unaccepted entry input produces `Unverified` and no downstream unlock.

### 3.2 Known PRE-O2 interface reconciliation

The current governing inputs list `rt_ant/ant_api.h` in the source-lock hash
table but name `rt_ant/rt_api.h` in an initial generated-C allowlist passage.
`FRZ-08` must resolve the exact installed public path, content hash, and symbol
surface. E0 consumes only that accepted manifest. It must not guess a path,
accept both paths implicitly, or close while the source/header allowlist is
ambiguous.

### 3.3 Hard stop rule

Qualification work is read-only consumption, replay, validation, and evidence
publication. Discovery of O2 implementation in the input rejects the candidate;
qualification restarts from a clean O0-only input.

## 4. Dependency Graphs

### 4.1 Implementation and build dependency graph

```text
accepted master and ADR plus FRZ-01..09
  -> complete external O0 producer bundle
       -> F1-PREP
       -> P0
       -> P1a
       -> P1b
       -> F1-IMPL
       -> F1-ACCEPT
  -> E0 immutable input manifest
  -> E0 independent replay and validation suites
  -> E0 read-only consumer probe
  -> atomic evidence publication and independent reproduction
  -> O2-O0Q-001 decision
  -> Qualified only: O2-E0-EXIT and O2-E1 may start
```

No P2, S1, or S2 implementation dependency points into E0. E0 must remain
buildable and runnable using the accepted O0 product and validation interfaces.

### 4.2 Compiler pass-order dependency being observed

E0 does not add or reorder a compiler pass. It verifies the accepted O0 order:

```text
fixed requested configuration including N
  -> all-PU canonicalization with fusion disabled
  -> mature Fhelipe layout and lowering pipeline
  -> accepted state and rescale handling
  -> protected baseline DP initial bootstrap placement
  -> final O0 CKKS and logical key records
  -> standard WHIRL and generated C
  -> ACE rtlib common plus ANT execution
```

The E0 consumer path is deliberately shorter and read-only:

```text
accepted O0 application.fhe.B plus baseline records
  -> independent process reopen
  -> schema, ownership, ID, hash, and protected-site validation
  -> canonical normalization
  -> immutable planner-input digest and comparison report
```

The consumer path stops before O2 candidate selection, identity planning, plan
publication, materialization, or provider execution.

### 4.3 Build and link impact

- E0 may add test and evidence tooling only after the complete O0 candidate is
  ready. It must not change production backend linkage.
- The generated-C probe compiles against the accepted installed public ACE
  rtlib headers and links `FHErt_common` plus `FHErt_ant`.
- The build audit compares the defined and undefined symbols of `be.so` and all
  recorded consumers against the PRE-O2 baseline. An unexplained ACE rtlib
  symbol or dependency is a rejection.
- Compiler-side exact parameter queries use the accepted isolated resolver or
  an explicitly approved dependency design. E0 does not choose the topology.
- Private ACE compiler/AIR headers, C++ provider classes, STL ABI, and exceptions
  are forbidden at the generated-C boundary.

### 4.4 Stale inputs and requalification

Any change in a bound input makes the qualification stale before downstream
work continues. Bound inputs include master/ADR text, Open64 and Fhelipe source,
pass order/configuration, support rows, record schema, diagnostics, fixtures,
weights/data, toolchain, security estimator, ACE rtlib revision/public headers,
build flags/libraries, provider capabilities, runtime lifecycle mode, host
protocol, or qualification runner.

A stale qualification is neither `Qualified` nor an acceptable prior. It must
be replaced by a new qualification ID after the requalification loop in
Section 11.

## 5. Qualification Identity and Evidence Contract

### 5.1 Qualification input manifest

The proposed `qualification-inputs.json` schema shall contain at least:

- schema version and canonicalization rule version;
- accepted master, companion, ADR, O2-plan, execution-index, and repository-rule hashes;
- Open64, Fhelipe, and ACE rtlib source commits and complete adopted file or
  bundle hashes;
- O0 pass order, configuration defaults, support matrix, diagnostics, license,
  build, and toolchain hashes;
- accepted P0/P1a/P1b/F1 handoff IDs, decisions, reviewer signoffs, and bundle
  hashes;
- public header path/hash allowlist and generated-C symbol allowlist;
- `FHErt_common`, `FHErt_ant`, `LIB_ANT`, library hashes, build flags, target,
  lifecycle mode, and provider capabilities;
- requested configuration, security estimator, CKKS parameter/profile, logical
  key, and ANT-expanded key fingerprints;
- fixture, model, weight, dataset, input, seed, generator, protocol, and
  environment hashes;
- exact commands, timeouts, retry policy, artifact root, and retention class;
  and
- required suite IDs and their owners/reviewers.

The qualification ID is the SHA-256 of the canonical bytes of this complete
input manifest. The canonical byte rule must be frozen in the manifest schema
before the first accepted run. A new input byte produces a new qualification
ID; it never overwrites a prior bundle.

### 5.2 Result state machine

```text
NotStarted
  -> Running
       -> Qualified
       -> Rejected
       -> Unverified

Qualified --bound input changes--> Stale
Rejected  --O0 remediation and new bundle--> NotStarted with new ID
Unverified --evidence/environment completed--> NotStarted with new ID
Stale --targeted requalification plus complete current matrix--> Qualified,
                                                             Rejected, or Unverified
```

- `Qualified` means every required row passed against the same current input
  manifest and independent reproduction passed.
- `Rejected` means evidence proves that the candidate violates a required
  contract. The first failing domain and its owner are recorded.
- `Unverified` means evidence, environment, provider availability, reviewer
  independence, or harness validity is insufficient to decide. It is not a
  soft pass.
- `Stale` is an invalidated historical result, not a fourth completion result.

### 5.3 Atomic and immutable publication

1. Write work products under an attempt-specific staging directory.
2. Use temporary suffixes for compiler images until the writer and verifier
   complete. A failure must not leave a file named as a valid `.B`.
3. Generate same-stem `.T` files only from completed `.B` files after the
   producer process exits.
4. Compute `SHA256SUMS`, run the secret scan, and generate the decision last.
5. Atomically publish the attempt directory under its qualification ID.
6. Never modify a published attempt. Corrections create a new attempt and, if
   any bound input changed, a new qualification ID.
7. A small current-decision index may point to the accepted immutable bundle,
   but changing the pointer does not erase or rewrite history.

Accepted bundles are retained for the repository lifetime. Rejected,
unverified, and stale qualification attempts are retained for at least 180
days. Generated evidence is not committed unless a separate review explicitly
selects a golden fixture or immutable reference file.

## 6. Interfaces and Data Contracts

### 6.1 Proposed qualification command

The following is a proposed target command. This document does not claim that
the runner or its fixtures already exist.

```text
python3 osprey/be/vho/tests/run_o0_handoff_qualification.py --build-dir build --manifest testdata/fhe_o2/o0-qualification/SHA256SUMS --artifacts test-artifacts/o2/O2-O0Q-001 --suite all
```

The target CLI contract is:

- `--suite all` is the only mode that may emit a qualification decision;
- a named suite may produce diagnostic evidence but cannot unlock O2;
- inputs are read-only and must match the manifest before execution;
- outputs are written only beneath the attempt staging directory;
- result, first failing test, failing owner, input fingerprint, and evidence
  root are machine-readable;
- an interrupted run returns `Unverified` and never publishes a valid-looking
  partial bundle; and
- no implicit download, moving source checkout, environment mutation, or
  fallback is permitted.

The exit-code and JSON result schema shall be frozen in the first E0 harness
commit. The signed `qualification-decision.md` plus machine-readable decision
record, not process exit code alone, is the release evidence.

### 6.2 Authoritative record crosswalk

E0 does not allocate or extend a record. It requires the accepted O0
`baseline-record-crosswalk.json` to identify the exact physical owner, version,
verifier, serializer, and compatibility rule for each semantic concept.

| Semantic concept | Required authoritative owner | E0 check |
| --- | --- | --- |
| Requested FHE compilation configuration | Accepted O0/common-com record | Fixed `N`, security, bootstrap, precision, and scale intent exist before canonicalization and survive reopen |
| Canonical source/value/slot identity | Accepted O0/common-com record and canonicalizer | All PUs and calls agree; ordering is deterministic; slot classes are exact |
| Baseline encrypted tensor layout | Accepted O0 layout record | Complete slot map, shards, masks, lineage, and frozen Fhelipe producer identity |
| Resolved CKKS parameters | Accepted O0/common-com parameter record | Complete Q/P/CRT/profile and fixed-N relationship; provider state cannot overwrite it |
| CKKS value state and actions | Accepted O0/common-com state record | Levels, scales, components, bounds, actions, and immediate Relin are explicit and valid |
| Baseline bootstrap placement | Accepted O0/common-com placement record | DP objective/provenance and manual/policy/protected identities are preserved |
| Logical key requirements | Accepted O0/common-com logical-key record | Exact signed rotations, relin, bootstrap, and first-consumer provenance |
| ANT-expanded key material | Accepted runtime evidence manifest | Derived from logical requirements and provider manifest; not compilation intent |
| Runtime capability and build | Accepted P1a/P1b manifest | Source/header/library hashes, target, capabilities, lifecycle, and exact parameter mapping |
| O2 consumer digest | E0 evidence only | Normalized immutable input summary; never a plan, action selection, or new semantic truth |

Every physical record must use the accepted version/capability mechanism,
invalid-zero IDs, checked ranges, pointer-free persisted data, deterministic
ordering, and independent reopen. Missing ownership, duplicate truth, or a
consumer digest that contains a planning decision rejects the bundle.

### 6.3 ACE rtlib public boundary

E0 consumes the exact PRE-O2 accepted path/hash allowlist. At minimum, the
accepted contract must identify the installed public common headers, ANT
umbrella/API headers, and any reviewed CKKS/LPOLY/POLY declarations reachable
from them. The manifest must resolve the `rt_ant/ant_api.h` versus
`rt_ant/rt_api.h` discrepancy described in Section 3.2.

The symbol contract must include only reviewed operations needed by O0,
including context lifecycle, parameter setup/query, encode/encrypt/decode test
support, rotate, `Mul_ciph3`, `Relin`, rescale, bootstrap, status, and cleanup.
The exact names come from the accepted source-locked manifest rather than this
plan. A non-allowlisted include or symbol is a hard failure.

### 6.4 State, effect, alias, and order checks

E0 validates the O0 contracts without defining new ones:

- immutable versus in-place behavior, alias/noalias, escape, ownership, and
  destruction are complete for every used rtlib entry point;
- context and key access are effects even when the underlying data is
  immutable;
- allocation, failure, bootstrap, key switching, Relin, rescale, mod-switch,
  import, retain/release, and cleanup are ordered and observable;
- WOPT does not CSE, PRE, hoist, speculate, duplicate, eliminate, or reorder
  those operations;
- CKKS joins have compatible parameter domain, level, scale, components,
  layout, range, and error after explicit actions; and
- the three-component `MulCC` result has no ordinary consumer between
  `Mul_ciph3` and adjacent `Relin`.

### 6.5 Serialization and compatibility

For every meaningful O0 image, the producer process exits before a separate
process reopens and verifies the file. Required checks include:

- old supported O0 artifacts;
- feature-absent artifacts;
- current complete baseline artifacts;
- two clean current productions;
- header, version, capability, count, range, ID, and hash corruptions;
- source/config/provider fingerprint mismatches; and
- stable normalized `ir_b2a -st -src` output.

Compatibility success cannot depend on producer memory, Python objects, ACE
AIR objects, or an in-memory bypass.

### 6.6 Diagnostics and fallback

Each negative has one stable first diagnostic and fails before partial
publication. Existing accepted diagnostic names are consumed from the locked
manifest; E0 does not rename them casually. At minimum, the manifest covers
stale source/plan/provider data, fixed-N capacity/security failure, malformed
IR, unsupported provider/profile, Q/P/CRT mismatch, invalid bootstrap policy,
non-adjacent or duplicate Relin, server secret presence, and publication
failure.

There is no fallback from a rejected or unverified qualification. The only
permitted baseline is the complete accepted O0 profile. A reduced profile,
synthetic substitute, or mixed O0/O2 plan is forbidden.

## 7. Detailed Work Breakdown

All files in this section are proposed targets unless they already belong to
the accepted O0 bundle. Product defects discovered by a task are returned to
the external owner; they are not fixed in the E0 PR.

| Task | Concrete change and files/components | Dependency | Tests and oracle | Required artifacts | Owner and reviewer | Merge and exit rule |
| --- | --- | --- | --- | --- | --- | --- |
| `E0-W00` | Validate entry checklist and reviewer independence in `run_o0_handoff_qualification.py`; read accepted handoff manifests only | `PRE-O2-LOCK-EXIT` and every producer handoff accepted | `O2-O0Q-ENTRY-001`; exact hash and status comparison | `entry-check.json`, authority and signoff report | Qualification owner / architecture reviewer | Any missing, pending, waived, or self-approved input returns `Unverified`; no later task runs |
| `E0-W01` | Define canonical `qualification-inputs.json`, result schema, staging rules, and suite registry under `testdata/fhe_o2/o0-qualification/` | W00 | Schema roundtrip, unknown-field/version negatives, canonical-byte determinism | Input manifest, schema report, qualification ID | Qualification owner / common-com and evidence reviewers | Schema and canonical hash rule frozen before execution; no O2 semantic record introduced |
| `E0-W02` | Verify Open64/Fhelipe/ACE source, complete pass/config/support/license bundle, toolchain, fixtures, environment, and document hashes | W01 | `O2-O0Q-ID-001`; recompute every hash from local immutable inputs | `source-lock.json`, pass/config, license, toolchain, environment reports | Evidence owner / independent reproducer | Zero placeholder, moving ref, missing file, or hash mismatch |
| `E0-W03` | Replay all-PU and call-consistent canonicalization with fusion disabled and fixed `N` | W02 and accepted P0 | `O2-O0Q-CAN-001`; two clean runs and producer-free reopen | Source and canonical `.B`/`.T`, normalized dumps, determinism report | O0 canonicalization owner supplies / E0 common-com reviewer validates | Exact structural and normalized equality; no address/traversal/order dependence |
| `E0-W04` | Validate complete `valid/zero/junk/gap` maps using nonzero sentinels and required-zero outputs | W03 | `O2-O0Q-SLOT-001`; independent logical and physical slot oracle from accepted P0 bundle | Slot map, sentinel inputs, decoded and oracle outputs | O0 layout owner supplies / independent tensor reviewer validates | Exact class/map equality; valid output unchanged; required-zero slots within tolerance |
| `E0-W05` | Replay malformed shape, lineage, overlapping write, integer/range overflow, capacity, and early gatekeeper negatives | W03 | `O2-O0Q-NEG-001`; phase-order witness proves rejection precedes lowering/WOPT/LNO/CG | Negative corpus, first-diagnostic and phase-order logs | O0 gatekeeper owner supplies / backend reviewer validates | Every required negative returns its frozen first diagnostic with no downstream pass entry |
| `E0-W06` | Audit baseline record crosswalk, one-owner rule, old/feature-absent compatibility, corruption handling, and independent reopen | W02 and accepted record contracts | `O2-O0Q-REC-001`; independent reader plus corruption matrix | Crosswalk, old/current `.B`/`.T`, corruption corpus, verifier logs | Common-com owner supplies / independent compatibility reviewer validates | No duplicate truth, dangling ID, unchecked range/hash, or producer-memory dependency |
| `E0-W07` | Replay the full mature Fhelipe assignment, compaction, conversion hoisting/decomposition, lowering, state/rescale, and protected-DP path | W03-W06 and accepted F1-IMPL | `O2-O0Q-BASE-001`; accepted independent layout/state/DP oracles | Pass trace, layout/state/placement records, objective and provenance reports | O0 Fhelipe owner supplies / independent O2 baseline reviewer validates | Every frozen pass is present in locked order/config; no stripped or replacement baseline |
| `E0-W08` | Validate auto/on/manual/off, advanced-off, full-baseline fallback, hard/manual/pre-ReLU boundaries, and DP-owned automatic sites | W07 and accepted ADR truth table | `O2-O0Q-POLICY-001`; protected-site identity and infeasibility negatives | Truth table results, protected sites, placement provenance, fallback report | O0 policy owner supplies / architecture and CKKS reviewers validate | Protected sites survive exactly; infeasible modes fail closed; fallback is whole baseline |
| `E0-W09` | Validate fixed `N`, Q/P, scales, levels, range/error, security, bootstrap profiles, action adjacency, and logical key census | W06-W08 | `O2-O0Q-CKKS-001`; independent state transfer and key collection | Requested config, resolved parameters, state trace, key requirements, security report | O0 CKKS owner supplies / cryptography reviewer validates | No N mutation; complete state; every `MulCC` has exactly one adjacent mandatory Relin |
| `E0-W10` | Audit accepted P1a public headers/symbols, compile/link/load/run probe, lifecycle/effects, drift negatives, and backend dependency inventory | W02 and accepted P1a | `O2-O0Q-P1A-001`; fresh-header, stale-header, private-header, non-allowlisted-symbol, and `be.so` dependency cases | Header hashes, symbol inventories, link maps, probe logs, consumer inventory | Runtime interface owner supplies / build and security reviewers validate | Only accepted public surface and `LIB_ANT`; `be.so` has no unreviewed dependency |
| `E0-W11` | Project Open64 records to `CKKS_PARAMS`, create/query ANT runtime context, compare exact Q/P/CRT/profile, and replay unsupported cases | W09-W10 and accepted P1b | `O2-O0Q-P1B-001`; independent field-by-field comparator | Projection, runtime query, capability, mismatch, and cleanup reports | ANT provider owner supplies / cryptography and runtime reviewers validate | Runtime state equals persisted plan exactly or rejects without mutation/publication |
| `E0-W12` | Validate client/test versus server-evaluation lifecycle and scan all code/evidence for secret material | W10-W11 and closed lifecycle TODO | `O2-O0Q-SEC-001`; process census, negative injected-secret corpus, cleanup and concurrency cases | Key/process census, secret-scan report, lifecycle and cleanup logs | Runtime security owner supplies / independent security reviewer validates | Server has no secret/decryptor; retained artifacts contain no secret; all cleanup passes |
| `E0-W13` | Execute focused MVM/Conv/ReLU/residual cases and the locked complete ResNet-20 path through generated C and ANT | W03-W12 and accepted F1-ACCEPT | `O2-O0Q-E2E-001`; clear tensor oracle and accepted decryption client | O0 materialized `.B`/`.T`, generated C, binary, build/run logs, decoded/oracle outputs | O0 acceptance owner supplies / independent runtime and numerical reviewers validate | Every locked workload executes and meets all numeric/state/security gates |
| `E0-W14` | Inject failure at each publication stage and test stale/corrupt/provider/security/manual-off failures | W03-W13 | `O2-O0Q-ATOM-001`; filesystem and artifact-name audit | Failure directories, atomicity report, diagnostics, cleanup logs | Reliability owner / backend reviewer | No valid-looking partial `.B`, plan, key, data, or decision artifact |
| `E0-W15` | Repeat two clean productions and normalize records, plan, keys, output structure, and materialized WHIRL | W13-W14 | `O2-O0Q-DET-001`; byte/normalized equality with nondeterministic runtime bytes excluded by contract | Two run trees, normalized comparison, key fingerprints | Evidence owner / independent reproducer | Required normalized outputs and deterministic artifacts match exactly |
| `E0-W16` | Implement the read-only O2 consumer probe; reopen O0 records and emit only a normalized immutable input digest | W06-W15 | `O2-O0Q-CONSUMER-001`; positive, stale, corrupt, duplicate-owner, and injected-planning-field cases | `consumer-probe.json`, digest, rejection logs | O2 qualification owner / common-com and O2-E1 consumer reviewers | Probe survives independent process; any O2 plan/action field is rejected |
| `E0-W17` | Finalize same-stem traces, SHA256SUMS, secret scan, reproduction report, owner routing, and signed decision; publish atomically | W00-W16 | `O2-O0Q-001 --suite all` plus independent full reproduction | Complete Section 9 bundle | Qualification owner / all domain reviewers | All required rows pass against one fingerprint for `Qualified`; otherwise exact `Rejected` or `Unverified` |

## 8. Proposed PR and Commit Sequence

All E0 implementation commits map to governing commit group `QUAL-O0`. They
must contain no O2 algorithm or product-record change.

| Commit or PR | Single semantic goal | Required review and evidence |
| --- | --- | --- |
| `QUAL-O0-1` | Input/result schema, suite registry, entry gate, immutable publication protocol | Architecture, common-com, and evidence review; schema determinism and missing-input negatives |
| `QUAL-O0-2` | Identity, canonicalization, slots, early negatives, record crosswalk, compatibility, and reopen suites | Common-com, gatekeeper, and tensor reviewers; W02-W06 evidence |
| `QUAL-O0-3` | Mature Fhelipe path, protected-DP policy, CKKS state, fixed N, immediate Relin, and logical-key suites | O0 baseline, CKKS, and architecture reviewers; W07-W09 evidence |
| `QUAL-O0-4` | P1a/P1b public interface, exact ANT projection, lifecycle, key separation, secret scan, and build-link audit | Runtime, build, cryptography, and security reviewers; W10-W12 evidence |
| `QUAL-O0-5` | Focused/ResNet execution, failure atomicity, two-run determinism, and read-only consumer probe | Runtime, reliability, evidence, and O2 consumer reviewers; W13-W16 evidence |
| `QUAL-O0-6` | Acceptance orchestration and immutable decision publication only | All domain reviewers; one full independent `O2-O0Q-001` reproduction |

Contract tests land with or before the implementation they constrain. Each
commit is independently runnable for its completed suites. The final decision
commit or review contains no compiler algorithm change and does not add
generated binary evidence to Git. If a suite exposes an O0 defect, the E0 PR
records and routes the defect; the external O0 owner supplies a new complete
bundle before E0 resumes.

## 9. Verification Matrix

Every command in this section is a proposed target command to be implemented by
E0. None is a claim about current tooling. Named-suite runs aid diagnosis; only
the final `--suite all` run can set `Qualified`.

The common command prefix is written in full in each row so the review contract
is unambiguous.

| Test ID and exact proposed command | Source, input, and hash class | Expected result, tolerance, and first failure | Seed and bounds | Platform/provider | Warmup, samples, timeout, and retained evidence | Owner/reviewer/pass rule |
| --- | --- | --- | --- | --- | --- | --- |
| `O2-O0Q-ENTRY-001`: `python3 osprey/be/vho/tests/run_o0_handoff_qualification.py --build-dir build --manifest testdata/fhe_o2/o0-qualification/SHA256SUMS --artifacts test-artifacts/o2/O2-O0Q-001 --suite entry` | Accepted authority and F1/P0/P1 manifests | Exact status/hash/signoff match; missing input is `Unverified`; no tolerance | No random input; all required handoffs | Provider manifest inspected, no provider execution | 0 warmups, 1 run, 120 s; entry and authority reports, 180 days | Qualification owner / architecture reviewer; every prerequisite accepted and current |
| `O2-O0Q-ID-001`: `python3 osprey/be/vho/tests/run_o0_handoff_qualification.py --build-dir build --manifest testdata/fhe_o2/o0-qualification/SHA256SUMS --artifacts test-artifacts/o2/O2-O0Q-001 --suite identity` | Open64, Fhelipe, ACE rtlib, docs, pass/config/support/license/toolchain, fixture/environment hashes | Exact bytes and hashes; first mismatched field reported; no tolerance | All manifest entries; no sampling | Pinned qualification host; provider files offline | 0/1/120 s; normalized source and environment reports, 180 days | Evidence owner / independent reproducer; zero missing, moving, placeholder, or mismatched input |
| `O2-O0Q-CAN-001`: `python3 osprey/be/vho/tests/run_o0_handoff_qualification.py --build-dir build --manifest testdata/fhe_o2/o0-qualification/SHA256SUMS --artifacts test-artifacts/o2/O2-O0Q-001 --suite canonicalization` | Locked focused and full all-PU inputs; fixed N; fusion disabled | Two clean structures and normalized dumps exact; idempotent after reopen; first canonicalization diagnostic exact | Replay producer seeds; all PUs/calls; 2 clean runs | Supported Open64 host; provider-free structural check | 0 warmups, 2 productions, 600 s each; source/canonical `.B`/`.T`, 180 days | Canonicalization owner / common-com reviewer; zero structural mismatch |
| `O2-O0Q-SLOT-001`: `python3 osprey/be/vho/tests/run_o0_handoff_qualification.py --build-dir build --manifest testdata/fhe_o2/o0-qualification/SHA256SUMS --artifacts test-artifacts/o2/O2-O0Q-001 --suite slots` | Locked layout cases with nonzero sentinel junk/gap values | Slot classes/maps exact; clear `1e-12/1e-12`; required-zero decoded abs `<=1e-8`; first map diagnostic exact | Replay locked seeds; every accepted layout boundary | Clear oracle plus accepted ANT execution cases | 0/1/20 min per case; slot maps, sentinel data, decoded/oracle results, 180 days | Layout owner / independent tensor reviewer; no sentinel contamination or out-of-range slot |
| `O2-O0Q-NEG-001`: `python3 osprey/be/vho/tests/run_o0_handoff_qualification.py --build-dir build --manifest testdata/fhe_o2/o0-qualification/SHA256SUMS --artifacts test-artifacts/o2/O2-O0Q-001 --suite negative-ir` | Malformed shape, lineage, write, overflow, capacity, stale/corrupt cases | Frozen first diagnostic before lowering/WOPT/LNO/CG; exact phase witness | Every required negative once; no random omission | Provider-free unless provider mismatch is the subject | 0/1/300 s; diagnostics and phase traces, 180 days | Gatekeeper owner / backend reviewer; every case rejects before downstream pass entry |
| `O2-O0Q-REC-001`: `python3 osprey/be/vho/tests/run_o0_handoff_qualification.py --build-dir build --manifest testdata/fhe_o2/o0-qualification/SHA256SUMS --artifacts test-artifacts/o2/O2-O0Q-001 --suite records` | Old, feature-absent, and current images; all count/range/ID/hash corruptions | Exact reopen or exact rejection; no tolerance; no duplicate owner | All record families and each corruption location | Supported readers; provider data treated as hashed evidence | 0 warmups, 2 current serializations, 600 s; `.B`/`.T`, corruptions, logs, 180 days | Common-com owner / compatibility reviewer; producer-free reopen and all corruption checks pass |
| `O2-O0Q-BASE-001`: `python3 osprey/be/vho/tests/run_o0_handoff_qualification.py --build-dir build --manifest testdata/fhe_o2/o0-qualification/SHA256SUMS --artifacts test-artifacts/o2/O2-O0Q-001 --suite baseline` | Accepted Fhelipe revision, complete pass/config/support manifest, focused and ResNet inputs | Every frozen pass and state transition present in locked order; independent baseline oracles exact | Replay producer seeds; complete accepted support domain | Accepted O0 Open64 and ANT environment | 0/1 structural run, 30 min focused and 3 h full timeout; pass/state/layout/DP traces, 180 days | O0 baseline owner / independent O2 baseline reviewer; no omitted or substituted pass |
| `O2-O0Q-POLICY-001`: `python3 osprey/be/vho/tests/run_o0_handoff_qualification.py --build-dir build --manifest testdata/fhe_o2/o0-qualification/SHA256SUMS --artifacts test-artifacts/o2/O2-O0Q-001 --suite policy` | Accepted option truth table, hard/manual/pre-ReLU and replaceable DP sites | Exact site identity/order/state; manual/off failure and advanced-off/fallback exact | Every truth-table row and infeasible case | Provider capability fixed across policy cases | 0/1/600 s; truth table, sites, placement/fallback records, 180 days | Policy owner / architecture and CKKS reviewers; protected sites unchanged and fallback is complete O0 |
| `O2-O0Q-CKKS-001`: `python3 osprey/be/vho/tests/run_o0_handoff_qualification.py --build-dir build --manifest testdata/fhe_o2/o0-qualification/SHA256SUMS --artifacts test-artifacts/o2/O2-O0Q-001 --suite ckks` | Fixed-N requested records, complete chain/profile, action and key cases | Exact state/security/key agreement; no N mutation; one adjacent Relin per `MulCC`; predicted bounds conservative | Accepted N/chain/profile domain and all boundary cases | Provider capability fixture plus runtime cross-check in P1b | 0/1/20 min; parameters, state/action trace, security and logical keys, 180 days | CKKS owner / cryptography reviewer; all fields and transitions pass exactly |
| `O2-O0Q-P1A-001`: `python3 osprey/be/vho/tests/run_o0_handoff_qualification.py --build-dir build --manifest testdata/fhe_o2/o0-qualification/SHA256SUMS --artifacts test-artifacts/o2/O2-O0Q-001 --suite p1a` | Accepted ACE rtlib source/header/build/library hashes and generated-C allowlists | Compile/link/load/run succeeds; drift/private include/non-allowlisted symbol and unreviewed backend dependency reject exactly | Every used header/symbol plus one negative per prohibited class | Pinned host; `FHErt_common` + `FHErt_ant`; `LIB_ANT` | 0 warmups, 1 probe per case, 600 s; hashes, symbols, link maps, logs, 180 days | Runtime interface owner / build reviewer; zero allowlist leak or new `be.so` dependency |
| `O2-O0Q-P1B-001`: `python3 osprey/be/vho/tests/run_o0_handoff_qualification.py --build-dir build --manifest testdata/fhe_o2/o0-qualification/SHA256SUMS --artifacts test-artifacts/o2/O2-O0Q-001 --suite p1b` | Persisted Open64 plan, `CKKS_PARAMS` projection, runtime-created Q/P/CRT/profile | Every field exact; unsupported projection rejects without mutation; duplicate Relin mapping rejects | Accepted N/chain/profile set plus each mismatch field | Pinned source-locked ANT build and reviewed query topology | 0 warmups, 1 run per case, 20 min; projection/query/mismatch/cleanup reports, 180 days | ANT owner / runtime and cryptography reviewers; zero field mismatch or silent provider override |
| `O2-O0Q-SEC-001`: `python3 osprey/be/vho/tests/run_o0_handoff_qualification.py --build-dir build --manifest testdata/fhe_o2/o0-qualification/SHA256SUMS --artifacts test-artifacts/o2/O2-O0Q-001 --suite security` | Client/test and server-evaluation processes plus every retained artifact class | No server secret/decryptor and no artifact secret; injected secret is detected first | All key classes and artifact formats; concurrency bounds from accepted lifecycle manifest | Accepted ANT lifecycle mode | 0 warmups, 1 lifecycle run per case, 20 min; census, secret scan, sanitizer/cleanup logs, 180 days | Runtime security owner / independent security reviewer; zero secret leak and clean lifecycle |
| `O2-O0Q-E2E-001`: `python3 osprey/be/vho/tests/run_o0_handoff_qualification.py --build-dir build --manifest testdata/fhe_o2/o0-qualification/SHA256SUMS --artifacts test-artifacts/o2/O2-O0Q-001 --suite workloads` | Locked focused MVM/Conv/ReLU/residual and complete ResNet-20 inputs, weights, data, parameters | Focused decoded abs `<=1e-4`, rel `<=1e-6`; application max abs `<=1e-3`, max rel `<=1e-4` for reference magnitude `>=1e-2`, top-1 exact; NaN/Inf fail | Locked workload seeds; complete support rows | Pinned accepted Open64, generated-C, and ANT environment | Focused 5 warmups/30 samples/20 min; full 1/5/3 h; complete runtime bundle retained indefinitely if accepted | O0 acceptance owner / independent runtime and numerical reviewers; every workload and predicted error bound passes |
| `O2-O0Q-ATOM-001`: `python3 osprey/be/vho/tests/run_o0_handoff_qualification.py --build-dir build --manifest testdata/fhe_o2/o0-qualification/SHA256SUMS --artifacts test-artifacts/o2/O2-O0Q-001 --suite failure-atomicity` | Every publication step; stale/corrupt/provider/security/manual-off failures | Exact first diagnostic; no valid-looking partial plan/image/key/data/decision | One injection at every enumerated publication point | Provider on only for provider failure points | 0/1/600 s per point; failure trees and filesystem audit, 180 days | Reliability owner / backend reviewer; zero partial accepted-name artifact |
| `O2-O0Q-DET-001`: `python3 osprey/be/vho/tests/run_o0_handoff_qualification.py --build-dir build --manifest testdata/fhe_o2/o0-qualification/SHA256SUMS --artifacts test-artifacts/o2/O2-O0Q-001 --suite determinism` | Two clean complete productions with identical input fingerprint | Normalized records, baseline plan, logical/ANT key manifests, transformed data, materialized WHIRL, and normalized outputs exact; random key bytes excluded | Same locked seeds, environment, and inputs | Same pinned host and ANT manifest | 0 warmups, 2 complete runs, full protocol timeout; two evidence trees and diff, 180 days | Evidence owner / independent reproducer; zero unexplained normalized or byte difference |
| `O2-O0Q-CONSUMER-001`: `python3 osprey/be/vho/tests/run_o0_handoff_qualification.py --build-dir build --manifest testdata/fhe_o2/o0-qualification/SHA256SUMS --artifacts test-artifacts/o2/O2-O0Q-001 --suite consumer-probe` | Accepted O0 `.B` and baseline records; stale/corrupt/duplicate-owner/planning-field variants | Exact immutable digest after independent reopen; variants reject; no O2 plan emitted | All record families; one corruption per identity/fingerprint class | Provider not loaded; provider manifest is an input hash | 0/1/300 s; consumer digest/report and rejection logs, 180 days | Qualification owner / common-com and E1 consumer reviewers; output contains no candidate, selected action, or identity plan |
| `O2-O0Q-TRACE-001`: `ir_b2a -st -src test-artifacts/o2/O2-O0Q-001/canonical.fhe.B test-artifacts/o2/O2-O0Q-001/canonical.fhe.T` | Completed canonical image and preserved source path | Same-stem source-interleaved trace generated; exact normalized trace | One accepted canonical image per fixture family | Supported `ir_b2a`; provider-free | 0/1/300 s; `.B` and `.T`, 180 days or indefinitely if accepted | Evidence owner / WHIRL reviewer; both `-st` and `-src`, no missing source mapping |
| `O2-O0Q-TRACE-002`: `ir_b2a -st -src test-artifacts/o2/O2-O0Q-001/baseline-plan.B test-artifacts/o2/O2-O0Q-001/baseline-plan.T` | Completed O0 baseline plan image | Same-stem trace shows accepted baseline records and no O2 identity | One accepted baseline plan per required workload | Supported `ir_b2a`; provider-free | 0/1/300 s; `.B` and `.T`, 180 days or indefinitely if accepted | Evidence owner / common-com reviewer; source-visible, independently reopened, no O2 record |
| `O2-O0Q-TRACE-003`: `ir_b2a -st -src test-artifacts/o2/O2-O0Q-001/materialized.o0.mid.B test-artifacts/o2/O2-O0Q-001/materialized.o0.mid.T` | Completed materialized O0 image | Same-stem trace contains only supported standard WHIRL at final boundary | One accepted materialized image per required workload | Supported `ir_b2a`; ANT is exercised by subsequent generated C | 0/1/300 s; `.B` and `.T`, 180 days or indefinitely if accepted | Evidence owner / backend reviewer; no private FHE/O2/HPOLY op reaches `whirl2c` |
| `O2-O0Q-001`: `python3 osprey/be/vho/tests/run_o0_handoff_qualification.py --build-dir build --manifest testdata/fhe_o2/o0-qualification/SHA256SUMS --artifacts test-artifacts/o2/O2-O0Q-001 --suite all` | One complete current manifest and every required suite above | `Qualified` only if all rows and independent reproduction pass; otherwise exact `Rejected` or `Unverified` | All locked seeds/bounds; no skipped required case | Pinned complete O0/ACE rtlib/ANT environment | Full protocol; accepted bundle retained for repository lifetime | Qualification owner / all independent domain reviewers; unanimous domain pass and current fingerprint |

No retry may discard a timeout, crash, sanitizer finding, provider error, or
numerical failure. A retry is a new attempt with the original failure retained.
No tolerance may be relaxed after observing a result without a new accepted
protocol and new qualification ID.

## 10. Required Qualification Bundle

An accepted `O2-O0Q-001` bundle contains at least:

```text
qualification-id.json
qualification-inputs.json
master-and-adr-lock.json
source-lock.json
pass-config-manifest.json
support-matrix.json
diagnostics.json
environment.json
protocol.json
baseline-record-crosswalk.json
ace-rtlib-ant-manifest.json
requested-configuration.json
source.fhe.B
source.fhe.T
canonical.fhe.B
canonical.fhe.T
baseline-plan.B
baseline-plan.T
layout-and-slot-map.json
ckks-state-trace.json
bootstrap-placement-and-protected-sites.json
logical-key-requirements.json
ant-key-manifest.json
materialized.o0.mid.B
materialized.o0.mid.T
generated.c
generated-c-symbols.txt
build-and-link.log
decoded-output.json
oracle-output.json
negative-tests.json
failure-atomicity.json
determinism-report.json
consumer-probe.json
secret-scan.json
reproduction-report.json
qualification-decision.json
qualification-decision.md
SHA256SUMS
```

The bundle also retains any phase trace under a descriptive lowercase `.t`
name that cannot collide with its `.T` file. It retains the original source at
the path recorded in DST, or an equivalent preserved path mapping accepted by
the WHIRL reviewer, so `-src` evidence remains reviewable.

The final `SHA256SUMS` covers every file except itself according to the frozen
manifest rule. The decision names the exact qualification ID, result, first
failing row if any, owners, reviewers, command log, evidence path, and stale
triggers. A result without the complete bundle is `Unverified`.

## 11. Failure Ownership and Requalification Loop

### 11.1 Failure routing

| Failure domain | Result and owner | Required response |
| --- | --- | --- |
| Missing environment, provider access, complete evidence, or independent reviewer | `Unverified`; qualification coordinator | Complete the missing prerequisite; do not change O0 or start O2 |
| Master/ADR contradiction, pending decision, or unresolved header allowlist | Entry remains blocked or `Rejected` if a supposedly accepted lock is inconsistent; architecture/PRE-O2 owner | Amend and reaccept the authority set, then issue a new input fingerprint |
| Source/pass/config/support/license or baseline algorithm mismatch | `Rejected`; O0 baseline owner | Repair and reaccept the affected O0 handoff and complete F1 bundle |
| Canonicalization, slot, gatekeeper, record, compatibility, or reopen defect | `Rejected`; corresponding O0/common-com owner | Fix within O0/shared scope; rerun producer acceptance; supply a complete new bundle |
| Public rtlib/header/symbol/build/link or `be.so` dependency defect | `Rejected`; P1a runtime/build owner | Restore accepted public boundary or obtain explicit dependency approval; rerun P1a and dependent handoffs |
| Q/P/CRT/profile, state, key, lifecycle, secret, or ANT execution defect | `Rejected`; P1b/CKKS/runtime/security owner | Correct the O0 projection/runtime contract and rerun P1b, F1-ACCEPT, and security evidence |
| Qualification harness defect or ambiguous result schema | `Unverified`; E0 qualification owner | Fix the harness without changing O0 semantics; rerun every affected suite and independent reproduction |
| Consumer probe requires an O2 identity or record | `Rejected`; E0 design issue unless an O0 record is missing | Remove the O2 dependency; if O0 is incomplete, route that defect to its authoritative owner |
| Timeout, crash, sanitizer failure, or nondeterminism | `Rejected` when reproducible product behavior is at fault; otherwise `Unverified` pending valid environment triage | Preserve raw failure; identify owner using a minimal reproducer; never discard the failed sample |

### 11.2 Remediation and requalification sequence

```text
E0 records immutable failure evidence and first failing owner
  -> all O2 implementation remains stopped
  -> external owner repairs only its accepted scope
  -> affected O0 handoff is rerun and reaccepted
  -> F1-ACCEPT republishes one complete coherent O0 bundle
  -> E0 computes a new qualification input fingerprint
  -> targeted suites rerun for diagnosis
  -> every required row is current under the new fingerprint
  -> full --suite all run and independent reproduction
  -> new Qualified, Rejected, or Unverified decision
```

Targeted requalification reduces diagnostic work but never creates a partial
qualification. The final result still requires every row to be current under
one fingerprint and the umbrella run to pass. Unchanged immutable evidence may
be referenced by hash only when the schema explicitly permits reuse and the
independent reviewer verifies that no dependency or stale trigger reaches it.

E0 never patches O0 in place, silently narrows support, substitutes a fixture,
or turns a required row into optional. A repeated failure remains owned by the
same domain until evidence proves a different cause.

### 11.3 Downstream stale handling

If a previously qualified input changes after O2 work has begun:

1. mark the qualification `Stale` immediately;
2. stop all dependent O2 merges, acceptance runs, and release claims;
3. record which O2 plans, caches, costs, records, fixtures, and artifacts bind
   the old fingerprint;
4. run the requalification sequence;
5. if requalified, invalidate and rebuild every affected downstream result;
6. if rejected or unverified, keep O2 stopped and route the issue to its owner.

Historical evidence is retained. Rollback changes the current qualification
pointer and downstream eligibility; it does not delete or rewrite a bundle.

## 12. Negative, Failure, and Compatibility Inventory

The following classes are mandatory and may not be represented only by prose:

- missing or altered master, ADR, source, pass, config, support, license,
  fixture, toolchain, environment, or reviewer-signoff input;
- moving source reference, placeholder hash, incomplete source bundle, and
  omitted mature Fhelipe pass;
- fusion-dependent canonicalization, nondeterministic PU/call order, ambiguous
  lineage, overlapping non-equivalent writes, shape/range overflow, and fixed-N
  capacity or security failure;
- nonzero junk/gap sentinel contamination and required-zero failure;
- unknown record version/capability, duplicate authority, dangling ID, invalid
  first/count range, stale hash, corrupt image, and feature-absent reader;
- lost manual/pre-ReLU/protected site, uncontrolled `RemoveBootstraps`, wrong DP
  provenance, illegal manual/off bootstrap, and reduced-profile fallback;
- changed requested `N`, incomplete Q/P/CRT/profile, unconservative error bound,
  illegal state join, ordinary three-component SSA value, non-adjacent Relin,
  missing Relin, and duplicate Relin;
- canonical `MulCC` mapped to fused `Mul_ciph` followed by an explicit Relin;
- ACE rtlib revision/header/build/library drift, unresolved ANT header path,
  private header inclusion, non-allowlisted symbol, `LIB_ACE` selection, runtime
  plan override, and unreviewed `be.so` dependency;
- unsupported ANT N/profile, field-by-field Q/P/CRT mismatch, cleanup failure,
  global-context cross-test contamination, server secret/decryptor, and secret
  bytes in `.B`, `.T`, generated C, manifests, logs, or retained artifacts;
- generated-C compile, link, load, run, sanitizer, status, or cleanup failure;
- failure at each publication point, interruption, timeout, and stale attempt;
- two-clean-run normalized nondeterminism; and
- a consumer probe that emits a candidate, selected action, identity plan, or
  provider-derived replacement for an authoritative Open64 field.

Each case records the first diagnostic, failing stage boundary, expected owner,
absence of partial publication, and retained evidence path.

## 13. Exit, Rollback, and Stop Rules

### 13.1 `Qualified`

`O2-E0-EXIT` is produced only when:

- all entry authorities and handoffs are accepted and current;
- every WBS task and required verification row passes under one qualification
  fingerprint;
- the complete O0 focused and ResNet-20 paths execute through the accepted ACE
  rtlib/ANT environment;
- fixed N, state, security, protected sites, keys, exact ANT projection,
  immediate Relin, and no-secret requirements pass;
- meaningful `.B` files have matching independently generated `.T` files;
- failure atomicity, compatibility, two-run determinism, secret scanning, and
  the read-only consumer probe pass;
- the complete immutable bundle and `SHA256SUMS` are published; and
- an independent reviewer reproduces the umbrella result and all domain owners
  sign the decision.

The exact completion wording is:

```text
O2-O0Q-001=Qualified. O2-E0-EXIT is satisfied for qualification ID <sha256>.
O2-E1 may begin against this exact fingerprint.
```

No stronger O2 acceptance claim is permitted.

### 13.2 `Rejected`

`Rejected` records the first failing contract, all observed failures, exact O0
or governance owner, immutable evidence, and required handoff to reopen. It
does not create `O2-E0-EXIT`. All O2 work remains stopped.

### 13.3 `Unverified`

`Unverified` records the missing evidence, environment, provider, reviewer, or
harness capability and its owner. It is not a waiver. All O2 work remains
stopped.

### 13.4 Rollback

E0 makes no O0 product change to roll back. If an accepted qualification is
later invalidated, mark the current pointer stale, stop dependent O2 work, and
invalidate derived results. Preserve all evidence and requalify a new complete
bundle. Never delete accepted history or revert unrelated user changes.

### 13.5 Architecture review triggers

Stop and return to PRE-O2 architecture review if:

- the master successor or ADR remains pending or contradicts the selected
  mature baseline, protected-DP policy, or ACE rtlib/ANT boundary;
- a required record has two owners or no owner;
- the exact public ANT header path/symbol surface cannot be resolved;
- O0 acceptance requires an O2-only record, planner, algorithm, fixture, or
  oracle;
- exact parameter queries require an unapproved `be.so` dependency;
- the server-evaluation lifecycle cannot exclude secret/decryptor state;
- a provider must change requested N or cannot express the persisted plan;
- a binary record cannot reopen independently; or
- a required support row lacks a deterministic positive/negative boundary.

## 14. Requirement Traceability

The accepted master successor must replace every `pending successor` entry
with an exact section/version/commit/hash before E0 starts.

| Requirement | Master and ADR status | O2-plan source | E0 work item | Verification and retained evidence |
| --- | --- | --- | --- | --- |
| Complete O0 before any O2 work | User-selected decision; master successor pending; ADR/F0 must record it | Sections 1, 13 `O2-O0Q`, 18, and 20 | W00, W17 | `O2-O0Q-ENTRY-001`, `O2-O0Q-001`, qualification decision and input hash |
| Mature Fhelipe baseline and protected DP | Current master v0.10 Sections 11.3-11.5 conflict with selected direction; `FRZ-01..07` pending acceptance | Sections 0, 11.3, 13 external handoffs | W02, W07, W08 | Identity, baseline, and policy suites; pass/config, layout, placement, protected-site evidence |
| File-wide deterministic canonicalization and slot semantics | Current master plus `AGENTS.md`; exact successor cross-reference pending | Sections 3.1, 6, 13 P0/O0Q, 15.2 | W03-W05 | Canonicalization, slots, negative suites; source/canonical `.B`/`.T` and sentinel outputs |
| One authoritative record owner and independent reopen | Current master plus `AGENTS.md`; `FRZ-05` | Sections 8, 12, 13 O0Q, 15.2 | W06, W16 | Records and consumer suites; crosswalk, old/current images, corruptions, digest |
| Fixed N and complete CKKS state/security | Current master successor exact section pending | Sections 4.2, 8-9, 13 O0Q, 15.2-15.3 | W09, W11, W13 | CKKS, P1b, workload suites; requested config, resolved parameters, state/security traces |
| Immediate Relin and exact ANT multiply mapping | Current master successor pending; `FRZ-08` | Sections 4.2, 7, 9, 13 O0Q, 15.2 | W09-W11 | CKKS/P1a/P1b suites; action trace, generated-C symbols, runtime query |
| ACE rtlib public C surface with `LIB_ANT` | Master successor pending; `FRZ-08..09` | Sections 0, 2.3, 5.1, 7, 13 P1a/P1b/O0Q | W10-W13 | P1a/P1b/security/workload suites; header/library hashes, symbols, links, execution |
| No unreviewed `be.so` dependency | `AGENTS.md`; `FRZ-09` topology pending | Sections 7.2, 13, 18 | W10 | P1a suite; dependency inventory, link maps, defined/undefined symbols |
| No server secret or artifact secret | Master successor pending; lifecycle TODO must close | Sections 7.3, 13 P1b/O0Q, 15.2, 18 | W12-W13 | Security and workload suites; process/key census and secret scan |
| Atomic evidence and same-stem `.B`/`.T` | `AGENTS.md` evidence rules | Sections 3.1, 8.2, 12.2, 13 O0Q, 15.7 | W03, W06, W13-W17 | Failure/determinism/trace/all suites; immutable bundle and SHA256SUMS |
| E0 consumer emits no O2 identity plan | Engineering boundary in governing O2 plan Section 12.4 | Section 13 O0Q item 8 | W16 | Consumer suite; digest schema and injected-planning-field rejection |

## 15. Definition of Done

This detailed plan is ready for implementation planning review when it has no
non-English text, placeholder test identity, implicit O2 implementation, or
unowned requirement. The E0 stage itself is done only when the exact
`Qualified` exit contract in Section 13.1 is satisfied.

The next permitted work item is O2-E1/S1.0. O2-E1 must bind the qualification
ID and immediately stop if that fingerprint becomes stale.
