# O2-E5A Selected-Layout Pre-ReSBM State Detailed Execution Plan

Status: Proposed detailed plan; implementation is locked until all entry gates are accepted
Plan version: 0.1
Date: 2026-09-15
Engineering stage: `O2-E5A`
Governing milestone: `S1.5A` integration
Exit gate: `O2-E5A-EXIT`
## 1. Authority and Metadata

### 1.1 Governing authority

This plan is subordinate to the following authorities, in order:

1. Explicit accepted user decisions and repository invariants in `AGENTS.md`.
2. The accepted master architecture and its accepted successor or amendment.
3. Accepted architecture decision records within their stated scope.
4. `doc/FHE-O2-ACE-RESBM-METAKERNEL-INTEGRATION-PLAN.md`, version 1.6.
5. `doc/FHE-O2-DETAILED-EXECUTION-PLAN.md`, plan set version 0.2, for navigation.
6. This detailed stage plan.
7. Locked papers and source artifacts within their declared claim boundaries.

The currently recorded master baseline is commit `ee1dc6382246c58f49a3097157a8c4e8ff2440c8`.

The recorded master Markdown SHA-256 is `7EF644E9BECEFB541102514B83656C78756ECA71398A771A2A5160FC10B0CB25`.

The recorded master DOCX SHA-256 is `0018769C26B5A0BCD1BDFCBD85AA97B8BAFEA381D7640FBB9E2E81B0022013D9`.

The pending ownership decision is `doc/adr/FHE-O0-O2-PLANNER-OWNERSHIP.md`.

The accepted replacement master and ADR identifiers, versions, commits, and hashes must replace the pending references before implementation begins.
### 1.2 Source and qualification baseline

O2-E5A consumes, but does not recreate, these frozen inputs:

- the current `O2-O0Q-001=Qualified` bundle;
- the accepted O2-E1 source, support, fixture, diagnostic, and oracle lock;
- the O2-E2 record and atomic transaction contracts;
- the O2-E3 graph, state-transfer, effect, alias, and verifier substrate;
- the O2-E4 selected layout, iteration-space, slot-map, and lineage records;
- the accepted fixed `N` from `FHECompilationConfigIR`;
- the accepted O0 protected-site manifest and baseline-DP provenance identity;
- the pinned ACE rtlib/ANT capability manifest as a fingerprint only.

O2-E5A is provider-neutral. The ACE rtlib/ANT manifest is used only to detect stale planning inputs. This stage does not include, link, load, query, or run ACE rtlib or ANT.
### 1.3 Ownership and review

The proposed stage owner is the Open64 CKKS semantic-state owner.

Required reviewers are:

- the O2-E3 state-transfer and effect owner;
- the O2-E4 layout and slot-semantics owner;
- the common/com serialization and compatibility owner;
- the O0 protected-site and baseline-DP contract owner;
- an independent CKKS state-oracle reviewer;
- a WOPT non-interference reviewer;
- an independent evidence and reliability reviewer.

One common/com editor owns any accepted physical record change. O2-E5A must not introduce a parallel record family or a second CKKS parameter truth.
### 1.4 Entry gate

Implementation may begin only when all of these conditions are true:

- `PRE-O2-LOCK-EXIT` is accepted;
- `O2-O0Q-001=Qualified` is current;
- `O2-E1-EXIT` is accepted;
- `O2-E2-EXIT` is accepted;
- `O2-E3-EXIT` is accepted;
- `O2-E4-EXIT` is accepted;
- the selected layout record independently reopens and verifies;
- the state schema and transfer API have accepted owners and versions;
- fixed `N`, security intent, key domain, and bootstrap policy are present;
- protected-site and baseline-DP provenance records independently reopen;
- no unresolved master or ADR conflict affects this stage.

Planning text may be reviewed before these gates close. No production source, fixture, oracle, snapshot, or acceptance artifact for E5A may be created early.
### 1.5 Consumed evidence IDs

The stage manifest must bind at least these evidence IDs:

- `O2-O0Q-001`;
- `O2-S10-001`;
- `O2-P2-001`;
- `O2-S11-001`;
- `O2-S12-001`;
- `O2-S13-001` when the selected plan contains MetaKernel MVM;
- `O2-S14-001` when the selected plan contains MetaKernel Conv;
- early `O2-LAYOUT-001` evidence applicable to the selected layout;
- `O2-S15A-001` as the governing CKKS state contract.
### 1.6 Stale triggers

The stage stops and all unpublished results are discarded if any of these inputs changes:

- master or ADR authority affecting ownership, fixed `N`, or placement policy;
- the `O2-O0Q-001` qualification fingerprint;
- source, configuration, support, diagnostic, or fixture hashes;
- the accepted record schema, version, capability, or identity rules;
- canonical graph, PU, node, edge, call, or value identities;
- selected layout, slot map, iteration space, transformed-data identity, or lineage;
- fixed `N`, key domain, security intent, bootstrap policy, or scale intent;
- CKKS transfer rules, range model, error model, or component-count rules;
- protected-site manifest or baseline-DP provenance;
- the ACE rtlib/ANT capability fingerprint used by downstream planning;
- a verifier version or normalization rule that contributes to the snapshot digest.

The stale report must name the first changed input, affected snapshot IDs, downstream invalidations, and the required targeted requalification path.
## 2. Objective and User-Visible Capability

O2-E5A establishes exactly one deterministic, immutable, provider-neutral canonical CKKS state snapshot for the layout selected by O2-E4 and for the program graph accepted by O2-E3.

At exit, each materializable value and each explicit CKKS action boundary has:

- a stable value or edge identity;
- the selected layout and slot-map identity;
- the unchanged requested ring dimension `N`;
- a key and encryption domain identity;
- a symbolic scale identity and a verified scale requirement;
- a logical level or chain requirement without invented final Q/P primes;
- a legal component count;
- conservative value-range and absolute/relative error bounds;
- explicit pending actions and consumer obligations;
- protected-site and baseline-DP provenance references where applicable;
- a deterministic state identity and snapshot digest.

The capability visible to the next stage is a verified pre-ReSBM snapshot. It is not a selected ReSBM schedule, a final CKKS parameter set, or an executable ANT program.
## 3. Scope and Boundaries

### 3.1 In scope

This stage includes:

- importing the independently reopened O2-E4 selected layout;
- rejecting every provisional state attached to non-selected candidates;
- rebuilding CKKS state from canonical graph facts and selected layout facts;
- validating fixed `N` and `S=N/2` capacity without changing `N`;
- binding every state to value, layout, slot, key-domain, and parameter-intent identities;
- applying the accepted provider-neutral transfer rules from O2-E3;
- validating scale, logical level, component count, range, and error joins;
- representing the transient three-component `MulCC` result only on the immediate transition to `Relinearize`;
- proving immediate `MulCC -> Relin` adjacency and preventing duplicate Relin;
- importing protected-site and baseline-DP provenance identities without running or modifying baseline DP;
- producing an immutable canonical pre-ReSBM snapshot;
- writing that snapshot through the accepted O2-E2 transaction;
- producer-free independent reopen and verification;
- matching `.B` and same-stem `.T` review evidence;
- stable negative diagnostics, deterministic output, and failure injection;
- a precise handoff to O2-E5B.
### 3.2 Explicit non-goals

O2-E5A does not:

- run ReSBM region construction, ScaleMgr, min-cut, or endpoint DP;
- select, replace, remove, move, or merge any bootstrap site;
- run the baseline DP algorithm;
- apply baseline-DP or ReSBM placement actions to produce post-state;
- finalize the Q chain, P primes, CRT basis, or bootstrap profile;
- perform a security-estimator decision for a final parameter set;
- project any state to `CKKS_PARAMS`;
- build, link, load, query, or execute ACE rtlib or ANT;
- create logical or ANT-expanded final key manifests;
- materialize standard calls or generated C;
- add a direct or indirect rtlib dependency to `be.so`;
- implement MetaKernel, Fhelipe, ReSBM, FHEFusion, HPOLY, or HPAO;
- change the selected layout or transformed plaintext data;
- change requested `N`, silently shard to fit, or recommend a different `N` as if it were selected;
- expose the transient three-component multiply result to ordinary SSA users;
- permit ordinary WOPT to move, duplicate, remove, or speculate CKKS actions;
- claim Stage 1 acceptance or O2 completion.
### 3.3 Inherited invariants

The following invariants are mandatory:

- binary WHIRL remains the process boundary;
- Open64 owns CKKS-and-above semantics and persisted planning state;
- ACE AIR is not imported as a second physical IR universe;
- no new WHIRL operator, table, image, or section is allocated without the accepted common/com compatibility review;
- requested `N` exists before canonicalization and never changes in O2;
- physical slot capacity is `S=N/2`;
- selected layout and slot classes are immutable inputs to this stage;
- every `MulCC(two,two)` has an instantaneous three-component result followed immediately by mandatory `Relin` to two components;
- no ordinary SSA consumer may observe the transient three-component result;
- `Mul_ciph` followed by an explicit second Relin is forbidden downstream;
- joins use explicit alignment actions and cannot silently coerce states;
- protected/manual/pre-ReLU boundaries remain protected;
- no secret key, decryptor, or secret-derived private material is persisted;
- failure cannot publish a snapshot that appears complete;
- every meaningful WHIRL artifact has a matching same-stem `.T` generated by `ir_b2a -st -src`.
### 3.4 Support scopes

The stage consumes support rows with these meanings:

| Scope | E5A responsibility |
| --- | --- |
| `o0-handoff-required` | Validate and consume fixed `N`, configuration, protected sites, baseline provenance, and shared state contracts. |
| `shared-o0-o2-comparison-required` | Use the same canonical graph, fixed `N`, security intent, and input domains used by the qualified baseline. |
| `o2-core-only` | Build the selected-layout canonical pre-ReSBM snapshot and its cross-process proof. |
| `o2-extension-only` | Reject any extension whose E1 support row and exact oracle are not accepted. |
| `fallback-required` | Return control to the complete qualified baseline profile; do not publish a mixed state snapshot. |
## 4. Dependency Graphs

### 4.1 Implementation and build dependencies

The implementation dependency chain is:
```text
current O2-O0Q-001 qualification
  -> E1 claim and fixture locks
  -> E2 record and atomic transaction APIs
  -> E3 CKKS state, transfer, effect, and verifier substrate
  -> E4 selected layout and slot-map record
  -> E5A selected-layout canonical pre-ReSBM state
  -> E5B ReSBM and protected adapter
```
E5A may reuse accepted interfaces from these components:

- `osprey/common/com/dsl_fhe.h` and `dsl_fhe.cxx`;
- `osprey/common/com/dsl_fhe_plan.h` and `dsl_fhe_plan.cxx`;
- `osprey/common/com/dsl_fhe_plan_print.cxx`;
- the accepted O2-E2 record serializer and validator;
- the accepted O2-E3 state-transfer and effect library;
- the accepted O2-E4 selected-layout reader;
- `osprey/be/vho/dsl_opt.h` and `dsl_opt.cxx` only through reviewed extension points;
- `osprey/be/vho/fhe_convert.h` and `fhe_convert.cxx` only at the accepted gatekeeper boundary;
- the existing all-PU atomic checkpoint pattern in `osprey/be/be/driver.cxx`.

Proposed new component paths must be finalized by the E3/E5A owner before the first source commit. A proposed split is:
```text
osprey/be/vho/fhe_o2_pre_resbm_state.h
osprey/be/vho/fhe_o2_pre_resbm_state.cxx
osprey/be/vho/fhe_o2_pre_resbm_verify.h
osprey/be/vho/fhe_o2_pre_resbm_verify.cxx
osprey/be/vho/tests/run_ckks_state_contract.py
osprey/be/vho/tests/run_e5a_selected_layout_gate.py
testdata/fhe_o2/ckks/
testdata/fhe_o2/e5a-pre-resbm/
```
These are proposed implementation locations, not claims that the files exist.
### 4.2 Build and link impact

The intended implementation is provider-neutral C++ inside existing Open64 backend and common/com targets.

- No ACE rtlib header is included by the E5A implementation.
- No `FHErt_common` or `FHErt_ant` symbol is referenced.
- No new defined or undefined rtlib symbol is added to `be.so`.
- No private ACE compiler or AIR header is included.
- No frontend-only builder library leaks into `be.so`.
- Any new common/com physical record requires the accepted compatibility review before code lands.
- A proposed dependency change must trigger a separate `be.so` consumer and link-closure review; it cannot be approved by this stage plan.

The build review must compare defined and undefined symbols for `be.so` and its known consumers before and after E5A. Any unexpected new dependency stops the stage.
### 4.3 Compiler pass-order dependency

The compiler execution order is distinct from the implementation queue:
```text
canonical Open64 FHE graph
  -> selected Fhelipe or MetaKernel layout
  -> E5A recompute canonical pre-ReSBM CKKS state
  -> import baseline-DP placement and protected provenance
  -> optional E5B ReSBM replacement
  -> verify post-ReSBM state
  -> E5C final parameters, security, keys, and ANT projection
```
E5A must run after layout selection and before either placement replacement or final parameter projection. A state snapshot computed on a losing candidate layout is never promoted by changing only a status flag.
### 4.4 External evidence and environment

Required evidence includes:

- a supported Open64 build able to write and reopen binary WHIRL;
- an `ir_b2a` build with both `-st` and `-src` support;
- retained original source files at the DST-recorded paths;
- E1 fixture manifests and deterministic generators;
- independently reopenable E4 selected plans;
- accepted fixed-N and state-transfer fixtures from E3;
- a host artifact directory that remains available after test completion.

No ANT installation is needed to run the provider-neutral E5A tests.
## 5. Interface and Data Contracts

### 5.1 Authoritative inputs

| Input | Owner | E5A use | Mutation rule |
| --- | --- | --- | --- |
| `FHECompilationConfigIR` | O0/common-com | Fixed `N`, security intent, bootstrap policy, precision and scale constraints | Read-only; `N` cannot change. |
| Canonical graph | P0/E3 | Values, operations, calls, edges, effects, lineage | Read-only. |
| Selected layout | E4 | Layout, slot map, class map, iteration space, transformed-data IDs | Read-only; losing candidates are ignored. |
| CKKS transfer schema | E3 | State construction, joins, action preconditions, range/error transfer | Read-only, version-bound. |
| Protected-site manifest | O0 | Protected/manual/pre-ReLU identities and semantic boundaries | Read-only; no site is moved or removed. |
| Baseline-DP provenance | O0 | Objective/config/source identity required by E5B | Read-only; no placement action is executed here. |
| Provider capability fingerprint | P1a/P1b/O0Q | Staleness and downstream compatibility reference | Read-only; no provider query occurs. |
### 5.2 State classes and identity

The accepted E3 schema must distinguish at least these semantic statuses:

| Status | Meaning | Publishable by E5A |
| --- | --- | --- |
| `PROVISIONAL_CANDIDATE` | Estimate associated with an unselected or candidate layout | No |
| `CANONICAL_PRE_RESBM` | Recomputed state on the one selected layout before placement actions | Yes |
| `SELECTED_POST_RESBM` | State after selected baseline-DP or ReSBM actions | No |
| `FINAL_RESOLVED` | State bound to final Q/P/CRT, security, and provider projection | No |

Status participates in state identity. Two records with identical numeric fields but different statuses cannot share an identity.

Each canonical pre-ReSBM state must contain or reference:

- schema and capability version;
- state status;
- compilation-config ID;
- canonical graph and PU ID;
- node, edge, call, and value/version IDs as applicable;
- selected-plan ID and selected-layout ID;
- slot-map and iteration-space IDs;
- unchanged fixed `N` and derived `S=N/2`;
- encryption and key-domain IDs;
- parameter-intent or accepted provisional-parameter identity;
- logical level and remaining-depth requirement;
- symbolic scale identity and scale bit requirement;
- provider-native scale only if inherited as verified evidence, never invented;
- component count;
- value-range bound;
- accumulated absolute and relative error bounds;
- plaintext compatibility class;
- pending non-relin action and consumer obligations;
- source lineage and reason/provenance IDs;
- protected-site and baseline-DP provenance references when applicable.

Final concrete Q/P prime IDs, CRT tables, achieved-security decisions, runtime objects, and ANT-expanded keys are not E5A fields unless they already exist as immutable qualified input references. E5A cannot synthesize or finalize them.
### 5.3 Snapshot contract

The semantic snapshot is one immutable all-PU object. Its digest covers:

- source and canonical graph fingerprints;
- qualification ID;
- configuration and fixed-N identity;
- selected-plan, layout, slot-map, and transformed-data fingerprints;
- state-transfer, range, error, and normalization versions;
- ordered state and action-boundary records;
- protected-site and baseline-DP provenance identities;
- support and diagnostic catalog hashes;
- downstream provider capability fingerprint;
- producer tool and build identity.

The physical representation must use the E2/P2 accepted storage decision. E5A does not name or allocate a new `WT_*`, ELF section, table, or public operator.
### 5.4 Proposed semantic APIs

The following signatures are proposed semantic interfaces. Their exact C++ types and physical owners require E3/common-com review before implementation:
```text
BuildCanonicalPreResbmState(
    const CanonicalFheGraphView& graph,
    const SelectedLayoutView& layout,
    const FheCompilationConfigView& config,
    const ProtectedPlacementView& protected_sites,
    PreResbmSnapshotBuilder* output,
    FheDiagnosticSink* diagnostics)

VerifyCanonicalPreResbmState(
    const CanonicalFheGraphView& graph,
    const SelectedLayoutView& layout,
    const PreResbmSnapshotView& snapshot,
    FheDiagnosticSink* diagnostics)

PublishCanonicalPreResbmState(
    const PreResbmSnapshotView& verified_snapshot,
    O2PlanTransaction* transaction,
    FheDiagnosticSink* diagnostics)
```
Required API behavior:

- input views are immutable;
- builders cannot publish before whole-program verification;
- the first diagnostic is deterministic;
- failure returns no valid snapshot ID;
- no pointer or producer-memory identity enters persisted data;
- iteration order is stable and independent of addresses or thread timing;
- the verifier independently recomputes derived fields where practical;
- reopening does not require the producer process or its memory.
### 5.5 Transfer and join rules

The stage reuses one E3 transfer implementation. It does not create a second state engine for the selected layout.

Required rules include:

- Cipher and Plain classifications are explicit;
- add joins require identical key domain and compatible layout, level, scale, components, range, and error after explicit alignment actions;
- rotations preserve state except for explicitly modeled error and key needs;
- masks and plaintext operations use the selected layout and plaintext compatibility class;
- every capacity calculation uses checked arithmetic;
- fanout preserves one immutable input state unless an explicit versioned mutation is represented;
- phi and call joins are explicit and keyed by stable edge/call identities;
- unknown frequency remains unknown and is never treated as zero;
- unreachable frequency zero requires an `unreachable-proof` provenance;
- protected boundaries remain barriers in the state/action graph.
### 5.6 Immediate Relin contract

For each canonical ciphertext-ciphertext multiplication:
```text
two-component left state
  + two-component right state
  -> MulCC transient three-component transition state
  -> immediately adjacent Relinearize action
  -> ordinary two-component result state
```
The verifier must reject:

- a missing `Relinearize` action;
- any ordinary consumer between `MulCC` and `Relinearize`;
- a phi, call, store, or scheduling barrier between the pair;
- a second explicit Relin on the two-component result;
- a candidate-only transient state published as canonical;
- a normal SSA state with three components;
- a fused-provider assumption that hides the semantic adjacency.

E5A records provider-neutral semantics. The exact ANT mapping `Mul_ciph3 -> Relin` is checked later by E5C.
### 5.7 Range and error contract

Range and error bounds must be conservative and deterministic.

- Every source bound names its provenance.
- Every transfer records input bounds, rule ID, and output bounds.
- NaN, Inf, negative error, overflow, or unordered bounds fail closed.
- A join takes only the accepted conservative merge; it cannot choose a tighter observed result after seeing a test output.
- Bootstrap profile input requirements may be recorded as obligations, but no bootstrap transition is applied here.
- Final achieved precision and security remain E5C responsibilities.
### 5.8 Serialization and independent reopen

Persisted records must use:

- fixed-width fields;
- invalid-zero IDs;
- checked `first/count` ranges;
- no pointers;
- no host-sized enums;
- explicit schema and capability versions;
- compile-time size assertions where physical records are used;
- deterministic ordering and normalized printing.

The proof sequence is:

1. build the snapshot from accepted inputs;
2. verify every PU and cross-PU reference;
3. atomically publish through the E2 transaction;
4. exit the producer process;
5. reopen source, selected plan, and snapshot in a new process;
6. validate every ID, range, hash, and cross-record link;
7. regenerate a normalized digest;
8. compare the digest and printed structure with the producer result;
9. create same-stem `.T` evidence with `ir_b2a -st -src`.

Producer-memory inspection is not reopen evidence.
### 5.9 Backward and feature-absent compatibility

- Old WHIRL without O2 records must retain its accepted reader behavior.
- Feature-absent O2 input must not fabricate an E5A snapshot.
- Unknown required versions or capabilities fail closed.
- Unknown optional fields follow the accepted P2 compatibility rule.
- E5A records cannot become mandatory for O0 runtime execution.
- `ir_a2b` and `ir_b2a` compatibility remains part of any physical change.
- An accepted existing image may be extended only through its reviewed version and capability mechanism.
### 5.10 Diagnostics and fallback

The E1 diagnostic catalog is authoritative. Proposed diagnostic purposes are:

| Purpose | Proposed stable diagnostic |
| --- | --- |
| Qualification or selected-plan fingerprint is stale | `FHE-O2-PRE-RESBM-STALE` |
| Candidate state was presented as canonical input | `FHE-O2-PRE-RESBM-PROVISIONAL-STATE` |
| Fixed `N` differs from the qualified request | `FHE-O2-FIXED-N-MISMATCH` |
| Selected layout exceeds `S=N/2` capacity | `FHE-O2-LAYOUT-CAPACITY` |
| Layout, slot, value, or lineage identity disagrees | `FHE-O2-PRE-RESBM-IDENTITY` |
| State join cannot be made explicit and legal | `FHE-O2-CKKS-ILLEGAL-JOIN` |
| `MulCC` lacks immediate Relin | `FHE-O2-CKKS-RELIN-NOT-IMMEDIATE` |
| Duplicate Relin follows a two-component result | `FHE-O2-CKKS-DUPLICATE-RELIN` |
| Range or error arithmetic is invalid or non-conservative | `FHE-O2-CKKS-BOUND-INVALID` |
| Protected-site provenance is absent or changed | `FHE-O2-PROTECTED-SITE-STALE` |
| Atomic snapshot publication fails | `FHE-O2-PRE-RESBM-PUBLISH-FAILED` |

The exact spelling must match the accepted E1 catalog. This table does not allocate diagnostics by itself.

Fallback is the complete qualified O0 profile only when the accepted ADR and support row permit it. E5A never publishes a partial O2 snapshot and never combines candidate layout state with baseline layout state.
## 6. Detailed Work Breakdown

Each item requires an owner, reviewer, tests, artifacts, and merge rule in the stage work tracker.

| Task ID | Concrete change | Components | Dependency type | Tests and artifacts | Owner and reviewer | Merge or exit rule |
| --- | --- | --- | --- | --- | --- | --- |
| `E5A-W01` | Freeze the consumed IDs, schema versions, support rows, diagnostic IDs, and stale triggers. | E5A manifest and traceability table | Governance and implementation | Manifest normalization and stale-input negatives | Stage owner; architecture reviewer | No pending authority field or moving hash. |
| `E5A-W02` | Audit E3 state/status rules and E2 physical ownership; resolve gaps without allocating a second record family. | common/com crosswalk and API review | Implementation | Record-owner report and compatibility review | CKKS owner; common/com reviewer | One owner per field and accepted version path. |
| `E5A-W03` | Define the selected-layout import adapter and complete identity checks. | E4 reader and E5A builder | Implementation | Baseline/MetaKernel positive cases and stale layout negatives | Layout owner; state reviewer | Every selected input has one verified layout and slot map. |
| `E5A-W04` | Define candidate-state rejection and canonical state rebuilding. | E5A builder | Compiler pass order | Candidate poison fixtures and recomputation trace | CKKS owner; oracle reviewer | No candidate field is copied without independent derivation. |
| `E5A-W05` | Bind fixed `N`, `S=N/2`, parameter intent, and capacity checks. | Config/state adapter | Implementation | Fixed-N, exact-capacity, one-over-capacity, overflow tests | CKKS owner; crypto reviewer | Zero `N` mutation and checked arithmetic. |
| `E5A-W06` | Bind value, edge, call, layout, slot, lineage, and key-domain identities. | State builder and verifier | Implementation | Cross-PU/call/phi/fanout identity fixtures | State owner; common/com reviewer | No missing, dangling, duplicate, or cross-domain identity. |
| `E5A-W07` | Apply scale, logical-level, component, plaintext, range, and error transfer rules. | Accepted E3 transfer library | Compiler pass order | Independent transfer tables and boundary values | E3 owner; crypto reviewer | Production and oracle fields match exactly. |
| `E5A-W08` | Enforce canonical `MulCC -> Relin` adjacency and two-component ordinary values. | State graph verifier | Compiler pass order | Missing, delayed, consumer-interposed, and duplicate Relin negatives | State owner; WOPT reviewer | All positives exact; first diagnostic exact for negatives. |
| `E5A-W09` | Import protected-site and baseline-DP provenance identities without applying placement. | O0 handoff adapter | Implementation | Protected/manual/pre-ReLU and stale-site fixtures | O0 owner; boundary reviewer | Identity/order/state preserved; no placement mutation. |
| `E5A-W10` | Implement all-PU snapshot verification and deterministic normalization. | E5A verifier | Implementation | Two clean runs, PU-order permutation, thread-order fixtures | Reliability owner; independent reviewer | Byte or approved structural determinism and equal digest. |
| `E5A-W11` | Integrate atomic publication through the E2 transaction. | Backend driver checkpoint | Implementation | Failure at each publication step | Transaction owner; reliability reviewer | Failure leaves no valid-looking snapshot. |
| `E5A-W12` | Implement producer-free reopen and corruption checks. | common/com reader and verifier | Implementation | Header/count/range/ID/hash corruptions | Serialization owner; independent reviewer | All corruptions fail closed before downstream use. |
| `E5A-W13` | Add WOPT non-interference coverage on selected-layout state and actions. | WOPT bridge tests | Compiler pass order | CSE/PRE/DCE/hoist/speculation negatives | WOPT owner; CKKS reviewer | No ordered action is moved, merged, removed, or duplicated. |
| `E5A-W14` | Add secret-material and dependency scans. | Build/test tooling | Build and security | Header/symbol scan and artifact content scan | Build owner; security reviewer | No rtlib dependency, secret, decryptor, or private key artifact. |
| `E5A-W15` | Produce matching `.B`/`.T`, traces, reports, and SHA-256 inventory. | Evidence runner | Verification | Full stage command and independent review | Test owner; evidence reviewer | Bundle complete, deterministic, and retained. |
| `E5A-W16` | Review and sign the E5B handoff. | Handoff manifest | Exit | Consumer dry-run that performs validation only | E5A and E5B owners | E5B can reopen and validate without running ReSBM. |
## 7. Proposed Commit and PR Sequence

Each commit has one semantic goal and maps to governing commit group `S1-8A`.

1. `e5a-contract`: add the accepted manifest, traceability rows, proposed APIs,
diagnostics, and tests that initially fail for missing implementation.
2. `e5a-selected-layout-import`: validate E4 selected layout and reject stale or
provisional candidate identities.
3. `e5a-state-recompute`: rebuild provider-neutral state with fixed-N, scale,
level, component, range, and error rules.
4. `e5a-immediate-relin`: enforce transient component-three state, adjacency,
and duplicate-Relin negatives.
5. `e5a-protected-provenance`: bind protected-site and baseline-DP identities
without executing or changing placement.
6. `e5a-transaction-reopen`: add atomic all-PU publication, independent reopen,
corruption checks, and deterministic normalization.
7. `e5a-wopt-security`: add optimizer non-interference, dependency, and secret
scans.
8. `e5a-acceptance-evidence`: add only the stage runner, evidence schema, and
signed decision template; no algorithm change is permitted in this commit.

Contract tests land with or before implementation. A common/com physical change must be isolated in its own reviewed commit. An optional support extension must have its own plan amendment, support row, oracle, and commit.

No generated run artifacts are committed unless an accepted test contract explicitly designates a golden file.
## 8. Verification Plan

All commands below are proposed target commands. They are exact planned interfaces and do not claim that the scripts, fixtures, or build targets exist.
### 8.1 Governing state-contract test

Test ID: `O2-S15A-001`

Proposed command:
```text
python3 osprey/be/vho/tests/run_ckks_state_contract.py --mode pre-resbm --build-dir build --manifest testdata/fhe_o2/ckks/SHA256SUMS --artifacts test-artifacts/o2/O2-S15A-001
```
Input classes:

- fixed-N requested records;
- selected baseline and MetaKernel layouts;
- layout-independent and selected-layout transfer cases;
- uniform and explicitly unsupported requirements;
- bootstrap barriers;
- `MulCC -> Relin` positives and negatives;
- call, phi, fanout, and critical-edge cases;
- WOPT PRE/CSE/DCE/hoist/speculation cases.

Required result:

- requested `N` is unchanged;
- state, transfer, effect, range, and error traces match exactly;
- immediate Relin adjacency is exact;
- canonical ordinary states have two components;
- provisional and canonical state identities never alias;
- WOPT shows complete non-interference for ordered actions;
- first diagnostics match the accepted catalog.

Protocol:

- seed `0x434b4b53`;
- fixed `N` in `{2^12,2^13,2^14,2^15,2^16}`;
- chain requirement length `1..16`;
- fanout `1..8`;
- zero warmups;
- one deterministic transfer run;
- timeout 20 minutes;
- provider-free capability fixtures;
- no numeric tolerance for state or structure;
- conservative bound checks use exact persisted rational or integer forms.
### 8.2 Selected-layout integration and reopen test

Test ID: `O2-E5A-001`

Proposed command:
```text
python3 osprey/be/vho/tests/run_e5a_selected_layout_gate.py --build-dir build --manifest testdata/fhe_o2/e5a-pre-resbm/SHA256SUMS --mode integration --artifacts test-artifacts/o2/O2-E5A-001
```
Required result:

- E4 selected layout imports exactly;
- poisoned candidate state does not affect canonical output;
- every selected-layout value has one state identity;
- two clean productions have identical normalized snapshots;
- a new process reopens and verifies source, selected plan, and snapshot;
- baseline and MetaKernel input families use the same fixed-N rules;
- no ReSBM or provider operation is invoked.

Bounds and protocol:

- seed `0x45354131`;
- `1..8` PUs;
- up to `2000` canonical operations;
- up to `16` logical chain requirements;
- two clean producer runs;
- one independent consumer run per producer result;
- timeout 20 minutes;
- retain all evidence for at least 180 days.
### 8.3 Atomicity and corruption test

Test ID: `O2-E5A-002`

Proposed command:
```text
python3 osprey/be/vho/tests/run_e5a_selected_layout_gate.py --build-dir build --manifest testdata/fhe_o2/e5a-pre-resbm/SHA256SUMS --mode failure-and-corruption --artifacts test-artifacts/o2/O2-E5A-002
```
Required cases:

- failure before snapshot allocation;
- failure after each PU is staged;
- failure before cross-PU verification;
- failure before and during atomic commit;
- corrupted header, count, range, ID, status, and digest;
- stale graph, config, qualification, layout, slot, and protected-site hashes;
- producer termination before publication and after publication.

Pass rule: no failure leaves an artifact that the independent consumer accepts as a complete canonical pre-ReSBM snapshot.
### 8.4 WOPT non-interference test

Test ID: `O2-E5A-003`

Proposed command:
```text
python3 osprey/be/vho/tests/run_e5a_selected_layout_gate.py --build-dir build --manifest testdata/fhe_o2/e5a-pre-resbm/SHA256SUMS --mode wopt-noninterference --artifacts test-artifacts/o2/O2-E5A-003
```
Pass rule: PRE, CSE, DCE, hoisting, speculation, and copy propagation do not move, duplicate, remove, merge, or expose an ordered state action or the `MulCC -> Relin` boundary.
### 8.5 WHIRL evidence generation

Test ID: `O2-E5A-004`

Proposed commands:
```text
ir_b2a -st -src test-artifacts/o2/O2-E5A-001/selected-layout-pre-resbm.B test-artifacts/o2/O2-E5A-001/selected-layout-pre-resbm.T
python3 osprey/be/vho/tests/run_e5a_selected_layout_gate.py --build-dir build --manifest testdata/fhe_o2/e5a-pre-resbm/SHA256SUMS --mode evidence-audit --artifacts test-artifacts/o2/O2-E5A-004
```
The original source must remain at the path recorded in DST. If `-src` is not available, the test is `Unverified`; it must not silently omit source evidence.
### 8.6 Build, dependency, and secret scan

Test ID: `O2-E5A-005`

Proposed command:
```text
python3 osprey/be/vho/tests/run_e5a_selected_layout_gate.py --build-dir build --manifest testdata/fhe_o2/e5a-pre-resbm/SHA256SUMS --mode dependency-and-secret-scan --artifacts test-artifacts/o2/O2-E5A-005
```
Pass rule:

- expected Open64 targets rebuild;
- `be.so` has no new rtlib-defined or undefined symbol;
- no ACE AIR or private compiler header is included;
- retained `.B`, `.T`, JSON, traces, logs, and manifests contain no secret key, decryptor, or private key bytes;
- provider-neutral tests do not load `FHErt_common` or `FHErt_ant`.
### 8.7 Verification matrix

| Test ID | Source and input identity | Expected result | Seed and bounds | Platform/capability | Samples and timeout | Required artifacts | Owner and pass rule |
| --- | --- | --- | --- | --- | --- | --- | --- |
| `O2-S15A-001` | E3 schema plus selected-layout cases | Exact state/effect/relin result | `0x434b4b53`; stated fixed-N and chain bounds | Supported Open64 host; provider-free | 0 warmups, 1 run, 20 min | Config, states, actions, optimizer dumps, `.B`, `.T` | CKKS owner; zero structural mismatch |
| `O2-E5A-001` | Current O0Q, E2, E3, E4 hashes | Deterministic canonical snapshot and reopen | `0x45354131`; 1..8 PUs, <=2000 ops | Provider-free | 2 producers, 1 reopen each, 20 min | Input plan, snapshot, digest, `.B`, `.T` | Stage owner; exact digest and identity match |
| `O2-E5A-002` | Failure and corruption corpus | Fail closed, no partial publication | Every publication step and record field class | Provider-free | 1 run per injection, 20 min | Failure logs and rejected images | Reliability owner; exact first diagnostic |
| `O2-E5A-003` | WOPT transform corpus | Ordered actions unchanged | Fanout 1..8, phi/call/edge cases | Provider-free | 1 run, 20 min | Before/after dumps and trace | WOPT owner; zero illegal rewrite |
| `O2-E5A-004` | Accepted E5A binary and source | Same-stem `.T`, stable normalized view | Deterministic | `ir_b2a -st -src` required | 1 audit, 10 min | `.B`, `.T`, audit report | Evidence owner; complete source cross-reference |
| `O2-E5A-005` | Build and artifact inventory | No dependency or secret leak | Complete touched-target and artifact set | Supported build host | 1 clean comparison, 20 min | Symbol, include, and secret-scan reports | Build/security owners; zero leak |
## 9. Negative, Failure, and Compatibility Matrix

| Case | Required behavior | First evidence | Publication result |
| --- | --- | --- | --- |
| Qualification fingerprint changed | Stop and request targeted requalification | Stale-input report | None |
| E4 selected-plan hash changed | Reject before state construction | Selected-layout mismatch | None |
| Provisional candidate state injected with plausible values | Recompute from canonical input and reject attempted promotion | Poison-field comparison | None on mismatch |
| Fixed `N` missing or changed | Fail closed and preserve requested configuration | Fixed-N diagnostic | None |
| Slot usage equals `S=N/2` | Accept if every slot/class rule is legal | Capacity trace | Canonical snapshot allowed |
| Slot usage exceeds `S=N/2` by one | Reject; may report recommended N only as a diagnostic | Checked-capacity report | None |
| Integer overflow in slot or bound arithmetic | Reject before any record publication | Overflow diagnostic | None |
| Layout or slot-map ID does not match the selected plan | Reject | Identity crosswalk | None |
| Ambiguous or missing value lineage | Reject before transfer | Lineage report | None |
| Cross-key-domain join | Reject unless an explicit accepted KeySwitch action exists | Join trace | None |
| Scale or level mismatch at join | Require explicit legal action or reject | Join trace | None |
| Range or error bound is NaN, Inf, negative, or non-conservative | Reject | Bound trace | None |
| `MulCC` has no adjacent Relin | Reject | Action adjacency trace | None |
| Ordinary consumer reads transient component-three result | Reject | Consumer-edge trace | None |
| Two-component result receives another Relin | Reject duplicate Relin | Action adjacency trace | None |
| Provider-fused `Mul_ciph` assumption appears | Reject semantic shortcut; E5A remains provider-neutral | Operation trace | None |
| Protected/manual/pre-ReLU identity changed | Reject and return to owning stage | Protected-site comparison | None |
| Baseline-DP provenance missing | Reject E5B handoff | Provenance crosswalk | None |
| ReSBM action appears in input or output | Reject stage-boundary violation | Operation census | None |
| Final Q/P/CRT or runtime object is synthesized | Reject ownership violation | Record-field audit | None |
| ACE rtlib header or symbol appears in E5A implementation | Stop for architecture/build review | Include/symbol report | None |
| Server secret, decryptor, or private material appears | Reject and quarantine evidence | Secret scan | None |
| Unknown required record version | Reject before read use | Version diagnostic | None |
| Feature-absent legacy WHIRL | Preserve accepted legacy behavior without fabricating E5A data | Compatibility report | No E5A snapshot |
| Failure during any all-PU publication step | Roll back the staged transaction | Failure-injection log | No valid-looking snapshot |
| Two clean runs differ | Reject determinism claim and retain both bundles | Normalized diff | Neither accepted |
| Independent process cannot reopen | Mark stage `Unverified` | Reopen log | Not accepted |
## 10. Evidence and Retention

The accepted `O2-E5A-EXIT` bundle must contain at least:
```text
qualification-reference.json
master-and-adr-lock.json
source-lock.json
support-matrix.json
diagnostics.json
requested-configuration.json
canonical-graph-reference.json
selected-layout-plan.B
selected-layout-plan.T
layout-and-slot-map.json
protected-sites.json
baseline-dp-provenance.json
pre-resbm-state-schema.json
pre-resbm-state-trace.json
pre-resbm-action-boundaries.json
range-and-error-proof.json
immediate-relin-audit.json
wopt-noninterference.json
selected-layout-pre-resbm.B
selected-layout-pre-resbm.T
selected-layout-pre-resbm.ckks-state.t
producer-normalized-digest.json
consumer-normalized-digest.json
independent-reopen-report.json
corruption-and-failure-tests.json
be-symbol-diff.txt
header-allowlist-report.json
secret-scan.json
stage-decision.md
SHA256SUMS
```
`selected-layout-pre-resbm.T` must be generated from `selected-layout-pre-resbm.B` with `ir_b2a -st -src`. The phase trace uses the non-colliding name `selected-layout-pre-resbm.ckks-state.t`.

Failed development evidence is retained at least 30 days. Milestone evidence is retained at least 180 days. An accepted stage bundle and its governing fingerprints are retained for the repository lifetime.

Artifacts are published atomically. A failed run may retain diagnostic logs and rejected inputs, but it may not leave partial output under an accepted `.B` name. The runner cleans the designated artifact directory at the start of the next run, not at the end of the current run.
## 11. Exit Decision

### 11.1 Accepted

`O2-E5A-EXIT=Accepted` requires all of the following:

- every entry gate remains current;
- `O2-S15A-001` passes on the selected-layout integration suite;
- every materializable value has exactly one canonical pre-ReSBM state;
- all state identities bind the selected layout and fixed `N`;
- provisional candidate facts cannot enter the published snapshot;
- all scale, level, component, range, error, call, phi, and fanout joins verify;
- every `MulCC` has only a transient three-component result and immediate Relin;
- no duplicate Relin or ordinary component-three SSA value exists;
- protected-site and baseline-DP provenance identities are unchanged;
- two clean productions are deterministic;
- independent producer-free reopen verifies every record and digest;
- matching `.B` and same-stem `.T` evidence exists;
- failure and corruption tests leave no partial valid-looking artifact;
- WOPT non-interference tests pass;
- build scans show no new rtlib dependency or private ACE header;
- secret scans show no secret key, decryptor, or private material;
- independent reviewers sign the stage decision.

The exact completion wording is:
```text
O2-E5A accepted: the selected layout has one verified, immutable,
provider-neutral canonical pre-ReSBM CKKS state snapshot.
```
This wording does not claim that ReSBM, final CKKS resolution, ANT projection, materialization, Stage 1, or O2 is accepted.
### 11.2 Rejected

`O2-E5A-EXIT=Rejected` means an implemented input or stage behavior violates an accepted contract. The decision names the owning upstream or E5A work item, the first diagnostic, retained evidence, and the required correction.

No E5B work begins from a rejected snapshot.
### 11.3 Unverified

`O2-E5A-EXIT=Unverified` means required authority, environment, tool, provider- neutral capability evidence, independent reopen, or reviewer evidence is missing. Unverified does not permit provisional E5B work.
## 12. Rollback, Invalidation, and Stop Rules

Rollback disables the E5A increment and returns to the complete qualified O0 profile. It does not select an old candidate snapshot, weaken the fixed-N rule, or mix a MetaKernel layout with baseline-layout state.

An accepted E5A snapshot is invalidated by any graph, layout, slot, state-rule, config, protected-site, or fingerprint change. Invalidation propagates to all E5B/E5C plans, keys, costs, materialization, and acceptance evidence that names the snapshot.

Stop and return to architecture or common/com review if:

- two owners or no owner exist for a state field;
- a new physical record, section, or operator is required without review;
- the implementation needs ACE AIR as semantic state;
- the implementation needs a direct `be.so` rtlib dependency;
- fixed `N` cannot be preserved;
- final Q/P/CRT must be selected before E5B decisions;
- a provider-specific shortcut is needed to represent `MulCC -> Relin`;
- protected/manual/pre-ReLU boundaries cannot be represented exactly;
- independent reopen requires producer memory;
- deterministic normalization cannot be defined;
- an extension lacks an accepted support row and independent oracle;
- secret material would need to enter a compiler or retained artifact.
## 13. Exact O2-E5B Handoff

E5A publishes one handoff manifest containing:

- `O2-E5A-EXIT` decision and snapshot ID;
- current `O2-O0Q-001` qualification ID;
- governing master, ADR, O2 plan, execution-index, and schema fingerprints;
- canonical graph, selected-plan, selected-layout, slot-map, and iteration-space identities;
- fixed `N`, derived `S=N/2`, key domain, security intent, bootstrap policy, and parameter-intent identity;
- ordered canonical pre-ReSBM value states;
- explicit action-boundary and join records;
- immediate-Relin proof for every `MulCC`;
- range and error proof identities;
- protected/manual/pre-ReLU site identities and constraints;
- baseline-DP objective/config/provenance references;
- support rows and diagnostic catalog hash;
- provider capability fingerprint for staleness only;
- producer and independent-consumer normalized digests;
- matching `.B`/`.T` and evidence-bundle hashes.

The E5B consumer must:

1. reopen the source, selected plan, and E5A snapshot in an independent process;
2. validate every fingerprint and cross-record identity;
3. reject any state not marked `CANONICAL_PRE_RESBM`;
4. verify fixed `N`, selected layout, protected sites, and Relin adjacency;
5. use the snapshot as immutable ReSBM input;
6. import baseline-DP placement only through its accepted provenance contract;
7. publish ReSBM candidates and selected actions under separate identities;
8. leave E5A input records unchanged.

E5A does not authorize E5B to relax `q_w=q`, uniform one-level consumption, SCC restrictions, protected boundaries, or deterministic tie rules.
## 14. Requirement Traceability

| Requirement | Master or ADR status | O2 plan milestone | Work items | Verification | Retained evidence |
| --- | --- | --- | --- | --- | --- |
| Complete O0 before O2 | Master successor pending; user decision frozen | `O2-O0Q-001` | `E5A-W01`, `E5A-W10` | `O2-E5A-001` stale check | Qualification reference and digest |
| Selected-layout canonical state | O2 plan v1.6 Sections 9, 12, and 13 | `S1.5A` | `E5A-W03` through `E5A-W07` | `O2-S15A-001`, `O2-E5A-001` | State and transfer traces, `.B`, `.T` |
| Fixed `N` | Master successor pending; O2 invariant frozen | `S1.5A` | `E5A-W05` | Fixed-N and capacity rows | Requested config and capacity proof |
| Immediate Relin | Current master and O2 invariant | `S1.5A` | `E5A-W08`, `E5A-W13` | `O2-S15A-001`, `O2-E5A-003` | Relin and WOPT audits |
| Protected boundaries | FRZ-03 and ADR acceptance pending | `S1.5A`, later `S1.7` | `E5A-W09` | Protected-site identity cases | Protected-site and provenance records |
| Atomic publication and reopen | P2/S1.1 accepted input | `S1.1`, `S1.5A` integration | `E5A-W10` through `E5A-W12` | `O2-E5A-001`, `O2-E5A-002` | Digests, failure logs, reopen report |
| No runtime work in E5A | O2 stage boundary | `S1.5A` | `E5A-W14` | `O2-E5A-005` | Symbol and include reports |
| No server secret or artifact secret | FRZ-09 lifecycle pending; repository invariant | `S1.5A` evidence hygiene | `E5A-W14` | Secret scan | `secret-scan.json` |
| Meaningful WHIRL evidence | `AGENTS.md` and O2 evidence contract | `S1.5A` | `E5A-W15` | `O2-E5A-004` | Same-stem `.B` and `.T` |
| Exact E5B input | O2 pass-order contract | `S1.6`, `S1.7` prerequisite | `E5A-W16` | E5B consumer dry-run | Handoff manifest and hashes |

Every accepted traceability row must be completed as:
```text
requirement
  -> accepted master section, version, and hash
  -> accepted ADR decision
  -> O2 plan milestone and test
  -> E5A work item
  -> exact verification command
  -> retained artifact and reviewer decision
```
A row containing `pending`, an unknown owner, a moving source reference, or a placeholder hash prevents `O2-E5A-EXIT=Accepted`.
