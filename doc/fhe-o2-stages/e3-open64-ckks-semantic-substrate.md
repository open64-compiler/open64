# O2-E3 Detailed Plan: Open64-Owned CKKS Semantic Substrate

Status: proposed detailed execution plan; implementation has not started

Plan version: 0.1

Date: 2026-09-15

Engineering stage: `O2-E3`

Formal milestones: `S1.2` and the substrate portion of `S1.5A`

Exit gate: `O2-E3-EXIT`

## 1. Authority and Metadata

### 1.1 Governing inputs

| Input | Reference | Entry requirement |
| --- | --- | --- |
| Repository rules | `AGENTS.md` | Mandatory |
| Current recorded master | `DSC_FHE_Compiler_Architecture_and_Integration_Plan_v0.10.md` | Governing until an accepted successor replaces it |
| Master commit | `ee1dc6382246c58f49a3097157a8c4e8ff2440c8` | Recorded authority |
| Master DOCX SHA-256 | `0018769C26B5A0BCD1BDFCBD85AA97B8BAFEA381D7640FBB9E2E81B0022013D9` | Recorded authority |
| Master Markdown SHA-256 | `7EF644E9BECEFB541102514B83656C78756ECA71398A771A2A5160FC10B0CB25` | Recorded authority |
| Governing O2 plan | `FHE-O2-ACE-RESBM-METAKERNEL-INTEGRATION-PLAN.md`, version 1.6 | Accepted or explicitly proposed where master reconciliation is pending; Sections 12.3-12.5 govern stage decomposition |
| O2 plan SHA-256 used by this draft | Accepted committed hash required at activation | Replace with accepted commit and hash before implementation |
| Detailed execution index | `FHE-O2-DETAILED-EXECUTION-PLAN.md`, plan set version 0.2 | Non-normative stage navigation |
| Pending ADR | `doc/adr/FHE-O0-O2-PLANNER-OWNERSHIP.md` | Must close before `PRE-O2-LOCK-EXIT` |
| Downstream runtime source | `../../ace-compiler@929e9b621f11bebbaa9ec1e215f4a52e3d07109b` | Fingerprint input only; no E3 link or execution |

The accepted master successor, ADR, O2 plan, and execution-index hashes replace the
working-tree references before the first implementation commit. A later change
to any bound input makes E3 `Unverified` until targeted review and retesting.

### 1.2 Roles

| Role | Responsibility |
| --- | --- |
| Stage owner | Schedule, gate state, final decision, and retained evidence |
| Semantic graph editor | Immutable graph, logical operation APIs, and graph verifier |
| CKKS state editor | State schema, transfer functions, action legality, and state verifier |
| Common/com editor | Sole editor and compatibility reviewer for persisted shared records |
| WOPT reviewer | Admission rules, effect handling, and non-interference evidence |
| Cryptography reviewer | Scale, level, components, range, error, and key-use legality |
| Security reviewer | Server-secret exclusion and artifact-secret scan |
| Independent test reviewer | Oracle independence, corruption, determinism, and reopen evidence |

### 1.3 Entry gates

E3 starts only after all of these are accepted and current:

1. `PRE-O2-LOCK-EXIT`.
2. `O2-O0Q-001=Qualified` with its complete qualification fingerprint.
3. `O2-E1-EXIT` with source, support, diagnostics, fixtures, and oracle locks.
4. `O2-E2-EXIT`, including `O2-P2-001` and `O2-S11-001`.
5. The independently reopenable E2 identity selected plan.
6. The O0 config/layout/state/placement/protected-site/key record crosswalk.
7. The ACE rtlib/ANT capability-manifest fingerprint as a downstream input.

No E3 source, fixture, oracle, or production work may merge while an entry item
is missing, rejected, unverified, or stale.

### 1.4 Stale triggers

E3 evidence becomes stale after a change to:

- master, ADR, O2 plan, execution index, or repository rules;
- O0 qualification ID or a bound source, pass, config, support, fixture,
  toolchain, record-schema, or environment hash;
- P2 record ownership, version, identity rules, or binary carrier;
- E2 transaction, publication protocol, or identity-plan format;
- fixed `N`, slot, scale, level, component, range, error, bootstrap, protected-
  site, or logical-key semantics;
- logical opcode, TensorDescriptorIR, call, edge, value, or lineage identity;
- ACE rtlib revision, public-header hashes, `LIB_ANT`, build, or capability
  manifest, even though E3 does not link the runtime;
- WOPT admission, effect, alias, simplifier, or phase-order behavior; or
- an oracle dependency that reaches a production graph or transfer helper.

The stage owner records the first stale cause, affected tests, and the minimum
valid requalification scope before work resumes.

## 2. Objective and Delivery Boundary

E3 establishes one Open64-owned, provider-neutral semantic substrate shared by
MetaKernel and ReSBM. It converts canonical Open64 identities into an immutable
virtual operation graph, assigns explicit Cipher or Plain semantics, defines
CKKS state and action transfer, records effects and ordering, and rejects an
illegal graph or state merge before lowering.

Reviewers must be able to inspect a deterministic graph and state trace that
explains classification, shape/layout/slot lineage, level and scale transfer,
ordered state actions, immediate Relin, and the first unsupported condition.

### 2.1 In scope

- All-PU immutable graph construction from independently reopened Open64 input.
- Stable PU, node, value, edge, call, layout, state, and key-domain references.
- Cipher, Plain, encoded-Plain, and non-FHE classification.
- Shape, layout, slots, gaps, masks, padding, and value-lineage preservation.
- Provider-neutral Add, MulCC, MulCP, Rotate, Encode, Encrypt, Relin, Rescale,
  ModSwitch, Bootstrap, KeySwitch, and Decode actions.
- Fixed `N` propagation, `S=N/2` capacity checks, state transfer, range/error,
  key-use obligations, effects, aliasing, barriers, calls, joins, and edge IDs.
- Immediate Relin and WOPT admission/non-interference verification.
- Deterministic trace, compatibility, corruption, and reopen evidence through
  the accepted E2/P2 carriers.

### 2.2 Non-goals

- No MetaKernel search, cost, selected layout, or transformed weights.
- No selected-layout canonical state publication; E5A owns that gate.
- No baseline-DP import, ReSBM regions, cuts, DP, or replacement placement.
- No final Q/P/CRT, security, bootstrap-profile, or provider-key resolution.
- No ACE rtlib call, ANT context, generated C, link, load, run, or decrypt.
- No ACE AIR, private ACE compiler header, provider object, or second IR universe.
- No new FHE-specific WOPT profitability pass or parallel simplifier.
- No lazy Relin, auto-selected larger `N`, or private physical WHIRL opcode.
- No new WHIRL table, section, or image without common/com review.
- No direct or indirect rtlib dependency in `be.so`.

### 2.3 Support scopes

Each fixture cites one E1 support row and one exact scope:

- `o0-handoff-required`: fixed-N config and accepted baseline records;
- `shared-o0-o2-comparison-required`: common kernel and state cases;
- `o2-core-only`: file-wide identities, edge actions, and semantic planning;
- `o2-extension-only`: a separately reviewed bounded extension; or
- `fallback-required`: whole qualified O0 fallback for an unsupported increment.

E3 never broadens O0 support. An out-of-scope input is rejected before complete
publication or follows an already accepted whole-baseline fallback rule.

## 3. Inherited Invariants

1. Binary WHIRL remains the process boundary.
2. Existing mapped-image and ELF mechanisms remain authoritative.
3. Python is absent from E3 production analysis.
4. Open64 owns CKKS-and-above semantics and planning.
5. ACE rtlib with `LIB_ANT` is the eventual provider, not an E3 executor.
6. Requested `N` exists before canonicalization and never changes.
7. Slot capacity is `S=N/2`.
8. Canonical ciphertext values have two components.
9. MulCC creates only a transient three-component state.
10. Exactly one Relin immediately follows every canonical MulCC.
11. No ordinary SSA consumer observes the transient result.
12. No secret key, decryptor, or secret-derived material enters E3 evidence.
13. Logical key obligations remain distinct from ANT-expanded material.
14. State joins use explicit compatibility or explicit alignment actions.
15. Ordered actions are not moved, merged, deleted, or duplicated by WOPT.
16. Provisional, pre-ReSBM, post-ReSBM, and final states never share identity.
17. E3 does not authorize a private operation at the final `whirl2c` boundary.

## 4. Two Dependency Graphs

### 4.1 Implementation and build dependency

```text
PRE-O2-LOCK -> O0Q Qualified -> E1 -> E2
  -> E3 contract and dependency audit
  -> immutable graph and classification
  -> effects, aliases, calls, edges, and ordering
  -> CKKS state and action transfer
  -> immediate Relin enforcement
  -> WOPT admission and non-interference
  -> deterministic evidence and independent verification
  -> O2-E3-EXIT
```

E3 depends on E2 APIs, not on E4 MetaKernel, E5B ReSBM, or E5C ANT
materialization. Downstream consumers wait for `O2-E3-EXIT`.

### 4.2 Compiler pass order

```text
reopen canonical Open64 FHE image and E2 identity plan
  -> validate fingerprints
  -> build immutable provider-neutral graph
  -> classify values and attach lineage/effects
  -> run provider-neutral CKKS transfer and verifier
  -> expose provisional requirements to E4
  -> E4 selects layout
  -> E5A recomputes canonical pre-ReSBM state
  -> E5B selects permitted placement
  -> E5C finalizes parameters and projects to ANT
```

Implementation order does not authorize pass reordering. E3 may define and
exercise the pre-ReSBM schema, but E5A publishes state for the E4-selected
layout after discarding all candidate facts.

### 4.3 Build and link impact

E3 may add Open64 objects through existing build extension points. It may not
add an ACE rtlib symbol to `be.so`, `lw_inline`, a backend plugin, or a WHIRL
tool. The build review records new objects, includes, defined/undefined symbol
deltas, consumer link closure, platform/static/shared impact, and absence of
`FHErt_*`, `rt_ant`, ACE AIR, or private ACE dependencies. Any direct rtlib
dependency stops E3 for explicit architecture and build approval.

## 5. Interfaces and Data Contracts

### 5.1 Provider-neutral boundary

```text
canonical WHIRL identities plus E2 identity plan
  -> immutable FHE semantic graph
  -> CKKS transfer and effect analysis
  -> semantic verifier
  -> normalized graph/state/effect traces
  -> E4 and E5 consumers
```

The graph is an Open64 analysis object keyed to persisted identities. It is not
ACE AIR or alternate program serialization. E3 uses only a carrier accepted by
P2. Otherwise, it reconstructs a transient graph deterministically after reopen.

### 5.2 Sources of truth

| Fact | Owner | E3 action |
| --- | --- | --- |
| Requested config and fixed `N` | `FHECompilationConfigIR` | Read and verify |
| Accepted parameter identity | O0/P2 parameter record | Reference; do not finalize Q/P/CRT |
| Tensor semantics | TensorDescriptorIR and canonical lineage | Preserve |
| Stable plan identities | E2 transaction | Bind every graph object |
| Graph and CKKS transfer semantics | E3 | Own one provider-neutral implementation |
| Final layout | E4 | Do not select |
| Selected-layout pre-ReSBM state | E5A | Prepare schema only |
| Placement | O0 baseline and E5B replacement | Preserve references only |
| Final parameters and ANT projection | E5C | Do not create |

### 5.3 Proposed API shape

Exact spelling is approved before coding. The target API uses stable IDs, not
raw WN pointers across the plan boundary:

```cpp
FHE_GRAPH_STATUS Build_fhe_semantic_graph(
    const FHE_CANONICAL_PROGRAM_VIEW& source,
    const FHE_IDENTITY_PLAN_VIEW& identity_plan,
    FHE_SEMANTIC_GRAPH* graph,
    FHE_DIAGNOSTIC_SINK* diagnostics);

CKKS_ANALYSIS_STATUS Analyze_ckks_semantics(
    const FHE_SEMANTIC_GRAPH& graph,
    const CKKS_PARAMETER_VIEW& parameters,
    CKKS_ANALYSIS_RESULT* result,
    FHE_DIAGNOSTIC_SINK* diagnostics);

VERIFY_STATUS Verify_fhe_semantic_graph(
    const FHE_SEMANTIC_GRAPH& graph,
    const CKKS_ANALYSIS_RESULT& analysis,
    FHE_DIAGNOSTIC_SINK* diagnostics);
```

The accepted API has explicit version/capability queries, established Open64
lifetime ownership, deterministic diagnostics, immutable success results, and
failure-atomic outputs. It exposes no ACE, ANT, Python, or frontend-builder type.

### 5.4 Graph identity and classification

Each graph object carries program/plan fingerprint, PU ID, source-node ID,
value/version ID, edge ID, call/formal/actual/return IDs where applicable,
TensorDescriptorIR and layout references, encryption/key domains, source
position, lineage, and deterministic ordinal. Allocation order, address, PU read
order, and thread scheduling do not affect identity.

The value lattice contains at least `NonFHE`, `PlainScalar`, `PlainTensor`,
`EncodedPlain`, `CipherInput`, `CipherDerived`, and
`UnsupportedOrAmbiguous`. Classification uses explicit source, descriptor,
encryption, and key-domain contracts. It never guesses from a runtime symbol.

Every virtual operation records logical opcode/version, ordered operands and
results, shape/layout/slot/lineage preconditions, domains, CKKS transfer kind,
effect/alias/order summary, logical key class, support row, and diagnostic.
Physical `OPR_DSL` remains private; dumps use logical names.

### 5.5 CKKS state

Each state contains or references:

- parameter ID, fixed requested `N`, value/version ID, and state ID;
- status: provisional, canonical-pre-resbm, post-resbm, or final;
- chain position and remaining named-prime IDs;
- symbolic/provider scale identities already present in accepted records;
- exact or log2 scale bits and rounding policy;
- component count, layout ID, slots, and plaintext compatibility;
- encryption and key domains;
- value range and accumulated absolute/relative error bounds;
- pending non-Relin action obligations; and
- complete derivation provenance.

E3 defines and tests this schema. It may exercise canonical-pre-resbm status on
locked identity/baseline fixtures, but E5A alone publishes that status for the
E4-selected layout.

### 5.6 Action transfer

| Action | Required result |
| --- | --- |
| `Encode` | Explicit plaintext scale and compatibility |
| `Encrypt` | Two-component ciphertext in the declared key domain |
| `AddAlign` | Compatible level, scale, layout, and error before Add |
| `MulCC` | Transient three-component result only |
| `MulCP` | Explicit plaintext scale and resulting range/error |
| `Relinearize` | Immediate transition from MulCC to two components |
| `Rescale` | Named-prime consumption and new scale/level identity |
| `ModSwitch` | Named-prime consumption with compatible scale semantics |
| `Bootstrap` | Opaque profile transition and ordered barrier |
| `KeySwitch` | Explicit source and target key domains |
| `Rotate` | Layout permutation and signed logical key requirement |
| `Decode` | Client/test boundary only; forbidden in server evaluation |

Every action records input/output state IDs, consumed prime IDs, preconditions,
range/error transfer, reason, lineage, effects, and later provider capability.
The state schema can represent non-uniform chains, but Stage 1 ReSBM accepts
only `q_w=q` and uniform one-level consumption.

### 5.7 Immediate Relin

```text
two-component lhs and rhs
  -> MulCC
  -> transient three-component state
  -> adjacent Relin
  -> ordinary two-component SSA value
```

No operation, phi, call, branch, memory action, or consumer may intervene.
Exactly one logical relin-key requirement belongs to the pair. E3 rejects a
missing, delayed, duplicate, already-two-component, phi-exposed, call-exposed,
or fused-plus-explicit Relin. E5C later verifies exact ANT
`Mul_ciph3 -> Relin`; E3 makes that projection unambiguous.

### 5.8 Effects, joins, calls, and loops

- Pure pre-conversion expressions enter WOPT only with complete identity and a
  pure effect summary.
- High-level FHE arithmetic stays in the dedicated semantic graph.
- Relin, Rescale, ModSwitch, Bootstrap, KeySwitch, context/key import, and
  lifetime actions are ordered and observable.
- Allocation and failure are observable; context and evaluation-key reads are
  effects even when immutable.
- Joins require identical parameter/key domains and compatible level, scale,
  components, layout, range, and error after explicit actions.
- Fanout preserves an immutable input or creates explicit versions.
- Critical-edge actions bind to stable edge IDs.
- Direct nonrecursive calls require resolved bodies, formal/actual state,
  effects, domains, layout, and frequency.
- Recursion, indirect calls, unresolved externals, and scale-changing recursion
  are unsupported.
- Retained loops have exact trip/frequency and zero level consumption;
  multiplication SCCs are marked for later bounded unroll or rejection.
- Unreachable frequency zero remains distinct from unknown frequency.

### 5.9 Serialization and compatibility

E3 introduces no carrier. Any new field, record, opcode, table, image, or
version returns to common/com review. Authorized persisted data uses fixed-width
fields, invalid-zero IDs, checked ranges, deterministic order, explicit version
gates, and no pointer or host-sized enum. A transient graph must reconstruct to
the same normalized trace after independent reopen. Old and feature-absent
inputs retain their accepted behavior; unsupported versions fail closed.

## 6. Diagnostics

The E1 manifest owns exact spelling. These target meanings must match it:

| Target diagnostic | First failing condition |
| --- | --- |
| `FHE-O2-E3-UPSTREAM-STALE` | Any bound upstream fingerprint mismatch |
| `FHE-O2-GRAPH-IDENTITY-INVALID` | Duplicate, dangling, or inconsistent ID |
| `FHE-O2-GRAPH-CLASSIFICATION-AMBIGUOUS` | Cipher/Plain class cannot be proved |
| `FHE-O2-GRAPH-UNSUPPORTED-OP` | Missing semantic operation contract |
| `FHE-O2-GRAPH-LINEAGE-INVALID` | Shape, slot, gap, layout, or lineage conflict |
| `FHE-O2-CKKS-N-MUTATION` | Analysis attempts to change requested `N` |
| `FHE-O2-CKKS-CAPACITY` | Fixed `N` cannot hold the accepted layout |
| `FHE-O2-CKKS-STATE-MERGE` | Join lacks compatible states or explicit action |
| `FHE-O2-CKKS-PARAMETER-MISMATCH` | State and authoritative parameter/Q/P/CRT fingerprints differ |
| `FHE-O2-CKKS-UNSUPPORTED-LEVEL-CONSUMPTION` | Stage 1 projection is non-uniform or multi-level |
| `FHE-O2-RELIN-NOT-IMMEDIATE` | MulCC lacks exactly one adjacent Relin |
| `FHE-O2-RELIN-DUPLICATE` | A second Relin is attached to the same MulCC |
| `FHE-O2-WOPT-ORDER-VIOLATION` | WOPT changes an ordered action |
| `FHE-O2-SERVER-SECRET-FORBIDDEN` | Server graph or artifact contains secret/decrypt capability |
| `FHE-O2-E3-PUBLICATION-INCOMPLETE` | Failure exposes an apparently complete result |

Diagnostics are deterministic in order, object ID, and source position.

## 7. Work Breakdown Structure

Each row names the change, dependency, verification, evidence, ownership, and
merge rule. Proposed component paths are finalized by the design-continuity
audit; none is claimed to exist yet.

| ID | Change and components | Dependency type | Tests and artifacts | Owner / reviewer / merge rule |
| --- | --- | --- | --- | --- |
| `E3-W01` | Freeze intake manifest, owners, hashes, versions, and stale triggers under proposed `testdata/fhe_o2/e3/` | Entry gate | Hash mutation tests; intake report | Stage / architecture; no placeholder or moving ref |
| `E3-W02` | Audit WHIRL, TensorDescriptorIR, mapping, WOPT, pool, verifier, and simplifier extension points | Implementation after W01 | Source crosswalk and dependency audit | Graph / common-com and WOPT; every parallel design justified or rejected |
| `E3-W03` | Define immutable graph views, versions, IDs, lifetimes, and printer | P2/E2 API | ID, lifetime, order, and API tests; type inventory | Graph / common-com; no pointer identity, ACE type, or new carrier |
| `E3-W04` | Build all-PU graph from reopened source and identity plan | Pass order after reopen | 1-8 PUs, reordered input, calls, fanout; graph `.B`/`.T` and JSON | Graph / independent; exact identity census |
| `E3-W05` | Implement Cipher/Plain classification | W04 and E1 support | Operation corpus and ambiguity negatives; classification table | Graph / FHE; one class or fail closed |
| `E3-W06` | Preserve shape, layout, slot classes, gaps, masks, padding, and lineage | W04 | Sentinel and lineage oracle; slot trace | Graph / tensor; no scalar-deadness inference over encrypted gaps |
| `E3-W07` | Define read/write/failure/allocation/context/key/lifetime effects and aliases | W03-W06 | Pure, may-alias, escape, and failure cases; effect trace | Graph / WOPT; every operation has one accepted summary |
| `E3-W08` | Model calls, phis, fanout, critical edges, loops, and frequency status | W04-W07 | Direct/recursive/indirect, SCC, unknown frequency; census | Graph / IPA and ReSBM; no deleted backedge or guessed frequency |
| `E3-W09` | Implement structural graph verifier and corruption matrix | W03-W08 | Field-by-field corruption; first diagnostic report | Graph / independent; fail before CKKS analysis |
| `E3-W10` | Define CKKS state/action views and status identities | W02, W09, P2 | Versions, chain 1-16, components 2/3; schema trace | State / common-com and crypto; no duplicate parameter truth |
| `E3-W11` | Propagate fixed `N`, derive `S`, and check capacity/overflow | W10 | `N=2^12..2^16`, exact/one-over capacity; report | State / crypto and security; zero N mutation |
| `E3-W12` | Implement all provider-neutral transfer functions | W05-W11 | Raw independent oracle; before/after field diff | State / independent crypto; oracle imports no production helper |
| `E3-W13` | Enforce immediate Relin and one logical relin-key obligation | W07, W12 | Positive, missing, delayed, duplicate, fused-plus-duplicate; trace | State / crypto and runtime; exactly one adjacent Relin |
| `E3-W14` | Propagate conservative range and absolute/relative error | W12 | Signs, zero, cancellation, boundaries, NaN/Inf; proof | State / numerical crypto; no wrap, guess, or non-conservative bound |
| `E3-W15` | Verify joins and stable edge-action identities | W08, W12-W14 | Scale/level/component/layout/domain mismatches; join proof | State / control-flow and crypto; no implicit coercion |
| `E3-W16` | Separate provisional, pre-ReSBM, post-ReSBM, and final identities | W10-W15 | Status alias, stale layout, premature final; transition report | State / E4/E5A; no selected-layout publication in E3 |
| `E3-W17` | Integrate WOPT admission through existing CODEREP and `wn_simp_code.h` services | W07, W09 | Identity/effect distinctions, simplifier on/off; WOPT dump | WOPT / graph and common-com; no parallel simplifier |
| `E3-W18` | Prove PRE/CSE/DCE/hoist/speculation non-interference | W17 | Ordered-action matrix; pre/post `.B`/`.T` and trace | WOPT / independent crypto; zero movement, loss, or duplication |
| `E3-W19` | Enforce provider neutrality and secret exclusion | All code | Header/symbol/drift/secret injections; audits | Stage / build and security; no rtlib link or secret artifact |
| `E3-W20` | Close reopen, compatibility, determinism, and failure atomicity | W01-W19 | Two clean runs, old/absent/corrupt records, every publication fault | Stage / compatibility and reliability; exact and all-or-nothing |
| `E3-W21` | Publish exit bundle and signed decision without algorithm changes | All rows | Manifest completeness and independent hash verification | Stage / all reviewers; acceptance-only commit |

## 8. Commit and PR Sequence

| Order | Group | Content | Gate |
| --- | --- | --- | --- |
| 1 | `E3-CONTRACT` | Intake, continuity audit, API, identities, diagnostics | W01-W03; governing `S1-3` and `S1-8A` |
| 2 | `E3-GRAPH` | Graph, classification, lineage, structural verifier | W04-W06, W09; governing `S1-3` |
| 3 | `E3-EFFECTS` | Effects, aliases, calls, edges, loops | W07-W08; governing `S1-3` |
| 4 | `E3-STATE-SCHEMA` | State/action schema, fixed-N, statuses | W10-W11, W16; governing `S1-8A` |
| 5 | `E3-TRANSFER` | Transfers, Relin, joins, range/error | W12-W15; governing `S1-8A` |
| 6 | `E3-WOPT` | Admission and non-interference | W17-W18; governing `S1-8A` |
| 7 | `E3-EVIDENCE` | Reopen, compatibility, dependency, and security | Candidate formal evidence |
| 8 | `E3-ACCEPT` | Exit bundle and signoff only | All formal rows accepted and current |

Each commit has one semantic goal. Contract tests land with or before their
implementation. An extension row gets a separate plan amendment and commit.

## 9. Verification Matrix

All commands below are proposed target commands. Their scripts and fixtures are
not claimed to exist until the matching implementation commit lands.

### 9.1 Formal milestone tests

| Test ID | Exact proposed command | Inputs and bounds | Expected result and pass rule | Environment, time, and evidence |
| --- | --- | --- | --- | --- |
| `O2-S12-001` | `python3 osprey/be/vho/tests/run_fhe_ace_graph_contract.py --build-dir build --manifest testdata/fhe_o2/ace-graph/SHA256SUMS --artifacts test-artifacts/o2/O2-S12-001` | Locked Add/Mul/Rotate/Mask/MVM/Conv/ReLU/call/phi/fanout/loop graphs; seed `0x41434531`; up to 8 PUs and 2000 ops | Graph, classes, identities, effects, and raw transfer trace exact; stable first diagnostic; every accepted op has one owner/transfer | Provider-free fixtures; 0 warmups, 1 run, 300 seconds; graph/state/effect dumps and matching `.B`/`.T` retained 180 days |
| `O2-S15A-001` | `python3 osprey/be/vho/tests/run_ckks_state_contract.py --mode pre-resbm --build-dir build --manifest testdata/fhe_o2/ckks/SHA256SUMS --artifacts test-artifacts/o2/O2-S15A-001` | Fixed `N=2^12..2^16`; chain 1-16; canonical components 2, transient 3; fanout 1-8; seed `0x434b4b53` | N unchanged; transfer, effect, bounds, statuses, Relin, and WOPT behavior exact; zero ambiguity or illegal movement | Provider-free; 0 warmups, 1 run, 20 minutes; config, traces, diagnostics, WOPT dumps, `.B`/`.T` retained 180 days |

### 9.2 Supplemental tests

| Test ID | Exact proposed command | Required result |
| --- | --- | --- |
| `O2-E3-GRAPH-001` | `python3 osprey/be/vho/tests/run_fhe_ace_graph_contract.py --build-dir build --manifest testdata/fhe_o2/ace-graph/SHA256SUMS --suite identity-classification-lineage --artifacts test-artifacts/o2/O2-E3-GRAPH-001` | Two PU read orders produce exact graph/classification/lineage equality; seed `0x45334731`; 300 seconds |
| `O2-E3-STATE-001` | `python3 osprey/be/vho/tests/run_ckks_state_contract.py --mode pre-resbm --build-dir build --manifest testdata/fhe_o2/ckks/SHA256SUMS --suite transfer-range-error --artifacts test-artifacts/o2/O2-E3-STATE-001` | Production and raw oracle match every field; bounds conservative; seed `0x45335331`; 1200 seconds |
| `O2-E3-RELIN-001` | `python3 osprey/be/vho/tests/run_ckks_state_contract.py --mode pre-resbm --build-dir build --manifest testdata/fhe_o2/ckks/SHA256SUMS --suite immediate-relin-negatives --artifacts test-artifacts/o2/O2-E3-RELIN-001` | Exactly one adjacent Relin in positives; exact first diagnostic for missing, delayed, duplicate, fused-plus-duplicate, phi, and call cases; 300 seconds |
| `O2-E3-WOPT-001` | `python3 osprey/be/vho/tests/run_ckks_state_contract.py --mode pre-resbm --build-dir build --manifest testdata/fhe_o2/ckks/SHA256SUMS --suite wopt-noninterference --artifacts test-artifacts/o2/O2-E3-WOPT-001` | PRE/CSE/DCE/hoist/speculation preserve ordered action IDs, edges, count, and order; seed `0x45335731`; 1200 seconds |
| `O2-E3-COMPAT-001` | `python3 osprey/be/vho/tests/run_fhe_e3_compatibility.py --build-dir build --manifest testdata/fhe_o2/e3/SHA256SUMS --artifacts test-artifacts/o2/O2-E3-COMPAT-001` | Old and feature-absent behavior preserved; reopen exact; all ID/range/hash corruptions fail; seed `0x45334331`; two clean runs, 600 seconds |
| `O2-E3-SEC-001` | `python3 osprey/be/vho/tests/run_fhe_e3_dependency_security_audit.py --build-dir build --manifest testdata/fhe_o2/e3/SHA256SUMS --artifacts test-artifacts/o2/O2-E3-SEC-001` | No private ACE include, rtlib symbol, `be.so` dependency, server secret, decryptor, or artifact secret; provider drift stales E3; 600 seconds |

### 9.3 WHIRL evidence

Every meaningful `.B` receives a same-stem `.T`:

```text
ir_b2a -st -src test-artifacts/o2/O2-S12-001/e3-graph.B test-artifacts/o2/O2-S12-001/e3-graph.T
ir_b2a -st -src test-artifacts/o2/O2-S15A-001/e3-state.B test-artifacts/o2/O2-S15A-001/e3-state.T
```

Preserve driver traces under non-colliding names such as `e3-state.vho.t` before
creating `.T`. Missing `-src` support leaves the row `Unverified`.

## 10. Negative, Failure, and Compatibility Coverage

### 10.1 Required negatives

- malformed shape, ambiguous lineage, duplicate/dangling ID, overlapping write,
  invalid slot class, unknown operation, or inconsistent call binding;
- indirect/recursive/unresolved call, retained depth-consuming SCC, unknown
  trip, or unknown frequency treated as zero;
- missing fixed N, N mutation, overflow, one-over capacity, parameter/Q/P/CRT/
  scale/key-domain fingerprint mismatch, or illegal join;
- non-uniform or multi-level action marked Stage 1 ReSBM-compatible;
- missing, delayed, duplicate, fused-plus-duplicate, join-exposed, or call-
  exposed Relin result;
- non-conservative range/error, NaN/Inf, or unchecked saturation;
- stale provisional fact relabeled pre-ReSBM, post-ReSBM, or final;
- PRE/CSE/DCE/hoist/speculation across different identities or ordered actions;
- ACE rtlib/header/build drift, `LIB_ACE` substitution, ACE AIR/private include,
  or new `FHErt_*`/`rt_ant` symbol in `be.so`;
- secret/decryptor in server mode, Decode in the server graph, or secret content
  in `.B`, `.T`, JSON, trace, diagnostic, or log.

The Q/P/CRT negative compares E3 state with the authoritative Open64 record.
Runtime-created ANT Q/P/CRT comparison remains E5C work.

### 10.2 Failure atomicity

Inject failure before and after graph, state, trace, manifest, hash, and decision
publication. A failure may retain diagnostics in a clearly named failure
directory, but never a valid-looking E3 `.B`, plan, or completed manifest. Use
the accepted E2 all-PU atomic transaction pattern.

### 10.3 Compatibility and determinism

- accepted legacy WHIRL behavior is unchanged;
- feature-absent E2 plans preserve O0 behavior;
- authorized records reopen after producer exit;
- unsupported versions and corrupt headers/counts/ranges/IDs/hashes fail closed;
- two clean runs produce identical normalized graph, state, effects,
  diagnostics, logical key obligations, and authorized E3 binary evidence;
- randomized ciphertext and key bytes are not determinism evidence.

## 11. Evidence and Retention

The accepted bundle contains at least:

```text
stage-input-lock.json
qualification-reference.json
source-lock.json
support-matrix.json
diagnostics.json
e2-record-and-transaction-lock.json
requested-configuration.json
provider-capability-reference.json
input.fhe.B
input.fhe.T
identity-plan.B
identity-plan.T
e3-graph.B
e3-graph.T
e3-graph.vho.t
semantic-graph.json
value-classification.json
lineage-and-slot-classes.json
effect-alias-order.json
call-edge-loop-census.json
ckks-state-schema.json
ckks-state-trace.json
ckks-action-trace.json
range-error-report.json
logical-key-obligations.json
wopt-before.B
wopt-before.T
wopt-after.B
wopt-after.T
wopt.e3-noninterference.t
wopt-action-order-diff.json
compatibility-report.json
corruption-tests.json
dependency-and-symbol-audit.json
secret-scan.json
failure-injection.json
two-run-determinism.json
reproduction-report.json
review-signoff.json
e3-exit-decision.md
SHA256SUMS
```

If P2 does not serialize the transient graph, `e3-graph.B/.T` show source plus
accepted semantic record references, while JSON and the phase trace show the
reconstructed graph. The decision must not claim a separate graph-image reopen.

Failed development evidence is retained at least 30 days, milestone evidence
180 days, and accepted gate evidence for repository lifetime. Clean the artifact
directory at the next run start, not at current run end. Docker uses a host bind
mount. The reproduction report lists absolute retained paths.

## 12. Exit, Rollback, and Stop Rules

### 12.1 Accepted

`O2-E3-EXIT=Accepted` requires:

1. All upstream fingerprints remain current.
2. `O2-S12-001` and `O2-S15A-001` pass with no required skip.
3. Every accepted operation has one identity, class, effect, and transfer.
4. Every materializable value can receive one unambiguous state identity.
5. Fixed `N`, capacity, range/error, joins, and state statuses verify.
6. Every MulCC has exactly one adjacent Relin and no normal value has three
   components.
7. WOPT preserves all ordered actions and reuses existing simplifier services.
8. No provider call, private ACE header, rtlib dependency, secret, or decryptor
   enters E3.
9. Reopen, compatibility, corruption, failure, and two-run checks pass.
10. Meaningful `.B` files have same-stem `.T` from `ir_b2a -st -src`.
11. Common/com, WOPT, crypto, security, compatibility, and independent test
    reviewers approve their rows.

Exact completion wording:

`O2-E3 accepted: the provider-neutral Open64 FHE graph and CKKS pre-ReSBM semantic substrate satisfy S1.2 and the substrate portion of S1.5A.`

This does not claim E5A selected-layout state, ReSBM, ANT execution, Stage 1
acceptance, or O2 completion.

### 12.2 Rejected or Unverified

`Rejected` records the first failed contract, owner, diagnostic, reproducer,
downstream impact, and rollback point. `Unverified` applies when source,
environment, reader, `ir_b2a -src`, reviewer, capability fingerprint, or
upstream evidence is unavailable. Neither state unlocks E4.

### 12.3 Rollback and invalidation

- The complete qualified O0 profile remains the only executable fallback.
- E3 failure publishes no mixed O0/O2 graph or state plan.
- Roll back in reverse order: accept, evidence, WOPT, transfer, schema, effects,
  graph, contract.
- Preserve P2 compatibility; disable E3 through accepted capability gates.
- Return any required binary representation change to common/com review.
- E4 graph/layout selection invalidates provisional state and triggers E5A.
- A stale O0 qualification suspends E3 and every downstream stage.

### 12.4 Architecture stop rules

Stop and return to the owning review if:

- master/ADR reconciliation or O0 qualification is not current;
- any graph, state, action, layout, placement, or parameter fact has two owners;
- a new WHIRL opcode/table/image or an unreopenable object is required;
- ACE AIR or provider memory is proposed as Open64 semantic truth;
- an unapproved rtlib dependency enters `be.so`;
- requested `N` changes or immediate Relin cannot be proved;
- a server secret/decryptor is required;
- WOPT must move an effectful FHE action;
- an extension lacks support row, diagnostic, independent oracle, or fallback;
- the oracle imports production logic, a calculation guesses/saturates, an
  observed tolerance must be relaxed, or publication is not atomic.

## 13. Traceability

| Requirement | Authority | O2 section | Work | Test | Evidence |
| --- | --- | --- | --- | --- | --- |
| O0 complete before O2 | User decision; master/ADR exact section pending lock | 0.4, 12.3, 13, 18 | W01 | Both formal tests intake | qualification reference |
| Open64 owns CKKS semantics | Master plus pending runtime ADR | 5.1, 7, 9 | W02, W03, W10 | S12, S15A | graph and state schema |
| Fixed requested N | Master CKKS contract | 4.2, 6, 9.1, 15.2 | W11 | S15A, STATE | config and capacity report |
| Immediate Relin | Master v0.10 rule | 7.1, 9.2-9.3, 15.2 | W13 | RELIN | action trace |
| WOPT non-interference | `AGENTS.md` continuity rules | 11.1 | W07, W17, W18 | WOPT | pre/post `.B`/`.T` and diff |
| One state per value | Master record/state contract | 8-9 | W10, W12, W15, W16 | S15A | state trace |
| No duplicate truth | Pending ADR and common/com review | 8.1, 9.1 | W02, W10 | COMPAT | crosswalk and reopen report |
| ANT is downstream only | `FRZ-08/09` | 7, 18 | W19 | SEC | dependency/symbol audit |
| No server secret | `FRZ-09` | 7.3, 15.2, 18 | W19 | SEC | secret scan |
| Same-stem WHIRL evidence | `AGENTS.md` artifact rules | 3.1, 8.2, 15.2 | W04, W18, W20 | COMPAT | `.B`/`.T` families |

Replace every pending master/ADR reference with exact accepted section,
version, commit, and hash before E3 implementation begins.
