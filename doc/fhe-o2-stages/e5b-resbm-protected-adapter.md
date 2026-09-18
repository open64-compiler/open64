# O2-E5B ReSBM Core and Protected Adapter Detailed Execution Plan

Status: Proposed detailed plan; implementation is locked until all entry gates are accepted

Plan version: 0.1

Date: 2026-09-15

Engineering stage: `O2-E5B`

Governing milestones: `S1.6`, `S1.7`

Exit gate: `O2-E5B-EXIT`

## 1. Authority and Metadata

### 1.1 Governing authority

This plan is subordinate to these authorities, in order:

1. Explicit accepted user decisions and repository invariants in `AGENTS.md`.
2. The accepted master architecture and an accepted successor or amendment.
3. Accepted architecture decision records within their stated scope.
4. `doc/FHE-O2-ACE-RESBM-METAKERNEL-INTEGRATION-PLAN.md`, version 1.6.
5. `doc/FHE-O2-DETAILED-EXECUTION-PLAN.md`, plan set version 0.2, for navigation.
6. This detailed stage plan.
7. Locked papers and source artifacts within their declared claim boundaries.

Recorded master commit: `ee1dc6382246c58f49a3097157a8c4e8ff2440c8`.

Recorded master Markdown SHA-256:
`7EF644E9BECEFB541102514B83656C78756ECA71398A771A2A5160FC10B0CB25`.

Recorded master DOCX SHA-256:
`0018769C26B5A0BCD1BDFCBD85AA97B8BAFEA381D7640FBB9E2E81B0022013D9`.

Pending ADR: `doc/adr/FHE-O0-O2-PLANNER-OWNERSHIP.md`.

Accepted replacement master and ADR versions, commits, and hashes must replace
pending references before implementation starts.

### 1.2 ReSBM source authority

The provider-free production core is locked to:

- paper `../../ace-paper/RESBM_paper.pdf` with SHA-256
  `C6D46834C37CF534D1D13050E7CB946962E3D7FD810539C347BE21EBEA6C3A00`;
- fixed code `origin/20250524@323e8bb02a0e036fe2369eb48badfe6692398c2b`;
- source anchors `resbm.cxx`, `dfg_region_builder.cxx`,
  `min_cut_region.cxx`, and `resbm_scale_mgr.cxx` under
  `fhe-cmplr/ckks/src/` at that revision.

Production claims are limited to the conservative artifact-backed subset.
Paper behavior is supporting and differential evidence. E5B does not claim
general paper Algorithm 3 coverage, general loop coverage, or global optimality.

### 1.3 Ownership and review

The proposed owner is the Open64 ReSBM planner owner. Required reviewers are:

- the E5A CKKS pre-state owner;
- the common/com record and compatibility owner;
- an independent artifact-core algorithm reviewer;
- an independent raw-graph oracle reviewer;
- the O0 protected-site and baseline-DP owner;
- an independent protected-boundary reviewer;
- a CKKS state and immediate-Relin reviewer;
- a build and `be.so` dependency reviewer;
- an evidence, failure-atomicity, and security reviewer;
- the E5C consumer owner.

The core and protected adapter have separate code owners and primary tests.
One common/com editor owns any physical-record change.

### 1.4 Entry gate and consumed evidence

Implementation begins only after:

- `PRE-O2-LOCK-EXIT` is accepted;
- `O2-O0Q-001=Qualified` is current;
- `O2-E1-EXIT` through `O2-E5A-EXIT` are accepted and current;
- the E5A snapshot independently reopens as `CANONICAL_PRE_RESBM`;
- fixed `N`, selected layout, key domain, parameter intent, and Relin proof match;
- protected/manual/pre-ReLU sites and baseline-DP provenance reopen;
- ReSBM source, support, latency, diagnostic, tie, and oracle locks match;
- record ownership and physical versioning decisions are accepted;
- no unresolved master or ADR conflict affects protected placement policy.

The manifest binds `O2-O0Q-001`, `O2-S10-001`, `O2-P2-001`, `O2-S11-001`,
`O2-S12-001`, applicable `O2-S13-001` and `O2-S14-001`, `O2-S15A-001`,
`O2-E5A-EXIT`, and qualified O0 baseline-DP/protected-site evidence.

Planning may be reviewed early. Production source, fixtures, oracles, and
acceptance artifacts cannot be created before the gates close.

### 1.5 Stale triggers

E5B stops and discards unpublished results when any of these changes:

- master, ADR, bootstrap policy, ownership, or qualification fingerprint;
- ReSBM paper, source revision, source hashes, or reviewed source delta;
- support, diagnostics, fixtures, generator, oracle, cost unit, or tie order;
- E2 schema/version/transaction or E3 graph/state/effect semantics;
- E4 selected layout, slot map, iteration space, or transformed data;
- E5A graph, pre-state, fixed-N, key-domain, or snapshot digest;
- protected-site class, baseline-DP provenance, or fallback identity;
- raw latency table, frequency source, merge rule, or model ID;
- SCC/unroll bound or uniform-level restriction;
- downstream ACE rtlib/ANT capability fingerprint bound for staleness.

The report names the first mismatch, affected plan IDs, invalidated artifacts,
responsible owner, and targeted requalification path.

## 2. Objective and Delivery Boundary

E5B consumes one immutable selected-layout E5A snapshot and produces one
verified disposition for E5C:

- a selected protected ReSBM schedule and `SELECTED_POST_RESBM` state;
- a verified manual/off bypass schedule and state; or
- an atomic whole-qualified-baseline fallback with an explicit reason.

The stage has two separate layers:

1. A provider-free artifact core operates only on normalized raw graph, state
   projection, costs, frequencies, and legal candidates.
2. A protected Open64 adapter imports O0 site provenance, constrains or splits
   core problems, preserves hard sites, and publishes the combined result.

E5C can independently reopen input, baseline, sites, regions, cuts, actions,
pre/post state, cost, fallback, and normalized result digest.

This is the final ReSBM schedule/state, not final CKKS parameters and not
provider execution or final runtime materialization.

## 3. Scope and Invariants

### 3.1 In scope

E5B includes:

- E5A input validation and raw graph normalization;
- provider-free region construction, SCC policy, ScaleMgr, cuts, and endpoint DP;
- checked frequency-weighted integer-nanosecond costs and deterministic ties;
- full-state cross-checking for rescale, mod-switch, and bootstrap actions;
- independent raw-graph exhaustive and fixed-seed oracle;
- protected/manual/pre-ReLU site import and identity validation;
- replacement of only baseline-DP-owned automatic sites;
- barrier partitioning and endpoint/cut filtering;
- manual/off bypass and failure-atomic whole-baseline fallback;
- all-PU result publication, independent reopen, and E5C handoff;
- matching `.B` and same-stem `.T` evidence.

### 3.2 Explicit non-goals

E5B does not:

- modify E5A, layout, slot map, fixed `N`, or requested configuration;
- rerun Fhelipe layout or baseline DP;
- recreate or reclassify O0 protected sites;
- remove, move, merge, or weaken manual/protected/pre-ReLU sites;
- support `q_w!=q`, non-uniform, or multi-level-consuming ReSBM;
- retain multiplication or scale-changing multi-node SCCs;
- implement bounded call/control-flow materialization extensions;
- finalize Q/P primes, CRT, security, bootstrap setup, or provider keys;
- include, link, load, query, or execute ACE rtlib or ANT;
- produce generated C or standard runtime calls;
- run ANT, decrypt ciphertexts, or materialize runtime operations;
- add a direct or indirect rtlib dependency to `be.so`;
- introduce ACE AIR or private provider IR;
- implement FHEFusion, HPOLY, HPAO, O3 scheduling, or memory placement;
- claim Stage 1 acceptance or O2 completion.

### 3.3 Inherited invariants

- Binary WHIRL remains the process boundary.
- Open64 owns CKKS-and-above semantics.
- Requested `N` never changes; capacity is `S=N/2`.
- E5A pre-state and selected layout are immutable.
- Ordinary ciphertext SSA values have two components.
- Every `MulCC` has a transient component-three result and immediate Relin.
- Relin is not a ReSBM candidate and is never delayed or duplicated.
- Protected identities, order, state, and provenance remain exact.
- Only explicitly replaceable DP-owned automatic sites may be invalidated.
- Unknown frequency differs from proven unreachable zero.
- Checked cost saturation disqualifies selection.
- No server secret, decryptor, or secret-derived material is persisted.
- Failed planning cannot publish an apparently valid result.
- Meaningful `.B` evidence always has a same-stem `.T` from `ir_b2a -st -src`.

### 3.4 Support boundary

| Dimension | Accepted behavior | Rejected/deferred behavior |
| --- | --- | --- |
| Graph | Source-locked intragraph static CKKS DFG | Indirect calls, recursion, unresolved externals, irreducible encrypted flow |
| Regions | `R0` depth 0; each later region depth 1 | Weighted or multi-level region depth |
| Chain | Complete E5A state projected to uniform one-level model | Non-uniform or multi-level transitions |
| Scale | Integer scale degree with `q_w=q` | Independent `q_w` or silent coercion |
| SCC | DAG or known zero-level retained SCC | Retained multiplication or scale-changing SCC |
| Loop | Known zero-level loop or bounded legal unroll | Unknown trip or bound overflow |
| Frequency | Known positive or proven unreachable zero | Unknown guessed as zero; saturated selection |
| Sites | Protected fixed; automatic DP site replaceable only when marked | Missing, stale, ambiguous, or reclassified site |
| Modes | Auto/on constrained planning; manual/off verified bypass | Compiler-created site in manual/off |
| Fallback | Complete current qualified O0 baseline | Mixed ReSBM and stale baseline actions |

Scope labels remain `o0-handoff-required`,
`shared-o0-o2-comparison-required`, `o2-core-only`, `o2-extension-only`, and
`fallback-required` with their governing O2 plan Section 4.2 meanings.

## 4. Implementation Dependencies and Pass Order

### 4.1 Implementation and build dependency

The implementation dependency is:

    current O2-O0Q-001
      -> E1 locks -> E2 records/transaction -> E3 graph/state/effects
      -> E4 selected layout -> E5A immutable pre-state
      -> E5B artifact core -> E5B protected adapter/publication
      -> E5C final resolution/materialization

The core builds and tests without the adapter. The adapter depends only on the
core's immutable public problem/result interface. The core cannot include O0
site, baseline-DP, Open64 driver, ACE rtlib, or ANT headers.

Proposed paths, subject to owner review, are:

- `osprey/be/vho/fhe_resbm_core.{h,cxx}`;
- `osprey/be/vho/fhe_resbm_region.{h,cxx}`;
- `osprey/be/vho/fhe_resbm_scale_mgr.{h,cxx}`;
- `osprey/be/vho/fhe_resbm_cut.{h,cxx}`;
- `osprey/be/vho/fhe_resbm_protected_adapter.{h,cxx}`;
- `osprey/be/vho/fhe_resbm_verify.{h,cxx}`;
- `osprey/be/vho/tests/run_resbm_region_contract.py`;
- `osprey/be/vho/tests/run_resbm_semantic_oracle.py`;
- `osprey/be/vho/tests/run_e5b_resbm_gate.py`;
- `testdata/fhe_o2/resbm-{region,oracle,protected}/`.

These are proposed locations, not existence claims.

### 4.2 Build and link impact

- E5B is provider-free Open64 C++ and tests.
- It includes no ACE rtlib, ANT, AIR, or private compiler header.
- It adds no rtlib-defined or undefined symbol to `be.so`.
- It introduces no frontend-only `DSL_Builder_*` dependency.
- It reuses accepted Open64 graph, table, pool, and mapped-image services.
- Physical record changes require isolated E2/P2/common-com review.
- Any new library dependency requires separate design and link-closure review.

The build gate compares `be.so` and known consumers before and after E5B. An
unauthorized dependency stops the stage even if functional tests pass.

### 4.3 Compiler pass order

The compiler order, which must not be confused with build order, is:

    canonical graph -> selected layout -> E5A canonical pre-state
      -> import baseline placement/protected provenance
      -> E5B optional protected ReSBM replacement
      -> E5B selected post-state verification
      -> E5C final parameters/security/keys/ANT projection/materialization

E5B never consumes provisional candidate state. E5C never infers placement
from reports or reruns E5B.

### 4.4 Required environment

Provider-free verification needs a supported Open64 build, binary reader/writer,
`ir_b2a -st -src`, original DST source, locked ReSBM source manifest, accepted
E1 raw tables/fixtures, reopenable E5A bundle, and retained host artifact path.
It needs no ANT installation, runtime context, key, ciphertext execution, or
network access.

## 5. Interface, Record, and Version Contracts

### 5.1 Sources of truth

| Input | Owner | E5B use | Mutation rule |
| --- | --- | --- | --- |
| `FHECompilationConfigIR` | O0/common-com | Fixed N and mode | Read-only; N never changes |
| E5A snapshot | E5A owner | Raw graph, state, identities | Read-only |
| Selected layout | E4 owner | Layout/slot identity | Read-only |
| Protected sites | O0 owner | Hard/manual/pre-ReLU constraints | Read-only; no reclassification |
| Baseline-DP plan | O0 owner | Replaceable sites and fallback | Read-only |
| Raw latency table | E1 cost owner | Integer-ns costs | Read-only and hash-bound |
| Support/diagnostics | E1 owner | Bounds and first error | Read-only and version-bound |

### 5.2 Core raw problem

`RawResbmProblem` contains schema/algorithm version, graph/PU/node/value/edge
IDs, deterministic order, operation class, one-level effect, projected integer
scale degree/level, full E5A state references, boundaries, frequency/provenance,
candidate IDs, raw cost-table ID, SCC/unroll facts, `q_w=q`, and fingerprints.

It contains no protected-site meaning, baseline record, provider object,
transaction handle, mutable WHIRL node, pointer, or producer-memory identity.

### 5.3 Regions, cuts, and actions

Each region records ID/order, member range, critical multiply, entry/exit and
bypass edges, depth, scale/level bounds, SCC disposition, frequency, and source
fingerprint. `R0` contains inputs and has depth exactly zero. Every `Ri`,
`i>0`, has depth exactly one.

Each cut/endpoint record contains source/destination regions, admissible window,
candidate ranges, rescale/mod-switch/bootstrap cut classes, required state
transition, `l_bts` excluding `src`, selected edges, pre/post state IDs, exact
cost components, and rejection reason. Candidate data is immutable.

Each action records typed kind, stable edge/site, regions, input/output state,
scale/level transition, full-state cross-check, semantic bootstrap profile,
lineage, reason, cost, frequency, and adapter disposition. Immediate Relin is
referenced as a fixed inherited action and is never enumerated.

### 5.4 Cost, frequency, and tie records

- Costs are nonnegative integer nanoseconds with checked arithmetic.
- Each component names its raw-table row and independent operation count.
- Known frequency multiplies cost with overflow checking.
- Unreachable zero needs `unreachable-proof` provenance.
- Unknown frequency cannot become zero or a guess.
- Saturation at `UINT64_MAX` emits `FHE-O2-FREQ-SATURATED` and disqualifies the
  candidate from selection or optimality claims.
- Total cost, bootstrap count, rescale count, and action-edge order are retained.

### 5.5 Protected adapter input and result

The adapter separately consumes site ID/source, class, state contract, relative
order, barrier, baseline-DP provenance, replaceability, mode, and qualified
fallback identity. It converts the program into immutable core subproblems by
splitting at hard barriers and filtering endpoints/cuts. The core has no hidden
protected-site special case.

The result envelope records version, disposition, E5A/qualification IDs, core
digests, protected/baseline fingerprints, ordered regions/cuts/candidates/
actions, preserved and replaced sites, pre/post state, cost/tie/frequency,
fallback reason, support/source/tool fingerprints, and producer/consumer digest.

Allowed dispositions are `selected`, `manual-bypass`, `off-bypass`,
`baseline-fallback`, and `error`. Published post-state is
`SELECTED_POST_RESBM`; `FINAL_RESOLVED` belongs to E5C.

### 5.6 Proposed semantic APIs

These are proposed semantic interfaces, not accepted declarations:

    BuildRawResbmProblem(E5aSnapshotView, RawLatencyTableView,
                         RawResbmProblemBuilder, DiagnosticSink)

    PlanArtifactResbmCore(RawResbmProblemView,
                          ArtifactResbmResultBuilder, DiagnosticSink)

    PlanProtectedResbm(E5aSnapshotView, ProtectedPlacementView,
                       BaselinePlacementView, RawLatencyTableView,
                       ResbmResultBuilder, DiagnosticSink)

    VerifySelectedPostResbmState(E5aSnapshotView, ResbmResultView,
                                  DiagnosticSink)

Inputs are immutable, failure returns no valid ID, first diagnostics are stable,
and builders cannot publish before all-PU verification.

### 5.7 Serialization, reopen, and compatibility

E5B uses E2/P2-approved storage and does not allocate a `WT_*`, ELF section,
table, or operator by itself. Physical records use fixed-width fields,
invalid-zero IDs, checked ranges, explicit versions/capabilities, deterministic
order, no pointers, no host enums, no nested STL, and complete fingerprints.

Required behavior:

- old WHIRL retains accepted reader behavior;
- feature-absent input fabricates no E5B result;
- unknown required versions/capabilities fail closed;
- optional fields follow accepted P2 rules;
- O0 runtime does not depend on E5B-only records;
- binary write, producer exit, independent reopen, and `ir_b2a` pass;
- header/count/range/ID/version/hash corruption fails before use.

## 6. Core and Protected Algorithm Invariants

### 6.1 Raw graph, SCCs, and regions

- Stable IDs, never pointer order, define enumeration.
- Backedges are retained; none is deleted to force a DAG.
- `q_w=q`, uniform one-level effects, and known frequency are prechecks.
- Known-trip zero-level SCCs may remain.
- Multiplication SCCs may unroll only with trip at most 64 and at most 10000
  expanded encrypted nodes; otherwise they fail.
- Unknown-trip, scale-changing, level-changing, recursive, or mutually recursive
  SCCs are outside the core.
- Region membership is total/non-overlapping; `R0=0`; all later regions depth 1.
- Critical multiplication, entry/exit, bypass, and disconnected-component order
  follow the fixed artifact contract.

### 6.2 ScaleMgr, cuts, endpoints, and state

ScaleMgr uses integer `(scale_degree, logical_level)` only as a projection from
complete E5A state. It cannot erase prime intent, scale identity, components,
range/error, key domain, layout, slots, or profile requirements.

For endpoint segment `[src,dst]`, ScaleMgr identifies rescaling regions,
`l_bts` counts one-level rescaling regions excluding `src`, artifact SMOPLC and
BTSPLC construct supported cuts, and endpoint DP selects minimum modeled cost.
Illegal joins, profiles, or full-state transitions remove a candidate before
selection. Non-uniform and multi-level actions fail before planning.

### 6.3 Selection and claims

Production follows the fixed artifact. Where it is silent, tie order is:

1. total cost;
2. bootstrap count;
3. rescale count;
4. lexicographic ordered action-edge IDs.

The only allowed claim is region-optimal under the named artifact contract and
cost model. No global or general-paper optimality is claimed.

### 6.4 Independent core oracle

The raw-graph oracle imports none of the production region, SCC, transfer,
ScaleMgr, cut, endpoint, cost, tie, or adapter helpers. For graphs at most 8
nodes, 12 edges, 6 levels, and 10 candidate sites, it enumerates all legal
artifact-contract regions and action placements. It compares legality, regions,
cuts, actions, state, levels, frequency, cost, winner, tie, and first diagnostic.

Random coverage uses 2000 graphs and seed `0x5253424d`; generator version and
bounds are manifest-locked.

### 6.5 Protected adapter

Every site has exactly one class: user manual, policy-mandated protected,
required pre-ReLU protected, or DP-owned replaceable automatic. Missing,
duplicate, conflicting, unknown, or stale classification rejects the adapter.

For each protected site the adapter preserves identity, source, relative order,
boundary, input/output state, approximation/ReLU provenance, fixed-N/layout
binding, policy owner, and semantic bootstrap profile.

In auto/on mode it validates the whole baseline, freezes hard actions, removes
only replaceable automatic sites from the candidate domain, splits at barriers,
filters endpoints/cuts, calls the core for immutable subproblems, recomposes in
program order, verifies whole-program post-state, and publishes atomically.

Manual mode creates no bootstrap and preserves manual sites. Off mode permits
no bootstrap. Both bypass automatic ReSBM but still verify feasibility; failure
cannot be hidden by adding a site.

Fallback, when policy allows, selects the entire current qualified baseline.
No ReSBM action survives. Stale or infeasible baseline means error/Unverified,
not a mixed result.

### 6.6 Independent protected oracle

The constraint oracle receives raw graph/cost/site/baseline/mode tables and
imports neither production core nor adapter. With graphs at most 8 nodes and 12
edges and seed `0x52534250`, it covers barriers, joins, shortcuts, replaceable
sites, manual/off, infeasible cuts, corrupt IDs, and fallback. It compares site
identity/order/state, windows, replacement set, regions, cuts, actions,
post-state, cost, tie, fallback, diagnostic, and independent reopen.

## 7. Diagnostics and Failure Policy

E1 owns exact spellings. Proposed diagnostic purposes are:

| Purpose | Proposed code |
| --- | --- |
| Stale qualification or E5A input | `FHE-O2-RESBM-STALE` |
| `q_w!=q` | `FHE-O2-RESBM-QW-UNSUPPORTED` |
| Non-uniform/multi-level action | `FHE-O2-CKKS-UNSUPPORTED-LEVEL-CONSUMPTION` |
| Retained multiplication SCC | `FHE-O2-RESBM-MUL-SCC-UNSUPPORTED` |
| Unknown frequency | `FHE-O2-RESBM-FREQ-UNKNOWN` |
| Saturated frequency/cost | `FHE-O2-FREQ-SATURATED` |
| Illegal region/cut | `FHE-O2-RESBM-ILLEGAL-CUT` |
| Stale protected site | `FHE-O2-PROTECTED-SITE-STALE` |
| Hard-site replacement | `FHE-O2-PROTECTED-SITE-REPLACEMENT` |
| Manual/off violation | `FHE-O2-BOOTSTRAP-POLICY-VIOLATION` |
| No constrained schedule | `FHE-O2-RESBM-PROTECTED-INFEASIBLE` |
| Atomic publication failure | `FHE-O2-RESBM-PUBLISH-FAILED` |

Stale/corrupt identity and semantic preconditions precede profitability or
no-candidate errors. Secondary details cannot replace the stable first code.

## 8. Detailed Work Breakdown

| Task | Change | Component/dependency | Test/evidence | Owner/reviewer | Merge rule |
| --- | --- | --- | --- | --- | --- |
| `E5B-W01` | Freeze IDs, hashes, bounds, diagnostics, ties, and stale triggers | Manifest/governance | Lock and mismatch tests | Stage/architecture | No pending authority or moving ref |
| `E5B-W02` | Accept record owners/version path | common-com/build | Crosswalk/compatibility | Record/common-com | One owner per field |
| `E5B-W03` | Define core-only raw API | Core interface | Dependency audit | Core/algorithm | No protected/provider dependency |
| `E5B-W04` | Normalize graph/state/cost/order | Core normalizer | Permutation/corruption | Core/oracle | Equal normalized digest |
| `E5B-W05` | Classify SCC and unroll/reject | SCC module/pass order | Loop/SCC cases | Graph/CKKS | No level-consuming SCC |
| `E5B-W06` | Build and verify regions | Region module | Chain/diamond/bypass | Region/artifact | Exact `R0` and depths |
| `E5B-W07` | Implement `q_w=q` ScaleMgr projection | Scale module | Threshold/full-state | CKKS/crypto | Exact projection |
| `E5B-W08` | Implement immutable cuts | Cut module | Cut/join/result-level | Cut/oracle | Exact legal cuts |
| `E5B-W09` | Implement endpoint DP/cost/frequency/ties | Solver | Cost/tie/saturation | Solver/cost | Checked exact selection |
| `E5B-W10` | Build independent core oracle | Test-only oracle | Exhaustive/random | Independent/auditor | No production imports |
| `E5B-W11` | Import/classify protected sites | Adapter input | Manual/ReLU/stale | O0 adapter/boundary | Unique exact class |
| `E5B-W12` | Partition barriers/filter candidates | Adapter/pass order | Barriers/joins/shortcuts | Adapter/core | Core stays site-neutral |
| `E5B-W13` | Recompose auto/on and replay state | Adapter/verifier | Cross-boundary cases | Adapter/CKKS | Hard sites unchanged |
| `E5B-W14` | Implement manual/off bypass | Policy adapter | Policy positives/negatives | Policy/boundary | No forbidden site |
| `E5B-W15` | Implement atomic whole-baseline fallback | Adapter/transaction | Every failure point | Transaction/reliability | No mixed result |
| `E5B-W16` | Build independent protected oracle | Test-only oracle | Seed `0x52534250` | Independent/auditor | No production imports |
| `E5B-W17` | Publish, reopen, corrupt, normalize | Serializer/build | Two runs/reopen | Record/reliability | Deterministic and closed |
| `E5B-W18` | Audit N, Relin, secret, dependency | Build/security | Symbols/includes/content | Build/security | Zero boundary violation |
| `E5B-W19` | Produce `.B`/`.T`, trace, hashes | Evidence | Full audit | Test/evidence | Complete retained bundle |
| `E5B-W20` | Sign E5C handoff | Exit manifest | Validation-only dry run | E5B/E5C | Reopen without replanning |

## 9. Proposed Commit and PR Sequence

Commits map to governing groups `S1-9`, `S1-10`, and `S1-11`:

1. `e5b-contract`: manifests, crosswalk, APIs, and failing contract tests.
2. `e5b-core-normalize-scc`: raw normalization and SCC boundary.
3. `e5b-core-regions`: artifact region builder and verifier.
4. `e5b-core-scale-cuts`: `q_w=q` projection and immutable cuts.
5. `e5b-core-endpoint-dp`: endpoint DP, costs, frequency, and ties.
6. `e5b-core-oracle`: independent raw-graph oracle and dependency audit.
7. `e5b-protected-import`: protected/baseline validation only.
8. `e5b-protected-partition`: barrier partition and candidate filtering.
9. `e5b-protected-policy`: modes, recomposition, and atomic fallback.
10. `e5b-protected-oracle`: independent constraint oracle.
11. `e5b-record-reopen`: publication, corruption, and determinism.
12. `e5b-boundary-audits`: fixed-N, Relin, dependency, and secret scans.
13. `e5b-acceptance-evidence`: runner, evidence schema, decision template only.

Core commits contain no adapter code. Adapter commits do not change core
semantics to pass protected cases. Physical records use a separate reviewed
commit. Generated run artifacts are not committed unless designated goldens.

## 10. Proposed Verification Matrix

All commands are proposed future exact interfaces. They do not claim the
scripts, fixtures, targets, or outputs exist, and this planning task does not
execute them.

| Test ID | Proposed exact command | Inputs and bounds | Exact pass rule | Retained evidence |
| --- | --- | --- | --- | --- |
| `O2-S16-001` | `python3 osprey/be/vho/tests/run_resbm_region_contract.py --build-dir build --source-ref 323e8bb02a0e036fe2369eb48badfe6692398c2b --manifest testdata/fhe_o2/resbm-region/SHA256SUMS --artifacts test-artifacts/o2/O2-S16-001` | Artifact examples; chains/diamonds/bypasses/loops/SCCs; exhaustive <=8 nodes/12 edges; 2000 random seed `0x5253424d`; unroll <=64/10000 | Regions, entries/exits, SCC/unroll exact; `R0=0`, later depths 1 | Raw/unrolled graphs, production/oracle regions, minimized mismatch, `.B`, `.T` |
| `O2-S17-001` | `python3 osprey/be/vho/tests/run_resbm_semantic_oracle.py --build-dir build --source-ref 323e8bb02a0e036fe2369eb48badfe6692398c2b --manifest testdata/fhe_o2/resbm-oracle/SHA256SUMS --artifacts test-artifacts/o2/O2-S17-001` | `q_w=q` plus rejects; thresholds, chains, frequency, cuts; <=8/12/6 levels/10 sites; 2000 seed `0x5253424d` | Legality, regions, cuts, actions, states, levels, frequency, cost, tie, diagnostic exact | Raw tables, candidates, actions, states, costs, oracle/dependency report |
| `O2-S17-PROTECTED` | `python3 osprey/be/vho/tests/run_resbm_semantic_oracle.py --build-dir build --source-ref 323e8bb02a0e036fe2369eb48badfe6692398c2b --manifest testdata/fhe_o2/resbm-protected/SHA256SUMS --protected-sites testdata/fhe_o2/resbm-protected/sites.json --mode protected --artifacts test-artifacts/o2/O2-S17-PROTECTED` | Barriers, joins, shortcuts, replaceable sites, manual/off, infeasible/corrupt; <=8/12; seed `0x52534250` | Protected identity/order/state, replacement, schedule, fallback, first diagnostic, reopen exact | Original baseline/sites, windows, results, fallback, oracle report |
| `O2-E5B-001` | `python3 osprey/be/vho/tests/run_e5b_resbm_gate.py --build-dir build --manifest testdata/fhe_o2/resbm-protected/SHA256SUMS --mode integration-reopen --artifacts test-artifacts/o2/O2-E5B-001` | Current E5A; 1..8 PUs, <=2000 ops; two producers and one consumer each; seed `0x45354231` | Core/adapter distinct; E5A unchanged; result/state replay and digests exact; no provider call | Input/result `.B`/`.T`, traces, digests, reopen report |
| `O2-E5B-002` | `python3 osprey/be/vho/tests/run_e5b_resbm_gate.py --build-dir build --manifest testdata/fhe_o2/resbm-protected/SHA256SUMS --mode failure-corruption --artifacts test-artifacts/o2/O2-E5B-002` | Failure after every core/adapter/publication step; all header/version/range/ID/state/site/cost/hash fields | No independent consumer accepts partial output; first diagnostic exact | Rejected images, injection log, normalized diffs |
| `O2-E5B-003` | `ir_b2a -st -src test-artifacts/o2/O2-E5B-001/resbm-selected-plan.B test-artifacts/o2/O2-E5B-001/resbm-selected-plan.T` | Accepted binary and original DST source | Same-stem source-aware dump exists; no omitted `-src` | `resbm-selected-plan.B`, `.T`, and `resbm-selected-plan.resbm.t` |
| `O2-E5B-004` | `python3 osprey/be/vho/tests/run_e5b_resbm_gate.py --build-dir build --manifest testdata/fhe_o2/resbm-protected/SHA256SUMS --mode boundary-audit --artifacts test-artifacts/o2/O2-E5B-004` | Complete build and artifact inventory | N/Relin unchanged; no rtlib/ANT/AIR/private header, provider load, secret, or unauthorized `be.so` dependency | Symbol/include/library, N/Relin, and secret reports |

All tests are provider-free with zero warmups and no structural/cost tolerance.
Timeouts are 300 seconds for `O2-S16-001`, 600 seconds for each oracle and
evidence audit, and 1200 seconds for integration/failure/boundary tests. Each
test runs once except the two required clean producers in `O2-E5B-001`.

If `ir_b2a -src` is unavailable, `O2-E5B-003` is `Unverified`; `-src` is not
silently omitted. The lower-case phase trace avoids `.T` collision.

## 11. Negative, Failure, and Compatibility Matrix

| Case | Required behavior | Publication |
| --- | --- | --- |
| Qualification/E5A/layout/site/cost hash changed | Stop and target requalification | None |
| Fixed `N` missing or changed | Reject before normalization | None |
| E5A status is not `CANONICAL_PRE_RESBM` | Reject | None |
| `q_w!=q`, non-uniform, or multi-level action | Reject before region planning | None |
| `R0` depth one or later region not depth one | Reject verifier/implementation | None |
| Multiplication SCC | Unroll within 64/10000 or reject | Only legal unroll |
| Unknown trip or expansion overflow | Reject without partial graph | None |
| Backedge deleted to force DAG | Reject implementation | None |
| Unknown frequency | Reject costing | None |
| Proven unreachable zero | Accept only with proof | Candidate allowed |
| Frequency/cost overflow | Saturate, diagnose, disqualify | No selected claim |
| Exact cost tie | Use frozen tuple | Deterministic result |
| Missing/duplicate/conflicting site class | Reject adapter | None |
| Manual or pre-ReLU site replaced/moved | Reject | None |
| Marked DP automatic site replaced legally | Permit and retain provenance | Selected result |
| Manual/off creates bootstrap | Reject | None |
| Constrained domain infeasible | Whole baseline only when authorized/current | Baseline only |
| Fallback mixes ReSBM/baseline actions | Reject atomicity | None |
| Relin missing, delayed, duplicated, or selected | Reject inherited/action contract | None |
| Final Q/P/CRT/provider selection appears | Reject stage ownership violation | None |
| ACE rtlib/ANT symbol/header/library appears | Stop for build review | None |
| Secret/decryptor/private bytes appear | Reject and quarantine | None |
| Unknown required record version | Reject before use | None |
| Feature-absent legacy WHIRL | Preserve accepted legacy behavior | No fabricated plan |
| Failure before commit | Roll back staged records | No valid `.B` |
| Producer dies after commit | Independent reopen decides completeness | Complete only |
| Two clean runs differ | Reject determinism | Neither accepted |
| Independent reopen unavailable | Mark `Unverified` | Not accepted |

## 12. Evidence, Retention, and Exit

### 12.1 Required bundle

The accepted bundle contains:

- authority/qualification/source/support/diagnostic/cost manifests;
- requested configuration and E5A selected-layout pre-state `.B`/`.T`;
- baseline plan `.B`/`.T`, protected sites, and baseline-DP provenance;
- raw graph, SCC decisions, regions, candidates, cuts, and selected actions;
- state replay, cost/tie, frequency, core-oracle, and adapter-oracle reports;
- policy/fallback report and final `resbm-selected-plan.B`/`.T`;
- non-colliding `resbm-selected-plan.resbm.t` phase trace;
- producer/consumer digests and independent reopen report;
- corruption/failure, symbol/include/library, fixed-N/Relin, and secret reports;
- E5C handoff, stage decision, and complete `SHA256SUMS`.

The same-stem command is exactly:

    ir_b2a -st -src resbm-selected-plan.B resbm-selected-plan.T

Artifacts are cleaned at the start of the next run, never the end of the
current run. Failed development evidence is retained at least 30 days,
milestone evidence at least 180 days, and accepted stage evidence for repository
lifetime. A failed run can retain logs/rejected inputs but not a partial valid
`.B`. `SHA256SUMS` contains no placeholder.

### 12.2 Accepted

`O2-E5B-EXIT=Accepted` requires:

- current entry gates and fingerprints;
- passing `O2-S16-001`, `O2-S17-001`, and `O2-S17-PROTECTED`;
- separate core/adapter source, dependencies, tests, and oracle proofs;
- exact production/oracle equality for all required fields;
- `R0=0`, later depth 1, `q_w=q`, and uniform one-level enforcement;
- correct SCC/loop/frequency/cost/tie rejection and selection;
- unchanged fixed N, layout, E5A state, and immediate Relin;
- unchanged protected identities/order/state and permitted replacement only;
- manual/off correctness and atomic whole-baseline fallback;
- independently replayed selected post-state;
- two-run determinism, independent reopen, and matching `.B`/`.T`;
- no provider execution/materialization, unauthorized dependency, or secret;
- signed independent review.

Exact completion wording:

    O2-E5B accepted: the provider-free ReSBM artifact core and the distinct
    protected Open64 adapter produce one verified, immutable final ReSBM
    schedule and selected post-ReSBM state for O2-E5C.

It does not claim final CKKS resolution, ANT projection, execution, Stage 1
acceptance, or O2 completion.

### 12.3 Rejected and Unverified

`Rejected` identifies the violated contract, owner, first diagnostic, evidence,
and correction. E5C cannot consume it.

`Unverified` means authority, source lock, environment, oracle independence,
same-stem evidence, reopen, or review proof is missing. It cannot unlock E5C.

## 13. Rollback, Invalidation, and Stop Rules

Rollback disables the increment and returns to the complete current qualified
O0 baseline when policy permits. It does not retain partial ReSBM, change layout,
rerun baseline DP, or weaken protected boundaries.

Any graph, layout, E5A state, fixed-N, protected-site, baseline, cost,
frequency, algorithm, tie, schema, verifier, support, diagnostic, or fingerprint
change invalidates E5B and every dependent E5C plan/key/materialization artifact.

Stop for architecture/common-com review if:

- a placement/state field has two owners or no owner;
- protected policy cannot be represented without semantic change;
- a hard/manual site must move, merge, or disappear;
- a physical record/section/operator is needed without review;
- fixed `N` or immediate Relin cannot be preserved;
- mandatory inputs require non-uniform/multi-level ReSBM or retained multiply SCC;
- selection needs unknown/saturated frequency;
- an oracle must import production logic;
- reopen requires producer memory;
- E5B requires provider query, final Q/P/CRT, or runtime materialization;
- an unauthorized `be.so` rtlib dependency is proposed;
- secret material must enter WHIRL or evidence;
- fallback cannot preserve the complete baseline atomically;
- support/tolerance must be relaxed after observing results.

## 14. Exact O2-E5C Handoff

The immutable handoff includes exit and qualification IDs; authority/source/
schema fingerprints; graph/layout/slot/fixed-N/key-domain IDs; E5A pre-state;
protected and baseline digests; mode/disposition; core digest; regions/SCCs/
endpoints/cuts/candidates; preserved/replaced sites; selected action order;
selected post-state and replay proof; range/error/component/Relin proofs;
cost/frequency/tie; fallback identity/reason; provider fingerprint for staleness;
producer/consumer digests; `.B`/`.T`/trace/evidence hashes.

The E5C consumer must independently reopen and validate all versions, IDs,
ranges, hashes, and state replay; accept only a verified disposition; preserve
fixed N/layout/sites/Relin; finalize Q/P/CRT/security/profiles/keys without
replanning; and materialize solely from the reopened selected result.

E5C may reject unsupported projection. It cannot rerun ReSBM, change placement
or N, or materialize a mixed fallback.

## 15. Requirement Traceability

| Requirement | Authority/milestone | Work | Verification | Evidence |
| --- | --- | --- | --- | --- |
| O0 qualified before O2 | User decision; `O2-O0Q-001` | `W01/W17` | All stale checks | Qualification reference |
| Immutable E5A input | Stage boundary; `S1.5A-S1.6` | `W04/W17` | `O2-E5B-001` | E5A `.B`/`.T` and digest |
| Regions/SCC rules | Fixed source; `S1.6` | `W05/W06` | `O2-S16-001` | Graph/SCC/region tables |
| Scale/cuts/DP/cost/tie | Fixed subset; `S1.7` | `W07-W10` | `O2-S17-001` | Core oracle result |
| Protected boundaries/fallback | FRZ-03 pending; `S1.7` | `W11-W16` | `O2-S17-PROTECTED` | Sites/baseline/adapter oracle |
| Fixed N and immediate Relin | Master/O2 invariants | `W18` | `O2-E5B-004` | N/Relin audit |
| Provider-free/no secret | Stage/security boundary | `W03/W18` | `O2-E5B-004` | Dependency/secret audits |
| Atomic reopen and evidence | P2/S1.1/evidence rules | `W15/W17/W19` | `O2-E5B-001/002/003` | Digests, failures, `.B`/`.T` |
| Exact E5C input | Pass-order contract | `W20` | Consumer dry run | E5C handoff |

Each accepted row closes requirement -> accepted master/hash -> ADR -> O2
milestone/test -> E5B task -> exact proposed command -> retained artifact and
review. A pending authority, unknown owner, moving ref, or placeholder hash
prevents `O2-E5B-EXIT=Accepted`.
