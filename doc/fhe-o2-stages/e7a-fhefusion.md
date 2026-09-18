# O2-E7A FHEFusion Detailed Execution Plan

Status: Proposed detailed plan; implementation is locked until all entry gates are accepted
Plan version: 0.1
Date: 2026-09-15
Engineering stage: `O2-E7A`
Governing milestones: `S2.0`, `S2.1`, `S2.2`
Exit gate: `O2-E7A-EXIT`
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
Recorded master Markdown SHA-256: `7EF644E9BECEFB541102514B83656C78756ECA71398A771A2A5160FC10B0CB25`.
Recorded master DOCX SHA-256: `0018769C26B5A0BCD1BDFCBD85AA97B8BAFEA381D7640FBB9E2E81B0022013D9`.
Pending ADR: `doc/adr/FHE-O0-O2-PLANNER-OWNERSHIP.md`.

Accepted replacement master and ADR versions, commits, and hashes must replace
pending references before implementation starts.

### 1.2 FHEFusion source authority

The proposed production catalog is locked to:

- paper `../../ace-paper/FHEFusion_paper.pdf` with SHA-256
  `415E2ACFE8E7505263B369BF3B550A190612E48E8E92D66A321C0D8098A24A7E`;
- fixed code `origin/fhefusion@f18f971710b7270aec2c3963878ac3493d9f8e06`;
- artifact anchors `nn-addon/vector/src/vector.cxx`, `nn-addon/include/nn/vector/cf.h`,
  `nn-addon/include/nn/vector/cf_simp.h`, `nn-addon/include/nn/vector/mask_fusion.h`,
  `nn-addon/include/nn/vector/strided_slice_fusion.h`, and `nn-addon/include/nn/vector/selective_strided_slice.h`;
- architecture Appendix E only as a recorded catalog cross-check, not as proof
  that an artifact rule has general semantics beyond its reviewed domain.

`S2.0` records a source-to-paper-to-Open64 delta for every rule. An unmatched
paper or artifact behavior is explicit and gets its own legality/oracle/claim;
no branch, guide, or paper title without a fixed hash is authority.

### 1.3 Ownership and review

The proposed owner is the Open64 FHEFusion pass owner. Required reviewers are
the E6 Stage 1 integration owner; mandatory canonicalization/gatekeeper owner;
common/com record owner; E4 slot-oracle owner; E5A-E5C CKKS, ReSBM, and
materialization owners; independent paper/artifact and tensor/slot-oracle
reviewers; fixed-N/security/profitability reviewer; ACE rtlib/ANT security
reviewer; build/`be.so` reviewer; E7B/E8 consumers; and evidence/reliability
reviewer.

One common/com editor owns any physical record change. The independent oracle
owner cannot own or import the production matcher, attribute updater, candidate
enumerator, cost selector, or commit engine.

### 1.4 Entry gate and consumed evidence

Implementation begins only after `PRE-O2-LOCK-EXIT`; a current
`O2-O0Q-001=Qualified`; accepted, current `O2-E1-EXIT` through `O2-E6-EXIT`;
the exact signed statement `O2 Stage 1 accepted`; independently reopenable
`S1.0-S1.9` and `S1.LAYOUT` evidence; matching `.B`/same-stem `.T` for the E6
canonical graph, selected plan, and materialized plan; fixed FHEFusion sources,
deltas, support, diagnostics, cost requirements, and oracle interfaces ready
for `S2.0`; matching fixed-N/Relin/policy/record/ANT fingerprints; and no
unresolved master or ADR conflict affecting Stage 2 controls.

`O2-E6-EXIT` is the only stage-entry acceptance gate. E7A and E7B may be
implemented in parallel after it, but neither may consume an unaccepted result
from the other to close its own exit.

### 1.5 Stale triggers

E7A stops and discards unpublished results after a change to authority, options,
ownership, O0 qualification, E6 decision/input/support/plan, FHEFusion sources
or delta, catalog/order/traversal, rule semantics/oracle/diagnostics, canonical
shape/mask/slice/slot/gap/lineage, fixed N/security/parameter/key domain,
layout/state/site/placement/key/materialization contracts, ANT/rtlib/build/cost
fingerprint, test bounds/generators/seeds/metrics/thresholds/ties, or E2/P2
schema/serialization/transaction behavior.

The stale report names the first mismatch, affected candidate and selected-plan
IDs, invalidated evidence, responsible owner, and targeted requalification path.

## 2. Objective and Delivery Boundary

E7A implements and certifies the accepted CF, MF, and SF FHEFusion catalog on
the canonical high-level Open64 FHE graph before layout planning. It produces a
deterministic catalog/support decision; immutable candidates with legality and
profitability proofs; one atomic fixed point or verified no-fusion result;
exact output/mask/slice/slot/gap/lineage evidence; an E7A-local invalidation and
rebuilt Stage 1 plan; fixed-N/security/ANT cost proof; and a reopenable handoff.

The user-visible capability is an optional Stage 2 pre-layout optimization that
reduces depth or execution cost without changing source semantics, requested
ring dimension, security, protected policy, or the public runtime boundary.

E7A is not final O2 certification. E8 owns cross-pass replanning, the legal
FHEFusion-plus-HPAO matrix, `O2-ALL`, bare `-O2`, and the O3 handoff.

## 3. Scope and Invariants

### 3.1 In scope

E7A includes the full reviewed registry/source crosswalk; deterministic
distributive exposure, successor-first search, predecessor fallback, and fixed
point; per-candidate semantic legality; fixed-N/security/gap/layout/key/memory/
ANT profitability; immutable candidates, stable ties, and atomic commit; exact
attribute and Open64 cleanup; semantic re-derivation; overlap/permutation/cycle
tests; nonzero sentinels; E7A-local E4-E5C invalidation/rebuild; off/per-family/
auto/fallback modes; binary reopen and generated-C/ANT validation; and matching
`.B`/same-stem `.T` for meaningful WHIRL checkpoints.

### 3.2 Complete family inventory

The `S2.0` catalog must classify at least the recorded Appendix E rules exactly:

| Family | Required rule IDs |
| --- | --- |
| CF, constant folding | `FUSED-GEMV-SCALAR`, `FUSED-SCALAR-GEMV`, `FUSED-CONV-SCALAR`, `FUSED-SCALAR-CONV`, `FUSED-SCALAR-RELU`, `SCALAR`, `CONCAT`, `RELU-DIS`, `AVGPOOL-SCALAR`, `FLATTEN-SCALAR`, `RESHAPE-SCALAR` |
| MF, masking folding | `FUSED-RELU-MASKING`, `MASKING-SCALAR`, `SCALAR-MASKING`, `FUSED-CMPT-MASKING`, `MASKING-MUL`, `MASKING-ADD-RELU`, `FLATTEN-MASKING` |
| SF, Strided_Slice/compaction folding | `FUSED-CMPT`, `FUSED-GEMV-CMPT`, `CONCAT-CMPT`, `CMPT-MUL`, `CMPT-ADD`, `CONV-CMPT`, `RELU-CMPT`, `AVGPOOL-CMPT`, `CMPT-SCALAR`, `SCALAR-CMPT`, `FLATTEN-CMPT`, `RESHAPE-CMPT` |

This inventory is a planning minimum, not permission to enable an unmapped
rule. `CMPT` is the catalog spelling for compaction/Strided_Slice behavior.
Stable public reports use the accepted logical name and version, never a private
physical carrier.

### 3.3 Explicit non-goals

E7A does not alter or conditionalize mandatory canonicalization; run FHEFusion
after layout/CKKS/HPOLY/materialization; implement or modify MetaKernel, ReSBM,
baseline DP, E5, HPOLY, HPAO, E8 cross-pass certification, or O3 physical work;
change N; weaken protected provenance; change immediate Relin; create duplicate
semantic truth; import ACE AIR/private structures; expose private generated-C
headers; add an rtlib/ANT dependency to `be.so`; persist secrets; allocate an
unreviewed WHIRL/public contract; overclaim the paper; or claim O2 completion.

### 3.4 Inherited invariants

- Binary WHIRL is the process boundary; Open64 owns FHE-through-key semantics.
- Canonicalization runs for all PUs; fusion uses high-level logical Open64 ops.
- Requested N is fixed, byte-stable, and implies capacity `S=N/2`.
- Every valid/zero/junk/gap slot has exact lineage and observability.
- Ordered effects are never crossed, duplicated, speculated, or removed.
- Ordinary ciphertext values have two components; each downstream `MulCC` has
  only a transient third component followed by one adjacent Relin.
- ANT mapping stays `Mul_ciph3 -> Relin`, never `Mul_ciph` plus duplicate Relin.
- Execution uses public ACE rtlib C symbols, `FHErt_common` + `FHErt_ant`,
  `LIB_ANT`; server evaluation receives no secret or decryptor.
- Failure publishes no valid partial plan; meaningful `.B` has same-stem `.T`.

### 3.5 Support boundary

| Dimension | Accepted behavior | Rejected or fallback behavior |
| --- | --- | --- |
| Input graph | Accepted E6 canonical, static, file-wide Open64 FHE graph | Noncanonical, stale, unresolved, or private graph |
| Rule | Exact versioned catalog entry with source and oracle | Unknown, ambiguous, disabled, or unmapped rule |
| Constant | Immutable compile-time scalar/weight with complete fingerprint | Dynamic, NaN/Inf, unknown rounding, or stale payload |
| Mask | Derivable binary 0/1 mask with exact shape and valid region | Nonbinary, ambiguous, misaligned, or observable junk |
| Slice | Static checked begin/end/stride and exact composed map | Dynamic bounds, overflow, overlap ambiguity, lost gap state |
| Shape | Static rank/dimensions and exact attribute update | Dynamic or incompatible shape |
| Effects | Pure admitted logical operation only | Context/key/lifetime/provider action or unknown effect |
| Control | Intraprocedural acyclic match with resolved uses | Cross-call, unresolved phi, irreducible flow, or unsafe fanout |
| Capacity | Minimum required N is at most requested fixed N | Larger-N candidate rejected with recommendation only |
| Security | Rebuilt plan validates under unchanged security intent | Insecure or unrepresentable resolved envelope |
| Profit | Complete model proof under current ANT capability | Unknown/saturated/unprofitable candidate |
| Fallback | Current accepted E6 no-fusion plan, or F0-authorized full baseline | Mixed fusion graph with stale Stage 1 records |

Scope labels remain `o0-handoff-required`,
`shared-o0-o2-comparison-required`, `o2-core-only`, `o2-extension-only`, and
`fallback-required` with their governing O2 plan Section 4.2 meanings.

## 4. Implementation Dependencies and Pass Order

### 4.1 Implementation and build dependency

The implementation dependency is:

    current O2-O0Q-001
      -> E1-E5C accepted services -> E6 Stage 1 acceptance
      -> S2.0 catalog/support/source lock
      -> CF/MF/SF matcher, legality, oracle, and transaction
      -> S2.1 semantic and fixed-point certification
      -> S2.2 fixed-N profitability and downstream Stage 1 rebuild
      -> E7A evidence and handoff
      -> E8 composition and final certification

E7B may proceed from E6 in parallel. E7A does not depend on E7B code or results.
The E8 integration owner later composes both accepted branches.

Proposed paths, subject to review, are `osprey/be/vho/fhe_fusion_{catalog,match,
legality,profit,commit,verify}.{h,cxx}`, Section 10 runners under
`osprey/be/vho/tests/`, and `testdata/fhe_o2/fhefusion/` plus `testdata/fhe_o2/fhefusion-profit/`.

These are proposed locations, not existence claims.

### 4.2 Build and link impact

- Match/semantic/clear-oracle code is provider-free; profitability reads an
  immutable capability/cost manifest; encrypted tests reuse the E5C route.
- `be.so` includes no ACE rtlib/ANT/AIR/private or frontend-builder dependency
  and gains no rtlib symbol; generated C uses only the public allowlist.
- Open64 graph/SSA/dominance/table/pool services are reused at accepted points.
- Equivalent scalar algebra uses `wn_simp_code.h`, not a duplicate engine.
- Physical record changes require isolated common/com compatibility review.

The build gate compares `be.so` and every known shared consumer before and after
E7A. Any new dependency or unresolved symbol stops the stage even if semantic
tests pass.

### 4.3 Compiler pass order

The compiler order, which must not be inferred from build order, is:

    fixed-N application.fhe.B -> independent reopen -> all-PU canonicalization
      -> verify mask/slice/slot/gap/lineage -> distributive CF/MF/SF search
      -> immutable legality/profitability -> deterministic atomic commit
      -> semantic re-derivation -> idempotent fixed point -> publish fused graph
      -> invalidate Stage 1 derived data -> rerun layout -> canonical pre-state
      -> regenerate baseline DP sites -> optional accepted ReSBM replacement
      -> verify post-state -> finalize parameters/security/keys -> ANT projection
      -> optional E7B HPOLY/HPAO -> standard WHIRL -> whirl2c -> ANT execution

Mandatory canonicalization precedes FHEFusion. After every committed fusion,
only affected semantic facts are re-derived before the next match. Before any
downstream plan is published, the complete fused graph is re-canonicalized and
verified with the accepted P0 owner so stale descriptors cannot survive.

### 4.4 E7A reanalysis boundary versus E8

E7A must rebuild the Stage 1 pipeline once for each final no-fusion/per-family/
selected E7A graph used to certify S2.1 or S2.2. This is an E7A-local proof that
the selected FHEFusion graph is consumable and that its cost evidence comes from
the actual post-fusion layout, state, placement, keys, and ANT plan.

E7A does not run HPAO or a FHEFusion-plus-HPAO pair. It does not own the F0
factorial matrix, `O2-ALL`, final bare `-O2`, or final cross-pass recosting.
Those are E8 responsibilities after both E7A and E7B exits are accepted.

## 5. Interface, Record, and Version Contracts

### 5.1 Sources of truth

| Input | Owner | E7A use | Mutation rule |
| --- | --- | --- | --- |
| `FHECompilationConfigIR` | O0/common-com | Fixed N, security, control mode | Read-only; N never changes |
| Canonical FHE graph | P0/E6 owner | Match and rewrite input | Original immutable |
| Slot validity/mask/slice | Canonicalization owner | Legality and updates | Derived only through owner API |
| Fusion catalog | S2.0/FHEFusion owner | Rule semantics and order | Immutable and hash-bound |
| Stage 1 plan | E6 owner | No-fusion baseline and fallback | Read-only |
| Protected policy | O0 owner | Boundary/provenance regeneration | No weakening or reclassification |
| Cost/capability model | E1/runtime owner | Candidate profitability | Read-only and fingerprinted |
| E4-E5C services | Stage 1 owners | Downstream rebuild | Invoke accepted APIs; do not fork |

### 5.2 Rule catalog contract

Each `FHEFusionRule` records fixed-width rule ID/version, family, paper/source
anchors, behavior label, algebraic class, pattern, source and target classes,
phase ordinal, priority, traversal permissions, preconditions, attribute-update
function ID, benefit class, profitability policy, invalidation mask, diagnostic,
oracle ID, support scope, capability bit, and fingerprint.

The catalog is immutable during a compilation. It has no executable callback,
host pointer, STL object, or address-based order in persisted form. A source
delta, changed callback implementation, or changed priority changes its hash.

### 5.3 Candidate contract

Each candidate contains input graph digest, epoch, stable source/value/node/edge
IDs, direction, path edge sequence, target ID, ordered rule IDs, matched operand
and attribute IDs, before/after semantic digest, mask/slice/slot lineage delta,
effect proof, capacity/security facts, provisional downstream census, complete
integer-nanosecond cost components, rejection reason, tie tuple, and fingerprint.

A multi-step distributive exposure is never committed alone. It is represented
as an atomic candidate bundle ending in a legal profitable fusion. Intermediate
graphs are private candidates and cannot become materializer input.

### 5.4 Decision and commit contract

The decision record contains catalog/input/capability/cost fingerprints, every
enumerated candidate ID in stable order, legal and rejected sets, selected ID,
complete comparison tuple, tie reason, fallback/no-fusion reason, and producer
digest.

One candidate is applied to a cloned immutable graph. The updater derives exact
attributes and lineage, runs accepted cleanup services, verifies the entire
affected region, and only then atomically swaps the staged graph into the
current epoch. Failure leaves the prior committed epoch unchanged.

### 5.5 Invalidation and reanalysis contract

A commit immediately invalidates affected high-level analyses and, before
handoff, every derived Stage 1 layout candidate/selection, transformed plaintext,
cache, provisional/canonical pre-state, DP automatic site, ReSBM candidate/action,
post/final state, parameter proof, logical/ANT key set, minimum-N/security result,
provider projection, materialized WHIRL, generated C, and cost.

Requested configuration, fixed N, source semantic identities, user/manual
boundaries, and protected-policy ownership are preserved. Graph-derived site
locations are regenerated from lineage under the same accepted policy; they are
not copied from stale E6 locations.

### 5.6 Proposed semantic APIs

These are proposed semantic interfaces, not accepted declarations:

    LoadFHEFusionCatalog(CatalogImageView, CatalogBuilder, DiagnosticSink)
    EnumerateFHEFusionCandidates(CanonicalGraphView, CatalogView,
                                 CandidateSetBuilder, DiagnosticSink)
    EvaluateFHEFusionCandidate(CanonicalGraphView, CandidateView,
                               CapabilityCostView, EvaluationBuilder,
                               DiagnosticSink)
    SelectAndCommitFHEFusion(CanonicalGraphView, CandidateSetView,
                             DecisionBuilder, CanonicalGraphBuilder,
                             DiagnosticSink)
    VerifyFHEFusionFixedPoint(CanonicalGraphView, DecisionLogView,
                              DiagnosticSink)

Inputs are immutable, failure returns no valid output ID, and builders cannot
publish before all-PU verification and atomic transaction commit.

### 5.7 Serialization, reopen, and compatibility

E7A uses E2/P2-approved selected-plan storage. It does not allocate a `WT_*`,
ELF section, table, operator, or opcode by itself. If candidate/decision/report
fields cannot fit the accepted extensibility path, implementation stops for a
separate common/com version and compatibility review.

Physical records use fixed-width fields, invalid-zero IDs, checked first/count
ranges, explicit schema/algorithm/capability versions, deterministic order, no
pointers, no host-sized enums, no nested STL, and complete fingerprints.

Required behavior is unchanged old-reader support; no fabricated feature-absent
decision; exact E6 digest with fusion off; fail-closed unknown versions and
corrupt headers/counts/ranges/IDs/hashes; optional fields only under accepted
rules; no O0/Stage 1 dependency on E7A records; binary producer-exit/reopen/
`ir_b2a`; and no private FHE/O2 operation at final `whirl2c` input.

## 6. Candidate, Legality, Profitability, and Fixed-Point Contracts

### 6.1 Deterministic enumeration

The catalog freezes phase ordinals and rule priorities in `S2.0`. Within an
epoch, sources are ordered by stable PU, node, value, and edge IDs. Search visits
successors in stable edge order first. Predecessor fallback is permitted for a
source only when no legal successor target exists under the catalog contract.

Commutative traversal records every crossed edge. Associative targets terminate
a candidate path. A noncompatible or effectful node terminates that path.
Candidates with identical normalized post-graph digests are deduplicated by the
smallest stable candidate ID. Pointer order, hash-table iteration, allocation,
thread scheduling, and PU read order never affect enumeration.
The catalog must reconcile the fixed artifact pass order and the paper graph
algorithm explicitly. The implementation follows the accepted catalog order;
it cannot infer order from header registration or visitor traversal.

### 6.2 Common legality

Every candidate proves exact logical op/type/shape/attributes; stable source,
value, lineage, use, and target IDs; pure effects and legal dominance/alias/
fanout replacement; no call/phi/loop/exception/provider/key/lifetime/bootstrap/
manual/protected/pre-ReLU crossing; exact logical outputs and slot classes; no
range/overflow/overlap fault; exact source/provenance; fixed trust/key/config;
and a supported catalog row with an independent oracle.
Legality is semantic and target-independent except for explicit provider
capability predicates. Profitability never makes an illegal candidate legal.

### 6.3 CF legality

CF additionally requires immutable finite compile-time constants with content,
shape, element type, encode-scale, rounding, parameter-intent, and payload
fingerprints. Folding into Conv/GEMV weights updates the plaintext payload and
cache identity atomically. A stale or mutable payload rejects the candidate.

`RELU-DIS` requires the accepted polynomial identity, nonzero divisor, exact
coefficient transformation under the declared arithmetic model, conservative
range/error propagation, and unchanged ReLU/approximation provenance. Existing
`wn_simp_code.h` is used only for equivalent scalar algebra after FHE-specific
preparation and before FHE-specific postprocessing.

### 6.4 MF legality

MF requires a binary 0/1 mask derived independently from static source shape,
kernel, stride, padding, and valid-output positions. Mask/input shapes and slot
lineage align exactly. A moved or absorbed mask cannot expose junk to any
consumer or change a required-zero position.

`MASKING-ADD-RELU` additionally proves that the ReLU operand's zero/junk region
aligns with positions cleared by the mask. `FUSED-CMPT-MASKING` proves that the
slice selects exactly the observable valid region. Nonzero sentinel tests are
mandatory; all-zero junk cannot certify MF.

### 6.5 SF legality

SF uses checked static begin/end/stride composition, exact output shape, exact
logical-to-physical map, and complete inherited `Gap_Strides`. Moving a slice
through Conv, GEMV, Pool, ReLU, Flatten, Reshape, Concat, Add, Mul, or Scalar
updates every affected operator attribute through its catalog updater.

A consumer may accept gaps only when its semantics and selected layout planner
prove them unobservable. Slice propagation cannot silently turn padding or
replicas into active values. A complex slice that still requires masking keeps
that mask unless an MF rule independently proves absorption.

### 6.6 Fixed-N and security feasibility

Every post-candidate graph records active/gap slots, ciphertexts, shards,
packing density, minimum required N, depth, bootstraps, rotations, logical keys,
provider-key bytes, transformed-weight bytes, and peak live bytes.

Requested N is read from `FHECompilationConfigIR` and never rewritten. If
minimum required N exceeds it, the candidate is rejected and a minimum
acceptable or recommended N is reported for an explicit user recompilation.
That diagnostic cannot mutate or restart the current compilation.

The candidate also proves that the eventual rebuilt Q/P envelope, bootstrap
profiles, and achieved security can be represented by the current ANT manifest.
Unsupported or insecure projections reject before commit.

### 6.7 Profitability and deterministic selection

Costs are nonnegative integer nanoseconds with checked arithmetic. Each cost
component maps to an independently counted operation, state transition, byte,
or capacity fact. Unknown frequency, missing cost, or saturation disqualifies a
candidate; it is not guessed or treated as zero.

The proposed `S2.0/S2.2` policy accepts a direct candidate when depth decreases
without predicted slowdown, or runtime strictly decreases without depth growth.
An enabling mask/compaction move is atomic with its qualifying payoff and must
strictly reduce runtime. No candidate increases N, violates security, or loses
slot proof. The final fixed point is no worse than the no-fusion E6 input under
identical fixed parameters and the complete downstream model.

Among profitable candidates, select the minimum stable tuple
`(predicted_total_ns, multiplicative_depth, bootstrap_count, ciphertext_count,
rotation_count, logical_key_count, provider_key_bytes, peak_live_bytes,
transformed_weight_bytes, phase_ordinal, rule_priority, source_id,
direction_rank, edge_path, target_id, rule_sequence, candidate_id)`.

Every field and tie reason is persisted. Measured runtime validates the model;
measurement noise never changes a decision retrospectively within one run.

### 6.8 Atomic commit and termination

Only the selected candidate bundle is committed in an epoch. Attribute updates,
cleanup, descriptor derivation, local verification, decision logging, and graph
replacement are one transaction. A failure preserves the previous epoch.

Each accepted commit must strictly decrease the versioned fusion potential
`(predicted_total_ns, multiplicative_depth, observable_cleanup_count,
high_level_op_count)` lexicographically under the accepted policy. Seen normalized
graph digests are retained; revisiting one is a cycle and fails closed. Catalog
and support manifests freeze candidate/commit resource bounds and the stable
diagnostic for exceeding them.

The pass reaches fixed point only when a fresh complete enumeration has no legal
profitable candidate. Rerunning E7A on its accepted output produces no commit and
the same normalized graph, decisions, reports, and downstream plan digest.

### 6.9 Independent semantic oracle

The independent oracle consumes raw graphs, catalog, constants, masks, slices,
shapes, and fixed configuration. It imports no production matcher, updater,
traversal, simplifier, cost, selection, commit, or census helper.

For local graphs with at most 12 nodes it enumerates every accepted overlap and
legal rule permutation. It recomputes logical tensors and every physical slot
class, mask, slice map, `Gap_Strides`, attribute, lineage edge, depth, capacity,
cost component, candidate order, winner, commit log, fixed point, and first
diagnostic. Random coverage uses 2000 legal contexts and seed `0x4655534e`.

## 7. Diagnostics and Failure Policy

E1 and S2.0 own exact spellings. Proposed diagnostic purposes are:

| Purpose | Proposed code |
| --- | --- |
| Catalog/source mismatch | `FHE-O2-FUSION-CATALOG-MISMATCH` |
| Stale E6/config/capability input | `FHE-O2-FUSION-STALE` |
| Noncanonical input | `CFHEFUSE-NonCanonicalFHEGraph` |
| Rule precondition | `CFHEFUSE-RulePreconditionFailed` |
| Mask derivation | `CFHEFUSE-MaskDerivationFailed` |
| Slice mismatch | `CFHEFUSE-SliceBoundaryMismatch` |
| Observable junk | `CFHEFUSE-JunkSlotMayBeObserved` |
| Lost gap lineage | `CFHEFUSE-GapStateLost` |
| Larger required N | `CFHEFUSE-FusionWouldIncreaseRingDimension` |
| Unsupported security/provider | `FHE-O2-FUSION-PROVIDER-UNSUPPORTED` |
| Missing or saturated cost | `FHE-O2-FUSION-COST-UNUSABLE` |
| Legal but unprofitable | `FHE-O2-FUSION-NOT-PROFITABLE` |
| Cycle or nondeterministic order | `FHE-O2-FUSION-NONDETERMINISTIC` |
| Atomic publication failure | `FHE-O2-FUSION-PUBLISH-FAILED` |

Authority/schema/hash corruption precedes canonical-form and semantic errors.
Semantic legality precedes capacity/security/provider checks. Profitability is
last. A rejected candidate may record details, but the stable first diagnostic
cannot change with traversal order or hide an earlier contract failure.

## 8. Detailed Work Breakdown

| Task | Change | Component/dependency | Test/evidence | Owner/reviewer | Merge rule |
| --- | --- | --- | --- | --- | --- |
| `E7A-W01` | Freeze authority, source, hashes, and entry fingerprints | Governance/E6 | Lock mismatch tests | Stage/architecture | Current E6 only |
| `E7A-W02` | Freeze all CF/MF/SF rows and source deltas | S2.0 catalog | Positive/negative per rule | Catalog/paper-source | No unmapped rule |
| `E7A-W03` | Freeze pass order, traversal, priorities, and ties | Catalog/search | Permutation tests | Search/oracle | No incidental order |
| `E7A-W04` | Accept record/version/capability path | common-com/E2 | Compatibility/corruption | Record/common-com | One owner per field |
| `E7A-W05` | Normalize immutable high-level graph | Canonical input | Two-run/reopen digest | Canonicalization | No private IR |
| `E7A-W06` | Implement stable source/target enumeration | Matcher | Order/fanout/path cases | Search/oracle | Stable IDs only |
| `E7A-W07` | Implement atomic distributive bundles | Matcher/commit | Enabling/payoff cases | Search/tensor | No exposed intermediate |
| `E7A-W08` | Implement common legality | Legality/effects | Shape/alias/barrier negatives | Semantic/WOPT | Exact rejection |
| `E7A-W09` | Implement CF and scalar reuse protocol | CF/wn_simp | All CF rows/payloads | CF/Open64 continuity | No duplicate algebra engine |
| `E7A-W10` | Implement MF mask/lineage updates | MF | Mask/sentinel cases | MF/slot oracle | Binary exact masks |
| `E7A-W11` | Implement SF slice/gap updates | SF | Slice/gap/capacity cases | SF/layout oracle | Exact slot maps |
| `E7A-W12` | Re-derive affected canonical facts | Canonical owner API | Idempotence/stale facts | Canonicalization | No duplicate truth |
| `E7A-W13` | Implement fixed-N capacity/security filter | Profitability | Boundary/recommendation | CKKS/security | Zero N mutation |
| `E7A-W14` | Implement complete candidate census | Candidate records | Enumeration/dedup | Search/record | All candidates retained |
| `E7A-W15` | Implement ANT integer-ns cost components | Cost model | Component holdout | Cost/performance | Complete explainability |
| `E7A-W16` | Implement deterministic selection tuple | Selector | Exact ties/permutations | Selector/oracle | One stable winner |
| `E7A-W17` | Implement atomic commit and cycle guard | Transaction | Failure at each step | Reliability/oracle | Prior epoch intact |
| `E7A-W18` | Build independent semantic oracle | Test-only oracle | Exhaustive/random | Independent/auditor | No production imports |
| `E7A-W19` | Certify fixed point and second-run identity | Verifier | Reapply/permutation | Verifier/oracle | Zero second commit |
| `E7A-W20` | Publish and independently reopen fused graph | Serializer/E2 | Binary/corruption | Record/reliability | Exact digest |
| `E7A-W21` | Invalidate all downstream Stage 1 facts | Driver/plan | Stale-injection matrix | Integration/all owners | No stale consumer |
| `E7A-W22` | Rerun E4 layout and slot oracle | Accepted E4 API | Baseline/MetaKernel rows | E4/E7A | Post-fusion graph only |
| `E7A-W23` | Rerun E5A-E5B state and placement | Accepted E5 APIs | State/site/action replay | CKKS/ReSBM | Same protected policy |
| `E7A-W24` | Rerun E5C resolution, keys, projection | Accepted E5C API | N/security/keys/ANT | E5C/runtime | Exact projection |
| `E7A-W25` | Execute CF/MF/SF and selected profiles | Validation | Clear/decrypted outputs | Test/runtime | Numeric gates pass |
| `E7A-W26` | Verify no-fusion and failure fallback | Compatibility | Digest/atomicity | Reliability/E6 | No mixed plan |
| `E7A-W27` | Audit Relin, public ABI, symbols, and secrets | Build/security | Boundary scan | Build/security | Zero violation |
| `E7A-W28` | Produce `.B`/`.T`, traces, reports, hashes | Evidence | Bundle audit | Evidence/reviewer | Complete retention |
| `E7A-W29` | Sign E7B/E8 handoff | Exit manifest | Consumer dry runs | E7A/E7B/E8 | Reopen without inference |

## 9. Proposed Commit and PR Sequence

Commits map to governing groups `S2-1`, `S2-2`, and `S2-3`:

1. `e7a-catalog-contract`: source lock, complete rules, support, diagnostics,
   order, and failing catalog tests.
2. `e7a-record-contract`: reviewed candidate/decision/report extensions and
   legacy/feature-absent tests when a physical change is required.
3. `e7a-search`: immutable graph normalization, source/target traversal, and
   candidate IDs without mutation.
4. `e7a-legality`: common shape/effect/alias/barrier legality only.
5. `e7a-cf`: complete accepted CF family and independent CF cases.
6. `e7a-mf`: complete accepted MF family and sentinel oracle cases.
7. `e7a-sf`: complete accepted SF family and gap/capacity oracle cases.
8. `e7a-selection`: immutable candidate census, integer costs, tuple, and ties.
9. `e7a-commit-fixed-point`: atomic bundles, cleanup, cycle guard, and idempotence.
10. `e7a-independent-oracle`: test-only exhaustive/random oracle and dependency audit.
11. `e7a-fixed-n-profitability`: capacity/security/recommended-N and ANT model gates.
12. `e7a-downstream-invalidation`: E4-E5C invalidation and accepted-service rerun.
13. `e7a-compatibility-fallback`: fusion-off, feature-absent, rollback, and corruption.
14. `e7a-boundary-audits`: immediate Relin, rtlib allowlist, `be.so`, and secret scans.
15. `e7a-acceptance-evidence`: runners, evidence schema, and decision template only.

Each commit has one semantic goal. CF, MF, and SF do not share implementation
commits merely to shorten review. The independent oracle is separate from
production helpers. The acceptance-evidence commit contains no algorithm change.
Generated run artifacts are not committed unless designated goldens.

## 10. Proposed Verification Matrix

All commands below are proposed future exact interfaces. They do not claim the
scripts, fixtures, targets, or outputs exist, and this planning task does not
execute them.

| Test ID | Proposed exact command | Inputs and bounds | Exact pass rule | Retained evidence |
| --- | --- | --- | --- | --- |
| `O2-S20-001` | `python3 osprey/be/vho/tests/verify_fhefusion_catalog.py --source-lock testdata/fhe_o2/stage2/source-lock.json --catalog testdata/fhe_o2/fhefusion/rules.json --support testdata/fhe_o2/fhefusion/support-matrix.json --artifacts test-artifacts/o2/O2-S20-001` | Fixed paper/ref hashes; every CF/MF/SF row; positive/negative context and all legal rule permutations | Exact source mapping, pre/postconditions, order, capability, diagnostic, oracle; no placeholder or ambiguity | Source delta, normalized catalog/support/diagnostics, hashes |
| `O2-S21-001` | `python3 osprey/be/vho/tests/run_fhefusion_semantic_contract.py --build-dir build --catalog testdata/fhe_o2/fhefusion/rules.json --manifest testdata/fhe_o2/fhefusion/SHA256SUMS --artifacts test-artifacts/o2/O2-S21-001` | Rule graphs, overlaps, pass permutations, sentinel junk/gaps, no-fusion; exhaustive <=12 nodes; 2000 random seed `0x4655534e` | Match/reject, slot map, lineage, order, commit log, fixed point exact; clear `1e-12/1e-12`; decoded `1e-4/1e-6` | Matching pre/post `.B`/`.T`, slot traces, outputs, rule log |
| `O2-S22-001` | `python3 osprey/be/vho/tests/run_fhefusion_profitability_contract.py --build-dir build --manifest testdata/fhe_o2/fhefusion-profit/SHA256SUMS --model testdata/fhe_o2/cost/ant-v1.json --artifacts test-artifacts/o2/O2-S22-001` | Gap densities 0..15/16; fixed `N` in `2^12..2^16`; shards 1..8; fixed/auto-remaining with same N | Capacity/security/recommended-N, cost components, decision, measurement exact; zero N mutation | Config, candidates, decisions, model validation, C, matching `.B`/`.T` |
| `O2-E7A-001` | `python3 osprey/be/vho/tests/run_e7a_fhefusion_gate.py --build-dir build --catalog testdata/fhe_o2/fhefusion/rules.json --manifest testdata/fhe_o2/fhefusion/SHA256SUMS --mode selection-reopen --artifacts test-artifacts/o2/O2-E7A-001` | Current E6 input; no-fusion, CF, MF, SF, selected; 1..8 PUs, <=2000 ops; two clean producers | Catalog/candidates/winner/fixed point and independent reopen exact; original E6 input unchanged | Input/fused `.B`/`.T`, candidates, decisions, digests, reopen report |
| `O2-E7A-002` | `python3 osprey/be/vho/tests/run_e7a_fhefusion_gate.py --build-dir build --catalog testdata/fhe_o2/fhefusion/rules.json --manifest testdata/fhe_o2/fhefusion-profit/SHA256SUMS --mode downstream-replan-ant --artifacts test-artifacts/o2/O2-E7A-002` | Selected fused graph; accepted E4-E5C services; fixed and auto-remaining suites with same N | All stale Stage 1 facts rejected; rebuilt layout/state/placement/keys/ANT exact; decoded gates pass | Invalidation report, replanned `.B`/`.T`, C, manifests, output |
| `O2-E7A-003` | `python3 osprey/be/vho/tests/run_e7a_fhefusion_gate.py --build-dir build --catalog testdata/fhe_o2/fhefusion/rules.json --manifest testdata/fhe_o2/fhefusion/SHA256SUMS --mode failure-corruption --artifacts test-artifacts/o2/O2-E7A-003` | Failure at every match/evaluate/commit/publish/replan step; all version/range/ID/hash fields | No partial candidate graph or downstream plan accepted; first diagnostic exact; E6 fallback intact | Rejected images, failure logs, fallback digests, normalized diffs |
| `O2-E7A-004` | `ir_b2a -st -src test-artifacts/o2/O2-E7A-001/fusion-selected.B test-artifacts/o2/O2-E7A-001/fusion-selected.T` | Accepted binary and preserved original DST source | Same-stem source-aware dump exists; logical names only; no omitted `-src` | `fusion-selected.B`, `fusion-selected.T`, `fusion-selected.fhefusion.t` |
| `O2-E7A-005` | `python3 osprey/be/vho/tests/run_e7a_fhefusion_gate.py --build-dir build --catalog testdata/fhe_o2/fhefusion/rules.json --manifest testdata/fhe_o2/fhefusion-profit/SHA256SUMS --mode boundary-audit --artifacts test-artifacts/o2/O2-E7A-005` | Complete build, generated C, plan, server artifact, and symbol inventory | Fixed N and immediate Relin exact; public ABI only; no secret/decryptor; no unauthorized `be.so` dependency; no private final op | N/Relin, symbols/includes/libraries, secret scan, final WHIRL audit |

`O2-S20-001` is provider-free, runs once, and has a 120-second timeout.
`O2-S21-001` uses two warmups and ten paired samples with a 20-minute timeout
per sample. `O2-S22-001` uses five warmups and thirty paired focused samples,
a 20-minute timeout per sample, and no outlier deletion. Structural portions of
`O2-E7A-001/003/005` use zero warmups, one run, and a 1200-second timeout;
`O2-E7A-001` has two clean producers and independent consumers.

If `ir_b2a -src` is unavailable, `O2-E7A-004` is `Unverified`; `-src` is not
silently omitted. The lower-case phase trace avoids `.T` collision.

### 10.1 Numerical and model gates

Pure double tensor/slot tolerances are `abs<=1e-12`, `rel<=1e-12`; focused
decrypted tolerances are `abs<=1e-4`, `rel<=1e-6`; required-zero absolute error
is at most `1e-8`; NaN/Inf fails; and observations stay within predicted bounds.
For operations at least 1 microsecond, ANT holdout MAPE is at most 20 percent
and P95 APE at most 35 percent; smaller-operation absolute error is at most 2
microseconds. Wrong-order rate is at most 10 percent under governing separation,
and every selected candidate has independently explained cost components.

E7A reports per-family and no-fusion/selected pairs; E8 applies the final
`O2-ALL` cross-pass non-regression protocol.

## 11. Negative, Failure, and Compatibility Matrix

| Case | Required behavior | Publication |
| --- | --- | --- |
| Qualification/E6/catalog/cost hash changed | Stop and target requalification | None |
| Fixed N missing or changed | Reject before search | None |
| Input not canonical or not E6-accepted | Reject | None |
| Unknown/duplicate/ambiguous rule ID | Reject catalog | None |
| Rule source mapping or updater hash stale | Reject catalog | None |
| Dynamic/nonfinite/stale constant | Reject CF candidate | Prior epoch only |
| `RELU-DIS` zero divisor or unsafe range | Reject CF candidate | Prior epoch only |
| Nonbinary or misaligned mask | Reject MF candidate | Prior epoch only |
| Junk becomes observable | Reject candidate | Prior epoch only |
| Slice bound/stride overflow or mismatch | Reject SF candidate | Prior epoch only |
| Gap lineage lost or padding becomes active | Reject candidate | Prior epoch only |
| Effectful/barrier/call/phi/loop crossing | Reject candidate | Prior epoch only |
| Unsafe fanout or overlapping rewrite | Reject candidate | Prior epoch only |
| Minimum required N exceeds requested N | Diagnose recommendation; do not mutate N | Prior epoch only |
| Security/provider profile unsupported | Reject candidate or stage | Prior epoch only |
| Unknown frequency or missing cost | Disqualify profitability | Prior epoch only |
| Cost arithmetic saturates | Diagnose and disqualify claim | Prior epoch only |
| Exact candidate tie | Use complete frozen tuple | One deterministic winner |
| Candidate graph digest repeats | Diagnose cycle and roll back current bundle | Prior epoch only |
| Commit updater or cleanup fails | Atomic rollback | Prior epoch only |
| Final fixed point unprofitable after rebuild | Roll back all E7A commits | E6 no-fusion only |
| Stale layout/state/placement/key consumed | Reject downstream consumer | None |
| Protected/manual/pre-ReLU provenance lost | Reject and roll back E7A | E6 no-fusion only |
| Relin missing, delayed, or duplicated | Reject downstream rebuilt plan | None |
| `Mul_ciph` plus explicit Relin | Reject materialization | None |
| ACE private header/symbol appears | Stop for interface review | None |
| ACE rtlib/ANT appears as `be.so` dependency | Stop for build review | None |
| Server secret/decryptor or retained secret bytes | Reject and quarantine | None |
| Unknown required record version | Reject before use | None |
| Feature-absent legacy WHIRL | Preserve accepted legacy behavior | No fabricated decision |
| Fusion disabled with Stage 1 controls retained | Reproduce E6 graph/plan digest | Identity result |
| Advanced-off | Preserve complete qualified O0 profile under F0 mapping | Baseline only |
| Failure before atomic commit | Remove staged output name | No valid `.B` |
| Producer dies after commit | Independent reopen decides completeness | Complete only |
| Two clean runs differ | Reject determinism | Neither accepted |
| Independent reopen unavailable | Mark `Unverified` | Not accepted |
| Private FHE/O2 op reaches whirl2c | Reject final boundary | None |

Candidate rejection normally leaves the current fusion epoch and continues
search. A stage-level invariant, stale input, nondeterminism, or failed final
rebuild rejects the entire E7A result. No policy converts a corrupt partial
fusion result into a mixed fallback.

## 12. Evidence, Retention, and Exit

### 12.1 Required bundle

The accepted bundle contains at least:

```text
authority-and-entry.json o0-qualification-reference.json
e6-stage1-acceptance-reference.json source-lock.json fhefusion-source-delta.json
rules.json support-matrix.json diagnostics.json environment.json
requested-configuration.json provider-capabilities.json ant-v1-cost-model.json
input.stage1.B input.stage1.T canonical.prefusion.B canonical.prefusion.T
candidate-census.json candidate-legality.json candidate-profitability.json
candidate-ties.json fusion-decision-log.json fixed-point-report.json
fusion-selected.B fusion-selected.T fusion-selected.fhefusion.t
slot-and-lineage-before.json slot-and-lineage-after.json clear-oracle-output.json
fusion-semantic-oracle.json fixed-n-capacity-security-diagnostics.json
invalidation-report.json replanned-layout-and-slot-map.json
replanned-ckks-state-trace.json replanned-bootstrap-actions.json
replanned-logical-key-requirements.json replanned-ant-key-manifest.json
fusion-replanned-plan.B fusion-replanned-plan.T
fusion-materialized.o2.mid.B fusion-materialized.o2.mid.T
generated.c generated-c-symbols.txt build-and-link.log
server-key-census.json secret-scan.json decoded-output.json oracle-output.json
raw-samples.csv metrics.json cost-model-validation.json
failure-and-corruption.json compatibility-and-fallback.json
e7b-handoff.json e8-handoff.json stage-decision.md SHA256SUMS
```
Every `.B` above has its same-stem `.T`. The exact representative command is:

    ir_b2a -st -src fusion-selected.B fusion-selected.T

Artifacts are cleaned at the start of the next run, never at the end of the
current run. Failed development evidence is retained at least 30 days,
milestone evidence at least 180 days, and accepted stage evidence for repository
lifetime. A failed run may retain rejected inputs and logs but cannot retain a
partial output with a valid `.B` name. `SHA256SUMS` contains no placeholder.

### 12.2 Accepted

`O2-E7A-EXIT=Accepted` requires:

current entry gates/fingerprints; `O2-S20-001` through `O2-S22-001` and
`O2-E7A-001` through `O2-E7A-005`; a complete unambiguous source-mapped catalog;
exact production/oracle semantics, slots, lineage, enumeration, winner, commit,
and fixed point; deterministic terminating atomic selection; per-selection
fixed-N/security/profit proof; unchanged N/policy/Open64 ownership; complete
post-fusion Stage 1 rebuild; exact logical/ANT keys and public rtlib projection;
one adjacent Relin per rebuilt `MulCC`; no secret/private ABI/`be.so` dependency/
private final op; two-run reopenable `.B`/`.T`; tested off/rollback/feature-absent/
authorized fallback; and signed semantic/performance/compatibility/security review.

Exact completion wording:

    O2-E7A accepted: the reviewed CF, MF, and SF FHEFusion catalog produces a
    deterministic, fixed-N-safe, semantically exact high-level fusion fixed
    point and a fully rebuilt Stage 1 downstream plan for E7B and E8.

It does not claim HPOLY/HPAO acceptance, cross-pass E8 certification, final
Stage 2 acceptance, O2 completion, or O3 readiness.

### 12.3 Rejected and Unverified

`Rejected` identifies the contract, owner, first diagnostic, evidence, affected
IDs, and correction. E7B may continue from E6 but cannot consume the graph; E8
cannot start. `Unverified` means required authority/source/environment/provider/
model/oracle/evidence/reopen/review is missing; it cannot unlock E8 or be
relabeled as no-fusion acceptance.

## 13. Rollback, Invalidation, and Stop Rules

Candidate rollback restores the previous committed fusion epoch. Stage rollback
restores the complete current E6 no-fusion graph and plan. A F0-authorized whole
profile fallback may instead select the complete qualified O0 profile, but it
cannot mix E7A nodes with stale Stage 1 layout, placement, state, keys, or calls.

Any graph, catalog, constant, mask, slice, lineage, fixed-N, security, provider,
cost, schema, tie, support, diagnostic, or fingerprint change invalidates E7A
and all E7A-derived E4-E5C plans. An E7B change does not invalidate E7A alone;
E8 owns recomposition and revalidation of their combined profile.

Stop for architecture/common-com review if:

ownership is missing/duplicated; a rule lacks fixed source, exact semantics, or
independent oracle; pass order depends on incidental traversal; legality crosses
an ordered/protected boundary; a physical/public contract lacks review; N must
change; unique adjacent Relin cannot survive; the oracle needs production logic;
profitability needs unknown/saturated cost or post-observation changes; stale
downstream data cannot be detected; fallback is not coherent and atomic;
generated C needs private ACE or another provider; an unapproved `be.so` rtlib
dependency appears; server/evidence needs secret material or a decryptor; reopen
needs producer memory; a private FHE/O2 op reaches `whirl2c`; completion requires
HPAO or E8 cross-pass work; or an observed result forces relaxed support/gates.

## 14. Exact O2-E7B and O2-E8 Handoffs

### 14.1 E7B coordination handoff

The immutable E7B-facing handoff contains E7A exit/source/catalog/schema hashes;
the selected or no-fusion high-level graph identity; fixed N/config/key domain;
logical shape and source lineage; protected/manual/pre-ReLU provenance; fusion
decision log; invalidation mask; rebuilt final CKKS plan identity; bootstrap
barriers; immediate-Relin proof; ACE rtlib/ANT capability fingerprint; and
matching `.B`/`.T`/trace hashes.

E7B implementation does not wait for this handoff and does not modify it. Once
both stages are accepted, E7B may use it as a compatibility input to prove that
its HPOLY boundary recognizes the rebuilt CKKS plan. That check cannot reopen
E7A or E7B exit decisions. Joint profitability remains E8 work.

### 14.2 E8 required handoff

The immutable E8 handoff additionally includes every candidate and rejection,
family/off profile IDs, before/after graphs, complete invalidation/rebuild proof,
post-fusion layout/state/placement/parameter/key/provider records, minimum-N
diagnostics, full cost components, measurements, fallback identity/reason,
producer/consumer digests, and all evidence hashes.

E8 must independently reopen and validate versions, IDs, ranges, hashes,
semantics, fixed N, policy, state, keys, and provider projection. It then owns
legal FHEFusion/HPAO composition, cross-pass invalidation, factorial ablations,
`O2-ALL`, bare `-O2`, final certification, and the O3 handoff. E8 may reject a
composition but cannot silently rewrite E7A's catalog, decision, or evidence.

## 15. Requirement Traceability

| Requirement | Authority/milestone | Work | Verification | Evidence |
| --- | --- | --- | --- | --- |
| Stage 1 accepted before E7A | User sequence; `S1.9` | `W01/W20` | Entry/reopen checks | E6 decision and `.B`/`.T` |
| Full catalog/source mapping | O2 `S2.0` | `W02/W03` | `O2-S20-001` | Catalog/source delta |
| CF/MF/SF exact semantics | O2 `S2.1` | `W07-W12/W18` | `O2-S21-001` | Oracle, slots, rule log |
| Deterministic selection/commit | O2 `S2.1` | `W14/W16/W17/W19` | `O2-E7A-001/003` | Candidates, ties, commit log |
| Fixed N and profitability | O2 `S2.2` | `W13/W15` | `O2-S22-001` | Capacity/security/cost reports |
| Downstream invalidation/rebuild | Governing plan Section 12.4; O2 `S2.2` | `W21-W24` | `O2-E7A-002` | Invalidation and replanned bundle |
| Open64 ownership/public ANT ABI | `AGENTS.md`; FRZ-08/09 | `W24/W27` | `O2-E7A-005` | Symbol/include/projection audits |
| Immediate Relin/no duplicate | Master/O2 invariant | `W24/W27` | `O2-E7A-002/005` | State/action/generated-C audit |
| No server secret/decryptor | Runtime/security decision | `W24/W27` | `O2-E7A-005` | Key census and secret scan |
| No unauthorized `be.so` dependency | `AGENTS.md` | `W04/W27` | `O2-E7A-005` | Link-closure/symbol report |
| No private op at whirl2c | O2 hard gate | `W20/W24/W27` | `O2-E7A-004/005` | Final `.B`/`.T` and audit |
| Compatibility and atomic fallback | P2/evidence rules | `W20/W26` | `O2-E7A-001/003` | Reopen/fallback/corruption |
| E7B/E8 boundary | Governing plan Section 12.4 | `W29` | Consumer dry runs | Signed handoffs |

Each accepted row closes requirement -> accepted master/hash -> ADR -> O2
milestone/test -> E7A task -> exact proposed command -> retained artifact and
review. A pending authority, unknown owner, moving ref, placeholder hash, stale
E6 fingerprint, or unavailable required provider prevents
`O2-E7A-EXIT=Accepted`.
