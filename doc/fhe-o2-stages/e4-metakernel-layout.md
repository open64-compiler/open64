# O2-E4 MetaKernel Layout Detailed Execution Plan

Status: Proposed detailed plan; implementation is locked until the entry gate is accepted
Plan version: 0.1
Date: 2026-09-15
Engineering stage: `O2-E4`
Governing milestones: `S1.3`, `S1.4`, and the provider-free portion of `S1.LAYOUT`
Exit gate: `O2-E4-EXIT`

## 1. Authority and Metadata

### 1.1 Governing documents

This plan is subordinate to the following authorities, in order:

1. Explicit accepted user decisions and repository invariants in `AGENTS.md`.
2. The accepted master architecture and its recorded amendment or successor.
3. Accepted architecture decision records within their stated scope.
4. `doc/FHE-O2-ACE-RESBM-METAKERNEL-INTEGRATION-PLAN.md`, version 1.6.
5. `doc/FHE-O2-DETAILED-EXECUTION-PLAN.md`, plan set version 0.2, for navigation.
6. This detailed stage plan.
7. Locked papers and source artifacts within their declared claim boundaries.

The recorded master baseline is commit `ee1dc6382246c58f49a3097157a8c4e8ff2440c8`.
The recorded master Markdown SHA-256 is `7EF644E9BECEFB541102514B83656C78756ECA71398A771A2A5160FC10B0CB25`.
The recorded master DOCX SHA-256 is `0018769C26B5A0BCD1BDFCBD85AA97B8BAFEA381D7640FBB9E2E81B0022013D9`.
The pending ownership decision is `doc/adr/FHE-O0-O2-PLANNER-OWNERSHIP.md`.
This plan must be updated if the accepted master successor changes the O0 baseline, fixed-N rule, layout ownership, runtime boundary, or O2 completion definition.

### 1.2 Source authority

Production MetaKernel behavior is locked to:

- repository: `../../ace-compiler`; source revision: `d0c14ab101c1f1fedfb31d8fdb553867ae7ce7be`; source role: artifact-backed production behavior inside the accepted support domain; paper role: intent, teaching cases, and a separate differential reference; Open64 role: integration, persisted semantic identity, validation, comparison, and extension ownership.

The MetaKernel paper SHA-256 is `E2AB5C3C89EBC7B5B6CEA7E79EAA00BC98975EF66EC69104EA458570DDBE2FEF`.
The source-lock manifest created by O2-E1 is authoritative for exact file hashes, line anchors, branch provenance, fixtures, diagnostics, and support rows.
No moving branch name or remote URL is sufficient source evidence.

### 1.3 Ownership

The proposed stage owner is the Open64 MetaKernel layout owner.
The proposed editor owns only the files assigned to this stage.
Required reviewers are:

- an independent artifact-oracle reviewer; a tensor and slot-semantics reviewer; a common/com record compatibility reviewer; an O0 baseline and comparison reviewer; a CKKS state consumer reviewer; an independent test and evidence reviewer.

The artifact oracle reviewer must not author or import the production search helper.

### 1.4 Entry gate

Work begins only after all of the following are accepted and current:

- `O2-O0Q-001=Qualified`; `O2-E1-EXIT`; `O2-E2-EXIT`; `O2-E3-EXIT`; accepted MetaKernel source, support, diagnostic, and fixture locks; accepted O2 selected-plan record and atomic publication interfaces; accepted virtual operation, state-label, effect, and verifier substrate; a current qualification fingerprint for the frozen Fhelipe baseline.

Planning notes may exist before these gates, but no E4 source, fixture, oracle, cost model, selected plan, or acceptance artifact may be implemented early.

### 1.5 Stale triggers

The stage stops and its evidence becomes stale if any of these inputs changes:

- accepted master or ADR text affecting layout ownership or support; `O2-O0Q-001` qualification fingerprint; frozen Fhelipe pass, configuration, support, or protected-site manifest; MetaKernel source revision or any locked source file hash; fixed-N configuration schema or slot-capacity definition; O2 record schema, capability version, or identity rules; canonical input, value lineage, slot-class, or iteration-space schema; selected-plan publication or independent-reopen protocol; diagnostic catalog, fixture generator, or oracle interface; ACE rtlib/ANT manifest fields used only as comparison fingerprints; cost-unit definition or tie-order contract.

The owner must record the stale reason, affected artifacts, and required targeted requalification before continuing.

## 2. Objective and User-Visible Capability

O2-E4 implements MetaKernel as a selectable, disableable, independently verifiable layout and kernel-planning increment over the qualified Fhelipe O0 baseline.
At exit, an accepted canonical MVM or Conv input can produce:

- a complete deterministic candidate census; an artifact-equivalent legality and cost decision; a selected MetaKernel layout identity; a complete logical-to-physical slot map; exact `valid`, `zero`, `junk`, and `gap` classifications; transformed plaintext weights with content fingerprints; required signed logical rotations, shifts, masks, and reductions; a selected-plan record that survives independent reopen; clear tensor and physical slot-oracle evidence; a provider-free comparison with the full qualified baseline layout; a stable rejection or accepted whole-baseline fallback for unsupported input.

This capability is not an encrypted execution claim.

## 3. Scope

### 3.1 In scope

The following work is required:

- MVM artifact-equivalent `(Pb, Ps)` search; independent artifact-contract MVM oracle; separate paper-strict differential calculation; selected MVM plan construction; complete MVM slot mapping and class mapping; transformed plaintext weight generation and hashing; source-locked Ke2Col Conv core; independent clear tensor and physical slot Conv oracle; early provider-free `S1.LAYOUT` comparison infrastructure; deterministic layout census and selection reports; increment-off and qualified-baseline fallback behavior; one separately reviewed plan slice for each broader Conv extension; binary selected-plan persistence through the accepted O2-E2 contract; independent reopen and verifier checks; exact diagnostics and failure-atomic publication.

### 3.2 Explicit non-goals

O2-E4 does not implement or accept:

- final CKKS parameter resolution; Q/P/CRT construction or validation; runtime projection to `CKKS_PARAMS`; ANT context construction or provider execution; generated C compilation, linking, loading, or execution; ReSBM region construction, cuts, or endpoint planning; replacement of baseline DP bootstrap sites; post-ReSBM state propagation; final logical or ANT-expanded key manifests; standard-call materialization; final Stage 1 performance acceptance; FHEFusion, HPOLY, or HPAO; physical thread scheduling, memory placement, or other O3 work; arbitrary Conv semantics outside an accepted extension row; a new WHIRL operator, table, or image not accepted in O2-E2; a direct or indirect `be.so` dependency on ACE rtlib or ANT.

### 3.3 Inherited invariants

Every implementation and test must preserve these invariants:

- `N` is fixed before canonicalization and is never changed by layout search; slot capacity is `S=N/2`; Python objects are not compiler planning identities; binary WHIRL remains the process boundary; Open64 records remain the semantic authority; the frozen Fhelipe pipeline remains the complete fallback baseline; disabling the increment does not disable any qualified baseline pass; layout selection is separate from CKKS scale and bootstrap placement; a layout plan cannot publish provisional CKKS state as canonical state; protected, manual, and pre-ReLU boundary identities are immutable inputs; no private FHE, O2, or provider operation reaches a standard WHIRL consumer; no secret material is created, consumed, or retained by this stage; every meaningful WHIRL artifact has a same-stem `.T` dump produced with `ir_b2a -st -src`.

### 3.4 Support scopes

Every fixture and decision row uses one of these scope labels:

- `shared-o0-o2-comparison-required` for MVM and locked Conv comparisons; `o0-handoff-required` for fixed N, baseline layout, protected sites, and imported baseline DP inputs; `o2-core-only` for the MetaKernel artifact-backed planner increment; `o2-extension-only` for each broader Conv row; `fallback-required` when an unsupported or unprofitable increment must retain the whole qualified baseline profile.

No E4 result may silently widen O0 support.

## 4. Dependency Models

### 4.1 Implementation and build dependencies

The implementation dependency order is:

```text
O2-E0 qualified baseline
  -> O2-E1 source, support, fixture, diagnostic, and oracle locks
  -> O2-E2 selected-plan records and atomic transaction
  -> O2-E3 virtual graph, transfer, effect, and verifier substrate
  -> E4 MVM search and independent oracles
  -> E4 MVM selected plan and persistence
  -> E4 Conv core and independent oracles
  -> E4 early layout comparison
  -> optional one-row-at-a-time Conv extensions
  -> O2-E4-EXIT
```

An optional extension cannot block acceptance of the locked core unless its row is declared required by the accepted support matrix.

### 4.2 Compiler pass-order dependency

The compiler pass order containing E4 is:

```text
canonical Open64 FHE graph
  -> optional FHEFusion in Stage 2
  -> frozen Fhelipe layout or selected MetaKernel layout
  -> recompute canonical pre-ReSBM CKKS state in O2-E5A
  -> import baseline DP and protected-site provenance
  -> optional ReSBM replacement in O2-E5B
  -> finalize CKKS and provider projection in O2-E5C
  -> standard WHIRL and ACE rtlib/ANT execution in O2-E5C
```

The implementation dependency graph does not authorize a later pass to execute inside E4.

### 4.3 Accepted upstream records and APIs

E4 consumes, without redefining:

- fixed `FHECompilationConfigIR` and `N`; canonical source, PU, operation, value, tensor, and lineage identities; immutable planner-input digest; qualified baseline layout and packing identities; iteration-space and layout-census record interfaces; O2 candidate and selected-plan record interfaces; source, config, capability, and qualification fingerprints; stable slot-class vocabulary; protected-site and baseline-DP provenance inputs; atomic all-PU publication and independent-reopen interfaces; provisional versus canonical/final CKKS state labels; diagnostics and support-matrix registries.

If an upstream interface is incomplete, E4 returns the gap to its owner rather than creating a parallel truth.

### 4.4 External evidence and environment

Provider-free E4 tests require:

- the locked MetaKernel source checkout or immutable extracted source bundle; the locked paper and teaching cases; the qualified O0 baseline plan and layout evidence; O2-E1 fixture and generator manifests; an Open64 build capable of producing and reopening O2-E2 records; `ir_b2a` with working `-st -src` support; a deterministic host-independent integer cost implementation; a clear double-precision tensor oracle.

ACE rtlib and ANT are fingerprints in comparison inputs only.
E4 does not load their libraries or call their symbols.

### 4.5 Build and link impact

The proposed production components are pure compiler-side C++ facilities under the existing VHO build ownership.
The proposed test runners are Python coordination scripts plus native unit-test executables.
This stage must not:

- add an ACE include directory to backend production compilation; include an ACE rtlib or ANT header in `be.so` sources; add an undefined ACE rtlib or ANT symbol to `be.so`; link `FHErt_common`, `FHErt_ant`, or any new third-party library into `be.so`; add provider-dependent floating-point costs to selection; use private ACE AIR classes as production records.

The link audit for E4 must show no new external dependency.

## 5. MetaKernel Algorithm Contract

### 5.1 Search inputs

The MVM search consumes immutable raw inputs including:

- stable source operation ID; matrix and tensor dimensions; `nd` and `kd` under the locked source definition; `bs_input`; `Shift_input`; fixed `N` and derived `S=N/2`; layout and slot-class input IDs; transformed-weight payload identity; source-lock and support-row IDs.

All arithmetic is checked for overflow before candidate enumeration.

### 5.2 Candidate enumeration

Production enumeration is exact and deterministic:

```text
enumerate bsopt in ascending order where bsopt divides nd
derive Pb = nd / bsopt
enumerate Ps in ascending order over divisors of Pb
derive rep = ceil((kd * Ps + nd) / kd)
derive gs = Pb / Ps
derive cost = ceil(log2(rep))
              + (bsopt - 1)
              + (gs - 1)
              + (Ps - 1)
derive f = bsopt / bs_input
derive Shiftopt = f * Shift_input
derive sf = gs * Shiftopt
```

The candidate must satisfy every integral mapping constraint involving `bs_input`, `f`, `Shift_input`, `Shiftopt`, and `sf`.
No integer truncation may silently make an invalid mapping legal.

### 5.3 Capacity contract

The artifact-backed production predicate is:

```text
if kd != S: kd * Ps + nd <= S
if kd == S: Ps == 1
```

The exact-capacity boundary is legal.
One-over-capacity is illegal.
No candidate can request a larger `N` during the compilation.
When no candidate fits, the result records a stable diagnostic and the minimum recommended `N` only as a recompilation suggestion.

### 5.4 Selection and tie order

The production winner is selected by:

1. minimum artifact `Get_num_rot` cost;
2. larger `Ps` when cost ties;
3. first candidate in ascending `bsopt` and `Ps` enumeration order when both prior fields tie.

The candidate table records the legality result, every cost component, complete selection tuple, and the first rejected predicate.

### 5.5 Paper differential

The paper-strict oracle separately computes:

- the paper Eq. 11 `gamma` replication term; the paper predicate `kd*(Ps+1)<=S`; the paper candidate and cost result; the exact reason for agreement or disagreement with production behavior.

The differential oracle does not alter production selection.
An observed mismatch is retained and explained, not forced to match.
Reports use `artifact-backed behavior` for production claims.
They do not claim general paper-exact behavior.

### 5.6 MVM selected-plan contract

The selected MVM plan records:

- raw search inputs and source identity; candidate enumeration version; winning `(Pb, Ps)` and derived `bsopt`; `rep`, `gs`, `f`, `Shiftopt`, and `sf`; each cost component and final integer cost; winner and tie reason; complete logical-to-physical input and output maps; every input replication and alignment rotation; every MetaKernel, shift, and reduction rotation; masks and required-zero behavior; complete output slot classes; transformed plaintext payload ID, shape, encoding-independent values, and content hash; signed logical rotation requirements; source, support, config, qualification, and record-schema fingerprints; fallback eligibility and exact reason when not selected.

The selected plan cannot contain provider object addresses or provider memory.

### 5.7 Conv core contract

The locked Conv core accepts only the frozen support rows, initially:

- one ciphertext input and one ciphertext output; plaintext kernel weights; square odd kernels; kernel sizes covered by the manifest, including 1, 3, and 5; stride 1; symmetric same padding; channel-divisible layouts or explicit zero padding; static dimensions and deterministic padding lineage.

The Ke2Col plan records:

- exact input, padded, kernel, and output coordinates; logical-to-physical slot maps; kernel-to-plaintext transformation; masks, shifts, rotations, and reductions; padding, replica, valid, zero, junk, and gap classifications; output layout and lineage; capacity and out-of-range proofs; transformed-weight content hashes; support-row and source-lock identities.

### 5.8 Conv extension contract

Each of the following remains an independent `o2-extension-only` row unless the source-lock review proves a narrower case belongs to the core:

- stride greater than one; valid or general padding; asymmetric padding; depthwise convolution; grouped convolution; multiple ciphertexts for one tensor; height sharding; halo exchange; output compaction.

Each extension must define:

- a frozen positive input subset; explicit boundary and unsupported cases; one capability bit; one first diagnostic; an exact tensor oracle; an exact slot-map oracle; sentinel-filled junk and gap cases; signed rotation-component recomputation; fallback behavior; a separate commit and review decision.

No extension is enabled through a generic best-effort path.

## 6. Data, Identity, and Publication Contract

### 6.1 Authoritative and derived data

Authoritative inputs are the canonical Open64 graph, fixed configuration, qualified baseline records, and accepted support contracts.
The selected layout record becomes authoritative only after complete E4 verification and atomic publication.
Candidate tables, cost reports, transformed-weight caches, and paper differentials are derived evidence.
Derived data cannot overwrite source intent, fixed N, baseline protection, or selected-layout identity.

### 6.2 Stable identities

Every record references stable IDs for:

- source and canonical operation; PU, node, edge, value, tensor, and lineage; configuration and fixed N; baseline layout; MetaKernel candidate and selected layout; slot map and slot class; transformed plaintext payload; iteration space and layout census; support row, diagnostic, source lock, and qualification fingerprint.

Pointer values, traversal addresses, thread order, and allocation order are not identities.

### 6.3 Serialization and reopen

E4 uses only the record and image decisions accepted in O2-E2.
Every persisted record follows:

- fixed-width fields; invalid-zero IDs; checked `first/count` ranges; deterministic ordering; explicit version and capability checks; no pointers or host-sized enums; exact source and config fingerprints.

The producer must exit before the independent reader reopens the selected plan.
The reader revalidates all IDs, ranges, hashes, slot maps, and support rows.
Producer-memory inspection is not reopen evidence.

### 6.4 Backward and feature-absent compatibility

The implementation must accept qualified old artifacts that contain no O2 MetaKernel selection and retain the baseline path.
Feature-absent O2 records must not require a dummy MetaKernel candidate.
An unsupported record version or unknown required capability fails closed.
Existing WHIRL binary image layout is unchanged unless O2-E2 has explicitly accepted a versioned compatible change.

### 6.5 Publication behavior

Publication uses the accepted all-PU atomic transaction.
No selected plan, transformed-weight manifest, or comparison report is visible under a valid final name until all PUs and cross-record invariants pass.
Injected failure leaves diagnostic evidence but no apparently complete `.B`.
A whole-baseline fallback publishes one coherent baseline result.
It never mixes a partial MetaKernel layout with stale baseline placement data.

## 7. Oracle and Comparison Discipline

### 7.1 Independence rules

The artifact oracle accepts raw inputs and a frozen algorithm contract.
It must not import or call:

- the production candidate enumerator; the production cost helper; the production legality predicate; the production tie-break helper; the production slot-map builder; the production transformed-weight builder.

An automated dependency audit enforces this boundary.

### 7.2 Oracle outputs

The independent oracle recomputes:

- every candidate and its enumeration position; each legality predicate and first failed condition; `rep`, `gs`, `f`, `Shiftopt`, and `sf`; every rotation cost component and total; the exact winner and tie reason; the complete physical slot map; all slot classes; transformed plaintext values and hashes; every signed logical rotation; clear logical tensor output; required-zero output slots; all out-of-range checks.

Every structural field must match exactly.
Pure double outputs use absolute and relative tolerances of `1e-12`.
NaN and infinity always fail.

### 7.3 Sentinel discipline

All required gap and junk tests use nonzero, nonuniform sentinel values.
The oracle verifies:

- valid outputs are unchanged by sentinel variation; required-zero outputs are zero within `1e-12` in the clear oracle; invalid slots are never observed as valid tensor values; masks and reductions do not leak junk or gap data; padding zeros remain distinct from inactive gap slots.

Tests that initialize every unused slot to zero do not close this gate.

### 7.4 Baseline comparison controls

The early provider-free `S1.LAYOUT` cell uses identical:

- canonical pre-layout input; fixed `N` and `S=N/2`; requested parameter intent; source and weight payload; protected-site manifest; baseline DP algorithm, configuration, and constraint manifest; non-layout controls; ACE rtlib/ANT manifest fingerprint.

The baseline and MetaKernel layouts each feed a fresh run of the same qualified baseline DP algorithm.
ReSBM is disabled in this comparison.
No stale baseline-DP site location is copied to the changed graph.
The provider-free cell checks structure, census, plan legality, and clear semantics only.
Encrypted execution, latency ratios, and final performance conclusions belong to O2-E5C and O2-E6.

### 7.5 Census metrics

The comparison recomputes, per value, operation, PU, and program:

- total logical rotations; signed logical rotation offsets; rotation components by purpose; total physical slots; active logical slots; gap slots; gap ratio; introduced, peak, and compacted gaps; masks; ciphertext count; packing density; layout conversions; permutations; rotate-add reductions; transformed plaintext bytes; static and known-frequency weighted counts.

`gap_slots = total_slots - active_slots`.
Padding and replicas are not active logical values.
Unknown frequency remains unknown and is never costed as zero.

## 8. Work Breakdown Structure

| Task ID | Concrete change | Proposed files or components | Dependency | Verification and artifacts | Owner and exit rule |
| --- | --- | --- | --- | --- | --- |
| `E4-W01` | Revalidate entry fingerprints and produce the E4 input lock | `testdata/fhe_o2/metakernel-common/SHA256SUMS`, stage runner | Entry gate | Lock report, qualification reference, normalized input digest | Stage owner; all hashes exact and current |
| `E4-W02` | Define pure checked integer helpers and candidate record adapters | proposed `osprey/be/vho/fhe_metakernel_plan.h/.cxx` | E3 APIs, E2 records | Overflow and determinism unit tests | MetaKernel owner; no provider or new library dependency |
| `E4-W03` | Implement artifact-equivalent MVM candidate enumeration | proposed `osprey/be/vho/fhe_metakernel_search.h/.cxx` | W02, E1 contract | Candidate tables for all divisors and boundaries | MetaKernel owner; exact oracle match |
| `E4-W04` | Implement capacity, integral mapping, cost components, and tie order | same pure search component | W03 | Exact/one-over capacity, tie and overflow cases | MetaKernel owner; first diagnostic exact |
| `E4-W05` | Implement the independent artifact oracle without production imports | proposed `osprey/be/vho/tests/metakernel_artifact_oracle.py` | E1 raw interface | Dependency audit and complete independent result | Independent oracle owner; no production helper dependency |
| `E4-W06` | Implement the separate paper-strict differential oracle | proposed `osprey/be/vho/tests/metakernel_paper_oracle.py` | E1 paper cases | Figure 4, MVM1, MVM2, differential report | Paper reviewer; every mismatch explained |
| `E4-W07` | Construct MVM slot maps, rotations, masks, and output classes | proposed `osprey/be/vho/fhe_metakernel_mvm.h/.cxx` | W04, E3 slot semantics | Full slot traces and sentinel cases | Tensor reviewer; no out-of-range slot |
| `E4-W08` | Transform plaintext weights deterministically | MVM transformer and external payload cache adapter | W07, E2 payload IDs | Value oracle, content hash, cache-key negatives | Tensor reviewer; exact values and hashes |
| `E4-W09` | Publish and reopen the selected MVM plan | E2 selected-plan builder, printer, verifier | W07, W08 | Matching `.B`/`.T`, two clean runs, independent reopen | Record reviewer; structure and hashes exact |
| `E4-W10` | Implement the locked Ke2Col Conv core | proposed `osprey/be/vho/fhe_metakernel_conv.h/.cxx` | W02, E3 semantics | Core 1x1, 3x3, 5x5 fixtures | Conv owner; core support exact |
| `E4-W11` | Implement independent Conv tensor and slot oracles | proposed `osprey/be/vho/tests/metakernel_conv_oracle.py` | E1 raw fixtures | Full coordinate, slot, mask, padding, sentinel traces | Independent tensor owner; no production imports |
| `E4-W12` | Add unsupported-boundary diagnostics and fallback selection | Conv planner and diagnostic registry adapter | W10, W11 | Unsupported and no-candidate cases | Stage owner; no silent approximation |
| `E4-W13` | Add early provider-free layout comparison and census | proposed `osprey/be/vho/tests/run_fhe_layout_ab_comparison.py` | W09, W12, qualified baseline | Baseline/candidate checkpoints, census, DP provenance | Comparison owner; all controls held fixed |
| `E4-W14` | Verify increment-off and whole-baseline fallback | comparison and transaction harness | W13 | Baseline manifest equality and failure injection | O0 reviewer; full baseline retained |
| `E4-W15` | Add one accepted Conv extension row | separate extension source, fixture, and manifest slice | Core exit plus accepted row | Exact extension oracle and capability bit | Extension owner; row passes independently |
| `E4-W16` | Run complete stage evidence and review | stage aggregation runner | W01-W14 and required W15 rows | Signed decision, artifact index, SHA256SUMS | Stage owner and all reviewers; E4 exit criteria pass |

### 8.1 Proposed production file boundaries

Exact filenames require repository review before implementation.
The intended component boundaries are:

- a pure search library with no WHIRL mutation; a plan builder that consumes stable Open64 identities; separate MVM and Conv slot-map builders; a transformed-plaintext builder keyed by complete fingerprints; a record adapter using only O2-E2 accepted APIs; a verifier independent from plan construction where practical; test-only independent oracles outside production linkage.

The implementation must not place an oracle inside the production library.

## 9. Proposed Commit and Pull Request Split

### 9.1 Commit sequence

1. `S1-4a`: add MVM raw contracts, fixtures, and independent oracle tests.
2. `S1-4b`: add pure artifact-equivalent MVM enumeration and checked math.
3. `S1-4c`: add paper-strict differential oracle and locked differential cases.
4. `S1-5a`: add MVM slot map, masks, rotations, and clear oracle integration.
5. `S1-5b`: add transformed plaintext generation and content fingerprints.
6. `S1-5c`: add selected-plan persistence, printer, verifier, and reopen tests.
7. `S1-6a`: add Ke2Col Conv core contracts and independent oracles.
8. `S1-6b`: add Ke2Col Conv core implementation and boundary diagnostics.
9. `S1-LAYOUT-a`: add provider-free baseline comparison and census harness.
10. `S1-LAYOUT-b`: add increment-off, fallback, stale-census, and atomicity tests.
11. `S1-7-<row>`: add one separately accepted Conv extension row.
12. `S1-E4-ACCEPT`: add only the stage evidence index and signed review decision.

### 9.2 Commit rules

Each commit has one semantic goal.
Contract and tests land with or before implementation.
An extension row never shares a commit with the core.
The acceptance commit contains no algorithm or support change.
Generated local evidence is not committed unless it is an accepted golden or review fixture explicitly named by the plan.
No commit mixes formatting changes in unrelated code.
Each commit maps to an O2 total-plan commit group and at least one traceability row.

### 9.3 Pull request review order

The recommended review order is:

1. algorithm and source-contract review;
2. independent oracle review;
3. slot and tensor-semantics review;
4. common/com record and compatibility review;
5. baseline comparison review;
6. build and dependency audit;
7. final evidence and exit review.

## 10. Verification Matrix

All commands below are proposed target commands.
They are exact future interfaces, not claims that the scripts already exist.

### 10.1 Source and dependency lock

Test ID: `O2-E4-LOCK-001`
Proposed command:

```text
python3 osprey/be/vho/tests/verify_metakernel_stage_lock.py --source-ref d0c14ab101c1f1fedfb31d8fdb553867ae7ce7be --source-lock testdata/fhe_o2/source-lock.json --qualification testdata/fhe_o2/o0-qualification/qualification-id.json --manifest testdata/fhe_o2/metakernel-common/SHA256SUMS --artifacts test-artifacts/o2/O2-E4-LOCK-001
```

Inputs include every governing fingerprint, fixture generator, diagnostic, support row, record version, and cost-unit definition.
Expected result is exact hash and dependency agreement.
The first diagnostic is `FHE-O2-SOURCE-LOCK-MISMATCH` for source drift.
Seed: none.
Bounds: complete manifest.
Platform: supported compiler host; provider-free.
Protocol: zero warmups, one run, 120-second timeout.
Artifacts: normalized lock report, dependency audit, and SHA256SUMS.
Pass rule: no placeholder, moving ref, duplicate ID, missing owner, or stale qualification fingerprint.

### 10.2 MVM contract

Test ID: `O2-S13-001`
Proposed command:

```text
python3 osprey/be/vho/tests/run_metakernel_mvm_contract.py --build-dir build --source-ref d0c14ab101c1f1fedfb31d8fdb553867ae7ce7be --manifest testdata/fhe_o2/metakernel-mvm/SHA256SUMS --artifacts test-artifacts/o2/O2-S13-001
```

Inputs include AE and golden neural shapes, Figure 4, MVM1, MVM2, non-power-of-two `rep` and `Ps`, `nd<kd`, `kd==S`, exact capacity, one-over capacity, zero weights, ill-conditioned weights, and no-candidate cases.
Expected structural results are exact for candidate legality, cost components, tie reason, winner, slot classes, transformed weights, and signed rotations.
Expected clear tolerance is `1e-12` absolute and `1e-12` relative.
Unsupported inputs emit the manifest-defined first diagnostic.
Seed: `0x4d4b5231`.
Bounds: `n,k=1..32`; fixed N with `S` in `{8,16,32,64}`; all divisor pairs.
Platform: provider-free pure search and clear oracle.
Protocol: zero warmups, one run, 10-minute timeout.
Artifacts: source delta, production and oracle candidate tables, paper differential, selected-plan records, complete slot traces, transformed-weight hashes, and matching `metakernel-mvm.B` and `metakernel-mvm.T`.
Pass rule: zero production-oracle mismatch, exact recorded differential, no implicit fallback, and no runtime claim.

### 10.3 Conv contract

Test ID: `O2-S14-001`
Proposed command:

```text
python3 osprey/be/vho/tests/run_metakernel_conv_contract.py --build-dir build --source-ref d0c14ab101c1f1fedfb31d8fdb553867ae7ce7be --manifest testdata/fhe_o2/metakernel-conv/SHA256SUMS --support testdata/fhe_o2/stage1/support-matrix.json --artifacts test-artifacts/o2/O2-S14-001
```

Inputs include locked artifact and AE golden 1x1, 3x3, and 5x5 stride-1 same-padding shapes, plus every accepted extension and unsupported boundary row.
Expected structural results are exact for plan, coordinates, slot classes, masks, padding, output layout, rotations, and transformed-weight hashes.
Expected clear tolerance is `1e-12` absolute and `1e-12` relative.
Unsupported rows emit their exact manifest diagnostic or use the explicitly accepted whole-planner fallback.
Seed: `0x4d4b5243`.
Bounds: fixed N; input and output channels 1 through 16; height and width 3 through 32; kernel sizes 1, 3, and 5; extension bounds from the manifest.
Platform: provider-free clear tensor and slot oracle.
Protocol: zero warmups, one run, 20-minute timeout.
Artifacts: source delta, selected plans, transformed-weight hashes, complete slot and sentinel traces, and matching `metakernel-conv.B` and `metakernel-conv.T`.
Pass rule: all locked core rows pass, no unsupported input enters the core, and each enabled extension passes its complete independent row.

### 10.4 Selected-plan reopen and corruption

Test ID: `O2-E4-REOPEN-001`
Proposed command:

```text
python3 osprey/be/vho/tests/run_metakernel_plan_reopen.py --build-dir build --manifest testdata/fhe_o2/metakernel-reopen/SHA256SUMS --artifacts test-artifacts/o2/O2-E4-REOPEN-001
```

Inputs include MVM and Conv plans, feature-absent plans, old qualified baseline artifacts, and corrupt version, ID, range, count, hash, and capability variants.
Expected result is exact two-run determinism and independent producer-free reopen for valid plans.
Corrupt inputs must fail before publication with their frozen first diagnostic.
Seed: `0x4d4b5250`.
Bounds: 1 through 16 PUs and 1 through 4096 plan records per case.
Platform: supported Open64 reader host; provider-free.
Protocol: zero warmups, two clean productions, one independent reopen each, 300-second timeout.
Artifacts: source `.B`/`.T`, selected-plan `.B`/`.T`, normalized dumps, corruption inputs, diagnostics, and hashes.
Pass rule: exact structure and byte determinism where specified; no producer memory dependency; no partial valid-named artifact.

### 10.5 Early layout comparison

Test ID: `O2-LAYOUT-001-EARLY`
Proposed command:

```text
python3 osprey/be/vho/tests/run_fhe_layout_ab_comparison.py --mode provider-free-early --build-dir build --source-lock testdata/fhe_o2/source-lock.json --manifest testdata/fhe_o2/layout-ab/SHA256SUMS --artifacts test-artifacts/o2/O2-LAYOUT-001-EARLY
```

Inputs include identical canonical pre-layout inputs, fixed N, parameter intent, protected sites, baseline DP manifest, MVM, Conv, residual, gap, padding, replication, capacity, unsupported, and no-profitable-candidate cases.
Expected result is an exact independent census for the full qualified baseline and MetaKernel candidate, plus a fresh baseline-DP plan for each layout.
ReSBM is disabled.
No encrypted tolerance or performance pass criterion applies in E4.
Seed: `0x4c41594f`.
Bounds: focused dimensions 1 through 32 and the locked structural ResNet-20 fixture; no ANT execution.
Platform: supported compiler host; provider-free.
Protocol: zero warmups, two structural runs, 300-second timeout.
Artifacts: canonical input, baseline and candidate pre-placement checkpoints, selected layouts, matching `.B`/`.T`, census, protected-site input, fresh baseline-DP outputs, fallback reason, and clear oracle results.
Pass rule: controls are identical, census is exact, increment-off reproduces the full baseline manifest, and no execution or latency conclusion is claimed.

### 10.6 Failure atomicity

Test ID: `O2-E4-ATOMIC-001`
Proposed command:

```text
python3 osprey/be/vho/tests/run_metakernel_publication_failures.py --build-dir build --manifest testdata/fhe_o2/metakernel-failure/SHA256SUMS --artifacts test-artifacts/o2/O2-E4-ATOMIC-001
```

Failure injection covers every candidate-table, slot-map, payload, record, cross-PU verification, rename, and checksum publication step.
Expected result is either one complete verified selected plan or no selected plan under a valid final name.
Seed: `0x4d4b5246`.
Bounds: 1 through 16 PUs and every declared publication step.
Platform: supported compiler host; provider-free.
Protocol: zero warmups, one injection per step, 600-second total timeout.
Artifacts: failure logs, temporary-file inventory, diagnostics, and final-name audit.
Pass rule: no mixed baseline/MetaKernel plan, stale transformed weight, or apparently valid partial `.B`.

### 10.7 Build and dependency audit

Test ID: `O2-E4-BUILD-001`
Proposed command:

```text
python3 osprey/be/vho/tests/audit_metakernel_build_boundary.py --build-dir build --manifest testdata/fhe_o2/metakernel-build/SHA256SUMS --artifacts test-artifacts/o2/O2-E4-BUILD-001
```

Expected result is no new ACE, rtlib, ANT, Python, or third-party dependency in backend production objects.
The audit also rejects private ACE headers and undefined provider symbols.
Seed: none.
Bounds: all changed production objects and every `be.so` consumer named by the repository build review.
Platform: each supported build configuration available to the stage.
Protocol: zero warmups, one audit per build, 300-second timeout.
Artifacts: include graph, object symbol inventory, linked-library inventory, and build logs.
Pass rule: no new library dependency and no provider surface in E4 production objects.

## 11. Negative, Failure, and Compatibility Matrix

### 11.1 Search and arithmetic negatives

Required cases include:

- zero `nd` or zero `kd`; negative or unrepresentable parsed dimensions; fixed N absent; non-power-of-two or invalid N when forbidden by the accepted config; multiplication and addition overflow in capacity formulas; no divisor candidate; non-integral `f`, `Shiftopt`, or `sf` mapping; exact capacity and one-over capacity; `kd==S` with `Ps!=1`; cost-component or total-cost overflow; ambiguous tie order; unknown support row; source-lock mismatch.

No arithmetic saturation may be treated as a valid profitable candidate.

### 11.2 Slot and tensor negatives

Required cases include:

- out-of-range physical slot; duplicate logical output ownership; missing valid output slot; invalid overlap between valid and required-zero classes; junk or gap leakage into a valid output; padding recorded as active data; replica recorded as an independent logical value; incorrect signed rotation normalization; missing mask or reduction; transformed-weight value or hash mismatch; stale layout or lineage ID; Conv output-shape disagreement; unsupported stride, padding, group, shard, halo, or compaction row.

### 11.3 Record and publication negatives

Required cases include:

- unknown record version; missing required capability; invalid-zero required ID; duplicate selected-plan owner; out-of-range `first/count` pair; source, config, schema, support, or qualification hash mismatch; feature-absent artifact with a fabricated candidate; old baseline artifact rejected solely because MetaKernel is absent; injected failure at every publication step; partial payload paired with a complete plan name; two clean runs with different normalized output.

### 11.4 Baseline and fallback negatives

Required cases include:

- increment-off disables a frozen Fhelipe pass; fallback retains a partial MetaKernel slot map; fallback reuses stale baseline-DP sites after layout change; layout comparison enables ReSBM; baseline and candidate use different N, inputs, weights, or protected sites; unknown frequency is converted to zero; an unsupported input silently enters the closest core case; a no-candidate result silently increases N; a baseline failure is relabeled as an E4 fallback success.

### 11.5 Runtime-boundary negatives

E4 must fail its dependency audit if it:

- includes an ACE compiler, rtlib, or ANT header in production code; resolves or calls an ACE rtlib or ANT symbol; adds a `be.so` dependency; creates or persists `CKKS_PARAMS` as the selected layout truth; queries Q/P/CRT or constructs a runtime context; maps `MulCC` to either `Mul_ciph` or `Mul_ciph3`; emits an ANT `Relin` call; claims encrypted execution or provider performance.

The duplicate `Mul_ciph` plus explicit `Relin` negative is exercised in E5C, not implemented in E4. E4 only proves that no runtime mapping exists here.

### 11.6 Security negatives

No E4 input or artifact should contain secret material.
The artifact scan rejects:

- secret key bytes; decryptor state; key-generation seed material; provider context dumps; unredacted environment secrets.

Logical rotation requirements are allowed semantic evidence.
Provider-expanded keys are outside E4.

## 12. Diagnostics and Fallback

Every diagnostic must be registered in the O2-E1 catalog.
Required diagnostic categories include:

- source or qualification drift; unsupported MVM shape; unsupported Conv shape or semantic row; missing fixed N; capacity exceeded; checked arithmetic overflow; invalid integral mapping; no legal candidate; invalid slot map; transformed-weight mismatch; stale selected plan; corrupt record or capability mismatch; comparison-control mismatch; atomic publication failure.

The first diagnostic is deterministic for each invalid fixture.
Fallback is permitted only under the accepted ADR and support row.
Fallback means the entire qualified baseline layout profile.
Fallback preserves the baseline pass/config manifest and creates fresh baseline-DP output for the selected baseline graph.
Fallback never publishes a mixed plan.

## 13. Evidence Contract

### 13.1 Required stage artifact tree

The accepted E4 evidence contains at least:

```text
qualification-reference.json
source-lock.json
support-matrix.json
diagnostics.json
requested-configuration.json
canonical-input.B
canonical-input.T
baseline-layout.B
baseline-layout.T
metakernel-mvm.B
metakernel-mvm.T
metakernel-conv.B
metakernel-conv.T
selected-plan.B
selected-plan.T
mvm-candidates.json
mvm-artifact-oracle.json
mvm-paper-differential.json
conv-plan.json
conv-tensor-oracle.json
layout-and-slot-map.json
slot-sentinel-trace.json
transformed-plaintext-manifest.json
rotation-requirements.json
layout-census.json
baseline-dp-comparison.json
fallback-report.json
reopen-report.json
dependency-audit.json
negative-tests.json
diagnostics.log
stage-decision.md
SHA256SUMS
```

### 13.2 WHIRL evidence

Each meaningful `.B` uses a same-stem `.T`.
The proposed dump command form is:

```text
ir_b2a -st -src selected-plan.B selected-plan.T
```

The original source path recorded in DST remains available for `-src`.
If a phase trace would collide on a case-insensitive filesystem, it uses a descriptive name such as `selected-plan.metakernel.t`.
Failure logs do not use the final name of a valid `.B`.

### 13.3 Retention

Failed development evidence is retained for at least 30 days.
Milestone evidence is retained for at least 180 days.
Accepted E4 evidence and its source locks are retained for the repository lifetime or until a formally migrated successor supersedes them.
Artifacts are indexed by absolute host path in the review report.
Local generated evidence is not committed unless explicitly accepted as a golden fixture.

## 14. Traceability Matrix

| Requirement | Master or ADR status | O2 total-plan mapping | E4 work item | Verification | Retained evidence |
| --- | --- | --- | --- | --- | --- |
| O0 complete before O2 | Master successor pending; user decision recorded | `O2-O0Q-001` | `E4-W01` | `O2-E4-LOCK-001` | qualification reference and lock report |
| Fixed N and `S=N/2` | Governing invariant | Sections 4, 9, 10; `S1.3/S1.4` | `E4-W03`, `E4-W10` | `O2-S13-001`, `O2-S14-001` | requested config and candidate tables |
| Artifact-backed MVM behavior | Locked source decision | Section 10.1; `S1.3` | `E4-W03-W06` | `O2-S13-001` | source delta, candidates, two oracles |
| Exact slot classes | Master and canonicalization contract | Sections 6, 10, 15 | `E4-W07`, `E4-W11` | MVM and Conv contracts | slot maps and sentinel traces |
| Ke2Col Conv core | Locked support contract | Sections 4.2, 10.2; `S1.4` | `E4-W10-W12` | `O2-S14-001` | Conv plan and tensor oracle |
| One gate per broader Conv extension | O2 extension policy | Sections 4.2 and 10.2 | `E4-W15` | extension row in `O2-S14-001` | support row and signed decision |
| Binary reopen | WHIRL process-boundary invariant | Sections 8 and 13; `S1.3/S1.4` | `E4-W09` | `O2-E4-REOPEN-001` | matching `.B`/`.T`, reopen report |
| Baseline-preserving comparison | Pending ADR plus O2 contract | `S1.LAYOUT` | `E4-W13-W14` | `O2-LAYOUT-001-EARLY` | census, DP provenance, fallback report |
| No runtime execution in E4 | Stage boundary | Sections 12.3 and 13 | all work items | `O2-E4-BUILD-001` | include, symbol, and link audit |
| Failure-atomic publication | Existing transaction invariant | Section 12.2; `S1.1` reuse | `E4-W09`, `E4-W14` | `O2-E4-ATOMIC-001` | failure logs and final-name audit |

Before E4 acceptance, every row must contain an accepted master section and hash or an explicit pending status that blocks the exit gate.
No pending master conflict may be waived by a stage reviewer.

## 15. Exit Review

### 15.1 Required pass conditions

`O2-E4-EXIT` is accepted only when:

- every entry fingerprint is current; all required MVM and Conv core support rows pass; production MVM candidate, legality, cost, winner, and tie fields match the independent artifact oracle exactly; paper-strict differentials match the frozen expected differences exactly; complete MVM and Conv slot maps and slot classes match independent oracles; clear logical outputs meet `1e-12` absolute and relative tolerances; sentinel-filled junk and gaps cannot affect valid or required-zero output; transformed plaintext values and hashes are deterministic and exact; selected-plan records survive producer exit and independent reopen; two clean runs meet the accepted structural and byte determinism rules; increment-off reproduces the full qualified baseline manifest; early layout comparison uses fresh baseline DP on each layout with ReSBM off; unsupported inputs are rejected or use only the accepted whole-baseline fallback; failure injection leaves no partial valid-named plan; no provider header, symbol, execution, or new `be.so` dependency enters E4; every required `.B` has a same-stem `.T` from `ir_b2a -st -src`; all required reviewers sign the retained stage decision.

### 15.2 Exit result vocabulary

The only stage results are:

- `Accepted` when every required row passes; `Rejected` when a stage implementation or contract is wrong; `Unverified` when required inputs, tools, environment, or evidence are absent.

Only `Accepted` produces `O2-E4-EXIT`.
The report may state:
`O2-E4 MetaKernel layout planning accepted for the locked provider-free support domain.`
It must not state:

- `O2 Stage 1 accepted`; `O2 complete`; `MetaKernel encrypted execution accepted`; `MetaKernel is generally paper-exact`; `ANT performance accepted`.

## 16. Rollback and Invalidation

If E4 is rejected, disable the MetaKernel increment and retain the full qualified baseline profile.
Rollback does not remove, weaken, or reconfigure baseline Fhelipe passes.
Rollback invalidates:

- MetaKernel selected layouts; transformed plaintext caches; layout census and comparison reports; all canonical pre-ReSBM state derived from the rejected layout; downstream ReSBM, final CKKS, key, materialization, and cost evidence; Stage 1 acceptance evidence consuming the rejected plan.

Rollback does not invalidate the qualified O0 bundle unless the defect exposes an O0 qualification error or a stale fingerprint.
If an O0 defect is discovered, E4 stops and returns it to the O0 owner for targeted correction and complete requalification.

## 17. Stop and Architecture Review Rules

Implementation stops and returns to architecture or governing-plan review if:

- O2 work is requested without a current qualified O0 fingerprint; layout selection has two authoritative owners; the selected layout would change fixed N; a support row requires a silent shape or semantic approximation; artifact and paper behavior are being conflated; a broader Conv row lacks an exact independent oracle; a record cannot reopen without producer memory; an E4 change requires a new WHIRL operator, table, or image not accepted by O2-E2 and common/com review; an ACE AIR class or provider object is proposed as Open64 semantic truth; an ACE rtlib or ANT dependency is proposed for `be.so`; final CKKS, ReSBM, runtime projection, generated C, or execution is required to close an E4 test; cost arithmetic overflows or relies on unknown frequency; an unsupported row can publish a partial plan; a tolerance is relaxed after observing a result; a master or ADR conflict remains pending at exit review.

## 18. Downstream Handoff

The accepted E4 handoff to O2-E5A contains:

- canonical source and planner-input fingerprints; fixed configuration and unchanged N; selected layout identity and complete slot map; all slot classes and lineage; selected MVM or Conv plan and algorithm version; transformed plaintext manifest and hashes; signed logical rotation requirements; masks, shifts, reductions, and output layout; protected-site and baseline-DP input provenance; support, source-lock, and qualification identities; clear oracle and independent-reopen decision; no canonical pre-ReSBM, post-ReSBM, or final CKKS state.

O2-E5A must discard any provisional candidate state and recompute one canonical pre-ReSBM CKKS state over this selected layout.
O2-E5B may consume that canonical state only after `O2-E5A-EXIT`.
O2-E5C owns all ANT projection, materialization, generated C, and execution.
O2-E6 owns the complete layout comparison, encrypted performance protocol, and the only Stage 1 acceptance decision.
