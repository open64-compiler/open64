# O2-E8 Final Certification and O3 Handoff Detailed Execution Plan

Status: Proposed detailed plan; implementation is locked until all entry gates are accepted
Plan version: 0.1
Date: 2026-09-15
Engineering stage: `O2-E8`
Governing milestones: `S2.5`, `S2.6`
Exit gate: `O2-E8-EXIT`

## 1. Authority and Metadata

### 1.1 Governing authority

This plan is subordinate to these authorities, in order:

1. Explicit accepted user decisions and repository invariants in `AGENTS.md`.
2. The accepted master architecture and an accepted successor or amendment.
3. Accepted architecture decision records within their stated scope.
4. `doc/FHE-O2-ACE-RESBM-METAKERNEL-INTEGRATION-PLAN.md`, version 1.6.
5. `doc/FHE-O2-DETAILED-EXECUTION-PLAN.md`, plan set version 0.2, for navigation.
6. Accepted O2-E0 through O2-E7B plans/bundles, then this plan, then locked research evidence within its claim boundaries.

Recorded master commit: `ee1dc6382246c58f49a3097157a8c4e8ff2440c8`.
Recorded master Markdown SHA-256: `7EF644E9BECEFB541102514B83656C78756ECA71398A771A2A5160FC10B0CB25`.
Recorded master DOCX SHA-256: `0018769C26B5A0BCD1BDFCBD85AA97B8BAFEA381D7640FBB9E2E81B0022013D9`.
Pending ADR: `doc/adr/FHE-O0-O2-PLANNER-OWNERSHIP.md`.

Accepted replacement master/ADR versions, commits, hashes, option mapping, and completion wording must replace pending references before certification can be accepted. E8 cannot close an upstream architecture conflict.

### 1.2 Ownership and review

The proposed owner is the O2 final integration and release owner. Required reviewers are the architecture/ADR, O0 qualification, E6, E7A, E7B, common/com, canonicalization, MetaKernel, CKKS, ReSBM, key, materialization, ACE rtlib/ANT runtime/security, correctness, cost, performance, build, reliability, evidence, and O3 consumer owners. One release owner signs; no pass owner solely reviews its result, and O3 cannot redefine an O2 field.

### 1.3 Required entry state

Certification execution begins only when all of the following are current:

- `PRE-O2-LOCK-EXIT=Accepted` with accepted master and ADR references;
- `O2-O0Q-001=Qualified` and its complete qualification fingerprint;
- `O2-E1-EXIT` through `O2-E5C-EXIT` are accepted and current;
- `O2-E6-EXIT=Accepted` with the exact signed claim `O2 Stage 1 accepted`;
- `O2-E7A-EXIT=Accepted` with a reopenable FHEFusion handoff;
- `O2-E7B-EXIT=Accepted` with a reopenable HPOLY/HPAO handoff;
- all `S1.0-S1.9`, `S1.LAYOUT`, and `S2.0-S2.4` evidence is available;
- fixed F0 profile, option, legal-composition, and support matrices are accepted;
- fixed workload, seed, tolerance, cost, performance, and retention protocols exist;
- ACE rtlib/ANT source, public headers, build, library, and capability hashes match;
- the server-evaluation lifecycle is accepted with no secret key or decryptor;
- any physical record used by E8 has an accepted version and common/com owner; and
- no required release row contains a placeholder, moving reference, or unknown owner.

E7A and E7B may have developed in parallel from E6. Their separate acceptance does not prove composition: E8 reopens both, applies accepted semantics in compiler order, invalidates all affected derived state, and certifies the combined plan.

### 1.4 Consumed evidence IDs

The entry manifest hashes `O2-O0Q-001`, `O2-P2-001`, applicable `O2-S10-001` through `O2-S26-001`, `O2-LAYOUT-001`, every E1-E7B exit, both handoffs, authority decisions, source/schema/provider/build/protocol locks, and every consumed `.B`/same-stem `.T`.

Strict O0 ancestry is transitive and explicit. Every profile points to the same qualified O0 identity, pass/config/support manifest, protected policy, fixed N, provider, and qualification fingerprint; no synthetic, narrowed, rebuilt, or relabeled O0 substitutes.

### 1.5 Stale triggers

E8 stops and invalidates unpublished results after any change to authority, options, ancestry, qualification, E6/E7A/E7B, pass order, catalog/support/diagnostics, graph/layout/slots/lineage, state/actions/sites, parameters/keys, HPOLY/HPAO, fixed N/security/approximation, provider/rtlib/build/target, cost/workload/protocol, record/serialization/materialization, or O3 schema.

The stale report names the first mismatch, invalidated profiles/results, last valid ancestor, owner, and targeted requalification. A passing E8 test cannot waive stale qualification.

## 2. Objective and Delivery Boundary

### 2.1 Objective

E8 composes accepted E7A/E7B capabilities in compiler order, proves complete invalidation/replanning, runs the full ablation/default matrices, certifies bare `-O2`, disposes every claim/risk, and publishes an independently reopenable O3 semantic handoff. The user-visible result is one evidence-bound correctness, compatibility, security, provider, cost, performance, determinism, fallback, and support decision.

### 2.2 In scope

E8 includes:

- validate E6/E7A/E7B handoffs and compose FHEFusion before layout and HPAO after final CKKS planning;
- invalidate/rebuild layout, state, placement, parameters, keys, HPOLY/HPAO input, provider projection, materialization, and costs;
- run required baseline, single, legal-pair, Stage 1/2, all-on, off, fallback, fixed-resolved, and auto-remaining profiles;
- certify bare `-O2`, advanced-off, two clean replays, full public ACE rtlib/ANT results, compatibility, failure, security, symbols, dependencies, focused cases, and complete ResNet-20; and
- close claims/risks/evidence and publish/version/independently reopen the O3 handoff with O3-off identity.

### 2.3 Explicit non-goals

E8 does not modify existing O0/O2 algorithms, broaden support, relax observed tolerances, change N, enable unapproved HPAO-MD/lazy Relin/public WHIRL, import private ACE, add an unauthorized `be.so` dependency, select another provider, persist secrets, or overclaim papers. It contracts and verifies the O3 handoff only; physical scheduling, NUMA/GPU or memory/data placement, working sets, transfer overlap, O3 defaults/profitability, performance, and execution acceptance remain unimplemented.

### 2.4 Inherited invariants

- Binary WHIRL is the boundary; Open64 is semantic authority; meaningful `.B` has `ir_b2a -st -src` same-stem `.T`.
- Qualified Fhelipe O0/protected DP is ancestry and whole fallback; O2-off never strips it.
- Requested N is fixed before canonicalization with `S=N/2`; all-PU canonicalization precedes FHEFusion.
- Layout precedes canonical pre-ReSBM state; ReSBM replaces permitted automatic sites only; protected/manual/pre-ReLU identity persists.
- Ordinary ciphertext values have two components; every `MulCC` has a transient third component and one immediately adjacent `Relin`.
- ANT is exactly public `FHErt_common` plus `FHErt_ant`, `LIB_ANT`, with `Mul_ciph3 -> Relin`; bootstrap stays opaque and server evaluation has no secret/decryptor.
- Publication is all-PU atomic, and no private FHE/O2/HPOLY operation reaches `whirl2c`.

### 2.5 Support and completion boundary

Rows retain `o0-handoff-required`, `shared-o0-o2-comparison-required`, `o2-core-only`, `o2-extension-only`, or `fallback-required`; success does not promote optional scope. Complete O2 covers accepted MetaKernel, ReSBM, FHEFusion, HPOLY, and HPAO-MU/FM/LM. HPAO-MD stays `design-pending, implementation-disabled` without separate approval; O3 and research backlog remain outside the claim.

## 3. Implementation Dependencies, Build Dependencies, and Pass Order

### 3.1 Implementation and build dependency graph

The implementation/build dependency is:

    accepted PRE-O2 lock and current O0 qualification
      -> accepted E1-E5C services and E6 Stage 1 release
      -> independently accepted E7A and E7B handoffs
      -> E8 handoff readers and cross-reanalysis controller
      -> legal profile and interaction matrix implementation
      -> complete invalidation, rebuild, and recost verification
      -> deterministic clean-build and replay harness
      -> S2.5 factorial ablation evidence
      -> S2.6 final provider, result, security, and performance evidence
      -> release decision and immutable O3 handoff

This is reviewed build dependency, not pass order. Proposed, not existing, paths are `osprey/be/vho/fhe_o2_final_{profile,reanalysis,verify}.{h,cxx}`, the Section 10 runners, and manifests under `testdata/fhe_o2/{acceptance-final,ablations,o3-handoff}/`.

### 3.2 Build and link impact

- E8 reuses accepted E2-E7 services for orchestration/evidence and does not fork semantic or algorithm code.
- Provider-free checks read immutable capability/cost manifests; executable checks use accepted E5C ACE rtlib/ANT.
- `be.so` gains no ACE rtlib, ANT, AIR, private ACE, frontend-builder, or utility dependency.
- Generated C uses only the accepted public C header and symbol allowlist.
- Record extensions require common/com version, old/new/feature-absent/corruption/reopen review before resuming.
- Build flags, tool versions, library hashes, and known `be.so` consumers are captured before/after.

The build gate audits defined/undefined symbols and link closure for `be.so`, `lw_inline`, plugins, and standalone consumers; any new `be.so` rtlib symbol stops for design/build approval.

### 3.3 Compiler pass order

The accepted compiler schedule is:

    fixed-N application.fhe.B -> independent reopen -> all-PU canonicalization
      -> optional accepted FHEFusion -> re-canonicalize and verify high-level graph
      -> frozen Fhelipe layout or selected MetaKernel layout
      -> discard provisional state -> recompute canonical pre-ReSBM CKKS state
      -> import baseline DP actions and protected-site provenance
      -> optional ReSBM replacement of replaceable automatic sites
      -> verify post-ReSBM state -> finalize Q/P/CRT/security/profiles
      -> resolve logical and ANT-expanded keys -> exact ANT projection
      -> optional HPOLY/HPAO on legal non-bootstrap regions
      -> final verifier -> standard WHIRL -> whirl2c -> generated C
      -> compile/link/load/run ACE rtlib/ANT -> decode and compare

FHEFusion never consumes HPOLY and HPAO never drives layout/bootstrap placement. A requested packing, CKKS, approximation, or protected-site change rejects composition and returns to its O2 owner.

### 3.4 Dependency separation rule

A built harness or provider-free replay cannot accept a profile while provider evidence is missing. E7A/E7B acceptance proves separate inputs only; the E8 result requires a complete pass-order rebuild.

## 4. Interfaces, Manifests, Records, and Versioning

### 4.1 Sources of truth

| Input or result | Authoritative owner | E8 use | Mutation rule |
| --- | --- | --- | --- |
| O0 qualification and ancestry | O0/E0 owner | Baseline, fallback, comparison root | Read-only and transitive |
| `FHECompilationConfigIR` | O0/common-com | Fixed N, security, policy, precision | Read-only; N never changes |
| Canonical source graph | P0/E6 owner | Root of every profile | Original immutable |
| E7A catalog and decision | E7A owner | Apply accepted pre-layout semantics | No silent catalog rewrite |
| Layout/slot selection | E4 owner | Rebuilt after graph changes | One selected authoritative layout |
| CKKS state and actions | E5A/E5B owner | Recomputed and verified | No candidate state as final truth |
| Parameters, keys, ANT projection | E5C owner | Rebuilt after upstream changes | Exact one-way checked projection |
| E7B carrier/rules/decision | E7B owner | Apply post-CKKS legal lowering | No second CKKS truth |
| Profile matrix | F0/E8 integration owner | Enumerate required runs | Immutable during campaign |
| Cost/performance protocol | E1/performance owner | Recost and measurement | Hash-bound before samples |
| Final release decision | E8 release board | User-visible status | Signed after all gates only |
| O3 handoff | E8 producer, O3 consumer verifier | Semantic input to future O3 | O3 read-only for semantic fields |

### 4.2 Final profile manifest

Each `O2FinalProfile` records stable ID/version, O0 ancestor, source/options, FHEFusion rules, layout, baseline DP, ReSBM actions, parameters, fixed N, logical/provider keys, HPOLY carrier, HPAO rules, provider/capability/cost hashes, epoch, materialized result, fallback, scopes, diagnostics, protocol, and fingerprint.

The campaign manifest is immutable; pointer/filesystem/hash/allocation/process/time/locale/PU/thread order is excluded from semantic identity.

### 4.3 Invalidation and reanalysis manifest

Each epoch records producer/rule, before/after digests, changed IDs, required/actual invalidation, rebuild order and owner/API, new IDs, verifier, cost delta, and acknowledgements. Consumers prove epoch equality; copying an old fingerprint cannot revive invalid data, and any missed field rejects despite good decoded output.

### 4.4 Provider and result evidence manifest

Each executable result binds Open64 source/config/plan; rtlib source/header/build/library; `LIB_ANT`; target/environment; C includes/symbols; exact `CKKS_PARAMS` and runtime Q/P/CRT/scales/levels/profiles/capabilities; key manifests; lifecycle/server census/cleanup; inputs; decoded/oracle outputs; samples/metrics/logs. Ciphertext and generated secret-key bytes are excluded from determinism; no secret material is retained.

### 4.5 O3 handoff contract

The proposed `O2O3SemanticHandoff` contains:

- schema, capability, producer, stage, campaign, and handoff IDs;
- accepted master/ADR/O2-plan versions, commits, and hashes;
- strict O0 qualification ancestry and E6/E7A/E7B exit fingerprints;
- source, canonical graph, selected profile, option, and support-matrix hashes;
- requested configuration, unchanged fixed N, trust/key domain, and security intent;
- finalized logical layout, shards, slot maps/classes, masks, and lineage;
- iteration spaces, affine bounds where present, call/edge IDs, and frequencies;
- selected baseline-DP/ReSBM actions and protected/manual/pre-ReLU boundaries;
- final CKKS parameter, value-state, transition, range, error, and profile IDs;
- HPOLY representation decision and state/action/trace identities;
- effect, alias, lifetime, control, dependence, and barrier facts;
- logical keys, ANT-expanded keys, transformed-plaintext, and package/cache IDs;
- ACE rtlib/ANT capability, target, cost, and environment fingerprints;
- final standard-WHIRL and generated-C result IDs and evidence hashes;
- known unsupported rows, authorized deferrals, residual risks, and owners;
- required O3 semantic-preservation checks and forbidden-change mask; and
- full artifact index, `SHA256SUMS`, producer signature, and consumer receipt.

The handoff contains IDs and fingerprints, never pointers, runtime objects,
secret keys, decryptors, or a private ACE compiler image.

### 4.6 Proposed semantic APIs

These are proposed interfaces, not accepted declarations:

    LoadAcceptedO2StageHandoffs(E6View, E7AView, E7BView, FinalIntegrationBuilder, DiagnosticSink)
    BuildO2FinalProfileMatrix(AuthorityView, SupportView, ProfileMatrixBuilder, DiagnosticSink)
    InvalidateAndReplanO2Profile(CanonicalGraphView, FinalProfileView, ReanalysisBuilder, DiagnosticSink)
    VerifyO2FinalProfile(FinalProfileView, FinalPlanView, EvidenceView, DiagnosticSink)
    BuildO2O3SemanticHandoff(FinalPlanView, EvidenceView, O3HandoffBuilder, DiagnosticSink)
    VerifyO2O3SemanticHandoff(O3HandoffView, O3ConsumerMode, DiagnosticSink)

Inputs are immutable. Failure returns no valid selected profile, release decision,
or handoff ID. Builders publish only after all-PU and cross-record verification.

### 4.7 Serialization and versioning

E8 reuses E2/P2-approved mapped-image storage. It does not allocate a new `WT_*`,
ELF section, table, opcode, or physical carrier without a separate common/com
review. Evidence-only JSON manifests do not replace authoritative WHIRL records.

Persisted records use fixed-width fields, invalid-zero IDs, checked first/count
ranges, explicit schema/algorithm/capability versions, deterministic ordering,
no pointers, no host-sized enums, no nested STL, and complete fingerprints.

Required compatibility includes old feature-absent WHIRL, Stage 1-only WHIRL,
E7A-only and E7B-only evidence, transient and persisted HPOLY decisions as
accepted, unknown-version rejection, every header/count/range/ID/hash corruption,
two clean writes, producer exit, independent reopen, and stable `ir_b2a -st -src`.

## 5. Cross-Pass Interaction and Reanalysis Contract

### 5.1 Required interaction matrix

| Change or selected action | Must invalidate | Required rebuild before use | Must remain unchanged |
| --- | --- | --- | --- |
| FHEFusion commits high-level graph | Canonical derived facts, layout/census, plaintext transforms, all CKKS states/actions, DP sites, ReSBM, parameters, keys, HPOLY/HPAO, provider projection, materialization, costs | Recanonicalize, E4, E5A, baseline DP, E5B, E5C, then E7B | Source intent, fixed N, protected policy |
| FHEFusion off/fallback | Any staged fused epoch | Reopen exact E6 graph and rebuild or reuse only hash-identical accepted E6 results | Complete E6 and O0 ancestry |
| Layout selection changes | Slot census, transforms, pre/post state, graph-derived DP sites, ReSBM, parameters, keys, HPOLY/HPAO inputs, provider result, costs | Slot oracle, E5A, DP/E5B, E5C, E7B | Fixed N and source semantics |
| Protected policy/site input changes | Placement, state, parameters, keys, materialization, costs, all downstream acceptance | Targeted upstream requalification, then complete affected profile replay | Manual/protected identities under accepted policy |
| ReSBM selection changes | Post-state, final parameters, keys, HPOLY/HPAO inputs, provider projection, materialization, costs | E5B verifier, E5C, then E7B | Layout and protected sites |
| CKKS parameter/profile changes | State proof, keys, HPAO packages, provider projection, materialization, costs | Full state/security/key/provider verification and E7B recost | Requested N and security intent |
| HPAO-MU selection changes | HPOLY value numbering/placement, materialization, operation counts, costs | E7B semantic/effect verifier, keys if affected, materializer, recost | CKKS and bootstrap semantics |
| HPAO-FM selection changes | Static packages/cache, provider keys if affected, materialization, bytes, costs | Package fingerprint, ordinary-reference proof, materializer, recost | CKKS state and fixed N |
| HPAO-LM selection changes | Reduction schedule, materialization, modular counts, costs | Word-safety proof, modular oracle, materializer, recost | CKKS state and bootstrap boundaries |
| HPAO requests semantic CKKS/layout change | Entire composition | Reject and return to owning O2 planner; no E8 repair | Accepted ownership boundary |
| ANT capability or cost changes | Provider projection, packages, keys, every profitability decision and performance result | P1-targeted requalification, reselect and replay every affected profile | Open64 semantic plan until reaccepted |
| Target/environment changes | Cost and performance evidence; target-sensitive capability evidence | Recalibration, protocol update before samples, complete target campaign | Semantic results if fingerprints prove target independent |
| Record/reader/writer changes | Reopen, compatibility, every dependent plan and handoff | Common/com requalification and complete deterministic replay | Nothing inferred from old bytes |

The actual invalidation set must equal or conservatively contain the required
set. Over-invalidation is reported as engineering cost; under-invalidation is a
hard correctness failure.

### 5.2 Required profile and ablation matrix

The accepted F0 matrix enumerates, at minimum:

- `BASE-O0` with strict qualified ancestry;
- `S1-MKR`, `S1-RESBM`, and `S1-FULL`;
- `LAYOUT-FHELIPE` and `LAYOUT-MKR` under the same baseline DP policy;
- `S2-CF`, `S2-MF`, and `S2-SF` separately;
- every accepted legal FHEFusion family pair and the full accepted fusion catalog;
- `S2-HPAO-MU`, `S2-HPAO-FM`, and `S2-HPAO-LM` separately;
- every F0-required legal HPAO family pair and accepted MU/FM/LM combination;
- every F0-required legal FHEFusion by HPAO pair;
- Stage 1 plus each Stage 2 family;
- `O2-ALL`, `O2-ADVANCED-OFF`, and bare `-O2`;
- each algorithm-off control and every authorized whole-profile fallback; and
- `O3-HANDOFF` with all O3 scheduling and memory controls disabled.

`S2-HPAO-MD` is recorded as `design-pending, implementation-disabled` unless its
separate design and amendment are accepted before protocol freeze. It is not a
fabricated runnable cell. Any other unavailable required legal cell makes the
campaign `Unverified` or `Rejected`; it cannot be hidden in an aggregate average.

Every matrix row has fixed-resolved and auto-remaining families. Both start from
the same requested fixed N. Auto-remaining may resolve active slots, Q/P, scales,
bootstrap profiles, keys, and achieved security; it may not select a new N.

### 5.3 Composition discipline

E8 never copies E7A's rebuilt Stage 1 result and E7B's E6-based HPOLY result into
one plan. It replays accepted E7A semantics on the canonical E6 root, rebuilds
E4-E5C completely, then applies accepted E7B semantics to that rebuilt final
CKKS input. All E7B legality, basis, effect, package, and word-safety proofs are
recomputed for the post-fusion input.

After each graph-changing action, all affected plans and costs are invalidated
before selection continues. Cost is computed from the actual composed graph,
not by adding paper speedups or separately measured E7A/E7B ratios.

### 5.4 Bare O2 and advanced-off mapping

The accepted option truth table maps bare `-O2` to exactly one immutable profile
ID and full fingerprint. Repeating the command with explicit equivalent controls
must produce the same normalized semantic plan and materialized WHIRL.

`O2-ADVANCED-OFF` disables only accepted O2 increments. It reproduces the complete
qualified O0 Fhelipe layout/lowering/state/rescale/protected-DP plan under the
accepted mapping. It may not fall back to a stripped layout, greedy substitute,
or O2-created hybrid.

### 5.5 Conflict and failure policy

An illegal interaction rejects the candidate composition and preserves the last
complete accepted ancestor. A profile-level failure never publishes a mixture of
new graph nodes and stale layout, placement, state, key, HPAO, or provider data.
An all-on failure is not silently converted into bare-O2 acceptance; the option
mapping or selected default must be explicitly reviewed and the campaign rerun.

## 6. Clean Rebuild, Deterministic Replay, and Final Plan Proof

### 6.1 Required clean-rebuild sequence

Two isolated build roots, `build-e8-a` and `build-e8-b`, are created from the same
accepted source commit, submodule/source locks, toolchain image, build manifest,
and environment. Neither may reuse object files, generated tables, planner caches,
transformed plaintexts, HPAO packages, plan images, or runtime state from the other.

Each build performs this sequence:

1. verify authority, source, fixture, toolchain, and provider hashes;
2. build Open64 and every relevant shared consumer from a clean root;
3. build or install the pinned `FHErt_common` and `FHErt_ant` artifacts;
4. compile each required profile from the same source `.B` and configuration;
5. exit the producer and destroy producer memory;
6. reopen the selected plan in an independent materializer process;
7. materialize standard WHIRL, emit same-stem `.T`, and audit the final boundary;
8. compile, link, load, and run generated C through `LIB_ANT`;
9. decode in the isolated client/test process and compare with the clear oracle;
10. emit normalized plans, reports, result manifests, and `SHA256SUMS`;
11. reopen the O3 handoff in a separate consumer process with O3 controls off; and
12. compare the two roots under the determinism rules.

### 6.2 Deterministic fields

The two roots must match byte-for-byte or by an explicitly normalized contract
for canonical graph, selected profile ID, layout/slot maps, placement, CKKS
parameters and states, actions, logical keys, ANT-expanded key requirements,
transformed-plaintext and HPAO package content hashes, standard WHIRL, generated-C
semantic form, diagnostics, cost inputs, decision logs, O3 handoff, and artifact
index. Timestamps, absolute build-root paths, randomized ciphertext bytes, and
ephemeral test keys are normalized or excluded with a reviewed reason.

A normalized match without binary-WHIRL reopen is insufficient. At least the
final selected plan, materialized standard WHIRL, and O3 handoff input cross the
process boundary and are verified independently in both clean roots.

### 6.3 Final semantic and runtime proof

Every selected profile proves source-to-clear tensor semantics, slot classes,
range/error bounds, CKKS state transitions, immediate Relin, protected sites,
logical/provider keys, HPOLY/HPAO legality, provider projection, generated-C
effects/lifetimes/status, decoded output, and predicted bounds. Ciphertext byte
equality is never used as an oracle.

The locked complete ResNet-20 run is mandatory. Focused-only success cannot
close E8. A timeout, missing sample, provider failure, or unavailable source-aware
dump is evidence of failure or `Unverified`, not permission to narrow the suite.

## 7. Correctness, Compatibility, Security, and Performance Reporting

### 7.1 Correctness report

The report gives each profile's exact input and output hashes; support row;
clear and decrypted tolerances; NaN/Inf status; maximum absolute and relative
errors; required-zero error; predicted-bound compliance; top-1 result and tie
rule; slot-map result; state/action result; protected-site result; immediate-Relin
result; oracle identity; and first diagnostic for each negative.

Pure double layout/kernel checks use `abs<=1e-12` and `rel<=1e-12`. Focused
encrypted checks use `abs<=1e-4` and `rel<=1e-6`. Required-zero slots use
`abs<=1e-8`. Application logits use maximum absolute error `<=1e-3`, maximum
relative error `<=1e-4` for reference magnitude at least `1e-2`, and identical
top-1 with lowest class index breaking ties. NaN or Inf always fails.

### 7.2 Compatibility report

The report covers legacy and feature-absent WHIRL, E6-only input, E7A-only and
E7B-only records, accepted HPOLY carrier mode, all supported readers, unknown
versions, corrupt headers/counts/ranges/IDs/hashes, two clean serializations,
producer-free reopen, stable logical `ir_b2a -st -src`, standard final WHIRL,
driver option propagation, advanced-off identity, and public rtlib ABI drift.

It names any binary or API version change, migration path, affected reader,
fallback, and retained golden. Compatibility cannot be declared from an in-memory
round trip or from a single current reader.

### 7.3 Security and provider report

The report binds fixed N; security estimator/version/inputs/result; Q/P/CRT and
bootstrap profiles; client/test and server-evaluation process inventories;
logical and ANT-expanded keys; generated-C includes and symbols; runtime library
hashes; ownership, allocation, failure, and cleanup behavior; sanitizer result;
secret scan; and `be.so`/consumer symbol audit.

The server process and its artifacts must contain no secret key, decryptor, test
decryption callback, secret-derived private material, or embedded client state.
Test decryption happens only in the isolated client/test process with ephemeral
keys. Any secret finding rejects and quarantines the affected evidence.

### 7.4 Cost-model and performance report

Costs use nonnegative integer nanoseconds and checked sums. Each predicted
component maps to independently counted rotations, adds, MulCP, MulCC, Relin,
rescale, mod-switch, bootstrap profile/result level, bytes, keys, ciphertexts,
regions, or transitions. Unknown frequency or saturation disqualifies a
profitability claim.

For operations with median at least 1 microsecond, holdout MAPE is at most 20
percent and P95 APE at most 35 percent. Smaller operations use absolute prediction
error at most 2 microseconds. The wrong-order rate is at most 10 percent under the
governing separation rule, and component explanations must be correct.

Focused tests use five warmups and 30 paired measured samples. Full applications
use one warmup and five paired samples. Pair order alternates AB/BA from the locked
seed. No outlier is deleted. Timeouts are failed samples. Reports retain raw data,
paired medians, MAD, 10,000 fixed-seed paired-resample 95 percent intervals, and
all mandatory static and runtime metrics.

Final non-regression requires the upper 95 percent interval bound for
`O2-ALL / BASE-O0` to be below `1.05` on every mandatory workload and the
geometric mean of workload median ratios to be at most `1.00`. Every required
ablation is reported separately; aggregation cannot hide a failing workload.

### 7.5 Unresolved-risk disposition

| Risk class | Required disposition at decision | Release effect |
| --- | --- | --- |
| Authority, ancestry, or owner unresolved | Close through accepted upstream decision | `Unverified`; no release |
| Correctness, state, boundary, or atomicity defect | Correct and rerun affected ancestry | `Rejected` |
| Provider unavailable or required environment missing | Record exact missing evidence and owner | `Unverified` |
| Security, secret, private ABI, or unauthorized dependency issue | Reject, quarantine, and review | `Rejected` |
| Performance hard gate fails | Retain raw evidence and reject default claim | `Rejected` |
| Cost model required but unusable | Recalibrate before protocol freeze and rerun | `Unverified` or `Rejected` by cause |
| HPAO-MD design pending | Record disabled capability and owner | Authorized deferral only |
| O3 parallel/data optimization | Transfer through handoff only | Outside O2 release |
| Lazy Relin or broader ReSBM/Conv research | Record governing backlog and disabled capability | Outside accepted support |
| Bootstrap-internal HPOLY/GPU work | Record separate design/provider gate | Outside accepted support |

Every risk has a stable ID, severity, owner, evidence, affected profiles, release
classification, next decision gate, and explicit accepted/deferred/rejected state.
There is no silent or ownerless residual-risk bucket.

## 8. Detailed Work Breakdown

| Task | Concrete change | Component/dependency | Test or oracle | Owner/reviewer | Merge and exit rule |
| --- | --- | --- | --- | --- | --- |
| `E8-W01` | Freeze authority and completion wording | Governance | Authority hash audit | Architecture/release | No pending required authority |
| `E8-W02` | Import strict O0 ancestry and E6 exit | Entry loader | Ancestry chain test | E0/E6/reliability | Exact current fingerprints |
| `E8-W03` | Import and reopen E7A handoff | Handoff reader | E7A consumer dry run | E7A/E8 | No inferred field |
| `E8-W04` | Import and reopen E7B handoff | Handoff reader | E7B consumer dry run | E7B/E8 | Carrier decision exact |
| `E8-W05` | Freeze legal profile matrix | S2.5/F0 | Completeness and duplicate audit | Integration/architecture | Every required cell owned |
| `E8-W06` | Freeze bare-O2 and off mappings | Driver/options | Truth-table enumeration | Driver/O0/release | Exact profile bijection |
| `E8-W07` | Implement immutable profile records | E2/common-com | Version/corruption/reopen | Record/compatibility | One owner per field |
| `E8-W08` | Implement invalidation manifest | Reanalysis controller | Under/over-invalidation matrix | Integration/all passes | No stale result accepted |
| `E8-W09` | Compose E7A before layout | Pass orchestration | Pass-order negatives | E7A/canonicalization | No HPOLY input to fusion |
| `E8-W10` | Recanonicalize composed graph | P0 service | Idempotence and slot oracle | Canonicalization/tensor | All-PU exact |
| `E8-W11` | Rebuild layout and census | E4 service | Fhelipe/MetaKernel oracle | E4/integration | Post-fusion graph only |
| `E8-W12` | Rebuild canonical pre-state | E5A service | State/epoch verifier | CKKS/crypto | Provisional state discarded |
| `E8-W13` | Regenerate DP and protected sites | O0 policy service | Identity and boundary oracle | O0/ReSBM | Protected identities unchanged |
| `E8-W14` | Rebuild ReSBM selection | E5B service | Region/cut/action oracle | ReSBM/independent | Replaceable sites only |
| `E8-W15` | Rebuild final parameters and state | E5C service | Q/P/CRT/security oracle | CKKS/security | Fixed N unchanged |
| `E8-W16` | Rebuild logical and ANT keys | E5C service | Separate key census | Key/runtime | Exact sets and provenance |
| `E8-W17` | Apply E7B to rebuilt CKKS input | HPOLY/HPAO | Basis/effect/modular oracle | E7B/E8 | Bootstrap opaque |
| `E8-W18` | Rebuild packages and provider projection | E7B/E5C | Cache/projection mismatch | Runtime/E7B | No stale package or key |
| `E8-W19` | Materialize standard WHIRL | Materializer | Independent reopen | E5C/backend | No private final op |
| `E8-W20` | Execute full provider/result path | ACE rtlib/ANT | Clear/decrypted oracle | Runtime/test | Public ABI and numeric gates |
| `E8-W21` | Execute single-factor cells | S2.5 | Profile semantic comparison | Pass owners/independent | Every required row passes |
| `E8-W22` | Execute every legal-pair cell | S2.5 | Interaction oracle | Integration/all passes | No missing legal pair |
| `E8-W23` | Execute all-on and bare O2 | S2.5/S2.6 | Plan/result identity | Release/driver | Same accepted profile |
| `E8-W24` | Execute advanced-off and fallback | Compatibility | Qualified O0 digest comparison | O0/reliability | Complete baseline only |
| `E8-W25` | Validate cost model and recosting | Cost/performance | Holdout/component checks | Cost/independent | Quantitative gates pass |
| `E8-W26` | Run paired performance campaign | S2.6 | Pre-registered statistics | Performance/independent | No hidden regression |
| `E8-W27` | Run two clean builds and replay | Reproducibility | Root A/B comparison | Build/reliability | Deterministic fields exact |
| `E8-W28` | Audit records and legacy compatibility | common-com/readers | Old/new/corruption matrix | Compatibility/record | Independent reopen exact |
| `E8-W29` | Audit Relin and final boundary | State/generated C | Action/symbol scan | CKKS/backend | One adjacent Relin per MulCC |
| `E8-W30` | Audit public ABI and `be.so` closure | Build/link | Include/symbol/library report | Build/security | No unauthorized dependency |
| `E8-W31` | Audit lifecycle and secrets | Runtime/security | Process census and secret scan | Security/independent | No server secret/decryptor |
| `E8-W32` | Close claim and risk ledger | Release evidence | Traceability/risk audit | Release/all reviewers | No ownerless/open blocker |
| `E8-W33` | Build versioned O3 handoff | Handoff producer | Schema and forbidden-field test | E8/O3 | Semantic fields complete |
| `E8-W34` | Reopen O3 handoff independently | O3 consumer | Producer-free verification | O3/compatibility | No producer memory |
| `E8-W35` | Prove O3-off identity | O3 consumer mode | Input/output digest comparison | O3/E8 | No O3 semantic mutation |
| `E8-W36` | Publish evidence and decision | Atomic release transaction | Bundle/hash/signature audit | Release/evidence | No algorithm change |

## 9. Proposed Commit and PR Sequence

Commits map to governing groups `S2-6` and `S2-7`:

1. `e8-entry-profile-contract`: entry fingerprints, strict ancestry, legal matrix,
   bare-O2 mapping, diagnostics, and failing contract tests.
2. `e8-record-and-handoff-contract`: reviewed profile, reanalysis, result, and O3
   handoff schemas with legacy, feature-absent, reopen, and corruption tests.
3. `e8-invalidation-controller`: epoch tracking and exact cross-pass invalidation.
4. `e8-fusion-stage1-rebuild`: E7A composition, recanonicalization, and E4-E5C replay.
5. `e8-hpao-composition`: E7B application to the rebuilt final CKKS plan.
6. `e8-provider-result-proof`: final materialization, public ANT projection, and
   decoded-result manifest.
7. `e8-factorial-single-pass`: required baseline and single-factor cells.
8. `e8-factorial-legal-pairs`: every F0-defined legal pair and unavailable-row audit.
9. `e8-all-on-default-off`: `O2-ALL`, bare `-O2`, advanced-off, and fallback identity.
10. `e8-cost-performance`: recosting, holdout validation, metrics, and paired protocol.
11. `e8-clean-replay`: two isolated build roots and deterministic consumer replay.
12. `e8-compatibility-failure`: legacy, feature-absent, corruption, stale, and
    failure-injection matrices.
13. `e8-security-boundary`: Relin, public ABI, symbols, `be.so`, lifecycle, cleanup,
    and secret audits.
14. `e8-o3-handoff`: handoff producer, independent verifier, forbidden-change mask,
    and O3-off identity test only.
15. `e8-final-acceptance-evidence`: runners, artifact index, risk ledger, decision
    template, and reviewer signatures only.

Each commit has one semantic goal and is independently buildable. Contract tests
land with or before implementation. The O3 commit contains no scheduling or
memory optimization. The final evidence commit contains no algorithm, option,
schema, or threshold change. Generated campaign artifacts are not committed
unless explicitly designated as goldens.

## 10. Proposed Verification Matrix

All commands below are proposed future exact interfaces. They do not claim that
the scripts, fixtures, targets, or outputs already exist. This planning task does
not execute them.

| Test ID | Proposed exact command | Inputs and bounds | Exact pass rule | Retained evidence |
| --- | --- | --- | --- | --- |
| `O2-S25-001` | `python3 osprey/be/vho/tests/run_o2_factorial_ablations.py --build-dir build --manifest testdata/fhe_o2/ablations/SHA256SUMS --protocol testdata/fhe_o2/ablations/protocol.json --artifacts test-artifacts/o2/O2-S25-001` | F0 legal profile matrix; all accepted E6/E7A/E7B hashes; locked focused and full workloads; fixed/auto-remaining | Every required single, legal pair, Stage 1, all-on, off, and fallback cell exists; semantics, invalidation, recosting, N, and diagnostics exact | Matrix, plan deltas, result manifests, raw samples, unavailable reasons, matching `.B`/`.T` |
| `O2-S26-001` | `python3 osprey/be/vho/tests/run_o2_final_acceptance.py --build-dir build --manifest testdata/fhe_o2/acceptance-final/SHA256SUMS --protocol testdata/fhe_o2/acceptance-final/protocol.json --artifacts test-artifacts/o2/O2-S26-001` | Accepted O0/Stage 1/Stage 2 evidence; bare O2; fixed and auto-remaining with same N; complete ResNet-20 | All governing hard, numeric, security, provider, model, performance, deterministic, and evidence gates pass; bare O2 exact | Signed release bundle, decision, reports, matching `.B`/`.T`, C, outputs, hashes |
| `O2-E8-001` | `python3 osprey/be/vho/tests/run_e8_cross_reanalysis_gate.py --build-dir build --e7a-handoff test-artifacts/o2/O2-E7A-EXIT/e8-handoff.json --e7b-handoff test-artifacts/o2/O2-E7B-EXIT/e8-handoff.json --matrix testdata/fhe_o2/ablations/profile-matrix.json --artifacts test-artifacts/o2/O2-E8-001` | Every interaction in Section 5.1; 1..8 PUs, <=2000 ops; all accepted family combinations | Required and actual invalidation sets, epochs, rebuild order, plan IDs, costs, and first diagnostics exact; no stale consumer | Handoff reopen, invalidation matrix, rebuilt profile `.B`/`.T`, state/key/cost reports |
| `O2-E8-002` | `python3 osprey/be/vho/tests/run_e8_clean_rebuild_replay.py --source-tree . --build-root-a test-artifacts/o2/O2-E8-002/build-e8-a --build-root-b test-artifacts/o2/O2-E8-002/build-e8-b --build-manifest testdata/fhe_o2/acceptance-final/build-manifest.json --protocol testdata/fhe_o2/acceptance-final/protocol.json --artifacts test-artifacts/o2/O2-E8-002` | Two empty isolated roots; identical locked source/toolchain/rtlib inputs; all required profiles; two producer and consumer processes | Builds succeed; normalized deterministic fields and binary semantic images match; independent reopen in each root; no cache leakage | Build logs, environment, root manifests, normalized diffs, `.B`/`.T`, C, handoffs, hashes |
| `O2-E8-003` | `python3 osprey/be/vho/tests/run_e8_provider_result_gate.py --build-dir build --manifest testdata/fhe_o2/acceptance-final/SHA256SUMS --provider LIB_ANT --mode full --artifacts test-artifacts/o2/O2-E8-003` | Focused plus complete ResNet-20; fixed/auto-remaining; accepted ACE rtlib/ANT build | Exact Q/P/CRT/profile and `Mul_ciph3 -> Relin`; public symbols; compile/link/load/run/cleanup; decoded gates and no server secret | Provider/result manifests, keys, process census, C, binary, logs, decoded/oracle outputs |
| `O2-E8-004` | `python3 osprey/be/vho/tests/run_e8_failure_compat_security.py --build-dir build --manifest testdata/fhe_o2/acceptance-final/SHA256SUMS --cases testdata/fhe_o2/acceptance-final/failure-cases.json --artifacts test-artifacts/o2/O2-E8-004` | Every stale/version/count/range/ID/hash, failure point, legacy image, provider mismatch, secret fixture, private symbol, duplicate-Relin case | Stable first diagnostic; no valid partial plan; legacy behavior preserved; security violations rejected/quarantined | Rejected inputs, diagnostics, fallback digests, compatibility, symbols, secret and dependency reports |
| `O2-E8-005` | `python3 osprey/be/vho/tests/run_e8_o3_handoff_gate.py --build-dir build --schema testdata/fhe_o2/o3-handoff/schema.json --manifest testdata/fhe_o2/o3-handoff/SHA256SUMS --mode producer-exit-consumer-reopen-off --artifacts test-artifacts/o2/O2-E8-005` | Final selected plan; all handoff fields; corruption/stale/unknown version; O3 controls off | Independent consumer validates all fields; off-mode semantic and standard-WHIRL digests equal; forbidden O3 changes reject | O3 input `.B`/`.T`, handoff, receipt, corruption cases, identity report, hashes |
| `O2-E8-006` | `ir_b2a -st -src test-artifacts/o2/O2-E8-005/o3-handoff-input.B test-artifacts/o2/O2-E8-005/o3-handoff-input.T` | Final O3 input binary and source preserved at recorded DST path | Same-stem source-aware dump exists; logical names only; no private FHE/O2/HPOLY operation | `o3-handoff-input.B`, `o3-handoff-input.T`, `o3-handoff-input.o3-ready.t` |
| `O2-E8-007` | `python3 osprey/be/vho/tests/run_e8_release_audit.py --bundle test-artifacts/o2/O2-S26-001 --traceability testdata/fhe_o2/acceptance-final/traceability.json --risk-ledger testdata/fhe_o2/acceptance-final/risk-ledger.json --artifacts test-artifacts/o2/O2-E8-007` | Full bundle, all claims/risks/tests/artifacts/reviewers | Every requirement and risk has current authority, owner, test, result, artifact, disposition, and signature; no placeholder | Claim ledger, risk ledger, traceability, reviewer report, decision digest |

Structural `O2-E8-001/004/005/007` cases use zero warmups, one run, and a
1200-second timeout. `O2-E8-002` permits up to three hours per clean root.
Provider-focused cases use five warmups and 30 paired samples with a 20-minute
timeout per sample. Full ResNet-20 uses one warmup and five paired samples with a
three-hour timeout per sample. No outlier is deleted.

If `ir_b2a -src` is unavailable, `O2-E8-006` and the stage are `Unverified`.
The command is not weakened to omit `-src`. Lower-case descriptive phase traces
avoid a case-insensitive collision with same-stem `.T` files.

## 11. Negative, Failure, Compatibility, and Security Matrix

| Case | Required behavior | Publication |
| --- | --- | --- |
| Entry decision or fingerprint not accepted/current | Stop and target requalification | None |
| E7A or E7B handoff missing/unreopenable | Mark `Unverified` | None |
| Strict O0 ancestry differs between profiles | Reject campaign | None |
| Required legal matrix cell missing | Reject or mark unavailable by cause; never hide | None |
| FHEFusion scheduled after layout/HPOLY | Reject pass order | None |
| HPAO scheduled before final CKKS plan | Reject pass order | None |
| Fusion commit leaves stale layout/state/key/cost | Reject profile | Prior complete ancestor only |
| Layout change reuses old DP site location | Reject profile | Prior complete ancestor only |
| ReSBM changes protected/manual/pre-ReLU site | Reject profile | Complete baseline only |
| HPAO changes CKKS/layout/approximation semantics | Reject composition | Rebuilt pre-HPAO plan only |
| Provisional state presented as final | Reject | None |
| Fixed N absent or changed | Reject before publication | None |
| Larger N needed | Diagnose explicit recompilation; do not mutate | Prior complete plan only |
| Non-immediate, missing, or duplicate Relin | Reject | None |
| Canonical MulCC maps to `Mul_ciph` plus Relin | Reject materialization | None |
| Bootstrap expanded inside HPOLY/HPAO | Reject | None |
| Unknown frequency or saturated cost | Disqualify profitability | No accepted cost claim |
| Stale cost/capability used for selection | Reject profile | Prior complete ancestor only |
| Bare O2 differs from accepted explicit profile | Reject release mapping | None |
| Advanced-off strips any O0 baseline pass or DP | Reject | None |
| Fallback mixes O2 graph with stale baseline plan | Reject | None |
| Runtime Q/P/CRT/profile differs from Open64 plan | Reject provider result | None |
| ACE rtlib revision/header/build drift | Stale provider evidence and requalify | None |
| Generated C uses private ACE header/symbol | Stop for interface review | None |
| Another release provider selected | Reject | None |
| `be.so` gains unauthorized rtlib dependency | Stop for build review | None |
| Server receives secret/decryptor | Reject and quarantine | None |
| Retained artifact contains secret material | Reject and quarantine | None |
| Cleanup, sanitizer, or lifetime failure | Reject executable profile | None |
| Unknown record/handoff version | Reject before use | None |
| Corrupt header/count/range/ID/hash | Reject with stable first diagnostic | None |
| Legacy or feature-absent WHIRL | Preserve accepted old behavior | No fabricated O2 record |
| Failure before atomic release commit | Remove staged valid output names | None |
| Producer dies after commit | Independent consumer validates completeness | Complete only |
| Clean roots differ in deterministic field | Reject reproducibility | Neither root accepted |
| Ciphertext bytes differ | Expected; compare decoded values and manifests | Not a failure alone |
| Source-aware same-stem `.T` unavailable | Mark `Unverified` | Not accepted |
| Private FHE/O2/HPOLY op reaches `whirl2c` | Reject final boundary | None |
| O3-off changes semantic plan or standard WHIRL | Reject handoff | None |
| O3 consumer requests logical semantic change | Return to O2 owner for new plan | Old handoff remains immutable |
| HPAO-MD has no accepted design/amendment | Keep disabled and report | Authorized deferral only |
| Required performance interval fails | Reject final default | Evidence retained |
| Missing full ResNet-20 result | Mark `Unverified` | Not accepted |

Authority/schema/hash failures precede semantic checks. Semantic and pass-order
failures precede capacity/security/provider checks. Profitability is evaluated
only for legal plans. The first diagnostic is stable across traversal and build
root. Failure details may be retained without giving a rejected image a valid
selected `.B` name.

## 12. Evidence Bundle and Retention

### 12.1 Required final bundle

The final bundle contains at least:

```text
authority-and-entry.json master-and-adr-lock.json strict-o0-ancestry.json
o0-qualification-reference.json e6-stage1-acceptance-reference.json
e7a-handoff.json e7b-handoff.json source-lock.json support-matrix.json
profile-matrix.json option-truth-table.json diagnostics.json environment.json
build-manifest.json requested-configuration.json provider-capabilities.json
ace-rtlib-ant-manifest.json input.fhe.B input.fhe.T
canonical.prefusion.B canonical.prefusion.T fusion-selected.B fusion-selected.T
cross-pass-invalidation.json reanalysis-epochs.json layout-and-slot-map.json
ckks-state-trace.json bootstrap-placement-and-protected-sites.json
resolved-parameters.json logical-key-requirements.json ant-key-manifest.json
hpoly-contract-and-trace.json hpao-rule-and-package-manifest.json
selected-plan.B selected-plan.T materialized.o2.mid.B materialized.o2.mid.T
generated.c generated-c-symbols.txt build-and-link.log
server-key-census.json secret-scan.json cleanup-and-sanitizer.json
decoded-output.json oracle-output.json provider-result-manifest.json
raw-samples.csv metrics.json cost-model-validation.json factorial-ablations.json
performance-report.json compatibility-report.json security-report.json
failure-and-corruption.json clean-rebuild-replay.json
claims-ledger.json risk-ledger.json traceability.json
o3-handoff-input.B o3-handoff-input.T o3-semantic-handoff.json
o3-consumer-receipt.json o3-off-identity.json release-decision.md
reviewer-signatures.json SHA256SUMS
```

Every meaningful `.B` above has a same-stem `.T` created with `ir_b2a -st -src`.
Profile-specific meaningful `.B` files emitted beneath the ablation directory
also require same-stem `.T`. A phase trace uses a lower-case descriptive suffix,
such as `materialized.o2.mid.vho.t`, rather than colliding with `.T`.

Artifacts are cleaned at the start of the next run, never at the end of the
current run. Failed development evidence is retained at least 30 days, milestone
evidence at least 180 days, and accepted release/O3 handoff evidence for repository
lifetime. `SHA256SUMS` contains no placeholder. Retained generated artifacts are
normally not committed unless explicitly designated as goldens.

### 12.2 Evidence completeness rules

Each claim row points to exact authority, source hash, profile, command, test ID,
raw result, summary, artifact path, reviewer, and decision. Each result manifest
contains both provider evidence and clear/decrypted result evidence. A pass/fail
summary without the underlying `.B`/`.T`, raw samples, plan, keys, and logs cannot
close a release claim.

The release bundle is atomically staged, verified by a separate process, signed,
and renamed to its accepted path. A failed signature, missing hash, or incomplete
consumer receipt leaves no accepted release directory.

## 13. Release Decision and Exit Gate

### 13.1 Accepted

`O2-E8-EXIT=Accepted` requires current entry gates; `O2-S25-001`, `O2-S26-001`, and `O2-E8-001` through `O2-E8-007`; all profiles; exact invalidation/pass-order rebuild; fixed N; correctness/bounds; exact sites/state/keys/Relin/provider/public ABI; full ResNet-20; cost/performance; compatibility; atomic failure; two clean replays; no secret/private ABI/unauthorized `be.so` dependency/private final op; closed risks; reopened O3 handoff and O3-off identity; and signatures.

There is no `Accepted with exceptions`. Authorized deferred HPAO-MD/O3 scope is named and cannot hide a failed required gate.

If and only if the accepted master/ADR authorizes complete-O2 wording, the exact statement is:

    O2-E8 accepted: all required O2 cross-pass replanning, ablation, correctness,
    compatibility, security, ACE rtlib/ANT, cost-model, performance, deterministic
    replay, and evidence gates passed. O2 complete under the accepted authority;
    HPAO-MD remains design-pending and disabled, and O3 physical parallel/data
    optimization remains outside this release.

It does not claim full HPAO-paper equivalence, lazy Relin, unsupported ReSBM/Conv, GPU/native bootstrap lowering, or O3 acceptance.

### 13.2 Rejected

`O2-E8-EXIT=Rejected` means a required executed contract failed. The decision names the first diagnostic, requirement/profile/test, evidence, last valid ancestor, owner, claims, rollback, and correction. No accepted O3 handoff or bare-`-O2` certification results.

### 13.3 Unverified

`O2-E8-EXIT=Unverified` means required authority, source, environment, provider, workload, test/sample, oracle, artifact, source-aware dump, reopen, or review is unavailable. It is not provisional acceptance, cannot be cured by narrowing/fallback, and cannot authorize O3.

## 14. Rollback, Invalidation, and Stop Rules

Profile rollback restores the last complete immutable ancestor: fusion to E6, reanalysis to its input epoch, HPAO to rebuilt final CKKS, and final campaign to E6 or an explicitly authorized complete qualified O0 profile. Rollback never mixes epochs; it removes staged plan/data/key/materialized/C/result/decision/handoff names. Rejected evidence may remain under rejected paths, and corrections rerun all affected rows.

Stop for architecture, common/com, build, security, or owning-pass review if:

- authority, decision, record, profile, risk, or claim ownership is missing/duplicated, or E6/E7A/E7B is missing/stale/unreopenable;
- strict O0 ancestry fails, a required profile is absent, pass contracts cannot compose, or pass order changes;
- stale data is undetectable, invalidation is incomplete, N must change, or security cannot be proven;
- protected/manual/pre-ReLU identity, immediate Relin, or the HPOLY/CKKS/bootstrap boundary cannot survive;
- a public/physical contract lacks review, or runtime/provider state differs from Open64 truth;
- generated C needs private ACE/another provider, or `be.so` gains unapproved rtlib symbols without consumer closure;
- server/evidence needs a secret/decryptor, `.B` cannot independently reopen with same-stem `.T`, or a private op reaches `whirl2c`;
- cost saturates, unknown frequency is free, thresholds change after observation, or aggregation/samples hide performance failure; or
- completion needs O3 implementation, expanded support, or a governing-invariant change.

## 15. Explicit O3 Handoff and Consumer Rules

### 15.1 Handoff purpose

The accepted O3 handoff is a frozen semantic scheduling input for mapping selected operations/data to physical resources without changing O2 choices. It follows final materialization/provider certification; `o3-semantic-handoff.json` and `o3-handoff-input.B`/same-stem `.T` bind the same plan.

### 15.2 O3 permitted work

Future O3 may plan physical scheduling, workers, separately supported NUMA/GPU memory/data/buffer placement, working sets, and transfer overlap only under its own design, cost, race, lifetime, determinism, security, performance, and acceptance plan.

### 15.3 O3 forbidden work

O3 may not rerun/extend DP/ReSBM; change logical packing/shards/slots/masks/approximation/polynomial identity/N/Q/P/CRT/scale/level/profile/placement/boundaries/Relin/HPOLY/keys/lineage/provider; introduce a server secret; or relabel O2 results. New physical schedules/placements use O3-owned records and never overwrite O2 truth.

### 15.4 Independent consumer procedure

The O3 consumer:

1. starts after the E8 producer exits and without producer memory;
2. validates all authority, ancestry, version, capability, source, config, target, plan, record, and artifact hashes;
3. reopens `o3-handoff-input.B` and validates same-stem `.T` evidence;
4. checks layout, iteration, state, action, placement, boundary, effect, dependence, key, provider, and risk fields against final O2;
5. rejects unknown, missing, duplicate, stale, corrupt, secret, or mutable fields;
6. runs with every O3 scheduling and memory/data control disabled;
7. proves normalized semantic-plan and standard-WHIRL identity in off mode; and
8. emits a signed immutable consumer receipt.

An O3 semantic change request returns to the owning O0/O2 stage for a new accepted plan and affected E8 rerun; O3 never patches or locally reaccepts the old handoff.

### 15.5 Handoff acceptance wording

The O3 receipt may state:

    O3 handoff accepted as an immutable O2 semantic input; O3 scheduling and
    memory/data controls are disabled and no O3 execution capability is certified.

It may not claim O3 implementation, optimization, execution, performance, or acceptance; those belong to a future O3 plan.

## 16. Requirement Traceability

| Requirement | Authority or milestone | E8 work | Proposed verification | Required evidence |
| --- | --- | --- | --- | --- |
| E6/E7A/E7B accepted first | Governing plan Section 12.4; `S2.5` | `W02-W04` | `O2-E8-001` | Exit decisions and handoff reopen |
| Strict O0 ancestry | User sequence; O2 Sections 0/13/15 | `W02/W24/W32` | `O2-S25-001`, `O2-E8-007` | Ancestry and baseline digest |
| Cross-pass invalidation/replanning | O2 `S2.5` | `W08-W18` | `O2-E8-001` | Epoch and invalidation matrix |
| Required ablation matrix | O2 `S2.5`, Section 15.1 | `W05/W21-W23` | `O2-S25-001` | Complete profile matrix and plan deltas |
| Bare O2 exact mapping | O2 `S2.6` | `W06/W23` | `O2-S26-001` | Option truth table and plan identity |
| Advanced-off preserves O0 | O2 Sections 0.5/15.1 | `W06/W24` | `O2-S25-001`, `O2-E8-004` | Qualified baseline digest |
| Fixed N and immediate Relin | Master/O2 invariant | `W12-W18/W29` | `O2-E8-001/003/004` | Config, state, action, C audits |
| Public ACE rtlib/ANT boundary | FRZ-08/09; `S2.6` | `W18/W20/W30` | `O2-E8-003/004` | Header/symbol/build/projection report |
| No unauthorized `be.so` dependency | `AGENTS.md` | `W30` | `O2-E8-004` | Defined/undefined/link-closure audit |
| No server secret/decryptor | Runtime/security decision | `W20/W31` | `O2-E8-003/004` | Process census and secret scan |
| Full provider/result evidence | O2 Sections 7/15 | `W20/W26` | `O2-E8-003`, `O2-S26-001` | Provider/result manifests and raw output |
| Compatibility and independent reopen | `AGENTS.md`; P2 | `W07/W27/W28` | `O2-E8-002/004/006` | Old/new images, `.B`/`.T`, replay |
| No private op at `whirl2c` | O2 hard gate | `W19/W29` | `O2-E8-003/006` | Final source-aware dump and audit |
| Cost and performance gates | O2 Sections 15.5/15.6 | `W25/W26` | `O2-S25-001`, `O2-S26-001` | Model validation, raw samples, CI |
| Claims and risks closed | O2 definition of done | `W32/W36` | `O2-E8-007` | Claim/risk ledgers and signatures |
| O3 handoff without O3 implementation | O2 Sections 0.6/17.2 | `W33-W35` | `O2-E8-005/006` | Handoff, receipt, off-mode identity |
| Exact release decision | O2 `S2.6` | `W36` | `O2-S26-001`, `O2-E8-007` | Signed `release-decision.md` |

Every accepted row closes the chain from requirement to accepted master/hash,
ADR/decision, governing O2 section and milestone, E8 task, exact proposed command,
retained artifact, result, and reviewer signature. A pending authority, unknown
owner, moving source, placeholder hash, stale qualification, missing required
profile, unavailable provider, or absent O3 consumer receipt prevents
`O2-E8-EXIT=Accepted`.
