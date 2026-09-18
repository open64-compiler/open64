# O2-E1 Detailed Plan: Claims, Support, Fixtures, Diagnostics, and Oracles

Status: proposed execution plan; inactive until `O2-O0Q-001=Qualified`

Engineering stage: `O2-E1`

Governing milestone: `S1.0`

Exit gate: `O2-E1-EXIT`

Plan date: 2026-09-15

## 1. Authority and Metadata

### 1.1 Authority order

This stage plan is subordinate to the following authorities, in order:

1. Explicit user decisions and the repository invariants in `AGENTS.md`.
2. The accepted DSC FHE compiler master plan and any accepted amendment.
3. Accepted architecture decision records within their stated scope.
4. `doc/FHE-O2-ACE-RESBM-METAKERNEL-INTEGRATION-PLAN.md`, version 1.6.
5. `doc/FHE-O2-DETAILED-EXECUTION-PLAN.md`, plan set version 0.2, for navigation.
6. This stage plan.
7. Locked papers and fixed source revisions as evidence within their stated domains.

The currently recorded master is
`doc/DSC_FHE_Compiler_Architecture_and_Integration_Plan_v0.10.md` at
`ee1dc6382246c58f49a3097157a8c4e8ff2440c8`, with SHA-256
`7EF644E9BECEFB541102514B83656C78756ECA71398A771A2A5160FC10B0CB25`.
Its DOCX companion has SHA-256
`0018769C26B5A0BCD1BDFCBD85AA97B8BAFEA381D7640FBB9E2E81B0022013D9`.
The successor master and
`doc/adr/FHE-O0-O2-PLANNER-OWNERSHIP.md` remain pending. This stage cannot be
activated until `PRE-O2-LOCK-EXIT` records the accepted replacement sections,
commit, hashes, and `FRZ-01..09` dispositions.

Writing or reviewing this planning document is governance work, not execution of
`O2-E1`. No source lock, fixture, oracle, cost model, test implementation, or
production branch described here may begin before the activation gate in
Section 2 passes.

### 1.2 Locked planning inputs

| Input | Locked identity | E1 use |
| --- | --- | --- |
| Open64 reviewed baseline | `develop@eca97d4843aef03c50e5cb866f2f0e6c82e6b064` | Record the implementation baseline from which E1 work starts. Revalidate the actual activation commit and delta. |
| ACE architectural evidence | `cgo2025-artifacts@15c95a7346d89355d68d5bf4fe8ab3b952740e1a` | Bound VECTOR, SIHE, and CKKS claim inventory. |
| MetaKernel production evidence | `origin/metakernel-proof@d0c14ab101c1f1fedfb31d8fdb553867ae7ce7be` | Authority for artifact-backed Stage 1 MetaKernel behavior in the accepted source domain. |
| MetaKernel paper | SHA-256 `E2AB5C3C89EBC7B5B6CEA7E79EAA00BC98975EF66EC69104EA458570DDBE2FEF` | Intent, teaching cases, and a paper-strict differential oracle, not a replacement for artifact behavior. |
| ReSBM production evidence | `origin/20250524@323e8bb02a0e036fe2369eb48badfe6692398c2b` | Authority for the conservative artifact-backed ReSBM core. |
| ReSBM paper | SHA-256 `C6D46834C37CF534D1D13050E7CB946962E3D7FD810539C347BE21EBEA6C3A00` | Intent and differential evidence outside the fixed artifact contract. |
| ACE rtlib and ANT | `../../ace-compiler@929e9b621f11bebbaa9ec1e215f4a52e3d07109b` | Fixed required runtime interface and provider capability input. |
| Qualified O0 baseline | Exact `O2-O0Q-001` qualification ID and fingerprint supplied at activation | Sole executable comparison baseline and sole unlock for E1. |

The E1 source lock shall include every transitive file, fixture, generator,
license, toolchain, and build input used to support a claim. A branch name,
remote URL, package range, or file path without a content hash is not a lock.

### 1.3 ACE rtlib public-header lock

The initial planning lock contains these reviewed ACE rtlib headers and hashes:

| File relative to `../../ace-compiler/` | SHA-256 |
| --- | --- |
| `fhe-cmplr/rtlib/include/common/rt_api.h` | `0025B6FB981F9578B15C9F05435DB0951E4B6C5E7547B398496F39CD0C5BC5C8` |
| `fhe-cmplr/rtlib/include/common/common.h` | `1DF878C051ADACCEABD3ECB57A88C5566E19F446AAE9C974FF9430FBD64E4AAB` |
| `fhe-cmplr/rtlib/include/rt_ant/rt_ant.h` | `AB4189F3970C2CEB80EC70018A31650E8ECC9C8102AB7BB55C3DB92A67E50C6E` |
| `fhe-cmplr/rtlib/include/rt_ant/ant_api.h` | `B9AE0016F701DE2FBE6D12BE59604DDF5C04B7666A90AFA44B8E1B671B20577A` |
| `fhe-cmplr/rtlib/ant/include/ckks/cipher.h` | `D5166E651AE61828841AFE8991A6AB0E73DAA36D161DE8A2C19344EBF4684469` |
| `fhe-cmplr/include/fhe/core/lib_provider.h` | `E1C3D250F62AC7E72FDB2E24E75C150412743D39FFBE745BC5144D9E26EC40EF` |

The generated-C contract uses only reviewed public declarations reachable from
`common/rtlib.h`, `common/rt_api.h`, `common/common.h`, `rt_ant/rt_ant.h`, and
`rt_ant/rt_api.h`. It links `FHErt_common` and `FHErt_ant` with provider
`LIB_ANT`. `LIB_ACE` is not an alias for ANT and is not an accepted release
provider for this plan.

### 1.4 Ownership and review roles

- E1 owner: bind qualification, coordinate the freeze, and publish the reviewed family.
- Source/claims/fixture/diagnostic/cost editors: own their respective immutable inputs, mappings, deterministic cases, codes, units, and provenance.
- Independent oracle owners: define raw interfaces and prove that no production helper is imported.
- Required reviewers: paper/source, common/com, CKKS/security, optimizer, ANT runtime, testing, and reproducibility.

Actual names and signoffs are required before exit. One person may hold editing roles, but the oracle and production algorithm for one claim cannot share an implementation helper or approving reviewer.

## 2. Activation Gate and Staleness

### 2.1 Mandatory activation gate

E1 starts only when all of the following are true:

1. `PRE-O2-LOCK-EXIT=Accepted` binds the accepted master, ADR, mature Fhelipe
   baseline, protected-DP policy, option truth table, record ownership, and ACE
   rtlib/ANT boundary.
2. `O2-O0Q-001=Qualified` covers the complete producer handoff
   `F1-PREP/P0/P1a/P1b/F1-IMPL/F1-ACCEPT`.
3. The qualification includes the complete focused and ResNet-20 executable
   path through generated C, `FHErt_common`, `FHErt_ant`, and `LIB_ANT`.
4. The E1 owner independently verifies the qualification signature, manifest
   hashes, environment, support rows, negative results, and retained evidence.
5. The qualification fingerprint is current at the E1 branch point.

`Rejected` returns to the named O0 producer. `Unverified` does not permit
fixture-only, source-only, or provider-free E1 work. E1 must not patch, narrow,
or substitute the qualified O0 baseline.

### 2.2 Qualification binding

The first E1 artifact, `e1-activation.json`, records the accepted master/ADR; qualification ID, decision hash, signers, and date; Open64/Fhelipe/ACE-rtlib/ANT/toolchain/fixture/environment fingerprints; baseline pass/config/support and record-schema hashes; fixed-request family hashes; retained evidence root and `SHA256SUMS`; and the E1 branch point and cleanliness statement.

### 2.3 Stale triggers

Any of these makes E1 `Unverified` and blocks later O2 work:

- accepted master/ADR text, qualification fingerprint, or a bound O0 manifest changes;
- a paper, source, license, fixture, generator, toolchain, public header, build flag, library, or symbol changes;
- provider ID, allowlist, lifecycle/key-separation decision, record schema, identity rule, fixed-`N` rule, immediate-Relin rule, protected-site policy, or fallback changes;
- a support row, diagnostic, tolerance, tie, cost unit, seed, or generator bound changes; or
- an oracle gains a production-helper dependency.

Targeted requalification must identify affected claims, rerun every dependent
verification row, produce a delta report, and issue a new E1 decision. A stale
E1 lock cannot be repaired by changing only `SHA256SUMS`.

## 3. Objectives, Capability, and Boundaries

### 3.1 Objective

Freeze an executable contract for every Stage 1 production claim before any
Stage 1 production algorithm is implemented. The contract must make each claim
bounded, independently testable, attributable to fixed evidence, and connected
to a stable diagnostic, support row, oracle, verification command, and retained
artifact family.

The user-visible result of E1 is no new optimization capability. Its value is a
reviewed and reproducible specification that prevents later implementation from
choosing its own evidence, support domain, tolerance, tie rule, or baseline
after observing results.

### 3.2 In scope

- Stage 1 claim catalog and authority classification.
- MetaKernel and ReSBM source locks and paper/artifact delta records.
- Stage 1 support matrix with positive, negative, and boundary domains.
- Fixed-resolved and auto-remaining fixture families that use the same fixed
  requested `N`.
- Stable diagnostics, payloads, precedence, and first-diagnostic expectations.
- Deterministic fixture generators with fixed seeds and finite bounds.
- Independent MetaKernel artifact oracle and paper-differential interfaces.
- Independent ReSBM raw-graph oracle interface and dependency denylist.
- Clear tensor, physical slot, CKKS state, protected-boundary, key, capability,
  and cost-model proof obligations needed by later stages.
- Integer cost units, raw-table provenance, calibration/holdout split, and
  checked saturation rules.
- Evidence schemas, hash rules, retention rules, and traceability.

### 3.3 Explicit non-goals

E1 shall not:

- implement or enable a production MetaKernel, ReSBM, CKKS resolver,
  materializer, FHEFusion, HPOLY, or HPAO pass;
- allocate a new WHIRL operator, opcode, ELF section, mapped-image table, or
  public driver option;
- implement the P2 record extension or S1.1 identity-plan transaction;
- generate an O2 selected plan, materialize O2 calls, or claim ANT execution;
- change or weaken the qualified mature Fhelipe baseline or protected DP;
- add a source-level or link dependency from `be.so` to ACE rtlib;
- import ACE AIR as a second physical IR universe;
- accept a new provider or substitute `LIB_ACE` for `LIB_ANT`;
- resolve `ANT-RT-TODO-CLIENT-SERVER-LIFECYCLE` by assumption;
- fit a cost model or relax a tolerance after seeing acceptance measurements;
- claim paper-exact behavior for an artifact-backed extension; or
- start P2, S1.1, or any production implementation before `O2-E1-EXIT`.

### 3.4 Inherited invariants

- Binary WHIRL is the process boundary. Meaningful WHIRL evidence retains a
  `.B` and same-stem `.T` generated by `ir_b2a -st -src input.B input.T`.
- Open64 owns CKKS-and-above semantic IR, planning, verification, and
  optimization. ACE rtlib/ANT receives a checked one-way execution projection.
- Requested `N` exists before canonicalization and never changes. Capacity or
  security failure reports a minimum acceptable or recommended `N` and requires
  explicit recompilation.
- Canonical multiplication is
  `MulCC(two,two) -> transient three components -> immediate Relin -> two
  components`.
- The ANT mapping is `Mul_ciph3` followed by adjacent `Relin`. Mapping to fused
  `Mul_ciph` and then emitting another `Relin` is invalid.
- Secret keys and decryptors never appear in server-evaluation artifacts,
  WHIRL, generated C, plans, manifests, or logs.
- Protected manual and policy-mandated sites are not removed, moved, merged, or
  reclassified by O2. ReSBM may replace only explicitly replaceable automatic
  baseline-DP sites.
- An unsupported O2 increment fails before partial publication or uses only an
  ADR-approved whole-profile fallback to the complete qualified O0 baseline.
- Ordinary WOPT must not move, duplicate, eliminate, speculate, or CSE ordered
  FHE state transitions or ACE rtlib calls.

## 4. Dependency Graphs

### 4.1 Implementation and build dependencies

```text
PRE-O2-LOCK-EXIT
  -> complete external O0 producer acceptance
  -> O2-O0Q-001=Qualified
  -> bind e1-activation.json
  -> freeze claims and authorities
  -> freeze support, diagnostics, fixtures, oracles, and cost inputs
  -> O2-S10-001
  -> O2-E1-EXIT
  -> P2 record extensions
  -> S1.1 identity-plan transaction
```

E1 is data, test-contract, and review work. Its planned verification utilities
may link only to already accepted repository test support. E1 adds no runtime
library dependency to `be.so` and does not call ANT. ACE rtlib/ANT revision,
headers, symbols, capabilities, and lifecycle mode are hashed requirements for
later stages, not E1 execution services.

If a future E1 verification utility needs an exact Q/P/CRT compiler query, stop
and use the accepted isolated resolver design or complete the explicit backend
dependency and link-closure review first. This stage does not authorize that
dependency.

### 4.2 Compiler pass-order relationship

E1 does not add or execute a compiler transformation. It freezes the contracts
that later passes must follow:

```text
canonical Open64 FHE graph
  -> optional MetaKernel selected layout
  -> canonical pre-ReSBM CKKS state
  -> qualified baseline-DP plan and protected-site provenance
  -> optional ReSBM replacement
  -> final state, parameters, security, capabilities, and keys
  -> standard WHIRL and generated C
  -> ACE rtlib/ANT execution
```

The implementation queue must not be inferred from this pass order. In
particular, E1 freezes MetaKernel and ReSBM oracle interfaces before their
production implementations, but it cannot claim plan materialization or runtime
execution.

### 4.3 Accepted upstream inputs

E1 consumes only immutable copies or content-addressed references to:

- the complete `O2-O0Q-001` qualification bundle;
- the accepted master, ADR, and `FRZ-01..09` decision ledger;
- qualified O0 config, canonicalization, layout, CKKS state, placement,
  protected-site, key, runtime capability, and support records;
- the exact qualified `BASE-O0` focused and ResNet-20 fixtures and outputs;
- locked research papers and source repositories; and
- a supported test host and toolchain identity for reproduction checks.

E1 must not depend on an in-memory producer object, a moving external branch, or
an O2 record/API that P2 has not accepted.

## 5. Claim and Support Contracts

### 5.1 Claim classes

Every claim shall use exactly one primary class:

| Class | Meaning | Required evidence |
| --- | --- | --- |
| `artifact-backed-production` | Later production behavior must match the fixed source artifact within its accepted domain. | Fixed source hash, source anchors, independent artifact oracle, support row, and exact structural comparison. |
| `paper-strict-differential` | The paper formula or algorithm is evaluated separately to explain a known source delta. | Fixed paper hash, independently implemented paper rule, named differential fixtures, and exact delta report. |
| `open64-extension` | Behavior not attributed to the paper or fixed artifact. | Explicit semantics, narrow support domain, exact oracle, capability bit, diagnostic, and independent review. |
| `o0-handoff-consumption` | O0 produces the foundation and O2 validates and consumes it without reimplementation. | Current qualification fingerprint, record owner, schema/hash, and consumer validation. |
| `runtime-projection-requirement` | A later Open64 semantic decision must project exactly to ACE rtlib/ANT. | Public-header/symbol lock, provider capability, field mapping, mismatch negative, and runtime query evidence in the owning later stage. |

No claim may combine artifact-backed behavior and paper-exact wording. Open64
extensions receive distinct claim IDs and support rows even when inspired by a
source artifact.

### 5.2 Minimum Stage 1 claim catalog

The planned `claims.json` must include at least these entries:

| Claim ID | Class | Frozen claim | Downstream gate |
| --- | --- | --- | --- |
| `CLM-S1-CONFIG-FIXED-N` | `o0-handoff-consumption` | All planners consume the same requested fixed `N`; no rewrite changes it. | `O2-S12-001`, `O2-S15A-001`, `O2-S15B-001` |
| `CLM-S1-ANT-BOUNDARY` | `runtime-projection-requirement` | Release execution uses public ACE rtlib C surfaces, `FHErt_common`, `FHErt_ant`, and `LIB_ANT`. | `O2-S15B-001`, `O2-S18-001` |
| `CLM-S1-MULCC-RELIN` | `runtime-projection-requirement` | Canonical `MulCC` projects to `Mul_ciph3` and adjacent `Relin`, with no duplicate Relin. | `O2-S15A-001`, `O2-S15B-001`, `O2-S18-001` |
| `CLM-S1-MKR-SEARCH` | `artifact-backed-production` | MetaKernel enumerates artifact-equivalent `(Pb,Ps)`, capacity, cost, and tie order. | `O2-S13-001` |
| `CLM-S1-MKR-PAPER-DELTA` | `paper-strict-differential` | Paper Eq. 11 and capacity differences are preserved and explained, not forced to match the artifact. | `O2-S13-001` |
| `CLM-S1-MKR-MVM-SLOTS` | `artifact-backed-production` | Accepted MVM plans have exact slot classes, rotations, and transformed plaintext. | `O2-S13-001` |
| `CLM-S1-MKR-CONV-CORE` | `artifact-backed-production` | The locked Ke2Col Conv core is supported only within its frozen input domain. | `O2-S14-001` |
| `CLM-S1-RESBM-REGION` | `artifact-backed-production` | `R0` has depth zero and computation regions have uniform depth one under the fixed projection. | `O2-S16-001` |
| `CLM-S1-RESBM-SCHEDULE` | `artifact-backed-production` | The accepted solver uses `q_w=q`, uniform one-level consumption, fixed cost inputs, and deterministic tie order. | `O2-S17-001` |
| `CLM-S1-RESBM-PROTECTED` | `open64-extension` | Only replaceable automatic baseline-DP sites may be replaced; protected sites constrain the search. | `O2-S17-PROTECTED` |
| `CLM-S1-CALL-CFG` | `open64-extension` | Only the bounded direct-call and zero-level retained-loop subset is accepted. | `O2-S18-001` |
| `CLM-S1-BASELINE-FALLBACK` | `o0-handoff-consumption` | Increment-off or approved fallback preserves the complete qualified O0 profile. | `O2-LAYOUT-001`, `O2-S19-001` |

Each claim entry records authority file and hash, exact source anchors, support
row IDs, positive and negative fixture IDs, oracle ID, first diagnostic,
capability requirements, tolerance, owner, reviewers, downstream invalidation
set, and retained artifact paths.

### 5.3 Support-scope vocabulary

Every support row uses exactly one scope label:

| Scope | Contract |
| --- | --- |
| `shared-o0-o2-comparison-required` | O0 and O2 execute the same input under the same fixed `N`, parameter family, provider, protected sites, and measurement protocol. |
| `o0-handoff-required` | O0 produces and E0 qualifies the record, API, or evidence; E1 and later stages only consume it. |
| `o2-core-only` | The row is mandatory O2 behavior and does not broaden O0 acceptance. |
| `o2-extension-only` | The row is optional and remains disabled until its separate semantics, oracle, capability, and acceptance gate pass. |
| `fallback-required` | Unsupported or unprofitable O2 behavior must reject or restore the complete qualified O0 profile under the accepted fallback contract. |

Every row records a non-circular input predicate, source authority, positive
bounds, negative boundary, fixed diagnostic, oracle, capability requirements,
owner, fallback behavior, and acceptance test. A row cannot define support as
"whatever the implementation accepts."

### 5.4 Minimum support boundaries

- Graphs are static typed tensor/CKKS DFGs. Direct nonrecursive calls are a
  bounded Open64 extension. Indirect calls, unresolved externals, recursion,
  mutually recursive calls, and irreducible encrypted control flow reject.
- Shapes have static rank and dimensions. Deterministic padding records lineage.
  Dynamic rank, dynamic dimension, and arithmetic overflow reject.
- `N` is fixed before canonicalization and provides `S=N/2`. `N=auto` and silent
  larger-`N` rewrites reject.
- MVM uses one ciphertext input and a known plaintext matrix within the locked
  source/AE shape domain. Ciphertext or unknown weights reject.
- Conv core uses one ciphertext input/output, plaintext square odd kernel,
  stride 1, symmetric same padding, and channel-divisible or explicitly
  zero-padded shapes. Broader cases are disabled extension rows.
- ReSBM uses `q_w=q`, integer scale-degree plus logical level, uniform one-level
  consumption, and known nonnegative frequency. Non-uniform or multi-level
  consumption rejects before ReSBM.
- A retained multi-node SCC consumes zero levels. A multiplication SCC is
  legally unrolled within trip count 64 and expanded encrypted-node count 10000,
  or rejects.
- Unknown frequency is distinct from unreachable zero. Saturation at
  `UINT64_MAX` emits a stable diagnostic and disqualifies profitability claims.

## 6. Manifest, Interface, and Data Contracts

### 6.1 Manifest family and common invariants

E1 publishes a content-addressed family under `testdata/fhe_o2/stage1/`:

```text
e1-activation.json  source-lock.json  claims.json  support-matrix.json
diagnostics.json  tolerances.json  fixture-index.json  generator-bounds.json
oracle-contracts.json  oracle-dependency-policy.json  cost-contract.json
evidence-schema.json  traceability.json  SHA256SUMS
```

Algorithm fixtures reside under `testdata/fhe_o2/metakernel-mvm/`,
`metakernel-conv/`, `resbm-region/`, `resbm-oracle/`, and `resbm-protected/`.
Each family has its own hashes and is referenced from `fixture-index.json`.

All JSON uses a reviewed schema version, canonical normalization, UTF-8 without
a byte-order mark, explicit integer ranges, and hashes for referenced files.
Stable IDs cannot depend on addresses, temporary paths, directory order, or
threads. Every manifest records its stage/work item, authority hashes, O0
qualification fingerprint, owner/reviewers, status, dependent claims,
invalidation set, and no-placeholder assertion.

`source-lock.json` also records immutable commits, submodules, paper/file hashes,
licenses, build/tool versions, source anchors, claim IDs, and normalized delta.
A mismatch emits `FHE-O2-SOURCE-LOCK-MISMATCH` before fixture evaluation.

### 6.2 Diagnostics and precedence

Each diagnostic records code, owner, claim/support row, severity, message,
payload keys, trigger, precedence, fallback permission, and invalidation. The
minimum set includes `FHE-O2-SOURCE-LOCK-MISMATCH`,
`FHE-O2-ARCH-FREEZE-UNRESOLVED`,
`FHE-O2-CKKS-UNSUPPORTED-LEVEL-CONSUMPTION`,
`FHE-O2-RESBM-MUL-SCC-UNSUPPORTED`, and `FHE-O2-FREQ-SATURATED`, plus reviewed
stale-plan, provider/profile, protected-site, fixed-N, duplicate-Relin, and
oracle-dependency codes.

Precedence is fail-closed: stale authority before support; malformed input before
profitability; fixed-N, security, and provider impossibility before planning;
protected-boundary illegality before cost; oracle dependency failure before
accepting an oracle result. Later production code cannot invent an acceptance-
path diagnostic outside the frozen registry.

### 6.3 Oracle interface

E1 freezes data interfaces, not production algorithms. Each oracle accepts only
normalized raw JSON graphs/tensors/slots, fixed parameter intent, and raw cost or
capability tables. It returns normalized legality, candidates, selection, state,
cost, diagnostic, and explanation; reports all rejected predicates and ties;
uses checked arithmetic; is deterministic; reads no producer memory or private
WN layout; and imports no production search, region, transfer, cost, tie, slot,
or materialization helper. Its explicit allowlist is dependency-audited.

```text
python3 <oracle> --input <raw.json> --contract <contract.json> \
  --output <oracle.json> --diagnostics <diagnostics.json>
```

The accepted `oracle-contracts.json` supplies each exact future path. This plan
does not claim that an oracle tool exists.

### 6.4 MetaKernel contract

For fixed `S=N/2`, the independent artifact oracle freezes:

```text
bsopt divides nd; Pb = nd / bsopt; Pb % Ps == 0
rep = ceil((kd*Ps + nd) / kd); gs = Pb / Ps
cost = ceil(log2(rep)) + (bsopt - 1) + (gs - 1) + (Ps - 1)
kd != S requires kd*Ps + nd <= S; kd == S requires Ps == 1
```

Selection is minimum cost, then larger `Ps`, then first ascending enumeration.
The oracle independently recomputes every candidate/derived field, full slot
map and `valid/zero/junk/gap` class, transformed plaintext, signed rotations,
winner, and tie reason. A separate paper oracle computes Eq. 11 and
`kd*(Ps+1)<=S`; named differences are preserved. Fixtures include Figure 4,
MVM1/MVM2, non-power-of-two cases, `nd<kd`, `kd==S`, exact capacity, and one over
capacity.

The Conv contract freezes the Ke2Col core separately from stride, padding,
depthwise/grouped, multi-ciphertext, sharding, halo, and compaction extensions.
Each extension requires its own row, clear tensor and slot oracles, sentinel
case, capability, and diagnostic.

### 6.5 ReSBM contract

The raw-graph oracle freezes `R0` depth zero; computation-region depth one;
`q_w=q`; uniform one-level transitions; zero-level retained loops; bounded
unroll or rejection of multiplication SCCs; legal rescale, mod-switch,
bootstrap, and cuts; mandatory immediate Relin outside placement choice; full
before/after state; explicit frequency states; checked integer cost; and tie
tuple `(total_cost, bootstrap_count, rescale_count,
lexicographic_action_edges)`.

Exhaustive fixtures use at most 8 nodes, 12 edges, 6 logical levels, and 10
action sites. Random coverage uses 2000 graphs with seed `0x5253424d`.
Protected-adapter coverage uses seed `0x52534250` and imports qualified immutable
protected-site and baseline-DP provenance.

### 6.6 Tolerance and cost contracts

| Test class | Frozen tolerance |
| --- | --- |
| Structural, identity, state, action, slot, candidate, tie, cost, hash, and diagnostic | Exact. |
| Pure double tensor/MVM/Conv | Absolute and relative error `<=1e-12`; NaN/Inf fail. |
| Later focused encrypted kernel | Absolute `<=1e-4`, relative `<=1e-6`; NaN/Inf fail. |
| Later application logits | Max absolute `<=1e-3`; max relative `<=1e-4` when reference magnitude `>=1e-2`; exact top-1 with lowest-index tie. |
| Required-zero decoded slot | Absolute value `<=1e-8`. |

Ciphertext bytes are not a correctness oracle. Any post-result tolerance change
requires a new pre-registered review and full dependent rerun.

MetaKernel artifact cost remains an exact integer operation count, not measured
ANT latency. Runtime models use integer nanoseconds, checked/saturating sums,
complete component counts, and raw-table provenance bound to the ANT manifest.
Calibration/holdout IDs are split by a checked-in hash before fitting. E1 records
the governing later thresholds, including MAPE `<=20%`, 95th-percentile APE
`<=35%`, absolute error `<=2 us` for sub-microsecond operations, and wrong-order
rate `<=10%`; it performs no fit and claims no accuracy.

## 7. Work Breakdown Structure

Each item below has one concrete output and one merge gate. File paths are
relative to `testdata/fhe_o2/stage1/` unless stated otherwise.

| Work item | Concrete change and files/components | Dependency type | Tests/oracle and artifacts | Owner/reviewer | Merge or exit rule |
| --- | --- | --- | --- | --- | --- |
| `E1-W01` | Validate the current PRE-O2 and O0 qualification gates; write `e1-activation.json`. | Hard implementation gate | Signature, hash, support, environment, artifact-root, and stale-trigger validation; activation report and fingerprint delta. | E1 owner / independent O0 qualification and reproducibility reviewers | No later E1 commit merges until accepted; mismatch makes E1 `Unverified`. |
| `E1-W02` | Write `claims.json`, its schema, source anchors, and claim-to-test crosswalk. | Contract after W01 | Schema, duplicate-ID, authority/hash, claim-class, and downstream coverage checks; normalized catalog and unmapped report. | Claims editor / architecture and paper/source reviewers | No unmapped, unowned, moving, duplicate, or paper-mislabeled claim. |
| `E1-W03` | Complete `source-lock.json`, license inventory, public-header/symbol allowlist, and source delta. | Evidence/build input after W02 | Recompute all hashes from clean sources; reject private ACE headers, `LIB_ACE`, missing licenses, and unbound libraries; retain hash/symbol reports. | Source-lock editor / legal, build, and ANT reviewers | Every source and ABI input is content-addressed. |
| `E1-W04` | Write non-circular `support-matrix.json` for graph, shape, fixed `N`, kernels, CKKS, Relin, SCC/calls, frequency, provider, security, and fallback. | Semantic contract after W02-W03 | Positive/negative boundary enumeration; normalized matrix, uncovered-dimension report, and fallback crosswalk. | Claims/fixture editors / O0, CKKS, MetaKernel, and ReSBM reviewers | Every row has one scope, exact bounds, diagnostic, oracle, owner, and fallback; no O0 broadening. |
| `E1-W05` | Write `diagnostics.json` with codes, payloads, severity, precedence, and fallback permission. | Interface after W04 | Duplicate-code, missing-payload, ambiguous-precedence, and missing-diagnostic mutations; normalized registry and decision table. | Diagnostics owner / compiler and test reviewers | Every negative fixture has one frozen first diagnostic. |
| `E1-W06` | Write `fixture-index.json`, `generator-bounds.json`, per-algorithm manifests, and approved golden inputs. | Test data after W04-W05 | Two clean generations, fixed seeds, finite bounds, hash equality, sentinel coverage; logs, fixtures, and same-stem `.B`/`.T`. | Fixture editor / reproducibility and oracle reviewers | No placeholder, hidden download, unbounded generator, zero-only gap test, or post-result seed. |
| `E1-W07` | Freeze MetaKernel raw oracle, candidate, slot-map, transformed-plaintext, clear-oracle, and paper-differential schemas. | Algorithm-test contract after W06 | Named teaching/boundary/tie cases and dependency audit; expected candidate tables, rotations, slot classes, weight hashes, and deltas. | MetaKernel oracle owner / artifact, paper, and tensor reviewers | Every derived field and tie is independently recomputable and paper/artifact classes remain separate. |
| `E1-W08` | Freeze ReSBM raw-graph, region/cut/action/state/cost, protected-site, and production-helper-denylist schemas. | Algorithm-test contract after W06 | Exhaustive/random SCC, loop, chain, frequency, protected-boundary, and fallback cases; raw graphs, expected plans, and dependency audit. | ReSBM oracle owner / artifact, CKKS, CFG, and O0 boundary reviewers | No production helper dependency; all complete fields, not only cost or placement, are compared. |
| `E1-W09` | Freeze `cost-contract.json`, CKKS/ANT capability and key obligations, lifecycle contract, secret-scan policy, and exact mapping requirements. | Runtime-projection contract after W03-W05 | `LIB_ANT`, `Mul_ciph3 -> Relin`, duplicate-Relin, runtime-mismatch, lifecycle, secret, and model-provenance mutations; capability and cost reports. | CKKS/runtime/cost owners / crypto, build, and performance reviewers | No unknown capability, secret server field, or unauthorized `be.so` dependency. |
| `E1-W10` | Implement proposed `osprey/be/vho/tests/verify_o2_stage1_source_lock.py` plus test-only schema/normalization support. | Verification after W02-W09 | Positive family and one mutation per field class; normalized lock, dependency, mutation, and failure reports. | Test owner / reproducibility and security reviewers | Verifier fails closed without later production imports or environment-dependent order. |
| `E1-W11` | Run all rows, inspect deltas, write traceability and evidence schemas, sign, and atomically publish final hashes. | Exit after W10 | Full `O2-S10-001`, traceability and artifact audit; decision, signoff, and `SHA256SUMS`. | E1 owner / all named reviewers | Every required row passes; `Accepted` unlocks P2 only. |

## 8. Proposed Commit and Review Sequence

All commits map to governing commit group `S1-1`. Each commit has one semantic
goal and contains no production optimizer code or generated acceptance artifacts.

| Commit | Scope | Required review before merge |
| --- | --- | --- |
| `E1-C1` | Activation schema and current `O2-O0Q-001` binding | O0 qualification and reproducibility review |
| `E1-C2` | Claim catalog, authority classes, source/paper locks, and delta inventory | Architecture and paper/source review |
| `E1-C3` | Support matrix, fallback boundaries, diagnostics, and precedence | O0 baseline, CKKS, and diagnostics review |
| `E1-C4` | Deterministic fixture index, generator bounds, tolerances, and evidence schema | Test and reproducibility review |
| `E1-C5` | MetaKernel artifact-oracle and paper-differential contracts | Independent artifact, paper, and tensor review |
| `E1-C6` | ReSBM raw-graph and protected-adapter oracle contracts | Independent algorithm, CKKS, and boundary review |
| `E1-C7` | ANT capability, key/security, lifecycle, and cost obligations | Runtime, cryptography, build, and performance review |
| `E1-C8` | Contract verifier, mutation tests, traceability, and exit decision | Independent test and integration review |

If a commit changes a support row, diagnostic, oracle contract, or tolerance, it
must update all linked fixtures, traceability rows, and hashes in the same commit.
An optional extension row is always a separate commit. The exit decision commit
contains no algorithm change.

## 9. Verification Matrix

All commands below are proposed target commands. They are exact future
interfaces, not claims that the scripts or manifests already exist.

| Test ID and proposed command | Source/hash and input class | Expected result and tolerance | Seed/bounds | Platform/provider | Run protocol and retained artifacts | Owner/reviewer and pass rule |
| --- | --- | --- | --- | --- | --- | --- |
| `O2-E1-ACT-001`; `python3 osprey/be/vho/tests/verify_o2_e1_activation.py --qualification test-artifacts/o2/O2-O0Q-001 --activation testdata/fhe_o2/stage1/e1-activation.json --artifacts test-artifacts/o2/O2-E1-ACT-001` | Accepted PRE-O2 lock and full O0 qualification bundle | Exact signature, hash, schema, environment, support, and artifact-root match; stale input is `Unverified`; no tolerance | Deterministic; one mutation per bound fingerprint class | Qualified host and pinned ACE rtlib/ANT identity; no runtime call | 0 warmups, 1 run, 120 s; retain activation report, deltas, and diagnostics 180 days | E1 owner / independent O0 qualification reviewer; exact current fingerprint required |
| `O2-E1-CLAIM-001`; `python3 osprey/be/vho/tests/verify_o2_stage1_claims.py --claims testdata/fhe_o2/stage1/claims.json --source-lock testdata/fhe_o2/stage1/source-lock.json --support testdata/fhe_o2/stage1/support-matrix.json --diagnostics testdata/fhe_o2/stage1/diagnostics.json --artifacts test-artifacts/o2/O2-E1-CLAIM-001` | All Stage 1 claims, source anchors, support rows, and diagnostics | Exact one-to-one links; zero duplicate, unowned, unmapped, moving, circular, or paper-mislabeled claim; no tolerance | Deterministic catalog; every entry plus one mutation per required field | Provider-free; ACE rtlib/ANT is a locked requirement only | 0 warmups, 1 run, 120 s; retain normalized catalog and mutation report indefinitely | Claims editor / architecture and paper/source reviewers; zero error rows |
| `O2-E1-FIX-001`; `python3 osprey/be/vho/tests/verify_o2_stage1_fixtures.py --index testdata/fhe_o2/stage1/fixture-index.json --bounds testdata/fhe_o2/stage1/generator-bounds.json --tolerances testdata/fhe_o2/stage1/tolerances.json --artifacts test-artifacts/o2/O2-E1-FIX-001` | Positive, negative, boundary, sentinel, differential, malformed, stale, and corruption fixture families | Two clean generations normalize and hash identically; every boundary has expected first diagnostic; numeric self-checks use frozen tolerances | Named seeds; MetaKernel dimensions and ReSBM limits from contracts; no unbounded generator | Provider-free fixture generation | 0 warmups, 2 clean generations, 300 s; retain manifests, generator logs, hashes, and matching `.B`/`.T` 180 days | Fixture editor / reproducibility and oracle reviewers; byte-normalized equality and complete coverage |
| `O2-E1-ORACLE-001`; `python3 osprey/be/vho/tests/verify_o2_stage1_oracle_contracts.py --contracts testdata/fhe_o2/stage1/oracle-contracts.json --dependency-policy testdata/fhe_o2/stage1/oracle-dependency-policy.json --fixtures testdata/fhe_o2/stage1/fixture-index.json --artifacts test-artifacts/o2/O2-E1-ORACLE-001` | MetaKernel artifact/paper and ReSBM raw-graph/protected oracle interfaces | Schema and expected teaching-case data exact; production-helper dependency graph empty; no tolerance for structural fields | MetaKernel named boundaries; ReSBM exhaustive bounds and seeds `0x5253424d` and `0x52534250` | Provider-free | 0 warmups, 1 run, 300 s; retain contracts, dependency graph, expected tables, and mutation diagnostics 180 days | Oracle owners / independent artifact and security reviewers; every contract field and denylist rule passes |
| `O2-E1-ANT-001`; `python3 osprey/be/vho/tests/verify_o2_stage1_ant_requirements.py --source-lock testdata/fhe_o2/stage1/source-lock.json --cost-contract testdata/fhe_o2/stage1/cost-contract.json --diagnostics testdata/fhe_o2/stage1/diagnostics.json --artifacts test-artifacts/o2/O2-E1-ANT-001` | Pinned ACE rtlib headers/symbol requirements, `LIB_ANT`, state/key/cost obligations | Exact hashes and allowlist; exact `Mul_ciph3 -> Relin`; `LIB_ACE`, private header, duplicate Relin, missing lifecycle, secret field, or unbound cost input rejects; no tolerance | One mutation per header, symbol, provider, mapping, key, lifecycle, secret, and cost field | Provider-free contract audit; no ACE library load | 0 warmups, 1 run, 120 s; retain header/symbol report, mappings, mutations, and secret-scan schema 180 days | Runtime/CKKS owners / build, crypto, and performance reviewers; zero unbound or unsafe requirement |
| `O2-S10-001`; `python3 osprey/be/vho/tests/verify_o2_stage1_source_lock.py --manifest testdata/fhe_o2/stage1/source-lock.json --diagnostics testdata/fhe_o2/stage1/diagnostics.json --support testdata/fhe_o2/stage1/support-matrix.json --artifacts test-artifacts/o2/O2-S10-001` | Complete E1 manifest family, all governing source hashes, MetaKernel/ReSBM delta corpus, and generated fixture manifests | Exact refs, hashes, claims, test IDs, support rows, fixed-N rule, diagnostics, tolerances, cost units, and authority labels; `FHE-O2-SOURCE-LOCK-MISMATCH` on change | All generators declare fixed finite seeds and bounds | Provider-free; manifest names exact ACE rtlib/ANT requirements | 0 warmups, 1 run, 120 s; retain normalized lock report, differential cases, traceability, and source delta indefinitely | E1 owner / paper-code, test, architecture, CKKS, and runtime reviewers; exit 0 with no placeholder, moving ref, duplicate ID, unowned diagnostic, or invalid claim label |

The primary governing test is `O2-S10-001`. The `O2-E1-*` rows are planned
subchecks and do not redefine the formal milestone.

## 10. Negative, Failure, Security, and Compatibility Matrix

The E1 suite shall include at least one independently reviewed negative for each
row below.

| Negative class | Required behavior |
| --- | --- |
| Missing, moving, or altered source | Fail with `FHE-O2-SOURCE-LOCK-MISMATCH` before dependent fixture or oracle evaluation. |
| Stale O0 qualification | Mark E1 `Unverified`; do not update hashes or continue with provisional work. |
| Missing master/ADR reconciliation | Fail with `FHE-O2-ARCH-FREEZE-UNRESOLVED`. |
| Duplicate or unowned claim/support/diagnostic/test ID | Fail normalization and identify every conflicting owner/path. |
| Artifact behavior labeled paper-exact | Reject the claim catalog and require separate artifact and differential rows. |
| Circular support predicate | Reject any row defined by production acceptance rather than a fixed input predicate. |
| Missing boundary or fallback | Reject the row; unsupported input cannot reach a partial O2 plan. |
| Post-hoc tolerance, seed, or fixture change | Stale every dependent claim and require a new pre-registered review. |
| Unbounded or nondeterministic generator | Reject the fixture family and retain the two-run mismatch. |
| Zero-only junk/gap fixture | Reject as insufficient evidence; require nonzero sentinels. |
| Oracle imports a production helper | Reject the oracle contract and all dependent acceptance claims. |
| MetaKernel paper/artifact delta suppressed | Reject the differential report even if one selected result happens to match. |
| Unsupported MetaKernel shape accepted because a candidate fits | Reject the support row and expected result. |
| `q_w!=q`, non-uniform, or multi-level ReSBM accepted | Require the frozen first diagnostic before planning. |
| Retained multiplication SCC outside bounds | Require legal unroll or `FHE-O2-RESBM-MUL-SCC-UNSUPPORTED`. |
| Unknown frequency treated as zero | Reject costing; unreachable zero requires explicit proof provenance. |
| Frequency or cost saturation used for optimality | Emit `FHE-O2-FREQ-SATURATED` or the accepted cost diagnostic and disqualify the claim. |
| `N=auto` or plan-selected larger `N` | Reject; a recommendation requires explicit recompilation and cannot mutate the current request. |
| `LIB_ACE` selected as ANT | Reject provider identity before later projection. |
| ACE private compiler/AIR header in generated-C allowlist | Reject the ABI lock. |
| ACE rtlib header, symbol, or build drift | Stale P1a/P1b, O0 qualification, E1, and every dependent projection/cost claim. |
| Q/P/CRT/profile mismatch contract omitted | Reject the runtime-projection claim as incomplete. |
| Canonical `MulCC` mapped to fused `Mul_ciph` plus explicit `Relin` | Reject as duplicate Relin; require `Mul_ciph3` followed by adjacent `Relin`. |
| Server-evaluation secret or decryptor field permitted | Reject the schema and secret-scan policy. |
| Direct `be.so` dependency assumed | Stop for explicit design/build/link-closure review; E1 cannot authorize it. |
| Missing feature in an old artifact | Preserve accepted old/feature-absent behavior; do not interpret absence as corrupt unless the version requires the feature. |
| Failed manifest publication | Leave no apparently accepted manifest family or valid final `SHA256SUMS`; retain failure logs under a distinct path. |

E1 corruption coverage mutates every required count, ID, path, hash, version,
scope, owner, source anchor, tolerance, seed, bound, capability, and traceability
reference. Failure injection covers each publication step. Publication becomes
visible only after the complete family and final hashes validate.

## 11. Evidence and Retention

### 11.1 Required E1 evidence family

An accepted run retains at least:

```text
e1-activation.json
source-lock.json
claims.json
support-matrix.json
diagnostics.json
tolerances.json
fixture-index.json
generator-bounds.json
oracle-contracts.json
oracle-dependency-policy.json
oracle-dependency-report.json
metakernel-artifact-cases.json
metakernel-paper-differential.json
resbm-raw-graph-cases.json
resbm-protected-cases.json
cost-contract.json
ant-requirements.json
generated-c-include-symbol-allowlist.json
secret-scan-policy.json
evidence-schema.json
traceability.json
normalized-lock-report.json
mutation-tests.json
review-signoff.json
e1-decision.md
SHA256SUMS
```

If a fixture produces meaningful WHIRL evidence, retain `fixture.B` and
same-stem `fixture.T` from `ir_b2a -st -src fixture.B fixture.T`. Preserve the
recorded source at its DST path. A phase trace uses a descriptive lower-case
suffix such as `fixture.vho.t` and must not replace `fixture.T`.

E1 does not produce an O2 selected-plan `.B`, materialized O2 `.B`, generated C,
or ANT execution result. Those artifacts belong to later stages. Any such file
in an E1 acceptance directory is an error unless it is an immutable referenced
copy from the qualified O0 bundle and is labeled as O0 evidence.

### 11.2 Retention and publication

- Failed development runs remain at least 30 days under distinct failure IDs.
- E1 milestone evidence remains at least 180 days.
- The accepted E1 source/claim/support/oracle lock and signoff remain for the
  repository lifetime.
- Generated local review artifacts are not committed unless explicitly approved
  as golden fixtures.
- The next run cleans its own designated artifact directory at start, never at
  the end of the preceding run.
- Final reporting includes absolute retained artifact paths and a directly
  inspectable link to every retained `.T` trace.

## 12. Requirement Traceability

Before E1 exit, `traceability.json` shall encode the complete chain
`requirement -> master/ADR -> O2 plan -> E1 work item -> test -> artifact`.
The master-successor section, version, commit, and hash must replace every
pending entry before activation.

| Requirement | Master/ADR binding | Governing O2 plan | E1 work item | Verification | Evidence |
| --- | --- | --- | --- | --- | --- |
| No O2 work before complete O0 qualification | Accepted successor section and planner-ownership ADR; pending at plan-writing time | Sections 0.1, 2.1, 13 `O2-O0Q` | E1-W01 | `O2-E1-ACT-001`, `O2-S10-001` | `e1-activation.json`, qualification delta, decision |
| Freeze claims before production algorithms | Accepted O2 ownership and evidence section | Sections 2.2, 2.3, 13 `S1.0` | E1-W02 | `O2-E1-CLAIM-001`, `O2-S10-001` | `claims.json`, `source-lock.json` |
| Preserve fixed requested `N` | Accepted configuration and CKKS sections | Sections 4.2, 6, 9, 15.2 | E1-W04, E1-W09 | `O2-E1-CLAIM-001`, `O2-E1-ANT-001` | support, diagnostics, CKKS obligations |
| Use public ACE rtlib and ANT only | Accepted runtime amendment and `FRZ-08/09` | Sections 2.3, 5.1, 7, 13 `P1a/P1b` | E1-W03, E1-W09 | `O2-E1-ANT-001`, `O2-S10-001` | header/symbol hashes, `ant-requirements.json` |
| Preserve immediate Relin | Accepted CKKS semantic contract | Sections 4.2, 7.1, 9.2, 15.2 | E1-W09 | `O2-E1-ANT-001` | mapping contract and duplicate-Relin negative |
| Separate MetaKernel artifact behavior from paper differential | Accepted algorithm evidence section | Sections 2.3, 10.1, 13 `S1.3/S1.4` | E1-W02, E1-W07 | `O2-E1-ORACLE-001`, `O2-S10-001` | candidate tables and paper delta |
| Bound ReSBM to the conservative artifact core | Accepted CKKS/ReSBM sections | Sections 4.2, 10.3-10.5, 13 `S1.6/S1.7` | E1-W04, E1-W08 | `O2-E1-ORACLE-001`, `O2-S10-001` | raw graphs, expected plans, rejection cases |
| Preserve protected/manual/pre-ReLU sites | Accepted protected-DP amendment and ADR | Sections 10.4, 11.3, `O2-S17-PROTECTED` | E1-W04, E1-W08 | `O2-E1-ORACLE-001` | protected-site cases and fallback contract |
| Keep server evaluation free of secrets | Accepted runtime/security lifecycle decision | Sections 7.3, 11.2, 15.2 | E1-W09 | `O2-E1-ANT-001` | secret-scan policy and negative schema |
| Retain reviewable WHIRL evidence | `AGENTS.md` reviewable-artifact rules | Sections 3.1, 8.2, 15.7 | E1-W06, E1-W11 | `O2-E1-FIX-001`, `O2-S10-001` | matching fixture `.B`/`.T`, hashes, reports |

## 13. Exit, Rollback, and Stop Rules

### 13.1 Exit decision

`O2-E1-EXIT=Accepted` requires all of the following:

1. The bound `O2-O0Q-001=Qualified` fingerprint is still current.
2. The accepted master/ADR traceability entries contain no pending placeholder.
3. Every Stage 1 claim maps to one immutable authority class, non-circular
   support row, stable diagnostic, deterministic fixture family, independent
   oracle contract, downstream test, owner, reviewer, and retained artifact.
4. Artifact-backed behavior, paper-strict differential behavior, and Open64
   extensions are separated in claims, support rows, reports, and capability
   requirements.
5. Fixed-resolved and auto-remaining families are distinct and retain the same
   fixed requested `N`.
6. The ACE rtlib/ANT lock names `FHErt_common`, `FHErt_ant`, and `LIB_ANT`, and
   records the public-header/symbol/build fingerprints.
7. Immediate Relin, protected-boundary, full-baseline fallback, no-server-secret,
   and no-unreviewed-`be.so` rules have explicit positive and negative contracts.
8. Oracle dependency audits find no production helper dependency.
9. Every proposed verification row passes, including `O2-S10-001`.
10. The complete evidence family is atomically published, hashed, retained, and
    independently reviewed.

The only permitted completion wording is:

```text
O2-E1 accepted; S1.0 claim, source, support, fixture, diagnostic, oracle, and
cost contracts are frozen for the bound O0 qualification. P2 may begin.
```

E1 acceptance does not mean MetaKernel, ReSBM, Stage 1, or O2 is implemented or
accepted.

### 13.2 Rejected and Unverified outcomes

- `Rejected` means a reviewed E1 contract is internally wrong, contradictory,
  insecure, non-reproducible, or inconsistent with governing authority. Return
  the defect to its E1 owner and retain exact evidence.
- `Unverified` means required authority, qualification, source, environment,
  provider lock, reviewer, or artifact is absent or stale. It does not permit
  provisional downstream work.

### 13.3 Rollback and invalidation

Before E1 exit, rollback removes the candidate manifest family from publication
and retains it only under a failure ID. After E1 exit, a contract change creates
a new version and delta review; accepted manifests are never silently rewritten.
All affected P2/S1 work pauses until targeted requalification accepts the new
E1 version.

An unsupported or unprofitable later increment may use only the accepted
whole-profile fallback. It must not roll back individual O0 passes, protected
sites, fixed `N`, canonicalization, state records, or ANT provider requirements.

### 13.4 Architecture review triggers

Stop E1 and return to architecture review if:

- two owners or no owner exist for a semantic decision;
- the current master conflict is treated as resolved without the accepted
  successor or ADR;
- a moving or unhashed source is required for a claim;
- E1 begins without a current complete O0 qualification;
- a claim requires changing requested `N`, immediate Relin, protected-site
  semantics, the Open64/ACE ownership boundary, or the completion definition;
- a new WHIRL operator, section, record image, or driver option is required
  before common/com review;
- a direct `be.so` dependency on ACE rtlib is proposed without explicit
  dependency and link-closure approval;
- ACE rtlib/ANT cannot express the complete required parameter, bootstrap,
  action, key, or lifecycle contract;
- the server-evaluation design requires a secret key or decryptor;
- an exact independent oracle cannot be constructed without production helpers;
- a support extension lacks a fixed rejection boundary and exact oracle; or
- a tolerance must be relaxed after observing a result.

## 14. Handoff to O2-E2

The accepted E1 handoff to P2 and S1.1 contains only reviewed contracts and test
data. It includes the current activation fingerprint, claims, record requirements,
support rows, diagnostics, fixture hashes, oracle interfaces, cost/capability
requirements, evidence schema, traceability, and signoff.

P2 may use these requirements to review O2 record extensions. S1.1 may use the
accepted records to implement the first identity selected-plan transaction.
Neither stage may reinterpret E1 support, tolerances, tie orders, or authority
classes without a versioned E1 delta and targeted requalification.
