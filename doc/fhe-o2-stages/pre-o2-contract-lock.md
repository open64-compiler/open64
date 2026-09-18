# PRE-O2-LOCK Detailed Execution Plan

Status: Open. The successor master plan and ownership ADR have not been accepted.

Plan revision: 0.1

Date: 2026-09-15

Engineering stage: `PRE-O2-LOCK`

Formal O2 milestone mapping: `F0`, `FRZ-01` through `FRZ-09`

Exit gate: `PRE-O2-LOCK-EXIT`

This document defines external governance work. It is not an O2 implementation
stage, an O0 implementation plan, an architecture amendment, or evidence that
the proposed baseline has been accepted.

## 1. Authority and Metadata

### 1.1 Governing inputs

| Input | Locked reference for this revision | Status at plan creation |
| --- | --- | --- |
| Repository rules | `AGENTS.md` | Governing repository invariants |
| Current recorded master, Markdown | `doc/DSC_FHE_Compiler_Architecture_and_Integration_Plan_v0.10.md`, SHA-256 `7EF644E9BECEFB541102514B83656C78756ECA71398A771A2A5160FC10B0CB25` | Current authority; successor pending |
| Current recorded master, DOCX | `doc/DSC_FHE_Compiler_Architecture_and_Integration_Plan_v0.10.docx`, SHA-256 `0018769C26B5A0BCD1BDFCBD85AA97B8BAFEA381D7640FBB9E2E81B0022013D9` | Current authority; successor pending |
| Governing architecture commit | `ee1dc6382246c58f49a3097157a8c4e8ff2440c8` | Recorded baseline |
| Governing O2 plan | `doc/FHE-O2-ACE-RESBM-METAKERNEL-INTEGRATION-PLAN.md`, version 1.6 | Proposed where it differs from the master; Sections 12.3-12.5 govern stage structure |
| Detailed execution index | `doc/FHE-O2-DETAILED-EXECUTION-PLAN.md`, plan set version 0.2 | Non-normative navigation for the stage plan set |
| Ownership ADR | `doc/adr/FHE-O0-O2-PLANNER-OWNERSHIP.md` | Proposed path; not yet accepted |
| Reviewed Open64 baseline | `develop@eca97d4843aef03c50e5cb866f2f0e6c82e6b064` | Planning input, not a release claim |
| Fhelipe source input | `../../fhelipe/fhelipe@891b3086bf6a144deebac79290801253b9cc510c` | Partial source inspection complete; full adopted bundle pending |
| ACE rtlib source input | `../../ace-compiler@929e9b621f11bebbaa9ec1e215f4a52e3d07109b` | Selected runtime planning input |

The authority order is: explicit user decisions and `AGENTS.md`; the accepted
master; accepted ADRs within their stated scope; the governing O2 plan; this
stage plan; then papers and source-locked implementations as evidence. No task
in this stage may treat a proposed O2-plan statement as an accepted master-plan
amendment.

### 1.2 Roles

| Role | Responsibility |
| --- | --- |
| Architecture owner | Accepts master-plan wording and resolves conflicts with the proposed baseline |
| O0 and O2 interface owners | Accept the baseline input and confirm that it is sufficient for later producer acceptance and consumer qualification |
| Common/com and driver editors | Own record compatibility, option semantics, and option propagation |
| Runtime, build, and security owners | Own the ANT lock, dependency topology, lifecycle, and server key separation |
| Independent evidence reviewer | Recomputes hashes and verifies that accepted documents and manifests agree |

One named editor must be recorded for each source document. A reviewer may not
approve evidence that the same person alone produced and signed without an
independent check.

### 1.3 Consumed evidence and stale triggers

PRE-O2-LOCK consumes no O0 qualification ID because O0 implementation follows
this gate. Its inputs become stale on any accepted master or ADR change; any
adopted Fhelipe source, pass, default, build, support, or license change; any
baseline schema or owner change; any ACE rtlib revision, public header, symbol,
build, installed library, or provider change; any protected-site, DP, or option
change; or any applicable repository-invariant change.

Any stale trigger reopens the affected `FRZ-*` item. The gate returns to
`Unverified`, and every downstream acceptance fingerprint bound to that item is
invalid until a targeted delta review and reapproval complete.

## 2. Objective, Capability, and Boundaries

### 2.1 Objective

Produce one accepted, internally consistent, hash-locked contract that tells
the O0 producer what baseline and runtime boundary to implement and tells the
O2 consumer exactly what it may later qualify. The contract must close
`FRZ-01` through `FRZ-09` without implementing either optimization level. It
adds no executable user-visible compiler capability; its output is the accepted
governance bundle used by the independent O0 workstream.

### 2.2 In scope

- `FRZ-01` through `FRZ-09` contract decisions and their evidence;
- master-to-O2 conflict inventory and accepted disposition;
- adopted source, pass, configuration, support, build, license, and toolchain
  manifests;
- protected/manual/pre-ReLU boundary semantics and the constrained DP contract;
- the four O levels and `auto`, `on`, `manual`, `off`, advanced-off, and
  whole-baseline fallback truth table;
- authoritative baseline record ownership and the boundary for later O2
  extensions;
- ACE rtlib revision, public headers, generated-C include/symbol allowlist,
  build, library, provider, and operation mapping locks;
- the compiler-side exact-parameter query topology decision or an explicitly
  blocking TODO with a named owner and deadline before its dependent gate;
- client/test versus server-evaluation key and context lifecycle decisions or
  an explicitly blocking TODO before P1b;
- tracker synchronization, automated consistency checks, and signed review.

### 2.3 Explicit non-goals

- No O0 source implementation, baseline port, protected-DP implementation, or
  executable O0 acceptance is performed here.
- No O2 source, fixture, oracle, cost model, provider-free algorithm, record,
  implementation branch, or production artifact starts here.
- No MetaKernel, ReSBM, FHEFusion, HPOLY, or HPAO work starts here.
- No new WHIRL section, record, opcode, binary layout, or reader/writer behavior
  is allocated by this document.
- No direct or source-level ACE rtlib dependency is added to `be.so`.
- No document-only review is substituted for later O0 executable acceptance or
  `O2-O0Q-001`.
- No current conflict is described as resolved before the successor master and
  ADR are accepted and hash-locked.

### 2.4 Inherited invariants

- Binary WHIRL compatibility is preserved, and any future common/com record
  change requires versioning, old-reader behavior, independent reopen, and
  `ir_b2a -st -src` review.
- Python remains a source frontend. The compiler consumes persisted binary
  WHIRL without a Python runtime dependency.
- The requested ring dimension `N` is fixed before canonicalization and is not
  silently changed by layout, parameter resolution, or runtime projection.
- Canonical `MulCC(two,two)` creates only a transient three-component result
  followed immediately by mandatory `Relin` to two components.
- Manual and policy-protected bootstrap sites cannot be silently removed,
  moved, merged, or reclassified.
- Open64 owns CKKS-and-above semantic IR and planning. ACE rtlib/ANT receives a
  checked one-way execution projection and cannot replace Open64 plan truth.
- No server-evaluation process or retained artifact contains a secret key or a
  decryptor.
- O2 work remains blocked until the later `O2-O0Q-001` result is `Qualified`,
  even after this contract gate is accepted.

### 2.5 Support-scope implications

PRE-O2-LOCK accepts no workload row. It freezes the later scope vocabulary as
`shared-o0-o2-comparison-required`, `o0-handoff-required`, `o2-core-only`,
`o2-extension-only`, and `fallback-required`. Each row must name authority,
bounds, first diagnostic, oracle, capability, owner, fallback, and acceptance
test; an O2 extension cannot silently broaden O0 acceptance.

## 3. Dependency Contracts

### 3.1 Implementation and build dependency graph

```text
accepted collaborator decision on successor master wording
  -> accepted master and ownership ADR
  -> frozen Fhelipe baseline and protected-DP manifests
  -> option, record-owner, comparison, and runtime contracts
  -> synchronized trackers and hash-locked review bundle
  -> PRE-O2-LOCK-EXIT = Accepted
  -> independent O0 implementation and producer acceptance
  -> O2-E0 independent qualification
  -> O2-O0Q-001 = Qualified
  -> first O2 work in O2-E1
```

The PRE-O2-LOCK changes are documents, manifests, and verification tooling only.
They must not create a build dependency from O0 to P2 or any O2-only record.

### 3.2 Compiler pass-order contract

No compiler pass executes in PRE-O2-LOCK. The gate freezes the future order that
O0 and O2 plans must preserve:

```text
fixed requested configuration
  -> canonical Open64 FHE graph
  -> optional Stage 2 FHEFusion
  -> frozen Fhelipe layout or selected MetaKernel layout
  -> canonical pre-ReSBM CKKS state
  -> protected baseline DP import
  -> optional permitted ReSBM replacement
  -> verified final state and parameters
  -> logical and ANT-expanded keys
  -> optional legal HPOLY/HPAO
  -> standard WHIRL and generated C
  -> ACE rtlib/ANT execution
```

This pass order is a contract output, not evidence that the passes exist.
Implementation dependencies must not be inferred from the pass order. In
particular, O0 does not depend on P2, MetaKernel, or ReSBM.

### 3.3 Required external inputs

- an agreed successor-master proposal with an exact diff against v0.10;
- the complete adopted Fhelipe source bundle and dependency inventory;
- an explicit Fhelipe pass/configuration/default/support manifest;
- a protected-site and constrained-DP design decision;
- current public ACE rtlib headers, exported symbols, build files, and installed
  library artifacts for the selected revision;
- a complete list of `be.so` consumers and supported build configurations if a
  direct backend dependency is proposed; and
- security-owner input on context reentrancy, client/test key generation,
  server-evaluation imports, cleanup, status, and concurrency.

### 3.4 Build and link impact

The default accepted topology must keep compiler-side ACE rtlib queries outside
`be.so`, for example in an isolated resolver/tool with a versioned process
contract. A direct dependency is not accepted unless a separate review proves:

- link closure for every `be.so` consumer, including `lw_inline`, plugins, and
  standalone tools;
- supported static and shared configurations;
- platform, compiler, ABI, exception, and runtime-loader behavior;
- license and binary-distribution compatibility;
- defined and undefined symbol inventories before and after the change; and
- rebuild and load success for all consumers.

Until that review is explicitly accepted, the dependency decision is recorded
as `isolated resolver required` or `topology unresolved and blocking S1.5B`.
Neither state authorizes a direct `be.so` reference.

## 4. Contract Deliverables

All paths in this section are proposed outputs unless they already name a
governing document. Their existence is not claimed by this plan.

### 4.1 Master and ADR decision set

The accepted master and `doc/adr/FHE-O0-O2-PLANNER-OWNERSHIP.md` must state:

1. the mature Fhelipe layout/lowering/rescale/protected-DP package is the
   intended O0 baseline after reconciliation;
2. frozen baseline components are distinguished from future Fhelipe-derived or
   O2 increments;
3. the previous no-profitability, greedy/JIT, no-DP, and unrestricted
   `RemoveBootstraps` conflicts have explicit changed or retained dispositions;
4. O0 initial placement and O2 ReSBM replacement have separate ownership;
5. Open64 owns CKKS-and-above semantics, while ACE rtlib/ANT owns the selected
   CKKS-and-below execution surface;
6. no O2 work starts before complete O0 consumer qualification; and
7. every changed statement identifies the superseded master text and affected
   tracker and test IDs.

### 4.2 Fhelipe baseline manifest

The proposed `testdata/fhe_o2/baseline/fhelipe-baseline-lock.json` must include:

- repository URL or local authority, immutable commit, complete file and bundle
  hashes, submodules, third-party dependencies, build flags, toolchain, license,
  and environment requirements;
- ordered layout assignment, compaction, conversion hoisting/decomposition,
  lowering, rescale/state, bootstrap placement, and verification passes;
- default and disabled experimental options;
- accepted input/support rows and explicit unsupported rows;
- the exact DP objective, search domain, level/result budget, shortcut behavior,
  tie order, infeasibility rule, and protected-boundary adaptation;
- fixture and oracle ownership without constructing the fixtures in this stage;
  and
- a delta inventory separating the adopted baseline from future work.

The four previously inspected source hashes are evidence inputs, not a complete
bundle lock:

| Fhelipe file | SHA-256 |
| --- | --- |
| `backend/src/compiler.cc` | `DA1732396725CBE16D06E70CC1344F0CE2FF48B73F35086CA25C162686346BEA` |
| `backend/src/targets/compile.cc` | `5A3D61E24A41B29223357F080B3B109ABF4FC471F6B953CF9353912A922FB090` |
| `backend/src/layout_hoisting_pass.cc` | `DE71CB7FEE5F4844C6DFF27DFD8D08CF4D859AE1A0571F5CC1992603EB8FDA72` |
| `backend/src/dp_bootstrapping_pass.cc` | `BD057964E7260F72D604C115B434C859A05162DAE0359C5AB49A5D26A64D0A16` |

`PRE-O2-LOCK-EXIT` requires the complete adopted bundle lock, not only these
four rows.

### 4.3 Option and fallback truth table

The proposed `testdata/fhe_o2/governance/o-level-option-truth-table.json` must
enumerate every supported combination of:

- O0, O1, O2, and O3;
- bootstrap `auto`, `on`, `manual`, and `off`;
- baseline DP, MetaKernel, ReSBM, FHEFusion, HPAO, advanced-off, and their
  independently controlled off states;
- protected manual and pre-ReLU sites;
- feasible, unsupported, insecure, capacity-exceeded, and planner-failure
  results; and
- unrelated driver options that each phase must silently ignore.

The truth table must prove that O2 increment-off and advanced-off retain the
complete accepted baseline, including baseline layout passes and protected DP.
Whole-baseline fallback must be atomic. It cannot publish a mixed O0/O2 plan or
reuse stale placement from another layout.

### 4.4 Record ownership crosswalk

The proposed `testdata/fhe_o2/governance/baseline-record-crosswalk.json` must
assign one authoritative owner, schema/version source, verifier, serializer,
and compatibility policy to each of:

- `FHECompilationConfigIR`;
- `CKKSResolvedParameterIR`;
- `CKKSValueStateIR`;
- `CKKSScaleBootstrapPlanIR`;
- `EncryptedTensorLayoutIR`;
- logical key requirements;
- protected-site and baseline-DP provenance; and
- future O2 candidate and selected-plan extensions.

O0/shared records must be accepted before P2. P2 may extend them only after
`O2-O0Q-001=Qualified`; it cannot introduce baseline representation or make O0
depend on an O2-only identity. No physical `WT_*`, optional image, record, or
operator allocation is accepted without common/com compatibility review.

### 4.5 ACE rtlib and ANT lock

The proposed `testdata/fhe_o2/runtime/ace-rtlib-ant-lock.json` must bind:

- ACE rtlib revision `929e9b621f11bebbaa9ec1e215f4a52e3d07109b`;
- provider `LIB_ANT`, not `LIB_ACE`;
- generated-C include allowlist;
- exact exported-symbol allowlist;
- public-header hashes;
- build configuration and installed `FHErt_common` and `FHErt_ant` hashes;
- target triples and supported fixed ring dimensions;
- `CKKS_PARAMS`, Q/P/CRT, scale/level, bootstrap-profile, key, status, lifetime,
  and cleanup projection rules;
- compiler-query topology and any reviewed process/API boundary;
- client/test and server-evaluation lifecycle modes; and
- invalidation rules for any revision, header, symbol, build, or library drift.

The initial public-header hash set is:

| Header relative to `../../ace-compiler/` | SHA-256 |
| --- | --- |
| `fhe-cmplr/rtlib/include/common/rt_api.h` | `0025B6FB981F9578B15C9F05435DB0951E4B6C5E7547B398496F39CD0C5BC5C8` |
| `fhe-cmplr/rtlib/include/common/common.h` | `1DF878C051ADACCEABD3ECB57A88C5566E19F446AAE9C974FF9430FBD64E4AAB` |
| `fhe-cmplr/rtlib/include/rt_ant/rt_ant.h` | `AB4189F3970C2CEB80EC70018A31650E8ECC9C8102AB7BB55C3DB92A67E50C6E` |
| `fhe-cmplr/rtlib/include/rt_ant/ant_api.h` | `B9AE0016F701DE2FBE6D12BE59604DDF5C04B7666A90AFA44B8E1B671B20577A` |
| `fhe-cmplr/rtlib/ant/include/ckks/cipher.h` | `D5166E651AE61828841AFE8991A6AB0E73DAA36D161DE8A2C19344EBF4684469` |
| `fhe-cmplr/include/fhe/core/lib_provider.h` | `E1C3D250F62AC7E72FDB2E24E75C150412743D39FFBE745BC5144D9E26EC40EF` |

The generated-C include allowlist initially contains only reviewed public
surfaces reachable through:

```text
common/rtlib.h
common/rt_api.h
common/common.h
rt_ant/rt_ant.h
rt_ant/rt_api.h
reviewed ANT CKKS/LPOLY/POLY declarations reachable from rt_ant/rt_ant.h
```

The accepted manifest must replace the descriptive final row with an exact
header closure and exact symbol inventory. It must reject ACE AIR headers,
private implementation headers, C++ provider classes, STL types, and exceptions
from the generated-C boundary.

The canonical multiplication mapping is fixed:

```text
Open64 MulCC(two,two)
  -> ANT Mul_ciph3
  -> adjacent ANT Relin
  -> two-component result
```

Mapping canonical `MulCC` to fused `Mul_ciph` and then emitting another `Relin`
is invalid. The richer ANT surface remains available only for separately
accepted noncanonical use cases.

### 4.6 Decision register

| Decision ID | Required decision | Current state | Blocking effect |
| --- | --- | --- | --- |
| `PRE-DEC-MASTER` | Accept successor master wording and exact v0.10 disposition | Pending collaborator agreement | Blocks this exit gate |
| `PRE-DEC-DP` | Accept protected Fhelipe DP objective, domain, sites, and infeasibility behavior | Proposed | Blocks O0 baseline implementation contract |
| `PRE-DEC-OPTIONS` | Accept O-level, bootstrap-mode, advanced-off, and fallback truth table | Proposed | Blocks driver/default integration |
| `PRE-DEC-RECORDS` | Accept baseline versus O2 record ownership | Proposed | Blocks common/com handoff and P2 |
| `PRE-DEC-RTLIB` | Accept public ACE rtlib C surface and `LIB_ANT` | User-selected; master reconciliation pending | Blocks runtime contract closure |
| `PRE-DEC-QUERY` | Choose isolated resolver or approve a reviewed backend dependency | TODO; no direct `be.so` dependency authorized | Blocks S1.5B if exact compiler query needs it |
| `PRE-DEC-LIFECYCLE` | Accept client/test and server-evaluation context/key lifecycle | TODO; ANT remains selected | Blocks P1b release acceptance |
| `PRE-DEC-MD` | Keep HPAO-MD implementation disabled pending separate design | Proposed carry-forward | Blocks MD implementation only |

Every accepted decision records the date, approvers, exact replaced text,
source hashes, affected `FRZ-*` items, downstream milestones, and stale triggers.

## 5. Work Breakdown Structure

All verification commands in this section and Section 7 are proposed target
interfaces. This plan does not claim that the scripts or manifests exist.

| Task ID | FRZ mapping | Concrete change and proposed files | Dependencies | Test or review | Retained evidence | Owner and reviewers | Merge and exit rule |
| --- | --- | --- | --- | --- | --- | --- | --- |
| `PRE-W001` | `FRZ-07` | Produce an exact v0.10-to-successor conflict and authority inventory in `doc/adr/FHE-O0-O2-PLANNER-OWNERSHIP.md`; identify every master, O2-plan, and tracker statement affected | Collaborator direction; authority inputs | Manual two-editor diff plus `O2-PRE-AUTH-001` | Normalized diff, line/section crosswalk, hashes, review notes | Architecture editor; O0, O2, independent document reviewers | No unresolved or silently retained contradiction |
| `PRE-W010` | `FRZ-01` | Define the complete adopted Fhelipe pass order, defaults, support domain, disabled experiments, and baseline versus increment delta in the baseline manifest | `PRE-W001`; complete source inventory | `O2-PRE-FRZ01-001` | Pass/config/support manifest, source map, delta review | O0/Fhelipe owner; architecture and O2 interface reviewers | Every adopted pass has one role, order, source, control, and support row |
| `PRE-W020` | `FRZ-02` | Lock the complete Fhelipe source bundle, dependencies, build, license, toolchain, and future fixture/oracle ownership | `PRE-W010` | `O2-PRE-FRZ02-001` | Bundle/file hashes, dependency and license inventory, reproducible build specification | O0 source owner; build, legal, independent hash reviewers | No moving ref, missing dependency, placeholder hash, or undocumented patch |
| `PRE-W030` | `FRZ-03` | Resolve no-profitability, greedy/JIT, no-DP, and bootstrap-removal conflicts; specify protected/manual/pre-ReLU sites, constrained DP, objective, budget, shortcuts, tie order, and infeasibility | `PRE-W001`, `PRE-W010` | `O2-PRE-FRZ03-001`; bounded independent-enumerator design review, not execution | Accepted master/ADR text, policy table, oracle specification | Architecture and O0 placement owners; approximation, CKKS, O2 ReSBM reviewers | Accepted text forbids unrestricted removal and distinguishes hard from replaceable sites |
| `PRE-W040` | `FRZ-04` | Freeze the O0/O1/O2/O3 and auto/on/manual/off/advanced-off/fallback truth table; preserve unrelated driver-option propagation | `PRE-W030` | `O2-PRE-FRZ04-001` | Machine-readable truth table, normalized report, diagnostics inventory | Driver/config owner; O0, O2, integration reviewers | Every supported combination has one deterministic result and fallback owner |
| `PRE-W050` | `FRZ-05` | Accept one owner for each baseline record and define the later O2 extension boundary without naming an unreviewed image or opcode | `PRE-W001`, `PRE-W030`, `PRE-W040` | `O2-PRE-FRZ05-001` | Record crosswalk, versioning and compatibility obligations, common/com review | Common/com editor; O0, O2, WHIRL compatibility reviewers | No duplicate truth, missing verifier, or O0 dependency on O2-only records |
| `PRE-W060` | `FRZ-06` | Freeze baseline-versus-increment comparison profiles, identical-input controls, DP-on-layout discipline, measurements, and source/cost labels | `PRE-W010`, `PRE-W030`, `PRE-W040` | `O2-PRE-FRZ06-001` | Profile matrix, support mapping, pre-registration ownership and required metrics | Performance owner; O0, O2, independent statistics reviewers | Required single-factor cells cannot be waived or change two owners at once |
| `PRE-W070` | `FRZ-08` | Lock ACE rtlib revision, public headers, generated-C include/symbol allowlist, build and installed library hashes, `LIB_ANT`, records, operation mappings, and invalidation rules | Complete ACE rtlib public build and export inventory | `O2-PRE-FRZ08-001` | Rtlib/ANT lock manifest, header and symbol reports, build provenance | Runtime owner; Open64, build, security reviewers | Exact public closure is recorded; `LIB_ACE`, private headers, and ambiguous symbols reject |
| `PRE-W080` | `FRZ-09` | Decide compiler-query topology or record an explicit S1.5B blocker; review `be.so` consumers if direct linkage is proposed | `PRE-W070`; complete consumer inventory | `O2-PRE-FRZ09A-001` | Topology ADR section, process/API contract or complete link-closure report | Build/runtime owners; all `be.so` consumer maintainers | No direct dependency without explicit accepted review; unresolved state has a named blocker |
| `PRE-W090` | `FRZ-09` | Define client/test and server-evaluation context, key import/generation, secret isolation, reentrancy, concurrency, status, ownership, and cleanup | `PRE-W070`; ANT lifecycle inventory | `O2-PRE-FRZ09B-001` | Lifecycle state machine, key census contract, threat review, test specification | Runtime/security owners; O0 execution and independent security reviewers | May remain a named P1b blocker, but cannot be omitted or treated as solved |
| `PRE-W100` | `FRZ-07` | Apply accepted master/ADR wording and synchronize all source tracker links, milestone mappings, provider names, and stale rules | `PRE-W010` through `PRE-W090`; all decisions accepted as required for this gate | `O2-PRE-SYNC-001` | Accepted diffs, source hashes, cross-document traceability report | One editor per document; architecture and independent traceability reviewers | No contradictory accepted document or obsolete runtime/provider facade remains |
| `PRE-W110` | `F0` | Run the consolidated architecture-lock check and publish the signed decision without implementation changes | `PRE-W100`; all required manifest fields concrete | `O2-F0-001` | Complete governance bundle, `SHA256SUMS`, signed decision | Architecture owner and independent evidence reviewer | Result is exactly `Accepted`, `Rejected`, or `Unverified`; only `Accepted` closes the gate |

## 6. Proposed Commit and Review Sequence

No commit in this sequence may contain O0 or O2 implementation code, generated
compiler artifacts, or unrelated formatting.

| Commit group | Single semantic goal | Required review before merge |
| --- | --- | --- |
| `ARCH-1A` | Record the authority/conflict inventory and proposed master/ADR disposition | Architecture, O0, and O2 interface review |
| `ARCH-1B` | Freeze `FRZ-01` through `FRZ-04`: baseline, source/build, protected DP, and option truth table | O0, driver, approximation, CKKS, and independent source review |
| `ARCH-1C` | Freeze `FRZ-05` and `FRZ-06`: record ownership and comparison protocol | Common/com, WHIRL compatibility, performance, and O2 review |
| `ARCH-1D` | Freeze `FRZ-08`: ACE rtlib public surface, `LIB_ANT`, build, symbols, and canonical action mapping | Runtime, build, security, and Open64 review |
| `ARCH-1E` | Record `FRZ-09`: compiler-query topology and client/server lifecycle decision or explicit dependent blockers | Build-consumer and security review |
| `ARCH-1F` | Synchronize the accepted master, ADR, O2 plan, and source trackers | One editor per document and independent traceability review |
| `ARCH-1G` | Add only the signed consolidated acceptance bundle and hashes | Independent evidence review; no semantic change in this commit |

If collaborator agreement changes the selected direction, revise the governing
O2 plan and this stage plan before merging the affected semantic commit. Do not
hide a changed architecture decision in the acceptance-bundle commit.

## 7. Verification Matrix

Every command below is a proposed target command. A command must be checked in
with its manifest before it can be used as evidence. Any command or interface
change requires a reviewed plan update.

| Test ID and proposed exact command | Inputs and hashes | Expected result and first failure | Seed and bounds | Platform/provider | Protocol | Artifacts | Owner, reviewer, pass rule |
| --- | --- | --- | --- | --- | --- | --- | --- |
| `O2-PRE-AUTH-001`: `python3 osprey/common/com/tests/verify_fhe_o2_authority_delta.py --current-master doc/DSC_FHE_Compiler_Architecture_and_Integration_Plan_v0.10.md --successor testdata/fhe_o2/governance/accepted-master.json --adr doc/adr/FHE-O0-O2-PLANNER-OWNERSHIP.md --plan doc/FHE-O2-ACE-RESBM-METAKERNEL-INTEGRATION-PLAN.md --out test-artifacts/o2/O2-PRE-AUTH-001` | Recorded master hashes, successor hash, ADR and plan | Exact changed/retained/conflicting inventory; first unresolved conflict is `FHE-O2-ARCH-FREEZE-UNRESOLVED` | Deterministic, no random input | Provider-free, supported documentation host | 0 warmups, 1 run, 120 s | Normalized diff, authority graph, hashes, diagnostics | Architecture owner and independent document reviewer; zero unowned conflict |
| `O2-PRE-FRZ01-001`: `python3 osprey/common/com/tests/verify_fhelipe_baseline_manifest.py --source ../../fhelipe/fhelipe --manifest testdata/fhe_o2/baseline/fhelipe-baseline-lock.json --out test-artifacts/o2/O2-PRE-FRZ01-001` | Commit, complete source/bundle hashes, pass/config/support manifest | Exact commit, files, pass order, defaults, scope, and delta; first drift is `FHE-O2-SOURCE-LOCK-MISMATCH` | Deterministic complete manifest, no sampling | Provider-free source host | 0 warmups, 1 run, 120 s | Source inventory, pass graph, delta report, hashes | O0 source owner and independent hash reviewer; no moving ref or missing adopted pass |
| `O2-PRE-FRZ02-001`: `python3 osprey/common/com/tests/verify_fhelipe_reproducibility_lock.py --source ../../fhelipe/fhelipe --manifest testdata/fhe_o2/baseline/fhelipe-reproducibility-lock.json --out test-artifacts/o2/O2-PRE-FRZ02-001` | Complete dependency, build, license, toolchain, fixture-owner, and oracle-owner data | Every referenced input is immutable and licensed; first missing field is `FHE-O2-BASELINE-LOCK-INCOMPLETE` | Deterministic complete inventory | Frozen build platforms named by manifest | 0 warmups, 1 run per supported build specification, 300 s each | Dependency graph, license report, build specification, hashes | O0 build owner and build/legal reviewers; no placeholder or omitted dependency |
| `O2-PRE-FRZ03-001`: `python3 osprey/common/com/tests/verify_fhe_protected_dp_contract.py --adr doc/adr/FHE-O0-O2-PLANNER-OWNERSHIP.md --manifest testdata/fhe_o2/baseline/protected-dp-contract.json --out test-artifacts/o2/O2-PRE-FRZ03-001` | Accepted master/ADR hash, DP and protected-site contract | Objective, domain, budget, shortcuts, ties, hard sites, replaceable sites, and infeasibility are complete; uncontrolled removal fails with `FHE-O2-PROTECTED-SITE-CONTRACT-MISSING` | Enumerate the finite policy truth table; no graph execution | Provider-free | 0 warmups, 1 run, 120 s | Normalized contract and decision trace | O0 placement owner and independent CKKS/approximation reviewers; no ambiguous site owner |
| `O2-PRE-FRZ04-001`: `python3 osprey/common/com/tests/verify_fhe_o_level_truth_table.py --table testdata/fhe_o2/governance/o-level-option-truth-table.json --out test-artifacts/o2/O2-PRE-FRZ04-001` | Accepted decision hashes and complete option table | Every combination has one owner/result; advanced-off retains baseline; first contradiction is `FHE-O2-OPTION-CONTRACT-CONFLICT` | Exhaustive table enumeration | Provider-free | 0 warmups, 1 run, 120 s | Expanded table, uncovered combinations, diagnostics | Driver owner and O0/O2 reviewers; zero uncovered or duplicate outcome |
| `O2-PRE-FRZ05-001`: `python3 osprey/common/com/tests/verify_fhe_record_ownership.py --crosswalk testdata/fhe_o2/governance/baseline-record-crosswalk.json --out test-artifacts/o2/O2-PRE-FRZ05-001` | Accepted schema/version/owner inventory | One authority, verifier, serializer, and compatibility policy per field; first duplicate is `FHE-O2-RECORD-OWNER-CONFLICT` | All records and fields enumerated | Provider-free | 0 warmups, 1 run, 120 s | Ownership graph and compatibility obligations | Common/com editor and WHIRL compatibility reviewer; no duplicate or missing owner |
| `O2-PRE-FRZ06-001`: `python3 osprey/be/vho/tests/verify_fhe_o2_comparison_contract.py --profiles testdata/fhe_o2/governance/comparison-profiles.json --support testdata/fhe_o2/governance/support-scope.json --out test-artifacts/o2/O2-PRE-FRZ06-001` | Accepted option table, support rows, required profile definitions | Required baseline/single-factor/off/fallback rows and metrics are complete; first missing cell is `FHE-O2-COMPARISON-CELL-MISSING` | Exhaustive manifest rows | Provider-free contract check | 0 warmups, 1 run, 120 s | Expanded profile matrix, missing-cell report | Performance owner and independent statistics reviewer; zero missing required row |
| `O2-PRE-FRZ08-001`: `python3 osprey/be/vho/tests/verify_ant_rtlib_lock.py --ace-root ../../ace-compiler --manifest testdata/fhe_o2/runtime/ace-rtlib-ant-lock.json --out test-artifacts/o2/O2-PRE-FRZ08-001` | ACE commit, all public-header hashes, exported symbols, build and library hashes | Exact `LIB_ANT`, public closure, targets, records, and `Mul_ciph3` to adjacent `Relin`; first drift is `FHE-O2-RTLIB-LOCK-MISMATCH` | All allowlisted headers and symbols; no sampling | Every supported ACE rtlib/ANT build target | 0 warmups, 1 audit per target, 300 s | Header closure, symbol inventory, build and library hashes, operation map | Runtime owner and build/security reviewers; no private include, `LIB_ACE`, or duplicate Relin path |
| `O2-PRE-FRZ09A-001`: `python3 osprey/be/be/tests/verify_fhe_rtlib_dependency_topology.py --decision testdata/fhe_o2/runtime/compiler-query-topology.json --be-consumers testdata/fhe_o2/runtime/be-so-consumers.json --out test-artifacts/o2/O2-PRE-FRZ09A-001` | Topology decision; complete consumer inventory if direct dependency proposed | Isolated boundary is complete, or approved direct dependency proves all closure rows; unauthorized linkage fails with `FHE-O2-BE-DEPENDENCY-UNAPPROVED` | All declared builds and consumers | Every supported build configuration | 0 warmups, 1 audit per configuration, 600 s | Dependency graph, defined/undefined symbols, load reports, licenses | Build owner and all consumer maintainers; unresolved is an explicit S1.5B blocker, never implicit approval |
| `O2-PRE-FRZ09B-001`: `python3 osprey/be/vho/tests/verify_ant_lifecycle_contract.py --manifest testdata/fhe_o2/runtime/ant-lifecycle-contract.json --out test-artifacts/o2/O2-PRE-FRZ09B-001` | Context/key/lifetime state machine and threat model | Separate client/test and server-evaluation capabilities; server secret/decryptor is forbidden with `FHE-O2-SERVER-SECRET-FORBIDDEN` | All lifecycle states and failure transitions enumerated | `LIB_ANT` contract, no execution claim | 0 warmups, 1 contract run, 120 s | State machine, key census, threat review, diagnostic map | Runtime/security owners and independent security reviewer; TODO may block P1b but cannot disappear |
| `O2-PRE-SYNC-001`: `python3 osprey/common/com/tests/verify_fhe_o2_document_traceability.py --manifest testdata/fhe_o2/governance/document-lock.json --out test-artifacts/o2/O2-PRE-SYNC-001` | Accepted master, ADR, O2 plan, trackers, manifests, and exact hashes | All decisions and milestone links agree; first contradiction is `FHE-O2-DOCUMENT-TRACEABILITY-MISMATCH` | All listed documents and requirements | Provider-free | 0 warmups, 1 run, 120 s | Requirement graph, hashes, obsolete-reference report | Traceability editor and independent reviewer; no pending master field at exit |
| `O2-F0-001`: `python3 osprey/common/com/tests/verify_fhe_o2_architecture_lock.py --adr doc/adr/FHE-O0-O2-PLANNER-OWNERSHIP.md --design doc/DSC_FHE_Compiler_Architecture_and_Integration_Plan_v0.10.md --plan doc/FHE-O2-ACE-RESBM-METAKERNEL-INTEGRATION-PLAN.md --source-lock testdata/fhe_o2/source-lock.json` | All accepted governance artifacts and hashes | Exit 0 only when `FRZ-01` through `FRZ-09` are consistently closed or carry only explicitly permitted later blockers; unresolved gate issue fails with `FHE-O2-ARCH-FREEZE-UNRESOLVED` | Deterministic, one complete run | Supported governance host, provider contract fixed to ANT | 0 warmups, 1 run, 120 s | Complete governance bundle, normalized report, `SHA256SUMS`, signed decision | Architecture owner and independent evidence reviewer; no placeholder, mismatch, or implementation claim |

The `O2-F0-001` command is inherited from O2 plan version 1.6. The other
commands are proposed decomposition targets for this detailed plan. They must
not be reported as passing until the scripts, inputs, and outputs exist and are
reviewed.

## 8. Negative, Failure, and Compatibility Plan

### 8.1 Architecture and source negatives

- Reject a successor master whose recorded hash does not match its bytes.
- Reject an ADR that claims acceptance without naming approvers, date, exact
  replaced text, or affected milestones.
- Reject a moving Fhelipe branch, unpinned dependency, missing license, missing
  pass, undocumented default, or placeholder hash.
- Reject wording that preserves both the old greedy/JIT automatic policy and
  the selected default DP policy for the same mode.
- Reject a protected-DP contract that permits wholesale `RemoveBootstraps`
  before classifying hard/manual and replaceable automatic sites.
- Reject an option table in which advanced-off, increment-off, or fallback
  strips a frozen baseline pass or DP.
- Reject any O0 handoff that depends on P2 or an O2-only record.

### 8.2 Runtime and build negatives

- Reject an ACE rtlib revision, header, exported symbol, build flag, or library
  hash that differs from the accepted lock without a delta review.
- Reject `LIB_ACE` as a substitute for the selected `LIB_ANT` provider.
- Reject generated-C access to ACE AIR, private compiler headers, C++ provider
  classes, STL types, exceptions, or a non-allowlisted symbol.
- Reject an operation map that lowers canonical `MulCC` to `Mul_ciph` and also
  emits explicit `Relin`.
- Reject a contract that does not require exact runtime-created Q/P/CRT/profile
  agreement with persisted Open64 state.
- Reject any direct or source-level `be.so` rtlib dependency without the full
  accepted consumer and link-closure review.
- Reject a lifecycle contract that places secret key material or a decryptor in
  the server-evaluation process or any retained artifact.

### 8.3 Record and compatibility negatives

- Reject duplicate authoritative owners, host-sized enums, pointers, unchecked
  ranges, or an unversioned persisted extension.
- Reject a new WHIRL image, table, or operator named before common/com review.
- Reject any plan that makes provider records authoritative over Open64 config,
  parameter, layout, placement, or logical-key records.
- Reject a compatibility promise without old, feature-absent, new, corruption,
  binary-reopen, and `ir_b2a -st -src` obligations assigned to a later gate.

### 8.4 Sequencing and publication negatives

- Reject any O2 source branch, fixture, oracle, cost table, provider-free
  algorithm, or production artifact started before `O2-O0Q-001=Qualified`.
- Reject a PRE-O2-LOCK decision package that contains O0 or O2 implementation
  code.
- Reject a signed acceptance bundle with a pending master hash, placeholder,
  missing reviewer, missing tracker link, or contradictory accepted document.
- Treat a missing environment, inaccessible source bundle, or incomplete
  evidence as `Unverified`, not as an implicit pass.

## 9. Evidence and Retention

### 9.1 Governance evidence package

The proposed accepted package is retained under
`test-artifacts/o2/PRE-O2-LOCK-EXIT` and contains at least:

```text
authority-order.json
current-to-successor-master-diff.json
accepted-master.json
accepted-adr.json
fhelipe-baseline-lock.json
fhelipe-reproducibility-lock.json
protected-dp-contract.json
o-level-option-truth-table.json
baseline-record-crosswalk.json
comparison-profiles.json
support-scope.json
ace-rtlib-ant-lock.json
generated-c-header-closure.txt
generated-c-symbol-allowlist.txt
compiler-query-topology.json
be-so-consumers.json
ant-lifecycle-contract.json
document-lock.json
requirement-traceability.json
review-signatures.json
diagnostics.log
acceptance-decision.md
SHA256SUMS
```

This governance stage does not generate meaningful WHIRL. It must not fabricate
`.B` or `.T` evidence. The contracts assigned here require later O0 and O2 tests
to retain matching-stem `.B` and `.T`, with `.T` produced by
`ir_b2a -st -src input.B input.T`, after compiler artifacts actually exist.

The accepted governance bundle is retained for the repository lifetime. Failed
or incomplete review bundles are retained for at least 30 days with clear
`Rejected` or `Unverified` labels and cannot use the accepted directory name.

### 9.2 Result states

- `Accepted`: all required PRE-O2-LOCK exit conditions pass and the exact
  governance fingerprint is published.
- `Rejected`: at least one contract is contradictory, unsafe, or outside the
  accepted direction; the defect returns to its named owner.
- `Unverified`: evidence, authority, environment, or collaborator acceptance is
  incomplete. This state cannot start O0 under the new contract and cannot
  start any O2 work.

## 10. Exit Criteria

`PRE-O2-LOCK-EXIT` is `Accepted` only when all of the following are true:

1. The successor master and ADR are accepted, hash-locked, and explicitly
   reconcile every identified v0.10 conflict.
2. `FRZ-01` through `FRZ-09` have complete owners, evidence, and disposition.
   A later technical TODO may remain only where the governing O2 plan allows it,
   with a named owner, dependent gate, and no implicit authorization.
3. The complete Fhelipe source/pass/config/support/build/license/toolchain
   baseline is frozen with no placeholder or moving reference.
4. Protected/manual/pre-ReLU sites, constrained DP, objective, domain, budget,
   shortcut, tie, and infeasibility semantics are unambiguous.
5. O-level, bootstrap-mode, advanced-off, and fallback semantics are exhaustive
   and preserve the full baseline.
6. Every shared baseline record has one accepted owner and a defined
   compatibility path; O2 extension boundaries are explicit.
7. ACE rtlib public headers, exact symbols, build and installed libraries are
   locked; provider is `LIB_ANT`; canonical multiply is
   `Mul_ciph3 -> adjacent Relin`.
8. The compiler-query topology contains no unauthorized `be.so` dependency.
9. The lifecycle contract makes P1b block on proof of no server secret key or
   decryptor, reentrancy/concurrency behavior, and complete cleanup/status.
10. Master, ADR, O2 plan, and source trackers agree and their exact accepted
    hashes appear in the evidence bundle.
11. Every required verification row passes with no placeholder, skip, or
    changed command interface.
12. The signed report states only `PRE-O2-LOCK accepted`. It does not state that
    O0, O2 Stage 1, or O2 is implemented or accepted.

Acceptance of this gate authorizes the O0 owner to implement and accept the
external baseline contract. It does not authorize O2 implementation. The sole
O2 unlock remains `O2-O0Q-001=Qualified` in O2-E0.

## 11. Rollback, Invalidation, and Stop Rules

### 11.1 Rollback and invalidation

Governance changes are rolled back by reverting the specific semantic commit or
by accepting a superseding decision with a complete delta. Never partially
restore old option, provider, or ownership text. On invalidation:

1. mark the exact `FRZ-*` item and governance fingerprint stale;
2. stop dependent implementation or acceptance work;
3. preserve the old bundle as historical evidence, never as current input;
4. perform a targeted source, build, security, and document delta review;
5. update the master/ADR/O2 plan/trackers consistently; and
6. publish a new signed fingerprint before resuming.

If an invalidation occurs after O0 acceptance, that O0 evidence and any
`O2-O0Q-001` qualification bound to it are stale. O2 does not patch or silently
narrow the baseline to continue.

### 11.2 Immediate stop rules

Stop and return to architecture review if:

- the successor master has not been accepted but a task attempts to close the
  architecture conflict;
- one planner decision has two owners or no owner;
- a moving or unhashed source is required;
- the baseline cannot preserve hard/manual sites under the proposed DP policy;
- an option or fallback disables part of the frozen baseline;
- an O0 record depends on an O2-only identity;
- generated C needs a private ACE compiler surface;
- the runtime provider is changed from `LIB_ANT` without a new accepted decision;
- exact ANT parameters cannot match the persisted Open64 contract;
- a server-evaluation design requires a secret key or decryptor;
- direct `be.so` linkage appears without explicit dependency approval;
- a proposed record cannot support backward/feature-absent compatibility and an
  independent binary reopen; or
- any O2 implementation work begins before complete O0 qualification.

## 12. Requirement Traceability

| Requirement | Master or ADR status | O2 plan mapping | Work item | Verification | Retained artifact |
| --- | --- | --- | --- | --- | --- |
| Mature Fhelipe baseline | Successor master pending | `FRZ-01`, `FRZ-02`, Section 0.1 | `PRE-W010`, `PRE-W020` | `O2-PRE-FRZ01-001`, `O2-PRE-FRZ02-001` | Baseline and reproducibility locks |
| Protected baseline DP | Successor master and ADR pending | `FRZ-03`, Section 11.3 | `PRE-W030` | `O2-PRE-FRZ03-001` | Protected-DP contract and accepted diff |
| Baseline-preserving controls | Successor master and ADR pending | `FRZ-04`, Sections 0.5 and 15.1 | `PRE-W040` | `O2-PRE-FRZ04-001` | O-level option truth table |
| Unique record ownership | ADR pending | `FRZ-05`, Section 8 | `PRE-W050` | `O2-PRE-FRZ05-001` | Baseline record crosswalk |
| Comparable O0/O2 evidence | ADR pending | `FRZ-06`, Section 15 | `PRE-W060` | `O2-PRE-FRZ06-001` | Comparison profiles and support scope |
| Synchronized authority | Successor master pending | `FRZ-07`, Sections 0.3 and 2.1 | `PRE-W001`, `PRE-W100` | `O2-PRE-AUTH-001`, `O2-PRE-SYNC-001` | Accepted diffs and traceability graph |
| ACE rtlib public C plus ANT | Successor master pending | `FRZ-08`, Section 7 | `PRE-W070` | `O2-PRE-FRZ08-001` | Header/symbol/build/library lock |
| No unreviewed backend dependency | ADR decision pending | `FRZ-09`, Section 7.2 | `PRE-W080` | `O2-PRE-FRZ09A-001` | Topology decision and consumer audit |
| No server secret or decryptor | Runtime/security TODO pending | `FRZ-09`, Section 7.3 | `PRE-W090` | `O2-PRE-FRZ09B-001` | Lifecycle and key-separation contract |
| Complete O0 before O2 | User-selected; successor master pending | `O2-O0Q-001`, Sections 13 and 18 | `PRE-W100`, `PRE-W110` | `O2-PRE-SYNC-001`, `O2-F0-001` | Accepted governance fingerprint and signed decision |

Before this gate closes, every `pending` master or ADR cell in the traceability
data must be replaced by the accepted section, version, commit, and hash. This
document may retain historical wording, but the machine-readable accepted
traceability package may not.

## 13. Handoff

The PRE-O2-LOCK handoff contains only the accepted governance bundle and its
fingerprint. The O0 owner then implements the independent
`F1-PREP/P0/P1a/P1b/F1-IMPL/F1-ACCEPT` queue. The O2 owner may inspect planning
notes but may not create O2 fixtures, oracles, records, algorithms, or artifacts.

After the O0 producer publishes the complete candidate bundle, O2-E0 reopens
and independently qualifies it. Only `O2-O0Q-001=Qualified` permits O2-E1 to
begin.
