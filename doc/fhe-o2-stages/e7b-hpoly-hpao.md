# O2-E7B HPOLY/HPAO Detailed Execution Plan

Status: Proposed detailed plan; implementation is locked until all entry gates and the HPOLY representation decision are accepted
Plan version: 0.1
Date: 2026-09-15
Engineering stage: `O2-E7B`
Governing milestones: `S2.3`, `S2.4`
Exit gate: `O2-E7B-EXIT`
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
Recorded source baseline: `develop@eca97d4843aef03c50e5cb866f2f0e6c82e6b064`.
Recorded master commit: `ee1dc6382246c58f49a3097157a8c4e8ff2440c8`.
Recorded master Markdown SHA-256: `7EF644E9BECEFB541102514B83656C78756ECA71398A771A2A5160FC10B0CB25`.
Recorded master DOCX SHA-256: `0018769C26B5A0BCD1BDFCBD85AA97B8BAFEA381D7640FBB9E2E81B0022013D9`.
Pending ownership ADR: `doc/adr/FHE-O0-O2-PLANNER-OWNERSHIP.md`.
Proposed mandatory representation ADR: `doc/adr/FHE-HPOLY-REPRESENTATION.md`.
Accepted replacement master and ADR versions, commits, hashes, and exact authority sections must replace pending references before implementation.
### 1.2 HPAO source authority

The proposed source lock is:
paper `../../ace-paper/HPAO_AE.pdf` with SHA-256 `FAFCEE50E3978FECE2D847DD7B2623C71DB5B8E7CD79226B43917F87188D02FC`; fixed code
`origin/hpao@990e2289a866397e92c69ebe251adedba46cd44a`; guide `../../ace-paper-guide/05-HPAO.md` with SHA-256
`3F95DA6683C3C3D6E138C2C89E7CCD8481300FEBE743FF3529E1C67BD2A7BF5A`; ACE rtlib/ANT runtime input `../../ace-compiler@929e9b621f11bebbaa9ec1e215f4a52e3d07109b`;
reviewed source anchors `fhe-cmplr/poly/src/ckks2hpoly.cxx`, `fhe-cmplr/poly/include/ckks2hpoly.h`, `fhe-cmplr/include/fhe/poly/opcode_def.inc`,
`fhe-cmplr/poly/src/h2lpoly.cxx`, `fhe-cmplr/opt/src/op_fusion.cxx`, and `fhe-cmplr/opt/src/mdown_hoist_opt.cxx` at the fixed HPAO revision.
The fixed branch is evidence, not code to import blindly. Its physical AIR operators, memory operations, assertions, incomplete paths, option names, and ModDown
implementation do not automatically become Open64 contracts.
The source contains ModDown work, but this project keeps HPAO-MD `design-pending, implementation-disabled`. Source presence is not an accepted design, support
row, implementation requirement, or completion claim.
`S2.3` and `S2.4` must record paper-to-source-to-Open64 deltas for every accepted logical operation and HPAO rule. No guide, branch name, or unfixed source is
independent authority.
### 1.3 Ownership and review

The proposed owner is the Open64 HPOLY/HPAO stage owner.
Required reviewers are:
the E6 Stage 1 integration owner; the E2/common-com record and binary compatibility owner; the E3/E5 CKKS state, effect, immediate-Relin, and materialization
owners; the Open64 WHIRL operator, opcode, mapped-image, reader/writer, and printer owners; the dedicated HPAO-MU and Open64 SSA/HSSA analysis reviewers; the
HPAO-FM model-package, cache, and static-data reviewers; the HPAO-LM modular-arithmetic and word-safety reviewers; independent HPOLY semantic,
modular-arithmetic, and cost oracles; the ACE rtlib/ANT capability, ABI, lifecycle, and security reviewers; the build and `be.so` link-closure reviewer; the
E7A compatibility consumer and E8 integration consumer; and the evidence, failure-atomicity, and release reviewers.
One common/com editor owns any accepted physical record or logical operator change. The independent oracle owner cannot own or import production lowering,
equivalence, placement, profitability, selection, commit, or bit-bound helpers.
### 1.4 Entry gate and consumed evidence

Implementation starts only after all of the following are current and accepted:
`PRE-O2-LOCK-EXIT`; `O2-O0Q-001=Qualified`; `O2-E1-EXIT` through `O2-E6-EXIT`; the exact signed statement `O2 Stage 1 accepted`; independently reopenable
`S1.0-S1.9` and `S1.LAYOUT` evidence; matching `.B` and same-stem `.T` for E6 selected CKKS and materialized plans; fixed N, immediate Relin, protected-site,
state, key, and ANT fingerprints; the HPAO paper/source delta, operation catalog, diagnostics, cost inputs, support rows, and independent oracle interface
ready for review; and `O2-E7B-DECISION-001=Accepted`, selecting exactly one HPOLY representation.
E7B consumes E6 directly. E7A may run in parallel after E6. E7B does not wait for E7A, run FHEFusion, or consume an unaccepted fused graph to close E7B.
After both exits, E7B may test the accepted E7A handoff as a compatibility input. That test cannot change E7A or E7B selection and cannot replace E8.
### 1.5 Stale triggers

E7B stops and discards unpublished work after a change to:
any governing authority, accepted ADR, ownership, or completion wording; the O0 qualification or E6 input, decision, support, schema, or plan digest; the
accepted HPOLY persisted/transient representation decision; the HPAO paper, source, source delta, catalog, rule order, or diagnostic set; the logical operator
contract, physical carrier, allowed WHIRL level, or ABI; CKKS state, action, basis, level, scale, component, key-domain, or effect rules; immediate Relin or
bootstrap boundary semantics; fixed N, Q/P/CRT chain, security, bootstrap profile, or ANT capabilities; static package, payload, encoding, cache, or
transformed-data fingerprints; word-size, modular arithmetic, bit-bound, reduction, or cost-model rules; test bounds, generators, seeds, tolerances, metrics,
ties, or retention rules; or E2 transaction, mapped-image, reader/writer, `ir_b2a`, or final lowering behavior.
The stale report names the first mismatch, affected region/candidate/decision IDs, invalidated artifacts, responsible owner, and targeted requalification.
## 2. Objective and Delivery Boundary

E7B introduces and certifies one reviewed HPOLY middle-level contract between the finalized E6 CKKS plan and ANT LPOLY/POLY/runtime lowering. It implements only
legal non-bootstrap-region lowering and the independently controlled HPAO-MU, HPAO-FM, and HPAO-LM families.
The user-visible capability is target-aware polynomial optimization that may share redundant ModUp work, pre-encode eligible static plaintext weights and select
fast ct-by-pt multiplication, and defer modular reduction only while machine-word safety is proved.
E7B preserves CKKS semantic truth. It does not rediscover layout, scale, level, bootstrap placement, parameter selection, protected sites, or key domains.
E7B is not Stage 2 or final O2 certification. E8 owns cross-pass invalidation, FHEFusion-plus-HPAO composition, factorial ablations, `O2-ALL`, bare `-O2`, final
non-regression, and the O3 handoff.
## 3. Scope and Invariants

### 3.1 In scope

E7B includes:
a mandatory persisted-versus-transient representation review and ADR; a versioned logical HPOLY operation and static-state catalog; exact CKKS-to-HPOLY region
formation and lowering; exact HPOLY-to-ANT LPOLY/POLY/runtime lowering; bootstrap-opaque region barriers; preservation of the adjacent `MulCC -> Relin`
semantic boundary; effect, alias, lifetime, ownership, failure, and order contracts; deterministic HPAO candidate census, legality, cost, selection, and
commit; a dedicated HPAO-MU phase following the SSAPRE algorithmic model; HPAO-FM static package/cache and `poly.fast_mul` selection; HPAO-LM
forward/backward bit bounds and safe reduction placement; per-family off/on/auto behavior without inventing public option syntax; E7B-local HPOLY reanalysis
and downstream standard-call rematerialization; clear, exact modular, and decoded ANT validation; compatibility, corruption, failure injection, determinism,
and fallback; and reviewable `.B`/same-stem `.T` evidence according to the selected representation.
### 3.2 Explicit non-goals

E7B does not:
implement or modify FHEFusion, MetaKernel, ReSBM, baseline DP, or Stage 1; perform E8 cross-pass replanning or final O2 certification; implement HPAO-MD,
ModDown sinking, or ModDown merging; expose bootstrap internals or optimize across bootstrap calls; move, delay, duplicate, or remove the CKKS immediate-Relin
boundary; allow a three-component multiplication result to become ordinary SSA state; change requested N, Q/P/CRT truth, bootstrap profiles, security, or
policy; create a second CKKS parameter, layout, placement, or key truth; import ACE AIR as a second physical Open64 IR universe; route HPOLY through legacy
WOPT SSAPRE or modify that implementation; introduce physical scheduling, NUMA/GPU data placement, or transfer overlap; add an ACE rtlib/ANT dependency to
`be.so` without explicit approval; expose private ACE compiler headers or C++ objects to generated C; persist any secret key, decryptor, or secret-derived
private material; or claim HPAO paper equivalence, Stage 2 acceptance, O2 completion, or O3 readiness.
### 3.3 Inherited invariants

Binary WHIRL remains the frontend and compiler process boundary. Open64 owns CKKS-and-above semantic plans; ANT receives a checked projection. Requested N is
fixed before canonicalization and remains byte-identical. Every ordinary ciphertext SSA value has two components. `MulCC` creates only a transient
three-component edge consumed immediately by Relin. The ANT mapping remains `Mul_ciph3 -> Relin`, never `Mul_ciph` plus Relin. Bootstrap remains one opaque
ACE rtlib/ANT call with verified input/output state. Protected/manual/pre-ReLU boundaries and ReSBM decisions are read-only. Ordered state, key, context,
allocation, failure, and lifetime effects are preserved. Generated C uses only the accepted public ACE rtlib/ANT include/symbol allowlist. Server evaluation
receives no secret key or decryptor. A failure publishes no apparently valid partial `.B`, package, key, or plan. Every meaningful retained `.B` has a
same-stem `.T` from `ir_b2a -st -src`. No private FHE, O2, HPOLY, HPAO, LPOLY, or POLY operator reaches `whirl2c`.
### 3.4 Proposed logical operation review inventory

This inventory is not an operator allocation. The representation ADR and common/com review must accept exact names, versions, operands, results, kids,
attributes, effects, level, and lowering for each enabled row.

| Class | Proposed logical names | Required meaning |
| --- | --- | --- |
| Q-basis arithmetic | `poly.add`, `poly.sub`, `poly.mul`, `poly.rotate`, `poly.rescale` | Whole-polynomial operations with exact basis, level, scale, domain, and reduction state |
| Extended-basis arithmetic | `poly.add_ext`, `poly.sub_ext`, `poly.mul_ext`, `poly.rotate_ext` | Operations in the accepted QP or named extended basis with exact limb set |
| Key-switch structure | `poly.modup`, `poly.dotprod`, `poly.moddown` | Explicit decomposition, key product, and basis return outside bootstrap only |
| Basis lift | `poly.extend` | Checked Q-to-extended-basis lift without inventing CKKS state |
| Encoding | `poly.encode`, `poly.const_encode` | Runtime or parameter-bound static plaintext encoding |
| FM specialization | `poly.fast_mul` | Reviewed static ct-by-pt fast path with ordinary-reference equivalence |
| LM operations | reviewed no-mod arithmetic and explicit reduce IDs | Deferred reduction only under a proved word bound |
Private ACE physical spellings such as `PREC_PLAIN`, `MULP_FAST`, or fused ModDown operations may inform the delta review but are not public Open64 names.
### 3.5 Support boundary

| Dimension | Accepted E7B behavior | Rejected or fallback behavior |
| --- | --- | --- |
| Input | Current E6 finalized CKKS plan with exact state/action IDs | Stale, provisional, unverified, or partially materialized plan |
| Region | Single legal non-bootstrap CKKS region with complete entry/exit contract | Bootstrap body/crossing, unresolved call, exception, or effect boundary |
| State | Exact N, Q/P basis, level, scale, components, key domain, range/error | Unknown, mismatched, inferred, or provider-overridden state |
| Relin | Adjacent CKKS boundary retained; internal polynomial proof is subordinate | Delayed, duplicated, removed, or externally consumable ciph3 value |
| MU | Equivalent pure ModUp under complete state/decomposition identity | Different source/version/basis/level/scale/key/effect or illegal placement |
| FM | Immutable static plaintext, exact package identity, accepted ANT capability | Dynamic or stale payload, ct-by-ct input, unsupported or unprofitable fast path |
| LM | Exact conservative bound proves every intermediate fits | Unknown width, overflow, unsupported word model, or missing reduction |
| MD | `design-pending, implementation-disabled` | Any executable sinking, merging, or MD profitability claim |
| Calls/control | Accepted E6 direct-call summaries; region-local proof only | Cross-call speculation, recursion inference, unresolved alias/effect |
| Fallback | Unoptimized legal HPOLY lowering or complete E6 direct path | Mixed optimized HPOLY with stale state/package/materialization |
Scope labels remain `o0-handoff-required`, `shared-o0-o2-comparison-required`, `o2-core-only`, `o2-extension-only`, and `fallback-required` with their
governing O2 plan Section 4.2 meanings.
## 4. Implementation Dependencies and Compiler Pass Order

### 4.1 Implementation and build dependencies

The implementation dependency graph is:

    current O2-O0Q-001
      -> accepted E1-E5C services -> E6 Stage 1 acceptance
      -> HPAO source/catalog/support/diagnostic/oracle lock
      -> representation ADR and common-com compatibility review
      -> logical HPOLY state/effect/verifier contract
      -> CKKS-to-HPOLY and HPOLY-to-ANT lowering
      -> MU then FM then LM independent family implementation
      -> deterministic selection, commit, fallback, and evidence
      -> O2-E7B-EXIT
      -> E8 composition and final certification
E7A may proceed independently from E6. E7B has no implementation or build dependency on E7A. E8 consumes both accepted outputs later.
Proposed Open64 paths, subject to ownership and representation review, are:
`osprey/be/vho/fhe_hpoly_{catalog,region,state,lower,verify}.{h,cxx}`; `osprey/be/vho/fhe_hpao_{mu,fm,lm,candidate,select,commit,verify}.{h,cxx}`; the
proposed runners in Section 12 under `osprey/be/vho/tests/`; and `testdata/fhe_o2/{hpoly,hpao,hpao-cost}/`.
These are proposed locations, not existence or implementation claims.
### 4.2 Build and link impact

HPOLY construction, verification, MU, and LM analysis are provider-free. FM legality reads immutable parameter, payload, and capability manifests. Encrypted
execution reuses the accepted E5C generated-C/ANT route. Open64 WN, SSA/HSSA, dominance, value numbering, alias, effect, table, and memory-pool services are
reused only at reviewed stable extension points. Legacy WOPT SSAPRE source, options, output, and tests remain unchanged. No ACE AIR, compiler-private, C++
runtime, or frontend-builder dependency enters `be.so`. A persisted carrier requires a common/com and all-consumer compatibility review. A transient carrier
remains backend-private and may not allocate public operators.
The build gate rebuilds `be.so` and every known shared consumer, including `lw_inline`, plugins, and standalone tools, then compares defined/undefined symbols
and linked libraries. Any new unapproved dependency stops E7B.
### 4.3 Compiler pass order

The compiler order, distinct from build order, is:

    fixed-N application.fhe.B -> independent reopen -> canonical FHE graph
      -> selected layout -> canonical pre-ReSBM state -> baseline DP import
      -> optional ReSBM replacement -> verified post-state
      -> final CKKS parameters/security/keys/ANT capabilities
      -> form legal non-bootstrap CKKS regions
      -> CKKS-to-HPOLY lowering under selected representation
      -> HPAO-MU -> cleanup -> HPAO-FM -> cleanup -> HPAO-LM -> cleanup
      -> verify HPOLY entry/exit against unchanged CKKS contracts
      -> HPOLY-to-ANT LPOLY/POLY/runtime standard-call lowering
      -> erase every private operation -> final verifiers
      -> whirl2c -> generated C -> FHErt_common + FHErt_ant execution
Bootstrap nodes terminate HPOLY regions and lower through the existing opaque runtime path. No HPOLY visitor descends into a bootstrap implementation.
### 4.4 E7B reanalysis boundary versus E8

After an E7B commit, E7B rederives HPOLY def-use, SSA/HSSA facts, dominance, equivalence, basis/level/scale state, decomposition identity, bit widths,
lifetimes, package references, operation census, keys used by the polynomial lowering, ANT calls, generated C, and E7B cost.
E7B does not rerun FHEFusion, layout selection, baseline DP, ReSBM, or CKKS parameter resolution. A candidate that requires changing those authoritative
upstream decisions is illegal in E7B and is returned for future architecture or E8 consideration; it is not silently repaired.
E8 owns composition with E7A, cross-stage invalidation and replanning, legal pair/all-on ablations, final recosting, bare `-O2`, and the O3 handoff.
## 5. Mandatory HPOLY Representation Decision

### 5.1 Decision gate

Before any production HPOLY operator, carrier, lowering, or HPAO pass is coded, `doc/adr/FHE-HPOLY-REPRESENTATION.md` must select exactly one of:
`PERSISTED-HPOLY`: HPOLY is a versioned WHIRL-visible middle level; or `TRANSIENT-HPOLY`: HPOLY exists only inside one backend process between independently
inspectable pre- and post-HPOLY WHIRL checkpoints.
`Undecided`, `hybrid`, per-PU mixing, build-dependent selection, or silent promotion from transient to persisted blocks implementation.
The ADR records owner, reviewers, decision date, accepted master/O2 hashes, use cases, process boundary, debugging requirements, compatibility cost, rollback,
future migration, and rejected alternatives.
### 5.2 Common decision criteria

The review must answer:

1. Does a separate process need to consume or optimize HPOLY?
2. Must HPOLY survive a compiler checkpoint or only be reviewable?
3. Can the established logical DSL/operator model represent every HPOLY state?
4. Does the carrier preserve strict tree structure and direct operand kids?
5. Can level, scale, basis, decomposition, domain, and effects be complete?
6. Can old readers safely ignore feature absence and reject unknown versions?
7. What is the full mapped-image, printer, verifier, and lowering cost?
8. Can deterministic traces provide equivalent reviewability if transient?
9. Does the choice preserve final standard WHIRL and `whirl2c` compatibility?
10. Can the choice be rolled back without changing CKKS semantics?
The review must consult the relevant `WHIRL.pdf` operator, node layout, WHIRL level, type, mapping, binary-image, and ASCII-format requirements plus the current
source implementation. Historical ABI sizes are not assumed.
### 5.3 Persisted representation contract

If `PERSISTED-HPOLY` is selected, the same reviewed change set must define:
the allowed WHIRL level and exact logical operator registry entries; result and descriptor types, kid count/order, attributes, effects, and aliases; the
physical `OPR_DSL` or other accepted carrier mapping without leaking it; fixed-width records, invalid-zero IDs, checked first/count ranges, and size asserts;
operator, opcode, record, image, schema, and capability versions; mapped-image/ELF section allocation or a reviewed versioned existing image; common/com
builder, verifier, reader, writer, dumper, and stable logical printer; `ir_a2b` and `ir_b2a -st -src` behavior; old-reader, new-reader/old-file,
feature-absent, and unknown-version behavior; independent producer-exit/reopen and byte/normalized-text determinism; and corrupt
header/count/range/ID/hash/operator/state rejection before optimization.
The compatibility review explicitly covers every operator and record against:
current `OPERATOR`/`OPCODE` allocation and property tables; logical DSL registry versioning and physical escape-tag rules; WN kid layout, result/descriptor
completeness, maps, and source positions; binary image layout, section ownership, reader mapping, writer finalization; ASCII names, `ir_b2a`, `ir_a2b`,
unknown tools, and stripped-feature builds; WOPT/LNO/CG admission or mandatory pre-pass rejection; and every `be.so`, reader, writer, dumper, linker, and
standalone consumer.
A meaningful persisted artifact contains the selected HPOLY graph, logical operation names, stable value/region IDs, basis/level/scale/static/effect state, and
HPAO decisions. A comment-only marker, opaque blob without a standard reader, producer-memory dump, or post-lowering file is not HPOLY persistence.
Required persisted evidence includes:

    hpoly.input.ckks.B
    hpoly.input.ckks.T
    hpoly.unoptimized.B
    hpoly.unoptimized.T
    hpoly.optimized.B
    hpoly.optimized.T
    hpoly.materialized.o2.mid.B
    hpoly.materialized.o2.mid.T
Every `.T` uses the same stem and is produced with both `-st` and `-src`.
### 5.4 Transient representation contract

If `TRANSIENT-HPOLY` is selected, E7B must not allocate a `WT_*`, ELF section, public table, persistent operator, or persisted HPOLY record. It must retain:

    hpoly.pre.ckks.B
    hpoly.pre.ckks.T
    hpoly.post.std.B
    hpoly.post.std.T
    hpoly.transformation.t
    hpoly.transformation.json
The pre file is the exact finalized CKKS input. The post file is standard WHIRL after all HPOLY/HPAO private operations have lowered. Both `.T` files are
same-stem `ir_b2a -st -src` dumps.
The deterministic trace is a first-class review contract. It contains:
schema/algorithm version and all authority/source/capability fingerprints; input and output `.B` hashes and normalized graph digests; stable PU, region, CKKS
node/value/action, and transient HPOLY value IDs; region entry/exit states and every bootstrap barrier identity; each CKKS-to-HPOLY expansion in stable
order; every logical HPOLY operation, operand/result, and source lineage; basis, prime-set, level, scale, components, domain, decomposition, and effects;
MU/FM/LM candidate census, legality, rejection, cost, ties, and selected IDs; commit epochs, cleanup, invalidation/reanalysis, and final fixed point; static
package/cache references and word-bound/reduction proofs; every HPOLY-to-ANT/standard-call lowering and resulting stable node ID; and first diagnostics,
fallback identity, producer digest, and consumer verification.
Two clean runs with different allocator schedules and PU read order must produce byte-identical normalized JSON and text traces. Pointer values, timestamps,
temporary paths, hash-table order, and nondeterministic addresses are forbidden.
Transient HPOLY has no independent HPOLY reopen claim. The independent consumer instead reopens both WHIRL checkpoints and validates the transformation trace
against their digests and standard-call output.
### 5.5 Decision acceptance and change control

`O2-E7B-DECISION-001=Accepted` requires one chosen branch, a completed checklist, common/com and architecture signoff, exact evidence names, compatibility
rules, and rejected-alternative rationale.
A later representation change is a new versioned architecture decision. It invalidates all E7B implementation and evidence; it is not a mechanical refactor.
## 6. Interface, State, Effect, and Version Contracts

### 6.1 Sources of truth

| Input | Owner | E7B use | Mutation rule |
| --- | --- | --- | --- |
| `FHECompilationConfigIR` | O0/common-com | Fixed N and policy | Read-only |
| Final E6 CKKS plan | E5C/E6 | Region input and semantic oracle | Read-only |
| `CKKSResolvedParameterIR` | E5C | Q/P/CRT, levels, scales, profiles | Read-only |
| `CKKSValueStateIR` | E5C | Entry/exit and action state | Read-only |
| Bootstrap/protected sites | O0/E5B | Hard region boundaries | Never moved or opened |
| Logical/provider keys | E5C/runtime | Key domain and capability proof | Read-only; derived polynomial uses checked |
| HPOLY catalog | S2.3/E7B | Operation semantics and versions | Immutable and hash-bound |
| HPAO catalog | S2.4/E7B | MU/FM/LM rules and order | Immutable and hash-bound |
| ANT capability/cost | runtime/E1 | Lowering and profitability | Read-only and fingerprinted |
### 6.2 HPOLY value state

Each logical HPOLY value carries or references:
stable region, source CKKS action/value, and result IDs; immutable requested N and polynomial degree; exact Q, P, QP, or named basis ID and ordered prime
IDs; level and remaining-modulus-chain identity; symbolic/provider scale identity and exact or bounded bit length; ciphertext component index/count and
ct-by-ct or ct-by-pt domain; coefficient or NTT representation and reduction state; decomposition base/count/digit identity where relevant; key domain and
required rotation/relin/key-switch identity; static/dynamic plaintext classification and payload/package ID; conservative coefficient/intermediate bit-width
bounds; effect, alias, lifetime, ownership, failure, and source lineage; and state fingerprint derived from authoritative E5C inputs.
No HPOLY field may overwrite the E5C CKKS plan. Entry and exit verification projects HPOLY results back to the already selected CKKS action contract.
### 6.3 Proposed semantic APIs

These are proposed interfaces, not accepted declarations:

    FormHPOLYRegions(FinalCKKSPlanView, HPOLYRegionBuilder, DiagnosticSink)
    LowerCKKSToHPOLY(CKKSRegionView, HPOLYGraphBuilder, DiagnosticSink)
    VerifyHPOLYGraph(HPOLYGraphView, FinalCKKSPlanView, DiagnosticSink)
    EnumerateHPAOCandidates(HPOLYGraphView, HPAOCatalogView,
                            CandidateSetBuilder, DiagnosticSink)
    EvaluateHPAOCandidate(HPOLYGraphView, CandidateView,
                          CapabilityCostView, EvaluationBuilder,
                          DiagnosticSink)
    SelectAndCommitHPAO(HPOLYGraphView, CandidateSetView,
                        DecisionBuilder, HPOLYGraphBuilder, DiagnosticSink)
    LowerHPOLYToANT(HPOLYGraphView, ANTCapabilityView,
                    StandardWHIRLBuilder, DiagnosticSink)
    VerifyHPOLYBoundary(FinalCKKSPlanView, StandardWHIRLView,
                        HPOLYTraceView, DiagnosticSink)
Inputs are immutable. Failure returns no valid output ID. Builders publish only after full-region and all-PU verification in the E2 atomic transaction.
### 6.4 Effects, aliasing, lifetime, and order

Pure polynomial arithmetic is shareable only when every semantic state field matches. ModUp and Extend read immutable parameter/basis data and may
allocate/fail at runtime. DotProd reads exact evaluation-key/decomposition state and may allocate/fail. ModDown and reduction change basis or representation
state and are ordered. Static encoding reads immutable payload/config and publishes package data atomically. Fast multiply uses only the accepted immutable
package and provider capability. In-place ANT operations are effectful unless the public contract proves noalias. Context, key, package load, allocation,
free, and error status are observable effects. No candidate crosses a may-alias store, lifetime end, call, exception, or bootstrap. Failure order and cleanup
obligations are preserved by every committed rewrite.
### 6.5 Version and compatibility rules

Catalog, state, rule, trace, package, cost, and provider versions are explicit. Unknown required versions fail before region formation. Optional feature absence
preserves the accepted E6 direct materialization behavior.
Persisted mode additionally follows Section 5.3 binary versions. Transient mode versions only the internal contract and trace; it cannot claim binary HPOLY ABI.
## 7. Region Formation and Lowering Legality

### 7.1 Region boundaries

A region is a maximal accepted non-bootstrap sequence whose entry/exit CKKS states, control flow, key domain, effects, and lifetimes are complete.
A boundary is mandatory at:
every opaque bootstrap call and its exact input/output action; unresolved or unsupported call/return, exception, or indirect edge; context/key/package
creation or destruction; unsupported aliasing, mutation, or lifetime escape; incompatible parameter, basis, level, scale, component, or key domain; and any
point where the HPOLY verifier cannot prove a unique state.
Region formation is deterministic by stable PU, block, edge, node, and value IDs. It never deletes control-flow edges to form a more convenient graph.
### 7.2 CKKS-to-HPOLY lowering

Each supported CKKS operation has one cataloged expansion with exact entry and exit states. Rotate and Relin may expose ModUp/DotProd/ModDown key-switch
structure. Add, subtract, multiply, rescale, and mod-switch expose only the reviewed polynomial operations needed by their accepted ANT lowering.
Bootstrap has no HPOLY expansion. Its surrounding regions retain a barrier ID and its complete E5C state transition.
The `MulCC -> Relin` pair remains one indivisible CKKS semantic boundary. Polynomial lowering may expose the internal work of Relin but cannot:
schedule anything between MulCC and Relin; publish the three-component result to another consumer; share it with another ordinary operation; move Relin
across a branch, call, bootstrap, rescale, or failure point; or map MulCC to ANT `Mul_ciph` and emit an additional Relin.
### 7.3 HPOLY-to-ANT lowering

Each selected HPOLY operation maps to a reviewed public ANT LPOLY/POLY/runtime symbol or to standard WHIRL that contains only such calls and standard control.
The mapping records input/output ownership, aliasing, allocation, cleanup, status propagation, parameter/basis/prime ordering, static package identity, and
exact source operation lineage.
The final verifier rejects any remaining logical or physical FHE, O2, HPOLY, HPAO, LPOLY, or POLY operator before `whirl2c`.
## 8. HPAO Legality, Selection, and Commit

### 8.1 HPAO-MU contract

MU is a dedicated HPOLY phase based on the SSAPRE algorithmic model for value numbering, redundancy discovery, insertion/placement, and elimination.
Two ModUp expressions are equivalent only when source polynomial version, basis input/output, ordered primes, level, scale, component, representation,
decomposition, key domain, effects, failure contract, and lineage all match.
Placement requires dominance on every use path, availability, no killed source or parameter state, legal lifetime, no aliasing mutation, and no movement across
bootstrap, Relin boundary, context/key/package, call, exception, or failure.
Inserted computations and phis receive stable IDs and exact state. Critical edges are split only through accepted Open64 CFG services and reverified.
Legacy WOPT SSAPRE is neither invoked for HPOLY nor modified. A source and behavior diff must show its files, controls, and existing tests unchanged.
### 8.2 HPAO-FM contract

FM accepts only immutable static plaintext weights with exact content hash, element type, shape/layout, encode scale, level, Q/P/CRT, N, rounding, package
schema, key domain, source provenance, and ANT capability.
`poly.const_encode` is produced offline and committed atomically to a parameter-keyed model package/cache. The cache key includes every field that can change
encoded bytes or runtime interpretation.
`poly.fast_mul` is selected only for accepted ct-by-pt multiplication and only when exact modular/state equivalence with the ordinary ct-by-pt lowering passes.
Dynamic plaintext, ciphertext second operands, stale payload, mismatched parameters, unsupported layout/basis, missing package, or failed cost proof keeps the
ordinary operation.
Offline encoding time, package bytes, upload/load cost, runtime operations, memory, and cache reuse are separate reported cost components.
### 8.3 HPAO-LM contract

LM computes conservative forward and backward bounds for every no-mod add, subtract, multiply, and dot-product intermediate under the exact ANT word model.
The proof includes operand bounds, modulus bits, signedness, accumulation length, multiplication widening, carry/headroom, provider lazy-reduction primitive,
machine word width, and the first required normalized consumer.
A reduction may be deferred only if every intermediate and all control-flow merges fit strictly within the accepted bound. Unknown or saturated arithmetic,
unbounded loop count, mismatched word model, or unsupported primitive inserts an explicit reduction at the earliest safe point or rejects the candidate.
The exact modular result must match the always-reduce reference for every limb. Decoded tolerance cannot substitute for modular equality.
### 8.4 HPAO-MD gate

MD remains `design-pending, implementation-disabled`. Its catalog row has no executable matcher or selector. Enabling it requires a separate design that defines
extended-basis legality, control-flow placement, lifetime, pass order, cost, oracles, and an accepted plan amendment.
If any MD option or pattern is requested before that acceptance, E7B emits the frozen unsupported diagnostic. An off or disabled row is evidence; a fabricated
MD ablation is not.
### 8.5 Deterministic candidate selection

Candidates are immutable and enumerated by family ordinal, stable PU/region/block/node/value IDs, rule ID, operand IDs, insertion edges, and source lineage.
Pointer order, address, allocation, hash-table iteration, worker order, and PU read order never affect enumeration.
Legality is evaluated before profitability. Costs are nonnegative integer nanoseconds with checked arithmetic and complete independently counted components.
Unknown frequency, missing cost, or saturation disqualifies a profitability claim.
The proposed stable tuple is:

    (predicted_total_ns, modular_reduction_count, modup_count,
     runtime_encode_count, extended_basis_limb_ops, peak_live_bytes,
     package_bytes, family_ordinal, rule_ordinal, region_id,
     source_value_id, insertion_edge_ids, candidate_id)
The accepted S2.4 catalog may refine the tuple before implementation. Once accepted, every field and tie reason is recorded and versioned.
### 8.6 Atomic commit and fixed point

One selected candidate bundle is applied to a cloned HPOLY graph. State update, SSA/HSSA repair, cleanup, package update, effect/lifetime verification, boundary
verification, decision logging, and graph replacement form one transaction.
Failure preserves the prior accepted epoch. No intermediate ModUp placement, fast multiply, no-mod operation, package, or partially reduced graph is published.
Each commit strictly decreases an accepted lexicographic potential containing predicted cost and the target redundant-operation count. Seen normalized graph
digests detect cycles. A second E7B run on accepted output makes zero commits and produces the same graph, trace/records, decisions, and standard-call output.
### 8.7 Independent oracle

The independent oracle consumes raw CKKS regions, parameters, keys, payloads, word model, public ANT capability/cost data, and rule catalog. It imports no
production region builder, lowerer, equivalence, dominance placement, updater, bit-bound, cost, selector, commit, or cleanup helper.
It independently recomputes:
CKKS-to-HPOLY expansions and region barriers; every HPOLY state and entry/exit projection; MU equivalence, availability, placement, and eliminated/inserted
operations; FM encoded package identity, ordinary and fast modular results, and costs; LM forward/backward bounds, reductions, and exact modular results;
candidates, rejected reasons, stable tuple, winner, commits, and fixed point; HPOLY-to-ANT calls, ownership/lifetime cleanup, and first diagnostic; and persisted
records or transient traces according to the selected ADR.
Bounded exhaustive cases use primes 1..6, fanout 1..4, dot length 1..8, and local graphs up to 10 nodes. Random coverage uses 2000 cases and seed `0x4850414f`.
Production-oracle dependency audit is a hard gate.
## 9. Invalidation, Diagnostics, and Fallback

### 9.1 E7B-local invalidation

Any committed HPOLY change invalidates all derived HPOLY equivalence, SSA/HSSA, dominance, liveness, effects, basis state, bit bounds, lifetimes, package/cache,
candidate/cost data, provider-call mappings, materialized WHIRL, generated C, and E7B metrics before the next decision.
Upstream E6 configuration, layout, placement, bootstrap, CKKS state, parameters, and key domains remain immutable. If a rewrite would invalidate one of them,
the candidate is rejected rather than triggering hidden upstream replanning.
### 9.2 Proposed diagnostics

E1/S2.3/S2.4 own final spellings. Proposed purposes are:

| Purpose | Proposed code |
| --- | --- |
| Representation undecided or stale | `FHE-O2-HPOLY-REPRESENTATION-UNRESOLVED` |
| Persisted compatibility failure | `FHE-O2-HPOLY-BINARY-INCOMPATIBLE` |
| Transient trace mismatch | `FHE-O2-HPOLY-TRACE-MISMATCH` |
| Stale E6/state/provider input | `FHE-O2-HPOLY-STALE` |
| Unsupported CKKS operation | `FHE-O2-HPOLY-UNSUPPORTED-CKKS-OP` |
| Illegal bootstrap crossing | `FHE-O2-HPOLY-BOOTSTRAP-BARRIER` |
| Unknown or mismatched state | `FHE-O2-HPOLY-STATE-MISMATCH` |
| Immediate Relin violation | `FHE-O2-HPOLY-RELIN-BOUNDARY` |
| Illegal MU equivalence/placement | `FHE-O2-HPAO-MU-ILLEGAL` |
| FM static package mismatch | `FHE-O2-HPAO-FM-PACKAGE-MISMATCH` |
| FM unsupported or unprofitable | `FHE-O2-HPAO-FM-NOT-SELECTED` |
| LM word safety not proved | `FHE-O2-HPAO-LM-WORD-UNSAFE` |
| MD requested while disabled | `FHE-O2-HPAO-MD-DESIGN-PENDING` |
| Missing or saturated cost | `FHE-O2-HPAO-COST-UNUSABLE` |
| Nondeterminism or cycle | `FHE-O2-HPAO-NONDETERMINISTIC` |
| Atomic publication failure | `FHE-O2-HPAO-PUBLISH-FAILED` |
| Private operation at whirl2c | `FHE-O2-HPOLY-FINAL-BOUNDARY` |
Authority/version/hash failures precede region and semantic failures. Region and state legality precede family legality. Safety precedes profitability. The
stable first diagnostic cannot vary with traversal or thread order.
### 9.3 Fallback and control behavior

Public option syntax is not invented here. The accepted control ADR must map:
HPOLY off to the complete accepted E6 direct CKKS-to-ANT path; HPOLY on to required legal lowering, failing if the contract is unsupported; HPAO family off to
unoptimized legal HPOLY lowering; HPAO family auto to legal profitable selection or that same unoptimized path; MD to disabled with its exact diagnostic; and
advanced off to the complete F0-authorized qualified baseline.
An illegal or unprofitable candidate preserves the current coherent HPOLY epoch. A stage-level failure before commit preserves the complete E6 direct path when
fallback is authorized. No fallback combines optimized HPOLY nodes with stale packages, states, calls, keys, or materialized WHIRL.
## 10. Detailed Work Breakdown

| Task | Concrete change | Component/dependency | Test or artifact | Owner/reviewer | Merge or exit rule |
| --- | --- | --- | --- | --- | --- |
| `E7B-W01` | Freeze entry authority and fingerprints | Governance/E6 | Entry lock report | Stage/architecture | Current E6 only |
| `E7B-W02` | Freeze HPAO paper/source/Open64 delta | S2.3/S2.4 | Source delta | Paper-source/HPOLY | No unmapped claim |
| `E7B-W03` | Freeze logical operation catalog and support | S2.3 | Catalog positives/negatives | HPOLY/common-com | No implicit operator |
| `E7B-W04` | Decide persisted versus transient | Representation ADR | `O2-E7B-DECISION-001` | Architecture/common-com | Before code |
| `E7B-W05` | Complete selected carrier compatibility contract | Common-com/backend | Branch evidence checklist | Record/build | One physical owner |
| `E7B-W06` | Define stable HPOLY state and identities | Semantic contract | State verifier | CKKS/HPOLY | No second CKKS truth |
| `E7B-W07` | Implement deterministic region formation | E6 CKKS input | Region/barrier oracle | Lowering/CKKS | Bootstrap opaque |
| `E7B-W08` | Implement CKKS-to-HPOLY lowering | Lowering | Expansion oracle | Lowering/crypto | Exact state projection |
| `E7B-W09` | Implement HPOLY verifier | Verifier | Malformed/state cases | Verifier/independent | Fail before HPAO |
| `E7B-W10` | Implement selected persistence or trace writer | ADR branch | Reopen or trace tests | Record/evidence | Deterministic review |
| `E7B-W11` | Build MU equivalence and availability | MU/SSA | Exact redundancy cases | MU/SSA reviewer | Full state equality |
| `E7B-W12` | Build MU placement/elimination phase | MU/dominance | Path/phi/critical edge | MU/Open64 continuity | Legacy SSAPRE unchanged |
| `E7B-W13` | Build FM static encoding contract | FM/package | Payload/package tests | FM/package | Complete cache key |
| `E7B-W14` | Build FM fast-multiply legality | FM/ANT | Ordinary-reference oracle | FM/runtime | Exact modular equality |
| `E7B-W15` | Build LM forward bit-bound analysis | LM | Boundary vectors | LM/modular reviewer | Conservative proof |
| `E7B-W16` | Build LM backward demand/reduction insertion | LM | Always-reduce reference | LM/oracle | Word safe or reduce |
| `E7B-W17` | Keep MD catalog disabled | Design gate | Unsupported diagnostic | MD owner/stage | No executable code |
| `E7B-W18` | Implement immutable candidate census | Candidate records/trace | Enumeration/dedup | Selector/record | All candidates visible |
| `E7B-W19` | Implement integer costs and stable ties | Cost/selector | Component and tie tests | Performance/oracle | No unknown cost |
| `E7B-W20` | Implement atomic commit and cycle guard | E2 transaction | Failure injection | Reliability/oracle | Prior epoch intact |
| `E7B-W21` | Build independent semantic/modular oracle | Test-only | Exhaustive/random | Independent/auditor | No production imports |
| `E7B-W22` | Implement HPOLY-to-ANT lowering | E5C public runtime | Symbol/call/lifetime audit | Runtime/HPOLY | Public allowlist only |
| `E7B-W23` | Verify Relin and bootstrap boundaries | CKKS/HPOLY | Boundary negatives | CKKS/crypto | Zero movement/expansion |
| `E7B-W24` | Invalidate and rerun E7B-local analyses | Driver/HPOLY | Stale injection | Integration/all owners | No stale consumer |
| `E7B-W25` | Certify family-off and HPOLY-off fallback | Compatibility | Digest/call comparison | Reliability/E6 | Coherent whole path |
| `E7B-W26` | Execute MU/FM/LM focused profiles | Validation/ANT | Modular/decoded/cost | Runtime/performance | Family gates pass |
| `E7B-W27` | Audit `be.so`, headers, symbols, and secrets | Build/security | Boundary scan | Build/security | Zero violation |
| `E7B-W28` | Produce selected branch `.B`/`.T` and reports | Evidence | Bundle audit | Evidence/reviewer | Complete retention |
| `E7B-W29` | Validate optional E7A compatibility input | E7A handoff | Read-only compatibility | E7A/E7B | No joint selection |
| `E7B-W30` | Sign E8 handoff and E7B decision | Exit | Consumer dry run | E7B/E8 | Reopen without inference |
## 11. Proposed Commit and PR Sequence

Commits map to governing groups `S2-4` and `S2-5`:

1. `e7b-source-contract`: HPAO source lock, deltas, support, diagnostics, and failing tests.
2. `e7b-representation-adr`: accepted persisted/transient decision and compatibility checklist.
3. `e7b-logical-contract`: logical operation, state, effect, region, and verifier contract.
4. `e7b-carrier-contract`: only the selected carrier path and its compatibility tests.
5. `e7b-ckks-to-hpoly`: deterministic non-bootstrap region formation and lowering.
6. `e7b-hpoly-to-ant`: public LPOLY/POLY/runtime mapping and lifetime/error handling.
7. `e7b-mu-equivalence`: polynomial value numbering and complete equivalence only.
8. `e7b-mu-placement`: dedicated SSAPRE-model placement/elimination and CFG repairs.
9. `e7b-fm-package`: static encoding package/cache identity and atomic publication.
10. `e7b-fm-fast-mul`: ordinary-reference legality, capability, and cost selection.
11. `e7b-lm-bounds`: independent forward/backward bit-width analysis.
12. `e7b-lm-reduction`: no-mod selection, reduction insertion, and modular oracle.
13. `e7b-selection-commit`: candidate census, costs, stable tuple, transaction, fixed point.
14. `e7b-independent-oracle`: test-only exhaustive/random oracle and dependency audit.
15. `e7b-compatibility-fallback`: HPOLY/family off, corruption, rollback, and stale cases.
16. `e7b-boundary-audits`: bootstrap, Relin, final WHIRL, ABI, `be.so`, and secret scans.
17. `e7b-acceptance-evidence`: runners, artifact schema, and decision template only.
Each commit has one semantic goal. MU, FM, and LM remain separate. The selected representation commit cannot contain HPAO implementation. The independent oracle
is separate from production helpers. The acceptance commit contains no algorithm change. Generated run artifacts are not committed unless accepted as goldens.
## 12. Proposed Verification Matrix

All commands below are proposed future exact interfaces. They do not claim that the scripts, fixtures, targets, or artifacts exist, and this planning task does
not execute them.

| Test ID | Proposed exact command | Inputs and bounds | Exact pass rule | Retained artifacts |
| --- | --- | --- | --- | --- |
| `O2-E7B-DECISION-001` | `python3 osprey/common/com/tests/verify_hpoly_representation_decision.py --adr doc/adr/FHE-HPOLY-REPRESENTATION.md --master doc/DSC_FHE_Compiler_Architecture_and_Integration_Plan_v0.10.md --o2-plan doc/FHE-O2-ACE-RESBM-METAKERNEL-INTEGRATION-PLAN.md --execution-index doc/FHE-O2-DETAILED-EXECUTION-PLAN.md --artifacts test-artifacts/o2/O2-E7B-DECISION-001` | Accepted authority; both branch checklists; one selected branch | Exactly one representation selected; all selected-branch compatibility/evidence fields complete; no implementation files predate acceptance | Decision report, reviews, hashes, rejected alternative |
| `O2-S23-001` | `python3 osprey/be/vho/tests/run_hpoly_contract.py --build-dir build --manifest testdata/fhe_o2/hpoly/SHA256SUMS --artifacts test-artifacts/o2/O2-S23-001` | Rotate/KeySwitch/Mul/adjacent-Relin/ModUp/ModDown/DotProd and bootstrap barriers; chain 1..16; fanout 1..8 | Basis/level/scale/static/effect/lowering exact; bootstrap opaque; Relin unchanged; decoded `1e-4/1e-6`; exact first diagnostic | CKKS/HPOLY/POLY evidence, C, outputs, matching `.B`/`.T` required by selected branch |
| `O2-S24-001` | `python3 osprey/be/vho/tests/run_hpao_rule_contract.py --build-dir build --catalog testdata/fhe_o2/hpao/rules.json --weights testdata/fhe_o2/hpao/ant-v1.json --manifest testdata/fhe_o2/hpao/SHA256SUMS --artifacts test-artifacts/o2/O2-S24-001` | MU/FM/LM; primes 1..16; fanout 1..8; dot length 1..64; seed `0x4850414f` | Exact rule/state/modular/count results; decoded `1e-4/1e-6`; legacy SSAPRE unchanged; MD disabled | Rules, proofs, counters, package/cost hashes, `.B`/dumps/C/output, MD disposition |
| `O2-E7B-001-P` | `python3 osprey/be/vho/tests/run_e7b_hpoly_gate.py --build-dir build --representation persisted --manifest testdata/fhe_o2/hpoly/SHA256SUMS --mode roundtrip --artifacts test-artifacts/o2/O2-E7B-001-P` | Selected only for persisted ADR; 1..8 PUs; chain 1..16; two clean producers | Meaningful HPOLY binary survives producer exit/reopen; old/new/feature-absent/corruption rules exact; byte and text deterministic | Input/unoptimized/optimized/materialized matching `.B`/`.T`, reopen/corruption reports |
| `O2-E7B-001-T` | `python3 osprey/be/vho/tests/run_e7b_hpoly_gate.py --build-dir build --representation transient --manifest testdata/fhe_o2/hpoly/SHA256SUMS --mode trace-boundary --artifacts test-artifacts/o2/O2-E7B-001-T` | Selected only for transient ADR; 1..8 PUs; chain 1..16; two allocator/PU orders | Pre/post WHIRL reopens; normalized trace accounts for every transform and is byte-identical; no binary HPOLY ABI claimed | Pre/post matching `.B`/`.T`, deterministic `.t`/JSON trace, consumer report |
| `O2-E7B-002` | `python3 osprey/be/vho/tests/run_e7b_hpoly_gate.py --build-dir build --representation-from doc/adr/FHE-HPOLY-REPRESENTATION.md --manifest testdata/fhe_o2/hpao/SHA256SUMS --mode selection-commit --artifacts test-artifacts/o2/O2-E7B-002` | MU/FM/LM off and isolated; exhaustive <=10 nodes; random 2000 seed `0x4850414f` | Candidate census, legality, bounds, cost components, tuple, winner, commits, fixed point exact against independent oracle | Candidates, rejections, ties, commit log, oracle, fixed-point report |
| `O2-E7B-003` | `python3 osprey/be/vho/tests/run_e7b_hpoly_gate.py --build-dir build --representation-from doc/adr/FHE-HPOLY-REPRESENTATION.md --manifest testdata/fhe_o2/hpao/SHA256SUMS --mode failure-compatibility --artifacts test-artifacts/o2/O2-E7B-003` | Failure at every form/lower/analyze/package/commit/publish step; corrupt/stale fields | First diagnostic exact; no partial valid output; selected rollback/fallback coherent; MD disabled | Rejected inputs/images/traces, failure logs, fallback digests, normalized diffs |
| `O2-E7B-004` | `python3 osprey/be/vho/tests/run_e7b_hpoly_gate.py --build-dir build --representation-from doc/adr/FHE-HPOLY-REPRESENTATION.md --manifest testdata/fhe_o2/hpao/SHA256SUMS --mode ant-execution --artifacts test-artifacts/o2/O2-E7B-004` | Accepted E6 plan and ANT manifest; MU/FM/LM isolated; bootstrap cases | Exact modular reference and decoded gates; public calls, ownership, cleanup, state, keys, and package hashes exact | Standard `.B`/`.T`, generated C, symbols, packages, outputs, logs, metrics |
| `O2-E7B-005` | `python3 osprey/be/vho/tests/run_e7b_hpoly_gate.py --build-dir build --representation-from doc/adr/FHE-HPOLY-REPRESENTATION.md --manifest testdata/fhe_o2/hpao/SHA256SUMS --mode boundary-audit --artifacts test-artifacts/o2/O2-E7B-005` | Complete build, final WHIRL, generated C, server artifact, symbol inventory | Fixed N; bootstrap opaque; immediate Relin unique; no private final op, secret/decryptor, private ABI, or unauthorized `be.so` dependency | N/Relin/bootstrap audits, includes/symbols/libraries, secret scan, final boundary report |
| `O2-E7B-006` | `python3 osprey/be/vho/tests/run_e7b_hpoly_gate.py --build-dir build --representation-from doc/adr/FHE-HPOLY-REPRESENTATION.md --manifest testdata/fhe_o2/hpao/SHA256SUMS --mode e7a-compatibility --artifacts test-artifacts/o2/O2-E7B-006` | Accepted E7A handoff after both exits; no joint cost selection | HPOLY boundary recognizes E7A-rebuilt CKKS plan without changing either decision; failures are compatibility-only | Read-only handoff check, digests, no-reselection proof |
Persisted-mode exact dump commands are proposed as:

    ir_b2a -st -src test-artifacts/o2/O2-E7B-001-P/hpoly.unoptimized.B test-artifacts/o2/O2-E7B-001-P/hpoly.unoptimized.T
    ir_b2a -st -src test-artifacts/o2/O2-E7B-001-P/hpoly.optimized.B test-artifacts/o2/O2-E7B-001-P/hpoly.optimized.T
Transient-mode exact dump commands are proposed as:

    ir_b2a -st -src test-artifacts/o2/O2-E7B-001-T/hpoly.pre.ckks.B test-artifacts/o2/O2-E7B-001-T/hpoly.pre.ckks.T
    ir_b2a -st -src test-artifacts/o2/O2-E7B-001-T/hpoly.post.std.B test-artifacts/o2/O2-E7B-001-T/hpoly.post.std.T
`O2-E7B-DECISION-001`, structural portions of `O2-S23-001`, and `O2-E7B-001/002/003/005/006` use zero warmups, one run, and a 1200-second timeout. Roundtrip
tests use two clean producers and independent consumers.
`O2-S24-001` and focused execution use five warmups and thirty paired samples, a 20-minute timeout per sample, and no outlier deletion. Exact modular checks run
before decoded/performance checks.
If `ir_b2a -src` is unavailable, the representation test is `Unverified`. `-src` is not omitted. Lower-case `.t` trace names avoid `.T` collisions.
### 12.1 Numerical, modular, and cost gates

Pure double reference checks use `abs<=1e-12` and `rel<=1e-12`. Focused decrypted results use elementwise `abs<=1e-4` and `rel<=1e-6`. Required-zero decoded
slots use absolute value at most `1e-8`. NaN or Inf always fails. Polynomial modular outputs match exactly limb by limb. Every observed decoded error remains
within its persisted E6 error bound. HPAO does not weaken range, precision, or security proofs. Cost weights are nonnegative integer nanoseconds with checked
sums. Operations with measured median at least 1 microsecond have holdout median absolute percentage error at most 20 percent and p95 at most 35 percent.
Smaller operations have absolute prediction error at most 2 microseconds. Wrong-order rate is at most 10 percent under governing separation rules. Every
selected candidate has an independently explained component census. Offline FM encoding, package bytes, load/upload, and runtime savings are separate.
E7B reports `HPOLY-NOOPT`, `HPAO-MU`, `HPAO-FM`, and `HPAO-LM` under identical E6 inputs and parameters. Any E7B-local combined stress row is informational. E8
owns HPAO composition with FHEFusion and final non-regression.
## 13. Negative, Failure, Compatibility, and Performance Cases

| Case | Required behavior | Publication |
| --- | --- | --- |
| Entry/E6/source/ADR hash changed | Stop and target requalification | None |
| Representation missing, hybrid, or disagrees with build | Reject before code/use | None |
| Persisted operator/record compatibility incomplete | Stop common-com review | None |
| Transient trace omits a transform or digest | Reject transformation | No post result |
| Fixed N missing or changed | Reject before region formation | None |
| Q/P/CRT, level, scale, or key domain mismatch | Reject region | Prior E6 path only |
| Bootstrap body or crossing requested | Reject region/candidate | Prior epoch only |
| Unknown CKKS operation | Reject or whole-region bypass per support row | No partial region |
| Relin missing, delayed, duplicated, or externally consumed ciph3 | Reject | None |
| MulCC maps to `Mul_ciph` plus explicit Relin | Reject materialization | None |
| MU state/decomposition differs | Not equivalent | Prior epoch only |
| MU placement crosses effect, alias, call, or lifetime | Reject candidate | Prior epoch only |
| MU placement lacks dominance/availability | Reject candidate | Prior epoch only |
| Legacy WOPT SSAPRE source/control/output changes | Stop continuity review | None |
| FM plaintext is dynamic or ciphertext | Keep ordinary multiply | Prior epoch only |
| FM payload/config/package/cache hash stale | Reject package/candidate | Prior epoch only |
| FM capability absent or candidate unprofitable | Keep ordinary multiply | Prior epoch only |
| FM fast result differs modulo any prime | Reject family/stage | None |
| LM bound unknown or arithmetic saturates | Insert safe reduction or reject | Prior epoch only |
| LM intermediate reaches/exceeds word bound | Insert earlier reduction | Safe graph only |
| LM no-mod result differs from always-reduce | Reject family/stage | None |
| MD option or pattern requested | Emit design-pending diagnostic | No MD result |
| Unknown frequency or missing cost | Disqualify profitability | Prior epoch only |
| Candidate tie | Use complete accepted stable tuple | One deterministic winner |
| Graph digest repeats | Diagnose cycle and roll back bundle | Prior epoch only |
| Commit/cleanup/package publication fails | Atomic rollback | Prior epoch only |
| Stale E7B analysis or materialization consumed | Reject consumer | None |
| HPOLY off | Reproduce accepted E6 direct-path digest/calls | Identity path |
| MU/FM/LM off | Reproduce unoptimized HPOLY result | Coherent HPOLY path |
| Feature-absent legacy WHIRL | Preserve accepted legacy behavior | No fabricated HPOLY |
| Unknown persisted version | Reject before HPAO | None |
| Corrupt header/count/range/ID/hash | Reject before use | None |
| Two clean runs or traces differ | Reject determinism | Neither accepted |
| Independent reopen/consumer unavailable | Mark `Unverified` | Not accepted |
| ACE private header/symbol appears | Stop interface review | None |
| ACE rtlib/ANT appears as `be.so` dependency | Stop build review | None |
| Server secret/decryptor or secret bytes appear | Reject and quarantine | None |
| Private FHE/O2/HPOLY/HPAO/POLY op reaches whirl2c | Reject final boundary | None |
| E7A input is rejected but E6 input is valid | E7B remains accepted from E6 only | No joint result |
| E8 cross-replanning is requested | Defer to E8 | No E7B scope growth |
Candidate rejection normally leaves the current HPOLY epoch and continues with the unoptimized legal operation. A stage invariant, stale input, nondeterminism,
or final-boundary failure rejects the entire E7B result. No corrupt partial result is relabeled as fallback.
Performance cases include repeated ModUp diamonds and loops with legal/illegal placements; static weights from empty through cache-reuse and large-package
boundaries; ct-by-pt eligible and ct-by-ct/dynamic negatives; dot products of length 1..64; exact safe, one-below, equal, and one-above word bounds; Q versus QP
limb counts; no-benefit, cost-tie, missing-cost, and saturated-cost cases; and bootstrap-dominated regions where no paper headline speedup is expected.
Paper speedups are prior evidence, never E7B thresholds or Open64 predictions.
## 14. Evidence, Retention, and Exit

### 14.1 Common required bundle

The accepted bundle contains at least:

```text
authority-and-entry.json
o0-qualification-reference.json
e6-stage1-acceptance-reference.json
hpoly-representation-adr.json
hpoly-representation-review.json
source-lock.json
hpao-source-delta.json
hpoly-operators.json
hpao-rules.json
support-matrix.json
diagnostics.json
environment.json
requested-configuration.json
final-ckks-plan-reference.json
provider-capabilities.json
ant-v1-cost-model.json
hpoly-regions.json
hpoly-state-trace.json
bootstrap-barriers.json
relin-boundary-proof.json
candidate-census.json
candidate-legality.json
candidate-costs.json
candidate-ties.json
hpao-decision-log.json
fixed-point-report.json
mu-equivalence-and-placement.json
fm-static-package-manifest.json
fm-ordinary-fast-reference.json
lm-bit-width-and-reduction-proof.json
md-design-pending.json
invalidation-report.json
logical-key-usage.json
provider-key-usage.json
generated.c
generated-c-symbols.txt
build-and-link.log
server-key-census.json
secret-scan.json
decoded-output.json
oracle-output.json
modular-oracle-output.json
raw-samples.csv
metrics.json
cost-model-validation.json
failure-and-corruption.json
compatibility-and-fallback.json
e7a-compatibility.json
e8-handoff.json
stage-decision.md
SHA256SUMS
```
Persisted mode adds every artifact in Section 5.3 plus mapped-image, operator, record, version, old/new reader, `ir_a2b`, `ir_b2a`, and corruption reports.
Transient mode adds every artifact in Section 5.4 plus the trace schema, normalization rules, two-order determinism report, and consumer reconciliation.
Artifacts are cleaned at the start of the next run, never at the end. Failed development evidence is retained at least 30 days, milestone evidence at least 180
days, and accepted stage/ADR evidence for repository lifetime.
`SHA256SUMS` contains no placeholder. A failed run may retain rejected inputs and logs but cannot retain a partial output under a valid `.B` or package name.
### 14.2 Accepted

`O2-E7B-EXIT=Accepted` requires:
every entry gate and fingerprint is current; `O2-E7B-DECISION-001` selected exactly one representation before code; `O2-S23-001`, `O2-S24-001`, the selected
`O2-E7B-001-*` branch, and `O2-E7B-002` through `O2-E7B-005` pass; the HPOLY operation/state/effect/lowering contract is source-mapped and exact; persisted
mode has meaningful binary HPOLY, compatibility, corruption, and reopen; or transient mode has matching pre/post `.B`/`.T` and deterministic complete trace;
bootstrap is opaque and every barrier identity/state is unchanged; every `MulCC -> Relin` remains adjacent, unique, and externally indivisible; production and
independent oracle agree on all region, state, modular, MU/FM/LM, candidate, cost, selection, commit, fixed-point, and lowering fields; legacy WOPT SSAPRE is
unchanged; HPAO-MD is explicitly disabled with no executable rule; fixed N, CKKS parameters, security, placement, protected sites, and keys remain exact;
final standard WHIRL contains no private FHE/O2/HPOLY/HPAO/LPOLY/POLY op; generated C uses the public ACE rtlib/ANT surface and executes correctly; there is
no server secret/decryptor, secret artifact, or unauthorized dependency; two-run determinism, fallback, corruption, failure atomicity, and retention pass; and
semantic, compatibility, crypto, runtime, cost, security, and evidence reviews sign.
Exact completion wording:

    O2-E7B accepted: the reviewed HPOLY representation and bootstrap-opaque
    CKKS-to-HPOLY-to-ANT path preserve the finalized Stage 1 CKKS contract,
    and the independently controlled HPAO-MU, HPAO-FM, and HPAO-LM families
    pass semantic, modular-safety, deterministic-selection, cost, compatibility,
    and final-standard-WHIRL gates for E8 consumption; HPAO-MD remains disabled.
It does not claim FHEFusion acceptance, E8 cross-pass certification, complete Stage 2, O2 completion, HPAO-MD, bootstrap-internal optimization, or O3 readiness.
### 14.3 Rejected and Unverified

`Rejected` identifies the violated contract, owner, first diagnostic, affected region/candidate/decision IDs, artifacts, and correction. E7A may continue
independently from E6. E8 cannot start.
`Unverified` means authority, representation decision, source lock, environment, provider, oracle, same-stem evidence, binary reopen or trace reconciliation, or
review proof is missing. It cannot unlock E8 and cannot be relabeled as HPOLY-off acceptance.
## 15. Rollback and Stop Rules

Candidate rollback restores the prior verified HPOLY epoch. Family rollback restores unoptimized HPOLY lowering. Stage rollback restores the complete E6 direct
CKKS-to-ANT path when the accepted policy authorizes fallback.
Rollback never mixes an optimized HPOLY graph with stale state, package, cost, call, generated C, or evidence. Persisted staged output is removed before a valid
name is published. Transient output is accepted only if the complete pre/trace/post transaction verifies.
Stop for architecture/common-com/build/security review if:
the representation ADR is missing, ambiguous, or contradicted by implementation; persisted HPOLY lacks complete operator/record/mapped-image compatibility;
transient HPOLY cannot produce deterministic complete transformation evidence; any semantic field has two authoritative owners or none; a logical operation
lacks fixed source, complete state, effects, or lowering; bootstrap must be opened or crossed; immediate Relin cannot remain adjacent, unique, and externally
indivisible; any E6 layout, placement, CKKS parameter, security, or key truth must change; MU requires legacy WOPT SSAPRE modification or unsafe effect
motion; FM lacks exact ordinary-reference equivalence or complete package identity; LM safety needs unknown/saturated bounds or relaxed modular equality; MD
implementation is required without its separate accepted design/amendment; an oracle must import production transformation or cost helpers; selection needs
unknown cost or a post-observation threshold change; atomic fallback cannot restore one coherent upstream path; generated C needs a private ACE header/symbol
or provider other than ANT; a direct or indirect `be.so` rtlib dependency appears without approval; server execution requires a secret/decryptor or evidence
contains secret material; any private operation can reach `whirl2c`; E7B completion would require E7A implementation or E8 cross-replanning; or a tolerance,
support row, or profitability rule must be relaxed after results.
## 16. Exact E8 Handoff

The immutable E8 handoff contains:
E7B exit, authority, source, schema, catalog, and ADR hashes; selected representation and its compatibility/evidence mode; exact E6 input CKKS plan, fixed
N/config/parameter/key/provider fingerprints; region IDs, bootstrap barriers, entry/exit states, and immediate-Relin proof; persisted HPOLY record/image
hashes or transient pre/trace/post hashes; every MU/FM/LM candidate, rejection, cost, tie, decision, and commit epoch; HPAO family off/selected results and
unoptimized HPOLY fallback identity; static package/cache identities and offline/runtime cost components; bit-width/reduction proofs and exact modular-oracle
results; HPOLY-to-ANT call, ownership, lifetime, key-use, and cleanup mapping; final standard `.B`/same-stem `.T`, generated C, symbols, outputs, and
metrics; HPAO-MD disabled disposition and future design owner; invalidation, rollback, failure, compatibility, secret, and build audit; and producer/consumer
normalized digests and all evidence hashes.
E8 independently reopens or reconciles the selected representation evidence, then validates IDs, versions, ranges, hashes, CKKS states, fixed N, policy, keys,
packages, and provider calls.
E8 alone may compose E7A and E7B, invalidate/replan across their interaction, run legal pair/all-on ablations, decide `O2-ALL` and bare `-O2`, enforce final
non-regression, and create the O3 handoff. E8 may reject a composition but may not silently rewrite E7B's representation, rule catalog, or accepted decisions.
The E8/O3 handoff may carry finalized HPOLY state and actions for audit, but O3 may only perform physical scheduling and memory/data optimization. O3 cannot
rerun baseline DP/ReSBM, change logical packing, scale, approximation, bootstrap/protected boundaries, or reopen HPAO semantic selection.
## 17. Requirement Traceability

| Requirement | Authority or milestone | Work | Proposed verification | Retained evidence |
| --- | --- | --- | --- | --- |
| Stage 1 before E7B | User sequence; `S1.9` | `W01/W07` | Entry checks in `O2-S23-001` | E6 decision and `.B`/`.T` |
| Representation before code | O2 `S2.3`; governing plan Section 12.4 | `W04/W05` | `O2-E7B-DECISION-001` | ADR and compatibility review |
| Persisted compatibility branch | `AGENTS.md`; O2 `S2.3` | `W05/W10` | `O2-E7B-001-P` | Meaningful HPOLY `.B`/`.T`, reopen/corruption |
| Transient trace branch | O2 `S2.3`; governing plan Section 12.5 | `W05/W10` | `O2-E7B-001-T` | Pre/post `.B`/`.T`, deterministic trace |
| Bootstrap opaque | Master 12.8; O2 `S2.3` | `W07/W08/W23` | `O2-S23-001`, `O2-E7B-005` | Barrier and lowering proof |
| Immediate Relin | Master/O2 invariant | `W08/W23` | `O2-S23-001`, `O2-E7B-005` | State/action/generated-C audit |
| HPAO-MU dedicated phase | Master 12.4/12.5; `S2.4` | `W11/W12` | `O2-S24-001`, `O2-E7B-002` | Equivalence/placement and SSAPRE diff |
| HPAO-FM static/fast path | Master 12.4/12.7; `S2.4` | `W13/W14` | `O2-S24-001`, `O2-E7B-004` | Package and ordinary-fast oracle |
| HPAO-LM word safety | Master 12.4; `S2.4` | `W15/W16` | `O2-S24-001`, `O2-E7B-002` | Bit-bound/reduction/modular proof |
| HPAO-MD disabled | O2 `S2.4`, MD-DESIGN | `W17` | `O2-S24-001`, `O2-E7B-003` | Disabled disposition/diagnostic |
| Selection and commit | O2 `S2.4` | `W18-W21` | `O2-E7B-002/003` | Census, ties, commit, rollback |
| State invalidation | Governing plan E7B/E8 boundary | `W20/W24` | `O2-E7B-002/003` | Epoch/invalidation/fixed-point reports |
| Fixed N and no second CKKS truth | O2 Sections 9/14 | `W06/W08/W23` | `O2-S23-001`, `O2-E7B-005` | State crosswalk and N audit |
| Public ANT ABI and ownership | `AGENTS.md`; FRZ-08/09 | `W22/W27` | `O2-E7B-004/005` | Includes/symbols/lifetime report |
| No unauthorized `be.so` dependency | `AGENTS.md` | `W05/W27` | `O2-E7B-005` | Link-closure/symbol report |
| No server secret/decryptor | Runtime/security decision | `W22/W27` | `O2-E7B-005` | Key census and secret scan |
| No private op at whirl2c | O2 hard gate | `W22/W23/W27` | `O2-E7B-004/005` | Final standard `.B`/`.T` and audit |
| Fallback and feature absence | P2/evidence rules | `W20/W25` | `O2-E7B-003` | Fallback/corruption/compatibility |
| E7A not folded into E7B | Governing plan Section 12.4 | `W29` | `O2-E7B-006` | Read-only compatibility proof |
| E8 handoff | Governing plan Section 12.4; O2 `S2.5/S2.6` | `W30` | E8 consumer dry run | Signed immutable handoff |
Each accepted row closes:

    requirement -> accepted master/hash -> ADR -> governing O2 milestone/test
      -> E7B task -> exact proposed command -> retained artifact -> reviewer
A pending authority, undecided representation, unknown owner, moving source, placeholder hash, stale E6 fingerprint, missing independent oracle, unavailable
required ANT provider, or incomplete same-stem evidence prevents `O2-E7B-EXIT=Accepted`.
