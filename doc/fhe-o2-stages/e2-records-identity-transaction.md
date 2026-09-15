# O2-E2 Detailed Plan: Records and Atomic Identity-Plan Transaction

Status: proposed execution plan; implementation is locked until all entry gates pass

Plan date: 2026-09-15

Engineering stage: `O2-E2`

Governing milestones: `P2` and `S1.1`

Exit gate: `O2-E2-EXIT`

Primary acceptance tests: `O2-P2-001` and `O2-S11-001`

## 1. Authority and Planning Baseline

This document refines only the `O2-E2` stage defined by governing O2 plan
Section 12.4. It does not amend the master plan, the governing O2 plan,
the pending ownership ADR, or any O0 producer contract.

| Input | Locked planning value | Status in this plan |
| --- | --- | --- |
| Repository instructions | `AGENTS.md` as read on 2026-09-15 | Mandatory |
| Recorded master | `doc/DSC_FHE_Compiler_Architecture_and_Integration_Plan_v0.10.md` | Current authority pending successor agreement |
| Recorded master commit | `ee1dc6382246c58f49a3097157a8c4e8ff2440c8` | Mandatory traceability input |
| Recorded master Markdown SHA-256 | `7EF644E9BECEFB541102514B83656C78756ECA71398A771A2A5160FC10B0CB25` | Mandatory traceability input |
| Recorded master DOCX SHA-256 | `0018769C26B5A0BCD1BDFCBD85AA97B8BAFEA381D7640FBB9E2E81B0022013D9` | Mandatory traceability input |
| Governing O2 plan | `doc/FHE-O2-ACE-RESBM-METAKERNEL-INTEGRATION-PLAN.md`, version 1.6 | Governs semantics, stage decomposition, and completion |
| Governing O2 plan SHA-256 | Accepted committed hash required at activation | Planning snapshot must be refreshed before execution |
| Detailed execution index | `doc/FHE-O2-DETAILED-EXECUTION-PLAN.md`, plan set version 0.2 | Non-normative stage navigation |
| Pending ADR | `doc/adr/FHE-O0-O2-PLANNER-OWNERSHIP.md` | Must be accepted before entry |
| Observed repository HEAD | `9c8ca1d3cae1c9f4761feb75673739b5b4d96aef` | Observation only; not an implementation lock |

The master successor must replace every pending master reference before
`PRE-O2-LOCK-EXIT` can close. A changed governing document hash requires a
traceability review and either an update to this plan or an explicit finding
that the change is immaterial to E2.

## 2. Ownership and Reviews

One `osprey/common/com` editor owns the physical record decision, public API,
mapped-image write/load/validation, and logical printer. The common DSL identity
owner owns stable IDs; the backend transaction owner owns all-PU publication;
the O2 transaction owner owns identity production/materialization; and an
independent test owner owns corruption and failure injection. WHIRL, binary
compatibility, call-image, backend reliability, FHE semantics, and determinism
reviewers approve their respective surfaces. A planner may propose fields but
cannot allocate storage or change a public record without common/com approval.

## 3. Objective and User-Visible Result

E2 establishes the persistent and transactional boundary needed by all later
O2 planners. It must prove that an identity selected plan can be created for an
entire input file, published atomically, reopened after the producer exits, and
materialized without changing any source semantics.

The user-visible result is a reviewable binary WHIRL plan artifact with stable
identities, deterministic fingerprints, matching ASCII evidence, and exact
failure behavior. It is infrastructure evidence, not an optimization result.

### 3.1 In scope

- Audit and reuse the qualified O0 configuration, source, layout, CKKS state,
  placement, protected-site, key, call, node, and value identities.
- Decide the reviewed physical carrier for O2 extension records.
- Add only the O2 iteration-space, census, candidate/selection envelope,
  provenance, identity mapping, and transaction metadata required downstream.
- Define stable PU, node, value, edge, and direct-call identities.
- Bind the plan to source, configuration, O0 qualification, record schema, and
  ACE rtlib/ANT capability fingerprints without calling the provider.
- Build all-PU staging, validation, commit, abort, and failure injection.
- Produce and reopen an identity selected plan in separate processes.
- Identity-materialize every PU and prove structural and semantic preservation.
- Verify old artifacts, feature-absent artifacts, and accepted new artifacts.
- Retain meaningful `.B` files and same-stem `.T` files produced with
  `ir_b2a -st -src`.

### 3.2 Explicit non-goals

- No MetaKernel search, candidate scoring, layout selection, or transformed
  plaintext generation.
- No Cipher/Plain graph construction, CKKS transfer analysis, parameter
  resolution, or range/error propagation.
- No baseline-DP modification, ReSBM region construction, cut solving, or
  bootstrap replacement.
- No generated-C, ACE rtlib/ANT execution, provider query, or provider key
  expansion.
- No FHEFusion, HPOLY, HPAO, O3 scheduling, or memory placement.
- No new public command-line optimization flag.
- No new WHIRL operator, opcode encoding, type kind, or private O2 node.
- No new `be.so` dependency and no ACE compiler, AIR, rtlib, or ANT header in
  backend compiler sources.
- No duplicate layout, CKKS parameter, bootstrap placement, or key truth.

### 3.3 Inherited invariants

- `O2-O0Q-001=Qualified` is required before any E2 implementation, test
  fixture, oracle, or implementation branch begins.
- Requested `N` is already fixed and cannot be changed by E2.
- Binary WHIRL remains the process boundary.
- Mapped-image and ELF mechanisms are used; this is not object-stream encoding.
- Invalid IDs are zero and persisted fields use fixed-width types.
- Persisted records contain no pointers, host-sized enums, or nested STL state.
- Domain semantics remain visible until their gatekeeper checks complete.
- No secret key or secret-derived private material enters records or evidence.
- Canonical `MulCC` and immediate `Relin` semantics are preserved unchanged;
  E2 does not create or materialize either operation.
- A failed transaction cannot leave a file that appears to be a valid plan.

## 4. Entry Gate and Staleness Policy

### 4.1 Required entry evidence

E2 work may start only when all rows are accepted and hash-bound in an entry
record named `o2-e2-entry-lock.json`.

| Gate | Required evidence | Rejection condition |
| --- | --- | --- |
| `PRE-O2-LOCK-EXIT` | Accepted master/ADR/baseline/runtime decisions | Pending or contradictory ownership or storage rule |
| `O2-E0-EXIT` | Current `O2-O0Q-001=Qualified` bundle and fingerprint | `Rejected`, `Unverified`, missing artifact, or stale bound input |
| `O2-E1-EXIT` | Accepted `O2-S10-001` source/support/diagnostic/oracle lock | Placeholder, moving ref, duplicate ID, or unresolved E2 requirement |
| FRZ-05 | Baseline record owner and O2 extension boundary | More than one authoritative owner or missing common/com editor |
| O0 record crosswalk | Version, capability, owner, verifier, and compatibility rows | Missing old/feature-absent behavior |
| Toolchain | Compiler, linker, `ir_b2a`, test runtime, and host hashes | Unreproducible or different tool used without delta review |

The entry checker must fail before compilation or fixture generation if any row
is not accepted. A planning document is not proof that the gate passed.

### 4.2 Stale triggers

The following changes invalidate the E2 entry lock and every E2 artifact:

- master, ADR, governing O2 plan, or stage-control changes affecting record ownership,
  runtime boundary, qualification order, or completion semantics;
- a changed `O2-O0Q-001` qualification ID or any bound O0 source, pass, config,
  support, record, or protected-site fingerprint;
- a changed existing DSL/FHE/plan image magic, version, size, capability, or
  reader/writer rule;
- a changed stable identity algorithm or canonical source ordering;
- a changed ACE rtlib revision, public-header hash, build hash, `LIB_ANT`
  capability manifest, or provider parameter contract;
- a changed compiler, linker, `ir_b2a`, or target ABI;
- a changed test generator version, seed, corruption schema, or expected
  diagnostic catalog.

On a stale trigger, stop publication, mark prior evidence `Stale`, rerun the
entry checker, perform the targeted upstream requalification, and then rerun
both E2 acceptance tests. Do not rewrite a prior accepted bundle in place.

## 5. Two Dependency Graphs

### 5.1 Implementation and build dependency

```text
PRE-O2-LOCK-EXIT
  -> O2-E0-EXIT and current O2-O0Q-001=Qualified
  -> O2-E1-EXIT and accepted E2 contract requirements
  -> E2 physical-storage and compatibility review
  -> P2 record schema, APIs, reader, writer, printer, verifier
  -> P2 compatibility and corruption acceptance
  -> S1.1 stable identity and fingerprint services
  -> S1.1 all-PU transaction and identity-plan producer
  -> S1.1 independent reopen and identity materializer
  -> O2-E2-EXIT
```

No later O2 algorithm is an E2 build dependency. E2 must remain testable with
an identity plan whose selections exactly preserve the qualified O0 input.

### 5.2 Compiler pass order served by E2

```text
reopened canonical Open64 FHE source
  -> validate source/config/O0 qualification fingerprints
  -> begin one file-wide O2 plan transaction
  -> visit all PUs and bind stable source identities
  -> later E3/E4/E5 producers populate reviewed records
  -> validate all-PU and cross-record invariants
  -> atomically publish selected plan
  -> producer exits
  -> independent materializer reopens source and plan
  -> validate all fingerprints and records
  -> later stages materialize selected actions
```

For the E2 identity profile, the later producers and action materialization are
replaced by no-op selections. The source tree, layout, state, placement, keys,
and call relationships must remain unchanged.

### 5.3 Build and link impact

- Common/com changes may affect every WHIRL reader and writer and therefore
  require the normal common/com build and compatibility suite.
- Backend transaction changes may change `be.so` symbols and all consumers must
  remain link-closed, but E2 must add no external library dependency.
- Existing `be.so` consumers, including `lw_inline`, backend plugins, and
  standalone tools, must be inventoried before a public symbol change.
- Hashing must use an already approved Open64 facility or a reviewed
  dependency-free implementation. Adding a hash library to `be.so` is forbidden
  without the separate dependency review required by `AGENTS.md`.
- No ACE rtlib/ANT library is linked or loaded in E2. Its accepted manifest hash
  is opaque input data only.
- Frontend-only builder libraries must not become backend dependencies.

## 6. Authoritative Record Contract

### 6.1 Required physical-storage decision

Before record code is written, `E2-REC-001` must select exactly one reviewed
carrier:

1. a versioned extension of the existing `WT_DSL_FHE_PLAN` mapped image;
2. a separately reviewed optional mapped image using established WHIRL/ELF
   extension mechanisms; or
3. another existing mapped-image pattern accepted by common/com review.

The decision must cite the relevant `WHIRL.pdf` sections and current source,
record old-reader behavior, new-reader behavior, migration, capability bits,
image size checks, and `ir_b2a` visibility. This plan does not pre-authorize a
new `WT_*` value, section name, operator, or in-place layout change.

The existing `DSL_FHE_PLAN_IMAGE_VERSION` value and fixed record sizes are
compatibility contracts. They must not be silently changed. If the review
selects a versioned successor, version 1 read behavior remains covered. If it
selects a new optional image, absence of that image must preserve legacy input
behavior.

### 6.2 Logical record families

Names in this table are semantic planning names until `E2-REC-001` accepts
their exact public spelling and physical carrier.

| Logical family | Required fields | Authority | E2 identity behavior |
| --- | --- | --- | --- |
| `O2PlanHeaderIR` | magic, schema version, capabilities, flags, counts, stage status | Authoritative plan envelope | Marks `identity-selected`, never `optimized` |
| `O2PlanProvenanceIR` | master/ADR/O2-plan hashes, O0 qualification ID, E1 lock ID, toolchain ID | Authoritative provenance | Exact copy of accepted entry lock |
| `O2SourceFingerprintIR` | source image hash, config hash, canonical digest, PU count, source identity version | Authoritative binding | Must match reopened source exactly |
| `O2CapabilityFingerprintIR` | O0 record schema hash, ACE rtlib/ANT manifest hash, target capability hash | Evidence binding, not semantic truth | Consumed opaquely; no provider query |
| `O2PUIdentityIR` | stable PU source identity, symbol identity, deterministic ordinal, first/count ranges | Authoritative identity map | One row per source PU |
| `O2NodeIdentityIR` | stable PU ID, source node ID, logical opcode identity, source lineage digest | Authoritative source reference | One-to-one pass-through mapping |
| `O2ValueIdentityIR` | stable PU ID, source value ID, defining node/result position, descriptor ID | Authoritative source reference | One-to-one pass-through mapping |
| `O2EdgeIdentityIR` | source/destination stable IDs, operand/result position, control/data kind, callsite ID | Authoritative edge identity | No action attached in E2 |
| `O2IterationSpaceIR` | stable owner, rank, canonical bounds/steps, lineage ID, flags | Shared planner input | Imported or empty; no transformed schedule |
| `O2CensusIR` | stable owner, metric kind, exact integer value, provenance ID, unknown flag | Derived report input | Source census only; no profitability claim |
| `O2CandidateEnvelopeIR` | candidate ID, planner family, support row, status, reason, first/count ranges | Derived candidate container | Exactly one identity candidate |
| `O2SelectionIR` | selection ID, candidate ID, source/config fingerprints, status, fallback reason | Authoritative selected-plan choice | Selects identity candidate explicitly |
| `O2TransactionIR` | transaction ID, expected PU count, completed PU count, validation state, publication state | Publication evidence | Complete only after all-PU validation |

E2 does not duplicate fields already owned by O0 records. Its rows reference
qualified O0 configuration, layout, CKKS state, placement, protected-site, and
logical-key records by stable ID and fingerprint.

### 6.3 Field and range rules

- All persisted integer fields have explicit widths.
- ID zero is invalid and never names a record.
- Enum storage uses fixed-width integers plus explicit range validation.
- Every `first/count` pair is checked for addition overflow and image bounds.
- Every ID is unique within its family and resolves to exactly one owner.
- Cross-PU references use accepted source identities, not pointer addresses or
  traversal ordinals alone.
- Strings use the established WHIRL string-table identity and validated bounds.
- Reserved fields are written as zero and rejected if the accepted schema says
  they must remain zero.
- Records are sorted by a frozen tuple of stable IDs before finalization.
- Hash inputs use a documented canonical byte order and exclude mutable cache
  paths, timestamps, addresses, temporary filenames, and random key bytes.
- Counts and sizes use checked arithmetic before allocation or pointer advance.
- The writer finalizes mapped images into ELF sections only after validation.

### 6.4 Stable identity rules

| Identity | Construction input | Required stability property |
| --- | --- | --- |
| PU | Existing `DSL_PU_SOURCE_IDENTITY_ID` and accepted symbol/source identity | Independent of read order and address |
| Callsite | Existing `DSL_CALLSITE_METADATA_ID` and canonical callsite lineage | Stable after independent reopen |
| Node | Existing `DSL_IR_NODE_ID`, owning PU, and logical opcode contract | No physical `OPR_DSL` leakage in diagnostics |
| Value | Existing `DSL_IR_VALUE_ID`, defining node/result position, and descriptor | Unique for each semantic result |
| Edge | Source value, consumer node, operand index, edge kind, and callsite context | Stable on critical edges and fanout |
| Candidate | Planner family, support row, canonical inputs, and deterministic ordinal | Identity candidate is ordinal zero only by contract |
| Selection | Candidate identity plus complete source/config/capability fingerprint | Changes whenever a bound input changes |
| Transaction | Selection fingerprint plus deterministic transaction schema | Excludes wall-clock time and random data |

Duplicate tuples, dangling references, ambiguous lineage, or an identity that
changes across two clean productions fail with a stable diagnostic.

### 6.5 Required public operation surface

The exact C names are frozen by `E2-REC-001`. The accepted common/com API must
provide reset, header query, record-presence query, validation, mapped load,
logical printing, stable-ID intern, add, find, count, and finalize operations.
Each operation uses opaque handles or fixed records and explicit success/failure;
it exposes no record layout to Python or planner clients.

The accepted backend API must provide
`BeginTransaction(source_fingerprint, expected_pu_count, diagnostic)`,
`AppendPU(transaction, pu_identity, ranges, diagnostic)`, validate, finalize,
publish, and idempotent abort operations. Finalize freezes the image, append is
illegal afterward, and abort removes only exact transaction-owned staging
targets.

### 6.6 Identity-plan contract

The identity selected plan must satisfy all of these rules:

1. It references every input PU exactly once.
2. It references every required source node, value, edge, and call relationship
   without introducing an O2 semantic operation.
3. It selects one explicit identity candidate per applicable planning domain.
4. It preserves source layout, CKKS state, bootstrap placement, protected-site
   identity, and logical key requirements byte-for-byte or by an accepted
   normalized equivalence when source storage ordering differs.
5. It adds no runtime call, transformed constant, provider key, or CKKS action.
6. It carries source/config/schema/qualification/capability fingerprints.
7. It materializes every PU through the standard backend path without an
   in-memory producer bypass.
8. A second materialization produces structurally identical WHIRL.

### 6.7 Compatibility matrix

| Producer | Consumer | Required result |
| --- | --- | --- |
| Existing version-1 artifact | New reader | Accepted with unchanged normalized records and dump |
| New writer with O2 feature absent | Existing behavior and new reader | Structurally equivalent to the accepted legacy artifact |
| New writer with O2 feature present | New reader | Accepted only with known version and required capabilities |
| New writer with O2 feature present | Old reader | Exact safe behavior selected by `E2-REC-001`: ignore optional image or reject unsupported version before use |
| Corrupt new artifact | New reader | Fail closed before mapped record access or plan use |
| Unknown required capability | New reader | `FHE-O2-PLAN-CAPABILITY-UNSUPPORTED` |

An old reader is never required to understand new semantics. It must not accept
a plan and silently apply incomplete semantics. Feature absence must remain a
valid and deterministic state.

## 7. Transaction and Publication Contract

### 7.1 State machine

```text
Empty
  -> Staging
  -> AllPUsCollected
  -> Validated
  -> Finalized
  -> Published

Any nonterminal state
  -> Aborted
```

Only `Finalized` may transition to `Published`. A validation or publication
failure transitions to `Aborted`. A published primary `.B` is valid only when
its internal transaction state and externally retained manifest agree.

### 7.2 Atomicity rules

- Reuse the existing all-PU FHE checkpoint pattern where its guarantees apply.
- Resolve and validate the exact destination and staging paths before mutation.
- Stage output beside the destination when filesystem atomicity requires it.
- Write and validate all mapped images before the primary file is published.
- Close and sync the staging file according to the accepted platform contract.
- Publish the primary `.B` with one reviewed atomic operation.
- Never write directly into an existing accepted artifact.
- Publish evidence metadata only after it references the final primary hash.
- A failed `.T` generation does not rename a partial file as valid evidence;
  the `.B` remains a failed-run artifact with an explicit incomplete decision.
- Cleanup targets only transaction-owned staging paths with exact resolved
  names. It never removes a prior accepted bundle.
- Injection at every state transition and file operation must leave no
  apparently complete destination.

### 7.3 Reopen boundary

The producer must terminate and release all mapped images, pools, global state,
and in-memory transaction objects. A separately launched consumer reopens the
source and identity-plan `.B`, validates every image and fingerprint, performs
identity materialization, and writes a new materialized `.B`.

Passing an in-memory address, shared process global, temporary pointer map, or
producer callback to the consumer invalidates the test.

## 8. Diagnostic Contract

The E1 diagnostic catalog must assign and freeze the exact text template for
each code before implementation.

| Code | First failing condition |
| --- | --- |
| `FHE-O2-PLAN-SCHEMA-UNSUPPORTED` | Unknown image version or record kind |
| `FHE-O2-PLAN-CAPABILITY-UNSUPPORTED` | Unknown required capability bit |
| `FHE-O2-PLAN-ID-ZERO` | Required reference uses invalid ID zero |
| `FHE-O2-PLAN-ID-DUPLICATE` | Two records claim one identity |
| `FHE-O2-PLAN-ID-DANGLING` | Reference does not resolve |
| `FHE-O2-PLAN-RANGE-INVALID` | Count/range overflow or out-of-image range |
| `FHE-O2-PLAN-OWNER-MISMATCH` | Record resolves under the wrong PU or family |
| `FHE-O2-PLAN-STALE` | Source, config, schema, qualification, or capability hash differs |
| `FHE-O2-PLAN-PU-INCOMPLETE` | Expected and completed PU sets differ |
| `FHE-O2-PLAN-NONDETERMINISTIC` | Clean productions differ after normalization |
| `FHE-O2-PLAN-IDENTITY-NONPASS` | Identity selection changes source semantics or structure |
| `FHE-O2-PLAN-PUBLISH-FAILED` | An atomic publication step fails |
| `FHE-O2-PLAN-SECRET-MATERIAL` | Secret material pattern is detected in evidence |

First-diagnostic ordering is schema/header, size arithmetic, capability,
identity/range, ownership/cross-record, fingerprint, transaction completeness,
and semantic identity. Tests must assert that order.

## 9. Work Breakdown Structure

| Task ID | Concrete change and components | Dependency | Tests and artifacts | Owner and reviewer | Merge or exit rule |
| --- | --- | --- | --- | --- | --- |
| `E2-GATE-001` | Add entry-lock checker for master/ADR/O0Q/E1 hashes under proposed `osprey/common/com/tests/run_fhe_o2_record_contract.py` support files | Implementation gate | Entry report and stale variants | Test owner / architecture reviewer | No implementation work before exact pass |
| `E2-REC-001` | Review `WHIRL.pdf`, current `dsl_fhe_plan.*`, `elf_whirl.h`, readers, writers, and printers; select physical carrier | Design dependency | Accepted storage decision and compatibility matrix | Common/com editor / WHIRL reviewers | Required before schema code |
| `E2-REC-002` | Freeze logical record crosswalk, authority, field widths, invalid IDs, range rules, and capabilities | `E2-REC-001` | Machine-readable schema and normalized crosswalk | Common/com editor / FHE reviewer | No duplicated O0 truth |
| `E2-REC-003` | Implement accepted fixed-width declarations and compile-time size/width assertions in `osprey/common/com/dsl_fhe_plan.h` or reviewed successor | `E2-REC-002` | Layout report and build logs | Common/com editor / ABI reviewer | All supported ABI checks exact |
| `E2-REC-004` | Implement initialization, add/intern/find/count APIs in `osprey/common/com/dsl_fhe_plan.cxx` or reviewed successor | `E2-REC-003` | Unit records and duplicate-ID negatives | Common/com editor / API reviewer | Invalid-zero and ownership rules enforced |
| `E2-REC-005` | Implement mapped-image size calculation, finalization, load, and capability/version validation | `E2-REC-003` | Valid and corrupt image corpus | Common/com editor / reader-writer reviewer | Checked arithmetic at every range |
| `E2-REC-006` | Extend binary writer/reader integration only at reviewed extension points, including `osprey/include/sys/elf_whirl.h` if authorized | `E2-REC-001`, `E2-REC-005` | Old/new write/reopen matrix | Common/com editor / WHIRL reviewer | No unreviewed `WT_*` or section |
| `E2-REC-007` | Add logical record printing and `ir_b2a -st -src` visibility without exposing physical escape encodings | `E2-REC-005` | Same-stem `.B` and `.T` pairs | Printer owner / DSL reviewer | Stable normalized text twice |
| `E2-ID-001` | Specify and implement PU/call/node/value identity reuse and deterministic validation | `E2-REC-002` | Multi-PU reordered-input cases | Identity owner / call-image reviewer | Reopen identity equality exact |
| `E2-ID-002` | Specify and implement stable edge identities for data, control, call, phi, fanout, and critical edges | `E2-ID-001` | Edge collision/dangling fixtures | Identity owner / CFG reviewer | No address or traversal dependency |
| `E2-ID-003` | Implement canonical record ordering and canonical fingerprint byte stream | `E2-ID-001`, `E2-ID-002` | Two-run binary/text/hash comparison | Identity owner / determinism reviewer | Exact normalized and byte equality |
| `E2-ID-004` | Add source/config/O0Q/schema/ACE-capability fingerprint binding and stale-plan checks | `E2-ID-003` | One-field-at-a-time stale corpus | Identity owner / runtime-interface reviewer | Every bound delta rejects before use |
| `E2-COMPAT-001` | Add legacy version-1, new feature-absent, new feature-present, and unknown-capability corpus | `E2-REC-005` | Golden `.B`/`.T`, expected diagnostics | Test owner / compatibility reviewer | Complete matrix accepted |
| `E2-CORRUPT-001` | Generate header/count/range/ID/hash/owner/capability corruptions without production validator reuse | `E2-REC-005` | Corruption manifest and first diagnostics | Independent test owner / security reviewer | Every corruption fails closed |
| `E2-TXN-001` | Design backend transaction context and state machine in proposed `osprey/be/driver` testable interface | P2 accepted | State transition unit trace | Transaction owner / reliability reviewer | No global partial plan state |
| `E2-TXN-002` | Reuse and extend all-PU checkpoint collection in `osprey/be/be/driver.cxx` under a reviewed isolated change | `E2-TXN-001` | 1..16 PU completion tests | Transaction owner / backend reviewer | Expected PU set exact |
| `E2-TXN-003` | Implement staging, validation, finalize, atomic publish, abort, and exact-target cleanup | `E2-TXN-002` | Injection at every operation | Transaction owner / reliability reviewer | No apparently valid partial artifact |
| `E2-TXN-004` | Implement identity candidate and selected-plan production with no algorithmic fields | P2, `E2-ID-004` | Identity selection report | O2 transaction owner / FHE reviewer | Explicit pass-through only |
| `E2-TXN-005` | Implement producer shutdown harness and separate-process source/plan reopen | `E2-TXN-003`, `E2-TXN-004` | Reopen logs and mapped-image audit | Test owner / process-boundary reviewer | No shared producer state |
| `E2-TXN-006` | Implement identity materialization for every PU using only reopened records | `E2-TXN-005` | Source versus mid structural proof | O2 transaction owner / backend reviewer | No semantic or structural O2 change |
| `E2-TXN-007` | Add second materialization and reordered-PU scheduling determinism tests | `E2-TXN-006` | Two materialized `.B`/`.T` pairs | Test owner / determinism reviewer | Exact normalized structural equality |
| `E2-BUILD-001` | Rebuild common/com, backend, readers, writers, `ir_b2a`, `be.so`, and known shared consumers; inspect symbols | All code tasks | Build and symbol inventory | Build owner / link-closure reviewer | No new external dependency or unresolved symbol |
| `E2-SEC-001` | Scan `.B`, `.T`, manifests, logs, and diagnostics for secret/private key material | Artifact production | Secret-scan report | Security test owner / crypto reviewer | Zero secret material |
| `E2-ACC-001` | Run `O2-P2-001`, assemble P2 evidence, and obtain common/com acceptance | Record tasks | P2 acceptance bundle | Common/com owner / independent reviewer | P2 accepted before S1.1 exit |
| `E2-ACC-002` | Run `O2-S11-001`, assemble transaction evidence, and sign E2 decision | All tasks | S1.1 and E2 bundles | E2 acceptance owner / independent reviewer | All exit criteria pass |

## 10. Proposed Commit and Pull Request Split

Each commit has one semantic purpose and contains its tests. Generated local
evidence is not committed unless the accepted fixture policy names it as a
golden.

| Order | Commit label | Content | Governing commit group | Required pre-merge evidence |
| --- | --- | --- | --- | --- |
| 1 | `E2-CONTRACT` | Storage decision, record crosswalk, schema, diagnostics, compatibility behavior | `REC-O2` | Common/com and WHIRL approval |
| 2 | `E2-RECORD-API` | Fixed declarations, initialization, builders, readers, and validators | `REC-O2` | Unit, width, range, and duplicate tests |
| 3 | `E2-IMAGE` | Mapped-image write/load/finalize integration | `REC-O2` | Old/new reopen and corruption tests |
| 4 | `E2-PRINT` | Logical printer and `ir_b2a` visibility | `REC-O2` | Stable same-stem `.B`/`.T` evidence |
| 5 | `E2-IDENTITY` | Stable PU/node/value/call/edge identities and canonical order | `S1-2` | Reorder, fanout, call, and two-run tests |
| 6 | `E2-FINGERPRINT` | Source/config/schema/O0Q/capability bindings and stale rejection | `S1-2` | One-field stale matrix |
| 7 | `E2-TRANSACTION` | All-PU state machine, staging, finalize, publish, abort | `S1-2` | Failure injection at every step |
| 8 | `E2-IDENTITY-PLAN` | Identity candidate, selected plan, producer exit, reopen | `S1-2` | Separate-process exact reopen |
| 9 | `E2-IDENTITY-MATERIALIZE` | Reopened identity materializer for all PUs | `S1-2` | Source/mid structural equality |
| 10 | `E2-ACCEPT-P2` | P2 acceptance fixtures and signed record decision only | `REC-O2` | `O2-P2-001` pass |
| 11 | `E2-ACCEPT-S11` | S1.1 acceptance fixtures and signed E2 decision only | `S1-2` | `O2-S11-001` pass |

Do not combine the record ABI change with backend transaction logic. Do not add
later planner fields because their future use appears convenient. A later stage
adds a field only through the accepted version/capability process.

## 11. Verification Matrix

All commands labeled `Proposed target command` are interfaces to be created by
this stage. They are not claims that the scripts or directories exist now.

### 11.1 Primary P2 record contract

Test ID: `O2-P2-001`

Proposed target command:

```text
python3 osprey/common/com/tests/run_fhe_o2_record_contract.py --build-dir build --manifest testdata/fhe_o2/records/SHA256SUMS --artifacts test-artifacts/o2/O2-P2-001
```

| Field | Contract |
| --- | --- |
| Source/input | Qualified O0 old artifacts, feature-absent new artifacts, accepted O2 identity records, and corruption corpus |
| Expected | Exact record counts, IDs, ranges, ownership, capabilities, fingerprints, normalized dumps, and first diagnostics |
| Tolerance | None; all structural and hash comparisons are exact |
| Seed/bounds | `0x52454344`; chain-size metadata 1..16; component metadata 2 and 3; record counts at 0, 1, boundary, and one-over-boundary |
| Platform | Every supported WHIRL reader ABI; provider-free; ACE capability manifest is opaque input |
| Runs/timeout | Two clean writes plus independent reopen; 0 warmups; 300 seconds |
| Retention | Images, `.T` dumps, hashes, and corruptions for 180 days; accepted goldens indefinitely |
| Pass rule | Zero mismatch; no skipped required reader; no placeholder; common/com and compatibility signoff |

### 11.2 Primary S1.1 transaction contract

Test ID: `O2-S11-001`

Proposed target command:

```text
python3 osprey/be/driver/tests/run_fhe_o2_transaction_contract.py --build-dir build --manifest testdata/fhe_o2/transaction/SHA256SUMS --artifacts test-artifacts/o2/O2-S11-001
```

| Field | Contract |
| --- | --- |
| Source/input | Multi-PU canonical `.B`, identity plan, stale/corrupt variants, reordered PU visitation, and injected failure at each publication step |
| Expected | Independent reopen exact; identity materialization exact; stale hash gives `FHE-O2-PLAN-STALE`; no partial valid artifact |
| Tolerance | None; byte and normalized structural comparisons are exact |
| Seed/bounds | `0x54584e31`; 1..16 PUs; fanout 1..8; failure at every enumerated state/file transition |
| Platform | Supported host and filesystem atomicity profile; provider manifest hash only |
| Runs/timeout | Two clean productions, two materializations, 0 warmups, 300 seconds |
| Retention | Source, plan, and mid `.B`/`.T`, hashes, manifests, logs, and failure directories for 180 days |
| Pass rule | Byte determinism, all-or-nothing publication, exact diagnostics, separate-process proof, independent review |

### 11.3 Exact same-stem inspection commands

Proposed target commands:

```text
build/bin/ir_b2a -st -src test-artifacts/o2/O2-S11-001/source.fhe.B test-artifacts/o2/O2-S11-001/source.fhe.T
build/bin/ir_b2a -st -src test-artifacts/o2/O2-S11-001/identity-plan.B test-artifacts/o2/O2-S11-001/identity-plan.T
build/bin/ir_b2a -st -src test-artifacts/o2/O2-S11-001/materialized.identity.mid.B test-artifacts/o2/O2-S11-001/materialized.identity.mid.T
```

The source file named by DST must remain available at its recorded path so
`-src` can interleave source statements. If `ir_b2a` lacks `-src`, E2 remains
open and the tooling gap must be fixed.

### 11.4 Focused verification rows

| Test ID | Proposed target command | Expected result |
| --- | --- | --- |
| `O2-E2-REG-001` | `sh osprey/common/com/tests/dsl_fhe_sync3_plan_image_test.sh` | Existing version-1 plan image behavior remains accepted |
| `O2-E2-REG-002` | `sh osprey/common/com/tests/dsl_fhe_conversion_checkpoint_test.sh` | Existing all-PU checkpoint behavior remains accepted |
| `O2-E2-REG-003` | `sh osprey/common/com/tests/dsl_multi_pu_artifact_test.sh` | Existing multi-PU binary boundary remains accepted |
| `O2-E2-REG-004` | `sh osprey/common/com/tests/dsl_ir_tools_smoke_test.sh` | Existing IR tools read and print accepted images |
| `O2-E2-ID-001` | `python3 osprey/common/com/tests/run_fhe_o2_record_contract.py --case stable-identities --build-dir build --manifest testdata/fhe_o2/records/SHA256SUMS --artifacts test-artifacts/o2/O2-E2-ID-001` | PU/node/value/call/edge identities match after reopen and reorder |
| `O2-E2-CORRUPT-001` | `python3 osprey/common/com/tests/run_fhe_o2_record_contract.py --case corruptions --build-dir build --manifest testdata/fhe_o2/records/SHA256SUMS --artifacts test-artifacts/o2/O2-E2-CORRUPT-001` | Every malformed header/count/range/ID/hash fails with frozen first diagnostic |
| `O2-E2-COMPAT-001` | `python3 osprey/common/com/tests/run_fhe_o2_record_contract.py --case compatibility --build-dir build --manifest testdata/fhe_o2/records/SHA256SUMS --artifacts test-artifacts/o2/O2-E2-COMPAT-001` | Complete producer/consumer compatibility matrix passes |
| `O2-E2-TXN-001` | `python3 osprey/be/driver/tests/run_fhe_o2_transaction_contract.py --case failure-injection --build-dir build --manifest testdata/fhe_o2/transaction/SHA256SUMS --artifacts test-artifacts/o2/O2-E2-TXN-001` | Every failure leaves no apparently valid destination |
| `O2-E2-TXN-002` | `python3 osprey/be/driver/tests/run_fhe_o2_transaction_contract.py --case independent-reopen --build-dir build --manifest testdata/fhe_o2/transaction/SHA256SUMS --artifacts test-artifacts/o2/O2-E2-TXN-002` | Consumer has no producer memory and identity-materializes all PUs |
| `O2-E2-TXN-003` | `python3 osprey/be/driver/tests/run_fhe_o2_transaction_contract.py --case determinism --build-dir build --manifest testdata/fhe_o2/transaction/SHA256SUMS --artifacts test-artifacts/o2/O2-E2-TXN-003` | Two clean plan and mid images are byte and text deterministic |
| `O2-E2-SEC-001` | `python3 osprey/be/driver/tests/run_fhe_o2_transaction_contract.py --case secret-scan --build-dir build --manifest testdata/fhe_o2/transaction/SHA256SUMS --artifacts test-artifacts/o2/O2-E2-SEC-001` | No secret or decryptor material appears in retained evidence |
| `O2-E2-LINK-001` | `python3 osprey/be/driver/tests/run_fhe_o2_transaction_contract.py --case link-closure --build-dir build --manifest testdata/fhe_o2/transaction/SHA256SUMS --artifacts test-artifacts/o2/O2-E2-LINK-001` | `be.so` and known consumers remain link-closed with no new external dependency |

The four shell regression commands already name repository tests observed during
planning. The Python E2 runners and their case interfaces are proposed.

## 12. Negative, Failure, and Compatibility Coverage

| Category | Required cases | Pass rule |
| --- | --- | --- |
| Header/schema | Wrong magic/version/header size; unknown capability; illegal count/capability pair | Frozen first diagnostic before record access |
| Bounds/IDs | Size and `first + count` overflow; truncation; overlap; zero, duplicate, dangling, wrong-kind, cross-PU, string, symbol, node, value, callsite, and descriptor IDs | Every mutation fails closed |
| Selection | Missing or duplicate identity candidate; multiple selections; nonzero reserved fields | No complete plan publication |
| Fingerprints | Change source, config, schema, O0Q, ACE revision/header/build/library, `LIB_ANT`, allowlist, Q/P/CRT/profile, `Mul_ciph3 -> Relin`, or lifecycle field one at a time | `FHE-O2-PLAN-STALE` before use |
| PU transaction | Zero, missing, duplicate, unexpected, and reordered PUs; append after finalize; repeated abort | Exact state-machine result |
| File failure | Inject allocation, open, write, close, sync, validate, publish, manifest, `ir_b2a`, termination, collision, permission, cross-device, and space failures | No apparently valid partial output; prior accepted bundle survives |
| Process boundary | Consumer before publication; consumer after producer exit; retry after abort | Only complete published plan reopens |
| Semantic identity | Calls, phis, fanout, critical edges, loops, unreachable nodes, residual, MVM, Conv, ReLU, and protected bootstrap sites | No node, layout, state, placement, key, descriptor, source-position, or action change |
| Private representation | Physical escape name or private FHE/O2 node in logical dump or materialized input | Reject before standard WHIRL boundary |
| Compatibility | Version-1/new-reader; feature-absent; feature-present/new-reader; reviewed old-reader behavior | Exact accepted compatibility matrix |
| Determinism | Two allocation maps, PU orders, process IDs, writes, reopens, materializations, and normalized `.T` dumps | Exact byte and normalized equality |
| Secret safety | Secret key, decryptor, or secret-derived data in any artifact | Zero findings |

Actual runtime Q/P/CRT mismatch, duplicate Relin execution, and server-secret
behavior remain O0Q and E5C execution tests. E2 validates their accepted
fingerprints only and must not claim runtime requalification.

## 13. Evidence and Retention

### 13.1 Required E2 acceptance bundle

| Evidence family | Required files |
| --- | --- |
| Entry and contract | `o2-e2-entry-lock.json`, `record-storage-decision.md`, `record-crosswalk.json`, `record-schema.json`, `compatibility-matrix.json`, `diagnostics.json`, `source-lock.json`, `toolchain-and-abi.json` |
| WHIRL | `source.fhe.B/.T`, `identity-plan.B/.T`, `materialized.identity.mid.B/.T` as separate same-stem files |
| Normalized proof | `normalized-source.json`, `normalized-plan.json`, `normalized-materialized.json`, `stable-identity-map.json`, `source-config-schema-capability-fingerprints.json` |
| Failure proof | `transaction-state-trace.json`, `publication-failure-matrix.json`, `corruption-manifest.json`, `corruption-results.json`, `compatibility-results.json` |
| Build and safety | `build-and-link.log`, `defined-and-undefined-symbols.txt`, `secret-scan.json`, `reproduction-report.json` |
| Decisions | `o2-p2-acceptance-decision.md`, `o2-s11-acceptance-decision.md`, `o2-e2-exit-decision.md`, `SHA256SUMS` |

Phase traces, if enabled, use descriptive lowercase `.t` names such as
`identity-plan.vho.t`; they never replace or collide with same-stem `.T` files.

### 13.2 Retention and publication

- Clean the designated artifact directory at the start of the next run, not at
  the end of the current run.
- Keep failed development evidence for at least 30 days.
- Keep milestone evidence for at least 180 days.
- Keep accepted compatibility goldens and signed gate decisions for repository
  lifetime.
- Do not add generated acceptance artifacts to Git unless the accepted fixture
  policy explicitly designates a golden.
- Report absolute host paths for retained artifacts at the end of validation.
- Publish artifacts atomically and never overwrite an accepted bundle in place.

## 14. Exit Decision

### 14.1 Accepted

`O2-E2-EXIT=Accepted` requires all of the following:

1. The entry lock is current and `O2-O0Q-001=Qualified` remains valid.
2. Common/com and WHIRL reviewers accept the physical carrier and compatibility
   behavior.
3. `O2-P2-001` passes without skipped required readers or compatibility rows.
4. `O2-S11-001` passes every PU count, failure point, stale input, and separate
   process case.
5. Stable PU/node/value/edge/call identities are exact across two clean runs and
   independent reopen.
6. The identity plan preserves every authoritative O0 semantic record and does
   not introduce later-stage semantics.
7. Source, plan, and identity-materialized `.B` files each have matching `.T`
   output produced with `ir_b2a -st -src`.
8. No partial publication, secret material, private operation, unreviewed image,
   or new external backend dependency is present.
9. `be.so` and known consumers remain link-closed.
10. Signed P2, S1.1, and E2 decisions identify owners, reviewers, hashes, and
    retained artifacts.

The exact completion statement is:

```text
O2-E2 records and atomic identity-plan transaction accepted.
```

It must not say that MetaKernel, ReSBM, Stage 1, or O2 is accepted.

### 14.2 Rejected

Use `O2-E2-EXIT=Rejected` when an implemented contract fails a required test or
review. The decision records the first diagnostic, owning task, minimized
reproducer, affected artifacts, and required correction. No downstream E3 work
may consume the failed record or transaction.

### 14.3 Unverified

Use `O2-E2-EXIT=Unverified` when a required reader, toolchain, platform,
artifact, reviewer, or environment is unavailable. Unverified does not permit a
provisional schema, partial transaction, or downstream implementation.

## 15. Rollback, Invalidation, and Stop Rules

Rollback means disabling or removing the incomplete O2 extension while
preserving accepted O0 artifacts and existing version-1 behavior. It does not
mean rewriting O0 records, narrowing the O0 support matrix, or substituting an
in-memory bypass.

Stop and return to common/com or architecture review if:

- a new `WT_*`, ELF section, operator, opcode, or type kind is proposed without
  the accepted physical-storage decision;
- an existing record changes size or meaning without a versioned migration;
- old or feature-absent artifacts cannot be reopened safely;
- two record families claim the same layout, CKKS, placement, or key truth;
- stable identity requires an address, traversal accident, producer memory, or
  Python object;
- a record cannot be reopened after producer exit;
- transaction failure can leave an apparently complete `.B`;
- the accepted O0 qualification or capability fingerprint is stale;
- `N`, CKKS state, layout, placement, protected sites, or key requirements are
  changed by identity materialization;
- a provider library, ACE header, frontend library, or new hash dependency
  enters `be.so` without explicit approval;
- a secret key, decryptor, or secret-derived material appears in any artifact;
- a private FHE/O2 operator reaches the standard WHIRL boundary; or
- a tolerance or expected diagnostic is changed after observing a failure.

## 16. Traceability Matrix

| Requirement | Master or ADR status | O2 plan mapping | E2 work | Verification | Primary evidence |
| --- | --- | --- | --- | --- | --- |
| Complete O0 before O2 | Master successor pending; user decision | Section 13, `O2-O0Q-001` | `E2-GATE-001` | Entry checker plus both primary tests | `o2-e2-entry-lock.json` |
| One authoritative record owner | FRZ-05 pending acceptance | Sections 8 and 13, `P2` | `E2-REC-001..007` | `O2-P2-001` | Record crosswalk and storage decision |
| Binary WHIRL compatibility | `AGENTS.md` and master architecture | Sections 3, 8, and 15 | `E2-REC-003..007`, `E2-COMPAT-001` | P2 compatibility case and regressions | Old/new `.B`/`.T` corpus |
| Stable file-wide identities | Pending O2 physical contract | Sections 6, 12, and 13, `S1.1` | `E2-ID-001..004` | Stable-identity and determinism cases | `stable-identity-map.json` |
| Atomic all-PU publication | Existing checkpoint pattern plus O2 extension review | Sections 12 and 13, `S1.1` | `E2-TXN-001..007` | `O2-S11-001` | Transaction and failure traces |
| Independent process reopen | `AGENTS.md` binary boundary | Sections 8, 12, and 15 | `E2-TXN-005..007` | Independent-reopen case | Source/plan/mid `.B`/`.T` |
| ACE rtlib/ANT binding without execution | FRZ-08/09 pending master acceptance | Sections 7, 8, and 13 | `E2-ID-004` | Stale-capability cases | Capability fingerprint record |
| No new backend dependency | `AGENTS.md` backend dependency rule | Sections 7 and 18 | `E2-BUILD-001` | Link-closure case | Symbol and build reports |
| No secret material | Runtime/security decision pending | Sections 7, 13, and 15 | `E2-SEC-001` | Secret-scan case | `secret-scan.json` |

The traceability owner replaces every pending master/ADR status with the exact
accepted section, version, commit, and hash before E2 execution begins. A
missing link prevents `O2-E2-EXIT`.

## 17. Handoff to O2-E3

E2 hands E3 only these accepted capabilities:

- a reviewed versioned record and mapped-image API;
- stable source identities and deterministic ordering;
- immutable source/config/schema/qualification/capability fingerprints;
- an atomic all-PU plan transaction;
- independent source/plan reopen;
- an identity selected plan and identity materializer;
- exact compatibility, corruption, and failure behavior; and
- retained matching `.B`/`.T` evidence.

E3 must add semantic graph, transfer, effect, and verifier content through the
accepted version/capability process. It must not reopen the physical carrier
decision, bypass the transaction, or treat the identity plan as a CKKS analysis
or optimization result.
