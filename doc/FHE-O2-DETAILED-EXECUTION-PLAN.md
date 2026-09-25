# Open64 FHE O2 Detailed Execution Plan

Status: Proposed plan set; implementation is not authorized by this document

Plan set version: 0.2

Governing O2 plan: `FHE-O2-ACE-RESBM-METAKERNEL-INTEGRATION-PLAN.md`, version 1.6

## 1. Purpose

This document is the navigation and execution-control index for the detailed
O2 stage plans. Each linked stage document is independently executable after
its entry gates are accepted. This index does not replace the governing O2
plan, resolve a pending master-plan decision, authorize implementation, or
change any support, runtime, security, or completion contract.

All commands in the stage documents are proposed target commands unless the
document explicitly identifies an already-existing command. A future command
must be implemented and reviewed before its result can be accepted as evidence.

## 2. Authority and Activation

The effective authority order is:

1. Explicit accepted user decisions and repository rules in `AGENTS.md`.
2. The accepted master plan and the accepted O0/O2 ownership ADR.
3. The governing O2 integration plan, including its detailed stage contract.
4. This index and the individual stage plans.

The successor master plan and the ownership ADR remain pending. Consequently,
`PRE-O2-LOCK` remains open. Planning and review may continue, but implementation
must not start until the applicable entry gates are accepted.

The O2 activation rule is absolute:

```text
PRE-O2-LOCK-EXIT
  -> execute O2-E0 only
  -> every O0 qualification row passes
  -> publish O2-O0Q-001=Qualified
  -> O2-E1 may start
```

No O2 source change, production fixture, oracle implementation, provider-free
algorithm implementation, or runtime implementation may start before the full
O0 qualification record is current and accepted. O0 remediation remains O0
work and must be followed by complete requalification.

## 3. Stage Plan Catalog

| Stage | Detailed plan | Governing milestones | Required entry | Exit gate |
| --- | --- | --- | --- | --- |
| `PRE-O2-LOCK` | [Pre-O2 contract lock](fhe-o2-stages/pre-o2-contract-lock.md) | `F0`, `FRZ-01..09` | Planning authorities available for review | `PRE-O2-LOCK-EXIT` |
| `O2-E0` | [Complete O0 baseline qualification](fhe-o2-stages/e0-o0-baseline-qualification.md) | consume `F1-PREP/P0/P1a/P1b/F1-IMPL/F1-ACCEPT` | `PRE-O2-LOCK-EXIT` | `O2-E0-EXIT` and `O2-O0Q-001=Qualified` |
| `O2-E1` | [Claims, fixtures, and oracles](fhe-o2-stages/e1-claims-fixtures-oracles.md) | `S1.0` | current `O2-O0Q-001=Qualified` | `O2-E1-EXIT` |
| `O2-E2` | [Records and identity transaction](fhe-o2-stages/e2-records-identity-transaction.md) | `P2`, `S1.1` | accepted E1 | `O2-E2-EXIT` |
| `O2-E3` | [Open64 CKKS semantic substrate](fhe-o2-stages/e3-open64-ckks-semantic-substrate.md) | `S1.2`, semantic `S1.5A` substrate | accepted E2 | `O2-E3-EXIT` |
| `O2-E4` | [MetaKernel layout](fhe-o2-stages/e4-metakernel-layout.md) | `S1.3`, `S1.4`, early `S1.LAYOUT` | accepted E3 | `O2-E4-EXIT` |
| `O2-E5A` | [Selected-layout pre-ReSBM state](fhe-o2-stages/e5a-pre-resbm-state.md) | `S1.5A` integration | accepted E4 | `O2-E5A-EXIT` |
| `O2-E5B` | [ReSBM core and protected adapter](fhe-o2-stages/e5b-resbm-protected-adapter.md) | `S1.6`, `S1.7` | accepted E5A | `O2-E5B-EXIT` |
| `O2-E5C` | [ANT projection and materialization](fhe-o2-stages/e5c-ant-projection-materialization.md) | `S1.5B`, `S1.8` | accepted E5B | `O2-E5C-EXIT` |
| `O2-E6` | [Stage 1 acceptance](fhe-o2-stages/e6-stage1-acceptance.md) | final `S1.LAYOUT`, `S1.9` | accepted E5C | `O2-E6-EXIT` |
| `O2-E7A` | [FHEFusion](fhe-o2-stages/e7a-fhefusion.md) | `S2.0`, `S2.1`, `S2.2` | accepted E6 | `O2-E7A-EXIT` |
| `O2-E7B` | [HPOLY and HPAO](fhe-o2-stages/e7b-hpoly-hpao.md) | `S2.3`, `S2.4` | accepted E6 and representation ADR | `O2-E7B-EXIT` |
| `O2-E8` | [Final certification and O3 handoff](fhe-o2-stages/e8-final-o3-handoff.md) | `S2.5`, `S2.6` | accepted E7A and E7B | `O2-E8-EXIT` |

## 4. Implementation and Build Dependency Graph

```text
PRE-O2-LOCK
  -> O2-E0
  -> O2-E1
  -> O2-E2
  -> O2-E3
  -> O2-E4
  -> O2-E5A
  -> O2-E5B
  -> O2-E5C
  -> O2-E6
  -> O2-E7A ----\
                   -> O2-E8
  -> O2-E7B ----/
```

E7A and E7B may be implemented and certified independently after E6. E8 alone
owns their composition, cross-stage invalidation, complete replanning, and final
O2 certification.

## 5. Compiler Pass Order

The implementation dependency graph is not the compiler pass order. The
accepted compiler order remains:

```text
canonical Open64 FHE graph
  -> FHEFusion
  -> recanonicalization and derived-fact invalidation
  -> MetaKernel layout selection
  -> canonical selected-layout pre-ReSBM CKKS state
  -> baseline DP import
  -> optional ReSBM replacement
  -> post-ReSBM state verification
  -> final CKKS parameters and security
  -> HPOLY construction and HPAO
  -> ANT projection and standard-call materialization
  -> whirl2c and generated C
```

Stage plans may establish reusable infrastructure earlier than the pass that
uses it. They must not use that implementation order to bypass semantic
invalidation or change the compiler pass order.

## 6. Global Invariants

- Open64 owns CKKS-and-above semantic IR, planning, verification, and records.
- ACE rtlib is the runtime interface and ANT is the fixed provider.
- Release execution uses the public C surfaces `FHErt_common` and `FHErt_ant`
  with provider `LIB_ANT`; `LIB_ACE` is not an accepted substitute.
- No additional opaque `dsc_fhe_*` facade is assumed by this plan set.
- A direct ACE rtlib dependency from `be.so` is not authorized. Any proposal
  requires an explicit design, build, and complete consumer-link review.
- `N` is fixed before canonicalization and cannot be silently increased.
- `MulCC` produces a transient three-component value and is followed by one
  immediately adjacent `Relin`; duplicate or delayed relinearization rejects.
- Server evaluation has no secret key, decryptor, decrypt call, or retained
  secret-derived material.
- Binary WHIRL is the stable process boundary. Every meaningful `.B` artifact
  has a same-stem `.T` generated with `ir_b2a -st -src`.
- No private FHE, O2, or HPOLY operation may reach `whirl2c`.
- Failure never publishes a partial valid-named plan. Rejection and fallback
  are explicit, atomic, and evidence-bound.

## 7. Plan Use and Maintenance

For each stage:

1. Reopen and verify every authority, source, schema, provider, build, and entry
   fingerprint named by the stage plan.
2. Assign the named owners and independent reviewers.
3. Execute the WBS and commit sequence without combining review boundaries.
4. Implement and review proposed verification tooling before relying on it.
5. Run every required positive, negative, compatibility, security, failure,
   determinism, and evidence row.
6. Publish only `Accepted`, `Rejected`, or `Unverified` with the complete evidence
   bundle and content hashes.
7. Unlock the next stage only for a current `Accepted` exit decision.

Any change to the master authority, O0 qualification, source lock, support
scope, record schema, runtime boundary, provider identity, security lifecycle,
compiler pass order, or completion definition invalidates the affected stage
and every downstream consumer. Update the governing authority first, then
perform traceability review and rerun the required qualification or acceptance
chain. An individual stage plan cannot authorize such a change by itself.
