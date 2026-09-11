# FHE SYNC-3 Context-Specific CKKS State Contract

## Purpose

Shared DSL procedure definitions can execute in call contexts with different
cryptographic states. The existing `.WHIRL.dsl_fhe_plan` v1 CKKS value-state
row is keyed by `(value_id, state_version)` and remains correct for values whose
accepted state is context independent. A callsite must not be represented as a
fake state version.

SYNC-3 adds the optional `.WHIRL.dsl_fhe_context_state` image for exact
context-specific planning state. It is append-only: no existing WN, TY,
opcode, ELF section, FHE plan row, or approximation-profile row changes.

## Physical Image

| Property | Contract |
| --- | --- |
| ELF kind | `WT_DSL_FHE_CONTEXT_STATE` (`0x28`) |
| Section name | `.WHIRL.dsl_fhe_context_state` |
| Version | 1 |
| Header | 64 bytes |
| CKKS context-state row | 88 bytes |
| Alignment | 8 bytes |

The row key is:

```text
(owner_pu_st,
 source_value_id,
 context_pu_identity_id,
 context_callsite_id,
 state_role,
 state_version)
```

The row carries the complete machine-verifiable CKKS state payload: encryption
descriptor, scheme, value class, level, scale, component and precision facts,
slot and alignment facts, encrypted layout, pending actions, and bootstrap
reason. `state_version` orders state revisions within one exact context; it
does not identify a callsite or approximation stage.

## Context Identity

`context_pu_identity_id` identifies the callee/source-definition PU.

- Root context: `context_callsite_id == 0` and the identity owner equals
  `owner_pu_st`.
- Called context: the callsite callee, identity owner, and `owner_pu_st` are
  equal. The callsite caller must independently be a valid global function.

This is the same source-context identity used by BatchNorm-fold provenance and
approximation context ranges.

The callee-definition interpretation is introduced with append-only row flags:

- `DSL_FHE_CONTEXT_RANGE_IDENTITY_IS_CALLEE` marks a context-range row whose
  identity owner is the callee/source-definition PU;
- `DSL_FHE_BN_FOLD_CONTEXT_IDENTITY_IS_CALLEE` marks the corresponding
  BatchNorm-fold provenance convention;
- an existing v1 row with the flag clear retains the earlier caller-identity
  interpretation and remains structurally loadable.

New producers set the applicable flag. Context-state association accepts only
callee-tagged context ranges because its source value is owned by the callee.
The current FHE semantic gate must reject legacy caller-identity rows when
Commit 19 requires context-specific CKKS state. This migration changes no row
size, section version, or previously published flag value.

A current reader therefore accepts both legacy and callee-tagged rows. A
previous reader that does not know the new flag fails closed on a callee-tagged
planning row; it must not silently reinterpret that row using caller identity.
This is the established forward-compatibility behavior for an unknown semantic
flag. Physical WHIRL remains available, while FHE planning evidence requires a
reader that understands the new identity contract.

## Composite ReLU Rule

When this optional image is present, every composite approximation context
range has exactly one matching `POST_REFRESH.v1` state and no unmatched
`POST_REFRESH.v1` row is accepted. Other structurally valid roles and later
state versions remain legal. The matched state encryption descriptor's
configuration must equal the composite profile configuration.

`POST_REFRESH` is the target state of the planned mandatory pre-operation
refresh anchored to the source ReLU value and context. In SYNC-3 it is planning
evidence, not evidence that bootstrap executed. A pending bootstrap action with
reason `PRE_RELU_REFRESH` is therefore legal.

The common image validator owns structural correspondence and configuration
agreement. The FHE semantic gate owns ACE-specific policy: allowed levels
`{15,17,18}`, scale bits `56`, component count `2`, minimum required precision
`30`, and the exact pending-refresh obligation.

## Compatibility

Absence of the optional section remains valid for binary WHIRL produced before
this extension. A current FHE certification pass fails closed when its policy
requires context state and the section is absent. Readers that do not know the
new optional section retain their established unknown-extension behavior.

Mapped-image loading validates the complete candidate before replacing managed
tables. `ir_b2a -st -src` prints logical context, role, and CKKS state evidence
without exposing private WN storage.

Compatibility tests retain a legacy caller-identity two-PU row, accept a new
callee-tagged row with the current reader, require the previous reader to fail
closed, reject crossed flag/identity relations, and prove the exact
callee-tagged context-range to `POST_REFRESH.v1` state join.
