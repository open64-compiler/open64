# FHE SYNC-C Composite Approximation Image Contract

Status: main/common implementation in progress; FHE consumer contract reviewed.

## Purpose

The selected ResNet ReLU policy is an ordered composition of polynomial
stages. The existing version-1 `DSL_FHE_APPROXIMATION_CONTRACT_RECORD`
describes exactly one polynomial and remains unchanged. It must not be
expanded, overloaded, or populated with a fake summary polynomial.

SYNC-C adds the optional `.WHIRL.dsl_fhe_approx_profile` mapped-image section.
Its records preserve composite profile identity, ordered machine-verifiable
stage policy, an explicit disposition-to-profile association, and exact
source/context range bindings. The section contains fixed-width rows, no
pointers, and no STL objects. Rows and the ELF section use 8-byte alignment.

## Compatibility

- `WT_DSL_FHE_APPROX_PROFILE` is `0x27` and does not alter existing sections.
- Absence of the optional section preserves legacy behavior.
- Disposition values `1` through `5` retain their published meanings.
- `DSL_FHE_DISPOSITION_REQUIRE_COMPOSITE_APPROXIMATION` is append-only value
  `6`. Its existing `approximation_contract_id` field is a tagged reference to
  a composite profile. For value `5`, that field continues to reference the
  version-1 single-polynomial table.
- A previous reader rejects disposition value `6` rather than interpreting the
  profile as a version-1 polynomial.
- The binary WHIRL revision, WN representation, opcode encoding, TY encoding,
  and existing FHE image layouts do not change.

## Fixed Rows

| Row | Bytes | Required semantics |
| --- | ---: | --- |
| Header | 64 | Magic, version, header size, four capabilities, flags, four counts, and zero reserved fields. |
| Composite profile | 80 | Config, stable name/version, reconstruction, total depth, normalization, pre-refresh policy, source revision, manifest SHA-256, and one immutable contiguous stage range. |
| Ordered stage | 80 | Profile and dense ordinal, family, basis, degree, evaluation scheme, symbolic input requirements, level consumption, symbolic output policy, minimum precision, coefficient TCON, and coefficient-byte SHA-256. |
| Profile association | 32 | Exact disposition, source ReLU value, profile, and owning PU. |
| Context range | 64 | Profile, exact source ReLU value, owner PU, PU identity, optional callsite, positive bound, observed extrema, provenance, and reject-on-out-of-range policy. |

Stage state is symbolic policy. It does not reference
`DSL_FHE_CKKS_VALUE_STATE_ID`, because that identity belongs to an actual value,
context, and state version. SYNC-4 binds materialized stage values to concrete
CKKS state rows.

The stage evaluator is an algorithm contract (`clenshaw`,
`paterson_stockmeyer`, or `addition_chain`), not the stage's role in the
composition. Profile membership and order already carry that role. Input scale
is either any compatible scale or the profile-normalized scale; output scale
either preserves the input scale or uses the configured default rescale.
Version 1 has no scale-bit field and therefore does not admit an unresolved
`explicit` scale policy.

## Construction

`DSL_FHE_Approx_Profile_Intern_Complete(profile, stages, count)` prevalidates
the complete profile and all ordered stages before changing any table. Stage
ordinals are assigned densely from zero and their rows remain contiguous.
Contexts are not part of this range because they are discovered across PUs.
Profile identity and lookup use `(config_id, profile_name, profile_version)`;
the same mathematical profile may therefore have independently verified
realizations under different FHE compilation configurations without renaming
the algorithm or changing its version.

`DSL_FHE_Plan_Add_Composite_Disposition(disposition, profile_id)` atomically
adds the value-6 disposition and its explicit association. The ordinary
single-record disposition API rejects value `6`, preventing a transient
unassociated composite disposition.

`DSL_FHE_Approx_Profile_Bind_Context_Range(record)` appends an independently
keyed context. Its key is:

```text
(profile_id,
 source_relu_value_id,
 context_pu_identity_id,
 context_callsite_id)
```

For a root context, `context_callsite_id` is zero and the PU identity owner
must equal `owner_pu_st`. For a called context, the callsite must name
`owner_pu_st` as callee and its caller must equal the PU identity owner. The
current SecureResNet contract expects one root and eighteen called contexts.

## Validation

Validation proves:

1. Every profile and stage identity is dense and every profile owns exactly
   its declared ordered stage range.
2. Checksums are lowercase 64-character hexadecimal strings and referenced
   coefficient TCONs are rank-1 floating tensors with exactly `degree + 1`
   elements.
3. Stage state fields use known enums and valid signed-level conventions.
   The sum of stage level consumption equals the profile's declared total
   multiplicative depth. The final stage's consumption includes any
   normalization or reconstruction multiplication assigned to the profile,
   so the declared depth has one unambiguous accounting scope.
4. Every association resolves to one value-6 disposition and the same live
   `common.relu` result, profile, owner PU, and FHE compilation config selected
   by the result CKKS state.
5. Every context resolves to exactly one association and a live
   `common.relu` result.
6. Context keys are unique; bounds are positive and contain the recorded
   observed extrema; missing or out-of-range evidence rejects.
7. Every value-6 disposition has exactly one association and at least one
   context before binary WHIRL publication.
8. Unknown flags, reserved fields, enums, record sizes, section sizes, and
   capabilities reject before pass use.

## Inspection

`ir_b2a -st -src` prints logical evidence under:

- `FHE Composite Approximation Profile Image`
- `FHE Composite Profile Table`
- `FHE Ordered Approximation Stage Table`
- `FHE Composite Approximation Association Table`
- `FHE ReLU Context Range Table`

Disposition value `6` prints `composite_profile=<id>`. The private WN escape
representation remains hidden.

## Staging Boundary

This infrastructure commit defines representation, mapped-image movement,
validation, lookup, and inspection. It does not encode ACE coefficients,
approve range evidence, insert bootstrap, evaluate polynomial stages, or lower
to OpenFHE. Until the FHE task completes those certification gates, full
SecureResNet conversion remains fail-closed at `CFHECNN-RELU-002`.
