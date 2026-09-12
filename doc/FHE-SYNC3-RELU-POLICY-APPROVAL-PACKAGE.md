# FHE SYNC-3 ReLU Composite-Policy Approval Package

Status: Commit 17 policy tuple approved; Commit 19 planning evidence certified

Profile under review:
`ace.chebyshev.sign.7x15x13.depth11.v1`

This package supports Commit 17 and the focused Commit 19 gate of
`doc/FHE-SYNC3-COMMIT-ACCEPTANCE-PLAN.md`. It records project approval of the
exact numeric ACE coefficient profile from empirical ANT ACE evidence, the
approved identity-bound calibration ranges, the held-out clear accuracy run,
and the compiler/static CKKS schedule. It authorizes only the focused SYNC-3
conversion-planning `secure_resnet20.fhe.B`. It does not insert bootstrap,
lower polynomial arithmetic, execute ciphertext inference, or claim SYNC-4.

## Decision Summary

| Gate | Status | Decision or blocker |
| --- | --- | --- |
| Canonical coefficient provenance | Approved from empirical ANT ACE evidence | The exact ACE `7 -> 15 -> 13` Chebyshev coefficient bytes, source revision, file digest, SPDX expression, stage digests, and bundle digest are frozen without fitting or scaling. Open64's independent formal proof is deferred. |
| Nineteen identity-bound ranges | Approved | A 5,000-image class-stratified CIFAR-10 training subset records all 19 exact Open64 identities, extrema, distribution evidence, positive bounds, hashes, and reject-on-outlier policy. |
| Accuracy evidence | Approved for SYNC-3 planning | A disjoint 1,000-image held-out test subset reproduces 91.6% clear and 91.5% polynomial accuracy, 0.1 percentage-point degradation, 99.9% prediction agreement, and zero out-of-range values against thresholds frozen before execution. |
| Concrete CKKS state proof | Approved static compiler schedule | Pinned ACE compiler output and project-reviewed `N=65536`, `Q0=60`, scale 56 configuration establish context-specific post-refresh levels 15, 17, and 18 and depth 11. This is planning evidence, not executed OpenFHE ciphertext inference. |

The complete tuple may now be interned and certified as SYNC-3 planning
evidence. Runtime enablement remains false: the resulting `.fhe.B` records
required composite approximation and pending pre-ReLU refresh, but does not
materialize either operation.

## Gate 1: Coefficient Provenance

The canonical candidate is the static ACE array
`App_sign_comp_poly_chebyshev_alfa6_depth11`:

| Field | Evidence |
| --- | --- |
| Repository | `https://github.com/ant-research/ace-compiler` |
| Audited revision | `fb76131171b9f82aa6387f84dd73684fba5277e8` |
| Introduction revision | `15c95a7346d89355d68d5bf4fe8ab3b952740e1a` |
| Source | `fhe-cmplr/util/src/app_composite_poly.cxx` |
| Source SHA-256 | `bd752b96eaaef5e4f14851c057d38cee9e45264e230dbb637dd59ba3cabb0f93` |
| Source SPDX | `Apache-2.0 WITH LLVM-exception` |
| Basis/order | Chebyshev, direct `c0*T0 + ... + cn*Tn`, `T0` through `Tdegree` |
| Storage | C++ `double`, canonical IEEE-754 binary64, little-endian evidence bytes |
| Stage SHA-256 | degree 7: `6bcab92ecd5198ae14d21a633e2767758d8f53145d136b1d578f2c821a95f736` |
| Stage SHA-256 | degree 15: `f509c310ab54d31aa7f61e4bdc18f896dd920bcde01bc9c9d41ae95902f4545a` |
| Stage SHA-256 | degree 13: `7768d20e17d427ece7f1ed572f7f065a606e40ee7e1f081253222376ad93378f` |
| Concatenated bundle SHA-256 | `d4e7f691fe763d5673384e23e0bc825875e49de545df78d7f7d7b2ca5e613438` |

ACE stores raw sign-approximation stage coefficients. Its SIHE lowering later
halves the outer-stage coefficients, multiplies them by the original `x`, and
adds `0.5*x` to implement:

```text
relu(x) ~= 0.5*x*P13(P15(P7(x/B))) + 0.5*x
```

The candidate manifest therefore preserves the raw source coefficients. It
does not pre-scale the final stage. The decimal-to-byte protocol is explicit:
parse each pinned C++ decimal token as nearest IEEE-754 binary64, serialize
little-endian in `T0..Tdegree` order, then hash each stage and their ordered
concatenation. This is transparent extraction, not coefficient derivation.

Project decision: the pinned source, SPDX evidence, direct Chebyshev `c0`
convention, binary64 encoding, and unscaled raw stage bytes are approved based
on empirical ANT ACE implementation evidence. Open64 has not independently
proved the approximation, and this approval establishes no trained
SecureResNet accuracy result. Formal proof and broader reproducibility remain
deferred, reviewable evidence.

## Gate 2: Nineteen Context Ranges

The range manifest enumerates exactly one root context and eighteen called
contexts. Each key is:

```text
(profile_name,
 owner_pu_st,
 source_relu_value_id,
 context_pu_identity_id,
 context_callsite_id)
```

The 11 reusable physical `common.relu` definitions expand to 19 source
contexts through the nine persisted callsites. Shared physical values retain
distinct callsite identities; no name or tensor-shape fallback is allowed.

The accepted collector uses the pinned ACE-derived Open64 checkpoint
`75fb9294272845b19eaeea3e1ee644d289536ea6711b27ec9527e658f5d20ff5`.
Original training provenance is unknown and is not claimed. It observes a
deterministic class-stratified 5,000-image subset of the canonical CIFAR-10
training split after the frozen ACE preprocessing. For each identity it
records minima, maxima, sample and element counts, tail quantiles, nonfinite
and outlier counts, and `B = 1.1 * max(abs(min), abs(max))`. Runtime policy is
to reject values outside the approved bound. The official CIFAR-10 test split
is excluded from calibration and reserved for held-out acceptance.

## Gate 3: Accuracy Evidence

Accuracy execution ran after Gates 1 and 2 with the same trained checkpoint and
preprocessing twice:

```text
clear common.relu baseline
clear 0.5*x*P13(P15(P7(x/B)))+0.5*x emulation
```

The predeclared gates required a credible clear baseline, at most 1.0
percentage-point polynomial degradation, at least 98% prediction agreement,
and zero out-of-range values. The disjoint 1,000-image result is 91.6% clear,
91.5% polynomial, 0.1 percentage-point degradation, 99.9% agreement, and zero
out-of-range values. Logit and activation errors remain report-only evidence;
no threshold was changed after results were visible.

## Gate 4: CKKS State Proof

The accepted static compiler schedule requires pre-ReLU refresh with reason
`PRE_RELU_REFRESH`, normalization by positive `B`, ordered degrees
`7 -> 15 -> 13`, total multiplicative depth 11, and ReLU reconstruction. The
selected contract uses CKKS security class 128-classic, `N=65536`, 32768
slots, depth 33, `Q0=60`, scale 56, two components, and minimum precision 30.
The exact 19 context states preserve ACE post-refresh levels 15, 17, or 18.

The symbolic/static schedule is internally consistent and accepted for this
conversion-planning checkpoint. Executed OpenFHE ciphertext state and numerical
precision remain a later runtime/lowering certification gate.

## Native API Approval Checklist

| Evidence | Native mapping after approval | Current action |
| --- | --- | --- |
| Profile name/version, reconstruction, total depth, normalization, refresh, source revision, coefficient-manifest hash | `DSL_FHE_COMPOSITE_PROFILE_RECORD` passed to `DSL_FHE_Approx_Profile_Intern_Complete()` | Integrated from approved empirical ACE profile |
| Three ordered coefficient tensors, byte hashes, basis, degree, evaluator, scale/level policies | Three `DSL_FHE_APPROX_STAGE_RECORD` rows passed atomically with the profile | Complete: ordered degrees `7,15,13`, depth 11, and exact stage hashes reopen |
| One composite disposition per live source ReLU | `DSL_FHE_Plan_Add_Composite_Disposition()` | Complete: 11 reusable definition dispositions cover 19 source contexts |
| Nineteen exact value/PU/callsite bounds and extrema | `DSL_FHE_Approx_Profile_Bind_Context_Range()` | Complete: 19 callee-tagged approved context rows |
| Context-specific level, scale, components, precision, pending refresh reason | `DSL_FHE_Context_State_Intern()` | Complete: one exact `POST_REFRESH.v1` row per range, with levels 15/17/18 |
| Reopen and cross-table evidence | profile/stage/association/context/state APIs plus `ir_b2a -st -src` | Complete for the focused SYNC-3 planning artifact |

The approved tuple authorizes the focused Commit 19 planning image and its
atomic payload/report publication. It does not authorize SYNC-4 bootstrap or
polynomial materialization, standard-WHIRL runtime lowering, or OpenFHE
execution.

## Machine-Readable Evidence

| Artifact | SHA-256 |
| --- | --- |
| `doc/fhe-policy/sync3-relu/coefficient-manifest.json` | `75132d449852303ec3e44e86c8a5b5ffc196c0643cf7fadff453d797c2266931` |
| `doc/fhe-policy/sync3-relu/range-manifest.json` | `55dcb4ec993a9f09901eaf61aa49dde8628e3eb032a24429a40715fc3fc4113c` |
| `doc/fhe-policy/sync3-relu/accuracy-manifest.json` | `1fd33f514363cf5b1f4f092e5553cb4e77d0f03eff3bbed4852ca0a6ae32ea25` |
| `doc/fhe-policy/sync3-relu/ckks-schedule-manifest.json` | `27fe104aa5a159baefd0255c82e0c9193c1ecdf28b73f97e2f8830bb1444ae62` |

`doc/fhe-policy/sync3-relu/package-index.json` binds those paths, hashes, gate
statuses, permits SYNC-3 planning publication, and keeps runtime enablement
false. The manifest test independently rebuilds
the coefficient bytes and hashes, validates all 19 identities, and rejects
missing stages, changed hashes, duplicate identities, incomplete range or
accuracy evidence, and incomplete CKKS schedules.

## Approval Record

- [x] Coefficient source, revision, license provenance, decimal order,
  binary64 bytes, and reconstruction interpretation accepted from empirical
  ANT ACE evidence.
- [x] Trained checkpoint, calibration data, preprocessing, bound rule, and
  outlier policy accepted before capture.
- [x] Accuracy dataset, metrics, and thresholds accepted before execution.
- [x] CKKS configuration and compiler/static depth/state schedule accepted for
  SYNC-3 planning.
- [x] All four final manifest hashes reviewed as one immutable tuple.
- [x] FHE conversion may replace `CFHECNN-RELU-003` with complete
  context-bound composite planning.
- [ ] OpenFHE ciphertext execution confirms the planned schedule; deferred to
  the runtime/lowering milestone and not a Commit 19 blocker.

Commit 17 is complete and Commit 19 may close after exact-candidate integration
review. The unchecked runtime item remains mandatory later but does not block
the focused SYNC-3 conversion-planning checkpoint.
