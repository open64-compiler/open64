# FHE SYNC-3 ReLU Composite-Policy Approval Package

Status: reviewable evidence package; compiler enablement blocked

Profile under review:
`ace.chebyshev.sign.7x15x13.depth11.v1`

This package supports Commit 17 of
`doc/FHE-SYNC3-COMMIT-ACCEPTANCE-PLAN.md`. It records project approval of the
exact numeric ACE coefficient profile from empirical ANT ACE evidence. It does
not approve model-specific ranges or trained-model accuracy, insert bootstrap,
lower polynomial arithmetic, or authorize publication of a ReLU-bearing
`secure_resnet20.fhe.B`.

## Decision Summary

| Gate | Status | Decision or blocker |
| --- | --- | --- |
| Canonical coefficient provenance | Approved from empirical ANT ACE evidence | The exact ACE `7 -> 15 -> 13` Chebyshev coefficient bytes, source revision, file digest, SPDX expression, stage digests, and bundle digest are frozen without fitting or scaling. Open64's independent formal proof is deferred. |
| Nineteen identity-bound ranges | Blocked | All 19 Open64 identity keys are enumerated, but the current SecureResNet fixture has synthetic weights and only an all-ones sample. An approved trained checkpoint, calibration split, preprocessing, bound estimator, and safety/outlier policy are missing. |
| Accuracy evidence | Blocked | The dataset/checkpoint protocol and numerical thresholds have not been approved in advance. No accuracy or tolerance claim can be made from the synthetic fixture. |
| Concrete CKKS state proof | Blocked | ACE's symbolic depth-11 label is not a concrete OpenFHE level/scale/precision proof. One OpenFHE configuration and evaluator schedule must be selected and executed. |

The coefficient profile may now be interned and certified independently. The
correct model decision remains **do not enable SecureResNet ReLU conversion**:
preserve fail-closed behavior at the range/calibration gate and publish no full
SecureResNet FHE binary.

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
 source_relu_value_id,
 context_pu_identity_id,
 context_callsite_id)
```

The 11 reusable physical `common.relu` definitions expand to 19 source
contexts through the nine persisted callsites. Shared physical values retain
distinct callsite identities; no name or tensor-shape fallback is allowed.

The current source fixture cannot certify ranges. It initializes synthetic
parameters by a deterministic arithmetic pattern and exposes one all-ones
sample input. Before instrumentation runs, reviewers must freeze:

1. A trained ResNet-20/CIFAR-10 checkpoint and SHA-256.
2. An immutable calibration dataset/split, sample order, and dataset hash.
3. Exact normalization, resize/crop, dtype, and layout preprocessing with a
   hashed executable specification.
4. A bound estimator, safety margin, and `reject` out-of-range behavior.
5. A separate acceptance set or a documented non-tuning role for acceptance
   data.

After approval, clear/reference instrumentation records pre-ReLU minima,
maxima, element sample counts, and the selected positive `B` against the exact
19 keys. Calibration chooses bounds; acceptance data only evaluates the
already frozen policy.

## Gate 3: Accuracy Evidence

Accuracy execution is ordered after Gates 1 and 2. The comparison must run the
same trained checkpoint and preprocessing twice:

```text
clear common.relu baseline
clear 0.5*x*P13(P15(P7(x/B)))+0.5*x emulation
```

Before either run, reviewers must approve minimum sample count, maximum top-1
drop, maximum logit `Linf`, and per-context ReLU `Linf`/`L2` limits. The run
manifest then records baseline and emulated top-1, logit differences,
activation errors, out-of-range count, software lock, and every input hash.
No threshold may be selected after results are visible.

## Gate 4: CKKS State Proof

The symbolic obligations are known: required pre-ReLU refresh with reason
`PRE_RELU_REFRESH`, normalization by positive `B`, ordered degrees
`7 -> 15 -> 13`, claimed total multiplicative depth 11, and ReLU
reconstruction. These facts are not yet a concrete state schedule.

Reviewers must select one OpenFHE revision and CKKS configuration, including
security level, ring dimension, slots, modulus chain, scaling technique,
bootstrap parameters, and secret-key distribution. For normalization, each
stage, and reconstruction, the completed manifest must state input/output
level, scale, component count, minimum precision, evaluator, and level
consumption. The consumptions must sum to 11 under the profile contract.

Two proofs are required and remain distinct:

- Symbolic proof: the schedule transitions are internally consistent and meet
  every stage/profile contract.
- Executed proof: a focused OpenFHE run observes compatible state and numerical
  precision for the exact configuration and coefficient/range manifests.

## Native API Approval Checklist

| Evidence | Native mapping after approval | Current action |
| --- | --- | --- |
| Profile name/version, reconstruction, total depth, normalization, refresh, source revision, coefficient-manifest hash | `DSL_FHE_COMPOSITE_PROFILE_RECORD` passed to `DSL_FHE_Approx_Profile_Intern_Complete()` | Blocked until all four manifests form one approved tuple |
| Three ordered coefficient tensors, byte hashes, basis, degree, evaluator, scale/level policies | Three `DSL_FHE_APPROX_STAGE_RECORD` rows passed atomically with the profile | Coefficient bytes frozen; evaluator and concrete state policies blocked |
| One composite disposition per live source ReLU | `DSL_FHE_Plan_Add_Composite_Disposition()` | Continue range-specific `CFHECNN-RELU-003`; do not insert model rows |
| Nineteen exact value/PU/callsite bounds and extrema | `DSL_FHE_Approx_Profile_Bind_Context_Range()` | Identity keys enumerated; numeric records blocked |
| Value-specific level, scale, components, precision, pending refresh reason | `DSL_FHE_Plan_Add_CKKS_Value_State()` and lookup APIs | Symbolic skeleton only; concrete rows blocked |
| Reopen and cross-table evidence | profile/stage/association/context count/get/find APIs plus `ir_b2a -st -src` | Run only after accepted rows exist |

Approval is atomic: no subset authorizes a native profile row, disposition,
context binding, or full `.fhe.B` publication.

## Machine-Readable Evidence

| Artifact | SHA-256 |
| --- | --- |
| `doc/fhe-policy/sync3-relu/coefficient-manifest.json` | `75132d449852303ec3e44e86c8a5b5ffc196c0643cf7fadff453d797c2266931` |
| `doc/fhe-policy/sync3-relu/range-manifest.json` | `f3877a0dc98360b39f27ea6d7dc27eb28849d3df190fd1214808b306b2df7dd2` |
| `doc/fhe-policy/sync3-relu/accuracy-manifest.json` | `a6c7992462e590804564d0bddd13704ca2a7ebe7cf587c67645b1c575336c530` |
| `doc/fhe-policy/sync3-relu/ckks-schedule-manifest.json` | `95d7b63c7a7c509fdd9115bcef1c067009c90e7f8b63fe1357262ba320da342f` |

`doc/fhe-policy/sync3-relu/package-index.json` binds those paths, hashes, gate
statuses, and the fail-closed result. The manifest test independently rebuilds
the coefficient bytes and hashes, validates all 19 identities, and rejects
missing stages, changed hashes, duplicate identities, incomplete range or
accuracy evidence, and incomplete CKKS schedules.

## Approval Record

- [x] Coefficient source, revision, license provenance, decimal order,
  binary64 bytes, and reconstruction interpretation accepted from empirical
  ANT ACE evidence.
- [ ] Trained checkpoint, calibration data, preprocessing, bound rule, and
  outlier policy accepted before capture.
- [ ] Accuracy dataset, metrics, and thresholds accepted before execution.
- [ ] OpenFHE configuration and depth/state schedule accepted and executed.
- [ ] All four final manifest hashes reviewed as one immutable tuple.
- [ ] FHE conversion may replace `CFHECNN-RELU-003` with complete
  context-bound composite planning.

Until every box is checked, Commit 17 remains evidence preparation, Commit 19
remains blocked, and the certified ReLU-free BatchNorm path is preserved.
