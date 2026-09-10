# FHE ReLU Composite-Polynomial Policy Decision

Status: composite architecture selected; coefficient certification pending;
compiler enablement remains fail-closed.

The filename is retained for review-link compatibility. Degree 3 is no longer
the recommended ResNet-20 baseline.

## Decision

The first CKKS ResNet-20 baseline will use an ACE-compatible composite
Chebyshev approximation of `sign(x)` with ordered stage degrees
`7 -> 15 -> 13`, followed by

```text
relu(x) ~= 0.5 * x * (composite_sign(x / B) + 1)
```

where `B` is an approved positive range bound for the source ReLU context.
Every surviving `common.relu` remains the source-semantic operator. At `-O0`,
the compiler first materializes the mandatory CKKS bootstrap boundary with
reason `PRE_RELU_REFRESH`, then normalizes by `B`, evaluates the three ordered
Chebyshev stages, and reconstructs the ReLU result with the original `x`.
Bootstrap restores ciphertext capacity; it does not compute ReLU.

The initial candidate profile is named
`ace.chebyshev.sign.7x15x13.depth11.v1`. The name intentionally states the
basis, ordered stages, and claimed multiplication depth instead of relying on
ACE's undocumented `alpha6` label.

This direction supersedes the earlier recommendation to use a single cubic as
the acceptance baseline. A degree-3 profile may remain available later as an
experimental low-depth option, but it cannot satisfy ResNet-20 acceptance
without its own range, error, model-accuracy, and CKKS certification.

## ACE Source Audit

The audited ACE checkout was
`fb76131171b9f82aa6387f84dd73684fba5277e8`. The candidate coefficients are in
`fhe-cmplr/util/src/app_composite_poly.cxx`; that file has SHA-256
`bd752b96eaaef5e4f14851c057d38cee9e45264e230dbb637dd59ba3cabb0f93`.
Its coefficient history begins with ACE commit
`15c95a7346d89355d68d5bf4fe8ab3b952740e1a`.

ACE performs ReLU as compiler-generated arithmetic rather than as a secure
runtime-library ReLU primitive:

1. `tensor2sihe_impl.h` inserts bootstrap before each encrypted ReLU.
2. The input is normalized by a model/operator range bound.
3. `app_composite_poly.cxx` supplies ordered sign-polynomial stages.
4. `tensor2sihe_impl.cxx` builds `0.5*x*sign(x/B) + 0.5*x` from SIHE
   arithmetic.
5. `Relu_msg` is plaintext validation support. `Real_relu` decrypts and
   re-encrypts and therefore is not an acceptable production implementation.

ACE's default Chebyshev profile uses degrees 7, 15, and 13 and declares
multiplication depth 11. The generated ResNet-20 fixture has 19 ReLU calls and
19 pre-ReLU bootstraps. Its range profile uses a default bound of 3 and
per-operator overrides, but Open64 must not copy ACE's name-based/default
fallback policy. Bounds must attach to exact Open64 source/context identities
and out-of-range or missing evidence must fail closed.

## Policy Contract

The approved profile tuple must contain all of the following:

```text
profile_name_and_version
source_repository_revision_and_file_sha256
basis = chebyshev
coefficient_order = T0_through_Tdegree
stage_degrees = [7, 15, 13]
stage_coefficient_binary64_bytes_and_sha256
stage_evaluation_scheme
claimed_multiplicative_depth = 11
normalization_rule = x_divided_by_positive_context_bound
context_bound_table_and_provenance
out_of_range_policy = reject
bootstrap_before_relu = required
bootstrap_reason = PRE_RELU_REFRESH
reconstruction = 0.5*x*composite_sign_plus_0.5*x
clear_sign_error_domain_and_bound
clear_relu_error_norm_and_bound
post_bootstrap_level_scale_precision_contract
plaintext_resnet20_top1_tolerance
ckks_vs_plaintext_logit_and_top1_tolerance
calibration_and_acceptance_dataset_hashes
```

Coefficient interning excludes source position and call context. Context range
bindings do not: each of the 19 source contexts must retain its exact
`DSL_IR_VALUE_ID`, source position, instance path, range evidence, and selected
profile. Compatible contexts may share coefficient storage after those joins
are proven.

## Current Representation Gap

The version-1 `DSL_FHE_APPROXIMATION_CONTRACT_RECORD` stores one degree, one
coefficient tensor, one range, and one depth. It cannot represent three ordered
polynomial stages without losing stage order, basis, coefficient identity, or
evaluation obligations. Open64 must not flatten the composition to an
effective degree or overload `polynomial_name` with hidden semantics.

Before compiler enablement, main/common must review an append-only composite
profile representation. The minimum semantic surface is:

| Record | Required identity and fields |
| --- | --- |
| Composite profile | Stable profile name/version, reconstruction rule, total depth, normalization policy, pre-refresh requirement, source revision, manifest SHA-256 |
| Ordered stage | Parent profile ID plus stage ordinal; basis, degree, coefficient TCON, coefficient-byte SHA-256, evaluation scheme, stage input/output contract |
| Context range binding | Source ReLU value/context identity, profile ID, positive bound `B`, bound provenance, observed calibration extrema, out-of-range policy |

The physical layout, section ownership, and APIs remain main/common decisions.
No FHE task code may change the existing v1 row or mapped-image layout.

## Certification Gates

Architecture selection does not by itself approve coefficient bytes. Compiler
enablement requires all gates below:

1. Pin candidate decimals as canonical binary64 bytes and record their checksum
   and ACE provenance in a review artifact, not enabled compiler code.
2. Independently evaluate all three Chebyshev stages; verify coefficient count,
   finite values, odd symmetry, stage order, and declared clear error bounds.
3. Capture pre-ReLU ranges for all 19 SecureResNet source contexts and bind each
   range to exact Open64 identity records. No name-only or silent-default
   lookup is permitted.
4. Run the polynomial-substituted plaintext ResNet-20 on pinned calibration and
   CIFAR-10 acceptance datasets and enforce approved logit/top-1 tolerances.
5. Prove the depth-11 evaluation schedule and post-bootstrap CKKS
   level/scale/precision state using the selected OpenFHE parameters.
6. Run focused OpenFHE numerical tests and complete encrypted ResNet inference.
7. Only after review of retained evidence may `CFHECNN-RELU-002` be removed for
   this exact profile.

## Fail-Closed Behavior

Until the representation and certification gates close:

- SecureResNet checkpoint publication stops with `CFHECNN-RELU-002`;
- no zero, cubic, or ACE coefficients are silently substituted;
- no `.fhe.B` is published for the full ReLU-bearing model;
- the ReLU-free six-PU fixture remains the positive BatchNorm-fold
  certification artifact; and
- no bootstrap, SIHE arithmetic, or OpenFHE runtime lowering is introduced in
  SYNC-3.
