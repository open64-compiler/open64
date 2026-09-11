# FHE SYNC-3 Identity-Bound ReLU Range Calibration

Status: calibration machinery, authenticated all-PU binding, and structural
validation are implementation-complete locally after PR #128. Trained-model
calibration evidence is not yet available; `CFHECNN-RELU-003` remains required
without an approved manifest.

## Purpose and boundary

This checkpoint collects the range of the tensor entering each source
`common.relu`. It does not infer identity from tensor shape, module name alone,
or call order alone. Every observation is joined to the complete persisted
Open64 identity:

```text
(owner_pu_st,
 source_relu_value_id,
 context_pu_identity_id,
 context_callsite_id)
```

The clear-model routing key `(module_path, invocation_ordinal)` only locates an
observation hook. It is not a compiler identity and is never sufficient for
native range binding. The committed template enumerates exactly one root ReLU
and eighteen called contexts. Shared compiler PUs therefore retain distinct
callsite observations even when they reuse the same source ReLU value ID.

This commit does not certify full SecureResNet FHE. The repository has no
approved trained SecureResNet20 checkpoint or immutable CIFAR-10 calibration
input locally. The deterministic source fixture and generated inputs certify
the collector machinery only.

## Collection algorithm

`IdentityBoundReluCalibrationCollector` performs these steps:

1. Validate exactly 19 unique persisted identities and 19 unique clear-model
   routes. Require one root callsite ID of zero and contiguous invocation
   ordinals for every reused ReLU module.
2. Resolve every `module_path` in the supplied clear PyTorch model and install
   a forward-pre-hook. The pre-hook observes the ReLU input, not its
   nonnegative output.
3. Run the model in evaluation and no-gradient modes. Version 1 requires one
   declared sample per invocation so sample IDs and per-context sample counts
   cannot diverge silently.
4. For each context, reject nonfinite values and accumulate element count,
   observed minimum, observed maximum, absolute maximum, and the deterministic
   nearest-rank 99th and 99.9th percentiles of per-sample absolute maxima.
5. Select `B = max(observed_abs_max * safety_factor, minimum_positive_bound)`.
   Version 1 requires `safety_factor >= 1`, `B > 0`, and the explicit
   out-of-range policy `reject`. Record the number of observed samples outside
   `B`; approval requires zero.
6. Record the ordered sample IDs and their canonical SHA-256, a byte digest of
   the observed input tensors, a canonical digest of the executed model state,
   checkpoint, dataset, preprocessing, collector revision, environment,
   source-artifact hashes, and the coefficient-manifest hash.
7. Serialize canonical ASCII JSON with sorted keys and compact separators.
   `manifest_sha256` covers the complete manifest excluding that field itself.
8. Validate the final manifest against the exact persisted identity template.
   Any missing, duplicate, reordered, or changed identity fails closed.

The distribution fields are evidence, not a license to choose an acceptance
threshold after observing results. Calibration data selects normalization
bounds. A separately declared held-out acceptance set evaluates model and
approximation accuracy.

## Manifest contract

An approved manifest must identify:

- profile and coefficient-manifest SHA-256;
- binary WHIRL, model source, and parameter payload SHA-256 values;
- trained checkpoint identity and SHA-256;
- dataset name, version, split, files or canonical digest;
- preprocessing definition and SHA-256;
- deterministic sample selection, order, count, and seed;
- collector revision and software environment;
- bound rule, positive `B`, extrema, tail evidence, element/sample counts,
  nonfinite count, outlier count, and `reject` policy for every context;
- a distinct held-out acceptance-data role; and
- an explicit approval identity.

`status=approved` is necessary but not sufficient. Approval validation checks
all required fields, hashes, identities, measurements, bounds, and data-role
separation. Candidate and fixture manifests cannot authorize native records.

## Native binding

After approval, the FHE conversion pass must:

1. deep-copy the path and lowercase digest from
   `VHO_FHE_CONVERT_OPTIONS` on the first semantic callback and require the
   same pair on every later PU callback;
2. read exact bytes and verify the external SHA-256 before parsing or using
   any semantic field;
3. parse the fixed schema with bundled header-only RapidJSON, verify the
   embedded canonical-content hash, approval authority, source/checkpoint/data
   hashes, sample evidence, finite bounds, and held-out acceptance-data role;
4. derive the expected identity set from live Open64 DSL node, PU-identity,
   callsite, and value tables and reject anything other than the exact 19-row
   set before mutating planning tables;
5. join each manifest context to the live source ReLU by the complete identity
   and create finite observed-min, observed-max, and positive-bound TCONs;
6. call `DSL_FHE_Approx_Profile_Bind_Context_Range()` exactly once per context
   and add one composite disposition per reusable physical ReLU definition;
7. require the checkpoint finalizer to prove all 19 contexts were consumed
   exactly once, then clear retained state through completion on success or
   failure; and
8. require 19 reopened context rows and inspect them with
   `ir_b2a -st -src`.

PR #127 supplies the reviewed runtime-only path and expected SHA-256 fields.
The FHE consumer adds no JsonCpp or frontend-builder dependency; it uses the
repository's bundled RapidJSON headers. No binary row, opcode, type encoding,
or common/com mutation is introduced.

## Tests and evidence

Focused tests prove:

- exactly 19 complete identities and one root context;
- no duplicate or missing identities or routes;
- deterministic byte-identical output;
- identity mismatch rejection;
- zero and negative `B` rejection;
- manifest hash mismatch rejection;
- native exact-byte SHA rejection before JSON use;
- native malformed JSON, unapproved status, unknown/duplicate/missing identity,
  and invalid-bound rejection with no checkpoint artifact;
- real six-PU fail-closed derivation of exactly 19 expected context identities;
- callback selection mismatch rejection and retained-state cleanup;
- candidate manifests cannot pass approval validation; and
- a fully populated, explicitly approved test-only manifest can pass the
  approval validator without being bound into a model artifact.

The retained fixture must be labeled `fixture-only-not-calibration`. It may not
replace trained checkpoint, CIFAR-10 calibration, held-out acceptance, or
runtime CKKS evidence.

## External evidence blocker

To produce a model-authoritative manifest, reviewers must supply or approve:

1. a licensed trained ResNet-20/CIFAR-10 checkpoint and immutable SHA-256;
2. the CIFAR-10 source/version and redistribution or download policy;
3. a frozen calibration split or ordered sample-ID list and canonical digest;
4. a preprocessing definition and implementation hash;
5. a safety factor and explicit outlier/rejection policy; and
6. a disjoint held-out acceptance set with predeclared metrics and thresholds.

Until those evidence inputs are accepted, `CFHECNN-RELU-003` remains the
correct no-manifest conversion boundary and no ReLU-bearing
`secure_resnet20.fhe.B` may be published. Authenticated but malformed,
unapproved, or identity-incomplete manifests fail earlier under the stable
`CFHECNN-RELU-004` / `CFHECNN-RELU-005` diagnostics.
