# FHE SYNC-3 Identity-Bound ReLU Range Calibration

Status: calibration machinery, authenticated all-PU binding, trained-model
range evidence, and Commit 19 structural validation are complete. An absent
manifest still requires fail-closed `CFHECNN-RELU-003` behavior.

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

The accepted model evidence uses the pinned ACE ONNX artifact as the compiler
fixture, an immutable Open64 PyTorch translation, and the canonical CIFAR-10
distribution. Original training provenance is unknown and is not claimed. The
deterministic synthetic fixture remains a machinery-only test and contributes
no model range or accuracy evidence.

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
   embedded canonical-content hash, approval authority, hash syntax, sample
   evidence, finite bounds, and held-out acceptance-data role;
4. before conversion, hash the active SafeTensors parameter side file and
   require an exact match with `source_artifact.parameter_payload_sha256`;
   this is the native model-state binding available at the binary-WHIRL
   boundary;
5. derive the expected identity set from live Open64 DSL node, PU-identity,
   callsite, and value tables and reject anything other than the exact 19-row
   set before mutating planning tables;
6. join each manifest context to the live source ReLU by the complete identity
   and create finite observed-min, observed-max, and positive-bound TCONs;
7. call `DSL_FHE_Approx_Profile_Bind_Context_Range()` exactly once per context,
   add one matching `POST_REFRESH.v1` context-state row, and add one composite
   disposition per reusable physical ReLU definition;
8. require the checkpoint finalizer to prove all 19 ranges and states were
   consumed exactly once, then clear retained state through completion on
   success or failure; and
9. require 19 reopened range rows and 19 context-state rows with
   `ir_b2a -st -src`.

The backend does not reopen the training checkpoint or source file, and it
does not derive an input `.B` whole-file digest from the per-PU callback. The
independent certification lane therefore verifies the exact `.B`, model
source, trained checkpoint, CIFAR-10 inputs, and their recorded hashes before
the candidate is accepted. Native prepublication verification covers the
active SafeTensors whole-file digest plus live WHIRL identities, routes, and
FHE configuration.

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
- active parameter payload mismatch rejection before conversion;
- native malformed JSON, unapproved status, unknown/duplicate/missing identity,
  and invalid-bound rejection with no checkpoint artifact;
- route-to-level lookup structurally accepts no mapped-image numeric identity,
  so an equivalent legal table allocation cannot alter the stable
  `instance_path` schedule;
- real six-PU fail-closed derivation of exactly 19 expected context identities;
- callback selection mismatch rejection and retained-state cleanup;
- candidate manifests cannot pass approval validation; and
- a fully populated, explicitly approved test-only manifest can pass the
  approval validator without being bound into a model artifact; and
- the model-authoritative manifest binds all 19 ranges and context states in a
  six-PU checkpoint that independently reopens.

The retained fixture must be labeled `fixture-only-not-calibration`. It may not
replace trained checkpoint, CIFAR-10 calibration, held-out acceptance, or
runtime CKKS evidence.

## Accepted model evidence

The Commit 19 model-authoritative manifest freezes:

1. the pinned ACE-derived checkpoint SHA-256, while recording unknown original
   training provenance;
2. the canonical CIFAR-10 source/version and local-only hashed-input policy;
3. a deterministic class-stratified 5,000-image training calibration subset;
4. the exact ACE preprocessing definition and hash;
5. `B = 1.1 * observed_abs_max` and reject-on-outlier policy; and
6. a disjoint 1,000-image held-out test subset with predeclared thresholds.

The accepted model manifest permits only the SYNC-3 planning checkpoint.
Without it, `CFHECNN-RELU-003` remains the correct conversion boundary.
Authenticated but malformed, unapproved, or identity-incomplete manifests
fail under stable `CFHECNN-RELU-004` / `CFHECNN-RELU-005` diagnostics. SYNC-4
materialization and later ciphertext execution remain separate gates.
