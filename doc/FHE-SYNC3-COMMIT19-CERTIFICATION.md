# FHE SYNC-3 Commit 19 Certification Record

Status: PR #131 is merged. The historical 2026-09-14 certification remains
recorded, and a new exact-snapshot certification passed on 2026-09-29 against
the current integrated source tree. The complete current artifact family is
retained and independently reopenable, so current SYNC-3 verification is
**verified**.

The sole highest FHE semantic authority is
`doc/DSC_FHE_Compiler_Architecture_and_Integration_Plan_v0.10.docx`, whose
repository copy has SHA-256
`0018769C26B5A0BCD1BDFCBD85AA97B8BAFEA381D7640FBB9E2E81B0022013D9`.

## Scope

This record documents the historical certification of the focused C3 / SYNC-3
conversion-planning checkpoint.
It does not claim completion of architecture Phase 3, M4, SYNC-4 ReLU
materialization, standard-WHIRL runtime lowering, generated C, or OpenFHE
ciphertext execution.

The accepted implementation is Commit 19
`93d80a6b79858ee5b7183f4426d3c94573b53e95`, merged by PR #131 at
`d424c00be487f884a9ae7fb5cfc688f39e96d279`. The merge and Commit 19 trees are
both `14296246aca8ba61ec1d56fc6b1050d7db0a99e9`, so the merged-tip rerun tested
the exact independently reviewed feature tree.

## Decision

Current exact-snapshot verdict on 2026-09-29: **Pass**. The current frontend
recapture, backend conversion checkpoint, separate-process `ir_b2a -st -src`
reopen, independent verifier, and fail-closed negative suite all ran during
this revision. The approved calibration values and accuracy results were not
recomputed; their immutable evidence was rebound only to the semantically
identical current `.B` snapshot whose source and parameter payload hashes match
the approved inputs.

The six-PU SecureResNet20 checkpoint publishes the converted side payload and
conversion report as auxiliary artifacts, then publishes `.fhe.B` last as the
commit marker. Independent `ir_b2a -st -src` reopen and the independent Python
consumer agree with the compiler report.

| Evidence | Certified result |
| --- | --- |
| Program structure | 6 `FUNC_ENTRY` procedures, 9 class-context calls, 5 managed `cnn.basic_block.v1` definitions |
| BatchNorm folding | 13 physical definition retirements, 21 source-context folds, 42 converted tensors |
| Operator planning | 46 source dispositions and 46 accepted converted dispositions |
| ReLU definitions and contexts | 11 reusable `common.relu` definitions represent 19 exact source contexts |
| Composite profile | `ace.chebyshev.sign.7x15x13.depth11.v1`, ordered degrees `7,15,13`, depth 11 |
| Context ranges | 19 approved callee-tagged rows, exactly one per source context |
| CKKS context state | 19 `POST_REFRESH.v1` rows; levels 15, 17, or 18; scale 56; 2 components; precision 30; 32768 slots |
| Accuracy | 91.6% clear, 91.5% polynomial, 0.1 percentage-point drop, 99.9% prediction agreement, 0 out-of-range values |
| Backend boundary | `be.so` and `lw_inline` contain no `DSL_Builder_*` or JsonCpp symbols |

Before publication, the backend authenticates the active SafeTensors
whole-file digest against the approved range manifest and validates the live
WHIRL identity, route, and FHE-config joins. The independent lane separately
authenticates the exact input `.B`, model source, trained checkpoint, and
dataset evidence; those files are not reopened by the per-PU backend callback.

The 11 physical ReLU nodes remain inspectable source-semantic anchors. Every
one has a composite disposition and complete context evidence. No bootstrap,
polynomial arithmetic, SIHE/CKKS primitive, runtime C ABI, or OpenFHE call is
materialized in this checkpoint.

## Policy Tuple

The SHA-256 values below cover the exact ASCII artifact bytes committed to Git.
Each JSON file uses LF line endings and exactly one terminal LF. Verification
hashes the validated bytes directly, without JSON reserialization, text-mode
newline conversion, or platform-native line-ending substitution.

| Artifact | SHA-256 |
| --- | --- |
| `doc/fhe-policy/sync3-relu/coefficient-manifest.json` | `75132d449852303ec3e44e86c8a5b5ffc196c0643cf7fadff453d797c2266931` |
| `doc/fhe-policy/sync3-relu/range-manifest.json` | `f9dbcb26f22a9fb12a3bfba046504ae2b88bea81449d10e16a2cb5432c579078` |
| `doc/fhe-policy/sync3-relu/accuracy-manifest.json` | `1e6dbc64074c504749ce7854ae34524cb5c48a63218a0f0bae517043f3643760` |
| `doc/fhe-policy/sync3-relu/ckks-schedule-manifest.json` | `27fe104aa5a159baefd0255c82e0c9193c1ecdf28b73f97e2f8830bb1444ae62` |
| `doc/fhe-policy/sync3-relu/package-index.json` | `161fa24568561d7b918529684f4daf0165c9646d840885e1afd244f8cf7b1d5a` |

The model fixture is pinned to ANT ACE revision
`fb76131171b9f82aa6387f84dd73684fba5277e8`. Its ONNX SHA-256 is
`6627e8494884a7422fd8f2247469ae12cf8167bcabeda0033725387b8089f9d9`.
Original training provenance is unknown and is not claimed.

## Retained Evidence

Current host-visible exact-snapshot directory:
`/private/tmp/open64-fhe-sync3-recertification-20260929/current-snapshot-certification`

The directory contains the complete source and converted artifact families,
commands, diagnostics, and `SHA256SUMS`. The original accepted checkpoint is
retained beside it as
`/private/tmp/open64-fhe-sync3-recertification-20260929/ace-resnet20-open64.pt`
with SHA-256
`75fb9294272845b19eaeea3e1ee644d289536ea6711b27ec9527e658f5d20ff5`.

| Artifact | SHA-256 |
| --- | --- |
| `secure_resnet20.B` | `0067b3255068bcbc6587cf98b3d57d6d08fd4a90a23f326bdc6e16e081156b9e` |
| `secure_resnet20.T` | `c1c0169fb9e3472faa0204e1b7f88ba5d51802121351d26ac5748e513f0e7896` |
| `secure_resnet20.safetensors` | `3ddba0cee92d2f7e975d59a6a05d03ccd0578d798adae0e812a5ec8fcd617701` |
| `secure_resnet20.fhe.B` | `1772931c580b722092c2a184d6e1fa0aa0d864d275e830b622406513d95c31a1` |
| `secure_resnet20.fhe.T` | `beea24052f8b0d89c3b9733c58fd79103c666f765887d8151a2d15aa458e08a1` |
| `secure_resnet20.fhe.safetensors` | `045a3bf1ac9967bcc8e962ec198179549e6a8beba91ba96195bae1a0452d8ffe` |
| `secure_resnet20.fhe.conversion-report.txt` | `9a3f5bf2898a59a77008b5125a44a30ad26e593f46015bdc6d03c400e35e0997` |
| `secure_resnet20.fhe.vho.t` | `a7c2c1ecf4bc57e8526b0960ce94f1b8dd052795f1cce52eb1e36aed19806665` |
| `conversion.log` | `96f2db91e474b0a2baa235fb70d0155ce2704f826bdcbd37f2a8ff4bdf73a3ff` |
| `independent-verifier.log` | `8c6cdf9dde40129e39abb6747afe50fe56b256188f4f009b06af696e8d43d736` |

## Validation

- The linked native semantic test passed against the current integrated build.
- The current full native Python suite passed all 207 tests.
- The optional capture lane passed all 56 tests.
- The focused policy, evidence, plan-consistency, and ABI suites passed all 52
  tests.
- The full Commit 19 checkpoint suite passed conversion, separate-process
  `ir_b2a -st -src` reopen, independent verification, and every required
  fail-closed negative.
- Native ResNet and Llama prefill, decode, and multiple-PU lanes remain
  supporting historical evidence; they were not rerun for this exact-snapshot
  rebind.
- The x86-64, MIPS, MIPS-SL, KEY-generic, Loongson, and baseline syntax/layout
  matrix passed against the current integrated source.
- The rebuilt `be.so` and `lw_inline` contain no `DSL_Builder_*` or `Json::`
  symbols.
- A dependency-light SafeTensors oracle authenticated the
  pinned checkpoint, source, binary WHIRL, and source payload hashes, then
  recomputed all 42 folded tensors without calling production folding code.
- Independent trace validation proved the exact disposition,
  stage, range, state, source, and report joins.
- Missing manifest (`CFHECNN-RELU-003`), wrong exact-byte hash and malformed
  bounds (`CFHECNN-RELU-004`), unknown identity or a correctly hashed route
  inconsistent with its persisted callsite (`CFHECNN-RELU-005`), and stale
  destination (`CFHE-CHECKPOINT-006`) failed without final or
  temporary output.
- A topology-identical input paired with changed parameter-payload bytes was
  rejected natively under `CFHECNN-RELU-007` before conversion or
  publication.

## Historical Formal Acceptance

The 2026-09-14 record states that all procedural gates were complete:

1. the exact FHE-owned Commit 19 diff received independent main-side review;
2. the retained `.fhe.T`, payload, report, diagnostics, and hashes passed that
   review;
3. PR #131 merged without conflict-resolution changes; and
4. the complete certification passed again at merged tip `d424c00b`.

Merge mechanics: **Pass**. The merge parents are `864eb7cc` and `93d80a6b`,
and the merge tree exactly equals the accepted Commit 19 tree.

Integrated implementation state: Commit 19 remains merged through PR #131.
Current verification state: **verified** by the retained 2026-09-29
exact-snapshot certification. SYNC-4 may consume this focused planning input.
Focused SYNC-3 remains distinct from v0.10 Architecture Phase 3 and M4.
