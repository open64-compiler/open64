# FHE SYNC-3 Commit 19 Historical Certification Record

Status: PR #131 is merged, and a complete certification was recorded as Pass at
the exact merged `develop` tip on 2026-09-14. That is historical evidence.
Current independent re-certification is **unverified** because the retained
artifact bytes are not presently accessible.

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

Historical merged-tip verdict on 2026-09-14: **Pass**. The tests and numerical
results below were not rerun for this documentation revision.

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
| `doc/fhe-policy/sync3-relu/range-manifest.json` | `55dcb4ec993a9f09901eaf61aa49dde8628e3eb032a24429a40715fc3fc4113c` |
| `doc/fhe-policy/sync3-relu/accuracy-manifest.json` | `1fd33f514363cf5b1f4f092e5553cb4e77d0f03eff3bbed4852ca0a6ae32ea25` |
| `doc/fhe-policy/sync3-relu/ckks-schedule-manifest.json` | `27fe104aa5a159baefd0255c82e0c9193c1ecdf28b73f97e2f8830bb1444ae62` |
| `doc/fhe-policy/sync3-relu/package-index.json` | `c1006e2c8b6b7c676f84fdd515d8cd81769e7504031ea5d0998e1b81d61578d4` |

The model fixture is pinned to ANT ACE revision
`fb76131171b9f82aa6387f84dd73684fba5277e8`. Its ONNX SHA-256 is
`6627e8494884a7422fd8f2247469ae12cf8167bcabeda0033725387b8089f9d9`.
Original training provenance is unknown and is not claimed.

## Retained Evidence

Historical host-local directory (not a current evidence locator):
`/private/tmp/open64-fhe-sync3-final-closure-artifacts/sync3_commit19_merged_tip`

This directory is unavailable to the current reviewer. The hashes below are
useful identities, but they cannot substitute for the artifact bytes. Current
re-certification requires either an accessible immutable complete bundle or a
new exact-snapshot run that retains the full evidence family.

| Artifact | SHA-256 |
| --- | --- |
| `secure_resnet20.B` | `24b26be80882b613e9d3c07b304775bdbdcb3a371d95204e1f7e955eab12257f` |
| `secure_resnet20.T` | `52dd8acba26aec0fd853e231d923cf37886ca837f03d4633f9a39818466ab561` |
| `secure_resnet20.safetensors` | `3ddba0cee92d2f7e975d59a6a05d03ccd0578d798adae0e812a5ec8fcd617701` |
| `secure_resnet20.fhe.B` | `ffdb3e19d1b73e24c82c804d5277cf53ec891070445f93f2e005a7961153c4a0` |
| `secure_resnet20.fhe.T` | `e752c8a141ed7ebdfada17439eafa5a5469c605ac7e23a54f8ae8806f53dfa56` |
| `secure_resnet20.fhe.safetensors` | `045a3bf1ac9967bcc8e962ec198179549e6a8beba91ba96195bae1a0452d8ffe` |
| `secure_resnet20.fhe.conversion-report.txt` | `737e985457e2eb5dcd5b209dad10e60fd21cf6a559d8af80f381a00b478861db` |
| `secure_resnet20.fhe.vho.t` | `5d9691ee0c9e8e4c8f4140ee8ee3168912091f6422518f9e47d882869d23cead` |
| `conversion.log` | `8f2cb8d4ab762f6e75e234e6228354a7166fedc73876563ce5f836a1db67e1b8` |
| `independent-verifier.log` | `5f9376be1a6fe6980c763fd2d3765c88591e219426f70734152311b7768f6191` |

## Historical Validation

- The linked native semantic test was recorded as passing.
- The full native Python suite was recorded as passing 170 tests.
- The optional capture lane was recorded as passing 56 tests.
- Native ResNet and Llama prefill, decode, and multiple-PU lanes were recorded
  as passing and
  retain artifacts.
- The x86-64, MIPS, MIPS-SL, KEY-generic, Loongson, and baseline syntax/layout
  matrix was recorded as passing.
- A dependency-light SafeTensors oracle was recorded as authenticating the
  pinned checkpoint, source, binary WHIRL, and source payload hashes, then
  recomputing all 42 folded tensors without calling production folding code.
- Independent trace validation was recorded as proving the exact disposition,
  stage, range, state, source, and report joins.
- Missing manifest (`CFHECNN-RELU-003`), wrong exact-byte hash and malformed
  bounds (`CFHECNN-RELU-004`), unknown identity or a correctly hashed route
  inconsistent with its persisted callsite (`CFHECNN-RELU-005`), and stale
  destination (`CFHE-CHECKPOINT-006`) were recorded as failing without final or
  temporary output.
- A topology-identical input paired with changed parameter-payload bytes was
  recorded as failing natively under `CFHECNN-RELU-007` before conversion or
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
Current verification state: **unverified**. SYNC-4 may use these records for
contract and design preparation, but must not consume them as verified
implementation input until the complete bytes are accessible and independently
checked, or an exact-snapshot certification is rerun and retained. Focused
SYNC-3 remains distinct from v0.10 Architecture Phase 3 and M4.
