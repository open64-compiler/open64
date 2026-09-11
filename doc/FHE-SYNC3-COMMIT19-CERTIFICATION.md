# FHE SYNC-3 Commit 19 Certification Record

Status: local candidate pass; independent main-side review and merged-tip
recertification remain before formal SYNC-3 acceptance.

## Scope

This record certifies the focused C3 / SYNC-3 conversion-planning checkpoint.
It does not claim completion of architecture Phase 3, M4, SYNC-4 ReLU
materialization, standard-WHIRL runtime lowering, generated C, or OpenFHE
ciphertext execution.

The candidate is based on `origin/develop` at
`864eb7ccdef3a6bd042851b0da5a105ca4202d49` and inherits the merged
context-state implementation at `1f02778018bb3759793e2bf7f5704187306bc344`.

## Decision

Local verdict: **Pass**.

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

Host directory:
`/private/tmp/open64-fhe-sync3-conversion/artifacts/fhe/sync3_commit19_lane`

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

## Validation

- The linked native semantic test passes.
- The full native Python suite passes 170 tests.
- The optional capture lane passes 56 tests.
- Native ResNet and Llama prefill, decode, and multiple-PU lanes pass and
  retain artifacts.
- The x86-64, MIPS, MIPS-SL, KEY-generic, Loongson, and baseline syntax/layout
  matrix passes.
- A dependency-light SafeTensors oracle authenticates the pinned checkpoint,
  source, binary WHIRL, and source payload hashes, then recomputes all 42
  folded tensors without calling production folding code.
- Independent trace validation proves the exact disposition, stage, range,
  state, source, and report joins.
- Missing manifest (`CFHECNN-RELU-003`), wrong exact-byte hash and malformed
  bounds (`CFHECNN-RELU-004`), unknown identity or a correctly hashed route
  inconsistent with its persisted callsite (`CFHECNN-RELU-005`), and stale
  destination (`CFHE-CHECKPOINT-006`) fail without final or temporary output.
- A topology-identical input paired with changed parameter-payload bytes fails
  natively under `CFHECNN-RELU-007` before conversion or publication.

## Formal Acceptance

The remaining steps are procedural rather than implementation blockers:

1. freeze the FHE-owned candidate commit and review its exact diff;
2. independently inspect the retained `.fhe.T`, payload, report, diagnostics,
   and hashes;
3. open and merge the narrow FHE-owned PR; and
4. rerun this certification at the merged PR tip before marking SYNC-3 closed.

SYNC-4 starts only after that acceptance. It must consume these planning
records to materialize the mandatory pre-ReLU refresh, context normalization,
three Chebyshev stages, and ReLU reconstruction.
