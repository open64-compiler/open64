# WHIRL DSL Simplification M7 Certification

## Scope

M7 certifies the simplification behavior that is enabled through M6. It does
not broaden tensor algebra speculatively. Broadcasting, symbolic-shape,
quantized, placement-changing, reshape, reduce, matmul, convolution, and dense
payload rules remain disabled until their operator-specific semantic and
profitability contracts are reviewed.

The certification command is:

```sh
OPEN64_BUILD_ROOT=/build/osprey/targdir \
OPEN64_DSL_M7_ARTIFACT_DIR=/open64/artifacts/m7-certification \
  osprey/common/com/tests/dsl_simplification_m7_certification_test.sh
```

Run it in the Linux build container with the source, build, and artifact
directories mounted from the host.

## Coverage

| Capability | Positive evidence | Rejection or control evidence | Stage |
| --- | --- | --- | --- |
| Traditional `wn_simp` | scalar constant, bitwise identity, `SELECT` | `-OPT:wn_simp` master control | WN |
| Deterministic canonical order | `common.add`, `common.mul` | disabled, impure, mismatched descriptor | construction/VHO |
| Compact tensor folding | integer ADD/MUL/DIV/REM | nonconstant, effectful, unresolved shape, dense input | WN/VHO/WOPT |
| Parent revisiting | folded child exposes parent fold | no recursive evaluator entry | construction/VHO |
| Tensor descriptor safety | exact canonical `TY_IDX` identity | dtype/shape/layout/quantization/placement differences reject through descriptor identity | all |
| Numeric policy | signed positive and negative DIVREM | floating type, zero divisor, signed-minimum divided by `-1` | evaluator |
| Materialization bounds | ZERO/ONE/SPLAT stay compact | result count, total result elements, combined work, compact policy | evaluator |
| Projectable DIVREM | common logical identity and two result TCONs | option off, target capability off, profitability off | WOPT |
| One live projection | standalone DIV or REM reconstruction | internal projection cannot reach binary publication | WOPT/emitter |
| Two live projections | shared logical identity and use accounting | combined target emission requires a reviewed target consumer | WOPT |
| Folded TCON binding | carrier, descriptor, payload, size, alignment | corrupt or missing `tensor_tcon_idx` | gatekeeper |
| Tensor TCON storage | ZERO/ONE/SPLAT/INLINE_DENSE/SIDE_FILE_DENSE | malformed envelope, range, path, alignment, checksum collision | common/com |
| Binary compatibility | mapped-image reopen and derived-cache rebuild | physical string carrier hidden; `.WHIRL.dsl` v1 unchanged | reader/writer |
| Human inspection | retained `.B`, `.T`, WOPT traces | physical `OPR_DSL` and `MTYPE_STRING` stay hidden | `ir_b2a -st -src` |

## Deferred Expansion

The following cases are certification rejections, not missing silent
behavior:

- a `<pending>` or otherwise unresolved shape is not folded;
- effectful or runtime-state-dependent operations are not folded;
- dense inline and side-file values are valid tensor constants but are not
  evaluated by the compact arithmetic evaluator;
- quantized tensors require a reviewed zero-point and arithmetic policy;
- broadcasting requires explicit result-shape and representation legality;
- reshape, transpose, reduce, matmul, and convolution require
  operator-specific handlers; and
- a target must independently approve DIVREM lowering capability and
  profitability before WOPT can retain the combined form; and
- tensor division may later be transformed from `x / y` to
  `x * reciprocal(y)` and the reciprocal scheduled early on a GPU target
  that can overlap its latency with independent matrix-unit work. This is a
  target-aware optimization, not an unconditional algebraic identity. It
  requires reviewed strict-FP, NaN, infinity, signed-zero, exception,
  approximation-accuracy, descriptor, effect, dependency, and profitability
  checks.

These cases should land as independently reviewable post-M7 operator-family
changes, each with positive and rejection coverage.

## Retained Evidence

The default host artifact directory is `artifacts/m7-certification`. It
contains:

- construction-time folded and disabled `.B`/`.T` evidence;
- compact and side-file tensor TCON mapped-image evidence;
- VHO folded `.B` and `ir_b2a -st -src` output;
- WOPT enabled/disabled, factorization, no-factor, and DIVREM traces; and
- `certification.txt`, which records the completed gates.

Artifacts are cleaned at the beginning of the next run and retained after the
current run for human review.
