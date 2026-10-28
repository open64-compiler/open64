# LN-REF-1 Independent Arithmetic Oracle

Version v0.1 | 2026-10-28 | LN1-d implemented; host/specification evidence only

## 1. Scope And Ownership

Validate the explicitly declared version-2
[LN-REF-1 arithmetic contract](AI-COMPILER-LAYERNORM-REFERENCE-ARITHMETIC-v0.1.md)
without depending on reduction coding, a GPU, generated CUDA, or the production
arithmetic helpers. Version 1 remains unresolved and unchanged. Agreement on
samples does not promote v1, prove original Python-source equivalence, permit
reassociation, or authorize a numerical-policy change.

| Component | Responsibility / boundary |
| --- | --- |
| `normalization_oracle.py` | Test-only exact integer/rational reference for binary32 operations and every ordered row state; no host FP, compiler calls or reduction imports. |
| `normalization_oracle_host.cxx` | Standalone native binary32 witnesses and a separately implemented MPFR interval engine. Not a generated kernel or compiler runtime. |
| `run_normalization_oracle_review.py` | Deterministic inputs, exact expected census, strict trace audit, rejections and conventional reports. |
| `test_normalization_oracle.py` | Boundary/tie/class/contract tests and malformed numerical-trace negatives. |
| `replay_normalization_oracle_context.py` | Authenticate retained LN1-c evidence and unchanged native sources; independently reopen mapped images, regenerate native fixtures and run real PREOPT. No numerical execution of these images. |
| Production WHIRL pipeline | Sole executable truth. This milestone adds no source matcher, lowering, option, runtime, plan selector or binary-layout change. |

The independent oracle deliberately does not reuse the other chat's reduction
arithmetic implementation. It implements the mathematical specification in
test infrastructure, not a competing production compiler service.

## 2. Exact Scalar Algorithm

Inputs and outputs are unsigned 32-bit words, never decimal floating literals.
Finite values decode to signed integer significands times powers of two.
Addition, subtraction, multiplication and division form an exact rational and
round once to binary32; every operation has separate IEEE zero/infinity/NaN
handling. Exact cancellation under RNE is positive zero, except negative zero
plus negative zero. Multiplication/division retain the XOR sign on zero.

```text
round32(exact rational x):
    handle exact zero with the operation's required zero sign
    e = floor(log2(abs(x))) using integer comparisons
    q = max(e - 23, -149)                 // binary32 quantum
    integer, remainder = divmod(abs(x) / 2^q)
    increment if above half, or at half with odd integer
    pack rounded significand, handling carry, subnormal and overflow

sqrt32(exact rational x):
    handle negative/zero/infinite/NaN classes separately
    binary-search ordered positive binary32 words for adjacent lo, hi
    compare x exactly against ((value(lo) + value(hi)) / 2)^2
    choose nearer word; exact tie chooses even significand
```

Subnormal quantization is directly at `2^-149`, not an intermediate host-double
or normal-only rounding. The overflow midpoint is `2^128 - 2^103`; its tie
rounds to infinity. Sqrt tests also include squared rational midpoints which
are not themselves representable as binary32 inputs. NaN payload, sign and
signaling state are unobserved; every non-NaN result bit is compared exactly,
including both signs of zero and infinity.

## 3. Independent Engines And Environment

The native witness uses volatile binary32 operand/result stores and explicit
`-fno-fast-math -ffp-contract=off -frounding-math`. It establishes RNE, masked
exceptions, FTZ=0 and DAZ=0 in its own process. Current supported host route is
SSE binary32, tested through macOS x86_64 execution on the development host;
unverified non-SSE environments refuse rather than assume conformance.
No floating flags, traps or errno become observable reference outputs.

The MPFR engine constructs inputs from integer significands, not host float
conversions. At precision 64, then 128/256/512/1024 if necessary, it evaluates
the same scalar operation with rounding toward negative and positive infinity.
Both endpoints are converted to binary32 RNE. Accept only if both conversions
agree bitwise, or both are NaN class. Resolve exact-zero sign separately with
RNE. An inconclusive 1024-bit interval fails the review, never guesses.

This avoids assuming that an arbitrary high precision is sufficient or that
24-bit MPFR precision alone models binary32 underflow. MPFR provides directed
rounding and float conversion facilities; the interval acceptance rule is our
test design. See the [MPFR rounding documentation](https://www.mpfr.org/mpfr-current/mpfr.html#Rounding)
and [conversion functions](https://www.mpfr.org/mpfr-current/mpfr.html#Conversion-Functions).
The retained run uses MPFR 4.2.2 and GMP 6.3.0. A halfway product perturbed by
minimum subnormal forces refinement to 256 bits, exercising the retry path.

## 4. Row States And Storage Witnesses

The reference records `count`, every `sum_step`, `mu`, every `d`, `square` and
`var_step`, then `v`, `t`, `h`, `q`, every `u`, `z` and `y`. There are `7*N+6`
states per row/engine. Ascending positive-zero folds include the first add;
both means divide the completed sum by the exact count. Sqrt and reciprocal
division round separately. No implicit FMA, FP64 accumulation, tree reduction,
epsilon relocation, raw-moment variance or zero-gamma shortcut is admitted.

| Witness | Behavior | What agreement establishes |
| --- | --- | --- |
| C0 | Recompute centered values in the affine epilogue. | Recalculation agrees on immutable operands under the bound arithmetic. |
| C1 | Retain centered values. | Retention preserves all required bits. |
| C2 | Overwrite a private row copy only after the mean and each last original read. | This particular host storage schedule agrees; it does not prove GPU alias, copy visibility, event or lifetime legality. |
| MPFR | Execute the entire ordered row through directed intervals. | A second scalar implementation agrees at every state, not only the final result. |

The 28 rows include negative zero, subnormal ties/epsilon, cancellation and
reordering, overflowed mean, NaN/signaling NaN/infinities, invalid zero-gamma
products, affine overflow, and seeded finite/arbitrary-bit rows at widths
1/2/3/17/65/127. Widths 17/65/127 exercise non-power-of-two host traversal, not
proof of GPU tile-tail masks. Six count-only cases reach `2^24` without huge
allocations. Ten fresh-process rejections include v1, invalid count/epsilon,
unknown engine, noncanonical word and truncated row.

| Counterexample | Required result | Forbidden substitution |
| --- | --- | --- |
| First mean add with one `-0` | `+0` mean | Copying the input preserves `-0`. |
| `[2^24,1,-2^24]` mean | `00000000` | Reordered fold yields `3eaaaaab`. |
| Square `3f800001`, then add `33800000` | `3f800002` | Fused result `3f800003`. |
| `t=6`: sqrt then reciprocal divide | `h=401cc471`, `q=3ed105eb` | One-round rsqrt `3ed105ec`. |
| `5/3` | `3fd55555` | Multiplying by rounded reciprocal yields `3fd55556`. |
| Two max-finite values, mean | `+inf` | Scaling each contribution before the fold yields finite max. |

These are exact semantic counterexamples, not measured LLM accuracy losses.
No FP64 accuracy report is produced or conflated with bit-contract agreement.

## 5. Trace And Artifact Contract

The raw standalone `arithmetic.t` uses additive test-only positional tags:

```text
AI_NORM_ENV 1 HOST rne ftz daz exceptions nan_policy mpfr_version source_line
AI_NORM_PRIM 1 HEADER case operation left right third host mpfr precision source_line
AI_NORM_NUM 1 HEADER case engine stage index bits source_line
```

Values use `VALUE` in place of `HEADER`; the environment row has fixed fields.
Operation IDs 1..5 are add/sub/mul/div/sqrt; 6 is FMA for counterexamples only,
never an admitted reference instruction. Last-column coordinates name the
retained host test producer, **not** Python input or WHIRL source. Actual native
WOPT `.t` rows retain their existing schemas, option controls and source lines.
The numerical trace is never relabeled as a compiler transformation trace.

The auditor reconstructs the full expected primitive/state census independently
of the trace. It rejects missing, duplicate, unknown, wrong-version, extra-field,
changed-input, changed-environment, changed-source-line or wrong-bit records.
Only NaN payload/class-equivalent changes may pass. Output directories must be
new; a failed run cannot overwrite or certify accepted evidence.

| Report | What it summarizes |
| --- | --- |
| `numerical.log` | Primitive/row/state counts, exact policy, counterexamples, rejections and independent-engine provenance. |
| `optimization.log` | Complete successful numerical-phase content; native semantic/target reports remain separately retained, with their historical maturity labels. |
| `optimization-summary.log` | Numerical completion, unchanged v1, no selection/cost/transformation, no GPU proof and next integration gate. |
| `native-replay/replay-report.json` | Authenticated parent, 16 full-dump comparisons, four byte-identical producers, 156 native target queries, actual PREOPT and expected executable-lowering refusal. |

All `.log` reports start with `# What it summarizes:`. Retain exact requests,
manifest/expected states, raw success and rejection traces, source snapshots,
tools/dependency hashes, commands, both streams and checksums. Reopen from the
immutable final receipt before publishing its index/current link. Raw `.t`
and source-interleaved `.T` occupy separate directories on case-insensitive
filesystems.

## 6. Reproduction And Completion Gates

```sh
clang++ -arch x86_64 -std=c++11 -O2 -fno-fast-math -ffp-contract=off \
  -frounding-math -I/usr/local/include \
  osprey/be/opt/tests/normalization_oracle_host.cxx \
  -L/usr/local/lib -lmpfr -lgmp -o /tmp/normalization_oracle_host
python3 osprey/be/opt/tests/run_normalization_oracle_review.py \
  --host /tmp/normalization_oracle_host --output /tmp/ln1d-fresh-review
python3 -m unittest discover -s osprey/be/opt/tests -p test_normalization_oracle.py
```

Paths above describe this machine's optional test dependencies; they are not
backend link requirements. Keep MPFR/GMP and the witness executable outside the
compiler product. The retained receipt includes exact tool/dependency hashes.

Completed: 12,666 scalar cases checked against exact rational results by both
native and MPFR paths, 13,474 state comparisons across 28 rows/four engines and
six count descriptors, ten rejections, 14 new unit tests and 58 existing trace
tests. The full numerical review also passes an independently built UBSan
witness. Native/mapped verification authenticates all 442 parent files before
replay; source hashes authorize cached tools, not a fabricated compiler rebuild.

Next LN1-e: explicitly associate native-bound source and target/environment
contracts with the numerical fixtures and preserve refusal/trace/mapped
provenance. The arbitrary-width numerical cases here are specification
witnesses, not executions of the retained fixed-width native source image.
LN2 storage/event legality, LN3 complete profitability, LN4 selected executable
WHIRL lowering and LN6 GPU accuracy/resource/performance certification remain
separate work. No GPU is needed to continue compiler-side development.
