# Centered LayerNorm Reference Arithmetic Binding

Version v0.1 | 2026-10-28 | Reference semantics defined; native support not implemented

## 1. Decision And Compatibility Boundary

Define **LN-REF-1**, an explicit FP32 centered LayerNorm reference. Both means
use positive-zero ascending left folds and one final division each. Reciprocal
square root uses a separately rounded square root followed by a separately
rounded division of one. Every scalar arithmetic result is binary32 RNE with
gradual underflow. This supplies a reproducible reference for comparing C0/C1/C2
in the [LayerNorm execution design](AI-COMPILER-LAYERNORM-OPTIMIZATION-DESIGN-v0.1.md).

LN-REF-1 is a reference-specification identifier, not a registered native
operator/version, attribute set, compiler option, emitted trace schema or
implementation claim. Native admission and persistence need a separate reviewed
contract. This document changes no source, builder, mapped image or lowering.

Preserve `common.mean_variance_norm.v1` exactly, including its existing eight
attributes, `rsqrt_semantics=source_unresolved`, unresolved mean order/scaling,
and false reassociation/state-reformulation/contraction permissions. Existing
Python capture continues to emit v1. Neither a target option, optimization
level, profile, successful sample nor this document binds that node implicitly.

A future explicit producer may request a separately registered LN-REF-1 native
contract only when its source/DSL semantics authorize this reference. A future
source-compatible refinement requires independent proof and reviewed admission;
there is no such automatic v1 refinement in the present implementation.
Diagnostic metadata or a runtime manifest cannot replace semantic WHIRL state.

## 2. Terms And Domain

| Term | Definition / requirement |
| --- | --- |
| Binary32 | IEEE 754 single precision; each scalar input/intermediate/output is a binary32 value. |
| RNE | Round to nearest, ties to even, applied after every specified arithmetic operation. |
| Gradual underflow | Preserve subnormal operands/results. No flush-to-zero (FTZ) or denormals-as-zero (DAZ) substitution. |
| No-trap observation | Arithmetic does not trap; exception flags, errno, NaN payload/sign/signaling status and intermediate evaluation time are not observable. |
| Class-only NaN comparison | Any NaN result matches a reference NaN. All non-NaN results, including infinity signs and signed zeros, require exact bits. |
| `add32/sub32/mul32/div32` | The indicated IEEE binary32 operation with RNE and gradual underflow, including defined exceptional outcomes. |
| `sqrt32` | Correctly rounded binary32 square root under the same environment, not an approximate instruction. |
| `count32(N)` | One exact conversion of the positive integer feature count into binary32. The initial domain below guarantees exact representation. |
| Row / feature | Indices `r in [0,R)` and `k in [0,N)`, using checked integer address/loop arithmetic. |
| Bound equivalence | Exact non-NaN bits or matching NaN class for the specified recurrence; distinct from accuracy relative to real arithmetic. |
| Source-compatible proof | Evidence that an existing source operation already permits this precise arithmetic; not a tolerance test. |
| Target binding | Evidence that instructions, compiler controls and execution environment implement the reference; separate from numerical definition. |

Initial domain: positive static integer R and `1 <= N <= 16777216` (`2^24`),
contiguous FP32 `X[R,N]`, affine vectors `gamma[N]`, `beta[N]`, and FP32 result
`Y[R,N]`. Denominator is population N. Epsilon is an exact positive finite
binary32 value supplied by its canonical bits, including positive subnormals.
The native v1 epsilon parser and descriptor rules remain unchanged.

This width bound is an LN-REF-1 admission boundary, not a new v1 restriction or
an SMEM capacity claim. A wider row receives `reference_width_unsupported`,
not a silently rounded count or padded denominator. Some wider integers are
exactly representable, but admitting them needs a reviewed extension. R and all
byte/offset products must fit the selected integer/address representation.

All FP32 bit patterns are admitted for X/gamma/beta, including infinities and
NaNs. Do not add a finite-input scan or silently replace exceptional results.
Storage, alias, input immutability and runtime ownership still need their own
gates. A value-domain specialization requires separate permission/proof and
complete cost accounting; it is not part of this reference.

## 3. Exact Mean Binding

Both means use `count32(N)` once per invocation or an equivalent exact constant
materialization. The integer N remains authoritative. Never derive the count
by repeatedly adding FP32 one, counting padded lanes or counting loaded chunks.

For each row, the input mean is:

```text
n32 = count32(N)
s = binary32 bits 00000000                 # positive zero
for k = 0 .. N-1:
    s = add32(s, X[r,k])
mu = div32(s, n32)                         # one final division
```

Every addition is part of the recurrence, including positive zero plus the
first element. The denominator is exact N; divisions are not distributed over
summands. Do not use partial sums, a parallel tree, a running-mean update,
multiply by a rounded reciprocal, FP64 accumulation or a fused operation as a
replacement. An alternative instruction implementation needs proof of the
same complete operation semantics, not just real-arithmetic equivalence.

The centered population variance uses a second independent positive-zero
accumulator and the same finalization:

```text
vsum = binary32 bits 00000000
for k = 0 .. N-1:
    d[k] = sub32(X[r,k], mu)
    square = mul32(d[k], d[k])             # round before adding
    vsum = add32(vsum, square)
v = div32(vsum, n32)                       # one final division
```

The square and accumulation are not an FMA. No reciprocal-N multiplication,
N-1 denominator, variance clamp, raw moment identity, Welford update/merge or
tree reduction is authorized. A fixed tree is still a different recurrence.

This reuses the
[ordered reduction numerical contract](AI-COMPILER-REDUCTION-NUMERICAL-BINDING-v0.1.md)
for accumulation, not an assumption that ordered sum already defines mean.
Reuse phase-owned recurrence/lowering services where compatible; do not force
allocation of a full squared tensor just to call a reduction helper. Native
representation and service interfaces require review before implementation.

## 4. Exact Reciprocal-Square-Root Binding

After variance is complete:

```text
t = add32(v, epsilon)
h = sqrt32(t)                              # first rounding point
q = div32(binary32 bits 3f800000, h)         # second rounding point; +1
```

The reference is `round32(1 / round32(sqrt(t)))`, with IEEE exceptional rules.
It is not `round32(1/sqrt(t))` evaluated with an unrounded square root, nor a
generic `rsqrtf`, approximate rsqrt instruction or an implicitly selected
Newton refinement. Even a correctly rounded single-operation reciprocal
square root can differ from this two-operation definition.

The reason to choose the composition is an explicit, portable operation-level
reference with known rounding boundaries, not a claim of peak performance or
bitwise PyTorch rsqrt parity. Future faster realizations may enter the same
comparison only with an exact-equivalence proof; a tolerance-based rsqrt
contract is a different explicitly authorized numerical policy.

Helper exceptional outcomes, including values not normally reachable as t
after positive epsilon, are fixed:

| Input t | h | q |
| --- | --- | --- |
| Positive finite, including subnormal | Correctly rounded positive sqrt. | Correctly rounded positive `1/h`. |
| +0 | +0 | +infinity |
| -0 | -0 | -infinity |
| +infinity | +infinity | +0 |
| Negative finite or -infinity | NaN | NaN |
| NaN | NaN | NaN |

No clamp, absolute value, epsilon relocation or special finite-result repair
is permitted. Positive epsilon does not prove mean, square or affine overflow
freedom. Intermediate infinities/NaNs propagate through the specified recipe.

## 5. Complete Scalar Reference And Retention Legality

The affine epilogue is:

```text
for k = 0 .. N-1:
    u = mul32(d[k], q)
    z = mul32(u, gamma[k])
    Y[r,k] = add32(z, beta[k])
```

The two multiplies and final add stay separately rounded. Zero gamma does not
authorize dropping an exceptional product. N=1 does not authorize returning
beta without evaluating the bound recipe. Identical read-only formals do not
imply physical noalias; output and scratch ownership are separate requirements.

C0 may recompute d from unchanged X and the same bound mu instead of retaining
it. C1 may retain its result bits. C2 may reuse original-row storage for d
after all original readers finish. This equivalence relies on the same
arithmetic/environment, immutable operands, unobservable flags/traps and
completed access-lifetime proofs. It grants no change to either accumulator
order, square rounding, epsilon position or epilogue rounding.

Rows, loads and independent elementwise operations may be scheduled in parallel
when all dependency, alias, visibility and ownership constraints hold. Within
each statistics fold, the ordered numerical dependence remains. Loop unrolling
may preserve that dependence; multi-accumulator reduction does not. Lowering
cannot discover numerical permissions in the source emitter.

Finite, infinite and signed-zero results are exact to this recurrence; NaN
observations are class-only. General error relative to a real-valued LayerNorm
is not bounded here. Sequential accumulation can lose small contributions or
overflow. FP64 source witnesses and LLM-level accuracy remain separate evidence
and must not be relabeled as bound equivalence or target certification.

## 6. Target Realization And WHIRL Obligations

The reference is target-neutral. NVIDIA realization candidates, not current
support declarations, are:

| Required operation | CUDA spelling candidate | PTX meaning required |
| --- | --- | --- |
| add32 | `__fadd_rn` | `add.rn.f32`, no FTZ. |
| sub32 | `__fsub_rn` | `sub.rn.f32`, no FTZ. |
| mul32 | `__fmul_rn` | `mul.rn.f32`, no FTZ or contraction. |
| div32 | `__fdiv_rn` | `div.rn.f32`, no FTZ or approximate replacement. |
| sqrt32 | `__fsqrt_rn` | `sqrt.rn.f32`, no FTZ or approximation. |

Explicit rounding and gradual-underflow behavior must survive optimization,
WHIRL mapping, emission and target compilation. Reject conflicting fast-math,
FTZ or approximate-div/sqrt settings unless a reviewed route demonstrably
preserves the full contract. Flag spellings alone are not conformance proof.
An exact reciprocal implementation for division by one is admissible only
when independently proved equivalent under all bound observations.

NVIDIA distinguishes rounded division/square root from approximate variants
in the [PTX ISA](https://docs.nvidia.com/cuda/parallel-thread-execution/index.html#floating-point-instructions-div).
The [CUDA single-precision intrinsics](https://docs.nvidia.com/cuda/cuda-math-api/cuda_math_api/group__CUDA__MATH__INTRINSIC__SINGLE.html)
provide explicit-rounding interfaces. Hardware availability of those operations
does not establish Open64 carrier/emitter completeness or GPU validation.

Reuse review anchors in current source:

- `osprey/common/com/intrn_entry.def` has existing F4ADD_ROUND/F4MUL_ROUND
  contracts; `osprey/be/cg/NVISA/expand.cxx` maps them to rounded add/mul.
- The same NVISA file's `Expand_Sqrt` uses TOP_sqrt_f32; the historical ISA
  inventory spells `sqrt.f32` and `div.f32` in
  `osprey/common/targ_info/isa/NVISA/isa.cxx`. These paths are not proof of the
  required modern rounding qualifiers and need explicit audit/extension.
- Ordinary F4 DIV/SQRT or textual C operators are insufficient unless their
  mode/carrier/lowering contract enforces the selected semantics. Review the
  host-built CUDA path separately from TARG_NVISA-gated intrinsic availability.

Common/com owns policy-free semantic records, structural checks and identity.
Owning optimization phases bind/verify/select/lower within driver scope. The
selected WHIRL must carry operation order, rounding, subnormal policy, exact
constants and the numerical contract. A helper may inline into the device PU;
no external arithmetic runtime call or host PU is inherently required.
`whirl2c -cuda` only prints already-selected meaning and diagnoses unsupported
carriers. No private template, metadata manifest or emitter heuristic becomes
another executable source of truth.

## 7. Profitability And Required Reports

Per row, the exact statistics/normalization reference includes two N-step
ordered-add chains, two mean-finalization divisions, one epsilon add, one sqrt
and one reciprocal division, plus elementwise subtraction/square/affine work.
Cost all three divisions and the sqrt with the actual lowerable target path.
Do not charge a single approximate-rsqrt latency or use Tensor Core rates.
Instruction helpers may expand to several target instructions; profile their
real service/dependence behavior rather than counting a helper as one cycle.

P7/P8 storage and delivery choices do not shorten the statistics recurrence.
After model arithmetic is updated, compare C0/C1/C2 with the same LN-REF-1
contract, target and invocation boundary; retain direct fallback if lowerable.
Unknown timing is recorded, not repaired with hard-coded delays or samples.

The following report fields are requirements, not newly implemented schemas:

| Report | What it must summarize |
| --- | --- |
| `semantic.log` | Original native version, LN-REF-1 admission/proof source or refusal, exact N/count bits, both order/identity/finalization rules, epsilon bits, sqrt-then-div sequence, environment/NaN policy and remaining binding gates. |
| `target-binding.log` | Exact architecture/toolchain/carriers, round/underflow controls, supported or refused routes and emitted-instruction checks; no inferred GPU proof. |
| `alternatives.log` | Same-contract C0/C1/C2 eligibility and C3/tree/approximation/precision rejections, separate from profitability failures. |
| `physical-plan.log` | Two means plus sqrt/div costs, dependency/readiness and all movement/resources; selected/fallback reason, uncertainty and actual WHIRL actions when implemented. |
| `numerical.log` | Exact reference/candidate bits or NaN class, intermediate checkpoints and independent oracle provenance; FP64 accuracy reported separately. |
| `optimization-summary.log` | Arithmetic specified versus native binding/target/lowering/GPU status, actual rewrite count and next gate. |

Keep existing option-controlled positional `.t`, source line last and first-line
`# What it summarizes:` log convention. Preserve AI_NORM v1 FORM/FACT schemas
and their unresolved facts. A future bound version needs separately reviewed
records; do not insert resolved facts into a v1 row. Trace-off still verifies
semantics and produces the same decisions.

## 8. Validation Cases And Remaining Implementation Gates

These are required future native/oracle tests, not newly executed kernels:

| Case | Required observation |
| --- | --- |
| Mean of one negative zero | Positive-zero initial add yields +0 mean; an identity-copy implementation is wrong. |
| Mean of minimum positive subnormal and +0 | Sum is minimum subnormal; division by two ties to +0 under RNE. No input flush. |
| Ordered `[2^24, 1, -2^24]` | Mean +0; changing to `[2^24, -2^24, 1]` gives rounded 1/3, proving order matters. |
| Two maximum finite inputs | Sum overflows to +infinity; mean remains +infinity, not a repaired finite real mean. |
| Count boundary | N=1 and N=2^24 convert exactly; zero and N=2^24+1 refuse this binding without mutating source. |
| Square-then-add | Include a fixture where fused square/add differs from two rounded operations; reject contraction. |
| Reciprocal-sqrt argument t=1 / t=4 | h/q bits `3f800000/3f800000` and `40000000/3f000000`. |
| Reciprocal-sqrt argument t=6 | h=`401cc471`, q=`3ed105eb`; one-rounding reciprocal sqrt instead yields `3ed105ec`. Include an exact midpoint-based oracle, not a host-double assumption. |
| Positive subnormal epsilon, constant zero row | Preserve nonzero t and finite q; no FTZ-induced infinite q or repaired output. |
| Exceptional inputs and zero affine factors | NaN class, infinity signs, signed zeros and IEEE invalid products propagate as specified; do not skip operations because gamma is zero. |
| C0/C1/C2 witnesses | Identical non-NaN bits/NaN classes at mu/v/t/h/q/Y with legal chunk/tail and storage variants; retained and recomputed d agree under the binding. |

Extend/reuse the independent integer binary32 addition oracle in
`osprey/be/opt/tests/reduction_binding_numerics.py` with separately verified
multiply/divide/sqrt oracle services, or adopt an appropriate proven oracle
library. Do not use the emitted kernel, the candidate helper, or an unchecked
FP64 computation as its own reference. Intermediate oracle evaluation uses
exact arithmetic with explicit binary32 rounding boundaries; that is distinct
from forbidden hidden FP64 accumulation in a candidate.

LN1 arithmetic design is now specified. The native-version/attribute/builder/
canonical-WHIRL contract, target arithmetic carriers/emission, oracle regression
suite, PREOPT preservation and before/after mapped execution evidence remain
implementation work. Preserve v1 fixtures and exact attributes in every lane.
LN2-LN4 remain blocked on those native prerequisites, not on GPU availability.
LN6 separately requires native numerical/resource/performance certification.

This increment is documentation-only. No operator/version, option, ABI, source
matcher, target profile, generated CUDA or native report format is changed.
Defined reference arithmetic is not implemented native binding, target
certification, a universal accuracy proof or a performance result.
