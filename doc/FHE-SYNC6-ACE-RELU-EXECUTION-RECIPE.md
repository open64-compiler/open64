# SYNC-6 ANT ACE ReLU Execution Recipe

Status: the algebraic recipe and clear oracle are implemented. Real-WHIRL
bootstrap, constant materialization, CKKS scale repair, and all nineteen
context expansions remain the next S6-0c work. This document corrects an
execution-scheme mismatch discovered before executable ReLU publication.

## Purpose

Open64 uses the empirically approved coefficient vectors from
`ace.chebyshev.sign.7x15x13.depth11.v1`. Executable captures use profile
version 2, `ace.chebyshev.sign.7x15x13.depth11.v2`, to distinguish the exact
ACE addition-chain from earlier planning rows. Those earlier rows labeled the
evaluation scheme `CLENSHAW`, and independent clear tests used Clenshaw as a
numerically stable oracle. The pinned ANT ACE compiler does not generate a
Clenshaw evaluator for ResNet-20. Executable Open64 lowering must follow the
compiler-generated ACE operation graph because the selected downstream
runtime is ANT ACE.

The authoritative pinned sources at ACE revision
`fb76131171b9f82aa6387f84dd73684fba5277e8` are:

| Source | SHA-256 | Role |
| --- | --- | --- |
| `fhe-cmplr/util/src/app_composite_poly.cxx` | `bd752b96eaaef5e4f14851c057d38cee9e45264e230dbb637dd59ba3cabb0f93` | coefficients and polynomial decomposition |
| `fhe-cmplr/sihe/src/tensor2sihe_impl.cxx` | `159863955dd439cdf382b1826a5b0eefafe10d2b29e18a80a4a90a70712ffebe` | Chebyshev basis precomputation and `App_relu` construction |
| `fhe-cmplr/rtlib/ant/dataset/resnet20_cifar10_pre.onnx.inc` | `55e0deded19fdef9383f1e368dfeb3d3920535635a07ac55cadd1086bc9b1c94` | generated CKKS program evidence |

## Correct Operation Order

For every context, ACE performs:

```text
x
  -> required capacity preparation and Bootstrap(target=15|17|18)
  -> x_refreshed
  -> Encode(1/B), MulPlain, Rescale
  -> x_normalized
  -> App_relu(x_refreshed, x_normalized)
```

`App_relu` composes degree-7, degree-15, and degree-13 Chebyshev
polynomials. It then reconstructs
`0.5*x*composite_sign(x/B) + 0.5*x`. The original refreshed ciphertext and
the normalized ciphertext are therefore distinct inputs. Bootstrap restores
capacity; it does not compute ReLU.

## ACE Polynomial Algorithm

For each stage, ACE computes the low-degree threshold and full-decomposition
threshold from the next power of two above `degree+1`. It precomputes reusable
Chebyshev values with:

```text
T_(2n)   = 2*T_n*T_n - 1
T_(n+m)  = 2*T_n*T_m - T_(m-n)
```

It recursively decomposes the coefficient vector as:

```text
p(x) = q(x) * T_(2^k)(x) + r(x)
```

and emits the quotient, divisor multiply, remainder, and add in dependency
order. The outer degree-13 stage folds `0.5*x` into its coefficients before
the final `+0.5*x`. In the common profile vocabulary this is
`DSL_FHE_APPROX_EVAL_ADDITION_CHAIN`, more specifically the ACE BSGS-style
Chebyshev decomposition. It is not Clenshaw and is not the ANT runtime's
separate generic `Eval_chebyshev_ps` entry point.

`VHO_FHE_CKKS_Build_Ace_Relu_Recipe()` ports this algorithm without mutating
WHIRL. The pinned fixture produces 94 algebraic steps, divided 18/35/39 among
the three polynomial stages plus two reconstruction steps. Its exact outer
stage ordering is `(x * encoded_coefficient) * T_degree`, including the `T0`
term. It must not be reassociated to `(T_degree * coefficient) * x`: that
apparently equivalent clear expression consumes one extra CKKS multiplication
layer. The resulting critical-path allocation is exactly `3+4+4=11`, matching
the pinned ACE `App_relu` profile. ACE's preceding `x/B` normalization is an
additional encoded-plaintext multiply and rescale, so the complete path from
the post-bootstrap value to the ReLU result consumes 12 levels. The executable
materializer must add explicit encode/rescale/relinearize/modswitch nodes
according to the pinned generated program and prove each concrete context's
final level independently.

## Compatibility Rule

No operator, ELF section, row layout, or binary WHIRL revision changes. The
existing append-only evaluator enum already contains `ADDITION_CHAIN`.
Current producers and semantic gates use that value for newly captured ACE
profiles. Previously retained `CLENSHAW`-tagged files remain readable as
historical planning artifacts but fail the executable S6-0c ReLU gate. They
must be recaptured; the compiler must not reinterpret their persisted enum.

The coefficient bytes, coefficient hashes, v1 range evidence, and clear
accuracy evidence do not change because they identify the mathematical
polynomial. The native parser accepts that exact authenticated v1 calibration
identity as evidence for the v2 execution profile; no other cross-version
mapping is legal.
Clenshaw remains a useful independent clear numerical oracle, not the emitted
operation schedule.

## Next Implementation Steps

1. Materialize one authenticated repeated-scalar tensor for `1/B`, each
   derived coefficient used by the decomposed tree, `-1`, `2`, and `0.5`.
2. Insert explicit `ckks.encode` at the operand level required by each use.
3. Bind the pre-ReLU input's pending refresh state, then emit
   `ckks.bootstrap` with the exact context target and key.
4. Emit normalization multiply/rescale and the 94-step algebraic DAG with
   explicit multiply repair and level alignment.
5. Prove the physical state path reaches the reviewed context result level
   and matches direct Chebyshev evaluation before retiring `common.relu`.
6. Repeat for all nineteen specialized contexts, then allow pool lowering to
   consume the nineteenth concrete ReLU result.

Any mismatch in coefficient bytes, evaluator tag, B, target level, key,
operation order, or final state fails before source retirement and leaves no
published checkpoint.

## Executable Event Plan

`VHO_FHE_CKKS_Build_Ace_Relu_Event_Plan()` now converts the corrected
94-step algebraic recipe into a canonical explicit CKKS event plan without
mutating WHIRL. For each approved refresh target (15, 17, or 18), the plan has
the same 223-step shape:

| CKKS operation | Count |
| --- | ---: |
| bootstrap | 1 |
| encode | 31 |
| multiply | 50 |
| relinearize | 27 |
| rescale | 50 |
| modswitch | 27 |
| add | 37 |

The final levels are respectively 3, 5, and 6 after the complete 12-level
post-refresh path. Every multiply has an explicit rescale; each ciphertext
product additionally has an explicit relinearization; and additions receive
explicit modulus alignment where dependency depths differ. The plan uses
authenticated external scalar-value IDs for `1/B`, coefficients, `-1`, `2`,
and `0.5`. Creating those values, invoking the native expansion transaction
for all nineteen contexts, and retiring the source `common.relu` definitions
remain the next milestone.

The checked-in SYNC-3 `ckks-schedule-manifest.json` remains immutable
historical conversion-planning evidence. It modeled normalization as
level-neutral and records final levels 4/6/7; it is not executable schedule
authority for this SYNC-6 path. SYNC-6 uses the explicit O0 operations above,
whose additional normalization rescale yields 3/5/6. Before replacing all
nineteen ReLUs, the producer must reflow these concrete results into
downstream Conv planning rather than consuming the historical predicted
states. This reflow and topological materialization ordering are part of the
next mutation milestone.
