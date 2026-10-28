# LN-REF-1 Target Arithmetic And Environment Binding

Version v0.1 | 2026-10-28 | LN1-c compiler binding and primitive carriers; no LayerNorm execution claim

## 1. Scope And Terms

Implement the target-arithmetic slice of
[LN-REF-1](AI-COMPILER-LAYERNORM-REFERENCE-ARITHMETIC-v0.1.md), after
[native v2 admission](AI-COMPILER-LAYERNORM-NATIVE-REFERENCE-CONTRACT-v0.1.md).
V1 and its Python producer remain unresolved and unchanged. No reduction
selector/lowering/runtime service is required or modified.

| Term | Definition |
| --- | --- |
| Source binding | Exact native v2 ordered means, count, sqrt/div and observation semantics. |
| Target binding | Conditional compiler eligibility for the selected scalar ISA/carrier/environment; not a completed toolchain invocation or GPU proof. |
| Carrier | Actual standard WHIRL operations holding the selected arithmetic, operands and effects. |
| Pointer-scalar route | One standalone `ASM_STMT` with two/three typed F4 pointer inputs: result, left, optional right. |
| RNE/gradual | Each scalar operation rounds to nearest, ties to even; subnormal inputs/results are preserved. |
| No-trap/class observations | No observable traps, floating flags/errno or NaN payload/sign/signaling distinctions; non-NaN bits stay exact. |
| Toolchain floor | Minimum declared toolkit/PTX contract for an exact target. Actual compiler/version/options and outputs need separate evidence. |
| Source snapshot | All active-PU native node/value/TY/shape/epsilon/source facts; stale snapshots cannot bind another operation. |
| Offline assembly | CUDA-to-PTX compilation and cubin assembly without executing on a GPU. |

`common/com` remains the policy-free source/shape/target-description authority.
`be/opt/opt_dsl_normalization_target` owns PU-local characterization and binding.
The driver owns scope, invocation, target/toolchain controls and publication.
`whirl2c -cuda` prints the explicit WHIRL carrier; it chooses no arithmetic.

## 2. Exact Arithmetic And Target Envelopes

| Operation | Actual WHIRL assembly text | Required meaning |
| --- | --- | --- |
| add32 | `add.rn.f32` | Separately rounded binary32 addition, no FTZ. |
| sub32 | `sub.rn.f32` | Separately rounded binary32 subtraction, no FTZ. |
| mul32 | `mul.rn.f32` | Separately rounded product, not contracted with addition. |
| div32 | `div.rn.f32` | Rounded division, not rounded reciprocal multiplication. |
| sqrt32 | `sqrt.rn.f32` | Rounded square root, not approximate rsqrt. |

The mean divisions and `t=add32(v,epsilon); h=sqrt32(t); q=div32(+1,h)`
retain the source's separate rounding points. This milestone supplies primitive
carriers, not their insertion into a complete normalized-row loop. Exact count
bits derive from integer N using the existing LN1-b predicate; no repeated
floating-point count or padded denominator is introduced.

| Exact common target | Toolkit floor encoding | PTX floor encoding | Initial route |
| --- | --- | --- | --- |
| `sm_90`, `sm_90a` | 12000 (CUDA 12.0) | 800 (PTX 8.0) | Pointer-scalar v1 |
| `sm_100`, `sm_100a` | 12080 (CUDA 12.8) | 807 (PTX 8.7) | Pointer-scalar v1 |

Toolkit encoding is `major*1000+minor*10`; PTX is `major*100+minor`.
These are reviewed compiler envelopes, not a quantitative latency/throughput
profile or claimed earliest hardware support for basic arithmetic. Use the
existing exact common architecture lookup, including suffixes; do not alias
unknown names such as `sm_100f`. No architecture/latency table is duplicated.

The NVIDIA [PTX division specification](https://docs.nvidia.com/cuda/parallel-thread-execution/index.html#floating-point-instructions-div)
and [square-root specification](https://docs.nvidia.com/cuda/parallel-thread-execution/index.html#floating-point-instructions-sqrt)
distinguish rounded and approximate forms and document subnormal support when
FTZ is absent. The [inline PTX guide](https://docs.nvidia.com/cuda/inline-ptx-assembly/index.html)
defines operands, volatility and compiler memory effects. These references
justify the ISA contract, not a claim that this compiler or GPU was certified.

## 3. Read-Only Binding Algorithm

`WOPT_DSL_bind_normalization_target` reuses the existing native whole-active-PU
characterization service. It is an explicit service, not a newly enabled pass,
compiler option or heuristic. Existing PREOPT/O2 invocation and trace controls
are unchanged; no O0/O1 profitability or cross-PU analysis is introduced.

```text
bind(scope, assignment, request):
    rederive verified source facts from active whole-PU WHIRL
    if scope/node/arguments invalid: return API failure, output/trace unchanged
    if any expected source field differs: refuse stale_source
    if source is not explicit LN-REF-1 v2: refuse source_unbound
    resolve exact common sm_* description or refuse unknown_architecture
    require pointer_scalar_v1 or refuse unsupported_route
    require target-specific toolkit/PTX floors or refuse unsupported_toolchain
    require RNE/gradual/no-traps/class-only observation contract
    reject every nonzero unsafe/unknown FP option bit
    reject active fast-math, nonzero roundoff or IEEE_INEXACT/IEEE_ANY
    derive exact count bits from integer N
    return conditional_isa_carrier_binding with source+ISA stages only
```

All source facts, including the source characterization's `TARGET_UNBOUND`
flag, stay unchanged. A separate transient binding records conditional target
eligibility. No source mutation, v1 promotion, managed row, new physical opcode,
intrinsic ordinal, binary layout or 64-byte semantic-key change occurs.

Unsafe options include FTZ, DAZ, fast-math, approximate div/sqrt, contraction,
reassociation, observable exceptions and NaN-payload observations. Unknown bits
also fail closed. The binding API never silently rewrites caller controls.
The actual toolchain adapter must enforce accepted options and record the
invocation; a request field is not proof that the toolchain obeyed it.

## 4. Carrier Construction And Ownership

Reuse the existing `ASM_STMT`/`ASM_INPUT` protocol already supported by the
MMA CUDA emitter. The legacy `TARG_NVISA` F4ADD/F4MUL rounded intrinsics are
review anchors, not host-built CUDA availability. Unguarded ordinal insertion,
new intrinsics or plain F4 arithmetic with presumed rounding are unnecessary.
This follows [WHIRL's](https://www.mcs.anl.gov/OpenAD/open64A.pdf) typed operands,
strict-tree ownership, explicit effects and continuous-lowering principles.

```text
create_scalar(target, operation, address_trees, source_position):
    validate active PU, exact target, strict controls and complete source site
    validate closed operation, arity, distinct WN trees and local F4 pointer ST/TY joins
    only after complete preflight, construct:
        ASM_STMT volatile + memory-clobber
        kid0 = empty explicit-clobber block
        kid1 = empty output-constraint block
        remaining kids = ordered ASM_INPUT "l" pointers
        text = load all operands; exact operation.rn.f32; store result
    return detached owned statement; caller inserts/publishes it
```

Unary sqrt has two pointer inputs; binary operations have three. Result storage
may alias an input: all operand loads precede the store. Tree nodes themselves
must be distinct. Caller guarantees live, naturally aligned four-byte addresses
and any larger algorithm's lifetime/alias/event obligations. No lease or
physical-storage guarantee is inferred from a logical tensor descriptor.

`WOPT_DSL_verify_normalization_scalar` rechecks actual text, types, operand
order, effects and supported assembly shape after edits. A debug label cannot
make an altered `.ftz`/approximate/incorrect body conforming. Construction
failure allocates no product WN/ST/TY and does not take argument ownership.
Source position is attached to the statement and introduced symbols; expression
kids use their normal WHIRL representation rather than invented line fields.

The memory clobber is intentionally conservative. This first conformance route
may inhibit motion and force memory traffic; it is not the final efficient
register-output realization. Later register-output carriers require an explicit
review of existing ASM copyout-preg semantics and preservation/emission tests.
Cost models must not count this route as one bare arithmetic instruction or
promote it to a performance plan without charging its loads/stores and effects.

## 5. Traces And Tool Summaries

`AI_NORM_REF` and v1 FORM/FACT traces remain unchanged. The additive query trace
is controlled by the existing caller's trace stream; NULL gives identical
validation/results without output. No new trace bit or automatic pass is added.
The explicit native fixture exercises enabled/disabled parity for every query.

```text
AI_NORM_TARGET 1 HEADER pu node result recipe arch arch_id route toolkit ptx environment fp_options fast_math roundoff ieee source_match status stages count_bits reason source_line
AI_NORM_TARGET 1 BIND ...
```

`recipe=1` is unresolved v1, `recipe=2` explicit LN-REF-1. Status values follow
the public enum: bound=0, source-unbound=1, stale=2, unknown-target=3,
unsupported-route=4, unsupported-toolchain=5, unsafe-environment=6. Stage bits
are source=1 and ISA/carrier=2. No runtime, executable-plan or calibration stage
is implied. Refusal count bits stay zero. Source line remains last.

| Report | What it summarizes |
| --- | --- |
| `semantic.log` | Actual PREOPT v2 binding plus preserved unresolved v1, source/TY/epsilon/count identities. |
| `target-binding.log` | Canonical source join, exact target, route, toolchain/environment inputs, stage/status and each refusal reason. |
| `compile.log` | Actual compiler/version/options, WHIRL-to-CUDA operands/effects, exact PTX instructions and offline cubin acceptance. No GPU accuracy claim. |
| `optimization.log` | Complete successful semantic, target and scalar-compile report collection. |
| `optimization-summary.log` | Phase coverage, query census, v1 preservation, scalar-only actions and remaining oracle/storage/cost/LayerNorm/GPU gates. |

All logs begin `# What it summarizes:`. The auditor joins actual native query
rows to real PREOPT source facts, recomputes status precedence/stages/count bits
and rejects forged schema/status/proof claims. Source-snapshot equality itself
is checked natively field-by-field; its boolean trace is not a substitute for
retained negative-test evidence. Numerical oracle results are not generated by
this contract-only audit.

## 6. Evidence And Next Gates

`run_normalization_target_review.py` produces four exact target cases with
mixed v1/v2 source, 39 target queries each, real PREOPT and mapped reopen.
Separate fresh-process primitive fixtures preserve before/after `.B`/`.T` and
raw diffs, then generate `.cu` through rebuilt `whirl2c -cuda`. The runner checks
all five operations, pointer operands, volatility/memory effects, no FTZ or
approximate replacement in PTX, and offline cubin assembly. Explicit CUDA flags
are `--fmad=false --ftz=false --prec-div=true --prec-sqrt=true`.

These tests establish compiler binding and primitive-carrier/toolchain
acceptance, not LN1-d exact arithmetic, a full normalized-row kernel, GPU
instruction behavior, accuracy or timing. LN1-e integrated numerical evidence,
LN2 storage/events, LN3 complete profitability and LN4 selected WHIRL lowering
remain open. The progress ledger indexes immutable receipts and final-path
mapped/trace replay; reduction receipts and coding are independent.
