# Native mean/variance normalization contract v0.1

Status: bounded R1 native representation, exact Python ingestion and canonical-WHIRL characterization implemented.
Independent of MMA1. General normalization ingestion, executable lowering,
physical optimization and GPU certification remain separate gates.

## Terms and architectural baseline

| Term | Definition |
| --- | --- |
| FP32 | IEEE binary32 floating-point values; native tensor elements must have the corresponding `MTYPE_F4` representation. |
| FMA | Fused multiply-add with one rounding, distinct from a separately rounded multiply followed by add. |
| FTZ/DAZ | Flush-to-zero / denormals-as-zero handling; target treatment of subnormal results/inputs. |
| Row domain | `r in [0,R)`, `k in [0,N)` of static positive FP32 input `X[R,N]`. |
| Population centered variance | Mean of squared deviations from the input row mean; mathematical denominator `N`, not `N-1`. |
| Affine operands | FP32 vectors `gamma[N]` (scale) and `beta[N]` (bias), shared across rows by logical broadcast. |
| Epsilon | A positive finite binary32 scalar added to variance before reciprocal square root. |
| Recipe | The exact sequence of mean, center, square, mean, epsilon-add, rsqrt, normalization, scale and bias. |
| Characterization | Read-only facts derived from canonical native WHIRL and managed records, not source/Python/debug facts. |
| Unresolved | A numerical or execution obligation not yet bound; never permission to substitute an implementation. |

The [WHIRL specification](https://www.mcs.anl.gov/OpenAD/open64A.pdf)
(August 3, 2007, sections 1.3, 1.4 and 1.18) supplies the high-level typed
expression, direct-kid strict tree, continuous-lowering and ordinary ASCII
inspection rules. This extension adds logical meaning through the existing
native `OPR_DSL` carrier; it does not import historical physical structure sizes.

## Normative native v1 contract

| Property | Requirement |
| --- | --- |
| Name and ID | `common.mean_variance_norm.v1`, append-only `OPR_DSLMEANVARNORM = 39`. |
| Operands | Three direct typed `LDID` kids in order `X[R,N]`, `gamma[N]`, `beta[N]`, all FP32. |
| Result | Native expression assigned by `STID` into an explicit managed FP32 `[R,N]` result. |
| Axis | Exact `attr.axis=1`; producer normalizes source axis spelling. |
| Precision | `attr.accum_dtype=float32`; intermediates/results are FP32. |
| Variance | `attr.variance_kind=population_centered`. |
| Epsilon | `attr.epsilon_bits`: exactly eight lowercase hexadecimal digits encoding a positive finite IEEE binary32 scalar. |
| Permissions | `attr.reassociate=false`, `attr.state_reformulation=false`, `attr.contract=false`. |
| Rsqrt | `attr.rsqrt_semantics=source_unresolved`. |
| Effects/scope | Pure reads of all three inputs; a distinct logical result; whole active driver-selected PU only. |

Exactly those eight attributes are required. Missing/extra/duplicate attributes,
unsupported versions, mixed precision, dynamic/empty axes, wrong ranks/extents,
row-dependent affine arrays and foreign-PU operands fail closed before builder
WN/symbol/image allocation. Explicit-result construction is mandatory. Input
and result may share an interned TY; gamma and beta may share a TY or even an
input value. None of these facts proves physical alias freedom.

The recipe, with FP32 intermediate values and the displayed operation order:

```text
mu[r] = mean_fp32(X[r,:])
d[r,k] = X[r,k] - mu[r]
v[r] = mean_fp32(d[r,:] * d[r,:])
q[r] = rsqrt_fp32(v[r] + epsilon)
u[r,k] = d[r,k] * q[r]
z[r,k] = u[r,k] * gamma[k]
Y[r,k] = z[r,k] + beta[k]
```

`mean_fp32` is a source-level mean primitive with mathematical population
denominator `N`; this contract does not prescribe a sum tree or the finite-
precision point of reciprocal scaling. It must not be silently lowered as an
assumed sequential sum followed by one division. `contract=false` prohibits
replacing the displayed final multiply/add by FMA. Rsqrt-to-sqrt/divide,
reciprocal approximations and precision changes are not implicitly authorized.

This is the centered affine LayerNorm source recipe, not RMSNorm, inference
BatchNorm with supplied statistics, unbiased sample variance, raw second moments
`E[x*x]-E[x]^2`, Welford, or generic framework `layer_norm` equivalence.
Existing `common.normalization_base`, RMSNorm and BatchNorm contracts are unchanged.

## Epsilon and numerical boundaries

For example, binary32-rounded `1e-5` is encoded as `3727c5ac`. The attribute
captures the binary32 value, not a host-parsed decimal string or a double.
Canonical parsing is locale/host-rounding/endian independent: no optional
prefix, uppercase, sign, alternate length, zero, infinity or NaN is accepted.
Positive subnormal encodings are representable, but target FTZ/DAZ behavior
must still be bound before executable selection. Parsing failure leaves the
caller's bits unchanged. A producer must diagnose scalar conversion to zero
or infinity, not repair it silently.

Strict numerical facts remain explicit:

- Reassociation, partitioned-state reformulation and FMA contraction are forbidden.
- Mean accumulation order/scaling and rsqrt approximation/rounding/exception
  behavior remain unresolved. They are not selected by PREOPT.
- Input finiteness, subtraction/squaring/sum/affine overflow safety, and general
  accuracy are unproved. Positive epsilon alone does not prove finite execution.
- NaN/infinity, huge offsets with small spread, cancellation, and constant rows
  require the existing R1 numerical acceptance discipline. No clamping,
  epsilon relocation or replacement of exceptional outputs is permitted.
- Constant finite rows yield beta in real arithmetic. Sample agreement with
  that property is not a universal binary32/target proof.

Future alternatives may retain or recompute centered values while preserving
the same means, rsqrt and affine order. They still need numerical binding and
complete storage/event/cost evaluation. Welford and raw-moment substitution are
permission-rejected under v1, not merely assigned a worse cost.

## Ownership and canonicalization

Common Compiler Substrate owns stable schemas, type/shape/epsilon predicates,
native construction and managed identity in `common/com`; it contains no cost,
candidate or target selection. The row recipe is reusable tensor numerical
meaning, not an MMA capability or FHE approximation policy.

WOPT owns transient `WOPT_DSL_NORMALIZATION_CHARACTERIZATION`, deriving the
recipe, PU/node/operand/result IDs, TYs, shape, epsilon and permission/unknown
flags from a verified active-PU assignment. It reads no Python report, sample
values or diagnostic metadata. Output is published only on complete success.
The snapshot is not persisted and is not added to the existing 64-byte WOPT
semantic identity structure. Any relevant WN/TY/attribute/managed-link/ownership
change invalidates it; callers recompute rather than trusting a stale cache.

PREOPT verifies the canonical form before import and after emission. WOPT
imports it as an opaque pure ternary expression, including all operand
descriptors and exact attributes in semantic identity. Emission uses existing
managed-node rewrite services to keep all three direct kids, payload names and
managed operand references consistent. No separate scalar simplifier is added.
The existing PREOPT DSL expression emitter now admits up to three kids; the
operator-specific bridge still validates exact arity. Actual PREOPT tests found
and cover this integration boundary in addition to direct API round trips.

Scope is the whole active PU. Explicit REGION normalization scopes and foreign
or detached assignments are rejected; no PU traversal or symbol-table switch
enlarges scope. Non-normalization trees are no-ops. This verifies an already
canonical native form, not arbitrary scalar-loop recognition or algorithm choice.
Actual integration is exercised at `-O2` with DSL PREOPT enabled; this adds no
performance transformation at any optimization level. Lower-level execution
of an unbound recipe remains unsupported even with DSL PREOPT disabled.

## Logical state, access and storage obligations

There are `R` logical means and `R` logical centered population variances,
followed by `R` inverse-standard-deviation values. This is not a simultaneous
physical allocation ledger and not a Welford mergeable-state declaration.
Centered values `d[R,N]` may be retained or recomputed in a later selected plan.

Logical reads are `X[r,k]`, `gamma[k]`, `beta[k]`; logical output definitions
are `Y[r,k]`. Affine vectors broadcast across `r`; output ownership is an
unassigned per-row obligation. Physical alias freedom, ownership by CTA/warp/
thread, data residency, HBM traffic, buffer capacities/lifetimes and async
events remain unknown/unplanned. Read-only inputs do not prove noalias.

## Trace and report contract

Use existing `-DSL:wopt=on` and `-DSL:ai_opt_trace=on|off` (default on), with
the existing TP_WOPT2 AI trace mask `0x00800000`; no new bit is allocated.
Frontend pass-through remains `-Wb,`; focused backend tests use `-tr25`.
Verification/derivation still execute when trace emission is disabled.

New additive tags are `AI_NORM 1 FORM` and `AI_NORM_FACT 1 FACT`. Their exact
positional headers live in `normalization_trace_report.py`, once per verifier
invocation; source line is last. FORM records shape, population variance,
epsilon bits and permissions. FACT adds all three operand identities/TYs,
state recipe, denominator, epsilon position, affine order, access relations,
ownership, alias/storage obligations and numerical unknowns. Existing reduction
and Softmax schemas are unchanged.

The auditor must require complete identical entry/exit pairs, exact schema and
permissions, canonical epsilon, positive identities/domains/source lines,
FACT-to-FORM agreement and denominator equal to width. It must reject invented
range/accuracy/noalias/storage proofs, altered affine broadcasts, sample
variance and epsilon moved outside rsqrt. It must not infer frontend provenance
from a native verification trace.

`semantic.log` summarizes recipe, states, affine reads/broadcasts, epsilon,
population denominator, numerical prohibitions/unknowns and ownership/storage
obligations. `optimization.log` collects successful phase reports;
`optimization-summary.log` reports coverage and remaining gates. Every log
starts with `# What it summarizes:` and explicitly says
`candidate_selection=not_run profitability=not_run transformation=none` where
appropriate. Verification events are not optimization actions.

## Compatibility, lowering and evidence

Logical ID 39 and its common runtime seed are appended; existing IDs, common
seed order and operator meanings are retained. No physical opcode/kid layout,
TY kind, ELF section, binary version, mapped row or reader/writer format changes.
The native ternary carrier already exists for other DSL operators. Current
`ir_b2a -st -src` inspects the new logical name; older readers are not promised
understanding of newly introduced logical semantics.

Very-high/high WHIRL and PREOPT admit the contract. Registered `MARKER_ONLY`
means no executable lowering route, not a legacy physical marker. VHO must
diagnose that missing route rather than invent an implementation. Failed partial
`.O` output is labeled `.incomplete-mapped-output`, never a finalized image.

Required evidence: static `3x17`, unit `1x1`, wide `7x65`, and second-epsilon
native fixtures; malformed type/shape/schema/epsilon/ownership and output-
unchanged checks; three-operand WOPT import/emission and actual PREOPT;
trace default/on/off, PREOPT-off and lower-level rejection; separate-process
mapped source-interleaved reopen; strict auditor, previous reduction/Softmax,
numerical and link-boundary regressions. Preserve `.B`, `.T`, raw `.t`, reports,
source snapshots, exact commands, product hashes and checksums in a fresh
immutable `r1-native-normalization` review receipt. ASCII and trace directories
stay separate on case-insensitive filesystems. No transformed before/after
images are fabricated because this increment performs no executable transformation.

The exact centered affine Python matcher now connects to this published
contract (see the source-ingestion increment below). General normalization forms, executable numerical binding,
storage/event alternatives, complete profitability comparison and selected
WHIRL lowering remain separate gates. No MMA1, CUDA, PTX/SASS or GPU claim.

## Accepted evidence, 2026-10-27

All four native/PREOPT variants pass: `3x17`, `1x1`, `7x65`, and `3x17` with
epsilon `3a83126f` instead of `3727c5ac`. Each executes 51 main contract assertions
plus three cross-PU operand checks: 54 checks per variant, 216 executions total.
Nineteen trace-report tests (seven normalization, twelve existing row-operation
tests), 21 source semantic/numerical tests, native syntax/layout, WOPT semantic/
driver/canonicalization, CKKS transaction rollback, view retirement and checkpoint
publication checks pass. Rebuilt be/WOPT/inline symbol boundaries remain closed.
Five authenticated historical Python/driver row-sum images also replay under
the new reader/backend; native Softmax and reduction fixtures pass again.

The 12-case CPU report is source evidence only. It retains strict/permitted
alternative acceptance and numerical/domain/permission rejections; it does not
execute the new native normalization node or grant target accuracy permission.
The full FHE producer is not rerun: no FHE algorithm, materializer, mapped layout
or existing logical meaning changed. Focused common/FHE compatibility lanes
cover the append-only structural integration.

Reproduce the four variants using fresh subdirectories:

```sh
make -C "$BUILD/osprey/targdir/ir_tools" -j6 dsl_normalization_contract_test ir_b2a
make -C "$BUILD/osprey/targdir/wopt" -j6
python3 osprey/be/opt/tests/run_native_normalization_review.py \
  --build-dir "$BUILD/osprey/targdir" --output-dir /absolute/new/native/default
# Repeat with --variant unit, wide and epsilon2 in their own fresh subdirectories.
python3 osprey/be/opt/tests/run_native_normalization_review.py \
  --build-dir "$BUILD/osprey/targdir" --output-dir /absolute/new/native --summarize-suite
python3 -m unittest discover -s osprey/be/opt/tests -p 'test_*trace_report.py' -v
```

Immutable review root (primary `native/default/normalization.B`):

```text
/Users/shinmingliu/open64/artifacts/review/ai-optimization/research/r1-native-normalization/sha256-0dd6b7f8ec92add7324127ffe37422d7162de0de5d177f7e8a0c0df5b559b91c/
```

Start with `native/optimization-summary.log`,
`native/default/reports/semantic.log`, `native/default/ascii/normalization.T`
and `native/default/trace/preopt-on.t`. The representative trace preserves
node 1, input/gamma/beta/result values 1/2/3/5, `[3,17]`, epsilon `3727c5ac`
and fixture line 206. These are receipt observations, not ABI constants.
Four durable source-interleaved mapped reopens and strict report replays must
match the staged artifacts. No executable transformation or GPU run is claimed.

## Python Ingestion Increment, 2026-10-27

The shared structural matcher now recognizes the complete exact centered
affine recipe before ShapeProp, native allocation or publication. Both source
analysis and ingestion use the same edge checks, not model/class names. It
requires all three placeholders, the same centered node on both sides of the
square, last-axis means with `keepdim=True`, epsilon inside rsqrt, then
normalization, scale, bias in that order. Extra effects, unused operations/
arguments, sample variance, raw moments, duplicate centering and changed affine
order fail closed. Opaque framework `layer_norm` is not declared equivalent.

Source inputs must be contiguous FP32 `X[R,N]`, `gamma[N]`, `beta[N]`, with
static positive extents. Descriptor and recipe rejection precede evaluation,
so a rejected mutation is never applied to the caller's tensors. Source axis
spellings `-1`, `1`, `(-1,)` and `[1]` normalize to native axis `1`.
`struct.pack`/`unpack` encodes the scalar epsilon as binary32 bits, rejecting
conversion to zero or infinity before evaluation; positive subnormals remain
representable without granting target FTZ/DAZ permission.

The existing row emitter now accepts ordered input nodes: three runtime
formals, explicit result, direct typed operands and PU return use opaque C++
builder APIs. No sample tensor is embedded as a constant; no new physical
layout/opcode or C++ API is introduced. Unary row-sum/Softmax names and emission
remain compatible. The region source location is the actual final addition,
not the class/forward header. Python mirrors the exact eight-attribute schema
and `[R,N],[N],[N] -> [R,N]` descriptor checks; native common/com remains the
authority. WOPT still derives every FACT from canonical WHIRL, never FX reports.

The review producer exercises five fresh-process API cases (`3x17` with negative
and positive axes, `1x1`, `7x65`, and epsilon `3a83126f`) plus the real
`torch2whirl --model-factory AffineLayerNorm` driver. All six images are reopened
with `ir_b2a -st -src` and reach actual PREOPT. Default/on/off AI trace and
PREOPT-off controls are checked. Sixteen unsupported cases publish no `.B`.
Five CPU source witnesses compare to an independent scalar FP64 centered
population oracle with binary32-rounded epsilon at the existing `rtol=2e-5`,
`atol=2e-6`; these do not execute the native node or prove general accuracy.

The required summary distinguishes ingestion/native verification from
`candidate_selection=not_run profitability=not_run executable_transformation=none`.
`semantic.log` retains epsilon, denominator, affine input identities/accesses,
strict numerical permissions/unknowns and alias/ownership/storage obligations.
Raw `AI_NORM`/`AI_NORM_FACT` traces and trace controls are unchanged. The auditor
does not infer Python provenance from them; source witnesses and producer
receipts independently establish that bridge.

Reproduction (use a fresh output directory and the native-extension package):

```sh
make -C "$BUILD/osprey/targdir/torch2whirl" -j6 python_native_extension all
python3 osprey/torch2whirl/python/tests/normalization_native_review.py \
  --build-dir "$BUILD/osprey/targdir" --artifact-dir /absolute/new/python-normalization
python3 -m unittest discover -s osprey/torch2whirl/python/tests \
  -p 'test_normalization_capture_optional.py' -v
```

Ten focused source tests cover structural rejection before evaluation, source
locations, numerical restrictions, binary32 epsilon conversion, affine shapes
and verifier drift. Regression coverage includes both prior row emitters,
source numerical witnesses, unchanged FX census, isolated native API lifecycles,
mapped inspection and strict trace auditors. Native backend products must match
the authenticated native-normalization receipt; no MMA1/compute profile changes.
General axes/types/regions, numerical binding, executable alternatives, complete
profitability, selected lowering and GPU certification remain open.

Accepted source-connection validation: all ten focused normalization tests,
prior row-sum/Softmax native review producers, 13 isolated native API tests,
native mapped-image smoke, 19 trace-auditor tests, 21 source numerical tests,
fixture census and skeleton checks pass. The full source suite runs 288 tests,
with 14 skips and one unchanged FHE authority DOCX digest failure (actual
`84D74059...`, expected `0018769C...`). Neither that document nor its assertion
is modified; the suite is not reported as wholly passing.

Immutable source-connection review root (primary
`native/negative-axis/normalization.B`):

```text
/Users/shinmingliu/open64/artifacts/review/ai-optimization/research/r1-python-normalization/sha256-cdd091976cb45ed1390f8dfaf4e0c9de32e595edeb77533be8ef318d8f53ac0e/
```

Start with `native/optimization-summary.log`,
`native/negative-axis/reports/semantic.log`,
`native/negative-axis/ascii/normalization.T` and
`native/negative-axis/trace/default.t`. The trace retains values `1/2/3/5`,
`[3,17]`, epsilon `3727c5ac`, denominator `17` and source final-add line `56`.
Those identities/line numbers are receipt observations, not contract constants.
Six separate final-path source-interleaved mapped reopens and strict report
replays must match before updating the durable review index.
