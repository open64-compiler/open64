# Native LN-REF-1 Contract And Producer Admission

Version v0.1 | 2026-10-28 | Native contract design specified; not registered or implemented

## 1. Decision And Scope

Specify `common.mean_variance_norm.v2` as the explicit native meaning of
[LN-REF-1](AI-COMPILER-LAYERNORM-REFERENCE-ARITHMETIC-v0.1.md). Reuse logical
`OPR_DSLMEANVARNORM` (existing ID 39), its three direct operands, explicit
result and managed WHIRL representation. Do not allocate another logical ID,
physical opcode, node field, type kind, image row or ELF section.

This document defines the proposed native schema and producer-admission rules.
It does not register v2, change code, publish a new C++ API, implement lowering
or certify a target. V2 remains unsupported by current binaries until the
implementation gates below are met. Naming v2 here is not registration.

The source meaning is independent of ongoing reduction profitability/lowering
work. Native construction and verification need no reduction optimizer, selected
reduction plan, cost model or new reduction-runtime dependency. Later lowering
should reuse compatible arithmetic services after their interfaces stabilize,
but the contract is not defined by those services' implementation details.

The [WHIRL specification](https://www.mcs.anl.gov/OpenAD/open64A.pdf), sections
1.3, 1.4 and 1.18, supplies continuous lowering, typed expressions, strict-tree
operand structure and ordinary inspection. The existing DSL extension supplies
versioned logical semantics and managed links; no historical physical sizes
from that specification are copied into a new representation.

## 2. Terms And Trust Boundary

| Term | Definition |
| --- | --- |
| V1 | Existing `common.mean_variance_norm.v1`: exact eight-attribute centered recipe with unresolved mean and rsqrt semantics. |
| V2 | The separately specified native LN-REF-1 contract below, not an automatic refinement of v1. |
| Producer | Frontend or native source builder that declares the operation's source meaning using ordinary opaque builder APIs. |
| Producer admission | Establishing that a producer may declare LN-REF-1 because its source/DSL semantics explicitly provide that meaning. |
| Native structural admission | Checking exact identity, schema, descriptors, operands, result and ownership; not proving an arbitrary producer's honesty or source equivalence. |
| Provenance | Review evidence identifying actual source, producer/version and invocation. Diagnostic context, not a substitute for semantic WHIRL. |
| Canonical form | The versioned pure ternary assignment consumed and preserved by PREOPT and downstream analyses. |
| Exact-version lookup | Lookup by stable name and explicit semantic version, never by latest/current or by truncating the requested number. |
| Publication | Exposing a completed result/managed node or finalized binary image to another phase/process. |
| LN-REF-1 | Ordered binary32 means with final divisions and separately rounded sqrt-then-divide, under RNE/gradual/no-trap/class-only-NaN observations. See the arithmetic specification. |

Native IR is a declaration of program semantics. A verifier checks its internal
consistency; it cannot infer arbitrary source equivalence from a string saying
`authorized`. The producer must actually implement its source-language contract.
No producer allowlist, model class, metadata label, test tolerance, hash or user
flag is by itself a semantic proof. Keep review provenance outside the operator's
attribute set and never require a particular producer name to interpret v2.

## 3. Exact Native Contract

### 3.1 Identity, operands and result

| Property | Required meaning |
| --- | --- |
| Stable identity | `common.mean_variance_norm`, exact semantic version 2. |
| Logical identity | Existing `OPR_DSLMEANVARNORM`, ID 39; version differentiates v1/v2. |
| Operand order | Three direct typed `LDID` operands: X, gamma, beta. No helper calls, hidden policy kid or epsilon tensor. |
| Descriptors | FP32 X/result `[R,N]`, FP32 gamma/beta `[N]`; positive static R, `1 <= N <= 16777216`. Same element/descriptor compatibility rules as v1. |
| Epsilon | One positive finite binary32 scalar in canonical attribute bits, including positive subnormals. |
| Result | Explicit supplied result descriptor and a distinct managed result value, assigned with ordinary typed `STID`; no in-place logical result. |
| Effects | Pure read of the three operands, no program-visible side effects beyond the assigned value under LN-REF-1 observations. |
| Initial compiler scope | Whole active driver-selected PU, as with current native row contracts. No implicit REGION/cross-PU traversal. |
| IR levels | Very-high/high WHIRL and the opaque PREOPT bridge after v2 support is implemented; unresolved executable lowering remains a diagnosed boundary. |
| Initial lowering registration | Existing `MARKER_ONLY` convention means no implemented executable route; it does not mean a legacy textual carrier or permission to emit a placeholder kernel. |

Input/result types may share an interned TY; gamma and beta may even refer to
the same read-only managed value. None of this proves physical alias freedom.
The result value/ST is distinct from operands. Concrete output/scratch ranges,
leases, physical strides and launch resources remain later planning/runtime
obligations, not extra native arithmetic operands.

Initial producers use the existing contiguous/row-major representation
conventions from the bounded source fixtures. Native logical shapes do not prove
actual runtime contiguity; later storage binding must verify real descriptors.
Do not add a new layout spelling or use physical layout as numerical permission.

### 3.2 Exact fifteen-attribute schema

Every listed attribute is required exactly once. No extra, duplicate, missing,
alternate-case or synonymous values are accepted. Producers emit this order for
deterministic review; validation compares the unique name/value set, not a new
semantic dependence on attribute insertion order. Existing canonical identity
services remain authoritative for ordering and comparison.

| Attribute | Canonical value | Meaning |
| --- | --- | --- |
| `attr.axis` | `1` | Last feature axis of rank-2 X. |
| `attr.accum_dtype` | `float32` | FP32 state and scalar intermediates. |
| `attr.variance_kind` | `population_centered` | Mean of separately rounded centered squares, denominator N. |
| `attr.epsilon_bits` | Eight lowercase hexadecimal digits | Exact positive finite binary32 value; existing parser rules. |
| `attr.reassociate` | `false` | No alternative reduction grouping. |
| `attr.state_reformulation` | `false` | No Welford, raw moments or other statistics reformulation. |
| `attr.contract` | `false` | No multiply/add contraction. |
| `attr.rsqrt_semantics` | `sqrt_rne_then_div_rne_f32` | Rounded sqrt, then rounded division of +1 by that result. |
| `attr.mean_order` | `ascending_left_fold` | Both statistics consume features in increasing logical k. |
| `attr.mean_identity_bits` | `00000000` | Independent positive-zero identity for each fold, including first add. |
| `attr.mean_finalize` | `sum_then_div_rne` | One rounded final division per mean, not scaled summands or reciprocal multiply. |
| `attr.count_conversion` | `exact_f32_from_static_n` | Denominator derives exactly from valid integer N. |
| `attr.fp_environment` | `rne_gradual_no_traps` | Each specified scalar operation is binary32 RNE, no FTZ/DAZ, no traps or observable flags/errno. |
| `attr.nan_policy` | `class_only` | NaNs compare by class; all non-NaN result bits, including signed zeros/infinities, are exact. |
| `attr.reference_binding` | `ln_ref_1` | Identifies the complete fixed LN-REF-1 semantics; never overrides contradictory fields. |

Version 2 fixes the two means to the same order/identity/finalization. Their
separate accumulators, count domain, separately rounded square and affine
operation order follow from LN-REF-1, not hidden defaults inferred by a target.
No dynamic denominator, finite-input assumption, approximation tolerance,
source-proof string, architecture key or cost hint belongs in this schema.

Reference identity and explicit fields are intentionally cross-checked. A row
with `reference_binding=ln_ref_1` and an approximate rsqrt or changed mean order
is malformed; the identifier must not make the remaining fields decorative.
Unknown versions and future policies fail closed rather than selecting v2 by
nearest match. This schema is a design specification, not a claim that today's
builder accepts these tokens.

## 4. Frozen V1 And Version Dispatch

The [existing v1 contract](AI-COMPILER-NATIVE-NORMALIZATION-CONTRACT-v0.1.md)
remains authoritative without edits. Its exact eight fields and values remain
unchanged, including `rsqrt_semantics=source_unresolved`. Its positive-width
domain is not narrowed to the v2 count bound. Its unresolved flags are not
cleared by knowledge that a newer native version exists.

Required dispatch behavior after registration:

| Request or image | Required outcome |
| --- | --- |
| Version 1, exact v1 schema | Admit v1; preserve unresolved semantics and existing FORM/FACT traces. |
| Version 1, any v2-only field or resolved rsqrt token | Reject malformed v1; do not upgrade. |
| Version 2, exact schema/domain above | Admit native LN-REF-1; target/storage/cost/lowering still unproved. |
| Version 2, v1 schema or unresolved rsqrt token | Reject malformed v2; do not fill defaults or downgrade. |
| Version 0, unsupported version or unknown name | Reject explicit identity; never use current/latest. |
| Wide public version such as 65538 | Reject before narrowing to UINT16; it must not alias version 2. |
| V2 request to a current binary without registration | Unsupported version; no image or invented compatibility projection. |

Preserve explicit v1 lookup in the existing source row emitter. In particular,
`row_ingestion.py` currently calls `builder.operator(opcode, 1, ...)`; adding v2
must not replace that with current-version lookup. Do not change global defaults
to migrate old producers. No v1-to-v2 rewrite or migration pass is authorized by
this milestone, including a rewrite under fast-math or an experimental option.

## 5. Producer Admission Rules

| Producer path | Admission rule |
| --- | --- |
| Explicit native LN-REF-1 fixture | First implementation lane: a fixture authors the bound operation as its source meaning and requests exact version 2/schema through the existing explicit-result builder. It does not claim to represent an unbound PyTorch operator. |
| Conforming explicit DSL producer | Source language/API must explicitly define the complete LN-REF-1 observation contract. A future frontend mapping needs its own capture/operator-census and integration review before claiming support. |
| Independently read v2 binary WHIRL | Native structural admission checks the actual version/schema/links. Meaning comes from the operator, not a required producer label. Missing frontend provenance prevents a source-connection claim, not interpretation of otherwise valid v2 semantics. |
| Current exact centered PyTorch matcher | Continue to emit v1. Capturing the centered recipe, positive epsilon or sample agreement does not bind mean/rsqrt. |
| Opaque `torch.nn.LayerNorm` / framework primitive | No new admission from this design; capture and prove a separately reviewed mapping. |
| Arbitrary scalar loops or a graph resembling LN-REF-1 | No recognition/promotion pass is introduced. Exact source-equivalence analysis is a separate future proposal, not an assertion attached by metadata. |
| Class name, function annotation, `lowering_hint`, manifest, command-line target/optimization flag | Insufficient. Do not convert v1 or synthesize v2 attributes from these alone. |

The explicit native producer takes runtime X/gamma/beta values, not sampled
constants or a finite-only specialization. It authors epsilon as an exact
binary32 bit pattern before construction. It supplies an explicit result TY,
actual source position and ordinary runtime formal/result/return records.
Whole-source program construction remains driver-owned; no temporary selection
of another PU or source-language interpreter enters the backend.

The producer's obligation is semantic fidelity, not a cryptographic capability
token. A caller can lie about any compiler IR's source meaning; a string or
allowlist cannot fix that. Certification retains the actual source/producer and
commands so reviewers can establish admission. Optimization rederives facts from
verified native v2 and never consults an external report as executable authority.

## 6. Builder, Verifier And Failure Protocol

Reuse `DSL_Builder_Create_Operator_With_Result` and the existing opaque bridge
where applicable, with explicit name/version lookup. Do not add a separate
LayerNorm runtime/kernel builder. The inferred-result compatibility builder
continues to refuse row contracts. C++ remains the schema/shape/native authority;
any future Python mirror must not independently assign WHIRL tables or types.

Required preflight order, after normal context/registry initialization:

```text
validate public integer version in [1,65535] before any narrowing
resolve exact stable name + version and its descriptor
validate active PU, three live operand handles and explicit result descriptor
validate exact unique attribute set and all fixed LN-REF-1 values
validate canonical epsilon, FP32 ranks/extents and v2 count bound
validate operand/result ownership, type compatibility and managed joins
only then construct the provisional native node/result and managed links
verify canonical form and publish result through ordinary builder services
```

Malformed input refusal must allocate no product WN/ST/TY or managed node/value/
attribute rows and leave prior program state/caller output unchanged. Already
existing context/registry initialization is not a new program product. Reject
null/expired/foreign handles, detached operands, unknown versions, schema/count
errors and overflow before commit. Every count/range calculation is checked.

After product allocation starts, use existing transaction/publication services
for any fallible work; do not claim that every memory pool can be rolled back.
If the implementation cannot satisfy its state/publication guarantee, stop at
that gate and review the missing mechanism rather than exposing a half-node.
Binary image publication is atomic; failure retains diagnostics and labels any
partial output incomplete, never as a valid `.B`.

Split reusable descriptor/epsilon validation from version-specific arithmetic
validation without widening v1. The existing
`DSL_mean_variance_norm_types_valid` requires the v1 unresolved token; do not
pass fake v1 attributes to it and call that v2 verification. Preserve its public
contract, then add a version-aware adapter or separately reviewed v2 predicate
over genuinely shared structural checks. No optimization policy enters common/com.

The gatekeeper checks both the physical/native tree's allowed shape and every
managed join: descriptor version/schema, three direct operands in order,
assignment/result TY and producer/result/value links in the active PU. It must
not infer noalias, finite ranges, target support or producer equivalence.

## 7. Canonical WHIRL, Identity And Compatibility

Canonical inspection form, schematic rather than a literal ASCII grammar:

```text
STID Y_value : Tensor<float32,[R,N]>
  OPR_DSLMEANVARNORM version=2 attributes=<exact fifteen-field schema>
    LDID X_value     : Tensor<float32,[R,N]>
    LDID gamma_value : Tensor<float32,[N]>
    LDID beta_value  : Tensor<float32,[N]>
```

Use the existing typed ternary carrier and normal tensor result conventions;
do not invent new rtype/desc encodings. Numeric flags, child count and mapped
layout remain existing contracts. Source file/line/column are diagnostic
positions on defining statements/result symbols, not semantic attributes.
Producers provide real positions; no fabricated Python location for a C++ fixture.

Append v2 after current logical seeds and use exact-version runtime registration
without reordering v1/common seeds. Logical ID 39 remains stable. Runtime lookup
IDs are process-local and must not be hard-coded as semantic versions or image
identity; regression-test existing image interpretation through the established
mapped descriptor validation path rather than assuming incidental runtime IDs
are globally stable across registration sequences.

PREOPT verifies v1/v2 separately before import and after emission. WOPT semantic
identity includes exact version, all canonical attributes and all three operand
descriptors. Reuse existing CODEREP identity and ternary emission; do not enlarge
the compact semantic-key structure or add a parallel simplifier. V1 and v2
never hash/equal as the same operation merely because their operand TYs match.
Changes to version, attributes, TYs, values, managed links or ownership invalidate
derived snapshots; recompute in the current driver-established scope.

Any current/latest-version accessor may describe the registry's newest entry;
it must not decide a source node's meaning. Schema-sensitive consumers use the
node's explicit version with existing exact-version accessors. Audit all such
consumers, not only the normalization-specific builder predicate.

Audit enum-only consumers. Current normalization derivation admits version 1
only, while its form walker counts this logical operator without version
dispatch. The future implementation must add explicit v2 derivation/trace
handling; it must not assert that every encountered normalization is v1 or
replace v1's unknown flags with v2's known arithmetic. PREOPT-disabled paths
still require structural/semantic verification at their normal boundaries.

Structural admission is required at every optimization level. Read-only
characterization follows the existing v1 O2/DSL-WOPT pipeline and PU scope;
it adds no profitability pass. At O0, unsupported executable lowering still
refuses v2 rather than substituting v1 or guessed scalar code. Later O3
candidate search remains separately gated in the execution design.

Use existing managed descriptor/node/attribute/value tables and the current
mapped-image writer/reader. No binary layout/version/ELF-section change is
proposed. Old v1 images must reopen unchanged. New images carry an actual v2
descriptor; a reader unaware of v2 must diagnose unsupported logical semantics,
not reinterpret it as v1. Current ASCII tools must print the exact logical
version/schema and preserve existing `ir_b2a -st -src` inspection obligations.

## 8. Numerical Facts, Trace And Required Tool Summaries

V2 derivation can report known LN-REF-1 order/identity/count/finalization,
sqrt/div sequence, environment and observational policy. It must still report
target conformance, real-error bounds, physical aliases/storage, ownership,
execution events, profitability and executable lowering as unproved/unplanned.
All FP32 input patterns remain admitted; no source finiteness/overflow proof
is invented. Mathematical binding is not target-instruction certification.

Proposed additive native inspection record, requiring implementation/auditor
review before use:

```text
AI_NORM_REF 1 BIND phase pu node x gamma beta result version rows width epsilon_bits count_bits reference mean_order mean_identity_bits mean_finalize count_conversion environment nan_policy rsqrt verified source_line
```

One positional header per invocation, source line last. The source version
must be 2, reference `ln_ref_1`, count bits derived from exact N, and fixed
tokens/schema must agree with the native node. This record reports verified
native meaning, not frontend proof or producer history. Retain current v1
`AI_NORM`/`AI_NORM_FACT` output byte/schema behavior; never insert bound fields
into their unresolved slots. Trace schema version 1 is distinct from semantic
operator version 2.

Use existing AI trace controls/mask and default-on option convention; no new
bit or optimization-enabling option. Trace-off performs identical verification.
Auditors require complete PREOPT entry/exit joins, exact source/schema/operand/
result identities and count/epsilon consistency, and reject fabricated target,
noalias, finiteness, cost-selection or transformation claims.

| Report | What it must summarize |
| --- | --- |
| `semantic.log` | Native name/version, fifteen-field binding, operand/result identities, N/count/epsilon bits, known arithmetic and remaining unknowns; retain v1 as unresolved. |
| Producer review summary | Actual source/producer/commands and explicit declaration path or refusal; separate evidence, not a WOPT `.t` provenance assertion. |
| `numerical.log` | Reference versus candidate bits/NaN classes when numerical tests exist; no new execution from structural admission. |
| `target-binding.log` | Not run/unbound until carriers/environment are implemented and checked. |
| `physical-plan.log` | Candidate selection/profitability/transformation not run for this contract milestone; later actual actions have separate records. |
| `optimization-summary.log` | Native-contract design versus actual implementation, admitted/refused versions, phase coverage and open target/storage/cost/lowering/GPU gates. |

Every generated log starts `# What it summarizes:`. `optimization.log` collects
phase detail, including rejection reasons, without treating verification events
as rewrites. Report default/on/off controls and zero rewrite count where no
executable transformation happened. No new trace file is fabricated here.

## 9. Implementation And Validation Queue

| Slice | Required work | Status |
| --- | --- | --- |
| LN1-b-design | Exact v2 identity/schema, source admission, preflight, canonical form, version/reader behavior and evidence ownership. | Specified here; no registration or code. |
| LN1-b-native | Common exact-version descriptor/schema/shape checks, explicit-result builder/gatekeeper and version-dispatched read-only derivation. | Open, independent of reduction profitability/lowering. |
| LN1-b-producer | Explicit native LN-REF-1 fixture and real formal/result/return/DST paths; source-preserving v1 negative lane. | Open; no general Python or v1 refinement added. |
| LN1-b-preservation | Actual PREOPT v1/v2 coexistence, semantic identity, mapped image/ASCII, controlled trace and strict audit. | Open; no numerical execution implied. |
| LN1-c / LN1-d | Target scalar carriers/environment plus independent arithmetic oracle. | Open as separately planned; not delivered by native admission. |
| LN1-e | Integrated explicitly bound evidence and version/source provenance. | Open; requires preceding native/target/oracle gates. |

Required future tests and retained evidence:

- Exact v1/v2 coexistence, same operands with different versions unequal,
  changed epsilon/binding unequal, registration-order and old-image replay.
- Native v2 shapes `3x17`, `1x1`, `7x65`, alternate/subnormal epsilon and
  descriptor-only count boundary N=2^24. No enormous tensor payload is needed
  merely to test a static dimension predicate.
- Reject each of fifteen field mutations, omissions, duplicates, extras,
  contradictions and malformed epsilon; refuse v1/v2 mixed schemas.
- Reject count zero/negative/dynamic/>2^24, rank/type/broadcast mismatches,
  wrong result, foreign/detached/null handles and non-PU scopes; prove output/
  managed-count/state preservation on invalid preflight.
- Reject version narrowing aliases, unsupported future versions and implicit
  latest-version fallback; verify current Python producer still emits exact v1.
- No class/metadata/flag/sample-based v1 promotion; imported valid v2 remains
  interpretable without an invented trusted-producer name.
- Preserve all direct/managed operand/result links through actual PREOPT,
  trace default/on/off and PREOPT-off structural/lowering refusal paths.
- Separate-process mapped `.B` reopen with `ir_b2a -st -src`, exact version/
  attributes/source positions, unknown-v2 rejection and no physical layout churn.
- Independent arithmetic tests remain distinct: structural admission alone
  executes no mu/variance/sqrt/affine computation and closes no accuracy gate.

Retain source snapshots, exact commands/binary hashes, `.B`, `.T`, raw `.t`,
reports and checksums under the existing immutable AI review convention. The
native-contract stage is not a WHIRL optimization rewrite: do not fabricate a
transformed image/diff. Later LN4 transformations require actual before/after
mapped images and raw diffs. Keep artifacts separate from reduction receipts.

## 10. Deliberate Non-Goals

No v1 migration, Python matcher change, approximate math, precision narrowing,
tree/Welford permission, kernel selection, TMA schedule, host PU, new runtime
provider or GPU performance claim. No source file from the separate reduction
coding effort is modified or required to define this contract. Native v2
implementation can be developed separately; shared emitter/lowering reuse is a
later reviewed integration, not a second implementation hidden in this design.
