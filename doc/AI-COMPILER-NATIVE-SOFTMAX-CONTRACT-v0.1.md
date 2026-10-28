# Native stable Softmax contract and canonical WHIRL characterization v0.1

Status: bounded R1 native contract, exact Python ingestion and read-only PREOPT
characterization implemented. Independent of MMA1. Executable Softmax lowering
and GPU validation remain open.

## Terms and architectural baseline

| Term | Definition |
| --- | --- |
| Row | `X[r,:]` of a positive static rank-2 FP32 tensor `[R,N]`. |
| Stable source recipe | Row maximum, subtract maximum, FP32 exponential, FP32 sum, divide by that sum. Not a generic framework Softmax. |
| Logical state | A maximum `m[r]` followed by a sum `l[r]`; describes computation, not allocation. |
| Canonical form | Typed native unary `OPR_DSL` expression assigned by `STID`, with direct typed `LDID` operand and consistent managed records. |
| Characterization | Facts derived from that verified WHIRL, without source-graph metadata, candidate selection or transformation. |
| Unresolved source semantics | An obligation that must be bound before executable lowering; not permission to choose arbitrary semantics. |

The [WHIRL specification](https://www.mcs.anl.gov/OpenAD/open64A.pdf)
(August 3, 2007, sections 1.3, 1.4 and 1.18) supplies the high-level expression,
strict tree, typed-kid, continuous-lowering and ASCII inspection baseline. This
extension retains the existing mapped-image framework and native DSL carrier;
it imports no historical physical structure sizes.

## Normative v1 contract

| Property | Requirement |
| --- | --- |
| Operation | `common.stable_softmax.v1`, logical `OPR_DSLSTABLESOFTMAX = 38`. |
| Input/result | Static positive FP32 `[R,N] -> [R,N]`, one operand, same logical shape and compatible FP32 element representation. |
| Exact attributes | `attr.axis=1`, `attr.accum_dtype=float32`, `attr.reassociate=false`, `attr.state_reformulation=false`, `attr.exp_semantics=source_unresolved`. |
| Computation | `m=max(X[r,:]); E=exp_fp32(X-m); l=sum_fp32(E); Y=E/l`, with FP32 intermediate/result values. |
| Effects | Pure logical read of input and definition of a distinct managed result value; no physical allocation or input mutation semantics. |
| Scope | Whole driver-selected active PU; all operand/result/node joins belong to it. |
| Construction | Explicit-result builder only, with no-allocation schema/type/ownership preflight. |

Negative axis spelling is normalized by a producer, not admitted as another
native form. Empty axes, dynamic dimensions, other ranks/dtypes, mixed precision,
extra/missing/duplicate attributes, other versions and foreign-PU operands fail
closed. Input and result can use the same interned TY: their managed **value**
identities must still differ. No physical no-alias fact follows from this.

`common.softmax` remains unchanged. Its historical marker is not silently
upgraded to this more specific source recipe. In particular, an opaque
`torch.softmax` call is not evidence of the exact decomposed FP32 recipe.

## Numerical meaning and deferred obligations

The native contract preserves the existing R1 source-level numerical boundaries:

1. `reassociate=false` prohibits reassociation of the sum; it does not assert
   left-to-right accumulation or a particular source-framework reduction tree.
2. `state_reformulation=false` prohibits online/partitioned max-sum merge,
   even though a real-arithmetic `(m,l)` reformulation exists.
3. `exp_semantics=source_unresolved` records that no target exp implementation,
   approximation error, rounding or exceptional-value policy has been bound.
   It does not authorize a fast exponential or reciprocal approximation.
4. Finite inputs, subtraction/intermediate overflow safety and error acceptance
   are unproved. Finite FP32 inputs alone do not prove that `X-m` is finite.
   NaN, infinity and all-minus-infinity rows require explicit source/target
   semantics; do not replace their outputs by zeros or another repaired value.
5. Positive `N` makes the source maximum nonempty. Its identity is therefore a
   nonempty-row reduction, not an unconditional `m=-inf,l=0` online algorithm.
   The subsequent sum has zero identity. Do not evaluate empty-state exponentials.

For finite real arithmetic, shifted exponentials are at most one and the
denominator is positive. This mathematical observation is not a proof of the
entire FP32/target implementation. Existing CPU FP32 versus independent
FP64/math.fsum witnesses remain sample evidence, never compiler authorization.
Native structural verification is not native numerical execution.

## Ownership, scope and phase integration

Common Compiler Substrate owns stable IDs, exact schemas, policy-free shape/type
predicates and managed records in `common/com`. WOPT owns harvesting from the
active canonical tree, including transient `WOPT_DSL_SOFTMAX_CHARACTERIZATION`.
The FP32 row recipe is AI/tensor numerical meaning, not an MMA target profile.
Neither common code nor the source printer selects a realization.

PREOPT verifies the form before WOPT import and after emission, including
PREOPT variants used by LNO/IPA when their driver supplies a whole active PU.
An explicitly supplied REGION containing this operation is rejected in this
bounded increment; no service traverses other PUs or switches symbol tables.
No-Softmax trees are no-ops. This is preservation/verification of an already
canonical native form, not recognition of arbitrary scalar loops.

WOPT carries the operation as an opaque pure unary CODEREP identity. Exact
attributes, result/operand descriptors, effects and origin remain in its
semantic identity. Emission rewrites direct kids and managed operand links
together. No scalar fold or algebraic rewrite applies to the opaque recipe.
Facts are recomputed on demand and invalid after any relevant tree, TY,
attribute, ownership or managed-link change; there is no persisted fact cache.
Failure leaves output records and compiler tables unchanged.

This is a structural service, not a new optimization-level policy. Actual
integration is tested through the explicitly enabled DSL PREOPT path at `-O2`.
No profitability analysis or performance transformation is introduced at any
level. Lower levels still cannot execute an unbound recipe.

## State, accesses and future choices

The derived record identifies PU/node/input/result/TYs, `[R,N]`, source line,
recipe and explicit permission/unknown flags. It describes `R` semantic maxima
and `R` sums, not `2R` simultaneously live allocated scalars. `E[r,k]` retention
is an unresolved realization choice, not a required temporary tensor.

Logical relations are input read `X[r,k]` and output definition `Y[r,k]`, with
`r in [0,R)` and `k in [0,N)`. The output has a per-row ownership obligation;
no CTA/warp/thread ownership is assigned. HBM traffic, rereads, vector width,
shared-memory allocation, alias freedom and async event schedules are unknown.

The later planner may compare retaining exponentials with recomputing them
under the same reductions. Both require a bound exp/numerical contract and
complete storage/event/cost evaluation. Online max-sum is permission-rejected
under v1, not simply an expensive alternative. No alternative is selected here.

## Trace and tool contract

Existing controls are retained: backend `-DSL:wopt=on`,
`-DSL:ai_opt_trace=on|off` (default on), TP_WOPT2 AI mask `0x00800000`.
Frontend pass-through uses `-Wb,`; tests retain the established `-tr25` setup.
No trace bit is reallocated. Verification still runs when emission is off.

`AI_SOFTMAX 1 FORM` records canonical shape/permissions; additive
`AI_SOFTMAX_FACT 1 FACT` records native state/access/numerical facts. Each has
one positional header per verifier invocation and source line as its last
column. Existing `AI_REDUCE` schemas are unchanged. Header definitions in
`softmax_trace_report.py` are the executable column specification.

The auditor requires matching entry/exit pairs, positive identities/domains and
source lines, exact fixed numerical facts, and FACT-to-FORM joins. It rejects
partial/malformed traces and invented accuracy/noalias/storage claims. It must
summarize verified count, dimensions, recipe/states, logical accesses, ownership,
numerical prohibitions/unknowns and remaining gates. It must say
`candidate_selection=not_run profitability=not_run transformation=none`.

`semantic.log`, complete phase aggregate `optimization.log` and
`optimization-summary.log` start with `# What it summarizes:`. The report's
alternative names are explanatory source-recipe possibilities, not selected
compiler candidates. Raw `.t` is authoritative for what the compiler verified.

## Compatibility, lowering and validation

ID 38 is append-only; IDs 0-37 and historical `common.softmax` meaning remain
unchanged. No physical opcode, kid layout, TY kind, ELF section, binary version
or managed row size changes. Existing images reopen with current tools; old
readers are not promised understanding of this new logical operation.

Very-high/high WHIRL and PREOPT admit the native contract. Registered
`MARKER_ONLY` means no executable lowering route, **not** a legacy physical
marker representation. VHO deliberately diagnoses the missing executable route.
Partial `.O` output from that failed compilation is retained as
`.incomplete-mapped-output`, never advertised as a finalized selected image.

Reproduce with the isolated Linux build:

```sh
make -C "$BUILD/osprey/targdir/ir_tools" -j6 dsl_softmax_contract_test ir_b2a
make -C "$BUILD/osprey/targdir/wopt" -j6
python3 osprey/be/opt/tests/run_native_softmax_review.py \
  --build-dir "$BUILD/osprey/targdir" --output-dir /absolute/fresh/review
python3 -m unittest discover -s osprey/be/opt/tests -p 'test_*trace_report.py' -v
```

Retain `.B`, separate-process `ir_b2a -st -src` `.T`, raw `.t`, negative checks,
reports, numerical witnesses, exact command vectors, producer hashes and source
snapshots in a new immutable `r1-native-softmax` review receipt. ASCII and trace
directories are separate. No executable transformation is performed, so this
increment does not fabricate transformed before/after images.

The exact Python connection is described below. Next specify native
mean/variance normalization. Executable numerical
binding, legal alternatives, storage/events, profitability and WHIRL lowering
remain separate gates. MMA1, CUDA profiles and GPU validation remain untouched.

## Accepted evidence, 2026-10-27

Focused Linux/Docker validation passes: 41 native assertions (40 main plus
cross-PU rejection), actual PREOPT entry/exit, default/on/off traces, disabled
DSL PREOPT, `-O0`/`-O1` with DSL PREOPT disabled, mapped source-interleaved
reopen, twelve reduction/Softmax trace-auditor tests, 21 source/numerical CPU
tests, canonicalization/WOPT driver/semantic regressions, native syntax/layout,
CKKS expansion/rollback, view retirement and checkpoint publication. Five
authenticated earlier Python/driver row-sum images also replay with the new
reader/backend. Rebuilt be/WOPT/inline products retain their symbol boundaries.

The unchanged 12-case CPU report retains 50 strict/permitted alternative
evaluations and explicit numerical/domain/permission rejections. These are
source witnesses, not execution of the native Softmax node. The FHE impact
selector's conservative broad lanes were exercised with focused transactions;
the full ResNet producer is not rerun: no FHE operator, materializer, physical
opcode, mapped layout, native transaction or existing logical ID changed.

Immutable final-source review root (primary: `native/softmax.B`):

```text
/Users/shinmingliu/open64/artifacts/review/ai-optimization/research/r1-native-softmax/sha256-376d41028b2ca4b63341afe6b6a187517ab737551ca5cb0566ba3126feae7e7f/
```

Start with `native/reports/optimization-summary.log`,
`native/reports/semantic.log` and `native/ascii/softmax.T`.
The primary trace verifies `[3,17]`, node 1, input value 1 and result value 3,
shared tensor TY 13570, and fixture source line 191. These IDs are this
receipt's observations, not fixed ABI values. Seven durable mapped-image and
report replays cover Softmax, native row sum and five historical row-sum inputs.
The receipt retains commands, product/source hashes, negative evidence and
explicit remaining gates. No generated CUDA or selected executable image exists.

## Python ingestion increment, 2026-10-27

The frontend now consumes this existing v1 contract. No native opcode/schema,
mapped layout, target profile or MMA1 implementation changes are needed.

```text
capture with source positions
  -> exact whole-graph max/subtract/exp/sum/divide structural preflight
  -> positive contiguous rank-2 FP32 input descriptor check
  -> ShapeProp on the verified pure graph
  -> shared R1 characterization and exact shape/edge verification
  -> runtime input formal + explicit result/return + common.stable_softmax.v1
  -> native .B -> independent ir_b2a .T -> actual PREOPT -> audited .t reports
```

The analysis and ingestion paths share the same structural Softmax matcher.
The numerator must be the exact exponential value reduced by the denominator;
a second exponential, raw-input sum, unshifted exp, epsilon-added denominator,
extra effect/operation or unused input fails before ShapeProp. Only accepted
pure graphs are evaluated for shape propagation. This does not purport to
sandbox arbitrary Python execution during symbolic tracing itself.

Both reductions accept canonicalizable single last-axis spellings `-1`, `1`,
or a singleton tuple/list. Maximum and sum retain the reduced dimension.
Opaque `torch.softmax`, method-call approximations, alternate reductions and
embedded/compositional regions do not gain support by presumed equivalence.
Dynamic/empty shapes, non-FP32, non-contiguous tensors and multi-PU ingestion
remain outside this bounded connection. Model/class names do not select meaning.

The shared frontend row emitter reuses the existing reduction's native
formal/result/return construction, import/class census and source-position APIs.
It supplies descriptor requests and fixed published attributes; C++ still owns
WHIRL types/symbols/records. No sample tensor becomes a constant. The region's
native assignment/result symbol use the actual final-division source line,
not the Python class declaration. Source intermediate operations are subsumed
only by the exact published recipe, not by an independent target implementation.

No permission is inferred from ShapeProp or CPU sample agreement. Strict
reassociation/state-reformulation prohibitions and unresolved exp/order/exception,
finite/overflow/accuracy facts survive native PREOPT. Nonfinite sample inputs
do not become finite-domain proofs or silently acquire repaired outputs.

The native trace schema and bit are unchanged. A verification `.t` file alone
does not prove frontend provenance; the per-phase auditor now reports
`frontend_provenance=not_in_trace` instead of a hard-coded project-status claim.
The producer/review receipt and aggregate `optimization-summary.log` separately
verify Python ingestion. This report-only correction reuses raw traces;
initial derived reports are retained as history, not presented as current status.

Validation: eight focused source tests; five native image/PREOPT cases
(`3x17` with negative/positive axes, `1x1`, `7x65`, and the standalone driver);
ten native prepublication rejection cases; four FP32 source-versus-independent
FP64/math.fsum witnesses under existing tolerance; default/on/off trace checks;
thirteen isolated native API tests and mapped artifact smoke. Existing row-sum,
semantic/numerical, building-block, skeleton and twelve trace-auditor regressions
pass. Full source suite: 278 tests, fourteen skips, only the unchanged FHE
authority DOCX digest failure. No full FHE producer or GPU run is claimed.

Final immutable Python-ingestion receipt (primary `native/negative-axis/softmax.B`):

```text
/Users/shinmingliu/open64/artifacts/review/ai-optimization/research/r1-python-softmax/sha256-03c7edf8146bd09e9791a831c4bf918f95e79f1f9088a37304ddaf3f3923c76d/
```

Start with `native/optimization-summary.log`,
`native/negative-axis/reports/semantic.log`,
`native/negative-axis/ascii/softmax.T` and
`native/negative-axis/trace/default.t`. The representative trace preserves
node 1, input/result values 1/3, shared TY 13570 and Python division line 44.
Those IDs/line describe this receipt, not a fixed ABI. Five durable source-
interleaved mapped reopens and strict report replays must match staged evidence.

R1-d remains partial: native mean/variance normalization, generalized forms,
executable numerical binding, legal/profitable alternatives and lowering are
still separate work. Source ingestion is not an optimization or speedup claim.

The subsequent [native normalization contract](AI-COMPILER-NATIVE-NORMALIZATION-CONTRACT-v0.1.md)
now implements bounded centered affine population semantics and PREOPT
characterization. Its Python connection and executable realization are still
open; no Softmax numerical permission or native contract is broadened.
