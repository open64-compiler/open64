# Row Semantic Characterization And Numerical Acceptance

Version v0.1 | 2026-10-27 | R1 frontend analysis slice implemented

## 1. Scope And Ownership

Implement read-only characterization in this order: ordinary row reduction,
stable Softmax, affine mean/variance LayerNorm. Analyze **actual captured FX
edges, attributes and ShapeProp descriptors**, not model names or a supplied
cost ledger. This continues the [six-fixture ingestion work](../osprey/torch2whirl/BUILDING-BLOCK-INGESTION-PLAN.md)
and the [common execution-planning design](AI-COMPILER-COMMON-EXECUTION-PLANNING.md).
It is independent of MMA1; no compute-realization record, target profile,
hardware rate, native opcode, mapped-image format or compiler selector changes.

The [characterizer](../osprey/torch2whirl/python/open64_dsc/row_characterization.py)
is an experimental frontend analysis service, not a production P0/P2 pass or
an alternate executable IR. It calls no WHIRL builder and performs no graph
rewrite. The report describes source semantics and requirements for native
contracts. Canonical-WHIRL characterization remains open until ingestion and
PREOPT contracts are published and verified through mapped `.B`/`.T`.

## 2. Defined Terms And Recognition Boundary

| Term | Meaning |
| --- | --- |
| Row domain | A concrete rank-2 FP32 tensor with positive extents `R x N`. |
| Indices | `r` identifies a row; `k` is a bound statistics/reduction index; `j` is a distinct free output-column index. Domains are half-open `[0, extent)`. |
| State recipe | Identity, update/merge/finalize equations associated with an exactly matched source dataflow. These are real-arithmetic descriptions, not inferred FP equivalence proofs. |
| Numerical permission | Caller authorization for regrouping or state reformulation, separate from measured sample accuracy. Default is neither. |
| Numerical witness | A deterministic CPU FP32 execution compared with an independent scalar FP64 reference under declared error thresholds. Not a universal bound or GPU certificate. |
| Fingerprint | SHA256 of topology, targets, arguments/attributes, shapes, dtypes and strides, with node names excluded. Invalidates derived facts when these inputs change. |
| Native requirement | A semantic/type/effect/canonicalization obligation for the owning C++ infrastructure, not a new opcode declaration made by Python. |

Initial support is one complete pure graph, not arbitrary subgraph discovery.
Recognize exact function identities and shared node relationships; canonicalize
axis `-1`, `1`, or their singleton tuple/list form to last axis 1. Require
current ShapeProp FP32 metadata consistent with every inferred intermediate
and result shape. Obtain LayerNorm epsilon and affine sizes from the graph.
Renaming a class or FX node is not evidence for or against recognition.

Reject unsupported axes, keepdim/dtype options, layouts of the *logical
shapes*, inconsistent/missing metadata, empty extents, non-row ranks,
unmatched extra operations, in-place effects, extra/unused arguments and
multiple outputs. This is a deliberately bounded matcher, not a claim that
these rejected programs are invalid or unoptimizable. Noncontiguous input
strides are preserved in the fingerprint; no physical layout is selected.

All accesses are read-only, so inputs **may alias**. Do not claim disjointness
from separate formal names. Pure arithmetic outputs are fresh; no in-place
buffer assignment, physical placement or lifetime packing is performed.
These effect facts are scoped to the matched FX graph. FX capture alone does
not prove completeness for arbitrary Python-side state effects; native
ingestion must establish that boundary before an optimizer relies on purity.

## 3. Derived Characterizations

| Region | Matched source, domain and access | State and conditional alternatives |
| --- | --- | --- |
| Reduction | `sum(X, dim=last, keepdim=False)`; output `Y[r]`; accesses `X[r,k]`, `0 <= k < N`. | Identity 0, update/merge by addition, final scalar sum. Original source reduction retained; partition-and-merge requires explicit reassociation permission and numerical acceptance. |
| Stable Softmax | `m=amax(X,last,keepdim=True)`; `e=exp(X-m)`; `l=sum(e,last,keepdim=True)`; `Y=e/l`. The numerator and denominator must refer to the same `e`, and max/subtract to the same `X`. | Source max/exponential/sum state; retain exponentials or recompute them with the same reductions. Optional online `(n,m,l)` state needs reassociation and state-reformulation permission. Empty state is guarded before exponential evaluation, not computed as `exp(-inf-(-inf))`. |
| Affine LayerNorm | `mu=mean(X)`; `c=X-mu`; `var=mean(c*c)`; `Y=c*rsqrt(var+epsilon)*gamma+beta`. Both product operands must be the same centered node; gamma/beta have shape `[N]`; epsilon is finite and positive. | Population denominator `N`, not unbiased variance. Centered source algorithm retained; optional Welford `(n,mu,M2)` partial/merge requires both permissions. Finalize variance as `M2/N` and apply the original epsilon and affine terms. |

Normalization output is `Y[r,j]`; its logical finalization accesses use
`X[r,j]`, `gamma[j]`, `beta[j]`. These semantic access relations are not a
physical traffic count: retained intermediates may avoid rereads. State merge
rules and source node identities are included in the machine-readable report.

Shape facts are derived, but `finite_inputs`, valid intermediate numerical
domains and candidate error acceptance are **not proved** by ShapeProp.
Characterization records these as outstanding obligations. One observed
finite input never establishes a program-wide range or overflow guarantee.
Do not add a finite-input specialization to native code without proving its
conditions or providing guards and a semantics-preserving fallback.

## 4. Numerical Permission And Acceptance

`RowNumericalPermissions(reassociate=False, state_reformulation=False)` is a
test/analysis API, not a new compiler switch. It rejects non-boolean values.
Each alternative records required/missing permissions; permitted alternatives
still say `requires_numerical_acceptance` and `selected=False`. No result
changes source WHIRL or automatically authorizes an optimization.

The [CPU witness tool](../osprey/torch2whirl/python/tests/row_semantic_report.py)
uses real FP32 partial/merge arithmetic, FP32 online Softmax states and FP32
Welford partials/merge. It compares against the existing independent Python
FP64/`math.fsum` references, not against the same alternative implementation.
Exponentials are recomputed with the same source operations; no approximate
math, reciprocal substitution, narrowing or reassociation permission is
silently inferred for that strategy.

For each result element and for the aggregate vector:

```text
abs(actual_i - reference_i) <= atol + rtol*abs(reference_i)
norm(error) <= atol*sqrt(element_count) + rtol*norm(reference)
```

Current fixture thresholds are `rtol=2e-5`, `atol=2e-6`, explicitly a local
test policy rather than model-level accuracy criteria. Report maximum
absolute/scaled error, error/reference norms and a zero-safe relative norm
diagnostic. Shape mismatches, empty output, nonfinite results and invalid
thresholds fail explicitly. Bitwise equality, signed-zero, subnormal and
full nonfinite semantics are not certified by these metrics.

Twelve samples cover ordinary fixture values, zero/cancellation/overflow sums,
constant/large-range Softmax, constant/large-offset-small-variance/overflow
LayerNorm, and out-of-contract all-negative-infinity Softmax. Both strict
and explicitly permitted modes are recorded. Nonfinite inputs reject the
current numerical specialization before evaluating a candidate; this is not
a new definition of general PyTorch Softmax behavior.

The report deliberately retains failures of **source and alternative**
arithmetic against FP64. Source acceptance is not presumed. Additional
negative tests demonstrate why unshifted exponentials and
`E[x*x]-E[x]*E[x]` are not silently accepted replacements. Passing ordinary
samples neither excuses an adversarial failure nor proves all inputs safe.

## 5. Execution-Planning Obligations, Not Yet Implemented

| View | Facts now available / downstream obligation |
| --- | --- |
| Work Partition | Row/output and reduction domains, logical output ownership. Future partitions must cover contributions exactly and name the merge/final owner. No threads, CTA or distributed ownership selected. |
| Resource Lifetime | Read-only original inputs; logical partial/statistics/exponential state and affine broadcasts. Future packing must include every consumer/access lifetime; input aliases cannot be ignored. No byte allocation or reuse proof emitted here. |
| Execution Events | Statistics must precede normalization finalization. Partition writes must complete and become visible to merge readers; finalization waits for merged statistics. Cross-CTA merge needs an appropriate kernel/event protocol, not a CTA-only barrier. |
| Complete-plan gate | Numerical permission and accepted domain precede resource/performance comparison. No costs or profitability are fabricated by this R1 slice; R0's synthetic examples remain separate. |

The characterizer does not prove generic associativity, choose a merge tree,
generate guards, assign storage or lower executable events. R2-R4 own those
next steps, using canonical WHIRL and the common planning infrastructure.

## 6. Reports, Reproduction And Remaining Gates

```sh
# From osprey/torch2whirl with CPU torch installed:
PYTHONDONTWRITEBYTECODE=1 PYTHONPATH=python:python/tests python3 -m unittest -v test_row_semantic_capture_optional
PYTHONDONTWRITEBYTECODE=1 PYTHONPATH=python:python/tests python3 python/tests/row_semantic_report.py --artifact-dir /absolute/fresh/run
```

Without explicit `--artifact-dir`, honor `OPEN64_DSL_TEST_ARTIFACT_DIR` and
write `row-semantics/`. Existing output directories are never overwritten.
Retain `report.json`, compact `semantic-summary.log`, each actual FX graph,
typed input values, FP64 expected/FP32 actual values, exact analysis/test
sources and SHA256 checksums. Nonfinite witnesses use explicit string tokens
in otherwise strict JSON. The report's producer-source hashes bind it to
the exact implementation and numerical tests. Captured input/output programs
remain the source; the analysis report is not executable IR.

`semantic-summary.log` begins `# What it summarizes:` and reports sample,
permission mode, alternative, acceptance and reason, with source file and
function-entry line last. This is a whole-function origin, not a fabricated
per-operator source position. This is **not** a WOPT
`.t` trace or compiler optimization report. When native P0/P2 is implemented,
`semantic.log` must summarize matched region/domain/access/state facts,
outstanding range/effect conditions, permission and native-ingestion gaps;
later `physical-plan.log` must distinguish conditional alternatives from
selected plans and actual WHIRL transformations. No `.B/.T/.t/.cu` is fabricated.

R1 remains partial: publish/reuse native sum/max/mean and elementwise/broadcast
contracts, normalization-region boundaries and PREOPT canonical forms; certify
mapped `.B/.T` and derive equivalent facts from canonical WHIRL. General
subgraph recognition, dynamic/ragged domains, numerical-range proofs, automatic
alternative generation, physical execution planning and GPU validation remain
open. None is closed by this frontend analysis milestone.

## 7. Validation And Durable Evidence

CPU torch 2.4.1: 21 new characterization/numerical tests pass; the focused
set including prior six-fixture/oracle regressions passes 37 tests. The
86-test FX capture lane and frontend driver smoke pass. Full Docker
`python_test` runs 265 tests with 14 skips and the one unchanged FHE authority
DOCX digest failure; exact baseline file hashes are retained. Host-wide tests
also lack torch/numpy. No native builder, MMA or backend materializer changed,
so no native image or unrelated full FHE producer is rerun or certified.

The 12-case report contains 50 strict/permitted alternative records: 30
accepted sample evaluations, 11 permission rejections and 9 numerical
rejections. The invalid-domain Softmax case yields two further domain
rejections rather than evaluating candidate arithmetic. These are expected
test outcomes, not nine test failures or production optimization decisions.

Accepted immutable source-analysis evidence (primary `report.json`):

```text
/Users/shinmingliu/open64/artifacts/review/ai-optimization/research/r1-row-semantics/sha256-05c66ed3104c1816cd4b30d8a038b66a2d935fc699c757356a1e1b14a84cdc3f/
```

Keep this receipt separate from the earlier six-fixture capture receipt and
R0 synthetic planning demonstration. Replaying retained source checks the
semantic/numerical report and exact per-case graph/input/result evidence,
not native execution or target performance.
