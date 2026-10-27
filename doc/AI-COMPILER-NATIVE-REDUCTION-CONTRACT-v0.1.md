# Native reduction contract and PREOPT canonical form v0.1

Status: bounded R1 native contract and exact Python row-sum ingestion implemented;
executable reduction lowering is
not implemented by this milestone. Independent of MMA1 and target compute
realization selection.

## Architectural baseline

The [WHIRL specification](https://www.mcs.anl.gov/OpenAD/open64A.pdf)
(August 3, 2007) supplies the tree, typed-expression, high-level control and
continuous-lowering rules. This extension preserves direct operand kids,
typed results, PU ownership and the existing mapped-image framework. Historical
physical structure sizes are not imported from that specification.

## Normative contract

| Property | v1 requirement |
| --- | --- |
| Common operation | `common.reduce_sum`, logical `OPR_DSLREDUCESUM.v1` |
| Physical expression | Existing `OPR_DSL`, one direct typed operand kid |
| Result | `STID` of the expression into a managed typed result symbol |
| Input | Static positive rank-2 FP32 tensor `[R,N]` |
| Output | Static rank-1 FP32 tensor `[R]` |
| Exact attributes | `attr.axis=1`, `attr.keepdim=false`, `attr.accum_dtype=float32`, `attr.reassociate=false` |
| Effects | Pure read of the input; no mutation or allocation semantics |
| Ownership | Input, result and managed node belong to the active PU |
| Phase scope | Whole active PU for the initial PREOPT integration |

The explicit-result builder is required because input and output ranks differ.
Negative axis spelling must be normalized by the producer before construction;
it is not a second native canonical form. Missing, duplicate, extra or unsupported
attributes, versions and tensor types must be rejected, not downgraded to a
legacy marker. This v1 contract does not cover empty axes, dynamic dimensions,
rank changes other than 2-to-1, keepdim, dtype conversion or cross-PU operands.

`reassociate=false` is a prohibition, not a chosen tree reduction. It preserves
the source numerical contract and does not grant permission for tree, chunked,
atomic or parallel accumulation. A later executable realization must specify
and verify its accumulation order and numerical acceptance before selection.
FP32 storage/accumulation alone does not establish bitwise PyTorch parity.

## Ownership and PREOPT

Common compiler substrate owns the opcode schema, type/shape validation,
managed identity and mapped-image records. These records carry no profitability
policy. R1 characterization owns the source-operation match and numerical
permissions; executable realization planning remains a later optimization gate.

PREOPT verifies the canonical row-sum form on entry and exit. WOPT imports it as
an opaque pure unary expression, retaining typed descriptors, exact attributes,
effect identity and managed origin. Emission updates both direct kids and
managed operand links. It must not apply scalar algebraic reassociation,
constant folding or a reduction schedule to this contract. This first slice
verifies/preserves a canonical form; it does not claim general reduction
canonicalization or executable code generation. The subsequent Python
connection is specified below; it does not broaden the native v1 contract.

A reduction inside a separately supplied REGION is explicitly unsupported in
this first slice. Verification never expands the optimization scope or switches
the active PU. Non-reduction compilations are a no-op for this check.

## WHIRL levels, compatibility and lowering

The operation is admitted in very-high/high WHIRL through PREOPT. It is not
admitted to low-level execution without a selected implementation. The existing
`MARKER_ONLY` lowering-model enumeration means that no executable lowering route
is registered; the physical node remains native `OPR_DSL`, not a marker call.
Downstream VHO must diagnose the missing route. This milestone adds no CUDA,
runtime call or independent code-emission template.

Logical ID 37 is appended; existing logical IDs and registry ordering are
preserved. Physical opcode encodings, kid layout, type kinds, ELF sections and
mapped-image record layouts are unchanged. Current readers can inspect the
new logical name with ordinary `ir_b2a -st -src`; old binaries are not promised
semantic understanding of a newly introduced logical operation.

## Trace and review artifacts

The existing AI trace option/mask controls emission. Raw `.t` files retain the
positional schema once per verifier invocation:

```text
AI_REDUCE 1 HEADER phase node value rows width axis keepdim accum_dtype reassociate status source_line
AI_REDUCE 1 FORM preopt_enter 1 2 3 17 1 0 float32 0 verified 123
AI_REDUCE 1 FORM preopt_exit 1 2 3 17 1 0 float32 0 verified 123
```

IDs and source lines above are illustrative. These are verification events,
not candidate-selection, profitability or transformation events. The reporting
tool must summarize contract count, shapes, source lines, entry/exit agreement,
trace-disabled behavior and downstream lowering status. It must report that
no executable reduction transformation occurred. Derived `semantic.log` and
`optimization-summary.log` begin with `# What it summarizes:`.

Preserve the native producer `.B`, separate-process `.T`, raw `.t`, exact
commands, negative checks, report output, sources and checksums in a new immutable
receipt under `artifacts/review/ai-optimization/research/r1-native-reduction/`.
Use separate directories for ASCII `.T` and trace `.t` on case-insensitive hosts.

## Remaining gates

1. Completed: connect exact Python row-sum ingestion to this contract; see below.
2. Generalize canonicalization only with separately specified shapes/axes and
   numerical permissions; do not silently broaden v1.
3. Compare ordered SIMT and permitted parallel realizations with complete
   storage/event constraints, numerical tests and profitability evidence.
4. Lower the selected executable plan into WHIRL; then emit device code from it.
5. Extend stable Softmax and mean/variance normalization independently of MMA1.

## Reproduction and validation

With the configured Linux build products:

```sh
make -C "$BUILD/osprey/targdir/ir_tools" -j6 dsl_reduce_sum_contract_test ir_b2a
make -C "$BUILD/osprey/targdir/wopt" -j6
python3 osprey/be/opt/tests/run_native_reduction_review.py \
  --build-dir "$BUILD/osprey/targdir" --output-dir /absolute/new/review-directory
python3 -m unittest discover -s osprey/be/opt/tests -p test_reduction_trace_report.py -v
```

The native runner checks 23 contract/bridge/image assertions plus a separate
cross-PU rejection. Actual backend runs cover explicit trace-on, default trace,
trace-off and PREOPT-off. The successful result is PREOPT preservation followed
by the explicitly expected unsupported executable-lowering diagnostic, not a
successful compilation to an object. Incomplete backend mapped outputs are
retained under unmistakable names; only the finalized producer `.B` is a valid
native image. Separate-process `ir_b2a -st -src` verifies its operator,
descriptors, attributes and source positions. Two Python audit tests include
missing/malformed/changed trace negatives.

Focused regressions pass: common opcode/canonicalization contracts, WOPT
semantic-info, algebraic canonicalization/simplification, tensor constants and
interner, CKKS opcode and state/effect checks, artifact-mode static/symbolic
shape, runtime-interface and standard/group lowering, and the real WOPT driver.
The WOPT regression now examines the post-pass section rather than rejecting
an unchanged operator merely because it appeared in a pre-pass dump. Rebuilt
`be.so`, `wopt.so` and `lw_inline` contain no builder or JSON dependency.

Known harness limitations are not hidden: the combined builder driver hits
`Illegal attempt to start DST file-scope twice`; the production-native and
multi-PU legacy fixtures fail mapped finalization. An isolated rebuild with
all six pre-change common source/header files reproduces the same three
diagnostics. Unparameterized artifact-required invocations are not passing
coverage; their properly configured equivalents are retained separately.
These limitations do not close general native/FHE certification gates.

Durable receipt (raw trace, reports, source snapshots, commands, baseline
comparison and checksums):

```text
/Users/shinmingliu/open64/artifacts/review/ai-optimization/research/r1-native-reduction/
  sha256-800075e3c5c65388799998947cc6730fb1dae885e2fef92f3b258a72286d6c04/
    native-final/reduction.B
    native-final/ascii/reduction.T
    native-final/trace/preopt-on.t
    native-final/reports/semantic.log
    native-final/reports/optimization-summary.log
```

This native-contract-only receipt does not certify source Python mapping,
executable reduction, MMA target profiles, GPU correctness or performance.

## Python Ingestion Increment (2026-10-27)

`row_sum_ingestion.py` connects the existing exact row matcher to the published
C++ explicit-result builder. Recognition is by callable identity and graph
edges, not model name, a string `sum` match, or a numerical sample result.

| Boundary | Implemented behavior |
| --- | --- |
| Source graph | Exactly one placeholder, direct `torch.sum`, and tensor output. Embedded sums, extra effects/operations, method-form sum and multiple outputs remain outside this slice. |
| Input | Plain contiguous FP32 tensor with static positive rank-2 shape. Example data supplies shape/dtype, not a constant replacement for runtime input. Strided, empty, dynamic and other-dtype inputs remain unsupported. |
| Source options | Positional or keyword last axis, `-1` or `1`, including singleton tuple/list spelling; keepdim false and no dtype override. Unsupported/duplicate attributes fail the shared matcher. |
| Native options | Exactly `axis=1`, `keepdim=false`, `accum_dtype=float32`, `reassociate=false`. No numerical permission is inferred from test agreement or finite-looking inputs. |
| Input/output ownership | Runtime tensor formal `[R,N]`, explicit result descriptor `[R]`, typed native reduction, declared PU result and return. No `model_input` operation or sample tensor constant in this path. |
| Source provenance | The traced Python sum call-site is required and preserved on the native assignment. Generic class-level position binding must not overwrite an FX result's call-site. |
| Common ownership | No new opcode, native builder API, physical/mapped layout or backend dependency. Python mirrors the already published schema; common/com verifies the actual image. |

The ingestion sequence is deliberately bounded:

```text
capture FX once
if graph is not the exact three-node sum family:
    retain existing frontend handling (no general sum mapping)
validate graph edges and static attributes with the shared R1 matcher
validate positive contiguous rank-2 FP32 example descriptor
run ShapeProp only on that already-validated pure graph
derive strict R1 characterization and verify the result shape
require the sum's inspectable source call-site
declare runtime input formal and rank-1 result descriptor
create common.reduce_sum.v1 through the explicit-result builder
bind source position; append typed result and PU return
run Python and native verification; atomically finalize .B
```

Shape propagation evaluates only this pure source graph. It neither executes
compiled code nor proves accuracy, finiteness or reassociation permission.
Python analysis remains a source-language service; PREOPT still consumes native
WHIRL without a Python dependency and preserves the strict reduction boundary.
The high-level verifier checks exact attributes and the recorded result type,
so mock/manifest review cannot silently claim the input rank as the result rank.

### Reproduction

```sh
"$BUILD/osprey/targdir/torch2whirl/torch2whirl" \
  osprey/torch2whirl/python/tests/models/building_blocks.py \
  --model-factory RowReduction --sample-input shape:3,17 \
  --backend native -o /absolute/new/row_sum.B
"$BUILD/osprey/targdir/ir_tools/ir_b2a" -st -src \
  /absolute/new/row_sum.B /absolute/new/ascii/row_sum.T
python3 osprey/torch2whirl/python/tests/row_sum_native_review.py \
  --build-dir "$BUILD/osprey/targdir" --artifact-dir /absolute/new/review
```

Use the configured native Python package on `PYTHONPATH`. The artifact directory
may alternatively be supplied by `OPEN64_DSL_TEST_ARTIFACT_DIR`; it must be fresh.
The native review retains four separate-process Python producers (negative and
positive source axis, unit dimensions, and a wider 7x65 case), `.B`, separate
ASCII `.T`, raw `.t`, manifests, exact source lines, source-only FP64 witnesses,
strict reports, and expected unsupported-lowering diagnostics. It also inspects
a native image produced through the standalone C++ driver. FP64, wrong axis,
keepdim and composed-graph negatives publish no image. Incomplete backend
mapped outputs are labeled as such and are not certified images.

`AI_REDUCE 1` and the existing trace control are unchanged. Per-case
`semantic.log` describes canonical dimensions/attributes and PREOPT agreement;
the root `optimization.log` collects successful semantic reports, and
`optimization-summary.log` reports native ingestion, source-line agreement,
trace/preopt off behavior, rejection and remaining gates. Every log begins with
`# What it summarizes:`. No cost, selected plan or executable transformation is
reported by this slice.

Validation: five new source-ingestion tests with contract/result drift and
unsupported-form subcases; 91 torch capture tests and driver smoke; thirteen
existing native API tests in fresh processes and native artifact smoke. The
full source-Python suite runs 270 tests with 14 native/optional skips and the
unchanged FHE authority-document digest failure. The aggregate native API
invocation also retains the known DST-reset assertion; fresh-process native
tests avoid that lifecycle limitation, not a reduction-semantic failure.

Current receipt family:

```text
/Users/shinmingliu/open64/artifacts/review/ai-optimization/research/r1-python-row-sum/
  sha256-37b7da81faffba9da8c8d6b82474df00259fe37a12178261514e6f928ba27e51/
    native-complete/optimization-summary.log
    native-complete/negative-axis/row_sum.B
    native-complete/negative-axis/ascii/row_sum.T
    native-complete/negative-axis/trace/default.t
    native-complete/driver/row_sum.B
```

R1 remains partial: full state/access characterization from canonical WHIRL,
native Softmax/LayerNorm forms, executable realization selection/lowering and
GPU certification remain open. MMA1 sources, profiles and compute plans are
untouched by this ingestion increment.
