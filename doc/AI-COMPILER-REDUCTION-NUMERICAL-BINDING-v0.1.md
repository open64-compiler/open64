# Reduction Numerical Binding And Legal Alternatives

Version v0.1 | 2026-10-27 | Bounded R2 native contract and legality-query implementation

## 1. Purpose And Status

Continue R1's native row-sum characterization into explicit numerical binding
and executable-recipe legality, independently of MMA1. This increment implements
a native ordered source contract, read-only alternative discovery and independent
host witnesses. It does **not** implement device reduction lowering, target
arithmetic certification, complete profitability or production plan selection.

The existing exact Python `torch.sum` matcher continues to emit v1. No option,
sample result, class name or metadata hint silently changes its unresolved
source order into an ordered loop or a parallel tree.

## 2. Terms

| Term | Definition |
| --- | --- |
| Numerical binding | Explicit source meaning fixing the arithmetic sequence and observable floating-point behavior, not a fast-math permission inferred from samples. |
| Ordered recurrence | One state initialized to positive binary32 zero; consume row elements in ascending index order, rounding each addition to FP32. |
| RNE | Round to nearest, ties to even. |
| Gradual underflow | Preserve binary32 subnormal operands/results, rather than treating or flushing them to zero. |
| No-trap environment | Arithmetic exceptions do not trap and floating-point exception flags are not observable to this contract. |
| NaN class equivalence | NaN remains NaN; payload, sign, signaling status and exception flags are not specified. Finite values, infinities and signed zeros require exact result bits. |
| Direct ordered recipe | One row owner reads each element and updates the same accumulator immediately. |
| Staged ordered recipe | Copy a bounded consecutive chunk into fresh scratch, then consume its elements in ascending order using the same accumulator across all chunks. |
| Scratch budget | Explicit logical capacity supplied to the query per active row. It is not an SMEM allocation or a hardware occupancy estimate. |
| Legal recipe | Preserves the bound numerical recurrence, subject to stated storage/event/target obligations. It is not yet a fully certified executable device plan. |
| Target binding | Proof that the selected instructions, compilation controls and execution environment implement the required rounding/subnormal/exception behavior. |

## 3. Native Source Contracts

`common.reduce_sum.v1` remains unchanged: positive static FP32 `X[R,N]` to
FP32 `Y[R]`, axis 1, keepdim false, FP32 accumulation and reassociation false.
Source accumulation order, environment and exceptional behavior are unresolved.
Both proposed ordered recipes are therefore **source-binding blocked** for v1.

Append `common.reduce_sum.v2` to the existing logical descriptor registry, using
logical operator ID 37 and the existing unary physical `OPR_DSL` carrier.
V2 has exactly these eight attributes:

| Attribute | Exact value |
| --- | --- |
| `attr.axis` | `1` |
| `attr.keepdim` | `false` |
| `attr.accum_dtype` | `float32` |
| `attr.reassociate` | `false` |
| `attr.order` | `ascending_left_fold` |
| `attr.identity_bits` | `00000000` |
| `attr.fp_environment` | `rne_gradual_no_traps` |
| `attr.nan_policy` | `class_only` |

Normative semantics for each row:

```text
state = binary32(+0)
for k = 0 .. N-1:
    state = round_binary32_RNE(state + X[r,k])  // gradual subnormals
Y[r] = state
```

Every addition, including positive zero plus the first element, is required.
A width-one reduction of negative zero is not an identity copy. Partitioning
the input and summing partials changes this sequence and remains forbidden.
Independent rows may be assigned to different owners, but that does not
authorize parallel accumulation within a row.

V2 is an explicit source/DSL contract, not a claim of bitwise PyTorch parity
and not a relaxation of v1. A producer may use it only when its source semantics
actually provide this contract. It currently has a native fixture producer;
the Python framework-sum frontend is deliberately not upgraded.

Subsequent direct-path increment: [target/storage/event binding](AI-COMPILER-REDUCTION-DIRECT-BINDING-v0.1.md)
now validates a conditional scalar ISA, symbolic range/lease, owner and host-event
contract against this v2 source. Runtime enforcement and selected executable
lowering remain separate; the original recipe query below still reports its
own target-unbound status rather than fabricating a later binding result.

The WHIRL architecture baseline remains the existing high-level typed
expression, direct-kid, active-PU and continuous-lowering model described in
[WHIRL sections 1.3, 1.4, 1.15 and 1.18](https://www.mcs.anl.gov/OpenAD/open64A.pdf).
No physical WN/opcode layout, TY kind, mapped-image row, ELF format or binary
version changes. Existing logical operators and meanings are retained.

## 4. Ownership And Gates

Common Compiler Substrate owns versioned descriptors, exact attribute/type
predicates, native construction, managed identity and gatekeeper verification.
It contains no cost or candidate-selection policy.

WOPT owns transient characterization and
`WOPT_DSL_query_reduction_alternatives`. The query revalidates canonical native
WHIRL and whole active-PU membership. It derives a fresh snapshot rather than
trusting a supplied source report or a stale plan. Invalid scope, nonmember,
null output, zero chunk and overflow fail without publishing output or mutating
IR. A chunk larger than N is clipped to N before footprint calculation.
Requery after any WN, descriptor, attribute or managed-identity change.

PREOPT preserves the numerical source binding before import and after emission;
it does not choose a recipe or target. Version and all exact attributes
participate in the existing WOPT semantic identity. No scalar simplifier
replacement or separate numerical optimization engine is added.

The alternative query is presently a native planning service, exercised by
the focused producer, not an automatically enabled optimizing pass. Production
integration belongs at `-O2` or above, whole active PU only, under the existing
DSL optimization controls once the remaining target/storage/cost gates exist.
Canonical verification remains separate from that integration. There is no new
driver option authorizing source-order changes, no IPA enlargement and no MMA1
profile dependency.

## 5. Two Order-Preserving Realizations

| Recipe | Numerical legality | Resource and event obligations |
| --- | --- | --- |
| Direct ordered | Legal for v2, blocked for v1. | One FP32 state per active row; exactly N additions. Read X in ascending consumption order and publish Y once. Fresh/disjoint output or a proved safe alias plan; input stability and ownership must be established. |
| Staged ordered | Legal for v2 if scratch capacity is sufficient; blocked for v1. | `C=min(requested_chunk,N)`, scratch `4*C` bytes per active row, one unchanged accumulator. Every chunk copy completes before consumption; consume ascending indices; release/reuse scratch only after the last read. Tail copies/reads are in bounds. |
| Partition-and-merge tree | Forbidden for both versions under reassociation false. | Do not repair legality with a low cost, sample agreement, atomics or a numerical tolerance. A separately reviewed source/equivalence contract would be required. |

Scratch excludes accumulator storage, output storage, address/control state,
alignment, padding, synchronization objects and target resource granularity.
It is not multiplied by R without a selected concurrency/residency plan.
`budget_ok` proves only this logical byte inequality, not SMEM capacity,
alias freedom, synchronization correctness or occupancy.

Direct recipe:

```text
for each row r assigned to its unique owner:
    state = +0_f32
    for k in [0,N):
        state = add_f32_rne_gradual(state, load_X(r,k))
    store_Y(r,state)
```

Staged recipe:

```text
require C > 0 and 4*min(C,N) <= scratch_budget_per_active_row
for each row r assigned to its unique owner:
    acquire fresh scratch slot with capacity min(C,N)
    state = +0_f32
    for base = 0; base < N; base += C:
        count = min(C,N-base)
        copy_bits(X[r,base:base+count], scratch[0:count])
        require copy-complete and visible to the consuming owner
        for j in [0,count):
            state = add_f32_rne_gradual(state, scratch[j])
        require last-consumer-complete before overwrite/reuse
    store_Y(r,state)
    release scratch
```

A synchronous same-owner copy needs no inter-thread barrier. A cooperative or
asynchronous implementation must prove its corresponding visibility, ownership
and progress protocol. This increment selects neither a CUDA barrier nor TMA.
Bit copies must not narrow, canonicalize NaNs, flush subnormals or perform
arithmetic. Staging does not introduce partial sums or reset state at boundaries.

## 6. Target Binding, Accuracy And Profitability

The instruction contract must prove FP32 per-add rounding, RNE, gradual
underflow and the stated no-trap/NaN policy. Compiler settings, instruction
forms and lowering must prevent reassociation, tree reduction, hidden widening
or FTZ/DAZ substitutions. No existing Hopper/Blackwell/MMA profile is assumed
to prove these conditions. Both alternatives remain `target=unbound` here.

Numerical equivalence to the explicit recurrence is different from accuracy
against a real-arithmetic sum. Cancellation can cause large relative error;
overflow may legitimately produce infinity; NaN class propagation is required.
Finiteness, overflow freedom and general real-sum accuracy remain unproved.
An application requiring a tolerance relative to an independent reference needs
an additional numerical acceptance contract. Samples never grant source
permission or universal accuracy.

Later complete comparison must include N dependent additions per row, load
delivery/readiness, staged copy/store/read costs, scratch capacity and active
row count, memory coalescing, synchronization/launch costs and overlap.
Staging may improve delivery or coalescing, but it does not shorten the
within-row accumulator dependency chain. It can lose by adding traffic or
reducing occupancy. Do not select it based only on fewer modeled bubbles.
Preserve a legal direct fallback; if the source or target binding is missing,
fail closed or retain a separately bound source provider, not a guessed loop.

## 7. Trace And Tool Summary

Existing `-DSL:wopt=on`, `-DSL:ai_opt_trace=on|off` (default on), `-tr25`
and the AI TP_WOPT2 mask are reused. Actual PREOPT verification emits the
additive `AI_REDUCE_NUM 1 BIND` schema once per boundary invocation, with source
line last. V1's prior `AI_REDUCE 1 FORM` and `AI_REDUCE_FACT 1 FACT` rows remain
unchanged; v2 uses the new numerical schema rather than falsely claiming v1's
unresolved order. Trace-off does not disable structural verification.

The explicit native query emits `AI_REDUCE_ALT 1 ALT` rows for direct, staged
and forbidden tree recipes. Its caller supplies an optional trace stream; the
focused test opens one explicitly and checks identical output with tracing off.
This is not yet evidence that a production cost-selection pass ran.

Headers are positional and published in `reduction_binding_trace_report.py`:

```text
AI_REDUCE_NUM: phase pu node input result version rows width order identity_bits environment nan_policy binding source_line
AI_REDUCE_ALT: phase node candidate chunk scratch_bytes budget_bytes numerical storage target selected reason source_line
```

Required tool summaries:

| File | What it summarizes |
| --- | --- |
| `semantic.log` | Source version/order/identity/environment/NaN policy, actual PREOPT entry/exit agreement, numerical prohibitions and unknowns. |
| `alternatives.log` | Complete recipe set, chunk/byte budget, numerical legality, capacity rejection and explicit reason; no invented target or selection. |
| `numerical.log` | Host candidate/oracle agreement, exact inputs/results, exceptional/signed-zero/subnormal checks and illegal-tree counterexample; not native execution. |
| `optimization.log` | Full collection of successful semantic/alternative reports, preserving case/phase identity. |
| `optimization-summary.log` | Coverage, source-binding/capacity/permission failures and remaining target, alias/storage/event, cost, selection and transformation gates. |

Every derived log starts with `# What it summarizes:`. Strict auditing rejects
missing/duplicate alternatives, changed schemas/bindings, mismatched PREOPT
pairs, false byte accounting, invented target certification and selected-plan
claims. All current reports state
`candidate_selection=not_run profitability=not_run transformation=none`.
A verification event is not a transformation.

## 8. Evidence And Reproduction

Five native cases: ordered `3x17`, one-byte-insufficient scratch budget,
unbound source v1, width-one with clipped chunk, and width-65 with a tail.
Retain each `.B`, separate-process `ir_b2a -st -src` `.T`, native query and
actual PREOPT raw `.t`, default/on/off/preopt-off controls, native assertion
output, strict reports, exact commands and source snapshots. Incomplete backend
outputs are labeled as incomplete; VHO still diagnoses the missing executable
lowering route. No selected WHIRL transformation occurs, so no fabricated
before/after transformed images are supplied.

Host witnesses execute direct and seven staged chunk configurations on 13
adversarial cases (104 recipe checks). An independent integer oracle adds
binary32 operands exactly in units of 2^-149, then performs explicit nearest-even
rounding. A deterministic 2,048-pair check compares host addition to this oracle.
The cancellation case `[1e20,1,-1e20,1]` demonstrates why chunk-local partial
sums cannot replace one ordered state. Signed zero, subnormals, normal/subnormal
boundaries, rounding ties, overflow, infinity and NaN classes are retained.
This certifies host recipe witnesses only, not generated native/device code.

```sh
make -C "$BUILD/osprey/targdir/ir_tools" -j6 dsl_reduction_binding_test ir_b2a
make -C "$BUILD/osprey/targdir/wopt" -j6
python3 osprey/be/opt/tests/run_reduction_binding_review.py \
  --build-dir "$BUILD/osprey/targdir" --output-dir /absolute/new/reduction-binding
python3 -m unittest discover -s osprey/be/opt/tests \
  -p 'test_*trace_report.py' -v
```

Use the isolated R1/R2 build, not MMA1's shared build. Review receipts use a
fresh immutable `artifacts/review/ai-optimization/research/r2-reduction-binding/`
directory with SHA256 manifest, no-replace promotion and final-path mapped/report
replay before index updates. Raw `.t` and ASCII `.T` stay in separate directories.

## 9. Implementation Sequence

1. Completed in this bounded increment: explicit native v2 arithmetic contract,
   shared admission, version-preserving PREOPT characterization, native legal
   recipe query, scratch-budget gate, strict traces and host oracle witnesses.
2. Direct compiler-contract binding implemented in the linked detail document.
   Implement provider enforcement/guard wiring and bind staged storage/event
   constraints next; keep v1 unresolved unless a source-preserving provider
   or independently justified source binding is supplied.
3. Build complete target costs and compare direct vs staged under those gates;
   introduce no selection before cost and legality are complete.
4. Lower the selected plan into canonical high-level WHIRL loops, preserve
   before/after mapped images/diffs and emit code from WHIRL.
5. Add separately authorized parallel numerical contracts and alternatives;
   extend this discipline to stable Softmax and centered normalization.
6. Certify native numerical behavior, resource use and performance on available
   hardware. CPU witnesses do not close this gate.

R2 remains partial. No changes to MMA1, compute-realization profiles, GPU kernels,
global mapped-image formats or the existing Python v1 contract are implied.

## 10. Accepted Compiler-Side Evidence

All five native cases pass: four v2 variants execute 27 assertions each and the
unbound-v1 case executes 13 (121 assertion executions total). All query traces
and actual PREOPT numerical bindings pass strict audit. The combined trace suite
passes 26 tests, including seven new R2 audit/oracle tests and 19 existing row
auditor tests. Four normalization variants, native Softmax/reduction, five
authenticated historical Python/API/driver reduction images, WOPT semantic-info,
canonicalization, syntax/layout, common CKKS transaction, view retirement,
miniature checkpoint, real WOPT driver and 21 source numerical tests pass.
Rebuilt backend/WOPT/inline products contain no builder/JSON symbol dependency.

The impact selector conservatively lists whole-model FHE certification for
unclassified backend/common paths. Manual source impact review limits this
increment to an appended reduction version, v2-only binding admission, transient
PU-local queries and additive traces; no FHE operator meaning, materializer,
state transfer, transaction implementation or physical image format is changed.
Focused selected compatibility lanes and authenticated prior reduction images
are rerun. No full ResNet producer, full Python suite or GPU run is claimed.
An initial orchestration attempt preceded installation of the new review runner;
its missing-script diagnostic is retained separately from the successful run.

Immutable receipt root, primary `binding/ordered/reduction.B`:

```text
/Users/shinmingliu/open64/artifacts/review/ai-optimization/research/r2-reduction-binding/sha256-bcf98f047873fea7d05556eec4e0ebae981740986d7c196f68e68e3339f8f40b/
```

Start with `binding/optimization-summary.log`,
`binding/ordered/reports/alternatives.log`,
`binding/ordered/ascii/reduction.T`,
`binding/ordered/trace/query.t` and `binding/ordered/trace/default.t`.
The representative query records source node 1, input/result values 1/3,
`[3,17]` and source line 139; staged chunk 8 requires 32 logical scratch bytes.
The 31-byte-budget case rejects staging while preserving the direct numerical
recipe. These are fixture observations, not ABI constants or hardware estimates.
Five durable mapped reopens and five strict query/PREOPT report replays must
match before the review index is updated.
