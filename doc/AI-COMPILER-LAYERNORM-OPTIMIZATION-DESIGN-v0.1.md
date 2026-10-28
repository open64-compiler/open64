# LayerNorm Optimization Through Common Execution Planning

Version v0.1 | 2026-10-28 | Design proposal for review; no new executable support

Arithmetic amendment, 2026-10-28: the separate
[LN-REF-1 reference binding](AI-COMPILER-LAYERNORM-REFERENCE-ARITHMETIC-v0.1.md)
now defines both means, sqrt-then-divide reciprocal square root and the FP32
environment. This resolves the reference-design choice, not native v1's
unknowns or the implementation/target gates.

## 1. Decision, Scope And Evidence

Design LayerNorm now, concurrently with reduction profitability and executable
WHIRL lowering. Reuse reduction services rather than cloning their arithmetic
contracts, target lookup, planning, event checks or trace infrastructure.
Attention and MoE receive later semantic specializations of this framework;
they are not prerequisites for this bounded normalization design.

This document extends the
[common execution-planning framework](AI-COMPILER-COMMON-EXECUTION-PLANNING.md)
and consumes the existing
[native normalization contract](AI-COMPILER-NATIVE-NORMALIZATION-CONTRACT-v0.1.md).
Current code recognizes the exact centered affine Python recipe, preserves
`common.mean_variance_norm.v1` through actual PREOPT, and derives native semantic
facts. It does not bind executable mean/rsqrt arithmetic, select normalization
plans or lower an executable normalization kernel.

Initial scope: one canonical assignment in the active driver-selected PU;
contiguous positive static FP32 `X[R,N]`, `gamma[N]`, `beta[N]`, result `Y[R,N]`,
population centered variance and exact binary32 epsilon. Initially require
fresh physical output, immutable inputs during invocation and a single device.
Opaque framework `layer_norm`, multiple normalized axes, arbitrary strides,
in-place output, backward/training, mixed precision, distributed normalization
and cross-PU optimization require separate extensions.

This design-only increment allocates no opcode, mapped-image field, API, trace
bit or command-line option. It changes neither v1 numerical permissions nor
binary WHIRL compatibility. Pseudocode, plan labels and additional trace fields
below are proposals, not evidence of generated code.

## 2. Terms Before Use

| Term | Definition |
| --- | --- |
| Row / feature | `r` selects a row; `k` selects a feature. Domains are `[0,R)` and `[0,N)`. |
| Centered recipe | Mean, subtraction, mean of squared deviations, epsilon-add, reciprocal square root, normalization, scale, then bias in native source order. |
| Mean binding | Exact reduction identity/order/tree, count conversion, scaling operation and rounding points implementing a mean primitive. |
| Arithmetic binding | Mean bindings plus scalar operation, rsqrt, rounding, subnormal, NaN and observable exception policies; not a timing profile. |
| Numerical authorization | Explicit permission for a particular changed reduction/state/precision/approximation policy, independent of sample agreement and profitability. |
| Acceptance policy | Declared input domain and comparison criteria for an authorized realization, including exceptional values. Samples alone are not a universal proof. |
| Statistics state | Mean `mu`, centered variance `v` and inverse standard deviation `q`; logical values, not necessarily allocated arrays. |
| Work partition | Assignment of contributions and outputs to owners, followed by a provisional thread/warp/CTA mapping. |
| CTA | CUDA thread block; execution scope, not the compiler's PU analysis scope. |
| SIMT / MMA | Single-instruction, multiple-thread execution / matrix multiply-accumulate realization. Ordinary LayerNorm has no source contraction requiring MMA. |
| GMEM / HBM | Device global address space / off-chip backing-memory tier. A GMEM instruction can be serviced by caches rather than HBM. |
| SMEM / RF | CTA-shared scratch memory / register file. Both impose physical reservations; RF retention can spill to local memory. |
| Row buffer | Candidate-owned FP32 storage for one original or centered row, with explicit layout, alignment and access lifetime. |
| Retain / recompute | Keep a bit pattern until last use / reevaluate its exact bound operation on unchanged operands. Neither authorizes reassociation. |
| Logical / transaction traffic | Semantic load/store bytes / serviced bytes after active-lane mapping, vector width, alignment, masks, sectors and cache-path assumptions. |
| Access completion | Event proving an operation will no longer access a range; issue alone does not establish completion. |
| Visibility / release | Admit intended readers / complete every conflicting access before storage reuse. |
| Storage generation | One use of an allocated range, distinguished by iteration/epoch when a slot is reused. |
| Reservation | Target-rounded resource allocation charged to a resident CTA; logical death does not release part of it to another CTA. |
| Complete plan | Arithmetic/algorithm, partition, representation, storage, movement, events, guards, fallback and cost evidence considered together. |
| Ordered fallback | A fallback with explicitly bound statistics order and scalar semantics, not an assumed implementation of unresolved v1 means. |
| Welford state | Count `n`, running mean `m` and centered squared-deviation sum `M2`, with explicit update/merge/finalize rules. |
| TMA | NVIDIA Tensor Memory Accelerator bulk/bulk-tensor asynchronous movement; optional delivery, not a statistics algorithm or automatic speedup. |
| Cost evidence | Units, exact target/profile/revision, field provenance, uncertainty, assumptions and comparison boundary. |

## 3. Semantic Characterization And Numerical Gates

Canonical WHIRL preserves this recipe with FP32 intermediate values:

```text
mu[r] = mean_fp32(X[r,:])
d[r,k] = sub_fp32(X[r,k], mu[r])
v[r] = mean_fp32(mul_fp32(d[r,:], d[r,:]))
q[r] = rsqrt_fp32(add_fp32(v[r], epsilon))
u[r,k] = mul_fp32(d[r,k], q[r])
z[r,k] = mul_fp32(u[r,k], gamma[k])
Y[r,k] = add_fp32(z[r,k], beta[k])
```

The denominator is N, not N-1; epsilon stays inside rsqrt. The final multiply/
add is not contracted to FMA. Current attributes include `reassociate=false`,
`state_reformulation=false`, `contract=false` and
`rsqrt_semantics=source_unresolved`. Mean order and scaling remain unresolved.
An ordered-sum service alone does not bind a mean: sum-then-divide and scaling
each summand can round differently.

Before executable selection, the native/target implementation must establish
all of the following; LN-REF-1's numerical choices are now explicitly defined
in the separate reference specification:

1. Both mean identities, update/order or tree, FP32 count conversion,
   division/reciprocal and scaling placement, including very large N.
2. Subtraction, square, epsilon-add and affine rounding, with contraction
   disabled unless separately authorized; subnormal and NaN policies.
3. Exact rsqrt semantics and target realization, including allowed error,
   exceptional arguments and target math-mode controls.
4. Observable exceptions/traps/flags. Removing a repeated subtraction or
   overlapping independent arithmetic needs a policy permitting those changes.
5. Source-compatible binding evidence, or a separately reviewed explicit bound
   native contract. Never silently upgrade v1 or infer framework bitwise behavior.

An unbound source receives characterization and an explicit refusal, not an
invented ordered fallback. Once binding exists, storage changes can preserve
the same arithmetic. Parallel trees need their own numerical authorization
unless already prescribed by the binding. Welford additionally needs state
reformulation permission and acceptance. Precision, approximate reciprocal/
rsqrt, FMA and exceptional-value changes remain separate permissions.

Finite FP32 inputs and positive epsilon do not prove safe intermediates:
subtraction, squaring, accumulation and affine operations can overflow. No
variance clamp, NaN repair or epsilon relocation is implicit. For N=1, do not
replace the computation with beta without proving exceptional-value and
signed-zero behavior under the selected binding.

LN-REF-1 fixes the centered reference before algorithm changes:

```text
n32 = exact_binary32(N), 1 <= N <= 2^24
mu = div32(ascending_add32_fold_from_positive_zero(X), n32)
d[k] = sub32(X[k], mu)
v = div32(ascending_add32_fold_from_positive_zero(mul32(d[k], d[k])), n32)
t = add32(v, epsilon)
h = sqrt32(t)
q = div32(+1_f32, h)
```

All operations are separately rounded binary32 RNE with gradual underflow;
traps/flags are unobservable, NaN comparison is class-only and all non-NaN
results require exact bits, including signed zeros. Epsilon stays an exact
positive finite binary32 value, including subnormals. Epilogue multiplies/add
remain separate. All X/gamma/beta bit patterns are admitted; no finite-only
repair is introduced. The width bound makes denominator conversion exact and
does not restrict existing v1. q has two specified rounding points, not a
single correctly rounded reciprocal sqrt or hardware approximate rsqrt.

This explicit reference does not assert current Python mean/rsqrt equivalence
and is not a registered native v2. A future producer needs an authorized native
contract; the source matcher continues to emit unbound v1. Exact compatible
source primitive bindings, if established separately, retain their own actual
order/scaling/rsqrt rather than being silently replaced by LN-REF-1.

## 4. Alternatives And Delayed Transformation

Candidate dimensions are crossed only when their contracts are compatible;
these labels are not mandatory successive transformations.

| Label | Computation and storage | Eligibility and tradeoff |
| --- | --- | --- |
| C0 direct centered | Mean scan, centered-squares scan, final scan recomputing d; no full-row retention. | Same bound centered arithmetic. Small scratch, broad capacity coverage, three X scans. Initial strict realization has one statistics owner per row. |
| C1 retain centered | Mean from X; second X scan writes d; bound variance and epilogue read retained d. | Same means/subtraction bits with permitted removal of repeated evaluation. Two X scans, scratch traffic and a long-lived full-row buffer. |
| C2 retain row / overwrite with centered | Load X once into row buffer; bound mean; replace X with d after original readers finish; bound variance; epilogue reads d. | One X scan, longer row reservation and added SMEM service/events. RF retention is a separate variant with spill/exchange costs. |
| C3 authorized Welford | Authorized running/partitioned `(n,m,M2)` state, finalize variance as M2/N, then reread X and normalize. | Permission-rejected under v1. Fewer scans than C0, additional state/update/division/merge/dependence and numerical obligations. |

Tree-reduced centered statistics, multiple rows/CTA, warp-per-row,
thread-per-row, RF fragments and cooperative SMEM loading are independent
execution choices, not synonyms for C0-C3. First compare C0/C1/C2 under one
explicit arithmetic binding and bounded same-CTA ownership. C3 stays rejected
under current v1. A fixed tree does not preserve an ascending left fold.
Cross-CTA statistics are deferred.

For large rows, chunk staging without full-row retention is another candidate:
count rereads required by later scans. Two-slot delivery hides some latency
but does not preserve old chunks once overwritten. Partial retention needs
an explicit retained-range map and corresponding mixed traffic/event accounting.

Raw moments `mean(X*X)-mean(X)^2` are not an admitted shortcut: cancellation/
overflow behavior differs and permissions reject substitution before costing.
Welford is not universally more accurate than a bound centered algorithm;
evaluate its declared domain and finite-precision realization.

Delayed transformation retains alternatives while refining coalescing, banks,
RF/SMEM reservations, active CTAs, statistics dependencies and overlap. A plan
that saves X traffic can lose after occupancy/barrier costs are known. P9
selects a complete tuple; earlier phases do not commit retention, reduction
trees or delivery mechanisms irreversibly.

### 4.1 Conditional state reformulation

For an authorized C3, characterize state explicitly rather than recognizing
the name Welford. The real-arithmetic invariant for a nonempty set S is
`n=|S|`, `m=sum(S)/n`, `M2=sum((x-m)^2 for x in S)`. The update is:

```text
n_new = n + 1
delta = x - m
m_new = m + delta / n_new
M2_new = M2 + delta * (x - m_new)
```

For two nonempty disjoint contribution sets with states A and B:

```text
n = n_A + n_B
delta = m_B - m_A
m = m_A + delta * n_B / n
M2 = M2_A + M2_B + delta*delta * n_A*n_B/n
v = M2 / N                         # only after complete row coverage
```

Treat the empty state as an explicit identity before division; do not evaluate
the nonempty formulas with zero count. Coverage must be exact, including
tails, and count arithmetic must not overflow. These equations prove a
real-arithmetic formulation, not FP32 equivalence. A concrete realization
specifies count conversion, operation order, merge tree, rounding, contraction
and overflow/exception behavior, and passes the authorized acceptance policy.
Current v1 cannot enter this path merely because a state record is present.

## 5. Work Partition, Storage And Execution Events

### 5.1 Ownership and SIMT access mapping

Every output has one final writer; statistics contributions have specified
owners, order and coverage. Read-only inputs may alias each other. That does
not permit Y/scratch to overwrite X/gamma/beta while any reader remains.
Initially require canonical disjoint output/scratch ranges and immutable read
leases. In-place or overlapping-output assignments are refused.

Separate statistics order from loading ownership. A CTA can cooperatively
load contiguous columns using `k = base + lane + j*thread_count`, publish the
buffer, and let one owner consume k=0..N-1 in bound order. This gains coalescing
without claiming parallel statistics. Its serial path and idle lanes stay in
the cost. Here lane denotes the CTA-local linear loader index; warp transaction
analysis uses the corresponding hardware lane and warp identities.

Conversely, one lane per row exposes row parallelism but can produce strided
GMEM requests across a warp. Derive transactions from actual simultaneous lane
addresses, not row-contiguous descriptors alone. Coalescing usually changes
access ownership, not host/GMEM tensor layout. Actual repacking is a distinct
allocation/movement/lifetime-costed candidate.

### 5.2 Buffer and lifetime ledger

| Object | Producer / consumers | Storage and lifetime obligation |
| --- | --- | --- |
| X, gamma, beta | Runtime / statistics and/or epilogue. | Canonical backing ranges and immutable leases through last use; affine reuse across rows is not a guaranteed cache hit. |
| Y | Epilogue / downstream or runtime. | Unique valid-element writer, physical output range, completion before publication. |
| mu | First mean / centering and recomputation. | Survives through last subtraction using it; publish if read by other owners. |
| v, q | Second mean then epsilon-add/rsqrt / epilogue. | May share a scalar slot only after last-use proof. No saved-statistics outputs initially. |
| C1 row buffer | Centering / variance and epilogue. | FP32 N-element buffer per active row plus layout/alignment padding, retained until all readers finish. |
| C2 row buffer | Loaders write X; mean reads; centering writes d; variance/epilogue read d. | Same addresses, distinct X/d generations. Join original readers before conflicting overwrite. |
| Reduction scratch | Authorized contributors / statistics owner. | Only for the bound partition/tree; reuse between statistics requires reader completion and visibility. |
| Async slots/barriers | Optional copy producers / consumers. | Source completion, destination ready, consumer completion, next acquire and barrier lifetime all explicit. |

Row storage is not an automatically evicted cache. Requested SMEM derives from
aligned offsets and simultaneous arenas, then exact-target reservation rounding.
Overlay may reduce requested size; logical death does not release part of a
CTA allocation. RF retention must include spills or be refused when spill
behavior cannot meet the selected resource/semantics contract.

Bank analysis consumes per-instruction lane addresses, access width, bank
width/count, broadcasts, padding and swizzles. Do not charge same-address
broadcast as a different-address conflict, or prove only consumer loads while
ignoring cooperative stores. Reuse target bank/resource descriptors rather
than hard-coding Hopper assumptions into a generic normalization planner.

### 5.3 Conservative C2 event sequence

```text
input readiness and leases
 -> cooperative X loads/stores -> X buffer visible
 -> bound mean reads complete -> mu visible
 -> centering reads X and writes d -> all d writes visible
 -> bound squared-deviation mean reads complete -> q visible
 -> epilogue reads d/gamma/beta and writes Y
 -> all row-buffer readers complete -> next generation may acquire
 -> all Y writes complete -> result publication -> lease release
```

The mean reader-completion edge precedes overwriting its original values.
Centering may overwrite an element after reading it only when no other old-X
reader can reach it. Each synchronization covers actual participants and
visibility scope; a CTA barrier is not inter-CTA synchronization. Inactive
lanes must participate in required CTA barriers; avoid nonuniform early exits.

Feature tails are excluded by valid-element domains. Zero-filling X outside N
and subtracting mu would not give zero squared-deviation contributions. Predicate
before invalid arithmetic/access; use tree identities only under a binding
that preserves the specified reduction. Never divide by padded width.

### 5.4 Optional prefetch and TMA

Start with demand/synchronous loading. Propose async chunk or next-row delivery
only with independent consumer work, legal ranges and a lowerable completion
protocol. A memory-bound label does not prove TMA profitable.

P7 supplies data-needed events, extents/alignment, consumer order, earliest
legal fetch, source last use, slot pressure and independent work. P8 evaluates
issue/ready/wait/release for distances and slot counts; added reservations and
waits feed back into complete costs. Two slots may overlap chunk i+1 delivery
with i consumption. A later scan still needs retained values or another load;
no credit crosses a statistics dependency without actual ready inputs.

Bind the exact bulk or bulk-tensor path, alignment, extent, byte/arrival counts,
tail handling, proxy visibility and barrier protocol. Not every TMA copy needs
a tensor map; bulk-tensor paths do. Selected-plan requirements drive launch/
package descriptor construction. Never transfer outside the allocation merely
because consumers are masked. These are future candidates, not implemented
LayerNorm paths. See NVIDIA's [async-copy contract](https://docs.nvidia.com/cuda/cuda-programming-guide/04-special-topics/async-copies.html).

## 6. Complete Profitability And Readiness

### 6.1 Work and traffic ledger

Compare the same external numerical contract and invocation boundary. This
table counts FP32 logical bytes for R rows; no cache hits, spills or saved
statistics are assumed. State/control traffic is added by the concrete plan.
It is not a prediction of HBM transactions or latency.

| Plan | X reads | gamma/beta reads + Y writes | Main SMEM row traffic | Other costs |
| --- | --- | --- | --- | --- |
| C0 | `12*R*N` | `12*R*N` | None for direct scalar recipe. | Three scans, repeated subtraction, bound statistics dependency chains. |
| C1 conservative | `8*R*N` | `12*R*N` | `12*R*N`: d write, variance read, epilogue read. | Live centered buffer, visibility and capacity. |
| C2 conservative | `4*R*N` | `12*R*N` | `24*R*N`: X write/mean read, X-read/d-write, variance read, epilogue read. | Cooperative ownership, longer reservation, bank service and barriers. |
| C3 streaming | `8*R*N` | `12*R*N` | Selected state/merge traffic; no full row retained. | Welford updates/divisions/merges/finalization; numerical change. |

If bound order/ownership permits forming d, feeding its square immediately
to variance and storing it, separate fused variants save the variance buffer
read: C1 uses `8*R*N` SMEM bytes and C2 uses `20*R*N`. They require explicit
compatibility proofs, not free traffic discounts. RF variants have their own
exchange/spill ledger; they cannot reuse SMEM byte counts.

Derive sectors from actual lane addresses. Model GMEM requests, L1/L2 service
and HBM service separately with cache-path assumptions/unknowns. Repeated
addresses and read-only parameters do not prove cache hits. Include spills,
padding, partial-lane issue, workspace and stores. Gamma/beta cache reuse or
cached X rereads can narrow the expected HBM savings of retention.

### 6.2 Arithmetic, resources and events

Count subtraction/square/affine, additions, division/rsqrt, address/mask issue,
exchanges and synchronization separately. Preserve dependent-add latency in
bound statistics. Welford divisions/merge chains are not modeled by a GEMM FLOP
count. Use scalar and special-function resources, not Tensor Core peak rates.

For LN-REF-1 explicitly cost two mean-finalization divisions plus one square
root and one reciprocal division per row, in addition to the folds and
elementwise work. A single approximate-rsqrt latency does not model this path.

Evaluate a deterministic resource-constrained schedule over typed events:

```text
ready(e)  = max(finish(p) for p in predecessors(e))
start(e)  = first admissible slot >= ready(e), considering engine queues,
            issue/service capacity and resident CTA reservations
finish(e) = target_path_finish(start(e), work(e), profile, dependency_kind)
T_kernel = last required output-completion time across scheduled CTAs
T_total  = T_guard_setup + T_workspace + sum(kernel_launch_costs)
           + T_kernel + T_required_publication
```

Include partial waves and shared device bandwidth. Do not multiply CTA time
by waves while also treating its bandwidth as a whole-device rate. Distinguish
startup, issue service and completion latency; record whether latency already
includes service. Occupancy constrains admission and available independent
work, not an arbitrary speedup multiplier. Unknown durations are not zero.

Expose a critical-path decomposition with exclusive accounting categories:

```text
T_total = T_compute + T_memory_unhidden + T_communication_unhidden
          + T_sync_control + T_launch_setup + T_publication
T_overlap_saved = T_same_schedule_without_overlap - T_same_schedule_with_overlap
```

Attribute each interval on one deterministic critical path once. Noncritical
engine service has a separate utilization ledger. A barrier may wait for memory;
do not charge both causes to the same interval or subtract another overlap
credit. Communication is zero initially. The decomposition reconciles to the
same total, not a separately fitted sum.

Cross-check per-engine service and dependency lower bounds. Memory Roofline
uses each candidate's `useful_scalar_ops / modeled_HBM_bytes` and applicable
scalar throughput/bandwidth ceilings; report division/rsqrt limits separately.
Recompute intensity for changed retention/cache/spill traffic; cache immutable
target ceilings, not a universal candidate intensity. Roofline does not replace
statistics readiness, underfill, bank analysis or full invocation latency.

All hardware quantities come through the exact
[Hardware Description IR](AI-COMPILER-HARDWARE-DESCRIPTION-TARGET-SELECTION.md),
with revisions, units and per-field provenance. Missing reduction, rsqrt/div,
cache-path or issue data are extension requirements, not private constants.
Estimated envelopes allow conditional/shadow comparisons, not calibrated
physical cycles. An `sm_*` key is not a full SKU/HBM/clock description.

### 6.3 Selection and fallback

Retain legal complete plans through costing. Within a comparable declared
model, choose minimum complete cost with deterministic ties favoring the
simpler source-preserving fallback. Label analytical/model-only evidence.
Automatic performance promotion additionally needs reviewed target evidence
and a minimum-benefit/uncertainty policy. One conservative interval rule is:

```text
upper(candidate) + margin < lower(fallback)
```

Unknown/overlapping bounds retain an implemented legal fallback, or support
an explicit diagnostic experiment without a speedup claim. Unbound arithmetic
means no executable fallback, only refusal. Do not invent constants to force
retention or TMA to win: savings must outweigh extra SMEM service, barriers,
resource pressure and serial statistics. Trace rejections and dominated plans.

## 7. Review Pseudocode

### 7.1 Planning without mutation

```text
require active PU and canonical verified normalization assignment
facts = derive_semantics_from_WHIRL()
binding = resolve_explicit_arithmetic_contract(facts)
if unresolved(binding):
    report numerical_binding_unresolved; return without transformation

baseline = construct_source_preserving_direct_plan(binding)
if not complete_legality_and_lowerability_proved(baseline):
    report baseline_unavailable; return without transformation
candidates = {baseline}
for algorithm in centered_and_explicitly_authorized_alternatives:
    for partition, retention, layout, delivery in bounded_compatible_choices:
        P = construct_complete_candidate(facts, binding, algorithm,
                                         partition, retention, layout, delivery)
        prove coverage, numerical policy, aliases and unique output ownership
        derive generations, events and storage assignments
        prove visibility, last-reader reuse, uniform participation and progress
        check target support, tails, rounded reservations and lowerability
        record failed obligations separately from high costs
        if all legality gates pass: candidates.add(P)

cost all legal plans with identical target, units and invocation boundary
winner = choose_under_reviewed_evidence_policy(candidates, baseline)
revalidate source, target, guards and dependencies immediately before commit
materialize winner transactionally in high-level WHIRL
verify transformed semantics, lineage, resource and event contracts
publish actual action trace and mapped before/after evidence
emit from selected WHIRL; runtime consumes the same launch/guard contract
```

Bound expansion deterministically and report pruning. Reuse existing common
candidate/plan services; this sketch is not another executable IR. Source,
type, attributes, aliases, target/profile, permissions and runtime ABI changes
invalidate dependent snapshots, costs and plans.

### 7.2 Conservative C2, one CTA per row

This sketch shows ordered consumers and cooperative delivery, not an
implementation of unresolved means or a parallel reduction tree. Every
`bound_*` operation implements the selected environment/rounding contract.

```text
r = CTA row identity; all threads agree on row validity
if entire CTA row is invalid: return
acquire row-buffer generation; invocation leases already cover actual buffers
cooperatively copy valid X[r,k] -> shared_row[k]
publish shared_row to statistics owner with uniform CTA participation

statistics owner: mu = bound_mean(shared_row[0:N])
publish mu; join all original-X reads before conflicting overwrite
cooperatively for valid k:
    shared_row[k] = bound_sub(shared_row[k], mu)
publish all centered writes

statistics owner:
    v = bound_mean_of_bound_squares(shared_row[0:N])
    q = bound_rsqrt(bound_add(v, epsilon))
publish q before epilogue readers
cooperatively for valid k:
    u = bound_mul(shared_row[k], q)
    z = bound_mul(u, gamma[k])
    Y[r,k] = bound_add(z, beta[k])

join all row-buffer consumers before next generation can acquire
complete all Y writes before runtime publication and lease release
```

Host completion/lease edges are runtime obligations, not device-side host API
calls. Standalone dispatch and service-JIT adapters consume the same selected
requirements through the existing compilation/execution-mode architecture.
No host PU is created just to implement this sketch.

## 8. IR Ownership, Phases And Tool Summaries

| Requirement | Classification / ownership |
| --- | --- |
| Descriptor, representation lineage, layout, access, placement, residency | Common Compiler Substrate: existing TensorEvolutionGraph and common descriptor/layout/access/residency records. |
| Numerical recipe, bound update/finalize, permission/acceptance references | Reusable common tensor semantics, not model-name dispatch. New persisted bindings need separate schema/compatibility review. |
| Tile/partition, candidates, plans, cost, guards and generic runtime variants | Common Compiler Substrate records; common/com remains policy-free. |
| Lifetimes, alias ranges, generation reuse, fetch/pipeline events, resources | Common Compiler Substrate concepts with exact-target extensions; generic P8 packing/progress proof remains open. |
| Derivation, legality, costing, selection and mutation | Owning VHO/WOPT/LNO phase in driver-selected scope. PREOPT verifies canonical form; reuse LNO dependence/locality and scalar simplification only under equivalent semantics. |
| Invocation and serving | Runtime owns actual buffers, backing roots, leases, streams and completion. Batch/SLO/KV/expert state is AI-domain-specific, not common normalization meaning. |
| CUDA realization | Target backend owns instructions/address spaces/barriers. WHIRL carries selected meaning; whirl2c validates/prints, never selects. |

Loop/tiling/locality candidate search is proposed at O3 after canonical
verification, disabled below that level. Eventual straightforward O0 lowering
still requires numerical binding. PU-local work never traverses another PU;
cross-PU work needs explicit IPA. New normalization/authorization controls need
option-ledger review before registration. Existing `-DSL:wopt=on`,
`-DSL:ai_opt_trace=on|off` (default on), `-Wb,` and TP_WOPT2 AI mask
`0x00800000` are reused, not new numerical or optimization authorization.

| Phase | Responsibility | What the tool must summarize |
| --- | --- | --- |
| P0 | Exact centered recipe and native binding. | Shapes/affine access, epsilon bits, permissions, numerical unknowns and source. |
| P1 | Access lifetimes and aliases. | Original/centered/statistics consumers, backing ranges, conflicts and lease obligations. |
| P2 | Algorithm/retention alternatives. | C0-C3 eligibility, binding, proof/acceptance, missing permission and prune reasons. |
| P3-P5 | Logical representation and placement. | Row/feature layout, actual repacking costs, single-device placement and communication not-applicable reason. |
| P6 | Storage and physical layout. | Offsets/generations, RF/SMEM reservation, banks, capacity, spill uncertainty and reuse proof. |
| P7 | Partition and access mapping. | Row/lane ownership, transactions, widths/tails, statistics order, padding and resource limiters. |
| P8 | Movement, visibility and reuse. | Copy/ready/read-complete/release, participants, generations, need times, waits and progress proof/refusal. |
| P9 | Complete costs, selection and WHIRL. | Components/units/provenance/bounds, selected/fallback reason, actual actions and mapped diff. |
| P10 | Guards and later certified variants. | Actual shape/alignment/lease/resource admission, variant, fallback/failure and launch requirements. Guards are not adaptive scheduling. |
| P11 | Calibration and feedback. | Predicted/measured service/waits, target/compiler identity, numerical results and unknowns. No inferred GPU evidence. |

Each existing phase log starts `# What it summarizes:`. Reuse `semantic.log`,
`alternatives.log`, `locality.log`, `residency.log`, `tile.log`,
`fetch-pipeline.log`, `physical-plan.log` and `runtime-guard.log` where relevant.
`optimization.log` collects phase detail; `optimization-summary.log` includes
P0-P11 coverage, disabled/not-applicable/refused states, actual rewrite count
and remaining gates. This document/verification does not count as a rewrite.

Future compact trace extensions require versioned positional headers, not
repeated-field JSON rows. Candidates retain region/plan IDs, binding/algorithm/
partition/storage/delivery IDs, legality reason and source line last. Costs
retain profile/model/unit/confidence, logical/transaction traffic, reservations,
critical-path components and explicit selection/fallback reason. Action rows
name actual WHIRL loops, storage, accesses and synchronization. No new tags
are implemented here; preserve existing AI_NORM FORM/FACT and trace-off checks.

## 9. Implementation Gates And Concurrent Ownership

| Gate | Deliverable / acceptance boundary | Status |
| --- | --- | --- |
| LN0 | Review semantics, alternatives, lifetimes and costs in this document. | Proposed; native/source R1 evidence exists, not executable optimization. |
| LN1 | LN-REF-1 ordered means, exact count/final division, sqrt-then-divide, RNE/gradual/class-only-NaN environment; separately authorized native contract, carriers and oracle required. Preserve v1. | Arithmetic design specified; native admission/target binding/oracle implementation open. See the LN1 sub-gates in the implementation plan. |
| LN2 | Read-only C0/C1/C2 plans, exact traffic/resources/events and refusal cases; reject C3 under v1 and LN-REF-1. | Open after LN1 native prerequisites; reuse common/reduction services. |
| LN3 | Complete costs, independent event/model oracle, analytical winner/fallback and trace auditor. | Open; reuse reduction profitability. |
| LN4 | Selected high-level WHIRL, guards, CUDA emission and mapped before/after evidence. | Open; reuse reduction lowering; initial non-TMA scope. |
| LN5 | Separately authorized parallel statistics, RF retention, Welford or async/TMA refinements. | Deferred until base semantics/executable alternatives are reviewed. |
| LN6 | Native numerical, spill/resource, PTX/SASS and paired-latency verification/calibration. | GPU gate open, not a blocker to design/compiler-side work. |

The user authorized **Code AI optimizations** to implement reduction R2-d/R2-e
on an isolated branch from the verified PR #201 integration base. That chat
owns reduction code/tests and milestone notes, not LayerNorm arithmetic policy.
This chat owns cross-building-block design and top-level design/plan amendments.
Review shared service boundaries before LayerNorm coding; do not create parallel
planners. Assignment does not mean reduction selection/lowering has completed.

Required validation for later LN gates:

- Native/PREOPT: unchanged v1 identities/attributes/direct operands; bound
  contract negatives and output-unchanged-on-failed-query checks.
- Numerical: source-compatible bit/class checks separate from FP64 accuracy
  witnesses; constant, cancellation, large-offset/small-spread, overflow,
  signed-zero, subnormal/epsilon and NaN/infinity cases. FP64 alone is not an
  exact rsqrt oracle; bind exact behavior or an authorized approximation.
- Domain: R/N=1, odd widths, just below/at/above chunk/warp width, row tails,
  multiple chunks and capacity boundaries; denominator is always valid N.
- Storage/events: aliases, overlap, premature overwrite/release, bank-sensitive
  loads/stores, reader joins, missing visibility, divergence, cycles, spills
  and reservation overflow.
- Cost: independent ledger/schedule, serial bottleneck, cache/no-cache,
  occupancy cliffs, grid underfill, async refusal, unknown data and identical
  trace-on/off decisions.
- Transformation: reopen before/after `.B` independently with `ir_b2a -st -src`;
  retain raw `.T` diff/source, actual loops/symbols/address spaces/rounding;
  prohibit unauthorized reassociation/FMA; CUDA reflects selected WHIRL only.
- Native target: distinguish assembler-only evidence from GPU arithmetic and
  timing; retain explicit unverified gates.

Retain option-controlled raw `.t`, reports, numerical witnesses, before/after
`.B`/`.T`/diff, `.cu`, commands/sources, profile/model identity, hashes and receipt.
Promote fresh evidence under existing
`artifacts/review/ai-optimization/research/` conventions without replacing old
receipts. No compiler artifact is fabricated for this design-only increment.

## 10. Open Review Decisions

1. LN-REF-1 arithmetic is defined separately from v1. Review the exact new
   native version/attributes and producer admission before implementation;
   no numerical choice remains for the emitter to invent.
2. LN-REF-1 binds exceptional/signed-zero/subnormal observations without a
   finite-only restriction. Any later application-specific real-error policy
   or narrower specialization needs explicit acceptance/proof/guard/fallback
   and accounting for its runtime checks.
3. First mapping: row-thread C0/C1 or CTA-row C0/C2? Compare coalescing, serial
   statistics and resources rather than choosing by bytes alone.
4. Which scalar/div/rsqrt/issue/queue profile fields need extension, and which
   remain estimated? Exact architecture lookup is not calibration.
5. What benefit/uncertainty rule permits automatic promotion? Keep explicit
   analytical experiments separate from production performance claims.
6. Which P8 typed events/range services are reused by later generic storage
   packing without pretending the allocator already exists?

## 11. Research Provenance And Design Proposals

Primary sources checked 2026-10-28. Local implemented contracts govern Open64;
external equations or examples do not grant numerical permission.

| Source | Observation and design use |
| --- | --- |
| [Ba, Kiros and Hinton, Layer Normalization, 2016](https://arxiv.org/abs/1607.06450) | Per-example normalization with adaptive gain/bias; motivation, not a prescribed FP reduction order or GPU schedule. |
| [PyTorch LayerNorm reference](https://docs.pytorch.org/docs/2.14/generated/torch.nn.LayerNorm.html) | Last normalized dimensions, population variance and elementwise affine terms. Does not prove decomposed/native bitwise equivalence to opaque framework kernels. |
| [Triton Layer Normalization tutorial](https://triton-lang.org/main/getting-started/tutorials/05-layer-norm.html) | Row-program forward example, centered reductions and output scan; wrapper chooses block size/warps and bounds its example's feature size. Mapping inspiration, not a universal compiler limit or proof of our mean/rsqrt semantics. Source load loops do not establish final physical traffic. |
| [Chan, Golub and LeVeque, Algorithms for Computing the Sample Variance: Analysis and Recommendations, 1983](https://doi.org/10.1080/00031305.1983.10483115) | Variance algorithms and roundoff, including difficult small-variance cases. Numerical review basis, not permission to substitute algorithms based only on real-arithmetic identities. |
| [NVIDIA CUDA Best Practices Guide](https://docs.nvidia.com/cuda/cuda-c-best-practices-guide/index.html) | Coalescing, bank service and resource-limited occupancy constraints. No proprietary timings or guaranteed latencies imported. |
| [NVIDIA asynchronous copies](https://docs.nvidia.com/cuda/cuda-programming-guide/04-special-topics/async-copies.html) | Element-wise async versus bulk/bulk-tensor paths, completion and visibility. Future delivery legality, not automatic TMA profitability. |

C0-C3, binding gates, delayed complete-plan selection, proposed traffic/event
costs, WHIRL ownership and the LN queue are Open64 proposals, not claimed
implementations/results of those systems. R0 synthetic demonstrations and R1
samples remain separate evidence classes, not native LayerNorm certification.
