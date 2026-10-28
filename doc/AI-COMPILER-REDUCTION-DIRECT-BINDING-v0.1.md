# Direct Ordered Reduction Target, Storage And Event Binding

Version v0.1 | 2026-10-27 | Bounded R2-c compiler-contract implementation

## 1. Scope And Maturity

Follow-up: [runtime guards and enforcement](AI-COMPILER-REDUCTION-RUNTIME-GUARDS-v0.1.md)
now implements fresh export and separately linked host-mock lease/event admission.
The original compiler-contract evidence below remains conditional; production
CUDA adaptation, selected WHIRL lowering and native certification remain open.

This document extends [reduction numerical binding](AI-COMPILER-REDUCTION-NUMERICAL-BINDING-v0.1.md)
with the direct ordered fallback's scalar target contract, symbolic buffer
binding, row ownership and host dependency protocol. It is independent of MMA1.
No hardware table, compute-realization profile, native opcode/version, mapped
record or Python ingestion rule changes.

Implemented: `WOPT_DSL_bind_reduction_direct` in WOPT-owned
`opt_dsl_reduction.h/.cxx`, native positive/refusal tests, strict positional
trace auditing and actual PREOPT source-preservation checks. The service
validates a **compiler contract conditional on explicit runtime obligations**.
It does not observe a live device buffer, enforce a runtime lease, emit a
selected kernel or certify PTX/SASS.

Existing Python `torch.sum` still emits source-order-unresolved v1. Only the
explicit ordered v2 recipe can acquire this direct binding. No caller can
replace v1 with v2 by supplying a target request or metadata hint.

## 2. Defined Terms

| Term | Meaning |
| --- | --- |
| ISA contract | A documented instruction-level numerical guarantee, distinct from proof that generated code actually uses that instruction. |
| Runtime contract ID | Identity of the proposed invocation/buffer/event contract; nonzero does not prove an adapter exists or has enforced it. |
| Context ID | One runtime device/address-space context shared by both buffers and all protocol events. |
| Allocation ID | Canonical underlying storage-object identity within that runtime contract/context. All aliases/views of the same storage use the same ID and capacity. |
| Allocation capacity | Byte extent of that underlying object, not the size of one view. |
| Buffer offset | Byte displacement from the canonical allocation base. |
| Accessed interval | Half-open byte interval actually used by this tensor, derived from its native type and offset. |
| Base alignment | Runtime guarantee about the canonical allocation base; a positive power of two, at least four bytes here. |
| Snapshot input lease | The exact input interval remains live, readable, contiguous FP32 and unchanged during all kernel reads. |
| Exclusive output lease | The output interval is reserved against outside writers/readers until production completes; designated consumers are authorized only after completion. It stays live until the consumer join completes. |
| Row owner | One linear global thread ID maps to the equal row index, with `row < R` guarding extra threads. One serial accumulator produces each row. |
| Event edge | A required happens-before dependency in the proposed host plan; not a timestamp, a stream number or an observed completion. |
| Consumer event | Join of all authorized output consumers, not merely the first consumer's start or completion. |
| Bound contract | Source, ISA policy, symbolic storage and event/owner checks pass; runtime enforcement and code conformance remain required. |

Runtime allocation IDs are not ST/TY identities, sample Python addresses,
CUDA stream identifiers or invented noalias proofs. A provider must canonicalize
suballocations, imported allocations and views, then enforce the declared
ranges, context, leases and ordering. Without such a provider the contract
remains conditional and cannot be advertised as runtime-verified.

## 3. Scalar Arithmetic Binding

The binder reuses `DSL_hardware_description_find_arch` with exact existing
public spellings `sm_90`, `sm_90a`, `sm_100`, `sm_100a`. It consumes the
architecture identity only. It does not use estimated memory delays,
performance rates or MMA capabilities as numerical evidence.

The one admitted instruction contract is scalar `add.rn.f32`, with neither
`.ftz` nor `.sat`. NVIDIA's PTX specification defines explicit nearest-even
rounding and supports subnormal input/results by default on these architectures;
the optional FTZ and saturation modifiers change those semantics.
[PTX floating-point add](https://docs.nvidia.com/cuda/parallel-thread-execution/index.html#floating-point-instructions-add)

The source v2 policy also requires no traps or observable FP exception flags.
NVIDIA documents instruction-encoded rounding and the absence of GPU
floating-point trap/status-flag mechanisms.
[Floating Point and IEEE 754, sections 4.4-4.5](https://docs.nvidia.com/cuda/floating-point/index.html#differences-from-x86)

The target request explicitly identifies the instruction contract and compiler
policy. FTZ, fast math, reassociation, hidden widening, observable exceptions,
finite-only assumptions and all unknown option bits are refused. This is a
deliberately strict first policy; even an explicit non-FTZ instruction is not
used to silently override contradictory whole-pipeline settings.

For a future CUDA source path, `__fadd_rn` is an existing documented candidate:
nearest-even addition, specified special-value behavior and no multiply-add
merging. Its actual FTZ settings and emitted code still need certification.
[CUDA single-precision intrinsics](https://docs.nvidia.com/cuda/cuda-math-api/cuda_math_api/group__CUDA__MATH__INTRINSIC__SINGLE.html)

### Existing Open64 Reuse Point

`common/com/intrn_entry.def` already defines `INTRN_F4ADD_ROUND` in the
`TARG_NVISA` section; `be/cg/NVISA/expand.cxx` maps it to `TOP_add_rn_f32`.
The NVISA target description spells the corresponding PTX instruction.
Reuse or explicitly adapt these existing semantics before inventing a
parallel arithmetic opcode. The current host backend does not automatically
expose this target-conditional intrinsic, and legacy `fadd_rn` naming does not
by itself prove `whirl2c -cuda` emits the required CUDA intrinsic.
No intrinsic encoding/availability or emitter change is made here.

ISA binding, selected-WHIRL instruction materialization, emitted PTX inspection
and native GPU numerical testing are four distinct evidence levels.

## 4. Storage Binding And Alias Gate

Only native types whose representation explicitly says `row_major` or
`contiguous` are admitted. Runtime buffer descriptors must implement that
same static FP32 shape/layout. The existing native reduction verifier also
requires compatible input/result representation; the unsupported-layout
fixture therefore uses matching column-major input/result types before asking
this narrower direct binder to refuse them.

For native source shape `X[R,N] -> Y[R]`:

```text
input_bytes  = checked_u64(4 * R * N)
output_bytes = checked_u64(4 * R)
input_range  = [input_offset,  input_offset  + input_bytes)
output_range = [output_offset, output_offset + output_bytes)
```

Both buffers must refer to device-global memory in the same declared context.
Require nonzero allocation IDs, valid base alignment, four-byte-aligned offsets
and complete in-capacity intervals. Use subtraction-based capacity checks
before computing endpoints. No arithmetic overflow is silently wrapped.

If allocation IDs differ, disjointness follows only under the runtime's
canonical allocation-object contract. If IDs match, require identical underlying
capacity and disjoint accessed intervals. Adjacent intervals are legal; even a
single-element overlap is refused. This first direct fallback intentionally
does not support in-place output, cross-row alias schedules or conflicting
read/write leases.

The input requires a snapshot lease and the output requires exclusive
reservation through its authorized consumer join. Source purity only proves
this operation does not write X; it does not prove some other kernel/service
will leave X unchanged. No `__restrict__` qualifier or physical noalias flag
is generated from this query.

Capacity is an actual interval obligation, not a shared-memory budget or
occupancy model. The fallback uses no explicit SMEM scratch; one logical FP32
accumulator per active row is not a claim of zero register/control/address
overhead or zero spills.

## 5. Ownership And Event Protocol

The direct mapping is `row = linear_global_thread_id`, guarded by `row < R`.
The declared launch domain must cover all R rows. This injective mapping gives
one logical owner per row and no partial-sum merge. Actual block/grid limits,
64-bit index materialization, launch configuration and hardware resources must
be validated when lowering/launch construction is implemented; this query does
not select those parameters.

Six event roles, with fixed indices in the request:

| Index | Event | Meaning |
| --- | --- | --- |
| 0 | Input ready | Input contents and snapshot lease are ready for kernel consumption. |
| 1 | Launch | Submission/execution-order point that follows readiness. |
| 2 | Complete | Kernel has finished all row reads and output stores, with results visible to subsequent consumers. |
| 3 | Consume | Join of all authorized output consumers. |
| 4 | Input release | End/reuse of the input lease. |
| 5 | Output release | End/reuse of the output reservation after all consumers. |

Required order:

```text
input_ready -> launch -> complete -> consume -> output_release
                         |
                         +-> input_release
```

The request contains an adjacency mask per role. Compute transitive closure,
reject cycles/self-edges/unknown bits, and require every dependency above.
An indirect path is sufficient; textual list order and equal stream IDs are
not substitutes. Input release may precede consume because the accessed input
and output intervals are disjoint and all reads completed. Output release may
not precede the consumer join.

Kernel-local protocol remains synchronous:

```text
row = linear_global_thread_id
if row < R:
    state = +0_f32
    for k in [0,N):
        value = load_global_f32(X + 4*(row*N + k))
        state = strict_add_rn_f32_noftz(state, value)
    store_global_f32(Y + 4*row, state)
```

The next addition consumes the previous accumulator; even positive zero plus
the first element is retained. No barriers, atomics, inter-thread reductions,
async copy or TMA is introduced. Same-thread data/control dependencies provide
the local order; the host event protocol covers other kernels and consumers.
The pseudocode is normative planning intent, not independently generated CUDA.

## 6. Binding Algorithm And Ownership

```text
require output/request and canonical member assignment in active whole PU
derive source facts freshly from native WHIRL
reject stale expected source identity/type/shape/order/source position
require explicit ordered source v2
resolve exact architecture; require supported scalar instruction and FP policy
require supported contiguous representation
derive checked byte footprints and validate both allocation-relative ranges
require runtime contract/context and declared input/output lease kinds
prove disjoint intervals under canonical allocation identity semantics
require total, injective linear row-owner mapping
prove acyclic event graph and all lifetime/visibility dependencies
publish bound compiler contract WITH outstanding runtime obligations
do not select, cost, mutate WHIRL or emit executable code
```

Invalid scope, detached/cross-PU member or null arguments return failure without
changing the output. A canonical source with a refused request returns an
explicit reason and only the already-validated stage prefix. Stage bits are
source=1, scalar ISA=2, storage=4, owner/events=8. Successful stage mask is 15.
Refused requests carry no granted runtime mask.

On success the runtime-obligation mask is still 15:

| Bit | Required provider/codegen obligation |
| --- | --- |
| 1 | Canonical device-global allocation IDs, actual capacity/base alignment/context and matching contiguous tensor descriptors. |
| 2 | Live snapshot input and exclusive output leases; no outside conflicting accesses. |
| 4 | Enforce the proved event dependencies and the completion/consumer-join meanings. |
| 8 | Launch and selected kernel conform to the row mapping, coverage and ordered scalar protocol. |

The binding result owns a numeric snapshot of the checked instruction/options,
runtime/context IDs, both buffer descriptors, row mapping and event edges;
it retains no architecture-string pointer into caller storage. Any source,
target or host-request change requires a fresh query. This transient snapshot
is not a second executable IR and cannot bypass selected-WHIRL materialization.

These are required obligations, not four checked runtime facts. The provider,
guard/discharge mechanism and selected WHIRL lowering are not implemented by
this increment. Cost selection must never interpret this mask as zero.

Common/com retains native structure and existing architecture lookup.
WOPT owns PU-local binding/legality. Runtime APIs/lease implementation belong
to the generic runtime provider with a configurable CUDA adapter; they are not
compiler PUs. Program-specific host dispatch may generate calls to that adapter.
No CUDA allocation API is hard-coded in this binder.

## 7. Trace And Reporting Contract

The service accepts an optional caller-controlled trace stream and emits
additive `AI_REDUCE_DIRECT 1 BIND` positional rows. The header is published in
`reduction_direct_trace_report.py`; source line remains last. The row records
source-match/layout observations, architecture/instruction/policy, complete
symbolic intervals/lease kinds, owner mapping, event masks, computed footprints,
stage/obligation masks, refusal reason and selected=0. The auditor recomputes
range, alias, ownership and graph legality independently and joins the source
identity to actual PREOPT `AI_REDUCE_NUM` entry/exit evidence.

Existing `-DSL:wopt`, `-DSL:ai_opt_trace` (default on), `-tr25` and AI trace
mask are unchanged. Actual PREOPT still uses them for source verification.
The focused binding producer opens its query trace explicitly and tests the
same output with tracing disabled. The binder is not yet an automatically
invoked production pass; future integration must use those existing controls
at `-O2` or above, whole active PU only. No new trace bit or source-relaxation
option is allocated.

| Log | What it summarizes |
| --- | --- |
| `target-binding.log` | Scalar ISA contract, architecture/policy, source/stage failures and absence of emitted-instruction certification. |
| `residency.log` | Tensor byte footprints, canonical object/subrange identity, capacity/alignment/lease and alias checks, row-owner obligations. |
| `fetch-pipeline.log` | Required dependency edges, cycle/missing-edge rejection, synchronous local sequence and zero explicit SMEM/async/barrier use. |
| `optimization.log` | Full successful phase-report collection, with case/query identity and recorded refusals. |
| `optimization-summary.log` | Bound/refused contracts, outstanding runtime obligations, unchanged source semantics and remaining cost/selection/lowering/certification gates. |

Every log starts with `# What it summarizes:`. Always report
`candidate_selection=not_run profitability=not_run transformation=none` until
those phases actually execute. A conditional binding is not a transformation
or observed runtime proof. Never emit CUDA/PTX from these logs or the request.

## 8. Evidence And Remaining Work

Native fixtures cover four existing Hopper/Blackwell architecture names, source
v1 refusal, width one, width 65 and a valid-native but unsupported layout.
Positive cases also test stale identity, unknown targets, unsupported arithmetic,
each unsafe policy flag, overflowing/out-of-capacity/unaligned/incorrect-space
ranges, absent runtime/context/leases, overlap, insufficient owners, missing
dependencies, cycles, self/unknown edges, adjacent subranges and valid transitive
release order. Trace-off facts and read-only native tables are checked.

Retain `.B`, separate-process `ir_b2a -st -src` `.T`, direct query and actual
PREOPT `.t`, concise reports, source snapshots, commands, receipt and checksums
under a fresh immutable `r2-reduction-direct-binding` receipt. Separate `.T`
and `.t` directories on the case-insensitive host. Partial backend outputs are
marked incomplete; VHO still rejects missing executable reduction lowering.
There is no selected transformation and no fabricated before/after image pair.

Next gates:

1. Implement runtime/provider enforcement and explicit guard/discharge wiring;
   carry the selected contracts into WHIRL before executable emission.
2. Bind the staged alternative's storage reuse and copy visibility/progress.
3. Compare complete legal direct/staged costs, retaining direct as fallback.
4. Lower the selected plan into high-level WHIRL loops; reuse/adapt existing
   NVISA strict-add semantics, preserve before/after mapped images/diffs and
   add CUDA emission only as a consumer of selected WHIRL.
5. Validate compiler options and actual PTX/SASS, then native numerical/resource/
   performance behavior. GPU certification remains unperformed.

Neither target lookup nor this symbolic proof completes these execution gates.

## 9. Accepted Compiler-Side Evidence

Eight native fixtures execute 274 assertions. The six supported v2 fixtures
each produce 35 query rows; source v1 and unsupported-layout fixtures each
produce one refused row: 212 rows total, 18 conditionally bound contracts.
Every accepted contract retains the four runtime obligations. All source images
are independently reopened with `ir_b2a -st -src` and preserve numerical meaning
through actual PREOPT. Trace default/on/off and PREOPT-off behavior is checked;
query trace-off returns the identical owned binding snapshot.

The full trace-auditor suite passes 34 tests, including eight direct-binding
tests with 2,048 independent graph comparisons. Prior five-case reduction
binding, four native normalization variants, native Softmax/reduction, five
authenticated Python/API/driver reduction images, WOPT semantic-info and real
driver checks, canonicalization, syntax/layout, common CKKS transactions, view
retirement, miniature checkpoint and 21 source numerical tests pass. Rebuilt
backend/WOPT/inline symbol checks remain closed to builder/JSON dependencies.

Impact is confined to a new WOPT-owned read-only service and test/link wiring;
the common architecture tables, native opcodes/schema, mapped images, FHE
algorithms and MMA1 sources/profiles are unchanged. Conservative selector
unknowns are resolved by that source review and the focused compatibility
lanes. No full ResNet producer or full Python suite is claimed. An initial
disposable run exposed a fixture representation mismatch before binding; the
accepted fresh run uses consistent native input/result layout and tests the
intended direct-binder refusal. No native or GPU arithmetic execution is claimed.

Reproduction using the isolated build and a fresh artifact directory:

```sh
make -C "$BUILD/osprey/targdir/wopt" -j6
make -C "$BUILD/osprey/targdir/ir_tools" -j6 dsl_reduction_direct_test ir_b2a
python3 osprey/be/opt/tests/run_reduction_direct_review.py \
  --build-dir "$BUILD/osprey/targdir" --output-dir /absolute/new/direct-binding
python3 -m unittest discover -s osprey/be/opt/tests -p 'test_*trace_report.py' -v
```

Immutable receipt root (primary `direct/hopper/reduction.B`):

```text
/Users/shinmingliu/open64/artifacts/review/ai-optimization/research/r2-reduction-direct-binding/sha256-65a9337e7755af534b50870a58c28f1ed2b9e58b5b952363630773efdf76732f/
```

Start with `direct/optimization-summary.log`,
`direct/hopper/reports/target-binding.log`,
`direct/hopper/reports/residency.log`,
`direct/hopper/reports/fetch-pipeline.log`,
`direct/hopper/ascii/reduction.T` and `direct/hopper/trace/direct.t`.
The baseline records native values 1/3, `[3,17]`, 204 input bytes, 12 output
bytes, four declared row-owner threads, stage mask 15 and still-required
runtime mask 15, with source line 124. These are fixture observations, not
ABI constants, real allocation handles or measured device data.
Eight final-path mapped reopens and strict direct/PREOPT report replays must
match before updating the review index.
