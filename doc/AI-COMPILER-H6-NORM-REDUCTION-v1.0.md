# H6 LayerNorm and Reduction memory hierarchy v1.0

H6 implements a bounded O3, active-PU optimization for the existing explicit
LN-REF-1 LayerNorm v2 and ascending-F32 Reduction v2 contracts. It consumes
owned H2 facts, the driver's finite H3 search owner, and separately imported
`h20_96gb` or `h20_141gb` device/resource/execution/budget profiles. It does not
infer board bandwidth from `sm_90`. Earlier H1-H5 documents remain authoritative
for their unchanged contracts.

The central choice is to change load ownership and retention while preserving
ordered arithmetic. LayerNorm compares its existing C0/C1/C2 executable bodies
at narrower CTA widths. Reduction gains a cooperative contiguous load panel
followed by the original ascending left fold. A parallel sum tree would change
floating-point order and requires a different source contract; it is not an
eligible substitute here.

## Scope, owners and entry points

| Owner | Contract |
| --- | --- |
| WOPT `opt_dsl_h6.*` | Native admission/harvest before search; owned catalog construction, finite H3 comparison, fresh fixed-winner revalidation |
| Existing LN2/LN3/LN4 | Source/target/range/event admission, exact scalar instruction carriers, fixed-only legacy cycle evidence and executable LayerNorm construction/verification |
| WOPT `opt_dsl_h6_lower.cxx` | Selected Reduction construction into a driver-owned empty kernel PU; ordinary WN loads/stores, counted loops, predicates, CTA barriers and COMMENT |
| VHO H3 | Pure physical-unit cost engine and request-wide cache/proposal/unique/memory/deadline owner; optional explicit transaction envelopes |
| Backend LN4 request/driver | `-DSL:h6_memory_hierarchy=on` opts the existing CLI/typed request into H6; default OFF and old selector remain. H6 manifests carry the selected launch, independently of the original request CTA width |
| Caller of Reduction APIs | Supplies existing R2 buffer/event/lease contracts, owns fresh device PU lifetime and enforces the **new selected launch**. The original row-thread runtime guard is source provenance only |
| `whirl2c -cuda` | Prints selected native semantics and COMMENTs; chooses no layout, reduction order, CTA size or resource budget |

The public APIs are `WOPT_DSL_select_h6_normalization`,
`WOPT_DSL_prepare_h6_normalization`, `WOPT_DSL_select_h6_reduction`,
`WOPT_DSL_prepare_h6_reduction`, and
`WOPT_DSL_materialize_h6_reduction`. Full Reduction body/declaration/launch
identity is checked with `WOPT_DSL_verify_h6_reduction`. These are production
WOPT services, exercised by a driver-owned native fixture. H6 does not add a
new generic Reduction JSON checkpoint or serving launch-provider ABI.

General scalar-loop discovery, unsupported canonical forms, new Reduction
runtime launch adapters, RMSNorm, tree/split reductions, vector loads, deeper
asynchronous pipelines, TMA, cluster cooperation and hardware acceptance are
separate gates. No claim of a general four-family automatic pipeline follows
from these bounded explicit-contract routes.

Minimum optimization level is O3; maximum analysis scope is the active whole
PU. No service traverses another PU or changes `Current_pu`. The driver owns
the source-to-fresh-device transition. Existing WHIRL opcodes, tensor traits,
logical operators, mapped-image layout and runtime guard ABIs remain unchanged.
This design adopts no MLIR operation interface or trait system.

## Two phases and convergence

1. Admit the current native source with the actual LN2 or R2 owner. Capture
   H2 storage/access/version/lifetime/numerical/device facts. For LayerNorm,
   compare all common source, target, range, lease and event controls through
   existing LN3 comparability and full LN2 guard exports.
2. Construct the finite owned catalog. LayerNorm has nine candidates: C0/C1/C2
   at 128, 32 and 64 threads, in that order. Reduction has five: direct
   row/thread at 128, 32 and 64 threads, then cooperative CTA/row at 32 and 128
   threads with panel width equal to CTA width. The original 128-thread direct
   or C0 candidate is first for stable ties.
3. Evaluate detached H3 recipes using one shared request owner. Each proposal
   consumes the existing proposal limit; unique estimates, cache storage,
   site/request limits, reserved work memory and monotonic compiler deadline
   remain global. The catalog is exhausted once, with stable first-minimum
   selection under the pinned H3 objective. Any incomplete/refused comparison
   conservatively declines H6; it does not turn an unknown cost into zero.
4. Native admission services are outside the detached cost loop. Candidate
   evaluation invokes no WN/ST/TY construction, lowering, correctness program,
   profiler or GPU. Source recapture and exact source/native/table comparisons
   establish isolation. Search diagnostics state estimates and fallback status.
5. Rebind only the fixed winner: current native H2 content; common controls;
   immutable device/resource/execution/budget snapshot; H3 model; complete
   recipe/capability/counts; estimate key/full binding and rank. Check the owned
   incumbent remains the stable minimum. Re-estimate that fixed recipe without
   spending proposal/unique budget or reranking alternatives.
6. Only this selected plan reaches executable construction and subsequent
   correctness tests. LayerNorm's single selected LN3 work graph supplies the
   historical `reference_cycles` record required by LN4; no three-way LN3
   comparison is reopened by the successful H6 route. Reference cycles and H3
   nanoseconds remain separate. The old cycle margin is bound as a control,
   not silently converted to a nanosecond confidence margin.

This is a best plan in the declared finite catalog/model, not a global GPU
optimum. The enabled production route falls through to the historical
LayerNorm selector when H6 declines. If that selector also lacks legal or
complete evidence, its existing refusal remains. Below O3, without H20/H3,
with unknown/stale inputs or after exhausted budgets, no H6 kernel is granted.

## LayerNorm data flow and cost census

For a row of width N, lane zero keeps the ascending ordered mean and variance
folds. C0 rereads X. C1 writes centered d once to a shared row. C2 first copies
X cooperatively to that row, then overwrites each element with d only after
that element's final X read. Mean, center, variance, finalize and affine
operations preserve the existing rounded F32 scalar instructions; no
reassociation, FMA, inverse-standard-deviation reformulation or parallel fold.
All cooperative tails predicate before access. Uniform publish/release barriers
remain outside owner and active-feature predicates.

| Per-row work | C0 | C1 | C2 |
| --- | ---: | ---: | ---: |
| External input F32 reads, including gamma/beta | 5N | 4N | 3N |
| External output F32 stores | N | N | N |
| Scalar operations including five finalize operations | 8N+5 | 7N+5 | 7N+5 |
| Shared F32 requests, M=min(N,threads) | 0 | 3N+2+2M | 6N+2+2M |
| Uniform CTA barriers | 0 | 4 | 5 |

Shared row stride/capacity/bank legality, overwrite generations, runtime leases
and complete events remain LN2-owned. Static retained arenas above 48 KiB are
not silently enabled. Smaller CTAs can reduce rounding/residency pressure and
idle lanes; they do not imply fewer global source elements. Narrow CTA sizes
remain subject to imported resource limits and the exact selected host launch.

Existing pointer-scalar packaging is paid explicitly: each binary operation
has three four-byte reads and three four-byte writes; unary sqrt has two of
each. These private requests are kept separate conceptually from the external
source ledger. H6 conservatively places their service in the global engine
and charges one sector per lane access, with no unproved register-promotion,
local-memory coalescing or spill-free credit. This is an explicit model
scenario. Actual allocation/local traffic remain a ptxas/profiling obligation.
Reduction recipes declare a 32-register-per-thread reservation
input to this model; it is not an allocator result or hardware register count.
The first offline panel assembly used 26 registers and forced correction of
the initial 16-register hypothesis before qualification; the LayerNorm sample
used 31 against its declared 32. Other selected products still need their own
allocator/resource qualification.
The H3 `rsqrts`/`rsqrt_ns` service slot is used as the declared single finalize
sqrt service envelope; the native body still uses sqrt then reciprocal divide.
It does not replace the algorithm with an approximate rsqrt instruction.

## Reduction panel ownership

Direct K1 maps one row to a thread. Threads in a warp therefore load different
rows at a common column, often issuing strided sectors. Guard the last partial
CTA before arithmetic/access/output. K2 maps one row to a CTA:

1. For each ascending panel, every lane copies its valid contiguous F32 column
   into its own shared slot. Tail lanes issue no global or shared access.
2. All lanes execute a CTA publish barrier, including inactive tail lanes.
3. Lane zero consumes only valid slots in ascending order and updates **one
   accumulator initialized to +0**, retained across every panel.
4. Every lane executes a readers-release CTA barrier before the next refill.
5. Lane zero writes the single row result. There are no global partials,
   atomics, parallel sum tree, reassociation or padded input loads.

For panel C and width N, there are ceil(N/C) iterations, two CTA barriers per
iteration, C*4 reserved shared bytes, N*4 global input bytes, one four-byte
output and N*8 shared request bytes per row. The arithmetic dependency chain
remains N ordered adds. More CTAs and contiguous loads may improve memory
coverage; added barriers/shared traffic and the serial owner can erase that
benefit. Direct candidates remain eligible; N=1 fixtures retain direct work.

Materialization requires a fresh empty active kernel PU, n64, the strict
RNE/gradual/no-trap FP environment, exact selected seal/current H20/H3 binding,
legal geometry/resources and canonical builtin declarations. Recoverable
refusals precede allocation. After commit, internal construction contradictions
are fatal compiler invariants; no partial product is published. Source IR is
not rewritten and its analyses are not transferred into the new device PU.
Arguments point to already adjusted logical subranges. The caller must enforce
canonical backing IDs/capacities, live immutable input/exclusive output leases,
nonaliasing, completion-before-consume/release, exact grid/block/shared launch
and strict backend/toolchain FP controls. In particular, R2's original
`launched_threads` guard cannot authorize a new CTA-per-row launch by itself.

## Transaction envelopes and compatibility

H3 stages optionally carry owned `read_transactions`, `write_transactions` and
`transaction_proof`. Logical request counts remain unchanged by sector
rounding. Explicit envelopes must be quantum multiples no smaller than rounded
logical requests, with a bounded nonempty proof. Source writes and additional
modeled spill service are charged separately. Checked arithmetic refuses
underflow/overflow and incomplete evidence. All-zero/empty fields retain the
previous cost behavior and exact legacy plan-key spelling.

H6 uses 32-lane instruction groups and the worst valid four-byte base residue
for 32/64/128-byte model quanta; it assumes neither base sector alignment nor
L1 hits. Direct Reduction counts distinct sectors across row-strided lanes at
each column. Its bounded periodic implementation is checked against an
independent full-column enumeration. Cooperative instructions count contiguous
warp sectors, including tails. These are instruction/service envelopes under
declared cache assumptions, not measured HBM bytes, saturation or elapsed time.
H2 useful/source bytes, alias/version/lifetime proofs and source identities are
not replaced by these estimates.

## Identity, invalidation and explanations

A source edit, allocation/range/lease/event/version change, numerical change,
profile/resource/budget/model change or recipe/score change invalidates the
selected handoff. H6 performs fresh source capture before construction rather
than using old WN/ST/TY pointers. Ordinary source analyses remain untouched;
new device loops/accesses require their own ordinary phase analysis before
further optimization. Existing High/Mid/Low contracts are not bypassed for
unsupported forms. No new cross-PU transformation is introduced.

LayerNorm's compiler-only handoff appends optional H6 model/estimate/rank fields
and seals them. Zero preserves the historical seal inputs. Every same-build
consumer must rebuild; raw handoff test files are explicitly nonportable.
Reduction retains a full native body/referenced-declaration and launch-metadata
key. Both families are verified after independent mapped reopen. Tree hashes
or metadata labels alone cannot prove legality. Loop bounds, barriers, shared
bounds, scalar effects and launch changes invalidate the expected identity.

Generated COMMENT nodes explain the actual selected ownership, loops, retained
generations, tails, arithmetic and synchronization, estimates and host
obligations. `whirl2c` prints those COMMENTs. The existing PREOPT COMMENT
workaround remains; repairing PREOPT's COMMENT handling is still separate.

## Reproduction and evidence gates

Build `be`, `be.so`, `wopt.so`, `lno.so`, `ir_b2a`, `whirl2c`, `dsl_h6_test`,
`dsl_normalization_request_test` and their existing shared consumers with the
normal Linux integration build. Run from a fresh writable output directory:

```sh
python3 -B osprey/be/opt/tests/run_h6_memory_review.py \
  --build /build --previous-h3 /h3 --output /out/h6 \
  --cuda /usr/local/cuda
python3 -B osprey/be/opt/tests/run_h6_driver_review.py \
  --build /build --previous-h3 /h3 --output /out/h6-driver
```

`/h3` is the authenticated H3 receipt with its SHA256SUMS, pinned base profiles
and synthetic model supplements; mount original source as `/src` for DST
interleave. Exact commands, input/binary/source hashes, environment, real
immediate before/after `.B`, exact-stem `ir_b2a -st -src` `.T`, full raw diffs,
selected CUDA, reference payloads/output and static compile logs belong in a
new immutable review receipt. Source-search diffs must be empty. Device diffs
must show formals, typed coordinates/loops/accesses/shared/barriers/COMMENTs,
with no hidden CUDA template implementation. An independent final read-only
reopen and all-file hash audit closes publication.

Software gates cover both H20 variants; unit/feature/row tails; exact ordered
binary32 reference including cancellation, signed zero, subnormal, Inf and NaN
class; two Reduction execution orders; unchanged input/redzones; native
identity mutations; selected control/key/score refusals; O0/O1/O2; global finite
budgets; default/no-model fallback; a high-barrier C0 counterexample; actual
post-PREOPT production CLI and standalone launch binding; legacy H3 four-family
references, MR2/M3 controls and unchanged backend library dependencies. Offline
NVCC/PTX compile evidence stays separate from host simulation and GPU evidence.

H0/H7 must still establish actual H20 inventory/resources, code/ISA/register
allocation and spills, device correctness and sanitizers/races, separate paired
timing and profiling, counter interpretation and model calibration. No host or
static-toolchain pass certifies hardware correctness or speedup. Key risks are
serial statistics/folds, private scalar traffic, overconservative sector costs,
extra barriers/shared traffic, incomplete canonical coverage and unimplemented
new Reduction runtime launch providers. AOT selection and OpenCTI remain
deferred. Primary owner, successor, independent reviewer and approving lead
remain TBD; personnel transfer acceptance is not granted by this specification.
