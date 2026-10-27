# MMA2 Analytical Shadow Model

Scope: O3, one active PU, independent CTAs, the four MMA1 dense SS recipes.
This is a conditional analytical schedule, not an instruction simulator,
numerical proof, implemented typed fallback or native performance model.
Selected/lowered WHIRL remains the executable authority.

Common owns runtime timing ranges and immutable unknown target descriptions.
VHO owns compatibility screening, work/resource derivation, event scheduling
and diagnostic ranking. No new compute option or automatic promotion.

## Inputs And Unknowns

Every rate/duration has an explicit known state and lower/nominal/upper values.
Unknown is printed as `unknown`, never evaluated as zero. Zero is allowed
only for explicitly known durations. Rates must be positive. The initial
real-target profiles have unknown timing and configured resource availability.
They produce no numerical makespan or nominal winner. Native MMA, typed
fallback, descriptor and progress obligations remain unproved.

Synthetic profiles supplied explicitly by focused fixtures exercise the
evaluator. Units are synthetic reference cycles, not a physical clock.
Resource capacities reuse the existing runtime resource envelope; no scalar
throughput is reused as Tensor Core throughput. A TMEM-dependent candidate
additionally requires an explicit per-SM CTA reservation cap supplied by the
model. The per-CTA 512-column ISA limit is never divided into SM occupancy.

MMA1 shapes/major modes, numerical permissions, source/region/plan/value and
generation links remain mandatory. Only synchronous SS staging is modeled.
Direct GMEM cannot feed SS MMA; pipelined/TMA compute lifetime proofs remain
MMA5. Matrix tails reject; partial residency rounds remain supported.

## Work, Residency And Schedule

CTA tiles equal the instruction M/N atom; BK is a multiple of instruction K.
Useful FLOPs = 2*M*N*K. Issued atoms = CTA_count*(K/K_inst), and executed work
counts every atom. Logical staged bytes, SMEM writes/reads, TMEM epilogue reads
and output bytes are separate ledgers; none is claimed as measured HBM misses.
A hypothetical same-contract typed SIMT companion uses RF accumulation,
explicit low-format input conversion and its own scalar rate. It is not the
current F32-buffer fallback, and remains unimplemented.

Use the existing rounded RF/SMEM residency calculation. SIMT estimates include
its RF accumulator and operand temporaries; MMA RF uses the declared ledger.
TMEM is charged separately. Full rounds use S*R CTAs; evaluate one full round
and one partial class, then multiply full-round costs with checked arithmetic.
No resource throughput is multiplied by R. This lockstep round approximation
forbids overlap between residency rounds and is not a native timing bound.

Within a round, assign CTAs round-robin to modeled SMs. Each CTA owns one
staging buffer and one completion window. Stages are ordered K-major, atoms
then CTA-major. Input transfer serializes through one device bandwidth;
staging writes and compute operand reads share each SM's SMEM service budget.
Issue/control and scalar epilogue share a per-SM scalar actor. Tensor service
has a separate per-SM calendar. A finite shared compute queue limits pending
issues across resident CTAs; every CTA waits for its prior accumulator result.
Queue slots retire on complete results, not issue. Output uses the same
device bandwidth as input. TMEM allocation, result read, load completion and
teardown costs are explicit. Unsupported protocols are never inferred from
predicted timestamps.

For each event, start=max(data/accumulator/actor/resource/queue predecessors).
Compute completion=max(issue+service end, issue start+declared result latency).
Every timestamp carries its critical-path cost attribution; deterministic max
ties preserve the earlier predecessor. Attribution sums exactly to makespan.
Busy-service floors and accumulated waits are separate diagnostics, not
additional additive costs. Invocation adds explicit preparation and launch
once. Bound literal schedule complexity by a caller budget; exhaustion fails
without output mutation instead of guessing a compressed recurrence.
Scratch calendars are PU-local heap storage, not WHIRL table fields. Explicit
evaluation limits are 1024 modeled SMs, 65536 CTAs per round, eight pending
queue entries per SM and the caller's checked event budget. Exhaustion is a
diagnosed unknown evaluation, never a cheaper candidate or target rejection.

Evaluate three consistent corners: fast rates/short durations, nominal, and
slow rates/long durations. These are declared scenario envelopes, not
statistical confidence intervals. Roofline/service floors are diagnostics;
verify makespan >= floor. Unknown inputs prevent timing/ranking. Overflow,
invalid ranges, bad units/provenance, incompatible contracts and exhausted
budgets fail closed. Returned work/resource facts never imply lowerability.

## Shadow Results And Evidence

`AI_MMA_SHADOW 1` is separate from AI_OPT 7/model6 and AI_COMPUTE 1. It reports
tuple identity, family, work/traffic, rounded reservations, full/tail rounds,
timing inputs, costs/components/floors/waits, bounds and conditional ranking.
Candidate source line is last. Reports begin `# What it summarizes:`.
Production F32 joint planning reports exact-target/numerical incompatibility
and unknown timing while preserving its existing selected SIMT plan. No source
is implicitly narrowed. Typed test fixtures may rank hypothetical plans;
emitted execution remains SIMT regardless of the shadow winner.

Acceptance requires an independent literal oracle, finite-queue/shared-resource
cases, one/two/many K atoms, underfill/tails, bounds/floors, unknown/range/
overflow/budget/contract failures and output preservation. Retain unchanged
SIMT `.B`, independently reopened `-st -src` `.T`, raw `.t`, CUDA, full diffs,
phase reports and authenticated baseline comparisons. Compiler-only evidence
does not close MMA3, MMA4/5, MMA6 or native certification.

## Local Validation

The native fixture covers 496 complete synthetic schedules, each at three
corners, plus eight wholly unknown descriptions. This includes 132-SM
fictional scenarios, both formats, both compute families, one/two/many atoms,
BK=16/32, partial rounds, queue=1/2/4, known zero durations, missing individual
metrics, missing TMEM capacity, numerical/shape/identity rejection, arithmetic
overflow and budget failure with untouched output/empty inspection streams.
The independent Python event-DAG oracle checks work, rounded residency,
timing envelopes, shared-service floors and additive attribution; synthetic
phase reports show 248 paired conditional comparisons. No synthetic rate is
a target default. TMEM epilogue service assumes streaming through the declared
RF ledger, not an implemented native fragment mapping.

113 host regression tests pass. Linux builds rebuild the native tests,
`be.so`, `be` and `lw_inline`; symbol inspection excludes frontend-builder and
JsonCpp symbols. The existing syntax/operator-layout matrix and miniature
CKKS2C publication checkpoint also pass. The impact selector conservatively
marks unfamiliar MMA paths as full-FHE candidates: these changes only reach
the CUDA joint-plan trace, runtime-only descriptions and pure model helpers,
not FHE planning, checkpoint or mapped-image code. This is not a fresh full
FHE model certification; no historical FHE receipt is relabeled or replaced.

Fresh compiler/source review covers 15 existing D/S1/T2 configurations and
their host simulations. Authenticate all 1384 files in the accepted M9
receipt before comparing. All 60 before/after `.B`/`.T` files and existing
AI_OPT rows match that baseline exactly. CUDA bodies match; retained raw CUDA
diffs contain only the translator date. The normal before/after transform
diff still shows existing SIMT materialization; the MMA2 delta is the empty
`baseline-current.before.diff`/`baseline-current.after.diff`, not a newly
invented transform checkpoint. Three trace-disabled controls also preserve
the exact selected mapped images and reopen independently with `-st -src`.

Reproduction entry points under `osprey/be/vho/tests/ai_matmul_cuda/`:

- `compute_shadow_report.py <native-trace> --reports <new-directory>`
- `run_readiness_review.py --build-dir <build> --output-dir <new-directory>`
- `run_compute_shadow_audit.py --legacy-receipt <M9-receipt> --current <fresh-review> --output <new-directory>`
- `run_compute_shadow_controls.py --build-dir <build> --current <fresh-review> --output <new-directory>`

The native target is `make -C <build>/osprey/targdir/ir_tools
dsl_compute_shadow_contract_test`; capture its stdout as the oracle input.
Accepted receipt paths and source checkpoint follow in the durable acceptance
record. No PR or push is part of this local milestone.

## Durable Acceptance Record

Implementation checkpoint: `e0a50cbb46c99a767175236e135b14e4ffbcdf92`, based on
updated develop `7019e53bcfc0c2cef1521d7119f6f0c457f263cb` (merged PR #195).
Branch: `codex/ai-mma2-shadow`. Local only, no PR/push.

Immutable primary receipt (1064 checksummed files, source snapshots, copied
Linux binaries, exact command vectors, raw traces, mapped images, CUDA,
native/host results, all phase reports and empty MMA2 delta diffs):

`/Users/shinmingliu/open64/artifacts/review/ai-optimization/mma2/shadow/sha256-7d3a792cac5e23b258f4a447f6d488107b1e6ceeb213339195ba421fbefa2ba3/`

Independent read-only durable replay: copied native output and synthetic
summary are byte-identical; six separate `ir_b2a -st -src` processes reopen
both images for direct/staged/large-tail; regenerated phase and aggregate
reports match exactly:

`/Users/shinmingliu/open64/artifacts/review/ai-optimization/mma2/reopen/sha256-4f0a23e3404af1cf4c73ee141a2f9ffc93b277ce2b110337a52591e5e055649f/`

The replay's `verification.json` records a metadata correction: the primary
command record's `build_root` parent traversal abbreviates to `/private/tmp`;
the actual build root is `/private/tmp/ai-m2-build-20261021`. Its explicit
mount mappings and all `/build` commands are correct. Neither immutable
receipt is edited to conceal that correction.

Review entry points relative to the primary receipt:

- `acceptance/shadow.t`: all conditional model inputs/results and unknown descriptions.
- `acceptance/synthetic/physical-plan.log`: 248 paired comparisons, nominal versus bounded family and unchanged execution authority.
- `acceptance/compiler-review/staged-sm90a/gemm.t`: actual F32 compiler trace with four rejected/unknown MMA source rows.
- `acceptance/compiler-review/staged-sm90a/reports/optimization.log`: complete owning-phase collection, including numerical incompatibility.
- `acceptance/compiler-review/staged/gemm.before.T` and `gemm.after.T`: unchanged source/device SIMT materialization evidence.
- `acceptance/parity/staged/baseline-current.after.diff`: empty MMA2 device-IR delta against authenticated M9.
- `acceptance/parity/summary.log`: fifteen configurations, sixty identical images/dumps, unchanged old trace rows and CUDA bodies.

Representative real trace (joint candidate ID 3 remains selected):

```text
AI_MMA_SHADOW 1 SOURCE 1 2 1 numerical_contract_required unknown 3 unknown 3 none 1
AI_MMA_SHADOW 1 SOURCE 2 2 1 numerical_contract_required unknown 3 unknown 3 none 1
AI_MMA_SHADOW 1 SOURCE 3 2 1 exact_target_required unknown 3 unknown 3 none 1
```

This closes the bounded MMA2 analytical/shadow coding milestone, not typed
lowering, real timing availability, guarded native promotion or any native
numerical/performance gate.
