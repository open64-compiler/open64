# Building Block Characterization And Execution Planning

Version v0.1 | Research snapshot 2026-10-27 | R0 worked milestone

## 1. Purpose And Evidence Boundary

Demonstrate the two layers adopted in
[Common execution planning](../../AI-COMPILER-COMMON-EXECUTION-PLANNING.md):
computation characterization/verified algebraic alternatives upstream, then
joint Work Partition, Resource Lifetime and Execution Events planning.

Four cases each have two explicitly specified realizations, algebraic and
numerical obligations, storage/event constraints, and complete cost ledgers
under one declared analytical model. A runnable host reference checks example
outputs; the same operation-independent cost evaluator consumes all ledgers.
The generators are manually reviewed research recipes, not automatic program
recognition or a new Open64 pass. No WHIRL, CUDA, runtime ABI or compiler
option is changed. Native FP32 numerical certification and target performance
remain open. Reproducing this experiment does not complete a compiler milestone.

Legality here means valid over the declared finite, real-arithmetic research
domain with specified numerical permissions and storage/event obligations.
It does not mean unconditionally legal under strict FP semantics. A changed
reduction grouping or statistics algorithm is ineligible without permission;
the demo exercises that rejection. FP64 host examples check implementation
arithmetic on fixtures, not all-input error bounds or native FP32 rounding.

## 2. Common Characterization

| Property | Required interpretation |
| --- | --- |
| Output/contribution domains | Which outputs exist and which input coordinates contribute; exact coverage and multiplicity. |
| Access relations | Logical coordinates, storage representation and affine/indirect ranges; views retain alias identity. |
| State algebra | Identity, update, optional merge, finalization inputs and partition/merge invariant. |
| Numerical contract | Finite values, empty/masked policy, accumulation precision, allowed reassociation/algorithm change and tolerance evidence. |
| Work Partition | Logical partition and partial/final ownership; provisional execution map and reduction owner. |
| Resource Lifetime | Buffer ranges, live accesses, completion joins, reservation and feasible aligned storage assignments. |
| Execution Events | Issue, ready, access/operation completion, ordering scope, generation and target realization. |

All cases assume nonempty exact domains, nonaliasing read-only inputs and
distinct output storage. Global workspace is preallocated and checked for
capacity before entry; host transfers, workspace allocation and communication
are zero in this invocation model because they are outside this contract,
not because they are free in an application. A production comparison must add
those costs if the boundary changes. Inputs are device-resident in the model.

## 3. GEMM

Characterization: `C[i,j] = sum_k A[i,k]*B[k,j]`, with M=N=K=64,
row-major input/output and no beta/epilogue. State is an accumulator per
output coordinate, identity zero; the contribution access maps are `(i,k)`
and `(k,j)`. FMA contraction is not silently introduced. Both realizations
retain the same ascending K accumulation order.

| Realization | Work Partition | Storage and event constraints |
| --- | --- | --- |
| G16: S1 tile16 | 16 CTAs; 16x16 output tile, BK=16, 64 threads. Each CTA exclusively owns its C tile. | 2048-byte A/B stage; register accumulators. Four rounds of fill -> publication -> compute reads -> release; store after all K tiles. |
| G32: S1 tile32 | Four CTAs; 32x32 output tile, BK=16, 64 threads. | 4096-byte A/B stage, larger accumulator/register estimate; same four-stage iteration protocol. |

Both partition output exactly and visit every K contribution exactly once.
For each stage `fill(t) -> ready(t) -> reads_done(t) -> overwrite(t+1)`.
Publication and release each require the declared uniform CTA synchronization.
No new fusion, TMA, vectorization or native tensor-core performance is assumed.

The memory ledger counts A/B GMEM tile requests plus the C write. SMEM traffic
counts staging writes and scalar operand reads; it does not equate requested
GMEM bytes with measured HBM traffic. Larger G32 reduces repeated operand
requests but consumes more storage. Both resource demands go through the
same rounding/residency and latency model, not a special GEMM selector.

## 4. Ordinary Reduction

Characterization: one row of 8192 values, `y = sum_i x[i]`; identity zero,
partial-state merge addition, finalization identity. A sequential fold and
partitioned sum are equal over reals because partitions are disjoint and
cover the row. Floating-point regrouping is separately permitted for R16.

| Realization | Work Partition | Storage and event constraints |
| --- | --- | --- |
| R1: one_cta | One CTA scans the entire row; fixed local/warp/CTA reduction recipe, one output owner. | Register partials and 16 logical SMEM scratch bytes, rounded to 128 by the synthetic target. Read -> partials -> scratch publication/reduction -> output. |
| R16: split16 | 16 CTAs process 512 elements each; a second kernel merges 16 partials. | 64-byte global workspace, same per-CTA scratch. All partial stores complete and are visible before the merge kernel; workspace stays valid through merge reads. |

The second kernel cannot run concurrently with unfinished producers of its
inputs. A stream-ordered kernel boundary is one future target realization,
not a CTA barrier across different CTAs. R16 pays two launches, extra partial
traffic and merge work; R1 can underfill the machine. Resource reuse does not
justify dropping the second launch or its dependence.

## 5. Stable Softmax

Characterization: four rows of 256 finite scores, `m=max(x)`,
`l=sum(exp(x-m))`, `y[i]=exp(x[i]-m)/l`. The stable normalizer state is
`(m,l)`; an empty partition has a separately handled identity, not an
unchecked `exp(-inf-(-inf))`. The current fixture has no empty/masked rows.
Both realizations use the same reduction grouping and exponential operation.

| Realization | Work Partition | Storage and event constraints |
| --- | --- | --- |
| Sretain: retain_row | One 128-thread CTA per row; retain scores/exponentials in registers. | 20 modeled registers/thread; 16 logical SMEM scratch bytes. Load -> max ready -> exponential/sum ready -> normalize/store; input is not fetched again. |
| Sreread: reread_row | Same row ownership; bounded per-thread storage, three input scans. | 18 modeled registers/thread; same scratch. Max scan -> max published -> exponential-sum scan -> sum published -> reread/recompute exponential/output. |

Retaining a whole row is an alternative, not a universal rule. Its register
requirement may fail even when Sreread is legal; the negative fixture tests
that case. Scratch for max and sum may share an offset only after the first
reduction's scratch accesses end. The scalar maximum remains live separately.
Normalizer state alone is insufficient to emit all probabilities: Sreread
still needs each original score in its final pass. No fictitious one-read
full-output algorithm is credited.

## 6. Mean And Variance Normalization

Characterization: four rows of 2048 finite inputs; population mean/variance,
`y[i]=(x[i]-mean)/sqrt(M2/N+1e-5)`, gamma=1, beta=0. Divisor N and epsilon
placement are semantic facts, not tunable profitability knobs. Sample variance
and RMSNorm are different contracts. Numerically unstable raw second-moment
subtraction is not substituted.

| Realization | Work Partition | Storage and event constraints |
| --- | --- | --- |
| Ncenter: centered | One CTA/row: mean scan, centered-squares scan, normalization scan. | 24 registers/thread, 16 logical scratch bytes reused between reductions. Mean remains live until both later scans finish; M2 ready precedes normalization. |
| Nwelford: welford | 128 disjoint strided thread partitions; Welford updates followed by pairwise state merge, then reread/normalize. | 32 registers/thread; 48 logical scratch bytes for `(n,mean,M2)` states; reduced input scans but more state and divisions. All state fields are published before merge. |

For disjoint states a and b, define `n=na+nb`, `d=mean_b-mean_a`:

```text
mean = mean_a + d*nb/n
M2   = M2_a + M2_b + d*d*na*nb/n
```

Handle zero-count identities explicitly. The invariant is that each state
summarizes exactly its assigned input subset; the merge reconstructs the mean
and centered squared deviations of the union. This proves the real-arithmetic
alternative, not bitwise FP equivalence. Algorithm-change permission plus
target numerical acceptance is required in production. The host experiment
compares against a centered `math.fsum` reference, including large-offset,
small-variance and constant rows. Neptune's paper explicitly identifies a
Welford-related limitation in its correction solver; we therefore preserve a
reviewed statistics contract rather than claiming automatic discovery [R2].

## 7. Shared Cost Model And Complete Comparison

All costs are integer `research_ticks` from
[synthetic_profile.json](synthetic_profile.json), explicitly not a NVIDIA
description or production table. The profile has four abstract SMs, fixed
capacity/rounding and separate ALU, exponential, division, reciprocal-square-
root, memory, barrier and launch costs. Four scenarios vary memory, division
or launch service; no coefficients are fitted to measured rankings.

For each kernel its shared/register reservation is fixed across all phases.
Let R be the minimum block/thread/warp/register/SMEM residency quotient. Up to
`SMs*R` CTAs form a round; `a` is its active CTA count,
`s=min(a,SMs)` and `q=ceil(a/s)` is its busiest SM's CTA count.

```text
B = min(device_global_rate, s*per_SM_global_rate)
C_phase = sum_engine ceil(q*operations_per_CTA/engine_rate_per_SM)
G_phase = sum_nonempty(read,write) [startup + ceil(a*bytes_per_CTA/B)]
S_phase = ceil(q*SMEM_bytes_per_CTA/SMEM_rate_per_SM)
Y_phase = q*barriers_per_CTA*barrier_cost
T_plan = kernels*launch_cost + sum_round,phase(C_phase+G_phase+S_phase+Y_phase)
```

There is no overlap credit: all memory is unhidden in this serial model.
Compute and SMEM service stay shared per SM; global bandwidth stays shared
device-wide. Launch is charged once per kernel, not once per CTA or phase.
Admission, rounded reservations and partial rounds are recorded for every
kernel. Costs include the partial-reduction workspace traffic and merger.

This is complete **within the declared serial model**, not a complete native
GPU prediction: caches, sectors/banks, instruction/control overhead, register
spills, native occupancy, special-function latency chains, queue behavior,
barrier implementation and actual overlap remain unmodeled. All those facts
must be supplied or marked unknown before production selection. The prototype
does not export its numbers to CandidateCostIR or the GEMM selector.

Base-profile results (memory includes GMEM service/startup and SMEM traffic):

| Case/plan | Compute | Unhidden memory | Sync | Launch | Total | Rounded SMEM/CTA | Workspace |
| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| GEMM G16 | 512 | 6696 | 96 | 40 | 7344 | 2048 | 0 |
| GEMM G32 | 512 | 5544 | 24 | 40 | **6120** | 4096 | 0 |
| Reduction R1 | 32 | 2090 | 6 | 40 | 2168 | 128 | 0 |
| Reduction R16 | 9 | 600 | 30 | 80 | **719** | 128 | 64 |
| Softmax Sretain | 21 | 169 | 12 | 40 | **242** | 128 | 0 |
| Softmax Sreread | 38 | 338 | 12 | 40 | 428 | 128 | 0 |
| Normalization Ncenter | 51 | 2130 | 12 | 40 | **2233** | 128 | 0 |
| Normalization Nwelford | 638 | 1597 | 6 | 40 | 2281 | 128 | 0 |

The same evaluator produces meaningful changes without an operation-name
selection rule. In `memory_slow`, Nwelford wins 13033 versus Ncenter 16569;
in `division_slow`, Ncenter wins 2233 versus 4008. In `launch_heavy`, R1 wins
6224 versus R16 8831. Thus fewer passes or more CTAs is not inherently better.
All eight plans remain legal in the four timing scenarios. Separate negative
tests cover numerical permission, zero residency and retained-row register
rejection. These are hypothetical model preferences, not measured speedups.

## 8. Reproduction And Acceptance

From the checkout root:

```sh
python3 -m unittest discover -s doc/research_library/building_block_planning -p 'test_*.py' -v
python3 doc/research_library/building_block_planning/research_demo.py --output artifacts/ai_optimization/research-building-blocks/run-001
```

Use a fresh output directory; the tool refuses to overwrite an existing one.
`comparison.log` begins with `What it summarizes`, contains compact positional
cost/decision rows, and is explicitly not a compiler `.t` trace. `results.json`
retains input ledgers, every rounded resource quotient, residency round,
component, winner and resolved scenario definition. `inputs.json` fingerprints
the script and profile. Reproduction is deterministic apart from irrelevant
filesystem location; no runtime timing is collected.

R0 acceptance: 21 tests, 32 candidate/scenario comparisons, 16 decisions and
nine host numerical checks pass. Tests include hand-calculated cost/rounding
oracles, shared-throughput accounting, scratch/permission rejection,
non-mutation, missing completion, multiple consumers, generation ordering,
cyclic dependencies, state merge and stable shifted Softmax. The event checks
prove only small ordering predicates, not a general allocator or CUDA model.

Accepted research evidence (not a compiler receipt):

```text
/Users/shinmingliu/open64/artifacts/review/ai-optimization/research/building-block-planning/sha256-96d85ef9c2bbfb3147f0388786befd83efb5ec20816062743d6e4aa26c2cbba7/
```

The primary artifact is `results.json`; `comparison.log`, exact source/profile
snapshots, test output, commands, input hashes, receipt and `SHA256SUMS` are
retained with it. Independent replay from those snapshots must match both
results and comparison log before acceptance. No `.B`, `.T`, CUDA or native
timings are claimed or fabricated for this research-only run. The impact
selector classifies these paths as documentation; the new host code receives
its own focused tests in addition to hygiene, with no backend producer rerun.

## 9. Follow-On Research And Compiler Gates

| Gate | Deliverable | Status |
| --- | --- | --- |
| R0 | Both-layer worked cases and reproducible declared-model comparisons. | Research demonstration complete; native/WHIRL claims excluded. |
| R1 | Capture representative inputs, review native operator census; derive characterization instead of supplying ledgers. | Open. |
| R2 | Shared state/proof contracts, deterministic alternative generation, guards and strict-FP rejection. | Open. |
| R3 | General lifetime conflict/packing and symbolic event scheduling, including alias/progress negatives. | Open. |
| R4 | Integrate real target data, bounded profitability and selected-plan WHIRL; reopen/diff and emit reviewable CUDA. | Open; existing bounded GEMM does not imply other cases. |
| R5 | GPU correctness, resource reports, calibration and measured baseline comparison. | Open; requires hardware. |
| R6 Attention | Validate compositional state reformulation and sequential online vs partition/merge execution. | Open research extension. |
| R7 MoE | Validate dynamic/ragged domains, routing ownership, duplicate-token weights, indirect aliasing and guarded runtime scheduling. | Open research extension. |

Attention: for a fixed query and disjoint KV subsets, `(m,l,u)` summarizes
maximum score, scaled exponential sum and scaled weighted-value sum. Merge
by common maximum with corresponding rescaling and finish with `u/l`.
Handle empty/masked subsets and numerical policy explicitly. Compare retained
intermediates vs online state, then parallel partial states including merge,
workspace and launch/communication. FlashInfer exposes a composable attention
state interface [R5]; it is not evidence that our compiler discovers it.

MoE: characterize routing/top-k separately from expert computation and
weighted combine. Compare packed grouped expert work vs block-sparse work
only when both preserve routes, multiplicity, weights and output ownership.
Include routing/permutation, workspace, imbalance, metadata, gather/scatter,
combine and communication costs, not merely expert GEMM time. Runtime route
counts are guarded facts, not compile-time constants. MegaBlocks supplies a
block-sparse formulation precedent [R6]; its training results do not certify
Open64 inference kernels.

## 10. References And Attribution

- [R1 Colleague's proposal](https://chatgpt.com/s/t_6ad72f513a208191b65ba17bd33c5fe6): Work Partition, Resource Lifetime, Execution Events.
- [R2 Neptune, PLDI 2026, especially section 4.2](https://arxiv.org/html/2510.08726): correction analysis and its scope.
- [R3 Online normalizer calculation for softmax, 2018](https://arxiv.org/pdf/1805.02867): stable normalizer reformulation.
- [R4 Chan, Golub and LeVeque, 1983](https://doi.org/10.1080/00031305.1983.10483115): variance algorithms and numerical analysis; our state merge follows the established pairwise-statistics identity.
- [R5 FlashInfer attention-state tutorial](https://docs.flashinfer.ai/tutorials/recursive_attention.html): composition across KV subsets.
- [R6 MegaBlocks, MLSys 2023](https://proceedings.mlsys.org/paper_files/paper/2023/hash/5a54f79333768effe7e8927bcccffe40-Abstract-mlsys2023.html): block-sparse MoE formulation.
- [R7 Welder, OSDI 2023](https://www.usenix.org/conference/osdi23/presentation/shi): tile-graph reuse planning.
- [R8 Nautilus, April 2026 preprint](https://arxiv.org/html/2604.14825): automatic high-level scheduling and tile lowering.

All fixture sizes, synthetic coefficients, ledger recipes, comparisons and
Open64 adoption gates are our research design, not published performance
results or implementations attributed to these sources.
