# H7 H20 package and reported qualification v1.0

H7 integrates fixed H4/H5/H6 selected products into the existing combined GPU
test package, shared receiver and GS1/GS2 service. It changes no compiler search,
WHIRL lowering, generated kernel arithmetic, opcode/tensor-trait/native format,
Q3 runtime ABI or serving/AOT selection mechanism. H20 96 GB and 141 GB are
separate explicitly targeted source packets. Personnel roles remain TBD.

## Main choice and scope

Extend the outer combined catalog to `open64.gpu-test.suite.v2` with one optional
`h20` inner packet and four independent shared-result profiles. V1 catalogs keep
exactly their existing three profiles and original behavior. Earlier scaffold
and Q7 inner packets, all their source-adoption scripts, data, oracles and
manifests remain byte-identical. Preserve historical outer tools under
`prior-tools/`; the new outer tools own the additive integration. Original
inner shared-module versions remain pinned by their original identities; the
new canonical outer validator adds H7 without rewriting those distributions.

| New profile | Frozen selected products / numerical boundary |
| --- | --- |
| h20-gemm.selected-f32.v1 | H4 synchronous aligned and ragged F32 GEMM, selected B scalar retention and original shared schedule; independent binary-exact finite input/golden corpus |
| h20-mla.selected-f32.v1 | H5 short/long full absorbed F32 operation, materialized projections, grouped-head shared panels, split tuple/stable merge, output and LSE; seven original retained FP64 reference variants/tolerances (mask, tails, empty-first, equal/large-score) |
| h20-layernorm.selected-f32.v1 | H6 selected C1/C2 unit, 65 and 257 feature widths; original LN-REF-1 centered ordered binary32 oracle, exceptional payloads and NaN class |
| h20-reduction.selected-f32.v1 | H6 direct row/thread and 32/128-thread cooperative panels, width/row tails; ascending +0 F32 fold, exceptional values and NaN class |

There are eleven selected case programs. Preserve CUDA/COMMENT and genuine
native B/T, original launch evidence, source numerical contracts and immutable
upstream receipt identities. Preparation authenticates each reused artifact.
The MLA **Low-WHIRL device CUDA** is compiled; its separate serial optimized
algorithm-review source cannot substitute for executable mapped GPU kernels.
The new profiles execute the selected CUDA-to-NVCC route; the original Q7
native PTX/cubin route is preserved separately and is not relabeled as new
H5 native-PTX hardware qualification. Only allocation/launch/IO metadata and
include-only compilation wrappers are
created by H7. No candidate algorithm template or emitter-side decision exists.

This first H7 packet executes synchronous H4 GEMM. The H4 TMA descriptor/launch
provider remains an explicit additional qualification gate; the original Q7
quantized/TMA/Q3 test packet and mandatory bulk/proxy sanitizer coverage remain
present and unchanged. General canonical discovery, new Reduction serving
launch providers, RMSNorm and wider algorithm catalogs retain their earlier
gates. The H7 private Driver adapter is a test owner, not a new production
Reduction or serving ABI.

## Explicit H20 admission and evidence bindings

The package preparer requires `--gpu-device h20_96gb` or `h20_141gb`; the outer
catalog and inner metadata must agree. The worker registers those capabilities
and serves them through GS2. Runtime does not choose an algorithm or switch to
another variant. All jobs use the same explicit CUDA-visible device 0 under
the existing physical-GPU lease/cancellation/drain contract.

Reuse Q7's actual Linux x86_64 CUDA Driver API >=12.8 / SM90 requirements. Before
allocating test buffers or submitting kernels, observe name, UUID, total memory,
SM count, grid/thread/register/shared limits and full-device/MIG state. Require
an H20 name plus matching variant admission interval, not merely SM90. Broad
80–112 GiB and 120–160 GiB intervals are explicit package capacity-screening
policy for 96/141 targets; they are not bandwidth specifications or inferred
SKU resources. A missing MIG observation, wrong name/capacity/UUID/ISA, unsupported
tool or unknown eligibility denies H7 execution and stays unavailable/incomplete;
an observed wrong device/variant is a binding failure.
The existing physical UUID/mask binding remains with the worker.

Every case preserves its exact compiler resource/execution/budget profile and
H3 model files/digests, original native source/launch trace, selected CUDA digest
and compiled cubin digest. The actual Driver queries each function's register,
static shared/local, maximum thread, PTX/binary and occupancy attributes. Require
selected grid/CTA/static shared to fit actual limits and at least one resident
CTA; compiled shared cannot exceed the selected reservation. Record planning
register estimates separately; an exceeded estimate requires a fresh compiler
rebind rather than silently claiming accurate costing.

The retained H3 profile/model inputs are synthetic fixtures. Even a successful
real run cannot certify their bandwidth, SM count, clock/cache/power assumptions
or prediction accuracy. Physical code/resource eligibility, observed timing and
model calibration are separate dispositions. Preserve observed limits beside
original bindings; do not overwrite them or call the compiler/search from H7.
Model qualification remains `UNQUALIFIED_SYNTHETIC_PROFILE` until separately
reviewed physical profiles and compiler bindings exist.

## Ordered execution gates

1. Authenticate the complete source packet, selected symbol/launch metadata,
   compiler profile/model bindings, corpus and numerical rules. `--check-only`
   performs these CPU checks, submits no CUDA work and reports not_run/null
   measurements. Success is not hardware acceptance.
2. AOT-compile only frozen selected CUDA under SM90, no fast math/contraction,
   gradual underflow, precise divide/sqrt flags. Retain exact commands, compiler
   diagnostics, wrapper/source and cubin hashes. This is not Open64 search or
   JIT tuning. Compilation happens outside all timing/profiling measurements.
3. In a separate child/context, validate every variant at 16/256-byte logical
   offsets. Inputs stay immutable; all allocation guards are checked. Scratch
   is left uninitialized so initcheck can detect missing initialization. Outputs
   receive a finite poison outside the operation. Return real output bytes,
   input hashes, raw guards, actual resources and selected launch identity.
4. Run memcheck, racecheck, initcheck and synccheck as separate bounded children.
   Reuse Q7's required bulk/proxy/shared options and clean-summary parser. A
   zero tool exit alone is insufficient: require actual GPU child work, the
   complete same variant/offset/binary matrix, valid outputs, no errors/warnings
   or missing instrumentation and clean race/error summaries. Absence cannot
   authorize timing. Failures stop only dependent H7 phases; other existing
   combined workloads continue under their historical rules.
5. Only after correctness/races/resources pass, measure persistent-buffer fixed
   requests. Allocation, upload, output poison, guards and oracle transfers are
   outside samples. Warm up first. A wall-only batch measures Python/Driver
   enqueue plus stream completion, including required operation stages and host
   API overhead, with **no CUDA event recording in that batch**. A separate CUDA
   event batch spans the complete selected operation and may include host gaps
   between kernels. Validate final outputs outside each batch. Retain raw arrays
   and recomputed shared count/median/MAD/p95; never pool different families.
6. Optional profiling runs afterward in fresh diagnostic children. Query actual
   NCU capabilities, capture only one fixed selected operation via profiler
   APIs, retain raw CSV/native reports and exact binary/case/GPU identities.
   Reuse Q7 counter meanings for compute activity/stall proxies, data-path byte
   rates/utilization and local/peer/remote unknown attribution. Counter replay
   uses kernel replay, cache-control all, clock-control none. An optional
   separate Nsight Systems capture is diagnostic only. Profile durations never
   enter wall/event arrays, ratios or compiler estimates. Unsupported metrics,
   permissions or profiler failures remain separate optional diagnostic status.

Use explicit finite sample/warmup/per-child budgets (samples <=10000, warmup
<=1000, child timeout <=86400 s), existing owned child process-group cleanup,
physical GPU leases and automatic outer result finalization. Stop/drain descendants
before snapshotting. Interrupted/partial raw reports are returned unchanged.
No source or alternate-kernel fallback is launched on uncertainty/failure;
existing compiler fallback remains frozen in its original artifact semantics.

## Shared results and receiver

Four new producer `h20-h7` envelopes use `open64.gpu-test.results.v1`. Each family
has its own workload/suite/corpus/source/session/environment, correctness, raw
wall/event samples and diagnostics. Source packet authentication binds the
server-owned reference/launch data. GEMM useful work is 2MNK; other full-operation
numerators remain unknown. No matched native GPU baseline is included in this
first extension, so baseline samples, ratios and speedup acceptance remain null.
Original scaffold same-GPU baseline/ratio contracts remain unchanged. Do not use
those BF16 datasets or CPU reference programs as native F32 denominators.

The existing combined collector snapshots `h20/` as `results/h20/`, including
build/correctness/sanitizer/timing/profiling and workflow evidence, and indexes
four new profiles. Its original always-attempted finalization/environment capture
and bounded archive/checksum publication remain. Collectors execute no GPU work.
The shared offline validator delegates H7 to `validate_h20_h7.py`; the combined
validator includes H7 in complete profile/workflow/GPU UUID checks. Keep trusted
code outside returned-data import paths. Never execute extracted files.

Independent receiver checks authenticate source/cubin/model identities,
recompare actual returned outputs with original goldens, verify input hashes and
all guards, check exact variant/offset/symbol/launch/resource census, reject
swapped GPUs, inspect sanitizer summaries and coverage, and join unprofiled
native/shared timing samples. Recompute counter summaries from original CSV;
recompare the saved final wall-batch and event-batch outputs and require exact
timing case coverage. Validate register-estimate dispositions against recorded
attributes instead of trusting a returned pass flag.
missing/contradictory profile evidence cannot masquerade as measured values.
Unsafe archive members/paths, tampered manifests and budgets retain the existing
scanner's INVALID handling. Missing GPU/tool evidence is INCOMPLETE; nonzero
correctness/resource/race evidence is FAIL. Package/receiver success and reported
consistency are not hardware attestation or personnel transfer approval.

With current synthetic compiler bindings and absent native matched baselines,
real correctness/race/resource gates and measured latencies can be reported,
but full hardware/model/performance acceptance intentionally remains INCOMPLETE.
H7 does not silently reclassify those gaps as complete GPU qualification.

## Usage and validation

Run the versioned combined packet with `bash RUN_COMMANDS.sh`; add
`--profiling auto` and optionally `--profile-timeline`. It automatically creates
one results tar plus checksum, including failure/incomplete evidence. Reuse the
existing `tools/validate_gpu_test_suite.py` with the original packet and external
PACKET_ID to review returned data. GS1/GS2 register the normal source archive;
job submission/idempotency/cancellation/result retrieval APIs are unchanged.

`prepare_h20_h7.py` takes authenticated H3/H4/H5/H6 receipts, the original
combined packet, explicit target and a fresh output directory. It preserves all
old inner files and scripts and outputs a new target-specific archive. Never
replace earlier releases or receipts. Exact preparation/execution commands and
all source/packet/archive digests belong in the H7 review receipt.

Focused tests cover device/capacity/partition mismatch, exceptional numerical
classes, source/binary/launch/resource and return corruption, missing/failed
sanitizer gates, profile/timing separation, check-only, v1/v2 composition,
partial collection, real extracted one-command/receiver flows and GS1/GS2
regressions. Offline NVCC cubin compilation proves compile compatibility only.
Actual H20 correctness, race checks, resource observations, timing, profiling and
physical model calibration require the server run; this development host cannot
certify them. AOT serving selection, PREOPT COMMENT repair and OpenCTI remain
deferred. Primary owner, successor, independent reviewer and approving lead
remain TBD; transfer acceptance remains blocked on assigned personnel/evidence.
