# Intra-kernel memory/compute diagnostics

Status: optional collector/exporter/HTML viewer implemented; real CUDA/Nsight
Compute validation is outstanding. Host tests use synthetic vendor-API objects
and mocked orchestration, and do not certify hardware behavior.
SPEC.md remains the agreed performance-testing baseline.

## Recommended first method

Use hardware performance-monitor (PM) sampling in a separate Nsight Compute run,
with no changes to candidate or baseline source. Start with the smallest useful
metric group that can be collected together in one pass. Query available metrics
on the actual GPU/tool version; record unavailable metrics as unsupported.

Select an appropriate compute activity metric (Tensor Core for MMA kernels,
FP32/SIMT for the shipped FMA examples), and memory activity (prefer a supported
async-copy/TMA metric when relevant; otherwise DRAM/L2 throughput as a proxy).
Add source/warp-stall sampling only if the first pass cannot explain the behavior.
Separate MLA score MMA, softmax, and value MMA when source attribution is available;
Tensor Core activity alone cannot represent all MLA computation.

Nsight Compute supports timestamped PM samples and a Timeline page. The generic
PmSampling section can collect multiple passes. Do not assume that selecting it
alone satisfies a one-pass overhead budget. Request a small compatible group,
record pass count/group, and flag multi-pass data instead of treating different
replays as simultaneously observed. Disable optional warp sampling when the
installed tool supports doing so and only PM timelines are needed.

Choose the coarsest sampling interval that resolves the behavior of interest.
Interval units and minimums depend on architecture. A kernel shorter than a few
sampling intervals cannot yield a detailed internal timeline; report insufficient
resolution rather than inventing finer bins. Repeating launches can characterize
repeatability but does not recover sub-interval stages inside a single launch.

## Display and interpretation

Use the scaffold HTML viewer or native Nsight Compute Timeline page. The viewer
shows two aligned tracks per kernel: compute activity and memory
activity, with real sample intervals, scope, pass identity and missing data.
Compare corresponding baseline/candidate cases in separate panels. Preserve the
native .ncu-rep and raw metric values/units.

Simultaneously nonzero memory and compute samples are evidence of aggregate
coactivity, not an exact byte-transfer/compute overlap percentage. Activity can
come from different SMs, CTAs or instructions within the same sample interval.
DRAM activity also omits L2 hits, and global-memory traffic is not automatically
TMA staging traffic. Only co-recorded compatible metrics support simultaneous
sample interpretation; aligned metrics from distinct replay passes are estimates.
Do not label coactivity as per-tile or per-CTA overlap.

## If stage-level detail is necessary

Use a separate instrumented build and sparse recording by a selected CTA/warp
for a few tile iterations. Emit bounded binary records to a preallocated buffer
and copy them out after execution; no device printf, per-element logs or new
synchronization. Record memory-copy issue, existing wait entry/exit, compute
issue, and existing compute-wait completion observations.

Issue-to-observed-completion is an envelope, not the exact hardware transfer
interval; asynchronous MMA issue time is not MMA execution duration. Instrument
existing completion observations rather than introducing waits that destroy
overlap. Selected-CTA traces are not device-wide measurements. Compare timing
and resource use of instrumented/uninstrumented builds separately to assess
perturbation. This requires a future optional diagnostic adapter; do not enlarge
the benchmark ABI to require instrumentation from every compiler submission.

## Integration into the server

1. Complete ordinary correctness and unprofiled baseline/candidate timing first.
2. Select a small number of representative datasets or a detected regression.
3. Profile baseline and candidate in separate diagnostic executions, recording
   exact binary, case, GPU, tool, cache policy and metric identities. Never import
   host/CUDA-event timings from an Nsight Compute run into performance results.
4. Store diagnostics/<case>/<baseline|candidate>/ with native reports and a
   metadata record: method, scope, requested/actual interval and units, pass
   groups, metric definitions, dropped/merged samples and resolution limitations.
5. Add only an optional reference to these artifacts in result.json. A profile
   failure does not invalidate an otherwise completed benchmark; label diagnostic
   status separately. No profiler is launched in default evaluation.

Optional case result field (emitted for selected datasets):

```json
{
  "diagnostics": {
    "status": "collected",
    "artifact": "diagnostics/long-context/index.json",
    "timeline": "diagnostics/long-context/timeline.html"
  }
}
```

No universal overhead percentage is promised. Sampling collection, buffer traffic,
tool setup and replay all have costs. Single-pass, few-metric hardware sampling
is the recommended low-intrusion starting point, not a proven minimum for every
GPU/kernel. Default evaluation has zero incremental profiling overhead.

## Implemented execution and files

`run.py --profile-pm` persists the unprofiled result first. It selects explicit
`--profile-case` values or the passed case with largest useful-FLOP count, then
calls `pm.py` without giving it the benchmark result object. Only diagnostic
fields are added afterward. A failed diagnostic leaves benchmark status,
acceptance, timings, FLOPs and all ratios unchanged.

`pm.py` discovers `ncu`, its controls, and PM-supported full metric names. It
checks import of the optional ncu_report module in a separate interpreter. It
profiles baseline/candidate independently with profile-from-start disabled,
kernel replay, cache-control none, clock-control none, one maximum PM pass and
warp sampling disabled. Default interval is 20000 architecture-specific units.
The profile runner checks the benchmark GPU UUID and validates before/after
the captured invocation. No profile times enter performance reporting.

`pm_report.py` uses NVIDIA's report API to verify exactly one PM group, both
requested metrics in that group, and one collection pass for each kernel. It
extracts actual interval, workload timestamps, raw timestamped samples and
available merge counts. Missing proof or additional passes reject the diagnostic;
there is no multi-pass fallback. Dropped samples are unknown when the report
does not provide a count, never assumed zero.

`pm_view.py` renders standalone HTML/SVG, without third-party packages or network
dependencies. It shows sample points without interpolation, warns about sparse
data, and separates kernels rather than merging replay timelines. JSON retains
pre/post-kernel samples; display clips by measured workload bounds. Samples are
not context-filtered in this minimal exporter: isolated GPU execution is required
for attribution. See README.md for commands and CLI controls.

Single pass is verified **per kernel**. An MLA implementation containing three
kernels produces three individually checked timelines, not one measured continuous
pipeline. Metric compatibility varies by GPU; selecting two individually supported
metrics does not guarantee they fit together in one pass. Such a pair fails the
diagnostic explicitly. Hardware/tool validation remains necessary.

## Primary references

* [Nsight Compute PM sampling, support and overhead](https://docs.nvidia.com/nsight-compute/ProfilingGuide/index.html#pm-sampling)
* [Nsight Compute Timeline display](https://docs.nvidia.com/nsight-compute/NsightCompute/index.html)
* [CLI metric queries and sampling controls](https://docs.nvidia.com/nsight-compute/NsightComputeCli/index.html)
* [Async-copy/TMA pipeline guidance](https://docs.nvidia.com/nsight-compute/ComputeTriage/)
