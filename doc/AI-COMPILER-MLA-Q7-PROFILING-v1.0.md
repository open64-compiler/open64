# Q7 diagnostic performance measurements v1.0

November 11, 2026. Additive to Q7 qualification, return collection and results
validation. Execution distribution v1.3; receiver distribution v1.1. Owners,
successor, reviewer and approving lead remain TBD. NVIDIA execution is pending.

## Purpose and isolation

Answer three questions separately: what compute capacity is waiting for data,
how busy each data path is, and where the awaited data originates. Counter
observations alone do not establish a causal partition of elapsed time.

The workflow runs unit tests, generated-source compilation, existing numerical /
sanitizer / unprofiled performance qualification, then a separate diagnostic
profiling process. Profiling never calls the compiler, changes a selected plan,
performs search, emits timing samples or updates a cost model. Existing Q3
bindings, source fallback policy, WHIRL COMMENT and generated kernels remain
unchanged. AOT selection and JIT remain deferred.

`run_mla_q7.py --mode profile` validates one fixed case, exercises the existing
lifecycle/guard checks outside capture, then executes exactly one clean request.
`cuProfilerStart` / `cuProfilerStop` bracket that request including its Q3
completion. Setup, initial validation and output/reference/consumer reads are
outside the capture. No CUDA timing events are created in this mode. The host
function's internal elapsed-time return is discarded, never published as a
benchmark. Each diagnostic request must still pass all six outputs against all
three references, immutable inputs, guards, publication and consumer checks.

Default NCU policy is kernel replay, cache-control all, clock-control none,
profile-from-start off. These are deliberately explicit profiling conditions;
they do not represent application warm-cache behavior or end-to-end timing.
Replay can alter overlap and cache state. Native reports and exact commands
are retained. Profiling is serialized after timing, never concurrent with it.

## Evidence record

Schema: `open64.mla-q7.profiling.v1`. Every normalized numerical record contains:

| Field | Contract |
| --- | --- |
| value | Finite nonnegative number; null exactly when evidence is unknown |
| unit | Base unit or explicitly labeled percentage; rates use byte/second |
| evidence | measured, derived, proxy, estimate, unknown |
| sources | Exact counter names or raw snapshot files |
| denominator | Active warp cycles, scheduler active cycles, profiled kernel duration, device collection interval or another explicit reference |
| scope | Kernel, device-wide collection, or host interface; no silent attribution across scopes |
| reason | Explanation of unknowns and limits of interpretation |

Measured means a reported hardware counter observation, not independent hardware
attestation. Derived means arithmetic on measured inputs. Proxy means a measured
quantity that is related to, but does not equal, the requested performance cause.
Estimate requires a documented model; this release does not invent a model for
causal waiting time and emits no such numerical estimates. Unknown is not zero.

## Group 1: compute waiting

Collect supported scheduler issue-active, eligible-warps, Tensor/FP32 pipeline
activity and long/short-scoreboard counters. Exact metric names are in
`mla_q7_profile.py:COUNTERS`; only names returned by the installed NCU capability
query are requested. Do not guess aliases from approximate names.

Issue-inactive percentage is `100 - issue_active_pct` with scheduler-active-cycle
denominator. This is unused issue capacity, not idle SM time or causal data-wait
time. Long-scoreboard share is labeled a memory-dependency proxy with an active-
warp-cycle denominator. Short-scoreboard share is an MIO dependency proxy,
including non-memory MIO instructions. It must not be added to long-scoreboard
and labeled pure memory waiting. A stalled warp can overlap execution by another
warp. Pipeline activity also has a distinct denominator.

`exposed_data_wait_pct` and `unknown_wait_pct` remain null: aggregate counters
cannot establish mutually exclusive causal idle-time attribution. Never derive
data waiting as 100 minus compute utilization or multiply unrelated stall and
issue percentages.

## Group 2: data paths

Collect supported HBM read/write bytes, L2 bytes, HBM/L2 throughput utilization,
and device-scoped NVLink TX/RX and PCIe read/write bytes. NCU device counters do
not become kernel-exclusive because they appear in a kernel report. Link byte
observations are not summed across replayed kernels into application traffic.

HBM byte/s is `(read_bytes + write_bytes) * 1e9 / profile_duration_ns`.
L2 byte/s uses its own byte count and the same profiled duration. Missing inputs
or zero duration produce unknown rates. Tool utilization is labeled with the
tool's sustained-peak convention; it is distinct from calibrated payload
bandwidth efficiency. No undocumented physical link peak is assumed.

`time_bandwidth_saturated_pct` remains null. Average utilization does not measure
the fraction of time above a saturation threshold. That requires a time series,
declared threshold/reference bandwidth and sufficient sampling resolution, which
this release does not fabricate.

The profile child also snapshots `/sys/class/net/*/statistics/{rx,tx}_bytes`
excluding loopback before and after the clean request. Derived rates use the
host monotonic snapshot interval. Scope is host interface/all processes, not
GPU or request ownership. A missing interface, missing counter or reset becomes
unknown. An unavailable sysfs surface is an explicit empty observation, not
zero network traffic. These counters may be noisy for a short request and may
exclude RDMA traffic; no remote-GPU traffic inference is made from them.

## Group 3: data origin and exposed wait

Retain `nvidia-smi -L`, `topo -m` and NVLink status observations. Board and node
placement are topology facts; transport type alone does not identify placement.
Current fixed Q6d fixtures use owned local-GPU buffers and have no peer/remote
producer. This is a runtime fixture contract, not a measured wait percentage.

`local_gpu_exposed_wait_pct`, `peer_gpu_exposed_wait_pct` and
`remote_node_exposed_wait_pct` remain null. Generic scoreboard counters do not
identify the owner of the data. Link traffic does not prove a consumer stalled,
and total communication duration is not exposed waiting when compute overlaps.

`--profile-timeline` additionally runs a separate Nsight Systems CUDA/NVTX trace
of the same clean request using the profiler API capture range. Retain its
native `.nsys-rep` and original child checks. No automatic critical-path analysis
is claimed. Multi-GPU/NCCL/network workloads, cross-node trace synchronization,
GPU-aware NIC accounting, calibrated link saturation and causal attribution
require a future workload/serving contract. Unknowns preserve that boundary.

## Commands and returned files

One command on the recipient's Linux capability-9.0 machine:

```bash
bash RUN_COMMANDS.sh
```

Optional independent timeline and a smaller diagnostic subset:

```bash
bash RUN_COMMANDS.sh --profile-timeline \
  --profile-case dense_s8_decode--async \
  --profile-case dense_s8_decode_long--async
```

Numerical/sanitizer/timing coverage remains the complete existing matrix.
`--profiling off` explicitly skips diagnostics. Diagnostic failure is recorded
separately and never replaces qualification's exit code. The workflow collects
partial/failure evidence through the existing finalizer. No-GPU or prior
qualification failure skips profiling. SIGKILL/power loss cannot be finalized.
Each tool owns a process group and cleanup stops descendants on timeout/signal
before collection. Tools are optional for diagnostics; query or permission
failure is not numerical success or a zero counter.

The additional return root is `results/04-profiling/` in the standard workflow:

```text
report.json                         # schema, packet binding, commands, status
ncu-version.stdout / ncu-query.stdout / topology.stdout / link-status.stdout
<case>/metrics.csv                  # raw names, base units, values, launch IDs
<case>/counters.ncu-rep              # native profiler report
<case>/execution/report.json         # actual GPU/reference/Q3 facts
<case>/execution/network-before.json / network-after.json
<case>-timeline/timeline.nsys-rep    # optional, separate process
```

When NCU is absent, a plain observation child can retain reference/Q3 checks and
host network snapshots. It emits no hardware-counter summary. All files are
covered by the existing return archive manifest. The collector performs no GPU
workload, profiling, compilation or search; its environment capture is read-only.

## Receiver rules

The v1.1 receiver accepts earlier Q7 return formats. It authenticates the source
manifest and consumed code metadata and inspects the tar without extraction or
execution. It independently parses raw CSV, rejects malformed/duplicate rows,
scaled or incompatible units, negative/nonfinite/out-of-range values and wrong
launch names/order. Capture must match exactly the three trusted selected stage
entries. All derived values, units, scopes, evidence labels, unknowns and source
names must match a fresh recomputation; a supplied summary is never authority.
Child source/device/reference checks and profile-vs-timing/search isolation are
verified. Unsupported metrics remain unknown. Native binaries are hashed and
retained, not executed or independently decoded by this receiver.

`summary.json` and `summary.md` have a separate profiling section. Status is
MEASURED_WITH_UNKNOWNS, UNAVAILABLE, NOT_REQUESTED_OR_LEGACY or INVALID. Missing
diagnostic tools/CSV/native reports produce visible findings independently of
numerical/timing qualification. Contradictory or forged measurements fail the
review. Overall PASS_REPORTED means reported qualification evidence consistency;
it does not close unknown diagnostic causes. No speedup is claimed without an
appropriate matched baseline; this packet still has none.

## Validation and remaining gates

Focused CPU tests cover rate/issue arithmetic, exact counter discovery, unknowns
versus measured zeros, unit/range/duplicate errors, false causal attribution,
forged labels, stage identity, missing evidence, ambient network scope and resets,
timing/search isolation, and workflow finalization after profiler failures.
Fixtures are synthetic protocol evidence, never GPU qualification. A new version
of the full execution archive preserves generated CUDA/PTX/WHIRL/native images,
runtime and input/oracle bytes. Compiler production is not rerun for a harness
change. NVIDIA capture, runtime counter availability and actual correctness,
race and timing results remain pending on real hardware.

Primary semantics: [Nsight Compute profiling guide](https://docs.nvidia.com/nsight-compute/ProfilingGuide/),
[NCU CLI](https://docs.nvidia.com/nsight-compute/NsightComputeCli/),
[Nsight Systems guide](https://docs.nvidia.com/nsight-systems/UserGuide/).
