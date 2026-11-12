# Minimal CUDA compiler evaluation scaffold

Version 2 uses the same results format and offline validator as the Open64 MLA
Q7 package. The minimal compiler ABI and server-owned datasets/oracles are
preserved. Profiling remains off by default. See SHARED_RESULTS.md.

After a benchmark, validate its `result.json` with the exact original packet:

```bash
python3 validate_gpu_results.py --result runs/mla/result.json \
  --packet . --expected-packet-sha256 "$(cat PACKET_ID)" \
  --output /path/to/fresh-review
```

The review output must be outside the immutable packet. Check-only output
validates the corpus but remains INCOMPLETE as GPU evidence. Original v1 fields
remain available in `extensions.scaffold_v1` and `result.legacy.json`.

[SPEC.md](SPEC.md) records the agreed testing baseline for all future changes.
[PROFILING.md](PROFILING.md) describes the optional single-pass PM sampling and
HTML timeline path. Profiling is off by default; real GPU validation is pending.

Two workload headers, two example submissions, deterministic BF16 input files,
FP64 golden outputs, and one CUDA/server harness. Python uses only the standard
library (Python 3.9+). GPU execution requires Linux, a C++17-capable CUDA toolkit
(CUDA 11+), and an NVIDIA GPU supporting the selected target (default SM80+).

```sh
python3 server/data.py all
python3 server/run.py submissions/gemm
python3 server/run.py submissions/mla
```

Each command prints the resulting `runs/<kind>/result.json` path. Both baseline
and candidate are measured for every case in the same executable, CUDA context,
GPU and session. The result contains no invented measurements. Without CUDA:

```sh
python3 server/run.py submissions/gemm --check-only
python3 server/run.py submissions/mla --check-only
python3 -m unittest discover -s tests -v
```

`--check-only` verifies the package and corpus and emits `status: "not_run"`,
null timings/ratios, and `decision: "inconclusive"`. The checked-in run results
were produced in this mode; they are not GPU benchmark results.

## Compiler package and ABI

`submissions/<kind>/` contains `manifest.json`, `candidate.cu`, `baseline.cu`.
The server supplies `server/include/<kind>/workload.h`. The manifest selects an
immutable workload and suite, target SM, compile flags, and libraries. This
minimal server accepts package-local `baseline.source` and `cudart` only.
The same flags apply to baseline and candidate. Change `target_sm` for your
target and rebuild; the server records the actual device and compiler identity.

Both implementations export only:

```cpp
extern "C" size_t workspace_bytes(const WorkloadArgs*);
extern "C" cudaError_t run(const WorkloadArgs*, void*, cudaStream_t);
```

`WorkloadArgs` is a host descriptor containing device pointers. `run` passes its
contents by value to kernels, enqueues only on the supplied stream, does not
allocate/synchronize, and initializes scratch before reading it. Inputs are
immutable, outputs do not alias inputs, and all output elements must be written.
Required per-call packing/preprocessing belongs inside `run`. Workspace is
allocated once per case and its size is reported. No persistent preprocessing
or cross-call result cache is permitted. Server compilation renames the two
implementations' exported functions to link both into one process.

## Workloads and corpus

GEMM: row-major `D = alpha * op(A) * op(B) + beta * C`, BF16 storage and FP32
accumulation, separate immutable C and output D, arbitrary supported row strides,
transpose bits, no fused epilogue, single GEMM. Cases: 16³, 128³, and three padded
non-square boundary cases covering TT, TN, NT (NN is covered by the square cases).
The candidate uses shared-memory tiles; the baseline is a naive CUDA GEMM.

MLA: absorbed dense paged decode, one query token per sequence, BF16 storage and
FP32 arithmetic. Query and KV vectors are concatenated `[latent | RoPE]`.
`score[t] = scale * dot(q, kv[t])`; apply softmax over the valid context and
return the weighted sum of the latent portion of KV. RoPE application and input/
output projections are excluded. Cases use 4 heads, latent dimension 32, RoPE
dimension 8, page size 16, context lengths 32, 2048, and ragged [17,65]. Physical
pages are shuffled; unused page entries are -1. The baseline recomputes scores
per output dimension; the candidate computes/reuses scores through workspace.

These implementations demonstrate the interface, not production performance.
The MLA dimensions are deliberately small for cheap CPU oracle generation.
For production dimensions or context lengths, add a new versioned server suite;
no ABI change is needed. Prefill and batched GEMM are outside these v1 contracts.

The server stores corpus files in `data/<kind>/<case>/`:

* BF16 tensors: little-endian uint16 bit patterns, rounded to nearest even.
* Page tables/lengths: little-endian int32.
* `golden.bin`: little-endian float64, logical output order without GEMM padding.
* `case.json`: shape parameters, seed, tolerance, oracle identity and useful FLOPs.
* `params.txt`: the same parameters in the fixed order consumed by the native harness.

`index.json` records SHA256 hashes for every case artifact. Inputs are generated
once with fixed seeds; exact files, not RNG behavior, are the permanent fixtures.
Golden outputs use the stored BF16 values with Python float64, `math.fsum`, and
stable softmax. The tests check independent small analytical examples. Elementwise
comparison is `abs(actual-golden) <= 0.003 + 0.02*abs(golden)`; these demonstration
tolerances are specific to the supplied small suites. Inputs and GEMM output
padding are checked for modification. Regenerate explicitly with:

```sh
python3 server/data.py all --force
```

Changing cases, references or numerical rules requires a new suite/workload ID
for durable benchmark history. The manifest references server assets; compiler
submissions do not supply golden outputs or acceptance tolerances.

## Timing and result.json

After validation and 3 warmups, collect 20 wall-clock samples per implementation,
alternating order. Each sample drains previous work, starts a host steady clock,
calls `run`, synchronizes the supplied stream, then stops the clock. Allocation,
output reset and validation are excluded. Validate the final measured output for
each implementation in each timing phase, outside timing. Separate CUDA-event samples measure device time.
Their interval includes the complete submitted operation; for multi-kernel MLA
it can include device idle gaps between launches. They are not hardware counters.

Every case includes:

```text
wall_time_ratio = candidate.wall_us / baseline.wall_us
effective_tflops = useful_flops / (device_us * 1e6)
```

`wall_us`/`device_us` are medians; `wall_mad_us` is median absolute deviation.
Ratio < 1 means improvement. GEMM useful FLOPs = 2MNK. MLA useful FLOPs =
2 * heads * sum(context_lengths) * (2*latent_dim + rope_dim). Both exclude scalar
scaling, softmax and other non-matmul work; FMA counts as 2. The same numerator
is used for baseline/candidate, even if actual arithmetic differs.

Failed/skipped cases have null timings and a null ratio plus a reason.
`decision` is accepted only if correctness passes and **every** case's ratio is
below `1 - max(min_improvement, 3*(baseline_MAD+candidate_MAD)/baseline_median)`.
Otherwise the decision is inconclusive, or rejected on correctness failure.
This simple noise heuristic is not a statistical confidence interval.

```sh
python3 server/run.py submissions/mla --samples 50 --warmup 5 \
  --min-improvement 0.03 --timeout 300 --output runs/mla/result.json
```

Builds are cached by package/header/harness/compiler identity in `.build/`;
baseline timings are freshly measured every run. Results include build commands,
compiler version, device UUID, driver/runtime, source/corpus identities, workspace,
correctness and performance. Build/runner diagnostics remain in log files.

This is a local testing-service prototype. Run untrusted compiler submissions in
an isolated GPU worker/container; this package does not implement a multi-tenant
sandbox. GPU sanitizer coverage, concurrent clients and clock/power management
are extensions, not part of this minimal example.

## Optional memory/compute timeline

Run the ordinary unprofiled benchmark first, then collect optional diagnostics:

```sh
python3 server/run.py submissions/gemm --profile-pm --profile-case larger
python3 server/run.py submissions/mla --profile-pm --profile-case long-context
```

Omit `--profile-case` to select the passed dataset with the largest useful-FLOP
count. Repeat the flag to select more datasets. Every dataset keeps its ordinary
unprofiled wall-clock ratio regardless of diagnostic selection or failure.

Profiling requires a recent `ncu` with PM controls and NVIDIA's `ncu_report`
Python module (bundled in Nsight Compute's extras/python directory). If necessary:

```sh
python3 server/run.py submissions/mla --profile-pm \
  --ncu /opt/nvidia/nsight-compute/ncu \
  --ncu-python-path /opt/nvidia/nsight-compute/extras/python \
  --pm-interval 20000
```

Use `--ncu-python` if the report module requires a different Python interpreter.
The interval is cycles or nanoseconds depending on architecture; 0 selects auto.
Actual values/units are recorded from the report. Optional overrides
`--pm-memory-metric` and `--pm-compute-metric` select exact, query-discovered
metric names, for example a supported Tensor Core metric for an MMA candidate.
Default compute selection targets the shipped FP32 FMA examples, falling back
to SM throughput (a broader proxy); default memory selection uses DRAM/L2 proxies.

Open `runs/<kind>/diagnostics/<case>/timeline.html`. Each role also retains its
native `.ncu-rep`, exported samples, commands and logs. `result.json` links these
artifacts. Both implementations are collected separately after the benchmark.
The runner verifies the same GPU UUID, warms up/validates, and captures only one
operation invocation through cudaProfilerStart/Stop. Multi-kernel MLA profiles
each constituent kernel; **single pass means per kernel**, not one GPU launch
or a reconstructed continuous execution of the full operation.

The collector does not select the generic multi-pass PmSampling section. It caps
PM collection at one pass, disables optional warp sampling, and requires report
evidence that both metrics share one pass group with one total collection pass.
Missing/unsupported metrics, missing metadata, extra passes, tool failure or
timeout affect only diagnostic status. No automatic multi-pass retry occurs.

The viewer shows raw sampled points clipped to each kernel's measured bounds,
with independent value scales and a shared within-kernel time axis. It reports
insufficient resolution when fewer than three samples lie inside the kernel.
It does not compute an exact overlap percentage or interpolate across gaps.
Raw PM samples are device-wide, so use an isolated GPU worker. Native reports
remain available for NVIDIA's context-aware timeline view.

`examples/pm-fixture/timeline.html` is a clearly labeled synthetic host-test
preview, not a GPU measurement. The implementation has host/mock integration
coverage; actual CUDA compilation, hardware sampling and vendor API compatibility
must still be verified on a supported GPU server.
