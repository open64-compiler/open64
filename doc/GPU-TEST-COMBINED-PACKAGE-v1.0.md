# Combined GEMM and MLA NVIDIA test package v1.0

November 11, 2026. One source archive, one run command, one automatic return tar.
This composes the aligned Testing scaffold v2.0 and MLA Q7 v1.4 packets without
changing their kernels, oracles, workload ABIs, shared result format or numerical
acceptance. Source and receiver are Python standard library only; Python >=3.9
is required by the existing scaffold. Roles remain TBD. GPU qualification pending.

## Source layout and identities

```text
Open64_GPU_Test_Suite_v1.0/
  RUN_COMMANDS.sh
  README.md / RELEASE_NOTES.md / COMBINED_PACKAGE.md / SHARED_RESULTS.md
  PACKET_ID / SHA256SUMS / suite.json
  tools/                          # combined orchestration/collector/reviewer
  scaffold/                       # unchanged aligned GEMM and BF16 MLA packet
  mla-q7/                         # unchanged aligned full quantized MLA packet
```

The outer manifest covers all supplied files, including inner manifests.
`suite.json` pins the exact inner packet identities and fixed directory names.
Authenticate the complete outer source packet before starting any workload.
The offline receiver authenticates the outer metadata and inner manifest
bindings, then each existing validator authenticates its consumed source data.
Earlier standalone distributions remain intact and usable independently.

## One command and execution policy

On the NVIDIA host, after checking the archive checksum and extracting:

```bash
bash RUN_COMMANDS.sh
```

The combined package contains all three previously aligned profiles:

| Job | Existing execution/acceptance |
| --- | --- |
| GEMM | All five BF16 GEMM datasets, baseline and candidate correctness/input immutability/padding, fresh same-GPU baseline, separate wall/device batches, useful FLOPs and candidate/baseline wall ratios |
| BF16 MLA core | All three short/long/ragged paged decode datasets, existing golden results/tolerances, same-session baseline and candidate, separate wall/device batches and ratios |
| Full quantized MLA/Q3 | All 14 fixed products, six outputs versus selected-source/PyTorch/scalar oracles, both address residues, guards, cache/scale versions, Q3 lifetime/publication/consumer checks, generated CUDA/PTX compilation and four required sanitizer lanes, then separate wall/event sample batches |

First run the existing scaffold host tests. Run GEMM and BF16 MLA in independent
processes, continuing to the next workload after an ordinary failure. For Q7,
retain its existing unit -> generated compilation -> qualification order; a Q7
phase failure skips its dependent phases. The outer runner invokes the same
authenticated Q7 worker entry points directly so it owns timeout/cancellation
boundaries and finalization. It does not launch a nested workflow that can start
another collector while its parent is stopping it.

All jobs run sequentially and use CUDA-visible device 0. Default visibility is
inherited. `--gpu 0` or `--gpu GPU-UUID` explicitly sets the child visibility
mask consistently for all jobs. The return records that mask; receiver review
also checks actual execution GPU UUIDs across profiles. Hardware requirements
for the complete run remain Linux x86_64, capability 9.0 (H100/H800), compatible
glibc/libstdc++ for the retained Q3 runtime, CUDA Driver >=12.8, CUDA 12.8.1
compile tools and Compute Sanitizer with required bulk/proxy coverage.

`--samples 20 --warmup 3` are the defaults. `--results-parent PATH` chooses a
writable output parent outside the source packet. Each invocation gets a new
`gpu-suite-run-*` directory; neither an earlier run nor a release is overwritten.
The first ordinary nonzero test status is preserved while independent work
continues. Catchable interruption stops active work and reaches collection.
Timeouts/signals stop owned child process groups before snapshotting evidence.
No in-process GPU recovery or source fallback is attempted by the wrapper.

The scaffold executes from an authenticated, fresh writable copy under the run
directory, keeping its source-relative `.build` cache outside the installed
packet. Original script bytes and identities are unchanged. Copy only listed
source files, not old caches, and retain this invocation's build logs/receipts
under `build-evidence/` in the return; compiled executables are not duplicated.

## Separate timing and optional profiling

Profiling is off by default. Enable existing independent diagnostics with:

```bash
bash RUN_COMMANDS.sh --profiling auto
```

This passes `--profile-pm` to the scaffold and invokes Q7's existing read-only
counter profiler after successful Q7 qualification. `--profile-timeline` adds
a separate Q7 Nsight Systems trace and requires `--profiling auto`. Each workload
saves its ordinary timing before its own diagnostic subprocesses. Jobs are
serialized, so profilers do not run concurrently with another workload's timing.
PM sampling still verifies one compatible pass per kernel; Q7 counter replay
keeps its own documented replay/cache policy. They are not one continuous trace.

Diagnostic failures remain distinct from correctness/benchmark status. Profiler
durations never enter wall/event statistics or candidate/baseline ratios. All
measured/derived/proxy/estimate/unknown labels remain with the existing reports.
Q7 still has no matched GPU baseline or reviewed full-operation FLOP formula;
those values remain null. Do not pool GEMM, BF16 MLA core and full quantized MLA
timings or FLOP numerators. No compiler search, JIT, lowering or cost-model update
is invoked. AOT selection, general PREOPT COMMENT repair and dispatch stay deferred.

`--check-only` runs CPU/corpus/host checks, skips Q7 compilation/execution and
still creates the return archive. Its successful process exit is not hardware
qualification: the receiver reports INCOMPLETE until actual GPU evidence exists.

## Automatic return packaging

After all jobs, ordinary failure, timeout or a catchable SIGINT/SIGTERM, first
attempt the existing Q7 evidence collector, then the combined collector. Preserve
raw malformed/partial reports. Packaging is never numerical acceptance. Source
authentication, functioning Python and writable storage are required; SIGKILL
or power loss cannot be handled by a finalizer. Collector failures retain logs.

The terminal prints exactly the two files to send back:

```text
gpu-suite-run-XXXXXXXX/Open64_GPU_Test_results.tar.gz
gpu-suite-run-XXXXXXXX/Open64_GPU_Test_results.tar.gz.sha256
```

The outer return has a SHA256SUMS manifest and contains:

```text
Open64_GPU_Test_results/
  bundle.json / suite-results.json / SHA256SUMS / README.md
  workflow/                       # commands, statuses, closed stdout/stderr
  results/gemm/                   # common/native results and PM artifacts
  results/mla-core/               # common/native results and PM artifacts
  results/mla-q7/Open64_MLA_Q7_results.tar.gz[.sha256]
  environment/                    # GPU/Driver/CUDA/tools and allowlisted env
  build-evidence/                 # this invocation's scaffold compiler logs/receipts
```

When native Q7 collection succeeds, keep its full authenticated inner tar instead
of duplicating those logs and profiler reports. If it cannot create a tar,
preserve `results/mla-q7/raw-partial/` and explicitly leave Q7 incomplete.
Collection launches no GPU workload. Existing read-only environment capture and
snapshot helpers are reused. Missing tools, links, unreadable or changing files
are recorded rather than rewritten as successful/zero-valued evidence.

## Single receiver review

Use the original combined source packet and its external identity:

```bash
python3 tools/validate_gpu_test_suite.py \
  --archive /path/to/Open64_GPU_Test_results.tar.gz \
  --packet /path/to/Open64_GPU_Test_Suite_v1.0 \
  --expected-packet-sha256 HEX \
  --output /path/to/fresh-review
```

HEX is the PACKET_ID of that exact original source packet. The return checksum
sidecar is read by default; explicit checksum/digest options are also available.
The receiver reuses the existing archive scanner with an optional root-prefix
argument (its Q7 default is unchanged). Verify outer checksum, file types/names,
manifest, source binding and resource budgets before unpacking bounded data into
a fresh private temporary directory. Reject traversal, links, duplicate/special
members, missing/unlisted files and changing input. Never execute extracted data
or import it as code. Native Q7 archives remain subject to the existing deep audit.

Delegate each workload to the same shared validator. Enforce its expected profile
identity, original corpus/reference checks and scope. Preserve Q7's six-output,
three-oracle, Q3/race/counter checks. Mixed GPU UUIDs, swapped workload results,
contradictory statuses and nonzero workflow steps are visible failures; missing,
check-only and interrupted evidence remain incomplete. Profiles have separate
review subdirectories and individual common results. The combined summary lists
statuses and links without merging timing statistics or acceptance decisions.

Exit statuses: PASS_REPORTED 0, FAIL 1, INCOMPLETE 2, INVALID 3. Reported consistency
is not independent GPU execution, hardware attestation or transfer approval.
Raw GPU output tensors are still not returned. Preserve all earlier receipts.

## Qualification

CPU tests exercise normal/failed/partial collection, independent continuation,
first failure, optional profiling, check-only, timeout/signal cleanup, checksum
and expansion limits, all-three-profile receiver dispatch, mixed GPUs and swapped
workloads. Fixtures are synthetic. Extract the final archive, authenticate every
file and both inner manifests, compare preserved assets and exercise the real
one-command no-GPU/check-only paths plus the receiver. Actual NVIDIA execution,
race qualification, timing and profiler collection remain hardware gates.
