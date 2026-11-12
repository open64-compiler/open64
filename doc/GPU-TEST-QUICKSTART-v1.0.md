# Combined GPU test package quickstart v1.0

November 11, 2026. These commands use the released package today. The proposed
server API/client is separate future work; OpenCTI is not required.

## Send to the GPU operator

Send these two files together:

```text
Open64_GPU_Test_Suite_v1.0.tar.gz
Open64_GPU_Test_Suite_v1.0.tar.gz.sha256
```

The archive SHA-256 is
`849d123c1801e8bdd7142fe2f8f84ee51138a5c439df4072075c10c91d43ba8d`.
Requirements for the complete suite: Linux x86_64, Python >=3.9, H100/H800
capability 9.0, Driver >=12.8, CUDA 12.8.1 compile tools and Compute Sanitizer
with the packet's required bulk/proxy/shared coverage. No PyTorch/model download
is required. The packet README details runtime compatibility and tool setup.

## Run on the NVIDIA host

In the directory containing the archive and checksum:

```bash
sha256sum -c Open64_GPU_Test_Suite_v1.0.tar.gz.sha256
tar -xzf Open64_GPU_Test_Suite_v1.0.tar.gz
cd Open64_GPU_Test_Suite_v1.0
bash RUN_COMMANDS.sh
```

Extract into a fresh directory. The default run performs correctness/race checks
and separate unprofiled timing. To request independent performance-counter
diagnostics instead, use this run command:

```bash
bash RUN_COMMANDS.sh --profiling auto
```

Optional `--gpu GPU-UUID` selects one GPU consistently for all workloads.
`--results-parent /writable/path` selects output storage. Avoid competing GPU
work during measurement. Do not run a timing invocation and profiling invocation
concurrently on the same GPU. Within an invocation the runner serializes them.

## Return the results even if testing fails

At completion the runner prints `SEND BACK:` and `CHECKSUM:` with the exact
paths of a results tar and sidecar. Send both files, including for failed or
partial runs. Ordinary failures, timeouts and catchable interruption trigger
automatic collection; Python, writable storage and a functioning finalizer are
required. SIGKILL or power loss cannot finalize a run. Consult the packet README
for manual recovery; do not overwrite any existing return tar.

The tar contains all three workload results, closed logs, this run's compiler
diagnostics, GPU/CUDA environment and original native Q7 evidence. Timing and
profiling remain separate. Unknown or unavailable counters are not zero.

## Review on the receiving machine

Keep the exact original source packet. Verify the returned transport pair:

```bash
cd /path/to/returned-files
shasum -a 256 -c Open64_GPU_Test_results.tar.gz.sha256
```

Then use that packet's validator, with fresh review output:

```bash
python3 /path/to/Open64_GPU_Test_Suite_v1.0/tools/validate_gpu_test_suite.py \
  --archive /path/to/returned-files/Open64_GPU_Test_results.tar.gz \
  --packet /path/to/Open64_GPU_Test_Suite_v1.0 \
  --expected-packet-sha256 b070675941adcfb2d55a791f7ce87c874c2b444bd165092c644cedc3b155d032 \
  --output /path/to/fresh-review
```

Read `summary.md`, `summary.json` and the `gemm/`, `mla-core/`, `mla-q7/` reviews.
Exit codes are 0 PASS_REPORTED, 1 FAIL, 2 INCOMPLETE and 3 INVALID. A successful
check-only run remains INCOMPLETE for hardware qualification. PASS_REPORTED
means the evidence passes the receiver's consistency checks; it is not an
independent device rerun. Per-workload acceptance and unknown baseline/FLOP
fields remain visible rather than pooled into one speedup.

The original package is unchanged. Software packaging is complete; actual
NVIDIA correctness, races, timing and requested diagnostics remain to be run.
