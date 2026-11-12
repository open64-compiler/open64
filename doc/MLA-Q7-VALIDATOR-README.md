# Open64 MLA Q7 returned-results validator

This receiver-side tool checks a returned results tar and produces a readable
review plus a machine-readable summary. It runs on Python 3.8 or later using
only the standard library. No NVIDIA GPU, CUDA, PyTorch or Open64 build is needed.
The v1.1 receiver supports Q7 v1.0–v1.3 execution packages. Keep the supplied
`mla_q7_profile.py` beside the validator; this shared counter-semantics module
has no external dependencies. The NVIDIA packet already includes both files.

## Quick start

Keep the returned tar and its `.sha256` sidecar together. Use the **same original
NVIDIA source packet version** that was sent to the operator:

```bash
python3 validate_mla_q7_results.py \
  --archive /path/to/Open64_MLA_Q7_results.tar.gz \
  --packet /path/to/Open64_MLA_Q7_NVIDIA_v1.3 \
  --expected-packet-sha256 "$(cat /path/to/Open64_MLA_Q7_NVIDIA_v1.3/PACKET_ID)" \
  --output /path/to/new-q7-review
```

Read `new-q7-review/summary.md`. Automation can read `summary.json` and the exit
code. The output directory must be fresh and outside the original source packet.
Use `--checksum /path/to/sidecar` if it has a different name, or provide a trusted
transport digest with `--expected-archive-sha256 HEX`.

## Results and exit codes

| Status | Exit | Meaning |
| --- | --- | --- |
| PASS_REPORTED | 0 | Full reported case, reference, sanitizer and timing evidence is present and consistent |
| FAIL | 1 | Explicit test/build/hazard failure, contradictory reports, invalid statistics or incompatible execution facts |
| INCOMPLETE | 2 | Missing/subset/ambiguous evidence, unavailable hardware/tools or insufficient instrumentation |
| INVALID | 3 | Bad checksum, source identity, archive layout, protocol or resource budget |

No top-level PASS is trusted on its own. The review checks child reports,
commands and logs, case/residue coverage, reference-error statistics, raw timing
samples and actual GPU/Driver facts. It reports host microseconds and submission-
stream milliseconds separately, including recomputed median/p95 and sample counts.
nvidia-smi's advertised CUDA capability is not treated as the installed toolkit;
nvcc and execution Driver observations are labeled separately.
Diagnostic profiling has its own status and is never pooled into benchmark
timing. The receiver recomputes rates/issue inactivity from raw NCU CSV and
checks units, evidence labels, stage identities and profile-child correctness.
Missing diagnostic tools are visible separately; fabricated attribution or
contradictory values fail validation. MEASURED_WITH_UNKNOWNS does not mean
exposed compute waiting has been attributed. See the included profiling spec.
Malformed outer/archive/source protocol is INVALID; malformed child reports
are evidence failures (FAIL). Neither is accepted as a reported pass.

Transport/file checks run without extraction or execution of archive contents.
Unsafe paths, duplicate members, links, special/sparse files, missing/unlisted
files and hash mismatches are rejected. No workload, profiler, search or lowering
is launched. Default limits are 20,000 tar members, 16 GiB expanded data, 32 MiB
per interpreted text and 256 MiB total retained text. See `--help` to change them.

If several qualification runs were collected, select one explicitly:

```bash
python3 validate_mla_q7_results.py ... --run results/03-qualification
```

Use the full required arguments from Quick start in place of `...`. Passes from
different runs are never pooled. Other returned workflow/build failures remain
visible. Standalone performance data can be summarized, but missing command/tool
provenance prevents complete Q7 acceptance. Failed or partial archives still
produce useful summaries.

## Validation limits

PASS_REPORTED is **reported evidence consistency**, not an independent GPU rerun,
proof of all possible races, signed hardware attestation or transfer approval.
The standard return archive contains numerical comparison summaries rather than
raw GPU output tensors. The tool checks bounds and consistency but cannot repeat
element-wise GPU comparison from that tar alone. Timing is neither pure kernel
duration nor serving throughput; no speedup is claimed without a matched baseline.

The original archives/reports are never rewritten. Earlier v1.2 returns remain
supported; collecting the new profiling evidence requires v1.3. Q3 bindings,
COMMENT, two-phase search, fallback
and compiled kernels are untouched. AOT selection remains deferred.

## Package contents and tests

- `validate_mla_q7_results.py`, `mla_q7_profile.py`: receiver and counter semantics.
- `test_mla_q7_validation.py`: synthetic format/negative fixtures, not GPU tests.
- `README.md`, `RELEASE_NOTES.md`, `SPECIFICATION.md`: usage, release record and detailed rules.
- `examples/`: summaries of actual producer no-GPU/compile-failure returns; these are diagnostics, not NVIDIA qualification results.
- `SHA256SUMS`: distribution file checksums.

Run the focused tests after extracting the validator package:

```bash
python3 -m unittest -v test_mla_q7_validation test_mla_q7_profiling
```
