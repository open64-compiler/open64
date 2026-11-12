# MLA Q7 returned-results validation v1.0

November 11, 2026. Receiver-side companion to Q7 friend packages v1.0–v1.2
and the automatic-return contract v1.1. This tool uses Python >=3.8 and only
the standard library. It needs no GPU, CUDA installation, PyTorch or compiler.
The existing v1.2 execution archive remains unchanged.

## Run and outputs

Keep the returned tar and its checksum sidecar together. Point to the exact
original source packet that was sent, using its trusted PACKET_ID:

```bash
python3 validate_mla_q7_results.py \
  --archive /path/to/Open64_MLA_Q7_results.tar.gz \
  --packet /path/to/Open64_MLA_Q7_NVIDIA_v1.2 \
  --expected-packet-sha256 "$(cat /path/to/Open64_MLA_Q7_NVIDIA_v1.2/PACKET_ID)" \
  --output /path/to/new-q7-review
```

The default checksum is `<archive>.sha256`; `--checksum` selects another
sidecar or `--expected-archive-sha256` supplies a trusted digest directly.
The output directory must be new. The script writes `summary.md` for reading
and `summary.json` for automation, even when input validation fails (provided
the output directory is writable). Neither input archive nor reports change.

| Result / exit | Meaning |
| --- | --- |
| PASS_REPORTED / 0 | Required case matrices, report bindings, commands, logs and recorded statistics are consistent with the fixed source contract |
| FAIL / 1 | Explicit test/build failure, nonzero hazard, numerical/statistical inconsistency or incompatible execution GPU/Driver/platform |
| INCOMPLETE / 2 | Missing/subset/ambiguous evidence, unavailable hardware/tools, unsupported instrumentation or standalone results lacking full provenance |
| INVALID / 3 | Transport/hash/source identity failure, malformed archive/protocol, unsafe member or resource-budget violation |

FAIL takes precedence over INCOMPLETE; an archive-integrity/input error takes
precedence over interpretation. A valid checksum is never a hardware pass.
Normal CLI syntax errors also use argparse's exit 2. Existing summary directories
are refused; the tool does not overwrite previous reviews.

## Integrity and isolation

The receiver verifies the outer SHA-256, scans the tar without extraction,
rejects unsafe paths, duplicates, links, special/sparse members and file/directory
conflicts, then verifies every regular file against the return SHA256SUMS.
Missing and unlisted files are rejected. The embedded source manifest and case
metadata must match the independently identified original packet. Only source
metadata/oracle files actually consumed are read and hash-checked; this is not
a redundant full compiler/source rebuild or reauthentication of unused binaries.
No archive executable, shell command, Python module, profiler or GPU work runs.

Resource limits are configurable: default 20,000 members, 16 GiB expanded bytes,
32 MiB per interpreted text and 256 MiB total retained text. Binary/profiler files
are streamed and hashed without interpretation. Oversized required logs cannot
close a gate; missing/oversized manifests invalidate the archive. Strict JSON
rejects duplicate keys, nonfinite numbers and oversized integer tokens. Returned
paths/text remain data, and Markdown display escapes active markup.

## Evidence checks

Correctness requires every source product at prefixes 256 and 40; performance
requires every product at prefix 256. Unknown/duplicate pairs, missing products
and partial scopes are identified independently of a parent's PASS. Six outputs
must each record selected-source, PyTorch and scalar comparisons with expected
element counts, finite nonnegative error statistics and the original tolerance
contract. Exact cache/scale comparisons must report zero errors. First-two-output
aggregate error bounds are checked using authenticated reference values; RMS
cannot exceed peak error. Q3 tuple, consumer, guard, input and cancellation flags
and the three ordered launch count remain required. GPU execution and complete
source authentication must be recorded. Summaries include per-case error peaks.

Each of memcheck/racecheck/synccheck/initcheck needs a successful recorded command,
full matching case/reference records, actual GPU child evidence, logs, a clean
summary and run-time tool version. Bulk-copy checks, race async/proxy checks,
info-level hazard detection, full reports, global/shared init coverage and full
memcheck leak checking are explicit. Missing capabilities, unsupported operations
or tracking overflow are incomplete; nonzero errors, warnings or hazards fail.
Suppression options cannot establish the full gate. The validator compares parent
assertions with independent child reports and does not substitute embedded parent
records for missing files.

Performance requires uninstrumented command provenance and raw finite samples.
Count, minimum, median, interpolated p95, mean and population standard deviation
are independently recomputed and checked. Host/event sample counts and recorded
iteration budgets must agree. The table labels host microseconds and submission-
stream milliseconds explicitly. These intervals include admission/control/host
gaps; they are neither pure kernel duration nor serving throughput. No baseline
speedup, universal optimum or timing result is inferred from compiler estimates.

Execution compatibility uses actual reported UUIDs, compute capability 9.0,
CUDA Driver API >=12.8 and Linux x86_64. Mixed execution devices/Driver versions
are flagged. Collection-time installed nvcc release is separated from nvidia-smi's
Driver-advertised CUDA capability. Toolkit differences/unavailable version tools
are warnings to review, not automatic proof that retained cubins cannot execute.
Original run-time device facts take precedence; later collection observations
do not rewrite them. The producer contract remains sm_90/PTX 8.6/CUDA 12.8.1.

Multiple qualification roots are never pooled into a pass. Use `--run` with an
exact archive-relative root (for example `results/03-qualification`) to select
one; other global workflow/build failures remain visible. Standalone correctness
or performance evidence is still summarized, but cannot close the complete gate
without originating command/tool provenance. Partial/failed/missing returns are
useful diagnostics and remain distinct from integrity failures.

## Limits and release packaging

PASS_REPORTED means evidence consistency, not independent numerical execution,
a proof of all races, authenticity of the operator's claims or transfer approval.
The standard return format contains comparison/error summaries rather than raw
GPU result tensors, so the receiver cannot independently rerun element-wise GPU
comparison from that tar alone. Signed hardware attestation is not introduced.
An independent reviewer still evaluates the original evidence and scope.

The validator distribution includes README.md, RELEASE_NOTES.md, this contract,
the standalone script and its tests. Synthetic positive/mutated report fixtures
test parsing and gate logic; they are never delivered as NVIDIA evidence. Real
producer return archives validate the unavailable-Driver and missing-compiler
paths. Kernel/WHIRL/COMMENT, Q3 bindings, two-phase search and fallback are
untouched. AOT selection remains deferred; owner/reviewer roles remain TBD.
