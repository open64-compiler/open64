# Combined GPU test packaging closure v1.0

November 11, 2026. Packaging status: SOFTWARE_VALIDATED_GPU_PENDING.

## Decision and scope

The combined GEMM/MLA software package is complete. Its remaining acceptance
gate is execution on suitable NVIDIA hardware, not another packaging rewrite.
Keep OpenCTI integration deferred until its modernization is complete and its
applicable compatibility gates have passed. Neither this package nor the GPU
server API work depends on OpenCTI. Continue the server API and operator/client
usability work in parallel within AI optimization.

## Released identity

| Item | Value |
| --- | --- |
| Archive | `Open64_GPU_Test_Suite_v1.0.tar.gz` |
| Archive bytes | 199055673 |
| Transport SHA-256 | `849d123c1801e8bdd7142fe2f8f84ee51138a5c439df4072075c10c91d43ba8d` |
| Source PACKET_ID | `b070675941adcfb2d55a791f7ce87c874c2b444bd165092c644cedc3b155d032` |
| Producer commit | `a3d6f3309e1debc238d90fb30fe888bc55381eb0` |
| Workloads | GEMM, BF16 MLA core, full quantized MLA/Q3 |
| Run command | `bash RUN_COMMANDS.sh` |
| Optional profiling | `bash RUN_COMMANDS.sh --profiling auto` |
| Return | `Open64_GPU_Test_results.tar.gz` and its `.sha256` sidecar |

The transport digest authenticates the compressed archive; PACKET_ID identifies
the source manifest. These are different identities. The release README supplies
the receiver command, platform/tool requirements, recovery and measurement scope.
The original release is immutable; later contracts are companion documents.
The [quickstart](GPU-TEST-QUICKSTART-v1.0.md) provides the concise send/run/return/
review flow using commands that already exist.

## Completed evidence

All 271 existing subpacket scripts are preserved, including extensionless
helpers. Complete scaffold/Q7 inner file sets and hashes match their released
packets. Advanced standalone entry points remain accessible; the root wrapper
does not claim to forward every option. McClintock's separate OpenCTI inventory
does not establish OpenCTI integration with this package.

The independent GPU script audit's writable-cache and omitted-build-log findings
were resolved through writable per-run scaffold staging and returned compiler
logs/receipts. Shared-adapter tests are explicitly included in release
qualification; they are not claimed to run under Q7's narrower per-run pattern.

The final extracted archive passed 39 focused tool tests, 25 existing scaffold
host tests and 51 existing Q7 host tests. Actual check-only and missing-CUDA
invocations returned evidence automatically. Their receiver verdicts were
INCOMPLETE and FAIL respectively, rather than false GPU passes. Authentication
also confirmed final runtime tools match the committed producer source.

Canonical evidence is under the primary checkout's `artifacts/review/`:

```text
ai-optimization/research/gpu-combined-suite/
sha256-849d123c1801e8bdd7142fe2f8f84ee51138a5c439df4072075c10c91d43ba8d/
  receipt.json / report.json / SHA256SUMS
  evidence/archive-replay.json / evidence/replay-report.json
  audit/report.md / SCRIPT_AUDIT_FOLLOWUP.md
```

## Remaining gates and parallel work

CP0 through CP5 in [plan v1.28](AI-COMPILER-OPTIMIZATION-SEARCH-IMPLEMENTATION-PLAN-v1.28.md)
are software-complete for the identity above. CP6 remains pending: actual CUDA
build/device correctness, required sanitizer lanes, unprofiled timing and any
requested real counter/PM collection. No NVIDIA host is currently available.
Counter absence, unknown causal stalls and Q7's absent matched GPU baseline or
full-boundary FLOP formula remain explicit limitations.

The parallel server contract is [API v0.1](GPU-TEST-SERVER-API-v0.1.md).
[Plan v1.29](AI-COMPILER-OPTIMIZATION-SEARCH-IMPLEMENTATION-PLAN-v1.29.md)
separates the completed delivery, NVIDIA gate and server/client milestones.
Roles remain TBD; technical packaging closure is not personnel transfer approval.
