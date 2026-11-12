# GPU testing/service review series v1.0

The service is reviewed as dependency-ordered layers on develop. Each PR targets
its predecessor so review diffs contain only that layer. Merge in order using
merge commits to preserve ancestry, then retarget the next PR to develop. If a
parent is squash-merged, rebase its successors before retargeting. Do not merge the whole
historical AI/compiler branch as one change.

| Order | Branch | Scope |
| --- | --- | --- |
| 1 | `codex/gpu-01-evidence-contracts` | Add GPU result arithmetic and counter evidence contracts |
| 2 | `codex/gpu-02-benchmark-harness` | Add server-owned GEMM and MLA benchmark harness |
| 3 | `codex/gpu-03-pm-diagnostics` | Add isolated single-pass GPU PM diagnostics |
| 4 | `codex/gpu-04-native-return-validation` | Validate native Q7 return archives without GPU execution |
| 5 | `codex/gpu-05-shared-validation` | Validate GEMM and MLA through shared evidence contracts |
| 6 | `codex/gpu-06-collection-workflow` | Collect GPU test evidence after success failure or interruption |
| 7 | `codex/gpu-07-combined-runner` | Compose GEMM and MLA tests with one automatic return archive |
| 8 | `codex/gpu-08-persistent-state` | Persist GPU jobs and exclusive lease ownership before execution |
| 9 | `codex/gpu-09-worker-adapter` | Run registered GPU packets with cancellation and lease-safe recovery |
| 10 | `codex/gpu-10-http-contract` | Define the authenticated GPU service wire contract |
| 11 | `codex/gpu-11-http-server` | Serve authenticated GPU jobs progress cancellation and artifacts |
| 12 | `codex/gpu-12-remote-client` | Reconnectable remote client and independent local review |

The final runtime implementation matches the released GS2 service; publication
adds focused foundation/server tests and one non-executable quota docstring.
The original source branch, dirty documents and immutable execution/GS1/GS2
archives are preserved. Recursively repeated historical plans are retained in
those histories/receipts, not copied into every review diff. Current standalone
contracts live beside these tools; historical plan names are plain references.

Read GPU-TEST-SERVER-GS2-v1.0.md for installation, SSH-tunnel/credential setup,
submit/wait, watch/reconnect, cancel, fetch and trusted offline review. The
server operates on registered immutable packets supplied separately. It does
not build compiler lowering or accept arbitrary new source uploads.

Original deployment identities:

- Combined execution archive SHA-256:
  `849d123c1801e8bdd7142fe2f8f84ee51138a5c439df4072075c10c91d43ba8d`.
- GS2 companion SHA-256:
  `c8bf96961edb1ba924c79aa88d5d83a0f2ad26aa2badb59917aec364238dd06e`.

Validation retains host checks per layer and at the final tip. Earlier actual
packet/check-only and API-crash reconnect evidence is requalified for the final
review tip. Synthetic/CPU evidence never certifies NVIDIA correctness, races or
performance. GS3 needs actual supported hardware. OpenCTI remains deferred.

See [compiler backlog](AI-COMPILER-PENDING-REVIEW-INVENTORY-v1.0.md) for the older
work deliberately excluded from this series.
