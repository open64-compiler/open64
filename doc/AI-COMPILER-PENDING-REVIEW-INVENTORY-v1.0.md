# AI compiler pending-review inventory v1.0

Inventory source: local `codex/ai-layernorm-profitability` at
`2ba92036c3f58e0b4725f600a3f5a7f1bce0710d`. Testing/service review base:
`d69db73f298ac0ffb99b5da40f4965bd614412c6` (develop). The original branch has
88 unpublished commits; this review series extracts testing/service
software only. All original commits and pre-existing dirty/untracked documents
remain untouched. Inventory is not approval to merge compiler transformations.

| Future review family | Boundary and acceptance required before publication |
| --- | --- |
| LN3/LN4 costing and device/host execution | Isolate profitability, selected WHIRL, compiled-entry resource checks, host/service leases and dispatch. Revalidate against current develop ownership relocations. |
| Native partition-axis discovery | LNO instance/contribution axes, bounded applicability, diagnostics and standalone trace tests. |
| Absorbed MLA ingestion and CUDA emission | Frontend operator census/unsqueeze and native B/T boundary; common High lowering, readable/WHIRL-derived CUDA and reference fixtures. |
| MLA kernel schedules | Fusion, split/warp reductions, staging, register broadcasts and chunk selection; split by semantic transformation with native before/after evidence and independent references. |
| Two-phase search and M1-M6 production route | Estimation-only portfolios, owned snapshots/epochs, admission, read-only observer, budgets, atomic selected application/rollback and verified exit/driver bridges. |
| MR0-MR2 data movement/reuse | Cache traits, owned access/version/lifetime facts and cross-head reuse costing; keep profiling distinct from estimation. |
| Q0-Q6d quantized/paged MLA | Capture/native contracts, ingestion, prefill/decode unknowns, Q3 bindings, costing, selected staging, GPU mapping and device completion/publication. |
| Q7 producer/native harness integration | Retain prepare/build/run/profiler capture scripts and native `tensor_serving_cuda_review.cxx`/Makefile changes with their Q6d/compiler prerequisites. Current service executes previously registered packets instead. |
| Historical architecture/design plans | Reconcile authoritative current documents and preserve old receipts. Publish focused deltas with links instead of repeatedly embedding all prior plans. |

For each family: inspect current develop, identify the smallest dependency-
complete batch, run impact-selected tests, retain native before/after B/T/diff
when an IR transformation changes, and publish separate PRs. Prior hardware-
pending statements remain pending; no global GPU optimum or measured speedup
is inferred from the host/software evidence. Source admission/AOT/JIT and
OpenCTI integration remain separately deferred.

A machine-readable original-commit/file inventory and full validation logs are
retained in the local PR-publication receipt; generated binaries, secrets and
large runtime archives are not committed to these review branches.
