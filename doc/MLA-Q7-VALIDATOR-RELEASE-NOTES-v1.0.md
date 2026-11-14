# Q7 returned-results validator v1.0 — release notes

Released November 11, 2026. Receiver-side companion to Q7 result archives.

This release adds integrity-first tar inspection, source-packet binding and
Markdown/JSON summaries of reported correctness, all four sanitizer gates,
raw performance samples and GPU/CUDA compatibility. It is a small standalone
archive with README and release notes; the existing v1.2 NVIDIA execution
distribution remains unchanged.

Features:

- Stream and hash every tar file without extracting or executing its contents;
  reject unsafe/duplicate/link/special/sparse members and missing/unlisted data.
- Check the independently identified original packet and each metadata/oracle
  file consumed. Do not substitute archive self-assertions for trusted source identity.
- Check full fixed-product/residue coverage, six outputs and three numerical
  reference lanes, finite statistics, exact cache/scale errors and Q3/lifetime guards.
- Check actual sanitizer commands, bulk/async/proxy/shared coverage, tool versions,
  summaries and child-case equality; flag hazards, overflow and missing evidence.
- Recompute timing count/min/median/p95/mean/standard deviation and reject profiled
  baseline commands, inconsistent counts and nonfinite/raw-sample errors.
- Flag unsupported GPU capability, insufficient Driver API, incompatible platform,
  mixed execution devices and toolkit differences. Separate Driver CUDA capability
  from installed nvcc release and collection-time snapshots from run-time facts.
- Keep PASS_REPORTED, FAIL, INCOMPLETE and INVALID distinct; never pool independent
  runs into a pass. Summarize usable partial evidence without inventing missing stages.

Validation covers synthetic positive and mutated protocol fixtures (including
hash/path/duplicate failures, omitted logs/cases, hazards/overflow, timing tampering,
instrumented timing, incompatible/mixed device facts, source mismatches and
resource limits). Synthetic positive fixtures are not real device evidence.
Integration checks use actual producer return archives from no-Driver and missing-
compiler runs, which must remain incomplete/failed rather than GPU-qualified.

Limitations: no raw GPU output tensors or signed hardware attestation are present
in the current return format. This tool reviews evidence consistency and required
coverage, not independent device execution, a general race proof, global optimum
or transfer signoff. No GPU was available for the producer's validation work.
All original kernel/WHIRL/COMMENT/Q3/search/fallback behavior remains unchanged;
AOT selection remains deferred.
