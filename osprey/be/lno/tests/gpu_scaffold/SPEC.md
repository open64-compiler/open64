# Agreed performance-testing baseline, version 1

Distribution v2 preserves this workload/ABI contract and adopts the shared
`open64.gpu-test.results.v1` result format. See SHARED_RESULTS.md. The original
v1 payload is retained under `extensions.scaffold_v1` and in a `.legacy.json`
sidecar. Raw wall/device sample arrays are now emitted for independent review.

This document records the converged GEMM/MLA testing contract. Use it as the
basis for scaffold changes and future improvements. "Converged" here means the
agreed specification; the current fixed-sample benchmark does not implement
statistical convergence or adaptive stopping.

## Required contract

1. A submission contains manifest.json, candidate.cu, and baseline.cu. The server
   supplies the selected workload.h. The compiler-facing ABI remains only
   workspace_bytes(args) and run(args, workspace, stream).
2. The server owns versioned workload definitions, exact test tensors, reviewed
   golden results, numerical acceptance criteria, useful-FLOP formulas, and the
   measurement policy. Generate oracles from the stored input values, independently
   of compiler-generated candidates. Cache reusable data and compiled artifacts.
3. Check baseline and candidate correctness against the same golden outputs,
   including required side effects and input immutability. Failed validation
   prevents performance acceptance.
4. Measure baseline and candidate for every dataset on the same GPU and in the
   same evaluation session. Use identical inputs, numerical rules, cache/reuse
   assumptions and operation boundaries. Warm up and alternate their order.
5. Wall-clock measurement begins before run and ends after its work completes.
   Exclude allocation, input restoration and validation. Include required per-call
   preprocessing. Record device-event measurements separately.
6. Every dataset has wall_time_ratio = candidate median wall time / baseline
   median wall time. Below 1 means improvement. If unavailable, use null and a
   reason. Never replace this ratio with its reciprocal or a profiler-derived time.
7. Record useful FLOPs and effective TFLOP/s for both implementations using one
   declared useful-work numerator. GEMM uses 2MNK; MLA's count follows its declared
   operation boundary. Distinguish useful work from executed hardware operations.
8. Preserve timing variability, sample count, workspace, source/corpus/build
   identity, device identity, software versions, decision and failure reasons.
9. Keep rejection cheap: validate before benchmarking; profile only selected
   correct cases/candidates. Optional diagnostics must not add overhead or
   dependencies to ordinary timing runs or change their acceptance results.

## Current implementation boundaries

GEMM v1 is single real dense GEMM with BF16 storage, FP32 accumulation, layouts/
transposes/row strides, alpha/beta, separate C and D, and no epilogue. MLA v1 is
single-query absorbed paged decode with BF16 storage and FP32 arithmetic; query
projection, RoPE application and output projection are outside the boundary.
The shipped kernels demonstrate the interface, not state-of-the-art performance.

The minimal server supports local baseline source and cudart. Registered
baselines, prefill, batched GEMM, fused epilogues, and persistent preprocessing
are extensions. No CUDA compilation or GPU execution has yet been verified on
the development Mac; validation.json records actual checks.

## Extension and validation rules

Keep the common ABI small. Put operation-specific semantics in versioned workload
contracts and data in versioned suites. Change workload/suite identities when
semantics, corpus or tolerances change. Add optional diagnostic artifacts rather
than mandatory compiler-facing fields. Keep unprofiled timing authoritative.

For changes, explain which required contract is affected and how it is preserved.
Run focused host tests for reference/reporting changes and CUDA execution checks
on an appropriate GPU for CUDA changes. Never represent host syntax checks or
check-only results as GPU certification.

The optional single-pass PM diagnostic path is documented in PROFILING.md.
It is implemented with host tests; real CUDA/Nsight Compute validation remains
outstanding. Changes to sampling policy or statistical convergence
require a separately recorded policy, without silently redefining the ratio.
