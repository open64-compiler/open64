# Shared GPU test results and validation v1.0

November 11, 2026. Aligns NVIDIA MLA Q7 v1.4 with Testing scaffold v2.0.
Canonical implementation: `osprey/be/lno/tests/gpu_test_results.py` and
`validate_gpu_results.py`. Both distributions and the unified receiver ship
byte-identical copies. This changes reporting and host measurement only;
compiler search, lowering, native MLA kernels and Q3 runtime are unchanged.
Personnel roles remain TBD. Actual NVIDIA qualification is pending.

## Workload boundaries remain explicit

| Profile | Numerical/operation contract | Useful FLOPs |
| --- | --- | --- |
| gemm.dense.bf16-fp32.bf16.v1 | Existing dense BF16 GEMM, FP32 accumulation, strides/transposes/alpha/beta | 2MNK; scaling/add excluded |
| mla.decode.paged.bf16-fp32.v1 | Existing single-query absorbed BF16 paged attention core; projections/cache update outside boundary | 2 H sum(L) (2 Dlatent + Drope); softmax excluded |
| mla.quantized.functional-cache.q3.v1 | Existing Q7 quantized MLA full operation, projections/cache results, six outputs, three reference lanes and Q3 lifetime/publication checks | Unknown until a full-boundary useful-work formula is reviewed |

Do not apply the BF16 core tolerances or FLOP numerator to the Q7 full operation.
Keep the minimal scaffold `workspace_bytes/run` ABI; Q7 retains its Q3 adapter.
No dispatch, AOT selection, JIT or new compiler transformations are introduced.

## Shared result format

Schema is `open64.gpu-test.results.v1`. The result has `producer`, `workload_id`,
`suite_id`, source/corpus/baseline/session `identity`, execution `environment`,
`status`, `policy`, `decision`, `reason`, `cases`, `search_invocations` and
`extensions`. Every case has:

| Field | Meaning |
| --- | --- |
| case_id / status | Server-owned dataset identity and execution disposition |
| correctness | Baseline/candidate checks, preserving workload-specific evidence in extensions/native artifacts |
| useful_flops | value/unit/evidence/reason; declared semantic work or explicit unknown |
| timing.baseline / candidate | Separate wall and device-event records; base microseconds, raw sample arrays, recomputed count/median/MAD/p95, scope, profiled=false and measured/unknown classification |
| wall_time_ratio | Candidate median wall / baseline median wall; derived or null with reason |
| diagnostics | Optional profiling references/method/status; excluded from benchmark timing and acceptance |

Unknowns are null, not zero or reconstructed measurements. Declared FLOPs are
not hardware-counter operations. Counter observations retain measured, derived,
proxy, estimate and unknown labels from the Q7 profiling contract. PM memory /
compute activity is labeled a per-kernel aggregate proxy, not exact transfer /
compute overlap or exposed waiting. No arbitrary estimates are generated.

`extensions.scaffold_v1` preserves the original scaffold payload, also written
as `result.legacy.json`. `extensions.q7` preserves native qualification,
correctness, performance and profiling records. Native reports, sanitizer logs,
references and transport identities remain available for independent auditing.
The shared envelope cannot replace missing native evidence.

## Timing and comparison contracts

Scaffold wall scope is `enqueue-plus-stream-completion.v1`; allocation, reset
and oracle checks are excluded, while required per-call preprocessing remains
inside. Device scope is `cuda-event-stream-span.v1`. Both implementations run on
the same GPU/session with identical inputs and alternating sample order. The
harness now emits raw arrays at round-trip double precision as well as existing
summary fields. The receiver recomputes useful FLOPs, throughput, ratios and the
existing improvement/MAD heuristic; it does not add adaptive convergence.

Q7 v1.4 uses a wall-only sample batch followed by a separate device-event batch.
Its wall scope is `q3-begin-submit-complete.separate-wall-batch.v1`, including
Begin/Submit/Q3 completion/publication control. Its device-event span includes
control transfers and host gaps and is explicitly not pure kernel time.
Earlier Q7 returns keep `q3-begin-submit-complete.with-event-recording.v1`;
their timing meaning is not silently upgraded by migration.

These workload/timing boundaries are different. Do not compare Q7 full-operation
latency with BF16 attention-core latency as equivalent operations. Q7 still has
no matched GPU baseline, so every Q7 wall ratio remains null. The scalar/PyTorch
oracles are correctness references, not performance denominators. A ratio is
eligible only with both correctness checks, raw positive-baseline wall samples,
matching scopes, no profiling instrumentation and same-device/session binding.

## Profiling is opt-in and independent

Both packages default to profiling off. Scaffold preserves `--profile-pm` and
its single-pass-per-kernel proof, cache-control none, disabled optional warp
sampling and independent baseline/candidate profiling children. Q7 uses explicit
`--profiling auto`, optionally with `--profile-case` and `--profile-timeline`.
Its counter replay retains cache-control all/clock-control none, with separate
Nsight Systems traces when requested. These are different methods and their
raw timelines are never stitched together or substituted for wall samples.

The scaffold saves common/native unprofiled results before diagnostics and adds
only diagnostic fields afterward. Q7 runs profiling after qualification/timing
and snapshots both through its existing always-attempted return collector.
Failures/unavailable tools have separate diagnostic status; contradictory
measurements remain validation failures. Neither collector calls compiler
search, selects another configuration or changes a cost model.

## One offline validator

For a scaffold result, use the exact authenticated source/corpus packet:

```bash
python3 validate_gpu_results.py --result /path/to/run/result.json \
  --packet /path/to/gpu-compiler-scaffold-v2.0 \
  --expected-packet-sha256 HEX --output /path/to/fresh-review
```

For an MLA Q7 return:

```bash
python3 validate_gpu_results.py --archive /path/to/Open64_MLA_Q7_results.tar.gz \
  --packet /path/to/Open64_MLA_Q7_NVIDIA_v1.4 \
  --expected-packet-sha256 HEX --output /path/to/fresh-review
```

Use the PACKET_ID supplied with that exact original packet as HEX. Keep the
return checksum sidecar, or specify `--checksum` / a trusted archive digest.
Outputs are `result.json`, `summary.json`, `summary.md`, and for Q7 a complete
`q7-review/` with the preexisting deep audit. Output directories must be fresh
and outside the trusted source packet. No archive code is extracted/executed.

Scaffold validation authenticates source manifest, corpus/file hashes,
submission and baseline/header identity, complete case coverage, numerical
tolerances, useful-work formulas, raw timing arithmetic, ratio direction,
same-session/GPU facts and the existing acceptance heuristic. PM diagnostics
check case/source/GPU bindings, raw samples/pass metadata and native-file
availability. Missing diagnostic data does not rewrite benchmark results.

Q7 archive validation delegates to the existing integrity-first validator and
retains every six-output/three-oracle, address-residue, Q3, publication,
sanitizer-capability/log, timing and counter check. It then independently
recreates the shared envelope from those native records and compares it with
the collector's root `result.json`. Q7 JSON alone is INCOMPLETE and cannot close
the native gates. Historical Q7 archives remain supported through an adapter.

Statuses remain PASS_REPORTED (0), FAIL (1), INCOMPLETE (2), INVALID (3).
PASS_REPORTED is reported-evidence consistency, never independent GPU rerun,
physical attestation or transfer acceptance. Raw GPU output tensors are not
returned; element-wise comparisons cannot be repeated offline from summaries.

## Packaging and preservation

Publish new Q7 v1.4, scaffold v2.0 and unified-validator v2.0 archives with hashes
and instructions. Keep earlier Q7/scaffold distributions unchanged. Q7's
generated CUDA/PTX/WHIRL/native images, runtime and all references/inputs remain
byte-for-byte unchanged. Scaffold kernels and deterministic corpora remain
unchanged. Workload header ABIs are preserved with added contract comments;
their new source hashes are recorded. Harness reporting/session metadata and
shared result persistence are adapted. Keep PM exporter/viewer behavior.

The scaffold source manifest covers server/submission/corpus and shared tools;
mutable `runs`, `.build` and Python caches are excluded. Source read/write and
data ownership remain server-controlled. A changed submitted source requires
a correspondingly authenticated source packet for full offline validation.

Focused tests cover all three profiles with one CLI, ratio direction, raw
sample tampering, tolerance/FLOP/source mismatches, baseline correctness,
profiling isolation, PM multi-pass refusal, Q7 native-gate preservation, forged
shared envelopes and absent baselines. Host syntax checks cover scaffold
harness changes; they are not CUDA compilation. Final acceptance still needs
actual NVIDIA correctness/races/timing/counter and PM collection evidence.
