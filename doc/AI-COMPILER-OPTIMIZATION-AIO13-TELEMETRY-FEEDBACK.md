# AIO-13 Telemetry And Feedback

## Status

The first AIO-13 PU-local, check-only slice is implemented as stage G16. It
accepts immutable runtime measurements for the certified AIO-12 variant family,
validates that the profile still describes the active PU and exact variant
identities, constructs a separate measured-cost plan view, and recommends the
most profitable variant for a future compilation. It does not mutate current
legality, current selection, RuntimeVariantIR, executable WHIRL, or binary
WHIRL.

## Problem And Purpose

A static target model can rank legal implementations, but it cannot prove
achieved latency, occupancy, memory traffic, communication overlap, cache
behavior, launch overhead, or guard behavior on a deployed workload. AIO-13
keeps those measured facts distinct from architectural capability and semantic
legality. This permits later compilation or policy tuning to improve
profitability without retroactively changing what a previously certified plan
means.

## Open64 Continuity

Open64 already has instrumentation phases, PU-owned feedback, freshness checks,
and optimizer consumers. AIO-13 reuses the `PROFILE_PHASE_BEFORE_VHO` phase
identity and the established rule that feedback is optional and PU-scoped.
The current Open64 `.fb` header and record families do not contain tensor,
runtime-variant, occupancy, communication, or accelerator-memory metrics.
Therefore this slice does not change `Fb_Hdr`, `Pu_Hdr`, or the existing
feedback file format. A later instrumentation/transport milestone must either
extend that facility through a reviewed versioned record family or provide an
equally explicit compatible transport.

## Ownership Boundary

`common/com/dsl_telemetry_profile.{h,cxx}` owns only flat immutable records,
construction, structural verification, access, stable names, and generic
printing. It does not inspect active WHIRL, decide freshness, create costs, or
select a plan.

`be/vho/dsl_telemetry_feedback_opt.{h,cxx}` owns the PU-local semantic join to
the current RuntimeVariantIR, target/generation/identity freshness checks,
profile interpretation, measured-cost construction, and future-plan
recommendation. Cross-PU profile aggregation remains explicit IPA/runtime
work.

## TelemetryProfileIR

The profile header records:

- schema version;
- producer kind;
- profile generation;
- target profile;
- owner PU;
- Open64 instrumentation phase;
- exact record count.

Each flat variant record carries:

- runtime site, variant, optimization plan, and stable variant identity;
- sample, selection, guard evaluation, and guard-pass counts;
- total, minimum, and maximum end-to-end latency in nanoseconds;
- achieved occupancy in parts per million;
- memory read and write bytes;
- communication and overlapped communication time;
- cache hit and miss counts;
- launch time.

All arithmetic fields are unsigned integers so identical input records produce
identical analysis on every host. The first slice requires complete records and
does not represent unknown measurements by magic numeric values.

## Freshness And Legality

Common structural verification rejects malformed schema, phase, counters,
ranges, duplicate site/variant rows, and incomplete records. VHO additionally
requires:

1. active PU ownership;
2. expected target and profile generation;
3. complete coverage of the current runtime variants;
4. exact site, variant, optimization-plan, and stable variant identity;
5. an already certified RuntimeVariantIR and source OptimizationPlanIR.

Any mismatch is stale evidence and fails closed. Profile evidence can change a
profitability recommendation, but the feedback pass copies the original
candidate and plan legality unchanged. It never turns an unknown or rejected
candidate into a legal one.

## Measured Cost View

The first slice treats average measured end-to-end latency as the comparable
cost. It creates a new OptimizationPlanIR context, copies candidate identity
and legality, and attaches complete nanosecond cost terms with high confidence
and `telemetry` evidence. End-to-end latency is placed in the compute term;
other terms are zero in the first slice to avoid double counting launch,
communication, memory, and overlap components already included by the measured
latency. The raw components remain separately inspectable.

The standard VHO plan selector chooses within this new measured view. The
original static cost records and selected plan remain untouched. This is the
contract for updating a future estimate without mutating a previously
certified plan.

## No-Profile Behavior

A missing profile is not an error. The phase produces a deterministic
`status=no_profile` result with no feedback plans or recommendations. It does
not silently synthesize measurements or alter the static selection.

## First Certified Fixture

The existing rank-2 F4 `common.matmul.v1` AIO-12 fixture supplies two certified
variants:

- unguarded Open64 direct baseline;
- selected cuBLASLt variant guarded by two 16-byte alignment checks.

The deterministic benchmark profile reports 700 ns average latency for the
baseline and 1000 ns for the statically selected guarded variant, with a 70%
guard-hit rate and complete occupancy, traffic, communication, overlap, cache,
and launch measurements. The measured plan recommends the baseline while the
source plan remains selected on the guarded variant. This proves that feedback
can alter profitability but not legality or current IR meaning.

## Certification

Certification requires:

- structural malformed-record rejection;
- target, generation, plan, and variant-identity stale-profile rejection;
- incomplete-profile rejection;
- deterministic no-profile behavior;
- repeated profile runs with identical feedback traces;
- source and feedback `.B` files and `ir_b2a -st -src` traces byte-identical;
- no telemetry records in binary WHIRL;
- rebuilt `be.so`, `be`, and `lw_inline` with no frontend-builder or new
  library dependency.

## Deferred Work

- runtime instrumentation insertion and counter collection;
- reviewed on-disk profile transport;
- profile merging, weighting, confidence decay, and stale-age policy;
- workload-state and cross-run distribution modeling;
- profile-driven AIO-10 guard ordering and hysteresis;
- applying a recommendation during a future compilation;
- cross-PU aggregation under explicit IPA control;
- profiler-provider integration for NVIDIA occupancy and traffic counters.
