# Hardware resource and memory planning v0.10

This additive revision preserves [v0.9](AI-COMPILER-HARDWARE-RESOURCE-AND-MEMORY-PLANNING-DESIGN-v0.9.md)
and adopts [H7](AI-COMPILER-H7-H20-QUALIFICATION-v1.0.md). Selected compilation
bindings remain immutable. Runtime qualification observes physical H20 identity,
variant capacity, partition/MIG state, actual function resources and occupancy.
SM90 is an ISA prerequisite, not evidence of H20 board resources or bandwidth.

H7 returns original compiler profile/model digests beside actual GPU UUID,
Driver/toolchain/cubin identity and resource/timing observations. Code eligibility
is checked against actual limits; synthetic SM/cache/clock/throughput inputs
remain unqualified model assumptions. Exceeded RF estimates require rebind.
Neither profiling nor benchmark paths update compiler cost models or reopen
selection. Correctness/races gate separate wall and event batches. Diagnostic
counters keep measured/derived/proxy/unknown meanings and do not establish exact
causal compute bubbles or peer/remote critical-path waiting from aggregate data.

The GPU package/receiver and GS1/GS2 integration preserve earlier suites, scripts,
COMMENTs, finite budgets, two-phase search and fallback. Native matched baselines,
physical calibration, broader TMA providers and actual H20 evidence remain gates.
No static/mock check grants GPU performance acceptance. AOT selection, OpenCTI
and personnel transfer assignments stay deferred/TBD.
