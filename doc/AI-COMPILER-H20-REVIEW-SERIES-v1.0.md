# H20 compiler review series v1.0

November 13, 2026. Publication covers the completed H20 design and H1a-H7 software work. Review scope, merge status and hardware acceptance are separate. The original tested development branch and all immutable milestone receipts remain preserved.

## Prerequisite promotion

[268](https://github.com/open64-compiler/open64/pull/268) promotes the previously merged compiler integration stack to `develop`. It is a draft pending current-develop integration reconciliation/checks. Earlier PRs were merged into intermediate branches; that alone did not promote the complete stack into develop. This promotion introduces no new H20 implementation.

## Focused review order

| Stage | PR | Files | Scope |
| --- | --- | ---: | --- |
| architecture | [#269](https://github.com/open64-compiler/open64/pull/269) | 9 | Define H20 memory hierarchy architecture and delivery gates |
| whirl-docs | [#270](https://github.com/open64-compiler/open64/pull/270) | 67 | Align AI compiler designs with WHIRL opcodes and tensor traits |
| H1a | [#271](https://github.com/open64-compiler/open64/pull/271) | 16 | Select H20 96GB and 141GB devices with ISA validation |
| H1b | [#272](https://github.com/open64-compiler/open64/pull/272) | 15 | Resolve immutable H20 resource and execution profiles |
| H1c | [#273](https://github.com/open64-compiler/open64/pull/273) | 50 | Bind H20 profiles to snapshots, cache keys and selected plans |
| H1d | [#274](https://github.com/open64-compiler/open64/pull/274) | 26 | Carry H20 device bindings through device and serving contracts |
| H2 | [#275](https://github.com/open64-compiler/open64/pull/275) | 41 | Capture owned movement and reuse facts across AI operators |
| H3 | [#276](https://github.com/open64-compiler/open64/pull/276) | 38 | Cost data movement and reuse with finite estimation-only search |
| H4 | [#277](https://github.com/open64-compiler/open64/pull/277) | 25 | Optimize GEMM shared-to-register reuse with bounded H20 plans |
| H5 | [#278](https://github.com/open64-compiler/open64/pull/278) | 30 | Reuse MLA latent-cache panels across heads in selected H20 plans |
| H6 | [#279](https://github.com/open64-compiler/open64/pull/279) | 24 | Optimize LayerNorm and Reduction memory hierarchy for H20 |
| H7 | [#280](https://github.com/open64-compiler/open64/pull/280) | 23 | Integrate H20 qualification into GPU packages and shared validation |

Merge the prerequisite promotion into `develop` after its integration gates pass. Retarget the first H20 PR to `develop`, review and merge there, then repeat for each successor. Do not merge a child only into its predecessor branch and assume it reached develop. Each PR body links its immediate dependency.

## Validation and evidence continuity

The publication replay preserves every non-document tracked blob at every milestone boundary. The only source-tree reconciliation adds the already merged predecessor documentation, including its precision/feedback amendments and full historical implementation queues. The final review index is documentation only. Original native/static evidence is reused by exact source identity; publishing new commits is not a new GPU run.

Fresh publication-tip validation passes 111 tests: 82 shared/package/worker/H7 tests and 29 loopback API/client/server tests. H7 retains 22 offline SM90 cubins, both extracted one-command and offline receiver paths, persistent GS1 check-only execution and independent final-path audits. All 1,439 original inner-suite files remain unchanged.

Earlier milestone receipts retain native selected/reference, stale-binding, rollback, O-level, finite-budget, no-model fallback, independent mapped before/after and link-boundary evidence at their original source boundaries. H6 includes 26,396 exact reference outputs and 161 native images. Receipt IDs are in the individual PR descriptions and the local publication ledger.

Two-phase estimation-only search, selected-only native construction, explanatory WHIRL/CUDA COMMENTs, native compatibility and existing fallback are preserved. No emitter-side optimization policy is introduced.

## Open acceptance gates

Actual NVIDIA/H20 correctness, sanitizer/resource observations, timing and hardware-counter measurements remain pending server execution. Synthetic profile/model fixtures do not qualify physical predictions, and missing matched native GPU baselines leave speedup ratios unknown. H7 currently covers synchronous H4 GEMM and H5 WHIRL-derived CUDA via NVCC; broader H4 TMA-provider and new H5 native-PTX hardware qualification remain separate.

AOT serving selection, OpenCTI and PREOPT COMMENT repair remain deferred. Roles remain TBD. Source publication and GitHub merge status do not close these acceptance gates.
