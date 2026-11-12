# AI compiler review series v1.0

The 403 committed compiler/design backlog paths inventoried after the testing/service extraction are now represented in focused review slices. Five paths follow the merged owner relocations. Publication is separate from milestone acceptance.

The complete testing/service stack reached develop through merged [#218](https://github.com/open64-compiler/open64/pull/218). The compiler slices start on that exact reviewed stack. Only LN3 has fresh per-slice native qualification and completed manual compliance review. Later milestones remain drafts pending their own current-base build, native before/after evidence, fallback/rollback checks and complete file review.

## Merge procedure

1. #218 is merged into develop.
2. #219 now targets develop directly; merge it after review.
3. For each later slice, finish its listed acceptance gates, retarget it to develop after its predecessor has merged into develop, then mark it ready and merge.
4. Do not merge a slice into its preceding stack branch and assume it has reached develop. A stack-branch merge requires a later promotion; this caused the #207–#217 integration gap.

## Review slices

| Slice | Pull request | Files | Acceptance status |
| --- | --- | ---: | --- |
| 1: Add complete LayerNorm costs with conservative analytical selection | [#219](https://github.com/open64-compiler/open64/pull/219) | 20 | LN3 native qualified; ready for review |
| 2: Materialize selected LayerNorm device WHIRL with native verification | [#220](https://github.com/open64-compiler/open64/pull/220) | 17 | Draft; per-slice gates pending |
| 3: Route typed LayerNorm requests through cost-gated device lowering | [#221](https://github.com/open64-compiler/open64/pull/221) | 26 | Draft; per-slice gates pending |
| 4: Bind LayerNorm compiled entries to resource-checked CUDA adapters | [#222](https://github.com/open64-compiler/open64/pull/222) | 12 | Draft; per-slice gates pending |
| 5: Compose LayerNorm host and service execution over shared leases | [#223](https://github.com/open64-compiler/open64/pull/223) | 16 | Draft; per-slice gates pending |
| 6: Complete bounded LayerNorm host dispatch and service packaging | [#224](https://github.com/open64-compiler/open64/pull/224) | 24 | Draft; per-slice gates pending |
| 7: Discover native instance and contribution partition axes in LNO | [#225](https://github.com/open64-compiler/open64/pull/225) | 16 | Draft; per-slice gates pending |
| 8: Ingest absorbed MLA tensor expressions through native WHIRL | [#226](https://github.com/open64-compiler/open64/pull/226) | 28 | Draft; per-slice gates pending |
| 9: Emit compilable tensor reference CUDA from native WHIRL | [#227](https://github.com/open64-compiler/open64/pull/227) | 10 | Draft; per-slice gates pending |
| 10: Lower absorbed MLA tensor graphs into executable CUDA WHIRL | [#228](https://github.com/open64-compiler/open64/pull/228) | 13 | Draft; per-slice gates pending |
| 11: Fuse MLA tensor producers and split long-context reductions | [#229](https://github.com/open64-compiler/open64/pull/229) | 14 | Draft; per-slice gates pending |
| 12: Preserve optimization explanations in WHIRL and generated CUDA | [#230](https://github.com/open64-compiler/open64/pull/230) | 13 | Draft; per-slice gates pending |
| 13: Lower MLA reductions to cooperative warps and shared tiles | [#231](https://github.com/open64-compiler/open64/pull/231) | 13 | Draft; per-slice gates pending |
| 14: Fuse MLA scores softmax and weighted latent accumulation | [#232](https://github.com/open64-compiler/open64/pull/232) | 13 | Draft; per-slice gates pending |
| 15: Reduce MLA barriers with register accumulation and finer chunks | [#233](https://github.com/open64-compiler/open64/pull/233) | 12 | Draft; per-slice gates pending |
| 16: Select MLA chunks from explicit compile-time GPU resource envelopes | [#234](https://github.com/open64-compiler/open64/pull/234) | 14 | Draft; per-slice gates pending |
| 17: Map narrow MLA chunks to 32-thread and 64-thread blocks | [#235](https://github.com/open64-compiler/open64/pull/235) | 13 | Draft; per-slice gates pending |
| 18: Broadcast single-warp MLA statistics through registers | [#236](https://github.com/open64-compiler/open64/pull/236) | 13 | Draft; per-slice gates pending |
| 19: Merge two-warp MLA partial tuples with one CTA barrier | [#237](https://github.com/open64-compiler/open64/pull/237) | 14 | Draft; per-slice gates pending |
| 20: Separate estimation-only MLA search from selected WHIRL lowering | [#238](https://github.com/open64-compiler/open64/pull/238) | 24 | Draft; per-slice gates pending |
| 21: Carry common MLA High WHIRL into LNO with canonical admission | [#239](https://github.com/open64-compiler/open64/pull/239) | 14 | Draft; per-slice gates pending |
| 22: Own High WHIRL snapshots with stable identities and mutation epochs | [#240](https://github.com/open64-compiler/open64/pull/240) | 13 | Draft; per-slice gates pending |
| 23: Observe MLA search with validated bindings and native-state isolation | [#241](https://github.com/open64-compiler/open64/pull/241) | 17 | Draft; per-slice gates pending |
| 24: Bound production MLA search strategies with global budgets | [#242](https://github.com/open64-compiler/open64/pull/242) | 16 | Draft; per-slice gates pending |
| 25: Apply selected MLA High plans atomically with rollback and rebuild | [#243](https://github.com/open64-compiler/open64/pull/243) | 19 | Draft; per-slice gates pending |
| 26: Verify selected MLA High exit through GPU mapping and PTX | [#244](https://github.com/open64-compiler/open64/pull/244) | 18 | Draft; per-slice gates pending |
| 27: Integrate selected MLA production and legacy dependence-image recovery | [#245](https://github.com/open64-compiler/open64/pull/245) | 17 | Draft; per-slice gates pending |
| 28: Preserve MLA cache traits through ingestion and High WHIRL | [#246](https://github.com/open64-compiler/open64/pull/246) | 32 | Draft; per-slice gates pending |
| 29: Derive owned MLA cache storage access version and lifetime facts | [#247](https://github.com/open64-compiler/open64/pull/247) | 18 | Draft; per-slice gates pending |
| 30: Cost MLA data movement and cross-head cache reuse without mutation | [#248](https://github.com/open64-compiler/open64/pull/248) | 24 | Draft; per-slice gates pending |
| 31: Capture quantized paged MLA graphs and define native serving contracts | [#249](https://github.com/open64-compiler/open64/pull/249) | 11 | Draft; per-slice gates pending |
| 32: Ingest quantized MLA caches with inferred traits and native images | [#250](https://github.com/open64-compiler/open64/pull/250) | 33 | Draft; per-slice gates pending |
| 33: Lower quantized MLA to canonical High WHIRL with owned scale facts | [#251](https://github.com/open64-compiler/open64/pull/251) | 17 | Draft; per-slice gates pending |
| 34: Route quantized MLA through production PREOPT with fresh fact rebinding | [#252](https://github.com/open64-compiler/open64/pull/252) | 23 | Draft; per-slice gates pending |
| 35: Infer MLA workload geometry with explicit unknown phase cases | [#253](https://github.com/open64-compiler/open64/pull/253) | 14 | Draft; per-slice gates pending |
| 36: Bind MLA cache generations and publish serving bundles atomically | [#254](https://github.com/open64-compiler/open64/pull/254) | 17 | Draft; per-slice gates pending |
| 37: Cost quantized paged MLA movement from live serving bindings | [#255](https://github.com/open64-compiler/open64/pull/255) | 17 | Draft; per-slice gates pending |
| 38: Qualify quantized MLA plans and lower only the selected High algorithm | [#256](https://github.com/open64-compiler/open64/pull/256) | 27 | Draft; per-slice gates pending |
| 39: Qualify selected quantized MLA staging layouts and pipeline contracts | [#257](https://github.com/open64-compiler/open64/pull/257) | 9 | Draft; per-slice gates pending |
| 40: Generate native quantized MLA staging dequantization and explicit waits | [#258](https://github.com/open64-compiler/open64/pull/258) | 18 | Draft; per-slice gates pending |
| 41: Lower selected native staging to synchronized GPU CUDA and PTX | [#259](https://github.com/open64-compiler/open64/pull/259) | 22 | Draft; per-slice gates pending |
| 42: Bind selected MLA device completion to serving publication lifetimes | [#260](https://github.com/open64-compiler/open64/pull/260) | 10 | Draft; per-slice gates pending |
| 43: Connect selected quantized MLA compiler products to the NVIDIA harness | [#261](https://github.com/open64-compiler/open64/pull/261) | 9 | Draft; per-slice gates pending |
| 44: Align native Q7 producer tooling with shared timing profiling and result returns | [#262](https://github.com/open64-compiler/open64/pull/262) | 6 | Draft; per-slice gates pending |
| 45: Preserve packaging closure and historical optimization plan revisions | [#263](https://github.com/open64-compiler/open64/pull/263) | 10 | Draft; per-slice gates pending |

| 46: Rebind compiler consumers to relocated DSL service owners | [#264](https://github.com/open64-compiler/open64/pull/264) | 11 | Integration build/reference checks pass; draft pending predecessor acceptance |

## Evidence and limits

Fresh LN3 evidence: 7 host oracle/schema tests, 40 native cases, 728 comparisons, 2,184 candidate costs, 160 PREOPT control runs, 80 separate-process `ir_b2a -st -src` dumps, 32 byte-identical retained baseline cases and 40 empty native query diffs. Backend symbol isolation and `lw_inline` closure passed.

Immutable local receipt: `ai-optimization/research/compiler-publication-ln3/sha256-aee34127cac7d1806d8cf02bfd7a2a199296ea69fca26f9045dc7591e4cb8921`. Its `source/` preserves the complete source coordinates required for reopening under `/open64`. This is native analytical qualification, not GPU execution.

Cross-series ownership reconciliation rebuilt be/be.so, ir_tools, LNO, WOPT, whirl2c and the native frontend. Seven selected native regression tests pass with the rebuilt emitter; 34 PyTorch capture/reference tests and 26 Q7/partition-axis tests also pass. A tip build or a host test does not certify every intermediate slice. NVIDIA correctness, sanitizer, performance-counter and overlap measurements remain pending hardware. Historical API v0.1 documents are archival; v0.2 remains the service wire contract.

The original worktree remains unchanged, including its 14 modified and 10 untracked design-document files. These pre-existing uncommitted documents are not silently included in this publication. OpenCTI, AOT selection and PREOPT COMMENT repair remain deferred.
