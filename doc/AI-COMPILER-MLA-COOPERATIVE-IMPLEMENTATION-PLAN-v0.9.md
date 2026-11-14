# MLA implementation plan v0.9: cooperative AOT candidate

November 5, 2026. Implements the next step after the retained fusion/split and
COMMENT milestones. Design authority for this step:
[cooperative lowering v0.3](AI-COMPILER-TENSOR-KERNEL-LOWERING-v0.3.md).
Primary owner, successor, reviewer and approving lead remain TBD; assignment and
independent review remain transfer-acceptance blockers.

| Milestone | Implementation and acceptance |
| --- | --- |
| C1: closed selection | Explicit v3 fused-O3 recipe, reassociation permission, v1/v2 fallback and unchanged source image. Reject stale policy and lower optimization levels. |
| C2: native warp reductions | Lane-strided F32 reduction, full-mask five-step tree, neutral inactive lanes, lane-zero writes. Retain native images/traces, test NaN maximum, tails and merges. |
| C3: native shared staging | 32x4 dot tile, two unconditional CTA barriers, 640-byte aligned shared allocation, bank-swizzled indices and guarded source loads. Test unequal broadcast batches, K and column tails. |
| C4: explanations and audits | Generate selected ownership, grouping, scratch, staging and synchronization explanations as real WHIRL COMMENT statements. Reopened auditor rejects altered collectives, bounds, resources and comments. Require comment equality across source views and COMMENT-only executable-token equality. |
| C5: executable certification | Seven scenarios for each MLA length against independent FP64 reference; matching scalar warp-tree review and synchronized mapped host execution; NVCC sm_90 compilation and retained resource reports. Publish immutable artifacts and focused commit. |
| C6: hardware acceptance | On sm_90 hardware, execute all cases and Compute Sanitizer memcheck/racecheck/synccheck, record registers/spills/occupancy/bandwidth/latency, benchmark v2 versus v3 with full stage and launch costs. Pending usable GPU. |
| C7: policy and integration | Calibrate workload buckets, select fallback when cooperative loses, integrate target/numerical contracts and owned phase ordering in be_driver. Preserve Very High domain operators, High algorithmic DO_LOOP/ARRAY and LNO scheduling, Mid structured memory lowering, Low mapping, and PTX architecture. No JIT dependency. |

C1-C5 are implemented by the focused PU-local lowering service and its validation
harness. Completion evidence belongs to the v0.9 delivery receipt; compiler and
host-only success cannot close C6. C7 requires its own code review and native
before/after checkpoints in the actual integrated pipeline. Existing general
architecture and implementation documents remain authoritative for that work.

Driver order for this candidate is prepare/verify source, select fused/split and
cooperative policy, establish device PUs, capture before, materialize active PU,
validate, annotate COMMENTS, capture after, independently reopen/audit, emit normal
whirl2c CUDA, compile AOT, then launch using audited metadata. No emitter chooses
staging or parallelization. No new be.so dependency is introduced.

Any change to arithmetic grouping, tail ownership, shared layout, barrier
participation, launch geometry or numerical permission invalidates the physical
products and all dependent comments/manifests/source. Rerun the selected generic
and MLA semantic tests and regenerate retained evidence. Printer-only changes
add source/compile checks; COMMENT-only changes require token equality. Keep all
older artifact receipts and create a new version for every certified candidate.

The principal performance risks are launch and synchronization overhead on short
workloads, increased CTA counts, extra live state and shared-memory use, and
remaining score traffic. The principal correctness risks are divergent
collectives, wrong matrix/batch indexing, shared-buffer reuse before readers
finish and F32 regrouping. The focused tests exercise those contracts; only
hardware profiling can establish speedup and profitable dispatch thresholds.
