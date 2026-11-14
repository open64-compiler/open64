# H20 memory hierarchy implementation plan v1.3

2026-11-12. This revision extends [v1.2](AI-COMPILER-H20-MEMORY-HIERARCHY-IMPLEMENTATION-PLAN-v1.2.md)
and the original [H0-H8 plan](AI-COMPILER-H20-MEMORY-HIERARCHY-IMPLEMENTATION-PLAN-v1.0.md).
Earlier material is preserved. H1 software bindings remain implemented;
H0/H7 hardware inventory and qualification remain open.

## H2 implementation and remaining gates

[Owned movement facts v1.0](AI-COMPILER-H2-OWNED-MOVEMENT-FACTS-v1.0.md)
specifies the implemented records, actual phase-owner capture sites, evidence
meaning, recapture and fallback. Its family matrix is the exact current admission
scope; it does not claim four new executable LNO routes.

| H2 work | Delivery and acceptance |
| --- | --- |
| Owned common records | C++98 Work Partition, storage/access/version/alias facts, byte ledger and bounded Resource Lifetime/Execution Events validation; detached copy and full-content identity |
| Native family adapters | GEMM VHO common matmul/locality, MLA canonical High/MR1 LNO, LN2 centered source/conditional storage events and R2 direct/staged WOPT; exact owning admission reused |
| Device binding | Immutable H20 96/141 resource/profile/partition/model/toolchain/budget dependency captured; no imported profile preserves architecture-only behavior |
| Serving/staging projection | Q3 registered ranges/content/scale/page/publication and H1d device-sidecar identity; selected Q6 verified raw/decode pipeline, not candidate staging expansion |
| Recapture/invalidation | Native recapture before selected application/emission; known LNO activation/mutation epoch, no fabricated epoch elsewhere; changed source/profile/view/generation refuses reuse |
| Focused qualification | Both variant fixtures, native no-mutation B/T diffs, stale/unknown/overflow/lifetime negatives, separate selected native and quantized reference execution, backend link closure |
| Remaining canonical-route preparation | General canonical common High admission/DO_LOOP/ARRAY materialization for unsupported GEMM/LN/Reduction forms remains open. Facts do not grant it; operation-specific admission and before/after/O-level evidence are required before extending execution |

The delivered H2 fact-capture layer can feed H3 models now. The original
canonical-route preparation gate remains explicit; do not relabel conditional
LN2/R2 facts as new production GPU algorithms. H4-H6 must either supply those
missing routes with evidence or retain an existing admitted fallback.

## Next phase ordering

1. H3 consumes owned source/request opportunities and exact GPU dependency keys.
   Add separate physical-service estimates, confidence/calibration provenance and
   finite search budgets. Unknown HBM/L2/spills remain unknown unless established
   by a qualified model or later measurement; no fabricated zero-cost term.
2. Preserve estimation-only portfolios and global convergence budgets. Cache
   keys bind complete source, numerical, lifetime and device dependencies.
3. H4 GEMM and H5 MLA select reuse/staging plans only with operation-specific
   numerical, alias, ownership, memory budget and event legality. Cross-head
   cache reuse requires Q3 generations and complete all-head release proof.
4. H6 LayerNorm/Reduction completes any missing canonical executable routes and
   materializers before pipeline selection. Ordered statistics/folds and
   per-element C2 overwrite proof remain controlling constraints.
5. H7 uses server-side correctness/race qualification, then separate timing and
   profiling. Compare identical workloads and physical device/partition/budget
   identities. Hardware counters do not retroactively become source facts.
6. H8 accepts only complete selected-route/reference evidence and documented
   unknowns. Personnel transfer roles remain TBD; AOT selection and OpenCTI
   integration retain their previously deferred status.

Risks: stale phase records or incremental objects, incomplete alias/stride
proof, shape-only storage matching, scale/page generation ABA, raw/decoded
readiness conflation, unsafe multihead overwrite, logical requests mistaken for
HBM service, and unknown device resources mistaken for another board's defaults.
Mitigations are explicit complete bindings, checked counts, distinct event joins,
fresh owning recapture, native parity checks and compatible fallback. H2 does
not establish NVIDIA execution or performance qualification.
