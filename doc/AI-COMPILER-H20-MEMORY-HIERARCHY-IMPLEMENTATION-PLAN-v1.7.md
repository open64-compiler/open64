# H20 memory-hierarchy implementation plan v1.7

This additive revision preserves [v1.6](AI-COMPILER-H20-MEMORY-HIERARCHY-IMPLEMENTATION-PLAN-v1.6.md)
and all earlier H1-H5 deliveries. [H6 specification](AI-COMPILER-H6-NORM-REDUCTION-v1.0.md)
defines the bounded explicit-contract LayerNorm/Reduction software delivery.
H20 96 GB and 141 GB remain distinct immutable device/resource/profile bindings.

| Phase | Implementation / acceptance |
| --- | --- |
| H6a admission | O3 active whole-PU LN2 v2 or R2 v2 source, exact numerical/alias/buffer/version/lifetime/event proof, owned H2 and imported H20/H3. Existing source matcher/owner remains authoritative; no general scalar-loop discovery claim |
| H6b finite exploration | Nine C0/C1/C2 × 128/32/64-thread LayerNorm recipes; five direct/cooperative Reduction recipes. Global H3 proposal/unique/site/request/memory/deadline limits; stable finite minimum and conservative refusal of incomplete comparisons. Cost only owned records, no candidate native construction or execution |
| H6c fixed winner | Full current source/control/profile/model/recipe/estimate equality, fresh H2 capture and fixed-only cost revalidation. LayerNorm's legacy cycle graph is expanded only for the winner, without reopening alternatives |
| H6d native construction | Existing exact LN4 body with retained row/narrow CTA; new ordinary-WHIRL Reduction contiguous shared panel, publish, ascending one-owner F32 fold, readers-release, guarded tails and output. Fresh driver-owned PU; COMMENT explanations; full native body/declaration/launch identity |
| H6e production binding | Default-off `-DSL:h6_memory_hierarchy=on` enters existing LayerNorm request route; old selector/refusal remains. H6 review manifest records selected grid/block/shared and separate estimated H3 ns. Reduction production APIs require an explicit caller with a new selected-launch obligation; old row-thread guard does not authorize cooperative launch |
| H6f software evidence | Both H20 profiles; selected WHIRL-derived reference checks, exceptional values/tails, unchanged source/native isolation, selected/native stale refusals, budgets/levels/fallback, post-PREOPT CLI and standalone construction, offline CUDA/PTX and legacy four-family/control/link checks. Retain native boundaries, full diffs, original source, hashes/receipts and final read-only reopen |
| H0/H7 hardware gate | Actual H20 inventory, device correctness/sanitizers/races, code/resource/spill checks, separate timing/profiling and calibration; pending until server execution |

H6 keeps source useful bytes separate from selected requests and declared sector
envelopes. The optional H3 transaction fields preserve legacy zero/default keys
and costs; malformed envelopes/overflow refuse. LayerNorm C1 pays four external
input sweeps, C2 pays all six shared row read/write sweeps and both statistics.
Private pointer-scalar traffic remains explicit and has no free RF/spill credit.
Reduction changes load ownership while retaining +0 identity, ascending left
fold and NaN-class policy. No reassociation/tree/padded access is introduced.

General canonical discovery, a new automatic Reduction request/serving launch
adapter, RMSNorm, wider algorithm catalogs, vectorization, deeper async/TMA and
cluster routes retain their separate gates. H6 source is unchanged; analyses
are freshly owned in the new device PU. Existing COMMENT workaround and
High/Mid/Low/PTX phase contracts remain. This software delivery does not close
H0/H7/H8 hardware/transfer acceptance. AOT selection and OpenCTI stay deferred;
transfer roles stay TBD. No measured speedup is claimed.
