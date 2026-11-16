# Hardware resource and memory planning v0.9

This additive revision preserves [v0.8](AI-COMPILER-HARDWARE-RESOURCE-AND-MEMORY-PLANNING-DESIGN-v0.8.md)
and incorporates [H6](AI-COMPILER-H6-NORM-REDUCTION-v1.0.md). Immutable H20
96/141 device/resource/execution/budget bindings govern CTA/RF/shared/storage
admission, physical-unit costs, cache identities and selected revalidation.
An ISA name alone still grants no board performance/resource assumptions.

H6 compares retained LayerNorm rows and narrower CTAs, plus direct row/thread
and cooperative CTA/row Reduction loads. The shared panel changes memory
ownership while preserving the exact ordered F32 fold. Source requests are not
physical transfers. Optional H3 transaction envelopes carry an owned proof and
validated quantum/count bounds; legacy empty fields keep old semantics and
keys. H6's explicit L1-miss/worst-F32-base-residue model pays strided/coalesced
sector instructions, tails, all shared generations, pointer-scalar service,
resource rounding/residency and synchronization. Actual local allocation,
spills, cache hits, bubbles and bandwidth saturation need separate evidence.

Successful exploration is read-only and finite. One fixed winner is freshly
revalidated and constructed in a driver-owned native PU with COMMENTs and exact
body/launch verification. LayerNorm's existing production route records its
selected launch rather than reusing the input request width. New cooperative
Reduction launch enforcement is an explicit caller/provider obligation, without
a changed runtime ABI. Broader canonical discovery, new serving adapters,
async/TMA/cluster materializers and H0/H7 execution/calibration retain their
own gates. No host or offline compile pass establishes GPU correctness, races
or speedup. AOT selection, OpenCTI and personnel transfer assignments remain
deferred/TBD.
