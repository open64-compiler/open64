# MLA 32/64 automatic chunk search v0.1

This amendment extends the existing offline MLA selection with widths 32 and
64. New auto-attention invocations default to coverage-v2. The source topology,
register accumulation, stable local normalization/rescaling and native GPU
mapping are reused unchanged. AOT remains baseline.

## Versioned control and compatibility

    tensor_kernel_lower auto-attention input.json selected.json \
      --gpu-profile gpu.json --allow-reassociate

coverage-v2 searches direct attention when L<=4096 plus split widths
32,64,128,256,512,1024,2048,4096 below L. The same eligibility, estimated
occupancy, scratch budget, coverage comparison, fewer-chunk tie break and
L<=256 legal-direct exception apply.

    tensor_kernel_lower auto-attention input.json legacy.json \
      --gpu-profile gpu.json --allow-reassociate --chunk-policy coverage-v1

coverage-v1 keeps its original 128..4096 candidates. A sealed old v6 recipe
continues to decode, verify, materialize and explain with its own recorded
policy. No new binary WHIRL/ELF layout or logical opcode is introduced. The
v6 record shape remains unchanged; its policy field versions the candidate set.
Native selection identity and recipe fingerprint include the policy name.
Manual stream-attention override still emits v5 and clears automatic policy.

This is the existing standalone tensor-lowering driver integration, not a
claim of a new general opencc/be_driver option or runtime probing. The VHO
phase owns selection; whirl2c prints already selected native semantics.

## Default 132-SM review result

For B=1,H=3,L=8193,R=16 and the supplied two-CTA/SM target, the goal is 264.
The selected local estimated residency remains ten CTAs/SM, above the target.

| Width | Chunks | Local CTAs | Capped coverage | Partial scratch | Merge shared |
| --- | ---: | ---: | --- | ---: | ---: |
| 32 | 257 | 771 | 264/264 | 55512 bytes | 1568 bytes |
| 64 | 129 | 387 | 264/264 | 27864 bytes | 1056 bytes |
| 128 | 65 | 195 | 195/264 | 14040 bytes | 544 bytes |

Width 64 now wins. Both 32 and 64 meet coverage, so 64 wins the fewer-chunk tie.
The prior width128 plan covered about 73.9% of the requested CTA population;
the new plan reaches 100% of that static goal. This is not measured device
utilization, achieved occupancy or a measured speedup.

Total global temporaries grow from 14508 to 28332 bytes for width64. The
width 32 plan uses 55980 bytes. Six launches remain; no global score/probability
buffers return. Both tails still consist of the token at index 8192.

The 8-SM/two-CTA profile still selects width1024: it reaches the 16-CTA goal
with nine chunks, so 32/64 add scratch and merge work without coverage credit.
A third explicit review experiment keeps 132 SMs but requests four CTAs/SM.
Its 528-CTA goal selects 32 (771 CTAs), exercising the 257-chunk merge. This
profile changes a policy target; it is not another physical-device discovery.

## Small widths reuse the native implementation

Blocks remain 128 threads/four full warps. Shared scores round to 128 slots even
at width 32/64. Width 64 has two useful score warps; width 32 has one. Neutral
padding, all four warp folds and five uniform CTA barriers remain required.
Register latent accumulation uses the actual span, lane-strided tokens,
four private column sums and full-mask shuffle trees, including column tails.

Score/exponential shared storage remains544bytes per local CTA. More chunks
repeat fixed synchronization and padded exponential work. Nominal shared exp
slots over this review grid are:
width128:3*65*128=24960;
width64:3*129*128=49536;
width 32:3*257*128=98688.
These source-loop counts are not hardware instruction or cycle measurements.

Partial tuples remain [m_c,d_c,z_c], with normalized z_c. The merge visits
all real C tuples and never reads unused padded slots. C=129 means tid0
processes chunks 0 and 128 in max/D folds; C=257 means tid0 processes0,128,256.
Vector lanes still visit c=lane,lane+32,...; lane0 includes the final tuple.
Shared merge allocations round C to 128 slots, hence 256 slots for 129 and 384
for 257, plus eight statistics values.

All-stage resource checks include this larger merge. Tests cap shared budget
at 1024 bytes: local 32/64 allocations of 544 bytes fit, but merge 1568/1056 do not. Those
candidates reject and width 128 remains eligible. This prevents an apparently
cheap local chunk from selecting a merge that exceeds the supplied envelope.

## Numeric, comment and validation contracts

Masks, ascending source dot loops, FP32 reassociation permission, safe maxima
and denominators, zero-mass chunk guards and final LSE remain unchanged.
Different partitions can change FP32 rounding; correctness uses independent
expanded FP64 references at rtol 2e-5, atol 2e-6, including infinity/masking rules.

The canonical explanation generator prints coverage-v2, the complete expanded
ledger, why64 beats32 at the default goal, the legal-direct short exception,
partial/merge footprints, neutral score lanes and padded/fixed-work costs.
Real WHIRL COMMENT payloads match readable CUDA and normal whirl2c output.
Legacy v1 text, recipes and native-image audits remain byte/semantically stable.

Focused validation covers native width 32 and64 outputs with uneven tails,
deterministic/resource-sensitive choices, scratch/merge bounds, legacy policy
reproduction and unknown-policy rejection, forged ledgers/winners, manual
override, O0 exclusion and inherited fused/cooperative/source contracts.
Three profile selections retain 42 saved short/long oracle combinations,
including full L=8193 merges with 129 and 257 chunks. Compile all readable and
mapped CUDA for sm_90, record registers/shared/spills and retain native
before/after.B with independently reopened ir_b2a -st -src .T/raw diffs.

The retained v1 four-profile/context images must still audit, reselect and
regenerate byte-identical readable source using current tools. Preserve
their old receipts. New evidence goes in a fresh versioned delivery.

Shape, profile, policy and numerical permission changes invalidate selected
kernels, manifests, resources and explanation fingerprints. Hardware latency,
Compute Sanitizer and production tuning remain pending without a usable GPU.
Primary owner/successor/reviewer/approving lead remain TBD as requested.
