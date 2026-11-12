# MLA automatic chunk selection walkthrough v0.1

The v0.16 delivery annotates the unchanged v0.15 coverage-v1 selection.
It expands automatic comments in readable CUDA, real WHIRL COMMENT nodes and
normal whirl2c output. Recipes, candidate ledgers, selected widths, executable
CUDA tokens, host output bytes and compiled resource counts stay unchanged.

## Read the common decision first

Both review profiles use B=1,H=3,R=16, architecture sm_90 and the same supplied
per-SM capacity envelope. They estimate 48 registers/thread and request two
local CTAs/SM. Their partial-scratch budget is 1 MiB. Only profile identity
and enabled SM count differ: 132 versus 8. These are explicit offline review
envelopes, not device discovery or a claim about a particular GPU SKU.

The existing occupancy helper estimates the maximum resident local CTAs/SM.
For width 128 that estimate is ten. The policy requests only two, so:

    goal = SMs * min(requested CTAs/SM, estimated resident CTAs/SM)
    coverage = min(local CTAs, goal) / goal

The goals are therefore 132*min(2,10)=264 and 8*min(2,10)=16.
Maximum estimated residency, the requested population, and achieved CTA coverage
are distinct quantities. Coverage is not measured GPU utilization or achieved
warp occupancy.

Candidates must pass native admission, all-stage estimated resource fit and
partial-scratch budget checks. Ranking then maximizes the coverage fraction
and breaks ties with fewer chunks. Comparing fractions uses integer cross
multiplication. The printer reads the selected ledger; it does not choose again.

## Short L=127: both profiles select zero, meaning whole-row attention

Selected split_width=0 means span L=127, one chunk per row. It never means
zero work. There are three local CTAs, one for each head, no partial tuples,
and no merge launch. Shared score/statistic storage is 544 bytes/CTA.

The split candidates begin at 128 and only widths below L are enumerated;
all of them would cover the whole short row, so those duplicate plans are
omitted. Only direct attention remains. It is eligible, and the L<=256
short-direct rule is recorded as applied.

The three CTAs fall below either 264 or 16, intentionally. coverage-v1 does
not create additional short-row split/merge work merely to populate the device.
That short-row rule is heuristic; it is not a measured latency result.
For other short shapes a split can be considered when the direct plan does
not fit. The retained L=256/1024-byte shared-budget boundary test verifies it.

The rest of the short file stays in the same five-stage order: query absorption,
fused attention, value projection, output store, LSE store. Register accumulation,
five required softmax-publication barriers, masks and numeric guards are intact.

## Long L=8193: compare every candidate

A direct candidate is absent because L exceeds the admitted direct-attention
limit 4096. This is a native implementation limit, not a claim that the profile's
physical memory cannot hold a whole-row array.

All six split candidates pass both supplied envelopes' capacity/resource/scratch
checks. The wider candidates are not rejected as illegal in this example.

| Width | Chunks ceil(8193/W) | Local CTAs 3*C | Partial bytes 216*C | 132-SM coverage | 8-SM coverage |
| --- | ---: | ---: | ---: | --- | --- |
| 128 | 65 | 195 | 14040 | 195/264 | 16/16 |
| 256 | 33 | 99 | 7128 | 99/264 | 16/16 |
| 512 | 17 | 51 | 3672 | 51/264 | 16/16 |
| 1024 | 9 | 27 | 1944 | 27/264 | 16/16 |
| 2048 | 5 | 15 | 1080 | 15/264 | 15/16 |
| 4096 | 3 | 9 | 648 | 9/264 | 9/16 |

The tuple has R+2=18 F32 values per row, hence 3*18*4=216 bytes per chunk.
Estimated local residency is ten at widths 128..1024, seven at 2048 and three
at 4096. All remain at least the requested two, so each profile's goal is
constant across these candidates.

## Why the 132-SM profile selects width 128

No candidate reaches the 264-CTA target. Width 128 gives 195/264, approximately
73.9% of the requested CTA population. Width 256 gives 99/264; every wider
candidate reduces coverage further. The largest available coverage wins,
even though narrower chunks increase tuple and merge work.

Width 128 is the smallest candidate supported by coverage-v1. Widths 64 and
below are outside this search; this result is not a global optimum claim.
Reaching a static coverage goal is not a native correctness requirement.

The chosen layout has 65 chunks and 195 local CTAs. The last chunk starts at
token 8192 and contains one token. Partial scratch is 14040 bytes; total global
temporary storage is 14508 bytes. The merge has three CTAs and consumes the
stable locally normalized [m_c,d_c,z_c] tuples.

## Why the 8-SM profile selects width 1024

The target is only 16 CTAs. Widths 128,256,512 and 1024 all get coverage 16/16.
The policy caps coverage at the goal, so 195 CTAs get no extra coverage credit
over 27. Width 1024 wins the tie using nine chunks instead of 17,33 or 65,
with 1944 partial bytes instead of 3672,7128 or14040.

Width 2048 would be wider and use fewer tuples, but yields only 15 CTAs:
15/16 is below full coverage. Width 4096 yields nine CTAs, only 9/16.
Both are resource-eligible; their coverage is why they lose.

The chosen layout has nine chunks and 27 local CTAs, with the same one-token
tail at 8192. Partial scratch is 1944 bytes; total global temporaries are
2412 bytes. Larger local chunks increase shared score storage from 544 to
4128 bytes/CTA and lengthen each CTA's token loops. The merge still has three
CTAs but now reads nine tuples per row instead of 65.

Both long files retain six launches and the same stable rescaling equations,
register/private-sum ownership, five CTA barriers per local/merge CTA, full-mask
shuffle trees, mask guards and all-empty behavior.

## Interpret the choice and verify the annotations

The resource constraints decide feasibility. The coverage goal and fewer-chunk
tie break are a policy for balancing scheduling opportunity against scratch and
merge/fixed work. They do not model calibrated instruction stalls or guarantee
the fastest kernel. Architecture alone does not supply SM count. Real GPU
timing, occupancy and Compute Sanitizer evidence remain pending.

The new WINNER WALKTHROUGH comments substitute the actual B,H,L,R, target,
selected span, tail, tuple footprint and per-alternative loss reason. They
distinguish the short-direct exception from general coverage ranking.
The generator emits them automatically, rather than hand editing four CUDA
files or performing a second selection inside whirl2c.

Retain native comments.before/after.B and independently reopened matching .T
traces with ir_b2a -st -src, raw annotation diffs and prior-annotation comparisons.
Verify payload equality across CUDA and WHIRL-derived source, identical recipes,
manifests, executable tokens, compiled resource counts and reference output
bytes for both profiles. Focused native tests cover direct short, underfilled
long, coverage-saturating long and short resource fallback paths; all 28 saved
profile/scenario FP64 reference checks are replayed.

The previous 59-test implementation validation remains retained separately.
Transfer roles remain TBD at the user's request; formal acceptance remains open.
