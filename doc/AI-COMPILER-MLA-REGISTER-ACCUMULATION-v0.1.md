# Fused MLA register accumulation and smaller long chunks v0.1

This amendment extends AI-COMPILER-FUSED-MLA-v0.1.md. The default remains AOT.
The selected v5 path removes shared cache-tile staging from fused local
attention and gives each warp four lane-private column sums. Long-context
review selects width 128 instead of 256. Older v1–v4 recipes retain their
existing implementations.

## Algorithm and synchronization proof

A 128-thread CTA first computes masked scores and a stable local softmax.
Four warp trees publish maxima; thread zero folds four summaries. The same
protocol obtains the denominator. Two CTA barriers for each fold and one for
normalized-probability publication remain, totaling five. These barriers are
required by cross-warp dependencies and are outside all tail predicates.

After publication the shared probability array is immutable. Warp w owns
columns group*16+w*4+j for j=0..3. Lane l visits tokens l,l+32,..., reads their
shared weight once and updates four private F32 sums with directly loaded
cache values. Each sum uses the full-mask shuffle tree 16,8,4,2,1. Lane zero
stores valid columns. All lanes execute every shuffle even when a column is
outside R. There is no shared cache tile, no cross-lane register access except
the typed shuffles, and no CTA barrier in this accumulation phase.

The merge uses the same four-column ownership over chunk tuples rather than
cache tokens. It retains the five statistic/publication barriers. No additional
warp synchronization is needed for private state. This does not authorize
removing barriers from the old shared-tile implementation.

For an unchanged partition each output column retains its lane-strided sum
and tree. Changing partition permits different F32 rounding under the existing
explicit numerical permission. Empty chunks select zero rescaled mass before
exponentiation. Fully masked rows return zero latent and negative-infinite LSE.
Normalized local z_c values and the stable merge remain:
m=max m_c; t_c=d_c exp(m_c-m); D=sum t_c;
z=sum (t_c/D) z_c; LSE=m+log(D).

## Selected review workloads and tradeoffs

| Quantity | Previous short | Register short | Previous long | Register long |
| --- | ---: | ---: | ---: | ---: |
| B,H,L,R | 1,3,127,16 | same | 1,3,8193,16 | same |
| Chunk width | 127 | 127 | 256 | 128 |
| Chunks | 1 | 1 | 33 | 65 |
| Local attention CTAs | 3 | 3 | 99 | 195 |
| CTA barriers/full local chunk | 37 | 5 | 69 | 5 |
| CTA barriers/one-token long tail | — | — | 13 | 5 |
| Local shared bytes/CTA | 1056 | 544 | 1568 | 544 |
| Merge shared bytes/CTA | — | — | 544 | 544 |
| Launches | 5 | 5 | 6 | 6 |
| Global internal allocation bytes | 468 | 468 | 7596 | 14508 |
| Partial tuple bytes | 0 | 0 | 7128 | 14040 |

Global allocations exclude inputs, outputs, canaries and shared memory.
Score/probability tensors remain absent from global storage. More local CTAs
give the GPU more independently schedulable work and shorten local token spans.
They also increase tuple traffic, chunk-rescaling and merge work. Direct cache
loads can lose staging locality; four independent accumulators increase live
register demand. Instruction count, cache behavior and occupancy require
architecture-specific evidence. No universal choice of width or speedup is
claimed. The register path and width are explicit selected inputs, not emitter
heuristics or a JIT requirement.

## Representation and phase ordering

The per-PU VHO service validates the complete admitted absorbed MLA topology.
The detached v5 recipe adds mandatory true register_latent to the sealed v4
contract. It requires fused/cooperative O3 and explicit reassociation permission.
The focused driver operation is:
stream-attention v4.json v5.json --split-width 128 --allow-reassociate
(use zero for unsplit short attention). Width 1, >4096, malformed text and
unproved source graphs reject before publication. Existing capacity bounds apply.

The owning phase selects chunks, kernel graph, register ownership, storage and
launch geometry before native construction. Native WHIRL contains the private
scalar STs, guarded ARRAY accesses, DO_LOOPs and typed effectful shuffle/barrier
CALLs with immediate return-preg stores. The selected accumulation metadata is
audited against reconstructed semantics after reopening. WHIRL opcode/TY-kind,
mapped binary layout and ELF contracts do not change. No be.so dependency is
added; only the standalone lowerer consumes the detached JSON.

Ordering is source gatekeeper -> O3 recipe selection -> v4 attention proof ->
explicit v5 register/partition selection -> resource/capacity validation ->
native device materialization -> semantic verifier -> canonical explanation
insertion as real COMMENT nodes -> independent mapped-image reopen/audit ->
normal whirl2c printing -> AOT CUDA compilation -> launch-manifest consumer.
The readable tensor-level CUDA uses the same per-column arithmetic helper as
v4; its register-prefixed helper identifies selected ownership, while the
mapped WHIRL/CUDA carries the actual physical register loops.

Any change to register_latent, width, dimensions, numeric policy, topology or
target invalidates materialized kernels, kernel fingerprints, manifests,
resources and generated comments. Rebuild and reverify together; never reuse a
manifest or COMMENT payload from another recipe. Backend mapping does not
change upstream source tensor semantics or traverse another PU. Annotations
have their own executable-token-invariant checkpoint.

## Implementation milestones and acceptance

1. Add sealed v5 policy, strict validation, selected ownership metadata and
   native private-accumulator loops. Keep the v4 staged path selectable.
2. Update canonical walkthrough generation and both source views; retain exact
   COMMENT payload equality. Explicitly describe five remaining barriers and
   the scratch/parallelism tradeoff.
3. Validate unequal B,H,R shapes, single-token and uneven tails, masked heads,
   empty very-negative chunks and large finite values against an independent
   expanded FP64 reference (rtol 2e-5, atol 2e-6). Compare readable and mapped
   host outputs exactly and reverse worker/CTA startup order.
4. Audit rejection of changed barriers, masks, shared bounds, resources,
   collective prototypes, tuple layouts, ownership metadata and COMMENT text.
   Retain before/after .B, independent ir_b2a -st -src .T and raw diffs.
5. Compile both views with nvcc for sm_90, record ptxas register/shared/spill
   results and link device reference runners. Probe actual GPU availability.
6. On an available H100, run reference tests and Compute Sanitizer memcheck,
   racecheck and synccheck, then benchmark paired v4/v5 width candidates with
   warmups, repeated CUDA-event timing and launch overhead included. Sweep
   production B/H/L/R and masks; record latency distribution, active CTAs,
   memory throughput, stall reasons and register occupancy. Use measured
   end-to-end performance to choose defaults or dispatch boundaries.

Host synchronization simulation and nvcc compilation do not close milestone 6.
Owner, successor, independent reviewer and approving lead remain TBD as the
user requested; transfer acceptance remains open. Retained source, exact tools,
commands, oracle data and immutable receipt provide the reproducible knowledge
package; current validation results belong in that receipt.

## Remaining risks

For R<16 some warp/column work is inactive; all shuffles remain uniform.
For large R the four-register group repeats and cache traffic may dominate.
A one-token chunk still pays fixed CTA reduction overhead. Smaller chunks can
increase merge time enough to offset local parallelism. Host simulation cannot
prove device scheduling, bank behavior or race freedom. Production dimensions,
precision modes, architectures and arbitrary attention graphs outside the
closed admitted topology need separate admission and validation.
