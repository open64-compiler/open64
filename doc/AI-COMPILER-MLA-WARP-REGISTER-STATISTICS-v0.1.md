# MLA single-warp register statistics, v0.1

This v0.21 implementation replaces the width-32 local kernel's three shared
publication fences with two lane-zero register broadcasts. It retains the
width-64, direct and 128-thread merge realizations. The AOT selection default
is now coverage-v4; explicit coverage-v1/v2/v3 policies remain reproducible.
Source truth remains the PU-local VHO plan and native WHIRL, before CUDA emission.

## Admission and selected representation

VHO_TENSOR_KERNEL.register_statistics is true only for coverage-v4, mapping3,
stage1, split_width32, threads32 and register latent accumulation. Each admitted
lane owns exactly one token and its probability for every latent-column group.
The copied merge entry explicitly clears the flag. There is no new opcode,
TY-kind, binary section, be.so dependency, JIT or runtime device discovery.

The unchanged v6 recipe envelope seals the new policy and a candidate statistics
field, warp_register_broadcast for width32 and shared otherwise. All-stage
resource admission uses a zero local shared footprint for the register candidate;
merge/projection limits, scratch bounds, register estimate and supplied allocation
quanta still apply. Native kernel metadata attention_statistics records none,
shared or warp_register_broadcast, and the reopening audit recomputes that fact.
Launch manifests expose the same fact together with block_x and shared_bytes.
Original source-operation records retain their baseline128-thread mapping;
physical launches consume the audited per-kernel manifest.

## Register dataflow and memory-order proof

1. Each lane computes its token's score using the existing ascending F32 dot
   loops. Mask-false and tail lanes initialize score to negative infinity and
   skip source dot reads. The score remains private; no shared store occurs.
2. All32 lanes execute shuffle-down max offsets16,8,4,2,1. A full-mask
   __shfl_sync(maximum,source_lane0,width32) broadcasts lane zero's complete
   result into every lane's raw-maximum register. Each derives the existing
   finite-safe maximum with zero fallback. Keep raw maximum for the tuple.
3. Compute exp(score-safe_max) once into a lane-private register. The existing
   F32 sum tree followed by another full-mask lane-zero broadcast supplies the
   same denominator to all lanes. A nonpositive denominator still uses one.
4. Normalize the private exponential into a private probability register.
   It stays immutable across all column groups. There is no memory publication.
5. Warp0 processes columns group*4+j, j0..3. Each group resets four private
   sums, reads only its owning lane's probability, traverses valid cache
   columns, and executes the four existing five-round shuffle trees. Lane0
   stores valid columns. Column and token predicates guard memory/stores,
   never collective participation. The broadcast return-preg must be read
   immediately after each typed WHIRL CALL.

The first two old fences existed to publish shared max and denominator to
other lanes. Register broadcasts remove those shared reads/writes. The third
existed to publish shared probabilities. Width=threads=32 proves every latent
lane reads its own token's private probability, so that fence has no remaining
memory dependency. This argument holds under independent thread scheduling;
it does not assume warp lockstep or treat a shuffle as a shared-memory fence.

The new local kernel has zero __syncwarp calls, zero __syncthreads calls and
zero shared arrays. Shuffle collectives still rendezvous all32 lanes. No lane
returns early before a collective, including empty chunks or one-token tails.
The proof does not extend to width64, a wider per-lane token span, shared cache
staging, a different column/token mapping or the chunk merge. Such changes must
invalidate the plan, ownership proof, body hash, resource ledger and comments.

## WHIRL and CUDA contracts

__shfl_sync is an ordinary effectful typed CUDA function CALL with signature
F4(U4 mask,F4 value,I4 source_lane,I4 width). Use mask0xffffffff, source0 and
width32. Its F4 return-preg is captured immediately into the selected scalar
symbol. Existing shuffle-down remains F4(U4,F4,U4,I4); the signed source-lane
type is validated independently. Normal whirl2c prints these selected CALLs.
No emitter infers or chooses the register optimization.

The readable warp_register_fused_attention_to helper consumes the selected
32-thread plan and emulates its F32 lane/tree arithmetic through the existing
serial helper. Its host arrays represent lane registers; actual mapped CUDA
comes from native scalar WHIRL and has no shared arrays in this local kernel.
Automatic VHO descriptions feed the same real COMMENT payloads to readable
CUDA and normal-whirl2c CUDA. They document each register lifetime, full-mask
participation, the removed memory dependencies, tails, guards and remaining
four-column grouping. Annotation has a separate bare/annotated native checkpoint.

## Numerical and resource consequences

Tuples remain [raw_m_c,d_c,normalized_z_c] at (chunk*rows+row)*(R+2). A finite
allowed one-token tail has d_c1 and z_c equal to that cache vector; an empty
chunk has d_c0,z_c0. The unchanged128-thread merge computes m=max m_c,
t_c=d_c*exp(m_c-m), D=sum t_c, z=sum(t_c/D)*z_c and LSE=m+log(D). Select zero
mass before exp for empty chunks, preserving all-empty zero latent/-infinity LSE.
F32 reassociation permission, score scale and safe guards remain unchanged.

At R16, four groups still cover columns0..3,4..7,8..11,12..15. The latent work
remains80 warp-level shuffle calls/block. Statistics retain ten shuffle-down
calls and add two broadcasts. Three memory fences and the entire local shared
allocation disappear (160 selected /156 prior compiled bytes -> zero).
The block/chunk count, compact tuple allocation, merge and projection stages
are unchanged at a fixed width. No global score/probability buffer is introduced.

Registers for score, raw/safe maximum, denominator and normalized probability
can extend scalar lifetimes; eliminating shared addresses can save other
registers. Report actual ptxas registers, shared bytes and spills instead of
assuming a pressure increase or a speedup. Supplied register allocation quanta
remain authoritative in the planning estimate. Coverage is not measured warp
occupancy or whole-call latency. The standard review envelopes still select
width64 (132 SMs/two CTAs), width32 (132 SMs/four CTAs) and width1024 (8 SMs).
The rank16 sm_90 compile reports32 registers, zero shared bytes and no spills
for the new local kernel, versus35 registers/156 shared bytes previously. The
width64 local kernel remains40 registers/288 shared bytes. These are compilation
facts, not measured GPU utilization or speed.
GPU timing and device sanitizer remain hardware-gated.

## Implementation order and acceptance

1. Version/seal the ownership decision and candidate resource footprint;
   initialize the transient flag for all kernels and clear it for the merge.
2. Materialize private score/statistics/probability scalars and two broadcasts;
   remove shared allocations only in the admitted one-token-per-lane path.
3. Verify typed builtins, immediate return-preg use, full-mask/source0/width32,
   resource/ownership metadata and body fingerprints on independent reopening.
4. Emit the corresponding readable helper and automatic comments. Preserve
   exact payload equality with real WHIRL COMMENT and normal CUDA emission.
5. Run independent expanded FP64 reference checks, forward/reverse synchronized
   host execution, unequal ranks/batches/heads, masked and empty chunks, large
   finite values and uneven tails. Reject forged statistics/footprint ledgers,
   wrong broadcast mask/source/width/type, missing broadcasts and stale bindings.
6. Compile sm_90 readable/mapped source with strict floating flags; retain ptxas
   resource/spill reports, genuine native device before/after.B/T and raw diffs,
   and bare/annotated COMMENT checkpoints. Compare unaffected kernel bodies
   after normalizing policy-derived names and verify v1/v2/v3 reproduction.
7. Publish a fresh v0.21 receipt with exact tools, committed sources, commands,
   hashes and isolated retained-tool replay. Keep all earlier immutable packets.

New native metadata, scalar WN/ST/TY changes and two typed CALLs are intentional
in the transformation diff; any unexplained executable churn is a blocker.
Reference tolerance remains rtol2e-5,atol2e-6. CPU/reference correctness and CUDA
compilation do not certify GPU execution or speed. Owner, successor, independent
reviewer and approving lead remain TBD; transfer acceptance remains open.
