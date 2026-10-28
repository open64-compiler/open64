# MMA4-H: First Hopper WGMMA Lowering

## Bounded Contract

From merged MMA3 PR197 (`c5aec7b1`), add an explicitly requested, active-PU
VHO lowering service. The existing F32 planner and MMA3 typed SIMT remain the
default. No production option, automatic selection, target inference, timing
claim or library dependency is added. Minimum transformation level is O3;
lower levels and disabled requests preserve the caller's SIMT body/output.

Initial recipe: exact `sm_90a`, dense F16/BF16 U2 buffers, F32 output,
`m64n64k16` shared/shared WGMMA, one 128-thread warpgroup per CTA, no TMA,
swizzle, padding, split-K, fusion or epilogue conversion. M/N are multiples of
64 and K is a positive multiple of 16. Each CTA owns one 64x64 output tile.
Each K step cooperatively stages 2048 bytes per operand, makes generic writes
visible to the async proxy, synchronizes the CTA, executes one fully waited
atom, then joins all readers before shared-memory reuse. Excess X/Y blocks
return uniformly; wrong launch dimensions return uniformly before any output
or collective. Caller chooses the verified SIMT fallback before launch when
target, shape, acceptance or configured resources do not meet this recipe.

WGMMA is not equivalent to the strict MMA3 scalar rounding contract merely
because its accumulator is F32. Require the same accepted numerical ID,
F16/BF16 source/input and F32 product/partial/outer/output, reassociation
permission, and explicit acceptance of ISA reduction order, contraction,
rounding, subnormal and nonfinite behavior. No input narrowing is permitted.
These permissions are caller policy, not numerical certification. The MMA3
SIMT body implements a legal stronger ordered/separately-rounded alternative
within that accepted contract, and is verified before replacement.

## WHIRL Representation And Continuity

Reuse the standard `ASM_STMT`/`ASM_INPUT` representation, volatile and memory
clobber flags, ordinary U2 loads/stores, scalar loops, aligned shared symbols,
private F4 fragment array and explicit CTA barrier calls. No opcode, intrinsic
ID, node layout, mapped-image section or type encoding changes. The lowered
image is authoritative; `whirl2c` prints actual assembly operands and flags,
not a pattern-matched scalar-loop-to-MMA template or a new scheduling policy.
Review the specification's direct-kid/effect/continuous-lowering principles
and existing `clang2whirl` extended-assembly construction protocol. As in MMA3,
the checkout lacks `WHIRL.pdf`; the architectural baseline is the
[Open64 WHIRL specification](https://www.mcs.anl.gov/OpenAD/open64A.pdf).

A fully waited atom is one opaque *effectful assembly statement*, not an
asynchronous operation whose incomplete result escapes. Three ordinary input
kids reference the private 32-F4 fragment and the two shared allocations.
Its literal PTX loads fragment registers, constructs uniform descriptors,
executes `wgmma.fence`, `mma_async`, `commit_group`, `wait_group 0`, and only
then stores completed F32 fragments. The memory clobber protects fragment and
shared accesses at the C/WHIRL boundary; the enclosing CTA barrier joins all
asynchronous readers before slot reuse. A CTA barrier alone is not MMA done.
The constructor and postchecker validate actual instruction text, operands,
effects, packing, geometry, ownership, source positions and ordering.

No-swizzle K-major packing for each 64x16 operand, in U2 elements:
`(mn/8)*128 + (k/8)*64 + (mn%8)*8 + k%8`.
Descriptor LBO=128 bytes, SBO=256 bytes, base/swizzle/reserved bits zero;
start address is 16-byte aligned and encoded as `(shared_address >> 4)&16383`.
For B, the source row-major KxN buffer is explicitly repacked to this K-major
64x16 layout. The fragment's register `reg` in thread `tid` owns:
`row=(tid/32)*16+(tid%32)/4+((reg%4)/2)*8`,
`column=(reg/4)*8+(tid%4)*2+reg%2`.
All 4096 output coordinates have exactly one owner.

The conservative compound carrier deliberately materializes a private 128-byte
F32 fragment between atoms. NVIDIA's compiler currently places that array in
a 128-byte local stack frame; each atom's accumulator itself is 32 F32 PTX
registers. This is not a claim of register residency across K steps, absence
of local-memory traffic, or performance advantage over SIMT. Direct extended
asm copyout/tied-register fragments and pipelined scheduling are follow-on
optimization work, not inferred by the CUDA emitter.

## Target Evidence And Validation Boundary

Pinned primary references: NVIDIA
[PTX 8.7 WGMMA](https://docs.nvidia.com/cuda/archive/12.8.1/parallel-thread-execution/index.html#asynchronous-warpgroup-level-matrix-instructions),
sections 9.7.15.5 (descriptors/fragments/arithmetic) and 9.7.15.7 (ordering),
and [CUTLASS v3.8.0 instruction bindings](https://github.com/NVIDIA/cutlass/blob/v3.8.0/include/cute/arch/mma_sm90_gmma.hpp).
These document capability; they are not evidence of Open64 GPU correctness.
Exact target compile checks, independent fragment/packing/protocol audits,
mapped before/after reopen, rejected/mutated bodies, unchanged default SIMT,
shared-consumer link checks and retained durable evidence are required.
Native GPU arithmetic, resource usage and timing remain explicit unknowns
unless actually measured on the required device. Host emulation validates
lowered geometry/packing/progress only, not ISA floating-point guarantees.

## Validated Local Boundary

| Gate | Result |
| --- | --- |
| Actual same-PU before/after artifacts | Ten F16/BF16 cases: atom, multiple output tiles, K=16/32/48/64, rectangular output, master simplifier on/off. Before is the verified MMA3 typed SIMT GRID body; after contains real volatile/memory-clobber assembly WNs, not scalar stand-ins. |
| Native target compilation | All twenty before/after CUDA sources compile to PTX and `sm_90a` cubin with NVIDIA CUDA 12.8.1, NVCC 12.8.93. Both formats reject `sm_90` in the real assembler, without publishing a partial cubin. |
| Assembler resource evidence | Ten WGMMA kernels report 58 or 64 registers, 4096 shared bytes, 128-byte local stack frame and zero spill stores/loads. These are compiler results, not measured native occupancy or performance. |
| Negative/invalidated source | 380 preflight rejections include disabled/default, O0/O1/O2, exact suffix/profile, dtype, all five ISA freedoms, acceptance identity, shape, physical alignment/storage, alias, resource and changed canonical-source checks. |
| Changed executable bodies | 160 body rejections include missing fence/commit/wait, changed descriptors/fragments/guards, removed volatile/memory effects, wrong pointer operands, changed call flags and shared WN children. Twenty stale-generation/evidence checks and ninety independent CUDA-source corruptions reject. |
| Geometry/protocol host checks | Forty final O0/O3 before/after runs compare actual emitted addressing/repacking/epilogues to independent original-buffer dense references. The test-only completed-atom substitution checks publication/completion/reuse phases, exact fragment coverage, immutable inputs, output canaries and wrong/excess launch guards. It does not simulate ISA numerical freedoms. |
| Preserved typed SIMT | All eighteen MMA3 configurations pass 88 execution runs, exhaustive 65536 encodings per format, adversarial numerical cases and existing malformed-body checks. |
| Default compatibility | Authenticate all 1790 files in the immutable MMA3 receipt. Fifteen fresh F32 and eighteen typed-SIMT configurations have 66 byte-identical full `ir_b2a -st -src` dumps, unchanged CUDA bodies except translator date, and unchanged F32 costs/selection/compute/shadow rows. Raw `.B` difference offsets are retained without normalization or byte-equality claims. |
| Reader compatibility | The MMA3 reader independently reopens four new before/after images, including the actual Hopper assembly carrier, with identical full dumps. |
| Other regressions | All 123 Python host tests pass. Existing MMA1 four-profile/276-negative inventory, MMA2 496-schedule/1488-corner oracle, scalar accumulation, syntax/operator-layout matrix and six-case miniature CKKS2C checkpoint pass. |
| Link boundary | Rebuild `be.so`, `be`, `lw_inline`, `whirl2c`, `ir_b2a` and native fixtures. Backend/shared consumer have no `DSL_Builder_*` or `Json::` symbols and no new shared-library dependencies. |

The source impact is bounded: VHO exposes the existing typed binding preflight
without changing its behavior, adds an otherwise uncalled explicit target
lowerer, and the CUDA printer fixes the existing input-only assembly protocol.
No FHE producer, transaction, image schema, generic gatekeeper or non-CUDA
assembly path changes. The conservative impact selector lists unknown MMA
paths; the focused native/syntax/link/checkpoint and immutable-default parity
evidence are the semantic resolution, not a full FHE model recertification.
The previously recorded MMA3 monolithic/isolated builder failures are not
claimed repaired or represented as passing here.

Review evidence includes actual `.B`, source-interleaved `.T`, before/after
diffs, WHIRL-derived `.cu`, native `.ptx`/`.cubin`, assembler diagnostics,
exact commands, all five phase reports, host-emulation sources/results and
the official checksummed CUDA redistribution archives. The reference diagram
was visually checked against the emitted 4096-element fragment bijection.
Only target assembly and host geometry/protocol are certified by these tests;
native GPU arithmetic, runtime launch/resource behavior and timing remain open.
