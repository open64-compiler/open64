# Numerical Accumulation And Scaled GEMM Design v0.1

Date: 2026-10-25. Status: common contract, VHO segment planning and a verified
scalar WHIRL adapter implemented; native FP8/FP4 support remains planned.
Current D/S1/T2 device kernels remain scalar FP32. Open64
`F4` means four-byte FP32, not four-bit FP4.

## 1. Purpose And Terms

Model operand quantization and reduction accuracy separately. A declared
FP32 result does not establish the precision of every internal addition.
Hopper FP8 and Blackwell FP4 require distinct instruction-specific profiles;
neither a family name nor an output type is a numerical guarantee.

| Term | Definition |
| --- | --- |
| Significand | Significant binary digits, often called the mantissa. Alignment, rounding and cancellation can lose significant bits; a normalized carry is not itself overflow. |
| Storage format | Encoding and packing of an operand in memory. |
| Product precision | Arithmetic precision of the product of already-quantized operands; it cannot restore quantization losses. |
| Internal accumulation | Addition performed inside the selected MMA implementation, distinct from its visible result type. |
| Outer accumulator | Explicit accumulator used to combine completed partial sums, such as CUDA-core FP32 registers. |
| `K_total` | Full reduction length for an output element. |
| `BK` | Reduction extent of a staged operand tile; a movement/resource parameter. |
| `K_inst` | Reduction extent of one selected hardware MMA instruction. |
| `K_scale` | Number of operand elements sharing a block scale along K. |
| `N_C` | Maximum planned reduction span between explicit partial-sum merges; recorded in reduction elements, not instruction count. |
| Promotion | Completing a partial MMA sum and merging it into the outer accumulator; not recovery of bits already discarded. |
| Evidence | Separately identified ISA contract, published empirical observation, compiler-source behavior, or local native measurement. |

`BK`, `K_inst`, `K_scale`, `N_C`, A/B slot count and prefetch distance are
different parameters. They may have compatibility constraints but must not
be aliases for one another.

## 2. Hopper FP8: Specific Evidence And Mitigation

DeepSeek-V3 reports limited effective accumulation precision on its H800 FP8
path, approximately 14 bits, and nearly 2% maximum relative error in a tested
K=4096 GEMM. Its mitigation periodically combines Tensor Core partial sums
using CUDA-core FP32 additions; its reported interval is 128 reduction
elements. This is empirical evidence for that configuration, not a universal
error bound, universal hardware bit count, or universally optimal interval.
[DeepSeek-V3, Sections 3.3.2 and 3.5.2](https://arxiv.org/html/2412.19437v2#S3.SS3.SSS2)

NVIDIA's current PTX WGMMA documentation independently notes that FP8 inputs
with an FP32 result can use accumulation precision between half and single
precision. Record that ISA caveat separately from the measured 14-bit finding.
[PTX WGMMA contract](https://docs.nvidia.com/cuda/parallel-thread-execution/index.html#asynchronous-warpgroup-level-matrix-multiply-accumulate-operation-using-wgmma-mma-async-instruction)

The proposed Hopper plan must explicitly carry E4M3/E5M2 input formats,
WGMMA variant, internal-precision evidence, partial and outer accumulator
locations/types, requested and effective promotion intervals, scale boundaries,
merge order, and completion dependencies. Do not infer full internal FP32
precision from FP32 registers or a library result descriptor.

Illustrative algorithm, after numerical permission and target legality:

```text
outer = FP32 zero
for each planned reduction segment in increasing K order:
    partial = fresh Tensor Core accumulator; span = 0
    for each legal MMA fragment in this segment:
        acquire ready A/B operands
        issue FP8 MMA into partial
        span += K_inst
    wait for partial MMA completion
    apply this segment's scales at their defined precision
    outer = FP32_add(outer, scaled_partial)
    retire partial only after its merge
store permitted output conversion(outer)
```

Segments must not mix differently scaled unscaled partial sums. If scales
change along K, subdivide or use an explicitly valid scaled representation.
Prove each segment span meets the acceptance contract. An implementation may
flush earlier at a tile boundary; it must not silently exceed the permitted
span by rounding to an instruction boundary. Report requested versus actual
spans, including tails. If an instruction is indivisible and too wide, reject
that instruction path or use the permitted fallback.

Completion waits, register reads, FP32 merges and subsequent WGMMA reuse need
explicit ordering. A TMA-ready barrier proves operand availability, not MMA
result completion. SMEM slot recycling must also wait for all MMA readers.

## 3. What Triton Actually Does

Source inspection pinned to upstream revision
`2c9fb87df0e656cefeb5d18d3ac1b06c1e0a25d1`. These are code-path observations,
not results from running Triton on a GPU. Main-branch documentation can lag
implementation; use the pinned files for reproducibility.

### 3.1 Hopper FP8 Dot

`tl.dot` accepts `max_num_imprecise_acc`, propagated to dot/WGMMA IR.
The inspected NVIDIA backend defaults this value to `2**30` for SM89/SM90,
otherwise zero. Thus ordinary Hopper FP8 dot does not automatically request
short partial sums. The explicit API value is checked against the dot's local
K, not necessarily the entire application's reduction length.
[Backend defaults](https://github.com/triton-lang/triton/blob/2c9fb87df0e656cefeb5d18d3ac1b06c1e0a25d1/third_party/nvidia/backend/compiler.py),
[Dot semantic handling](https://github.com/triton-lang/triton/blob/2c9fb87df0e656cefeb5d18d3ac1b06c1e0a25d1/python/triton/language/semantic.py)

The WGMMA operation's `needsPartialAccumulator()` tests FP8 operands, FP32
result, and a threshold no larger than local K. Its verifier rejects a
threshold below 32 for this FP8/FP32 WGMMA path. Zero is therefore not a
portable Hopper request for arbitrarily precise accumulation.
[NVIDIA operation verifier](https://github.com/triton-lang/triton/blob/2c9fb87df0e656cefeb5d18d3ac1b06c1e0a25d1/lib/Dialect/TritonNvidiaGPU/IR/Ops.cpp)

WGMMA lowering maintains a partial accumulator and a count incremented by
instruction K. At the threshold or end of local K it combines the partial
through LLVM floating additions, then resets the partial state. This is the
same broad mitigation pattern as periodic promotion, but it is not a theorem
about end-to-end LLM accuracy. Non-instruction-aligned thresholds must not be
assumed to imply an exact element boundary.
[WGMMA lowering](https://github.com/triton-lang/triton/blob/2c9fb87df0e656cefeb5d18d3ac1b06c1e0a25d1/third_party/nvidia/lib/TritonNVIDIAGPUToLLVM/DotOpToLLVM/WGMMA.cpp)

For a compatible local dot K>=128, explicitly passing
`max_num_imprecise_acc=128` expresses a bounded-accumulation request. This is
not equivalent to choosing `BLOCK_K=128` while allowing a persistent native
accumulator across loop iterations. Inspect the lowered operation sequence
and completion ordering; neither a Python dtype nor a tuning constant alone
is evidence that promotion occurred. `input_precision="ieee"` controls FP32
input handling, not the Hopper FP8 accumulation caveat.

### 3.2 Blackwell Block-Scaled FP4

Triton's block-scaled tutorial handles packed E2M1 operands and separate
scale tensors. NVFP4 uses E4M3 scales over 16 elements; MXFP4 uses E8M0 scales
over 32. It loads both payload and scale tiles, rearranges scales into the
logical shape, calls `tl.dot_scaled`, maintains an FP32 accumulator and
converts only for the requested output. Scale preshuffling and TMA descriptors
are explicit parts of the example, not inferred from scalar element width.
[Block-scaled tutorial](https://github.com/triton-lang/triton/blob/2c9fb87df0e656cefeb5d18d3ac1b06c1e0a25d1/python/tutorials/10-block-scaled-matmul.py)

The NVIDIA IR has a separate `TCGen5MMAScaledOp` carrying operand memdescs,
A/B scales, accumulator dependency and completion barriers. MMAv5 lowering
selects block-scaled instruction variants and scale addresses in TMEM.
This is not the Hopper WGMMA partial-accumulator control reused for FP4.
[Scaled MMA IR](https://github.com/triton-lang/triton/blob/2c9fb87df0e656cefeb5d18d3ac1b06c1e0a25d1/include/triton/Dialect/TritonNvidiaGPU/IR/TritonNvidiaGPUOps.td),
[MMAv5 lowering](https://github.com/triton-lang/triton/blob/2c9fb87df0e656cefeb5d18d3ac1b06c1e0a25d1/third_party/nvidia/lib/TritonNVIDIAGPUToLLVM/DotOpToLLVM/MMAv5.cpp)

Do not generalize SM100 `tcgen05`/TMEM to every product named Blackwell.
Triton's accelerator selection distinguishes SM100-class targets from SM120;
the latter has different MMA lowering. Native capability, toolchain, exact
instruction and validated recipe must identify the target profile.
[MMA family selection](https://github.com/triton-lang/triton/blob/2c9fb87df0e656cefeb5d18d3ac1b06c1e0a25d1/lib/Dialect/TritonGPU/Transforms/AccelerateMatmul.cpp)

The FP32 tensor-wide scales used by a complete NVFP4 quantization recipe are
not automatically created by passing packed E2M1 values to `dot_scaled`.
Record whether they are folded into quantization, applied in an epilogue or
supplied elsewhere, exactly once. A low-level scaled-dot kernel does not
choose the model's calibration recipe or prove model accuracy.
[NVFP4 representation](https://docs.nvidia.com/deeplearning/transformer-engine/features/low_precision_training/nvfp4/nvfp4.html)

## 4. Proposed Common Records And Ownership

These are architectural record names. Section 7 identifies the implemented
C++ subset; the full numerical-policy fields below are not all implemented.

| Owner/tag | Record or extension | Required information |
| --- | --- | --- |
| Common Compiler Substrate | `CommonNumericalContractIR` | Source/input/output formats, allowed narrowing/reassociation/contraction, rounding/saturation/subnormal policy, acceptance-evidence identity and provenance. |
| Common Compiler Substrate | `CommonAccumulationPlanIR` | Product/internal/outer precision evidence, accumulator tiers, reduction order, segment map, requested/effective `N_C`, reset/complete/scale/merge events, fallback. |
| Common Compiler Substrate | Representation and tile plans | Payload packing, scale objects, scale grouping/layout, instruction shape, separate payload/scale/accumulator allocation ledger. |
| Common Compiler Substrate | Pipeline and cost records | Dependencies, waits, merges, scale transfers, spills, overlap and evidence confidence; no target-selection policy in common/com. |
| NVIDIA target extension | Per-instruction numerical profile | Exact SM and toolchain constraints, WGMMA caveat or independent Blackwell contract, legal segments, register/TMEM costs and measured versus specified facts. |
| AI-domain-specific | Model acceptance policy | Checkpoint/recipe/calibration identity, layer exceptions, task metrics and thresholds, separate KV-cache and context-length acceptance. |
| VHO/P7-P9, active PU | Analysis, legality and plan selection | Enumerate and cost only accepted recipes; select full tile/movement/accumulation tuples, construct and verify WHIRL. No cross-PU inference without IPA scope. |

Unknown internal precision remains unknown, not an invented bit count.
Acceptance can use a validated target-specific recipe where an exact error
proof is unavailable, with scope and confidence explicit. Unknown or stale
evidence that cannot satisfy the contract rejects the narrowed candidate.
Retain the source-precision fallback. Already-quantized operands still require
an accumulation contract; their dtype does not authorize arbitrary reduction
reassociation. The current S1 fallback is FP32, not an FP8 accuracy fallback.

## 5. Joint Planning And Profitability

P0 records source precision and permission. P6/P7 propose representations,
instruction families, scale layouts and accumulation segmentations. P8 builds
the combined transfer/MMA/merge event graph. P9 gates numerical legality,
then compares complete plans before any transformation is committed. P11
joins target-native numerical and performance evidence with exact provenance.

For each output tile, derive segment counts from the actual segment map,
not just `ceil(K_total/BK)`. In the simple uniform case with no extra tile or
scale flushes, `segments = ceil(K_total/N_C)`. Initial assignment, later
merges and tail handling must follow the emitted sequence.

```text
cost = critical_path(operand_and_scale_transfers, MMA, completion_waits,
                     partial_reads, rescaling, outer_merges, output)
       + launch_and_nonoverlapped_setup
```

Use resource-constrained overlap: promotion consumes registers, CUDA-core
issue capacity and synchronization opportunities; on SM100, scale and
accumulator allocation also consume TMEM. Increased live ranges may reduce
residency or cause spills. Do not add the same wait as both sync cost and
unhidden compute cost. No fixed overlap credit merely because two warpgroups
or two staging slots exist. Report conversion, calibration/repacking and
reuse amortization assumptions separately.

Search a bounded set of instruction-aligned, numerically accepted intervals;
128 is an initial research-derived test case, not a hardware constant or a
default supported by our current compiler. Permission to narrow does not
authorize an unsafe promotion interval. No latency benefit can override a
failed numerical gate. Numerical rearrangement also requires source permission.

Blackwell uses separate NVFP4 and MXFP4 profiles, not an inherited Hopper
14-bit penalty. Count scales and physical layout padding in memory/resource
costs. Scale granularity protects quantization range; it does not itself set
or guarantee accumulation precision.

## 6. Trace And Acceptance Work

Extend the compact trace under a reviewed future schema, not by changing v7
silently. Proposed positional records must state columns once in the header:

```text
NUM_CONTRACT contract source inputs output permission recipe evidence
ACC_CAND plan target instruction internal outer requested_NC effective_max_NC segments scale_group legality reason source_line
ACC_COST plan wait merge scale_move rescale pressure total units confidence
ACC_ACTION plan emitted_segments waits merges accumulator_tier proof
```

`tile.log` must identify instruction and segmentation alternatives;
`fetch-pipeline.log` must explain payload/scale readiness and MMA/merge
dependencies; `physical-plan.log` must compare complete costs and numerical
rejections. `optimization-summary.log` must state selected precision,
internal-precision evidence, requested/effective intervals, actual WHIRL
actions, source-format fallback and remaining native/model acceptance gates.
`optimization.log` retains all phase details. Preserve raw `.t`, before/after
`.B/.T`, WHIRL-derived `.cu`, later PTX/SASS, and recipes/receipts together.

Next implementation slices, not completed by this document:

1. Add policy-free records/verifiers and per-instruction target profiles;
   distinguish published 14-bit evidence from guaranteed target facts.
2. Implement Hopper FP8 partial/reset/wait/scale/merge WHIRL and verify actual
   segment spans across BK changes, tails and pipeline-slot reuse.
3. Compare accepted tile/movement/accumulation tuples with the FP32 fallback;
   audit selected-plan trace against actual emitted operations.
4. Test large K, large-plus-small contributions, alternating signs,
   cancellation, outliers, saturation/subnormals and changing scale groups.
   Compare against both original high-precision operands and a high-precision
   reference of quantized operands to separate quantization from accumulation
   error. Use absolute-plus-relative and norm metrics, not relative error alone
   near zero. Host simulation does not certify Hopper internal arithmetic.
5. Implement Blackwell FP4 separately: NVFP4/MXFP4 packing, scale layouts,
   scale movement, target-specific MMA/TMEM lifetime and output/global scales.
6. Certify exact instruction sequences, target-native numerical results,
   cost calibration and end-to-end model quality when hardware is available.

## 7. Implemented Contract Slice

Partial-sum promotion is adopted as an internal semantic obligation, not a
required out-of-line function call. The selected WHIRL must expose bounded
partial computation, completion, scale application when required, ordered
outer merge and reset. Standard WHIRL is reused; no new opcode, mapped-image
row or binary format is added. A target implementation may inline the entire
sequence. Inlining removes call overhead, not the waits, additions or live
accumulator storage. Promotion means merging partial results, not converting
every FP8 input to FP16 and not recovering previously discarded bits.
CUTLASS is a reference mechanism: its
[accumulation helper](https://github.com/NVIDIA/cutlass/blob/main/include/cutlass/gemm/collective/fp8_accumulation.hpp)
exposes the wait/merge/reset sequence and its device methods use
[force-inline device annotations](https://github.com/NVIDIA/cutlass/blob/main/include/cutlass/detail/helper_macros.hpp).
Our contract adopts the semantics, not the C++ template ABI or a guarantee
that every target uses the same promotion interval.

The implemented initial APIs are:

| Layer | Implemented interface and limit |
| --- | --- |
| Common substrate | `dsl_accumulation.h/.cxx`: runtime-only `DSL_NUMERICAL_CONTRACT_RECORD`, `DSL_ACCUMULATION_PLAN_RECORD`, segment records, structural verification and compact printing. Initial formats include F32, E4M3, E5M2 and E2M1; partial/outer records require F32. |
| VHO | `dsl_accumulation_opt.h/.cxx`: active-PU, -O3-only segment planning; numerical permission/evidence gates; exact coverage and tail accounting; instruction alignment and optional software scale-group subdivision. |
| Scalar materializer | `VHO_DSL_accumulation_materialize_f32`: detached ordinary DO_LOOP/LDID/ILOAD/MPY/ADD/STID/ISTORE code, private partial resets and outer merges. Caller commits only after verification. This adapter is synchronous and needs no completion instruction. |
| Postcheck | `VHO_DSL_accumulation_verify_f32`: checks the selected sequence, types, addresses, bounds, nesting and unique WN ownership using Open64 WN equivalence/traversal. Wrong scope, symbol aliasing or unsupported native profiles cannot publish an output block. |
| Hopper profile | Planning only: K=32 WGMMA reduction quantum, FP8 inputs, requested/effective element intervals. Internal bits remain unknown; ISA evidence ID 2 records the WGMMA precision caveat, not a fabricated guaranteed 14-bit width. |
| Blackwell profile | Rejected until independent scaled-MMA, packing, scales and resource semantics are implemented. It cannot reuse Hopper's limitation or scalar lowering. |
| Trace/report | Separate `AI_ACC 1` contract/plan/segment/action/rejection stream and standalone report audit. Existing `AI_OPT 7` remains unchanged. |

The scalar profile uses ISA evidence ID 1 for F32's 24-bit significand.
Contract evidence IDs identify caller-accepted numerical recipes; they are
not populated by guessing LLM accuracy. VHO supplies internal precision
facts; requests cannot override them with an invented bit count. The planner
rounds a requested interval down to an instruction boundary, subdivides at
software scale groups and records any final neutral padding separately as
issued versus valid K. It caps expansion at 4096 segments and rejects unsafe
I4 geometry. `BK` remains independent of the promotion interval.

This is an opt-in internal service, exercised by a linked native producer;
it is not automatically invoked for existing D/S1/T2 matmul kernels and adds
no `openpy` driver option. The generic scalar fixture is a dot product, not
a new optimized GEMM. Hopper can be planned, but attempts to pass it through
the scalar materializer fail. No Hopper WGMMA code generation, native GPU
accuracy fix or Blackwell FP4 implementation is claimed.

The records currently live for the owning phase. The executable reset/merge
sequence survives ordinary mapped-image writing; semantic annotations for
protection through later optimizing phases and automatic phase-boundary
verification remain required before production native FP8 enablement. A
later pass must not collapse bounded partial sums into an unrestricted native
accumulation chain. The caller must invalidate/revalidate the contract after
changes to reduction order, instructions, scales, loop bounds or accumulators.

`test_accumulation_contract.py` retains independently produced same-PU before
and after `.B`, source-interleaved `.T`, the raw diff and WHIRL-derived `.cu`.
The producer checks 45 scalar K/interval combinations, scale subdivision,
14 malformed contracts, four altered WHIRL sequences, -O0/-O1/-O2 rejection,
aliased private-symbol rollback and unsupported Hopper/Blackwell lowering.
Host execution checks exact and varying-sign inputs plus an absorption stress
case; an improvement for that case is not a universal error bound.

Reports start with `What it summarizes`. `accumulation.log` and
`optimization.log` retain the complete accumulation stream;
`optimization-summary.log` distinguishes scalar materialization from planned
Hopper and rejected native paths. They are fixture-scoped, not an assertion
of coverage by every P0-P11 phase of the matmul compiler. The standalone tool
can audit a retained trace without rebuilding:

```sh
python3 osprey/be/vho/tests/ai_matmul_cuda/test_accumulation_contract.py \
  --trace /absolute/path/accumulation.t --output-dir /new/report/directory
```

Existing FP32 matmul option behavior and WHIRL binary format are unchanged.
