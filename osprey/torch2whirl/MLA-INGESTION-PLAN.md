# Absorbed MLA Standalone Ingestion Test

Version v0.6 dated November 5 2026. This fixture extracts the absorbed single
query MLA algorithm into a deterministic read-only tensor program. Short and
long contexts execute the same source. CPU FP32 eager and genuine FX execution
are checked against an independently expanded scalar FP64 reference.

The controlling frontend workflow is
[WHIRL DSL infrastructure](../../doc/WHIRL-DSL-INFRASTRUCTURE.md).
The [building block ingestion plan](BUILDING-BLOCK-INGESTION-PLAN.md) supplies
the existing source capture conventions. Native MLA ingestion now emits real
typed WHIRL through both the public API and standalone driver, with independent
mapped-image reopening. Compilable WHIRL-derived reference CUDA can now be
emitted before lowering. A VHO-owned primitive lowering also generates ordinary
device WHIRL and executable CUDA kernels for both contexts. Short-context
producer fusion and long-context parallel split reductions are implemented in
the focused lowering pipeline; GPU execution validation remains open.
The source graph consumes the published
[native tensor expression contract](../../doc/AI-COMPILER-NATIVE-TENSOR-EXPRESSION-CONTRACT-v0.1.md).

## Algorithm and boundary

[AbsorbedMLA](python/tests/models/absorbed_mla.py) expresses the absorbed branch
of [DeepSeek V3 inference/model.py](https://github.com/deepseek-ai/DeepSeek-V3/blob/main/inference/model.py),
class MLA, as explicit matrix multiplications. This is a standalone
re-expression of the algorithm, not a verbatim copy or full upstream model.
The upstream moving main branch was inspected November 5 2026; the retained
local source and SHA256 manifest identify the actual test implementation.

For each batch b, head h and visible cached token j, the program computes:

- Absorbed query: q_latent = transpose(Uk) q_nope.
- Score: alpha times (dot(q_latent, c_j) + dot(q_rope, k_rope_j)).
- Probabilities: stable masked softmax over visible cached tokens.
- Latent output: sum_j probability_j times c_j.
- Per-head output: Uv times latent output.

The original model alpha remains explicit in the module. Absorption must not
replace it by a scale derived from the latent rank. Both fixtures use
alpha = 1/sqrt(Dn + Dr), while the module also accepts another finite explicit
model scale, including a model-specific positional scaling adjustment.

Inputs contain already normalized latent cache values and already rotated
query/key positional components. Cache append, projections from hidden states,
normalization, RoPE generation, final cross-head output projection, training,
dropout, paged-cache address translation and tensor parallel collectives are
outside this bounded core. The cache is a shared dense latent tensor, not
expanded per-head K/V storage. There are no downloads or external weights.

| Input | Shape | Meaning |
| --- | --- | --- |
| query_nope | B H Dn | Non-positional query before absorption |
| query_rope | B H Dr | Positional query after RoPE |
| cache | B L R | Shared normalized latent cache |
| rope_cache | B L Dr | Shared positional keys after RoPE |
| key_up | H Dn R | Per-head key reconstruction weights |
| value_up | H Dv R | Per-head value reconstruction weights |
| allowed | B H L | Boolean runtime visibility, True permits attention |
| output | B H Dv | Per-head value-projected attention output |
| lse | B H | Natural-log score normalization |

All floating inputs and outputs in this source lane are CPU FP32; the oracle
uses Python double precision and math.fsum. Inputs must be contiguous, finite,
rank three, compatible and have positive extents. Preflight runs outside the
captured forward function. An all-masked head returns zero output and
negative-infinity LSE. Empty physical cache extents are rejected by this bounded
fixture; all-masked nonempty storage tests zero visible context. This convention
must be adapted explicitly when comparing an external kernel with a different
empty-row LSE sentinel.

## Short and long contexts

Both cases use B=1, H=3, Dn=8, Dr=4, R=16 and Dv=6. Short has L=127; long has
L=8193. Unequal dimensions expose transpose errors, and uneven sequence lengths
exercise tails. These are practical correctness inputs, not production model
dimensions or a measured scheduling crossover. No runtime dispatch threshold
is inferred from the names.

Each input has a separate local deterministic generator. The cases preserve
the same query and weights; factories return fresh nonaliasing inputs and do
not advance global RNG state. Inputs and weights remain runtime operands in
the FX graph. No short/long choice is embedded in forward and no JIT is used.

The compiler may eventually choose unsplit short execution or split long
execution with stable partial-softmax merge. This source milestone neither
implements that transformation nor authorizes FP reassociation. It supplies
the same oracle for checking those later plans.

## Correctness and capture

The [independent reference](python/tests/models/mla_reference.py) reconstructs
keys and values for each token before evaluating attention. It never calls
the absorbed implementation and imports neither torch nor numpy. The
[dependency-free tests](python/tests/test_mla_reference.py) check known answers
for a single token, uniform probabilities, masks and large logits.

The [PyTorch tests](python/tests/test_mla_capture_optional.py) compare eager
and real FX outputs and LSE against that reference at rtol=2e-5 and atol=2e-6.
They also check identical operation census for both lengths, exact eager/FX
replay, no mutation, fresh deterministic inputs, batch isolation, single-token
value projection, nonuniform masks, all-masked heads, excluded-cache
perturbation, original scale, stable extreme logits, invalid-input rejection,
artifact replay, checksums and no-overwrite behavior.

These tolerances apply to this FP32 source fixture only. They do not establish
BF16 numerical acceptance, arbitrary model accuracy, native correctness or a
performance improvement.

[The runner](python/tests/mla_standalone.py) also calls the existing
export_to_whirl mapper with backend="mock" when open64_dsc is available. The
current checkout maps the complete graph to typed common operations. This
mock probe does not emit an image. Native image and reader/driver certification
use mla_native_review.py separately. Without open64_dsc, a portable source
bundle reports not_available.

The census classifies inputs/outputs as region contracts and captured tensor,
mask and numerical operations as common substrate requirements. In particular,
rank-broadcast matmul, views, predicate/select, masked-fill and explicit
normalization now use native common contracts. Numerical domain policy, cache
validity, all-masked semantics and alpha remain in the MLA semantic boundary.
Common/com owns logical definitions, types and verification; Python does not
construct WN or mapped-image records.

## Running the test

From osprey/torch2whirl in an environment with CPU PyTorch:

```sh
PYTHONDONTWRITEBYTECODE=1 PYTHONPATH=python:python/tests python3 -m unittest -v test_mla_reference test_mla_capture_optional
PYTHONDONTWRITEBYTECODE=1 PYTHONPATH=python:python/tests python3 python/tests/mla_standalone.py
PYTHONDONTWRITEBYTECODE=1 PYTHONPATH=python:python/tests python3 python/tests/mla_standalone.py --artifact-dir /absolute/fresh/mla-run
```

Use --case short or --case long for one case. --short-length and --long-length
override extents for reduction or debugging; the report always records actual
shapes. If no --artifact-dir is supplied, OPEN64_DSL_TEST_ARTIFACT_DIR selects
a fresh mla-absorbed child. An existing destination is rejected.

On this macOS host, use the existing open64:torch2whirl-torch-test Docker image
(PyTorch 2.4.1+cpu), with the repository mounted read-only:

```sh
docker run --rm --platform linux/amd64 -v "$PWD:/src:ro" -w /src/osprey/torch2whirl -e PYTHONDONTWRITEBYTECODE=1 -e PYTHONPATH=python:python/tests open64:torch2whirl-torch-test python3 -m unittest -v test_mla_reference test_mla_capture_optional
```

The Docker command starts at the Open64 repository root. To retain artifacts,
also mount a fresh host staging directory at /artifacts and invoke the runner
with --artifact-dir /artifacts/capture. Ordinary unittest discovery includes
the four dependency-free tests; make python_torch_test discovers the eleven
new PyTorch tests through the existing test_*capture_optional.py pattern.

## Retained evidence and next gates

The artifact writer retains typed inputs.pt, expected FP64 reference.pt,
actual FP32 actual.pt, FX graph.txt, generated FX forward.py and census.json
for each case. It also retains exact source/tests, a version/environment/error
report, a described capture-summary.log and SHA256SUMS. Use torch.load with
weights_only=True for these tensor dictionaries. From a portable source/
directory with PyTorch installed, run python3 mla_standalone.py to replay;
the native mapper diagnostic is available only with the repository Python
package on PYTHONPATH.

Source/capture evidence alone is not a native compiler milestone receipt.
The native review follows
[review artifact retention](../../doc/REVIEW-ARTIFACT-RETENTION-POLICY.md),
including durable primary-checkout storage. The source-only runner fabricates
no .B, .T, .t, PTX or CUDA image.

Native review runs each context in its own process, emits 46 typed operations
with seven runtime formals and two results, reopens the real ELF mapped image
using ir_b2a -st -src, then repeats production through the standalone driver.
No cache, query or weight tensor values are embedded as constants. Both images
are about 47 KB despite the large difference in context length.

From osprey/torch2whirl with the native extension on PYTHONPATH:

```sh
python3 python/tests/mla_native_review.py --artifact-dir /absolute/fresh/mla-native --ir-b2a /build/osprey/targdir/ir_tools/ir_b2a --driver /build/osprey/targdir/torch2whirl/torch2whirl
```

The script also honors OPEN64_DSL_TEST_ARTIFACT_DIR and OPEN64_IR_B2A. A model
file may provide create_model and open64_sample_inputs; the driver delegates
sample resolution to Python instead of requiring redundant CLI descriptors.
Tests require Python to reject a missing provider without writing an image.

The subsequent v0.4/v0.5 sections record executable lowering and host numerical
validation of the generated code. Remaining gates include real GPU execution,
timing, and production driver integration. AOT remains the production baseline.

## Historical v0.1 validation

The focused MLA suite passes all 15 tests with PyTorch 2.4.1+cpu in the existing
Linux Docker image. The dependency-free oracle also passes all four tests with
host CPython 3.12.14. The existing python_torch_test target passes all 120 tests.
The broad python_test target runs 306 tests with 17 skips and one existing
failure, test_v010_authority_digest_matches_repository_bytes. Both that test
and its FHE v0.10 DOCX are byte-identical to HEAD; the stored authority digest
and document bytes already disagree. This fixture does not change that gate.

Retained default-case maximum absolute output errors are 5.52e-9 for L=127 and
1.37e-8 for L=8193. Maximum finite LSE errors are 3.22e-7 and 7.80e-7 respectively.
Both cases replay exactly through FX without input mutation. These are source
correctness observations in this environment, not timing or GPU results.

## Native v0.2 validation

The focused source, view, native boundary and building-block regressions pass
35 tests. The existing PyTorch capture lane passes 127 tests, standalone driver
smoke passes, and the native Python, ResNet, Llama prefill, Llama decode and
multiple-PU reader lanes pass. Both MLA lengths also pass independent API and
standalone-driver image production and ir_b2a reopening.

New native checks reject wrong arity/version, numerical attributes, result
shape, bool/FP32 mismatch, malformed fill bits, invalid axes and incompatible
batch prefixes. Existing versioned schemas remain checked by their old tests.
Both broad Python configurations run 317 tests. Source-only discovery has
21 skips; native-enabled discovery has one skip. Each has only the separately
verified pre-existing FHE authority-digest failure. This milestone does not
repair that unrelated gate.

The native images preserve source semantics structurally. They are not compiled
GPU execution evidence and do not close MTKP transfer or performance gates.

## Reference CUDA v0.3

The [tensor source contract](../../doc/AI-COMPILER-TENSOR-SOURCE-v0.1.md)
adds an explicit read-only `whirl2c -cuda -tensor-source` mode. It consumes the
native `.B`, validates all PUs, and emits compilable serial host/device CUDA
functions for the actual typed graph, including all 46 MLA operations. The
generic reference header is supplied with the source. Seven inputs remain
runtime arguments; output and LSE remain separate mutable result references.
There is no model-specific CUDA template or input payload baked into source.

```sh
whirl2c -cuda -tensor-source -CLIST:dotc_file=mla.reference.cu mla.B
python3 python/tests/mla_cuda_review.py --artifact-dir /absolute/fresh/mla-cuda \
  --native-review /absolute/mla-native --whirl2c /build/ir_tools/whirl2c \
  --ir-b2a /build/ir_tools/ir_b2a \
  --header /src/osprey/be/whirl2c/open64_tensor_reference.cuh \
  --nvcc /cuda/bin/nvcc
```

`--native-review` contains the prior review's short/long directories with
`mla.B`, `inputs.pt`, and `reference.pt`. The harness compiles the actual
emitted source as host C++, executes original inputs, one all-masked head and
all heads masked, and compares output/LSE with the independent expanded FP64
oracle at the established tolerances. It separately compiles both CUDA device
functions for sm_90, preserves unchanged image digests and identical independent
before/after `.T`, and checks no-overwrite and invalid-option rejection.

The reference function is callable CUDA device code, with no kernel launcher
or resource plan. Optimized common-IR lowering, High WHIRL scheduling and
tiling, Mid memory lowering, Low GPU mapping, normal selected-kernel CUDA/PTX,
device execution and timing remain subsequent milestones. AOT remains the
production baseline. Named workstream roles remain TBD, as requested.

The v0.3 focused suite passes all 41 tests. Host execution of emitted CUDA
passes all six context/mask combinations. Maximum absolute output error on
the original short/long cases is 8.36e-9 / 9.84e-9; maximum finite LSE error is
3.78e-7 / 1.18e-6. Both files compile with CUDA 12.8 NVCC for sm_90 with FMA
and FTZ disabled. Independent `.T` and `.B` remain unchanged. Additional
standalone C++ checks pass on macOS and Linux. GPU execution and performance
remain NOT_RUN.

## Executable device WHIRL v0.4

The [native tensor kernel lowering specification](../../doc/AI-COMPILER-TENSOR-KERNEL-LOWERING-v0.1.md)
controls the new PU-local VHO service and focused native driver. It consumes
the existing source `.B` and builds ordinary scalar device WHIRL. For either
context, 46 source operations plus two result copies become 34 executable
kernels and 14 shape views. The source image remains preserved as provenance.

```sh
tensor_kernel_lower prepare mla.B recipe.json --verify 3
tensor_kernel_lower device recipe.json device.before.B device.after.B
tensor_kernel_lower audit device.after.B recipe.json launch.json
whirl2c -cuda -TARG:abi=n64 -O0 \
  -CLIST:dotc_file=kernels.cu -CLIST:doth_file=kernels.h device.after.B
```

O0/O1/O2 retain serial output loops; O3 explicitly maps independent output
elements to 128-thread blocks. Contractions and reductions retain serial
ascending accumulation. The launch manifest is derived from independently
reopened device metadata and checked against the prepared source recipe.
Owning-kernel/argument-ordinal keys avoid local symbol-ID collisions across PUs.

The `mla_kernel_review.py` harness in be/vho/tests retains source and device
images, independent `ir_b2a -st -src` dumps, raw transformation differences,
CUDA source, launchers, cubins, numerical results and diagnostics. Short/long
and O0/O3 pass 12 host numerical cases against the same expanded FP64 oracle,
including all-masked rows and allocation canaries. Scoped compiler/host
sanitizer checks pass. Thirteen native boundary/semantic tests and portable
oracle-checker controls cover malformed/stale handoffs and generic operations.

The generated source compiles to sm_90 CUDA kernels and linked test launchers.
The local CUDA runtime reports no usable device (probe error 35), so actual
GPU execution and timing remain open. `validate_mla_gpu.py` can execute the
retained launchers and portable saved oracles on a GPU host without PyTorch.

At v0.4 this was an AOT executable baseline with 34 launches and explicit
intermediate traffic. The v0.5 milestone below adds producer fusion and split
reductions. Automatic be_driver integration, tiling/staging, production runtime
dispatch and measured performance acceptance remain later milestones. Existing
source and reference CUDA lanes remain available; no new MLIR interfaces or
tensor traits are added.

## Fused and split device WHIRL v0.5

The [v0.2 lowering specification](../../doc/AI-COMPILER-TENSOR-KERNEL-LOWERING-v0.2.md)
controls O3 producer fusion and parallel contiguous reduction chunks. The
source graph, source attributes and original v0.1/v0.4 baseline remain intact.
No frontend changes or new native logical operators are required.

Short L=127 now uses 9 kernels and 3,528 temporary bytes, compared with 34
kernels and 27,542 bytes. Elementwise operations and transpose/reshape indexing
are fused into materialized contractions, reductions, and result stores.
Long L=8193 uses the same producer fusion plus split maximum, denominator sum,
and weighted latent contraction: 12 kernels and 204,240 temporary bytes versus
34 and 1,664,940. W=256 gives 33 chunks with a one-element tail. Partial writes
are independent; ordered merge kernels consume them on the same stream.

```sh
tensor_kernel_lower prepare mla.B baseline.json --verify 3
tensor_kernel_lower optimize baseline.json short.json --fuse
tensor_kernel_lower optimize baseline.json long.json --fuse \
  --split-width 256 --allow-reassociate
```

Each context uses its own shape-specialized baseline recipe. The subsequent
device/audit/whirl2c commands are unchanged. Nonzero split width requires
explicit reassociation permission; this is tolerance-preserving F32 execution,
not a bitwise promise. O0/O1/O2 reject optimization selection and retain serial
semantic lowering. The emitter prints already-selected WHIRL semantics.

Seven scenarios per context cover original inputs, one masked head, all masked,
tail-only valid data, empty first chunk, equal scores, and large finite scores.
The harness checks unmodified emitted arithmetic against the independent FP64
oracle with rtol=2e-5 and atol=2e-6, including output/LSE, allocation canaries,
unchanged inputs, and reversed work-item order for optimized variants. Separate
native reopening checks bodies, prototypes, launch bindings, and split policy.
Scoped sanitizers, generic non-MLA semantic tests, missing-permission rejection,
optimization-level gates, CUDA compilation, and AOT coexistence are required.

The launchers and portable GPU-only checker retain all saved scenarios for a
compatible GPU host. GPU execution and timing are still NOT_RUN locally because
the runtime finds no usable device. Launch/storage reductions are structural
evidence, not measured speedups. This is not a single fused attention kernel;
global score intermediates, contraction/reduction boundaries, and some producer
recomputation remain. AOT is the baseline; runtime threshold tuning, be_driver
integration, warp/SMEM staging, and production acceptance remain open. Named
ownership/transfer roles remain TBD as requested.

## Readable selected-schedule CUDA v0.6

The [selected schedule source specification](../../doc/AI-COMPILER-TENSOR-SCHEDULE-SOURCE-v0.1.md)
adds `tensor_kernel_lower schedule-source selected.json optimized.cu`. The
printer consumes the same VHO-selected plan used for scalar device WHIRL,
after fusion/split selection and before low-level coordinate expansion.

The short file exposes 9 stages; long exposes 12, with explicit partial and
merge calls using width 256 and 33 chunks. Named lazy views show fused
reshape/transpose, masking, arithmetic, and exp/log without allocating those
intermediates. Materialized boundaries and source identities remain visible.
The supporting header executes the selected stages serially for checking.
This view is a compilable host/device review function; production GPU kernels
still come from the actual selected device `.B` through normal whirl2c.

The review harness joins stage IDs against independently audited device-image
metadata, compiles both files with host C++ and NVCC, checks all seven saved
scenarios per context against the FP64 oracle, and requires byte-for-byte
parity with the lowered host outputs. Input/guard preservation, deterministic
printing, no-overwrite behavior, generic non-MLA operations, baseline rejection,
and stale-policy rejection are covered. No source/device IR, schedule policy,
binary layout, normal emitter behavior, or GPU validation status changes.
There is no separate optimized tensor `.B` at this boundary in the current
focused pipeline; the readable source projects the selected detached schedule.
