# AI Building Block Ingestion Plan

Version: v0.1, 2026-10-27. Status: deterministic source fixtures, CPU reference
validation and real FX census implemented; general native WHIRL ingestion open.
Exact static FP32 row-sum native ingestion is now implemented and inspected
through the public API and standalone driver; executable reduction lowering
and native normalization ingestion remain open.
The [native stable-Softmax contract](../../doc/AI-COMPILER-NATIVE-SOFTMAX-CONTRACT-v0.1.md)
and its canonical-WHIRL characterization are implemented. The exact decomposed
Python stable-Softmax matcher now consumes it; opaque framework Softmax is not
silently treated as equivalent. Normalization's exact source connection remains open.
The [native centered population normalization contract](../../doc/AI-COMPILER-NATIVE-NORMALIZATION-CONTRACT-v0.1.md)
and canonical-WHIRL characterization are now implemented. Connecting the exact
centered affine Python matcher is the next ingestion gate; native representation
does not imply an opaque `layer_norm` or arbitrary normalization matcher.
Controlling architecture: [common execution planning](../../doc/AI-COMPILER-COMMON-EXECUTION-PLANNING.md),
[frontend workflow](../../doc/WHIRL-DSL-INFRASTRUCTURE.md) and
[building-block research queue](../../doc/AI-COMPILER-OPTIMIZATION-IMPLEMENTATION-PLAN.md#building-block-research-queue).

## Fixture Contracts

[Source fixtures](python/tests/models/building_blocks.py) use CPU FP32 tensors,
no random state, downloads, CUDA, training, dropout or mutation. Each factory
returns fresh inputs. [Independent scalar references](python/tests/models/building_block_reference.py)
use Python FP64 and `math.fsum`, not torch or numpy. Agreement at `rtol=2e-5`,
`atol=2e-6` validates these inputs only; it does not authorize floating-point
reassociation, approximate transcendental operations or reduced precision.

| Boundary | Inputs and output | Semantic conditions and focused checks |
| --- | --- | --- |
| GEMM | A `[5,7]`, B `[7,3]` -> C `[5,3]` | Rectangular rank-2 contraction, deliberately non-tile-divisible; no bias or fusion. |
| Ordinary reduction | X `[3,17]` -> Y `[3]` | Sum on last axis, `keepdim=False`; mixed signs and non-power-of-two extent. |
| Stable Softmax | X `[3,17]` -> Y `[3,17]` | Explicit row maximum, shifted exponential, sum and division. Large finite logits, constant rows and shift invariance. Empty rows, NaNs and all-negative-infinity rows are outside this fixture's contract. |
| Affine LayerNorm | X `[3,17]`, gamma/beta `[17]` -> Y `[3,17]` | Last-axis population variance; epsilon `1e-5` inside square root; nontrivial affine terms. Constant row must return beta. Not RMSNorm or unbiased sample variance. |
| Causal Attention | Q/K `[1,2,4,3]`, V `[1,2,4,2]`, allowed `[4,4]` bool -> Y `[1,2,4,2]` | Head dimension 3, scale `1/sqrt(3)`, True means permitted; each row has a valid key. Masked Softmax then value contraction, no KV-cache effect. Future-value perturbation cannot affect earlier outputs. |
| Routed MoE | tokens `[5,3]`, IDs/gates `[5,2]`, up `[3,3,4]`, down `[3,4,3]` -> Y `[5,3]` | Three experts, top-2 assignments supplied at runtime, two linear layers with ReLU. IDs must satisfy `0 <= id < 3`; nonnegative gates supplied externally. No gate computation, renormalization, capacity limit or token dropping. Duplicate IDs accumulate contributions. Base input leaves expert 2 empty; test routes all assignments to it and tests zero gates. |

All six check deterministic repeatability, no input mutation, finite output,
and eager/captured execution agreement. MoE's routing **values** change without
recapture; shapes remain fixed. This is not yet a ragged-domain or dynamic-shape
compiler implementation. Attention's scale and masks are fixture-specific,
not a general-purpose attention API.

## Real Capture And Census

[Capture utility](python/tests/building_block_capture.py) runs
`torch.fx.symbolic_trace` and `ShapeProp` on real modules. The checked-in
[golden census](python/tests/golden/building_blocks_fx.json) records every node,
target, arguments, axes/keywords, shape and dtype. Baseline: PyTorch 2.4.1+cpu.
Review graph/API drift before explicitly regenerating; tests never rewrite it.
Class names and per-case files preserve semantic boundaries despite decomposition.

| Case | Call census (excluding inputs/output) | First current generic FX ingestion blocker |
| --- | --- | --- |
| GEMM | matmul x1 | None in mock mapping (`common.matmul`); no native certification in this lane. |
| Reduction | sum x1 | None for exact bounded row sum; general sum forms remain unsupported. |
| Softmax | amax, sub, exp, sum, truediv x1 each | None for the exact whole bounded stable recipe; individual generic amax/exp graphs remain unsupported. |
| LayerNorm | mean x2, sub x1, mul x3, add x2, rsqrt x1 | mean |
| Attention | transpose, truediv, invert, masked_fill, softmax x1 each; matmul x2 | transpose |
| MoE | getitem x4, matmul x2, relu, squeeze, mul, sum x1 each | getitem |

The [boundary regression](python/tests/test_building_blocks_capture_optional.py)
actually calls `export_to_whirl(..., backend="mock")`: it checks GEMM's
`common.matmul`, exact row sum and exact stable Softmax, plus explicit
unsupported-node diagnostics for the other three.
This is an ingestion drift detector, not `.B` certification. Existing specialized
Llama2 semantic ingestion must not be confused with support for these generic
isolated graph shapes. Additional blockers may appear after the first one is fixed.

### Requirement Classification

The following classifies semantic requirements, **not allocated native opcodes**.
No new native contracts or production mapping are added by this milestone.

| Captured object | Classification | Ownership and missing contract |
| --- | --- | --- |
| Placeholders and output, all six cases | Region contract | Tensor operand/result shapes, dtype, rank and alias/effect boundary; use existing opaque builder contracts when ready. |
| matmul, relu, add, sub, mul, truediv | Common substrate expressions | Reuse published contracts where semantically applicable; define broadcast and strict FP legality before extending coverage. A mapped name alone does not prove every rank/broadcast pattern is supported. |
| sum, amax, mean | Common substrate requirements; currently unsupported in these generic captures | Axis, keepdim, identity, reduction state, dtype, empty-domain policy and reassociation permission. |
| exp, rsqrt, softmax | Common numerical expressions/region requirements; currently unsupported here | Stable Softmax boundary, accuracy, overflow/nonfinite behavior, normalization axes and epsilon policy. AI owns workload policy, not generic arithmetic opcodes. |
| transpose, squeeze, static getitem slices/axis insertion | Common view/access requirements; currently unsupported here | Stride/alias/view descriptor preservation; no implied physical re-layout or copying. |
| Tensor-index getitem of expert weights | Common gather requirement plus AI-domain routing semantics; currently unsupported | Indexed reads and range legality are common; expert ownership, assignment multiplicity and combine policy are AI-specific. |
| invert, masked_fill | Common predicate/select requirements; currently unsupported here | Broadcast and mask polarity, negative-infinity constant, valid reduction domain. Causal policy belongs to attention semantics. |
| MoE IDs and gates; attention mask | Read-only runtime state/effect inputs | AI-domain ownership/validity contract. No mutable cache, state write or inter-request lifetime is represented. |
| ShapeProp records, case/module identity, graph names | Compiler metadata | Capture provenance and semantic boundary evidence, not executable operators. |
| Axes, epsilon, scale, mask fill, slicing literals | Constants/attributes | Preserve exact meaning through native contracts; do not specialize away runtime IDs/gates. |

## Execution And Artifacts

With CPU torch installed, from `osprey/torch2whirl`:

```sh
PYTHONDONTWRITEBYTECODE=1 PYTHONPATH=python:python/tests python3 -m unittest -v test_building_block_reference test_building_blocks_capture_optional
PYTHONDONTWRITEBYTECODE=1 PYTHONPATH=python:python/tests python3 python/tests/building_block_capture.py --artifact-dir /absolute/fresh/run
```

Use the existing `open64:torch2whirl-torch-test` Docker image on hosts without
torch. The optional suite is discovered by `make python_torch_test`; the scalar
oracle tests also run without torch. Set `OPEN64_DSL_TEST_ARTIFACT_DIR` to retain
`building-blocks/` when no explicit `--artifact-dir` is given. Reusing a run
directory fails instead of deleting review evidence.

Each case retains typed input values, FP64 reference, FP32 result, graph text,
generated FX forward source and census. The root retains version/status/error
report, compact `capture-summary.log` with its required first-line description,
exact fixture/test/capture source, golden census and SHA256 checksums. No `.B`,
`.T`, `.t` or `.cu` is fabricated. This log is a source-validation report, not
the compiler's `optimization-summary.log`. Accepted evidence is promoted under
the primary checkout's `artifacts/review/ai-optimization/research/building-block-capture/`.

## Validation Snapshot

CPU torch 2.4.1: all 16 focused tests and the 65-test FX capture lane pass;
the frontend driver smoke also passes. Native image/IR-tool lanes were not
rerun because no native builder, mapping or binary format changed.
Full Docker `python_test` runs 244 tests with 14 skips and one existing FHE
authority-digest failure (`test_v010_authority_digest_matches_repository_bytes`).
The referenced DOCX and digest test are unchanged from the base commit; their
hash mismatch is not repaired in this frontend-fixture change. Host-wide tests
also lack torch/numpy. These broad-suite limitations remain visible, not waived
as a clean full-suite result.

Accepted source/capture receipt (primary artifact `report.json`):

```text
/Users/shinmingliu/open64/artifacts/review/ai-optimization/research/building-block-capture/sha256-9a1a7dfdf5c87d999ed42358ea9b60276abb8190187bbfc027a2d11af7624023/
```

## Next Gates

R1 frontend analysis now also implements
[source-derived row characterization and numerical acceptance](../../doc/AI-COMPILER-ROW-SEMANTIC-CHARACTERIZATION-v0.1.md).
`open64_dsc/row_characterization.py` recognizes exact rank-2 FP32 reduction,
stable Softmax and affine LayerNorm dataflows without changing export APIs or
native mappings. It reports domains, accesses, states/merge/finalize,
permissions, graph/descriptor invalidation fingerprint and native requirements.
`python/tests/row_semantic_report.py` retains strict/permitted CPU witnesses,
including rejected samples; the optional row suite runs in the torch Docker
lane. No common/com, backend, MMA1 profile or mapped-image contract changes.
The original six-case validation receipt above remains immutable historical
evidence; new row-analysis evidence is a distinct milestone.
The row milestone passes 21 new tests, 37 focused regressions, the 86-test
capture lane and driver smoke; its detail design links numerical outcomes,
retained source replay and the known full-suite baseline failure.

1. Completed for exact row sum: connect the bounded native contract
   ([native detail](../../doc/AI-COMPILER-NATIVE-REDUCTION-CONTRACT-v0.1.md))
   to exact source ingestion. This contract has real `.B/.T` and PREOPT
   verification evidence plus Python/runtime-formal ingestion and native
   standalone-driver inspection. Executable lowering is still unsupported.
   Agree the remaining native expression/region contracts from this census;
   reuse existing common services, preserve semantic boundaries and define
   PREOPT canonical forms before optimizer matching. Certify mapped `.B`/`.T`
   and derive characterization from canonical WHIRL rather than input ledgers.
2. R2-R4: verify per-case numerical alternatives; generate storage/events and
   full costs; materialize only the selected plan in WHIRL. The existing R0
   research comparison remains separate and synthetic.
3. R6/R7: extend attention to compositional online states and MoE to real
   dynamic/ragged work domains, capacity/combine policies and ownership.
4. R5: native numerical and performance certification after GPU access. Do
   not infer GPU correctness or speedup from these CPU reference tests.
