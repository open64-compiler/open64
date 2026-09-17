# WHIRL DSL Shape Propagation SP9 Certification

## Status

SP9 operator coverage, compatibility, normal `-O0` backend, and `whirl2c`
certification are complete and passing. The retained ResNet run uses the
rebuilt backend and DSL-aware `whirl2c` across process boundaries.

## Implemented SP9 Coverage

The shared `dsl_shape` service now supports:

- exact and NumPy broadcasting for `common.add.v1`, `common.mul.v1`,
  `common.div.v1`, and `common.rem.v1`;
- static and PU-scoped symbolic `common.matmul.v1/v2` inference;
- exact batch-dimension and contraction-dimension proof for batched matmul;
- transpose-aware result dimensions for `common.matmul.v2`;
- fail-closed rejection of unproved symbolic batches, unproved contractions,
  anonymous required equality, and impossible NumPy broadcasts.

The DSL gatekeeper delegates arithmetic shape legality to this same shared
service. It no longer rejects a reviewed broadcast before the operator shape
contract is evaluated.

No opcode, `TY_KIND`, mapped-image row, ELF section, or binary WHIRL layout was
added or changed by SP9.

## Certification Matrix

| Lane | Result | Evidence |
| --- | --- | --- |
| Common add/mul NumPy broadcast | Pass | `shape_sp9.T` contains `[2,4,3,5]` results |
| Symbolic batched matmul | Pass | `[B,H,M,K] x [B,H,K,N] -> [B,H,M,N]` |
| Matmul/broadcast negatives | Pass | Wrong batch, contraction, and broadcast fail closed |
| Linear/reshape/transpose/flatten | Pass | Existing SP3 rules plus Llama/ResNet artifacts |
| CNN convolution/BN/pooling/residual/logits | Pass | Six-PU ResNet admission artifact |
| Llama prefill | Pass | Static complete compiler-admission snapshot |
| Llama decode | Pass with stated split | Sample artifact is concrete; SP8 separately proves symbolic cache semantics |
| Llama multiple PU | Pass | Class-centric multi-PU artifact and calls reopen |
| FHE shape ownership | Pass | FHE phase contract passes; no FHE-private `DSL_Shape` implementation exists |
| Non-DSL C WHIRL | Pass | Two-PU C artifact reopens with source and is a shape no-op |
| Previous/current reader | Pass | SP1 reader opens SP9; SP9 reader opens SP2 |
| Syntax/operator target matrix | Pass | x86-64, MIPS, MIPS-SL, KEY, Loongson, baseline |
| `be.so`/`lw_inline` dependency boundary | Pass | No `DSL_Builder_*` or `Json::` symbols |
| Normal model `-O0` backend | Pass | ResNet emits post-shape trace, intermediate WHIRL, and x86-64 assembly |
| `whirl2c` model output | Pass | DSL-aware printer emits tensor declarations and logical DSL expressions |

## Descriptor Snapshots

For ResNet, Llama prefill, Llama decode, and Llama multi-PU, the frontend seed
descriptors are already complete. The native admission gate invokes the shared
compiler shape service in check-only mode. Therefore the retained
`.shape-before.t` and `.shape-after.t` files are intentionally identical
descriptor snapshots and each accompanying diagnostics file says
`vho_retyping_required=no`.

SP7 separately certifies actual VHO retyping and invalidation. SP8 separately
certifies `<pending>`, anonymous dynamic, PU-scoped symbols, and the reviewed
Llama decode symbolic relationship. These tests follow compilation scope: the
shape service validates one active PU; the backend driver owns traversal.

## Backend Startup Correction

The failed backend reproduced with no input, before WHIRL reading, alias
analysis, or shape refinement. Valgrind located undefined state in the static
`O64_Driver` construction path. Two pre-existing defects were corrected:

- `_DriverPool` and `_LocalPool` are zeroed before `MEM_POOL_Initialize`, whose
  duplicate-initialization guard reads prior pool state;
- `O64_ComponentDescriptorList::SetComponentRegistered()` now returns the
  assigned `BOOL` instead of reaching the end of a non-void function during
  static component registration.

The remaining driver members are initialized deterministically. A rebuilt
`be` with no input now exits through the expected `No source files specified`
path instead of aborting. The rebuilt driver then processes `resnet.B` at
`-O0`, emits the post-shape trace, retains `resnet.I`, and produces x86-64
assembly.

The installed `whirl2c.so` predates native `OPR_DSL` support and correctly
fails closed on the high-level artifact. Rebuilding the existing DSL-aware
`whirl2c` sources with `SKIP_DEP_BUILD=1` uses `WN2C_dsl()` and produces a
reviewable C diagnostic projection. The output contains `TENSOR` declarations
with complete descriptors and logical operators such as
`OPR_DSLCONV2D.v2`, `OPR_DSLRELU.v2`, and `OPR_DSLLINEAR.v2`.

## Retained Evidence

All generated evidence is local and excluded from Git:

```text
/private/tmp/open64-shape-sp5/artifacts/shape/sp9-certification/common/
/private/tmp/open64-shape-sp5/artifacts/shape/sp9-certification/models/
/private/tmp/open64-shape-sp5/artifacts/shape/sp9-certification/non-dsl/
/private/tmp/open64-shape-sp5/artifacts/shape/sp9-certification/compatibility/
/private/tmp/open64-shape-sp5/artifacts/shape/sp9-certification/fhe/
/private/tmp/open64-shape-sp5/artifacts/shape/sp9-certification/target-matrix/
/private/tmp/open64-shape-sp5/artifacts/shape/sp9-certification/pipeline-resnet/
/private/tmp/open64-shape-sp5/artifacts/shape/sp9-certification/pipeline-resnet-success/
```

Each passing model family retains its binary `.B`, `ir_b2a -st -src` `.T`,
descriptor snapshots, diagnostics, command record, and `SHA256SUMS`. The
backend-blocker family retains the original failed diagnostics for comparison.
The separate success family retains the source, `.B`, side payload, `-O0`
intermediate WHIRL, x86-64 assembly, `ir_b2a -st -src` trace, `whirl2c` C and
header projections, logs, commands, certification record, and checksums.

## Remaining Close Actions

None for SP9. Future interprocedural shape work remains separately scoped to
`-ipa` and is tracked in `IPA-DSL-SHAPE-PROPAGATION-TODO.md`.
