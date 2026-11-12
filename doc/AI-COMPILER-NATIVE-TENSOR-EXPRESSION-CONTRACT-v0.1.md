# Native Tensor Expression Contract

Version v0.1 dated November 5 2026. This additive contract admits the static
pure tensor graph used by absorbed MLA into native Very High WHIRL. It also
admits standalone unsqueeze, squeeze and transpose graphs. It does not implement
executable lowering, GPU mapping, reassociation, target selection or JIT.

## Representation and ownership

Python reads FX structure, static scalar attributes and input descriptors.
It preflights the entire pure graph before creating a PU and never executes
sample tensor arithmetic for inference. common/com creates WN, ST, TY and
managed image records, checks native types before allocation, and verifies
them again after mapped-image reopening. Scope is one PU; no pass traverses
or mutates another procedure. Existing bridge APIs and public Python APIs
remain unchanged.

Each operation has its own logical operator/version and direct operand kids.
Results have explicit tensor types and result symbols. Seven MLA inputs remain
runtime formals; output and natural-log LSE are separate declared results and
returned values. Scalar source literals use fill_like with exact binary32
bits, not captured example tensor data. No model-class-name dispatch or opaque
MLA marker substitutes for the captured graph.

Logical IDs 40 through 48 are appended; existing IDs and version schemas are
unchanged. Runtime opcode IDs are session-local. No physical WHIRL opcode,
WN layout, type-kind encoding, ELF section or mapped-image row layout changes.
The existing mapped writer, reader and logical printer carry the new records.

## Bounded source semantics

All tensor dimensions are positive static extents, ranks 1 through 16. Inputs
in this frontend lane are contiguous CPU FP32 or bool. FP32 uses F4 and bool
uses the existing U1 scalar carrier plus semantic bool dtype. Zero-dimensional
results, dynamic axes, dynamic shapes, tensor subclasses, get_attr weights,
mutation, unsupported kwargs and out parameters are rejected.

Unsqueeze normalizes a static insertion axis and uses common.reshape.v1.
Squeeze removes only selected singleton axes; a non-singleton selected axis
preserves shape, matching PyTorch. Transpose uses common.transpose.v1 with a
complete permutation. Result dimensions and element counts are verified.
These are logical tensor reindex operations. A logical layout does not claim
physical contiguity, a copy, zero motion or selected device strides; subsequent
memory lowering must derive those from the view operands and permutations.
Aliasing mutations are outside this pure lane.

## Native operation schemas

All operations are pure. Unless stated otherwise, the new operations use
version 1. Numeric input and result dtype is FP32. Broadcasting is trailing-axis
NumPy broadcasting, including rank padding with singleton axes.

| Logical operator | Arity | Exact attributes | Source meaning |
| --- | --- | --- | --- |
| common.sub | 2 | attr.broadcast_rule=numpy | Elementwise subtraction with source FP32 semantics |
| common.exp | 1 | attr.fp_semantics=source_unresolved | Elementwise source exponential; no approximate implementation selected |
| common.log | 1 | attr.fp_semantics=source_unresolved | Elementwise natural logarithm; no approximate implementation selected |
| common.reduce_max | 1 | attr.axis, attr.keepdim, attr.accum_dtype=float32, attr.reassociate=false | Last-axis maximum; NaNs propagate as torch.amax; positive domain |
| common.isfinite | 1 | none | FP32 input to bool of identical shape; false for infinities and NaNs |
| common.greater | 2 | attr.broadcast_rule=numpy | Ordered greater-than, bool result; false on unordered comparisons |
| common.select | 3 | attr.broadcast_rule=numpy | Bool condition selects between two FP32 tensors; operands may already be evaluated |
| common.logical_not | 1 | none | Bool input and bool result of identical shape |
| common.fill_like | 1 | attr.value_bits | FP32 tensor of operand shape filled with canonical lowercase 8-digit IEEE binary32 bits; finite and infinity literals only |
| common.matmul.v3 | 2 | attr.transpose_kid0=false, attr.transpose_kid1=false, attr.batch_rule=numpy, attr.accum_dtype=float32 | Rank at least 2, exact contracting dimensions and broadcast batch prefixes; vector promotion is outside scope |
| common.reduce_sum.v3 | 1 | attr.axis, attr.keepdim, attr.accum_dtype=float32, attr.reassociate=false | Last-axis FP32 sum over a positive domain; preserves source reduction meaning without granting a new order or reassociation |

Reduction axes are canonical nonnegative last axes. keepdim is exactly true
or false; resulting rank must remain positive. Versions 1 and 2 of row sum
and matmul retain their existing contracts. V3 is requested explicitly by the
new source lane; it is not a silent reinterpretation of an old image.

Existing common.add, common.mul and common.div v1 use their published numpy
broadcast contracts. masked_fill becomes an explicit fill_like and select
with the original True-means-replace polarity. zeros_like and ones_like become
fill_like; scalar binary operands become fill_like of the other operand shape.
There is no mathematical fusion or implicit numerical permission in these
source translations.

Native preflight checks exact attribute keys, version, arity, same-PU operand
ownership, dtype, physical carrier, static shapes, broadcast compatibility,
contraction dimensions and result descriptors. The mapped gatekeeper repeats
the shared shape predicate. Shape inference registers the same rules. New
operations have no algebraic or fusion permissions by default. All numerical
implementation and device costs remain unresolved until their owning phases.

## Numerical and phase boundaries

The composed MLA source retains its stable maximum subtraction, all-masked
neutral maximum and denominator, zero output, negative-infinity LSE and original
model scale. exp and log remain explicit nodes; this contract does not replace
them with a mathematically equivalent Softmax or invent an LSE convention.

The operations are retained as typed Very High WHIRL. Native image production
and ir_b2a reopening do not imply that VHO, PREOPT, WOPT, LNO or PTX can execute
the new graph. An unsupported consumer must diagnose missing support. Future
High lowering must preserve views, masks, independent values and numerical
permissions before scheduling and memory lowering.

## Validation and evidence

The absorbed source fixture and independent FP64 reference remain unchanged.
Focused tests cover positive/negative axes, non-singleton squeeze, transpose,
bad axes/kwargs before ShapeProp, native dtype/shape/schema rejection, clean
abort/retry, broadcast rank differences and two output formals. Native review
produces both context images via public APIs, independently reopens them with
ir_b2a -st -src, and also exercises the standalone driver.

The numerical oracle validates eager and captured source execution; it is
separate from structural native IR evidence. The repository's WHIRL.pdf was
not accessible in this checkout or primary checkout during this revision;
existing common/com view, direct-kid, mapped-image and verifier implementations
provide the concrete compatibility baseline. No binary format extension is made.
