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

### Reuse of ordinary WHIRL element semantics

The logical tensor IDs are compatibility wrappers for whole-tensor contracts,
not an independent scalar arithmetic hierarchy. `dsl_scalar.h` is the shared
typed mapping used by the existing integral preparation and target constant
folder and by the checked element-projection adapter in `dsl_simp.cxx`:

| Tensor wrapper | Existing scalar WHIRL meaning |
| --- | --- |
| common.add / sub / mul / div / integer rem | ADD / SUB / MPY / DIV / REM |
| common.greater | GT with I4 result and operand mtype as descriptor |
| common.logical_not | I4 LNOT on canonical 0/1 values |
| common.select | SELECT, with I4 condition and typed value arms |
| common.exp / log | INTRINSIC_OP with the existing F4EXP/F8EXP or F4LOG/F8LOG intrinsic |
| common.isfinite | INTRINSIC_OP with existing F4FINITE/F8FINITE, I4 result |

WHIRL has typed EXP/LOG intrinsics rather than core OPR_EXP/OPR_LOG operators.
Mapping availability does not admit additional tensor dtypes, schemas or
versions. In particular, scalar integer SUB tests do not extend the FP32-only
`common.sub.v1` ingestion contract. Bool tensor storage remains U1; ordinary
GT and LNOT must use supported register predicate opcodes, not invented U1
scalar predicates. SELECT represents already-evaluated operands; CSELECT's
short-circuit evaluation would have a different contract.

The retained wrappers are necessary at this boundary because ordinary scalar
opcodes do not describe trailing-axis broadcasting, tensor descriptors, bool
storage, per-value result symbols or unresolved source numerical permissions.
All operands, including an unchosen SELECT arm, must pass native preflight and
the mapped gatekeeper before lowering. For example, selecting between equal
arms of shape [1,3] with a condition of shape [2,1] still produces [2,3]. A
scalar identity cannot replace that result with an unexpanded arm. Public
logical IDs 40-48, names, schemas and mapped-image contracts remain unchanged.

Reduction, matmul and fill_like remain distinct: they describe an iteration
domain/accumulation contract, contraction, or tensor shape plus exact literal
bits. A scalar MAX, MPY or CONST alone cannot express those contracts. No
parallel DSL subtraction, transcendental or predicate rule engine is added.

The expression-scoped adapter delegates eligible disposable element projections
to `WN_SimplifyExp1/2/3`, hence the existing `wn_simp_code.h` rules. It does not
traverse a PU, index a tensor, lower the source graph or publish tensor results.
The owning caller must preserve all source producer evaluation, broadcast
indexing, descriptor/effect checks and result metadata. The legacy integral
ADD/MUL tensor adapter retains its previous admission; this revision does not
broaden it to unresolved FP source graphs.

The scalar adapter accepts only pure, distinct leaves. Integral DIV/REM remain
outside this new lane (the existing target folder keeps its divide guards).
FP arithmetic and ordered comparison are limited to nonvolatile local/formal
direct loads under IEEE_ACCURATE, ROUNDOFF_NONE, forced IEEE comparisons and
disabled reassociation, aggressive folding and division splitting. These are
unchanged engine probes, not permissions for FP identities. Constants, nested
FP trees and all FP PREG arithmetic/comparison operands are rejected: a backend
PREG home can expose a constant to unconditional zero/NEG rules. The adapter
does not change process controls and honors the master `Enable_WN_Simp` switch.

A literal canonical I4 SELECT condition may forward an already-evaluated pure
FP leaf bit-for-bit. No FP arithmetic, NaN comparison or host conversion is
used on that path. An unknown condition remains explicit. EXP/LOG's
`source_unresolved` contract forbids intrinsic simplifier dispatch even for
constants: the existing target intrinsic folder uses host math, which does
not bind source/device rounding, exceptions or approximation. ISFINITE's
intrinsic mapping likewise does not grant exception-sensitive folding here.
Future executable lowering must review the numerical binding before emitting
these typed intrinsic operations; ingestion still emits the domain-visible
tensor wrappers, never early intrinsic-call placeholders.

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

The scalar reuse fixture separately retains ordinary-WHIRL before/after images:
integer SUB, GT, LNOT and SELECT are simplified by the real engine; strict FP
SUB/GT and signed-zero/nested-arithmetic counterexamples remain unchanged.
Exact signed-zero, infinity and NaN SELECT tests verify selected WN identity;
negative tests cover unresolved intrinsics, backend PREG exclusion, relaxed
controls, malformed projections and master disable. Native tests independently
exercise SUB/GT/three-way SELECT broadcasting and rejected result/dead-arm
shapes. This is infrastructure and ingestion evidence, not compiled MLA or
GPU execution.

Build the standalone fixture with `make -C <build>/osprey/targdir/ir_tools
dsl_scalar_reuse_test ir_b2a`, then run
`OPEN64_BUILD_ROOT=<build>/osprey/targdir OPEN64_DSL_TEST_ARTIFACT_DIR=<host-run>
bash osprey/common/com/tests/dsl_scalar_reuse_test.sh` in the Linux environment.
The script retains a fresh before/after/disabled artifact family, matching
same-stem `-st -src` traces, source, logs and raw diffs. Native broadcasting
tests remain in `test_tensor_expression_native_optional.py`.

The numerical oracle validates eager and captured source execution; it is
separate from structural native IR evidence. For the November 12 operation-
reuse revision, the Open64 WHIRL manual was reviewed through the
[OpenAD mirror](https://www.mcs.anl.gov/OpenAD/open64A.pdf), sections on
operators/types, direct kids, SUB, GT, SELECT and INTRINSIC_OP. Concrete current
opcodes, types, layouts and mapped writer/reader behavior remain authoritative
for physical compatibility. No binary format extension is made.
