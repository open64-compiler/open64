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

Each operation retains a logical schema/version and direct operand kids;
matching elementwise computations use ordinary live WHIRL operators below.
Results have explicit tensor types and result symbols. Seven MLA inputs remain
runtime formals; output and natural-log LSE are separate declared results and
returned values. Scalar source literals use fill_like with exact binary32
bits, not captured example tensor data. No model-class-name dispatch or opaque
MLA marker substitutes for the captured graph.

Logical IDs 40 through 48 are appended; existing IDs and version schemas are
unchanged. Runtime opcode IDs are session-local. No physical WHIRL opcode encoding,
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

### Direct ordinary tensor elementwise operators

Canonical live tensor expressions use ordinary OPR_ADD, OPR_SUB, OPR_MPY,
OPR_DIV, OPR_REM, OPR_GT, OPR_LNOT and OPR_SELECT. The operator supplies the
element operation; required tensor types and semantic contracts supply the
result domain, operand indexing and numerical permissions. No independent
DSL arithmetic semantics or rule engine is defined.

This is the bounded elementwise.v1 extension to WHIRL's scalar signatures,
legal at the native Very High boundary until the owning lowering runs:

| Operation | Live result/descriptor mtypes | Mandatory result TY and meaning |
| --- | --- | --- |
| ADD, SUB, MPY, DIV, REM | M / V | TensorDescriptorIR for the complete result, including element dtype and broadcast shape |
| GT | M / M | Bool tensor result TY; direct operand TYs carry numeric element types; comparison remains ordered |
| LNOT | M / V | Bool tensor input and result of identical shape |
| SELECT | M / V | Typed value result, bool tensor condition; preserve eager operand evaluation and expansion from all three kids |

M here is exclusively a verified tensor carrier, never permission for arithmetic
on an arbitrary struct. This explicitly extends the manual's prohibition of
scalar aggregate arithmetic without changing its scalar signatures. WN_ty
contains the complete result tensor TY; the existing native semantic record
and managed image rows carry exact version, broadcast rule, operands, source
FP contract and ownership. These fields are semantic, not discardable hints.
The gatekeeper checks expression TY against its defining STID/result ST,
operator against the semantic record, and every existing managed-table join,
attribute, dtype and shape contract. No new source dtypes or versions are
admitted. Bool storage stays U1 inside the tensor descriptor; checked scalar
projections use supported register I4 predicates.

The opcode-specific type/annotation properties make copying and WN_Equiv
retain these fields while preserving every scalar opcode property. Raw scalar
simplifier entry points and tree/rebuild paths leave whole tensors unchanged;
scalar tree comparison conservatively distinguishes distinct tensor nodes.
An unknown or missing annotation cannot fall back to scalar arithmetic.
WOPT routes these WNs through its existing complete semantic CODEREP carrier,
preserving descriptor/attribute identity; operations outside its established
admission are explicitly refused. WOPT emission rebinds matching expressions
to ordinary WN operators. No new PU traversal or optimization pass is added.

For example, SELECT with equal arms [1,3] and condition [2,1] still has result
[2,3]. Neither an equal-arm rule nor a constant condition permits replacing
that tensor with an unexpanded arm or ignoring an incompatible unchosen arm.
All descriptor and FP preparation must precede reuse of a scalar rule.

### Frozen binary compatibility

The live representation and the existing .B wire contract have different
roles. Binary finalization projects only the saved output copy of each direct
tensor expression to the established native carrier and its existing semantic
record. It clears the new-only result TY slot; the defining STID already
persists that TY. Original WNs, operand pointers, map IDs and writer offset-map
keys remain unchanged. There is no new opcode enum/number, type kind, node size,
ELF section, row layout or file revision. Existing DSL IDs/names/schemas remain
compatibility keys for old files, not the live mathematical operator identity.

After mapping and fixing operand pointers, the reader rebinds eligible native
expressions under typed result STIDs to ordinary operators. It changes no
value, shape, FP permission or evaluation. Full gatekeeper verification remains
mandatory before lowering. Nonmatching families/versions stay explicit legacy
forms, and malformed ordinary tensor contracts are rejected by semantic gates.
This representation restoration is independent of optimization level; O0 does
not acquire an algebraic optimization.

Previous readers continue to see the original native representation in new
.B files; new readers accept retained old files and expose the direct typed
form. This deliberate wire projection avoids introducing incompatible tensor
opcode signatures into persisted trees. It must not be removed without a
separately versioned compatibility design. ASCII inspection prints real live
operators and their mandatory tensor TY/contract. The existing ir_a2b
new-symbol-table limitation remains explicit; this change does not claim
lossless ASCII assembly of managed tensor images.

### Shared scalar rules and remaining distinct contracts

dsl_scalar.h maps these families to existing scalar WHIRL operations for
checked element projections and the existing target constant folder. Eligible
projections call WN_SimplifyExp1/2/3 and therefore wn_simp_code.h; no algebraic
rules are duplicated. The legacy integral ADD/MUL adapter retains its existing
admission. Callers still own tensor indexing, source evaluation, metadata and
publication of a correctly typed tensor replacement.

EXP/LOG describe established F4EXP/F8EXP and F4LOG/F8LOG intrinsics, but their
source_unresolved contract does not bind those scalar implementations.
ISFINITE likewise lacks a reviewed exception-sensitive intrinsic binding.
These wrappers remain explicit rather than introducing premature intrinsic
placeholders. Reductions, matmul and fill_like retain their iteration-domain,
accumulation, contraction or shape/exact-literal contracts; a scalar MAX, MPY
or CONST alone does not express them.

The scalar adapter accepts pure distinct leaves. Integral DIV/REM remain
outside its new lane; the target folder keeps its existing divide guards.
FP arithmetic/comparison probes require nonvolatile local/formal loads and
IEEE_ACCURATE, ROUNDOFF_NONE, forced IEEE comparisons, disabled aggressive
folding, reassociation and division splitting. Constants, nested FP trees and
PREG arithmetic/comparison operands are rejected before engine dispatch.
Literal-condition SELECT may forward a pure FP leaf bit-for-bit, preserving
signed zero and NaN payloads. No production path changes compiler controls;
the master Enable_WN_Simp switch remains authoritative.

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

The direct tensor fixture checks all eight ordinary elementwise operators,
mandatory result TY/record rejection, clone/equality behavior and scalar-engine
exclusion, including CVTL and unsupported commutation. Generic writer negatives
reject contradictory operators/TYs before wire projection. Record decoding
checks table bounds and termination while preserving the historical unsigned
32-bit carrier bits. It verifies original WN bytes and SELECT's third kid inside the
writer process, not merely across fork isolation. Whole-tree simplification
retains an empty before/after mapped trace diff. The optional previous reader
reopens the same files as legacy native nodes, independently proving that
direct live representation has not changed the .B wire contract.

Build the standalone fixture with `make -C <build>/osprey/targdir/ir_tools
dsl_scalar_reuse_test ir_b2a`, then run
`OPEN64_BUILD_ROOT=<build>/osprey/targdir OPEN64_DSL_TEST_ARTIFACT_DIR=<host-run>
bash osprey/common/com/tests/dsl_scalar_reuse_test.sh` in the Linux environment.
The script retains a fresh before/after/disabled artifact family, matching
same-stem `-st -src` traces, source, logs and raw diffs. Native broadcasting
tests remain in `test_tensor_expression_native_optional.py`.

Run the direct fixture with the same build/artifact variables and
`OPEN64_PREVIOUS_IR_B2A=<retained-reader>/ir_b2a bash
osprey/common/com/tests/dsl_tensor_elementwise_test.sh`. The retained reader's
basename must remain ir_b2a because that tool selects its operation by argv[0].

The numerical oracle validates eager and captured source execution; it is
separate from structural native IR evidence. For the November 12 operation-
reuse revision, the Open64 WHIRL manual was reviewed through the
[OpenAD mirror](https://www.mcs.anl.gov/OpenAD/open64A.pdf), sections on
operators/types, direct kids, SUB, GT, SELECT and INTRINSIC_OP. Concrete current
opcodes, types, layouts and mapped writer/reader behavior remain authoritative
for physical compatibility. No binary format extension is made.
