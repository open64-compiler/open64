# S6-0c Conv Plaintext Asset Decision

Status: ACE-aligned raw F32 feature rows selected for C2. This selects the
plaintext asset representation, not an executable CKKS Conv schedule or a
provider-specific file format inside WHIRL. The four stride-two contexts,
value-specific state transfer, and full `.ckks_ops.B` remain open.

## Source Basis

The pinned ANT-ACE source is `fb76131171b9f82aa6387f84dd73684fba5277e8`.
`Get_im2col_kernel` in `nn-addon/vector/src/vector_utils.cxx` constructs a
matrix with input-channel/kernel feature rows and output-spatial columns.
The outer validity traversal visits output columns before kernel features.
Each cell is a folded F32 weight or positive zero after spatial and stride
masking. `tensor2vector_handler.h` stores the transformed matrix as an array
constant and expands the bias. SIHE inserts an explicit `encode` for the
plain vector. The pinned CKKS C generator writes raw `DE_MSG_F32` slices to
an external data file and emits indexed `Pt_from_msg` calls. ANT `pt_mgr.c`
reads a slice and calls `Encode_float` with the requested scale and level.
The runtime does not regenerate Conv geometry or plaintext masks.

The bounded Open64 O0 rule follows the **pre-blocking column-first matrix**:
for feature row `r` in `[0, C_in * 9)`, output channel `oc`, and output
position `(y,x)`, let `f = (r + oc * 9) mod (C_in * 9)`. Its entry is the
folded `OIHW[oc, f/9, (f%9)/3, f%3]` F32 value when that spatial source
position is valid, and positive F32 zero otherwise. Entries are contiguous
by `oc,y,x`, encoded as little-endian IEEE binary32. Row length is
`C_out * H * W`, not an automatically padded 32,768-slot F8 vector. The
separate encrypted input packing, row-indexed rotations, accumulation,
bias, and scale/level effects must still be proved before C2 expansion.
This is not the ACE fast blocking/cost-model schedule and does not silently
admit it as a MetaKernel optimization.

## Open64 Identity And Publication

One Open64 Conv source definition can be reused by callers with different
folded weights. A row asset is therefore identified by the exact source
Conv/value, owner PU, context PU identity, root/callsite identity, folded
weight value/TCON and digest, feature-row ordinal, shape/layout, and row
digest. A shared WN attribute or a PU-local ST index is not sufficient.
Equal complete bytes may be interned only after exact context/formal
binding; a digest alone is not semantic equality.

S6-0c will publish authenticated raw F32 side-file rows and a bounded index
as checkpoint auxiliary artifacts, with `.ckks_ops.B` last. The mapped IR
must contain an owned, typed rank-1 F32 external tensor value as each
`ckks.encode` direct operand, plus the row's source/context provenance and
checksum. Encoding, rotation, multiply-plaintext, rescale, accumulation,
and bias remain distinct executable CKKS steps with concrete value states.
No row bytes are embedded in `.B`, and no runtime-only geometry recipe may
stand in for `ckks.encode`. The S6-0d ACE adapter may repackage the
provider-independent side file into ACE's `RT_DATA_WRITER` envelope; S6-0c
must not link ACE or claim runtime encoding performance.

The current generic external-tensor materialization transaction requires
the source and replacement to have the same TY. A folded weight is rank-4
F32; its transformed row is rank-1 F32. `DSL_CKKS_EXPANSION_OPERAND` can
refer only to an existing value or a prior CKKS step. Main/common therefore
must review an atomic, owner-safe typed external-constant creation/binding
contract before row values can enter mapped WHIRL. FHE code will not create
WN, ST, TY, TCON, or image rows directly to bridge this gap.

The requested generic transaction takes the active PU, a canonical result
TY, side-file/TCON/range/checksum facts, a source external value, a pure
transformation/provenance identity, a deterministic insertion point, and
the owning context. It preflights the source owner, result shape/dtype/byte
length, side-file bounds and checksum syntax, source-to-result relationship,
name uniqueness, and every requested creation before mutation. It returns
stable value IDs usable as `ckks.encode` operands; it does not rewrite a
caller actual or reinterpret the source weight's TY. Failure leaves the
physical tree, managed images, and side-file publication unchanged. The
main/common review must settle the exact API and mapped provenance carrier;
this document does not allocate an opcode, section, or TY encoding.

## Comparison And Acceptance

The previous 10,386 signed-rotation-group masks and 2.536 GiB F8 budget
remain a **diagnostic alternative**, not the selected asset format. The
grouped clear-slot oracle is useful to cross-check tensor results, but it
does not model ACE's feature-row file layout. The selected F32 row count,
exact byte budget, and digest census must be recomputed from the 17
replay-authenticated stride-one contexts; four stride-two contexts remain
excluded. No source-model artifact or large generated side file belongs in
Git.

Focused certification must compare the ACE-style row bytes against an
independent column-first oracle for nonuniform folded weights, padding,
row permutation, positive-zero fill, and all supported captured shapes.
Reject changed source/payload hashes, duplicate or missing contexts,
invalid feature rows, wrong TY/shape, nonfinite weight, row-range/digest
mismatch, and any failed auxiliary publication without a final or `.tmp`
`.ckks_ops.B`. Benchmark actual ACE encoding separately; a clear-byte
rebuild is not an encode benchmark.

The compact derived-mask proposal is deferred. It would add a new logical
producer or typed association and is not how the pinned ACE path supplies
plain Conv weights. Do not introduce that contract as an implicit fallback.
