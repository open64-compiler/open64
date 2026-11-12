# Q0 quantized MLA native contracts v1.0

November 9, 2026 | Contract definition from real source census; registry implementation deferred to Q1

## Authority and evidence

This defines the native requirements discovered by the
[Q0 source capture](../osprey/torch2whirl/MLA-QUANTIZED-PAGED-INGESTION-PLAN-v1.0.md).
The captured fixtures are standalone absorbed-MLA re-expressions with explicit
quantization/page addressing, not a claim to reproduce an upstream engine's
quantizer or kernel. The existing absorbed equations follow the local reviewed
MLA fixture; [DeepSeek-V3's absorbed branch](https://github.com/deepseek-ai/DeepSeek-V3/blob/main/inference/model.py)
is algorithmic context. Quantization formats below are the actual local source
definitions. No FP8, dynamic zero point, training or TMA source operator occurs
in this census, so none is implicitly admitted.

The ten FX and ten normalized ATen graphs each contain 795 nodes in aggregate.
They include placeholder and output boundaries; equal totals do not imply
node-for-node name identity. Source tensor shapes, exact arguments, constants,
classifications and serialization/replay receipts are retained. The Q0 review
validator/census is outside the production frontend; the production mapper
continues refusing these graphs. Names/versions below are definitions for Q1,
not registered logical IDs, native .B support or executable materializers.
No numeric opcode IDs are allocated in Q0; Q1 appends new IDs and adds version
schemas without changing any existing encoding or schema.

Normative WHIRL was reviewed from the 52-page WHIRL specification, especially
pages 5-9 (levels, strict tree form and result/descriptor completeness).
Q0 does not change a WN layout, kid count, type-kind encoding, ELF section or
managed table format. Q1 must use existing direct-operand DSL node construction,
typed result symbols, managed descriptors and mapped writer/reader/ir_b2a
verification. Multi-result FX tuples become separately typed returned values
and explicit bundle joins; they are not arbitrary Python tuples inside WN.

## 1. Captured source and missing-support audit

| Source primitive | Required native meaning / reuse |
| --- | --- |
| abs, round, clamp, to | New common.abs.v1, common.round_even.v1, common.clamp.v1 and common.convert.v1; finite bounded first profile |
| U4 slices, shift/or, and/right-shift, stack, flatten | Prove complete common.pack_u4.v1 / common.unpack_u4.v1 patterns; unmatched patterns stay unsupported |
| index_select | Publish executable common.gather.v1 schema under the existing marker name; marker presence is not executable typed admission |
| functional index_copy | common.scatter.v2 pure fresh-result profile; common.scatter.v1 has modifying state-edge semantics and is not equivalent |
| nonnegative I64 page division/remainder | common.div.v2 / common.rem.v2 explicit floor profile; preserve F32 and legacy v1 meanings |
| I64 page multiplication/addition and scalar literals | common.mul.v2 / common.add.v2 checked nonnegative index profile, common.fill_like.v2 for typed I64 literals |
| reshape/unsqueeze/squeeze/transpose | Reuse common.reshape.v1 / common.transpose.v1 logical reindex rules; extend frontend datatype/form admission explicitly, not their meanings |
| contractions, softmax arithmetic, masks and LSE | Existing common.matmul.v3, reduce_max.v1, reduce_sum.v3, exp/log/select/isfinite/greater/logical_not/fill_like and F32 add/sub/mul/div contracts |

Existing shape, effect, memory and canonicalization services remain
authoritative. In particular common.gather currently has a generic marker
entry without a released executable DSL_OPERATOR schema; common.scatter.v1
requires a modifying state effect. Q0 does not bypass either boundary with an
opaque cache marker. A v2 pure functional scatter is a deliberate versioned
semantic distinction, not an independent implementation of the v1 modifying rule.

The generic TensorDescriptorIR carrier is reused. Logical cache role is distinct
from signed/unsigned storage dtype, nibble layout, scales, allocation identity
and phase evidence. Canonical type equality does not prove allocation equality,
ready state, cache content, unique ownership or persistent lifetime. Runtime
scale values, page IDs and request labels remain value/invocation data.

## 2. First numerical profiles

All source/compute/accumulation floats are F32. Scales are per physical token
row with one F32 scalar, broadcast over the last logical column. Inputs to the
source encoder are finite. epsilon is exactly 2^-12; source operation order is:

    a = amax(abs(x), last_axis, keepdim=true)
    s = F32(clamp_min(a, epsilon) / qmax)
    rounded = round_even(F32(x / s))

For s8-token-rne127-f32.v1, qmax=127 and:

    encoded = convert_I8_storage(clamp(rounded,-127,+127))
    decoded = F32(convert_F32(encoded) * s)

For u4-offset8-rne7-f32.v1, qmax=7 and:

    code = convert_U8_storage(clamp(F32(rounded+8),0,15))
    packed[j] = code[2*j] | (code[2*j+1] << 4)
    decoded[k] = F32((convert_F32(unpack(packed)[k])-8) * s)

Here I8_storage means signed eight-bit elements (torch.int8), not WHIRL MTYPE_I8,
which means 64 bits. U8_storage means uint8 elements, not MTYPE_U8. Q1 must use
canonical dtype names int8/uint8 and the correct I1/U1 memory descriptor widths.
Do not abbreviate these storage encodings as WHIRL arithmetic result types.

Decoder semantics cover all bits of the incoming storage: signed byte -128
decodes arithmetically even though this encoder clamps to -127; U4 codes 0..15
decode using offset 8. U4 logical C and R must be positive and even in this
first profile; physical last extent is logical_extent/2. Scale mapping uses
logical tokens, never packed-column indices. No endian assumption beyond the
published nibble order changes scalar byte values.

Fake quantization has F32 rounded/clamped storage without integer conversion
or packing. Its role remains analyzable but it receives no byte compression
credit. The independent oracle models F32 quantizer steps and reconstructs
expanded keys/values in FP64 for attention. Attention tolerance remains
rtol=2e-5, atol=2e-6; integer/cache/scale state is checked exactly in the retained
finite cases. Unquantized model quality and untested floating corner cases are
separate from that compiler/source correctness evidence.

## 3. Native schema definitions required by Q1

New names use version 1 unless a version is shown. All attributes below are
exact sets, not optional ignored hints. Static axis values are canonical
nonnegative integers. Shape-compatible tensors have positive bounded static
extents initially. No native implementation is enabled by this publication.

| Logical schema | Arity | Exact attributes | Result, effects and required guards |
| --- | ---: | --- | --- |
| common.abs.v1 | 1 | none | F32 tensor, same shape; pure abs including positive-zero result from negative zero. First encoder profile requires finite input |
| common.round_even.v1 | 1 | attr.rounding=ties_to_even | F32 same-shape tensor; pure integral-valued F32 result using source RNE, not truncation or an ambient rounding mode |
| common.clamp.v1 | 1 | attr.has_min, attr.min_bits, attr.has_max, attr.max_bits | F32 same shape; finite F32 bounds as canonical 8-digit bits; unused bound bits canonical 00000000. At least one bound; min<=max when both present. First profile finite input |
| common.convert.v1 | 1 | attr.src_dtype, attr.dst_dtype, attr.mode | Same shape; permitted pairs F32->int8/uint8 under mode=checked_integral and int8/uint8->F32 under mode=exact. F32 source must be finite, integral and in destination range; no bitcast, wrap or saturation hidden in conversion |
| common.pack_u4.v1 | 1 | attr.axis, attr.nibble_order=even_low, attr.code_bits=4 | uint8 code tensor to uint8 packed tensor; last positive even axis halves; all codes 0..15. Pure fresh logical result, explicit physical packing |
| common.unpack_u4.v1 | 1 | attr.axis, attr.nibble_order=even_low, attr.code_bits=4 | uint8 packed tensor to uint8 codes; last axis doubles with checked arithmetic. Pure fresh logical result; low nibble precedes high nibble |
| common.gather.v1 | 2 | attr.axis, attr.bounds=guarded_nonnegative, attr.alias_rule=fresh_result | data tensor T, rank-1 int64 indices -> data dtype, axis extent replaced by index count. Supports source axes 0 and 1; bounds proof/guard required; pure value semantics, reads data+indices |
| common.scatter.v2 | 3 | attr.axis=0, attr.bounds=guarded_nonnegative, attr.conflicts=unique_indices, attr.alias_rule=fresh_result | base T[N,...], rank-1 int64 indices[K], updates T[K,...] -> new T[N,...]. Preserve untouched rows; inputs immutable, result nonaliasing. Unique valid targets mandatory; no modifying old-state edge |
| common.add.v2 | 2 | attr.broadcast_rule=numpy, attr.integer_domain=nonnegative_i64_checked | int64 tensors to broadcast int64 tensor; guard/prove no overflow. Source page-index profile only |
| common.mul.v2 | 2 | attr.broadcast_rule=numpy, attr.integer_domain=nonnegative_i64_checked | Same typed/overflow rule for multiplication; cannot inherit F32 algebraic permissions |
| common.div.v2 | 2 | attr.broadcast_rule=numpy, attr.integer_domain=nonnegative_i64_checked, attr.rounding=floor | int64 n>=0, d>0, result floor(n/d); no division-by-zero or signed-overflow behavior inferred from v1 |
| common.rem.v2 | 2 | attr.broadcast_rule=numpy, attr.integer_domain=nonnegative_i64_checked, attr.rounding=floor | Matching int64 remainder n-floor(n/d)*d, 0<=r<d; do not use an unrelated signed remainder rule |
| common.fill_like.v2 | 1 | attr.value_i64 | int64 like tensor -> same-shaped int64 constant; canonical signed decimal literal within int64 range. Source P/offset constants use this typed representation |

Type names in this table denote tensor element encodings. The low native rules
must respect WHIRL res/desc restrictions: int8/uint8 memory accesses use I1/U1
descriptors and promoted integer results, with sign/zero extension preserved.
Unpack arithmetic/shift lowering must not pretend byte-width arithmetic is an
allowed WHIRL I1/U1 result operation. Tensor descriptors and scalar machine
carriers are related but distinct.

Reuse existing reshape/transpose element-preserving rules for quantized/scale/
index tensors only after their common verifier and frontend datatype admission
are qualified. They do not imply zero-copy physical layout. Integer source
literals must not be converted through a lossy F32 fill. Existing F32 v1
operators and common.fill_like.v1 remain unchanged. No FP reassociation, lower
precision, Tensor Core selection or phase inference is granted by these schemas.

Scalar primitive folding/decomposition must reuse the traditional Open64
simplifier when equivalent; Q1/Q5 own tensor guards, descriptor construction
and memory effects. Do not introduce a new ad hoc algebra engine.

## 4. Pattern, state and region joins

Pack/unpack contraction is allowed only after proving every source slice,
shift/mask, stack and flatten uses the published axis and byte order, with no
escaping intermediate whose semantics would disappear. Q0 labels these nodes
as composite-pattern requirements; it does not implement that proof.

Each cache bundle consists of four separately typed values: latent bytes,
latent scale, rotary bytes and rotary scale. A table is a fifth incoming value
in the paged profile. Output/LSE and successor bundle tensors remain distinct
results. Constructing or functionally replacing values is not an in-place
publication event. A future selected in-place implementation needs its own
alias/ownership/effect proof, state version and serving publication contract.

New semantic contract records are named transformer.mla_cache_bundle.v1 and
transformer.mla_workload.v1. They are proposed common contract-registry entries,
not new executable opaque MLA operators. Bundle checks join four value/storage
IDs, encoding, logical/physical extents, scale axes, table/layout ABI, incoming/
successor versions, read lifetime and publication/release obligations. Workload
checks join per-request Q, incoming/valid cache lengths, query/token positions,
mask axes, append extent and independently sourced phase evidence.

The existing transformer.kv_cache_state profile and common.scatter.v1 are not
redefined: their rank-4 BHSD/modifying-state semantics do not describe these
head-shared rank-3 pools and pure functional source results. Contradictory roles
or state joins refuse before lowering.

## 5. Phase ordering, fallback and acceptance

Source capture and CPU oracle execution are Q0 development activities, outside
an optimization search. Q1 native admission must preflight the whole graph,
then construct only registered typed nodes and common records in the active PU.
Very High retains these semantics; High lowers them to canonical DO_LOOP and
OPR_ARRAY plus explicit indirect page resolution, conversion and state effects.
Mid owns structured memory/control lowering, Low binds fixed GPU owners/target
operations, PTX/whirl2c emit the sealed plan. TMA remains Q6 and has no opcode
or capability in these source graphs.

Native publication requires constructor/predicate, shape/effect/alias rules,
logical printer, mapped writer/reader, reopened gatekeeper and source-parity
tests. Unsupported nodes/types/attributes must leave no valid partial .B.
Fresh legacy FP32 MLA .B/.T images are retained after quantized refusal to prove
the old bridge remains usable; they are not quantized native images.

All Q0 instances remain unsupported by production ingestion. Source CPU eager,
FX and normalized ATen replay are the available representation-compatible
reference path. Native code must refuse until a matching synchronous/generic
quantized implementation is qualified; raw packed data cannot enter FP32 G1.
No new search candidate, score/model/control schema or materializer is enabled.
Existing two-phase budgets, MR2 eligibility/fallback, COMMENT behavior and
selected-only correctness requirements remain unchanged.

Q0 completion is source census plus contract definition. Native quantized
mapped-image certification belongs to Q1/Q3; synchronous native correctness to
Q5; TMA/launch/synchronization to Q6 and measured GPU acceptance to Q7.
