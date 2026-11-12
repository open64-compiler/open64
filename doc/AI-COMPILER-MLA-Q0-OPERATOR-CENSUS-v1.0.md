# Q0 quantized/paged MLA operator census v1.0

November 9, 2026 | Generated from the certified CPU PyTorch 2.4.1 captures

Ten source cases produce ten FX graphs and ten serialized normalized ATen
programs. Each representation has 795 nodes in aggregate, including boundaries.
No native quantized WHIRL, optimization candidate or GPU execution is produced.
The source is a standalone deterministic MLA re-expression, not an upstream
engine graph. The [native contract](AI-COMPILER-MLA-Q0-NATIVE-CONTRACTS-v1.0.md)
and [serving contract](AI-COMPILER-MLA-Q0-SERVING-CONTRACT-v1.0.md) define its
supported first profiles and deliberate unsupported boundary.

## Case coverage

B=2, H=3, Dn=8, C=16, R=4, Dv=6. Paged cases use P=4,
three logical pages, nine physical pages, a shared immutable first page,
permuted table entries and a partial last page. Q=1 prefill-tail and decode
have identical graphs and inputs with distinct declared intent. All cases
include an all-masked head; multi-query cases also test causal query tails.
A is the number of appended tokens per request in this invocation.

| Case | Q | L | A | FX nodes | ATen nodes |
| --- | ---: | ---: | ---: | ---: | ---: |
| dense_s8_decode | 1 | 17 | 0 | 60 | 60 |
| dense_s8_prefill_tail | 1 | 17 | 0 | 60 | 60 |
| dense_s8_prefill | 3 | 17 | 0 | 60 | 60 |
| dense_u4_decode | 1 | 17 | 0 | 80 | 80 |
| paged_s8_decode | 1 | 11 | 0 | 70 | 70 |
| paged_u4_prefill | 3 | 11 | 0 | 80 | 80 |
| append_s8_decode | 1 | 11 | 1 | 103 | 103 |
| append_s8_prefill | 3 | 11 | 3 | 103 | 103 |
| append_u4_verify | 3 | 11 | 3 | 123 | 123 |
| fake_s8_decode | 1 | 17 | 0 | 56 | 56 |

## Typed FX audit

Every node receives one category; no unknown target is silently omitted.
Counts: common-substrate=618, domain-expression-component=50, region-contract=115, state-effect-functional=12.
The 12 functional state components are four index_copy calls in each of three
append graphs. They are pure fresh tensor results, not in-place engine writes.
The 50 domain-expression components are contractions inside absorbed attention;
they do not imply an opaque MLA operator. No get_attr/constant node occurs in
these captures; scalar constants remain typed graph arguments. Source/context
metadata and bundle semantics are preserved alongside ordinary tensor nodes.

The following counts exclude placeholders/output. Dtype is the result dtype;
operand dtypes/shapes, exact arguments and constants are retained per node in
`fx.census.json`. Existing-native refers to a reusable logical contract, not
whole-graph ingestion support. New integer and pure-functional versions keep
old meanings intact. Composite-pattern rows require complete proven pack/unpack
joins, not independent eager native admission of arbitrary bit operations.

| FX target | Result dtype | Count | Native requirement | Audit status |
| --- | --- | ---: | --- | --- |
| _operator.add | torch.float32 | 24 | common.add.v1 | dtype-dependent |
| _operator.add | torch.int64 | 5 | common.add.v2 | new-version-integer |
| _operator.getitem | torch.uint8 | 8 | common.pack_u4.v1 | composite-pattern |
| _operator.gt | torch.bool | 20 | common.greater.v1 | existing-native |
| _operator.invert | torch.bool | 10 | common.logical_not.v1 | existing-native |
| _operator.mul | torch.float32 | 30 | common.mul.v1 | dtype-dependent |
| _operator.mul | torch.int64 | 5 | common.mul.v2 | new-version-integer |
| _operator.sub | torch.float32 | 16 | common.sub.v1 | existing-native |
| _operator.truediv | torch.float32 | 42 | common.div.v1 | existing-native |
| flatten | torch.uint8 | 6 | common.unpack_u4.v1 | composite-pattern |
| masked_fill | torch.float32 | 10 | common.fill_like.v1+common.select.v1 | existing-native |
| reshape | torch.float32 | 38 | common.reshape.v1 | existing-native-frontend-extension |
| reshape | torch.int64 | 8 | common.reshape.v1 | existing-native-frontend-extension |
| reshape | torch.int8 | 24 | common.reshape.v1 | existing-native-frontend-extension |
| reshape | torch.uint8 | 14 | common.reshape.v1 | existing-native-frontend-extension |
| squeeze | torch.float32 | 10 | common.reshape.v1 | existing-native |
| to | torch.float32 | 18 | common.convert.v1 | new-contract |
| to | torch.int8 | 10 | common.convert.v1 | new-contract |
| to | torch.uint8 | 4 | common.convert.v1 | new-contract |
| torch.abs | torch.float32 | 16 | common.abs.v1 | new-contract |
| torch.amax | torch.float32 | 26 | common.reduce_max.v1 | existing-native |
| torch.bitwise_and | torch.uint8 | 6 | common.unpack_u4.v1 | composite-pattern |
| torch.bitwise_left_shift | torch.uint8 | 4 | common.pack_u4.v1 | composite-pattern |
| torch.bitwise_or | torch.uint8 | 4 | common.pack_u4.v1 | composite-pattern |
| torch.bitwise_right_shift | torch.uint8 | 6 | common.unpack_u4.v1 | composite-pattern |
| torch.clamp | torch.float32 | 32 | common.clamp.v1 | new-contract |
| torch.div | torch.int64 | 5 | common.div.v2 | new-version-integer |
| torch.exp | torch.float32 | 10 | common.exp.v1 | existing-native |
| torch.full_like | torch.float32 | 10 | common.fill_like.v1 | existing-native |
| torch.index_copy | torch.float32 | 6 | common.scatter.v2 | new-version-pure-functional |
| torch.index_copy | torch.int8 | 4 | common.scatter.v2 | new-version-pure-functional |
| torch.index_copy | torch.uint8 | 2 | common.scatter.v2 | new-version-pure-functional |
| torch.index_select | torch.float32 | 10 | common.gather.v1 | marker-needs-native-schema |
| torch.index_select | torch.int64 | 5 | common.gather.v1 | marker-needs-native-schema |
| torch.index_select | torch.int8 | 6 | common.gather.v1 | marker-needs-native-schema |
| torch.index_select | torch.uint8 | 4 | common.gather.v1 | marker-needs-native-schema |
| torch.isfinite | torch.bool | 10 | common.isfinite.v1 | existing-native |
| torch.log | torch.float32 | 10 | common.log.v1 | existing-native |
| torch.matmul | torch.float32 | 50 | common.matmul.v3 | existing-native |
| torch.ones_like | torch.float32 | 10 | common.fill_like.v1 | existing-native |
| torch.remainder | torch.int64 | 5 | common.rem.v2 | new-version-integer |
| torch.round | torch.float32 | 16 | common.round_even.v1 | new-contract |
| torch.stack | torch.uint8 | 6 | common.unpack_u4.v1 | composite-pattern |
| torch.sum | torch.float32 | 10 | common.reduce_sum.v3 | existing-native |
| torch.where | torch.float32 | 30 | common.select.v1 | existing-native |
| torch.zeros_like | torch.float32 | 10 | common.fill_like.v1 | existing-native |
| transpose | torch.float32 | 30 | common.transpose.v1 | existing-native |
| unsqueeze | torch.float32 | 30 | common.reshape.v1 | existing-native |
| unsqueeze | torch.int64 | 5 | common.reshape.v1 | existing-native |

## Normalized ATen audit

The table below is an independent observed target census. ATen decomposition
and source pattern joins must be checked together before Q1 changes admission.
For example, normalized slice/bitwise/stack/view nodes collectively express U4
packing/unpacking; index_copy remains functional. The normalized census labels
operations as source evidence rather than pretending each is already a native
schema. FX and ATen node totals happen to agree; target names need not agree.

| ATen target | Count |
| --- | ---: |
| aten._to_copy.default | 32 |
| aten.abs.default | 16 |
| aten.add.Tensor | 29 |
| aten.amax.default | 26 |
| aten.bitwise_and.Scalar | 6 |
| aten.bitwise_left_shift.Tensor_Scalar | 4 |
| aten.bitwise_not.default | 10 |
| aten.bitwise_or.Tensor | 4 |
| aten.bitwise_right_shift.Tensor_Scalar | 6 |
| aten.clamp.default | 32 |
| aten.div.Tensor | 42 |
| aten.div.Tensor_mode | 5 |
| aten.exp.default | 10 |
| aten.full_like.default | 10 |
| aten.gt.Scalar | 20 |
| aten.index_copy.default | 12 |
| aten.index_select.default | 25 |
| aten.isfinite.default | 10 |
| aten.log.default | 10 |
| aten.masked_fill.Scalar | 10 |
| aten.matmul.default | 50 |
| aten.mul.Tensor | 35 |
| aten.ones_like.default | 10 |
| aten.remainder.Scalar | 5 |
| aten.round.default | 16 |
| aten.slice.Tensor | 8 |
| aten.squeeze.dim | 10 |
| aten.stack.default | 6 |
| aten.sub.Tensor | 16 |
| aten.sum.dim_IntList | 10 |
| aten.transpose.int | 30 |
| aten.unsqueeze.default | 35 |
| aten.view.default | 90 |
| aten.where.self | 30 |
| aten.zeros_like.default | 10 |

## Observed refusals and validation

Production mock/native ingestion rejects all ten cases before any .B
publication. The observed first blockers are abs in construction/append/fake
sources and remainder in read-only paged sources. This first-error diagnostic
is not the exhaustive unsupported-node list: the typed table and native
contract record the remaining requirements. common.gather has a marker only;
common.scatter.v1 modifies state and cannot represent functional index_copy.

All six results per case match the independent scalar expanded-K/V attention
reference (attention/LSE rtol=2e-5, atol=2e-6; encoded storage/scales exact for
these finite fixtures), and eager/FX/serialized ATen replay agrees exactly.
Inputs remain unchanged. Storage tests distinguish int8 bytes, packed uint8
nibbles and float fake quantization. Page padding poisoning, shared-write
refusal, version joins and strict schema refusal are covered.

Fresh FP32 short/long .B files independently reopen with ir_b2a -st -src and
retain attention.latent_kv_cache.v1. Each native producer uses a fresh process.
A diagnostic multi-export run produced a long image whose reader was killed
(return -9); the isolated long producer/reopen passes. The native global-table lifetime interaction is suspected, not independently
root-caused or repaired by Q0; same-process multi-export robustness
remains a follow-up. No quantized .B is claimed or fabricated.

The twelve focused Q0 tests and 139-test torch capture lane pass. The 341-test
full Python lane has 27 skips and the previously present FHE v0.10 authority
digest mismatch (313 pass, one failure). The authoritative FHE file is unchanged
from HEAD. This unrelated failure is retained and qualified, not repaired here.
Native/global isolation validation uses unchanged MR2 frontend/tool binaries;
production Python and C++ implementation inputs remain unchanged.

FP8, dynamic zero-point quantization, odd U4 columns, arbitrary ragged queries,
in-place/concurrent engine publication, native quantized .B, inferred production
phase, TMA and hardware performance remain unsupported/unmeasured. Retained
source graph/oracle execution is outside estimation-only search. Q1 is next.
