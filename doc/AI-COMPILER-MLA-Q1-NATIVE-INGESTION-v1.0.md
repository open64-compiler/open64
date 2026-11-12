# Q1 native quantized-cache ingestion and source inference v1.0

November 9, 2026 | Native source representation and mapped parity; executable quantized lowering remains Q5

## Delivered scope

Q1 implements native ingestion for all ten bounded [Q0 source cases](AI-COMPILER-MLA-Q0-OPERATOR-CENSUS-v1.0.md):
dense signed-byte decode/prefill-tail/prefill, dense packed-U4 decode, paged
signed-byte decode/U4 prefill, functional signed-byte decode/prefill and U4
verification append, and float fake quantization. They emit genuine mapped ELF
WHIRL .B with direct native DSL operands, independently reopened .T traces and
native source records. These are deterministic local tensor programs, not
upstream serving-engine kernels or a deployed allocator adapter.

The source frontend owns recognition/preflight. common/com owns logical
versions, types, shape/effect contracts, construction and mapped verification.
A native C++ source query infers content/decoder facts on copied logical
dataflow before the first PU/type/value product. It performs no native table
mutation, target selection, candidate exploration or scope expansion. Native
construction remains in C++; Python never constructs WN/ST/TY/DST layouts.

Q1 source .B is an inspectable semantic boundary, not a qualified executable
quantized kernel. New source operations have marker-only lowering capability;
the old FP32 preparation route refuses them. Q3 must bind serving/runtime guards
and Q5 must implement a compatible synchronous selected rule. No packed buffer
is routed to FP32 G1. No TMA, JIT, GPU mapping, PTX or performance claim is added.

## Versioned native representation

The [Q0 native definitions](AI-COMPILER-MLA-Q0-NATIVE-CONTRACTS-v1.0.md) now have
implemented native registry/constructor/type/shape/effect/printer/reopen support:

| Logical operation | Version | Logical ID / semantics |
| --- | ---: | --- |
| common.abs | 1 | OPR_DSLABS=49; F32 same shape |
| common.round_even | 1 | OPR_DSLROUNDEVEN=50; explicit ties_to_even |
| common.clamp | 1 | OPR_DSLCLAMP=51; finite binary32 bound bits and exact presence flags |
| common.convert | 1 | OPR_DSLCONVERT=52; F32->int8/uint8 checked_integral, bytes->F32 exact |
| common.pack_u4 | 1 | OPR_DSLPACKU4=53; even logical last axis, even column in low nibble |
| common.unpack_u4 | 1 | OPR_DSLUNPACKU4=54; low nibble followed by high nibble |
| common.gather | 1 | OPR_DSLGATHER=55; indexed source axis 0/1, guarded bounds, fresh result |
| common.scatter | 2 | Existing logical ID21 with a new pure, fresh-result schema; unique targets and guarded bounds; no modifying state edge |
| common.add / mul / div / rem | 2 | Existing IDs2/22/23/24, nonnegative checked int64 index arithmetic; div/rem explicitly floor |
| common.fill_like | 2 | Existing ID48; exact signed int64 text, never converted through F32 |

The exact attributes/arity remain those published in Q0. The native predicate
validates semantic dtype and physical scalar carrier together, exact modes,
canonical bounds/literals/axes, broadcast/shape arithmetic and result extents.
The builder repeats these checks before operator allocation; the mapped
gatekeeper uses the same predicate after reopening. Runtime guard words are
obligations, not evidence that their input values have been proved safe.

Old schemas, numeric IDs and default Python mappings retain their meanings.
Unversioned native Get_Info reports the latest schema; callers constructing an
old form must request its explicit version. The regression fixtures now use an
actually unregistered version for rejection tests, separately verify v1/v2 and
prove checked-index v2 has no inherited fold evaluator. No existing fold rule
is broadened. Physical OPR_DSL encoding, WN kids, ELF/managed-image layout,
type-kind encodings and reader/writer format remain unchanged. New readers
reopen old source images; old readers are expected to refuse unknown Q1 forms.

int8 storage maps to I1 and uint8 to U1. MTYPE_I8/U8 still mean 64-bit carriers.
The tensor descriptor and a future scalar arithmetic lowering are distinct;
Q5 must preserve sign/zero extension and promoted integer arithmetic.

## Source proof boundaries

Frontend admission still reads only source graph, static attributes and input
descriptors. Byte conversion is admitted only after a closed rounded, bounded
integral producer. A general torch.to(int8) can truncate/wrap and is refused;
it is not silently mapped to checked_integral. Integer scalar literals use
fill_like.v2. New byte/int64 reshape forms preserve element counts and dtype.
New reshape/to triggers do not intercept existing call_module/get_attr model
families solely because they contain these methods.

U4 joins require the exact slices/shift/or and and/right-shift/stack/flatten
patterns, the published nibble order, bounded nibble-code production and no
escaping intermediate. Unmatched or leaking bit operations remain unsupported.
No optimization profitability or hardware policy participates in this source
canonicalization. Static frontend/mode/declaration rejection precedes native
PU construction and .B publication; final validation uses existing native
builder verification and mapped-image finalization.

F32 encoder inputs are the bounded finite first profile defined by Q0.
Conversion finiteness/range, nonnegative index/positive divisor/no-overflow,
gather bounds and scatter uniqueness remain explicit semantic preconditions.
Q1 does not implement their production guard materialization or dispatch. A
consumer must not erase these obligations or invoke unqualified native code.
The review interpreter checks them, including page bounds, integer overflow and
duplicate writes; those checks are not device guard or serving integration tests.
Additional mapped cases exercise signed -128 and all sixteen low/high nibble
codes against eager source and the independent expanded reference.

## Native inferred cache facts

The native source query is implemented in the source frontend's
`quantized_cache_inference.h` and exposed through the opaque builder's
infer_quantized_cache_traits query. It consumes topological copied records
with exact fields name/opcode/version/dtype/shape/operands/attrs, never pointers
to WN or source tensors. It recognizes paired score contractions and normalized
weighted-latent accumulation using the same latent operand, then the explicit
dequantization/scale joins for latent and rotary values. Renaming values or
changing the external phase label does not change its reasoning.

It validates singleton scale-column shapes and identical page-gather index
values. It follows reshapes, paired gathers and pure functional byte/scale
updates to identify source storage and successor SSA associations. A successor
byte pool paired with incoming scale state or different update indices refuses.
Missing/opaque decoder joins and mismatched page-scale gathers remain unknown.
Contradictory source declarations cannot override inferred roles. Declarations
on encoded inputs require the native source proof. The mock backend makes no
native inferred-fact claim; it still supports the old explicit FP32 declarations.

Existing single-role TensorTraitSet strings are reused:
attention.latent_kv_cache.v1 and attention.rotary_key_cache.v1. Byte encodings
use the existing descriptor's quantization field with conservative new names:

| Native fact | What is proved | What is not proved |
| --- | --- | --- |
| s8-token-scaled-f32.v1 | Signed-byte storage, exact byte->F32 conversion and per-token F32 scaling | The external encoder used RNE127, scales are calibrated, or the cache persists across calls |
| u4-offset8-f32.v1 | Packed uint8 storage, even-low nibble order, offset8 decode and per-token F32 scaling | The external encoder used RNE7 or any allocator/TMA alignment |
| fake-s8-float32.v1 | Closed source round/clamp/scale recipe with float output | Physical byte compression |

Q0's serving encoding names that include RNE127/RNE7 remain unchanged; those
stronger producer profiles are visible as explicit native source operations
when their producers occur in the graph. Incoming decoder use alone does not
prove them. Decoded F32 receives its content role but no byte quantization
descriptor. Fake quantization remains F32 and records its recipe in metadata,
not a packed-layout field that would change F32 expression compatibility.

Runtime SSA joins are ST compiler metadata cache.inference.role / encoding /
scale / evidence. The evidence version is native-source-qdq-use.v1. Scale value
names do not enter tensor type identity. They are local source associations,
not allocation IDs, epochs, persistent cache versions, DU/dependence proof,
publication events, unique ownership of inputs or MR1 reuse seals. Roles and
encoding alone cannot authorize grouped-head reuse, in-place append or phase
selection. The remaining Q1 High-provider step must establish the applicable native High
access/version/lifetime facts when its admitted loop/memory form exists. This
source boundary does not close that broader Q1 exit; Q3/Q4 must not consume
source metadata as a substitute for the owned High proof.

The query is bounded at 32768 source records and 256 storage-chain steps; a
million candidate-pair checks returns unknown rather than partial evidence.
Malformed/contradictory input refuses. Legacy FP32 graphs without quantization
operations bypass this new query. Any source graph/SSA/type change invalidates
its inferred metadata; rederive it instead of treating it as an optimizer seal.

## Independent correctness and fallback evidence

`tensor_kernel_lower q1-inspect input.B output.native.json` is an independent
standalone driver mode. The driver selects one PU, gatekeeper-verifies its
mapped native tree, copies actual operand/type/attribute records and repeats
native cache inference. It checks stored role/encoding/scale metadata against
those actual records and checks symbol/type table counts remain unchanged.
Its JSON library and frontend source-query helper are standalone-tool inputs;
neither introduces a dependency into be.so.

`mla_q1_native_review.py` creates each case in a fresh process, runs separate
ir_b2a -st -src and q1-inspect processes, then executes the reopened operation
records in a CPU semantic review interpreter. It does not reconstruct the FX
graph or call the source model for that execution. All six native results agree
with eager source at zero rtol/atol; encoded bytes/scales and immutable input
state are checked. The independent scalar quantizer/page traversal/expanded
FP64 attention oracle from Q0 remains the second numerical check (rtol2e-5,
atol2e-6 for attention/LSE, exact retained cache state). This is ingestion
parity, not compiled CPU/CUDA execution or model-quality acceptance.

All ten cases retain genuine .B/.T and native JSON: 731 native operation nodes
and 98 rederived source facts in aggregate. The 14 Q1 native tests include old
FP32 boundary tests; the focused combined lane has 37 tests. The 139-test torch
capture lane passes; the 355-test native Python lane has 347 pass, seven skips
and the unchanged FHE v0.10 authority-digest failure. Native syntax/physical
opcode compatibility and source/class capture drift checks pass.

The compatibility lane reopens fresh FP32 short/long source images, constructs
their old unoptimized High algorithms and reopens those before/after images,
and reads earlier native source images with the new reader. Fresh standalone controlled
search (37 assertions) and movement costing (144 assertions) tests pass. The
rebuilt backend separately processes both legacy High cases at O3 through the
production LNO plugin; these tests do not perform code generation. All ten source forms are refused by the old FP32 preparation path
before a recipe is published. The backend adds no shared-library dependency
or undefined symbol. Search controls, budgets, census, cost model, materializers,
selected-plan transaction, COMMENT workaround and PREOPT behavior are unchanged.

Source capture/oracles and artifact verification remain development activities
outside estimation-only exploration. Q1 adds no optimization candidate or
eligibility. Only a future qualified selected quantized plan may be lowered
and regression-checked. Quantized native correctness in the sense of executable
kernels, engine guards/publication and GPU performance remain Q3/Q5/Q7 gates.
Fresh producer processes remain required for certification; same-process native
multi-export lifetime robustness is not claimed fixed.

Owner, successor, independent reviewer and approving lead remain TBD. The
immutable packet records the producer commit, source/tool hashes, commands,
reopened traces, reference data and known failure qualification. It does not
claim independent transfer acceptance or close any hardware gate.
