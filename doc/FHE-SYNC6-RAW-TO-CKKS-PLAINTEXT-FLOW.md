# FHE SYNC-6 Raw Tensor to CKKS Plaintext Flow

Status: normative onboarding description for the correctness-first `-O0`
Conv path implemented during S6-0c. The complete S6-0c circuit remains in
progress; this document describes the certified Conv parameter flow and the
rules that later operators must preserve.

## Purpose

This document explains how Open64 transforms immutable, raw model parameters
into derived tensors that can be encoded as CKKS plaintexts without overwriting
the source model or embedding large tensor payloads in binary WHIRL. It is the
entry point for an engineer modifying parameter preparation, typed external
tensor values, Conv CKKS expansion, or checkpoint publication.

Read it with:

- [S6-0c detailed execution plan](FHE-SYNC6-S6-0C-DETAILED-EXECUTION-PLAN.md);
- [typed row value handoff](FHE-SYNC6-TYPED-ROW-VALUE-HANDOFF.md);
- [typed external tensor API contract](DSL-TYPED-EXTERNAL-TENSOR-VALUE-API-CONTRACT.md);
- [generated external tensor API contract](DSL-GENERATED-EXTERNAL-TENSOR-VALUE-CONTRACT.md);
- [checkpoint artifact contract](FHE-SYNC3-CHECKPOINT-ARTIFACT-CONTRACT.md); and
- [runtime C ABI contract](FHE-RUNTIME-C-ABI-V1-CONTRACT.md).

## The Three Different Meanings of Plaintext

Keep these objects distinct in code and design reviews:

| Object | Meaning | Current representation |
| --- | --- | --- |
| Source raw tensor | Trained model weight or folded bias captured by the frontend | A range in the immutable SafeTensors input, referenced by an existing external `common.tensor_const.v1` value |
| Derived plaintext coefficient tensor | Compiler-generated F32 row, expanded bias, or stride mask arranged for a CKKS operation | A range in the new `*.conv-plaintexts.f32` side payload plus a side-file Tensor TCON and a new typed or generated external tensor value |
| CKKS plaintext value | Scheme-level plaintext produced from a derived coefficient tensor at a specified level and scale | An explicit `ckks.encode` semantic node and CKKS value-state records |

The compiler performs the first-to-second transformation. It does not encrypt
or pre-encode a runtime CKKS plaintext object into the side file. The explicit
`ckks.encode` operation preserves the semantic boundary between raw F32
coefficient bytes and a CKKS plaintext.

## Serialized Formats and Terminology

The word *plaintext* is overloaded in FHE implementations. In this document,
an **Open64 plaintext coefficient asset** means the numeric input to an
explicit CKKS encode operation. An **ACE encoded plaintext** means the
provider-specific RNS polynomial object produced by that encode operation.
They are not the same serialized object.

### Current Open64 coefficient asset

The current S6-0c Conv side payload has this physical contract:

```text
file := range[0] || range[1] || ... || range[n-1]
range := element[0] || element[1] || ... || element[m-1]
element := IEEE-754 binary32 encoded in exactly four little-endian bytes
```

The file has no embedded header, magic, version, entry count, tensor name,
shape, offset table, or CKKS level/scale. Ranges are appended without a
container-level separator. Byte-identical semantic tensors may reuse one
earlier canonical range, so the file is a store of unique physical ranges,
not necessarily one physical copy per logical tensor value.

The missing structure is intentional because binary WHIRL is the index. For
each range, the Tensor TCON and external `common.tensor_const.v1` value retain:

- descriptor TY, dtype, shape, and layout;
- final relative file path and logical tensor key;
- canonical byte offset and byte length;
- range SHA-256;
- source lineage or source-free generation provenance; and
- the `raw_f32_le` storage-format tag.

Consequently, the current file is provider-independent encode input. It is not
an ACE runtime data file and is not directly consumable by ACE `Pt_mgr`.

### Original Open64 model data

The current source-model file is SafeTensors, not the same file format as the
derived coefficient asset. Its layout begins with an eight-byte little-endian
JSON-header length, followed by the JSON tensor directory and then tensor data
ranges. Its external WHIRL values use `storage_format=safetensors`.

Both files ultimately contain IEEE binary32 values for the tensors discussed
here, but sharing an element encoding does not make their container formats or
semantic layouts equal. SafeTensors stores source tensors in model layout; the
derived asset is headerless and stores CKKS-oriented feature rows, expanded
bias vectors, and generated masks.

### ANT ACE compiler/runtime data file

ANT ACE defines a self-identifying runtime data container in
`fhe-cmplr/include/fhe/core/rt_data_def.h`. Its high-level format is:

```text
offset 0:
    DATA_FILE_HDR
    zero/padding space through offset 4096
offset 4096:
    aligned data entry 0
    aligned data entry 1
    ...
header.lut_offset:
    DATA_LUT_ENTRY[header.entry_count]
```

`DATA_FILE_HDR` contains:

- eight-byte magic `!ANTFHE\0`;
- runtime version, flags, and entry type;
- log2 entry alignment, entry count, and LUT offset;
- creation time; and
- model and file UUID strings.

Each `DATA_LUT_ENTRY` contains a 16-byte entry name, index, byte size, scale,
level, and 48-bit file offset. ACE currently supports three file-wide entry
types:

| ACE type | Payload | Entry alignment | Runtime behavior |
| --- | --- | --- | --- |
| `DE_MSG_F32` | Native/compiler-host float32 message values | 32 bytes | `Pt_from_msg()` calls `Encode_float()` at runtime |
| `DE_MSG_F64` | Native/compiler-host float64 message values | 32 bytes | Message is encoded at runtime by the applicable provider path |
| `DE_PLAINTEXT` | Provider-specific encoded `PLAINTEXT_BUFFER` | 4096 bytes | `Pt_get()` reads and maps the already encoded plaintext |

The present ACE CKKS-to-C and POLY-to-C data managers default to
`DE_MSG_F32`. Thus the normal ACE compiler-generated weight file is not an
offline encoded CKKS plaintext file. It is a structured message container
whose F32 entries are encoded when generated code calls `Pt_from_msg()` or
`Pt_from_msg_ofst()`.

ACE also supplies an offline encoding tool. It reads a message container,
calls `Encode_plain_buffer()` for every LUT entry using that entry's scale and
level, and emits a new container with type `DE_PLAINTEXT`. An encoded entry is:

```text
PLAINTEXT_BUFFER:
    magic[8] = "ANTPLAIN"
    runtime_version : uint32
    encoded_size    : uint32
    serialized PLAINTEXT metadata
    RNS polynomial coefficient data
```

The serialized `PLAINTEXT` records slots, scaling factor, scaling-factor
degree, ring degree, active/allocated prime counts, NTT state, and signed
64-bit RNS coefficient storage. Its exact interpretation is tied to the ANT
runtime version, polynomial degree, modulus chain, encoding level, and scale.
It is therefore provider/configuration-specific and fundamentally different
from Open64's current `raw_f32_le` asset.

The ACE offline tool currently backs up the message container as
`<file>.raw_data` and replaces the original pathname with the encoded
container. Open64 must not copy that replacement policy: source-model data and
compiler-intermediate coefficient assets are immutable inputs, and every
derived ACE package must be a separately named checkpoint artifact.

### Format comparison

| Property | Source SafeTensors | Open64 S6-0c coefficient asset | ACE `DE_MSG_F32` | ACE `DE_PLAINTEXT` |
| --- | --- | --- | --- | --- |
| Semantic stage | Original/folded model parameter | CKKS-oriented encode input | CKKS-oriented encode input | Encoded CKKS plaintext |
| Numeric payload | Usually F32 in source tensor layout | Explicit little-endian F32 rows/bias/masks | Host-written F32 entries | RNS polynomial coefficients and plaintext state |
| Self-identifying file | SafeTensors header and JSON directory | No | `!ANTFHE\0` header and entry type | `!ANTFHE\0` header, type plus `ANTPLAIN` per entry |
| Entry directory | SafeTensors JSON | Tensor TCON/value rows in `.B` | In-file LUT | In-file LUT |
| Scale and level | Not CKKS state | CKKS state belongs to WHIRL encode/use | LUT fields | LUT plus encoded plaintext state |
| Directly accepted by ACE `Pt_mgr` | No | No | Yes | Yes |
| Provider-specific | No | No | No until runtime encoding | Yes |

Open64 and ACE therefore use the same broad *message values before encoding*
concept, but not the same file format. Open64's current derived asset is
closest semantically to ACE `DE_MSG_F32`; it is not byte-compatible with that
container and is not equivalent to ACE `DE_PLAINTEXT`.

## Identifying a Data File

Use metadata, not a filename guess:

1. An ACE runtime data file begins with `!ANTFHE\0`; its header entry type says
   message or encoded plaintext. An encoded entry additionally begins with
   `ANTPLAIN`.
2. A SafeTensors source has a plausible eight-byte header length followed by a
   valid JSON tensor directory. Open64 WHIRL labels it
   `storage_format=safetensors`.
3. An Open64 `raw_f32_le` side asset has no magic. Its authoritative identity
   comes from the companion `.B`: `storage_format`, final relative path,
   Tensor TCON, tensor key, byte range, SHA-256, and lineage/generation fields.

The `.conv-plaintexts.f32` suffix is useful for humans but is not a format
proof. If the companion WHIRL or a derived manifest is absent, arbitrary raw
F32 bytes cannot be classified reliably as original data or converted
coefficient data. This is a deliberate current limitation and must be stated
in artifact handoffs.

## ACE Packaging Boundary

Keep the current Open64 `raw_f32_le` asset through CKKS semantic conformance.
It is deterministic, provider-independent, and inspectable through WHIRL.
CKKS2C is the correct boundary for converting that artifact family into the
ACE runtime handoff:

1. Read and authenticate the Open64 ranges through their Tensor TCON/value
   records.
2. Emit an ACE `DE_MSG_F32` container with `!ANTFHE\0`, aligned entries, LUT,
   model/file identity, and the exact encode scale and level required by each
   `ckks.encode` use.
3. Generate `Pt_from_msg()` or `Pt_from_msg_ofst()` calls against those stable
   ACE entry indices.
4. Publish the ACE message container as a new auxiliary artifact; do not
   overwrite the SafeTensors source or Open64 coefficient asset.
5. Treat optional conversion to ACE `DE_PLAINTEXT` as a later explicit offline
   packaging mode. Publish it under another final name and bind it to the exact
   ANT runtime/configuration identity.

This division keeps SYNC-6 focused on correct CKKS semantics while matching
the runtime staging used by ACE. It also leaves later optimization free to
deduplicate or pre-encode plaintexts without changing the `-O0` semantic IR.

## End-to-End Execution Flow

```text
immutable model SafeTensors
  |  external value + Tensor TCON + exact range + SHA-256
  v
typed source lookup and authenticated F32 read
  |  DSL_IR_Image_Get_External_Tensor_Reference
  |  Read_F32_Source
  v
ACE-compatible column-first Conv recipe
  |  output-column-first row preparation; no im2col matrix
  |  folded OIHW weights + folded bias
  v
derived little-endian F32 bytes
  |  feature rows, spatially expanded bias, stride masks
  v
temporary side payload: <checkpoint>.conv-plaintexts.f32.tmp
  |  one canonical side-file Tensor TCON per semantic byte range
  v
new external common.tensor_const.v1 values
  |  source lineage for rows/bias; generation provenance for masks
  v
explicit CKKS semantic DAG
  |  rotate -> encode -> mul -> rescale -> add
  |  encode expanded bias -> add
  v
checkpoint validation and atomic publication
  |  publish auxiliary payload/report first
  v
<checkpoint>.B published last as the commit marker
```

No arrow writes to the source SafeTensors file. The flow is copy-on-write:
source ranges are authenticated inputs, and all CKKS-oriented layouts are new
derived artifacts with explicit lineage.

## Stage 1: Resolve and Authenticate Source Parameters

The Conv producer begins in
`osprey/be/vho/fhe_ckks_conv_materialize.cxx`.
`Prepare_Assets()` resolves the exact weight and bias values for each live Conv
context. `Read_F32_Source()` then:

1. calls `DSL_IR_Image_Get_External_Tensor_Reference()`;
2. requires the expected Tensor TCON, `float32`, and a nonempty four-byte-aligned
   range;
3. opens the referenced SafeTensors file with mode `rb`;
4. seeks through the SafeTensors header to the recorded data range;
5. hashes the exact bytes and compares the digest with WHIRL metadata;
6. decodes little-endian IEEE binary32 values; and
7. rejects a short read, a checksum mismatch, NaN, or infinity.

The runtime-only `DSL_IR_EXTERNAL_TENSOR_REFERENCE` view is declared in
`osprey/common/com/dsl_ir_image.h`. It exposes the value ID, producer node,
descriptor TY, ST, Tensor TCON, data format, side-file path, tensor key, byte
range, checksum, dtype, shape, and layout. Its strings are borrowed and must
not survive an owner-PU mutation or image reset.

The Tensor TCON is not the payload. It is the canonical compiler record that
binds a tensor's semantic type to its external path, byte range, element
contract, alignment, and checksum. Its fixed fields are defined by
`DSL_TENSOR_TCON_RECORD` in `osprey/common/com/dsl_tensor_fold.h`.

## Stage 2: Build the CKKS-Oriented Conv Layout

The transformation is implemented by the FHE-owned recipe and asset services:

| Function | File | Responsibility |
| --- | --- | --- |
| `VHO_FHE_CKKS_Build_Column_Conv_Recipe()` | `osprey/be/vho/fhe_ckks_conv_recipe.cxx` | Validate Conv geometry and build the accepted ACE-compatible output-column-first recipe |
| `VHO_FHE_CKKS_Build_Column_Conv_F32_Row_Bytes()` | same | Serialize one feature row as little-endian F32 bytes |
| `VHO_FHE_CKKS_Build_Conv_Expanded_Bias_F32()` | `osprey/be/vho/fhe_ckks_conv_assets.cxx` | Repeat each output-channel bias over its output spatial positions |
| `VHO_FHE_CKKS_Build_Stride_Compaction_F32_Masks()` | same | Generate source-free plaintext masks for stride-two compaction |

This path follows ANT ACE's column-first metakernel orientation. It does not
construct an im2col matrix. For every `(input channel, kernel row, kernel
column)` feature, it emits a coefficient row whose valid output-channel and
spatial slots contain the corresponding folded OIHW coefficient. Slots made
invalid by padding are zero. Bias expansion similarly produces the layout
expected by the final plaintext addition.

This work changes layout, not model semantics. BatchNorm has already been
folded into the source Conv weight and bias before this stage. The recipe must
not re-read or reconstruct pre-fold BatchNorm parameters.

## Stage 3: Create the Derived Side Payload

`Prepare_Assets()` derives these checkpoint-owned names:

```text
<checkpoint>.conv-plaintexts.f32.tmp   temporary producer output
<checkpoint>.conv-plaintexts.f32       final auxiliary artifact
<checkpoint>.conv-report.txt.tmp       temporary conversion report
<checkpoint>.conv-report.txt           final conversion report
```

It registers the temporary/final pair with the checkpoint transaction before
opening the temporary payload with mode `wb`. The final path is not opened or
modified by the producer.

`Write_Asset()` writes each feature row, expanded bias, or mask. Before a write
it computes the full SHA-256, creates a canonical
`DSL_TENSOR_TCON_STORAGE_SIDE_FILE_DENSE` Tensor TCON, and asks the TCON service
for the canonical physical range. If byte-identical semantic tensors have
already been interned, the returned TCON may reuse an earlier range. In that
case the writer records the returned offset and does not append duplicate
bytes.

The path persisted in WHIRL is the relative leaf name, for example:

```text
secure_resnet20.ckks_ops.B.conv-plaintexts.f32
```

It is not the temporary name and not a build-host absolute path. The `.B` and
its side payload are therefore one relocatable artifact family and must be
moved together. A representative logical value URI printed by `ir_b2a` is:

```text
raw_f32_le://secure_resnet20.ckks_ops.B.conv-plaintexts.f32#conv_s1_row_0?offset=0&length=65536&checksum=<sha256>
```

## Stage 4: Materialize New WHIRL Values

The producer does not repurpose the original model constant. It creates new
external tensor values immediately before the source Conv definition.

For feature rows and expanded bias, `Typed_Request()` fills
`DSL_IR_TYPED_EXTERNAL_TENSOR_VALUE_REQUEST`, and
`DSL_IR_Materialize_Typed_External_Tensor_Values()` in
`osprey/common/com/dsl_ir_rewrite.cxx` performs one preflighted transaction.
Every request carries:

- the source owner PU and source `DSL_IR_VALUE_ID`;
- a captured source handle proving the source while its PU was active;
- the derived rank-one descriptor TY and Tensor TCON;
- side-file path, tensor key, canonical byte offset and length, and SHA-256;
- insertion point and source position; and
- transformation name, version, and ordinal.

The transaction validates the entire request array before creating symbols,
WN nodes, or managed-image records. It then creates an external
`common.tensor_const.v1` definition and records attributes including:

```text
value_kind=external_data
storage_format=raw_f32_le
storage_file=<derived leaf path>
storage_tensor_key=<row or bias key>
storage_byte_offset=<canonical offset>
storage_byte_length=<canonical length>
storage_checksum=<sha256>
tensor_tcon_idx=<derived Tensor TCON>
dsl.converted_from_owner_pu_st=<source owner>
dsl.converted_from_value_id=<source value>
dsl.transformation_name=<stable transformation>
dsl.transformation_version=1
dsl.transformation_ordinal=<row ordinal>
```

This is the durable relationship from original data to derived data. The
source value, its TY, its Tensor TCON, and its SafeTensors bytes remain intact.
The derived value receives its own TY/TCON/range and points backward through
lineage metadata.

Stride masks have no source model tensor. They use the source-free generated
external tensor transaction and carry generation name/version, geometry
manifest hash, stage/diagonal ordinal, and variant signature instead of
`converted_from` lineage.

## Stage 5: Consume the Derived Values in CKKS Semantics

After all external values exist, `Expand_Job()` builds the Conv policy and
calls the CKKS Conv planner and expansion adapter. The fixed `-O0` plan is
explicit rather than fused:

```text
optional input rotation or duplication
for each feature row:
    rotate ciphertext input when required
    ckks.encode(derived external feature row)
    ckks.mul(ciphertext, plaintext)
    ckks.rescale(product)
    ckks.add(accumulator, product)
ckks.encode(derived expanded bias)
ckks.add(accumulator, plaintext bias)
optional stride-two compaction using generated masks
```

The planner is in `osprey/be/vho/fhe_ckks_conv_plan.cxx`; native WHIRL
construction and CKKS state binding are coordinated by
`osprey/be/vho/fhe_ckks_conv_expand.cxx` and
`osprey/be/vho/fhe_ckks_conv_materialize.cxx`.

Each `ckks.encode` references a derived external value ID and checksum. This is
how executable CKKS semantics consume the side payload without placing the
large coefficient bytes inside `.B`. The source Conv is atomically replaced by
the expansion only after its whole plan and state transitions validate.

## Stage 6: Publish the Artifact Family Atomically

The checkpoint driver owns publication; the Conv producer must not rename
files itself. The materialization path in `osprey/be/be/driver.cxx` performs:

1. all-PU semantic and image validation;
2. checkpoint finalization;
3. `Write_Global_Info()` and close of the temporary `.B`;
4. publication of registered auxiliary artifacts in deterministic path order;
5. publication of `.B` last as the commit marker; and
6. completion cleanup.

`osprey/be/vho/fhe_checkpoint.cxx` publishes with same-filesystem,
no-replacement `link(temp, final)` followed by `unlink(temp)`, while signals
are blocked across the state change. A pre-existing final destination is an
error. On any validation, finalization, or publication failure, abort removes
temporary files and any finals published by this run. It never removes or
changes the source SafeTensors file.

Therefore the existence of the final `.B` means its registered side payloads
and report were already published successfully. A final side payload without
the final `.B` is not a committed checkpoint and is rolled back by the
transaction's failure path.

## Ownership Boundaries

| Layer | Owns |
| --- | --- |
| Frontend/SYNC-2 capture | Original SafeTensors path, exact ranges/checksums, source values and Tensor TCONs |
| FHE VHO producer | Source authentication, column-first recipe, derived bytes, transformation provenance, CKKS event plan, semantic diagnostics |
| `common/com` | Generic typed/generated external value transactions, Tensor TCON canonicalization, WHIRL/image consistency and rollback |
| Checkpoint infrastructure | Temporary/final endpoint reservation, auxiliary publication, `.B` commit marker, abort cleanup |
| CKKS2C/runtime adapter | Lowering `ckks.encode` and the remaining CKKS semantic DAG to runtime API calls |

FHE code must not bypass a common transaction by directly editing WN, ST, TY,
Tensor TCON, or mapped-image tables. Generic infrastructure must not embed Conv
layout policy or ACE metakernel semantics.

## Required Invariants

A change to this flow is acceptable only when all of these remain true:

1. The source external tensor is opened read-only and its exact bytes match the
   checksum persisted in WHIRL.
2. Derived bytes go only to a registered temporary auxiliary artifact.
3. Every derived range has a matching descriptor TY, Tensor TCON, byte range,
   checksum, and external tensor value.
4. Source-derived rows and bias retain `converted_from` lineage; source-free
   masks retain generation and geometry provenance.
5. WHIRL stores the final relative side-file leaf, never the temporary path.
6. `ckks.encode` consumes the derived value; no code pretends raw F32 bytes are
   already a CKKS plaintext object.
7. The source Conv is replaced only after complete plan/state validation.
8. Auxiliary artifacts publish before `.B`; `.B` is the final commit marker.
9. Failure leaves no validly named partial `.B` or `.tmp` artifact and never
   changes source data.
10. `ir_b2a -st -src` can expose path, key, range, checksum, Tensor TCON,
    lineage/generation provenance, CKKS operations, and source positions.

## Debugging Guide

Use the following order when a conversion fails:

1. Inspect the source external value and Tensor TCON in the input `.T`.
2. Confirm the source SafeTensors file exists relative to the invocation and
   independently hash the recorded byte range.
3. Check `Read_F32_Source()` diagnostics before investigating the recipe.
4. Compare recipe geometry, row count, and expected slot count.
5. Inspect the temporary conversion log for the first rejected asset/TCON.
6. In the output `.T`, find the new tensor key and verify its URI range and
   `dsl.converted_from_*` or generated-mask provenance.
7. Verify every row/bias value is consumed by a `ckks.encode` node.
8. If the side payload exists but `.B` does not, treat it as a failed
   transaction and inspect checkpoint publication diagnostics.

Never repair a failed run by editing the side payload or WHIRL metadata by
hand. Fix the producer or contract and regenerate the artifact family.

## Current Certification and Limits

The retained real-model Conv checkpoint is:

```text
/private/tmp/open64-fhe-sync6-s6-0c/artifacts/conv-ckks-materialized-final/
```

It contains a ten-PU binary, a 202,620,928-byte derived plaintext side asset,
the conversion report, and a separate-process `ir_b2a -st -src` trace. The
artifact certifies 21 Conv contexts, 5,691 feature rows, 21 expanded biases,
104 masks, 28,927 CKKS operations, and zero executable physical Conv nodes.

This does not complete S6-0c. ReLU, residual add, pooling, flatten, and linear
must still be expanded to CKKS semantic WHIRL before CKKS2C acceptance. It also
does not claim that the side payload contains serialized ACE runtime plaintext
objects or ciphertexts. It contains authenticated little-endian F32 source
material for explicit scheme-level encoding.

## Extending the Flow

When adding linear or another plaintext-bearing operator:

1. define and test the semantic layout recipe independently;
2. read existing source values through the typed external reference API;
3. authenticate source bytes before semantic use;
4. write new derived bytes through the registered checkpoint auxiliary;
5. create canonical side-file Tensor TCONs using the returned physical range;
6. materialize typed values with source lineage, or generated values with
   source-free geometry provenance;
7. build explicit CKKS semantic operations that consume those values;
8. replace the source operator through the whole-PU transaction; and
9. retain `.B`, side payload, report, diagnostics, and `ir_b2a -st -src`
   evidence.

Do not overwrite the original model payload, mutate a canonical source TY to
represent a derived layout, reuse one value ID for source and derived data,
embed large coefficient arrays in `.B`, or publish a side payload outside the
checkpoint transaction.
