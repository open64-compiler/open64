# SYNC-6 Generated Mask Geometry Manifest

Status: normative FHE producer contract for S6-0c generated plaintext masks.
The generic source-free external tensor transaction is owned by common/com;
this document owns the meaning and canonical digest of the geometry evidence
that the FHE producer supplies to that transaction. See
`FHE-SYNC6-S6-0C-DETAILED-EXECUTION-PLAN.md` and
`FHE-SYNC6-CKKS-IR-CONFORMANCE-GATE.md`.

## Purpose And Boundary

The first consumer is the explicit `-O0` stride-two packing network. Its mask
values are generated from proved slot geometry, not converted from a source
weight tensor. Every generated external tensor therefore carries:

- a side-file range and SHA-256 for its exact rank-1 F32 bytes;
- a digest of this complete geometry manifest;
- the selected whole-PU CKKS variant signature digest; and
- exact stage and diagonal ordinals.

The manifest proves the role of each mask in one reviewed geometry. It does
not prove CKKS scale, level, precision, key availability, numerical error, or
provider execution. Those remain FHE semantic-gate and runtime certification
obligations. Common/com validates the fixed request shape, lowercase digest
syntax, ownership, external tensor evidence, and tuple uniqueness; it does not
interpret this FHE schema.

## Canonical Bytes

`geometry_manifest_sha256` is the lowercase hexadecimal SHA-256 of the exact
manifest bytes. Version 1 uses a restricted canonical JSON encoding:

1. UTF-8 without a byte-order mark and with exactly one final LF byte.
2. Object keys sorted lexicographically by their ASCII byte sequence.
3. No insignificant whitespace: separators are exactly `,` and `:`.
4. Strings contain ASCII only and use JSON escapes only when required.
5. Integers use unsigned canonical decimal notation except
   `signed_rotation`, which may use one leading `-`. No leading zero is
   allowed except the value zero itself.
6. Booleans use JSON `true` or `false`. Floats, null, exponent notation, and
   duplicate object keys are forbidden.
7. Arrays preserve semantic order. Stage and diagonal arrays are dense and
   ordered by their explicit zero-based ordinals.

A conforming producer may use an equivalent language-independent encoder. The
focused Python producer uses `json.dumps(value, sort_keys=True,
separators=(",", ":"), ensure_ascii=True) + "\n"` after schema validation.
Hashing a parsed and reserialized unvalidated document is not permitted.

## Version 1 Schema

The root object contains exactly these fields:

| Field | Contract |
| --- | --- |
| `schema` | Exact string `open64.fhe.sync6.generated-mask-geometry.v1`. |
| `generation_name` | Stable dotted name; first release uses `fhe.ckks.stride_compaction.mask`. |
| `generation_version` | Positive integer; `1` for this schema and algorithm. |
| `variant_signature_sha256` | SHA-256 of the selected canonical `VHO_FHE_CKKS_SIGNATURE_VARIANT.signature_bytes`. |
| `slot_count` | Positive CKKS slot count matching the selected FHE config. |
| `dtype` | Exact string `float32`. |
| `byte_order` | Exact string `little_endian_ieee_binary32`. |
| `encrypted_layout` | Exact selected layout name, initially `ckks.packed`. |
| `rotation_convention` | Exact string `dest[s]=source[(s+r) mod slot_count]`. |
| `input` | Static logical tensor geometry before this packing network. |
| `output` | Static logical tensor geometry after this packing network. |
| `side_file` | Basename and whole-file digest/length of the generated mask payload. |
| `stages` | Dense ordered stage objects described below. |

The `input` and `output` objects contain exactly `shape`, `layout`, and
`active_slots`. `shape` is a nonempty array of positive extents in logical
NCHW order, `layout` is `nchw_flattened_channels_first`, and `active_slots`
is positive and no greater than `slot_count`.

The `side_file` object contains exactly `basename`, `byte_length`, and
`sha256`. The basename must not contain a directory separator. The length is
positive, and the digest is lowercase 64-hex. The all-PU checkpoint registers
this file as an auxiliary artifact and publishes it before the final `.B`
commit marker.

Each stage contains exactly:

| Field | Contract |
| --- | --- |
| `stage_ordinal` | Dense zero-based ordinal. |
| `operation` | Stable dotted operation name for the linear packing stage. |
| `diagonals` | Nonempty dense ordered diagonal array. |

Each diagonal contains exactly:

| Field | Contract |
| --- | --- |
| `diagonal_ordinal` | Dense zero-based ordinal within its stage. |
| `signed_rotation` | Rotation under the root convention; zero is legal. |
| `tensor_key` | Unique external tensor key in the side payload. |
| `byte_offset` | Element-aligned byte offset in the payload. |
| `byte_length` | Exactly `slot_count * 4`. |
| `sha256` | Digest of the exact little-endian F32 mask bytes. |
| `nonzero_count` | Number of exact `1.0f` elements; all other elements are exact `0.0f`. |

Ranges may not overlap, must remain within the whole side file, and must be
ordered by `(stage_ordinal, diagonal_ordinal)`. Tensor keys and ranges are
unique. Every request passed to the common transaction must match one exact
diagonal's manifest digest, variant digest, stage/diagonal tuple, tensor key,
range, and checksum. No manifest entry may remain unused, and no generated
mask request may be absent from the manifest.

## Variant Identity

`variant_signature_sha256` is not an opaque label, call ordinal, state
version, or context ID. The FHE producer computes it from the canonical whole-
PU signature bytes after all structured event plans pass validation. Equal
contexts that reuse one physical variant share the digest. Any change in the
complete executable plan, including operator order, operand roles, tensor
types/layouts, result states, key requirements, rotations, or plaintext asset
digests, must change the signature bytes and therefore the digest.

The digest is stable within the binary artifact and its retained evidence
family. It is not a promise that unrelated captures with different stable IR
identities produce the same bytes. The FHE semantic gate joins it to the
active selected variant before creating generated values.

## Validation And Failure

Before common/com mutation, the FHE producer must:

1. Validate the manifest schema and canonical byte encoding.
2. Hash the exact bytes and compare `geometry_manifest_sha256`.
3. Recompute the active variant signature digest and compare it with the root
   and every generated-value request.
4. Verify complete dense stage/diagonal coverage and exact side-file ranges.
5. Read every range, validate exact F32 0/1 bytes, checksum, and nonzero count.
6. Match the reviewed source/event geometry and CKKS plan that selected the
   packing network.

Unknown fields, malformed canonical bytes, wrong variant, duplicate or
missing tuple, overlapping range, checksum mismatch, non-binary F32 value,
or unused manifest entry fail before native mutation. A later checkpoint
failure removes temporary mask payload, manifest/report, and `.B`; no final
or `.tmp` artifact may masquerade as a valid S6-0c result.

## Retained Evidence

The successful S6-0c family retains the exact canonical manifest, generated
F32 payload, conversion report, `.ckks_ops.B`, separate-process
`ir_b2a -st -src` trace, command log, diagnostics, and `SHA256SUMS`. The dump
must show source-free generation name/version, geometry-manifest digest,
variant-signature digest, stage/diagonal tuple, tensor TCON, and side-file
range/checksum without any `dsl.converted_from_*` lineage.
