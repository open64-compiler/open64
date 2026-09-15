# Open64 FHE Runtime C ABI Version 1 Contract

Status: normative public C ABI contract for SYNC-5 and SYNC-6

## Authority And Scope

The sole highest FHE security, semantic, and milestone authority remains
`doc/DSC_FHE_Compiler_Architecture_and_Integration_Plan_v0.10.docx`, with the
repository SHA-256 recorded by the consolidated plan. This document is the
single normative source for the public Open64 FHE C ABI version 1. The
project-approved ACE ANT decision selects the first provider and maps this ABI
to `FHErt_ant`; it does not change the v0.10 privacy boundary or create a
second public ABI.

ABI v1 selects the v0.10 library-call MVP. Generated C uses high-level calls
for the complete ResNet-20 path. Provider adapters may expand those calls into
ACE or other provider primitives privately. SYNC-5 freezes the header, the
complete generated-C call schedule, and the mock behavior. SYNC-6 links the
same generated C and implements the same calls with the ACE provider. SYNC-6
must not add a generated-C call, change a signature, reorder a semantic call,
or infer a provider-specific lowering surface.

The exact ACE pin has not yet been shown to import an evaluation-only public
context, required evaluation keys, and ciphertexts without creating or
retaining a secret key. Until that capability is demonstrated against the
exact source and build hashes, SYNC-6 is blocked. A reviewed ACE patch or a new
immutable pin may remove the block; an embedded secret-key lifecycle may not.

## Required Execution Profile

The required profile identifier is `open64-fhe-resnet20-ckks-v1`. It covers
the deterministic ResNet-20/CIFAR-10 source fixture and all generated-C calls
needed for ciphertext input through ciphertext logits output. BatchNorm is
folded into plaintext convolution weights and biases before runtime lowering;
ABI v1 has no BatchNorm call, and a surviving runtime BatchNorm call is a
SYNC-5 failure.

SYNC-5 publishes four distinct canonical UTF-8 JSON artifacts from the exact
`application.mid.B` and generated-C candidate. They use sorted keys, no
insignificant whitespace, LF line endings, and one final LF.

- `semantic-event-schedule-v1.json` is the execution-expanded sequence of
  evaluation calls. Its sole exhaustive field definition is the normative
  six-entry schema table under **Model Package Admission**. A reused
  class-centric PU has one static ordinal but distinct sequence positions,
  visits, event identities, and invocation identities.
- `abi-evaluation-call-census-v1.json` contains static and execution-expanded
  successful-call counts for each evaluation symbol, the total counts, and the
  SHA-256 of the ordered semantic schedule. It excludes imports, exports,
  retain/release/destroy, inspection, diagnostics, broker transport, and failed
  call attempts.
- `transport-lifecycle-transcript-v1.json` records successful broker transport,
  host capability acquire/release/claim, broker create/destroy, import/export,
  retain/release, and diagnostic activity. It is not an evaluation schedule or
  census.
- `failure-transcript-v1.json` records each rejected or failed attempt, its
  status, cursor before/after, and public-token, public-reference, and provider-
  object censuses before/after. It is not included in successful-call counts.

The generated C, all four artifacts, all referenced descriptors, the model
package directory, and the ABI header are retained and hashed together.

The certified SYNC-2 fixture supplies these reconciliation invariants. SYNC-5
must reproduce them from the currently revalidated input rather than copy the
numbers into a generated report.

| Semantic or ABI event | Static generated callsites | Execution-weighted calls |
| --- | ---: | ---: |
| Convolution with folded plaintext weight and bias | 13 | 21 |
| Residual ciphertext add | 5 | 9 |
| Bootstrap for a ReLU boundary | 11 | 19 |
| ReLU normalization | 11 | 19 |
| ReLU polynomial stage 0, degree 7 | 11 | 19 |
| ReLU polynomial stage 1, degree 15 | 11 | 19 |
| ReLU polynomial stage 2, degree 13 | 11 | 19 |
| ReLU reconstruction | 11 | 19 |
| Global average pool | 1 | 1 |
| Layout conversion for flatten | 1 | 1 |
| Plain-weight classifier linear | 1 | 1 |
| BatchNorm | 0 | 0 |
| **Successful ABI evaluation-call total** | **87** | **147** |

The 13/11/5 static counts reflect the class-centric PU representation; the
21/19/9 counts reflect the nine block calls. The six ReLU calls contribute
66 static and 114 execution-expanded calls; with convolution, residual add,
pooling, layout conversion, and linear, the exact total is 87/147. Plain-tensor
imports, transport, release, destroy, inspection, diagnostics, and failed
attempts appear only in the applicable transcript. Any mismatch with the
revalidated SYNC-3 input blocks SYNC-5 and requires an explained input or
lowering change, not an edited expected count.

Within one inference, the call order is:

1. before invoking generated C, the trusted launcher creates the explicit
   broker; the host imports one public context, one server keyset and one model
   package, creates one model, and imports every manifest-declared plaintext
   tensor, then binds every non-argument model asset; these are lifecycle/
   transport events;
2. import the ciphertext input, creating an inference-session cursor at zero;
3. execute all 147 evaluation calls in persisted semantic schedule order;
4. for each ReLU source context, call bootstrap, normalization, polynomial
   stages 0, 1, and 2, then reconstruction without interleaving another
   operation on that value;
5. execute global average pool, the certified flatten/layout step, and the
   classifier linear call, then export encrypted logits; and
6. release values in a reverse-lifetime order, destroy the model, release the
   keyset, and destroy the context.

The MVP always emits exactly one `open64_fhe_layout_convert_v1` call for
flatten. This remains a distinct semantic event even when the provider can
implement it as metadata-only work; generated C and the 87/147 census never
omit it.

## Public Types And Status Values

The following header sketch is normative for names, parameter order, and
ownership. The checked-in header created during SYNC-5 must match it. Fixed
width integers come from `<stdint.h>` and `NULL` from `<stddef.h>`. Public
structures begin with `abi_version` and `struct_size`; a caller initializes
unknown trailing bytes to zero, and fields are append-only within v1.

```c
#include <stddef.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

#define OPEN64_FHE_ABI_VERSION_V1 1u

typedef struct open64_fhe_broker_v1_s *open64_fhe_broker_v1_t;
typedef struct open64_fhe_host_bootstrap_v1_s
    *open64_fhe_host_bootstrap_v1_t;
typedef struct open64_fhe_launcher_capability_v1_s
    *open64_fhe_launcher_capability_v1_t;
typedef struct open64_fhe_context_v1_s *open64_fhe_context_v1_t;
typedef struct open64_fhe_keyset_v1_s *open64_fhe_keyset_v1_t;
typedef struct open64_fhe_model_package_v1_s *open64_fhe_model_package_v1_t;
typedef struct open64_fhe_model_v1_s *open64_fhe_model_v1_t;
typedef struct open64_fhe_ciphertext_v1_s *open64_fhe_ciphertext_v1_t;
typedef struct open64_fhe_plain_tensor_v1_s *open64_fhe_plain_tensor_v1_t;

typedef uint32_t open64_fhe_status_v1;
#define OPEN64_FHE_STATUS_OK UINT32_C(0)
#define OPEN64_FHE_STATUS_INVALID_ARGUMENT UINT32_C(1)
#define OPEN64_FHE_STATUS_INVALID_HANDLE UINT32_C(2)
#define OPEN64_FHE_STATUS_BUSY UINT32_C(3)
#define OPEN64_FHE_STATUS_ALIAS_FORBIDDEN UINT32_C(4)
#define OPEN64_FHE_STATUS_ABI_MISMATCH UINT32_C(5)
#define OPEN64_FHE_STATUS_ENVELOPE_INVALID UINT32_C(6)
#define OPEN64_FHE_STATUS_INTEGRITY_ERROR UINT32_C(7)
#define OPEN64_FHE_STATUS_KIND_MISMATCH UINT32_C(8)
#define OPEN64_FHE_STATUS_KEY_CLASS_MISMATCH UINT32_C(9)
#define OPEN64_FHE_STATUS_PROVIDER_MISMATCH UINT32_C(10)
#define OPEN64_FHE_STATUS_CONFIG_MISMATCH UINT32_C(11)
#define OPEN64_FHE_STATUS_SECRET_KEY_FORBIDDEN UINT32_C(12)
#define OPEN64_FHE_STATUS_CAPABILITY_MISSING UINT32_C(13)
#define OPEN64_FHE_STATUS_UNSUPPORTED UINT32_C(14)
#define OPEN64_FHE_STATUS_CALL_ORDER_MISMATCH UINT32_C(15)
#define OPEN64_FHE_STATUS_BUFFER_TOO_SMALL UINT32_C(16)
#define OPEN64_FHE_STATUS_PROVIDER_TERMINATED UINT32_C(17)
#define OPEN64_FHE_STATUS_CONTEXT_POISONED UINT32_C(18)
#define OPEN64_FHE_STATUS_OUT_OF_MEMORY UINT32_C(19)
#define OPEN64_FHE_STATUS_IO_ERROR UINT32_C(20)
#define OPEN64_FHE_STATUS_INTERNAL_ERROR UINT32_C(21)
#define OPEN64_FHE_STATUS_TRUST_FAILURE UINT32_C(22)
#define OPEN64_FHE_STATUS_MODEL_PACKAGE_INVALID UINT32_C(23)
#define OPEN64_FHE_STATUS_MODEL_REQUIREMENT_MISMATCH UINT32_C(24)
#define OPEN64_FHE_STATUS_NO_DIAGNOSTIC UINT32_C(25)
#define OPEN64_FHE_STATUS_TRUST_DOMAIN_MISMATCH UINT32_C(26)

typedef uint32_t open64_fhe_operation_kind_v1;
#define OPEN64_FHE_OP_CONV2D_PLAIN UINT32_C(1)
#define OPEN64_FHE_OP_RESIDUAL_ADD UINT32_C(2)
#define OPEN64_FHE_OP_BOOTSTRAP UINT32_C(3)
#define OPEN64_FHE_OP_RELU_NORMALIZE UINT32_C(4)
#define OPEN64_FHE_OP_RELU_POLY_STAGE UINT32_C(5)
#define OPEN64_FHE_OP_RELU_RECONSTRUCT UINT32_C(6)
#define OPEN64_FHE_OP_AVERAGE_POOL UINT32_C(7)
#define OPEN64_FHE_OP_LAYOUT_CONVERT UINT32_C(8)
#define OPEN64_FHE_OP_LINEAR_PLAIN UINT32_C(9)

typedef struct open64_fhe_broker_desc_v1 {
  uint32_t abi_version;
  uint32_t struct_size;
  uint32_t flags;
  uint32_t reserved;
} open64_fhe_broker_desc_v1;

typedef struct open64_fhe_context_desc_v1 {
  uint32_t abi_version;
  uint32_t struct_size;
  uint8_t execution_profile_sha256[32];
  uint8_t provider_identity_sha256[32];
  uint8_t provider_manifest_sha256[32];
  uint8_t config_identity_sha256[32];
  uint32_t flags;
  uint32_t reserved;
} open64_fhe_context_desc_v1;

typedef struct open64_fhe_import_binding_v1 {
  uint32_t abi_version;
  uint32_t struct_size;
  uint8_t authenticated_principal_sha256[32];
  uint8_t session_identity_sha256[32];
  uint8_t request_nonce[32];
  uint8_t expected_envelope_sha256[32];
  uint8_t model_identity_sha256[32];
  uint8_t auth_key_id_sha256[32];
  uint8_t auth_tag_hmac_sha256[32];
} open64_fhe_import_binding_v1;

typedef struct open64_fhe_export_binding_v1 {
  uint32_t abi_version;
  uint32_t struct_size;
  uint8_t authenticated_principal_sha256[32];
  uint8_t session_identity_sha256[32];
  uint8_t request_nonce[32];
  uint8_t model_identity_sha256[32];
  uint8_t final_output_identity_sha256[32];
  uint8_t auth_key_id_sha256[32];
  uint8_t auth_tag_hmac_sha256[32];
} open64_fhe_export_binding_v1;

typedef struct open64_fhe_export_receipt_v1 {
  uint32_t abi_version;
  uint32_t struct_size;
  uint8_t actual_envelope_sha256[32];
  uint8_t auth_key_id_sha256[32];
  uint8_t receipt_hmac_sha256[32];
} open64_fhe_export_receipt_v1;

typedef struct open64_fhe_operation_desc_v1 {
  uint32_t abi_version;
  uint32_t struct_size;
  uint32_t sequence_index;
  uint32_t operation_kind;
  uint32_t operation_ordinal;
  uint32_t visit_index;
  uint32_t input_count;
  uint32_t reserved;
  uint8_t semantic_event_id[32];
  uint8_t descriptor_sha256[32];
  uint8_t operation_identity_sha256[32];
  uint8_t config_identity_sha256[32];
  uint8_t input_value_identity_sha256[2][32];
  uint8_t output_value_identity_sha256[32];
  uint8_t input_tensor_identity_sha256[2][32];
  uint8_t output_tensor_identity_sha256[32];
  uint8_t input_layout_identity_sha256[2][32];
  uint8_t output_layout_identity_sha256[32];
  uint8_t payload_sha256[32];
  const void *payload;
  uint64_t payload_size;
} open64_fhe_operation_desc_v1;

typedef struct open64_fhe_ciphertext_info_v1 {
  uint32_t abi_version;
  uint32_t struct_size;
  uint32_t level;
  int32_t scale_bits;
  uint32_t components;
  uint32_t active_slots;
  uint64_t size_bytes;
  uint8_t tensor_identity_sha256[32];
  uint8_t layout_identity_sha256[32];
} open64_fhe_ciphertext_info_v1;

#if defined(__cplusplus)
static_assert(sizeof(open64_fhe_status_v1) == 4, "ABI status width");
static_assert(sizeof(void *) == 8, "ABI v1 requires 64-bit pointers");
static_assert(sizeof(open64_fhe_broker_desc_v1) == 16, "broker desc layout");
static_assert(sizeof(open64_fhe_context_desc_v1) == 144, "context desc layout");
static_assert(sizeof(open64_fhe_import_binding_v1) == 232, "import binding layout");
static_assert(sizeof(open64_fhe_export_binding_v1) == 232, "export binding layout");
static_assert(sizeof(open64_fhe_export_receipt_v1) == 104, "export receipt layout");
static_assert(sizeof(open64_fhe_operation_desc_v1) == 496, "operation desc layout");
static_assert(sizeof(open64_fhe_ciphertext_info_v1) == 96, "ciphertext info layout");
static_assert(offsetof(open64_fhe_operation_desc_v1, payload) == 480,
              "operation desc field offset");
#else
_Static_assert(sizeof(open64_fhe_status_v1) == 4, "ABI status width");
_Static_assert(sizeof(void *) == 8, "ABI v1 requires 64-bit pointers");
_Static_assert(sizeof(open64_fhe_broker_desc_v1) == 16, "broker desc layout");
_Static_assert(sizeof(open64_fhe_context_desc_v1) == 144, "context desc layout");
_Static_assert(sizeof(open64_fhe_import_binding_v1) == 232, "import binding layout");
_Static_assert(sizeof(open64_fhe_export_binding_v1) == 232, "export binding layout");
_Static_assert(sizeof(open64_fhe_export_receipt_v1) == 104, "export receipt layout");
_Static_assert(sizeof(open64_fhe_operation_desc_v1) == 496, "operation desc layout");
_Static_assert(sizeof(open64_fhe_ciphertext_info_v1) == 96, "ciphertext info layout");
_Static_assert(offsetof(open64_fhe_operation_desc_v1, payload) == 480,
               "operation desc field offset");
#endif
```

The normative header uses the shown `extern "C"` guard around all declarations
and closes it after the function prototypes. Every ABI-visible integer is a
fixed-width type. ABI v1 requires a 64-bit data-pointer ABI and the shown
`sizeof` and `offsetof` assertions are mandatory in C and C++ builds; adding a
supported compiler cannot change these layouts.

Every one of the nine operation kinds uses one common canonical descriptor-
payload object. It has exactly `schema_version:u32` (1),
`semantic_event_id:sha256`, `invocation_identity_sha256:sha256`,
`operation_identity_sha256:sha256`, `operation_kind:u32`,
`input_value_identity_sha256:sha256[]`,
`output_value_identity_sha256:sha256`,
`input_tensor_identity_sha256s:sha256[]`,
`output_tensor_identity_sha256:sha256`,
`weight_tensor_identity_sha256:sha256-or-null`,
`bias_tensor_identity_sha256:sha256-or-null`,
`asset_tensor_identity_sha256s:sha256[]`,
`input_layout_identity_sha256s:sha256[]`,
`output_layout_identity_sha256:sha256`, `signed_rotations:s32[]`,
`key_requirement_identity_sha256:sha256`, `input_states` (array),
`output_state` (object), and `parameters` (object). It has no unknown key.
`invocation_identity_sha256` equals `operation_identity_sha256`; both equal the
schedule event field. Each state has exactly `level:u32`, `scale_bits:s32`,
`components:u32` greater than zero, and `active_slots:u32` greater than zero.
The input-value, input-tensor, input-layout, and input-state array lengths all
equal value-input arity and use the same parameter order. Rotation arrays are strictly
increasing and unique; a descriptor that uses no rotations has exactly `[]`.

The value-input order, weight/bias presence, asset order, and exact `parameters`
keys are:

| Operation kind | Value inputs in order | Weight, bias, assets | Exact `parameters` keys |
| --- | --- | --- | --- |
| `CONV2D_PLAIN` | `[input]` | weight and folded bias required; assets `[]` | `input_nchw`, `weight_oihw`, `output_nchw`, `stride_hw`, `padding_tlbr`, `dilation_hw`, `groups` |
| `RESIDUAL_ADD` | `[main_path,shortcut_path]` | weight/bias null; assets `[]` when states already match, otherwise `[normalization]` | `shape`, `broadcast`, `alignment_mode` |
| `BOOTSTRAP` | `[input]` | weight/bias null; assets `[input_range]` | `relu_context_sha256`, `profile_identity_sha256`, `evaluation_schedule_identity_sha256`, `reason`, `target_level`, `target_scale_bits`, `active_slots` |
| `RELU_NORMALIZE` | `[input]` | weight/bias null; assets `[input_range,normalization]` | `relu_context_sha256`, `profile_identity_sha256`, `evaluation_schedule_identity_sha256`, `affine_convention` |
| `RELU_POLY_STAGE` | `[input]` | weight/bias null; assets `[coefficient]` | `relu_context_sha256`, `profile_identity_sha256`, `evaluation_schedule_identity_sha256`, `stage_index`, `degree`, `coefficient_order` |
| `RELU_RECONSTRUCT` | `[refreshed_input,stage2_result]` | weight/bias null; assets `[reconstruction]` | `relu_context_sha256`, `profile_identity_sha256`, `evaluation_schedule_identity_sha256`, `affine_convention` |
| `AVERAGE_POOL` | `[input]` | weight/bias null; assets `[scale]` | `input_shape`, `output_shape`, `mode`, `kernel_hw`, `stride_hw` |
| `LAYOUT_CONVERT` | `[input]` | weight/bias null; assets `[slot_permutation,mask]` | `input_shape`, `output_shape`, `output_strides`, `flatten_start_dim`, `flatten_end_dim` |
| `LINEAR_PLAIN` | `[input]` | weight and classifier bias required; assets `[]` | `input_features`, `output_features` |

The asset names in this table map exactly to weight-manifest roles:
`coefficient` to `poly_coefficient`, `scale` to `pool_scale`, and every other
name to the identical role. There is no implicit asset.

Unknown operation kinds or unknown required descriptor versions return
`UNSUPPORTED`. The required `open64-fhe-resnet20-ckks-v1` profile may not be
accepted if any required operation would later return `UNSUPPORTED`.
`context_import` rejects a missing profile-wide capability, and `model_create`
rejects a missing candidate-specific schedule, rotation, or key capability,
with `CAPABILITY_MISSING` before the first evaluation call.

## Model Package Admission

Model admission never looks up a hash in hidden process-global state.
`model_package_import` receives the complete versioned model-package envelope
bytes through an explicit broker. Its provider-neutral payload begins with an
eight-byte little-endian directory length, followed by that many bytes of a
canonical JSON directory and then the directory-addressed entry bytes. The
directory has exactly `schema_version` (`u32`, value 1),
`model_identity_sha256` (`sha256`), `source_artifact_identity` (`id`),
`source_artifact_sha256` (`sha256`), `execution_profile_sha256` (`sha256`),
`config_identity_sha256` (`sha256`), and `entries` (array). `entries` is sorted
by `name`; each entry has exactly
`name:id`, `kind:id` with literal value `canonical-json-v1`, `offset:u64`,
`length:u64` greater than zero, and `sha256:sha256`. Offsets are from the first
byte after the directory, must be contiguous, non-overlapping, and cover the
remainder exactly. Required
entry names are `semantic-event-schedule-v1.json`,
`abi-evaluation-call-census-v1.json`, `weight-manifest-v1.json`,
`operation-descriptor-directory-v1.json`, `rotation-manifest-v1.json`, and
`key-requirement-manifest-v1.json`; the name set has exactly these six members
and no seventh entry. The directory `config_identity_sha256` equals the outer
model-package envelope header, the imported context, and the identically named
field in every entry. Missing, duplicate, noncanonical, digest-
mismatched, or cross-reference-inconsistent data returns
`MODEL_PACKAGE_INVALID`; no model-package handle is published.

### Normative schemas for the six JSON entries

The following rules are exhaustive, not examples. The package directory and
each entry are JSON objects
with no unknown or duplicate keys. It is encoded as UTF-8 without BOM, object
keys are ASCII and lexicographically sorted, arrays retain the order stated
below, separators are exactly `,` and `:`, strings use no optional escapes,
integers use shortest decimal notation, and the object is followed by one LF;
the eight-byte directory length includes that LF.
There is no other whitespace. A `sha256` value is 64 lowercase hexadecimal
characters and denotes SHA-256 of the named canonical bytes including their
final LF. `u32`, `u64`, and `s32` are JSON integers in the corresponding C
range. An `id` is 1--255 ASCII characters matching `[A-Za-z0-9._:/-]+`. A
`b64` string is canonical padded RFC 4648 base64, has length divisible by four,
and matches `[A-Za-z0-9+/]*={0,2}`.

Every entry has exactly the common keys `schema_version` (`u32`, value 1),
`model_identity_sha256` (`sha256`), `config_identity_sha256` (`sha256`),
`source_artifact_identity` (`id`), `source_artifact_sha256` (`sha256`), and
`execution_profile_sha256` (`sha256`), plus the entry-specific keys below. The
common values must equal the package
directory values and the admitted context. `model_identity_sha256` is
`SHA256("OPEN64-FHE-MODEL-IDENTITY-V1\0" || UTF8(source_artifact_identity) ||
0x00 || source_artifact_sha256-bytes || config_identity_sha256-bytes ||
execution_profile_sha256-bytes)`. This nonrecursive identity is consistent
across all six entries; the outer envelope digest binds their complete content.

| Entry | Additional top-level keys | Entry record (exact keys and types) | Ordering and cardinality |
| --- | --- | --- | --- |
| `semantic-event-schedule-v1.json` | `value_declarations` (array), `events` (array), `final_output_value_identity_sha256:sha256` | Value declaration: `value_identity_sha256:sha256`, `tensor_identity_sha256:sha256`, `layout_identity_sha256:sha256`, `producer_kind:id`, `producer_sequence_index:u32-or-null`. Event: `sequence_index:u32`, `semantic_event_id:sha256`, `static_ordinal:u32`, `visit_index:u32`, `invocation_identity_sha256:sha256`, `source_context_sha256:sha256`, `operation_kind:u32`, `abi_symbol:id`, `descriptor_sha256:sha256`, `input_value_identity_sha256:sha256[]`, `output_value_identity_sha256:sha256` | Exactly 148 declarations sorted by unique value identity: one `model_input` with null producer index and one `event` declaration for each sequence index 0--146. Exactly 147 events sorted by `sequence_index` 0--146; event output identities are unique; static ordinals cover 0--86; visits for each static ordinal start at zero and are contiguous. |
| `abi-evaluation-call-census-v1.json` | `schedule_sha256:sha256`, `symbols` (array), `static_total:u32`, `dynamic_total:u32` | `operation_kind:u32`, `abi_symbol:id`, `static_count:u32`, `dynamic_count:u32` | Exactly nine records sorted by `operation_kind`; symbol and kind are unique; totals are 87 and 147. |
| `weight-manifest-v1.json` | `tensors` (array) | `tensor_identity_sha256:sha256`, `role:id`, `envelope_sha256:sha256`, `dtype:id`, `shape:u64[]`, `encoding:id`, `data_sha256:sha256`, `consumer_descriptor_sha256:sha256[]` | Nonempty, sorted by unique tensor identity; every dimension is 1--`UINT64_MAX`; consumer digests are sorted, unique, and nonempty; `role` is one of `weight`, `bias`, `poly_coefficient`, `normalization`, `reconstruction`, `pool_scale`, `input_range`, `slot_permutation`, or `mask`; `dtype`/`encoding` are `float64`/`ieee754-le-f64-v1`, except `slot_permutation` uses `uint32`/`uint32-le-v1`. |
| `operation-descriptor-directory-v1.json` | `descriptors` (array), `relu_profiles` (array) | Descriptor: `descriptor_sha256:sha256`, `semantic_event_id:sha256`, `sequence_index:u32`, `static_ordinal:u32`, `visit_index:u32`, `operation_kind:u32`, `operation_identity_sha256:sha256`, `config_identity_sha256:sha256`, `input_value_identity_sha256:sha256[]`, `output_value_identity_sha256:sha256`, `input_tensor_identity_sha256s:sha256[]`, `output_tensor_identity_sha256:sha256`, `input_layout_identity_sha256s:sha256[]`, `output_layout_identity_sha256:sha256`, `payload_sha256:sha256`, `payload_base64:b64`. ReLU profile: `profile_identity_sha256:sha256`, `evaluation_schedule_identity_sha256:sha256`, `bootstrap_input_state:state`, `bootstrap_output_state:state`, `normalize_output_state:state`, `stages` (array), `reconstruction_input_states` (`state[]`), `reconstruction_output_state:state`. | Exactly 147 descriptors sorted by unique `descriptor_sha256`; sequence indexes cover 0--146; semantic-event and operation identities are unique; static ordinals cover 0--86. `payload_base64` decodes to the actual canonical common descriptor-payload bytes. Profiles are nonempty and sorted by unique profile identity. |
| `rotation-manifest-v1.json` | `signed_rotations` (`s32[]`), `by_descriptor` (array) | `descriptor_sha256:sha256`, `signed_rotations:s32[]` | Exactly 147 records sorted by unique descriptor digest; every rotation list is strictly increasing; the top-level list is the sorted unique union. Zero is forbidden. |
| `key-requirement-manifest-v1.json` | `required_key_class_mask:u32`, `requirements` (array) | `key_requirement_identity_sha256:sha256`, `descriptor_sha256:sha256`, `key_class_mask:u32`, `signed_rotations:s32[]`, `bootstrap_target_level:u32-or-null` | Exactly 147 records sorted by unique descriptor digest; identity and descriptor digest are unique; rotation lists are strictly increasing; the top-level mask is the OR of record masks and is exactly `0x0000001f`. |

The census rows are fixed to these `(symbol, kind, static, dynamic)` tuples:
`(open64_fhe_conv2d_plain_v1,1,13,21)`,
`(open64_fhe_residual_add_v1,2,5,9)`,
`(open64_fhe_bootstrap_v1,3,11,19)`,
`(open64_fhe_relu_normalize_v1,4,11,19)`,
`(open64_fhe_relu_poly_stage_v1,5,33,57)`,
`(open64_fhe_relu_reconstruct_v1,6,11,19)`,
`(open64_fhe_average_pool_v1,7,1,1)`,
`(open64_fhe_layout_convert_v1,8,1,1)`, and
`(open64_fhe_linear_plain_v1,9,1,1)`.

Each ReLU profile's `stages` has exactly three records sorted by
`stage_index:u32` 0, 1, 2. A stage has exactly `stage_index:u32`, `degree:u32`,
`coefficient_tensor_identity_sha256:sha256`, `input_state:state`, and
`output_state:state`; degrees are respectively 7, 15, and 13. Its
`reconstruction_input_states` has exactly two states in
`[refreshed_input,stage2_result]` order. The evaluation-schedule identity is
`SHA256("OPEN64-FHE-RELU-SCHEDULE-V1\0" || canonical-stages-array-bytes)`.
The profile identity is
`SHA256("OPEN64-FHE-RELU-PROFILE-V1\0" || canonical-profile-object-bytes)`
where the canonical object omits only `profile_identity_sha256`; it includes
the schedule identity and every state. These byte arrays use the entry's JSON
canonicalization rules without a trailing LF inside the containing object.

For every ReLU source context, bootstrap, normalize, all three polynomial
stages, and reconstruction descriptors name the same profile and evaluation-
schedule identities. Their input/output states equal, in order, profile
`bootstrap_input_state`, `bootstrap_output_state`, `normalize_output_state`,
each stage's input/output, and `reconstruction_input_states`/output. Adjacent
states are equal byte for byte: normalize input is the bootstrap output, stage
0 input is the normalize output, each later stage input is the prior stage
output, and reconstruction inputs are the bootstrap output and stage-2 output.
Each stage coefficient identity matches exactly one `poly_coefficient` tensor
and the corresponding `RELU_POLY_STAGE` descriptor asset and explicit handle.

All common descriptor keys are present for every kind; unused weight/bias
fields are JSON null and unused asset/rotation arrays are `[]`, never absent.
A non-null weight, bias, or asset identity must match exactly one
`weight-manifest-v1.json.tensors[].tensor_identity_sha256` with the role required
by the operation table. Neither `envelope_sha256` nor `data_sha256` is a logical
reference; they are integrity subfields of that tensor record. The common
descriptor's ordered value/tensor/layout input arrays and its output value,
tensor, and layout equal its directory record and the corresponding value
declarations byte for byte. In particular, the two entries for residual add
are `[main_path,shortcut_path]`, and the two for reconstruction are
`[refreshed_input,stage2_result]`; neither operation assumes equal input layouts.

The kind-specific parameter types and ranges are exhaustive. `input_nchw`,
`weight_oihw`, `output_nchw`, and residual-add `shape` are arrays of exactly
four `u32` values in 1--`UINT32_MAX`. Average-pool `input_shape` and
`output_shape` are likewise four positive `u32` NCHW values. Layout-convert
`input_shape` is four positive `u32` NCHW values and its `output_shape` is
exactly two positive `u32` values equal to
`[input_shape[0],input_shape[1]*input_shape[2]*input_shape[3]]` without
overflow. `stride_hw`,
`dilation_hw`, and `kernel_hw` are exactly two such positive values.
`padding_tlbr` is four `u32` values in 0--`UINT32_MAX`. `groups` is a `u32` in
1--`min(input_channels,output_channels)` and divides both channel counts.
`target_level` is `u32`; `target_scale_bits` is `s32` in 1--`INT32_MAX`;
`active_slots`, `input_features`, and `output_features` are `u32` in
1--`UINT32_MAX`. `stage_index` is `u32` 0--2 and `degree` is `u32` equal to
7/15/13 respectively. `output_strides` is a positive `u64` array of the output
rank with last element 1 and each preceding element equal to the following
stride times following dimension without overflow. In this required profile,
`flatten_start_dim` and `flatten_end_dim` are `u32` values 1 and 3 respectively.

`operation_kind` is 1--9 and maps to the census symbol. `mode` is exactly
`global_mean`; `reason` is `pre_relu_range_refresh`; `coefficient_order` is
`ascending_power`; `affine_convention` is `manifest_asset_v1`; `broadcast` is
JSON false; `alignment_mode` is `none` with assets `[]` or
`manifest_normalize` with exactly one normalization asset.
Each of `relu_context_sha256`, `profile_identity_sha256`, and
`evaluation_schedule_identity_sha256` is a `sha256`. Parameter objects never
contain JSON null or an unlisted field.

Each `semantic_event_id` is
`SHA256("OPEN64-FHE-EVENT-V1\0" || LE32(sequence_index) ||
LE32(static_ordinal) || LE32(visit_index) || LE32(operation_kind) ||
invocation_identity_sha256-bytes || source_context_sha256-bytes)`. A value-
declaration tensor identity is
`SHA256("OPEN64-FHE-VALUE-TENSOR-V1\0" || model_identity_sha256-bytes ||
producer-kind-byte || LE32(producer-index-or-UINT32_MAX))`; its value identity
is `SHA256("OPEN64-FHE-VALUE-V1\0" || tensor_identity_sha256-bytes ||
layout_identity_sha256-bytes || producer-kind-byte ||
LE32(producer-index-or-UINT32_MAX))`. Producer-kind byte 0 with
`UINT32_MAX` denotes the sole model input; byte 1 with 0--146 denotes an event.
Each weight-manifest tensor identity is
`SHA256("OPEN64-FHE-TENSOR-V1\0" || UTF8(role) || 0x00 ||
UTF8(dtype) || 0x00 || concatenated-LE64(shape) || data_sha256-bytes)`.

For each descriptor, `descriptor_sha256` is SHA-256 of
`"OPEN64-FHE-DESCRIPTOR-V1\0"`, followed by little-endian `static_ordinal` and
`sequence_index`, `visit_index`, and `operation_kind`, the raw semantic-event,
operation-identity, config-identity, and payload digests in that order, then
the decoded payload bytes. The payload hash must match those
bytes and the earlier operation-kind table. A key-requirement identity is
SHA-256 of `"OPEN64-FHE-KEY-REQUIREMENT-V1\0"` followed by the canonical
requirement record with its `key_requirement_identity_sha256` member omitted.
Each schedule event names exactly one descriptor with its event ID, sequence,
kind, ABI symbol, static ordinal, visit, invocation/operation identity, ordered
value inputs, and output; every descriptor is referenced exactly once. The 148
value declarations have unique value and tensor identities, and producer
assignments are bijective: exactly one is
the `model_input`/null producer, and every sequence index has exactly one
`event` declaration whose value is that event's output. Ciphertext import
accepts only tensor/layout identities equal to the model-input declaration and
assigns its value identity. Each event input references a declaration produced
by either that model input or a lower sequence index, has exactly one producer,
and follows the per-kind ABI order and arity above. No undeclared value, second
producer, forward edge, self-edge, or unconsumed non-final output is allowed;
therefore the 147-event graph is closed and acyclic. The schedule
`final_output_value_identity_sha256` equals the last event's
`output_value_identity_sha256` and must equal the export binding's
`final_output_identity_sha256` byte for byte.

The generated `open64_fhe_operation_desc_v1` is a fixed-field view of that
same directory record and payload, not another descriptor language. Its
`sequence_index`, `operation_kind`, `operation_ordinal`, and `visit_index`
equal JSON `sequence_index`, `operation_kind`, `static_ordinal`, and
`visit_index`; `semantic_event_id`, `descriptor_sha256`,
`operation_identity_sha256`, and `config_identity_sha256` equal the same-named
JSON bytes. `input_count` is the common descriptor's input arity (1 or 2), and
the first `input_count` rows of C `input_value_identity_sha256` equal JSON
`input_value_identity_sha256`, C `input_tensor_identity_sha256` equal JSON
`input_tensor_identity_sha256s`, and C `input_layout_identity_sha256` equal
JSON `input_layout_identity_sha256s`, all in order; every byte in an unused
second row is zero.
Its output value/tensor/layout and `payload_sha256` fields equal the same-named
JSON fields. `reserved` is zero, and `payload`/`payload_size` address exactly
the decoded `payload_base64` bytes. Any byte mismatch returns
`CALL_ORDER_MISMATCH` before provider dispatch.

For each evaluation call, the runtime checks that each ciphertext token's value
identity equals the corresponding descriptor input value, and obtains its
tensor and layout identities through the same checked metadata path as
`ciphertext_inspect`; parameter position 0 or 1 must equal the corresponding
descriptor array row and value declaration. A successful output is published
only with the descriptor's output tensor/layout/value identities. Explicit
plain `weight`, `optional_bias`, and `coefficients` handles must equal the one
weight-manifest tensor identity and role selected by that descriptor. After
higher-priority handle/trust-domain validation, a wrong-role or wrong-identity
plain handle returns `MODEL_REQUIREMENT_MISMATCH` before provider dispatch.

Census
static counts are grouped from distinct `(static_ordinal,abi_symbol)` pairs and
dynamic counts from all 147 events; both must match the fixed tuples, and
`schedule_sha256` must match the complete schedule entry bytes. Weight consumer
references must be the inverse of descriptor weight/bias/asset joins.

Every dynamic descriptor identity selects exactly one rotation record and one
key-requirement record. Their `descriptor_sha256` values match, and the common
descriptor `key_requirement_identity_sha256` equals the requirement record's
identity. The descriptor and both records contain the same `signed_rotations`
array; no comparison is made against kind-specific `parameters`. Residual add,
ReLU normalize, ReLU polynomial stages, and ReLU reconstruction are nonrotating
and require `[]`. Other descriptors publish their actual derived set, which may
be empty only when lowering proves no rotation. `AVERAGE_POOL` is never empty:
SYNC-5 explicitly publishes its actual strictly increasing set derived from
the frozen input/output layout, input/output shape, kernel, and stride, and the
schedule-selected descriptor, rotation record, and key-requirement record must
all match it. The rotation top-level union covers all 147 records. Bootstrap
requirement records alone have a non-null target level.

Any type/range/encoding/hash/order/cardinality/identity/cross-reference failure,
unknown key, unreferenced record, missing record, or unexpected extra record
returns `MODEL_PACKAGE_INVALID` before worker dispatch. A well-formed package
whose complete config, rotations, key classes, or operation set differs from
the admitted context/keyset returns `MODEL_REQUIREMENT_MISMATCH`.

`model_create` consumes the imported package as a borrowed handle. Before
publishing the model, the broker parses all required entries, verifies every
value declaration, ReLU profile, operation descriptor, and plaintext reference,
reconciles the exact
87/147 census with the expanded semantic sequence, and compares the complete
operation, signed-rotation, public/evaluation/relinearization/rotation/bootstrap
key, and provider capability requirements with the imported context and
keyset. A missing provider capability returns `CAPABILITY_MISSING`; a missing
or extra model/key/config/rotation requirement returns
`MODEL_REQUIREMENT_MISMATCH`. Either failure occurs before the first evaluation
call, leaves the schedule cursor nonexistent, and publishes no model.

## Public Function Surface

All byte counts are `uint64_t`. Import calls consume neither the caller's
envelope bytes nor any input handle. Evaluation inputs are borrowed while the
context remains `ACTIVE`: success and recoverable failure preserve their
semantic values, while fatal termination preserves only public token bits and
reference counts for release because the provider objects are destroyed.

```c
open64_fhe_status_v1 open64_fhe_launcher_capability_acquire_v1(
    open64_fhe_host_bootstrap_v1_t host_bootstrap,
    open64_fhe_launcher_capability_v1_t *out_capability);

open64_fhe_status_v1 open64_fhe_launcher_capability_release_v1(
    open64_fhe_launcher_capability_v1_t *capability);

open64_fhe_status_v1 open64_fhe_broker_create_v1(
    open64_fhe_launcher_capability_v1_t *privileged_launcher,
    const open64_fhe_broker_desc_v1 *desc,
    open64_fhe_broker_v1_t *out_broker);

open64_fhe_status_v1 open64_fhe_broker_destroy_v1(
    open64_fhe_broker_v1_t *broker);

open64_fhe_status_v1 open64_fhe_context_import_v1(
    open64_fhe_broker_v1_t broker,
    const open64_fhe_context_desc_v1 *desc,
    const void *public_context_envelope,
    uint64_t envelope_size,
    open64_fhe_context_v1_t *out_context);

open64_fhe_status_v1 open64_fhe_context_destroy_v1(
    open64_fhe_context_v1_t *context);

open64_fhe_status_v1 open64_fhe_keyset_import_v1(
    open64_fhe_context_v1_t context,
    const void *evaluation_keyset_envelope,
    uint64_t envelope_size,
    open64_fhe_keyset_v1_t *out_keyset);

open64_fhe_status_v1 open64_fhe_keyset_release_v1(
    open64_fhe_keyset_v1_t *keyset);

open64_fhe_status_v1 open64_fhe_model_package_import_v1(
    open64_fhe_broker_v1_t broker,
    const void *model_package_envelope,
    uint64_t envelope_size,
    open64_fhe_model_package_v1_t *out_package);

open64_fhe_status_v1 open64_fhe_model_package_release_v1(
    open64_fhe_model_package_v1_t *package);

open64_fhe_status_v1 open64_fhe_model_create_v1(
    open64_fhe_context_v1_t context,
    open64_fhe_keyset_v1_t keyset,
    open64_fhe_model_package_v1_t package,
    open64_fhe_model_v1_t *out_model);

open64_fhe_status_v1 open64_fhe_model_destroy_v1(
    open64_fhe_model_v1_t *model);

open64_fhe_status_v1 open64_fhe_plain_tensor_import_v1(
    open64_fhe_model_v1_t model,
    const void *plain_tensor_envelope,
    uint64_t envelope_size,
    open64_fhe_plain_tensor_v1_t *out_plain_tensor);

open64_fhe_status_v1 open64_fhe_model_bind_asset_v1(
    open64_fhe_model_v1_t model,
    open64_fhe_plain_tensor_v1_t asset);

open64_fhe_status_v1 open64_fhe_plain_tensor_retain_v1(
    open64_fhe_plain_tensor_v1_t plain_tensor);

open64_fhe_status_v1 open64_fhe_plain_tensor_release_v1(
    open64_fhe_plain_tensor_v1_t *plain_tensor);

open64_fhe_status_v1 open64_fhe_ciphertext_import_v1(
    open64_fhe_model_v1_t model,
    const open64_fhe_import_binding_v1 *import_binding,
    const void *ciphertext_envelope,
    uint64_t envelope_size,
    open64_fhe_ciphertext_v1_t *out_ciphertext);

open64_fhe_status_v1 open64_fhe_ciphertext_export_v1(
    open64_fhe_model_v1_t model,
    open64_fhe_ciphertext_v1_t ciphertext,
    const open64_fhe_export_binding_v1 *export_binding,
    void *envelope_buffer,
    uint64_t buffer_capacity,
    uint64_t *out_required_or_written,
    open64_fhe_export_receipt_v1 *out_receipt);

open64_fhe_status_v1 open64_fhe_ciphertext_retain_v1(
    open64_fhe_ciphertext_v1_t ciphertext);

open64_fhe_status_v1 open64_fhe_ciphertext_release_v1(
    open64_fhe_ciphertext_v1_t *ciphertext);

open64_fhe_status_v1 open64_fhe_conv2d_plain_v1(
    open64_fhe_model_v1_t model,
    open64_fhe_ciphertext_v1_t input,
    open64_fhe_plain_tensor_v1_t weight,
    open64_fhe_plain_tensor_v1_t optional_bias,
    const open64_fhe_operation_desc_v1 *desc,
    open64_fhe_ciphertext_v1_t *out_result);

open64_fhe_status_v1 open64_fhe_residual_add_v1(
    open64_fhe_model_v1_t model,
    open64_fhe_ciphertext_v1_t main_path,
    open64_fhe_ciphertext_v1_t shortcut_path,
    const open64_fhe_operation_desc_v1 *desc,
    open64_fhe_ciphertext_v1_t *out_result);

open64_fhe_status_v1 open64_fhe_bootstrap_v1(
    open64_fhe_model_v1_t model,
    open64_fhe_ciphertext_v1_t input,
    const open64_fhe_operation_desc_v1 *desc,
    open64_fhe_ciphertext_v1_t *out_result);

open64_fhe_status_v1 open64_fhe_relu_normalize_v1(
    open64_fhe_model_v1_t model,
    open64_fhe_ciphertext_v1_t input,
    const open64_fhe_operation_desc_v1 *desc,
    open64_fhe_ciphertext_v1_t *out_result);

open64_fhe_status_v1 open64_fhe_relu_poly_stage_v1(
    open64_fhe_model_v1_t model,
    open64_fhe_ciphertext_v1_t input,
    open64_fhe_plain_tensor_v1_t coefficients,
    const open64_fhe_operation_desc_v1 *desc,
    open64_fhe_ciphertext_v1_t *out_result);

open64_fhe_status_v1 open64_fhe_relu_reconstruct_v1(
    open64_fhe_model_v1_t model,
    open64_fhe_ciphertext_v1_t refreshed_input,
    open64_fhe_ciphertext_v1_t stage2_result,
    const open64_fhe_operation_desc_v1 *desc,
    open64_fhe_ciphertext_v1_t *out_result);

open64_fhe_status_v1 open64_fhe_average_pool_v1(
    open64_fhe_model_v1_t model,
    open64_fhe_ciphertext_v1_t input,
    const open64_fhe_operation_desc_v1 *desc,
    open64_fhe_ciphertext_v1_t *out_result);

open64_fhe_status_v1 open64_fhe_layout_convert_v1(
    open64_fhe_model_v1_t model,
    open64_fhe_ciphertext_v1_t input,
    const open64_fhe_operation_desc_v1 *desc,
    open64_fhe_ciphertext_v1_t *out_result);

open64_fhe_status_v1 open64_fhe_linear_plain_v1(
    open64_fhe_model_v1_t model,
    open64_fhe_ciphertext_v1_t input,
    open64_fhe_plain_tensor_v1_t weight,
    open64_fhe_plain_tensor_v1_t optional_bias,
    const open64_fhe_operation_desc_v1 *desc,
    open64_fhe_ciphertext_v1_t *out_result);

open64_fhe_status_v1 open64_fhe_ciphertext_inspect_v1(
    open64_fhe_ciphertext_v1_t ciphertext,
    open64_fhe_ciphertext_info_v1 *out_info);

open64_fhe_status_v1 open64_fhe_get_last_diagnostic_v1(
    open64_fhe_context_v1_t context,
    char *utf8_buffer,
    uint64_t buffer_capacity,
    uint64_t *out_required_or_written);

open64_fhe_status_v1 open64_fhe_get_last_broker_diagnostic_v1(
    open64_fhe_broker_v1_t broker,
    char *utf8_buffer,
    uint64_t buffer_capacity,
    uint64_t *out_required_or_written);

#ifdef __cplusplus
} /* extern "C" */
#endif
```

`model_bind_asset` is the only source for descriptor assets that are not an
explicit evaluation argument. It accepts only manifest roles `input_range`,
`normalization`, `reconstruction`, `pool_scale`, `slot_permutation`, or `mask`.
Before the first ciphertext import, the host imports every such required plain
tensor and binds each required tensor identity exactly once. A successful bind
adds one model-owned reference in an immutable table keyed by
`tensor_identity_sha256`; the caller may then release its importing reference.
A duplicate, unmanifested, wrong-role, or wrong-model asset returns
`MODEL_REQUIREMENT_MISMATCH` and adds no reference. On the first ciphertext
import, after the higher-priority handle, trust-domain, envelope, integrity,
authentication, and identity checks, the runtime checks asset completeness
under the model lock. A missing required asset returns
`MODEL_REQUIREMENT_MISMATCH`, publishes no ciphertext, consumes no request
nonce, and leaves the table unchanged and unsealed; the host may bind the
missing asset and retry. Only an asset-complete, otherwise successful first
import atomically publishes its ciphertext and changes the table from open to
sealed. Every other failed first import also leaves it unsealed. Bind after a
successful seal returns `BUSY`. Evaluation resolves descriptor
asset identities only through this table. Thus there is no process-global or
provider-private implicit asset. Model destroy releases all model-owned asset
references in reverse binding order.

Weights and biases for convolution/linear and polynomial coefficients are not
model-bound: the generated evaluation call supplies their plain handles
explicitly, and the per-call identity/role checks above apply. Asset binding,
plain import/release, and table sealing are lifecycle events, not part of the
87/147 evaluation census.

`optional_bias` may be `NULL` only when the operation descriptor declares no
bias. All other handles are non-null. Ciphertext export and diagnostic queries
use this exact buffer protocol. Once `out_required_or_written` is validated, it
is set to zero before any other fallible work. A null buffer with zero capacity,
or a non-null buffer that is too small, returns `BUFFER_TOO_SMALL`, leaves the
buffer unchanged, and sets the field to the exact required byte count. Success
writes the complete envelope or diagnostic JSON, writes its exact byte count
(including the diagnostic's final LF, with no NUL terminator), and never partly
writes a record. Every other failure leaves the buffer unchanged and the field
zero.

For ciphertext export, the caller also sets `out_receipt.abi_version` and
`out_receipt.struct_size` and zeroes its three digest fields. A null receipt or
wrong version/size returns `INVALID_ARGUMENT`; every preflight or failure leaves
the digest fields zero, and only a formal successful export fills all three.

`ciphertext_inspect` is a validation/telemetry query and is not emitted as a
semantic generated-C model call. The caller supplies `abi_version` and
`struct_size` and zeroes the remaining fields; failure leaves the payload
fields zero. ABI v1 operations are synchronous, so this required profile has no
separate synchronization call.

Diagnostics are canonical UTF-8 JSON records with ABI status, operation ordinal and
identity, source identity, provider identity, configuration identity, worker
exit detail when applicable, and a redacted provider message. The record never
contains key bytes, ciphertext internals, client payload data, or filesystem
paths that reveal secret storage. `get_last_diagnostic` returns the calling
thread's context record. Once a context token has been validated, a failed
context/keyset/model/plain/ciphertext/evaluation/export call--including
`CALL_ORDER_MISMATCH`--atomically replaces that record; success leaves it.
`get_last_broker_diagnostic` similarly returns the calling thread's broker
record. A call is context-owned once its context or context-child argument is
validated; even a later cross-domain operand error (for example a package from
another broker passed to `model_create`) updates only that context record. A
call with a validated broker/package but no context--context import before
publication, model-package import/release, or broker destroy--updates only that
broker record. Exactly one record is updated per failed call. Capability
acquire/release, broker-create failure, and any failure for which no published
broker/context owner validates return only status.

With no applicable record, either query returns `NO_DIAGNOSTIC`, writes no
bytes, and leaves `out_required_or_written` zero. Diagnostic queries--including
successful, `BUFFER_TOO_SMALL`, and `NO_DIAGNOSTIC` queries--never create,
clear, or overwrite either record. A thread cannot overwrite another thread's
selection. Export preflight is the explicit no-record/no-transcript exception.

## Ownership And Failure Semantics

The following rules apply to every function and provider implementation.

- A caller initializes every output handle slot to `NULL`. Once the runtime
  accepts a writable null slot, every failure leaves it `NULL`.
- A non-null output slot is rejected before execution. If it names an input
  handle, the status is `ALIAS_FORBIDDEN`; otherwise it is
  `INVALID_ARGUMENT`. The slot, all inputs, and all reference counts remain
  unchanged.
- A successful create, import, or evaluation returns one distinct owning
  handle with reference count one. An evaluation output never aliases an
  input, a plaintext parameter, or another output.
- Evaluation and import inputs are borrowed. Success and recoverable failure
  leave their semantic values and reference counts unchanged. On fatal worker
  termination, provider values cease to exist; only the unchanged public token
  bits and reference counts remain, marked `POISONED`, solely for release.
  Provider calls that consume an object receive only a provider-private
  temporary, deep copy, or explicitly moved private object.
- Only `*_release_v1`, `*_destroy_v1`, and the explicitly documented post-claim
  `broker_create` path consume public handles. On successful release/destroy
  they release one ownership reference and set the caller's slot to `NULL`.
  Retain increments exactly one reference. Failed retain/release/destroy calls
  change no reference count except for the launcher-capability state rules
  stated below.
- Broker destroy returns `BUSY` until every context, keyset, model package,
  model, plaintext, and ciphertext token/reference owned by it--including a
  poisoned token--has been released. Otherwise it consumes the broker token,
  clears the immutable registry copy, invalidates its generation, and sets the
  slot to `NULL`.
- A release or destroy of `NULL`, a stale generation, the wrong handle kind,
  or a handle from another context returns `INVALID_HANDLE`. This rule makes
  double destroy and use after destroy deterministic instead of undefined
  behavior.
- Model creation records a checked lifetime dependency on its borrowed context
  and keyset and package without changing the caller's ownership reference.
  Keyset or package release returns `BUSY` while a live model depends on it.
  Model destroy returns `BUSY` while an operation is active, a ciphertext child
  is live, or a caller-owned plaintext reference remains; model-owned asset
  references alone do not block it and are released by successful destroy.
  Active-context destroy returns `BUSY`
  while any model, keyset, plaintext, or ciphertext child handle remains live
  or an operation is in flight; it changes no state. Normal successful destroy
  transitions `ACTIVE -> CLOSED`. After a fatal provider event, public child
  tokens remain releasable but are `POISONED`; releases discard those tokens
  without provider dispatch. Once they are released, poisoned-context destroy
  transitions `POISONED -> CLOSED` so a fresh context can be imported.
- Every broker/context/keyset/model-package/model/plaintext/ciphertext handle is
  a checked token carrying its owning broker identity and broker generation in
  addition to kind, context identity where applicable, and token generation.
  A broker identity is `SHA256("OPEN64-FHE-BROKER-V1\0" ||
  deployment_identity_sha256-bytes || LE64(launch_generation) ||
  launcher_random_nonce[32])`. Combining two otherwise-valid tokens
  whose broker identity or generation differs returns
  `TRUST_DOMAIN_MISMATCH` before provider dispatch and changes neither token.
  Implementations must not dereference an unchecked caller pointer.
- The runtime validates ABI version, envelope, provider/config identities,
  descriptor kind, model schedule, handle ownership, and alias rules before
  invoking the provider. A provider or C++ exception never crosses the C ABI.

For calls with multiple possible errors, the normative priority is: (1) a
recognized token belonging to a poisoned context returns `CONTEXT_POISONED`,
except that release/destroy and diagnostic queries remain available; (2)
invalid output pointer, ABI version/size, or alias; (3) invalid/stale/wrong-kind
handle; (4) valid handles from different broker identities/generations, which
returns `TRUST_DOMAIN_MISMATCH`; (5) envelope framing/length; (6) a declared
secret key class, which returns `SECRET_KEY_FORBIDDEN`; (7) integrity digest;
(8) registry or HMAC trust, returning `TRUST_FAILURE`; (9) kind, key-class,
provider, or configuration mismatch; (10) model/package/capability/call-order
validation; then (11) provider execution. This priority applies to every
function and prevents a lower-priority parse or handle error from masking a
poisoned child. An accepted output-handle slot is set to `NULL` before steps
5--11.

Each context serializes provider work. Calls from multiple client threads are
permitted, but the runtime orders them through the context queue and preserves
the frozen sequence. Model creation installs the immutable expanded semantic
schedule. Each accepted ciphertext input import creates an inference-session
identity, initializes its cursor to sequence index zero, and carries both on
derived ciphertexts. An evaluation must match every field of the cursor's
expanded entry. Only publication of its distinct owning output advances the
cursor by one. `CALL_ORDER_MISMATCH`, any other recoverable error, and a fatal
provider event leave it unchanged. The runtime rejects cross-session operands;
export is valid only after cursor 147. Destroy during queued or running work
returns `BUSY`. Handles from different contexts are never interoperable.

## Versioned Import And Export Envelope

Every public-context, keyset, model-package, ciphertext, and plaintext-tensor
import, and every ciphertext export, uses this wire envelope. The fixed header
is exactly 160 bytes. Integer fields are unsigned little-endian; byte arrays
have no byte-order conversion. The payload immediately follows the header.

| Offset | Field | Size | Rule |
| ---: | --- | ---: | --- |
| 0 | magic | 8 | bytes `4f 36 34 46 48 45 31 00` (`O64FHE1\0`) |
| 8 | envelope_version | 4 | exactly `1` |
| 12 | header_size | 4 | exactly `160` |
| 16 | kind | 4 | one `OPEN64_FHE_ENVELOPE_KIND_*` value below |
| 20 | key_class_mask | 4 | bit mask below; zero for non-keyset kinds |
| 24 | provider_identity_sha256 | 32 | exact provider identity, or all-zero for a provider-neutral kind |
| 56 | config_identity_sha256 | 32 | exact resolved CKKS/security configuration |
| 88 | payload_length | 8 | exactly `envelope_size - 160`, without overflow |
| 96 | payload_sha256 | 32 | SHA-256 of payload bytes |
| 128 | envelope_sha256 | 32 | SHA-256 of bytes 0--127, 32 zero bytes, then payload |

```c
#define OPEN64_FHE_ENVELOPE_HEADER_SIZE_V1 UINT32_C(160)
#define OPEN64_FHE_ENVELOPE_KIND_PUBLIC_CONTEXT UINT32_C(1)
#define OPEN64_FHE_ENVELOPE_KIND_KEYSET UINT32_C(2)
#define OPEN64_FHE_ENVELOPE_KIND_MODEL_PACKAGE UINT32_C(3)
#define OPEN64_FHE_ENVELOPE_KIND_PLAIN_TENSOR UINT32_C(4)
#define OPEN64_FHE_ENVELOPE_KIND_CIPHERTEXT UINT32_C(5)

#define OPEN64_FHE_KEY_CLASS_PUBLIC UINT32_C(0x00000001)
#define OPEN64_FHE_KEY_CLASS_EVALUATION UINT32_C(0x00000002)
#define OPEN64_FHE_KEY_CLASS_RELINEARIZATION UINT32_C(0x00000004)
#define OPEN64_FHE_KEY_CLASS_ROTATION UINT32_C(0x00000008)
#define OPEN64_FHE_KEY_CLASS_BOOTSTRAP UINT32_C(0x00000010)
#define OPEN64_FHE_KEY_CLASS_SECRET UINT32_C(0x80000000)
```

Model packages and plaintext tensors are provider-neutral: their provider hash
is all-zero and their canonical directory binds content to a configuration.
The plaintext-tensor directory has exactly `schema_version`,
`tensor_identity_sha256`, `dtype`, `shape`, `encoding`, `data_offset`,
`data_length`, and `data_sha256`; its addressed data covers the remainder
exactly. Public contexts, keysets, and ciphertexts are provider-specific. The
model-package, plaintext-tensor, and keyset directories are provider-neutral
schemas even when their addressed bytes are provider-private. A keyset payload
begins with the same eight-byte directory-length/canonical-directory layout as
a model package. Its directory has exactly `schema_version`,
`config_identity_sha256`, and `entries`; each entry has exactly
`object_sha256`, `offset`, `length`, and `key_class`. Entries are sorted by
unique object digest, ranges are contiguous and cover the payload remainder,
and `key_class` is exactly one non-secret key-class bit. The OR of all entry
bits must equal the outer `key_class_mask`.

ACE key blobs are opaque. The broker does not claim to infer their contents or
classify them independently. Every key object digest instead must have an exact
trusted-registry object record with the same key class, configuration, and
provider pin. The registry schema forbids the secret bit. A secret bit in the
outer header, directory, or registry returns `SECRET_KEY_FORBIDDEN`; an
unregistered or opaque-unknown object returns `TRUST_FAILURE`; a known but
inconsistent non-secret class returns `KEY_CLASS_MISMATCH`. All occur before
worker dispatch.

### Trusted launcher and provisioning registry

The platform-supplied opaque `host_bootstrap` is a host-integration prerequisite
available only to the trusted privileged launcher. It names the OS-protected
deployment resource, its offline-approved digest, a monotonic 64-bit generation
source, a cryptographic random source, and a non-exportable keystore verifier.
The FHE ABI neither creates nor serializes this prerequisite. Generated C,
server request handlers, and untrusted plugins never receive it, a launcher
capability, the registry address, or a keystore interface.

Before invoking generated C, `launcher_capability_acquire` borrows
`host_bootstrap`, opens and read-only maps the administrator/root-owned
deployment resource, verifies its offline-approved SHA-256, retains a reference
to the non-exportable verifier, and allocates a never-reused
`LE64(launch_generation)` plus independent 32-byte random nonce. Only after all
steps succeed does it publish one owning `MINTED` capability containing those
resources. The caller initializes the output slot to `NULL`. Any partial
failure releases the verifier if retained, unmaps/closes the registry if
opened, and wipes the tentative generation, nonce, and token in reverse
acquisition order; the output remains null. Capabilities are one-shot and have
no retain operation.

`broker_create` is privileged host initialization, not a generated-C call. It
accepts a pointer to a `MINTED` capability. Invalid arguments, ABI/struct size,
nonzero broker descriptor `flags`/`reserved`, non-null output slot, or
invalid/stale capability are pre-claim failures: they
do not consume the capability, so the launcher may correct the call or release
it. After those checks, broker creation atomically changes `MINTED -> CLAIMED`
and immediately nulls the caller's capability slot. From that point every
success or failure consumes it as `CLAIMED -> CONSUMED`; it is never reusable.
A simultaneous attempt that observes `CLAIMED` returns `BUSY` without consuming
another ownership reference; an attempt after consumption returns
`INVALID_HANDLE`.

`launcher_capability_release` is the only alternative consumer. On `MINTED` it
atomically changes `MINTED -> CONSUMED`, nulls the caller slot, unmaps/closes
the registry, releases and wipes the verifier reference, and wipes the
generation, nonce, and token. Release of null/stale/consumed returns
`INVALID_HANDLE`; release while claimed returns `BUSY` and changes nothing. It
performs no broker creation. For these three host-only functions, output/ABI/struct validation has
priority over capability validation; a valid `CLAIMED` token then returns
`BUSY`, and a null, stale, or `CONSUMED` token returns `INVALID_HANDLE`. They
have no envelope, key-class, schedule, or provider-dispatch phases. The
lifecycle and ownership matrix is therefore:

| Object | Producer | Borrowed by | Consumer and terminal rule |
| --- | --- | --- | --- |
| host bootstrap | platform host integration | capability acquire | external to this ABI; outlives every acquire call |
| launcher capability | successful acquire, refcount one, state `MINTED`; owns registry mapping, verifier reference, generation, and nonce | broker-create pre-claim validation | successful release, or every post-claim broker-create outcome; unmap/release/wipe; no retain/reuse |
| registry bytes | read-only mapping owned by the `MINTED` capability | broker-create pre-claim validation | copied after claim; mapping is unmapped by release or every post-claim outcome |
| broker | successful broker-create, refcount one | all broker-owned imports/queries | broker destroy after all owned tokens release |
| HMAC verifier capability | platform keystore through claimed launcher capability | broker only | released and verifier state wiped by failed post-claim create or broker destroy |

After claiming, `broker_create` copies the registry, generation, and nonce to
broker-private memory, validates the registry copy's canonical form and digest,
makes it immutable, and retains a separate broker-owned reference to the non-
exportable HMAC verifier; raw HMAC key bytes cannot be exported through this
ABI. Every post-claim outcome then unmaps/closes the capability's source
mapping, releases its verifier reference, wipes its generation/nonce/token, and
ends `CONSUMED`. The launcher drops privilege after the call, while a
successful broker remains able to verify tags. A malformed registry or pinned-digest mismatch returns `TRUST_FAILURE`;
a registry containing a secret bit returns `SECRET_KEY_FORBIDDEN`. Any failed
post-claim create wipes the copy and releases the verifier. Before `out_broker`
is published, only status is available because no diagnostic owner exists.
Broker destroy wipes the immutable copy, nonce/replay state, and releases the
verifier after the owned-token `BUSY` gate.

There is no partially specified signature/MAC protocol for this static
registry. Its canonical JSON has exactly `schema_version` (`u32`, 1),
`deployment_identity_sha256` (`sha256`), `auth_key_ids` (`sha256[]`), and
`records` (array). Auth-key IDs are sorted and unique and name 32-byte HMAC keys
available only through the launcher's OS-protected keystore capability. Records
are sorted by unique `envelope_sha256` and have exactly
`envelope_sha256:sha256`, `kind:u32` (one envelope-kind constant),
`provider_identity_sha256:sha256`, `config_identity_sha256:sha256`,
`key_class_mask:u32`, and `objects` (array). Object records are sorted by unique
`object_sha256` and have exactly `object_sha256:sha256`, `object_kind:id` (one
of `key_blob` or `payload`), and `key_class:u32`; non-key objects use kind
`payload` and key class zero. No mask may contain unknown bits or
`OPEN64_FHE_KEY_CLASS_SECRET`.

The two envelope SHA-256 fields provide corruption/integrity detection only; a
sender's self-reported digest is not authentication. Before worker dispatch,
static public-context, keyset, model-package, and model-bound weight envelopes
must pass framing/digest and canonical-directory checks, equality between
inner/outer metadata, and an exact registry envelope record. Keyset objects
must additionally match its object records one-for-one. An unknown envelope or
object returns `TRUST_FAILURE`; other metadata differences return the stable
mismatch status above.

### Authenticated ciphertext import and export

Dynamic ciphertext envelopes are not in the immutable registry. The broker
uses RFC 2104 HMAC with FIPS 180-4 SHA-256 and its full 32-byte output, compared
in constant time, with the 32-byte key named by `auth_key_id_sha256`; that ID
must be in the registry and resolved by the launcher's protected keystore.
Digest fields below are their raw 32 bytes. Import authentication covers:

`"O64FHE-IMPORT-V1\0" || principal || session || nonce ||
expected_envelope_sha256 || model_identity_sha256 || auth_key_id_sha256`.

The import tag is HMAC-SHA-256 of those bytes. It must match before ciphertext
worker dispatch; the expected hash must equal the actual whole-envelope hash.
Each `(key-id,principal,session,nonce)` is accepted once. An unknown key ID,
tag mismatch, identity mismatch, or replay returns `TRUST_FAILURE`. Only a
successful formal import consumes its nonce; every failure leaves it available.

Export authentication covers:

`"O64FHE-EXPORT-V1\0" || principal || session || nonce ||
model_identity_sha256 || final_output_identity_sha256 || auth_key_id_sha256`.

It intentionally contains no unknown future output-envelope digest. The final
identity must equal the last scheduled output. On a formal successful export,
the broker consumes the nonce, computes the actual envelope digest, writes it
to `out_receipt`, and sets `receipt_hmac_sha256` to HMAC-SHA-256 over
`"O64FHE-EXPORT-RECEIPT-V1\0"`, the complete covered export bytes above, and
the actual digest. `out_receipt.auth_key_id_sha256` repeats the verified key ID.
A null-buffer/zero-capacity or insufficient-buffer preflight validates handles,
binding tag, and required size but does not consume the nonce, write a
transcript/diagnostic, compute or expose the actual output digest, or modify
the caller-zeroed receipt. Only the later full successful export does so. All
other failures leave the receipt zero and do not consume the nonce.

A stored ciphertext is not authentic merely because its SHA-256 matches. It
requires a fresh valid import binding under an admitted HMAC key; otherwise
import returns `TRUST_FAILURE`.

`UNSUPPORTED` is not an allowed result for context, keyset, or ciphertext
import/export, or any required operation after the
`open64-fhe-resnet20-ckks-v1` context has been accepted. A provider unable to
perform one of them fails profile admission with `CAPABILITY_MISSING`.

## ACE Worker Containment Contract

The public runtime creates one supervised worker process for each successfully
imported public context. Each worker contains exactly one ACE global/singleton
context. Multiple public contexts therefore use multiple workers, never
multiple ACE contexts in one process. The broker serializes all calls for one
context; separate context workers may execute concurrently.

The public context state machine has a normal `ACTIVE -> CLOSED` path and a
fatal `ACTIVE -> POISONED -> CLOSED` path.

- A successful evaluation in `ACTIVE` atomically publishes one complete output
  and advances the session cursor once.
- A recoverable validation, allocation, ordinary provider-error return, or
  injected nonfatal error rolls back provider temporaries completely, publishes
  no output, leaves every input usable, and leaves the cursor, public-token
  census, total public-reference census, and provider-object census unchanged.
- An ACE assertion, abort, unhandled exception, signal, broken IPC channel, or
  nonzero worker exit changes the context to `POISONED` and returns
  `PROVIDER_TERMINATED` for the triggering call.
- On that fatal path the broker publishes no output, removes private temporary
  files, destroys all worker/provider objects so the provider-object census is
  zero, and marks the context and every live child public token `POISONED`.
  Public tokens and their reference counts are deliberately retained so the
  client can release them; the cursor does not advance. This is not a rollback
  to the pre-call provider-object census.
- Later calls through a recognized poisoned context or child return
  `CONTEXT_POISONED` without provider dispatch, except release/destroy and the
  diagnostic query. Release discards a poisoned public token/reference.
  Context destroy reaps the worker after child releases, transitions to
  `CLOSED`, and permits creation of an unrelated fresh context from the
  original trusted envelopes.
- After `CLOSED`, copied or stale handles return `INVALID_HANDLE`. No context
  is automatically recreated because silent replay could duplicate work or
  violate the frozen schedule.

Patching ACE to return errors instead of asserting is optional. Any such patch
changes the accepted source tree and therefore requires its own review, source
hash, build hash, license check, and immutable provider pin.

## Mock Provider Certification

The SYNC-5 mock implements every function required by
`open64-fhe-resnet20-ckks-v1`; a primitive-only or ReLU-only mock is
insufficient. It validates the same envelope, descriptor, identity, ownership,
alias, state, and cleanup rules as the ACE adapter.

Its host shim also implements capability acquire/release, atomic one-shot
claim, all pre-claim versus post-claim failure points, concurrent `BUSY`, unique
broker identity/generation, non-exportable verifier retention, and broker-
destroy wiping. Host-only calls appear only in the lifecycle/failure
transcripts, never the evaluation census.

The mock ciphertext value is deterministic metadata, not cryptography. Each
successful operation computes a trace digest from the operation kind and
ordinal, descriptor digest, ordered input trace digests, and ordered plaintext
asset digests. Import and export preserve the versioned envelope. This makes
the complete call graph and call order independently checkable without ACE.

Failure injection selects an expanded semantic event and one stable status.
Tests cover failure before allocation, after private allocation but before
commit, ordinary provider-error rollback, and simulated worker termination.
Every failure returns a null output and publishes no partial envelope.
Recoverable tests require all three censuses and the cursor to match their
pre-call values. Fatal tests require an unchanged cursor and public token/ref
censuses immediately after failure, zero provider objects, `POISONED` child
tokens, deterministic release, and then `CLOSED`. End-of-run tests require zero
unreleased owning tokens, deterministic reverse cleanup, double-destroy and
use-after-destroy statuses, concurrent-call serialization, and successful
destroy/recreate after a poisoned worker. `failure-transcript-v1.json` records
these three separate census dimensions; it never calls them one live-handle
census.

SYNC-5 passes only when generated C calls the complete surface in the frozen
order, all deterministic and injected-failure tests pass, and the retained
header, manifests, generated C, executable, logs, and hashes come from the same
exact candidate.
