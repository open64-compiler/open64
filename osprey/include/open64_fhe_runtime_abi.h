/*
 * Copyright (C) 2026 Open64 Project
 *
 * Stable public C ABI for Open64 FHE execution providers.
 */

#ifndef open64_fhe_runtime_abi_INCLUDED
#define open64_fhe_runtime_abi_INCLUDED

#include <stddef.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

#define OPEN64_FHE_ABI_VERSION_V1 UINT32_C(1)

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
static_assert(sizeof(open64_fhe_context_desc_v1) == 144,
              "context desc layout");
static_assert(sizeof(open64_fhe_import_binding_v1) == 232,
              "import binding layout");
static_assert(sizeof(open64_fhe_export_binding_v1) == 232,
              "export binding layout");
static_assert(sizeof(open64_fhe_export_receipt_v1) == 104,
              "export receipt layout");
static_assert(sizeof(open64_fhe_operation_desc_v1) == 496,
              "operation desc layout");
static_assert(sizeof(open64_fhe_ciphertext_info_v1) == 96,
              "ciphertext info layout");
static_assert(offsetof(open64_fhe_operation_desc_v1, payload) == 480,
              "operation desc field offset");
#else
_Static_assert(sizeof(open64_fhe_status_v1) == 4, "ABI status width");
_Static_assert(sizeof(void *) == 8, "ABI v1 requires 64-bit pointers");
_Static_assert(sizeof(open64_fhe_broker_desc_v1) == 16,
               "broker desc layout");
_Static_assert(sizeof(open64_fhe_context_desc_v1) == 144,
               "context desc layout");
_Static_assert(sizeof(open64_fhe_import_binding_v1) == 232,
               "import binding layout");
_Static_assert(sizeof(open64_fhe_export_binding_v1) == 232,
               "export binding layout");
_Static_assert(sizeof(open64_fhe_export_receipt_v1) == 104,
               "export receipt layout");
_Static_assert(sizeof(open64_fhe_operation_desc_v1) == 496,
               "operation desc layout");
_Static_assert(sizeof(open64_fhe_ciphertext_info_v1) == 96,
               "ciphertext info layout");
_Static_assert(offsetof(open64_fhe_operation_desc_v1, payload) == 480,
               "operation desc field offset");
#endif

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

open64_fhe_status_v1 open64_fhe_operation_desc_select_v1(
    open64_fhe_model_v1_t model,
    open64_fhe_ciphertext_v1_t anchor,
    uint32_t static_ordinal,
    uint32_t operation_kind,
    const open64_fhe_operation_desc_v1 **out_desc);

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

#endif /* open64_fhe_runtime_abi_INCLUDED */
