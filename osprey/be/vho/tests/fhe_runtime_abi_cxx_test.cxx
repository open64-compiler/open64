#include "open64_fhe_runtime_abi.h"

#include <type_traits>

static_assert(std::is_standard_layout<open64_fhe_operation_desc_v1>::value,
              "operation descriptor must be standard layout");
static_assert(OPEN64_FHE_STATUS_TRUST_DOMAIN_MISMATCH == 26,
              "status values are append-only");
static_assert(OPEN64_FHE_OP_LINEAR_PLAIN == 9,
              "operation kinds are append-only");
static_assert(OPEN64_FHE_ENVELOPE_HEADER_SIZE_V1 == 160,
              "envelope header size is frozen");
static_assert(OPEN64_FHE_KEY_CLASS_SECRET == UINT32_C(0x80000000),
              "secret-key class bit is frozen");

typedef open64_fhe_status_v1 (*conv2d_signature)(
    open64_fhe_model_v1_t,
    open64_fhe_ciphertext_v1_t,
    open64_fhe_plain_tensor_v1_t,
    open64_fhe_plain_tensor_v1_t,
    const open64_fhe_operation_desc_v1 *,
    open64_fhe_ciphertext_v1_t *);
typedef open64_fhe_status_v1 (*reconstruct_signature)(
    open64_fhe_model_v1_t,
    open64_fhe_ciphertext_v1_t,
    open64_fhe_ciphertext_v1_t,
    const open64_fhe_operation_desc_v1 *,
    open64_fhe_ciphertext_v1_t *);
typedef open64_fhe_status_v1 (*export_signature)(
    open64_fhe_model_v1_t,
    open64_fhe_ciphertext_v1_t,
    const open64_fhe_export_binding_v1 *,
    void *,
    uint64_t,
    uint64_t *,
    open64_fhe_export_receipt_v1 *);

static_assert(std::is_same<decltype(&open64_fhe_conv2d_plain_v1),
                           conv2d_signature>::value,
              "conv2d signature drifted");
static_assert(std::is_same<decltype(&open64_fhe_relu_reconstruct_v1),
                           reconstruct_signature>::value,
              "ReLU reconstruction signature drifted");
static_assert(std::is_same<decltype(&open64_fhe_ciphertext_export_v1),
                           export_signature>::value,
              "ciphertext export signature drifted");

int
main()
{
  open64_fhe_ciphertext_info_v1 info = {};
  info.abi_version = OPEN64_FHE_ABI_VERSION_V1;
  info.struct_size = sizeof(info);
  return info.struct_size == 96 ? 0 : 1;
}
