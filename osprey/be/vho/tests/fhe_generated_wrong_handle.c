/*
 * Compile-negative proof that the public FHE ABI distinguishes ciphertext
 * and plaintext tensor handles. See doc/FHE-SYNC5-NATIVE-OWNERSHIP-AUDIT.md,
 * S5-H, and doc/FHE-RUNTIME-C-ABI-V1-CONTRACT.md.
 */

#include "open64_fhe_runtime_abi.h"

/* This invalid weight argument must fail strict C pointer-type checking. */
open64_fhe_status_v1
FHE_Generated_Wrong_Handle(open64_fhe_model_v1_t model,
                           open64_fhe_ciphertext_v1_t input,
                           open64_fhe_plain_tensor_v1_t bias,
                           const open64_fhe_operation_desc_v1 *desc,
                           open64_fhe_ciphertext_v1_t *output)
{
  return open64_fhe_conv2d_plain_v1(model, input, input, bias, desc, output);
}
