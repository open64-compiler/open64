/*
 * Execute unchanged whirl2c output to record the real ResNet operation order.
 * See doc/FHE-SYNC5-NATIVE-OWNERSHIP-AUDIT.md, S5-H. This test provider does
 * not implement encryption or replace the standalone mock runtime.
 */

#include "open64_fhe_runtime_abi.h"
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#include "secure_resnet20.mid.w2c.c"

static uint32_t trace_select_count;
static uint32_t trace_eval_count;
static uint32_t trace_fail_select;
static uint32_t trace_fail_eval;
static open64_fhe_operation_desc_v1 trace_descriptor;

/* Return one distinguishable non-null opaque output for each successful call. */
static open64_fhe_status_v1
Trace_Evaluate(open64_fhe_ciphertext_v1_t input0,
               open64_fhe_ciphertext_v1_t input1,
               open64_fhe_ciphertext_v1_t *out_result)
{
  ++trace_eval_count;
  if (trace_eval_count == trace_fail_eval)
    return OPEN64_FHE_STATUS_UNSUPPORTED;
  *out_result = (open64_fhe_ciphertext_v1_t)
      (uintptr_t)(0x1000U + trace_eval_count * 16U);
  printf("EVAL %u %lu %lu %lu\n", trace_eval_count,
         (unsigned long)(uintptr_t)input0,
         (unsigned long)(uintptr_t)input1,
         (unsigned long)(uintptr_t)*out_result);
  return OPEN64_FHE_STATUS_OK;
}

/* Emit the selected ordinal and kind in actual interprocedural execution order. */
open64_fhe_status_v1
open64_fhe_operation_desc_select_v1(open64_fhe_model_v1_t model,
                                    open64_fhe_ciphertext_v1_t anchor,
                                    uint32_t static_ordinal,
                                    uint32_t operation_kind,
                                    const open64_fhe_operation_desc_v1 **out_desc)
{
  (void)model;
  (void)anchor;
  ++trace_select_count;
  printf("SELECT %u %u %u %lu\n", trace_select_count, static_ordinal,
         operation_kind, (unsigned long)(uintptr_t)anchor);
  if (trace_select_count == trace_fail_select)
    return OPEN64_FHE_STATUS_UNSUPPORTED;
  trace_descriptor.operation_kind = operation_kind;
  trace_descriptor.operation_ordinal = static_ordinal;
  *out_desc = &trace_descriptor;
  return OPEN64_FHE_STATUS_OK;
}

/* The remaining stubs exercise generated argument and status paths only. */
open64_fhe_status_v1
open64_fhe_conv2d_plain_v1(open64_fhe_model_v1_t model,
                           open64_fhe_ciphertext_v1_t input,
                           open64_fhe_plain_tensor_v1_t weight,
                           open64_fhe_plain_tensor_v1_t bias,
                           const open64_fhe_operation_desc_v1 *desc,
                           open64_fhe_ciphertext_v1_t *out_result)
{
  (void)model; (void)input; (void)weight; (void)bias; (void)desc;
  return Trace_Evaluate(input, NULL, out_result);
}

/* Trace the two-input residual operation. */
open64_fhe_status_v1
open64_fhe_residual_add_v1(open64_fhe_model_v1_t model,
                           open64_fhe_ciphertext_v1_t main_path,
                           open64_fhe_ciphertext_v1_t shortcut_path,
                           const open64_fhe_operation_desc_v1 *desc,
                           open64_fhe_ciphertext_v1_t *out_result)
{
  (void)model; (void)main_path; (void)shortcut_path; (void)desc;
  return Trace_Evaluate(main_path, shortcut_path, out_result);
}

/* Generate the identical trace behavior for each unary runtime operation. */
#define TRACE_UNARY(name) \
  open64_fhe_status_v1 name(open64_fhe_model_v1_t model, \
                            open64_fhe_ciphertext_v1_t input, \
                            const open64_fhe_operation_desc_v1 *desc, \
                            open64_fhe_ciphertext_v1_t *out_result) \
  { \
    (void)model; (void)input; (void)desc; \
    return Trace_Evaluate(input, NULL, out_result); \
  }

TRACE_UNARY(open64_fhe_bootstrap_v1)
TRACE_UNARY(open64_fhe_relu_normalize_v1)
TRACE_UNARY(open64_fhe_average_pool_v1)
TRACE_UNARY(open64_fhe_layout_convert_v1)

/* Trace one polynomial stage while retaining its coefficient operand. */
open64_fhe_status_v1
open64_fhe_relu_poly_stage_v1(open64_fhe_model_v1_t model,
                              open64_fhe_ciphertext_v1_t input,
                              open64_fhe_plain_tensor_v1_t coefficients,
                              const open64_fhe_operation_desc_v1 *desc,
                              open64_fhe_ciphertext_v1_t *out_result)
{
  (void)model; (void)input; (void)coefficients; (void)desc;
  return Trace_Evaluate(input, NULL, out_result);
}

/* Trace reconstruction, including its preserved refreshed-input argument. */
open64_fhe_status_v1
open64_fhe_relu_reconstruct_v1(open64_fhe_model_v1_t model,
                               open64_fhe_ciphertext_v1_t refreshed_input,
                               open64_fhe_ciphertext_v1_t stage2_result,
                               const open64_fhe_operation_desc_v1 *desc,
                               open64_fhe_ciphertext_v1_t *out_result)
{
  (void)model; (void)refreshed_input; (void)stage2_result; (void)desc;
  return Trace_Evaluate(refreshed_input, stage2_result, out_result);
}

/* Trace the final plaintext-weighted linear operation. */
open64_fhe_status_v1
open64_fhe_linear_plain_v1(open64_fhe_model_v1_t model,
                           open64_fhe_ciphertext_v1_t input,
                           open64_fhe_plain_tensor_v1_t weight,
                           open64_fhe_plain_tensor_v1_t bias,
                           const open64_fhe_operation_desc_v1 *desc,
                           open64_fhe_ciphertext_v1_t *out_result)
{
  (void)model; (void)input; (void)weight; (void)bias; (void)desc;
  return Trace_Evaluate(input, NULL, out_result);
}

#define TRACE_P4 p, p, p, p
#define TRACE_P6 TRACE_P4, p, p

/* Execute the real six-PU generated program with non-dereferenced handles. */
int
main(int argc, char **argv)
{
  open64_fhe_ciphertext_v1_t input =
      (open64_fhe_ciphertext_v1_t)(uintptr_t)0x8000U;
  open64_fhe_ciphertext_v1_t output = 0;
  open64_fhe_plain_tensor_v1_t p =
      (open64_fhe_plain_tensor_v1_t)(uintptr_t)0x9000U;
  open64_fhe_model_v1_t model =
      (open64_fhe_model_v1_t)(uintptr_t)0xa000U;

  if (argc == 3 && argv[1][0] == 's')
    trace_fail_select = (uint32_t)strtoul(argv[2], 0, 10);
  else if (argc == 3 && argv[1][0] == 'e')
    trace_fail_eval = (uint32_t)strtoul(argv[2], 0, 10);
  else if (argc != 1)
    return 2;

  SecureResNet20(input, &output,
                 TRACE_P4, TRACE_P4, TRACE_P4, TRACE_P6, TRACE_P4,
                 TRACE_P4, TRACE_P6, TRACE_P4, TRACE_P4,
                 p, p, p, p, model, p, p, p);
  printf("SUMMARY selects=%u evals=%u output=%s\n", trace_select_count,
         trace_eval_count, output ? "set" : "null");
  if (trace_fail_select || trace_fail_eval)
    return output == 0 ? 0 : 1;
  return trace_select_count == 147 && trace_eval_count == 147 && output != 0
      ? 0 : 1;
}
