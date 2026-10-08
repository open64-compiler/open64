/*
 * Copyright (C) 2026 Open64 Project
 *
 * Compile/link-only CKKS2C facade stub.  This file checks generated-C linkage;
 * it is not a runtime mock or ciphertext correctness test.  See
 * doc/FHE-SYNC6-S6-0D-DETAILED-EXECUTION-PLAN.md, D1.
 */

#include "open64_fhe_ckks2c_facade.h"

struct open64_fhe_ckks2c_execution_v1_s { int unused; };
struct open64_fhe_ckks2c_value_v1_s { int unused; };
static struct open64_fhe_ckks2c_value_v1_s value_storage;

#define RETURN_VALUE(out_value) \
  do { *(out_value) = &value_storage; return OPEN64_FHE_CKKS2C_STATUS_OK_V1; } while (0)

open64_fhe_ckks2c_status_v1
open64_fhe_ckks2c_source_value_v1(open64_fhe_ckks2c_execution_v1 execution,
                                  uint32_t source_value_id,
                                  open64_fhe_ckks2c_value_v1 *out_value)
{ (void)execution; (void)source_value_id; RETURN_VALUE(out_value); }

open64_fhe_ckks2c_status_v1
open64_fhe_ckks2c_bound_value_v1(open64_fhe_ckks2c_execution_v1 execution,
                                 const char *role,
                                 open64_fhe_ckks2c_value_v1 *out_value)
{ (void)execution; (void)role; RETURN_VALUE(out_value); }

open64_fhe_ckks2c_status_v1
open64_fhe_ckks2c_encode_asset_v1(open64_fhe_ckks2c_execution_v1 execution,
                                  uint32_t source_value_id,
                                  const char *sha256,
                                  const open64_fhe_ckks2c_state_v1 *state,
                                  open64_fhe_ckks2c_value_v1 *out_value)
{ (void)execution; (void)source_value_id; (void)sha256; (void)state; RETURN_VALUE(out_value); }

#define BINARY(name) \
  open64_fhe_ckks2c_status_v1 name( \
      open64_fhe_ckks2c_execution_v1 execution, \
      open64_fhe_ckks2c_value_v1 left, open64_fhe_ckks2c_value_v1 right, \
      const open64_fhe_ckks2c_state_v1 *state, \
      open64_fhe_ckks2c_value_v1 *out_value) \
  { (void)execution; (void)left; (void)right; (void)state; RETURN_VALUE(out_value); }
BINARY(open64_fhe_ckks2c_add_v1)
BINARY(open64_fhe_ckks2c_sub_v1)
BINARY(open64_fhe_ckks2c_mul_v1)

open64_fhe_ckks2c_status_v1
open64_fhe_ckks2c_rotate_v1(open64_fhe_ckks2c_execution_v1 execution,
                            open64_fhe_ckks2c_value_v1 input,
                            int32_t steps, const char *key,
                            const open64_fhe_ckks2c_state_v1 *state,
                            open64_fhe_ckks2c_value_v1 *out_value)
{ (void)execution; (void)input; (void)steps; (void)key; (void)state; RETURN_VALUE(out_value); }

open64_fhe_ckks2c_status_v1
open64_fhe_ckks2c_rescale_v1(open64_fhe_ckks2c_execution_v1 execution,
                             open64_fhe_ckks2c_value_v1 input,
                             uint32_t levels, int32_t scale,
                             const open64_fhe_ckks2c_state_v1 *state,
                             open64_fhe_ckks2c_value_v1 *out_value)
{ (void)execution; (void)input; (void)levels; (void)scale; (void)state; RETURN_VALUE(out_value); }

open64_fhe_ckks2c_status_v1
open64_fhe_ckks2c_modswitch_v1(open64_fhe_ckks2c_execution_v1 execution,
                               open64_fhe_ckks2c_value_v1 input,
                               int32_t level,
                               const open64_fhe_ckks2c_state_v1 *state,
                               open64_fhe_ckks2c_value_v1 *out_value)
{ (void)execution; (void)input; (void)level; (void)state; RETURN_VALUE(out_value); }

open64_fhe_ckks2c_status_v1
open64_fhe_ckks2c_relin_v1(open64_fhe_ckks2c_execution_v1 execution,
                           open64_fhe_ckks2c_value_v1 input,
                           const char *key,
                           const open64_fhe_ckks2c_state_v1 *state,
                           open64_fhe_ckks2c_value_v1 *out_value)
{ (void)execution; (void)input; (void)key; (void)state; RETURN_VALUE(out_value); }

open64_fhe_ckks2c_status_v1
open64_fhe_ckks2c_bootstrap_v1(open64_fhe_ckks2c_execution_v1 execution,
                               open64_fhe_ckks2c_value_v1 input,
                               int32_t level, const char *reason,
                               const char *key,
                               const open64_fhe_ckks2c_state_v1 *state,
                               open64_fhe_ckks2c_value_v1 *out_value)
{ (void)execution; (void)input; (void)level; (void)reason; (void)key; (void)state; RETURN_VALUE(out_value); }

void
open64_fhe_ckks2c_value_release_v1(open64_fhe_ckks2c_execution_v1 execution,
                                   open64_fhe_ckks2c_value_v1 value)
{ (void)execution; (void)value; }

extern open64_fhe_ckks2c_status_v1 open64_fhe_eval_fixture_group(
    open64_fhe_ckks2c_execution_v1 execution,
    open64_fhe_ckks2c_value_v1 *out_value);

int main(void)
{
  open64_fhe_ckks2c_value_v1 output = 0;
  return (int)open64_fhe_eval_fixture_group(0, &output);
}
