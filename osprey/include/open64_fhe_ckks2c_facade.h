/*
 * Copyright (C) 2026 Open64 Project
 *
 * Provider-private CKKS2C facade consumed by generated C evaluators.  This is
 * not the public Open64 FHE application ABI.  A provider adapter implements
 * these calls over the selected runtime while keeping keys, assets, state,
 * and failures explicit.  See doc/FHE-SYNC6-ACE-CKKS-C-STAGING.md and
 * doc/FHE-SYNC6-S6-0D-DETAILED-EXECUTION-PLAN.md.
 */

#ifndef open64_fhe_ckks2c_facade_INCLUDED
#define open64_fhe_ckks2c_facade_INCLUDED

#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

#define OPEN64_FHE_CKKS2C_FACADE_VERSION_V1 UINT32_C(1)
#define OPEN64_FHE_CKKS2C_STATUS_OK_V1 UINT32_C(0)

typedef uint32_t open64_fhe_ckks2c_status_v1;

typedef struct open64_fhe_ckks2c_execution_v1_s
    *open64_fhe_ckks2c_execution_v1;
typedef struct open64_fhe_ckks2c_value_v1_s
    *open64_fhe_ckks2c_value_v1;

/* Expected result state is checked by the adapter after every primitive. */
typedef struct {
  uint32_t abi_version;
  uint32_t struct_size;
  uint32_t encryption_descriptor_id;
  uint32_t scheme;
  uint32_t value_class;
  int32_t level;
  int32_t scale_bits;
  int32_t component_count;
  int32_t precision_bits;
  uint32_t slot_count;
  uint32_t alignment_group;
  uint32_t pending_actions;
  uint32_t pending_bootstrap_reason;
  const char *encrypted_layout;
} open64_fhe_ckks2c_state_v1;

/* Source and bound lookups return borrowed handles owned by the execution. */
open64_fhe_ckks2c_status_v1 open64_fhe_ckks2c_source_value_v1(
    open64_fhe_ckks2c_execution_v1 execution, uint32_t source_value_id,
    open64_fhe_ckks2c_value_v1 *out_value);
open64_fhe_ckks2c_status_v1 open64_fhe_ckks2c_bound_value_v1(
    open64_fhe_ckks2c_execution_v1 execution, const char *semantic_role,
    open64_fhe_ckks2c_value_v1 *out_value);

/* Encode resolves an authenticated external asset; generated C performs no
 * file I/O and never embeds the plaintext bytes in its source. */
open64_fhe_ckks2c_status_v1 open64_fhe_ckks2c_encode_asset_v1(
    open64_fhe_ckks2c_execution_v1 execution, uint32_t source_value_id,
    const char *asset_sha256, const open64_fhe_ckks2c_state_v1 *expected,
    open64_fhe_ckks2c_value_v1 *out_value);

open64_fhe_ckks2c_status_v1 open64_fhe_ckks2c_add_v1(
    open64_fhe_ckks2c_execution_v1 execution,
    open64_fhe_ckks2c_value_v1 left, open64_fhe_ckks2c_value_v1 right,
    const open64_fhe_ckks2c_state_v1 *expected,
    open64_fhe_ckks2c_value_v1 *out_value);
open64_fhe_ckks2c_status_v1 open64_fhe_ckks2c_sub_v1(
    open64_fhe_ckks2c_execution_v1 execution,
    open64_fhe_ckks2c_value_v1 left, open64_fhe_ckks2c_value_v1 right,
    const open64_fhe_ckks2c_state_v1 *expected,
    open64_fhe_ckks2c_value_v1 *out_value);
open64_fhe_ckks2c_status_v1 open64_fhe_ckks2c_mul_v1(
    open64_fhe_ckks2c_execution_v1 execution,
    open64_fhe_ckks2c_value_v1 left, open64_fhe_ckks2c_value_v1 right,
    const open64_fhe_ckks2c_state_v1 *expected,
    open64_fhe_ckks2c_value_v1 *out_value);
open64_fhe_ckks2c_status_v1 open64_fhe_ckks2c_rotate_v1(
    open64_fhe_ckks2c_execution_v1 execution,
    open64_fhe_ckks2c_value_v1 input, int32_t signed_steps,
    const char *rotation_key, const open64_fhe_ckks2c_state_v1 *expected,
    open64_fhe_ckks2c_value_v1 *out_value);
open64_fhe_ckks2c_status_v1 open64_fhe_ckks2c_rescale_v1(
    open64_fhe_ckks2c_execution_v1 execution,
    open64_fhe_ckks2c_value_v1 input, uint32_t levels,
    int32_t target_scale_bits,
    const open64_fhe_ckks2c_state_v1 *expected,
    open64_fhe_ckks2c_value_v1 *out_value);
open64_fhe_ckks2c_status_v1 open64_fhe_ckks2c_modswitch_v1(
    open64_fhe_ckks2c_execution_v1 execution,
    open64_fhe_ckks2c_value_v1 input, int32_t target_level,
    const open64_fhe_ckks2c_state_v1 *expected,
    open64_fhe_ckks2c_value_v1 *out_value);
open64_fhe_ckks2c_status_v1 open64_fhe_ckks2c_relin_v1(
    open64_fhe_ckks2c_execution_v1 execution,
    open64_fhe_ckks2c_value_v1 input, const char *relinearization_key,
    const open64_fhe_ckks2c_state_v1 *expected,
    open64_fhe_ckks2c_value_v1 *out_value);
open64_fhe_ckks2c_status_v1 open64_fhe_ckks2c_bootstrap_v1(
    open64_fhe_ckks2c_execution_v1 execution,
    open64_fhe_ckks2c_value_v1 input, int32_t target_level,
    const char *reason, const char *bootstrap_key,
    const open64_fhe_ckks2c_state_v1 *expected,
    open64_fhe_ckks2c_value_v1 *out_value);

/* Primitive results are owned until transferred to the group output. */
void open64_fhe_ckks2c_value_release_v1(
    open64_fhe_ckks2c_execution_v1 execution,
    open64_fhe_ckks2c_value_v1 value);

#ifdef __cplusplus
}
#endif

#endif /* open64_fhe_ckks2c_facade_INCLUDED */
