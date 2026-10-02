/*
 * Copyright (C) 2026 Open64 Project
 *
 * Bridge Open64 ownership/status rules to pinned ACE result-first operations.
 * Design: doc/FHE-ACE-RTLIB-RUNTIME-DECISION.md and
 * doc/FHE-SYNC6-CKKS-IR-CONFORMANCE-GATE.md. Fatal ACE assertions require
 * the later supervised worker; this unit does not claim process isolation.
 */

#include "open64_fhe_ace_eval_adapter.h"

/* Dispatch one explicit CKKS step without borrowing the output object. */
static CIPHER
Open64_FHE_ACE_Call(OPEN64_FHE_ACE_EVAL_OPERATION operation,
                    CIPHER result, CIPHER left, CIPHER right,
                    int32_t parameter)
{
  switch (operation) {
  case OPEN64_FHE_ACE_EVAL_ADD:
    return Add_ciph(result, left, right);
  case OPEN64_FHE_ACE_EVAL_SUB:
    return Sub_ciph(result, left, right);
  case OPEN64_FHE_ACE_EVAL_MUL:
    return Mul_ciph(result, left, right);
  case OPEN64_FHE_ACE_EVAL_RESCALE:
    return Rescale_ciph(result, left);
  case OPEN64_FHE_ACE_EVAL_ROTATE:
    return Rotate_ciph(result, left, parameter);
  case OPEN64_FHE_ACE_EVAL_BOOTSTRAP:
    return Bootstrap(result, left, uint32_t(parameter));
  }
  return NULL;
}

/* Keep borrowed inputs and caller output unchanged on a rejected operation. */
open64_fhe_status_v1
Open64_FHE_ACE_Evaluate(OPEN64_FHE_ACE_EVAL_OPERATION operation,
                        CIPHER left, CIPHER right, int32_t parameter,
                        CIPHER *out_result)
{
  if (out_result == NULL || left == NULL)
    return OPEN64_FHE_STATUS_INVALID_ARGUMENT;
  if (*out_result != NULL)
    return *out_result == left || *out_result == right
        ? OPEN64_FHE_STATUS_ALIAS_FORBIDDEN
        : OPEN64_FHE_STATUS_INVALID_ARGUMENT;
  const bool binary = operation == OPEN64_FHE_ACE_EVAL_ADD ||
                      operation == OPEN64_FHE_ACE_EVAL_SUB ||
                      operation == OPEN64_FHE_ACE_EVAL_MUL;
  if ((binary && right == NULL) || (!binary && right != NULL) ||
      (operation != OPEN64_FHE_ACE_EVAL_ROTATE &&
       operation != OPEN64_FHE_ACE_EVAL_BOOTSTRAP &&
       operation != OPEN64_FHE_ACE_EVAL_RESCALE && !binary) ||
      (operation == OPEN64_FHE_ACE_EVAL_ROTATE && parameter == 0) ||
      (operation == OPEN64_FHE_ACE_EVAL_BOOTSTRAP && parameter <= 0) ||
      Level(left) == 0 || Get_slots(left) == 0 ||
      (binary && (Level(right) == 0 ||
                  Get_slots(left) != Get_slots(right))))
    return OPEN64_FHE_STATUS_INVALID_ARGUMENT;
  if ((operation == OPEN64_FHE_ACE_EVAL_ADD ||
       operation == OPEN64_FHE_ACE_EVAL_SUB) &&
      Sc_degree(left) != Sc_degree(right))
    return OPEN64_FHE_STATUS_MODEL_REQUIREMENT_MISMATCH;

  const size_t left_level = Level(left);
  const uint32_t left_scale = Sc_degree(left);
  const uint32_t left_slots = Get_slots(left);
  const size_t right_level = binary ? Level(right) : 0;
  const uint32_t right_scale = binary ? Sc_degree(right) : 0;
  const uint32_t right_slots = binary ? Get_slots(right) : 0;

  CIPHER result = Alloc_ciphertext();
  if (result == NULL)
    return OPEN64_FHE_STATUS_OUT_OF_MEMORY;
  CIPHER returned = Open64_FHE_ACE_Call(
      operation, result, left, right, parameter);
  if (returned != result || Level(left) != left_level ||
      Sc_degree(left) != left_scale || Get_slots(left) != left_slots ||
      (binary && (Level(right) != right_level ||
                  Sc_degree(right) != right_scale ||
                  Get_slots(right) != right_slots)) ||
      Get_slots(result) != left_slots ||
      (operation == OPEN64_FHE_ACE_EVAL_BOOTSTRAP &&
       Level(result) != uint32_t(parameter))) {
    Free_cipher(result);
    return OPEN64_FHE_STATUS_INTERNAL_ERROR;
  }
  *out_result = result;
  return OPEN64_FHE_STATUS_OK;
}

/* Release only the result owned by this adapter and clear the caller handle. */
void
Open64_FHE_ACE_Release(CIPHER *value)
{
  if (value != NULL && *value != NULL) {
    Free_cipher(*value);
    *value = NULL;
  }
}
