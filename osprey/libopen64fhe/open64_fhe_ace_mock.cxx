/*
 * Copyright (C) 2026 Open64 Project
 *
 * Dependency-free test double for pinned ACE result-first arithmetic calls.
 * Design: doc/FHE-ACE-RTLIB-RUNTIME-DECISION.md and
 * doc/FHE-SYNC6-CKKS-IR-CONFORMANCE-GATE.md. Tags are test tokens, not CKKS.
 */

#include "open64_fhe_ace_mock_test.h"

#include <new>

struct open64_fhe_ace_mock_cipher_s {
  uint32_t level;
  uint32_t scale_degree;
  uint32_t slots;
  int64_t tag;
};

static uint32_t mock_calls[OPEN64_FHE_ACE_MOCK_BOOTSTRAP + 1];
static uint32_t mock_live_count;
static uint32_t mock_fail_next;

/* Record one ACE-shaped call, including an injected failure before mutation. */
static bool
Mock_Begin(uint32_t call)
{
  ++mock_calls[call];
  if (mock_fail_next == call) {
    mock_fail_next = 0;
    return false;
  }
  return true;
}

/* Allocate the result object expected by ACE's result-first convention. */
extern "C" CIPHER
Alloc_ciphertext(void)
{
  CIPHER cipher = new (std::nothrow) CIPHERTEXT();
  if (cipher != NULL)
    ++mock_live_count;
  return cipher;
}

/* Release only the owned result object; borrowed inputs remain untouched. */
extern "C" void
Free_cipher(CIPHER cipher)
{
  if (cipher != NULL) {
    --mock_live_count;
    delete cipher;
  }
}

/* Report the mock's remaining-level token through ACE's query spelling. */
extern "C" size_t
Level(CIPHER cipher)
{
  return cipher == NULL ? 0 : cipher->level;
}

/* Report scale degree, not the Open64 logical scale-bit contract. */
extern "C" uint32_t
Sc_degree(CIPHER cipher)
{
  return cipher == NULL ? 0 : cipher->scale_degree;
}

/* Report active slots without opening the mock's private representation. */
extern "C" uint32_t
Get_slots(CIPHER cipher)
{
  return cipher == NULL ? 0 : cipher->slots;
}

/* Copy compatible input state to a distinct result for additive operations. */
static bool
Mock_Additive_State(CIPHER result, CIPHER left, CIPHER right)
{
  if (result == NULL || left == NULL || right == NULL ||
      result == left || result == right ||
      left->slots != right->slots ||
      left->scale_degree != right->scale_degree)
    return false;
  result->level = left->level < right->level ? left->level : right->level;
  result->scale_degree = left->scale_degree;
  result->slots = left->slots;
  return true;
}

/* Mirror ACE Add_ciph's result-first, borrowed-input call contract. */
extern "C" CIPHER
Add_ciph(CIPHER result, CIPHER left, CIPHER right)
{
  if (!Mock_Begin(OPEN64_FHE_ACE_MOCK_ADD) ||
      !Mock_Additive_State(result, left, right))
    return NULL;
  result->tag = left->tag + right->tag;
  return result;
}

/* Mirror ACE Sub_ciph for the explicit Chebyshev subtraction step. */
extern "C" CIPHER
Sub_ciph(CIPHER result, CIPHER left, CIPHER right)
{
  if (!Mock_Begin(OPEN64_FHE_ACE_MOCK_SUB) ||
      !Mock_Additive_State(result, left, right))
    return NULL;
  result->tag = left->tag - right->tag;
  return result;
}

/* Model a CKKS multiply's increased scale degree without hidden rescale. */
extern "C" CIPHER
Mul_ciph(CIPHER result, CIPHER left, CIPHER right)
{
  if (!Mock_Begin(OPEN64_FHE_ACE_MOCK_MUL) || result == NULL ||
      left == NULL || right == NULL || result == left || result == right ||
      left->slots != right->slots || left->level == 0 || right->level == 0)
    return NULL;
  result->level = left->level < right->level ? left->level : right->level;
  result->scale_degree = left->scale_degree + right->scale_degree;
  result->slots = left->slots;
  result->tag = left->tag * right->tag;
  return result;
}

/* Consume one mock level as a visible, separate rescale operation. */
extern "C" CIPHER
Rescale_ciph(CIPHER result, CIPHER input)
{
  if (!Mock_Begin(OPEN64_FHE_ACE_MOCK_RESCALE) || result == NULL ||
      input == NULL || result == input || input->level < 2 ||
      input->scale_degree == 0)
    return NULL;
  result->level = input->level - 1;
  result->scale_degree = input->scale_degree - 1;
  result->slots = input->slots;
  result->tag = input->tag;
  return result;
}

/* Preserve state and record the signed rotation in a diagnostic tag. */
extern "C" CIPHER
Rotate_ciph(CIPHER result, CIPHER input, int32_t rotation)
{
  if (!Mock_Begin(OPEN64_FHE_ACE_MOCK_ROTATE) || result == NULL ||
      input == NULL || result == input || rotation == 0)
    return NULL;
  *result = *input;
  result->tag = input->tag * 100 + rotation;
  return result;
}

/* Model the explicit target-level post-refresh contract, not decryption. */
extern "C" CIPHER
Bootstrap(CIPHER result, CIPHER input, uint32_t target_level)
{
  if (!Mock_Begin(OPEN64_FHE_ACE_MOCK_BOOTSTRAP) || result == NULL ||
      input == NULL || result == input || input->level == 0 ||
      target_level == 0)
    return NULL;
  *result = *input;
  result->level = target_level;
  return result;
}

/* Create a test input without invoking ACE's secret-key dataset helpers. */
extern "C" CIPHER
Open64_FHE_ACE_Mock_Cipher(uint32_t level, uint32_t scale_degree,
                            uint32_t slots, int64_t tag)
{
  if (level == 0 || slots == 0)
    return NULL;
  CIPHER cipher = Alloc_ciphertext();
  if (cipher != NULL) {
    cipher->level = level;
    cipher->scale_degree = scale_degree;
    cipher->slots = slots;
    cipher->tag = tag;
  }
  return cipher;
}

/* Reveal only the diagnostic token to focused adapter tests. */
extern "C" int64_t
Open64_FHE_ACE_Mock_Tag(CIPHER cipher)
{
  return cipher == NULL ? 0 : cipher->tag;
}

/* Count calls to the exact ACE-shaped entry point selected by the adapter. */
extern "C" uint32_t
Open64_FHE_ACE_Mock_Call_Count(uint32_t call)
{
  return call <= OPEN64_FHE_ACE_MOCK_BOOTSTRAP ? mock_calls[call] : 0;
}

/* Detect leaked result objects in success and failure tests. */
extern "C" uint32_t
Open64_FHE_ACE_Mock_Live_Count(void)
{
  return mock_live_count;
}

/* Inject one provider failure before the result or inputs are modified. */
extern "C" void
Open64_FHE_ACE_Mock_Fail_Next(uint32_t call)
{
  mock_fail_next = call;
}

/* Reset call evidence only when all test-owned ciphertexts are released. */
extern "C" void
Open64_FHE_ACE_Mock_Reset(void)
{
  if (mock_live_count != 0)
    return;
  for (uint32_t i = 0; i <= OPEN64_FHE_ACE_MOCK_BOOTSTRAP; ++i)
    mock_calls[i] = 0;
  mock_fail_next = 0;
}
