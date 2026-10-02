/*
 * Copyright (C) 2026 Open64 Project
 *
 * Certify the Open64 ownership adapter against ACE-named mock operations.
 * Design: doc/FHE-ACE-RTLIB-RUNTIME-DECISION.md and
 * doc/FHE-SYNC6-CKKS-IR-CONFORMANCE-GATE.md.
 */

#include "open64_fhe_ace_eval_adapter.h"
#include "open64_fhe_ace_mock_test.h"

#include <assert.h>
#include <type_traits>

static_assert(std::is_same<decltype(&Add_ciph),
                           CIPHER (*)(CIPHER, CIPHER, CIPHER)>::value,
              "ACE add call shape changed");
static_assert(std::is_same<decltype(&Rotate_ciph),
                           CIPHER (*)(CIPHER, CIPHER, int32_t)>::value,
              "ACE rotate call shape changed");
static_assert(std::is_same<decltype(&Bootstrap),
                           CIPHER (*)(CIPHER, CIPHER, uint32_t)>::value,
              "ACE bootstrap call shape changed");

/* Exercise result-first calls, borrowed inputs, failure rollback, and cleanup. */
int
main()
{
  Open64_FHE_ACE_Mock_Reset();
  CIPHER left = Open64_FHE_ACE_Mock_Cipher(3, 1, 8, 7);
  CIPHER right = Open64_FHE_ACE_Mock_Cipher(3, 1, 8, 2);
  assert(left != NULL && right != NULL);

  CIPHER alias = left;
  assert(Open64_FHE_ACE_Evaluate(OPEN64_FHE_ACE_EVAL_ADD, left, right, 0,
                                  &alias) == OPEN64_FHE_STATUS_ALIAS_FORBIDDEN);
  assert(alias == left);
  assert(Open64_FHE_ACE_Mock_Call_Count(OPEN64_FHE_ACE_MOCK_ADD) == 0);

  CIPHER sum = NULL;
  assert(Open64_FHE_ACE_Evaluate(OPEN64_FHE_ACE_EVAL_ADD, left, right, 0,
                                  &sum) == OPEN64_FHE_STATUS_OK);
  assert(sum != left && sum != right);
  assert(Open64_FHE_ACE_Mock_Tag(sum) == 9);
  assert(Level(sum) == 3 && Sc_degree(sum) == 1);

  CIPHER difference = NULL;
  assert(Open64_FHE_ACE_Evaluate(OPEN64_FHE_ACE_EVAL_SUB, left, right, 0,
                                  &difference) == OPEN64_FHE_STATUS_OK);
  assert(Open64_FHE_ACE_Mock_Tag(difference) == 5);

  CIPHER product = NULL;
  assert(Open64_FHE_ACE_Evaluate(OPEN64_FHE_ACE_EVAL_MUL, left, right, 0,
                                  &product) == OPEN64_FHE_STATUS_OK);
  assert(Open64_FHE_ACE_Mock_Tag(product) == 14);
  assert(Sc_degree(product) == 2);

  CIPHER rescaled = NULL;
  assert(Open64_FHE_ACE_Evaluate(OPEN64_FHE_ACE_EVAL_RESCALE, product,
                                  NULL, 0, &rescaled) == OPEN64_FHE_STATUS_OK);
  assert(Level(rescaled) == 2 && Sc_degree(rescaled) == 1);
  assert(Level(product) == 3 && Sc_degree(product) == 2);

  CIPHER rotated = NULL;
  assert(Open64_FHE_ACE_Evaluate(OPEN64_FHE_ACE_EVAL_ROTATE, left, NULL,
                                  -3, &rotated) == OPEN64_FHE_STATUS_OK);
  assert(Open64_FHE_ACE_Mock_Tag(rotated) == 697);
  assert(Level(rotated) == 3 && Level(left) == 3);

  CIPHER refreshed = NULL;
  assert(Open64_FHE_ACE_Evaluate(OPEN64_FHE_ACE_EVAL_BOOTSTRAP, left,
                                  NULL, 17, &refreshed) == OPEN64_FHE_STATUS_OK);
  assert(Level(refreshed) == 17 && Level(left) == 3);

  uint32_t live_before = Open64_FHE_ACE_Mock_Live_Count();
  Open64_FHE_ACE_Mock_Fail_Next(OPEN64_FHE_ACE_MOCK_BOOTSTRAP);
  CIPHER rejected = NULL;
  assert(Open64_FHE_ACE_Evaluate(OPEN64_FHE_ACE_EVAL_BOOTSTRAP, left,
                                  NULL, 18, &rejected) ==
         OPEN64_FHE_STATUS_INTERNAL_ERROR);
  assert(rejected == NULL && Level(left) == 3);
  assert(Open64_FHE_ACE_Mock_Live_Count() == live_before);

  CIPHER wrong_scale = Open64_FHE_ACE_Mock_Cipher(3, 2, 8, 1);
  CIPHER no_result = NULL;
  uint32_t add_calls = Open64_FHE_ACE_Mock_Call_Count(OPEN64_FHE_ACE_MOCK_ADD);
  assert(Open64_FHE_ACE_Evaluate(OPEN64_FHE_ACE_EVAL_ADD, left,
                                  wrong_scale, 0, &no_result) ==
         OPEN64_FHE_STATUS_MODEL_REQUIREMENT_MISMATCH);
  assert(no_result == NULL);
  assert(Open64_FHE_ACE_Mock_Call_Count(OPEN64_FHE_ACE_MOCK_ADD) == add_calls);

  assert(Open64_FHE_ACE_Mock_Call_Count(OPEN64_FHE_ACE_MOCK_ADD) == 1);
  assert(Open64_FHE_ACE_Mock_Call_Count(OPEN64_FHE_ACE_MOCK_SUB) == 1);
  assert(Open64_FHE_ACE_Mock_Call_Count(OPEN64_FHE_ACE_MOCK_MUL) == 1);
  assert(Open64_FHE_ACE_Mock_Call_Count(OPEN64_FHE_ACE_MOCK_RESCALE) == 1);
  assert(Open64_FHE_ACE_Mock_Call_Count(OPEN64_FHE_ACE_MOCK_ROTATE) == 1);
  assert(Open64_FHE_ACE_Mock_Call_Count(OPEN64_FHE_ACE_MOCK_BOOTSTRAP) == 2);

  Open64_FHE_ACE_Release(&sum);
  Open64_FHE_ACE_Release(&difference);
  Open64_FHE_ACE_Release(&product);
  Open64_FHE_ACE_Release(&rescaled);
  Open64_FHE_ACE_Release(&rotated);
  Open64_FHE_ACE_Release(&refreshed);
  Free_cipher(wrong_scale);
  Free_cipher(right);
  Free_cipher(left);
  assert(Open64_FHE_ACE_Mock_Live_Count() == 0);
  Open64_FHE_ACE_Mock_Reset();
  return 0;
}
