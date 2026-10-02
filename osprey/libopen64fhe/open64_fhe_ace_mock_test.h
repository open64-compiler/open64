/*
 * Copyright (C) 2026 Open64 Project
 *
 * Test-only construction and observation of the ACE-shaped arithmetic mock.
 * Design: doc/FHE-ACE-RTLIB-RUNTIME-DECISION.md.
 */

#ifndef open64_fhe_ace_mock_test_INCLUDED
#define open64_fhe_ace_mock_test_INCLUDED

#include "open64_fhe_ace_api.h"

#ifdef __cplusplus
extern "C" {
#endif

enum OPEN64_FHE_ACE_MOCK_CALL {
  OPEN64_FHE_ACE_MOCK_ADD = 1,
  OPEN64_FHE_ACE_MOCK_SUB = 2,
  OPEN64_FHE_ACE_MOCK_MUL = 3,
  OPEN64_FHE_ACE_MOCK_RESCALE = 4,
  OPEN64_FHE_ACE_MOCK_ROTATE = 5,
  OPEN64_FHE_ACE_MOCK_BOOTSTRAP = 6
};

CIPHER Open64_FHE_ACE_Mock_Cipher(uint32_t level, uint32_t scale_degree,
                                  uint32_t slots, int64_t tag);
int64_t Open64_FHE_ACE_Mock_Tag(CIPHER cipher);
uint32_t Open64_FHE_ACE_Mock_Call_Count(uint32_t call);
uint32_t Open64_FHE_ACE_Mock_Live_Count(void);
void Open64_FHE_ACE_Mock_Fail_Next(uint32_t call);
void Open64_FHE_ACE_Mock_Reset(void);

#ifdef __cplusplus
}
#endif

#endif /* open64_fhe_ace_mock_test_INCLUDED */
