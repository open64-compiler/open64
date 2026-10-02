/*
 * Copyright (C) 2026 Open64 Project
 *
 * Private ACE arithmetic boundary for an adapter or a development mock.
 * Design: doc/FHE-ACE-RTLIB-RUNTIME-DECISION.md and
 * doc/FHE-SYNC6-CKKS-IR-CONFORMANCE-GATE.md. This is not the public FHE ABI.
 */

#ifndef open64_fhe_ace_api_INCLUDED
#define open64_fhe_ace_api_INCLUDED

#ifdef OPEN64_FHE_REAL_ACE
#include "rt_ant/rt_ant.h"
#else

#include <stddef.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef struct open64_fhe_ace_mock_cipher_s CIPHERTEXT;
typedef CIPHERTEXT *CIPHER;

CIPHER Alloc_ciphertext(void);
void Free_cipher(CIPHER cipher);
size_t Level(CIPHER cipher);
uint32_t Sc_degree(CIPHER cipher);
uint32_t Get_slots(CIPHER cipher);
CIPHER Add_ciph(CIPHER result, CIPHER left, CIPHER right);
CIPHER Sub_ciph(CIPHER result, CIPHER left, CIPHER right);
CIPHER Mul_ciph(CIPHER result, CIPHER left, CIPHER right);
CIPHER Rescale_ciph(CIPHER result, CIPHER input);
CIPHER Rotate_ciph(CIPHER result, CIPHER input, int32_t rotation);
CIPHER Bootstrap(CIPHER result, CIPHER input, uint32_t target_level);

#ifdef __cplusplus
}
#endif
#endif

#endif /* open64_fhe_ace_api_INCLUDED */
