/*
 * Copyright (C) 2026 Open64 Project
 *
 * Small dependency-free SHA-256 service for FHE backend policy artifacts.
 * It avoids adding a crypto-library dependency to be.so and lw_inline.
 * Design: doc/FHE-SYNC6-S6-0C-DETAILED-EXECUTION-PLAN.md.
 */

#ifndef fhe_sha256_INCLUDED
#define fhe_sha256_INCLUDED

#include <stddef.h>
#include <string>

/* Hash one in-memory canonical policy buffer and return lowercase hex. */
std::string VHO_FHE_SHA256(const unsigned char *bytes, size_t size);

#endif /* fhe_sha256_INCLUDED */
