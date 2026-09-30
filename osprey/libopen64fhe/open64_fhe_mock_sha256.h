/*
 * Copyright (C) 2026 Open64 Project
 *
 * Internal SHA-256 support for the deterministic FHE mock provider.
 */

#ifndef open64_fhe_mock_sha256_INCLUDED
#define open64_fhe_mock_sha256_INCLUDED

#include <stddef.h>
#include <stdint.h>

void open64_fhe_mock_sha256(const void *bytes, size_t size,
                            uint8_t digest[32]);

#endif /* open64_fhe_mock_sha256_INCLUDED */
