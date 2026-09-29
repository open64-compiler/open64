/*
 * Copyright (C) 2026 Open64 Project
 *
 * Test-only host integration for the deterministic FHE runtime provider.
 */

#ifndef open64_fhe_mock_test_INCLUDED
#define open64_fhe_mock_test_INCLUDED

#include "open64_fhe_runtime_abi.h"

#ifdef __cplusplus
extern "C" {
#endif

open64_fhe_status_v1 open64_fhe_mock_host_bootstrap_create_v1(
    const uint8_t deployment_identity_sha256[32],
    open64_fhe_host_bootstrap_v1_t *out_bootstrap);

open64_fhe_status_v1 open64_fhe_mock_host_bootstrap_destroy_v1(
    open64_fhe_host_bootstrap_v1_t *bootstrap);

#ifdef __cplusplus
} /* extern "C" */
#endif

#endif /* open64_fhe_mock_test_INCLUDED */
