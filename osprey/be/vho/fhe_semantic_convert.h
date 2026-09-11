/*
 * Copyright (C) 2026 Open64 Project
 */

#ifndef fhe_semantic_convert_INCLUDED
#define fhe_semantic_convert_INCLUDED

#include <stdio.h>

#include "defs.h"
#include "dsl_fhe.h"
#include "dsl_fhe_plan.h"

#define VHO_FHE_ACE_RELU_PROFILE_NAME \
    "ace.chebyshev.sign.7x15x13.depth11"
#define VHO_FHE_ACE_RELU_PROFILE_VERSION 1U
#define VHO_FHE_ACE_RELU_PROFILE_IDENTITY \
    "ace.chebyshev.sign.7x15x13.depth11.v1"
#define VHO_FHE_ACE_RELU_MANIFEST_SHA256 \
    "75132d449852303ec3e44e86c8a5b5ffc196c0643cf7fadff453d797c2266931"

typedef struct {
    UINT32 ordinal;
    UINT32 degree;
    const UINT64 *coefficient_binary64_bits;
    UINT32 coefficient_count;
    const char *coefficient_sha256;
} VHO_FHE_RELU_STAGE_MANIFEST;

typedef struct {
    const char *profile_name;
    UINT32 profile_version;
    const char *source_revision;
    const char *source_sha256;
    const char *manifest_sha256;
    const char *coefficient_bundle_sha256;
    const VHO_FHE_RELU_STAGE_MANIFEST *stages;
    UINT32 stage_count;
} VHO_FHE_RELU_PROFILE_MANIFEST;

extern BOOL VHO_FHE_Register_Default_Semantic_Conversion (void);
extern const VHO_FHE_RELU_PROFILE_MANIFEST *
    VHO_FHE_Approved_Ace_Relu_Profile (void);
extern BOOL VHO_FHE_Validate_Relu_Profile_Manifest
                                (const VHO_FHE_RELU_PROFILE_MANIFEST *manifest,
                                 FILE *diagnostic);
extern DSL_FHE_COMPOSITE_PROFILE_ID
    VHO_FHE_Intern_Approved_Ace_Relu_Profile
                                (DSL_FHE_CONFIG_ID config_id,
                                 FILE *diagnostic);

#endif /* fhe_semantic_convert_INCLUDED */
