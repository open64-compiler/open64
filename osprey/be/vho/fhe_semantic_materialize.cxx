/*
 * Copyright (C) 2026 Open64 Project
 *
 * FHE-owned SYNC-4 policy for the approved ACE composite ReLU schedule.
 * Fixed rows, binary images, and checkpoint publication remain owned by the
 * common VHO infrastructure.
 */

#include <ctype.h>
#include <stdio.h>
#include <string.h>

#include <set>
#include <string>
#include <vector>

#include "rapidjson/document.h"

#include "defs.h"
#include "config_fhe.h"
#include "fhe_image.h"
#include "fhe_plan.h"
#include "dsl_ir_image.h"
#include "fhe_materialize.h"
#include "fhe_semantic_materialize.h"
#include "pu_info.h"
#include "strtab.h"
#include "wn.h"

#define VHO_FHE_SYNC4_PROVIDER_SCHEMA \
    "open64.fhe.sync4.relu-provider-capability.v1"
#define VHO_FHE_SYNC4_PROVIDER_STATUS \
    "approved_static_subset_evidence"
#define VHO_FHE_SYNC4_PROVIDER_SHA256 \
    "5c7c072b90c1461cb5278b2beb38fdad1713815dc048e68cef8dd94d99731b58"
#define VHO_FHE_SYNC4_PROVIDER_REVISION \
    "fb76131171b9f82aa6387f84dd73684fba5277e8"
#define VHO_FHE_SYNC4_PROFILE_NAME \
    "ace.chebyshev.sign.7x15x13.depth11"
#define VHO_FHE_SYNC4_PROFILE_IDENTITY \
    "ace.chebyshev.sign.7x15x13.depth11.v1"
#define VHO_FHE_SYNC4_COEFFICIENT_MANIFEST_SHA256 \
    "75132d449852303ec3e44e86c8a5b5ffc196c0643cf7fadff453d797c2266931"
#define VHO_FHE_SYNC4_COEFFICIENT_BUNDLE_SHA256 \
    "d4e7f691fe763d5673384e23e0bc825875e49de545df78d7f7d7b2ca5e613438"

typedef struct {
    UINT32 state[8];
    UINT64 bit_count;
    unsigned char block[64];
    UINT32 block_size;
} VHO_FHE_SYNC4_SHA256_CONTEXT;

typedef struct {
    BOOL initialized;
    BOOL approved;
    std::string bootstrap_mode;
    std::string manifest_path;
    std::string manifest_sha256;
    std::set<ST_IDX> processed_owners;
} VHO_FHE_SYNC4_STATE;

static VHO_FHE_SYNC4_STATE VHO_FHE_sync4_state;

static BOOL
VHO_FHE_SYNC4_Report
        (FILE *diagnostic, const char *code, const char *message)
{
    if (diagnostic != NULL)
        fprintf(diagnostic, "%s: %s\n", code, message);
    return FALSE;
}

static UINT32
VHO_FHE_SYNC4_SHA256_Rotate (UINT32 value, UINT32 amount)
{
    return (value >> amount) | (value << (32 - amount));
}

static void
VHO_FHE_SYNC4_SHA256_Transform
        (VHO_FHE_SYNC4_SHA256_CONTEXT *context,
         const unsigned char *block)
{
    static const UINT32 constants[64] = {
        0x428a2f98U, 0x71374491U, 0xb5c0fbcfU, 0xe9b5dba5U,
        0x3956c25bU, 0x59f111f1U, 0x923f82a4U, 0xab1c5ed5U,
        0xd807aa98U, 0x12835b01U, 0x243185beU, 0x550c7dc3U,
        0x72be5d74U, 0x80deb1feU, 0x9bdc06a7U, 0xc19bf174U,
        0xe49b69c1U, 0xefbe4786U, 0x0fc19dc6U, 0x240ca1ccU,
        0x2de92c6fU, 0x4a7484aaU, 0x5cb0a9dcU, 0x76f988daU,
        0x983e5152U, 0xa831c66dU, 0xb00327c8U, 0xbf597fc7U,
        0xc6e00bf3U, 0xd5a79147U, 0x06ca6351U, 0x14292967U,
        0x27b70a85U, 0x2e1b2138U, 0x4d2c6dfcU, 0x53380d13U,
        0x650a7354U, 0x766a0abbU, 0x81c2c92eU, 0x92722c85U,
        0xa2bfe8a1U, 0xa81a664bU, 0xc24b8b70U, 0xc76c51a3U,
        0xd192e819U, 0xd6990624U, 0xf40e3585U, 0x106aa070U,
        0x19a4c116U, 0x1e376c08U, 0x2748774cU, 0x34b0bcb5U,
        0x391c0cb3U, 0x4ed8aa4aU, 0x5b9cca4fU, 0x682e6ff3U,
        0x748f82eeU, 0x78a5636fU, 0x84c87814U, 0x8cc70208U,
        0x90befffaU, 0xa4506cebU, 0xbef9a3f7U, 0xc67178f2U
    };
    UINT32 words[64];
    for (UINT32 i = 0; i < 16; ++i) {
        words[i] = ((UINT32)block[i * 4] << 24) |
                   ((UINT32)block[i * 4 + 1] << 16) |
                   ((UINT32)block[i * 4 + 2] << 8) |
                   (UINT32)block[i * 4 + 3];
    }
    for (UINT32 i = 16; i < 64; ++i) {
        UINT32 s0 = VHO_FHE_SYNC4_SHA256_Rotate(words[i - 15], 7) ^
                    VHO_FHE_SYNC4_SHA256_Rotate(words[i - 15], 18) ^
                    (words[i - 15] >> 3);
        UINT32 s1 = VHO_FHE_SYNC4_SHA256_Rotate(words[i - 2], 17) ^
                    VHO_FHE_SYNC4_SHA256_Rotate(words[i - 2], 19) ^
                    (words[i - 2] >> 10);
        words[i] = words[i - 16] + s0 + words[i - 7] + s1;
    }
    UINT32 a = context->state[0];
    UINT32 b = context->state[1];
    UINT32 c = context->state[2];
    UINT32 d = context->state[3];
    UINT32 e = context->state[4];
    UINT32 f = context->state[5];
    UINT32 g = context->state[6];
    UINT32 h = context->state[7];
    for (UINT32 i = 0; i < 64; ++i) {
        UINT32 s1 = VHO_FHE_SYNC4_SHA256_Rotate(e, 6) ^
                    VHO_FHE_SYNC4_SHA256_Rotate(e, 11) ^
                    VHO_FHE_SYNC4_SHA256_Rotate(e, 25);
        UINT32 choice = (e & f) ^ ((~e) & g);
        UINT32 temp1 = h + s1 + choice + constants[i] + words[i];
        UINT32 s0 = VHO_FHE_SYNC4_SHA256_Rotate(a, 2) ^
                    VHO_FHE_SYNC4_SHA256_Rotate(a, 13) ^
                    VHO_FHE_SYNC4_SHA256_Rotate(a, 22);
        UINT32 majority = (a & b) ^ (a & c) ^ (b & c);
        UINT32 temp2 = s0 + majority;
        h = g;
        g = f;
        f = e;
        e = d + temp1;
        d = c;
        c = b;
        b = a;
        a = temp1 + temp2;
    }
    context->state[0] += a;
    context->state[1] += b;
    context->state[2] += c;
    context->state[3] += d;
    context->state[4] += e;
    context->state[5] += f;
    context->state[6] += g;
    context->state[7] += h;
}

static std::string
VHO_FHE_SYNC4_SHA256 (const std::vector<unsigned char> &bytes)
{
    static const UINT32 initial[8] = {
        0x6a09e667U, 0xbb67ae85U, 0x3c6ef372U, 0xa54ff53aU,
        0x510e527fU, 0x9b05688cU, 0x1f83d9abU, 0x5be0cd19U
    };
    VHO_FHE_SYNC4_SHA256_CONTEXT context;
    memcpy(context.state, initial, sizeof(initial));
    context.bit_count = 0;
    context.block_size = 0;
    for (size_t i = 0; i < bytes.size(); ++i) {
        context.block[context.block_size++] = bytes[i];
        context.bit_count += 8;
        if (context.block_size == 64) {
            VHO_FHE_SYNC4_SHA256_Transform(&context, context.block);
            context.block_size = 0;
        }
    }
    context.block[context.block_size++] = 0x80;
    if (context.block_size > 56) {
        while (context.block_size < 64)
            context.block[context.block_size++] = 0;
        VHO_FHE_SYNC4_SHA256_Transform(&context, context.block);
        context.block_size = 0;
    }
    while (context.block_size < 56)
        context.block[context.block_size++] = 0;
    for (INT32 shift = 56; shift >= 0; shift -= 8)
        context.block[context.block_size++] =
            (unsigned char)(context.bit_count >> shift);
    VHO_FHE_SYNC4_SHA256_Transform(&context, context.block);

    char digest[65];
    for (UINT32 i = 0; i < 8; ++i)
        snprintf(digest + i * 8, 9, "%08x", context.state[i]);
    digest[64] = '\0';
    return digest;
}

static BOOL
VHO_FHE_SYNC4_Read_File
        (const char *path, std::vector<unsigned char> *bytes)
{
    if (path == NULL || path[0] == '\0' || bytes == NULL)
        return FALSE;
    FILE *file = fopen(path, "rb");
    if (file == NULL)
        return FALSE;
    unsigned char block[16384];
    BOOL valid = TRUE;
    bytes->clear();
    for (;;) {
        size_t count = fread(block, 1, sizeof(block), file);
        if (count != 0)
            bytes->insert(bytes->end(), block, block + count);
        if (count != sizeof(block)) {
            if (ferror(file))
                valid = FALSE;
            break;
        }
        if (bytes->size() > 1024U * 1024U)
            valid = FALSE;
        if (!valid)
            break;
    }
    valid = fclose(file) == 0 && valid && !bytes->empty();
    return valid;
}

static BOOL
VHO_FHE_SYNC4_JSON_String_Equals
        (const rapidjson::Value &object, const char *name,
         const char *expected)
{
    return object.IsObject() && object.HasMember(name) &&
           object[name].IsString() &&
           strcmp(object[name].GetString(), expected) == 0;
}

static BOOL
VHO_FHE_SYNC4_JSON_Uint_Equals
        (const rapidjson::Value &object, const char *name, UINT32 expected)
{
    return object.IsObject() && object.HasMember(name) &&
           object[name].IsUint() && object[name].GetUint() == expected;
}

static BOOL
VHO_FHE_SYNC4_Has_Primitive
        (const rapidjson::Value &primitives, const char *name)
{
    if (!primitives.IsArray())
        return FALSE;
    for (rapidjson::SizeType i = 0; i < primitives.Size(); ++i) {
        if (primitives[i].IsString() &&
            strcmp(primitives[i].GetString(), name) == 0)
            return TRUE;
    }
    return FALSE;
}

static BOOL
VHO_FHE_SYNC4_Parse_Provider
        (const std::vector<unsigned char> &bytes, FILE *diagnostic)
{
    rapidjson::Document root;
    root.Parse((const char *)&bytes[0], bytes.size());
    if (root.HasParseError() || !root.IsObject() ||
        !VHO_FHE_SYNC4_JSON_String_Equals
            (root, "schema", VHO_FHE_SYNC4_PROVIDER_SCHEMA) ||
        !VHO_FHE_SYNC4_JSON_String_Equals
            (root, "status", VHO_FHE_SYNC4_PROVIDER_STATUS) ||
        !root.HasMember("provider") || !root["provider"].IsObject() ||
        !root.HasMember("profile") || !root["profile"].IsObject() ||
        !root.HasMember("logical_configuration") ||
        !root["logical_configuration"].IsObject() ||
        !root.HasMember("required_primitives") ||
        !root.HasMember("bootstrap_evidence") ||
        !root["bootstrap_evidence"].IsObject() ||
        !root.HasMember("security_boundary") ||
        !root["security_boundary"].IsObject())
        return VHO_FHE_SYNC4_Report
                   (diagnostic, "CFHEMAT-CAP-001",
                    "provider capability manifest is malformed");

    const rapidjson::Value &provider = root["provider"];
    const rapidjson::Value &profile = root["profile"];
    const rapidjson::Value &config = root["logical_configuration"];
    const rapidjson::Value &bootstrap = root["bootstrap_evidence"];
    const rapidjson::Value &security = root["security_boundary"];
    if (!VHO_FHE_SYNC4_JSON_String_Equals(provider, "name", "ace-ant") ||
        !VHO_FHE_SYNC4_JSON_String_Equals(provider, "runtime", "FHErt_ant") ||
        !VHO_FHE_SYNC4_JSON_String_Equals
            (provider, "revision", VHO_FHE_SYNC4_PROVIDER_REVISION) ||
        !VHO_FHE_SYNC4_JSON_String_Equals
            (profile, "name", VHO_FHE_SYNC4_PROFILE_IDENTITY) ||
        !VHO_FHE_SYNC4_JSON_String_Equals
            (profile, "coefficient_manifest_sha256",
             VHO_FHE_SYNC4_COEFFICIENT_MANIFEST_SHA256) ||
        !VHO_FHE_SYNC4_JSON_String_Equals
            (profile, "coefficient_bundle_sha256",
             VHO_FHE_SYNC4_COEFFICIENT_BUNDLE_SHA256) ||
        !VHO_FHE_SYNC4_JSON_Uint_Equals
            (profile, "total_multiplicative_depth", 11) ||
        !VHO_FHE_SYNC4_JSON_String_Equals
            (profile, "evaluation_scheme", "clenshaw") ||
        !profile.HasMember("stage_degrees") ||
        !profile["stage_degrees"].IsArray() ||
        profile["stage_degrees"].Size() != 3 ||
        !profile["stage_degrees"][0].IsUint() ||
        profile["stage_degrees"][0].GetUint() != 7 ||
        !profile["stage_degrees"][1].IsUint() ||
        profile["stage_degrees"][1].GetUint() != 15 ||
        !profile["stage_degrees"][2].IsUint() ||
        profile["stage_degrees"][2].GetUint() != 13)
        return VHO_FHE_SYNC4_Report
                   (diagnostic, "CFHEMAT-CAP-002",
                    "provider profile does not match the approved ACE policy");

    if (!VHO_FHE_SYNC4_JSON_String_Equals(config, "scheme", "CKKS") ||
        !VHO_FHE_SYNC4_JSON_Uint_Equals(config, "ring_dimension", 65536) ||
        !config.HasMember("slot_count") || !config["slot_count"].IsUint() ||
        config["slot_count"].GetUint() < 32768 ||
        !config.HasMember("multiplicative_depth") ||
        !config["multiplicative_depth"].IsUint() ||
        config["multiplicative_depth"].GetUint() < 11 ||
        !VHO_FHE_SYNC4_JSON_Uint_Equals
            (config, "first_modulus_bits", 60) ||
        !VHO_FHE_SYNC4_JSON_Uint_Equals
            (config, "scaling_modulus_bits", 56) ||
        !VHO_FHE_SYNC4_JSON_Uint_Equals(config, "component_count", 2) ||
        !config.HasMember("minimum_precision_bits") ||
        !config["minimum_precision_bits"].IsUint() ||
        config["minimum_precision_bits"].GetUint() < 30)
        return VHO_FHE_SYNC4_Report
                   (diagnostic, "CFHEMAT-CAP-002",
                    "provider CKKS configuration is insufficient");

    static const char *required[] = {
        "bootstrap_to_target_level", "ciphertext_add",
        "ciphertext_multiply", "plaintext_multiply", "scalar_multiply",
        "rescale", "relinearize", "constant_encode",
        "clenshaw_chebyshev_evaluation"
    };
    for (UINT32 i = 0; i < sizeof(required) / sizeof(required[0]); ++i) {
        if (!VHO_FHE_SYNC4_Has_Primitive(root["required_primitives"],
                                         required[i]))
            return VHO_FHE_SYNC4_Report
                       (diagnostic, "CFHEMAT-CAP-002",
                        "provider lacks a required ReLU primitive");
    }
    if (!VHO_FHE_SYNC4_JSON_Uint_Equals
            (bootstrap, "context_count", 19) ||
        !bootstrap.HasMember("target_level_counts") ||
        !bootstrap["target_level_counts"].IsObject() ||
        !VHO_FHE_SYNC4_JSON_Uint_Equals
            (bootstrap["target_level_counts"], "15", 16) ||
        !VHO_FHE_SYNC4_JSON_Uint_Equals
            (bootstrap["target_level_counts"], "17", 1) ||
        !VHO_FHE_SYNC4_JSON_Uint_Equals
            (bootstrap["target_level_counts"], "18", 2) ||
        !security.HasMember("logical_materialization_requires_secret_key") ||
        !security["logical_materialization_requires_secret_key"].IsBool() ||
        security["logical_materialization_requires_secret_key"].GetBool())
        return VHO_FHE_SYNC4_Report
                   (diagnostic, "CFHEMAT-CAP-002",
                    "provider bootstrap or security capability is invalid");
    return TRUE;
}

/* Recheck the exact pinned provider package without process-local state. */
BOOL
VHO_FHE_Authenticate_Approved_Provider_Manifest
        (const char *path, const char *sha256, FILE *diagnostic)
{
    if (path == NULL || path[0] == '\0' || sha256 == NULL ||
        strcmp(sha256, VHO_FHE_SYNC4_PROVIDER_SHA256) != 0)
        return VHO_FHE_SYNC4_Report
                   (diagnostic, "CFHEMAT-CAP-001",
                    "provider manifest digest is not the approved package");
    std::vector<unsigned char> bytes;
    if (!VHO_FHE_SYNC4_Read_File(path, &bytes) ||
        VHO_FHE_SYNC4_SHA256(bytes) != sha256)
        return VHO_FHE_SYNC4_Report
                   (diagnostic, "CFHEMAT-CAP-001",
                    "provider manifest exact-byte SHA-256 mismatch");
    return VHO_FHE_SYNC4_Parse_Provider(bytes, diagnostic);
}

/* Freeze one approved provider policy across all materialization PUs. */
static BOOL
VHO_FHE_SYNC4_Prepare_Provider
        (const VHO_FHE_MATERIALIZE_OPTIONS *options, FILE *diagnostic)
{
    const std::string mode = options != NULL && options->bootstrap_mode != NULL ?
                                 options->bootstrap_mode : "";
    const std::string path = options != NULL &&
                             options->provider_manifest_path != NULL ?
                                 options->provider_manifest_path : "";
    const std::string digest = options != NULL &&
                               options->provider_manifest_sha256 != NULL ?
                                   options->provider_manifest_sha256 : "";
    if (VHO_FHE_sync4_state.initialized) {
        if (mode != VHO_FHE_sync4_state.bootstrap_mode ||
            path != VHO_FHE_sync4_state.manifest_path ||
            digest != VHO_FHE_sync4_state.manifest_sha256)
            return VHO_FHE_SYNC4_Report
                       (diagnostic, "CFHEMAT-CAP-001",
                        "materialization policy changed across PUs");
        return TRUE;
    }
    VHO_FHE_sync4_state.bootstrap_mode = mode;
    VHO_FHE_sync4_state.manifest_path = path;
    VHO_FHE_sync4_state.manifest_sha256 = digest;
    if (mode == "manual" || mode == "off") {
        VHO_FHE_sync4_state.initialized = TRUE;
        return TRUE;
    }
    if (!VHO_FHE_Authenticate_Approved_Provider_Manifest
             (path.c_str(), digest.c_str(), diagnostic))
        return FALSE;
    VHO_FHE_sync4_state.approved = TRUE;
    VHO_FHE_sync4_state.initialized = TRUE;
    return TRUE;
}

static BOOL
VHO_FHE_SYNC4_Range_Eligible (const DSL_FHE_CONTEXT_RANGE_RECORD &range)
{
    if ((range.flags & DSL_FHE_CONTEXT_RANGE_IDENTITY_IS_CALLEE) == 0)
        return FALSE;
    UINT32 matches = 0;
    for (UINT32 id = 1; id <= DSL_FHE_Approx_Association_Count(); ++id) {
        DSL_FHE_APPROX_ASSOCIATION_RECORD association;
        DSL_FHE_CONVERSION_DISPOSITION_RECORD disposition;
        if (!DSL_FHE_Approx_Association_Get(id, &association) ||
            association.profile_id != range.profile_id ||
            association.source_relu_value_id != range.source_relu_value_id)
            continue;
        if (!DSL_FHE_Plan_Get_Conversion_Disposition
                (association.disposition_id, &disposition) ||
            disposition.disposition !=
                DSL_FHE_DISPOSITION_REQUIRE_COMPOSITE_APPROXIMATION ||
            disposition.owner_pu_st != range.owner_pu_st)
            return FALSE;
        ++matches;
    }
    return matches == 1;
}

static BOOL
VHO_FHE_SYNC4_Profile_Valid
        (DSL_FHE_COMPOSITE_PROFILE_ID profile_id, FILE *diagnostic)
{
    DSL_FHE_COMPOSITE_PROFILE_RECORD profile;
    if (!DSL_FHE_Approx_Profile_Get(profile_id, &profile) ||
        strcmp(Index_To_Str(profile.profile_name),
               VHO_FHE_SYNC4_PROFILE_NAME) != 0 ||
        profile.profile_version != 1 ||
        profile.total_multiplicative_depth != 11 ||
        profile.reconstruction !=
            DSL_FHE_RECONSTRUCTION_RELU_FROM_NORMALIZED_SIGN ||
        profile.normalization_policy !=
            DSL_FHE_NORMALIZATION_POSITIVE_CONTEXT_BOUND ||
        profile.pre_refresh_policy != DSL_FHE_PRE_REFRESH_REQUIRED ||
        strcmp(Index_To_Str(profile.manifest_sha256),
               VHO_FHE_SYNC4_COEFFICIENT_MANIFEST_SHA256) != 0 ||
        profile.stage_count != 3)
        return VHO_FHE_SYNC4_Report
                   (diagnostic, "CFHEMAT-RELU-005",
                    "composite ReLU profile does not match the approved ACE profile");
    static const UINT32 degrees[3] = { 7, 15, 13 };
    static const INT32 consumptions[3] = { 3, 4, 4 };
    for (UINT32 i = 0; i < 3; ++i) {
        DSL_FHE_APPROX_STAGE_RECORD stage;
        if (!DSL_FHE_Approx_Stage_Find(profile_id, i, &stage) ||
            stage.degree != degrees[i] ||
            stage.evaluation_scheme != DSL_FHE_APPROX_EVAL_CLENSHAW ||
            stage.level_consumption != consumptions[i] ||
            stage.minimum_precision_bits < 30 ||
            stage.output_component_policy !=
                DSL_FHE_APPROX_COMPONENT_RELINEARIZED_TWO)
            return VHO_FHE_SYNC4_Report
                       (diagnostic, "CFHEMAT-RELU-005",
                        "composite ReLU stage contract is invalid");
    }
    return TRUE;
}

static BOOL
VHO_FHE_SYNC4_State_Valid
        (const DSL_FHE_CONTEXT_CKKS_STATE_RECORD &state,
         const DSL_FHE_CONTEXT_RANGE_RECORD &range,
         UINT32 role, UINT32 version, INT32 level, BOOL refresh)
{
    return state.owner_pu_st == range.owner_pu_st &&
           state.source_value_id == range.source_relu_value_id &&
           state.context_pu_identity_id == range.context_pu_identity_id &&
           state.context_callsite_id == range.context_callsite_id &&
           state.state_role == role && state.state_version == version &&
           state.scheme == DSL_FHE_SCHEME_CKKS &&
           state.value_class == DSL_FHE_VALUE_CLASS_CIPHERTEXT &&
           state.level == level && state.scale_bits == 56 &&
           state.component_count == 2 && state.precision_bits >= 30 &&
           state.slot_count == 32768 && state.alignment_group == 0 &&
           strcmp(Index_To_Str(state.encrypted_layout_name), "ckks.packed") == 0 &&
           state.pending_actions ==
               (refresh ? DSL_FHE_CKKS_PENDING_BOOTSTRAP :
                          DSL_FHE_CKKS_PENDING_NONE) &&
           state.pending_bootstrap_reason ==
               (refresh ? DSL_FHE_BOOTSTRAP_REASON_PRE_RELU_REFRESH :
                          DSL_FHE_BOOTSTRAP_REASON_NONE);
}

static BOOL
VHO_FHE_SYNC4_Context_Schedule_Valid
        (const DSL_FHE_CONTEXT_RANGE_RECORD &range, FILE *diagnostic)
{
    DSL_FHE_CONTEXT_CKKS_STATE_RECORD refresh;
    if (!VHO_FHE_SYNC4_Profile_Valid(range.profile_id, diagnostic) ||
        !DSL_FHE_Context_State_Find
            (range.owner_pu_st, range.source_relu_value_id,
             range.context_pu_identity_id, range.context_callsite_id,
             DSL_FHE_CONTEXT_STATE_ROLE_POST_REFRESH, 1, &refresh) ||
        (refresh.level != 15 && refresh.level != 17 && refresh.level != 18) ||
        !VHO_FHE_SYNC4_State_Valid
            (refresh, range, DSL_FHE_CONTEXT_STATE_ROLE_POST_REFRESH,
             1, refresh.level, TRUE))
        return VHO_FHE_SYNC4_Report
                   (diagnostic, "CFHEMAT-RELU-001",
                    "approved ReLU refresh state is missing or inconsistent");
    static const UINT32 roles[6] = {
        DSL_FHE_CONTEXT_STATE_ROLE_POST_REFRESH,
        DSL_FHE_CONTEXT_STATE_ROLE_POST_OPERATION,
        DSL_FHE_CONTEXT_STATE_ROLE_POST_OPERATION,
        DSL_FHE_CONTEXT_STATE_ROLE_POST_OPERATION,
        DSL_FHE_CONTEXT_STATE_ROLE_POST_OPERATION,
        DSL_FHE_CONTEXT_STATE_ROLE_RESULT
    };
    static const UINT32 versions[6] = { 1, 1, 2, 3, 4, 1 };
    const INT32 levels[6] = {
        refresh.level, refresh.level, refresh.level - 3,
        refresh.level - 7, refresh.level - 11, refresh.level - 11
    };
    for (UINT32 ordinal = 0; ordinal < 6; ++ordinal) {
        DSL_FHE_MATERIALIZATION_OPERATION_RECORD operation;
        DSL_FHE_CONTEXT_CKKS_STATE_RECORD output;
        if (!DSL_FHE_Materialization_Find
                (range.owner_pu_st, range.source_relu_value_id,
                 range.context_pu_identity_id, range.context_callsite_id,
                 ordinal, &operation) ||
            !DSL_FHE_Context_State_Get(operation.output_state_id, &output) ||
            !VHO_FHE_SYNC4_State_Valid
                (output, range, roles[ordinal], versions[ordinal],
                 levels[ordinal], ordinal == 0))
            return VHO_FHE_SYNC4_Report
                       (diagnostic, "CFHEMAT-RELU-002",
                        "ReLU materialization schedule is incomplete or invalid");
    }
    return TRUE;
}

static BOOL
VHO_FHE_SYNC4_Create_Context
        (const DSL_FHE_CONTEXT_RANGE_RECORD &range, FILE *diagnostic)
{
    DSL_FHE_CONTEXT_CKKS_STATE_RECORD refresh;
    if (!VHO_FHE_SYNC4_Profile_Valid(range.profile_id, diagnostic) ||
        !DSL_FHE_Context_State_Find
            (range.owner_pu_st, range.source_relu_value_id,
             range.context_pu_identity_id, range.context_callsite_id,
             DSL_FHE_CONTEXT_STATE_ROLE_POST_REFRESH, 1, &refresh) ||
        !VHO_FHE_SYNC4_State_Valid
            (refresh, range, DSL_FHE_CONTEXT_STATE_ROLE_POST_REFRESH,
             1, refresh.level, TRUE) ||
        (refresh.level != 15 && refresh.level != 17 && refresh.level != 18))
        return VHO_FHE_SYNC4_Report
                   (diagnostic, "CFHEMAT-RELU-001",
                    "ReLU context has no approved post-refresh state");

    static const UINT32 roles[5] = {
        DSL_FHE_CONTEXT_STATE_ROLE_POST_OPERATION,
        DSL_FHE_CONTEXT_STATE_ROLE_POST_OPERATION,
        DSL_FHE_CONTEXT_STATE_ROLE_POST_OPERATION,
        DSL_FHE_CONTEXT_STATE_ROLE_POST_OPERATION,
        DSL_FHE_CONTEXT_STATE_ROLE_RESULT
    };
    static const UINT32 versions[5] = { 1, 2, 3, 4, 1 };
    const INT32 levels[5] = {
        refresh.level, refresh.level - 3, refresh.level - 7,
        refresh.level - 11, refresh.level - 11
    };
    DSL_FHE_CONTEXT_CKKS_STATE_RECORD states[5];
    for (UINT32 i = 0; i < 5; ++i) {
        states[i] = refresh;
        states[i].id = DSL_FHE_CONTEXT_CKKS_STATE_INVALID_ID;
        states[i].state_role = roles[i];
        states[i].state_version = versions[i];
        states[i].level = levels[i];
        states[i].pending_actions = DSL_FHE_CKKS_PENDING_NONE;
        states[i].pending_bootstrap_reason = DSL_FHE_BOOTSTRAP_REASON_NONE;
        states[i].flags = 0;
        states[i].reserved = 0;
    }
    if (DSL_FHE_Materialization_Intern_Complete_Context
            (range.id, states, 5) ==
        DSL_FHE_MATERIALIZATION_OPERATION_INVALID_ID)
        return VHO_FHE_SYNC4_Report
                   (diagnostic, "CFHEMAT-RELU-002",
                    "could not atomically materialize ReLU context");
    return VHO_FHE_SYNC4_Context_Schedule_Valid(range, diagnostic);
}

static BOOL
VHO_FHE_SYNC4_Visit_Owner_Contexts
        (ST_IDX owner, const char *mode, BOOL create,
         UINT32 *context_count, FILE *diagnostic)
{
    UINT32 matched = 0;
    for (UINT32 id = 1; id <= DSL_FHE_Context_Range_Count(); ++id) {
        DSL_FHE_CONTEXT_RANGE_RECORD range;
        if (!DSL_FHE_Context_Range_Get(id, &range) ||
            range.owner_pu_st != owner || !VHO_FHE_SYNC4_Range_Eligible(range))
            continue;
        ++matched;
        if (strcmp(mode, "off") == 0)
            return VHO_FHE_SYNC4_Report
                       (diagnostic, "CFHEMAT-RELU-003",
                        "bootstrap=off rejects surviving common.relu");
        DSL_FHE_MATERIALIZATION_OPERATION_RECORD operation;
        BOOL exists = DSL_FHE_Materialization_Find
            (range.owner_pu_st, range.source_relu_value_id,
             range.context_pu_identity_id, range.context_callsite_id,
             0, &operation);
        if (strcmp(mode, "manual") == 0) {
            if (!exists ||
                !VHO_FHE_SYNC4_Context_Schedule_Valid(range, diagnostic))
                return VHO_FHE_SYNC4_Report
                           (diagnostic, "CFHEMAT-RELU-004",
                            "manual mode requires an exact complete schedule");
        } else if (create) {
            if (exists || !VHO_FHE_SYNC4_Create_Context(range, diagnostic))
                return FALSE;
        } else if (VHO_FHE_sync4_state.processed_owners.find(owner) !=
                       VHO_FHE_sync4_state.processed_owners.end()) {
            if (!exists ||
                !VHO_FHE_SYNC4_Context_Schedule_Valid(range, diagnostic))
                return FALSE;
        } else if (exists) {
            return VHO_FHE_SYNC4_Report
                       (diagnostic, "CFHEMAT-RELU-002",
                        "auto/on input already contains a materialized schedule");
        }
    }
    if (context_count != NULL)
        *context_count = matched;
    return TRUE;
}

static BOOL
VHO_FHE_SYNC4_Gatekeeper
        (struct pu_info *pu_info, WN *tree,
         const VHO_FHE_MATERIALIZE_OPTIONS *options, FILE *diagnostic)
{
    if (pu_info == NULL || tree == NULL || options == NULL ||
        !VHO_FHE_SYNC4_Prepare_Provider(options, diagnostic))
        return FALSE;
    return VHO_FHE_SYNC4_Visit_Owner_Contexts
        (PU_Info_proc_sym(pu_info), options->bootstrap_mode, FALSE,
         NULL, diagnostic);
}

static BOOL
VHO_FHE_SYNC4_Pass
        (struct pu_info *pu_info, WN **tree,
         const VHO_FHE_MATERIALIZE_OPTIONS *options, FILE *diagnostic,
         VHO_FHE_MATERIALIZE_RESULT *result)
{
    if (pu_info == NULL || tree == NULL || *tree == NULL ||
        options == NULL || result == NULL)
        return FALSE;
    ST_IDX owner = PU_Info_proc_sym(pu_info);
    if (VHO_FHE_sync4_state.processed_owners.find(owner) !=
        VHO_FHE_sync4_state.processed_owners.end())
        return VHO_FHE_SYNC4_Report
                   (diagnostic, "CFHEMAT-RELU-002",
                    "PU materialization callback was repeated");
    UINT32 contexts = 0;
    BOOL create = strcmp(options->bootstrap_mode, "auto") == 0 ||
                  strcmp(options->bootstrap_mode, "on") == 0;
    if (!VHO_FHE_SYNC4_Visit_Owner_Contexts
            (owner, options->bootstrap_mode, create, &contexts, diagnostic))
        return FALSE;
    VHO_FHE_sync4_state.processed_owners.insert(owner);
    result->context_count += contexts;
    result->operation_count += contexts * 6;
    result->refresh_count += contexts;
    return TRUE;
}

static BOOL
VHO_FHE_SYNC4_Write_Reports
        (const VHO_FHE_MATERIALIZE_RESULT *aggregate, FILE *diagnostic)
{
    const char *output = VHO_FHE_Materialization_Checkpoint_Output;
    if (output == NULL || output[0] == '\0')
        return VHO_FHE_SYNC4_Report
                   (diagnostic, "CFHEMAT-CHECKPOINT-002",
                    "materialization output path is unavailable");
    std::string report_final = std::string(output) +
                               ".materialization-report.txt";
    std::string report_temp = report_final + ".tmp";
    std::string capability_final = std::string(output) +
                                   ".capability-report.txt";
    std::string capability_temp = capability_final + ".tmp";
    FILE *report = fopen(report_temp.c_str(), "w");
    if (report == NULL)
        return VHO_FHE_SYNC4_Report
                   (diagnostic, "CFHEMAT-CHECKPOINT-002",
                    "could not create materialization report");
    fprintf(report, "FHE SYNC-4 ReLU materialization report\n");
    fprintf(report, "bootstrap_mode=%s\n",
            VHO_FHE_sync4_state.bootstrap_mode.c_str());
    fprintf(report, "contexts=%u\n", aggregate->context_count);
    fprintf(report, "operations=%u\n", aggregate->operation_count);
    fprintf(report, "refreshes=%u\n", aggregate->refresh_count);
    fprintf(report, "post_refresh_levels=15:16,17:1,18:2\n");
    fprintf(report, "profile=%s\n", VHO_FHE_SYNC4_PROFILE_IDENTITY);
    fprintf(report, "stage_degrees=7,15,13\n");
    fprintf(report, "stage_level_consumption=3,4,4\n");
    BOOL valid = fclose(report) == 0;

    FILE *capability = valid ? fopen(capability_temp.c_str(), "w") : NULL;
    if (capability == NULL)
        valid = FALSE;
    if (capability != NULL) {
        fprintf(capability, "FHE SYNC-4 provider capability report\n");
        fprintf(capability, "provider=ace-ant\n");
        fprintf(capability, "runtime=FHErt_ant\n");
        fprintf(capability, "revision=%s\n",
                VHO_FHE_SYNC4_PROVIDER_REVISION);
        fprintf(capability, "manifest_sha256=%s\n",
                VHO_FHE_sync4_state.manifest_sha256.empty() ?
                    "manual-not-supplied" :
                    VHO_FHE_sync4_state.manifest_sha256.c_str());
        fprintf(capability, "logical_ckks=N65536,Q0=60,scale=56,slots=32768\n");
        fprintf(capability, "scope=relu-materialization-only\n");
        valid = fclose(capability) == 0 && valid;
    }
    if (!valid) {
        remove(report_temp.c_str());
        remove(capability_temp.c_str());
        return VHO_FHE_SYNC4_Report
                   (diagnostic, "CFHEMAT-CHECKPOINT-002",
                    "could not finalize materialization reports");
    }
    if (!VHO_FHE_Materialize_Checkpoint_Register_Artifact
            (capability_temp.c_str(), capability_final.c_str()) ||
        !VHO_FHE_Materialize_Checkpoint_Register_Artifact
            (report_temp.c_str(), report_final.c_str())) {
        remove(report_temp.c_str());
        remove(capability_temp.c_str());
        return VHO_FHE_SYNC4_Report
                   (diagnostic, "CFHEMAT-CHECKPOINT-002",
                    "could not register materialization reports");
    }
    return TRUE;
}

static BOOL
VHO_FHE_SYNC4_Finalizer
        (const VHO_FHE_MATERIALIZE_RESULT *aggregate, FILE *diagnostic)
{
    if (aggregate == NULL || aggregate->context_count != 19 ||
        aggregate->operation_count != 114 || aggregate->refresh_count != 19 ||
        DSL_FHE_Materialization_Operation_Count() != 114 ||
        !DSL_FHE_Materialization_Image_Validate(diagnostic))
        return VHO_FHE_SYNC4_Report
                   (diagnostic, "CFHEMAT-CHECKPOINT-001",
                    "materialization coverage is not exactly 19 contexts");
    UINT32 levels15 = 0;
    UINT32 levels17 = 0;
    UINT32 levels18 = 0;
    for (UINT32 id = 1; id <= DSL_FHE_Context_Range_Count(); ++id) {
        DSL_FHE_CONTEXT_RANGE_RECORD range;
        DSL_FHE_CONTEXT_CKKS_STATE_RECORD refresh;
        if (!DSL_FHE_Context_Range_Get(id, &range) ||
            !VHO_FHE_SYNC4_Range_Eligible(range))
            continue;
        if (!VHO_FHE_SYNC4_Context_Schedule_Valid(range, diagnostic) ||
            !DSL_FHE_Context_State_Find
                (range.owner_pu_st, range.source_relu_value_id,
                 range.context_pu_identity_id, range.context_callsite_id,
                 DSL_FHE_CONTEXT_STATE_ROLE_POST_REFRESH, 1, &refresh))
            return FALSE;
        if (refresh.level == 15)
            ++levels15;
        else if (refresh.level == 17)
            ++levels17;
        else if (refresh.level == 18)
            ++levels18;
    }
    if (levels15 != 16 || levels17 != 1 || levels18 != 2)
        return VHO_FHE_SYNC4_Report
                   (diagnostic, "CFHEMAT-RELU-001",
                    "post-refresh level distribution is not 16/1/2");
    fprintf(diagnostic,
            "FHE-SYNC4-MATERIALIZATION: contexts=19 operations=114 "
            "refresh_levels=15:16,17:1,18:2\n");
    return VHO_FHE_SYNC4_Write_Reports(aggregate, diagnostic);
}

static void
VHO_FHE_SYNC4_Completion (BOOL committed)
{
    (void)committed;
    VHO_FHE_sync4_state = VHO_FHE_SYNC4_STATE();
}

BOOL
VHO_FHE_Register_Default_Semantic_Materialization (void)
{
    return VHO_FHE_Materialize_Register_Semantic_Gatekeeper
               (VHO_FHE_SYNC4_Gatekeeper) &&
           VHO_FHE_Materialize_Register_Pass(VHO_FHE_SYNC4_Pass) &&
           VHO_FHE_Materialize_Register_Checkpoint_Lifecycle
               (VHO_FHE_SYNC4_Finalizer, VHO_FHE_SYNC4_Completion);
}

namespace {
struct VHO_FHE_SYNC4_Default_Registration {
    VHO_FHE_SYNC4_Default_Registration()
    {
        (void)VHO_FHE_Register_Default_Semantic_Materialization();
    }
};

static VHO_FHE_SYNC4_Default_Registration VHO_FHE_sync4_registration;
}
