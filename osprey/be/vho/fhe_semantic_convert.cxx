/*
 * Copyright (C) 2026 Open64 Project
 */

#include <ctype.h>
#include <errno.h>
#include <math.h>
#include <stdlib.h>
#include <string.h>

#include <algorithm>
#include <deque>
#include <map>
#include <sstream>
#include <string>
#include <vector>

#include "fhe_semantic_convert.h"
#include "fhe_convert.h"
#include "dsl_fhe.h"
#include "dsl_fhe_plan.h"
#include "dsl_ir_image.h"
#include "dsl_opcode.h"
#include "dsl_tensor_fold.h"
#include "pu_info.h"
#include "stab.h"
#include "strtab.h"
#include "symtab.h"
#include "symtab_utils.h"
#include "targ_const.h"
#include "wn.h"

typedef struct {
    DSL_FHE_CONFIG_ID config_id;
    DSL_FHE_ENCRYPTION_DESCRIPTOR_ID ciphertext_descriptor_id;
    UINT32 bootstrap_policy;
} FHE_CONVERSION_CONTEXT;

typedef struct {
    ST_IDX callee_pu_st;
    DSL_PU_SOURCE_IDENTITY_ID caller_identity_id;
    DSL_CALLSITE_METADATA_ID callsite_id;
    UINT32 conv_weight_formal;
    UINT32 conv_bias_formal;
    UINT32 bn_scale_formal;
    UINT32 bn_bias_formal;
    UINT32 bn_mean_formal;
    UINT32 bn_variance_formal;
    DSL_IR_VALUE_ID source_conv_weight;
    DSL_IR_VALUE_ID source_conv_bias;
    DSL_IR_VALUE_ID source_bn_scale;
    DSL_IR_VALUE_ID source_bn_bias;
    DSL_IR_VALUE_ID source_bn_mean;
    DSL_IR_VALUE_ID source_bn_variance;
    DSL_IR_VALUE_ID callee_conv_weight;
    DSL_IR_VALUE_ID callee_conv_bias;
    DSL_IR_VALUE_ID callee_bn_scale;
    DSL_IR_VALUE_ID callee_bn_bias;
    DSL_IR_VALUE_ID callee_bn_mean;
    DSL_IR_VALUE_ID callee_bn_variance;
    DSL_IR_NODE_ID conv_node_id;
    DSL_IR_NODE_ID batch_norm_node_id;
    DSL_IR_VALUE_ID folded_weight_value_id;
    DSL_IR_VALUE_ID folded_bias_value_id;
    TCON_IDX folded_weight_tcon;
    TCON_IDX folded_bias_tcon;
    double epsilon;
    BOOL implicit_zero_bias;
    std::string role_prefix;
} FHE_BN_CONTEXT_FOLD;

typedef struct {
    std::string key;
    TY_IDX ty;
    DSL_IR_VALUE_ID source_value_id;
    UINT64 offset;
    std::vector<unsigned char> bytes;
    std::string checksum;
} FHE_CONVERTED_TENSOR;

static std::vector<FHE_BN_CONTEXT_FOLD> VHO_FHE_bn_context_folds;
static std::deque<FHE_CONVERTED_TENSOR> VHO_FHE_converted_tensors;
static std::string VHO_FHE_converted_payload_temp;
static std::string VHO_FHE_converted_payload_final;
static std::string VHO_FHE_conversion_report_temp;
static std::string VHO_FHE_conversion_report_final;
static BOOL VHO_FHE_artifacts_registered;
static BOOL VHO_FHE_relu_policy_blocked;

typedef struct {
    UINT32 state[8];
    UINT64 bit_count;
    unsigned char block[64];
    UINT32 block_size;
} FHE_SHA256_CONTEXT;

static UINT32
VHO_FHE_SHA256_Rotate (UINT32 value, UINT32 amount)
{
    return (value >> amount) | (value << (32 - amount));
}

static void
VHO_FHE_SHA256_Transform
        (FHE_SHA256_CONTEXT *context,
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
        UINT32 s0 = VHO_FHE_SHA256_Rotate(words[i - 15], 7) ^
                    VHO_FHE_SHA256_Rotate(words[i - 15], 18) ^
                    (words[i - 15] >> 3);
        UINT32 s1 = VHO_FHE_SHA256_Rotate(words[i - 2], 17) ^
                    VHO_FHE_SHA256_Rotate(words[i - 2], 19) ^
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
        UINT32 s1 = VHO_FHE_SHA256_Rotate(e, 6) ^
                    VHO_FHE_SHA256_Rotate(e, 11) ^
                    VHO_FHE_SHA256_Rotate(e, 25);
        UINT32 choice = (e & f) ^ ((~e) & g);
        UINT32 temp1 = h + s1 + choice + constants[i] + words[i];
        UINT32 s0 = VHO_FHE_SHA256_Rotate(a, 2) ^
                    VHO_FHE_SHA256_Rotate(a, 13) ^
                    VHO_FHE_SHA256_Rotate(a, 22);
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
VHO_FHE_SHA256 (const std::vector<unsigned char> &bytes)
{
    FHE_SHA256_CONTEXT context;
    static const UINT32 initial[8] = {
        0x6a09e667U, 0xbb67ae85U, 0x3c6ef372U, 0xa54ff53aU,
        0x510e527fU, 0x9b05688cU, 0x1f83d9abU, 0x5be0cd19U
    };
    memcpy(context.state, initial, sizeof(initial));
    context.bit_count = 0;
    context.block_size = 0;
    for (size_t i = 0; i < bytes.size(); ++i) {
        context.block[context.block_size++] = bytes[i];
        context.bit_count += 8;
        if (context.block_size == 64) {
            VHO_FHE_SHA256_Transform(&context, context.block);
            context.block_size = 0;
        }
    }
    context.block[context.block_size++] = 0x80;
    if (context.block_size > 56) {
        while (context.block_size < 64)
            context.block[context.block_size++] = 0;
        VHO_FHE_SHA256_Transform(&context, context.block);
        context.block_size = 0;
    }
    while (context.block_size < 56)
        context.block[context.block_size++] = 0;
    for (INT32 shift = 56; shift >= 0; shift -= 8)
        context.block[context.block_size++] =
            (unsigned char)(context.bit_count >> shift);
    VHO_FHE_SHA256_Transform(&context, context.block);

    char digest[65];
    for (UINT32 i = 0; i < 8; ++i)
        snprintf(digest + i * 8, 9, "%08x", context.state[i]);
    digest[64] = '\0';
    return digest;
}

static BOOL
VHO_FHE_Semantic_Report (FILE *diagnostic, const char *code,
                         const char *message)
{
    if (diagnostic != NULL)
        fprintf(diagnostic, "%s: %s\n", code, message);
    return FALSE;
}

static BOOL
VHO_FHE_Read_External_Float32
        (ST_IDX owner_pu_st,
         DSL_IR_VALUE_ID value_id,
         std::vector<float> *values,
         DSL_IR_EXTERNAL_TENSOR_REFERENCE *reference,
         FILE *diagnostic)
{
    if (values == NULL || reference == NULL ||
        !DSL_IR_Image_Get_External_Tensor_Reference
             (owner_pu_st, value_id, reference) ||
        strcmp(reference->dtype, "float32") != 0 ||
        reference->byte_length == 0 || reference->byte_length % 4 != 0) {
        return VHO_FHE_Semantic_Report
                   (diagnostic, "CFHECNN-BN-002",
                    "BatchNorm fold requires a typed float32 external tensor");
    }

    FILE *file = fopen(reference->side_file, "rb");
    unsigned char length_bytes[8];
    UINT64 header_length = 0;
    if (file == NULL || fread(length_bytes, 1, sizeof(length_bytes), file) !=
                            sizeof(length_bytes)) {
        if (file != NULL)
            fclose(file);
        return VHO_FHE_Semantic_Report
                   (diagnostic, "CFHECNN-BN-002",
                    "could not open the source tensor payload");
    }
    for (UINT32 i = 0; i < 8; ++i)
        header_length |= (UINT64)length_bytes[i] << (i * 8);
    if (header_length > 64 * 1024 * 1024ULL ||
        reference->byte_offset > ~0ULL - header_length - 8 ||
        fseek(file, (long)(8 + header_length + reference->byte_offset),
              SEEK_SET) != 0) {
        fclose(file);
        return VHO_FHE_Semantic_Report
                   (diagnostic, "CFHECNN-BN-002",
                    "source tensor payload range is invalid");
    }

    std::vector<unsigned char> bytes((size_t)reference->byte_length);
    BOOL read_ok = fread(&bytes[0], 1, bytes.size(), file) == bytes.size();
    fclose(file);
    if (!read_ok || VHO_FHE_SHA256(bytes) != reference->checksum) {
        return VHO_FHE_Semantic_Report
                   (diagnostic, "CFHECNN-BN-002",
                    "source tensor payload checksum does not match WHIRL");
    }
    values->resize(bytes.size() / 4);
    memcpy(&(*values)[0], &bytes[0], bytes.size());
    for (size_t i = 0; i < values->size(); ++i) {
        if (!isfinite((*values)[i]))
            return VHO_FHE_Semantic_Report
                       (diagnostic, "CFHECNN-BN-002",
                        "source tensor payload contains NaN or infinity");
    }
    return TRUE;
}

static BOOL
VHO_FHE_Value_Is_Implicit_Zero
        (DSL_IR_VALUE_ID value_id,
         TY_IDX expected_ty)
{
    DSL_IR_VALUE_RECORD value;
    DSL_IR_NODE_RECORD node;
    DSL_IR_OPCODE_DESCRIPTOR_RECORD descriptor;
    if (!DSL_IR_Image_Get_Value(value_id, &value) || value.ty != expected_ty ||
        value.value_kind != DSL_IR_VALUE_CONSTANT ||
        !DSL_IR_Image_Get_Node(value.producer_node_id, &node) ||
        !DSL_IR_Image_Get_Opcode_Descriptor
             (node.opcode_descriptor_id, &descriptor) ||
        descriptor.logical_operator != OPR_DSLTENSORCONST ||
        descriptor.version != 1)
        return FALSE;
    for (UINT32 i = 0; i < node.attribute_count; ++i) {
        DSL_IR_ATTRIBUTE_RECORD attribute;
        if (DSL_IR_Image_Get_Attribute
                (node.first_attribute_id + i, &attribute) &&
            strcmp(Index_To_Str(attribute.name), "value_kind") == 0 &&
            strcmp(Index_To_Str(attribute.value), "implicit_zero") == 0)
            return TRUE;
    }
    return FALSE;
}

static BOOL
VHO_FHE_BN_Epsilon
        (const DSL_IR_NODE_RECORD *node,
         double *epsilon)
{
    if (node == NULL || epsilon == NULL)
        return FALSE;
    for (UINT32 i = 0; i < node->attribute_count; ++i) {
        DSL_IR_ATTRIBUTE_RECORD attribute;
        if (!DSL_IR_Image_Get_Attribute
                (node->first_attribute_id + i, &attribute))
            return FALSE;
        if (strcmp(Index_To_Str(attribute.name), "attr.epsilon") == 0) {
            char *end = NULL;
            errno = 0;
            *epsilon = strtod(Index_To_Str(attribute.value), &end);
            return errno == 0 && end != NULL && *end == '\0' &&
                   *epsilon > 0.0 && isfinite(*epsilon);
        }
    }
    return FALSE;
}

static BOOL
VHO_FHE_Create_Side_TCON
        (TY_IDX ty,
         const std::string &path,
         UINT64 offset,
         const std::vector<unsigned char> &bytes,
         const std::string &checksum,
         TCON_IDX *tcon)
{
    DSL_TENSOR_TCON_CREATE_INFO info;
    UINT64 checksum_hi = 0;
    UINT64 checksum_lo = 0;
    if (tcon == NULL || bytes.empty() || checksum.size() != 64)
        return FALSE;
    for (UINT32 i = 0; i < 16; ++i) {
        char pair[3] = { checksum[i * 2], checksum[i * 2 + 1], '\0' };
        UINT64 byte = strtoul(pair, NULL, 16);
        if (i < 8)
            checksum_hi = (checksum_hi << 8) | byte;
        else
            checksum_lo = (checksum_lo << 8) | byte;
    }
    memset(&info, 0, sizeof(info));
    info.descriptor_ty = ty;
    info.element_mtype = MTYPE_F4;
    info.element_count = bytes.size() / 4;
    info.logical_bytes = bytes.size();
    info.required_alignment = TY_align(ty) < 4 ? 4 : TY_align(ty);
    info.element_size = 4;
    info.dense_bytes = &bytes[0];
    info.dense_bytes_length = bytes.size();
    info.side_path = path.c_str();
    info.side_path_length = path.size();
    info.byte_offset = offset;
    info.byte_length = bytes.size();
    info.checksum_hi = checksum_hi;
    info.checksum_lo = checksum_lo;
    return DSL_Tensor_TCON_Create_Side_File_Dense(&info, tcon, NULL);
}

static std::string
VHO_FHE_Sanitize_Key (const std::string &text)
{
    std::string result;
    for (size_t i = 0; i < text.size(); ++i) {
        unsigned char character = (unsigned char)text[i];
        result += isalnum(character) ? (char)character : '_';
    }
    return result;
}

static BOOL
VHO_FHE_Register_Converted_Artifacts (const char *source_side_file)
{
    if (VHO_FHE_artifacts_registered)
        return TRUE;
    if (source_side_file == NULL || source_side_file[0] == '\0')
        return FALSE;
    std::string stem(source_side_file);
    const std::string suffix = ".safetensors";
    if (stem.size() >= suffix.size() &&
        stem.compare(stem.size() - suffix.size(), suffix.size(), suffix) == 0)
        stem.erase(stem.size() - suffix.size());
    VHO_FHE_converted_payload_final = stem + ".fhe.safetensors";
    VHO_FHE_converted_payload_temp =
        VHO_FHE_converted_payload_final + ".tmp";
    VHO_FHE_conversion_report_final =
        stem + ".fhe.conversion-report.txt";
    VHO_FHE_conversion_report_temp =
        VHO_FHE_conversion_report_final + ".tmp";
    if (!VHO_FHE_Convert_Checkpoint_Register_Artifact
             (VHO_FHE_converted_payload_temp.c_str(),
              VHO_FHE_converted_payload_final.c_str()) ||
        !VHO_FHE_Convert_Checkpoint_Register_Artifact
             (VHO_FHE_conversion_report_temp.c_str(),
              VHO_FHE_conversion_report_final.c_str()))
        return FALSE;
    VHO_FHE_artifacts_registered = TRUE;
    return TRUE;
}

static BOOL
VHO_FHE_Value_Belongs_To_PU
        (const DSL_IR_VALUE_RECORD *value,
         ST_IDX owner_pu_st)
{
    DSL_IR_VALUE_RECORD owned;
    const char *owner_name;

    if (value == NULL || value->name == STR_IDX_ZERO ||
        ST_IDX_level(owner_pu_st) != GLOBAL_SYMTAB ||
        ST_IDX_index(owner_pu_st) == 0 ||
        ST_IDX_index(owner_pu_st) >= ST_Table_Size(GLOBAL_SYMTAB))
        return FALSE;
    owner_name = ST_name(St_Table[owner_pu_st]);
    return DSL_IR_Image_Find_PU_Value
               (value->st, Index_To_Str(value->name), owner_name, &owned) &&
           owned.id == value->id;
}

static BOOL
VHO_FHE_Validate_Entry_Contracts
        (ST_IDX owner_pu_st,
         FILE *diagnostic)
{
    BOOL valid = TRUE;

    for (UINT32 id = 1; id <= DSL_FHE_Entry_Contract_Count(); ++id) {
        DSL_FHE_ENTRY_CONTRACT_RECORD entry;
        DSL_FHE_COMPILATION_CONFIG_RECORD config;

        if (!DSL_FHE_Get_Entry_Contract(id, &entry))
            return VHO_FHE_Semantic_Report
                       (diagnostic, "CFHE-ENTRY-001",
                        "entry contract lookup failed");
        if (entry.owner_pu_st != owner_pu_st)
            continue;
        if (!DSL_FHE_Get_Compilation_Config(entry.config_id, &config) ||
            config.scheme != DSL_FHE_SCHEME_CKKS) {
            valid = VHO_FHE_Semantic_Report
                        (diagnostic, "CFHE-ENTRY-001",
                         "entry contract must use CKKS config");
        }

        for (UINT32 i = 0; i < entry.entry_value_count; ++i) {
            DSL_FHE_ENTRY_VALUE_RECORD entry_value;
            DSL_FHE_ENCRYPTION_DESCRIPTOR_RECORD descriptor;
            DSL_IR_VALUE_RECORD value;
            DSL_FHE_ENTRY_VALUE_ID entry_value_id =
                entry.first_entry_value_id + i;

            if (!DSL_FHE_Get_Entry_Value(entry_value_id, &entry_value) ||
                !DSL_IR_Image_Get_Value(entry_value.value_id, &value) ||
                !VHO_FHE_Value_Belongs_To_PU(&value, owner_pu_st) ||
                !DSL_FHE_Get_Encryption_Descriptor
                    (entry_value.encryption_descriptor_id, &descriptor)) {
                valid = VHO_FHE_Semantic_Report
                            (diagnostic, "CFHE-ENTRY-002",
                             "entry value binding is not resolvable");
                continue;
            }

            if (entry_value.role == DSL_FHE_ENTRY_VALUE_INPUT ||
                entry_value.role == DSL_FHE_ENTRY_VALUE_OUTPUT) {
                if (entry_value.value_class !=
                        DSL_FHE_VALUE_CLASS_CIPHERTEXT ||
                    descriptor.scheme != DSL_FHE_SCHEME_CKKS ||
                    descriptor.value_class !=
                        DSL_FHE_VALUE_CLASS_CIPHERTEXT) {
                    valid = VHO_FHE_Semantic_Report
                                (diagnostic, "CFHE-ENTRY-002",
                                 "FHE input/output must be CKKS ciphertext");
                }
            } else if (entry_value.role == DSL_FHE_ENTRY_VALUE_PARAMETER) {
                if (entry_value.value_class !=
                        DSL_FHE_VALUE_CLASS_ENCODED_PLAINTEXT &&
                    entry_value.value_class != DSL_FHE_VALUE_CLASS_CLEAR) {
                    valid = VHO_FHE_Semantic_Report
                                (diagnostic, "CFHE-ENTRY-002",
                                 "FHE parameter must be plaintext or encoded");
                }
            }
        }
    }

    return valid;
}

static BOOL
VHO_FHE_Select_Default_Context
        (ST_IDX owner_pu_st,
         FHE_CONVERSION_CONTEXT *context,
         FILE *diagnostic)
{
    DSL_FHE_COMPILATION_CONFIG_RECORD config;
    DSL_FHE_ENCRYPTION_DESCRIPTOR_RECORD descriptor;
    BOOL saw_entry = FALSE;

    if (context == NULL)
        return FALSE;
    memset(context, 0, sizeof(*context));

    for (UINT32 id = 1; id <= DSL_FHE_Entry_Contract_Count(); ++id) {
        DSL_FHE_ENTRY_CONTRACT_RECORD entry;
        if (!DSL_FHE_Get_Entry_Contract(id, &entry))
            return VHO_FHE_Semantic_Report
                       (diagnostic, "CFHE-ENTRY-001",
                        "entry contract lookup failed");
        if (entry.owner_pu_st != owner_pu_st)
            continue;
        saw_entry = TRUE;
        context->config_id = entry.config_id;
        break;
    }

    if (context->config_id == DSL_FHE_CONFIG_INVALID_ID) {
        for (UINT32 id = 1; id <= DSL_FHE_Config_Count(); ++id) {
            if (DSL_FHE_Get_Compilation_Config(id, &config) &&
                config.scheme == DSL_FHE_SCHEME_CKKS) {
                context->config_id = id;
                break;
            }
        }
    }

    if (context->config_id == DSL_FHE_CONFIG_INVALID_ID ||
        !DSL_FHE_Get_Compilation_Config(context->config_id, &config) ||
        config.scheme != DSL_FHE_SCHEME_CKKS) {
        return VHO_FHE_Semantic_Report
                   (diagnostic, "CFHE-ENTRY-001",
                    "FHE conversion requires a CKKS compilation config");
    }
    context->bootstrap_policy = config.bootstrap_policy;

    for (UINT32 id = 1; id <= DSL_FHE_Encryption_Descriptor_Count(); ++id) {
        if (DSL_FHE_Get_Encryption_Descriptor(id, &descriptor) &&
            descriptor.config_id == context->config_id &&
            descriptor.scheme == DSL_FHE_SCHEME_CKKS &&
            descriptor.value_class == DSL_FHE_VALUE_CLASS_CIPHERTEXT) {
            context->ciphertext_descriptor_id = id;
            break;
        }
    }

    if (context->ciphertext_descriptor_id ==
        DSL_FHE_ENCRYPTION_DESCRIPTOR_INVALID_ID) {
        return VHO_FHE_Semantic_Report
                   (diagnostic, "CFHE-ENTRY-001",
                    "FHE conversion requires a CKKS ciphertext descriptor");
    }

    (void)saw_entry;
    return TRUE;
}

static BOOL
VHO_FHE_Bootstrap_Policy_Allows_Relu
        (UINT32 policy,
         FILE *diagnostic)
{
    if (policy == DSL_FHE_BOOTSTRAP_AUTO ||
        policy == DSL_FHE_BOOTSTRAP_ON)
        return TRUE;
    if (policy == DSL_FHE_BOOTSTRAP_MANUAL)
        return VHO_FHE_Semantic_Report
                   (diagnostic, "CFHECNN-RELU-001",
                    "manual bootstrap requires an explicit ReLU boundary");
    if (policy == DSL_FHE_BOOTSTRAP_OFF)
        return VHO_FHE_Semantic_Report
                   (diagnostic, "CFHECNN-RELU-001",
                    "bootstrap=off cannot lower surviving common.relu");
    return VHO_FHE_Semantic_Report
               (diagnostic, "CFHECNN-RELU-001",
                "common.relu requires a known bootstrap policy");
}

static BOOL
VHO_FHE_Create_Relu_Approximation
        (const FHE_CONVERSION_CONTEXT *context,
         TY_IDX result_ty,
         DSL_FHE_APPROXIMATION_CONTRACT_ID *approximation_id,
         FILE *diagnostic)
{
    (void)context;
    (void)result_ty;
    if (approximation_id != NULL)
        *approximation_id = DSL_FHE_APPROXIMATION_CONTRACT_INVALID_ID;
    return VHO_FHE_Semantic_Report
               (diagnostic, "CFHECNN-RELU-002",
                "common.relu degree-3 coefficient policy is not approved");
}

static BOOL
VHO_FHE_Static_Element_Count (TY_IDX tensor_ty, UINT64 *element_count)
{
    const char *shape;
    const char *cursor;
    UINT32 dimensions = 0;
    UINT64 count = 1;
    INT32 rank;

    if (element_count != NULL)
        *element_count = 0;
    if (tensor_ty == TY_IDX_ZERO || element_count == NULL ||
        !TY_is_tensor_extension(tensor_ty))
        return FALSE;
    shape = TY_tensor_attribute(tensor_ty, TY_TENSOR_SCHEMA_SHAPE);
    rank = TY_tensor_rank(tensor_ty);
    if (shape == NULL || rank < 0)
        return FALSE;

    cursor = shape;
    while (isspace((unsigned char)*cursor))
        ++cursor;
    if (*cursor++ != '[')
        return FALSE;
    while (TRUE) {
        while (isspace((unsigned char)*cursor))
            ++cursor;
        if (*cursor == ']') {
            ++cursor;
            break;
        }
        errno = 0;
        char *end;
        unsigned long long dimension = strtoull(cursor, &end, 10);
        if (errno == ERANGE || end == cursor || dimension == 0 ||
            count > ~0ULL / dimension)
            return FALSE;
        count *= dimension;
        ++dimensions;
        cursor = end;
        while (isspace((unsigned char)*cursor))
            ++cursor;
        if (*cursor == ',') {
            ++cursor;
            continue;
        }
        if (*cursor != ']')
            return FALSE;
    }
    while (isspace((unsigned char)*cursor))
        ++cursor;
    if (*cursor != '\0' || dimensions != (UINT32)rank)
        return FALSE;
    *element_count = count;
    return TRUE;
}

static BOOL
VHO_FHE_Create_Zero_Tensor_TCON
        (TY_IDX tensor_ty,
         TCON_IDX *tcon_idx)
{
    DSL_TENSOR_TCON_CREATE_INFO info;
    TY_IDX element_ty;
    UINT64 element_count;
    UINT32 element_size;

    if (tcon_idx != NULL)
        *tcon_idx = TCON_IDX_ZERO;
    if (tensor_ty == TY_IDX_ZERO || tcon_idx == NULL ||
        !VHO_FHE_Static_Element_Count(tensor_ty, &element_count))
        return FALSE;
    element_ty = TY_tensor_element_ty(tensor_ty);
    if (element_ty == TY_IDX_ZERO)
        return FALSE;
    element_size = TY_size(element_ty);
    if (element_size == 0 || element_count > ~0ULL / element_size)
        return FALSE;

    memset(&info, 0, sizeof(info));
    info.descriptor_ty = tensor_ty;
    info.scalar_tcon = Enter_tcon
                           (Host_To_Targ_Float(TY_mtype(element_ty), 0.0));
    info.element_mtype = TY_mtype(element_ty);
    info.element_count = element_count;
    info.logical_bytes = element_count * element_size;
    info.required_alignment = TY_align(tensor_ty);
    if (info.required_alignment < element_size)
        info.required_alignment = element_size;
    info.element_size = element_size;
    return DSL_Tensor_TCON_Create_Zero(&info, tcon_idx, NULL);
}

static BOOL
VHO_FHE_Node_Operand_Value
        (const DSL_IR_NODE_RECORD *node,
         UINT32 ordinal,
         DSL_IR_VALUE_RECORD *value)
{
    DSL_IR_VALUE_REFERENCE_RECORD reference;

    return node != NULL && value != NULL && ordinal < node->operand_count &&
           DSL_IR_Image_Get_Value_Reference
               (node->first_operand_reference_id + ordinal, &reference) &&
           reference.ordinal == ordinal &&
           DSL_IR_Image_Get_Value(reference.value_id, value);
}

static BOOL
VHO_FHE_Append_Folded_Tensor
        (const std::string &key,
         TY_IDX ty,
         DSL_IR_VALUE_ID source_value_id,
         const std::vector<float> &values,
         TCON_IDX *tcon,
         FHE_CONVERTED_TENSOR **tensor)
{
    FHE_CONVERTED_TENSOR output;
    output.key = VHO_FHE_Sanitize_Key(key);
    output.ty = ty;
    output.source_value_id = source_value_id;
    output.offset = VHO_FHE_converted_tensors.empty() ? 0 :
        VHO_FHE_converted_tensors.back().offset +
        VHO_FHE_converted_tensors.back().bytes.size();
    output.bytes.resize(values.size() * sizeof(float));
    if (!output.bytes.empty())
        memcpy(&output.bytes[0], &values[0], output.bytes.size());
    output.checksum = VHO_FHE_SHA256(output.bytes);
    if (!VHO_FHE_Create_Side_TCON
             (ty, VHO_FHE_converted_payload_final, output.offset,
              output.bytes, output.checksum, tcon))
        return FALSE;
    VHO_FHE_converted_tensors.push_back(output);
    if (tensor != NULL)
        *tensor = &VHO_FHE_converted_tensors.back();
    return TRUE;
}

static BOOL
VHO_FHE_Compute_BN_Fold
        (ST_IDX source_owner_pu_st,
         DSL_IR_VALUE_ID conv_weight_id,
         DSL_IR_VALUE_ID conv_bias_id,
         DSL_IR_VALUE_ID bn_scale_id,
         DSL_IR_VALUE_ID bn_bias_id,
         DSL_IR_VALUE_ID bn_mean_id,
         DSL_IR_VALUE_ID bn_variance_id,
         double epsilon,
         const std::string &key_prefix,
         TCON_IDX *folded_weight_tcon,
         TCON_IDX *folded_bias_tcon,
         FHE_CONVERTED_TENSOR **folded_weight,
         FHE_CONVERTED_TENSOR **folded_bias,
         BOOL *implicit_zero_bias,
         FILE *diagnostic)
{
    DSL_IR_EXTERNAL_TENSOR_REFERENCE weight_reference;
    DSL_IR_EXTERNAL_TENSOR_REFERENCE bias_reference;
    DSL_IR_EXTERNAL_TENSOR_REFERENCE scale_reference;
    DSL_IR_EXTERNAL_TENSOR_REFERENCE bn_bias_reference;
    DSL_IR_EXTERNAL_TENSOR_REFERENCE mean_reference;
    DSL_IR_EXTERNAL_TENSOR_REFERENCE variance_reference;
    DSL_IR_VALUE_RECORD conv_bias_value;
    std::vector<float> weights;
    std::vector<float> conv_bias;
    std::vector<float> scale;
    std::vector<float> bn_bias;
    std::vector<float> mean;
    std::vector<float> variance;

    if (!VHO_FHE_Read_External_Float32
             (source_owner_pu_st, conv_weight_id, &weights,
              &weight_reference, diagnostic) ||
        !VHO_FHE_Read_External_Float32
             (source_owner_pu_st, bn_scale_id, &scale,
              &scale_reference, diagnostic) ||
        !VHO_FHE_Read_External_Float32
             (source_owner_pu_st, bn_bias_id, &bn_bias,
              &bn_bias_reference, diagnostic) ||
        !VHO_FHE_Read_External_Float32
             (source_owner_pu_st, bn_mean_id, &mean,
              &mean_reference, diagnostic) ||
        !VHO_FHE_Read_External_Float32
             (source_owner_pu_st, bn_variance_id, &variance,
              &variance_reference, diagnostic))
        return FALSE;
    if (!VHO_FHE_Register_Converted_Artifacts(weight_reference.side_file))
        return VHO_FHE_Semantic_Report
                   (diagnostic, "CFHE-CHECKPOINT-005",
                    "could not register converted FHE artifacts");

    if (!DSL_IR_Image_Get_Value(conv_bias_id, &conv_bias_value))
        return VHO_FHE_Semantic_Report
                   (diagnostic, "CFHECNN-BN-001",
                    "convolution bias value is not resolvable");
    *implicit_zero_bias = VHO_FHE_Value_Is_Implicit_Zero
                              (conv_bias_id, conv_bias_value.ty);
    if (*implicit_zero_bias) {
        conv_bias.assign(scale.size(), 0.0f);
        memset(&bias_reference, 0, sizeof(bias_reference));
    } else if (!VHO_FHE_Read_External_Float32
                    (source_owner_pu_st, conv_bias_id, &conv_bias,
                     &bias_reference, diagnostic)) {
        return FALSE;
    }

    if (scale.empty() || bn_bias.size() != scale.size() ||
        mean.size() != scale.size() || variance.size() != scale.size() ||
        conv_bias.size() != scale.size() ||
        weights.size() % scale.size() != 0 ||
        weight_reference.layout == NULL ||
        strcmp(weight_reference.layout, "OIHW") != 0) {
        return VHO_FHE_Semantic_Report
                   (diagnostic, "CFHECNN-BN-002",
                    "BatchNorm channels or OIHW convolution layout mismatch");
    }

    std::vector<float> folded_weights(weights.size());
    std::vector<float> folded_biases(scale.size());
    const size_t channel_stride = weights.size() / scale.size();
    for (size_t channel = 0; channel < scale.size(); ++channel) {
        double denominator = (double)variance[channel] + epsilon;
        if (!(denominator > 0.0) || !isfinite(denominator))
            return VHO_FHE_Semantic_Report
                       (diagnostic, "CFHECNN-BN-002",
                        "BatchNorm variance and epsilon are not foldable");
        double factor = (double)scale[channel] / sqrt(denominator);
        double bias_value = (double)bn_bias[channel] +
                            ((double)conv_bias[channel] -
                             (double)mean[channel]) * factor;
        if (!isfinite(factor) || !isfinite(bias_value))
            return VHO_FHE_Semantic_Report
                       (diagnostic, "CFHECNN-BN-002",
                        "BatchNorm fold produced a non-finite value");
        folded_biases[channel] = (float)bias_value;
        for (size_t item = 0; item < channel_stride; ++item)
            folded_weights[channel * channel_stride + item] =
                (float)((double)weights[channel * channel_stride + item] *
                        factor);
    }

    if (!VHO_FHE_Append_Folded_Tensor
             (key_prefix + ".folded_weight", weight_reference.descriptor_ty,
              conv_weight_id, folded_weights, folded_weight_tcon,
              folded_weight) ||
        !VHO_FHE_Append_Folded_Tensor
             (key_prefix + ".folded_bias", conv_bias_value.ty,
              conv_bias_id, folded_biases, folded_bias_tcon, folded_bias)) {
        return VHO_FHE_Semantic_Report
                   (diagnostic, "CFHECNN-BN-002",
                    "could not stage folded tensor payload evidence");
    }
    return TRUE;
}

static BOOL
VHO_FHE_Call_Role
        (DSL_CALLSITE_METADATA_ID callsite_id,
         const char *role,
         DSL_CALL_ARGUMENT_RECORD *record)
{
    for (UINT32 id = 1; id <= DSL_Call_ABI_Image_Argument_Count(); ++id) {
        DSL_CALL_ARGUMENT_RECORD candidate;
        if (DSL_Call_ABI_Image_Get_Argument(id, &candidate) &&
            candidate.callsite_id == callsite_id &&
            candidate.semantic_role != STR_IDX_ZERO &&
            strcmp(Index_To_Str(candidate.semantic_role), role) == 0) {
            if (record != NULL)
                *record = candidate;
            return TRUE;
        }
    }
    return FALSE;
}

static BOOL
VHO_FHE_Resolve_Callee_Formal
        (ST_IDX callee_pu_st,
         const DSL_CALL_ARGUMENT_RECORD *argument,
         DSL_IR_VALUE_RECORD *formal_value,
         FILE *diagnostic)
{
    DSL_PU_FORMAL_RECORD formal;
    DSL_IR_VALUE_RECORD actual_value;

    if (argument == NULL || formal_value == NULL ||
        !DSL_PU_Interface_Image_Find_Formal
             (callee_pu_st, argument->callee_formal_ordinal, &formal) ||
        !DSL_IR_Image_Get_Value(formal.formal_value_id, formal_value) ||
        !DSL_IR_Image_Get_Value(argument->argument_value_id, &actual_value) ||
        formal.owner_pu_st != callee_pu_st ||
        formal.formal_value_id != formal_value->id ||
        formal.formal_st != formal_value->st ||
        formal.formal_ty != formal_value->ty ||
        formal.formal_ty != actual_value.ty) {
        return VHO_FHE_Semantic_Report
                   (diagnostic, "CFHECNN-BN-001",
                    "call argument does not resolve to its exact callee value");
    }
    return TRUE;
}

static BOOL
VHO_FHE_Resolve_Callee_BN_Pair
        (FHE_BN_CONTEXT_FOLD *fold,
         FILE *diagnostic)
{
    DSL_IR_VALUE_RECORD conv_weight;
    DSL_IR_VALUE_RECORD conv_bias;
    DSL_IR_VALUE_RECORD bn_scale;
    DSL_IR_VALUE_RECORD bn_bias;
    DSL_IR_VALUE_RECORD bn_mean;
    DSL_IR_VALUE_RECORD bn_variance;
    DSL_IR_VALUE_RECORD conv_result;
    DSL_IR_NODE_RECORD matched_conv;
    UINT32 conv_matches = 0;
    UINT32 bn_matches = 0;

    if (fold == NULL ||
        !DSL_IR_Image_Get_Value(fold->callee_conv_weight, &conv_weight) ||
        !DSL_IR_Image_Get_Value(fold->callee_conv_bias, &conv_bias) ||
        !DSL_IR_Image_Get_Value(fold->callee_bn_scale, &bn_scale) ||
        !DSL_IR_Image_Get_Value(fold->callee_bn_bias, &bn_bias) ||
        !DSL_IR_Image_Get_Value(fold->callee_bn_mean, &bn_mean) ||
        !DSL_IR_Image_Get_Value(fold->callee_bn_variance, &bn_variance))
        return FALSE;

    for (UINT32 id = 1; id <= DSL_IR_Image_Node_Count(); ++id) {
        DSL_IR_NODE_RECORD node;
        DSL_IR_OPCODE_DESCRIPTOR_RECORD descriptor;
        DSL_IR_VALUE_RECORD result;
        DSL_IR_VALUE_RECORD weight;
        DSL_IR_VALUE_RECORD bias;
        if (!DSL_IR_Image_Get_Node(id, &node) ||
            !DSL_IR_Image_Get_Opcode_Descriptor
                (node.opcode_descriptor_id, &descriptor) ||
            descriptor.logical_operator != OPR_DSLCONV2D ||
            descriptor.version != 2 || node.operand_count < 3 ||
            !DSL_IR_Image_Get_Value(node.result_value_id, &result) ||
            !VHO_FHE_Value_Belongs_To_PU(&result, fold->callee_pu_st) ||
            !VHO_FHE_Node_Operand_Value(&node, 1, &weight) ||
            !VHO_FHE_Node_Operand_Value(&node, 2, &bias) ||
            weight.id != conv_weight.id || bias.id != conv_bias.id)
            continue;
        matched_conv = node;
        conv_result = result;
        ++conv_matches;
    }
    if (conv_matches != 1)
        return VHO_FHE_Semantic_Report
                   (diagnostic, "CFHECNN-BN-001",
                    "callee formal roles do not identify one convolution");

    for (UINT32 id = 1; id <= DSL_IR_Image_Node_Count(); ++id) {
        DSL_IR_NODE_RECORD node;
        DSL_IR_OPCODE_DESCRIPTOR_RECORD descriptor;
        DSL_IR_VALUE_RECORD result;
        DSL_IR_VALUE_RECORD operand[5];
        if (!DSL_IR_Image_Get_Node(id, &node) ||
            !DSL_IR_Image_Get_Opcode_Descriptor
                (node.opcode_descriptor_id, &descriptor) ||
            descriptor.logical_operator != OPR_DSLBATCHNORMINFER ||
            descriptor.version != 2 || node.operand_count < 5 ||
            !DSL_IR_Image_Get_Value(node.result_value_id, &result) ||
            !VHO_FHE_Value_Belongs_To_PU(&result, fold->callee_pu_st))
            continue;
        BOOL operands_match = TRUE;
        for (UINT32 ordinal = 0; ordinal < 5; ++ordinal)
            operands_match = operands_match &&
                VHO_FHE_Node_Operand_Value(&node, ordinal, &operand[ordinal]);
        if (!operands_match || operand[0].id != conv_result.id ||
            operand[1].id != bn_scale.id || operand[2].id != bn_bias.id ||
            operand[3].id != bn_mean.id ||
            operand[4].id != bn_variance.id)
            continue;
        if (!VHO_FHE_BN_Epsilon(&node, &fold->epsilon))
            return VHO_FHE_Semantic_Report
                       (diagnostic, "CFHECNN-BN-002",
                        "BatchNorm epsilon is missing or invalid");
        fold->batch_norm_node_id = node.id;
        ++bn_matches;
    }
    if (bn_matches != 1)
        return VHO_FHE_Semantic_Report
                   (diagnostic, "CFHECNN-BN-001",
                    "callee formal roles do not identify one BatchNorm");
    fold->conv_node_id = matched_conv.id;
    return TRUE;
}

static void
VHO_FHE_Find_Value_Definition
        (WN *node,
         WN *containing_block,
         ST_IDX owner_pu_st,
         DSL_IR_VALUE_ID value_id,
         WN **definition,
         WN **definition_block);

static BOOL
VHO_FHE_Prepare_Call_Context_Folds
        (ST_IDX owner_pu_st,
         WN *tree,
         std::vector<DSL_IR_EXTERNAL_TENSOR_MATERIALIZATION_REQUEST> *requests,
         FILE *diagnostic)
{
    static const char *conv_roles[] = {
        "cnn.basic_block.conv1",
        "cnn.basic_block.conv2",
        "cnn.basic_block.downsample.conv"
    };
    static const char *bn_roles[] = {
        "cnn.basic_block.bn1",
        "cnn.basic_block.bn2",
        "cnn.basic_block.downsample.bn"
    };
    DSL_PU_SOURCE_IDENTITY_RECORD caller_identity;
    WN *body = WN_func_body(tree);
    size_t first_context = VHO_FHE_bn_context_folds.size();
    if (body == NULL || !DSL_Call_Image_Find_PU_Identity
                            (owner_pu_st, &caller_identity))
        return FALSE;

    for (UINT32 callsite_id = 1;
         callsite_id <= DSL_Call_Image_Callsite_Count(); ++callsite_id) {
        DSL_CALLSITE_METADATA_RECORD callsite;
        if (!DSL_Call_Image_Get_Callsite(callsite_id, &callsite) ||
            callsite.owner_pu_st != owner_pu_st)
            continue;
        WN *call = const_cast<WN *>(DSL_Call_Image_Get_Call_WN(callsite_id));
        if (call == NULL)
            return VHO_FHE_Semantic_Report
                       (diagnostic, "CFHECNN-BN-001",
                        "callsite has no active WHIRL call");
        for (UINT32 group = 0; group < 3; ++group) {
            std::string weight_role = std::string(conv_roles[group]) +
                                      ".weight";
            DSL_CALL_ARGUMENT_RECORD weight;
            if (!VHO_FHE_Call_Role(callsite_id, weight_role.c_str(), &weight))
                continue;
            std::string bias_role = std::string(conv_roles[group]) + ".bias";
            std::string scale_role = std::string(bn_roles[group]) + ".scale";
            std::string bn_bias_role = std::string(bn_roles[group]) + ".bias";
            std::string mean_role = std::string(bn_roles[group]) + ".mean";
            std::string variance_role = std::string(bn_roles[group]) +
                                        ".variance";
            DSL_CALL_ARGUMENT_RECORD bias;
            DSL_CALL_ARGUMENT_RECORD scale;
            DSL_CALL_ARGUMENT_RECORD bn_bias;
            DSL_CALL_ARGUMENT_RECORD mean;
            DSL_CALL_ARGUMENT_RECORD variance;
            DSL_IR_VALUE_RECORD callee_weight;
            DSL_IR_VALUE_RECORD callee_bias;
            DSL_IR_VALUE_RECORD callee_scale;
            DSL_IR_VALUE_RECORD callee_bn_bias;
            DSL_IR_VALUE_RECORD callee_mean;
            DSL_IR_VALUE_RECORD callee_variance;
            if (!VHO_FHE_Call_Role(callsite_id, bias_role.c_str(), &bias) ||
                !VHO_FHE_Call_Role(callsite_id, scale_role.c_str(), &scale) ||
                !VHO_FHE_Call_Role
                    (callsite_id, bn_bias_role.c_str(), &bn_bias) ||
                !VHO_FHE_Call_Role(callsite_id, mean_role.c_str(), &mean) ||
                !VHO_FHE_Call_Role
                    (callsite_id, variance_role.c_str(), &variance)) {
                return VHO_FHE_Semantic_Report
                           (diagnostic, "CFHECNN-BN-001",
                            "callsite BatchNorm argument roles are incomplete");
            }
            if (!VHO_FHE_Resolve_Callee_Formal
                    (callsite.callee_pu_st, &weight, &callee_weight,
                     diagnostic) ||
                !VHO_FHE_Resolve_Callee_Formal
                    (callsite.callee_pu_st, &bias, &callee_bias,
                     diagnostic) ||
                !VHO_FHE_Resolve_Callee_Formal
                    (callsite.callee_pu_st, &scale, &callee_scale,
                     diagnostic) ||
                !VHO_FHE_Resolve_Callee_Formal
                    (callsite.callee_pu_st, &bn_bias, &callee_bn_bias,
                     diagnostic) ||
                !VHO_FHE_Resolve_Callee_Formal
                    (callsite.callee_pu_st, &mean, &callee_mean,
                     diagnostic) ||
                !VHO_FHE_Resolve_Callee_Formal
                    (callsite.callee_pu_st, &variance, &callee_variance,
                     diagnostic))
                return FALSE;

            FHE_BN_CONTEXT_FOLD fold;
            fold.callee_pu_st = callsite.callee_pu_st;
            fold.caller_identity_id = caller_identity.id;
            fold.callsite_id = callsite_id;
            fold.conv_weight_formal = weight.callee_formal_ordinal;
            fold.conv_bias_formal = bias.callee_formal_ordinal;
            fold.bn_scale_formal = scale.callee_formal_ordinal;
            fold.bn_bias_formal = bn_bias.callee_formal_ordinal;
            fold.bn_mean_formal = mean.callee_formal_ordinal;
            fold.bn_variance_formal = variance.callee_formal_ordinal;
            fold.source_conv_weight = weight.argument_value_id;
            fold.source_conv_bias = bias.argument_value_id;
            fold.source_bn_scale = scale.argument_value_id;
            fold.source_bn_bias = bn_bias.argument_value_id;
            fold.source_bn_mean = mean.argument_value_id;
            fold.source_bn_variance = variance.argument_value_id;
            fold.callee_conv_weight = callee_weight.id;
            fold.callee_conv_bias = callee_bias.id;
            fold.callee_bn_scale = callee_scale.id;
            fold.callee_bn_bias = callee_bn_bias.id;
            fold.callee_bn_mean = callee_mean.id;
            fold.callee_bn_variance = callee_variance.id;
            fold.conv_node_id = DSL_IR_NODE_INVALID_ID;
            fold.batch_norm_node_id = DSL_IR_NODE_INVALID_ID;
            fold.folded_weight_value_id = DSL_IR_VALUE_INVALID_ID;
            fold.folded_bias_value_id = DSL_IR_VALUE_INVALID_ID;
            fold.role_prefix = conv_roles[group];
            if (!VHO_FHE_Resolve_Callee_BN_Pair(&fold, diagnostic))
                return FALSE;

            FHE_CONVERTED_TENSOR *folded_weight = NULL;
            FHE_CONVERTED_TENSOR *folded_bias = NULL;
            std::ostringstream key;
            key << "call" << callsite_id << "." << conv_roles[group];
            if (!VHO_FHE_Compute_BN_Fold
                    (owner_pu_st, fold.source_conv_weight,
                     fold.source_conv_bias, fold.source_bn_scale,
                     fold.source_bn_bias, fold.source_bn_mean,
                     fold.source_bn_variance, fold.epsilon, key.str(),
                     &fold.folded_weight_tcon, &fold.folded_bias_tcon,
                     &folded_weight, &folded_bias,
                     &fold.implicit_zero_bias, diagnostic))
                return FALSE;

            DSL_IR_EXTERNAL_TENSOR_MATERIALIZATION_REQUEST weight_request;
            DSL_IR_External_Tensor_Materialization_Request_Init
                (&weight_request);
            weight_request.name = folded_weight->key.c_str();
            weight_request.descriptor_ty = folded_weight->ty;
            weight_request.tensor_tcon = fold.folded_weight_tcon;
            weight_request.source_value_id = fold.source_conv_weight;
            weight_request.insertion_block = body;
            weight_request.insert_before = call;
            weight_request.call = call;
            weight_request.actual_ordinal = weight.actual_ordinal;
            weight_request.expected_actual_value_id = fold.source_conv_weight;
            weight_request.source_position = WN_Get_Linenum(call);
            weight_request.storage_format = "safetensors";
            weight_request.side_file = VHO_FHE_converted_payload_final.c_str();
            weight_request.tensor_key = folded_weight->key.c_str();
            weight_request.byte_offset = folded_weight->offset;
            weight_request.byte_length = folded_weight->bytes.size();
            weight_request.checksum = folded_weight->checksum.c_str();
            requests->push_back(weight_request);

            DSL_IR_EXTERNAL_TENSOR_MATERIALIZATION_REQUEST bias_request;
            DSL_IR_External_Tensor_Materialization_Request_Init(&bias_request);
            bias_request.name = folded_bias->key.c_str();
            bias_request.descriptor_ty = folded_bias->ty;
            bias_request.tensor_tcon = fold.folded_bias_tcon;
            bias_request.source_value_id = fold.source_conv_bias;
            bias_request.insertion_block = body;
            bias_request.insert_before = call;
            bias_request.call = call;
            bias_request.actual_ordinal = bias.actual_ordinal;
            bias_request.expected_actual_value_id = fold.source_conv_bias;
            bias_request.source_position = WN_Get_Linenum(call);
            bias_request.storage_format = "safetensors";
            bias_request.side_file = VHO_FHE_converted_payload_final.c_str();
            bias_request.tensor_key = folded_bias->key.c_str();
            bias_request.byte_offset = folded_bias->offset;
            bias_request.byte_length = folded_bias->bytes.size();
            bias_request.checksum = folded_bias->checksum.c_str();
            bias_request.source_policy =
                DSL_IR_MATERIALIZE_SOURCE_EXTERNAL_OR_IMPLICIT_ZERO;
            requests->push_back(bias_request);
            VHO_FHE_bn_context_folds.push_back(fold);
        }
    }
    if (requests->empty())
        return TRUE;
    std::vector<DSL_IR_EXTERNAL_TENSOR_MATERIALIZATION_RESULT> results
        (requests->size());
    if (!DSL_IR_Materialize_External_Tensor_Values
             (owner_pu_st, &(*requests)[0], requests->size(), &results[0])) {
        return VHO_FHE_Semantic_Report
                   (diagnostic, "CFHECNN-BN-003",
                    "atomic folded call-operand materialization failed");
    }
    size_t context_count = VHO_FHE_bn_context_folds.size() - first_context;
    if (results.size() != context_count * 2)
        return VHO_FHE_Semantic_Report
                   (diagnostic, "CFHECNN-BN-003",
                    "folded call-operand result count is inconsistent");
    for (size_t i = 0; i < context_count; ++i) {
        VHO_FHE_bn_context_folds[first_context + i].folded_weight_value_id =
            results[i * 2].value_id;
        VHO_FHE_bn_context_folds[first_context + i].folded_bias_value_id =
            results[i * 2 + 1].value_id;
    }
    return TRUE;
}

static BOOL
VHO_FHE_PU_Is_FHE_Entry (ST_IDX owner_pu_st)
{
    for (UINT32 id = 1; id <= DSL_FHE_Entry_Contract_Count(); ++id) {
        DSL_FHE_ENTRY_CONTRACT_RECORD entry;
        if (DSL_FHE_Get_Entry_Contract(id, &entry) &&
            entry.owner_pu_st == owner_pu_st)
            return TRUE;
    }
    return FALSE;
}

static BOOL
VHO_FHE_Copy_Node_Attributes
        (const DSL_IR_NODE_RECORD *node,
         std::vector<DSL_IR_ATTRIBUTE_RECORD> *attributes)
{
    if (node == NULL || attributes == NULL)
        return FALSE;
    attributes->clear();
    attributes->reserve(node->attribute_count);
    for (UINT32 i = 0; i < node->attribute_count; ++i) {
        DSL_IR_ATTRIBUTE_RECORD attribute;
        if (!DSL_IR_Image_Get_Attribute
                 (node->first_attribute_id + i, &attribute))
            return FALSE;
        attributes->push_back(attribute);
    }
    return TRUE;
}

static BOOL
VHO_FHE_Rewrite_Entry_Conv_Operands
        (ST_IDX owner_pu_st,
         WN *tree,
         const DSL_IR_NODE_RECORD *conv_node,
         DSL_IR_VALUE_ID folded_weight_value_id,
         DSL_IR_VALUE_ID folded_bias_value_id,
         FILE *diagnostic)
{
    DSL_IR_VALUE_RECORD conv_result;
    DSL_IR_VALUE_RECORD weight;
    DSL_IR_VALUE_RECORD bias;
    WN *definition = NULL;
    WN *definition_block = NULL;
    std::vector<DSL_IR_ATTRIBUTE_RECORD> attributes;

    if (conv_node == NULL || conv_node->operand_count != 3 ||
        !DSL_IR_Image_Get_Value(conv_node->result_value_id, &conv_result) ||
        !DSL_IR_Image_Get_Value(folded_weight_value_id, &weight) ||
        !DSL_IR_Image_Get_Value(folded_bias_value_id, &bias) ||
        !VHO_FHE_Copy_Node_Attributes(conv_node, &attributes))
        return FALSE;
    VHO_FHE_Find_Value_Definition
        (tree, NULL, owner_pu_st, conv_result.id,
         &definition, &definition_block);
    if (definition == NULL || definition_block == NULL)
        return VHO_FHE_Semantic_Report
                   (diagnostic, "CFHECNN-BN-003",
                    "entry convolution definition is not resolvable");

    const WN *old_expression = WN_kid0(definition);
    if (old_expression == NULL || WN_kid_count(old_expression) != 3)
        return FALSE;
    WN *weight_load = WN_CreateLdid
                          (OPR_LDID, MTYPE_M, MTYPE_M, 0,
                           weight.st, weight.ty);
    WN *bias_load = WN_CreateLdid
                        (OPR_LDID, MTYPE_M, MTYPE_M, 0,
                         bias.st, bias.ty);
    const WN *templates[3] = {
        WN_kid(old_expression, 0), weight_load, bias_load
    };
    DSL_IR_VALUE_REFERENCE_RECORD input_reference;
    DSL_IR_VALUE_ID operand_ids[3];
    if (!DSL_IR_Image_Get_Value_Reference
             (conv_node->first_operand_reference_id, &input_reference))
        return FALSE;
    operand_ids[0] = input_reference.value_id;
    operand_ids[1] = folded_weight_value_id;
    operand_ids[2] = folded_bias_value_id;

    DSL_IR_NATIVE_VALUE_REWRITE_REQUEST request;
    memset(&request, 0, sizeof(request));
    request.expected_operator = OPR_DSLCONV2D;
    request.expected_version = 2;
    request.replacement_operator = OPR_DSLCONV2D;
    request.replacement_version = 2;
    request.operand_templates = templates;
    request.operand_value_ids = operand_ids;
    request.operand_count = 3;
    request.attributes = attributes.empty() ? NULL : &attributes[0];
    request.attribute_count = attributes.size();
    request.payload = conv_node->payload;
    request.result_value_kind = DSL_IR_VALUE_OPERATOR_RESULT;
    if (!DSL_IR_Rewrite_Native_Value
             (owner_pu_st, definition, conv_result.id, &request)) {
        return VHO_FHE_Semantic_Report
                   (diagnostic, "CFHECNN-BN-003",
                    "atomic entry convolution operand rewrite failed");
    }
    return TRUE;
}

static BOOL
VHO_FHE_Prepare_Entry_Context_Folds
        (ST_IDX owner_pu_st,
         WN *tree,
         FILE *diagnostic)
{
    DSL_PU_SOURCE_IDENTITY_RECORD identity;
    WN *body = WN_func_body(tree);
    UINT32 pair_count = 0;

    if (!VHO_FHE_PU_Is_FHE_Entry(owner_pu_st))
        return TRUE;
    if (body == NULL || !DSL_Call_Image_Find_PU_Identity
                            (owner_pu_st, &identity))
        return FALSE;

    for (UINT32 id = 1; id <= DSL_IR_Image_Node_Count(); ++id) {
        DSL_IR_NODE_RECORD conv;
        DSL_IR_OPCODE_DESCRIPTOR_RECORD descriptor;
        DSL_IR_VALUE_RECORD conv_result;
        DSL_IR_VALUE_RECORD conv_weight;
        DSL_IR_VALUE_RECORD conv_bias;
        if (!DSL_IR_Image_Get_Node(id, &conv) ||
            !DSL_IR_Image_Get_Opcode_Descriptor
                (conv.opcode_descriptor_id, &descriptor) ||
            descriptor.logical_operator != OPR_DSLCONV2D ||
            descriptor.version != 2 || conv.operand_count != 3 ||
            !DSL_IR_Image_Get_Value(conv.result_value_id, &conv_result) ||
            !VHO_FHE_Value_Belongs_To_PU(&conv_result, owner_pu_st) ||
            !VHO_FHE_Node_Operand_Value(&conv, 1, &conv_weight) ||
            !VHO_FHE_Node_Operand_Value(&conv, 2, &conv_bias) ||
            conv_weight.value_kind != DSL_IR_VALUE_CONSTANT)
            continue;

        DSL_IR_NODE_RECORD bn;
        DSL_IR_VALUE_RECORD bn_operand[5];
        UINT32 bn_matches = 0;
        for (UINT32 candidate_id = 1;
             candidate_id <= DSL_IR_Image_Node_Count(); ++candidate_id) {
            DSL_IR_NODE_RECORD candidate;
            DSL_IR_OPCODE_DESCRIPTOR_RECORD candidate_descriptor;
            DSL_IR_VALUE_RECORD candidate_result;
            if (!DSL_IR_Image_Get_Node(candidate_id, &candidate) ||
                !DSL_IR_Image_Get_Opcode_Descriptor
                    (candidate.opcode_descriptor_id,
                     &candidate_descriptor) ||
                candidate_descriptor.logical_operator !=
                    OPR_DSLBATCHNORMINFER ||
                candidate_descriptor.version != 2 ||
                candidate.operand_count != 5 ||
                !DSL_IR_Image_Get_Value
                    (candidate.result_value_id, &candidate_result) ||
                !VHO_FHE_Value_Belongs_To_PU
                    (&candidate_result, owner_pu_st))
                continue;
            BOOL operands_valid = TRUE;
            for (UINT32 ordinal = 0; ordinal < 5; ++ordinal)
                operands_valid = operands_valid &&
                    VHO_FHE_Node_Operand_Value
                        (&candidate, ordinal, &bn_operand[ordinal]);
            if (!operands_valid || bn_operand[0].id != conv_result.id)
                continue;
            for (UINT32 ordinal = 1; ordinal < 5; ++ordinal)
                operands_valid = operands_valid &&
                    bn_operand[ordinal].value_kind == DSL_IR_VALUE_CONSTANT;
            if (!operands_valid)
                continue;
            bn = candidate;
            ++bn_matches;
        }
        if (bn_matches == 0)
            continue;
        if (bn_matches != 1)
            return VHO_FHE_Semantic_Report
                       (diagnostic, "CFHECNN-BN-001",
                        "entry convolution has ambiguous BatchNorm users");

        FHE_BN_CONTEXT_FOLD fold;
        fold.callee_pu_st = owner_pu_st;
        fold.caller_identity_id = identity.id;
        fold.callsite_id = DSL_CALLSITE_METADATA_INVALID_ID;
        fold.conv_weight_formal = 0;
        fold.conv_bias_formal = 0;
        fold.bn_scale_formal = 0;
        fold.bn_bias_formal = 0;
        fold.bn_mean_formal = 0;
        fold.bn_variance_formal = 0;
        fold.source_conv_weight = conv_weight.id;
        fold.source_conv_bias = conv_bias.id;
        fold.source_bn_scale = bn_operand[1].id;
        fold.source_bn_bias = bn_operand[2].id;
        fold.source_bn_mean = bn_operand[3].id;
        fold.source_bn_variance = bn_operand[4].id;
        fold.callee_conv_weight = conv_weight.id;
        fold.callee_conv_bias = conv_bias.id;
        fold.callee_bn_scale = bn_operand[1].id;
        fold.callee_bn_bias = bn_operand[2].id;
        fold.callee_bn_mean = bn_operand[3].id;
        fold.callee_bn_variance = bn_operand[4].id;
        fold.conv_node_id = conv.id;
        fold.batch_norm_node_id = bn.id;
        fold.folded_weight_value_id = DSL_IR_VALUE_INVALID_ID;
        fold.folded_bias_value_id = DSL_IR_VALUE_INVALID_ID;
        fold.role_prefix = "entry.stem.conv";
        if (!VHO_FHE_BN_Epsilon(&bn, &fold.epsilon))
            return VHO_FHE_Semantic_Report
                       (diagnostic, "CFHECNN-BN-002",
                        "entry BatchNorm epsilon is missing or invalid");

        FHE_CONVERTED_TENSOR *folded_weight = NULL;
        FHE_CONVERTED_TENSOR *folded_bias = NULL;
        if (!VHO_FHE_Compute_BN_Fold
                 (owner_pu_st, fold.source_conv_weight,
                  fold.source_conv_bias, fold.source_bn_scale,
                  fold.source_bn_bias, fold.source_bn_mean,
                  fold.source_bn_variance, fold.epsilon, "entry.stem.conv",
                  &fold.folded_weight_tcon, &fold.folded_bias_tcon,
                  &folded_weight, &folded_bias,
                  &fold.implicit_zero_bias, diagnostic))
            return FALSE;

        WN *conv_definition = NULL;
        WN *conv_block = NULL;
        VHO_FHE_Find_Value_Definition
            (tree, NULL, owner_pu_st, conv_result.id,
             &conv_definition, &conv_block);
        if (conv_definition == NULL || conv_block == NULL)
            return FALSE;
        DSL_IR_EXTERNAL_TENSOR_MATERIALIZATION_REQUEST requests[2];
        DSL_IR_EXTERNAL_TENSOR_MATERIALIZATION_RESULT results[2];
        DSL_IR_External_Tensor_Materialization_Request_Init(&requests[0]);
        requests[0].name = folded_weight->key.c_str();
        requests[0].descriptor_ty = folded_weight->ty;
        requests[0].tensor_tcon = fold.folded_weight_tcon;
        requests[0].source_value_id = fold.source_conv_weight;
        requests[0].insertion_block = conv_block;
        requests[0].insert_before = conv_definition;
        requests[0].source_position = WN_Get_Linenum(conv_definition);
        requests[0].storage_format = "safetensors";
        requests[0].side_file = VHO_FHE_converted_payload_final.c_str();
        requests[0].tensor_key = folded_weight->key.c_str();
        requests[0].byte_offset = folded_weight->offset;
        requests[0].byte_length = folded_weight->bytes.size();
        requests[0].checksum = folded_weight->checksum.c_str();
        DSL_IR_External_Tensor_Materialization_Request_Init(&requests[1]);
        requests[1].name = folded_bias->key.c_str();
        requests[1].descriptor_ty = folded_bias->ty;
        requests[1].tensor_tcon = fold.folded_bias_tcon;
        requests[1].source_value_id = fold.source_conv_bias;
        requests[1].insertion_block = conv_block;
        requests[1].insert_before = conv_definition;
        requests[1].source_position = WN_Get_Linenum(conv_definition);
        requests[1].storage_format = "safetensors";
        requests[1].side_file = VHO_FHE_converted_payload_final.c_str();
        requests[1].tensor_key = folded_bias->key.c_str();
        requests[1].byte_offset = folded_bias->offset;
        requests[1].byte_length = folded_bias->bytes.size();
        requests[1].checksum = folded_bias->checksum.c_str();
        requests[1].source_policy =
            DSL_IR_MATERIALIZE_SOURCE_EXTERNAL_OR_IMPLICIT_ZERO;
        if (!DSL_IR_Materialize_External_Tensor_Values
                 (owner_pu_st, requests, 2, results))
            return VHO_FHE_Semantic_Report
                       (diagnostic, "CFHECNN-BN-003",
                        "entry folded tensor materialization failed");
        fold.folded_weight_value_id = results[0].value_id;
        fold.folded_bias_value_id = results[1].value_id;
        if (!VHO_FHE_Rewrite_Entry_Conv_Operands
                 (owner_pu_st, tree, &conv, results[0].value_id,
                  results[1].value_id, diagnostic))
            return FALSE;
        VHO_FHE_bn_context_folds.push_back(fold);
        ++pair_count;
    }
    (void)pair_count;
    return TRUE;
}

static BOOL
VHO_FHE_Record_BN_Fold_For_Conv
        (ST_IDX owner_pu_st,
         const DSL_IR_NODE_RECORD *conv_node,
         const DSL_IR_VALUE_RECORD *conv_result,
         DSL_FHE_BN_FOLD_PROVENANCE_ID *first_fold_id,
         UINT32 *fold_count,
         FILE *diagnostic)
{
    if (first_fold_id != NULL)
        *first_fold_id = DSL_FHE_BN_FOLD_PROVENANCE_INVALID_ID;
    if (fold_count != NULL)
        *fold_count = 0;
    if (conv_node == NULL || conv_result == NULL ||
        first_fold_id == NULL || fold_count == NULL)
        return FALSE;

    for (size_t i = 0; i < VHO_FHE_bn_context_folds.size(); ++i) {
        const FHE_BN_CONTEXT_FOLD &context = VHO_FHE_bn_context_folds[i];
        if (context.callee_pu_st != owner_pu_st ||
            context.conv_node_id != conv_node->id)
            continue;
        DSL_FHE_BN_FOLD_PROVENANCE_RECORD fold;
        DSL_FHE_BN_Fold_Provenance_Record_Init(&fold);
        fold.owner_pu_st = owner_pu_st;
        fold.conv_node_id = conv_node->id;
        fold.batch_norm_node_id = context.batch_norm_node_id;
        fold.context_pu_identity_id = context.caller_identity_id;
        fold.context_callsite_id = context.callsite_id;
        fold.source_conv_weight_value_id = context.source_conv_weight;
        fold.source_conv_bias_value_id = context.implicit_zero_bias ?
            DSL_IR_VALUE_INVALID_ID : context.source_conv_bias;
        fold.source_bn_scale_value_id = context.source_bn_scale;
        fold.source_bn_bias_value_id = context.source_bn_bias;
        fold.source_bn_mean_value_id = context.source_bn_mean;
        fold.source_bn_variance_value_id = context.source_bn_variance;
        fold.folded_weight_tcon = context.folded_weight_tcon;
        fold.folded_bias_tcon = context.folded_bias_tcon;
        fold.flags =
            (context.callsite_id != DSL_CALLSITE_METADATA_INVALID_ID ?
             DSL_FHE_BN_FOLD_SHARED_PU_DEFINITION :
             DSL_FHE_BN_FOLD_FLAG_NONE) |
            (context.implicit_zero_bias ?
             DSL_FHE_BN_FOLD_IMPLICIT_ZERO_BIAS :
             DSL_FHE_BN_FOLD_FLAG_NONE);

        DSL_FHE_BN_FOLD_PROVENANCE_ID fold_id =
            DSL_FHE_Plan_Add_BN_Fold_Provenance(&fold);
        if (fold_id == DSL_FHE_BN_FOLD_PROVENANCE_INVALID_ID)
            return VHO_FHE_Semantic_Report
                       (diagnostic, "CFHECNN-BN-003",
                        "could not record context BatchNorm fold provenance");
        if (*fold_count == 0)
            *first_fold_id = fold_id;
        ++*fold_count;
    }
    (void)conv_result;
    return TRUE;
}

static void
VHO_FHE_Find_Value_Definition
        (WN *node,
         WN *containing_block,
         ST_IDX owner_pu_st,
         DSL_IR_VALUE_ID value_id,
         WN **definition,
         WN **definition_block)
{
    if (node == NULL || definition == NULL || *definition != NULL)
        return;
    if (WN_operator(node) == OPR_BLOCK) {
        for (WN *statement = WN_first(node); statement != NULL;
             statement = WN_next(statement)) {
            VHO_FHE_Find_Value_Definition
                (statement, node, owner_pu_st, value_id,
                 definition, definition_block);
            if (*definition != NULL)
                return;
        }
        return;
    }
    if (WN_operator(node) == OPR_STID) {
        DSL_IR_VALUE_RECORD value;
        if (DSL_IR_Image_Find_Definition_Value
                (owner_pu_st, node, &value) && value.id == value_id) {
            *definition = node;
            *definition_block = containing_block;
            return;
        }
    }
    for (INT32 kid = 0; kid < WN_kid_count(node); ++kid)
        VHO_FHE_Find_Value_Definition
            (WN_kid(node, kid), containing_block, owner_pu_st, value_id,
             definition, definition_block);
}

static BOOL
VHO_FHE_Retire_Batch_Norm
        (ST_IDX owner_pu_st,
         WN *tree,
         const DSL_IR_NODE_RECORD *conv_node,
         DSL_IR_VALUE_ID conv_value_id,
         UINT32 *rewritten_count,
         FILE *diagnostic)
{
    DSL_IR_NODE_ID bn_node_id = DSL_IR_NODE_INVALID_ID;
    for (size_t i = 0; i < VHO_FHE_bn_context_folds.size(); ++i) {
        const FHE_BN_CONTEXT_FOLD &fold = VHO_FHE_bn_context_folds[i];
        if (fold.callee_pu_st != owner_pu_st ||
            fold.conv_node_id != conv_node->id)
            continue;
        if (bn_node_id != DSL_IR_NODE_INVALID_ID &&
            bn_node_id != fold.batch_norm_node_id)
            return VHO_FHE_Semantic_Report
                       (diagnostic, "CFHECNN-BN-001",
                        "one convolution maps to multiple BatchNorm nodes");
        bn_node_id = fold.batch_norm_node_id;
    }
    if (bn_node_id == DSL_IR_NODE_INVALID_ID)
        return TRUE;

    DSL_IR_NODE_RECORD bn_node;
    if (!DSL_IR_Image_Get_Node(bn_node_id, &bn_node) ||
        bn_node.result_value_id == DSL_IR_VALUE_INVALID_ID)
        return VHO_FHE_Semantic_Report
                   (diagnostic, "CFHECNN-BN-001",
                    "BatchNorm result is not resolvable for retirement");
    WN *conv_definition = NULL;
    WN *conv_block = NULL;
    WN *bn_definition = NULL;
    WN *bn_block = NULL;
    VHO_FHE_Find_Value_Definition
        (tree, NULL, owner_pu_st, conv_value_id,
         &conv_definition, &conv_block);
    VHO_FHE_Find_Value_Definition
        (tree, NULL, owner_pu_st, bn_node.result_value_id,
         &bn_definition, &bn_block);
    if (conv_definition == NULL || bn_definition == NULL ||
        conv_block == NULL || conv_block != bn_block)
        return VHO_FHE_Semantic_Report
                   (diagnostic, "CFHECNN-BN-003",
                    "Conv and BatchNorm are not retireable in one block");

    DSL_IR_NATIVE_VALUE_RETIRE_REQUEST request;
    memset(&request, 0, sizeof(request));
    request.pu_root = tree;
    request.containing_block = conv_block;
    request.replacement_definition = conv_definition;
    request.replacement_value_id = conv_value_id;
    request.retiring_definition = bn_definition;
    request.retiring_value_id = bn_node.result_value_id;
    request.expected_retiring_operator = OPR_DSLBATCHNORMINFER;
    request.expected_retiring_version = 2;
    request.replacement_operand_ordinal = 0;
    if (!DSL_IR_Redirect_And_Retire_Native_Value(owner_pu_st, &request))
        return VHO_FHE_Semantic_Report
                   (diagnostic, "CFHECNN-BN-003",
                    "atomic BatchNorm result retirement failed");
    if (rewritten_count != NULL)
        ++*rewritten_count;
    return TRUE;
}

static DSL_FHE_CKKS_VALUE_STATE_ID
VHO_FHE_Record_CKKS_State
        (DSL_IR_VALUE_ID value_id,
         const FHE_CONVERSION_CONTEXT *context,
         UINT32 pending_actions,
         UINT32 bootstrap_reason)
{
    DSL_FHE_CKKS_VALUE_STATE_RECORD prior;
    DSL_FHE_CKKS_VALUE_STATE_RECORD record;
    UINT32 version = 1;

    if (DSL_FHE_Plan_Find_Latest_CKKS_Value_State(value_id, &prior))
        version = prior.state_version + 1;

    DSL_FHE_CKKS_Value_State_Record_Init(&record);
    record.value_id = value_id;
    record.encryption_descriptor_id = context->ciphertext_descriptor_id;
    record.state_version = version;
    record.scheme = DSL_FHE_SCHEME_CKKS;
    record.value_class = DSL_FHE_VALUE_CLASS_CIPHERTEXT;
    record.level = -1;
    record.scale_bits = -1;
    record.component_count = -1;
    record.precision_bits = -1;
    record.slot_count = 0;
    record.alignment_group = 0;
    record.encrypted_layout_name = Save_Str("ckks.packed");
    record.pending_actions = pending_actions;
    record.pending_bootstrap_reason = bootstrap_reason;
    return DSL_FHE_Plan_Add_CKKS_Value_State(&record);
}

static BOOL
VHO_FHE_Operator_Is_Diagnostic_Source (DSL_OPERATOR dsl_operator)
{
    return dsl_operator == OPR_DSLMODELINPUT ||
           dsl_operator == OPR_DSLTENSORCONST;
}

static BOOL
VHO_FHE_Operator_Is_Unsupported_For_FHE (DSL_OPERATOR dsl_operator)
{
    return dsl_operator == OPR_DSLMAXPOOL2D ||
           dsl_operator == OPR_DSLRESHAPE ||
           dsl_operator == OPR_DSLTRANSPOSE ||
           dsl_operator == OPR_DSLTOKENEMBEDDING ||
           dsl_operator == OPR_DSLRMSNORM ||
           dsl_operator == OPR_DSLROTARYEMBEDDING ||
           dsl_operator == OPR_DSLATTENTION ||
           dsl_operator == OPR_DSLSWIGLU ||
           dsl_operator == OPR_DSLSCATTER ||
           dsl_operator == OPR_DSLDIV ||
           dsl_operator == OPR_DSLREM ||
           dsl_operator == OPR_DSLDIVREM ||
           dsl_operator == OPR_DSLDIVPART ||
           dsl_operator == OPR_DSLREMPART;
}

static BOOL
VHO_FHE_Check_Source_Operator
        (DSL_OPERATOR dsl_operator,
         UINT16 operator_version,
         FILE *diagnostic)
{
    if (dsl_operator == OPR_DSLLINEAR && operator_version != 2 &&
        operator_version != 3)
        return VHO_FHE_Semantic_Report
                   (diagnostic, "CFHECNN-LINEAR-001",
                    "common.linear version has no FHE wrapper contract");
    if (!VHO_FHE_Operator_Is_Unsupported_For_FHE(dsl_operator))
        return TRUE;
    if (dsl_operator == OPR_DSLMAXPOOL2D)
        return VHO_FHE_Semantic_Report
                   (diagnostic, "CFHECNN-POOL-001",
                    "cnn.max_pool2d has no first-release CKKS lowering");
    return VHO_FHE_Semantic_Report
               (diagnostic, "CFHE-OP-001",
                "operator has no first-release FHE conversion contract");
}

static BOOL
VHO_FHE_Disposition_For_Operator
        (DSL_OPERATOR dsl_operator,
         UINT16 operator_version,
         DSL_FHE_CONVERSION_DISPOSITION_INFO *info,
         BOOL *supported)
{
    if (info == NULL || supported == NULL)
        return FALSE;
    memset(info, 0, sizeof(*info));
    *supported = TRUE;

    switch (dsl_operator) {
    case OPR_DSLADD:
    case OPR_DSLMUL:
        info->disposition = DSL_FHE_DISPOSITION_PRESERVE;
        info->flags = DSL_FHE_DISPOSITION_OUTPUT_ENCRYPTED;
        return TRUE;
    case OPR_DSLCONV2D:
        info->disposition = DSL_FHE_DISPOSITION_DOMAIN_WRAPPER;
        info->wrapper_name = DSL_FHE_WRAPPER_CNN_CONV2D;
        info->wrapper_version = 1;
        info->flags = DSL_FHE_DISPOSITION_DEFINITION_REWRITE |
                      DSL_FHE_DISPOSITION_OUTPUT_ENCRYPTED;
        return TRUE;
    case OPR_DSLRESIDUALADD:
        info->disposition = DSL_FHE_DISPOSITION_DOMAIN_WRAPPER;
        info->wrapper_name = DSL_FHE_WRAPPER_CNN_RESIDUAL_ADD;
        info->wrapper_version = 1;
        info->flags = DSL_FHE_DISPOSITION_OUTPUT_ENCRYPTED;
        return TRUE;
    case OPR_DSLGLOBALAVGPOOL2D:
        info->disposition = DSL_FHE_DISPOSITION_DOMAIN_WRAPPER;
        info->wrapper_name = DSL_FHE_WRAPPER_CNN_GLOBAL_AVG_POOL2D;
        info->wrapper_version = 1;
        info->flags = DSL_FHE_DISPOSITION_OUTPUT_ENCRYPTED;
        return TRUE;
    case OPR_DSLLINEAR:
        if (operator_version != 2 && operator_version != 3) {
            *supported = FALSE;
            return TRUE;
        }
        info->disposition = DSL_FHE_DISPOSITION_DOMAIN_WRAPPER;
        info->wrapper_name = DSL_FHE_WRAPPER_CNN_LINEAR;
        info->wrapper_version = operator_version == 2 ?
            DSL_FHE_WRAPPER_CNN_LINEAR_COMMON_V2_VERSION :
            DSL_FHE_WRAPPER_CNN_LINEAR_COMMON_V3_VERSION;
        info->flags = DSL_FHE_DISPOSITION_OUTPUT_ENCRYPTED;
        return TRUE;
    case OPR_DSLFLATTEN:
        info->disposition = DSL_FHE_DISPOSITION_LAYOUT_REINTERPRET;
        info->flags = DSL_FHE_DISPOSITION_NO_DATA_MOVEMENT |
                      DSL_FHE_DISPOSITION_OUTPUT_ENCRYPTED;
        return TRUE;
    case OPR_DSLOUTPUTLOGITS:
        info->disposition = DSL_FHE_DISPOSITION_PRESERVE;
        info->flags = DSL_FHE_DISPOSITION_OUTPUT_ENCRYPTED;
        return TRUE;
    case OPR_DSLBATCHNORMINFER:
        info->disposition = DSL_FHE_DISPOSITION_FOLD_INTO_PRODUCER;
        info->flags = DSL_FHE_DISPOSITION_DEFINITION_REWRITE |
                      DSL_FHE_DISPOSITION_NO_DATA_MOVEMENT |
                      DSL_FHE_DISPOSITION_OUTPUT_ENCRYPTED;
        return TRUE;
    case OPR_DSLRELU:
        info->disposition = DSL_FHE_DISPOSITION_REQUIRE_APPROXIMATION;
        info->flags = DSL_FHE_DISPOSITION_OUTPUT_ENCRYPTED;
        return TRUE;
    default:
        *supported = FALSE;
        return TRUE;
    }
}

static BOOL
VHO_FHE_Default_Semantic_Gatekeeper
        (struct pu_info *pu_info,
         WN *tree,
         const VHO_FHE_CONVERT_OPTIONS *options,
         FILE *diagnostic)
{
    FHE_CONVERSION_CONTEXT context;
    ST_IDX owner_pu_st;
    BOOL valid = TRUE;

    (void)tree;
    if (pu_info == NULL || options == NULL)
        return FALSE;
    owner_pu_st = PU_Info_proc_sym(pu_info);
    if (!VHO_FHE_Select_Default_Context(owner_pu_st, &context, diagnostic))
        return FALSE;
    if (!VHO_FHE_Validate_Entry_Contracts(owner_pu_st, diagnostic))
        valid = FALSE;

    for (UINT32 id = 1; id <= DSL_IR_Image_Node_Count(); ++id) {
        DSL_IR_NODE_RECORD node;
        DSL_IR_OPCODE_DESCRIPTOR_RECORD descriptor;
        DSL_IR_VALUE_RECORD value;

        if (!DSL_IR_Image_Get_Node(id, &node) ||
            !DSL_IR_Image_Get_Opcode_Descriptor
                (node.opcode_descriptor_id, &descriptor) ||
            node.result_value_id == DSL_IR_VALUE_INVALID_ID ||
            !DSL_IR_Image_Get_Value(node.result_value_id, &value) ||
            !VHO_FHE_Value_Belongs_To_PU(&value, owner_pu_st))
            continue;

        DSL_OPERATOR dsl_operator = (DSL_OPERATOR)descriptor.logical_operator;
        if (VHO_FHE_Operator_Is_Diagnostic_Source(dsl_operator))
            continue;
        if (!VHO_FHE_Check_Source_Operator
                 (dsl_operator, descriptor.version, diagnostic))
            valid = FALSE;
        if (descriptor.logical_operator == OPR_DSLRELU &&
            options->strict_o0 &&
            !VHO_FHE_Bootstrap_Policy_Allows_Relu
                 (context.bootstrap_policy, diagnostic))
            valid = FALSE;
    }

    return valid;
}

static BOOL
VHO_FHE_Default_Conversion_Pass
        (struct pu_info *pu_info,
         WN **tree,
         const VHO_FHE_CONVERT_OPTIONS *options,
         FILE *diagnostic,
         VHO_FHE_CONVERT_RESULT *result)
{
    FHE_CONVERSION_CONTEXT context;
    ST_IDX owner_pu_st;
    BOOL valid = TRUE;

    (void)tree;
    (void)options;
    if (pu_info == NULL || result == NULL)
        return FALSE;
    owner_pu_st = PU_Info_proc_sym(pu_info);
    if (!VHO_FHE_Select_Default_Context(owner_pu_st, &context, diagnostic))
        return FALSE;
    DSL_FHE_Plan_Register_Domain_Wrappers();

    if (VHO_FHE_PU_Is_FHE_Entry(owner_pu_st)) {
        std::vector<DSL_IR_EXTERNAL_TENSOR_MATERIALIZATION_REQUEST> requests;
        if (!VHO_FHE_bn_context_folds.empty() ||
            !VHO_FHE_converted_tensors.empty())
            return VHO_FHE_Semantic_Report
                       (diagnostic, "CFHECNN-BN-003",
                        "BatchNorm conversion transaction was already used");
        if (!VHO_FHE_Prepare_Entry_Context_Folds
                 (owner_pu_st, *tree, diagnostic) ||
            !VHO_FHE_Prepare_Call_Context_Folds
                 (owner_pu_st, *tree, &requests, diagnostic))
            return FALSE;
    }

    for (UINT32 id = 1; id <= DSL_IR_Image_Node_Count(); ++id) {
        DSL_IR_NODE_RECORD node;
        DSL_IR_OPCODE_DESCRIPTOR_RECORD descriptor;
        DSL_IR_VALUE_RECORD value;
        DSL_FHE_CONVERSION_DISPOSITION_RECORD existing;
        DSL_FHE_CONVERSION_DISPOSITION_INFO disposition;
        DSL_FHE_CKKS_VALUE_STATE_ID state_id;
        BOOL supported;
        UINT32 pending_actions = DSL_FHE_CKKS_PENDING_NONE;
        UINT32 bootstrap_reason = DSL_FHE_BOOTSTRAP_REASON_NONE;

        if (!DSL_IR_Image_Get_Node(id, &node) ||
            !DSL_IR_Image_Get_Opcode_Descriptor
                (node.opcode_descriptor_id, &descriptor) ||
            node.result_value_id == DSL_IR_VALUE_INVALID_ID ||
            !DSL_IR_Image_Get_Value(node.result_value_id, &value) ||
            !VHO_FHE_Value_Belongs_To_PU(&value, owner_pu_st))
            continue;

        if (!VHO_FHE_Disposition_For_Operator
                 ((DSL_OPERATOR)descriptor.logical_operator,
                  descriptor.version,
                  &disposition, &supported))
            return FALSE;
        if (VHO_FHE_Operator_Is_Diagnostic_Source
                ((DSL_OPERATOR)descriptor.logical_operator))
            continue;
        if (!supported) {
            if (!VHO_FHE_Check_Source_Operator
                     ((DSL_OPERATOR)descriptor.logical_operator,
                      descriptor.version,
                      diagnostic))
                valid = FALSE;
            continue;
        }

        ++result->source_disposition_count;
        if (DSL_FHE_Plan_Find_Conversion_Disposition
                (node.id, &existing))
            continue;

        if (descriptor.logical_operator == OPR_DSLRELU) {
            if (!VHO_FHE_Bootstrap_Policy_Allows_Relu
                     (context.bootstrap_policy, diagnostic))
                return FALSE;
            VHO_FHE_relu_policy_blocked = TRUE;
            continue;
        }

        state_id = VHO_FHE_Record_CKKS_State
                       (value.id, &context, pending_actions,
                        bootstrap_reason);
        if (state_id == DSL_FHE_CKKS_VALUE_STATE_INVALID_ID) {
            valid = VHO_FHE_Semantic_Report
                        (diagnostic, "CFHE-STATE-001",
                         "could not record CKKS value state");
            continue;
        }
        disposition.result_ckks_value_state_id = state_id;

        if (descriptor.logical_operator == OPR_DSLRELU) {
            if (!VHO_FHE_Create_Relu_Approximation
                     (&context, value.ty,
                      &disposition.approximation_contract_id,
                      diagnostic)) {
                valid = FALSE;
                continue;
            }
            ++result->approximation_contract_count;
        } else if (descriptor.logical_operator == OPR_DSLCONV2D) {
            if (!VHO_FHE_Record_BN_Fold_For_Conv
                     (owner_pu_st, &node, &value,
                      &disposition.first_bn_fold_id,
                      &disposition.bn_fold_count, diagnostic)) {
                valid = FALSE;
                continue;
            }
            result->folded_batch_norm_count += disposition.bn_fold_count;
            if (disposition.bn_fold_count != 0 &&
                !VHO_FHE_Retire_Batch_Norm
                    (owner_pu_st, *tree, &node, value.id,
                     &result->rewritten_value_count, diagnostic)) {
                valid = FALSE;
                continue;
            }
        }

        DSL_FHE_CONVERSION_DISPOSITION_RECORD record;
        DSL_FHE_Conversion_Disposition_Record_Init(&record);
        record.source_node_id = node.id;
        record.result_value_id = value.id;
        record.disposition = disposition.disposition;
        record.owner_pu_st = owner_pu_st;
        record.wrapper_version = disposition.wrapper_version;
        if (disposition.wrapper_name != NULL)
            record.wrapper_name = Save_Str(disposition.wrapper_name);
        record.approximation_contract_id =
            disposition.approximation_contract_id;
        record.result_ckks_value_state_id =
            disposition.result_ckks_value_state_id;
        record.first_bn_fold_id = disposition.first_bn_fold_id;
        record.bn_fold_count = disposition.bn_fold_count;
        record.flags = disposition.flags;

        if (DSL_FHE_Plan_Add_Conversion_Disposition(&record) ==
            DSL_FHE_CONVERSION_DISPOSITION_INVALID_ID) {
            char message[512];
            snprintf(message, sizeof(message),
                     "could not record disposition for %s.v%u "
                     "node=%u value=%u owner_pu=0x%x disposition=%u "
                     "wrapper=%s.v%u approximation=%u state=%u "
                     "bn_first=%u bn_count=%u flags=0x%x",
                     Index_To_Str(descriptor.stable_name),
                     (unsigned)descriptor.version,
                     (unsigned)record.source_node_id,
                     (unsigned)record.result_value_id,
                     (unsigned)record.owner_pu_st,
                     (unsigned)record.disposition,
                     record.wrapper_name == STR_IDX_ZERO ? "<none>" :
                         Index_To_Str(record.wrapper_name),
                     (unsigned)record.wrapper_version,
                     (unsigned)record.approximation_contract_id,
                     (unsigned)record.result_ckks_value_state_id,
                     (unsigned)record.first_bn_fold_id,
                     (unsigned)record.bn_fold_count,
                     (unsigned)record.flags);
            valid = VHO_FHE_Semantic_Report
                        (diagnostic, "CFHE-DISPOSITION-001",
                         message);
            continue;
        }
        ++result->converted_disposition_count;
    }

    return valid;
}

static BOOL
VHO_FHE_Write_Converted_Payload (FILE *diagnostic)
{
    std::ostringstream header;
    header << "{";
    for (size_t i = 0; i < VHO_FHE_converted_tensors.size(); ++i) {
        const FHE_CONVERTED_TENSOR &tensor = VHO_FHE_converted_tensors[i];
        const char *shape = TY_tensor_attribute
                                (tensor.ty, TY_TENSOR_SCHEMA_SHAPE);
        if (shape == NULL)
            return VHO_FHE_Semantic_Report
                       (diagnostic, "CFHECNN-BN-003",
                        "converted tensor has no canonical shape");
        if (i != 0)
            header << ",";
        header << "\"" << tensor.key << "\":{";
        header << "\"dtype\":\"F32\",\"shape\":" << shape << ",";
        header << "\"data_offsets\":[" << tensor.offset << ","
               << tensor.offset + tensor.bytes.size() << "]}";
    }
    header << "}";
    std::string header_bytes = header.str();
    while ((header_bytes.size() & 7) != 0)
        header_bytes += ' ';

    FILE *file = fopen(VHO_FHE_converted_payload_temp.c_str(), "wb");
    if (file == NULL)
        return VHO_FHE_Semantic_Report
                   (diagnostic, "CFHECNN-BN-003",
                    "could not create converted tensor payload");
    UINT64 header_size = header_bytes.size();
    unsigned char encoded_size[8];
    for (UINT32 i = 0; i < 8; ++i)
        encoded_size[i] = (unsigned char)(header_size >> (i * 8));
    BOOL valid = fwrite(encoded_size, 1, sizeof(encoded_size), file) ==
                     sizeof(encoded_size) &&
                 fwrite(header_bytes.data(), 1, header_bytes.size(), file) ==
                     header_bytes.size();
    for (size_t i = 0; valid && i < VHO_FHE_converted_tensors.size(); ++i) {
        const std::vector<unsigned char> &bytes =
            VHO_FHE_converted_tensors[i].bytes;
        valid = fwrite(&bytes[0], 1, bytes.size(), file) == bytes.size();
    }
    valid = fclose(file) == 0 && valid;
    if (!valid)
        return VHO_FHE_Semantic_Report
                   (diagnostic, "CFHECNN-BN-003",
                    "could not finalize converted tensor payload");
    return TRUE;
}

static BOOL
VHO_FHE_Write_Conversion_Report
        (const VHO_FHE_CONVERT_RESULT *aggregate,
         FILE *diagnostic)
{
    FILE *file = fopen(VHO_FHE_conversion_report_temp.c_str(), "w");
    if (file == NULL)
        return VHO_FHE_Semantic_Report
                   (diagnostic, "CFHECNN-BN-003",
                    "could not create FHE conversion report");
    fprintf(file, "FHE SYNC-3 BatchNorm conversion report\n");
    fprintf(file, "source_dispositions=%u\n",
            aggregate->source_disposition_count);
    fprintf(file, "converted_dispositions=%u\n",
            aggregate->converted_disposition_count);
    fprintf(file, "rewritten_values=%u\n", aggregate->rewritten_value_count);
    fprintf(file, "folded_batch_norm=%u\n",
            aggregate->folded_batch_norm_count);
    fprintf(file, "context_folds=%u\n",
            (unsigned)VHO_FHE_bn_context_folds.size());
    fprintf(file, "converted_tensors=%u\n",
            (unsigned)VHO_FHE_converted_tensors.size());
    for (size_t i = 0; i < VHO_FHE_converted_tensors.size(); ++i) {
        const FHE_CONVERTED_TENSOR &tensor = VHO_FHE_converted_tensors[i];
        fprintf(file,
                "tensor[%u]=%s offset=%llu length=%llu checksum=%s "
                "converted_from=%u\n",
                (unsigned)i, tensor.key.c_str(),
                (unsigned long long)tensor.offset,
                (unsigned long long)tensor.bytes.size(),
                tensor.checksum.c_str(), (unsigned)tensor.source_value_id);
    }
    if (fclose(file) != 0)
        return VHO_FHE_Semantic_Report
                   (diagnostic, "CFHECNN-BN-003",
                    "could not finalize FHE conversion report");
    return TRUE;
}

static BOOL
VHO_FHE_Value_Has_No_Executable_Use (DSL_IR_VALUE_ID value_id)
{
    for (UINT32 id = 1; id <= DSL_IR_Image_Value_Reference_Count(); ++id) {
        DSL_IR_VALUE_REFERENCE_RECORD reference;
        DSL_IR_NODE_RECORD owner;
        if (!DSL_IR_Image_Get_Value_Reference(id, &reference) ||
            reference.value_id != value_id)
            continue;
        if (!DSL_IR_Image_Get_Node(reference.owner_node_id, &owner) ||
            (owner.flags & DSL_IR_NODE_FLAG_RETIRED) == 0)
            return FALSE;
    }
    return TRUE;
}

static BOOL
VHO_FHE_Verify_BN_Retirement
        (UINT32 *definition_count,
         FILE *diagnostic)
{
    std::vector<DSL_IR_NODE_ID> retired_nodes;
    if (definition_count != NULL)
        *definition_count = 0;
    for (size_t i = 0; i < VHO_FHE_bn_context_folds.size(); ++i) {
        const FHE_BN_CONTEXT_FOLD &fold = VHO_FHE_bn_context_folds[i];
        DSL_IR_NODE_RECORD bn;
        if (!DSL_IR_Image_Get_Node(fold.batch_norm_node_id, &bn) ||
            (bn.flags & DSL_IR_NODE_FLAG_RETIRED) == 0 ||
            !VHO_FHE_Value_Has_No_Executable_Use(fold.callee_bn_scale) ||
            !VHO_FHE_Value_Has_No_Executable_Use(fold.callee_bn_bias) ||
            !VHO_FHE_Value_Has_No_Executable_Use(fold.callee_bn_mean) ||
            !VHO_FHE_Value_Has_No_Executable_Use(fold.callee_bn_variance)) {
            return VHO_FHE_Semantic_Report
                       (diagnostic, "CFHECNN-BN-003",
                        "retired BatchNorm retains an executable use");
        }
        if (std::find(retired_nodes.begin(), retired_nodes.end(),
                      fold.batch_norm_node_id) == retired_nodes.end())
            retired_nodes.push_back(fold.batch_norm_node_id);
    }
    if (definition_count != NULL)
        *definition_count = retired_nodes.size();
    return TRUE;
}

static BOOL
VHO_FHE_Default_Checkpoint_Finalizer
        (const VHO_FHE_CONVERT_RESULT *aggregate,
         FILE *diagnostic)
{
    UINT32 retired_definition_count = 0;
    if (!VHO_FHE_Verify_BN_Retirement
             (&retired_definition_count, diagnostic))
        return FALSE;
    fprintf(diagnostic,
            "FHE-BN-CHECKPOINT: definitions=%u contexts=%u tensors=%u "
            "provenance=%u rewritten=%u\n",
            (unsigned)retired_definition_count,
            (unsigned)VHO_FHE_bn_context_folds.size(),
            (unsigned)VHO_FHE_converted_tensors.size(),
            (unsigned)aggregate->folded_batch_norm_count,
            (unsigned)aggregate->rewritten_value_count);
    if (VHO_FHE_relu_policy_blocked)
        return VHO_FHE_Semantic_Report
                   (diagnostic, "CFHECNN-RELU-002",
                    "common.relu degree-3 coefficient policy is not approved");
    if (!VHO_FHE_artifacts_registered)
        return TRUE;
    return VHO_FHE_Write_Converted_Payload(diagnostic) &&
           VHO_FHE_Write_Conversion_Report(aggregate, diagnostic);
}

static void
VHO_FHE_Default_Checkpoint_Completion (BOOL committed)
{
    (void)committed;
    VHO_FHE_bn_context_folds.clear();
    VHO_FHE_converted_tensors.clear();
    VHO_FHE_converted_payload_temp.clear();
    VHO_FHE_converted_payload_final.clear();
    VHO_FHE_conversion_report_temp.clear();
    VHO_FHE_conversion_report_final.clear();
    VHO_FHE_artifacts_registered = FALSE;
    VHO_FHE_relu_policy_blocked = FALSE;
}

BOOL
VHO_FHE_Register_Default_Semantic_Conversion (void)
{
    return VHO_FHE_Convert_Register_Semantic_Gatekeeper
               (VHO_FHE_Default_Semantic_Gatekeeper) &&
           VHO_FHE_Convert_Register_Pass(VHO_FHE_Default_Conversion_Pass) &&
           VHO_FHE_Convert_Register_Checkpoint_Lifecycle
               (VHO_FHE_Default_Checkpoint_Finalizer,
                VHO_FHE_Default_Checkpoint_Completion);
}

namespace {
struct VHO_FHE_Default_Registration {
    VHO_FHE_Default_Registration()
    {
        (void)VHO_FHE_Register_Default_Semantic_Conversion();
    }
};

static VHO_FHE_Default_Registration VHO_FHE_default_registration;
}
