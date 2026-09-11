/*
 * Copyright (C) 2026 Open64 Project
 */

#ifndef dsl_fhe_plan_INCLUDED
#define dsl_fhe_plan_INCLUDED

#include <stdio.h>

#include "defs.h"
#include "dsl_fhe.h"
#include "dsl_ir_image.h"
#include "symtab_idx.h"

#define DSL_FHE_PLAN_IMAGE_MAGIC   0x46485031
#define DSL_FHE_PLAN_IMAGE_VERSION 1

#define DSL_FHE_PLAN_IMAGE_HEADER_SIZE          64
#define DSL_FHE_PLAN_DISPOSITION_RECORD_SIZE    56
#define DSL_FHE_PLAN_APPROXIMATION_RECORD_SIZE  64
#define DSL_FHE_PLAN_CKKS_STATE_RECORD_SIZE     64
#define DSL_FHE_PLAN_BN_FOLD_RECORD_SIZE        64

#define DSL_FHE_APPROX_PROFILE_IMAGE_MAGIC        0x46415031
#define DSL_FHE_APPROX_PROFILE_IMAGE_VERSION      1
#define DSL_FHE_APPROX_PROFILE_IMAGE_HEADER_SIZE  64
#define DSL_FHE_COMPOSITE_PROFILE_RECORD_SIZE     80
#define DSL_FHE_APPROX_STAGE_RECORD_SIZE           80
#define DSL_FHE_APPROX_ASSOCIATION_RECORD_SIZE     32
#define DSL_FHE_CONTEXT_RANGE_RECORD_SIZE          64

#define DSL_FHE_CONTEXT_STATE_IMAGE_MAGIC        0x46435331
#define DSL_FHE_CONTEXT_STATE_IMAGE_VERSION      1
#define DSL_FHE_CONTEXT_STATE_IMAGE_HEADER_SIZE  64
#define DSL_FHE_CONTEXT_CKKS_STATE_RECORD_SIZE   88

#define DSL_FHE_WRAPPER_CNN_CONV2D \
    "fhe.cnn.conv2d"
#define DSL_FHE_WRAPPER_CNN_RESIDUAL_ADD \
    "fhe.cnn.residual_add"
#define DSL_FHE_WRAPPER_CNN_GLOBAL_AVG_POOL2D \
    "fhe.cnn.global_avg_pool2d"
#define DSL_FHE_WRAPPER_CNN_LINEAR \
    "fhe.cnn.linear"

/*
 * Wrapper versions are append-only compatibility identities. Version 1 was
 * published for common.linear.v3; version 2 admits the earlier three-kid
 * common.linear.v2 contract used by the certified ResNet capture.
 */
#define DSL_FHE_WRAPPER_CNN_LINEAR_COMMON_V3_VERSION 1
#define DSL_FHE_WRAPPER_CNN_LINEAR_COMMON_V2_VERSION 2

typedef UINT32 DSL_FHE_CONVERSION_DISPOSITION_ID;
typedef UINT32 DSL_FHE_APPROXIMATION_CONTRACT_ID;
typedef UINT32 DSL_FHE_BN_FOLD_PROVENANCE_ID;
typedef UINT32 DSL_FHE_COMPOSITE_PROFILE_ID;
typedef UINT32 DSL_FHE_APPROX_STAGE_ID;
typedef UINT32 DSL_FHE_APPROX_ASSOCIATION_ID;
typedef UINT32 DSL_FHE_CONTEXT_RANGE_ID;
typedef UINT32 DSL_FHE_CONTEXT_CKKS_STATE_ID;

#define DSL_FHE_CONVERSION_DISPOSITION_INVALID_ID 0
#define DSL_FHE_APPROXIMATION_CONTRACT_INVALID_ID 0
#define DSL_FHE_BN_FOLD_PROVENANCE_INVALID_ID     0
#define DSL_FHE_COMPOSITE_PROFILE_INVALID_ID       0
#define DSL_FHE_APPROX_STAGE_INVALID_ID             0
#define DSL_FHE_APPROX_ASSOCIATION_INVALID_ID       0
#define DSL_FHE_CONTEXT_RANGE_INVALID_ID            0
#define DSL_FHE_CONTEXT_CKKS_STATE_INVALID_ID        0

typedef enum {
    DSL_FHE_PLAN_RECORD_UNKNOWN = 0,
    DSL_FHE_PLAN_RECORD_DISPOSITION = 1,
    DSL_FHE_PLAN_RECORD_APPROXIMATION = 2,
    DSL_FHE_PLAN_RECORD_CKKS_VALUE_STATE = 3,
    DSL_FHE_PLAN_RECORD_BN_FOLD = 4
} DSL_FHE_PLAN_RECORD_KIND;

typedef enum {
    DSL_FHE_PLAN_CAP_DISPOSITION = 0x00000001,
    DSL_FHE_PLAN_CAP_APPROXIMATION = 0x00000002,
    DSL_FHE_PLAN_CAP_CKKS_VALUE_STATE = 0x00000004,
    DSL_FHE_PLAN_CAP_BN_FOLD = 0x00000008
} DSL_FHE_PLAN_CAPABILITY;

typedef enum {
    DSL_FHE_DISPOSITION_UNKNOWN = 0,
    DSL_FHE_DISPOSITION_PRESERVE = 1,
    DSL_FHE_DISPOSITION_DOMAIN_WRAPPER = 2,
    DSL_FHE_DISPOSITION_FOLD_INTO_PRODUCER = 3,
    DSL_FHE_DISPOSITION_LAYOUT_REINTERPRET = 4,
    DSL_FHE_DISPOSITION_REQUIRE_APPROXIMATION = 5,
    DSL_FHE_DISPOSITION_REQUIRE_COMPOSITE_APPROXIMATION = 6
} DSL_FHE_CONVERSION_DISPOSITION;

typedef enum {
    DSL_FHE_APPROX_PROFILE_CAP_PROFILE = 0x00000001,
    DSL_FHE_APPROX_PROFILE_CAP_STAGE = 0x00000002,
    DSL_FHE_APPROX_PROFILE_CAP_ASSOCIATION = 0x00000004,
    DSL_FHE_APPROX_PROFILE_CAP_CONTEXT_RANGE = 0x00000008
} DSL_FHE_APPROX_PROFILE_CAPABILITY;

typedef enum {
    DSL_FHE_RECONSTRUCTION_UNKNOWN = 0,
    DSL_FHE_RECONSTRUCTION_RELU_FROM_NORMALIZED_SIGN = 1
} DSL_FHE_APPROX_RECONSTRUCTION;

typedef enum {
    DSL_FHE_NORMALIZATION_UNKNOWN = 0,
    DSL_FHE_NORMALIZATION_POSITIVE_CONTEXT_BOUND = 1
} DSL_FHE_APPROX_NORMALIZATION_POLICY;

typedef enum {
    DSL_FHE_PRE_REFRESH_UNKNOWN = 0,
    DSL_FHE_PRE_REFRESH_INHERIT = 1,
    DSL_FHE_PRE_REFRESH_REQUIRED = 2,
    DSL_FHE_PRE_REFRESH_PROVEN_EXISTING = 3
} DSL_FHE_APPROX_PRE_REFRESH_POLICY;

typedef enum {
    DSL_FHE_APPROX_BASIS_UNKNOWN = 0,
    DSL_FHE_APPROX_BASIS_CHEBYSHEV = 1,
    DSL_FHE_APPROX_BASIS_MONOMIAL = 2
} DSL_FHE_APPROX_BASIS;

typedef enum {
    DSL_FHE_APPROX_EVAL_UNKNOWN = 0,
    DSL_FHE_APPROX_EVAL_CLENSHAW = 1,
    DSL_FHE_APPROX_EVAL_PATERSON_STOCKMEYER = 2,
    DSL_FHE_APPROX_EVAL_ADDITION_CHAIN = 3
} DSL_FHE_APPROX_EVALUATION_SCHEME;

typedef enum {
    DSL_FHE_APPROX_INPUT_SCALE_UNKNOWN = 0,
    DSL_FHE_APPROX_INPUT_SCALE_ANY_COMPATIBLE = 1,
    DSL_FHE_APPROX_INPUT_SCALE_PROFILE_NORMALIZED = 2
} DSL_FHE_APPROX_INPUT_SCALE_POLICY;

typedef enum {
    DSL_FHE_APPROX_OUTPUT_SCALE_UNKNOWN = 0,
    DSL_FHE_APPROX_OUTPUT_SCALE_PRESERVE_INPUT = 1,
    DSL_FHE_APPROX_OUTPUT_SCALE_DEFAULT_RESCALE = 2
} DSL_FHE_APPROX_OUTPUT_SCALE_POLICY;

typedef enum {
    DSL_FHE_APPROX_LEVEL_UNKNOWN = 0,
    DSL_FHE_APPROX_LEVEL_ANY_SUFFICIENT = 1,
    DSL_FHE_APPROX_LEVEL_MINIMUM = 2,
    DSL_FHE_APPROX_LEVEL_EXACT = 3
} DSL_FHE_APPROX_LEVEL_POLICY;

typedef enum {
    DSL_FHE_APPROX_COMPONENT_UNKNOWN = 0,
    DSL_FHE_APPROX_COMPONENT_PRESERVE = 1,
    DSL_FHE_APPROX_COMPONENT_RELINEARIZED_TWO = 2,
    DSL_FHE_APPROX_COMPONENT_MAY_GROW = 3
} DSL_FHE_APPROX_COMPONENT_POLICY;

typedef enum {
    DSL_FHE_CONTEXT_RANGE_UNKNOWN = 0,
    DSL_FHE_CONTEXT_RANGE_REJECT = 1
} DSL_FHE_CONTEXT_OUT_OF_RANGE_POLICY;

typedef enum {
    DSL_FHE_CONTEXT_RANGE_FLAG_NONE = 0,
    DSL_FHE_CONTEXT_RANGE_IDENTITY_IS_CALLEE = 0x00000001
} DSL_FHE_CONTEXT_RANGE_FLAG;

typedef enum {
    DSL_FHE_CONTEXT_STATE_ROLE_UNKNOWN = 0,
    DSL_FHE_CONTEXT_STATE_ROLE_PRE_OPERATION = 1,
    DSL_FHE_CONTEXT_STATE_ROLE_POST_REFRESH = 2,
    DSL_FHE_CONTEXT_STATE_ROLE_POST_OPERATION = 3,
    DSL_FHE_CONTEXT_STATE_ROLE_RESULT = 4
} DSL_FHE_CONTEXT_STATE_ROLE;

typedef enum {
    DSL_FHE_CONTEXT_STATE_CAP_CKKS_STATE = 0x00000001
} DSL_FHE_CONTEXT_STATE_CAPABILITY;

typedef enum {
    DSL_FHE_DISPOSITION_FLAG_NONE = 0,
    DSL_FHE_DISPOSITION_DEFINITION_REWRITE = 0x00000001,
    DSL_FHE_DISPOSITION_CONTEXT_SENSITIVE = 0x00000002,
    DSL_FHE_DISPOSITION_NO_DATA_MOVEMENT = 0x00000004,
    DSL_FHE_DISPOSITION_OUTPUT_ENCRYPTED = 0x00000008
} DSL_FHE_CONVERSION_DISPOSITION_FLAG;

typedef enum {
    DSL_FHE_APPROXIMATION_UNKNOWN = 0,
    DSL_FHE_APPROXIMATION_MINIMAX = 1,
    DSL_FHE_APPROXIMATION_CHEBYSHEV = 2,
    DSL_FHE_APPROXIMATION_TAYLOR = 3
} DSL_FHE_APPROXIMATION_FAMILY;

typedef enum {
    DSL_FHE_APPROX_SCALE_UNKNOWN = 0,
    DSL_FHE_APPROX_SCALE_INHERIT = 1,
    DSL_FHE_APPROX_SCALE_PRESERVE = 2,
    DSL_FHE_APPROX_SCALE_EXPLICIT = 3
} DSL_FHE_APPROXIMATION_SCALE_POLICY;

typedef enum {
    DSL_FHE_CKKS_PENDING_NONE = 0,
    DSL_FHE_CKKS_PENDING_RESCALE = 0x00000001,
    DSL_FHE_CKKS_PENDING_RELINEARIZE = 0x00000002,
    DSL_FHE_CKKS_PENDING_BOOTSTRAP = 0x00000004
} DSL_FHE_CKKS_PENDING_ACTION;

typedef enum {
    DSL_FHE_BOOTSTRAP_REASON_NONE = 0,
    DSL_FHE_BOOTSTRAP_REASON_PRE_RELU_REFRESH = 1,
    DSL_FHE_BOOTSTRAP_REASON_DEPTH_EXHAUSTION = 2,
    DSL_FHE_BOOTSTRAP_REASON_MANUAL_BOUNDARY_REQUIRED = 3
} DSL_FHE_BOOTSTRAP_REASON;

typedef enum {
    DSL_FHE_BN_FOLD_FLAG_NONE = 0,
    DSL_FHE_BN_FOLD_IMPLICIT_ZERO_BIAS = 0x00000001,
    DSL_FHE_BN_FOLD_SHARED_PU_DEFINITION = 0x00000002,
    DSL_FHE_BN_FOLD_CONTEXT_IDENTITY_IS_CALLEE = 0x00000004
} DSL_FHE_BN_FOLD_FLAG;

typedef struct {
    UINT32 magic;
    UINT32 version;
    UINT32 header_size;
    UINT32 record_kind_count;
    UINT32 capabilities;
    UINT32 flags;
    UINT32 disposition_count;
    UINT32 approximation_count;
    UINT32 ckks_value_state_count;
    UINT32 bn_fold_count;
    UINT32 reserved0;
    UINT32 reserved1;
    UINT32 reserved2;
    UINT32 reserved3;
    UINT32 reserved4;
    UINT32 reserved5;
} DSL_FHE_PLAN_IMAGE_HEADER;

typedef struct {
    DSL_FHE_CONVERSION_DISPOSITION_ID id;
    DSL_IR_NODE_ID source_node_id;
    DSL_IR_VALUE_ID result_value_id;
    UINT32 disposition;
    ST_IDX owner_pu_st;
    UINT32 wrapper_version;
    STR_IDX wrapper_name;
    DSL_FHE_APPROXIMATION_CONTRACT_ID approximation_contract_id;
    DSL_FHE_CKKS_VALUE_STATE_ID result_ckks_value_state_id;
    DSL_FHE_BN_FOLD_PROVENANCE_ID first_bn_fold_id;
    UINT32 bn_fold_count;
    UINT32 flags;
    UINT32 reserved;
} DSL_FHE_CONVERSION_DISPOSITION_RECORD;

typedef struct {
    DSL_FHE_APPROXIMATION_CONTRACT_ID id;
    DSL_FHE_CONFIG_ID config_id;
    STR_IDX polynomial_name;
    UINT32 approximation_family;
    UINT32 polynomial_version;
    UINT32 degree;
    TCON_IDX coefficient_tensor_tcon;
    TCON_IDX valid_range_min_tcon;
    TCON_IDX valid_range_max_tcon;
    TCON_IDX max_abs_error_tcon;
    UINT32 scale_policy;
    UINT32 required_multiplicative_depth;
    UINT32 bootstrap_policy;
    UINT32 requires_pre_refresh;
    UINT32 flags;
} DSL_FHE_APPROXIMATION_CONTRACT_RECORD;

typedef struct {
    DSL_FHE_CKKS_VALUE_STATE_ID id;
    DSL_IR_VALUE_ID value_id;
    DSL_FHE_ENCRYPTION_DESCRIPTOR_ID encryption_descriptor_id;
    UINT32 state_version;
    UINT32 scheme;
    UINT32 value_class;
    INT32 level;
    INT32 scale_bits;
    INT32 component_count;
    INT32 precision_bits;
    UINT32 slot_count;
    UINT32 alignment_group;
    STR_IDX encrypted_layout_name;
    UINT32 pending_actions;
    UINT32 pending_bootstrap_reason;
} DSL_FHE_CKKS_VALUE_STATE_RECORD;

typedef struct {
    DSL_FHE_BN_FOLD_PROVENANCE_ID id;
    ST_IDX owner_pu_st;
    DSL_IR_NODE_ID conv_node_id;
    DSL_IR_NODE_ID batch_norm_node_id;
    DSL_PU_SOURCE_IDENTITY_ID context_pu_identity_id;
    DSL_CALLSITE_METADATA_ID context_callsite_id;
    DSL_IR_VALUE_ID source_conv_weight_value_id;
    DSL_IR_VALUE_ID source_conv_bias_value_id;
    DSL_IR_VALUE_ID source_bn_scale_value_id;
    DSL_IR_VALUE_ID source_bn_bias_value_id;
    DSL_IR_VALUE_ID source_bn_mean_value_id;
    DSL_IR_VALUE_ID source_bn_variance_value_id;
    TCON_IDX folded_weight_tcon;
    TCON_IDX folded_bias_tcon;
    UINT32 flags;
    UINT32 reserved;
} DSL_FHE_BN_FOLD_PROVENANCE_RECORD;

typedef struct {
    UINT32 magic;
    UINT32 version;
    UINT32 header_size;
    UINT32 record_kind_count;
    UINT32 capabilities;
    UINT32 flags;
    UINT32 profile_count;
    UINT32 stage_count;
    UINT32 association_count;
    UINT32 context_range_count;
    UINT32 reserved0;
    UINT32 reserved1;
    UINT32 reserved2;
    UINT32 reserved3;
    UINT32 reserved4;
    UINT32 reserved5;
} DSL_FHE_APPROX_PROFILE_IMAGE_HEADER;

typedef struct {
    DSL_FHE_COMPOSITE_PROFILE_ID id;
    DSL_FHE_CONFIG_ID config_id;
    STR_IDX profile_name;
    STR_IDX source_revision;
    STR_IDX manifest_sha256;
    UINT32 profile_version;
    UINT32 reconstruction;
    UINT32 total_multiplicative_depth;
    UINT32 normalization_policy;
    UINT32 pre_refresh_policy;
    DSL_FHE_APPROX_STAGE_ID first_stage_id;
    UINT32 stage_count;
    UINT32 flags;
    UINT32 reserved0;
    UINT32 reserved1;
    UINT32 reserved2;
    UINT32 reserved3;
} DSL_FHE_COMPOSITE_PROFILE_RECORD;

typedef struct {
    DSL_FHE_APPROX_STAGE_ID id;
    DSL_FHE_COMPOSITE_PROFILE_ID profile_id;
    UINT32 stage_ordinal;
    UINT32 approximation_family;
    UINT32 basis;
    UINT32 degree;
    UINT32 evaluation_scheme;
    UINT32 required_input_value_class;
    UINT32 input_scale_policy;
    UINT32 input_level_policy;
    INT32 required_input_level;
    INT32 level_consumption;
    UINT32 output_scale_policy;
    UINT32 output_component_policy;
    INT32 minimum_precision_bits;
    TCON_IDX coefficient_tensor_tcon;
    STR_IDX coefficient_sha256;
    UINT32 flags;
    UINT32 reserved;
} DSL_FHE_APPROX_STAGE_RECORD;

typedef struct {
    DSL_FHE_APPROX_ASSOCIATION_ID id;
    DSL_FHE_CONVERSION_DISPOSITION_ID disposition_id;
    DSL_IR_VALUE_ID source_relu_value_id;
    DSL_FHE_COMPOSITE_PROFILE_ID profile_id;
    ST_IDX owner_pu_st;
    UINT32 flags;
    UINT32 reserved0;
    UINT32 reserved1;
} DSL_FHE_APPROX_ASSOCIATION_RECORD;

typedef struct {
    DSL_FHE_CONTEXT_RANGE_ID id;
    DSL_FHE_COMPOSITE_PROFILE_ID profile_id;
    DSL_IR_VALUE_ID source_relu_value_id;
    DSL_PU_SOURCE_IDENTITY_ID context_pu_identity_id;
    DSL_CALLSITE_METADATA_ID context_callsite_id;
    ST_IDX owner_pu_st;
    TCON_IDX positive_bound_tcon;
    TCON_IDX observed_min_tcon;
    TCON_IDX observed_max_tcon;
    UINT32 out_of_range_policy;
    STR_IDX provenance;
    UINT32 flags;
    UINT32 reserved0;
    UINT32 reserved1;
    UINT32 reserved2;
} DSL_FHE_CONTEXT_RANGE_RECORD;

typedef struct {
    UINT32 magic;
    UINT32 version;
    UINT32 header_size;
    UINT32 record_kind_count;
    UINT32 capabilities;
    UINT32 flags;
    UINT32 context_ckks_state_count;
    UINT32 reserved0;
    UINT32 reserved1;
    UINT32 reserved2;
    UINT32 reserved3;
    UINT32 reserved4;
    UINT32 reserved5;
    UINT32 reserved6;
    UINT32 reserved7;
    UINT32 reserved8;
} DSL_FHE_CONTEXT_STATE_IMAGE_HEADER;

/*
 * Context-specific planning state for a shared source value. POST_REFRESH is
 * the target of a planned pre-operation refresh, not evidence that the
 * refresh has executed. State version never identifies a call context.
 */
typedef struct {
    DSL_FHE_CONTEXT_CKKS_STATE_ID id;
    ST_IDX owner_pu_st;
    DSL_IR_VALUE_ID source_value_id;
    DSL_PU_SOURCE_IDENTITY_ID context_pu_identity_id;
    DSL_CALLSITE_METADATA_ID context_callsite_id;
    UINT32 state_role;
    UINT32 state_version;
    DSL_FHE_ENCRYPTION_DESCRIPTOR_ID encryption_descriptor_id;
    UINT32 scheme;
    UINT32 value_class;
    INT32 level;
    INT32 scale_bits;
    INT32 component_count;
    INT32 precision_bits;
    UINT32 slot_count;
    UINT32 alignment_group;
    STR_IDX encrypted_layout_name;
    UINT32 pending_actions;
    UINT32 pending_bootstrap_reason;
    UINT32 flags;
    UINT32 reserved;
} DSL_FHE_CONTEXT_CKKS_STATE_RECORD;

/* Producer-runtime inputs. No pointer in these records enters the IR image. */
typedef struct {
    UINT32 disposition;
    UINT32 wrapper_version;
    const char *wrapper_name;
    DSL_FHE_APPROXIMATION_CONTRACT_ID approximation_contract_id;
    DSL_FHE_CKKS_VALUE_STATE_ID result_ckks_value_state_id;
    DSL_FHE_BN_FOLD_PROVENANCE_ID first_bn_fold_id;
    UINT32 bn_fold_count;
    UINT32 flags;
} DSL_FHE_CONVERSION_DISPOSITION_INFO;

typedef struct {
    DSL_FHE_ENCRYPTION_DESCRIPTOR_ID encryption_descriptor_id;
    UINT32 state_version;
    UINT32 scheme;
    UINT32 value_class;
    INT32 level;
    INT32 scale_bits;
    INT32 component_count;
    INT32 precision_bits;
    UINT32 slot_count;
    UINT32 alignment_group;
    const char *encrypted_layout_name;
    UINT32 pending_actions;
    UINT32 pending_bootstrap_reason;
} DSL_FHE_CKKS_VALUE_STATE_INFO;

typedef struct {
    DSL_PU_SOURCE_IDENTITY_ID context_pu_identity_id;
    DSL_CALLSITE_METADATA_ID context_callsite_id;
    DSL_BUILDER_VALUE source_conv_weight;
    DSL_BUILDER_VALUE source_conv_bias;
    DSL_BUILDER_VALUE source_bn_scale;
    DSL_BUILDER_VALUE source_bn_bias;
    DSL_BUILDER_VALUE source_bn_mean;
    DSL_BUILDER_VALUE source_bn_variance;
    TCON_IDX folded_weight_tcon;
    TCON_IDX folded_bias_tcon;
    UINT32 flags;
} DSL_FHE_BN_FOLD_INFO;

extern UINT32 DSL_FHE_Plan_Register_Domain_Wrappers (void);

extern void DSL_FHE_Plan_Image_Reset (void);
extern void DSL_FHE_Plan_Image_Get_Header
                                (DSL_FHE_PLAN_IMAGE_HEADER *header);
extern BOOL DSL_FHE_Plan_Image_Has_Records (void);
extern BOOL DSL_FHE_Plan_Image_Validate (FILE *diagnostic);
extern BOOL DSL_FHE_Plan_Image_Load_Mapped (const void *section_base,
                                            UINT64 section_size,
                                            FILE *diagnostic);
extern void DSL_FHE_Plan_Image_Print (FILE *file);

extern void DSL_FHE_Conversion_Disposition_Record_Init
                                (DSL_FHE_CONVERSION_DISPOSITION_RECORD *record);
extern void DSL_FHE_Approximation_Contract_Record_Init
                                (DSL_FHE_APPROXIMATION_CONTRACT_RECORD *record);
extern void DSL_FHE_CKKS_Value_State_Record_Init
                                (DSL_FHE_CKKS_VALUE_STATE_RECORD *record);
extern void DSL_FHE_BN_Fold_Provenance_Record_Init
                                (DSL_FHE_BN_FOLD_PROVENANCE_RECORD *record);

extern DSL_FHE_CONVERSION_DISPOSITION_ID
    DSL_FHE_Plan_Add_Conversion_Disposition
                                (const DSL_FHE_CONVERSION_DISPOSITION_RECORD
                                     *record);
extern DSL_FHE_APPROXIMATION_CONTRACT_ID
    DSL_FHE_Plan_Intern_Approximation_Contract
                                (const DSL_FHE_APPROXIMATION_CONTRACT_RECORD
                                     *record);
extern DSL_FHE_CKKS_VALUE_STATE_ID DSL_FHE_Plan_Add_CKKS_Value_State
                                (const DSL_FHE_CKKS_VALUE_STATE_RECORD *record);
extern DSL_FHE_BN_FOLD_PROVENANCE_ID
    DSL_FHE_Plan_Add_BN_Fold_Provenance
                                (const DSL_FHE_BN_FOLD_PROVENANCE_RECORD
                                     *record);

extern DSL_FHE_CONVERSION_DISPOSITION_ID
    DSL_Builder_Record_FHE_Conversion_Disposition
                                (DSL_BUILDER_VALUE source_value,
                                 const DSL_FHE_CONVERSION_DISPOSITION_INFO
                                     *info);
extern DSL_FHE_CKKS_VALUE_STATE_ID
    DSL_Builder_Bind_FHE_Value_CKKS_State
                                (DSL_BUILDER_VALUE value,
                                 const DSL_FHE_CKKS_VALUE_STATE_INFO *info);
extern DSL_FHE_BN_FOLD_PROVENANCE_ID DSL_Builder_Record_FHE_BN_Fold
                                (DSL_BUILDER_VALUE conv_value,
                                 DSL_BUILDER_VALUE batch_norm_value,
                                 const DSL_FHE_BN_FOLD_INFO *info);

extern UINT32 DSL_FHE_Plan_Conversion_Disposition_Count (void);
extern UINT32 DSL_FHE_Plan_Approximation_Contract_Count (void);
extern UINT32 DSL_FHE_Plan_CKKS_Value_State_Count (void);
extern UINT32 DSL_FHE_Plan_BN_Fold_Provenance_Count (void);

extern BOOL DSL_FHE_Plan_Get_Conversion_Disposition
                                (DSL_FHE_CONVERSION_DISPOSITION_ID id,
                                 DSL_FHE_CONVERSION_DISPOSITION_RECORD *record);
extern BOOL DSL_FHE_Plan_Get_Approximation_Contract
                                (DSL_FHE_APPROXIMATION_CONTRACT_ID id,
                                 DSL_FHE_APPROXIMATION_CONTRACT_RECORD *record);
extern BOOL DSL_FHE_Plan_Get_CKKS_Value_State
                                (DSL_FHE_CKKS_VALUE_STATE_ID id,
                                 DSL_FHE_CKKS_VALUE_STATE_RECORD *record);
extern BOOL DSL_FHE_Plan_Get_BN_Fold_Provenance
                                (DSL_FHE_BN_FOLD_PROVENANCE_ID id,
                                 DSL_FHE_BN_FOLD_PROVENANCE_RECORD *record);

extern BOOL DSL_FHE_Plan_Find_Conversion_Disposition
                                (DSL_IR_NODE_ID source_node_id,
                                 DSL_FHE_CONVERSION_DISPOSITION_RECORD *record);
extern BOOL DSL_FHE_Plan_Find_CKKS_Value_State
                                (DSL_IR_VALUE_ID value_id,
                                 UINT32 state_version,
                                 DSL_FHE_CKKS_VALUE_STATE_RECORD *record);
extern BOOL DSL_FHE_Plan_Find_Latest_CKKS_Value_State
                                (DSL_IR_VALUE_ID value_id,
                                 DSL_FHE_CKKS_VALUE_STATE_RECORD *record);
extern BOOL DSL_FHE_Plan_Find_BN_Fold_Provenance
                                (DSL_IR_NODE_ID conv_node_id,
                                 DSL_PU_SOURCE_IDENTITY_ID
                                     context_pu_identity_id,
                                 DSL_CALLSITE_METADATA_ID context_callsite_id,
                                 DSL_FHE_BN_FOLD_PROVENANCE_RECORD *record);

extern void DSL_FHE_Approx_Profile_Image_Reset (void);
extern void DSL_FHE_Approx_Profile_Image_Get_Header
                                (DSL_FHE_APPROX_PROFILE_IMAGE_HEADER *header);
extern BOOL DSL_FHE_Approx_Profile_Image_Has_Records (void);
extern BOOL DSL_FHE_Approx_Profile_Image_Validate (FILE *diagnostic);
extern BOOL DSL_FHE_Approx_Profile_Image_Load_Mapped
                                (const void *section_base,
                                 UINT64 section_size,
                                 FILE *diagnostic);
extern void DSL_FHE_Approx_Profile_Image_Print (FILE *file);

extern void DSL_FHE_Composite_Profile_Record_Init
                                (DSL_FHE_COMPOSITE_PROFILE_RECORD *record);
extern void DSL_FHE_Approx_Stage_Record_Init
                                (DSL_FHE_APPROX_STAGE_RECORD *record);
extern void DSL_FHE_Approx_Association_Record_Init
                                (DSL_FHE_APPROX_ASSOCIATION_RECORD *record);
extern void DSL_FHE_Context_Range_Record_Init
                                (DSL_FHE_CONTEXT_RANGE_RECORD *record);

extern DSL_FHE_COMPOSITE_PROFILE_ID
    DSL_FHE_Approx_Profile_Intern_Complete
                                (const DSL_FHE_COMPOSITE_PROFILE_RECORD
                                     *profile,
                                 const DSL_FHE_APPROX_STAGE_RECORD *stages,
                                 UINT32 stage_count);
extern DSL_FHE_CONVERSION_DISPOSITION_ID
    DSL_FHE_Plan_Add_Composite_Disposition
                                (const DSL_FHE_CONVERSION_DISPOSITION_RECORD
                                     *disposition,
                                 DSL_FHE_COMPOSITE_PROFILE_ID profile_id);
extern DSL_FHE_CONTEXT_RANGE_ID
    DSL_FHE_Approx_Profile_Bind_Context_Range
                                (const DSL_FHE_CONTEXT_RANGE_RECORD *record);

extern UINT32 DSL_FHE_Approx_Profile_Count (void);
extern UINT32 DSL_FHE_Approx_Stage_Count (void);
extern UINT32 DSL_FHE_Approx_Association_Count (void);
extern UINT32 DSL_FHE_Context_Range_Count (void);
extern BOOL DSL_FHE_Approx_Profile_Get
                                (DSL_FHE_COMPOSITE_PROFILE_ID id,
                                 DSL_FHE_COMPOSITE_PROFILE_RECORD *record);
extern BOOL DSL_FHE_Approx_Stage_Get
                                (DSL_FHE_APPROX_STAGE_ID id,
                                 DSL_FHE_APPROX_STAGE_RECORD *record);
extern BOOL DSL_FHE_Approx_Association_Get
                                (DSL_FHE_APPROX_ASSOCIATION_ID id,
                                 DSL_FHE_APPROX_ASSOCIATION_RECORD *record);
extern BOOL DSL_FHE_Context_Range_Get
                                (DSL_FHE_CONTEXT_RANGE_ID id,
                                 DSL_FHE_CONTEXT_RANGE_RECORD *record);
extern BOOL DSL_FHE_Approx_Profile_Find
                                (DSL_FHE_CONFIG_ID config_id,
                                 const char *profile_name,
                                 UINT32 profile_version,
                                 DSL_FHE_COMPOSITE_PROFILE_RECORD *record);
extern BOOL DSL_FHE_Approx_Stage_Find
                                (DSL_FHE_COMPOSITE_PROFILE_ID profile_id,
                                 UINT32 stage_ordinal,
                                 DSL_FHE_APPROX_STAGE_RECORD *record);
extern BOOL DSL_FHE_Approx_Association_Find
                                (DSL_FHE_CONVERSION_DISPOSITION_ID
                                     disposition_id,
                                 DSL_FHE_APPROX_ASSOCIATION_RECORD *record);
extern BOOL DSL_FHE_Context_Range_Find
                                (DSL_FHE_COMPOSITE_PROFILE_ID profile_id,
                                 DSL_IR_VALUE_ID source_relu_value_id,
                                 DSL_PU_SOURCE_IDENTITY_ID
                                     context_pu_identity_id,
                                 DSL_CALLSITE_METADATA_ID context_callsite_id,
                                 DSL_FHE_CONTEXT_RANGE_RECORD *record);

extern void DSL_FHE_Context_State_Image_Reset (void);
extern void DSL_FHE_Context_State_Image_Get_Header
                                (DSL_FHE_CONTEXT_STATE_IMAGE_HEADER *header);
extern BOOL DSL_FHE_Context_State_Image_Has_Records (void);
extern BOOL DSL_FHE_Context_State_Image_Validate (FILE *diagnostic);
extern BOOL DSL_FHE_Context_State_Image_Load_Mapped
                                (const void *section_base,
                                 UINT64 section_size,
                                 FILE *diagnostic);
extern void DSL_FHE_Context_State_Image_Print (FILE *file);

extern void DSL_FHE_Context_CKKS_State_Record_Init
                                (DSL_FHE_CONTEXT_CKKS_STATE_RECORD *record);
extern DSL_FHE_CONTEXT_CKKS_STATE_ID
    DSL_FHE_Context_State_Intern
                                (const DSL_FHE_CONTEXT_CKKS_STATE_RECORD
                                     *record);
extern UINT32 DSL_FHE_Context_State_Count (void);
extern BOOL DSL_FHE_Context_State_Get
                                (DSL_FHE_CONTEXT_CKKS_STATE_ID id,
                                 DSL_FHE_CONTEXT_CKKS_STATE_RECORD *record);
extern BOOL DSL_FHE_Context_State_Find
                                (ST_IDX owner_pu_st,
                                 DSL_IR_VALUE_ID source_value_id,
                                 DSL_PU_SOURCE_IDENTITY_ID
                                     context_pu_identity_id,
                                 DSL_CALLSITE_METADATA_ID context_callsite_id,
                                 UINT32 state_role,
                                 UINT32 state_version,
                                 DSL_FHE_CONTEXT_CKKS_STATE_RECORD *record);
extern BOOL DSL_FHE_Context_State_Find_Latest
                                (ST_IDX owner_pu_st,
                                 DSL_IR_VALUE_ID source_value_id,
                                 DSL_PU_SOURCE_IDENTITY_ID
                                     context_pu_identity_id,
                                 DSL_CALLSITE_METADATA_ID context_callsite_id,
                                 UINT32 state_role,
                                 DSL_FHE_CONTEXT_CKKS_STATE_RECORD *record);

#endif /* dsl_fhe_plan_INCLUDED */
