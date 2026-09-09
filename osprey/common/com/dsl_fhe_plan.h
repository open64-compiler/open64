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

#define DSL_FHE_WRAPPER_CNN_CONV2D \
    "fhe.cnn.conv2d"
#define DSL_FHE_WRAPPER_CNN_RESIDUAL_ADD \
    "fhe.cnn.residual_add"
#define DSL_FHE_WRAPPER_CNN_GLOBAL_AVG_POOL2D \
    "fhe.cnn.global_avg_pool2d"
#define DSL_FHE_WRAPPER_CNN_LINEAR \
    "fhe.cnn.linear"

typedef UINT32 DSL_FHE_CONVERSION_DISPOSITION_ID;
typedef UINT32 DSL_FHE_APPROXIMATION_CONTRACT_ID;
typedef UINT32 DSL_FHE_BN_FOLD_PROVENANCE_ID;

#define DSL_FHE_CONVERSION_DISPOSITION_INVALID_ID 0
#define DSL_FHE_APPROXIMATION_CONTRACT_INVALID_ID 0
#define DSL_FHE_BN_FOLD_PROVENANCE_INVALID_ID     0

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
    DSL_FHE_DISPOSITION_REQUIRE_APPROXIMATION = 5
} DSL_FHE_CONVERSION_DISPOSITION;

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
    DSL_FHE_BN_FOLD_SHARED_PU_DEFINITION = 0x00000002
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

#endif /* dsl_fhe_plan_INCLUDED */
