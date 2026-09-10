/*
 * Copyright (C) 2026 Open64 Project
 */

#include <float.h>
#include <string.h>

#include "dsl_domain.h"
#include "dsl_fhe_plan.h"
#include "dsl_opcode.h"
#include "dsl_tensor_fold.h"
#include "segmented_array.h"
#include "strtab.h"
#include "symtab.h"
#include "targ_const.h"

typedef SEGMENTED_ARRAY<DSL_FHE_CONVERSION_DISPOSITION_RECORD>
    DSL_FHE_DISPOSITION_TABLE;
typedef SEGMENTED_ARRAY<DSL_FHE_APPROXIMATION_CONTRACT_RECORD>
    DSL_FHE_APPROXIMATION_TABLE;
typedef SEGMENTED_ARRAY<DSL_FHE_CKKS_VALUE_STATE_RECORD>
    DSL_FHE_CKKS_STATE_TABLE;
typedef SEGMENTED_ARRAY<DSL_FHE_BN_FOLD_PROVENANCE_RECORD>
    DSL_FHE_BN_FOLD_TABLE;
typedef SEGMENTED_ARRAY<DSL_FHE_COMPOSITE_PROFILE_RECORD>
    DSL_FHE_COMPOSITE_PROFILE_TABLE;
typedef SEGMENTED_ARRAY<DSL_FHE_APPROX_STAGE_RECORD>
    DSL_FHE_APPROX_STAGE_TABLE;
typedef SEGMENTED_ARRAY<DSL_FHE_APPROX_ASSOCIATION_RECORD>
    DSL_FHE_APPROX_ASSOCIATION_TABLE;
typedef SEGMENTED_ARRAY<DSL_FHE_CONTEXT_RANGE_RECORD>
    DSL_FHE_CONTEXT_RANGE_TABLE;

static DSL_FHE_DISPOSITION_TABLE DSL_fhe_disposition_table;
static DSL_FHE_APPROXIMATION_TABLE DSL_fhe_approximation_table;
static DSL_FHE_CKKS_STATE_TABLE DSL_fhe_ckks_state_table;
static DSL_FHE_BN_FOLD_TABLE DSL_fhe_bn_fold_table;
static DSL_FHE_COMPOSITE_PROFILE_TABLE DSL_fhe_composite_profile_table;
static DSL_FHE_APPROX_STAGE_TABLE DSL_fhe_approx_stage_table;
static DSL_FHE_APPROX_ASSOCIATION_TABLE DSL_fhe_approx_association_table;
static DSL_FHE_CONTEXT_RANGE_TABLE DSL_fhe_context_range_table;

static BOOL DSL_FHE_Approx_Profile_Cross_Validate (FILE *diagnostic);

typedef struct {
    const DSL_FHE_PLAN_IMAGE_HEADER *header;
    const DSL_FHE_CONVERSION_DISPOSITION_RECORD *dispositions;
    const DSL_FHE_APPROXIMATION_CONTRACT_RECORD *approximations;
    const DSL_FHE_CKKS_VALUE_STATE_RECORD *ckks_states;
    const DSL_FHE_BN_FOLD_PROVENANCE_RECORD *bn_folds;
} DSL_FHE_PLAN_IMAGE_VIEW;

typedef struct {
    const DSL_FHE_APPROX_PROFILE_IMAGE_HEADER *header;
    const DSL_FHE_COMPOSITE_PROFILE_RECORD *profiles;
    const DSL_FHE_APPROX_STAGE_RECORD *stages;
    const DSL_FHE_APPROX_ASSOCIATION_RECORD *associations;
    const DSL_FHE_CONTEXT_RANGE_RECORD *context_ranges;
} DSL_FHE_APPROX_PROFILE_IMAGE_VIEW;

typedef char DSL_FHE_Plan_TY_IDX_Width_Check
    [sizeof(TY_IDX) == 4 ? 1 : -1];
typedef char DSL_FHE_Plan_ST_IDX_Width_Check
    [sizeof(ST_IDX) == 4 ? 1 : -1];
typedef char DSL_FHE_Plan_TCON_IDX_Width_Check
    [sizeof(TCON_IDX) == 4 ? 1 : -1];
typedef char DSL_FHE_Plan_STR_IDX_Width_Check
    [sizeof(STR_IDX) == 8 ? 1 : -1];
typedef char DSL_FHE_Plan_Header_Size_Check
    [sizeof(DSL_FHE_PLAN_IMAGE_HEADER) ==
        DSL_FHE_PLAN_IMAGE_HEADER_SIZE ? 1 : -1];
typedef char DSL_FHE_Plan_Disposition_Size_Check
    [sizeof(DSL_FHE_CONVERSION_DISPOSITION_RECORD) ==
        DSL_FHE_PLAN_DISPOSITION_RECORD_SIZE ? 1 : -1];
typedef char DSL_FHE_Plan_Approximation_Size_Check
    [sizeof(DSL_FHE_APPROXIMATION_CONTRACT_RECORD) ==
        DSL_FHE_PLAN_APPROXIMATION_RECORD_SIZE ? 1 : -1];
typedef char DSL_FHE_Plan_CKKS_State_Size_Check
    [sizeof(DSL_FHE_CKKS_VALUE_STATE_RECORD) ==
        DSL_FHE_PLAN_CKKS_STATE_RECORD_SIZE ? 1 : -1];
typedef char DSL_FHE_Plan_BN_Fold_Size_Check
    [sizeof(DSL_FHE_BN_FOLD_PROVENANCE_RECORD) ==
        DSL_FHE_PLAN_BN_FOLD_RECORD_SIZE ? 1 : -1];
typedef char DSL_FHE_Approx_Profile_Header_Size_Check
    [sizeof(DSL_FHE_APPROX_PROFILE_IMAGE_HEADER) ==
        DSL_FHE_APPROX_PROFILE_IMAGE_HEADER_SIZE ? 1 : -1];
typedef char DSL_FHE_Composite_Profile_Size_Check
    [sizeof(DSL_FHE_COMPOSITE_PROFILE_RECORD) ==
        DSL_FHE_COMPOSITE_PROFILE_RECORD_SIZE ? 1 : -1];
typedef char DSL_FHE_Approx_Stage_Size_Check
    [sizeof(DSL_FHE_APPROX_STAGE_RECORD) ==
        DSL_FHE_APPROX_STAGE_RECORD_SIZE ? 1 : -1];
typedef char DSL_FHE_Approx_Association_Size_Check
    [sizeof(DSL_FHE_APPROX_ASSOCIATION_RECORD) ==
        DSL_FHE_APPROX_ASSOCIATION_RECORD_SIZE ? 1 : -1];
typedef char DSL_FHE_Context_Range_Size_Check
    [sizeof(DSL_FHE_CONTEXT_RANGE_RECORD) ==
        DSL_FHE_CONTEXT_RANGE_RECORD_SIZE ? 1 : -1];

template <typename RECORD>
static void
DSL_FHE_Plan_Record_Init (RECORD *record)
{
    if (record != NULL)
        memset(record, 0, sizeof(RECORD));
}

template <typename TABLE, typename RECORD>
static BOOL
DSL_FHE_Plan_Table_Get (TABLE &table, UINT32 id, RECORD *record)
{
    if (id == 0 || id > table.Size())
        return FALSE;
    if (record != NULL)
        *record = table[id - 1];
    return TRUE;
}

template <typename RECORD>
static BOOL
DSL_FHE_Plan_Equivalent_Record (const RECORD &left, const RECORD &right)
{
    RECORD left_copy = left;
    RECORD right_copy = right;
    left_copy.id = 0;
    right_copy.id = 0;
    return memcmp(&left_copy, &right_copy, sizeof(RECORD)) == 0;
}

static BOOL
DSL_FHE_Plan_Report (FILE *diagnostic, const char *message, UINT32 id)
{
    if (diagnostic != NULL)
        fprintf(diagnostic, "FHE plan image error: %s id=%u\n", message, id);
    return FALSE;
}

static BOOL
DSL_FHE_Plan_String_Id_Valid (STR_IDX id, BOOL required)
{
    if (id == STR_IDX_ZERO)
        return !required;
    return id < STR_Table_Size();
}

static BOOL
DSL_FHE_Plan_PU_ST_Valid (ST_IDX st)
{
    return ST_IDX_level(st) == GLOBAL_SYMTAB && ST_IDX_index(st) != 0 &&
           ST_IDX_index(st) < ST_Table_Size(GLOBAL_SYMTAB) &&
           ST_class(st) == CLASS_FUNC;
}

static BOOL
DSL_FHE_Plan_Value_Belongs_To_PU
        (const DSL_IR_VALUE_RECORD &value, ST_IDX owner_pu_st)
{
    DSL_IR_VALUE_RECORD owned_value;
    const char *owner_pu_name;

    if (!DSL_FHE_Plan_PU_ST_Valid(owner_pu_st) ||
        value.name == STR_IDX_ZERO)
        return FALSE;
    owner_pu_name = ST_name(St_Table[owner_pu_st]);
    return DSL_IR_Image_Find_PU_Value
               (value.st, Index_To_Str(value.name), owner_pu_name,
                &owned_value) &&
           owned_value.id == value.id;
}

static BOOL
DSL_FHE_Plan_Range_Valid (UINT32 first, UINT32 count, UINT32 limit)
{
    if (count == 0)
        return first == 0;
    return first != 0 && count <= limit && first <= limit - count + 1;
}

static UINT32
DSL_FHE_Plan_View_Disposition_Count (const DSL_FHE_PLAN_IMAGE_VIEW *view)
{
    return view == NULL ? DSL_fhe_disposition_table.Size() :
                          view->header->disposition_count;
}

static UINT32
DSL_FHE_Plan_View_Approximation_Count (const DSL_FHE_PLAN_IMAGE_VIEW *view)
{
    return view == NULL ? DSL_fhe_approximation_table.Size() :
                          view->header->approximation_count;
}

static UINT32
DSL_FHE_Plan_View_CKKS_State_Count (const DSL_FHE_PLAN_IMAGE_VIEW *view)
{
    return view == NULL ? DSL_fhe_ckks_state_table.Size() :
                          view->header->ckks_value_state_count;
}

static UINT32
DSL_FHE_Plan_View_BN_Fold_Count (const DSL_FHE_PLAN_IMAGE_VIEW *view)
{
    return view == NULL ? DSL_fhe_bn_fold_table.Size() :
                          view->header->bn_fold_count;
}

static const DSL_FHE_CONVERSION_DISPOSITION_RECORD &
DSL_FHE_Plan_View_Disposition
        (const DSL_FHE_PLAN_IMAGE_VIEW *view, UINT32 ordinal)
{
    return view == NULL ? DSL_fhe_disposition_table[ordinal] :
                          view->dispositions[ordinal];
}

static const DSL_FHE_APPROXIMATION_CONTRACT_RECORD &
DSL_FHE_Plan_View_Approximation
        (const DSL_FHE_PLAN_IMAGE_VIEW *view, UINT32 ordinal)
{
    return view == NULL ? DSL_fhe_approximation_table[ordinal] :
                          view->approximations[ordinal];
}

static const DSL_FHE_CKKS_VALUE_STATE_RECORD &
DSL_FHE_Plan_View_CKKS_State
        (const DSL_FHE_PLAN_IMAGE_VIEW *view, UINT32 ordinal)
{
    return view == NULL ? DSL_fhe_ckks_state_table[ordinal] :
                          view->ckks_states[ordinal];
}

static const DSL_FHE_BN_FOLD_PROVENANCE_RECORD &
DSL_FHE_Plan_View_BN_Fold
        (const DSL_FHE_PLAN_IMAGE_VIEW *view, UINT32 ordinal)
{
    return view == NULL ? DSL_fhe_bn_fold_table[ordinal] :
                          view->bn_folds[ordinal];
}

static BOOL
DSL_FHE_Plan_Node_Operator
        (DSL_IR_NODE_ID node_id, DSL_OPERATOR *dsl_operator,
         UINT32 *version)
{
    DSL_IR_NODE_RECORD node;
    DSL_IR_OPCODE_DESCRIPTOR_RECORD descriptor;

    if (!DSL_IR_Image_Get_Node(node_id, &node) ||
        !DSL_IR_Image_Get_Opcode_Descriptor
            (node.opcode_descriptor_id, &descriptor))
        return FALSE;
    if (dsl_operator != NULL)
        *dsl_operator = (DSL_OPERATOR)descriptor.logical_operator;
    if (version != NULL)
        *version = descriptor.version;
    return TRUE;
}

static BOOL
DSL_FHE_Plan_Tensor_TCON_Valid (TCON_IDX tcon_idx)
{
    DSL_TENSOR_TCON_RECORD tensor;
    return DSL_Tensor_TCON_Get(tcon_idx, &tensor);
}

static BOOL
DSL_FHE_Plan_Scalar_TCON_Value (TCON_IDX tcon_idx, double *value)
{
    if (tcon_idx == TCON_IDX_ZERO || tcon_idx >= TCON_Table_Size())
        return FALSE;
    const TCON &tcon = Tcon_Table[tcon_idx];
    if (TCON_ty(tcon) != MTYPE_F4 && TCON_ty(tcon) != MTYPE_F8)
        return FALSE;
    double converted = Targ_To_Host_Float(tcon);
    if (converted != converted || converted > DBL_MAX || converted < -DBL_MAX)
        return FALSE;
    if (value != NULL)
        *value = converted;
    return TRUE;
}

static BOOL
DSL_FHE_Plan_Approximation_Valid
        (const DSL_FHE_APPROXIMATION_CONTRACT_RECORD &record)
{
    DSL_FHE_COMPILATION_CONFIG_RECORD config;
    DSL_TENSOR_TCON_RECORD coefficients;
    double range_min;
    double range_max;
    double max_error;

    return DSL_FHE_Get_Compilation_Config(record.config_id, &config) &&
           DSL_FHE_Plan_String_Id_Valid(record.polynomial_name, TRUE) &&
           record.approximation_family >= DSL_FHE_APPROXIMATION_MINIMAX &&
           record.approximation_family <= DSL_FHE_APPROXIMATION_TAYLOR &&
           record.polynomial_version != 0 &&
           DSL_Tensor_TCON_Get(record.coefficient_tensor_tcon,
                               &coefficients) &&
           coefficients.element_count == (UINT64)record.degree + 1 &&
           DSL_FHE_Plan_Scalar_TCON_Value(record.valid_range_min_tcon,
                                          &range_min) &&
           DSL_FHE_Plan_Scalar_TCON_Value(record.valid_range_max_tcon,
                                          &range_max) &&
           DSL_FHE_Plan_Scalar_TCON_Value(record.max_abs_error_tcon,
                                          &max_error) &&
           range_min < range_max && max_error >= 0.0 &&
           record.scale_policy >= DSL_FHE_APPROX_SCALE_INHERIT &&
           record.scale_policy <= DSL_FHE_APPROX_SCALE_EXPLICIT &&
           record.bootstrap_policy >= DSL_FHE_BOOTSTRAP_AUTO &&
           record.bootstrap_policy <= DSL_FHE_BOOTSTRAP_OFF &&
           record.requires_pre_refresh <= 1 && record.flags == 0;
}

static BOOL
DSL_FHE_Plan_CKKS_State_Valid
        (const DSL_FHE_CKKS_VALUE_STATE_RECORD &record)
{
    DSL_IR_VALUE_RECORD value;
    DSL_FHE_ENCRYPTION_DESCRIPTOR_RECORD descriptor;
    const UINT32 known_actions = DSL_FHE_CKKS_PENDING_RESCALE |
                                 DSL_FHE_CKKS_PENDING_RELINEARIZE |
                                 DSL_FHE_CKKS_PENDING_BOOTSTRAP;

    if (!DSL_IR_Image_Get_Value(record.value_id, &value) ||
        !DSL_FHE_Get_Encryption_Descriptor
            (record.encryption_descriptor_id, &descriptor) ||
        record.state_version == 0 || record.scheme != DSL_FHE_SCHEME_CKKS ||
        descriptor.scheme != record.scheme ||
        descriptor.value_class != record.value_class ||
        (record.value_class != DSL_FHE_VALUE_CLASS_CIPHERTEXT &&
         record.value_class != DSL_FHE_VALUE_CLASS_ENCODED_PLAINTEXT) ||
        record.level < -1 || record.scale_bits < -1 ||
        record.component_count < -1 || record.precision_bits < -1 ||
        !DSL_FHE_Plan_String_Id_Valid
            (record.encrypted_layout_name, FALSE) ||
        (record.pending_actions & ~known_actions) != 0 ||
        record.pending_bootstrap_reason >
            DSL_FHE_BOOTSTRAP_REASON_MANUAL_BOUNDARY_REQUIRED)
        return FALSE;

    if ((record.pending_actions & DSL_FHE_CKKS_PENDING_BOOTSTRAP) == 0)
        return record.pending_bootstrap_reason ==
                   DSL_FHE_BOOTSTRAP_REASON_NONE;
    return record.pending_bootstrap_reason !=
               DSL_FHE_BOOTSTRAP_REASON_NONE;
}

static BOOL
DSL_FHE_Plan_BN_Fold_Valid
        (const DSL_FHE_BN_FOLD_PROVENANCE_RECORD &record)
{
    DSL_PU_SOURCE_IDENTITY_RECORD identity;
    DSL_CALLSITE_METADATA_RECORD callsite;
    DSL_OPERATOR conv_operator;
    DSL_OPERATOR bn_operator;
    const UINT32 known_flags = DSL_FHE_BN_FOLD_IMPLICIT_ZERO_BIAS |
                               DSL_FHE_BN_FOLD_SHARED_PU_DEFINITION;
    DSL_IR_VALUE_RECORD value;

    if (!DSL_FHE_Plan_PU_ST_Valid(record.owner_pu_st) ||
        !DSL_FHE_Plan_Node_Operator
            (record.conv_node_id, &conv_operator, NULL) ||
        conv_operator != OPR_DSLCONV2D ||
        !DSL_FHE_Plan_Node_Operator
            (record.batch_norm_node_id, &bn_operator, NULL) ||
        bn_operator != OPR_DSLBATCHNORMINFER ||
        !DSL_Call_Image_Get_PU_Identity
            (record.context_pu_identity_id, &identity) ||
        (record.flags & ~known_flags) != 0 || record.reserved != 0)
        return FALSE;

    if (record.context_callsite_id == DSL_CALLSITE_METADATA_INVALID_ID) {
        if (identity.owner_pu_st != record.owner_pu_st)
            return FALSE;
    } else {
        if (!DSL_Call_Image_Get_Callsite
                (record.context_callsite_id, &callsite) ||
            callsite.callee_pu_st != record.owner_pu_st ||
            callsite.owner_pu_st != identity.owner_pu_st)
            return FALSE;
    }

    const DSL_IR_VALUE_ID required_values[] = {
        record.source_conv_weight_value_id,
        record.source_bn_scale_value_id,
        record.source_bn_bias_value_id,
        record.source_bn_mean_value_id,
        record.source_bn_variance_value_id
    };
    for (UINT32 i = 0; i < sizeof(required_values) /
                               sizeof(required_values[0]); ++i) {
        if (!DSL_IR_Image_Get_Value(required_values[i], &value))
            return FALSE;
    }
    if (record.source_conv_bias_value_id == DSL_IR_VALUE_INVALID_ID) {
        if ((record.flags & DSL_FHE_BN_FOLD_IMPLICIT_ZERO_BIAS) == 0)
            return FALSE;
    } else if (!DSL_IR_Image_Get_Value
                    (record.source_conv_bias_value_id, &value) ||
               (record.flags & DSL_FHE_BN_FOLD_IMPLICIT_ZERO_BIAS) != 0) {
        return FALSE;
    }

    return DSL_FHE_Plan_Tensor_TCON_Valid(record.folded_weight_tcon) &&
           DSL_FHE_Plan_Tensor_TCON_Valid(record.folded_bias_tcon);
}

static BOOL
DSL_FHE_Plan_Wrapper_Valid
        (const DSL_FHE_CONVERSION_DISPOSITION_RECORD &record,
         const DSL_IR_OPCODE_DESCRIPTOR_RECORD &source_descriptor)
{
    DSL_DOMAIN_ID fhe_cnn_id;
    DSL_OPCODE_ID wrapper_id;
    DSL_OPCODE_INFO wrapper;
    DSL_OPCODE_INFO target;

    if (!DSL_FHE_Plan_String_Id_Valid(record.wrapper_name, TRUE) ||
        record.wrapper_version == 0 ||
        DSL_FHE_Plan_Register_Domain_Wrappers() == 0)
        return FALSE;
    fhe_cnn_id = DSL_Domain_Find("fhe.cnn");
    wrapper_id = DSL_Opcode_Find
                     (fhe_cnn_id, Index_To_Str(record.wrapper_name),
                      (UINT16)record.wrapper_version);
    return wrapper_id != DSL_OPCODE_INVALID_ID &&
           DSL_Opcode_Get_Info(wrapper_id, &wrapper) &&
           wrapper.wrapper_target_id != DSL_OPCODE_INVALID_ID &&
           DSL_Opcode_Get_Info(wrapper.wrapper_target_id, &target) &&
           target.version == source_descriptor.version &&
           strcmp(target.name, Index_To_Str(source_descriptor.stable_name)) ==
               0;
}

static BOOL
DSL_FHE_Plan_Disposition_Valid
        (const DSL_FHE_CONVERSION_DISPOSITION_RECORD &record,
         const DSL_FHE_PLAN_IMAGE_VIEW *view)
{
    DSL_IR_NODE_RECORD node;
    DSL_IR_VALUE_RECORD value;
    DSL_IR_OPCODE_DESCRIPTOR_RECORD descriptor;
    const UINT32 known_flags = DSL_FHE_DISPOSITION_DEFINITION_REWRITE |
                               DSL_FHE_DISPOSITION_CONTEXT_SENSITIVE |
                               DSL_FHE_DISPOSITION_NO_DATA_MOVEMENT |
                               DSL_FHE_DISPOSITION_OUTPUT_ENCRYPTED;

    if (!DSL_IR_Image_Get_Node(record.source_node_id, &node) ||
        !DSL_IR_Image_Get_Opcode_Descriptor
            (node.opcode_descriptor_id, &descriptor) ||
        node.result_value_id != record.result_value_id ||
        !DSL_IR_Image_Get_Value(record.result_value_id, &value) ||
        value.producer_node_id != record.source_node_id ||
        !DSL_FHE_Plan_Value_Belongs_To_PU(value, record.owner_pu_st) ||
        record.disposition < DSL_FHE_DISPOSITION_PRESERVE ||
        record.disposition >
            DSL_FHE_DISPOSITION_REQUIRE_COMPOSITE_APPROXIMATION ||
        (record.flags & ~known_flags) != 0 || record.reserved != 0 ||
        !DSL_FHE_Plan_Range_Valid
            (record.first_bn_fold_id, record.bn_fold_count,
             DSL_FHE_Plan_View_BN_Fold_Count(view)))
        return FALSE;

    if (record.disposition == DSL_FHE_DISPOSITION_DOMAIN_WRAPPER) {
        if (!DSL_FHE_Plan_Wrapper_Valid(record, descriptor))
            return FALSE;
    } else if (record.wrapper_name != STR_IDX_ZERO ||
               record.wrapper_version != 0) {
        return FALSE;
    }

    if (record.disposition == DSL_FHE_DISPOSITION_REQUIRE_APPROXIMATION) {
        if (record.approximation_contract_id == 0 ||
            record.approximation_contract_id >
                DSL_FHE_Plan_View_Approximation_Count(view))
            return FALSE;
    } else if (record.disposition ==
                   DSL_FHE_DISPOSITION_REQUIRE_COMPOSITE_APPROXIMATION) {
        if (record.approximation_contract_id == 0)
            return FALSE;
    } else if (record.approximation_contract_id != 0) {
        return FALSE;
    }

    if (record.result_ckks_value_state_id == 0 ||
        record.result_ckks_value_state_id >
            DSL_FHE_Plan_View_CKKS_State_Count(view) ||
        DSL_FHE_Plan_View_CKKS_State
            (view, record.result_ckks_value_state_id - 1).value_id !=
                record.result_value_id)
        return FALSE;

    for (UINT32 i = 0; i < record.bn_fold_count; ++i) {
        const DSL_FHE_BN_FOLD_PROVENANCE_RECORD &fold =
            DSL_FHE_Plan_View_BN_Fold
                (view, record.first_bn_fold_id - 1 + i);
        if (fold.conv_node_id != record.source_node_id ||
            fold.owner_pu_st != record.owner_pu_st)
            return FALSE;
    }
    return TRUE;
}

UINT32
DSL_FHE_Plan_Register_Domain_Wrappers (void)
{
    typedef struct {
        const char *wrapper_name;
        UINT16 wrapper_version;
        const char *target_domain;
        const char *target_name;
        UINT16 target_version;
        const char *diagnostic_prefix;
    } DSL_FHE_WRAPPER_SEED;
    static const DSL_FHE_WRAPPER_SEED seeds[] = {
        { DSL_FHE_WRAPPER_CNN_CONV2D, 1, "cnn", "cnn.conv2d", 2,
          "CFHECNN_CONV2D" },
        { DSL_FHE_WRAPPER_CNN_RESIDUAL_ADD, 1, "common",
          "common.residual_add", 2, "CFHECNN_RESIDUAL_ADD" },
        { DSL_FHE_WRAPPER_CNN_GLOBAL_AVG_POOL2D, 1, "cnn",
          "cnn.global_avg_pool2d", 2, "CFHECNN_GLOBAL_AVG_POOL2D" },
        { DSL_FHE_WRAPPER_CNN_LINEAR,
          DSL_FHE_WRAPPER_CNN_LINEAR_COMMON_V3_VERSION,
          "common", "common.linear", 3, "CFHECNN_LINEAR" },
        { DSL_FHE_WRAPPER_CNN_LINEAR,
          DSL_FHE_WRAPPER_CNN_LINEAR_COMMON_V2_VERSION,
          "common", "common.linear", 2, "CFHECNN_LINEAR_V2" }
    };
    UINT32 registered = 0;

    DSL_Opcode_Register_Domain_Wrapper_Examples();
    DSL_DOMAIN_ID common_id = DSL_Domain_Find("common");
    DSL_DOMAIN_ID fhe_id = DSL_Domain_Find("fhe");
    if (common_id == DSL_DOMAIN_INVALID_ID)
        return 0;
    if (fhe_id == DSL_DOMAIN_INVALID_ID)
        fhe_id = DSL_Domain_Register("fhe", common_id, 1, 0);
    DSL_DOMAIN_ID fhe_cnn_id = DSL_Domain_Find("fhe.cnn");
    if (fhe_cnn_id == DSL_DOMAIN_INVALID_ID)
        fhe_cnn_id = DSL_Domain_Register("fhe.cnn", fhe_id, 1, 0);
    if (fhe_id == DSL_DOMAIN_INVALID_ID ||
        fhe_cnn_id == DSL_DOMAIN_INVALID_ID)
        return 0;

    for (UINT32 i = 0; i < sizeof(seeds) / sizeof(seeds[0]); ++i) {
        DSL_DOMAIN_ID target_domain = DSL_Domain_Find(seeds[i].target_domain);
        DSL_OPCODE_ID target = DSL_Opcode_Find
                                   (target_domain, seeds[i].target_name,
                                    seeds[i].target_version);
        DSL_OPCODE_ID wrapper = DSL_Opcode_Register_Domain_Wrapper
                                    (fhe_cnn_id, seeds[i].wrapper_name,
                                     seeds[i].wrapper_version,
                                     target, seeds[i].diagnostic_prefix, 0);
        if (wrapper != DSL_OPCODE_INVALID_ID)
            ++registered;
    }
    return registered;
}

void
DSL_FHE_Plan_Image_Reset (void)
{
    DSL_fhe_disposition_table.Delete_down_to(0);
    DSL_fhe_approximation_table.Delete_down_to(0);
    DSL_fhe_ckks_state_table.Delete_down_to(0);
    DSL_fhe_bn_fold_table.Delete_down_to(0);
    DSL_FHE_Approx_Profile_Image_Reset();
}

void
DSL_FHE_Plan_Image_Get_Header (DSL_FHE_PLAN_IMAGE_HEADER *header)
{
    if (header == NULL)
        return;
    memset(header, 0, sizeof(*header));
    header->magic = DSL_FHE_PLAN_IMAGE_MAGIC;
    header->version = DSL_FHE_PLAN_IMAGE_VERSION;
    header->header_size = DSL_FHE_PLAN_IMAGE_HEADER_SIZE;
    header->record_kind_count = DSL_FHE_PLAN_RECORD_BN_FOLD;
    header->capabilities = DSL_FHE_PLAN_CAP_DISPOSITION |
                           DSL_FHE_PLAN_CAP_APPROXIMATION |
                           DSL_FHE_PLAN_CAP_CKKS_VALUE_STATE |
                           DSL_FHE_PLAN_CAP_BN_FOLD;
    header->disposition_count = DSL_fhe_disposition_table.Size();
    header->approximation_count = DSL_fhe_approximation_table.Size();
    header->ckks_value_state_count = DSL_fhe_ckks_state_table.Size();
    header->bn_fold_count = DSL_fhe_bn_fold_table.Size();
}

BOOL
DSL_FHE_Plan_Image_Has_Records (void)
{
    return DSL_fhe_disposition_table.Size() != 0 ||
           DSL_fhe_approximation_table.Size() != 0 ||
           DSL_fhe_ckks_state_table.Size() != 0 ||
           DSL_fhe_bn_fold_table.Size() != 0;
}

static BOOL
DSL_FHE_Plan_View_Validate
        (const DSL_FHE_PLAN_IMAGE_VIEW *view, FILE *diagnostic)
{
    DSL_FHE_PLAN_IMAGE_HEADER header;
    if (view == NULL)
        DSL_FHE_Plan_Image_Get_Header(&header);
    else
        header = *view->header;
    const UINT32 capabilities = DSL_FHE_PLAN_CAP_DISPOSITION |
                                DSL_FHE_PLAN_CAP_APPROXIMATION |
                                DSL_FHE_PLAN_CAP_CKKS_VALUE_STATE |
                                DSL_FHE_PLAN_CAP_BN_FOLD;
    if (header.magic != DSL_FHE_PLAN_IMAGE_MAGIC ||
        header.version != DSL_FHE_PLAN_IMAGE_VERSION ||
        header.header_size != DSL_FHE_PLAN_IMAGE_HEADER_SIZE ||
        header.record_kind_count != DSL_FHE_PLAN_RECORD_BN_FOLD ||
        header.capabilities != capabilities || header.flags != 0 ||
        header.reserved0 != 0 || header.reserved1 != 0 ||
        header.reserved2 != 0 || header.reserved3 != 0 ||
        header.reserved4 != 0 || header.reserved5 != 0)
        return DSL_FHE_Plan_Report(diagnostic, "invalid header", 0);

    for (UINT32 i = 0;
         i < DSL_FHE_Plan_View_Approximation_Count(view); ++i) {
        const DSL_FHE_APPROXIMATION_CONTRACT_RECORD &record =
            DSL_FHE_Plan_View_Approximation(view, i);
        if (record.id != i + 1 ||
            !DSL_FHE_Plan_Approximation_Valid(record))
            return DSL_FHE_Plan_Report
                       (diagnostic, "invalid approximation", i + 1);
    }
    for (UINT32 i = 0;
         i < DSL_FHE_Plan_View_CKKS_State_Count(view); ++i) {
        const DSL_FHE_CKKS_VALUE_STATE_RECORD &record =
            DSL_FHE_Plan_View_CKKS_State(view, i);
        if (record.id != i + 1 || !DSL_FHE_Plan_CKKS_State_Valid(record))
            return DSL_FHE_Plan_Report
                       (diagnostic, "invalid CKKS value state", i + 1);
        UINT32 expected_version = 1;
        for (UINT32 j = 0; j < i; ++j) {
            if (DSL_FHE_Plan_View_CKKS_State(view, j).value_id ==
                record.value_id)
                ++expected_version;
        }
        if (record.state_version != expected_version)
            return DSL_FHE_Plan_Report
                       (diagnostic, "noncontiguous CKKS state version", i + 1);
    }
    for (UINT32 i = 0;
         i < DSL_FHE_Plan_View_BN_Fold_Count(view); ++i) {
        const DSL_FHE_BN_FOLD_PROVENANCE_RECORD &record =
            DSL_FHE_Plan_View_BN_Fold(view, i);
        if (record.id != i + 1 || !DSL_FHE_Plan_BN_Fold_Valid(record))
            return DSL_FHE_Plan_Report
                       (diagnostic, "invalid BatchNorm fold", i + 1);
        for (UINT32 j = 0; j < i; ++j) {
            const DSL_FHE_BN_FOLD_PROVENANCE_RECORD &prior =
                DSL_FHE_Plan_View_BN_Fold(view, j);
            if (prior.conv_node_id == record.conv_node_id &&
                prior.batch_norm_node_id == record.batch_norm_node_id &&
                prior.context_pu_identity_id ==
                    record.context_pu_identity_id &&
                prior.context_callsite_id == record.context_callsite_id)
                return DSL_FHE_Plan_Report
                           (diagnostic, "duplicate BatchNorm fold", i + 1);
        }
    }
    for (UINT32 i = 0;
         i < DSL_FHE_Plan_View_Disposition_Count(view); ++i) {
        const DSL_FHE_CONVERSION_DISPOSITION_RECORD &record =
            DSL_FHE_Plan_View_Disposition(view, i);
        if (record.id != i + 1 ||
            !DSL_FHE_Plan_Disposition_Valid(record, view))
            return DSL_FHE_Plan_Report
                       (diagnostic, "invalid disposition", i + 1);
        for (UINT32 j = 0; j < i; ++j) {
            if (DSL_FHE_Plan_View_Disposition(view, j).source_node_id ==
                    record.source_node_id)
                return DSL_FHE_Plan_Report
                           (diagnostic, "duplicate disposition", i + 1);
        }
    }
    for (UINT32 i = 0;
         i < DSL_FHE_Plan_View_BN_Fold_Count(view); ++i) {
        UINT32 owners = 0;
        for (UINT32 j = 0;
             j < DSL_FHE_Plan_View_Disposition_Count(view); ++j) {
            const DSL_FHE_CONVERSION_DISPOSITION_RECORD &disposition =
                DSL_FHE_Plan_View_Disposition(view, j);
            if (disposition.bn_fold_count != 0 &&
                i + 1 >= disposition.first_bn_fold_id &&
                i + 1 < disposition.first_bn_fold_id +
                            disposition.bn_fold_count)
                ++owners;
        }
        if (owners != 1)
            return DSL_FHE_Plan_Report
                       (diagnostic, "unowned BatchNorm fold", i + 1);
    }
    return TRUE;
}

BOOL
DSL_FHE_Plan_Image_Validate (FILE *diagnostic)
{
    return DSL_FHE_Plan_View_Validate(NULL, diagnostic) &&
           DSL_FHE_Approx_Profile_Cross_Validate(diagnostic);
}

static BOOL
DSL_FHE_Plan_Add_Section_Size
        (UINT64 *size, UINT32 count, UINT32 record_size)
{
    const UINT64 max_size = (UINT64)-1;
    if (count != 0 && count > (max_size - *size) / record_size)
        return FALSE;
    *size += (UINT64)count * record_size;
    return TRUE;
}

BOOL
DSL_FHE_Plan_Image_Load_Mapped
        (const void *section_base, UINT64 section_size, FILE *diagnostic)
{
    if (section_base == NULL || section_size < DSL_FHE_PLAN_IMAGE_HEADER_SIZE)
        return DSL_FHE_Plan_Report(diagnostic, "section is truncated", 0);
    const char *cursor = (const char *)section_base;
    const DSL_FHE_PLAN_IMAGE_HEADER *header =
        (const DSL_FHE_PLAN_IMAGE_HEADER *)cursor;
    UINT64 expected_size = DSL_FHE_PLAN_IMAGE_HEADER_SIZE;
    if (!DSL_FHE_Plan_Add_Section_Size
             (&expected_size, header->disposition_count,
              DSL_FHE_PLAN_DISPOSITION_RECORD_SIZE) ||
        !DSL_FHE_Plan_Add_Section_Size
             (&expected_size, header->approximation_count,
              DSL_FHE_PLAN_APPROXIMATION_RECORD_SIZE) ||
        !DSL_FHE_Plan_Add_Section_Size
             (&expected_size, header->ckks_value_state_count,
              DSL_FHE_PLAN_CKKS_STATE_RECORD_SIZE) ||
        !DSL_FHE_Plan_Add_Section_Size
             (&expected_size, header->bn_fold_count,
              DSL_FHE_PLAN_BN_FOLD_RECORD_SIZE) ||
        expected_size != section_size)
        return DSL_FHE_Plan_Report
                   (diagnostic, "section size mismatch", 0);

    DSL_FHE_PLAN_IMAGE_VIEW view;
    view.header = header;
    cursor += DSL_FHE_PLAN_IMAGE_HEADER_SIZE;
    view.dispositions =
        (const DSL_FHE_CONVERSION_DISPOSITION_RECORD *)cursor;
    cursor += (UINT64)header->disposition_count *
              DSL_FHE_PLAN_DISPOSITION_RECORD_SIZE;
    view.approximations =
        (const DSL_FHE_APPROXIMATION_CONTRACT_RECORD *)cursor;
    cursor += (UINT64)header->approximation_count *
              DSL_FHE_PLAN_APPROXIMATION_RECORD_SIZE;
    view.ckks_states = (const DSL_FHE_CKKS_VALUE_STATE_RECORD *)cursor;
    cursor += (UINT64)header->ckks_value_state_count *
              DSL_FHE_PLAN_CKKS_STATE_RECORD_SIZE;
    view.bn_folds = (const DSL_FHE_BN_FOLD_PROVENANCE_RECORD *)cursor;

    if (!DSL_FHE_Plan_View_Validate(&view, diagnostic))
        return FALSE;

    DSL_FHE_Plan_Image_Reset();
    if (header->disposition_count != 0)
        DSL_fhe_disposition_table.Insert
            (view.dispositions, header->disposition_count);
    if (header->approximation_count != 0)
        DSL_fhe_approximation_table.Insert
            (view.approximations, header->approximation_count);
    if (header->ckks_value_state_count != 0)
        DSL_fhe_ckks_state_table.Insert
            (view.ckks_states, header->ckks_value_state_count);
    if (header->bn_fold_count != 0)
        DSL_fhe_bn_fold_table.Insert(view.bn_folds, header->bn_fold_count);
    return TRUE;
}

void DSL_FHE_Conversion_Disposition_Record_Init
        (DSL_FHE_CONVERSION_DISPOSITION_RECORD *record)
{ DSL_FHE_Plan_Record_Init(record); }
void DSL_FHE_Approximation_Contract_Record_Init
        (DSL_FHE_APPROXIMATION_CONTRACT_RECORD *record)
{ DSL_FHE_Plan_Record_Init(record); }
void DSL_FHE_CKKS_Value_State_Record_Init
        (DSL_FHE_CKKS_VALUE_STATE_RECORD *record)
{ DSL_FHE_Plan_Record_Init(record); }
void DSL_FHE_BN_Fold_Provenance_Record_Init
        (DSL_FHE_BN_FOLD_PROVENANCE_RECORD *record)
{ DSL_FHE_Plan_Record_Init(record); }

DSL_FHE_APPROXIMATION_CONTRACT_ID
DSL_FHE_Plan_Intern_Approximation_Contract
        (const DSL_FHE_APPROXIMATION_CONTRACT_RECORD *record)
{
    if (record == NULL || !DSL_FHE_Plan_Approximation_Valid(*record))
        return DSL_FHE_APPROXIMATION_CONTRACT_INVALID_ID;
    for (UINT32 i = 0; i < DSL_fhe_approximation_table.Size(); ++i) {
        if (DSL_FHE_Plan_Equivalent_Record
                (DSL_fhe_approximation_table[i], *record))
            return i + 1;
    }
    DSL_FHE_APPROXIMATION_CONTRACT_RECORD copy = *record;
    UINT32 index = DSL_fhe_approximation_table.Insert(copy);
    DSL_fhe_approximation_table[index].id = index + 1;
    return index + 1;
}

DSL_FHE_CKKS_VALUE_STATE_ID
DSL_FHE_Plan_Add_CKKS_Value_State
        (const DSL_FHE_CKKS_VALUE_STATE_RECORD *record)
{
    if (record == NULL || !DSL_FHE_Plan_CKKS_State_Valid(*record))
        return DSL_FHE_CKKS_VALUE_STATE_INVALID_ID;
    UINT32 expected_version = 1;
    for (UINT32 i = 0; i < DSL_fhe_ckks_state_table.Size(); ++i) {
        if (DSL_fhe_ckks_state_table[i].value_id == record->value_id) {
            if (DSL_fhe_ckks_state_table[i].state_version ==
                record->state_version)
                return DSL_FHE_CKKS_VALUE_STATE_INVALID_ID;
            ++expected_version;
        }
    }
    if (record->state_version != expected_version)
        return DSL_FHE_CKKS_VALUE_STATE_INVALID_ID;
    DSL_FHE_CKKS_VALUE_STATE_RECORD copy = *record;
    UINT32 index = DSL_fhe_ckks_state_table.Insert(copy);
    DSL_fhe_ckks_state_table[index].id = index + 1;
    return index + 1;
}

DSL_FHE_BN_FOLD_PROVENANCE_ID
DSL_FHE_Plan_Add_BN_Fold_Provenance
        (const DSL_FHE_BN_FOLD_PROVENANCE_RECORD *record)
{
    if (record == NULL || !DSL_FHE_Plan_BN_Fold_Valid(*record))
        return DSL_FHE_BN_FOLD_PROVENANCE_INVALID_ID;
    for (UINT32 i = 0; i < DSL_fhe_bn_fold_table.Size(); ++i) {
        const DSL_FHE_BN_FOLD_PROVENANCE_RECORD &prior =
            DSL_fhe_bn_fold_table[i];
        if (prior.conv_node_id == record->conv_node_id &&
            prior.batch_norm_node_id == record->batch_norm_node_id &&
            prior.context_pu_identity_id == record->context_pu_identity_id &&
            prior.context_callsite_id == record->context_callsite_id)
            return DSL_FHE_BN_FOLD_PROVENANCE_INVALID_ID;
    }
    DSL_FHE_BN_FOLD_PROVENANCE_RECORD copy = *record;
    UINT32 index = DSL_fhe_bn_fold_table.Insert(copy);
    DSL_fhe_bn_fold_table[index].id = index + 1;
    return index + 1;
}

DSL_FHE_CONVERSION_DISPOSITION_ID
DSL_FHE_Plan_Add_Conversion_Disposition
        (const DSL_FHE_CONVERSION_DISPOSITION_RECORD *record)
{
    if (record == NULL || record->disposition ==
            DSL_FHE_DISPOSITION_REQUIRE_COMPOSITE_APPROXIMATION ||
        !DSL_FHE_Plan_Disposition_Valid(*record, NULL))
        return DSL_FHE_CONVERSION_DISPOSITION_INVALID_ID;
    for (UINT32 i = 0; i < DSL_fhe_disposition_table.Size(); ++i) {
        if (DSL_fhe_disposition_table[i].source_node_id ==
            record->source_node_id)
            return DSL_FHE_CONVERSION_DISPOSITION_INVALID_ID;
    }
    DSL_FHE_CONVERSION_DISPOSITION_RECORD copy = *record;
    UINT32 index = DSL_fhe_disposition_table.Insert(copy);
    DSL_fhe_disposition_table[index].id = index + 1;
    return index + 1;
}

UINT32 DSL_FHE_Plan_Conversion_Disposition_Count (void)
{ return DSL_fhe_disposition_table.Size(); }
UINT32 DSL_FHE_Plan_Approximation_Contract_Count (void)
{ return DSL_fhe_approximation_table.Size(); }
UINT32 DSL_FHE_Plan_CKKS_Value_State_Count (void)
{ return DSL_fhe_ckks_state_table.Size(); }
UINT32 DSL_FHE_Plan_BN_Fold_Provenance_Count (void)
{ return DSL_fhe_bn_fold_table.Size(); }

BOOL DSL_FHE_Plan_Get_Conversion_Disposition
        (DSL_FHE_CONVERSION_DISPOSITION_ID id,
         DSL_FHE_CONVERSION_DISPOSITION_RECORD *record)
{ return DSL_FHE_Plan_Table_Get(DSL_fhe_disposition_table, id, record); }
BOOL DSL_FHE_Plan_Get_Approximation_Contract
        (DSL_FHE_APPROXIMATION_CONTRACT_ID id,
         DSL_FHE_APPROXIMATION_CONTRACT_RECORD *record)
{ return DSL_FHE_Plan_Table_Get(DSL_fhe_approximation_table, id, record); }
BOOL DSL_FHE_Plan_Get_CKKS_Value_State
        (DSL_FHE_CKKS_VALUE_STATE_ID id,
         DSL_FHE_CKKS_VALUE_STATE_RECORD *record)
{ return DSL_FHE_Plan_Table_Get(DSL_fhe_ckks_state_table, id, record); }
BOOL DSL_FHE_Plan_Get_BN_Fold_Provenance
        (DSL_FHE_BN_FOLD_PROVENANCE_ID id,
         DSL_FHE_BN_FOLD_PROVENANCE_RECORD *record)
{ return DSL_FHE_Plan_Table_Get(DSL_fhe_bn_fold_table, id, record); }

BOOL
DSL_FHE_Plan_Find_Conversion_Disposition
        (DSL_IR_NODE_ID source_node_id,
         DSL_FHE_CONVERSION_DISPOSITION_RECORD *record)
{
    for (UINT32 i = 0; i < DSL_fhe_disposition_table.Size(); ++i) {
        if (DSL_fhe_disposition_table[i].source_node_id == source_node_id)
            return DSL_FHE_Plan_Table_Get
                       (DSL_fhe_disposition_table, i + 1, record);
    }
    return FALSE;
}

BOOL
DSL_FHE_Plan_Find_CKKS_Value_State
        (DSL_IR_VALUE_ID value_id, UINT32 state_version,
         DSL_FHE_CKKS_VALUE_STATE_RECORD *record)
{
    for (UINT32 i = 0; i < DSL_fhe_ckks_state_table.Size(); ++i) {
        if (DSL_fhe_ckks_state_table[i].value_id == value_id &&
            DSL_fhe_ckks_state_table[i].state_version == state_version)
            return DSL_FHE_Plan_Table_Get
                       (DSL_fhe_ckks_state_table, i + 1, record);
    }
    return FALSE;
}

BOOL
DSL_FHE_Plan_Find_Latest_CKKS_Value_State
        (DSL_IR_VALUE_ID value_id, DSL_FHE_CKKS_VALUE_STATE_RECORD *record)
{
    for (UINT32 i = DSL_fhe_ckks_state_table.Size(); i != 0; --i) {
        if (DSL_fhe_ckks_state_table[i - 1].value_id == value_id)
            return DSL_FHE_Plan_Table_Get
                       (DSL_fhe_ckks_state_table, i, record);
    }
    return FALSE;
}

BOOL
DSL_FHE_Plan_Find_BN_Fold_Provenance
        (DSL_IR_NODE_ID conv_node_id,
         DSL_PU_SOURCE_IDENTITY_ID context_pu_identity_id,
         DSL_CALLSITE_METADATA_ID context_callsite_id,
         DSL_FHE_BN_FOLD_PROVENANCE_RECORD *record)
{
    for (UINT32 i = 0; i < DSL_fhe_bn_fold_table.Size(); ++i) {
        const DSL_FHE_BN_FOLD_PROVENANCE_RECORD &fold =
            DSL_fhe_bn_fold_table[i];
        if (fold.conv_node_id == conv_node_id &&
            fold.context_pu_identity_id == context_pu_identity_id &&
            fold.context_callsite_id == context_callsite_id)
            return DSL_FHE_Plan_Table_Get
                       (DSL_fhe_bn_fold_table, i + 1, record);
    }
    return FALSE;
}

static BOOL
DSL_FHE_Approx_Profile_Report
        (FILE *diagnostic, const char *message, UINT32 id)
{
    if (diagnostic != NULL)
        fprintf(diagnostic, "FHE approximation profile image error: "
                "%s id=%u\n", message, id);
    return FALSE;
}

static BOOL
DSL_FHE_Approx_Profile_SHA256_Valid (STR_IDX id)
{
    if (!DSL_FHE_Plan_String_Id_Valid(id, TRUE))
        return FALSE;
    const char *value = Index_To_Str(id);
    if (strlen(value) != 64)
        return FALSE;
    for (UINT32 i = 0; i < 64; ++i) {
        if (!((value[i] >= '0' && value[i] <= '9') ||
              (value[i] >= 'a' && value[i] <= 'f')))
            return FALSE;
    }
    return TRUE;
}

static BOOL
DSL_FHE_Composite_Profile_Basic_Valid
        (const DSL_FHE_COMPOSITE_PROFILE_RECORD &record,
         UINT32 stage_limit)
{
    DSL_FHE_COMPILATION_CONFIG_RECORD config;
    return DSL_FHE_Get_Compilation_Config(record.config_id, &config) &&
           DSL_FHE_Plan_String_Id_Valid(record.profile_name, TRUE) &&
           record.profile_version != 0 &&
           record.reconstruction ==
               DSL_FHE_RECONSTRUCTION_RELU_FROM_NORMALIZED_SIGN &&
           record.total_multiplicative_depth != 0 &&
           record.normalization_policy ==
               DSL_FHE_NORMALIZATION_POSITIVE_CONTEXT_BOUND &&
           record.pre_refresh_policy >= DSL_FHE_PRE_REFRESH_INHERIT &&
           record.pre_refresh_policy <=
               DSL_FHE_PRE_REFRESH_PROVEN_EXISTING &&
           DSL_FHE_Plan_String_Id_Valid(record.source_revision, TRUE) &&
           DSL_FHE_Approx_Profile_SHA256_Valid(record.manifest_sha256) &&
           record.stage_count != 0 &&
           DSL_FHE_Plan_Range_Valid
               (record.first_stage_id, record.stage_count, stage_limit) &&
           record.flags == 0 && record.reserved0 == 0 &&
           record.reserved1 == 0 && record.reserved2 == 0 &&
           record.reserved3 == 0;
}

static BOOL
DSL_FHE_Approx_Stage_Basic_Valid
        (const DSL_FHE_APPROX_STAGE_RECORD &record, UINT32 profile_limit)
{
    DSL_TENSOR_TCON_RECORD coefficients;
    if (record.profile_id == 0 || record.profile_id > profile_limit ||
        record.approximation_family < DSL_FHE_APPROXIMATION_MINIMAX ||
        record.approximation_family > DSL_FHE_APPROXIMATION_TAYLOR ||
        record.basis < DSL_FHE_APPROX_BASIS_CHEBYSHEV ||
        record.basis > DSL_FHE_APPROX_BASIS_MONOMIAL ||
        record.degree == 0 ||
        record.evaluation_scheme < DSL_FHE_APPROX_EVAL_CLENSHAW ||
        record.evaluation_scheme >
            DSL_FHE_APPROX_EVAL_ADDITION_CHAIN ||
        record.required_input_value_class <
            DSL_FHE_VALUE_CLASS_CIPHERTEXT ||
        record.required_input_value_class > DSL_FHE_VALUE_CLASS_CLEAR ||
        record.input_scale_policy <
            DSL_FHE_APPROX_INPUT_SCALE_ANY_COMPATIBLE ||
        record.input_scale_policy >
            DSL_FHE_APPROX_INPUT_SCALE_PROFILE_NORMALIZED ||
        record.input_level_policy < DSL_FHE_APPROX_LEVEL_ANY_SUFFICIENT ||
        record.input_level_policy > DSL_FHE_APPROX_LEVEL_EXACT ||
        record.level_consumption < 0 ||
        record.output_scale_policy <
            DSL_FHE_APPROX_OUTPUT_SCALE_PRESERVE_INPUT ||
        record.output_scale_policy >
            DSL_FHE_APPROX_OUTPUT_SCALE_DEFAULT_RESCALE ||
        record.output_component_policy <
            DSL_FHE_APPROX_COMPONENT_PRESERVE ||
        record.output_component_policy > DSL_FHE_APPROX_COMPONENT_MAY_GROW ||
        record.minimum_precision_bits <= 0 ||
        !DSL_Tensor_TCON_Get(record.coefficient_tensor_tcon, &coefficients) ||
        TY_tensor_rank(coefficients.descriptor_ty) != 1 ||
        (coefficients.element_mtype != MTYPE_F4 &&
         coefficients.element_mtype != MTYPE_F8) ||
        coefficients.element_count != (UINT64)record.degree + 1 ||
        !DSL_FHE_Approx_Profile_SHA256_Valid(record.coefficient_sha256) ||
        record.flags != 0 || record.reserved != 0)
        return FALSE;
    if (record.input_level_policy == DSL_FHE_APPROX_LEVEL_ANY_SUFFICIENT)
        return record.required_input_level == -1;
    return record.required_input_level >= 0;
}

static BOOL
DSL_FHE_Approx_Profile_Value_Is_Live_Relu
        (DSL_IR_VALUE_ID value_id, ST_IDX owner_pu_st)
{
    DSL_IR_VALUE_RECORD value;
    DSL_OPERATOR dsl_operator;
    if (!DSL_IR_Image_Get_Value(value_id, &value) ||
        (value.flags & DSL_IR_VALUE_FLAG_REDIRECTED) != 0 ||
        value.producer_node_id == DSL_IR_NODE_INVALID_ID ||
        !DSL_FHE_Plan_Value_Belongs_To_PU(value, owner_pu_st) ||
        !DSL_FHE_Plan_Node_Operator
            (value.producer_node_id, &dsl_operator, NULL))
        return FALSE;
    return dsl_operator == OPR_DSLRELU;
}

static BOOL
DSL_FHE_Context_Range_Basic_Valid
        (const DSL_FHE_CONTEXT_RANGE_RECORD &record, UINT32 profile_limit)
{
    DSL_PU_SOURCE_IDENTITY_RECORD identity;
    DSL_CALLSITE_METADATA_RECORD callsite;
    double bound;
    double observed_min;
    double observed_max;
    if (record.profile_id == 0 || record.profile_id > profile_limit ||
        !DSL_FHE_Approx_Profile_Value_Is_Live_Relu
            (record.source_relu_value_id, record.owner_pu_st) ||
        !DSL_Call_Image_Get_PU_Identity
            (record.context_pu_identity_id, &identity) ||
        !DSL_FHE_Plan_Scalar_TCON_Value
            (record.positive_bound_tcon, &bound) || bound <= 0.0 ||
        !DSL_FHE_Plan_Scalar_TCON_Value
            (record.observed_min_tcon, &observed_min) ||
        !DSL_FHE_Plan_Scalar_TCON_Value
            (record.observed_max_tcon, &observed_max) ||
        observed_min > observed_max || observed_min < -bound ||
        observed_max > bound ||
        record.out_of_range_policy != DSL_FHE_CONTEXT_RANGE_REJECT ||
        !DSL_FHE_Plan_String_Id_Valid(record.provenance, TRUE) ||
        record.flags != 0 || record.reserved0 != 0 ||
        record.reserved1 != 0 || record.reserved2 != 0)
        return FALSE;

    if (record.context_callsite_id == DSL_CALLSITE_METADATA_INVALID_ID)
        return identity.owner_pu_st == record.owner_pu_st;
    return DSL_Call_Image_Get_Callsite(record.context_callsite_id, &callsite) &&
           callsite.callee_pu_st == record.owner_pu_st &&
           callsite.owner_pu_st == identity.owner_pu_st;
}

static UINT32
DSL_FHE_Approx_Profile_View_Profile_Count
        (const DSL_FHE_APPROX_PROFILE_IMAGE_VIEW *view)
{
    return view == NULL ? DSL_fhe_composite_profile_table.Size() :
                          view->header->profile_count;
}

static UINT32
DSL_FHE_Approx_Profile_View_Stage_Count
        (const DSL_FHE_APPROX_PROFILE_IMAGE_VIEW *view)
{
    return view == NULL ? DSL_fhe_approx_stage_table.Size() :
                          view->header->stage_count;
}

static UINT32
DSL_FHE_Approx_Profile_View_Association_Count
        (const DSL_FHE_APPROX_PROFILE_IMAGE_VIEW *view)
{
    return view == NULL ? DSL_fhe_approx_association_table.Size() :
                          view->header->association_count;
}

static UINT32
DSL_FHE_Approx_Profile_View_Context_Count
        (const DSL_FHE_APPROX_PROFILE_IMAGE_VIEW *view)
{
    return view == NULL ? DSL_fhe_context_range_table.Size() :
                          view->header->context_range_count;
}

static const DSL_FHE_COMPOSITE_PROFILE_RECORD &
DSL_FHE_Approx_Profile_View_Profile
        (const DSL_FHE_APPROX_PROFILE_IMAGE_VIEW *view, UINT32 ordinal)
{
    return view == NULL ? DSL_fhe_composite_profile_table[ordinal] :
                          view->profiles[ordinal];
}

static const DSL_FHE_APPROX_STAGE_RECORD &
DSL_FHE_Approx_Profile_View_Stage
        (const DSL_FHE_APPROX_PROFILE_IMAGE_VIEW *view, UINT32 ordinal)
{
    return view == NULL ? DSL_fhe_approx_stage_table[ordinal] :
                          view->stages[ordinal];
}

static const DSL_FHE_APPROX_ASSOCIATION_RECORD &
DSL_FHE_Approx_Profile_View_Association
        (const DSL_FHE_APPROX_PROFILE_IMAGE_VIEW *view, UINT32 ordinal)
{
    return view == NULL ? DSL_fhe_approx_association_table[ordinal] :
                          view->associations[ordinal];
}

static const DSL_FHE_CONTEXT_RANGE_RECORD &
DSL_FHE_Approx_Profile_View_Context
        (const DSL_FHE_APPROX_PROFILE_IMAGE_VIEW *view, UINT32 ordinal)
{
    return view == NULL ? DSL_fhe_context_range_table[ordinal] :
                          view->context_ranges[ordinal];
}

static BOOL
DSL_FHE_Approx_Profile_View_Validate
        (const DSL_FHE_APPROX_PROFILE_IMAGE_VIEW *view, FILE *diagnostic)
{
    DSL_FHE_APPROX_PROFILE_IMAGE_HEADER header;
    if (view == NULL)
        DSL_FHE_Approx_Profile_Image_Get_Header(&header);
    else
        header = *view->header;
    const UINT32 capabilities = DSL_FHE_APPROX_PROFILE_CAP_PROFILE |
                                DSL_FHE_APPROX_PROFILE_CAP_STAGE |
                                DSL_FHE_APPROX_PROFILE_CAP_ASSOCIATION |
                                DSL_FHE_APPROX_PROFILE_CAP_CONTEXT_RANGE;
    if (header.magic != DSL_FHE_APPROX_PROFILE_IMAGE_MAGIC ||
        header.version != DSL_FHE_APPROX_PROFILE_IMAGE_VERSION ||
        header.header_size != DSL_FHE_APPROX_PROFILE_IMAGE_HEADER_SIZE ||
        header.record_kind_count != 4 ||
        header.capabilities != capabilities || header.flags != 0 ||
        header.reserved0 != 0 || header.reserved1 != 0 ||
        header.reserved2 != 0 || header.reserved3 != 0 ||
        header.reserved4 != 0 || header.reserved5 != 0)
        return DSL_FHE_Approx_Profile_Report
                   (diagnostic, "invalid header", 0);

    for (UINT32 i = 0;
         i < DSL_FHE_Approx_Profile_View_Profile_Count(view); ++i) {
        const DSL_FHE_COMPOSITE_PROFILE_RECORD &profile =
            DSL_FHE_Approx_Profile_View_Profile(view, i);
        UINT64 total_level_consumption = 0;
        if (profile.id != i + 1 ||
            !DSL_FHE_Composite_Profile_Basic_Valid
                (profile, DSL_FHE_Approx_Profile_View_Stage_Count(view)))
            return DSL_FHE_Approx_Profile_Report
                       (diagnostic, "invalid composite profile", i + 1);
        for (UINT32 j = 0; j < i; ++j) {
            const DSL_FHE_COMPOSITE_PROFILE_RECORD &prior =
                DSL_FHE_Approx_Profile_View_Profile(view, j);
            if (prior.config_id == profile.config_id &&
                prior.profile_name == profile.profile_name &&
                prior.profile_version == profile.profile_version)
                return DSL_FHE_Approx_Profile_Report
                           (diagnostic, "duplicate composite profile", i + 1);
        }
        for (UINT32 j = 0; j < profile.stage_count; ++j) {
            const DSL_FHE_APPROX_STAGE_RECORD &stage =
                DSL_FHE_Approx_Profile_View_Stage
                    (view, profile.first_stage_id - 1 + j);
            if (stage.profile_id != profile.id ||
                stage.stage_ordinal != j)
                return DSL_FHE_Approx_Profile_Report
                           (diagnostic, "noncontiguous profile stages", i + 1);
            total_level_consumption += (UINT32)stage.level_consumption;
        }
        if (total_level_consumption !=
                profile.total_multiplicative_depth)
            return DSL_FHE_Approx_Profile_Report
                       (diagnostic, "profile depth does not match stages",
                        i + 1);
    }
    for (UINT32 i = 0;
         i < DSL_FHE_Approx_Profile_View_Stage_Count(view); ++i) {
        const DSL_FHE_APPROX_STAGE_RECORD &stage =
            DSL_FHE_Approx_Profile_View_Stage(view, i);
        if (stage.id != i + 1 ||
            !DSL_FHE_Approx_Stage_Basic_Valid
                (stage, DSL_FHE_Approx_Profile_View_Profile_Count(view)))
            return DSL_FHE_Approx_Profile_Report
                       (diagnostic, "invalid approximation stage", i + 1);
        UINT32 owners = 0;
        for (UINT32 j = 0;
             j < DSL_FHE_Approx_Profile_View_Profile_Count(view); ++j) {
            const DSL_FHE_COMPOSITE_PROFILE_RECORD &profile =
                DSL_FHE_Approx_Profile_View_Profile(view, j);
            if (stage.id >= profile.first_stage_id &&
                stage.id < profile.first_stage_id + profile.stage_count)
                ++owners;
        }
        if (owners != 1)
            return DSL_FHE_Approx_Profile_Report
                       (diagnostic, "orphaned approximation stage", i + 1);
    }
    for (UINT32 i = 0;
         i < DSL_FHE_Approx_Profile_View_Association_Count(view); ++i) {
        const DSL_FHE_APPROX_ASSOCIATION_RECORD &association =
            DSL_FHE_Approx_Profile_View_Association(view, i);
        DSL_FHE_CONVERSION_DISPOSITION_RECORD disposition;
        DSL_FHE_CKKS_VALUE_STATE_RECORD state;
        DSL_FHE_ENCRYPTION_DESCRIPTOR_RECORD encryption;
        if (association.id != i + 1 || association.profile_id == 0 ||
            association.profile_id >
                DSL_FHE_Approx_Profile_View_Profile_Count(view))
            return DSL_FHE_Approx_Profile_Report
                       (diagnostic, "invalid profile association", i + 1);
        const DSL_FHE_COMPOSITE_PROFILE_RECORD &profile =
            DSL_FHE_Approx_Profile_View_Profile
                (view, association.profile_id - 1);
        if (!DSL_FHE_Plan_Get_Conversion_Disposition
                (association.disposition_id, &disposition) ||
            disposition.disposition !=
                DSL_FHE_DISPOSITION_REQUIRE_COMPOSITE_APPROXIMATION ||
            disposition.approximation_contract_id != association.profile_id ||
            disposition.result_value_id != association.source_relu_value_id ||
            disposition.owner_pu_st != association.owner_pu_st ||
            !DSL_FHE_Plan_Get_CKKS_Value_State
                (disposition.result_ckks_value_state_id, &state) ||
            !DSL_FHE_Get_Encryption_Descriptor
                (state.encryption_descriptor_id, &encryption) ||
            profile.config_id != encryption.config_id ||
            !DSL_FHE_Approx_Profile_Value_Is_Live_Relu
                (association.source_relu_value_id, association.owner_pu_st) ||
            association.flags != 0 || association.reserved0 != 0 ||
            association.reserved1 != 0)
            return DSL_FHE_Approx_Profile_Report
                       (diagnostic, "invalid profile association", i + 1);
        for (UINT32 j = 0; j < i; ++j) {
            const DSL_FHE_APPROX_ASSOCIATION_RECORD &prior =
                DSL_FHE_Approx_Profile_View_Association(view, j);
            if (prior.disposition_id == association.disposition_id ||
                prior.source_relu_value_id ==
                    association.source_relu_value_id)
                return DSL_FHE_Approx_Profile_Report
                           (diagnostic, "duplicate profile association", i + 1);
        }
    }
    for (UINT32 i = 0;
         i < DSL_FHE_Approx_Profile_View_Context_Count(view); ++i) {
        const DSL_FHE_CONTEXT_RANGE_RECORD &context =
            DSL_FHE_Approx_Profile_View_Context(view, i);
        if (context.id != i + 1 ||
            !DSL_FHE_Context_Range_Basic_Valid
                (context, DSL_FHE_Approx_Profile_View_Profile_Count(view)))
            return DSL_FHE_Approx_Profile_Report
                       (diagnostic, "invalid context range", i + 1);
        UINT32 associations = 0;
        for (UINT32 j = 0;
             j < DSL_FHE_Approx_Profile_View_Association_Count(view); ++j) {
            const DSL_FHE_APPROX_ASSOCIATION_RECORD &association =
                DSL_FHE_Approx_Profile_View_Association(view, j);
            if (association.profile_id == context.profile_id &&
                association.source_relu_value_id ==
                    context.source_relu_value_id)
                ++associations;
        }
        if (associations != 1)
            return DSL_FHE_Approx_Profile_Report
                       (diagnostic, "unassociated context range", i + 1);
        for (UINT32 j = 0; j < i; ++j) {
            const DSL_FHE_CONTEXT_RANGE_RECORD &prior =
                DSL_FHE_Approx_Profile_View_Context(view, j);
            if (prior.profile_id == context.profile_id &&
                prior.source_relu_value_id == context.source_relu_value_id &&
                prior.context_pu_identity_id ==
                    context.context_pu_identity_id &&
                prior.context_callsite_id == context.context_callsite_id)
                return DSL_FHE_Approx_Profile_Report
                           (diagnostic, "duplicate context range", i + 1);
        }
    }
    for (UINT32 i = 0; i < DSL_fhe_disposition_table.Size(); ++i) {
        const DSL_FHE_CONVERSION_DISPOSITION_RECORD &disposition =
            DSL_fhe_disposition_table[i];
        if (disposition.disposition !=
            DSL_FHE_DISPOSITION_REQUIRE_COMPOSITE_APPROXIMATION)
            continue;
        UINT32 associations = 0;
        UINT32 contexts = 0;
        for (UINT32 j = 0;
             j < DSL_FHE_Approx_Profile_View_Association_Count(view); ++j) {
            const DSL_FHE_APPROX_ASSOCIATION_RECORD &association =
                DSL_FHE_Approx_Profile_View_Association(view, j);
            if (association.disposition_id == disposition.id)
                ++associations;
        }
        for (UINT32 j = 0;
             j < DSL_FHE_Approx_Profile_View_Context_Count(view); ++j) {
            const DSL_FHE_CONTEXT_RANGE_RECORD &context =
                DSL_FHE_Approx_Profile_View_Context(view, j);
            if (context.profile_id ==
                    disposition.approximation_contract_id &&
                context.source_relu_value_id == disposition.result_value_id)
                ++contexts;
        }
        if (associations != 1 || contexts == 0)
            return DSL_FHE_Approx_Profile_Report
                       (diagnostic, "incomplete composite disposition",
                        disposition.id);
    }
    return TRUE;
}

static BOOL
DSL_FHE_Approx_Profile_Cross_Validate (FILE *diagnostic)
{
    return DSL_FHE_Approx_Profile_View_Validate(NULL, diagnostic);
}

void
DSL_FHE_Approx_Profile_Image_Reset (void)
{
    DSL_fhe_composite_profile_table.Delete_down_to(0);
    DSL_fhe_approx_stage_table.Delete_down_to(0);
    DSL_fhe_approx_association_table.Delete_down_to(0);
    DSL_fhe_context_range_table.Delete_down_to(0);
}

void
DSL_FHE_Approx_Profile_Image_Get_Header
        (DSL_FHE_APPROX_PROFILE_IMAGE_HEADER *header)
{
    if (header == NULL)
        return;
    memset(header, 0, sizeof(*header));
    header->magic = DSL_FHE_APPROX_PROFILE_IMAGE_MAGIC;
    header->version = DSL_FHE_APPROX_PROFILE_IMAGE_VERSION;
    header->header_size = DSL_FHE_APPROX_PROFILE_IMAGE_HEADER_SIZE;
    header->record_kind_count = 4;
    header->capabilities = DSL_FHE_APPROX_PROFILE_CAP_PROFILE |
                           DSL_FHE_APPROX_PROFILE_CAP_STAGE |
                           DSL_FHE_APPROX_PROFILE_CAP_ASSOCIATION |
                           DSL_FHE_APPROX_PROFILE_CAP_CONTEXT_RANGE;
    header->profile_count = DSL_fhe_composite_profile_table.Size();
    header->stage_count = DSL_fhe_approx_stage_table.Size();
    header->association_count = DSL_fhe_approx_association_table.Size();
    header->context_range_count = DSL_fhe_context_range_table.Size();
}

BOOL
DSL_FHE_Approx_Profile_Image_Has_Records (void)
{
    return DSL_fhe_composite_profile_table.Size() != 0 ||
           DSL_fhe_approx_stage_table.Size() != 0 ||
           DSL_fhe_approx_association_table.Size() != 0 ||
           DSL_fhe_context_range_table.Size() != 0;
}

BOOL
DSL_FHE_Approx_Profile_Image_Validate (FILE *diagnostic)
{
    return DSL_FHE_Approx_Profile_Cross_Validate(diagnostic);
}

BOOL
DSL_FHE_Approx_Profile_Image_Load_Mapped
        (const void *section_base, UINT64 section_size, FILE *diagnostic)
{
    if (section_base == NULL ||
        section_size < DSL_FHE_APPROX_PROFILE_IMAGE_HEADER_SIZE)
        return DSL_FHE_Approx_Profile_Report
                   (diagnostic, "section is truncated", 0);
    const char *cursor = (const char *)section_base;
    const DSL_FHE_APPROX_PROFILE_IMAGE_HEADER *header =
        (const DSL_FHE_APPROX_PROFILE_IMAGE_HEADER *)cursor;
    UINT64 expected_size = DSL_FHE_APPROX_PROFILE_IMAGE_HEADER_SIZE;
    if (!DSL_FHE_Plan_Add_Section_Size
             (&expected_size, header->profile_count,
              DSL_FHE_COMPOSITE_PROFILE_RECORD_SIZE) ||
        !DSL_FHE_Plan_Add_Section_Size
             (&expected_size, header->stage_count,
              DSL_FHE_APPROX_STAGE_RECORD_SIZE) ||
        !DSL_FHE_Plan_Add_Section_Size
             (&expected_size, header->association_count,
              DSL_FHE_APPROX_ASSOCIATION_RECORD_SIZE) ||
        !DSL_FHE_Plan_Add_Section_Size
             (&expected_size, header->context_range_count,
              DSL_FHE_CONTEXT_RANGE_RECORD_SIZE) ||
        expected_size != section_size)
        return DSL_FHE_Approx_Profile_Report
                   (diagnostic, "section size mismatch", 0);

    DSL_FHE_APPROX_PROFILE_IMAGE_VIEW view;
    view.header = header;
    cursor += DSL_FHE_APPROX_PROFILE_IMAGE_HEADER_SIZE;
    view.profiles = (const DSL_FHE_COMPOSITE_PROFILE_RECORD *)cursor;
    cursor += (UINT64)header->profile_count *
              DSL_FHE_COMPOSITE_PROFILE_RECORD_SIZE;
    view.stages = (const DSL_FHE_APPROX_STAGE_RECORD *)cursor;
    cursor += (UINT64)header->stage_count *
              DSL_FHE_APPROX_STAGE_RECORD_SIZE;
    view.associations = (const DSL_FHE_APPROX_ASSOCIATION_RECORD *)cursor;
    cursor += (UINT64)header->association_count *
              DSL_FHE_APPROX_ASSOCIATION_RECORD_SIZE;
    view.context_ranges = (const DSL_FHE_CONTEXT_RANGE_RECORD *)cursor;
    if (!DSL_FHE_Approx_Profile_View_Validate(&view, diagnostic))
        return FALSE;

    DSL_FHE_Approx_Profile_Image_Reset();
    if (header->profile_count != 0)
        DSL_fhe_composite_profile_table.Insert
            (view.profiles, header->profile_count);
    if (header->stage_count != 0)
        DSL_fhe_approx_stage_table.Insert(view.stages, header->stage_count);
    if (header->association_count != 0)
        DSL_fhe_approx_association_table.Insert
            (view.associations, header->association_count);
    if (header->context_range_count != 0)
        DSL_fhe_context_range_table.Insert
            (view.context_ranges, header->context_range_count);
    return DSL_FHE_Approx_Profile_Cross_Validate(diagnostic);
}

void DSL_FHE_Composite_Profile_Record_Init
        (DSL_FHE_COMPOSITE_PROFILE_RECORD *record)
{ DSL_FHE_Plan_Record_Init(record); }
void DSL_FHE_Approx_Stage_Record_Init
        (DSL_FHE_APPROX_STAGE_RECORD *record)
{
    DSL_FHE_Plan_Record_Init(record);
    if (record != NULL)
        record->required_input_level = -1;
}
void DSL_FHE_Approx_Association_Record_Init
        (DSL_FHE_APPROX_ASSOCIATION_RECORD *record)
{ DSL_FHE_Plan_Record_Init(record); }
void DSL_FHE_Context_Range_Record_Init
        (DSL_FHE_CONTEXT_RANGE_RECORD *record)
{ DSL_FHE_Plan_Record_Init(record); }

DSL_FHE_COMPOSITE_PROFILE_ID
DSL_FHE_Approx_Profile_Intern_Complete
        (const DSL_FHE_COMPOSITE_PROFILE_RECORD *profile,
         const DSL_FHE_APPROX_STAGE_RECORD *stages, UINT32 stage_count)
{
    if (profile == NULL || stages == NULL || stage_count == 0 ||
        profile->stage_count != stage_count)
        return DSL_FHE_COMPOSITE_PROFILE_INVALID_ID;
    for (UINT32 i = 0; i < DSL_fhe_composite_profile_table.Size(); ++i) {
        const DSL_FHE_COMPOSITE_PROFILE_RECORD &prior =
            DSL_fhe_composite_profile_table[i];
        if (prior.config_id != profile->config_id ||
            prior.profile_name != profile->profile_name ||
            prior.profile_version != profile->profile_version)
            continue;
        if (prior.stage_count != stage_count)
            return DSL_FHE_COMPOSITE_PROFILE_INVALID_ID;
        DSL_FHE_COMPOSITE_PROFILE_RECORD candidate = *profile;
        candidate.id = prior.id;
        candidate.first_stage_id = prior.first_stage_id;
        if (memcmp(&candidate, &prior, sizeof(candidate)) != 0)
            return DSL_FHE_COMPOSITE_PROFILE_INVALID_ID;
        for (UINT32 j = 0; j < stage_count; ++j) {
            DSL_FHE_APPROX_STAGE_RECORD candidate_stage = stages[j];
            candidate_stage.id = prior.first_stage_id + j;
            candidate_stage.profile_id = prior.id;
            candidate_stage.stage_ordinal = j;
            if (memcmp(&candidate_stage,
                       &DSL_fhe_approx_stage_table
                            [prior.first_stage_id - 1 + j],
                       sizeof(candidate_stage)) != 0)
                return DSL_FHE_COMPOSITE_PROFILE_INVALID_ID;
        }
        return prior.id;
    }

    const UINT32 profile_id = DSL_fhe_composite_profile_table.Size() + 1;
    const UINT32 first_stage_id = DSL_fhe_approx_stage_table.Size() + 1;
    DSL_FHE_COMPOSITE_PROFILE_RECORD profile_copy = *profile;
    profile_copy.id = profile_id;
    profile_copy.first_stage_id = first_stage_id;
    profile_copy.stage_count = stage_count;
    if (!DSL_FHE_Composite_Profile_Basic_Valid
            (profile_copy, first_stage_id + stage_count - 1))
        return DSL_FHE_COMPOSITE_PROFILE_INVALID_ID;
    UINT64 total_level_consumption = 0;
    for (UINT32 i = 0; i < stage_count; ++i) {
        DSL_FHE_APPROX_STAGE_RECORD stage = stages[i];
        stage.id = first_stage_id + i;
        stage.profile_id = profile_id;
        stage.stage_ordinal = i;
        if (!DSL_FHE_Approx_Stage_Basic_Valid(stage, profile_id))
            return DSL_FHE_COMPOSITE_PROFILE_INVALID_ID;
        total_level_consumption += (UINT32)stage.level_consumption;
    }
    if (total_level_consumption !=
            profile_copy.total_multiplicative_depth)
        return DSL_FHE_COMPOSITE_PROFILE_INVALID_ID;
    DSL_fhe_composite_profile_table.Insert(profile_copy);
    for (UINT32 i = 0; i < stage_count; ++i) {
        DSL_FHE_APPROX_STAGE_RECORD stage = stages[i];
        stage.id = first_stage_id + i;
        stage.profile_id = profile_id;
        stage.stage_ordinal = i;
        DSL_fhe_approx_stage_table.Insert(stage);
    }
    return profile_id;
}

DSL_FHE_CONVERSION_DISPOSITION_ID
DSL_FHE_Plan_Add_Composite_Disposition
        (const DSL_FHE_CONVERSION_DISPOSITION_RECORD *disposition,
         DSL_FHE_COMPOSITE_PROFILE_ID profile_id)
{
    if (disposition == NULL || profile_id == 0 ||
        profile_id > DSL_fhe_composite_profile_table.Size() ||
        disposition->disposition !=
            DSL_FHE_DISPOSITION_REQUIRE_COMPOSITE_APPROXIMATION ||
        disposition->approximation_contract_id != profile_id ||
        !DSL_FHE_Plan_Disposition_Valid(*disposition, NULL))
        return DSL_FHE_CONVERSION_DISPOSITION_INVALID_ID;
    for (UINT32 i = 0; i < DSL_fhe_disposition_table.Size(); ++i) {
        if (DSL_fhe_disposition_table[i].source_node_id ==
            disposition->source_node_id)
            return DSL_FHE_CONVERSION_DISPOSITION_INVALID_ID;
    }
    DSL_FHE_CONVERSION_DISPOSITION_RECORD disposition_copy = *disposition;
    disposition_copy.id = DSL_fhe_disposition_table.Size() + 1;
    DSL_FHE_APPROX_ASSOCIATION_RECORD association;
    DSL_FHE_Approx_Association_Record_Init(&association);
    association.id = DSL_fhe_approx_association_table.Size() + 1;
    association.disposition_id = disposition_copy.id;
    association.source_relu_value_id = disposition_copy.result_value_id;
    association.profile_id = profile_id;
    association.owner_pu_st = disposition_copy.owner_pu_st;
    DSL_fhe_disposition_table.Insert(disposition_copy);
    DSL_fhe_approx_association_table.Insert(association);
    return disposition_copy.id;
}

DSL_FHE_CONTEXT_RANGE_ID
DSL_FHE_Approx_Profile_Bind_Context_Range
        (const DSL_FHE_CONTEXT_RANGE_RECORD *record)
{
    if (record == NULL || !DSL_FHE_Context_Range_Basic_Valid
            (*record, DSL_fhe_composite_profile_table.Size()))
        return DSL_FHE_CONTEXT_RANGE_INVALID_ID;
    BOOL associated = FALSE;
    for (UINT32 i = 0; i < DSL_fhe_approx_association_table.Size(); ++i) {
        const DSL_FHE_APPROX_ASSOCIATION_RECORD &association =
            DSL_fhe_approx_association_table[i];
        if (association.profile_id == record->profile_id &&
            association.source_relu_value_id ==
                record->source_relu_value_id)
            associated = TRUE;
    }
    if (!associated)
        return DSL_FHE_CONTEXT_RANGE_INVALID_ID;
    for (UINT32 i = 0; i < DSL_fhe_context_range_table.Size(); ++i) {
        const DSL_FHE_CONTEXT_RANGE_RECORD &prior =
            DSL_fhe_context_range_table[i];
        if (prior.profile_id == record->profile_id &&
            prior.source_relu_value_id == record->source_relu_value_id &&
            prior.context_pu_identity_id == record->context_pu_identity_id &&
            prior.context_callsite_id == record->context_callsite_id)
            return DSL_FHE_CONTEXT_RANGE_INVALID_ID;
    }
    DSL_FHE_CONTEXT_RANGE_RECORD copy = *record;
    UINT32 index = DSL_fhe_context_range_table.Insert(copy);
    DSL_fhe_context_range_table[index].id = index + 1;
    return index + 1;
}

UINT32 DSL_FHE_Approx_Profile_Count (void)
{ return DSL_fhe_composite_profile_table.Size(); }
UINT32 DSL_FHE_Approx_Stage_Count (void)
{ return DSL_fhe_approx_stage_table.Size(); }
UINT32 DSL_FHE_Approx_Association_Count (void)
{ return DSL_fhe_approx_association_table.Size(); }
UINT32 DSL_FHE_Context_Range_Count (void)
{ return DSL_fhe_context_range_table.Size(); }

BOOL DSL_FHE_Approx_Profile_Get
        (DSL_FHE_COMPOSITE_PROFILE_ID id,
         DSL_FHE_COMPOSITE_PROFILE_RECORD *record)
{ return DSL_FHE_Plan_Table_Get
             (DSL_fhe_composite_profile_table, id, record); }
BOOL DSL_FHE_Approx_Stage_Get
        (DSL_FHE_APPROX_STAGE_ID id, DSL_FHE_APPROX_STAGE_RECORD *record)
{ return DSL_FHE_Plan_Table_Get(DSL_fhe_approx_stage_table, id, record); }
BOOL DSL_FHE_Approx_Association_Get
        (DSL_FHE_APPROX_ASSOCIATION_ID id,
         DSL_FHE_APPROX_ASSOCIATION_RECORD *record)
{ return DSL_FHE_Plan_Table_Get
             (DSL_fhe_approx_association_table, id, record); }
BOOL DSL_FHE_Context_Range_Get
        (DSL_FHE_CONTEXT_RANGE_ID id, DSL_FHE_CONTEXT_RANGE_RECORD *record)
{ return DSL_FHE_Plan_Table_Get(DSL_fhe_context_range_table, id, record); }

BOOL
DSL_FHE_Approx_Profile_Find
        (DSL_FHE_CONFIG_ID config_id, const char *profile_name,
         UINT32 profile_version,
         DSL_FHE_COMPOSITE_PROFILE_RECORD *record)
{
    if (profile_name == NULL)
        return FALSE;
    for (UINT32 i = 0; i < DSL_fhe_composite_profile_table.Size(); ++i) {
        const DSL_FHE_COMPOSITE_PROFILE_RECORD &profile =
            DSL_fhe_composite_profile_table[i];
        if (profile.config_id == config_id &&
            profile.profile_version == profile_version &&
            strcmp(Index_To_Str(profile.profile_name), profile_name) == 0)
            return DSL_FHE_Approx_Profile_Get(i + 1, record);
    }
    return FALSE;
}

BOOL
DSL_FHE_Approx_Stage_Find
        (DSL_FHE_COMPOSITE_PROFILE_ID profile_id, UINT32 stage_ordinal,
         DSL_FHE_APPROX_STAGE_RECORD *record)
{
    for (UINT32 i = 0; i < DSL_fhe_approx_stage_table.Size(); ++i) {
        const DSL_FHE_APPROX_STAGE_RECORD &stage =
            DSL_fhe_approx_stage_table[i];
        if (stage.profile_id == profile_id &&
            stage.stage_ordinal == stage_ordinal)
            return DSL_FHE_Approx_Stage_Get(i + 1, record);
    }
    return FALSE;
}

BOOL
DSL_FHE_Approx_Association_Find
        (DSL_FHE_CONVERSION_DISPOSITION_ID disposition_id,
         DSL_FHE_APPROX_ASSOCIATION_RECORD *record)
{
    for (UINT32 i = 0; i < DSL_fhe_approx_association_table.Size(); ++i) {
        if (DSL_fhe_approx_association_table[i].disposition_id ==
            disposition_id)
            return DSL_FHE_Approx_Association_Get(i + 1, record);
    }
    return FALSE;
}

BOOL
DSL_FHE_Context_Range_Find
        (DSL_FHE_COMPOSITE_PROFILE_ID profile_id,
         DSL_IR_VALUE_ID source_relu_value_id,
         DSL_PU_SOURCE_IDENTITY_ID context_pu_identity_id,
         DSL_CALLSITE_METADATA_ID context_callsite_id,
         DSL_FHE_CONTEXT_RANGE_RECORD *record)
{
    for (UINT32 i = 0; i < DSL_fhe_context_range_table.Size(); ++i) {
        const DSL_FHE_CONTEXT_RANGE_RECORD &context =
            DSL_fhe_context_range_table[i];
        if (context.profile_id == profile_id &&
            context.source_relu_value_id == source_relu_value_id &&
            context.context_pu_identity_id == context_pu_identity_id &&
            context.context_callsite_id == context_callsite_id)
            return DSL_FHE_Context_Range_Get(i + 1, record);
    }
    return FALSE;
}
