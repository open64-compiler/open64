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

static DSL_FHE_DISPOSITION_TABLE DSL_fhe_disposition_table;
static DSL_FHE_APPROXIMATION_TABLE DSL_fhe_approximation_table;
static DSL_FHE_CKKS_STATE_TABLE DSL_fhe_ckks_state_table;
static DSL_FHE_BN_FOLD_TABLE DSL_fhe_bn_fold_table;

typedef struct {
    const DSL_FHE_PLAN_IMAGE_HEADER *header;
    const DSL_FHE_CONVERSION_DISPOSITION_RECORD *dispositions;
    const DSL_FHE_APPROXIMATION_CONTRACT_RECORD *approximations;
    const DSL_FHE_CKKS_VALUE_STATE_RECORD *ckks_states;
    const DSL_FHE_BN_FOLD_PROVENANCE_RECORD *bn_folds;
} DSL_FHE_PLAN_IMAGE_VIEW;

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
        record.disposition > DSL_FHE_DISPOSITION_REQUIRE_APPROXIMATION ||
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
        const char *target_domain;
        const char *target_name;
        UINT16 target_version;
        const char *diagnostic_prefix;
    } DSL_FHE_WRAPPER_SEED;
    static const DSL_FHE_WRAPPER_SEED seeds[] = {
        { DSL_FHE_WRAPPER_CNN_CONV2D, "cnn", "cnn.conv2d", 2,
          "CFHECNN_CONV2D" },
        { DSL_FHE_WRAPPER_CNN_RESIDUAL_ADD, "common",
          "common.residual_add", 2, "CFHECNN_RESIDUAL_ADD" },
        { DSL_FHE_WRAPPER_CNN_GLOBAL_AVG_POOL2D, "cnn",
          "cnn.global_avg_pool2d", 2, "CFHECNN_GLOBAL_AVG_POOL2D" },
        { DSL_FHE_WRAPPER_CNN_LINEAR, "common", "common.linear", 3,
          "CFHECNN_LINEAR" }
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
                                    (fhe_cnn_id, seeds[i].wrapper_name, 1,
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
    return DSL_FHE_Plan_View_Validate(NULL, diagnostic);
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
    if (record == NULL || !DSL_FHE_Plan_Disposition_Valid(*record, NULL))
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
