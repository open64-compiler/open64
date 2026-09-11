/*
 * Copyright (C) 2026 Open64 Project
 */

#include "dsl_fhe_plan.h"
#include "dsl_opcode.h"
#include "strtab.h"
#include "symtab.h"

static const char *
DSL_FHE_Plan_Name (UINT32 value, const char *const *names, UINT32 count)
{
    return value < count ? names[value] : names[0];
}

static const char *
DSL_FHE_Plan_Disposition_Name (UINT32 value)
{
    static const char *names[] = {
        "unknown", "preserve", "domain_wrapper", "fold_into_producer",
        "layout_reinterpret", "require_approximation",
        "require_composite_approximation"
    };
    return DSL_FHE_Plan_Name
               (value, names, sizeof(names) / sizeof(names[0]));
}

static const char *
DSL_FHE_Plan_Approximation_Name (UINT32 value)
{
    static const char *names[] = {
        "unknown", "minimax", "chebyshev", "taylor"
    };
    return DSL_FHE_Plan_Name
               (value, names, sizeof(names) / sizeof(names[0]));
}

static const char *
DSL_FHE_Plan_Scale_Policy_Name (UINT32 value)
{
    static const char *names[] = {
        "unknown", "inherit", "preserve", "explicit"
    };
    return DSL_FHE_Plan_Name
               (value, names, sizeof(names) / sizeof(names[0]));
}

static const char *
DSL_FHE_Plan_Bootstrap_Reason_Name (UINT32 value)
{
    static const char *names[] = {
        "none", "pre_relu_refresh", "depth_exhaustion",
        "manual_boundary_required"
    };
    return DSL_FHE_Plan_Name
               (value, names, sizeof(names) / sizeof(names[0]));
}

static const char *
DSL_FHE_Context_State_Role_Name (UINT32 value)
{
    static const char *names[] = {
        "unknown", "pre_operation", "post_refresh", "post_operation",
        "result"
    };
    return DSL_FHE_Plan_Name
               (value, names, sizeof(names) / sizeof(names[0]));
}

static const char *
DSL_FHE_Context_State_Scheme_Name (UINT32 value)
{
    static const char *names[] = {
        "unknown", "ckks"
    };
    return DSL_FHE_Plan_Name
               (value, names, sizeof(names) / sizeof(names[0]));
}

static const char *
DSL_FHE_Context_State_Value_Class_Name (UINT32 value)
{
    static const char *names[] = {
        "unknown", "ciphertext", "encoded_plaintext", "clear"
    };
    return DSL_FHE_Plan_Name
               (value, names, sizeof(names) / sizeof(names[0]));
}

static const char *
DSL_FHE_Plan_Value_Name (DSL_IR_VALUE_ID value_id)
{
    DSL_IR_VALUE_RECORD value;
    if (!DSL_IR_Image_Get_Value(value_id, &value) ||
        value.name == STR_IDX_ZERO)
        return "";
    return Index_To_Str(value.name);
}

static const char *
DSL_FHE_Plan_Node_Operator_Name (DSL_IR_NODE_ID node_id)
{
    DSL_IR_NODE_RECORD node;
    DSL_IR_OPCODE_DESCRIPTOR_RECORD descriptor;
    if (!DSL_IR_Image_Get_Node(node_id, &node) ||
        !DSL_IR_Image_Get_Opcode_Descriptor
            (node.opcode_descriptor_id, &descriptor))
        return "OPR_DSLUNKNOWN";
    return DSL_OPERATOR_name((DSL_OPERATOR)descriptor.logical_operator);
}

static void
DSL_FHE_Plan_Print_Pending_Integer (FILE *file, const char *name, INT32 value)
{
    if (value == -1)
        fprintf(file, " %s=<pending>", name);
    else
        fprintf(file, " %s=%d", name, value);
}

void
DSL_FHE_Plan_Image_Print (FILE *file)
{
    if (file == NULL || !DSL_FHE_Plan_Image_Has_Records())
        return;
    DSL_FHE_PLAN_IMAGE_HEADER header;
    DSL_FHE_Plan_Image_Get_Header(&header);
    fprintf(file, "\nFHE Plan Image: version=%u capabilities=0x%08x\n",
            header.version, header.capabilities);

    fprintf(file, "FHE Conversion Disposition Table:\n");
    for (UINT32 i = 1; i <= header.disposition_count; ++i) {
        DSL_FHE_CONVERSION_DISPOSITION_RECORD record;
        DSL_FHE_Plan_Get_Conversion_Disposition(i, &record);
        fprintf(file, "  [%u] source_node=node%u source_operator=%s "
                "result=value%u(%s) owner_pu=%s disposition=%s",
                record.id, record.source_node_id,
                DSL_FHE_Plan_Node_Operator_Name(record.source_node_id),
                record.result_value_id,
                DSL_FHE_Plan_Value_Name(record.result_value_id),
                ST_name(St_Table[record.owner_pu_st]),
                DSL_FHE_Plan_Disposition_Name(record.disposition));
        if (record.wrapper_name != STR_IDX_ZERO)
            fprintf(file, " wrapper=%s.v%u",
                    Index_To_Str(record.wrapper_name),
                    record.wrapper_version);
        if (record.approximation_contract_id != 0) {
            if (record.disposition ==
                DSL_FHE_DISPOSITION_REQUIRE_COMPOSITE_APPROXIMATION)
                fprintf(file, " composite_profile=%u",
                        record.approximation_contract_id);
            else
                fprintf(file, " approximation=%u",
                        record.approximation_contract_id);
        }
        fprintf(file, " ckks_state=%u bn_folds=[%u,%u] flags=0x%x\n",
                record.result_ckks_value_state_id,
                record.first_bn_fold_id, record.bn_fold_count, record.flags);
    }

    fprintf(file, "FHE Approximation Contract Table:\n");
    for (UINT32 i = 1; i <= header.approximation_count; ++i) {
        DSL_FHE_APPROXIMATION_CONTRACT_RECORD record;
        DSL_FHE_Plan_Get_Approximation_Contract(i, &record);
        fprintf(file, "  [%u] config=%u polynomial=%s.v%u family=%s "
                "degree=%u coefficients=tcon%u range=[tcon%u,tcon%u] "
                "max_abs_error=tcon%u scale_policy=%s depth=%u "
                "bootstrap=%u pre_refresh=%u flags=0x%x\n", record.id,
                record.config_id, Index_To_Str(record.polynomial_name),
                record.polynomial_version,
                DSL_FHE_Plan_Approximation_Name
                    (record.approximation_family),
                record.degree, (UINT32)record.coefficient_tensor_tcon,
                (UINT32)record.valid_range_min_tcon,
                (UINT32)record.valid_range_max_tcon,
                (UINT32)record.max_abs_error_tcon,
                DSL_FHE_Plan_Scale_Policy_Name(record.scale_policy),
                record.required_multiplicative_depth,
                record.bootstrap_policy, record.requires_pre_refresh,
                record.flags);
    }

    fprintf(file, "FHE CKKS Value State Table:\n");
    for (UINT32 i = 1; i <= header.ckks_value_state_count; ++i) {
        DSL_FHE_CKKS_VALUE_STATE_RECORD record;
        DSL_FHE_Plan_Get_CKKS_Value_State(i, &record);
        fprintf(file, "  [%u] value=value%u(%s) state_version=%u "
                "encryption=%u scheme=%u class=%u", record.id,
                record.value_id, DSL_FHE_Plan_Value_Name(record.value_id),
                record.state_version, record.encryption_descriptor_id,
                record.scheme, record.value_class);
        DSL_FHE_Plan_Print_Pending_Integer(file, "level", record.level);
        DSL_FHE_Plan_Print_Pending_Integer
            (file, "scale_bits", record.scale_bits);
        DSL_FHE_Plan_Print_Pending_Integer
            (file, "components", record.component_count);
        DSL_FHE_Plan_Print_Pending_Integer
            (file, "precision_bits", record.precision_bits);
        fprintf(file, " slots=%u alignment_group=%u layout=%s "
                "pending=0x%x bootstrap_reason=%s\n", record.slot_count,
                record.alignment_group,
                record.encrypted_layout_name == STR_IDX_ZERO ? "<pending>" :
                    Index_To_Str(record.encrypted_layout_name),
                record.pending_actions,
                DSL_FHE_Plan_Bootstrap_Reason_Name
                    (record.pending_bootstrap_reason));
    }

    fprintf(file, "FHE BatchNorm Fold Provenance Table:\n");
    for (UINT32 i = 1; i <= header.bn_fold_count; ++i) {
        DSL_FHE_BN_FOLD_PROVENANCE_RECORD record;
        DSL_PU_SOURCE_IDENTITY_RECORD identity;
        DSL_FHE_Plan_Get_BN_Fold_Provenance(i, &record);
        DSL_Call_Image_Get_PU_Identity
            (record.context_pu_identity_id, &identity);
        fprintf(file, "  [%u] owner_pu=%s conv=node%u batch_norm=node%u "
                "context=%s callsite=%u conv_weight=%s conv_bias=%s "
                "bn_scale=%s bn_bias=%s bn_mean=%s bn_variance=%s "
                "folded_weight=tcon%u folded_bias=tcon%u flags=0x%x\n",
                record.id, ST_name(St_Table[record.owner_pu_st]),
                record.conv_node_id, record.batch_norm_node_id,
                Index_To_Str(identity.canonical_definition_name),
                record.context_callsite_id,
                DSL_FHE_Plan_Value_Name
                    (record.source_conv_weight_value_id),
                DSL_FHE_Plan_Value_Name
                    (record.source_conv_bias_value_id),
                DSL_FHE_Plan_Value_Name(record.source_bn_scale_value_id),
                DSL_FHE_Plan_Value_Name(record.source_bn_bias_value_id),
                DSL_FHE_Plan_Value_Name(record.source_bn_mean_value_id),
                DSL_FHE_Plan_Value_Name
                    (record.source_bn_variance_value_id),
                (UINT32)record.folded_weight_tcon,
                (UINT32)record.folded_bias_tcon, record.flags);
    }
}

static const char *
DSL_FHE_Approx_Reconstruction_Name (UINT32 value)
{
    static const char *names[] = {
        "unknown", "relu_from_normalized_sign"
    };
    return DSL_FHE_Plan_Name
               (value, names, sizeof(names) / sizeof(names[0]));
}

static const char *
DSL_FHE_Approx_Normalization_Name (UINT32 value)
{
    static const char *names[] = {
        "unknown", "positive_context_bound"
    };
    return DSL_FHE_Plan_Name
               (value, names, sizeof(names) / sizeof(names[0]));
}

static const char *
DSL_FHE_Approx_Pre_Refresh_Name (UINT32 value)
{
    static const char *names[] = {
        "unknown", "inherit", "required", "proven_existing"
    };
    return DSL_FHE_Plan_Name
               (value, names, sizeof(names) / sizeof(names[0]));
}

static const char *
DSL_FHE_Approx_Basis_Name (UINT32 value)
{
    static const char *names[] = {
        "unknown", "chebyshev", "monomial"
    };
    return DSL_FHE_Plan_Name
               (value, names, sizeof(names) / sizeof(names[0]));
}

static const char *
DSL_FHE_Approx_Evaluation_Name (UINT32 value)
{
    static const char *names[] = {
        "unknown", "clenshaw", "paterson_stockmeyer", "addition_chain"
    };
    return DSL_FHE_Plan_Name
               (value, names, sizeof(names) / sizeof(names[0]));
}

static const char *
DSL_FHE_Approx_Input_Scale_Name (UINT32 value)
{
    static const char *names[] = {
        "unknown", "any_compatible", "profile_normalized"
    };
    return DSL_FHE_Plan_Name
               (value, names, sizeof(names) / sizeof(names[0]));
}

static const char *
DSL_FHE_Approx_Output_Scale_Name (UINT32 value)
{
    static const char *names[] = {
        "unknown", "preserve_input", "default_rescale"
    };
    return DSL_FHE_Plan_Name
               (value, names, sizeof(names) / sizeof(names[0]));
}

static const char *
DSL_FHE_Approx_Input_Class_Name (UINT32 value)
{
    static const char *names[] = {
        "unknown", "ciphertext", "plaintext", "clear"
    };
    return DSL_FHE_Plan_Name
               (value, names, sizeof(names) / sizeof(names[0]));
}

static const char *
DSL_FHE_Approx_Level_Policy_Name (UINT32 value)
{
    static const char *names[] = {
        "unknown", "any_sufficient", "minimum", "exact"
    };
    return DSL_FHE_Plan_Name
               (value, names, sizeof(names) / sizeof(names[0]));
}

static const char *
DSL_FHE_Approx_Component_Policy_Name (UINT32 value)
{
    static const char *names[] = {
        "unknown", "preserve", "relinearized_two", "may_grow"
    };
    return DSL_FHE_Plan_Name
               (value, names, sizeof(names) / sizeof(names[0]));
}

void
DSL_FHE_Approx_Profile_Image_Print (FILE *file)
{
    if (file == NULL || !DSL_FHE_Approx_Profile_Image_Has_Records())
        return;
    DSL_FHE_APPROX_PROFILE_IMAGE_HEADER header;
    DSL_FHE_Approx_Profile_Image_Get_Header(&header);
    fprintf(file, "\nFHE Composite Approximation Profile Image: "
            "version=%u capabilities=0x%08x\n",
            header.version, header.capabilities);

    fprintf(file, "FHE Composite Profile Table:\n");
    for (UINT32 i = 1; i <= header.profile_count; ++i) {
        DSL_FHE_COMPOSITE_PROFILE_RECORD record;
        DSL_FHE_Approx_Profile_Get(i, &record);
        fprintf(file, "  [%u] config=%u profile=%s.v%u "
                "reconstruction=%s depth=%u normalization=%s "
                "pre_refresh=%s source_revision=%s manifest_sha256=%s "
                "stages=[%u,%u] flags=0x%x\n", record.id,
                record.config_id, Index_To_Str(record.profile_name),
                record.profile_version,
                DSL_FHE_Approx_Reconstruction_Name(record.reconstruction),
                record.total_multiplicative_depth,
                DSL_FHE_Approx_Normalization_Name
                    (record.normalization_policy),
                DSL_FHE_Approx_Pre_Refresh_Name(record.pre_refresh_policy),
                Index_To_Str(record.source_revision),
                Index_To_Str(record.manifest_sha256),
                record.first_stage_id, record.stage_count, record.flags);
    }

    fprintf(file, "FHE Ordered Approximation Stage Table:\n");
    for (UINT32 i = 1; i <= header.stage_count; ++i) {
        DSL_FHE_APPROX_STAGE_RECORD record;
        DSL_FHE_Approx_Stage_Get(i, &record);
        fprintf(file, "  [%u] profile=%u ordinal=%u family=%s basis=%s "
                "degree=%u evaluation=%s input_class=%s "
                "input_scale=%s input_level_policy=%s "
                "required_level=%d level_consumption=%d "
                "output_scale=%s output_components=%s "
                "minimum_precision=%d coefficients=tcon%u "
                "coefficient_sha256=%s flags=0x%x\n", record.id,
                record.profile_id, record.stage_ordinal,
                DSL_FHE_Plan_Approximation_Name
                    (record.approximation_family),
                DSL_FHE_Approx_Basis_Name(record.basis), record.degree,
                DSL_FHE_Approx_Evaluation_Name(record.evaluation_scheme),
                DSL_FHE_Approx_Input_Class_Name
                    (record.required_input_value_class),
                DSL_FHE_Approx_Input_Scale_Name(record.input_scale_policy),
                DSL_FHE_Approx_Level_Policy_Name(record.input_level_policy),
                record.required_input_level,
                record.level_consumption,
                DSL_FHE_Approx_Output_Scale_Name(record.output_scale_policy),
                DSL_FHE_Approx_Component_Policy_Name
                    (record.output_component_policy),
                record.minimum_precision_bits,
                (UINT32)record.coefficient_tensor_tcon,
                Index_To_Str(record.coefficient_sha256), record.flags);
    }

    fprintf(file, "FHE Composite Approximation Association Table:\n");
    for (UINT32 i = 1; i <= header.association_count; ++i) {
        DSL_FHE_APPROX_ASSOCIATION_RECORD record;
        DSL_FHE_Approx_Association_Get(i, &record);
        fprintf(file, "  [%u] disposition=%u source_relu=value%u(%s) "
                "profile=%u owner_pu=%s flags=0x%x\n", record.id,
                record.disposition_id, record.source_relu_value_id,
                DSL_FHE_Plan_Value_Name(record.source_relu_value_id),
                record.profile_id, ST_name(St_Table[record.owner_pu_st]),
                record.flags);
    }

    fprintf(file, "FHE ReLU Context Range Table:\n");
    for (UINT32 i = 1; i <= header.context_range_count; ++i) {
        DSL_FHE_CONTEXT_RANGE_RECORD record;
        DSL_FHE_Context_Range_Get(i, &record);
        fprintf(file, "  [%u] profile=%u source_relu=value%u(%s) "
                "owner_pu=%s context_identity=%u callsite=%u "
                "positive_bound=tcon%u observed=[tcon%u,tcon%u] "
                "out_of_range=reject provenance=%s flags=0x%x\n",
                record.id, record.profile_id, record.source_relu_value_id,
                DSL_FHE_Plan_Value_Name(record.source_relu_value_id),
                ST_name(St_Table[record.owner_pu_st]),
                record.context_pu_identity_id, record.context_callsite_id,
                (UINT32)record.positive_bound_tcon,
                (UINT32)record.observed_min_tcon,
                (UINT32)record.observed_max_tcon,
                Index_To_Str(record.provenance), record.flags);
    }
}

void
DSL_FHE_Context_State_Image_Print (FILE *file)
{
    if (file == NULL || !DSL_FHE_Context_State_Image_Has_Records())
        return;
    DSL_FHE_CONTEXT_STATE_IMAGE_HEADER header;
    DSL_FHE_Context_State_Image_Get_Header(&header);
    fprintf(file, "\nFHE Context CKKS State Image: version=%u "
            "capabilities=0x%08x\n", header.version, header.capabilities);
    fprintf(file, "FHE Context CKKS State Table:\n");
    for (UINT32 i = 1; i <= header.context_ckks_state_count; ++i) {
        DSL_FHE_CONTEXT_CKKS_STATE_RECORD record;
        DSL_PU_SOURCE_IDENTITY_RECORD identity;
        DSL_FHE_Context_State_Get(i, &record);
        DSL_Call_Image_Get_PU_Identity
            (record.context_pu_identity_id, &identity);
        fprintf(file, "  [%u] owner_pu=%s source=value%u(%s) "
                "context_identity=%u(%s) callsite=%u role=%s "
                "state_version=%u encryption=%u scheme=%s class=%s",
                record.id, ST_name(St_Table[record.owner_pu_st]),
                record.source_value_id,
                DSL_FHE_Plan_Value_Name(record.source_value_id),
                record.context_pu_identity_id,
                Index_To_Str(identity.canonical_definition_name),
                record.context_callsite_id,
                DSL_FHE_Context_State_Role_Name(record.state_role),
                record.state_version, record.encryption_descriptor_id,
                DSL_FHE_Context_State_Scheme_Name(record.scheme),
                DSL_FHE_Context_State_Value_Class_Name(record.value_class));
        DSL_FHE_Plan_Print_Pending_Integer(file, "level", record.level);
        DSL_FHE_Plan_Print_Pending_Integer
            (file, "scale_bits", record.scale_bits);
        DSL_FHE_Plan_Print_Pending_Integer
            (file, "components", record.component_count);
        DSL_FHE_Plan_Print_Pending_Integer
            (file, "precision_bits", record.precision_bits);
        fprintf(file, " slots=%u alignment_group=%u layout=%s "
                "pending=0x%x bootstrap_reason=%s flags=0x%x\n",
                record.slot_count, record.alignment_group,
                record.encrypted_layout_name == STR_IDX_ZERO ? "<pending>" :
                    Index_To_Str(record.encrypted_layout_name),
                record.pending_actions,
                DSL_FHE_Plan_Bootstrap_Reason_Name
                    (record.pending_bootstrap_reason), record.flags);
    }
}
