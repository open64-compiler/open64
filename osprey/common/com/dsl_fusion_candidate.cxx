/*
 * Copyright (C) 2026 Open64 Project
 */

/*
 * Discovers AIO-5 PU-local fusion candidates from logical DSL contracts and
 * records check-only plans. It does not rewrite executable WHIRL or persist a
 * new binary image. Design:
 * doc/AI-COMPILER-OPTIMIZATION-AIO5-FUSION-CANDIDATES.md.
 */

#include <string.h>
#include <vector>

#include "dsl_fusion_candidate.h"
#include "dsl_opcode.h"
#include "pu_info.h"

struct DSL_FUSION_PATTERN_MATCH {
    UINT32 pattern;
    DSL_IR_NODE_ID root_node_id;
    DSL_IR_VALUE_ID result_value_id;
    std::vector<DSL_IR_NODE_ID> members;
    std::vector<UINT32> member_roles;
    std::vector<DSL_FUSION_BOUNDARY_RECORD> boundaries;
    std::vector<DSL_IR_VALUE_ID> eliminated_values;
};

struct DSL_FUSION_CANDIDATE_ANALYSIS {
    PU_Info *pu;
    ST_IDX owner_pu_st;
    const DSL_TENSOR_EVOLUTION_GRAPH *graph;
    const DSL_TENSOR_ANALYSIS *tensor_analysis;
    const DSL_TENSOR_LOCALITY_ANALYSIS *locality;
    const DSL_LOGICAL_LAYOUT_ANALYSIS *layout;
    DSL_FUSION_CONTROL control;
    std::vector<DSL_FUSION_SITE_RECORD> sites;
    std::vector<DSL_FUSION_MEMBER_RECORD> members;
    std::vector<DSL_FUSION_BOUNDARY_RECORD> boundaries;
    std::vector<DSL_OPT_PLAN_CONTEXT *> plans;
    BOOL built;
};

static const char *DSL_fusion_pattern_name_table[] = {
    "unknown", "matmul_bias_activation", "residual_activation",
    "generic_cluster"
};

static const char *DSL_fusion_member_role_name_table[] = {
    "unknown", "matmul", "bias_add", "residual_add", "activation",
    "generic_contraction", "generic_pointwise"
};

static const char *DSL_fusion_boundary_kind_name_table[] = {
    "unknown", "input", "output", "alternative_cut"
};

static const char *DSL_fusion_fact_state_name_table[] = {
    "unknown", "proven", "rejected"
};

static BOOL
DSL_Fusion_Report (FILE *diagnostic, const char *message, UINT32 id)
{
    if (diagnostic != NULL)
        fprintf(diagnostic, "DSL fusion candidate error: %s id=%u\n",
                message, id);
    return FALSE;
}

static BOOL
DSL_Fusion_Owner_Valid (ST_IDX owner_pu_st)
{
    return ST_IDX_level(owner_pu_st) == GLOBAL_SYMTAB &&
           ST_IDX_index(owner_pu_st) != 0 &&
           ST_IDX_index(owner_pu_st) < ST_Table_Size(GLOBAL_SYMTAB) &&
           ST_class(owner_pu_st) == CLASS_FUNC &&
           ST_pu(St_Table[owner_pu_st]) != PU_IDX_ZERO &&
           ST_pu(St_Table[owner_pu_st]) < PU_Table_Size();
}

static BOOL
DSL_Fusion_Active (const DSL_FUSION_CANDIDATE_ANALYSIS *analysis)
{
    return analysis != NULL && analysis->pu != NULL &&
           Current_PU_Info == analysis->pu &&
           DSL_Fusion_Owner_Valid(analysis->owner_pu_st) &&
           PU_Info_proc_sym(analysis->pu) == analysis->owner_pu_st &&
           Current_pu ==
               &Pu_Table[ST_pu(St_Table[analysis->owner_pu_st])];
}

const char *
DSL_fusion_pattern_name (UINT32 pattern)
{
    return pattern < sizeof(DSL_fusion_pattern_name_table) /
                         sizeof(DSL_fusion_pattern_name_table[0]) ?
           DSL_fusion_pattern_name_table[pattern] : "unknown";
}

const char *
DSL_fusion_member_role_name (UINT32 role)
{
    return role < sizeof(DSL_fusion_member_role_name_table) /
                      sizeof(DSL_fusion_member_role_name_table[0]) ?
           DSL_fusion_member_role_name_table[role] : "unknown";
}

const char *
DSL_fusion_boundary_kind_name (UINT32 kind)
{
    return kind < sizeof(DSL_fusion_boundary_kind_name_table) /
                      sizeof(DSL_fusion_boundary_kind_name_table[0]) ?
           DSL_fusion_boundary_kind_name_table[kind] : "unknown";
}

const char *
DSL_fusion_fact_state_name (UINT32 state)
{
    return state < sizeof(DSL_fusion_fact_state_name_table) /
                       sizeof(DSL_fusion_fact_state_name_table[0]) ?
           DSL_fusion_fact_state_name_table[state] : "unknown";
}

void
DSL_fusion_control_init (DSL_FUSION_CONTROL *control)
{
    if (control == NULL)
        return;
    memset(control, 0, sizeof(*control));
    control->generate_candidates = 1;
    control->target_profile_id = 1;
    control->max_sites = 64;
    control->enable_semantic_patterns = 1;
    control->max_cluster_members = 8;
}

static BOOL
DSL_Fusion_Control_Valid (const DSL_FUSION_CONTROL &control)
{
    return control.reserved == 0 && control.generate_candidates <= 1 &&
           control.select_plans <= 1 && control.apply_transformation <= 1 &&
           control.enable_semantic_patterns <= 1 &&
           control.enable_generic_clusters <= 1 &&
           control.max_sites != 0 && control.target_profile_id != 0 &&
           control.max_cluster_members >= 2 &&
           (!control.select_plans || control.generate_candidates) &&
           control.apply_transformation == 0;
}

static BOOL
DSL_Fusion_Node_Info
        (DSL_IR_NODE_ID node_id, DSL_IR_NODE_RECORD *node,
         DSL_IR_OPCODE_DESCRIPTOR_RECORD *descriptor)
{
    return node != NULL && descriptor != NULL &&
           DSL_IR_Image_Get_Node(node_id, node) &&
           (node->flags & DSL_IR_NODE_FLAG_RETIRED) == 0 &&
           DSL_IR_Image_Get_Opcode_Descriptor
               (node->opcode_descriptor_id, descriptor);
}

static BOOL
DSL_Fusion_Operand
        (const DSL_IR_NODE_RECORD &node, UINT32 ordinal,
         DSL_IR_VALUE_ID *value_id)
{
    DSL_IR_VALUE_REFERENCE_RECORD reference;
    if (value_id != NULL)
        *value_id = DSL_IR_VALUE_INVALID_ID;
    if (ordinal >= node.operand_count ||
        !DSL_IR_Image_Get_Value_Reference
             (node.first_operand_reference_id + ordinal, &reference) ||
        reference.owner_node_id != node.id ||
        reference.ordinal != ordinal || value_id == NULL)
        return FALSE;
    *value_id = reference.value_id;
    return TRUE;
}

static BOOL
DSL_Fusion_Value_Producer
        (DSL_IR_VALUE_ID value_id, DSL_IR_VALUE_RECORD *value,
         DSL_IR_NODE_RECORD *node,
         DSL_IR_OPCODE_DESCRIPTOR_RECORD *descriptor)
{
    return DSL_IR_Image_Get_Value(value_id, value) &&
           value->producer_node_id != DSL_IR_NODE_INVALID_ID &&
           DSL_Fusion_Node_Info(value->producer_node_id, node, descriptor) &&
           node->result_value_id == value_id;
}

static void
DSL_Fusion_Add_Boundary
        (DSL_FUSION_PATTERN_MATCH *match, DSL_IR_VALUE_ID value_id,
         DSL_IR_NODE_ID producer_node_id, DSL_IR_NODE_ID consumer_node_id,
         UINT32 kind, UINT32 operand_ordinal)
{
    DSL_FUSION_BOUNDARY_RECORD boundary;
    memset(&boundary, 0, sizeof(boundary));
    boundary.value_id = value_id;
    boundary.producer_node_id = producer_node_id;
    boundary.consumer_node_id = consumer_node_id;
    boundary.kind = kind;
    boundary.operand_ordinal = operand_ordinal;
    match->boundaries.push_back(boundary);
}

static BOOL
DSL_Fusion_Match_Matmul_Bias_Activation
        (const DSL_IR_NODE_RECORD &activation,
         DSL_FUSION_PATTERN_MATCH *match)
{
    DSL_IR_VALUE_ID add_value;
    DSL_IR_VALUE_RECORD add_result;
    DSL_IR_NODE_RECORD add;
    DSL_IR_OPCODE_DESCRIPTOR_RECORD add_descriptor;
    DSL_IR_VALUE_ID add_operands[2];
    DSL_IR_VALUE_RECORD candidate_value;
    DSL_IR_NODE_RECORD matmul;
    UINT32 matmul_ordinal = 2;
    UINT32 matmul_count = 0;

    if (activation.operand_count != 1 ||
        !DSL_Fusion_Operand(activation, 0, &add_value) ||
        !DSL_Fusion_Value_Producer
             (add_value, &add_result, &add, &add_descriptor) ||
        add_descriptor.logical_operator != OPR_DSLADD ||
        add.operand_count != 2 ||
        !DSL_Fusion_Operand(add, 0, &add_operands[0]) ||
        !DSL_Fusion_Operand(add, 1, &add_operands[1]))
        return FALSE;
    for (UINT32 i = 0; i < 2; ++i) {
        DSL_IR_NODE_RECORD producer;
        DSL_IR_OPCODE_DESCRIPTOR_RECORD producer_descriptor;
        if (DSL_Fusion_Value_Producer
                (add_operands[i], &candidate_value, &producer,
                 &producer_descriptor) &&
            producer_descriptor.logical_operator == OPR_DSLMATMUL) {
            ++matmul_count;
            matmul_ordinal = i;
            matmul = producer;
        }
    }
    if (matmul_count != 1 || matmul_ordinal >= 2 ||
        matmul.operand_count != 2)
        return FALSE;

    match->pattern = DSL_FUSION_PATTERN_MATMUL_BIAS_ACTIVATION;
    match->root_node_id = activation.id;
    match->result_value_id = activation.result_value_id;
    match->members.push_back(matmul.id);
    match->member_roles.push_back(DSL_FUSION_MEMBER_MATMUL);
    match->members.push_back(add.id);
    match->member_roles.push_back(DSL_FUSION_MEMBER_BIAS_ADD);
    match->members.push_back(activation.id);
    match->member_roles.push_back(DSL_FUSION_MEMBER_ACTIVATION);
    match->eliminated_values.push_back(matmul.result_value_id);
    match->eliminated_values.push_back(add.result_value_id);

    for (UINT32 i = 0; i < matmul.operand_count; ++i) {
        DSL_IR_VALUE_ID value_id;
        DSL_IR_VALUE_RECORD value;
        if (!DSL_Fusion_Operand(matmul, i, &value_id) ||
            !DSL_IR_Image_Get_Value(value_id, &value))
            return FALSE;
        DSL_Fusion_Add_Boundary
            (match, value_id, value.producer_node_id, matmul.id,
             DSL_FUSION_BOUNDARY_INPUT, i);
    }
    DSL_IR_VALUE_ID bias_id = add_operands[1 - matmul_ordinal];
    DSL_IR_VALUE_RECORD bias;
    if (!DSL_IR_Image_Get_Value(bias_id, &bias))
        return FALSE;
    DSL_Fusion_Add_Boundary
        (match, bias_id, bias.producer_node_id, add.id,
         DSL_FUSION_BOUNDARY_INPUT, 1 - matmul_ordinal);
    DSL_Fusion_Add_Boundary
        (match, matmul.result_value_id, matmul.id, add.id,
         DSL_FUSION_BOUNDARY_ALTERNATIVE_CUT, matmul_ordinal);
    DSL_Fusion_Add_Boundary
        (match, add.result_value_id, add.id, activation.id,
         DSL_FUSION_BOUNDARY_ALTERNATIVE_CUT, 0);
    DSL_Fusion_Add_Boundary
        (match, activation.result_value_id, activation.id,
         DSL_IR_NODE_INVALID_ID, DSL_FUSION_BOUNDARY_OUTPUT,
         DSL_FUSION_BOUNDARY_NO_OPERAND);
    return TRUE;
}

static BOOL
DSL_Fusion_Match_Residual_Activation
        (const DSL_IR_NODE_RECORD &activation,
         DSL_FUSION_PATTERN_MATCH *match)
{
    DSL_IR_VALUE_ID residual_value;
    DSL_IR_VALUE_RECORD residual_result;
    DSL_IR_NODE_RECORD residual;
    DSL_IR_OPCODE_DESCRIPTOR_RECORD residual_descriptor;

    if (activation.operand_count != 1 ||
        !DSL_Fusion_Operand(activation, 0, &residual_value) ||
        !DSL_Fusion_Value_Producer
             (residual_value, &residual_result, &residual,
              &residual_descriptor) ||
        residual_descriptor.logical_operator != OPR_DSLRESIDUALADD ||
        residual.operand_count != 2)
        return FALSE;

    match->pattern = DSL_FUSION_PATTERN_RESIDUAL_ACTIVATION;
    match->root_node_id = activation.id;
    match->result_value_id = activation.result_value_id;
    match->members.push_back(residual.id);
    match->member_roles.push_back(DSL_FUSION_MEMBER_RESIDUAL_ADD);
    match->members.push_back(activation.id);
    match->member_roles.push_back(DSL_FUSION_MEMBER_ACTIVATION);
    match->eliminated_values.push_back(residual.result_value_id);
    for (UINT32 i = 0; i < residual.operand_count; ++i) {
        DSL_IR_VALUE_ID value_id;
        DSL_IR_VALUE_RECORD value;
        if (!DSL_Fusion_Operand(residual, i, &value_id) ||
            !DSL_IR_Image_Get_Value(value_id, &value))
            return FALSE;
        DSL_Fusion_Add_Boundary
            (match, value_id, value.producer_node_id, residual.id,
             DSL_FUSION_BOUNDARY_INPUT, i);
    }
    DSL_Fusion_Add_Boundary
        (match, residual.result_value_id, residual.id, activation.id,
         DSL_FUSION_BOUNDARY_ALTERNATIVE_CUT, 0);
    DSL_Fusion_Add_Boundary
        (match, activation.result_value_id, activation.id,
         DSL_IR_NODE_INVALID_ID, DSL_FUSION_BOUNDARY_OUTPUT,
         DSL_FUSION_BOUNDARY_NO_OPERAND);
    return TRUE;
}

static BOOL
DSL_Fusion_Match
        (DSL_IR_NODE_ID root_node_id, DSL_FUSION_PATTERN_MATCH *match)
{
    DSL_IR_NODE_RECORD activation;
    DSL_IR_OPCODE_DESCRIPTOR_RECORD descriptor;
    if (match == NULL ||
        !DSL_Fusion_Node_Info(root_node_id, &activation, &descriptor) ||
        descriptor.logical_operator != OPR_DSLRELU)
        return FALSE;
    if (DSL_Fusion_Match_Matmul_Bias_Activation(activation, match))
        return TRUE;
    return DSL_Fusion_Match_Residual_Activation(activation, match);
}

static BOOL
DSL_Fusion_Generic_Trait
        (const DSL_IR_OPCODE_DESCRIPTOR_RECORD &descriptor,
         DSL_FUSIBILITY_INFO *trait)
{
    return DSL_Operator_Get_Fusibility_Info
               ((DSL_OPERATOR)descriptor.logical_operator,
                descriptor.version, trait) &&
           (trait->flags & DSL_FUSIBILITY_SEMANTIC_PATTERN_REQUIRED) == 0 &&
           (trait->flags & DSL_FUSIBILITY_SINGLE_RESULT) != 0;
}

static BOOL
DSL_Fusion_Generic_Has_Forward_Consumer
        (const DSL_FUSION_CANDIDATE_ANALYSIS *analysis,
         DSL_IR_VALUE_ID value_id)
{
    DSL_TENSOR_FACT_RECORD fact;
    DSL_TENSOR_USE_FACT_RECORD use;
    DSL_FUSIBILITY_INFO trait;
    if (!DSL_tensor_analysis_find_fact
             (analysis->tensor_analysis, value_id, &fact) ||
        fact.use_count != 1 ||
        !DSL_tensor_analysis_get_use
             (analysis->tensor_analysis, fact.first_use_id, &use) ||
        !DSL_Operator_Get_Fusibility_Info
             ((DSL_OPERATOR)use.consumer_operator,
              use.consumer_version, &trait))
        return FALSE;
    return (trait.flags & DSL_FUSIBILITY_CONSUMER) != 0 &&
           (trait.flags & DSL_FUSIBILITY_SEMANTIC_PATTERN_REQUIRED) == 0;
}

static BOOL
DSL_Fusion_Generic_Member_Index
        (const DSL_FUSION_PATTERN_MATCH &match, DSL_IR_NODE_ID node_id,
         UINT32 *index)
{
    for (UINT32 i = 0; i < match.members.size(); ++i) {
        if (match.members[i] == node_id) {
            if (index != NULL)
                *index = i;
            return TRUE;
        }
    }
    return FALSE;
}

static BOOL
DSL_Fusion_Match_Generic
        (const DSL_FUSION_CANDIDATE_ANALYSIS *analysis,
         DSL_IR_NODE_ID root_node_id, DSL_FUSION_PATTERN_MATCH *match)
{
    DSL_IR_NODE_RECORD root;
    DSL_IR_OPCODE_DESCRIPTOR_RECORD root_descriptor;
    DSL_FUSIBILITY_INFO root_trait;
    std::vector<DSL_IR_NODE_ID> reverse_members;
    std::vector<UINT32> reverse_roles;

    if (analysis == NULL || match == NULL ||
        !DSL_Fusion_Node_Info
             (root_node_id, &root, &root_descriptor) ||
        !DSL_Fusion_Generic_Trait(root_descriptor, &root_trait) ||
        (root_trait.flags & DSL_FUSIBILITY_CONSUMER) == 0 ||
        root_descriptor.effect_model != DSL_EFFECT_MODEL_PURE ||
        DSL_Fusion_Generic_Has_Forward_Consumer
            (analysis, root.result_value_id))
        return FALSE;

    /*
     * Grow one deterministic producer chain backward from the consumer. A
     * fan-out, diamond, effect, semantic-pattern boundary, or budget limit
     * terminates the cluster; this stage records a candidate and never fuses.
     */
    DSL_IR_NODE_RECORD current = root;
    DSL_FUSIBILITY_INFO current_trait = root_trait;
    while (TRUE) {
        reverse_members.push_back(current.id);
        reverse_roles.push_back
            (current_trait.iteration_space ==
                 DSL_FUSION_ITERATION_CONTRACTION ?
             DSL_FUSION_MEMBER_GENERIC_CONTRACTION :
             DSL_FUSION_MEMBER_GENERIC_POINTWISE);
        if ((current_trait.flags & DSL_FUSIBILITY_CLUSTER_ANCHOR) != 0 ||
            reverse_members.size() >=
                analysis->control.max_cluster_members)
            break;

        DSL_IR_NODE_RECORD next;
        DSL_FUSIBILITY_INFO next_trait;
        UINT32 eligible_count = 0;
        for (UINT32 operand = 0; operand < current.operand_count; ++operand) {
            DSL_IR_VALUE_ID value_id;
            DSL_IR_VALUE_RECORD value;
            DSL_IR_NODE_RECORD producer;
            DSL_IR_OPCODE_DESCRIPTOR_RECORD producer_descriptor;
            DSL_FUSIBILITY_INFO producer_trait;
            if (!DSL_Fusion_Operand(current, operand, &value_id) ||
                !DSL_Fusion_Value_Producer
                     (value_id, &value, &producer,
                      &producer_descriptor) ||
                !DSL_Fusion_Generic_Trait
                     (producer_descriptor, &producer_trait) ||
                (producer_trait.flags & DSL_FUSIBILITY_PRODUCER) == 0 ||
                producer_descriptor.effect_model != DSL_EFFECT_MODEL_PURE)
                continue;
            ++eligible_count;
            next = producer;
            next_trait = producer_trait;
        }
        if (eligible_count != 1)
            break;
        current = next;
        current_trait = next_trait;
    }
    if (reverse_members.size() < 2)
        return FALSE;

    match->pattern = DSL_FUSION_PATTERN_GENERIC_CLUSTER;
    match->root_node_id = root.id;
    match->result_value_id = root.result_value_id;
    for (INT32 i = (INT32)reverse_members.size() - 1; i >= 0; --i) {
        match->members.push_back(reverse_members[i]);
        match->member_roles.push_back(reverse_roles[i]);
    }
    for (UINT32 i = 0; i + 1 < match->members.size(); ++i) {
        DSL_IR_NODE_RECORD member;
        if (!DSL_IR_Image_Get_Node(match->members[i], &member))
            return FALSE;
        match->eliminated_values.push_back(member.result_value_id);
    }
    for (UINT32 i = 0; i < match->members.size(); ++i) {
        DSL_IR_NODE_RECORD member;
        if (!DSL_IR_Image_Get_Node(match->members[i], &member))
            return FALSE;
        for (UINT32 operand = 0; operand < member.operand_count; ++operand) {
            DSL_IR_VALUE_ID value_id;
            DSL_IR_VALUE_RECORD value;
            UINT32 producer_index;
            if (!DSL_Fusion_Operand(member, operand, &value_id) ||
                !DSL_IR_Image_Get_Value(value_id, &value))
                return FALSE;
            if (DSL_Fusion_Generic_Member_Index
                    (*match, value.producer_node_id, &producer_index)) {
                DSL_Fusion_Add_Boundary
                    (match, value_id, value.producer_node_id, member.id,
                     DSL_FUSION_BOUNDARY_ALTERNATIVE_CUT, operand);
            } else {
                DSL_Fusion_Add_Boundary
                    (match, value_id, value.producer_node_id, member.id,
                     DSL_FUSION_BOUNDARY_INPUT, operand);
            }
        }
    }
    DSL_Fusion_Add_Boundary
        (match, root.result_value_id, root.id, DSL_IR_NODE_INVALID_ID,
         DSL_FUSION_BOUNDARY_OUTPUT, DSL_FUSION_BOUNDARY_NO_OPERAND);
    return TRUE;
}

static BOOL
DSL_Fusion_Node_Has_State_Effect (DSL_IR_NODE_ID node_id)
{
    for (DSL_STATE_EFFECT_ID id = 1;
         id <= DSL_Effect_Image_State_Effect_Count(); ++id) {
        DSL_STATE_EFFECT_RECORD effect;
        if (DSL_Effect_Image_Get_State_Effect(id, &effect) &&
            effect.owner_node_id == node_id)
            return TRUE;
    }
    return FALSE;
}

static BOOL
DSL_Fusion_Add_U64 (UINT64 value, UINT64 *sum)
{
    if (sum == NULL || value == DSL_FUSION_UNKNOWN_U64 ||
        DSL_FUSION_UNKNOWN_U64 - *sum < value)
        return FALSE;
    *sum += value;
    return TRUE;
}

static UINT32
DSL_Fusion_Layout_State
        (const DSL_FUSION_CANDIDATE_ANALYSIS *analysis,
         const DSL_FUSION_PATTERN_MATCH &match)
{
    if (analysis->layout == NULL)
        return DSL_FUSION_FACT_UNKNOWN;
    UINT32 aggregate = DSL_FUSION_FACT_PROVEN;
    for (UINT32 i = 0; i < match.eliminated_values.size(); ++i) {
        DSL_LOGICAL_LAYOUT_SITE_RECORD site;
        if (!DSL_logical_layout_find_site
                 (analysis->layout, match.eliminated_values[i], &site)) {
            aggregate = DSL_FUSION_FACT_UNKNOWN;
            continue;
        }
        BOOL found_proven = FALSE;
        BOOL found_unknown = FALSE;
        for (UINT32 j = 0; j < site.alternative_count; ++j) {
            DSL_LOGICAL_LAYOUT_ALTERNATIVE_RECORD alternative;
            if (!DSL_logical_layout_get_alternative
                     (analysis->layout, site.first_alternative_id + j,
                      &alternative)) {
                found_unknown = TRUE;
                continue;
            }
            if (alternative.compatibility_state ==
                    DSL_LAYOUT_COMPATIBILITY_PROVEN)
                found_proven = TRUE;
            else if (alternative.compatibility_state ==
                         DSL_LAYOUT_COMPATIBILITY_UNKNOWN)
                found_unknown = TRUE;
        }
        if (!found_proven) {
            if (found_unknown || site.alternative_count == 0)
                aggregate = DSL_FUSION_FACT_UNKNOWN;
            else
                return DSL_FUSION_FACT_REJECTED;
        }
    }
    return aggregate;
}

static void
DSL_Fusion_Classify
        (const DSL_FUSION_CANDIDATE_ANALYSIS *analysis,
         const DSL_FUSION_PATTERN_MATCH &match,
         DSL_FUSION_SITE_RECORD *site)
{
    DSL_IR_VALUE_RECORD result;
    BOOL descriptor_ok = DSL_IR_Image_Get_Value(match.result_value_id,
                                                 &result);
    BOOL semantic_ok = TRUE;
    BOOL effect_ok = TRUE;
    BOOL ownership_ok = TRUE;
    BOOL control_exact = TRUE;
    BOOL eliminated_size_known = TRUE;
    BOOL growth_size_known = TRUE;
    UINT64 eliminated_bytes = 0;
    UINT64 growth_bytes = 0;

    site->representation_constraints =
        DSL_FUSION_REPRESENTATION_EXACT_INTERMEDIATE |
        DSL_FUSION_REPRESENTATION_PRESERVE_REGION;
    if (match.pattern == DSL_FUSION_PATTERN_MATMUL_BIAS_ACTIVATION)
        site->representation_constraints |=
            DSL_FUSION_REPRESENTATION_BROADCAST_BIAS;

    if (match.pattern == DSL_FUSION_PATTERN_MATMUL_BIAS_ACTIVATION) {
        BOOL found_bias = FALSE;
        for (UINT32 i = 0; i < match.boundaries.size(); ++i) {
            const DSL_FUSION_BOUNDARY_RECORD &boundary =
                match.boundaries[i];
            if (boundary.kind == DSL_FUSION_BOUNDARY_INPUT &&
                boundary.consumer_node_id == match.members[1]) {
                DSL_TENSOR_FACT_RECORD bias;
                found_bias = TRUE;
                if (!DSL_tensor_analysis_find_fact
                         (analysis->tensor_analysis, boundary.value_id,
                          &bias) ||
                    (bias.value_role != DSL_TENSOR_VALUE_ROLE_CONSTANT &&
                     bias.value_role != DSL_TENSOR_VALUE_ROLE_FORMAL &&
                     bias.value_role != DSL_TENSOR_VALUE_ROLE_SYMBOL))
                    semantic_ok = FALSE;
            }
        }
        if (!found_bias)
            semantic_ok = FALSE;
    }

    for (UINT32 i = 0; i < match.members.size(); ++i) {
        DSL_IR_NODE_RECORD node;
        DSL_IR_OPCODE_DESCRIPTOR_RECORD descriptor;
        DSL_IR_VALUE_RECORD value;
        if (!DSL_Fusion_Node_Info(match.members[i], &node, &descriptor) ||
            descriptor.effect_model != DSL_EFFECT_MODEL_PURE ||
            DSL_Fusion_Node_Has_State_Effect(node.id))
            effect_ok = FALSE;
        if (!DSL_IR_Image_Get_Value(node.result_value_id, &value) ||
            !descriptor_ok || value.ty != result.ty)
            descriptor_ok = FALSE;
    }

    for (UINT32 i = 0; i < match.eliminated_values.size(); ++i) {
        DSL_TENSOR_FACT_RECORD tensor;
        DSL_TENSOR_USE_FACT_RECORD use;
        DSL_TENSOR_LOCALITY_FACT_RECORD locality;
        if (!DSL_tensor_analysis_find_fact
                 (analysis->tensor_analysis, match.eliminated_values[i],
                  &tensor) ||
            !DSL_tensor_locality_find_fact
                 (analysis->locality, match.eliminated_values[i],
                  &locality)) {
            control_exact = FALSE;
            eliminated_size_known = FALSE;
            continue;
        }
        if (tensor.use_count != 1 ||
            !DSL_tensor_analysis_get_use
                 (analysis->tensor_analysis, tensor.first_use_id, &use) ||
            i + 1 >= match.members.size() ||
            use.consumer_node_id != match.members[i + 1] ||
            locality.alias_state != DSL_TENSOR_ALIAS_PROVEN_UNIQUE)
            ownership_ok = FALSE;
        if (locality.lifetime_state == DSL_TENSOR_LIFETIME_EFFECT)
            effect_ok = FALSE;
        else if (locality.lifetime_state !=
                     DSL_TENSOR_LIFETIME_EXACT_BLOCK ||
                 locality.reuse_distance_state != DSL_TENSOR_DISTANCE_EXACT)
            control_exact = FALSE;
        if (locality.size_state != DSL_TENSOR_SIZE_STATIC ||
            !DSL_Fusion_Add_U64(locality.object_bytes,
                                &eliminated_bytes))
            eliminated_size_known = FALSE;
    }
    for (UINT32 i = 0; i < match.boundaries.size(); ++i) {
        if (match.boundaries[i].kind == DSL_FUSION_BOUNDARY_INPUT) {
            DSL_TENSOR_LOCALITY_FACT_RECORD locality;
            if (!DSL_tensor_locality_find_fact
                     (analysis->locality, match.boundaries[i].value_id,
                      &locality) ||
                locality.size_state != DSL_TENSOR_SIZE_STATIC ||
                !DSL_Fusion_Add_U64(locality.object_bytes, &growth_bytes))
                growth_size_known = FALSE;
        }
    }

    site->semantic_state = semantic_ok ? DSL_FUSION_FACT_PROVEN :
                                         DSL_FUSION_FACT_REJECTED;
    site->descriptor_state = descriptor_ok ? DSL_FUSION_FACT_PROVEN :
                                             DSL_FUSION_FACT_REJECTED;
    site->effect_state = effect_ok ? DSL_FUSION_FACT_PROVEN :
                                     DSL_FUSION_FACT_REJECTED;
    site->layout_state = DSL_Fusion_Layout_State(analysis, match);
    site->eliminated_materialization_count =
        match.eliminated_values.size();
    site->eliminated_materialization_bytes =
        eliminated_size_known ? eliminated_bytes : DSL_FUSION_UNKNOWN_U64;
    site->live_range_growth_bytes =
        growth_size_known ? growth_bytes : DSL_FUSION_UNKNOWN_U64;
    site->live_range_growth_statements = match.members.size() - 1;

    if (!semantic_ok) {
        site->resource_state = DSL_FUSION_FACT_UNKNOWN;
        site->legality = DSL_OPT_LEGALITY_REJECTED;
        site->rejection_reason = DSL_OPT_REJECT_MALFORMED;
    } else if (!descriptor_ok) {
        site->resource_state = DSL_FUSION_FACT_UNKNOWN;
        site->legality = DSL_OPT_LEGALITY_REJECTED;
        site->rejection_reason = DSL_OPT_REJECT_DESCRIPTOR;
    } else if (!effect_ok) {
        site->resource_state = DSL_FUSION_FACT_UNKNOWN;
        site->legality = DSL_OPT_LEGALITY_REJECTED;
        site->rejection_reason = DSL_OPT_REJECT_EFFECT;
    } else if (!ownership_ok) {
        site->resource_state = DSL_FUSION_FACT_UNKNOWN;
        site->legality = DSL_OPT_LEGALITY_REJECTED;
        site->rejection_reason = DSL_OPT_REJECT_OWNERSHIP;
    } else if (!control_exact || !eliminated_size_known ||
               !growth_size_known ||
               analysis->control.resource_limit_bytes == 0) {
        site->resource_state = DSL_FUSION_FACT_UNKNOWN;
        site->legality = DSL_OPT_LEGALITY_UNKNOWN;
        site->rejection_reason = DSL_OPT_REJECT_INCOMPLETE_ANALYSIS;
    } else if (growth_bytes > analysis->control.resource_limit_bytes) {
        site->resource_state = DSL_FUSION_FACT_REJECTED;
        site->legality = DSL_OPT_LEGALITY_REJECTED;
        site->rejection_reason = DSL_OPT_REJECT_RESOURCE;
    } else {
        site->resource_state = DSL_FUSION_FACT_PROVEN;
        site->legality = DSL_OPT_LEGALITY_PROVEN;
        site->rejection_reason = DSL_OPT_REJECT_NONE;
    }
}

static void
DSL_Fusion_Cost_Term
        (DSL_OPT_COST_TERM *term, UINT64 amount, UINT32 confidence,
         UINT32 evidence)
{
    memset(term, 0, sizeof(*term));
    term->amount = amount;
    term->unit = DSL_OPT_COST_UNIT_RELATIVE;
    term->confidence = confidence;
    term->evidence = evidence;
}

static void
DSL_Fusion_Unknown_Cost_Term (DSL_OPT_COST_TERM *term)
{
    memset(term, 0, sizeof(*term));
}

static BOOL
DSL_Fusion_Build_Plan
        (DSL_FUSION_CANDIDATE_ANALYSIS *analysis,
         DSL_FUSION_SITE_RECORD *site, FILE *diagnostic)
{
    DSL_TENSOR_EVOLUTION_NODE_RECORD root;
    DSL_OPT_PLAN_BUDGET budget;
    DSL_OPT_CANDIDATE_INPUT candidate;
    DSL_OPT_COST_INPUT cost;
    DSL_OPT_PLAN_INPUT plan;
    DSL_OPT_COST_ID baseline_cost_id;
    DSL_OPT_COST_ID fusion_cost_id;
    DSL_OPT_CANDIDATE_ID member_id;
    DSL_OPT_SELECTION_RESULT selection;

    if (!DSL_tensor_evolution_find_semantic_root
             (analysis->graph, site->result_value_id, &root))
        return DSL_Fusion_Report
                   (diagnostic, "missing result evolution root", site->id);
    budget.max_candidates = 2;
    budget.max_plans = 2;
    DSL_OPT_PLAN_CONTEXT *context = DSL_opt_plan_create
                                        (analysis->pu, analysis->graph,
                                         &budget, diagnostic);
    if (context == NULL)
        return FALSE;

    memset(&candidate, 0, sizeof(candidate));
    candidate.kind = DSL_OPT_CANDIDATE_BASELINE;
    candidate.semantic_node_id = site->root_node_id;
    candidate.source_evolution_node_id = root.id;
    candidate.result_evolution_node_id = root.id;
    candidate.legality = DSL_OPT_LEGALITY_PROVEN;
    candidate.rejection_reason = DSL_OPT_REJECT_NONE;
    candidate.ordering_key = 1;
    candidate.flags = DSL_OPT_CANDIDATE_FLAG_BASELINE;
    if (!DSL_opt_plan_add_candidate
             (context, &candidate, &site->baseline_candidate_id,
              diagnostic)) {
        DSL_opt_plan_destroy(context);
        return FALSE;
    }

    memset(&cost, 0, sizeof(cost));
    cost.target_profile_id = analysis->control.target_profile_id;
    cost.ordering_key = 1;
    DSL_Fusion_Cost_Term
        (&cost.terms[DSL_OPT_COST_COMPUTE], site->member_count * 100,
         DSL_OPT_COST_CONFIDENCE_MEDIUM,
         DSL_OPT_COST_EVIDENCE_BASELINE_POLICY);
    DSL_Fusion_Cost_Term
        (&cost.terms[DSL_OPT_COST_MEMORY_UNHIDDEN], site->member_count * 50,
         DSL_OPT_COST_CONFIDENCE_LOW,
         DSL_OPT_COST_EVIDENCE_BASELINE_POLICY);
    DSL_Fusion_Cost_Term
        (&cost.terms[DSL_OPT_COST_COMMUNICATION_UNHIDDEN], 0,
         DSL_OPT_COST_CONFIDENCE_MEDIUM,
         DSL_OPT_COST_EVIDENCE_BASELINE_POLICY);
    DSL_Fusion_Cost_Term
        (&cost.terms[DSL_OPT_COST_SYNC], 0,
         DSL_OPT_COST_CONFIDENCE_MEDIUM,
         DSL_OPT_COST_EVIDENCE_BASELINE_POLICY);
    DSL_Fusion_Cost_Term
        (&cost.terms[DSL_OPT_COST_LAUNCH], site->member_count * 25,
         DSL_OPT_COST_CONFIDENCE_LOW,
         DSL_OPT_COST_EVIDENCE_BASELINE_POLICY);
    DSL_Fusion_Cost_Term
        (&cost.terms[DSL_OPT_COST_RUNTIME_SELECTION], 0,
         DSL_OPT_COST_CONFIDENCE_MEDIUM,
         DSL_OPT_COST_EVIDENCE_BASELINE_POLICY);
    if (!DSL_opt_plan_add_cost
             (context, &cost, &baseline_cost_id, diagnostic)) {
        DSL_opt_plan_destroy(context);
        return FALSE;
    }

    member_id = site->baseline_candidate_id;
    memset(&plan, 0, sizeof(plan));
    plan.candidate_ids = &member_id;
    plan.candidate_count = 1;
    plan.cost_id = baseline_cost_id;
    plan.legality = DSL_OPT_LEGALITY_PROVEN;
    plan.rejection_reason = DSL_OPT_REJECT_NONE;
    plan.ordering_key = 1;
    plan.flags = DSL_OPT_PLAN_FLAG_BASELINE |
                 DSL_OPT_PLAN_FLAG_ANALYSIS_ONLY;
    if (!DSL_opt_plan_add_plan
             (context, &plan, &site->baseline_plan_id, diagnostic)) {
        DSL_opt_plan_destroy(context);
        return FALSE;
    }

    memset(&candidate, 0, sizeof(candidate));
    candidate.kind = DSL_OPT_CANDIDATE_FUSION;
    candidate.semantic_node_id = site->root_node_id;
    candidate.source_evolution_node_id = root.id;
    candidate.result_evolution_node_id = root.id;
    candidate.parent_candidate_id = site->baseline_candidate_id;
    candidate.legality = site->legality;
    candidate.rejection_reason = site->rejection_reason;
    candidate.ordering_key = 2;
    candidate.flags = DSL_OPT_CANDIDATE_FLAG_PROVISIONAL;
    if (!DSL_opt_plan_add_candidate
             (context, &candidate, &site->fusion_candidate_id,
              diagnostic)) {
        DSL_opt_plan_destroy(context);
        return FALSE;
    }

    memset(&cost, 0, sizeof(cost));
    cost.target_profile_id = analysis->control.target_profile_id;
    cost.ordering_key = 2;
    DSL_Fusion_Cost_Term
        (&cost.terms[DSL_OPT_COST_COMPUTE], site->member_count * 100,
         DSL_OPT_COST_CONFIDENCE_MEDIUM,
         DSL_OPT_COST_EVIDENCE_STATIC_ANALYSIS);
    if (site->legality == DSL_OPT_LEGALITY_PROVEN)
        DSL_Fusion_Cost_Term
            (&cost.terms[DSL_OPT_COST_MEMORY_UNHIDDEN], 0,
             DSL_OPT_COST_CONFIDENCE_MEDIUM,
             DSL_OPT_COST_EVIDENCE_STATIC_ANALYSIS);
    else
        DSL_Fusion_Unknown_Cost_Term
            (&cost.terms[DSL_OPT_COST_MEMORY_UNHIDDEN]);
    DSL_Fusion_Cost_Term
        (&cost.terms[DSL_OPT_COST_COMMUNICATION_UNHIDDEN], 0,
         DSL_OPT_COST_CONFIDENCE_MEDIUM,
         DSL_OPT_COST_EVIDENCE_STATIC_ANALYSIS);
    DSL_Fusion_Cost_Term
        (&cost.terms[DSL_OPT_COST_SYNC],
         site->legality == DSL_OPT_LEGALITY_PROVEN ?
             site->live_range_growth_statements : 0,
         DSL_OPT_COST_CONFIDENCE_LOW,
         DSL_OPT_COST_EVIDENCE_STATIC_ANALYSIS);
    DSL_Fusion_Cost_Term
        (&cost.terms[DSL_OPT_COST_LAUNCH], 25,
         DSL_OPT_COST_CONFIDENCE_LOW,
         DSL_OPT_COST_EVIDENCE_STATIC_ANALYSIS);
    DSL_Fusion_Cost_Term
        (&cost.terms[DSL_OPT_COST_RUNTIME_SELECTION], 0,
         DSL_OPT_COST_CONFIDENCE_MEDIUM,
         DSL_OPT_COST_EVIDENCE_STATIC_ANALYSIS);
    if (!DSL_opt_plan_add_cost
             (context, &cost, &fusion_cost_id, diagnostic)) {
        DSL_opt_plan_destroy(context);
        return FALSE;
    }

    member_id = site->fusion_candidate_id;
    memset(&plan, 0, sizeof(plan));
    plan.candidate_ids = &member_id;
    plan.candidate_count = 1;
    plan.cost_id = fusion_cost_id;
    plan.fallback_plan_id = site->baseline_plan_id;
    plan.legality = site->legality;
    plan.rejection_reason = site->rejection_reason;
    plan.ordering_key = 2;
    plan.flags = DSL_OPT_PLAN_FLAG_ANALYSIS_ONLY;
    if (!DSL_opt_plan_add_plan
             (context, &plan, &site->fusion_plan_id, diagnostic) ||
        !DSL_opt_plan_verify(context, diagnostic)) {
        DSL_opt_plan_destroy(context);
        return FALSE;
    }
    if (analysis->control.select_plans) {
        if (!DSL_opt_plan_select
                 (context, analysis->control.target_profile_id,
                  &selection, diagnostic)) {
            DSL_opt_plan_destroy(context);
            return FALSE;
        }
        site->selected_plan_id = selection.selected_plan_id;
    }
    analysis->plans.push_back(context);
    return TRUE;
}

DSL_FUSION_CANDIDATE_ANALYSIS *
DSL_fusion_candidates_create
        (PU_Info *pu, const DSL_TENSOR_EVOLUTION_GRAPH *graph,
         const DSL_TENSOR_ANALYSIS *tensor_analysis,
         const DSL_TENSOR_LOCALITY_ANALYSIS *locality,
         const DSL_FUSION_CONTROL *control, FILE *diagnostic)
{
    return DSL_fusion_candidates_create_with_layout
               (pu, graph, tensor_analysis, locality, NULL, control,
                diagnostic);
}

DSL_FUSION_CANDIDATE_ANALYSIS *
DSL_fusion_candidates_create_with_layout
        (PU_Info *pu, const DSL_TENSOR_EVOLUTION_GRAPH *graph,
         const DSL_TENSOR_ANALYSIS *tensor_analysis,
         const DSL_TENSOR_LOCALITY_ANALYSIS *locality,
         const DSL_LOGICAL_LAYOUT_ANALYSIS *layout,
         const DSL_FUSION_CONTROL *control, FILE *diagnostic)
{
    if (pu == NULL || graph == NULL || tensor_analysis == NULL ||
        locality == NULL || control == NULL || Current_PU_Info != pu ||
        DSL_tensor_evolution_owner(graph) != PU_Info_proc_sym(pu) ||
        !DSL_Fusion_Control_Valid(*control) ||
        !DSL_tensor_evolution_verify(graph, diagnostic) ||
        !DSL_tensor_analysis_verify(tensor_analysis, diagnostic) ||
        !DSL_tensor_locality_verify(locality, diagnostic) ||
        (layout != NULL &&
         !DSL_logical_layout_verify(layout, diagnostic))) {
        DSL_Fusion_Report(diagnostic, "invalid active analysis", 0);
        return NULL;
    }
    DSL_FUSION_CANDIDATE_ANALYSIS *analysis =
        new DSL_FUSION_CANDIDATE_ANALYSIS;
    analysis->pu = pu;
    analysis->owner_pu_st = PU_Info_proc_sym(pu);
    analysis->graph = graph;
    analysis->tensor_analysis = tensor_analysis;
    analysis->locality = locality;
    analysis->layout = layout;
    analysis->control = *control;
    analysis->built = FALSE;
    return analysis;
}

void
DSL_fusion_candidates_destroy (DSL_FUSION_CANDIDATE_ANALYSIS *analysis)
{
    if (analysis == NULL)
        return;
    for (UINT32 i = 0; i < analysis->plans.size(); ++i)
        DSL_opt_plan_destroy(analysis->plans[i]);
    delete analysis;
}

BOOL
DSL_fusion_candidates_build
        (DSL_FUSION_CANDIDATE_ANALYSIS *analysis, FILE *diagnostic)
{
    if (!DSL_Fusion_Active(analysis) || analysis->built)
        return DSL_Fusion_Report(diagnostic, "analysis is not mutable", 0);
    if (!analysis->control.generate_candidates) {
        analysis->built = TRUE;
        return TRUE;
    }
    for (DSL_TENSOR_FACT_ID id = 1;
         id <= DSL_tensor_analysis_fact_count(analysis->tensor_analysis);
         ++id) {
        DSL_TENSOR_FACT_RECORD fact;
        DSL_FUSION_PATTERN_MATCH match;
        if (!DSL_tensor_analysis_get_fact
                 (analysis->tensor_analysis, id, &fact) ||
            fact.producer_node_id == DSL_IR_NODE_INVALID_ID)
            continue;
        BOOL matched = analysis->control.enable_semantic_patterns &&
                       DSL_Fusion_Match(fact.producer_node_id, &match);
        if (!matched && analysis->control.enable_generic_clusters)
            matched = DSL_Fusion_Match_Generic
                          (analysis, fact.producer_node_id, &match);
        if (!matched)
            continue;
        if (analysis->sites.size() >= analysis->control.max_sites)
            return DSL_Fusion_Report
                       (diagnostic, "fusion site budget exhausted", id);

        DSL_FUSION_SITE_RECORD site;
        memset(&site, 0, sizeof(site));
        site.id = analysis->sites.size() + 1;
        site.owner_pu_st = analysis->owner_pu_st;
        site.pattern = match.pattern;
        site.root_node_id = match.root_node_id;
        site.result_value_id = match.result_value_id;
        site.first_member_id = analysis->members.size() + 1;
        site.member_count = match.members.size();
        site.first_boundary_id = analysis->boundaries.size() + 1;
        site.boundary_count = match.boundaries.size();
        DSL_Fusion_Classify(analysis, match, &site);
        for (UINT32 member = 0; member < match.members.size(); ++member) {
            DSL_IR_NODE_RECORD node;
            DSL_FUSION_MEMBER_RECORD record;
            if (!DSL_IR_Image_Get_Node(match.members[member], &node))
                return DSL_Fusion_Report
                           (diagnostic, "missing fusion member", site.id);
            memset(&record, 0, sizeof(record));
            record.id = analysis->members.size() + 1;
            record.site_id = site.id;
            record.node_id = node.id;
            record.result_value_id = node.result_value_id;
            record.role = match.member_roles[member];
            record.ordinal = member;
            analysis->members.push_back(record);
        }
        for (UINT32 boundary = 0;
             boundary < match.boundaries.size(); ++boundary) {
            DSL_FUSION_BOUNDARY_RECORD record = match.boundaries[boundary];
            record.id = analysis->boundaries.size() + 1;
            record.site_id = site.id;
            analysis->boundaries.push_back(record);
        }
        if (!DSL_Fusion_Build_Plan(analysis, &site, diagnostic))
            return FALSE;
        analysis->sites.push_back(site);
    }
    analysis->built = TRUE;
    return DSL_fusion_candidates_verify(analysis, diagnostic);
}

BOOL
DSL_fusion_candidates_verify
        (const DSL_FUSION_CANDIDATE_ANALYSIS *analysis, FILE *diagnostic)
{
    if (!DSL_Fusion_Active(analysis) || !analysis->built ||
        !DSL_Fusion_Control_Valid(analysis->control) ||
        analysis->sites.size() > analysis->control.max_sites ||
        analysis->sites.size() != analysis->plans.size())
        return DSL_Fusion_Report(diagnostic, "invalid analysis", 0);
    UINT32 expected_member = 1;
    UINT32 expected_boundary = 1;
    for (UINT32 i = 0; i < analysis->sites.size(); ++i) {
        const DSL_FUSION_SITE_RECORD &site = analysis->sites[i];
        UINT32 expected_member_count = site.member_count;
        UINT32 expected_boundary_count = site.boundary_count;
        if (site.pattern == DSL_FUSION_PATTERN_MATMUL_BIAS_ACTIVATION) {
            expected_member_count = 3;
            expected_boundary_count = 6;
        } else if (site.pattern == DSL_FUSION_PATTERN_RESIDUAL_ACTIVATION) {
            expected_member_count = 2;
            expected_boundary_count = 4;
        }
        UINT32 expected_materialization_count = expected_member_count - 1;
        if (site.id != i + 1 || site.owner_pu_st != analysis->owner_pu_st ||
            site.pattern < DSL_FUSION_PATTERN_MATMUL_BIAS_ACTIVATION ||
            site.pattern > DSL_FUSION_PATTERN_GENERIC_CLUSTER ||
            (site.pattern == DSL_FUSION_PATTERN_GENERIC_CLUSTER &&
             (!analysis->control.enable_generic_clusters ||
              site.member_count >
                  analysis->control.max_cluster_members)) ||
            (site.pattern != DSL_FUSION_PATTERN_GENERIC_CLUSTER &&
             !analysis->control.enable_semantic_patterns) ||
            site.first_member_id != expected_member ||
            expected_member_count < 2 ||
            site.member_count != expected_member_count ||
            site.first_boundary_id != expected_boundary ||
            site.boundary_count != expected_boundary_count ||
            site.eliminated_materialization_count !=
                expected_materialization_count ||
            site.semantic_state > DSL_FUSION_FACT_REJECTED ||
            site.descriptor_state > DSL_FUSION_FACT_REJECTED ||
            site.effect_state > DSL_FUSION_FACT_REJECTED ||
            site.resource_state > DSL_FUSION_FACT_REJECTED ||
            site.layout_state > DSL_FUSION_FACT_REJECTED ||
            site.baseline_candidate_id == 0 ||
            site.fusion_candidate_id == 0 ||
            site.baseline_plan_id == 0 || site.fusion_plan_id == 0 ||
            (analysis->control.select_plans &&
             site.selected_plan_id == 0) ||
            (!analysis->control.select_plans &&
             site.selected_plan_id != 0) ||
            !DSL_opt_plan_verify(analysis->plans[i], diagnostic))
            return DSL_Fusion_Report
                       (diagnostic, "invalid fusion site", site.id);
        if ((site.legality == DSL_OPT_LEGALITY_PROVEN &&
             (site.rejection_reason != DSL_OPT_REJECT_NONE ||
              site.semantic_state != DSL_FUSION_FACT_PROVEN ||
              site.descriptor_state != DSL_FUSION_FACT_PROVEN ||
              site.effect_state != DSL_FUSION_FACT_PROVEN ||
              site.resource_state != DSL_FUSION_FACT_PROVEN ||
              site.eliminated_materialization_bytes ==
                  DSL_FUSION_UNKNOWN_U64 ||
              site.live_range_growth_bytes == DSL_FUSION_UNKNOWN_U64 ||
              (analysis->control.select_plans &&
               site.selected_plan_id != site.fusion_plan_id))) ||
            (site.legality == DSL_OPT_LEGALITY_UNKNOWN &&
             (site.rejection_reason !=
                  DSL_OPT_REJECT_INCOMPLETE_ANALYSIS ||
              (analysis->control.select_plans &&
               site.selected_plan_id != site.baseline_plan_id))) ||
            (site.legality == DSL_OPT_LEGALITY_REJECTED &&
             (((site.rejection_reason < DSL_OPT_REJECT_OWNERSHIP ||
                site.rejection_reason > DSL_OPT_REJECT_RESOURCE) &&
               site.rejection_reason != DSL_OPT_REJECT_MALFORMED) ||
              (analysis->control.select_plans &&
               site.selected_plan_id != site.baseline_plan_id))))
            return DSL_Fusion_Report
                       (diagnostic, "inconsistent fusion legality", site.id);
        for (UINT32 j = 0; j < site.member_count; ++j) {
            const DSL_FUSION_MEMBER_RECORD &member =
                analysis->members[expected_member - 1];
            if (member.id != expected_member || member.site_id != site.id ||
                member.ordinal != j || member.node_id == 0 ||
                member.result_value_id == 0 ||
                member.role < DSL_FUSION_MEMBER_MATMUL ||
                member.role > DSL_FUSION_MEMBER_GENERIC_POINTWISE ||
                (site.pattern == DSL_FUSION_PATTERN_GENERIC_CLUSTER &&
                 member.role < DSL_FUSION_MEMBER_GENERIC_CONTRACTION) ||
                (site.pattern != DSL_FUSION_PATTERN_GENERIC_CLUSTER &&
                 member.role > DSL_FUSION_MEMBER_ACTIVATION))
                return DSL_Fusion_Report
                           (diagnostic, "invalid fusion member", member.id);
            ++expected_member;
        }
        const DSL_FUSION_MEMBER_RECORD &root_member =
            analysis->members[site.first_member_id + site.member_count - 2];
        if (root_member.node_id != site.root_node_id ||
            root_member.result_value_id != site.result_value_id ||
            (site.pattern != DSL_FUSION_PATTERN_GENERIC_CLUSTER &&
             root_member.role != DSL_FUSION_MEMBER_ACTIVATION))
            return DSL_Fusion_Report
                       (diagnostic, "invalid fusion root member", site.id);
        UINT32 cut_count = 0;
        UINT32 output_count = 0;
        for (UINT32 j = 0; j < site.boundary_count; ++j) {
            const DSL_FUSION_BOUNDARY_RECORD &boundary =
                analysis->boundaries[expected_boundary - 1];
            if (boundary.id != expected_boundary ||
                boundary.site_id != site.id || boundary.value_id == 0 ||
                boundary.kind < DSL_FUSION_BOUNDARY_INPUT ||
                boundary.kind > DSL_FUSION_BOUNDARY_ALTERNATIVE_CUT ||
                ((boundary.kind == DSL_FUSION_BOUNDARY_OUTPUT) !=
                 (boundary.operand_ordinal ==
                  DSL_FUSION_BOUNDARY_NO_OPERAND)))
                return DSL_Fusion_Report
                           (diagnostic, "invalid fusion boundary",
                            boundary.id);
            if (boundary.kind == DSL_FUSION_BOUNDARY_ALTERNATIVE_CUT)
                ++cut_count;
            else if (boundary.kind == DSL_FUSION_BOUNDARY_OUTPUT)
                ++output_count;
            ++expected_boundary;
        }
        if (cut_count != site.member_count - 1 || output_count != 1)
            return DSL_Fusion_Report
                       (diagnostic, "invalid fusion cluster boundary", site.id);
    }
    if (expected_member != analysis->members.size() + 1 ||
        expected_boundary != analysis->boundaries.size() + 1)
        return DSL_Fusion_Report(diagnostic, "orphan fusion record", 0);
    return TRUE;
}

static void
DSL_Fusion_Print_U64 (FILE *file, UINT64 value)
{
    if (value == DSL_FUSION_UNKNOWN_U64)
        fprintf(file, "<unknown>");
    else
        fprintf(file, "%llu", (unsigned long long)value);
}

void
DSL_fusion_candidates_print
        (FILE *file, const DSL_FUSION_CANDIDATE_ANALYSIS *analysis)
{
    if (file == NULL || analysis == NULL)
        return;
    fprintf(file,
            "DSLFusionCandidates: owner=<%u,%u> sites=%u "
            "generate=%s semantic=%s generic=%s select=%s apply=%s "
            "target=%u resource_limit=",
            ST_IDX_level(analysis->owner_pu_st),
            ST_IDX_index(analysis->owner_pu_st),
            (UINT32)analysis->sites.size(),
            analysis->control.generate_candidates ? "yes" : "no",
            analysis->control.enable_semantic_patterns ? "yes" : "no",
            analysis->control.enable_generic_clusters ? "yes" : "no",
            analysis->control.select_plans ? "yes" : "no",
            analysis->control.apply_transformation ? "yes" : "no",
            analysis->control.target_profile_id);
    DSL_Fusion_Print_U64(file, analysis->control.resource_limit_bytes == 0 ?
                               DSL_FUSION_UNKNOWN_U64 :
                               analysis->control.resource_limit_bytes);
    fprintf(file, "\n");
    for (UINT32 i = 0; i < analysis->sites.size(); ++i) {
        const DSL_FUSION_SITE_RECORD &site = analysis->sites[i];
        fprintf(file,
                "  site %u pattern=%s root=%u result=%u members=%u "
                "boundaries=%u legality=%s reason=%s semantic=%s "
                "descriptor=%s effect=%s resource=%s layout=%s "
                "materializations=%u bytes=",
                site.id, DSL_fusion_pattern_name(site.pattern),
                site.root_node_id, site.result_value_id,
                site.member_count, site.boundary_count,
                DSL_opt_legality_name(site.legality),
                DSL_opt_rejection_reason_name(site.rejection_reason),
                DSL_fusion_fact_state_name(site.semantic_state),
                DSL_fusion_fact_state_name(site.descriptor_state),
                DSL_fusion_fact_state_name(site.effect_state),
                DSL_fusion_fact_state_name(site.resource_state),
                DSL_fusion_fact_state_name(site.layout_state),
                site.eliminated_materialization_count);
        DSL_Fusion_Print_U64(file, site.eliminated_materialization_bytes);
        fprintf(file, " live_growth_bytes=");
        DSL_Fusion_Print_U64(file, site.live_range_growth_bytes);
        fprintf(file,
                " live_growth_statements=%llu rep=0x%x "
                "plans=%u/%u selected=%u\n",
                (unsigned long long)site.live_range_growth_statements,
                site.representation_constraints, site.baseline_plan_id,
                site.fusion_plan_id, site.selected_plan_id);
        for (UINT32 j = 0; j < site.member_count; ++j) {
            const DSL_FUSION_MEMBER_RECORD &member =
                analysis->members[site.first_member_id - 1 + j];
            fprintf(file,
                    "    member %u ordinal=%u role=%s node=%u value=%u\n",
                    member.id, member.ordinal,
                    DSL_fusion_member_role_name(member.role),
                    member.node_id, member.result_value_id);
        }
        for (UINT32 j = 0; j < site.boundary_count; ++j) {
            const DSL_FUSION_BOUNDARY_RECORD &boundary =
                analysis->boundaries[site.first_boundary_id - 1 + j];
            fprintf(file,
                    "    boundary %u kind=%s value=%u producer=%u "
                    "consumer=%u operand=",
                    boundary.id,
                    DSL_fusion_boundary_kind_name(boundary.kind),
                    boundary.value_id, boundary.producer_node_id,
                    boundary.consumer_node_id);
            if (boundary.operand_ordinal ==
                DSL_FUSION_BOUNDARY_NO_OPERAND)
                fprintf(file, "<none>\n");
            else
                fprintf(file, "%u\n", boundary.operand_ordinal);
        }
        DSL_opt_plan_print(file, analysis->plans[i]);
    }
}

UINT32
DSL_fusion_candidates_site_count
        (const DSL_FUSION_CANDIDATE_ANALYSIS *analysis)
{
    return analysis == NULL ? 0 : analysis->sites.size();
}

UINT32
DSL_fusion_candidates_member_count
        (const DSL_FUSION_CANDIDATE_ANALYSIS *analysis)
{
    return analysis == NULL ? 0 : analysis->members.size();
}

UINT32
DSL_fusion_candidates_boundary_count
        (const DSL_FUSION_CANDIDATE_ANALYSIS *analysis)
{
    return analysis == NULL ? 0 : analysis->boundaries.size();
}

BOOL
DSL_fusion_candidates_get_site
        (const DSL_FUSION_CANDIDATE_ANALYSIS *analysis,
         DSL_FUSION_SITE_ID id, DSL_FUSION_SITE_RECORD *record)
{
    if (analysis == NULL || record == NULL || id == 0 ||
        id > analysis->sites.size())
        return FALSE;
    *record = analysis->sites[id - 1];
    return TRUE;
}

BOOL
DSL_fusion_candidates_get_member
        (const DSL_FUSION_CANDIDATE_ANALYSIS *analysis,
         DSL_FUSION_MEMBER_ID id, DSL_FUSION_MEMBER_RECORD *record)
{
    if (analysis == NULL || record == NULL || id == 0 ||
        id > analysis->members.size())
        return FALSE;
    *record = analysis->members[id - 1];
    return TRUE;
}

BOOL
DSL_fusion_candidates_get_boundary
        (const DSL_FUSION_CANDIDATE_ANALYSIS *analysis,
         DSL_FUSION_BOUNDARY_ID id, DSL_FUSION_BOUNDARY_RECORD *record)
{
    if (analysis == NULL || record == NULL || id == 0 ||
        id > analysis->boundaries.size())
        return FALSE;
    *record = analysis->boundaries[id - 1];
    return TRUE;
}

const DSL_OPT_PLAN_CONTEXT *
DSL_fusion_candidates_get_plan_context
        (const DSL_FUSION_CANDIDATE_ANALYSIS *analysis,
         DSL_FUSION_SITE_ID id)
{
    return analysis == NULL || id == 0 || id > analysis->plans.size() ?
           NULL : analysis->plans[id - 1];
}
