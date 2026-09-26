/*
 * Copyright (C) 2026 Open64 Project
 */

/*
 * Derives AIO-3 PU-local semantic tensor facts from canonical descriptors and
 * logical DSL operator contracts. Source metadata is deliberately excluded
 * from semantic equivalence. Design:
 * doc/AI-COMPILER-OPTIMIZATION-AIO3-SEMANTIC-TENSOR.md.
 */

#include <string.h>
#include <vector>

#include "dsl_tensor_analysis.h"
#include "dsl_memory_behavior.h"
#include "dsl_shape.h"
#include "pu_info.h"
#include "strtab.h"

struct DSL_TENSOR_ANALYSIS {
    PU_Info *pu;
    ST_IDX owner_pu_st;
    const DSL_TENSOR_EVOLUTION_GRAPH *graph;
    std::vector<DSL_TENSOR_FACT_RECORD> facts;
    std::vector<DSL_TENSOR_USE_FACT_RECORD> uses;
    UINT32 incomplete_fact_count;
};

static const char *DSL_tensor_dimension_state_name[] = {
    "unknown", "static", "unresolved"
};

static const char *DSL_tensor_ownership_name[] = {
    "unknown", "unique", "shared"
};

static const char *DSL_tensor_reuse_role_name[] = {
    "unknown", "unused", "single_use", "multiple_use"
};

static const char *DSL_tensor_value_role_name[] = {
    "unknown", "constant", "model_input", "formal", "intermediate",
    "symbol"
};

static const char *DSL_tensor_use_role_name[] = {
    "unknown", "generic", "contraction_kid0", "contraction_kid1",
    "activation", "weight", "bias", "scale", "mean", "variance",
    "query", "key", "value", "index", "view_source",
    "reduction_source", "elementwise_input"
};

static const char *DSL_tensor_shape_rule_name[] = {
    "opaque", "identity", "broadcast", "contraction", "reduction",
    "view", "layout", "runtime_guarded"
};

static const char *
DSL_Tensor_Analysis_Shape_Rule_Name (UINT32 rule)
{
    return rule < sizeof(DSL_tensor_shape_rule_name) /
                      sizeof(DSL_tensor_shape_rule_name[0]) ?
           DSL_tensor_shape_rule_name[rule] : "unknown";
}

static const char *
DSL_Tensor_Analysis_Logical_Name
        (DSL_IR_NODE_ID node_id, DSL_OPERATOR dsl_operator)
{
    DSL_IR_NODE_RECORD node;
    DSL_IR_OPCODE_DESCRIPTOR_RECORD descriptor;
    if (node_id != DSL_IR_NODE_INVALID_ID &&
        DSL_IR_Image_Get_Node(node_id, &node) &&
        DSL_IR_Image_Get_Opcode_Descriptor
            (node.opcode_descriptor_id, &descriptor) &&
        descriptor.stable_name != STR_IDX_ZERO)
        return Index_To_Str(descriptor.stable_name);
    return DSL_OPERATOR_name(dsl_operator);
}

static BOOL
DSL_Tensor_Analysis_Report
        (FILE *diagnostic, const char *message, UINT32 id)
{
    if (diagnostic != NULL)
        fprintf(diagnostic, "DSL tensor analysis error: %s id=%u\n",
                message, id);
    return FALSE;
}

static void
DSL_Tensor_Analysis_Report_Incomplete
        (FILE *diagnostic, DSL_IR_VALUE_ID value_id, UINT32 completeness)
{
    if (diagnostic != NULL)
        fprintf(diagnostic,
                "DSL tensor analysis incomplete: value=%u missing=0x%x\n",
                value_id, DSL_TENSOR_FACT_COMPLETE_ALL & ~completeness);
}

static BOOL
DSL_Tensor_Analysis_Owner_Valid (ST_IDX owner_pu_st)
{
    return ST_IDX_level(owner_pu_st) == GLOBAL_SYMTAB &&
           ST_IDX_index(owner_pu_st) != 0 &&
           ST_IDX_index(owner_pu_st) < ST_Table_Size(GLOBAL_SYMTAB) &&
           ST_class(owner_pu_st) == CLASS_FUNC &&
           ST_pu(St_Table[owner_pu_st]) != PU_IDX_ZERO &&
           ST_pu(St_Table[owner_pu_st]) < PU_Table_Size();
}

static BOOL
DSL_Tensor_Analysis_Active (const DSL_TENSOR_ANALYSIS *analysis)
{
    return analysis != NULL && analysis->pu != NULL &&
           Current_PU_Info == analysis->pu &&
           DSL_Tensor_Analysis_Owner_Valid(analysis->owner_pu_st) &&
           PU_Info_proc_sym(analysis->pu) == analysis->owner_pu_st &&
           Current_pu ==
               &Pu_Table[ST_pu(St_Table[analysis->owner_pu_st])] &&
           DSL_Tensor_Evolution_Owner(analysis->graph) ==
               analysis->owner_pu_st;
}

static BOOL
DSL_Tensor_Analysis_Value_Owned
        (const DSL_TENSOR_ANALYSIS *analysis,
         const DSL_IR_VALUE_RECORD &value)
{
    DSL_IR_VALUE_RECORD resolved;

    if (!DSL_Tensor_Analysis_Active(analysis) ||
        value.id == DSL_IR_VALUE_INVALID_ID ||
        value.name == STR_IDX_ZERO || ST_IDX_index(value.st) == 0 ||
        ST_IDX_level(value.st) != CURRENT_SYMTAB ||
        ST_IDX_index(value.st) >= ST_Table_Size(CURRENT_SYMTAB) ||
        value.ty != ST_type(St_Table[value.st]))
        return FALSE;

    return DSL_IR_Image_Find_PU_Value
               (value.st, Index_To_Str(value.name),
                ST_name(St_Table[analysis->owner_pu_st]), &resolved) &&
           resolved.id == value.id;
}

static BOOL
DSL_Tensor_Analysis_Node_Info
        (DSL_IR_NODE_ID node_id,
         DSL_IR_NODE_RECORD *node,
         DSL_IR_OPCODE_DESCRIPTOR_RECORD *descriptor)
{
    DSL_OPERATOR_INFO info;
    if (node == NULL || descriptor == NULL ||
        !DSL_IR_Image_Get_Node(node_id, node) ||
        (node->flags & DSL_IR_NODE_FLAG_RETIRED) != 0 ||
        !DSL_IR_Image_Get_Opcode_Descriptor
             (node->opcode_descriptor_id, descriptor) ||
        descriptor->logical_operator == OPR_DSLUNKNOWN ||
        descriptor->version == 0 ||
        descriptor->version > ~(UINT16)0)
        return FALSE;
    return DSL_Operator_Get_Info_Version
               ((DSL_OPERATOR)descriptor->logical_operator,
                (UINT16)descriptor->version, &info);
}

static BOOL
DSL_Tensor_Analysis_Is_Formal
        (ST_IDX owner_pu_st, DSL_IR_VALUE_ID value_id)
{
    for (DSL_PU_FORMAL_ID id = 1;
         id <= DSL_PU_Interface_Image_Formal_Count(); ++id) {
        DSL_PU_FORMAL_RECORD formal;
        if (DSL_PU_Interface_Image_Get_Formal(id, &formal) &&
            formal.owner_pu_st == owner_pu_st &&
            formal.formal_value_id == value_id)
            return TRUE;
    }
    return FALSE;
}

static UINT32
DSL_Tensor_Analysis_Value_Role
        (const DSL_TENSOR_ANALYSIS *analysis,
         const DSL_IR_VALUE_RECORD &value,
         UINT32 producer_operator)
{
    if (value.value_kind == DSL_IR_VALUE_CONSTANT)
        return DSL_TENSOR_VALUE_ROLE_CONSTANT;
    if (producer_operator == OPR_DSLMODELINPUT)
        return DSL_TENSOR_VALUE_ROLE_MODEL_INPUT;
    if (DSL_Tensor_Analysis_Is_Formal(analysis->owner_pu_st, value.id))
        return DSL_TENSOR_VALUE_ROLE_FORMAL;
    if (value.value_kind == DSL_IR_VALUE_OPERATOR_RESULT)
        return DSL_TENSOR_VALUE_ROLE_INTERMEDIATE;
    if (value.value_kind == DSL_IR_VALUE_SYMBOL)
        return DSL_TENSOR_VALUE_ROLE_SYMBOL;
    return DSL_TENSOR_VALUE_ROLE_UNKNOWN;
}

static UINT32
DSL_Tensor_Analysis_Ownership
        (const DSL_IR_VALUE_RECORD &value,
         UINT32 producer_operator,
         UINT16 producer_version)
{
    DSL_MEMORY_BEHAVIOR_CONTRACT contract;
    BOOL unique_symbol = DSL_Tensor_Has_Unique_Ownership(value.st);

    if (producer_operator != OPR_DSLUNKNOWN &&
        DSL_Memory_Behavior_Get_Contract
            ((DSL_OPERATOR)producer_operator, producer_version, &contract)) {
        if ((contract.result_flags &
             DSL_MEMORY_BEHAVIOR_UNIQUE_OWNERSHIP) != 0)
            return unique_symbol ? DSL_TENSOR_OWNERSHIP_UNIQUE :
                                   DSL_TENSOR_OWNERSHIP_UNKNOWN;
        if ((contract.result_flags &
             DSL_MEMORY_BEHAVIOR_SHARED_OWNERSHIP) != 0)
            return unique_symbol ? DSL_TENSOR_OWNERSHIP_UNKNOWN :
                                   DSL_TENSOR_OWNERSHIP_SHARED;
    }
    return unique_symbol ? DSL_TENSOR_OWNERSHIP_UNIQUE :
                           DSL_TENSOR_OWNERSHIP_UNKNOWN;
}

static UINT32
DSL_Tensor_Analysis_Use_Role
        (DSL_OPERATOR dsl_operator, UINT16 version, UINT32 ordinal,
         UINT32 shape_rule)
{
    switch (dsl_operator) {
    case OPR_DSLMATMUL:
        return ordinal == 0 ? DSL_TENSOR_USE_ROLE_CONTRACTION_KID0 :
               ordinal == 1 ? DSL_TENSOR_USE_ROLE_CONTRACTION_KID1 :
                              DSL_TENSOR_USE_ROLE_UNKNOWN;
    case OPR_DSLLINEAR:
    case OPR_DSLCONV2D:
        return ordinal == 0 ? DSL_TENSOR_USE_ROLE_ACTIVATION :
               ordinal == 1 ? DSL_TENSOR_USE_ROLE_WEIGHT :
               ordinal == 2 ? DSL_TENSOR_USE_ROLE_BIAS :
                              DSL_TENSOR_USE_ROLE_UNKNOWN;
    case OPR_DSLBATCHNORMINFER:
        return ordinal == 0 ? DSL_TENSOR_USE_ROLE_ACTIVATION :
               ordinal == 1 ? DSL_TENSOR_USE_ROLE_SCALE :
               ordinal == 2 ? DSL_TENSOR_USE_ROLE_BIAS :
               ordinal == 3 ? DSL_TENSOR_USE_ROLE_MEAN :
               ordinal == 4 ? DSL_TENSOR_USE_ROLE_VARIANCE :
                              DSL_TENSOR_USE_ROLE_UNKNOWN;
    case OPR_DSLATTENTION:
        return ordinal == 0 ? DSL_TENSOR_USE_ROLE_QUERY :
               ordinal == 1 ? DSL_TENSOR_USE_ROLE_KEY :
               ordinal == 2 ? DSL_TENSOR_USE_ROLE_VALUE :
                              DSL_TENSOR_USE_ROLE_UNKNOWN;
    case OPR_DSLTOKENEMBEDDING:
        return ordinal == 0 ? DSL_TENSOR_USE_ROLE_INDEX :
               ordinal == 1 ? DSL_TENSOR_USE_ROLE_WEIGHT :
                              DSL_TENSOR_USE_ROLE_UNKNOWN;
    case OPR_DSLRMSNORM:
        return ordinal == 0 ? DSL_TENSOR_USE_ROLE_ACTIVATION :
               ordinal == 1 ? DSL_TENSOR_USE_ROLE_WEIGHT :
                              DSL_TENSOR_USE_ROLE_UNKNOWN;
    default:
        break;
    }

    if (shape_rule == DSL_SHAPE_RULE_CONTRACTION)
        return ordinal == 0 ? DSL_TENSOR_USE_ROLE_CONTRACTION_KID0 :
               ordinal == 1 ? DSL_TENSOR_USE_ROLE_CONTRACTION_KID1 :
                              DSL_TENSOR_USE_ROLE_GENERIC;
    if (shape_rule == DSL_SHAPE_RULE_VIEW ||
        shape_rule == DSL_SHAPE_RULE_LAYOUT)
        return ordinal == 0 ? DSL_TENSOR_USE_ROLE_VIEW_SOURCE :
                              DSL_TENSOR_USE_ROLE_GENERIC;
    if (shape_rule == DSL_SHAPE_RULE_REDUCTION)
        return ordinal == 0 ? DSL_TENSOR_USE_ROLE_REDUCTION_SOURCE :
                              DSL_TENSOR_USE_ROLE_GENERIC;
    if (shape_rule == DSL_SHAPE_RULE_BROADCAST)
        return DSL_TENSOR_USE_ROLE_ELEMENTWISE_INPUT;
    if (shape_rule == DSL_SHAPE_RULE_IDENTITY)
        return ordinal == 0 ? DSL_TENSOR_USE_ROLE_ACTIVATION :
                              DSL_TENSOR_USE_ROLE_GENERIC;
    (void)version;
    return DSL_TENSOR_USE_ROLE_GENERIC;
}

static UINT32
DSL_Tensor_Analysis_Operand_Memory
        (DSL_OPERATOR dsl_operator, UINT16 version, UINT32 ordinal,
         UINT32 effect_model)
{
    DSL_MEMORY_BEHAVIOR_CONTRACT contract;
    if (DSL_Memory_Behavior_Get_Contract
            (dsl_operator, version, &contract) &&
        ordinal < contract.operand_count)
        return contract.operand_flags[ordinal];
    return effect_model == DSL_EFFECT_MODEL_PURE ?
           DSL_MEMORY_BEHAVIOR_READ : DSL_MEMORY_BEHAVIOR_NONE;
}

static BOOL
DSL_Tensor_Analysis_Add_Uses
        (DSL_TENSOR_ANALYSIS *analysis,
         DSL_TENSOR_FACT_RECORD *fact,
         FILE *diagnostic)
{
    fact->first_use_id = analysis->uses.size() + 1;
    for (DSL_IR_VALUE_REFERENCE_ID id = 1;
         id <= DSL_IR_Image_Value_Reference_Count(); ++id) {
        DSL_IR_VALUE_REFERENCE_RECORD reference;
        DSL_IR_NODE_RECORD node;
        DSL_IR_OPCODE_DESCRIPTOR_RECORD descriptor;
        DSL_IR_VALUE_RECORD result;
        DSL_TENSOR_USE_FACT_RECORD use;

        if (!DSL_IR_Image_Get_Value_Reference(id, &reference))
            return DSL_Tensor_Analysis_Report
                       (diagnostic, "missing value reference", id);
        if (reference.value_id != fact->value_id)
            continue;
        if (!DSL_Tensor_Analysis_Node_Info
                 (reference.owner_node_id, &node, &descriptor) ||
            !DSL_IR_Image_Get_Value(node.result_value_id, &result))
            return DSL_Tensor_Analysis_Report
                       (diagnostic, "invalid consumer", id);
        if (!DSL_Tensor_Analysis_Value_Owned(analysis, result))
            continue;
        if (reference.ordinal >= node.operand_count)
            return DSL_Tensor_Analysis_Report
                       (diagnostic, "invalid operand ordinal", id);

        memset(&use, 0, sizeof(use));
        use.id = analysis->uses.size() + 1;
        use.tensor_fact_id = fact->id;
        use.consumer_node_id = node.id;
        use.consumer_operator = descriptor.logical_operator;
        use.consumer_version = descriptor.version;
        use.operand_ordinal = reference.ordinal;
        use.role = DSL_Tensor_Analysis_Use_Role
                       ((DSL_OPERATOR)descriptor.logical_operator,
                        descriptor.version, reference.ordinal,
                        descriptor.shape_rule);
        use.shape_rule = descriptor.shape_rule;
        use.memory_behavior = DSL_Tensor_Analysis_Operand_Memory
                                  ((DSL_OPERATOR)descriptor.logical_operator,
                                   descriptor.version, reference.ordinal,
                                   descriptor.effect_model);
        analysis->uses.push_back(use);
    }
    fact->use_count = analysis->uses.size() - fact->first_use_id + 1;
    if (fact->use_count == 0)
        fact->first_use_id = DSL_TENSOR_USE_FACT_INVALID_ID;
    fact->reuse_role = fact->use_count == 0 ? DSL_TENSOR_REUSE_UNUSED :
                       fact->use_count == 1 ? DSL_TENSOR_REUSE_SINGLE_USE :
                                              DSL_TENSOR_REUSE_MULTIPLE_USE;
    fact->completeness |= DSL_TENSOR_FACT_COMPLETE_CONSUMERS;
    return TRUE;
}

const char *
DSL_Tensor_Dimension_State_Name (UINT32 state)
{
    return state < sizeof(DSL_tensor_dimension_state_name) /
                       sizeof(DSL_tensor_dimension_state_name[0]) ?
           DSL_tensor_dimension_state_name[state] : "unknown";
}

const char *
DSL_Tensor_Ownership_Name (UINT32 ownership)
{
    return ownership < sizeof(DSL_tensor_ownership_name) /
                           sizeof(DSL_tensor_ownership_name[0]) ?
           DSL_tensor_ownership_name[ownership] : "unknown";
}

const char *
DSL_Tensor_Reuse_Role_Name (UINT32 role)
{
    return role < sizeof(DSL_tensor_reuse_role_name) /
                      sizeof(DSL_tensor_reuse_role_name[0]) ?
           DSL_tensor_reuse_role_name[role] : "unknown";
}

const char *
DSL_Tensor_Value_Role_Name (UINT32 role)
{
    return role < sizeof(DSL_tensor_value_role_name) /
                      sizeof(DSL_tensor_value_role_name[0]) ?
           DSL_tensor_value_role_name[role] : "unknown";
}

const char *
DSL_Tensor_Use_Role_Name (UINT32 role)
{
    return role < sizeof(DSL_tensor_use_role_name) /
                      sizeof(DSL_tensor_use_role_name[0]) ?
           DSL_tensor_use_role_name[role] : "unknown";
}

DSL_TENSOR_ANALYSIS *
DSL_Tensor_Analysis_Create
        (PU_Info *pu, const DSL_TENSOR_EVOLUTION_GRAPH *graph,
         FILE *diagnostic)
{
    if (pu == NULL || graph == NULL || Current_PU_Info != pu ||
        DSL_Tensor_Evolution_Owner(graph) != PU_Info_proc_sym(pu) ||
        !DSL_Tensor_Evolution_Verify(graph, diagnostic)) {
        DSL_Tensor_Analysis_Report
            (diagnostic, "invalid active program unit", 0);
        return NULL;
    }
    DSL_TENSOR_ANALYSIS *analysis = new DSL_TENSOR_ANALYSIS;
    analysis->pu = pu;
    analysis->owner_pu_st = PU_Info_proc_sym(pu);
    analysis->graph = graph;
    analysis->incomplete_fact_count = 0;
    return analysis;
}

void
DSL_Tensor_Analysis_Destroy (DSL_TENSOR_ANALYSIS *analysis)
{
    delete analysis;
}

BOOL
DSL_Tensor_Analysis_Find_Fact
        (const DSL_TENSOR_ANALYSIS *analysis, DSL_IR_VALUE_ID value_id,
         DSL_TENSOR_FACT_RECORD *record)
{
    if (analysis == NULL || value_id == DSL_IR_VALUE_INVALID_ID)
        return FALSE;
    for (UINT32 i = 0; i < analysis->facts.size(); ++i) {
        if (analysis->facts[i].value_id == value_id) {
            if (record != NULL)
                *record = analysis->facts[i];
            return TRUE;
        }
    }
    return FALSE;
}

BOOL
DSL_Tensor_Analysis_Build
        (DSL_TENSOR_ANALYSIS *analysis, FILE *diagnostic)
{
    if (!DSL_Tensor_Analysis_Active(analysis))
        return DSL_Tensor_Analysis_Report
                   (diagnostic, "program unit is not active", 0);
    if (!analysis->facts.empty())
        return DSL_Tensor_Analysis_Verify(analysis, diagnostic);

    /*
     * Facts come from canonical tensor descriptors and versioned logical
     * operator contracts. Source names and free-form metadata are not semantic
     * evidence and therefore do not participate in classification.
     */
    for (DSL_TENSOR_EVOLUTION_NODE_ID root_id = 1;
         root_id <= DSL_Tensor_Evolution_Node_Count(analysis->graph);
         ++root_id) {
        DSL_TENSOR_EVOLUTION_NODE_RECORD root;
        DSL_IR_VALUE_RECORD value;
        DSL_TENSOR_FACT_RECORD fact;
        DSL_IR_NODE_RECORD producer;
        DSL_IR_OPCODE_DESCRIPTOR_RECORD descriptor;
        const char *shape;
        UINT32 parsed_rank = 0;

        if (!DSL_Tensor_Evolution_Get_Node
                 (analysis->graph, root_id, &root))
            return DSL_Tensor_Analysis_Report
                       (diagnostic, "invalid evolution node", root_id);
        if (root.kind != DSL_TENSOR_EVOLUTION_NODE_SEMANTIC)
            continue;
        if (!DSL_IR_Image_Get_Value(root.semantic_value_id, &value) ||
            !DSL_Tensor_Analysis_Value_Owned(analysis, value))
            return DSL_Tensor_Analysis_Report
                       (diagnostic, "invalid semantic root", root_id);

        memset(&fact, 0, sizeof(fact));
        fact.id = analysis->facts.size() + 1;
        fact.semantic_root_id = root.id;
        fact.value_id = value.id;
        fact.descriptor_ty = value.ty;
        fact.element_ty = TY_tensor_element_ty(value.ty);
        fact.rank = TY_tensor_rank(value.ty);
        if (fact.element_ty != TY_IDX_ZERO && fact.rank >= 0 &&
            TY_tensor_is_canonical(value.ty) &&
            TY_tensor_attribute(value.ty, TY_TENSOR_SCHEMA_DTYPE) != NULL &&
            TY_tensor_attribute(value.ty, TY_TENSOR_SCHEMA_SHAPE) != NULL)
            fact.completeness |= DSL_TENSOR_FACT_COMPLETE_TYPE_CORE;

        shape = TY_tensor_attribute(value.ty, TY_TENSOR_SCHEMA_SHAPE);
        if (shape != NULL && fact.rank >= 0 &&
            DSL_Shape_Parse_Static_Dimensions
                (shape, NULL, 0, &parsed_rank) &&
            parsed_rank == (UINT32)fact.rank) {
            fact.dimension_state = DSL_TENSOR_DIMENSION_STATIC;
            fact.static_dimension_count = parsed_rank;
            fact.dynamic_dimension_count = 0;
            fact.completeness |= DSL_TENSOR_FACT_COMPLETE_SHAPE;
        } else {
            fact.dimension_state = DSL_TENSOR_DIMENSION_UNRESOLVED;
            fact.static_dimension_count =
                DSL_TENSOR_DIMENSION_COUNT_UNKNOWN;
            fact.dynamic_dimension_count =
                DSL_TENSOR_DIMENSION_COUNT_UNKNOWN;
        }

        if (value.producer_node_id != DSL_IR_NODE_INVALID_ID &&
            DSL_Tensor_Analysis_Node_Info
                (value.producer_node_id, &producer, &descriptor)) {
            fact.producer_node_id = producer.id;
            fact.producer_operator = descriptor.logical_operator;
            fact.producer_version = descriptor.version;
            fact.completeness |= DSL_TENSOR_FACT_COMPLETE_PRODUCER;
        } else if (value.value_kind == DSL_IR_VALUE_SYMBOL) {
            fact.completeness |= DSL_TENSOR_FACT_COMPLETE_PRODUCER;
        }
        fact.value_role = DSL_Tensor_Analysis_Value_Role
                              (analysis, value, fact.producer_operator);
        fact.ownership = DSL_Tensor_Analysis_Ownership
                             (value, fact.producer_operator,
                              fact.producer_version);
        if (fact.ownership != DSL_TENSOR_OWNERSHIP_UNKNOWN)
            fact.completeness |= DSL_TENSOR_FACT_COMPLETE_OWNERSHIP;

        analysis->facts.push_back(fact);
        DSL_TENSOR_FACT_RECORD &stored = analysis->facts.back();
        if (!DSL_Tensor_Analysis_Add_Uses
                 (analysis, &stored, diagnostic))
            return FALSE;
        if (stored.completeness != DSL_TENSOR_FACT_COMPLETE_ALL) {
            ++analysis->incomplete_fact_count;
            DSL_Tensor_Analysis_Report_Incomplete
                (diagnostic, stored.value_id, stored.completeness);
        }
    }
    return DSL_Tensor_Analysis_Verify(analysis, diagnostic);
}

BOOL
DSL_Tensor_Analysis_Verify
        (const DSL_TENSOR_ANALYSIS *analysis, FILE *diagnostic)
{
    if (!DSL_Tensor_Analysis_Active(analysis))
        return DSL_Tensor_Analysis_Report
                   (diagnostic, "program unit is not active", 0);
    UINT32 incomplete = 0;
    UINT32 expected_use = 1;
    for (UINT32 i = 0; i < analysis->facts.size(); ++i) {
        const DSL_TENSOR_FACT_RECORD &fact = analysis->facts[i];
        DSL_TENSOR_EVOLUTION_NODE_RECORD root;
        DSL_IR_VALUE_RECORD value;
        DSL_IR_NODE_RECORD producer;
        DSL_IR_OPCODE_DESCRIPTOR_RECORD producer_descriptor;
        if (fact.id != i + 1 || fact.reserved0 != 0 || fact.reserved1 != 0 ||
            fact.semantic_root_id == DSL_TENSOR_EVOLUTION_NODE_INVALID_ID ||
            !DSL_Tensor_Evolution_Get_Node
                 (analysis->graph, fact.semantic_root_id, &root) ||
            root.semantic_value_id != fact.value_id ||
            root.descriptor_ty != fact.descriptor_ty ||
            !DSL_IR_Image_Get_Value(fact.value_id, &value) ||
            !DSL_Tensor_Analysis_Value_Owned(analysis, value) ||
            value.ty != fact.descriptor_ty ||
            fact.element_ty != TY_tensor_element_ty(fact.descriptor_ty) ||
            fact.rank != TY_tensor_rank(fact.descriptor_ty) ||
            fact.dimension_state < DSL_TENSOR_DIMENSION_STATIC ||
            fact.dimension_state > DSL_TENSOR_DIMENSION_UNRESOLVED ||
            fact.ownership > DSL_TENSOR_OWNERSHIP_SHARED ||
            fact.reuse_role < DSL_TENSOR_REUSE_UNUSED ||
            fact.reuse_role > DSL_TENSOR_REUSE_MULTIPLE_USE ||
            fact.value_role < DSL_TENSOR_VALUE_ROLE_CONSTANT ||
            fact.value_role > DSL_TENSOR_VALUE_ROLE_SYMBOL ||
            fact.value_role != DSL_Tensor_Analysis_Value_Role
                                   (analysis, value,
                                    fact.producer_operator) ||
            fact.ownership != DSL_Tensor_Analysis_Ownership
                                  (value, fact.producer_operator,
                                   fact.producer_version) ||
            (fact.completeness & ~DSL_TENSOR_FACT_COMPLETE_ALL) != 0 ||
            (fact.use_count == 0 && fact.first_use_id != 0) ||
            (fact.use_count != 0 && fact.first_use_id != expected_use) ||
            (fact.use_count == 0 &&
             fact.reuse_role != DSL_TENSOR_REUSE_UNUSED) ||
            (fact.use_count == 1 &&
             fact.reuse_role != DSL_TENSOR_REUSE_SINGLE_USE) ||
            (fact.use_count > 1 &&
             fact.reuse_role != DSL_TENSOR_REUSE_MULTIPLE_USE))
            return DSL_Tensor_Analysis_Report
                       (diagnostic, "invalid tensor fact", fact.id);
        if (value.producer_node_id != DSL_IR_NODE_INVALID_ID) {
            if (!DSL_Tensor_Analysis_Node_Info
                     (value.producer_node_id, &producer,
                      &producer_descriptor) ||
                fact.producer_node_id != producer.id ||
                fact.producer_operator !=
                    producer_descriptor.logical_operator ||
                fact.producer_version != producer_descriptor.version)
                return DSL_Tensor_Analysis_Report
                           (diagnostic, "invalid producer fact", fact.id);
        } else if (fact.producer_node_id != DSL_IR_NODE_INVALID_ID ||
                   fact.producer_operator != OPR_DSLUNKNOWN ||
                   fact.producer_version != 0) {
            return DSL_Tensor_Analysis_Report
                       (diagnostic, "unexpected producer fact", fact.id);
        }
        if (fact.dimension_state == DSL_TENSOR_DIMENSION_STATIC &&
            (fact.static_dimension_count != (UINT32)fact.rank ||
             fact.dynamic_dimension_count != 0 ||
             (fact.completeness & DSL_TENSOR_FACT_COMPLETE_SHAPE) == 0))
            return DSL_Tensor_Analysis_Report
                       (diagnostic, "invalid static shape fact", fact.id);
        if (fact.dimension_state == DSL_TENSOR_DIMENSION_UNRESOLVED &&
            (fact.static_dimension_count !=
                 DSL_TENSOR_DIMENSION_COUNT_UNKNOWN ||
             fact.dynamic_dimension_count !=
                 DSL_TENSOR_DIMENSION_COUNT_UNKNOWN ||
             (fact.completeness & DSL_TENSOR_FACT_COMPLETE_SHAPE) != 0))
            return DSL_Tensor_Analysis_Report
                       (diagnostic, "invalid unresolved shape fact", fact.id);
        if (fact.completeness != DSL_TENSOR_FACT_COMPLETE_ALL)
            ++incomplete;
        expected_use += fact.use_count;
    }
    if (expected_use != analysis->uses.size() + 1 ||
        incomplete != analysis->incomplete_fact_count)
        return DSL_Tensor_Analysis_Report
                   (diagnostic, "fact/use census mismatch", 0);

    for (UINT32 i = 0; i < analysis->uses.size(); ++i) {
        const DSL_TENSOR_USE_FACT_RECORD &use = analysis->uses[i];
        DSL_IR_NODE_RECORD node;
        DSL_IR_OPCODE_DESCRIPTOR_RECORD descriptor;
        DSL_IR_VALUE_REFERENCE_RECORD reference;
        const DSL_TENSOR_FACT_RECORD *fact =
            use.tensor_fact_id == 0 ||
            use.tensor_fact_id > analysis->facts.size() ? NULL :
            &analysis->facts[use.tensor_fact_id - 1];
        if (use.id != i + 1 || use.reserved != 0 ||
            fact == NULL ||
            !DSL_Tensor_Analysis_Node_Info
                 (use.consumer_node_id, &node, &descriptor) ||
            node.first_operand_reference_id ==
                DSL_IR_VALUE_REFERENCE_INVALID_ID ||
            !DSL_IR_Image_Get_Value_Reference
                 (node.first_operand_reference_id + use.operand_ordinal,
                  &reference) ||
            reference.owner_node_id != node.id ||
            reference.ordinal != use.operand_ordinal ||
            reference.value_id != fact->value_id ||
            use.consumer_operator != descriptor.logical_operator ||
            use.consumer_version != descriptor.version ||
            use.operand_ordinal >= node.operand_count ||
            use.role < DSL_TENSOR_USE_ROLE_GENERIC ||
            use.role > DSL_TENSOR_USE_ROLE_ELEMENTWISE_INPUT ||
            use.role != DSL_Tensor_Analysis_Use_Role
                            ((DSL_OPERATOR)descriptor.logical_operator,
                             descriptor.version, use.operand_ordinal,
                             descriptor.shape_rule) ||
            use.shape_rule != descriptor.shape_rule ||
            use.memory_behavior != DSL_Tensor_Analysis_Operand_Memory
                                       ((DSL_OPERATOR)
                                            descriptor.logical_operator,
                                        descriptor.version,
                                        use.operand_ordinal,
                                        descriptor.effect_model))
            return DSL_Tensor_Analysis_Report
                       (diagnostic, "invalid tensor use fact", use.id);
    }
    return TRUE;
}

BOOL
DSL_Tensor_Analysis_Is_Complete (const DSL_TENSOR_ANALYSIS *analysis)
{
    return analysis != NULL && analysis->incomplete_fact_count == 0;
}

void
DSL_Tensor_Analysis_Print
        (FILE *file, const DSL_TENSOR_ANALYSIS *analysis)
{
    if (file == NULL || analysis == NULL)
        return;
    fprintf(file,
            "DSLTensorAnalysis: owner=<%u,%u,%s> facts=%u uses=%u "
            "complete=%s\n",
            ST_IDX_level(analysis->owner_pu_st),
            ST_IDX_index(analysis->owner_pu_st),
            DSL_Tensor_Analysis_Owner_Valid(analysis->owner_pu_st) ?
                ST_name(St_Table[analysis->owner_pu_st]) : "<invalid>",
            (UINT32)analysis->facts.size(), (UINT32)analysis->uses.size(),
            analysis->incomplete_fact_count == 0 ? "yes" : "no");
    for (UINT32 i = 0; i < analysis->facts.size(); ++i) {
        const DSL_TENSOR_FACT_RECORD &fact = analysis->facts[i];
        DSL_IR_VALUE_RECORD value;
        const char *name = "<invalid>";
        const char *dtype = "<pending>";
        const char *shape = "<pending>";
        const char *producer = "none";
        if (DSL_IR_Image_Get_Value(fact.value_id, &value) &&
            value.name != STR_IDX_ZERO)
            name = Index_To_Str(value.name);
        const char *candidate = TY_tensor_attribute
                                    (fact.descriptor_ty,
                                     TY_TENSOR_SCHEMA_DTYPE);
        if (candidate != NULL)
            dtype = candidate;
        candidate = TY_tensor_attribute
                        (fact.descriptor_ty, TY_TENSOR_SCHEMA_SHAPE);
        if (candidate != NULL)
            shape = candidate;
        if (fact.producer_operator != OPR_DSLUNKNOWN)
            producer = DSL_Tensor_Analysis_Logical_Name
                           (fact.producer_node_id,
                            (DSL_OPERATOR)fact.producer_operator);
        fprintf(file,
                "  fact[%u] root=%u value=%u name=%s ty=%u element_ty=%u "
                "dtype=%s rank=%d shape=%s dimensions=%s "
                "producer=%s.v%u role=%s ownership=%s reuse=%s "
                "first_use=%u use_count=%u completeness=0x%x\n",
                fact.id, fact.semantic_root_id, fact.value_id, name,
                TY_IDX_index(fact.descriptor_ty),
                TY_IDX_index(fact.element_ty), dtype, fact.rank, shape,
                DSL_Tensor_Dimension_State_Name(fact.dimension_state),
                producer, fact.producer_version,
                DSL_Tensor_Value_Role_Name(fact.value_role),
                DSL_Tensor_Ownership_Name(fact.ownership),
                DSL_Tensor_Reuse_Role_Name(fact.reuse_role),
                fact.first_use_id, fact.use_count, fact.completeness);
    }
    for (UINT32 i = 0; i < analysis->uses.size(); ++i) {
        const DSL_TENSOR_USE_FACT_RECORD &use = analysis->uses[i];
        fprintf(file,
                "  use[%u] fact=%u consumer=%s.v%u node=%u kid%u "
                "role=%s shape_rule=%s memory=0x%x\n",
                use.id, use.tensor_fact_id,
                DSL_Tensor_Analysis_Logical_Name
                    (use.consumer_node_id,
                     (DSL_OPERATOR)use.consumer_operator),
                use.consumer_version, use.consumer_node_id,
                use.operand_ordinal,
                DSL_Tensor_Use_Role_Name(use.role),
                DSL_Tensor_Analysis_Shape_Rule_Name(use.shape_rule),
                use.memory_behavior);
    }
}

UINT32
DSL_Tensor_Analysis_Fact_Count (const DSL_TENSOR_ANALYSIS *analysis)
{
    return analysis == NULL ? 0 : analysis->facts.size();
}

UINT32
DSL_Tensor_Analysis_Use_Count (const DSL_TENSOR_ANALYSIS *analysis)
{
    return analysis == NULL ? 0 : analysis->uses.size();
}

BOOL
DSL_Tensor_Analysis_Get_Fact
        (const DSL_TENSOR_ANALYSIS *analysis, DSL_TENSOR_FACT_ID id,
         DSL_TENSOR_FACT_RECORD *record)
{
    if (analysis == NULL || record == NULL || id == 0 ||
        id > analysis->facts.size())
        return FALSE;
    *record = analysis->facts[id - 1];
    return TRUE;
}

BOOL
DSL_Tensor_Analysis_Get_Use
        (const DSL_TENSOR_ANALYSIS *analysis, DSL_TENSOR_USE_FACT_ID id,
         DSL_TENSOR_USE_FACT_RECORD *record)
{
    if (analysis == NULL || record == NULL || id == 0 ||
        id > analysis->uses.size())
        return FALSE;
    *record = analysis->uses[id - 1];
    return TRUE;
}
