/*
 * Copyright (C) 2026 Open64 Project
 */

/*
 * Stores and validates AIO-3 PU-local semantic tensor facts. VHO derives the
 * records from canonical descriptors and logical DSL operator contracts.
 * Source metadata is deliberately excluded from semantic equivalence. Design:
 * doc/AI-COMPILER-OPTIMIZATION-AIO3-SEMANTIC-TENSOR.md.
 */

#include <vector>

#include "dsl_tensor_analysis.h"
#include "dsl_tensor_analysis_internal.h"
#include "pu_info.h"
#include "strtab.h"

static const char *DSL_tensor_dimension_state_name_table[] = {
    "unknown", "static", "unresolved"
};

static const char *DSL_tensor_ownership_name_table[] = {
    "unknown", "unique", "shared"
};

static const char *DSL_tensor_reuse_role_name_table[] = {
    "unknown", "unused", "single_use", "multiple_use"
};

static const char *DSL_tensor_value_role_name_table[] = {
    "unknown", "constant", "model_input", "formal", "intermediate",
    "symbol"
};

static const char *DSL_tensor_use_role_name_table[] = {
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
           DSL_tensor_evolution_owner(analysis->graph) ==
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

const char *
DSL_tensor_dimension_state_name (UINT32 state)
{
    return state < sizeof(DSL_tensor_dimension_state_name_table) /
                       sizeof(DSL_tensor_dimension_state_name_table[0]) ?
           DSL_tensor_dimension_state_name_table[state] : "unknown";
}

const char *
DSL_tensor_ownership_name (UINT32 ownership)
{
    return ownership < sizeof(DSL_tensor_ownership_name_table) /
                           sizeof(DSL_tensor_ownership_name_table[0]) ?
           DSL_tensor_ownership_name_table[ownership] : "unknown";
}

const char *
DSL_tensor_reuse_role_name (UINT32 role)
{
    return role < sizeof(DSL_tensor_reuse_role_name_table) /
                      sizeof(DSL_tensor_reuse_role_name_table[0]) ?
           DSL_tensor_reuse_role_name_table[role] : "unknown";
}

const char *
DSL_tensor_value_role_name (UINT32 role)
{
    return role < sizeof(DSL_tensor_value_role_name_table) /
                      sizeof(DSL_tensor_value_role_name_table[0]) ?
           DSL_tensor_value_role_name_table[role] : "unknown";
}

const char *
DSL_tensor_use_role_name (UINT32 role)
{
    return role < sizeof(DSL_tensor_use_role_name_table) /
                      sizeof(DSL_tensor_use_role_name_table[0]) ?
           DSL_tensor_use_role_name_table[role] : "unknown";
}

DSL_TENSOR_ANALYSIS *
DSL_tensor_analysis_create
        (PU_Info *pu, const DSL_TENSOR_EVOLUTION_GRAPH *graph,
         FILE *diagnostic)
{
    if (pu == NULL || graph == NULL || Current_PU_Info != pu ||
        DSL_tensor_evolution_owner(graph) != PU_Info_proc_sym(pu) ||
        !DSL_tensor_evolution_verify(graph, diagnostic)) {
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
DSL_tensor_analysis_destroy (DSL_TENSOR_ANALYSIS *analysis)
{
    delete analysis;
}

BOOL
DSL_tensor_analysis_find_fact
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
DSL_tensor_analysis_verify
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
            !DSL_tensor_evolution_get_node
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
            use.shape_rule != descriptor.shape_rule)
            return DSL_Tensor_Analysis_Report
                       (diagnostic, "invalid tensor use fact", use.id);
    }
    return TRUE;
}

BOOL
DSL_tensor_analysis_is_complete (const DSL_TENSOR_ANALYSIS *analysis)
{
    return analysis != NULL && analysis->incomplete_fact_count == 0;
}

void
DSL_tensor_analysis_print
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
                DSL_tensor_dimension_state_name(fact.dimension_state),
                producer, fact.producer_version,
                DSL_tensor_value_role_name(fact.value_role),
                DSL_tensor_ownership_name(fact.ownership),
                DSL_tensor_reuse_role_name(fact.reuse_role),
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
                DSL_tensor_use_role_name(use.role),
                DSL_Tensor_Analysis_Shape_Rule_Name(use.shape_rule),
                use.memory_behavior);
    }
}

UINT32
DSL_tensor_analysis_fact_count (const DSL_TENSOR_ANALYSIS *analysis)
{
    return analysis == NULL ? 0 : analysis->facts.size();
}

UINT32
DSL_tensor_analysis_use_count (const DSL_TENSOR_ANALYSIS *analysis)
{
    return analysis == NULL ? 0 : analysis->uses.size();
}

BOOL
DSL_tensor_analysis_get_fact
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
DSL_tensor_analysis_get_use
        (const DSL_TENSOR_ANALYSIS *analysis, DSL_TENSOR_USE_FACT_ID id,
         DSL_TENSOR_USE_FACT_RECORD *record)
{
    if (analysis == NULL || record == NULL || id == 0 ||
        id > analysis->uses.size())
        return FALSE;
    *record = analysis->uses[id - 1];
    return TRUE;
}
