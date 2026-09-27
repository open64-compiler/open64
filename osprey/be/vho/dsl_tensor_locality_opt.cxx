/*
 * Copyright (C) 2026 Open64 Project
 */

/* VHO-owned AIO-4 PU-local lifetime and locality derivation. */

#include <algorithm>
#include <limits.h>
#include <string.h>
#include <vector>

#include "dsl_tensor_locality_opt.h"
#include "dsl_tensor_locality_internal.h"
#include "dsl_tensor_analysis_opt.h"
#include "dsl_memory_behavior.h"
#include "dsl_shape.h"
#include "pu_info.h"

static BOOL
DSL_Tensor_Locality_Report
        (FILE *diagnostic, const char *message, UINT32 id)
{
    if (diagnostic != NULL)
        fprintf(diagnostic, "DSL tensor locality error: %s id=%u\n",
                message, id);
    return FALSE;
}

static BOOL
DSL_Tensor_Locality_Owner_Valid (ST_IDX owner_pu_st)
{
    return ST_IDX_level(owner_pu_st) == GLOBAL_SYMTAB &&
           ST_IDX_index(owner_pu_st) != 0 &&
           ST_IDX_index(owner_pu_st) < ST_Table_Size(GLOBAL_SYMTAB) &&
           ST_class(owner_pu_st) == CLASS_FUNC &&
           ST_pu(St_Table[owner_pu_st]) != PU_IDX_ZERO &&
           ST_pu(St_Table[owner_pu_st]) < PU_Table_Size();
}

static BOOL
DSL_Tensor_Control_Active (const DSL_TENSOR_CONTROL_SNAPSHOT *snapshot)
{
    return snapshot != NULL && snapshot->pu != NULL &&
           Current_PU_Info == snapshot->pu &&
           DSL_Tensor_Locality_Owner_Valid(snapshot->owner_pu_st) &&
           PU_Info_proc_sym(snapshot->pu) == snapshot->owner_pu_st &&
           Current_pu ==
               &Pu_Table[ST_pu(St_Table[snapshot->owner_pu_st])];
}

static BOOL
DSL_Tensor_Locality_Active (const DSL_TENSOR_LOCALITY_ANALYSIS *analysis)
{
    return analysis != NULL && analysis->pu != NULL &&
           Current_PU_Info == analysis->pu &&
           DSL_Tensor_Locality_Owner_Valid(analysis->owner_pu_st) &&
           PU_Info_proc_sym(analysis->pu) == analysis->owner_pu_st &&
           DSL_Tensor_Control_Active(analysis->snapshot);
}

static const DSL_TENSOR_CONTROL_BLOCK *
DSL_Tensor_Control_Find_Block
        (const DSL_TENSOR_CONTROL_SNAPSHOT *snapshot, UINT32 block_id)
{
    if (snapshot == NULL || block_id == 0)
        return NULL;
    for (UINT32 i = 0; i < snapshot->blocks.size(); ++i) {
        if (snapshot->blocks[i].block_id == block_id)
            return &snapshot->blocks[i];
    }
    return NULL;
}

static const DSL_TENSOR_CONTROL_POSITION *
DSL_Tensor_Control_Find_Position
        (const DSL_TENSOR_CONTROL_SNAPSHOT *snapshot,
         DSL_IR_NODE_ID node_id)
{
    if (snapshot == NULL || node_id == DSL_IR_NODE_INVALID_ID)
        return NULL;
    for (UINT32 i = 0; i < snapshot->positions.size(); ++i) {
        if (snapshot->positions[i].node_id == node_id)
            return &snapshot->positions[i];
    }
    return NULL;
}

static UINT32
DSL_Tensor_Control_Path_Position
        (const DSL_TENSOR_CONTROL_SNAPSHOT *snapshot,
         DSL_IR_NODE_ID node_id)
{
    if (snapshot == NULL || node_id == DSL_IR_NODE_INVALID_ID)
        return 0;
    for (UINT32 i = 0; i < snapshot->positions.size(); ++i) {
        if (snapshot->positions[i].node_id == node_id)
            return i + 1;
    }
    return 0;
}

static BOOL
DSL_Tensor_Control_Ancestor
        (const DSL_TENSOR_CONTROL_SNAPSHOT *snapshot,
         UINT32 ancestor_id, UINT32 descendant_id, BOOL postdominator)
{
    UINT32 current = descendant_id;
    UINT32 steps = 0;
    while (current != 0 && steps <= snapshot->blocks.size()) {
        if (current == ancestor_id)
            return TRUE;
        const DSL_TENSOR_CONTROL_BLOCK *block =
            DSL_Tensor_Control_Find_Block(snapshot, current);
        if (block == NULL)
            return FALSE;
        current = postdominator ? block->immediate_postdominator :
                                  block->immediate_dominator;
        ++steps;
    }
    return FALSE;
}

static BOOL
DSL_Tensor_Control_Block_Less
        (const DSL_TENSOR_CONTROL_BLOCK &left,
         const DSL_TENSOR_CONTROL_BLOCK &right)
{
    return left.block_id < right.block_id;
}

static BOOL
DSL_Tensor_Control_Position_Less
        (const DSL_TENSOR_CONTROL_POSITION &left,
         const DSL_TENSOR_CONTROL_POSITION &right)
{
    if (left.reverse_postorder != right.reverse_postorder)
        return left.reverse_postorder < right.reverse_postorder;
    if (left.statement_order != right.statement_order)
        return left.statement_order < right.statement_order;
    return left.node_id < right.node_id;
}

static UINT32
DSL_Tensor_Locality_Size
        (const DSL_TENSOR_FACT_RECORD &tensor, UINT64 *bytes)
{
    const char *shape = TY_tensor_attribute
                            (tensor.descriptor_ty,
                             TY_TENSOR_SCHEMA_SHAPE);
    if (bytes != NULL)
        *bytes = DSL_TENSOR_LOCALITY_UNKNOWN_U64;
    if (shape == NULL || tensor.element_ty == TY_IDX_ZERO ||
        TY_size(tensor.element_ty) == 0)
        return DSL_TENSOR_SIZE_UNKNOWN;
    if (tensor.dimension_state != DSL_TENSOR_DIMENSION_STATIC) {
        return strstr(shape, "<pending>") == NULL ?
               DSL_TENSOR_SIZE_SYMBOLIC : DSL_TENSOR_SIZE_UNKNOWN;
    }
    std::vector<UINT64> dimensions(tensor.rank == 0 ? 1 : tensor.rank);
    UINT32 rank = 0;
    if (!DSL_Shape_Parse_Static_Dimensions
             (shape, dimensions.empty() ? NULL : &dimensions[0],
              dimensions.size(), &rank) || rank != (UINT32)tensor.rank)
        return DSL_TENSOR_SIZE_UNKNOWN;
    UINT64 total = TY_size(tensor.element_ty);
    for (UINT32 i = 0; i < rank; ++i) {
        if (dimensions[i] != 0 &&
            total > DSL_TENSOR_LOCALITY_UNKNOWN_U64 / dimensions[i])
            return DSL_TENSOR_SIZE_OVERFLOW;
        total *= dimensions[i];
    }
    if (bytes != NULL)
        *bytes = total;
    return DSL_TENSOR_SIZE_STATIC;
}

static UINT32
DSL_Tensor_Locality_Access (const DSL_TENSOR_USE_FACT_RECORD &use)
{
    if (use.role == DSL_TENSOR_USE_ROLE_CONTRACTION_KID0 ||
        use.role == DSL_TENSOR_USE_ROLE_CONTRACTION_KID1 ||
        use.shape_rule == DSL_SHAPE_RULE_CONTRACTION)
        return DSL_TENSOR_ACCESS_CONTRACTION;
    if (use.role == DSL_TENSOR_USE_ROLE_REDUCTION_SOURCE ||
        use.shape_rule == DSL_SHAPE_RULE_REDUCTION)
        return DSL_TENSOR_ACCESS_REDUCTION;
    if (use.role == DSL_TENSOR_USE_ROLE_VIEW_SOURCE ||
        use.shape_rule == DSL_SHAPE_RULE_VIEW ||
        use.shape_rule == DSL_SHAPE_RULE_LAYOUT)
        return DSL_TENSOR_ACCESS_VIEW;
    if (use.role == DSL_TENSOR_USE_ROLE_INDEX)
        return DSL_TENSOR_ACCESS_INDEXED;
    if (use.shape_rule == DSL_SHAPE_RULE_IDENTITY ||
        use.shape_rule == DSL_SHAPE_RULE_BROADCAST ||
        use.role == DSL_TENSOR_USE_ROLE_ELEMENTWISE_INPUT ||
        use.role == DSL_TENSOR_USE_ROLE_ACTIVATION ||
        use.role == DSL_TENSOR_USE_ROLE_WEIGHT ||
        use.role == DSL_TENSOR_USE_ROLE_BIAS)
        return DSL_TENSOR_ACCESS_ELEMENTWISE;
    return DSL_TENSOR_ACCESS_UNKNOWN;
}

static BOOL
DSL_Tensor_Locality_Use_Less
        (const DSL_TENSOR_LOCALITY_USE_RECORD &left,
         const DSL_TENSOR_LOCALITY_USE_RECORD &right)
{
    if (left.path_position != right.path_position)
        return left.path_position < right.path_position;
    return left.tensor_use_fact_id < right.tensor_use_fact_id;
}

static BOOL
DSL_Tensor_Locality_Effectful
        (const DSL_TENSOR_LOCALITY_USE_RECORD &local_use,
         const DSL_TENSOR_USE_FACT_RECORD &tensor_use)
{
    const UINT32 unsafe = DSL_MEMORY_BEHAVIOR_MODIFY |
                          DSL_MEMORY_BEHAVIOR_MAY_ALIAS |
                          DSL_MEMORY_BEHAVIOR_INPLACE_UPDATE |
                          DSL_MEMORY_BEHAVIOR_CONSUMES;
    return (local_use.control_flags &
            DSL_TENSOR_CONTROL_EFFECT_BARRIER) != 0 ||
           (tensor_use.memory_behavior & unsafe) != 0;
}

static void
DSL_Tensor_Locality_Dataflow_Depths
        (std::vector<UINT32> *forward, std::vector<UINT32> *reverse,
         UINT32 *maximum)
{
    UINT32 node_count = DSL_IR_Image_Node_Count();
    forward->assign(node_count + 1, 1);
    reverse->assign(node_count + 1, 1);
    *maximum = 0;
    for (DSL_IR_NODE_ID id = 1; id <= node_count; ++id) {
        DSL_IR_NODE_RECORD node;
        if (!DSL_IR_Image_Get_Node(id, &node) ||
            (node.flags & DSL_IR_NODE_FLAG_RETIRED) != 0)
            continue;
        for (UINT32 ordinal = 0; ordinal < node.operand_count; ++ordinal) {
            DSL_IR_VALUE_REFERENCE_RECORD reference;
            DSL_IR_VALUE_RECORD value;
            if (DSL_IR_Image_Get_Value_Reference
                    (node.first_operand_reference_id + ordinal, &reference) &&
                DSL_IR_Image_Get_Value(reference.value_id, &value) &&
                value.producer_node_id != DSL_IR_NODE_INVALID_ID)
                (*forward)[id] = std::max
                    ((*forward)[id], (*forward)[value.producer_node_id] + 1);
        }
        *maximum = std::max(*maximum, (*forward)[id]);
    }
    for (DSL_IR_NODE_ID id = node_count; id != 0; --id) {
        DSL_IR_NODE_RECORD node;
        if (!DSL_IR_Image_Get_Node(id, &node) ||
            (node.flags & DSL_IR_NODE_FLAG_RETIRED) != 0)
            continue;
        for (DSL_IR_VALUE_REFERENCE_ID reference_id = 1;
             reference_id <= DSL_IR_Image_Value_Reference_Count();
             ++reference_id) {
            DSL_IR_VALUE_REFERENCE_RECORD reference;
            DSL_IR_VALUE_RECORD value;
            if (DSL_IR_Image_Get_Value_Reference(reference_id, &reference) &&
                DSL_IR_Image_Get_Value(reference.value_id, &value) &&
                value.producer_node_id == id)
                (*reverse)[id] = std::max
                    ((*reverse)[id], (*reverse)[reference.owner_node_id] + 1);
        }
    }
}

static void
DSL_Tensor_Locality_Finalize_Fact
        (DSL_TENSOR_LOCALITY_ANALYSIS *analysis,
         DSL_TENSOR_LOCALITY_FACT_RECORD *fact,
         const DSL_TENSOR_FACT_RECORD &tensor,
         const std::vector<UINT32> &forward,
         const std::vector<UINT32> &reverse, UINT32 maximum)
{
    const DSL_TENSOR_CONTROL_POSITION *producer =
        DSL_Tensor_Control_Find_Position
            (analysis->snapshot, fact->producer_node_id);
    BOOL effectful = FALSE;
    BOOL region_crossing = FALSE;
    BOOL loop_crossing = FALSE;
    BOOL branch = FALSE;
    UINT32 access = DSL_TENSOR_ACCESS_UNKNOWN;
    UINT32 producer_block = producer == NULL ? 0 : producer->block_id;
    UINT32 producer_region = 0;
    UINT32 producer_loop = 0;
    if (producer != NULL) {
        const DSL_TENSOR_CONTROL_BLOCK *block =
            DSL_Tensor_Control_Find_Block
                (analysis->snapshot, producer->block_id);
        producer_region = block->region_id;
        producer_loop = block->loop_depth;
        branch = (block->flags & DSL_TENSOR_CONTROL_BRANCH) != 0;
        effectful = (block->flags &
                     DSL_TENSOR_CONTROL_EFFECT_BARRIER) != 0;
    }
    for (UINT32 i = 0; i < fact->use_count; ++i) {
        const DSL_TENSOR_LOCALITY_USE_RECORD &local_use =
            analysis->uses[fact->first_use_id - 1 + i];
        DSL_TENSOR_USE_FACT_RECORD tensor_use;
        DSL_tensor_analysis_get_use
            (analysis->tensor_analysis, local_use.tensor_use_fact_id,
             &tensor_use);
        UINT32 candidate = DSL_Tensor_Locality_Access(tensor_use);
        if (access == DSL_TENSOR_ACCESS_UNKNOWN)
            access = candidate;
        else if (candidate != DSL_TENSOR_ACCESS_UNKNOWN &&
                 candidate != access)
            access = DSL_TENSOR_ACCESS_MIXED;
        effectful |= DSL_Tensor_Locality_Effectful(local_use, tensor_use);
        region_crossing |= local_use.region_id != producer_region;
        loop_crossing |= local_use.loop_depth != producer_loop ||
                         local_use.loop_depth != 0;
        branch |= (local_use.control_flags & DSL_TENSOR_CONTROL_BRANCH) != 0;
        if (producer_block != 0 && local_use.block_id != producer_block)
            branch = TRUE;
    }
    fact->access_pattern = access;
    fact->alias_state = tensor.ownership == DSL_TENSOR_OWNERSHIP_UNIQUE ?
                        DSL_TENSOR_ALIAS_PROVEN_UNIQUE :
                        tensor.ownership == DSL_TENSOR_OWNERSHIP_SHARED ?
                        DSL_TENSOR_ALIAS_CONSERVATIVE :
                        DSL_TENSOR_ALIAS_UNKNOWN;

    /*
     * Keep uncertain control flow conservative. Exact-block evidence is the
     * only class later stages may treat as a precise lifetime; REGION, loop,
     * effect, and alias boundaries remain explicit instead of being guessed.
     */
    if (fact->alias_state != DSL_TENSOR_ALIAS_PROVEN_UNIQUE)
        fact->lifetime_state = DSL_TENSOR_LIFETIME_ALIAS;
    else if (effectful)
        fact->lifetime_state = DSL_TENSOR_LIFETIME_EFFECT;
    else if (producer == NULL)
        fact->lifetime_state = DSL_TENSOR_LIFETIME_UNKNOWN;
    else if (region_crossing)
        fact->lifetime_state = DSL_TENSOR_LIFETIME_REGION;
    else if (loop_crossing)
        fact->lifetime_state = DSL_TENSOR_LIFETIME_LOOP;
    else if (!branch)
        fact->lifetime_state = DSL_TENSOR_LIFETIME_EXACT_BLOCK;
    else {
        BOOL dominated = TRUE;
        BOOL postdominated = fact->use_count == 0;
        for (UINT32 i = 0; i < fact->use_count; ++i) {
            const DSL_TENSOR_LOCALITY_USE_RECORD &use =
                analysis->uses[fact->first_use_id - 1 + i];
            dominated &= DSL_Tensor_Control_Ancestor
                             (analysis->snapshot, producer_block,
                              use.block_id, FALSE);
        }
        if (fact->use_count != 0) {
            const DSL_TENSOR_LOCALITY_USE_RECORD &last =
                analysis->uses[fact->first_use_id + fact->use_count - 2];
            postdominated = DSL_Tensor_Control_Ancestor
                                (analysis->snapshot, last.block_id,
                                 producer_block, TRUE);
        }
        fact->lifetime_state = dominated && postdominated ?
                               DSL_TENSOR_LIFETIME_DOMINATED :
                               DSL_TENSOR_LIFETIME_BRANCH;
    }

    if (fact->lifetime_state == DSL_TENSOR_LIFETIME_EXACT_BLOCK &&
        fact->use_count != 0) {
        UINT32 previous = producer->statement_order;
        UINT64 maximum_distance = 0;
        for (UINT32 i = 0; i < fact->use_count; ++i) {
            const DSL_TENSOR_LOCALITY_USE_RECORD &use =
                analysis->uses[fact->first_use_id - 1 + i];
            if (use.block_id != producer->block_id ||
                use.statement_order < previous) {
                fact->reuse_distance_state = DSL_TENSOR_DISTANCE_UNKNOWN;
                break;
            }
            maximum_distance = std::max
                (maximum_distance,
                 (UINT64)(use.statement_order - previous));
            previous = use.statement_order;
            fact->reuse_distance_state = DSL_TENSOR_DISTANCE_EXACT;
        }
        fact->reuse_distance_statements = maximum_distance;
    } else if (fact->use_count != 0) {
        fact->reuse_distance_state = DSL_TENSOR_DISTANCE_CONSERVATIVE;
    }

    if (fact->producer_node_id != DSL_IR_NODE_INVALID_ID &&
        fact->producer_node_id < forward.size() &&
        fact->lifetime_state != DSL_TENSOR_LIFETIME_BRANCH &&
        fact->lifetime_state != DSL_TENSOR_LIFETIME_LOOP &&
        fact->lifetime_state != DSL_TENSOR_LIFETIME_REGION &&
        fact->lifetime_state != DSL_TENSOR_LIFETIME_EFFECT &&
        fact->lifetime_state != DSL_TENSOR_LIFETIME_ALIAS)
        fact->critical_path_state =
            forward[fact->producer_node_id] +
                reverse[fact->producer_node_id] - 1 == maximum ?
            DSL_TENSOR_CRITICAL_PATH_ON : DSL_TENSOR_CRITICAL_PATH_OFF;
    else
        fact->critical_path_state = DSL_TENSOR_CRITICAL_PATH_UNKNOWN;

    if (fact->alias_state != DSL_TENSOR_ALIAS_PROVEN_UNIQUE || effectful)
        fact->residency_benefit = DSL_TENSOR_RESIDENCY_UNKNOWN;
    else if (fact->use_count == 0)
        fact->residency_benefit = DSL_TENSOR_RESIDENCY_NONE;
    else if (fact->use_count == 1)
        fact->residency_benefit = DSL_TENSOR_RESIDENCY_LOW;
    else if (fact->reuse_distance_state == DSL_TENSOR_DISTANCE_EXACT &&
             fact->reuse_distance_statements <= 4)
        fact->residency_benefit = DSL_TENSOR_RESIDENCY_HIGH;
    else
        fact->residency_benefit = DSL_TENSOR_RESIDENCY_MEDIUM;

    if (fact->size_state == DSL_TENSOR_SIZE_STATIC) {
        fact->estimated_read_bytes =
            fact->use_count == 0 ? 0 :
            fact->object_bytes <=
                DSL_TENSOR_LOCALITY_UNKNOWN_U64 / fact->use_count ?
            fact->object_bytes * fact->use_count :
            DSL_TENSOR_LOCALITY_UNKNOWN_U64;
        fact->estimated_write_bytes =
            fact->producer_node_id == DSL_IR_NODE_INVALID_ID ? 0 :
            fact->object_bytes;
    }
}

BOOL
VHO_DSL_Tensor_Locality_Build
        (DSL_TENSOR_LOCALITY_ANALYSIS *analysis, FILE *diagnostic)
{
    if (!DSL_Tensor_Locality_Active(analysis) ||
        !VHO_DSL_Tensor_Analysis_Verify
             (analysis->tensor_analysis, diagnostic) ||
        !analysis->snapshot->sealed)
        return DSL_Tensor_Locality_Report
                   (diagnostic, "analysis is not active", 0);
    if (!analysis->facts.empty())
        return DSL_tensor_locality_verify(analysis, diagnostic);

    for (DSL_TENSOR_FACT_ID tensor_id = 1;
         tensor_id <= DSL_tensor_analysis_fact_count
                          (analysis->tensor_analysis);
         ++tensor_id) {
        DSL_TENSOR_FACT_RECORD tensor;
        DSL_TENSOR_LOCALITY_FACT_RECORD fact;
        std::vector<DSL_TENSOR_LOCALITY_USE_RECORD> uses;
        if (!DSL_tensor_analysis_get_fact
                 (analysis->tensor_analysis, tensor_id, &tensor))
            return DSL_Tensor_Locality_Report
                       (diagnostic, "missing tensor fact", tensor_id);
        memset(&fact, 0, sizeof(fact));
        fact.id = analysis->facts.size() + 1;
        fact.tensor_fact_id = tensor.id;
        fact.value_id = tensor.value_id;
        fact.producer_node_id = tensor.producer_node_id;
        fact.object_bytes = DSL_TENSOR_LOCALITY_UNKNOWN_U64;
        fact.reuse_distance_statements = DSL_TENSOR_LOCALITY_UNKNOWN_U64;
        fact.working_set_bytes = DSL_TENSOR_LOCALITY_UNKNOWN_U64;
        fact.estimated_read_bytes = DSL_TENSOR_LOCALITY_UNKNOWN_U64;
        fact.estimated_write_bytes = DSL_TENSOR_LOCALITY_UNKNOWN_U64;
        fact.size_state = DSL_Tensor_Locality_Size(tensor,
                                                   &fact.object_bytes);
        fact.producer_path_position = DSL_Tensor_Control_Path_Position
                                          (analysis->snapshot,
                                           fact.producer_node_id);
        fact.last_use_path_position = fact.producer_path_position;

        for (UINT32 i = 0; i < tensor.use_count; ++i) {
            DSL_TENSOR_USE_FACT_RECORD tensor_use;
            DSL_TENSOR_LOCALITY_USE_RECORD use;
            const DSL_TENSOR_CONTROL_POSITION *position;
            const DSL_TENSOR_CONTROL_BLOCK *block;
            DSL_tensor_analysis_get_use
                (analysis->tensor_analysis, tensor.first_use_id + i,
                 &tensor_use);
            memset(&use, 0, sizeof(use));
            use.tensor_use_fact_id = tensor_use.id;
            use.consumer_node_id = tensor_use.consumer_node_id;
            position = DSL_Tensor_Control_Find_Position
                           (analysis->snapshot, use.consumer_node_id);
            if (position != NULL) {
                block = DSL_Tensor_Control_Find_Block
                            (analysis->snapshot, position->block_id);
                use.block_id = position->block_id;
                use.statement_order = position->statement_order;
                use.path_position = DSL_Tensor_Control_Path_Position
                                        (analysis->snapshot,
                                         use.consumer_node_id);
                use.loop_depth = block->loop_depth;
                use.region_id = block->region_id;
                use.control_flags = block->flags;
            }
            uses.push_back(use);
        }
        std::sort(uses.begin(), uses.end(), DSL_Tensor_Locality_Use_Less);
        fact.first_use_id = uses.empty() ? 0 : analysis->uses.size() + 1;
        for (UINT32 i = 0; i < uses.size(); ++i) {
            uses[i].id = analysis->uses.size() + 1;
            uses[i].locality_fact_id = fact.id;
            analysis->uses.push_back(uses[i]);
        }
        fact.use_count = uses.size();
        if (!uses.empty()) {
            fact.last_consumer_node_id = uses.back().consumer_node_id;
            fact.last_use_path_position = uses.back().path_position;
        }
        analysis->facts.push_back(fact);
    }

    std::vector<UINT32> forward;
    std::vector<UINT32> reverse;
    UINT32 maximum = 0;
    DSL_Tensor_Locality_Dataflow_Depths(&forward, &reverse, &maximum);
    for (UINT32 i = 0; i < analysis->facts.size(); ++i) {
        DSL_TENSOR_FACT_RECORD tensor;
        DSL_tensor_analysis_get_fact
            (analysis->tensor_analysis,
             analysis->facts[i].tensor_fact_id, &tensor);
        DSL_Tensor_Locality_Finalize_Fact
            (analysis, &analysis->facts[i], tensor,
             forward, reverse, maximum);
    }

    for (UINT32 i = 0; i < analysis->facts.size(); ++i) {
        DSL_TENSOR_LOCALITY_FACT_RECORD &fact = analysis->facts[i];
        if (fact.lifetime_state != DSL_TENSOR_LIFETIME_EXACT_BLOCK ||
            fact.size_state != DSL_TENSOR_SIZE_STATIC)
            continue;
        UINT64 working_set = 0;
        for (UINT32 j = 0; j < analysis->facts.size(); ++j) {
            const DSL_TENSOR_LOCALITY_FACT_RECORD &candidate =
                analysis->facts[j];
            if (candidate.size_state != DSL_TENSOR_SIZE_STATIC ||
                candidate.producer_path_position <
                    fact.producer_path_position ||
                candidate.producer_path_position >
                    fact.last_use_path_position)
                continue;
            if (working_set > DSL_TENSOR_LOCALITY_UNKNOWN_U64 -
                                  candidate.object_bytes) {
                working_set = DSL_TENSOR_LOCALITY_UNKNOWN_U64;
                break;
            }
            working_set += candidate.object_bytes;
        }
        fact.working_set_bytes = working_set;
    }
    return DSL_tensor_locality_verify(analysis, diagnostic);
}
