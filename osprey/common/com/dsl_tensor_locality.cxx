/*
 * Copyright (C) 2026 Open64 Project
 */

/*
 * Computes AIO-4 PU-local tensor lifetime, reuse, alias, and locality facts
 * from stable DSL IDs plus a driver-owned control snapshot. It does not retain
 * CFG or WOPT pointers. Design:
 * doc/AI-COMPILER-OPTIMIZATION-AIO4-LIFETIME-LOCALITY.md.
 */

#include <algorithm>
#include <limits.h>
#include <string.h>
#include <vector>

#include "dsl_tensor_locality.h"
#include "dsl_memory_behavior.h"
#include "dsl_shape.h"
#include "pu_info.h"

struct DSL_TENSOR_CONTROL_SNAPSHOT {
    PU_Info *pu;
    ST_IDX owner_pu_st;
    BOOL sealed;
    std::vector<DSL_TENSOR_CONTROL_BLOCK> blocks;
    std::vector<DSL_TENSOR_CONTROL_POSITION> positions;
};

struct DSL_TENSOR_LOCALITY_ANALYSIS {
    PU_Info *pu;
    ST_IDX owner_pu_st;
    const DSL_TENSOR_ANALYSIS *tensor_analysis;
    const DSL_TENSOR_CONTROL_SNAPSHOT *snapshot;
    std::vector<DSL_TENSOR_LOCALITY_FACT_RECORD> facts;
    std::vector<DSL_TENSOR_LOCALITY_USE_RECORD> uses;
};

static const char *DSL_tensor_size_state_name[] = {
    "unknown", "static", "symbolic", "overflow"
};

static const char *DSL_tensor_lifetime_state_name[] = {
    "unknown", "exact_block", "dominated", "branch", "loop",
    "region", "effect", "alias"
};

static const char *DSL_tensor_distance_state_name[] = {
    "unknown", "exact", "conservative"
};

static const char *DSL_tensor_access_pattern_name[] = {
    "unknown", "elementwise", "contraction", "reduction", "view",
    "indexed", "mixed"
};

static const char *DSL_tensor_residency_benefit_name[] = {
    "unknown", "none", "low", "medium", "high"
};

static const char *DSL_tensor_critical_path_state_name[] = {
    "unknown", "off", "on"
};

static const char *DSL_tensor_alias_state_name[] = {
    "unknown", "conservative", "proven_unique"
};

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

DSL_TENSOR_CONTROL_SNAPSHOT *
DSL_Tensor_Control_Snapshot_Create (PU_Info *pu, FILE *diagnostic)
{
    if (pu == NULL || Current_PU_Info != pu ||
        !DSL_Tensor_Locality_Owner_Valid(PU_Info_proc_sym(pu))) {
        DSL_Tensor_Locality_Report
            (diagnostic, "invalid active program unit", 0);
        return NULL;
    }
    DSL_TENSOR_CONTROL_SNAPSHOT *snapshot =
        new DSL_TENSOR_CONTROL_SNAPSHOT;
    snapshot->pu = pu;
    snapshot->owner_pu_st = PU_Info_proc_sym(pu);
    snapshot->sealed = FALSE;
    return snapshot;
}

void
DSL_Tensor_Control_Snapshot_Destroy
        (DSL_TENSOR_CONTROL_SNAPSHOT *snapshot)
{
    delete snapshot;
}

BOOL
DSL_Tensor_Control_Snapshot_Add_Block
        (DSL_TENSOR_CONTROL_SNAPSHOT *snapshot,
         const DSL_TENSOR_CONTROL_BLOCK *block, FILE *diagnostic)
{
    if (!DSL_Tensor_Control_Active(snapshot) || snapshot->sealed ||
        block == NULL || block->block_id == 0 ||
        block->reverse_postorder == 0 || block->reserved != 0 ||
        (block->flags & ~(DSL_TENSOR_CONTROL_BRANCH |
                          DSL_TENSOR_CONTROL_LOOP |
                          DSL_TENSOR_CONTROL_REGION |
                          DSL_TENSOR_CONTROL_EFFECT_BARRIER)) != 0)
        return DSL_Tensor_Locality_Report
                   (diagnostic, "invalid control block", block == NULL ?
                    0 : block->block_id);
    if (DSL_Tensor_Control_Find_Block(snapshot, block->block_id) != NULL)
        return DSL_Tensor_Locality_Report
                   (diagnostic, "duplicate control block", block->block_id);
    snapshot->blocks.push_back(*block);
    return TRUE;
}

BOOL
DSL_Tensor_Control_Snapshot_Add_Position
        (DSL_TENSOR_CONTROL_SNAPSHOT *snapshot,
         const DSL_TENSOR_CONTROL_POSITION *position, FILE *diagnostic)
{
    DSL_IR_NODE_RECORD node;
    if (!DSL_Tensor_Control_Active(snapshot) || snapshot->sealed ||
        position == NULL || position->node_id == DSL_IR_NODE_INVALID_ID ||
        position->block_id == 0 || position->statement_order == 0 ||
        position->reverse_postorder == 0 || position->reserved != 0 ||
        !DSL_IR_Image_Get_Node(position->node_id, &node) ||
        (node.flags & DSL_IR_NODE_FLAG_RETIRED) != 0)
        return DSL_Tensor_Locality_Report
                   (diagnostic, "invalid control position",
                    position == NULL ? 0 : position->node_id);
    if (DSL_Tensor_Control_Find_Position(snapshot, position->node_id) != NULL)
        return DSL_Tensor_Locality_Report
                   (diagnostic, "duplicate control position",
                    position->node_id);
    snapshot->positions.push_back(*position);
    return TRUE;
}

BOOL
DSL_Tensor_Control_Snapshot_Verify
        (const DSL_TENSOR_CONTROL_SNAPSHOT *snapshot, FILE *diagnostic)
{
    if (!DSL_Tensor_Control_Active(snapshot))
        return DSL_Tensor_Locality_Report
                   (diagnostic, "control snapshot is not active", 0);
    for (UINT32 i = 0; i < snapshot->blocks.size(); ++i) {
        const DSL_TENSOR_CONTROL_BLOCK &block = snapshot->blocks[i];
        if (block.block_id == 0 || block.reverse_postorder == 0 ||
            block.reserved != 0 ||
            (block.immediate_dominator != 0 &&
             DSL_Tensor_Control_Find_Block
                 (snapshot, block.immediate_dominator) == NULL) ||
            (block.immediate_postdominator != 0 &&
             DSL_Tensor_Control_Find_Block
                 (snapshot, block.immediate_postdominator) == NULL) ||
            (i != 0 &&
             snapshot->blocks[i - 1].block_id >= block.block_id))
            return DSL_Tensor_Locality_Report
                       (diagnostic, "invalid control block", block.block_id);
        for (UINT32 j = i + 1; j < snapshot->blocks.size(); ++j) {
            if (snapshot->blocks[j].reverse_postorder ==
                    block.reverse_postorder)
                return DSL_Tensor_Locality_Report
                           (diagnostic, "duplicate reverse postorder",
                            block.block_id);
        }
    }
    for (UINT32 i = 0; i < snapshot->positions.size(); ++i) {
        const DSL_TENSOR_CONTROL_POSITION &position = snapshot->positions[i];
        DSL_IR_NODE_RECORD node;
        if (position.node_id == DSL_IR_NODE_INVALID_ID ||
            position.statement_order == 0 ||
            position.reverse_postorder == 0 || position.reserved != 0 ||
            DSL_Tensor_Control_Find_Block
                (snapshot, position.block_id) == NULL ||
            DSL_Tensor_Control_Find_Block
                (snapshot, position.block_id)->reverse_postorder !=
                    position.reverse_postorder ||
            !DSL_IR_Image_Get_Node(position.node_id, &node) ||
            (node.flags & DSL_IR_NODE_FLAG_RETIRED) != 0 ||
            (i != 0 && !DSL_Tensor_Control_Position_Less
                           (snapshot->positions[i - 1], position)))
            return DSL_Tensor_Locality_Report
                       (diagnostic, "invalid control position",
                        position.node_id);
    }
    return TRUE;
}

BOOL
DSL_Tensor_Control_Snapshot_Seal
        (DSL_TENSOR_CONTROL_SNAPSHOT *snapshot, FILE *diagnostic)
{
    if (!DSL_Tensor_Control_Active(snapshot))
        return DSL_Tensor_Locality_Report
                   (diagnostic, "control snapshot is not active", 0);
    if (!snapshot->sealed) {
        std::sort(snapshot->blocks.begin(), snapshot->blocks.end(),
                  DSL_Tensor_Control_Block_Less);
        std::sort(snapshot->positions.begin(), snapshot->positions.end(),
                  DSL_Tensor_Control_Position_Less);
        snapshot->sealed = TRUE;
    }
    return DSL_Tensor_Control_Snapshot_Verify(snapshot, diagnostic);
}

void
DSL_Tensor_Control_Snapshot_Print
        (FILE *file, const DSL_TENSOR_CONTROL_SNAPSHOT *snapshot)
{
    if (file == NULL || snapshot == NULL)
        return;
    fprintf(file, "DSLTensorControlSnapshot: owner=<%u,%u,%s> "
                  "blocks=%u positions=%u sealed=%s\n",
            ST_IDX_level(snapshot->owner_pu_st),
            ST_IDX_index(snapshot->owner_pu_st),
            DSL_Tensor_Locality_Owner_Valid(snapshot->owner_pu_st) ?
                ST_name(St_Table[snapshot->owner_pu_st]) : "<invalid>",
            (UINT32)snapshot->blocks.size(),
            (UINT32)snapshot->positions.size(),
            snapshot->sealed ? "yes" : "no");
    for (UINT32 i = 0; i < snapshot->blocks.size(); ++i) {
        const DSL_TENSOR_CONTROL_BLOCK &block = snapshot->blocks[i];
        fprintf(file, "  block[%u] rpo=%u idom=%u ipdom=%u loop=%u "
                      "region=%u flags=0x%x\n",
                block.block_id, block.reverse_postorder,
                block.immediate_dominator,
                block.immediate_postdominator, block.loop_depth,
                block.region_id, block.flags);
    }
    for (UINT32 i = 0; i < snapshot->positions.size(); ++i) {
        const DSL_TENSOR_CONTROL_POSITION &position = snapshot->positions[i];
        fprintf(file, "  position[%u] node=%u block=%u rpo=%u "
                      "statement=%u\n",
                i + 1, position.node_id, position.block_id,
                position.reverse_postorder, position.statement_order);
    }
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

DSL_TENSOR_LOCALITY_ANALYSIS *
DSL_Tensor_Locality_Create
        (PU_Info *pu, const DSL_TENSOR_ANALYSIS *tensor_analysis,
         const DSL_TENSOR_CONTROL_SNAPSHOT *snapshot, FILE *diagnostic)
{
    if (pu == NULL || tensor_analysis == NULL || snapshot == NULL ||
        Current_PU_Info != pu || snapshot->pu != pu || !snapshot->sealed ||
        !DSL_Tensor_Analysis_Verify(tensor_analysis, diagnostic) ||
        !DSL_Tensor_Control_Snapshot_Verify(snapshot, diagnostic)) {
        DSL_Tensor_Locality_Report
            (diagnostic, "invalid per-PU analysis input", 0);
        return NULL;
    }
    DSL_TENSOR_LOCALITY_ANALYSIS *analysis =
        new DSL_TENSOR_LOCALITY_ANALYSIS;
    analysis->pu = pu;
    analysis->owner_pu_st = PU_Info_proc_sym(pu);
    analysis->tensor_analysis = tensor_analysis;
    analysis->snapshot = snapshot;
    return analysis;
}

void
DSL_Tensor_Locality_Destroy (DSL_TENSOR_LOCALITY_ANALYSIS *analysis)
{
    delete analysis;
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
        DSL_Tensor_Analysis_Get_Use
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
DSL_Tensor_Locality_Build
        (DSL_TENSOR_LOCALITY_ANALYSIS *analysis, FILE *diagnostic)
{
    if (!DSL_Tensor_Locality_Active(analysis) ||
        !analysis->snapshot->sealed)
        return DSL_Tensor_Locality_Report
                   (diagnostic, "analysis is not active", 0);
    if (!analysis->facts.empty())
        return DSL_Tensor_Locality_Verify(analysis, diagnostic);

    for (DSL_TENSOR_FACT_ID tensor_id = 1;
         tensor_id <= DSL_Tensor_Analysis_Fact_Count
                          (analysis->tensor_analysis);
         ++tensor_id) {
        DSL_TENSOR_FACT_RECORD tensor;
        DSL_TENSOR_LOCALITY_FACT_RECORD fact;
        std::vector<DSL_TENSOR_LOCALITY_USE_RECORD> uses;
        if (!DSL_Tensor_Analysis_Get_Fact
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
            DSL_Tensor_Analysis_Get_Use
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
        DSL_Tensor_Analysis_Get_Fact
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
    return DSL_Tensor_Locality_Verify(analysis, diagnostic);
}

BOOL
DSL_Tensor_Locality_Verify
        (const DSL_TENSOR_LOCALITY_ANALYSIS *analysis, FILE *diagnostic)
{
    if (!DSL_Tensor_Locality_Active(analysis) ||
        !DSL_Tensor_Control_Snapshot_Verify
             (analysis->snapshot, diagnostic) ||
        !DSL_Tensor_Analysis_Verify
             (analysis->tensor_analysis, diagnostic))
        return DSL_Tensor_Locality_Report
                   (diagnostic, "invalid analysis context", 0);
    UINT32 expected_use = 1;
    for (UINT32 i = 0; i < analysis->facts.size(); ++i) {
        const DSL_TENSOR_LOCALITY_FACT_RECORD &fact = analysis->facts[i];
        DSL_TENSOR_FACT_RECORD tensor;
        if (fact.id != i + 1 || fact.tensor_fact_id == 0 ||
            !DSL_Tensor_Analysis_Get_Fact
                 (analysis->tensor_analysis, fact.tensor_fact_id, &tensor) ||
            fact.value_id != tensor.value_id ||
            fact.producer_node_id != tensor.producer_node_id ||
            fact.use_count != tensor.use_count || fact.reserved != 0 ||
            (fact.use_count == 0 && fact.first_use_id != 0) ||
            (fact.use_count != 0 && fact.first_use_id != expected_use) ||
            fact.size_state > DSL_TENSOR_SIZE_OVERFLOW ||
            fact.lifetime_state > DSL_TENSOR_LIFETIME_ALIAS ||
            fact.reuse_distance_state >
                DSL_TENSOR_DISTANCE_CONSERVATIVE ||
            fact.access_pattern > DSL_TENSOR_ACCESS_MIXED ||
            fact.residency_benefit > DSL_TENSOR_RESIDENCY_HIGH ||
            fact.critical_path_state > DSL_TENSOR_CRITICAL_PATH_ON ||
            fact.alias_state > DSL_TENSOR_ALIAS_PROVEN_UNIQUE ||
            (fact.size_state == DSL_TENSOR_SIZE_STATIC &&
             fact.object_bytes == DSL_TENSOR_LOCALITY_UNKNOWN_U64) ||
            (fact.reuse_distance_state == DSL_TENSOR_DISTANCE_EXACT &&
             fact.reuse_distance_statements ==
                 DSL_TENSOR_LOCALITY_UNKNOWN_U64))
            return DSL_Tensor_Locality_Report
                       (diagnostic, "invalid locality fact", fact.id);
        expected_use += fact.use_count;
    }
    if (expected_use != analysis->uses.size() + 1)
        return DSL_Tensor_Locality_Report
                   (diagnostic, "locality use census mismatch", 0);
    for (UINT32 i = 0; i < analysis->uses.size(); ++i) {
        const DSL_TENSOR_LOCALITY_USE_RECORD &use = analysis->uses[i];
        DSL_TENSOR_USE_FACT_RECORD tensor_use;
        if (use.id != i + 1 || use.locality_fact_id == 0 ||
            use.locality_fact_id > analysis->facts.size() ||
            !DSL_Tensor_Analysis_Get_Use
                 (analysis->tensor_analysis, use.tensor_use_fact_id,
                  &tensor_use) ||
            use.consumer_node_id != tensor_use.consumer_node_id ||
            use.reserved != 0 ||
            (use.block_id == 0) != (use.path_position == 0))
            return DSL_Tensor_Locality_Report
                       (diagnostic, "invalid locality use", use.id);
    }
    return TRUE;
}

void
DSL_Tensor_Locality_Print
        (FILE *file, const DSL_TENSOR_LOCALITY_ANALYSIS *analysis)
{
    if (file == NULL || analysis == NULL)
        return;
    fprintf(file, "DSLTensorLocality: owner=<%u,%u,%s> facts=%u uses=%u\n",
            ST_IDX_level(analysis->owner_pu_st),
            ST_IDX_index(analysis->owner_pu_st),
            DSL_Tensor_Locality_Owner_Valid(analysis->owner_pu_st) ?
                ST_name(St_Table[analysis->owner_pu_st]) : "<invalid>",
            (UINT32)analysis->facts.size(),
            (UINT32)analysis->uses.size());
    for (UINT32 i = 0; i < analysis->facts.size(); ++i) {
        const DSL_TENSOR_LOCALITY_FACT_RECORD &fact = analysis->facts[i];
        char object_bytes[32];
        char distance[32];
        char working_set[32];
        char read_bytes[32];
        char write_bytes[32];
        if (fact.object_bytes == DSL_TENSOR_LOCALITY_UNKNOWN_U64)
            strcpy(object_bytes, "<unknown>");
        else
            snprintf(object_bytes, sizeof(object_bytes), "%llu",
                     (unsigned long long)fact.object_bytes);
        if (fact.reuse_distance_statements ==
                DSL_TENSOR_LOCALITY_UNKNOWN_U64)
            strcpy(distance, "<unknown>");
        else
            snprintf(distance, sizeof(distance), "%llu",
                     (unsigned long long)fact.reuse_distance_statements);
        if (fact.working_set_bytes == DSL_TENSOR_LOCALITY_UNKNOWN_U64)
            strcpy(working_set, "<unknown>");
        else
            snprintf(working_set, sizeof(working_set), "%llu",
                     (unsigned long long)fact.working_set_bytes);
        if (fact.estimated_read_bytes == DSL_TENSOR_LOCALITY_UNKNOWN_U64)
            strcpy(read_bytes, "<unknown>");
        else
            snprintf(read_bytes, sizeof(read_bytes), "%llu",
                     (unsigned long long)fact.estimated_read_bytes);
        if (fact.estimated_write_bytes == DSL_TENSOR_LOCALITY_UNKNOWN_U64)
            strcpy(write_bytes, "<unknown>");
        else
            snprintf(write_bytes, sizeof(write_bytes), "%llu",
                     (unsigned long long)fact.estimated_write_bytes);
        fprintf(file, "  fact[%u] tensor_fact=%u value=%u producer=%u "
                      "last_consumer=%u first_use=%u use_count=%u "
                      "size=%s bytes=%s lifetime=%s distance=%s:%s "
                      "working_set=%s access=%s residency=%s "
                      "critical=%s alias=%s path=%u..%u "
                      "cost_read=%s cost_write=%s\n",
                fact.id, fact.tensor_fact_id, fact.value_id,
                fact.producer_node_id, fact.last_consumer_node_id,
                fact.first_use_id, fact.use_count,
                DSL_Tensor_Size_State_Name(fact.size_state),
                object_bytes,
                DSL_Tensor_Lifetime_State_Name(fact.lifetime_state),
                DSL_Tensor_Distance_State_Name
                    (fact.reuse_distance_state),
                distance, working_set,
                DSL_Tensor_Access_Pattern_Name(fact.access_pattern),
                DSL_Tensor_Residency_Benefit_Name
                    (fact.residency_benefit),
                DSL_Tensor_Critical_Path_State_Name
                    (fact.critical_path_state),
                DSL_Tensor_Alias_State_Name(fact.alias_state),
                fact.producer_path_position,
                fact.last_use_path_position,
                read_bytes, write_bytes);
    }
    for (UINT32 i = 0; i < analysis->uses.size(); ++i) {
        const DSL_TENSOR_LOCALITY_USE_RECORD &use = analysis->uses[i];
        fprintf(file, "  use[%u] fact=%u tensor_use=%u consumer=%u "
                      "block=%u statement=%u path=%u loop=%u region=%u "
                      "control=0x%x\n",
                use.id, use.locality_fact_id, use.tensor_use_fact_id,
                use.consumer_node_id, use.block_id,
                use.statement_order, use.path_position,
                use.loop_depth, use.region_id, use.control_flags);
    }
}

UINT32
DSL_Tensor_Locality_Fact_Count
        (const DSL_TENSOR_LOCALITY_ANALYSIS *analysis)
{
    return analysis == NULL ? 0 : analysis->facts.size();
}

UINT32
DSL_Tensor_Locality_Use_Count
        (const DSL_TENSOR_LOCALITY_ANALYSIS *analysis)
{
    return analysis == NULL ? 0 : analysis->uses.size();
}

BOOL
DSL_Tensor_Locality_Get_Fact
        (const DSL_TENSOR_LOCALITY_ANALYSIS *analysis,
         DSL_TENSOR_LOCALITY_FACT_ID id,
         DSL_TENSOR_LOCALITY_FACT_RECORD *record)
{
    if (analysis == NULL || record == NULL || id == 0 ||
        id > analysis->facts.size())
        return FALSE;
    *record = analysis->facts[id - 1];
    return TRUE;
}

BOOL
DSL_Tensor_Locality_Get_Use
        (const DSL_TENSOR_LOCALITY_ANALYSIS *analysis,
         DSL_TENSOR_LOCALITY_USE_ID id,
         DSL_TENSOR_LOCALITY_USE_RECORD *record)
{
    if (analysis == NULL || record == NULL || id == 0 ||
        id > analysis->uses.size())
        return FALSE;
    *record = analysis->uses[id - 1];
    return TRUE;
}

BOOL
DSL_Tensor_Locality_Find_Fact
        (const DSL_TENSOR_LOCALITY_ANALYSIS *analysis,
         DSL_IR_VALUE_ID value_id,
         DSL_TENSOR_LOCALITY_FACT_RECORD *record)
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

#define DSL_TENSOR_LOCALITY_NAME_FUNCTION(function, table) \
const char *function (UINT32 value) \
{ \
    return value < sizeof(table) / sizeof(table[0]) ? \
           table[value] : "unknown"; \
}

DSL_TENSOR_LOCALITY_NAME_FUNCTION
    (DSL_Tensor_Size_State_Name, DSL_tensor_size_state_name)
DSL_TENSOR_LOCALITY_NAME_FUNCTION
    (DSL_Tensor_Lifetime_State_Name, DSL_tensor_lifetime_state_name)
DSL_TENSOR_LOCALITY_NAME_FUNCTION
    (DSL_Tensor_Distance_State_Name, DSL_tensor_distance_state_name)
DSL_TENSOR_LOCALITY_NAME_FUNCTION
    (DSL_Tensor_Access_Pattern_Name, DSL_tensor_access_pattern_name)
DSL_TENSOR_LOCALITY_NAME_FUNCTION
    (DSL_Tensor_Residency_Benefit_Name,
     DSL_tensor_residency_benefit_name)
DSL_TENSOR_LOCALITY_NAME_FUNCTION
    (DSL_Tensor_Critical_Path_State_Name,
     DSL_tensor_critical_path_state_name)
DSL_TENSOR_LOCALITY_NAME_FUNCTION
    (DSL_Tensor_Alias_State_Name, DSL_tensor_alias_state_name)

#undef DSL_TENSOR_LOCALITY_NAME_FUNCTION
