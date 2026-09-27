/*
 * Copyright (C) 2026 Open64 Project
 */

/*
 * Stores and validates AIO-4 PU-local tensor lifetime, reuse, alias, and
 * locality facts plus a copied control snapshot. VHO derives locality facts;
 * this common service retains no CFG or WOPT pointers. Design:
 * doc/AI-COMPILER-OPTIMIZATION-AIO4-LIFETIME-LOCALITY.md.
 */

#include <algorithm>
#include <string.h>
#include <vector>

#include "dsl_tensor_locality.h"
#include "dsl_tensor_locality_internal.h"
#include "pu_info.h"

static const char *DSL_tensor_size_state_name_table[] = {
    "unknown", "static", "symbolic", "overflow"
};

static const char *DSL_tensor_lifetime_state_name_table[] = {
    "unknown", "exact_block", "dominated", "branch", "loop",
    "region", "effect", "alias"
};

static const char *DSL_tensor_distance_state_name_table[] = {
    "unknown", "exact", "conservative"
};

static const char *DSL_tensor_access_pattern_name_table[] = {
    "unknown", "elementwise", "contraction", "reduction", "view",
    "indexed", "mixed"
};

static const char *DSL_tensor_residency_benefit_name_table[] = {
    "unknown", "none", "low", "medium", "high"
};

static const char *DSL_tensor_critical_path_state_name_table[] = {
    "unknown", "off", "on"
};

static const char *DSL_tensor_alias_state_name_table[] = {
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
DSL_tensor_control_snapshot_create (PU_Info *pu, FILE *diagnostic)
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
DSL_tensor_control_snapshot_destroy
        (DSL_TENSOR_CONTROL_SNAPSHOT *snapshot)
{
    delete snapshot;
}

BOOL
DSL_tensor_control_snapshot_add_block
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
DSL_tensor_control_snapshot_add_position
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
DSL_tensor_control_snapshot_verify
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
DSL_tensor_control_snapshot_seal
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
    return DSL_tensor_control_snapshot_verify(snapshot, diagnostic);
}

void
DSL_tensor_control_snapshot_print
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

DSL_TENSOR_LOCALITY_ANALYSIS *
DSL_tensor_locality_create
        (PU_Info *pu, const DSL_TENSOR_ANALYSIS *tensor_analysis,
         const DSL_TENSOR_CONTROL_SNAPSHOT *snapshot, FILE *diagnostic)
{
    if (pu == NULL || tensor_analysis == NULL || snapshot == NULL ||
        Current_PU_Info != pu || snapshot->pu != pu || !snapshot->sealed ||
        !DSL_tensor_analysis_verify(tensor_analysis, diagnostic) ||
        !DSL_tensor_control_snapshot_verify(snapshot, diagnostic)) {
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
DSL_tensor_locality_destroy (DSL_TENSOR_LOCALITY_ANALYSIS *analysis)
{
    delete analysis;
}

BOOL
DSL_tensor_locality_verify
        (const DSL_TENSOR_LOCALITY_ANALYSIS *analysis, FILE *diagnostic)
{
    if (!DSL_Tensor_Locality_Active(analysis) ||
        !DSL_tensor_control_snapshot_verify
             (analysis->snapshot, diagnostic) ||
        !DSL_tensor_analysis_verify
             (analysis->tensor_analysis, diagnostic))
        return DSL_Tensor_Locality_Report
                   (diagnostic, "invalid analysis context", 0);
    UINT32 expected_use = 1;
    for (UINT32 i = 0; i < analysis->facts.size(); ++i) {
        const DSL_TENSOR_LOCALITY_FACT_RECORD &fact = analysis->facts[i];
        DSL_TENSOR_FACT_RECORD tensor;
        if (fact.id != i + 1 || fact.tensor_fact_id == 0 ||
            !DSL_tensor_analysis_get_fact
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
            !DSL_tensor_analysis_get_use
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
DSL_tensor_locality_print
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
                DSL_tensor_size_state_name(fact.size_state),
                object_bytes,
                DSL_tensor_lifetime_state_name(fact.lifetime_state),
                DSL_tensor_distance_state_name
                    (fact.reuse_distance_state),
                distance, working_set,
                DSL_tensor_access_pattern_name(fact.access_pattern),
                DSL_tensor_residency_benefit_name
                    (fact.residency_benefit),
                DSL_tensor_critical_path_state_name
                    (fact.critical_path_state),
                DSL_tensor_alias_state_name(fact.alias_state),
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
DSL_tensor_locality_fact_count
        (const DSL_TENSOR_LOCALITY_ANALYSIS *analysis)
{
    return analysis == NULL ? 0 : analysis->facts.size();
}

UINT32
DSL_tensor_locality_use_count
        (const DSL_TENSOR_LOCALITY_ANALYSIS *analysis)
{
    return analysis == NULL ? 0 : analysis->uses.size();
}

BOOL
DSL_tensor_locality_get_fact
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
DSL_tensor_locality_get_use
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
DSL_tensor_locality_find_fact
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
    (DSL_tensor_size_state_name, DSL_tensor_size_state_name_table)
DSL_TENSOR_LOCALITY_NAME_FUNCTION
    (DSL_tensor_lifetime_state_name, DSL_tensor_lifetime_state_name_table)
DSL_TENSOR_LOCALITY_NAME_FUNCTION
    (DSL_tensor_distance_state_name, DSL_tensor_distance_state_name_table)
DSL_TENSOR_LOCALITY_NAME_FUNCTION
    (DSL_tensor_access_pattern_name, DSL_tensor_access_pattern_name_table)
DSL_TENSOR_LOCALITY_NAME_FUNCTION
    (DSL_tensor_residency_benefit_name,
     DSL_tensor_residency_benefit_name_table)
DSL_TENSOR_LOCALITY_NAME_FUNCTION
    (DSL_tensor_critical_path_state_name,
     DSL_tensor_critical_path_state_name_table)
DSL_TENSOR_LOCALITY_NAME_FUNCTION
    (DSL_tensor_alias_state_name, DSL_tensor_alias_state_name_table)

#undef DSL_TENSOR_LOCALITY_NAME_FUNCTION
