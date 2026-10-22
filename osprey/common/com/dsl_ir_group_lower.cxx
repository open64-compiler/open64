/*
 * Copyright (C) 2026 Open64 Project
 *
 * Atomic grouped native-value lowering for one active PU. The local contract
 * is doc/WHIRL-DSL-GROUPED-LOWERING-M1.md; the FHE consumer boundary follows
 * doc/FHE-SYNC6-CKKS-ABI-LOWERING-BOUNDARY.md. No FHE policy belongs here.
 */

#include <algorithm>
#include <map>
#include <set>
#include <string>
#include <string.h>
#include <vector>

#include "dsl_memory_behavior.h"
#include "dsl_ir_image.h"
#include "dsl_ir_lower_internal.h"
#include "dsl_region.h"
#include "dsl_region_internal.h"
#include "pu_info.h"
#include "strtab.h"
#include "symtab.h"
#include "wn.h"
#include "wn_util.h"

extern BOOL DSL_IR_Image_Mark_Value_Lowered (DSL_IR_VALUE_ID);

typedef struct {
    WN *containing_block;
    UINT32 tree_order;
    UINT32 subtree_end_order;
    UINT32 block_ordinal;
} DSL_IR_GROUP_TREE_POSITION;

/* One preorder walk indexes both direct BLOCK membership and WN presence. */
static BOOL
DSL_IR_Group_Index_Tree
        (WN *tree, WN *containing_block, UINT32 block_ordinal,
         UINT32 *next_order,
         std::map<const WN *, DSL_IR_GROUP_TREE_POSITION> *positions,
         std::set<ST_IDX> *physical_symbols)
{
    if (tree == NULL || *next_order == UINT32_MAX)
        return FALSE;
    DSL_IR_GROUP_TREE_POSITION position;
    position.containing_block = containing_block;
    position.tree_order = (*next_order)++;
    position.block_ordinal = block_ordinal;
    if (!positions->insert(std::make_pair((const WN *)tree, position)).second)
        return FALSE;
    if (WN_has_sym(tree))
        physical_symbols->insert(WN_st_idx(tree));
    if (WN_operator(tree) == OPR_BLOCK) {
        UINT32 ordinal = 0;
        for (WN *stmt = WN_first(tree); stmt != NULL;
             stmt = WN_next(stmt)) {
            if (WN_operator(stmt) != OPR_COMMENT)
                ++ordinal;
            if (!DSL_IR_Group_Index_Tree
                    (stmt, tree, ordinal, next_order, positions,
                     physical_symbols))
                return FALSE;
        }
        (*positions)[tree].subtree_end_order = *next_order;
        return TRUE;
    }
    for (INT32 kid = 0; kid < WN_kid_count(tree); ++kid) {
        WN *child = WN_kid(tree, kid);
        if (child != NULL && !DSL_IR_Group_Index_Tree
                                  (child, containing_block, block_ordinal,
                                   next_order, positions,
                                   physical_symbols))
            return FALSE;
    }
    (*positions)[tree].subtree_end_order = *next_order;
    return TRUE;
}

typedef std::pair<UINT32, UINT32> DSL_IR_GROUP_USE_EDGE;

/* A second linear PU walk records only uses of the transaction's symbols. */
static BOOL
DSL_IR_Group_Index_Physical_Uses
        (const WN *tree, UINT32 enclosing_member,
         const std::map<const WN *, UINT32> &member_by_definition,
         const std::map<ST_IDX, UINT32> &member_by_st,
         std::vector<UINT32> *definition_counts,
         std::vector<DSL_IR_GROUP_USE_EDGE> *edges)
{
    if (tree == NULL)
        return TRUE;
    std::map<const WN *, UINT32>::const_iterator definition =
        member_by_definition.find(tree);
    if (definition != member_by_definition.end()) {
        enclosing_member = definition->second;
        ++(*definition_counts)[enclosing_member];
    }
    if (WN_has_sym(tree)) {
        std::map<ST_IDX, UINT32>::const_iterator symbol =
            member_by_st.find(WN_st_idx(tree));
        if (symbol != member_by_st.end() &&
            (definition == member_by_definition.end() ||
             definition->second != symbol->second)) {
            if (WN_operator(tree) != OPR_LDID ||
                enclosing_member == UINT32_MAX)
                return FALSE;
            edges->push_back
                (std::make_pair(symbol->second, enclosing_member));
        }
    }
    if (WN_operator(tree) == OPR_BLOCK) {
        for (const WN *stmt = WN_first(tree); stmt != NULL;
             stmt = WN_next(stmt)) {
            if (!DSL_IR_Group_Index_Physical_Uses
                    (stmt, enclosing_member, member_by_definition,
                     member_by_st, definition_counts, edges))
                return FALSE;
        }
        return TRUE;
    }
    for (INT32 kid = 0; kid < WN_kid_count(tree); ++kid) {
        if (!DSL_IR_Group_Index_Physical_Uses
                (WN_kid(tree, kid), enclosing_member,
                 member_by_definition, member_by_st,
                 definition_counts, edges))
            return FALSE;
    }
    return TRUE;
}

typedef struct {
    const DSL_IR_NATIVE_GROUP_LOWER_REQUEST *request;
    UINT32 request_index;
    UINT32 first_member;
    UINT32 member_count;
    UINT32 first_order;
    UINT32 last_order;
    UINT32 statement_count;
    WN *containing_block;
    WN *first_definition;
    WN *output_definition;
    DSL_IR_VALUE_RECORD semantic_source;
    DSL_IR_VALUE_RECORD output;
    ST_IDX handle_st;
    TY_IDX handle_ty;
} DSL_IR_GROUP_LOWER_JOURNAL;

struct DSL_IR_GROUP_LOWER_LESS {
    BOOL operator()
        (const DSL_IR_GROUP_LOWER_JOURNAL &left,
         const DSL_IR_GROUP_LOWER_JOURNAL &right) const
    {
        return left.first_order < right.first_order;
    }
};

/* A group is closed only when every private producer contributes to output. */
static BOOL
DSL_IR_Group_Lower_Reaches_Output
        (const DSL_IR_GROUP_LOWER_JOURNAL &group,
         const std::vector<DSL_IR_NATIVE_VALUE_LOWER_JOURNAL> &members,
         const std::map<DSL_IR_VALUE_ID, UINT32> &member_by_value)
{
    std::vector<UINT8> reached(group.member_count, 0);
    std::vector<UINT32> worklist;
    for (UINT32 i = 0; i < group.member_count; ++i) {
        if (members[group.first_member + i].value.id == group.output.id) {
            reached[i] = 1;
            worklist.push_back(i);
            break;
        }
    }
    if (worklist.empty())
        return FALSE;
    while (!worklist.empty()) {
        UINT32 current = worklist.back();
        worklist.pop_back();
        const DSL_IR_NODE_RECORD &node =
            members[group.first_member + current].node;
        for (UINT32 kid = 0; kid < node.operand_count; ++kid) {
            DSL_IR_VALUE_REFERENCE_RECORD reference;
            if (!DSL_IR_Image_Get_Value_Reference
                    (node.first_operand_reference_id + kid, &reference))
                return FALSE;
            std::map<DSL_IR_VALUE_ID, UINT32>::const_iterator found =
                member_by_value.find(reference.value_id);
            if (found != member_by_value.end() &&
                found->second >= group.first_member &&
                found->second < group.first_member + group.member_count) {
                UINT32 i = found->second - group.first_member;
                if (reached[i] == 0) {
                    reached[i] = 1;
                    worklist.push_back(i);
                }
            }
        }
    }
    for (UINT32 i = 0; i < group.member_count; ++i) {
        if (reached[i] == 0)
            return FALSE;
    }
    return TRUE;
}

/* Between the first and last member, only inert comments may be retained. */
static BOOL
DSL_IR_Group_Lower_Statements_Closed
        (const DSL_IR_GROUP_LOWER_JOURNAL &group,
         const std::vector<DSL_IR_NATIVE_VALUE_LOWER_JOURNAL> &members)
{
    UINT32 first = UINT32_MAX;
    UINT32 last = 0;
    UINT32 output = UINT32_MAX;
    for (UINT32 i = 0; i < group.member_count; ++i) {
        const DSL_IR_NATIVE_VALUE_LOWER_JOURNAL &member =
            members[group.first_member + i];
        if (member.block_ordinal < first)
            first = member.block_ordinal;
        if (member.block_ordinal > last)
            last = member.block_ordinal;
        if (member.request->native_definition == group.output_definition)
            output = member.block_ordinal;
    }
    return first != UINT32_MAX && output == last &&
           last - first + 1 == group.member_count;
}

typedef struct {
    UINT32 projection_count;
    UINT32 input_count;
    UINT32 call_count;
    UINT32 call_argument_count;
    UINT32 effect_count;
    UINT32 region_count;
    DSL_RUNTIME_VALUE_PROJECTION_RECORD projection;
} DSL_IR_GROUP_MEMBER_USES;

/* Census every managed relation once, keyed by exact value, node, or ST. */
static BOOL
DSL_IR_Group_Index_Managed_Uses
        (PU_Info *pu_info,
         const std::map<DSL_IR_VALUE_ID, UINT32> &member_by_value,
         const std::map<DSL_IR_NODE_ID, UINT32> &member_by_node,
         const std::map<ST_IDX, UINT32> &member_by_st,
         std::vector<DSL_IR_GROUP_MEMBER_USES> *uses,
         std::vector<std::vector<UINT32> > *member_calls,
         std::vector<std::vector<UINT32> > *member_arguments,
         std::map<DSL_IR_GROUP_USE_EDGE, UINT32> *call_by_actual,
         std::map<DSL_IR_GROUP_USE_EDGE, UINT32> *argument_by_actual,
         std::set<DSL_CALL_ARGUMENT_ID> *retired_arguments,
         std::set<DSL_IR_NODE_ID> *effect_nodes)
{
    for (UINT32 id = 1; id <= DSL_Runtime_Interface_Image_Value_Count();
         ++id) {
        DSL_RUNTIME_VALUE_PROJECTION_RECORD projection;
        if (!DSL_Runtime_Interface_Image_Get_Value(id, &projection))
            return FALSE;
        std::map<DSL_IR_VALUE_ID, UINT32>::const_iterator member =
            member_by_value.find(projection.source_value_id);
        if (member != member_by_value.end()) {
            DSL_IR_GROUP_MEMBER_USES &use = (*uses)[member->second];
            ++use.projection_count;
            use.projection = projection;
        }
    }
    for (UINT32 id = 1;
         id <= DSL_Program_Interface_Image_Runtime_Input_Count(); ++id) {
        DSL_RUNTIME_INPUT_RECORD input;
        if (!DSL_Program_Interface_Image_Get_Runtime_Input(id, &input))
            return FALSE;
        std::map<DSL_IR_VALUE_ID, UINT32>::const_iterator member =
            member_by_value.find(input.source_value_id);
        if (member != member_by_value.end())
            ++(*uses)[member->second].input_count;
    }
    for (UINT32 id = 1; id <= DSL_Runtime_Interface_Image_Call_Count();
         ++id) {
        DSL_RUNTIME_CALL_PROJECTION_RECORD call;
        if (!DSL_Runtime_Interface_Image_Get_Call(id, &call))
            return FALSE;
        if (!call_by_actual->insert
                (std::make_pair
                    (std::make_pair(call.callsite_id,
                                    call.actual_ordinal), id)).second)
            return FALSE;
        std::map<DSL_IR_VALUE_ID, UINT32>::const_iterator member =
            member_by_value.find(call.source_value_id);
        if (member != member_by_value.end()) {
            ++(*uses)[member->second].call_count;
            (*member_calls)[member->second].push_back(id);
        }
    }
    for (UINT32 id = 1; id <= DSL_Call_ABI_Image_Argument_Count(); ++id) {
        DSL_CALL_ARGUMENT_RECORD argument;
        if (!DSL_Call_ABI_Image_Get_Argument(id, &argument))
            return FALSE;
        if (!argument_by_actual->insert
                (std::make_pair
                    (std::make_pair(argument.callsite_id,
                                    argument.actual_ordinal), id)).second)
            return FALSE;
        std::map<DSL_IR_VALUE_ID, UINT32>::const_iterator member =
            member_by_value.find(argument.argument_value_id);
        if (member != member_by_value.end()) {
            ++(*uses)[member->second].call_argument_count;
            (*member_arguments)[member->second].push_back(id);
        }
    }
    for (UINT32 id = 1;
         id <= DSL_Program_Interface_Image_Retired_Call_Count(); ++id) {
        DSL_RETIRED_CALL_ARGUMENT_RECORD retired;
        if (!DSL_Program_Interface_Image_Get_Retired_Call(id, &retired))
            return FALSE;
        retired_arguments->insert(retired.call_argument_id);
    }
    for (UINT32 id = 1; id <= DSL_Effect_Image_State_Effect_Count(); ++id) {
        DSL_STATE_EFFECT_RECORD effect;
        if (!DSL_Effect_Image_Get_State_Effect(id, &effect))
            return FALSE;
        effect_nodes->insert(effect.owner_node_id);
        std::map<DSL_IR_NODE_ID, UINT32>::const_iterator member =
            member_by_node.find(effect.owner_node_id);
        if (member != member_by_node.end())
            ++(*uses)[member->second].effect_count;
    }
    for (UINT32 id = 1; id <= DSL_Region_Interface_Count(pu_info); ++id) {
        DSL_REGION_INTERFACE_RECORD interface_record;
        if (!DSL_Region_Get_Interface(pu_info, id, &interface_record))
            return FALSE;
        std::map<ST_IDX, UINT32>::const_iterator member =
            member_by_st.find(interface_record.st);
        if (member != member_by_st.end())
            ++(*uses)[member->second].region_count;
    }
    return TRUE;
}

/* A final output may cross a rewritten call only through its own handle. */
static BOOL
DSL_IR_Group_Output_Calls_Valid
        (ST_IDX owner_pu_st, const DSL_IR_VALUE_RECORD &output,
         DSL_RUNTIME_VALUE_PROJECTION_ID projection_id,
         const std::vector<UINT32> &call_ids,
         const std::vector<UINT32> &argument_ids,
         const std::map<DSL_IR_GROUP_USE_EDGE, UINT32> &call_by_actual,
         const std::map<DSL_IR_GROUP_USE_EDGE, UINT32> &argument_by_actual,
         const std::set<DSL_CALL_ARGUMENT_ID> &retired_arguments)
{
    for (UINT32 i = 0; i < call_ids.size(); ++i) {
        DSL_RUNTIME_CALL_PROJECTION_RECORD call;
        DSL_CALLSITE_METADATA_RECORD callsite;
        if (!DSL_Runtime_Interface_Image_Get_Call(call_ids[i], &call) ||
            call.owner_pu_st != owner_pu_st ||
            call.source_value_id != output.id ||
            call.value_projection_id != projection_id ||
            !DSL_Call_Image_Get_Callsite(call.callsite_id, &callsite) ||
            callsite.owner_pu_st != owner_pu_st)
            return FALSE;
        if (call.direction == DSL_RUNTIME_CALL_INPUT) {
            DSL_CALL_ARGUMENT_RECORD argument;
            std::map<DSL_IR_GROUP_USE_EDGE, UINT32>::const_iterator found =
                argument_by_actual.find
                    (std::make_pair(call.callsite_id, call.actual_ordinal));
            if (found == argument_by_actual.end() ||
                !DSL_Call_ABI_Image_Get_Argument(found->second, &argument) ||
                argument.argument_value_id != output.id ||
                argument.callee_formal_ordinal !=
                    call.callee_formal_ordinal ||
                retired_arguments.find(argument.id) !=
                    retired_arguments.end())
                return FALSE;
        }
    }
    for (UINT32 i = 0; i < argument_ids.size(); ++i) {
        DSL_CALL_ARGUMENT_RECORD argument;
        DSL_CALLSITE_METADATA_RECORD callsite;
        DSL_RUNTIME_CALL_PROJECTION_RECORD call;
        if (!DSL_Call_ABI_Image_Get_Argument(argument_ids[i], &argument) ||
            argument.argument_value_id != output.id ||
            !DSL_Call_Image_Get_Callsite(argument.callsite_id, &callsite) ||
            callsite.owner_pu_st != owner_pu_st)
            return FALSE;
        BOOL retired = retired_arguments.find(argument.id) !=
                           retired_arguments.end();
        std::map<DSL_IR_GROUP_USE_EDGE, UINT32>::const_iterator found =
            call_by_actual.find
                (std::make_pair(argument.callsite_id,
                                argument.actual_ordinal));
        BOOL projected = found != call_by_actual.end() &&
            DSL_Runtime_Interface_Image_Get_Call(found->second, &call);
        if (retired ? projected :
            (!projected || call.direction != DSL_RUNTIME_CALL_INPUT ||
             call.source_value_id != output.id ||
             call.value_projection_id != projection_id ||
             call.callee_formal_ordinal != argument.callee_formal_ordinal))
            return FALSE;
    }
    return TRUE;
}

/* Detached replacement blocks cannot read any retiring native symbol. */
static BOOL
DSL_IR_Group_Block_Uses_Member
        (const WN *tree, const std::map<ST_IDX, UINT32> &member_by_st)
{
    if (tree == NULL)
        return FALSE;
    if (WN_has_sym(tree) &&
        member_by_st.find(WN_st_idx(tree)) != member_by_st.end())
        return TRUE;
    if (WN_operator(tree) == OPR_BLOCK) {
        for (const WN *stmt = WN_first(tree); stmt != NULL;
             stmt = WN_next(stmt)) {
            if (DSL_IR_Group_Block_Uses_Member(stmt, member_by_st))
                return TRUE;
        }
        return FALSE;
    }
    for (INT32 kid = 0; kid < WN_kid_count(tree); ++kid) {
        if (DSL_IR_Group_Block_Uses_Member
                (WN_kid(tree, kid), member_by_st))
            return TRUE;
    }
    return FALSE;
}

typedef struct {
    ST_IDX replacement_st;
    UINT32 group_index;
    std::vector<WN *> reads;
} DSL_IR_GROUP_RETIRED_ALIAS;

/* A detached block must not hide a read of a retired projection handle. */
static BOOL
DSL_IR_Group_Block_Uses_Alias
        (const WN *tree,
         const std::map<ST_IDX, DSL_IR_GROUP_RETIRED_ALIAS> &aliases)
{
    if (tree == NULL)
        return FALSE;
    if (WN_has_sym(tree) &&
        aliases.find(WN_st_idx(tree)) != aliases.end())
        return TRUE;
    if (WN_operator(tree) == OPR_BLOCK) {
        for (const WN *stmt = WN_first(tree); stmt != NULL;
             stmt = WN_next(stmt))
            if (DSL_IR_Group_Block_Uses_Alias(stmt, aliases))
                return TRUE;
        return FALSE;
    }
    for (INT32 kid = 0; kid < WN_kid_count(tree); ++kid)
        if (DSL_IR_Group_Block_Uses_Alias(WN_kid(tree, kid), aliases))
            return TRUE;
    return FALSE;
}

/*
 * Replace complete, pure, owner-local native groups with one checked standard
 * block apiece. This is the common/com side of
 * doc/FHE-SYNC6-CKKS-ABI-LOWERING-BOUNDARY.md. All caller arrays and detached
 * blocks remain owned by the caller on rejection; a successful commit
 * consumes each detached block. No fallible work follows full preflight.
 */
BOOL
DSL_IR_Lower_Native_Value_Groups_To_Standard_Blocks
        (PU_Info *pu_info,
         const DSL_IR_NATIVE_GROUP_LOWER_REQUEST *requests,
         UINT32 request_count,
         FILE *diagnostic,
         DSL_IR_NATIVE_GROUP_LOWER_RESULT *results)
{
    WN *pu_root = pu_info == NULL ? NULL : PU_Info_tree_ptr(pu_info);
    ST_IDX owner_pu_st = pu_info == NULL ? ST_IDX_ZERO :
        PU_Info_proc_sym(pu_info);
    if (!DSL_IR_Lower_Current_PU_Is(pu_info) || pu_root == NULL ||
        requests == NULL || request_count == 0 || results == NULL ||
        !DSL_IR_Image_Validate(diagnostic) ||
        !DSL_Runtime_Interface_Image_Validate(diagnostic) ||
        !DSL_Program_Interface_Image_Validate(diagnostic) ||
        !DSL_Call_ABI_Image_Validate_PU(pu_info, diagnostic) ||
        !DSL_Program_Interface_Validate_PU(pu_info, diagnostic) ||
        !DSL_Region_Verify_PU(pu_info, diagnostic))
        return DSL_IR_Lower_Report(diagnostic, 0,
                                   "group transaction is incomplete");

    UINT32 member_count = 0;
    for (UINT32 i = 0; i < request_count; ++i) {
        if (requests[i].member_count == 0 || requests[i].members == NULL ||
            requests[i].member_count > UINT32_MAX - member_count)
            return DSL_IR_Lower_Report(diagnostic, i,
                                       "group member array is invalid");
        member_count += requests[i].member_count;
    }
    std::vector<DSL_IR_NATIVE_VALUE_LOWER_REQUEST> member_requests
        (member_count);
    std::map<const WN *, DSL_IR_GROUP_TREE_POSITION> tree_positions;
    std::set<ST_IDX> physical_symbols;
    UINT32 next_order = 0;
    if (!DSL_IR_Group_Index_Tree
            (pu_root, NULL, 0, &next_order, &tree_positions,
             &physical_symbols))
        return DSL_IR_Lower_Report(diagnostic, 0,
                                   "PU tree indexing failed");
    std::string owner_metadata("owner_pu=");
    owner_metadata += ST_name(St_Table[owner_pu_st]);
    std::map<ST_IDX, DSL_IR_VALUE_RECORD> owner_values;
    for (UINT32 id = 1; id <= DSL_IR_Image_Value_Count(); ++id) {
        DSL_IR_VALUE_RECORD value;
        if (!DSL_IR_Image_Get_Value(id, &value))
            return DSL_IR_Lower_Report
                       (diagnostic, 0, "value indexing failed");
        if (value.metadata == STR_IDX_ZERO ||
            owner_metadata != Index_To_Str(value.metadata))
            continue;
        if (!owner_values.insert(std::make_pair(value.st, value)).second)
            return DSL_IR_Lower_Report
                       (diagnostic, 0, "ambiguous owner-local value");
    }
    std::vector<DSL_IR_NATIVE_VALUE_LOWER_JOURNAL> members;
    std::vector<DSL_IR_GROUP_LOWER_JOURNAL> groups;
    std::vector<UINT32> member_groups;
    std::map<DSL_IR_VALUE_ID, UINT32> member_by_value;
    std::map<DSL_IR_NODE_ID, UINT32> member_by_node;
    std::map<ST_IDX, UINT32> member_by_st;
    std::map<const WN *, UINT32> member_by_definition;
    std::set<const WN *> detached_blocks;
    members.reserve(member_count);
    groups.reserve(request_count);
    member_groups.reserve(member_count);

    for (UINT32 i = 0; i < request_count; ++i) {
        const DSL_IR_NATIVE_GROUP_LOWER_REQUEST &request = requests[i];
        DSL_IR_GROUP_LOWER_JOURNAL group;
        memset(&group, 0, sizeof(group));
        group.request = &request;
        group.request_index = i;
        group.first_member = members.size();
        group.member_count = request.member_count;
        group.first_order = UINT32_MAX;
        std::map<ST_IDX, DSL_IR_VALUE_RECORD>::const_iterator
            observed_source;
        if (request.owner_pu_st != owner_pu_st ||
            !DSL_IR_Image_Get_Value
                (request.semantic_source_value_id,
                 &group.semantic_source) ||
            group.semantic_source.id == request.group_output_value_id ||
            (group.semantic_source.flags &
             (DSL_IR_VALUE_FLAG_REDIRECTED |
              DSL_IR_VALUE_FLAG_DEAD_ELIDED)) != 0 ||
            ST_IDX_level(group.semantic_source.st) != CURRENT_SYMTAB ||
            ST_IDX_index(group.semantic_source.st) == 0 ||
            ST_IDX_index(group.semantic_source.st) >=
                ST_Table_Size(CURRENT_SYMTAB) ||
            (observed_source = owner_values.find
                (group.semantic_source.st)) == owner_values.end() ||
            observed_source->second.id != group.semantic_source.id ||
            group.semantic_source.name == STR_IDX_ZERO ||
            strcmp(Index_To_Str(group.semantic_source.name),
                   ST_name(St_Table[group.semantic_source.st])) != 0)
            return DSL_IR_Lower_Report
                       (diagnostic, i, "semantic source owner mismatch");
        for (UINT32 j = 0; j < request.member_count; ++j) {
            const DSL_IR_NATIVE_GROUP_MEMBER_REQUEST &selected =
                request.members[j];
            UINT32 index = members.size();
            DSL_IR_NATIVE_VALUE_LOWER_REQUEST &fake =
                member_requests[index];
            memset(&fake, 0, sizeof(fake));
            fake.native_definition = selected.native_definition;
            fake.source_value_id = selected.value_id;
            fake.expected_operator = selected.expected_operator;
            fake.expected_version = selected.expected_version;
            fake.mode = DSL_IR_NATIVE_LOWER_COMPUTED_STANDARD_BLOCK;
            DSL_IR_NATIVE_VALUE_LOWER_JOURNAL member;
            memset(&member, 0, sizeof(member));
            member.request = &fake;
            member.request_index = i;
            std::map<const WN *, DSL_IR_GROUP_TREE_POSITION>::const_iterator
                tree_position = tree_positions.find
                                    (selected.native_definition);
            member.containing_block =
                tree_position == tree_positions.end() ? NULL :
                tree_position->second.containing_block;
            DSL_LOGICAL_OPCODE logical_opcode;
            WN *expression =
                selected.native_definition != NULL &&
                WN_operator(selected.native_definition) == OPR_STID &&
                WN_kid_count(selected.native_definition) == 1 ?
                WN_kid0(selected.native_definition) : NULL;
            if (selected.reserved != 0 ||
                member.containing_block == NULL ||
                selected.native_definition == NULL ||
                WN_operator(selected.native_definition) != OPR_STID ||
                WN_kid_count(selected.native_definition) != 1 ||
                expression == NULL || !DSL_WN_Is_Native(expression) ||
                !DSL_WN_Get_Logical_Opcode
                    (expression, &logical_opcode, NULL) ||
                !DSL_IR_Image_Get_Value
                    (selected.value_id, &member.value) ||
                member.value.id != selected.value_id ||
                member.value.value_kind != DSL_IR_VALUE_OPERATOR_RESULT ||
                member.value.st != WN_st_idx(selected.native_definition) ||
                member.value.ty != WN_ty(selected.native_definition) ||
                owner_values.find(member.value.st) == owner_values.end() ||
                owner_values.find(member.value.st)->second.id !=
                    member.value.id ||
                ST_IDX_level(member.value.st) != CURRENT_SYMTAB ||
                ST_IDX_index(member.value.st) == 0 ||
                ST_IDX_index(member.value.st) >=
                    ST_Table_Size(CURRENT_SYMTAB) ||
                ST_type(St_Table[member.value.st]) != member.value.ty ||
                member.value.producer_node_id != selected.node_id ||
                !DSL_IR_Image_Get_Node(selected.node_id, &member.node) ||
                member.node.result_value_id != selected.value_id ||
                member.node.operand_count !=
                    (UINT32)WN_kid_count(expression) ||
                (member.node.payload == STR_IDX_ZERO &&
                 logical_opcode.payload[0] != '\0') ||
                (member.node.payload != STR_IDX_ZERO &&
                 strcmp(Index_To_Str(member.node.payload),
                        logical_opcode.payload) != 0) ||
                !DSL_IR_Image_Get_Opcode_Descriptor
                    (member.node.opcode_descriptor_id, &member.opcode) ||
                member.opcode.logical_operator !=
                    selected.expected_operator ||
                member.opcode.version != selected.expected_version ||
                logical_opcode.dsl_operator !=
                    selected.expected_operator ||
                logical_opcode.source_version !=
                    selected.expected_version ||
                member.opcode.effect_model != DSL_EFFECT_MODEL_PURE ||
                (member.node.flags &
                 ~(DSL_IR_NODE_FLAG_TYPED_EXTERNAL_ROW |
                   DSL_IR_NODE_FLAG_GENERATED_EXTERNAL)) != 0 ||
                member.value.flags != DSL_IR_VALUE_FLAG_NONE ||
                !DSL_Tensor_Has_Unique_Ownership(member.value.st) ||
                WN_Get_Linenum(selected.native_definition) == 0)
                return DSL_IR_Lower_Report
                           (diagnostic, i, "group member identity mismatch");
            if (tree_position == tree_positions.end() ||
                (group.containing_block != NULL &&
                 group.containing_block != member.containing_block))
                return DSL_IR_Lower_Report
                           (diagnostic, i, "mixed or foreign group blocks");
            member.tree_order = tree_position->second.tree_order;
            member.block_ordinal = tree_position->second.block_ordinal;
            group.containing_block = member.containing_block;
            if (member.tree_order < group.first_order) {
                group.first_order = member.tree_order;
                group.first_definition = selected.native_definition;
            }
            if (member.tree_order > group.last_order)
                group.last_order = member.tree_order;
            if (member_by_value.find(member.value.id) !=
                    member_by_value.end() ||
                member_by_node.find(member.node.id) !=
                    member_by_node.end() ||
                member_by_st.find(member.value.st) !=
                    member_by_st.end() ||
                member_by_definition.find(selected.native_definition) !=
                    member_by_definition.end())
                return DSL_IR_Lower_Report
                           (diagnostic, i, "duplicate group member");
            member_by_value[member.value.id] = index;
            member_by_node[member.node.id] = index;
            member_by_st[member.value.st] = index;
            member_by_definition[selected.native_definition] = index;
            if (member.value.id == request.group_output_value_id) {
                if (group.output_definition != NULL)
                    return DSL_IR_Lower_Report
                               (diagnostic, i, "duplicate group output");
                group.output_definition = selected.native_definition;
                group.output = member.value;
                fake.relation = request.relation;
                fake.standard_block = request.standard_block;
            }
            members.push_back(member);
            member_groups.push_back(i);
        }
        UINT32 output_order = 0;
        for (UINT32 j = 0; j < request.member_count; ++j) {
            const DSL_IR_NATIVE_VALUE_LOWER_JOURNAL &member =
                members[group.first_member + j];
            if (member.value.id == request.group_output_value_id)
                output_order = member.tree_order;
        }
        if (group.output_definition == NULL ||
            output_order != group.last_order ||
            !DSL_IR_Group_Lower_Reaches_Output
                 (group, members, member_by_value) ||
            !DSL_IR_Group_Lower_Statements_Closed(group, members))
            return DSL_IR_Lower_Report
                       (diagnostic, i, "group output or closure mismatch");
        if (!detached_blocks.insert(request.standard_block).second)
            return DSL_IR_Lower_Report
                       (diagnostic, i, "duplicate group result block");
        groups.push_back(group);
    }

    std::vector<DSL_IR_GROUP_MEMBER_USES> uses(members.size());
    memset(&uses[0], 0, uses.size() * sizeof(uses[0]));
    std::vector<std::vector<UINT32> > member_calls(members.size());
    std::vector<std::vector<UINT32> > member_arguments(members.size());
    std::map<DSL_IR_GROUP_USE_EDGE, UINT32> call_by_actual;
    std::map<DSL_IR_GROUP_USE_EDGE, UINT32> argument_by_actual;
    std::set<DSL_CALL_ARGUMENT_ID> retired_arguments;
    std::set<DSL_IR_NODE_ID> effect_nodes;
    if (!DSL_IR_Group_Index_Managed_Uses
            (pu_info, member_by_value, member_by_node, member_by_st,
             &uses, &member_calls, &member_arguments,
              &call_by_actual, &argument_by_actual,
              &retired_arguments, &effect_nodes))
        return DSL_IR_Lower_Report
                   (diagnostic, 0, "managed-use indexing failed");
    std::vector<DSL_REGION_SYMBOL_REDIRECT> region_redirects;
    for (UINT32 i = 0; i < groups.size(); ++i) {
        DSL_IR_GROUP_LOWER_JOURNAL &group = groups[i];
        const DSL_IR_NATIVE_GROUP_LOWER_REQUEST &request = *group.request;
        UINT32 output_member = member_by_value.find(group.output.id)->second;
        const DSL_IR_GROUP_MEMBER_USES &output_uses =
            uses[output_member];
        const DSL_RUNTIME_VALUE_PROJECTION_RECORD &projection =
            output_uses.projection;
        if (request.relation.relation_kind !=
                DSL_IR_LOWER_RELATION_RUNTIME_VALUE_PROJECTION ||
            request.relation.value_projection_id == 0 ||
            request.relation.runtime_input_id != 0 ||
            request.relation.runtime_binding_id != 0 ||
            output_uses.projection_count != 1 ||
            output_uses.input_count != 0 ||
            projection.id != request.relation.value_projection_id ||
            projection.owner_pu_st != owner_pu_st ||
            projection.source_value_id != group.output.id ||
            projection.source_st != group.output.st ||
            projection.source_ty != group.output.ty ||
            projection.binding_kind != DSL_RUNTIME_BINDING_LOCAL_VALUE ||
            !DSL_IR_Group_Output_Calls_Valid
                (owner_pu_st, group.output, projection.id,
                 member_calls[output_member],
                 member_arguments[output_member], call_by_actual,
                 argument_by_actual, retired_arguments) ||
            request.standard_block == NULL ||
            WN_operator(request.standard_block) != OPR_BLOCK ||
            WN_first(request.standard_block) == NULL ||
            tree_positions.find(request.standard_block) !=
                tree_positions.end() ||
            WN_operator(WN_last(request.standard_block)) != OPR_STID ||
            WN_st_idx(WN_last(request.standard_block)) !=
                projection.handle_st ||
            WN_ty(WN_last(request.standard_block)) !=
                projection.handle_ty)
            return DSL_IR_Lower_Report
                       (diagnostic, group.request_index,
                        "group output projection or block mismatch");
        group.handle_st = projection.handle_st;
        group.handle_ty = projection.handle_ty;
        DSL_IR_LOWER_STANDARD_BLOCK_SCAN scan;
        scan.source_st = group.output.st;
        scan.handle_st = group.handle_st;
        scan.result_handle_definition = WN_last(request.standard_block);
        scan.source_position =
            WN_Get_Linenum(group.output_definition);
        scan.handle_definition_count = 0;
        scan.statement_count = 0;
        scan.valid = TRUE;
        DSL_IR_Lower_Scan_Standard_Block(request.standard_block, &scan);
        if (!scan.valid || scan.handle_definition_count != 1 ||
            scan.statement_count == 0)
            return DSL_IR_Lower_Report
                       (diagnostic, group.request_index,
                        "group standard block is malformed");
        if (DSL_IR_Group_Block_Uses_Member
                (request.standard_block, member_by_st))
            return DSL_IR_Lower_Report
                       (diagnostic, group.request_index,
                        "standard block reads a retiring member");
        group.statement_count = scan.statement_count;
        if (output_uses.region_count != 0) {
            DSL_REGION_SYMBOL_REDIRECT redirect;
            redirect.old_st = group.output.st;
            redirect.new_st = group.handle_st;
            region_redirects.push_back(redirect);
        }
    }

    std::vector<UINT32> definition_counts(members.size(), 0);
    std::vector<DSL_IR_GROUP_USE_EDGE> physical_edges;
    std::vector<DSL_IR_GROUP_USE_EDGE> logical_edges;
    BOOL has_external_provenance = FALSE;
    if (!DSL_IR_Group_Index_Physical_Uses
            (pu_root, UINT32_MAX, member_by_definition, member_by_st,
             &definition_counts, &physical_edges))
        return DSL_IR_Lower_Report
                   (diagnostic, 0, "member has an escaping physical use");
    for (UINT32 id = 1; id <= DSL_IR_Image_Value_Reference_Count(); ++id) {
        DSL_IR_VALUE_REFERENCE_RECORD reference;
        if (!DSL_IR_Image_Get_Value_Reference(id, &reference))
            return DSL_IR_Lower_Report
                       (diagnostic, 0, "logical-use indexing failed");
        std::map<DSL_IR_VALUE_ID, UINT32>::const_iterator source =
            member_by_value.find(reference.value_id);
        if (source == member_by_value.end())
            continue;
        std::map<DSL_IR_NODE_ID, UINT32>::const_iterator destination =
            member_by_node.find(reference.owner_node_id);
        if (destination == member_by_node.end()) {
            const DSL_IR_GROUP_LOWER_JOURNAL &group =
                groups[member_groups[source->second]];
            DSL_IR_NODE_RECORD owner_node;
            DSL_IR_VALUE_RECORD owner_result;
            DSL_IR_VALUE_ID redirect_target = DSL_IR_VALUE_INVALID_ID;
            if (members[source->second].value.id != group.output.id ||
                !DSL_IR_Image_Get_Node(reference.owner_node_id,
                                       &owner_node) ||
                !DSL_IR_Image_Get_Value(owner_node.result_value_id,
                                        &owner_result) ||
                (owner_node.flags &
                 (DSL_IR_NODE_FLAG_RETIRED | DSL_IR_NODE_FLAG_LOWERED |
                  DSL_IR_NODE_FLAG_DEAD_ELIDED)) == 0 ||
                ((owner_node.flags & DSL_IR_NODE_FLAG_RETIRED) != 0 &&
                 !DSL_IR_Image_Value_Redirect_Target
                      (owner_result.id, &redirect_target)) ||
                ST_IDX_level(owner_result.st) != CURRENT_SYMTAB ||
                owner_values.find(owner_result.st) == owner_values.end() ||
                owner_values.find(owner_result.st)->second.id !=
                    owner_result.id ||
                physical_symbols.find(owner_result.st) !=
                    physical_symbols.end() ||
                DSL_Region_Symbol_Use_Count(pu_info, owner_result.st) != 0 ||
                effect_nodes.find(owner_node.id) != effect_nodes.end())
                return DSL_IR_Lower_Report
                           (diagnostic, member_groups[source->second],
                            "member has an escaping logical use");
            has_external_provenance = TRUE;
            continue;
        }
        logical_edges.push_back
            (std::make_pair(source->second, destination->second));
    }
    if (has_external_provenance &&
        !DSL_IR_Image_Validate_Lowered_Relations(diagnostic))
        return DSL_IR_Lower_Report
                   (diagnostic, 0, "non-executable provenance is invalid");
    for (UINT32 i = 0; i < members.size(); ++i) {
        const DSL_IR_NATIVE_VALUE_LOWER_JOURNAL &member = members[i];
        const DSL_IR_GROUP_LOWER_JOURNAL &group =
            groups[member_groups[i]];
        const DSL_IR_GROUP_MEMBER_USES &use = uses[i];
        if (definition_counts[i] != 1 || use.effect_count != 0)
            return DSL_IR_Lower_Report
                       (diagnostic, group.request_index,
                        "member definition or effect mismatch");
        if (member.value.id != group.output.id &&
            (use.projection_count != 0 || use.input_count != 0 ||
             use.call_count != 0 || use.call_argument_count != 0 ||
             use.region_count != 0))
            return DSL_IR_Lower_Report
                       (diagnostic, group.request_index,
                        "private member escapes the group");
    }
    if (physical_edges.size() != logical_edges.size())
        return DSL_IR_Lower_Report
                   (diagnostic, 0, "physical/logical use census mismatch");
    std::sort(physical_edges.begin(), physical_edges.end());
    std::sort(logical_edges.begin(), logical_edges.end());
    if (physical_edges != logical_edges)
        return DSL_IR_Lower_Report
                   (diagnostic, 0, "physical/logical use edges differ");
    for (UINT32 i = 0; i < physical_edges.size(); ++i) {
        UINT32 source = physical_edges[i].first;
        UINT32 destination = physical_edges[i].second;
        const DSL_IR_GROUP_LOWER_JOURNAL &source_group =
            groups[member_groups[source]];
        if (members[source].tree_order >= members[destination].tree_order ||
            members[source].containing_block !=
                members[destination].containing_block ||
            (member_groups[source] != member_groups[destination] &&
             (members[source].value.id != source_group.output.id ||
              uses[source].region_count != 0)))
            return DSL_IR_Lower_Report
                       (diagnostic, source_group.request_index,
                        "member use violates group dominance");
    }
    std::set<DSL_RUNTIME_VALUE_PROJECTION_ID> call_projections;
    std::set<DSL_IR_VALUE_ID> call_values;
    for (UINT32 id = 1; id <= DSL_Runtime_Interface_Image_Call_Count();
         ++id) {
        DSL_RUNTIME_CALL_PROJECTION_RECORD call;
        if (!DSL_Runtime_Interface_Image_Get_Call(id, &call))
            return DSL_IR_Lower_Report
                       (diagnostic, 0, "runtime call indexing failed");
        call_projections.insert(call.value_projection_id);
        call_values.insert(call.source_value_id);
    }
    std::map<ST_IDX, DSL_IR_GROUP_RETIRED_ALIAS> aliases;
    std::map<UINT32, UINT32> member_subtrees;
    for (UINT32 i = 0; i < members.size(); ++i) {
        std::map<const WN *, DSL_IR_GROUP_TREE_POSITION>::const_iterator
            position = tree_positions.find
                (members[i].request->native_definition);
        if (position == tree_positions.end())
            return DSL_IR_Lower_Report
                       (diagnostic, 0, "group member tree index is missing");
        member_subtrees[position->second.tree_order] =
            position->second.subtree_end_order;
    }
    for (UINT32 id = 1; id <= DSL_Runtime_Interface_Image_Value_Count();
         ++id) {
        DSL_RUNTIME_VALUE_PROJECTION_RECORD projection;
        DSL_IR_VALUE_ID target_id = DSL_IR_VALUE_INVALID_ID;
        if (!DSL_Runtime_Interface_Image_Get_Value(id, &projection))
            return DSL_IR_Lower_Report
                       (diagnostic, 0, "runtime alias indexing failed");
        if (!DSL_IR_Image_Value_Redirect_Target
                (projection.source_value_id, &target_id))
            continue;
        std::map<DSL_IR_VALUE_ID, UINT32>::const_iterator target =
            member_by_value.find(target_id);
        if (target == member_by_value.end())
            continue;
        UINT32 group_index = member_groups[target->second];
        const DSL_IR_GROUP_LOWER_JOURNAL &group = groups[group_index];
        if (target_id != group.output.id ||
            projection.owner_pu_st != owner_pu_st)
            continue;
        DSL_IR_VALUE_RECORD retired;
        DSL_IR_NODE_RECORD retired_node;
        std::map<ST_IDX, DSL_IR_VALUE_RECORD>::const_iterator owned =
            owner_values.find(projection.source_st);
        if (projection.owner_pu_st != owner_pu_st ||
            !DSL_IR_Image_Get_Value
                (projection.source_value_id, &retired) ||
            !DSL_IR_Image_Get_Node
                (retired.producer_node_id, &retired_node) ||
            (retired_node.flags & DSL_IR_NODE_FLAG_RETIRED) == 0 ||
            owned == owner_values.end() ||
            owned->second.id != retired.id ||
            retired.st != projection.source_st ||
            retired.ty != projection.source_ty ||
            retired.ty != group.output.ty ||
            projection.handle_ty != group.handle_ty ||
            projection.binding_kind != DSL_RUNTIME_BINDING_LOCAL_VALUE ||
            ST_IDX_level(projection.handle_st) != CURRENT_SYMTAB ||
            ST_IDX_index(projection.handle_st) == 0 ||
            ST_IDX_index(projection.handle_st) >=
                ST_Table_Size(CURRENT_SYMTAB) ||
            ST_sclass(St_Table[projection.handle_st]) != SCLASS_AUTO ||
            ST_type(St_Table[projection.handle_st]) !=
                projection.handle_ty ||
            projection.handle_st == group.handle_st ||
            call_projections.find(projection.id) !=
                call_projections.end() ||
            call_values.find(retired.id) != call_values.end() ||
            DSL_Region_Symbol_Use_Count
                (pu_info, projection.handle_st) != 0 ||
            effect_nodes.find(retired_node.id) != effect_nodes.end())
            return DSL_IR_Lower_Report
                       (diagnostic, group.request_index,
                        "retired output projection is not a local alias");
        DSL_IR_GROUP_RETIRED_ALIAS alias;
        alias.replacement_st = group.handle_st;
        alias.group_index = group_index;
        if (!aliases.insert
                (std::make_pair(projection.handle_st, alias)).second)
            return DSL_IR_Lower_Report
                       (diagnostic, group.request_index,
                        "retired output projection handle is ambiguous");
    }
    for (std::map<const WN *, DSL_IR_GROUP_TREE_POSITION>::const_iterator
             current = tree_positions.begin();
         current != tree_positions.end(); ++current) {
        WN *wn = const_cast<WN *>(current->first);
        if (!WN_has_sym(wn))
            continue;
        std::map<ST_IDX, DSL_IR_GROUP_RETIRED_ALIAS>::iterator alias =
            aliases.find(WN_st_idx(wn));
        if (alias == aliases.end())
            continue;
        const DSL_IR_GROUP_LOWER_JOURNAL &group =
            groups[alias->second.group_index];
        std::map<const WN *, DSL_IR_GROUP_TREE_POSITION>::const_iterator
            output = tree_positions.find(group.output_definition);
        std::map<UINT32, UINT32>::const_iterator member =
            member_subtrees.upper_bound(current->second.tree_order);
        BOOL inside_member = FALSE;
        if (member != member_subtrees.begin()) {
            --member;
            inside_member = current->second.tree_order < member->second;
        }
        if (WN_operator(wn) != OPR_LDID ||
            WN_ty(wn) != group.handle_ty ||
            output == tree_positions.end() ||
            current->second.containing_block != group.containing_block ||
            current->second.tree_order <
                output->second.subtree_end_order || inside_member)
            return DSL_IR_Lower_Report
                       (diagnostic, group.request_index,
                        "retired alias has a non-dominating use or escape");
        alias->second.reads.push_back(wn);
    }
    for (UINT32 i = 0; i < groups.size(); ++i)
        if (DSL_IR_Group_Block_Uses_Alias
                (groups[i].request->standard_block, aliases))
            return DSL_IR_Lower_Report
                       (diagnostic, groups[i].request_index,
                        "standard block reads a retired alias handle");
    if (!region_redirects.empty() &&
        !DSL_Region_Can_Apply_Lowering_Transitions
             (pu_info, &region_redirects[0], region_redirects.size(),
              NULL, 0))
        return DSL_IR_Lower_Report
                   (diagnostic, 0, "group REGION transition is invalid");

    std::sort(groups.begin(), groups.end(), DSL_IR_GROUP_LOWER_LESS());
    for (UINT32 i = 1; i < groups.size(); ++i) {
        if (groups[i - 1].containing_block == groups[i].containing_block &&
            groups[i - 1].last_order >= groups[i].first_order)
            return DSL_IR_Lower_Report
                       (diagnostic, groups[i].request_index,
                        "interleaved groups");
    }
    for (UINT32 i = 0; i < groups.size(); ++i) {
        DSL_IR_GROUP_LOWER_JOURNAL &group = groups[i];
        WN *block = group.request->standard_block;
        while (WN_first(block) != NULL) {
            WN *stmt = WN_EXTRACT_FromBlock(block, WN_first(block));
            WN_INSERT_BlockBefore
                (group.containing_block, group.first_definition, stmt);
        }
        WN_DELETE_Tree(block);
        for (UINT32 j = 0; j < group.member_count; ++j) {
            WN *definition =
                members[group.first_member + j].request->native_definition;
            WN *removed = WN_EXTRACT_FromBlock
                              (group.containing_block, definition);
            FmtAssert(removed == definition,
                      ("preflighted group extraction failed"));
            WN_DELETE_Tree(removed);
        }
    }
    for (std::map<ST_IDX, DSL_IR_GROUP_RETIRED_ALIAS>::iterator alias =
             aliases.begin(); alias != aliases.end(); ++alias)
        for (UINT32 i = 0; i < alias->second.reads.size(); ++i)
            WN_st_idx(alias->second.reads[i]) =
                alias->second.replacement_st;
    for (UINT32 i = 0; i < members.size(); ++i)
        FmtAssert(DSL_IR_Image_Mark_Value_Lowered(members[i].value.id),
                  ("preflighted grouped image update failed"));
    if (!region_redirects.empty())
        FmtAssert(DSL_Region_Apply_Lowering_Transitions
                      (pu_info, &region_redirects[0],
                       region_redirects.size(), NULL, 0),
                  ("preflighted group REGION update failed"));
    for (UINT32 i = 0; i < groups.size(); ++i) {
        const DSL_IR_GROUP_LOWER_JOURNAL &group = groups[i];
        DSL_IR_NATIVE_GROUP_LOWER_RESULT &result =
            results[group.request_index];
        result.owner_pu_st = owner_pu_st;
        result.semantic_source_value_id = group.semantic_source.id;
        result.retired_group_output_value_id = group.output.id;
        result.retired_member_count = group.member_count;
        result.inserted_statement_count = group.statement_count;
        result.handle_st = group.handle_st;
        result.handle_ty = group.handle_ty;
    }
    FmtAssert(DSL_IR_Image_Validate(NULL) &&
              DSL_IR_Image_Validate_Lowered_Relations(NULL) &&
              DSL_Region_Verify_PU(pu_info, NULL),
              ("grouped DSL lowering failed postcondition"));
    return TRUE;
}
