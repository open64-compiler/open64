/*
 * Copyright (C) 2026 Open64 Project
 *
 * Atomic lowering of native DSL value definitions to standard WHIRL blocks.
 * See doc/FHE-SYNC5-STANDARD-WHIRL-LOWERING-CONTRACT.md and
 * doc/FHE-SYNC5-NATIVE-OWNERSHIP-AUDIT.md.
 */

#include <algorithm>
#include "../../common/ir_image/dsl_lowered_relation.h"
#include "../../common/ir_image/dsl_image_service.h"
#include "dsl_ir_lower.h"
#include <map>
#include <set>
#include <string>
#include <string.h>
#include <vector>

#include "dsl_memory_behavior.h"
#include "dsl_ir_image.h"
#include "dsl_ir_lower_internal.h"
#include "dsl_ir_transaction_internal.h"
#include "dsl_region.h"
#include "dsl_region_internal.h"
#include "pu_info.h"
#include "strtab.h"
#include "symtab.h"
#include "wn.h"
#include "wn_util.h"

/* Commit-only image mutation after complete transaction preflight. */
extern BOOL DSL_IR_Image_Mark_Value_Lowered (DSL_IR_VALUE_ID);
/* Mark a checked pure constant as dead-elided provenance only; FALSE leaves
 * the image unchanged, and no WN is removed here. */
extern BOOL DSL_IR_Image_Mark_Value_Dead_Elided (DSL_IR_VALUE_ID);

/* Confirm that the caller selected the PU whose local WHIRL state is active. */
BOOL
DSL_IR_Lower_Current_PU_Is (PU_Info *pu_info)
{
    return pu_info != NULL && Current_PU_Info == pu_info;
}

typedef std::map<const WN *, UINT32> DSL_IR_LOWER_DEFINITION_INDEX;

typedef struct {
    WN *containing_block;
    UINT32 tree_order;
    UINT32 occurrences;
} DSL_IR_LOWER_TREE_POSITION;

/* One preorder walk locates every requested definition and attached block. */
static void
DSL_IR_Lower_Index_Tree
        (WN *tree, WN *containing_block,
         const DSL_IR_LOWER_DEFINITION_INDEX &definitions,
         const std::set<const WN *> &standard_blocks,
         std::vector<DSL_IR_LOWER_TREE_POSITION> *positions,
         std::set<const WN *> *attached_blocks,
         UINT32 *next_order)
{
    if (tree == NULL)
        return;
    UINT32 order = (*next_order)++;
    DSL_IR_LOWER_DEFINITION_INDEX::const_iterator found =
        definitions.find(tree);
    if (found != definitions.end()) {
        DSL_IR_LOWER_TREE_POSITION &position = (*positions)[found->second];
        position.containing_block = containing_block;
        position.tree_order = order;
        ++position.occurrences;
    }
    if (standard_blocks.find(tree) != standard_blocks.end())
        attached_blocks->insert(tree);
    if (WN_operator(tree) == OPR_BLOCK) {
        for (WN *stmt = WN_first(tree); stmt != NULL;
             stmt = WN_next(stmt))
            DSL_IR_Lower_Index_Tree
                (stmt, tree, definitions, standard_blocks, positions,
                 attached_blocks, next_order);
        return;
    }
    for (INT32 i = 0; i < WN_kid_count(tree); ++i)
        DSL_IR_Lower_Index_Tree
            (WN_kid(tree, i), NULL, definitions, standard_blocks,
             positions, attached_blocks, next_order);
}

typedef std::map<std::pair<ST_IDX, std::string>,
                 std::vector<DSL_IR_VALUE_RECORD> > DSL_IR_LOWER_VALUE_INDEX;

/* Preserve Find_PU_Value's owner/name and duplicate semantics in one pass. */
static BOOL
DSL_IR_Lower_Build_Value_Index
        (PU_Info *pu_info, DSL_IR_LOWER_VALUE_INDEX *values)
{
    std::string owner_metadata("owner_pu=");
    owner_metadata += ST_name(St_Table[PU_Info_proc_sym(pu_info)]);
    for (UINT32 id = 1; id <= DSL_IR_Image_Value_Count(); ++id) {
        DSL_IR_VALUE_RECORD value;
        if (!DSL_IR_Image_Get_Value(id, &value))
            return FALSE;
        if (value.name == STR_IDX_ZERO || value.metadata == STR_IDX_ZERO ||
            owner_metadata != Index_To_Str(value.metadata))
            continue;
        std::pair<ST_IDX, std::string> key
            (value.st, Index_To_Str(value.name));
        (*values)[key].push_back(value);
    }
    return TRUE;
}

/* Resolve a native definition by exact value ID without rescanning values. */
static BOOL
DSL_IR_Lower_Find_Native_Definition_Value
        (const WN *definition,
         DSL_IR_VALUE_ID value_id,
         UINT32 mode,
         const DSL_IR_LOWER_VALUE_INDEX &values,
         DSL_IR_VALUE_RECORD *value_record)
{
    if (definition == NULL || WN_operator(definition) != OPR_STID ||
        WN_kid_count(definition) != 1 || WN_kid0(definition) == NULL ||
        !DSL_WN_Is_Native(WN_kid0(definition)))
        return FALSE;
    DSL_LOGICAL_OPCODE logical_opcode;
    if (!DSL_WN_Get_Logical_Opcode
            (WN_kid0(definition), &logical_opcode, NULL))
        return FALSE;
    ST_IDX st = WN_st_idx(definition);
    if (ST_IDX_level(st) != CURRENT_SYMTAB || ST_IDX_index(st) == 0 ||
        ST_IDX_index(st) >= ST_Table_Size(CURRENT_SYMTAB) ||
        ST_type(St_Table[st]) != WN_ty(definition))
        return FALSE;
    std::pair<ST_IDX, std::string> key(st, ST_name(St_Table[st]));
    DSL_IR_LOWER_VALUE_INDEX::const_iterator found = values.find(key);
    if (found == values.end() || found->second.size() != 1 ||
        found->second[0].id != value_id)
        return FALSE;
    const DSL_IR_VALUE_RECORD &value = found->second[0];
    if (value.st != st || value.ty != WN_ty(definition) ||
        (value.value_kind == DSL_IR_VALUE_OPERATOR_RESULT &&
         !DSL_IR_Image_Current_PU_Is
             (PU_Info_proc_sym(Current_PU_Info))) ||
        (value.value_kind != DSL_IR_VALUE_OPERATOR_RESULT &&
         ((mode != DSL_IR_NATIVE_LOWER_PROMOTED_SOURCE_ELISION &&
           mode != DSL_IR_NATIVE_LOWER_VERIFIED_DEAD_SOURCE_ELISION) ||
          value.value_kind != DSL_IR_VALUE_CONSTANT)))
        return FALSE;
    DSL_IR_NODE_RECORD node;
    DSL_IR_OPCODE_DESCRIPTOR_RECORD opcode;
    if (!DSL_IR_Image_Get_Node(value.producer_node_id, &node) ||
        node.result_value_id != value.id ||
        !DSL_IR_Image_Get_Opcode_Descriptor
            (node.opcode_descriptor_id, &opcode) ||
        opcode.logical_operator != logical_opcode.dsl_operator ||
        opcode.version != logical_opcode.source_version ||
        node.operand_count != (UINT32)WN_kid_count(WN_kid0(definition)) ||
        (node.payload == STR_IDX_ZERO && logical_opcode.payload[0] != '\0') ||
        (node.payload != STR_IDX_ZERO &&
         strcmp(Index_To_Str(node.payload), logical_opcode.payload) != 0))
        return FALSE;
    if (value_record != NULL)
        *value_record = value;
    return TRUE;
}

/* Emit one stable diagnostic for a request and convert failure to FALSE. */
BOOL
DSL_IR_Lower_Report
        (FILE *diagnostic,
         UINT32 request_index,
         const char *message)
{
    if (diagnostic != NULL)
        fprintf(diagnostic,
                "DSL standard lowering error: request=%u %s\n",
                request_index, message);
    return FALSE;
}

/* Retired ABI rows preserve provenance but no longer require a live actual. */
static BOOL
DSL_IR_Lower_Call_Argument_Is_Verified_Dead
        (const DSL_CALL_ARGUMENT_RECORD &argument,
         const std::map<DSL_CALL_ARGUMENT_ID,
                        DSL_RETIRED_CALL_ARGUMENT_RECORD> &retired_calls)
{
    std::map<DSL_CALL_ARGUMENT_ID,
             DSL_RETIRED_CALL_ARGUMENT_RECORD>::const_iterator found =
        retired_calls.find(argument.id);
    if (found == retired_calls.end())
        return FALSE;
    const DSL_RETIRED_CALL_ARGUMENT_RECORD &retired = found->second;
    return retired.call_argument_id == argument.id &&
           retired.callsite_id == argument.callsite_id &&
           retired.argument_value_id == argument.argument_value_id &&
           retired.old_actual_ordinal == argument.actual_ordinal &&
           retired.old_callee_formal_ordinal ==
               argument.callee_formal_ordinal &&
           retired.retirement_reason ==
               DSL_INTERFACE_RETIREMENT_VERIFIED_DEAD_INPUT;
}

/* One walk checks source uses and dead local projections for the entire batch. */
static void
DSL_IR_Lower_Scan_All_Source_Uses
        (const WN *tree, const WN *native_definition,
         const DSL_IR_LOWER_DEFINITION_INDEX &definitions,
         const std::map<ST_IDX, UINT32> &source_symbols,
         std::vector<BOOL> *valid,
         std::set<ST_IDX> *used_symbols)
{
    if (tree == NULL)
        return;
    const WN *enclosing_definition = native_definition;
    if (WN_operator(tree) == OPR_STID && WN_kid_count(tree) == 1 &&
        WN_kid0(tree) != NULL && DSL_WN_Is_Native(WN_kid0(tree)))
        enclosing_definition = tree;
    if (WN_has_sym(tree)) {
        ST_IDX st = WN_st_idx(tree);
        used_symbols->insert(st);
        std::map<ST_IDX, UINT32>::const_iterator found =
            source_symbols.find(st);
        if (found != source_symbols.end()) {
            DSL_IR_LOWER_DEFINITION_INDEX::const_iterator own =
                definitions.find(tree);
            BOOL own_definition = own != definitions.end() &&
                own->second == found->second;
            BOOL request_operand = WN_operator(tree) == OPR_LDID &&
                enclosing_definition != NULL &&
                definitions.find(enclosing_definition) != definitions.end();
            if (!own_definition && !request_operand)
                (*valid)[found->second] = FALSE;
        }
    }
    if (WN_operator(tree) == OPR_BLOCK) {
        for (const WN *stmt = WN_first(tree); stmt != NULL;
             stmt = WN_next(stmt))
            DSL_IR_Lower_Scan_All_Source_Uses
                (stmt, NULL, definitions, source_symbols, valid,
                 used_symbols);
        return;
    }
    for (INT32 i = 0; i < WN_kid_count(tree); ++i)
        DSL_IR_Lower_Scan_All_Source_Uses
            (WN_kid(tree, i), enclosing_definition, definitions,
             source_symbols, valid, used_symbols);
}

/* Validate a detached replacement block and its sole final handle definition. */
void
DSL_IR_Lower_Scan_Standard_Block
        (const WN *tree, DSL_IR_LOWER_STANDARD_BLOCK_SCAN *scan)
{
    if (tree == NULL || !scan->valid)
        return;
    OPERATOR opr = WN_operator(tree);
    if (DSL_WN_Is_Native(tree) ||
        (WN_has_sym(tree) && WN_st_idx(tree) == scan->source_st)) {
        scan->valid = FALSE;
        return;
    }
    if ((OPERATOR_is_stmt(opr) || OPERATOR_is_scf(opr)) &&
        opr != OPR_BLOCK) {
        ++scan->statement_count;
        if (WN_Get_Linenum(tree) != scan->source_position)
            scan->valid = FALSE;
    }
    if (opr == OPR_STID && WN_st_idx(tree) == scan->handle_st) {
        ++scan->handle_definition_count;
        if (tree != scan->result_handle_definition)
            scan->valid = FALSE;
    }
    if (opr == OPR_BLOCK) {
        for (const WN *stmt = WN_first(tree); stmt != NULL;
             stmt = WN_next(stmt))
            DSL_IR_Lower_Scan_Standard_Block(stmt, scan);
        return;
    }
    for (INT32 i = 0; i < WN_kid_count(tree); ++i)
        DSL_IR_Lower_Scan_Standard_Block(WN_kid(tree, i), scan);
}

typedef struct {
    std::map<DSL_IR_VALUE_ID,
             std::vector<DSL_RUNTIME_VALUE_PROJECTION_RECORD> > projections;
    std::map<DSL_IR_VALUE_ID,
             std::vector<DSL_RUNTIME_INPUT_RECORD> > inputs;
    std::map<DSL_IR_VALUE_ID,
             std::vector<DSL_RUNTIME_CALL_PROJECTION_RECORD> > calls;
    std::set<DSL_RUNTIME_VALUE_PROJECTION_ID> call_projection_ids;
    std::map<std::pair<DSL_CALLSITE_METADATA_ID, UINT32>,
             DSL_RUNTIME_CALL_PROJECTION_RECORD> calls_by_actual;
    std::map<DSL_IR_VALUE_ID,
             std::vector<DSL_CALL_ARGUMENT_RECORD> > arguments;
    std::map<DSL_CALL_ARGUMENT_ID,
             DSL_RETIRED_CALL_ARGUMENT_RECORD> retired_calls;
} DSL_IR_LOWER_RELATION_INDEX;

/* Snapshot runtime relations once; every request observes the same tables. */
static BOOL
DSL_IR_Lower_Build_Relation_Index (DSL_IR_LOWER_RELATION_INDEX *index)
{
    for (UINT32 id = 1; id <= DSL_Runtime_Interface_Image_Value_Count();
         ++id) {
        DSL_RUNTIME_VALUE_PROJECTION_RECORD record;
        if (!DSL_Runtime_Interface_Image_Get_Value(id, &record))
            return FALSE;
        index->projections[record.source_value_id].push_back(record);
    }
    for (UINT32 id = 1;
         id <= DSL_Program_Interface_Image_Runtime_Input_Count(); ++id) {
        DSL_RUNTIME_INPUT_RECORD record;
        if (!DSL_Program_Interface_Image_Get_Runtime_Input(id, &record))
            return FALSE;
        index->inputs[record.source_value_id].push_back(record);
    }
    for (UINT32 id = 1; id <= DSL_Runtime_Interface_Image_Call_Count();
         ++id) {
        DSL_RUNTIME_CALL_PROJECTION_RECORD record;
        if (!DSL_Runtime_Interface_Image_Get_Call(id, &record))
            return FALSE;
        index->calls[record.source_value_id].push_back(record);
        index->call_projection_ids.insert(record.value_projection_id);
        index->calls_by_actual.insert
            (std::make_pair
                (std::make_pair(record.callsite_id, record.actual_ordinal),
                 record));
    }
    for (UINT32 id = 1; id <= DSL_Call_ABI_Image_Argument_Count(); ++id) {
        DSL_CALL_ARGUMENT_RECORD record;
        if (!DSL_Call_ABI_Image_Get_Argument(id, &record))
            return FALSE;
        index->arguments[record.argument_value_id].push_back(record);
    }
    for (UINT32 id = 1;
         id <= DSL_Program_Interface_Image_Retired_Call_Count(); ++id) {
        DSL_RETIRED_CALL_ARGUMENT_RECORD record;
        if (!DSL_Program_Interface_Image_Get_Retired_Call(id, &record))
            return FALSE;
        index->retired_calls.insert
            (std::make_pair(record.call_argument_id, record));
    }
    return TRUE;
}

/* Prove logical and call-ABI closure using the batch's indexed relations. */
static BOOL
DSL_IR_Lower_Logical_Uses_Closed
        (const DSL_IR_NATIVE_VALUE_LOWER_JOURNAL &entry,
         const DSL_IR_LOWER_RELATION_INDEX &relations,
         const std::map<DSL_IR_VALUE_ID, BOOL> &references_closed)
{
    std::map<DSL_IR_VALUE_ID, BOOL>::const_iterator closed =
        references_closed.find(entry.value.id);
    if (closed != references_closed.end() && !closed->second)
        return FALSE;
    std::map<DSL_IR_VALUE_ID,
             std::vector<DSL_CALL_ARGUMENT_RECORD> >::const_iterator args =
        relations.arguments.find(entry.value.id);
    if (args == relations.arguments.end())
        return TRUE;
    for (UINT32 i = 0; i < args->second.size(); ++i) {
        const DSL_CALL_ARGUMENT_RECORD &argument = args->second[i];
        if (entry.request->mode ==
                DSL_IR_NATIVE_LOWER_VERIFIED_DEAD_SOURCE_ELISION) {
            if (!DSL_IR_Lower_Call_Argument_Is_Verified_Dead
                    (argument, relations.retired_calls))
                return FALSE;
            continue;
        }
        std::pair<DSL_CALLSITE_METADATA_ID, UINT32> key
            (argument.callsite_id, argument.actual_ordinal);
        std::map<std::pair<DSL_CALLSITE_METADATA_ID, UINT32>,
                 DSL_RUNTIME_CALL_PROJECTION_RECORD>::const_iterator call =
            relations.calls_by_actual.find(key);
        if (call == relations.calls_by_actual.end() ||
            call->second.source_value_id != entry.value.id)
            return FALSE;
    }
    return TRUE;
}

/* Resolve a computed projection or promoted root input to its runtime handle. */
static BOOL
DSL_IR_Lower_Relation_Resolve
        (ST_IDX owner_pu_st,
         const DSL_IR_NATIVE_VALUE_LOWER_REQUEST &request,
         const DSL_IR_VALUE_RECORD &value,
         const DSL_IR_LOWER_RELATION_INDEX &relations,
         ST_IDX *handle_st,
         TY_IDX *handle_ty,
         ST_IDX *local_projection_st,
         DSL_RUNTIME_VALUE_PROJECTION_ID *local_projection_id)
{
    *local_projection_st = ST_IDX_ZERO;
    *local_projection_id = DSL_RUNTIME_VALUE_PROJECTION_INVALID_ID;
    if (request.mode ==
            DSL_IR_NATIVE_LOWER_VERIFIED_DEAD_SOURCE_ELISION) {
        if (request.relation.relation_kind !=
                DSL_IR_LOWER_RELATION_VERIFIED_DEAD_SOURCE ||
            request.relation.value_projection_id != 0 ||
            request.relation.runtime_input_id != 0 ||
            request.relation.runtime_binding_id != 0 ||
            value.value_kind != DSL_IR_VALUE_CONSTANT)
            return FALSE;
        UINT32 projection_count = 0;
        std::map<DSL_IR_VALUE_ID,
                 std::vector<DSL_RUNTIME_VALUE_PROJECTION_RECORD> >
            ::const_iterator projections = relations.projections.find(value.id);
        if (projections != relations.projections.end()) {
            for (UINT32 i = 0; i < projections->second.size(); ++i) {
                const DSL_RUNTIME_VALUE_PROJECTION_RECORD &projection =
                    projections->second[i];
                if (++projection_count > 1 ||
                    projection.owner_pu_st != owner_pu_st ||
                    projection.source_st != value.st ||
                    projection.source_ty != value.ty ||
                    projection.binding_kind !=
                        DSL_RUNTIME_BINDING_LOCAL_VALUE ||
                    ST_IDX_level(projection.handle_st) != CURRENT_SYMTAB ||
                    ST_IDX_index(projection.handle_st) == 0 ||
                    ST_IDX_index(projection.handle_st) >=
                        ST_Table_Size(CURRENT_SYMTAB) ||
                    ST_sclass(St_Table[projection.handle_st]) !=
                        SCLASS_AUTO ||
                    ST_type(St_Table[projection.handle_st]) !=
                        projection.handle_ty ||
                    projection.handle_st == value.st)
                    return FALSE;
                *local_projection_st = projection.handle_st;
                *local_projection_id = projection.id;
            }
        }
        if (relations.inputs.find(value.id) != relations.inputs.end() ||
            relations.calls.find(value.id) != relations.calls.end() ||
            (projection_count == 1 &&
             relations.call_projection_ids.find(*local_projection_id) !=
                 relations.call_projection_ids.end()))
            return FALSE;
        std::map<DSL_IR_VALUE_ID,
                 std::vector<DSL_CALL_ARGUMENT_RECORD> >::const_iterator
            arguments = relations.arguments.find(value.id);
        if (arguments != relations.arguments.end())
            for (UINT32 i = 0; i < arguments->second.size(); ++i)
                if (!DSL_IR_Lower_Call_Argument_Is_Verified_Dead
                        (arguments->second[i], relations.retired_calls))
                    return FALSE;
        *handle_st = ST_IDX_ZERO;
        *handle_ty = TY_IDX_ZERO;
        return TRUE;
    }
    if (request.mode == DSL_IR_NATIVE_LOWER_COMPUTED_STANDARD_BLOCK) {
        DSL_RUNTIME_VALUE_PROJECTION_RECORD projection;
        if (request.relation.relation_kind !=
                DSL_IR_LOWER_RELATION_RUNTIME_VALUE_PROJECTION ||
            request.relation.value_projection_id ==
                DSL_RUNTIME_VALUE_PROJECTION_INVALID_ID ||
            request.relation.runtime_input_id !=
                DSL_RUNTIME_INPUT_INVALID_ID ||
            request.relation.runtime_binding_id !=
                DSL_RUNTIME_INPUT_BINDING_INVALID_ID ||
            !DSL_Runtime_Interface_Image_Get_Value
                (request.relation.value_projection_id, &projection) ||
            projection.owner_pu_st != owner_pu_st ||
            projection.source_value_id != value.id ||
            projection.source_st != value.st ||
            projection.source_ty != value.ty ||
            projection.binding_kind != DSL_RUNTIME_BINDING_LOCAL_VALUE)
            return FALSE;
        if (relations.inputs.find(value.id) != relations.inputs.end())
            return FALSE;
        *handle_st = projection.handle_st;
        *handle_ty = projection.handle_ty;
        return TRUE;
    }

    DSL_RUNTIME_INPUT_RECORD input;
    DSL_RUNTIME_INPUT_BINDING_RECORD binding;
    if (request.mode != DSL_IR_NATIVE_LOWER_PROMOTED_SOURCE_ELISION ||
        request.relation.relation_kind !=
            DSL_IR_LOWER_RELATION_ROOT_PROMOTED_INPUT ||
        request.relation.value_projection_id !=
            DSL_RUNTIME_VALUE_PROJECTION_INVALID_ID ||
        request.relation.runtime_input_id == DSL_RUNTIME_INPUT_INVALID_ID ||
        request.relation.runtime_binding_id ==
            DSL_RUNTIME_INPUT_BINDING_INVALID_ID ||
        !DSL_Program_Interface_Image_Get_Runtime_Input
            (request.relation.runtime_input_id, &input) ||
        !DSL_Program_Interface_Image_Get_Runtime_Binding
            (request.relation.runtime_binding_id, &binding) ||
        input.input_kind != DSL_RUNTIME_INPUT_SOURCE_EXTERNAL_TENSOR ||
        input.source_owner_pu_st != owner_pu_st ||
        input.source_value_id != value.id || input.source_st != value.st ||
        input.source_ty != value.ty || input.source_tcon == TCON_IDX_ZERO ||
        binding.runtime_input_id != input.id ||
        binding.owner_pu_st != owner_pu_st ||
        binding.binding_kind !=
            DSL_RUNTIME_INPUT_BINDING_ROOT_PROMOTED_SOURCE ||
        binding.handle_ty != input.handle_ty)
        return FALSE;
    DSL_RUNTIME_VALUE_PROJECTION_RECORD projection;
    UINT32 projection_count = 0;
    std::map<DSL_IR_VALUE_ID,
             std::vector<DSL_RUNTIME_VALUE_PROJECTION_RECORD> >
        ::const_iterator projections = relations.projections.find(value.id);
    if (projections != relations.projections.end()) {
        for (UINT32 i = 0; i < projections->second.size(); ++i) {
            const DSL_RUNTIME_VALUE_PROJECTION_RECORD &current =
                projections->second[i];
            if (current.owner_pu_st != owner_pu_st ||
                current.source_value_id != value.id)
                continue;
            projection = current;
            ++projection_count;
        }
    }
    if (projection_count > 1 ||
        (projection_count == 1 &&
         (projection.source_st != value.st ||
          projection.source_ty != value.ty ||
          projection.handle_ty != binding.handle_ty ||
          projection.binding_kind != DSL_RUNTIME_BINDING_LOCAL_VALUE ||
          ST_IDX_level(projection.handle_st) != CURRENT_SYMTAB ||
          ST_IDX_index(projection.handle_st) == 0 ||
          ST_IDX_index(projection.handle_st) >=
              ST_Table_Size(CURRENT_SYMTAB) ||
          ST_type(St_Table[projection.handle_st]) !=
              projection.handle_ty ||
          ST_sclass(St_Table[projection.handle_st]) != SCLASS_AUTO)))
        return FALSE;
    if (projection_count == 1) {
        *local_projection_st = projection.handle_st;
        *local_projection_id = projection.id;
    }
    *handle_st = binding.handle_st;
    *handle_ty = binding.handle_ty;
    return TRUE;
}

struct DSL_IR_LOWER_JOURNAL_LESS {
    /* Sort transactions by native tree order before extracting definitions. */
    BOOL operator()
        (const DSL_IR_NATIVE_VALUE_LOWER_JOURNAL &left,
         const DSL_IR_NATIVE_VALUE_LOWER_JOURNAL &right) const
    {
        return left.tree_order < right.tree_order;
    }
};

/*
 * Lower all requested native definitions for one active PU. Preflight derives
 * every physical parent/result relationship from PU_Info and the borrowed
 * request blocks, validates source/effect/runtime closure, then commits WHIRL
 * extraction and logical LOWERED flags in tree order. No rollback allocation
 * or mapped-image layout change is part of the commit path.
 */
BOOL
DSL_IR_Lower_Native_Values_To_Standard_Blocks
        (PU_Info *pu_info,
         const DSL_IR_NATIVE_VALUE_LOWER_REQUEST *requests,
         UINT32 request_count,
         FILE *diagnostic,
         DSL_IR_NATIVE_VALUE_LOWER_RESULT *results)
{
    if (results != NULL && request_count != 0)
        memset(results, 0,
               request_count * sizeof(DSL_IR_NATIVE_VALUE_LOWER_RESULT));
    WN *pu_root = pu_info == NULL ? NULL : PU_Info_tree_ptr(pu_info);
    ST_IDX owner_pu_st = pu_info == NULL ? ST_IDX_ZERO :
        PU_Info_proc_sym(pu_info);
    if (!DSL_IR_Lower_Current_PU_Is(pu_info) || pu_root == NULL ||
        requests == NULL || request_count == 0 || results == NULL)
        return DSL_IR_Lower_Report(diagnostic, 0, "transaction is incomplete");

    /* Prove the complete physical/logical replacement set before mutation. */
    DSL_IR_LOWER_VALUE_INDEX values;
    if (!DSL_IR_Lower_Build_Value_Index(pu_info, &values))
        return DSL_IR_Lower_Report
                   (diagnostic, 0, "value indexing failed");
    DSL_IR_LOWER_RELATION_INDEX relations;
    if (!DSL_IR_Lower_Build_Relation_Index(&relations))
        return DSL_IR_Lower_Report
                   (diagnostic, 0, "runtime relation indexing failed");
    DSL_IR_LOWER_DEFINITION_INDEX definitions;
    std::set<const WN *> standard_blocks;
    std::set<const WN *> handle_definitions;
    for (UINT32 i = 0; i < request_count; ++i) {
        const DSL_IR_NATIVE_VALUE_LOWER_REQUEST &request = requests[i];
        if (request.native_definition == NULL ||
            !definitions.insert
                (std::make_pair(request.native_definition, i)).second ||
            (request.standard_block != NULL &&
             !standard_blocks.insert(request.standard_block).second) ||
            (request.standard_block != NULL &&
             WN_operator(request.standard_block) == OPR_BLOCK &&
             WN_last(request.standard_block) != NULL &&
             !handle_definitions.insert
                 (WN_last(request.standard_block)).second))
            return DSL_IR_Lower_Report
                       (diagnostic, i, "duplicate transaction member");
    }
    std::vector<DSL_IR_LOWER_TREE_POSITION> positions(request_count);
    std::set<const WN *> attached_blocks;
    UINT32 next_order = 0;
    DSL_IR_Lower_Index_Tree
        (pu_root, NULL, definitions, standard_blocks, &positions,
         &attached_blocks, &next_order);
    std::vector<DSL_IR_NATIVE_VALUE_LOWER_JOURNAL> journal;
    std::set<DSL_IR_VALUE_ID> value_ids;
    std::set<DSL_IR_NODE_ID> node_ids;
    std::map<ST_IDX, UINT32> source_symbols;
    for (UINT32 i = 0; i < request_count; ++i) {
        const DSL_IR_NATIVE_VALUE_LOWER_REQUEST &request = requests[i];
        DSL_IR_NATIVE_VALUE_LOWER_JOURNAL entry;
        memset(&entry, 0, sizeof(entry));
        entry.request = &request;
        entry.request_index = i;
        entry.containing_block = positions[i].containing_block;
        entry.tree_order = positions[i].tree_order;
        if (request.standard_block != NULL &&
            WN_operator(request.standard_block) == OPR_BLOCK)
            entry.result_handle_definition = WN_last(request.standard_block);
        if (request.reserved != 0 || entry.containing_block == NULL ||
            positions[i].occurrences != 1)
            return DSL_IR_Lower_Report
                       (diagnostic, i, "PU or containing block mismatch");
        if (!DSL_IR_Lower_Find_Native_Definition_Value
                (request.native_definition, request.source_value_id,
                 request.mode, values, &entry.value) ||
            entry.value.id != request.source_value_id)
            return DSL_IR_Lower_Report
                       (diagnostic, i, "native definition identity mismatch");
        if (!DSL_IR_Image_Get_Node
                (entry.value.producer_node_id, &entry.node) ||
            !DSL_IR_Image_Get_Opcode_Descriptor
                (entry.node.opcode_descriptor_id, &entry.opcode) ||
            entry.opcode.logical_operator != request.expected_operator ||
            entry.opcode.version != request.expected_version ||
            entry.opcode.effect_model != DSL_EFFECT_MODEL_PURE)
            return DSL_IR_Lower_Report
                       (diagnostic, i, "logical opcode contract mismatch");
        if ((entry.node.flags & ~(DSL_IR_NODE_FLAG_TYPED_EXTERNAL_ROW |
                                  DSL_IR_NODE_FLAG_GENERATED_EXTERNAL)) !=
                DSL_IR_NODE_FLAG_NONE ||
            entry.value.flags != DSL_IR_VALUE_FLAG_NONE ||
            !DSL_Tensor_Has_Unique_Ownership(entry.value.st) ||
            WN_Get_Linenum(request.native_definition) == 0)
            return DSL_IR_Lower_Report
                       (diagnostic, i, "source ownership or use mismatch");
        if (!DSL_IR_Lower_Relation_Resolve
                (owner_pu_st, request, entry.value, relations,
                 &entry.handle_st, &entry.handle_ty,
                 &entry.local_projection_st,
                 &entry.local_projection_id))
            return DSL_IR_Lower_Report
                       (diagnostic, i, "runtime relation mismatch");
        if (!value_ids.insert(entry.value.id).second ||
            !node_ids.insert(entry.node.id).second ||
            !source_symbols.insert
                (std::make_pair(entry.value.st, i)).second)
            return DSL_IR_Lower_Report
                       (diagnostic, i, "duplicate transaction member");
        journal.push_back(entry);
    }

    std::map<DSL_IR_VALUE_ID, BOOL> references_closed;
    for (UINT32 id = 1; id <= DSL_IR_Image_Value_Reference_Count(); ++id) {
        DSL_IR_VALUE_REFERENCE_RECORD reference;
        if (!DSL_IR_Image_Get_Value_Reference(id, &reference))
            return DSL_IR_Lower_Report
                       (diagnostic, 0, "value-reference indexing failed");
        if (value_ids.find(reference.value_id) == value_ids.end())
            continue;
        DSL_IR_NODE_RECORD owner;
        if (!DSL_IR_Image_Get_Node(reference.owner_node_id, &owner))
            references_closed[reference.value_id] = FALSE;
        else if ((owner.flags & (DSL_IR_NODE_FLAG_RETIRED |
                                 DSL_IR_NODE_FLAG_LOWERED |
                                 DSL_IR_NODE_FLAG_DEAD_ELIDED)) == 0 &&
                 node_ids.find(owner.id) == node_ids.end())
            references_closed[reference.value_id] = FALSE;
    }

    std::vector<BOOL> source_uses_valid(request_count, TRUE);
    std::set<ST_IDX> used_symbols;
    DSL_IR_Lower_Scan_All_Source_Uses
        (pu_root, NULL, definitions, source_symbols, &source_uses_valid,
         &used_symbols);
    std::set<DSL_IR_NODE_ID> effected_nodes;
    for (UINT32 id = 1; id <= DSL_Effect_Image_State_Effect_Count(); ++id) {
        DSL_STATE_EFFECT_RECORD effect;
        if (!DSL_Effect_Image_Get_State_Effect(id, &effect))
            return DSL_IR_Lower_Report
                       (diagnostic, 0, "state-effect indexing failed");
        effected_nodes.insert(effect.owner_node_id);
    }
    std::map<ST_IDX, UINT32> region_symbol_uses;
    for (UINT32 id = 1; id <= DSL_Region_Interface_Count(pu_info); ++id) {
        DSL_REGION_INTERFACE_RECORD interface_record;
        if (!DSL_Region_Get_Interface(pu_info, id, &interface_record))
            return DSL_IR_Lower_Report
                       (diagnostic, 0, "REGION interface indexing failed");
        ++region_symbol_uses[interface_record.st];
    }

    for (UINT32 i = 0; i < journal.size(); ++i) {
        DSL_IR_NATIVE_VALUE_LOWER_JOURNAL &entry = journal[i];
        const DSL_IR_NATIVE_VALUE_LOWER_REQUEST &request = *entry.request;
        if (effected_nodes.find(entry.node.id) != effected_nodes.end())
            return DSL_IR_Lower_Report
                       (diagnostic, entry.request_index,
                        "source node has a state effect");
        if (!source_uses_valid[i] ||
            !DSL_IR_Lower_Logical_Uses_Closed
                (entry, relations, references_closed))
            return DSL_IR_Lower_Report
                       (diagnostic, entry.request_index,
                        "source value has an unlowered or escaping use");

        if (region_symbol_uses[entry.value.st] != 0 &&
            (request.mode !=
                 DSL_IR_NATIVE_LOWER_COMPUTED_STANDARD_BLOCK ||
             entry.handle_st == ST_IDX_ZERO))
            return DSL_IR_Lower_Report
                       (diagnostic, entry.request_index,
                        "source REGION interface cannot be redirected");

        if (request.mode ==
                DSL_IR_NATIVE_LOWER_PROMOTED_SOURCE_ELISION ||
            request.mode ==
                DSL_IR_NATIVE_LOWER_VERIFIED_DEAD_SOURCE_ELISION) {
            if (request.standard_block != NULL ||
                (entry.local_projection_st != ST_IDX_ZERO &&
                 entry.containing_block != WN_func_body(pu_root)) ||
                (request.mode ==
                     DSL_IR_NATIVE_LOWER_VERIFIED_DEAD_SOURCE_ELISION &&
                 (entry.opcode.logical_operator != OPR_DSLTENSORCONST ||
                  entry.node.operand_count != 0 ||
                  entry.value.value_kind != DSL_IR_VALUE_CONSTANT ||
                  (entry.local_projection_st != ST_IDX_ZERO &&
                   (used_symbols.find(entry.local_projection_st) !=
                        used_symbols.end() ||
                    region_symbol_uses[entry.local_projection_st] != 0)))))
                return DSL_IR_Lower_Report
                           (diagnostic, entry.request_index,
                            "source elision contract mismatch");
            continue;
        }
        if (request.mode !=
                DSL_IR_NATIVE_LOWER_COMPUTED_STANDARD_BLOCK ||
            request.standard_block == NULL ||
            WN_operator(request.standard_block) != OPR_BLOCK ||
            WN_first(request.standard_block) == NULL ||
            attached_blocks.find(request.standard_block) !=
                attached_blocks.end() ||
            WN_operator(WN_last(request.standard_block)) != OPR_STID ||
            WN_st_idx(WN_last(request.standard_block)) != entry.handle_st ||
            WN_ty(WN_last(request.standard_block)) != entry.handle_ty)
            return DSL_IR_Lower_Report
                       (diagnostic, entry.request_index,
                        "computed standard block is invalid");
        DSL_IR_LOWER_STANDARD_BLOCK_SCAN block_scan;
        block_scan.source_st = entry.value.st;
        block_scan.handle_st = entry.handle_st;
        entry.result_handle_definition = WN_last(request.standard_block);
        block_scan.result_handle_definition = entry.result_handle_definition;
        block_scan.source_position =
            WN_Get_Linenum(request.native_definition);
        block_scan.handle_definition_count = 0;
        block_scan.statement_count = 0;
        block_scan.valid = TRUE;
        DSL_IR_Lower_Scan_Standard_Block
            (request.standard_block, &block_scan);
        if (!block_scan.valid || block_scan.handle_definition_count != 1 ||
            block_scan.statement_count == 0)
            return DSL_IR_Lower_Report
                       (diagnostic, entry.request_index,
                        "standard block output or source position mismatch");
        entry.statement_count = block_scan.statement_count;
    }

    std::vector<DSL_REGION_SYMBOL_REDIRECT> region_redirects;
    for (UINT32 i = 0; i < journal.size(); ++i) {
        const DSL_IR_NATIVE_VALUE_LOWER_JOURNAL &entry = journal[i];
        if (region_symbol_uses[entry.value.st] == 0)
            continue;
        DSL_REGION_SYMBOL_REDIRECT redirect;
        redirect.old_st = entry.value.st;
        redirect.new_st = entry.handle_st;
        region_redirects.push_back(redirect);
    }
    if (!region_redirects.empty() &&
        !DSL_Region_Can_Apply_Lowering_Transitions
             (pu_info, &region_redirects[0], region_redirects.size(),
              NULL, 0))
        return DSL_IR_Lower_Report
                   (diagnostic, 0, "REGION transition is invalid");

    /* Commit in tree order so detached blocks replace their definitions. */
    std::sort(journal.begin(), journal.end(), DSL_IR_LOWER_JOURNAL_LESS());
    for (UINT32 i = 0; i < journal.size(); ++i) {
        DSL_IR_NATIVE_VALUE_LOWER_JOURNAL &entry = journal[i];
        const DSL_IR_NATIVE_VALUE_LOWER_REQUEST &request = *entry.request;
        if (request.mode ==
            DSL_IR_NATIVE_LOWER_COMPUTED_STANDARD_BLOCK) {
            while (WN_first(request.standard_block) != NULL) {
                WN *statement = WN_EXTRACT_FromBlock
                    (request.standard_block, WN_first(request.standard_block));
                WN_INSERT_BlockBefore
                    (entry.containing_block,
                     request.native_definition, statement);
            }
            WN_DELETE_Tree(request.standard_block);
        } else if (request.mode ==
                       DSL_IR_NATIVE_LOWER_PROMOTED_SOURCE_ELISION &&
                   entry.local_projection_st != ST_IDX_ZERO) {
            TYPE_ID mtype = TY_mtype(entry.handle_ty);
            WN *load = WN_CreateLdid
                (OPR_LDID, mtype, mtype, 0,
                 entry.handle_st, entry.handle_ty);
            WN *copy = WN_CreateStid
                (OPR_STID, MTYPE_V, mtype, 0,
                 entry.local_projection_st, entry.handle_ty, load);
            WN_Set_Linenum
                (copy, WN_Get_Linenum(request.native_definition));
            WN_INSERT_BlockBefore
                (entry.containing_block, request.native_definition, copy);
        }
        WN *removed = WN_EXTRACT_FromBlock
                          (entry.containing_block,
                           request.native_definition);
        FmtAssert(removed == request.native_definition,
                  ("preflighted native lowering extraction failed"));
        WN_DELETE_Tree(removed);
    }
    for (UINT32 i = 0; i < journal.size(); ++i) {
        DSL_IR_NATIVE_VALUE_LOWER_JOURNAL &entry = journal[i];
        BOOL marked = entry.request->mode ==
            DSL_IR_NATIVE_LOWER_VERIFIED_DEAD_SOURCE_ELISION ?
            DSL_IR_Image_Mark_Value_Dead_Elided(entry.value.id) :
            DSL_IR_Image_Mark_Value_Lowered(entry.value.id);
        FmtAssert(marked,
                  ("preflighted native lowering image update failed"));
        DSL_IR_NATIVE_VALUE_LOWER_RESULT &result =
            results[entry.request_index];
        result.source_node_id = entry.node.id;
        result.source_value_id = entry.value.id;
        result.mode = entry.request->mode;
        result.relation_kind = entry.request->relation.relation_kind;
        result.value_projection_id =
            entry.request->mode !=
                DSL_IR_NATIVE_LOWER_VERIFIED_DEAD_SOURCE_ELISION &&
            entry.local_projection_id !=
                DSL_RUNTIME_VALUE_PROJECTION_INVALID_ID ?
                entry.local_projection_id :
                entry.request->relation.value_projection_id;
        result.runtime_input_id = entry.request->relation.runtime_input_id;
        result.runtime_binding_id =
            entry.request->relation.runtime_binding_id;
        result.handle_st = entry.handle_st;
        result.handle_ty = entry.handle_ty;
        result.inserted_statement_count = entry.statement_count;
    }
    if (!region_redirects.empty())
        FmtAssert(DSL_Region_Apply_Lowering_Transitions
                      (pu_info, &region_redirects[0],
                       region_redirects.size(), NULL, 0),
                  ("preflighted REGION lowering transition failed"));
    FmtAssert(DSL_IR_Image_Validate(NULL) &&
              DSL_IR_Image_Validate_Lowered_Relations(NULL) &&
              DSL_Region_Verify_PU(pu_info, NULL),
              ("lowered DSL value failed postcondition"));
    return TRUE;
}

/* Reuse the complete native-lowering transaction for dead constant batches.
 * The stricter identity record prevents a caller from selecting a different
 * definition after its deadness decision was made. */
BOOL
DSL_IR_Elide_Dead_Native_Constants
        (PU_Info *pu_info,
         const DSL_IR_DEAD_NATIVE_CONSTANT_REQUEST *requests,
         UINT32 request_count,
         FILE *diagnostic,
         UINT32 *elided_count)
{
    if (!DSL_IR_Lower_Current_PU_Is(pu_info) ||
        PU_Info_tree_ptr(pu_info) == NULL || requests == NULL ||
        request_count == 0 || elided_count == NULL ||
        !DSL_IR_Image_Validate(NULL) ||
        !DSL_IR_Image_Validate_Lowered_Relations(NULL) ||
        !DSL_Region_Verify_PU(pu_info, NULL))
        return DSL_IR_Lower_Report
                   (diagnostic, 0, "dead-constant transaction is invalid");

    std::vector<DSL_IR_NATIVE_VALUE_LOWER_REQUEST> lower_requests
        (request_count);
    std::vector<DSL_IR_NATIVE_VALUE_LOWER_RESULT> lower_results
        (request_count);
    DSL_IR_LOWER_VALUE_INDEX values;
    if (!DSL_IR_Lower_Build_Value_Index(pu_info, &values))
        return DSL_IR_Lower_Report
                   (diagnostic, 0, "value indexing failed");
    for (UINT32 i = 0; i < request_count; ++i) {
        const DSL_IR_DEAD_NATIVE_CONSTANT_REQUEST &request = requests[i];
        DSL_IR_VALUE_RECORD value;
        DSL_IR_NODE_RECORD node;
        if (request.reserved != 0 || request.expected_version == 0 ||
            request.owner_pu_st != PU_Info_proc_sym(pu_info) ||
            request.native_definition == NULL ||
            WN_operator(request.native_definition) != OPR_STID ||
            WN_st_idx(request.native_definition) != request.st ||
            WN_ty(request.native_definition) != request.ty ||
            !DSL_IR_Lower_Find_Native_Definition_Value
                (request.native_definition, request.value_id,
                 DSL_IR_NATIVE_LOWER_VERIFIED_DEAD_SOURCE_ELISION,
                 values, &value) ||
            value.id != request.value_id || value.st != request.st ||
            value.ty != request.ty ||
            value.producer_node_id != request.node_id ||
            !DSL_IR_Image_Get_Node(request.node_id, &node) ||
            node.result_value_id != value.id)
            return DSL_IR_Lower_Report
                       (diagnostic, i, "dead-constant identity mismatch");

        DSL_IR_NATIVE_VALUE_LOWER_REQUEST &lower = lower_requests[i];
        memset(&lower, 0, sizeof(lower));
        lower.native_definition = request.native_definition;
        lower.source_value_id = request.value_id;
        lower.expected_operator = OPR_DSLTENSORCONST;
        lower.expected_version = request.expected_version;
        lower.mode = DSL_IR_NATIVE_LOWER_VERIFIED_DEAD_SOURCE_ELISION;
        lower.relation.relation_kind =
            DSL_IR_LOWER_RELATION_VERIFIED_DEAD_SOURCE;
    }
    if (!DSL_IR_Lower_Native_Values_To_Standard_Blocks
            (pu_info, &lower_requests[0], request_count, diagnostic,
             &lower_results[0]))
        return FALSE;
    *elided_count = request_count;
    return TRUE;
}
