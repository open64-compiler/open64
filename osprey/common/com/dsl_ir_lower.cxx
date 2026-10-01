/*
 * Copyright (C) 2026 Open64 Project
 *
 * Atomic lowering of native DSL value definitions to standard WHIRL blocks.
 * See doc/FHE-SYNC5-STANDARD-WHIRL-LOWERING-CONTRACT.md and
 * doc/FHE-SYNC5-NATIVE-OWNERSHIP-AUDIT.md.
 */

#include <algorithm>
#include <string.h>
#include <vector>

#include "dsl_memory_behavior.h"
#include "dsl_ir_image.h"
#include "dsl_region.h"
#include "pu_info.h"
#include "strtab.h"
#include "symtab.h"
#include "wn.h"
#include "wn_util.h"

/* Commit-only image mutation after complete transaction preflight. */
extern BOOL DSL_IR_Image_Mark_Value_Lowered (DSL_IR_VALUE_ID);
extern UINT32 DSL_Region_Symbol_Use_Count (PU_Info *, ST_IDX);

static BOOL
DSL_IR_Lower_Current_PU_Is (ST_IDX owner_pu_st)
{
    return Current_PU_Info != NULL &&
           PU_Info_proc_sym(Current_PU_Info) == owner_pu_st;
}

static BOOL
DSL_IR_Lower_Block_Contains (const WN *block, const WN *statement)
{
    if (block == NULL || WN_operator(block) != OPR_BLOCK || statement == NULL)
        return FALSE;
    for (const WN *current = WN_first(block); current != NULL;
         current = WN_next(current)) {
        if (current == statement)
            return TRUE;
    }
    return FALSE;
}

typedef struct {
    const DSL_IR_NATIVE_VALUE_LOWER_REQUEST *request;
    UINT32 request_index;
    UINT32 tree_order;
    UINT32 statement_count;
    DSL_IR_VALUE_RECORD value;
    DSL_IR_NODE_RECORD node;
    DSL_IR_OPCODE_DESCRIPTOR_RECORD opcode;
    ST_IDX handle_st;
    TY_IDX handle_ty;
} DSL_IR_NATIVE_VALUE_LOWER_JOURNAL;

static BOOL
DSL_IR_Lower_Find_Native_Definition_Value
        (ST_IDX owner_pu_st,
         const WN *definition,
         UINT32 mode,
         DSL_IR_VALUE_RECORD *value_record)
{
    if (DSL_IR_Image_Find_Definition_Value
            (owner_pu_st, definition, value_record))
        return TRUE;
    if (mode != DSL_IR_NATIVE_LOWER_PROMOTED_SOURCE_ELISION ||
        definition == NULL || WN_operator(definition) != OPR_STID ||
        WN_kid_count(definition) != 1 || WN_kid0(definition) == NULL ||
        !DSL_WN_Is_Native(WN_kid0(definition)))
        return FALSE;
    DSL_LOGICAL_OPCODE logical_opcode;
    if (!DSL_WN_Get_Logical_Opcode
            (WN_kid0(definition), &logical_opcode, NULL))
        return FALSE;
    ST_IDX st = WN_st_idx(definition);
    DSL_IR_VALUE_RECORD value;
    DSL_IR_NODE_RECORD node;
    DSL_IR_OPCODE_DESCRIPTOR_RECORD opcode;
    if (ST_IDX_level(st) != CURRENT_SYMTAB || ST_IDX_index(st) == 0 ||
        ST_IDX_index(st) >= ST_Table_Size(CURRENT_SYMTAB) ||
        ST_type(St_Table[st]) != WN_ty(definition) ||
        !DSL_IR_Image_Find_PU_Value
            (st, ST_name(St_Table[st]), ST_name(St_Table[owner_pu_st]),
             &value) ||
        value.value_kind != DSL_IR_VALUE_CONSTANT || value.st != st ||
        value.ty != WN_ty(definition) ||
        !DSL_IR_Image_Get_Node(value.producer_node_id, &node) ||
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

static BOOL
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

static BOOL
DSL_IR_Lower_Tree_Contains (const WN *tree, const WN *target)
{
    if (tree == NULL)
        return FALSE;
    if (tree == target)
        return TRUE;
    if (WN_operator(tree) == OPR_BLOCK) {
        for (const WN *stmt = WN_first(tree); stmt != NULL;
             stmt = WN_next(stmt)) {
            if (DSL_IR_Lower_Tree_Contains(stmt, target))
                return TRUE;
        }
        return FALSE;
    }
    for (INT32 i = 0; i < WN_kid_count(tree); ++i) {
        if (DSL_IR_Lower_Tree_Contains(WN_kid(tree, i), target))
            return TRUE;
    }
    return FALSE;
}

static BOOL
DSL_IR_Lower_Find_Tree_Order
        (const WN *tree,
         const WN *target,
         UINT32 *next_order,
         UINT32 *target_order)
{
    if (tree == NULL)
        return FALSE;
    UINT32 order = (*next_order)++;
    if (tree == target) {
        *target_order = order;
        return TRUE;
    }
    if (WN_operator(tree) == OPR_BLOCK) {
        for (const WN *stmt = WN_first(tree); stmt != NULL;
             stmt = WN_next(stmt)) {
            if (DSL_IR_Lower_Find_Tree_Order
                    (stmt, target, next_order, target_order))
                return TRUE;
        }
        return FALSE;
    }
    for (INT32 i = 0; i < WN_kid_count(tree); ++i) {
        if (DSL_IR_Lower_Find_Tree_Order
                (WN_kid(tree, i), target, next_order, target_order))
            return TRUE;
    }
    return FALSE;
}

static const DSL_IR_NATIVE_VALUE_LOWER_JOURNAL *
DSL_IR_Lower_Find_Definition
        (const std::vector<DSL_IR_NATIVE_VALUE_LOWER_JOURNAL> &journal,
         const WN *definition)
{
    for (UINT32 i = 0; i < journal.size(); ++i) {
        if (journal[i].request->native_definition == definition)
            return &journal[i];
    }
    return NULL;
}

static const DSL_IR_NATIVE_VALUE_LOWER_JOURNAL *
DSL_IR_Lower_Find_Node
        (const std::vector<DSL_IR_NATIVE_VALUE_LOWER_JOURNAL> &journal,
         DSL_IR_NODE_ID node_id)
{
    for (UINT32 i = 0; i < journal.size(); ++i) {
        if (journal[i].node.id == node_id)
            return &journal[i];
    }
    return NULL;
}

typedef struct {
    ST_IDX source_st;
    const WN *source_definition;
    const std::vector<DSL_IR_NATIVE_VALUE_LOWER_JOURNAL> *journal;
    UINT32 definition_count;
    BOOL valid;
} DSL_IR_LOWER_SOURCE_USE_SCAN;

static void
DSL_IR_Lower_Scan_Source_Uses
        (const WN *tree,
         const WN *native_definition,
         DSL_IR_LOWER_SOURCE_USE_SCAN *scan)
{
    if (tree == NULL || !scan->valid)
        return;
    const WN *enclosing_definition = native_definition;
    if (WN_operator(tree) == OPR_STID && WN_kid_count(tree) == 1 &&
        WN_kid0(tree) != NULL && DSL_WN_Is_Native(WN_kid0(tree)))
        enclosing_definition = tree;
    if (tree == scan->source_definition)
        ++scan->definition_count;
    if (WN_has_sym(tree) && WN_st_idx(tree) == scan->source_st) {
        BOOL source_definition = tree == scan->source_definition;
        BOOL request_operand = WN_operator(tree) == OPR_LDID &&
            enclosing_definition != NULL &&
            DSL_IR_Lower_Find_Definition
                (*scan->journal, enclosing_definition) != NULL;
        if (!source_definition && !request_operand)
            scan->valid = FALSE;
    }
    if (WN_operator(tree) == OPR_BLOCK) {
        for (const WN *stmt = WN_first(tree); stmt != NULL;
             stmt = WN_next(stmt))
            DSL_IR_Lower_Scan_Source_Uses(stmt, NULL, scan);
        return;
    }
    for (INT32 i = 0; i < WN_kid_count(tree); ++i)
        DSL_IR_Lower_Scan_Source_Uses
            (WN_kid(tree, i), enclosing_definition, scan);
}

typedef struct {
    ST_IDX source_st;
    ST_IDX handle_st;
    const WN *result_handle_definition;
    SRCPOS source_position;
    UINT32 handle_definition_count;
    UINT32 statement_count;
    BOOL valid;
} DSL_IR_LOWER_STANDARD_BLOCK_SCAN;

static void
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

static BOOL
DSL_IR_Lower_Logical_Uses_Closed
        (const DSL_IR_NATIVE_VALUE_LOWER_JOURNAL &entry,
         const std::vector<DSL_IR_NATIVE_VALUE_LOWER_JOURNAL> &journal)
{
    for (UINT32 i = 1; i <= DSL_IR_Image_Value_Reference_Count(); ++i) {
        DSL_IR_VALUE_REFERENCE_RECORD reference;
        DSL_IR_NODE_RECORD owner;
        if (!DSL_IR_Image_Get_Value_Reference(i, &reference) ||
            reference.value_id != entry.value.id)
            continue;
        if (!DSL_IR_Image_Get_Node(reference.owner_node_id, &owner))
            return FALSE;
        if ((owner.flags & DSL_IR_NODE_FLAG_LOWERED) == 0 &&
            DSL_IR_Lower_Find_Node(journal, owner.id) == NULL)
            return FALSE;
    }
    for (UINT32 i = 1; i <= DSL_Call_ABI_Image_Argument_Count(); ++i) {
        DSL_CALL_ARGUMENT_RECORD argument;
        if (!DSL_Call_ABI_Image_Get_Argument(i, &argument) ||
            argument.argument_value_id != entry.value.id)
            continue;
        DSL_RUNTIME_CALL_PROJECTION_RECORD call;
        if (!DSL_Runtime_Interface_Image_Find_Call
                (argument.callsite_id, argument.actual_ordinal, &call) ||
            call.source_value_id != entry.value.id)
            return FALSE;
    }
    return TRUE;
}

static BOOL
DSL_IR_Lower_Relation_Resolve
        (ST_IDX owner_pu_st,
         const DSL_IR_NATIVE_VALUE_LOWER_REQUEST &request,
         const DSL_IR_VALUE_RECORD &value,
         ST_IDX *handle_st,
         TY_IDX *handle_ty)
{
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
        for (UINT32 i = 1;
             i <= DSL_Program_Interface_Image_Runtime_Input_Count(); ++i) {
            DSL_RUNTIME_INPUT_RECORD input;
            if (DSL_Program_Interface_Image_Get_Runtime_Input(i, &input) &&
                input.source_value_id == value.id)
                return FALSE;
        }
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
    if (DSL_Runtime_Interface_Image_Find_Value
            (owner_pu_st, value.id, &projection))
        return FALSE;
    *handle_st = binding.handle_st;
    *handle_ty = binding.handle_ty;
    return TRUE;
}

struct DSL_IR_LOWER_JOURNAL_LESS {
    BOOL operator()
        (const DSL_IR_NATIVE_VALUE_LOWER_JOURNAL &left,
         const DSL_IR_NATIVE_VALUE_LOWER_JOURNAL &right) const
    {
        return left.tree_order < right.tree_order;
    }
};

BOOL
DSL_IR_Lower_Native_Values_To_Standard_Blocks
        (ST_IDX owner_pu_st,
         const DSL_IR_NATIVE_VALUE_LOWER_REQUEST *requests,
         UINT32 request_count,
         FILE *diagnostic,
         DSL_IR_NATIVE_VALUE_LOWER_RESULT *results)
{
    if (!DSL_IR_Lower_Current_PU_Is(owner_pu_st) ||
        Current_PU_Info == NULL ||
        PU_Info_proc_sym(Current_PU_Info) != owner_pu_st ||
        requests == NULL || request_count == 0 || results == NULL)
        return DSL_IR_Lower_Report(diagnostic, 0, "transaction is incomplete");
    memset(results, 0,
           request_count * sizeof(DSL_IR_NATIVE_VALUE_LOWER_RESULT));

    /* Prove the complete physical/logical replacement set before mutation. */
    std::vector<DSL_IR_NATIVE_VALUE_LOWER_JOURNAL> journal;
    for (UINT32 i = 0; i < request_count; ++i) {
        const DSL_IR_NATIVE_VALUE_LOWER_REQUEST &request = requests[i];
        DSL_IR_NATIVE_VALUE_LOWER_JOURNAL entry;
        memset(&entry, 0, sizeof(entry));
        entry.request = &request;
        entry.request_index = i;
        if (request.reserved != 0 ||
            request.pu_root != PU_Info_tree_ptr(Current_PU_Info) ||
            request.containing_block == NULL ||
            WN_operator(request.containing_block) != OPR_BLOCK ||
            !DSL_IR_Lower_Block_Contains
                (request.containing_block, request.native_definition))
            return DSL_IR_Lower_Report
                       (diagnostic, i, "PU or containing block mismatch");
        if (!DSL_IR_Lower_Find_Native_Definition_Value
                (owner_pu_st, request.native_definition, request.mode,
                 &entry.value) ||
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
        if (entry.node.flags != DSL_IR_NODE_FLAG_NONE ||
            entry.value.flags != DSL_IR_VALUE_FLAG_NONE ||
            !DSL_Tensor_Has_Unique_Ownership(entry.value.st) ||
            WN_Get_Linenum(request.native_definition) == 0 ||
            DSL_Region_Symbol_Use_Count
                (Current_PU_Info, entry.value.st) != 0)
            return DSL_IR_Lower_Report
                       (diagnostic, i, "source ownership or use mismatch");
        if (!DSL_IR_Lower_Relation_Resolve
                (owner_pu_st, request, entry.value,
                 &entry.handle_st, &entry.handle_ty))
            return DSL_IR_Lower_Report
                       (diagnostic, i, "runtime relation mismatch");
        UINT32 order = 0;
        if (!DSL_IR_Lower_Find_Tree_Order
                (request.pu_root, request.native_definition, &order,
                 &entry.tree_order))
            return DSL_IR_Lower_Report
                       (diagnostic, i, "definition is outside the PU tree");
        for (UINT32 j = 0; j < journal.size(); ++j) {
            if (journal[j].value.id == entry.value.id ||
                journal[j].node.id == entry.node.id ||
                journal[j].request->native_definition ==
                    request.native_definition ||
                (request.standard_block != NULL &&
                 journal[j].request->standard_block ==
                    request.standard_block) ||
                (request.result_handle_definition != NULL &&
                 journal[j].request->result_handle_definition ==
                    request.result_handle_definition))
                return DSL_IR_Lower_Report
                           (diagnostic, i, "duplicate transaction member");
        }
        journal.push_back(entry);
    }

    for (UINT32 i = 0; i < journal.size(); ++i) {
        DSL_IR_NATIVE_VALUE_LOWER_JOURNAL &entry = journal[i];
        const DSL_IR_NATIVE_VALUE_LOWER_REQUEST &request = *entry.request;
        for (UINT32 effect_id = 1;
             effect_id <= DSL_Effect_Image_State_Effect_Count();
             ++effect_id) {
            DSL_STATE_EFFECT_RECORD effect;
            if (!DSL_Effect_Image_Get_State_Effect(effect_id, &effect) ||
                effect.owner_node_id == entry.node.id)
                return DSL_IR_Lower_Report
                           (diagnostic, entry.request_index,
                            "source node has a state effect");
        }
        DSL_IR_LOWER_SOURCE_USE_SCAN source_scan;
        source_scan.source_st = entry.value.st;
        source_scan.source_definition = request.native_definition;
        source_scan.journal = &journal;
        source_scan.definition_count = 0;
        source_scan.valid = TRUE;
        DSL_IR_Lower_Scan_Source_Uses(request.pu_root, NULL, &source_scan);
        if (!source_scan.valid || source_scan.definition_count != 1 ||
            !DSL_IR_Lower_Logical_Uses_Closed(entry, journal))
            return DSL_IR_Lower_Report
                       (diagnostic, entry.request_index,
                        "source value has an unlowered or escaping use");

        if (request.mode ==
                DSL_IR_NATIVE_LOWER_PROMOTED_SOURCE_ELISION) {
            if (request.standard_block != NULL ||
                request.result_handle_definition != NULL)
                return DSL_IR_Lower_Report
                           (diagnostic, entry.request_index,
                            "promoted source must not provide statements");
            continue;
        }
        if (request.mode !=
                DSL_IR_NATIVE_LOWER_COMPUTED_STANDARD_BLOCK ||
            request.standard_block == NULL ||
            WN_operator(request.standard_block) != OPR_BLOCK ||
            WN_first(request.standard_block) == NULL ||
            request.result_handle_definition == NULL ||
            !DSL_IR_Lower_Block_Contains
                (request.standard_block,
                 request.result_handle_definition) ||
            WN_last(request.standard_block) !=
                request.result_handle_definition ||
            DSL_IR_Lower_Tree_Contains
                (request.pu_root, request.standard_block) ||
            WN_operator(request.result_handle_definition) != OPR_STID ||
            WN_st_idx(request.result_handle_definition) != entry.handle_st ||
            WN_ty(request.result_handle_definition) != entry.handle_ty)
            return DSL_IR_Lower_Report
                       (diagnostic, entry.request_index,
                        "computed standard block is invalid");
        DSL_IR_LOWER_STANDARD_BLOCK_SCAN block_scan;
        block_scan.source_st = entry.value.st;
        block_scan.handle_st = entry.handle_st;
        block_scan.result_handle_definition =
            request.result_handle_definition;
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
                    (request.containing_block,
                     request.native_definition, statement);
            }
            WN_DELETE_Tree(request.standard_block);
        }
        WN *removed = WN_EXTRACT_FromBlock
                          (request.containing_block,
                           request.native_definition);
        FmtAssert(removed == request.native_definition,
                  ("preflighted native lowering extraction failed"));
        WN_DELETE_Tree(removed);
    }
    for (UINT32 i = 0; i < journal.size(); ++i) {
        DSL_IR_NATIVE_VALUE_LOWER_JOURNAL &entry = journal[i];
        FmtAssert(DSL_IR_Image_Mark_Value_Lowered(entry.value.id),
                  ("preflighted native lowering image update failed"));
        DSL_IR_NATIVE_VALUE_LOWER_RESULT &result =
            results[entry.request_index];
        result.source_node_id = entry.node.id;
        result.source_value_id = entry.value.id;
        result.mode = entry.request->mode;
        result.relation_kind = entry.request->relation.relation_kind;
        result.value_projection_id =
            entry.request->relation.value_projection_id;
        result.runtime_input_id = entry.request->relation.runtime_input_id;
        result.runtime_binding_id =
            entry.request->relation.runtime_binding_id;
        result.handle_st = entry.handle_st;
        result.handle_ty = entry.handle_ty;
        result.inserted_statement_count = entry.statement_count;
    }
    FmtAssert(DSL_IR_Image_Validate(NULL) &&
              DSL_IR_Image_Validate_Lowered_Relations(NULL) &&
              DSL_Region_Verify_PU(Current_PU_Info, NULL),
              ("lowered DSL value failed postcondition"));
    return TRUE;
}
