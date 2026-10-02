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
extern BOOL DSL_IR_Image_Mark_Value_Dead_Elided (DSL_IR_VALUE_ID);
extern UINT32 DSL_Region_Symbol_Use_Count (PU_Info *, ST_IDX);

/* Confirm that the caller selected the PU whose local WHIRL state is active. */
static BOOL
DSL_IR_Lower_Current_PU_Is (PU_Info *pu_info)
{
    return pu_info != NULL && Current_PU_Info == pu_info;
}

/* Find the innermost BLOCK that directly or recursively contains a statement. */
static WN *
DSL_IR_Lower_Find_Containing_Block (WN *tree, const WN *target)
{
    if (tree == NULL || target == NULL)
        return NULL;
    if (WN_operator(tree) == OPR_BLOCK) {
        for (WN *current = WN_first(tree); current != NULL;
             current = WN_next(current)) {
            if (current == target)
                return tree;
            WN *containing = DSL_IR_Lower_Find_Containing_Block
                                  (current, target);
            if (containing != NULL)
                return containing;
        }
        return NULL;
    }
    for (INT32 i = 0; i < WN_kid_count(tree); ++i) {
        WN *containing = DSL_IR_Lower_Find_Containing_Block
                              (WN_kid(tree, i), target);
        if (containing != NULL)
            return containing;
    }
    return NULL;
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
    ST_IDX call_projection_st;
    DSL_RUNTIME_VALUE_PROJECTION_ID call_projection_id;
    WN *containing_block;
    WN *result_handle_definition;
} DSL_IR_NATIVE_VALUE_LOWER_JOURNAL;

/* Resolve a native definition to its image value, including promoted sources. */
static BOOL
DSL_IR_Lower_Find_Native_Definition_Value
        (PU_Info *pu_info,
         const WN *definition,
         UINT32 mode,
         DSL_IR_VALUE_RECORD *value_record)
{
    if (DSL_IR_Image_Find_Definition_Value
            (pu_info, definition, value_record))
        return TRUE;
    if ((mode != DSL_IR_NATIVE_LOWER_PROMOTED_SOURCE_ELISION &&
         mode != DSL_IR_NATIVE_LOWER_VERIFIED_DEAD_SOURCE_ELISION) ||
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
    ST_IDX owner_pu_st = PU_Info_proc_sym(pu_info);
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

/* Emit one stable diagnostic for a request and convert failure to FALSE. */
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

/* Test whether a detached standard block is already attached to the PU tree. */
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

/* Assign deterministic preorder positions used to order physical commits. */
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

/* Locate a journal entry by its native definition during source-use scans. */
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

/* Locate a journal entry by logical node identity during closure checks. */
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

/* Reject source-symbol reads, definitions, and escapes outside this transaction. */
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

/* Validate a detached replacement block and its sole final handle definition. */
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

/* Prove that every logical value reference is lowered or in this request set. */
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
        if ((owner.flags & (DSL_IR_NODE_FLAG_RETIRED |
                            DSL_IR_NODE_FLAG_LOWERED |
                            DSL_IR_NODE_FLAG_DEAD_ELIDED)) == 0 &&
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

/* Resolve a computed projection or promoted root input to its runtime handle. */
static BOOL
DSL_IR_Lower_Relation_Resolve
        (ST_IDX owner_pu_st,
         const DSL_IR_NATIVE_VALUE_LOWER_REQUEST &request,
         const DSL_IR_VALUE_RECORD &value,
         ST_IDX *handle_st,
         TY_IDX *handle_ty,
         ST_IDX *call_projection_st,
         DSL_RUNTIME_VALUE_PROJECTION_ID *call_projection_id)
{
    *call_projection_st = ST_IDX_ZERO;
    *call_projection_id = DSL_RUNTIME_VALUE_PROJECTION_INVALID_ID;
    if (request.mode ==
            DSL_IR_NATIVE_LOWER_VERIFIED_DEAD_SOURCE_ELISION) {
        if (request.relation.relation_kind !=
                DSL_IR_LOWER_RELATION_VERIFIED_DEAD_SOURCE ||
            request.relation.value_projection_id != 0 ||
            request.relation.runtime_input_id != 0 ||
            request.relation.runtime_binding_id != 0 ||
            value.value_kind != DSL_IR_VALUE_CONSTANT)
            return FALSE;
        for (UINT32 i = 1;
             i <= DSL_Runtime_Interface_Image_Value_Count(); ++i) {
            DSL_RUNTIME_VALUE_PROJECTION_RECORD projection;
            if (!DSL_Runtime_Interface_Image_Get_Value(i, &projection) ||
                projection.source_value_id == value.id)
                return FALSE;
        }
        for (UINT32 i = 1;
             i <= DSL_Program_Interface_Image_Runtime_Input_Count(); ++i) {
            DSL_RUNTIME_INPUT_RECORD input;
            if (!DSL_Program_Interface_Image_Get_Runtime_Input(i, &input) ||
                input.source_value_id == value.id)
                return FALSE;
        }
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
    UINT32 projection_count = 0;
    for (UINT32 i = 1;
         i <= DSL_Runtime_Interface_Image_Value_Count(); ++i) {
        DSL_RUNTIME_VALUE_PROJECTION_RECORD current;
        if (!DSL_Runtime_Interface_Image_Get_Value(i, &current))
            return FALSE;
        if (current.owner_pu_st != owner_pu_st ||
            current.source_value_id != value.id)
            continue;
        projection = current;
        ++projection_count;
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
        *call_projection_st = projection.handle_st;
        *call_projection_id = projection.id;
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
    std::vector<DSL_IR_NATIVE_VALUE_LOWER_JOURNAL> journal;
    for (UINT32 i = 0; i < request_count; ++i) {
        const DSL_IR_NATIVE_VALUE_LOWER_REQUEST &request = requests[i];
        DSL_IR_NATIVE_VALUE_LOWER_JOURNAL entry;
        memset(&entry, 0, sizeof(entry));
        entry.request = &request;
        entry.request_index = i;
        entry.containing_block = DSL_IR_Lower_Find_Containing_Block
                                    (pu_root, request.native_definition);
        if (request.standard_block != NULL &&
            WN_operator(request.standard_block) == OPR_BLOCK)
            entry.result_handle_definition = WN_last(request.standard_block);
        if (request.reserved != 0 || entry.containing_block == NULL)
            return DSL_IR_Lower_Report
                       (diagnostic, i, "PU or containing block mismatch");
        if (!DSL_IR_Lower_Find_Native_Definition_Value
                (pu_info, request.native_definition, request.mode,
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
                (pu_info, entry.value.st) != 0)
            return DSL_IR_Lower_Report
                       (diagnostic, i, "source ownership or use mismatch");
        if (!DSL_IR_Lower_Relation_Resolve
                (owner_pu_st, request, entry.value,
                 &entry.handle_st, &entry.handle_ty,
                 &entry.call_projection_st,
                 &entry.call_projection_id))
            return DSL_IR_Lower_Report
                       (diagnostic, i, "runtime relation mismatch");
        UINT32 order = 0;
        if (!DSL_IR_Lower_Find_Tree_Order
                (pu_root, request.native_definition, &order,
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
                (entry.result_handle_definition != NULL &&
                 journal[j].result_handle_definition ==
                    entry.result_handle_definition))
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
        DSL_IR_Lower_Scan_Source_Uses(pu_root, NULL, &source_scan);
        if (!source_scan.valid || source_scan.definition_count != 1 ||
            !DSL_IR_Lower_Logical_Uses_Closed(entry, journal))
            return DSL_IR_Lower_Report
                       (diagnostic, entry.request_index,
                        "source value has an unlowered or escaping use");

        if (request.mode ==
                DSL_IR_NATIVE_LOWER_PROMOTED_SOURCE_ELISION ||
            request.mode ==
                DSL_IR_NATIVE_LOWER_VERIFIED_DEAD_SOURCE_ELISION) {
            if (request.standard_block != NULL ||
                (entry.call_projection_st != ST_IDX_ZERO &&
                 entry.containing_block != WN_func_body(pu_root)) ||
                (request.mode ==
                     DSL_IR_NATIVE_LOWER_VERIFIED_DEAD_SOURCE_ELISION &&
                 (entry.opcode.logical_operator != OPR_DSLTENSORCONST ||
                  entry.node.operand_count != 0 ||
                  entry.value.value_kind != DSL_IR_VALUE_CONSTANT)))
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
            DSL_IR_Lower_Tree_Contains(pu_root, request.standard_block) ||
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
        } else if (entry.call_projection_st != ST_IDX_ZERO) {
            TYPE_ID mtype = TY_mtype(entry.handle_ty);
            WN *load = WN_CreateLdid
                (OPR_LDID, mtype, mtype, 0,
                 entry.handle_st, entry.handle_ty);
            WN *copy = WN_CreateStid
                (OPR_STID, MTYPE_V, mtype, 0,
                 entry.call_projection_st, entry.handle_ty, load);
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
            entry.call_projection_id !=
                DSL_RUNTIME_VALUE_PROJECTION_INVALID_ID ?
                entry.call_projection_id :
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
              DSL_Region_Verify_PU(pu_info, NULL),
              ("lowered DSL value failed postcondition"));
    return TRUE;
}
