/*
 * Copyright (C) 2026 Open64 Project
 *
 * Native DSL definition lookup, logical rewrite and redirect/retire transactions
 * for the caller-selected active PU. External tensor state/materialization and
 * physical ABI verification belong to their matching owner implementations.
 * Design: doc/WHIRL-DSL-INFRASTRUCTURE.md, persistent IR / service separation.
 * Public linkage, executable contracts and mapped WHIRL format are unchanged.
 */

#include <string.h>
#include <vector>

#include "dsl_ir_rewrite.h"
#include "../../common/ir_image/dsl_image_service.h"
#include "dsl_memory_behavior.h"
#include "dsl_region.h"
#include "dsl_shape.h"
#include "pu_info.h"
#include "strtab.h"
#include "symtab.h"
#include "wn.h"
#include "wn_util.h"
#include "dsl_ir_rewrite_queries_internal.h"

/* Commit a preflighted logical redirect/retirement over image rows. Caller
 * proves physical, owner and consumer legality and coordinates wider
 * rollback. */
extern BOOL DSL_IR_Image_Redirect_And_Retire_Value
                                (DSL_IR_VALUE_ID, DSL_IR_VALUE_ID, UINT32,
                                 BOOL);
/* Check image projection compatibility before a cross-TY view retirement; no
 * row mutation. */
extern BOOL DSL_Runtime_Interface_Image_Can_Retire_View
                                (ST_IDX, DSL_IR_VALUE_ID, DSL_IR_VALUE_ID);
/* Commit compatible view-projection retirement after owner preflight; no
 * physical WHIRL or canonical TY mutation. */
extern BOOL DSL_Runtime_Interface_Image_Retire_View
                                (ST_IDX, DSL_IR_VALUE_ID, DSL_IR_VALUE_ID);
/* Commit a checked projection source redirect over image rows; the physical
 * transaction owns preflight and combined rollback. */
extern BOOL DSL_Runtime_Interface_Image_Redirect_Value
                                (ST_IDX, DSL_IR_VALUE_ID, DSL_IR_VALUE_ID);
/* Count selected-PU REGION uses without activating another PU or modifying
 * its interface. */
extern UINT32 DSL_Region_Symbol_Use_Count (PU_Info *, ST_IDX);
/* Read-only REGION preflight for the caller-selected PU and
 * source/replacement symbols; no PU activation. */
extern BOOL DSL_Region_Can_Redirect_Symbol (PU_Info *, ST_IDX, ST_IDX);
/* Apply the previously checked REGION symbol redirect in the selected PU. The
 * owning transaction coordinates physical/image edits and rollback. */
extern BOOL DSL_Region_Redirect_Symbol (PU_Info *, ST_IDX, ST_IDX);

/*
 * Compare replacement attributes with the registered operator schema by
 * canonical name and required presence. It is a preflight predicate only.
 */
static BOOL
DSL_IR_Rewrite_Attributes_Match_Schema
        (const char *schema,
         const DSL_IR_ATTRIBUTE_RECORD *attributes,
         UINT32 attribute_count)
{
    UINT32 expected_count = 0;
    const char *begin = schema == NULL ? "" : schema;

    while (*begin != '\0') {
        const char *end = strchr(begin, ';');
        size_t length = end == NULL ? strlen(begin) :
                                      (size_t)(end - begin);
        if (length != 0) {
            UINT32 matches = 0;
            ++expected_count;
            for (UINT32 i = 0; i < attribute_count; ++i) {
                const char *name = Index_To_Str(attributes[i].name);
                if (strlen(name) == length &&
                    strncmp(name, begin, length) == 0)
                    ++matches;
            }
            if (matches != 1)
                return FALSE;
        }
        if (end == NULL)
            break;
        begin = end + 1;
    }
    return expected_count == attribute_count;
}

/*
 * Resolve a native STID definition to its exact logical result value in the
 * active owner PU. The lookup is read-only and rejects ambiguous local STs.
 */
BOOL
DSL_IR_Image_Find_Definition_Value
        (PU_Info *pu_info,
         const WN *definition,
         DSL_IR_VALUE_RECORD *value_record)
{
    DSL_IR_VALUE_RECORD value;
    DSL_IR_NODE_RECORD node;
    DSL_IR_OPCODE_DESCRIPTOR_RECORD descriptor;
    DSL_LOGICAL_OPCODE logical_opcode;
    const WN *expression;
    ST_IDX result_st;

    ST_IDX owner_pu_st = pu_info == NULL ? ST_IDX_ZERO :
        PU_Info_proc_sym(pu_info);
    if (pu_info != Current_PU_Info ||
        !DSL_IR_Image_Current_PU_Is(owner_pu_st) || definition == NULL ||
        WN_operator(definition) != OPR_STID ||
        WN_kid_count(definition) != 1 || WN_kid0(definition) == NULL)
        return FALSE;
    expression = WN_kid0(definition);
    if (!DSL_WN_Is_Native(expression) ||
        !DSL_WN_Get_Logical_Opcode(expression, &logical_opcode, NULL))
        return FALSE;

    result_st = WN_st_idx(definition);
    if (ST_IDX_index(result_st) == 0 ||
        ST_IDX_level(result_st) != CURRENT_SYMTAB ||
        ST_IDX_index(result_st) >= ST_Table_Size(CURRENT_SYMTAB) ||
        ST_type(St_Table[result_st]) != WN_ty(definition) ||
        !DSL_IR_Image_Find_PU_Value
            (result_st, ST_name(St_Table[result_st]),
             ST_name(St_Table[owner_pu_st]), &value) ||
        value.value_kind != DSL_IR_VALUE_OPERATOR_RESULT ||
        value.st != result_st || value.ty != WN_ty(definition) ||
        !DSL_IR_Image_Get_Node(value.producer_node_id, &node) ||
        node.result_value_id != value.id ||
        !DSL_IR_Image_Get_Opcode_Descriptor
            (node.opcode_descriptor_id, &descriptor) ||
        descriptor.logical_operator != logical_opcode.dsl_operator ||
        descriptor.version != logical_opcode.source_version ||
        node.operand_count != (UINT32)WN_kid_count(expression))
        return FALSE;
    if ((node.payload == STR_IDX_ZERO && logical_opcode.payload[0] != '\0') ||
        (node.payload != STR_IDX_ZERO &&
         strcmp(Index_To_Str(node.payload), logical_opcode.payload) != 0))
        return FALSE;

    if (value_record != NULL)
        *value_record = value;
    return TRUE;
}

/*
 * Replace one native DSL expression while preserving definition,
 * result ST/TY, node/value identity, owner, source position, and operands.
 * Physical/schema preflight precedes replacement allocation and image rewrite;
 * attach the new expression only after the logical rewrite succeeds. FALSE
 * preserves the original physical definition, not every temporary allocation,
 * shared string or ensured opcode row. Allocation exceptions are not journaled.
 */
BOOL
DSL_IR_Rewrite_Native_Value
        (PU_Info *pu_info,
         WN *definition,
         DSL_IR_VALUE_ID value_id,
         const DSL_IR_NATIVE_VALUE_REWRITE_REQUEST *request)
{
    DSL_IR_VALUE_RECORD result;
    DSL_IR_NODE_RECORD node;
    DSL_LOGICAL_OPCODE logical_opcode;
    DSL_OPERATOR_INFO replacement_info;
    std::vector<WN *> operands;

    ST_IDX owner_pu_st = pu_info == NULL ? ST_IDX_ZERO :
        PU_Info_proc_sym(pu_info);
    if (request == NULL ||
        !DSL_IR_Image_Find_Definition_Value
            (pu_info, definition, &result) ||
        result.id != value_id || WN_Get_Linenum(definition) == 0 ||
        !DSL_WN_Get_Logical_Opcode
            (WN_kid0(definition), &logical_opcode, NULL) ||
        logical_opcode.dsl_operator != request->expected_operator ||
        logical_opcode.source_version != request->expected_version ||
        !DSL_Operator_Get_Info_Version
            (request->replacement_operator, request->replacement_version,
             &replacement_info) ||
        (replacement_info.nkids >= 0 &&
         (UINT32)replacement_info.nkids != request->operand_count) ||
        (request->operand_count != 0 &&
         (request->operand_templates == NULL ||
          request->operand_value_ids == NULL)) ||
        (request->attribute_count != 0 && request->attributes == NULL) ||
        request->payload == STR_IDX_ZERO ||
        request->payload >= STR_Table_Size() ||
        request->result_value_kind != DSL_IR_VALUE_OPERATOR_RESULT ||
        !DSL_IR_Image_Get_Node(result.producer_node_id, &node))
        return FALSE;

    for (UINT32 i = 0; i < request->attribute_count; ++i) {
        const DSL_IR_ATTRIBUTE_RECORD &attribute = request->attributes[i];
        if (attribute.name == STR_IDX_ZERO ||
            attribute.name >= STR_Table_Size() ||
            attribute.value >= STR_Table_Size() ||
            attribute.value_kind <= DSL_IR_ATTRIBUTE_VALUE_UNKNOWN ||
            attribute.value_kind > DSL_IR_ATTRIBUTE_VALUE_SYMBOL)
            return FALSE;
    }
    if (!DSL_IR_Rewrite_Attributes_Match_Schema
             (replacement_info.attribute_schema, request->attributes,
              request->attribute_count))
        return FALSE;

    operands.reserve(request->operand_count);
    for (UINT32 i = 0; i < request->operand_count; ++i) {
        const WN *operand = request->operand_templates[i];
        DSL_IR_VALUE_RECORD operand_value;
        if (operand == NULL || WN_operator(operand) != OPR_LDID ||
            !DSL_IR_Image_Get_Value
                (request->operand_value_ids[i], &operand_value) ||
            !DSL_IR_Image_Value_Belongs_To_PU
                (operand_value, owner_pu_st) ||
            operand_value.st != WN_st_idx(operand) ||
            operand_value.ty != WN_ty(operand) ||
            ST_type(St_Table[operand_value.st]) != operand_value.ty)
            return FALSE;
    }

    for (UINT32 i = 0; i < request->operand_count; ++i)
        operands.push_back
            (WN_COPY_Tree(const_cast<WN *>(request->operand_templates[i])));
    WN *replacement = DSL_WN_Create_Native
                          (request->replacement_operator,
                           request->replacement_version,
                           request->payload == STR_IDX_ZERO ? "" :
                               Index_To_Str(request->payload),
                           operands.empty() ? NULL : &operands[0],
                           operands.size());
    if (replacement == NULL)
        return FALSE;

    DSL_IR_OPCODE_DESCRIPTOR_ID descriptor_id =
        DSL_IR_Image_Ensure_Opcode_Descriptor
            (request->replacement_operator, request->replacement_version);
    if (descriptor_id == DSL_IR_OPCODE_DESCRIPTOR_INVALID_ID)
        return FALSE;

    DSL_IR_NODE_REWRITE_REQUEST image_request;
    image_request.node_id = node.id;
    image_request.opcode_descriptor_id = descriptor_id;
    image_request.payload = request->payload;
    image_request.operand_value_ids = request->operand_value_ids;
    image_request.operand_count = request->operand_count;
    image_request.attributes = request->attributes;
    image_request.attribute_count = request->attribute_count;
    image_request.result_value_kind = request->result_value_kind;
    if (!DSL_IR_Image_Rewrite_Node(&image_request))
        return FALSE;

    WN_kid0(definition) = replacement;
    return TRUE;
}

typedef struct {
    ST_IDX retiring_st;
    WN *retiring_definition;
    BOOL retiring_seen;
    BOOL valid;
    UINT32 definition_count;
    std::vector<WN *> reads;
} DSL_IR_RETIRE_USE_SCAN;

/*
 * Journal physical definitions, post-definition LDID uses, and forbidden
 * aliases for redirect/retire preflight. The traversal never mutates WHIRL.
 */
static void
DSL_IR_Retire_Scan_Tree (WN *wn, DSL_IR_RETIRE_USE_SCAN *scan)
{
    if (wn == NULL || scan == NULL || !scan->valid)
        return;
    if (WN_operator(wn) == OPR_BLOCK) {
        for (WN *statement = WN_first(wn); statement != NULL;
             statement = WN_next(statement)) {
            if (statement == scan->retiring_definition) {
                if (scan->retiring_seen) {
                    scan->valid = FALSE;
                    return;
                }
                for (INT32 kid = 0; kid < WN_kid_count(statement); ++kid)
                    DSL_IR_Retire_Scan_Tree(WN_kid(statement, kid), scan);
                ++scan->definition_count;
                scan->retiring_seen = TRUE;
            } else {
                DSL_IR_Retire_Scan_Tree(statement, scan);
            }
        }
        return;
    }

    if (WN_has_sym(wn) && WN_st_idx(wn) == scan->retiring_st) {
        if (WN_operator(wn) == OPR_STID) {
            ++scan->definition_count;
            if (wn != scan->retiring_definition)
                scan->valid = FALSE;
        } else if (WN_operator(wn) == OPR_LDID && scan->retiring_seen) {
            scan->reads.push_back(wn);
        } else {
            scan->valid = FALSE;
        }
    }
    for (INT32 kid = 0; scan->valid && kid < WN_kid_count(wn); ++kid)
        DSL_IR_Retire_Scan_Tree(WN_kid(wn, kid), scan);
}

/* Prove replacement-before-retirement order within one exact BLOCK. */
static BOOL
DSL_IR_Definition_Precedes
        (const WN *block, const WN *first, const WN *second)
{
    if (block == NULL || WN_operator(block) != OPR_BLOCK || first == NULL ||
        second == NULL)
        return FALSE;
    BOOL first_seen = FALSE;
    for (const WN *statement = WN_first(block); statement != NULL;
         statement = WN_next(statement)) {
        if (statement == first)
            first_seen = TRUE;
        if (statement == second)
            return first_seen;
    }
    return FALSE;
}



/* Recursively test physical symbol references without changing WNs; NULL has
 * no uses. */
static BOOL
DSL_IR_Retire_Has_Symbol_Use (const WN *wn, ST_IDX st)
{
    if (wn == NULL)
        return FALSE;
    if (WN_has_sym(wn) && WN_st_idx(wn) == st)
        return TRUE;
    if (WN_operator(wn) == OPR_BLOCK) {
        for (const WN *statement = WN_first(wn); statement != NULL;
             statement = WN_next(statement)) {
            if (DSL_IR_Retire_Has_Symbol_Use(statement, st))
                return TRUE;
        }
        return FALSE;
    }
    for (INT32 kid = 0; kid < WN_kid_count(wn); ++kid) {
        if (DSL_IR_Retire_Has_Symbol_Use(WN_kid(wn, kid), st))
            return TRUE;
    }
    return FALSE;
}

/*
 * Atomically redirect all owner-safe uses to a dominating replacement and
 * retire one pure native definition. Tree, REGION interfaces, logical value
 * references, and status flags commit only after exhaustive preflight.
 */
static BOOL
DSL_IR_Redirect_And_Retire_Native_Value_Internal
        (PU_Info *pu_info,
         const DSL_IR_NATIVE_VALUE_RETIRE_REQUEST *request,
         BOOL view_redirect)
{
    ST_IDX owner_pu_st = pu_info == NULL ? ST_IDX_ZERO :
        PU_Info_proc_sym(pu_info);
    WN *pu_root = pu_info == NULL ? NULL : PU_Info_tree_ptr(pu_info);
    WN *containing_block = DSL_IR_Find_Containing_Block
        (pu_root, request == NULL ? NULL : request->retiring_definition);
    if (pu_info != Current_PU_Info || pu_root == NULL ||
        !DSL_IR_Image_Current_PU_Is(owner_pu_st) || request == NULL ||
        containing_block == NULL ||
        containing_block != DSL_IR_Find_Containing_Block
                                (pu_root, request->replacement_definition) ||
        !DSL_IR_Definition_Precedes
            (containing_block, request->replacement_definition,
             request->retiring_definition))
        return FALSE;

    DSL_IR_VALUE_RECORD replacement;
    DSL_IR_VALUE_RECORD retiring;
    if (!DSL_IR_Image_Find_Definition_Value
            (pu_info, request->replacement_definition, &replacement) ||
        !DSL_IR_Image_Find_Definition_Value
            (pu_info, request->retiring_definition, &retiring) ||
        replacement.id != request->replacement_value_id ||
        retiring.id != request->retiring_value_id ||
        (view_redirect ?
         (replacement.ty == retiring.ty ||
          !DSL_Shape_Representation_Preserving_View
              (replacement.ty, retiring.ty)) :
         replacement.ty != retiring.ty) ||
        !DSL_Tensor_Has_Unique_Ownership(retiring.st))
        return FALSE;

    DSL_IR_NODE_RECORD retiring_node;
    DSL_IR_OPCODE_DESCRIPTOR_RECORD retiring_opcode;
    if (!DSL_IR_Image_Get_Node
            (retiring.producer_node_id, &retiring_node) ||
        !DSL_IR_Image_Get_Opcode_Descriptor
            (retiring_node.opcode_descriptor_id, &retiring_opcode) ||
        retiring_opcode.logical_operator !=
            request->expected_retiring_operator ||
        retiring_opcode.version != request->expected_retiring_version ||
        retiring_opcode.effect_model != DSL_EFFECT_MODEL_PURE ||
        (view_redirect &&
         (retiring_opcode.shape_rule != DSL_SHAPE_RULE_VIEW ||
          !DSL_Shape_Zero_Motion_View_Operator
              (retiring_opcode.logical_operator,
               retiring_opcode.version))) ||
        request->replacement_operand_ordinal >= retiring_node.operand_count)
        return FALSE;
    DSL_IR_VALUE_REFERENCE_RECORD replacement_reference;
    if (!DSL_IR_Image_Get_Value_Reference
            (retiring_node.first_operand_reference_id +
             request->replacement_operand_ordinal,
             &replacement_reference) ||
        replacement_reference.value_id != replacement.id)
        return FALSE;
    WN *retiring_expression = WN_kid0(request->retiring_definition);
    WN *replacement_operand = retiring_expression == NULL ||
        request->replacement_operand_ordinal >=
            (UINT32)WN_kid_count(retiring_expression) ? NULL :
        WN_kid(retiring_expression, request->replacement_operand_ordinal);
    if (replacement_operand == NULL ||
        WN_operator(replacement_operand) != OPR_LDID ||
        WN_st_idx(replacement_operand) != replacement.st ||
        WN_ty(replacement_operand) != replacement.ty)
        return FALSE;

    for (UINT32 i = 1; i <= DSL_IR_Image_Value_Reference_Count(); ++i) {
        DSL_IR_VALUE_REFERENCE_RECORD reference;
        if (!DSL_IR_Image_Get_Value_Reference(i, &reference) ||
            (reference.owner_node_id == retiring_node.id &&
             reference.value_id == retiring.id))
            return FALSE;
    }

    for (UINT32 i = 1; i <= DSL_Effect_Image_State_Effect_Count(); ++i) {
        DSL_STATE_EFFECT_RECORD effect;
        if (!DSL_Effect_Image_Get_State_Effect(i, &effect) ||
            effect.owner_node_id == retiring_node.id)
            return FALSE;
    }
    for (UINT32 i = 1; i <= DSL_Call_ABI_Image_Argument_Count(); ++i) {
        DSL_CALL_ARGUMENT_RECORD argument;
        if (!DSL_Call_ABI_Image_Get_Argument(i, &argument) ||
            argument.argument_value_id == retiring.id)
            return FALSE;
    }

    DSL_IR_RETIRE_USE_SCAN scan;
    scan.retiring_st = retiring.st;
    scan.retiring_definition = request->retiring_definition;
    scan.retiring_seen = FALSE;
    scan.valid = TRUE;
    scan.definition_count = 0;
    DSL_IR_Retire_Scan_Tree(containing_block, &scan);
    if (!scan.valid || !scan.retiring_seen || scan.definition_count != 1 ||
        DSL_IR_Retire_Has_Use_Outside_Block
            (pu_root, containing_block, retiring.st))
        return FALSE;

    if (view_redirect) {
        if (!DSL_Runtime_Interface_Image_Can_Retire_View
                 (owner_pu_st, retiring.id, replacement.id))
            return FALSE;
        for (UINT32 i = 0; i < scan.reads.size(); ++i) {
            if (WN_ty(scan.reads[i]) != retiring.ty)
                return FALSE;
        }
        for (UINT32 i = 1;
             i <= DSL_Runtime_Interface_Image_Value_Count(); ++i) {
            DSL_RUNTIME_VALUE_PROJECTION_RECORD projection;
            if (!DSL_Runtime_Interface_Image_Get_Value(i, &projection))
                return FALSE;
            if (projection.owner_pu_st == owner_pu_st &&
                projection.source_value_id == retiring.id &&
                DSL_IR_Retire_Has_Symbol_Use
                    (pu_root, projection.handle_st))
                return FALSE;
        }
    }

    UINT32 region_uses = DSL_Region_Symbol_Use_Count
                             (Current_PU_Info, retiring.st);
    if (region_uses != 0 &&
        !DSL_Region_Can_Redirect_Symbol
             (Current_PU_Info, retiring.st, replacement.st))
        return FALSE;

    for (UINT32 i = 0; i < scan.reads.size(); ++i) {
        WN_st_idx(scan.reads[i]) = replacement.st;
        if (view_redirect)
            WN_set_ty(scan.reads[i], replacement.ty);
    }
    if (region_uses != 0) {
        BOOL redirected = DSL_Region_Redirect_Symbol
                              (Current_PU_Info, retiring.st, replacement.st);
        FmtAssert(redirected, ("preflighted REGION redirect failed"));
    }
    BOOL image_redirected = DSL_IR_Image_Redirect_And_Retire_Value
        (replacement.id, retiring.id, request->replacement_operand_ordinal,
         view_redirect);
    FmtAssert(image_redirected, ("preflighted DSL value redirect failed"));
    if (view_redirect) {
        BOOL projection_retired = DSL_Runtime_Interface_Image_Retire_View
            (owner_pu_st, retiring.id, replacement.id);
        FmtAssert(projection_retired,
                  ("preflighted runtime view retirement failed"));
    }
    WN *removed = WN_EXTRACT_FromBlock
                      (containing_block,
                       request->retiring_definition);
    FmtAssert(removed == request->retiring_definition,
              ("preflighted DSL definition retirement failed"));
    WN_DELETE_Tree(removed);
    FmtAssert(DSL_IR_Image_Validate(NULL) &&
              (!view_redirect ||
               DSL_Runtime_Interface_Image_Validate(NULL)) &&
              DSL_Region_Verify_PU(Current_PU_Info, NULL),
              ("retired DSL value failed postcondition"));
    return TRUE;
}

/* Apply the active-PU same-type retirement transaction; preflight precedes
 * physical/image edits, with no allocation-exception rollback guarantee. */
BOOL
DSL_IR_Redirect_And_Retire_Native_Value
        (PU_Info *pu_info,
         const DSL_IR_NATIVE_VALUE_RETIRE_REQUEST *request)
{
    return DSL_IR_Redirect_And_Retire_Native_Value_Internal
               (pu_info, request, FALSE);
}

/* Apply the checked cross-TY zero-motion retirement variant. Canonical TYs
 * remain immutable; caller-selected PU scope is not enlarged. */
BOOL
DSL_IR_Redirect_And_Retire_Native_View
        (PU_Info *pu_info,
         const DSL_IR_NATIVE_VALUE_RETIRE_REQUEST *request)
{
    return DSL_IR_Redirect_And_Retire_Native_Value_Internal
               (pu_info, request, TRUE);
}
