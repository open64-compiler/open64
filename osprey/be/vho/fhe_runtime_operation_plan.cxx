/*
 * Copyright (C) 2026 Open64 Project
 *
 * Purpose: plan the complete active-PU FHE operation replacement before the
 * generic DSL standard-WHIRL transaction mutates any native definition.
 * FHE owns the ABI operation choice and exact static selector ordinal;
 * common/com owns the atomic tree/image commit and lowered relation checks.
 *
 * Design references:
 *   doc/FHE-SYNC5-NATIVE-OWNERSHIP-AUDIT.md, S5-F
 *   doc/FHE-SYNC5-STANDARD-WHIRL-LOWERING-CONTRACT.md
 *   doc/FHE-RUNTIME-C-ABI-V1-CONTRACT.md
 */

#include <string.h>

#include <vector>

#include "dsl_opcode.h"
#include "fhe_runtime_operation_plan.h"
#include "fhe_semantic_runtime_lower.h"
#include "ir_reader.h"
#include "pu_info.h"
#include "wn.h"

static std::vector<DSL_IR_NATIVE_VALUE_LOWER_REQUEST>
    VHO_FHE_operation_requests;
static PU_Info *VHO_FHE_operation_owner;
static BOOL VHO_FHE_operation_prepared;

typedef struct {
    VHO_FHE_STANDARD_FAILURE_BUILDER builder;
    void *user_context;
    SRCPOS source_position;
} VHO_FHE_OPERATION_FAILURE_CONTEXT;

/* Report a stable FHE planning failure without changing WHIRL or DSL rows. */
static BOOL
VHO_FHE_Operation_Report (FILE *diagnostic, const char *message)
{
    if (diagnostic != NULL)
        fprintf(diagnostic, "CFHELOWER-OP-001: %s\n", message);
    return FALSE;
}

/* Give synthetic failure statements the same source anchor as their call. */
static void
VHO_FHE_Operation_Set_Source_Position (WN *tree, SRCPOS position)
{
    if (tree == NULL)
        return;
    if (WN_operator(tree) == OPR_BLOCK) {
        for (WN *statement = WN_first(tree); statement != NULL;
             statement = WN_next(statement))
            VHO_FHE_Operation_Set_Source_Position(statement, position);
        return;
    }
    if (OPCODE_is_stmt(WN_opcode(tree)))
        WN_Set_Linenum(tree, position);
    for (INT32 kid = 0; kid < WN_kid_count(tree); ++kid)
        VHO_FHE_Operation_Set_Source_Position
            (WN_kid(tree, kid), position);
}

/* Adapt an owning pass's failure body to the exact source definition. */
static WN *
VHO_FHE_Operation_Build_Failure
        (ST_IDX status_st, ST_IDX output_st,
         void *context, FILE *diagnostic)
{
    VHO_FHE_OPERATION_FAILURE_CONTEXT *failure =
        (VHO_FHE_OPERATION_FAILURE_CONTEXT *)context;
    if (failure == NULL || failure->builder == NULL ||
        failure->source_position == 0)
        return NULL;
    WN *block = failure->builder
                    (status_st, output_st, failure->user_context,
                     diagnostic);
    VHO_FHE_Operation_Set_Source_Position
        (block, failure->source_position);
    return block;
}

/* Dispose of detached blocks on failure or after their statements move. */
void
VHO_FHE_Runtime_Operation_Plan_Reset (void)
{
    for (UINT32 i = 0; i < VHO_FHE_operation_requests.size(); ++i) {
        WN *block = VHO_FHE_operation_requests[i].standard_block;
        if (block != NULL)
            WN_DELETE_Tree(block);
    }
    VHO_FHE_operation_requests.clear();
    VHO_FHE_operation_owner = NULL;
    VHO_FHE_operation_prepared = FALSE;
}

/* Recover direct logical operands in the native node's declared order. */
static BOOL
VHO_FHE_Operation_Operands
        (const DSL_IR_NODE_RECORD &node,
         std::vector<DSL_IR_VALUE_ID> *operands)
{
    if (operands == NULL)
        return FALSE;
    operands->clear();
    for (UINT32 ordinal = 0; ordinal < node.operand_count; ++ordinal) {
        DSL_IR_VALUE_REFERENCE_RECORD reference;
        if (!DSL_IR_Image_Get_Value_Reference
                 (node.first_operand_reference_id + ordinal, &reference) ||
            reference.owner_node_id != node.id ||
            reference.ordinal != ordinal || reference.value_id == 0)
            return FALSE;
        operands->push_back(reference.value_id);
    }
    return TRUE;
}

/* Find the unique root-promoted input/binding pair for a source constant. */
static BOOL
VHO_FHE_Operation_Promoted_Source
        (ST_IDX owner_pu_st, const DSL_IR_VALUE_RECORD &value,
         DSL_IR_LOWER_RELATION *relation)
{
    if (relation == NULL)
        return FALSE;
    memset(relation, 0, sizeof(*relation));
    for (UINT32 id = 1;
         id <= DSL_Program_Interface_Image_Runtime_Input_Count(); ++id) {
        DSL_RUNTIME_INPUT_RECORD input;
        if (!DSL_Program_Interface_Image_Get_Runtime_Input(id, &input))
            return FALSE;
        if (input.source_owner_pu_st != owner_pu_st ||
            input.source_value_id != value.id)
            continue;
        if (relation->runtime_input_id != 0 ||
            input.input_kind != DSL_RUNTIME_INPUT_SOURCE_EXTERNAL_TENSOR ||
            input.source_st != value.st || input.source_ty != value.ty)
            return FALSE;
        relation->runtime_input_id = id;
    }
    if (relation->runtime_input_id == 0)
        return FALSE;
    for (UINT32 id = 1;
         id <= DSL_Program_Interface_Image_Runtime_Binding_Count(); ++id) {
        DSL_RUNTIME_INPUT_BINDING_RECORD binding;
        if (!DSL_Program_Interface_Image_Get_Runtime_Binding(id, &binding))
            return FALSE;
        if (binding.runtime_input_id != relation->runtime_input_id ||
            binding.owner_pu_st != owner_pu_st)
            continue;
        if (relation->runtime_binding_id != 0 ||
            binding.binding_kind !=
                DSL_RUNTIME_INPUT_BINDING_ROOT_PROMOTED_SOURCE)
            return FALSE;
        relation->runtime_binding_id = id;
    }
    if (relation->runtime_binding_id == 0)
        return FALSE;
    relation->relation_kind = DSL_IR_LOWER_RELATION_ROOT_PROMOTED_INPUT;
    return TRUE;
}

/* Count executable symbol reads and escapes, not the defining STID itself. */
static UINT32
VHO_FHE_Operation_Physical_Use_Count (const WN *tree, ST_IDX source_st)
{
    if (tree == NULL)
        return 0;
    if (WN_operator(tree) == OPR_BLOCK) {
        UINT32 count = 0;
        for (const WN *statement = WN_first(tree); statement != NULL;
             statement = WN_next(statement))
            count += VHO_FHE_Operation_Physical_Use_Count
                         (statement, source_st);
        return count;
    }
    UINT32 count = ((WN_operator(tree) == OPR_LDID ||
                     WN_operator(tree) == OPR_LDA) &&
                    WN_st_idx(tree) == source_st) ? 1 : 0;
    for (INT32 kid = 0; kid < WN_kid_count(tree); ++kid)
        count += VHO_FHE_Operation_Physical_Use_Count
                     (WN_kid(tree, kid), source_st);
    return count;
}

/* Retired node references are provenance; live references bar dead elision. */
static BOOL
VHO_FHE_Operation_Has_Live_Logical_Use (DSL_IR_VALUE_ID source_value_id)
{
    for (UINT32 id = 1; id <= DSL_IR_Image_Value_Reference_Count(); ++id) {
        DSL_IR_VALUE_REFERENCE_RECORD reference;
        DSL_IR_NODE_RECORD consumer;
        if (!DSL_IR_Image_Get_Value_Reference(id, &reference) ||
            !DSL_IR_Image_Get_Node(reference.owner_node_id, &consumer))
            return TRUE;
        if (reference.value_id == source_value_id &&
            (consumer.flags & DSL_IR_NODE_FLAG_RETIRED) == 0)
            return TRUE;
    }
    return FALSE;
}

/* Translate one scheduled logical family to its exact ABI operation kind. */
static BOOL
VHO_FHE_Operation_Kind (DSL_OPERATOR logical_operator, UINT32 *kind)
{
    if (kind == NULL)
        return FALSE;
    switch (logical_operator) {
    case OPR_DSLCONV2D:
        *kind = VHO_FHE_RUNTIME_OP_CONV2D_PLAIN;
        break;
    case OPR_DSLRESIDUALADD:
        *kind = VHO_FHE_RUNTIME_OP_RESIDUAL_ADD;
        break;
    case OPR_DSLGLOBALAVGPOOL2D:
        *kind = VHO_FHE_RUNTIME_OP_AVERAGE_POOL;
        break;
    case OPR_DSLFLATTEN:
        *kind = VHO_FHE_RUNTIME_OP_LAYOUT_CONVERT;
        break;
    case OPR_DSLLINEAR:
        *kind = VHO_FHE_RUNTIME_OP_LINEAR_PLAIN;
        break;
    default:
        return FALSE;
    }
    return TRUE;
}

/* Build one computed request while preserving the exact source definition. */
static BOOL
VHO_FHE_Operation_Append_Computed
        (PU_Info *pu_info, WN *definition,
         const DSL_IR_VALUE_RECORD &value,
         const DSL_IR_NODE_RECORD &node,
         const DSL_IR_OPCODE_DESCRIPTOR_RECORD &opcode,
         VHO_FHE_STANDARD_FAILURE_BUILDER build_failure,
         void *failure_context, FILE *diagnostic,
         VHO_FHE_RUNTIME_OPERATION_PLAN_RESULT *summary)
{
    ST_IDX owner_pu_st = PU_Info_proc_sym(pu_info);
    DSL_RUNTIME_VALUE_PROJECTION_RECORD projection;
    if (!DSL_Runtime_Interface_Image_Find_Value
             (owner_pu_st, value.id, &projection) ||
        projection.binding_kind != DSL_RUNTIME_BINDING_LOCAL_VALUE)
        return VHO_FHE_Operation_Report
                   (diagnostic, "computed result lacks one local projection");

    std::vector<DSL_IR_VALUE_ID> operands;
    if (!VHO_FHE_Operation_Operands(node, &operands))
        return VHO_FHE_Operation_Report
                   (diagnostic, "logical operands are not canonical");
    VHO_FHE_RUNTIME_CALL_SEQUENCE sequence;
    VHO_FHE_Runtime_Call_Sequence_Init(&sequence);
    SRCPOS position = WN_Get_Linenum(definition);
    if (position == 0)
        return VHO_FHE_Operation_Report
                   (diagnostic, "native definition has no source position");
    VHO_FHE_OPERATION_FAILURE_CONTEXT failure;
    failure.builder = build_failure;
    failure.user_context = failure_context;
    failure.source_position = position;

    VHO_FHE_RUNTIME_STATIC_SCHEDULE_RECORD scheduled;
    BOOL has_schedule = VHO_FHE_Runtime_Static_Schedule_Find
                            (node.id, &scheduled);
    BOOL valid = FALSE;
    if (opcode.logical_operator == OPR_DSLOUTPUTLOGITS &&
        opcode.version == 2 && operands.size() == 1 && !has_schedule) {
        valid = VHO_FHE_Runtime_Build_Identity_Sequence
                    (pu_info, operands[0], diagnostic, &sequence);
    }
    else if (has_schedule && scheduled.owner_pu_st == owner_pu_st &&
             scheduled.source_node_id == node.id &&
             scheduled.result_value_id == value.id &&
             scheduled.logical_operator == opcode.logical_operator &&
             scheduled.first_static_ordinal != 0) {
        if (opcode.logical_operator == OPR_DSLRELU &&
            opcode.version == 2 && operands.size() == 1 &&
            scheduled.static_evaluation_count == 6) {
            valid = VHO_FHE_Runtime_Build_Relu_Sequence
                        (pu_info, operands[0],
                         scheduled.first_static_ordinal, position,
                         VHO_FHE_Operation_Build_Failure, &failure,
                         diagnostic,
                         &sequence);
        }
        else if (scheduled.static_evaluation_count == 1) {
            VHO_FHE_RUNTIME_OPERATION_REQUEST operation;
            memset(&operation, 0, sizeof(operation));
            operation.static_ordinal = scheduled.first_static_ordinal;
            operation.operand_value_ids = operands.empty() ? NULL :
                &operands[0];
            operation.operand_count = operands.size();
            if (opcode.version == 2 &&
                VHO_FHE_Operation_Kind
                    ((DSL_OPERATOR)opcode.logical_operator,
                     &operation.operation_kind))
                valid = VHO_FHE_Runtime_Build_Operation_Sequence
                            (pu_info, &operation, position,
                             VHO_FHE_Operation_Build_Failure, &failure,
                             diagnostic, &sequence);
        }
    }
    if (!valid || sequence.block == NULL) {
        if (sequence.block != NULL)
            WN_DELETE_Tree(sequence.block);
        return VHO_FHE_Operation_Report
                   (diagnostic, "operation sequence is unsupported");
    }

    VHO_FHE_RUNTIME_HANDLE_BINDING output;
    WN *result_definition = NULL;
    if (!VHO_FHE_Runtime_Resolve_Value_Handle
             (pu_info, value.id, diagnostic, &output) ||
        output.handle_st != projection.handle_st ||
        output.handle_ty != projection.handle_ty ||
        !VHO_FHE_Runtime_Finalize_Projected_Output
             (&output, position, &sequence, &result_definition)) {
        WN_DELETE_Tree(sequence.block);
        return VHO_FHE_Operation_Report
                   (diagnostic, "projected result store is invalid");
    }

    DSL_IR_NATIVE_VALUE_LOWER_REQUEST request;
    memset(&request, 0, sizeof(request));
    request.native_definition = definition;
    request.source_value_id = value.id;
    request.expected_operator = (DSL_OPERATOR)opcode.logical_operator;
    request.expected_version = opcode.version;
    request.mode = DSL_IR_NATIVE_LOWER_COMPUTED_STANDARD_BLOCK;
    request.relation.relation_kind =
        DSL_IR_LOWER_RELATION_RUNTIME_VALUE_PROJECTION;
    request.relation.value_projection_id = projection.id;
    request.standard_block = sequence.block;
    VHO_FHE_operation_requests.push_back(request);
    ++summary->computed_count;
    summary->standard_call_count += sequence.standard_call_count;
    summary->output_handle_count += sequence.output_handle_count;
    summary->status_check_count += sequence.status_check_count;
    if (has_schedule) {
        summary->selector_count += scheduled.static_evaluation_count;
        summary->evaluation_count += scheduled.static_evaluation_count;
    }
    return TRUE;
}

/* Plan every native definition, including verified-dead source constants. */
static BOOL
VHO_FHE_Operation_Append_Definition
        (PU_Info *pu_info, WN *definition,
         VHO_FHE_STANDARD_FAILURE_BUILDER build_failure,
         void *failure_context, FILE *diagnostic,
         VHO_FHE_RUNTIME_OPERATION_PLAN_RESULT *summary)
{
    DSL_IR_VALUE_RECORD value;
    DSL_IR_NODE_RECORD node;
    DSL_IR_OPCODE_DESCRIPTOR_RECORD opcode;
    BOOL found = DSL_IR_Image_Find_Definition_Value
                     (pu_info, definition, &value);
    if (!found) {
        ST_IDX owner = PU_Info_proc_sym(pu_info);
        ST_IDX st = WN_st_idx(definition);
        DSL_LOGICAL_OPCODE logical;
        if (!DSL_WN_Get_Logical_Opcode
                 (WN_kid0(definition), &logical, NULL) ||
            logical.dsl_operator != OPR_DSLTENSORCONST ||
            ST_IDX_level(st) != CURRENT_SYMTAB ||
            ST_IDX_index(st) == 0 ||
            ST_IDX_index(st) >= ST_Table_Size(CURRENT_SYMTAB) ||
            !DSL_IR_Image_Find_PU_Value
                 (st, ST_name(St_Table[st]),
                  ST_name(St_Table[owner]), &value) ||
            value.value_kind != DSL_IR_VALUE_CONSTANT ||
            value.st != st || value.ty != WN_ty(definition))
            return VHO_FHE_Operation_Report
                       (diagnostic, "source constant identity is invalid");
    }
    if (!DSL_IR_Image_Get_Node(value.producer_node_id, &node) ||
        !DSL_IR_Image_Get_Opcode_Descriptor
             (node.opcode_descriptor_id, &opcode) ||
        node.result_value_id != value.id || node.flags != 0 ||
        value.flags != 0)
        return VHO_FHE_Operation_Report
                   (diagnostic, "native definition identity is invalid");

    if (opcode.logical_operator == OPR_DSLTENSORCONST) {
        DSL_IR_LOWER_RELATION relation;
        if (!VHO_FHE_Operation_Promoted_Source
                 (PU_Info_proc_sym(pu_info), value, &relation)) {
            ++summary->unpromoted_source_count;
            if (VHO_FHE_Operation_Physical_Use_Count
                    (PU_Info_tree_ptr(pu_info), value.st) != 0 ||
                VHO_FHE_Operation_Has_Live_Logical_Use(value.id)) {
                ++summary->live_unpromoted_source_count;
                return VHO_FHE_Operation_Report
                           (diagnostic, "unpromoted source has a live use");
            }
            relation.relation_kind =
                DSL_IR_LOWER_RELATION_VERIFIED_DEAD_SOURCE;
        }
        DSL_IR_NATIVE_VALUE_LOWER_REQUEST request;
        memset(&request, 0, sizeof(request));
        request.native_definition = definition;
        request.source_value_id = value.id;
        request.expected_operator = OPR_DSLTENSORCONST;
        request.expected_version = opcode.version;
        request.mode = relation.relation_kind ==
            DSL_IR_LOWER_RELATION_VERIFIED_DEAD_SOURCE ?
            DSL_IR_NATIVE_LOWER_VERIFIED_DEAD_SOURCE_ELISION :
            DSL_IR_NATIVE_LOWER_PROMOTED_SOURCE_ELISION;
        request.relation = relation;
        VHO_FHE_operation_requests.push_back(request);
        if (request.mode == DSL_IR_NATIVE_LOWER_PROMOTED_SOURCE_ELISION)
            ++summary->promoted_source_count;
        return TRUE;
    }
    return VHO_FHE_Operation_Append_Computed
               (pu_info, definition, value, node, opcode,
                build_failure, failure_context, diagnostic, summary);
}

/* Visit statements in physical order, including nested REGION bodies. */
static BOOL
VHO_FHE_Operation_Collect_Tree
        (PU_Info *pu_info, WN *tree,
         VHO_FHE_STANDARD_FAILURE_BUILDER build_failure,
         void *failure_context, FILE *diagnostic,
         VHO_FHE_RUNTIME_OPERATION_PLAN_RESULT *summary)
{
    if (tree == NULL)
        return TRUE;
    if (WN_operator(tree) == OPR_BLOCK) {
        for (WN *statement = WN_first(tree); statement != NULL;
             statement = WN_next(statement)) {
            if (!VHO_FHE_Operation_Collect_Tree
                     (pu_info, statement, build_failure, failure_context,
                      diagnostic, summary))
                return FALSE;
        }
        return TRUE;
    }
    if (WN_operator(tree) == OPR_STID && WN_kid_count(tree) == 1 &&
        WN_kid0(tree) != NULL && DSL_WN_Is_Native(WN_kid0(tree)) &&
        !VHO_FHE_Operation_Append_Definition
             (pu_info, tree, build_failure, failure_context,
              diagnostic, summary))
        return FALSE;
    for (INT32 kid = 0; kid < WN_kid_count(tree); ++kid) {
        if (!VHO_FHE_Operation_Collect_Tree
                 (pu_info, WN_kid(tree, kid), build_failure,
                  failure_context, diagnostic, summary))
            return FALSE;
    }
    return TRUE;
}

/* Construct the complete per-PU request array without replacing a node. */
BOOL
VHO_FHE_Runtime_Operation_Plan_Prepare_PU
        (PU_Info *pu_info,
         VHO_FHE_STANDARD_FAILURE_BUILDER build_failure,
         void *failure_context, FILE *diagnostic,
         VHO_FHE_RUNTIME_OPERATION_PLAN_RESULT *result)
{
    VHO_FHE_Runtime_Operation_Plan_Reset();
    if (result != NULL)
        memset(result, 0, sizeof(*result));
    if (pu_info == NULL || pu_info != Current_PU_Info ||
        PU_Info_tree_ptr(pu_info) == NULL || build_failure == NULL ||
        result == NULL)
        return VHO_FHE_Operation_Report
                   (diagnostic, "active PU or failure builder is missing");
    if (!VHO_FHE_Operation_Collect_Tree
             (pu_info, PU_Info_tree_ptr(pu_info), build_failure,
              failure_context, diagnostic, result) ||
        VHO_FHE_operation_requests.empty()) {
        VHO_FHE_Runtime_Operation_Plan_Reset();
        return VHO_FHE_Operation_Report
                   (diagnostic, "complete operation plan was not built");
    }
    VHO_FHE_operation_owner = pu_info;
    VHO_FHE_operation_prepared = TRUE;
    return TRUE;
}

/* Expose the complete borrowed request array for focused negative tests. */
BOOL
VHO_FHE_Runtime_Operation_Plan_Get
        (const DSL_IR_NATIVE_VALUE_LOWER_REQUEST **requests,
         UINT32 *request_count)
{
    if (!VHO_FHE_operation_prepared || requests == NULL ||
        request_count == NULL)
        return FALSE;
    *requests = &VHO_FHE_operation_requests[0];
    *request_count = VHO_FHE_operation_requests.size();
    return TRUE;
}

/* Submit the whole PU once; generic preflight owns all persistent mutation. */
BOOL
VHO_FHE_Runtime_Operation_Plan_Apply_PU
        (PU_Info *pu_info, FILE *diagnostic,
         DSL_IR_NATIVE_VALUE_LOWER_RESULT *results)
{
    if (!VHO_FHE_operation_prepared || pu_info != VHO_FHE_operation_owner ||
        pu_info != Current_PU_Info || results == NULL)
        return VHO_FHE_Operation_Report
                   (diagnostic, "prepared PU and active PU differ");
    if (!DSL_IR_Lower_Native_Values_To_Standard_Blocks
             (pu_info, &VHO_FHE_operation_requests[0],
              VHO_FHE_operation_requests.size(), diagnostic, results))
        return FALSE;
    for (UINT32 i = 0; i < VHO_FHE_operation_requests.size(); ++i)
        VHO_FHE_operation_requests[i].standard_block = NULL;
    VHO_FHE_operation_prepared = FALSE;
    return TRUE;
}
