/*
 * Copyright (C) 2026 Open64 Project
 *
 * Transactional shape-only refinement of native DSL tensor value types.
 * See doc/WHIRL-DSL-SHAPE-PROPAGATION-DESIGN.md and
 * doc/WHIRL-DSL-SHAPE-PROPAGATION-IMPLEMENTATION-PLAN.md.
 */

#include <stdlib.h>
#include <string.h>
#include <vector>

#include "dsl_memory_behavior.h"
#include "dsl_fhe.h"
#include "dsl_fhe_plan.h"
#include "dsl_gatekeeper.h"
#include "dsl_shape.h"
#include "dsl_ir_image.h"
#include "dsl_region.h"
#include "pu_info.h"
#include "symtab.h"
#include "wn.h"
#include "wn_util.h"

/* Commit-only image mutation after complete transaction preflight. */
extern BOOL DSL_IR_Image_Retype_Value (DSL_IR_VALUE_ID, TY_IDX, TY_IDX);

static BOOL
DSL_IR_Retype_Current_PU_Is (ST_IDX owner_pu_st)
{
    return Current_PU_Info != NULL &&
           PU_Info_proc_sym(Current_PU_Info) == owner_pu_st;
}

typedef struct {
    WN *definition;
    std::vector<WN *> reads;
    UINT32 definition_count;
    BOOL valid;
} DSL_IR_RETYPE_TREE_USE;

typedef struct {
    DSL_IR_VALUE_TYPE_REFINEMENT_REQUEST request;
    DSL_IR_VALUE_RECORD value;
    DSL_IR_NODE_RECORD node;
    DSL_IR_OPCODE_DESCRIPTOR_RECORD opcode;
    WN *definition;
    std::vector<WN *> reads;
} DSL_IR_RETYPE_JOURNAL;

static BOOL
DSL_IR_Retype_Report
        (FILE *diagnostic,
         const char *code,
         DSL_IR_VALUE_ID value_id,
         const char *message)
{
    if (diagnostic != NULL)
        fprintf(diagnostic, "%s: value=%u %s\n", code, value_id, message);
    return FALSE;
}

static void
DSL_IR_Retype_Scan_Tree
        (WN *wn,
         WN *parent,
         ST_IDX st,
         DSL_IR_RETYPE_TREE_USE *use)
{
    if (wn == NULL || use == NULL || !use->valid)
        return;
    if (WN_has_sym(wn) && WN_st_idx(wn) == st) {
        if (WN_operator(wn) == OPR_STID && WN_kid0(wn) != NULL &&
            DSL_WN_Is_Native(WN_kid0(wn))) {
            ++use->definition_count;
            if (use->definition == NULL)
                use->definition = wn;
            else
                use->valid = FALSE;
        } else if (WN_operator(wn) == OPR_LDID && parent != NULL &&
                   DSL_WN_Is_Native(parent)) {
            use->reads.push_back(wn);
        } else {
            use->valid = FALSE;
        }
    }
    if (WN_operator(wn) == OPR_BLOCK) {
        for (WN *statement = WN_first(wn); statement != NULL;
             statement = WN_next(statement))
            DSL_IR_Retype_Scan_Tree(statement, wn, st, use);
        return;
    }
    for (INT32 kid = 0; use->valid && kid < WN_kid_count(wn); ++kid)
        DSL_IR_Retype_Scan_Tree(WN_kid(wn, kid), wn, st, use);
}

static BOOL
DSL_IR_Retype_Type_Valid (TY_IDX old_ty, TY_IDX refined_ty)
{
    if (old_ty == refined_ty || !TY_tensor_is_canonical(old_ty) ||
        !TY_tensor_is_canonical(refined_ty) ||
        !DSL_Shape_Tensor_Core_Complete(refined_ty) ||
        TY_align(old_ty) != TY_align(refined_ty))
        return FALSE;

    TENSOR_DESCRIPTOR_RECORD old_descriptor;
    TENSOR_DESCRIPTOR_RECORD refined_descriptor;
    if (!TY_get_tensor_descriptor_record(old_ty, &old_descriptor) ||
        !TY_get_tensor_descriptor_record(refined_ty, &refined_descriptor) ||
        old_descriptor.element_ty != refined_descriptor.element_ty ||
        old_descriptor.kind != refined_descriptor.kind ||
        old_descriptor.dtype != refined_descriptor.dtype ||
        old_descriptor.traits != refined_descriptor.traits ||
        old_descriptor.layout != refined_descriptor.layout ||
        old_descriptor.sharding != refined_descriptor.sharding ||
        old_descriptor.placement != refined_descriptor.placement ||
        old_descriptor.memory != refined_descriptor.memory ||
        old_descriptor.quantization != refined_descriptor.quantization)
        return FALSE;

    DSL_SHAPE_FACT old_fact;
    DSL_SHAPE_FACT refined_fact;
    if (!DSL_Shape_Fact_From_Type(old_ty, &old_fact) ||
        !DSL_Shape_Fact_From_Type(refined_ty, &refined_fact) ||
        refined_fact.state != DSL_SHAPE_FACT_COMPLETE ||
        old_fact.rank != refined_fact.rank)
        return FALSE;
    for (INT32 i = 0; i < old_fact.rank; ++i) {
        if (old_fact.dimension_known[i] &&
            (!refined_fact.dimension_known[i] ||
             old_fact.dimension[i] != refined_fact.dimension[i]))
            return FALSE;
    }
    return TRUE;
}

static BOOL
DSL_IR_Retype_Has_Auxiliary_Relation (TY_IDX old_ty)
{
    if (DSL_FHE_Plan_Image_Has_Records() ||
        DSL_FHE_Approx_Profile_Image_Has_Records() ||
        DSL_FHE_Context_State_Image_Has_Records())
        return TRUE;
    for (UINT32 i = 1; i <= DSL_FHE_Tensor_Binding_Count(); ++i) {
        DSL_FHE_TENSOR_BINDING_RECORD binding;
        if (!DSL_FHE_Get_Tensor_Binding(i, &binding) ||
            binding.tensor_ty == old_ty)
            return TRUE;
    }
    return FALSE;
}

static BOOL
DSL_IR_Retype_Preflight
        (PU_Info *pu_info,
         WN *tree,
         const DSL_IR_VALUE_TYPE_REFINEMENT_REQUEST &request,
         FILE *diagnostic,
         DSL_IR_RETYPE_JOURNAL *journal)
{
    if (journal == NULL || pu_info == NULL || tree == NULL ||
        request.owner_pu_st != PU_Info_proc_sym(pu_info) ||
        Current_PU_Info != pu_info || PU_Info_tree_ptr(pu_info) != tree ||
        !DSL_IR_Retype_Current_PU_Is(request.owner_pu_st))
        return DSL_IR_Retype_Report
                   (diagnostic, "DSL-SHAPE-RETYPE-002", request.value_id,
                    "active PU or local symbol table does not match");
    if (request.value_id == DSL_IR_VALUE_INVALID_ID ||
        request.expected_old_ty == request.refined_ty)
        return DSL_IR_Retype_Report
                   (diagnostic, "DSL-SHAPE-RETYPE-001", request.value_id,
                    "request is malformed or has no type change");
    if (!DSL_IR_Retype_Type_Valid
             (request.expected_old_ty, request.refined_ty))
        return DSL_IR_Retype_Report
                   (diagnostic, "DSL-SHAPE-RETYPE-004", request.value_id,
                    "refined type is not a monotonic shape-only refinement");

    DSL_IR_VALUE_RECORD value;
    if (!DSL_IR_Image_Get_Value(request.value_id, &value) ||
        value.value_kind != DSL_IR_VALUE_OPERATOR_RESULT ||
        value.ty != request.expected_old_ty ||
        value.producer_node_id == DSL_IR_NODE_INVALID_ID ||
        (value.flags & DSL_IR_VALUE_FLAG_REDIRECTED) != 0 ||
        ST_IDX_level(value.st) != CURRENT_SYMTAB ||
        ST_IDX_index(value.st) == 0 ||
        ST_IDX_index(value.st) >= ST_Table_Size(CURRENT_SYMTAB))
        return DSL_IR_Retype_Report
                   (diagnostic, "DSL-SHAPE-RETYPE-003", request.value_id,
                    "value or expected old type does not match");
    ST &st = St_Table[value.st];
    if (ST_class(st) != CLASS_VAR || ST_sclass(st) != SCLASS_AUTO ||
        ST_export(st) != EXPORT_LOCAL || !ST_is_temp_var(st) ||
        ST_type(st) != request.expected_old_ty || ST_addr_saved(st) ||
        ST_addr_passed(st) || !DSL_Tensor_Has_Unique_Ownership(value.st))
        return DSL_IR_Retype_Report
                   (diagnostic, "DSL-SHAPE-RETYPE-005", request.value_id,
                    "result symbol is not an unescaped unique local temp");

    DSL_IR_NODE_RECORD node;
    DSL_IR_OPCODE_DESCRIPTOR_RECORD opcode;
    if (!DSL_IR_Image_Get_Node(value.producer_node_id, &node) ||
        node.result_value_id != value.id ||
        (node.flags & DSL_IR_NODE_FLAG_RETIRED) != 0 ||
        !DSL_IR_Image_Get_Opcode_Descriptor
             (node.opcode_descriptor_id, &opcode) ||
        opcode.effect_model != DSL_EFFECT_MODEL_PURE ||
        opcode.logical_operator == OPR_DSLMODELINPUT ||
        opcode.logical_operator == OPR_DSLTENSORCONST)
        return DSL_IR_Retype_Report
                   (diagnostic, "DSL-SHAPE-RETYPE-006", request.value_id,
                    "producer is not an eligible pure local expression");
    for (UINT32 i = 1; i <= DSL_Effect_Image_State_Effect_Count(); ++i) {
        DSL_STATE_EFFECT_RECORD effect;
        if (!DSL_Effect_Image_Get_State_Effect(i, &effect) ||
            effect.owner_node_id == node.id)
            return DSL_IR_Retype_Report
                       (diagnostic, "DSL-SHAPE-RETYPE-006", request.value_id,
                        "producer participates in a state effect");
    }

    DSL_IR_RETYPE_TREE_USE use;
    use.definition = NULL;
    use.definition_count = 0;
    use.valid = TRUE;
    DSL_IR_Retype_Scan_Tree(tree, NULL, value.st, &use);
    if (!use.valid || use.definition_count != 1 || use.definition == NULL ||
        WN_ty(use.definition) != request.expected_old_ty)
        return DSL_IR_Retype_Report
                   (diagnostic, "DSL-SHAPE-RETYPE-005", request.value_id,
                    "physical definition or use is unsupported");
    for (UINT32 i = 0; i < use.reads.size(); ++i) {
        if (WN_ty(use.reads[i]) != request.expected_old_ty)
            return DSL_IR_Retype_Report
                       (diagnostic, "DSL-SHAPE-RETYPE-003", request.value_id,
                        "LDID type disagrees with expected old type");
    }

    UINT32 reference_count = 0;
    for (UINT32 i = 1; i <= DSL_IR_Image_Value_Reference_Count(); ++i) {
        DSL_IR_VALUE_REFERENCE_RECORD reference;
        if (!DSL_IR_Image_Get_Value_Reference(i, &reference))
            return FALSE;
        if (reference.value_id == value.id)
            ++reference_count;
    }
    if (reference_count != use.reads.size())
        return DSL_IR_Retype_Report
                   (diagnostic, "DSL-SHAPE-RETYPE-005", request.value_id,
                    "physical and logical use counts disagree");
    for (UINT32 i = 1; i <= DSL_Call_ABI_Image_Argument_Count(); ++i) {
        DSL_CALL_ARGUMENT_RECORD argument;
        if (!DSL_Call_ABI_Image_Get_Argument(i, &argument) ||
            argument.argument_value_id == value.id)
            return DSL_IR_Retype_Report
                       (diagnostic, "DSL-SHAPE-RETYPE-006", request.value_id,
                        "value participates in a call ABI");
    }
    for (UINT32 i = 1; i <= DSL_PU_Interface_Image_Formal_Count(); ++i) {
        DSL_PU_FORMAL_RECORD formal;
        if (!DSL_PU_Interface_Image_Get_Formal(i, &formal) ||
            formal.formal_value_id == value.id || formal.formal_st == value.st)
            return DSL_IR_Retype_Report
                       (diagnostic, "DSL-SHAPE-RETYPE-006", request.value_id,
                        "value participates in a PU interface");
    }
    if (DSL_IR_Retype_Has_Auxiliary_Relation(request.expected_old_ty))
        return DSL_IR_Retype_Report
                   (diagnostic, "DSL-SHAPE-RETYPE-006", request.value_id,
                    "an auxiliary image has no SP5 retype participant");

    journal->request = request;
    journal->value = value;
    journal->node = node;
    journal->opcode = opcode;
    journal->definition = use.definition;
    journal->reads.swap(use.reads);
    return TRUE;
}

static void
DSL_IR_Retype_Apply
        (DSL_IR_RETYPE_JOURNAL *journal,
         TY_IDX from_ty,
         TY_IDX to_ty)
{
    for (UINT32 i = 0; i < journal->reads.size(); ++i)
        WN_set_ty(journal->reads[i], to_ty);
    WN_set_ty(journal->definition, to_ty);
    Set_ST_type(St_Table[journal->value.st], to_ty);
    BOOL changed = DSL_IR_Image_Retype_Value
                       (journal->value.id, from_ty, to_ty);
    FmtAssert(changed, ("preflighted DSL value retype failed"));
}

BOOL
DSL_IR_Refine_Native_Value_Types
        (PU_Info *pu_info,
         WN *tree,
         const DSL_IR_VALUE_TYPE_REFINEMENT_REQUEST *requests,
         UINT32 request_count,
         FILE *diagnostic,
         DSL_IR_VALUE_TYPE_REFINEMENT_RESULT *result)
{
    DSL_IR_VALUE_TYPE_REFINEMENT_RESULT local_result;
    memset(&local_result, 0, sizeof(local_result));
    if (requests == NULL || request_count == 0) {
        if (result != NULL)
            *result = local_result;
        return DSL_IR_Retype_Report
                   (diagnostic, "DSL-SHAPE-RETYPE-001", 0,
                    "empty request array");
    }
    if (!DSL_Region_Verify_PU(pu_info, diagnostic))
        return DSL_IR_Retype_Report
                   (diagnostic, "DSL-SHAPE-RETYPE-006", 0,
                    "active REGION image is invalid before retyping");

    /* Preflight every value so the type transaction cannot partially apply. */
    std::vector<DSL_IR_RETYPE_JOURNAL> journals(request_count);
    for (UINT32 i = 0; i < request_count; ++i) {
        for (UINT32 prior = 0; prior < i; ++prior) {
            if (requests[prior].value_id == requests[i].value_id)
                return DSL_IR_Retype_Report
                           (diagnostic, "DSL-SHAPE-RETYPE-001",
                            requests[i].value_id, "duplicate value request");
        }
        if (!DSL_IR_Retype_Preflight
                 (pu_info, tree, requests[i], diagnostic, &journals[i]))
            return FALSE;
        for (UINT32 prior = 0; prior < i; ++prior) {
            if (journals[prior].value.st == journals[i].value.st)
                return DSL_IR_Retype_Report
                           (diagnostic, "DSL-SHAPE-RETYPE-001",
                            requests[i].value_id, "duplicate result symbol");
        }
    }

    /* Apply WN, ST, and logical-image type changes as one transaction. */
    local_result.request_count = request_count;
    for (UINT32 i = 0; i < request_count; ++i) {
        DSL_IR_Retype_Apply
            (&journals[i], journals[i].request.expected_old_ty,
             journals[i].request.refined_ty);
        ++local_result.updated_st_count;
        local_result.updated_wn_count += 1 + journals[i].reads.size();
        ++local_result.updated_value_count;
    }

    DSL_GATEKEEPER_RESULT gatekeeper_result;
    const char *force_post_failure =
        getenv("OPEN64_DSL_SHAPE_RETYPE_TEST_POSTFAIL");
    BOOL valid = (force_post_failure == NULL ||
                  strcmp(force_post_failure, "1") != 0) &&
                 DSL_IR_Image_Validate(diagnostic) &&
                 DSL_Region_Verify_PU(pu_info, diagnostic) &&
                 DSL_Gatekeeper_Verify_PU_Mode
                     (pu_info, DSL_GATEKEEPER_STRICT, diagnostic,
                      &gatekeeper_result);
    if (!valid) {
        /* Reverse order restores the exact pre-transaction type graph. */
        for (UINT32 i = request_count; i != 0; --i) {
            DSL_IR_RETYPE_JOURNAL &journal = journals[i - 1];
            DSL_IR_Retype_Apply
                (&journal, journal.request.refined_ty,
                 journal.request.expected_old_ty);
            ++local_result.rollback_count;
        }
        FmtAssert(DSL_IR_Image_Validate(diagnostic) &&
                  DSL_Region_Verify_PU(pu_info, diagnostic),
                  ("DSL shape retype rollback failed"));
        if (result != NULL)
            *result = local_result;
        return DSL_IR_Retype_Report
                   (diagnostic, "DSL-SHAPE-RETYPE-007", 0,
                    "strict post-verification failed; transaction rolled back");
    }
    if (result != NULL)
        *result = local_result;
    return TRUE;
}

