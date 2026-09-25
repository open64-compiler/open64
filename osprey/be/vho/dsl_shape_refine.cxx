/*
 * Copyright (C) 2026 Open64 Project
 */

#include <string.h>
#include <vector>

#include "dsl_shape_refine.h"
#include "config_dsl.h"
#include "dsl_gatekeeper.h"
#include "dsl_ir_image.h"
#include "errors.h"
#include "ir_reader.h"
#include "pu_info.h"
#include "symtab.h"
#include "tracing.h"
#include "wn.h"

typedef struct {
    PU_Info *pu_info;
    FILE *diagnostic;
    VHO_DSL_SHAPE_REFINE_RESULT *result;
    std::vector<DSL_IR_VALUE_TYPE_REFINEMENT_REQUEST> requests;
} VHO_DSL_SHAPE_REFINE_CONTEXT;

typedef struct {
    PU_Info *pu_info;
    ST_IDX owner_pu_st;
    WN *tree;
    UINT64 generation;
    UINT64 validated_generation;
    VHO_DSL_SHAPE_TRIGGER invalidated_by;
} VHO_DSL_SHAPE_GENERATION_STATE;

static std::vector<VHO_DSL_SHAPE_GENERATION_STATE>
    VHO_DSL_shape_generation_state;

static const char *VHO_DSL_shape_trigger_name[VHO_DSL_SHAPE_TRIGGER_COUNT] = {
    "none",
    "pu_admission",
    "pu_identity_change",
    "seed_refinement",
    "operator_constraint_change",
    "value_relationship_change",
    "structural_transformation",
    "pu_region_restructuring",
    "symbolic_resolution",
    "dsl_wopt",
    "fhe_conversion",
    "vho_dsl_optimization"
};

const char *
VHO_DSL_Shape_Trigger_Name (VHO_DSL_SHAPE_TRIGGER trigger)
{
    if (trigger < VHO_DSL_SHAPE_TRIGGER_NONE ||
        trigger >= VHO_DSL_SHAPE_TRIGGER_COUNT)
        return "unknown";
    return VHO_DSL_shape_trigger_name[trigger];
}

static VHO_DSL_SHAPE_GENERATION_STATE *
VHO_DSL_Shape_Generation_State
        (PU_Info *pu_info,
         WN *tree,
         BOOL create)
{
    if (pu_info == NULL || tree == NULL)
        return NULL;
    ST_IDX owner_pu_st = PU_Info_proc_sym(pu_info);
    if (ST_IDX_index(owner_pu_st) == 0)
        return NULL;
    for (UINT32 i = 0; i < VHO_DSL_shape_generation_state.size(); ++i) {
        VHO_DSL_SHAPE_GENERATION_STATE *state =
            &VHO_DSL_shape_generation_state[i];
        if (state->pu_info == pu_info) {
            if (state->owner_pu_st != owner_pu_st) {
                state->owner_pu_st = owner_pu_st;
                state->tree = tree;
                state->generation = 1;
                state->validated_generation = 0;
                state->invalidated_by =
                    VHO_DSL_SHAPE_TRIGGER_PU_IDENTITY_CHANGE;
            }
            return state;
        }
    }
    if (!create)
        return NULL;
    VHO_DSL_SHAPE_GENERATION_STATE state;
    state.pu_info = pu_info;
    state.owner_pu_st = owner_pu_st;
    state.tree = tree;
    state.generation = 1;
    state.validated_generation = 0;
    state.invalidated_by = VHO_DSL_SHAPE_TRIGGER_PU_ADMISSION;
    VHO_DSL_shape_generation_state.push_back(state);
    return &VHO_DSL_shape_generation_state.back();
}

static BOOL
VHO_DSL_Shape_Refinement_Mark_Current
        (PU_Info *pu_info,
         WN *tree)
{
    VHO_DSL_SHAPE_GENERATION_STATE *state =
        VHO_DSL_Shape_Generation_State(pu_info, tree, TRUE);
    if (state == NULL)
        return FALSE;
    state->tree = tree;
    state->validated_generation = state->generation;
    state->invalidated_by = VHO_DSL_SHAPE_TRIGGER_NONE;
    return TRUE;
}

static BOOL
VHO_DSL_Shape_Validate_Active_Boundary
        (PU_Info *pu_info,
         WN *tree,
         FILE *diagnostic,
         UINT32 *counter)
{
    if (counter != NULL)
        ++*counter;
    DSL_IR_ACTIVE_PU_BOUNDARY_CONTEXT boundary;
    boundary.pu_info = pu_info;
    boundary.tree = tree;
    boundary.owner_pu_st = pu_info == NULL ? ST_IDX_ZERO :
                           PU_Info_proc_sym(pu_info);
    return DSL_IR_Image_Validate_Active_PU_Boundaries
               (&boundary, diagnostic);
}

static BOOL
VHO_DSL_Shape_Complete_Success
        (PU_Info *pu_info,
         WN *tree,
         FILE *diagnostic,
         VHO_DSL_SHAPE_REFINE_RESULT *local_result,
         VHO_DSL_SHAPE_REFINE_RESULT *result)
{
    if (!VHO_DSL_Shape_Validate_Active_Boundary
             (pu_info, tree, diagnostic,
              &local_result->boundary_success_exit_count) ||
        !VHO_DSL_Shape_Refinement_Mark_Current(pu_info, tree)) {
        ++local_result->diagnostic_count;
        if (result != NULL)
            *result = *local_result;
        return FALSE;
    }
    if (result != NULL)
        *result = *local_result;
    return TRUE;
}

BOOL
VHO_DSL_Shape_Refinement_Invalidate
        (PU_Info *pu_info,
         WN *tree,
         VHO_DSL_SHAPE_TRIGGER trigger,
         FILE *diagnostic)
{
    if (trigger <= VHO_DSL_SHAPE_TRIGGER_NONE ||
        trigger >= VHO_DSL_SHAPE_TRIGGER_COUNT) {
        if (diagnostic != NULL)
            fprintf(diagnostic,
                    "DSL-SHAPE-INVALIDATE-ERROR: trigger=%d name=%s\n",
                    (INT)trigger, VHO_DSL_Shape_Trigger_Name(trigger));
        return FALSE;
    }
    VHO_DSL_SHAPE_GENERATION_STATE *state =
        VHO_DSL_Shape_Generation_State(pu_info, tree, TRUE);
    if (state == NULL)
        return FALSE;
    state->tree = tree;
    ++state->generation;
    if (state->generation == 0)
        state->generation = 1;
    state->invalidated_by = trigger;
    if (diagnostic != NULL)
        fprintf(diagnostic,
                "DSL-SHAPE-INVALIDATE: pu=%s generation=%llu reason=%s\n",
                ST_name(St_Table[state->owner_pu_st]),
                (unsigned long long)state->generation,
                VHO_DSL_Shape_Trigger_Name(state->invalidated_by));
    return TRUE;
}

BOOL
VHO_DSL_Shape_Refinement_Is_Current
        (PU_Info *pu_info,
         WN *tree,
         FILE *diagnostic)
{
    VHO_DSL_SHAPE_GENERATION_STATE *state =
        VHO_DSL_Shape_Generation_State(pu_info, tree, FALSE);
    BOOL current = state != NULL && state->tree == tree &&
                   state->validated_generation == state->generation;
    if (!current && diagnostic != NULL) {
        const char *pu_name = "<unknown>";
        if (pu_info != NULL && ST_IDX_index(PU_Info_proc_sym(pu_info)) != 0)
            pu_name = ST_name(St_Table[PU_Info_proc_sym(pu_info)]);
        fprintf(diagnostic,
                "DSL-SHAPE-STALE: pu=%s generation=%llu validated=%llu "
                "reason=%s\n",
                pu_name,
                (unsigned long long)(state != NULL ? state->generation : 0),
                (unsigned long long)(state != NULL ?
                    state->validated_generation : 0),
                state != NULL ?
                    VHO_DSL_Shape_Trigger_Name(state->invalidated_by) :
                    "no_refinement_state");
    }
    return current;
}

static BOOL
VHO_DSL_Shape_Has_Native_Node (WN *wn)
{
    if (wn == NULL)
        return FALSE;
    if (DSL_WN_Is_Native(wn))
        return TRUE;
    if (WN_operator(wn) == OPR_BLOCK) {
        for (WN *statement = WN_first(wn); statement != NULL;
             statement = WN_next(statement)) {
            if (VHO_DSL_Shape_Has_Native_Node(statement))
                return TRUE;
        }
        return FALSE;
    }
    for (INT32 kid = 0; kid < WN_kid_count(wn); ++kid) {
        if (VHO_DSL_Shape_Has_Native_Node(WN_kid(wn, kid)))
            return TRUE;
    }
    return FALSE;
}

static BOOL
VHO_DSL_Shape_Collect_Refinement
        (const DSL_SHAPE_REFINEMENT *refinement,
         void *opaque_context)
{
    if (refinement == NULL || opaque_context == NULL ||
        refinement->refined_fact.state != DSL_SHAPE_FACT_COMPLETE ||
        refinement->refined_fact.rank < 0 ||
        refinement->refined_fact.rank > DSL_SHAPE_MAX_RANK)
        return FALSE;
    VHO_DSL_SHAPE_REFINE_CONTEXT *context =
        (VHO_DSL_SHAPE_REFINE_CONTEXT *)opaque_context;
    char shape[512];
    if (!DSL_Shape_Format_Fact
             (&refinement->refined_fact, shape, sizeof(shape)))
        return FALSE;

    TY_TENSOR_TYPE_CORE_REFINEMENT type_refinement;
    type_refinement.rank = refinement->refined_fact.rank;
    type_refinement.logical_shape = shape;
    BOOL created = FALSE;
    TY_IDX refined_ty = TY_Intern_Refined_Tensor_Type
                            (refinement->expected_old_ty,
                             &type_refinement, &created);
    if (TY_IDX_index(refined_ty) == 0)
        return FALSE;

    DSL_IR_VALUE_RECORD value;
    DSL_IR_NODE_RECORD node;
    DSL_IR_OPCODE_DESCRIPTOR_RECORD opcode;
    if (!DSL_IR_Image_Get_Value(refinement->value_id, &value) ||
        !DSL_IR_Image_Get_Node(value.producer_node_id, &node) ||
        !DSL_IR_Image_Get_Opcode_Descriptor
             (node.opcode_descriptor_id, &opcode))
        return FALSE;
    DSL_IR_VALUE_TYPE_REFINEMENT_REQUEST request;
    request.owner_pu_st = PU_Info_proc_sym(Current_PU_Info);
    request.value_id = refinement->value_id;
    request.expected_old_ty = refinement->expected_old_ty;
    request.refined_ty = refined_ty;
    context->requests.push_back(request);
    ++context->result->requested_value_count;
    if (created)
        ++context->result->created_type_count;
    else
        ++context->result->reused_type_count;
    if (context->diagnostic != NULL)
        fprintf(context->diagnostic,
                "DSL-SHAPE-REFINE: pu=%s value=%u operator=%s.v%u "
                "old_ty=%u new_ty=%u shape=%s file=%u line=%u\n",
                ST_name(St_Table[PU_Info_proc_sym(context->pu_info)]),
                refinement->value_id, Index_To_Str(opcode.stable_name),
                opcode.version,
                TY_IDX_index(refinement->expected_old_ty),
                TY_IDX_index(refined_ty), shape,
                SRCPOS_filenum(refinement->source_position),
                SRCPOS_linenum(refinement->source_position));
    return TRUE;
}

BOOL
VHO_DSL_Shape_Refine_Program_Unit
        (struct pu_info *pu_info,
         WN *tree,
         BOOL enable_refinement,
         FILE *diagnostic,
         VHO_DSL_SHAPE_REFINE_RESULT *result)
{
    VHO_DSL_SHAPE_REFINE_RESULT local_result;
    memset(&local_result, 0, sizeof(local_result));
    if (!VHO_DSL_Shape_Validate_Active_Boundary
             (pu_info, tree, diagnostic,
              &local_result.boundary_admission_count)) {
        ++local_result.diagnostic_count;
        if (result != NULL)
            *result = local_result;
        return FALSE;
    }
    if (!VHO_DSL_Shape_Has_Native_Node(tree))
        return VHO_DSL_Shape_Complete_Success
                   (pu_info, tree, diagnostic, &local_result, result);
    DSL_GATEKEEPER_RESULT gatekeeper_result;
    if (!DSL_Gatekeeper_Verify_PU_Mode
             (pu_info, DSL_GATEKEEPER_ADMISSION, diagnostic,
              &gatekeeper_result)) {
        ++local_result.diagnostic_count;
        if (result != NULL)
            *result = local_result;
        return FALSE;
    }

    VHO_DSL_SHAPE_REFINE_CONTEXT context;
    context.pu_info = pu_info;
    context.diagnostic = diagnostic;
    context.result = &local_result;
    BOOL analyzed = DSL_Shape_Analyze_PU_With_Refinements
                        (pu_info, tree, diagnostic,
                         enable_refinement ?
                             VHO_DSL_Shape_Collect_Refinement : NULL,
                         enable_refinement ? &context : NULL,
                         &local_result.solver);
    if (!analyzed) {
        ++local_result.diagnostic_count;
        if (result != NULL)
            *result = local_result;
        return FALSE;
    }
    if (diagnostic != NULL)
        fprintf(diagnostic,
                "DSL-SHAPE-REFINE-SUMMARY: pu=%s nodes=%u values=%u "
                "refinable=%u pending=%u unresolved=%u iterations=%u\n",
                ST_name(St_Table[PU_Info_proc_sym(pu_info)]),
                local_result.solver.visited_node_count,
                local_result.solver.value_count,
                local_result.solver.refinable_value_count,
                local_result.solver.pending_value_count,
                local_result.solver.unresolved_value_count,
                local_result.solver.iteration_count);

    if (!enable_refinement) {
        BOOL strict = DSL_Gatekeeper_Verify_PU_Mode
                          (pu_info, DSL_GATEKEEPER_STRICT, diagnostic,
                           &gatekeeper_result);
        if (!strict && diagnostic != NULL)
            fprintf(diagnostic,
                    "DSL-SHAPE-REFINE-DISABLED: strict check-only "
                    "verification rejected pending or refinable shapes\n");
        if (!strict) {
            if (result != NULL)
                *result = local_result;
            return FALSE;
        }
        return VHO_DSL_Shape_Complete_Success
                   (pu_info, tree, diagnostic, &local_result, result);
    }

    if (!context.requests.empty()) {
        DSL_IR_VALUE_TYPE_REFINEMENT_RESULT retype_result;
        if (!DSL_IR_Refine_Native_Value_Types
                 (pu_info, tree, &context.requests[0],
                  context.requests.size(), diagnostic, &retype_result)) {
            local_result.rollback_count = retype_result.rollback_count;
            local_result.retype_boundary_precheck_count =
                retype_result.boundary_precheck_count;
            local_result.retype_boundary_postcheck_count =
                retype_result.boundary_postcheck_count;
            ++local_result.diagnostic_count;
            if (result != NULL)
                *result = local_result;
            return FALSE;
        }
        local_result.retyped_value_count = retype_result.updated_value_count;
        local_result.updated_st_count = retype_result.updated_st_count;
        local_result.updated_wn_count = retype_result.updated_wn_count;
        local_result.rollback_count = retype_result.rollback_count;
        local_result.retype_boundary_precheck_count =
            retype_result.boundary_precheck_count;
        local_result.retype_boundary_postcheck_count =
            retype_result.boundary_postcheck_count;
    } else if (!DSL_Gatekeeper_Verify_PU_Mode
                    (pu_info, DSL_GATEKEEPER_STRICT, diagnostic,
                     &gatekeeper_result)) {
        ++local_result.diagnostic_count;
        if (result != NULL)
            *result = local_result;
        return FALSE;
    }

    return VHO_DSL_Shape_Complete_Success
               (pu_info, tree, diagnostic, &local_result, result);
}

WN *
VHO_DSL_Shape_Refine_Driver
        (struct pu_info *pu_info,
         WN *tree)
{
    VHO_DSL_SHAPE_REFINE_RESULT result;
    BOOL valid = VHO_DSL_Shape_Refine_Program_Unit
                     (pu_info, tree, VHO_DSL_Enable_Shape_Refinement,
                      stderr, &result);
    FmtAssert(valid, ("DSL tensor shape refinement failed"));
    if (VHO_DSL_Dump_After_Shape_Refinement) {
        fprintf(TFile,
                "\n\n========== WHIRL after VHO DSL Shape Refinement "
                "==========\n");
        fdump_tree(TFile, tree);
        fflush(TFile);
    }
    return tree;
}
