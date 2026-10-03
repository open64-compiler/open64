/*
 * Copyright (C) 2026 Open64 Project
 *
 * Read-only program-scope preflight for context-specialized PU cloning.
 * Physical cloning and publication are deliberately separate from this
 * validation pass; no PU, ST, WN, or mapped-image table is modified here.
 */

#include <float.h>
#include <string.h>

#include "dsl_pu_specialize.h"
#include "dsl_opcode.h"
#include "strtab.h"
#include "symtab.h"
#include "targ_const.h"

static BOOL
VHO_DSL_PU_Specialization_Report
        (FILE *diagnostic, const char *message, UINT32 index)
{
    if (diagnostic != NULL)
        fprintf(diagnostic, "DSL PU specialization: %s index=%u\n",
                message, index);
    return FALSE;
}

static BOOL
VHO_DSL_PU_Specialization_SHA256_Valid (const char *sha256)
{
    if (sha256 == NULL || strlen(sha256) != 64)
        return FALSE;
    for (UINT32 i = 0; i < 64; ++i) {
        if ((sha256[i] < '0' || sha256[i] > '9') &&
            (sha256[i] < 'a' || sha256[i] > 'f'))
            return FALSE;
    }
    return TRUE;
}

static BOOL
VHO_DSL_PU_Specialization_Has_PU (PU_Info *program, ST_IDX owner_pu_st)
{
    for (PU_Info *pu = program; pu != NULL; pu = PU_Info_next(pu)) {
        if (PU_Info_proc_sym(pu) == owner_pu_st)
            return TRUE;
    }
    return FALSE;
}

static BOOL
VHO_DSL_PU_Specialization_PU_ST_Valid (ST_IDX owner_pu_st)
{
    return ST_IDX_level(owner_pu_st) == GLOBAL_SYMTAB &&
           ST_IDX_index(owner_pu_st) != 0 &&
           ST_IDX_index(owner_pu_st) < ST_Table_Size(GLOBAL_SYMTAB) &&
           ST_class(St_Table[owner_pu_st]) == CLASS_FUNC;
}

static BOOL
VHO_DSL_PU_Specialization_Source_Value_Valid
        (DSL_IR_VALUE_ID value_id, ST_IDX owner_pu_st)
{
    DSL_IR_VALUE_RECORD value;
    DSL_IR_VALUE_RECORD owned;
    DSL_IR_NODE_RECORD node;
    DSL_IR_OPCODE_DESCRIPTOR_RECORD opcode;
    if (!DSL_IR_Image_Get_Value(value_id, &value) ||
        value.name == STR_IDX_ZERO ||
        value.value_kind != DSL_IR_VALUE_OPERATOR_RESULT ||
        value.flags != DSL_IR_VALUE_FLAG_NONE ||
        !DSL_IR_Image_Find_PU_Value
            (value.st, Index_To_Str(value.name),
             ST_name(St_Table[owner_pu_st]), &owned) ||
        owned.id != value.id ||
        !DSL_IR_Image_Get_Node(value.producer_node_id, &node) ||
        node.flags != DSL_IR_NODE_FLAG_NONE ||
        node.result_value_id != value.id ||
        !DSL_IR_Image_Get_Opcode_Descriptor
            (node.opcode_descriptor_id, &opcode))
        return FALSE;
    return opcode.logical_operator == OPR_DSLRELU &&
           opcode.effect_model == DSL_EFFECT_MODEL_PURE;
}

static BOOL
VHO_DSL_PU_Specialization_Bound_Valid
        (const DSL_PU_SPECIALIZATION_BOUND &bound,
         const DSL_CALLSITE_METADATA_RECORD &callsite)
{
    DSL_FHE_CONTEXT_RANGE_RECORD range;
    DSL_PU_SOURCE_IDENTITY_RECORD identity;
    if (!DSL_FHE_Context_Range_Get(bound.context_range_id, &range) ||
        range.context_callsite_id != bound.callsite_id ||
        range.owner_pu_st != callsite.callee_pu_st ||
        range.source_relu_value_id != bound.source_relu_value_id ||
        range.positive_bound_tcon != bound.positive_bound_tcon ||
        (range.flags & DSL_FHE_CONTEXT_RANGE_IDENTITY_IS_CALLEE) == 0 ||
        !DSL_Call_Image_Get_PU_Identity
            (range.context_pu_identity_id, &identity) ||
        identity.owner_pu_st != range.owner_pu_st ||
        !VHO_DSL_PU_Specialization_Source_Value_Valid
            (bound.source_relu_value_id, range.owner_pu_st) ||
        bound.positive_bound_tcon == TCON_IDX_ZERO ||
        bound.positive_bound_tcon >= TCON_Table_Size())
        return FALSE;
    const TCON &tcon = Tcon_Table[bound.positive_bound_tcon];
    if (TCON_ty(tcon) != MTYPE_F8)
        return FALSE;
    double value = Targ_To_Host_Float(tcon);
    return value > 0.0 && value <= DBL_MAX;
}

BOOL
VHO_DSL_PU_Specialization_Plan_Validate
        (PU_Info *program, const DSL_PU_SPECIALIZATION_PLAN *plan,
         FILE *diagnostic)
{
    if (program == NULL || plan == NULL || plan->variant_count == 0 ||
        plan->route_count == 0 || plan->bound_count == 0 ||
        plan->variants == NULL || plan->routes == NULL ||
        plan->bounds == NULL ||
        !DSL_IR_Image_Validate(diagnostic) ||
        !DSL_Call_Image_Validate(diagnostic) ||
        !DSL_PU_Interface_Image_Validate(diagnostic) ||
        !DSL_Call_ABI_Image_Validate(diagnostic) ||
        !DSL_FHE_Approx_Profile_Image_Validate(diagnostic))
        return VHO_DSL_PU_Specialization_Report
                   (diagnostic, "invalid program or image", 0);

    for (UINT32 i = 0; i < plan->variant_count; ++i) {
        const DSL_PU_SPECIALIZATION_VARIANT &variant = plan->variants[i];
        if (!VHO_DSL_PU_Specialization_PU_ST_Valid
                 (variant.source_pu_st) ||
            !VHO_DSL_PU_Specialization_Has_PU
                (program, variant.source_pu_st) ||
            variant.clone_name == NULL || variant.clone_name[0] == '\0' ||
            !VHO_DSL_PU_Specialization_SHA256_Valid
                (variant.signature_sha256))
            return VHO_DSL_PU_Specialization_Report
                       (diagnostic, "invalid variant", i);
        for (UINT32 st_index = 1;
             st_index < ST_Table_Size(GLOBAL_SYMTAB); ++st_index) {
            ST_IDX st = make_ST_IDX(st_index, GLOBAL_SYMTAB);
            if (strcmp(ST_name(St_Table[st]), variant.clone_name) == 0)
                return VHO_DSL_PU_Specialization_Report
                           (diagnostic, "clone name exists", i);
        }
        for (UINT32 j = 0; j < i; ++j) {
            const DSL_PU_SPECIALIZATION_VARIANT &previous =
                plan->variants[j];
            if (strcmp(previous.clone_name, variant.clone_name) == 0 ||
                (previous.source_pu_st == variant.source_pu_st &&
                 strcmp(previous.signature_sha256,
                        variant.signature_sha256) == 0))
                return VHO_DSL_PU_Specialization_Report
                           (diagnostic, "duplicate variant", i);
        }
    }

    for (UINT32 i = 0; i < plan->route_count; ++i) {
        const DSL_PU_SPECIALIZATION_ROUTE &route = plan->routes[i];
        DSL_CALLSITE_METADATA_RECORD callsite;
        if (route.variant_index >= plan->variant_count ||
            !DSL_Call_Image_Get_Callsite(route.callsite_id, &callsite) ||
            callsite.callee_pu_st !=
                plan->variants[route.variant_index].source_pu_st ||
            callsite.owner_pu_st == callsite.callee_pu_st ||
            !VHO_DSL_PU_Specialization_Has_PU
                (program, callsite.owner_pu_st))
            return VHO_DSL_PU_Specialization_Report
                       (diagnostic, "invalid call route", i);
        for (UINT32 j = 0; j < i; ++j) {
            if (plan->routes[j].callsite_id == route.callsite_id)
                return VHO_DSL_PU_Specialization_Report
                           (diagnostic, "duplicate call route", i);
        }
    }

    for (UINT32 i = 1; i <= DSL_Call_Image_Callsite_Count(); ++i) {
        DSL_CALLSITE_METADATA_RECORD callsite;
        if (!DSL_Call_Image_Get_Callsite(i, &callsite))
            return VHO_DSL_PU_Specialization_Report
                       (diagnostic, "invalid callsite image", i);
        BOOL targeted = FALSE;
        BOOL routed = FALSE;
        for (UINT32 j = 0; j < plan->variant_count; ++j)
            targeted |= callsite.callee_pu_st ==
                        plan->variants[j].source_pu_st;
        for (UINT32 j = 0; j < plan->route_count; ++j)
            routed |= plan->routes[j].callsite_id == i;
        if (targeted && !routed)
            return VHO_DSL_PU_Specialization_Report
                       (diagnostic, "unrouted source call", i);
    }

    for (UINT32 i = 0; i < plan->bound_count; ++i) {
        const DSL_PU_SPECIALIZATION_BOUND &bound = plan->bounds[i];
        DSL_CALLSITE_METADATA_RECORD callsite;
        BOOL has_route = FALSE;
        if (!DSL_Call_Image_Get_Callsite(bound.callsite_id, &callsite) ||
            !VHO_DSL_PU_Specialization_Bound_Valid(bound, callsite))
            return VHO_DSL_PU_Specialization_Report
                       (diagnostic, "invalid bound", i);
        for (UINT32 j = 0; j < plan->route_count; ++j)
            has_route |= plan->routes[j].callsite_id == bound.callsite_id;
        if (!has_route)
            return VHO_DSL_PU_Specialization_Report
                       (diagnostic, "unrouted bound", i);
        for (UINT32 j = 0; j < i; ++j) {
            if (plan->bounds[j].callsite_id == bound.callsite_id &&
                plan->bounds[j].bound_slot == bound.bound_slot)
                return VHO_DSL_PU_Specialization_Report
                           (diagnostic, "duplicate bound slot", i);
        }
    }

    for (UINT32 i = 0; i < plan->route_count; ++i) {
        const DSL_PU_SPECIALIZATION_ROUTE &route = plan->routes[i];
        UINT32 slot_count = 0;
        for (UINT32 j = 0; j < plan->bound_count; ++j) {
            if (plan->bounds[j].callsite_id == route.callsite_id)
                ++slot_count;
        }
        if (slot_count == 0)
            return VHO_DSL_PU_Specialization_Report
                       (diagnostic, "route has no bound", i);
        for (UINT32 slot = 0; slot < slot_count; ++slot) {
            BOOL found = FALSE;
            DSL_IR_VALUE_ID source_value_id = DSL_IR_VALUE_INVALID_ID;
            for (UINT32 j = 0; j < plan->bound_count; ++j) {
                const DSL_PU_SPECIALIZATION_BOUND &bound = plan->bounds[j];
                if (bound.callsite_id == route.callsite_id &&
                    bound.bound_slot == slot) {
                    found = TRUE;
                    source_value_id = bound.source_relu_value_id;
                }
            }
            if (!found)
                return VHO_DSL_PU_Specialization_Report
                           (diagnostic, "non-dense bound slots", i);
            for (UINT32 other = 0; other < i; ++other) {
                if (plan->routes[other].variant_index !=
                    route.variant_index)
                    continue;
                BOOL matched = FALSE;
                for (UINT32 j = 0; j < plan->bound_count; ++j) {
                    const DSL_PU_SPECIALIZATION_BOUND &bound =
                        plan->bounds[j];
                    matched |= bound.callsite_id ==
                                   plan->routes[other].callsite_id &&
                               bound.bound_slot == slot &&
                               bound.source_relu_value_id == source_value_id;
                }
                if (!matched)
                    return VHO_DSL_PU_Specialization_Report
                               (diagnostic,
                                "variant has inconsistent bound roles", i);
            }
        }
    }
    return TRUE;
}
