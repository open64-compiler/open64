/*
 * Copyright (C) 2026 Open64 Project
 */

#include <ctype.h>
#include <string.h>
#include <errno.h>
#include <stdlib.h>
#include <algorithm>
#include <sstream>
#include <string>
#include <vector>

#include "dsl_memory_behavior.h"
#include "dsl_fhe.h"
#include "dsl_fhe_plan.h"
#include "dsl_gatekeeper.h"
#include "dsl_shape.h"
#include "dsl_tensor_fold.h"
#include "dsl_ir_image.h"
#include "dsl_region.h"
#include "pu_info.h"
#include "strtab.h"
#include "symtab.h"
#include "wn.h"

/* Commit-only helpers; public callers must use the transactional APIs below. */
extern BOOL DSL_Call_ABI_Image_Update_Argument_Value
                                (const WN *, UINT32, DSL_IR_VALUE_ID,
                                 DSL_IR_VALUE_ID);
extern BOOL DSL_IR_Image_Redirect_And_Retire_Value
                                (DSL_IR_VALUE_ID, DSL_IR_VALUE_ID, UINT32);
extern BOOL DSL_IR_Image_Mark_Value_Lowered (DSL_IR_VALUE_ID);
extern BOOL DSL_IR_Image_Retype_Value
                                (DSL_IR_VALUE_ID, TY_IDX, TY_IDX);
extern DSL_RUNTIME_VALUE_PROJECTION_ID
DSL_Runtime_Interface_Image_Add_Value
                                (const DSL_RUNTIME_VALUE_PROJECTION_RECORD *);
extern DSL_RUNTIME_CALL_PROJECTION_ID
DSL_Runtime_Interface_Image_Add_Call
                                (const DSL_RUNTIME_CALL_PROJECTION_RECORD *);
extern DSL_RETIRED_FORMAL_ID
DSL_Program_Interface_Image_Add_Retired_Formal
                                (const DSL_RETIRED_FORMAL_RECORD *);
extern DSL_RETIRED_CALL_ARGUMENT_ID
DSL_Program_Interface_Image_Add_Retired_Call
                                (const DSL_RETIRED_CALL_ARGUMENT_RECORD *);
extern DSL_RUNTIME_INPUT_ID DSL_Program_Interface_Image_Add_Runtime_Input
                                (const DSL_RUNTIME_INPUT_RECORD *);
extern DSL_RUNTIME_INPUT_BINDING_ID
DSL_Program_Interface_Image_Add_Runtime_Binding
                                (const DSL_RUNTIME_INPUT_BINDING_RECORD *);
extern DSL_RUNTIME_INPUT_CALL_ID
DSL_Program_Interface_Image_Add_Runtime_Call
                                (const DSL_RUNTIME_INPUT_CALL_RECORD *);
extern BOOL DSL_Call_Image_Replace_Call_WN
                                (DSL_CALLSITE_METADATA_ID, const WN *, WN *);
extern UINT32 DSL_Region_Symbol_Use_Count (PU_Info *, ST_IDX);
extern BOOL DSL_Region_Can_Redirect_Symbol (PU_Info *, ST_IDX, ST_IDX);
extern BOOL DSL_Region_Redirect_Symbol (PU_Info *, ST_IDX, ST_IDX);
#include "wn_util.h"

static BOOL DSL_IR_Image_PU_ST_Valid (ST_IDX st);
static BOOL DSL_IR_Image_Current_PU_Is (ST_IDX owner_pu_st);

static BOOL
DSL_Runtime_Interface_Value_Owner_Valid
        (ST_IDX owner_pu_st, const DSL_IR_VALUE_RECORD &value)
{
    DSL_IR_VALUE_RECORD owned;
    return DSL_IR_Image_PU_ST_Valid(owner_pu_st) &&
           value.name != STR_IDX_ZERO &&
           DSL_IR_Image_Find_PU_Value
               (value.st, Index_To_Str(value.name),
                ST_name(St_Table[owner_pu_st]), &owned) &&
           owned.id == value.id;
}

static BOOL
DSL_Runtime_Interface_Report
        (FILE *diagnostic, const char *message, UINT32 id)
{
    if (diagnostic != NULL)
        fprintf(diagnostic, "DSL runtime interface error: %s id=%u\n",
                message, id);
    return FALSE;
}

static const DSL_RUNTIME_VALUE_PROJECTION_REQUEST *
DSL_Runtime_Interface_Find_Value_Request
        (const DSL_RUNTIME_INTERFACE_PLAN *plan, ST_IDX owner_pu_st,
         DSL_IR_VALUE_ID source_value_id)
{
    if (plan == NULL)
        return NULL;
    for (UINT32 i = 0; i < plan->value_count; ++i) {
        const DSL_RUNTIME_VALUE_PROJECTION_REQUEST &request = plan->values[i];
        if (request.owner_pu_st == owner_pu_st &&
            request.source_value_id == source_value_id)
            return &request;
    }
    return NULL;
}

BOOL
DSL_Runtime_Interface_Plan_Validate
        (const DSL_RUNTIME_INTERFACE_PLAN *plan, FILE *diagnostic)
{
    if (plan == NULL ||
        (plan->value_count != 0 && plan->values == NULL) ||
        (plan->call_count != 0 && plan->calls == NULL))
        return DSL_Runtime_Interface_Report
                   (diagnostic, "plan is incomplete", 0);

    for (UINT32 i = 0; i < plan->value_count; ++i) {
        const DSL_RUNTIME_VALUE_PROJECTION_REQUEST &request = plan->values[i];
        DSL_IR_VALUE_RECORD source;
        DSL_PU_FORMAL_RECORD formal;
        BOOL is_formal = request.binding_kind ==
                             DSL_RUNTIME_BINDING_INPUT_FORMAL ||
                         request.binding_kind ==
                             DSL_RUNTIME_BINDING_RESULT_FORMAL;
        if (!DSL_IR_Image_PU_ST_Valid(request.owner_pu_st) ||
            request.source_value_id == DSL_IR_VALUE_INVALID_ID ||
            !DSL_IR_Image_Get_Value(request.source_value_id, &source) ||
            source.st != request.expected_source_st ||
            source.ty != request.expected_source_ty ||
            !DSL_Runtime_Interface_Value_Owner_Valid
                 (request.owner_pu_st, source) ||
            !TY_is_tensor_extension(request.expected_source_ty) ||
            request.handle_ty == TY_IDX_ZERO ||
            TY_IDX_index(request.handle_ty) >= TY_Table_Size() ||
            TY_kind(request.handle_ty) != KIND_POINTER ||
            request.binding_kind < DSL_RUNTIME_BINDING_LOCAL_VALUE ||
            request.binding_kind > DSL_RUNTIME_BINDING_RESULT_FORMAL ||
            (is_formal && request.formal_ordinal ==
                              DSL_RUNTIME_INTERFACE_INVALID_ORDINAL) ||
            (!is_formal && request.formal_ordinal !=
                               DSL_RUNTIME_INTERFACE_INVALID_ORDINAL))
            return DSL_Runtime_Interface_Report
                       (diagnostic, "invalid value request", i + 1);
        if (is_formal &&
            (!DSL_PU_Interface_Image_Find_Formal
                 (request.owner_pu_st, request.formal_ordinal, &formal) ||
             formal.formal_value_id != request.source_value_id ||
             formal.formal_st != request.expected_source_st ||
             formal.formal_ty != request.expected_source_ty))
            return DSL_Runtime_Interface_Report
                       (diagnostic, "formal request mismatch", i + 1);
        for (UINT32 j = 0; j < i; ++j) {
            if (plan->values[j].owner_pu_st == request.owner_pu_st &&
                (plan->values[j].source_value_id == request.source_value_id ||
                 (is_formal && plan->values[j].formal_ordinal ==
                                   request.formal_ordinal)))
                return DSL_Runtime_Interface_Report
                           (diagnostic, "duplicate value request", i + 1);
        }
    }

    for (UINT32 i = 1; i <= DSL_PU_Interface_Image_Formal_Count(); ++i) {
        DSL_PU_FORMAL_RECORD formal;
        if (!DSL_PU_Interface_Image_Get_Formal(i, &formal) ||
            DSL_Runtime_Interface_Find_Value_Request
                (plan, formal.owner_pu_st, formal.formal_value_id) == NULL)
            return DSL_Runtime_Interface_Report
                       (diagnostic, "incomplete formal projection", i);
    }

    for (UINT32 i = 0; i < plan->call_count; ++i) {
        const DSL_RUNTIME_CALL_PROJECTION_REQUEST &request = plan->calls[i];
        DSL_CALLSITE_METADATA_RECORD callsite;
        DSL_PU_FORMAL_RECORD formal;
        const DSL_RUNTIME_VALUE_PROJECTION_REQUEST *value =
            DSL_Runtime_Interface_Find_Value_Request
                (plan, request.owner_pu_st, request.source_value_id);
        if (request.callsite_id == DSL_CALLSITE_METADATA_INVALID_ID ||
            !DSL_Call_Image_Get_Callsite(request.callsite_id, &callsite) ||
            callsite.owner_pu_st != request.owner_pu_st || value == NULL ||
            request.actual_ordinal == DSL_RUNTIME_INTERFACE_INVALID_ORDINAL ||
            request.callee_formal_ordinal ==
                DSL_RUNTIME_INTERFACE_INVALID_ORDINAL ||
            request.actual_ordinal != request.callee_formal_ordinal ||
            request.direction < DSL_RUNTIME_CALL_INPUT ||
            request.direction > DSL_RUNTIME_CALL_RESULT ||
            !DSL_PU_Interface_Image_Find_Formal
                 (callsite.callee_pu_st, request.callee_formal_ordinal,
                  &formal))
            return DSL_Runtime_Interface_Report
                       (diagnostic, "invalid call request", i + 1);
        const DSL_RUNTIME_VALUE_PROJECTION_REQUEST *formal_projection =
            DSL_Runtime_Interface_Find_Value_Request
                (plan, callsite.callee_pu_st, formal.formal_value_id);
        if (formal_projection == NULL ||
            formal_projection->handle_ty != value->handle_ty ||
            formal_projection->formal_ordinal !=
                request.callee_formal_ordinal ||
            (request.direction == DSL_RUNTIME_CALL_INPUT &&
             formal_projection->binding_kind !=
                 DSL_RUNTIME_BINDING_INPUT_FORMAL) ||
            (request.direction == DSL_RUNTIME_CALL_RESULT &&
             formal_projection->binding_kind !=
                 DSL_RUNTIME_BINDING_RESULT_FORMAL))
            return DSL_Runtime_Interface_Report
                       (diagnostic, "call type mismatch", i + 1);
        if (request.direction == DSL_RUNTIME_CALL_INPUT) {
            DSL_CALL_ARGUMENT_RECORD argument;
            if (!DSL_Call_ABI_Image_Find_Argument_By_Id
                    (request.callsite_id, request.actual_ordinal, &argument) ||
                argument.argument_value_id != request.source_value_id ||
                argument.callee_formal_ordinal !=
                    request.callee_formal_ordinal)
                return DSL_Runtime_Interface_Report
                           (diagnostic, "call input mismatch", i + 1);
        }
        for (UINT32 j = 0; j < i; ++j) {
            if (plan->calls[j].callsite_id == request.callsite_id &&
                plan->calls[j].actual_ordinal == request.actual_ordinal)
                return DSL_Runtime_Interface_Report
                           (diagnostic, "duplicate call request", i + 1);
        }
    }

    for (UINT32 i = 1; i <= DSL_Call_ABI_Image_Argument_Count(); ++i) {
        DSL_CALL_ARGUMENT_RECORD argument;
        BOOL found = FALSE;
        if (!DSL_Call_ABI_Image_Get_Argument(i, &argument))
            return DSL_Runtime_Interface_Report
                       (diagnostic, "missing call argument", i);
        for (UINT32 j = 0; j < plan->call_count; ++j) {
            const DSL_RUNTIME_CALL_PROJECTION_REQUEST &request =
                plan->calls[j];
            if (request.callsite_id == argument.callsite_id &&
                request.actual_ordinal == argument.actual_ordinal &&
                request.direction == DSL_RUNTIME_CALL_INPUT) {
                found = TRUE;
                break;
            }
        }
        if (!found)
            return DSL_Runtime_Interface_Report
                       (diagnostic, "incomplete call input projection", i);
    }

    for (UINT32 call_id = 1;
         call_id <= DSL_Call_Image_Callsite_Count(); ++call_id) {
        DSL_CALLSITE_METADATA_RECORD callsite;
        if (!DSL_Call_Image_Get_Callsite(call_id, &callsite))
            return DSL_Runtime_Interface_Report
                       (diagnostic, "missing callsite", call_id);
        for (UINT32 formal_id = 1;
             formal_id <= DSL_PU_Interface_Image_Formal_Count();
             ++formal_id) {
            DSL_PU_FORMAL_RECORD formal;
            if (!DSL_PU_Interface_Image_Get_Formal(formal_id, &formal))
                return DSL_Runtime_Interface_Report
                           (diagnostic, "missing formal", formal_id);
            if (formal.owner_pu_st != callsite.callee_pu_st)
                continue;
            const DSL_RUNTIME_VALUE_PROJECTION_REQUEST *formal_request =
                DSL_Runtime_Interface_Find_Value_Request
                    (plan, formal.owner_pu_st, formal.formal_value_id);
            UINT32 input_relation_count = 0;
            for (UINT32 argument_id = 1;
                 argument_id <= DSL_Call_ABI_Image_Argument_Count();
                 ++argument_id) {
                DSL_CALL_ARGUMENT_RECORD argument;
                if (!DSL_Call_ABI_Image_Get_Argument
                        (argument_id, &argument))
                    return DSL_Runtime_Interface_Report
                               (diagnostic, "missing call argument",
                                argument_id);
                if (argument.callsite_id == callsite.id &&
                    argument.callee_formal_ordinal ==
                        formal.formal_ordinal)
                    ++input_relation_count;
            }
            UINT32 expected_direction = input_relation_count == 1 ?
                DSL_RUNTIME_CALL_INPUT : DSL_RUNTIME_CALL_RESULT;
            UINT32 expected_binding = input_relation_count == 1 ?
                DSL_RUNTIME_BINDING_INPUT_FORMAL :
                DSL_RUNTIME_BINDING_RESULT_FORMAL;
            if (input_relation_count > 1 || formal_request == NULL ||
                formal_request->binding_kind != expected_binding)
                return DSL_Runtime_Interface_Report
                           (diagnostic, "formal call role mismatch",
                            formal_id);
            UINT32 found = 0;
            for (UINT32 i = 0; i < plan->call_count; ++i) {
                const DSL_RUNTIME_CALL_PROJECTION_REQUEST &request =
                    plan->calls[i];
                if (request.callsite_id == callsite.id &&
                    request.callee_formal_ordinal ==
                        formal.formal_ordinal &&
                    request.direction == expected_direction)
                    ++found;
            }
            if (found != 1)
                return DSL_Runtime_Interface_Report
                           (diagnostic, "incomplete call projection",
                            call_id);
        }
    }
    return TRUE;
}

void
DSL_Runtime_Interface_Result_Init (DSL_RUNTIME_INTERFACE_RESULT *result)
{
    if (result != NULL)
        memset(result, 0, sizeof(*result));
}

typedef struct {
    const DSL_RUNTIME_VALUE_PROJECTION_REQUEST *request;
    ST_IDX handle_st;
    DSL_RUNTIME_VALUE_PROJECTION_ID projection_id;
} DSL_RUNTIME_CREATED_VALUE;

typedef struct {
    WN *parent_block;
    WN *store;
    const DSL_RUNTIME_VALUE_PROJECTION_REQUEST *result_formal;
    const DSL_RUNTIME_VALUE_PROJECTION_REQUEST *source_value;
} DSL_RUNTIME_RETURN_SITE;

static const DSL_RUNTIME_CREATED_VALUE *
DSL_Runtime_Interface_Find_Created
        (const std::vector<DSL_RUNTIME_CREATED_VALUE> &created,
         DSL_IR_VALUE_ID source_value_id)
{
    for (UINT32 i = 0; i < created.size(); ++i) {
        if (created[i].request->source_value_id == source_value_id)
            return &created[i];
    }
    return NULL;
}

static WN *
DSL_Runtime_Interface_Parent_Block (WN *tree, const WN *target)
{
    if (tree == NULL)
        return NULL;
    if (WN_operator(tree) == OPR_BLOCK) {
        for (WN *stmt = WN_first(tree); stmt != NULL; stmt = WN_next(stmt)) {
            if (stmt == target)
                return tree;
            WN *found = DSL_Runtime_Interface_Parent_Block(stmt, target);
            if (found != NULL)
                return found;
        }
        return NULL;
    }
    for (UINT32 i = 0; i < WN_kid_count(tree); ++i) {
        WN *found = DSL_Runtime_Interface_Parent_Block(WN_kid(tree, i), target);
        if (found != NULL)
            return found;
    }
    return NULL;
}

static BOOL
DSL_Runtime_Interface_Find_Source_Value
        (ST_IDX owner_pu_st, ST_IDX source_st,
         DSL_IR_VALUE_RECORD *source)
{
    if (ST_IDX_level(source_st) != CURRENT_SYMTAB ||
        ST_IDX_index(source_st) == 0 ||
        ST_IDX_index(source_st) >= ST_Table_Size(CURRENT_SYMTAB))
        return FALSE;
    return DSL_IR_Image_Find_PU_Value
               (source_st, ST_name(St_Table[source_st]),
                ST_name(St_Table[owner_pu_st]), source);
}

static BOOL
DSL_Runtime_Interface_Collect_Returns
        (WN *tree, WN *parent_block, ST_IDX owner_pu_st,
         const DSL_RUNTIME_INTERFACE_PLAN *plan,
         std::vector<DSL_RUNTIME_RETURN_SITE> *sites,
         FILE *diagnostic)
{
    if (tree == NULL)
        return TRUE;
    if (WN_operator(tree) == OPR_BLOCK) {
        for (WN *stmt = WN_first(tree); stmt != NULL; stmt = WN_next(stmt)) {
            if (!DSL_Runtime_Interface_Collect_Returns
                    (stmt, tree, owner_pu_st, plan, sites, diagnostic))
                return FALSE;
        }
        return TRUE;
    }
    if (WN_operator(tree) == OPR_STID) {
        for (UINT32 i = 0; i < plan->value_count; ++i) {
            const DSL_RUNTIME_VALUE_PROJECTION_REQUEST &result =
                plan->values[i];
            if (result.owner_pu_st != owner_pu_st ||
                result.binding_kind != DSL_RUNTIME_BINDING_RESULT_FORMAL ||
                WN_st_idx(tree) != result.expected_source_st)
                continue;
            WN *rhs = WN_kid0(tree);
            DSL_IR_VALUE_RECORD source;
            if (parent_block == NULL || rhs == NULL ||
                WN_operator(rhs) != OPR_LDID ||
                !DSL_Runtime_Interface_Find_Source_Value
                    (owner_pu_st, WN_st_idx(rhs), &source))
                return DSL_Runtime_Interface_Report
                           (diagnostic, "unsupported PU return store",
                            result.formal_ordinal);
            const DSL_RUNTIME_VALUE_PROJECTION_REQUEST *source_request =
                DSL_Runtime_Interface_Find_Value_Request
                    (plan, owner_pu_st, source.id);
            if (source_request == NULL ||
                source_request->handle_ty != result.handle_ty)
                return DSL_Runtime_Interface_Report
                           (diagnostic, "return handle type mismatch",
                            result.formal_ordinal);
            DSL_RUNTIME_RETURN_SITE site;
            site.parent_block = parent_block;
            site.store = tree;
            site.result_formal = &result;
            site.source_value = source_request;
            sites->push_back(site);
            return TRUE;
        }
    }
    for (UINT32 i = 0; i < WN_kid_count(tree); ++i) {
        if (!DSL_Runtime_Interface_Collect_Returns
                (WN_kid(tree, i), parent_block, owner_pu_st, plan, sites,
                 diagnostic))
            return FALSE;
    }
    return TRUE;
}

static TY_IDX
DSL_Runtime_Interface_Function_TY
        (ST_IDX owner_pu_st, const DSL_RUNTIME_INTERFACE_PLAN *plan)
{
    TY_IDX function_ty;
    TY &function = New_TY(function_ty);
    TY_Init(function, 0, KIND_FUNCTION, MTYPE_UNKNOWN, 0);
    Set_TY_align(function_ty, 1);

    TYLIST_IDX tylist_idx;
    Set_TYLIST_type(New_TYLIST(tylist_idx), MTYPE_To_TY(MTYPE_V));
    Set_TY_tylist(function_ty, tylist_idx);
    for (UINT32 ordinal = 0;; ++ordinal) {
        DSL_PU_FORMAL_RECORD formal;
        if (!DSL_PU_Interface_Image_Find_Formal
                (owner_pu_st, ordinal, &formal))
            break;
        const DSL_RUNTIME_VALUE_PROJECTION_REQUEST *request =
            DSL_Runtime_Interface_Find_Value_Request
                (plan, owner_pu_st, formal.formal_value_id);
        if (request == NULL)
            return TY_IDX_ZERO;
        TY_IDX formal_ty = request->binding_kind ==
                               DSL_RUNTIME_BINDING_RESULT_FORMAL ?
                           Make_Pointer_Type(request->handle_ty) :
                           request->handle_ty;
        Set_TYLIST_type(New_TYLIST(tylist_idx), formal_ty);
    }
    Set_TYLIST_type(New_TYLIST(tylist_idx), TY_IDX_ZERO);
    TY_IDX unique_ty = TY_is_unique(function_ty);
    return unique_ty;
}

static ST_IDX
DSL_Runtime_Interface_Create_Handle_ST
        (const DSL_RUNTIME_VALUE_PROJECTION_REQUEST &request)
{
    ST *source = &St_Table[request.expected_source_st];
    std::string name("__dsl_runtime_");
    name += ST_name(source);
    char suffix[32];
    snprintf(suffix, sizeof(suffix), "_%u", request.source_value_id);
    name += suffix;

    ST_SCLASS sclass = SCLASS_AUTO;
    if (request.binding_kind == DSL_RUNTIME_BINDING_INPUT_FORMAL)
        sclass = SCLASS_FORMAL;
    else if (request.binding_kind == DSL_RUNTIME_BINDING_RESULT_FORMAL)
        sclass = SCLASS_FORMAL_REF;
    ST *handle = New_ST(CURRENT_SYMTAB);
    ST_Init(handle, Save_Str(name.c_str()), CLASS_VAR, sclass,
            EXPORT_LOCAL, request.handle_ty);
    if (sclass == SCLASS_FORMAL)
        Set_ST_is_value_parm(*handle);
    else if (sclass == SCLASS_AUTO)
        Set_ST_is_temp_var(*handle);
    Set_ST_Srcpos(*handle, ST_Srcpos(*source));
    return ST_st_idx(handle);
}

static BOOL
DSL_Runtime_Interface_Preflight_PU
        (PU_Info *pu, const DSL_RUNTIME_INTERFACE_PLAN *plan,
         std::vector<DSL_RUNTIME_RETURN_SITE> *returns,
         FILE *diagnostic)
{
    if (pu == NULL || PU_Info_tree_ptr(pu) == NULL ||
        !DSL_IR_Image_Current_PU_Is(PU_Info_proc_sym(pu)))
        return DSL_Runtime_Interface_Report
                   (diagnostic, "program unit is not active", 0);
    ST_IDX owner_pu_st = PU_Info_proc_sym(pu);
    WN *entry = PU_Info_tree_ptr(pu);
    if (WN_operator(entry) != OPR_FUNC_ENTRY)
        return DSL_Runtime_Interface_Report
                   (diagnostic, "program unit has no FUNC_ENTRY", 0);

    UINT32 projected_formals = 0;
    for (UINT32 i = 0; i < plan->value_count; ++i) {
        const DSL_RUNTIME_VALUE_PROJECTION_REQUEST &request = plan->values[i];
        if (request.owner_pu_st != owner_pu_st)
            continue;
        if (DSL_Runtime_Interface_Image_Find_Value
                (owner_pu_st, request.source_value_id, NULL) ||
            ST_IDX_level(request.expected_source_st) != CURRENT_SYMTAB ||
            ST_IDX_index(request.expected_source_st) == 0 ||
            ST_IDX_index(request.expected_source_st) >=
                ST_Table_Size(CURRENT_SYMTAB) ||
            ST_type(St_Table[request.expected_source_st]) !=
                request.expected_source_ty)
            return DSL_Runtime_Interface_Report
                       (diagnostic, "active value request mismatch", i + 1);
        if (request.binding_kind == DSL_RUNTIME_BINDING_INPUT_FORMAL ||
            request.binding_kind == DSL_RUNTIME_BINDING_RESULT_FORMAL) {
            if (request.formal_ordinal >= WN_num_formals(entry) ||
                WN_st_idx(WN_formal(entry, request.formal_ordinal)) !=
                    request.expected_source_st)
                return DSL_Runtime_Interface_Report
                           (diagnostic, "physical formal mismatch", i + 1);
            ++projected_formals;
        }
    }
    if (projected_formals != WN_num_formals(entry))
        return DSL_Runtime_Interface_Report
                   (diagnostic, "incomplete physical formal projection", 0);

    for (UINT32 i = 0; i < plan->call_count; ++i) {
        const DSL_RUNTIME_CALL_PROJECTION_REQUEST &request = plan->calls[i];
        if (request.owner_pu_st != owner_pu_st)
            continue;
        const WN *call = DSL_Call_Image_Get_Call_WN(request.callsite_id);
        DSL_CALLSITE_METADATA_RECORD callsite;
        UINT32 callee_formal_count = 0;
        if (!DSL_Call_Image_Get_Callsite(request.callsite_id, &callsite))
            return DSL_Runtime_Interface_Report
                       (diagnostic, "callsite is unavailable", i + 1);
        for (UINT32 formal_id = 1;
             formal_id <= DSL_PU_Interface_Image_Formal_Count();
             ++formal_id) {
            DSL_PU_FORMAL_RECORD formal;
            if (!DSL_PU_Interface_Image_Get_Formal(formal_id, &formal))
                return DSL_Runtime_Interface_Report
                           (diagnostic, "missing formal", formal_id);
            if (formal.owner_pu_st == callsite.callee_pu_st)
                ++callee_formal_count;
        }
        if (call == NULL || request.actual_ordinal >= WN_kid_count(call) ||
            request.actual_ordinal != request.callee_formal_ordinal ||
            (UINT32)WN_kid_count(call) != callee_formal_count ||
            DSL_Runtime_Interface_Parent_Block(entry, call) == NULL)
            return DSL_Runtime_Interface_Report
                       (diagnostic, "physical call is unavailable", i + 1);
        WN *parm = WN_kid(call, request.actual_ordinal);
        WN *address = parm == NULL || WN_operator(parm) != OPR_PARM ?
                      NULL : WN_kid0(parm);
        const DSL_RUNTIME_VALUE_PROJECTION_REQUEST *value =
            DSL_Runtime_Interface_Find_Value_Request
                (plan, owner_pu_st, request.source_value_id);
        if (value == NULL || address == NULL ||
            WN_operator(address) != OPR_LDA ||
            WN_st_idx(address) != value->expected_source_st ||
            WN_ty(parm) != WN_ty(address) ||
            TY_kind(WN_ty(parm)) != KIND_POINTER ||
            TY_pointed(WN_ty(parm)) != value->expected_source_ty ||
            (request.direction == DSL_RUNTIME_CALL_INPUT &&
             WN_parm_flag(parm) !=
                 (WN_PARM_BY_REFERENCE | WN_PARM_READ_ONLY |
                  WN_PARM_PASSED_NOT_SAVED)) ||
            (request.direction == DSL_RUNTIME_CALL_RESULT &&
             WN_parm_flag(parm) !=
                 (WN_PARM_BY_REFERENCE | WN_PARM_OUT |
                  WN_PARM_PASSED_NOT_SAVED)))
            return DSL_Runtime_Interface_Report
                       (diagnostic, "physical call operand mismatch", i + 1);
    }

    return DSL_Runtime_Interface_Collect_Returns
               (entry, NULL, owner_pu_st, plan, returns, diagnostic);
}

BOOL
DSL_Runtime_Interface_Apply_PU
        (PU_Info *pu, const DSL_RUNTIME_INTERFACE_PLAN *plan,
         FILE *diagnostic, DSL_RUNTIME_INTERFACE_RESULT *result)
{
    DSL_Runtime_Interface_Result_Init(result);
    std::vector<DSL_RUNTIME_RETURN_SITE> returns;
    if (result == NULL || !DSL_Runtime_Interface_Plan_Validate(plan, diagnostic) ||
        !DSL_Runtime_Interface_Preflight_PU
             (pu, plan, &returns, diagnostic))
        return FALSE;

    ST_IDX owner_pu_st = PU_Info_proc_sym(pu);
    WN *old_entry = PU_Info_tree_ptr(pu);
    std::vector<DSL_RUNTIME_CREATED_VALUE> created;
    for (UINT32 i = 0; i < plan->value_count; ++i) {
        const DSL_RUNTIME_VALUE_PROJECTION_REQUEST &request = plan->values[i];
        if (request.owner_pu_st != owner_pu_st)
            continue;
        DSL_RUNTIME_CREATED_VALUE value;
        value.request = &request;
        value.handle_st = DSL_Runtime_Interface_Create_Handle_ST(request);
        value.projection_id = DSL_RUNTIME_VALUE_PROJECTION_INVALID_ID;
        created.push_back(value);
    }

    UINT32 formal_count = 0;
    for (UINT32 i = 0; i < created.size(); ++i) {
        if (created[i].request->binding_kind !=
            DSL_RUNTIME_BINDING_LOCAL_VALUE)
            ++formal_count;
    }
    WN *entry = WN_CreateEntry
                    ((INT16)formal_count, owner_pu_st,
                     WN_func_body(old_entry), WN_func_pragmas(old_entry),
                     WN_func_varrefs(old_entry));
    for (UINT32 ordinal = 0; ordinal < formal_count; ++ordinal) {
        const DSL_RUNTIME_CREATED_VALUE *formal = NULL;
        for (UINT32 i = 0; i < created.size(); ++i) {
            if (created[i].request->formal_ordinal == ordinal) {
                formal = &created[i];
                break;
            }
        }
        if (formal == NULL)
            return DSL_Runtime_Interface_Report
                       (diagnostic, "missing created formal", ordinal);
        WN_formal(entry, ordinal) = WN_CreateIdname(0, formal->handle_st);
    }
    TY_IDX function_ty = DSL_Runtime_Interface_Function_TY(owner_pu_st, plan);
    if (function_ty == TY_IDX_ZERO)
        return DSL_Runtime_Interface_Report
                   (diagnostic, "could not create projected prototype", 0);
    Set_PU_prototype(Pu_Table[ST_pu(St_Table[owner_pu_st])], function_ty);
    Set_PU_Info_tree_ptr(pu, entry);

    for (UINT32 i = 0; i < plan->call_count; ++i) {
        const DSL_RUNTIME_CALL_PROJECTION_REQUEST &request = plan->calls[i];
        if (request.owner_pu_st != owner_pu_st)
            continue;
        WN *call = const_cast<WN *>
                       (DSL_Call_Image_Get_Call_WN(request.callsite_id));
        WN *old_parm = WN_kid(call, request.actual_ordinal);
        const DSL_RUNTIME_CREATED_VALUE *value =
            DSL_Runtime_Interface_Find_Created
                (created, request.source_value_id);
        if (request.direction == DSL_RUNTIME_CALL_INPUT) {
            TYPE_ID mtype = TY_mtype(value->request->handle_ty);
            WN *load = WN_CreateLdid
                           (OPR_LDID, mtype, mtype, 0, value->handle_st,
                            value->request->handle_ty);
            WN_kid(call, request.actual_ordinal) = WN_CreateParm
                (mtype, load, value->request->handle_ty,
                 WN_PARM_BY_VALUE | WN_PARM_READ_ONLY |
                 WN_PARM_PASSED_NOT_SAVED);
        } else {
            TY_IDX pointer_ty = Make_Pointer_Type(value->request->handle_ty);
            WN *address = WN_CreateLda
                              (OPR_LDA, Pointer_Mtype, MTYPE_V, 0,
                               pointer_ty, value->handle_st);
            WN_kid(call, request.actual_ordinal) = WN_CreateParm
                (Pointer_Mtype, address, pointer_ty,
                 WN_PARM_BY_REFERENCE | WN_PARM_OUT |
                 WN_PARM_PASSED_NOT_SAVED);
            WN *parent = DSL_Runtime_Interface_Parent_Block(entry, call);
            TYPE_ID mtype = TY_mtype(value->request->handle_ty);
            WN *initialize = WN_CreateStid
                (OPR_STID, MTYPE_V, mtype, 0, value->handle_st,
                 value->request->handle_ty, WN_Intconst(mtype, 0));
            WN_Set_Linenum(initialize, WN_Get_Linenum(call));
            WN_INSERT_BlockBefore(parent, call, initialize);
        }
        WN_DELETE_Tree(old_parm);
        ++result->rewritten_call_count;
    }

    for (UINT32 i = 0; i < returns.size(); ++i) {
        const DSL_RUNTIME_CREATED_VALUE *target =
            DSL_Runtime_Interface_Find_Created
                (created, returns[i].result_formal->source_value_id);
        const DSL_RUNTIME_CREATED_VALUE *source =
            DSL_Runtime_Interface_Find_Created
                (created, returns[i].source_value->source_value_id);
        TYPE_ID mtype = TY_mtype(source->request->handle_ty);
        WN *load = WN_CreateLdid
                       (OPR_LDID, mtype, mtype, 0, source->handle_st,
                        source->request->handle_ty);
        WN *store = WN_CreateStid
                        (OPR_STID, MTYPE_V, mtype, 0, target->handle_st,
                         target->request->handle_ty, load);
        WN_Set_Linenum(store, WN_Get_Linenum(returns[i].store));
        WN_INSERT_BlockBefore
            (returns[i].parent_block, returns[i].store, store);
        WN_DELETE_FromBlock(returns[i].parent_block, returns[i].store);
        ++result->rewritten_return_count;
    }

    for (UINT32 i = 0; i < created.size(); ++i) {
        DSL_RUNTIME_VALUE_PROJECTION_RECORD record;
        memset(&record, 0, sizeof(record));
        record.owner_pu_st = owner_pu_st;
        record.source_value_id = created[i].request->source_value_id;
        record.source_st = created[i].request->expected_source_st;
        record.source_ty = created[i].request->expected_source_ty;
        record.handle_st = created[i].handle_st;
        record.handle_ty = created[i].request->handle_ty;
        record.binding_kind = created[i].request->binding_kind;
        record.formal_ordinal = created[i].request->formal_ordinal;
        created[i].projection_id =
            DSL_Runtime_Interface_Image_Add_Value(&record);
        ++result->value_projection_count;
        if (created[i].request->binding_kind !=
            DSL_RUNTIME_BINDING_LOCAL_VALUE)
            ++result->rebuilt_formal_count;
    }
    for (UINT32 i = 0; i < plan->call_count; ++i) {
        const DSL_RUNTIME_CALL_PROJECTION_REQUEST &request = plan->calls[i];
        if (request.owner_pu_st != owner_pu_st)
            continue;
        const DSL_RUNTIME_CREATED_VALUE *value =
            DSL_Runtime_Interface_Find_Created
                (created, request.source_value_id);
        DSL_RUNTIME_CALL_PROJECTION_RECORD record;
        memset(&record, 0, sizeof(record));
        record.owner_pu_st = owner_pu_st;
        record.callsite_id = request.callsite_id;
        record.value_projection_id = value->projection_id;
        record.source_value_id = request.source_value_id;
        record.actual_ordinal = request.actual_ordinal;
        record.callee_formal_ordinal = request.callee_formal_ordinal;
        record.direction = request.direction;
        DSL_Runtime_Interface_Image_Add_Call(&record);
        ++result->call_projection_count;
    }
    return TRUE;
}

static BOOL
DSL_Runtime_Interface_Tree_Uses_Source_ST
        (WN *tree, ST_IDX owner_pu_st)
{
    if (tree == NULL)
        return FALSE;
    if (WN_has_sym(tree)) {
        ST_IDX st = WN_st_idx(tree);
        for (UINT32 i = 1;
             i <= DSL_Runtime_Interface_Image_Value_Count(); ++i) {
            DSL_RUNTIME_VALUE_PROJECTION_RECORD record;
            if (DSL_Runtime_Interface_Image_Get_Value(i, &record) &&
                record.owner_pu_st == owner_pu_st && record.source_st == st)
                return TRUE;
        }
    }
    if (WN_operator(tree) == OPR_BLOCK) {
        for (WN *stmt = WN_first(tree); stmt != NULL; stmt = WN_next(stmt)) {
            if (DSL_Runtime_Interface_Tree_Uses_Source_ST
                    (stmt, owner_pu_st))
                return TRUE;
        }
        return FALSE;
    }
    for (UINT32 i = 0; i < WN_kid_count(tree); ++i) {
        if (DSL_Runtime_Interface_Tree_Uses_Source_ST
                (WN_kid(tree, i), owner_pu_st))
            return TRUE;
    }
    return FALSE;
}

BOOL
DSL_Runtime_Interface_Validate_PU (PU_Info *pu, FILE *diagnostic)
{
    if (pu == NULL || PU_Info_tree_ptr(pu) == NULL ||
        !DSL_IR_Image_Current_PU_Is(PU_Info_proc_sym(pu)))
        return DSL_Runtime_Interface_Report
                   (diagnostic, "program unit is not active", 0);
    ST_IDX owner_pu_st = PU_Info_proc_sym(pu);
    WN *entry = PU_Info_tree_ptr(pu);
    if (WN_operator(entry) != OPR_FUNC_ENTRY)
        return DSL_Runtime_Interface_Report
                   (diagnostic, "program unit has no FUNC_ENTRY", 0);

    UINT32 formal_count = 0;
    for (UINT32 i = 1;
         i <= DSL_Runtime_Interface_Image_Value_Count(); ++i) {
        DSL_RUNTIME_VALUE_PROJECTION_RECORD record;
        DSL_IR_VALUE_RECORD source;
        if (!DSL_Runtime_Interface_Image_Get_Value(i, &record) ||
            record.owner_pu_st != owner_pu_st)
            continue;
        if (!DSL_IR_Image_Get_Value(record.source_value_id, &source) ||
            source.st != record.source_st || source.ty != record.source_ty ||
            !DSL_Runtime_Interface_Value_Owner_Valid(owner_pu_st, source) ||
            ST_IDX_level(record.source_st) != CURRENT_SYMTAB ||
            ST_IDX_index(record.source_st) == 0 ||
            ST_IDX_index(record.source_st) >= ST_Table_Size(CURRENT_SYMTAB) ||
            ST_IDX_level(record.handle_st) != CURRENT_SYMTAB ||
            ST_IDX_index(record.handle_st) == 0 ||
            ST_IDX_index(record.handle_st) >= ST_Table_Size(CURRENT_SYMTAB) ||
            ST_type(St_Table[record.handle_st]) != record.handle_ty ||
            ST_Srcpos(St_Table[record.handle_st]) !=
                ST_Srcpos(St_Table[record.source_st]))
            return DSL_Runtime_Interface_Report
                       (diagnostic, "active value projection mismatch", i);
        if (record.binding_kind == DSL_RUNTIME_BINDING_LOCAL_VALUE) {
            if (ST_sclass(St_Table[record.handle_st]) != SCLASS_AUTO)
                return DSL_Runtime_Interface_Report
                           (diagnostic, "runtime local class mismatch", i);
            continue;
        }
        if (record.formal_ordinal >= WN_num_formals(entry) ||
            WN_st_idx(WN_formal(entry, record.formal_ordinal)) !=
                record.handle_st ||
            (record.binding_kind == DSL_RUNTIME_BINDING_INPUT_FORMAL &&
             ST_sclass(St_Table[record.handle_st]) != SCLASS_FORMAL) ||
            (record.binding_kind == DSL_RUNTIME_BINDING_RESULT_FORMAL &&
             ST_sclass(St_Table[record.handle_st]) != SCLASS_FORMAL_REF))
            return DSL_Runtime_Interface_Report
                       (diagnostic, "runtime formal mismatch", i);
        ++formal_count;
    }
    if (formal_count != WN_num_formals(entry))
        return DSL_Runtime_Interface_Report
                   (diagnostic, "incomplete runtime formal interface", 0);

    TY_IDX prototype = PU_prototype(Pu_Table[ST_pu(St_Table[owner_pu_st])]);
    if (prototype == TY_IDX_ZERO || TY_kind(prototype) != KIND_FUNCTION)
        return DSL_Runtime_Interface_Report
                   (diagnostic, "runtime prototype is missing", 0);
    TYLIST_IDX tylist = TY_tylist(prototype);
    if (TYLIST_type(Tylist_Table[tylist]) != MTYPE_To_TY(MTYPE_V))
        return DSL_Runtime_Interface_Report
                   (diagnostic, "runtime return type mismatch", 0);
    for (UINT32 ordinal = 0; ordinal < formal_count; ++ordinal) {
        DSL_PU_FORMAL_RECORD source_formal;
        DSL_RUNTIME_VALUE_PROJECTION_RECORD projection;
        if (!DSL_PU_Interface_Image_Find_Formal
                (owner_pu_st, ordinal, &source_formal) ||
            !DSL_Runtime_Interface_Image_Find_Value
                (owner_pu_st, source_formal.formal_value_id, &projection))
            return DSL_Runtime_Interface_Report
                       (diagnostic, "runtime prototype formal missing",
                        ordinal);
        TY_IDX actual = TYLIST_type(Tylist_Table[tylist + ordinal + 1]);
        BOOL type_matches = projection.binding_kind ==
                                DSL_RUNTIME_BINDING_RESULT_FORMAL ?
                            TY_kind(actual) == KIND_POINTER &&
                                TY_pointed(actual) == projection.handle_ty :
                            actual == projection.handle_ty;
        if (!type_matches)
            return DSL_Runtime_Interface_Report
                       (diagnostic, "runtime prototype formal mismatch",
                        ordinal);
    }
    if (TYLIST_type(Tylist_Table[tylist + formal_count + 1]) != TY_IDX_ZERO)
        return DSL_Runtime_Interface_Report
                   (diagnostic, "runtime prototype is not terminated", 0);

    for (UINT32 i = 1;
         i <= DSL_Runtime_Interface_Image_Call_Count(); ++i) {
        DSL_RUNTIME_CALL_PROJECTION_RECORD record;
        if (!DSL_Runtime_Interface_Image_Get_Call(i, &record) ||
            record.owner_pu_st != owner_pu_st)
            continue;
        const WN *call = DSL_Call_Image_Get_Call_WN(record.callsite_id);
        DSL_RUNTIME_VALUE_PROJECTION_RECORD value;
        if (call == NULL || record.actual_ordinal >= WN_kid_count(call) ||
            record.actual_ordinal != record.callee_formal_ordinal ||
            !DSL_Runtime_Interface_Image_Get_Value
                (record.value_projection_id, &value))
            return DSL_Runtime_Interface_Report
                       (diagnostic, "runtime call is unavailable", i);
        WN *parm = WN_kid(call, record.actual_ordinal);
        WN *actual = parm == NULL || WN_operator(parm) != OPR_PARM ?
                     NULL : WN_kid0(parm);
        if (record.direction == DSL_RUNTIME_CALL_INPUT) {
            if (actual == NULL || WN_operator(actual) != OPR_LDID ||
                WN_st_idx(actual) != value.handle_st ||
                WN_ty(parm) != value.handle_ty ||
                WN_ty(actual) != value.handle_ty ||
                WN_parm_flag(parm) !=
                    (WN_PARM_BY_VALUE | WN_PARM_READ_ONLY |
                     WN_PARM_PASSED_NOT_SAVED))
                return DSL_Runtime_Interface_Report
                           (diagnostic, "runtime input call mismatch", i);
        } else {
            if (actual == NULL || WN_operator(actual) != OPR_LDA ||
                WN_st_idx(actual) != value.handle_st ||
                WN_ty(parm) != WN_ty(actual) ||
                TY_kind(WN_ty(parm)) != KIND_POINTER ||
                TY_pointed(WN_ty(parm)) != value.handle_ty ||
                WN_parm_flag(parm) !=
                    (WN_PARM_BY_REFERENCE | WN_PARM_OUT |
                     WN_PARM_PASSED_NOT_SAVED))
                return DSL_Runtime_Interface_Report
                           (diagnostic, "runtime result call mismatch", i);
        }
    }

    if (DSL_Runtime_Interface_Tree_Uses_Source_ST(entry, owner_pu_st))
        return DSL_Runtime_Interface_Report
                   (diagnostic, "executable tree still uses tensor source", 0);
    return TRUE;
}

static BOOL
DSL_IR_Image_PU_ST_Valid (ST_IDX st)
{
    return ST_IDX_level(st) == GLOBAL_SYMTAB && ST_IDX_index(st) != 0 &&
           ST_IDX_index(st) < ST_Table_Size(GLOBAL_SYMTAB) &&
           ST_class(st) == CLASS_FUNC && ST_pu(St_Table[st]) != PU_IDX_ZERO &&
           ST_pu(St_Table[st]) < PU_Table_Size();
}

static BOOL
DSL_IR_Image_Current_PU_Is (ST_IDX owner_pu_st)
{
    return DSL_IR_Image_PU_ST_Valid(owner_pu_st) && Current_pu != NULL &&
           Current_pu == &Pu_Table[ST_pu(St_Table[owner_pu_st])];
}

BOOL
DSL_Runtime_Interface_Value_Record_Contract_Valid
        (const DSL_RUNTIME_VALUE_PROJECTION_RECORD *record)
{
    DSL_IR_VALUE_RECORD source;
    return record != NULL &&
           DSL_IR_Image_PU_ST_Valid(record->owner_pu_st) &&
           record->source_value_id != DSL_IR_VALUE_INVALID_ID &&
           DSL_IR_Image_Get_Value(record->source_value_id, &source) &&
           source.st == record->source_st && source.ty == record->source_ty &&
           DSL_Runtime_Interface_Value_Owner_Valid
               (record->owner_pu_st, source) &&
           TY_is_tensor_extension(record->source_ty) &&
           ST_IDX_level(record->source_st) > GLOBAL_SYMTAB &&
           ST_IDX_index(record->source_st) != 0 &&
           ST_IDX_level(record->handle_st) > GLOBAL_SYMTAB &&
           ST_IDX_index(record->handle_st) != 0 &&
           TY_IDX_index(record->handle_ty) != 0 &&
           TY_IDX_index(record->handle_ty) < TY_Table_Size() &&
           TY_kind(record->handle_ty) == KIND_POINTER;
}

BOOL
DSL_Program_Interface_TY_Contract_Valid (TY_IDX ty)
{
    return ty != TY_IDX_ZERO && TY_IDX_index(ty) < TY_Table_Size();
}

BOOL
DSL_Program_Interface_Pointer_TY_Contract_Valid (TY_IDX ty)
{
    return DSL_Program_Interface_TY_Contract_Valid(ty) &&
           TY_kind(ty) == KIND_POINTER;
}

BOOL
DSL_Program_Interface_Tensor_TY_Contract_Valid (TY_IDX ty)
{
    return DSL_Program_Interface_TY_Contract_Valid(ty) &&
           TY_is_tensor_extension(ty);
}

BOOL
DSL_Program_Interface_TCON_Contract_Valid (TCON_IDX tcon)
{
    return tcon != TCON_IDX_ZERO && tcon < TCON_Table_Size();
}

static BOOL
DSL_Call_ABI_PU_Report (FILE *diagnostic, const char *message, UINT32 id)
{
    if (diagnostic != NULL)
        fprintf(diagnostic, "DSL call ABI PU error: %s id=%u\n", message, id);
    return FALSE;
}

static BOOL
DSL_Call_ABI_Value_Matches_ST
        (const DSL_IR_VALUE_RECORD &value, ST_IDX owner_pu_st, ST_IDX st)
{
    if (ST_IDX_level(st) != CURRENT_SYMTAB || ST_IDX_index(st) == 0 ||
        ST_IDX_index(st) >= ST_Table_Size(CURRENT_SYMTAB) ||
        value.st != st || value.ty != ST_type(St_Table[st]) ||
        value.metadata == STR_IDX_ZERO)
        return FALSE;
    std::string owner = "owner_pu=";
    owner += ST_name(St_Table[owner_pu_st]);
    return owner == Index_To_Str(value.metadata);
}

BOOL
DSL_Call_ABI_Image_Validate_PU (PU_Info *pu, FILE *diagnostic)
{
    if (pu == NULL || PU_Info_tree_ptr(pu) == NULL ||
        ST_IDX_index(PU_Info_proc_sym(pu)) == 0 || Current_pu == NULL ||
        Current_pu != &Pu_Table[ST_pu(St_Table[PU_Info_proc_sym(pu)])])
        return DSL_Call_ABI_PU_Report(diagnostic, "missing program unit", 0);
    ST_IDX owner_pu_st = PU_Info_proc_sym(pu);
    WN *entry = PU_Info_tree_ptr(pu);

    for (UINT32 i = 1; i <= DSL_Call_ABI_Image_Argument_Count(); ++i) {
        DSL_CALL_ARGUMENT_RECORD argument;
        DSL_CALLSITE_METADATA_RECORD callsite;
        if (!DSL_Call_ABI_Image_Get_Argument(i, &argument) ||
            !DSL_Call_Image_Get_Callsite(argument.callsite_id, &callsite))
            return DSL_Call_ABI_PU_Report
                       (diagnostic, "missing relationship", i);

        DSL_RETIRED_CALL_ARGUMENT_RECORD retired_argument;
        BOOL retired = DSL_Program_Interface_Image_Find_Retired_Call
                           (argument.id, &retired_argument);
        if (retired)
            continue;

        if (callsite.owner_pu_st == owner_pu_st) {
            const WN *call = DSL_Call_Image_Get_Call_WN(callsite.id);
            UINT32 effective_ordinal = argument.actual_ordinal;
            for (UINT32 retired_id = 1;
                 retired_id <=
                     DSL_Program_Interface_Image_Retired_Call_Count();
                 ++retired_id) {
                DSL_RETIRED_CALL_ARGUMENT_RECORD retired_call;
                if (DSL_Program_Interface_Image_Get_Retired_Call
                        (retired_id, &retired_call) &&
                    retired_call.callsite_id == callsite.id &&
                    retired_call.old_actual_ordinal <
                        argument.actual_ordinal)
                    --effective_ordinal;
            }
            if (call == NULL || effective_ordinal >= WN_kid_count(call))
                return DSL_Call_ABI_PU_Report
                           (diagnostic, "actual ordinal out of range",
                            argument.id);
            DSL_RUNTIME_CALL_PROJECTION_RECORD runtime_call;
            if (DSL_Runtime_Interface_Image_Find_Call
                    (callsite.id, argument.actual_ordinal, &runtime_call)) {
                DSL_RUNTIME_VALUE_PROJECTION_RECORD runtime_value;
                const WN *parm = WN_kid(call, effective_ordinal);
                const WN *actual = parm == NULL ||
                                   WN_operator(parm) != OPR_PARM ?
                                   NULL : WN_kid0(parm);
                if (runtime_call.direction != DSL_RUNTIME_CALL_INPUT ||
                    !DSL_Runtime_Interface_Image_Get_Value
                        (runtime_call.value_projection_id, &runtime_value) ||
                    actual == NULL || WN_operator(actual) != OPR_LDID ||
                    WN_st_idx(actual) != runtime_value.handle_st ||
                    WN_ty(parm) != runtime_value.handle_ty ||
                    !WN_Parm_By_Value(parm))
                    return DSL_Call_ABI_PU_Report
                               (diagnostic, "runtime argument mismatch",
                                argument.id);
                continue;
            }
            const WN *parm = WN_kid(call, effective_ordinal);
            const WN *address = parm == NULL || WN_operator(parm) != OPR_PARM ?
                                NULL : WN_kid0(parm);
            DSL_IR_VALUE_RECORD value;
            if (WN_st_idx(call) != callsite.callee_pu_st ||
                address == NULL || WN_operator(address) != OPR_LDA ||
                !WN_Parm_By_Reference(parm) || !WN_Parm_Read_Only(parm) ||
                WN_Parm_Out(parm) || !WN_Parm_Passed_Not_Saved(parm) ||
                !DSL_IR_Image_Get_Value(argument.argument_value_id, &value) ||
                !DSL_Call_ABI_Value_Matches_ST
                    (value, owner_pu_st, WN_st_idx(address)) ||
                WN_ty(parm) != WN_ty(address) ||
                TY_kind(WN_ty(parm)) != KIND_POINTER ||
                TY_pointed(WN_ty(parm)) != value.ty)
                return DSL_Call_ABI_PU_Report
                           (diagnostic, "argument value mismatch", argument.id);
        }

        if (callsite.callee_pu_st == owner_pu_st) {
            UINT32 effective_ordinal = argument.callee_formal_ordinal;
            for (UINT32 retired_id = 1;
                 retired_id <=
                     DSL_Program_Interface_Image_Retired_Formal_Count();
                 ++retired_id) {
                DSL_RETIRED_FORMAL_RECORD retired_formal;
                if (DSL_Program_Interface_Image_Get_Retired_Formal
                        (retired_id, &retired_formal) &&
                    retired_formal.owner_pu_st == owner_pu_st &&
                    retired_formal.old_formal_ordinal <
                        argument.callee_formal_ordinal)
                    --effective_ordinal;
            }
            if (entry == NULL || WN_operator(entry) != OPR_FUNC_ENTRY ||
                effective_ordinal >= WN_num_formals(entry))
                return DSL_Call_ABI_PU_Report
                           (diagnostic, "formal ordinal out of range",
                            argument.id);
            ST_IDX formal_st =
                WN_st_idx(WN_formal(entry, effective_ordinal));
            DSL_IR_VALUE_RECORD value;
            DSL_PU_FORMAL_RECORD formal;
            DSL_RUNTIME_VALUE_PROJECTION_RECORD runtime_formal;
            BOOL projected = FALSE;
            if (!DSL_IR_Image_Get_Value(argument.argument_value_id, &value) ||
                (DSL_PU_Interface_Image_Has_Records() &&
                 !DSL_PU_Interface_Image_Find_Formal
                      (owner_pu_st, argument.callee_formal_ordinal, &formal)))
                return DSL_Call_ABI_PU_Report
                           (diagnostic, "actual/formal type mismatch",
                            argument.id);
            projected = DSL_PU_Interface_Image_Has_Records() &&
                DSL_Runtime_Interface_Image_Find_Value
                    (owner_pu_st, formal.formal_value_id, &runtime_formal);
            if ((!projected &&
                 (formal.formal_st != formal_st ||
                  formal.formal_ty != value.ty)) ||
                (projected &&
                 (runtime_formal.handle_st != formal_st ||
                  runtime_formal.handle_ty != ST_type(St_Table[formal_st]))))
                return DSL_Call_ABI_PU_Report
                           (diagnostic, "actual/formal type mismatch",
                            argument.id);
        }
    }
    return TRUE;
}

static BOOL
DSL_PU_Interface_PU_Report
        (FILE *diagnostic, const char *message, UINT32 id)
{
    if (diagnostic != NULL)
        fprintf(diagnostic, "DSL PU interface error: %s id=%u\n",
                message, id);
    return FALSE;
}

BOOL
DSL_PU_Interface_Image_Validate (FILE *diagnostic)
{
    for (UINT32 i = 1; i <= DSL_PU_Interface_Image_Formal_Count(); ++i) {
        DSL_PU_FORMAL_RECORD record;
        DSL_IR_VALUE_RECORD value;
        if (!DSL_PU_Interface_Image_Get_Formal(i, &record) ||
            record.id != i ||
            !DSL_IR_Image_PU_ST_Valid(record.owner_pu_st) ||
            record.formal_value_id == DSL_IR_VALUE_INVALID_ID ||
            !DSL_IR_Image_Get_Value(record.formal_value_id, &value) ||
            record.formal_ordinal == DSL_PU_FORMAL_INVALID_ORDINAL ||
            ST_IDX_level(record.formal_st) <= GLOBAL_SYMTAB ||
            ST_IDX_index(record.formal_st) == 0 ||
            TY_IDX_index(record.formal_ty) == 0 ||
            record.flags != 0 || record.reserved != 0 ||
            value.value_kind != DSL_IR_VALUE_SYMBOL ||
            value.producer_node_id != DSL_IR_NODE_INVALID_ID ||
            value.st != record.formal_st || value.ty != record.formal_ty)
            return DSL_PU_Interface_PU_Report
                       (diagnostic, "invalid image formal", i);
        for (UINT32 j = 1; j < i; ++j) {
            DSL_PU_FORMAL_RECORD previous;
            if (!DSL_PU_Interface_Image_Get_Formal(j, &previous))
                return DSL_PU_Interface_PU_Report
                           (diagnostic, "missing image formal", j);
            if (previous.owner_pu_st == record.owner_pu_st &&
                (previous.formal_ordinal == record.formal_ordinal ||
                 previous.formal_value_id == record.formal_value_id ||
                 previous.formal_st == record.formal_st))
                return DSL_PU_Interface_PU_Report
                           (diagnostic, "duplicate image formal", i);
        }
    }
    return TRUE;
}

BOOL
DSL_PU_Interface_Image_Validate_PU (PU_Info *pu, FILE *diagnostic)
{
    if (!DSL_PU_Interface_Image_Has_Records())
        return TRUE;
    if (pu == NULL || PU_Info_tree_ptr(pu) == NULL ||
        ST_IDX_index(PU_Info_proc_sym(pu)) == 0 ||
        !DSL_IR_Image_Current_PU_Is(PU_Info_proc_sym(pu)))
        return DSL_PU_Interface_PU_Report
                   (diagnostic, "missing program unit", 0);

    ST_IDX owner_pu_st = PU_Info_proc_sym(pu);
    WN *entry = PU_Info_tree_ptr(pu);
    UINT32 expected_ordinal = 0;
    if (WN_operator(entry) != OPR_FUNC_ENTRY)
        return DSL_PU_Interface_PU_Report
                   (diagnostic, "invalid function entry", 0);

    for (UINT32 i = 1; i <= DSL_PU_Interface_Image_Formal_Count(); ++i) {
        DSL_PU_FORMAL_RECORD formal;
        if (!DSL_PU_Interface_Image_Get_Formal(i, &formal))
            return DSL_PU_Interface_PU_Report
                       (diagnostic, "missing formal", i);
        if (formal.owner_pu_st != owner_pu_st)
            continue;
        DSL_RETIRED_FORMAL_RECORD retired;
        if (DSL_Program_Interface_Image_Find_Retired_Formal
                (formal.id, &retired))
            continue;
        if (expected_ordinal >= WN_num_formals(entry))
            return DSL_PU_Interface_PU_Report
                       (diagnostic, "formal ordinal mismatch", formal.id);
        WN *idname = WN_formal(entry, expected_ordinal);
        DSL_IR_VALUE_RECORD value;
        DSL_RUNTIME_VALUE_PROJECTION_RECORD runtime_formal;
        BOOL projected = DSL_Runtime_Interface_Image_Find_Value
                             (owner_pu_st, formal.formal_value_id,
                              &runtime_formal);
        if (idname == NULL || WN_operator(idname) != OPR_IDNAME ||
            WN_st_idx(idname) !=
                (projected ? runtime_formal.handle_st : formal.formal_st) ||
            ST_IDX_level(WN_st_idx(idname)) != CURRENT_SYMTAB ||
            ST_IDX_index(WN_st_idx(idname)) >=
                ST_Table_Size(CURRENT_SYMTAB) ||
            (ST_sclass(St_Table[WN_st_idx(idname)]) != SCLASS_FORMAL &&
             ST_sclass(St_Table[WN_st_idx(idname)]) != SCLASS_FORMAL_REF) ||
            ST_type(St_Table[WN_st_idx(idname)]) !=
                (projected ? runtime_formal.handle_ty : formal.formal_ty) ||
            !DSL_IR_Image_Get_Value(formal.formal_value_id, &value) ||
            (!projected && !DSL_Call_ABI_Value_Matches_ST
                 (value, owner_pu_st, formal.formal_st)))
            return DSL_PU_Interface_PU_Report
                       (diagnostic, "formal value mismatch", formal.id);
        ++expected_ordinal;
    }
    UINT32 binding_count = 0;
    for (UINT32 i = 1;
         i <= DSL_Program_Interface_Image_Runtime_Binding_Count(); ++i) {
        DSL_RUNTIME_INPUT_BINDING_RECORD binding;
        if (DSL_Program_Interface_Image_Get_Runtime_Binding(i, &binding) &&
            binding.owner_pu_st == owner_pu_st)
            ++binding_count;
    }
    if (expected_ordinal + binding_count != WN_num_formals(entry))
        return DSL_PU_Interface_PU_Report
                   (diagnostic, "incomplete formal interface", 0);
    return TRUE;
}

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

static BOOL
DSL_IR_Image_Value_Belongs_To_PU
        (const DSL_IR_VALUE_RECORD &value, ST_IDX owner_pu_st)
{
    DSL_IR_VALUE_RECORD owned_value;

    return DSL_IR_Image_PU_ST_Valid(owner_pu_st) &&
           value.name != STR_IDX_ZERO &&
           DSL_IR_Image_Find_PU_Value
               (value.st, Index_To_Str(value.name),
                ST_name(St_Table[owner_pu_st]), &owned_value) &&
           owned_value.id == value.id;
}

static BOOL
DSL_IR_Parse_Unsigned (const char *text, UINT64 *value)
{
    char *end;
    unsigned long long parsed;

    if (text == NULL || text[0] == '\0' || value == NULL || text[0] == '-')
        return FALSE;
    errno = 0;
    parsed = strtoull(text, &end, 10);
    if (errno == ERANGE || end == text || *end != '\0')
        return FALSE;
    *value = (UINT64)parsed;
    return TRUE;
}

static BOOL
DSL_IR_Checksum_Valid (const char *checksum)
{
    if (checksum == NULL || checksum[0] == '\0')
        return TRUE;
    if (strlen(checksum) != 64)
        return FALSE;
    for (UINT32 i = 0; i < 64; ++i) {
        if (!isxdigit((unsigned char)checksum[i]))
            return FALSE;
    }
    return TRUE;
}

static UINT64
DSL_IR_Tensor_Element_Size (const char *dtype)
{
    if (dtype == NULL)
        return 0;
    if (strcmp(dtype, "bool") == 0 || strcmp(dtype, "int8") == 0 ||
        strcmp(dtype, "uint8") == 0)
        return 1;
    if (strcmp(dtype, "float16") == 0 || strcmp(dtype, "bfloat16") == 0 ||
        strcmp(dtype, "int16") == 0 || strcmp(dtype, "uint16") == 0)
        return 2;
    if (strcmp(dtype, "float32") == 0 || strcmp(dtype, "int32") == 0 ||
        strcmp(dtype, "uint32") == 0)
        return 4;
    if (strcmp(dtype, "float64") == 0 || strcmp(dtype, "int64") == 0 ||
        strcmp(dtype, "uint64") == 0)
        return 8;
    return 0;
}

static BOOL
DSL_IR_Static_Tensor_Byte_Size
        (const TENSOR_DESCRIPTOR_RECORD &descriptor,
         UINT64 *element_size,
         UINT64 *byte_size)
{
    const char *dtype = descriptor.dtype == STR_IDX_ZERO ? NULL :
                        Index_To_Str(descriptor.dtype);
    const char *shape = descriptor.logical_shape == STR_IDX_ZERO ? NULL :
                        Index_To_Str(descriptor.logical_shape);
    UINT64 item_size = DSL_IR_Tensor_Element_Size(dtype);
    if (item_size == 0 || descriptor.rank < 0 || shape == NULL ||
        shape[0] != '[')
        return FALSE;

    const char *cursor = shape + 1;
    UINT64 elements = 1;
    INT32 dimension_count = 0;
    while (TRUE) {
        while (isspace((unsigned char)*cursor))
            ++cursor;
        if (*cursor == ']') {
            ++cursor;
            break;
        }
        if (!isdigit((unsigned char)*cursor))
            return FALSE;
        UINT64 dimension = 0;
        while (isdigit((unsigned char)*cursor)) {
            UINT64 digit = (UINT64)(*cursor - '0');
            if (dimension > (~(UINT64)0 - digit) / 10)
                return FALSE;
            dimension = dimension * 10 + digit;
            ++cursor;
        }
        if (dimension == 0 || elements > ~(UINT64)0 / dimension)
            return FALSE;
        elements *= dimension;
        ++dimension_count;
        while (isspace((unsigned char)*cursor))
            ++cursor;
        if (*cursor == ',') {
            ++cursor;
            continue;
        }
        if (*cursor != ']')
            return FALSE;
    }
    while (isspace((unsigned char)*cursor))
        ++cursor;
    if (*cursor != '\0' || dimension_count != descriptor.rank ||
        elements > ~(UINT64)0 / item_size)
        return FALSE;
    if (element_size != NULL)
        *element_size = item_size;
    if (byte_size != NULL)
        *byte_size = elements * item_size;
    return TRUE;
}

static BOOL
DSL_IR_Node_Attribute
        (const DSL_IR_NODE_RECORD &node,
         const char *name,
         const char **value)
{
    if (name == NULL || value == NULL)
        return FALSE;
    for (UINT32 i = 0; i < node.attribute_count; ++i) {
        DSL_IR_ATTRIBUTE_RECORD attribute;
        if (!DSL_IR_Image_Get_Attribute
                 (node.first_attribute_id + i, &attribute) ||
            attribute.name == STR_IDX_ZERO)
            return FALSE;
        if (strcmp(Index_To_Str(attribute.name), name) == 0) {
            *value = attribute.value == STR_IDX_ZERO ? "" :
                     Index_To_Str(attribute.value);
            return TRUE;
        }
    }
    return FALSE;
}

BOOL
DSL_IR_Image_Get_External_Tensor_Reference
        (ST_IDX owner_pu_st,
         DSL_IR_VALUE_ID value_id,
         DSL_IR_EXTERNAL_TENSOR_REFERENCE *reference)
{
    DSL_IR_VALUE_RECORD value;
    DSL_IR_NODE_RECORD node;
    DSL_IR_OPCODE_DESCRIPTOR_RECORD opcode;
    TENSOR_DESCRIPTOR_RECORD descriptor;
    const char *value_kind;
    const char *uri;
    const char *format;
    const char *file;
    const char *key;
    const char *offset_text;
    const char *length_text;
    const char *checksum;
    const char *tensor_tcon_text;
    const char *tensor_tcon_path;
    UINT64 offset;
    UINT64 length;
    UINT64 element_size;
    UINT64 tensor_size;
    UINT64 tensor_tcon_value = 0;
    UINT32 tensor_tcon_path_length = 0;
    DSL_TENSOR_TCON_RECORD tensor_tcon;

    if (reference == NULL)
        return FALSE;
    memset(reference, 0, sizeof(*reference));
    if (!DSL_IR_Image_Current_PU_Is(owner_pu_st) ||
        !DSL_IR_Image_Get_Value(value_id, &value) ||
        value.value_kind != DSL_IR_VALUE_CONSTANT ||
        !DSL_IR_Image_Value_Belongs_To_PU(value, owner_pu_st) ||
        ST_IDX_level(value.st) != CURRENT_SYMTAB ||
        ST_IDX_index(value.st) == 0 ||
        ST_IDX_index(value.st) >= ST_Table_Size(CURRENT_SYMTAB) ||
        ST_type(St_Table[value.st]) != value.ty ||
        !DSL_IR_Image_Get_Node(value.producer_node_id, &node) ||
        !DSL_IR_Image_Get_Opcode_Descriptor
             (node.opcode_descriptor_id, &opcode) ||
        opcode.logical_operator != OPR_DSLTENSORCONST ||
        opcode.version != 1 || node.result_value_id != value.id ||
        !DSL_IR_Node_Attribute(node, "value_kind", &value_kind) ||
        strcmp(value_kind, "external_data") != 0 ||
        !DSL_IR_Node_Attribute(node, "value", &uri) ||
        !TY_get_tensor_descriptor_record(value.ty, &descriptor))
        return FALSE;

    format = ST_tensor_metadata(value.st, "storage_format");
    file = ST_tensor_metadata(value.st, "storage_file");
    key = ST_tensor_metadata(value.st, "storage_tensor_key");
    offset_text = ST_tensor_metadata(value.st, "storage_byte_offset");
    length_text = ST_tensor_metadata(value.st, "storage_byte_length");
    checksum = ST_tensor_metadata(value.st, "storage_checksum");
    tensor_tcon_text = ST_tensor_metadata(value.st, "tensor_tcon_idx");
    if (format == NULL || format[0] == '\0' || file == NULL ||
        file[0] == '\0' || key == NULL || key[0] == '\0' ||
        !DSL_IR_Parse_Unsigned(offset_text, &offset) ||
        !DSL_IR_Parse_Unsigned(length_text, &length) || length == 0 ||
        offset + length < offset || !DSL_IR_Checksum_Valid(checksum) ||
        !DSL_IR_Static_Tensor_Byte_Size
             (descriptor, &element_size, &tensor_size) ||
        offset % element_size != 0 || length != tensor_size)
        return FALSE;

    const char *placement = descriptor.placement == STR_IDX_ZERO ? NULL :
                            Index_To_Str(descriptor.placement);
    const char *memory = descriptor.memory == STR_IDX_ZERO ? NULL :
                         Index_To_Str(descriptor.memory);
    if (descriptor.layout == STR_IDX_ZERO || placement == NULL ||
        strcmp(placement, "side_file") != 0 || memory == NULL ||
        strcmp(memory, "external_data") != 0)
        return FALSE;

    if (tensor_tcon_text != NULL && tensor_tcon_text[0] != '\0') {
        if (!DSL_IR_Parse_Unsigned(tensor_tcon_text, &tensor_tcon_value) ||
            tensor_tcon_value == 0 ||
            tensor_tcon_value > (UINT64)(~(TCON_IDX)0) ||
            !DSL_Tensor_TCON_Get
                 ((TCON_IDX)tensor_tcon_value, &tensor_tcon) ||
            tensor_tcon.storage_kind !=
                DSL_TENSOR_TCON_STORAGE_SIDE_FILE_DENSE ||
            tensor_tcon.descriptor_ty != value.ty ||
            tensor_tcon.element_mtype != TY_mtype(descriptor.element_ty) ||
            tensor_tcon.element_size != element_size ||
            tensor_tcon.element_count != tensor_size / element_size ||
            tensor_tcon.logical_bytes != tensor_size ||
            tensor_tcon.required_alignment < TY_align(value.ty) ||
            tensor_tcon.byte_offset != offset ||
            tensor_tcon.byte_length != length ||
            !DSL_Tensor_TCON_Get_Side_Path
                 ((TCON_IDX)tensor_tcon_value, &tensor_tcon_path,
                  &tensor_tcon_path_length) ||
            strlen(file) != tensor_tcon_path_length ||
            memcmp(file, tensor_tcon_path, tensor_tcon_path_length) != 0)
            return FALSE;
    }

    size_t uri_size = strlen(format) + strlen(file) + strlen(key) +
                      strlen(checksum == NULL ? "" : checksum) + 96;
    char *expected = new char[uri_size];
    snprintf(expected, uri_size,
             "%s://%s#%s?offset=%llu&length=%llu&checksum=%s",
             format, file, key, (unsigned long long)offset,
             (unsigned long long)length, checksum == NULL ? "" : checksum);
    BOOL matches = strcmp(uri, expected) == 0;
    delete [] expected;
    if (!matches)
        return FALSE;

    reference->value_id = value.id;
    reference->producer_node_id = value.producer_node_id;
    reference->descriptor_ty = value.ty;
    reference->st = value.st;
    reference->tensor_tcon = (TCON_IDX)tensor_tcon_value;
    reference->element_ty = descriptor.element_ty;
    reference->rank = descriptor.rank;
    reference->storage_format = format;
    reference->side_file = file;
    reference->tensor_key = key;
    reference->byte_offset = offset;
    reference->byte_length = length;
    reference->checksum = checksum == NULL ? "" : checksum;
    reference->dtype = Index_To_Str(descriptor.dtype);
    reference->logical_shape = Index_To_Str(descriptor.logical_shape);
    reference->layout = Index_To_Str(descriptor.layout);
    return TRUE;
}

BOOL
DSL_Program_Interface_Runtime_Input_Contract_Valid
        (const DSL_RUNTIME_INPUT_RECORD *record)
{
    if (record == NULL ||
        !DSL_Program_Interface_Pointer_TY_Contract_Valid(record->handle_ty))
        return FALSE;
    if (record->input_kind == DSL_RUNTIME_INPUT_OPAQUE_RESOURCE)
        return record->source_owner_pu_st == ST_IDX_ZERO &&
               record->source_value_id == DSL_IR_VALUE_INVALID_ID &&
               record->source_st == ST_IDX_ZERO &&
               record->source_ty == TY_IDX_ZERO &&
               record->source_tcon == TCON_IDX_ZERO;

    DSL_TENSOR_TCON_RECORD tensor_tcon;
    if (!DSL_Tensor_TCON_Get(record->source_tcon, &tensor_tcon) ||
        tensor_tcon.descriptor_ty != record->source_ty)
        return FALSE;
    if (record->input_kind == DSL_RUNTIME_INPUT_TENSOR_TCON_RESOURCE)
        return record->source_owner_pu_st == ST_IDX_ZERO &&
               record->source_value_id == DSL_IR_VALUE_INVALID_ID &&
               record->source_st == ST_IDX_ZERO &&
               DSL_Program_Interface_Tensor_TY_Contract_Valid
                   (record->source_ty);
    if (record->input_kind != DSL_RUNTIME_INPUT_SOURCE_EXTERNAL_TENSOR ||
        !DSL_IR_Image_PU_ST_Valid(record->source_owner_pu_st) ||
        record->source_value_id == DSL_IR_VALUE_INVALID_ID ||
        record->source_st == ST_IDX_ZERO)
        return FALSE;

    DSL_IR_VALUE_RECORD value;
    DSL_IR_NODE_RECORD node;
    DSL_IR_OPCODE_DESCRIPTOR_RECORD opcode;
    const char *value_kind = NULL;
    if (!DSL_IR_Image_Get_Value(record->source_value_id, &value) ||
        value.st != record->source_st || value.ty != record->source_ty ||
        value.value_kind != DSL_IR_VALUE_CONSTANT ||
        !DSL_IR_Image_Value_Belongs_To_PU
             (value, record->source_owner_pu_st) ||
        !DSL_IR_Image_Get_Node(value.producer_node_id, &node) ||
        node.result_value_id != value.id ||
        !DSL_IR_Image_Get_Opcode_Descriptor
             (node.opcode_descriptor_id, &opcode) ||
        opcode.logical_operator != OPR_DSLTENSORCONST ||
        opcode.version != 1 || opcode.effect_model != DSL_EFFECT_MODEL_PURE ||
        !DSL_IR_Node_Attribute(node, "value_kind", &value_kind) ||
        strcmp(value_kind, "external_data") != 0)
        return FALSE;

    if (DSL_IR_Image_Current_PU_Is(record->source_owner_pu_st)) {
        DSL_IR_EXTERNAL_TENSOR_REFERENCE reference;
        if (!DSL_IR_Image_Get_External_Tensor_Reference
                 (record->source_owner_pu_st, record->source_value_id,
                  &reference) ||
            reference.descriptor_ty != record->source_ty ||
            reference.st != record->source_st ||
            reference.tensor_tcon != record->source_tcon)
            return FALSE;
    }
    return TRUE;
}

static BOOL
DSL_IR_Block_Contains (const WN *block, const WN *statement)
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

static BOOL
DSL_IR_Value_From_Call_Actual
        (ST_IDX owner_pu_st,
         const WN *call,
         UINT32 ordinal,
         DSL_IR_VALUE_RECORD *value)
{
    DSL_CALLSITE_METADATA_RECORD callsite;
    if (call == NULL || WN_operator(call) != OPR_CALL ||
        ordinal >= (UINT32)WN_kid_count(call) ||
        !DSL_Call_Image_Find_Callsite(call, &callsite) ||
        callsite.owner_pu_st != owner_pu_st)
        return FALSE;

    const WN *parm = WN_kid(call, ordinal);
    const WN *address = parm == NULL || WN_operator(parm) != OPR_PARM ?
                        NULL : WN_kid0(parm);
    ST_IDX actual_st = address == NULL ? ST_IDX_ZERO : WN_st_idx(address);
    if (address == NULL || !WN_Parm_By_Reference(parm) ||
        !WN_Parm_Read_Only(parm) || !WN_Parm_Passed_Not_Saved(parm) ||
        WN_operator(address) != OPR_LDA ||
        ST_IDX_level(actual_st) != CURRENT_SYMTAB ||
        ST_IDX_index(actual_st) == 0 ||
        ST_IDX_index(actual_st) >= ST_Table_Size(CURRENT_SYMTAB))
        return FALSE;
    return DSL_IR_Image_Find_PU_Value
               (actual_st, ST_name(St_Table[actual_st]),
                ST_name(St_Table[owner_pu_st]), value);
}

static BOOL
DSL_IR_PU_Value_Name_Exists (ST_IDX owner_pu_st, const char *name)
{
    std::string owner = "owner_pu=";
    owner += ST_name(St_Table[owner_pu_st]);
    for (DSL_IR_VALUE_ID id = 1; id <= DSL_IR_Image_Value_Count(); ++id) {
        DSL_IR_VALUE_RECORD value;
        if (!DSL_IR_Image_Get_Value(id, &value) ||
            value.name == STR_IDX_ZERO || value.metadata == STR_IDX_ZERO)
            continue;
        if (strcmp(Index_To_Str(value.name), name) == 0 &&
            owner == Index_To_Str(value.metadata))
            return TRUE;
    }
    return FALSE;
}

static BOOL
DSL_IR_External_Tensor_Request_Valid
        (ST_IDX owner_pu_st,
         const DSL_IR_EXTERNAL_TENSOR_MATERIALIZATION_REQUEST &request,
         DSL_IR_VALUE_RECORD *source_value,
         std::string *uri,
         std::string *payload)
{
    TENSOR_DESCRIPTOR_RECORD descriptor;
    DSL_TENSOR_TCON_RECORD tensor_tcon;
    DSL_IR_EXTERNAL_TENSOR_REFERENCE source_reference;
    DSL_IR_VALUE_RECORD actual_value;
    const char *side_path;
    UINT32 side_path_length;
    UINT64 element_size;
    UINT64 tensor_size;

    if (source_value != NULL)
        memset(source_value, 0, sizeof(*source_value));
    if (request.name == NULL || request.name[0] == '\0' ||
        request.descriptor_ty == TY_IDX_ZERO ||
        request.tensor_tcon == TCON_IDX_ZERO ||
        request.source_value_id == DSL_IR_VALUE_INVALID_ID ||
        request.insertion_block == NULL || request.insert_before == NULL ||
        !DSL_IR_Block_Contains
             (request.insertion_block, request.insert_before) ||
        request.source_position == 0 ||
        request.storage_format == NULL ||
        request.storage_format[0] == '\0' || request.side_file == NULL ||
        request.side_file[0] == '\0' || request.tensor_key == NULL ||
        request.tensor_key[0] == '\0' || request.byte_length == 0 ||
        request.byte_offset + request.byte_length < request.byte_offset ||
        request.checksum == NULL || request.checksum[0] == '\0' ||
        !DSL_IR_Checksum_Valid(request.checksum) ||
        (request.source_policy != DSL_IR_MATERIALIZE_SOURCE_EXTERNAL_ONLY &&
         request.source_policy !=
             DSL_IR_MATERIALIZE_SOURCE_EXTERNAL_OR_IMPLICIT_ZERO) ||
        !TY_get_tensor_descriptor_record(request.descriptor_ty, &descriptor) ||
        !DSL_IR_Static_Tensor_Byte_Size
             (descriptor, &element_size, &tensor_size) ||
        request.byte_offset % element_size != 0 ||
        request.byte_length != tensor_size ||
        !DSL_Tensor_TCON_Get(request.tensor_tcon, &tensor_tcon) ||
        tensor_tcon.storage_kind != DSL_TENSOR_TCON_STORAGE_SIDE_FILE_DENSE ||
        tensor_tcon.descriptor_ty != request.descriptor_ty ||
        tensor_tcon.element_mtype != TY_mtype(descriptor.element_ty) ||
        tensor_tcon.element_size != element_size ||
        tensor_tcon.logical_bytes != tensor_size ||
        tensor_tcon.required_alignment < TY_align(request.descriptor_ty) ||
        tensor_tcon.byte_offset != request.byte_offset ||
        tensor_tcon.byte_length != request.byte_length ||
        !DSL_Tensor_TCON_Get_Side_Path
             (request.tensor_tcon, &side_path, &side_path_length) ||
        strlen(request.side_file) != side_path_length ||
        memcmp(request.side_file, side_path, side_path_length) != 0)
        return FALSE;

    const char *placement = descriptor.placement == STR_IDX_ZERO ? NULL :
                            Index_To_Str(descriptor.placement);
    const char *memory = descriptor.memory == STR_IDX_ZERO ? NULL :
                         Index_To_Str(descriptor.memory);
    if (descriptor.dtype == STR_IDX_ZERO ||
        descriptor.logical_shape == STR_IDX_ZERO ||
        descriptor.layout == STR_IDX_ZERO || placement == NULL ||
        strcmp(placement, "side_file") != 0 || memory == NULL ||
        strcmp(memory, "external_data") != 0)
        return FALSE;

    DSL_IR_VALUE_RECORD source;
    BOOL source_is_external =
        DSL_IR_Image_Get_External_Tensor_Reference
            (owner_pu_st, request.source_value_id, &source_reference);
    BOOL source_is_implicit_zero = FALSE;
    if (!source_is_external && request.source_policy ==
            DSL_IR_MATERIALIZE_SOURCE_EXTERNAL_OR_IMPLICIT_ZERO &&
        DSL_IR_Image_Get_Value(request.source_value_id, &source) &&
        source.value_kind == DSL_IR_VALUE_CONSTANT &&
        source.ty == request.descriptor_ty &&
        DSL_IR_Image_Value_Belongs_To_PU(source, owner_pu_st)) {
        DSL_IR_NODE_RECORD source_node;
        DSL_IR_OPCODE_DESCRIPTOR_RECORD source_opcode;
        const char *source_kind = NULL;
        source_is_implicit_zero =
            DSL_IR_Image_Get_Node(source.producer_node_id, &source_node) &&
            source_node.result_value_id == source.id &&
            DSL_IR_Image_Get_Opcode_Descriptor
                (source_node.opcode_descriptor_id, &source_opcode) &&
            source_opcode.logical_operator == OPR_DSLTENSORCONST &&
            source_opcode.version == 1 &&
            source_opcode.effect_model == DSL_EFFECT_MODEL_PURE &&
            DSL_IR_Node_Attribute
                (source_node, "value_kind", &source_kind) &&
            strcmp(source_kind, "implicit_zero") == 0;
        for (UINT32 i = 1;
             source_is_implicit_zero &&
             i <= DSL_Effect_Image_State_Effect_Count(); ++i) {
            DSL_STATE_EFFECT_RECORD effect;
            if (!DSL_Effect_Image_Get_State_Effect(i, &effect) ||
                effect.owner_node_id == source_node.id)
                source_is_implicit_zero = FALSE;
        }
    }
    if (!source_is_external && !source_is_implicit_zero)
        return FALSE;
    if (source_is_external) {
        if (source_reference.descriptor_ty != request.descriptor_ty ||
            !DSL_IR_Image_Get_Value(request.source_value_id, &source))
            return FALSE;
    }
    if (source_value != NULL)
        *source_value = source;

    if (request.call != NULL) {
        if (request.insert_before != request.call ||
            request.expected_actual_value_id == DSL_IR_VALUE_INVALID_ID ||
            !DSL_IR_Value_From_Call_Actual
                 (owner_pu_st, request.call, request.actual_ordinal,
                  &actual_value) ||
            actual_value.id != request.expected_actual_value_id ||
            actual_value.id != request.source_value_id ||
            actual_value.ty != request.descriptor_ty)
            return FALSE;
    } else if (request.expected_actual_value_id !=
                   DSL_IR_VALUE_INVALID_ID) {
        return FALSE;
    }

    char offset_text[32];
    char length_text[32];
    snprintf(offset_text, sizeof(offset_text), "%llu",
             (unsigned long long)request.byte_offset);
    snprintf(length_text, sizeof(length_text), "%llu",
             (unsigned long long)request.byte_length);
    size_t uri_size = strlen(request.storage_format) +
                      strlen(request.side_file) + strlen(request.tensor_key) +
                      strlen(request.checksum == NULL ? "" :
                                                        request.checksum) +
                      96;
    char *uri_text = new char[uri_size];
    snprintf(uri_text, uri_size,
             "%s://%s#%s?offset=%s&length=%s&checksum=%s",
             request.storage_format, request.side_file, request.tensor_key,
             offset_text, length_text,
             request.checksum == NULL ? "" : request.checksum);
    *uri = uri_text;
    delete [] uri_text;

    const char *dtype = Index_To_Str(descriptor.dtype);
    const char *shape = Index_To_Str(descriptor.logical_shape);
    size_t payload_size =
        strlen("name=;dtype=;rank=;shape=;value_kind=external_data;value=") +
        strlen(request.name) + strlen(dtype) + strlen(shape) + uri->size() +
        16;
    char *payload_text = new char[payload_size];
    snprintf(payload_text, payload_size,
             "name=%s;dtype=%s;rank=%d;shape=%s;"
             "value_kind=external_data;value=%s",
             request.name, dtype, descriptor.rank, shape, uri->c_str());
    *payload = payload_text;
    delete [] payload_text;
    return TRUE;
}

void
DSL_IR_External_Tensor_Materialization_Request_Init
        (DSL_IR_EXTERNAL_TENSOR_MATERIALIZATION_REQUEST *request)
{
    if (request != NULL) {
        memset(request, 0, sizeof(*request));
        request->source_policy = DSL_IR_MATERIALIZE_SOURCE_EXTERNAL_ONLY;
    }
}

static void
DSL_IR_Copy_Tensor_Metadata (ST_IDX source, ST_IDX destination)
{
    for (UINT32 i = 0; i < ST_tensor_metadata_count(source); ++i) {
        const char *key;
        const char *value;
        TY_DSL_BIND_STATE state;
        if (ST_tensor_metadata_at(source, i, &key, &value, &state) &&
            state == TY_DSL_BIND_BOUND && key != NULL && value != NULL)
            ST_tensor_bind_metadata(destination, key, value);
    }
}

BOOL
DSL_IR_Materialize_External_Tensor_Values
        (ST_IDX owner_pu_st,
         const DSL_IR_EXTERNAL_TENSOR_MATERIALIZATION_REQUEST *requests,
         UINT32 request_count,
         DSL_IR_EXTERNAL_TENSOR_MATERIALIZATION_RESULT *results)
{
    if (!DSL_IR_Image_Current_PU_Is(owner_pu_st) || requests == NULL ||
        request_count == 0 || results == NULL)
        return FALSE;

    std::vector<DSL_IR_VALUE_RECORD> source_values(request_count);
    std::vector<std::string> uris(request_count);
    std::vector<std::string> payloads(request_count);
    std::vector<BOOL> update_call_abi(request_count, FALSE);
    for (UINT32 i = 0; i < request_count; ++i) {
        if (!DSL_IR_External_Tensor_Request_Valid
                 (owner_pu_st, requests[i], &source_values[i], &uris[i],
                  &payloads[i]) ||
            DSL_IR_PU_Value_Name_Exists(owner_pu_st, requests[i].name))
            return FALSE;
        if (requests[i].call != NULL) {
            DSL_CALL_ARGUMENT_RECORD argument;
            if (DSL_Call_ABI_Image_Find_Argument
                    (requests[i].call, requests[i].actual_ordinal,
                     &argument)) {
                if (argument.argument_value_id != requests[i].source_value_id)
                    return FALSE;
                update_call_abi[i] = TRUE;
            }
        }
        for (UINT32 prior = 0; prior < i; ++prior) {
            if (strcmp(requests[prior].name, requests[i].name) == 0 ||
                (requests[i].call != NULL &&
                 requests[i].call == requests[prior].call &&
                 requests[i].actual_ordinal ==
                     requests[prior].actual_ordinal))
                return FALSE;
        }
    }

    DSL_IR_OPCODE_DESCRIPTOR_ID descriptor_id =
        DSL_IR_Image_Find_Opcode_Descriptor(OPR_DSLTENSORCONST, 1);
    if (descriptor_id == DSL_IR_OPCODE_DESCRIPTOR_INVALID_ID)
        return FALSE;

    memset(results, 0, request_count * sizeof(*results));

    for (UINT32 i = 0; i < request_count; ++i) {
        const DSL_IR_EXTERNAL_TENSOR_MATERIALIZATION_REQUEST &request =
            requests[i];
        ST_IDX result_st = DSL_Tensor_Create_Result_Symbol
                               (request.name, request.descriptor_ty,
                                SCLASS_AUTO, EXPORT_LOCAL);
        Set_ST_Srcpos(St_Table[result_st], request.source_position);
        DSL_IR_Copy_Tensor_Metadata(source_values[i].st, result_st);

        char offset_text[32];
        char length_text[32];
        char tcon_text[32];
        char source_text[32];
        snprintf(offset_text, sizeof(offset_text), "%llu",
                 (unsigned long long)request.byte_offset);
        snprintf(length_text, sizeof(length_text), "%llu",
                 (unsigned long long)request.byte_length);
        snprintf(tcon_text, sizeof(tcon_text), "%u",
                 (UINT32)request.tensor_tcon);
        snprintf(source_text, sizeof(source_text), "%u",
                 request.source_value_id);
        ST_tensor_bind_metadata(result_st, "storage_format",
                                request.storage_format);
        ST_tensor_bind_metadata(result_st, "storage_file", request.side_file);
        ST_tensor_bind_metadata(result_st, "storage_tensor_key",
                                request.tensor_key);
        ST_tensor_bind_metadata(result_st, "storage_byte_offset",
                                offset_text);
        ST_tensor_bind_metadata(result_st, "storage_byte_length",
                                length_text);
        ST_tensor_bind_metadata(result_st, "storage_checksum",
                                request.checksum == NULL ? "" :
                                                           request.checksum);
        ST_tensor_bind_metadata(result_st, "tensor_tcon_idx", tcon_text);
        ST_tensor_bind_metadata(result_st, "dsl.converted_from_value_id",
                                source_text);

        WN *expression = DSL_WN_Create_Native
                             (OPR_DSLTENSORCONST, 1, payloads[i].c_str(),
                              NULL, 0);
        WN *definition = WN_CreateStid
                             (OPR_STID, MTYPE_V, MTYPE_M, 0, result_st,
                              request.descriptor_ty, expression);
        WN_Set_Linenum(definition, request.source_position);

        DSL_IR_NODE_RECORD node;
        DSL_IR_Node_Record_Init(&node);
        node.opcode_descriptor_id = descriptor_id;
        node.payload = Save_Str(payloads[i].c_str());
        DSL_IR_NODE_ID node_id = DSL_IR_Image_Add_Node(&node);

        DSL_IR_ATTRIBUTE_RECORD attributes[2];
        DSL_IR_Attribute_Record_Init(&attributes[0]);
        attributes[0].owner_node_id = node_id;
        attributes[0].value_kind = DSL_IR_ATTRIBUTE_VALUE_STRING;
        attributes[0].name = Save_Str("value_kind");
        attributes[0].value = Save_Str("external_data");
        DSL_IR_ATTRIBUTE_ID first_attribute =
            DSL_IR_Image_Add_Attribute(&attributes[0]);
        DSL_IR_Attribute_Record_Init(&attributes[1]);
        attributes[1].owner_node_id = node_id;
        attributes[1].value_kind = DSL_IR_ATTRIBUTE_VALUE_STRING;
        attributes[1].name = Save_Str("value");
        attributes[1].value = Save_Str(uris[i].c_str());
        DSL_IR_Image_Add_Attribute(&attributes[1]);

        DSL_IR_VALUE_RECORD value;
        DSL_IR_Value_Record_Init(&value);
        value.value_kind = DSL_IR_VALUE_CONSTANT;
        value.producer_node_id = node_id;
        value.ty = request.descriptor_ty;
        value.st = result_st;
        value.name = Save_Str(request.name);
        std::string owner = "owner_pu=";
        owner += ST_name(St_Table[owner_pu_st]);
        value.metadata = Save_Str(owner.c_str());
        DSL_IR_VALUE_ID value_id = DSL_IR_Image_Add_Value(&value);
        DSL_IR_Image_Set_Node_Links
            (node_id, DSL_IR_VALUE_REFERENCE_INVALID_ID, 0,
             first_attribute, 2, value_id);

        WN_INSERT_BlockBefore(request.insertion_block,
                              request.insert_before, definition);
        if (request.call != NULL) {
            WN *parm = WN_COPY_Tree
                           (WN_kid(request.call, request.actual_ordinal));
            WN_st_idx(WN_kid0(parm)) = result_st;
            WN_kid(request.call, request.actual_ordinal) = parm;
            if (update_call_abi[i]) {
                BOOL updated = DSL_Call_ABI_Image_Update_Argument_Value
                    (request.call, request.actual_ordinal,
                     request.source_value_id, value_id);
                FmtAssert(updated,
                          ("preflighted DSL call ABI update failed"));
            }
        }

        results[i].value_id = value_id;
        results[i].st = result_st;
        results[i].definition = definition;
    }
    return TRUE;
}

BOOL
DSL_IR_Image_Find_Definition_Value
        (ST_IDX owner_pu_st,
         const WN *definition,
         DSL_IR_VALUE_RECORD *value_record)
{
    DSL_IR_VALUE_RECORD value;
    DSL_IR_NODE_RECORD node;
    DSL_IR_OPCODE_DESCRIPTOR_RECORD descriptor;
    DSL_LOGICAL_OPCODE logical_opcode;
    const WN *expression;
    ST_IDX result_st;

    if (!DSL_IR_Image_Current_PU_Is(owner_pu_st) || definition == NULL ||
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

BOOL
DSL_IR_Rewrite_Native_Value
        (ST_IDX owner_pu_st,
         WN *definition,
         DSL_IR_VALUE_ID value_id,
         const DSL_IR_NATIVE_VALUE_REWRITE_REQUEST *request)
{
    DSL_IR_VALUE_RECORD result;
    DSL_IR_NODE_RECORD node;
    DSL_LOGICAL_OPCODE logical_opcode;
    DSL_OPERATOR_INFO replacement_info;
    std::vector<WN *> operands;

    if (request == NULL ||
        !DSL_IR_Image_Find_Definition_Value
            (owner_pu_st, definition, &result) ||
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

static BOOL
DSL_IR_Retire_Has_Use_Outside_Block
        (const WN *wn, const WN *containing_block, ST_IDX retiring_st)
{
    if (wn == NULL || wn == containing_block)
        return FALSE;
    if (WN_has_sym(wn) && WN_st_idx(wn) == retiring_st)
        return TRUE;
    if (WN_operator(wn) == OPR_BLOCK) {
        for (const WN *statement = WN_first(wn); statement != NULL;
             statement = WN_next(statement)) {
            if (DSL_IR_Retire_Has_Use_Outside_Block
                    (statement, containing_block, retiring_st))
                return TRUE;
        }
        return FALSE;
    }
    for (INT32 kid = 0; kid < WN_kid_count(wn); ++kid) {
        if (DSL_IR_Retire_Has_Use_Outside_Block
                (WN_kid(wn, kid), containing_block, retiring_st))
            return TRUE;
    }
    return FALSE;
}

BOOL
DSL_IR_Redirect_And_Retire_Native_Value
        (ST_IDX owner_pu_st,
         const DSL_IR_NATIVE_VALUE_RETIRE_REQUEST *request)
{
    if (!DSL_IR_Image_Current_PU_Is(owner_pu_st) || request == NULL ||
        Current_PU_Info == NULL ||
        PU_Info_proc_sym(Current_PU_Info) != owner_pu_st ||
        request->pu_root != PU_Info_tree_ptr(Current_PU_Info) ||
        request->containing_block == NULL ||
        WN_operator(request->containing_block) != OPR_BLOCK ||
        !DSL_IR_Block_Contains
            (request->containing_block, request->replacement_definition) ||
        !DSL_IR_Block_Contains
            (request->containing_block, request->retiring_definition) ||
        !DSL_IR_Definition_Precedes
            (request->containing_block, request->replacement_definition,
             request->retiring_definition))
        return FALSE;

    DSL_IR_VALUE_RECORD replacement;
    DSL_IR_VALUE_RECORD retiring;
    if (!DSL_IR_Image_Find_Definition_Value
            (owner_pu_st, request->replacement_definition, &replacement) ||
        !DSL_IR_Image_Find_Definition_Value
            (owner_pu_st, request->retiring_definition, &retiring) ||
        replacement.id != request->replacement_value_id ||
        retiring.id != request->retiring_value_id ||
        replacement.ty != retiring.ty ||
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
    DSL_IR_Retire_Scan_Tree(request->containing_block, &scan);
    if (!scan.valid || !scan.retiring_seen || scan.definition_count != 1 ||
        DSL_IR_Retire_Has_Use_Outside_Block
            (request->pu_root, request->containing_block, retiring.st))
        return FALSE;

    UINT32 region_uses = DSL_Region_Symbol_Use_Count
                             (Current_PU_Info, retiring.st);
    if (region_uses != 0 &&
        !DSL_Region_Can_Redirect_Symbol
             (Current_PU_Info, retiring.st, replacement.st))
        return FALSE;

    for (UINT32 i = 0; i < scan.reads.size(); ++i)
        WN_st_idx(scan.reads[i]) = replacement.st;
    if (region_uses != 0) {
        BOOL redirected = DSL_Region_Redirect_Symbol
                              (Current_PU_Info, retiring.st, replacement.st);
        FmtAssert(redirected, ("preflighted REGION redirect failed"));
    }
    BOOL image_redirected = DSL_IR_Image_Redirect_And_Retire_Value
        (replacement.id, retiring.id, request->replacement_operand_ordinal);
    FmtAssert(image_redirected, ("preflighted DSL value redirect failed"));
    WN *removed = WN_EXTRACT_FromBlock
                      (request->containing_block,
                       request->retiring_definition);
    FmtAssert(removed == request->retiring_definition,
              ("preflighted DSL definition retirement failed"));
    WN_DELETE_Tree(removed);
    FmtAssert(DSL_IR_Image_Validate(NULL) &&
              DSL_Region_Verify_PU(Current_PU_Info, NULL),
              ("retired DSL value failed postcondition"));
    return TRUE;
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
    if (!DSL_IR_Image_Current_PU_Is(owner_pu_st) ||
        Current_PU_Info == NULL ||
        PU_Info_proc_sym(Current_PU_Info) != owner_pu_st ||
        requests == NULL || request_count == 0 || results == NULL)
        return DSL_IR_Lower_Report(diagnostic, 0, "transaction is incomplete");
    memset(results, 0,
           request_count * sizeof(DSL_IR_NATIVE_VALUE_LOWER_RESULT));

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
            !DSL_IR_Block_Contains
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
            !DSL_IR_Block_Contains
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
        !DSL_IR_Image_Current_PU_Is(request.owner_pu_st))
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

/*
 * Program-interface evolution extends runtime projection with verified ABI
 * pruning and rooted runtime resources.  Domains select semantic requests;
 * this generic layer owns physical WN/ST/TY and image consistency.
 */
static BOOL
DSL_Program_Interface_Report
        (FILE *diagnostic, const char *message, UINT32 id)
{
    if (diagnostic != NULL)
        fprintf(diagnostic, "DSL program interface error: %s id=%u\n",
                message, id);
    return FALSE;
}

static BOOL
DSL_Program_Interface_String_Valid (const char *text)
{
    return text != NULL && text[0] != '\0';
}

static const DSL_RETIRED_FORMAL_REQUEST *
DSL_Program_Interface_Find_Retired_Formal_Request
        (const DSL_PROGRAM_INTERFACE_PLAN *plan,
         DSL_PU_FORMAL_ID formal_id)
{
    for (UINT32 i = 0; plan != NULL && i < plan->retired_formal_count; ++i) {
        if (plan->retired_formals[i].pu_formal_id == formal_id)
            return &plan->retired_formals[i];
    }
    return NULL;
}

static const DSL_RETIRED_CALL_ARGUMENT_REQUEST *
DSL_Program_Interface_Find_Retired_Call_Request
        (const DSL_PROGRAM_INTERFACE_PLAN *plan,
         DSL_CALL_ARGUMENT_ID argument_id)
{
    for (UINT32 i = 0;
         plan != NULL && i < plan->retired_call_argument_count; ++i) {
        if (plan->retired_call_arguments[i].call_argument_id == argument_id)
            return &plan->retired_call_arguments[i];
    }
    return NULL;
}

static UINT32
DSL_Program_Interface_Retired_Formals_Before
        (const DSL_PROGRAM_INTERFACE_PLAN *plan, ST_IDX owner_pu_st,
         UINT32 ordinal)
{
    UINT32 count = 0;
    for (UINT32 i = 0; i < plan->retired_formal_count; ++i) {
        DSL_PU_FORMAL_RECORD formal;
        if (DSL_PU_Interface_Image_Get_Formal
                (plan->retired_formals[i].pu_formal_id, &formal) &&
            formal.owner_pu_st == owner_pu_st &&
            formal.formal_ordinal < ordinal)
            ++count;
    }
    return count;
}

static UINT32
DSL_Program_Interface_Live_Formal_Count
        (const DSL_PROGRAM_INTERFACE_PLAN *plan, ST_IDX owner_pu_st)
{
    UINT32 count = 0;
    for (UINT32 i = 1; i <= DSL_PU_Interface_Image_Formal_Count(); ++i) {
        DSL_PU_FORMAL_RECORD formal;
        if (DSL_PU_Interface_Image_Get_Formal(i, &formal) &&
            formal.owner_pu_st == owner_pu_st &&
            DSL_Program_Interface_Find_Retired_Formal_Request
                (plan, formal.id) == NULL)
            ++count;
    }
    return count;
}

static const DSL_RUNTIME_VALUE_PROJECTION_REQUEST *
DSL_Program_Interface_Find_Runtime_Value
        (const DSL_RUNTIME_INTERFACE_PLAN *plan, ST_IDX owner_pu_st,
         DSL_IR_VALUE_ID value_id)
{
    for (UINT32 i = 0; plan != NULL && i < plan->value_count; ++i) {
        if (plan->values[i].owner_pu_st == owner_pu_st &&
            plan->values[i].source_value_id == value_id)
            return &plan->values[i];
    }
    return NULL;
}

static const DSL_RUNTIME_CALL_PROJECTION_REQUEST *
DSL_Program_Interface_Find_Runtime_Call
        (const DSL_RUNTIME_INTERFACE_PLAN *plan,
         DSL_CALLSITE_METADATA_ID callsite_id, UINT32 actual_ordinal)
{
    for (UINT32 i = 0; plan != NULL && i < plan->call_count; ++i) {
        if (plan->calls[i].callsite_id == callsite_id &&
            plan->calls[i].actual_ordinal == actual_ordinal)
            return &plan->calls[i];
    }
    return NULL;
}

static BOOL
DSL_Program_Interface_Input_Less
        (const DSL_RUNTIME_INPUT_REQUEST *left,
         const DSL_RUNTIME_INPUT_REQUEST *right)
{
    if (left->input_kind != right->input_kind)
        return left->input_kind < right->input_kind;
    INT role_order = strcmp(left->stable_role, right->stable_role);
    if (role_order != 0)
        return role_order < 0;
    if (left->source_owner_pu_st != right->source_owner_pu_st)
        return left->source_owner_pu_st < right->source_owner_pu_st;
    if (left->source_value_id != right->source_value_id)
        return left->source_value_id < right->source_value_id;
    return left->source_tcon < right->source_tcon;
}

static BOOL
DSL_Program_Interface_Binding_Less
        (const DSL_RUNTIME_INPUT_BINDING_REQUEST *left,
         const DSL_RUNTIME_INPUT_BINDING_REQUEST *right,
         const DSL_PROGRAM_INTERFACE_PLAN *plan)
{
    if (left->binding_kind != right->binding_kind)
        return left->binding_kind < right->binding_kind;
    INT role_order = strcmp(left->semantic_role, right->semantic_role);
    if (role_order != 0)
        return role_order < 0;
    if (left->runtime_input_index ==
            DSL_PROGRAM_INTERFACE_INVALID_REQUEST_INDEX ||
        right->runtime_input_index ==
            DSL_PROGRAM_INTERFACE_INVALID_REQUEST_INDEX)
        return left->runtime_input_index < right->runtime_input_index;
    return DSL_Program_Interface_Input_Less
        (&plan->runtime_inputs[left->runtime_input_index],
         &plan->runtime_inputs[right->runtime_input_index]);
}

static UINT32
DSL_Program_Interface_Binding_Ordinal
        (const DSL_PROGRAM_INTERFACE_PLAN *plan, UINT32 binding_index)
{
    const DSL_RUNTIME_INPUT_BINDING_REQUEST &binding =
        plan->runtime_input_bindings[binding_index];
    UINT32 ordinal = DSL_Program_Interface_Live_Formal_Count
                         (plan, binding.owner_pu_st);
    for (UINT32 i = 0; i < plan->runtime_input_binding_count; ++i) {
        if (plan->runtime_input_bindings[i].owner_pu_st ==
                binding.owner_pu_st &&
            DSL_Program_Interface_Binding_Less
                (&plan->runtime_input_bindings[i], &binding, plan))
            ++ordinal;
    }
    return ordinal;
}

static BOOL
DSL_Program_Interface_Runtime_Value_Valid
        (const DSL_RUNTIME_VALUE_PROJECTION_REQUEST &request)
{
    DSL_IR_VALUE_RECORD source;
    return DSL_IR_Image_PU_ST_Valid(request.owner_pu_st) &&
           request.source_value_id != DSL_IR_VALUE_INVALID_ID &&
           DSL_IR_Image_Get_Value(request.source_value_id, &source) &&
           source.st == request.expected_source_st &&
           source.ty == request.expected_source_ty &&
           DSL_Runtime_Interface_Value_Owner_Valid
               (request.owner_pu_st, source) &&
           TY_is_tensor_extension(request.expected_source_ty) &&
           request.handle_ty != TY_IDX_ZERO &&
           TY_IDX_index(request.handle_ty) < TY_Table_Size() &&
           TY_kind(request.handle_ty) == KIND_POINTER &&
           request.binding_kind >= DSL_RUNTIME_BINDING_LOCAL_VALUE &&
           request.binding_kind <= DSL_RUNTIME_BINDING_RESULT_FORMAL;
}

static std::string DSL_program_interface_prepared_plan;

void
DSL_Program_Interface_Reset_Prepared_Plan (void)
{
    DSL_program_interface_prepared_plan.clear();
}

static void
DSL_Program_Interface_Append_Text
        (std::ostringstream *stream, const char *text)
{
    size_t length = text == NULL ? 0 : strlen(text);
    *stream << length << ':';
    if (length != 0)
        stream->write(text, length);
    *stream << ';';
}

static std::string
DSL_Program_Interface_Input_Request_Key
        (const DSL_RUNTIME_INPUT_REQUEST &request)
{
    std::ostringstream stream;
    stream << request.input_kind << ';' << request.source_owner_pu_st << ';'
           << request.source_value_id << ';' << request.source_ty << ';'
           << request.source_tcon << ';' << request.handle_ty << ';';
    DSL_Program_Interface_Append_Text(&stream, request.stable_role);
    return stream.str();
}

static std::string
DSL_Program_Interface_Binding_Request_Key
        (const DSL_PROGRAM_INTERFACE_PLAN *plan, UINT32 index)
{
    const DSL_RUNTIME_INPUT_BINDING_REQUEST &request =
        plan->runtime_input_bindings[index];
    std::ostringstream stream;
    stream << request.owner_pu_st << ';' << request.handle_ty << ';'
           << request.binding_kind << ';'
           << (unsigned long long)request.source_position << ';';
    DSL_Program_Interface_Append_Text(&stream, request.semantic_role);
    if (request.runtime_input_index !=
        DSL_PROGRAM_INTERFACE_INVALID_REQUEST_INDEX)
        stream << DSL_Program_Interface_Input_Request_Key
                      (plan->runtime_inputs[request.runtime_input_index]);
    return stream.str();
}

static void
DSL_Program_Interface_Append_Keys
        (std::ostringstream *stream, const char *category,
         std::vector<std::string> *keys)
{
    std::sort(keys->begin(), keys->end());
    *stream << category << ':' << keys->size() << ';';
    for (UINT32 i = 0; i < keys->size(); ++i) {
        *stream << (*keys)[i].size() << ':';
        stream->write((*keys)[i].data(), (*keys)[i].size());
        *stream << ';';
    }
}

static std::string
DSL_Program_Interface_Plan_Fingerprint
        (const DSL_PROGRAM_INTERFACE_PLAN *program_plan,
         const DSL_RUNTIME_INTERFACE_PLAN *runtime_plan)
{
    std::ostringstream result;
    std::vector<std::string> keys;
    for (UINT32 i = 0; i < program_plan->retired_formal_count; ++i) {
        std::ostringstream key;
        key << program_plan->retired_formals[i].pu_formal_id << ';';
        DSL_Program_Interface_Append_Text
            (&key, program_plan->retired_formals[i].semantic_role);
        keys.push_back(key.str());
    }
    DSL_Program_Interface_Append_Keys(&result, "retired_formal", &keys);
    keys.clear();
    for (UINT32 i = 0;
         i < program_plan->retired_call_argument_count; ++i) {
        std::ostringstream key;
        key << program_plan->retired_call_arguments[i].call_argument_id
            << ';';
        DSL_Program_Interface_Append_Text
            (&key, program_plan->retired_call_arguments[i].semantic_role);
        keys.push_back(key.str());
    }
    DSL_Program_Interface_Append_Keys(&result, "retired_call", &keys);
    keys.clear();
    for (UINT32 i = 0; i < program_plan->runtime_input_count; ++i)
        keys.push_back(DSL_Program_Interface_Input_Request_Key
                           (program_plan->runtime_inputs[i]));
    DSL_Program_Interface_Append_Keys(&result, "input", &keys);
    keys.clear();
    for (UINT32 i = 0;
         i < program_plan->runtime_input_binding_count; ++i)
        keys.push_back(DSL_Program_Interface_Binding_Request_Key
                           (program_plan, i));
    DSL_Program_Interface_Append_Keys(&result, "binding", &keys);
    keys.clear();
    for (UINT32 i = 0;
         i < program_plan->runtime_input_call_count; ++i) {
        const DSL_RUNTIME_INPUT_CALL_REQUEST &request =
            program_plan->runtime_input_calls[i];
        std::ostringstream key;
        key << request.callsite_id << ';'
            << DSL_Program_Interface_Binding_Request_Key
                   (program_plan, request.caller_binding_index)
            << DSL_Program_Interface_Binding_Request_Key
                   (program_plan, request.callee_binding_index);
        keys.push_back(key.str());
    }
    DSL_Program_Interface_Append_Keys(&result, "input_call", &keys);
    keys.clear();
    for (UINT32 i = 0; i < runtime_plan->value_count; ++i) {
        const DSL_RUNTIME_VALUE_PROJECTION_REQUEST &request =
            runtime_plan->values[i];
        std::ostringstream key;
        key << request.owner_pu_st << ';' << request.source_value_id << ';'
            << request.expected_source_st << ';'
            << request.expected_source_ty << ';' << request.handle_ty << ';'
            << request.binding_kind << ';' << request.formal_ordinal << ';';
        keys.push_back(key.str());
    }
    DSL_Program_Interface_Append_Keys(&result, "projection", &keys);
    keys.clear();
    for (UINT32 i = 0; i < runtime_plan->call_count; ++i) {
        const DSL_RUNTIME_CALL_PROJECTION_REQUEST &request =
            runtime_plan->calls[i];
        std::ostringstream key;
        key << request.owner_pu_st << ';' << request.callsite_id << ';'
            << request.source_value_id << ';' << request.actual_ordinal << ';'
            << request.callee_formal_ordinal << ';' << request.direction
            << ';';
        keys.push_back(key.str());
    }
    DSL_Program_Interface_Append_Keys(&result, "call_projection", &keys);
    return result.str();
}

void
DSL_Program_Interface_Result_Init (DSL_PROGRAM_INTERFACE_RESULT *result)
{
    if (result != NULL)
        memset(result, 0, sizeof(*result));
}

BOOL
DSL_Program_Interface_Plan_Validate
        (const DSL_PROGRAM_INTERFACE_PLAN *program_plan,
         const DSL_RUNTIME_INTERFACE_PLAN *runtime_plan,
         FILE *diagnostic)
{
    if (program_plan == NULL || runtime_plan == NULL ||
        (program_plan->retired_formal_count != 0 &&
         program_plan->retired_formals == NULL) ||
        (program_plan->retired_call_argument_count != 0 &&
         program_plan->retired_call_arguments == NULL) ||
        (program_plan->runtime_input_count != 0 &&
         program_plan->runtime_inputs == NULL) ||
        (program_plan->runtime_input_binding_count != 0 &&
         program_plan->runtime_input_bindings == NULL) ||
        (program_plan->runtime_input_call_count != 0 &&
         program_plan->runtime_input_calls == NULL) ||
        (runtime_plan->value_count != 0 && runtime_plan->values == NULL) ||
        (runtime_plan->call_count != 0 && runtime_plan->calls == NULL) ||
        DSL_Program_Interface_Image_Has_Records())
        return DSL_Program_Interface_Report
                   (diagnostic, "plan is incomplete or already applied", 0);

    for (UINT32 i = 0; i < program_plan->retired_formal_count; ++i) {
        const DSL_RETIRED_FORMAL_REQUEST &request =
            program_plan->retired_formals[i];
        DSL_PU_FORMAL_RECORD formal;
        if (!DSL_PU_Interface_Image_Get_Formal(request.pu_formal_id, &formal) ||
            !DSL_Program_Interface_String_Valid(request.semantic_role))
            return DSL_Program_Interface_Report
                       (diagnostic, "invalid retired formal request", i + 1);
        for (UINT32 j = 0; j < i; ++j) {
            if (program_plan->retired_formals[j].pu_formal_id ==
                request.pu_formal_id)
                return DSL_Program_Interface_Report
                           (diagnostic, "duplicate retired formal request",
                            i + 1);
        }
        UINT32 incoming = 0;
        UINT32 covered = 0;
        for (UINT32 j = 1; j <= DSL_Call_ABI_Image_Argument_Count(); ++j) {
            DSL_CALL_ARGUMENT_RECORD argument;
            DSL_CALLSITE_METADATA_RECORD callsite;
            if (!DSL_Call_ABI_Image_Get_Argument(j, &argument) ||
                !DSL_Call_Image_Get_Callsite(argument.callsite_id, &callsite))
                return DSL_Program_Interface_Report
                           (diagnostic, "missing canonical call", j);
            if (callsite.callee_pu_st == formal.owner_pu_st &&
                argument.callee_formal_ordinal == formal.formal_ordinal) {
                ++incoming;
                const DSL_RETIRED_CALL_ARGUMENT_REQUEST *retired =
                    DSL_Program_Interface_Find_Retired_Call_Request
                        (program_plan, argument.id);
                if (retired != NULL &&
                    strcmp(retired->semantic_role, request.semantic_role) == 0)
                    ++covered;
            }
        }
        if (incoming != covered)
            return DSL_Program_Interface_Report
                       (diagnostic, "incomplete retired formal coverage",
                        i + 1);
    }

    for (UINT32 i = 0;
         i < program_plan->retired_call_argument_count; ++i) {
        const DSL_RETIRED_CALL_ARGUMENT_REQUEST &request =
            program_plan->retired_call_arguments[i];
        DSL_CALL_ARGUMENT_RECORD argument;
        DSL_CALLSITE_METADATA_RECORD callsite;
        DSL_PU_FORMAL_RECORD formal;
        if (!DSL_Call_ABI_Image_Get_Argument
                (request.call_argument_id, &argument) ||
            !DSL_Call_Image_Get_Callsite(argument.callsite_id, &callsite) ||
            !DSL_PU_Interface_Image_Find_Formal
                (callsite.callee_pu_st, argument.callee_formal_ordinal,
                 &formal) ||
            DSL_Program_Interface_Find_Retired_Formal_Request
                (program_plan, formal.id) == NULL ||
            !DSL_Program_Interface_String_Valid(request.semantic_role))
            return DSL_Program_Interface_Report
                       (diagnostic, "invalid retired call request", i + 1);
        for (UINT32 j = 0; j < i; ++j) {
            if (program_plan->retired_call_arguments[j].call_argument_id ==
                request.call_argument_id)
                return DSL_Program_Interface_Report
                           (diagnostic, "duplicate retired call request",
                            i + 1);
        }
    }

    for (UINT32 i = 0; i < runtime_plan->value_count; ++i) {
        const DSL_RUNTIME_VALUE_PROJECTION_REQUEST &request =
            runtime_plan->values[i];
        DSL_PU_FORMAL_RECORD formal;
        BOOL is_formal = request.binding_kind ==
                             DSL_RUNTIME_BINDING_INPUT_FORMAL ||
                         request.binding_kind ==
                             DSL_RUNTIME_BINDING_RESULT_FORMAL;
        if (!DSL_Program_Interface_Runtime_Value_Valid(request) ||
            (is_formal &&
             (!DSL_PU_Interface_Image_Find_Formal
                  (request.owner_pu_st, request.formal_ordinal, &formal) ||
              formal.formal_value_id != request.source_value_id ||
              DSL_Program_Interface_Find_Retired_Formal_Request
                  (program_plan, formal.id) != NULL)) ||
            (!is_formal && request.formal_ordinal !=
                               DSL_RUNTIME_INTERFACE_INVALID_ORDINAL))
            return DSL_Program_Interface_Report
                       (diagnostic, "invalid live projection request", i + 1);
        for (UINT32 j = 0; j < i; ++j) {
            if (runtime_plan->values[j].owner_pu_st == request.owner_pu_st &&
                runtime_plan->values[j].source_value_id ==
                    request.source_value_id)
                return DSL_Program_Interface_Report
                           (diagnostic, "duplicate live projection", i + 1);
        }
    }

    for (UINT32 i = 1; i <= DSL_PU_Interface_Image_Formal_Count(); ++i) {
        DSL_PU_FORMAL_RECORD formal;
        if (!DSL_PU_Interface_Image_Get_Formal(i, &formal))
            return DSL_Program_Interface_Report
                       (diagnostic, "missing canonical formal", i);
        BOOL retired = DSL_Program_Interface_Find_Retired_Formal_Request
                           (program_plan, formal.id) != NULL;
        BOOL projected = DSL_Program_Interface_Find_Runtime_Value
                             (runtime_plan, formal.owner_pu_st,
                              formal.formal_value_id) != NULL;
        if (retired == projected)
            return DSL_Program_Interface_Report
                       (diagnostic, "formal retirement/projection mismatch",
                        i);
    }

    for (UINT32 i = 0; i < runtime_plan->call_count; ++i) {
        const DSL_RUNTIME_CALL_PROJECTION_REQUEST &request =
            runtime_plan->calls[i];
        DSL_CALLSITE_METADATA_RECORD callsite;
        DSL_CALL_ARGUMENT_RECORD argument;
        if (!DSL_Call_Image_Get_Callsite(request.callsite_id, &callsite) ||
            callsite.owner_pu_st != request.owner_pu_st ||
            request.actual_ordinal != request.callee_formal_ordinal ||
            request.direction < DSL_RUNTIME_CALL_INPUT ||
            request.direction > DSL_RUNTIME_CALL_RESULT ||
            DSL_Program_Interface_Find_Runtime_Value
                (runtime_plan, request.owner_pu_st,
                 request.source_value_id) == NULL ||
            (request.direction == DSL_RUNTIME_CALL_INPUT &&
             (!DSL_Call_ABI_Image_Find_Argument_By_Id
                  (request.callsite_id, request.actual_ordinal, &argument) ||
              argument.argument_value_id != request.source_value_id ||
              DSL_Program_Interface_Find_Retired_Call_Request
                  (program_plan, argument.id) != NULL)))
            return DSL_Program_Interface_Report
                       (diagnostic, "invalid live call projection", i + 1);
        for (UINT32 j = 0; j < i; ++j) {
            if (runtime_plan->calls[j].callsite_id == request.callsite_id &&
                runtime_plan->calls[j].actual_ordinal ==
                    request.actual_ordinal)
                return DSL_Program_Interface_Report
                           (diagnostic, "duplicate live call projection",
                            i + 1);
        }
    }
    for (UINT32 i = 1; i <= DSL_Call_ABI_Image_Argument_Count(); ++i) {
        DSL_CALL_ARGUMENT_RECORD argument;
        if (!DSL_Call_ABI_Image_Get_Argument(i, &argument))
            return DSL_Program_Interface_Report
                       (diagnostic, "missing canonical call argument", i);
        BOOL retired = DSL_Program_Interface_Find_Retired_Call_Request
                           (program_plan, argument.id) != NULL;
        BOOL projected = DSL_Program_Interface_Find_Runtime_Call
                             (runtime_plan, argument.callsite_id,
                              argument.actual_ordinal) != NULL;
        if (retired == projected)
            return DSL_Program_Interface_Report
                       (diagnostic, "call retirement/projection mismatch", i);
    }

    for (UINT32 i = 0; i < program_plan->runtime_input_count; ++i) {
        const DSL_RUNTIME_INPUT_REQUEST &input =
            program_plan->runtime_inputs[i];
        DSL_RUNTIME_INPUT_RECORD record;
        memset(&record, 0, sizeof(record));
        record.input_kind = input.input_kind;
        record.source_owner_pu_st = input.source_owner_pu_st;
        record.source_value_id = input.source_value_id;
        record.source_ty = input.source_ty;
        record.source_tcon = input.source_tcon;
        record.handle_ty = input.handle_ty;
        if (input.input_kind == DSL_RUNTIME_INPUT_SOURCE_EXTERNAL_TENSOR) {
            DSL_IR_VALUE_RECORD value;
            if (DSL_IR_Image_Get_Value(input.source_value_id, &value))
                record.source_st = value.st;
        }
        if (!DSL_Program_Interface_Runtime_Input_Contract_Valid(&record) ||
            !DSL_Program_Interface_String_Valid(input.stable_role) ||
            input.handle_ty == TY_IDX_ZERO)
            return DSL_Program_Interface_Report
                       (diagnostic, "invalid runtime input request", i + 1);
        for (UINT32 j = 0; j < i; ++j) {
            const DSL_RUNTIME_INPUT_REQUEST &previous =
                program_plan->runtime_inputs[j];
            if ((input.input_kind ==
                     DSL_RUNTIME_INPUT_SOURCE_EXTERNAL_TENSOR &&
                 previous.input_kind == input.input_kind &&
                 previous.source_owner_pu_st == input.source_owner_pu_st &&
                 previous.source_value_id == input.source_value_id) ||
                (input.input_kind !=
                     DSL_RUNTIME_INPUT_SOURCE_EXTERNAL_TENSOR &&
                 previous.input_kind !=
                     DSL_RUNTIME_INPUT_SOURCE_EXTERNAL_TENSOR &&
                 strcmp(previous.stable_role, input.stable_role) == 0))
                return DSL_Program_Interface_Report
                           (diagnostic, "duplicate runtime input request",
                            i + 1);
        }
    }

    for (UINT32 i = 0;
         i < program_plan->runtime_input_binding_count; ++i) {
        const DSL_RUNTIME_INPUT_BINDING_REQUEST &binding =
            program_plan->runtime_input_bindings[i];
        BOOL root = binding.binding_kind ==
                        DSL_RUNTIME_INPUT_BINDING_ROOT_PROMOTED_SOURCE ||
                    binding.binding_kind ==
                        DSL_RUNTIME_INPUT_BINDING_ROOT_RESOURCE;
        if (!DSL_IR_Image_PU_ST_Valid(binding.owner_pu_st) ||
            binding.handle_ty == TY_IDX_ZERO ||
            TY_IDX_index(binding.handle_ty) >= TY_Table_Size() ||
            TY_kind(binding.handle_ty) != KIND_POINTER ||
            !DSL_Program_Interface_String_Valid(binding.semantic_role) ||
            binding.source_position == 0 ||
            binding.binding_kind <
                DSL_RUNTIME_INPUT_BINDING_ROOT_PROMOTED_SOURCE ||
            binding.binding_kind >
                DSL_RUNTIME_INPUT_BINDING_THREADED_FORMAL ||
            (root &&
             (binding.runtime_input_index >=
                  program_plan->runtime_input_count ||
              program_plan->runtime_inputs[binding.runtime_input_index].
                  handle_ty != binding.handle_ty ||
              (binding.binding_kind ==
                   DSL_RUNTIME_INPUT_BINDING_ROOT_PROMOTED_SOURCE) !=
                  (program_plan->runtime_inputs
                       [binding.runtime_input_index].input_kind ==
                   DSL_RUNTIME_INPUT_SOURCE_EXTERNAL_TENSOR))) ||
            (!root && binding.runtime_input_index !=
                          DSL_PROGRAM_INTERFACE_INVALID_REQUEST_INDEX))
            return DSL_Program_Interface_Report
                       (diagnostic, "invalid runtime binding request", i + 1);
        for (UINT32 j = 0; j < i; ++j) {
            const DSL_RUNTIME_INPUT_BINDING_REQUEST &previous =
                program_plan->runtime_input_bindings[j];
            if (previous.owner_pu_st == binding.owner_pu_st &&
                strcmp(previous.semantic_role, binding.semantic_role) == 0)
                return DSL_Program_Interface_Report
                           (diagnostic, "duplicate runtime binding request",
                            i + 1);
        }
    }

    std::vector<std::vector<BOOL> > edges
        (program_plan->runtime_input_binding_count,
         std::vector<BOOL>(program_plan->runtime_input_binding_count, FALSE));
    for (UINT32 i = 0; i < program_plan->runtime_input_call_count; ++i) {
        const DSL_RUNTIME_INPUT_CALL_REQUEST &request =
            program_plan->runtime_input_calls[i];
        if (request.caller_binding_index >=
                program_plan->runtime_input_binding_count ||
            request.callee_binding_index >=
                program_plan->runtime_input_binding_count)
            return DSL_Program_Interface_Report
                       (diagnostic, "invalid runtime call request", i + 1);
        const DSL_RUNTIME_INPUT_BINDING_REQUEST &caller =
            program_plan->runtime_input_bindings
                [request.caller_binding_index];
        const DSL_RUNTIME_INPUT_BINDING_REQUEST &callee =
            program_plan->runtime_input_bindings
                [request.callee_binding_index];
        DSL_CALLSITE_METADATA_RECORD callsite;
        if (!DSL_Call_Image_Get_Callsite(request.callsite_id, &callsite) ||
            callsite.owner_pu_st != caller.owner_pu_st ||
            callsite.callee_pu_st != callee.owner_pu_st ||
            callee.binding_kind !=
                DSL_RUNTIME_INPUT_BINDING_THREADED_FORMAL ||
            caller.handle_ty != callee.handle_ty)
            return DSL_Program_Interface_Report
                       (diagnostic, "invalid runtime call request", i + 1);
        for (UINT32 j = 0; j < i; ++j) {
            if (program_plan->runtime_input_calls[j].callsite_id ==
                    request.callsite_id &&
                program_plan->runtime_input_calls[j].callee_binding_index ==
                    request.callee_binding_index)
                return DSL_Program_Interface_Report
                           (diagnostic, "duplicate runtime call request",
                            i + 1);
        }
        edges[request.caller_binding_index]
             [request.callee_binding_index] = TRUE;
    }
    for (UINT32 k = 0; k < edges.size(); ++k)
        for (UINT32 i = 0; i < edges.size(); ++i)
            for (UINT32 j = 0; j < edges.size(); ++j)
                edges[i][j] = edges[i][j] ||
                              (edges[i][k] && edges[k][j]);
    for (UINT32 i = 0; i < edges.size(); ++i) {
        const DSL_RUNTIME_INPUT_BINDING_REQUEST &binding =
            program_plan->runtime_input_bindings[i];
        BOOL root = binding.binding_kind !=
                        DSL_RUNTIME_INPUT_BINDING_THREADED_FORMAL;
        UINT32 incoming_calls = 0;
        UINT32 represented_calls = 0;
        for (UINT32 c = 1; c <= DSL_Call_Image_Callsite_Count(); ++c) {
            DSL_CALLSITE_METADATA_RECORD callsite;
            if (!DSL_Call_Image_Get_Callsite(c, &callsite))
                return DSL_Program_Interface_Report
                           (diagnostic, "missing callsite", c);
            if (callsite.callee_pu_st == binding.owner_pu_st) {
                ++incoming_calls;
                for (UINT32 r = 0;
                     r < program_plan->runtime_input_call_count; ++r) {
                    if (program_plan->runtime_input_calls[r].callsite_id ==
                            callsite.id &&
                        program_plan->runtime_input_calls[r].
                            callee_binding_index == i)
                        ++represented_calls;
                }
            }
        }
        if ((root && incoming_calls != 0) ||
            (!root && incoming_calls != represented_calls))
            return DSL_Program_Interface_Report
                       (diagnostic, "incomplete rooted runtime flow", i + 1);
        if (edges[i][i])
            return DSL_Program_Interface_Report
                       (diagnostic, "cyclic runtime flow", i + 1);
        BOOL reachable = root;
        for (UINT32 r = 0; r < edges.size() && !reachable; ++r) {
            if (program_plan->runtime_input_bindings[r].binding_kind !=
                    DSL_RUNTIME_INPUT_BINDING_THREADED_FORMAL &&
                edges[r][i])
                reachable = TRUE;
        }
        if (!reachable)
            return DSL_Program_Interface_Report
                       (diagnostic, "unrooted runtime binding", i + 1);
    }

    for (UINT32 i = 0; i < program_plan->runtime_input_count; ++i) {
        UINT32 roots = 0;
        for (UINT32 j = 0;
             j < program_plan->runtime_input_binding_count; ++j) {
            if (program_plan->runtime_input_bindings[j].runtime_input_index == i)
                ++roots;
        }
        if (roots != 1)
            return DSL_Program_Interface_Report
                       (diagnostic, "runtime input root coverage", i + 1);
    }
    return TRUE;
}

typedef struct {
    const DSL_RUNTIME_VALUE_PROJECTION_REQUEST *request;
    ST_IDX handle_st;
    DSL_RUNTIME_VALUE_PROJECTION_ID projection_id;
} DSL_PROGRAM_CREATED_VALUE;

typedef struct {
    UINT32 request_index;
    ST_IDX handle_st;
    UINT32 final_formal_ordinal;
} DSL_PROGRAM_CREATED_BINDING;

struct DSL_PROGRAM_RUNTIME_VALUE_INDEX_LESS {
    const DSL_RUNTIME_INTERFACE_PLAN *plan;
    explicit DSL_PROGRAM_RUNTIME_VALUE_INDEX_LESS
        (const DSL_RUNTIME_INTERFACE_PLAN *value) : plan(value) {}
    BOOL operator() (UINT32 left, UINT32 right) const {
        const DSL_RUNTIME_VALUE_PROJECTION_REQUEST &a = plan->values[left];
        const DSL_RUNTIME_VALUE_PROJECTION_REQUEST &b = plan->values[right];
        if (a.owner_pu_st != b.owner_pu_st)
            return a.owner_pu_st < b.owner_pu_st;
        if (a.source_value_id != b.source_value_id)
            return a.source_value_id < b.source_value_id;
        return a.binding_kind < b.binding_kind;
    }
};

struct DSL_PROGRAM_BINDING_INDEX_LESS {
    const DSL_PROGRAM_INTERFACE_PLAN *plan;
    explicit DSL_PROGRAM_BINDING_INDEX_LESS
        (const DSL_PROGRAM_INTERFACE_PLAN *value) : plan(value) {}
    BOOL operator() (UINT32 left, UINT32 right) const {
        const DSL_RUNTIME_INPUT_BINDING_REQUEST &a =
            plan->runtime_input_bindings[left];
        const DSL_RUNTIME_INPUT_BINDING_REQUEST &b =
            plan->runtime_input_bindings[right];
        if (a.owner_pu_st != b.owner_pu_st)
            return a.owner_pu_st < b.owner_pu_st;
        UINT32 a_ordinal = DSL_Program_Interface_Binding_Ordinal(plan, left);
        UINT32 b_ordinal = DSL_Program_Interface_Binding_Ordinal(plan, right);
        return a_ordinal != b_ordinal ? a_ordinal < b_ordinal : left < right;
    }
};

struct DSL_PROGRAM_RETIRED_FORMAL_INDEX_LESS {
    const DSL_PROGRAM_INTERFACE_PLAN *plan;
    explicit DSL_PROGRAM_RETIRED_FORMAL_INDEX_LESS
        (const DSL_PROGRAM_INTERFACE_PLAN *value) : plan(value) {}
    BOOL operator() (UINT32 left, UINT32 right) const {
        return plan->retired_formals[left].pu_formal_id <
               plan->retired_formals[right].pu_formal_id;
    }
};

struct DSL_PROGRAM_RETIRED_CALL_INDEX_LESS {
    const DSL_PROGRAM_INTERFACE_PLAN *plan;
    explicit DSL_PROGRAM_RETIRED_CALL_INDEX_LESS
        (const DSL_PROGRAM_INTERFACE_PLAN *value) : plan(value) {}
    BOOL operator() (UINT32 left, UINT32 right) const {
        return plan->retired_call_arguments[left].call_argument_id <
               plan->retired_call_arguments[right].call_argument_id;
    }
};

struct DSL_PROGRAM_RUNTIME_CALL_INDEX_LESS {
    const DSL_RUNTIME_INTERFACE_PLAN *plan;
    explicit DSL_PROGRAM_RUNTIME_CALL_INDEX_LESS
        (const DSL_RUNTIME_INTERFACE_PLAN *value) : plan(value) {}
    BOOL operator() (UINT32 left, UINT32 right) const {
        const DSL_RUNTIME_CALL_PROJECTION_REQUEST &a = plan->calls[left];
        const DSL_RUNTIME_CALL_PROJECTION_REQUEST &b = plan->calls[right];
        if (a.owner_pu_st != b.owner_pu_st)
            return a.owner_pu_st < b.owner_pu_st;
        if (a.callsite_id != b.callsite_id)
            return a.callsite_id < b.callsite_id;
        if (a.actual_ordinal != b.actual_ordinal)
            return a.actual_ordinal < b.actual_ordinal;
        return a.direction < b.direction;
    }
};

struct DSL_PROGRAM_INPUT_CALL_INDEX_LESS {
    const DSL_PROGRAM_INTERFACE_PLAN *plan;
    explicit DSL_PROGRAM_INPUT_CALL_INDEX_LESS
        (const DSL_PROGRAM_INTERFACE_PLAN *value) : plan(value) {}
    BOOL operator() (UINT32 left, UINT32 right) const {
        const DSL_RUNTIME_INPUT_CALL_REQUEST &a =
            plan->runtime_input_calls[left];
        const DSL_RUNTIME_INPUT_CALL_REQUEST &b =
            plan->runtime_input_calls[right];
        if (a.callsite_id != b.callsite_id)
            return a.callsite_id < b.callsite_id;
        UINT32 a_ordinal = DSL_Program_Interface_Binding_Ordinal
                               (plan, a.callee_binding_index);
        UINT32 b_ordinal = DSL_Program_Interface_Binding_Ordinal
                               (plan, b.callee_binding_index);
        return a_ordinal != b_ordinal ? a_ordinal < b_ordinal : left < right;
    }
};

static const DSL_PROGRAM_CREATED_VALUE *
DSL_Program_Interface_Find_Created_Value
        (const std::vector<DSL_PROGRAM_CREATED_VALUE> &created,
         DSL_IR_VALUE_ID value_id)
{
    for (UINT32 i = 0; i < created.size(); ++i) {
        if (created[i].request->source_value_id == value_id)
            return &created[i];
    }
    return NULL;
}

static const DSL_PROGRAM_CREATED_BINDING *
DSL_Program_Interface_Find_Created_Binding
        (const std::vector<DSL_PROGRAM_CREATED_BINDING> &created,
         UINT32 request_index)
{
    for (UINT32 i = 0; i < created.size(); ++i) {
        if (created[i].request_index == request_index)
            return &created[i];
    }
    return NULL;
}

static BOOL
DSL_Program_Interface_Tree_Uses_ST (const WN *tree, ST_IDX st)
{
    if (tree == NULL)
        return FALSE;
    if (WN_has_sym(tree) && WN_st_idx(tree) == st)
        return TRUE;
    if (WN_operator(tree) == OPR_BLOCK) {
        for (const WN *stmt = WN_first(tree); stmt != NULL;
             stmt = WN_next(stmt)) {
            if (DSL_Program_Interface_Tree_Uses_ST(stmt, st))
                return TRUE;
        }
        return FALSE;
    }
    for (UINT32 i = 0; i < WN_kid_count(tree); ++i) {
        if (DSL_Program_Interface_Tree_Uses_ST(WN_kid(tree, i), st))
            return TRUE;
    }
    return FALSE;
}

static UINT32
DSL_Program_Interface_Input_Id
        (const DSL_PROGRAM_INTERFACE_PLAN *plan, UINT32 request_index)
{
    UINT32 id = 1;
    for (UINT32 i = 0; i < plan->runtime_input_count; ++i) {
        if (DSL_Program_Interface_Input_Less
                (&plan->runtime_inputs[i],
                 &plan->runtime_inputs[request_index]))
            ++id;
    }
    return id;
}

static ST_IDX
DSL_Program_Interface_Create_Binding_ST
        (const DSL_RUNTIME_INPUT_BINDING_REQUEST &request)
{
    std::string name("__dsl_input_");
    for (const char *cursor = request.semantic_role; *cursor != '\0';
         ++cursor) {
        unsigned char ch = (unsigned char)*cursor;
        name += isalnum(ch) ? (char)ch : '_';
    }
    ST *handle = New_ST(CURRENT_SYMTAB);
    ST_Init(handle, Save_Str(name.c_str()), CLASS_VAR, SCLASS_FORMAL,
            EXPORT_LOCAL, request.handle_ty);
    Set_ST_is_value_parm(*handle);
    Set_ST_Srcpos(*handle, request.source_position);
    return ST_st_idx(handle);
}

static TY_IDX
DSL_Program_Interface_Function_TY
        (const std::vector<ST_IDX> &formals)
{
    TY_IDX function_ty;
    TY &function = New_TY(function_ty);
    TY_Init(function, 0, KIND_FUNCTION, MTYPE_UNKNOWN, 0);
    Set_TY_align(function_ty, 1);
    TYLIST_IDX tylist_idx;
    Set_TYLIST_type(New_TYLIST(tylist_idx), MTYPE_To_TY(MTYPE_V));
    Set_TY_tylist(function_ty, tylist_idx);
    for (UINT32 i = 0; i < formals.size(); ++i) {
        ST &formal = St_Table[formals[i]];
        TY_IDX formal_ty = ST_sclass(formal) == SCLASS_FORMAL_REF ?
                           Make_Pointer_Type(ST_type(formal)) :
                           ST_type(formal);
        Set_TYLIST_type(New_TYLIST(tylist_idx), formal_ty);
    }
    Set_TYLIST_type(New_TYLIST(tylist_idx), TY_IDX_ZERO);
    return TY_is_unique(function_ty);
}

static BOOL
DSL_Program_Interface_Preflight_PU
        (PU_Info *pu, const DSL_PROGRAM_INTERFACE_PLAN *program_plan,
         const DSL_RUNTIME_INTERFACE_PLAN *runtime_plan,
         std::vector<DSL_RUNTIME_RETURN_SITE> *returns,
         FILE *diagnostic)
{
    if (pu == NULL || PU_Info_tree_ptr(pu) == NULL ||
        !DSL_IR_Image_Current_PU_Is(PU_Info_proc_sym(pu)))
        return DSL_Program_Interface_Report
                   (diagnostic, "program unit is not active", 0);
    ST_IDX owner_pu_st = PU_Info_proc_sym(pu);
    WN *entry = PU_Info_tree_ptr(pu);
    if (WN_operator(entry) != OPR_FUNC_ENTRY)
        return DSL_Program_Interface_Report
                   (diagnostic, "program unit has no FUNC_ENTRY", 0);

    UINT32 canonical_formals = 0;
    for (UINT32 i = 1; i <= DSL_PU_Interface_Image_Formal_Count(); ++i) {
        DSL_PU_FORMAL_RECORD formal;
        if (!DSL_PU_Interface_Image_Get_Formal(i, &formal))
            return DSL_Program_Interface_Report
                       (diagnostic, "missing canonical formal", i);
        if (formal.owner_pu_st != owner_pu_st)
            continue;
        if (formal.formal_ordinal != canonical_formals ||
            formal.formal_ordinal >= WN_num_formals(entry) ||
            WN_st_idx(WN_formal(entry, formal.formal_ordinal)) !=
                formal.formal_st)
            return DSL_Program_Interface_Report
                       (diagnostic, "physical formal mismatch", i);
        const DSL_RETIRED_FORMAL_REQUEST *retired =
            DSL_Program_Interface_Find_Retired_Formal_Request
                (program_plan, formal.id);
        if (retired != NULL &&
            (DSL_Program_Interface_Tree_Uses_ST
                 (WN_func_body(entry), formal.formal_st) ||
             DSL_Region_Symbol_Use_Count(pu, formal.formal_st) != 0))
            return DSL_Program_Interface_Report
                       (diagnostic, "retired formal remains executable", i);
        ++canonical_formals;
    }
    if (canonical_formals != WN_num_formals(entry))
        return DSL_Program_Interface_Report
                   (diagnostic, "incomplete canonical formal interface", 0);

    for (UINT32 i = 0; i < runtime_plan->value_count; ++i) {
        const DSL_RUNTIME_VALUE_PROJECTION_REQUEST &request =
            runtime_plan->values[i];
        if (request.owner_pu_st != owner_pu_st)
            continue;
        if (ST_IDX_level(request.expected_source_st) != CURRENT_SYMTAB ||
            ST_IDX_index(request.expected_source_st) == 0 ||
            ST_IDX_index(request.expected_source_st) >=
                ST_Table_Size(CURRENT_SYMTAB) ||
            ST_type(St_Table[request.expected_source_st]) !=
                request.expected_source_ty)
            return DSL_Program_Interface_Report
                       (diagnostic, "active projection source mismatch",
                        i + 1);
    }

    for (UINT32 i = 0; i < program_plan->runtime_input_count; ++i) {
        const DSL_RUNTIME_INPUT_REQUEST &input =
            program_plan->runtime_inputs[i];
        if (input.input_kind == DSL_RUNTIME_INPUT_SOURCE_EXTERNAL_TENSOR &&
            input.source_owner_pu_st == owner_pu_st) {
            DSL_IR_EXTERNAL_TENSOR_REFERENCE reference;
            if (!DSL_IR_Image_Get_External_Tensor_Reference
                    (owner_pu_st, input.source_value_id, &reference) ||
                reference.descriptor_ty != input.source_ty ||
                reference.st == ST_IDX_ZERO)
                return DSL_Program_Interface_Report
                           (diagnostic, "external runtime input mismatch",
                            i + 1);
        }
    }

    std::vector<UINT32> runtime_call_indexes;
    for (UINT32 i = 0; i < runtime_plan->call_count; ++i) {
        if (runtime_plan->calls[i].owner_pu_st == owner_pu_st)
            runtime_call_indexes.push_back(i);
    }
    std::sort(runtime_call_indexes.begin(), runtime_call_indexes.end(),
              DSL_PROGRAM_RUNTIME_CALL_INDEX_LESS(runtime_plan));
    for (UINT32 order = 0; order < runtime_call_indexes.size(); ++order) {
        UINT32 i = runtime_call_indexes[order];
        const DSL_RUNTIME_CALL_PROJECTION_REQUEST &request =
            runtime_plan->calls[i];
        const WN *call = DSL_Call_Image_Get_Call_WN(request.callsite_id);
        if (call == NULL || WN_operator(call) != OPR_CALL ||
            request.actual_ordinal >= WN_kid_count(call) ||
            DSL_Runtime_Interface_Parent_Block(entry, call) == NULL)
            return DSL_Program_Interface_Report
                       (diagnostic, "physical call is unavailable", i + 1);
    }
    return DSL_Runtime_Interface_Collect_Returns
               (entry, NULL, owner_pu_st, runtime_plan, returns, diagnostic);
}

static WN *
DSL_Program_Interface_Create_Call_Parm
        (const DSL_PROGRAM_CREATED_VALUE &value, UINT32 direction)
{
    TYPE_ID mtype = TY_mtype(value.request->handle_ty);
    if (direction == DSL_RUNTIME_CALL_INPUT) {
        WN *load = WN_CreateLdid
                       (OPR_LDID, mtype, mtype, 0, value.handle_st,
                        value.request->handle_ty);
        return WN_CreateParm
                   (mtype, load, value.request->handle_ty,
                    WN_PARM_BY_VALUE | WN_PARM_READ_ONLY |
                    WN_PARM_PASSED_NOT_SAVED);
    }
    TY_IDX pointer_ty = Make_Pointer_Type(value.request->handle_ty);
    WN *address = WN_CreateLda
                      (OPR_LDA, Pointer_Mtype, MTYPE_V, 0, pointer_ty,
                       value.handle_st);
    return WN_CreateParm
               (Pointer_Mtype, address, pointer_ty,
                WN_PARM_BY_REFERENCE | WN_PARM_OUT |
                WN_PARM_PASSED_NOT_SAVED);
}

static WN *
DSL_Program_Interface_Create_Binding_Parm
        (const DSL_PROGRAM_CREATED_BINDING &binding,
         const DSL_PROGRAM_INTERFACE_PLAN *plan)
{
    const DSL_RUNTIME_INPUT_BINDING_REQUEST &request =
        plan->runtime_input_bindings[binding.request_index];
    TYPE_ID mtype = TY_mtype(request.handle_ty);
    WN *load = WN_CreateLdid
                   (OPR_LDID, mtype, mtype, 0, binding.handle_st,
                    request.handle_ty);
    return WN_CreateParm
               (mtype, load, request.handle_ty,
                WN_PARM_BY_VALUE | WN_PARM_READ_ONLY |
                WN_PARM_PASSED_NOT_SAVED);
}

static BOOL
DSL_Program_Interface_Install_Inputs
        (const DSL_PROGRAM_INTERFACE_PLAN *plan)
{
    if (DSL_Program_Interface_Image_Runtime_Input_Count() != 0)
        return DSL_Program_Interface_Image_Runtime_Input_Count() ==
               plan->runtime_input_count;
    for (UINT32 expected_id = 1;
         expected_id <= plan->runtime_input_count; ++expected_id) {
        UINT32 request_index = DSL_PROGRAM_INTERFACE_INVALID_REQUEST_INDEX;
        for (UINT32 i = 0; i < plan->runtime_input_count; ++i) {
            if (DSL_Program_Interface_Input_Id(plan, i) == expected_id) {
                request_index = i;
                break;
            }
        }
        if (request_index == DSL_PROGRAM_INTERFACE_INVALID_REQUEST_INDEX)
            return FALSE;
        const DSL_RUNTIME_INPUT_REQUEST &request =
            plan->runtime_inputs[request_index];
        DSL_RUNTIME_INPUT_RECORD record;
        memset(&record, 0, sizeof(record));
        record.input_kind = request.input_kind;
        record.source_owner_pu_st = request.source_owner_pu_st;
        record.source_value_id = request.source_value_id;
        record.source_ty = request.source_ty;
        record.source_tcon = request.source_tcon;
        record.stable_role = Save_Str(request.stable_role);
        record.handle_ty = request.handle_ty;
        if (request.input_kind ==
            DSL_RUNTIME_INPUT_SOURCE_EXTERNAL_TENSOR) {
            DSL_IR_VALUE_RECORD value;
            if (!DSL_IR_Image_Get_Value(request.source_value_id, &value))
                return FALSE;
            record.source_st = value.st;
        }
        if (DSL_Program_Interface_Image_Add_Runtime_Input(&record) !=
            expected_id)
            return FALSE;
    }
    return TRUE;
}

BOOL
DSL_Program_Interface_Apply_PU
        (PU_Info *pu, const DSL_PROGRAM_INTERFACE_PLAN *program_plan,
         const DSL_RUNTIME_INTERFACE_PLAN *runtime_plan,
         FILE *diagnostic, DSL_PROGRAM_INTERFACE_RESULT *result)
{
    DSL_Program_Interface_Result_Init(result);
    std::vector<DSL_RUNTIME_RETURN_SITE> returns;
    if (result == NULL || program_plan == NULL || runtime_plan == NULL)
        return FALSE;
    std::string fingerprint = DSL_Program_Interface_Plan_Fingerprint
                                  (program_plan, runtime_plan);
    BOOL already_applied = DSL_Program_Interface_Image_Has_Records();
    if ((already_applied &&
         (DSL_program_interface_prepared_plan.empty() ||
          DSL_program_interface_prepared_plan != fingerprint)) ||
        (!already_applied &&
         !DSL_Program_Interface_Plan_Validate
              (program_plan, runtime_plan, diagnostic)) ||
        !DSL_Program_Interface_Preflight_PU
             (pu, program_plan, runtime_plan, &returns, diagnostic)) {
        if (already_applied &&
            DSL_program_interface_prepared_plan != fingerprint)
            DSL_Program_Interface_Report
                (diagnostic, "prepared plan does not match", 0);
        return FALSE;
    }
    if (!already_applied)
        DSL_program_interface_prepared_plan = fingerprint;
    if (!DSL_Program_Interface_Install_Inputs(program_plan))
        return DSL_Program_Interface_Report
                   (diagnostic, "could not install runtime inputs", 0);

    ST_IDX owner_pu_st = PU_Info_proc_sym(pu);
    WN *old_entry = PU_Info_tree_ptr(pu);
    std::vector<DSL_PROGRAM_CREATED_VALUE> created_values;
    std::vector<DSL_PROGRAM_CREATED_BINDING> created_bindings;
    std::vector<UINT32> value_indexes;
    std::vector<UINT32> binding_indexes;

    for (UINT32 i = 0; i < runtime_plan->value_count; ++i) {
        if (runtime_plan->values[i].owner_pu_st == owner_pu_st)
            value_indexes.push_back(i);
    }
    std::sort(value_indexes.begin(), value_indexes.end(),
              DSL_PROGRAM_RUNTIME_VALUE_INDEX_LESS(runtime_plan));
    for (UINT32 order = 0; order < value_indexes.size(); ++order) {
        const DSL_RUNTIME_VALUE_PROJECTION_REQUEST &request =
            runtime_plan->values[value_indexes[order]];
        DSL_PROGRAM_CREATED_VALUE value;
        value.request = &request;
        value.handle_st = DSL_Runtime_Interface_Create_Handle_ST(request);
        value.projection_id = DSL_RUNTIME_VALUE_PROJECTION_INVALID_ID;
        created_values.push_back(value);
    }
    for (UINT32 i = 0;
         i < program_plan->runtime_input_binding_count; ++i) {
        if (program_plan->runtime_input_bindings[i].owner_pu_st ==
            owner_pu_st)
            binding_indexes.push_back(i);
    }
    std::sort(binding_indexes.begin(), binding_indexes.end(),
              DSL_PROGRAM_BINDING_INDEX_LESS(program_plan));
    for (UINT32 order = 0; order < binding_indexes.size(); ++order) {
        UINT32 i = binding_indexes[order];
        const DSL_RUNTIME_INPUT_BINDING_REQUEST &request =
            program_plan->runtime_input_bindings[i];
        DSL_PROGRAM_CREATED_BINDING binding;
        binding.request_index = i;
        binding.handle_st =
            DSL_Program_Interface_Create_Binding_ST(request);
        binding.final_formal_ordinal =
            DSL_Program_Interface_Binding_Ordinal(program_plan, i);
        created_bindings.push_back(binding);
    }

    UINT32 final_formal_count =
        DSL_Program_Interface_Live_Formal_Count(program_plan, owner_pu_st) +
        created_bindings.size();
    std::vector<ST_IDX> formal_sts(final_formal_count, ST_IDX_ZERO);
    for (UINT32 i = 1; i <= DSL_PU_Interface_Image_Formal_Count(); ++i) {
        DSL_PU_FORMAL_RECORD formal;
        if (!DSL_PU_Interface_Image_Get_Formal(i, &formal) ||
            formal.owner_pu_st != owner_pu_st ||
            DSL_Program_Interface_Find_Retired_Formal_Request
                (program_plan, formal.id) != NULL)
            continue;
        const DSL_PROGRAM_CREATED_VALUE *value =
            DSL_Program_Interface_Find_Created_Value
                (created_values, formal.formal_value_id);
        UINT32 ordinal = formal.formal_ordinal -
            DSL_Program_Interface_Retired_Formals_Before
                (program_plan, owner_pu_st, formal.formal_ordinal);
        FmtAssert(value != NULL && ordinal < formal_sts.size(),
                  ("preflighted live formal is missing"));
        formal_sts[ordinal] = value->handle_st;
    }
    for (UINT32 i = 0; i < created_bindings.size(); ++i) {
        FmtAssert(created_bindings[i].final_formal_ordinal <
                      formal_sts.size(),
                  ("preflighted runtime binding ordinal is invalid"));
        formal_sts[created_bindings[i].final_formal_ordinal] =
            created_bindings[i].handle_st;
    }
    for (UINT32 i = 0; i < formal_sts.size(); ++i)
        FmtAssert(formal_sts[i] != ST_IDX_ZERO,
                  ("preflighted final formal is missing"));

    WN *entry = WN_CreateEntry
                    ((INT16)formal_sts.size(), owner_pu_st,
                     WN_func_body(old_entry), WN_func_pragmas(old_entry),
                     WN_func_varrefs(old_entry));
    for (UINT32 i = 0; i < formal_sts.size(); ++i)
        WN_formal(entry, i) = WN_CreateIdname(0, formal_sts[i]);
    TY_IDX function_ty = DSL_Program_Interface_Function_TY(formal_sts);
    FmtAssert(function_ty != TY_IDX_ZERO,
              ("preflighted program interface prototype failed"));
    Set_PU_prototype(Pu_Table[ST_pu(St_Table[owner_pu_st])], function_ty);
    Set_PU_Info_tree_ptr(pu, entry);

    for (UINT32 callsite_id = 1;
         callsite_id <= DSL_Call_Image_Callsite_Count(); ++callsite_id) {
        DSL_CALLSITE_METADATA_RECORD callsite;
        if (!DSL_Call_Image_Get_Callsite(callsite_id, &callsite) ||
            callsite.owner_pu_st != owner_pu_st)
            continue;
        WN *old_call = const_cast<WN *>
                           (DSL_Call_Image_Get_Call_WN(callsite_id));
        UINT32 final_count =
            DSL_Program_Interface_Live_Formal_Count
                (program_plan, callsite.callee_pu_st);
        for (UINT32 i = 0;
             i < program_plan->runtime_input_binding_count; ++i) {
            if (program_plan->runtime_input_bindings[i].owner_pu_st ==
                callsite.callee_pu_st)
                ++final_count;
        }
        WN *new_call = WN_Create
                           (OPR_CALL, WN_rtype(old_call), WN_desc(old_call),
                            final_count);
        WN_st_idx(new_call) = WN_st_idx(old_call);
        WN_call_flag(new_call) = WN_call_flag(old_call);
        WN_Set_Linenum(new_call, WN_Get_Linenum(old_call));

        for (UINT32 old_ordinal = 0;
             old_ordinal < (UINT32)WN_kid_count(old_call); ++old_ordinal) {
            DSL_CALL_ARGUMENT_RECORD argument;
            BOOL has_argument = DSL_Call_ABI_Image_Find_Argument_By_Id
                                    (callsite_id, old_ordinal, &argument);
            if (has_argument &&
                DSL_Program_Interface_Find_Retired_Call_Request
                    (program_plan, argument.id) != NULL)
                continue;
            const DSL_RUNTIME_CALL_PROJECTION_REQUEST *request =
                DSL_Program_Interface_Find_Runtime_Call
                    (runtime_plan, callsite_id, old_ordinal);
            FmtAssert(request != NULL,
                      ("preflighted live call projection is missing"));
            const DSL_PROGRAM_CREATED_VALUE *value =
                DSL_Program_Interface_Find_Created_Value
                    (created_values, request->source_value_id);
            FmtAssert(value != NULL,
                      ("preflighted call value is missing"));
            UINT32 final_ordinal = old_ordinal -
                DSL_Program_Interface_Retired_Formals_Before
                    (program_plan, callsite.callee_pu_st, old_ordinal);
            WN_kid(new_call, final_ordinal) =
                DSL_Program_Interface_Create_Call_Parm
                    (*value, request->direction);
        }
        for (UINT32 i = 0;
             i < program_plan->runtime_input_call_count; ++i) {
            const DSL_RUNTIME_INPUT_CALL_REQUEST &request =
                program_plan->runtime_input_calls[i];
            if (request.callsite_id != callsite_id)
                continue;
            const DSL_PROGRAM_CREATED_BINDING *caller =
                DSL_Program_Interface_Find_Created_Binding
                    (created_bindings, request.caller_binding_index);
            UINT32 ordinal = DSL_Program_Interface_Binding_Ordinal
                                 (program_plan,
                                  request.callee_binding_index);
            FmtAssert(caller != NULL && ordinal < final_count,
                      ("preflighted runtime call binding is missing"));
            WN_kid(new_call, ordinal) =
                DSL_Program_Interface_Create_Binding_Parm
                    (*caller, program_plan);
        }
        for (UINT32 i = 0; i < final_count; ++i)
            FmtAssert(WN_kid(new_call, i) != NULL,
                      ("preflighted final call actual is missing"));
        WN *parent = DSL_Runtime_Interface_Parent_Block(entry, old_call);
        FmtAssert(parent != NULL,
                  ("preflighted call parent is missing"));
        WN_INSERT_BlockBefore(parent, old_call, new_call);
        FmtAssert(DSL_Call_Image_Replace_Call_WN
                      (callsite_id, old_call, new_call),
                  ("callsite runtime association update failed"));
        WN_DELETE_FromBlock(parent, old_call);
        ++result->rewritten_call_count;
    }

    for (UINT32 i = 0; i < returns.size(); ++i) {
        const DSL_PROGRAM_CREATED_VALUE *target =
            DSL_Program_Interface_Find_Created_Value
                (created_values,
                 returns[i].result_formal->source_value_id);
        const DSL_PROGRAM_CREATED_VALUE *source =
            DSL_Program_Interface_Find_Created_Value
                (created_values, returns[i].source_value->source_value_id);
        FmtAssert(target != NULL && source != NULL,
                  ("preflighted return projection is missing"));
        TYPE_ID mtype = TY_mtype(source->request->handle_ty);
        WN *load = WN_CreateLdid
                       (OPR_LDID, mtype, mtype, 0, source->handle_st,
                        source->request->handle_ty);
        WN *store = WN_CreateStid
                        (OPR_STID, MTYPE_V, mtype, 0, target->handle_st,
                         target->request->handle_ty, load);
        WN_Set_Linenum(store, WN_Get_Linenum(returns[i].store));
        WN_INSERT_BlockBefore
            (returns[i].parent_block, returns[i].store, store);
        WN_DELETE_FromBlock(returns[i].parent_block, returns[i].store);
        ++result->rewritten_return_count;
    }

    for (UINT32 i = 0; i < created_values.size(); ++i) {
        DSL_RUNTIME_VALUE_PROJECTION_RECORD record;
        memset(&record, 0, sizeof(record));
        record.owner_pu_st = owner_pu_st;
        record.source_value_id = created_values[i].request->source_value_id;
        record.source_st = created_values[i].request->expected_source_st;
        record.source_ty = created_values[i].request->expected_source_ty;
        record.handle_st = created_values[i].handle_st;
        record.handle_ty = created_values[i].request->handle_ty;
        record.binding_kind = created_values[i].request->binding_kind;
        record.formal_ordinal = created_values[i].request->formal_ordinal;
        created_values[i].projection_id =
            DSL_Runtime_Interface_Image_Add_Value(&record);
        ++result->canonical_projection_count;
    }
    std::vector<UINT32> projected_call_indexes;
    for (UINT32 i = 0; i < runtime_plan->call_count; ++i) {
        if (runtime_plan->calls[i].owner_pu_st == owner_pu_st)
            projected_call_indexes.push_back(i);
    }
    std::sort(projected_call_indexes.begin(), projected_call_indexes.end(),
              DSL_PROGRAM_RUNTIME_CALL_INDEX_LESS(runtime_plan));
    for (UINT32 order = 0; order < projected_call_indexes.size(); ++order) {
        UINT32 i = projected_call_indexes[order];
        const DSL_RUNTIME_CALL_PROJECTION_REQUEST &request =
            runtime_plan->calls[i];
        const DSL_PROGRAM_CREATED_VALUE *value =
            DSL_Program_Interface_Find_Created_Value
                (created_values, request.source_value_id);
        DSL_RUNTIME_CALL_PROJECTION_RECORD record;
        memset(&record, 0, sizeof(record));
        record.owner_pu_st = owner_pu_st;
        record.callsite_id = request.callsite_id;
        record.value_projection_id = value->projection_id;
        record.source_value_id = request.source_value_id;
        record.actual_ordinal = request.actual_ordinal;
        record.callee_formal_ordinal = request.callee_formal_ordinal;
        record.direction = request.direction;
        DSL_Runtime_Interface_Image_Add_Call(&record);
    }

    std::vector<UINT32> retired_formal_indexes;
    for (UINT32 i = 0; i < program_plan->retired_formal_count; ++i)
        retired_formal_indexes.push_back(i);
    std::sort(retired_formal_indexes.begin(), retired_formal_indexes.end(),
              DSL_PROGRAM_RETIRED_FORMAL_INDEX_LESS(program_plan));
    for (UINT32 order = 0; order < retired_formal_indexes.size(); ++order) {
        UINT32 i = retired_formal_indexes[order];
        DSL_PU_FORMAL_RECORD formal;
        if (!DSL_PU_Interface_Image_Get_Formal
                (program_plan->retired_formals[i].pu_formal_id, &formal) ||
            formal.owner_pu_st != owner_pu_st)
            continue;
        DSL_RETIRED_FORMAL_RECORD record;
        memset(&record, 0, sizeof(record));
        record.pu_formal_id = formal.id;
        record.owner_pu_st = owner_pu_st;
        record.formal_value_id = formal.formal_value_id;
        record.formal_st = formal.formal_st;
        record.formal_ty = formal.formal_ty;
        record.old_formal_ordinal = formal.formal_ordinal;
        record.retirement_reason =
            DSL_INTERFACE_RETIREMENT_VERIFIED_DEAD_INPUT;
        record.semantic_role =
            Save_Str(program_plan->retired_formals[i].semantic_role);
        DSL_Program_Interface_Image_Add_Retired_Formal(&record);
        ++result->retired_formal_count;
    }
    std::vector<UINT32> retired_call_indexes;
    for (UINT32 i = 0;
         i < program_plan->retired_call_argument_count; ++i)
        retired_call_indexes.push_back(i);
    std::sort(retired_call_indexes.begin(), retired_call_indexes.end(),
              DSL_PROGRAM_RETIRED_CALL_INDEX_LESS(program_plan));
    for (UINT32 order = 0; order < retired_call_indexes.size(); ++order) {
        UINT32 i = retired_call_indexes[order];
        DSL_CALL_ARGUMENT_RECORD argument;
        DSL_CALLSITE_METADATA_RECORD callsite;
        if (!DSL_Call_ABI_Image_Get_Argument
                (program_plan->retired_call_arguments[i].call_argument_id,
                 &argument) ||
            !DSL_Call_Image_Get_Callsite(argument.callsite_id, &callsite) ||
            callsite.owner_pu_st != owner_pu_st)
            continue;
        DSL_RETIRED_CALL_ARGUMENT_RECORD record;
        memset(&record, 0, sizeof(record));
        record.call_argument_id = argument.id;
        record.callsite_id = argument.callsite_id;
        record.argument_value_id = argument.argument_value_id;
        record.old_actual_ordinal = argument.actual_ordinal;
        record.old_callee_formal_ordinal = argument.callee_formal_ordinal;
        record.retirement_reason =
            DSL_INTERFACE_RETIREMENT_VERIFIED_DEAD_INPUT;
        record.semantic_role = Save_Str
            (program_plan->retired_call_arguments[i].semantic_role);
        DSL_Program_Interface_Image_Add_Retired_Call(&record);
        ++result->retired_call_argument_count;
    }
    for (UINT32 i = 0; i < created_bindings.size(); ++i) {
        const DSL_RUNTIME_INPUT_BINDING_REQUEST &request =
            program_plan->runtime_input_bindings
                [created_bindings[i].request_index];
        DSL_RUNTIME_INPUT_BINDING_RECORD record;
        memset(&record, 0, sizeof(record));
        record.owner_pu_st = owner_pu_st;
        record.runtime_input_id = request.runtime_input_index ==
            DSL_PROGRAM_INTERFACE_INVALID_REQUEST_INDEX ? 0 :
            DSL_Program_Interface_Input_Id
                (program_plan, request.runtime_input_index);
        record.handle_st = created_bindings[i].handle_st;
        record.handle_ty = request.handle_ty;
        record.final_formal_ordinal =
            created_bindings[i].final_formal_ordinal;
        record.binding_kind = request.binding_kind;
        record.semantic_role = Save_Str(request.semantic_role);
        DSL_Program_Interface_Image_Add_Runtime_Binding(&record);
        ++result->runtime_binding_count;
    }
    std::vector<UINT32> input_call_indexes;
    for (UINT32 i = 0;
         i < program_plan->runtime_input_call_count; ++i)
        input_call_indexes.push_back(i);
    std::sort(input_call_indexes.begin(), input_call_indexes.end(),
              DSL_PROGRAM_INPUT_CALL_INDEX_LESS(program_plan));
    for (UINT32 order = 0; order < input_call_indexes.size(); ++order) {
        UINT32 i = input_call_indexes[order];
        const DSL_RUNTIME_INPUT_CALL_REQUEST &request =
            program_plan->runtime_input_calls[i];
        DSL_CALLSITE_METADATA_RECORD callsite;
        if (!DSL_Call_Image_Get_Callsite(request.callsite_id, &callsite) ||
            callsite.owner_pu_st != owner_pu_st)
            continue;
        const DSL_RUNTIME_INPUT_BINDING_REQUEST &caller =
            program_plan->runtime_input_bindings
                [request.caller_binding_index];
        const DSL_RUNTIME_INPUT_BINDING_REQUEST &callee =
            program_plan->runtime_input_bindings
                [request.callee_binding_index];
        DSL_RUNTIME_INPUT_CALL_RECORD record;
        memset(&record, 0, sizeof(record));
        record.callsite_id = request.callsite_id;
        record.caller_owner_pu_st = caller.owner_pu_st;
        record.caller_final_formal_ordinal =
            DSL_Program_Interface_Binding_Ordinal
                (program_plan, request.caller_binding_index);
        record.callee_owner_pu_st = callee.owner_pu_st;
        record.callee_final_formal_ordinal =
            DSL_Program_Interface_Binding_Ordinal
                (program_plan, request.callee_binding_index);
        record.final_actual_ordinal =
            record.callee_final_formal_ordinal;
        record.final_callee_formal_ordinal =
            record.callee_final_formal_ordinal;
        record.handle_ty = callee.handle_ty;
        record.semantic_role = Save_Str(callee.semantic_role);
        DSL_Program_Interface_Image_Add_Runtime_Call(&record);
        ++result->runtime_call_count;
    }
    result->runtime_input_count = program_plan->runtime_input_count;
    return TRUE;
}

static UINT32
DSL_Program_Interface_Image_Retired_Before
        (ST_IDX owner_pu_st, UINT32 old_ordinal)
{
    UINT32 count = 0;
    for (UINT32 i = 1;
         i <= DSL_Program_Interface_Image_Retired_Formal_Count(); ++i) {
        DSL_RETIRED_FORMAL_RECORD retired;
        if (DSL_Program_Interface_Image_Get_Retired_Formal(i, &retired) &&
            retired.owner_pu_st == owner_pu_st &&
            retired.old_formal_ordinal < old_ordinal)
            ++count;
    }
    return count;
}

static UINT32
DSL_Program_Interface_Image_Retired_Call_Before
        (DSL_CALLSITE_METADATA_ID callsite_id, UINT32 old_ordinal)
{
    UINT32 count = 0;
    for (UINT32 i = 1;
         i <= DSL_Program_Interface_Image_Retired_Call_Count(); ++i) {
        DSL_RETIRED_CALL_ARGUMENT_RECORD retired;
        if (DSL_Program_Interface_Image_Get_Retired_Call(i, &retired) &&
            retired.callsite_id == callsite_id &&
            retired.old_actual_ordinal < old_ordinal)
            ++count;
    }
    return count;
}

static BOOL
DSL_Program_Interface_Parm_Matches_Handle
        (const WN *parm, ST_IDX handle_st, TY_IDX handle_ty,
         UINT32 direction)
{
    const WN *actual = parm == NULL || WN_operator(parm) != OPR_PARM ?
                       NULL : WN_kid0(parm);
    if (direction == DSL_RUNTIME_CALL_INPUT)
        return actual != NULL && WN_operator(actual) == OPR_LDID &&
               WN_st_idx(actual) == handle_st && WN_ty(parm) == handle_ty &&
               WN_ty(actual) == handle_ty &&
               WN_parm_flag(parm) ==
                   (WN_PARM_BY_VALUE | WN_PARM_READ_ONLY |
                    WN_PARM_PASSED_NOT_SAVED);
    return actual != NULL && WN_operator(actual) == OPR_LDA &&
           WN_st_idx(actual) == handle_st && WN_ty(parm) == WN_ty(actual) &&
           TY_kind(WN_ty(parm)) == KIND_POINTER &&
           TY_pointed(WN_ty(parm)) == handle_ty &&
           WN_parm_flag(parm) ==
               (WN_PARM_BY_REFERENCE | WN_PARM_OUT |
                WN_PARM_PASSED_NOT_SAVED);
}

BOOL
DSL_Program_Interface_Validate_PU (PU_Info *pu, FILE *diagnostic)
{
    if (pu == NULL || PU_Info_tree_ptr(pu) == NULL ||
        !DSL_IR_Image_Current_PU_Is(PU_Info_proc_sym(pu)))
        return DSL_Program_Interface_Report
                   (diagnostic, "program unit is not active", 0);
    ST_IDX owner_pu_st = PU_Info_proc_sym(pu);
    WN *entry = PU_Info_tree_ptr(pu);
    if (WN_operator(entry) != OPR_FUNC_ENTRY)
        return DSL_Program_Interface_Report
                   (diagnostic, "program unit has no FUNC_ENTRY", 0);

    UINT32 expected_formals = 0;
    for (UINT32 i = 1; i <= DSL_PU_Interface_Image_Formal_Count(); ++i) {
        DSL_PU_FORMAL_RECORD formal;
        if (!DSL_PU_Interface_Image_Get_Formal(i, &formal) ||
            formal.owner_pu_st != owner_pu_st)
            continue;
        DSL_RETIRED_FORMAL_RECORD retired;
        if (DSL_Program_Interface_Image_Find_Retired_Formal
                (formal.id, &retired)) {
            if (DSL_Program_Interface_Tree_Uses_ST
                    (WN_func_body(entry), formal.formal_st))
                return DSL_Program_Interface_Report
                           (diagnostic, "retired formal remains executable",
                            formal.id);
            continue;
        }
        DSL_RUNTIME_VALUE_PROJECTION_RECORD projection;
        UINT32 ordinal = formal.formal_ordinal -
            DSL_Program_Interface_Image_Retired_Before
                (owner_pu_st, formal.formal_ordinal);
        if (!DSL_Runtime_Interface_Image_Find_Value
                (owner_pu_st, formal.formal_value_id, &projection) ||
            ordinal >= WN_num_formals(entry) ||
            WN_st_idx(WN_formal(entry, ordinal)) != projection.handle_st)
            return DSL_Program_Interface_Report
                       (diagnostic, "live formal projection mismatch",
                        formal.id);
        ++expected_formals;
    }

    for (UINT32 i = 1;
         i <= DSL_Program_Interface_Image_Runtime_Binding_Count(); ++i) {
        DSL_RUNTIME_INPUT_BINDING_RECORD binding;
        if (!DSL_Program_Interface_Image_Get_Runtime_Binding(i, &binding) ||
            binding.owner_pu_st != owner_pu_st)
            continue;
        if (binding.final_formal_ordinal >= WN_num_formals(entry) ||
            WN_st_idx(WN_formal(entry, binding.final_formal_ordinal)) !=
                binding.handle_st ||
            ST_IDX_level(binding.handle_st) != CURRENT_SYMTAB ||
            ST_IDX_index(binding.handle_st) == 0 ||
            ST_IDX_index(binding.handle_st) >=
                ST_Table_Size(CURRENT_SYMTAB) ||
            ST_type(St_Table[binding.handle_st]) != binding.handle_ty ||
            ST_sclass(St_Table[binding.handle_st]) != SCLASS_FORMAL ||
            ST_Srcpos(St_Table[binding.handle_st]) == 0)
            return DSL_Program_Interface_Report
                       (diagnostic, "runtime binding formal mismatch", i);
        ++expected_formals;
    }
    if (expected_formals != WN_num_formals(entry))
        return DSL_Program_Interface_Report
                   (diagnostic, "incomplete final formal interface", 0);

    for (UINT32 callsite_id = 1;
         callsite_id <= DSL_Call_Image_Callsite_Count(); ++callsite_id) {
        DSL_CALLSITE_METADATA_RECORD callsite;
        if (!DSL_Call_Image_Get_Callsite(callsite_id, &callsite) ||
            callsite.owner_pu_st != owner_pu_st)
            continue;
        const WN *call = DSL_Call_Image_Get_Call_WN(callsite_id);
        if (call == NULL || WN_operator(call) != OPR_CALL)
            return DSL_Program_Interface_Report
                       (diagnostic, "final call is unavailable", callsite_id);
        UINT32 expected_actuals = 0;
        for (UINT32 i = 1; i <= DSL_Call_ABI_Image_Argument_Count(); ++i) {
            DSL_CALL_ARGUMENT_RECORD argument;
            if (!DSL_Call_ABI_Image_Get_Argument(i, &argument) ||
                argument.callsite_id != callsite_id)
                continue;
            DSL_RETIRED_CALL_ARGUMENT_RECORD retired;
            if (DSL_Program_Interface_Image_Find_Retired_Call
                    (argument.id, &retired))
                continue;
            DSL_RUNTIME_CALL_PROJECTION_RECORD runtime_call;
            DSL_RUNTIME_VALUE_PROJECTION_RECORD value;
            UINT32 ordinal = argument.actual_ordinal -
                DSL_Program_Interface_Image_Retired_Call_Before
                    (callsite_id, argument.actual_ordinal);
            if (!DSL_Runtime_Interface_Image_Find_Call
                    (callsite_id, argument.actual_ordinal, &runtime_call) ||
                !DSL_Runtime_Interface_Image_Get_Value
                    (runtime_call.value_projection_id, &value) ||
                ordinal >= WN_kid_count(call) ||
                !DSL_Program_Interface_Parm_Matches_Handle
                    (WN_kid(call, ordinal), value.handle_st,
                     value.handle_ty, runtime_call.direction))
                return DSL_Program_Interface_Report
                           (diagnostic, "live call projection mismatch",
                            argument.id);
            ++expected_actuals;
        }
        for (UINT32 i = 1;
             i <= DSL_Program_Interface_Image_Runtime_Call_Count(); ++i) {
            DSL_RUNTIME_INPUT_CALL_RECORD runtime_call;
            if (!DSL_Program_Interface_Image_Get_Runtime_Call
                    (i, &runtime_call) ||
                runtime_call.callsite_id != callsite_id)
                continue;
            DSL_RUNTIME_INPUT_BINDING_RECORD caller;
            if (!DSL_Program_Interface_Image_Find_Runtime_Binding
                    (runtime_call.caller_owner_pu_st,
                     runtime_call.caller_final_formal_ordinal, &caller) ||
                runtime_call.final_actual_ordinal >= WN_kid_count(call) ||
                !DSL_Program_Interface_Parm_Matches_Handle
                    (WN_kid(call, runtime_call.final_actual_ordinal),
                     caller.handle_st, caller.handle_ty,
                     DSL_RUNTIME_CALL_INPUT))
                return DSL_Program_Interface_Report
                           (diagnostic, "threaded runtime call mismatch", i);
            ++expected_actuals;
        }
        /* Hidden result projections are not represented in call-ABI rows. */
        for (UINT32 i = 1;
             i <= DSL_Runtime_Interface_Image_Call_Count(); ++i) {
            DSL_RUNTIME_CALL_PROJECTION_RECORD runtime_call;
            if (!DSL_Runtime_Interface_Image_Get_Call(i, &runtime_call) ||
                runtime_call.callsite_id != callsite_id ||
                runtime_call.direction != DSL_RUNTIME_CALL_RESULT)
                continue;
            DSL_RUNTIME_VALUE_PROJECTION_RECORD value;
            UINT32 ordinal = runtime_call.actual_ordinal -
                DSL_Program_Interface_Image_Retired_Call_Before
                    (callsite_id, runtime_call.actual_ordinal);
            if (!DSL_Runtime_Interface_Image_Get_Value
                    (runtime_call.value_projection_id, &value) ||
                ordinal >= WN_kid_count(call) ||
                !DSL_Program_Interface_Parm_Matches_Handle
                    (WN_kid(call, ordinal), value.handle_st,
                     value.handle_ty, DSL_RUNTIME_CALL_RESULT))
                return DSL_Program_Interface_Report
                           (diagnostic, "runtime result call mismatch", i);
            ++expected_actuals;
        }
        if (expected_actuals != WN_kid_count(call))
            return DSL_Program_Interface_Report
                       (diagnostic, "incomplete final call interface",
                        callsite_id);
    }
    return TRUE;
}

BOOL
DSL_Program_Interface_Validate_Lowered_PU
        (PU_Info *pu, FILE *diagnostic)
{
    if (!DSL_Program_Interface_Validate_PU(pu, diagnostic))
        return FALSE;
    ST_IDX owner_pu_st = PU_Info_proc_sym(pu);
    WN *entry = PU_Info_tree_ptr(pu);
    for (UINT32 i = 1;
         i <= DSL_Program_Interface_Image_Runtime_Input_Count(); ++i) {
        DSL_RUNTIME_INPUT_RECORD input;
        if (!DSL_Program_Interface_Image_Get_Runtime_Input(i, &input) ||
            input.input_kind != DSL_RUNTIME_INPUT_SOURCE_EXTERNAL_TENSOR ||
            input.source_owner_pu_st != owner_pu_st)
            continue;
        if (DSL_Program_Interface_Tree_Uses_ST(entry, input.source_st))
            return DSL_Program_Interface_Report
                       (diagnostic,
                        "promoted source definition remains executable", i);
    }
    if (DSL_Runtime_Interface_Tree_Uses_Source_ST(entry, owner_pu_st))
        return DSL_Program_Interface_Report
                   (diagnostic, "canonical tensor source remains executable",
                    0);
    return TRUE;
}
