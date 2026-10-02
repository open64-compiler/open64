/*
 * Copyright (C) 2026 Open64 Project
 *
 * Runtime handle, formal, call, and return projection transactions for native
 * DSL values. See doc/FHE-SYNC5-RUNTIME-ENTRY-BINDING-GAP.md,
 * doc/FHE-RUNTIME-C-ABI-V1-CONTRACT.md, and
 * doc/FHE-SYNC5-NATIVE-OWNERSHIP-AUDIT.md.
 */

#include <string.h>
#include <algorithm>
#include <string>
#include <vector>

#include "dsl_ir_image.h"
#include "dsl_ir_transaction_internal.h"
#include "dsl_runtime_interface_internal.h"
#include "pu_info.h"
#include "strtab.h"
#include "symtab.h"
#include "wn.h"
#include "wn_util.h"

/* Commit-only image services used after complete runtime-plan preflight. */
extern DSL_RUNTIME_VALUE_PROJECTION_ID
DSL_Runtime_Interface_Image_Add_Value
                                (const DSL_RUNTIME_VALUE_PROJECTION_RECORD *);
extern DSL_RUNTIME_CALL_PROJECTION_ID
DSL_Runtime_Interface_Image_Add_Call
                                (const DSL_RUNTIME_CALL_PROJECTION_RECORD *);

/*
 * Bind a value row to one global PU identity before local ST access. This is a
 * read-only preflight predicate shared by projection and program-interface
 * validation; it rejects free-form metadata that does not resolve uniquely.
 */
BOOL
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

/* Emit the stable runtime-interface diagnostic shape and return FALSE. */
static BOOL
DSL_Runtime_Interface_Report
        (FILE *diagnostic, const char *message, UINT32 id)
{
    if (diagnostic != NULL)
        fprintf(diagnostic, "DSL runtime interface error: %s id=%u\n",
                message, id);
    return FALSE;
}

/*
 * Find the unique planned projection for one owner/value pair. Plan validation
 * establishes uniqueness, so callers borrow the request without mutation.
 */
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

/*
 * Validate the complete cross-PU projection plan before any PU is rewritten.
 * It checks value/formal/call coverage, ownership, tensor/handle types, and
 * direction contracts; failure leaves WHIRL, symbols, and images unchanged.
 */
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

/* Initialize caller-owned result counters to the empty transaction state. */
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

/* Resolve a source value to the handle symbol created during PU commit. */
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

/*
 * Locate the physical parent BLOCK of target in the active PU tree. The
 * returned pointer is borrowed and is used only after preflight proves target
 * belongs to that tree.
 */
WN *
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

/*
 * Resolve an active local source ST to its structured DSL value row. This
 * helper performs no mutation and never treats a colliding local ST as global.
 */
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

/*
 * Preflight projected result-formal stores and journal their source/target
 * relations. No tree is changed; unsupported return shapes fail the whole PU.
 */
BOOL
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

/*
 * Build and canonicalize the projected function prototype from ordered PU
 * formal records and validated handle types. It allocates TY/TYLIST state only
 * during the commit phase and returns TY_IDX_ZERO on an incomplete contract.
 */
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

/*
 * Create one owner-local runtime handle symbol from a validated projection.
 * The caller owns commit ordering; source position and formal storage class
 * are preserved from the canonical tensor source.
 */
ST_IDX
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

/*
 * Validate one active PU against the already validated global plan, including
 * physical formals, calls, and return stores. It only fills a return journal;
 * failure performs no physical or image mutation.
 */
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

/*
 * Commit runtime handle projection for one active PU after global and local
 * preflight. It rebuilds the FUNC_ENTRY prototype, rewrites call/return ABI,
 * then publishes projection rows and result counters as one phase operation.
 */
BOOL
DSL_Runtime_Interface_Apply_PU
        (PU_Info *pu, const DSL_RUNTIME_INTERFACE_PLAN *plan,
         FILE *diagnostic, DSL_RUNTIME_INTERFACE_RESULT *result)
{
    DSL_Runtime_Interface_Result_Init(result);
    std::vector<DSL_RUNTIME_RETURN_SITE> returns;
    if (result == NULL || plan == NULL)
        return FALSE;
    TY_IDX void_ty = MTYPE_To_TY(MTYPE_V);
    if (void_ty == TY_IDX_ZERO ||
        TY_IDX_index(void_ty) >= TY_Table_Size() ||
        TY_kind(void_ty) != KIND_VOID)
        return DSL_Runtime_Interface_Report
                   (diagnostic, "predefined void type is not initialized", 0);
    if (!DSL_Runtime_Interface_Plan_Validate(plan, diagnostic) ||
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

/*
 * Recursively detect executable references to canonical tensor source symbols
 * that should have been replaced by runtime handles. This internal helper is
 * shared with the final program-interface postcondition.
 */
BOOL
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

/*
 * Verify a committed PU against persisted runtime projection rows. It checks
 * the rebuilt prototype, handle symbols, calls, returns, and absence of source
 * tensor uses without modifying either WHIRL or image state.
 */
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
    TY_IDX void_ty = MTYPE_To_TY(MTYPE_V);
    if (void_ty == TY_IDX_ZERO ||
        TY_IDX_index(void_ty) >= TY_Table_Size() ||
        TY_kind(void_ty) != KIND_VOID ||
        TYLIST_type(Tylist_Table[tylist]) != void_ty)
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
