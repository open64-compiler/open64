/*
 * Copyright (C) 2026 Open64 Project
 *
 * Domain-neutral physical PU cloning for program-scope DSL transactions.
 * The caller owns PU activation and the terminal checkpoint lifecycle.
 * See doc/FHE-SYNC6-PU-SPECIALIZATION-TRANSACTION.md.
 */

#include <string.h>

#include "clone.h"
#include "clone_DST_utils.h"
#include "const.h"
#include "cxx_memory.h"
#include "dsl_ir_image.h"
#include "dsl_ir_transaction_internal.h"
#include "dsl_pu_transaction.h"
#include "dsl_region.h"
#include "dsl_region_internal.h"
#include "dwarf_DST_mem.h"
#include "errors.h"
#include "mempool.h"
#include "strtab.h"
#include "symtab.h"
#include "targ_const.h"
#include "wn.h"
#include "wn_map.h"
#include "wn_util.h"

#include <string>
#include <vector>

static DSL_PU_TRANSACTION_POLICY DSL_pu_transaction_policy;

BOOL
DSL_PU_Transaction_Register_Policy
        (const DSL_PU_TRANSACTION_POLICY *policy)
{
    if (policy == NULL) {
        memset(&DSL_pu_transaction_policy, 0,
               sizeof(DSL_pu_transaction_policy));
        return TRUE;
    }
    if (DSL_pu_transaction_policy.build_plan != NULL ||
        policy->build_plan == NULL || policy->release == NULL)
        return FALSE;
    DSL_pu_transaction_policy = *policy;
    return TRUE;
}

BOOL
DSL_PU_Transaction_Get_Policy (DSL_PU_TRANSACTION_POLICY *policy)
{
    if (policy == NULL ||
        DSL_pu_transaction_policy.build_plan == NULL)
        return FALSE;
    *policy = DSL_pu_transaction_policy;
    return TRUE;
}

static BOOL
DSL_PU_Transaction_Report (FILE *diagnostic, const char *message)
{
    if (diagnostic != NULL)
        fprintf(diagnostic, "DSL PU transaction: %s\n", message);
    return FALSE;
}

static BOOL
DSL_PU_Transaction_Has_Unsupported_Call (WN *wn)
{
    if (wn == NULL)
        return FALSE;
    OPERATOR opr = WN_operator(wn);
    if (opr == OPR_CALL || opr == OPR_ICALL || opr == OPR_PICCALL ||
        opr == OPR_ALTENTRY)
        return TRUE;
    if (opr == OPR_BLOCK) {
        for (WN *stmt = WN_first(wn); stmt != NULL; stmt = WN_next(stmt)) {
            if (DSL_PU_Transaction_Has_Unsupported_Call(stmt))
                return TRUE;
        }
        return FALSE;
    }
    for (INT i = 0; i < WN_kid_count(wn); ++i) {
        if (DSL_PU_Transaction_Has_Unsupported_Call(WN_kid(wn, i)))
            return TRUE;
    }
    return FALSE;
}

static BOOL
DSL_PU_Transaction_Clone_Preflight
        (PU_Info *source, const char *clone_name,
         DSL_PU_CLONE_VALUE_PAIR *value_map,
         UINT32 value_map_capacity, FILE *diagnostic)
{
    if (source == NULL || clone_name == NULL || clone_name[0] == '\0' ||
        value_map == NULL || value_map_capacity == 0 ||
        PU_Info_state(source, WT_TREE) != Subsect_InMem ||
        PU_Info_state(source, WT_SYMTAB) != Subsect_InMem ||
        Current_pu != &PU_Info_pu(source) ||
        Current_Map_Tab != PU_Info_maptab(source) ||
        PU_Info_child(source) != NULL ||
        PU_Info_tree_ptr(source) == NULL ||
        WN_operator(PU_Info_tree_ptr(source)) != OPR_FUNC_ENTRY ||
        DSL_PU_Transaction_Has_Unsupported_Call
            (PU_Info_tree_ptr(source)))
        return DSL_PU_Transaction_Report
                   (diagnostic, "source is not a supported active PU");

    for (UINT32 i = 1; i < ST_Table_Size(GLOBAL_SYMTAB); ++i) {
        ST_IDX idx = make_ST_IDX(i, GLOBAL_SYMTAB);
        if (strcmp(ST_name(St_Table[idx]), clone_name) == 0)
            return DSL_PU_Transaction_Report
                       (diagnostic, "clone name already exists");
    }
    for (UINT32 i = 1; i < ST_Table_Size(CURRENT_SYMTAB); ++i) {
        ST_IDX idx = make_ST_IDX(i, CURRENT_SYMTAB);
        if (ST_sclass(St_Table[idx]) == SCLASS_PSTATIC)
            return DSL_PU_Transaction_Report
                       (diagnostic, "PU-local static needs promotion");
    }
    if (!DSL_IR_Image_Validate(diagnostic) ||
        !DSL_Call_Image_Validate(diagnostic) ||
        !DSL_PU_Interface_Image_Validate_PU(source, diagnostic) ||
        !DSL_Region_Verify_PU(source, diagnostic))
        return DSL_PU_Transaction_Report
                   (diagnostic, "source managed image is invalid");
    UINT32 source_value_count = 0;
    const char *source_name =
        ST_name(St_Table[PU_Info_proc_sym(source)]);
    for (UINT32 i = 1; i <= DSL_IR_Image_Value_Count(); ++i) {
        DSL_IR_VALUE_RECORD value;
        DSL_IR_VALUE_RECORD owned;
        if (!DSL_IR_Image_Get_Value(i, &value))
            return FALSE;
        if (value.name != STR_IDX_ZERO &&
            DSL_IR_Image_Find_PU_Value
                (value.st, Index_To_Str(value.name), source_name, &owned) &&
            owned.id == value.id)
            ++source_value_count;
    }
    if (source_value_count > value_map_capacity)
        return DSL_PU_Transaction_Report
                   (diagnostic, "value map capacity is insufficient");
    return TRUE;
}

BOOL
DSL_PU_Transaction_Clone_Active
        (PU_Info *source, const char *clone_name,
         DSL_PU_CLONE_VALUE_PAIR *value_map,
         UINT32 value_map_capacity, UINT32 *value_map_count,
         PU_Info **cloned, FILE *diagnostic)
{
    if (cloned != NULL)
        *cloned = NULL;
    if (value_map_count != NULL)
        *value_map_count = 0;
    if (cloned == NULL || value_map_count == NULL ||
        !DSL_PU_Transaction_Clone_Preflight
            (source, clone_name, value_map, value_map_capacity, diagnostic))
        return FALSE;

    ST_IDX source_idx = PU_Info_proc_sym(source);
    ST *source_st = ST_ptr(source_idx);
    STR_IDX name = Save_Str(clone_name);
    PU_IDX pu_idx;
    PU &pu = New_PU(pu_idx);
    pu = Pu_Table[ST_pu(source_st)];
    ST *clone_st = New_ST(GLOBAL_SYMTAB);
    ST_Init(clone_st, name, CLASS_FUNC, SCLASS_TEXT,
            EXPORT_LOCAL, pu_idx);
    Set_ST_Srcpos(*clone_st, ST_Srcpos(*source_st));

    if (PU_Info_symtab_ptr(source) == NULL)
        Save_Local_Symtab(CURRENT_SYMTAB, source);
    IPO_CLONE clone(PU_Info_tree_ptr(source), Scope_tab, CURRENT_SYMTAB,
                    PU_Info_maptab(source), Malloc_Mem_Pool,
                    Malloc_Mem_Pool);
    clone.New_Clone(clone_st);

    PU_Info *clone_info = CXX_NEW(PU_Info, Malloc_Mem_Pool);
    PU_Info_init(clone_info);
    Set_PU_Info_flags(clone_info, PU_IS_COMPILER_GENERATED);
    Set_PU_Info_tree_ptr(clone_info, clone.Get_Cloned_PU());
    PU_Info_proc_sym(clone_info) = ST_st_idx(*clone_st);
    PU_Info_maptab(clone_info) = clone.Get_Cloned_maptab();
    Set_PU_Info_state(clone_info, WT_TREE, Subsect_InMem);
    Set_PU_Info_state(clone_info, WT_SYMTAB, Subsect_InMem);
    Set_PU_Info_state(clone_info, WT_PROC_SYM, Subsect_InMem);
    Set_PU_Info_cu_dst(clone_info, PU_Info_cu_dst(source));

    Scope_tab[CURRENT_SYMTAB] =
        clone.Get_sym()->Get_cloned_scope_tab()[CURRENT_SYMTAB];
    Scope_tab[CURRENT_SYMTAB].st = clone_st;
    Current_pu = &PU_Info_pu(clone_info);
    Current_Map_Tab = PU_Info_maptab(clone_info);
    Set_PU_Info_pu_dst
        (clone_info,
         DST_enter_cloned_subroutine
             (DST_get_compile_unit(), PU_Info_pu_dst(source), clone_st,
              Current_DST, clone.Get_sym()));
    Set_PU_Info_symtab_ptr(clone_info, NULL);
    Save_Local_Symtab(CURRENT_SYMTAB, clone_info);

    DSL_PU_CLONE_IMAGE_SAVEPOINT savepoint;
    BOOL cloned_image = DSL_IR_Image_Clone_PU_Values
                            (source_idx, ST_name(*source_st),
                             PU_Info_proc_sym(clone_info),
                             ST_name(*clone_st), value_map,
                             value_map_capacity, value_map_count,
                             &savepoint);
    if (!cloned_image ||
        !DSL_Region_Clone_PU_Store(source, clone_info) ||
        !DSL_IR_Image_Validate(diagnostic) ||
        !DSL_PU_Interface_Image_Validate_PU(clone_info, diagnostic) ||
        !DSL_Region_Verify_PU(clone_info, diagnostic)) {
        DSL_Region_Discard_PU_Store(clone_info);
        if (cloned_image)
            DSL_IR_Image_Clone_PU_Restore(&savepoint);
        Restore_Local_Symtab(source);
        Current_pu = &PU_Info_pu(source);
        Current_Map_Tab = PU_Info_maptab(source);
        return DSL_PU_Transaction_Report
                   (diagnostic, "clone construction failed; stop process");
    }

    PU_Info_next(clone_info) = PU_Info_next(source);
    PU_Info_next(source) = clone_info;
    Restore_Local_Symtab(source);
    Current_pu = &PU_Info_pu(source);
    Current_Map_Tab = PU_Info_maptab(source);
    *cloned = clone_info;
    return TRUE;
}

static BOOL
DSL_PU_Transaction_Scalar_TY_Valid (TY_IDX ty)
{
    if (ty == TY_IDX_ZERO || TY_IDX_index(ty) >= TY_Table_Size())
        return FALSE;
    TYPE_ID mtype = TY_mtype(ty);
    return TY_kind(ty) == KIND_SCALAR &&
           (mtype == MTYPE_I4 || mtype == MTYPE_U4 ||
            mtype == MTYPE_I8 || mtype == MTYPE_U8 ||
            mtype == MTYPE_F4 || mtype == MTYPE_F8);
}

static PU_Info *
DSL_PU_Transaction_Find_PU (PU_Info *program, ST_IDX owner)
{
    for (PU_Info *pu = program; pu != NULL; pu = PU_Info_next(pu)) {
        if (PU_Info_proc_sym(pu) == owner)
            return pu;
    }
    return NULL;
}

static void
DSL_PU_Transaction_Activate (PU_Info *pu)
{
    Restore_Local_Symtab(pu);
    Current_pu = &PU_Info_pu(pu);
    Current_Map_Tab = PU_Info_maptab(pu);
}

static BOOL
DSL_PU_Transaction_Hex_SHA256_Valid (const char *digest)
{
    if (digest == NULL || strlen(digest) != 64)
        return FALSE;
    for (UINT32 i = 0; i < 64; ++i) {
        if ((digest[i] < '0' || digest[i] > '9') &&
            (digest[i] < 'a' || digest[i] > 'f'))
            return FALSE;
    }
    return TRUE;
}

static BOOL
DSL_PU_Transaction_Call_Routes_Complete
        (WN *tree, const DSL_PU_TRANSACTION_PLAN *plan)
{
    if (tree == NULL)
        return TRUE;
    if (WN_operator(tree) == OPR_ICALL ||
        WN_operator(tree) == OPR_PICCALL ||
        WN_operator(tree) == OPR_ALTENTRY)
        return FALSE;
    if (WN_operator(tree) == OPR_LDA) {
        for (UINT32 i = 0; i < plan->variant_count; ++i) {
            if (WN_st_idx(tree) == plan->variants[i].source_pu_st)
                return FALSE;
        }
    }
    if (WN_operator(tree) == OPR_CALL) {
        BOOL targeted = FALSE;
        for (UINT32 i = 0; i < plan->variant_count; ++i) {
            if (WN_st_idx(tree) == plan->variants[i].source_pu_st)
                targeted = TRUE;
        }
        if (targeted) {
            DSL_CALLSITE_METADATA_RECORD callsite;
            if (!DSL_Call_Image_Find_Callsite(tree, &callsite))
                return FALSE;
            UINT32 matches = 0;
            for (UINT32 i = 0; i < plan->route_count; ++i) {
                if (plan->routes[i].callsite_id == callsite.id)
                    ++matches;
            }
            if (matches != 1)
                return FALSE;
        }
    }
    if (WN_operator(tree) == OPR_BLOCK) {
        for (WN *stmt = WN_first(tree); stmt != NULL;
             stmt = WN_next(stmt)) {
            if (!DSL_PU_Transaction_Call_Routes_Complete(stmt, plan))
                return FALSE;
        }
        return TRUE;
    }
    for (INT i = 0; i < WN_kid_count(tree); ++i) {
        if (!DSL_PU_Transaction_Call_Routes_Complete
                (WN_kid(tree, i), plan))
            return FALSE;
    }
    return TRUE;
}

BOOL
DSL_PU_Transaction_Preflight_Resident
        (PU_Info *program, const DSL_PU_TRANSACTION_PLAN *plan,
         FILE *diagnostic)
{
    if (program == NULL || plan == NULL || plan->variants == NULL ||
        plan->variant_count == 0 ||
        (plan->route_count != 0 && plan->routes == NULL) ||
        !DSL_IR_Image_Validate(diagnostic) ||
        !DSL_Call_Image_Validate(diagnostic) ||
        !DSL_Call_ABI_Image_Validate(diagnostic) ||
        !DSL_PU_Interface_Image_Validate(diagnostic))
        return DSL_PU_Transaction_Report
                   (diagnostic, "invalid program plan or image");

    PU_Info *prior = NULL;
    for (PU_Info *pu = program; pu != NULL; pu = PU_Info_next(pu)) {
        if (Current_pu == &PU_Info_pu(pu))
            prior = pu;
        if (PU_Info_child(pu) != NULL ||
            PU_Info_state(pu, WT_TREE) != Subsect_InMem ||
            PU_Info_state(pu, WT_SYMTAB) != Subsect_InMem ||
            PU_Info_symtab_ptr(pu) == NULL ||
            PU_Info_maptab(pu) == NULL ||
            PU_Info_tree_ptr(pu) == NULL ||
            WN_operator(PU_Info_tree_ptr(pu)) != OPR_FUNC_ENTRY)
            return DSL_PU_Transaction_Report
                       (diagnostic, "program PU is not resident");
    }
    BOOL valid = TRUE;
    for (UINT32 i = 0; i < plan->variant_count && valid; ++i) {
        const DSL_PU_TRANSACTION_VARIANT_REQUEST &variant =
            plan->variants[i];
        PU_Info *source = DSL_PU_Transaction_Find_PU
                              (program, variant.source_pu_st);
        if (source == NULL || variant.signature_bytes == NULL ||
            variant.signature_size == 0 ||
            !DSL_PU_Transaction_Hex_SHA256_Valid
                (variant.signature_sha256) ||
            (variant.formal_count != 0 && variant.formals == NULL) ||
            (!variant.use_existing_pu &&
             (variant.clone_name == NULL ||
              variant.clone_name[0] == '\0'))) {
            valid = FALSE;
            break;
        }
        DSL_PU_Transaction_Activate(source);
        DSL_PU_SOURCE_IDENTITY_RECORD identity;
        if (!DSL_Call_Image_Find_PU_Identity
                (variant.source_pu_st, &identity) ||
            !DSL_PU_Interface_Image_Validate_PU(source, diagnostic) ||
            !DSL_Region_Verify_PU(source, diagnostic) ||
            (!variant.use_existing_pu &&
             DSL_PU_Transaction_Has_Unsupported_Call
                 (PU_Info_tree_ptr(source)))) {
            valid = FALSE;
            break;
        }
        for (UINT32 j = 0; j < variant.formal_count; ++j) {
            const DSL_PU_TRANSACTION_FORMAL_REQUEST &formal =
                variant.formals[j];
            if (formal.name == NULL || formal.name[0] == '\0' ||
                formal.semantic_role == NULL ||
                formal.semantic_role[0] == '\0' ||
                formal.source_position == 0 ||
                !DSL_PU_Transaction_Scalar_TY_Valid(formal.ty)) {
                valid = FALSE;
                break;
            }
            for (UINT32 k = 0; k < j; ++k) {
                if (strcmp(formal.name, variant.formals[k].name) == 0 ||
                    strcmp(formal.semantic_role,
                           variant.formals[k].semantic_role) == 0)
                    valid = FALSE;
            }
        }
        UINT32 existing_count = 0;
        for (UINT32 j = 0; j < plan->variant_count; ++j) {
            const DSL_PU_TRANSACTION_VARIANT_REQUEST &other =
                plan->variants[j];
            if (other.source_pu_st != variant.source_pu_st)
                continue;
            if (other.use_existing_pu)
                ++existing_count;
            if (j < i &&
                (other.signature_size == variant.signature_size &&
                 memcmp(other.signature_bytes, variant.signature_bytes,
                        variant.signature_size) == 0 ||
                 strcmp(other.signature_sha256,
                        variant.signature_sha256) == 0))
                valid = FALSE;
        }
        if (existing_count != 1)
            valid = FALSE;
        if (!variant.use_existing_pu) {
            for (UINT32 j = 1; j < ST_Table_Size(GLOBAL_SYMTAB); ++j) {
                ST_IDX st = make_ST_IDX(j, GLOBAL_SYMTAB);
                if (strcmp(ST_name(St_Table[st]), variant.clone_name) == 0)
                    valid = FALSE;
            }
            for (UINT32 j = 0; j < i; ++j) {
                if (!plan->variants[j].use_existing_pu &&
                    strcmp(plan->variants[j].clone_name,
                           variant.clone_name) == 0)
                    valid = FALSE;
            }
        }
    }
    for (UINT32 i = 0; i < plan->route_count && valid; ++i) {
        const DSL_PU_TRANSACTION_ROUTE_REQUEST &route =
            plan->routes[i];
        DSL_CALLSITE_METADATA_RECORD callsite;
        if (route.variant_index >= plan->variant_count ||
            !DSL_Call_Image_Get_Callsite
                (route.callsite_id, &callsite) ||
            callsite.callee_pu_st !=
                plan->variants[route.variant_index].source_pu_st ||
            callsite.owner_pu_st == callsite.callee_pu_st ||
            route.actual_count !=
                plan->variants[route.variant_index].formal_count ||
            (route.actual_count != 0 && route.actuals == NULL)) {
            valid = FALSE;
            break;
        }
        PU_Info *caller = DSL_PU_Transaction_Find_PU
                              (program, callsite.owner_pu_st);
        if (caller == NULL) {
            valid = FALSE;
            break;
        }
        DSL_PU_Transaction_Activate(caller);
        const WN *call = DSL_Call_Image_Get_Call_WN(route.callsite_id);
        if (call == NULL || WN_operator(call) != OPR_CALL ||
            WN_st_idx(call) != callsite.callee_pu_st ||
            !DSL_Call_ABI_Image_Validate_PU(caller, diagnostic)) {
            valid = FALSE;
            break;
        }
        UINT32 input_count = WN_kid_count(call);
        for (UINT32 j = 0; j < input_count; ++j) {
            if (WN_Parm_Out(WN_kid(call, j))) {
                input_count = j;
                break;
            }
        }
        for (UINT32 j = 0; j < route.actual_count; ++j) {
            const DSL_PU_TRANSACTION_ACTUAL_REQUEST &actual =
                route.actuals[j];
            const DSL_PU_TRANSACTION_FORMAL_REQUEST &formal =
                plan->variants[route.variant_index].formals[j];
            if (actual.callee_formal_ordinal != input_count + j ||
                actual.ty != formal.ty ||
                actual.semantic_role == NULL ||
                strcmp(actual.semantic_role, formal.semantic_role) != 0 ||
                actual.source_tcon == TCON_IDX_ZERO ||
                actual.source_tcon >= TCON_Table_Size() ||
                TCON_ty(Tcon_Table[actual.source_tcon]) !=
                    TY_mtype(actual.ty))
                valid = FALSE;
        }
        for (UINT32 j = 0; j < i; ++j) {
            if (plan->routes[j].callsite_id == route.callsite_id)
                valid = FALSE;
        }
    }
    for (PU_Info *pu = program; pu != NULL && valid;
         pu = PU_Info_next(pu)) {
        DSL_PU_Transaction_Activate(pu);
        if (!DSL_PU_Transaction_Call_Routes_Complete
                (PU_Info_tree_ptr(pu), plan))
            valid = FALSE;
    }
    if (prior != NULL)
        DSL_PU_Transaction_Activate(prior);
    return valid || DSL_PU_Transaction_Report
                        (diagnostic, "program variant preflight failed");
}

BOOL
DSL_PU_Transaction_Insert_Formals_Active
        (PU_Info *pu, const DSL_PU_TRANSACTION_FORMAL_REQUEST *requests,
         UINT32 request_count, ST_IDX *formal_sts,
         DSL_IR_VALUE_ID *formal_values, FILE *diagnostic)
{
    if (pu == NULL || requests == NULL || request_count == 0 ||
        formal_sts == NULL || formal_values == NULL ||
        Current_pu != &PU_Info_pu(pu) ||
        Current_Map_Tab != PU_Info_maptab(pu) ||
        PU_Info_state(pu, WT_TREE) != Subsect_InMem ||
        !DSL_PU_Interface_Image_Validate_PU(pu, diagnostic))
        return DSL_PU_Transaction_Report
                   (diagnostic, "invalid active formal transaction");
    WN *old_entry = PU_Info_tree_ptr(pu);
    if (old_entry == NULL || WN_operator(old_entry) != OPR_FUNC_ENTRY ||
        (UINT32)WN_num_formals(old_entry) + request_count > 32767)
        return DSL_PU_Transaction_Report
                   (diagnostic, "unsupported entry formal count");

    UINT32 old_count = WN_num_formals(old_entry);
    UINT32 insert_at = old_count;
    for (UINT32 i = 0; i < old_count; ++i) {
        ST_IDX st = WN_st_idx(WN_formal(old_entry, i));
        if (ST_sclass(St_Table[st]) == SCLASS_FORMAL_REF &&
            insert_at == old_count)
            insert_at = i;
        else if (insert_at != old_count &&
                 ST_sclass(St_Table[st]) != SCLASS_FORMAL_REF)
            return DSL_PU_Transaction_Report
                       (diagnostic, "result formals are not trailing");
    }
    for (UINT32 i = 0; i < request_count; ++i) {
        const DSL_PU_TRANSACTION_FORMAL_REQUEST &request = requests[i];
        if (request.name == NULL || request.name[0] == '\0' ||
            request.semantic_role == NULL ||
            request.semantic_role[0] == '\0' ||
            !DSL_PU_Transaction_Scalar_TY_Valid(request.ty) ||
            request.source_position == 0)
            return DSL_PU_Transaction_Report
                       (diagnostic, "invalid typed formal request");
        for (UINT32 j = 0; j < i; ++j) {
            if (strcmp(requests[j].name, request.name) == 0 ||
                strcmp(requests[j].semantic_role,
                       request.semantic_role) == 0)
                return DSL_PU_Transaction_Report
                           (diagnostic, "duplicate formal request");
        }
        for (UINT32 j = 1; j < ST_Table_Size(CURRENT_SYMTAB); ++j) {
            ST_IDX st = make_ST_IDX(j, CURRENT_SYMTAB);
            if (strcmp(ST_name(St_Table[st]), request.name) == 0)
                return DSL_PU_Transaction_Report
                           (diagnostic, "formal name already exists");
        }
    }

    ST_IDX owner = PU_Info_proc_sym(pu);
    const char *owner_name = ST_name(St_Table[owner]);
    std::string owner_metadata("owner_pu=");
    owner_metadata += owner_name;
    for (UINT32 i = 0; i < request_count; ++i) {
        const DSL_PU_TRANSACTION_FORMAL_REQUEST &request = requests[i];
        ST *st = New_ST(CURRENT_SYMTAB);
        ST_Init(st, Save_Str(request.name), CLASS_VAR, SCLASS_FORMAL,
                EXPORT_LOCAL, request.ty);
        Set_ST_is_value_parm(st);
        Set_ST_Srcpos(*st, request.source_position);
        formal_sts[i] = ST_st_idx(*st);
        DSL_IR_VALUE_RECORD value;
        DSL_IR_Value_Record_Init(&value);
        value.value_kind = DSL_IR_VALUE_SYMBOL;
        value.ty = request.ty;
        value.st = formal_sts[i];
        value.name = Save_Str(request.name);
        value.metadata = Save_Str(owner_metadata.c_str());
        formal_values[i] = DSL_IR_Image_Add_Value(&value);
        if (formal_values[i] == DSL_IR_VALUE_INVALID_ID)
            return DSL_PU_Transaction_Report
                       (diagnostic, "could not create formal value");
    }

    UINT32 new_count = old_count + request_count;
    WN *entry = WN_CreateEntry
                    ((INT16)new_count, owner, WN_func_body(old_entry),
                     WN_func_pragmas(old_entry), WN_func_varrefs(old_entry));
    WN_Set_Linenum(entry, WN_Get_Linenum(old_entry));
    for (UINT32 i = 0; i < new_count; ++i) {
        ST_IDX st = i < insert_at ?
            WN_st_idx(WN_formal(old_entry, i)) :
            i < insert_at + request_count ?
                formal_sts[i - insert_at] :
                WN_st_idx(WN_formal(old_entry, i - request_count));
        WN_formal(entry, i) = WN_CreateIdname(0, st);
    }

    TY_IDX old_prototype = PU_prototype(Pu_Table[ST_pu(St_Table[owner])]);
    TYLIST_IDX old_tylist = TY_tylist(old_prototype);
    TY_IDX return_ty = TYLIST_type(old_tylist);
    TY_IDX function_ty;
    TY &function = New_TY(function_ty);
    TY_Init(function, 0, KIND_FUNCTION, MTYPE_UNKNOWN, 0);
    Set_TY_align(function_ty, 1);
    TYLIST_IDX tylist;
    Set_TYLIST_type(New_TYLIST(tylist), return_ty);
    Set_TY_tylist(function_ty, tylist);
    for (UINT32 i = 0; i < new_count; ++i) {
        ST_IDX st = WN_st_idx(WN_formal(entry, i));
        TY_IDX formal_ty = ST_type(St_Table[st]);
        if (ST_sclass(St_Table[st]) == SCLASS_FORMAL_REF)
            formal_ty = Make_Pointer_Type(formal_ty);
        Set_TYLIST_type(New_TYLIST(tylist), formal_ty);
    }
    Set_TYLIST_type(New_TYLIST(tylist), TY_IDX_ZERO);

    if (!DSL_PU_Interface_Image_Shift_Formals
            (owner, insert_at, request_count))
        return DSL_PU_Transaction_Report
                   (diagnostic, "could not shift hidden result rows");
    for (UINT32 i = 0; i < request_count; ++i) {
        DSL_PU_FORMAL_RECORD formal;
        memset(&formal, 0, sizeof(formal));
        formal.owner_pu_st = owner;
        formal.formal_value_id = formal_values[i];
        formal.formal_ordinal = insert_at + i;
        formal.formal_st = formal_sts[i];
        formal.formal_ty = requests[i].ty;
        if (DSL_PU_Interface_Image_Add_Formal(&formal) ==
            DSL_PU_FORMAL_INVALID_ID)
            return DSL_PU_Transaction_Report
                       (diagnostic, "could not register formal row");
    }
    Set_PU_prototype(Pu_Table[ST_pu(St_Table[owner])], function_ty);
    Set_PU_Info_tree_ptr(pu, entry);
    return DSL_PU_Interface_Image_Validate_PU(pu, diagnostic);
}

static WN *
DSL_PU_Transaction_Parent_Block (WN *tree, const WN *target)
{
    if (tree == NULL || target == NULL)
        return NULL;
    if (WN_operator(tree) == OPR_BLOCK) {
        for (WN *stmt = WN_first(tree); stmt != NULL;
             stmt = WN_next(stmt)) {
            if (stmt == target)
                return tree;
            WN *nested = DSL_PU_Transaction_Parent_Block(stmt, target);
            if (nested != NULL)
                return nested;
        }
        return NULL;
    }
    for (INT i = 0; i < WN_kid_count(tree); ++i) {
        WN *nested = DSL_PU_Transaction_Parent_Block
                         (WN_kid(tree, i), target);
        if (nested != NULL)
            return nested;
    }
    return NULL;
}

static void
DSL_PU_Transaction_Find_Value_Definition
        (PU_Info *owner, WN *tree, DSL_IR_VALUE_ID value_id,
         WN **parent, WN **definition, UINT32 *matches)
{
    if (tree == NULL)
        return;
    if (WN_operator(tree) == OPR_BLOCK) {
        for (WN *stmt = WN_first(tree); stmt != NULL;
             stmt = WN_next(stmt)) {
            DSL_IR_VALUE_RECORD value;
            if (WN_operator(stmt) == OPR_STID &&
                DSL_IR_Image_Find_Definition_Value
                    (owner, stmt, &value) &&
                value.id == value_id) {
                *parent = tree;
                *definition = stmt;
                ++*matches;
            }
            DSL_PU_Transaction_Find_Value_Definition
                (owner, stmt, value_id, parent, definition, matches);
        }
        return;
    }
    for (INT i = 0; i < WN_kid_count(tree); ++i) {
        DSL_PU_Transaction_Find_Value_Definition
            (owner, WN_kid(tree, i), value_id,
             parent, definition, matches);
    }
}

BOOL
DSL_PU_Transaction_Root_Constant_Active
        (PU_Info *owner, DSL_IR_VALUE_ID anchor_value_id,
         const char *name, TY_IDX ty, TCON_IDX source_tcon,
         ST_IDX *created_st, DSL_IR_VALUE_ID *created_value,
         FILE *diagnostic)
{
    if (created_st != NULL)
        *created_st = ST_IDX_ZERO;
    if (created_value != NULL)
        *created_value = DSL_IR_VALUE_INVALID_ID;
    if (owner == NULL || name == NULL || name[0] == '\0' ||
        created_st == NULL || created_value == NULL ||
        Current_pu != &PU_Info_pu(owner) ||
        Current_Map_Tab != PU_Info_maptab(owner) ||
        !DSL_PU_Transaction_Scalar_TY_Valid(ty) ||
        source_tcon == TCON_IDX_ZERO ||
        source_tcon >= TCON_Table_Size() ||
        TCON_ty(Tcon_Table[source_tcon]) != TY_mtype(ty))
        return DSL_PU_Transaction_Report
                   (diagnostic, "invalid root constant request");
    for (UINT32 i = 1; i < ST_Table_Size(CURRENT_SYMTAB); ++i) {
        ST_IDX st = make_ST_IDX(i, CURRENT_SYMTAB);
        const char *old_name = ST_name(St_Table[st]);
        if (old_name != NULL && strcmp(old_name, name) == 0)
            return DSL_PU_Transaction_Report
                       (diagnostic, "root constant name already exists");
    }
    WN *parent = NULL;
    WN *definition = NULL;
    UINT32 matches = 0;
    DSL_PU_Transaction_Find_Value_Definition
        (owner, PU_Info_tree_ptr(owner), anchor_value_id,
         &parent, &definition, &matches);
    if (matches != 1 || parent == NULL || definition == NULL)
        return DSL_PU_Transaction_Report
                   (diagnostic, "root value has no unique definition");

    ST *st = New_ST(CURRENT_SYMTAB);
    ST_Init(st, Save_Str(name), CLASS_VAR, SCLASS_AUTO,
            EXPORT_LOCAL, ty);
    *created_st = ST_st_idx(*st);
    Set_ST_Srcpos(*st, WN_Get_Linenum(definition));
    ST_IDX constant_st = ST_st_idx
        (*New_Const_Sym(source_tcon, ty));
    TYPE_ID mtype = TY_mtype(ty);
    WN *initialize = WN_CreateStid
        (OPR_STID, MTYPE_V, mtype, 0, *created_st, ty,
         WN_CreateConst(OPR_CONST, mtype, MTYPE_V, constant_st));
    WN_Set_Linenum(initialize, WN_Get_Linenum(definition));
    WN_INSERT_BlockBefore(parent, definition, initialize);
    DSL_IR_VALUE_RECORD value;
    DSL_IR_Value_Record_Init(&value);
    value.value_kind = DSL_IR_VALUE_SYMBOL;
    value.ty = ty;
    value.st = *created_st;
    value.name = Save_Str(name);
    std::string metadata("owner_pu=");
    metadata += ST_name(St_Table[PU_Info_proc_sym(owner)]);
    value.metadata = Save_Str(metadata.c_str());
    *created_value = DSL_IR_Image_Add_Value(&value);
    return *created_value != DSL_IR_VALUE_INVALID_ID &&
           DSL_IR_Image_Validate(diagnostic);
}

static void
DSL_PU_Transaction_Scan_Scalar_Constant
        (WN *tree, ST_IDX st, TY_IDX ty,
         UINT32 *definitions, BOOL *address_taken, TCON_IDX *constant)
{
    if (tree == NULL)
        return;
    OPERATOR opr = WN_operator(tree);
    if (opr == OPR_LDA && WN_st_idx(tree) == st)
        *address_taken = TRUE;
    if (opr == OPR_STID && WN_st_idx(tree) == st) {
        ++*definitions;
        WN *rhs = WN_kid0(tree);
        ST_IDX constant_st = rhs != NULL &&
            WN_operator(rhs) == OPR_CONST ?
                WN_st_idx(rhs) : ST_IDX_ZERO;
        if (WN_ty(tree) == ty && rhs != NULL &&
            WN_operator(rhs) == OPR_CONST &&
            ST_IDX_level(constant_st) == GLOBAL_SYMTAB &&
            ST_IDX_index(constant_st) != 0 &&
            ST_IDX_index(constant_st) < ST_Table_Size(GLOBAL_SYMTAB) &&
            ST_class(St_Table[constant_st]) == CLASS_CONST)
            *constant = ST_tcon(St_Table[constant_st]);
        else
            *constant = TCON_IDX_ZERO;
    }
    if (opr == OPR_BLOCK) {
        for (WN *stmt = WN_first(tree); stmt != NULL;
             stmt = WN_next(stmt))
            DSL_PU_Transaction_Scan_Scalar_Constant
                (stmt, st, ty, definitions, address_taken, constant);
        return;
    }
    for (INT i = 0; i < WN_kid_count(tree); ++i)
        DSL_PU_Transaction_Scan_Scalar_Constant
            (WN_kid(tree, i), st, ty, definitions,
             address_taken, constant);
}

BOOL
DSL_PU_Transaction_Scalar_TCON_Active
        (PU_Info *owner, DSL_IR_VALUE_ID value_id,
         TCON_IDX *source_tcon, FILE *diagnostic)
{
    if (source_tcon != NULL)
        *source_tcon = TCON_IDX_ZERO;
    DSL_IR_VALUE_RECORD value;
    if (owner == NULL || source_tcon == NULL ||
        Current_pu != &PU_Info_pu(owner) ||
        Current_Map_Tab != PU_Info_maptab(owner) ||
        !DSL_IR_Image_Get_Value(value_id, &value) ||
        value.value_kind != DSL_IR_VALUE_SYMBOL ||
        !DSL_PU_Transaction_Scalar_TY_Valid(value.ty) ||
        ST_IDX_level(value.st) != CURRENT_SYMTAB ||
        ST_IDX_index(value.st) == 0 ||
        ST_IDX_index(value.st) >= ST_Table_Size(CURRENT_SYMTAB) ||
        ST_type(St_Table[value.st]) != value.ty ||
        value.metadata == STR_IDX_ZERO)
        return DSL_PU_Transaction_Report
                   (diagnostic, "invalid owned scalar value");
    std::string owner_metadata("owner_pu=");
    owner_metadata += ST_name(St_Table[PU_Info_proc_sym(owner)]);
    if (owner_metadata != Index_To_Str(value.metadata))
        return DSL_PU_Transaction_Report
                   (diagnostic, "scalar value owner mismatch");
    UINT32 definitions = 0;
    BOOL address_taken = FALSE;
    TCON_IDX constant = TCON_IDX_ZERO;
    DSL_PU_Transaction_Scan_Scalar_Constant
        (PU_Info_tree_ptr(owner), value.st, value.ty,
         &definitions, &address_taken, &constant);
    if (definitions != 1 || address_taken ||
        constant == TCON_IDX_ZERO || constant >= TCON_Table_Size() ||
        TCON_ty(Tcon_Table[constant]) != TY_mtype(value.ty))
        return DSL_PU_Transaction_Report
                   (diagnostic, "scalar value has no unique TCON origin");
    *source_tcon = constant;
    return TRUE;
}

BOOL
DSL_PU_Transaction_Route_Call_Active
        (PU_Info *caller, DSL_CALLSITE_METADATA_ID callsite_id,
         ST_IDX variant_pu_st,
         const DSL_PU_TRANSACTION_ACTUAL_REQUEST *requests,
         UINT32 request_count, FILE *diagnostic)
{
    DSL_CALLSITE_METADATA_RECORD callsite;
    if (caller == NULL || (request_count != 0 && requests == NULL) ||
        Current_pu != &PU_Info_pu(caller) ||
        Current_Map_Tab != PU_Info_maptab(caller) ||
        !DSL_IR_Image_PU_ST_Valid(variant_pu_st) ||
        !DSL_Call_Image_Get_Callsite(callsite_id, &callsite) ||
        callsite.owner_pu_st != PU_Info_proc_sym(caller) ||
        !DSL_Call_ABI_Image_Validate_PU(caller, diagnostic))
        return DSL_PU_Transaction_Report
                   (diagnostic, "invalid active call route");
    WN *old_call = const_cast<WN *>
        (DSL_Call_Image_Get_Call_WN(callsite_id));
    WN *parent = DSL_PU_Transaction_Parent_Block
                     (PU_Info_tree_ptr(caller), old_call);
    WN *old_comment = old_call == NULL ? NULL : WN_prev(old_call);
    UINT32 old_count = old_call == NULL ? 0 : WN_kid_count(old_call);
    if (old_call == NULL || WN_operator(old_call) != OPR_CALL ||
        WN_st_idx(old_call) != callsite.callee_pu_st ||
        parent == NULL || old_count + request_count > 32767 ||
        old_comment == NULL || WN_operator(old_comment) != OPR_COMMENT ||
        strncmp(Index_To_Str(WN_GetComment(old_comment)),
                "__WHIRL_DSL_CALL__:", 19) != 0)
        return DSL_PU_Transaction_Report
                   (diagnostic, "managed physical call is unavailable");

    UINT32 insert_at = request_count == 0 ? old_count :
        requests[0].callee_formal_ordinal;
    if (insert_at > old_count ||
        old_count + request_count == 0)
        return DSL_PU_Transaction_Report
                   (diagnostic, "invalid call insertion ordinal");
    for (UINT32 i = 0; i < request_count; ++i) {
        const DSL_PU_TRANSACTION_ACTUAL_REQUEST &request = requests[i];
        DSL_PU_FORMAL_RECORD formal;
        if (request.callee_formal_ordinal != insert_at + i ||
            request.semantic_role == NULL ||
            request.semantic_role[0] == '\0' ||
            !DSL_PU_Transaction_Scalar_TY_Valid(request.ty) ||
            request.source_tcon == TCON_IDX_ZERO ||
            request.source_tcon >= TCON_Table_Size() ||
            TCON_ty(Tcon_Table[request.source_tcon]) !=
                TY_mtype(request.ty) ||
            !DSL_PU_Interface_Image_Find_Formal
                (variant_pu_st, request.callee_formal_ordinal, &formal) ||
            formal.formal_ty != request.ty)
            return DSL_PU_Transaction_Report
                       (diagnostic, "typed actual/formal mismatch");
    }
    for (UINT32 i = 0; i < old_count; ++i) {
        DSL_CALL_ARGUMENT_RECORD argument;
        if (WN_kid(old_call, i) == NULL ||
            (!WN_Parm_Out(WN_kid(old_call, i)) &&
             (!DSL_Call_ABI_Image_Find_Argument_By_Id
                  (callsite_id, i, &argument) ||
              argument.callee_formal_ordinal != i)))
            return DSL_PU_Transaction_Report
                       (diagnostic, "old call ABI is incomplete");
    }
    UINT32 variant_formals = 0;
    for (UINT32 i = 1; i <= DSL_PU_Interface_Image_Formal_Count(); ++i) {
        DSL_PU_FORMAL_RECORD formal;
        if (DSL_PU_Interface_Image_Get_Formal(i, &formal) &&
            formal.owner_pu_st == variant_pu_st)
            ++variant_formals;
    }
    if (variant_formals != old_count + request_count)
        return DSL_PU_Transaction_Report
                   (diagnostic, "variant formal count mismatch");

    WN *new_call = WN_Create
                       (OPR_CALL, WN_rtype(old_call), WN_desc(old_call),
                        old_count + request_count);
    WN_st_idx(new_call) = variant_pu_st;
    WN_call_flag(new_call) = WN_call_flag(old_call);
    WN_Set_Linenum(new_call, WN_Get_Linenum(old_call));
    for (UINT32 i = 0; i < old_count; ++i)
        WN_kid(new_call, i < insert_at ? i : i + request_count) =
            WN_kid(old_call, i);

    std::vector<ST_IDX> actual_sts(request_count, ST_IDX_ZERO);
    std::vector<DSL_IR_VALUE_ID> actual_values
        (request_count, DSL_IR_VALUE_INVALID_ID);
    std::string metadata("owner_pu=");
    metadata += ST_name(St_Table[PU_Info_proc_sym(caller)]);
    for (UINT32 i = 0; i < request_count; ++i) {
        const DSL_PU_TRANSACTION_ACTUAL_REQUEST &request = requests[i];
        char name[96];
        snprintf(name, sizeof(name), "__dsl_arg_%u_%u",
                 callsite_id, request.callee_formal_ordinal);
        ST *st = New_ST(CURRENT_SYMTAB);
        ST_Init(st, Save_Str(name), CLASS_VAR, SCLASS_AUTO,
                EXPORT_LOCAL, request.ty);
        Set_ST_Srcpos(*st, WN_Get_Linenum(old_call));
        actual_sts[i] = ST_st_idx(*st);
        TYPE_ID mtype = TY_mtype(request.ty);
        ST_IDX constant_st = ST_st_idx
            (*New_Const_Sym(request.source_tcon, request.ty));
        WN *initialize = WN_CreateStid
            (OPR_STID, MTYPE_V, mtype, 0, actual_sts[i], request.ty,
             WN_CreateConst(OPR_CONST, mtype, MTYPE_V, constant_st));
        WN_Set_Linenum(initialize, WN_Get_Linenum(old_call));
        WN_INSERT_BlockBefore(parent, old_call, initialize);
        WN *load = WN_CreateLdid
                       (OPR_LDID, mtype, mtype, 0, actual_sts[i],
                        request.ty);
        WN_kid(new_call, insert_at + i) = WN_CreateParm
            (mtype, load, request.ty,
             WN_PARM_BY_VALUE | WN_PARM_READ_ONLY |
             WN_PARM_PASSED_NOT_SAVED);
        DSL_IR_VALUE_RECORD value;
        DSL_IR_Value_Record_Init(&value);
        value.value_kind = DSL_IR_VALUE_SYMBOL;
        value.ty = request.ty;
        value.st = actual_sts[i];
        value.name = Save_Str(name);
        value.metadata = Save_Str(metadata.c_str());
        actual_values[i] = DSL_IR_Image_Add_Value(&value);
        if (actual_values[i] == DSL_IR_VALUE_INVALID_ID)
            return DSL_PU_Transaction_Report
                       (diagnostic, "could not register actual value");
    }
    WN_INSERT_BlockBefore(parent, old_call, new_call);
    if (!DSL_Call_Image_Retarget_Call_WN
            (callsite_id, old_call, new_call, variant_pu_st) ||
        (request_count != 0 &&
         !DSL_Call_ABI_Image_Shift_Arguments
             (callsite_id, insert_at, request_count)))
        return DSL_PU_Transaction_Report
                   (diagnostic, "could not shift call image rows");
    for (UINT32 i = 0; i < request_count; ++i) {
        DSL_CALL_ARGUMENT_RECORD argument;
        memset(&argument, 0, sizeof(argument));
        argument.callsite_id = callsite_id;
        argument.argument_value_id = actual_values[i];
        argument.actual_ordinal = insert_at + i;
        argument.callee_formal_ordinal = insert_at + i;
        argument.semantic_role = Save_Str(requests[i].semantic_role);
        if (DSL_Call_ABI_Image_Add_Argument(new_call, &argument) ==
            DSL_CALL_ARGUMENT_INVALID_ID)
            return DSL_PU_Transaction_Report
                       (diagnostic, "could not register actual ABI row");
    }
    std::string comment_text("__WHIRL_DSL_CALL__:callee=");
    comment_text += ST_name(St_Table[variant_pu_st]);
    comment_text += ";class=";
    comment_text += Index_To_Str(callsite.canonical_class_name);
    comment_text += ";instance=";
    comment_text += Index_To_Str(callsite.instance_path);
    comment_text += ";context=";
    comment_text += Index_To_Str(callsite.context_identity);
    char ordinal_text[32];
    snprintf(ordinal_text, sizeof(ordinal_text), "%u",
             callsite.source_call_ordinal);
    comment_text += ";ordinal=";
    comment_text += ordinal_text;
    Set_ST_name_idx(ST_ptr(WN_st_idx(old_comment)),
                    Save_Str(comment_text.c_str()));
    WN_EXTRACT_FromBlock(parent, old_call);
    return DSL_Call_Image_Validate(diagnostic) &&
           DSL_Call_ABI_Image_Validate_PU(caller, diagnostic);
}

struct DSL_PU_TRANSACTION_RESULT {
    std::vector<ST_IDX> variant_pu_sts;
    std::vector<std::vector<DSL_PU_CLONE_VALUE_PAIR> > clone_values;
};

BOOL
DSL_PU_Transaction_Apply_Resident
        (PU_Info *program, const DSL_PU_TRANSACTION_PLAN *plan,
         DSL_PU_TRANSACTION_RESULT **result, FILE *diagnostic)
{
    if (result == NULL)
        return FALSE;
    *result = NULL;
    if (!DSL_PU_Transaction_Preflight_Resident
            (program, plan, diagnostic))
        return FALSE;

    DSL_PU_TRANSACTION_RESULT *applied =
        new DSL_PU_TRANSACTION_RESULT;
    applied->variant_pu_sts.resize(plan->variant_count, ST_IDX_ZERO);
    applied->clone_values.resize(plan->variant_count);
    // Reverse creation makes repeated insert-after-source clones retain
    // canonical plan order in the PU traversal.
    for (UINT32 n = plan->variant_count; n != 0; --n) {
        UINT32 i = n - 1;
        const DSL_PU_TRANSACTION_VARIANT_REQUEST &variant =
            plan->variants[i];
        PU_Info *source = DSL_PU_Transaction_Find_PU
                              (program, variant.source_pu_st);
        if (variant.use_existing_pu) {
            applied->variant_pu_sts[i] = variant.source_pu_st;
            continue;
        }
        DSL_PU_Transaction_Activate(source);
        UINT32 capacity = DSL_IR_Image_Value_Count();
        if (capacity == 0)
            capacity = 1;
        std::vector<DSL_PU_CLONE_VALUE_PAIR> pairs(capacity);
        UINT32 copied = 0;
        PU_Info *clone = NULL;
        if (!DSL_PU_Transaction_Clone_Active
                (source, variant.clone_name, &pairs[0], capacity,
                 &copied, &clone, diagnostic)) {
            delete applied;
            return DSL_PU_Transaction_Report
                       (diagnostic, "clone failed; stop checkpoint process");
        }
        pairs.resize(copied);
        applied->clone_values[i].swap(pairs);
        applied->variant_pu_sts[i] = PU_Info_proc_sym(clone);
    }
    for (UINT32 i = 0; i < plan->variant_count; ++i) {
        const DSL_PU_TRANSACTION_VARIANT_REQUEST &variant =
            plan->variants[i];
        if (variant.formal_count == 0)
            continue;
        PU_Info *pu = DSL_PU_Transaction_Find_PU
                          (program, applied->variant_pu_sts[i]);
        DSL_PU_Transaction_Activate(pu);
        std::vector<ST_IDX> sts(variant.formal_count);
        std::vector<DSL_IR_VALUE_ID> values(variant.formal_count);
        if (!DSL_PU_Transaction_Insert_Formals_Active
                (pu, variant.formals, variant.formal_count,
                 &sts[0], &values[0], diagnostic)) {
            delete applied;
            return DSL_PU_Transaction_Report
                       (diagnostic, "formal insertion failed; stop process");
        }
    }
    for (UINT32 i = 0; i < plan->route_count; ++i) {
        const DSL_PU_TRANSACTION_ROUTE_REQUEST &route =
            plan->routes[i];
        DSL_CALLSITE_METADATA_RECORD callsite;
        if (!DSL_Call_Image_Get_Callsite
                (route.callsite_id, &callsite)) {
            delete applied;
            return DSL_PU_Transaction_Report
                       (diagnostic, "callsite disappeared; stop process");
        }
        PU_Info *caller = DSL_PU_Transaction_Find_PU
                              (program, callsite.owner_pu_st);
        DSL_PU_Transaction_Activate(caller);
        if (!DSL_PU_Transaction_Route_Call_Active
                (caller, route.callsite_id,
                 applied->variant_pu_sts[route.variant_index],
                 route.actuals, route.actual_count, diagnostic)) {
            delete applied;
            return DSL_PU_Transaction_Report
                       (diagnostic, "call route failed; stop process");
        }
    }
    if (!DSL_IR_Image_Validate(diagnostic) ||
        !DSL_Call_Image_Validate(diagnostic) ||
        !DSL_Call_ABI_Image_Validate(diagnostic) ||
        !DSL_PU_Interface_Image_Validate(diagnostic)) {
        delete applied;
        return DSL_PU_Transaction_Report
                   (diagnostic, "final image failed; stop process");
    }
    for (PU_Info *pu = program; pu != NULL; pu = PU_Info_next(pu)) {
        DSL_PU_Transaction_Activate(pu);
        if (!DSL_PU_Interface_Image_Validate_PU(pu, diagnostic) ||
            !DSL_Call_ABI_Image_Validate_PU(pu, diagnostic) ||
            !DSL_Region_Verify_PU(pu, diagnostic)) {
            delete applied;
            return DSL_PU_Transaction_Report
                       (diagnostic, "final PU failed; stop process");
        }
    }
    *result = applied;
    return TRUE;
}

ST_IDX
DSL_PU_Transaction_Variant_PU_ST
        (const DSL_PU_TRANSACTION_RESULT *result, UINT32 variant_index)
{
    return result == NULL || variant_index >=
        result->variant_pu_sts.size() ? ST_IDX_ZERO :
        result->variant_pu_sts[variant_index];
}

BOOL
DSL_PU_Transaction_Cloned_Value
        (const DSL_PU_TRANSACTION_RESULT *result,
         UINT32 variant_index, DSL_IR_VALUE_ID source_value_id,
         DSL_IR_VALUE_ID *clone_value_id)
{
    if (clone_value_id == NULL || result == NULL ||
        variant_index >= result->clone_values.size())
        return FALSE;
    const std::vector<DSL_PU_CLONE_VALUE_PAIR> &pairs =
        result->clone_values[variant_index];
    for (UINT32 i = 0; i < pairs.size(); ++i) {
        if (pairs[i].source_value_id == source_value_id) {
            *clone_value_id = pairs[i].clone_value_id;
            return TRUE;
        }
    }
    return FALSE;
}

void
DSL_PU_Transaction_Result_Delete (DSL_PU_TRANSACTION_RESULT *result)
{
    delete result;
}
