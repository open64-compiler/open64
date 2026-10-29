/*
 * Backend-free structural interface predicates and physical caller/callee ABI
 * verification for driver-selected PU context. These checks do not switch PUs,
 * mutate WHIRL/images or acquire external tensor state. Mapped row contracts,
 * diagnostic spelling and legacy empty-interface behavior are unchanged.
 * Design: doc/WHIRL-DSL-INFRASTRUCTURE.md, persistent IR / service separation.
 */

#include <algorithm>
#include <string.h>
#include <string>
#include <vector>

#include "dsl_pu_interface_verify.h"
#include "dsl_pu_interface_verify_internal.h"
#include "../../common/ir_image/dsl_image_service.h"
#include "dsl_runtime_interface_internal.h"
#include "dsl_ir_transaction_internal.h"
#include "pu_info.h"
#include "strtab.h"
#include "symtab.h"
#include "wn.h"
#include "wn_util.h"

/*
 * Validate one persisted runtime value projection independently of an active
 * local symtab. Reader and gatekeeper paths use this structural predicate; it
 * performs no mutation.
 */
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

/* Check a nonzero packed TY and its table-index bound, not kind or sealing. */
BOOL
DSL_Program_Interface_TY_Contract_Valid (TY_IDX ty)
{
    return ty != TY_IDX_ZERO && TY_IDX_index(ty) < TY_Table_Size();
}

/* Require an existing pointer TY without independently proving its pointee. */
BOOL
DSL_Program_Interface_Pointer_TY_Contract_Valid (TY_IDX ty)
{
    return DSL_Program_Interface_TY_Contract_Valid(ty) &&
           TY_kind(ty) == KIND_POINTER;
}

/* Require an existing tensor-extension TY without proving descriptor sealing. */
BOOL
DSL_Program_Interface_Tensor_TY_Contract_Valid (TY_IDX ty)
{
    return DSL_Program_Interface_TY_Contract_Valid(ty) &&
           TY_is_tensor_extension(ty);
}

/* Validate that a persisted tensor constant index resolves in TCON storage. */
BOOL
DSL_Program_Interface_TCON_Contract_Valid (TCON_IDX tcon)
{
    return tcon != TCON_IDX_ZERO && tcon < TCON_Table_Size();
}

/* Emit the stable per-PU call-ABI diagnostic shape and return FALSE. */
static BOOL
DSL_Call_ABI_PU_Report (FILE *diagnostic, const char *message, UINT32 id)
{
    if (diagnostic != NULL)
        fprintf(diagnostic, "DSL call ABI PU error: %s id=%u\n", message, id);
    return FALSE;
}

/*
 * Prove that a call-ABI value row names the exact active-PU ST and TY. This
 * owner-sensitive check prevents colliding local ST_IDX values from matching.
 */
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

/*
 * Validate persisted call argument roles against one active caller/callee
 * physical ABI. It checks ordinals, directions, pointer contracts, and formal
 * identity without mutating the tree or mapped tables.
 */
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
                address == NULL ||
                (WN_operator(address) != OPR_LDA &&
                 WN_operator(address) != OPR_LDID) ||
                !DSL_IR_Image_Get_Value(argument.argument_value_id, &value) ||
                !DSL_Call_ABI_Value_Matches_ST
                    (value, owner_pu_st, WN_st_idx(address)))
                return DSL_Call_ABI_PU_Report
                           (diagnostic, "argument value mismatch", argument.id);
            BOOL tensor_reference = WN_operator(address) == OPR_LDA &&
                WN_Parm_By_Reference(parm) &&
                WN_ty(parm) == WN_ty(address) &&
                TY_kind(WN_ty(parm)) == KIND_POINTER &&
                TY_pointed(WN_ty(parm)) == value.ty;
            TYPE_ID scalar_mtype = TY_mtype(value.ty);
            BOOL scalar_value = WN_operator(address) == OPR_LDID &&
                WN_Parm_By_Value(parm) &&
                TY_kind(value.ty) == KIND_SCALAR &&
                (scalar_mtype == MTYPE_I4 ||
                 scalar_mtype == MTYPE_U4 ||
                 scalar_mtype == MTYPE_I8 ||
                 scalar_mtype == MTYPE_U8 ||
                 scalar_mtype == MTYPE_F4 ||
                 scalar_mtype == MTYPE_F8) &&
                WN_ty(address) == value.ty && WN_ty(parm) == value.ty;
            if ((!tensor_reference && !scalar_value) ||
                !WN_Parm_Read_Only(parm) || WN_Parm_Out(parm) ||
                !WN_Parm_Passed_Not_Saved(parm))
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

/* Emit the stable PU-interface diagnostic shape and return FALSE. */
static BOOL
DSL_PU_Interface_PU_Report
        (FILE *diagnostic, const char *message, UINT32 id)
{
    if (diagnostic != NULL)
        fprintf(diagnostic, "DSL PU interface error: %s id=%u\n",
                message, id);
    return FALSE;
}

/*
 * Validate logical formal-row identity, ownership, uniqueness and matching
 * recorded ST/TY facts without a callee local symtab or physical tree proof.
 */
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



/* Cross-check the physical formal order, even when append-only row IDs are
 * not in ordinal order after insertion before hidden results. */
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

    std::vector<DSL_PU_FORMAL_RECORD> ordered_formals;
    for (UINT32 i = 1; i <= DSL_PU_Interface_Image_Formal_Count(); ++i) {
        DSL_PU_FORMAL_RECORD formal;
        if (!DSL_PU_Interface_Image_Get_Formal(i, &formal))
            return DSL_PU_Interface_PU_Report
                       (diagnostic, "missing formal", i);
        if (formal.owner_pu_st != owner_pu_st)
            continue;
        ordered_formals.push_back(formal);
    }
    std::sort(ordered_formals.begin(), ordered_formals.end(),
              DSL_PU_FORMAL_ORDINAL_LESS());
    for (UINT32 i = 0; i < ordered_formals.size(); ++i) {
        const DSL_PU_FORMAL_RECORD &formal = ordered_formals[i];
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
