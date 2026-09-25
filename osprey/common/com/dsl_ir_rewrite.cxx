/*
 * Copyright (C) 2026 Open64 Project
 */

#include <ctype.h>
#include <string.h>
#include <errno.h>
#include <stdlib.h>
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
extern BOOL DSL_IR_Image_Retype_Value
                                (DSL_IR_VALUE_ID, TY_IDX, TY_IDX);
extern UINT32 DSL_Region_Symbol_Use_Count (PU_Info *, ST_IDX);
extern BOOL DSL_Region_Can_Redirect_Symbol (PU_Info *, ST_IDX, ST_IDX);
extern BOOL DSL_Region_Redirect_Symbol (PU_Info *, ST_IDX, ST_IDX);
#include "wn_util.h"

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

static BOOL
DSL_IR_Tree_Contains (const WN *tree, const WN *target)
{
    if (tree == NULL || target == NULL)
        return FALSE;
    if (tree == target)
        return TRUE;
    if (WN_operator(tree) == OPR_BLOCK) {
        for (const WN *statement = WN_first(tree); statement != NULL;
             statement = WN_next(statement)) {
            if (DSL_IR_Tree_Contains(statement, target))
                return TRUE;
        }
        return FALSE;
    }
    for (INT32 kid = 0; kid < WN_kid_count(tree); ++kid) {
        if (DSL_IR_Tree_Contains(WN_kid(tree, kid), target))
            return TRUE;
    }
    return FALSE;
}

static BOOL
DSL_Call_ABI_Validate_Visible_Parameters
        (ST_IDX owner_pu_st, WN *entry, FILE *diagnostic)
{
    for (UINT32 i = 1; i <= DSL_Call_Image_Callsite_Count(); ++i) {
        DSL_CALLSITE_METADATA_RECORD callsite;
        if (!DSL_Call_Image_Get_Callsite(i, &callsite))
            return DSL_Call_ABI_PU_Report
                       (diagnostic, "missing callsite", i);
        if (callsite.owner_pu_st != owner_pu_st)
            continue;
        const WN *call = DSL_Call_Image_Get_Call_WN(callsite.id);
        if (call == NULL || WN_operator(call) != OPR_CALL ||
            !DSL_IR_Tree_Contains(entry, call) ||
            WN_st_idx(call) != callsite.callee_pu_st)
            return DSL_Call_ABI_PU_Report
                       (diagnostic, "callsite is outside active PU", i);
        for (INT32 ordinal = 0; ordinal < WN_kid_count(call); ++ordinal) {
            DSL_PU_FORMAL_RECORD formal;
            const WN *parm = WN_kid(call, ordinal);
            const WN *address = parm == NULL || WN_operator(parm) != OPR_PARM ?
                                NULL : WN_kid0(parm);
            if (address == NULL || WN_operator(address) != OPR_LDA ||
                !DSL_PU_Interface_Image_Find_Formal
                    (callsite.callee_pu_st, ordinal, &formal))
                return DSL_Call_ABI_PU_Report
                           (diagnostic, "missing physical parameter", i);
            ST_IDX actual_st = WN_st_idx(address);
            DSL_IR_VALUE_RECORD value;
            if (ST_IDX_level(actual_st) != CURRENT_SYMTAB ||
                ST_IDX_index(actual_st) == 0 ||
                ST_IDX_index(actual_st) >= ST_Table_Size(CURRENT_SYMTAB) ||
                !DSL_IR_Image_Find_PU_Value
                    (actual_st, ST_name(St_Table[actual_st]),
                     ST_name(St_Table[owner_pu_st]), &value) ||
                !DSL_Call_ABI_Value_Matches_ST
                    (value, owner_pu_st, actual_st) ||
                WN_ty(address) != WN_ty(parm) ||
                TY_kind(WN_ty(parm)) != KIND_POINTER ||
                TY_pointed(WN_ty(parm)) != value.ty ||
                formal.formal_ty != value.ty ||
                !WN_Parm_By_Reference(parm) ||
                !WN_Parm_Passed_Not_Saved(parm))
                return DSL_Call_ABI_PU_Report
                           (diagnostic, "physical parameter type mismatch", i);
            if (WN_Parm_Out(parm)) {
                if (WN_Parm_Read_Only(parm))
                    return DSL_Call_ABI_PU_Report
                               (diagnostic, "output parameter is read-only", i);
            } else if (!WN_Parm_Read_Only(parm)) {
                return DSL_Call_ABI_PU_Report
                           (diagnostic, "input parameter is writable", i);
            }
        }
    }
    return TRUE;
}

BOOL
DSL_Call_ABI_Image_Validate_PU (PU_Info *pu, FILE *diagnostic)
{
    if (pu == NULL || PU_Info_tree_ptr(pu) == NULL ||
        !DSL_IR_Image_PU_ST_Valid(PU_Info_proc_sym(pu)) ||
        Current_pu == NULL ||
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

        if (callsite.owner_pu_st == owner_pu_st) {
            const WN *call = DSL_Call_Image_Get_Call_WN(callsite.id);
            if (call == NULL || argument.actual_ordinal >= WN_kid_count(call))
                return DSL_Call_ABI_PU_Report
                           (diagnostic, "actual ordinal out of range",
                            argument.id);
            const WN *parm = WN_kid(call, argument.actual_ordinal);
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
            if (entry == NULL || WN_operator(entry) != OPR_FUNC_ENTRY ||
                argument.callee_formal_ordinal >= WN_num_formals(entry))
                return DSL_Call_ABI_PU_Report
                           (diagnostic, "formal ordinal out of range",
                            argument.id);
            ST_IDX formal_st =
                WN_st_idx(WN_formal(entry, argument.callee_formal_ordinal));
            DSL_IR_VALUE_RECORD value;
            DSL_PU_FORMAL_RECORD formal;
            if (ST_IDX_level(formal_st) != CURRENT_SYMTAB ||
                ST_IDX_index(formal_st) == 0 ||
                ST_IDX_index(formal_st) >= ST_Table_Size(CURRENT_SYMTAB) ||
                !DSL_IR_Image_Get_Value(argument.argument_value_id, &value) ||
                value.ty != ST_type(St_Table[formal_st]) ||
                (DSL_PU_Interface_Image_Has_Records() &&
                 (!DSL_PU_Interface_Image_Find_Formal
                      (owner_pu_st, argument.callee_formal_ordinal, &formal) ||
                  formal.formal_st != formal_st ||
                  formal.formal_ty != value.ty)))
                return DSL_Call_ABI_PU_Report
                           (diagnostic, "actual/formal type mismatch",
                            argument.id);
        }
    }
    return DSL_Call_ABI_Validate_Visible_Parameters
               (owner_pu_st, entry, diagnostic);
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

static BOOL
DSL_PU_Interface_Tree_Has_Return (const WN *tree)
{
    if (tree == NULL)
        return FALSE;
    if (WN_operator(tree) == OPR_RETURN ||
        WN_operator(tree) == OPR_RETURN_VAL)
        return TRUE;
    if (WN_operator(tree) == OPR_BLOCK) {
        for (const WN *statement = WN_first(tree); statement != NULL;
             statement = WN_next(statement)) {
            if (DSL_PU_Interface_Tree_Has_Return(statement))
                return TRUE;
        }
        return FALSE;
    }
    for (INT32 kid = 0; kid < WN_kid_count(tree); ++kid) {
        if (DSL_PU_Interface_Tree_Has_Return(WN_kid(tree, kid)))
            return TRUE;
    }
    return FALSE;
}

static BOOL
DSL_PU_Interface_Tree_Has_Result_Store
        (const WN *tree, ST_IDX result_st, TY_IDX result_ty)
{
    if (tree == NULL)
        return FALSE;
    if (WN_operator(tree) == OPR_STID && WN_st_idx(tree) == result_st) {
        const WN *value = WN_kid0(tree);
        return WN_ty(tree) == result_ty && value != NULL &&
               WN_ty(value) == result_ty;
    }
    if (WN_operator(tree) == OPR_BLOCK) {
        for (const WN *statement = WN_first(tree); statement != NULL;
             statement = WN_next(statement)) {
            if (DSL_PU_Interface_Tree_Has_Result_Store
                    (statement, result_st, result_ty))
                return TRUE;
        }
        return FALSE;
    }
    for (INT32 kid = 0; kid < WN_kid_count(tree); ++kid) {
        if (DSL_PU_Interface_Tree_Has_Result_Store
                (WN_kid(tree, kid), result_st, result_ty))
            return TRUE;
    }
    return FALSE;
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
    BOOL saw_result = FALSE;
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
        if (formal.formal_ordinal != expected_ordinal ||
            formal.formal_ordinal >= WN_num_formals(entry))
            return DSL_PU_Interface_PU_Report
                       (diagnostic, "formal ordinal mismatch", formal.id);
        WN *idname = WN_formal(entry, formal.formal_ordinal);
        DSL_IR_VALUE_RECORD value;
        if (idname == NULL || WN_operator(idname) != OPR_IDNAME ||
            WN_st_idx(idname) != formal.formal_st ||
            ST_IDX_level(formal.formal_st) != CURRENT_SYMTAB ||
            ST_IDX_index(formal.formal_st) == 0 ||
            ST_IDX_index(formal.formal_st) >= ST_Table_Size(CURRENT_SYMTAB))
            return DSL_PU_Interface_PU_Report
                       (diagnostic, "formal value mismatch", formal.id);
        BOOL result_formal = ST_sclass(St_Table[formal.formal_st]) ==
                             SCLASS_FORMAL_REF;
        if ((ST_sclass(St_Table[formal.formal_st]) != SCLASS_FORMAL &&
             ST_sclass(St_Table[formal.formal_st]) != SCLASS_FORMAL_REF) ||
            ST_type(St_Table[formal.formal_st]) != formal.formal_ty ||
            !DSL_IR_Image_Get_Value(formal.formal_value_id, &value) ||
            !DSL_Call_ABI_Value_Matches_ST
                 (value, owner_pu_st, formal.formal_st))
            return DSL_PU_Interface_PU_Report
                       (diagnostic, "formal value mismatch", formal.id);
        if (saw_result && !result_formal)
            return DSL_PU_Interface_PU_Report
                       (diagnostic, "input follows result formal", formal.id);
        if (result_formal) {
            saw_result = TRUE;
            if (!DSL_PU_Interface_Tree_Has_Result_Store
                    (entry, formal.formal_st, formal.formal_ty))
                return DSL_PU_Interface_PU_Report
                           (diagnostic, "missing result store", formal.id);
        }
        ++expected_ordinal;
    }
    if (expected_ordinal != WN_num_formals(entry))
        return DSL_PU_Interface_PU_Report
                   (diagnostic, "incomplete formal interface", 0);
    if (saw_result && !DSL_PU_Interface_Tree_Has_Return(entry))
        return DSL_PU_Interface_PU_Report
                   (diagnostic, "missing function return", 0);
    return TRUE;
}

static BOOL
DSL_IR_Active_PU_Boundary_Report (FILE *diagnostic, const char *message)
{
    if (diagnostic != NULL)
        fprintf(diagnostic, "DSL active PU boundary error: %s\n", message);
    return FALSE;
}

BOOL
DSL_IR_Image_Validate_Active_PU_Boundaries
        (const DSL_IR_ACTIVE_PU_BOUNDARY_CONTEXT *context,
         FILE *diagnostic)
{
    if (context == NULL || context->pu_info == NULL ||
        context->tree == NULL || context->owner_pu_st == ST_IDX_ZERO ||
        Current_PU_Info != context->pu_info ||
        PU_Info_tree_ptr(context->pu_info) != context->tree ||
        PU_Info_proc_sym(context->pu_info) != context->owner_pu_st ||
        !DSL_IR_Image_Current_PU_Is(context->owner_pu_st) ||
        WN_operator(context->tree) != OPR_FUNC_ENTRY ||
        WN_st_idx(context->tree) != context->owner_pu_st ||
        CURRENT_SYMTAB <= GLOBAL_SYMTAB ||
        Scope_tab[CURRENT_SYMTAB].st == NULL ||
        ST_st_idx(Scope_tab[CURRENT_SYMTAB].st) != context->owner_pu_st)
        return DSL_IR_Active_PU_Boundary_Report
                   (diagnostic, "invalid explicit program-unit context");

    if (!DSL_Call_ABI_Image_Validate_PU(context->pu_info, diagnostic))
        return FALSE;
    if (!DSL_PU_Interface_Image_Validate_PU(context->pu_info, diagnostic))
        return FALSE;
    return DSL_Region_Verify_Active_PU
               (context->pu_info, context->tree, context->owner_pu_st,
                diagnostic);
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
    UINT64 offset;
    UINT64 length;
    UINT64 element_size;
    UINT64 tensor_size;

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
    UINT32 applied_write_count;
} DSL_IR_RETYPE_JOURNAL;

typedef struct {
    BOOL enabled;
    UINT32 fail_after_write;
    UINT32 next_write;
    FILE *diagnostic;
} DSL_IR_RETYPE_TEST_WRITE_CONTROL;

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
        !TY_tensor_preserves_non_shape_state(old_ty, refined_ty))
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

typedef struct {
    const DSL_IR_VALUE_TYPE_REFINEMENT_REQUEST *requests;
    UINT32 request_count;
    std::vector<BOOL> *matched;
    BOOL valid;
} DSL_IR_RETYPE_REFINEMENT_MATCH_CONTEXT;

static BOOL
DSL_IR_Retype_Match_Refinement
        (const DSL_SHAPE_REFINEMENT *refinement,
         void *visitor_context)
{
    DSL_IR_RETYPE_REFINEMENT_MATCH_CONTEXT *context =
        static_cast<DSL_IR_RETYPE_REFINEMENT_MATCH_CONTEXT *>
            (visitor_context);
    if (refinement == NULL || context == NULL ||
        context->requests == NULL || context->matched == NULL ||
        context->matched->size() != context->request_count) {
        if (context != NULL)
            context->valid = FALSE;
        return FALSE;
    }
    for (UINT32 i = 0; i < context->request_count; ++i) {
        const DSL_IR_VALUE_TYPE_REFINEMENT_REQUEST &request =
            context->requests[i];
        if (request.value_id != refinement->value_id)
            continue;
        if ((*context->matched)[i] ||
            !DSL_Shape_Refinement_Matches_Type
                 (refinement, request.value_id, request.expected_old_ty,
                  request.refined_ty)) {
            context->valid = FALSE;
            return FALSE;
        }
        (*context->matched)[i] = TRUE;
        return TRUE;
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
    if (!DSL_IR_Image_Get_Value(request.value_id, &value))
        return DSL_IR_Retype_Report
                   (diagnostic, "DSL-SHAPE-RETYPE-003", request.value_id,
                    "value or expected old type does not match");
    if (!DSL_IR_Image_Value_Belongs_To_PU(value, request.owner_pu_st))
        return DSL_IR_Retype_Report
                   (diagnostic, "DSL-SHAPE-RETYPE-002", request.value_id,
                    "logical value is not owned by the active PU");
    if (value.value_kind != DSL_IR_VALUE_OPERATOR_RESULT ||
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
    journal->applied_write_count = 0;
    return TRUE;
}

static BOOL
DSL_IR_Retype_Test_After_Write
        (DSL_IR_RETYPE_TEST_WRITE_CONTROL *control)
{
    if (control == NULL || !control->enabled)
        return TRUE;
    ++control->next_write;
    if (control->next_write != control->fail_after_write)
        return TRUE;
    if (control->diagnostic != NULL)
        fprintf(control->diagnostic,
                "DSL-SHAPE-RETYPE-INJECT: after_write=%u action=fail\n",
                control->next_write);
    return FALSE;
}

static UINT32
DSL_IR_Retype_Write_Count (const DSL_IR_RETYPE_JOURNAL &journal)
{
    return journal.reads.size() + 3;
}

static BOOL
DSL_IR_Retype_Test_Postcheck_Name_Valid (const char *name)
{
    return name == NULL || name[0] == '\0' ||
           strcmp(name, "active_boundary") == 0 ||
           strcmp(name, "dsl_image") == 0 ||
           strcmp(name, "region") == 0 ||
           strcmp(name, "gatekeeper") == 0 ||
           strcmp(name, "shared_shape") == 0 ||
           strcmp(name, "final") == 0;
}

static void
DSL_IR_Retype_Print_Write_Map
        (const std::vector<DSL_IR_RETYPE_JOURNAL> &journals,
         UINT32 selected,
         FILE *diagnostic)
{
    if (diagnostic == NULL)
        return;
    UINT32 ordinal = 0;
    for (UINT32 request = 0; request < journals.size(); ++request) {
        const DSL_IR_RETYPE_JOURNAL &journal = journals[request];
        for (UINT32 read = 0; read < journal.reads.size(); ++read) {
            ++ordinal;
            fprintf(diagnostic,
                    "DSL-SHAPE-RETYPE-WRITE-MAP: ordinal=%u request=%u "
                    "value=%u kind=read read_index=%u\n",
                    ordinal, request, journal.value.id, read);
        }
        ++ordinal;
        fprintf(diagnostic,
                "DSL-SHAPE-RETYPE-WRITE-MAP: ordinal=%u request=%u "
                "value=%u kind=definition read_index=-1\n",
                ordinal, request, journal.value.id);
        ++ordinal;
        fprintf(diagnostic,
                "DSL-SHAPE-RETYPE-WRITE-MAP: ordinal=%u request=%u "
                "value=%u kind=symbol read_index=-1\n",
                ordinal, request, journal.value.id);
        ++ordinal;
        fprintf(diagnostic,
                "DSL-SHAPE-RETYPE-WRITE-MAP: ordinal=%u request=%u "
                "value=%u kind=value read_index=-1\n",
                ordinal, request, journal.value.id);
    }
    fprintf(diagnostic,
            "DSL-SHAPE-RETYPE-WRITE-MAP: total=%u selected=%u\n",
            ordinal, selected);
}

static BOOL
DSL_IR_Retype_Apply
        (DSL_IR_RETYPE_JOURNAL *journal,
         TY_IDX from_ty,
         TY_IDX to_ty,
         DSL_IR_RETYPE_TEST_WRITE_CONTROL *control)
{
    FmtAssert(journal != NULL && journal->applied_write_count == 0,
              ("invalid DSL shape retype journal apply state"));
    for (UINT32 i = 0; i < journal->reads.size(); ++i) {
        WN_set_ty(journal->reads[i], to_ty);
        ++journal->applied_write_count;
        if (!DSL_IR_Retype_Test_After_Write(control))
            return FALSE;
    }
    WN_set_ty(journal->definition, to_ty);
    ++journal->applied_write_count;
    if (!DSL_IR_Retype_Test_After_Write(control))
        return FALSE;
    Set_ST_type(St_Table[journal->value.st], to_ty);
    ++journal->applied_write_count;
    if (!DSL_IR_Retype_Test_After_Write(control))
        return FALSE;
    BOOL changed = DSL_IR_Image_Retype_Value
                       (journal->value.id, from_ty, to_ty);
    FmtAssert(changed, ("preflighted DSL value retype failed"));
    ++journal->applied_write_count;
    if (!DSL_IR_Retype_Test_After_Write(control))
        return FALSE;
    return TRUE;
}

static BOOL
DSL_IR_Retype_Rollback (DSL_IR_RETYPE_JOURNAL *journal)
{
    if (journal == NULL)
        return FALSE;
    UINT32 read_count = journal->reads.size();
    UINT32 applied = journal->applied_write_count;
    if (applied > read_count + 3)
        return FALSE;
    if (applied > read_count + 2) {
        if (!DSL_IR_Image_Retype_Value
                 (journal->value.id, journal->request.refined_ty,
                  journal->request.expected_old_ty))
            return FALSE;
    }
    if (applied > read_count + 1)
        Set_ST_type(St_Table[journal->value.st],
                    journal->request.expected_old_ty);
    if (applied > read_count)
        WN_set_ty(journal->definition, journal->request.expected_old_ty);
    UINT32 applied_reads = applied < read_count ? applied : read_count;
    for (UINT32 i = applied_reads; i != 0; --i)
        WN_set_ty(journal->reads[i - 1],
                  journal->request.expected_old_ty);
    journal->applied_write_count = 0;
    return TRUE;
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
    if (result != NULL)
        *result = local_result;
    if (requests == NULL || request_count == 0) {
        return DSL_IR_Retype_Report
                   (diagnostic, "DSL-SHAPE-RETYPE-001", 0,
                    "empty request array");
    }
    DSL_IR_ACTIVE_PU_BOUNDARY_CONTEXT boundary;
    boundary.pu_info = pu_info;
    boundary.tree = tree;
    boundary.owner_pu_st = pu_info == NULL ? ST_IDX_ZERO :
                           PU_Info_proc_sym(pu_info);
    ++local_result.boundary_precheck_count;
    if (!DSL_IR_Image_Validate_Active_PU_Boundaries
             (&boundary, diagnostic)) {
        if (result != NULL)
            *result = local_result;
        return DSL_IR_Retype_Report
                   (diagnostic, "DSL-SHAPE-RETYPE-006", 0,
                    "active PU boundary is invalid before retyping");
    }

    for (UINT32 i = 0; i < request_count; ++i) {
        if (requests[i].value_id == DSL_IR_VALUE_INVALID_ID) {
            if (result != NULL)
                *result = local_result;
            return DSL_IR_Retype_Report
                       (diagnostic, "DSL-SHAPE-RETYPE-001",
                        requests[i].value_id, "request is malformed");
        }
        if (requests[i].owner_pu_st != boundary.owner_pu_st) {
            if (result != NULL)
                *result = local_result;
            return DSL_IR_Retype_Report
                       (diagnostic, "DSL-SHAPE-RETYPE-002",
                        requests[i].value_id,
                        "request owner does not match the active PU");
        }
        DSL_IR_VALUE_RECORD value;
        if (!DSL_IR_Image_Get_Value(requests[i].value_id, &value) ||
            value.ty != requests[i].expected_old_ty) {
            if (result != NULL)
                *result = local_result;
            return DSL_IR_Retype_Report
                       (diagnostic, "DSL-SHAPE-RETYPE-003",
                        requests[i].value_id,
                        "value or expected old type does not match");
        }
        if (!DSL_IR_Image_Value_Belongs_To_PU
                 (value, requests[i].owner_pu_st)) {
            if (result != NULL)
                *result = local_result;
            return DSL_IR_Retype_Report
                       (diagnostic, "DSL-SHAPE-RETYPE-002",
                        requests[i].value_id,
                        "logical value is not owned by the active PU");
        }
        if (!DSL_IR_Retype_Type_Valid
                 (requests[i].expected_old_ty, requests[i].refined_ty)) {
            if (result != NULL)
                *result = local_result;
            return DSL_IR_Retype_Report
                       (diagnostic, "DSL-SHAPE-RETYPE-004",
                        requests[i].value_id,
                        "refined type is not a monotonic shape-only refinement");
        }
        for (UINT32 prior = 0; prior < i; ++prior) {
            if (requests[prior].value_id == requests[i].value_id) {
                if (result != NULL)
                    *result = local_result;
                return DSL_IR_Retype_Report
                           (diagnostic, "DSL-SHAPE-RETYPE-001",
                            requests[i].value_id,
                            "duplicate value request");
            }
        }
    }

    std::vector<BOOL> matched_refinements(request_count, FALSE);
    DSL_IR_RETYPE_REFINEMENT_MATCH_CONTEXT match_context;
    match_context.requests = requests;
    match_context.request_count = request_count;
    match_context.matched = &matched_refinements;
    match_context.valid = TRUE;
    DSL_SHAPE_SOLVER_RESULT solver_result;
    memset(&solver_result, 0, sizeof(solver_result));
    BOOL semantic_admitted = DSL_Shape_Analyze_PU_With_Refinements
        (pu_info, tree, diagnostic, DSL_IR_Retype_Match_Refinement,
         &match_context, &solver_result);
    for (UINT32 i = 0; i < request_count && semantic_admitted; ++i)
        semantic_admitted = matched_refinements[i];
    if (!semantic_admitted || !match_context.valid) {
        if (result != NULL)
            *result = local_result;
        return DSL_IR_Retype_Report
                   (diagnostic, "DSL-SHAPE-RETYPE-006", 0,
                    "candidate refinement batch is not solver-authorized");
    }

    std::vector<DSL_IR_RETYPE_JOURNAL> journals(request_count);
    for (UINT32 i = 0; i < request_count; ++i) {
        if (!DSL_IR_Retype_Preflight
                 (pu_info, tree, requests[i], diagnostic, &journals[i])) {
            if (result != NULL)
                *result = local_result;
            return FALSE;
        }
        for (UINT32 prior = 0; prior < i; ++prior) {
            if (journals[prior].value.st == journals[i].value.st) {
                if (result != NULL)
                    *result = local_result;
                return DSL_IR_Retype_Report
                           (diagnostic, "DSL-SHAPE-RETYPE-001",
                            requests[i].value_id, "duplicate result symbol");
            }
        }
    }

    DSL_IR_RETYPE_TEST_WRITE_CONTROL write_control;
    memset(&write_control, 0, sizeof(write_control));
    const char *fail_after_write =
        getenv("OPEN64_DSL_SHAPE_RETYPE_TEST_FAIL_AFTER_WRITE");
    const char *force_postcheck =
        getenv("OPEN64_DSL_SHAPE_RETYPE_TEST_FAIL_POSTCHECK");
    UINT32 total_write_count = 0;
    for (UINT32 i = 0; i < request_count; ++i)
        total_write_count += DSL_IR_Retype_Write_Count(journals[i]);
    if (fail_after_write != NULL && fail_after_write[0] != '\0') {
        char *end = NULL;
        errno = 0;
        unsigned long selected = strtoul(fail_after_write, &end, 10);
        if (errno != 0 || end == fail_after_write || *end != '\0' ||
            selected > total_write_count) {
            if (result != NULL)
                *result = local_result;
            return DSL_IR_Retype_Report
                       (diagnostic, "DSL-SHAPE-RETYPE-001", 0,
                        "invalid test write-failure ordinal");
        }
        write_control.enabled = TRUE;
        write_control.fail_after_write = selected;
        write_control.diagnostic = diagnostic;
        DSL_IR_Retype_Print_Write_Map
            (journals, write_control.fail_after_write, diagnostic);
    }
    if (!DSL_IR_Retype_Test_Postcheck_Name_Valid(force_postcheck)) {
        if (result != NULL)
            *result = local_result;
        return DSL_IR_Retype_Report
                   (diagnostic, "DSL-SHAPE-RETYPE-001", 0,
                    "invalid test postcheck selector");
    }

    local_result.request_count = request_count;
    BOOL apply_valid = !write_control.enabled ||
                       write_control.fail_after_write != 0;
    if (!apply_valid && write_control.enabled && diagnostic != NULL)
        fprintf(diagnostic,
                "DSL-SHAPE-RETYPE-INJECT: after_write=0 action=fail\n");
    for (UINT32 i = 0; apply_valid && i < request_count; ++i) {
        apply_valid = DSL_IR_Retype_Apply
            (&journals[i], journals[i].request.expected_old_ty,
             journals[i].request.refined_ty, &write_control);
        if (!apply_valid)
            break;
        ++local_result.updated_st_count;
        local_result.updated_wn_count += 1 + journals[i].reads.size();
        ++local_result.updated_value_count;
    }

    DSL_GATEKEEPER_RESULT gatekeeper_result;
    const char *force_post_failure =
        getenv("OPEN64_DSL_SHAPE_RETYPE_TEST_POSTFAIL");
    BOOL valid = apply_valid;
    if (valid) {
        ++local_result.boundary_postcheck_count;
        valid = DSL_IR_Image_Validate_Active_PU_Boundaries
                    (&boundary, diagnostic) &&
                (force_postcheck == NULL ||
                 strcmp(force_postcheck, "active_boundary") != 0);
    }
    if (valid)
        valid = DSL_IR_Image_Validate(diagnostic) &&
                (force_postcheck == NULL ||
                 strcmp(force_postcheck, "dsl_image") != 0);
    if (valid)
        valid = DSL_Region_Verify_PU(pu_info, diagnostic) &&
                (force_postcheck == NULL ||
                 strcmp(force_postcheck, "region") != 0);
    if (valid)
        valid = DSL_Gatekeeper_Verify_PU_Mode
                    (pu_info, DSL_GATEKEEPER_STRICT, diagnostic,
                     &gatekeeper_result) &&
                (force_postcheck == NULL ||
                 strcmp(force_postcheck, "gatekeeper") != 0);
    if (valid) {
        DSL_SHAPE_SOLVER_RESULT shape_result;
        valid = DSL_Shape_Analyze_PU
                    (pu_info, tree, diagnostic, &shape_result) &&
                shape_result.refinable_value_count == 0 &&
                shape_result.pending_value_count == 0 &&
                shape_result.unresolved_value_count == 0 &&
                (force_postcheck == NULL ||
                 strcmp(force_postcheck, "shared_shape") != 0);
    }
    if (valid)
        valid = (force_postcheck == NULL ||
                 strcmp(force_postcheck, "final") != 0) &&
                (force_post_failure == NULL ||
                 strcmp(force_post_failure, "1") != 0);
    if (!valid && force_postcheck != NULL && diagnostic != NULL)
        fprintf(diagnostic,
                "DSL-SHAPE-RETYPE-POSTCHECK: name=%s action=fail\n",
                force_postcheck);
    if (!valid) {
        for (UINT32 i = request_count; i != 0; --i) {
            DSL_IR_RETYPE_JOURNAL &journal = journals[i - 1];
            if (journal.applied_write_count == 0)
                continue;
            BOOL restored = DSL_IR_Retype_Rollback(&journal);
            FmtAssert(restored, ("DSL shape retype rollback write failed"));
            ++local_result.rollback_count;
        }
        FmtAssert(DSL_IR_Image_Validate(diagnostic) &&
                  DSL_Region_Verify_PU(pu_info, diagnostic),
                  ("DSL shape retype rollback failed"));
        if (result != NULL)
            *result = local_result;
        return DSL_IR_Retype_Report
                   (diagnostic, "DSL-SHAPE-RETYPE-008", 0,
                    apply_valid ?
                    "strict post-verification failed; transaction rolled back" :
                    "injected commit write failure; transaction rolled back");
    }
    if (result != NULL)
        *result = local_result;
    return TRUE;
}
