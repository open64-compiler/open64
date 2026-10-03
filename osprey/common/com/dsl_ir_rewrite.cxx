/*
 * Copyright (C) 2026 Open64 Project
 *
 * Native DSL value lookup, external-tensor materialization, logical rewrite,
 * and redirect/retire transactions. See
 * doc/FHE-SYNC3-EXTERNAL-TENSOR-REWRITE-CONTRACT.md,
 * doc/FHE-SYNC5-NATIVE-OWNERSHIP-AUDIT.md, and
 * doc/FHE-SYNC5-STANDARD-WHIRL-LOWERING-CONTRACT.md.
 */

#include <ctype.h>
#include <string.h>
#include <errno.h>
#include <stdlib.h>
#include <algorithm>
#include <string>
#include <vector>

#include "dsl_memory_behavior.h"
#include "dsl_ckks_expand.h"
#include "dsl_ckks_event_internal.h"
#include "dsl_tensor_fold.h"
#include "dsl_ir_image.h"
#include "dsl_ir_transaction_internal.h"
#include "dsl_runtime_interface_internal.h"
#include "dsl_region.h"
#include "pu_info.h"
#include "strtab.h"
#include "symtab.h"
#include "wn.h"
#include "wn_util.h"

/* Commit-only helpers; public callers use the transactional APIs below. */
extern BOOL DSL_Call_ABI_Image_Update_Argument_Value
                                (const WN *, UINT32, DSL_IR_VALUE_ID,
                                 DSL_IR_VALUE_ID);
extern BOOL DSL_IR_Image_Redirect_And_Retire_Value
                                (DSL_IR_VALUE_ID, DSL_IR_VALUE_ID, UINT32);
extern BOOL DSL_IR_Image_Redirect_And_Lower_Value
                                (DSL_IR_VALUE_ID, DSL_IR_VALUE_ID);
extern UINT32 DSL_Region_Symbol_Use_Count (PU_Info *, ST_IDX);
extern BOOL DSL_Region_Can_Redirect_Symbol (PU_Info *, ST_IDX, ST_IDX);
extern BOOL DSL_Region_Redirect_Symbol (PU_Info *, ST_IDX, ST_IDX);

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

/* Validate a nonzero in-range canonical TY reference for interface rows. */
BOOL
DSL_Program_Interface_TY_Contract_Valid (TY_IDX ty)
{
    return ty != TY_IDX_ZERO && TY_IDX_index(ty) < TY_Table_Size();
}

/* Validate an interface TY as a pointer to a valid canonical pointee TY. */
BOOL
DSL_Program_Interface_Pointer_TY_Contract_Valid (TY_IDX ty)
{
    return DSL_Program_Interface_TY_Contract_Valid(ty) &&
           TY_kind(ty) == KIND_POINTER;
}

/* Validate that an interface TY is a sealed canonical tensor extension. */
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
 * Validate global PU-interface row identity, ownership, TY, and complete
 * uniqueness without requiring a callee local symtab.
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

struct DSL_PU_FORMAL_ORDINAL_LESS {
    BOOL operator()(const DSL_PU_FORMAL_RECORD &kid0,
                    const DSL_PU_FORMAL_RECORD &kid1) const
    { return kid0.formal_ordinal < kid1.formal_ordinal; }
};

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
 * Resolve structured owner identity for a value row before local ST access.
 * This read-only check rejects owner-name aliases and local-index collisions.
 */
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

/* Parse a complete decimal unsigned value with overflow/error rejection. */
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

/* Validate the canonical lowercase 64-hex SHA-256 representation. */
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

/* Map the closed persisted tensor dtype vocabulary to byte width. */
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

/*
 * Derive exact dense tensor byte size from rank, static dimensions, and dtype.
 * Dynamic, malformed, or overflowing descriptors fail closed.
 */
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

/* Return one canonical attribute value from a logical node, if present. */
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

/*
 * Resolve one external tensor value into a typed, borrowed side-file reference
 * plus canonical tensor TCON. It validates owner, descriptor, range, checksum,
 * and metadata agreement and performs no mutation.
 */
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

/*
 * Validate one persisted runtime-input row, including source tensor/TCON,
 * stable role, handle TY, and owner-sensitive value identity. This predicate
 * is shared by mapped-image validation and program-plan preflight.
 */
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

/* Find the innermost active-PU BLOCK that directly contains target. */
static WN *
DSL_IR_Find_Containing_Block (WN *tree, const WN *target)
{
    if (tree == NULL || target == NULL)
        return NULL;
    if (WN_operator(tree) == OPR_BLOCK) {
        for (WN *current = WN_first(tree); current != NULL;
             current = WN_next(current)) {
            if (current == target)
                return tree;
            WN *containing = DSL_IR_Find_Containing_Block(current, target);
            if (containing != NULL)
                return containing;
        }
        return NULL;
    }
    for (INT32 kid = 0; kid < WN_kid_count(tree); ++kid) {
        WN *containing = DSL_IR_Find_Containing_Block
                             (WN_kid(tree, kid), target);
        if (containing != NULL)
            return containing;
    }
    return NULL;
}

/*
 * Resolve a by-reference call actual to its exact owner-local DSL value row.
 * The call and argument metadata must agree with physical ST/TY identity.
 */
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

/* Check whether an owner PU already has a structured value with this name. */
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

/*
 * Preflight one converted external tensor request: source policy, call target,
 * TCON/path/range/checksum, type, shape, and provenance must all agree. It
 * records no symbols, values, or call edits.
 */
static BOOL
DSL_IR_External_Tensor_Request_Valid
        (ST_IDX owner_pu_st,
         const DSL_IR_EXTERNAL_TENSOR_MATERIALIZATION_REQUEST &request,
         const WN *insertion_block,
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
        insertion_block == NULL || request.insert_before == NULL ||
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

/* Initialize a request to the strict external-data-only source policy. */
void
DSL_IR_External_Tensor_Materialization_Request_Init
        (DSL_IR_EXTERNAL_TENSOR_MATERIALIZATION_REQUEST *request)
{
    if (request != NULL) {
        memset(request, 0, sizeof(*request));
        request->source_policy = DSL_IR_MATERIALIZE_SOURCE_EXTERNAL_ONLY;
    }
}

/*
 * Copy compiler metadata from source ST to a newly materialized tensor ST.
 * This commit helper runs only after the complete request array preflights.
 */
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

/*
 * Atomically materialize a complete same-PU array of converted external tensor
 * values and replace selected call actuals. All requests preflight together;
 * rejected arrays leave WN, ST, value, and call-ABI state unchanged.
 */
BOOL
DSL_IR_Materialize_External_Tensor_Values
        (PU_Info *pu_info,
         const DSL_IR_EXTERNAL_TENSOR_MATERIALIZATION_REQUEST *requests,
         UINT32 request_count,
         DSL_IR_EXTERNAL_TENSOR_MATERIALIZATION_RESULT *results)
{
    ST_IDX owner_pu_st = pu_info == NULL ? ST_IDX_ZERO :
        PU_Info_proc_sym(pu_info);
    WN *pu_root = pu_info == NULL ? NULL : PU_Info_tree_ptr(pu_info);
    if (pu_info != Current_PU_Info || pu_root == NULL ||
        !DSL_IR_Image_Current_PU_Is(owner_pu_st) || requests == NULL ||
        request_count == 0 || results == NULL)
        return FALSE;

    std::vector<DSL_IR_VALUE_RECORD> source_values(request_count);
    std::vector<std::string> uris(request_count);
    std::vector<std::string> payloads(request_count);
    std::vector<BOOL> update_call_abi(request_count, FALSE);
    std::vector<WN *> insertion_blocks(request_count, NULL);
    for (UINT32 i = 0; i < request_count; ++i) {
        insertion_blocks[i] = DSL_IR_Find_Containing_Block
                                  (pu_root, requests[i].insert_before);
        if (!DSL_IR_External_Tensor_Request_Valid
                 (owner_pu_st, requests[i], insertion_blocks[i],
                  &source_values[i], &uris[i], &payloads[i]) ||
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

        WN_INSERT_BlockBefore(insertion_blocks[i],
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
 * Atomically replace one native DSL expression while preserving definition,
 * result ST/TY, node/value identity, owner, source position, and operands.
 * Complete physical/logical preflight precedes the no-fail commit updates.
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

/*
 * Reject any retiring-symbol reference outside its containing BLOCK, enforcing
 * the v1 same-block dominance rule through a read-only PU traversal.
 */
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

/*
 * Atomically redirect all owner-safe uses to a dominating replacement and
 * retire one pure native definition. Tree, REGION interfaces, logical value
 * references, and status flags commit only after exhaustive preflight.
 */
BOOL
DSL_IR_Redirect_And_Retire_Native_Value
        (PU_Info *pu_info,
         const DSL_IR_NATIVE_VALUE_RETIRE_REQUEST *request)
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
    DSL_IR_Retire_Scan_Tree(containing_block, &scan);
    if (!scan.valid || !scan.retiring_seen || scan.definition_count != 1 ||
        DSL_IR_Retire_Has_Use_Outside_Block
            (pu_root, containing_block, retiring.st))
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
                      (containing_block,
                       request->retiring_definition);
    FmtAssert(removed == request->retiring_definition,
              ("preflighted DSL definition retirement failed"));
    WN_DELETE_Tree(removed);
    FmtAssert(DSL_IR_Image_Validate(NULL) &&
              DSL_Region_Verify_PU(Current_PU_Info, NULL),
              ("retired DSL value failed postcondition"));
    return TRUE;
}

static BOOL
DSL_IR_CKKS_Report (FILE *diagnostic, const char *message, UINT32 index)
{
    if (diagnostic != NULL)
        fprintf(diagnostic, "DSL CKKS expansion error: %s index=%u\n",
                message, index);
    return FALSE;
}

/* The opcode registry, not a frontend payload, owns exact static attributes. */
static BOOL
DSL_IR_CKKS_Attributes_Match
        (const DSL_OPERATOR_INFO &info,
         const DSL_CKKS_EXPANSION_STEP &step)
{
    const char *schema = info.attribute_schema == NULL ? "" :
                         info.attribute_schema;
    UINT32 required_count = 0;
    const char *cursor = schema;
    while (*cursor != '\0') {
        const char *end = strchr(cursor, ';');
        size_t length = end == NULL ? strlen(cursor) :
                                     (size_t)(end - cursor);
        if (length == 0)
            return FALSE;
        ++required_count;
        UINT32 matches = 0;
        for (UINT32 i = 0; i < step.attribute_count; ++i) {
            const DSL_CKKS_EXPANSION_ATTRIBUTE &attribute =
                step.attributes[i];
            if (attribute.name != NULL &&
                strlen(attribute.name) == length &&
                strncmp(attribute.name, cursor, length) == 0)
                ++matches;
        }
        if (matches != 1)
            return FALSE;
        if (end == NULL)
            break;
        cursor = end + 1;
    }
    if (required_count != step.attribute_count)
        return FALSE;
    for (UINT32 i = 0; i < step.attribute_count; ++i) {
        const DSL_CKKS_EXPANSION_ATTRIBUTE &attribute = step.attributes[i];
        if (attribute.name == NULL || attribute.value == NULL ||
            attribute.value[0] == '\0' ||
            strpbrk(attribute.name, ";=\n\r") != NULL ||
            strpbrk(attribute.value, ";\n\r") != NULL)
            return FALSE;
    }
    return TRUE;
}

static std::string
DSL_IR_CKKS_Payload
        (const DSL_CKKS_EXPANSION_REQUEST &request,
         UINT32 step_index)
{
    const DSL_CKKS_EXPANSION_STEP &step = request.steps[step_index];
    std::string payload;
    for (UINT32 i = 0; i < step.operand_count; ++i) {
        const DSL_CKKS_EXPANSION_OPERAND &operand = step.operands[i];
        const char *name = NULL;
        DSL_IR_VALUE_RECORD value;
        if (operand.kind == DSL_CKKS_EXPANSION_EXISTING_VALUE &&
            DSL_IR_Image_Get_Value(operand.value_id, &value))
            name = Index_To_Str(value.name);
        else if (operand.kind == DSL_CKKS_EXPANSION_PRIOR_STEP)
            name = request.steps[operand.step_index].result_name;
        char ordinal[32];
        snprintf(ordinal, sizeof(ordinal), "%u", i);
        if (!payload.empty())
            payload += ";";
        payload += "kid";
        payload += ordinal;
        payload += "=";
        payload += name == NULL ? "" : name;
    }
    for (UINT32 i = 0; i < step.attribute_count; ++i) {
        if (!payload.empty())
            payload += ";";
        payload += step.attributes[i].name;
        payload += "=";
        payload += step.attributes[i].value;
    }
    return payload;
}

typedef struct {
    WN *call;
    WN *address;
    UINT32 actual_ordinal;
} DSL_IR_CKKS_CALL_USE;

typedef struct {
    ST_IDX source_st;
    WN *source_definition;
    const std::vector<DSL_IR_CKKS_CALL_USE> *calls;
    BOOL source_seen;
    BOOL valid;
    UINT32 definition_count;
    std::vector<WN *> reads;
    std::vector<WN *> addresses;
} DSL_IR_CKKS_USE_SCAN;

/* The address exception is limited to an exact read-only call-ABI actual. */
static void
DSL_IR_CKKS_Scan_Tree (WN *wn, DSL_IR_CKKS_USE_SCAN *scan)
{
    if (wn == NULL || scan == NULL || !scan->valid)
        return;
    if (WN_operator(wn) == OPR_BLOCK) {
        for (WN *statement = WN_first(wn); statement != NULL;
             statement = WN_next(statement)) {
            if (statement == scan->source_definition) {
                if (scan->source_seen) {
                    scan->valid = FALSE;
                    return;
                }
                for (INT32 kid = 0; kid < WN_kid_count(statement); ++kid)
                    DSL_IR_CKKS_Scan_Tree(WN_kid(statement, kid), scan);
                ++scan->definition_count;
                scan->source_seen = TRUE;
            } else {
                DSL_IR_CKKS_Scan_Tree(statement, scan);
            }
        }
        return;
    }
    if (WN_has_sym(wn) && WN_st_idx(wn) == scan->source_st) {
        if (WN_operator(wn) == OPR_STID) {
            ++scan->definition_count;
            if (wn != scan->source_definition)
                scan->valid = FALSE;
        } else if (WN_operator(wn) == OPR_LDID) {
            if (scan->source_seen)
                scan->reads.push_back(wn);
            else
                scan->valid = FALSE;
        } else if (WN_operator(wn) == OPR_LDA) {
            BOOL allowed = FALSE;
            for (UINT32 i = 0; i < scan->calls->size(); ++i) {
                if ((*scan->calls)[i].address == wn) {
                    allowed = TRUE;
                    break;
                }
            }
            if (allowed && scan->source_seen)
                scan->addresses.push_back(wn);
            else
                scan->valid = FALSE;
        } else {
            scan->valid = FALSE;
        }
    }
    for (INT32 kid = 0; scan->valid && kid < WN_kid_count(wn); ++kid)
        DSL_IR_CKKS_Scan_Tree(WN_kid(wn, kid), scan);
}

static BOOL
DSL_IR_CKKS_Preflight_Calls
        (ST_IDX owner_pu_st, const DSL_IR_VALUE_RECORD &source,
         std::vector<DSL_IR_CKKS_CALL_USE> *uses, FILE *diagnostic)
{
    for (UINT32 i = 1; i <= DSL_Call_ABI_Image_Argument_Count(); ++i) {
        DSL_CALL_ARGUMENT_RECORD argument;
        if (!DSL_Call_ABI_Image_Get_Argument(i, &argument))
            return DSL_IR_CKKS_Report
                       (diagnostic, "missing call argument", i);
        if (argument.argument_value_id != source.id)
            continue;
        DSL_RETIRED_CALL_ARGUMENT_RECORD retired;
        if (DSL_Program_Interface_Image_Find_Retired_Call
                (argument.id, &retired))
            continue;
        DSL_CALLSITE_METADATA_RECORD callsite;
        const WN *call = DSL_Call_Image_Get_Call_WN(argument.callsite_id);
        if (!DSL_Call_Image_Get_Callsite
                (argument.callsite_id, &callsite) ||
            callsite.owner_pu_st != owner_pu_st || call == NULL)
            return DSL_IR_CKKS_Report
                       (diagnostic, "call owner mismatch", i);
        UINT32 effective_ordinal = argument.actual_ordinal;
        for (UINT32 j = 1;
             j <= DSL_Program_Interface_Image_Retired_Call_Count(); ++j) {
            DSL_RETIRED_CALL_ARGUMENT_RECORD retired_call;
            if (DSL_Program_Interface_Image_Get_Retired_Call
                    (j, &retired_call) &&
                retired_call.callsite_id == callsite.id &&
                retired_call.old_actual_ordinal < argument.actual_ordinal) {
                if (effective_ordinal == 0)
                    return DSL_IR_CKKS_Report
                               (diagnostic, "invalid call ordinal", i);
                --effective_ordinal;
            }
        }
        DSL_IR_VALUE_RECORD actual;
        if (effective_ordinal >= (UINT32)WN_kid_count(call) ||
            !DSL_IR_Value_From_Call_Actual
                (owner_pu_st, call, effective_ordinal, &actual) ||
            actual.id != source.id)
            return DSL_IR_CKKS_Report
                       (diagnostic, "call actual mismatch", i);
        const WN *parm = WN_kid(call, effective_ordinal);
        const WN *address = WN_kid0(parm);
        if (WN_Parm_Out(parm) ||
            TY_kind(WN_ty(parm)) != KIND_POINTER ||
            TY_pointed(WN_ty(parm)) != source.ty ||
            WN_st_idx(address) != source.st)
            return DSL_IR_CKKS_Report
                       (diagnostic, "call actual is not read-only", i);
        for (UINT32 j = 0; j < uses->size(); ++j) {
            if ((*uses)[j].address == address)
                return DSL_IR_CKKS_Report
                           (diagnostic, "duplicate call actual", i);
        }
        DSL_IR_CKKS_CALL_USE use;
        use.call = const_cast<WN *>(call);
        use.address = const_cast<WN *>(address);
        use.actual_ordinal = argument.actual_ordinal;
        uses->push_back(use);
    }
    return TRUE;
}

static BOOL
DSL_IR_CKKS_Preflight
        (PU_Info *pu_info, const DSL_CKKS_EXPANSION_REQUEST *request,
         DSL_IR_VALUE_RECORD *source_value, WN **source_block,
         std::vector<DSL_IR_CKKS_CALL_USE> *calls,
         DSL_IR_CKKS_USE_SCAN *scan, FILE *diagnostic)
{
    ST_IDX owner_pu_st = pu_info == NULL ? ST_IDX_ZERO :
                         PU_Info_proc_sym(pu_info);
    WN *pu_root = pu_info == NULL ? NULL : PU_Info_tree_ptr(pu_info);
    if (pu_info == NULL || request == NULL || source_value == NULL ||
        source_block == NULL || calls == NULL || scan == NULL ||
        pu_info != Current_PU_Info || pu_root == NULL ||
        !DSL_IR_Image_Current_PU_Is(owner_pu_st) ||
        request->source_definition == NULL ||
        request->groups == NULL || request->group_count == 0 ||
        request->steps == NULL || request->step_count == 0 ||
        request->contexts == NULL || request->context_count == 0 ||
        request->final_step_index >= request->step_count ||
        request->step_count >
            (~(UINT32)0 - DSL_CKKS_Event_Image_Count()) /
                request->context_count)
        return DSL_IR_CKKS_Report
                   (diagnostic, "invalid request or PU", 0);
    *source_block = DSL_IR_Find_Containing_Block
                        (pu_root, request->source_definition);
    DSL_IR_NODE_RECORD source_node;
    DSL_IR_OPCODE_DESCRIPTOR_RECORD source_opcode;
    if (*source_block == NULL ||
        !DSL_IR_Image_Find_Definition_Value
            (pu_info, request->source_definition, source_value) ||
        source_value->id != request->source_value_id ||
        source_value->flags != DSL_IR_VALUE_FLAG_NONE ||
        !DSL_IR_Image_Value_Belongs_To_PU
            (*source_value, owner_pu_st) ||
        !DSL_Tensor_Has_Unique_Ownership(source_value->st) ||
        !DSL_IR_Image_Get_Node
            (source_value->producer_node_id, &source_node) ||
        source_node.flags != DSL_IR_NODE_FLAG_NONE ||
        !DSL_IR_Image_Get_Opcode_Descriptor
            (source_node.opcode_descriptor_id, &source_opcode) ||
        source_opcode.logical_operator !=
            request->expected_source_operator ||
        source_opcode.version != request->expected_source_version ||
        source_opcode.effect_model != DSL_EFFECT_MODEL_PURE ||
        DSL_CKKS_Event_Image_Has_Source(source_value->id))
        return DSL_IR_CKKS_Report
                   (diagnostic, "source definition mismatch", 0);
    for (UINT32 i = 1; i <= DSL_Effect_Image_State_Effect_Count(); ++i) {
        DSL_STATE_EFFECT_RECORD effect;
        if (!DSL_Effect_Image_Get_State_Effect(i, &effect) ||
            effect.owner_node_id == source_node.id)
            return DSL_IR_CKKS_Report
                       (diagnostic, "source has state effect", i);
    }
    for (UINT32 i = 1;
         i <= DSL_Runtime_Interface_Image_Value_Count(); ++i) {
        DSL_RUNTIME_VALUE_PROJECTION_RECORD projection;
        if (!DSL_Runtime_Interface_Image_Get_Value(i, &projection) ||
            projection.source_value_id == source_value->id)
            return DSL_IR_CKKS_Report
                       (diagnostic, "source has runtime projection", i);
    }
    for (UINT32 i = 1;
         i <= DSL_Program_Interface_Image_Runtime_Input_Count(); ++i) {
        DSL_RUNTIME_INPUT_RECORD input;
        if (!DSL_Program_Interface_Image_Get_Runtime_Input(i, &input) ||
            input.source_value_id == source_value->id)
            return DSL_IR_CKKS_Report
                       (diagnostic, "source has runtime input", i);
    }

    UINT32 next_step = 0;
    UINT32 prior_static_ordinal = 0;
    for (UINT32 i = 0; i < request->group_count; ++i) {
        const DSL_CKKS_EXPANSION_GROUP &group = request->groups[i];
        if (group.source_static_ordinal == 0 ||
            group.origin_static_ordinal == 0 ||
            group.source_static_ordinal <= prior_static_ordinal ||
            group.first_step != next_step || group.step_count == 0 ||
            group.step_count > request->step_count - next_step)
            return DSL_IR_CKKS_Report
                       (diagnostic, "invalid event group", i);
        prior_static_ordinal = group.source_static_ordinal;
        next_step += group.step_count;
    }
    if (next_step != request->step_count ||
        request->steps[request->final_step_index].result_ty !=
            source_value->ty)
        return DSL_IR_CKKS_Report
                   (diagnostic, "incomplete groups or final TY", 0);

    for (UINT32 i = 0; i < request->step_count; ++i) {
        const DSL_CKKS_EXPANSION_STEP &step = request->steps[i];
        DSL_OPERATOR_INFO info;
        if (step.dsl_operator < OPR_DSLCKKSADD ||
            step.dsl_operator > OPR_DSLCKKSBOOTSTRAP ||
            !DSL_Operator_Get_Info_Version
                (step.dsl_operator, step.version, &info) ||
            info.nkids < 0 ||
            (UINT32)info.nkids != step.operand_count ||
            info.effect_model != DSL_EFFECT_MODEL_PURE ||
            (step.operand_count != 0 && step.operands == NULL) ||
            (step.attribute_count != 0 && step.attributes == NULL) ||
            !DSL_IR_CKKS_Attributes_Match(info, step) ||
            step.result_name == NULL || step.result_name[0] == '\0' ||
            strpbrk(step.result_name, ";=\n\r") != NULL ||
            !TY_is_tensor_extension(step.result_ty) ||
            !TY_tensor_is_canonical(step.result_ty) ||
            SRCPOS_linenum(step.source_position) == 0 ||
            DSL_IR_PU_Value_Name_Exists
                (owner_pu_st, step.result_name))
            return DSL_IR_CKKS_Report
                       (diagnostic, "invalid CKKS step", i);
        for (UINT32 prior = 0; prior < i; ++prior) {
            if (strcmp(request->steps[prior].result_name,
                       step.result_name) == 0)
                return DSL_IR_CKKS_Report
                           (diagnostic, "duplicate result name", i);
        }
        for (UINT32 kid = 0; kid < step.operand_count; ++kid) {
            const DSL_CKKS_EXPANSION_OPERAND &operand = step.operands[kid];
            if (operand.kind == DSL_CKKS_EXPANSION_PRIOR_STEP) {
                if (operand.step_index >= i || operand.value_id != 0)
                    return DSL_IR_CKKS_Report
                               (diagnostic, "forward step operand", i);
                continue;
            }
            DSL_IR_VALUE_RECORD value;
            if (operand.kind != DSL_CKKS_EXPANSION_EXISTING_VALUE ||
                operand.step_index != 0 ||
                operand.value_id == source_value->id ||
                !DSL_IR_Image_Get_Value(operand.value_id, &value) ||
                value.flags != DSL_IR_VALUE_FLAG_NONE ||
                !DSL_IR_Image_Value_Belongs_To_PU
                    (value, owner_pu_st) ||
                ST_IDX_level(value.st) != CURRENT_SYMTAB ||
                ST_IDX_index(value.st) == 0 ||
                ST_IDX_index(value.st) >= ST_Table_Size(CURRENT_SYMTAB) ||
                ST_type(St_Table[value.st]) != value.ty)
                return DSL_IR_CKKS_Report
                           (diagnostic, "invalid existing operand", i);
        }
    }

    for (UINT32 i = 0; i < request->context_count; ++i) {
        const DSL_CKKS_EXPANSION_CONTEXT &context = request->contexts[i];
        DSL_PU_SOURCE_IDENTITY_RECORD identity;
        DSL_IR_VALUE_RECORD origin;
        DSL_CALLSITE_METADATA_RECORD callsite;
        if (!DSL_Call_Image_Get_PU_Identity
                (context.context_pu_identity_id, &identity) ||
            identity.owner_pu_st != owner_pu_st ||
            !DSL_IR_Image_PU_ST_Valid(context.origin_owner_pu_st) ||
            !DSL_IR_Image_Get_Value
                (context.origin_source_value_id, &origin) ||
            !DSL_IR_Image_Value_Belongs_To_PU
                (origin, context.origin_owner_pu_st) ||
            (context.context_callsite_id != 0 &&
             (!DSL_Call_Image_Get_Callsite
                  (context.context_callsite_id, &callsite) ||
              callsite.callee_pu_st != owner_pu_st ||
              !DSL_IR_Image_PU_ST_Valid(callsite.owner_pu_st))))
            return DSL_IR_CKKS_Report
                       (diagnostic, "invalid source context", i);
        for (UINT32 prior = 0; prior < i; ++prior) {
            if (request->contexts[prior].context_pu_identity_id ==
                    context.context_pu_identity_id &&
                request->contexts[prior].context_callsite_id ==
                    context.context_callsite_id)
                return DSL_IR_CKKS_Report
                           (diagnostic, "duplicate source context", i);
        }
    }
    if (!DSL_Region_Verify_PU(pu_info, diagnostic) ||
        !DSL_Call_ABI_Image_Validate_PU(pu_info, diagnostic) ||
        !DSL_IR_CKKS_Preflight_Calls
            (owner_pu_st, *source_value, calls, diagnostic))
        return FALSE;
    scan->source_st = source_value->st;
    scan->source_definition = request->source_definition;
    scan->calls = calls;
    scan->source_seen = FALSE;
    scan->valid = TRUE;
    scan->definition_count = 0;
    DSL_IR_CKKS_Scan_Tree(*source_block, scan);
    if (!scan->valid || !scan->source_seen ||
        scan->definition_count != 1 ||
        scan->addresses.size() != calls->size() ||
        DSL_IR_Retire_Has_Use_Outside_Block
            (pu_root, *source_block, source_value->st))
        return DSL_IR_CKKS_Report
                   (diagnostic, "source use is not redirectable", 0);
    return TRUE;
}

BOOL
DSL_IR_Can_Expand_Native_Value_To_CKKS_Events
        (PU_Info *pu_info, const DSL_CKKS_EXPANSION_REQUEST *request,
         FILE *diagnostic)
{
    DSL_IR_VALUE_RECORD source_value;
    WN *source_block = NULL;
    std::vector<DSL_IR_CKKS_CALL_USE> calls;
    DSL_IR_CKKS_USE_SCAN scan;
    return DSL_IR_CKKS_Preflight
               (pu_info, request, &source_value, &source_block,
                &calls, &scan, diagnostic);
}
