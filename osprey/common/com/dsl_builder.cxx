/*
 * Copyright (C) 2026 Open64 Project
 */

#ifdef USE_PCH
#include "common_com_pch.h"
#endif /* USE_PCH */
#pragma hdrstop
#include <algorithm>
#include <ctype.h>
#include <errno.h>
#include <float.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <string>
#include <vector>
#if ! defined(BUILD_OS_DARWIN)
#include <elf.h>
#endif /* ! defined(BUILD_OS_DARWIN) */

#include "dsl_builder.h"
#include "dsl_fhe.h"
#include "dsl_fhe_plan.h"
#include "dsl_gatekeeper.h"
#include "dsl_memory_behavior.h"
#include "dsl_simp.h"
#include "dsl_tensor_fold.h"
#include "config.h"
#include "const.h"
#include "dwarf_DST.h"
#include "dwarf_DST_mem.h"
#include "dwarf_DST_producer.h"
#include "glob.h"
#include "pu_info.h"
#include "ir_bwrite.h"
#include "mempool.h"
#include "stab.h"
#include "strtab.h"
#include "symtab_utils.h"
#include "wn_util.h"

static PU_Info *DSL_Builder_PU_Root = NULL;
static PU_Info *DSL_Builder_PU_Last = NULL;
static PU_Info *DSL_Builder_Active_PU = NULL;
static UINT32 DSL_Builder_Result_Number = 0;
static DST_INFO_IDX DSL_Builder_CU_DST = DST_INVALID_INIT;
static std::vector<std::string> DSL_builder_source_files;
static std::vector<std::string> DSL_builder_source_directories;

typedef struct {
    DSL_BUILDER_PROGRAM_UNIT pu;
    WN *assignment;
    WN *expression;
    ST_IDX result_st;
    TY_IDX result_ty;
    DSL_IR_VALUE_ID image_value_id;
    UINT32 value_kind;
    TCON_IDX tensor_tcon;
    std::string canonical_key;
    BOOL materializing;
    BOOL materialized;
} DSL_BUILDER_VALUE_RECORD;

static std::vector<DSL_BUILDER_VALUE_RECORD> DSL_builder_value_registry;
static BOOL DSL_builder_canonicalization_enabled = FALSE;
static BOOL DSL_builder_tensor_folding_enabled = FALSE;

struct dsl_builder_state {
    DSL_BUILDER_PROGRAM_UNIT pu;
    DSL_STATE_OBJECT_ID image_state_id;
    ST_IDX st;
};

static std::vector<dsl_builder_state *> DSL_builder_state_registry;

struct dsl_builder_pu_interface_value {
    DSL_BUILDER_VALUE value;
    ST_IDX st;
    TY_IDX ty;
    UINT32 ordinal;
    DSL_BUILDER_PU_RESULT_ROLE role;
};

struct dsl_builder_pu_interface {
    DSL_BUILDER_PROGRAM_UNIT pu;
    std::vector<dsl_builder_pu_interface_value> formals;
    std::vector<dsl_builder_pu_interface_value> results;
    BOOL has_return;
};

struct dsl_builder_pu_source_identity {
    DSL_BUILDER_PROGRAM_UNIT pu;
    std::string canonical_definition_name;
    std::string defining_module;
    std::string defining_file;
    UINT32 defining_line;
    UINT32 flags;
};

struct dsl_builder_call_record {
    DSL_BUILDER_CALL call;
    DSL_BUILDER_PROGRAM_UNIT caller;
    DSL_BUILDER_PROGRAM_UNIT callee;
    std::string canonical_class_name;
    std::string instance_path;
    std::string context_identity;
    UINT32 call_ordinal;
    DSL_BUILDER_SOURCE_POSITION source_position;
    std::vector<DSL_BUILDER_VALUE> arguments;
    std::vector<DSL_BUILDER_VALUE> results;
};

static std::vector<dsl_builder_pu_interface *> DSL_builder_pu_interfaces;
static std::vector<dsl_builder_pu_source_identity *>
    DSL_builder_pu_source_identities;
static std::vector<dsl_builder_call_record *> DSL_builder_call_registry;

static void
DSL_Builder_Reset_Program (void)
{
    for (UINT32 i = 0; i < DSL_builder_state_registry.size(); ++i)
        delete DSL_builder_state_registry[i];
    for (UINT32 i = 0; i < DSL_builder_pu_interfaces.size(); ++i)
        delete DSL_builder_pu_interfaces[i];
    for (UINT32 i = 0; i < DSL_builder_pu_source_identities.size(); ++i)
        delete DSL_builder_pu_source_identities[i];
    for (UINT32 i = 0; i < DSL_builder_call_registry.size(); ++i)
        delete DSL_builder_call_registry[i];
    DSL_Builder_PU_Root = NULL;
    DSL_Builder_PU_Last = NULL;
    DSL_Builder_Active_PU = NULL;
    Current_PU_Info = NULL;
    DSL_Builder_Result_Number = 0;
    DSL_Builder_CU_DST = DST_INVALID_IDX;
    DSL_builder_source_files.clear();
    DSL_builder_source_directories.clear();
    DSL_builder_value_registry.clear();
    DSL_builder_state_registry.clear();
    DSL_builder_pu_interfaces.clear();
    DSL_builder_pu_source_identities.clear();
    DSL_builder_call_registry.clear();
    DSL_IR_Image_Reset();
    DSL_FHE_Image_Reset();
    DSL_FHE_Plan_Image_Reset();
    DSL_Region_Reset();
}

static dsl_builder_pu_interface *
DSL_Builder_Find_PU_Interface (DSL_BUILDER_PROGRAM_UNIT pu)
{
    for (UINT32 i = 0; i < DSL_builder_pu_interfaces.size(); ++i) {
        if (DSL_builder_pu_interfaces[i]->pu == pu)
            return DSL_builder_pu_interfaces[i];
    }
    return NULL;
}

static dsl_builder_call_record *
DSL_Builder_Find_Call_Record (DSL_BUILDER_CALL call)
{
    for (UINT32 i = 0; i < DSL_builder_call_registry.size(); ++i) {
        if (DSL_builder_call_registry[i]->call == call)
            return DSL_builder_call_registry[i];
    }
    return NULL;
}

static dsl_builder_pu_source_identity *
DSL_Builder_Find_PU_Source_Identity (DSL_BUILDER_PROGRAM_UNIT pu)
{
    for (UINT32 i = 0; i < DSL_builder_pu_source_identities.size(); ++i) {
        if (DSL_builder_pu_source_identities[i]->pu == pu)
            return DSL_builder_pu_source_identities[i];
    }
    return NULL;
}

static DSL_BUILDER_VALUE_RECORD *
DSL_Builder_Find_Value_Record (DSL_BUILDER_VALUE value)
{
    for (UINT32 i = 0; i < DSL_builder_value_registry.size(); ++i) {
        if (DSL_builder_value_registry[i].assignment == value)
            return &DSL_builder_value_registry[i];
    }

    return NULL;
}

static DSL_BUILDER_VALUE_RECORD *
DSL_Builder_Find_Value_Record_By_ST (ST_IDX st)
{
    for (UINT32 i = 0; i < DSL_builder_value_registry.size(); ++i) {
        if (DSL_builder_value_registry[i].pu == DSL_Builder_Active_PU &&
            DSL_builder_value_registry[i].result_st == st)
            return &DSL_builder_value_registry[i];
    }

    return NULL;
}

static BOOL
DSL_Builder_PU_Is_Registered (DSL_BUILDER_PROGRAM_UNIT pu)
{
    for (PU_Info *current = DSL_Builder_PU_Root; current != NULL;
         current = PU_Info_next(current)) {
        if (current == pu)
            return TRUE;
    }
    return FALSE;
}

BOOL
DSL_Builder_Select_PU (DSL_BUILDER_PROGRAM_UNIT pu)
{
    if (!DSL_Builder_PU_Is_Registered(pu) ||
        PU_Info_state(pu, WT_SYMTAB) != Subsect_InMem ||
        PU_Info_symtab_ptr(pu) == NULL)
        return FALSE;

    ST_IDX proc_st = PU_Info_proc_sym(pu);
    Current_pu = &Pu_Table[ST_pu(St_Table[proc_st])];
    Current_scope = Current_pu->lexical_level;
    Restore_Local_Symtab(pu);
    Current_Map_Tab = PU_Info_maptab(pu);
    Current_PU_Info = pu;
    DSL_Builder_Active_PU = pu;
    return TRUE;
}

static BOOL
DSL_Builder_Select_Value_PU (DSL_BUILDER_VALUE_RECORD *record)
{
    if (record == NULL)
        return FALSE;
    if (record->pu != NULL)
        return DSL_Builder_Select_PU(record->pu);
    return DSL_Builder_Active_PU == NULL && DSL_Builder_PU_Root == NULL;
}

static dsl_builder_state *
DSL_Builder_Find_State (DSL_BUILDER_STATE state)
{
    for (UINT32 i = 0; i < DSL_builder_state_registry.size(); ++i) {
        if (DSL_builder_state_registry[i] == state)
            return DSL_builder_state_registry[i];
    }

    return NULL;
}

static WN *
DSL_Builder_Value_Expression (DSL_BUILDER_VALUE value)
{
    DSL_BUILDER_VALUE_RECORD *record = DSL_Builder_Find_Value_Record(value);

    if (record != NULL)
        return record->expression;
    return value;
}

static BOOL
DSL_Builder_Get_Value_Annotation
        (DSL_BUILDER_VALUE value,
         DSL_OPCODE_ANNOTATION *annotation)
{
    return DSL_WN_Get_Opcode_Annotation
               (DSL_Builder_Value_Expression(value), annotation);
}

static WN *
DSL_Builder_PU_Body (DSL_BUILDER_PROGRAM_UNIT pu)
{
    WN *entry;
    WN *body;

    if (pu == NULL || PU_Info_state(pu, WT_TREE) != Subsect_InMem)
        return NULL;

    entry = PU_Info_tree_ptr(pu);
    if (entry == NULL || WN_operator(entry) != OPR_FUNC_ENTRY)
        return NULL;

    body = WN_func_body(entry);
    if (body == NULL || WN_operator(body) != OPR_BLOCK)
        return NULL;

    return body;
}

static BOOL
DSL_Builder_Materialize_Value_In_Block
        (DSL_BUILDER_PROGRAM_UNIT pu,
         WN *body,
         DSL_BUILDER_VALUE_RECORD *record)
{
    if (body == NULL || WN_operator(body) != OPR_BLOCK || record == NULL ||
        record->pu != pu)
        return FALSE;
    if (record->materialized)
        return TRUE;
    if (record->materializing || record->assignment == NULL ||
        record->expression == NULL ||
        !DSL_Builder_Get_Value_Annotation(record->assignment, NULL))
        return FALSE;

    record->materializing = TRUE;
    for (UINT32 i = 0; i < WN_kid_count(record->expression); ++i) {
        WN *kid = WN_kid(record->expression, i);
        if (kid == NULL || WN_operator(kid) != OPR_LDID) {
            record->materializing = FALSE;
            return FALSE;
        }
        DSL_BUILDER_VALUE_RECORD *dependency =
            DSL_Builder_Find_Value_Record_By_ST(WN_st_idx(kid));
        if (dependency == NULL ||
            !DSL_Builder_Materialize_Value_In_Block
                 (pu, body, dependency)) {
            record->materializing = FALSE;
            return FALSE;
        }
    }

    WN_INSERT_BlockLast(body, record->assignment);
    record->materializing = FALSE;
    record->materialized = TRUE;
    return TRUE;
}

static BOOL
DSL_Builder_Materialize_PU_Value
        (DSL_BUILDER_PROGRAM_UNIT pu,
         DSL_BUILDER_VALUE_RECORD *record)
{
    return DSL_Builder_Materialize_Value_In_Block
               (pu, DSL_Builder_PU_Body(pu), record);
}

static BOOL
DSL_Builder_Source_Position_Valid
        (const DSL_BUILDER_SOURCE_POSITION *source_position)
{
    return source_position != NULL && source_position->file_id != 0 &&
           source_position->file_id <= DSL_builder_source_files.size() &&
           source_position->line >= 0 && source_position->column <= 4095;
}

static SRCPOS
DSL_Builder_Source_Position
        (const DSL_BUILDER_SOURCE_POSITION *source_position)
{
    USRCPOS position;
    USRCPOS_clear(position);
    USRCPOS_filenum(position) = source_position->file_id;
    USRCPOS_linenum(position) = source_position->line;
    USRCPOS_column(position) = source_position->column;
    USRCPOS_stmt_begin(position) = source_position->statement_begin != 0;
    USRCPOS_bb_begin(position) = source_position->basic_block_begin != 0;
    return USRCPOS_srcpos(position);
}

static DSL_IR_VALUE_ID
DSL_Builder_Add_Image_Symbol_Value
        (ST_IDX st,
         TY_IDX ty,
         UINT32 value_kind)
{
    DSL_IR_VALUE_RECORD record;
    DSL_IR_Value_Record_Init(&record);
    record.value_kind = value_kind;
    record.ty = ty;
    record.st = st;
    record.name = Save_Str(ST_name(St_Table[st]));
    if (DSL_Builder_Active_PU != NULL) {
        std::string metadata = "owner_pu=";
        metadata += ST_name
                        (St_Table[PU_Info_proc_sym(DSL_Builder_Active_PU)]);
        record.metadata = Save_Str(metadata.c_str());
    }
    return DSL_IR_Image_Add_Value(&record);
}

static TY_IDX
DSL_Builder_Create_PU_Function_Type
        (const dsl_builder_pu_interface *interface_record)
{
    TY_IDX function_ty;
    TY &function = New_TY(function_ty);
    TY_Init(function, 0, KIND_FUNCTION, MTYPE_UNKNOWN, 0);
    Set_TY_align(function_ty, 1);

    TYLIST_IDX tylist_idx;
    Set_TYLIST_type(New_TYLIST(tylist_idx), MTYPE_To_TY(MTYPE_V));
    Set_TY_tylist(function_ty, tylist_idx);
    for (UINT32 i = 0; i < interface_record->formals.size(); ++i)
        Set_TYLIST_type(New_TYLIST(tylist_idx),
                        interface_record->formals[i].ty);
    for (UINT32 i = 0; i < interface_record->results.size(); ++i)
        Set_TYLIST_type(New_TYLIST(tylist_idx),
                        Make_Pointer_Type(interface_record->results[i].ty));
    Set_TYLIST_type(New_TYLIST(tylist_idx), TY_IDX_ZERO);
    return function_ty;
}

static BOOL
DSL_Builder_Rebuild_PU_Entry (dsl_builder_pu_interface *interface_record)
{
    if (interface_record == NULL ||
        !DSL_Builder_Select_PU(interface_record->pu))
        return FALSE;

    WN *old_entry = PU_Info_tree_ptr(interface_record->pu);
    if (old_entry == NULL || WN_operator(old_entry) != OPR_FUNC_ENTRY)
        return FALSE;
    UINT32 formal_count = interface_record->formals.size();
    UINT32 result_count = interface_record->results.size();
    if (formal_count + result_count > 32767)
        return FALSE;

    WN *entry = WN_CreateEntry
                    ((INT16)(formal_count + result_count),
                     PU_Info_proc_sym(interface_record->pu),
                     WN_func_body(old_entry), WN_func_pragmas(old_entry),
                     WN_func_varrefs(old_entry));
    for (UINT32 i = 0; i < formal_count; ++i)
        WN_formal(entry, i) = WN_CreateIdname
                                  (0, interface_record->formals[i].st);
    for (UINT32 i = 0; i < result_count; ++i)
        WN_formal(entry, formal_count + i) = WN_CreateIdname
                                                 (0,
                                                  interface_record->results[i].st);

    ST_IDX proc_st = PU_Info_proc_sym(interface_record->pu);
    TY_IDX function_ty =
        DSL_Builder_Create_PU_Function_Type(interface_record);
    Set_PU_prototype(Pu_Table[ST_pu(St_Table[proc_st])], function_ty);
    Set_PU_Info_tree_ptr(interface_record->pu, entry);
    return TRUE;
}

static const char *
DSL_Builder_Safe_String (const char *value)
{
    return value == NULL ? "" : value;
}

static void
DSL_Builder_Append_Canonical_Field
        (std::string *key,
         const char *name,
         const char *value)
{
    char length[32];
    const char *safe_value = DSL_Builder_Safe_String(value);

    snprintf(length, sizeof(length), "%u",
             (unsigned int)strlen(safe_value));
    *key += name;
    *key += ":";
    *key += length;
    *key += ":";
    *key += safe_value;
    *key += ";";
}

static std::string
DSL_Builder_Tensor_Canonical_Key (TY_IDX ty)
{
    static const TY_TENSOR_SCHEMA_KEY semantic_key[] = {
        TY_TENSOR_SCHEMA_KIND,
        TY_TENSOR_SCHEMA_DTYPE,
        TY_TENSOR_SCHEMA_RANK,
        TY_TENSOR_SCHEMA_SHAPE,
        TY_TENSOR_SCHEMA_TRAITS,
        TY_TENSOR_SCHEMA_LAYOUT,
        TY_TENSOR_SCHEMA_SHARDING,
        TY_TENSOR_SCHEMA_PLACEMENT,
        TY_TENSOR_SCHEMA_MEMORY,
        TY_TENSOR_SCHEMA_QUANTIZATION,
        TY_TENSOR_SCHEMA_RUNTIME_STATE
    };
    std::string key;
    char element_type[32];

    if (!TY_is_tensor_extension(ty))
        return key;

    snprintf(element_type, sizeof(element_type), "%u",
             (unsigned int)TY_mtype(TY_tensor_element_ty(ty)));
    DSL_Builder_Append_Canonical_Field
        (&key, "element_type", element_type);
    for (UINT32 i = 0;
         i < sizeof(semantic_key) / sizeof(semantic_key[0]); ++i) {
        const char *name = TY_tensor_schema_key_name(semantic_key[i]);
        const char *value = TY_tensor_attribute(ty, semantic_key[i]);
        DSL_Builder_Append_Canonical_Field(&key, name, value);
    }

    return key;
}

static std::string
DSL_Builder_Attribute_Canonical_Key
        (const DSL_BUILDER_OPERATOR_ATTRIBUTE *attrs,
         UINT32 attr_count)
{
    std::vector<std::string> fields;
    std::string key;

    for (UINT32 i = 0; attrs != NULL && i < attr_count; ++i) {
        std::string field;
        DSL_Builder_Append_Canonical_Field
            (&field, DSL_Builder_Safe_String(attrs[i].name),
             attrs[i].value);
        fields.push_back(field);
    }
    std::sort(fields.begin(), fields.end());
    for (UINT32 i = 0; i < fields.size(); ++i)
        key += fields[i];
    return key;
}

static std::string
DSL_Builder_Value_Canonical_Key
        (DSL_OPERATOR dsl_operator,
         UINT16 version,
         TY_IDX result_ty,
         DSL_BUILDER_VALUE_RECORD **operand_records,
         UINT32 operand_count,
         const DSL_BUILDER_OPERATOR_ATTRIBUTE *attrs,
         UINT32 attr_count,
         const char *result_name)
{
    std::string key;
    char version_text[32];

    DSL_Builder_Append_Canonical_Field
        (&key, "operator", DSL_OPERATOR_name(dsl_operator));
    snprintf(version_text, sizeof(version_text), "%u",
             (unsigned int)version);
    DSL_Builder_Append_Canonical_Field(&key, "version", version_text);
    key += DSL_Builder_Tensor_Canonical_Key(result_ty);
    key += DSL_Builder_Attribute_Canonical_Key(attrs, attr_count);
    for (UINT32 i = 0; i < operand_count; ++i)
        DSL_Builder_Append_Canonical_Field
            (&key, "operand", operand_records[i]->canonical_key.c_str());
    if (operand_count == 0)
        DSL_Builder_Append_Canonical_Field(&key, "name", result_name);
    return key;
}

static BOOL
DSL_Builder_Is_Integer_Tensor (TY_IDX ty)
{
    return TY_is_tensor_extension(ty) &&
           MTYPE_is_integral(TY_mtype(TY_tensor_element_ty(ty)));
}

static BOOL
DSL_Builder_Canonicalize_Kids
        (DSL_OPERATOR dsl_operator,
         UINT16 version,
         DSL_BUILDER_VALUE *kids,
         UINT32 kid_count)
{
    DSL_BUILDER_VALUE_RECORD *kid0;
    DSL_BUILDER_VALUE_RECORD *kid1;
    BOOL kid0_constant;
    BOOL kid1_constant;
    BOOL equivalent_descriptors;
    BOOL swap;
    std::string kid0_descriptor_key;
    std::string kid1_descriptor_key;

    if (!DSL_Builder_Canonicalization_Enabled() || kids == NULL ||
        kid_count != 2)
        return FALSE;

    kid0 = DSL_Builder_Find_Value_Record(kids[0]);
    kid1 = DSL_Builder_Find_Value_Record(kids[1]);
    if (kid0 == NULL || kid1 == NULL ||
        !DSL_Builder_Is_Integer_Tensor(kid0->result_ty) ||
        !DSL_Builder_Is_Integer_Tensor(kid1->result_ty))
        return FALSE;

    kid0_descriptor_key =
        DSL_Builder_Tensor_Canonical_Key(kid0->result_ty);
    kid1_descriptor_key =
        DSL_Builder_Tensor_Canonical_Key(kid1->result_ty);
    equivalent_descriptors =
        !kid0_descriptor_key.empty() &&
        kid0_descriptor_key == kid1_descriptor_key;
    kid0_constant = kid0->value_kind == DSL_IR_VALUE_CONSTANT;
    kid1_constant = kid1->value_kind == DSL_IR_VALUE_CONSTANT;
    swap = DSL_Algebraic_Should_Swap_Binary_Operands
               (dsl_operator, version, TRUE, equivalent_descriptors,
                kid0_constant, kid1_constant,
                kid0->canonical_key.c_str(), kid1->canonical_key.c_str());

    if (swap) {
        DSL_BUILDER_VALUE temporary = kids[0];
        kids[0] = kids[1];
        kids[1] = temporary;
    }
    return swap;
}

void
DSL_Builder_Set_Canonicalization_Enabled (BOOL enabled)
{
    DSL_builder_canonicalization_enabled = enabled;
}

BOOL
DSL_Builder_Canonicalization_Enabled (void)
{
    return Enable_WN_Simp && DSL_builder_canonicalization_enabled;
}

void
DSL_Builder_Set_Tensor_Folding_Enabled (BOOL enabled)
{
    DSL_builder_tensor_folding_enabled = enabled;
}

BOOL
DSL_Builder_Tensor_Folding_Enabled (void)
{
    return Enable_WN_Simp && DSL_builder_tensor_folding_enabled;
}

BOOL
DSL_Builder_Set_PU_Source_Identity
        (DSL_BUILDER_PROGRAM_UNIT pu,
         const DSL_BUILDER_PU_SOURCE_IDENTITY *identity)
{
    if (!DSL_Builder_PU_Is_Registered(pu) || identity == NULL ||
        identity->canonical_definition_name == NULL ||
        identity->canonical_definition_name[0] == '\0' ||
        identity->defining_file == NULL || identity->defining_file[0] == '\0' ||
        identity->defining_line == 0 ||
        DSL_Builder_Find_PU_Source_Identity(pu) != NULL)
        return FALSE;

    DSL_PU_SOURCE_IDENTITY_RECORD image_record;
    memset(&image_record, 0, sizeof(image_record));
    image_record.owner_pu_st = PU_Info_proc_sym(pu);
    image_record.canonical_definition_name =
        Save_Str(identity->canonical_definition_name);
    image_record.defining_module =
        Save_Str(DSL_Builder_Safe_String(identity->defining_module));
    image_record.defining_file = Save_Str(identity->defining_file);
    image_record.defining_line = identity->defining_line;
    image_record.flags = identity->flags;
    if (DSL_Call_Image_Add_PU_Identity(&image_record) ==
        DSL_PU_SOURCE_IDENTITY_INVALID_ID)
        return FALSE;

    dsl_builder_pu_source_identity *record =
        new dsl_builder_pu_source_identity;
    record->pu = pu;
    record->canonical_definition_name = identity->canonical_definition_name;
    record->defining_module = DSL_Builder_Safe_String
                                  (identity->defining_module);
    record->defining_file = identity->defining_file;
    record->defining_line = identity->defining_line;
    record->flags = identity->flags;
    DSL_builder_pu_source_identities.push_back(record);
    return TRUE;
}

BOOL
DSL_Builder_Get_PU_Source_Identity
        (DSL_BUILDER_PROGRAM_UNIT pu,
         DSL_BUILDER_PU_SOURCE_IDENTITY *identity)
{
    if (pu == NULL || identity == NULL)
        return FALSE;
    DSL_PU_SOURCE_IDENTITY_RECORD image_record;
    if (DSL_Call_Image_Find_PU_Identity
            (PU_Info_proc_sym(pu), &image_record)) {
        identity->canonical_definition_name =
            Index_To_Str(image_record.canonical_definition_name);
        identity->defining_module = Index_To_Str(image_record.defining_module);
        identity->defining_file = Index_To_Str(image_record.defining_file);
        identity->defining_line = image_record.defining_line;
        identity->flags = image_record.flags;
        return TRUE;
    }
    dsl_builder_pu_source_identity *record =
        DSL_Builder_Find_PU_Source_Identity(pu);
    if (record == NULL)
        return FALSE;

    identity->canonical_definition_name =
        record->canonical_definition_name.c_str();
    identity->defining_module = record->defining_module.c_str();
    identity->defining_file = record->defining_file.c_str();
    identity->defining_line = record->defining_line;
    identity->flags = record->flags;
    return TRUE;
}

static BOOL
DSL_Builder_Uses_Xpragma_Carrier (const DSL_OPCODE_INFO *info)
{
    if (info == NULL || info->name == NULL)
        return FALSE;

    return strcmp (info->name, DSL_OPCODE_COMMON_ADD) == 0 ||
           strcmp (info->name, DSL_OPCODE_COMMON_MUL) == 0 ||
           strcmp (info->name, DSL_OPCODE_COMMON_DIV) == 0 ||
           strcmp (info->name, DSL_OPCODE_COMMON_REM) == 0 ||
           strcmp (info->name, DSL_OPCODE_COMMON_MATMUL) == 0 ||
           strcmp (info->name, DSL_OPCODE_COMMON_TENSOR_CONST) == 0;
}

static void
DSL_Builder_Bind_Attribute_If_Present (TY_IDX ty,
                                       TY_TENSOR_SCHEMA_KEY key,
                                       const char *value)
{
    if (value != NULL)
        TY_tensor_bind_attribute (ty, key, value);
}

static const char *
DSL_Builder_Find_Payload_Value (const char *payload, const char *key,
                                char *buffer, size_t buffer_size)
{
    const char *match;
    const char *value;
    const char *end;
    size_t key_len;
    size_t value_len;

    if (payload == NULL || key == NULL || buffer == NULL || buffer_size == 0)
        return NULL;

    key_len = strlen (key);
    match = strstr (payload, key);
    while (match != NULL) {
        if ((match == payload || match[-1] == ';') &&
            match[key_len] == '=') {
            value = match + key_len + 1;
            end = strchr (value, ';');
            value_len = end == NULL ? strlen (value) :
                                      (size_t)(end - value);
            if (value_len >= buffer_size)
                value_len = buffer_size - 1;
            memcpy (buffer, value, value_len);
            buffer[value_len] = '\0';
            return buffer;
        }
        match = strstr (match + 1, key);
    }

    return NULL;
}

static const char *
DSL_Builder_Kid_Name (DSL_BUILDER_VALUE kid, UINT32 ordinal,
                      char *buffer, size_t buffer_size)
{
    DSL_OPCODE_ANNOTATION annotation;
    DSL_BUILDER_VALUE_RECORD *record;
    const char *name;

    if (buffer == NULL || buffer_size == 0)
        return "";

    record = DSL_Builder_Find_Value_Record(kid);
    if (record != NULL && ST_IDX_index(record->result_st) != 0) {
        snprintf(buffer, buffer_size, "%s",
                 ST_name(St_Table[record->result_st]));
        return buffer;
    }

    if (kid != NULL &&
        DSL_Builder_Get_Value_Annotation (kid, &annotation) &&
        annotation.payload != NULL) {
        name = DSL_Builder_Find_Payload_Value (annotation.payload, "name",
                                               buffer, buffer_size);
        if (name != NULL)
            return name;
    }

    snprintf (buffer, buffer_size, "kid%u", ordinal);
    return buffer;
}

static char *
DSL_Builder_Format_Operator_Payload
        (DSL_BUILDER_VALUE *kids,
         UINT32 kid_count,
         const DSL_BUILDER_OPERATOR_ATTRIBUTE *attrs,
         UINT32 attr_count)
{
    size_t payload_size = 1;
    BOOL first = TRUE;
    char *payload;
    size_t offset;

    for (UINT32 i = 0; i < kid_count; ++i) {
        char kid_name[256];
        const char *name = DSL_Builder_Kid_Name
                               (kids == NULL ? NULL : kids[i],
                                i, kid_name, sizeof(kid_name));
        payload_size += (i == 0 ? 0 : 1) + strlen("kid") + 10 + 1 +
                        strlen(name);
    }

    for (UINT32 i = 0; attrs != NULL && i < attr_count; ++i) {
        const char *name = DSL_Builder_Safe_String (attrs[i].name);
        const char *value = DSL_Builder_Safe_String (attrs[i].value);
        payload_size += (payload_size == 1 ? 0 : 1) + strlen(name) + 1 +
                        strlen(value);
    }

    payload = new char[payload_size];
    payload[0] = '\0';
    offset = 0;

    for (UINT32 i = 0; i < kid_count; ++i) {
        char kid_name[256];
        const char *name = DSL_Builder_Kid_Name
                               (kids == NULL ? NULL : kids[i],
                                i, kid_name, sizeof(kid_name));
        offset += snprintf (payload + offset, payload_size - offset,
                            "%skid%u=%s", first ? "" : ";", i, name);
        first = FALSE;
    }

    for (UINT32 i = 0; attrs != NULL && i < attr_count; ++i) {
        const char *name = DSL_Builder_Safe_String (attrs[i].name);
        const char *value = DSL_Builder_Safe_String (attrs[i].value);
        offset += snprintf (payload + offset, payload_size - offset,
                            "%s%s=%s", first ? "" : ";", name, value);
        first = FALSE;
    }

    return payload;
}

static char *
DSL_Builder_Format_Tensor_Constant_Payload
        (const char *name,
         const char *dtype,
         UINT32 rank,
         const char *logical_shape,
         const char *value_kind,
         const char *value)
{
    const char *safe_name = DSL_Builder_Safe_String(name);
    const char *safe_dtype = DSL_Builder_Safe_String(dtype);
    const char *safe_shape = DSL_Builder_Safe_String(logical_shape);
    const char *safe_value_kind = DSL_Builder_Safe_String(value_kind);
    const char *safe_value = DSL_Builder_Safe_String(value);
    size_t size = strlen("name=;dtype=;rank=;shape=;value_kind=;value=") +
                  strlen(safe_name) + strlen(safe_dtype) + 10 +
                  strlen(safe_shape) + strlen(safe_value_kind) +
                  strlen(safe_value) + 1;
    char *payload = new char[size];

    snprintf (payload, size,
              "name=%s;dtype=%s;rank=%u;shape=%s;value_kind=%s;value=%s",
              safe_name, safe_dtype, rank, safe_shape, safe_value_kind,
              safe_value);
    return payload;
}

static BOOL
DSL_Builder_Parse_Matrix_Shape
        (const char *shape,
         char *dim0,
         size_t dim0_size,
         char *dim1,
         size_t dim1_size)
{
    if (shape == NULL || dim0 == NULL || dim1 == NULL ||
        dim0_size == 0 || dim1_size == 0)
        return FALSE;

    const char *begin = shape;
    while (isspace((unsigned char)*begin))
        ++begin;
    if (*begin++ != '[')
        return FALSE;
    const char *comma = strchr(begin, ',');
    const char *close = strrchr(begin, ']');
    if (comma == NULL || close == NULL || comma >= close ||
        strchr(comma + 1, ',') != NULL)
        return FALSE;

    const char *dim0_end = comma;
    while (dim0_end > begin && isspace((unsigned char)dim0_end[-1]))
        --dim0_end;
    while (begin < dim0_end && isspace((unsigned char)*begin))
        ++begin;
    const char *dim1_begin = comma + 1;
    while (dim1_begin < close && isspace((unsigned char)*dim1_begin))
        ++dim1_begin;
    const char *dim1_end = close;
    while (dim1_end > dim1_begin && isspace((unsigned char)dim1_end[-1]))
        --dim1_end;

    size_t length0 = dim0_end - begin;
    size_t length1 = dim1_end - dim1_begin;
    if (length0 == 0 || length1 == 0 || length0 >= dim0_size ||
        length1 >= dim1_size)
        return FALSE;
    memcpy (dim0, begin, length0);
    dim0[length0] = '\0';
    memcpy (dim1, dim1_begin, length1);
    dim1[length1] = '\0';
    return TRUE;
}

static TY_IDX
DSL_Builder_Create_Matmul_V1_Result_Type
        (TY_IDX kid0_ty,
         TY_IDX kid1_ty,
         const char *name)
{
    char kid0_dim0[128];
    char kid0_dim1[128];
    char kid1_dim0[128];
    char kid1_dim1[128];
    char result_shape[260];
    DSL_BUILDER_TENSOR_DESCRIPTOR descriptor;

    const char *kid0_shape = TY_tensor_attribute
                                 (kid0_ty, TY_TENSOR_SCHEMA_SHAPE);
    const char *kid1_shape = TY_tensor_attribute
                                 (kid1_ty, TY_TENSOR_SCHEMA_SHAPE);
    if (!DSL_Builder_Parse_Matrix_Shape
             (kid0_shape, kid0_dim0, sizeof(kid0_dim0), kid0_dim1,
              sizeof(kid0_dim1)) ||
        !DSL_Builder_Parse_Matrix_Shape
             (kid1_shape, kid1_dim0, sizeof(kid1_dim0), kid1_dim1,
              sizeof(kid1_dim1)) ||
        strcmp(kid0_dim1, kid1_dim0) != 0)
        return TY_IDX_ZERO;

    snprintf (result_shape, sizeof(result_shape), "[%s,%s]",
              kid0_dim0, kid1_dim1);
    memset (&descriptor, 0, sizeof(descriptor));
    descriptor.type_core.kind =
        TY_tensor_attribute(kid0_ty, TY_TENSOR_SCHEMA_KIND);
    descriptor.type_core.dtype =
        TY_tensor_attribute(kid0_ty, TY_TENSOR_SCHEMA_DTYPE);
    descriptor.type_core.rank = 2;
    descriptor.type_core.logical_shape = result_shape;
    descriptor.traits.traits =
        TY_tensor_attribute(kid0_ty, TY_TENSOR_SCHEMA_TRAITS);
    descriptor.representation.layout =
        TY_tensor_attribute(kid0_ty, TY_TENSOR_SCHEMA_LAYOUT);
    descriptor.representation.sharding =
        TY_tensor_attribute(kid0_ty, TY_TENSOR_SCHEMA_SHARDING);
    descriptor.representation.placement =
        TY_tensor_attribute(kid0_ty, TY_TENSOR_SCHEMA_PLACEMENT);
    descriptor.representation.memory =
        TY_tensor_attribute(kid0_ty, TY_TENSOR_SCHEMA_MEMORY);
    descriptor.representation.quantization =
        TY_tensor_attribute(kid0_ty, TY_TENSOR_SCHEMA_QUANTIZATION);
    descriptor.representation.runtime_state =
        TY_tensor_attribute(kid0_ty, TY_TENSOR_SCHEMA_RUNTIME_STATE);
    descriptor.lineage.lineage = "common.matmul";

    TY_IDX result_ty = DSL_Builder_Create_Tensor_Type_Core
                           (name, TY_tensor_element_ty(kid0_ty),
                            &descriptor.type_core);
    if (!DSL_Builder_Attach_Tensor_Descriptor(result_ty, &descriptor))
        return TY_IDX_ZERO;
    return result_ty;
}

static const char *
DSL_Builder_Find_Operator_Attribute
        (const DSL_BUILDER_OPERATOR_ATTRIBUTE *attrs,
         UINT32 attr_count,
         const char *name)
{
    for (UINT32 i = 0; attrs != NULL && i < attr_count; ++i) {
        if (attrs[i].name != NULL && strcmp(attrs[i].name, name) == 0)
            return attrs[i].value;
    }
    return NULL;
}

static BOOL
DSL_Builder_Attributes_Match_Schema
        (const DSL_OPERATOR_INFO *info,
         const DSL_BUILDER_OPERATOR_ATTRIBUTE *attrs,
         UINT32 attr_count)
{
    if (info == NULL || (attr_count != 0 && attrs == NULL))
        return FALSE;

    const char *schema = DSL_Builder_Safe_String(info->attribute_schema);
    const char *cursor = schema;
    UINT32 required_count = 0;
    while (*cursor != '\0') {
        const char *end = strchr(cursor, ';');
        size_t length = end == NULL ? strlen(cursor) :
                                     (size_t)(end - cursor);
        if (length != 0) {
            ++required_count;
            UINT32 matches = 0;
            for (UINT32 i = 0; i < attr_count; ++i) {
                if (attrs[i].name != NULL &&
                    strlen(attrs[i].name) == length &&
                    strncmp(attrs[i].name, cursor, length) == 0)
                    ++matches;
            }
            if (matches != 1)
                return FALSE;
        }
        if (end == NULL)
            break;
        cursor = end + 1;
    }
    return required_count == attr_count;
}

static BOOL
DSL_Builder_Requires_Exact_Attribute_Schema
        (DSL_OPERATOR dsl_operator,
         UINT16 version)
{
    return dsl_operator == OPR_DSLRESHAPE ||
           dsl_operator == OPR_DSLTRANSPOSE ||
           dsl_operator == OPR_DSLTOKENEMBEDDING ||
           dsl_operator == OPR_DSLRMSNORM ||
           dsl_operator == OPR_DSLROTARYEMBEDDING ||
           dsl_operator == OPR_DSLATTENTION ||
           dsl_operator == OPR_DSLSWIGLU ||
           dsl_operator == OPR_DSLSCATTER ||
           (dsl_operator == OPR_DSLMATMUL && version == 2) ||
           (dsl_operator == OPR_DSLLINEAR && version == 3) ||
           (dsl_operator == OPR_DSLOUTPUTLOGITS && version == 3);
}

static BOOL
DSL_Builder_Parse_Signed_Attribute
        (const DSL_BUILDER_OPERATOR_ATTRIBUTE *attrs,
         UINT32 attr_count,
         const char *name,
         INT32 *value)
{
    const char *text = DSL_Builder_Find_Operator_Attribute
                           (attrs, attr_count, name);
    if (text == NULL || text[0] == '\0')
        return FALSE;
    char *end = NULL;
    long parsed = strtol(text, &end, 10);
    if (end == text || *end != '\0' || parsed < INT32_MIN ||
        parsed > INT32_MAX)
        return FALSE;
    *value = (INT32)parsed;
    return TRUE;
}

static BOOL
DSL_Builder_Parse_Unsigned_Attribute
        (const DSL_BUILDER_OPERATOR_ATTRIBUTE *attrs,
         UINT32 attr_count,
         const char *name,
         UINT64 *value)
{
    const char *text = DSL_Builder_Find_Operator_Attribute
                           (attrs, attr_count, name);
    if (text == NULL || !isdigit((unsigned char)text[0]))
        return FALSE;
    UINT64 parsed = 0;
    const char *cursor = text;
    while (isdigit((unsigned char)*cursor)) {
        UINT64 digit = (UINT64)(*cursor - '0');
        if (parsed > (~(UINT64)0 - digit) / 10)
            return FALSE;
        parsed = parsed * 10 + digit;
        ++cursor;
    }
    if (*cursor != '\0')
        return FALSE;
    if (value != NULL)
        *value = parsed;
    return TRUE;
}

static BOOL
DSL_Builder_Parse_Static_Shape
        (const char *shape,
         std::vector<UINT64> *dimensions)
{
    if (shape == NULL || dimensions == NULL)
        return FALSE;
    dimensions->clear();
    const char *cursor = shape;
    while (isspace((unsigned char)*cursor))
        ++cursor;
    if (*cursor++ != '[')
        return FALSE;
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
        if (dimension == 0)
            return FALSE;
        dimensions->push_back(dimension);
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
    return *cursor == '\0';
}

static BOOL
DSL_Builder_Parse_Unsigned_List
        (const char *text,
         BOOL allow_zero,
         std::vector<UINT64> *values)
{
    if (text == NULL || values == NULL || text[0] == '\0')
        return FALSE;

    values->clear();
    const char *cursor = text;
    while (*cursor != '\0') {
        if (!isdigit((unsigned char)*cursor))
            return FALSE;
        UINT64 value = 0;
        while (isdigit((unsigned char)*cursor)) {
            UINT64 digit = (UINT64)(*cursor - '0');
            if (value > (~(UINT64)0 - digit) / 10)
                return FALSE;
            value = value * 10 + digit;
            ++cursor;
        }
        if (!allow_zero && value == 0)
            return FALSE;
        values->push_back(value);
        if (*cursor == '\0')
            break;
        if (*cursor++ != ',' || *cursor == '\0')
            return FALSE;
    }
    return !values->empty();
}

static BOOL
DSL_Builder_Tensor_Element_Type_Compatible
        (TY_IDX left,
         TY_IDX right)
{
    const char *left_dtype =
        TY_tensor_attribute(left, TY_TENSOR_SCHEMA_DTYPE);
    const char *right_dtype =
        TY_tensor_attribute(right, TY_TENSOR_SCHEMA_DTYPE);

    return TY_is_tensor_extension(left) && TY_is_tensor_extension(right) &&
           TY_tensor_element_ty(left) == TY_tensor_element_ty(right) &&
           left_dtype != NULL && right_dtype != NULL &&
           strcmp(left_dtype, right_dtype) == 0;
}

static TY_IDX
DSL_Builder_Create_Result_Type
        (TY_IDX input_ty,
         const std::vector<UINT64> &dimensions,
         const char *name,
         const char *lineage,
         BOOL preserve_representation)
{
    std::string shape = "[";
    for (UINT32 i = 0; i < dimensions.size(); ++i) {
        char dimension[32];
        snprintf(dimension, sizeof(dimension), "%llu",
                 (unsigned long long)dimensions[i]);
        if (i != 0)
            shape += ",";
        shape += dimension;
    }
    shape += "]";

    DSL_BUILDER_TENSOR_DESCRIPTOR descriptor;
    memset(&descriptor, 0, sizeof(descriptor));
    descriptor.type_core.kind =
        TY_tensor_attribute(input_ty, TY_TENSOR_SCHEMA_KIND);
    descriptor.type_core.dtype =
        TY_tensor_attribute(input_ty, TY_TENSOR_SCHEMA_DTYPE);
    descriptor.type_core.rank = dimensions.size();
    descriptor.type_core.logical_shape = shape.c_str();
    descriptor.traits.traits =
        TY_tensor_attribute(input_ty, TY_TENSOR_SCHEMA_TRAITS);
    if (preserve_representation) {
        descriptor.representation.layout =
            TY_tensor_attribute(input_ty, TY_TENSOR_SCHEMA_LAYOUT);
        descriptor.representation.sharding =
            TY_tensor_attribute(input_ty, TY_TENSOR_SCHEMA_SHARDING);
        descriptor.representation.placement =
            TY_tensor_attribute(input_ty, TY_TENSOR_SCHEMA_PLACEMENT);
        descriptor.representation.memory =
            TY_tensor_attribute(input_ty, TY_TENSOR_SCHEMA_MEMORY);
        descriptor.representation.quantization =
            TY_tensor_attribute(input_ty, TY_TENSOR_SCHEMA_QUANTIZATION);
        descriptor.representation.runtime_state =
            TY_tensor_attribute(input_ty, TY_TENSOR_SCHEMA_RUNTIME_STATE);
    } else {
        descriptor.representation.layout =
            TY_tensor_attribute(input_ty, TY_TENSOR_SCHEMA_LAYOUT);
        descriptor.representation.quantization =
            TY_tensor_attribute(input_ty, TY_TENSOR_SCHEMA_QUANTIZATION);
    }
    descriptor.lineage.lineage = lineage;

    TY_IDX result_ty = DSL_Builder_Create_Tensor_Type_Core
                           (name, TY_tensor_element_ty(input_ty),
                            &descriptor.type_core);
    if (!DSL_Builder_Attach_Tensor_Descriptor(result_ty, &descriptor))
        return TY_IDX_ZERO;
    return result_ty;
}

static TY_IDX
DSL_Builder_Create_Derived_Result_Type
        (TY_IDX input_ty,
         const std::vector<UINT64> &dimensions,
         const char *name,
         const char *lineage)
{
    return DSL_Builder_Create_Result_Type
               (input_ty, dimensions, name, lineage, TRUE);
}

static BOOL
DSL_Builder_Parse_Pair_Attribute
        (const DSL_BUILDER_OPERATOR_ATTRIBUTE *attrs,
         UINT32 attr_count,
         const char *name,
         BOOL allow_zero,
         UINT64 values[2])
{
    const char *text = DSL_Builder_Find_Operator_Attribute
                           (attrs, attr_count, name);
    if (text == NULL || !isdigit((unsigned char)text[0]))
        return FALSE;
    char *end = NULL;
    unsigned long long first = strtoull(text, &end, 10);
    if (end == text || *end != ',')
        return FALSE;
    const char *second_text = end + 1;
    if (!isdigit((unsigned char)second_text[0]))
        return FALSE;
    unsigned long long second = strtoull(second_text, &end, 10);
    if (end == second_text || *end != '\0' ||
        (!allow_zero && (first == 0 || second == 0)))
        return FALSE;
    values[0] = first;
    values[1] = second;
    return TRUE;
}

static BOOL
DSL_Builder_Attribute_Equals
        (const DSL_BUILDER_OPERATOR_ATTRIBUTE *attrs,
         UINT32 attr_count,
         const char *name,
         const char *expected)
{
    const char *value = DSL_Builder_Find_Operator_Attribute
                            (attrs, attr_count, name);
    return value != NULL && strcmp(value, expected) == 0;
}

static TY_IDX
DSL_Builder_Create_Reshape_Result_Type
        (TY_IDX input_ty,
         const DSL_BUILDER_OPERATOR_ATTRIBUTE *attrs,
         UINT32 attr_count,
         const char *name)
{
    std::vector<UINT64> input;
    std::vector<UINT64> result;
    const char *target_shape = DSL_Builder_Find_Operator_Attribute
                                   (attrs, attr_count, "attr.target_shape");
    if (!DSL_Builder_Parse_Static_Shape
             (TY_tensor_attribute(input_ty, TY_TENSOR_SCHEMA_SHAPE), &input) ||
        !DSL_Builder_Parse_Unsigned_List
             (target_shape, FALSE, &result))
        return TY_IDX_ZERO;

    UINT64 input_elements = 1;
    UINT64 result_elements = 1;
    for (UINT32 i = 0; i < input.size(); ++i) {
        if (input_elements > ~(UINT64)0 / input[i])
            return TY_IDX_ZERO;
        input_elements *= input[i];
    }
    for (UINT32 i = 0; i < result.size(); ++i) {
        if (result_elements > ~(UINT64)0 / result[i])
            return TY_IDX_ZERO;
        result_elements *= result[i];
    }
    if (input_elements != result_elements)
        return TY_IDX_ZERO;

    return DSL_Builder_Create_Derived_Result_Type
               (input_ty, result, name, "common.reshape");
}

static TY_IDX
DSL_Builder_Create_Transpose_Result_Type
        (TY_IDX input_ty,
         const DSL_BUILDER_OPERATOR_ATTRIBUTE *attrs,
         UINT32 attr_count,
         const char *name)
{
    std::vector<UINT64> input;
    std::vector<UINT64> permutation;
    const char *permutation_text = DSL_Builder_Find_Operator_Attribute
                                       (attrs, attr_count, "attr.permutation");
    if (!DSL_Builder_Parse_Static_Shape
             (TY_tensor_attribute(input_ty, TY_TENSOR_SCHEMA_SHAPE), &input) ||
        !DSL_Builder_Parse_Unsigned_List
             (permutation_text, TRUE, &permutation) ||
        permutation.size() != input.size())
        return TY_IDX_ZERO;

    std::vector<BOOL> seen(input.size(), FALSE);
    std::vector<UINT64> result(input.size());
    for (UINT32 i = 0; i < permutation.size(); ++i) {
        if (permutation[i] >= input.size() || seen[permutation[i]])
            return TY_IDX_ZERO;
        seen[permutation[i]] = TRUE;
        result[i] = input[permutation[i]];
    }
    return DSL_Builder_Create_Derived_Result_Type
               (input_ty, result, name, "common.transpose");
}

static BOOL
DSL_Builder_Dimensions_Equal
        (const std::vector<UINT64> &left,
         const std::vector<UINT64> &right)
{
    if (left.size() != right.size())
        return FALSE;
    for (UINT32 i = 0; i < left.size(); ++i) {
        if (left[i] != right[i])
            return FALSE;
    }
    return TRUE;
}

static BOOL
DSL_Builder_Positive_Float_Attribute
        (const DSL_BUILDER_OPERATOR_ATTRIBUTE *attrs,
         UINT32 attr_count,
         const char *name)
{
    const char *text = DSL_Builder_Find_Operator_Attribute
                           (attrs, attr_count, name);
    if (text == NULL || text[0] == '\0')
        return FALSE;
    char *end = NULL;
    double value = strtod(text, &end);
    return end != text && *end == '\0' && value > 0.0 && value <= DBL_MAX;
}

static TY_IDX
DSL_Builder_Create_Transformer_Result_Type
        (DSL_OPERATOR dsl_operator,
         TY_IDX kid0_ty,
         TY_IDX kid1_ty,
         TY_IDX kid2_ty,
         const DSL_BUILDER_OPERATOR_ATTRIBUTE *attrs,
         UINT32 attr_count,
         const char *name)
{
    std::vector<UINT64> kid0;
    std::vector<UINT64> kid1;
    std::vector<UINT64> kid2;
    const char *lineage = NULL;

    if (!DSL_Builder_Parse_Static_Shape
             (TY_tensor_attribute(kid0_ty, TY_TENSOR_SCHEMA_SHAPE), &kid0) ||
        !DSL_Builder_Parse_Static_Shape
             (TY_tensor_attribute(kid1_ty, TY_TENSOR_SCHEMA_SHAPE), &kid1))
        return TY_IDX_ZERO;

    if (dsl_operator == OPR_DSLTOKENEMBEDDING) {
        const char *token_dtype =
            TY_tensor_attribute(kid0_ty, TY_TENSOR_SCHEMA_DTYPE);
        const char *weight_dtype =
            TY_tensor_attribute(kid1_ty, TY_TENSOR_SCHEMA_DTYPE);
        if (kid0.size() != 2 || kid1.size() != 2 ||
            token_dtype == NULL || strcmp(token_dtype, "int64") != 0 ||
            weight_dtype == NULL || strcmp(weight_dtype, "float32") != 0 ||
            !DSL_Builder_Attribute_Equals
                 (attrs, attr_count, "attr.padding_idx", "none") ||
            !DSL_Builder_Attribute_Equals
                 (attrs, attr_count, "attr.bounds_policy", "runtime_check"))
            return TY_IDX_ZERO;
        std::vector<UINT64> result;
        result.push_back(kid0[0]);
        result.push_back(kid0[1]);
        result.push_back(kid1[1]);
        return DSL_Builder_Create_Result_Type
                   (kid1_ty, result, name, "transformer.token_embedding",
                    FALSE);
    }

    const char *activation_dtype =
        TY_tensor_attribute(kid0_ty, TY_TENSOR_SCHEMA_DTYPE);
    if (activation_dtype == NULL || strcmp(activation_dtype, "float32") != 0)
        return TY_IDX_ZERO;

    if (dsl_operator == OPR_DSLRMSNORM) {
        if (kid0.size() < 2 || kid1.size() != 1 ||
            kid1[0] != kid0[kid0.size() - 1] ||
            !DSL_Builder_Tensor_Element_Type_Compatible(kid0_ty, kid1_ty) ||
            !DSL_Builder_Attribute_Equals
                 (attrs, attr_count, "attr.axis", "-1") ||
            !DSL_Builder_Positive_Float_Attribute
                 (attrs, attr_count, "attr.epsilon") ||
            !DSL_Builder_Attribute_Equals
                 (attrs, attr_count, "attr.accum_dtype", "float32"))
            return TY_IDX_ZERO;
        lineage = "transformer.rms_norm";
    } else if (dsl_operator == OPR_DSLROTARYEMBEDDING) {
        if (!DSL_Builder_Parse_Static_Shape
                 (TY_tensor_attribute(kid2_ty, TY_TENSOR_SCHEMA_SHAPE),
                  &kid2) ||
            kid0.size() != 4 || kid1.size() != 4 ||
            !DSL_Builder_Dimensions_Equal(kid1, kid2) ||
            kid1[0] != 1 || kid1[1] != 1 || kid1[2] != kid0[2] ||
            kid1[3] != kid0[3] || kid0[3] % 2 != 0 ||
            !DSL_Builder_Tensor_Element_Type_Compatible(kid0_ty, kid1_ty) ||
            !DSL_Builder_Tensor_Element_Type_Compatible(kid0_ty, kid2_ty) ||
            !DSL_Builder_Attribute_Equals
                 (attrs, attr_count, "attr.head_layout", "BHSD") ||
            !DSL_Builder_Attribute_Equals
                 (attrs, attr_count, "attr.sequence_axis", "2") ||
            !DSL_Builder_Attribute_Equals
                 (attrs, attr_count, "attr.feature_axis", "3") ||
            !DSL_Builder_Attribute_Equals
                 (attrs, attr_count, "attr.pairing", "half_split") ||
            !DSL_Builder_Attribute_Equals
                 (attrs, attr_count, "attr.position_mode",
                  "zero_based_static") ||
            !DSL_Builder_Attribute_Equals
                 (attrs, attr_count, "attr.position_offset", "0"))
            return TY_IDX_ZERO;
        lineage = "transformer.rotary_embedding";
    } else if (dsl_operator == OPR_DSLATTENTION) {
        UINT64 query_heads;
        UINT64 kv_heads;
        UINT64 head_dim;
        if (!DSL_Builder_Parse_Static_Shape
                 (TY_tensor_attribute(kid2_ty, TY_TENSOR_SCHEMA_SHAPE),
                  &kid2) ||
            kid0.size() != 4 ||
            !DSL_Builder_Dimensions_Equal(kid0, kid1) ||
            !DSL_Builder_Dimensions_Equal(kid0, kid2) ||
            !DSL_Builder_Tensor_Element_Type_Compatible(kid0_ty, kid1_ty) ||
            !DSL_Builder_Tensor_Element_Type_Compatible(kid0_ty, kid2_ty) ||
            !DSL_Builder_Parse_Unsigned_Attribute
                 (attrs, attr_count, "attr.query_heads", &query_heads) ||
            !DSL_Builder_Parse_Unsigned_Attribute
                 (attrs, attr_count, "attr.kv_heads", &kv_heads) ||
            !DSL_Builder_Parse_Unsigned_Attribute
                 (attrs, attr_count, "attr.head_dim", &head_dim) ||
            query_heads <= 0 || kv_heads != query_heads || head_dim <= 0 ||
            query_heads != kid0[1] || head_dim != kid0[3] ||
            !DSL_Builder_Attribute_Equals
                 (attrs, attr_count, "attr.execution_mode", "full_sequence") ||
            !DSL_Builder_Attribute_Equals
                 (attrs, attr_count, "attr.mask_mode", "causal") ||
            !DSL_Builder_Attribute_Equals
                 (attrs, attr_count, "attr.head_layout", "BHSD") ||
            !DSL_Builder_Attribute_Equals
                 (attrs, attr_count, "attr.scale_mode",
                  "inverse_sqrt_head_dim") ||
            !DSL_Builder_Attribute_Equals
                 (attrs, attr_count, "attr.softmax_axis", "-1") ||
            !DSL_Builder_Attribute_Equals
                 (attrs, attr_count, "attr.softmax_accum_dtype", "float32") ||
            !DSL_Builder_Attribute_Equals
                 (attrs, attr_count, "attr.cache_mode", "none"))
            return TY_IDX_ZERO;
        lineage = "transformer.attention";
    } else if (dsl_operator == OPR_DSLSWIGLU) {
        if (!DSL_Builder_Dimensions_Equal(kid0, kid1) ||
            kid0.size() != 3 ||
            !DSL_Builder_Tensor_Element_Type_Compatible(kid0_ty, kid1_ty) ||
            !DSL_Builder_Attribute_Equals
                 (attrs, attr_count, "attr.activation", "silu"))
            return TY_IDX_ZERO;
        lineage = "transformer.swiglu";
    } else {
        return TY_IDX_ZERO;
    }

    return DSL_Builder_Create_Derived_Result_Type
               (kid0_ty, kid0, name, lineage);
}

static TY_IDX
DSL_Builder_Create_Matmul_Result_Type
        (UINT16 version,
         TY_IDX kid0_ty,
         TY_IDX kid1_ty,
         const DSL_BUILDER_OPERATOR_ATTRIBUTE *attrs,
         UINT32 attr_count,
         const char *name)
{
    if (version == 1)
        return DSL_Builder_Create_Matmul_V1_Result_Type
                   (kid0_ty, kid1_ty, name);
    if (version != 2 ||
        !DSL_Builder_Attribute_Equals
             (attrs, attr_count, "attr.batch_rule", "exact") ||
        !DSL_Builder_Attribute_Equals
             (attrs, attr_count, "attr.accum_dtype", "float32") ||
        !DSL_Builder_Tensor_Element_Type_Compatible(kid0_ty, kid1_ty))
        return TY_IDX_ZERO;

    BOOL transpose_kid0;
    BOOL transpose_kid1;
    if (DSL_Builder_Attribute_Equals
            (attrs, attr_count, "attr.transpose_kid0", "true"))
        transpose_kid0 = TRUE;
    else if (DSL_Builder_Attribute_Equals
                 (attrs, attr_count, "attr.transpose_kid0", "false"))
        transpose_kid0 = FALSE;
    else
        return TY_IDX_ZERO;
    if (DSL_Builder_Attribute_Equals
            (attrs, attr_count, "attr.transpose_kid1", "true"))
        transpose_kid1 = TRUE;
    else if (DSL_Builder_Attribute_Equals
                 (attrs, attr_count, "attr.transpose_kid1", "false"))
        transpose_kid1 = FALSE;
    else
        return TY_IDX_ZERO;

    std::vector<UINT64> kid0;
    std::vector<UINT64> kid1;
    if (!DSL_Builder_Parse_Static_Shape
             (TY_tensor_attribute(kid0_ty, TY_TENSOR_SCHEMA_SHAPE), &kid0) ||
        !DSL_Builder_Parse_Static_Shape
             (TY_tensor_attribute(kid1_ty, TY_TENSOR_SCHEMA_SHAPE), &kid1) ||
        kid0.size() < 2 || kid0.size() != kid1.size())
        return TY_IDX_ZERO;
    for (UINT32 i = 0; i + 2 < kid0.size(); ++i) {
        if (kid0[i] != kid1[i])
            return TY_IDX_ZERO;
    }

    UINT32 rank = kid0.size();
    UINT64 left_m = kid0[rank - (transpose_kid0 ? 1 : 2)];
    UINT64 left_k = kid0[rank - (transpose_kid0 ? 2 : 1)];
    UINT64 right_k = kid1[rank - (transpose_kid1 ? 1 : 2)];
    UINT64 right_n = kid1[rank - (transpose_kid1 ? 2 : 1)];
    if (left_k != right_k)
        return TY_IDX_ZERO;

    std::vector<UINT64> result = kid0;
    result[rank - 2] = left_m;
    result[rank - 1] = right_n;
    return DSL_Builder_Create_Derived_Result_Type
               (kid0_ty, result, name, "common.matmul");
}

static TY_IDX
DSL_Builder_Create_Linear_Result_Type
        (UINT16 version,
         TY_IDX input_ty,
         TY_IDX weight_ty,
         TY_IDX bias_ty,
         const DSL_BUILDER_OPERATOR_ATTRIBUTE *attrs,
         UINT32 attr_count,
         const char *name)
{
    std::vector<UINT64> input;
    std::vector<UINT64> weight;
    std::vector<UINT64> bias;
    const char *required_bias = version == 2 ? "true" : "false";
    if ((version != 2 && version != 3) ||
        !DSL_Builder_Attribute_Equals
             (attrs, attr_count, "attr.has_bias", required_bias) ||
        !DSL_Builder_Attribute_Equals
             (attrs, attr_count, "attr.transpose_input", "false") ||
        !DSL_Builder_Attribute_Equals
             (attrs, attr_count, "attr.transpose_weight", "true") ||
        !DSL_Builder_Attribute_Equals
             (attrs, attr_count, "attr.weight_layout", "OI") ||
        !DSL_Builder_Parse_Static_Shape
             (TY_tensor_attribute(input_ty, TY_TENSOR_SCHEMA_SHAPE), &input) ||
        !DSL_Builder_Parse_Static_Shape
             (TY_tensor_attribute(weight_ty, TY_TENSOR_SCHEMA_SHAPE),
              &weight) ||
        input.size() < 2 || weight.size() != 2 ||
        input[input.size() - 1] != weight[1] ||
        !DSL_Builder_Tensor_Element_Type_Compatible(input_ty, weight_ty))
        return TY_IDX_ZERO;
    if (version == 2 &&
        (!DSL_Builder_Parse_Static_Shape
             (TY_tensor_attribute(bias_ty, TY_TENSOR_SCHEMA_SHAPE), &bias) ||
         bias.size() != 1 || bias[0] != weight[0] ||
         !DSL_Builder_Tensor_Element_Type_Compatible(input_ty, bias_ty)))
        return TY_IDX_ZERO;
    input[input.size() - 1] = weight[0];
    return DSL_Builder_Create_Derived_Result_Type
               (input_ty, input, name, "common.linear");
}

static TY_IDX
DSL_Builder_Create_Conv2D_Result_Type
        (TY_IDX input_ty,
         TY_IDX weight_ty,
         TY_IDX bias_ty,
         const DSL_BUILDER_OPERATOR_ATTRIBUTE *attrs,
         UINT32 attr_count,
         const char *name)
{
    std::vector<UINT64> input;
    std::vector<UINT64> weight;
    std::vector<UINT64> bias;
    UINT64 kernel[2];
    UINT64 stride[2];
    UINT64 padding[2];
    UINT64 dilation[2];
    INT32 groups;
    if (!DSL_Builder_Attribute_Equals
             (attrs, attr_count, "attr.input_layout", "NCHW") ||
        !DSL_Builder_Attribute_Equals
             (attrs, attr_count, "attr.weight_layout", "OIHW") ||
        !DSL_Builder_Attribute_Equals
             (attrs, attr_count, "attr.output_layout", "NCHW") ||
        !DSL_Builder_Parse_Pair_Attribute
             (attrs, attr_count, "attr.kernel_shape", FALSE, kernel) ||
        !DSL_Builder_Parse_Pair_Attribute
             (attrs, attr_count, "attr.stride", FALSE, stride) ||
        !DSL_Builder_Parse_Pair_Attribute
             (attrs, attr_count, "attr.padding", TRUE, padding) ||
        !DSL_Builder_Parse_Pair_Attribute
             (attrs, attr_count, "attr.dilation", FALSE, dilation) ||
        !DSL_Builder_Parse_Signed_Attribute
             (attrs, attr_count, "attr.groups", &groups) || groups <= 0 ||
        !DSL_Builder_Parse_Static_Shape
             (TY_tensor_attribute(input_ty, TY_TENSOR_SCHEMA_SHAPE), &input) ||
        !DSL_Builder_Parse_Static_Shape
             (TY_tensor_attribute(weight_ty, TY_TENSOR_SCHEMA_SHAPE),
              &weight) ||
        !DSL_Builder_Parse_Static_Shape
             (TY_tensor_attribute(bias_ty, TY_TENSOR_SCHEMA_SHAPE), &bias) ||
        input.size() != 4 || weight.size() != 4 || bias.size() != 1 ||
        kernel[0] != weight[2] || kernel[1] != weight[3] ||
        input[1] % groups != 0 || weight[0] % groups != 0 ||
        weight[1] * groups != input[1] || bias[0] != weight[0])
        return TY_IDX_ZERO;

    std::vector<UINT64> result(4);
    result[0] = input[0];
    result[1] = weight[0];
    for (UINT32 i = 0; i < 2; ++i) {
        if (kernel[i] - 1 > ~(UINT64)0 / dilation[i] ||
            padding[i] > (~(UINT64)0 - input[i + 2]) / 2)
            return TY_IDX_ZERO;
        UINT64 effective_kernel = dilation[i] * (kernel[i] - 1) + 1;
        UINT64 padded = input[i + 2] + 2 * padding[i];
        if (padded < effective_kernel)
            return TY_IDX_ZERO;
        result[i + 2] = (padded - effective_kernel) / stride[i] + 1;
    }
    return DSL_Builder_Create_Derived_Result_Type
               (input_ty, result, name, "cnn.conv2d");
}

static TY_IDX
DSL_Builder_Create_Max_Pool_Result_Type
        (TY_IDX input_ty,
         const DSL_BUILDER_OPERATOR_ATTRIBUTE *attrs,
         UINT32 attr_count,
         const char *name)
{
    std::vector<UINT64> input;
    UINT64 kernel[2];
    UINT64 stride[2];
    UINT64 padding[2];
    UINT64 dilation[2];
    const char *ceil_mode = DSL_Builder_Find_Operator_Attribute
                                (attrs, attr_count, "attr.ceil_mode");
    if (!DSL_Builder_Parse_Pair_Attribute
             (attrs, attr_count, "attr.kernel_shape", FALSE, kernel) ||
        !DSL_Builder_Parse_Pair_Attribute
             (attrs, attr_count, "attr.stride", FALSE, stride) ||
        !DSL_Builder_Parse_Pair_Attribute
             (attrs, attr_count, "attr.padding", TRUE, padding) ||
        !DSL_Builder_Parse_Pair_Attribute
             (attrs, attr_count, "attr.dilation", FALSE, dilation) ||
        (ceil_mode == NULL ||
         (strcmp(ceil_mode, "false") != 0 && strcmp(ceil_mode, "true") != 0)) ||
        !DSL_Builder_Parse_Static_Shape
             (TY_tensor_attribute(input_ty, TY_TENSOR_SCHEMA_SHAPE), &input) ||
        input.size() != 4)
        return TY_IDX_ZERO;

    std::vector<UINT64> result = input;
    BOOL use_ceil = strcmp(ceil_mode, "true") == 0;
    for (UINT32 i = 0; i < 2; ++i) {
        if (kernel[i] - 1 > ~(UINT64)0 / dilation[i] ||
            padding[i] > (~(UINT64)0 - input[i + 2]) / 2)
            return TY_IDX_ZERO;
        UINT64 effective_kernel = dilation[i] * (kernel[i] - 1) + 1;
        UINT64 padded = input[i + 2] + 2 * padding[i];
        if (padded < effective_kernel)
            return TY_IDX_ZERO;
        UINT64 numerator = padded - effective_kernel;
        result[i + 2] = numerator / stride[i] + 1;
        if (use_ceil && numerator % stride[i] != 0)
            ++result[i + 2];
    }
    return DSL_Builder_Create_Derived_Result_Type
               (input_ty, result, name, "cnn.max_pool2d");
}

static TY_IDX
DSL_Builder_Create_Global_Avg_Pool_Result_Type
        (TY_IDX input_ty,
         const DSL_BUILDER_OPERATOR_ATTRIBUTE *attrs,
         UINT32 attr_count,
         const char *name)
{
    std::vector<UINT64> input;
    UINT64 output_size[2];
    if (!DSL_Builder_Attribute_Equals
             (attrs, attr_count, "attr.reduction_axes", "spatial") ||
        !DSL_Builder_Parse_Pair_Attribute
             (attrs, attr_count, "attr.output_size", FALSE, output_size) ||
        output_size[0] != 1 || output_size[1] != 1 ||
        !DSL_Builder_Parse_Static_Shape
             (TY_tensor_attribute(input_ty, TY_TENSOR_SCHEMA_SHAPE), &input) ||
        input.size() != 4)
        return TY_IDX_ZERO;
    input[2] = output_size[0];
    input[3] = output_size[1];
    return DSL_Builder_Create_Derived_Result_Type
               (input_ty, input, name, "cnn.global_avg_pool2d");
}

static TY_IDX
DSL_Builder_Create_Flatten_Result_Type
        (TY_IDX input_ty,
         const DSL_BUILDER_OPERATOR_ATTRIBUTE *attrs,
         UINT32 attr_count,
         const char *name)
{
    INT32 rank = TY_tensor_rank(input_ty);
    INT32 start_dim;
    INT32 end_dim;
    if (rank <= 0 ||
        !DSL_Builder_Parse_Signed_Attribute
             (attrs, attr_count, "attr.start_dim", &start_dim) ||
        !DSL_Builder_Parse_Signed_Attribute
             (attrs, attr_count, "attr.end_dim", &end_dim))
        return TY_IDX_ZERO;
    if (start_dim < 0)
        start_dim += rank;
    if (end_dim < 0)
        end_dim += rank;
    if (start_dim < 0 || end_dim < start_dim || end_dim >= rank)
        return TY_IDX_ZERO;

    std::vector<UINT64> dimensions;
    if (!DSL_Builder_Parse_Static_Shape
             (TY_tensor_attribute(input_ty, TY_TENSOR_SCHEMA_SHAPE),
              &dimensions) || (INT32)dimensions.size() != rank)
        return TY_IDX_ZERO;

    UINT64 flattened = 1;
    for (INT32 i = start_dim; i <= end_dim; ++i) {
        if (flattened > ~(UINT64)0 / dimensions[i])
            return TY_IDX_ZERO;
        flattened *= dimensions[i];
    }
    std::string shape = "[";
    for (INT32 i = 0; i < start_dim; ++i) {
        char dimension[32];
        snprintf(dimension, sizeof(dimension), "%llu",
                 (unsigned long long)dimensions[i]);
        if (shape.size() != 1)
            shape += ",";
        shape += dimension;
    }
    char flattened_text[32];
    snprintf(flattened_text, sizeof(flattened_text), "%llu",
             (unsigned long long)flattened);
    if (shape.size() != 1)
        shape += ",";
    shape += flattened_text;
    for (INT32 i = end_dim + 1; i < rank; ++i) {
        char dimension[32];
        snprintf(dimension, sizeof(dimension), "%llu",
                 (unsigned long long)dimensions[i]);
        shape += ",";
        shape += dimension;
    }
    shape += "]";

    std::vector<UINT64> result_dimensions;
    if (!DSL_Builder_Parse_Static_Shape(shape.c_str(), &result_dimensions))
        return TY_IDX_ZERO;
    return DSL_Builder_Create_Derived_Result_Type
               (input_ty, result_dimensions, name, "common.flatten");
}

static DSL_IR_OPCODE_DESCRIPTOR_ID
DSL_Builder_Get_Image_Opcode_Descriptor
        (DSL_OPERATOR dsl_operator,
         UINT16 version)
{
    DSL_OPERATOR_INFO info;
    DSL_IR_OPCODE_DESCRIPTOR_RECORD record;
    DSL_IR_OPCODE_DESCRIPTOR_ID id;

    if (!DSL_Operator_Get_Info_Version(dsl_operator, version, &info))
        return DSL_IR_OPCODE_DESCRIPTOR_INVALID_ID;

    id = DSL_IR_Image_Find_Opcode_Descriptor(dsl_operator, info.version);
    if (id != DSL_IR_OPCODE_DESCRIPTOR_INVALID_ID)
        return id;

    DSL_IR_Opcode_Descriptor_Record_Init(&record);
    record.logical_operator = dsl_operator;
    record.version = info.version;
    record.operand_count = info.nkids;
    record.category = info.category;
    record.level = info.level;
    record.shape_rule = info.shape_rule;
    record.effect_model = info.effect_model;
    record.lowering_model = info.lowering_model;
    record.flags = info.flags;
    record.logical_name = Save_Str(info.logical_name);
    record.stable_name = Save_Str(info.stable_name);
    record.attribute_schema = Save_Str(DSL_Builder_Safe_String
                                            (info.attribute_schema));
    record.diagnostic_prefix = Save_Str(DSL_Builder_Safe_String
                                             (info.diagnostic_prefix));
    return DSL_IR_Image_Add_Opcode_Descriptor(&record);
}

static BOOL
DSL_Builder_Add_Image_Node
        (DSL_OPERATOR dsl_operator,
         UINT16 version,
         const char *payload,
         DSL_BUILDER_VALUE_RECORD **operand_records,
         UINT32 operand_count,
         const DSL_BUILDER_OPERATOR_ATTRIBUTE *attrs,
         UINT32 attr_count,
         ST_IDX result_st,
         TY_IDX result_ty,
         UINT32 value_kind,
         DSL_IR_VALUE_ID *result_value_id)
{
    DSL_IR_NODE_RECORD node_record;
    DSL_IR_VALUE_RECORD value_record;
    DSL_IR_OPCODE_DESCRIPTOR_ID descriptor_id;
    DSL_IR_NODE_ID node_id;
    DSL_IR_VALUE_REFERENCE_ID first_operand_id =
        DSL_IR_VALUE_REFERENCE_INVALID_ID;
    DSL_IR_ATTRIBUTE_ID first_attribute_id = DSL_IR_ATTRIBUTE_INVALID_ID;

    descriptor_id = DSL_Builder_Get_Image_Opcode_Descriptor
                        (dsl_operator, version);
    if (descriptor_id == DSL_IR_OPCODE_DESCRIPTOR_INVALID_ID)
        return FALSE;

    DSL_IR_Node_Record_Init(&node_record);
    node_record.opcode_descriptor_id = descriptor_id;
    node_record.payload = Save_Str(DSL_Builder_Safe_String(payload));
    node_id = DSL_IR_Image_Add_Node(&node_record);
    if (node_id == DSL_IR_NODE_INVALID_ID)
        return FALSE;

    for (UINT32 i = 0; i < operand_count; ++i) {
        DSL_IR_VALUE_REFERENCE_RECORD reference;
        DSL_IR_Value_Reference_Record_Init(&reference);
        reference.owner_node_id = node_id;
        reference.ordinal = i;
        reference.value_id = operand_records[i]->image_value_id;
        DSL_IR_VALUE_REFERENCE_ID id =
            DSL_IR_Image_Add_Value_Reference(&reference);
        if (id == DSL_IR_VALUE_REFERENCE_INVALID_ID)
            return FALSE;
        if (i == 0)
            first_operand_id = id;
    }

    for (UINT32 i = 0; i < attr_count; ++i) {
        DSL_IR_ATTRIBUTE_RECORD attribute;
        DSL_IR_Attribute_Record_Init(&attribute);
        attribute.owner_node_id = node_id;
        attribute.value_kind = DSL_IR_ATTRIBUTE_VALUE_STRING;
        attribute.name = Save_Str(attrs[i].name);
        attribute.value = Save_Str(DSL_Builder_Safe_String(attrs[i].value));
        DSL_IR_ATTRIBUTE_ID id = DSL_IR_Image_Add_Attribute(&attribute);
        if (id == DSL_IR_ATTRIBUTE_INVALID_ID)
            return FALSE;
        if (i == 0)
            first_attribute_id = id;
    }

    DSL_IR_Value_Record_Init(&value_record);
    value_record.value_kind = value_kind;
    value_record.producer_node_id = node_id;
    value_record.ty = result_ty;
    value_record.st = result_st;
    value_record.name = Save_Str(ST_name(St_Table[result_st]));
    if (DSL_Builder_Active_PU != NULL) {
        std::string metadata = "owner_pu=";
        metadata += ST_name
                        (St_Table[PU_Info_proc_sym(DSL_Builder_Active_PU)]);
        value_record.metadata = Save_Str(metadata.c_str());
    }
    *result_value_id = DSL_IR_Image_Add_Value(&value_record);
    if (*result_value_id == DSL_IR_VALUE_INVALID_ID)
        return FALSE;

    return DSL_IR_Image_Set_Node_Links
               (node_id, first_operand_id, operand_count,
                first_attribute_id, attr_count, *result_value_id);
}

static DSL_BUILDER_VALUE
DSL_Builder_Create_Native_Value
        (DSL_OPERATOR dsl_operator,
         UINT16 version,
         const char *payload,
         DSL_BUILDER_VALUE *kids,
         UINT32 kid_count,
         const DSL_BUILDER_OPERATOR_ATTRIBUTE *attrs,
         UINT32 attr_count,
         const char *result_name,
         TY_IDX result_ty,
         UINT32 value_kind)
{
    DSL_BUILDER_VALUE_RECORD **operand_records = NULL;
    WN **operands = NULL;
    WN *expression;
    WN *assignment;
    ST_IDX result_st;
    DSL_IR_VALUE_ID image_value_id;

    if ((DSL_Builder_Active_PU == NULL && DSL_Builder_PU_Root != NULL) ||
        !TY_is_tensor_extension(result_ty) ||
        (kid_count != 0 && kids == NULL) ||
        (attr_count != 0 && attrs == NULL))
        return NULL;

    if (kid_count != 0) {
        operand_records = new DSL_BUILDER_VALUE_RECORD *[kid_count];
        operands = new WN *[kid_count];
        for (UINT32 i = 0; i < kid_count; ++i) {
            operand_records[i] = DSL_Builder_Find_Value_Record(kids[i]);
            if (operand_records[i] == NULL ||
                operand_records[i]->pu != DSL_Builder_Active_PU) {
                delete [] operands;
                delete [] operand_records;
                return NULL;
            }
            operands[i] = WN_CreateLdid
                              (OPR_LDID, MTYPE_M, MTYPE_M, 0,
                               operand_records[i]->result_st,
                               operand_records[i]->result_ty);
        }
    }

    expression = DSL_WN_Create_Native
                     (dsl_operator, version, payload, operands, kid_count);
    delete [] operands;
    if (expression == NULL) {
        delete [] operand_records;
        return NULL;
    }

    result_st = DSL_Builder_Create_Tensor_Result_Symbol
                    (result_name, result_ty, SCLASS_AUTO, EXPORT_LOCAL);
    if (ST_IDX_index(result_st) == 0) {
        delete [] operand_records;
        return NULL;
    }
    assignment = WN_CreateStid(OPR_STID, MTYPE_V, MTYPE_M, 0, result_st,
                               result_ty, expression);

    if (!DSL_Builder_Add_Image_Node
             (dsl_operator, version, payload, operand_records, kid_count, attrs,
              attr_count, result_st, result_ty, value_kind,
              &image_value_id)) {
        delete [] operand_records;
        return NULL;
    }
    std::string canonical_key = DSL_Builder_Value_Canonical_Key
                                    (dsl_operator, version, result_ty,
                                     operand_records, kid_count, attrs,
                                     attr_count, result_name);
    delete [] operand_records;

    DSL_BUILDER_VALUE_RECORD record;
    record.pu = DSL_Builder_Active_PU;
    record.assignment = assignment;
    record.expression = expression;
    record.result_st = result_st;
    record.result_ty = result_ty;
    record.image_value_id = image_value_id;
    record.value_kind = value_kind;
    record.tensor_tcon = TCON_IDX_ZERO;
    record.canonical_key = canonical_key;
    record.materializing = FALSE;
    record.materialized = FALSE;
    DSL_builder_value_registry.push_back(record);
    return assignment;
}

TY_IDX
DSL_Builder_Create_Tensor_Type_Core
        (const char *name,
         TY_IDX element_ty,
         const DSL_BUILDER_TENSOR_TYPE_CORE *type_core)
{
    char rank_buf[32];
    INT32 rank = type_core == NULL ? -1 : type_core->rank;
    TY_IDX tensor_ty = TY_Create_Tensor_Type (name, element_ty, rank);

    if (type_core != NULL) {
        snprintf (rank_buf, sizeof(rank_buf), "%d", type_core->rank);
        DSL_Builder_Bind_Attribute_If_Present
            (tensor_ty, TY_TENSOR_SCHEMA_KIND, type_core->kind);
        DSL_Builder_Bind_Attribute_If_Present
            (tensor_ty, TY_TENSOR_SCHEMA_DTYPE, type_core->dtype);
        TY_tensor_bind_attribute (tensor_ty, TY_TENSOR_SCHEMA_RANK, rank_buf);
        DSL_Builder_Bind_Attribute_If_Present
            (tensor_ty, TY_TENSOR_SCHEMA_SHAPE, type_core->logical_shape);
    }

    return tensor_ty;
}

BOOL
DSL_Builder_Attach_Tensor_Descriptor
        (TY_IDX ty,
         const DSL_BUILDER_TENSOR_DESCRIPTOR *descriptor)
{
    if (descriptor == NULL || !TY_is_tensor_extension (ty) ||
        TY_tensor_is_canonical(ty))
        return FALSE;

    DSL_Builder_Bind_Attribute_If_Present
        (ty, TY_TENSOR_SCHEMA_KIND, descriptor->type_core.kind);
    DSL_Builder_Bind_Attribute_If_Present
        (ty, TY_TENSOR_SCHEMA_DTYPE, descriptor->type_core.dtype);
    if (descriptor->type_core.rank >= 0) {
        char rank_buf[32];
        snprintf (rank_buf, sizeof(rank_buf), "%d",
                  descriptor->type_core.rank);
        TY_tensor_bind_attribute (ty, TY_TENSOR_SCHEMA_RANK, rank_buf);
    }
    DSL_Builder_Bind_Attribute_If_Present
        (ty, TY_TENSOR_SCHEMA_SHAPE, descriptor->type_core.logical_shape);
    DSL_Builder_Bind_Attribute_If_Present
        (ty, TY_TENSOR_SCHEMA_TRAITS, descriptor->traits.traits);
    DSL_Builder_Bind_Attribute_If_Present
        (ty, TY_TENSOR_SCHEMA_LAYOUT, descriptor->representation.layout);
    DSL_Builder_Bind_Attribute_If_Present
        (ty, TY_TENSOR_SCHEMA_SHARDING, descriptor->representation.sharding);
    DSL_Builder_Bind_Attribute_If_Present
        (ty, TY_TENSOR_SCHEMA_PLACEMENT, descriptor->representation.placement);
    DSL_Builder_Bind_Attribute_If_Present
        (ty, TY_TENSOR_SCHEMA_MEMORY, descriptor->representation.memory);
    DSL_Builder_Bind_Attribute_If_Present
        (ty, TY_TENSOR_SCHEMA_QUANTIZATION,
         descriptor->representation.quantization);
    DSL_Builder_Bind_Attribute_If_Present
        (ty, TY_TENSOR_SCHEMA_RUNTIME_STATE,
         descriptor->representation.runtime_state);
    DSL_Builder_Bind_Attribute_If_Present
        (ty, TY_TENSOR_SCHEMA_LINEAGE, descriptor->lineage.lineage);

    return TRUE;
}

TY_IDX
DSL_Builder_Intern_Tensor_Type
        (const char *name,
         TY_IDX element_ty,
         const DSL_BUILDER_TENSOR_DESCRIPTOR *descriptor)
{
    if (descriptor == NULL)
        return TY_IDX_ZERO;

    TY_TENSOR_CANONICAL_DESCRIPTOR canonical = {
        descriptor->type_core.kind,
        descriptor->type_core.dtype,
        descriptor->type_core.rank,
        descriptor->type_core.logical_shape,
        descriptor->traits.traits,
        descriptor->representation.layout,
        descriptor->representation.sharding,
        descriptor->representation.placement,
        descriptor->representation.memory,
        descriptor->representation.quantization
    };
    return TY_Intern_Tensor_Type(name, element_ty, &canonical);
}

BOOL
DSL_Builder_Get_Tensor_Descriptor
        (TY_IDX ty,
         DSL_BUILDER_TENSOR_DESCRIPTOR *descriptor)
{
    if (descriptor == NULL || !TY_is_tensor_extension(ty))
        return FALSE;

    memset(descriptor, 0, sizeof(*descriptor));
    descriptor->type_core.kind =
        TY_tensor_attribute(ty, TY_TENSOR_SCHEMA_KIND);
    descriptor->type_core.dtype =
        TY_tensor_attribute(ty, TY_TENSOR_SCHEMA_DTYPE);
    descriptor->type_core.rank = TY_tensor_rank(ty);
    descriptor->type_core.logical_shape =
        TY_tensor_attribute(ty, TY_TENSOR_SCHEMA_SHAPE);
    descriptor->traits.traits =
        TY_tensor_attribute(ty, TY_TENSOR_SCHEMA_TRAITS);
    descriptor->representation.layout =
        TY_tensor_attribute(ty, TY_TENSOR_SCHEMA_LAYOUT);
    descriptor->representation.sharding =
        TY_tensor_attribute(ty, TY_TENSOR_SCHEMA_SHARDING);
    descriptor->representation.placement =
        TY_tensor_attribute(ty, TY_TENSOR_SCHEMA_PLACEMENT);
    descriptor->representation.memory =
        TY_tensor_attribute(ty, TY_TENSOR_SCHEMA_MEMORY);
    descriptor->representation.quantization =
        TY_tensor_attribute(ty, TY_TENSOR_SCHEMA_QUANTIZATION);
    descriptor->representation.runtime_state =
        TY_tensor_attribute(ty, TY_TENSOR_SCHEMA_RUNTIME_STATE);
    descriptor->lineage.lineage =
        TY_tensor_attribute(ty, TY_TENSOR_SCHEMA_LINEAGE);
    return TRUE;
}

BOOL
DSL_Builder_Tensor_Type_Is_Canonical (TY_IDX ty)
{
    return TY_tensor_is_canonical(ty);
}

ST_IDX
DSL_Builder_Create_Symbol
        (const char *name,
         TY_IDX ty,
         ST_CLASS sym_class,
         ST_SCLASS storage_class,
         ST_EXPORT export_class)
{
    ST *st = New_ST();
    ST_Init (st, Save_Str(DSL_Builder_Safe_String(name)), sym_class,
             storage_class, export_class, ty);
    return ST_st_idx(*st);
}

BOOL
DSL_Builder_Set_Tensor_Unique_Ownership (ST_IDX st)
{
    return DSL_Tensor_Set_Unique_Ownership(st);
}

ST_IDX
DSL_Builder_Create_Tensor_Result_Symbol
        (const char *name,
         TY_IDX ty,
         ST_SCLASS storage_class,
         ST_EXPORT export_class)
{
    return DSL_Tensor_Create_Result_Symbol
               (name, ty, storage_class, export_class);
}

BOOL
DSL_Builder_Tensor_Has_Unique_Ownership (ST_IDX st)
{
    return DSL_Tensor_Has_Unique_Ownership(st);
}

static BOOL
DSL_Builder_Attach_Tensor_TCON
        (DSL_BUILDER_VALUE value,
         TCON_IDX tensor_tcon)
{
    DSL_BUILDER_VALUE_RECORD *record =
        DSL_Builder_Find_Value_Record(value);
    DSL_TENSOR_TCON_RECORD tensor_record;
    char tcon_text[32];

    if (record == NULL || tensor_tcon == TCON_IDX_ZERO ||
        !DSL_Tensor_TCON_Get(tensor_tcon, &tensor_record) ||
        tensor_record.descriptor_ty != record->result_ty)
        return FALSE;

    record->tensor_tcon = tensor_tcon;
    snprintf(tcon_text, sizeof(tcon_text), "%u", (UINT32)tensor_tcon);
    ST_tensor_bind_metadata(record->result_st, "tensor_tcon_idx", tcon_text);
    return TRUE;
}

static BOOL
DSL_Builder_Create_Compact_Tensor_TCON
        (TY_IDX tensor_ty,
         const char *value,
         TCON_IDX *tensor_tcon)
{
    std::vector<UINT64> dimensions;
    DSL_TENSOR_TCON_CREATE_INFO info;
    TY_IDX element_ty;
    TYPE_ID element_mtype;
    TCON scalar;
    TCON_IDX scalar_tcon;
    UINT64 element_count = 1;
    INT64 integer_value;
    char *end;

    if (tensor_tcon != NULL)
        *tensor_tcon = TCON_IDX_ZERO;
    if (tensor_tcon == NULL || value == NULL ||
        !DSL_Builder_Tensor_Type_Is_Canonical(tensor_ty) ||
        !DSL_Builder_Parse_Static_Shape
             (TY_tensor_attribute(tensor_ty, TY_TENSOR_SCHEMA_SHAPE),
              &dimensions))
        return FALSE;

    for (UINT32 i = 0; i < dimensions.size(); ++i) {
        if (dimensions[i] == 0 ||
            element_count > ~(UINT64)0 / dimensions[i])
            return FALSE;
        element_count *= dimensions[i];
    }

    element_ty = TY_tensor_element_ty(tensor_ty);
    element_mtype = TY_mtype(element_ty);
    if (!MTYPE_is_integral(element_mtype) || TY_size(element_ty) == 0)
        return FALSE;

    errno = 0;
    integer_value = strtoll(value, &end, 0);
    if (errno == ERANGE || end == value || *end != '\0')
        return FALSE;
    scalar = Host_To_Targ(element_mtype, integer_value);
    if (Targ_To_Host(scalar) != integer_value)
        return FALSE;
    scalar_tcon = Enter_tcon(scalar);
    if (scalar_tcon == TCON_IDX_ZERO)
        return FALSE;

    memset(&info, 0, sizeof(info));
    info.descriptor_ty = tensor_ty;
    info.scalar_tcon = scalar_tcon;
    info.element_mtype = element_mtype;
    info.element_count = element_count;
    info.element_size = TY_size(element_ty);
    if (element_count > ~(UINT64)0 / info.element_size)
        return FALSE;
    info.logical_bytes = element_count * info.element_size;
    info.required_alignment = TY_align(tensor_ty);
    if (info.required_alignment < info.element_size)
        info.required_alignment = info.element_size;
    info.scalar_integer_value = integer_value;

    if (integer_value == 0)
        return DSL_Tensor_TCON_Create_Zero
                   (&info, tensor_tcon, NULL);
    if (integer_value == 1)
        return DSL_Tensor_TCON_Create_One
                   (&info, tensor_tcon, NULL);
    return DSL_Tensor_TCON_Create_Splat(&info, tensor_tcon, NULL);
}

static DSL_BUILDER_VALUE
DSL_Builder_Create_Tensor_Constant_Value
        (const char *name,
         TY_IDX tensor_ty,
         const char *dtype,
         UINT32 rank,
         const char *logical_shape,
         const char *value_kind,
         const char *value,
         TCON_IDX tensor_tcon)
{
    char *payload;
    DSL_BUILDER_VALUE result;
    DSL_BUILDER_OPERATOR_ATTRIBUTE attrs[2];

    if (name == NULL || name[0] == '\0' ||
        dtype == NULL || dtype[0] == '\0' ||
        logical_shape == NULL || value_kind == NULL || value == NULL)
        return NULL;

    payload = DSL_Builder_Format_Tensor_Constant_Payload
                  (name, dtype, rank, logical_shape, value_kind, value);
    attrs[0].name = "value_kind";
    attrs[0].value = value_kind;
    attrs[1].name = "value";
    attrs[1].value = value;
    result = DSL_Builder_Create_Native_Value
                 (OPR_DSLTENSORCONST, 1, payload, NULL, 0, attrs, 2,
                  name, tensor_ty, DSL_IR_VALUE_CONSTANT);
    delete [] payload;
    if (result != NULL && tensor_tcon != TCON_IDX_ZERO)
        DSL_Builder_Attach_Tensor_TCON(result, tensor_tcon);
    return result;
}

DSL_BUILDER_VALUE
DSL_Builder_Create_Tensor_Constant
        (const char *name,
         TY_IDX tensor_ty,
         const char *dtype,
         UINT32 rank,
         const char *logical_shape,
         const char *value_kind,
         const char *value)
{
    TCON_IDX tensor_tcon = TCON_IDX_ZERO;

    if (value_kind != NULL && strcmp(value_kind, "splat") == 0)
        DSL_Builder_Create_Compact_Tensor_TCON
            (tensor_ty, value, &tensor_tcon);
    return DSL_Builder_Create_Tensor_Constant_Value
               (name, tensor_ty, dtype, rank, logical_shape, value_kind,
                value, tensor_tcon);
}

DSL_BUILDER_VALUE
DSL_Builder_Create_Model_Input
        (const char *name,
         TY_IDX tensor_ty,
         UINT32 input_ordinal)
{
    char payload[320];
    char ordinal[32];
    DSL_BUILDER_OPERATOR_ATTRIBUTE attribute;

    if (name == NULL || name[0] == '\0' ||
        !TY_is_tensor_extension(tensor_ty))
        return NULL;

    snprintf(ordinal, sizeof(ordinal), "%u", input_ordinal);
    snprintf(payload, sizeof(payload), "name=%s;attr.input_ordinal=%s",
             name, ordinal);
    attribute.name = "attr.input_ordinal";
    attribute.value = ordinal;
    return DSL_Builder_Create_Native_Value
               (OPR_DSLMODELINPUT, 2, payload, NULL, 0, &attribute, 1,
                name, tensor_ty, DSL_IR_VALUE_SYMBOL);
}

static UINT64
DSL_Builder_Tensor_Element_Size (const char *dtype)
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
DSL_Builder_Static_Tensor_Byte_Size
        (const char *dtype,
         INT32 rank,
         const char *shape,
         UINT64 *element_size,
         UINT64 *byte_size)
{
    UINT64 item_size = DSL_Builder_Tensor_Element_Size(dtype);
    if (item_size == 0 || rank < 0 || shape == NULL || shape[0] != '[')
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
    if (*cursor != '\0' || dimension_count != rank ||
        elements > ~(UINT64)0 / item_size)
        return FALSE;
    if (element_size != NULL)
        *element_size = item_size;
    if (byte_size != NULL)
        *byte_size = elements * item_size;
    return TRUE;
}

DSL_BUILDER_VALUE
DSL_Builder_Create_External_Tensor_Constant
        (const char *name,
         TY_IDX tensor_ty,
         const DSL_BUILDER_EXTERNAL_TENSOR_REFERENCE *reference)
{
    TENSOR_DESCRIPTOR_RECORD descriptor;
    const char *dtype;
    const char *shape;
    const char *checksum;
    DSL_BUILDER_VALUE result;

    if (name == NULL || name[0] == '\0' ||
        reference == NULL || reference->storage_format == NULL ||
        reference->storage_format[0] == '\0' ||
        reference->side_file == NULL || reference->side_file[0] == '\0' ||
        reference->tensor_key == NULL || reference->tensor_key[0] == '\0' ||
        reference->byte_length == 0 ||
        reference->byte_offset + reference->byte_length <
            reference->byte_offset ||
        !TY_get_tensor_descriptor_record(tensor_ty, &descriptor))
        return NULL;

    checksum = reference->checksum == NULL ? "" : reference->checksum;
    if (checksum[0] != '\0') {
        if (strlen(checksum) != 64)
            return NULL;
        for (UINT32 i = 0; i < 64; ++i) {
            if (!isxdigit((unsigned char)checksum[i]))
                return NULL;
        }
    }

    dtype = descriptor.dtype == 0 ? NULL : Index_To_Str(descriptor.dtype);
    shape = descriptor.logical_shape == 0 ? NULL :
            Index_To_Str(descriptor.logical_shape);
    UINT64 element_size = 0;
    UINT64 tensor_byte_size = 0;
    if (dtype == NULL || shape == NULL || descriptor.rank < 0 ||
        !DSL_Builder_Static_Tensor_Byte_Size
             (dtype, descriptor.rank, shape, &element_size,
              &tensor_byte_size) ||
        reference->byte_offset % element_size != 0 ||
        reference->byte_length != tensor_byte_size)
        return NULL;

    size_t uri_size = strlen(reference->storage_format) +
                      strlen(reference->side_file) +
                      strlen(reference->tensor_key) + strlen(checksum) + 96;
    char *storage_uri = new char[uri_size];
    snprintf(storage_uri, uri_size,
             "%s://%s#%s?offset=%llu&length=%llu&checksum=%s",
             reference->storage_format, reference->side_file,
             reference->tensor_key,
             (unsigned long long)reference->byte_offset,
             (unsigned long long)reference->byte_length, checksum);
    result = DSL_Builder_Create_Tensor_Constant
                 (name, tensor_ty, dtype, descriptor.rank, shape,
                  "external_data", storage_uri);
    delete [] storage_uri;
    if (result == NULL)
        return NULL;

    char byte_offset[32];
    char byte_length[32];
    snprintf(byte_offset, sizeof(byte_offset), "%llu",
             (unsigned long long)reference->byte_offset);
    snprintf(byte_length, sizeof(byte_length), "%llu",
             (unsigned long long)reference->byte_length);
    ST_IDX result_st = WN_st_idx(result);
    ST_tensor_bind_metadata(result_st, "storage_format",
                            reference->storage_format);
    ST_tensor_bind_metadata(result_st, "storage_file", reference->side_file);
    ST_tensor_bind_metadata(result_st, "storage_tensor_key",
                            reference->tensor_key);
    ST_tensor_bind_metadata(result_st, "storage_byte_offset", byte_offset);
    ST_tensor_bind_metadata(result_st, "storage_byte_length", byte_length);
    ST_tensor_bind_metadata(result_st, "storage_checksum", checksum);
    return result;
}

static WN *
DSL_Builder_Project_Compact_Tensor_Constant
        (const DSL_BUILDER_VALUE_RECORD *record)
{
    DSL_TENSOR_TCON_RECORD tensor_tcon;
    TY_IDX element_ty;
    TYPE_ID element_mtype;
    ST *scalar_st;

    if (record == NULL || record->tensor_tcon == TCON_IDX_ZERO ||
        !DSL_Tensor_TCON_Get(record->tensor_tcon, &tensor_tcon) ||
        (tensor_tcon.storage_kind != DSL_TENSOR_TCON_STORAGE_ZERO &&
         tensor_tcon.storage_kind != DSL_TENSOR_TCON_STORAGE_ONE &&
         tensor_tcon.storage_kind != DSL_TENSOR_TCON_STORAGE_SPLAT))
        return NULL;

    element_ty = TY_tensor_element_ty(record->result_ty);
    element_mtype = TY_mtype(element_ty);
    scalar_st = New_Const_Sym(tensor_tcon.scalar_tcon, element_ty);
    return scalar_st == NULL ? NULL :
        WN_CreateConst(OPR_CONST, element_mtype, MTYPE_V, scalar_st);
}

static DSL_BUILDER_VALUE
DSL_Builder_Try_Fold_Tensor_Binary
        (DSL_OPERATOR dsl_operator,
         UINT16 version,
         DSL_BUILDER_VALUE *kids,
         UINT32 kid_count,
         const DSL_BUILDER_OPERATOR_ATTRIBUTE *attrs,
         UINT32 attr_count,
         const char *result_name,
         TY_IDX result_ty)
{
    DSL_BUILDER_VALUE_RECORD *kid_record[2];
    DSL_SIMP_BINARY_CANDIDATE simp_candidate;
    DSL_SIMP_BINARY_RESULT simp_result;
    DSL_TENSOR_FOLD_CANDIDATE fold_candidate;
    DSL_TENSOR_FOLD_OUTPUT fold_output;
    DSL_TENSOR_FOLD_POLICY fold_policy;
    DSL_TENSOR_TCON_RECORD folded_record;
    DSL_IR_ATTRIBUTE_RECORD *fold_attrs = NULL;
    TCON operands[2];
    TY_IDX operand_ty[2];
    TY_IDX result_types[1];
    TCON_IDX folded_tcon;
    DSL_BUILDER_VALUE folded_value;
    DSL_SIMP_STATUS simp_status;
    char folded_text[64];
    const char *dtype;
    const char *shape;
    INT32 rank;

    if (!DSL_Builder_Tensor_Folding_Enabled() || kids == NULL ||
        kid_count != 2 ||
        (dsl_operator != OPR_DSLADD && dsl_operator != OPR_DSLMUL &&
         dsl_operator != OPR_DSLDIV && dsl_operator != OPR_DSLREM))
        return NULL;

    for (UINT32 i = 0; i < 2; ++i) {
        kid_record[i] = DSL_Builder_Find_Value_Record(kids[i]);
        if (kid_record[i] == NULL ||
            kid_record[i]->tensor_tcon == TCON_IDX_ZERO)
            return NULL;
        if (!DSL_Tensor_TCON_Get_Carrier
                 (kid_record[i]->tensor_tcon, &operands[i]))
            return NULL;
        operand_ty[i] = kid_record[i]->result_ty;
    }

    memset(&simp_candidate, 0, sizeof(simp_candidate));
    simp_candidate.dsl_operator = dsl_operator;
    simp_candidate.version = version;
    simp_candidate.result_ty = result_ty;
    simp_candidate.operand_ty[0] = operand_ty[0];
    simp_candidate.operand_ty[1] = operand_ty[1];
    simp_candidate.projected_kid[0] =
        DSL_Builder_Project_Compact_Tensor_Constant(kid_record[0]);
    simp_candidate.projected_kid[1] =
        DSL_Builder_Project_Compact_Tensor_Constant(kid_record[1]);
    simp_candidate.projection_kind = DSL_SIMP_PROJECTION_TEST_SCALAR;
    if (simp_candidate.projected_kid[0] == NULL ||
        simp_candidate.projected_kid[1] == NULL)
        return NULL;
    simp_status = DSL_Simp_Binary(&simp_candidate, &simp_result);
    if (simp_status != DSL_SIMP_ENGINE_REWRITE &&
        simp_status != DSL_SIMP_UNCHANGED)
        return NULL;

    if (attr_count != 0) {
        fold_attrs = new DSL_IR_ATTRIBUTE_RECORD[attr_count];
        for (UINT32 i = 0; i < attr_count; ++i) {
            DSL_IR_Attribute_Record_Init(&fold_attrs[i]);
            fold_attrs[i].value_kind = DSL_IR_ATTRIBUTE_VALUE_STRING;
            fold_attrs[i].name = Save_Str(attrs[i].name);
            fold_attrs[i].value =
                Save_Str(DSL_Builder_Safe_String(attrs[i].value));
        }
    }

    DSL_Tensor_Fold_Default_Policy(&fold_policy);
    result_types[0] = result_ty;
    memset(&fold_candidate, 0, sizeof(fold_candidate));
    fold_candidate.dsl_operator = dsl_operator;
    fold_candidate.version = version;
    fold_candidate.result_count = 1;
    fold_candidate.operand_count = 2;
    fold_candidate.operands = operands;
    fold_candidate.operand_ty = operand_ty;
    fold_candidate.result_ty = result_types;
    fold_candidate.attributes = fold_attrs;
    fold_candidate.attribute_count = attr_count;
    fold_candidate.policy = &fold_policy;
    DSL_TENSOR_FOLD_STATUS fold_status =
        Targ_DSL_WhirlOp(&fold_candidate, &fold_output);
    delete [] fold_attrs;
    if (fold_status != DSL_TENSOR_FOLD_SUCCESS ||
        fold_output.result_count != 1 ||
        fold_output.results[0].kind != DSL_TENSOR_FOLD_RESULT_TCON ||
        fold_output.results[0].result_ty != result_ty)
        return NULL;

    if (!DSL_Tensor_TCON_Find_Carrier
             (&fold_output.results[0].result, &folded_tcon) ||
        !DSL_Tensor_TCON_Get(folded_tcon, &folded_record))
        return NULL;

    dtype = TY_tensor_attribute(result_ty, TY_TENSOR_SCHEMA_DTYPE);
    shape = TY_tensor_attribute(result_ty, TY_TENSOR_SCHEMA_SHAPE);
    rank = TY_tensor_rank(result_ty);
    if (dtype == NULL || shape == NULL || rank < 0)
        return NULL;
    snprintf(folded_text, sizeof(folded_text), "%lld",
             (long long)folded_record.scalar_integer_value);
    folded_value = DSL_Builder_Create_Tensor_Constant_Value
                       (result_name, result_ty, dtype, rank, shape,
                        "splat", folded_text, folded_tcon);
    if (folded_value != NULL) {
        DSL_BUILDER_VALUE_RECORD *folded_value_record =
            DSL_Builder_Find_Value_Record(folded_value);
        ST_tensor_bind_metadata
            (folded_value_record->result_st, "tensor_fold.origin",
             DSL_OPERATOR_name(dsl_operator));
    }
    return folded_value;
}

DSL_BUILDER_OPERATOR
DSL_Builder_Create_Operator
        (DSL_OPCODE_ID opcode_id,
         UINT16 version,
         DSL_BUILDER_VALUE *kids,
         UINT32 kid_count,
         const DSL_BUILDER_OPERATOR_ATTRIBUTE *attrs,
         UINT32 attr_count)
{
    DSL_OPCODE_INFO info;
    DSL_OPERATOR dsl_operator;
    DSL_OPERATOR_INFO logical_info;
    DSL_BUILDER_VALUE canonical_kids[2];
    DSL_BUILDER_VALUE *effective_kids = kids;
    char *payload;
    WN *wn;

    if (!DSL_Opcode_Get_Info (opcode_id, &info) || info.version != version)
        return NULL;

    dsl_operator = DSL_Operator_Find(info.name, strlen(info.name), version);
    if (dsl_operator != OPR_DSLUNKNOWN &&
        (!DSL_Operator_Get_Info_Version
              (dsl_operator, version, &logical_info) ||
         (DSL_Builder_Requires_Exact_Attribute_Schema
              (dsl_operator, version) &&
         !DSL_Builder_Attributes_Match_Schema
               (&logical_info, attrs, attr_count))))
        return NULL;

    if (dsl_operator != OPR_DSLUNKNOWN && kid_count == 2 && kids != NULL) {
        canonical_kids[0] = kids[0];
        canonical_kids[1] = kids[1];
        DSL_Builder_Canonicalize_Kids
            (dsl_operator, version, canonical_kids, kid_count);
        effective_kids = canonical_kids;
    }
    payload = DSL_Builder_Format_Operator_Payload
                  (effective_kids, kid_count, attrs, attr_count);
    if ((dsl_operator == OPR_DSLADD || dsl_operator == OPR_DSLMUL ||
         dsl_operator == OPR_DSLDIV || dsl_operator == OPR_DSLREM ||
         dsl_operator == OPR_DSLMATMUL ||
         dsl_operator == OPR_DSLLINEAR ||
         dsl_operator == OPR_DSLRELU || dsl_operator == OPR_DSLFLATTEN ||
         dsl_operator == OPR_DSLRESIDUALADD ||
         dsl_operator == OPR_DSLCONV2D ||
         dsl_operator == OPR_DSLBATCHNORMINFER ||
         dsl_operator == OPR_DSLMAXPOOL2D ||
         dsl_operator == OPR_DSLGLOBALAVGPOOL2D ||
         dsl_operator == OPR_DSLOUTPUTLOGITS ||
         dsl_operator == OPR_DSLRESHAPE ||
         dsl_operator == OPR_DSLTRANSPOSE ||
         dsl_operator == OPR_DSLTOKENEMBEDDING ||
         dsl_operator == OPR_DSLRMSNORM ||
         dsl_operator == OPR_DSLROTARYEMBEDDING ||
         dsl_operator == OPR_DSLATTENTION ||
         dsl_operator == OPR_DSLSWIGLU ||
         dsl_operator == OPR_DSLSCATTER) && kid_count != 0) {
        if ((info.nkids >= 0 && (UINT32)info.nkids != kid_count) ||
            (attr_count != 0 && attrs == NULL)) {
            delete [] payload;
            return NULL;
        }
        DSL_BUILDER_VALUE_RECORD *first =
            DSL_Builder_Find_Value_Record(effective_kids[0]);
        char result_name[64];
        char result_type_name[80];

        for (UINT32 i = 0; i < attr_count; ++i) {
            if (attrs[i].name == NULL || attrs[i].name[0] == '\0') {
                delete [] payload;
                return NULL;
            }
        }
        if (first != NULL) {
            snprintf (result_name, sizeof(result_name), "dsl_result_%u",
                      ++DSL_Builder_Result_Number);
            TY_IDX result_ty = first->result_ty;
            if (dsl_operator == OPR_DSLMATMUL) {
                DSL_BUILDER_VALUE_RECORD *second = kid_count < 2 ? NULL :
                    DSL_Builder_Find_Value_Record(kids[1]);
                snprintf (result_type_name, sizeof(result_type_name),
                          "%s_type", result_name);
                result_ty = second == NULL ? TY_IDX_ZERO :
                    DSL_Builder_Create_Matmul_Result_Type
                        (version, first->result_ty, second->result_ty,
                         attrs, attr_count,
                         result_type_name);
                if (result_ty == TY_IDX_ZERO) {
                    delete [] payload;
                    return NULL;
                }
            } else if (dsl_operator == OPR_DSLLINEAR) {
                DSL_BUILDER_VALUE_RECORD *weight = kid_count < 2 ? NULL :
                    DSL_Builder_Find_Value_Record(kids[1]);
                DSL_BUILDER_VALUE_RECORD *bias = kid_count < 3 ? NULL :
                    DSL_Builder_Find_Value_Record(kids[2]);
                snprintf (result_type_name, sizeof(result_type_name),
                          "%s_type", result_name);
                result_ty = weight == NULL ||
                            (version == 2 && bias == NULL) ? TY_IDX_ZERO :
                    DSL_Builder_Create_Linear_Result_Type
                        (version, first->result_ty, weight->result_ty,
                         bias == NULL ? TY_IDX_ZERO : bias->result_ty,
                         attrs, attr_count, result_type_name);
                if (result_ty == TY_IDX_ZERO) {
                    delete [] payload;
                    return NULL;
                }
            } else if (dsl_operator == OPR_DSLFLATTEN) {
                snprintf (result_type_name, sizeof(result_type_name),
                          "%s_type", result_name);
                result_ty = DSL_Builder_Create_Flatten_Result_Type
                                (first->result_ty, attrs, attr_count,
                                 result_type_name);
                if (result_ty == TY_IDX_ZERO) {
                    delete [] payload;
                    return NULL;
                }
            } else if (dsl_operator == OPR_DSLCONV2D) {
                DSL_BUILDER_VALUE_RECORD *weight = kid_count < 2 ? NULL :
                    DSL_Builder_Find_Value_Record(kids[1]);
                DSL_BUILDER_VALUE_RECORD *bias = kid_count < 3 ? NULL :
                    DSL_Builder_Find_Value_Record(kids[2]);
                snprintf (result_type_name, sizeof(result_type_name),
                          "%s_type", result_name);
                result_ty = weight == NULL || bias == NULL ? TY_IDX_ZERO :
                    DSL_Builder_Create_Conv2D_Result_Type
                        (first->result_ty, weight->result_ty, bias->result_ty,
                         attrs, attr_count, result_type_name);
                if (result_ty == TY_IDX_ZERO) {
                    delete [] payload;
                    return NULL;
                }
            } else if (dsl_operator == OPR_DSLMAXPOOL2D) {
                snprintf (result_type_name, sizeof(result_type_name),
                          "%s_type", result_name);
                result_ty = DSL_Builder_Create_Max_Pool_Result_Type
                                (first->result_ty, attrs, attr_count,
                                 result_type_name);
                if (result_ty == TY_IDX_ZERO) {
                    delete [] payload;
                    return NULL;
                }
            } else if (dsl_operator == OPR_DSLGLOBALAVGPOOL2D) {
                snprintf (result_type_name, sizeof(result_type_name),
                          "%s_type", result_name);
                result_ty = DSL_Builder_Create_Global_Avg_Pool_Result_Type
                                (first->result_ty, attrs, attr_count,
                                 result_type_name);
                if (result_ty == TY_IDX_ZERO) {
                    delete [] payload;
                    return NULL;
                }
            } else if (dsl_operator == OPR_DSLRESHAPE) {
                snprintf (result_type_name, sizeof(result_type_name),
                          "%s_type", result_name);
                result_ty = DSL_Builder_Create_Reshape_Result_Type
                                (first->result_ty, attrs, attr_count,
                                 result_type_name);
                if (result_ty == TY_IDX_ZERO) {
                    delete [] payload;
                    return NULL;
                }
            } else if (dsl_operator == OPR_DSLTRANSPOSE) {
                snprintf (result_type_name, sizeof(result_type_name),
                          "%s_type", result_name);
                result_ty = DSL_Builder_Create_Transpose_Result_Type
                                (first->result_ty, attrs, attr_count,
                                 result_type_name);
                if (result_ty == TY_IDX_ZERO) {
                    delete [] payload;
                    return NULL;
                }
            } else if (dsl_operator == OPR_DSLTOKENEMBEDDING ||
                       dsl_operator == OPR_DSLRMSNORM ||
                       dsl_operator == OPR_DSLROTARYEMBEDDING ||
                       dsl_operator == OPR_DSLATTENTION ||
                       dsl_operator == OPR_DSLSWIGLU) {
                DSL_BUILDER_VALUE_RECORD *second = kid_count < 2 ? NULL :
                    DSL_Builder_Find_Value_Record(kids[1]);
                DSL_BUILDER_VALUE_RECORD *third = kid_count < 3 ? NULL :
                    DSL_Builder_Find_Value_Record(kids[2]);
                snprintf (result_type_name, sizeof(result_type_name),
                          "%s_type", result_name);
                result_ty = second == NULL ? TY_IDX_ZERO :
                    DSL_Builder_Create_Transformer_Result_Type
                        (dsl_operator, first->result_ty, second->result_ty,
                         third == NULL ? TY_IDX_ZERO : third->result_ty,
                         attrs, attr_count, result_type_name);
                if (result_ty == TY_IDX_ZERO) {
                    delete [] payload;
                    return NULL;
                }
            }
            wn = DSL_Builder_Create_Native_Value
                     (dsl_operator, version, payload, effective_kids,
                      kid_count, attrs, attr_count, result_name, result_ty,
                      DSL_IR_VALUE_OPERATOR_RESULT);
            delete [] payload;
            return wn;
        }
    }

    WN **compatibility_kids = effective_kids;
    if (kid_count != 0) {
        compatibility_kids = new WN *[kid_count];
        for (UINT32 i = 0; i < kid_count; ++i) {
            DSL_BUILDER_VALUE_RECORD *kid_record =
                DSL_Builder_Find_Value_Record(effective_kids[i]);
            compatibility_kids[i] = kid_record == NULL ? effective_kids[i] :
                DSL_WN_Create_Opcode_Comment_Projection
                    (kid_record->expression);
            if (compatibility_kids[i] == NULL) {
                delete [] compatibility_kids;
                delete [] payload;
                return NULL;
            }
        }
    }

    if (dsl_operator != OPR_DSLUNKNOWN) {
        DSL_OPCODE_OUTPUT_MODE output_mode =
            DSL_Builder_Uses_Xpragma_Carrier(&info) ?
            DSL_OPCODE_OUTPUT_LEGACY_XPRAGMA :
            DSL_OPCODE_OUTPUT_LEGACY_EVAL;
        wn = DSL_WN_Create_Logical_Opcode
                 (dsl_operator, version, payload, compatibility_kids,
                  kid_count, output_mode);
    } else if (DSL_Builder_Uses_Xpragma_Carrier(&info)) {
        wn = DSL_WN_Create_Opcode_Xpragma(info.name, version, payload,
                                          compatibility_kids, kid_count);
    } else {
        wn = DSL_WN_Create_Opcode_With_Operands
                 (info.name, version, payload, compatibility_kids, kid_count);
    }
    if (compatibility_kids != effective_kids)
        delete [] compatibility_kids;
    delete [] payload;
    return wn;
}

DSL_BUILDER_OPERATOR
DSL_Builder_Create_Operator_With_Result
        (DSL_OPCODE_ID opcode_id,
         UINT16 version,
         DSL_BUILDER_VALUE *kids,
         UINT32 kid_count,
         const DSL_BUILDER_OPERATOR_ATTRIBUTE *attrs,
         UINT32 attr_count,
         const char *result_name,
         TY_IDX result_ty)
{
    DSL_OPCODE_INFO info;
    DSL_OPERATOR dsl_operator;
    DSL_OPERATOR_INFO logical_info;
    DSL_BUILDER_VALUE canonical_kids[2];
    DSL_BUILDER_VALUE *effective_kids = kids;
    char *payload;
    DSL_BUILDER_OPERATOR result;

    if (!DSL_Opcode_Get_Info(opcode_id, &info) || info.version != version ||
        result_name == NULL || result_name[0] == '\0' ||
        !DSL_Builder_Tensor_Type_Is_Canonical(result_ty) ||
        (kid_count != 0 && kids == NULL) ||
        (attr_count != 0 && attrs == NULL) ||
        (info.nkids >= 0 && (UINT32)info.nkids != kid_count))
        return NULL;

    for (UINT32 i = 0; i < attr_count; ++i) {
        if (attrs[i].name == NULL || attrs[i].name[0] == '\0')
            return NULL;
    }

    dsl_operator = DSL_Operator_Find(info.name, strlen(info.name), version);
    if (dsl_operator == OPR_DSLUNKNOWN ||
        !DSL_Operator_Get_Info_Version
             (dsl_operator, version, &logical_info) ||
        (DSL_Builder_Requires_Exact_Attribute_Schema
             (dsl_operator, version) &&
         !DSL_Builder_Attributes_Match_Schema
              (&logical_info, attrs, attr_count)))
        return NULL;

    if (kid_count == 2 && kids != NULL) {
        canonical_kids[0] = kids[0];
        canonical_kids[1] = kids[1];
        DSL_Builder_Canonicalize_Kids
            (dsl_operator, version, canonical_kids, kid_count);
        effective_kids = canonical_kids;
    }
    result = DSL_Builder_Try_Fold_Tensor_Binary
                 (dsl_operator, version, effective_kids, kid_count,
                  attrs, attr_count, result_name, result_ty);
    if (result != NULL)
        return result;
    payload = DSL_Builder_Format_Operator_Payload
                  (effective_kids, kid_count, attrs, attr_count);
    result = DSL_Builder_Create_Native_Value
                 (dsl_operator, version, payload, effective_kids, kid_count,
                  attrs, attr_count, result_name, result_ty,
                  DSL_IR_VALUE_OPERATOR_RESULT);
    delete [] payload;
    return result;
}

BOOL
DSL_Builder_Attach_Contract (DSL_BUILDER_OPERATOR wn,
                             DSL_CONTRACT_ID contract_id)
{
    DSL_CONTRACT_INFO info;

    return wn != NULL && DSL_Contract_Get_Info (contract_id, &info);
}

BOOL
DSL_Builder_Attach_Metadata
        (ST_IDX st,
         const DSL_BUILDER_COMPILER_METADATA *metadata,
         UINT32 metadata_count)
{
    if (ST_IDX_index(st) == 0)
        return FALSE;
    if (metadata_count != 0 && metadata == NULL)
        return FALSE;

    for (UINT32 i = 0; i < metadata_count; ++i) {
        if (metadata[i].name == NULL || metadata[i].name[0] == '\0')
            return FALSE;
        ST_tensor_bind_metadata (st, metadata[i].name, metadata[i].value);
    }

    return TRUE;
}

BOOL
DSL_Builder_Attach_Value_Metadata
        (DSL_BUILDER_VALUE value,
         const DSL_BUILDER_COMPILER_METADATA *metadata,
         UINT32 metadata_count)
{
    DSL_BUILDER_VALUE_RECORD *record = DSL_Builder_Find_Value_Record(value);

    return DSL_Builder_Select_Value_PU(record) &&
           DSL_Builder_Attach_Metadata
               (record->result_st, metadata, metadata_count);
}

BOOL
DSL_Builder_Attach_Value_Lineage
        (DSL_BUILDER_VALUE value,
         const char *lineage)
{
    DSL_BUILDER_VALUE_RECORD *record = DSL_Builder_Find_Value_Record(value);

    if (!DSL_Builder_Select_Value_PU(record) ||
        lineage == NULL || lineage[0] == '\0')
        return FALSE;
    ST_tensor_bind_attribute
        (record->result_st,
         TY_tensor_schema_key_name(TY_TENSOR_SCHEMA_LINEAGE), lineage);
    return TRUE;
}

TY_IDX
DSL_Builder_Get_Value_Type (DSL_BUILDER_VALUE value)
{
    DSL_BUILDER_VALUE_RECORD *record = DSL_Builder_Find_Value_Record(value);
    return record == NULL ? TY_IDX_ZERO : record->result_ty;
}

ST_IDX
DSL_Builder_Get_Value_Result_Symbol (DSL_BUILDER_VALUE value)
{
    DSL_BUILDER_VALUE_RECORD *record = DSL_Builder_Find_Value_Record(value);
    return record == NULL ? ST_IDX_ZERO : record->result_st;
}

DSL_IR_VALUE_ID
DSL_Builder_Get_Value_Image_Id (DSL_BUILDER_VALUE value)
{
    DSL_BUILDER_VALUE_RECORD *record = DSL_Builder_Find_Value_Record(value);
    return record == NULL ? DSL_IR_VALUE_INVALID_ID : record->image_value_id;
}

DSL_FHE_TENSOR_BINDING_ID
DSL_Builder_Bind_FHE_Tensor_Descriptor
        (TY_IDX tensor_ty,
         DSL_FHE_ENCRYPTION_DESCRIPTOR_ID descriptor_id,
         UINT32 flags)
{
    return DSL_FHE_Intern_Tensor_Binding(tensor_ty, descriptor_id, flags);
}

DSL_FHE_ENTRY_CONTRACT_ID
DSL_Builder_Attach_FHE_Entry_Contract
        (DSL_BUILDER_PROGRAM_UNIT pu,
         const DSL_FHE_ENTRY_CONTRACT_INFO *info)
{
    if (pu == NULL || info == NULL)
        return DSL_FHE_ENTRY_CONTRACT_INVALID_ID;
    DSL_FHE_ENTRY_CONTRACT_RECORD record;
    DSL_FHE_Entry_Contract_Record_Init(&record);
    record.owner_pu_st = PU_Info_proc_sym(pu);
    record.config_id = info->config_id;
    record.input_count = info->input_count;
    record.output_count = info->output_count;
    record.parameter_count = info->parameter_count;
    record.encrypted_io_policy = info->encrypted_io_policy;
    record.parameter_policy = info->parameter_policy;
    record.flags = info->flags;
    return DSL_FHE_Add_Entry_Contract(&record);
}

DSL_FHE_ENTRY_VALUE_ID
DSL_Builder_Declare_FHE_Entry_Value
        (DSL_FHE_ENTRY_CONTRACT_ID entry_contract_id,
         DSL_BUILDER_VALUE value,
         UINT32 ordinal,
         DSL_FHE_ENTRY_VALUE_ROLE role,
         const DSL_FHE_ENTRY_VALUE_INFO *info)
{
    DSL_BUILDER_VALUE_RECORD *value_record =
        DSL_Builder_Find_Value_Record(value);
    DSL_FHE_ENTRY_CONTRACT_RECORD entry;
    DSL_FHE_TENSOR_BINDING_RECORD binding;
    if (value_record == NULL || info == NULL ||
        !DSL_FHE_Get_Entry_Contract(entry_contract_id, &entry) ||
        !DSL_FHE_Find_Tensor_Binding
             (value_record->result_ty, info->encryption_descriptor_id,
              &binding))
        return DSL_FHE_ENTRY_VALUE_INVALID_ID;

    UINT32 role_count = 0;
    for (UINT32 i = 0; i < entry.entry_value_count; ++i) {
        DSL_FHE_ENTRY_VALUE_RECORD existing;
        if (!DSL_FHE_Get_Entry_Value
                 (entry.first_entry_value_id + i, &existing))
            return DSL_FHE_ENTRY_VALUE_INVALID_ID;
        if (existing.role == (UINT32)role)
            ++role_count;
    }
    UINT32 expected_role_count = role == DSL_FHE_ENTRY_VALUE_INPUT ?
                                 entry.input_count :
                                 role == DSL_FHE_ENTRY_VALUE_OUTPUT ?
                                 entry.output_count :
                                 role == DSL_FHE_ENTRY_VALUE_PARAMETER ?
                                 entry.parameter_count : 0;
    if (expected_role_count == 0 || role_count >= expected_role_count)
        return DSL_FHE_ENTRY_VALUE_INVALID_ID;

    DSL_FHE_ENTRY_VALUE_ID next_id = DSL_FHE_Entry_Value_Count() + 1;
    if (entry.entry_value_count != 0 &&
        next_id != entry.first_entry_value_id + entry.entry_value_count)
        return DSL_FHE_ENTRY_VALUE_INVALID_ID;

    DSL_FHE_ENTRY_VALUE_RECORD record;
    DSL_FHE_Entry_Value_Record_Init(&record);
    record.entry_contract_id = entry_contract_id;
    record.value_id = value_record->image_value_id;
    record.ordinal = ordinal;
    record.role = role;
    record.value_class = info->value_class;
    record.encryption_descriptor_id = info->encryption_descriptor_id;
    record.flags = info->flags;
    DSL_FHE_ENTRY_VALUE_ID id = DSL_FHE_Add_Entry_Value(&record);
    DSL_FHE_ENTRY_VALUE_ID first_id = entry.entry_value_count == 0 ?
                                      id : entry.first_entry_value_id;
    if (id == DSL_FHE_ENTRY_VALUE_INVALID_ID ||
        !DSL_FHE_Set_Entry_Value_Range
             (entry_contract_id, first_id, entry.entry_value_count + 1))
        return DSL_FHE_ENTRY_VALUE_INVALID_ID;
    return id;
}

BOOL
DSL_Builder_Get_FHE_Value_Encryption_Descriptor
        (DSL_BUILDER_VALUE value,
         DSL_FHE_ENCRYPTION_DESCRIPTOR_RECORD *record)
{
    DSL_BUILDER_VALUE_RECORD *value_record =
        DSL_Builder_Find_Value_Record(value);
    if (value_record == NULL)
        return FALSE;
    for (UINT32 i = 1; i <= DSL_FHE_Entry_Value_Count(); ++i) {
        DSL_FHE_ENTRY_VALUE_RECORD entry_value;
        if (!DSL_FHE_Get_Entry_Value(i, &entry_value))
            return FALSE;
        if (entry_value.value_id == value_record->image_value_id)
            return DSL_FHE_Get_Encryption_Descriptor
                       (entry_value.encryption_descriptor_id, record);
    }
    return FALSE;
}

DSL_FHE_CONVERSION_DISPOSITION_ID
DSL_Builder_Record_FHE_Conversion_Disposition
        (DSL_BUILDER_VALUE source_value,
         const DSL_FHE_CONVERSION_DISPOSITION_INFO *info)
{
    DSL_BUILDER_VALUE_RECORD *source =
        DSL_Builder_Find_Value_Record(source_value);
    DSL_IR_VALUE_RECORD image_value;
    DSL_FHE_CONVERSION_DISPOSITION_RECORD record;

    if (source == NULL || source->pu == NULL || info == NULL ||
        !DSL_IR_Image_Get_Value(source->image_value_id, &image_value) ||
        image_value.producer_node_id == DSL_IR_NODE_INVALID_ID)
        return DSL_FHE_CONVERSION_DISPOSITION_INVALID_ID;

    DSL_FHE_Conversion_Disposition_Record_Init(&record);
    record.source_node_id = image_value.producer_node_id;
    record.result_value_id = image_value.id;
    record.disposition = info->disposition;
    record.owner_pu_st = PU_Info_proc_sym(source->pu);
    record.wrapper_version = info->wrapper_version;
    if (info->wrapper_name != NULL && info->wrapper_name[0] != '\0')
        record.wrapper_name = Save_Str(info->wrapper_name);
    record.approximation_contract_id = info->approximation_contract_id;
    record.result_ckks_value_state_id = info->result_ckks_value_state_id;
    record.first_bn_fold_id = info->first_bn_fold_id;
    record.bn_fold_count = info->bn_fold_count;
    record.flags = info->flags;
    return DSL_FHE_Plan_Add_Conversion_Disposition(&record);
}

DSL_FHE_CKKS_VALUE_STATE_ID
DSL_Builder_Bind_FHE_Value_CKKS_State
        (DSL_BUILDER_VALUE value,
         const DSL_FHE_CKKS_VALUE_STATE_INFO *info)
{
    DSL_BUILDER_VALUE_RECORD *source = DSL_Builder_Find_Value_Record(value);
    DSL_FHE_CKKS_VALUE_STATE_RECORD record;

    if (source == NULL || info == NULL)
        return DSL_FHE_CKKS_VALUE_STATE_INVALID_ID;

    DSL_FHE_CKKS_Value_State_Record_Init(&record);
    record.value_id = source->image_value_id;
    record.encryption_descriptor_id = info->encryption_descriptor_id;
    record.state_version = info->state_version;
    record.scheme = info->scheme;
    record.value_class = info->value_class;
    record.level = info->level;
    record.scale_bits = info->scale_bits;
    record.component_count = info->component_count;
    record.precision_bits = info->precision_bits;
    record.slot_count = info->slot_count;
    record.alignment_group = info->alignment_group;
    if (info->encrypted_layout_name != NULL &&
        info->encrypted_layout_name[0] != '\0')
        record.encrypted_layout_name =
            Save_Str(info->encrypted_layout_name);
    record.pending_actions = info->pending_actions;
    record.pending_bootstrap_reason = info->pending_bootstrap_reason;
    return DSL_FHE_Plan_Add_CKKS_Value_State(&record);
}

DSL_FHE_BN_FOLD_PROVENANCE_ID
DSL_Builder_Record_FHE_BN_Fold
        (DSL_BUILDER_VALUE conv_value,
         DSL_BUILDER_VALUE batch_norm_value,
         const DSL_FHE_BN_FOLD_INFO *info)
{
    DSL_BUILDER_VALUE_RECORD *conv =
        DSL_Builder_Find_Value_Record(conv_value);
    DSL_BUILDER_VALUE_RECORD *batch_norm =
        DSL_Builder_Find_Value_Record(batch_norm_value);
    DSL_BUILDER_VALUE inputs[] = {
        info == NULL ? NULL : info->source_conv_weight,
        info == NULL ? NULL : info->source_conv_bias,
        info == NULL ? NULL : info->source_bn_scale,
        info == NULL ? NULL : info->source_bn_bias,
        info == NULL ? NULL : info->source_bn_mean,
        info == NULL ? NULL : info->source_bn_variance
    };
    DSL_IR_VALUE_ID input_ids[6];
    DSL_IR_VALUE_RECORD conv_image;
    DSL_IR_VALUE_RECORD batch_norm_image;
    DSL_FHE_BN_FOLD_PROVENANCE_RECORD record;

    if (conv == NULL || batch_norm == NULL || info == NULL ||
        conv->pu == NULL || conv->pu != batch_norm->pu ||
        !DSL_IR_Image_Get_Value(conv->image_value_id, &conv_image) ||
        !DSL_IR_Image_Get_Value(batch_norm->image_value_id,
                                &batch_norm_image))
        return DSL_FHE_BN_FOLD_PROVENANCE_INVALID_ID;

    for (UINT32 i = 0; i < 6; ++i) {
        DSL_BUILDER_VALUE_RECORD *input;
        if (i == 1 && inputs[i] == NULL &&
            (info->flags & DSL_FHE_BN_FOLD_IMPLICIT_ZERO_BIAS) != 0) {
            input_ids[i] = DSL_IR_VALUE_INVALID_ID;
            continue;
        }
        input = DSL_Builder_Find_Value_Record(inputs[i]);
        if (input == NULL || input->pu != conv->pu)
            return DSL_FHE_BN_FOLD_PROVENANCE_INVALID_ID;
        input_ids[i] = input->image_value_id;
    }

    DSL_FHE_BN_Fold_Provenance_Record_Init(&record);
    record.owner_pu_st = PU_Info_proc_sym(conv->pu);
    record.conv_node_id = conv_image.producer_node_id;
    record.batch_norm_node_id = batch_norm_image.producer_node_id;
    record.context_pu_identity_id = info->context_pu_identity_id;
    record.context_callsite_id = info->context_callsite_id;
    record.source_conv_weight_value_id = input_ids[0];
    record.source_conv_bias_value_id = input_ids[1];
    record.source_bn_scale_value_id = input_ids[2];
    record.source_bn_bias_value_id = input_ids[3];
    record.source_bn_mean_value_id = input_ids[4];
    record.source_bn_variance_value_id = input_ids[5];
    record.folded_weight_tcon = info->folded_weight_tcon;
    record.folded_bias_tcon = info->folded_bias_tcon;
    record.flags = info->flags;
    return DSL_FHE_Plan_Add_BN_Fold_Provenance(&record);
}

BOOL
DSL_Builder_Begin_Program (void)
{
    DSL_Builder_Reset_Program();
    Current_DST = New_DST();
    DST_Init(NULL, 0);
    return TRUE;
}

void
DSL_Builder_Abort_Program (void)
{
    DSL_Builder_Reset_Program();
}

DSL_BUILDER_PROGRAM_UNIT
DSL_Builder_Create_Minimal_PU (const char *name)
{
    TY_IDX function_ty;
    PU_IDX pu_idx;
    PU *pu;
    ST *func_st;
    WN *func_body;
    WN *entry_wn;
    PU_Info *pu_info;
    DST_INFO_IDX func_dst;
    USRCPOS source_position;
    char source_name[256];

    if (name == NULL || name[0] == '\0')
        return NULL;
    if (DSL_Builder_PU_Root != NULL) {
        for (PU_Info *current = DSL_Builder_PU_Root; current != NULL;
             current = PU_Info_next(current)) {
            ST_IDX current_st = PU_Info_proc_sym(current);
            if (strcmp(ST_name(St_Table[current_st]), name) == 0) {
                if (!DSL_Builder_Select_PU(current))
                    return NULL;
                return current;
            }
        }
    } else if (!DSL_builder_value_registry.empty() ||
               DSL_IR_Image_Has_Records()) {
        DSL_Builder_Reset_Program();
    }

    function_ty = Make_Function_Type(MTYPE_To_TY(MTYPE_V));
    pu = &New_PU(pu_idx);
    PU_Init(*pu, function_ty, GLOBAL_SYMTAB + 1);
    Set_PU_c_lang(*pu);

    func_st = New_ST(GLOBAL_SYMTAB);
    ST_Init(func_st, Save_Str(name), CLASS_FUNC, SCLASS_TEXT,
            EXPORT_LOCAL, function_ty);
    Set_ST_pu(func_st, pu_idx);

    func_body = WN_CreateBlock();
    entry_wn = WN_CreateEntry(0, func_st, func_body, NULL, NULL);

    if (Current_DST == NULL)
        DST_Init(NULL, 0);
    if (DST_IS_NULL(DSL_Builder_CU_DST)) {
        snprintf(source_name, sizeof(source_name), "%s.dsl", name);
        DSL_Builder_CU_DST = DST_mk_compile_unit
            (source_name, (char *)".", (char *)"open64-dsl-builder",
             DW_LANG_C89, DW_ID_case_sensitive);
    }
    USRCPOS_clear(source_position);
    func_dst = DST_mk_subprogram
                   (source_position, (char *)ST_name(func_st),
                    DST_INVALID_IDX, DST_INVALID_IDX, ST_st_idx(func_st),
                    DW_INL_not_inlined, DW_VIRTUALITY_none, 0, false, true,
                    true, true);

    Current_pu = pu;
    Current_scope = pu->lexical_level;
    New_Scope(Current_scope, Malloc_Mem_Pool, TRUE);
    Scope_tab[Current_scope].st = func_st;

    pu_info = TYPE_MEM_POOL_ALLOC(PU_Info, Malloc_Mem_Pool);
    PU_Info_init(pu_info);

    Set_PU_Info_symtab_ptr(pu_info, NULL);
    Set_PU_Info_tree_ptr(pu_info, entry_wn);
    Current_Map_Tab = WN_MAP_TAB_Create(Malloc_Mem_Pool);
    PU_Info_maptab(pu_info) = Current_Map_Tab;
    PU_Info_proc_sym(pu_info) = ST_st_idx(func_st);
    Set_PU_Info_pu_dst(pu_info, func_dst);
    Set_PU_Info_cu_dst(pu_info, DSL_Builder_CU_DST);
    Set_PU_Info_state(pu_info, WT_SYMTAB, Subsect_InMem);
    Set_PU_Info_state(pu_info, WT_TREE, Subsect_InMem);
    Set_PU_Info_state(pu_info, WT_PROC_SYM, Subsect_InMem);
    Set_PU_Info_state(pu_info, WT_DEPGRAPH, Subsect_Missing);
    Set_PU_Info_state(pu_info, WT_PREFETCH, Subsect_Missing);
    Set_PU_Info_state(pu_info, WT_REGIONS, Subsect_Missing);
    Set_PU_Info_state(pu_info, WT_FEEDBACK, Subsect_Missing);
    Set_PU_Info_state(pu_info, WT_FREQ, Subsect_Missing);
    Set_PU_Info_state(pu_info, WT_AC_INTERNAL, Subsect_Missing);
    Set_PU_Info_state(pu_info, WT_ALIAS_CLASS, Subsect_Missing);
    Set_PU_Info_state(pu_info, WT_SSA, Subsect_Missing);
    Set_PU_Info_state(pu_info, WT_ALIAS_CGNODE, Subsect_Missing);

    if (DSL_Builder_PU_Root != NULL)
        PU_Info_next(DSL_Builder_PU_Last) = pu_info;
    else
        DSL_Builder_PU_Root = pu_info;
    DSL_Builder_PU_Last = pu_info;

    Save_Local_Symtab(Current_scope, pu_info);
    DSL_Builder_Active_PU = pu_info;
    Current_PU_Info = pu_info;

    dsl_builder_pu_interface *interface_record =
        new dsl_builder_pu_interface;
    interface_record->pu = pu_info;
    interface_record->has_return = FALSE;
    DSL_builder_pu_interfaces.push_back(interface_record);

    return pu_info;
}

DSL_BUILDER_VALUE
DSL_Builder_Declare_PU_Formal
        (DSL_BUILDER_PROGRAM_UNIT pu,
         const char *name,
         UINT32 ordinal,
         TY_IDX ty,
         const DSL_BUILDER_SOURCE_POSITION *source_position)
{
    dsl_builder_pu_interface *interface_record =
        DSL_Builder_Find_PU_Interface(pu);
    if (interface_record == NULL || !DSL_Builder_Select_PU(pu) ||
        name == NULL || name[0] == '\0' ||
        ordinal != interface_record->formals.size() ||
        !interface_record->results.empty() || !TY_is_tensor_extension(ty) ||
        !DSL_Builder_Source_Position_Valid(source_position))
        return NULL;

    ST *st = New_ST();
    ST_Init(st, Save_Str(name), CLASS_VAR, SCLASS_FORMAL, EXPORT_LOCAL, ty);
    Set_ST_is_value_parm(st);
    Set_ST_Srcpos(*st, DSL_Builder_Source_Position(source_position));
    WN *value = WN_CreateLdid(OPR_LDID, MTYPE_M, MTYPE_M, 0,
                              ST_st_idx(*st), ty);

    DSL_IR_VALUE_ID image_value_id = DSL_Builder_Add_Image_Symbol_Value
                                         (ST_st_idx(*st), ty,
                                          DSL_IR_VALUE_SYMBOL);
    if (image_value_id == DSL_IR_VALUE_INVALID_ID)
        return NULL;

    DSL_BUILDER_VALUE_RECORD value_record;
    value_record.pu = pu;
    value_record.assignment = value;
    value_record.expression = value;
    value_record.result_st = ST_st_idx(*st);
    value_record.result_ty = ty;
    value_record.image_value_id = image_value_id;
    value_record.value_kind = DSL_IR_VALUE_SYMBOL;
    value_record.tensor_tcon = TCON_IDX_ZERO;
    value_record.canonical_key = DSL_Builder_Value_Canonical_Key
                                     (OPR_DSLUNKNOWN, 0, ty, NULL, 0,
                                      NULL, 0, name);
    value_record.materializing = FALSE;
    value_record.materialized = TRUE;
    DSL_builder_value_registry.push_back(value_record);

    dsl_builder_pu_interface_value formal;
    formal.value = value;
    formal.st = ST_st_idx(*st);
    formal.ty = ty;
    formal.ordinal = ordinal;
    formal.role = DSL_PU_RESULT_INVALID;
    interface_record->formals.push_back(formal);
    if (!DSL_Builder_Rebuild_PU_Entry(interface_record))
        return NULL;
    return value;
}

DSL_BUILDER_VALUE
DSL_Builder_Declare_PU_Result
        (DSL_BUILDER_PROGRAM_UNIT pu,
         const char *name,
         UINT32 ordinal,
         TY_IDX ty,
         DSL_BUILDER_PU_RESULT_ROLE role,
         const DSL_BUILDER_SOURCE_POSITION *source_position)
{
    dsl_builder_pu_interface *interface_record =
        DSL_Builder_Find_PU_Interface(pu);
    if (interface_record == NULL || !DSL_Builder_Select_PU(pu) ||
        name == NULL || name[0] == '\0' ||
        ordinal != interface_record->results.size() ||
        !TY_is_tensor_extension(ty) ||
        (role != DSL_PU_RESULT_TENSOR && role != DSL_PU_RESULT_STATE) ||
        !DSL_Builder_Source_Position_Valid(source_position))
        return NULL;

    ST *st = New_ST();
    ST_Init(st, Save_Str(name), CLASS_VAR, SCLASS_FORMAL_REF,
            EXPORT_LOCAL, ty);
    Set_ST_Srcpos(*st, DSL_Builder_Source_Position(source_position));
    ST_tensor_bind_metadata(ST_st_idx(*st), "dsl.pu.result_role",
                            role == DSL_PU_RESULT_STATE ? "state" : "tensor");
    char ordinal_text[32];
    snprintf(ordinal_text, sizeof(ordinal_text), "%u", ordinal);
    ST_tensor_bind_metadata(ST_st_idx(*st), "dsl.pu.result_ordinal",
                            ordinal_text);
    WN *value = WN_CreateLdid(OPR_LDID, MTYPE_M, MTYPE_M, 0,
                              ST_st_idx(*st), ty);

    DSL_IR_VALUE_ID image_value_id = DSL_Builder_Add_Image_Symbol_Value
                                         (ST_st_idx(*st), ty,
                                          DSL_IR_VALUE_SYMBOL);
    if (image_value_id == DSL_IR_VALUE_INVALID_ID)
        return NULL;

    DSL_BUILDER_VALUE_RECORD value_record;
    value_record.pu = pu;
    value_record.assignment = value;
    value_record.expression = value;
    value_record.result_st = ST_st_idx(*st);
    value_record.result_ty = ty;
    value_record.image_value_id = image_value_id;
    value_record.value_kind = DSL_IR_VALUE_SYMBOL;
    value_record.tensor_tcon = TCON_IDX_ZERO;
    value_record.canonical_key = DSL_Builder_Value_Canonical_Key
                                     (OPR_DSLUNKNOWN, 0, ty, NULL, 0,
                                      NULL, 0, name);
    value_record.materializing = FALSE;
    value_record.materialized = TRUE;
    DSL_builder_value_registry.push_back(value_record);

    dsl_builder_pu_interface_value result;
    result.value = value;
    result.st = ST_st_idx(*st);
    result.ty = ty;
    result.ordinal = ordinal;
    result.role = role;
    interface_record->results.push_back(result);
    if (!DSL_Builder_Rebuild_PU_Entry(interface_record))
        return NULL;
    return value;
}

BOOL
DSL_Builder_Return_PU_Values
        (DSL_BUILDER_PROGRAM_UNIT pu,
         DSL_BUILDER_VALUE *values,
         UINT32 value_count)
{
    dsl_builder_pu_interface *interface_record =
        DSL_Builder_Find_PU_Interface(pu);
    WN *body = DSL_Builder_PU_Body(pu);
    if (interface_record == NULL || !DSL_Builder_Select_PU(pu) ||
        body == NULL || interface_record->has_return ||
        value_count != interface_record->results.size() ||
        (value_count != 0 && values == NULL))
        return FALSE;

    for (UINT32 i = 0; i < value_count; ++i) {
        DSL_BUILDER_VALUE_RECORD *value_record =
            DSL_Builder_Find_Value_Record(values[i]);
        if (value_record == NULL || value_record->pu != pu ||
            value_record->result_ty != interface_record->results[i].ty)
            return FALSE;
    }
    for (UINT32 i = 0; i < value_count; ++i) {
        DSL_BUILDER_VALUE_RECORD *value_record =
            DSL_Builder_Find_Value_Record(values[i]);
        if (!DSL_Builder_Materialize_PU_Value(pu, value_record))
            return FALSE;
    }
    for (UINT32 i = 0; i < value_count; ++i) {
        DSL_BUILDER_VALUE_RECORD *value_record =
            DSL_Builder_Find_Value_Record(values[i]);
        WN *load = WN_CreateLdid(OPR_LDID, MTYPE_M, MTYPE_M, 0,
                                 value_record->result_st,
                                 value_record->result_ty);
        WN *store = WN_CreateStid
                        (OPR_STID, MTYPE_V, MTYPE_M, 0,
                         interface_record->results[i].st,
                         interface_record->results[i].ty, load);
        WN_INSERT_BlockLast(body, store);
    }
    WN_INSERT_BlockLast(body, WN_CreateReturn());
    interface_record->has_return = TRUE;
    return TRUE;
}

DSL_BUILDER_CALL
DSL_Builder_Create_PU_Call
        (DSL_BUILDER_PROGRAM_UNIT caller,
         DSL_BUILDER_PROGRAM_UNIT callee,
         DSL_BUILDER_VALUE *arguments,
         UINT32 argument_count,
         const char *const *result_names,
         UINT32 result_count,
         const DSL_BUILDER_CALLSITE_INFO *callsite)
{
    dsl_builder_pu_interface *callee_interface =
        DSL_Builder_Find_PU_Interface(callee);
    WN *body = DSL_Builder_PU_Body(caller);
    if (caller == callee || callee_interface == NULL ||
        !callee_interface->has_return || !DSL_Builder_Select_PU(caller) ||
        body == NULL || argument_count != callee_interface->formals.size() ||
        result_count != callee_interface->results.size() ||
        (argument_count != 0 && arguments == NULL) ||
        (result_count != 0 && result_names == NULL) || callsite == NULL ||
        !DSL_Builder_Source_Position_Valid(&callsite->source_position))
        return NULL;

    std::vector<DSL_BUILDER_VALUE_RECORD *> argument_records;
    for (UINT32 i = 0; i < argument_count; ++i) {
        DSL_BUILDER_VALUE_RECORD *record =
            DSL_Builder_Find_Value_Record(arguments[i]);
        if (record == NULL || record->pu != caller ||
            record->result_ty != callee_interface->formals[i].ty)
            return NULL;
        argument_records.push_back(record);
    }
    for (UINT32 i = 0; i < result_count; ++i) {
        if (result_names[i] == NULL || result_names[i][0] == '\0')
            return NULL;
    }
    for (UINT32 i = 0; i < argument_count; ++i) {
        if (!DSL_Builder_Materialize_PU_Value(caller,
                                              argument_records[i]))
            return NULL;
    }

    WN *call = WN_Create(OPR_CALL, MTYPE_V, MTYPE_V,
                         argument_count + result_count);
    WN_st_idx(call) = PU_Info_proc_sym(callee);
    WN_Set_Call_Default_Flags(call);
    WN_Set_Linenum(call,
                   DSL_Builder_Source_Position(&callsite->source_position));

    for (UINT32 i = 0; i < argument_count; ++i) {
        TY_IDX pointer_ty = Make_Pointer_Type(argument_records[i]->result_ty);
        WN *address = WN_CreateLda
                          (OPR_LDA, Pointer_Mtype, MTYPE_V, 0, pointer_ty,
                           argument_records[i]->result_st);
        WN_kid(call, i) = WN_CreateParm
                              (Pointer_Mtype, address, pointer_ty,
                               WN_PARM_BY_REFERENCE | WN_PARM_READ_ONLY |
                               WN_PARM_PASSED_NOT_SAVED);
    }

    dsl_builder_call_record *call_record = new dsl_builder_call_record;
    call_record->call = call;
    call_record->caller = caller;
    call_record->callee = callee;
    call_record->canonical_class_name = DSL_Builder_Safe_String
                                            (callsite->canonical_class_name);
    call_record->instance_path = DSL_Builder_Safe_String
                                     (callsite->instance_path);
    call_record->context_identity = DSL_Builder_Safe_String
                                        (callsite->context_identity);
    call_record->call_ordinal = callsite->call_ordinal;
    call_record->source_position = callsite->source_position;
    for (UINT32 i = 0; i < argument_count; ++i)
        call_record->arguments.push_back(arguments[i]);
    for (UINT32 i = 0; i < result_count; ++i) {
        TY_IDX result_ty = callee_interface->results[i].ty;
        ST_IDX result_st = DSL_Builder_Create_Tensor_Result_Symbol
                               (result_names[i], result_ty, SCLASS_AUTO,
                                EXPORT_LOCAL);
        if (ST_IDX_index(result_st) == 0) {
            delete call_record;
            return NULL;
        }
        Set_ST_Srcpos(St_Table[result_st],
                      DSL_Builder_Source_Position
                          (&callsite->source_position));
        ST_tensor_bind_metadata(result_st, "dsl.call.canonical_class",
                                DSL_Builder_Safe_String
                                    (callsite->canonical_class_name));
        ST_tensor_bind_metadata(result_st, "dsl.call.instance_path",
                                DSL_Builder_Safe_String
                                    (callsite->instance_path));
        ST_tensor_bind_metadata(result_st, "dsl.call.context_identity",
                                DSL_Builder_Safe_String
                                    (callsite->context_identity));
        char ordinal_text[32];
        snprintf(ordinal_text, sizeof(ordinal_text), "%u",
                 callsite->call_ordinal);
        ST_tensor_bind_metadata(result_st, "dsl.call.ordinal", ordinal_text);

        TY_IDX pointer_ty = Make_Pointer_Type(result_ty);
        WN *address = WN_CreateLda(OPR_LDA, Pointer_Mtype, MTYPE_V, 0,
                                   pointer_ty, result_st);
        WN_kid(call, argument_count + i) = WN_CreateParm
                                               (Pointer_Mtype, address,
                                                pointer_ty,
                                                WN_PARM_BY_REFERENCE |
                                                WN_PARM_OUT |
                                                WN_PARM_PASSED_NOT_SAVED);

        WN *value = WN_CreateLdid(OPR_LDID, MTYPE_M, MTYPE_M, 0,
                                  result_st, result_ty);
        DSL_IR_VALUE_ID image_value_id =
            DSL_Builder_Add_Image_Symbol_Value
                (result_st, result_ty, DSL_IR_VALUE_SYMBOL);
        if (image_value_id == DSL_IR_VALUE_INVALID_ID) {
            delete call_record;
            return NULL;
        }
        DSL_BUILDER_VALUE_RECORD value_record;
        value_record.pu = caller;
        value_record.assignment = value;
        value_record.expression = value;
        value_record.result_st = result_st;
        value_record.result_ty = result_ty;
        value_record.image_value_id = image_value_id;
        value_record.value_kind = DSL_IR_VALUE_SYMBOL;
        value_record.tensor_tcon = TCON_IDX_ZERO;
        value_record.canonical_key = DSL_Builder_Value_Canonical_Key
                                         (OPR_DSLUNKNOWN, 0, result_ty,
                                          NULL, 0, NULL, 0,
                                          ST_name(St_Table[result_st]));
        value_record.materializing = FALSE;
        value_record.materialized = TRUE;
        DSL_builder_value_registry.push_back(value_record);
        call_record->results.push_back(value);
    }

    DSL_CALLSITE_METADATA_RECORD image_callsite;
    memset(&image_callsite, 0, sizeof(image_callsite));
    image_callsite.owner_pu_st = PU_Info_proc_sym(caller);
    image_callsite.callee_pu_st = PU_Info_proc_sym(callee);
    image_callsite.canonical_class_name = Save_Str
        (DSL_Builder_Safe_String(callsite->canonical_class_name));
    image_callsite.instance_path = Save_Str
        (DSL_Builder_Safe_String(callsite->instance_path));
    image_callsite.context_identity = Save_Str
        (DSL_Builder_Safe_String(callsite->context_identity));
    image_callsite.source_call_ordinal = callsite->call_ordinal;
    if (DSL_Call_Image_Add_Callsite
            (PU_Info_proc_sym(caller), call, &image_callsite) ==
        DSL_CALLSITE_METADATA_INVALID_ID) {
        delete call_record;
        return NULL;
    }

    std::string comment_text = "__WHIRL_DSL_CALL__:callee=";
    comment_text += ST_name(St_Table[PU_Info_proc_sym(callee)]);
    comment_text += ";class=";
    comment_text += DSL_Builder_Safe_String(callsite->canonical_class_name);
    comment_text += ";instance=";
    comment_text += DSL_Builder_Safe_String(callsite->instance_path);
    comment_text += ";context=";
    comment_text += DSL_Builder_Safe_String(callsite->context_identity);
    char ordinal_text[32];
    snprintf(ordinal_text, sizeof(ordinal_text), "%u",
             callsite->call_ordinal);
    comment_text += ";ordinal=";
    comment_text += ordinal_text;
    WN *comment = WN_CreateComment(comment_text.c_str());
    WN_Set_Linenum(comment,
                   DSL_Builder_Source_Position(&callsite->source_position));
    WN_INSERT_BlockLast(body, comment);
    WN_INSERT_BlockLast(body, call);
    DSL_builder_call_registry.push_back(call_record);
    return call;
}

static BOOL
DSL_Builder_Call_Argument_Role_Valid (const char *role)
{
    if (role == NULL || role[0] == '\0')
        return FALSE;
    BOOL component_start = TRUE;
    for (const char *cursor = role; *cursor != '\0'; ++cursor) {
        if (*cursor == '.') {
            if (component_start)
                return FALSE;
            component_start = TRUE;
            continue;
        }
        if (component_start) {
            if (*cursor < 'a' || *cursor > 'z')
                return FALSE;
            component_start = FALSE;
        } else if ((*cursor < 'a' || *cursor > 'z') &&
                   (*cursor < '0' || *cursor > '9') && *cursor != '_') {
            return FALSE;
        }
    }
    return !component_start;
}

BOOL
DSL_Builder_Set_PU_Call_Argument_Role
        (DSL_BUILDER_CALL call, UINT32 actual_ordinal,
         UINT32 callee_formal_ordinal, const char *semantic_role)
{
    dsl_builder_call_record *call_record =
        DSL_Builder_Find_Call_Record(call);
    dsl_builder_pu_interface *callee_interface = call_record == NULL ? NULL :
        DSL_Builder_Find_PU_Interface(call_record->callee);
    if (call_record == NULL || callee_interface == NULL ||
        !DSL_Builder_Call_Argument_Role_Valid(semantic_role) ||
        actual_ordinal >= call_record->arguments.size() ||
        callee_formal_ordinal >= callee_interface->formals.size())
        return FALSE;

    DSL_BUILDER_VALUE_RECORD *argument = DSL_Builder_Find_Value_Record
        (call_record->arguments[actual_ordinal]);
    if (argument == NULL || argument->pu != call_record->caller ||
        argument->result_ty !=
            callee_interface->formals[callee_formal_ordinal].ty)
        return FALSE;

    DSL_CALLSITE_METADATA_RECORD callsite;
    if (!DSL_Call_Image_Find_Callsite(call, &callsite))
        return FALSE;
    DSL_CALL_ARGUMENT_RECORD record;
    memset(&record, 0, sizeof(record));
    record.callsite_id = callsite.id;
    record.argument_value_id = argument->image_value_id;
    record.actual_ordinal = actual_ordinal;
    record.callee_formal_ordinal = callee_formal_ordinal;
    record.semantic_role = Save_Str(semantic_role);
    return DSL_Call_ABI_Image_Add_Argument(call, &record) !=
           DSL_CALL_ARGUMENT_INVALID_ID;
}

BOOL
DSL_Builder_Get_PU_Call_Result
        (DSL_BUILDER_CALL call,
         UINT32 ordinal,
         DSL_BUILDER_VALUE *value)
{
    dsl_builder_call_record *record = DSL_Builder_Find_Call_Record(call);
    if (record == NULL || value == NULL || ordinal >= record->results.size())
        return FALSE;
    *value = record->results[ordinal];
    return TRUE;
}

BOOL
DSL_Builder_Get_PU_Callsite_Info
        (DSL_BUILDER_CALL call,
         DSL_BUILDER_CALLSITE_INFO *callsite)
{
    if (call == NULL || callsite == NULL || WN_operator(call) != OPR_CALL)
        return FALSE;

    dsl_builder_call_record *record = DSL_Builder_Find_Call_Record(call);
    DSL_CALLSITE_METADATA_RECORD image_record;
    if (DSL_Call_Image_Find_Callsite(call, &image_record)) {
        callsite->canonical_class_name =
            Index_To_Str(image_record.canonical_class_name);
        callsite->instance_path = Index_To_Str(image_record.instance_path);
        callsite->context_identity = Index_To_Str(image_record.context_identity);
        callsite->call_ordinal = image_record.source_call_ordinal;
        SRCPOS source_position = WN_Get_Linenum(call);
        callsite->source_position.file_id = SRCPOS_filenum(source_position);
        callsite->source_position.line = SRCPOS_linenum(source_position);
        callsite->source_position.column = SRCPOS_column(source_position);
        callsite->source_position.statement_begin =
            SRCPOS_stmt_begin(source_position);
        callsite->source_position.basic_block_begin =
            SRCPOS_bb_begin(source_position);
        return TRUE;
    }
    if (record != NULL) {
        callsite->canonical_class_name =
            record->canonical_class_name.c_str();
        callsite->instance_path = record->instance_path.c_str();
        callsite->context_identity = record->context_identity.c_str();
        callsite->call_ordinal = record->call_ordinal;
        callsite->source_position = record->source_position;
        return TRUE;
    }

    for (UINT32 i = 0; i < WN_kid_count(call); ++i) {
        WN *parm = WN_kid(call, i);
        if (parm == NULL || WN_operator(parm) != OPR_PARM ||
            !WN_Parm_Out(parm))
            continue;
        WN *address = WN_kid0(parm);
        if (address == NULL || WN_operator(address) != OPR_LDA)
            continue;
        ST_IDX result_st = WN_st_idx(address);
        if (ST_IDX_index(result_st) == 0)
            continue;
        const char *canonical_class =
            ST_tensor_metadata(result_st, "dsl.call.canonical_class");
        const char *instance_path =
            ST_tensor_metadata(result_st, "dsl.call.instance_path");
        const char *context_identity =
            ST_tensor_metadata(result_st, "dsl.call.context_identity");
        const char *ordinal =
            ST_tensor_metadata(result_st, "dsl.call.ordinal");
        if (canonical_class == NULL || instance_path == NULL ||
            context_identity == NULL || ordinal == NULL || ordinal[0] == '\0')
            continue;

        char *ordinal_end = NULL;
        unsigned long parsed_ordinal = strtoul(ordinal, &ordinal_end, 10);
        if (ordinal_end == NULL || ordinal_end[0] != '\0' ||
            parsed_ordinal > (unsigned long)~(UINT32)0)
            continue;

        callsite->canonical_class_name = canonical_class;
        callsite->instance_path = instance_path;
        callsite->context_identity = context_identity;
        callsite->call_ordinal = (UINT32)parsed_ordinal;
        SRCPOS source_position = WN_Get_Linenum(call);
        callsite->source_position.file_id = SRCPOS_filenum(source_position);
        callsite->source_position.line = SRCPOS_linenum(source_position);
        callsite->source_position.column = SRCPOS_column(source_position);
        callsite->source_position.statement_begin =
            SRCPOS_stmt_begin(source_position);
        callsite->source_position.basic_block_begin =
            SRCPOS_bb_begin(source_position);
        return TRUE;
    }
    return FALSE;
}

UINT32
DSL_Builder_Register_Source_File
        (DSL_BUILDER_PROGRAM_UNIT pu,
         const char *path)
{
    std::string full_path;
    std::string directory;
    std::string file_name;
    std::string::size_type separator;
    UINT32 directory_id = 0;

    if (DSL_Builder_PU_Body(pu) == NULL || path == NULL || path[0] == '\0')
        return 0;

    full_path = path;
    for (UINT32 i = 0; i < DSL_builder_source_files.size(); ++i) {
        if (DSL_builder_source_files[i] == full_path)
            return i + 1;
    }
    if (DSL_builder_source_files.size() >= 65535)
        return 0;

    separator = full_path.rfind('/');
    if (separator == std::string::npos) {
        directory = ".";
        file_name = full_path;
    } else {
        directory = separator == 0 ? "/" : full_path.substr(0, separator);
        file_name = full_path.substr(separator + 1);
    }
    if (file_name.empty())
        return 0;

    for (UINT32 i = 0; i < DSL_builder_source_directories.size(); ++i) {
        if (DSL_builder_source_directories[i] == directory) {
            directory_id = i + 1;
            break;
        }
    }
    if (directory_id == 0) {
        if (DSL_builder_source_directories.size() >= 65535)
            return 0;
        DST_mk_include_dir((char *)directory.c_str());
        DSL_builder_source_directories.push_back(directory);
        directory_id = DSL_builder_source_directories.size();
    }

    DST_mk_file_name((char *)file_name.c_str(), directory_id, 0, 0);
    DSL_builder_source_files.push_back(full_path);
    return DSL_builder_source_files.size();
}

BOOL
DSL_Builder_Set_Value_Source_Position
        (DSL_BUILDER_VALUE value,
         const DSL_BUILDER_SOURCE_POSITION *source_position)
{
    DSL_BUILDER_VALUE_RECORD *record = DSL_Builder_Find_Value_Record(value);
    USRCPOS position;

    if (!DSL_Builder_Select_Value_PU(record) ||
        source_position == NULL ||
        source_position->file_id == 0 ||
        source_position->file_id > DSL_builder_source_files.size() ||
        source_position->line < 0 || source_position->column > 4095)
        return FALSE;

    USRCPOS_clear(position);
    USRCPOS_filenum(position) = source_position->file_id;
    USRCPOS_linenum(position) = source_position->line;
    USRCPOS_column(position) = source_position->column;
    USRCPOS_stmt_begin(position) = source_position->statement_begin != 0;
    USRCPOS_bb_begin(position) = source_position->basic_block_begin != 0;
    WN_Set_Linenum(record->assignment, USRCPOS_srcpos(position));
    if (ST_IDX_index(record->result_st) != 0)
        Set_ST_Srcpos(St_Table[record->result_st],
                      USRCPOS_srcpos(position));
    return TRUE;
}

static BOOL
DSL_Builder_Verify_PU_Interfaces (FILE *diagnostic, UINT32 *error_count)
{
    BOOL valid = TRUE;
    for (UINT32 i = 0; i < DSL_builder_pu_interfaces.size(); ++i) {
        dsl_builder_pu_interface *interface_record =
            DSL_builder_pu_interfaces[i];
        if (!DSL_Builder_Select_PU(interface_record->pu)) {
            valid = FALSE;
        } else {
            WN *entry = PU_Info_tree_ptr(interface_record->pu);
            UINT32 expected = interface_record->formals.size() +
                              interface_record->results.size();
            if (entry == NULL || WN_operator(entry) != OPR_FUNC_ENTRY ||
                (UINT32)WN_num_formals(entry) != expected ||
                (!interface_record->results.empty() &&
                 !interface_record->has_return))
                valid = FALSE;
            for (UINT32 j = 0; valid && j < expected; ++j) {
                dsl_builder_pu_interface_value &formal =
                    j < interface_record->formals.size() ?
                    interface_record->formals[j] :
                    interface_record->results
                        [j - interface_record->formals.size()];
                WN *idname = WN_formal(entry, j);
                if (idname == NULL || WN_operator(idname) != OPR_IDNAME ||
                    WN_st_idx(idname) != formal.st)
                    valid = FALSE;
            }
        }
        if (!valid) {
            if (diagnostic != NULL)
                fprintf(diagnostic,
                        "DSL builder verification error: invalid PU interface\n");
            if (error_count != NULL)
                ++*error_count;
            return FALSE;
        }
    }

    for (UINT32 i = 0; i < DSL_builder_call_registry.size(); ++i) {
        dsl_builder_call_record *record = DSL_builder_call_registry[i];
        dsl_builder_pu_interface *callee =
            DSL_Builder_Find_PU_Interface(record->callee);
        WN *call = record->call;
        if (callee == NULL || call == NULL || WN_operator(call) != OPR_CALL ||
            WN_st_idx(call) != PU_Info_proc_sym(record->callee) ||
            (UINT32)WN_kid_count(call) != callee->formals.size() +
                                          callee->results.size() ||
            record->results.size() != callee->results.size())
            valid = FALSE;
        for (UINT32 j = 0; valid && j < callee->formals.size(); ++j) {
            WN *parm = WN_kid(call, j);
            if (parm == NULL || WN_operator(parm) != OPR_PARM ||
                !WN_Parm_By_Reference(parm) ||
                !WN_Parm_Read_Only(parm) || !WN_Parm_Passed_Not_Saved(parm))
                valid = FALSE;
        }
        for (UINT32 j = 0; valid && j < callee->results.size(); ++j) {
            WN *parm = WN_kid(call, callee->formals.size() + j);
            if (parm == NULL || WN_operator(parm) != OPR_PARM ||
                !WN_Parm_By_Reference(parm) || !WN_Parm_Out(parm) ||
                !WN_Parm_Passed_Not_Saved(parm))
                valid = FALSE;
        }
        if (!valid) {
            if (diagnostic != NULL)
                fprintf(diagnostic,
                        "DSL builder verification error: invalid PU call\n");
            if (error_count != NULL)
                ++*error_count;
            return FALSE;
        }
    }
    return TRUE;
}

BOOL
DSL_Builder_Verify_Program (DSL_BUILDER_VERIFY_RESULT *result)
{
    DSL_GATEKEEPER_RESULT gatekeeper_result;
    FILE *diagnostic = NULL;
    char *buffer = result == NULL ? NULL : result->diagnostic;
    UINT32 capacity = result == NULL ? 0 : result->diagnostic_capacity;

    if (buffer != NULL && capacity != 0) {
        buffer[0] = '\0';
        diagnostic = tmpfile();
    }

    if (DSL_Builder_PU_Root == NULL) {
        if (buffer != NULL && capacity != 0)
            snprintf(buffer, capacity,
                     "DSL builder verification error: no program unit");
        if (result != NULL) {
            result->native_node_count = 0;
            result->result_symbol_count = 0;
            result->error_count = 1;
        }
        if (diagnostic != NULL)
            fclose(diagnostic);
        return FALSE;
    }

    memset(&gatekeeper_result, 0, sizeof(gatekeeper_result));
    BOOL valid = TRUE;
    if (!DSL_Builder_Verify_PU_Interfaces
             (diagnostic, &gatekeeper_result.error_count))
        valid = FALSE;
    for (PU_Info *pu = DSL_Builder_PU_Root; pu != NULL;
         pu = PU_Info_next(pu)) {
        DSL_GATEKEEPER_RESULT pu_result;
        memset(&pu_result, 0, sizeof(pu_result));
        if (!DSL_Builder_Select_PU(pu)) {
            if (diagnostic != NULL)
                fprintf(diagnostic,
                        "DSL builder verification error: cannot select PU\n");
            valid = FALSE;
            ++gatekeeper_result.error_count;
            continue;
        }
        if (!DSL_Gatekeeper_Verify_PU(pu, diagnostic, &pu_result))
            valid = FALSE;
        gatekeeper_result.native_node_count += pu_result.native_node_count;
        gatekeeper_result.result_symbol_count += pu_result.result_symbol_count;
        gatekeeper_result.error_count += pu_result.error_count;
        if (!DSL_Region_Verify_PU(pu, diagnostic)) {
            valid = FALSE;
            ++gatekeeper_result.error_count;
        }
        if (!DSL_Call_ABI_Image_Validate_PU(pu, diagnostic)) {
            valid = FALSE;
            ++gatekeeper_result.error_count;
        }
    }
    if (!DSL_FHE_Image_Validate(diagnostic)) {
        valid = FALSE;
        ++gatekeeper_result.error_count;
    }
    if (!DSL_FHE_Plan_Image_Validate(diagnostic)) {
        valid = FALSE;
        ++gatekeeper_result.error_count;
    }
    if (gatekeeper_result.native_node_count !=
            DSL_IR_Image_Executable_Node_Count()) {
        if (diagnostic != NULL)
            fprintf(diagnostic,
                    "native tree node count %u does not match "
                    "DSL image node count %u\n",
                    gatekeeper_result.native_node_count,
                    DSL_IR_Image_Executable_Node_Count());
        valid = FALSE;
        ++gatekeeper_result.error_count;
    }
    if (diagnostic != NULL) {
        rewind(diagnostic);
        size_t count = fread(buffer, 1, capacity - 1, diagnostic);
        buffer[count] = '\0';
        fclose(diagnostic);
    }

    if (result != NULL) {
        result->native_node_count = gatekeeper_result.native_node_count;
        result->result_symbol_count = gatekeeper_result.result_symbol_count;
        result->error_count = gatekeeper_result.error_count;
    }
    return valid;
}

DSL_BUILDER_REGION
DSL_Builder_Create_Region
        (DSL_BUILDER_PROGRAM_UNIT pu,
         DSL_BUILDER_REGION parent,
         const char *contract_name,
         UINT32 contract_version)
{
    if (DSL_Builder_PU_Body(pu) == NULL)
        return NULL;
    return DSL_Region_Create(pu, parent, contract_name, contract_version);
}

BOOL
DSL_Builder_Append_Region_Value
        (DSL_BUILDER_REGION region,
         DSL_BUILDER_VALUE value)
{
    DSL_BUILDER_VALUE_RECORD *record = DSL_Builder_Find_Value_Record(value);
    WN *region_wn = DSL_Region_WN(region);
    return record != NULL && region_wn != NULL &&
           DSL_Builder_Select_PU(record->pu) &&
           DSL_Builder_Materialize_Value_In_Block
               (record->pu, WN_region_body(region_wn), record);
}

BOOL
DSL_Builder_Append_Child_Region
        (DSL_BUILDER_REGION parent,
         DSL_BUILDER_REGION child)
{
    return DSL_Region_Append_Child(parent, child);
}

BOOL
DSL_Builder_Append_PU_Region
        (DSL_BUILDER_PROGRAM_UNIT pu,
         DSL_BUILDER_REGION region)
{
    return DSL_Builder_PU_Body(pu) != NULL &&
           DSL_Region_Append_To_PU(region);
}

BOOL
DSL_Builder_Declare_Region_Value
        (DSL_BUILDER_REGION region,
         DSL_BUILDER_VALUE value,
         UINT32 roles,
         UINT32 ordinal,
         UINT32 flags)
{
    DSL_BUILDER_VALUE_RECORD *record = DSL_Builder_Find_Value_Record(value);
    return record != NULL &&
           DSL_Region_Declare_Symbol
               (region, record->result_st, roles, ordinal, flags);
}

BOOL
DSL_Builder_Set_Region_Metadata
        (DSL_BUILDER_REGION region,
         const char *key,
         const char *value)
{
    return DSL_Region_Set_Metadata(region, key, value);
}

DSL_BUILDER_STATE
DSL_Builder_Declare_State_Object
        (DSL_BUILDER_PROGRAM_UNIT pu,
         const char *name,
         DSL_STATE_KIND kind)
{
    return DSL_Builder_Declare_State_Object_With_Flags
               (pu, name, kind, DSL_STATE_OBJECT_FLAG_NONE);
}

DSL_BUILDER_STATE
DSL_Builder_Declare_State_Object_With_Flags
        (DSL_BUILDER_PROGRAM_UNIT pu,
         const char *name,
         DSL_STATE_KIND kind,
         UINT32 flags)
{
    if (DSL_Builder_PU_Body(pu) == NULL || !DSL_Builder_Select_PU(pu) ||
        name == NULL || name[0] == '\0' ||
        kind < DSL_STATE_KIND_RUNTIME_STATUS ||
        kind > DSL_STATE_KIND_OPAQUE ||
        (flags & ~DSL_STATE_OBJECT_UNIQUE_OWNERSHIP) != 0)
        return NULL;
    for (UINT32 i = 0; i < DSL_builder_state_registry.size(); ++i) {
        dsl_builder_state *state = DSL_builder_state_registry[i];
        if (state->pu == pu &&
            strcmp(ST_name(St_Table[state->st]), name) == 0)
            return NULL;
    }

    ST_IDX st = DSL_Builder_Create_Symbol
                    (name, MTYPE_To_TY(MTYPE_U8), CLASS_VAR,
                     SCLASS_AUTO, EXPORT_LOCAL);
    if (ST_IDX_index(st) == 0)
        return NULL;
    Set_ST_is_temp_var(St_Table[st]);
    Set_ST_addr_passed(St_Table[st]);

    DSL_STATE_OBJECT_RECORD record;
    DSL_State_Object_Record_Init(&record);
    record.kind = kind;
    record.owner_pu_st = PU_Info_proc_sym(pu);
    record.st = st;
    record.name = Save_Str(name);
    record.flags = flags;
    DSL_STATE_OBJECT_ID id = DSL_Effect_Image_Add_State_Object(&record);
    if (id == DSL_STATE_OBJECT_INVALID_ID)
        return NULL;

    dsl_builder_state *state = new dsl_builder_state;
    state->pu = pu;
    state->image_state_id = id;
    state->st = st;
    DSL_builder_state_registry.push_back(state);
    return state;
}

BOOL
DSL_Builder_Declare_Region_State
        (DSL_BUILDER_REGION region,
         DSL_BUILDER_STATE state,
         DSL_STATE_EFFECT_KIND effect_kind,
         UINT32 ordinal,
         UINT32 flags)
{
    const UINT32 caller_flags = DSL_REGION_INTERFACE_UNIQUE_OWNERSHIP |
                                DSL_REGION_INTERFACE_LAYER_OWNED;
    dsl_builder_state *state_record = DSL_Builder_Find_State(state);
    if (region == NULL || state_record == NULL ||
        (effect_kind != DSL_STATE_EFFECT_READ &&
         effect_kind != DSL_STATE_EFFECT_MODIFY) ||
        (flags & ~caller_flags) != 0)
        return FALSE;

    UINT32 roles = effect_kind == DSL_STATE_EFFECT_READ ?
                   DSL_REGION_VALUE_INPUT : DSL_REGION_VALUE_INOUT;
    UINT32 interface_flags = flags | DSL_REGION_INTERFACE_ABSTRACT_STATE |
        (effect_kind == DSL_STATE_EFFECT_READ ?
         DSL_REGION_INTERFACE_STATE_READ :
         DSL_REGION_INTERFACE_STATE_MODIFY);
    return DSL_Region_Declare_Symbol
               (region, state_record->st, roles, ordinal, interface_flags);
}

BOOL
DSL_Builder_Add_State_Effect
        (DSL_BUILDER_VALUE value,
         DSL_BUILDER_STATE state,
         DSL_STATE_EFFECT_KIND effect_kind)
{
    DSL_BUILDER_VALUE_RECORD *value_record =
        DSL_Builder_Find_Value_Record(value);
    dsl_builder_state *state_record = DSL_Builder_Find_State(state);
    DSL_IR_VALUE_RECORD image_value;
    if (value_record == NULL || state_record == NULL ||
        (effect_kind != DSL_STATE_EFFECT_READ &&
         effect_kind != DSL_STATE_EFFECT_MODIFY) ||
        !DSL_IR_Image_Get_Value(value_record->image_value_id, &image_value) ||
        image_value.producer_node_id == DSL_IR_NODE_INVALID_ID)
        return FALSE;

    UINT32 ordinal = 0;
    for (UINT32 i = 1; i <= DSL_Effect_Image_State_Effect_Count(); ++i) {
        DSL_STATE_EFFECT_RECORD previous;
        if (!DSL_Effect_Image_Get_State_Effect(i, &previous))
            return FALSE;
        if (previous.owner_node_id == image_value.producer_node_id)
            ++ordinal;
    }

    DSL_STATE_EFFECT_RECORD record;
    DSL_State_Effect_Record_Init(&record);
    record.owner_node_id = image_value.producer_node_id;
    record.state_object_id = state_record->image_state_id;
    record.effect_kind = effect_kind;
    record.ordinal = ordinal;
    return DSL_Effect_Image_Add_State_Effect(&record) !=
           DSL_STATE_EFFECT_INVALID_ID;
}

ST_IDX
DSL_Builder_Get_State_Symbol (DSL_BUILDER_STATE state)
{
    dsl_builder_state *state_record = DSL_Builder_Find_State(state);
    return state_record == NULL ? ST_IDX_ZERO : state_record->st;
}

BOOL
DSL_Builder_Set_Region_Source_Position
        (DSL_BUILDER_REGION region,
         const DSL_BUILDER_SOURCE_POSITION *source_position)
{
    USRCPOS position;
    if (region == NULL || source_position == NULL ||
        source_position->file_id == 0 ||
        source_position->file_id > DSL_builder_source_files.size() ||
        source_position->line < 0 || source_position->column > 4095)
        return FALSE;

    USRCPOS_clear(position);
    USRCPOS_filenum(position) = source_position->file_id;
    USRCPOS_linenum(position) = source_position->line;
    USRCPOS_column(position) = source_position->column;
    USRCPOS_stmt_begin(position) = source_position->statement_begin != 0;
    USRCPOS_bb_begin(position) = source_position->basic_block_begin != 0;
    return DSL_Region_Set_Source_Position
               (region, USRCPOS_srcpos(position));
}

BOOL
DSL_Builder_Append_PU_Value
        (DSL_BUILDER_PROGRAM_UNIT pu,
         DSL_BUILDER_VALUE value)
{
    WN *body;

    body = DSL_Builder_PU_Body(pu);
    DSL_BUILDER_VALUE_RECORD *record = DSL_Builder_Find_Value_Record(value);
    if (body == NULL || value == NULL ||
        (record != NULL && record->pu != pu) ||
        !DSL_Builder_Select_PU(pu))
        return FALSE;
    if (!DSL_Builder_Get_Value_Annotation(value, NULL))
        return FALSE;

    if (record != NULL)
        return DSL_Builder_Materialize_PU_Value(pu, record);

    WN_INSERT_BlockLast(body, value);
    return TRUE;
}

UINT32
DSL_Builder_Count_PU_Values (DSL_BUILDER_PROGRAM_UNIT pu)
{
    WN *body = DSL_Builder_PU_Body(pu);
    UINT32 count = 0;

    if (body == NULL)
        return 0;

    for (WN *marker = WN_first(body); marker != NULL;
         marker = WN_next(marker)) {
        if (DSL_Builder_Get_Value_Annotation(marker, NULL))
            ++count;
    }

    return count;
}

BOOL
DSL_Builder_Get_PU_Value
        (DSL_BUILDER_PROGRAM_UNIT pu,
         UINT32 index,
         DSL_BUILDER_VALUE_INFO *info)
{
    WN *body = DSL_Builder_PU_Body(pu);
    UINT32 current = 0;
    DSL_OPCODE_ANNOTATION annotation;

    if (body == NULL || info == NULL)
        return FALSE;

    for (WN *marker = WN_first(body); marker != NULL;
         marker = WN_next(marker)) {
        if (!DSL_Builder_Get_Value_Annotation(marker, &annotation))
            continue;
        if (current == index) {
            info->opcode_name = annotation.name;
            info->opcode_name_len = annotation.name_len;
            info->version = annotation.version;
            info->payload = annotation.payload;
            return TRUE;
        }
        ++current;
    }

    return FALSE;
}

UINT32
DSL_Builder_Count_Value_Operands (DSL_BUILDER_VALUE value)
{
    DSL_BUILDER_VALUE_RECORD *record = DSL_Builder_Find_Value_Record(value);

    if (record != NULL)
        return WN_kid_count(record->expression);
    if (value == NULL || !DSL_WN_Verify_Opcode_Carrier(value, NULL))
        return 0;

    return DSL_WN_Opcode_Operand_Count(value);
}

BOOL
DSL_Builder_Get_Value_Info
        (DSL_BUILDER_VALUE value,
         DSL_BUILDER_VALUE_INFO *info)
{
    DSL_OPCODE_ANNOTATION annotation;

    if (info == NULL ||
        !DSL_Builder_Get_Value_Annotation(value, &annotation))
        return FALSE;

    info->opcode_name = annotation.name;
    info->opcode_name_len = annotation.name_len;
    info->version = annotation.version;
    info->payload = annotation.payload;
    return TRUE;
}

BOOL
DSL_Builder_Get_Value_Operand
        (DSL_BUILDER_VALUE value,
         UINT32 operand_index,
         DSL_BUILDER_VALUE_INFO *info)
{
    DSL_BUILDER_VALUE_RECORD *value_record =
        DSL_Builder_Find_Value_Record(value);
    DSL_WHIRL_OPERAND_RECORD record;

    if (value_record != NULL) {
        DSL_OPCODE_ANNOTATION annotation;
        WN *expression = value_record->expression;
        DSL_BUILDER_VALUE_RECORD *operand_record;

        if (info == NULL || operand_index >= WN_kid_count(expression) ||
            WN_operator(WN_kid(expression, operand_index)) != OPR_LDID)
            return FALSE;
        operand_record = DSL_Builder_Find_Value_Record_By_ST
                             (WN_st_idx(WN_kid(expression, operand_index)));
        if (operand_record == NULL ||
            !DSL_WN_Get_Opcode_Annotation
                 (operand_record->expression, &annotation))
            return FALSE;
        info->opcode_name = annotation.name;
        info->opcode_name_len = annotation.name_len;
        info->version = annotation.version;
        info->payload = annotation.payload;
        return TRUE;
    }

    if (value == NULL ||
        info == NULL ||
        !DSL_WN_Verify_Opcode_Carrier(value, NULL) ||
        !DSL_WN_Decode_Opcode_Operand_Record(value, operand_index, &record))
        return FALSE;

    info->opcode_name = record.opcode_name;
    info->opcode_name_len = record.opcode_name_len;
    info->version = record.version;
    info->payload = record.payload;
    return TRUE;
}

BOOL
DSL_Builder_Append_PU_Marker
        (DSL_BUILDER_PROGRAM_UNIT pu,
         DSL_BUILDER_VALUE marker)
{
    return DSL_Builder_Append_PU_Value(pu, marker);
}

UINT32
DSL_Builder_Count_PU_Markers (DSL_BUILDER_PROGRAM_UNIT pu)
{
    return DSL_Builder_Count_PU_Values(pu);
}

BOOL
DSL_Builder_Get_PU_Marker
        (DSL_BUILDER_PROGRAM_UNIT pu,
         UINT32 index,
         DSL_BUILDER_MARKER_INFO *info)
{
    return DSL_Builder_Get_PU_Value(pu, index, info);
}

BOOL
DSL_Builder_Finalize_Mapped_Image
        (const DSL_BUILDER_MAPPED_IMAGE_REQUEST *request)
{
    if (request == NULL ||
        request->path == NULL ||
        request->path[0] == '\0' ||
        request->flags != 0)
        return FALSE;

    if (DSL_Builder_PU_Root == NULL ?
        !DSL_Gatekeeper_Verify_Program(NULL, stderr, NULL) :
        !DSL_Builder_Verify_Program(NULL))
        return FALSE;

    Irb_File_Name = (char *)request->path;

    if (Current_DST == NULL)
        DST_Init(NULL, 0);
    if (Open_Output_Info(Irb_File_Name) == NULL)
        return FALSE;

    for (PU_Info *pu = DSL_Builder_PU_Root; pu != NULL;
         pu = PU_Info_next(pu)) {
        if (!DSL_Builder_Select_PU(pu)) {
            Close_Output_Info();
            return FALSE;
        }
        if (PU_Info_state(pu, WT_SYMTAB) == Subsect_InMem ||
            PU_Info_state(pu, WT_TREE) == Subsect_InMem) {
            Write_PU_Info(pu);
        }
    }

    Write_Global_Info(DSL_Builder_PU_Root);
    Close_Output_Info();
    return TRUE;
}
