/*
 * Copyright (C) 2026 Open64 Project
 */

#include <string.h>
#include <string>
#include <vector>

#include "dsl_ir_image.h"
#include "dsl_opcode.h"
#include "segmented_array.h"
#include "strtab.h"

typedef struct wn_map_tab WN_MAP_TAB;
extern WN_MAP_TAB *Current_Map_Tab;
extern "C" INT32 IPA_WN_MAP32_Get (WN_MAP_TAB *maptab, WN_MAP wn_map,
                                    const WN *wn);

typedef SEGMENTED_ARRAY<DSL_IR_OPCODE_DESCRIPTOR_RECORD>
    DSL_IR_OPCODE_DESCRIPTOR_TABLE;
typedef SEGMENTED_ARRAY<DSL_IR_NODE_RECORD> DSL_IR_NODE_TABLE;
typedef SEGMENTED_ARRAY<DSL_IR_ATTRIBUTE_RECORD> DSL_IR_ATTRIBUTE_TABLE;
typedef SEGMENTED_ARRAY<DSL_IR_VALUE_RECORD> DSL_IR_VALUE_TABLE;
typedef SEGMENTED_ARRAY<DSL_IR_VALUE_REFERENCE_RECORD>
    DSL_IR_VALUE_REFERENCE_TABLE;
typedef SEGMENTED_ARRAY<DSL_STATE_OBJECT_RECORD> DSL_STATE_OBJECT_TABLE;
typedef SEGMENTED_ARRAY<DSL_STATE_EFFECT_RECORD> DSL_STATE_EFFECT_TABLE;
typedef SEGMENTED_ARRAY<DSL_PU_SOURCE_IDENTITY_RECORD>
    DSL_PU_SOURCE_IDENTITY_TABLE;
typedef SEGMENTED_ARRAY<DSL_CALLSITE_METADATA_RECORD>
    DSL_CALLSITE_METADATA_TABLE;
typedef SEGMENTED_ARRAY<DSL_CALL_ARGUMENT_RECORD> DSL_CALL_ARGUMENT_TABLE;
typedef SEGMENTED_ARRAY<DSL_PU_FORMAL_RECORD> DSL_PU_FORMAL_TABLE;

static DSL_IR_OPCODE_DESCRIPTOR_TABLE DSL_ir_opcode_descriptor_table;
static DSL_IR_NODE_TABLE DSL_ir_node_table;
static DSL_IR_ATTRIBUTE_TABLE DSL_ir_attribute_table;
static DSL_IR_VALUE_TABLE DSL_ir_value_table;
static DSL_IR_VALUE_REFERENCE_TABLE DSL_ir_value_reference_table;
static DSL_STATE_OBJECT_TABLE DSL_state_object_table;
static DSL_STATE_EFFECT_TABLE DSL_state_effect_table;
static DSL_PU_SOURCE_IDENTITY_TABLE DSL_pu_source_identity_table;
static DSL_CALLSITE_METADATA_TABLE DSL_callsite_metadata_table;
static DSL_CALL_ARGUMENT_TABLE DSL_call_argument_table;
static DSL_PU_FORMAL_TABLE DSL_pu_formal_table;

typedef struct {
    ST_IDX owner_pu_st;
    WN *call;
    DSL_CALLSITE_METADATA_ID id;
} DSL_CALLSITE_RUNTIME_ASSOCIATION;

static std::vector<DSL_CALLSITE_RUNTIME_ASSOCIATION>
    DSL_callsite_runtime_associations;

static BOOL DSL_IR_Image_String_Id_Valid (STR_IDX id, BOOL required);

typedef struct {
    const DSL_IR_IMAGE_HEADER *header;
    const DSL_IR_OPCODE_DESCRIPTOR_RECORD *opcode_descriptors;
    const DSL_IR_NODE_RECORD *nodes;
    const DSL_IR_ATTRIBUTE_RECORD *attributes;
    const DSL_IR_VALUE_RECORD *values;
    const DSL_IR_VALUE_REFERENCE_RECORD *value_references;
} DSL_IR_IMAGE_VIEW;

typedef char DSL_IR_Image_Header_Size_Check
    [sizeof(DSL_IR_IMAGE_HEADER) == DSL_IR_IMAGE_HEADER_SIZE ? 1 : -1];
typedef char DSL_IR_Opcode_Descriptor_Size_Check
    [sizeof(DSL_IR_OPCODE_DESCRIPTOR_RECORD) ==
        DSL_IR_OPCODE_DESCRIPTOR_RECORD_SIZE ? 1 : -1];
typedef char DSL_IR_Node_Size_Check
    [sizeof(DSL_IR_NODE_RECORD) == DSL_IR_NODE_RECORD_SIZE ? 1 : -1];
typedef char DSL_IR_Attribute_Size_Check
    [sizeof(DSL_IR_ATTRIBUTE_RECORD) == DSL_IR_ATTRIBUTE_RECORD_SIZE ? 1 : -1];
typedef char DSL_IR_Value_Size_Check
    [sizeof(DSL_IR_VALUE_RECORD) == DSL_IR_VALUE_RECORD_SIZE ? 1 : -1];
typedef char DSL_IR_Value_Reference_Size_Check
    [sizeof(DSL_IR_VALUE_REFERENCE_RECORD) ==
        DSL_IR_VALUE_REFERENCE_RECORD_SIZE ? 1 : -1];
typedef char DSL_Effect_Image_Header_Size_Check
    [sizeof(DSL_EFFECT_IMAGE_HEADER) == DSL_EFFECT_IMAGE_HEADER_SIZE ? 1 : -1];
typedef char DSL_State_Object_Size_Check
    [sizeof(DSL_STATE_OBJECT_RECORD) == DSL_STATE_OBJECT_RECORD_SIZE ? 1 : -1];
typedef char DSL_State_Effect_Size_Check
    [sizeof(DSL_STATE_EFFECT_RECORD) == DSL_STATE_EFFECT_RECORD_SIZE ? 1 : -1];
typedef char DSL_Call_Image_Header_Size_Check
    [sizeof(DSL_CALL_IMAGE_HEADER) == DSL_CALL_IMAGE_HEADER_SIZE ? 1 : -1];
typedef char DSL_PU_Source_Identity_Size_Check
    [sizeof(DSL_PU_SOURCE_IDENTITY_RECORD) ==
        DSL_PU_SOURCE_IDENTITY_RECORD_SIZE ? 1 : -1];
typedef char DSL_Callsite_Metadata_Size_Check
    [sizeof(DSL_CALLSITE_METADATA_RECORD) ==
        DSL_CALLSITE_METADATA_RECORD_SIZE ? 1 : -1];
typedef char DSL_Call_ABI_Image_Header_Size_Check
    [sizeof(DSL_CALL_ABI_IMAGE_HEADER) == DSL_CALL_ABI_IMAGE_HEADER_SIZE ? 1 : -1];
typedef char DSL_Call_Argument_Size_Check
    [sizeof(DSL_CALL_ARGUMENT_RECORD) == DSL_CALL_ARGUMENT_RECORD_SIZE ? 1 : -1];
typedef char DSL_PU_Interface_Image_Header_Size_Check
    [sizeof(DSL_PU_INTERFACE_IMAGE_HEADER) ==
        DSL_PU_INTERFACE_IMAGE_HEADER_SIZE ? 1 : -1];
typedef char DSL_PU_Formal_Size_Check
    [sizeof(DSL_PU_FORMAL_RECORD) == DSL_PU_FORMAL_RECORD_SIZE ? 1 : -1];

template <typename RECORD>
static void
DSL_IR_Record_Init (RECORD *record)
{
    if (record != NULL)
        memset (record, 0, sizeof(RECORD));
}

template <typename TABLE, typename RECORD>
static BOOL
DSL_IR_Table_Get (TABLE &table, UINT32 id, RECORD *record)
{
    if (id == 0 || id > table.Size())
        return FALSE;

    if (record != NULL)
        *record = table[id - 1];
    return TRUE;
}

void
DSL_IR_Image_Reset (void)
{
    DSL_ir_opcode_descriptor_table.Delete_down_to(0);
    DSL_ir_node_table.Delete_down_to(0);
    DSL_ir_attribute_table.Delete_down_to(0);
    DSL_ir_value_table.Delete_down_to(0);
    DSL_ir_value_reference_table.Delete_down_to(0);
    DSL_state_object_table.Delete_down_to(0);
    DSL_state_effect_table.Delete_down_to(0);
    DSL_Call_Image_Reset();
    DSL_Call_ABI_Image_Reset();
    DSL_PU_Interface_Image_Reset();
}

static BOOL
DSL_Call_Image_Report (FILE *diagnostic, const char *message, UINT32 id)
{
    if (diagnostic != NULL)
        fprintf(diagnostic, "DSL call image error: %s id=%u\n", message, id);
    return FALSE;
}

void
DSL_Call_Image_Get_Header (DSL_CALL_IMAGE_HEADER *header)
{
    if (header == NULL)
        return;
    memset(header, 0, sizeof(*header));
    header->magic = DSL_CALL_IMAGE_MAGIC;
    header->version = DSL_CALL_IMAGE_VERSION;
    header->pu_identity_count = DSL_pu_source_identity_table.Size();
    header->callsite_count = DSL_callsite_metadata_table.Size();
}

void
DSL_Call_Image_Reset (void)
{
    DSL_pu_source_identity_table.Delete_down_to(0);
    DSL_callsite_metadata_table.Delete_down_to(0);
    DSL_callsite_runtime_associations.clear();
}

BOOL
DSL_Call_Image_Has_Records (void)
{
    return DSL_pu_source_identity_table.Size() != 0 ||
           DSL_callsite_metadata_table.Size() != 0;
}

static BOOL
DSL_Call_Image_Call_Has_Association (UINT32 id)
{
    for (UINT32 i = 0; i < DSL_callsite_runtime_associations.size(); ++i) {
        if (DSL_callsite_runtime_associations[i].id == id &&
            DSL_callsite_runtime_associations[i].call != NULL)
            return TRUE;
    }
    return FALSE;
}

static BOOL
DSL_Call_Image_Association_Valid
        (const DSL_CALLSITE_METADATA_RECORD &record)
{
    for (UINT32 i = 0; i < DSL_callsite_runtime_associations.size(); ++i) {
        const DSL_CALLSITE_RUNTIME_ASSOCIATION &association =
            DSL_callsite_runtime_associations[i];
        if (association.id != record.id)
            continue;
        return ST_IDX_index(association.owner_pu_st) != 0 &&
               association.call != NULL &&
               association.owner_pu_st == record.owner_pu_st;
    }
    return record.wn_offset != 0;
}

BOOL
DSL_Call_Image_Validate (FILE *diagnostic)
{
    for (UINT32 i = 0; i < DSL_pu_source_identity_table.Size(); ++i) {
        const DSL_PU_SOURCE_IDENTITY_RECORD &record =
            DSL_pu_source_identity_table[i];
        if (record.id != i + 1 || ST_IDX_index(record.owner_pu_st) == 0 ||
            !DSL_IR_Image_String_Id_Valid
                 (record.canonical_definition_name, TRUE) ||
            !DSL_IR_Image_String_Id_Valid(record.defining_module, FALSE) ||
            !DSL_IR_Image_String_Id_Valid(record.defining_file, TRUE) ||
            record.defining_line == 0 || record.reserved != 0)
            return DSL_Call_Image_Report
                       (diagnostic, "invalid PU identity", i + 1);
        for (UINT32 j = 0; j < i; ++j) {
            if (DSL_pu_source_identity_table[j].owner_pu_st ==
                record.owner_pu_st)
                return DSL_Call_Image_Report
                           (diagnostic, "duplicate PU identity", i + 1);
        }
    }
    for (UINT32 i = 0; i < DSL_callsite_metadata_table.Size(); ++i) {
        const DSL_CALLSITE_METADATA_RECORD &record =
            DSL_callsite_metadata_table[i];
        if (record.id != i + 1 || ST_IDX_index(record.owner_pu_st) == 0 ||
            ST_IDX_index(record.callee_pu_st) == 0 ||
            !DSL_IR_Image_String_Id_Valid
                 (record.canonical_class_name, TRUE) ||
            !DSL_IR_Image_String_Id_Valid(record.instance_path, TRUE) ||
            !DSL_IR_Image_String_Id_Valid(record.context_identity, TRUE) ||
            (record.wn_offset == 0 &&
             !DSL_Call_Image_Call_Has_Association(record.id)) ||
            !DSL_Call_Image_Association_Valid(record))
            return DSL_Call_Image_Report
                       (diagnostic, "invalid callsite", i + 1);
    }
    return TRUE;
}

DSL_PU_SOURCE_IDENTITY_ID
DSL_Call_Image_Add_PU_Identity
        (const DSL_PU_SOURCE_IDENTITY_RECORD *record)
{
    if (record == NULL || ST_IDX_index(record->owner_pu_st) == 0 ||
        record->canonical_definition_name == STR_IDX_ZERO ||
        record->defining_file == STR_IDX_ZERO || record->defining_line == 0)
        return DSL_PU_SOURCE_IDENTITY_INVALID_ID;
    DSL_PU_SOURCE_IDENTITY_RECORD copy = *record;
    UINT32 index = DSL_pu_source_identity_table.Insert(copy);
    DSL_pu_source_identity_table[index].id = index + 1;
    if (!DSL_Call_Image_Validate(NULL)) {
        DSL_pu_source_identity_table.Delete_down_to(index);
        return DSL_PU_SOURCE_IDENTITY_INVALID_ID;
    }
    return index + 1;
}

DSL_CALLSITE_METADATA_ID
DSL_Call_Image_Add_Callsite
        (ST_IDX owner_pu_st, WN *call,
         const DSL_CALLSITE_METADATA_RECORD *record)
{
    if (ST_IDX_index(owner_pu_st) == 0 || call == NULL || record == NULL ||
        record->owner_pu_st != owner_pu_st ||
        ST_IDX_index(record->callee_pu_st) == 0)
        return DSL_CALLSITE_METADATA_INVALID_ID;
    DSL_CALLSITE_METADATA_RECORD copy = *record;
    UINT32 index = DSL_callsite_metadata_table.Insert(copy);
    DSL_callsite_metadata_table[index].id = index + 1;
    DSL_CALLSITE_RUNTIME_ASSOCIATION association;
    association.owner_pu_st = owner_pu_st;
    association.call = call;
    association.id = index + 1;
    DSL_callsite_runtime_associations.push_back(association);
    if (!DSL_Call_Image_Validate(NULL)) {
        DSL_callsite_runtime_associations.pop_back();
        DSL_callsite_metadata_table.Delete_down_to(index);
        return DSL_CALLSITE_METADATA_INVALID_ID;
    }
    return index + 1;
}

BOOL
DSL_Call_Image_Find_PU_Identity
        (ST_IDX owner_pu_st, DSL_PU_SOURCE_IDENTITY_RECORD *record)
{
    for (UINT32 i = 0; i < DSL_pu_source_identity_table.Size(); ++i) {
        if (DSL_pu_source_identity_table[i].owner_pu_st == owner_pu_st)
            return DSL_IR_Table_Get
                       (DSL_pu_source_identity_table, i + 1, record);
    }
    return FALSE;
}

BOOL
DSL_Call_Image_Find_Callsite
        (const WN *call, DSL_CALLSITE_METADATA_RECORD *record)
{
    for (UINT32 i = 0; i < DSL_callsite_runtime_associations.size(); ++i) {
        const DSL_CALLSITE_RUNTIME_ASSOCIATION &association =
            DSL_callsite_runtime_associations[i];
        if (association.call == call)
            return DSL_IR_Table_Get
                       (DSL_callsite_metadata_table, association.id, record);
    }
    return FALSE;
}

const WN *
DSL_Call_Image_Get_Call_WN (DSL_CALLSITE_METADATA_ID id)
{
    for (UINT32 i = 0; i < DSL_callsite_runtime_associations.size(); ++i) {
        if (DSL_callsite_runtime_associations[i].id == id)
            return DSL_callsite_runtime_associations[i].call;
    }
    return NULL;
}

UINT32 DSL_Call_Image_PU_Identity_Count (void)
{ return DSL_pu_source_identity_table.Size(); }

UINT32 DSL_Call_Image_Callsite_Count (void)
{ return DSL_callsite_metadata_table.Size(); }

BOOL DSL_Call_Image_Get_PU_Identity
        (DSL_PU_SOURCE_IDENTITY_ID id, DSL_PU_SOURCE_IDENTITY_RECORD *record)
{ return DSL_IR_Table_Get(DSL_pu_source_identity_table, id, record); }

BOOL DSL_Call_Image_Get_Callsite
        (DSL_CALLSITE_METADATA_ID id, DSL_CALLSITE_METADATA_RECORD *record)
{ return DSL_IR_Table_Get(DSL_callsite_metadata_table, id, record); }

BOOL
DSL_Call_Image_PU_Has_Calls (ST_IDX owner_pu_st)
{
    if (ST_IDX_index(owner_pu_st) == 0)
        return FALSE;
    for (UINT32 i = 0; i < DSL_callsite_metadata_table.Size(); ++i) {
        if (DSL_callsite_metadata_table[i].owner_pu_st == owner_pu_st)
            return TRUE;
    }
    return FALSE;
}

BOOL
DSL_Call_Image_Finalize_PU (ST_IDX owner_pu_st, WN_MAP off_map)
{
    if (!DSL_Call_Image_PU_Has_Calls(owner_pu_st))
        return TRUE;
    if (off_map == (WN_MAP)-1)
        return FALSE;
    for (UINT32 i = 0; i < DSL_callsite_runtime_associations.size(); ++i) {
        DSL_CALLSITE_RUNTIME_ASSOCIATION &association =
            DSL_callsite_runtime_associations[i];
        if (association.owner_pu_st != owner_pu_st)
            continue;
        UINT32 offset = IPA_WN_MAP32_Get
                            (Current_Map_Tab, off_map, association.call);
        if (offset == 0)
            return FALSE;
        DSL_callsite_metadata_table[association.id - 1].wn_offset = offset;
    }
    return DSL_Call_Image_Validate(NULL);
}

BOOL
DSL_Call_Image_Load_PU
        (ST_IDX owner_pu_st, const void *tree_base, UINT64 tree_size)
{
    if (ST_IDX_index(owner_pu_st) == 0 || tree_base == NULL || tree_size == 0)
        return FALSE;
    for (UINT32 i = 0; i < DSL_callsite_metadata_table.Size(); ++i) {
        const DSL_CALLSITE_METADATA_RECORD &record =
            DSL_callsite_metadata_table[i];
        if (record.owner_pu_st != owner_pu_st)
            continue;
        if (record.wn_offset == 0 || record.wn_offset >= tree_size)
            return FALSE;
        WN *call = (WN *)((const char *)tree_base + record.wn_offset);
        DSL_CALLSITE_RUNTIME_ASSOCIATION association;
        association.owner_pu_st = owner_pu_st;
        association.call = call;
        association.id = record.id;
        DSL_callsite_runtime_associations.push_back(association);
    }
    return TRUE;
}

BOOL
DSL_Call_Image_Load_Mapped
        (const void *section_base, UINT64 section_size, FILE *diagnostic)
{
    if (section_base == NULL || section_size < DSL_CALL_IMAGE_HEADER_SIZE)
        return DSL_Call_Image_Report(diagnostic, "section is truncated", 0);
    const DSL_CALL_IMAGE_HEADER *header =
        (const DSL_CALL_IMAGE_HEADER *)section_base;
    UINT64 expected = DSL_CALL_IMAGE_HEADER_SIZE +
        (UINT64)header->pu_identity_count *
            DSL_PU_SOURCE_IDENTITY_RECORD_SIZE +
        (UINT64)header->callsite_count * DSL_CALLSITE_METADATA_RECORD_SIZE;
    if (header->magic != DSL_CALL_IMAGE_MAGIC ||
        header->version != DSL_CALL_IMAGE_VERSION || header->flags != 0 ||
        header->reserved != 0 || expected != section_size)
        return DSL_Call_Image_Report(diagnostic, "invalid header", 0);
    const char *cursor = (const char *)section_base +
                         DSL_CALL_IMAGE_HEADER_SIZE;
    const DSL_PU_SOURCE_IDENTITY_RECORD *identities =
        (const DSL_PU_SOURCE_IDENTITY_RECORD *)cursor;
    cursor += (UINT64)header->pu_identity_count *
              DSL_PU_SOURCE_IDENTITY_RECORD_SIZE;
    const DSL_CALLSITE_METADATA_RECORD *calls =
        (const DSL_CALLSITE_METADATA_RECORD *)cursor;
    DSL_Call_Image_Reset();
    if (header->pu_identity_count != 0)
        DSL_pu_source_identity_table.Insert
            (identities, header->pu_identity_count);
    if (header->callsite_count != 0)
        DSL_callsite_metadata_table.Insert(calls, header->callsite_count);
    if (!DSL_Call_Image_Validate(diagnostic)) {
        DSL_pu_source_identity_table.Delete_down_to(0);
        DSL_callsite_metadata_table.Delete_down_to(0);
        return FALSE;
    }
    return TRUE;
}

static BOOL
DSL_Call_ABI_Image_Report (FILE *diagnostic, const char *message, UINT32 id)
{
    if (diagnostic != NULL)
        fprintf(diagnostic, "DSL call ABI image error: %s id=%u\n",
                message, id);
    return FALSE;
}

void
DSL_Call_ABI_Image_Get_Header (DSL_CALL_ABI_IMAGE_HEADER *header)
{
    if (header == NULL)
        return;
    memset(header, 0, sizeof(*header));
    header->magic = DSL_CALL_ABI_IMAGE_MAGIC;
    header->version = DSL_CALL_ABI_IMAGE_VERSION;
    header->argument_count = DSL_call_argument_table.Size();
}

void
DSL_Call_ABI_Image_Reset (void)
{
    DSL_call_argument_table.Delete_down_to(0);
}

BOOL
DSL_Call_ABI_Image_Has_Records (void)
{
    return DSL_call_argument_table.Size() != 0;
}

BOOL
DSL_Call_ABI_Image_Validate (FILE *diagnostic)
{
    for (UINT32 i = 0; i < DSL_call_argument_table.Size(); ++i) {
        const DSL_CALL_ARGUMENT_RECORD &record = DSL_call_argument_table[i];
        DSL_CALLSITE_METADATA_RECORD callsite;
        if (record.id != i + 1 ||
            record.callsite_id == DSL_CALLSITE_METADATA_INVALID_ID ||
            !DSL_Call_Image_Get_Callsite(record.callsite_id, &callsite) ||
            record.argument_value_id == DSL_IR_VALUE_INVALID_ID ||
            record.argument_value_id > DSL_ir_value_table.Size() ||
            record.actual_ordinal == DSL_CALL_ARGUMENT_INVALID_ORDINAL ||
            record.callee_formal_ordinal ==
                DSL_CALL_ARGUMENT_INVALID_ORDINAL ||
            record.flags != 0 ||
            !DSL_IR_Image_String_Id_Valid(record.semantic_role, TRUE))
            return DSL_Call_ABI_Image_Report
                       (diagnostic, "invalid argument", i + 1);

        for (UINT32 j = 0; j < i; ++j) {
            const DSL_CALL_ARGUMENT_RECORD &previous =
                DSL_call_argument_table[j];
            if (previous.callsite_id == record.callsite_id &&
                previous.actual_ordinal == record.actual_ordinal)
                return DSL_Call_ABI_Image_Report
                           (diagnostic, "duplicate call argument", i + 1);
            DSL_CALLSITE_METADATA_RECORD previous_callsite;
            if (previous.callee_formal_ordinal ==
                    record.callee_formal_ordinal &&
                DSL_Call_Image_Get_Callsite
                    (previous.callsite_id, &previous_callsite) &&
                previous_callsite.callee_pu_st == callsite.callee_pu_st &&
                strcmp(Index_To_Str(previous.semantic_role),
                       Index_To_Str(record.semantic_role)) != 0)
                return DSL_Call_ABI_Image_Report
                           (diagnostic, "inconsistent formal role", i + 1);
        }
    }
    return TRUE;
}

DSL_CALL_ARGUMENT_ID
DSL_Call_ABI_Image_Add_Argument
        (const WN *call, const DSL_CALL_ARGUMENT_RECORD *record)
{
    DSL_CALLSITE_METADATA_RECORD callsite;
    if (call == NULL || record == NULL ||
        !DSL_Call_Image_Find_Callsite(call, &callsite) ||
        record->callsite_id != callsite.id)
        return DSL_CALL_ARGUMENT_INVALID_ID;

    DSL_CALL_ARGUMENT_RECORD copy = *record;
    UINT32 index = DSL_call_argument_table.Insert(copy);
    DSL_call_argument_table[index].id = index + 1;
    if (!DSL_Call_ABI_Image_Validate(NULL)) {
        DSL_call_argument_table.Delete_down_to(index);
        return DSL_CALL_ARGUMENT_INVALID_ID;
    }
    return index + 1;
}

UINT32
DSL_Call_ABI_Image_Argument_Count (void)
{
    return DSL_call_argument_table.Size();
}

BOOL
DSL_Call_ABI_Image_Get_Argument
        (DSL_CALL_ARGUMENT_ID id, DSL_CALL_ARGUMENT_RECORD *record)
{
    return DSL_IR_Table_Get(DSL_call_argument_table, id, record);
}

BOOL
DSL_Call_ABI_Image_Find_Argument_By_Id
        (DSL_CALLSITE_METADATA_ID callsite_id, UINT32 actual_ordinal,
         DSL_CALL_ARGUMENT_RECORD *record)
{
    for (UINT32 i = 0; i < DSL_call_argument_table.Size(); ++i) {
        if (DSL_call_argument_table[i].callsite_id == callsite_id &&
            DSL_call_argument_table[i].actual_ordinal == actual_ordinal)
            return DSL_IR_Table_Get(DSL_call_argument_table, i + 1, record);
    }
    return FALSE;
}

BOOL
DSL_Call_ABI_Image_Find_Argument
        (const WN *call, UINT32 actual_ordinal,
         DSL_CALL_ARGUMENT_RECORD *record)
{
    DSL_CALLSITE_METADATA_RECORD callsite;
    return DSL_Call_Image_Find_Callsite(call, &callsite) &&
           DSL_Call_ABI_Image_Find_Argument_By_Id
               (callsite.id, actual_ordinal, record);
}

BOOL
DSL_Call_ABI_Image_Update_Argument_Value
        (const WN *call, UINT32 actual_ordinal,
         DSL_IR_VALUE_ID expected_value_id,
         DSL_IR_VALUE_ID replacement_value_id)
{
    DSL_CALLSITE_METADATA_RECORD callsite;
    if (call == NULL || replacement_value_id == DSL_IR_VALUE_INVALID_ID ||
        replacement_value_id > DSL_ir_value_table.Size() ||
        !DSL_Call_Image_Find_Callsite(call, &callsite))
        return FALSE;
    for (UINT32 i = 0; i < DSL_call_argument_table.Size(); ++i) {
        DSL_CALL_ARGUMENT_RECORD &argument = DSL_call_argument_table[i];
        if (argument.callsite_id == callsite.id &&
            argument.actual_ordinal == actual_ordinal) {
            if (argument.argument_value_id != expected_value_id)
                return FALSE;
            argument.argument_value_id = replacement_value_id;
            return TRUE;
        }
    }
    return TRUE;
}

UINT32
DSL_Call_ABI_Image_Callee_Formal_Count
        (ST_IDX callee_pu_st, UINT32 callee_formal_ordinal)
{
    UINT32 count = 0;
    for (UINT32 i = 0; i < DSL_call_argument_table.Size(); ++i) {
        DSL_CALLSITE_METADATA_RECORD callsite;
        if (DSL_call_argument_table[i].callee_formal_ordinal ==
                callee_formal_ordinal &&
            DSL_Call_Image_Get_Callsite
                (DSL_call_argument_table[i].callsite_id, &callsite) &&
            callsite.callee_pu_st == callee_pu_st)
            ++count;
    }
    return count;
}

BOOL
DSL_Call_ABI_Image_Get_Callee_Formal_Argument
        (ST_IDX callee_pu_st, UINT32 callee_formal_ordinal, UINT32 index,
         DSL_CALL_ARGUMENT_RECORD *record)
{
    UINT32 found = 0;
    for (UINT32 i = 0; i < DSL_call_argument_table.Size(); ++i) {
        DSL_CALLSITE_METADATA_RECORD callsite;
        if (DSL_call_argument_table[i].callee_formal_ordinal !=
                callee_formal_ordinal ||
            !DSL_Call_Image_Get_Callsite
                (DSL_call_argument_table[i].callsite_id, &callsite) ||
            callsite.callee_pu_st != callee_pu_st)
            continue;
        if (found++ == index)
            return DSL_IR_Table_Get(DSL_call_argument_table, i + 1, record);
    }
    return FALSE;
}

BOOL
DSL_Call_ABI_Image_Load_Mapped
        (const void *section_base, UINT64 section_size, FILE *diagnostic)
{
    if (section_base == NULL || section_size < DSL_CALL_ABI_IMAGE_HEADER_SIZE)
        return DSL_Call_ABI_Image_Report
                   (diagnostic, "section is truncated", 0);
    const DSL_CALL_ABI_IMAGE_HEADER *header =
        (const DSL_CALL_ABI_IMAGE_HEADER *)section_base;
    UINT64 expected = DSL_CALL_ABI_IMAGE_HEADER_SIZE +
        (UINT64)header->argument_count * DSL_CALL_ARGUMENT_RECORD_SIZE;
    if (header->magic != DSL_CALL_ABI_IMAGE_MAGIC ||
        header->version != DSL_CALL_ABI_IMAGE_VERSION || header->flags != 0 ||
        header->reserved0 != 0 || header->reserved1 != 0 ||
        expected != section_size)
        return DSL_Call_ABI_Image_Report(diagnostic, "invalid header", 0);

    const DSL_CALL_ARGUMENT_RECORD *arguments =
        (const DSL_CALL_ARGUMENT_RECORD *)
            ((const char *)section_base + DSL_CALL_ABI_IMAGE_HEADER_SIZE);
    DSL_Call_ABI_Image_Reset();
    if (header->argument_count != 0)
        DSL_call_argument_table.Insert(arguments, header->argument_count);
    if (!DSL_Call_ABI_Image_Validate(diagnostic)) {
        DSL_Call_ABI_Image_Reset();
        return FALSE;
    }
    return TRUE;
}

static BOOL
DSL_PU_Interface_Image_Report
        (FILE *diagnostic, const char *message, UINT32 id)
{
    if (diagnostic != NULL)
        fprintf(diagnostic, "DSL PU interface image error: %s id=%u\n",
                message, id);
    return FALSE;
}

void
DSL_PU_Interface_Image_Get_Header
        (DSL_PU_INTERFACE_IMAGE_HEADER *header)
{
    if (header == NULL)
        return;
    memset(header, 0, sizeof(*header));
    header->magic = DSL_PU_INTERFACE_IMAGE_MAGIC;
    header->version = DSL_PU_INTERFACE_IMAGE_VERSION;
    header->formal_count = DSL_pu_formal_table.Size();
}

void
DSL_PU_Interface_Image_Reset (void)
{
    DSL_pu_formal_table.Delete_down_to(0);
}

BOOL
DSL_PU_Interface_Image_Has_Records (void)
{
    return DSL_pu_formal_table.Size() != 0;
}

DSL_PU_FORMAL_ID
DSL_PU_Interface_Image_Add_Formal (const DSL_PU_FORMAL_RECORD *record)
{
    if (record == NULL)
        return DSL_PU_FORMAL_INVALID_ID;
    DSL_PU_FORMAL_RECORD copy = *record;
    UINT32 index = DSL_pu_formal_table.Insert(copy);
    DSL_pu_formal_table[index].id = index + 1;
    if (!DSL_PU_Interface_Image_Validate(NULL)) {
        DSL_pu_formal_table.Delete_down_to(index);
        return DSL_PU_FORMAL_INVALID_ID;
    }
    return index + 1;
}

UINT32
DSL_PU_Interface_Image_Formal_Count (void)
{
    return DSL_pu_formal_table.Size();
}

BOOL
DSL_PU_Interface_Image_Get_Formal
        (DSL_PU_FORMAL_ID id, DSL_PU_FORMAL_RECORD *record)
{
    return DSL_IR_Table_Get(DSL_pu_formal_table, id, record);
}

BOOL
DSL_PU_Interface_Image_Find_Formal
        (ST_IDX owner_pu_st, UINT32 formal_ordinal,
         DSL_PU_FORMAL_RECORD *record)
{
    for (UINT32 i = 0; i < DSL_pu_formal_table.Size(); ++i) {
        const DSL_PU_FORMAL_RECORD &formal = DSL_pu_formal_table[i];
        if (formal.owner_pu_st == owner_pu_st &&
            formal.formal_ordinal == formal_ordinal)
            return DSL_IR_Table_Get(DSL_pu_formal_table, i + 1, record);
    }
    return FALSE;
}

BOOL
DSL_PU_Interface_Image_Load_Mapped
        (const void *section_base, UINT64 section_size, FILE *diagnostic)
{
    if (section_base == NULL ||
        section_size < DSL_PU_INTERFACE_IMAGE_HEADER_SIZE)
        return DSL_PU_Interface_Image_Report
                   (diagnostic, "section is truncated", 0);
    const DSL_PU_INTERFACE_IMAGE_HEADER *header =
        (const DSL_PU_INTERFACE_IMAGE_HEADER *)section_base;
    UINT64 expected = DSL_PU_INTERFACE_IMAGE_HEADER_SIZE +
        (UINT64)header->formal_count * DSL_PU_FORMAL_RECORD_SIZE;
    if (header->magic != DSL_PU_INTERFACE_IMAGE_MAGIC ||
        header->version != DSL_PU_INTERFACE_IMAGE_VERSION ||
        header->flags != 0 || header->reserved0 != 0 ||
        header->reserved1 != 0 || expected != section_size)
        return DSL_PU_Interface_Image_Report
                   (diagnostic, "invalid header", 0);

    const DSL_PU_FORMAL_RECORD *formals =
        (const DSL_PU_FORMAL_RECORD *)
            ((const char *)section_base +
             DSL_PU_INTERFACE_IMAGE_HEADER_SIZE);
    DSL_PU_Interface_Image_Reset();
    if (header->formal_count != 0)
        DSL_pu_formal_table.Insert(formals, header->formal_count);
    if (!DSL_PU_Interface_Image_Validate(diagnostic)) {
        DSL_PU_Interface_Image_Reset();
        return FALSE;
    }
    return TRUE;
}

void
DSL_IR_Image_Get_Header (DSL_IR_IMAGE_HEADER *header)
{
    if (header == NULL)
        return;

    memset (header, 0, sizeof(*header));
    header->version = DSL_IR_IMAGE_VERSION;
    header->record_kind_count = DSL_IR_IMAGE_RECORD_VALUE_REFERENCE;
    header->capabilities = DSL_IR_IMAGE_CAP_OPCODE_DESCRIPTOR |
                           DSL_IR_IMAGE_CAP_NODE |
                           DSL_IR_IMAGE_CAP_TYPED_ATTRIBUTE |
                           DSL_IR_IMAGE_CAP_VALUE |
                           DSL_IR_IMAGE_CAP_VALUE_REFERENCE;
    header->opcode_descriptor_count = DSL_ir_opcode_descriptor_table.Size();
    header->node_count = DSL_ir_node_table.Size();
    header->attribute_count = DSL_ir_attribute_table.Size();
    header->value_count = DSL_ir_value_table.Size();
    header->value_reference_count = DSL_ir_value_reference_table.Size();
}

BOOL
DSL_IR_Image_Has_Records (void)
{
    return DSL_ir_opcode_descriptor_table.Size() != 0 ||
           DSL_ir_node_table.Size() != 0 ||
           DSL_ir_attribute_table.Size() != 0 ||
           DSL_ir_value_table.Size() != 0 ||
           DSL_ir_value_reference_table.Size() != 0;
}

static BOOL
DSL_IR_Image_String_Id_Valid (STR_IDX id, BOOL required)
{
    if (id == STR_IDX_ZERO)
        return !required;
    return id < STR_Table_Size();
}

static BOOL
DSL_IR_Image_Report (FILE *diagnostic, const char *message, UINT32 id)
{
    if (diagnostic != NULL)
        fprintf (diagnostic, "DSL image error: %s id=%u\n", message, id);
    return FALSE;
}

static BOOL
DSL_IR_Image_View_Validate (const DSL_IR_IMAGE_VIEW *view, FILE *diagnostic)
{
    const DSL_IR_IMAGE_HEADER &header = *view->header;
    UINT32 active_attribute_count = 0;
    UINT32 active_value_reference_count = 0;
    const UINT32 required_capabilities =
        DSL_IR_IMAGE_CAP_OPCODE_DESCRIPTOR |
        DSL_IR_IMAGE_CAP_NODE |
        DSL_IR_IMAGE_CAP_TYPED_ATTRIBUTE |
        DSL_IR_IMAGE_CAP_VALUE |
        DSL_IR_IMAGE_CAP_VALUE_REFERENCE;

    if (header.version != DSL_IR_IMAGE_VERSION ||
        header.record_kind_count != DSL_IR_IMAGE_RECORD_VALUE_REFERENCE ||
        header.flags != 0 || header.capabilities != required_capabilities ||
        header.reserved0 != 0 || header.reserved1 != 0 ||
        header.reserved2 != 0)
        return DSL_IR_Image_Report(diagnostic, "invalid header", 0);

    for (UINT32 i = 0; i < header.opcode_descriptor_count; ++i) {
        const DSL_IR_OPCODE_DESCRIPTOR_RECORD &record =
            view->opcode_descriptors[i];
        DSL_OPERATOR_INFO info;

        if (record.id != i + 1 || record.version == 0 ||
            !DSL_IR_Image_String_Id_Valid(record.logical_name, TRUE) ||
            !DSL_IR_Image_String_Id_Valid(record.stable_name, TRUE) ||
            !DSL_IR_Image_String_Id_Valid(record.attribute_schema, FALSE) ||
            !DSL_IR_Image_String_Id_Valid(record.diagnostic_prefix, FALSE) ||
            !DSL_Operator_Get_Info_Version
                 ((DSL_OPERATOR)record.logical_operator, record.version,
                  &info) ||
            strcmp(Index_To_Str(record.logical_name), info.logical_name) != 0 ||
            strcmp(Index_To_Str(record.stable_name), info.stable_name) != 0)
            return DSL_IR_Image_Report
                       (diagnostic, "invalid opcode descriptor", i + 1);
    }

    for (UINT32 i = 0; i < header.value_count; ++i) {
        const DSL_IR_VALUE_RECORD &record = view->values[i];
        if (record.id != i + 1 || record.value_kind == DSL_IR_VALUE_UNKNOWN ||
            record.producer_node_id > header.node_count ||
            (record.flags & ~DSL_IR_VALUE_FLAG_REDIRECTED) != 0 ||
            record.reserved != 0 ||
            !DSL_IR_Image_String_Id_Valid(record.name, FALSE) ||
            !DSL_IR_Image_String_Id_Valid(record.metadata, FALSE))
            return DSL_IR_Image_Report(diagnostic, "invalid value", i + 1);
    }

    for (UINT32 i = 0; i < header.attribute_count; ++i) {
        const DSL_IR_ATTRIBUTE_RECORD &record = view->attributes[i];
        if (record.id != i + 1 || record.owner_node_id == 0 ||
            record.owner_node_id > header.node_count ||
            record.value_kind == DSL_IR_ATTRIBUTE_VALUE_UNKNOWN ||
            !DSL_IR_Image_String_Id_Valid(record.name, TRUE) ||
            !DSL_IR_Image_String_Id_Valid(record.value, FALSE))
            return DSL_IR_Image_Report(diagnostic, "invalid attribute", i + 1);
    }

    for (UINT32 i = 0; i < header.value_reference_count; ++i) {
        const DSL_IR_VALUE_REFERENCE_RECORD &record =
            view->value_references[i];
        if (record.id != i + 1 || record.owner_node_id == 0 ||
            record.owner_node_id > header.node_count || record.value_id == 0 ||
            record.value_id > header.value_count)
            return DSL_IR_Image_Report
                       (diagnostic, "invalid value reference", i + 1);
    }

    for (UINT32 i = 0; i < header.node_count; ++i) {
        const DSL_IR_NODE_RECORD &record = view->nodes[i];
        const UINT32 valid_node_flags = DSL_IR_NODE_FLAG_RETIRED |
                                        DSL_IR_NODE_REDIRECT_ORDINAL_MASK;
        active_attribute_count += record.attribute_count;
        active_value_reference_count += record.operand_count;
        if (record.id != i + 1 || record.opcode_descriptor_id == 0 ||
            record.opcode_descriptor_id > header.opcode_descriptor_count ||
            record.result_value_id == 0 ||
            record.result_value_id > header.value_count ||
            (record.flags & ~valid_node_flags) != 0 ||
            ((record.flags & DSL_IR_NODE_FLAG_RETIRED) == 0 &&
             (record.flags & DSL_IR_NODE_REDIRECT_ORDINAL_MASK) != 0) ||
            !DSL_IR_Image_String_Id_Valid(record.payload, FALSE))
            return DSL_IR_Image_Report(diagnostic, "invalid node", i + 1);

        if ((record.operand_count == 0 &&
             record.first_operand_reference_id != 0) ||
            (record.operand_count != 0 &&
             (record.first_operand_reference_id == 0 ||
              record.operand_count > header.value_reference_count ||
              record.first_operand_reference_id >
                  header.value_reference_count - record.operand_count + 1)))
            return DSL_IR_Image_Report
                       (diagnostic, "invalid node operand range", i + 1);

        if ((record.attribute_count == 0 && record.first_attribute_id != 0) ||
            (record.attribute_count != 0 &&
             (record.first_attribute_id == 0 ||
              record.attribute_count > header.attribute_count ||
              record.first_attribute_id >
                  header.attribute_count - record.attribute_count + 1)))
            return DSL_IR_Image_Report
                       (diagnostic, "invalid node attribute range", i + 1);

        const DSL_IR_OPCODE_DESCRIPTOR_RECORD &descriptor =
            view->opcode_descriptors[record.opcode_descriptor_id - 1];
        if (descriptor.operand_count >= 0 &&
            (UINT32)descriptor.operand_count != record.operand_count)
            return DSL_IR_Image_Report
                       (diagnostic, "node operand contract mismatch", i + 1);

        for (UINT32 j = 0; j < record.operand_count; ++j) {
            const DSL_IR_VALUE_REFERENCE_RECORD &reference =
                view->value_references
                    [record.first_operand_reference_id - 1 + j];
            if (reference.owner_node_id != record.id || reference.ordinal != j)
                return DSL_IR_Image_Report
                           (diagnostic, "node operand ownership mismatch", i + 1);
        }

        for (UINT32 j = 0; j < record.attribute_count; ++j) {
            const DSL_IR_ATTRIBUTE_RECORD &attribute =
                view->attributes[record.first_attribute_id - 1 + j];
            if (attribute.owner_node_id != record.id)
                return DSL_IR_Image_Report
                           (diagnostic, "node attribute ownership mismatch", i + 1);
        }

        const DSL_IR_VALUE_RECORD &result =
            view->values[record.result_value_id - 1];
        if (result.producer_node_id != record.id)
            return DSL_IR_Image_Report
                       (diagnostic, "node result provenance mismatch", i + 1);
        if ((record.flags & DSL_IR_NODE_FLAG_RETIRED) != 0) {
            UINT32 ordinal = (record.flags &
                              DSL_IR_NODE_REDIRECT_ORDINAL_MASK) >>
                             DSL_IR_NODE_REDIRECT_ORDINAL_SHIFT;
            const DSL_IR_VALUE_REFERENCE_RECORD *target_reference =
                ordinal >= record.operand_count ? NULL :
                &view->value_references
                    [record.first_operand_reference_id - 1 + ordinal];
            const DSL_IR_VALUE_RECORD *target = target_reference == NULL ?
                NULL : &view->values[target_reference->value_id - 1];
            if ((result.flags & DSL_IR_VALUE_FLAG_REDIRECTED) == 0 ||
                descriptor.effect_model != DSL_EFFECT_MODEL_PURE ||
                target_reference == NULL ||
                target_reference->owner_node_id != record.id ||
                target_reference->ordinal != ordinal ||
                target == NULL || target->id == result.id ||
                target->ty != result.ty ||
                (target->flags & DSL_IR_VALUE_FLAG_REDIRECTED) != 0)
                return DSL_IR_Image_Report
                           (diagnostic, "invalid retired node", i + 1);
        } else if ((result.flags & DSL_IR_VALUE_FLAG_REDIRECTED) != 0) {
            return DSL_IR_Image_Report
                       (diagnostic, "redirected live value", result.id);
        }
    }

    if (active_attribute_count != header.attribute_count ||
        active_value_reference_count != header.value_reference_count)
        return DSL_IR_Image_Report
                   (diagnostic, "unowned relationship record", 0);

    for (UINT32 i = 0; i < header.value_reference_count; ++i) {
        const DSL_IR_VALUE_RECORD &referenced =
            view->values[view->value_references[i].value_id - 1];
        if ((referenced.flags & DSL_IR_VALUE_FLAG_REDIRECTED) != 0)
            return DSL_IR_Image_Report
                       (diagnostic, "reference to redirected value", i + 1);
    }

    return TRUE;
}

BOOL
DSL_IR_Image_Redirect_And_Retire_Value
        (DSL_IR_VALUE_ID replacement_value_id,
         DSL_IR_VALUE_ID retiring_value_id,
         UINT32 replacement_operand_ordinal)
{
    DSL_IR_VALUE_RECORD replacement;
    DSL_IR_VALUE_RECORD retiring;
    if (!DSL_IR_Table_Get
            (DSL_ir_value_table, replacement_value_id, &replacement) ||
        !DSL_IR_Table_Get
            (DSL_ir_value_table, retiring_value_id, &retiring) ||
        replacement_value_id == retiring_value_id ||
        replacement.ty != retiring.ty ||
        retiring.producer_node_id == DSL_IR_NODE_INVALID_ID ||
        retiring.producer_node_id > DSL_ir_node_table.Size() ||
        (retiring.flags & DSL_IR_VALUE_FLAG_REDIRECTED) != 0)
        return FALSE;

    DSL_IR_NODE_RECORD &node =
        DSL_ir_node_table[retiring.producer_node_id - 1];
    if ((node.flags & DSL_IR_NODE_FLAG_RETIRED) != 0 ||
        node.result_value_id != retiring_value_id ||
        replacement_operand_ordinal >= node.operand_count)
        return FALSE;
    DSL_IR_VALUE_REFERENCE_RECORD &replacement_reference =
        DSL_ir_value_reference_table
            [node.first_operand_reference_id - 1 + replacement_operand_ordinal];
    if (replacement_reference.value_id != replacement_value_id)
        return FALSE;

    for (UINT32 i = 0; i < DSL_ir_value_reference_table.Size(); ++i) {
        const DSL_IR_VALUE_REFERENCE_RECORD &reference =
            DSL_ir_value_reference_table[i];
        if (reference.value_id == retiring_value_id &&
            reference.owner_node_id == node.id)
            return FALSE;
    }

    for (UINT32 i = 0; i < DSL_ir_value_reference_table.Size(); ++i) {
        DSL_IR_VALUE_REFERENCE_RECORD &reference =
            DSL_ir_value_reference_table[i];
        if (reference.value_id == retiring_value_id)
            reference.value_id = replacement_value_id;
    }
    node.flags = DSL_IR_NODE_FLAG_RETIRED |
        (replacement_operand_ordinal << DSL_IR_NODE_REDIRECT_ORDINAL_SHIFT);
    DSL_ir_value_table[retiring_value_id - 1].flags |=
        DSL_IR_VALUE_FLAG_REDIRECTED;
    return TRUE;
}

BOOL
DSL_IR_Image_Value_Redirect_Target
        (DSL_IR_VALUE_ID value_id, DSL_IR_VALUE_ID *target_value_id)
{
    DSL_IR_VALUE_RECORD value;
    if (target_value_id == NULL ||
        !DSL_IR_Table_Get(DSL_ir_value_table, value_id, &value) ||
        (value.flags & DSL_IR_VALUE_FLAG_REDIRECTED) == 0 ||
        value.producer_node_id == DSL_IR_NODE_INVALID_ID)
        return FALSE;
    const DSL_IR_NODE_RECORD &node =
        DSL_ir_node_table[value.producer_node_id - 1];
    UINT32 ordinal = (node.flags & DSL_IR_NODE_REDIRECT_ORDINAL_MASK) >>
                     DSL_IR_NODE_REDIRECT_ORDINAL_SHIFT;
    if ((node.flags & DSL_IR_NODE_FLAG_RETIRED) == 0 ||
        ordinal >= node.operand_count)
        return FALSE;
    *target_value_id = DSL_ir_value_reference_table
        [node.first_operand_reference_id - 1 + ordinal].value_id;
    return TRUE;
}

BOOL
DSL_IR_Image_Validate (FILE *diagnostic)
{
    DSL_IR_IMAGE_HEADER header;
    DSL_IR_Image_Get_Header(&header);

    DSL_IR_OPCODE_DESCRIPTOR_RECORD *opcode_descriptors =
        header.opcode_descriptor_count == 0 ? NULL :
        new DSL_IR_OPCODE_DESCRIPTOR_RECORD[header.opcode_descriptor_count];
    DSL_IR_NODE_RECORD *nodes = header.node_count == 0 ? NULL :
        new DSL_IR_NODE_RECORD[header.node_count];
    DSL_IR_ATTRIBUTE_RECORD *attributes = header.attribute_count == 0 ? NULL :
        new DSL_IR_ATTRIBUTE_RECORD[header.attribute_count];
    DSL_IR_VALUE_RECORD *values = header.value_count == 0 ? NULL :
        new DSL_IR_VALUE_RECORD[header.value_count];
    DSL_IR_VALUE_REFERENCE_RECORD *value_references =
        header.value_reference_count == 0 ? NULL :
        new DSL_IR_VALUE_REFERENCE_RECORD[header.value_reference_count];

    for (UINT32 i = 0; i < header.opcode_descriptor_count; ++i)
        opcode_descriptors[i] = DSL_ir_opcode_descriptor_table[i];
    for (UINT32 i = 0; i < header.node_count; ++i)
        nodes[i] = DSL_ir_node_table[i];
    for (UINT32 i = 0; i < header.attribute_count; ++i)
        attributes[i] = DSL_ir_attribute_table[i];
    for (UINT32 i = 0; i < header.value_count; ++i)
        values[i] = DSL_ir_value_table[i];
    for (UINT32 i = 0; i < header.value_reference_count; ++i)
        value_references[i] = DSL_ir_value_reference_table[i];

    DSL_IR_IMAGE_VIEW view;
    view.header = &header;
    view.opcode_descriptors = opcode_descriptors;
    view.nodes = nodes;
    view.attributes = attributes;
    view.values = values;
    view.value_references = value_references;
    BOOL valid = DSL_IR_Image_View_Validate(&view, diagnostic);

    delete [] value_references;
    delete [] values;
    delete [] attributes;
    delete [] nodes;
    delete [] opcode_descriptors;
    return valid;
}

static BOOL
DSL_IR_Image_Add_Section_Size
        (UINT64 *size, UINT32 count, UINT32 record_size)
{
    const UINT64 max_size = (UINT64)-1;
    if (count != 0 && count > (max_size - *size) / record_size)
        return FALSE;
    *size += (UINT64)count * record_size;
    return TRUE;
}

BOOL
DSL_IR_Image_Load_Mapped
        (const void *section_base,
         UINT64 section_size,
         FILE *diagnostic)
{
    if (section_base == NULL || section_size < DSL_IR_IMAGE_HEADER_SIZE)
        return DSL_IR_Image_Report(diagnostic, "section is truncated", 0);

    const char *cursor = (const char *)section_base;
    const DSL_IR_IMAGE_HEADER *header =
        (const DSL_IR_IMAGE_HEADER *)cursor;
    UINT64 expected_size = DSL_IR_IMAGE_HEADER_SIZE;

    if (!DSL_IR_Image_Add_Section_Size
            (&expected_size, header->opcode_descriptor_count,
             DSL_IR_OPCODE_DESCRIPTOR_RECORD_SIZE) ||
        !DSL_IR_Image_Add_Section_Size
            (&expected_size, header->node_count, DSL_IR_NODE_RECORD_SIZE) ||
        !DSL_IR_Image_Add_Section_Size
            (&expected_size, header->attribute_count,
             DSL_IR_ATTRIBUTE_RECORD_SIZE) ||
        !DSL_IR_Image_Add_Section_Size
            (&expected_size, header->value_count, DSL_IR_VALUE_RECORD_SIZE) ||
        !DSL_IR_Image_Add_Section_Size
            (&expected_size, header->value_reference_count,
             DSL_IR_VALUE_REFERENCE_RECORD_SIZE) ||
        expected_size != section_size)
        return DSL_IR_Image_Report(diagnostic, "section size mismatch", 0);

    DSL_IR_IMAGE_VIEW view;
    view.header = header;
    cursor += DSL_IR_IMAGE_HEADER_SIZE;
    view.opcode_descriptors =
        (const DSL_IR_OPCODE_DESCRIPTOR_RECORD *)cursor;
    cursor += (UINT64)header->opcode_descriptor_count *
              DSL_IR_OPCODE_DESCRIPTOR_RECORD_SIZE;
    view.nodes = (const DSL_IR_NODE_RECORD *)cursor;
    cursor += (UINT64)header->node_count * DSL_IR_NODE_RECORD_SIZE;
    view.attributes = (const DSL_IR_ATTRIBUTE_RECORD *)cursor;
    cursor += (UINT64)header->attribute_count * DSL_IR_ATTRIBUTE_RECORD_SIZE;
    view.values = (const DSL_IR_VALUE_RECORD *)cursor;
    cursor += (UINT64)header->value_count * DSL_IR_VALUE_RECORD_SIZE;
    view.value_references = (const DSL_IR_VALUE_REFERENCE_RECORD *)cursor;

    if (!DSL_IR_Image_View_Validate(&view, diagnostic))
        return FALSE;

    DSL_IR_Image_Reset();
    if (header->opcode_descriptor_count != 0)
        DSL_ir_opcode_descriptor_table.Insert
            (view.opcode_descriptors,
             header->opcode_descriptor_count);
    if (header->node_count != 0)
        DSL_ir_node_table.Insert(view.nodes, header->node_count);
    if (header->attribute_count != 0)
        DSL_ir_attribute_table.Insert
            (view.attributes,
             header->attribute_count);
    if (header->value_count != 0)
        DSL_ir_value_table.Insert(view.values, header->value_count);
    if (header->value_reference_count != 0)
        DSL_ir_value_reference_table.Insert
            (view.value_references,
             header->value_reference_count);
    return TRUE;
}

void
DSL_IR_Opcode_Descriptor_Record_Init
        (DSL_IR_OPCODE_DESCRIPTOR_RECORD *record)
{
    DSL_IR_Record_Init (record);
    if (record != NULL)
        record->operand_count = -1;
}

void
DSL_IR_Node_Record_Init (DSL_IR_NODE_RECORD *record)
{
    DSL_IR_Record_Init (record);
}

void
DSL_IR_Attribute_Record_Init (DSL_IR_ATTRIBUTE_RECORD *record)
{
    DSL_IR_Record_Init (record);
}

void
DSL_IR_Value_Record_Init (DSL_IR_VALUE_RECORD *record)
{
    DSL_IR_Record_Init (record);
}

void
DSL_IR_Value_Reference_Record_Init
        (DSL_IR_VALUE_REFERENCE_RECORD *record)
{
    DSL_IR_Record_Init (record);
}

DSL_IR_OPCODE_DESCRIPTOR_ID
DSL_IR_Image_Add_Opcode_Descriptor
        (const DSL_IR_OPCODE_DESCRIPTOR_RECORD *record)
{
    if (record == NULL || record->stable_name == STR_IDX_ZERO ||
        record->version == 0)
        return DSL_IR_OPCODE_DESCRIPTOR_INVALID_ID;

    DSL_IR_OPCODE_DESCRIPTOR_RECORD copy = *record;
    UINT32 index = DSL_ir_opcode_descriptor_table.Insert(copy);
    DSL_ir_opcode_descriptor_table[index].id = index + 1;
    return index + 1;
}

DSL_IR_OPCODE_DESCRIPTOR_ID
DSL_IR_Image_Find_Opcode_Descriptor
        (UINT32 logical_operator,
         UINT32 version)
{
    for (UINT32 i = 0; i < DSL_ir_opcode_descriptor_table.Size(); ++i) {
        const DSL_IR_OPCODE_DESCRIPTOR_RECORD &record =
            DSL_ir_opcode_descriptor_table[i];
        if (record.logical_operator == logical_operator &&
            record.version == version)
            return i + 1;
    }

    return DSL_IR_OPCODE_DESCRIPTOR_INVALID_ID;
}

DSL_IR_OPCODE_DESCRIPTOR_ID
DSL_IR_Image_Ensure_Opcode_Descriptor
        (UINT32 logical_operator,
         UINT32 version)
{
    DSL_IR_OPCODE_DESCRIPTOR_ID id =
        DSL_IR_Image_Find_Opcode_Descriptor(logical_operator, version);
    if (id != DSL_IR_OPCODE_DESCRIPTOR_INVALID_ID)
        return id;

    DSL_OPERATOR_INFO info;
    if (!DSL_Operator_Get_Info_Version
             ((DSL_OPERATOR)logical_operator, version, &info))
        return DSL_IR_OPCODE_DESCRIPTOR_INVALID_ID;

    DSL_IR_OPCODE_DESCRIPTOR_RECORD record;
    DSL_IR_Opcode_Descriptor_Record_Init(&record);
    record.logical_operator = logical_operator;
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
    record.attribute_schema =
        Save_Str(info.attribute_schema == NULL ? "" : info.attribute_schema);
    record.diagnostic_prefix =
        Save_Str(info.diagnostic_prefix == NULL ? "" :
                 info.diagnostic_prefix);
    return DSL_IR_Image_Add_Opcode_Descriptor(&record);
}

DSL_IR_NODE_ID
DSL_IR_Image_Add_Node (const DSL_IR_NODE_RECORD *record)
{
    if (record == NULL ||
        record->opcode_descriptor_id == DSL_IR_OPCODE_DESCRIPTOR_INVALID_ID ||
        record->opcode_descriptor_id > DSL_ir_opcode_descriptor_table.Size())
        return DSL_IR_NODE_INVALID_ID;

    DSL_IR_NODE_RECORD copy = *record;
    UINT32 index = DSL_ir_node_table.Insert(copy);
    DSL_ir_node_table[index].id = index + 1;
    return index + 1;
}

DSL_IR_ATTRIBUTE_ID
DSL_IR_Image_Add_Attribute (const DSL_IR_ATTRIBUTE_RECORD *record)
{
    if (record == NULL || record->name == STR_IDX_ZERO ||
        record->value_kind == DSL_IR_ATTRIBUTE_VALUE_UNKNOWN ||
        record->owner_node_id == DSL_IR_NODE_INVALID_ID ||
        record->owner_node_id > DSL_ir_node_table.Size())
        return DSL_IR_ATTRIBUTE_INVALID_ID;

    DSL_IR_ATTRIBUTE_RECORD copy = *record;
    UINT32 index = DSL_ir_attribute_table.Insert(copy);
    DSL_ir_attribute_table[index].id = index + 1;
    return index + 1;
}

DSL_IR_VALUE_ID
DSL_IR_Image_Add_Value (const DSL_IR_VALUE_RECORD *record)
{
    if (record == NULL || record->value_kind == DSL_IR_VALUE_UNKNOWN ||
        (record->producer_node_id != DSL_IR_NODE_INVALID_ID &&
         record->producer_node_id > DSL_ir_node_table.Size()))
        return DSL_IR_VALUE_INVALID_ID;

    DSL_IR_VALUE_RECORD copy = *record;
    UINT32 index = DSL_ir_value_table.Insert(copy);
    DSL_ir_value_table[index].id = index + 1;
    return index + 1;
}

DSL_IR_VALUE_REFERENCE_ID
DSL_IR_Image_Add_Value_Reference
        (const DSL_IR_VALUE_REFERENCE_RECORD *record)
{
    if (record == NULL ||
        record->owner_node_id == DSL_IR_NODE_INVALID_ID ||
        record->owner_node_id > DSL_ir_node_table.Size() ||
        record->value_id == DSL_IR_VALUE_INVALID_ID ||
        record->value_id > DSL_ir_value_table.Size())
        return DSL_IR_VALUE_REFERENCE_INVALID_ID;

    DSL_IR_VALUE_REFERENCE_RECORD copy = *record;
    UINT32 index = DSL_ir_value_reference_table.Insert(copy);
    DSL_ir_value_reference_table[index].id = index + 1;
    return index + 1;
}

BOOL
DSL_IR_Image_Set_Node_Links
        (DSL_IR_NODE_ID node_id,
         DSL_IR_VALUE_REFERENCE_ID first_operand_id,
         UINT32 operand_count,
         DSL_IR_ATTRIBUTE_ID first_attribute_id,
         UINT32 attribute_count,
         DSL_IR_VALUE_ID result_value_id)
{
    UINT32 reference_count = DSL_ir_value_reference_table.Size();
    UINT32 current_attribute_count = DSL_ir_attribute_table.Size();

    if (node_id == DSL_IR_NODE_INVALID_ID ||
        node_id > DSL_ir_node_table.Size() ||
        (operand_count == 0 &&
         first_operand_id != DSL_IR_VALUE_REFERENCE_INVALID_ID) ||
        (operand_count != 0 &&
         (first_operand_id == DSL_IR_VALUE_REFERENCE_INVALID_ID ||
          operand_count > reference_count ||
          first_operand_id > reference_count - operand_count + 1)) ||
        (attribute_count == 0 &&
         first_attribute_id != DSL_IR_ATTRIBUTE_INVALID_ID) ||
        (attribute_count != 0 &&
         (first_attribute_id == DSL_IR_ATTRIBUTE_INVALID_ID ||
          attribute_count > current_attribute_count ||
          first_attribute_id >
              current_attribute_count - attribute_count + 1)) ||
        result_value_id == DSL_IR_VALUE_INVALID_ID ||
        result_value_id > DSL_ir_value_table.Size())
        return FALSE;

    const DSL_IR_NODE_RECORD &current_node =
        DSL_ir_node_table[node_id - 1];
    const DSL_IR_OPCODE_DESCRIPTOR_RECORD &descriptor =
        DSL_ir_opcode_descriptor_table[current_node.opcode_descriptor_id - 1];
    if (descriptor.operand_count >= 0 &&
        (UINT32)descriptor.operand_count != operand_count)
        return FALSE;

    for (UINT32 i = 0; i < operand_count; ++i) {
        const DSL_IR_VALUE_REFERENCE_RECORD &reference =
            DSL_ir_value_reference_table[first_operand_id - 1 + i];
        if (reference.owner_node_id != node_id || reference.ordinal != i)
            return FALSE;
    }

    for (UINT32 i = 0; i < attribute_count; ++i) {
        const DSL_IR_ATTRIBUTE_RECORD &attribute =
            DSL_ir_attribute_table[first_attribute_id - 1 + i];
        if (attribute.owner_node_id != node_id)
            return FALSE;
    }

    const DSL_IR_VALUE_RECORD &result =
        DSL_ir_value_table[result_value_id - 1];
    if (result.producer_node_id != node_id)
        return FALSE;

    DSL_IR_NODE_RECORD &node = DSL_ir_node_table[node_id - 1];
    node.first_operand_reference_id = first_operand_id;
    node.operand_count = operand_count;
    node.first_attribute_id = first_attribute_id;
    node.attribute_count = attribute_count;
    node.result_value_id = result_value_id;
    return TRUE;
}

BOOL
DSL_IR_Image_Rewrite_Node
        (const DSL_IR_NODE_REWRITE_REQUEST *request)
{
    if (request == NULL ||
        request->node_id == DSL_IR_NODE_INVALID_ID ||
        request->node_id > DSL_ir_node_table.Size() ||
        request->opcode_descriptor_id ==
            DSL_IR_OPCODE_DESCRIPTOR_INVALID_ID ||
        request->opcode_descriptor_id >
            DSL_ir_opcode_descriptor_table.Size() ||
        (request->operand_count != 0 &&
         request->operand_value_ids == NULL) ||
        (request->attribute_count != 0 &&
         request->attributes == NULL) ||
        request->result_value_kind == DSL_IR_VALUE_UNKNOWN)
        return FALSE;

    const DSL_IR_OPCODE_DESCRIPTOR_RECORD &descriptor =
        DSL_ir_opcode_descriptor_table[request->opcode_descriptor_id - 1];
    if (descriptor.operand_count >= 0 &&
        (UINT32)descriptor.operand_count != request->operand_count)
        return FALSE;

    DSL_IR_NODE_RECORD &node =
        DSL_ir_node_table[request->node_id - 1];
    if (node.result_value_id == DSL_IR_VALUE_INVALID_ID ||
        node.result_value_id > DSL_ir_value_table.Size())
        return FALSE;
    DSL_IR_VALUE_RECORD &result =
        DSL_ir_value_table[node.result_value_id - 1];
    if (result.producer_node_id != request->node_id)
        return FALSE;

    for (UINT32 i = 0; i < request->operand_count; ++i) {
        if (request->operand_value_ids[i] == DSL_IR_VALUE_INVALID_ID ||
            request->operand_value_ids[i] > DSL_ir_value_table.Size())
            return FALSE;
    }
    for (UINT32 i = 0; i < request->attribute_count; ++i) {
        const DSL_IR_ATTRIBUTE_RECORD &attribute = request->attributes[i];
        if (attribute.name == STR_IDX_ZERO ||
            attribute.value_kind == DSL_IR_ATTRIBUTE_VALUE_UNKNOWN)
            return FALSE;
    }

    std::vector<DSL_IR_VALUE_REFERENCE_RECORD> references;
    std::vector<DSL_IR_ATTRIBUTE_RECORD> attributes;
    std::vector<DSL_IR_VALUE_REFERENCE_ID> first_operand_ids;
    std::vector<DSL_IR_ATTRIBUTE_ID> first_attribute_ids;
    UINT32 node_count = DSL_ir_node_table.Size();

    references.reserve(DSL_ir_value_reference_table.Size() +
                       request->operand_count);
    attributes.reserve(DSL_ir_attribute_table.Size() +
                       request->attribute_count);
    first_operand_ids.resize(node_count,
                             DSL_IR_VALUE_REFERENCE_INVALID_ID);
    first_attribute_ids.resize(node_count, DSL_IR_ATTRIBUTE_INVALID_ID);

    for (UINT32 node_index = 0; node_index < node_count; ++node_index) {
        const DSL_IR_NODE_RECORD &current = DSL_ir_node_table[node_index];
        UINT32 operand_count = current.operand_count;
        UINT32 attribute_count = current.attribute_count;

        if (current.id == request->node_id) {
            operand_count = request->operand_count;
            attribute_count = request->attribute_count;
        }

        if (operand_count != 0)
            first_operand_ids[node_index] = references.size() + 1;
        for (UINT32 i = 0; i < operand_count; ++i) {
            DSL_IR_VALUE_REFERENCE_RECORD reference;
            if (current.id == request->node_id) {
                DSL_IR_Value_Reference_Record_Init(&reference);
                reference.owner_node_id = request->node_id;
                reference.ordinal = i;
                reference.value_id = request->operand_value_ids[i];
            } else {
                reference = DSL_ir_value_reference_table
                    [current.first_operand_reference_id - 1 + i];
            }
            reference.id = references.size() + 1;
            references.push_back(reference);
        }

        if (attribute_count != 0)
            first_attribute_ids[node_index] = attributes.size() + 1;
        for (UINT32 i = 0; i < attribute_count; ++i) {
            DSL_IR_ATTRIBUTE_RECORD attribute;
            if (current.id == request->node_id) {
                attribute = request->attributes[i];
                attribute.owner_node_id = request->node_id;
            } else {
                attribute = DSL_ir_attribute_table
                    [current.first_attribute_id - 1 + i];
            }
            attribute.id = attributes.size() + 1;
            attributes.push_back(attribute);
        }
    }

    DSL_ir_value_reference_table.Delete_down_to(0);
    if (!references.empty())
        DSL_ir_value_reference_table.Insert(&references[0],
                                            references.size());
    DSL_ir_attribute_table.Delete_down_to(0);
    if (!attributes.empty())
        DSL_ir_attribute_table.Insert(&attributes[0], attributes.size());

    for (UINT32 node_index = 0; node_index < node_count; ++node_index) {
        DSL_IR_NODE_RECORD &current = DSL_ir_node_table[node_index];
        current.first_operand_reference_id = first_operand_ids[node_index];
        current.first_attribute_id = first_attribute_ids[node_index];
    }

    node.opcode_descriptor_id = request->opcode_descriptor_id;
    node.operand_count = request->operand_count;
    node.attribute_count = request->attribute_count;
    node.payload = request->payload;
    result.value_kind = request->result_value_kind;
    return TRUE;
}

BOOL
DSL_IR_Image_Find_PU_Value
        (ST_IDX st,
         const char *name,
         const char *owner_pu,
         DSL_IR_VALUE_RECORD *record)
{
    DSL_IR_VALUE_ID found = DSL_IR_VALUE_INVALID_ID;
    std::string owner_metadata;

    if (ST_IDX_index(st) == 0 || name == NULL)
        return FALSE;
    if (owner_pu != NULL) {
        owner_metadata = "owner_pu=";
        owner_metadata += owner_pu;
    }
    for (UINT32 i = 0; i < DSL_ir_value_table.Size(); ++i) {
        const DSL_IR_VALUE_RECORD &value = DSL_ir_value_table[i];
        if (value.st != st || value.name == STR_IDX_ZERO ||
            strcmp(Index_To_Str(value.name), name) != 0)
            continue;
        if (owner_pu != NULL &&
            (value.metadata == STR_IDX_ZERO ||
             owner_metadata != Index_To_Str(value.metadata)))
            continue;
        if (found != DSL_IR_VALUE_INVALID_ID)
            return FALSE;
        found = i + 1;
    }
    return found != DSL_IR_VALUE_INVALID_ID &&
           DSL_IR_Image_Get_Value(found, record);
}

BOOL
DSL_IR_Image_Find_Value
        (ST_IDX st,
         const char *name,
         DSL_IR_VALUE_RECORD *record)
{
    return DSL_IR_Image_Find_PU_Value(st, name, NULL, record);
}

UINT32
DSL_IR_Image_Opcode_Descriptor_Count (void)
{
    return DSL_ir_opcode_descriptor_table.Size();
}

UINT32
DSL_IR_Image_Node_Count (void)
{
    return DSL_ir_node_table.Size();
}

UINT32
DSL_IR_Image_Executable_Node_Count (void)
{
    UINT32 count = 0;
    for (UINT32 i = 0; i < DSL_ir_node_table.Size(); ++i) {
        if ((DSL_ir_node_table[i].flags & DSL_IR_NODE_FLAG_RETIRED) == 0)
            ++count;
    }
    return count;
}

UINT32
DSL_IR_Image_Attribute_Count (void)
{
    return DSL_ir_attribute_table.Size();
}

UINT32
DSL_IR_Image_Value_Count (void)
{
    return DSL_ir_value_table.Size();
}

UINT32
DSL_IR_Image_Value_Reference_Count (void)
{
    return DSL_ir_value_reference_table.Size();
}

BOOL
DSL_IR_Image_Get_Opcode_Descriptor
        (DSL_IR_OPCODE_DESCRIPTOR_ID id,
         DSL_IR_OPCODE_DESCRIPTOR_RECORD *record)
{
    return DSL_IR_Table_Get (DSL_ir_opcode_descriptor_table, id, record);
}

BOOL
DSL_IR_Image_Get_Node (DSL_IR_NODE_ID id, DSL_IR_NODE_RECORD *record)
{
    return DSL_IR_Table_Get (DSL_ir_node_table, id, record);
}

BOOL
DSL_IR_Image_Get_Attribute
        (DSL_IR_ATTRIBUTE_ID id,
         DSL_IR_ATTRIBUTE_RECORD *record)
{
    return DSL_IR_Table_Get (DSL_ir_attribute_table, id, record);
}

BOOL
DSL_IR_Image_Get_Value (DSL_IR_VALUE_ID id, DSL_IR_VALUE_RECORD *record)
{
    return DSL_IR_Table_Get (DSL_ir_value_table, id, record);
}

BOOL
DSL_IR_Image_Get_Value_Reference
        (DSL_IR_VALUE_REFERENCE_ID id,
         DSL_IR_VALUE_REFERENCE_RECORD *record)
{
    return DSL_IR_Table_Get (DSL_ir_value_reference_table, id, record);
}

void
DSL_Effect_Image_Get_Header (DSL_EFFECT_IMAGE_HEADER *header)
{
    if (header == NULL)
        return;
    memset (header, 0, sizeof(*header));
    header->magic = DSL_EFFECT_IMAGE_MAGIC;
    header->version = DSL_EFFECT_IMAGE_VERSION;
    header->state_object_count = DSL_state_object_table.Size();
    header->state_effect_count = DSL_state_effect_table.Size();
}

void
DSL_Effect_Image_Reset (void)
{
    DSL_state_object_table.Delete_down_to(0);
    DSL_state_effect_table.Delete_down_to(0);
}

BOOL
DSL_Effect_Image_Has_Records (void)
{
    return DSL_state_object_table.Size() != 0 ||
           DSL_state_effect_table.Size() != 0;
}

static BOOL
DSL_Effect_Image_Report (FILE *diagnostic, const char *message, UINT32 id)
{
    if (diagnostic != NULL)
        fprintf (diagnostic, "DSL effect image error: %s id=%u\n",
                 message, id);
    return FALSE;
}

static BOOL
DSL_Effect_Image_Valid_State_Kind (UINT32 kind)
{
    return kind >= DSL_STATE_KIND_RUNTIME_STATUS &&
           kind <= DSL_STATE_KIND_OPAQUE;
}

static BOOL
DSL_Effect_Image_Valid_Effect_Kind (UINT32 kind)
{
    return kind == DSL_STATE_EFFECT_READ ||
           kind == DSL_STATE_EFFECT_MODIFY;
}

BOOL
DSL_Effect_Image_Validate (FILE *diagnostic)
{
    for (UINT32 i = 0; i < DSL_state_object_table.Size(); ++i) {
        const DSL_STATE_OBJECT_RECORD &record = DSL_state_object_table[i];
        if (record.id != i + 1 ||
            !DSL_Effect_Image_Valid_State_Kind(record.kind) ||
            ST_IDX_index(record.owner_pu_st) == 0 ||
            ST_IDX_index(record.st) == 0 ||
            !DSL_IR_Image_String_Id_Valid(record.name, TRUE) ||
            (record.flags & ~DSL_STATE_OBJECT_UNIQUE_OWNERSHIP) != 0 ||
            record.reserved0 != 0 || record.reserved1 != 0)
            return DSL_Effect_Image_Report
                       (diagnostic, "invalid state object", i + 1);
        for (UINT32 j = 0; j < i; ++j) {
            const DSL_STATE_OBJECT_RECORD &previous =
                DSL_state_object_table[j];
            if (previous.owner_pu_st == record.owner_pu_st &&
                (previous.st == record.st || previous.name == record.name))
                return DSL_Effect_Image_Report
                           (diagnostic, "duplicate state object", i + 1);
        }
    }

    for (UINT32 i = 0; i < DSL_state_effect_table.Size(); ++i) {
        const DSL_STATE_EFFECT_RECORD &record = DSL_state_effect_table[i];
        UINT32 expected_ordinal = 0;
        if (record.id != i + 1 || record.owner_node_id == 0 ||
            record.owner_node_id > DSL_ir_node_table.Size() ||
            record.state_object_id == 0 ||
            record.state_object_id > DSL_state_object_table.Size() ||
            !DSL_Effect_Image_Valid_Effect_Kind(record.effect_kind))
            return DSL_Effect_Image_Report
                       (diagnostic, "invalid state effect", i + 1);
        for (UINT32 j = 0; j < i; ++j) {
            const DSL_STATE_EFFECT_RECORD &previous =
                DSL_state_effect_table[j];
            if (previous.owner_node_id == record.owner_node_id) {
                ++expected_ordinal;
                if (previous.state_object_id == record.state_object_id)
                    return DSL_Effect_Image_Report
                               (diagnostic, "duplicate node-state effect",
                                i + 1);
            }
        }
        if (record.ordinal != expected_ordinal)
            return DSL_Effect_Image_Report
                       (diagnostic, "invalid state-effect ordinal", i + 1);
        const DSL_IR_NODE_RECORD &node =
            DSL_ir_node_table[record.owner_node_id - 1];
        const DSL_IR_OPCODE_DESCRIPTOR_RECORD &descriptor =
            DSL_ir_opcode_descriptor_table[node.opcode_descriptor_id - 1];
        if (descriptor.effect_model == DSL_EFFECT_MODEL_PURE)
            return DSL_Effect_Image_Report
                       (diagnostic, "pure node has state effect", i + 1);
    }
    return TRUE;
}

BOOL
DSL_Effect_Image_Load_Mapped
        (const void *section_base,
         UINT64 section_size,
         FILE *diagnostic)
{
    if (section_base == NULL || section_size < DSL_EFFECT_IMAGE_HEADER_SIZE)
        return DSL_Effect_Image_Report
                   (diagnostic, "section is truncated", 0);

    const char *cursor = (const char *)section_base;
    const DSL_EFFECT_IMAGE_HEADER *header =
        (const DSL_EFFECT_IMAGE_HEADER *)cursor;
    UINT64 expected_size = DSL_EFFECT_IMAGE_HEADER_SIZE;
    if (header->magic != DSL_EFFECT_IMAGE_MAGIC ||
        header->version != DSL_EFFECT_IMAGE_VERSION || header->flags != 0 ||
        header->reserved != 0 ||
        !DSL_IR_Image_Add_Section_Size
             (&expected_size, header->state_object_count,
              DSL_STATE_OBJECT_RECORD_SIZE) ||
        !DSL_IR_Image_Add_Section_Size
             (&expected_size, header->state_effect_count,
              DSL_STATE_EFFECT_RECORD_SIZE) ||
        expected_size != section_size)
        return DSL_Effect_Image_Report(diagnostic, "invalid header", 0);

    cursor += DSL_EFFECT_IMAGE_HEADER_SIZE;
    const DSL_STATE_OBJECT_RECORD *states =
        (const DSL_STATE_OBJECT_RECORD *)cursor;
    cursor += (UINT64)header->state_object_count *
              DSL_STATE_OBJECT_RECORD_SIZE;
    const DSL_STATE_EFFECT_RECORD *effects =
        (const DSL_STATE_EFFECT_RECORD *)cursor;

    DSL_state_object_table.Delete_down_to(0);
    DSL_state_effect_table.Delete_down_to(0);
    if (header->state_object_count != 0)
        DSL_state_object_table.Insert(states, header->state_object_count);
    if (header->state_effect_count != 0)
        DSL_state_effect_table.Insert(effects, header->state_effect_count);
    if (!DSL_Effect_Image_Validate(diagnostic)) {
        DSL_state_object_table.Delete_down_to(0);
        DSL_state_effect_table.Delete_down_to(0);
        return FALSE;
    }
    return TRUE;
}

void
DSL_State_Object_Record_Init (DSL_STATE_OBJECT_RECORD *record)
{
    DSL_IR_Record_Init(record);
}

void
DSL_State_Effect_Record_Init (DSL_STATE_EFFECT_RECORD *record)
{
    DSL_IR_Record_Init(record);
}

DSL_STATE_OBJECT_ID
DSL_Effect_Image_Add_State_Object
        (const DSL_STATE_OBJECT_RECORD *record)
{
    if (record == NULL || record->name == STR_IDX_ZERO ||
        !DSL_Effect_Image_Valid_State_Kind(record->kind) ||
        ST_IDX_index(record->owner_pu_st) == 0 ||
        ST_IDX_index(record->st) == 0 ||
        (record->flags & ~DSL_STATE_OBJECT_UNIQUE_OWNERSHIP) != 0)
        return DSL_STATE_OBJECT_INVALID_ID;
    DSL_STATE_OBJECT_RECORD copy = *record;
    UINT32 index = DSL_state_object_table.Insert(copy);
    DSL_state_object_table[index].id = index + 1;
    if (!DSL_Effect_Image_Validate(NULL)) {
        DSL_state_object_table.Delete_down_to(index);
        return DSL_STATE_OBJECT_INVALID_ID;
    }
    return index + 1;
}

DSL_STATE_EFFECT_ID
DSL_Effect_Image_Add_State_Effect
        (const DSL_STATE_EFFECT_RECORD *record)
{
    if (record == NULL || record->owner_node_id == 0 ||
        record->owner_node_id > DSL_ir_node_table.Size() ||
        record->state_object_id == 0 ||
        record->state_object_id > DSL_state_object_table.Size() ||
        !DSL_Effect_Image_Valid_Effect_Kind(record->effect_kind))
        return DSL_STATE_EFFECT_INVALID_ID;
    DSL_STATE_EFFECT_RECORD copy = *record;
    UINT32 index = DSL_state_effect_table.Insert(copy);
    DSL_state_effect_table[index].id = index + 1;
    if (!DSL_Effect_Image_Validate(NULL)) {
        DSL_state_effect_table.Delete_down_to(index);
        return DSL_STATE_EFFECT_INVALID_ID;
    }
    return index + 1;
}

UINT32
DSL_Effect_Image_State_Object_Count (void)
{
    return DSL_state_object_table.Size();
}

UINT32
DSL_Effect_Image_State_Effect_Count (void)
{
    return DSL_state_effect_table.Size();
}

BOOL
DSL_Effect_Image_Get_State_Object
        (DSL_STATE_OBJECT_ID id,
         DSL_STATE_OBJECT_RECORD *record)
{
    return DSL_IR_Table_Get(DSL_state_object_table, id, record);
}

BOOL
DSL_Effect_Image_Get_State_Effect
        (DSL_STATE_EFFECT_ID id,
         DSL_STATE_EFFECT_RECORD *record)
{
    return DSL_IR_Table_Get(DSL_state_effect_table, id, record);
}

const char *
DSL_State_Kind_Name (DSL_STATE_KIND kind)
{
    static const char *names[] = {
        "unknown", "runtime_status", "random", "mutable_buffer",
        "communication", "opaque"
    };
    UINT32 index = (UINT32)kind;
    return index < sizeof(names) / sizeof(names[0]) ?
           names[index] : names[0];
}

const char *
DSL_State_Effect_Kind_Name (DSL_STATE_EFFECT_KIND effect_kind)
{
    static const char *names[] = { "unknown", "read", "modify" };
    UINT32 index = (UINT32)effect_kind;
    return index < sizeof(names) / sizeof(names[0]) ?
           names[index] : names[0];
}
