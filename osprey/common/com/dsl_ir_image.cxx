/*
 * Copyright (C) 2026 Open64 Project
 */

#include <string.h>
#include <string>
#include <vector>

#include "dsl_ir_image.h"
#include "dsl_opcode.h"
#include "dsl_program_interface_internal.h"
#include "segmented_array.h"
#include "strtab.h"

extern BOOL DSL_Runtime_Interface_Value_Record_Contract_Valid
                                (const DSL_RUNTIME_VALUE_PROJECTION_RECORD *);
extern BOOL DSL_Program_Interface_Pointer_TY_Contract_Valid (TY_IDX);
extern BOOL DSL_Program_Interface_Runtime_Input_Contract_Valid
                                (const DSL_RUNTIME_INPUT_RECORD *);

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
typedef SEGMENTED_ARRAY<DSL_RUNTIME_VALUE_PROJECTION_RECORD>
    DSL_RUNTIME_VALUE_PROJECTION_TABLE;
typedef SEGMENTED_ARRAY<DSL_RUNTIME_CALL_PROJECTION_RECORD>
    DSL_RUNTIME_CALL_PROJECTION_TABLE;
typedef SEGMENTED_ARRAY<DSL_RETIRED_FORMAL_RECORD>
    DSL_RETIRED_FORMAL_TABLE;
typedef SEGMENTED_ARRAY<DSL_RETIRED_CALL_ARGUMENT_RECORD>
    DSL_RETIRED_CALL_ARGUMENT_TABLE;
typedef SEGMENTED_ARRAY<DSL_RUNTIME_INPUT_RECORD> DSL_RUNTIME_INPUT_TABLE;
typedef SEGMENTED_ARRAY<DSL_RUNTIME_INPUT_BINDING_RECORD>
    DSL_RUNTIME_INPUT_BINDING_TABLE;
typedef SEGMENTED_ARRAY<DSL_RUNTIME_INPUT_CALL_RECORD>
    DSL_RUNTIME_INPUT_CALL_TABLE;

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
static DSL_RUNTIME_VALUE_PROJECTION_TABLE DSL_runtime_value_projection_table;
static DSL_RUNTIME_CALL_PROJECTION_TABLE DSL_runtime_call_projection_table;
static DSL_RETIRED_FORMAL_TABLE DSL_retired_formal_table;
static DSL_RETIRED_CALL_ARGUMENT_TABLE DSL_retired_call_argument_table;
static DSL_RUNTIME_INPUT_TABLE DSL_runtime_input_table;
static DSL_RUNTIME_INPUT_BINDING_TABLE DSL_runtime_input_binding_table;
static DSL_RUNTIME_INPUT_CALL_TABLE DSL_runtime_input_call_table;

typedef struct {
    ST_IDX owner_pu_st;
    WN *call;
    DSL_CALLSITE_METADATA_ID id;
} DSL_CALLSITE_RUNTIME_ASSOCIATION;

static std::vector<DSL_CALLSITE_RUNTIME_ASSOCIATION>
    DSL_callsite_runtime_associations;

static BOOL DSL_IR_Image_String_Id_Valid (STR_IDX id, BOOL required);
static BOOL DSL_IR_Image_Report (FILE *diagnostic,
                                 const char *message,
                                 UINT32 id);

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
typedef char DSL_Runtime_Interface_Header_Size_Check
    [sizeof(DSL_RUNTIME_INTERFACE_IMAGE_HEADER) ==
        DSL_RUNTIME_INTERFACE_IMAGE_HEADER_SIZE ? 1 : -1];
typedef char DSL_Runtime_Value_Projection_Size_Check
    [sizeof(DSL_RUNTIME_VALUE_PROJECTION_RECORD) ==
        DSL_RUNTIME_VALUE_PROJECTION_RECORD_SIZE ? 1 : -1];
typedef char DSL_Runtime_Call_Projection_Size_Check
    [sizeof(DSL_RUNTIME_CALL_PROJECTION_RECORD) ==
        DSL_RUNTIME_CALL_PROJECTION_RECORD_SIZE ? 1 : -1];
typedef char DSL_Program_Interface_Header_Size_Check
    [sizeof(DSL_PROGRAM_INTERFACE_IMAGE_HEADER) ==
        DSL_PROGRAM_INTERFACE_IMAGE_HEADER_SIZE ? 1 : -1];
typedef char DSL_Retired_Formal_Size_Check
    [sizeof(DSL_RETIRED_FORMAL_RECORD) ==
        DSL_RETIRED_FORMAL_RECORD_SIZE ? 1 : -1];
typedef char DSL_Retired_Call_Argument_Size_Check
    [sizeof(DSL_RETIRED_CALL_ARGUMENT_RECORD) ==
        DSL_RETIRED_CALL_ARGUMENT_RECORD_SIZE ? 1 : -1];
typedef char DSL_Runtime_Input_Size_Check
    [sizeof(DSL_RUNTIME_INPUT_RECORD) ==
        DSL_RUNTIME_INPUT_RECORD_SIZE ? 1 : -1];
typedef char DSL_Runtime_Input_Binding_Size_Check
    [sizeof(DSL_RUNTIME_INPUT_BINDING_RECORD) ==
        DSL_RUNTIME_INPUT_BINDING_RECORD_SIZE ? 1 : -1];
typedef char DSL_Runtime_Input_Call_Size_Check
    [sizeof(DSL_RUNTIME_INPUT_CALL_RECORD) ==
        DSL_RUNTIME_INPUT_CALL_RECORD_SIZE ? 1 : -1];

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
    DSL_Runtime_Interface_Image_Reset();
    DSL_Program_Interface_Image_Reset();
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

BOOL
DSL_Call_Image_Replace_Call_WN
        (DSL_CALLSITE_METADATA_ID id, const WN *expected, WN *replacement)
{
    if (id == DSL_CALLSITE_METADATA_INVALID_ID || expected == NULL ||
        replacement == NULL)
        return FALSE;
    for (UINT32 i = 0; i < DSL_callsite_runtime_associations.size(); ++i) {
        DSL_CALLSITE_RUNTIME_ASSOCIATION &association =
            DSL_callsite_runtime_associations[i];
        if (association.id == id && association.call == expected) {
            association.call = replacement;
            return TRUE;
        }
    }
    return FALSE;
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

static BOOL
DSL_Runtime_Interface_Report
        (FILE *diagnostic, const char *message, UINT32 id)
{
    if (diagnostic != NULL)
        fprintf(diagnostic, "DSL runtime interface image error: %s id=%u\n",
                message, id);
    return FALSE;
}

static const DSL_RUNTIME_VALUE_PROJECTION_RECORD *
DSL_Runtime_Interface_Find_Value_In_View
        (const DSL_RUNTIME_VALUE_PROJECTION_RECORD *values,
         UINT32 value_count, ST_IDX owner_pu_st,
         DSL_IR_VALUE_ID source_value_id)
{
    for (UINT32 i = 0; i < value_count; ++i) {
        if (values[i].owner_pu_st == owner_pu_st &&
            values[i].source_value_id == source_value_id)
            return &values[i];
    }
    return NULL;
}

static BOOL
DSL_Runtime_Interface_View_Validate
        (const DSL_RUNTIME_VALUE_PROJECTION_RECORD *values,
         UINT32 value_count,
         const DSL_RUNTIME_CALL_PROJECTION_RECORD *calls,
         UINT32 call_count,
         const DSL_RETIRED_FORMAL_RECORD *retired_formals,
         UINT32 retired_formal_count, BOOL use_retired_view,
         FILE *diagnostic)
{
    for (UINT32 i = 0; i < value_count; ++i) {
        const DSL_RUNTIME_VALUE_PROJECTION_RECORD &record = values[i];
        DSL_PU_FORMAL_RECORD formal;
        BOOL is_formal = record.binding_kind ==
                             DSL_RUNTIME_BINDING_INPUT_FORMAL ||
                         record.binding_kind ==
                             DSL_RUNTIME_BINDING_RESULT_FORMAL;
        if (record.id != i + 1 ||
            !DSL_Runtime_Interface_Value_Record_Contract_Valid(&record) ||
            record.binding_kind < DSL_RUNTIME_BINDING_LOCAL_VALUE ||
            record.binding_kind > DSL_RUNTIME_BINDING_RESULT_FORMAL ||
            (is_formal && record.formal_ordinal ==
                              DSL_RUNTIME_INTERFACE_INVALID_ORDINAL) ||
            (!is_formal && record.formal_ordinal !=
                               DSL_RUNTIME_INTERFACE_INVALID_ORDINAL) ||
            record.flags != 0 || record.reserved0 != 0 ||
            record.reserved1 != 0)
            return DSL_Runtime_Interface_Report
                       (diagnostic, "invalid value projection", i + 1);
        if (is_formal &&
            (!DSL_PU_Interface_Image_Find_Formal
                 (record.owner_pu_st, record.formal_ordinal, &formal) ||
             formal.formal_value_id != record.source_value_id ||
             formal.formal_st != record.source_st ||
             formal.formal_ty != record.source_ty))
            return DSL_Runtime_Interface_Report
                       (diagnostic, "formal provenance mismatch", i + 1);
        BOOL retired = FALSE;
        if (is_formal && use_retired_view) {
            for (UINT32 retired_id = 0;
                 retired_id < retired_formal_count; ++retired_id) {
                if (retired_formals[retired_id].pu_formal_id == formal.id) {
                    retired = TRUE;
                    break;
                }
            }
        } else if (is_formal) {
            retired = DSL_Program_Interface_Image_Find_Retired_Formal
                          (formal.id, NULL);
        }
        if (retired)
            return DSL_Runtime_Interface_Report
                       (diagnostic, "formal provenance mismatch", i + 1);
        for (UINT32 j = 0; j < i; ++j) {
            if (values[j].owner_pu_st == record.owner_pu_st &&
                (values[j].source_value_id == record.source_value_id ||
                 values[j].handle_st == record.handle_st ||
                 (is_formal &&
                  values[j].formal_ordinal == record.formal_ordinal)))
                return DSL_Runtime_Interface_Report
                           (diagnostic, "duplicate value projection", i + 1);
        }
    }

    for (UINT32 i = 0; i < call_count; ++i) {
        const DSL_RUNTIME_CALL_PROJECTION_RECORD &record = calls[i];
        DSL_CALLSITE_METADATA_RECORD callsite;
        const DSL_RUNTIME_VALUE_PROJECTION_RECORD *value =
            record.value_projection_id == 0 ||
            record.value_projection_id > value_count ? NULL :
            &values[record.value_projection_id - 1];
        DSL_PU_FORMAL_RECORD formal;
        const DSL_RUNTIME_VALUE_PROJECTION_RECORD *formal_projection = NULL;
        if (record.id != i + 1 ||
            record.callsite_id == DSL_CALLSITE_METADATA_INVALID_ID ||
            !DSL_Call_Image_Get_Callsite(record.callsite_id, &callsite) ||
            callsite.owner_pu_st != record.owner_pu_st || value == NULL ||
            value->owner_pu_st != record.owner_pu_st ||
            value->source_value_id != record.source_value_id ||
            record.actual_ordinal == DSL_RUNTIME_INTERFACE_INVALID_ORDINAL ||
            record.callee_formal_ordinal ==
                DSL_RUNTIME_INTERFACE_INVALID_ORDINAL ||
            record.actual_ordinal != record.callee_formal_ordinal ||
            record.direction < DSL_RUNTIME_CALL_INPUT ||
            record.direction > DSL_RUNTIME_CALL_RESULT ||
            record.flags != 0 || record.reserved != 0 ||
            !DSL_PU_Interface_Image_Find_Formal
                 (callsite.callee_pu_st, record.callee_formal_ordinal,
                  &formal))
            return DSL_Runtime_Interface_Report
                       (diagnostic, "invalid call projection", i + 1);
        formal_projection = DSL_Runtime_Interface_Find_Value_In_View
                                (values, value_count,
                                 callsite.callee_pu_st,
                                 formal.formal_value_id);
        if (formal_projection == NULL ||
            formal_projection->formal_ordinal !=
                record.callee_formal_ordinal ||
            formal_projection->handle_ty != value->handle_ty ||
            (record.direction == DSL_RUNTIME_CALL_INPUT &&
             formal_projection->binding_kind !=
                 DSL_RUNTIME_BINDING_INPUT_FORMAL) ||
            (record.direction == DSL_RUNTIME_CALL_RESULT &&
             formal_projection->binding_kind !=
                 DSL_RUNTIME_BINDING_RESULT_FORMAL))
            return DSL_Runtime_Interface_Report
                       (diagnostic, "call/formal projection mismatch", i + 1);
        if (record.direction == DSL_RUNTIME_CALL_INPUT) {
            DSL_CALL_ARGUMENT_RECORD argument;
            if (!DSL_Call_ABI_Image_Find_Argument_By_Id
                    (record.callsite_id, record.actual_ordinal, &argument) ||
                argument.argument_value_id != record.source_value_id ||
                argument.callee_formal_ordinal !=
                    record.callee_formal_ordinal)
                return DSL_Runtime_Interface_Report
                           (diagnostic, "input provenance mismatch", i + 1);
        }
        for (UINT32 j = 0; j < i; ++j) {
            if (calls[j].callsite_id == record.callsite_id &&
                calls[j].actual_ordinal == record.actual_ordinal)
                return DSL_Runtime_Interface_Report
                           (diagnostic, "duplicate call projection", i + 1);
        }
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
            BOOL retired = FALSE;
            if (use_retired_view) {
                for (UINT32 retired_id = 0;
                     retired_id < retired_formal_count; ++retired_id) {
                    if (retired_formals[retired_id].pu_formal_id ==
                        formal.id) {
                        retired = TRUE;
                        break;
                    }
                }
            } else {
                retired = DSL_Program_Interface_Image_Find_Retired_Formal
                              (formal.id, NULL);
            }
            if (retired)
                continue;
            const DSL_RUNTIME_VALUE_PROJECTION_RECORD *projection =
                DSL_Runtime_Interface_Find_Value_In_View
                    (values, value_count, formal.owner_pu_st,
                     formal.formal_value_id);
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
            if (input_relation_count > 1 || projection == NULL ||
                projection->binding_kind != expected_binding)
                return DSL_Runtime_Interface_Report
                           (diagnostic, "formal call role mismatch",
                            formal_id);
            UINT32 found = 0;
            for (UINT32 i = 0; i < call_count; ++i) {
                if (calls[i].callsite_id == callsite.id &&
                    calls[i].callee_formal_ordinal ==
                        formal.formal_ordinal &&
                    calls[i].direction == expected_direction)
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

static BOOL DSL_Program_Interface_Validate_Runtime_View
        (const DSL_RUNTIME_VALUE_PROJECTION_RECORD *values,
         UINT32 value_count,
         const DSL_RUNTIME_CALL_PROJECTION_RECORD *calls,
         UINT32 call_count, FILE *diagnostic);

void
DSL_Runtime_Interface_Image_Get_Header
        (DSL_RUNTIME_INTERFACE_IMAGE_HEADER *header)
{
    if (header == NULL)
        return;
    memset(header, 0, sizeof(*header));
    header->magic = DSL_RUNTIME_INTERFACE_IMAGE_MAGIC;
    header->version = DSL_RUNTIME_INTERFACE_IMAGE_VERSION;
    header->value_projection_count =
        DSL_runtime_value_projection_table.Size();
    header->call_projection_count =
        DSL_runtime_call_projection_table.Size();
}

void
DSL_Runtime_Interface_Image_Reset (void)
{
    DSL_runtime_value_projection_table.Delete_down_to(0);
    DSL_runtime_call_projection_table.Delete_down_to(0);
}

BOOL
DSL_Runtime_Interface_Image_Has_Records (void)
{
    return DSL_runtime_value_projection_table.Size() != 0 ||
           DSL_runtime_call_projection_table.Size() != 0;
}

BOOL
DSL_Runtime_Interface_Image_Validate (FILE *diagnostic)
{
    std::vector<DSL_RUNTIME_VALUE_PROJECTION_RECORD> values;
    std::vector<DSL_RUNTIME_CALL_PROJECTION_RECORD> calls;
    for (UINT32 i = 0; i < DSL_runtime_value_projection_table.Size(); ++i)
        values.push_back(DSL_runtime_value_projection_table[i]);
    for (UINT32 i = 0; i < DSL_runtime_call_projection_table.Size(); ++i)
        calls.push_back(DSL_runtime_call_projection_table[i]);
    return DSL_Runtime_Interface_View_Validate
               (values.empty() ? NULL : &values[0], values.size(),
                calls.empty() ? NULL : &calls[0], calls.size(), NULL, 0,
                FALSE, diagnostic);
}

UINT32
DSL_Runtime_Interface_Image_Value_Count (void)
{
    return DSL_runtime_value_projection_table.Size();
}

UINT32
DSL_Runtime_Interface_Image_Call_Count (void)
{
    return DSL_runtime_call_projection_table.Size();
}

BOOL
DSL_Runtime_Interface_Image_Get_Value
        (DSL_RUNTIME_VALUE_PROJECTION_ID id,
         DSL_RUNTIME_VALUE_PROJECTION_RECORD *record)
{
    return DSL_IR_Table_Get(DSL_runtime_value_projection_table, id, record);
}

BOOL
DSL_Runtime_Interface_Image_Get_Call
        (DSL_RUNTIME_CALL_PROJECTION_ID id,
         DSL_RUNTIME_CALL_PROJECTION_RECORD *record)
{
    return DSL_IR_Table_Get(DSL_runtime_call_projection_table, id, record);
}

BOOL
DSL_Runtime_Interface_Image_Find_Value
        (ST_IDX owner_pu_st, DSL_IR_VALUE_ID source_value_id,
         DSL_RUNTIME_VALUE_PROJECTION_RECORD *record)
{
    for (UINT32 i = 0; i < DSL_runtime_value_projection_table.Size(); ++i) {
        const DSL_RUNTIME_VALUE_PROJECTION_RECORD &current =
            DSL_runtime_value_projection_table[i];
        if (current.owner_pu_st == owner_pu_st &&
            current.source_value_id == source_value_id) {
            if (record != NULL)
                *record = current;
            return TRUE;
        }
    }
    return FALSE;
}

BOOL
DSL_Runtime_Interface_Image_Find_Call
        (DSL_CALLSITE_METADATA_ID callsite_id, UINT32 actual_ordinal,
         DSL_RUNTIME_CALL_PROJECTION_RECORD *record)
{
    for (UINT32 i = 0; i < DSL_runtime_call_projection_table.Size(); ++i) {
        const DSL_RUNTIME_CALL_PROJECTION_RECORD &current =
            DSL_runtime_call_projection_table[i];
        if (current.callsite_id == callsite_id &&
            current.actual_ordinal == actual_ordinal) {
            if (record != NULL)
                *record = current;
            return TRUE;
        }
    }
    return FALSE;
}

DSL_RUNTIME_VALUE_PROJECTION_ID
DSL_Runtime_Interface_Image_Add_Value
        (const DSL_RUNTIME_VALUE_PROJECTION_RECORD *record)
{
    if (record == NULL)
        return DSL_RUNTIME_VALUE_PROJECTION_INVALID_ID;
    DSL_RUNTIME_VALUE_PROJECTION_RECORD copy = *record;
    UINT32 index = DSL_runtime_value_projection_table.Insert(copy);
    DSL_runtime_value_projection_table[index].id = index + 1;
    return index + 1;
}

DSL_RUNTIME_CALL_PROJECTION_ID
DSL_Runtime_Interface_Image_Add_Call
        (const DSL_RUNTIME_CALL_PROJECTION_RECORD *record)
{
    if (record == NULL)
        return DSL_RUNTIME_CALL_PROJECTION_INVALID_ID;
    DSL_RUNTIME_CALL_PROJECTION_RECORD copy = *record;
    UINT32 index = DSL_runtime_call_projection_table.Insert(copy);
    DSL_runtime_call_projection_table[index].id = index + 1;
    return index + 1;
}

struct DSL_RUNTIME_INTERFACE_MAPPED_VIEW {
    const DSL_RUNTIME_INTERFACE_IMAGE_HEADER *header;
    const DSL_RUNTIME_VALUE_PROJECTION_RECORD *values;
    const DSL_RUNTIME_CALL_PROJECTION_RECORD *calls;
};

static BOOL
DSL_Runtime_Interface_Mapped_View_Parse
        (const void *section_base, UINT64 section_size,
         const DSL_RETIRED_FORMAL_RECORD *retired_formals,
         UINT32 retired_formal_count, BOOL use_retired_view,
         DSL_RUNTIME_INTERFACE_MAPPED_VIEW *view, FILE *diagnostic)
{
    if (view == NULL || section_base == NULL ||
        section_size < DSL_RUNTIME_INTERFACE_IMAGE_HEADER_SIZE)
        return DSL_Runtime_Interface_Report
                   (diagnostic, "section is truncated", 0);
    const DSL_RUNTIME_INTERFACE_IMAGE_HEADER *header =
        (const DSL_RUNTIME_INTERFACE_IMAGE_HEADER *)section_base;
    UINT64 remaining = section_size -
                       DSL_RUNTIME_INTERFACE_IMAGE_HEADER_SIZE;
    if (header->magic != DSL_RUNTIME_INTERFACE_IMAGE_MAGIC ||
        header->version != DSL_RUNTIME_INTERFACE_IMAGE_VERSION ||
        header->flags != 0 || header->reserved0 != 0 ||
        header->reserved1 != 0 || header->reserved2 != 0 ||
        header->value_projection_count >
            remaining / DSL_RUNTIME_VALUE_PROJECTION_RECORD_SIZE)
        return DSL_Runtime_Interface_Report(diagnostic, "invalid header", 0);

    remaining -= (UINT64)header->value_projection_count *
                 DSL_RUNTIME_VALUE_PROJECTION_RECORD_SIZE;
    if (header->call_projection_count >
            remaining / DSL_RUNTIME_CALL_PROJECTION_RECORD_SIZE ||
        (UINT64)header->call_projection_count *
            DSL_RUNTIME_CALL_PROJECTION_RECORD_SIZE != remaining)
        return DSL_Runtime_Interface_Report(diagnostic, "invalid header", 0);

    const char *cursor = (const char *)section_base +
                         DSL_RUNTIME_INTERFACE_IMAGE_HEADER_SIZE;
    const DSL_RUNTIME_VALUE_PROJECTION_RECORD *values =
        (const DSL_RUNTIME_VALUE_PROJECTION_RECORD *)cursor;
    cursor += (UINT64)header->value_projection_count *
              DSL_RUNTIME_VALUE_PROJECTION_RECORD_SIZE;
    const DSL_RUNTIME_CALL_PROJECTION_RECORD *calls =
        (const DSL_RUNTIME_CALL_PROJECTION_RECORD *)cursor;
    if (!DSL_Runtime_Interface_View_Validate
             (values, header->value_projection_count, calls,
              header->call_projection_count, retired_formals,
              retired_formal_count, use_retired_view, diagnostic))
        return FALSE;
    view->header = header;
    view->values = values;
    view->calls = calls;
    return TRUE;
}

static void
DSL_Runtime_Interface_Mapped_View_Commit
        (const DSL_RUNTIME_INTERFACE_MAPPED_VIEW &view)
{
    DSL_Runtime_Interface_Image_Reset();
    if (view.header->value_projection_count != 0)
        DSL_runtime_value_projection_table.Insert
            (view.values, view.header->value_projection_count);
    if (view.header->call_projection_count != 0)
        DSL_runtime_call_projection_table.Insert
            (view.calls, view.header->call_projection_count);
}

BOOL
DSL_Runtime_Interface_Image_Load_Mapped
        (const void *section_base, UINT64 section_size, FILE *diagnostic)
{
    DSL_RUNTIME_INTERFACE_MAPPED_VIEW view;
    if (!DSL_Runtime_Interface_Mapped_View_Parse
             (section_base, section_size, NULL, 0, FALSE, &view,
              diagnostic))
        return FALSE;
    if (DSL_Program_Interface_Image_Has_Records() &&
        !DSL_Program_Interface_Validate_Runtime_View
             (view.values, view.header->value_projection_count, view.calls,
              view.header->call_projection_count, diagnostic))
        return FALSE;
    DSL_Runtime_Interface_Mapped_View_Commit(view);
    return TRUE;
}

static BOOL
DSL_Program_Interface_Report
        (FILE *diagnostic, const char *message, UINT32 id)
{
    if (diagnostic != NULL)
        fprintf(diagnostic, "DSL program interface image error: %s id=%u\n",
                message, id);
    return FALSE;
}

static BOOL
DSL_Program_Interface_PU_Valid (ST_IDX owner_pu_st)
{
    DSL_PU_SOURCE_IDENTITY_RECORD identity;
    return owner_pu_st != ST_IDX_ZERO &&
           DSL_Call_Image_Find_PU_Identity(owner_pu_st, &identity);
}

static const DSL_RETIRED_FORMAL_RECORD *
DSL_Program_Interface_Find_Retired_Formal_In_View
        (const DSL_RETIRED_FORMAL_RECORD *records, UINT32 count,
         DSL_PU_FORMAL_ID formal_id)
{
    for (UINT32 i = 0; i < count; ++i) {
        if (records[i].pu_formal_id == formal_id)
            return &records[i];
    }
    return NULL;
}

static const DSL_RETIRED_CALL_ARGUMENT_RECORD *
DSL_Program_Interface_Find_Retired_Call_In_View
        (const DSL_RETIRED_CALL_ARGUMENT_RECORD *records, UINT32 count,
         DSL_CALL_ARGUMENT_ID argument_id)
{
    for (UINT32 i = 0; i < count; ++i) {
        if (records[i].call_argument_id == argument_id)
            return &records[i];
    }
    return NULL;
}

static const DSL_RUNTIME_INPUT_BINDING_RECORD *
DSL_Program_Interface_Find_Binding_In_View
        (const DSL_RUNTIME_INPUT_BINDING_RECORD *records, UINT32 count,
         ST_IDX owner_pu_st, UINT32 final_formal_ordinal)
{
    for (UINT32 i = 0; i < count; ++i) {
        if (records[i].owner_pu_st == owner_pu_st &&
            records[i].final_formal_ordinal == final_formal_ordinal)
            return &records[i];
    }
    return NULL;
}

static const DSL_RUNTIME_VALUE_PROJECTION_RECORD *
DSL_Program_Interface_Find_Projection_In_View
        (const DSL_RUNTIME_VALUE_PROJECTION_RECORD *records, UINT32 count,
         ST_IDX owner_pu_st, DSL_IR_VALUE_ID source_value_id)
{
    for (UINT32 i = 0; i < count; ++i) {
        if (records[i].owner_pu_st == owner_pu_st &&
            records[i].source_value_id == source_value_id)
            return &records[i];
    }
    return NULL;
}

static const DSL_RUNTIME_CALL_PROJECTION_RECORD *
DSL_Program_Interface_Find_Call_Projection_In_View
        (const DSL_RUNTIME_CALL_PROJECTION_RECORD *records, UINT32 count,
         DSL_CALLSITE_METADATA_ID callsite_id, UINT32 actual_ordinal)
{
    for (UINT32 i = 0; i < count; ++i) {
        if (records[i].callsite_id == callsite_id &&
            records[i].actual_ordinal == actual_ordinal)
            return &records[i];
    }
    return NULL;
}

static UINT32
DSL_Program_Interface_Live_Formal_Count
        (const DSL_RETIRED_FORMAL_RECORD *records, UINT32 count,
         ST_IDX owner_pu_st)
{
    UINT32 formals = 0;
    UINT32 retired = 0;
    for (UINT32 i = 1; i <= DSL_PU_Interface_Image_Formal_Count(); ++i) {
        DSL_PU_FORMAL_RECORD formal;
        if (DSL_PU_Interface_Image_Get_Formal(i, &formal) &&
            formal.owner_pu_st == owner_pu_st)
            ++formals;
    }
    for (UINT32 i = 0; i < count; ++i) {
        if (records[i].owner_pu_st == owner_pu_st)
            ++retired;
    }
    return formals >= retired ? formals - retired : 0;
}

static BOOL
DSL_Program_Interface_Binding_Less
        (const DSL_RUNTIME_INPUT_BINDING_RECORD &left,
         const DSL_RUNTIME_INPUT_BINDING_RECORD &right,
         const DSL_RUNTIME_INPUT_RECORD *inputs, UINT32 input_count)
{
    if (left.binding_kind != right.binding_kind)
        return left.binding_kind < right.binding_kind;
    INT role_order = strcmp(Index_To_Str(left.semantic_role),
                            Index_To_Str(right.semantic_role));
    if (role_order != 0)
        return role_order < 0;
    const DSL_RUNTIME_INPUT_RECORD *left_input =
        left.runtime_input_id == 0 || left.runtime_input_id > input_count ?
            NULL : &inputs[left.runtime_input_id - 1];
    const DSL_RUNTIME_INPUT_RECORD *right_input =
        right.runtime_input_id == 0 || right.runtime_input_id > input_count ?
            NULL : &inputs[right.runtime_input_id - 1];
    if (left_input == NULL || right_input == NULL)
        return left.runtime_input_id < right.runtime_input_id;
    if (left_input->source_owner_pu_st != right_input->source_owner_pu_st)
        return left_input->source_owner_pu_st <
               right_input->source_owner_pu_st;
    if (left_input->source_value_id != right_input->source_value_id)
        return left_input->source_value_id < right_input->source_value_id;
    return left.runtime_input_id < right.runtime_input_id;
}

static BOOL
DSL_Program_Interface_View_Validate
        (const DSL_RETIRED_FORMAL_RECORD *retired_formals,
         UINT32 retired_formal_count,
         const DSL_RETIRED_CALL_ARGUMENT_RECORD *retired_calls,
         UINT32 retired_call_count,
         const DSL_RUNTIME_INPUT_RECORD *inputs, UINT32 input_count,
         const DSL_RUNTIME_INPUT_BINDING_RECORD *bindings,
         UINT32 binding_count,
         const DSL_RUNTIME_INPUT_CALL_RECORD *calls, UINT32 call_count,
         const DSL_RUNTIME_VALUE_PROJECTION_RECORD *projections,
         UINT32 projection_count,
         const DSL_RUNTIME_CALL_PROJECTION_RECORD *call_projections,
         UINT32 call_projection_count,
         BOOL validate_runtime_projections, FILE *diagnostic)
{
    for (UINT32 i = 0; i < retired_formal_count; ++i) {
        const DSL_RETIRED_FORMAL_RECORD &record = retired_formals[i];
        DSL_PU_FORMAL_RECORD formal;
        if (record.id != i + 1 ||
            !DSL_PU_Interface_Image_Get_Formal(record.pu_formal_id,
                                               &formal) ||
            formal.owner_pu_st != record.owner_pu_st ||
            formal.formal_value_id != record.formal_value_id ||
            formal.formal_st != record.formal_st ||
            formal.formal_ty != record.formal_ty ||
            formal.formal_ordinal != record.old_formal_ordinal ||
            record.retirement_reason !=
                DSL_INTERFACE_RETIREMENT_VERIFIED_DEAD_INPUT ||
            !DSL_IR_Image_String_Id_Valid(record.semantic_role, TRUE) ||
            record.flags != 0 || record.reserved != 0)
            return DSL_Program_Interface_Report
                       (diagnostic, "invalid retired formal", i + 1);
        for (UINT32 j = 0; j < i; ++j) {
            if (retired_formals[j].pu_formal_id == record.pu_formal_id)
                return DSL_Program_Interface_Report
                           (diagnostic, "duplicate retired formal", i + 1);
        }
    }

    for (UINT32 i = 0; i < retired_call_count; ++i) {
        const DSL_RETIRED_CALL_ARGUMENT_RECORD &record = retired_calls[i];
        DSL_CALL_ARGUMENT_RECORD argument;
        if (record.id != i + 1 ||
            !DSL_Call_ABI_Image_Get_Argument(record.call_argument_id,
                                             &argument) ||
            argument.callsite_id != record.callsite_id ||
            argument.argument_value_id != record.argument_value_id ||
            argument.actual_ordinal != record.old_actual_ordinal ||
            argument.callee_formal_ordinal !=
                record.old_callee_formal_ordinal ||
            record.retirement_reason !=
                DSL_INTERFACE_RETIREMENT_VERIFIED_DEAD_INPUT ||
            !DSL_IR_Image_String_Id_Valid(record.semantic_role, TRUE) ||
            record.flags != 0 || record.reserved[0] != 0 ||
            record.reserved[1] != 0)
            return DSL_Program_Interface_Report
                       (diagnostic, "invalid retired call argument", i + 1);
        for (UINT32 j = 0; j < i; ++j) {
            if (retired_calls[j].call_argument_id == record.call_argument_id)
                return DSL_Program_Interface_Report
                           (diagnostic, "duplicate retired call", i + 1);
        }
        DSL_CALLSITE_METADATA_RECORD callsite;
        DSL_PU_FORMAL_RECORD formal;
        if (!DSL_Call_Image_Get_Callsite(record.callsite_id, &callsite) ||
            !DSL_PU_Interface_Image_Find_Formal
                (callsite.callee_pu_st, record.old_callee_formal_ordinal,
                 &formal) ||
            DSL_Program_Interface_Find_Retired_Formal_In_View
                (retired_formals, retired_formal_count, formal.id) == NULL)
            return DSL_Program_Interface_Report
                       (diagnostic, "retired call has live formal", i + 1);
    }

    for (UINT32 i = 0; i < retired_formal_count; ++i) {
        UINT32 incoming = 0;
        UINT32 retired = 0;
        for (UINT32 j = 1; j <= DSL_Call_ABI_Image_Argument_Count(); ++j) {
            DSL_CALL_ARGUMENT_RECORD argument;
            DSL_CALLSITE_METADATA_RECORD callsite;
            if (!DSL_Call_ABI_Image_Get_Argument(j, &argument) ||
                !DSL_Call_Image_Get_Callsite(argument.callsite_id, &callsite))
                return DSL_Program_Interface_Report
                           (diagnostic, "missing canonical call", j);
            if (callsite.callee_pu_st == retired_formals[i].owner_pu_st &&
                argument.callee_formal_ordinal ==
                    retired_formals[i].old_formal_ordinal) {
                ++incoming;
                if (DSL_Program_Interface_Find_Retired_Call_In_View
                        (retired_calls, retired_call_count, argument.id) !=
                    NULL)
                    ++retired;
            }
        }
        if (incoming != retired)
            return DSL_Program_Interface_Report
                       (diagnostic, "incomplete retired call coverage", i + 1);
    }

    for (UINT32 i = 1;
         validate_runtime_projections &&
         i <= DSL_PU_Interface_Image_Formal_Count(); ++i) {
        DSL_PU_FORMAL_RECORD formal;
        if (!DSL_PU_Interface_Image_Get_Formal(i, &formal))
            return DSL_Program_Interface_Report
                       (diagnostic, "missing canonical formal", i);
        BOOL retired = DSL_Program_Interface_Find_Retired_Formal_In_View
                           (retired_formals, retired_formal_count,
                            formal.id) != NULL;
        BOOL projected = DSL_Program_Interface_Find_Projection_In_View
                             (projections, projection_count,
                              formal.owner_pu_st, formal.formal_value_id) !=
                         NULL;
        if (retired == projected)
            return DSL_Program_Interface_Report
                       (diagnostic, "formal is not exclusively retired or "
                                    "projected", i);
    }

    for (UINT32 i = 1;
         validate_runtime_projections &&
         i <= DSL_Call_ABI_Image_Argument_Count(); ++i) {
        DSL_CALL_ARGUMENT_RECORD argument;
        if (!DSL_Call_ABI_Image_Get_Argument(i, &argument))
            return DSL_Program_Interface_Report
                       (diagnostic, "missing canonical call argument", i);
        BOOL retired = DSL_Program_Interface_Find_Retired_Call_In_View
                           (retired_calls, retired_call_count,
                            argument.id) != NULL;
        BOOL projected =
            DSL_Program_Interface_Find_Call_Projection_In_View
                (call_projections, call_projection_count,
                 argument.callsite_id, argument.actual_ordinal) != NULL;
        if (retired == projected)
            return DSL_Program_Interface_Report
                       (diagnostic, "call argument is not exclusively retired "
                                    "or projected", i);
    }

    for (UINT32 i = 0; i < input_count; ++i) {
        const DSL_RUNTIME_INPUT_RECORD &record = inputs[i];
        if (record.id != i + 1 ||
            !DSL_Program_Interface_Runtime_Input_Contract_Valid(&record) ||
            !DSL_Program_Interface_Pointer_TY_Contract_Valid
                (record.handle_ty) ||
            !DSL_IR_Image_String_Id_Valid(record.stable_role, TRUE) ||
            record.flags != 0)
            return DSL_Program_Interface_Report
                       (diagnostic, "invalid runtime input", i + 1);
        for (UINT32 j = 0; j < 5; ++j) {
            if (record.reserved[j] != 0)
                return DSL_Program_Interface_Report
                           (diagnostic, "invalid runtime input", i + 1);
        }
        for (UINT32 j = 0; j < i; ++j) {
            BOOL duplicate_source =
                record.input_kind ==
                    DSL_RUNTIME_INPUT_SOURCE_EXTERNAL_TENSOR &&
                inputs[j].input_kind == record.input_kind &&
                inputs[j].source_owner_pu_st == record.source_owner_pu_st &&
                inputs[j].source_value_id == record.source_value_id;
            BOOL duplicate_role =
                record.input_kind !=
                    DSL_RUNTIME_INPUT_SOURCE_EXTERNAL_TENSOR &&
                inputs[j].input_kind !=
                    DSL_RUNTIME_INPUT_SOURCE_EXTERNAL_TENSOR &&
                inputs[j].stable_role == record.stable_role;
            if (duplicate_source || duplicate_role)
                return DSL_Program_Interface_Report
                           (diagnostic, "duplicate runtime input", i + 1);
        }
    }

    for (UINT32 i = 0; i < binding_count; ++i) {
        const DSL_RUNTIME_INPUT_BINDING_RECORD &record = bindings[i];
        BOOL root = record.binding_kind ==
                        DSL_RUNTIME_INPUT_BINDING_ROOT_PROMOTED_SOURCE ||
                    record.binding_kind ==
                        DSL_RUNTIME_INPUT_BINDING_ROOT_RESOURCE;
        const DSL_RUNTIME_INPUT_RECORD *input =
            record.runtime_input_id == 0 ||
            record.runtime_input_id > input_count ? NULL :
            &inputs[record.runtime_input_id - 1];
        if (record.id != i + 1)
            return DSL_Program_Interface_Report
                       (diagnostic, "invalid runtime binding id", i + 1);
        if (!DSL_Program_Interface_PU_Valid(record.owner_pu_st))
            return DSL_Program_Interface_Report
                       (diagnostic, "invalid runtime binding owner", i + 1);
        if (record.handle_st == ST_IDX_ZERO ||
            !DSL_Program_Interface_Pointer_TY_Contract_Valid
                (record.handle_ty))
            return DSL_Program_Interface_Report
                       (diagnostic, "invalid runtime binding handle", i + 1);
        if (record.final_formal_ordinal ==
                DSL_RUNTIME_INTERFACE_INVALID_ORDINAL ||
            record.binding_kind <
                DSL_RUNTIME_INPUT_BINDING_ROOT_PROMOTED_SOURCE ||
            record.binding_kind >
                DSL_RUNTIME_INPUT_BINDING_THREADED_FORMAL)
            return DSL_Program_Interface_Report
                       (diagnostic, "invalid runtime binding position", i + 1);
        if ((root &&
             (input == NULL || input->handle_ty != record.handle_ty ||
              input->stable_role != record.semantic_role ||
              (record.binding_kind ==
                   DSL_RUNTIME_INPUT_BINDING_ROOT_PROMOTED_SOURCE) !=
                  (input->input_kind ==
                   DSL_RUNTIME_INPUT_SOURCE_EXTERNAL_TENSOR))) ||
            (!root && record.runtime_input_id != 0))
            return DSL_Program_Interface_Report
                       (diagnostic, "invalid runtime binding input", i + 1);
        if (!DSL_IR_Image_String_Id_Valid(record.semantic_role, TRUE) ||
            record.flags != 0 || record.reserved[0] != 0 ||
            record.reserved[1] != 0)
            return DSL_Program_Interface_Report
                       (diagnostic, "invalid runtime binding fields", i + 1);
        if (root) {
            for (UINT32 j = 1; j <= DSL_Call_Image_Callsite_Count(); ++j) {
                DSL_CALLSITE_METADATA_RECORD callsite;
                if (!DSL_Call_Image_Get_Callsite(j, &callsite))
                    return DSL_Program_Interface_Report
                               (diagnostic, "missing callsite", j);
                if (callsite.callee_pu_st == record.owner_pu_st)
                    return DSL_Program_Interface_Report
                               (diagnostic,
                                "root binding owner has incoming call", i + 1);
            }
        }
        UINT32 live_formals = DSL_Program_Interface_Live_Formal_Count
                                  (retired_formals, retired_formal_count,
                                   record.owner_pu_st);
        if (record.final_formal_ordinal < live_formals)
            return DSL_Program_Interface_Report
                       (diagnostic, "binding overlaps canonical formal", i + 1);
        for (UINT32 j = 0; j < i; ++j) {
            if (bindings[j].owner_pu_st == record.owner_pu_st &&
                (bindings[j].final_formal_ordinal ==
                     record.final_formal_ordinal ||
                 bindings[j].semantic_role == record.semantic_role ||
                 bindings[j].handle_st == record.handle_st))
                return DSL_Program_Interface_Report
                           (diagnostic, "duplicate runtime binding", i + 1);
        }
    }

    for (UINT32 i = 0; i < binding_count; ++i) {
        const DSL_RUNTIME_INPUT_BINDING_RECORD &record = bindings[i];
        UINT32 live_formals = DSL_Program_Interface_Live_Formal_Count
                                  (retired_formals, retired_formal_count,
                                   record.owner_pu_st);
        UINT32 prior = 0;
        for (UINT32 j = 0; j < binding_count; ++j) {
            if (bindings[j].owner_pu_st == record.owner_pu_st &&
                DSL_Program_Interface_Binding_Less
                    (bindings[j], record, inputs, input_count))
                ++prior;
        }
        if (record.final_formal_ordinal != live_formals + prior)
            return DSL_Program_Interface_Report
                       (diagnostic, "noncanonical binding order", i + 1);
    }

    for (UINT32 i = 0; i < call_count; ++i) {
        const DSL_RUNTIME_INPUT_CALL_RECORD &record = calls[i];
        DSL_CALLSITE_METADATA_RECORD callsite;
        const DSL_RUNTIME_INPUT_BINDING_RECORD *caller =
            DSL_Program_Interface_Find_Binding_In_View
                (bindings, binding_count, record.caller_owner_pu_st,
                 record.caller_final_formal_ordinal);
        const DSL_RUNTIME_INPUT_BINDING_RECORD *callee =
            DSL_Program_Interface_Find_Binding_In_View
                (bindings, binding_count, record.callee_owner_pu_st,
                 record.callee_final_formal_ordinal);
        if (record.id != i + 1 ||
            !DSL_Call_Image_Get_Callsite(record.callsite_id, &callsite) ||
            callsite.owner_pu_st != record.caller_owner_pu_st ||
            callsite.callee_pu_st != record.callee_owner_pu_st ||
            caller == NULL || callee == NULL ||
            callee->binding_kind !=
                DSL_RUNTIME_INPUT_BINDING_THREADED_FORMAL ||
            caller->handle_ty != record.handle_ty ||
            callee->handle_ty != record.handle_ty ||
            callee->semantic_role != record.semantic_role ||
            record.final_actual_ordinal !=
                record.final_callee_formal_ordinal ||
            record.final_callee_formal_ordinal !=
                callee->final_formal_ordinal ||
            record.flags != 0)
            return DSL_Program_Interface_Report
                       (diagnostic, "invalid runtime input call", i + 1);
        for (UINT32 j = 0; j < i; ++j) {
            if (calls[j].callsite_id == record.callsite_id &&
                calls[j].final_actual_ordinal ==
                    record.final_actual_ordinal)
                return DSL_Program_Interface_Report
                           (diagnostic, "duplicate runtime input call", i + 1);
        }
    }

    for (UINT32 i = 0; i < input_count; ++i) {
        UINT32 roots = 0;
        for (UINT32 j = 0; j < binding_count; ++j) {
            if (bindings[j].runtime_input_id == inputs[i].id)
                ++roots;
        }
        if (roots != 1)
            return DSL_Program_Interface_Report
                       (diagnostic, "runtime input root coverage", i + 1);
    }

    std::vector<std::vector<BOOL> > edges
        (binding_count, std::vector<BOOL>(binding_count, FALSE));
    for (UINT32 i = 0; i < call_count; ++i) {
        const DSL_RUNTIME_INPUT_BINDING_RECORD *caller =
            DSL_Program_Interface_Find_Binding_In_View
                (bindings, binding_count, calls[i].caller_owner_pu_st,
                 calls[i].caller_final_formal_ordinal);
        const DSL_RUNTIME_INPUT_BINDING_RECORD *callee =
            DSL_Program_Interface_Find_Binding_In_View
                (bindings, binding_count, calls[i].callee_owner_pu_st,
                 calls[i].callee_final_formal_ordinal);
        edges[caller->id - 1][callee->id - 1] = TRUE;
    }
    for (UINT32 k = 0; k < binding_count; ++k)
        for (UINT32 i = 0; i < binding_count; ++i)
            for (UINT32 j = 0; j < binding_count; ++j)
                edges[i][j] = edges[i][j] ||
                              (edges[i][k] && edges[k][j]);
    for (UINT32 i = 0; i < binding_count; ++i) {
        if (edges[i][i])
            return DSL_Program_Interface_Report
                       (diagnostic, "cyclic runtime input flow", i + 1);
        BOOL reachable = FALSE;
        for (UINT32 root = 0; root < binding_count; ++root) {
            if (bindings[root].binding_kind !=
                    DSL_RUNTIME_INPUT_BINDING_THREADED_FORMAL &&
                (root == i || edges[root][i])) {
                reachable = TRUE;
                break;
            }
        }
        if (!reachable)
            return DSL_Program_Interface_Report
                       (diagnostic, "uninitialized runtime binding", i + 1);
    }

    for (UINT32 i = 0; i < binding_count; ++i) {
        if (bindings[i].binding_kind !=
            DSL_RUNTIME_INPUT_BINDING_THREADED_FORMAL)
            continue;
        UINT32 incoming_calls = 0;
        UINT32 represented_calls = 0;
        for (UINT32 j = 1; j <= DSL_Call_Image_Callsite_Count(); ++j) {
            DSL_CALLSITE_METADATA_RECORD callsite;
            if (!DSL_Call_Image_Get_Callsite(j, &callsite))
                return DSL_Program_Interface_Report
                           (diagnostic, "missing callsite", j);
            if (callsite.callee_pu_st == bindings[i].owner_pu_st) {
                ++incoming_calls;
                for (UINT32 k = 0; k < call_count; ++k) {
                    if (calls[k].callsite_id == callsite.id &&
                        calls[k].callee_owner_pu_st ==
                            bindings[i].owner_pu_st &&
                        calls[k].callee_final_formal_ordinal ==
                            bindings[i].final_formal_ordinal)
                        ++represented_calls;
                }
            }
        }
        if (incoming_calls != represented_calls)
            return DSL_Program_Interface_Report
                       (diagnostic, "incomplete threaded binding", i + 1);
    }
    return TRUE;
}

void
DSL_Program_Interface_Image_Get_Header
        (DSL_PROGRAM_INTERFACE_IMAGE_HEADER *header)
{
    if (header == NULL)
        return;
    memset(header, 0, sizeof(*header));
    header->magic = DSL_PROGRAM_INTERFACE_IMAGE_MAGIC;
    header->version = DSL_PROGRAM_INTERFACE_IMAGE_VERSION;
    header->retired_formal_count = DSL_retired_formal_table.Size();
    header->retired_call_argument_count =
        DSL_retired_call_argument_table.Size();
    header->runtime_input_count = DSL_runtime_input_table.Size();
    header->runtime_input_binding_count =
        DSL_runtime_input_binding_table.Size();
    header->runtime_input_call_count = DSL_runtime_input_call_table.Size();
}

void
DSL_Program_Interface_Image_Reset (void)
{
    DSL_retired_formal_table.Delete_down_to(0);
    DSL_retired_call_argument_table.Delete_down_to(0);
    DSL_runtime_input_table.Delete_down_to(0);
    DSL_runtime_input_binding_table.Delete_down_to(0);
    DSL_runtime_input_call_table.Delete_down_to(0);
    DSL_Program_Interface_Reset_Commit_State();
}

BOOL
DSL_Program_Interface_Image_Has_Records (void)
{
    return DSL_retired_formal_table.Size() != 0 ||
           DSL_retired_call_argument_table.Size() != 0 ||
           DSL_runtime_input_table.Size() != 0 ||
           DSL_runtime_input_binding_table.Size() != 0 ||
           DSL_runtime_input_call_table.Size() != 0;
}

BOOL
DSL_Program_Interface_Image_Validate (FILE *diagnostic)
{
    std::vector<DSL_RETIRED_FORMAL_RECORD> retired_formals;
    std::vector<DSL_RETIRED_CALL_ARGUMENT_RECORD> retired_calls;
    std::vector<DSL_RUNTIME_INPUT_RECORD> inputs;
    std::vector<DSL_RUNTIME_INPUT_BINDING_RECORD> bindings;
    std::vector<DSL_RUNTIME_INPUT_CALL_RECORD> calls;
    std::vector<DSL_RUNTIME_VALUE_PROJECTION_RECORD> projections;
    std::vector<DSL_RUNTIME_CALL_PROJECTION_RECORD> call_projections;
    for (UINT32 i = 0; i < DSL_retired_formal_table.Size(); ++i)
        retired_formals.push_back(DSL_retired_formal_table[i]);
    for (UINT32 i = 0; i < DSL_retired_call_argument_table.Size(); ++i)
        retired_calls.push_back(DSL_retired_call_argument_table[i]);
    for (UINT32 i = 0; i < DSL_runtime_input_table.Size(); ++i)
        inputs.push_back(DSL_runtime_input_table[i]);
    for (UINT32 i = 0; i < DSL_runtime_input_binding_table.Size(); ++i)
        bindings.push_back(DSL_runtime_input_binding_table[i]);
    for (UINT32 i = 0; i < DSL_runtime_input_call_table.Size(); ++i)
        calls.push_back(DSL_runtime_input_call_table[i]);
    for (UINT32 i = 0; i < DSL_runtime_value_projection_table.Size(); ++i)
        projections.push_back(DSL_runtime_value_projection_table[i]);
    for (UINT32 i = 0; i < DSL_runtime_call_projection_table.Size(); ++i)
        call_projections.push_back(DSL_runtime_call_projection_table[i]);
    return DSL_Program_Interface_View_Validate
        (retired_formals.empty() ? NULL : &retired_formals[0],
         retired_formals.size(),
         retired_calls.empty() ? NULL : &retired_calls[0],
         retired_calls.size(), inputs.empty() ? NULL : &inputs[0],
         inputs.size(), bindings.empty() ? NULL : &bindings[0],
         bindings.size(), calls.empty() ? NULL : &calls[0], calls.size(),
         projections.empty() ? NULL : &projections[0], projections.size(),
         call_projections.empty() ? NULL : &call_projections[0],
         call_projections.size(),
         TRUE, diagnostic);
}

static BOOL
DSL_Program_Interface_Validate_Runtime_View
        (const DSL_RUNTIME_VALUE_PROJECTION_RECORD *projections,
         UINT32 projection_count,
         const DSL_RUNTIME_CALL_PROJECTION_RECORD *call_projections,
         UINT32 call_projection_count, FILE *diagnostic)
{
    std::vector<DSL_RETIRED_FORMAL_RECORD> retired_formals;
    std::vector<DSL_RETIRED_CALL_ARGUMENT_RECORD> retired_calls;
    std::vector<DSL_RUNTIME_INPUT_RECORD> inputs;
    std::vector<DSL_RUNTIME_INPUT_BINDING_RECORD> bindings;
    std::vector<DSL_RUNTIME_INPUT_CALL_RECORD> calls;
    for (UINT32 i = 0; i < DSL_retired_formal_table.Size(); ++i)
        retired_formals.push_back(DSL_retired_formal_table[i]);
    for (UINT32 i = 0; i < DSL_retired_call_argument_table.Size(); ++i)
        retired_calls.push_back(DSL_retired_call_argument_table[i]);
    for (UINT32 i = 0; i < DSL_runtime_input_table.Size(); ++i)
        inputs.push_back(DSL_runtime_input_table[i]);
    for (UINT32 i = 0; i < DSL_runtime_input_binding_table.Size(); ++i)
        bindings.push_back(DSL_runtime_input_binding_table[i]);
    for (UINT32 i = 0; i < DSL_runtime_input_call_table.Size(); ++i)
        calls.push_back(DSL_runtime_input_call_table[i]);
    return DSL_Program_Interface_View_Validate
        (retired_formals.empty() ? NULL : &retired_formals[0],
         retired_formals.size(),
         retired_calls.empty() ? NULL : &retired_calls[0],
         retired_calls.size(), inputs.empty() ? NULL : &inputs[0],
         inputs.size(), bindings.empty() ? NULL : &bindings[0],
         bindings.size(), calls.empty() ? NULL : &calls[0], calls.size(),
         projections, projection_count, call_projections,
         call_projection_count, TRUE, diagnostic);
}

#define DSL_PROGRAM_INTERFACE_COUNT_GETTER(name, table) \
UINT32 name (void) { return table.Size(); }

DSL_PROGRAM_INTERFACE_COUNT_GETTER
    (DSL_Program_Interface_Image_Retired_Formal_Count,
     DSL_retired_formal_table)
DSL_PROGRAM_INTERFACE_COUNT_GETTER
    (DSL_Program_Interface_Image_Retired_Call_Count,
     DSL_retired_call_argument_table)
DSL_PROGRAM_INTERFACE_COUNT_GETTER
    (DSL_Program_Interface_Image_Runtime_Input_Count,
     DSL_runtime_input_table)
DSL_PROGRAM_INTERFACE_COUNT_GETTER
    (DSL_Program_Interface_Image_Runtime_Binding_Count,
     DSL_runtime_input_binding_table)
DSL_PROGRAM_INTERFACE_COUNT_GETTER
    (DSL_Program_Interface_Image_Runtime_Call_Count,
     DSL_runtime_input_call_table)

#undef DSL_PROGRAM_INTERFACE_COUNT_GETTER

BOOL
DSL_Program_Interface_Image_Get_Retired_Formal
        (DSL_RETIRED_FORMAL_ID id, DSL_RETIRED_FORMAL_RECORD *record)
{
    return DSL_IR_Table_Get(DSL_retired_formal_table, id, record);
}

BOOL
DSL_Program_Interface_Image_Get_Retired_Call
        (DSL_RETIRED_CALL_ARGUMENT_ID id,
         DSL_RETIRED_CALL_ARGUMENT_RECORD *record)
{
    return DSL_IR_Table_Get(DSL_retired_call_argument_table, id, record);
}

BOOL
DSL_Program_Interface_Image_Get_Runtime_Input
        (DSL_RUNTIME_INPUT_ID id, DSL_RUNTIME_INPUT_RECORD *record)
{
    return DSL_IR_Table_Get(DSL_runtime_input_table, id, record);
}

BOOL
DSL_Program_Interface_Image_Get_Runtime_Binding
        (DSL_RUNTIME_INPUT_BINDING_ID id,
         DSL_RUNTIME_INPUT_BINDING_RECORD *record)
{
    return DSL_IR_Table_Get(DSL_runtime_input_binding_table, id, record);
}

BOOL
DSL_Program_Interface_Image_Get_Runtime_Call
        (DSL_RUNTIME_INPUT_CALL_ID id, DSL_RUNTIME_INPUT_CALL_RECORD *record)
{
    return DSL_IR_Table_Get(DSL_runtime_input_call_table, id, record);
}

BOOL
DSL_Program_Interface_Image_Find_Retired_Formal
        (DSL_PU_FORMAL_ID pu_formal_id, DSL_RETIRED_FORMAL_RECORD *record)
{
    const DSL_RETIRED_FORMAL_RECORD *found =
        DSL_Program_Interface_Find_Retired_Formal_In_View
            (DSL_retired_formal_table.Size() == 0 ? NULL :
                 &DSL_retired_formal_table[0],
             DSL_retired_formal_table.Size(), pu_formal_id);
    if (found != NULL && record != NULL)
        *record = *found;
    return found != NULL;
}

BOOL
DSL_Program_Interface_Image_Find_Retired_Call
        (DSL_CALL_ARGUMENT_ID call_argument_id,
         DSL_RETIRED_CALL_ARGUMENT_RECORD *record)
{
    const DSL_RETIRED_CALL_ARGUMENT_RECORD *found =
        DSL_Program_Interface_Find_Retired_Call_In_View
            (DSL_retired_call_argument_table.Size() == 0 ? NULL :
                 &DSL_retired_call_argument_table[0],
             DSL_retired_call_argument_table.Size(), call_argument_id);
    if (found != NULL && record != NULL)
        *record = *found;
    return found != NULL;
}

BOOL
DSL_Program_Interface_Image_Find_Runtime_Binding
        (ST_IDX owner_pu_st, UINT32 final_formal_ordinal,
         DSL_RUNTIME_INPUT_BINDING_RECORD *record)
{
    const DSL_RUNTIME_INPUT_BINDING_RECORD *found =
        DSL_Program_Interface_Find_Binding_In_View
            (DSL_runtime_input_binding_table.Size() == 0 ? NULL :
                 &DSL_runtime_input_binding_table[0],
             DSL_runtime_input_binding_table.Size(), owner_pu_st,
             final_formal_ordinal);
    if (found != NULL && record != NULL)
        *record = *found;
    return found != NULL;
}

BOOL
DSL_IR_Image_Resolve_Lowered_Relation
        (DSL_IR_VALUE_ID value_id,
         DSL_IR_NATIVE_VALUE_LOWER_RESULT *result)
{
    DSL_IR_VALUE_RECORD value;
    if (result == NULL ||
        !DSL_IR_Table_Get(DSL_ir_value_table, value_id, &value) ||
        (value.flags & DSL_IR_VALUE_FLAG_LOWERED) == 0)
        return FALSE;

    DSL_RUNTIME_VALUE_PROJECTION_ID projection_id =
        DSL_RUNTIME_VALUE_PROJECTION_INVALID_ID;
    DSL_RUNTIME_VALUE_PROJECTION_RECORD projection;
    UINT32 projection_count = 0;
    for (UINT32 i = 1; i <= DSL_runtime_value_projection_table.Size(); ++i) {
        DSL_RUNTIME_VALUE_PROJECTION_RECORD current;
        if (DSL_IR_Table_Get
                (DSL_runtime_value_projection_table, i, &current) &&
            current.source_value_id == value_id) {
            projection = current;
            projection_id = i;
            ++projection_count;
        }
    }

    DSL_RUNTIME_INPUT_ID input_id = DSL_RUNTIME_INPUT_INVALID_ID;
    DSL_RUNTIME_INPUT_BINDING_ID binding_id =
        DSL_RUNTIME_INPUT_BINDING_INVALID_ID;
    DSL_RUNTIME_INPUT_RECORD input;
    DSL_RUNTIME_INPUT_BINDING_RECORD binding;
    UINT32 input_count = 0;
    for (UINT32 i = 1; i <= DSL_runtime_input_table.Size(); ++i) {
        DSL_RUNTIME_INPUT_RECORD current;
        if (!DSL_IR_Table_Get(DSL_runtime_input_table, i, &current) ||
            current.input_kind !=
                DSL_RUNTIME_INPUT_SOURCE_EXTERNAL_TENSOR ||
            current.source_value_id != value_id)
            continue;
        UINT32 root_count = 0;
        DSL_RUNTIME_INPUT_BINDING_RECORD root;
        DSL_RUNTIME_INPUT_BINDING_ID root_id =
            DSL_RUNTIME_INPUT_BINDING_INVALID_ID;
        for (UINT32 j = 1;
             j <= DSL_runtime_input_binding_table.Size(); ++j) {
            DSL_RUNTIME_INPUT_BINDING_RECORD candidate;
            if (DSL_IR_Table_Get
                    (DSL_runtime_input_binding_table, j, &candidate) &&
                candidate.runtime_input_id == current.id &&
                candidate.owner_pu_st == current.source_owner_pu_st &&
                candidate.binding_kind ==
                    DSL_RUNTIME_INPUT_BINDING_ROOT_PROMOTED_SOURCE) {
                root = candidate;
                root_id = j;
                ++root_count;
            }
        }
        if (root_count != 1)
            return FALSE;
        input = current;
        input_id = i;
        binding = root;
        binding_id = root_id;
        ++input_count;
    }

    if (projection_count > 1 || input_count > 1 ||
        (projection_count == 0 && input_count == 0))
        return FALSE;
    memset(result, 0, sizeof(*result));
    result->source_node_id = value.producer_node_id;
    result->source_value_id = value.id;
    if (input_count == 0) {
        if (projection.source_st != value.st ||
            projection.source_ty != value.ty ||
            projection.binding_kind != DSL_RUNTIME_BINDING_LOCAL_VALUE)
            return FALSE;
        result->mode = DSL_IR_NATIVE_LOWER_COMPUTED_STANDARD_BLOCK;
        result->relation_kind =
            DSL_IR_LOWER_RELATION_RUNTIME_VALUE_PROJECTION;
        result->value_projection_id = projection_id;
        result->handle_st = projection.handle_st;
        result->handle_ty = projection.handle_ty;
    } else {
        if (input.source_st != value.st || input.source_ty != value.ty ||
            input.source_tcon == TCON_IDX_ZERO ||
            binding.handle_ty != input.handle_ty ||
            (projection_count == 1 &&
             (projection.source_st != value.st ||
              projection.source_ty != value.ty ||
              projection.owner_pu_st != input.source_owner_pu_st ||
              projection.handle_ty != binding.handle_ty ||
              projection.binding_kind != DSL_RUNTIME_BINDING_LOCAL_VALUE ||
              projection.handle_st == binding.handle_st)))
            return FALSE;
        result->mode = DSL_IR_NATIVE_LOWER_PROMOTED_SOURCE_ELISION;
        result->relation_kind = DSL_IR_LOWER_RELATION_ROOT_PROMOTED_INPUT;
        if (projection_count == 1)
            result->value_projection_id = projection_id;
        result->runtime_input_id = input_id;
        result->runtime_binding_id = binding_id;
        result->handle_st = binding.handle_st;
        result->handle_ty = binding.handle_ty;
    }
    return TRUE;
}

BOOL
DSL_IR_Image_Validate_Lowered_Relations (FILE *diagnostic)
{
    for (UINT32 i = 1; i <= DSL_ir_value_table.Size(); ++i) {
        const DSL_IR_VALUE_RECORD &value = DSL_ir_value_table[i - 1];
        if ((value.flags & DSL_IR_VALUE_FLAG_DEAD_ELIDED) != 0) {
            UINT32 projection_count = 0;
            DSL_RUNTIME_VALUE_PROJECTION_ID projection_id = 0;
            for (UINT32 j = 0;
                 j < DSL_runtime_value_projection_table.Size(); ++j) {
                const DSL_RUNTIME_VALUE_PROJECTION_RECORD &projection =
                    DSL_runtime_value_projection_table[j];
                if (projection.source_value_id != value.id)
                    continue;
                if (++projection_count > 1 ||
                    projection.source_st != value.st ||
                    projection.source_ty != value.ty ||
                    projection.binding_kind !=
                        DSL_RUNTIME_BINDING_LOCAL_VALUE)
                    return DSL_IR_Image_Report
                               (diagnostic,
                                "invalid dead source projection", i);
                projection_id = projection.id;
            }
            for (UINT32 j = 0; j < DSL_runtime_input_table.Size(); ++j) {
                if (DSL_runtime_input_table[j].source_value_id == value.id)
                    return DSL_IR_Image_Report
                               (diagnostic,
                                "dead source has runtime input", i);
            }
            for (UINT32 j = 0;
                 j < DSL_runtime_call_projection_table.Size(); ++j) {
                const DSL_RUNTIME_CALL_PROJECTION_RECORD &call =
                    DSL_runtime_call_projection_table[j];
                if (call.source_value_id == value.id ||
                    (projection_id != 0 &&
                     call.value_projection_id == projection_id))
                    return DSL_IR_Image_Report
                               (diagnostic,
                                "dead source has runtime call", i);
            }
        }
        if ((value.flags & DSL_IR_VALUE_FLAG_LOWERED) == 0)
            continue;
        DSL_IR_NATIVE_VALUE_LOWER_RESULT relation;
        if (!DSL_IR_Image_Resolve_Lowered_Relation(i, &relation))
            return DSL_IR_Image_Report
                       (diagnostic, "invalid lowered runtime relation", i);
    }
    return TRUE;
}

DSL_RETIRED_FORMAL_ID
DSL_Program_Interface_Image_Add_Retired_Formal
        (const DSL_RETIRED_FORMAL_RECORD *record)
{
    if (record == NULL)
        return DSL_RETIRED_FORMAL_INVALID_ID;
    UINT32 index = DSL_retired_formal_table.Insert(*record);
    DSL_retired_formal_table[index].id = index + 1;
    return index + 1;
}

DSL_RETIRED_CALL_ARGUMENT_ID
DSL_Program_Interface_Image_Add_Retired_Call
        (const DSL_RETIRED_CALL_ARGUMENT_RECORD *record)
{
    if (record == NULL)
        return DSL_RETIRED_CALL_ARGUMENT_INVALID_ID;
    UINT32 index = DSL_retired_call_argument_table.Insert(*record);
    DSL_retired_call_argument_table[index].id = index + 1;
    return index + 1;
}

DSL_RUNTIME_INPUT_ID
DSL_Program_Interface_Image_Add_Runtime_Input
        (const DSL_RUNTIME_INPUT_RECORD *record)
{
    if (record == NULL)
        return DSL_RUNTIME_INPUT_INVALID_ID;
    UINT32 index = DSL_runtime_input_table.Insert(*record);
    DSL_runtime_input_table[index].id = index + 1;
    return index + 1;
}

DSL_RUNTIME_INPUT_BINDING_ID
DSL_Program_Interface_Image_Add_Runtime_Binding
        (const DSL_RUNTIME_INPUT_BINDING_RECORD *record)
{
    if (record == NULL)
        return DSL_RUNTIME_INPUT_BINDING_INVALID_ID;
    UINT32 index = DSL_runtime_input_binding_table.Insert(*record);
    DSL_runtime_input_binding_table[index].id = index + 1;
    return index + 1;
}

DSL_RUNTIME_INPUT_CALL_ID
DSL_Program_Interface_Image_Add_Runtime_Call
        (const DSL_RUNTIME_INPUT_CALL_RECORD *record)
{
    if (record == NULL)
        return DSL_RUNTIME_INPUT_CALL_INVALID_ID;
    UINT32 index = DSL_runtime_input_call_table.Insert(*record);
    DSL_runtime_input_call_table[index].id = index + 1;
    return index + 1;
}

struct DSL_PROGRAM_INTERFACE_MAPPED_VIEW {
    const DSL_PROGRAM_INTERFACE_IMAGE_HEADER *header;
    const DSL_RETIRED_FORMAL_RECORD *retired_formals;
    const DSL_RETIRED_CALL_ARGUMENT_RECORD *retired_calls;
    const DSL_RUNTIME_INPUT_RECORD *inputs;
    const DSL_RUNTIME_INPUT_BINDING_RECORD *bindings;
    const DSL_RUNTIME_INPUT_CALL_RECORD *calls;
};

static BOOL
DSL_Program_Interface_Mapped_View_Parse
        (const void *section_base, UINT64 section_size,
         DSL_PROGRAM_INTERFACE_MAPPED_VIEW *view, FILE *diagnostic)
{
    if (view == NULL || section_base == NULL ||
        section_size < DSL_PROGRAM_INTERFACE_IMAGE_HEADER_SIZE)
        return DSL_Program_Interface_Report
                   (diagnostic, "section is truncated", 0);
    const DSL_PROGRAM_INTERFACE_IMAGE_HEADER *header =
        (const DSL_PROGRAM_INTERFACE_IMAGE_HEADER *)section_base;
    if (header->magic != DSL_PROGRAM_INTERFACE_IMAGE_MAGIC ||
        header->version != DSL_PROGRAM_INTERFACE_IMAGE_VERSION ||
        header->flags != 0)
        return DSL_Program_Interface_Report(diagnostic, "invalid header", 0);
    for (UINT32 i = 0; i < 8; ++i) {
        if (header->reserved[i] != 0)
            return DSL_Program_Interface_Report
                       (diagnostic, "invalid header", 0);
    }

    UINT64 remaining = section_size -
                       DSL_PROGRAM_INTERFACE_IMAGE_HEADER_SIZE;
#define DSL_PROGRAM_INTERFACE_TAKE(count, size) \
    if ((count) > remaining / (size)) \
        return DSL_Program_Interface_Report \
                   (diagnostic, "invalid header", 0); \
    remaining -= (UINT64)(count) * (size)
    DSL_PROGRAM_INTERFACE_TAKE
        (header->retired_formal_count, DSL_RETIRED_FORMAL_RECORD_SIZE);
    DSL_PROGRAM_INTERFACE_TAKE
        (header->retired_call_argument_count,
         DSL_RETIRED_CALL_ARGUMENT_RECORD_SIZE);
    DSL_PROGRAM_INTERFACE_TAKE
        (header->runtime_input_count, DSL_RUNTIME_INPUT_RECORD_SIZE);
    DSL_PROGRAM_INTERFACE_TAKE
        (header->runtime_input_binding_count,
         DSL_RUNTIME_INPUT_BINDING_RECORD_SIZE);
    DSL_PROGRAM_INTERFACE_TAKE
        (header->runtime_input_call_count, DSL_RUNTIME_INPUT_CALL_RECORD_SIZE);
#undef DSL_PROGRAM_INTERFACE_TAKE
    if (remaining != 0)
        return DSL_Program_Interface_Report(diagnostic, "invalid header", 0);

    const char *cursor = (const char *)section_base +
                         DSL_PROGRAM_INTERFACE_IMAGE_HEADER_SIZE;
    const DSL_RETIRED_FORMAL_RECORD *retired_formals =
        (const DSL_RETIRED_FORMAL_RECORD *)cursor;
    cursor += (UINT64)header->retired_formal_count *
              DSL_RETIRED_FORMAL_RECORD_SIZE;
    const DSL_RETIRED_CALL_ARGUMENT_RECORD *retired_calls =
        (const DSL_RETIRED_CALL_ARGUMENT_RECORD *)cursor;
    cursor += (UINT64)header->retired_call_argument_count *
              DSL_RETIRED_CALL_ARGUMENT_RECORD_SIZE;
    const DSL_RUNTIME_INPUT_RECORD *inputs =
        (const DSL_RUNTIME_INPUT_RECORD *)cursor;
    cursor += (UINT64)header->runtime_input_count *
              DSL_RUNTIME_INPUT_RECORD_SIZE;
    const DSL_RUNTIME_INPUT_BINDING_RECORD *bindings =
        (const DSL_RUNTIME_INPUT_BINDING_RECORD *)cursor;
    cursor += (UINT64)header->runtime_input_binding_count *
              DSL_RUNTIME_INPUT_BINDING_RECORD_SIZE;
    const DSL_RUNTIME_INPUT_CALL_RECORD *calls =
        (const DSL_RUNTIME_INPUT_CALL_RECORD *)cursor;

    if (!DSL_Program_Interface_View_Validate
             (retired_formals, header->retired_formal_count,
              retired_calls, header->retired_call_argument_count,
              inputs, header->runtime_input_count,
              bindings, header->runtime_input_binding_count,
              calls, header->runtime_input_call_count,
              NULL, 0, NULL, 0, FALSE, diagnostic))
        return FALSE;
    view->header = header;
    view->retired_formals = retired_formals;
    view->retired_calls = retired_calls;
    view->inputs = inputs;
    view->bindings = bindings;
    view->calls = calls;
    return TRUE;
}

static void
DSL_Program_Interface_Mapped_View_Commit
        (const DSL_PROGRAM_INTERFACE_MAPPED_VIEW &view)
{
    DSL_Program_Interface_Image_Reset();
    if (view.header->retired_formal_count != 0)
        DSL_retired_formal_table.Insert
            (view.retired_formals, view.header->retired_formal_count);
    if (view.header->retired_call_argument_count != 0)
        DSL_retired_call_argument_table.Insert
            (view.retired_calls,
             view.header->retired_call_argument_count);
    if (view.header->runtime_input_count != 0)
        DSL_runtime_input_table.Insert
            (view.inputs, view.header->runtime_input_count);
    if (view.header->runtime_input_binding_count != 0)
        DSL_runtime_input_binding_table.Insert
            (view.bindings, view.header->runtime_input_binding_count);
    if (view.header->runtime_input_call_count != 0)
        DSL_runtime_input_call_table.Insert
            (view.calls, view.header->runtime_input_call_count);
    DSL_Program_Interface_Mark_Mapped_Committed();
}

BOOL
DSL_Program_Interface_Image_Load_Mapped
        (const void *section_base, UINT64 section_size, FILE *diagnostic)
{
    DSL_PROGRAM_INTERFACE_MAPPED_VIEW view;
    if (!DSL_Program_Interface_Mapped_View_Parse
             (section_base, section_size, &view, diagnostic))
        return FALSE;
    DSL_Program_Interface_Mapped_View_Commit(view);
    return TRUE;
}

static BOOL
DSL_IR_Lowered_Relation_Views_Validate
        (const DSL_RUNTIME_VALUE_PROJECTION_RECORD *projections,
         UINT32 projection_count,
         const DSL_RUNTIME_CALL_PROJECTION_RECORD *calls,
         UINT32 call_count,
         const DSL_RUNTIME_INPUT_RECORD *inputs,
         UINT32 input_count,
         const DSL_RUNTIME_INPUT_BINDING_RECORD *bindings,
         UINT32 binding_count,
         FILE *diagnostic)
{
    for (UINT32 value_id = 1;
         value_id <= DSL_ir_value_table.Size(); ++value_id) {
        const DSL_IR_VALUE_RECORD &value = DSL_ir_value_table[value_id - 1];
        if ((value.flags & DSL_IR_VALUE_FLAG_DEAD_ELIDED) != 0) {
            UINT32 matched_projections = 0;
            DSL_RUNTIME_VALUE_PROJECTION_ID projection_id = 0;
            for (UINT32 i = 0; i < projection_count; ++i) {
                const DSL_RUNTIME_VALUE_PROJECTION_RECORD &projection =
                    projections[i];
                if (projection.source_value_id != value.id)
                    continue;
                if (++matched_projections > 1 ||
                    projection.source_st != value.st ||
                    projection.source_ty != value.ty ||
                    projection.binding_kind !=
                        DSL_RUNTIME_BINDING_LOCAL_VALUE)
                    return DSL_IR_Image_Report
                               (diagnostic,
                                "invalid dead source projection",
                                value.id);
                projection_id = projection.id;
            }
            for (UINT32 i = 0; i < input_count; ++i) {
                if (inputs[i].source_value_id == value.id)
                    return DSL_IR_Image_Report
                               (diagnostic,
                                "dead source has runtime input", value.id);
            }
            for (UINT32 i = 0; i < call_count; ++i) {
                if (calls[i].source_value_id == value.id ||
                    (projection_id != 0 &&
                     calls[i].value_projection_id == projection_id))
                    return DSL_IR_Image_Report
                               (diagnostic, "dead source has runtime call",
                                value.id);
            }
        }
        if ((value.flags & DSL_IR_VALUE_FLAG_LOWERED) == 0)
            continue;
        UINT32 matched_projections = 0;
        const DSL_RUNTIME_VALUE_PROJECTION_RECORD *matched_projection = NULL;
        for (UINT32 i = 0; i < projection_count; ++i) {
            if (projections[i].source_value_id != value.id)
                continue;
            if (projections[i].source_st != value.st ||
                projections[i].source_ty != value.ty ||
                projections[i].binding_kind !=
                    DSL_RUNTIME_BINDING_LOCAL_VALUE)
                return DSL_IR_Image_Report
                           (diagnostic,
                            "lowered projection mismatch", value.id);
            ++matched_projections;
            matched_projection = &projections[i];
        }
        UINT32 matched_inputs = 0;
        const DSL_RUNTIME_INPUT_RECORD *matched_input = NULL;
        const DSL_RUNTIME_INPUT_BINDING_RECORD *matched_binding = NULL;
        for (UINT32 i = 0; i < input_count; ++i) {
            if (inputs[i].input_kind !=
                    DSL_RUNTIME_INPUT_SOURCE_EXTERNAL_TENSOR ||
                inputs[i].source_value_id != value.id)
                continue;
            if (inputs[i].source_st != value.st ||
                inputs[i].source_ty != value.ty ||
                inputs[i].source_tcon == TCON_IDX_ZERO)
                return DSL_IR_Image_Report
                           (diagnostic,
                            "lowered promoted input mismatch", value.id);
            UINT32 root_count = 0;
            for (UINT32 j = 0; j < binding_count; ++j) {
                if (bindings[j].runtime_input_id == inputs[i].id &&
                    bindings[j].owner_pu_st ==
                        inputs[i].source_owner_pu_st &&
                    bindings[j].binding_kind ==
                        DSL_RUNTIME_INPUT_BINDING_ROOT_PROMOTED_SOURCE &&
                    bindings[j].handle_ty == inputs[i].handle_ty) {
                    ++root_count;
                    matched_binding = &bindings[j];
                }
            }
            if (root_count != 1)
                return DSL_IR_Image_Report
                           (diagnostic,
                            "lowered promoted binding mismatch", value.id);
            ++matched_inputs;
            matched_input = &inputs[i];
        }
        if (matched_projections > 1 || matched_inputs > 1 ||
            (matched_projections == 0 && matched_inputs == 0) ||
            (matched_projections == 1 && matched_inputs == 1 &&
             (matched_projection->owner_pu_st !=
                  matched_input->source_owner_pu_st ||
              matched_projection->handle_ty !=
                  matched_binding->handle_ty ||
              matched_projection->handle_st ==
                  matched_binding->handle_st)))
            return DSL_IR_Image_Report
                       (diagnostic,
                        "ambiguous lowered runtime relation", value.id);
    }
    return TRUE;
}

BOOL
DSL_Program_Runtime_Interface_Images_Load_Mapped
        (const void *program_section_base, UINT64 program_section_size,
         const void *runtime_section_base, UINT64 runtime_section_size,
         FILE *diagnostic)
{
    BOOL has_program = program_section_base != NULL;
    BOOL has_runtime = runtime_section_base != NULL;
    DSL_PROGRAM_INTERFACE_MAPPED_VIEW program_view;
    DSL_RUNTIME_INTERFACE_MAPPED_VIEW runtime_view;
    if (has_program &&
        !DSL_Program_Interface_Mapped_View_Parse
             (program_section_base, program_section_size, &program_view,
              diagnostic))
        return FALSE;
    if (has_runtime &&
        !DSL_Runtime_Interface_Mapped_View_Parse
             (runtime_section_base, runtime_section_size,
              has_program ? program_view.retired_formals : NULL,
              has_program ? program_view.header->retired_formal_count : 0,
              has_program, &runtime_view, diagnostic))
        return FALSE;
    if (has_program &&
        !DSL_Program_Interface_View_Validate
             (program_view.retired_formals,
              program_view.header->retired_formal_count,
              program_view.retired_calls,
              program_view.header->retired_call_argument_count,
              program_view.inputs,
              program_view.header->runtime_input_count,
              program_view.bindings,
              program_view.header->runtime_input_binding_count,
              program_view.calls,
              program_view.header->runtime_input_call_count,
              has_runtime ? runtime_view.values : NULL,
              has_runtime ? runtime_view.header->value_projection_count : 0,
              has_runtime ? runtime_view.calls : NULL,
              has_runtime ? runtime_view.header->call_projection_count : 0,
              TRUE, diagnostic))
        return FALSE;
    if (!DSL_IR_Lowered_Relation_Views_Validate
             (has_runtime ? runtime_view.values : NULL,
              has_runtime ? runtime_view.header->value_projection_count : 0,
              has_runtime ? runtime_view.calls : NULL,
              has_runtime ? runtime_view.header->call_projection_count : 0,
              has_program ? program_view.inputs : NULL,
              has_program ? program_view.header->runtime_input_count : 0,
              has_program ? program_view.bindings : NULL,
              has_program ?
                  program_view.header->runtime_input_binding_count : 0,
              diagnostic))
        return FALSE;
    if (has_program)
        DSL_Program_Interface_Mapped_View_Commit(program_view);
    else
        DSL_Program_Interface_Image_Reset();
    if (has_runtime)
        DSL_Runtime_Interface_Mapped_View_Commit(runtime_view);
    else
        DSL_Runtime_Interface_Image_Reset();
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
DSL_IR_Image_Node_Acyclic
        (const DSL_IR_IMAGE_VIEW *view,
         DSL_IR_NODE_ID node_id,
         std::vector<UINT8> *state)
{
    if ((*state)[node_id - 1] == 1)
        return FALSE;
    if ((*state)[node_id - 1] == 2)
        return TRUE;
    (*state)[node_id - 1] = 1;
    const DSL_IR_NODE_RECORD &node = view->nodes[node_id - 1];
    for (UINT32 i = 0; i < node.operand_count; ++i) {
        const DSL_IR_VALUE_REFERENCE_RECORD &reference =
            view->value_references[node.first_operand_reference_id - 1 + i];
        const DSL_IR_VALUE_RECORD &value = view->values[reference.value_id - 1];
        if (value.producer_node_id != DSL_IR_NODE_INVALID_ID &&
            !DSL_IR_Image_Node_Acyclic
                (view, value.producer_node_id, state))
            return FALSE;
    }
    (*state)[node_id - 1] = 2;
    return TRUE;
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
            (record.flags & ~(DSL_IR_VALUE_FLAG_REDIRECTED |
                              DSL_IR_VALUE_FLAG_LOWERED |
                              DSL_IR_VALUE_FLAG_DEAD_ELIDED)) != 0 ||
            ((record.flags & (record.flags - 1)) != 0) ||
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
                                        DSL_IR_NODE_FLAG_LOWERED |
                                        DSL_IR_NODE_FLAG_DEAD_ELIDED |
                                        DSL_IR_NODE_REDIRECT_ORDINAL_MASK;
        active_attribute_count += record.attribute_count;
        active_value_reference_count += record.operand_count;
        if (record.id != i + 1 || record.opcode_descriptor_id == 0 ||
            record.opcode_descriptor_id > header.opcode_descriptor_count ||
            record.result_value_id == 0 ||
            record.result_value_id > header.value_count ||
            (record.flags & ~valid_node_flags) != 0 ||
            (((record.flags & DSL_IR_NODE_FLAG_RETIRED) != 0) +
             ((record.flags & DSL_IR_NODE_FLAG_LOWERED) != 0) +
             ((record.flags & DSL_IR_NODE_FLAG_DEAD_ELIDED) != 0) > 1) ||
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
        } else if ((record.flags & DSL_IR_NODE_FLAG_LOWERED) != 0) {
            if ((result.flags & DSL_IR_VALUE_FLAG_LOWERED) == 0 ||
                (result.flags & DSL_IR_VALUE_FLAG_REDIRECTED) != 0 ||
                descriptor.effect_model != DSL_EFFECT_MODEL_PURE)
                return DSL_IR_Image_Report
                           (diagnostic, "invalid lowered node", i + 1);
        } else if ((record.flags & DSL_IR_NODE_FLAG_DEAD_ELIDED) != 0) {
            if (result.flags != DSL_IR_VALUE_FLAG_DEAD_ELIDED ||
                result.value_kind != DSL_IR_VALUE_CONSTANT ||
                descriptor.logical_operator != OPR_DSLTENSORCONST ||
                descriptor.effect_model != DSL_EFFECT_MODEL_PURE ||
                record.operand_count != 0)
                return DSL_IR_Image_Report
                           (diagnostic, "invalid dead-elided node", i + 1);
        } else if ((result.flags & (DSL_IR_VALUE_FLAG_REDIRECTED |
                                    DSL_IR_VALUE_FLAG_LOWERED |
                                    DSL_IR_VALUE_FLAG_DEAD_ELIDED)) != 0) {
            return DSL_IR_Image_Report
                       (diagnostic, "nonlive flag on live value", result.id);
        }
    }

    if (active_attribute_count != header.attribute_count ||
        active_value_reference_count != header.value_reference_count)
        return DSL_IR_Image_Report
                   (diagnostic, "unowned relationship record", 0);

    for (UINT32 i = 0; i < header.value_reference_count; ++i) {
        const DSL_IR_VALUE_RECORD &referenced =
            view->values[view->value_references[i].value_id - 1];
        const DSL_IR_NODE_RECORD &owner =
            view->nodes[view->value_references[i].owner_node_id - 1];
        if ((referenced.flags & DSL_IR_VALUE_FLAG_REDIRECTED) != 0)
            return DSL_IR_Image_Report
                       (diagnostic, "reference to redirected value", i + 1);
        if ((referenced.flags & DSL_IR_VALUE_FLAG_LOWERED) != 0 &&
            (owner.flags & (DSL_IR_NODE_FLAG_RETIRED |
                            DSL_IR_NODE_FLAG_LOWERED |
                            DSL_IR_NODE_FLAG_DEAD_ELIDED)) == 0)
            return DSL_IR_Image_Report
                       (diagnostic, "live reference to lowered value", i + 1);
        if ((referenced.flags & DSL_IR_VALUE_FLAG_DEAD_ELIDED) != 0 &&
            (owner.flags & (DSL_IR_NODE_FLAG_RETIRED |
                            DSL_IR_NODE_FLAG_LOWERED |
                            DSL_IR_NODE_FLAG_DEAD_ELIDED)) == 0)
            return DSL_IR_Image_Report
                       (diagnostic, "live reference to dead value", i + 1);
    }

    std::vector<UINT8> node_state(header.node_count, 0);
    for (UINT32 i = 1; i <= header.node_count; ++i) {
        if (!DSL_IR_Image_Node_Acyclic(view, i, &node_state))
            return DSL_IR_Image_Report
                       (diagnostic, "cyclic value dependency", i);
    }

    return TRUE;
}

BOOL
DSL_IR_Image_Retype_Value
        (DSL_IR_VALUE_ID value_id,
         TY_IDX expected_old_ty,
         TY_IDX refined_ty)
{
    DSL_IR_VALUE_RECORD value;
    if (!DSL_IR_Table_Get(DSL_ir_value_table, value_id, &value) ||
        value.ty != expected_old_ty || TY_IDX_index(refined_ty) == 0)
        return FALSE;
    DSL_ir_value_table[value_id - 1].ty = refined_ty;
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
DSL_IR_Image_Mark_Value_Lowered (DSL_IR_VALUE_ID value_id)
{
    DSL_IR_VALUE_RECORD value;
    if (!DSL_IR_Table_Get(DSL_ir_value_table, value_id, &value) ||
        value.producer_node_id == DSL_IR_NODE_INVALID_ID ||
        value.producer_node_id > DSL_ir_node_table.Size() ||
        value.flags != DSL_IR_VALUE_FLAG_NONE)
        return FALSE;
    DSL_IR_NODE_RECORD &node =
        DSL_ir_node_table[value.producer_node_id - 1];
    DSL_IR_OPCODE_DESCRIPTOR_RECORD descriptor;
    if (node.result_value_id != value_id ||
        node.flags != DSL_IR_NODE_FLAG_NONE ||
        !DSL_IR_Table_Get
            (DSL_ir_opcode_descriptor_table, node.opcode_descriptor_id,
             &descriptor) ||
        descriptor.effect_model != DSL_EFFECT_MODEL_PURE)
        return FALSE;
    node.flags = DSL_IR_NODE_FLAG_LOWERED;
    DSL_ir_value_table[value_id - 1].flags = DSL_IR_VALUE_FLAG_LOWERED;
    return TRUE;
}

BOOL
DSL_IR_Image_Mark_Value_Dead_Elided (DSL_IR_VALUE_ID value_id)
{
    DSL_IR_VALUE_RECORD value;
    if (!DSL_IR_Table_Get(DSL_ir_value_table, value_id, &value) ||
        value.producer_node_id == DSL_IR_NODE_INVALID_ID ||
        value.producer_node_id > DSL_ir_node_table.Size() ||
        value.value_kind != DSL_IR_VALUE_CONSTANT ||
        value.flags != DSL_IR_VALUE_FLAG_NONE)
        return FALSE;
    DSL_IR_NODE_RECORD &node =
        DSL_ir_node_table[value.producer_node_id - 1];
    DSL_IR_OPCODE_DESCRIPTOR_RECORD descriptor;
    if (node.result_value_id != value_id ||
        node.flags != DSL_IR_NODE_FLAG_NONE || node.operand_count != 0 ||
        !DSL_IR_Table_Get
            (DSL_ir_opcode_descriptor_table, node.opcode_descriptor_id,
             &descriptor) ||
        descriptor.logical_operator != OPR_DSLTENSORCONST ||
        descriptor.effect_model != DSL_EFFECT_MODEL_PURE)
        return FALSE;
    node.flags = DSL_IR_NODE_FLAG_DEAD_ELIDED;
    DSL_ir_value_table[value_id - 1].flags =
        DSL_IR_VALUE_FLAG_DEAD_ELIDED;
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
        if ((DSL_ir_node_table[i].flags &
             (DSL_IR_NODE_FLAG_RETIRED | DSL_IR_NODE_FLAG_LOWERED |
              DSL_IR_NODE_FLAG_DEAD_ELIDED)) == 0)
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
