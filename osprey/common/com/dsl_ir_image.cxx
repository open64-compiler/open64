/*
 * Copyright (C) 2026 Open64 Project
 */

#include <string.h>

#include "dsl_ir_image.h"
#include "dsl_opcode.h"
#include "segmented_array.h"
#include "strtab.h"

typedef SEGMENTED_ARRAY<DSL_IR_OPCODE_DESCRIPTOR_RECORD>
    DSL_IR_OPCODE_DESCRIPTOR_TABLE;
typedef SEGMENTED_ARRAY<DSL_IR_NODE_RECORD> DSL_IR_NODE_TABLE;
typedef SEGMENTED_ARRAY<DSL_IR_ATTRIBUTE_RECORD> DSL_IR_ATTRIBUTE_TABLE;
typedef SEGMENTED_ARRAY<DSL_IR_VALUE_RECORD> DSL_IR_VALUE_TABLE;
typedef SEGMENTED_ARRAY<DSL_IR_VALUE_REFERENCE_RECORD>
    DSL_IR_VALUE_REFERENCE_TABLE;
typedef SEGMENTED_ARRAY<DSL_STATE_OBJECT_RECORD> DSL_STATE_OBJECT_TABLE;
typedef SEGMENTED_ARRAY<DSL_STATE_EFFECT_RECORD> DSL_STATE_EFFECT_TABLE;

static DSL_IR_OPCODE_DESCRIPTOR_TABLE DSL_ir_opcode_descriptor_table;
static DSL_IR_NODE_TABLE DSL_ir_node_table;
static DSL_IR_ATTRIBUTE_TABLE DSL_ir_attribute_table;
static DSL_IR_VALUE_TABLE DSL_ir_value_table;
static DSL_IR_VALUE_REFERENCE_TABLE DSL_ir_value_reference_table;
static DSL_STATE_OBJECT_TABLE DSL_state_object_table;
static DSL_STATE_EFFECT_TABLE DSL_state_effect_table;

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
        if (record.id != i + 1 || record.opcode_descriptor_id == 0 ||
            record.opcode_descriptor_id > header.opcode_descriptor_count ||
            record.result_value_id == 0 ||
            record.result_value_id > header.value_count ||
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
    }

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
