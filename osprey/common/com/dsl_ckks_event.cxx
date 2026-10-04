/*
 * Copyright (C) 2026 Open64 Project
 *
 * Mapped event-to-step evidence for executable CKKS DSL values. See
 * doc/FHE-SYNC6-NATIVE-CKKS-EXPANSION-CONTRACT.md.
 */

#include <string.h>
#include <vector>

#include "dsl_ckks_event_internal.h"
#include "dsl_ckks_expand_internal.h"
#include "dsl_ir_transaction_internal.h"
#include "segmented_array.h"
#include "strtab.h"
#include "symtab.h"

typedef SEGMENTED_ARRAY<DSL_CKKS_EVENT_RECORD> DSL_CKKS_EVENT_TABLE;

static DSL_CKKS_EVENT_TABLE DSL_ckks_event_table;

typedef char DSL_CKKS_Event_Header_Size_Check
    [sizeof(DSL_CKKS_EVENT_IMAGE_HEADER) ==
        DSL_CKKS_EVENT_IMAGE_HEADER_SIZE ? 1 : -1];
typedef char DSL_CKKS_Event_Record_Size_Check
    [sizeof(DSL_CKKS_EVENT_RECORD) == DSL_CKKS_EVENT_RECORD_SIZE ? 1 : -1];

static BOOL
DSL_CKKS_Event_Report (FILE *diagnostic, const char *message, UINT32 id)
{
    if (diagnostic != NULL)
        fprintf(diagnostic, "DSL CKKS event image error: %s id=%u\n",
                message, id);
    return FALSE;
}

void
DSL_CKKS_Event_Image_Reset (void)
{
    DSL_ckks_event_table.Delete_down_to(0);
}

BOOL
DSL_CKKS_Event_Image_Has_Records (void)
{
    return DSL_ckks_event_table.Size() != 0;
}

UINT32
DSL_CKKS_Event_Image_Count (void)
{
    return DSL_ckks_event_table.Size();
}

void
DSL_CKKS_Event_Image_Trim (UINT32 record_count)
{
    if (record_count <= DSL_ckks_event_table.Size())
        DSL_ckks_event_table.Delete_down_to(record_count);
}

void
DSL_CKKS_Event_Image_Get_Header (DSL_CKKS_EVENT_IMAGE_HEADER *header)
{
    if (header == NULL)
        return;
    memset(header, 0, sizeof(*header));
    header->magic = DSL_CKKS_EVENT_IMAGE_MAGIC;
    header->version = DSL_CKKS_EVENT_IMAGE_VERSION;
    header->header_size = DSL_CKKS_EVENT_IMAGE_HEADER_SIZE;
    header->record_size = DSL_CKKS_EVENT_RECORD_SIZE;
    header->record_count = DSL_ckks_event_table.Size();
    header->capabilities = 1;
}

BOOL
DSL_CKKS_Event_Image_Get
        (DSL_CKKS_EVENT_ID id, DSL_CKKS_EVENT_RECORD *record)
{
    if (id == DSL_CKKS_EVENT_INVALID_ID ||
        id > DSL_ckks_event_table.Size() || record == NULL)
        return FALSE;
    *record = DSL_ckks_event_table[id - 1];
    return TRUE;
}

BOOL
DSL_CKKS_Event_Image_Has_Source (DSL_IR_VALUE_ID source_value_id)
{
    for (UINT32 i = 0; i < DSL_ckks_event_table.Size(); ++i) {
        if (DSL_ckks_event_table[i].source_value_id == source_value_id)
            return TRUE;
    }
    return FALSE;
}

/* Commit-only insertion; the native expansion transaction owns preflight. */
DSL_CKKS_EVENT_ID
DSL_CKKS_Event_Image_Add (const DSL_CKKS_EVENT_RECORD *record)
{
    if (record == NULL)
        return DSL_CKKS_EVENT_INVALID_ID;
    DSL_CKKS_EVENT_RECORD copy = *record;
    copy.id = DSL_ckks_event_table.Size() + 1;
    DSL_ckks_event_table.Insert(copy);
    return copy.id;
}

static BOOL
DSL_CKKS_Event_Value_Owned
        (const DSL_IR_VALUE_RECORD &value, ST_IDX owner_pu_st)
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
DSL_CKKS_Event_Same_Source_Context
        (const DSL_CKKS_EVENT_RECORD &left,
         const DSL_CKKS_EVENT_RECORD &right)
{
    return left.owner_pu_st == right.owner_pu_st &&
           left.source_value_id == right.source_value_id &&
           left.context_pu_identity_id == right.context_pu_identity_id &&
           left.context_callsite_id == right.context_callsite_id;
}

static BOOL
DSL_CKKS_Event_Same_Static_Event
        (const DSL_CKKS_EVENT_RECORD &left,
         const DSL_CKKS_EVENT_RECORD &right)
{
    return DSL_CKKS_Event_Same_Source_Context(left, right) &&
           left.source_static_ordinal == right.source_static_ordinal;
}

static BOOL
DSL_CKKS_Event_Validate_View
        (const DSL_CKKS_EVENT_IMAGE_HEADER *header,
         const DSL_CKKS_EVENT_RECORD *records,
         FILE *diagnostic)
{
    if (header == NULL ||
        header->magic != DSL_CKKS_EVENT_IMAGE_MAGIC ||
        header->version != DSL_CKKS_EVENT_IMAGE_VERSION ||
        header->header_size != DSL_CKKS_EVENT_IMAGE_HEADER_SIZE ||
        header->record_size != DSL_CKKS_EVENT_RECORD_SIZE ||
        header->capabilities != 1 || header->flags != 0 ||
        header->reserved != 0 ||
        (header->record_count != 0 && records == NULL))
        return DSL_CKKS_Event_Report(diagnostic, "invalid header", 0);

    for (UINT32 i = 0; i < header->record_count; ++i) {
        const DSL_CKKS_EVENT_RECORD &row = records[i];
        DSL_IR_VALUE_RECORD source;
        DSL_IR_VALUE_RECORD result;
        DSL_IR_VALUE_RECORD origin;
        DSL_IR_NODE_RECORD source_node;
        DSL_IR_NODE_RECORD result_node;
        DSL_IR_OPCODE_DESCRIPTOR_RECORD opcode;
        DSL_PU_SOURCE_IDENTITY_RECORD identity;
        DSL_CALLSITE_METADATA_RECORD callsite;
        if (row.id != i + 1 ||
            row.source_static_ordinal == 0 ||
            row.origin_static_ordinal == 0 ||
            (row.flags & ~DSL_CKKS_EVENT_FINAL_RESULT) != 0 ||
            row.reserved0 != 0 || row.reserved1 != 0 ||
            !DSL_IR_Image_Get_Value(row.source_value_id, &source) ||
            !DSL_IR_Image_Get_Node(row.source_node_id, &source_node) ||
            source.producer_node_id != source_node.id ||
            source_node.result_value_id != source.id ||
            source_node.flags != DSL_IR_NODE_FLAG_LOWERED ||
            source.flags != DSL_IR_VALUE_FLAG_LOWERED ||
            !DSL_CKKS_Event_Value_Owned(source, row.owner_pu_st) ||
            !DSL_IR_Image_Get_Value(row.result_value_id, &result) ||
            !DSL_IR_Image_Get_Node(row.result_node_id, &result_node) ||
            result.producer_node_id != result_node.id ||
            result_node.result_value_id != result.id ||
            result_node.flags != DSL_IR_NODE_FLAG_NONE ||
            result.flags != DSL_IR_VALUE_FLAG_NONE ||
            ((row.flags & DSL_CKKS_EVENT_FINAL_RESULT) != 0 &&
             result.ty != source.ty) ||
            !DSL_CKKS_Event_Value_Owned(result, row.owner_pu_st) ||
            !DSL_IR_Image_Get_Opcode_Descriptor
                (result_node.opcode_descriptor_id, &opcode) ||
            opcode.logical_operator < OPR_DSLCKKSADD ||
            opcode.logical_operator > OPR_DSLCKKSBOOTSTRAP ||
            !DSL_Call_Image_Get_PU_Identity
                (row.context_pu_identity_id, &identity) ||
            identity.owner_pu_st != row.owner_pu_st ||
            !DSL_IR_Image_Get_Value(row.origin_source_value_id, &origin) ||
            !DSL_CKKS_Event_Value_Owned(origin, row.origin_owner_pu_st) ||
            (row.context_callsite_id != 0 &&
             (!DSL_Call_Image_Get_Callsite
                  (row.context_callsite_id, &callsite) ||
              callsite.callee_pu_st != row.owner_pu_st ||
              !DSL_IR_Image_PU_ST_Valid(callsite.owner_pu_st))))
            return DSL_CKKS_Event_Report
                       (diagnostic, "invalid event relation", row.id);

        UINT32 step_count = 0;
        UINT32 final_count = 0;
        for (UINT32 j = 0; j < header->record_count; ++j) {
            const DSL_CKKS_EVENT_RECORD &other = records[j];
            if (DSL_CKKS_Event_Same_Static_Event(row, other)) {
                ++step_count;
                if (row.origin_owner_pu_st !=
                        other.origin_owner_pu_st ||
                    row.origin_source_value_id !=
                        other.origin_source_value_id ||
                    row.origin_static_ordinal !=
                        other.origin_static_ordinal)
                    return DSL_CKKS_Event_Report
                               (diagnostic, "inconsistent event origin",
                                row.id);
                if (i != j && row.step_ordinal == other.step_ordinal)
                    return DSL_CKKS_Event_Report
                               (diagnostic, "duplicate step ordinal", row.id);
            }
            if (DSL_CKKS_Event_Same_Source_Context(row, other) &&
                (other.flags & DSL_CKKS_EVENT_FINAL_RESULT) != 0)
                ++final_count;
            if (i != j && row.owner_pu_st == other.owner_pu_st &&
                row.context_pu_identity_id ==
                    other.context_pu_identity_id &&
                row.context_callsite_id == other.context_callsite_id &&
                row.result_value_id == other.result_value_id)
                return DSL_CKKS_Event_Report
                           (diagnostic, "reused result in context", row.id);
        }
        if (row.step_ordinal >= step_count || final_count != 1)
            return DSL_CKKS_Event_Report
                       (diagnostic, "incomplete event ordinals", row.id);
    }
    return TRUE;
}

BOOL
DSL_CKKS_Event_Image_Validate (FILE *diagnostic)
{
    DSL_CKKS_EVENT_IMAGE_HEADER header;
    DSL_CKKS_Event_Image_Get_Header(&header);
    std::vector<DSL_CKKS_EVENT_RECORD> records;
    records.reserve(DSL_ckks_event_table.Size());
    for (UINT32 i = 0; i < DSL_ckks_event_table.Size(); ++i)
        records.push_back(DSL_ckks_event_table[i]);
    return DSL_CKKS_Event_Validate_View
               (&header, records.empty() ? NULL : &records[0], diagnostic);
}

BOOL
DSL_CKKS_Event_Image_Load_Mapped
        (const void *section_base, UINT64 section_size, FILE *diagnostic)
{
    if (section_base == NULL && section_size == 0) {
        DSL_CKKS_Event_Image_Reset();
        return TRUE;
    }
    if (section_base == NULL || section_size < DSL_CKKS_EVENT_IMAGE_HEADER_SIZE)
        return DSL_CKKS_Event_Report(diagnostic, "truncated section", 0);
    const DSL_CKKS_EVENT_IMAGE_HEADER *header =
        (const DSL_CKKS_EVENT_IMAGE_HEADER *)section_base;
    if (header->record_count >
            (section_size - DSL_CKKS_EVENT_IMAGE_HEADER_SIZE) /
                DSL_CKKS_EVENT_RECORD_SIZE ||
        section_size != DSL_CKKS_EVENT_IMAGE_HEADER_SIZE +
                        (UINT64)header->record_count *
                            DSL_CKKS_EVENT_RECORD_SIZE)
        return DSL_CKKS_Event_Report(diagnostic, "section size mismatch", 0);
    const DSL_CKKS_EVENT_RECORD *records =
        (const DSL_CKKS_EVENT_RECORD *)
            ((const char *)section_base + DSL_CKKS_EVENT_IMAGE_HEADER_SIZE);
    if (!DSL_CKKS_Event_Validate_View(header, records, diagnostic))
        return FALSE;
    DSL_CKKS_Event_Image_Reset();
    if (header->record_count != 0)
        DSL_ckks_event_table.Insert(records, header->record_count);
    return TRUE;
}

void
DSL_CKKS_Event_Image_Print (FILE *file)
{
    if (file == NULL || !DSL_CKKS_Event_Image_Has_Records())
        return;
    fprintf(file, "\nCKKS Event Image: version=1 records=%u\n",
            DSL_CKKS_Event_Image_Count());
    for (UINT32 i = 1; i <= DSL_CKKS_Event_Image_Count(); ++i) {
        DSL_CKKS_EVENT_RECORD row;
        DSL_CKKS_Event_Image_Get(i, &row);
        fprintf(file,
                "  [%u] owner=<%u,%u> source=value%u "
                "static_ordinal=%u context_identity=%u callsite=%u "
                "step=%u result=value%u origin=<%u,%u>/value%u/%u "
                "final=%s\n",
                row.id, ST_IDX_level(row.owner_pu_st),
                ST_IDX_index(row.owner_pu_st), row.source_value_id,
                row.source_static_ordinal, row.context_pu_identity_id,
                row.context_callsite_id, row.step_ordinal,
                row.result_value_id, ST_IDX_level(row.origin_owner_pu_st),
                ST_IDX_index(row.origin_owner_pu_st),
                row.origin_source_value_id, row.origin_static_ordinal,
                (row.flags & DSL_CKKS_EVENT_FINAL_RESULT) ? "true" : "false");
    }
}
