/*
 * Copyright (C) 2026 Open64 Project
 *
 * Mapped event-to-step evidence for executable CKKS DSL values. See
 * doc/FHE-SYNC6-NATIVE-CKKS-EXPANSION-CONTRACT.md.
 */

#include <string.h>
#include <map>
#include <set>
#include <string>
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

struct DSL_CKKS_OWNER_VALUE_KEY {
    ST_IDX st;
    std::string name;
    std::string metadata;

    BOOL operator< (const DSL_CKKS_OWNER_VALUE_KEY &other) const
    {
        if (st != other.st)
            return st < other.st;
        if (name != other.name)
            return name < other.name;
        return metadata < other.metadata;
    }
};

typedef std::map<DSL_CKKS_OWNER_VALUE_KEY, DSL_IR_VALUE_ID>
    DSL_CKKS_OWNER_VALUE_INDEX;

/* Match Find_PU_Value's exact owner/name rule without rescanning all values
 * for every source, origin, and result in the event image. A zero index marks
 * an ambiguous owner-local value and must never satisfy an event relation. */
static BOOL
DSL_CKKS_Event_Index_Values (DSL_CKKS_OWNER_VALUE_INDEX *values)
{
    for (UINT32 id = 1; id <= DSL_IR_Image_Value_Count(); ++id) {
        DSL_IR_VALUE_RECORD value;
        if (!DSL_IR_Image_Get_Value(id, &value))
            return FALSE;
        if (value.name == STR_IDX_ZERO || value.metadata == STR_IDX_ZERO)
            continue;
        const char *name = Index_To_Str(value.name);
        const char *metadata = Index_To_Str(value.metadata);
        if (name == NULL || metadata == NULL)
            return FALSE;
        DSL_CKKS_OWNER_VALUE_KEY key;
        key.st = value.st;
        key.name = name;
        key.metadata = metadata;
        std::pair<DSL_CKKS_OWNER_VALUE_INDEX::iterator, bool> inserted =
            values->insert(std::make_pair(key, value.id));
        if (!inserted.second)
            inserted.first->second = DSL_IR_VALUE_INVALID_ID;
    }
    return TRUE;
}

static BOOL
DSL_CKKS_Event_Value_Owned
        (const DSL_IR_VALUE_RECORD &value, ST_IDX owner_pu_st,
         const DSL_CKKS_OWNER_VALUE_INDEX &values)
{
    if (!DSL_IR_Image_PU_ST_Valid(owner_pu_st) ||
        value.name == STR_IDX_ZERO || value.metadata == STR_IDX_ZERO)
        return FALSE;
    const char *name = Index_To_Str(value.name);
    const char *metadata = Index_To_Str(value.metadata);
    if (name == NULL || metadata == NULL)
        return FALSE;
    std::string expected("owner_pu=");
    expected += ST_name(St_Table[owner_pu_st]);
    if (expected != metadata)
        return FALSE;
    DSL_CKKS_OWNER_VALUE_KEY key;
    key.st = value.st;
    key.name = name;
    key.metadata = metadata;
    DSL_CKKS_OWNER_VALUE_INDEX::const_iterator found = values.find(key);
    return found != values.end() && found->second == value.id;
}

struct DSL_CKKS_SOURCE_CONTEXT_KEY {
    ST_IDX owner_pu_st;
    DSL_IR_VALUE_ID source_value_id;
    DSL_PU_SOURCE_IDENTITY_ID context_pu_identity_id;
    DSL_CALLSITE_METADATA_ID context_callsite_id;

    BOOL operator< (const DSL_CKKS_SOURCE_CONTEXT_KEY &other) const
    {
        if (owner_pu_st != other.owner_pu_st)
            return owner_pu_st < other.owner_pu_st;
        if (source_value_id != other.source_value_id)
            return source_value_id < other.source_value_id;
        if (context_pu_identity_id != other.context_pu_identity_id)
            return context_pu_identity_id < other.context_pu_identity_id;
        return context_callsite_id < other.context_callsite_id;
    }
};

struct DSL_CKKS_STATIC_EVENT_KEY {
    DSL_CKKS_SOURCE_CONTEXT_KEY context;
    UINT32 source_static_ordinal;

    BOOL operator< (const DSL_CKKS_STATIC_EVENT_KEY &other) const
    {
        if (context < other.context)
            return TRUE;
        if (other.context < context)
            return FALSE;
        return source_static_ordinal < other.source_static_ordinal;
    }
};

struct DSL_CKKS_RESULT_CONTEXT_KEY {
    ST_IDX owner_pu_st;
    DSL_PU_SOURCE_IDENTITY_ID context_pu_identity_id;
    DSL_CALLSITE_METADATA_ID context_callsite_id;
    DSL_IR_VALUE_ID result_value_id;

    BOOL operator< (const DSL_CKKS_RESULT_CONTEXT_KEY &other) const
    {
        if (owner_pu_st != other.owner_pu_st)
            return owner_pu_st < other.owner_pu_st;
        if (context_pu_identity_id != other.context_pu_identity_id)
            return context_pu_identity_id < other.context_pu_identity_id;
        if (context_callsite_id != other.context_callsite_id)
            return context_callsite_id < other.context_callsite_id;
        return result_value_id < other.result_value_id;
    }
};

struct DSL_CKKS_STATIC_EVENT_INFO {
    UINT32 step_count;
    ST_IDX origin_owner_pu_st;
    DSL_IR_VALUE_ID origin_source_value_id;
    UINT32 origin_static_ordinal;
    std::set<UINT32> step_ordinals;

    DSL_CKKS_STATIC_EVENT_INFO()
        : step_count(0), origin_owner_pu_st(ST_IDX_ZERO),
          origin_source_value_id(0), origin_static_ordinal(0) {}
};

static DSL_CKKS_SOURCE_CONTEXT_KEY
DSL_CKKS_Event_Context_Key (const DSL_CKKS_EVENT_RECORD &row)
{
    DSL_CKKS_SOURCE_CONTEXT_KEY key;
    key.owner_pu_st = row.owner_pu_st;
    key.source_value_id = row.source_value_id;
    key.context_pu_identity_id = row.context_pu_identity_id;
    key.context_callsite_id = row.context_callsite_id;
    return key;
}

static DSL_CKKS_STATIC_EVENT_KEY
DSL_CKKS_Event_Static_Key (const DSL_CKKS_EVENT_RECORD &row)
{
    DSL_CKKS_STATIC_EVENT_KEY key;
    key.context = DSL_CKKS_Event_Context_Key(row);
    key.source_static_ordinal = row.source_static_ordinal;
    return key;
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

    DSL_CKKS_OWNER_VALUE_INDEX owner_values;
    if (!DSL_CKKS_Event_Index_Values(&owner_values))
        return DSL_CKKS_Event_Report
                   (diagnostic, "invalid value ownership index", 0);
    std::map<DSL_CKKS_STATIC_EVENT_KEY, DSL_CKKS_STATIC_EVENT_INFO>
        static_events;
    std::map<DSL_CKKS_SOURCE_CONTEXT_KEY, UINT32> final_counts;
    std::set<DSL_CKKS_RESULT_CONTEXT_KEY> context_results;

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
            !DSL_CKKS_Event_Value_Owned
                (source, row.owner_pu_st, owner_values) ||
            !DSL_IR_Image_Get_Value(row.result_value_id, &result) ||
            !DSL_IR_Image_Get_Node(row.result_node_id, &result_node) ||
            result.producer_node_id != result_node.id ||
            result_node.result_value_id != result.id ||
            !((result_node.flags == DSL_IR_NODE_FLAG_NONE &&
               result.flags == DSL_IR_VALUE_FLAG_NONE) ||
              (result_node.flags == DSL_IR_NODE_FLAG_LOWERED &&
               result.flags == DSL_IR_VALUE_FLAG_LOWERED)) ||
            ((row.flags & DSL_CKKS_EVENT_FINAL_RESULT) != 0 &&
             result.ty != source.ty) ||
            !DSL_CKKS_Event_Value_Owned
                (result, row.owner_pu_st, owner_values) ||
            !DSL_IR_Image_Get_Opcode_Descriptor
                (result_node.opcode_descriptor_id, &opcode) ||
            opcode.logical_operator < OPR_DSLCKKSADD ||
            opcode.logical_operator > OPR_DSLCKKSBOOTSTRAP ||
            !DSL_Call_Image_Get_PU_Identity
                (row.context_pu_identity_id, &identity) ||
            identity.owner_pu_st != row.owner_pu_st ||
            !DSL_IR_Image_Get_Value(row.origin_source_value_id, &origin) ||
            !DSL_CKKS_Event_Value_Owned
                (origin, row.origin_owner_pu_st, owner_values) ||
            (row.context_callsite_id != 0 &&
             (!DSL_Call_Image_Get_Callsite
                  (row.context_callsite_id, &callsite) ||
              callsite.callee_pu_st != row.owner_pu_st ||
              !DSL_IR_Image_PU_ST_Valid(callsite.owner_pu_st))))
            return DSL_CKKS_Event_Report
                       (diagnostic, "invalid event relation", row.id);

        DSL_CKKS_STATIC_EVENT_INFO &info =
            static_events[DSL_CKKS_Event_Static_Key(row)];
        if (info.step_count != 0 &&
            (info.origin_owner_pu_st != row.origin_owner_pu_st ||
             info.origin_source_value_id != row.origin_source_value_id ||
             info.origin_static_ordinal != row.origin_static_ordinal))
            return DSL_CKKS_Event_Report
                       (diagnostic, "inconsistent event origin", row.id);
        if (!info.step_ordinals.insert(row.step_ordinal).second)
            return DSL_CKKS_Event_Report
                       (diagnostic, "duplicate step ordinal", row.id);
        info.origin_owner_pu_st = row.origin_owner_pu_st;
        info.origin_source_value_id = row.origin_source_value_id;
        info.origin_static_ordinal = row.origin_static_ordinal;
        ++info.step_count;
        DSL_CKKS_SOURCE_CONTEXT_KEY context =
            DSL_CKKS_Event_Context_Key(row);
        UINT32 &final_count = final_counts[context];
        if ((row.flags & DSL_CKKS_EVENT_FINAL_RESULT) != 0)
            ++final_count;
        DSL_CKKS_RESULT_CONTEXT_KEY result_key;
        result_key.owner_pu_st = row.owner_pu_st;
        result_key.context_pu_identity_id = row.context_pu_identity_id;
        result_key.context_callsite_id = row.context_callsite_id;
        result_key.result_value_id = row.result_value_id;
        if (!context_results.insert(result_key).second)
            return DSL_CKKS_Event_Report
                       (diagnostic, "reused result in context", row.id);
    }
    for (UINT32 i = 0; i < header->record_count; ++i) {
        const DSL_CKKS_EVENT_RECORD &row = records[i];
        const DSL_CKKS_STATIC_EVENT_INFO &info =
            static_events.find(DSL_CKKS_Event_Static_Key(row))->second;
        const UINT32 final_count =
            final_counts.find(DSL_CKKS_Event_Context_Key(row))->second;
        if (row.step_ordinal >= info.step_count ||
            final_count != 1)
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
