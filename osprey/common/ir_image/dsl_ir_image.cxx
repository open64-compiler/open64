/*
 * Copyright (C) 2026 Open64 Project
 */

/*
 * common/ir_image owns managed DSL rows using the existing SEGMENTED_ARRAY
 * and WHIRL mapped-image services. Associations and transaction support are
 * separate from persisted scalar/table identities. Callers establish image and
 * active-PU lifetimes; process-visible rows do not authorize cross-PU mutation.
 * Append helpers perform limited construction checks, while explicit validators
 * and owning phases establish complete structural/domain contracts. This file
 * does not select optimization policy or define a parallel IR file format.
 * Design: doc/WHIRL-DSL-INFRASTRUCTURE.md, fixed-row mapped-image contracts.
 */

#include <string.h>
#include "dsl_lowered_relation.h"
#include "dsl_image_service.h"
#include <string>
#include <vector>

#include "dsl_ir_image.h"
#include "dsl_pu_specialize_internal.h"
#include "dsl_image_checkpoint_internal.h"
#include "dsl_ckks_event.h"
#include "dsl_opcode.h"
#include "dsl_shape.h"
#include "../../be/com/dsl_program_interface_internal.h"
#include "segmented_array.h"
#include "strtab.h"

/* Structural predicate implemented in dsl_ir_rewrite.cxx: check source value
 * ownership/ST/TY agreement, local-level nonzero source/handle ST indices and
 * pointer handle TY, using valid shared core/type/string state. Read-only; no
 * local-ST residency, physical handle or full projection-row/role proof. */
extern BOOL DSL_Runtime_Interface_Value_Record_Contract_Valid
                                (const DSL_RUNTIME_VALUE_PROJECTION_RECORD *);
/* External shared predicate: require a nonzero in-range TY whose kind is
 * POINTER; it does not validate the pointee or a physical handle symbol. */
extern BOOL DSL_Program_Interface_Pointer_TY_Contract_Valid (TY_IDX);
/* External runtime-input profile check using valid source/type/TCON/string
 * tables. External tensors also consult local reference facts if their owner
 * is already Current_pu; caller supplies that matching local context. No PU
 * activation or mutation; row IDs/roles/flags/reserved checks remain separate. */
extern BOOL DSL_Program_Interface_Runtime_Input_Contract_Valid
                                (const DSL_RUNTIME_INPUT_RECORD *);
/* Retire rewrite-owned external-source snapshots and advance their nonzero
 * handle generation during whole-image reset. Does not reset persistent rows,
 * free external files or restore an earlier handle generation; no PU switch. */
extern void DSL_IR_External_Tensor_Source_Reset (void);

typedef struct wn_map_tab WN_MAP_TAB;
extern WN_MAP_TAB *Current_Map_Tab;
/* Existing WN-map service used by call finalization: require live maptab/WN
 * and an allocated INT32 map of the correct kind. Read the node's mapped word,
 * returning zero for an unassigned/out-of-capacity node ID, not for invalid
 * map arguments. No map allocation or image mutation; caller interprets zero
 * as a missing output offset. */
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

/* Test presence/offset bounds in the current string buffer, not string content. */
static BOOL DSL_IR_Image_String_Id_Valid (STR_IDX id, BOOL required);
/* Optionally write a caller-stream error and return FALSE without table changes. */
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

/* Zero caller-owned POD storage if present, without allocating a table row. */
template <typename RECORD>
static void
DSL_IR_Record_Init (RECORD *record)
{
    if (record != NULL)
        memset (record, 0, sizeof(RECORD));
}

/*
 * Check a one-based ID before optional row copying. NULL output is a bounds
 * probe; invalid IDs write nothing. Referenced identities are not validated.
 */
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

/*
 * Retire external-source handles, truncate core/state/effect rows and delegate
 * dependent image resets. This is a whole-image lifetime service, not rollback
 * for one PU; shared strings, ST/TY tables and opcode/domain registries survive.
 */
void
DSL_IR_Image_Reset (void)
{
    DSL_IR_External_Tensor_Source_Reset();
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
    DSL_CKKS_Event_Image_Reset();
}

/* Report one call-image error if a stream is supplied, then return FALSE.
 * Does not mutate image state or propagate fprintf status. */
static BOOL
DSL_Call_Image_Report (FILE *diagnostic, const char *message, UINT32 id)
{
    if (diagnostic != NULL)
        fprintf(diagnostic, "DSL call image error: %s id=%u\n", message, id);
    return FALSE;
}

/* Clear the caller's header and fill magic/version and current row counts.
 * NULL is a no-op; counts are not validated and tables are unchanged. */
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

/* Drop persisted call rows and borrowed associations without freeing WNs.
 * IDs may be reused; dependent ABI/interface/core rows are not cleared here,
 * so callers must coordinate their invalidation or broader image reset. */
void
DSL_Call_Image_Reset (void)
{
    DSL_pu_source_identity_table.Delete_down_to(0);
    DSL_callsite_metadata_table.Delete_down_to(0);
    DSL_callsite_runtime_associations.clear();
}

/* Test either persisted call table for rows, not association occupancy. */
BOOL
DSL_Call_Image_Has_Records (void)
{
    return DSL_pu_source_identity_table.Size() != 0 ||
           DSL_callsite_metadata_table.Size() != 0;
}

/* Find any nonnull borrowed call with this ID. This presence test does not
 * check owner, row bounds, WN opcode or tree lifetime. No state is changed. */
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

/* For the first association with record.id, require a nonzero matching owner
 * and nonnull call. With none, accept a nonzero stored offset. This is only a
 * metadata consistency check, not offset bounds, CALL/callee or PU legality. */
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

/* Scan identity and callsite rows using current shared strings: enforce row
 * IDs, required strings/nonzero symbol indices, source line/reserved rules,
 * unique identity owners and association-or-offset consistency. The first
 * error returns FALSE with optional diagnostics; tables are unchanged.
 * Do not infer symbol liveness, flags/source-ordinal legality, native CALL
 * identity or offset bounds, or require every callee to have an identity row. */
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

/* Preflight required fields, copy the row and assign its one-based table ID.
 * Validate the whole call image, truncating only this append on FALSE; return
 * INVALID_ID on rejection. Duplicate owners are rejected, not interned. Input
 * and shared strings are unchanged; allocation exceptions have no rollback. */
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

/* Require matching nonzero owner and nonnull inputs, append a copied row with
 * fresh ID, then borrow call in the association vector. On whole-call-image
 * validation failure, pop/truncate the append and return INVALID_ID. Input
 * stays unchanged; exceptions during allocation need not undo earlier writes.
 * Caller owns physical CALL/callee proof and the borrowed WN/tree lifetime. */
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

/* Scan for the first exact owner; copy its row or use NULL as a presence
 * probe. FALSE leaves output unchanged. Shared string IDs are not deep copies. */
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

/* Resolve the first pointer-equal borrowed association to its table row, with
 * NULL output allowed as a probe. Failure leaves output unchanged. Stored
 * offsets alone are not searched; mapped rows need Load_PU associations. */
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

/* Return the first borrowed association pointer for id, or NULL if absent.
 * No row-bound or WN validity check; the caller retains tree lifetime control. */
const WN *
DSL_Call_Image_Get_Call_WN (DSL_CALLSITE_METADATA_ID id)
{
    for (UINT32 i = 0; i < DSL_callsite_runtime_associations.size(); ++i) {
        if (DSL_callsite_runtime_associations[i].id == id)
            return DSL_callsite_runtime_associations[i].call;
    }
    return NULL;
}

/* Commit-only compare/replace of the first association matching id/expected.
 * Require nonzero ID and nonnull pointers; FALSE changes nothing. This neither
 * replaces a physical tree node nor updates/validates callsite metadata.
 * The owning transaction supplies legal, live WNs and broader atomicity. */
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

/* Terminal-transaction helper: preflight the new callee's identity and matching
 * runtime-input call owners, compare/replace the borrowed WN, then retarget
 * callsite and selected runtime-input metadata. If call-image validation fails,
 * restore those metadata fields and the association before returning FALSE.
 * Does not perform or undo physical WN replacement or prove symbol/type/PU
 * legality; the owning program transaction supplies those and broader rollback.
 * Scratch allocation precedes mutation; no exception rollback is promised. */
BOOL
DSL_Call_Image_Retarget_Call_WN
        (DSL_CALLSITE_METADATA_ID callsite_id, const WN *expected,
         WN *replacement, ST_IDX new_callee)
{
    if (callsite_id == DSL_CALLSITE_METADATA_INVALID_ID ||
        callsite_id > DSL_callsite_metadata_table.Size() ||
        expected == NULL || replacement == NULL ||
        new_callee == ST_IDX_ZERO)
        return FALSE;
    DSL_PU_SOURCE_IDENTITY_RECORD identity;
    if (!DSL_Call_Image_Find_PU_Identity(new_callee, &identity))
        return FALSE;
    DSL_CALLSITE_METADATA_RECORD &record =
        DSL_callsite_metadata_table[callsite_id - 1];
    ST_IDX old_callee = record.callee_pu_st;
    std::vector<UINT32> retargeted_runtime_calls;
    for (UINT32 i = 0; i < DSL_runtime_input_call_table.Size(); ++i) {
        const DSL_RUNTIME_INPUT_CALL_RECORD &runtime_call =
            DSL_runtime_input_call_table[i];
        if (runtime_call.callsite_id != callsite_id)
            continue;
        if (runtime_call.callee_owner_pu_st != old_callee)
            return FALSE;
        retargeted_runtime_calls.push_back(i);
    }
    if (!DSL_Call_Image_Replace_Call_WN
            (callsite_id, expected, replacement))
        return FALSE;
    record.callee_pu_st = new_callee;
    for (UINT32 i = 0; i < retargeted_runtime_calls.size(); ++i)
        DSL_runtime_input_call_table[retargeted_runtime_calls[i]]
            .callee_owner_pu_st = new_callee;
    if (!DSL_Call_Image_Validate(NULL)) {
        for (UINT32 i = 0; i < retargeted_runtime_calls.size(); ++i)
            DSL_runtime_input_call_table[retargeted_runtime_calls[i]]
                .callee_owner_pu_st = old_callee;
        record.callee_pu_st = old_callee;
        DSL_Call_Image_Replace_Call_WN
            (callsite_id, replacement, const_cast<WN *>(expected));
        return FALSE;
    }
    return TRUE;
}

/* Return persisted identity table size without validation or mutation. */
UINT32 DSL_Call_Image_PU_Identity_Count (void)
{ return DSL_pu_source_identity_table.Size(); }

/* Return persisted callsite size, independently of runtime associations. */
UINT32 DSL_Call_Image_Callsite_Count (void)
{ return DSL_callsite_metadata_table.Size(); }

/* Copy a one-based identity row, or probe with NULL. Invalid IDs leave output
 * unchanged; copied STR_IDX fields still refer to shared string storage. */
BOOL DSL_Call_Image_Get_PU_Identity
        (DSL_PU_SOURCE_IDENTITY_ID id, DSL_PU_SOURCE_IDENTITY_RECORD *record)
{ return DSL_IR_Table_Get(DSL_pu_source_identity_table, id, record); }

/* Copy a one-based callsite row, or probe with NULL; invalid IDs leave output
 * unchanged. Does not turn the persisted offset into a runtime WN pointer. */
BOOL DSL_Call_Image_Get_Callsite
        (DSL_CALLSITE_METADATA_ID id, DSL_CALLSITE_METADATA_RECORD *record)
{ return DSL_IR_Table_Get(DSL_callsite_metadata_table, id, record); }

/* Reject zero symbol index, then test stored callsite owner equality. No tree
 * walk, association validation or activation of the named PU is performed. */
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

/* With writer-established Current_Map_Tab/INT32 offset map and live, valid
 * associations, publish each selected owner's WN offset, then validate all
 * call rows. No call rows means TRUE without inspecting off_map. A missing
 * map, zero mapped offset or final validation failure returns FALSE; earlier
 * writes remain. This does not establish PU context or prove all owner rows
 * have freshly mapped associations. */
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

/* Check nonzero owner index/base/size, then append borrowed associations for
 * that owner's offsets in (0, tree_size). The reader must separately prove
 * WN boundaries, full extents and CALL/callee legality, and retain the tree.
 * Never clears/deduplicates associations or switches PUs. A later bad offset
 * or allocation failure leaves earlier appends; no transactional rollback. */
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

/* From an aligned, readable buffer independent of live tables, verify exact
 * header/size before mutation. Then reset call rows/associations, copy rows and
 * validate using loaded shared strings. With associations cleared, every call
 * needs a nonzero persisted offset; tree/offset bounds remain reader-owned.
 * Invalid rows leave call tables empty; old rows/associations are not restored.
 * Success retains no section pointers and needs Load_PU for WN lookups. Other
 * image families are untouched. NULL is invalid (reader handles absence), NULL
 * diagnostic is silent, and allocation failures have no rollback guarantee. */
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

/* Optionally print one ABI-image error and return FALSE without table writes.
 * A NULL diagnostic is silent; stream errors are not propagated. */
static BOOL
DSL_Call_ABI_Image_Report (FILE *diagnostic, const char *message, UINT32 id)
{
    if (diagnostic != NULL)
        fprintf(diagnostic, "DSL call ABI image error: %s id=%u\n",
                message, id);
    return FALSE;
}

/* Clear the supplied header and fill magic/version and argument count.
 * NULL is a no-op; no table validation or mutation occurs. */
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

/* Discard only ABI argument rows, not call/core tables or physical WN trees. */
void
DSL_Call_ABI_Image_Reset (void)
{
    DSL_call_argument_table.Delete_down_to(0);
}

/* Test argument table occupancy without validation or mutation. */
BOOL
DSL_Call_ABI_Image_Has_Records (void)
{
    return DSL_call_argument_table.Size() != 0;
}

/* Check sequential IDs, call/value links, non-sentinel ordinals, zero flags and
 * role string IDs in current call/core/string context. Pairwise scans reject
 * duplicate callsite/actual slots and compare role string contents for equal
 * callee/formal identities. FALSE reports the first mismatch; NULL is silent.
 * Tables are unchanged. This does not validate native slot bounds, ST/TY or
 * owner compatibility, passing mode, contiguous slots or whole call images. */
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

/* Resolve call's borrowed association and require matching record.callsite_id.
 * Copy the row with a fresh ID, then validate the whole ABI table; on FALSE,
 * truncate this append and return INVALID_ID. Input/shared strings and physical
 * actuals are untouched; allocation exceptions have no rollback guarantee. */
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

/* Return all argument rows, without validation or mutation. */
UINT32
DSL_Call_ABI_Image_Argument_Count (void)
{
    return DSL_call_argument_table.Size();
}

/* Copy the one-based argument row or probe with NULL output; invalid IDs leave
 * output unchanged. The copied role remains a shared STR_IDX, not owned text. */
BOOL
DSL_Call_ABI_Image_Get_Argument
        (DSL_CALL_ARGUMENT_ID id, DSL_CALL_ARGUMENT_RECORD *record)
{
    return DSL_IR_Table_Get(DSL_call_argument_table, id, record);
}

/* Scan for the first exact callsite/actual key without validating key bounds.
 * Copy its row or probe with NULL; FALSE leaves output unchanged. */
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

/* Commit-only metadata shift: require a valid callsite/nonzero count, matching
 * actual/formal ordinals and the implemented overflow preflight, then increase
 * both ordinals at/above first_ordinal and validate the whole ABI table. FALSE
 * before the shift leaves state alone, but final validation has no rollback
 * (including a shift landing on INVALID_ORDINAL). No physical actual/formal
 * edits occur; the owning transaction must provide broader atomicity. */
BOOL
DSL_Call_ABI_Image_Shift_Arguments
        (DSL_CALLSITE_METADATA_ID callsite_id,
         UINT32 first_ordinal, UINT32 count)
{
    if (count == 0 ||
        callsite_id == DSL_CALLSITE_METADATA_INVALID_ID ||
        callsite_id > DSL_callsite_metadata_table.Size() ||
        first_ordinal == DSL_CALL_ARGUMENT_INVALID_ORDINAL)
        return FALSE;
    for (UINT32 i = 0; i < DSL_call_argument_table.Size(); ++i) {
        const DSL_CALL_ARGUMENT_RECORD &argument =
            DSL_call_argument_table[i];
        if (argument.callsite_id != callsite_id)
            continue;
        if (argument.actual_ordinal !=
            argument.callee_formal_ordinal ||
            (argument.actual_ordinal >= first_ordinal &&
             argument.actual_ordinal >
                 DSL_CALL_ARGUMENT_INVALID_ORDINAL - count))
            return FALSE;
    }
    for (UINT32 i = 0; i < DSL_call_argument_table.Size(); ++i) {
        DSL_CALL_ARGUMENT_RECORD &argument = DSL_call_argument_table[i];
        if (argument.callsite_id == callsite_id &&
            argument.actual_ordinal >= first_ordinal) {
            argument.actual_ordinal += count;
            argument.callee_formal_ordinal += count;
        }
    }
    return DSL_Call_ABI_Image_Validate(NULL);
}

/* Resolve the borrowed call pointer, then search its numeric actual slot.
 * NULL output probes presence; FALSE leaves output unchanged. This does not
 * inspect physical actuals or validate their bounds. */
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

/* Commit-only value-ID compare/replace for an associated call/actual. Require
 * an in-range nonzero replacement ID; a matched row with unexpected old value
 * returns FALSE unchanged. No matching argument is a successful no-op. Does
 * not rewrite physical actuals, validate replacement type/owner/liveness or run
 * image validators; the owning transaction supplies those obligations. */
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

/* Count argument rows whose resolved callsite names this callee/formal pair,
 * skipping missing callsites. This is a global metadata census only, not
 * permission to inspect/transform the opposite PU or switch its context. */
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

/* Select index as a zero-based ordinal among matching callee/formal rows in
 * table order, skipping missing callsites. Copy the row or probe with NULL;
 * FALSE leaves output unchanged. No physical formals or PU contexts are used. */
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

/* Require an aligned, readable section independent of live tables and loaded
 * call/core/string context. Check exact header/size before replacing argument
 * rows, then validate copied rows; rejection after replacement clears the ABI
 * table rather than restoring old data. Other tables are unchanged, success
 * retains no input pointers, and allocation failures have no rollback promise.
 * NULL is invalid, not an absent-section protocol; NULL diagnostic is silent. */
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

/* Optionally print one PU-interface image error, then return FALSE without
 * changing tables. NULL is silent; fprintf status is not propagated. */
static BOOL
DSL_PU_Interface_Image_Report
        (FILE *diagnostic, const char *message, UINT32 id)
{
    if (diagnostic != NULL)
        fprintf(diagnostic, "DSL PU interface image error: %s id=%u\n",
                message, id);
    return FALSE;
}

/* Clear the caller's header, then fill magic/version and formal table size.
 * NULL is a no-op; this does not validate rows or change image state. */
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

/* Drop only persisted formal rows; IDs may be reused. No physical formals or
 * source values are deleted, and dependent runtime/program interface rows are
 * not reset. Caller coordinates their invalidation or a whole-image reset. */
void
DSL_PU_Interface_Image_Reset (void)
{
    DSL_pu_formal_table.Delete_down_to(0);
}

/* Test table occupancy, not whether a selected PU has live physical formals.
 * Retired/projected source rows still count; no filtering or mutation. */
BOOL
DSL_PU_Interface_Image_Has_Records (void)
{
    return DSL_pu_formal_table.Size() != 0;
}

/* Reject NULL, copy the row with its fresh one-based ID, then use the external
 * structural validator on the complete formal table. FALSE truncates only this
 * append and returns INVALID_ID; duplicate relations are rejected, not interned.
 * Input is unchanged; no physical entry edits, active-PU proof or allocation
 * exception rollback. Global ST/PU and source-value context are caller-owned. */
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

/* Return table size, including retired/projected source rows; no physical
 * formal census, validation or mutation. */
UINT32
DSL_PU_Interface_Image_Formal_Count (void)
{
    return DSL_pu_formal_table.Size();
}

/* Use the shared one-based bounds check for an optional scalar-row copy.
 * NULL output probes presence; invalid IDs leave output unchanged. Does not
 * resolve ST/TY/value references or inspect the owning PU. */
BOOL
DSL_PU_Interface_Image_Get_Formal
        (DSL_PU_FORMAL_ID id, DSL_PU_FORMAL_RECORD *record)
{
    return DSL_IR_Table_Get(DSL_pu_formal_table, id, record);
}

/* Scan for the first exact owner/source-ordinal pair without revalidating
 * either key. Copy its row or probe with NULL; FALSE leaves output unchanged.
 * Does not activate the named PU or translate to a current physical position. */
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

/* Commit-only source-ordinal shift. Preflight nonzero count/owner, a valid
 * nonsentinel first ordinal, the owner's call-image identity and the overflow
 * bound before shifting matching rows. Then run the whole formal-table
 * structural validator; FALSE there does not undo shifts, including a shift
 * landing on INVALID_ORDINAL. No physical entry or related ABI/projection rows
 * are changed. The owning transaction supplies broader rollback and rebuilds
 * the physical interface before active-PU verification observes these rows. */
BOOL
DSL_PU_Interface_Image_Shift_Formals
        (ST_IDX owner_pu_st, UINT32 first_ordinal, UINT32 count)
{
    if (count == 0 || first_ordinal == DSL_PU_FORMAL_INVALID_ORDINAL ||
        owner_pu_st == ST_IDX_ZERO)
        return FALSE;
    DSL_PU_SOURCE_IDENTITY_RECORD identity;
    if (!DSL_Call_Image_Find_PU_Identity(owner_pu_st, &identity))
        return FALSE;
    for (UINT32 i = 0; i < DSL_pu_formal_table.Size(); ++i) {
        const DSL_PU_FORMAL_RECORD &formal = DSL_pu_formal_table[i];
        if (formal.owner_pu_st == owner_pu_st &&
            formal.formal_ordinal >= first_ordinal &&
            formal.formal_ordinal >
                DSL_PU_FORMAL_INVALID_ORDINAL - count)
            return FALSE;
    }
    for (UINT32 i = 0; i < DSL_pu_formal_table.Size(); ++i) {
        DSL_PU_FORMAL_RECORD &formal = DSL_pu_formal_table[i];
        if (formal.owner_pu_st == owner_pu_st &&
            formal.formal_ordinal >= first_ordinal)
            formal.formal_ordinal += count;
    }
    return DSL_PU_Interface_Image_Validate(NULL);
}

/* From an aligned/readable section independent of live formal storage, verify
 * exact header/size before mutation. Reset the formal table, copy rows, then
 * structurally validate against loaded global ST/PU and core-value context.
 * Row failure clears replacements, not restores old rows; allocation failure
 * has no rollback guarantee. Other tables/trees are untouched and no section
 * pointers retained. NULL is invalid (reader handles absence by reset); NULL
 * diagnostic is silent. Physical order/identity still needs active-PU checks. */
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

/* Optionally print one runtime-image error and return FALSE without mutation.
 * NULL is silent; stream errors are not propagated. */
static BOOL
DSL_Runtime_Interface_Report
        (FILE *diagnostic, const char *message, UINT32 id)
{
    if (diagnostic != NULL)
        fprintf(diagnostic, "DSL runtime interface image error: %s id=%u\n",
                message, id);
    return FALSE;
}

/* Scan a readable count-sized borrowed array for the first owner/value pair.
 * Return a borrowed element or NULL, without filtering retirement or validating
 * keys/uniqueness. The caller keeps array storage stable while using the result. */
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

/* Validate readable count-sized borrowed projection arrays in valid shared
 * core/ST/TY/string/call/ABI/formal context. Formal retirement comes from the
 * supplied array when use_retired_view is TRUE, otherwise live program rows;
 * source retirement/redirect evidence always comes from the current core image.
 * Check row identity, source ownership/handle types, formal provenance, retired
 * view targets, uniqueness, call direction/type agreement and complete coverage.
 * No live mutation or PU activation; first error returns FALSE with optional
 * diagnostic. This does not prove physical WNs/local ST residency or validate
 * the complete program image. Referenced core/type/string state is prevalidated. */
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
            (record.flags &
             ~DSL_RUNTIME_VALUE_PROJECTION_FLAG_RETIRED_VIEW) != 0 ||
            record.reserved0 != 0 ||
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
        DSL_IR_VALUE_RECORD source_value;
        DSL_IR_NODE_RECORD source_node;
        BOOL view_retired = FALSE;
        if (!DSL_IR_Image_Get_Value(record.source_value_id, &source_value) ||
            (source_value.producer_node_id != DSL_IR_NODE_INVALID_ID &&
             !DSL_IR_Image_Get_Node
                 (source_value.producer_node_id, &source_node)))
            return DSL_Runtime_Interface_Report
                       (diagnostic, "invalid projection source", i + 1);
        if (source_value.producer_node_id != DSL_IR_NODE_INVALID_ID)
            view_retired =
                (source_node.flags & DSL_IR_NODE_FLAG_RETIRED_VIEW) != 0;
        if (view_retired !=
            ((record.flags &
              DSL_RUNTIME_VALUE_PROJECTION_FLAG_RETIRED_VIEW) != 0))
            return DSL_Runtime_Interface_Report
                       (diagnostic, "projection retirement mismatch", i + 1);
        if ((record.flags &
             DSL_RUNTIME_VALUE_PROJECTION_FLAG_RETIRED_VIEW) != 0) {
            DSL_IR_VALUE_ID target = DSL_IR_VALUE_INVALID_ID;
            if (!DSL_IR_Image_Value_Redirect_Target
                    (source_value.id, &target))
                return DSL_Runtime_Interface_Report
                           (diagnostic, "invalid retired view projection",
                            i + 1);
            BOOL target_projection = FALSE;
            for (UINT32 j = 0; j < value_count; ++j) {
                if (values[j].owner_pu_st == record.owner_pu_st &&
                    values[j].source_value_id == target &&
                    (values[j].flags &
                     DSL_RUNTIME_VALUE_PROJECTION_FLAG_RETIRED_VIEW) == 0 &&
                    values[j].handle_ty == record.handle_ty)
                    target_projection = TRUE;
            }
            if (is_formal || !target_projection)
                return DSL_Runtime_Interface_Report
                           (diagnostic, "invalid retired view projection",
                            i + 1);
        }
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

    /* Relate each proposed call to both source and callee-formal projections;
     * input roles additionally retain their original call-ABI value identity. */
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
            (value->flags &
             DSL_RUNTIME_VALUE_PROJECTION_FLAG_RETIRED_VIEW) != 0 ||
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

    /* Coverage uses all stored call/formal metadata, not an active PU tree.
     * Zero input relations denotes a result; more than one is ambiguous. */
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

/* Cross-check a proposed readable runtime view against snapshots of the five
 * live program tables, before runtime-image replacement. Requires valid shared
 * source/type/string/call/formal context; no live writes or PU activation.
 * FALSE reports the first mismatch; scratch allocation exceptions propagate. */
static BOOL DSL_Program_Interface_Validate_Runtime_View
        (const DSL_RUNTIME_VALUE_PROJECTION_RECORD *values,
         UINT32 value_count,
         const DSL_RUNTIME_CALL_PROJECTION_RECORD *calls,
         UINT32 call_count, FILE *diagnostic);

/* Zero the caller's header and fill magic/version plus both current counts.
 * NULL is a no-op; tables are neither validated nor changed. */
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

/* Discard only projection rows, not source/ABI/formal/program data or WNs.
 * IDs may be reused; dependent invalidation and physical reconstruction belong
 * to the caller or a broader image reset. */
void
DSL_Runtime_Interface_Image_Reset (void)
{
    DSL_runtime_value_projection_table.Delete_down_to(0);
    DSL_runtime_call_projection_table.Delete_down_to(0);
}

/* Test either table's occupancy, including retired values. Does not walk WNs
 * or establish that an empty image satisfies current call/formal coverage. */
BOOL
DSL_Runtime_Interface_Image_Has_Records (void)
{
    return DSL_runtime_value_projection_table.Size() != 0 ||
           DSL_runtime_call_projection_table.Size() != 0;
}

/* Snapshot both segmented tables into vectors and run the shared view checker
 * with live retired-formal context. Caller supplies valid referenced shared
 * state; live rows remain unchanged and no PU is activated. Does not invoke the
 * program-image cross-validator or physical-PU verifier. FALSE is the checker's
 * first reported error; NULL diagnostic is silent, allocation exceptions escape. */
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

/* Return all value projection rows, including retired provenance, without
 * filtering or validation. */
UINT32
DSL_Runtime_Interface_Image_Value_Count (void)
{
    return DSL_runtime_value_projection_table.Size();
}

/* Return call projection table size, independently of physical CALL trees. */
UINT32
DSL_Runtime_Interface_Image_Call_Count (void)
{
    return DSL_runtime_call_projection_table.Size();
}

/* Shared one-based bounds check and optional scalar-row copy, retired rows
 * included. NULL probes presence; invalid IDs leave output unchanged. No ST/TY
 * resolution, handle ownership transfer or PU activation. */
BOOL
DSL_Runtime_Interface_Image_Get_Value
        (DSL_RUNTIME_VALUE_PROJECTION_ID id,
         DSL_RUNTIME_VALUE_PROJECTION_RECORD *record)
{
    return DSL_IR_Table_Get(DSL_runtime_value_projection_table, id, record);
}

/* Copy a one-based call projection or probe with NULL; invalid IDs leave output
 * unchanged. Does not resolve its IDs into physical calls or switch PU context. */
BOOL
DSL_Runtime_Interface_Image_Get_Call
        (DSL_RUNTIME_CALL_PROJECTION_ID id,
         DSL_RUNTIME_CALL_PROJECTION_RECORD *record)
{
    return DSL_IR_Table_Get(DSL_runtime_call_projection_table, id, record);
}

/* Scan the first exact owner/source pair whose RETIRED_VIEW bit is clear.
 * NULL output probes existence; failure leaves it unchanged. No key validation
 * or PU activation. Unlike Get_Value, this deliberately hides retired rows. */
BOOL
DSL_Runtime_Interface_Image_Find_Value
        (ST_IDX owner_pu_st, DSL_IR_VALUE_ID source_value_id,
         DSL_RUNTIME_VALUE_PROJECTION_RECORD *record)
{
    for (UINT32 i = 0; i < DSL_runtime_value_projection_table.Size(); ++i) {
        const DSL_RUNTIME_VALUE_PROJECTION_RECORD &current =
            DSL_runtime_value_projection_table[i];
        if (current.owner_pu_st == owner_pu_st &&
            (current.flags &
             DSL_RUNTIME_VALUE_PROJECTION_FLAG_RETIRED_VIEW) == 0 &&
            current.source_value_id == source_value_id) {
            if (record != NULL)
                *record = current;
            return TRUE;
        }
    }
    return FALSE;
}

/* Read-only retirement preflight: absence is TRUE, otherwise require exactly
 * one unflagged local projection, no referencing call projection, and an
 * unretired replacement lookup with equal handle TY. Does not require distinct
 * source/replacement IDs or prove physical handle uses, core retirement or
 * redirect legality; the owning native-view transaction supplies those checks. */
BOOL
DSL_Runtime_Interface_Image_Can_Retire_View
        (ST_IDX owner_pu_st, DSL_IR_VALUE_ID source_value_id,
         DSL_IR_VALUE_ID replacement_value_id)
{
    UINT32 matches = 0;
    DSL_RUNTIME_VALUE_PROJECTION_RECORD source_projection;
    DSL_RUNTIME_VALUE_PROJECTION_RECORD replacement_projection;
    for (UINT32 i = 0; i < DSL_runtime_value_projection_table.Size(); ++i) {
        const DSL_RUNTIME_VALUE_PROJECTION_RECORD &value =
            DSL_runtime_value_projection_table[i];
        if (value.owner_pu_st != owner_pu_st ||
            value.source_value_id != source_value_id)
            continue;
        if (++matches != 1 ||
            value.binding_kind != DSL_RUNTIME_BINDING_LOCAL_VALUE ||
            value.flags != 0)
            return FALSE;
        source_projection = value;
        for (UINT32 j = 0; j < DSL_runtime_call_projection_table.Size(); ++j) {
            if (DSL_runtime_call_projection_table[j].value_projection_id ==
                value.id)
                return FALSE;
        }
    }
    if (matches != 0 &&
        (!DSL_Runtime_Interface_Image_Find_Value
             (owner_pu_st, replacement_value_id,
              &replacement_projection) ||
         replacement_projection.handle_ty != source_projection.handle_ty))
        return FALSE;
    return TRUE;
}

/* Recheck retirement preflight, then set only the source projection's retired
 * provenance bit. FALSE changes nothing; absent source is a TRUE no-op. Does
 * not set a replacement link, retire core rows, rewrite WNs or validate the
 * resulting image. Caller owns core redirect/retirement ordering, physical-use
 * proof, postconditions and any broader rollback. */
BOOL
DSL_Runtime_Interface_Image_Retire_View
        (ST_IDX owner_pu_st, DSL_IR_VALUE_ID source_value_id,
         DSL_IR_VALUE_ID replacement_value_id)
{
    if (!DSL_Runtime_Interface_Image_Can_Retire_View
             (owner_pu_st, source_value_id, replacement_value_id))
        return FALSE;
    for (UINT32 i = 0; i < DSL_runtime_value_projection_table.Size(); ++i) {
        DSL_RUNTIME_VALUE_PROJECTION_RECORD &value =
            DSL_runtime_value_projection_table[i];
        if (value.owner_pu_st == owner_pu_st &&
            value.source_value_id == source_value_id) {
            value.flags |= DSL_RUNTIME_VALUE_PROJECTION_FLAG_RETIRED_VIEW;
            break;
        }
    }
    return TRUE;
}

/* Scan the first exact callsite/actual pair, with optional scalar copy or NULL
 * probe. FALSE leaves output unchanged; keys/WNs are not validated and no PU
 * context is established. */
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

/*
 * Commit-only metadata redirect for distinct existing equal-TY source values.
 * With no source projection, return TRUE unchanged. Otherwise require a unique
 * local source projection with matching ST/TY, validate a copied replacement
 * record and preflight referring call source IDs before writes. Preserve the
 * handle/row identity, update source value/ST/TY and related call source IDs.
 * FALSE leaves rows unchanged; no full-view uniqueness/retirement validation
 * or physical rewrite occurs. The owning transaction establishes legality,
 * verifies postconditions and provides rollback of the broader replacement.
 */
BOOL
DSL_Runtime_Interface_Image_Redirect_Value
        (ST_IDX owner_pu_st, DSL_IR_VALUE_ID source_value_id,
         DSL_IR_VALUE_ID replacement_value_id)
{
    DSL_IR_VALUE_RECORD source;
    DSL_IR_VALUE_RECORD replacement;
    if (source_value_id == replacement_value_id ||
        !DSL_IR_Image_Get_Value(source_value_id, &source) ||
        !DSL_IR_Image_Get_Value(replacement_value_id, &replacement) ||
        source.ty != replacement.ty)
        return FALSE;

    UINT32 projection_index = DSL_runtime_value_projection_table.Size();
    for (UINT32 i = 0; i < DSL_runtime_value_projection_table.Size(); ++i) {
        const DSL_RUNTIME_VALUE_PROJECTION_RECORD &current =
            DSL_runtime_value_projection_table[i];
        if (current.owner_pu_st != owner_pu_st ||
            current.source_value_id != source_value_id)
            continue;
        if (projection_index != DSL_runtime_value_projection_table.Size() ||
            current.source_st != source.st || current.source_ty != source.ty ||
            current.binding_kind != DSL_RUNTIME_BINDING_LOCAL_VALUE)
            return FALSE;
        projection_index = i;
    }
    if (projection_index == DSL_runtime_value_projection_table.Size())
        return TRUE;

    DSL_RUNTIME_VALUE_PROJECTION_RECORD redirected =
        DSL_runtime_value_projection_table[projection_index];
    redirected.source_value_id = replacement.id;
    redirected.source_st = replacement.st;
    redirected.source_ty = replacement.ty;
    if (!DSL_Runtime_Interface_Value_Record_Contract_Valid(&redirected))
        return FALSE;
    for (UINT32 i = 0; i < DSL_runtime_call_projection_table.Size(); ++i) {
        const DSL_RUNTIME_CALL_PROJECTION_RECORD &call =
            DSL_runtime_call_projection_table[i];
        if (call.value_projection_id == redirected.id &&
            call.source_value_id != source_value_id)
            return FALSE;
    }

    DSL_runtime_value_projection_table[projection_index] = redirected;
    for (UINT32 i = 0; i < DSL_runtime_call_projection_table.Size(); ++i) {
        DSL_RUNTIME_CALL_PROJECTION_RECORD &call =
            DSL_runtime_call_projection_table[i];
        if (call.value_projection_id == redirected.id)
            call.source_value_id = replacement.id;
    }
    return TRUE;
}

/* Commit-only append of a copied value projection with a fresh one-based ID.
 * Reject NULL only; no ownership/type/role/uniqueness checks or image validation.
 * Input is unchanged, scalar IDs retain shared identity. Owning transaction
 * supplies preflight/postconditions/rollback; allocation failures escape. */
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

/* Commit-only append of a copied call projection with a fresh one-based ID.
 * Reject NULL only; links, roles and complete coverage remain caller-validated.
 * Input/physical calls are unchanged, and broader transaction rollback and
 * allocation-failure handling are not provided here. */
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

/* Preflight an aligned/readable section and output view: subtract checked row
 * extents from remaining bytes, require exact size, then validate borrowed rows
 * with supplied-or-live retired-formal context. Caller supplies valid shared
 * tables and readable retired rows when selected. On FALSE, live tables and
 * output view are unchanged; NULL diagnostic is silent. On TRUE, view borrows
 * the section until commit; no physical proof or program-image cross-check. */
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

/* Commit a previously parsed/validated view whose independent section storage
 * remains readable: reset both projection tables and copy rows into them.
 * Retain no input pointers; no other tables/trees change. No revalidation or
 * allocation-exception rollback is provided after the reset. */
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

/* Parse/validate proposed rows with live retired-formal state, then, if program
 * rows exist, cross-check their snapshot against this runtime view before any
 * live mutation. Boolean rejection preserves old projections. Require aligned,
 * readable independent section storage and valid shared core/ST/TY/string and
 * call/ABI/formal context throughout. Commit resets/copies only projections;
 * allocation failure there has no rollback guarantee. No retained section
 * pointers, PU activation or physical ABI proof. NULL section is invalid (reader
 * handles absence); NULL diagnostic is silent. */
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

/* Optionally print one program-image error and return FALSE without changing
 * image state. NULL is silent; fprintf status is not propagated. */
static BOOL
DSL_Program_Interface_Report
        (FILE *diagnostic, const char *message, UINT32 id)
{
    if (diagnostic != NULL)
        fprintf(diagnostic, "DSL program interface image error: %s id=%u\n",
                message, id);
    return FALSE;
}

/* Require a nonzero numeric owner with a call-image source identity. This is
 * a metadata presence check, not live ST/PU resolution or scope activation. */
static BOOL
DSL_Program_Interface_PU_Valid (ST_IDX owner_pu_st)
{
    DSL_PU_SOURCE_IDENTITY_RECORD identity;
    return owner_pu_st != ST_IDX_ZERO &&
           DSL_Call_Image_Find_PU_Identity(owner_pu_st, &identity);
}

/* Return the first formal-ID match in a readable contiguous count-sized view,
 * or NULL. Result borrows that storage; no key/uniqueness validation or writes. */
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

/* Return a borrowed first argument-ID match from a readable contiguous view,
 * or NULL; caller owns view lifetime and key/uniqueness validation. */
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

/* Find the first owner/final-ordinal pair in a readable contiguous view.
 * Return a borrowed element or NULL; no PU activation or key validation. */
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

/* Find the first owner/source pair in a readable count-sized view, including
 * retired projection rows. Return borrowed storage or NULL; do not validate it. */
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

/* Find the first callsite/actual pair in a readable count-sized view. Return
 * a borrowed element or NULL, without checking direction or physical WNs. */
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

/* Count stored source formals for owner and subtract matching retirement rows,
 * clamping at zero. Caller has validated the readable retirement view; this
 * does not prove distinct retirement membership or inspect physical formals. */
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

/* Order validated bindings by kind, role text, then input source owner/value
 * and input ID; missing input links fall back to numeric IDs. Caller supplies
 * readable arrays and valid role strings. No mutation, PU lookup or policy. */
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

/* Check readable count-sized program views against valid shared core/ST/TY/
 * TCON/string/call/ABI/formal facts: exact retirement provenance and incoming
 * coverage, runtime input profiles, canonical binding order, root ownership,
 * matching flow endpoints, root coverage, acyclicity/reachability and incoming
 * threaded-call coverage. With validate_runtime_projections, also require each
 * canonical formal/argument to be exactly retired or present in the supplied
 * runtime views; those views are not fully validated here. External-input
 * checks may consult an already-active source PU's matching local context.
 * No live writes or PU activation; first mismatch returns FALSE with optional
 * diagnostics. Deadness/physical ABI/domain proof remains phase-owned, and
 * scratch allocation exceptions are not converted into Boolean failures. */
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

    /* Standalone program parsing omits this cross-image exclusivity gate;
     * coordinated loads enable it only after both proposed views are available. */
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

    /* Input profiles bind explicit resources or owner-qualified source values;
     * physical launcher and handle interpretation is deliberately elsewhere. */
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

    /* Closure is scratch metadata evidence only: every binding must be rooted
     * and cycle-free, without walking or switching any PU's physical tree. */
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

/* Clear the caller header and fill magic/version plus five table sizes.
 * NULL is a no-op; no validation or image mutation occurs. */
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

/* Discard all five program tables and reset process-local plan/commit evidence.
 * IDs may be reused. Core/call/formal/runtime-projection tables and physical
 * WNs survive; dependent invalidation/reconstruction remains caller-owned. */
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

/* Test the five program table sizes only, not local commit evidence or WNs.
 * Empty rows do not establish compatibility with current projection coverage. */
BOOL
DSL_Program_Interface_Image_Has_Records (void)
{
    return DSL_retired_formal_table.Size() != 0 ||
           DSL_retired_call_argument_table.Size() != 0 ||
           DSL_runtime_input_table.Size() != 0 ||
           DSL_runtime_input_binding_table.Size() != 0 ||
           DSL_runtime_input_call_table.Size() != 0;
}

/* Snapshot five program tables and both runtime-projection tables into vectors,
 * then run the shared checker with runtime exclusivity enabled. Caller supplies
 * valid shared references and any already-active source-PU local context. No
 * live mutation, PU activation, full runtime-view or physical ABI validation;
 * NULL diagnostic is silent and scratch allocation exceptions propagate. */
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

/* Snapshot only live program tables and cross-check them against the supplied
 * readable runtime arrays before runtime-image commit. The shared checker has
 * runtime exclusivity enabled, not full runtime-row verification. No live
 * mutation; valid shared/active-source context and array lifetimes are caller
 * obligations. FALSE reports first mismatch; scratch exceptions propagate. */
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

/* Define a trivial stored-row count accessor. Generated bodies neither filter
 * liveness nor validate rows; invocation comments identify the provenance kind. */
#define DSL_PROGRAM_INTERFACE_COUNT_GETTER(name, table) \
UINT32 name (void) { return table.Size(); }

/* Count formal-retirement provenance, not remaining physical formals. */
DSL_PROGRAM_INTERFACE_COUNT_GETTER
    (DSL_Program_Interface_Image_Retired_Formal_Count,
     DSL_retired_formal_table)
/* Count retired canonical call arguments without inspecting CALL kids. */
DSL_PROGRAM_INTERFACE_COUNT_GETTER
    (DSL_Program_Interface_Image_Retired_Call_Count,
     DSL_retired_call_argument_table)
/* Count input records independently of root/binding reachability. */
DSL_PROGRAM_INTERFACE_COUNT_GETTER
    (DSL_Program_Interface_Image_Runtime_Input_Count,
     DSL_runtime_input_table)
/* Count root and threaded binding rows without physical interface traversal. */
DSL_PROGRAM_INTERFACE_COUNT_GETTER
    (DSL_Program_Interface_Image_Runtime_Binding_Count,
     DSL_runtime_input_binding_table)
/* Count stored input-flow edges, not physical/dynamic call executions. */
DSL_PROGRAM_INTERFACE_COUNT_GETTER
    (DSL_Program_Interface_Image_Runtime_Call_Count,
     DSL_runtime_input_call_table)

#undef DSL_PROGRAM_INTERFACE_COUNT_GETTER

/* Bounded one-based optional retirement-row copy through the shared accessor.
 * NULL probes presence; invalid IDs leave output unchanged. No ST/TY resolution. */
BOOL
DSL_Program_Interface_Image_Get_Retired_Formal
        (DSL_RETIRED_FORMAL_ID id, DSL_RETIRED_FORMAL_RECORD *record)
{
    return DSL_IR_Table_Get(DSL_retired_formal_table, id, record);
}

/* Copy a one-based call-retirement row or probe with NULL; invalid IDs leave
 * output unchanged. Canonical IDs/ordinals are copied, not physically resolved. */
BOOL
DSL_Program_Interface_Image_Get_Retired_Call
        (DSL_RETIRED_CALL_ARGUMENT_ID id,
         DSL_RETIRED_CALL_ARGUMENT_RECORD *record)
{
    return DSL_IR_Table_Get(DSL_retired_call_argument_table, id, record);
}

/* Copy a one-based input row or probe with NULL; invalid IDs leave output
 * unchanged. Role/type/TCON/value IDs keep shared identity, not new ownership. */
BOOL
DSL_Program_Interface_Image_Get_Runtime_Input
        (DSL_RUNTIME_INPUT_ID id, DSL_RUNTIME_INPUT_RECORD *record)
{
    return DSL_IR_Table_Get(DSL_runtime_input_table, id, record);
}

/* Copy a one-based binding row or probe with NULL; invalid IDs leave output
 * unchanged. Does not dereference a local handle ST or establish PU context. */
BOOL
DSL_Program_Interface_Image_Get_Runtime_Binding
        (DSL_RUNTIME_INPUT_BINDING_ID id,
         DSL_RUNTIME_INPUT_BINDING_RECORD *record)
{
    return DSL_IR_Table_Get(DSL_runtime_input_binding_table, id, record);
}

/* Copy a one-based input-flow call row or probe with NULL; invalid IDs leave
 * output unchanged. Does not inspect physical calls or switch either PU. */
BOOL
DSL_Program_Interface_Image_Get_Runtime_Call
        (DSL_RUNTIME_INPUT_CALL_ID id, DSL_RUNTIME_INPUT_CALL_RECORD *record)
{
    return DSL_IR_Table_Get(DSL_runtime_input_call_table, id, record);
}

/* Scan a flat view rooted at table[0] for a canonical-formal ID; optional output
 * is copied only on success. This assumes all populated rows are contiguous,
 * unlike general SEGMENTED_ARRAY growth; indexed Get/count traversal avoids
 * that storage assumption. No key validation or physical formal lookup. */
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

/* Scan a table[0]-based flat view for the first canonical-argument ID. NULL
 * probes presence; failure leaves output unchanged. Requires contiguous full
 * populated storage, not guaranteed across segments; indexed Get/count access
 * avoids the assumption. No physical actual validation or PU activation. */
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

/* Scan a table[0]-based flat view for the first owner/final-ordinal key; optional
 * output changes only on success. The scan assumes contiguous populated rows,
 * which segmented growth does not ensure; indexed Get/count traversal avoids
 * that assumption. Does not validate keys/handle STs or activate the owner PU. */
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

/* Read a LOWERED value, count source-ID-keyed projection/input matches and
 * require one root-promoted binding per matching external input. Reject missing
 * or ambiguous candidates before touching output; then clear result/set source
 * IDs and check computed or promoted ST/TY/handle agreement. Late rejection
 * leaves those output writes, so FALSE is not an output-preserving probe.
 * Success returns a primary relation plus any ancillary projection with zero
 * inserted statements. Requires valid shared rows; no full owner/node/TCON,
 * physical ABI, private-dependency or CKKS-only relation proof. Tables are not
 * mutated and no PU is activated. */
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
    /* Output publication starts before the final agreement checks; callers
     * must ignore all result fields unless the function returns TRUE. */
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

/* Validate CKKS events, derive scratch reachability from resolvable lowered
 * runtime values, and check dead/CKKS/runtime/private relation rules on live
 * rows. Caller has validated core producer/operand ranges and shared image
 * tables; dead-call lookup also inherits the current contiguous retirement
 * storage requirement. This is not the proposed-view paired-loader checker or
 * a complete runtime/program/core/physical validator. No live writes or PU
 * activation; FALSE reports the first mismatch, NULL diagnostic is silent,
 * and scratch allocation exceptions are not converted into FALSE. */
BOOL
DSL_IR_Image_Validate_Lowered_Relations (FILE *diagnostic)
{
    if (!DSL_CKKS_Event_Image_Validate(diagnostic))
        return FALSE;

    /* Seed every resolvable lowered runtime value, including promoted roots,
     * then walk lowered operands backward. Group-private values need no direct
     * relation when this derived dependency path reaches them. */
    std::vector<UINT8> reaches_projection
        (DSL_ir_value_table.Size() + 1, 0);
    std::vector<DSL_IR_VALUE_ID> worklist;
    for (UINT32 i = 1; i <= DSL_ir_value_table.Size(); ++i) {
        const DSL_IR_VALUE_RECORD &value = DSL_ir_value_table[i - 1];
        DSL_IR_NATIVE_VALUE_LOWER_RESULT relation;
        if ((value.flags & DSL_IR_VALUE_FLAG_LOWERED) != 0 &&
            DSL_IR_Image_Resolve_Lowered_Relation(i, &relation)) {
            reaches_projection[i] = 1;
            worklist.push_back(i);
        }
    }
    while (!worklist.empty()) {
        DSL_IR_VALUE_ID value_id = worklist.back();
        worklist.pop_back();
        const DSL_IR_VALUE_RECORD &value = DSL_ir_value_table[value_id - 1];
        if (value.producer_node_id == 0 ||
            value.producer_node_id > DSL_ir_node_table.Size())
            continue;
        const DSL_IR_NODE_RECORD &node =
            DSL_ir_node_table[value.producer_node_id - 1];
        if ((node.flags & DSL_IR_NODE_FLAG_LOWERED) == 0)
            continue;
        for (UINT32 kid = 0; kid < node.operand_count; ++kid) {
            const DSL_IR_VALUE_REFERENCE_RECORD &reference =
                DSL_ir_value_reference_table
                    [node.first_operand_reference_id - 1 + kid];
            if (reference.value_id == 0 ||
                reference.value_id > DSL_ir_value_table.Size() ||
                reaches_projection[reference.value_id] != 0 ||
                (DSL_ir_value_table[reference.value_id - 1].flags &
                 DSL_IR_VALUE_FLAG_LOWERED) == 0)
                continue;
            reaches_projection[reference.value_id] = 1;
            worklist.push_back(reference.value_id);
        }
    }
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
            for (UINT32 j = 0; j < DSL_call_argument_table.Size(); ++j) {
                const DSL_CALL_ARGUMENT_RECORD &argument =
                    DSL_call_argument_table[j];
                if (argument.argument_value_id != value.id)
                    continue;
                DSL_RETIRED_CALL_ARGUMENT_RECORD retired;
                if (!DSL_Program_Interface_Image_Find_Retired_Call
                        (argument.id, &retired) ||
                    retired.callsite_id != argument.callsite_id ||
                    retired.argument_value_id != value.id ||
                    retired.old_actual_ordinal != argument.actual_ordinal ||
                    retired.old_callee_formal_ordinal !=
                        argument.callee_formal_ordinal ||
                    retired.retirement_reason !=
                        DSL_INTERFACE_RETIREMENT_VERIFIED_DEAD_INPUT)
                    return DSL_IR_Image_Report
                               (diagnostic,
                                "dead source has live call argument", i);
            }
        }
        if ((value.flags & DSL_IR_VALUE_FLAG_LOWERED) == 0)
            continue;
        if (DSL_CKKS_Event_Image_Has_Source(value.id)) {
            for (UINT32 j = 0;
                 j < DSL_runtime_value_projection_table.Size(); ++j) {
                if (DSL_runtime_value_projection_table[j].source_value_id ==
                        value.id)
                    return DSL_IR_Image_Report
                               (diagnostic,
                                "ambiguous CKKS/runtime lowering", i);
            }
            for (UINT32 j = 0; j < DSL_runtime_input_table.Size(); ++j) {
                if (DSL_runtime_input_table[j].source_value_id == value.id)
                    return DSL_IR_Image_Report
                               (diagnostic,
                                "ambiguous CKKS/runtime lowering", i);
            }
            continue;
        }
        DSL_IR_NATIVE_VALUE_LOWER_RESULT relation;
        if (!DSL_IR_Image_Resolve_Lowered_Relation(i, &relation) &&
            !reaches_projection[i])
            return DSL_IR_Image_Report
                       (diagnostic, "invalid lowered runtime relation", i);
        if (!DSL_IR_Image_Resolve_Lowered_Relation(i, &relation)) {
            for (UINT32 j = 0;
                 j < DSL_runtime_value_projection_table.Size(); ++j) {
                if (DSL_runtime_value_projection_table[j].source_value_id == i)
                    return DSL_IR_Image_Report
                               (diagnostic, "private value has projection", i);
            }
            for (UINT32 j = 0; j < DSL_runtime_input_table.Size(); ++j) {
                if (DSL_runtime_input_table[j].source_value_id == i)
                    return DSL_IR_Image_Report
                               (diagnostic, "private value has input", i);
            }
            for (UINT32 j = 0; j < DSL_call_argument_table.Size(); ++j) {
                if (DSL_call_argument_table[j].argument_value_id == i)
                    return DSL_IR_Image_Report
                               (diagnostic, "private value has call argument", i);
            }
        }
    }
    return TRUE;
}

/* Commit-only copied append with fresh one-based ID, rejecting NULL only.
 * Does not prove deadness/coverage or prune physical formals; input is unchanged.
 * Owning transaction supplies preflight, verification and broader rollback. */
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

/* Commit-only copied append with fresh ID, rejecting NULL only. Canonical
 * actuals/metadata are not removed or validated here; caller owns complete
 * retirement coverage, physical edits and broader transaction rollback. */
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

/* Commit-only copied input append with fresh ID; NULL is the only rejection.
 * Shared role/type/TCON/value IDs are not resolved or owned anew. No input
 * profile/uniqueness validation, physical promotion or rollback is provided. */
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

/* Commit-only copied binding append with fresh ID, rejecting NULL only. Caller
 * supplies handle/owner/order legality and physical formal reconstruction;
 * no validation or broader rollback occurs, and allocation exceptions escape. */
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

/* Commit-only copied flow-edge append with fresh ID, rejecting NULL only.
 * Does not validate endpoints/coverage/cycles or edit physical CALLs. Owning
 * transaction supplies preflight/postconditions and broader rollback. */
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

/* Preflight an aligned/readable program section and output view: require exact
 * header/row extents, borrow its five arrays and validate program relationships
 * without runtime-projection exclusivity. Require valid shared source/type/
 * string/TCON/call/formal context and matching locals for an already-active
 * external-source PU. FALSE preserves live state and output view; TRUE assigns
 * borrowed pointers, so caller retains section lifetime through commit. No
 * lowered-relation/physical proof; NULL diagnostic is silent. */
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
    /* Check each remaining extent before subtraction to avoid overflowing the
     * section-size calculation. This macro may return only before mutation. */
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

/* Commit a previously validated view with independent readable section storage:
 * reset five program tables/process-local commit state, copy rows, then mark
 * mapped eligibility for later per-PU verification. No input pointers retained
 * or physical tree edits. Allocation exceptions during copying have no rollback
 * guarantee; the final eligibility mark is not a physical-correctness proof. */
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

/* Parse/validate program-only structure and flow before replacing live rows.
 * FALSE leaves program tables/commit evidence intact. Success resets/copies
 * them and marks mapped eligibility without retaining section pointers; commit
 * allocation exceptions have no rollback guarantee. Caller provides aligned,
 * readable independent storage and valid shared/source context. Runtime-view
 * exclusivity and lowered-relation checks belong to the paired loader, not
 * this path. NULL input is invalid; NULL diagnostic is silent. */
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

/* Read-only paired-load preflight using readable proposed runtime/program
 * arrays and valid current core reference ranges plus CKKS facts. Walk lowered
 * operand dependencies back from projected values, then check dead-source
 * retirement and lowered projection/promoted-input/private-dependency relations,
 * including CKKS/runtime exclusion. Scratch reachability does not inspect WNs,
 * prove physical handles, or replace full row/domain validation. First mismatch
 * returns FALSE with optional diagnostics; allocation exceptions propagate. */
static BOOL
DSL_IR_Lowered_Relation_Views_Validate
        (const DSL_RUNTIME_VALUE_PROJECTION_RECORD *projections,
         UINT32 projection_count,
         const DSL_RUNTIME_CALL_PROJECTION_RECORD *calls,
         UINT32 call_count,
         const DSL_RETIRED_CALL_ARGUMENT_RECORD *retired_calls,
         UINT32 retired_call_count,
         const DSL_RUNTIME_INPUT_RECORD *inputs,
         UINT32 input_count,
         const DSL_RUNTIME_INPUT_BINDING_RECORD *bindings,
         UINT32 binding_count,
         FILE *diagnostic)
{
    /* Reachability admits private lowered dependencies only when connected to
     * a projected lowered result; core operand ranges are prevalidated. */
    std::vector<UINT8> reaches_projection
        (DSL_ir_value_table.Size() + 1, 0);
    std::vector<DSL_IR_VALUE_ID> worklist;
    for (UINT32 i = 0; i < projection_count; ++i) {
        DSL_IR_VALUE_ID value_id = projections[i].source_value_id;
        if (value_id == 0 || value_id > DSL_ir_value_table.Size() ||
            reaches_projection[value_id] != 0 ||
            (DSL_ir_value_table[value_id - 1].flags &
             DSL_IR_VALUE_FLAG_LOWERED) == 0)
            continue;
        reaches_projection[value_id] = 1;
        worklist.push_back(value_id);
    }
    while (!worklist.empty()) {
        DSL_IR_VALUE_ID value_id = worklist.back();
        worklist.pop_back();
        const DSL_IR_VALUE_RECORD &value = DSL_ir_value_table[value_id - 1];
        if (value.producer_node_id == 0 ||
            value.producer_node_id > DSL_ir_node_table.Size())
            continue;
        const DSL_IR_NODE_RECORD &node =
            DSL_ir_node_table[value.producer_node_id - 1];
        if ((node.flags & DSL_IR_NODE_FLAG_LOWERED) == 0)
            continue;
        for (UINT32 kid = 0; kid < node.operand_count; ++kid) {
            const DSL_IR_VALUE_REFERENCE_RECORD &reference =
                DSL_ir_value_reference_table
                    [node.first_operand_reference_id - 1 + kid];
            if (reference.value_id == 0 ||
                reference.value_id > DSL_ir_value_table.Size() ||
                reaches_projection[reference.value_id] != 0 ||
                (DSL_ir_value_table[reference.value_id - 1].flags &
                 DSL_IR_VALUE_FLAG_LOWERED) == 0)
                continue;
            reaches_projection[reference.value_id] = 1;
            worklist.push_back(reference.value_id);
        }
    }
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
            for (UINT32 i = 0; i < DSL_call_argument_table.Size(); ++i) {
                const DSL_CALL_ARGUMENT_RECORD &argument =
                    DSL_call_argument_table[i];
                if (argument.argument_value_id != value.id)
                    continue;
                UINT32 matched_retirements = 0;
                for (UINT32 j = 0; j < retired_call_count; ++j) {
                    const DSL_RETIRED_CALL_ARGUMENT_RECORD &retired =
                        retired_calls[j];
                    if (retired.call_argument_id != argument.id)
                        continue;
                    if (retired.callsite_id != argument.callsite_id ||
                        retired.argument_value_id != value.id ||
                        retired.old_actual_ordinal !=
                            argument.actual_ordinal ||
                        retired.old_callee_formal_ordinal !=
                            argument.callee_formal_ordinal ||
                        retired.retirement_reason !=
                            DSL_INTERFACE_RETIREMENT_VERIFIED_DEAD_INPUT)
                        return DSL_IR_Image_Report
                                   (diagnostic,
                                    "invalid dead call retirement", value.id);
                    ++matched_retirements;
                }
                if (matched_retirements != 1)
                    return DSL_IR_Image_Report
                               (diagnostic,
                                "dead source has live call argument",
                                value.id);
            }
        }
        if ((value.flags & DSL_IR_VALUE_FLAG_LOWERED) == 0)
            continue;
        if (DSL_CKKS_Event_Image_Has_Source(value.id)) {
            for (UINT32 i = 0; i < projection_count; ++i) {
                if (projections[i].source_value_id == value.id)
                    return DSL_IR_Image_Report
                               (diagnostic,
                                "ambiguous CKKS/runtime lowering", value.id);
            }
            for (UINT32 i = 0; i < input_count; ++i) {
                if (inputs[i].source_value_id == value.id)
                    return DSL_IR_Image_Report
                               (diagnostic,
                                "ambiguous CKKS/runtime lowering", value.id);
            }
            continue;
        }
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
            (matched_projections == 0 && matched_inputs == 0 &&
             reaches_projection[value.id] == 0) ||
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
        if (matched_projections == 0 && matched_inputs == 0) {
            for (UINT32 i = 0; i < call_count; ++i) {
                if (calls[i].source_value_id == value.id)
                    return DSL_IR_Image_Report
                               (diagnostic,
                                "private lowered value has runtime call",
                                value.id);
            }
            for (UINT32 i = 0; i < DSL_call_argument_table.Size(); ++i) {
                if (DSL_call_argument_table[i].argument_value_id == value.id)
                    return DSL_IR_Image_Report
                               (diagnostic,
                                "private lowered value has call argument",
                                value.id);
            }
        }
    }
    return TRUE;
}

/* Coordinate both optional image families. NULL means absence (ignore its
 * size); otherwise require aligned/readable section storage independent of
 * both live families throughout commit. Parse program first, validate runtime
 * with staged retired formals when present, cross-check program/runtime views
 * (runtime parsing falls back to live retired-formal state if program is absent)
 * and lowered/dead relations against valid shared core/type/string/TCON/call/
 * formal/CKKS context. Already-active source PU locals remain caller-owned.
 * All Boolean rejection precedes live writes and preserves both families.
 * Then commit/reset program and runtime in order, retaining no input pointers;
 * allocation failure during sequential reset/copy has no rollback guarantee.
 * No PU activation or physical ABI proof; NULL diagnostic suppresses messages. */
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
              has_program ? program_view.retired_calls : NULL,
              has_program ?
                  program_view.header->retired_call_argument_count : 0,
              has_program ? program_view.inputs : NULL,
              has_program ? program_view.header->runtime_input_count : 0,
              has_program ? program_view.bindings : NULL,
              has_program ?
                  program_view.header->runtime_input_binding_count : 0,
              diagnostic))
        return FALSE;
    /* Both proposed families passed preflight; absence deliberately discards
     * old rows rather than preserving a stale counterpart from another image. */
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

/*
 * Zero the caller header, fill core version/capabilities and current row counts.
 * Skip NULL without mutation; auxiliary image tables are deliberately excluded.
 */
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

/* Inspect only the five core table sizes; do not infer auxiliary image emptiness. */
BOOL
DSL_IR_Image_Has_Records (void)
{
    return DSL_ir_opcode_descriptor_table.Size() != 0 ||
           DSL_ir_node_table.Size() != 0 ||
           DSL_ir_attribute_table.Size() != 0 ||
           DSL_ir_value_table.Size() != 0 ||
           DSL_ir_value_reference_table.Size() != 0;
}

/*
 * Allow zero only for optional fields; otherwise require an offset below the
 * current STR buffer size. This does not prove string-start or NUL termination;
 * callers rely on the shared string-table contract before dereferencing it.
 */
static BOOL
DSL_IR_Image_String_Id_Valid (STR_IDX id, BOOL required)
{
    if (id == STR_IDX_ZERO)
        return !required;
    return id < STR_Table_Size();
}

/*
 * After required string-offset validation, require 64 lowercase hexadecimal
 * characters. This checks canonical digest spelling only, not a payload hash
 * or its authenticity, and neither interns strings nor changes image state.
 */
static BOOL
DSL_IR_Image_Lowercase_SHA256_Valid (STR_IDX id)
{
    if (!DSL_IR_Image_String_Id_Valid(id, TRUE))
        return FALSE;
    const char *digest = Index_To_Str(id);
    if (strlen(digest) != 64)
        return FALSE;
    for (UINT32 i = 0; i < 64; ++i) {
        if ((digest[i] < '0' || digest[i] > '9') &&
            (digest[i] < 'a' || digest[i] > 'f'))
            return FALSE;
    }
    return TRUE;
}

/*
 * Emit the internal message/row ID to a nonnull caller-owned stream, then return
 * FALSE. NULL suppresses output; I/O errors are not propagated or tables changed.
 */
static BOOL
DSL_IR_Image_Report (FILE *diagnostic, const char *message, UINT32 id)
{
    if (diagnostic != NULL)
        fprintf (diagnostic, "DSL image error: %s id=%u\n", message, id);
    return FALSE;
}

/*
 * For a bounds/links-validated view and count-sized zero-initialized state,
 * recursively follow operand value producers. Mark scratch nodes visiting (1)
 * or complete (2); a back edge returns FALSE and may leave visiting marks.
 * Only scratch state changes; borrowed image rows are never rewritten.
 */
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

/*
 * Read a nonnull borrowed header and readable, suitably aligned arrays covering
 * its counts, with current shared string/type tables available. Check the core
 * row/link/provenance, terminal-state, generated-profile and cycle invariants;
 * scratch allocation and optional diagnostics are the only effects. This is
 * not a byte-extent/alignment guard or full opcode-schema/domain verifier:
 * descriptor support/names are checked, but most semantic fields are not
 * compared with static metadata, whose version lookup narrows to UINT16.
 */
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

    /* Establish row IDs and endpoint/string bounds before traversing links. */
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
                                        DSL_IR_NODE_FLAG_TYPED_EXTERNAL_ROW |
                                        DSL_IR_NODE_FLAG_GENERATED_EXTERNAL |
                                        DSL_IR_NODE_FLAG_RETIRED_VIEW |
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
             (record.flags & (DSL_IR_NODE_REDIRECT_ORDINAL_MASK |
                              DSL_IR_NODE_FLAG_RETIRED_VIEW)) != 0) ||
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
        /* Generated constants require a fixed profile, not arbitrary metadata. */
        BOOL typed_row_marker = FALSE;
        BOOL generated_marker = FALSE;
        BOOL generated_field = FALSE;
        static const char *const generated_names[16] = {
            "value_kind", "value", "dsl.generated_external",
            "storage_format", "storage_file", "storage_tensor_key",
            "storage_byte_offset", "storage_byte_length",
            "storage_checksum", "tensor_tcon_idx",
            "dsl.generation_name", "dsl.generation_version",
            "dsl.geometry_manifest_sha256", "dsl.stage_ordinal",
            "dsl.diagonal_ordinal", "dsl.variant_signature_sha256"
        };
        for (UINT32 j = 0; j < record.attribute_count; ++j) {
            const DSL_IR_ATTRIBUTE_RECORD &attribute =
                view->attributes[record.first_attribute_id - 1 + j];
            const char *name = Index_To_Str(attribute.name);
            if ((record.flags & DSL_IR_NODE_FLAG_GENERATED_EXTERNAL) != 0 &&
                (j >= 16 || strcmp(name, generated_names[j]) != 0))
                return DSL_IR_Image_Report
                           (diagnostic, "generated tensor profile mismatch", i + 1);
            if ((record.flags & DSL_IR_NODE_FLAG_GENERATED_EXTERNAL) != 0 &&
                (j == 8 || j == 12 || j == 15) &&
                !DSL_IR_Image_Lowercase_SHA256_Valid(attribute.value))
                return DSL_IR_Image_Report
                           (diagnostic, "generated tensor digest mismatch", i + 1);
            if (strcmp(name, "dsl.typed_external_row") == 0) {
                if (typed_row_marker ||
                    !DSL_IR_Image_String_Id_Valid(attribute.value, TRUE) ||
                    strcmp(Index_To_Str(attribute.value), "1") != 0)
                    return DSL_IR_Image_Report
                               (diagnostic, "invalid typed row marker", i + 1);
                typed_row_marker = TRUE;
            }
            if (strcmp(name, "dsl.generated_external") == 0) {
                if (generated_marker ||
                    !DSL_IR_Image_String_Id_Valid(attribute.value, TRUE) ||
                    strcmp(Index_To_Str(attribute.value), "1") != 0)
                    return DSL_IR_Image_Report
                               (diagnostic, "invalid generated tensor marker", i + 1);
                generated_marker = TRUE;
            }
            if (strcmp(name, "dsl.generation_name") == 0 ||
                strcmp(name, "dsl.generation_version") == 0 ||
                strcmp(name, "dsl.geometry_manifest_sha256") == 0 ||
                strcmp(name, "dsl.stage_ordinal") == 0 ||
                strcmp(name, "dsl.diagonal_ordinal") == 0 ||
                strcmp(name, "dsl.variant_signature_sha256") == 0)
                generated_field = TRUE;
        }
        if (typed_row_marker !=
            ((record.flags & DSL_IR_NODE_FLAG_TYPED_EXTERNAL_ROW) != 0))
            return DSL_IR_Image_Report
                       (diagnostic, "typed row flag/marker mismatch", i + 1);
        if (generated_marker !=
            ((record.flags & DSL_IR_NODE_FLAG_GENERATED_EXTERNAL) != 0) ||
            (generated_marker && typed_row_marker) ||
            (generated_field && !generated_marker))
            return DSL_IR_Image_Report
                       (diagnostic, "generated tensor flag/marker mismatch", i + 1);
        if (generated_marker &&
            (descriptor.logical_operator != OPR_DSLTENSORCONST ||
             descriptor.version != 1 ||
             descriptor.effect_model != DSL_EFFECT_MODEL_PURE ||
             record.attribute_count != 16))
            return DSL_IR_Image_Report
                       (diagnostic, "generated tensor node mismatch", i + 1);
        if ((record.flags & DSL_IR_NODE_FLAG_TYPED_EXTERNAL_ROW) != 0 &&
            (descriptor.logical_operator != OPR_DSLTENSORCONST ||
             descriptor.version != 1 || record.attribute_count != 15))
            return DSL_IR_Image_Report
                       (diagnostic, "typed external row node mismatch", i + 1);
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
                (((record.flags & DSL_IR_NODE_FLAG_RETIRED_VIEW) != 0) ?
                 (target->ty == result.ty ||
                  descriptor.shape_rule != DSL_SHAPE_RULE_VIEW ||
                  !DSL_Shape_Zero_Motion_View_Operator
                      (descriptor.logical_operator, descriptor.version) ||
                  !DSL_Shape_Representation_Preserving_View
                      (target->ty, result.ty)) :
                 target->ty != result.ty) ||
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

    /* Producer recursion is safe to index only after all row/range checks. */
    std::vector<UINT8> node_state(header.node_count, 0);
    for (UINT32 i = 1; i <= header.node_count; ++i) {
        if (!DSL_IR_Image_Node_Acyclic(view, i, &node_state))
            return DSL_IR_Image_Report
                       (diagnostic, "cyclic value dependency", i);
    }

    return TRUE;
}

/* Commit one preflighted retype journal in either direction: require a stored
 * value with the expected old TY and a nonzero refined TY index, then replace
 * only value.ty. This does not resolve/check TY bounds, tensor compatibility,
 * physical WN/ST uses or ownership. The retype transaction owns those checks,
 * verification and reverse-order undo. FALSE leaves the row untouched; this
 * helper neither allocates nor supplies its own journal or PU activation. */
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

/* Image-only commit for an already preflighted native retirement. Require
 * distinct stored values, matching TYs (or a distinct representation-preserving
 * view pair), bounded retiring producer/result backlink and selected operand
 * equal to the replacement. View mode also checks a pure zero-motion VIEW
 * descriptor. Caller must provide valid TYs and node operand-reference ranges;
 * indexing the selected reference here does not bound-check the whole range.
 * Reject self-uses by the retiring producer before writes, then redirect every
 * stored reference to its value, set retired node flags/encoded operand, and
 * OR REDIRECTED into the retiring value. Only typed/generated node provenance
 * is retained; ordinary mode does not independently check opcode purity.
 * FALSE writes nothing; after mutation there is no verifier or local rollback.
 * The owning native transaction proves PU ownership, dominance, effects and
 * physical/ABI/REGION uses, edits those uses and removes the physical definition.
 * This helper does not do those edits or authorize whole-program mutation. */
BOOL
DSL_IR_Image_Redirect_And_Retire_Value
        (DSL_IR_VALUE_ID replacement_value_id,
         DSL_IR_VALUE_ID retiring_value_id,
         UINT32 replacement_operand_ordinal, BOOL view_redirect)
{
    DSL_IR_VALUE_RECORD replacement;
    DSL_IR_VALUE_RECORD retiring;
    if (!DSL_IR_Table_Get
            (DSL_ir_value_table, replacement_value_id, &replacement) ||
        !DSL_IR_Table_Get
            (DSL_ir_value_table, retiring_value_id, &retiring) ||
        replacement_value_id == retiring_value_id ||
        (view_redirect ?
         (replacement.ty == retiring.ty ||
          !DSL_Shape_Representation_Preserving_View
              (replacement.ty, retiring.ty)) :
         replacement.ty != retiring.ty) ||
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
    if (view_redirect) {
        DSL_IR_OPCODE_DESCRIPTOR_RECORD descriptor;
        if (!DSL_IR_Table_Get
                 (DSL_ir_opcode_descriptor_table,
                  node.opcode_descriptor_id, &descriptor) ||
            descriptor.shape_rule != DSL_SHAPE_RULE_VIEW ||
            !DSL_Shape_Zero_Motion_View_Operator
                (descriptor.logical_operator, descriptor.version) ||
            descriptor.effect_model != DSL_EFFECT_MODEL_PURE)
            return FALSE;
    }
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

    /* All Boolean rejections precede this image-wide reference update. Caller
     * coordinates physical edits and postchecks; there is no local savepoint. */
    for (UINT32 i = 0; i < DSL_ir_value_reference_table.Size(); ++i) {
        DSL_IR_VALUE_REFERENCE_RECORD &reference =
            DSL_ir_value_reference_table[i];
        if (reference.value_id == retiring_value_id)
            reference.value_id = replacement_value_id;
    }
    node.flags = (node.flags & (DSL_IR_NODE_FLAG_TYPED_EXTERNAL_ROW |
                                DSL_IR_NODE_FLAG_GENERATED_EXTERNAL)) |
                 DSL_IR_NODE_FLAG_RETIRED |
        (view_redirect ? DSL_IR_NODE_FLAG_RETIRED_VIEW : 0) |
        (replacement_operand_ordinal << DSL_IR_NODE_REDIRECT_ORDINAL_SHIFT);
    DSL_ir_value_table[retiring_value_id - 1].flags |=
        DSL_IR_VALUE_FLAG_REDIRECTED;
    return TRUE;
}

/* Commit-only terminal flag update: require an unflagged value with bounded
 * producer, matching result backlink, allowed provenance-only node flags and
 * a pure source descriptor. Then mark node/value LOWERED, preserving allowed
 * node provenance. FALSE writes nothing. Caller owns authorized scope, physical
 * definition removal, consumer closure, runtime relation proof and postchecks;
 * this helper supplies neither physical edits nor broader transaction rollback. */
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
        (node.flags & ~(DSL_IR_NODE_FLAG_TYPED_EXTERNAL_ROW |
                        DSL_IR_NODE_FLAG_GENERATED_EXTERNAL)) !=
            DSL_IR_NODE_FLAG_NONE ||
        !DSL_IR_Table_Get
            (DSL_ir_opcode_descriptor_table, node.opcode_descriptor_id,
             &descriptor) ||
        descriptor.effect_model != DSL_EFFECT_MODEL_PURE)
        return FALSE;
    node.flags |= DSL_IR_NODE_FLAG_LOWERED;
    DSL_ir_value_table[value_id - 1].flags = DSL_IR_VALUE_FLAG_LOWERED;
    return TRUE;
}

/* Read-only core journal for a caller-owned CKKS expansion. Preflight nonnull
 * output, bounded source/producer and their result backlink before filling five
 * counts plus source IDs/flags; FALSE preserves output. Caller supplies valid
 * core row identities and authorized scope/lifetime. No row copies, purity or
 * owner proof, mutation reservation or whole-transaction snapshot is made. */
BOOL
DSL_IR_Image_CKKS_Save
        (DSL_IR_VALUE_ID source_value_id,
         DSL_CKKS_IMAGE_SAVEPOINT *savepoint)
{
    DSL_IR_VALUE_RECORD source;
    DSL_IR_NODE_RECORD node;
    if (savepoint == NULL ||
        !DSL_IR_Table_Get(DSL_ir_value_table, source_value_id, &source) ||
        !DSL_IR_Table_Get
            (DSL_ir_node_table, source.producer_node_id, &node) ||
        node.result_value_id != source.id)
        return FALSE;
    savepoint->opcode_count = DSL_ir_opcode_descriptor_table.Size();
    savepoint->node_count = DSL_ir_node_table.Size();
    savepoint->attribute_count = DSL_ir_attribute_table.Size();
    savepoint->value_count = DSL_ir_value_table.Size();
    savepoint->reference_count = DSL_ir_value_reference_table.Size();
    savepoint->source_node_id = node.id;
    savepoint->source_value_id = source.id;
    savepoint->source_node_flags = node.flags;
    savepoint->source_value_flags = source.flags;
    return TRUE;
}

/* Core-only undo using a genuine savepoint from the same append/redirect
 * episode, with saved prefixes/IDs still intact and no unrelated edits. NULL
 * is a no-op. Scan the surviving old reference prefix and replace every match
 * of the transaction's fresh replacement ID (INVALID skips), restore source
 * flags only for IDs still in bounds, then trim five core tables. No owner or
 * generation validation, exact reference edit log, deleted-row reconstruction
 * or status is provided. Caller separately restores physical/ABI/REGION/runtime,
 * event, string, symbol and tensor metadata state, then verifies the combined
 * rollback. This helper neither activates a PU nor proves rollback succeeded. */
void
DSL_IR_Image_CKKS_Restore
        (const DSL_CKKS_IMAGE_SAVEPOINT *savepoint,
         DSL_IR_VALUE_ID replacement_value_id)
{
    if (savepoint == NULL)
        return;
    UINT32 old_count = savepoint->reference_count;
    if (old_count > DSL_ir_value_reference_table.Size())
        old_count = DSL_ir_value_reference_table.Size();
    for (UINT32 i = 0; i < old_count; ++i) {
        DSL_IR_VALUE_REFERENCE_RECORD &reference =
            DSL_ir_value_reference_table[i];
        if (replacement_value_id != DSL_IR_VALUE_INVALID_ID &&
            reference.value_id == replacement_value_id)
            reference.value_id = savepoint->source_value_id;
    }
    if (savepoint->source_node_id != 0 &&
        savepoint->source_node_id <= DSL_ir_node_table.Size())
        DSL_ir_node_table[savepoint->source_node_id - 1].flags =
            savepoint->source_node_flags;
    if (savepoint->source_value_id != 0 &&
        savepoint->source_value_id <= DSL_ir_value_table.Size())
        DSL_ir_value_table[savepoint->source_value_id - 1].flags =
            savepoint->source_value_flags;
    /* Remove dependent appended rows before their source tables. These
     * watermarks do not recover rows already removed by another operation. */
    DSL_ir_value_reference_table.Delete_down_to
        (savepoint->reference_count);
    DSL_ir_attribute_table.Delete_down_to(savepoint->attribute_count);
    DSL_ir_value_table.Delete_down_to(savepoint->value_count);
    DSL_ir_node_table.Delete_down_to(savepoint->node_count);
    DSL_ir_opcode_descriptor_table.Delete_down_to(savepoint->opcode_count);
}

/* With valid core row identities, commit-only logical redirect/lower requires
 * distinct equal-TY unflagged
 * values with matching bounded producers/backlinks and allowed node flags, a
 * pure source descriptor and no source-node self-reference. After preflight,
 * redirect every matching table reference and mark only the source LOWERED.
 * FALSE changes nothing; replacement rows and physical WNs are untouched.
 * Caller authorizes the complete reference scope, proves replacement/owner/
 * consumer legality and physical edits, then verifies or rolls back the wider
 * transaction. This does not check replacement effects, general cycles or
 * relation completeness and does not activate another PU. */
BOOL
DSL_IR_Image_Redirect_And_Lower_Value
        (DSL_IR_VALUE_ID source_value_id,
         DSL_IR_VALUE_ID replacement_value_id)
{
    DSL_IR_VALUE_RECORD source;
    DSL_IR_VALUE_RECORD replacement;
    DSL_IR_NODE_RECORD source_node;
    DSL_IR_NODE_RECORD replacement_node;
    DSL_IR_OPCODE_DESCRIPTOR_RECORD source_opcode;
    if (source_value_id == replacement_value_id ||
        !DSL_IR_Table_Get
            (DSL_ir_value_table, source_value_id, &source) ||
        !DSL_IR_Table_Get
            (DSL_ir_value_table, replacement_value_id, &replacement) ||
        source.ty != replacement.ty ||
        source.flags != DSL_IR_VALUE_FLAG_NONE ||
        replacement.flags != DSL_IR_VALUE_FLAG_NONE ||
        !DSL_IR_Table_Get
            (DSL_ir_node_table, source.producer_node_id, &source_node) ||
        !DSL_IR_Table_Get
            (DSL_ir_node_table, replacement.producer_node_id,
             &replacement_node) ||
        (source_node.flags & ~(DSL_IR_NODE_FLAG_TYPED_EXTERNAL_ROW |
                               DSL_IR_NODE_FLAG_GENERATED_EXTERNAL)) !=
            DSL_IR_NODE_FLAG_NONE ||
        (replacement_node.flags & ~(DSL_IR_NODE_FLAG_TYPED_EXTERNAL_ROW |
                                    DSL_IR_NODE_FLAG_GENERATED_EXTERNAL)) !=
            DSL_IR_NODE_FLAG_NONE ||
        source_node.result_value_id != source.id ||
        replacement_node.result_value_id != replacement.id ||
        !DSL_IR_Table_Get
            (DSL_ir_opcode_descriptor_table,
             source_node.opcode_descriptor_id, &source_opcode) ||
        source_opcode.effect_model != DSL_EFFECT_MODEL_PURE)
        return FALSE;
    for (UINT32 i = 0; i < DSL_ir_value_reference_table.Size(); ++i) {
        const DSL_IR_VALUE_REFERENCE_RECORD &reference =
            DSL_ir_value_reference_table[i];
        if (reference.value_id == source_value_id &&
            reference.owner_node_id == source_node.id)
            return FALSE;
    }
    for (UINT32 i = 0; i < DSL_ir_value_reference_table.Size(); ++i) {
        DSL_IR_VALUE_REFERENCE_RECORD &reference =
            DSL_ir_value_reference_table[i];
        if (reference.value_id == source_value_id)
            reference.value_id = replacement_value_id;
    }
    DSL_ir_node_table[source_node.id - 1].flags =
        source_node.flags | DSL_IR_NODE_FLAG_LOWERED;
    DSL_ir_value_table[source.id - 1].flags =
        DSL_IR_VALUE_FLAG_LOWERED;
    return TRUE;
}

/* Commit-only dead-provenance flag update: require an unflagged CONSTANT with
 * bounded producer/result backlink, allowed node provenance flags, zero operands
 * and a pure DSLTENSORCONST descriptor. Then mark node/value DEAD_ELIDED.
 * FALSE leaves rows unchanged; version/type/owner, physical dead-use and absent
 * runtime-relation proof remain caller-owned. No WN edits, final verification
 * or broader rollback are provided by this helper. */
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
        (node.flags & ~(DSL_IR_NODE_FLAG_TYPED_EXTERNAL_ROW |
                        DSL_IR_NODE_FLAG_GENERATED_EXTERNAL)) !=
            DSL_IR_NODE_FLAG_NONE || node.operand_count != 0 ||
        !DSL_IR_Table_Get
            (DSL_ir_opcode_descriptor_table, node.opcode_descriptor_id,
             &descriptor) ||
        descriptor.logical_operator != OPR_DSLTENSORCONST ||
        descriptor.effect_model != DSL_EFFECT_MODEL_PURE)
        return FALSE;
    node.flags |= DSL_IR_NODE_FLAG_DEAD_ELIDED;
    DSL_ir_value_table[value_id - 1].flags =
        DSL_IR_VALUE_FLAG_DEAD_ELIDED;
    return TRUE;
}

/* With caller-validated producer/reference bounds, read a REDIRECTED value's
 * RETIRED node and encoded operand ordinal, then copy that immediate operand's
 * value ID. Reject NULL output or failed flag/ordinal checks without writes;
 * upper bounds and returned target validity are not independently proved.
 * No chain traversal, runtime-handle resolution, mutation or PU activation. */
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

/*
 * Flatten the five segmented tables into temporary copies for the shared view
 * validator, then free them on its normal TRUE/FALSE return. Live rows remain
 * unchanged and auxiliary validators are not called. Raw scratch allocations
 * have no exception cleanup guarantee; this is not a mapped-section size check.
 */
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

/*
 * Given nonnull size and nonzero record_size, add count times row size only if
 * it fits UINT64. Overflow returns FALSE without changing size; this is checked
 * byte accounting, not section allocation, alignment or row validation.
 */
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

/*
 * Preflight exact checked byte extent and a borrowed core view against shared
 * tables before touching live image state. Input must remain aligned/readable
 * and independent of storage discarded by Reset. Commit resets all image
 * families and copies core rows; allocation failure after reset is not rolled
 * back. NULL is malformed input, not the reader's absent-section protocol.
 */
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

    /* Rejection above preserves old rows; commit also retires auxiliary images. */
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

/* Zero the POD row and mark arity unspecified/variadic with -1; NULL is safe. */
void
DSL_IR_Opcode_Descriptor_Record_Init
        (DSL_IR_OPCODE_DESCRIPTOR_RECORD *record)
{
    DSL_IR_Record_Init (record);
    if (record != NULL)
        record->operand_count = -1;
}

/* Delegate null-safe POD initialization; do not allocate or establish node links. */
void
DSL_IR_Node_Record_Init (DSL_IR_NODE_RECORD *record)
{
    DSL_IR_Record_Init (record);
}

/* Delegate null-safe POD initialization, leaving attribute ownership unset. */
void
DSL_IR_Attribute_Record_Init (DSL_IR_ATTRIBUTE_RECORD *record)
{
    DSL_IR_Record_Init (record);
}

/* Delegate null-safe POD initialization, leaving kind and identity unknown. */
void
DSL_IR_Value_Record_Init (DSL_IR_VALUE_RECORD *record)
{
    DSL_IR_Record_Init (record);
}

/* Delegate null-safe POD initialization; neither endpoint is resolved here. */
void
DSL_IR_Value_Reference_Record_Init
        (DSL_IR_VALUE_REFERENCE_RECORD *record)
{
    DSL_IR_Record_Init (record);
}

/*
 * Reject only absent input/name/version, then copy to existing table storage
 * and replace the copied ID. Preserve caller storage and duplicate keys; defer
 * logical/string/enum validation. No multi-row allocation rollback is provided.
 */
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

/* Scan table order for an exact numeric key, returning the first row's index ID. */
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

/*
 * Prefer an existing exact key without validation. On a miss, use static
 * metadata's UINT16 version lookup, intern strings with Save_Str, then append.
 * The UINT32 argument is not range-checked before narrowing; callers must keep
 * versions representable. String interning and append are not one transaction.
 */
DSL_IR_OPCODE_DESCRIPTOR_ID
DSL_IR_Image_Ensure_Opcode_Descriptor
        (UINT32 logical_operator,
         UINT32 version)
{
    DSL_IR_OPCODE_DESCRIPTOR_ID id =
        DSL_IR_Image_Find_Opcode_Descriptor(logical_operator, version);
    if (id != DSL_IR_OPCODE_DESCRIPTOR_INVALID_ID)
        return id;

    /* Never substitute current-version metadata for a requested exact version. */
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

/* Copy after descriptor bounds preflight; assign the table ID, not caller ID. */
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

/*
 * Check name presence, non-UNKNOWN kind and owner bounds before copying. Do not
 * validate string offsets, enum range or schema; leave caller input unchanged.
 */
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

/*
 * Reject NULL/UNKNOWN kind and out-of-range nonzero producer, then copy/assign
 * ID. Producer zero is accepted; type, symbol and role proofs remain deferred.
 */
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

/* Copy only after both nonzero endpoint bounds pass; ordinal checks are deferred. */
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

/*
 * With the existing descriptor link valid, preflight bounds, count/start pairs,
 * fixed arity, range ownership/ordinals and result producer before assigning
 * links. Rejection preserves the old node but does not undo caller appends.
 * No allocation, semantic verification or physical WN mutation occurs below.
 */
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

    /* All fallible link checks precede these five scalar assignments. */
    DSL_IR_NODE_RECORD &node = DSL_ir_node_table[node_id - 1];
    node.first_operand_reference_id = first_operand_id;
    node.operand_count = operand_count;
    node.first_attribute_id = first_attribute_id;
    node.attribute_count = attribute_count;
    node.result_value_id = result_value_id;
    return TRUE;
}

/*
 * Image-only part of DSL_IR_Rewrite_Native_Value: preflight target/descriptor,
 * result backlink, fixed arity, operand bounds and minimal attribute/kind fields.
 * Caller-valid sequential row IDs and all existing node ranges are required;
 * this does not check complete enum/string/schema/type/effect/owner legality.
 * Prepare replacement ranges for every node in temporary vectors, then replace
 * both flat tables and all range starts, preserving node/result IDs and ST/TY.
 * New references are initialized afresh; attributes retain supplied fields
 * except assigned ID/owner. Even unchanged nodes' reference/attribute IDs may
 * change, so callers must invalidate those IDs/views across this whole-image
 * rebuild. Reported FALSE leaves live tables unchanged, not earlier caller
 * allocations/appends. Commit has no FALSE path or final verification; uncaught
 * allocation failure during table installation has no rollback guarantee.
 * Physical WN replacement and authorized transformation scope remain external.
 */
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

    /* Stage every range before deleting live storage, so explicit rejection
     * and preparation do not require an old-row rollback journal. */
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

    /* Commit may allocate while inserting; staging is not an exception-safe
     * whole-image transaction once either live flat table has been cleared. */
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

/* Scan stored rows by numeric ST and exact name, optionally exact owner text.
 * Do not dereference a local ST or infer the active PU; owner-sensitive native
 * callers establish physical context before using this metadata lookup.
 * Remember the matching table position and reject a second match before any
 * output copy. Final indexed Get supports a NULL uniqueness probe and leaves
 * failed-query output unchanged. Include all provenance flags; valid shared
 * name/metadata strings are caller-owned prerequisites. Temporary string
 * allocation exceptions propagate; no tables or interned strings are changed. */
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

/* Delegate the same read-only uniqueness check without an owner filter.
 * This can reject otherwise valid PU-local ordinal/name collisions; it neither
 * selects Current_pu nor substitutes a physical symbol/definition lookup. */
BOOL
DSL_IR_Image_Find_Value
        (ST_IDX st,
         const char *name,
         DSL_IR_VALUE_RECORD *record)
{
    return DSL_IR_Image_Find_PU_Value(st, name, NULL, record);
}

/*
 * Roll back the clone helper's append-only managed rows, including after a
 * later REGION/physical-interface rejection by DSL_PU_Transaction_Clone_Active.
 * The caller must supply a genuine captured point in this image, retain all
 * old prefixes and exclude unrelated appends. NULL is a no-op; Delete_down_to
 * only shrinks, so stale/oversized counts neither reconstruct nor validate rows.
 * No old-row edits, opcode descriptors, strings, WNs/STs, REGION/domain stores,
 * pair outputs or active PU state are restored. No post-rollback check or
 * failure status is provided; combined restoration belongs to the caller.
 */
void
DSL_IR_Image_Clone_PU_Restore
        (const DSL_PU_CLONE_IMAGE_SAVEPOINT *savepoint)
{
    if (savepoint == NULL)
        return;
    /* Drop dependent interface suffixes before their formal/value/node rows. */
    DSL_runtime_input_call_table.Delete_down_to
        (savepoint->runtime_input_call_count);
    DSL_runtime_input_binding_table.Delete_down_to
        (savepoint->runtime_input_binding_count);
    DSL_runtime_input_table.Delete_down_to(savepoint->runtime_input_count);
    DSL_retired_call_argument_table.Delete_down_to
        (savepoint->retired_call_argument_count);
    DSL_retired_formal_table.Delete_down_to
        (savepoint->retired_formal_count);
    DSL_runtime_call_projection_table.Delete_down_to
        (savepoint->runtime_call_projection_count);
    DSL_runtime_value_projection_table.Delete_down_to
        (savepoint->runtime_value_projection_count);
    DSL_pu_formal_table.Delete_down_to(savepoint->formal_count);
    DSL_pu_source_identity_table.Delete_down_to
        (savepoint->pu_identity_count);
    DSL_ir_value_reference_table.Delete_down_to
        (savepoint->reference_count);
    DSL_ir_attribute_table.Delete_down_to(savepoint->attribute_count);
    DSL_ir_value_table.Delete_down_to(savepoint->value_count);
    DSL_ir_node_table.Delete_down_to(savepoint->node_count);
}

/*
 * Supply the logical-image portion of the owning program transaction's physical
 * PU clone. Preflight core/formal validity, distinct same-level nonzero STs and
 * names, exact owner-metadata values/capacity and source/absent-clone identities.
 * The caller establishes physical ownership and independent local ST storage;
 * unchanged ST ordinals, descriptor IDs and other copied fields rely on that.
 * Capture table watermarks, append nodes/values with fresh IDs, then remap links
 * and selected formal/runtime/retirement rows. Unmapped operands are retained
 * only at the source PU ST's level; this test alone is not an ownership proof.
 * Reject owner-local runtime calls, retired call arguments, source runtime-input
 * resources and runtime-input caller rows; ordinary callsite/ABI rows, REGION,
 * state/effect and domain tables are not cloned. The outer transaction owns
 * those legality checks and any subsequent incoming-call retargeting.
 * Verify core/formal/call images before publishing pair_count. A reported late
 * failure truncates recorded suffixes, not strings or arbitrary external edits;
 * pairs may be partial and savepoint remains captured. Earlier FALSE changes
 * neither pairs nor savepoint, but every non-NULL pair_count is reset to zero.
 * Allocation failures escape without guaranteed rollback. This is neither an
 * atomic whole-program clone nor authority for a PU-local pass to cross PUs.
 */
BOOL
DSL_IR_Image_Clone_PU_Values
        (ST_IDX source_pu_st, const char *source_pu_name,
         ST_IDX clone_pu_st, const char *clone_pu_name,
         DSL_PU_CLONE_VALUE_PAIR *pairs, UINT32 pair_capacity,
         UINT32 *pair_count,
         DSL_PU_CLONE_IMAGE_SAVEPOINT *savepoint)
{
    if (pair_count != NULL)
        *pair_count = 0;
    if (pairs == NULL || pair_count == NULL || savepoint == NULL ||
        source_pu_st == clone_pu_st ||
        ST_IDX_level(source_pu_st) != ST_IDX_level(clone_pu_st) ||
        ST_IDX_index(source_pu_st) == 0 ||
        ST_IDX_index(clone_pu_st) == 0 ||
        source_pu_name == NULL || source_pu_name[0] == '\0' ||
        clone_pu_name == NULL || clone_pu_name[0] == '\0' ||
        strcmp(source_pu_name, clone_pu_name) == 0 ||
        !DSL_IR_Image_Validate(NULL) ||
        !DSL_PU_Interface_Image_Validate(NULL))
        return FALSE;

    std::string source_owner("owner_pu=");
    source_owner += source_pu_name;
    std::string clone_owner("owner_pu=");
    clone_owner += clone_pu_name;
    std::vector<DSL_IR_VALUE_ID> source_values;
    std::vector<DSL_IR_NODE_ID> source_nodes;
    std::vector<DSL_IR_NODE_ID> node_map
        (DSL_ir_node_table.Size() + 1, DSL_IR_NODE_INVALID_ID);
    std::vector<BOOL> node_seen(DSL_ir_node_table.Size() + 1, FALSE);
    std::vector<DSL_IR_VALUE_ID> value_map
        (DSL_ir_value_table.Size() + 1, DSL_IR_VALUE_INVALID_ID);
    for (UINT32 i = 0; i < DSL_ir_value_table.Size(); ++i) {
        const DSL_IR_VALUE_RECORD &value = DSL_ir_value_table[i];
        if (value.metadata == STR_IDX_ZERO ||
            source_owner != Index_To_Str(value.metadata))
            continue;
        source_values.push_back(value.id);
        if (value.producer_node_id != DSL_IR_NODE_INVALID_ID) {
            if (value.producer_node_id >= node_map.size() ||
                DSL_ir_node_table[value.producer_node_id - 1]
                    .result_value_id != value.id)
                return FALSE;
            if (!node_seen[value.producer_node_id]) {
                source_nodes.push_back(value.producer_node_id);
                node_seen[value.producer_node_id] = TRUE;
            }
        }
    }
    if (source_values.empty() || source_values.size() > pair_capacity)
        return FALSE;
    DSL_PU_SOURCE_IDENTITY_RECORD source_identity;
    DSL_PU_SOURCE_IDENTITY_RECORD existing_identity;
    if (!DSL_Call_Image_Find_PU_Identity
            (source_pu_st, &source_identity) ||
        DSL_Call_Image_Find_PU_Identity
            (clone_pu_st, &existing_identity))
        return FALSE;

    /* Capture every suffix this helper may need to discard before appending;
     * existing row contents and shared string storage are not journaled. */
    savepoint->node_count = DSL_ir_node_table.Size();
    savepoint->attribute_count = DSL_ir_attribute_table.Size();
    savepoint->value_count = DSL_ir_value_table.Size();
    savepoint->reference_count = DSL_ir_value_reference_table.Size();
    savepoint->formal_count = DSL_pu_formal_table.Size();
    savepoint->pu_identity_count = DSL_pu_source_identity_table.Size();
    savepoint->runtime_value_projection_count =
        DSL_runtime_value_projection_table.Size();
    savepoint->runtime_call_projection_count =
        DSL_runtime_call_projection_table.Size();
    savepoint->retired_formal_count = DSL_retired_formal_table.Size();
    savepoint->retired_call_argument_count =
        DSL_retired_call_argument_table.Size();
    savepoint->runtime_input_count = DSL_runtime_input_table.Size();
    savepoint->runtime_input_binding_count =
        DSL_runtime_input_binding_table.Size();
    savepoint->runtime_input_call_count =
        DSL_runtime_input_call_table.Size();

    /* Allocate fresh identities before relinking producers and operand ranges. */
    BOOL valid = TRUE;
    for (UINT32 i = 0; valid && i < source_nodes.size(); ++i) {
        DSL_IR_NODE_ID source_id = source_nodes[i];
        if (node_map[source_id] != DSL_IR_NODE_INVALID_ID)
            continue;
        DSL_IR_NODE_RECORD node = DSL_ir_node_table[source_id - 1];
        node.id = DSL_IR_NODE_INVALID_ID;
        node.first_operand_reference_id = DSL_IR_VALUE_REFERENCE_INVALID_ID;
        node.operand_count = 0;
        node.first_attribute_id = DSL_IR_ATTRIBUTE_INVALID_ID;
        node.attribute_count = 0;
        node.result_value_id = DSL_IR_VALUE_INVALID_ID;
        node_map[source_id] = DSL_IR_Image_Add_Node(&node);
        valid = node_map[source_id] != DSL_IR_NODE_INVALID_ID;
    }
    for (UINT32 i = 0; valid && i < source_values.size(); ++i) {
        DSL_IR_VALUE_ID source_id = source_values[i];
        DSL_IR_VALUE_RECORD value = DSL_ir_value_table[source_id - 1];
        if (value.producer_node_id != DSL_IR_NODE_INVALID_ID)
            value.producer_node_id = node_map[value.producer_node_id];
        value.id = DSL_IR_VALUE_INVALID_ID;
        value.metadata = Save_Str(clone_owner.c_str());
        value_map[source_id] = DSL_IR_Image_Add_Value(&value);
        valid = value_map[source_id] != DSL_IR_VALUE_INVALID_ID;
        pairs[i].source_value_id = source_id;
        pairs[i].clone_value_id = value_map[source_id];
    }
    for (UINT32 i = 0; valid && i < source_nodes.size(); ++i) {
        DSL_IR_NODE_ID source_id = source_nodes[i];
        DSL_IR_NODE_RECORD source = DSL_ir_node_table[source_id - 1];
        DSL_IR_VALUE_REFERENCE_ID first_reference =
            DSL_IR_VALUE_REFERENCE_INVALID_ID;
        DSL_IR_ATTRIBUTE_ID first_attribute = DSL_IR_ATTRIBUTE_INVALID_ID;
        for (UINT32 j = 0; valid && j < source.operand_count; ++j) {
            DSL_IR_VALUE_REFERENCE_RECORD reference =
                DSL_ir_value_reference_table
                    [source.first_operand_reference_id + j - 1];
            reference.id = DSL_IR_VALUE_REFERENCE_INVALID_ID;
            reference.owner_node_id = node_map[source_id];
            if (reference.value_id < value_map.size() &&
                value_map[reference.value_id] != DSL_IR_VALUE_INVALID_ID)
                reference.value_id = value_map[reference.value_id];
            else if (reference.value_id == DSL_IR_VALUE_INVALID_ID ||
                     reference.value_id > DSL_ir_value_table.Size() ||
                     ST_IDX_level
                         (DSL_ir_value_table[reference.value_id - 1].st) !=
                         ST_IDX_level(source_pu_st))
                valid = FALSE;
            if (valid) {
                DSL_IR_VALUE_REFERENCE_ID id =
                    DSL_IR_Image_Add_Value_Reference(&reference);
                valid = id != DSL_IR_VALUE_REFERENCE_INVALID_ID;
                if (j == 0)
                    first_reference = id;
            }
        }
        for (UINT32 j = 0; valid && j < source.attribute_count; ++j) {
            DSL_IR_ATTRIBUTE_RECORD attribute = DSL_ir_attribute_table
                [source.first_attribute_id + j - 1];
            attribute.id = DSL_IR_ATTRIBUTE_INVALID_ID;
            attribute.owner_node_id = node_map[source_id];
            DSL_IR_ATTRIBUTE_ID id = DSL_IR_Image_Add_Attribute(&attribute);
            valid = id != DSL_IR_ATTRIBUTE_INVALID_ID;
            if (j == 0)
                first_attribute = id;
        }
        if (valid)
            valid = source.result_value_id < value_map.size() &&
                value_map[source.result_value_id] !=
                    DSL_IR_VALUE_INVALID_ID &&
                DSL_IR_Image_Set_Node_Links
                    (node_map[source_id], first_reference,
                     source.operand_count, first_attribute,
                     source.attribute_count,
                     value_map[source.result_value_id]);
    }
    UINT32 original_formal_count = savepoint->formal_count;
    std::vector<DSL_PU_FORMAL_ID> formal_map
        (original_formal_count + 1, DSL_PU_FORMAL_INVALID_ID);
    for (UINT32 i = 0; valid && i < original_formal_count; ++i) {
        DSL_PU_FORMAL_RECORD formal = DSL_pu_formal_table[i];
        if (formal.owner_pu_st != source_pu_st)
            continue;
        if (formal.formal_value_id >= value_map.size() ||
            value_map[formal.formal_value_id] == DSL_IR_VALUE_INVALID_ID) {
            valid = FALSE;
            break;
        }
        formal.id = DSL_PU_FORMAL_INVALID_ID;
        formal.owner_pu_st = clone_pu_st;
        formal.formal_value_id = value_map[formal.formal_value_id];
        formal_map[i + 1] = DSL_PU_Interface_Image_Add_Formal(&formal);
        valid = formal_map[i + 1] != DSL_PU_FORMAL_INVALID_ID;
    }
    /* Owner-local runtime calls need callsite cloning, which this helper does
     * not support. Incoming-call retargeting remains an outer transaction step. */
    UINT32 original_runtime_call_count =
        savepoint->runtime_call_projection_count;
    for (UINT32 i = 0; valid && i < original_runtime_call_count; ++i) {
        if (DSL_runtime_call_projection_table[i].owner_pu_st ==
                source_pu_st)
            valid = FALSE;
    }
    UINT32 original_runtime_value_count =
        savepoint->runtime_value_projection_count;
    for (UINT32 i = 0; valid && i < original_runtime_value_count; ++i) {
        DSL_RUNTIME_VALUE_PROJECTION_RECORD projection =
            DSL_runtime_value_projection_table[i];
        if (projection.owner_pu_st != source_pu_st)
            continue;
        if (projection.source_value_id >= value_map.size() ||
            value_map[projection.source_value_id] ==
                DSL_IR_VALUE_INVALID_ID) {
            valid = FALSE;
            break;
        }
        projection.id = DSL_RUNTIME_VALUE_PROJECTION_INVALID_ID;
        projection.owner_pu_st = clone_pu_st;
        projection.source_value_id = value_map[projection.source_value_id];
        valid = DSL_Runtime_Interface_Image_Add_Value(&projection) !=
                DSL_RUNTIME_VALUE_PROJECTION_INVALID_ID;
    }
    /* Preserve the post-projection physical ABI: canonical dead formals and
     * threaded runtime-resource formals are owner-qualified clone state. */
    UINT32 original_retired_formal_count = savepoint->retired_formal_count;
    for (UINT32 i = 0; valid && i < original_retired_formal_count; ++i) {
        DSL_RETIRED_FORMAL_RECORD retired = DSL_retired_formal_table[i];
        if (retired.owner_pu_st != source_pu_st)
            continue;
        if (retired.pu_formal_id >= formal_map.size() ||
            formal_map[retired.pu_formal_id] ==
                DSL_PU_FORMAL_INVALID_ID ||
            retired.formal_value_id >= value_map.size() ||
            value_map[retired.formal_value_id] ==
                DSL_IR_VALUE_INVALID_ID) {
            valid = FALSE;
            break;
        }
        retired.id = DSL_RETIRED_FORMAL_INVALID_ID;
        retired.pu_formal_id = formal_map[retired.pu_formal_id];
        retired.owner_pu_st = clone_pu_st;
        retired.formal_value_id = value_map[retired.formal_value_id];
        valid = DSL_Program_Interface_Image_Add_Retired_Formal(&retired) !=
                DSL_RETIRED_FORMAL_INVALID_ID;
    }
    UINT32 original_retired_call_count =
        savepoint->retired_call_argument_count;
    for (UINT32 i = 0; valid && i < original_retired_call_count; ++i) {
        DSL_CALLSITE_METADATA_RECORD callsite;
        const DSL_RETIRED_CALL_ARGUMENT_RECORD &retired =
            DSL_retired_call_argument_table[i];
        if (!DSL_Call_Image_Get_Callsite(retired.callsite_id, &callsite) ||
            callsite.owner_pu_st == source_pu_st)
            valid = FALSE;
    }
    UINT32 original_runtime_input_count = savepoint->runtime_input_count;
    for (UINT32 i = 0; valid && i < original_runtime_input_count; ++i) {
        if (DSL_runtime_input_table[i].source_owner_pu_st == source_pu_st)
            valid = FALSE;
    }
    UINT32 original_runtime_binding_count =
        savepoint->runtime_input_binding_count;
    for (UINT32 i = 0; valid && i < original_runtime_binding_count; ++i) {
        DSL_RUNTIME_INPUT_BINDING_RECORD binding =
            DSL_runtime_input_binding_table[i];
        if (binding.owner_pu_st != source_pu_st)
            continue;
        binding.id = DSL_RUNTIME_INPUT_BINDING_INVALID_ID;
        binding.owner_pu_st = clone_pu_st;
        valid = DSL_Program_Interface_Image_Add_Runtime_Binding(&binding) !=
                DSL_RUNTIME_INPUT_BINDING_INVALID_ID;
    }
    UINT32 original_runtime_input_call_count =
        savepoint->runtime_input_call_count;
    for (UINT32 i = 0; valid && i < original_runtime_input_call_count; ++i) {
        if (DSL_runtime_input_call_table[i].caller_owner_pu_st ==
                source_pu_st)
            valid = FALSE;
    }
    if (valid) {
        source_identity.id = DSL_PU_SOURCE_IDENTITY_INVALID_ID;
        source_identity.owner_pu_st = clone_pu_st;
        valid = DSL_Call_Image_Add_PU_Identity(&source_identity) !=
                DSL_PU_SOURCE_IDENTITY_INVALID_ID;
    }
    /* Structural image checks precede count publication; physical PU/REGION
     * verification and linking into the PU list remain caller responsibilities. */
    if (valid)
        valid = DSL_IR_Image_Validate(NULL) &&
                DSL_PU_Interface_Image_Validate(NULL) &&
                DSL_Call_Image_Validate(NULL);
    if (!valid) {
        DSL_IR_Image_Clone_PU_Restore(savepoint);
        return FALSE;
    }
    *pair_count = source_values.size();
    return TRUE;
}

/* Read physical row count; duplicate logical keys remain distinct stored rows. */
UINT32
DSL_IR_Image_Opcode_Descriptor_Count (void)
{
    return DSL_ir_opcode_descriptor_table.Size();
}

/* Include provenance rows regardless of retirement/lowering flags. */
UINT32
DSL_IR_Image_Node_Count (void)
{
    return DSL_ir_node_table.Size();
}

/*
 * Count only the absence of three terminal flags across the whole node table.
 * Do not consult current PU, opcode category or the executable WHIRL tree.
 */
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

/* Read stored attribute count, without filtering active node link ranges. */
UINT32
DSL_IR_Image_Attribute_Count (void)
{
    return DSL_ir_attribute_table.Size();
}

/* Read stored value count without filtering lowered/redirected/dead flags. */
UINT32
DSL_IR_Image_Value_Count (void)
{
    return DSL_ir_value_table.Size();
}

/* Read stored reference count without traversing active node link ranges. */
UINT32
DSL_IR_Image_Value_Reference_Count (void)
{
    return DSL_ir_value_reference_table.Size();
}

/* Delegate bounded, optional POD copying; no semantic verification is implied. */
BOOL
DSL_IR_Image_Get_Opcode_Descriptor
        (DSL_IR_OPCODE_DESCRIPTOR_ID id,
         DSL_IR_OPCODE_DESCRIPTOR_RECORD *record)
{
    return DSL_IR_Table_Get (DSL_ir_opcode_descriptor_table, id, record);
}

/* Delegate bounded optional row copying; leave invalid-query output untouched. */
BOOL
DSL_IR_Image_Get_Node (DSL_IR_NODE_ID id, DSL_IR_NODE_RECORD *record)
{
    return DSL_IR_Table_Get (DSL_ir_node_table, id, record);
}

/* Copy through the shared bounds helper, retaining STR_IDX identities, not strings. */
BOOL
DSL_IR_Image_Get_Attribute
        (DSL_IR_ATTRIBUTE_ID id,
         DSL_IR_ATTRIBUTE_RECORD *record)
{
    return DSL_IR_Table_Get (DSL_ir_attribute_table, id, record);
}

/* Delegate row copying only; referenced symbol/type/value state is not validated. */
BOOL
DSL_IR_Image_Get_Value (DSL_IR_VALUE_ID id, DSL_IR_VALUE_RECORD *record)
{
    return DSL_IR_Table_Get (DSL_ir_value_table, id, record);
}

/* Delegate bounded row copying without resolving endpoints or changing links. */
BOOL
DSL_IR_Image_Get_Value_Reference
        (DSL_IR_VALUE_REFERENCE_ID id,
         DSL_IR_VALUE_REFERENCE_RECORD *record)
{
    return DSL_IR_Table_Get (DSL_ir_value_reference_table, id, record);
}

/* Snapshot counts for writer/printer callers into a freshly zeroed header;
 * preserve no old flags/reserved bytes and do not validate tables. NULL writes
 * nothing; this neither emits a section nor changes live image ownership. */
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

/* Truncate only the two effect-family tables, as on reader-owned absence.
 * IDs/views cease to identify retained rows; no core, shared-symbol/string,
 * physical-state or external handle repair and no saved prefix to restore. */
void
DSL_Effect_Image_Reset (void)
{
    DSL_state_object_table.Delete_down_to(0);
    DSL_state_effect_table.Delete_down_to(0);
}

/* Use either table's stored size for writer section-presence selection;
 * nonempty does not imply a valid family or any active-PU state/effect. */
BOOL
DSL_Effect_Image_Has_Records (void)
{
    return DSL_state_object_table.Size() != 0 ||
           DSL_state_effect_table.Size() != 0;
}

/* Optionally emit an effect-row error to a caller stream; return FALSE, no mutation. */
static BOOL
DSL_Effect_Image_Report (FILE *diagnostic, const char *message, UINT32 id)
{
    if (diagnostic != NULL)
        fprintf (diagnostic, "DSL effect image error: %s id=%u\n",
                 message, id);
    return FALSE;
}

/* Check the supported non-UNKNOWN state-kind range without interpreting a state. */
static BOOL
DSL_Effect_Image_Valid_State_Kind (UINT32 kind)
{
    return kind >= DSL_STATE_KIND_RUNTIME_STATUS &&
           kind <= DSL_STATE_KIND_OPAQUE;
}

/* Accept READ or MODIFY only; this predicate does not analyze actual effects. */
static BOOL
DSL_Effect_Image_Valid_Effect_Kind (UINT32 kind)
{
    return kind == DSL_STATE_EFFECT_READ ||
           kind == DSL_STATE_EFFECT_MODIFY;
}

/*
 * Read current effect tables, relying on already valid core descriptor links.
 * Check state identity/flags and duplicate owner/name-or-ST pairs, then effect
 * endpoints, per-node ordinals, duplicate node/state pairs and non-pure owners.
 * Only nonzero symbol indices are tested, not symbol liveness/PU ownership;
 * effect flags and full domain scheduling semantics are not certified here.
 * Fail at the first optional diagnostic without changing any rows.
 */
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

/*
 * With readable/aligned independent input and valid core/string context,
 * reject header/extent failures before mutation. Then copy into freshly cleared
 * effect tables and validate live rows. Row failure clears the replacement
 * without restoring the old image; no allocation rollback is provided. Core rows
 * are unchanged and no input-buffer pointers are retained after success.
 */
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

    /* Unlike the core loader, payload validation follows destructive replacement. */
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

/* Delegate null-tolerant POD zeroing; caller still supplies kind, identities
 * and optional ownership flags before append. No table or ST allocation. */
void
DSL_State_Object_Record_Init (DSL_STATE_OBJECT_RECORD *record)
{
    DSL_IR_Record_Init(record);
}

/* Delegate null-tolerant POD zeroing without attaching an effect. Zero kind
 * and endpoints are invalid; ordinal zero is only the first edge for a node. */
void
DSL_State_Effect_Record_Init (DSL_STATE_EFFECT_RECORD *record)
{
    DSL_IR_Record_Init(record);
}

/* Preflight nonnull/name/kind/nonzero ST indices/allowed flags, copy the input
 * and assign its slot ID. Validate both complete effect tables: bad shared
 * string IDs, reserved words, owner-local duplicate ST/name or earlier family
 * errors can reject the append. Truncation then removes only this tail row;
 * no prior row or caller-created ST/string is changed. Core descriptor links
 * and shared strings must already be safe to inspect. No physical ownership
 * proof, diagnostic, exception recovery or general transaction journal. */
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
    /* Assign table identity before whole-family verification; reject by tail
     * truncation rather than replacing or repairing any pre-existing row. */
    DSL_STATE_OBJECT_RECORD copy = *record;
    UINT32 index = DSL_state_object_table.Insert(copy);
    DSL_state_object_table[index].id = index + 1;
    if (!DSL_Effect_Image_Validate(NULL)) {
        DSL_state_object_table.Delete_down_to(index);
        return DSL_STATE_OBJECT_INVALID_ID;
    }
    return index + 1;
}

/* Bound node/state endpoints and kind before copying, then assign a slot ID
 * and verify the whole family. The validator checks per-node ordinal order,
 * duplicate node/state pairs and non-PURE owner descriptors, not physical
 * ownership/scheduling or effect flags. Caller supplies the ordinal and safe
 * core/string context; other invalid rows can cause rejection. INVALID_ID
 * leaves old rows intact by truncating this attempted append only; allocation
 * failures propagate without a journal or rollback of caller work. */
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
    /* Commit a provisional tail row so the shared validator can check its
     * ordering with existing edges; a Boolean rejection drops just that row. */
    DSL_STATE_EFFECT_RECORD copy = *record;
    UINT32 index = DSL_state_effect_table.Insert(copy);
    DSL_state_effect_table[index].id = index + 1;
    if (!DSL_Effect_Image_Validate(NULL)) {
        DSL_state_effect_table.Delete_down_to(index);
        return DSL_STATE_EFFECT_INVALID_ID;
    }
    return index + 1;
}

/* Read the global stored count, independent of active PU or ST liveness. */
UINT32
DSL_Effect_Image_State_Object_Count (void)
{
    return DSL_state_object_table.Size();
}

/* Read all stored edges, including those whose owner nodes are terminal. */
UINT32
DSL_Effect_Image_State_Effect_Count (void)
{
    return DSL_state_effect_table.Size();
}

/* Delegate bounded POD copy/NULL probe; invalid IDs write nothing. Retain
 * shared string/ST indices without resolving ownership or validating rows. */
BOOL
DSL_Effect_Image_Get_State_Object
        (DSL_STATE_OBJECT_ID id,
         DSL_STATE_OBJECT_RECORD *record)
{
    return DSL_IR_Table_Get(DSL_state_object_table, id, record);
}

/* Delegate bounded optional copying, not endpoint/kind/ordinal verification;
 * no mutation, and invalid-query output remains untouched. */
BOOL
DSL_Effect_Image_Get_State_Effect
        (DSL_STATE_EFFECT_ID id,
         DSL_STATE_EFFECT_RECORD *record)
{
    return DSL_IR_Table_Get(DSL_state_effect_table, id, record);
}

/* Index static diagnostic literals after unsigned conversion and bounds
 * check; negative/out-of-range kinds share UNKNOWN's fallback. No allocation,
 * ownership transfer or claim that the kind is legal for a stored state. */
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

/* Map the bounded kind to a static literal, otherwise UNKNOWN's spelling;
 * this read-only printer helper does not validate or attach an effect. */
const char *
DSL_State_Effect_Kind_Name (DSL_STATE_EFFECT_KIND effect_kind)
{
    static const char *names[] = { "unknown", "read", "modify" };
    UINT32 index = (UINT32)effect_kind;
    return index < sizeof(names) / sizeof(names[0]) ?
           names[index] : names[0];
}
