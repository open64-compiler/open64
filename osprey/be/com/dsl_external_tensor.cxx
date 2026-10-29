/*
 * Backend-free external tensor references, captured source generations and
 * active-PU materialization. This implementation owns the single process-local
 * snapshot store; callers retain PU selection, payload files and legality.
 * Preflight/commit and allocation-failure limits remain the existing contracts.
 * Design: doc/WHIRL-DSL-INFRASTRUCTURE.md, persistent IR / service separation.
 * No new persistent records, mapped layout or binary WHIRL behavior.
 */

#include <ctype.h>
#include <errno.h>
#include <stdlib.h>
#include <string.h>
#include <set>
#include <string>
#include <vector>

#include "dsl_external_tensor.h"
#include "dsl_pu_interface_verify_internal.h"
#include "../../common/ir_image/dsl_image_service.h"
#include "dsl_tensor_fold.h"
#include "dsl_memory_behavior.h"
#include "dsl_runtime_interface_internal.h"
#include "pu_info.h"
#include "strtab.h"
#include "symtab.h"
#include "wn.h"
#include "wn_util.h"
#include "dsl_ir_rewrite_queries_internal.h"

/* Compare/replace the associated actual's logical value ID after preflight;
 * no physical actual is changed here. Require a valid replacement; unmatched
 * argument is a successful no-op and FALSE writes nothing. Caller owns WN edits. */
extern BOOL DSL_Call_ABI_Image_Update_Argument_Value
                                (const WN *, UINT32, DSL_IR_VALUE_ID,
                                 DSL_IR_VALUE_ID);
/* Parse with base-10 strtoull, rejecting NULL/empty, a leading '-' byte, ERANGE
 * and unconsumed suffixes; FALSE preserves output. Libc leading whitespace/sign
 * admission is retained, not a canonical unsigned-spelling guarantee. Success
 * assigns caller storage; errno is cleared/set by parsing, no IR is changed. */
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

/* Accept absent/empty or exactly 64 hexadecimal checksum characters. This
 * legacy predicate accepts either case and does not authenticate payloads. */
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
 * Require valid shared descriptor strings, a known dtype, positive dimensions
 * and rank agreement; reject dynamic syntax/overflow without output writes.
 * The implemented bracket parser admits whitespace and a trailing comma, not
 * a canonical-shape grammar. Optional size outputs are assigned only on TRUE;
 * no descriptor/table mutation or allocation occurs.
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

/* Return the first name-matching borrowed attribute text (empty literal for
 * zero value). Require valid shared strings; NULL arguments, a missing row/name
 * or no match returns FALSE without output writes. No duplicate, kind/schema
 * or whole-node validation, interning or lifetime extension is performed. */
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
 * and optional legacy TCON facts. Clear required output first, then check owner,
 * resident ST/type, descriptor size/range, checksum spelling and URI agreement.
 * Missing legacy checksum/TCON is allowed; a supplied TCON must match side-file
 * geometry. Success borrows strings, not payloads; no file read, digest-content
 * authentication, PU activation or table mutation. Allocation failures escape.
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
    const char *typed_row_marker = NULL;
    const char *generated_marker = NULL;
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

    BOOL typed_row = DSL_IR_Node_Attribute
                         (node, "dsl.typed_external_row", &typed_row_marker);
    BOOL generated_row = DSL_IR_Node_Attribute
                             (node, "dsl.generated_external", &generated_marker);
    if (typed_row !=
            ((node.flags & DSL_IR_NODE_FLAG_TYPED_EXTERNAL_ROW) != 0) ||
        generated_row !=
            ((node.flags & DSL_IR_NODE_FLAG_GENERATED_EXTERNAL) != 0) ||
        (typed_row && generated_row))
        return FALSE;
    if (typed_row &&
        (strcmp(typed_row_marker, "1") != 0 ||
         node.attribute_count != 15))
        return FALSE;
    if (generated_row &&
        (strcmp(generated_marker, "1") != 0 ||
         node.attribute_count != 16))
        return FALSE;
    if (typed_row || generated_row) {
        if (!DSL_IR_Node_Attribute(node, "storage_format", &format) ||
            !DSL_IR_Node_Attribute(node, "storage_file", &file) ||
            !DSL_IR_Node_Attribute(node, "storage_tensor_key", &key) ||
            !DSL_IR_Node_Attribute
                 (node, "storage_byte_offset", &offset_text) ||
            !DSL_IR_Node_Attribute
                 (node, "storage_byte_length", &length_text) ||
            !DSL_IR_Node_Attribute(node, "storage_checksum", &checksum) ||
            !DSL_IR_Node_Attribute(node, "tensor_tcon_idx",
                                   &tensor_tcon_text))
            return FALSE;
    } else {
        format = ST_tensor_metadata(value.st, "storage_format");
        file = ST_tensor_metadata(value.st, "storage_file");
        key = ST_tensor_metadata(value.st, "storage_tensor_key");
        offset_text = ST_tensor_metadata(value.st, "storage_byte_offset");
        length_text = ST_tensor_metadata(value.st, "storage_byte_length");
        checksum = ST_tensor_metadata(value.st, "storage_checksum");
        tensor_tcon_text = ST_tensor_metadata(value.st, "tensor_tcon_idx");
    }
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

/* Keep source-local facts after its PU symtab is no longer active. */
typedef struct {
    ST_IDX owner_pu_st;
    DSL_IR_VALUE_ID value_id;
    DSL_IR_NODE_ID producer_node_id;
    ST_IDX st;
    TY_IDX descriptor_ty;
    TCON_IDX tensor_tcon;
    SRCPOS source_position;
    std::string storage_format;
    std::string side_file;
    std::string tensor_key;
    std::string checksum;
    UINT64 byte_offset;
    UINT64 byte_length;
} DSL_IR_EXTERNAL_SOURCE_SNAPSHOT;

static std::vector<DSL_IR_EXTERNAL_SOURCE_SNAPSHOT>
    DSL_ir_external_source_snapshots;
static UINT32 DSL_ir_external_source_generation = 1;

/* Discard copied source proofs and advance the nonzero 32-bit generation,
 * rejecting handles from the previous generation. This is a process-local
 * lifetime fence, not unbounded uniqueness after generation wraparound.
 * Persistent rows, WNs and payload files are untouched; no PU activation. */
void
DSL_IR_External_Tensor_Source_Reset (void)
{
    DSL_ir_external_source_snapshots.clear();
    ++DSL_ir_external_source_generation;
    if (DSL_ir_external_source_generation == 0)
        ++DSL_ir_external_source_generation;
}

/* Capture validated external facts from the exact Current_PU_Info before a
 * caller switches locals. Clear nonnull output on entry; require a TCON and
 * 64-lowercase-hex checksum, then append copied strings/scalars and publish the
 * generation/index handle. No image/WN/file writes; shared source rows remain
 * caller-owned and allocation exceptions are not converted to FALSE. */
BOOL
DSL_IR_Capture_External_Tensor_Source
        (PU_Info *source_pu_info,
         DSL_IR_VALUE_ID source_value_id,
         DSL_IR_EXTERNAL_TENSOR_SOURCE_HANDLE *source_handle)
{
    if (source_handle != NULL)
        *source_handle = 0;
    ST_IDX owner = source_pu_info == NULL ? ST_IDX_ZERO :
        PU_Info_proc_sym(source_pu_info);
    DSL_IR_EXTERNAL_TENSOR_REFERENCE reference;
    if (source_handle == NULL || source_pu_info != Current_PU_Info ||
        !DSL_IR_Image_Current_PU_Is(owner) ||
        !DSL_IR_Image_Get_External_Tensor_Reference
             (owner, source_value_id, &reference) ||
        reference.tensor_tcon == TCON_IDX_ZERO ||
        reference.checksum == NULL ||
        strlen(reference.checksum) != 64 ||
        DSL_ir_external_source_snapshots.size() >= 0xffffffffu)
        return FALSE;
    for (UINT32 i = 0; i < 64; ++i) {
        if (!isdigit((unsigned char)reference.checksum[i]) &&
            (reference.checksum[i] < 'a' || reference.checksum[i] > 'f'))
            return FALSE;
    }

    DSL_IR_EXTERNAL_SOURCE_SNAPSHOT snapshot;
    snapshot.owner_pu_st = owner;
    snapshot.value_id = source_value_id;
    snapshot.producer_node_id = reference.producer_node_id;
    snapshot.st = reference.st;
    snapshot.descriptor_ty = reference.descriptor_ty;
    snapshot.tensor_tcon = reference.tensor_tcon;
    snapshot.source_position = ST_Srcpos(St_Table[reference.st]);
    snapshot.storage_format = reference.storage_format;
    snapshot.side_file = reference.side_file;
    snapshot.tensor_key = reference.tensor_key;
    snapshot.checksum = reference.checksum;
    snapshot.byte_offset = reference.byte_offset;
    snapshot.byte_length = reference.byte_length;
    DSL_ir_external_source_snapshots.push_back(snapshot);
    *source_handle = ((UINT64)DSL_ir_external_source_generation << 32) |
                     (UINT64)DSL_ir_external_source_snapshots.size();
    return TRUE;
}

/* Borrow a copied source snapshot only when generation and one-based index
 * match. NULL rejects mismatches; returned storage expires on snapshot-vector
 * reallocation/reset, so do not keep it across another capture. No PU switch
 * or mutation; generation wraparound is not an indefinite freshness proof. */
static const DSL_IR_EXTERNAL_SOURCE_SNAPSHOT *
DSL_IR_External_Tensor_Source_Snapshot
        (DSL_IR_EXTERNAL_TENSOR_SOURCE_HANDLE handle)
{
    UINT32 generation = (UINT32)(handle >> 32);
    UINT32 index = (UINT32)handle;
    if (generation != DSL_ir_external_source_generation || index == 0 ||
        index > DSL_ir_external_source_snapshots.size())
        return NULL;
    return &DSL_ir_external_source_snapshots[index - 1];
}

/*
 * Validate one persisted runtime-input row, including source tensor/TCON,
 * pointer handle kind and owner-sensitive source identity. If an external
 * source owner is already active, also check resident reference facts; never
 * activate it. Require valid shared rows/strings/types/TCONs. Stable role, row
 * IDs, flags/reserved and full physical ABI are caller/outer-validator checks,
 * not checked here. Read-only, FALSE changes nothing; no payload authentication.
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







/*
 * Preflight one converted external tensor request: source policy, call target,
 * TCON/path/range/checksum, type, shape, and provenance must all agree. It
 * records no symbols, values, or call edits. Caller supplies selected-PU locals,
 * readable strings and nonnull uri/payload scratch. source_value is cleared
 * first and may be filled before later FALSE; strings are produced on success.
 * No payload read/computation/authentication or allocation-exception rollback.
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
 * Preflight a complete same-PU array before constructing converted external
 * tensor values and replacing selected call actuals. Boolean preflight failure
 * leaves WN, ST, value and call-ABI state unchanged. Commit creates symbols,
 * WNs, metadata and image rows without a general allocation-failure journal;
 * assertions/exceptions during commit are not recoverable atomic rollback.
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

    /* Every request is accepted before clearing output and publishing symbols,
     * trees and image rows. Commit failures have no general rollback journal. */
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

/* Zero caller-owned request storage when nonnull; acquire no string, symbol,
 * WN or source-handle ownership. */
void
DSL_IR_Typed_External_Tensor_Value_Request_Init
        (DSL_IR_TYPED_EXTERNAL_TENSOR_VALUE_REQUEST *request)
{
    if (request != NULL)
        memset(request, 0, sizeof(*request));
}

/* Admit a nonempty lineage label containing '.' and only alnum/underscore/dot.
 * This is spelling admission, not a registered transformation or executable
 * opcode lookup; NULL rejects and no strings or IR are changed. */
static BOOL
DSL_IR_Transformation_Name_Valid (const char *name)
{
    if (name == NULL || name[0] == '\0' || strchr(name, '.') == NULL)
        return FALSE;
    for (const char *cursor = name; *cursor != '\0'; ++cursor) {
        if (!isalnum((unsigned char)*cursor) && *cursor != '_' &&
            *cursor != '.')
            return FALSE;
    }
    return TRUE;
}

/* Clear required lineage output, verify destination reference/typed marker and
 * source owner/value constant facts, then publish scalars/borrowed name text.
 * Caller supplies active destination locals and valid shared source tables;
 * source locals are not activated. FALSE leaves zero output; no IR/file writes
 * or complete transformation/numerical proof, and scratch allocations escape. */
BOOL
DSL_IR_Image_Get_Typed_External_Tensor_Lineage
        (ST_IDX owner_pu_st,
         DSL_IR_VALUE_ID value_id,
         DSL_IR_TYPED_EXTERNAL_TENSOR_LINEAGE *lineage)
{
    if (lineage != NULL)
        memset(lineage, 0, sizeof(*lineage));
    DSL_IR_VALUE_RECORD value;
    DSL_IR_EXTERNAL_TENSOR_REFERENCE reference;
    DSL_IR_NODE_RECORD node;
    const char *marker = NULL;
    if (lineage == NULL || !DSL_IR_Image_Current_PU_Is(owner_pu_st) ||
        !DSL_IR_Image_Get_External_Tensor_Reference
             (owner_pu_st, value_id, &reference) ||
        !DSL_IR_Image_Get_Value(value_id, &value) ||
        !DSL_IR_Image_Get_Node(value.producer_node_id, &node) ||
        (node.flags & DSL_IR_NODE_FLAG_TYPED_EXTERNAL_ROW) == 0 ||
        !DSL_IR_Node_Attribute(node, "dsl.typed_external_row", &marker) ||
        strcmp(marker, "1") != 0 || node.attribute_count != 15)
        return FALSE;

    const char *owner_text = NULL;
    const char *source_text = NULL;
    const char *name = NULL;
    const char *version_text = NULL;
    const char *ordinal_text = NULL;
    if (!DSL_IR_Node_Attribute
             (node, "dsl.converted_from_owner_pu_st", &owner_text) ||
        !DSL_IR_Node_Attribute
             (node, "dsl.converted_from_value_id", &source_text) ||
        !DSL_IR_Node_Attribute
             (node, "dsl.transformation_name", &name) ||
        !DSL_IR_Node_Attribute
             (node, "dsl.transformation_version", &version_text) ||
        !DSL_IR_Node_Attribute
             (node, "dsl.transformation_ordinal", &ordinal_text))
        return FALSE;
    UINT64 source_owner;
    UINT64 source_id;
    UINT64 version;
    UINT64 ordinal;
    if (!DSL_IR_Parse_Unsigned(owner_text, &source_owner) ||
        !DSL_IR_Parse_Unsigned(source_text, &source_id) ||
        !DSL_IR_Parse_Unsigned(version_text, &version) ||
        !DSL_IR_Parse_Unsigned(ordinal_text, &ordinal) ||
        source_owner == 0 || source_owner > 0xffffffffu ||
        source_id == 0 || source_id > 0xffffffffu ||
        version == 0 || version > 0xffffffffu ||
        ordinal > 0xffffffffu ||
        !DSL_IR_Transformation_Name_Valid(name))
        return FALSE;

    DSL_IR_VALUE_RECORD source;
    ST_IDX source_owner_pu_st = (ST_IDX)source_owner;
    if (!DSL_IR_Image_Get_Value((DSL_IR_VALUE_ID)source_id, &source) ||
        !DSL_IR_Image_Value_Belongs_To_PU(source, source_owner_pu_st))
        return FALSE;
    DSL_IR_NODE_RECORD source_node;
    DSL_IR_OPCODE_DESCRIPTOR_RECORD source_opcode;
    const char *source_kind = NULL;
    if (source.value_kind != DSL_IR_VALUE_CONSTANT ||
        !DSL_IR_Image_Get_Node(source.producer_node_id, &source_node) ||
        source_node.result_value_id != source.id ||
        !DSL_IR_Image_Get_Opcode_Descriptor
             (source_node.opcode_descriptor_id, &source_opcode) ||
        source_opcode.logical_operator != OPR_DSLTENSORCONST ||
        source_opcode.version != 1 ||
        source_opcode.effect_model != DSL_EFFECT_MODEL_PURE ||
        !DSL_IR_Node_Attribute(source_node, "value_kind", &source_kind) ||
        strcmp(source_kind, "external_data") != 0)
        return FALSE;

    lineage->value_id = value_id;
    lineage->source_owner_pu_st = source_owner_pu_st;
    lineage->source_value_id = (DSL_IR_VALUE_ID)source_id;
    lineage->transformation_name = name;
    lineage->transformation_version = (UINT32)version;
    lineage->transformation_ordinal = (UINT32)ordinal;
    return TRUE;
}

/* Scan owner-qualified values under resident selected-PU locals: check local ST
 * bounds even on untyped rows and require valid lineage for any typed marker.
 * Producerless rows are skipped after ST checks. No physical-definition, payload
 * or full core proof; FALSE is sometimes silent even with diagnostic supplied.
 * No mutation/PU activation; shared row/string validity is a prerequisite. */
BOOL
DSL_IR_Typed_External_Tensor_Validate_PU
        (PU_Info *pu_info, FILE *diagnostic)
{
    ST_IDX owner_pu_st = pu_info == NULL ? ST_IDX_ZERO :
        PU_Info_proc_sym(pu_info);
    if (pu_info == NULL || !DSL_IR_Image_Current_PU_Is(owner_pu_st))
        return FALSE;
    for (DSL_IR_VALUE_ID id = 1; id <= DSL_IR_Image_Value_Count(); ++id) {
        DSL_IR_VALUE_RECORD value;
        if (!DSL_IR_Image_Get_Value(id, &value) ||
            !DSL_IR_Image_Value_Belongs_To_PU(value, owner_pu_st))
            continue;
        if (ST_IDX_level(value.st) != CURRENT_SYMTAB ||
            ST_IDX_index(value.st) == 0 ||
            ST_IDX_index(value.st) >= ST_Table_Size(CURRENT_SYMTAB))
            return FALSE;
        DSL_IR_NODE_RECORD node;
        if (value.producer_node_id == DSL_IR_NODE_INVALID_ID)
            continue;
        if (!DSL_IR_Image_Get_Node(value.producer_node_id, &node))
            return FALSE;
        const char *field = NULL;
        if ((node.flags & DSL_IR_NODE_FLAG_TYPED_EXTERNAL_ROW) == 0 &&
            !DSL_IR_Node_Attribute
                 (node, "dsl.typed_external_row", &field) &&
            !DSL_IR_Node_Attribute
                 (node, "dsl.converted_from_owner_pu_st", &field) &&
            !DSL_IR_Node_Attribute
                 (node, "dsl.converted_from_value_id", &field) &&
            !DSL_IR_Node_Attribute
                 (node, "dsl.transformation_name", &field) &&
            !DSL_IR_Node_Attribute
                 (node, "dsl.transformation_version", &field) &&
            !DSL_IR_Node_Attribute
                 (node, "dsl.transformation_ordinal", &field))
            continue;
        DSL_IR_TYPED_EXTERNAL_TENSOR_LINEAGE lineage;
        if (!DSL_IR_Image_Get_Typed_External_Tensor_Lineage
                 (owner_pu_st, id, &lineage)) {
            if (diagnostic != NULL)
                fprintf(diagnostic,
                        "DSL typed external lineage error: value id=%u\n",
                        id);
            return FALSE;
        }
    }
    return TRUE;
}

/* Preflight one typed destination against a live copied source handle and
 * shared source identity, descriptor/TCON/range/path/checksum/lineage facts.
 * Caller supplies a derived containing BLOCK and nonnull string scratch; no
 * inactive local ST access or PU switch. FALSE leaves strings unchanged; TRUE
 * fills URI/payload only. No payload authentication; allocation failures escape. */
static BOOL
DSL_IR_Typed_External_Tensor_Request_Valid
        (const DSL_IR_TYPED_EXTERNAL_TENSOR_VALUE_REQUEST &request,
         const WN *insertion_block,
         std::string *uri,
         std::string *payload)
{
    const DSL_IR_EXTERNAL_SOURCE_SNAPSHOT *snapshot =
        DSL_IR_External_Tensor_Source_Snapshot(request.source_handle);
    DSL_IR_VALUE_RECORD source;
    DSL_IR_NODE_RECORD source_node;
    DSL_IR_OPCODE_DESCRIPTOR_RECORD source_opcode;
    TENSOR_DESCRIPTOR_RECORD descriptor;
    DSL_TENSOR_TCON_RECORD tensor_tcon;
    const char *side_path;
    UINT32 side_path_length;
    UINT64 element_size;
    UINT64 tensor_size;
    if (snapshot == NULL || request.name == NULL ||
        request.name[0] == '\0' || request.source_position == 0 ||
        insertion_block == NULL || request.insert_before == NULL ||
        request.source_owner_pu_st != snapshot->owner_pu_st ||
        request.source_value_id != snapshot->value_id ||
        !DSL_IR_Image_Get_Value(snapshot->value_id, &source) ||
        source.st != snapshot->st || source.ty != snapshot->descriptor_ty ||
        source.producer_node_id != snapshot->producer_node_id ||
        !DSL_IR_Image_Value_Belongs_To_PU
             (source, snapshot->owner_pu_st) ||
        !DSL_IR_Image_Get_Node(source.producer_node_id, &source_node) ||
        source_node.result_value_id != source.id ||
        !DSL_IR_Image_Get_Opcode_Descriptor
             (source_node.opcode_descriptor_id, &source_opcode) ||
        source_opcode.logical_operator != OPR_DSLTENSORCONST ||
        source_opcode.version != 1 ||
        source_opcode.effect_model != DSL_EFFECT_MODEL_PURE ||
        request.descriptor_ty == TY_IDX_ZERO ||
        request.tensor_tcon == TCON_IDX_ZERO ||
        !TY_get_tensor_descriptor_record(request.descriptor_ty, &descriptor) ||
        !DSL_IR_Static_Tensor_Byte_Size
             (descriptor, &element_size, &tensor_size) ||
        request.byte_length != tensor_size ||
        request.byte_offset % element_size != 0 ||
        request.byte_offset + request.byte_length < request.byte_offset ||
        !DSL_Tensor_TCON_Get(request.tensor_tcon, &tensor_tcon) ||
        tensor_tcon.storage_kind != DSL_TENSOR_TCON_STORAGE_SIDE_FILE_DENSE ||
        tensor_tcon.descriptor_ty != request.descriptor_ty ||
        tensor_tcon.element_mtype != TY_mtype(descriptor.element_ty) ||
        tensor_tcon.element_size != element_size ||
        tensor_tcon.element_count != tensor_size / element_size ||
        tensor_tcon.logical_bytes != tensor_size ||
        tensor_tcon.required_alignment < TY_align(request.descriptor_ty) ||
        tensor_tcon.byte_offset != request.byte_offset ||
        tensor_tcon.byte_length != request.byte_length ||
        !DSL_Tensor_TCON_Get_Side_Path
             (request.tensor_tcon, &side_path, &side_path_length) ||
        request.storage_format == NULL ||
        request.storage_format[0] == '\0' || request.side_file == NULL ||
        request.side_file[0] == '\0' ||
        strlen(request.side_file) != side_path_length ||
        memcmp(request.side_file, side_path, side_path_length) != 0 ||
        request.tensor_key == NULL || request.tensor_key[0] == '\0' ||
        request.checksum == NULL || strlen(request.checksum) != 64 ||
        !DSL_IR_Checksum_Valid(request.checksum) ||
        !DSL_IR_Transformation_Name_Valid(request.transformation_name) ||
        request.transformation_version == 0)
        return FALSE;
    for (UINT32 i = 0; i < 64; ++i) {
        if (!isdigit((unsigned char)request.checksum[i]) &&
            (request.checksum[i] < 'a' || request.checksum[i] > 'f'))
            return FALSE;
    }
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

    char offset_text[32];
    char length_text[32];
    snprintf(offset_text, sizeof(offset_text), "%llu",
             (unsigned long long)request.byte_offset);
    snprintf(length_text, sizeof(length_text), "%llu",
             (unsigned long long)request.byte_length);
    size_t uri_size = strlen(request.storage_format) +
                      strlen(request.side_file) +
                      strlen(request.tensor_key) +
                      strlen(request.checksum) + 96;
    std::vector<char> uri_text(uri_size);
    snprintf(&uri_text[0], uri_size,
             "%s://%s#%s?offset=%s&length=%s&checksum=%s",
             request.storage_format, request.side_file, request.tensor_key,
             offset_text, length_text, request.checksum);
    *uri = &uri_text[0];

    const char *dtype = Index_To_Str(descriptor.dtype);
    const char *shape = Index_To_Str(descriptor.logical_shape);
    size_t payload_size =
        strlen("name=;dtype=;rank=;shape=;value_kind=external_data;value=") +
        strlen(request.name) + strlen(dtype) + strlen(shape) + uri->size() +
        16;
    std::vector<char> payload_text(payload_size);
    snprintf(&payload_text[0], payload_size,
             "name=%s;dtype=%s;rank=%d;shape=%s;"
             "value_kind=external_data;value=%s",
             request.name, dtype, descriptor.rank, shape, uri->c_str());
    *payload = &payload_text[0];
    return TRUE;
}

/* Preflight the complete active-PU batch before creating typed constants;
 * leave calls/source definitions unchanged. Entry rejection preserves outputs;
 * later rejection leaves cleared results. Commit has no allocation journal. */
BOOL
DSL_IR_Materialize_Typed_External_Tensor_Values
        (PU_Info *pu_info,
         const DSL_IR_TYPED_EXTERNAL_TENSOR_VALUE_REQUEST *requests,
         UINT32 request_count,
         DSL_IR_TYPED_EXTERNAL_TENSOR_VALUE_RESULT *results)
{
    ST_IDX owner_pu_st = pu_info == NULL ? ST_IDX_ZERO :
        PU_Info_proc_sym(pu_info);
    WN *pu_root = pu_info == NULL ? NULL : PU_Info_tree_ptr(pu_info);
    if (pu_info != Current_PU_Info || pu_root == NULL ||
        !DSL_IR_Image_Current_PU_Is(owner_pu_st) || requests == NULL ||
        request_count == 0 || results == NULL ||
        !DSL_IR_Typed_External_Tensor_Validate_PU(pu_info, NULL))
        return FALSE;

    memset(results, 0, request_count * sizeof(*results));
    std::vector<DSL_IR_TYPED_EXTERNAL_TENSOR_LINEAGE> existing_lineage;
    for (DSL_IR_VALUE_ID id = 1; id <= DSL_IR_Image_Value_Count(); ++id) {
        DSL_IR_VALUE_RECORD value;
        if (!DSL_IR_Image_Get_Value(id, &value) ||
            !DSL_IR_Image_Value_Belongs_To_PU(value, owner_pu_st) ||
            ST_IDX_level(value.st) != CURRENT_SYMTAB ||
            ST_IDX_index(value.st) == 0 ||
            ST_IDX_index(value.st) >= ST_Table_Size(CURRENT_SYMTAB))
            continue;
        DSL_IR_NODE_RECORD node;
        const char *marker = NULL;
        if (!DSL_IR_Image_Get_Node(value.producer_node_id, &node) ||
            !DSL_IR_Node_Attribute
                 (node, "dsl.typed_external_row", &marker))
            continue;
        DSL_IR_TYPED_EXTERNAL_TENSOR_LINEAGE lineage;
        if (!DSL_IR_Image_Get_Typed_External_Tensor_Lineage
                 (owner_pu_st, id, &lineage))
            return FALSE;
        existing_lineage.push_back(lineage);
    }
    std::vector<WN *> blocks(request_count, NULL);
    std::vector<std::string> uris(request_count);
    std::vector<std::string> payloads(request_count);
    for (UINT32 i = 0; i < request_count; ++i) {
        const DSL_IR_TYPED_EXTERNAL_TENSOR_VALUE_REQUEST &request =
            requests[i];
        blocks[i] = DSL_IR_Find_Containing_Block
                        (pu_root, request.insert_before);
        if (!DSL_IR_Typed_External_Tensor_Request_Valid
                 (request, blocks[i], &uris[i], &payloads[i]) ||
            DSL_IR_PU_Value_Name_Exists(owner_pu_st, request.name))
            return FALSE;
        for (UINT32 prior = 0; prior < existing_lineage.size(); ++prior) {
            const DSL_IR_TYPED_EXTERNAL_TENSOR_LINEAGE &other =
                existing_lineage[prior];
            if (other.source_owner_pu_st == request.source_owner_pu_st &&
                other.source_value_id == request.source_value_id &&
                other.transformation_version ==
                    request.transformation_version &&
                other.transformation_ordinal ==
                    request.transformation_ordinal &&
                strcmp(other.transformation_name,
                       request.transformation_name) == 0)
                return FALSE;
        }
        for (UINT32 prior = 0; prior < i; ++prior) {
            const DSL_IR_TYPED_EXTERNAL_TENSOR_VALUE_REQUEST &other =
                requests[prior];
            if (strcmp(other.name, request.name) == 0 ||
                (other.source_owner_pu_st == request.source_owner_pu_st &&
                 other.source_value_id == request.source_value_id &&
                 other.transformation_version ==
                     request.transformation_version &&
                 other.transformation_ordinal ==
                     request.transformation_ordinal &&
                 strcmp(other.transformation_name,
                        request.transformation_name) == 0))
                return FALSE;
        }
    }
    DSL_IR_OPCODE_DESCRIPTOR_ID descriptor_id =
        DSL_IR_Image_Find_Opcode_Descriptor(OPR_DSLTENSORCONST, 1);
    if (descriptor_id == DSL_IR_OPCODE_DESCRIPTOR_INVALID_ID)
        return FALSE;

    /* Preflight used copied source facts rather than activating source PUs.
     * From here on, append destination symbols/rows without exception rollback. */
    for (UINT32 i = 0; i < request_count; ++i) {
        const DSL_IR_TYPED_EXTERNAL_TENSOR_VALUE_REQUEST &request =
            requests[i];
        ST_IDX result_st = DSL_Tensor_Create_Result_Symbol
                               (request.name, request.descriptor_ty,
                                SCLASS_AUTO, EXPORT_LOCAL);
        Set_ST_Srcpos(St_Table[result_st], request.source_position);

        char offset_text[32];
        char length_text[32];
        char tcon_text[32];
        char source_owner_text[32];
        char source_text[32];
        char version_text[32];
        char ordinal_text[32];
        snprintf(offset_text, sizeof(offset_text), "%llu",
                 (unsigned long long)request.byte_offset);
        snprintf(length_text, sizeof(length_text), "%llu",
                 (unsigned long long)request.byte_length);
        snprintf(tcon_text, sizeof(tcon_text), "%u", (UINT32)request.tensor_tcon);
        snprintf(source_owner_text, sizeof(source_owner_text), "%u",
                 (UINT32)request.source_owner_pu_st);
        snprintf(source_text, sizeof(source_text), "%u",
                 request.source_value_id);
        snprintf(version_text, sizeof(version_text), "%u",
                 request.transformation_version);
        snprintf(ordinal_text, sizeof(ordinal_text), "%u",
                 request.transformation_ordinal);

        WN *expression = DSL_WN_Create_Native
                             (OPR_DSLTENSORCONST, 1, payloads[i].c_str(),
                              NULL, 0);
        WN *definition = WN_CreateStid
                             (OPR_STID, MTYPE_V, MTYPE_M, 0, result_st,
                              request.descriptor_ty, expression);
        WN_Set_Linenum(definition, request.source_position);
        DSL_IR_NODE_RECORD node;
        DSL_IR_Node_Record_Init(&node);
        node.flags = DSL_IR_NODE_FLAG_TYPED_EXTERNAL_ROW;
        node.opcode_descriptor_id = descriptor_id;
        node.payload = Save_Str(payloads[i].c_str());
        DSL_IR_NODE_ID node_id = DSL_IR_Image_Add_Node(&node);
        const char *attribute_names[15] = {
            "value_kind", "value", "dsl.typed_external_row",
            "storage_format", "storage_file", "storage_tensor_key",
            "storage_byte_offset", "storage_byte_length",
            "storage_checksum", "tensor_tcon_idx",
            "dsl.converted_from_owner_pu_st",
            "dsl.converted_from_value_id", "dsl.transformation_name",
            "dsl.transformation_version", "dsl.transformation_ordinal"
        };
        const char *attribute_values[15] = {
            "external_data", uris[i].c_str(), "1",
            request.storage_format, request.side_file, request.tensor_key,
            offset_text, length_text, request.checksum, tcon_text,
            source_owner_text, source_text, request.transformation_name,
            version_text, ordinal_text
        };
        DSL_IR_ATTRIBUTE_ID first_attribute =
            DSL_IR_ATTRIBUTE_INVALID_ID;
        for (UINT32 attr = 0; attr < 15; ++attr) {
            DSL_IR_ATTRIBUTE_RECORD attribute;
            DSL_IR_Attribute_Record_Init(&attribute);
            attribute.owner_node_id = node_id;
            attribute.value_kind = DSL_IR_ATTRIBUTE_VALUE_STRING;
            attribute.name = Save_Str(attribute_names[attr]);
            attribute.value = Save_Str(attribute_values[attr]);
            DSL_IR_ATTRIBUTE_ID id = DSL_IR_Image_Add_Attribute(&attribute);
            if (attr == 0)
                first_attribute = id;
        }
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
             first_attribute, 15, value_id);
        WN_INSERT_BlockBefore(blocks[i], request.insert_before, definition);
        results[i].value_id = value_id;
        results[i].st = result_st;
        results[i].definition = definition;
    }
    return TRUE;
}

/* Test exactly 64 lowercase hexadecimal digest characters; NULL rejects.
 * This does not normalize/write the text or hash/authenticate any payload. */
static BOOL
DSL_IR_Lowercase_SHA256_Valid (const char *digest)
{
    if (digest == NULL || strlen(digest) != 64)
        return FALSE;
    for (UINT32 i = 0; i < 64; ++i) {
        if (!isdigit((unsigned char)digest[i]) &&
            (digest[i] < 'a' || digest[i] > 'f'))
            return FALSE;
    }
    return TRUE;
}

/* Preflight a source-free generated value's descriptor, dense TCON geometry,
 * path/range, provenance label/version and three lowercase digest spellings.
 * Require shared types/strings and nonnull URI/payload scratch; caller proves
 * insertion membership. FALSE preserves scratch, TRUE fills owned strings;
 * no symbol/row/file writes, geometry computation or digest authentication. */
static BOOL
DSL_IR_Generated_External_Tensor_Request_Valid
        (const DSL_IR_GENERATED_EXTERNAL_TENSOR_REQUEST &request,
         const WN *insertion_block, std::string *uri,
         std::string *payload)
{
    TENSOR_DESCRIPTOR_RECORD descriptor;
    DSL_TENSOR_TCON_RECORD tensor_tcon;
    const char *side_path;
    UINT32 side_path_length;
    UINT64 element_size;
    UINT64 tensor_size;
    if (request.name == NULL || request.name[0] == '\0' ||
        request.source_position == 0 || insertion_block == NULL ||
        request.insert_before == NULL ||
        request.descriptor_ty == TY_IDX_ZERO ||
        request.tensor_tcon == TCON_IDX_ZERO ||
        !TY_get_tensor_descriptor_record(request.descriptor_ty, &descriptor) ||
        !DSL_IR_Static_Tensor_Byte_Size
             (descriptor, &element_size, &tensor_size) ||
        request.byte_length != tensor_size ||
        request.byte_offset % element_size != 0 ||
        request.byte_offset + request.byte_length < request.byte_offset ||
        !DSL_Tensor_TCON_Get(request.tensor_tcon, &tensor_tcon) ||
        tensor_tcon.storage_kind != DSL_TENSOR_TCON_STORAGE_SIDE_FILE_DENSE ||
        tensor_tcon.descriptor_ty != request.descriptor_ty ||
        tensor_tcon.element_mtype != TY_mtype(descriptor.element_ty) ||
        tensor_tcon.element_size != element_size ||
        tensor_tcon.element_count != tensor_size / element_size ||
        tensor_tcon.logical_bytes != tensor_size ||
        tensor_tcon.required_alignment < TY_align(request.descriptor_ty) ||
        tensor_tcon.byte_offset != request.byte_offset ||
        tensor_tcon.byte_length != request.byte_length ||
        !DSL_Tensor_TCON_Get_Side_Path
             (request.tensor_tcon, &side_path, &side_path_length) ||
        request.storage_format == NULL ||
        request.storage_format[0] == '\0' ||
        request.side_file == NULL || request.side_file[0] == '\0' ||
        strlen(request.side_file) != side_path_length ||
        memcmp(request.side_file, side_path, side_path_length) != 0 ||
        request.tensor_key == NULL || request.tensor_key[0] == '\0' ||
        !DSL_IR_Lowercase_SHA256_Valid(request.checksum_sha256) ||
        !DSL_IR_Lowercase_SHA256_Valid
             (request.geometry_manifest_sha256) ||
        !DSL_IR_Lowercase_SHA256_Valid
             (request.variant_signature_sha256) ||
        !DSL_IR_Transformation_Name_Valid(request.generation_name) ||
        request.generation_version == 0)
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

    char offset_text[32];
    char length_text[32];
    snprintf(offset_text, sizeof(offset_text), "%llu",
             (unsigned long long)request.byte_offset);
    snprintf(length_text, sizeof(length_text), "%llu",
             (unsigned long long)request.byte_length);
    size_t uri_size = strlen(request.storage_format) +
                      strlen(request.side_file) +
                      strlen(request.tensor_key) +
                      strlen(request.checksum_sha256) + 96;
    std::vector<char> uri_text(uri_size);
    snprintf(&uri_text[0], uri_size,
             "%s://%s#%s?offset=%s&length=%s&checksum=%s",
             request.storage_format, request.side_file, request.tensor_key,
             offset_text, length_text, request.checksum_sha256);
    *uri = &uri_text[0];

    const char *dtype = Index_To_Str(descriptor.dtype);
    const char *shape = Index_To_Str(descriptor.logical_shape);
    size_t payload_size =
        strlen("name=;dtype=;rank=;shape=;value_kind=external_data;value=") +
        strlen(request.name) + strlen(dtype) + strlen(shape) + uri->size() +
        16;
    std::vector<char> payload_text(payload_size);
    snprintf(&payload_text[0], payload_size,
             "name=%s;dtype=%s;rank=%d;shape=%s;"
             "value_kind=external_data;value=%s",
             request.name, dtype, descriptor.rank, shape, uri->c_str());
    *payload = &payload_text[0];
    return TRUE;
}

/* Initialize every optional request field to its invalid default. */
void
DSL_IR_Generated_External_Tensor_Request_Init
        (DSL_IR_GENERATED_EXTERNAL_TENSOR_REQUEST *request)
{
    if (request != NULL)
        memset(request, 0, sizeof(*request));
}

/* Clear required provenance output, then verify the selected owner's reference,
 * exclusive generated marker, field spellings and bounded ordinals. Publish
 * borrowed names/digests only on TRUE; FALSE leaves zero output. Require valid
 * shared tables and matching resident locals; no geometry interpretation,
 * payload authentication, row mutation or PU activation. Allocations escape. */
BOOL
DSL_IR_Image_Get_Generated_External_Tensor_Provenance
        (ST_IDX owner_pu_st, DSL_IR_VALUE_ID value_id,
         DSL_IR_GENERATED_EXTERNAL_TENSOR_PROVENANCE *provenance)
{
    if (provenance != NULL)
        memset(provenance, 0, sizeof(*provenance));
    DSL_IR_EXTERNAL_TENSOR_REFERENCE reference;
    DSL_IR_VALUE_RECORD value;
    DSL_IR_NODE_RECORD node;
    const char *marker = NULL;
    const char *name = NULL;
    const char *version_text = NULL;
    const char *geometry = NULL;
    const char *stage_text = NULL;
    const char *diagonal_text = NULL;
    const char *variant = NULL;
    if (provenance == NULL ||
        !DSL_IR_Image_Get_External_Tensor_Reference
             (owner_pu_st, value_id, &reference) ||
        !DSL_IR_Lowercase_SHA256_Valid(reference.checksum) ||
        !DSL_IR_Image_Get_Value(value_id, &value) ||
        !DSL_IR_Image_Get_Node(value.producer_node_id, &node) ||
        (node.flags & DSL_IR_NODE_FLAG_GENERATED_EXTERNAL) == 0 ||
        (node.flags & DSL_IR_NODE_FLAG_TYPED_EXTERNAL_ROW) != 0 ||
        node.attribute_count != 16 ||
        !DSL_IR_Node_Attribute(node, "dsl.generated_external", &marker) ||
        strcmp(marker, "1") != 0 ||
        !DSL_IR_Node_Attribute(node, "dsl.generation_name", &name) ||
        !DSL_IR_Node_Attribute
             (node, "dsl.generation_version", &version_text) ||
        !DSL_IR_Node_Attribute
             (node, "dsl.geometry_manifest_sha256", &geometry) ||
        !DSL_IR_Node_Attribute(node, "dsl.stage_ordinal", &stage_text) ||
        !DSL_IR_Node_Attribute
             (node, "dsl.diagonal_ordinal", &diagonal_text) ||
        !DSL_IR_Node_Attribute
             (node, "dsl.variant_signature_sha256", &variant) ||
        !DSL_IR_Transformation_Name_Valid(name) ||
        !DSL_IR_Lowercase_SHA256_Valid(geometry) ||
        !DSL_IR_Lowercase_SHA256_Valid(variant))
        return FALSE;
    UINT64 version;
    UINT64 stage;
    UINT64 diagonal;
    if (!DSL_IR_Parse_Unsigned(version_text, &version) ||
        !DSL_IR_Parse_Unsigned(stage_text, &stage) ||
        !DSL_IR_Parse_Unsigned(diagonal_text, &diagonal) ||
        version == 0 || version > 0xffffffffu ||
        stage > 0xffffffffu || diagonal > 0xffffffffu)
        return FALSE;
    provenance->value_id = value_id;
    provenance->owner_pu_st = owner_pu_st;
    provenance->generation_name = name;
    provenance->generation_version = (UINT32)version;
    provenance->geometry_manifest_sha256 = geometry;
    provenance->stage_ordinal = (UINT32)stage;
    provenance->diagonal_ordinal = (UINT32)diagonal;
    provenance->variant_signature_sha256 = variant;
    return TRUE;
}

/* Scan owner-qualified values with resolvable producers for generated markers,
 * validate provenance and reject repeated generated names/semantic identities.
 * Caller has prevalidated core links and selected resident owner locals; missing
 * producers are skipped, not a full core error check. No tree/file/row mutation
 * or geometry proof; some FALSE paths are silent and scratch allocations escape. */
BOOL
DSL_IR_Generated_External_Tensor_Validate_PU
        (PU_Info *pu_info, FILE *diagnostic)
{
    ST_IDX owner = pu_info == NULL ? ST_IDX_ZERO : PU_Info_proc_sym(pu_info);
    if (pu_info == NULL || !DSL_IR_Image_Current_PU_Is(owner))
        return FALSE;
    std::set<std::string> identities;
    std::set<std::string> names;
    for (DSL_IR_VALUE_ID id = 1; id <= DSL_IR_Image_Value_Count(); ++id) {
        DSL_IR_VALUE_RECORD value;
        if (!DSL_IR_Image_Get_Value(id, &value) ||
            !DSL_IR_Image_Value_Belongs_To_PU(value, owner))
            continue;
        DSL_IR_NODE_RECORD node;
        if (value.producer_node_id == DSL_IR_NODE_INVALID_ID ||
            !DSL_IR_Image_Get_Node(value.producer_node_id, &node))
            continue;
        const char *marker = NULL;
        if ((node.flags & DSL_IR_NODE_FLAG_GENERATED_EXTERNAL) == 0 &&
            !DSL_IR_Node_Attribute
                 (node, "dsl.generated_external", &marker) &&
            !DSL_IR_Node_Attribute
                 (node, "dsl.geometry_manifest_sha256", &marker) &&
            !DSL_IR_Node_Attribute
                 (node, "dsl.variant_signature_sha256", &marker) &&
            !DSL_IR_Node_Attribute
                 (node, "dsl.generation_name", &marker) &&
            !DSL_IR_Node_Attribute
                 (node, "dsl.generation_version", &marker) &&
            !DSL_IR_Node_Attribute
                 (node, "dsl.stage_ordinal", &marker) &&
            !DSL_IR_Node_Attribute
                 (node, "dsl.diagonal_ordinal", &marker))
            continue;
        DSL_IR_GENERATED_EXTERNAL_TENSOR_PROVENANCE provenance;
        if (!DSL_IR_Image_Get_Generated_External_Tensor_Provenance
                 (owner, id, &provenance)) {
            if (diagnostic != NULL)
                fprintf(diagnostic,
                        "DSL generated external value error: id=%u\n", id);
            return FALSE;
        }
        if (value.name == STR_IDX_ZERO ||
            !names.insert(Index_To_Str(value.name)).second)
            return FALSE;
        char numeric[96];
        snprintf(numeric, sizeof(numeric), "%u:%u:%u",
                 provenance.generation_version,
                 provenance.stage_ordinal,
                 provenance.diagonal_ordinal);
        std::string identity = provenance.generation_name;
        identity += ':';
        identity += numeric;
        identity += ':';
        identity += provenance.geometry_manifest_sha256;
        identity += ':';
        identity += provenance.variant_signature_sha256;
        if (!identities.insert(identity).second) {
            if (diagnostic != NULL)
                fprintf(diagnostic,
                        "DSL generated external duplicate: id=%u\n", id);
            return FALSE;
        }
    }
    return TRUE;
}

/* Clear caller results, preflight all source-free requests, then materialize
 * the complete active-PU batch. Boolean rejection precedes row/physical writes;
 * commit allocation/assertion failures are not recoverable rollback. */
BOOL
DSL_IR_Materialize_Generated_External_Tensor_Values
        (PU_Info *pu_info,
         const DSL_IR_GENERATED_EXTERNAL_TENSOR_REQUEST *requests,
         UINT32 request_count,
         DSL_IR_GENERATED_EXTERNAL_TENSOR_RESULT *results)
{
    ST_IDX owner = pu_info == NULL ? ST_IDX_ZERO : PU_Info_proc_sym(pu_info);
    WN *root = pu_info == NULL ? NULL : PU_Info_tree_ptr(pu_info);
    if (results != NULL && request_count != 0)
        memset(results, 0, request_count * sizeof(*results));
    if (pu_info != Current_PU_Info || root == NULL ||
        !DSL_IR_Image_Current_PU_Is(owner) || requests == NULL ||
        request_count == 0 || results == NULL ||
        !DSL_IR_Generated_External_Tensor_Validate_PU(pu_info, NULL))
        return FALSE;
    std::vector<WN *> blocks(request_count, NULL);
    std::vector<std::string> uris(request_count);
    std::vector<std::string> payloads(request_count);
    for (UINT32 i = 0; i < request_count; ++i) {
        const DSL_IR_GENERATED_EXTERNAL_TENSOR_REQUEST &request = requests[i];
        blocks[i] = DSL_IR_Find_Containing_Block(root, request.insert_before);
        if (!DSL_IR_Generated_External_Tensor_Request_Valid
                 (request, blocks[i], &uris[i], &payloads[i]) ||
            DSL_IR_PU_Value_Name_Exists(owner, request.name))
            return FALSE;
        for (UINT32 prior = 0; prior < i; ++prior) {
            const DSL_IR_GENERATED_EXTERNAL_TENSOR_REQUEST &other =
                requests[prior];
            if (strcmp(other.name, request.name) == 0 ||
                (other.generation_version == request.generation_version &&
                 other.stage_ordinal == request.stage_ordinal &&
                 other.diagonal_ordinal == request.diagonal_ordinal &&
                 strcmp(other.generation_name,
                        request.generation_name) == 0 &&
                 strcmp(other.geometry_manifest_sha256,
                        request.geometry_manifest_sha256) == 0 &&
                 strcmp(other.variant_signature_sha256,
                        request.variant_signature_sha256) == 0))
                return FALSE;
        }
        for (DSL_IR_VALUE_ID id = 1; id <= DSL_IR_Image_Value_Count(); ++id) {
            DSL_IR_VALUE_RECORD value;
            if (!DSL_IR_Image_Get_Value(id, &value) ||
                !DSL_IR_Image_Value_Belongs_To_PU(value, owner))
                continue;
            DSL_IR_NODE_RECORD node;
            if (!DSL_IR_Image_Get_Node(value.producer_node_id, &node) ||
                (node.flags & DSL_IR_NODE_FLAG_GENERATED_EXTERNAL) == 0)
                continue;
            DSL_IR_GENERATED_EXTERNAL_TENSOR_PROVENANCE prior_value;
            if (!DSL_IR_Image_Get_Generated_External_Tensor_Provenance
                     (owner, id, &prior_value) ||
                (prior_value.generation_version ==
                     request.generation_version &&
                 prior_value.stage_ordinal == request.stage_ordinal &&
                 prior_value.diagonal_ordinal == request.diagonal_ordinal &&
                 strcmp(prior_value.generation_name,
                        request.generation_name) == 0 &&
                 strcmp(prior_value.geometry_manifest_sha256,
                        request.geometry_manifest_sha256) == 0 &&
                 strcmp(prior_value.variant_signature_sha256,
                        request.variant_signature_sha256) == 0))
                return FALSE;
        }
    }
    DSL_IR_OPCODE_DESCRIPTOR_ID descriptor_id =
        DSL_IR_Image_Find_Opcode_Descriptor(OPR_DSLTENSORCONST, 1);
    if (descriptor_id == DSL_IR_OPCODE_DESCRIPTOR_INVALID_ID)
        return FALSE;

    /* Commit trusts the reviewed generation identity and digest spelling;
     * payload contents and geometry computation remain the producer's job. */
    for (UINT32 i = 0; i < request_count; ++i) {
        const DSL_IR_GENERATED_EXTERNAL_TENSOR_REQUEST &request = requests[i];
        ST_IDX st = DSL_Tensor_Create_Result_Symbol
                        (request.name, request.descriptor_ty,
                         SCLASS_AUTO, EXPORT_LOCAL);
        Set_ST_Srcpos(St_Table[st], request.source_position);
        char offset[32];
        char length[32];
        char tcon[32];
        char version[32];
        char stage[32];
        char diagonal[32];
        snprintf(offset, sizeof(offset), "%llu",
                 (unsigned long long)request.byte_offset);
        snprintf(length, sizeof(length), "%llu",
                 (unsigned long long)request.byte_length);
        snprintf(tcon, sizeof(tcon), "%u", (UINT32)request.tensor_tcon);
        snprintf(version, sizeof(version), "%u", request.generation_version);
        snprintf(stage, sizeof(stage), "%u", request.stage_ordinal);
        snprintf(diagonal, sizeof(diagonal), "%u", request.diagonal_ordinal);
        WN *expression = DSL_WN_Create_Native
                             (OPR_DSLTENSORCONST, 1, payloads[i].c_str(),
                              NULL, 0);
        WN *definition = WN_CreateStid
                             (OPR_STID, MTYPE_V, MTYPE_M, 0, st,
                              request.descriptor_ty, expression);
        WN_Set_Linenum(definition, request.source_position);
        DSL_IR_NODE_RECORD node;
        DSL_IR_Node_Record_Init(&node);
        node.flags = DSL_IR_NODE_FLAG_GENERATED_EXTERNAL;
        node.opcode_descriptor_id = descriptor_id;
        node.payload = Save_Str(payloads[i].c_str());
        DSL_IR_NODE_ID node_id = DSL_IR_Image_Add_Node(&node);
        const char *names[16] = {
            "value_kind", "value", "dsl.generated_external",
            "storage_format", "storage_file", "storage_tensor_key",
            "storage_byte_offset", "storage_byte_length",
            "storage_checksum", "tensor_tcon_idx",
            "dsl.generation_name", "dsl.generation_version",
            "dsl.geometry_manifest_sha256", "dsl.stage_ordinal",
            "dsl.diagonal_ordinal", "dsl.variant_signature_sha256"
        };
        const char *values[16] = {
            "external_data", uris[i].c_str(), "1",
            request.storage_format, request.side_file, request.tensor_key,
            offset, length, request.checksum_sha256, tcon,
            request.generation_name, version,
            request.geometry_manifest_sha256, stage, diagonal,
            request.variant_signature_sha256
        };
        DSL_IR_ATTRIBUTE_ID first = DSL_IR_ATTRIBUTE_INVALID_ID;
        for (UINT32 attr = 0; attr < 16; ++attr) {
            DSL_IR_ATTRIBUTE_RECORD attribute;
            DSL_IR_Attribute_Record_Init(&attribute);
            attribute.owner_node_id = node_id;
            attribute.value_kind = DSL_IR_ATTRIBUTE_VALUE_STRING;
            attribute.name = Save_Str(names[attr]);
            attribute.value = Save_Str(values[attr]);
            DSL_IR_ATTRIBUTE_ID id = DSL_IR_Image_Add_Attribute(&attribute);
            if (attr == 0)
                first = id;
        }
        DSL_IR_VALUE_RECORD value;
        DSL_IR_Value_Record_Init(&value);
        value.value_kind = DSL_IR_VALUE_CONSTANT;
        value.producer_node_id = node_id;
        value.ty = request.descriptor_ty;
        value.st = st;
        value.name = Save_Str(request.name);
        std::string metadata = "owner_pu=";
        metadata += ST_name(St_Table[owner]);
        value.metadata = Save_Str(metadata.c_str());
        DSL_IR_VALUE_ID id = DSL_IR_Image_Add_Value(&value);
        DSL_IR_Image_Set_Node_Links
            (node_id, DSL_IR_VALUE_REFERENCE_INVALID_ID, 0, first, 16, id);
        WN_INSERT_BlockBefore(blocks[i], request.insert_before, definition);
        results[i].value_id = id;
        results[i].st = st;
        results[i].definition = definition;
    }
    return TRUE;
}
