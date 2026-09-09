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
#include "dsl_tensor_fold.h"
#include "dsl_ir_image.h"
#include "strtab.h"
#include "symtab.h"
#include "wn.h"
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
        !DSL_IR_Checksum_Valid(request.checksum) ||
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
    if (!DSL_IR_Image_Get_External_Tensor_Reference
             (owner_pu_st, request.source_value_id, &source_reference) ||
        source_reference.descriptor_ty != request.descriptor_ty ||
        !DSL_IR_Image_Get_Value(request.source_value_id, &source))
        return FALSE;
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
    for (UINT32 i = 0; i < request_count; ++i) {
        if (!DSL_IR_External_Tensor_Request_Valid
                 (owner_pu_st, requests[i], &source_values[i], &uris[i],
                  &payloads[i]) ||
            DSL_IR_PU_Value_Name_Exists(owner_pu_st, requests[i].name))
            return FALSE;
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
