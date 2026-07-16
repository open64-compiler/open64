/*
 * Copyright (C) 2026 Open64 Project
 */

#ifdef USE_PCH
#include "common_com_pch.h"
#endif /* USE_PCH */
#pragma hdrstop
#include <stdio.h>
#include <string.h>

#include "defs.h"
#include "config.h"
#include "config_targ_opt.h"
#include "mempool.h"
#include "erglob.h"
#include "errors.h"
#include "err_host.tab"
#include "config.h"
#include "config_targ_opt.h"
#include "stab.h"
#include "dsl_builder.h"
#include "open64_dsc_native_bridge.h"

static BOOL Open64_DSC_Context_Initialized = FALSE;

static TY_IDX
Open64_DSC_Dtype_To_TY(const char *dtype)
{
    if (dtype == NULL)
        return TY_IDX_ZERO;

    if (strcmp(dtype, "int32") == 0 || strcmp(dtype, "i32") == 0)
        return MTYPE_To_TY(MTYPE_I4);
    if (strcmp(dtype, "int64") == 0 || strcmp(dtype, "i64") == 0)
        return MTYPE_To_TY(MTYPE_I8);
    if (strcmp(dtype, "float32") == 0 || strcmp(dtype, "f32") == 0)
        return MTYPE_To_TY(MTYPE_F4);
    if (strcmp(dtype, "float64") == 0 || strcmp(dtype, "f64") == 0)
        return MTYPE_To_TY(MTYPE_F8);
    if (strcmp(dtype, "bool") == 0 || strcmp(dtype, "uint8") == 0 ||
        strcmp(dtype, "u8") == 0)
        return MTYPE_To_TY(MTYPE_U1);

    return TY_IDX_ZERO;
}

static DSL_DOMAIN_ID
Open64_DSC_Domain_For_Opcode(const char *opcode_name)
{
    const char *dot;
    size_t domain_len;
    char domain_name[64];

    if (opcode_name == NULL || opcode_name[0] == '\0')
        return DSL_DOMAIN_INVALID_ID;

    dot = strchr(opcode_name, '.');
    if (dot == NULL || dot == opcode_name)
        return DSL_Domain_Find("common");

    domain_len = (size_t) (dot - opcode_name);
    if (domain_len >= sizeof(domain_name))
        return DSL_DOMAIN_INVALID_ID;

    memcpy(domain_name, opcode_name, domain_len);
    domain_name[domain_len] = '\0';
    return DSL_Domain_Find(domain_name);
}

static void
Open64_DSC_Initialize_Context(void)
{
    if (Open64_DSC_Context_Initialized)
        return;

    MEM_Initialize();
    Set_Error_Tables(Phases, host_errlist);
    Init_Error_Handler(10);
    Set_Error_File(NULL);
    Set_Error_Line(ERROR_LINE_UNKNOWN);
    Preconfigure();
    ABI_Name = "n64";
    Configure();
    Initialize_Symbol_Tables(TRUE);
    DSL_Opcode_Register_Common_Substrate();
    DSL_Opcode_Register_Domain_Wrapper_Examples();

    Open64_DSC_Context_Initialized = TRUE;
}

static BOOL
Open64_DSC_Copy_Tensor_Descriptor
        (const Open64_DSC_Tensor_Descriptor *descriptor,
         DSL_BUILDER_TENSOR_DESCRIPTOR *builder_descriptor)
{
    if (descriptor == NULL || builder_descriptor == NULL ||
        descriptor->dtype == NULL || descriptor->dtype[0] == '\0' ||
        descriptor->rank < 0 || descriptor->logical_shape == NULL ||
        descriptor->logical_shape[0] == '\0')
        return FALSE;

    memset(builder_descriptor, 0, sizeof(*builder_descriptor));
    builder_descriptor->type_core.kind =
        descriptor->kind == NULL ? "tensor" : descriptor->kind;
    builder_descriptor->type_core.dtype = descriptor->dtype;
    builder_descriptor->type_core.rank = descriptor->rank;
    builder_descriptor->type_core.logical_shape = descriptor->logical_shape;
    builder_descriptor->traits.traits = descriptor->traits;
    builder_descriptor->representation.layout = descriptor->layout;
    builder_descriptor->representation.sharding = descriptor->sharding;
    builder_descriptor->representation.placement = descriptor->placement;
    builder_descriptor->representation.memory = descriptor->memory;
    builder_descriptor->representation.quantization = descriptor->quantization;
    builder_descriptor->representation.runtime_state = descriptor->runtime_state;
    builder_descriptor->lineage.lineage = descriptor->lineage;
    return TRUE;
}

Open64_DSC_Handle
Open64_DSC_Intern_Tensor_Type
        (const char *name,
         const Open64_DSC_Tensor_Descriptor *descriptor)
{
    DSL_BUILDER_TENSOR_DESCRIPTOR builder_descriptor;
    TY_IDX element_ty;

    if (name == NULL || name[0] == '\0' ||
        !Open64_DSC_Copy_Tensor_Descriptor(descriptor, &builder_descriptor))
        return 0;

    Open64_DSC_Initialize_Context();
    element_ty = Open64_DSC_Dtype_To_TY(descriptor->dtype);
    if (element_ty == TY_IDX_ZERO)
        return 0;
    TY_IDX tensor_ty = DSL_Builder_Intern_Tensor_Type
                           (name, element_ty, &builder_descriptor);
    return DSL_Builder_Tensor_Type_Is_Canonical(tensor_ty) ?
           (Open64_DSC_Handle) tensor_ty : 0;
}

Open64_DSC_Handle
Open64_DSC_Create_Tensor_Type(const char *name,
                              const char *dtype,
                              int rank,
                              const char *logical_shape)
{
    Open64_DSC_Tensor_Descriptor descriptor;

    if (name == NULL || name[0] == '\0' ||
        dtype == NULL || dtype[0] == '\0' ||
        rank < 0)
        return 0;

    memset(&descriptor, 0, sizeof(descriptor));
    descriptor.kind = "tensor";
    descriptor.dtype = dtype;
    descriptor.rank = rank;
    descriptor.logical_shape = logical_shape;
    return Open64_DSC_Intern_Tensor_Type(name, &descriptor);
}

int
Open64_DSC_Attach_Tensor_Descriptor
        (Open64_DSC_Handle tensor_type,
         const Open64_DSC_Tensor_Descriptor *descriptor)
{
    DSL_BUILDER_TENSOR_DESCRIPTOR builder_descriptor;

    if (tensor_type == 0 || descriptor == NULL ||
        descriptor->dtype == NULL || descriptor->dtype[0] == '\0' ||
        descriptor->rank < 0)
        return 0;

    Open64_DSC_Initialize_Context();

    if (!Open64_DSC_Copy_Tensor_Descriptor(descriptor, &builder_descriptor))
        return 0;

    return DSL_Builder_Attach_Tensor_Descriptor
               ((TY_IDX) tensor_type, &builder_descriptor) ? 1 : 0;
}

Open64_DSC_Handle
Open64_DSC_Create_Tensor_Constant(const char *name,
                                  const char *dtype,
                                  unsigned int rank,
                                  const char *logical_shape,
                                  const char *value_kind,
                                  const char *value)
{
    Open64_DSC_Tensor_Descriptor descriptor;
    TY_IDX tensor_ty;
    DSL_BUILDER_VALUE result;

    if (name == NULL || name[0] == '\0' ||
        dtype == NULL || dtype[0] == '\0' ||
        logical_shape == NULL || value_kind == NULL || value == NULL)
        return 0;

    Open64_DSC_Initialize_Context();

    memset(&descriptor, 0, sizeof(descriptor));
    descriptor.kind = "tensor";
    descriptor.dtype = dtype;
    descriptor.rank = rank;
    descriptor.logical_shape = logical_shape;
    tensor_ty = (TY_IDX) Open64_DSC_Intern_Tensor_Type(name, &descriptor);
    result = DSL_Builder_Create_Tensor_Constant
                 (name, tensor_ty, dtype, rank, logical_shape,
                  value_kind, value);
    return (Open64_DSC_Handle) result;
}
Open64_DSC_Handle
Open64_DSC_Create_Model_Input(const char *name,
                              Open64_DSC_Handle tensor_type,
                              unsigned int input_ordinal)
{
    DSL_BUILDER_VALUE value;

    if (name == NULL || name[0] == '\0' || tensor_type == 0)
        return 0;

    Open64_DSC_Initialize_Context();

    value = DSL_Builder_Create_Model_Input(name, (TY_IDX) tensor_type,
                                           (UINT32) input_ordinal);
    return (Open64_DSC_Handle) value;
}

Open64_DSC_Handle
Open64_DSC_Create_External_Tensor_Constant
        (const char *name,
         Open64_DSC_Handle tensor_type,
         const Open64_DSC_External_Tensor_Reference *reference)
{
    DSL_BUILDER_EXTERNAL_TENSOR_REFERENCE builder_reference;
    DSL_BUILDER_VALUE value;

    if (name == NULL || name[0] == '\0' || tensor_type == 0 ||
        reference == NULL ||
        reference->storage_format == NULL ||
        reference->storage_format[0] == '\0' ||
        reference->side_file == NULL || reference->side_file[0] == '\0' ||
        reference->tensor_key == NULL || reference->tensor_key[0] == '\0' ||
        reference->byte_length == 0)
        return 0;

    Open64_DSC_Initialize_Context();

    builder_reference.storage_format = reference->storage_format;
    builder_reference.side_file = reference->side_file;
    builder_reference.tensor_key = reference->tensor_key;
    builder_reference.byte_offset = (UINT64) reference->byte_offset;
    builder_reference.byte_length = (UINT64) reference->byte_length;
    builder_reference.checksum = reference->checksum;

    value = DSL_Builder_Create_External_Tensor_Constant
                (name, (TY_IDX) tensor_type, &builder_reference);
    return (Open64_DSC_Handle) value;
}

Open64_DSC_Handle
Open64_DSC_Create_Operator(const char *opcode_name,
                           unsigned int version,
                           const Open64_DSC_Handle *kids,
                           unsigned int kid_count,
                           const Open64_DSC_Attribute *attrs,
                           unsigned int attr_count)
{
    DSL_DOMAIN_ID domain_id;
    DSL_OPCODE_ID opcode_id;
    DSL_BUILDER_VALUE *builder_kids = NULL;
    DSL_BUILDER_OPERATOR_ATTRIBUTE *builder_attrs = NULL;
    DSL_BUILDER_OPERATOR op;
    unsigned int i;

    if (opcode_name == NULL || opcode_name[0] == '\0' || version == 0 ||
        (kid_count != 0 && kids == NULL) ||
        (attr_count != 0 && attrs == NULL))
        return 0;

    Open64_DSC_Initialize_Context();

    domain_id = Open64_DSC_Domain_For_Opcode(opcode_name);
    if (domain_id == DSL_DOMAIN_INVALID_ID)
        return 0;

    opcode_id = DSL_Opcode_Find(domain_id, opcode_name, (UINT16) version);
    if (opcode_id == DSL_OPCODE_INVALID_ID)
        return 0;

    if (kid_count != 0) {
        builder_kids = new DSL_BUILDER_VALUE[kid_count];
        for (i = 0; i < kid_count; ++i) {
            if (kids[i] == 0) {
                delete [] builder_kids;
                return 0;
            }
            builder_kids[i] = (DSL_BUILDER_VALUE) kids[i];
        }
    }

    if (attr_count != 0) {
        builder_attrs = new DSL_BUILDER_OPERATOR_ATTRIBUTE[attr_count];
        for (i = 0; i < attr_count; ++i) {
            if (attrs[i].name == NULL || attrs[i].name[0] == '\0') {
                delete [] builder_kids;
                delete [] builder_attrs;
                return 0;
            }
            builder_attrs[i].name = attrs[i].name;
            builder_attrs[i].value = attrs[i].value;
        }
    }

    op = DSL_Builder_Create_Operator(opcode_id, (UINT16) version,
                                     builder_kids, kid_count,
                                     builder_attrs, attr_count);

    delete [] builder_kids;
    delete [] builder_attrs;

    return (Open64_DSC_Handle) op;
}

Open64_DSC_Handle
Open64_DSC_Create_Operator_With_Result
        (const char *opcode_name,
         unsigned int version,
         const Open64_DSC_Handle *kids,
         unsigned int kid_count,
         const Open64_DSC_Attribute *attrs,
         unsigned int attr_count,
         const char *result_name,
         Open64_DSC_Handle result_type)
{
    DSL_DOMAIN_ID domain_id;
    DSL_OPCODE_ID opcode_id;
    DSL_BUILDER_VALUE *builder_kids = NULL;
    DSL_BUILDER_OPERATOR_ATTRIBUTE *builder_attrs = NULL;
    DSL_BUILDER_OPERATOR op;
    unsigned int i;

    if (opcode_name == NULL || version == 0 || result_name == NULL ||
        result_name[0] == '\0' || result_type == 0 ||
        (kid_count != 0 && kids == NULL) ||
        (attr_count != 0 && attrs == NULL))
        return 0;
    Open64_DSC_Initialize_Context();
    domain_id = Open64_DSC_Domain_For_Opcode(opcode_name);
    if (domain_id == DSL_DOMAIN_INVALID_ID)
        return 0;
    opcode_id = DSL_Opcode_Find(domain_id, opcode_name, (UINT16) version);
    if (opcode_id == DSL_OPCODE_INVALID_ID)
        return 0;

    if (kid_count != 0) {
        builder_kids = new DSL_BUILDER_VALUE[kid_count];
        for (i = 0; i < kid_count; ++i)
            builder_kids[i] = (DSL_BUILDER_VALUE) kids[i];
    }
    if (attr_count != 0) {
        builder_attrs = new DSL_BUILDER_OPERATOR_ATTRIBUTE[attr_count];
        for (i = 0; i < attr_count; ++i) {
            builder_attrs[i].name = attrs[i].name;
            builder_attrs[i].value = attrs[i].value;
        }
    }
    op = DSL_Builder_Create_Operator_With_Result
             (opcode_id, (UINT16) version, builder_kids, kid_count,
              builder_attrs, attr_count, result_name, (TY_IDX) result_type);
    delete [] builder_kids;
    delete [] builder_attrs;
    return (Open64_DSC_Handle) op;
}

Open64_DSC_Handle
Open64_DSC_Create_Symbol(const char *name, Open64_DSC_Handle tensor_type)
{
    ST_IDX st;

    if (name == NULL || name[0] == '\0' || tensor_type == 0)
        return 0;

    Open64_DSC_Initialize_Context();

    st = DSL_Builder_Create_Symbol(name, (TY_IDX) tensor_type, CLASS_VAR,
                                   SCLASS_UGLOBAL, EXPORT_LOCAL);
    return (Open64_DSC_Handle) st;
}

int
Open64_DSC_Attach_Symbol_Metadata(Open64_DSC_Handle symbol,
                                  const Open64_DSC_Attribute *metadata,
                                  unsigned int metadata_count)
{
    DSL_BUILDER_COMPILER_METADATA *builder_metadata = NULL;
    unsigned int i;
    BOOL ok;

    if (symbol == 0 || (metadata_count != 0 && metadata == NULL))
        return 0;

    Open64_DSC_Initialize_Context();

    if (metadata_count != 0) {
        builder_metadata =
            new DSL_BUILDER_COMPILER_METADATA[metadata_count];
        for (i = 0; i < metadata_count; ++i) {
            if (metadata[i].name == NULL || metadata[i].name[0] == '\0') {
                delete [] builder_metadata;
                return 0;
            }
            builder_metadata[i].name = metadata[i].name;
            builder_metadata[i].value = metadata[i].value;
        }
    }

    ok = DSL_Builder_Attach_Metadata((ST_IDX) symbol, builder_metadata,
                                     metadata_count);
    delete [] builder_metadata;
    return ok ? 1 : 0;
}

int
Open64_DSC_Attach_Value_Metadata(Open64_DSC_Handle value,
                                 const Open64_DSC_Attribute *metadata,
                                 unsigned int metadata_count)
{
    DSL_BUILDER_COMPILER_METADATA *builder_metadata = NULL;
    unsigned int i;
    BOOL ok;

    Open64_DSC_Initialize_Context();
    if (metadata_count != 0 && metadata == NULL)
        return 0;
    if (metadata_count != 0) {
        builder_metadata = new DSL_BUILDER_COMPILER_METADATA[metadata_count];
        for (i = 0; i < metadata_count; ++i) {
            builder_metadata[i].name = metadata[i].name;
            builder_metadata[i].value = metadata[i].value;
        }
    }
    ok = DSL_Builder_Attach_Value_Metadata
             ((DSL_BUILDER_VALUE) value, builder_metadata, metadata_count);
    delete [] builder_metadata;
    return ok ? 1 : 0;
}

int
Open64_DSC_Attach_Value_Lineage(Open64_DSC_Handle value, const char *lineage)
{
    Open64_DSC_Initialize_Context();
    return DSL_Builder_Attach_Value_Lineage
               ((DSL_BUILDER_VALUE) value, lineage) ? 1 : 0;
}

Open64_DSC_Handle
Open64_DSC_Get_Value_Type(Open64_DSC_Handle value)
{
    Open64_DSC_Initialize_Context();
    return (Open64_DSC_Handle) DSL_Builder_Get_Value_Type
               ((DSL_BUILDER_VALUE) value);
}

Open64_DSC_Handle
Open64_DSC_Get_Value_Result_Symbol(Open64_DSC_Handle value)
{
    Open64_DSC_Initialize_Context();
    return (Open64_DSC_Handle) DSL_Builder_Get_Value_Result_Symbol
               ((DSL_BUILDER_VALUE) value);
}

int
Open64_DSC_Begin_Program(void)
{
    Open64_DSC_Initialize_Context();
    return DSL_Builder_Begin_Program() ? 1 : 0;
}

void
Open64_DSC_Abort_Program(void)
{
    Open64_DSC_Initialize_Context();
    DSL_Builder_Abort_Program();
}

Open64_DSC_Handle
Open64_DSC_Create_Minimal_Program_Unit(const char *name)
{
    DSL_BUILDER_PROGRAM_UNIT pu;

    if (name == NULL || name[0] == '\0')
        return 0;

    Open64_DSC_Initialize_Context();

    pu = DSL_Builder_Create_Minimal_PU(name);
    return (Open64_DSC_Handle) pu;
}

unsigned int
Open64_DSC_Register_Source_File(Open64_DSC_Handle program_unit,
                                const char *path)
{
    Open64_DSC_Initialize_Context();
    return DSL_Builder_Register_Source_File
               ((DSL_BUILDER_PROGRAM_UNIT) program_unit, path);
}

int
Open64_DSC_Set_Value_Source_Position
        (Open64_DSC_Handle value,
         const Open64_DSC_Source_Position *position)
{
    DSL_BUILDER_SOURCE_POSITION builder_position;

    if (position == NULL)
        return 0;
    Open64_DSC_Initialize_Context();
    builder_position.file_id = position->file_id;
    builder_position.line = position->line;
    builder_position.column = position->column;
    builder_position.statement_begin = position->statement_begin;
    builder_position.basic_block_begin = position->basic_block_begin;
    return DSL_Builder_Set_Value_Source_Position
               ((DSL_BUILDER_VALUE) value, &builder_position) ? 1 : 0;
}

Open64_DSC_Handle
Open64_DSC_Create_Region(Open64_DSC_Handle program_unit,
                         Open64_DSC_Handle parent_region,
                         const char *contract_name,
                         unsigned int contract_version)
{
    Open64_DSC_Initialize_Context();
    return (Open64_DSC_Handle) DSL_Builder_Create_Region
               ((DSL_BUILDER_PROGRAM_UNIT) program_unit,
                (DSL_BUILDER_REGION) parent_region, contract_name,
                (UINT32) contract_version);
}

int
Open64_DSC_Append_Region_Value(Open64_DSC_Handle region,
                               Open64_DSC_Handle value)
{
    return DSL_Builder_Append_Region_Value
               ((DSL_BUILDER_REGION) region,
                (DSL_BUILDER_VALUE) value) ? 1 : 0;
}

int
Open64_DSC_Append_Program_Unit_Region(Open64_DSC_Handle program_unit,
                                      Open64_DSC_Handle region)
{
    return DSL_Builder_Append_PU_Region
               ((DSL_BUILDER_PROGRAM_UNIT) program_unit,
                (DSL_BUILDER_REGION) region) ? 1 : 0;
}

int
Open64_DSC_Declare_Region_Value(Open64_DSC_Handle region,
                                Open64_DSC_Handle value,
                                unsigned int roles,
                                unsigned int ordinal,
                                unsigned int flags)
{
    return DSL_Builder_Declare_Region_Value
               ((DSL_BUILDER_REGION) region, (DSL_BUILDER_VALUE) value,
                (UINT32) roles, (UINT32) ordinal, (UINT32) flags) ? 1 : 0;
}

Open64_DSC_Handle
Open64_DSC_Declare_State_Object(Open64_DSC_Handle program_unit,
                                const char *name,
                                unsigned int kind,
                                unsigned int flags)
{
    Open64_DSC_Initialize_Context();
    return (Open64_DSC_Handle) DSL_Builder_Declare_State_Object_With_Flags
               ((DSL_BUILDER_PROGRAM_UNIT) program_unit, name,
                (DSL_STATE_KIND) kind, (UINT32) flags);
}

int
Open64_DSC_Add_State_Effect(Open64_DSC_Handle value,
                            Open64_DSC_Handle state,
                            unsigned int effect_kind)
{
    return DSL_Builder_Add_State_Effect
               ((DSL_BUILDER_VALUE) value, (DSL_BUILDER_STATE) state,
                (DSL_STATE_EFFECT_KIND) effect_kind) ? 1 : 0;
}

int
Open64_DSC_Declare_Region_State(Open64_DSC_Handle region,
                                Open64_DSC_Handle state,
                                unsigned int effect_kind,
                                unsigned int ordinal,
                                unsigned int flags)
{
    return DSL_Builder_Declare_Region_State
               ((DSL_BUILDER_REGION) region, (DSL_BUILDER_STATE) state,
                (DSL_STATE_EFFECT_KIND) effect_kind, (UINT32) ordinal,
                (UINT32) flags) ? 1 : 0;
}

int
Open64_DSC_Set_Region_Source_Position
        (Open64_DSC_Handle region,
         const Open64_DSC_Source_Position *position)
{
    DSL_BUILDER_SOURCE_POSITION builder_position;
    if (position == NULL)
        return 0;
    builder_position.file_id = position->file_id;
    builder_position.line = position->line;
    builder_position.column = position->column;
    builder_position.statement_begin = position->statement_begin;
    builder_position.basic_block_begin = position->basic_block_begin;
    return DSL_Builder_Set_Region_Source_Position
               ((DSL_BUILDER_REGION) region, &builder_position) ? 1 : 0;
}

int
Open64_DSC_Verify_Program(Open64_DSC_Verify_Result *result)
{
    DSL_BUILDER_VERIFY_RESULT builder_result;
    BOOL valid;

    if (result == NULL)
        return 0;
    builder_result.native_node_count = 0;
    builder_result.result_symbol_count = 0;
    builder_result.error_count = 0;
    builder_result.diagnostic = result->diagnostic;
    builder_result.diagnostic_capacity = result->diagnostic_capacity;
    valid = DSL_Builder_Verify_Program(&builder_result);
    result->native_node_count = builder_result.native_node_count;
    result->result_symbol_count = builder_result.result_symbol_count;
    result->error_count = builder_result.error_count;
    return valid ? 1 : 0;
}

int
Open64_DSC_Append_Program_Unit_Value(Open64_DSC_Handle program_unit,
                                     Open64_DSC_Handle value)
{
    if (program_unit == 0 || value == 0)
        return 0;

    Open64_DSC_Initialize_Context();

    return DSL_Builder_Append_PU_Value
               ((DSL_BUILDER_PROGRAM_UNIT) program_unit,
                (DSL_BUILDER_VALUE) value) ? 1 : 0;
}

unsigned int
Open64_DSC_Count_Program_Unit_Values(Open64_DSC_Handle program_unit)
{
    if (program_unit == 0)
        return 0;

    Open64_DSC_Initialize_Context();

    return (unsigned int) DSL_Builder_Count_PU_Values
                              ((DSL_BUILDER_PROGRAM_UNIT) program_unit);
}

int
Open64_DSC_Get_Program_Unit_Value(Open64_DSC_Handle program_unit,
                                  unsigned int index,
                                  Open64_DSC_Value_Info *info)
{
    DSL_BUILDER_VALUE_INFO builder_info;

    if (program_unit == 0 || info == NULL)
        return 0;

    Open64_DSC_Initialize_Context();

    if (!DSL_Builder_Get_PU_Value((DSL_BUILDER_PROGRAM_UNIT) program_unit,
                                  (UINT32) index, &builder_info))
        return 0;

    info->opcode_name = builder_info.opcode_name;
    info->opcode_name_len = builder_info.opcode_name_len;
    info->version = builder_info.version;
    info->payload = builder_info.payload;
    return 1;
}

int
Open64_DSC_Append_Program_Unit_Marker(Open64_DSC_Handle program_unit,
                                      Open64_DSC_Handle marker)
{
    return Open64_DSC_Append_Program_Unit_Value(program_unit, marker);
}

unsigned int
Open64_DSC_Count_Program_Unit_Markers(Open64_DSC_Handle program_unit)
{
    return Open64_DSC_Count_Program_Unit_Values(program_unit);
}

int
Open64_DSC_Get_Program_Unit_Marker(Open64_DSC_Handle program_unit,
                                   unsigned int index,
                                   Open64_DSC_Marker_Info *info)
{
    return Open64_DSC_Get_Program_Unit_Value(program_unit, index, info);
}

int
Open64_DSC_Finalize_Mapped_Image(const char *path)
{
    DSL_BUILDER_MAPPED_IMAGE_REQUEST request;

    if (path == NULL || path[0] == '\0')
        return 0;

    Open64_DSC_Initialize_Context();

    request.path = path;
    request.flags = 0;

    return DSL_Builder_Finalize_Mapped_Image(&request) ? 1 : 0;
}
