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
#include "mempool.h"
#include "erglob.h"
#include "errors.h"
#include "err_host.tab"
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
    Initialize_Symbol_Tables(TRUE);
    DSL_Opcode_Register_Common_Substrate();

    Open64_DSC_Context_Initialized = TRUE;
}

Open64_DSC_Handle
Open64_DSC_Create_Tensor_Type(const char *name,
                              const char *dtype,
                              int rank,
                              const char *logical_shape)
{
    DSL_BUILDER_TENSOR_TYPE_CORE type_core;
    TY_IDX element_ty;
    TY_IDX tensor_ty;

    if (name == NULL || name[0] == '\0' ||
        dtype == NULL || dtype[0] == '\0' ||
        rank < 0)
        return 0;

    Open64_DSC_Initialize_Context();

    element_ty = Open64_DSC_Dtype_To_TY(dtype);
    if (element_ty == TY_IDX_ZERO)
        return 0;

    type_core.kind = "tensor";
    type_core.dtype = dtype;
    type_core.rank = rank;
    type_core.logical_shape = logical_shape;

    tensor_ty = DSL_Builder_Create_Tensor_Type_Core(name, element_ty,
                                                    &type_core);
    return (Open64_DSC_Handle) tensor_ty;
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

    memset(&builder_descriptor, 0, sizeof(builder_descriptor));
    builder_descriptor.type_core.kind =
        descriptor->kind == NULL ? "tensor" : descriptor->kind;
    builder_descriptor.type_core.dtype = descriptor->dtype;
    builder_descriptor.type_core.rank = descriptor->rank;
    builder_descriptor.type_core.logical_shape = descriptor->logical_shape;
    builder_descriptor.traits.traits = descriptor->traits;
    builder_descriptor.representation.layout = descriptor->layout;
    builder_descriptor.representation.sharding = descriptor->sharding;
    builder_descriptor.representation.placement = descriptor->placement;
    builder_descriptor.representation.memory = descriptor->memory;
    builder_descriptor.representation.quantization = descriptor->quantization;
    builder_descriptor.representation.runtime_state =
        descriptor->runtime_state;
    builder_descriptor.lineage.lineage = descriptor->lineage;

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
    WN *wn;

    if (name == NULL || name[0] == '\0' ||
        dtype == NULL || dtype[0] == '\0' ||
        logical_shape == NULL || value_kind == NULL || value == NULL)
        return 0;

    Open64_DSC_Initialize_Context();

    if (Open64_DSC_Dtype_To_TY(dtype) == TY_IDX_ZERO)
        return 0;

    wn = DSL_WN_Create_Tensor_Const(name, dtype, rank, logical_shape,
                                    value_kind, value);
    return (Open64_DSC_Handle) wn;
}

Open64_DSC_Handle
Open64_DSC_Create_Operator(const char *opcode_name,
                           unsigned int version,
                           const Open64_DSC_Handle *kids,
                           unsigned int kid_count,
                           const Open64_DSC_Attribute *attrs,
                           unsigned int attr_count)
{
    DSL_DOMAIN_ID common_id;
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

    common_id = DSL_Domain_Find("common");
    opcode_id = DSL_Opcode_Find(common_id, opcode_name, (UINT16) version);
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

int
Open64_DSC_Append_Program_Unit_Marker(Open64_DSC_Handle program_unit,
                                      Open64_DSC_Handle marker)
{
    if (program_unit == 0 || marker == 0)
        return 0;

    Open64_DSC_Initialize_Context();

    return DSL_Builder_Append_PU_Marker
               ((DSL_BUILDER_PROGRAM_UNIT) program_unit,
                (DSL_BUILDER_VALUE) marker) ? 1 : 0;
}

unsigned int
Open64_DSC_Count_Program_Unit_Markers(Open64_DSC_Handle program_unit)
{
    if (program_unit == 0)
        return 0;

    Open64_DSC_Initialize_Context();

    return (unsigned int) DSL_Builder_Count_PU_Markers
                              ((DSL_BUILDER_PROGRAM_UNIT) program_unit);
}

int
Open64_DSC_Get_Program_Unit_Marker(Open64_DSC_Handle program_unit,
                                   unsigned int index,
                                   Open64_DSC_Marker_Info *info)
{
    DSL_BUILDER_MARKER_INFO builder_info;

    if (program_unit == 0 || info == NULL)
        return 0;

    Open64_DSC_Initialize_Context();

    if (!DSL_Builder_Get_PU_Marker((DSL_BUILDER_PROGRAM_UNIT) program_unit,
                                   (UINT32) index, &builder_info))
        return 0;

    info->opcode_name = builder_info.opcode_name;
    info->opcode_name_len = builder_info.opcode_name_len;
    info->version = builder_info.version;
    info->payload = builder_info.payload;
    return 1;
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
