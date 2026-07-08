/*
 * Copyright (C) 2026 Open64 Project
 */

#ifdef USE_PCH
#include "common_com_pch.h"
#endif /* USE_PCH */
#pragma hdrstop
#include <stdio.h>
#include <string.h>
#if ! defined(BUILD_OS_DARWIN)
#include <elf.h>
#endif /* ! defined(BUILD_OS_DARWIN) */

#include "dsl_builder.h"
#include "dwarf_DST_mem.h"
#include "glob.h"
#include "pu_info.h"
#include "ir_bwrite.h"
#include "mempool.h"
#include "stab.h"
#include "strtab.h"
#include "symtab_utils.h"

static PU_Info *DSL_Builder_PU_Root = NULL;
static PU_Info *DSL_Builder_PU_Last = NULL;

static const char *
DSL_Builder_Safe_String (const char *value)
{
    return value == NULL ? "" : value;
}

static void
DSL_Builder_Bind_Attribute_If_Present (TY_IDX ty,
                                       TY_TENSOR_SCHEMA_KEY key,
                                       const char *value)
{
    if (value != NULL)
        TY_tensor_bind_attribute (ty, key, value);
}

static const char *
DSL_Builder_Find_Payload_Value (const char *payload, const char *key,
                                char *buffer, size_t buffer_size)
{
    const char *match;
    const char *value;
    const char *end;
    size_t key_len;
    size_t value_len;

    if (payload == NULL || key == NULL || buffer == NULL || buffer_size == 0)
        return NULL;

    key_len = strlen (key);
    match = strstr (payload, key);
    while (match != NULL) {
        if ((match == payload || match[-1] == ';') &&
            match[key_len] == '=') {
            value = match + key_len + 1;
            end = strchr (value, ';');
            value_len = end == NULL ? strlen (value) :
                                      (size_t)(end - value);
            if (value_len >= buffer_size)
                value_len = buffer_size - 1;
            memcpy (buffer, value, value_len);
            buffer[value_len] = '\0';
            return buffer;
        }
        match = strstr (match + 1, key);
    }

    return NULL;
}

static const char *
DSL_Builder_Kid_Name (DSL_BUILDER_VALUE kid, UINT32 ordinal,
                      char *buffer, size_t buffer_size)
{
    DSL_OPCODE_ANNOTATION annotation;
    const char *name;

    if (buffer == NULL || buffer_size == 0)
        return "";

    if (kid != NULL &&
        DSL_WN_Get_Opcode_Annotation (kid, &annotation) &&
        annotation.payload != NULL) {
        name = DSL_Builder_Find_Payload_Value (annotation.payload, "name",
                                               buffer, buffer_size);
        if (name != NULL)
            return name;
    }

    snprintf (buffer, buffer_size, "kid%u", ordinal);
    return buffer;
}

static char *
DSL_Builder_Format_Operator_Payload
        (DSL_BUILDER_VALUE *kids,
         UINT32 kid_count,
         const DSL_BUILDER_OPERATOR_ATTRIBUTE *attrs,
         UINT32 attr_count)
{
    size_t payload_size = 1;
    BOOL first = TRUE;
    char *payload;
    size_t offset;

    for (UINT32 i = 0; i < kid_count; ++i) {
        char kid_name[256];
        const char *name = DSL_Builder_Kid_Name
                               (kids == NULL ? NULL : kids[i],
                                i, kid_name, sizeof(kid_name));
        payload_size += (i == 0 ? 0 : 1) + strlen("kid") + 10 + 1 +
                        strlen(name);
    }

    for (UINT32 i = 0; attrs != NULL && i < attr_count; ++i) {
        const char *name = DSL_Builder_Safe_String (attrs[i].name);
        const char *value = DSL_Builder_Safe_String (attrs[i].value);
        payload_size += (payload_size == 1 ? 0 : 1) + strlen(name) + 1 +
                        strlen(value);
    }

    payload = new char[payload_size];
    payload[0] = '\0';
    offset = 0;

    for (UINT32 i = 0; i < kid_count; ++i) {
        char kid_name[256];
        const char *name = DSL_Builder_Kid_Name
                               (kids == NULL ? NULL : kids[i],
                                i, kid_name, sizeof(kid_name));
        offset += snprintf (payload + offset, payload_size - offset,
                            "%skid%u=%s", first ? "" : ";", i, name);
        first = FALSE;
    }

    for (UINT32 i = 0; attrs != NULL && i < attr_count; ++i) {
        const char *name = DSL_Builder_Safe_String (attrs[i].name);
        const char *value = DSL_Builder_Safe_String (attrs[i].value);
        offset += snprintf (payload + offset, payload_size - offset,
                            "%s%s=%s", first ? "" : ";", name, value);
        first = FALSE;
    }

    return payload;
}

TY_IDX
DSL_Builder_Create_Tensor_Type_Core
        (const char *name,
         TY_IDX element_ty,
         const DSL_BUILDER_TENSOR_TYPE_CORE *type_core)
{
    char rank_buf[32];
    INT32 rank = type_core == NULL ? -1 : type_core->rank;
    TY_IDX tensor_ty = TY_Create_Tensor_Type (name, element_ty, rank);

    if (type_core != NULL) {
        snprintf (rank_buf, sizeof(rank_buf), "%d", type_core->rank);
        DSL_Builder_Bind_Attribute_If_Present
            (tensor_ty, TY_TENSOR_SCHEMA_KIND, type_core->kind);
        DSL_Builder_Bind_Attribute_If_Present
            (tensor_ty, TY_TENSOR_SCHEMA_DTYPE, type_core->dtype);
        TY_tensor_bind_attribute (tensor_ty, TY_TENSOR_SCHEMA_RANK, rank_buf);
        DSL_Builder_Bind_Attribute_If_Present
            (tensor_ty, TY_TENSOR_SCHEMA_SHAPE, type_core->logical_shape);
    }

    return tensor_ty;
}

BOOL
DSL_Builder_Attach_Tensor_Descriptor
        (TY_IDX ty,
         const DSL_BUILDER_TENSOR_DESCRIPTOR *descriptor)
{
    if (descriptor == NULL || !TY_is_tensor_extension (ty))
        return FALSE;

    DSL_Builder_Bind_Attribute_If_Present
        (ty, TY_TENSOR_SCHEMA_KIND, descriptor->type_core.kind);
    DSL_Builder_Bind_Attribute_If_Present
        (ty, TY_TENSOR_SCHEMA_DTYPE, descriptor->type_core.dtype);
    if (descriptor->type_core.rank >= 0) {
        char rank_buf[32];
        snprintf (rank_buf, sizeof(rank_buf), "%d",
                  descriptor->type_core.rank);
        TY_tensor_bind_attribute (ty, TY_TENSOR_SCHEMA_RANK, rank_buf);
    }
    DSL_Builder_Bind_Attribute_If_Present
        (ty, TY_TENSOR_SCHEMA_SHAPE, descriptor->type_core.logical_shape);
    DSL_Builder_Bind_Attribute_If_Present
        (ty, TY_TENSOR_SCHEMA_TRAITS, descriptor->traits.traits);
    DSL_Builder_Bind_Attribute_If_Present
        (ty, TY_TENSOR_SCHEMA_LAYOUT, descriptor->representation.layout);
    DSL_Builder_Bind_Attribute_If_Present
        (ty, TY_TENSOR_SCHEMA_SHARDING, descriptor->representation.sharding);
    DSL_Builder_Bind_Attribute_If_Present
        (ty, TY_TENSOR_SCHEMA_PLACEMENT, descriptor->representation.placement);
    DSL_Builder_Bind_Attribute_If_Present
        (ty, TY_TENSOR_SCHEMA_MEMORY, descriptor->representation.memory);
    DSL_Builder_Bind_Attribute_If_Present
        (ty, TY_TENSOR_SCHEMA_QUANTIZATION,
         descriptor->representation.quantization);
    DSL_Builder_Bind_Attribute_If_Present
        (ty, TY_TENSOR_SCHEMA_RUNTIME_STATE,
         descriptor->representation.runtime_state);
    DSL_Builder_Bind_Attribute_If_Present
        (ty, TY_TENSOR_SCHEMA_LINEAGE, descriptor->lineage.lineage);

    return TRUE;
}

ST_IDX
DSL_Builder_Create_Symbol
        (const char *name,
         TY_IDX ty,
         ST_CLASS sym_class,
         ST_SCLASS storage_class,
         ST_EXPORT export_class)
{
    ST *st = New_ST();
    ST_Init (st, Save_Str(DSL_Builder_Safe_String(name)), sym_class,
             storage_class, export_class, ty);
    return ST_st_idx(*st);
}

DSL_BUILDER_OPERATOR
DSL_Builder_Create_Operator
        (DSL_OPCODE_ID opcode_id,
         UINT16 version,
         DSL_BUILDER_VALUE *kids,
         UINT32 kid_count,
         const DSL_BUILDER_OPERATOR_ATTRIBUTE *attrs,
         UINT32 attr_count)
{
    DSL_OPCODE_INFO info;
    char *payload;
    WN *wn;

    if (!DSL_Opcode_Get_Info (opcode_id, &info))
        return NULL;

    payload = DSL_Builder_Format_Operator_Payload (kids, kid_count, attrs,
                                                   attr_count);
    wn = DSL_WN_Create_Opcode (info.name, version, payload);
    delete [] payload;
    return wn;
}

BOOL
DSL_Builder_Attach_Contract (DSL_BUILDER_OPERATOR wn,
                             DSL_CONTRACT_ID contract_id)
{
    DSL_CONTRACT_INFO info;

    return wn != NULL && DSL_Contract_Get_Info (contract_id, &info);
}

BOOL
DSL_Builder_Attach_Metadata
        (ST_IDX st,
         const DSL_BUILDER_COMPILER_METADATA *metadata,
         UINT32 metadata_count)
{
    if (ST_IDX_index(st) == 0)
        return FALSE;
    if (metadata_count != 0 && metadata == NULL)
        return FALSE;

    for (UINT32 i = 0; i < metadata_count; ++i) {
        if (metadata[i].name == NULL || metadata[i].name[0] == '\0')
            return FALSE;
        ST_tensor_bind_metadata (st, metadata[i].name, metadata[i].value);
    }

    return TRUE;
}

DSL_BUILDER_PROGRAM_UNIT
DSL_Builder_Create_Minimal_PU (const char *name)
{
    TY_IDX function_ty;
    PU_IDX pu_idx;
    PU *pu;
    ST *func_st;
    WN *func_body;
    WN *entry_wn;
    PU_Info *pu_info;

    if (name == NULL || name[0] == '\0')
        return NULL;
    /*
     * Keep the first PU bridge to one entry function.  Multiple top-level PUs
     * need explicit local-scope ownership before Python should expose them.
     */
    if (DSL_Builder_PU_Root != NULL)
        return DSL_Builder_PU_Root;

    function_ty = Make_Function_Type(MTYPE_To_TY(MTYPE_V));
    pu = &New_PU(pu_idx);
    PU_Init(*pu, function_ty, GLOBAL_SYMTAB + 1);

    func_st = New_ST();
    ST_Init(func_st, Save_Str(name), CLASS_FUNC, SCLASS_TEXT,
            EXPORT_LOCAL, function_ty);
    Set_ST_pu(func_st, pu_idx);

    func_body = WN_CreateBlock();
    entry_wn = WN_CreateEntry(0, func_st, func_body, NULL, NULL);

    Current_pu = pu;
    Current_scope = pu->lexical_level;
    New_Scope(Current_scope, Malloc_Mem_Pool, TRUE);
    Scope_tab[Current_scope].st = func_st;

    pu_info = TYPE_MEM_POOL_ALLOC(PU_Info, Malloc_Mem_Pool);
    PU_Info_init(pu_info);

    Set_PU_Info_tree_ptr(pu_info, entry_wn);
    PU_Info_maptab(pu_info) = Current_Map_Tab;
    PU_Info_proc_sym(pu_info) = ST_st_idx(func_st);
    Set_PU_Info_state(pu_info, WT_SYMTAB, Subsect_InMem);
    Set_PU_Info_state(pu_info, WT_TREE, Subsect_InMem);
    Set_PU_Info_state(pu_info, WT_PROC_SYM, Subsect_InMem);
    Set_PU_Info_state(pu_info, WT_DEPGRAPH, Subsect_Missing);
    Set_PU_Info_state(pu_info, WT_PREFETCH, Subsect_Missing);
    Set_PU_Info_state(pu_info, WT_REGIONS, Subsect_Missing);
    Set_PU_Info_state(pu_info, WT_FEEDBACK, Subsect_Missing);
    Set_PU_Info_state(pu_info, WT_FREQ, Subsect_Missing);
    Set_PU_Info_state(pu_info, WT_AC_INTERNAL, Subsect_Missing);
    Set_PU_Info_state(pu_info, WT_ALIAS_CLASS, Subsect_Missing);
    Set_PU_Info_state(pu_info, WT_SSA, Subsect_Missing);
    Set_PU_Info_state(pu_info, WT_ALIAS_CGNODE, Subsect_Missing);

    if (DSL_Builder_PU_Root != NULL)
        PU_Info_next(DSL_Builder_PU_Last) = pu_info;
    else
        DSL_Builder_PU_Root = pu_info;
    DSL_Builder_PU_Last = pu_info;

    return pu_info;
}

BOOL
DSL_Builder_Finalize_Mapped_Image
        (const DSL_BUILDER_MAPPED_IMAGE_REQUEST *request)
{
    if (request == NULL ||
        request->path == NULL ||
        request->path[0] == '\0' ||
        request->flags != 0)
        return FALSE;

    Irb_File_Name = (char *)request->path;

    if (Current_DST == NULL)
        DST_Init(NULL, 0);
    if (Open_Output_Info(Irb_File_Name) == NULL)
        return FALSE;

    for (PU_Info *pu = DSL_Builder_PU_Root; pu != NULL;
         pu = PU_Info_next(pu)) {
        if (PU_Info_state(pu, WT_SYMTAB) == Subsect_InMem ||
            PU_Info_state(pu, WT_TREE) == Subsect_InMem)
            Write_PU_Info(pu);
    }

    Write_Global_Info(DSL_Builder_PU_Root);
    Close_Output_Info();
    return TRUE;
}
