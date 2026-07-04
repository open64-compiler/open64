/*
 * Native C++ fixtures that simulate the future Python ingestion builder calls.
 *
 * These helpers intentionally use today's staged DSL marker and tensor
 * descriptor storage.  They create native KIND_TENSOR TY records through the
 * existing descriptor side table, but do not introduce Python bindings, native
 * WHIRL opcodes, or binary image sections.
 */

#ifndef dsl_ingestion_fixture_INCLUDED
#define dsl_ingestion_fixture_INCLUDED

#include <stdio.h>
#include <string.h>

#include "defs.h"
#include "wn.h"
#include "symtab.h"
#include "stab.h"
#include "dsl_builder.h"

typedef struct {
    TY_IDX ty;
    ST_IDX st;
    WN *value;
    const char *name;
} DSL_FIXTURE_VALUE;

static TY_IDX
DSL_Fixture_Create_Tensor_Type (const char *type_name,
                                const char *dtype,
                                INT32 rank,
                                const char *shape,
                                const char *layout)
{
    DSL_BUILDER_TENSOR_TYPE_CORE type_core;
    DSL_BUILDER_TENSOR_DESCRIPTOR descriptor;
    TY_IDX tensor_ty;

    memset (&type_core, 0, sizeof(type_core));
    memset (&descriptor, 0, sizeof(descriptor));

    type_core.kind = "tensor";
    type_core.dtype = dtype;
    type_core.rank = rank;
    type_core.logical_shape = shape;
    tensor_ty = DSL_Builder_Create_Tensor_Type_Core
                    (type_name, MTYPE_To_TY(MTYPE_I4), &type_core);

    descriptor.type_core = type_core;
    descriptor.representation.layout = layout;
    DSL_Builder_Attach_Tensor_Descriptor (tensor_ty, &descriptor);
    TY_tensor_declare_attribute (tensor_ty, TY_TENSOR_SCHEMA_TRAITS);
    TY_tensor_declare_attribute (tensor_ty, TY_TENSOR_SCHEMA_SHARDING);
    TY_tensor_declare_attribute (tensor_ty, TY_TENSOR_SCHEMA_LINEAGE);

    return tensor_ty;
}

static ST_IDX
DSL_Fixture_Create_Tensor_Symbol (const char *symbol_name,
                                  TY_IDX tensor_ty,
                                  const char *source_layer_name,
                                  const char *lowering_hint)
{
    DSL_BUILDER_COMPILER_METADATA metadata[2];
    ST_IDX st = DSL_Builder_Create_Symbol (symbol_name, tensor_ty, CLASS_VAR,
                                           SCLASS_UGLOBAL, EXPORT_LOCAL);

    metadata[0].name = TY_tensor_schema_key_name
                           (TY_TENSOR_SCHEMA_SOURCE_LAYER_NAME);
    metadata[0].value = source_layer_name;
    metadata[1].name = TY_tensor_schema_key_name
                           (TY_TENSOR_SCHEMA_LOWERING_HINT);
    metadata[1].value = lowering_hint;
    DSL_Builder_Attach_Metadata (st, metadata, 2);
    return st;
}

static DSL_FIXTURE_VALUE
DSL_Fixture_Create_Tensor_Const (const char *value_name,
                                 const char *type_name,
                                 const char *dtype,
                                 INT32 rank,
                                 const char *shape,
                                 const char *layout,
                                 const char *value)
{
    DSL_FIXTURE_VALUE fixture;

    fixture.name = value_name;
    fixture.ty = DSL_Fixture_Create_Tensor_Type (type_name, dtype, rank,
                                                 shape, layout);
    fixture.st = DSL_Fixture_Create_Tensor_Symbol (value_name,
                                                   fixture.ty,
                                                   "dsl_ingestion_fixture",
                                                   "native_fixture");
    fixture.value = DSL_WN_Create_Tensor_Const (value_name, dtype, rank,
                                                shape, "splat", value);
    return fixture;
}

static WN *
DSL_Fixture_Create_Binary_Operator (const char *opcode,
                                    const DSL_FIXTURE_VALUE *kid0,
                                    const DSL_FIXTURE_VALUE *kid1,
                                    const char *attributes)
{
    DSL_DOMAIN_ID common_id;
    DSL_OPCODE_ID opcode_id;
    DSL_BUILDER_VALUE kids[2];
    DSL_BUILDER_OPERATOR_ATTRIBUTE attrs[4];
    UINT32 attr_count = 0;
    char attr_buf[1024];

    DSL_Opcode_Register_Common_Substrate ();
    common_id = DSL_Domain_Find ("common");
    opcode_id = DSL_Opcode_Find (common_id, opcode, 1);

    if (attributes != NULL) {
        char *cursor;
        size_t attr_len = strlen (attributes);
        if (attr_len >= sizeof(attr_buf))
            attr_len = sizeof(attr_buf) - 1;
        memcpy (attr_buf, attributes, attr_len);
        attr_buf[attr_len] = '\0';

        cursor = attr_buf;
        while (*cursor != '\0' && attr_count < 4) {
            char *next = strchr (cursor, ';');
            char *equals = strchr (cursor, '=');
            if (next != NULL)
                *next = '\0';
            if (equals != NULL) {
                *equals = '\0';
                attrs[attr_count].name = cursor;
                attrs[attr_count].value = equals + 1;
            } else {
                attrs[attr_count].name = cursor;
                attrs[attr_count].value = "";
            }
            ++attr_count;
            if (next == NULL)
                break;
            cursor = next + 1;
        }
    }

    kids[0] = kid0 == NULL ? NULL : kid0->value;
    kids[1] = kid1 == NULL ? NULL : kid1->value;
    return DSL_Builder_Create_Operator (opcode_id, 1, kids, 2, attrs,
                                        attr_count);
}

static WN *
DSL_Fixture_Create_Common_Add (const DSL_FIXTURE_VALUE *kid0,
                               const DSL_FIXTURE_VALUE *kid1)
{
    return DSL_Fixture_Create_Binary_Operator
               (DSL_OPCODE_COMMON_ADD, kid0, kid1, "attr.broadcast_rule=none");
}

static WN *
DSL_Fixture_Create_Common_Matmul (const DSL_FIXTURE_VALUE *kid0,
                                  const DSL_FIXTURE_VALUE *kid1)
{
    return DSL_Fixture_Create_Binary_Operator
               (DSL_OPCODE_COMMON_MATMUL, kid0, kid1,
                "attr.transpose_kid0=false;attr.transpose_kid1=false");
}

#endif /* dsl_ingestion_fixture_INCLUDED */
