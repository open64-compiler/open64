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
    TY_IDX tensor_ty = TY_Create_Tensor_Type
			  (type_name, MTYPE_To_TY(MTYPE_I4), rank);
    char rank_buf[32];

    snprintf (rank_buf, sizeof(rank_buf), "%d", rank);
    TY_tensor_bind_attribute (tensor_ty, TY_TENSOR_SCHEMA_KIND, "tensor");
    TY_tensor_bind_attribute (tensor_ty, TY_TENSOR_SCHEMA_DTYPE, dtype);
    TY_tensor_bind_attribute (tensor_ty, TY_TENSOR_SCHEMA_RANK, rank_buf);
    TY_tensor_bind_attribute (tensor_ty, TY_TENSOR_SCHEMA_SHAPE, shape);
    TY_tensor_bind_attribute (tensor_ty, TY_TENSOR_SCHEMA_LAYOUT, layout);
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
    ST *st = New_ST();
    ST_Init (st, Save_Str(symbol_name), CLASS_VAR, SCLASS_UGLOBAL,
	     EXPORT_LOCAL, tensor_ty);
    ST_tensor_bind_metadata (ST_st_idx(*st),
			     TY_TENSOR_SCHEMA_SOURCE_LAYER_NAME,
			     source_layer_name);
    ST_tensor_bind_metadata (ST_st_idx(*st),
			     TY_TENSOR_SCHEMA_LOWERING_HINT,
			     lowering_hint);
    return ST_st_idx(*st);
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
    char payload[1024];

    snprintf (payload, sizeof(payload), "kid0=%s;kid1=%s;%s",
	      kid0 == NULL ? "" : kid0->name,
	      kid1 == NULL ? "" : kid1->name,
	      attributes == NULL ? "" : attributes);
    return DSL_WN_Create_Opcode (opcode, 1, payload);
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
