/*
 * Copyright (C) 2026 Open64 Project
 */

#ifndef dsl_builder_INCLUDED
#define dsl_builder_INCLUDED

#include "defs.h"
#include "pu_info.h"
#include "symtab.h"
#include "wn.h"
#include "dsl_contract.h"
#include "dsl_ir_image.h"
#include "dsl_opcode.h"

/*
 * Minimal C++ builder-facing DSL API.
 *
 * The future Python ingestion layer should capture model operations and hand
 * them to this native boundary.  This API intentionally creates first-class DSL
 * operators at ingestion time; intrinsic or target-specific lowering remains a
 * later compiler phase.
 *
 * Keep this boundary narrow.  Bindings may pass names, attributes, metadata,
 * and opaque Open64 handles such as TY_IDX, ST_IDX, and WN*, but Python must
 * not construct WHIRL nodes, mutate symbol/type tables, or depend on table
 * layout.  C++ owns those compiler objects.
 *
 * This header stages construction only.  Binary image finalization still uses
 * existing mapped-image / ELF WHIRL mechanisms and must not introduce a new
 * source-language file format.
 */

typedef WN *DSL_BUILDER_VALUE;
typedef WN *DSL_BUILDER_OPERATOR;
typedef PU_Info *DSL_BUILDER_PROGRAM_UNIT;

typedef struct {
    const char *kind;
    const char *dtype;
    INT32 rank;
    const char *logical_shape;
} DSL_BUILDER_TENSOR_TYPE_CORE;

typedef struct {
    const char *traits;
} DSL_BUILDER_TENSOR_TRAITS;

typedef struct {
    const char *layout;
    const char *sharding;
    const char *placement;
    const char *memory;
    const char *quantization;
    const char *runtime_state;
} DSL_BUILDER_TENSOR_REPRESENTATION;

typedef struct {
    const char *lineage;
} DSL_BUILDER_TENSOR_LINEAGE;

typedef struct {
    DSL_BUILDER_TENSOR_TYPE_CORE type_core;
    DSL_BUILDER_TENSOR_TRAITS traits;
    DSL_BUILDER_TENSOR_REPRESENTATION representation;
    DSL_BUILDER_TENSOR_LINEAGE lineage;
} DSL_BUILDER_TENSOR_DESCRIPTOR;

typedef struct {
    const char *name;
    const char *value;
} DSL_BUILDER_OPERATOR_ATTRIBUTE;

typedef struct {
    const char *name;
    const char *value;
} DSL_BUILDER_COMPILER_METADATA;

typedef struct {
    const char *path;
    UINT32 flags;
} DSL_BUILDER_MAPPED_IMAGE_REQUEST;

extern TY_IDX DSL_Builder_Create_Tensor_Type_Core
                                (const char *name,
                                 TY_IDX element_ty,
                                 const DSL_BUILDER_TENSOR_TYPE_CORE *type_core);
extern BOOL DSL_Builder_Attach_Tensor_Descriptor
                                (TY_IDX ty,
                                 const DSL_BUILDER_TENSOR_DESCRIPTOR *descriptor);
extern ST_IDX DSL_Builder_Create_Symbol
                                (const char *name,
                                 TY_IDX ty,
                                 ST_CLASS sym_class,
                                 ST_SCLASS storage_class,
                                 ST_EXPORT export_class);
extern DSL_BUILDER_OPERATOR DSL_Builder_Create_Operator
                                (DSL_OPCODE_ID opcode_id,
                                 UINT16 version,
                                 DSL_BUILDER_VALUE *kids,
                                 UINT32 kid_count,
                                 const DSL_BUILDER_OPERATOR_ATTRIBUTE *attrs,
                                 UINT32 attr_count);
extern BOOL DSL_Builder_Attach_Contract
                                (DSL_BUILDER_OPERATOR wn,
                                 DSL_CONTRACT_ID contract_id);
extern BOOL DSL_Builder_Attach_Metadata
                                (ST_IDX st,
                                 const DSL_BUILDER_COMPILER_METADATA *metadata,
                                 UINT32 metadata_count);
extern DSL_BUILDER_PROGRAM_UNIT DSL_Builder_Create_Minimal_PU
                                (const char *name);
extern BOOL DSL_Builder_Finalize_Mapped_Image
                                (const DSL_BUILDER_MAPPED_IMAGE_REQUEST *request);

#endif /* dsl_builder_INCLUDED */
