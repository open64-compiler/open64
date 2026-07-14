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

typedef struct {
    UINT32 file_id;
    INT32 line;
    UINT16 column;
    UINT8 statement_begin;
    UINT8 basic_block_begin;
} DSL_BUILDER_SOURCE_POSITION;

typedef struct {
    UINT32 native_node_count;
    UINT32 result_symbol_count;
    UINT32 error_count;
    char *diagnostic;
    UINT32 diagnostic_capacity;
} DSL_BUILDER_VERIFY_RESULT;

typedef struct {
    const char *storage_format;
    const char *side_file;
    const char *tensor_key;
    UINT64 byte_offset;
    UINT64 byte_length;
    const char *checksum;
} DSL_BUILDER_EXTERNAL_TENSOR_REFERENCE;

typedef struct {
    const char *opcode_name;
    UINT32 opcode_name_len;
    UINT32 version;
    const char *payload;
} DSL_BUILDER_MARKER_INFO;

typedef DSL_BUILDER_MARKER_INFO DSL_BUILDER_VALUE_INFO;

extern TY_IDX DSL_Builder_Create_Tensor_Type_Core
                                (const char *name,
                                 TY_IDX element_ty,
                                 const DSL_BUILDER_TENSOR_TYPE_CORE *type_core);
extern BOOL DSL_Builder_Attach_Tensor_Descriptor
                                (TY_IDX ty,
                                 const DSL_BUILDER_TENSOR_DESCRIPTOR *descriptor);
extern TY_IDX DSL_Builder_Intern_Tensor_Type
                                (const char *name,
                                 TY_IDX element_ty,
                                 const DSL_BUILDER_TENSOR_DESCRIPTOR *descriptor);
extern BOOL DSL_Builder_Get_Tensor_Descriptor
                                (TY_IDX ty,
                                 DSL_BUILDER_TENSOR_DESCRIPTOR *descriptor);
extern BOOL DSL_Builder_Tensor_Type_Is_Canonical (TY_IDX ty);
extern ST_IDX DSL_Builder_Create_Symbol
                                (const char *name,
                                 TY_IDX ty,
                                 ST_CLASS sym_class,
                                 ST_SCLASS storage_class,
                                 ST_EXPORT export_class);
extern ST_IDX DSL_Builder_Create_Tensor_Result_Symbol
                                (const char *name,
                                 TY_IDX ty,
                                 ST_SCLASS storage_class,
                                 ST_EXPORT export_class);
extern BOOL DSL_Builder_Set_Tensor_Unique_Ownership (ST_IDX st);
extern BOOL DSL_Builder_Tensor_Has_Unique_Ownership (ST_IDX st);
extern DSL_BUILDER_VALUE DSL_Builder_Create_Tensor_Constant
                                (const char *name,
                                 TY_IDX tensor_ty,
                                 const char *dtype,
                                 UINT32 rank,
                                 const char *logical_shape,
                                 const char *value_kind,
                                 const char *value);
extern DSL_BUILDER_VALUE DSL_Builder_Create_Model_Input
                                (const char *name,
                                 TY_IDX tensor_ty,
                                 UINT32 input_ordinal);
extern DSL_BUILDER_VALUE DSL_Builder_Create_External_Tensor_Constant
                                (const char *name,
                                 TY_IDX tensor_ty,
                                 const DSL_BUILDER_EXTERNAL_TENSOR_REFERENCE
                                     *reference);
extern DSL_BUILDER_OPERATOR DSL_Builder_Create_Operator
                                (DSL_OPCODE_ID opcode_id,
                                 UINT16 version,
                                 DSL_BUILDER_VALUE *kids,
                                 UINT32 kid_count,
                                 const DSL_BUILDER_OPERATOR_ATTRIBUTE *attrs,
                                 UINT32 attr_count);
extern DSL_BUILDER_OPERATOR DSL_Builder_Create_Operator_With_Result
                                (DSL_OPCODE_ID opcode_id,
                                 UINT16 version,
                                 DSL_BUILDER_VALUE *kids,
                                 UINT32 kid_count,
                                 const DSL_BUILDER_OPERATOR_ATTRIBUTE *attrs,
                                 UINT32 attr_count,
                                 const char *result_name,
                                 TY_IDX result_ty);
extern BOOL DSL_Builder_Attach_Contract
                                (DSL_BUILDER_OPERATOR wn,
                                 DSL_CONTRACT_ID contract_id);
extern BOOL DSL_Builder_Attach_Metadata
                                (ST_IDX st,
                                 const DSL_BUILDER_COMPILER_METADATA *metadata,
                                 UINT32 metadata_count);
extern BOOL DSL_Builder_Attach_Value_Metadata
                                (DSL_BUILDER_VALUE value,
                                 const DSL_BUILDER_COMPILER_METADATA *metadata,
                                 UINT32 metadata_count);
extern BOOL DSL_Builder_Attach_Value_Lineage
                                (DSL_BUILDER_VALUE value,
                                 const char *lineage);
extern TY_IDX DSL_Builder_Get_Value_Type (DSL_BUILDER_VALUE value);
extern ST_IDX DSL_Builder_Get_Value_Result_Symbol (DSL_BUILDER_VALUE value);
extern BOOL DSL_Builder_Begin_Program (void);
extern void DSL_Builder_Abort_Program (void);
extern DSL_BUILDER_PROGRAM_UNIT DSL_Builder_Create_Minimal_PU
                                (const char *name);
extern UINT32 DSL_Builder_Register_Source_File
                                (DSL_BUILDER_PROGRAM_UNIT pu,
                                 const char *path);
extern BOOL DSL_Builder_Set_Value_Source_Position
                                (DSL_BUILDER_VALUE value,
                                 const DSL_BUILDER_SOURCE_POSITION
                                     *source_position);
extern BOOL DSL_Builder_Verify_Program
                                (DSL_BUILDER_VERIFY_RESULT *result);
/*
 * Formal VHO DSL value representation.
 *
 * The stable API is an opaque WHIRL WN handle.  Native common operators return
 * the defining STID for a no-alias tensor result temporary; its expression kid
 * is the logical DSL operator and its operand kids are LDID references to prior
 * result temporaries.  Compatibility operators may retain annotated COMMENT,
 * EVAL, or XPRAGMA carriers.  Python and other frontends must not depend on
 * either physical representation.
 *
 * The marker-named entry points below remain compatibility wrappers for
 * earlier tests and callers.
 */
extern BOOL DSL_Builder_Append_PU_Value
                                (DSL_BUILDER_PROGRAM_UNIT pu,
                                 DSL_BUILDER_VALUE value);
extern UINT32 DSL_Builder_Count_PU_Values
                                (DSL_BUILDER_PROGRAM_UNIT pu);
extern BOOL DSL_Builder_Get_PU_Value
                                (DSL_BUILDER_PROGRAM_UNIT pu,
                                 UINT32 index,
                                 DSL_BUILDER_VALUE_INFO *info);
extern UINT32 DSL_Builder_Count_Value_Operands
                                (DSL_BUILDER_VALUE value);
extern BOOL DSL_Builder_Get_Value_Info
                                (DSL_BUILDER_VALUE value,
                                 DSL_BUILDER_VALUE_INFO *info);
extern BOOL DSL_Builder_Get_Value_Operand
                                (DSL_BUILDER_VALUE value,
                                 UINT32 operand_index,
                                 DSL_BUILDER_VALUE_INFO *info);
extern BOOL DSL_Builder_Append_PU_Marker
                                (DSL_BUILDER_PROGRAM_UNIT pu,
                                 DSL_BUILDER_VALUE marker);
extern UINT32 DSL_Builder_Count_PU_Markers
                                (DSL_BUILDER_PROGRAM_UNIT pu);
extern BOOL DSL_Builder_Get_PU_Marker
                                (DSL_BUILDER_PROGRAM_UNIT pu,
                                 UINT32 index,
                                 DSL_BUILDER_MARKER_INFO *info);
extern BOOL DSL_Builder_Finalize_Mapped_Image
                                (const DSL_BUILDER_MAPPED_IMAGE_REQUEST *request);

#endif /* dsl_builder_INCLUDED */
